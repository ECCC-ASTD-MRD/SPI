#!/bin/ksh
#===============================================================================
# Environnement Canada
# Centre Meteorologique Canadien
# Dorval, Quebec
#
# Projet     : Interface pour le modele MLDP0.
# Nom        : <Model_MeteoMLDP0.sh>
# Creation   : Avril 2000 - S. Trudel - CMC/CMOE
#
# Description: Generate one standard file for trials and prognostics
#              meteorological data required for driving MLDP0 model.
#
# Parametres :
#   ${1}     : Simulation trunk directory.
#   ${2}     : Type of meteorological config (glb|reg).
#   ${3}     : Number of processes.
#   ${4}     : Grid size (NIxNJxNK).
#   ${5}     : Printing debug level (low|moderate|high).
#
# Retour     : one standard file (tape30) for the following fields :
#              - ES (dew point deviation)           [deg C]
#              - HU (specific humidity)             [kg/kg]
#              - GZ (geopotential height)           [dam]
#              - H  (boundary layer height)         [m]
#              - P0 (surface pression)              [mb]
#              - PT (pression at top of atmosphere) [mb]
#              - TT (temperature)                   [deg C]
#              - UV (wind speed)                    [knots]
#              - WE (vertical motion)               [s -1]
#              - WW (vertical motion)               [Pa/s]
#              - Z0 (roughness length)              [m]
#
# Remarques  :
#   Aucune.
#===============================================================================

#----- Source user's profile.
. ~/.profile >/dev/null 2>&1
. ~/.profile_eer >/dev/null 2>&1

#----- Load standard functions
export EER_DIRSCRIPT=/home/binops/afse/eer/eer_SPI-7.5.1/Script
. ${EER_DIRSCRIPT}/Logger.sh

Log_Start Model_MeteoMLDP0.sh 2.0

#----- Get arguments.
Dir="${1}"
Model="${2}"
NbProc="${3}"
GridSize="${4}"
Debug="${5}"
Ext="${6}"
Debug="high"

Log_Print INFO "Temporary directory  : ${DirTmp}"
Log_Print INFO "Meteorological config: ${Model}"
Log_Print INFO "Number of processes  : ${NbProc}"
Log_Print INFO "Printing debug level : ${Debug}"

#----- Define grid parameters.
if [[ ${GridSize} = "" ]] ; then
   GridSize="229x229x25"
   Log_Print WARNING "Grid size not defined, using default grid size : ${GridSize}"
else
   Log_Print INFO "Grid size parameters : ${GridSize}"
fi

cd ${Dir}

#----- Read the grid parameters from grid file and redirect into "grid" variable.
read < tmp/griddef.in grid

#----- Create configuration input file for PGSM according to type of meteorological model.

#----- Interpolate met fields over a polar stereographic grid.
#----- Set lower boundary value to 100 m for 'H' (boundary layer height).
#----- Set lower boundary value to 0.0 m/s for 'RT' (precipitation rate).
#----- Set lower boundary value to 0.01 m for 'Z0' (roughness length).
#----- Clamp 'HR' (relative humidity) in the range [0,1].
#----- Clamp 'FN' (cloud fraction) in the range [0,1].

cat <<EOF_PGSM_METEO > tmp/pgsm.dir
 SORTIE(STD,2000,A)
 GRILLE(PS,${grid})
 IP3ENT=0
 HEURE(-1)
C
 EXTRAP(VOISIN)
 CONV(H ,0.0,1.0,100.0)
 CONV(HR,0.0,1.0,0.0,1.0)
 CONV(FN,0.0,1.0,0.0,1.0)
 CONV(RT,0.0,1.0,0.0)
 CONV(Z0,0.0,1.0,0.01)
 CHAMP(H ,TOUT)
 CHAMP(P0,TOUT)
 CHAMP(PT,TOUT)
 CHAMP(PR,TOUT)
 CHAMP(OL,TOUT)
 CHAMP(RT,TOUT)
 CHAMP(UE,TOUT)
 CHAMP(Z0,1195)
EOF_PGSM_METEO

#----- Meteorological fields
if [ -r ${Model} ]
then
   cat ${Model} >> tmp/pgsm.dir
elif [ -r ${EER_DIRSCRIPT}/Model_MeteoConfig/${Model}.dir ]
then
   cat ${EER_DIRSCRIPT}/Model_MeteoConfig/${Model}.dir >> tmp/pgsm.dir
else
    Log_Print ERROR "Wrong meteorological model config (${Model})."
    Log_End 1
fi

#----- Read the list of all standard meteorological files.
read < tmp/data_std_eta.in stdfiles

set -A ArrayStdFiles ${stdfiles}
nbfiles=${#ArrayStdFiles[@]}

#----- Print number of processes and all standard files to process.
Log_Print INFO "Number of processes: ${NbProc}"
Log_Print INFO "Number of standard files to process: ${nbfiles}"

#----- PGSM.
export FST_OPTIONS="DATATYPE_REMAP=1,134 5,133"

idx=0
nbproc=0
Log_Print INFO "Executing PGSM: Interpolating met fields on the specified grid for standard files ..."

#----- Erase old file.
rm -f meteo/*.std

while [ ${idx} -lt ${nbfiles} ] ; do

   #----- Initialize output filenames.
   file=${ArrayStdFiles[${idx}]}
   filename=`basename ${file}`
   idx=`expr ${idx} + 1` #----- Increment index of current standard file.

   #----- Verify if standard file exists and is readable.
   if [ ! -r ${file} ] ; then
     Log_Print ERROR "   This standard file is not readable : ${file}."
     Log_End 1
   fi

   #----- Interpolate meteorological fields for the specified grid and standard file using PGSM.
   Log_Print INFO "   Processing standard file ${file} (${idx}/${nbfiles}) ..."
   pgsm+ -iment ${file} \
      -ozsrt meteo/${filename}.std \
      -i tmp/pgsm.dir \
      >tmp/pgsm.${filename}.out 2>tmp/pgsm.${filename}.err &

   nbproc=`expr ${nbproc} + 1` #----- Increment number of processes.

   if [ \( ${nbproc} -eq ${NbProc} \) -o \( ${idx} -eq ${nbfiles} \) ] ; then

      Log_Print INFO "   Waiting until all background processes are completed ..."
      wait
      nbproc=0 #----- Reset number of processes.

      #----- Verify if PGSM has terminated successfully.
      nbline=`grep "PGSM.*OK" tmp/pgsm.*.out | wc -l`
      if [[ ${nbline} -lt ${idx} ]] ; then
         Log_Print ERROR "   PGSM has encountered an errors."
         Log_End 1
      fi
   fi
done

wait
export FST_OPTIONS=""

#----- MLDP0 pre-processor.
idx=0
nbproc=0
NI=`echo ${GridSize} | cut -d"x" -f1`
NJ=`echo ${GridSize} | cut -d"x" -f2`
NK=`echo ${GridSize} | cut -d"x" -f3`

Log_Print INFO "Executing MLDP0 pre-processor: Computing other meteorological fields required by the model ..."

while [ ${idx} -lt ${nbfiles} ] ; do

   #----- Initialize output filenames.
   filename=`basename ${ArrayStdFiles[${idx}]}`
   idx=`expr ${idx} + 1` #----- Increment index of current standard file.

   #----- Launch MLDP0 pre-processor to compute other meteorological fields required by the model:
   #-----   - 'H'  : [2D] Boundary layer height [m] (if missing field),
   #-----   - 'RA' : [2D] Atmospheric resistance for the momentum [s/m],
   #-----   - 'RP' : [2D] Precipitation rate [mm/h],
   #-----   - 'Z0' : [2D] Roughness length [m],
   #-----   - 'FN' : [3D] Cloud fraction [dimensionless],
   #-----   - 'HR' : [3D] Relative humidity [dimensionless],
   #-----   - 'KT' : [3D] Vertical diffusion coefficient [m2/s],
   #-----   - 'RI' : [3D] Richardson number [dimensionless],
   #-----   - 'SU' : [3D] Wind speed X-component ('UU') variance [kt2],
   #-----   - 'SV' : [3D] Wind speed Y-component ('VV') variance [kt2],
   #-----   - 'TH' : [3D] Virtual potential temperature [K].
   Log_Print INFO "   Processing standard file meteo/${filename}.std (${idx}/${nbfiles}) ..."
   ${EER_DIRBIN}/metfields_MLDP0${Ext} \
      -iment meteo/${filename}.std \
      -ozsrt meteo/${filename}.met.std \
      -print ${Debug} \
      -ni ${NI} \
      -nj ${NJ} \
      -nk ${NK} \
      >tmp/metfields.${filename}.out 2>tmp/metfields.${filename}.err &

   nbproc=`expr ${nbproc} + 1` #----- Increment number of processes.

   if [ \( ${nbproc} -eq ${NbProc} \) -o \( ${idx} -eq ${nbfiles} \) ] ; then

      Log_Print INFO "   Waiting until all background processes are completed ..."
      wait
      nbproc=0 #----- Reset number of processes.

      #----- Verify if Metfields has terminated successfully.
      nbline=`grep "METFLD0.*FIN" tmp/metfields.*.out | wc -l`
      if [[ ${nbline} -lt ${idx} ]] ; then
         Log_Print ERROR "   ${EER_DIRBIN}/metfields_MLDP0 has encountered an errors."
         Log_End 1
      fi
    fi
done

wait

#----- EDITFST.
idx=0
nbproc=0

Log_Print INFO "Executing EDITFST: Merging the two meteorological files (PGSM + metfields) into one standard file for MLDP0 ..."

while [ ${idx} -lt ${nbfiles} ] ; do
   #----- Initialize output filenames.
   filename=`basename ${ArrayStdFiles[${idx}]}`
   idx=`expr ${idx} + 1`

   #----- Merge the two meteorological files (PGSM + Metfields) into one standard file for MLDP0.
   Log_Print INFO "   Processing standard file ${filename}.std (${idx}/${nbfiles}) ..."
   editfst+ \
      -s meteo/${filename}.met.std \
      -d meteo/${filename}.std \
      -i 0 \
      >tmp/editfst.${filename}.out 2>tmp/editfst.${filename}.err

   rm -f meteo/${filename}.met.std
   nbproc=`expr ${nbproc} + 1` #----- Increment number of processes.

   if [ \( ${nbproc} -eq ${NbProc} \) -o \( ${idx} -eq ${nbfiles} \) ] ; then

      Log_Print INFO "   Waiting until all background processes are completed ..."
      wait
      nbproc=0 #----- Reset number of processes.

      #----- Verify if EDITFST has terminated successfully.
      nbline=`grep "EDITFST.*NORMAL" tmp/editfst.*.out | wc -l`
      if [[ ${nbline} -lt ${idx} ]] ; then
         Log_Print ERROR "   EDITFST has encountered an errors."
         Log_End 1
      fi
   fi
done

wait
Log_End 0
