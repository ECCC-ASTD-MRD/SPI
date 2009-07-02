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
#   ${1}     : Temporary working directory.
#   ${2}     : Type of meteorological model (glb|reg).
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
. ${EER_DIRSCRIPT}/Logger.sh

Log_Start Model_MeteoMLDP0.sh 2.0

#----- Get arguments.
DirTmp="${1}"
Model="${2}"
NbProc="${3}"
GridSize="${4}"
Debug="${5}"

Log_Print INFO "Temporary directory  : ${DirTmp}"
Log_Print INFO "Meteorological model : ${Model}"
Log_Print INFO "Number of processes  : ${NbProc}"
Log_Print INFO "Printing debug level : ${Debug}"

#----- Define grid parameters.
if [[ ${GridSize} = "" ]] ; then
   GridSize="229x229x25"
   Log_Print WARNING "Grid size not defined, using default grid size : ${GridSize}"
else
   Log_Print INFO "Grid size parameters : ${GridSize}"
fi

cd ${DirTmp}

#----- Read the grid parameters from grid file and redirect into "grid" variable.
read < griddef.in grid

#----- Create configuration input file for PGSM according to type of meteorological model.

#----- Interpolate met fields over a polar stereographic grid.
#----- Set lower boundary value to 100 m for 'H' (boundary layer height).
#----- Set lower boundary value to 0.0 m/s for 'RT' (precipitation rate).
#----- Set lower boundary value to 0.01 m for 'Z0' (roughness length).
#----- Clamp 'HR' (relative humidity) in the range [0,1].
#----- Clamp 'FN' (cloud fraction) in the range [0,1].

cat <<EOF_PGSM_METEO > pgsm.dir
 SORTIE(STD,2000,A)
 GRILLE(PS,${grid})
 IP3ENT=0
 HEURE(-1)
C
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

if [ "${Model}" = "glb" ] ; then #----- Meteorological fields from GEM Meso-Global 33 km.

   cat <<EOF_PGSM_METEOg33 >> pgsm.dir
C
C Meteorological fields from GEM Meso-Global 33 km.
C
 CHAMP(ES,2374,2720,2897,3044,3248,3541,3976,4522,5144,5843,
  6348,6612,7446,8034,8646,9272,9845,10346,10780,11151,
  11467,11733,11850,11950,12000)
 CHAMP(HU,2374,2720,2897,3044,3248,3541,3976,4522,5144,5843,
  6348,6612,7446,8034,8646,9272,9845,10346,10780,11151,
  11467,11733,11850,11950,12000)
 CHAMP(HR,2374,2720,2897,3044,3248,3541,3976,4522,5144,5843,
  6348,6612,7446,8034,8646,9272,9845,10346,10780,11151,
  11467,11733,11850,11950,12000)
 CHAMP(FN,2374,2720,2897,3044,3248,3541,3976,4522,5144,5843,
  6348,6612,7446,8034,8646,9272,9845,10346,10780,11151,
  11467,11733,11850,11950,12000)
 CHAMP(GZ,2374,2720,2897,3044,3248,3541,3976,4522,5144,5843,
  6348,6612,7446,8034,8646,9272,9845,10346,10780,11151,
  11467,11733,11850,11950,12000)
 CHAMP(TT,2374,2720,2897,3044,3248,3541,3976,4522,5144,5843,
  6348,6612,7446,8034,8646,9272,9845,10346,10780,11151,
  11467,11733,11850,11950,12000)
 CHAMP(UV,2374,2720,2897,3044,3248,3541,3976,4522,5144,5843,
  6348,6612,7446,8034,8646,9272,9845,10346,10780,11151,
  11467,11733,11850,11950,12000)
 CHAMP(WE,2374,2720,2897,3044,3248,3541,3976,4522,5144,5843,
  6348,6612,7446,8034,8646,9272,9845,10346,10780,11151,
  11467,11733,11850,11950,12000)
 CHAMP(WW,2374,2720,2897,3044,3248,3541,3976,4522,5144,5843,
  6348,6612,7446,8034,8646,9272,9845,10346,10780,11151,
  11467,11733,11850,11950,12000)
EOF_PGSM_METEOg33

elif [ "${Model}" = "glb100" ] ; then #----- Meteorological fields from GEM Global 100 km.

   cat <<EOF_PGSM_METEOg100 >> pgsm.dir
C
C Meteorological fields from GEM Global 100 km.
C
 CHAMP(ES,2510,2750,3010,3270,3550,3850,4190,4580,5020,5510,
  6050,6600,7160,7740,8310,8880,9440,9960,10420,10840,
  11220,11550,11800,11930,12000)
 CHAMP(HU,2510,2750,3010,3270,3550,3850,4190,4580,5020,5510,
  6050,6600,7160,7740,8310,8880,9440,9960,10420,10840,
  11220,11550,11800,11930,12000)
 CHAMP(HR,2510,2750,3010,3270,3550,3850,4190,4580,5020,5510,
  6050,6600,7160,7740,8310,8880,9440,9960,10420,10840,
  11220,11550,11800,11930,12000)
 CHAMP(FN,2510,2750,3010,3270,3550,3850,4190,4580,5020,5510,
  6050,6600,7160,7740,8310,8880,9440,9960,10420,10840,
  11220,11550,11800,11930,12000)
 CHAMP(GZ,2510,2750,3010,3270,3550,3850,4190,4580,5020,5510,
  6050,6600,7160,7740,8310,8880,9440,9960,10420,10840,
  11220,11550,11800,11930,12000)
 CHAMP(TT,2510,2750,3010,3270,3550,3850,4190,4580,5020,5510,
  6050,6600,7160,7740,8310,8880,9440,9960,10420,10840,
  11220,11550,11800,11930,12000)
 CHAMP(UV,2510,2750,3010,3270,3550,3850,4190,4580,5020,5510,
  6050,6600,7160,7740,8310,8880,9440,9960,10420,10840,
  11220,11550,11800,11930,12000)
 CHAMP(WE,2510,2750,3010,3270,3550,3850,4190,4580,5020,5510,
  6050,6600,7160,7740,8310,8880,9440,9960,10420,10840,
  11220,11550,11800,11930,12000)
 CHAMP(WW,2510,2750,3010,3270,3550,3850,4190,4580,5020,5510,
  6050,6600,7160,7740,8310,8880,9440,9960,10420,10840,
  11220,11550,11800,11930,12000)
EOF_PGSM_METEOg100

elif [ "${Model}" = "reg" ] ; then #----- Meteorological fields from GEM Regional 15 km.

   cat <<EOF_PGSM_METEOr15 >> pgsm.dir
C
C Meteorological fields from GEM Regional 15 km.
C
 CHAMP(ES,2374,2720,2897,3044,3248,3541,3976,4522,5144,5843,
  6348,6612,7446,8034,8646,9272,9845,10346,10780,11151,
  11467,11733,11850,11950,12000)
 CHAMP(HU,2374,2720,2897,3044,3248,3541,3976,4522,5144,5843,
  6348,6612,7446,8034,8646,9272,9845,10346,10780,11151,
  11467,11733,11850,11950,12000)
 CHAMP(HR,2374,2720,2897,3044,3248,3541,3976,4522,5144,5843,
  6348,6612,7446,8034,8646,9272,9845,10346,10780,11151,
  11467,11733,11850,11950,12000)
 CHAMP(FN,2374,2720,2897,3044,3248,3541,3976,4522,5144,5843,
  6348,6612,7446,8034,8646,9272,9845,10346,10780,11151,
  11467,11733,11850,11950,12000)
 CHAMP(GZ,2374,2720,2897,3044,3248,3541,3976,4522,5144,5843,
  6348,6612,7446,8034,8646,9272,9845,10346,10780,11151,
  11467,11733,11850,11950,12000)
 CHAMP(TT,2374,2720,2897,3044,3248,3541,3976,4522,5144,5843,
  6348,6612,7446,8034,8646,9272,9845,10346,10780,11151,
  11467,11733,11850,11950,12000)
 CHAMP(UV,2374,2720,2897,3044,3248,3541,3976,4522,5144,5843,
  6348,6612,7446,8034,8646,9272,9845,10346,10780,11151,
  11467,11733,11850,11950,12000)
 CHAMP(WE,2374,2720,2897,3044,3248,3541,3976,4522,5144,5843,
  6348,6612,7446,8034,8646,9272,9845,10346,10780,11151,
  11467,11733,11850,11950,12000)
 CHAMP(WW,2374,2720,2897,3044,3248,3541,3976,4522,5144,5843,
  6348,6612,7446,8034,8646,9272,9845,10346,10780,11151,
  11467,11733,11850,11950,12000)
EOF_PGSM_METEOr15

elif [ "${Model}" = "reg24" ] ; then #----- Meteorological fields from GEM Regional 24 km.

   cat <<EOF_PGSM_METEOr24 >> pgsm.dir
C
C Meteorological fields from GEM Regional 24 km.
C
 CHAMP(ES,2400,2610,2910,3310,3770,4220,4730,5280,5840,6440,
  7000,7550,8110,8660,9220,9730,10180,10590,10940,11250,
  11500,11700,11850,11950,12000)
 CHAMP(HU,2400,2610,2910,3310,3770,4220,4730,5280,5840,6440,
  7000,7550,8110,8660,9220,9730,10180,10590,10940,11250,
  11500,11700,11850,11950,12000)
 CHAMP(HR,2400,2610,2910,3310,3770,4220,4730,5280,5840,6440,
  7000,7550,8110,8660,9220,9730,10180,10590,10940,11250,
  11500,11700,11850,11950,12000)
 CHAMP(FN,2400,2610,2910,3310,3770,4220,4730,5280,5840,6440,
  7000,7550,8110,8660,9220,9730,10180,10590,10940,11250,
  11500,11700,11850,11950,12000)
 CHAMP(GZ,2400,2610,2910,3310,3770,4220,4730,5280,5840,6440,
  7000,7550,8110,8660,9220,9730,10180,10590,10940,11250,
  11500,11700,11850,11950,12000)
 CHAMP(TT,2400,2610,2910,3310,3770,4220,4730,5280,5840,6440,
  7000,7550,8110,8660,9220,9730,10180,10590,10940,11250,
  11500,11700,11850,11950,12000)
 CHAMP(UV,2400,2610,2910,3310,3770,4220,4730,5280,5840,6440,
  7000,7550,8110,8660,9220,9730,10180,10590,10940,11250,
  11500,11700,11850,11950,12000)
 CHAMP(WE,2400,2610,2910,3310,3770,4220,4730,5280,5840,6440,
  7000,7550,8110,8660,9220,9730,10180,10590,10940,11250,
  11500,11700,11850,11950,12000)
 CHAMP(WW,2400,2610,2910,3310,3770,4220,4730,5280,5840,6440,
  7000,7550,8110,8660,9220,9730,10180,10590,10940,11250,
  11500,11700,11850,11950,12000)
EOF_PGSM_METEOr24

else
    Log_Print ERROR "Wrong type of meteorological model. Available met models: reg, glb, reg24, glb100."
    Log_End 1
fi

#----- Read the list of all standard meteorological files.
read < data_std_eta.in stdfiles

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
rm -f ../meteo/*.std

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
      -ozsrt ../meteo/${filename}.std \
      -i pgsm.dir \
      >pgsm.${filename}.out 2>pgsm.${filename}.err &

   nbproc=`expr ${nbproc} + 1` #----- Increment number of processes.

   if [ \( ${nbproc} -eq ${NbProc} \) -o \( ${idx} -eq ${nbfiles} \) ] ; then

      Log_Print INFO "   Waiting until all background processes are completed ..."
      wait
      nbproc=0 #----- Reset number of processes.

      #----- Verify if PGSM has terminated successfully.
      nbline=`grep "PGSM.*OK" pgsm.*.out | wc -l`
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
   Log_Print INFO "   Processing standard file ./meteo/${filename}.std (${idx}/${nbfiles}) ..."
   ${EER_DIRBIN}/metfields_mldp0 \
      -iment ../meteo/${filename}.std \
      -ozsrt ../meteo/${filename}.met.std \
      -print ${Debug} \
      -ni ${NI} \
      -nj ${NJ} \
      -nk ${NK} \
      >metfields.${filename}.out 2>metfields.${filename}.err &

   nbproc=`expr ${nbproc} + 1` #----- Increment number of processes.

   if [ \( ${nbproc} -eq ${NbProc} \) -o \( ${idx} -eq ${nbfiles} \) ] ; then

      Log_Print INFO "   Waiting until all background processes are completed ..."
      wait
      nbproc=0 #----- Reset number of processes.

      #----- Verify if Metfields has terminated successfully.
      nbline=`grep "METFLD0.*FIN" metfields.*.out | wc -l`
      if [[ ${nbline} -lt ${idx} ]] ; then
         Log_Print ERROR "   ${EER_DIRBIN}/metfields_mldp0 has encountered an errors."
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
      -s ../meteo/${filename}.met.std \
      -d ../meteo/${filename}.std \
      -i 0 \
      >editfst.${filename}.out 2>editfst.${filename}.err

   nbproc=`expr ${nbproc} + 1` #----- Increment number of processes.

   if [ \( ${nbproc} -eq ${NbProc} \) -o \( ${idx} -eq ${nbfiles} \) ] ; then

      Log_Print INFO "   Waiting until all background processes are completed ..."
      wait
      nbproc=0 #----- Reset number of processes.

      #----- Verify if EDITFST has terminated successfully.
      nbline=`grep "EDITFST.*NORMAL" editfst.*.out | wc -l`
      if [[ ${nbline} -lt ${idx} ]] ; then
         Log_Print ERROR "   EDITFST has encountered an errors."
         Log_End 1
      fi
   fi
done

wait
Log_End 0
