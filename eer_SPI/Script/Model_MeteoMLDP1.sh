#!/bin/ksh
#===============================================================================
# Environnement Canada
# Centre Meteorologique Canadien
# Dorval, Quebec
#
# Projet     : Interface pour le modele MLDP1.
# Nom        : <Model_MeteoMLDP1.sh>
# Creation   : 11 March 2005 - A. Malo - CMC/CMOE
#
# Description: Generate one standard file for trials and prognostics
#              meteorological data required for driving MLDP1 model.
#
# Parametres :
#   ${1}     : Temporary working directory.
#   ${2}     : Type of meteorological model (reg).
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

Log_Start Model_MeteoMLDP1.sh 2.0

#----- Get arguments.
DirTmp="${1}"
Model="${2}"
NbProc="${3}"
GridSize="${4}"
Debug="${5}"
Ext="${6}"

Log_Print INFO "Temporary directory  : ${DirTmp}"
Log_Print INFO "Meteorological model : ${Model}"
Log_Print INFO "Number of processes  : ${NbProc}"
Log_Print INFO "Printing debug level : ${Debug}"

cd ${DirTmp}

#----- Read the grid parameters from grid file and redirect into "grid" variable.
read < griddef.in grid

#----- Create configuration input file for PGSM according to type of meteorological model.

#----- Interpolate met fields over a polar stereographic grid.
#----- Set lower boudary value to 100 m for 'H' (boundary layer height).
#----- Set lower boundary value to 0.0 m/s for 'RT' (precipitation rate).
#----- Set lower boundary value to 0.01 m for 'Z0' (roughness length).
#----- Set lower boundary value to 0.0001 m for 'EN' (turbulent kinetic energy).
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
 CONV(EN,0.0,1.0,0.0001)
 CHAMP(H ,TOUT)
 CHAMP(P0,TOUT)
 CHAMP(PT,TOUT)
 CHAMP(TG,TOUT)
 CHAMP(UE,TOUT)
 CHAMP(IO,1195)
 CHAMP(J9,1195)
 CHAMP(Z0,1195)
 CHAMP(RT,TOUT)
 CHAMP(EN,TOUT)
EOF_PGSM_METEO

if [ "${Model}" = "reg" ] ; then #----- Meteorological fields from GEM Regional 15 km.

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

elif [ "${Model}" = "glb" ] ; then #----- Meteorological fields from GEM Meso-Global 33 km.

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

else
    Log_Print ERROR "Wrong type of meteorological model. Available met models: reg, glb."
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

#----- MLDP1 pre-processor.
idx=0
nbproc=0

Log_Print INFO "Executing MLDP1 pre-processor: Computing other meteorological fields required by the model ..."

while [ ${idx} -lt ${nbfiles} ] ; do

   #----- Initialize output filenames.
   filename=`basename ${ArrayStdFiles[${idx}]}`
   idx=`expr ${idx} + 1` #----- Increment index of current standard file.

   #----- Launch MLDP1 pre-processor to compute other meteorological fields required by the model:
   #-----   - 'H'  : [2D] Boundary layer height [m],
   #-----   - 'IO' : [2D] Inverse of Monin-Obukhov length [m -1],
   #-----   - 'P0' : [2D] Surface pressure [mb],
   #-----   - 'PT' : [2D] Pressure at top of atmosphere [mb],
   #-----   - 'RA' : [2D] Atmospheric resistance for the momentum [s/m],
   #-----   - 'RP' : [2D] Precipitation rate [mm/h],
   #-----   - 'UE' : [2D] Friction velocity at the surface [m/s],
   #-----   - 'Z0' : [2D] Roughness length [m],
   #-----   - 'ZS' : [2D] Geopotential height at surface [dam],
   #-----   - 'FN' : [3D] Cloud fraction [dimensionless],
   #-----   - 'GZ' : [3D] Geopotential height [dam],
   #-----   - 'HR' : [3D] Relative humidity [dimensionless],
   #-----   - 'HU' : [3D] Specific humidity [kg/kg],
   #-----   - 'RI' : [3D] Richardson number [dimensionless],
   #-----   - 'SU' : [3D] Wind speed X-component ('UU') variance [kt2],
   #-----   - 'SV' : [3D] Wind speed Y-component ('VV') variance [kt2],
   #-----   - 'TH' : [3D] Virtual potential temperature [K],
   #-----   - 'TK' : [3D] Turbulent kinetic energy [m2/s2],
   #-----   - 'TT' : [3D] Temperature [C],
   #-----   - 'UU' : [3D] Wind speed X-component [kt],
   #-----   - 'VV' : [3D] Wind speed Y-component [kt],
   #-----   - 'WE' : [3D] Vertical Motion [s -1].
   Log_Print INFO "   Processing standard file ./meteo/${filename}.std (${idx}/${nbfiles}) ..."
   ${EER_DIRBIN}/metfields_MLDP1${Ext} \
      -iment ../meteo/${filename}.std \
      -ozsrt ../meteo/${filename}.met.std \
      -print ${Debug} \
      >metfields.${filename}.out 2>metfields.${filename}.err &
      > ${OutFile} 2>&1 &

    nbproc=`expr ${nbproc} + 1` #----- Increment number of processes.

   if [ \( ${nbproc} -eq ${NbProc} \) -o \( ${idx} -eq ${nbfiles} \) ] ; then

      Log_Print INFO "   Waiting until all background processes are completed ..."
      wait
      nbproc=0 #----- Reset number of processes.

      #----- Verify if Metfields has terminated successfully.
      nbline=`grep "METFLD1.*FIN" metfields.*.out | wc -l`
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

Log_Print INFO "Executing EDITFST: Merging the two meteorological files (PGSM + metfields) into one standard file for MLDP1 ..."

#----- Create editfst directives file.
cat <<EOF_EDITFST > editfst.dir
 DESIRE(-1,['HY'],-1,-1,-1,-1,-1)
EOF_EDITFST

for file in ${ArrayStdFiles[@]} ; do

    #----- Initialize output filenames.
    filename=`basename ${file}`

    #----- Merge the 'HY' record with meteorological file (metfields) into one standard file for MLDP1.
    Log_Print INFO "Processing standard file ${filename}.std ..."
    editfst+ \
       -s ../meteo/${filename}.std \
       -d ../meteo/${filename}.met.std \
       -i editfst.dir \
       >editfst.${filename}.out 2>editfst.${filename}.err
    mv ../meteo/${filename}.met.std ../meteo/${filename}.std
done

Log_End 0
