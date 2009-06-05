#!/bin/ksh
#=============================================================================== 
# Environnement Canada
# Centre Meteorologique Canadien
# Dorval, Quebec
#
# Projet     : Interface pour le modele MLDP0.
# Nom        : <InterpolateMeteoFieldsMLDP0.sh>
# Creation   : 11 March 2005 - A. Malo - CMC/CMOE
#   
# Description: Generate one standard file for trials and prognostics
#              meteorological data required for driving MLDP1 model.
#
# Parametres :
#   ${1}     : Name of metfields binary file in pre-processor for
#              generating required fields for launching MLDP1
#              (including path).
#   ${2}     : Temporary working directory.
#   ${3}     : Type of meteorological model (reg).
#   ${4}     : Number of processes.
#   ${5}     : Printing debug level (low|moderate|high).
#   
# Retour     : one standard file (tape30) for the following fields :
#
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
# Script(s) Appelant(s) :
#   MLDP1.tcl
#
# Remarques  :
#
# Modifications :
#
#
#   Nom         : - A. Malo - CMC/CMOE
#   Date        : - 12 December 2006
#   Description : - Launch several interpolation processes.
#
#   Nom         : - A. Malo - CMC/CMOE
#   Date        : - 2 October 2007
#   Description : - Use environment variable 'SECONDS' to compute elapsed time
#                   rather than 'date' command.
#
#   Nom         : - A. Malo - CMC/CMOE
#   Date        : - 1 February 2008.
#   Description : - Read printing level for debugging.
#
#   Nom         : - A. Malo - CMC/CMOE
#   Date        : - 25 April 2008.
#   Description : - Verify if all meteorological standard files exist
#                   and are readable.
#
#=============================================================================== 

sec=$SECONDS

#----- Source user's profile.
. ~/.profile ####### > /dev/null 2>&1

#----- Get arguments.
MainScript="${0}"
Version="1.14"
BinMetFields="${1}"
DirTmp="${2}"
Model="${3}"
NbProc="${4}"
Debug="${5}"

echo ""
echo "Script name : ${MainScript}"
echo "Version     : ${Version}"

echo ""
echo "MLDP1 pre-processors : ${BinMetFields}"
echo "Temporary directory  : ${DirTmp}"
echo "Meteorological model : ${Model}"
echo "Number of processes  : ${NbProc}"
echo "Printing debug level : ${Debug}"


#----- Define other variables.
MaxNbProc=64
TokenHY="hybrid"
TokenOutMeteo="tape30"
TokenOutMeteoFields="metfields_mldp1"
TokenMeteo="meteo"
DirMeteo="../${TokenMeteo}"
OutMeteo="${DirMeteo}/${TokenOutMeteo}"
OutMeteoFields="${DirMeteo}/${TokenOutMeteoFields}"
OutMetSim="sim.meteo"
OutTmpDir="${DirTmp}/tmpdir"
GridFile="griddef"
ListStdFiles="data_std_sim.eta"
DirectivesPGSM="pgsm_meteo.dir"
DirectivesEDITFST="editfst_meteo.dir"
OutStatus="out.meteo_status.txt"

cd ${DirTmp}

echo "doing" > ${OutMetSim}
echo ${DirTmp} > ${OutTmpDir}

#----- Read the grid parameters from grid file and redirect into "grid" variable.
read < ${GridFile} grid

#----- Create configuration input file for PGSM according to type of meteorological model.

#----- Interpolate met fields over a polar stereographic grid.
#----- Set lower boudary value to 100 m for 'H' (boundary layer height).
#----- Set lower boundary value to 0.0 m/s for 'RT' (precipitation rate).
#----- Set lower boundary value to 0.01 m for 'Z0' (roughness length).
#----- Set lower boundary value to 0.0001 m for 'EN' (turbulent kinetic energy).
#----- Clamp 'HR' (relative humidity) in the range [0,1].
#----- Clamp 'FN' (cloud fraction) in the range [0,1].

cat <<EOF_PGSM_METEO > ${DirectivesPGSM}
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
   
   cat <<EOF_PGSM_METEOr15 >> ${DirectivesPGSM}
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
   
   cat <<EOF_PGSM_METEOg33 >> ${DirectivesPGSM}
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
    string="\n`date +%Y-%m-%d\ %T` : *** Error! Wrong type of meteorological model. Available met models: reg, glb."
    echo "$string"
    echo "$string" >> ${OutStatus}
    exit
fi

#----- Read the list of all standard meteorological files.
read < ${ListStdFiles} stdfiles

set -A ArrayStdFiles ${stdfiles}
nbfiles=${#ArrayStdFiles[@]}

rm -f ${OutStatus}

#----- Validate number of processes.
if [ ${NbProc} -gt ${MaxNbProc} ] ; then
    echo "\n*** Warning! The number of processes if greater than the maximum number of processes declared in the script."
    echo "    Re-initializing number of processes to a smaller value."
    NbProc=${MaxNbProc}
fi

#----- Print number of processes and all standard files to process.
echo "\nNumber of processes: ${NbProc}"
echo "\nTotal number of standard files to process: ${nbfiles}"
echo "\nList of standard files:"

#----- Verify if standard files exist and are readable.
iserror=0
for file in ${ArrayStdFiles[@]} ; do
    
    ls -la ${file}
    exitstatus=${?}
    
    if [ ${exitstatus} -ne 0 ] ; then
        iserror=1
    fi
    
done

if [ ${iserror} -eq 1 ] ; then
    string="\n`date +%Y-%m-%d\ %T` : *** Error! Some standard files do not exist or are not readable."
    echo "$string"
    echo "$string" >> ${OutStatus}
    exit
fi


#----- PGSM.

export FST_OPTIONS="DATATYPE_REMAP=1,134 5,133"

idx=0
nbproc=0
sec0=$SECONDS

string="\n`date +%Y-%m-%d\ %T` : Executing PGSM: Interpolating met fields on the specified grid for standard files ..."
echo "$string"
echo "$string" >> ${OutStatus}

while [ ${idx} -lt ${nbfiles} ] ; do
    
    #----- Initialize output filenames.
    file=${ArrayStdFiles[${idx}]}
    filename=`basename ${file}`
    NewOutMeteoPGSM="${DirMeteo}/${filename}.std"
    OutFile="out.pgsm.${filename}.txt"
    idx=`expr ${idx} + 1` #----- Increment index of current standard file.
    
    #----- Erase old file.
    if [ -r ${NewOutMeteoPGSM} ] ; then
        string="`date +%Y-%m-%d\ %T` : Erasing old standard file ${NewOutMeteoPGSM} ..."
        echo "$string" >> ${OutStatus}
        rm -f ${NewOutMeteoPGSM}
    fi
    
    #----- Verify if standard file exists and is readable.
    if [ ! -r ${file} ] ; then
        string="\n`date +%Y-%m-%d\ %T` : *** Error! The following standard file does not exist or is not readable: ${file}."
        echo "$string"
        echo "$string" >> ${OutStatus}
        exit
    fi
    
    #----- Interpolate meteorological fields for the specified grid and standard file using PGSM.
    string="`date +%Y-%m-%d\ %T` : Processing standard file ${file} (${idx}/${nbfiles}) ..."
    echo "$string"
    echo "$string" >> ${OutStatus}
    (time pgsm+ -iment ${file} -ozsrt ${NewOutMeteoPGSM} -i ${DirectivesPGSM}) > ${OutFile} 2>&1 &
    
    nbproc=`expr ${nbproc} + 1` #----- Increment number of processes.
    
    if [ \( ${nbproc} -eq ${NbProc} \) -o \( ${idx} -eq ${nbfiles} \) ] ; then
        
        string="`date +%Y-%m-%d\ %T` : Waiting until all background processes are completed ..."
        echo "$string"
        echo "$string" >> ${OutStatus}
        
        wait     #----- Wait until all background processes are finished.
        nbproc=0 #----- Reset number of processes.
        
    fi
    
done

wait

sec1=$SECONDS
elapsedtime=`expr ${sec1} - ${sec0}`

string="`date +%Y-%m-%d\ %T` : PGSM done! Elapsed real time: ~ ${elapsedtime} s."
echo "$string"
echo "$string" >> ${OutStatus}

export FST_OPTIONS=""


#----- MLDP1 pre-processor.

idx=0
nbproc=0
sec0=$SECONDS

string="\n`date +%Y-%m-%d\ %T` : Executing MLDP1 pre-processor: Computing other meteorological fields required by the model ..."
echo "$string"
echo "$string" >> ${OutStatus}

while [ ${idx} -lt ${nbfiles} ] ; do
    
    #----- Initialize output filenames.
    filename=`basename ${ArrayStdFiles[${idx}]}`
    NewOutMeteoPGSM="${DirMeteo}/${filename}.std"
    NewOutMeteoFields="${DirMeteo}/${TokenOutMeteoFields}.${filename}.std"
    OutFile="out.${TokenOutMeteoFields}.${filename}.txt"
    idx=`expr ${idx} + 1` #----- Increment index of current standard file.
    
    #----- Erase old file.
    if [ -r ${NewOutMeteoFields} ] ; then
        string="`date +%Y-%m-%d\ %T` : Erasing old standard file ${NewOutMeteoFields} ..."
        echo "$string" >> ${OutStatus}
        rm -f ${NewOutMeteoFields}
    fi
    
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
    string="`date +%Y-%m-%d\ %T` : Processing standard file ${NewOutMeteoPGSM} (${idx}/${nbfiles}) ..."
    echo "$string"
    echo "$string" >> ${OutStatus}
    (time ${BinMetFields} -iment ${NewOutMeteoPGSM} -ozsrt ${NewOutMeteoFields} -print ${Debug}) > ${OutFile} 2>&1 &
    
    nbproc=`expr ${nbproc} + 1` #----- Increment number of processes.
    
    if [ \( ${nbproc} -eq ${NbProc} \) -o \( ${idx} -eq ${nbfiles} \) ] ; then
        
        string="`date +%Y-%m-%d\ %T` : Waiting until all background processes are completed ..."
        echo "$string"
        echo "$string" >> ${OutStatus}
        
        wait     #----- Wait until all background processes are finished.
        nbproc=0 #----- Reset number of processes.
        
    fi
    
done

wait

sec1=$SECONDS
elapsedtime=`expr ${sec1} - ${sec0}`

string="`date +%Y-%m-%d\ %T` : MLDP1 pre-processor done! Elapsed real time: ~ ${elapsedtime} s."
echo "$string"
echo "$string" >> ${OutStatus}


#----- EDITFST.

sec0=$SECONDS

string="\n`date +%Y-%m-%d\ %T` : Executing EDITFST: Merging the 'HY' record with meteorological file (metfields) into one standard file for MLDP1 ..."
echo "$string"
echo "$string" >> ${OutStatus}

#----- Create editfst directives file.
cat <<EOF_EDITFST > ${DirectivesEDITFST}
 DESIRE(-1,['HY'],-1,-1,-1,-1,-1)
EOF_EDITFST

for file in ${ArrayStdFiles[@]} ; do
    
    #----- Initialize output filenames.
    filename=`basename ${file}`
    NewOutMeteoPGSM="${DirMeteo}/${filename}.std"
    NewOutMeteoFields="${DirMeteo}/${TokenOutMeteoFields}.${filename}.std"
    OutMeteoHY="${DirMeteo}/${TokenHY}.${filename}.std"
    OutFile="out.editfst.${filename}.txt"
    
    #----- Merge the 'HY' record with meteorological file (metfields) into one standard file for MLDP1.
    string="`date +%Y-%m-%d\ %T` : Processing standard file ${NewOutMeteoFields} ..."
    echo "$string"
    echo "$string" >> ${OutStatus}
    (time editfst+ -s ${NewOutMeteoPGSM} -d ${OutMeteoHY}        -i ${DirectivesEDITFST}) >  ${OutFile} 2>&1 #----- Grab 'HY' record.
    (time editfst+ -s ${OutMeteoHY}      -d ${NewOutMeteoFields} -i 0                   ) >> ${OutFile} 2>&1 #----- Merge 'HY' record to metfields output file.
    
    #----- Rename final meteorological file.
    string="`date +%Y-%m-%d\ %T` : Rename final meteorological file ..."
#    echo "$string"
    echo "$string" >> ${OutStatus}
    mv -f ${NewOutMeteoFields} ${NewOutMeteoPGSM}
    
    #----- Erase old files.
    string="`date +%Y-%m-%d\ %T` : Erasing temporary old files ..."
#    echo "$string"
    echo "$string" >> ${OutStatus}
    rm -f ${OutMeteoHY}
    
done

sec1=$SECONDS
elapsedtime=`expr ${sec1} - ${sec0}`

string="`date +%Y-%m-%d\ %T` : EDITFST done! Elapsed real time: ~ ${elapsedtime} s."
echo "$string"
echo "$string" >> ${OutStatus}


#----- Concatenate output files.
string="\n`date +%Y-%m-%d\ %T` : Concatenating output files ..."
echo "$string"
echo "$string" >> ${OutStatus}
cat out.pgsm.*.txt > out.pgsm.txt
cat out.${TokenOutMeteoFields}.*.txt > out.${TokenOutMeteoFields}.txt
cat out.editfst.*.txt > out.editfst.txt


#----- Erase old files.
string="\n`date +%Y-%m-%d\ %T` : Erasing temporary old files ..."
echo "$string"
echo "$string" >> ${OutStatus}
rm -f ${DirMeteo}/${TokenOutMeteoFields}.*.std
rm -f out.pgsm.*.txt out.${TokenOutMeteoFields}.*.txt out.editfst.*.txt


#----- End of program.
echo "done" > ${OutMetSim}
sec2=$SECONDS
elapsedtime=`expr ${sec2} - ${sec}`
string="\n`date +%Y-%m-%d\ %T` : Done! Elapsed real time: ~ ${elapsedtime} s.\n"
echo "$string"
echo "$string" >> ${OutStatus}
