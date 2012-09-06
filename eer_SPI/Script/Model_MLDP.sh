#===============================================================================
# Environnement Canada - Service meteorologique du Canada
# Centre meteorologique canadien
# 2121 Route Trans-canadienne
# Dorval, Quebec
# H9P 1J3
#
# Projet     : MLDP0 and MLDP1 model processing interfaces.
# Nom        : <Model_MLDP.sh>
# Creation   : Juin 2009 - J.P. Gauthier - CMC/CMOE
#
# Description: Support file for MLDP0 and MLDP1 models, it defines the following functions:
#                - {Model}_Pre     : Do the pre-processing stuff
#                - {Model}_Run     : Run the model
#                - {Model}_Post    : Do the post-processing stuff
# Variables:
#    MLDP_METEO              Meteo type or path to meteo files if already interpolated
#    MLDP_GRIDDEF            Grid dimension
#    MLDP_INPUT              Model input file
#    MLDP_LOGLEVEL           Model log level
#    MLDP_SEED               Random number seed mode
#    MLDP_SOURCE             Soure type
#    MLDP_OUTMODE            Output mode
#
# Remarques  :
#   Aucune.
#===============================================================================

#----- Patch to use old model until RPB upgrades ARGOS
ARGOS=""
if [[ ${MLDP_OUTMODE} = "argos" ]] ;then
   ARGOS="-ARGOS"
fi

function MLDP_Pre {

   taskstatus=0

   #----- Check for pre-calculated meteo.
   if [[ ${#MLDP_METEO} -gt 10 ]] ; then
      Log_Print INFO "Metfields are pre-calculated."
   else
      ${EER_DIRSCRIPT}/Model_Meteo${MODEL_NAME}${MODEL_TYPE}.sh ${MODEL_RUNDIR} ${MLDP_METEO} ${MODEL_PRE} ${MLDP_GRIDDEF} ${MLDP_LOGLEVEL} ${ARGOS}\
         >tmp/Model_Meteo${MODEL_NAME}${MODEL_TYPE}.out 2>tmp/Model_Meteo${MODEL_NAME}${MODEL_TYPE}.err
      taskstatus=$?
      MODEL_EXITSTATUS=$((MODEL_EXITSTATUS+$taskstatus))

      if [[ ${taskstatus} -eq 0 ]] ; then
         if [[ ${LOG_JOBCLASS} = "INTERACTIVE" ]]; then
            Log_Mail "Meteorological preprocessing done (NORMAL)" tmp/Model_Meteo${MODEL_NAME}${MODEL_TYPE}.out
         fi
      else
         Log_Print ERROR "Problems in metfield calculations, Meteorological data might be being written, if this is the case, wait 10 minutes before trying again."
         Log_Mail "Meteorological preprocessing done (ERROR)" tmp/Model_Meteo${MODEL_NAME}${MODEL_TYPE}.err
      fi
   fi

   return ${taskstatus}
}

function MLDP_Post {

   if [[ ! ${MODEL_SOFTWARE} = ARGOS ]] ; then
      Model_PoolEncode
   fi

   return 0
}

function MLDP_Run {

   export MLDP0_PARAMS=""
   export MLDP1_MPI_OMP_PARAMS=""

   #----- Check for MPI params.
   if [[ `uname` = "Linux" && ${MODEL_ISREMOTE} -eq 0 && ${MODEL_TYPE} = "1" ]] ;then
      export OMP_NUM_THREADS=${MODEL_NBOMPTHREADS}
      ${MODEL_TIMER} r.mpirun2 \
         -npex ${MODEL_NBMPITASKS} \
         -args "\-input ${MLDP_INPUT} \-print ${MLDP_LOGLEVEL} \-seed ${MLDP_SEED} \-source ${MLDP_SOURCE} \-outmode ${MLDP_OUTMODE}" \
         -pgm ${EER_DIRBIN}/${MODEL_NAME}${MODEL_TYPE}${ARGOS} \
         >tmp/${MODEL_NAME}${MODEL_TYPE}.out 2>tmp/${MODEL_NAME}${MODEL_TYPE}.err
   else
      ${MODEL_TIMER} r.mpirun2 ${EER_DIRBIN}/${MODEL_NAME}${MODEL_TYPE}${ARGOS} \
         -input ${MLDP_INPUT} \
         -print ${MLDP_LOGLEVEL} \
         -seed ${MLDP_SEED} \
         -source ${MLDP_SOURCE} \
         -outmode ${MLDP_OUTMODE} \
         >tmp/${MODEL_NAME}${MODEL_TYPE}.out 2>tmp/${MODEL_NAME}${MODEL_TYPE}.err
   fi
   taskstatus=$?
   MODEL_EXITSTATUS=$((MODEL_EXITSTATUS+$taskstatus))

   #----- Create small model output file for email.
   res=`awk -F"," '{print $5}' tmp/griddef.in | cut -d"." -f1`
   res=`expr ${res} / 100`
   egrep -v "Read\([0-9]*\) |Write\([0-9]*\) | X | N | S | 0 | ${res} " tmp/${MODEL_NAME}${MODEL_TYPE}.out > tmp/${MODEL_NAME}${MODEL_TYPE}_email.out

   #----- Verify if model has terminated successfully.
   if [[ ${taskstatus} -eq 0 ]] ; then
      if [[ ${LOG_JOBCLASS} = "INTERACTIVE" ]]; then
         Log_Mail "Atmospheric dispersion model done (NORMAL)" tmp/${MODEL_NAME}${MODEL_TYPE}_email.out
      fi
   else
      Log_Print ERROR "${MODEL_NAME}${MODEL_TYPE} has encountered an error."
      Log_Mail "Atmospheric dispersion model done (ERROR)" tmp/${MODEL_NAME}.err
   fi

   return ${taskstatus}
}
