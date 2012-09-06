#===============================================================================
# Environnement Canada - Service meteorologique du Canada
# Centre meteorologique canadien
# 2121 Route Trans-canadienne
# Dorval, Quebec
# H9P 1J3
#
# Projet     : MLDP0 and MLDP1 model processing interfaces.
# Nom        : <Model_MLDPn.sh>
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
#    MLDP_RESULT             Result dir name
#    MLDP_SEED               Random number seed mode
#
# Remarques  :
#   Aucune.
#===============================================================================

#----- Patch to use old model until RPB upgrades ARGOS

function MLDPn_Pre {

   taskstatus=0

   #----- Check for pre-calculated meteo.
   if [[ ${#MLDP_METEO} -gt 10 ]] ; then
      Log_Print INFO "Metfields are pre-calculated."
   else
      ${EER_DIRSCRIPT}/Model_Meteo${MODEL_NAME}.sh ${MODEL_RUNDIR} ${MLDP_METEO} ${MODEL_PRE} ${MLDP_GRIDDEF} ${MLDP_KERNEL} ${MLDP_LOGLEVEL} \
         >tmp/Model_Meteo${MODEL_NAME}.out 2>tmp/Model_Meteo${MODEL_NAME}.err
      taskstatus=$?
      MODEL_EXITSTATUS=$((MODEL_EXITSTATUS+$taskstatus))

      if [[ ${taskstatus} -eq 0 ]] ; then
         if [[ ${LOG_JOBCLASS} = "INTERACTIVE" ]]; then
            Log_Mail "Meteorological preprocessing done (NORMAL)" tmp/Model_Meteo${MODEL_NAME}.out
         fi
      else
         Log_Print ERROR "Problems in metfield calculations, Meteorological data might be being written, if this is the case, wait 10 minutes before trying again."
         Log_Mail "Meteorological preprocessing done (ERROR)" tmp/Model_Meteo${MODEL_NAME}.err
      fi
   fi

   return ${taskstatus}
}

function MLDPn_Post {

   Model_PoolEncode

   return 0
}

function MLDPn_Run {

   unset MODEL_PARAMS

   #----- Check for MPI params.
   if [[ ${ARCH} = "Linux" && ${MODEL_NBMPITASKS} -gt 1 && ${MODEL_ISREMOTE} -eq 0 ]] ;then
      export PATH=/home/dormrb02/ssm-mpich2-pgi6/mpich2_1.0.6_linux24-i386/bin:${PATH}
      Log_Print DEBUG "Version of r.mpiexec: `which r.mpiexec`"
      ${MODEL_TIMER} r.mpiexec \
         -npex ${MODEL_NBMPITASKS} \
         -args "\-i ${MLDP_INPUT} \-o ${MLDP_RESULT} \-v ${LOG_LEVEL} \-s ${MODEL_SEED} \-t ${MODEL_NBOMPTHREADS}" \
         -pgm ${EER_DIRBIN}/${MODEL_NAME} \
         >tmp/${MODEL_NAME}.out 2>tmp/${MODEL_NAME}.err
   else
      ${MODEL_TIMER} r.mpirun2 ${EER_DIRBIN}/${MODEL_NAME} \
         -i ${MLDP_INPUT} \
         -o ${MLDP_RESULT} \
         -v ${LOG_LEVEL} \
         -s ${MODEL_SEED} \
         -t ${MODEL_NBOMPTHREADS} \
         >tmp/${MODEL_NAME}.out 2>tmp/${MODEL_NAME}.err
   fi

   taskstatus=$?
   MODEL_EXITSTATUS=$((MODEL_EXITSTATUS+$taskstatus))

   #----- Verify if model has terminated successfully.
   if [[ ${taskstatus} -eq 0 ]] ; then
      if [[ ${LOG_JOBCLASS} = "INTERACTIVE" ]]; then
         Log_Mail "Atmospheric dispersion model ${MODEL_NAME} done (NORMAL)" tmp/${MODEL_NAME}$.out
      fi
   else
      Log_Print ERROR "${MODEL_NAME} has encountered an error."
      Log_Mail "Atmospheric dispersion model ${MODEL_NAME} done (ERROR)" tmp/${MODEL_NAME}.err
   fi

   return ${taskstatus}
}
