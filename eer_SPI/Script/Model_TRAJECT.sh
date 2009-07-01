#!/bin/ksh
#===============================================================================
# Environnement Canada - Service meteorologique du Canada
# Centre meteorologique canadien
# 2121 Route Trans-canadienne
# Dorval, Quebec
# H9P 1J3
#
# Projet     : TRAJECT model processing interfaces.
# Nom        : <Model_TRAJECT.sh>
# Creation   : Juin 2009 - J.P. Gauthier - CMC/CMOE
#
# Description: Support file for MLDP0 and MLDP1 models, it defines the following functions:
#                - {Model}_Pre     : Do the pre-processing stuff
#                - {Model}_Run     : Run the model
#                - {Model}_Post    : Do the post-processing stuff
#
# Remarques  :
#   Aucune.
#===============================================================================

function TRAJECT_Pre {
  return 0
}

function TRAJECT_Post {
   return 0
}

function TRAJECT_Run {

   ${MODEL_TIMER} ${EER_DIRBIN}/${MODEL_NAME}${MODEL_TYPE} \
      -i ${TRAJECT_INPUT} \
      -fich10 ${TRAJECT_METEO} \
      -tinc ${TRAJECT_INC} \
      -tlen ${TRAJECT_LEN} \
      -o ${TRAJECT_RESULT} \
      -v ${TRAJECT_LOGLEVEL} \
      >${MODEL_TMPDIR}/${MODEL_NAME}${MODEL_TYPE}.out 2>${MODEL_TMPDIR}/${MODEL_NAME}${MODEL_TYPE}.err

   taskstatus=$?
   MODEL_EXITSTATUS=$((MODEL_EXITSTATUS+$taskstatus))

   #----- Verify if model has terminated successfully.
   if [[ ${taskstatus} -eq 0 ]] ; then

      #----- Copy results file to local directory.
      if [[ ${MODEL_NEEDCOPY} -eq 1 ]] ; then

         Log_Print INFO "Copying following model output files to temporary directory on local host (${MODEL_LOCALHOST}):"

         sec=${SECONDS}

         #----- Copy model results to local directory.
         scp -p ${MODEL_RUNDIR}/results/* ${MODEL_USER}@${MODEL_LOCALHOST}:${MODEL_LOCALDIR}/results
         status=${?}
         MODEL_EXITSTATUS=$((MODEL_EXITSTATUS+$status))

         if [[ ${status} -eq 0 ]] ; then
            Log_Print INFO "Model results have been copied successfully to results directory on local host (${MODEL_LOCALHOST})."
         else
            Log_Print ERROR "Problems while copying model results to results directory on local host (${MODEL_LOCALHOST})."
         fi

         Log_Print INFO "Elapsed time copying reults:" `expr ${SECONDS}-${sec}`
      fi
      Log_Mail "Atmospheric transport/dispersion model (NORMAL)" ${MODEL_TMPDIR}/${MODEL_NAME}${MODEL_TYPE}.out
   else
      Log_Print ERROR "${MODEL_NAME}${MODEL_TYPE} has encountered an error."
      Log_Mail "Atmospheric transport/dispersion model (ERROR)" ${MODEL_TMPDIR}/${MODEL_NAME}.err
   fi

   return ${taskstatus}
}
