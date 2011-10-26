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
# Description: Support file for TRAJECT models, it defines the following functions:
#                - {Model}_Pre     : Do the pre-processing stuff
#                - {Model}_Run     : Run the model
#                - {Model}_Post    : Do the post-processing stuff
# Variable:
#    TRAJECT_INPUT           Input file for model
#    TRAJECT_RESULT          Result file name
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

#   ${MODEL_TIMER} ${EER_DIRBIN}/${MODEL_NAME}${MODEL_TYPE}
   ${MODEL_TIMER} ${EER_DIRBIN}/Traj-1.5 \
      -i ${TRAJECT_INPUT} \
      -o ${TRAJECT_RESULT} \
      -v ${LOG_LEVEL} \
      >${MODEL_TMPDIR}/${MODEL_NAME}.out 2>${MODEL_TMPDIR}/${MODEL_NAME}.err

   taskstatus=$?
   MODEL_EXITSTATUS=$((MODEL_EXITSTATUS+$taskstatus))

   #----- Verify if model has terminated successfully.
   if [[ ${taskstatus} -eq 0 ]] ; then
      if [[ ${LOG_JOBCLASS} = "INTERACTIVE" ]]; then
         Log_Mail "Atmospheric dispersion model ${MODEL_NAME} done (NORMAL)" ${MODEL_TMPDIR}/${MODEL_NAME}$.out
      fi
   else
      Log_Print ERROR "${MODEL_NAME} has encountered an error."
      Log_Mail "Atmospheric dispersion model ${MODEL_NAME} done (ERROR)" ${MODEL_TMPDIR}/${MODEL_NAME}.err
   fi

   return ${taskstatus}
}
