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
#    TRAJECT_METEO           List of meteo data files
#    TRAJECT_INC             Increment between simulations
#    TRAJECT_LEN             Lenght of the simulations
#    TRAJECT_INPUT           Input file for model
#    TRAJECT_SPLIT           Split result file by source location
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

   ${MODEL_TIMER} ${EER_DIRBIN}/${MODEL_NAME}${MODEL_TYPE} \
      -i ${TRAJECT_INPUT} \
      -fich10 ${TRAJECT_METEO} \
      -tinc ${TRAJECT_INC} \
      -tlen ${TRAJECT_LEN} \
      -split ${TRAJECT_SPLIT} \
      -o ${TRAJECT_RESULT} \
      -v ${LOG_LEVEL} \
      >${MODEL_TMPDIR}/${MODEL_NAME}${MODEL_TYPE}.out 2>${MODEL_TMPDIR}/${MODEL_NAME}${MODEL_TYPE}.err

   taskstatus=$?
   MODEL_EXITSTATUS=$((MODEL_EXITSTATUS+$taskstatus))

   #----- Verify if model has terminated successfully.
   if [[ ${taskstatus} -eq 0 ]] ; then
      Log_MailIf "Atmospheric dispersion model done (NORMAL)" ${MODEL_TMPDIR}/${MODEL_NAME}${MODEL_TYPE}.out
   else
      Log_Print ERROR "${MODEL_NAME}${MODEL_TYPE} has encountered an error."
      Log_Mail "Atmospheric dispersion model done (ERROR)" ${MODEL_TMPDIR}/${MODEL_NAME}.err
   fi

   return ${taskstatus}
}
