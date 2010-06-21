#===============================================================================
# Environnement Canada - Service meteorologique du Canada
# Centre meteorologique canadien
# 2121 Route Trans-canadienne
# Dorval, Quebec
# H9P 1J3
#
# Projet     : MLDC model processing interfaces.
# Nom        : <Model_MLCD.sh>
# Creation   : Juin 2009 - J.P. Gauthier - CMC/CMOE
#
# Description: Support file for MLCD models, it defines the following functions:
#                - {Model}_Pre     : Do the pre-processing stuff
#                - {Model}_Run     : Run the model
#                - {Model}_Post    : Do the post-processing stuff
# Variables:
#    MLCD_MODE               Mode type (0: forward, 1: backward).
#    MLCD_EMISSION           Release input file.
#    MLCD_WIND               Windfield input file.
#    MLCD_POS                Output position file.
#    MLCD_CONC               Output concentration file.
#    MLCD_GRID               Type of grid.
#    MLCD_ALGO               Type of algorithm (0: The nearest grid point, 1: The 4 nearest grid points).
#    MLCD_DOMAIN             Grid domain for concentration calculations [km].
#    MLCD_OUTPUTTS           Output time step [min].
#    MLCD_MODELTS            Model time step [min].
#    MLCD_SEED               Type of seed (0: fixed seed, 1: variable seed)
#
# Remarques  :
#   Aucune.
#===============================================================================

function MLCD_Pre {
  return 0
}

function MLCD_Post {

   #----- Encode the pool info on all of the result files
   Model_PoolEncode

   return 0
}

function MLCD_Run {

   ${MODEL_TIMER} ${EER_DIRBIN}/${MODEL_NAME}${MODEL_TYPE} \
      -mode ${MLCD_MODE} \
      -emission ${MLCD_EMISSION} \
      -windfield ${MLCD_WIND} \
      -pos ${MLCD_POS} \
      -conc ${MLCD_CONC} \
      -grid ${MLCD_GRID} \
      -algo ${MLCD_ALGO} \
      -domain ${MLCD_DOMAIN} \
      -outputts ${MLCD_OUTPUTTS} \
      -modelts ${MLCD_MODELTS} \
      -seed ${MLCD_SEED} \
      -error /dev/null \
      -diag /dev/null \
      >${MODEL_TMPDIR}/${MODEL_NAME}${MODEL_TYPE}.out 2>${MODEL_TMPDIR}/${MODEL_NAME}${MODEL_TYPE}.err

   taskstatus=$?
   MODEL_EXITSTATUS=$((MODEL_EXITSTATUS+$taskstatus))

   #----- Verify if model has terminated successfully.
   if [[ ${taskstatus} -eq 0 ]] ; then
      if [[ ${LOG_JOBCLASS} = "INTERACTIVE" ]]; then
         Log_Mail "Atmospheric dispersion model done (NORMAL)" ${MODEL_TMPDIR}/${MODEL_NAME}${MODEL_TYPE}.out
      fi
   else
      Log_Print ERROR "${MODEL_NAME}${MODEL_TYPE} has encountered an error."
      Log_Mail "Atmospheric dispersion model done (ERROR)" ${MODEL_TMPDIR}/${MODEL_NAME}.err
   fi

   return ${taskstatus}
}
