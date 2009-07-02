#!/bin/ksh
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
#
# Remarques  :
#   Aucune.
#===============================================================================

function MLDP_Pre {

   ${EER_DIRSCRIPT}/Model_Meteo${MODEL_NAME}${MODEL_TYPE}.sh ${MODEL_TMPDIR} ${MLDP_METEO} ${MODEL_PRE} ${MLDP_GRIDDEF} ${MLDP_LOGLEVEL} \
      >${MODEL_TMPDIR}/Model_Meteo${MODEL_NAME}${MODEL_TYPE}.out 2>${MODEL_TMPDIR}/Model_Meteo${MODEL_NAME}${MODEL_TYPE}.err
   taskstatus=$?
   MODEL_EXITSTATUS=$((MODEL_EXITSTATUS+$taskstatus))

   if [[ ${taskstatus} -eq 0 ]] ; then
      Log_Mail "Meteorological preprocessing (NORMAL)" ${MODEL_TMPDIR}/Model_Meteo${MODEL_NAME}${MODEL_TYPE}.out
   else
      Log_Print ERROR "Problems in metfield calculations."
      Log_Mail "Meteorological preprocessing (ERROR)" ${MODEL_TMPDIR}/Model_Meteo${MODEL_NAME}${MODEL_TYPE}.err
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
   if [[ ${EER_ARCH} = "Linux" && ${MODEL_ISREMOTE} -eq 0 && ${MODEL_TYPE} = "1" ]] ;then
      export PATH=/home/dormrb02/ssm-mpich2-pgi6/mpich2_1.0.6_linux24-i386/bin:${PATH}
      export OMP_NUM_THREADS=${MODEL_NBOMPTHREADS}
      Log_Print DEBUG "Version of r.mpiexec: `which r.mpiexec`"
      ${MODEL_TIMER} r.mpiexec \
         -npex ${MODEL_NBMPITASKS} \
         -args "\-input ${MLDP_INPUT} \-print ${MLDP_LOGLEVEL} \-seed ${MLDP_SEED} \-source ${MLDP_SOURCE} \-outmode ${MLDP_OUTMODE}" \
         -pgm ${EER_DIRBIN}/${MODEL_NAME}${MODEL_TYPE} \
         >${MODEL_TMPDIR}/${MODEL_NAME}${MODEL_TYPE}.out 2>${MODEL_TMPDIR}/${MODEL_NAME}${MODEL_TYPE}.err
   else
      ${MODEL_TIMER} ${EER_DIRBIN}/${MODEL_NAME}${MODEL_TYPE} \
         -input ${MLDP_INPUT} \
         -print ${MLDP_LOGLEVEL} \
         -seed ${MLDP_SEED} \
         -source ${MLDP_SOURCE} \
         -outmode ${MLDP_OUTMODE} \
         >${MODEL_TMPDIR}/${MODEL_NAME}${MODEL_TYPE}.out 2>${MODEL_TMPDIR}/${MODEL_NAME}${MODEL_TYPE}.err
   fi
   taskstatus=$?
   MODEL_EXITSTATUS=$((MODEL_EXITSTATUS+$taskstatus))

   #----- Create small model output file for email.
   res=`awk -F"," '{print $5}' ${MODEL_TMPDIR}/griddef.in | cut -d"." -f1`
   res=`expr ${res} / 100`
   egrep -v "Read\([0-9]*\) |Write\([0-9]*\) | X | N | S | 0 | ${res} " ${MODEL_TMPDIR}/${MODEL_NAME}${MODEL_TYPE}.out > ${MODEL_TMPDIR}/${MODEL_NAME}${MODEL_TYPE}_email.out

   #----- Verify if model has terminated successfully.
   if [[ ${taskstatus} -eq 0 ]] ; then
      Log_Mail "Atmospheric dispersion model (NORMAL)" ${MODEL_TMPDIR}/${MODEL_NAME}${MODEL_TYPE}_email.out
   else
      Log_Print ERROR "${MODEL_NAME}${MODEL_TYPE} has encountered an error."
      Log_Mail "Atmospheric dispersion model (ERROR)" ${MODEL_TMPDIR}/${MODEL_NAME}.err
   fi

   return ${taskstatus}
}
