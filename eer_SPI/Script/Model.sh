#!/bin/ksh
#===============================================================================
# Environnement Canada - Service meteorologique du Canada
# Centre meteorologique canadien
# 2121 Route Trans-canadienne
# Dorval, Quebec
# H9P 1J3
#
# Projet     : Launching interface for various EER models.
# Nom        : <Model.sh>
# Creation   : 22 Juin 2009 - J.P. Gauthier - CMC/CMOE
#
# Description: Take care of all the model run processes.
#              For each supported model, a support file has to be created containing the following functions:
#                - {Model}_Pre     : Do the pre-processing stuff
#                - {Model}_Run     : Runs the model
#                - {Model}_Post    : Do the post-processing stuff
#
# Parametres :
#   ${1}     : Parameters input file.
#   ${2}     : Launching host (Optional)
#
# Retour     :
#   Success code (0=ok, otherwise error).
#
# Remarques  :
#   Aucune.
#===============================================================================

function Model_PoolEncode {

   Log_Print INFO "Encoding pool information within model results standard files."

   for file in `ls -1 ${MODEL_RUNDIR}/results`; do
      ${EER_DIRBIN}/CodeInfo \
         -INFO tmp/sim.pool \
         -FSTD results/$file \
         -CKEY codef \
         -NOMVAR INFO \
         >tmp/CodeInfo.out 2>tmp/CodeInfo.err

      status=$?
      MODEL_EXITSTATUS=$((MODEL_EXITSTATUS+$status))

      if [[ $status -ne 0 ]] ; then
         Log_Print ERROR "Problems while encoding pool information in file ${file}."
      fi
   done

   return 0
}

function Model_PoolSet {

   if [[ ${MODEL_POOL} -gt 0 ]] ; then

      POOL_ERROR=-1;
      POOL_RESTART=0;
      POOL_DONE=1;
      POOL_RUN=2;
      POOL_SUSPEND=3;

      if [[ -f tmp/sim.pool ]]; then

         master=${MODEL_LOCALDIR}/../${MODEL_NAME}${MODEL_TYPE}.pool
         mode=${1}
         status=${2}

         line=`cat tmp/sim.pool`

         #----- Get pool parts
         start=`echo ${line} | cut -d: -f1`
         end=`echo ${line} | cut -d: -f3-`
         state=`echo ${line} | cut -d: -f2`

         token=`echo ${state} | cut -d= -f1`
         if [[ $status -eq 0 ]]; then
            eval state=\"\$\{token\}=\${POOL_${mode}\}\"
         else
            state="${token}=${POOL_ERROR}"
         fi

         #----- Replace pool info.
         if [[ ${MODEL_NEEDCOPY} -eq 1 ]] ; then
            ssh ${MODEL_USER}@${MODEL_RUNHOST} "cp ${master} ${master}.exec; grep -v \"${start}:.*:${end}\" ${master}.exec > ${master}; echo \"${start}:${state}:${end}\" >> ${master}"
            status=$?
            MODEL_EXITSTATUS=$((MODEL_EXITSTATUS+$status))
         else
            cp ${master} ${master}.exec
            grep -v "${start}:.*:${end}" ${master}.exec > ${master}
            echo "${start}:${state}:${end}" >> ${master}
         fi
      fi
   fi
}

function Model_Init {

   #----- Atmospheric transport/dispersion model name.
   if [[ ${MODEL_NAME} = "" ]] ; then
      Log_Print ERROR "Missing model name (MODEL_NAME)."
      exit 1
   fi

   #----- Username.
   if [[ ${MODEL_USER} = "" ]] ; then
      Log_Print ERROR "Missing user name (MODEL_USER)."
      exit 1
   fi

   #----- Local host name.
   if [[ ${MODEL_LOCALHOST} = "" ]] ; then
      Log_Print ERROR "Missing local host name (MODEL_LOCALHOST)."
      exit 1
   fi

   #----- Local host main directory.
   if [[ ${MODEL_LOCALDIR} = "" ]] ; then
      Log_Print ERROR "Missing local main directory (MODEL_LOCALDIR)."
      exit 1
   fi

    #----- Define flag indicating if running job on remote host (MODEL_ISREMOTE).
    if [[ ${MODEL_LOCALHOST} != ${MODEL_RUNHOST} ]] ; then
       MODEL_ISREMOTE=1
       MODEL_NEEDCOPY=1
    fi

   #----- Remote host main directory.
   if [[ ${MODEL_ISREMOTE} -eq 1 && ${MODEL_RUNDIR} = "" ]] ; then
      if [[ -d ${MODEL_LOCALDIR} ]]; then
         MODEL_RUNDIR=${MODEL_LOCALDIR}
         Log_Print WARNING "Missing remote run directory (MODEL_RUNDIR), will use local one (MODEL_LOCALDIR=${MODEL_LOCALDIR})"
      else
         Log_Print ERROR "Missing remote run directory (MODEL_RUNDIR)."
         exit 1
      fi
   fi

   #----- Remote copy of data.
   if [[ ${MODEL_RUNDIR} = ${MODEL_LOCALDIR} ]] ; then
       MODEL_NEEDCOPY=0
   fi

   #----- Number of processes for meteorological preprocessing.
   if [[ ${MODEL_PRE} -gt 64 ]] ; then
      Log_Print ERROR "Wrong number of processes for running meteorological preprocessing (MODEL_PRE=${MODEL_PRE}) [0, 64]."
      exit 1
   fi

   #----- Define host type.
   if [[ ${MODEL_ISREMOTE} -eq 1 ]] ; then
      MODEL_RUNTYPE="remote"

      #----- If data is not already there, pull it form local hist
      if [[ ! -d ${MODEL_RUNDIR} ]]; then
         Log_Print WARNING "Missing remote run directory (MODEL_RUNDIR=${MODEL_RUNDIR}), pulling it from local host (MODEL_LOCALDIR=${MODEL_LOCALDIR})"
         mkdir -p ${MODEL_RUNDIR}/tmp ${MODEL_RUNDIR}/results
         srcp -r ${MODEL_LOCALHOST}:${MODEL_LOCALDIR}/meteo ${MODEL_RUNHOST}:${MODEL_RUNDIR}
         srcp ${MODEL_LOCALHOST}:${MODEL_LOCALDIR}/tmp/*.{in,pool} ${MODEL_RUNHOST}:${MODEL_RUNDIR}/tmp
      fi
   fi
   cd ${MODEL_RUNDIR}

   if [[ ${BASE_ARCH} = AIX ]] ; then
      MODEL_TIMER=hpmcount
   fi

   #----- Local/Remote host parameters.
   Log_Print INFO "Request software                                        : ${MODEL_SOFTWARE}"
   Log_Print INFO "User name                                               : ${MODEL_USER}"
   Log_Print INFO "Local host name                                         : ${MODEL_LOCALHOST}"
   Log_Print INFO "Main experiment directory on local host                 : ${MODEL_LOCALDIR}"

   if [[ ${MODEL_ISREMOTE} -eq 1 ]] ; then
      Log_Print INFO "Running job on host                                     : ${MODEL_RUNHOST}"
      Log_Print INFO "Main experiment directory on remote host                : ${MODEL_RUNDIR}"
   fi

   #----- Meteorological preprocessing parameters.
   if [[ ${MODEL_PRE} -ge 1 ]] ; then
      Log_Print INFO "Meteorological preprocessing parameters"
      Log_Print INFO "   Number of processes for meteorological preprocessing : ${MODEL_PRE}"
      Log_Print INFO "   Number of meteorological files                       : `wc -w tmp/data_std_eta.in | awk '{print $1}'`"
   else
      Log_Print INFO "Meteorological preprocessing not requested"
   fi

   #----- Model parameters.
   if [[ ${MODEL_RUN} -eq 1 ]] ; then
      Log_Print INFO "Model parameters"
      Log_Print INFO "   Model name                                           : ${MODEL_NAME}${MODEL_TYPE}"
   else
      Log_Print INFO "Model run not requested"
   fi
}

function Model_CopyTrace {

   if [[ ${MODEL_TRACE} = "" ]]; then
      return
   fi

   path=`echo ${MODEL_RUNDIR} | tr "/" "\n" | tail -2 | tr "\n" "."`
   trace="tmp/${MODEL_NAME}${MODEL_TYPE}_${path}"

   echo "\n##### Fichier meteo (data_std_eta.in)\n" >> ${trace}
   cat  tmp/data_std_eta.in >> ${trace}

   echo "\n##### Parametres du script lancement (.in)\n" >> ${trace}
   cat  tmp/Model_${MODEL_NAME}${MODEL_TYPE}.in >> ${trace}

   echo "\n##### Output du script lancement (.out)\n" >> ${trace}
   cat  tmp/Model_${MODEL_NAME}${MODEL_TYPE}.out >> ${trace}

   echo "\n##### Erreur du script lancement (.err)\n" >> ${trace}
   cat  tmp/Model_${MODEL_NAME}${MODEL_TYPE}.err >> ${trace}

   echo "\n##### Parametres du modele (.in)\n" >> ${trace}
   cat  tmp/${MODEL_NAME}${MODEL_TYPE}.in >> ${trace}

   echo "\n##### Sortie du modele (.out)\n" >> ${trace}
   head -3000 tmp/${MODEL_NAME}${MODEL_TYPE}.out >> ${trace}

   echo "\n#####  Erreur du modele (.err)\n" >> ${trace}
   cat  tmp/${MODEL_NAME}${MODEL_TYPE}.err >> ${trace}

   #----- Exit function if running locally or not doing meteo.
   if [[ ${MODEL_NEEDCOPY} -eq 1 ]] ; then
      ssh ${MODEL_USER}@${MODEL_LOCALHOST} "mkdir -p ${MODEL_TRACE}"
      scp -p ${trace} ${MODEL_USER}@${MODEL_LOCALHOST}:${MODEL_TRACE}
   else
      mkdir -p ${MODEL_TRACE}
      mv ${trace} ${MODEL_TRACE}

      #----- Faire un menage dans les traces (15 jours de trace)
      find ${MODEL_TRACE} -name "*_*" -mtime +15 -exec rm -f {} \;
   fi
}

function Model_CleanUp {

   #----- Exit function if not remote or erasing.
   if [[ ${MODEL_ISREMOTE} -eq 0 || ${MODEL_NEEDCOPY} -eq 0 || ${MODEL_CLEAN} -eq 0  || ${MODEL_EXITSTATUS} -gt 0 ]] ; then
      return 0
   fi

   Log_Print INFO "Deleting temporary files from remote ${HOSTTYPE} host (${MODEL_RUNTYPE})."

   cd `dirname ${MODEL_RUNDIR}`
   rm -rf ${MODEL_RUNDIR}

   if [[ $? -ne 0 ]] ; then
      Log_Print ERROR "Problems while deleting temporary files."
      return 1
   fi

   return 0
}

function Model_CopyMeteo {

   #----- Exit function if running locally or not doing meteo.
   if [[ ${MODEL_PRE} -eq 0 || ${MODEL_NEEDCOPY} -eq 0 || ${#MLDP_METEO} -gt 10 ]] ; then
      return 0
   fi

   Log_Print INFO "Copying following meteorological data files to meteo directory on local host (${MODEL_LOCALHOST}): \n`ls -l ${MODEL_RUNDIR}/meteo/*`"

   sec=${SECONDS}

   srcp -p ${MODEL_RUNHOST}:${MODEL_RUNDIR}/meteo/* ${MODEL_USER}@${MODEL_LOCALHOST}:${MODEL_LOCALDIR}/meteo
   status=$?
   MODEL_EXITSTATUS=$((MODEL_EXITSTATUS+$status))

   if [[ ${status} -eq 0 ]] ; then
      Log_Print INFO "Meteorological data files have been copied successfully to meteo directory on local host (${MODEL_LOCALHOST})."
      Log_Print INFO "Elapsed time copying meteo:" `expr ${SECONDS}-${sec}`
   else
      Log_Print ERROR "Problems while copying meteorological data files to meteo directory on local host (${MODEL_LOCALHOST})."
   fi

   return ${status}
}

function Model_CopyResult {

   if [[ ${MODEL_NEEDCOPY} -eq 1 ]] ; then

      Log_Print INFO "Copying following model output files to local host (${MODEL_LOCALHOST}): \n`ls -l ${MODEL_RUNDIR}/results/*`"

      sec=${SECONDS}

      #----- Copy model results to local directory.
      srcp -p ${MODEL_RUNHOST}:${MODEL_RUNDIR}/results/* ${MODEL_USER}@${MODEL_LOCALHOST}:${MODEL_LOCALDIR}/results
      status=${?}
      MODEL_EXITSTATUS=$((MODEL_EXITSTATUS+$status))

      if [[ ${status} -eq 0 ]] ; then
         Log_Print INFO "Model results have been copied successfully local host (${MODEL_LOCALHOST})."
      else
         Log_Print ERROR "Problems while copying model results to local host (${MODEL_LOCALHOST})."
      fi

      Log_Print INFO "Elapsed time copying results:" `expr ${SECONDS}-${sec}`
   fi

   return ${status}
}

function Model_CopyLog {

   if [[ ${MODEL_NEEDCOPY} -eq 1 ]] ; then
      Log_Print INFO "Copying log files (output and error) to temporary directory on local host (${MODEL_LOCALHOST})"

      #----- Copy relevant log files to local results directory.
      srcp -p ${MODEL_RUNHOST}:${MODEL_RUNDIR}/tmp/*.err ${MODEL_RUNHOST}:${MODEL_RUNDIR}/tmp/*.out ${MODEL_USER}@${MODEL_LOCALHOST}:${MODEL_LOCALDIR}/tmp
      status=$?
      MODEL_EXITSTATUS=$((MODEL_EXITSTATUS+$status))

      if [[ ${status} -eq 0 ]] ; then
         Log_Print INFO "Log files have been copied successfully to temporary directory on local host (${MODEL_LOCALHOST})."
      else
         Log_Print ERROR "Problems while copying log files to temporary directory on local host (${MODEL_LOCALHOST})."
      fi
   fi
}

#----- Initialize default values.
MODEL_SOFTWARE=""
MODEL_NAME=""
MODEL_TYPE=""
MODEL_USER=""
MODEL_TRACE=""
MODEL_LOCALHOST=""
MODEL_LOCALDIR=""
MODEL_RUNHOST=${TRUE_HOST}
MODEL_RUNDIR=""
MODEL_PRE=1
MODEL_RUN=1
MODEL_POST=1
MODEL_POOL=1
MODEL_CLEAN=1
MODEL_NBMPITASKS=0
MODEL_NBOMPTHREADS=0
MODEL_RESTARTABLE=0
MODEL_DIRSCRIPT=${EER_DIRSCRIPT}

#----- Initialize internal variables.
MODEL_RUNTYPE="local"
MODEL_TIMER=time
MODEL_ISREMOTE=0
MODEL_NEEDCOPY=0
MODEL_EXITSTATUS=0

#----- Logger parameters
LOG_TIME=1
LOG_LEVEL=INFO

#----- Read parameters within directives input file.
if [[ -f ${1} ]]
then
   . ${1}
elif [[ -n ${2} ]]
then
   scp ${2}:${1} /tmp/$$.in
   . /tmp/$$.in
   rm /tmp/$$.in
fi

. ~/.profile >/dev/null 2>&1
. ~/.profile.d/.batch_profile >/dev/null 2>&1

#----- Load logging and specific model related functions
. ${EER_DIRSCRIPT}/Logger.sh
. ${MODEL_DIRSCRIPT}/Model_${MODEL_NAME}.sh

Log_Start Model.sh 1.0 ${1}

Model_Init

if [[ ${MODEL_PRE} -gt 0 ]] ; then
   Log_Print INFO "Launching preprocessing for ${MODEL_NAME} on ${MODEL_RUNTYPE} host (${MODEL_RUNHOST})."
   ${MODEL_NAME}_Pre
   status=$?
fi

if [[ ${status} -eq 0 ]] ; then
   if [[ ${MODEL_RUN} -gt 0 ]] ; then
      Log_Print INFO "Launching model ${MODEL_NAME} on ${MODEL_RUNTYPE} host (${MODEL_RUNHOST})."
      ${MODEL_NAME}_Run
      status=$?
   fi
fi

if [[ ${status} -eq 0 ]] ; then
   if [[ ${MODEL_POST} -gt 0 ]] ; then
      Log_Print INFO "Launching postprocessing for ${MODEL_NAME} on ${MODEL_RUNTYPE} host (${MODEL_RUNHOST})."
      ${MODEL_NAME}_Post
   fi
fi

#----- Job finished
Model_CopyResult
Model_CopyMeteo

if [[ ${MODEL_RESTARTABLE} -eq 1 ]] ; then
   Model_PoolSet RESTART ${MODEL_EXITSTATUS}
else
   Model_PoolSet DONE ${MODEL_EXITSTATUS}
fi

Log_End ${MODEL_EXITSTATUS} False

Model_CopyLog
#Model_CopyTrace
Model_CleanUp

exit ${MODEL_EXITSTATUS}
