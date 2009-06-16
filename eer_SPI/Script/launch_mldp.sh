#!/bin/ksh
#===============================================================================
# Environnement Canada - Service meteorologique du Canada
# Centre meteorologique canadien
# 2121 Route Trans-canadienne
# Dorval, Quebec
# H9P 1J3
#
# Projet     : Launching interface for MLDP0 and MLDP1 models.
# Nom        : <launch_mldp.sh>
# Creation   : 24 October 2007 - A. Malo - CMC/CMOE
#
# Description: Launch following jobs for MLDP0 and MLDP1 models:
#                - meteorological preprocessing ;
#                - atmospheric transport/dispersion model.
#
# Parametres :
#   ${1}     : Parameters input file.
#
# Retour     :
#   Aucun.
#
# Remarques  :
#   Aucune.
#===============================================================================

. ~/.profile >/dev/null 2>&1
. ~/.profile_eer >/dev/null 2>&1

#----- Load standard functions
. ${EER_DIRSCRIPT}/Logger.sh

function Initialize {

    #----- Define flag indicating if running job on remote host (isremote).
    if [[ ${localhost} != ${runhost} ]] ; then
       isremote=1
    fi

   #----- Atmospheric transport/dispersion model name.
   if [[ ${model} = "" ]] ; then
      Log_Print ERROR "Missing model name (model)."
      exit 1
   fi

   #----- Username.
   if [[ ${user} = "" ]] ; then
      Log_Print ERROR "Missing user name (user)."
      exit 1
   fi

   #----- Local host name.
   if [[ ${localhost} = "" ]] ; then
      Log_Print ERROR "Missing local host name (localhost)."
      exit 1
   fi

   #----- Local host main directory.
   if [[ ${localdir} = "" ]] ; then
      Log_Print ERROR "Missing local main directory (localdir)."
      exit 1
   fi

   #----- Remote host main directory.
   if [[ ${isremote} -eq 1 && ${rundir} = "" ]] ; then
      Log_Print ERROR "Missing remote main directory (rundir)."
      exit 1
   fi

   #----- Number of processes for meteorological preprocessing.
   if [[ ${runmeteo} -gt 64 ]] ; then
      Log_Print ERROR "Wrong number of processes for running meteorological preprocessing (runmeteo) [0, 64]."
      exit 1
   fi

   #----- Define host type.
   if [[ ${isremote} -eq 1 ]] ; then
      hosttype="remote host"
      tmpdir=${rundir}/tmp
   else
      hosttype="local host"
      tmpdir=${localdir}/tmp
   fi
   cd ${tmpdir}

   nbmetfiles=`wc -w ${tmpdir}/data_std_sim.eta | awk '{print $1}'`

   timing=time
   if [[ ${EER_ARCH} = AIX ]] ; then
      timing=hpmcount
   fi

   #----- Local/Remote host parameters.
   Log_Print INFO "Request software name                                   : ${software}"
   Log_Print INFO "User name                                               : ${user}"
   Log_Print INFO "Local host name                                         : ${localhost}"
   Log_Print INFO "Main experiment directory on local host                 : ${localdir}"

   if [[ ${isremote} -eq 1 ]] ; then
      Log_Print INFO "Running job on host                                     : ${runhost}"
      Log_Print INFO "Main experiment directory on remote host                : ${rundir}"
   fi

   #----- Meteorological preprocessing parameters.
   if [[ ${runmeteo} -ge 1 ]] ; then
      Log_Print INFO "Meteorological preprocessing parameters"
      Log_Print INFO "   Number of processes for meteorological preprocessing : ${runmeteo}"
      Log_Print INFO "   Number of meteorological files                       : ${nbmetfiles}"
   else
      Log_Print INFO "Meteorological preprocessing not requested"
   fi

   #----- Model parameters.
   if [[ ${runmodel} -eq 1 ]] ; then
      Log_Print INFO "Model parameters"
      Log_Print INFO "   Model name                                           : ${model}"
      Log_Print INFO "   Model directives input file                          : ${tmpdir}/${model}.in"
   else
      Log_Print INFO "Model run not requested"
   fi
}

function CopyMetData {

   #----- Exit function if running locally or not doing meteo.
   if [[ ${isremote} -eq 0 || ${runmeteo} -eq 0 ]] ; then
      return 0
   fi

   Log_Print INFO "Copying following meteorological data files to meteo directory on local host (${localhost}): \n`ls -la ${rundir}/meteo/*.std`"

   sec=${SECONDS}

   scp -p ${rundir}/meteo/*.std ${user}@${localhost}:${localdir}/meteo
   status=$?
   exitstatus=$((exitstatus+$status))

   if [[ ${status} -eq 0 ]] ; then
      Log_Print INFO "Meteorological data files have been copied successfully to meteo directory on local host (${localhost})."
      Log_Print INFO "Elapsed time copying meteo:" `expr ${SECONDS}-${sec}`
   else
      Log_Print ERROR "'scp' command has encountered an error while copying meteorological data files to meteo directory on local host (${localhost})."
   fi

   return 0
}

function CleanUp {

   #----- Exit function if not remote or erasing.
   if [[ ${isremote} -eq 0 || ${cleanup} -eq 0 ]] ; then
      return 0
   fi

   Log_Print INFO "Deleting temporary files from remote host (${runhost})."

   cd `dirname ${rundir}`
   rm -rf ${rundir}

   if [[ $? -ne 0 ]] ; then
      Log_Print ERROR "'rm' command has encountered an error while deleting temporary files."
      return 1
   fi

   return 0
}

function EncodePoolInfo {

   if [[ ${software} = ARGOS ]] ; then
      return 0
   fi

   Log_Print INFO "Encoding pool information within model results standard files on local host (${localhost})."

   for file in `ls -1 ${rundir}/results`; do
      ${EER_DIRBIN}/CodeInfo \
         -INFO sim.pool \
         -FSTD ${rundir}/results/$file \
         -CKEY codef \
         -NOMVAR INFO \
         >${tmpdir}/CodeInfo.out 2>${tmpdir}/CodeInfo.err

      status=$?
      exitstatus=$((exitstatus+$status))

      if [[ $status -ne 0 ]] ; then
         Log_Print ERROR "Problems while encoding pool information inf file ${file}."
      fi
   done

   return 0
}

function LaunchMeteo {

   if [[ ${runmeteo} -eq 0 ]] ; then
      return 0
   fi

   Log_Print INFO "Launching meteorological preprocessing for ${model} on ${hosttype} (${runhost})."

   ${EER_DIRSCRIPT}/InterpolateMeteoFields_${model}.sh ${tmpdir} ${meteo} ${runmeteo} ${griddef} ${debug} >${tmpdir}/InterpolateMeteoFields_${model}.out 2>${tmpdir}/InterpolateMeteoFields_${model}.err
   taskstatus=$?
   exitstatus=$((exitstatus+$taskstatus))

   if [[ ${taskstatus} -eq 0 ]] ; then
      #----- Copy meteorological output files from remote host to local host.
      if [[ ${isremote} -eq 1 ]] ; then
         Log_Print INFO "Copying output files to temporary directory on local host (${localhost})"

         #----- Copy relevant log files to local results directory.
         scp -p pgsm* metfields.* ${user}@${localhost}:${localdir}/tmp
         status=$?
         exitstatus=$((exitstatus+$status))

         if [[ ${status} -eq 0 ]] ; then
            Log_Print INFO "Meteorological output files have been copied successfully to temporary directory on local host (${localhost})."
         else
            Log_Print ERROR "'scp' command has encountered an error while copying meteorological output files to temporary directory on local host (${localhost})."
         fi
      fi
      Log_Mail "Meteorological preprocessing (NORMAL)" ${tmpdir}/InterpolateMeteoFields_${model}.out
   else
      Log_Print ERROR "Problems in metfield calculations."
      Log_Mail "Meteorological preprocessing (ERROR)" ${tmpdir}/InterpolateMeteoFields_${model}.err
   fi


   return ${taskstatus}
}

function LaunchModel {

   if [[ ${runmodel} -eq 0 ]] ; then
      return 0
   fi

   Log_Print INFO "Launching model ${model} on ${hosttype} (${runhost})."
   export MLDP0_PARAMS=""
   export MLDP1_MPI_OMP_PARAMS=""

   #----- Check for MPI params.
   if [[ ${EER_ARCH} = "Linux" && ${isremote} -eq 0 && ${model} = "mldp1" ]] ;then
      export PATH=/home/dormrb02/ssm-mpich2-pgi6/mpich2_1.0.6_linux24-i386/bin:${PATH}
      export OMP_NUM_THREADS=${nbompthreads}
      Log_Print DEBUG "Version of r.mpiexec: `which r.mpiexec`"
      ${timing} r.mpiexec \
         -npex ${nbmpitasks} \
         -args "\-input ${input} \-print ${debug} \-seed ${seed} \-source ${source} \-outmode ${outmode}" \
         -pgm ${EER_DIRBIN}/${model} \
         >${tmpdir}/${model}.out 2>${tmpdir}/${model}.err
   else
      ${timing} ${EER_DIRBIN}/${model} \
         -input ${input} \
         -print ${debug} \
         -seed ${seed} \
         -source ${source} \
         -outmode ${outmode} \
         >${tmpdir}/${model}.out 2>${tmpdir}/${model}.err
   fi
   taskstatus=$?
   exitstatus=$((exitstatus+$taskstatus))

   #----- Create small model output file for email.
   res=`awk -F"," '{print $5}' ${tmpdir}/griddef | cut -d"." -f1`
   res=`expr ${res} / 100`
   egrep -v "Read\([0-9]*\) |Write\([0-9]*\) | X | N | S | 0 | ${res} " ${tmpdir}/${model}.out > ${tmpdir}/${model}_email.out

   #----- Verify if model has terminated successfully.
   if [[ ${taskstatus} -eq 0 ]] ; then
      EncodePoolInfo

      #----- Copy output file to local directory.
      if [[ ${isremote} -eq 1 ]] ; then

         Log_Print INFO "Copying following model output files to temporary directory on local host (${localhost}):"

         sec=${SECONDS}

         #----- Copy model results to local directory.
         scp -p ${rundir}/results/* ${user}@${localhost}:${localdir}/results
         status=${?}
         exitstatus=$((exitstatus+$status))

         if [[ ${status} -eq 0 ]] ; then
            Log_Print INFO "Model output results have been copied successfully to results directory on local host (${localhost})."
         else
            Log_Print ERROR "'scp' command has encountered an error while copying model output results to results directory on local host (${localhost})."
         fi

         #----- Copy model traces to local directory.
         scp -p ${tmpdir}/${model}.out ${tmpdir}/${model}.err ${user}@${localhost}:${localdir}/tmp
         status=$?
         exitstatus=$((exitstatus+$status))

         if [[ ${status} -eq 0 ]] ; then
            Log_Print INFO "Model output files have been copied successfully to temporary directory on local host (${localhost})."
         else
            Log_Print ERROR "'scp' command has encountered an error while copying model output files to temporary directory on local host (${localhost})."
         fi
         Log_Print INFO "Elapsed time copying reults:" `expr ${SECONDS}-${sec}`
      fi
      Log_Mail "Atmospheric transport/dispersion model (NORMAL)" ${tmpdir}/${model}_email.out
   else
      Log_Print ERROR "${model} has encountered an error."
      Log_Mail "Atmospheric transport/dispersion model (ERROR)" ${tmpdir}/${model}.err
   fi

   return ${taskstatus}
}

#----- Initialize default values.
software=""
model=""
user=""
runhost=`hostname`
rundir=""
localhost=""
localdir=""
isremote=0
runmeteo=1
runmodel=1
cleanup=1
exitstatus=0

#----- Read parameters within directives input file.
. ${1}

#----- Start the job
Log_Start launch_mldp.sh 2.0 ${1}

Initialize
LaunchMeteo
if [[ $? -eq 0 ]] ; then
   LaunchModel
   CopyMetData
fi

#----- Job finished
ssh ${user}@${localhost} ${EER_DIRSCRIPT}/SimDone.sh ${localdir}/../`echo ${model} | tr [a-z] [A-Z]`.pool ${localdir}/tmp/sim.pool ${exitstatus}
CleanUp

Log_End ${exitstatus}
