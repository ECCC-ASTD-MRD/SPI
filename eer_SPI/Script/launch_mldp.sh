#!/bin/ksh
#===============================================================================
# Environnement Canada - Service meteorologique du Canada
# Centre meteorologique canadien
# 2121 Route Trans-canadienne
# Dorval, Quebec
# H9P 1J3
#
# Projet     : Launching interface for MLDP0 and MLDP1 models.
# Nom        : <launch_mldp.ksh>
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
# Script(s) Appelant(s) :
#
# Remarques  :
#   Aucune.
#
# Modifications :
#
#   Nom         : -
#   Date        : -
#   Description : -
#===============================================================================

. ~/.profile > /dev/null 2>&1

#----- Load standard functions
. ${SPI_PATH}/Logger.sh

Log::Start $0 2.0 ${1}

params_input_file=${1}

function ValidateInput {

    #----- Define flag indicating if running job on remote host (isremote).
    if [[ ${localhost} != ${runhost} ]] ; then
       isremote=1
    fi

   #----- Atmospheric transport/dispersion model name.
   if [[ ${model} = "" ]] ; then
      Log::Print ERROR "Missing model name (model)."
      exit 1
   fi

   #----- Username.
   if [[ ${user} = "" ]] ; then
      Log::Print ERROR "Missing user name (user)."
      exit 1
   fi

   #----- Local host name.
   if [[ ${localhost} = "" ]] ; then
      Log::Print ERROR "Missing local host name (localhost)."
      exit 1
   fi

   #----- Local host main directory.
   if [[ ${localdir} = "" ]] ; then
      Log::Print ERROR "Missing local main directory (localdir)."
      exit 1
   fi

   #----- Remote host main directory.
   if [[ ${isremote} -eq 1 && ${rundir} = "" ]] ; then
      Log::Print ERROR "Missing remote main directory (rundir)."
      exit 1
   fi

   #----- Number of processes for meteorological preprocessing.
   if [[ ${runmeteo} -gt 64 ]] ; then
      Log::Print ERROR "Wrong number of processes for running meteorological preprocessing (runmeteo) [0, 64]."
      exit 1
   fi
}

function ParseInput {

    software=SPI
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

    #----- Read parameters within directives input file.
    . ${params_input_file}
}

function Initialize {

   exitstatus=0

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
   if [[ ${ARCH} = AIX ]] ; then
      timing=hpmcount
   fi

   #----- Local/Remote host parameters.
   Log::Print INFO "Request software name                                   : ${software}"
   Log::Print INFO "User name                                               : ${user}"
   Log::Print INFO "Local host name                                         : ${localhost}"
   Log::Print INFO "Main experiment directory on local host                 : ${localdir}"

   if [[ ${isremote} -eq 1 ]] ; then
      Log::Print INFO "Running job on host                                     : ${runhost}"
      Log::Print INFO "Main experiment directory on remote host                : ${rundir}"
   fi

   #----- Meteorological preprocessing parameters.
   if [[ ${runmeteo} -ge 1 ]] ; then
      Log::Print INFO "Meteorological preprocessing parameters"
      Log::Print INFO "   Number of processes for meteorological preprocessing : ${runmeteo}"
      Log::Print INFO "   Number of meteorological files                       : ${nbmetfiles}"
   else
      Log::Print INFO "Meteorological preprocessing not requested"
   fi

   #----- Model parameters.
   if [[ ${runmodel} -eq 1 ]] ; then
      Log::Print INFO "Model parameters"
      Log::Print INFO "   Model name                                           : ${model}"
      Log::Print INFO "   Model directives input file                          : ${tmpdir}/input_${model}.txt"
   else
      Log::Print INFO "Model run not requested"
   fi
}

function CopyMetData {

   #----- Exit function if running locally or not doing meteo.
   if [[ ${isremote} -eq 0 && ${runmeteo} -eq 0 ]] ; then
      return 0
   fi

   Log::Print INFO "Copying following meteorological data files to meteo directory on local host (${localhost}): \n`ls -la ${rundir}/meteo/*.std`"

   sec0=${SECONDS}

   scp -p ${rundir}/meteo/*.std ${user}@${localhost}:${localdir}/meteo
   status=$?
   exitstatus=$((exitstatus+$status))

   if [[ ${status} -eq 0 ]] ; then
      Log::Print INFO "Meteorological data files have been copied successfully to meteo directory on local host (${localhost})."
      Log::TimeFormat `expr ${SECONDS}-${sec0}` "Elapsed time copying meteo:"
   else
      Log::Print ERROR "'scp' command has encountered an error while copying meteorological data files to meteo directory on local host (${localhost})."
   fi

   return 0
}

function CleanUp {

   #----- Exit function if not remote or erasing.
   if [[ ${isremote} -eq 0 || ${cleanup} -eq 0 ]] ; then
      return 0
   fi

   Log::Print INFO "Deleting temporary files from remote host (${runhost})."

   cd `dirname ${rundir}`
   rm -rf ${rundir}

   if [[ $? -ne 0 ]] ; then
      Log::Print ERROR "'rm' command has encountered an error while deleting temporary files."
      return 1
   fi

   return 0
}

function EncodePoolInfo {

   if [[ ${software} = ARGOS ]] ; then
      return 0
   fi

   Log::Print INFO "Encoding pool information within model results standard files on local host (${localhost})."

   ${DIRBIN}/${ARCH}/CodeInfo -INFO sim.pool -FSTD \$posi -CKEY codef -NOMVAR INFO
   ${DIRBIN}/${ARCH}/CodeInfo -INFO sim.pool -FSTD \$posc -CKEY codef -NOMVAR INFO
   status=$?
   exitstatus=$((exitstatus+$status))

   if [[ $status -ne 0 ]] ; then
      Log::Print ERROR "Encode Info module has encountered an error while encoding pool information."
      return 1
   fi

   return 0
}

function LaunchMeteo {

   if [[ ${runmeteo} -eq 0 ]] ; then
      return 0
   fi

   Log::Print INFO "Launching meteorological preprocessing on ${hosttype} (${runhost})."

   ${timing} ${DIRSCRIPT}/InterpolateMeteoFieldsMLDP0.sh ${tmpdir} ${meteo} ${runmeteo} ${griddef} ${debug} >${tmpdir}/InterpolateMeteoFieldsMLDP0.out 2>${tmpdir}/InterpolateMeteoFieldsMLDP0.err
   taskstatus=$?
   exitstatus=$((exitstatus+$taskstatus))

   if [[ ${taskstatus} -eq 0 ]] ; then
      #----- Copy meteorological output files from remote host to local host.
      if [[ ${isremote} -eq 1 ]] ; then
         Log::Print INFO "Copying following output files to temporary directory on local host (${localhost}): \n`ls -la ${rundir}/tmp/out* ${rundir}/tmp/pgsm* ${rundir}/tmp/tmpdir`"

         sec0=${SECONDS}

         #----- Copy relevant log files to local results directory.
         scp -p out* pgsm* sim.* tmpdir ${user}@${localhost}:${localdir}/tmp
         status=$?
         exitstatus=$((exitstatus+$status))

         if [[ ${status} -eq 0 ]] ; then
            Log::Print INFO "Meteorological output files have been copied successfully to temporary directory on local host (${localhost})."
         else
            Log::Print ERROR "'scp' command has encountered an error while copying meteorological output files to temporary directory on local host (${localhost})."
         fi
         Log::TimeFormat `expr ${SECONDS}-${sec0}` "Elapsed time copying log files:"
      fi
   else
      Log::Print ERROR "Problems in metfield calculations."
   fi

   Log::Mail "Meteorological preprocessing" ${tmpdir}/InterpolateMeteoFieldsMLDP0.out

   return ${taskstatus}
}

function LaunchModel {

   if [[ ${runmodel} -eq 0 ]] ; then
      return 0
   fi

   Log::Print INFO "Launching model ${model} on ${hosttype} (${runhost})."
   export MLDP0_PARAMS=""

   ${timing} ${bindir}/${model} -input ${input} -print ${debug} -seed ${seed} -source ${source} -outmode ${outmode} >${tmpdir}/${model}.out 2>${tmpdir}/${model}.err
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

         Log::Print INFO "Copying following model output files to temporary directory on local host (${localhost}):"

         sec0=${SECONDS}

         #----- Copy model results to local directory.
         scp -p ${rundir}/results/* ${user}@${localhost}:${localdir}/results
         status=${?}
         exitstatus=$((exitstatus+$status))

         if [[ ${status} -eq 0 ]] ; then
            Log::Print INFO "Model output results have been copied successfully to results directory on local host (${localhost})."
         else
            Log::Print ERROR "'scp' command has encountered an error while copying model output results to results directory on local host (${localhost})."
         fi

         #----- Copy model traces to local directory.
         scp -p ${tmpdir}/out.exec_${model}.txt ${user}@${localhost}:${localdir}/tmp
         status=$?
         exitstatus=$((exitstatus+$status))

         if [[ ${status} -eq 0 ]] ; then
            Log::Print INFO "Model output files have been copied successfully to temporary directory on local host (${localhost})."
         else
            Log::Print ERROR "'scp' command has encountered an error while copying model output files to temporary directory on local host (${localhost})."
         fi
         Log::TimeFormat `expr ${SECONDS}-${sec0}` "Elapsed time copying reults:"
      fi
   else
      Log::Print ERROR "${model} has encountered an error."
   fi

   Log::Mail "Atmospheric transport/dispersion model" ${tmpdir}/${model}_email.out

   return ${taskstatus}
}

#----- Parse directives and initialize.
ParseInput
ValidateInput
Initialize

#----- Start the job
LaunchMeteo

if [[ $? -eq 0 ]] ; then
    LaunchModel
fi

#----- Job finished
#${DIRSCRIPT}/SimDone.sh ${localdir}/../MLDP0.pool ${exitstatus}
CopyMetData
CleanUp

Log::End ${exitstatus}