#===============================================================================
# Environnement Canada - Service meteorologique du Canada
# Centre meteorologique canadien
# 2121 Route Trans-canadienne
# Dorval, Quebec
# H9P 1J3
#
# Projet     : Logger to generalize the logging mechanism among the jobs.
# Nom        : <Logger.sh>
# Creation   : Juin 2009 - J.P. Gauhier - CMC/CMOE
#
# Description: Functions to generalize the loggin mechanism amongs various jobs:
#
# Global Settings
#   LOG_LEVEL      Logging level (MUST,ERROR,WARNING,INFO,DEBUG)
#   LOG_MAIL       EMail address for mail reports
#   LOG_MAILTITLE Title to include in mail reports
#   LOG_JOBID      Job Identificator
#   LOG_FILE       File to use for log (stdout if undefined)
#   LOG_TIME       Include dates in log
#   LOG_MODE       Mode de log (AUTO=automated jobs, send emails on error)
#
# Functions:
#   Log_Start     $Job $Version [$Paramfile]
#   Log_Mail      $Subject $File
#   Log_End       $ExitStatus
#   Log_Print     $Level $Message $Seconds
#   Log_Time      $Seconds
#
# Retour     :
#
# Remarques  :
#   Aucune.
#===============================================================================

#----- Check for environment settings
LOG_LEVEL=${LOG_LEVEL:=INFO}
LOG_MODE=${LOG_MODE:=SCRIPT}
LOG_MAIL=${LOG_MAIL:=""}
LOG_MAILTITLE=${LOG_MAILTITLE:="Job Info"}
LOG_JOBID=${LOG_JOBID:=""}
LOG_FILE=${LOG_FILE:=""}
LOG_TIME=${LOG_TIME:=0}
LOG_OC=""

#----- Logger internal variables
LogSecTime=`date +%s`
LogSecStart=${SECONDS}
LogSecEnd=${SECONDS}
LogJob="Unknown"
LogVersion="Unknown"

LogMUST=-1
LogERROR=0
LogWARNING=1
LogINFO=2
LogDEBUG=3
LogLevelNo=${LogINFO}

function realpath {
   fname=${1%/} # strips trailing '/'
   while [ -L "$fname" ]; do
      oldfname="$fname"
      fname="$(command ls -l $fname)"
      fname="${fname#*\> }"
      if [ "$fname" = . ] ; then
         fname="$(dirname $oldfname)"
      elif echo $fname | grep -vq '^/' - ; then
         fname="$(dirname $oldfname)/$fname"
      fi
   done
   pushd $(dirname $fname) > /dev/null
   fname=$(pwd -P)/$(basename $fname)
   popd > /dev/null
   echo $fname
}

function Log_MailIf {

   subject=${1}
   file=${2}

   if [[ ${LOG_MODE} = "ALL" ]]; then
      Log_Mail "$subject" $file
   fi
}

function Log_Mail {

   subject=${1}
   file=${2}

   if [[ ${LOG_MAIL} = "" ]] ; then
      return 0
   fi

   if [[ -r ${file} ]] ;then
      if [[ ${EER_ARCH} = IRIX64 ]] ; then
         mailx -s "${LOG_MAILTITLE}: ${subject} (${LOG_JOBID})" ${LOG_MAIL} < ${file}
      else
         mail -s "${LOG_MAILTITLE}: ${subject} (${LOG_JOBID})" ${LOG_MAIL} < ${file}
      fi
   else
      if [[ ${EER_ARCH} = IRIX64 ]] ; then
         printf $file | mailx -s "${LOG_MAILTITLE}: ${subject} (${LOG_JOBID})" ${LOG_MAIL}
      else
         printf $file | mail -s "${LOG_MAILTITLE}: ${subject} (${LOG_JOBID})" ${LOG_MAIL}
      fi
   fi
}

function Log_Start {

   LogSecStart=${SECONDS}
   LogJob=${1}
   LogVersion=${2}
   in=${3}

   #----- Simulation run time ID.
   if [[ ${LOG_JOBID} = "" ]] ; then
      #----- Define run time ID if not defined.
      LOG_JOBID=`date +%Y%m%d%H%M%S`
   fi

   if [[ ! ${LOG_FILE} = "" ]] ; then
      rm -f ${LOG_FILE}
   fi

   Log_Print MUST "-------------------------------------------------------------------------------"
   Log_Print MUST "Script              : ${LogJob}"
   Log_Print MUST "Version             : ${LogVersion}"
   Log_Print MUST "Hostname            : `hostname`"
   Log_Print MUST "Architecture        : `uname -s`"
   Log_Print MUST "Run ID              : ${LOG_JOBID}"

   if [[ ${LOG_MAIL} != "" ]] ; then
      Log_Print MUST "E-mail Address      : ${LOG_MAIL}"
   fi

   #----- Queue stuff
   if [[ ! $LOADL_STEP_ID = "" ]]; then
      Log_Print MUST "Queue Method        : llv"
      Log_Print MUST "   Queue            : $LOADL_STEP_CLASS"
      Log_Print MUST "   Job ID           : $LOADL_STEP_ID"

      if [[ -r ${in} ]]; then
         secs0=`date +%s`
         eval secs=\`perl -e \'\$mtime=\(stat\(\"${in}\"\)\)\[9\]\; print ${secs0}-\$mtime\'\`
         Log_Print MUST "   Waiting time     : $(Log_Time ${secs})"
      fi
    elif [[ ! $SGE_CELL = "" ]]; then
      Log_Print MUST "Queue Method        : sge"
      Log_Print MUST "   Queue               : $QUEUE"
      Log_Print MUST "   Job ID              : $JOB_ID"
      if [[ -r ${in} ]]; then
         secs0=`date +%s`
         eval secs=\`perl -e \'\$mtime=\(stat\(\"${in}\"\)\)\[9\]\; print ${secs0}-\$mtime\'\`
         Log_Print MUST "   Waiting time     : $(Log_Time ${secs})"
      fi
   fi

   Log_Print MUST "Start time          : `date +\"%c %Z\"`"
   Log_Print MUST "-------------------------------------------------------------------------------\n"

   Log_MailIf "Job started" ${LOG_FILE}
}

function Log_End {

   status=${1}
   exitnow=${2}
   LogSecEnd=${SECONDS}

   Log_Print MUST "\n-------------------------------------------------------------------------------"
   if [[ ${status} -eq 0 ]]; then
      Log_Print MUST "Status              : Job has terminated successfully."
   else
      Log_Print MUST "Status              : Job has encountered some errors."
   fi
   Log_Print MUST "End time            : `date +\"%c %Z\"`"
   Log_Print MUST "Total running time  : $(Log_Time `expr ${LogSecEnd} - ${LogSecStart}`)"
   Log_Print MUST "-------------------------------------------------------------------------------\n"

   if [[ ${status} -eq 0 ]]; then
      Log_MailIf "Job finished (NORMAL)" ${LOG_FILE}
   else
      Log_Mail "Job finished (ERROR)" ${LOG_FILE}
   fi

   if [[ ${exitnow} = "" ]]; then
      exit $status
   fi
}

function Log_Print {

   level=${1}
   msg=${2}
   time=${3}

   #----- Levels are MUST,ERROR,WARNING,INFO,DEBUG
   eval lvl=\$\{Log$level\}
   eval LogLevelNo=\$\{Log${LOG_LEVEL}\}

   #----- Format the time if it is specified
   if [[ ${time} != "" ]]; then
      time=$(Log_Time $time)
   fi

   #----- Check for event time
   datetime=""
   if [[ ${LOG_TIME} -eq 1 ]]; then
      datetime="($(date)) "
   fi

   if [[ ${lvl} -le ${LogLevelNo} ]]; then
      if [[ ${level} = "MUST" ]] ; then
         datetime=""
         levels=""
      else
         levels="(${level}) "
      fi

      if [[ ${LOG_FILE} = "" ]] ; then
         printf "${datetime}${levels}${msg} ${time}\n"
      else
         printf "${datetime}${levels}${msg} ${time}\n" >> ${LOG_FILE}
      fi
      if [[ $level = "ERROR" ]] ; then
         printf "${datetime}${levels}${msg} ${time}\n" 1>&2
         if [[ ${LOG_OC} != "" ]]; then
            oclog ${LogJob} x "${LOG_OC}\n\n${datetime}${levels}${msg} ${time}"
         fi
      fi
   fi
}

function Log_Time {

   secs=$1

   hours=$((secs / 3600))
   secs=$((secs % 3600))
   mins=$((secs / 60))
   secs=$((secs % 60))

   printf "%02d:%02d:%02d\n" $hours $mins $secs
}
