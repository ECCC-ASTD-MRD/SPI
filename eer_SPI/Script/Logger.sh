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
#   LogLevel      Logging level (MUST,ERROR,WARNING,INFO,DEBUG)
#   LogMail       EMail address for mail reports
#   LogMailTitle  Title to include in mail reports
#   LogJobID      Job Identificator
#   LogFile       File to use for log (stdout if undefined)
#   LogTime       Include dates in log
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
LogLevel=${LogLevel:=INFO}
LogMail=${LogMail:=""}
LogMailTitle=${LogMailTitle:="Job Info"}
LogJobID=${LogJobID:=""}
LogFile=${LogFile:=""}
LogTime=${LogTime:=0}

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

function Log_Mail {

   subject=${1}
   file=${2}

   if [[ ${LogMail} = "" ]] ; then
      return 0
   fi

   if [[ ! -r ${file} ]] ;then
      file=/dev/null
   fi

   if [[ ${EER_ARCH} = IRIX64 ]] ; then
      mailx -s "${LogMailTitle}: ${subject} (${LogJobID})" ${LogMail} < ${file}
   else
      mail -s "${LogMailTitle}: ${subject} (${LogJobID})" ${LogMail} < ${file}
   fi
}

function Log_Start {

   LogSecStart=${SECONDS}
   LogJob=${1}
   LogVersion=${2}
   in=${3}

  #----- Simulation run time ID.
   if [[ ${LogJobID} = "" ]] ; then
      #----- Define run time ID if not defined.
      LogJobID=`date +%Y%m%d%H%M%S`
   fi

   if [[ ! ${LogFile} = "" ]] ; then
      rm -f ${LogFile}
   fi

   Log_Print MUST "-------------------------------------------------------------------------------"
   Log_Print MUST "Script              : ${LogJob}"
   Log_Print MUST "Version             : ${LogVersion}"
   Log_Print MUST "Hostname            : `hostname`"
   Log_Print MUST "Architecture        : `uname -s`"
   Log_Print MUST "Run ID              : ${LogJobID}"

   if [[ ${LogMail} != "" ]] ; then
      Log_Print MUST "E-mail Address      : ${LogMail}"
   fi

   #----- Queue stuff
   if [[ ! $LOADL_STEP_ID = "" ]]; then
      Log_Print MUST "Queue Method        : llv"
      Log_Print MUST "   Queue            : $LOADL_STEP_CLASS"
      Log_Print MUST "   Job ID           : $LOADL_STEP_ID"
      if [[ -r ${in} ]]; then
         eval secs=\`perl -e \'\$mtime=\(stat\(\"${in}\"\)\)\[9\]\; print \$mtime\'\`
         Log_Print MUST "   Waiting time     : $(Log_Time `expr ${LogSecTime}-${secs}`)"
      fi
    elif [[ ! $SGE_CELL = "" ]]; then
      Log_Print MUST "Queue Method        : sge"
      Log_Print MUST "   Queue               : $QUEUE"
      Log_Print MUST "   Job ID              : $JOB_ID"
      if [[ -r ${in} ]]; then
         eval secs=\`perl -e \'\$mtime=\(stat\(\"${in}\"\)\)\[9\]\; print \$mtime\'\`
         Log_Print MUST "   Waiting time     : $(Log_Time `expr ${LogSecTime}-${secs}`)"
      fi
   fi

   Log_Print MUST "Start time          : `date +\"%c %Z\"`"
   Log_Print MUST "-------------------------------------------------------------------------------\n"

   Log_Mail "Job started" ${LogFile}
}

function Log_End {

   status=${1}
   LogSecEnd=${SECONDS}

   Log_Print MUST "\n -------------------------------------------------------------------------------"
   if [[ ${status} -eq 0 ]] ; then
      Log_Print MUST "Status              : Job has terminated successfully."
   else
      Log_Print MUST "Status              : Job has encountered some errors."
   fi
   Log_Print MUST "End time            : `date +\"%c %Z\"`"
   Log_Print MUST "Total running time  : $(Log_Time `expr ${LogSecEnd} - ${LogSecStart}`)"
   Log_Print MUST "-------------------------------------------------------------------------------\n"

   if [[ ${status} -eq 0 ]] ; then
      Log_Mail "Job finished (NORMAL)" ${LogFile}
   else
      Log_Mail "Job finished (ERROR)" ${LogFile}
   fi

   exit $status
}

function Log_Print {

   level=${1}
   msg=${2}
   time=${3}

   #----- Levels are MUST,ERROR,WARNING,INFO,DEBUG
   eval lvl=\$\{Log$level\}
   eval LogLevelNo=\$\{Log${LogLevel}\}

   #----- Format the time if it is specified
   if [[ ${time} != "" ]]; then
      time=$(Log_Time $time)
   fi

   #----- Check for event time
   datetime=" "
   if [[ ${LogTime} -eq 1 ]]; then
      datetime=" ($(date)) "
   fi

   if [[ ${lvl} -le ${LogLevelNo} ]]; then
      if [[ ${level} = "MUST" ]] ; then
         levels=""
      else
         levels="(${level})"
      fi

      if [[ ${LogFile} = "" ]] ; then
         echo "${levels}${datetime}${msg} ${time}"
      else
         echo "${levels}${datetime}${msg} ${time}" >> ${LogFile}
      fi
      if [[ $level = "ERROR" ]] ; then
         echo "${levels}${datetime}${msg} ${time}" 1>&2
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
