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
#   LOG_LEVEL      Logging level (MUST,ERROR,WARNING,INFO,DEBUG,EXTRA)
#   LOG_MAILTO     EMail address for mail reports
#   LOG_MAILTITLE  Title to include in mail reports
#   LOG_FILE       File to use for log (stdout if undefined)
#   LOG_TIME       Include dates in log
#   LOG_JOBID      Job Identificator
#   LOG_JOBCLASS   Job class (SCRIPT,DAEMON,ORJI,HCRON,INTERACTIVE)
#   LOG_CYCLOPE    Use cyclope (TRUE,FALSE)
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
LOG_TIME=${LOG_TIME:=0}
LOG_PROC=${LOG_PROC:=0}
LOG_FILE=${LOG_FILE:=""}

LOG_MAILTO=${LOG_MAILTO:=""}
LOG_MAILTITLE=${LOG_MAILTITLE:="Job Info"}

LOG_JOB=${LOG_JOB:="Unknown"}
LOG_JOBVERSION=${LOG_JOBVERSION:="Unknown"}
LOG_JOBID=${LOG_JOBID:=""}
LOG_JOBDATE=""
LOG_JOBPATH=${LOG_JOBPATH:=""}
LOG_JOBCLASS=${LOG_JOBCLASS:=SCRIPT}
LOG_JOBCOMMAND=$0
LOG_JOBARGS=$*

LOG_OC=""
LOG_CYCLOPE=FALSE
LOG_CYCLOPEPATH=${HOME}/projets/Cyclope

#----- Logger internal variables
LOG_SECTIME=`date +%s`
LOG_SECSTART=`date +%s`
LOG_SECEND=`date +%s`
LOG_WARNINGS=0

LOG_LEVELMUST=-1
LOG_LEVELERROR=0
LOG_LEVELWARNING=1
LOG_LEVELINFO=2
LOG_LEVELDEBUG=3
LOG_LEVELEXTRA=4
LOG_LEVELNO=${LOG_LEVELINFO}

#----------------------------------------------------------------------------
# Nom      : <Log_Start>
# Creation : Octobre 2009 - J.P. Gauthier - CMC/CMOE
#
# But      : Afficher une message de demarrage standard.
#
# Parametres  :
#    <Job>    : Nom de la job
#    <Version>: Version de la job
#    <Input>  : fichier d'entre (afin de recupere le temps d'attente en queue)
#
# Retour:
#
# Remarques :
#----------------------------------------------------------------------------

function Log_Start {

   LOG_SECSTART=`date +%s`
   LOG_JOB=${1}
   LOG_JOBVERSION=${2}
   in=${3}

   #----- Simulation run time ID.
   LOG_JOBID="${LOG_JOBID}-`date +%Y%m%d_%H%M%S`"

   if [[ ! ${LOG_FILE} = "" ]] ; then
      rm -f ${LOG_FILE}
   fi

   Log_Print MUST "-------------------------------------------------------------------------------"
   Log_Print MUST "Script              : ${LOG_JOB}"
   Log_Print MUST "Version             : ${LOG_JOBVERSION}"
   Log_Print MUST "Hostname            : `hostname`"
   Log_Print MUST "Architecture        : `uname -s`"
   Log_Print MUST "Run ID              : ${LOG_JOBID}"

   if [[ ${LOG_MAILTO} != "" ]] ; then
      Log_Print MUST "E-mail Address      : ${LOG_MAILTO}"
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

   if [[ ${LOG_JOBCLASS} = "INTERACTIVE" ]]; then
      Log_Mail "Job started" ${LOG_FILE}
   fi

   #----- Activate Cylope links
   Log_CyclopeStart

}

#----------------------------------------------------------------------------
# Nom      : <Log_End>
# Creation : Octobre 2009 - J.P. Gauthier - CMC/CMOE
#
# But      : Afficher une message de fin standard.
#
# Parametres  :
#    <Status> : Code de retour de la job (0=ok, sinon erreur)
#    <Exit>   : Sortie du prorgamme (Default=True)
#
# Retour:
#
# Remarques :
#----------------------------------------------------------------------------

function Log_End {

   status=${1}
   exitnow=${2}

   LOG_SECEND=`date +%s`

   Log_Print MUST "\n-------------------------------------------------------------------------------"
   if [[ ${status} -eq 0 ]]; then
      Log_Print MUST "Status              : Job has terminated successfully (${LOG_WARNINGS} Warning(s))."
   else
      Log_Print MUST "Status              : Job has encountered some errors."
   fi
   Log_Print MUST "End time            : `date +\"%c %Z\"`"
   Log_Print MUST "Total running time  : $(Log_Time `expr ${LOG_SECEND} - ${LOG_SECSTART}`)"
   Log_Print MUST "-------------------------------------------------------------------------------\n"

   if [[ ${status} -eq 0 ]]; then
      if [[ ${LOG_JOBCLASS} = "INTERACTIVE" ]]; then
         Log_Mail "Job finished (NORMAL)" ${LOG_FILE}
      fi
   else
      Log_Mail "Job finished (ERROR)" ${LOG_FILE}
   fi

   #----- Activate Cylope links
   Log_CyclopeEnd $status
   Log_CyclopeSysInfo
   Log_CyclopeProcInfo

   if [[ ${exitnow} = "" ]]; then
      exit $status
   fi
}

#----------------------------------------------------------------------------
# Nom      : <Log_Print>
# Creation : Octobre 2009 - J.P. Gauthier - CMC/CMOE
#
# But      : Afficher une message standard.
#
# Parametres  :
#    <Type>   : Type de mesage (MUST,ERROR,WARNING,INFO,DEBUG,EXTRA)
#    <Message>: Message a afficher
#    <Time>   : Temps specifique
#
# Retour:
#
# Remarques :
#----------------------------------------------------------------------------

function Log_Print {

   level=${1}
   msg=${2}
   time=${3}

   #----- Levels are MUST,ERROR,WARNING,INFO,DEBUG,EXTRA
   eval lvl=\$\{LOG_LEVEL$level\}
   eval LOG_LEVELNO=\$\{LOG_LEVEL${LOG_LEVEL}\}

   #----- If it is a  warning, add to count for end result
   if [[ ${level} = "WARNING" ]]; then
      LOG_WARNINGS=$((LOG_WARNINGS+1))
   fi

   #----- Format the time if it is specified
   if [[ ${time} != "" ]]; then
      time=$(Log_Time $time)
   fi

   #----- Check for event time
   datetime=""
   if [[ ${LOG_TIME} -eq 1 ]]; then
      datetime="($(date)) "
   fi

   if [[ ${lvl} -le ${LOG_LEVELNO} ]]; then
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
            oclog ${LOG_JOB} x "${LOG_OC}\n\n${datetime}${levels}${msg} ${time}"
         fi
      fi
   fi
}

#----------------------------------------------------------------------------
# Nom      : <Log_Mail>
# Creation : Octobre 2009 - J.P. Gauthier - CMC/CMOE
#
# But      : Envoyer un message par courriel.
#
# Parametres  :
#    <Subject>: Sujet du message
#    <File>   : Fichier a envoyer
#    <Address>: Adresse destinataire
#
# Retour:
#
# Remarques :
#----------------------------------------------------------------------------

function Log_Mail {

   subject=${1}
   file=${2}
   address=${3}

   if [[ $address = "" ]]; then
      address=${LOG_MAILTO}
   fi

   if [[ ${address} = "" ]] ; then
      return 0
   fi

   if [[ -r ${file} ]] ;then
      mail -s "${LOG_MAILTITLE}: ${subject} (${LOG_JOBID})" ${address} < ${file}
   else
      printf "$file" | mail -s "${LOG_MAILTITLE}: ${subject} (${LOG_JOBID})" ${address}
   fi
}

#----------------------------------------------------------------------------
# Nom      : <Log_Time>
# Creation : Mai 2010 - J.P. Gauthier - CMC/CMOE
#
# But      : Formater un temps en secondes.
#
# Parametres  :
#   <Sec>     : Secondes systeme
#
# Retour      :
#   <Date>    : Date formatee
#
# Remarques   :
#----------------------------------------------------------------------------

function Log_Time {

   secs=$1

   hours=$((secs / 3600))
   secs=$((secs % 3600))
   mins=$((secs / 60))
   secs=$((secs % 60))

   printf "%02d:%02d:%02d\n" $hours $mins $secs
}

#----------------------------------------------------------------------------
# Nom      : <Log_CyclopePing>
# Creation : Juin 2012 - J.P. Gauthier - CMC/CMOE
#
# But      : Touch du fichier ping de la job pour cyclope.
#
# Parametres  :
#   <Delay>   : Delai de ping normal (Pour cyclope)
#   <Job>     : Job to ping
#
# Retour:
#
# Remarques :
#----------------------------------------------------------------------------

function Log_CyclopePing {

   delay=$1
   job=$2

   if [[ ${job} = "" ]]; then
      job=${LOG_JOB}
   fi
   echo $delay > ${LOG_CYCLOPEPATH}/ping/$job
}

#----------------------------------------------------------------------------
# Nom      : <Log_CyclopeStart>
# Creation : Mai 2010 - J.P. Gauthier - CMC/CMOE
#
# But      : Initialiser les informations du process pour cyclope.
#
# Parametres  :
#
# Retour:
#
# Remarques :
#----------------------------------------------------------------------------

function Log_CyclopeStart {

   if [[ ${LOG_CYCLOPE} = "TRUE" ]]; then
      path=${LOG_CYCLOPEPATH}/jobs/${LOG_JOBID}

      #----- Setup process info
      mkdir $path
      printf "Class     : ${LOG_JOBCLASS}\nJob       : ${LOG_JOB} ${LOG_JOBVERSION}\n" >$path/info.txt

      if [[ ! ${SelfJobResubmit} = "" ]]; then
         printf "Command   : $SelfJobResubmit\n" >>$path/info.txt
         printf "Kill      : $SelfJobKill\n" >>$path/info.txt
      else
         printf "Command   : ssh `hostname` $LOG_JOBCOMMAND $LOG_JOBARGS $*\n" >>$path/info.txt
         printf "Kill      : ssh `hostname` kill $$\n" >>$path/info.txt
      fi
      printf "Path      : ${LOG_JOBPATH}\nLog       : ${LOG_FILE}\nHostname  : `hostname`\nArch      : `uname -s`\nStart time: ${LOG_SECSTART}\n" >>$path/info.txt

      chmod 666 $path/info.txt
   fi
}

#----------------------------------------------------------------------------
# Nom      : <Log_CyclopeEnd>
# Creation : Mai 2010 - J.P. Gauthier - CMC/CMOE
#
# But      : Finaliser les informations du process pour cyclope.
#
# Parametres  :
#    <Status> : Code de retour de la job (0=ok, sinon erreur)
#
# Retour:
#
# Remarques :
#----------------------------------------------------------------------------

function Log_CyclopeEnd {

   status=$1

   if [[ ${LOG_CYCLOPE} = "TRUE" ]]; then
      path=${LOG_CYCLOPEPATH}/jobs/${LOG_JOBID}

      #----- Close process info
      printf "End time  : ${LOG_SECEND}\nRun time  : $((LOG_SECEND-LOG_SECSTART))\n" >> $path/info.txt

      if [[ ! $status -eq 0 ]]; then
         printf "Status    : Error ($Status)\n" >> $path/info.txt
      else
         if [[ ${LOG_WARNINGS} -gt 0 ]]; then
            printf "Status    : Warning (${LOG_WARNINGS})\n" >> $path/info.txt
         else
            printf "Status    : Success\n" >> $path/info.txt
         fi
      fi
   fi
}

function Log_CyclopeSysInfo {

   if [[ ${LOG_CYCLOPE} = "TRUE" ]]; then
      path=${LOG_CYCLOPEPATH}/jobs/${LOG_JOBID}

      printf "None\n" >> $path/sysinfo.txt
   fi
}

function Log_CyclopeProcInfo {

   if [[ ${LOG_CYCLOPE} = "TRUE" ]]; then
      path=${LOG_CYCLOPEPATH}/jobs/${LOG_JOBID}

      printf "None\n" >> $path/procinfo.txt
   fi
}
