#----- Global general variable
DIRBIN=/home/binops/afse/eer/bin
DIRSCRIPT=/home/binops/afse/eer/eer_SPI-7.3.1/Script
ARCH=`uname -s`

#----- Logger specific variable
LogSecTime=`date +%s`
LogSecStart=${SECONDS}
LogSecEnd=${SECONDS}
LogJob="Unknown"
LogVersion="Unknown"
LogLevel=INFO
LogLevelNo=2
LogMail=""
LogMailTitle="Job Info"
LogRunID=""

LogERROR=0
LogWARNING=1
LogINFO=2
LogDEBUG=3

function Log::Mail {

   subject=${1}
   file=${2}

   if [[ ${LogMail} = "" ]] ; then
      return 0
   fi

   if [[ ${ARCH} = IRIX64 ]] ; then
      mailx -s "${LogMailTitle}: ${subject} (${LogRunID})" ${LogMail} < ${file}
   else
      mail -s "${LogMailTitle}: ${subject} (${LogRunID})" ${LogMail} < ${file}
   fi
}

function Log::Start {

   LogSecStart=${SECONDS}
   LogJob=${1}
   LogVersion=${2}
   out=${3}

   #----- Simulation run time ID.
   if [[ ${LogRunID} = "" ]] ; then
      #----- Define run time ID if not defined.
      LogRunID=`date +%Y%m%d%H%M%S`
   fi

   echo "-------------------------------------------------------------------------------"
   echo "Script              : ${LogJob}"
   echo "Version             : ${LogVersion}"
   echo "Hostname            : `hostname`"
   echo "Architecture        : `uname -s`"
   echo "Run ID              : ${LogRunID}"

   if [[ ${LogMail} != "" ]] ; then
      echo "E-mail Address      : ${LogMail}"
   fi

   #----- Queue stuff
   if [[ ! $LOADL_STEP_IDL -eq "" ]]; then
      echo "Queue Method        : llv"
      echo "   Queue               : $LOADL_STEP_CLASS"
      echo "   Job ID              : $LOADL_STEP_ID"
      secs=`perl -e '$mtime=(stat("./tmpdir"))[9]; print $mtime'`
      Log::TimeFormat `expr ${LogSecTime}-${secs}` "Waiting time        :"
    elif [[ ! $SGE_CELL -eq "" ]]; then
      echo "Queue Method        : sge"
      echo "   Queue               : $QUEUE"
      echo "   Job ID              : $JOB_ID"
      secs=`perl -e '$mtime=(stat("./tmpdir"))[9]; print $mtime'`
      Log::TimeFormat `expr ${LogSecTime}-${secs}` "Waiting time        :"
   fi

   date +"Start time          : %c %Z"
   echo "-------------------------------------------------------------------------------\n"

   Log::Mail "Job started" ${out}
}

function Log::End {

   status=${1}
   out=${2}
   err=${3}
   LogSecEnd=${SECONDS}

   echo "\n-------------------------------------------------------------------------------\n"
   if [[ ${status} -eq 0 ]] ; then
      echo "Status              : Job has terminated successfully."
   else
      echo "Status              : Job has encountered some errors."
   fi
   date +"End time            : %c %Z"
   Log::TimeFormat `expr ${LogSecEnd} - ${LogSecStart}` "Total runing time   :"
   echo "-------------------------------------------------------------------------------\n"

   if [[ -r ${out} ]]; then
      Log::Mail "Job finished" ${out}
   fi

   if [[ ${status} -gt 0 && -r ${err} ]]; then
      Log::Mail "Error log" ${err}
   fi

   exit $status
}

function Log::Print {

   level=$1
   msg=$2

   #----- Levels are ERROR,WARNING,INFO,DEBUG
   eval lvl=\$\{Log$level\}
   eval LogLevelNo=\$\{Log${LogLevel}\}

   if [[ ${lvl} -le ${LogLevelNo} ]]; then
      echo "($level) $msg"
      if [[ $level = "ERROR" ]] ; then
         echo "($level) $msg" 1>&2
      fi
   fi
}

function Log::TimeFormat {

   secs=$1
   msg=$2

   hours=$((secs / 3600))
   secs=$((secs % 3600))
   mins=$((secs / 60))
   secs=$((secs % 60))

   printf "%s %02d:%02d:%02d\n" "$msg" $hours $mins $secs
}
