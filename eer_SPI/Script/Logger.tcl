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

namespace eval Log {
   global env

   #----- Check for environment settings
   set Param(Level)     =${LOG_LEVEL:=INFO}
   set Param(Mode)      =${LOG_MODE:=""}
   set Param(Mail)      =${LOG_MAIL:=""}
   set Param(MailTitle) =${LOG_MAILTITLE:="Job Info"}
   set Param(Id)        =${LOG_JOBID:=""}
   set Param(File)      =${LOG_FILE:=""}
   set Param(Time)      =${LOG_TIME:=0}
   set Param(OC)        =""

   #----- Logger internal variables
   set Param(SecTime)  [clock seconds]
   set Param(SecStart) $Param(SecTime)
   set Param(SecEnd)   $Param(SecTime)
   set Param(Job)      "Unknown"
   set Param(Version)  "Unknown"

   set Level(MUST)   -1
   set Level(ERROR)   0
   set Level(WARNING) 1
   set Level(INFO)    2
   set Level(DEBUG)   3
   set Level(No)      $Level(INFO)

proc Log::Mail { Subject File Auto } {
   variable Param

   if { $Param(Mail}="" || $Param(Mode)!="$Auto" } {
      return
   }

   if { ![file readable ${file}] } {
      set file /dev/null
   }

   if { $env(EER_ARCH)=="IRIX64" } {
      eval exec mailx -s \"$Param(MailTitle): ${Subject} ($Param(JobID)\" $Param(Mail) < ${file}
   } else {
      eval exec mail -s \"$Param(MailTitle): ${Subject} ($Param(JobID)\" $Param(Mail) < ${file}
   }
}

proc Log::Start { Job Version { Input "" } } {
   global env
   variable Param

   set Param(SecStart) [clock seconds]

  #----- Simulation run time ID.
   if { $Param(JobID)=="" } {
      #----- Define run time ID if not defined.
      set Param(JobID) exec date +%Y%m%d%H%M%S
   fi

   if { [file exists $Param(File)] } {
      file delete -force $Param(File)
   }

   Log::Print MUST "-------------------------------------------------------------------------------"
   Log::Print MUST "Script              : $Job"
   Log::Print MUST "Version             : $Version"
   Log::Print MUST "Hostname            : [exec hostname]"
   Log::Print MUST "Architecture        : [exec uname -s]"
   Log::Print MUST "Run ID              : $Param(JobID)"

   if { $Param(Mail)!="" } {
      Log::Print MUST "E-mail Address      : $Param(Mail)"
   fi

   #----- Queue stuff
   if { [info exists env(LOADL_STEP_ID)] } {
      Log::Print MUST "Queue Method        : llv"
      Log::Print MUST "   Queue            : $env(LOADL_STEP_CLASS)"
      Log::Print MUST "   Job ID           : $env(LOADL_STEP_ID)"
      if { [file exists $Input] } {
         eval secs=\`perl -e \'\$mtime=\(stat\(\"${in}\"\)\)\[9\]\; print \$mtime\'\`
         Log::Print MUST "   Waiting time     : $(Log_Time `expr ${LogSecTime}-${secs}`)"
      fi
    } elseif { [info exists env(SGE_CELL)] } {
      Log::Print MUST "Queue Method        : sge"
      Log::Print MUST "   Queue               : $env(QUEUE)"
      Log::Print MUST "   Job ID              : $env(JOB_ID)"
      if { [file exists $Input] } {
        eval secs=\`perl -e \'\$mtime=\(stat\(\"${in}\"\)\)\[9\]\; print \$mtime\'\`
         Log_Print MUST "   Waiting time     : $(Log_Time `expr ${LogSecTime}-${secs}`)"
      fi
   fi

   Log::Print MUST "Start time          : [exec date +\"%c %Z\"]"
   Log::Print MUST "-------------------------------------------------------------------------------\n"

   Log::Mail "Job started" $Param(File)
}

proc Log::End { { Status 0 } } {
   variabl Param

   set Param(SecEnd) [clock seconds]

   Log::Print MUST "\n -------------------------------------------------------------------------------"
   if { ${Status}==0 } {
      Log::Print MUST "Status              : Job has terminated successfully."
   } else {
      Log::Print MUST "Status              : Job has encountered some errors."
   }
   Log::Print MUST "End time            : [exec date +\"%c %Z\"]"
   Log::Print MUST "Total running time  : [Log::Time [expr $Param(SecEnd)-$Param(SecStart)]]"
   Log::Print MUST "-------------------------------------------------------------------------------\n"

   if { $Status==0 } {
      Log::Mail "Job finished (NORMAL)" $Param(File)
   } else {
      Log::Mail "Job finished (ERROR)" $Param(File) AUTO
   }

   exit $Status
}

proc Log::Print { Level Msg { Time 0 } } {
   variable Param
   variable Level

   level=${1}
   msg=${2}
   time=${3}

   #----- Levels are MUST,ERROR,WARNING,INFO,DEBUG
   set lvl=$Level($Level)

   #----- Format the time if it is specified
   if { $Time } {
      set Time [Log_Time $Time]
   }

   #----- Check for event time
   set datetime=" "
   if { $Param(Time) } {
      set datetime=" ($(date)) "
   fi

   if { ${lvl}<=$Param($Param(Level)) } {
      if { ${Level}=="MUST"  } {
         levels=""
      } else {
         levels="(${Level})"
      }

      if { $Param(File)=="" } {
         exec echo "${levels}${datetime}${msg} ${time}"
      } else {
         exec echo "${levels}${datetime}${msg} ${time}" >> $Param(File)
      }
      if { $Level=="ERROR" } {
         echo "${levels}${datetime}${msg} ${time}" 1>&2
         if { $Param(OC) } {
            exec oclog x "$Param(OC)\n\n${levels}${datetime}${msg} ${time}"
         }
      }
   }
}

proc Log::Time { Secs } {
   return [clock format $Secs -format "%H:%M:%S" -gmt True]
}
