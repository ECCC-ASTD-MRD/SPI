#===============================================================================
# Environnement Canada
# Centre Meteorologique Canadian
# 2100 Trans-Canadienne
# Dorval, Quebec
#
# Projet    : Librairie de gestion des logs
# Fichier   : Log.tcl
# Creation  : Octobre 2009 - J.P. Gauthier - CMC/CMOE
#
# Description: Definitions de diverses fonctions pour la gestion standardisee des logs.
#
# Fonctions:
#
#   Log::Print { Type Message { Proc True } }

# Remarques :
#   Aucune
#
#===============================================================================

package provide Log 1.0

catch { SPI::Splash "Loading Package Log 1.0" }

namespace eval Log { } {
   global env
   variable Param

   set Param(Out)       stdout                ;#Output file/channel
   set Param(Level)     DEBUG                 ;#Log level
   set Param(Time)      False                 ;#Print the time
   set Param(Proc)      True                  ;#Print the calling proc
   set Param(Path)      $env(HOME)/.spi/logs  ;#Path where to stro the log files
   set Param(Mail)      False
   set Param(MailTitle) "Job Info"
   set Param(Id)        ""
   set Param(OCLog)     ""
   set Param(SecTime)  [clock seconds]
   set Param(SecStart) $Param(SecTime)
   set Param(SecEnd)   $Param(SecTime)
   set Param(Job)      "Unknown"
   set Param(Version)  "Unknown"

   array set Param { MUST -1 ERROR 0 WARNING 1 INFO 2 DEBUG 3 };
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
      Log::Mail "Job finished (ERROR)" $Param(File)
   }

   exit $Status
}

proc Log::Print { Type Message { Pre "" } } {
   variable Param

   #----- Check for log file
   if { $Param(Out)=="" || [string first "/" $Param(Out)]!=-1 } {

      #----- Use temp path of specified path
      if { $Param(Out)=="" } {

         #----- Keep only last 3
         if { [llength [set logs [lrange [lsort -decreasing [glob -nocomplain $Param(Path)/*.log]] 3 end]]] } {
            eval file delete $logs
         }
         set Param(Out) [open $Data(Path)/[clock format [clock seconds] -format "%Y%m%d%H%M" -gmt True].log w]
      } else {
         if { [file exists $Param(Out)] } {
            file rename $Param(Out) $Param(Out).[clock format [clock seconds] -format "%Y%m%d%H%M" -gmt True]
         }
         set Param(Out) [open $Param(Out) w]
      }
   }

   #----- If the message is within the specified log level
   if { $Param($Type)<=$Param($Param(Level)) } {

      #----- Do we print the time
      if { $Param(Time) } {
         set time "([Log::Time [clock seconds]]) "
      } else {
         set time ""
      }

      #----- Do we print the calling proc
      if { $Param(Proc) && [set lvl [expr [info level]-1]]>0 } {
         set proc "[lindex [info level $lvl] 0]: "
      } else {
         set proc ""
      }

      #----- If it is an error, print it on stderr
      if { $Type=="ERROR" && $Param(Out)!="stdout" } {
         puts stderr "${time}(${Type}) ${proc}${Message}"
         if { $Param(OCLog)!=""  } {
            exec oclog x "$Param(OCLog)\n\n${time}(${Type}) ${proc}${Message}"
         }
      }

      puts $Param(Out) "${time}(${Type}) ${proc}${Message}"
      flush $Param(Out)
   }
}

proc Log::Mail { Subject File } {
   global env
   variable Param

   if { $Param(Mail}="" } {
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

proc Log::Time { Secs } {
   return [clock format $Secs -format "%H:%M:%S" -gmt True]
}

