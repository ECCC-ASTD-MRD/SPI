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
#   Log::Start { Job Version { Input "" } }
#   Log::End   { { Status 0 } }
#   Log::Print { Type Message { Var "" } }
#   Log::Mail  { Subject File { Address { } } }
#
#   Log::CyclopeStart    { }
#   Log::CyclopeEnd      { }
#   Log::CyclopeSysInfo  { }
#   Log::CyclopeProcInfo { { PID "" } }
#
# Remarques :
#   Aucune
#
#===============================================================================

package provide Logger 1.1
package require TclSystem

catch { SPI::Splash "Loading Package Logger 1.1" }

namespace eval Log { } {
   global env
   variable Param

   set Param(Out)         stdout                ;#Output file/channel
   set Param(OutFile)     ""                    ;#Output filename
   set Param(Level)       DEBUG                 ;#Log level
   set Param(Time)        False                 ;#Print the time
   set Param(Proc)        True                  ;#Print the calling proc
   set Param(Path)        $env(HOME)/.spi/logs  ;#Path where to store the log files
   set Param(OCLog)       ""                    ;#Message to send to OCLOG on error
   set Param(Warning)     0                     ;#Number of warning

   set Param(SecTime)     [clock seconds]       ;#Current time
   set Param(SecLog)      $Param(SecTime)       ;#Log time
   set Param(SecStart)    $Param(SecTime)       ;#Start time
   set Param(SecEnd)      $Param(SecTime)       ;#End time

   set Param(MailTo)      ""                    ;#Users to which mail will be sent
   set Param(MailTitle)   "Job Info"            ;#Mail title

   set Param(Cyclope)     False                                      ;#Use Cyclope
   set Param(CyclopePath) /home/binops/afse/eer/projets/cyclope/jobs ;#Path to Cyclope

   set Param(Job)         "Unknown"             ;#Job name
   set Param(JobVersion)  "Unknown"             ;#Job version
   set Param(JobId)       "JOB"                 ;#Job unique identifier
   set Param(JobPath)     ""                    ;#Job temp dir
   set Param(JobClass)    SCRIPT                ;#Job class (SCRIPT,DAEMON,ORJI,HCRON,INTERACTIVE)

   array set Param { MUST -1 ERROR 0 WARNING 1 INFO 2 DEBUG 3 };
}

#----------------------------------------------------------------------------
# Nom      : <Log::Start>
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

proc Log::Start { Job Version { Input "" } } {
   global env argv argc
   variable Param

   set Param(SecStart)   [clock seconds]
   set Param(SecLog)     $Param(SecStart)
   set Param(Job)        $Job
   set Param(JobVersion) $Version

   #----- Job run time ID.
   append Param(JobId) "-[clock format [clock seconds] -format "%Y%m%d_%H%M%S" -gmt True]"

   Log::Print MUST "-------------------------------------------------------------------------------"
   Log::Print MUST "Script              : $Job"
   Log::Print MUST "Version             : $Version"
   Log::Print MUST "Hostname            : [exec hostname]"
   Log::Print MUST "Architecture        : [exec uname -s]"
   Log::Print MUST "Run ID              : $Param(JobId)"

   if { $Param(MailTo)!="" } {
      Log::Print MUST "E-mail Address      : [join $Param(MailTo) "\n                      "]"
   }

   #----- Queue stuff
   if { [info exists env(LOADL_STEP_ID)] } {
      Log::Print MUST "Queue Method        : llv"
      catch { Log::Print MUST "   Queue            : $env(LOADL_STEP_CLASS)" }
      catch { Log::Print MUST "   Job ID           : $env(LOADL_STEP_ID)" }
      if { [file exists $Input] } {
         set secs [file mtime $Input]
         Log::Print MUST "   Waiting time     : [clock format [expr $Param(SecTime)-${secs}] -format "%H:%M:%S" -gmt True]"
      }
    } elseif { [info exists env(SGE_CELL)] } {
      Log::Print MUST "Queue Method        : sge"
      catch { Log::Print MUST "   Queue               : $env(QUEUE)" }
      catch { Log::Print MUST "   Job ID              : $env(JOB_ID)" }
      if { [file exists $Input] } {
         set secs [file mtime $Input]
         Log_Print MUST "   Waiting time     : [clock format [expr $Param(SecTime)-${secs}] -format "%H:%M:%S" -gmt True]"
      }
   }
   Log::Print MUST "Start time          : [clock format $Param(SecStart)]"
   Log::Print MUST "-------------------------------------------------------------------------------\n"

   if { $Param(JobClass)=="INTERACTIVE" } {
      Log::Mail "Job started" $Param(OutFile)
   }

   #----- Activate Cylope links
   Log::CyclopeStart
}

#----------------------------------------------------------------------------
# Nom      : <Log::End>
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

proc Log::End { { Status 0 } { Exit True } } {
   variable Param

   set Param(SecEnd) [clock seconds]

   Log::Print MUST "\n-------------------------------------------------------------------------------"
   if { ${Status}==0 } {
      Log::Print MUST "Status              : Job has terminated successfully ($Param(Warning) Warning(s))."
   } else {
      Log::Print MUST "Status              : Job has encountered some errors."
   }
   Log::Print MUST "End time            : [clock format $Param(SecEnd)]"
   Log::Print MUST "Total running time  : [clock format [expr $Param(SecEnd)-$Param(SecStart)] -format "%H:%M:%S" -gmt True]"
   Log::Print MUST "-------------------------------------------------------------------------------\n"

   if { $Param(Out)!="stdout" && $Param(Out)!="stderr" } {
      close $Param(Out)
   }

   if { $Status==0 } {
      if { $Param(JobClass)=="INTERACTIVE" } {
         Log::Mail "Job finished (NORMAL)" $Param(OutFile)
      }
   } else {
      Log::Mail "Job finished (ERROR)" $Param(OutFile)
   }

   #----- Activate Cylope links
   Log::CyclopeSysInfo
   Log::CyclopeProcInfo
   Log::CyclopeEnd $Status

   if { $Exit } {
      exit $Status
   }
}

#----------------------------------------------------------------------------
# Nom      : <Log::Print>
# Creation : Octobre 2009 - J.P. Gauthier - CMC/CMOE
#
# But      : Afficher une message standard.
#
# Parametres  :
#    <Type>   : Type de mesage (MUST,ERROR,WARNING,INFO,DEBUG)
#    <Message>: Message a afficher
#
# Retour:
#
# Remarques :
#----------------------------------------------------------------------------

proc Log::Print { Type Message { Var "" } } {
   variable Param

   #----- Check for log file
   if { $Param(Out)=="" || [string first "/" $Param(Out)]!=-1 } {

      #----- Use temp path of specified path
      if { $Param(Out)=="" } {

         #----- Keep only last 3
         if { [llength [set logs [lrange [lsort -decreasing [glob -nocomplain $Param(Path)/*.log]] 3 end]]] } {
            eval file delete $logs
         }
         set Param(OutFile) $Param(Path)/[clock format [clock seconds] -format "%Y%m%d%H%M" -gmt True].log
      } else {
         if { [file exists $Param(Out)] } {
            file rename -force $Param(Out) $Param(Out).[clock format [clock seconds] -format "%Y%m%d%H%M" -gmt True]
         }
         set Param(OutFile) $Param(Out)
      }
      set Param(Out) [open $Param(OutFile) w]
      fconfigure $Param(Out) -buffering line
   }

   #----- Check if we need to split the log file
   if { $Param(JobClass)=="DAEMON" && [expr [clock seconds]-$Param(SecLog)]>86400 } {
      if { [file exists $Param(OutFile)] } {
         close $Param(Out)
         file rename -force $Param(OutFile) $Param(OutFile).[clock format $Param(SecLog) -format "%Y%m%d" -gmt True]
         set Param(Out) [open $Param(OutFile) w]
         fconfigure $Param(Out) -buffering line
      }
      incr Param(SecLog) 86400

      #----- Print stats up til now
      Log::CyclopeSysInfo
      Log::CyclopeProcInfo
   }

   #----- Print the variable if given
   set vars  ""
   if { $Var!="" } {
      if { [array size $Var] } {
         set vars  \n
         set len   0
         set names [lsort [array names $Var]]

         #----- Get maximum length
         foreach name $names {
            if { [set l [string length $name]]>$len } {
               set len $l
            }
         }
         incr len [string length $Var]
         incr len 2

         #----- Print the array
         foreach name $names {
            eval append vars \[format \"\t%-${len}s : %s\n\" ${Var}($name) \${${Var}($name)}\]
         }
      }
   }

   #----- If the message is within the specified log level
   if { $Param($Type)<=$Param($Param(Level)) } {

      #----- Do we print the time
      if { $Param(Time) } {
         set time "([clock format [clock seconds]]) "
      } else {
         set time ""
      }

      #----- Do we print the calling proc
      if { $Param(Proc) && [set lvl [expr [info level]-1]]>0 } {
         set proc "[lindex [info level $lvl] 0]: "
      } else {
         set proc ""
      }

      #----- If it is a  warning, add to count for end result
      if { $Type=="WARNING" } {
         incr Param(Warning)
      }

      #----- If it is an error, print it on stderr
      if { $Type=="ERROR" && $Param(Out)!="stdout" } {
         puts stderr "${time}(${Type}) ${proc}${Message}"
         if { $Param(OCLog)!=""  } {
            exec oclog $Param(Job) x "$Param(OCLog)\n\n${time}(${Type}) ${proc}${Message}"
         }
      }

      if { $Type=="MUST" } {
         puts $Param(Out) "${Message}"
      } else {
         puts $Param(Out) "${time}(${Type}) ${proc}${Message}${vars}"
      }
   }
}

#----------------------------------------------------------------------------
# Nom      : <Log::Mail>
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

proc Log::Mail { Subject File { Address { } } } {
   global env
   variable Param

   set address $Param(MailTo)

   if { [llength $Address] } {
      set address $Address
   }

   if { [llength $address]  } {
      if { ![file exists $File] || ![file readable $File] } {
         if { $env(EER_ARCH)=="IRIX64" } {
            eval exec echo -e \$File | mailx -s \"$Param(MailTitle): ${Subject} ($Param(JobId))\" $address
         } else {
            eval exec echo -e \$File | mail -s \"$Param(MailTitle): ${Subject} ($Param(JobId))\" $address
         }
      } else {
         if { $env(EER_ARCH)=="IRIX64" } {
            eval exec mailx -s \"$Param(MailTitle): ${Subject} ($Param(JobId))\" $address < $File
         } else {
            eval exec mail -s \"$Param(MailTitle): ${Subject} ($Param(JobId))\" $address < $File
         }
      }
   }
}

#----------------------------------------------------------------------------
# Nom      : <Log::CyclopeStart>
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

proc Log::CyclopeStart { } {
   global env argv argc
   variable Param

   if { $Param(Cyclope) } {
      set path $Param(CyclopePath)/$Param(JobId)

      #----- Setup process info
      file mkdir $path
      exec echo "Class     : $Param(JobClass)
Job       : $Param(Job) $Param(JobVersion)
Command   : [info script] [split $argv]
Path      : $Param(JobPath)
Log       : $Param(OutFile)
Hostname  : [info hostname]
Arch      : [exec uname -s]
Start time: $Param(SecStart)" > $path/info.txt
   }
}

#----------------------------------------------------------------------------
# Nom      : <Log::CyclopeEnd>
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

proc Log::CyclopeEnd { Status } {
   variable Param

   if { $Param(Cyclope) } {
      set path $Param(CyclopePath)/$Param(JobId)

      #----- Close process info
      exec echo "End time  : $Param(SecEnd)\nRun time  : [expr $Param(SecEnd)-$Param(SecStart)]" >> $path/info.txt

      if { $Status } {
         exec echo "Status    : Error ($Status)" >> $path/info.txt
      } elseif { $Param(Warning) } {
         exec echo "Status    : Warning ($Param(Warning))" >> $path/info.txt
      } else {
         exec echo "Status    : Success" >> $path/info.txt
      }
   }
}

#----------------------------------------------------------------------------
# Nom      : <Log::CyclopeSysInfo>
# Creation : Mai 2010 - J.P. Gauthier - CMC/CMOE
#
# But      : Extraire les statistiques du process pour cyclope.
#
# Parametres  :
#
# Retour:
#
# Remarques :
#----------------------------------------------------------------------------

proc Log::CyclopeSysInfo { } {
   variable Param

   #----- Activate Cylope links
   if { $Param(Cyclope) } {
      set path $Param(CyclopePath)/$Param(JobId)

      set calls "-uptime -loads -totalmem -freemem -sharedmem -buffermem -totalswap -freeswap -process -totalhigh -freehigh -memunit"
      eval set stats \[system info $calls\]

      #----- Print some stats
      set f [open $path/sysinfo.txt w]
      puts $f [format "%-10s: %s" Hostname [info hostname]]
      puts $f [format "%-10s: %s" Arch [exec uname -s]]
      foreach info $calls stat $stats {
         puts $f [format "%-10s: %s" [string totitle [string trimleft $info -]] $stat]
      }
      close $f
   }
}

#----------------------------------------------------------------------------
# Nom      : <Log::CyclopeProcInfo>
# Creation : Mai 2010 - J.P. Gauthier - CMC/CMOE
#
# But      : Extraire les statistiques du process pour cyclope.
#
# Parametres  :
#
# Retour:
#
# Remarques :
#----------------------------------------------------------------------------

proc Log::CyclopeProcInfo { { PID "" } } {
   variable Param
   variable Stat

   set Stat(Names) { PID Name State PPID PGRP SID STTY PTTY Flags MinFLT CMinFLT MajFLT CMajFLT UTime STime CUTime CSTime Priority Nice 0 ITRealValue
      StartTime VSize RSS RLim StartCode EndCode Stack StackESP StackEIP SignalPending SignalBlocked SignalIgnored SignalCatched WChan NSwap CNSwap
      SignalExit CPU RTPriority Policy }

   set Stat(Infos) {
      "Process ID"
      "Filename of the executable"
      "Proces state "
      "Parent PID"
      "Process group ID"
      "Session"
      "TTY"
      "TTY's owner process group ID"
      "Process kernel flags word"
      "Number of minor faults"
      "Chilren's number of minor faults"
      "Number of major faults"
      "Children's number of major faults"
      "Number of user mode jiffies"
      "Number of system mode jiffies"
      "Chilren's number of user mode jiffies"
      "Chilren's number of system mode jiffies"
      "Nice value, plus fifteen"
      "The nice value ranges"
      "Placeholder for a removed field"
      "Time in jiffies to SIGALRM"
      "After boot start time in jiffies"
      "Virtual memory size in bytes"
      "Resident Set Size"
      "RSS limit in bytes"
      "Start code address"
      "End code address"
      "Stack start address"
      "Stack pointer"
      "Instruction pointer"
      "Pending signals bitmap"
      "Blocked signals bitmap"
      "Ignored signals bitmap"
      "Caught signals bitmap"
      "Waiting channel"
      "Pages swapped"
      "Children's cumulative pages swapped"
      "Signal to be sent to parent when we die"
      "CPU number last executed on"
      "Real-time scheduling priority"
      "Policy" }

   if { $Param(Cyclope) } {
      set path $Param(CyclopePath)/$Param(JobId)

      if { $PID=="" } {
         set PID [pid]
      }
      set stats [exec cat /proc/$PID/stat]

      set f [open $path/procinfo.txt w]
      foreach info $Stat(Infos) stat $stats {
         puts $f [format "%-40s: %s" $info $stat]
      }
      close $f
   }
}
