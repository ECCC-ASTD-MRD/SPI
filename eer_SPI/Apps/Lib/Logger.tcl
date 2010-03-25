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
#   Log::Print { Type Message }
#   Log::Mail  { Subject File }
#
# Remarques :
#   Aucune
#
#===============================================================================

package provide Logger 1.0

catch { SPI::Splash "Loading Package Logger 1.0" }

namespace eval Log { } {
   global env
   variable Param

   set Param(Out)       stdout                ;#Output file/channel
   set Param(OutFile)   ""                    ;#Output filename
   set Param(Level)     DEBUG                 ;#Log level
   set Param(Time)      False                 ;#Print the time
   set Param(Proc)      True                  ;#Print the calling proc
   set Param(Path)      $env(HOME)/.spi/logs  ;#Path where to store the log files
   set Param(Mode)      SCRIPT                ;#Mode du logger (SCRIPT,ALL)
   set Param(Mail)      ""
   set Param(MailTitle) "Job Info"
   set Param(JobId)     ""
   set Param(OCLog)     ""
   set Param(SecTime)  [clock seconds]
   set Param(SecStart) $Param(SecTime)
   set Param(SecEnd)   $Param(SecTime)
   set Param(Job)      "Unknown"
   set Param(Version)  "Unknown"

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
   global env
   variable Param

   set Param(SecStart) [clock seconds]
   set Param(Job)      $Job
   set Param(Version)  $Version

  #----- Simulation run time ID.
   if { $Param(JobId)=="" } {
      #----- Define run time ID if not defined.
      set Param(JobId) [clock format [clock seconds] -format "%Y%m%d%H%M%S" -gmt True]
   }

   Log::Print MUST "-------------------------------------------------------------------------------"
   Log::Print MUST "Script              : $Job"
   Log::Print MUST "Version             : $Version"
   Log::Print MUST "Hostname            : [exec hostname]"
   Log::Print MUST "Architecture        : [exec uname -s]"
   Log::Print MUST "Run ID              : $Param(JobId)"

   if { $Param(Mail)!="" } {
      Log::Print MUST "E-mail Address      : [join $Param(Mail) "\n                      "]"
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

   if { $Param(Mode)=="ALL" } {
      Log::Mail "Job started" $Param(OutFile)
   }
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
      Log::Print MUST "Status              : Job has terminated successfully."
   } else {
      Log::Print MUST "Status              : Job has encountered some errors."
   }
   Log::Print MUST "End time            : [clock format $Param(SecEnd)]"
   Log::Print MUST "Total running time  : [clock format [expr $Param(SecEnd)-$Param(SecStart)] -format "%H:%M:%S" -gmt True]"
   Log::Print MUST "-------------------------------------------------------------------------------\n"

   if { $Status==0 } {
      if { $Param(Mode)=="ALL" } {
         Log::Mail "Job finished (NORMAL)" $Param(OutFile)
      }
   } else {
      Log::Mail "Job finished (ERROR)" $Param(OutFile)
   }

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

proc Log::Print { Type Message } {
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
         puts $Param(Out) "${time}(${Type}) ${proc}${Message}"
      }
      flush $Param(Out)
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
#
# Retour:
#
# Remarques :
#----------------------------------------------------------------------------

proc Log::Mail { Subject File } {
   global env
   variable Param

   if { $Param(Mail)==""  } {
      return
   }

   if { ![file exists $File] || ![file readable $File] } {
      if { $env(EER_ARCH)=="IRIX64" } {
         eval exec echo -e \$File | mailx -s \"$Param(MailTitle): ${Subject} ($Param(JobId))\" $Param(Mail)
      } else {
         eval exec echo -e \$File | mail -s \"$Param(MailTitle): ${Subject} ($Param(JobId))\" $Param(Mail)
      }
   } else {
      if { $env(EER_ARCH)=="IRIX64" } {
         eval exec mailx -s \"$Param(MailTitle): ${Subject} ($Param(JobId))\" $Param(Mail) < $File
      } else {
         eval exec mail -s \"$Param(MailTitle): ${Subject} ($Param(JobId))\" $Param(Mail) < $File
      }
   }
}
