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
#   Args::ParseDo    { Argv Argc No Multi Must Cmd }
#   Args::Parse      { Argv Argc No Multi Var { Values {} } { Glob "" } }
#   Args::ParseInput { File Var }
#
#   Log::Start    { Job Version { Input "" } }
#   Log::End      { { Status 0 } }
#   Log::Print    { Type Message { Var "" } }
#   Log::Mail     { Subject File { Address { } } }
#   Log::CheckSPI { Version }
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

package provide Logger 1.2
package require TclSystem

catch { SPI::Splash "Loading Package Logger 1.2" }

#----- Force default language
set GDefs(Lang) 1

#----- Define defeaul shelle color (as in the App C package
set APP_COLOR_RED     "\x1b\[31m"
set APP_COLOR_GREEN   "\x1b\[32m"
set APP_COLOR_YELLOW  "\x1b\[33m"
set APP_COLOR_BLUE    "\x1b\[34m"
set APP_COLOR_MAGENTA "\x1b\[35m"
set APP_COLOR_CYAN    "\x1b\[36m"
set APP_COLOR_RESET   "\x1b\[0m"

namespace eval Log { } {
   global env
   variable Param
   variable Color
   
   set Param(Out)         stdout                ;#Output file/channel
   set Param(OutFile)     ""                    ;#Output filename
   set Param(Level)       INFO                  ;#Log level
   set Param(Color)       False                 ;#Log color
   set Param(Time)        False                 ;#Print the time
   set Param(Proc)        True                  ;#Print the calling proc
   set Param(Path)        $env(HOME)/.spi/logs  ;#Path where to store the log files
   set Param(Keep)        24                    ;#Number of back log to keep
   set Param(OCLog)       ""                    ;#Message to send to OCLOG on error
   set Param(Warning)     0                     ;#Number of warnings
   set Param(Error)       0                     ;#Number of errors
   set Param(Process)     ""                    ;#Process number
   set Param(SPI)         ""                    ;#SPI version requirement

   set Param(SecTime)     [clock seconds]       ;#Current time
   set Param(SecLog)      $Param(SecTime)       ;#Log time
   set Param(SecStart)    $Param(SecTime)       ;#Start time
   set Param(SecEnd)      $Param(SecTime)       ;#End time
   set Param(Rotate)      86400                 ;#Log rotate time

   set Param(MailTo)      ""                    ;#Users to which mail will be sent
   set Param(MailTitle)   ""                    ;#Mail title

   set Param(Cyclope)     False                 ;#Use Cyclope
   set Param(CyclopePath) $env(HOME)/.Cyclope   ;#Path to Cyclope

   set Param(Job)         "Unknown"             ;#Job name
   set Param(JobVersion)  "Unknown"             ;#Job version
   set Param(JobId)       ""                    ;#Job unique identifier
   set Param(JobDate)     [clock format $Param(SecTime) -format "%Y%m%d_%H%MZ" -gmt True] ;#----- Current date.
   set Param(JobPath)     ""                    ;#Job temp dir
   set Param(JobClass)    SCRIPT                ;#Job class (SCRIPT,DAEMON,ORJI,HCRON,INTERACTIVE,REPORT)
   set Param(JobReport)   ALL                   ;#Job report (True,ALL,ERROR,WARNING)

   set Param(Levels)     { ERROR WARNING INFO DEBUG EXTRA }
   array set Param       { MUST -1 ERROR 0 WARNING 1 INFO 2 MESSAGE 2 QUESTION 2 DEBUG 3 EXTRA 4 -1 -1 0 0 1 1 2 2 3 3 4 4 }
   array set Color       { MUST "" ERROR "\x1b\[31m" WARNING "\x1b\[34m" INFO "" MESSAGE "\x1b\[33m" QUESTION "\x1b\[33m" DEBUG "\x1b\[36m" EXTRA "\x1b\[36m" RESET "\x1b\[0m" -1 "" 0 "\x1b\[31m" 1 "\x1b\[34m" 2 "\x1b\[33m" 3  "\x1b\[36m" 4  "\x1b\[36m"};
}

#---------------------------------------------------------------------------
# Nom      : <Args::Parse>
# Creation : Decembre 2000 - J.P. Gauthier - CMC/CMOE
#
# But      : Parcourir les listes d'arguments et lancer les commandes associees
#            aux type de ces arguments
#
# Parametres :
#   <Argv>   : Liste des arguments
#   <Argc>   : Nombre d'arguments
#   <No>     : Index dans la liste complete des arguments
#   <Multi>  : Est-ce que ce type d'argument peut etre gerer de facon multiple par Cmd
#   <Must>   : Est-ce que ce type d'argument doit absolument avoir des valeurs
#   <Cmd>    : Commande a effectuer sur le ou les arguments
#
# Retour:
#   <Idx>    : Index apres les arguments traites.
#
# Remarques :
#
#----------------------------------------------------------------------------

namespace eval Args { }

proc Args::ParseDo { Argv Argc No Multi Must Cmd } {

   #----- Garder l'index de depart
   set idx [incr No]
   set files ""

   #----- Parcourir les arguments du token specifie
   while { ([llength [lindex $Argv $No]]>1 || [string is double [lindex $Argv $No]] || [string index [lindex $Argv $No] 0]!="-")  && $No < $Argc } {

   if { $Cmd!="" } {
         if { $Multi } {
            lappend files [lindex $Argv $No]
         } else {
            eval $Cmd [lindex $Argv $No]
         }
      }
      incr No
   }

   if { $No==$idx && $Must } {
      Log::Print ERROR "No arguments value were specified for argument [lindex $Argv [incr idx -1]]"
      exit 1
   }

   if { $Cmd!="" && $Multi } {
      eval $Cmd \$files
   }

   if { $No != $idx } {
      incr No -1
   }
   return $No
}

#----------------------------------------------------------------------------
# Name     : <Args::Parse>
# Creation : Decembre 2000 - J.P. Gauthier - CMC/CMOE
#
# Goal     : Parcourir les listes d'arguments et lancer les commandes associees
#            aux type de ces arguments.
#
# Parameters :
#   <Argv>   : Liste des arguments
#   <Argc>   : Nombre d'arguments
#   <No>     : Index dans la liste complete des arguments
#   <Multi>  : Multiplicite des valeurs (0,FLAG=True,1,VALUE=1 valeur,2,LIST=Multiples valeurs, 3,FLAG_OR_VALUE=True ou 1 valeur)
#   <Var>    : Variable a a assigner les arguments
#   <Values> : Valid values accepted
#   <Glob>   : Glob pattern to validate values accepted
#
# Return:
#   <Idx>    : Index apres les arguments traites.
#
# Remarks :
#
#----------------------------------------------------------------------------

proc Args::Parse { Argv Argc No Multi Var { Values {} } { Glob "" } } {

   upvar #0 $Var var

   #----- If a token is used, get it's correspondance number
   if { ![string is integer $Multi] } {
      set Multi [lsearch -exact [list FLAG VALUE LIST FLAG_OR_VALUE] $Multi]
   }

   if { !$Multi } {
      set var True
   } else {

      #----- Garder l'index de depart
      set idx [incr No]
      set var {}

      if { $Multi==3 } {
         set var True
      } else {
         set var {}
      }

      #----- Parcourir les arguments du token specifie
      while { ([string is double [lindex $Argv $No]] || [string index [lindex $Argv $No] 0]!="-") && $No<$Argc } {

         #----- Check for argument validity
         set vs [lindex $Argv $No]
         if { $Multi==2 } {
            set vs [split $vs +]
         }

         if { [llength $Values] } {
            foreach v $vs {
               if { $Glob=="" || ![string match $Glob $v] } {
                  if { [lsearch -exact $Values $v]==-1 } {
                     Log::Print ERROR "Invalid value ($v) for parameter [lindex $Argv [expr $No-1]], must be one of { $Values }"
                     Log::End 1
                  }
               }
            }
         }

         if { $Multi==1 || $Multi==3 } {
            set var $vs
         } else {
            eval lappend var $vs
         }
         incr No
      }

      #----- Verifier le nombre de valeur
      if { $Multi && ![llength $var] }  {
         Log::Print ERROR "No value specified for parameter [lindex $Argv [expr $No-1]]"
         Log::End 1
      }

      if { [string index [lindex $Argv $No] 0]=="-" } {
         incr No -1
      }
   }

   return $No
}

#----------------------------------------------------------------------------
# Name     : <Args::ParseInput>
# Creation : Mai 2014 - J.P. Gauthier - CMC/CMOE
#
# Goal     : Parcourir un fichier de directives.
#
# Parameters :
#   <File>   : Fichier de directives
#   <Var>    : Variable a a assigner les valeurs
#
# Return:
#   <Nb>     : Nombre d'arguments traites.
#
# Remarks :
#    - Le fichier doit avoir le format suivant:
#
#         SRC_NAME   = Rigaud Test           # Source name.
#         SRC_TYPE   = ACCIDENT              # Source type
#         SRC_TIME   = 201205181620          # Emission date-time [UTC]: Year, Month, Day, Hour, Minutes.
#         ...
#----------------------------------------------------------------------------

proc Args::ParseInput { File Var } {

   upvar #0 $Var input
   
   set tok  ""
   set ntok 0
   array unset input
   
   #----- Parse input file
   foreach dir [split [exec cat $File] '\n'] {
   
      #----- Get rid of comments
      if { [set idx [string first # $dir]]!=-1 } {
         set dir [string range $dir 0 $idx-1]
      }
      
      if { [string length [set dir [string trim $dir]]] } {
      
         #----- Check for line continuation
         if { [set idx [string first = $dir]]!=-1 } {
            set param       [split $dir =]
            set tok         [string trim [lindex $param 0]]
            set input($tok) [string trim [lindex $param end]]
            incr ntok
         } else {
            lappend input($tok) [string trim $dir]
         }
      }
   }
   return $ntok
}

proc Log::CheckSPI { Version } {
   global env
   
   if { $Version!="" && ![package vsatisfies $env(SPI_VERSION) $Version] } {
      Log::Print ERROR "The version of SPI provided ($env(SPI_VERSION)) does not meet the minimum requirement ($Version)"
      Log::End 1 True
   }
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

   #----- If report is request, we can't output to stdout
   if { $Param(JobClass)=="REPORT" && $Param(Out)=="stdout" } {
      set Param(Out) ""
   }

   if { $Param(MailTitle)=="" } {
      set Param(MailTitle) "$Param(Job) Report"
   }

   #----- Job run time ID.
   if { $Param(JobId)=="" } {
      set Param(JobId) [string toupper $Param(Job)]
   }
   append Param(JobId) "-[clock format [clock seconds] -format "%Y%m%d_%H%M%S" -gmt True]"

   Log::Print MUST "-------------------------------------------------------------------------------"
   Log::Print MUST "App/Script          : $Job"
   Log::Print MUST "Version             : $Version"
   Log::Print MUST "Hostname            : [system info -name]"
   Log::Print MUST "Architecture        : $env(ORDENV_PLAT)"
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
         Log::Print MUST "   Waiting time     : [clock format [expr $Param(SecTime)-${secs}] -format "%H:%M:%S" -gmt True]"
      }
   }
   Log::Print MUST "Start time          : [clock format $Param(SecStart)]"
   Log::Print MUST "-------------------------------------------------------------------------------\n"

   Log::CheckSPI $Param(SPI)

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
      if { $Param(Warning)>0 } {
          Log::Print MUST "Status              : Job has terminated successfully ($Param(Warning) Warning(s))."
       } else {
          Log::Print MUST "Status              : Job has terminated successfully."
       }       
   } else {
      Log::Print MUST "Status              : Job has encountered some errors ($Param(Error) Error(s))."
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
      } elseif { $Param(JobClass)=="REPORT" } {
         if { $Param(Error)>0 } {
            Log::Mail "Job finished (ERROR ($Param(Error))" $Param(OutFile)
         } elseif { $Param(Warning)>0 } {
            Log::Mail "Job finished (WARNING ($Param(Warning))" $Param(OutFile)
         } elseif { $Param(JobReport)==True || $Param(JobReport)=="ALL" } {
            Log::Mail "Job finished (NORMAL)" $Param(OutFile)
         }    
      }
   } else {
      Log::Mail "Job finished (ERROR)" $Param(OutFile)
   }

   #----- Activate Cylope links
   Log::CyclopeEnd $Status
   Log::CyclopeSysInfo
   Log::CyclopeProcInfo

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
   variable Color
   
   #----- Check for log file
   if { $Param(Out)=="" || [string first "/" $Param(Out)]!=-1 } {

      #----- Use temp path of specified path
      if { $Param(Out)=="" } {

         file mkdir $Param(Path)

         #----- Keep only last $Param(Keep) hour logs
         set err [catch { exec find $Param(Path) -name *.log -ctime +$Param(Keep) -exec rm \{\} \; } msg]

         set Param(OutFile) $Param(Path)/[clock format [clock seconds] -format "%Y%m%d%H%M" -gmt True]-[pid].log
      } else {
         if { $Param(Rotate) && [file exists $Param(Out)] } {
            file rename -force $Param(Out) $Param(Out).[clock format [clock seconds] -format "%Y%m%d%H%M" -gmt True]
         }
         set Param(OutFile) $Param(Out)
      }
      set Param(Out) [open $Param(OutFile) a]
      fconfigure $Param(Out) -buffering line
   }

   #----- Check if we need to rotate the log file
   if { $Param(Rotate) && $Param(JobClass)=="DAEMON" && [expr [clock seconds]-$Param(SecLog)]>$Param(Rotate) } {
      if { [file exists $Param(OutFile)] } {
         close $Param(Out)
         file rename -force $Param(OutFile) $Param(OutFile).[clock format $Param(SecLog) -format "%Y%m%d" -gmt True]
         set Param(Out) [open $Param(OutFile) w]
         fconfigure $Param(Out) -buffering line
      }
      incr Param(SecLog) $Param(Rotate)

      #----- Print stats up til now
      Log::CyclopeSysInfo
      Log::CyclopeProcInfo
   }

   if { $Param($Type)=="-" } {
      puts $Param(Out) "----------------------------------------------------------------------------------------------------"
      return
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

      #----- Check for process id
      if { $Param(Process)!="" } {
         set id "($Param(Process)) "
      } else {
         set id ""
      }

      #----- Do we print the time
      if { $Param(Time) } {
         set time "([clock format [clock seconds]]) "
      } else {
         set time ""
      }

      #----- Do we print the calling proc
      if { $Param(Proc) && $Param($Type)<3 && [set lvl [expr [info level]-1]]>0 } {
         set proc "[lindex [info level $lvl] 0]: "
      } else {
         set proc ""
      }

      #----- If it is a  warning, add to count for end result
      if { $Type=="WARNING" } {
         incr Param(Warning)
      }

      #----- If it is a  warning, add to count for end result
      if { $Type=="ERROR" } {
         incr Param(Error)
      }
      
      #----- If it is an error, print it on stderr
      if { $Type=="ERROR" && $Param(Out)!="stdout" } {
         puts stderr "${time}(${Type}) ${proc}${Message}"
         if { $Param(OCLog)!=""  } {
            set err [catch { exec oclog $Param(Job) x "$Param(OCLog)\n\n${time}(${Type}) ${proc}${Message}" } msg]
            if { $err } {
               puts stderr "${time}(ERROR) Problems while calling oclog:\n\n\t$msg"
               puts $Param(Out) "$Color($Type)${time}(ERROR) Problems while calling oclog:\n\n\t$msg$Color(RESET)"
            }
         }
      }

      if { $Type=="MUST" } {
         puts $Param(Out) "${Message}"
      } else {
         puts $Param(Out) "$Color($Type)${time}${id}(${Type}) ${proc}${Message}${vars}$Color(RESET)"
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
   set err 0

   if { [llength $Address] } {
      set address $Address
   }

   if { [llength $address] } {
      if { ![file exists $File] || ![file readable $File] } {
         set err [catch { eval exec echo -e \$File | mail -s \"$Param(MailTitle): ${Subject} ($Param(JobId))\" $address } msg]
      } else {
         set err [catch { eval exec mail -s \"$Param(MailTitle): ${Subject} ($Param(JobId))\" $address < $File } msg]
      }
      if { $err } {
         Log::Print ERROR "Problems while mailing info to $address:\n\n\t"
      }
   }
}

#----------------------------------------------------------------------------
# Nom      : <Log::CyclopePing>
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

proc Log::CyclopePing { Delay { Job "" } } {
   variable Param

   if { $Job=="" } {
      set Job $Param(JobId)
   }
   exec echo $Delay > $Param(CyclopePath)/ping/$Job
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
      set path $Param(CyclopePath)/jobs/$Param(JobId)

      #----- Setup process info
      file mkdir $path
      set f [open $path/info.txt w 00644]
      puts $f  "Class     : $Param(JobClass)\nJob       : $Param(Job) $Param(JobVersion)"

      if { [info exists env(SelfJobResubmit)] } {
         puts $f "Command   : $env(SelfJobResubmit)"
         puts $f "Kill      : $env(SelfJobKill)"
      } else {
         puts $f "Command   : ssh [info hostname] [info script] [split $argv]"
         puts $f "Kill      : ssh [info hostname] kill [pid]"
      }
      puts $f  "Path      : $Param(JobPath)\nLog       : $Param(OutFile)\nHostname  : [system info -name]\nArch      : [system info -os]\nStart time: $Param(SecStart)"

      close $f
      file attributes $path/info.txt  -permissions 00644
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
      set f [open $Param(CyclopePath)/jobs/$Param(JobId)/info.txt a]

      #----- Close process info
      puts $f "End time  : $Param(SecEnd)\nRun time  : [expr $Param(SecEnd)-$Param(SecStart)]"

      if { $Status } {
         puts $f "Status    : Error ($Status) ($Param(Error))"
      } elseif { $Param(Warning) } {
         puts $f "Status    : Warning ($Param(Warning))"
      } else {
         puts $f "Status    : Success"
      }

      close $f
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
      set path $Param(CyclopePath)/jobs/$Param(JobId)

      set calls "-uptime -loads -totalmem -freemem -sharedmem -buffermem -totalswap -freeswap -process -totalhigh -freehigh -memunit"
      eval set stats \[system info $calls\]

      #----- Print some stats
      set f [open $path/sysinfo.txt w]
      puts $f [format "%-10s: %s" Hostname [system info -name]]
      puts $f [format "%-10s: %s" Arch [system info -os]]
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
      "Policy"
      "Unknown" }

   if { $Param(Cyclope) } {
      set path $Param(CyclopePath)/jobs/$Param(JobId)

      set calls "-utime -stime -cutime -cstime -rss -shared -data -stack -minpagefault -majpagefault -swap -inblock -outblock -signal -vcswitch -ivcswitch"
      eval set stats \[system usage $calls\]

      #----- Print some stats
      set f [open $path/procinfo.txt w]
      puts $f "RUSAGE:"
      foreach info $calls stat $stats {
         puts $f [format "   %-10s: %s" [string totitle [string trimleft $info -]] $stat]
      }

      if { $PID=="" } {
         set PID [pid]
      }

      #----- Got a signal 2 kill on the cat a few times so catch it for now
      puts $f "PROCINFO:"
      catch {
         foreach info $Stat(Infos) stat [exec cat /proc/$PID/stat] {
            puts $f [format "   %-40s: %s" $info $stat]
         }
      }
      close $f
   }
}
