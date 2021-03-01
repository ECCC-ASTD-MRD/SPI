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
#   Log::Stack    { }
#
# Remarques :
#   Aucune
#
#===============================================================================

package provide Logger 1.3
package require TclSystem

catch { SPI::Splash "Loading Package Logger 1.3" }

#----- Force default language
set GDefs(Lang) 1

#----- Define default shellecolor (as in the App C package)
set APP_COLOR_BLINK   "\x1b\[5m"
set APP_COLOR_BLACK   "\x1b\[30m"
set APP_COLOR_RED     "\x1b\[31m"
set APP_COLOR_GREEN   "\x1b\[32m"
set APP_COLOR_YELLOW  "\x1b\[33m"
set APP_COLOR_BLUE    "\x1b\[34m"
set APP_COLOR_MAGENTA "\x1b\[35m"
set APP_COLOR_CYAN    "\x1b\[36m"
set APP_COLOR_GRAY    "\x1b\[37m"
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
   set Param(Proc)        False                 ;#Print the calling proc
   set Param(Path)        $env(HOME)/.spi/logs  ;#Path where to store the log files
   set Param(Keep)        24                    ;#Number of back log to keep
   set Param(XFlow)       ""                    ;#Message to send to OPS xflow app
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

   set Param(Job)         "Unknown"             ;#Job name
   set Param(JobVersion)  "Unknown"             ;#Job version
   set Param(JobId)       ""                    ;#Job unique identifier
   set Param(JobDate)     [clock format $Param(SecTime) -format "%Y%m%d_%H%M%S" -timezone :UTC] ;#----- Current date.
   set Param(JobPath)     ""                    ;#Job temp dir
   set Param(JobClass)    SCRIPT                ;#Job class (SCRIPT,DAEMON,ORJI,HCRON,INTERACTIVE,REPORT)
   set Param(JobReport)   ALL                   ;#Job report (True,ALL,ERROR,WARNING)

   set Param(Levels)     { ERROR WARNING INFO DEBUG EXTRA }
   array set Param       { MUST -1 ERROR 0 WARNING 1 INFO 2 MESSAGE 2 QUESTION 2 DEBUG 3 EXTRA 4 -1 -1 0 0 1 1 2 2 3 3 4 4 }
   array set Color       { MUST "" ERROR "\x1b\[31m" WARNING "\x1b\[34m" INFO "" MESSAGE "\x1b\[33m" QUESTION "\x1b\[33m" DEBUG "\x1b\[36m" EXTRA "\x1b\[36m" RESET "\x1b\[0m" PROGRESS "\x1b\[35m" -1 "" 0 "\x1b\[31m" 1 "\x1b\[34m" 2 "\x1b\[33m" 3  "\x1b\[36m" 4  "\x1b\[36m"};
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
   while { ([llength [lindex $Argv $No]]>1 || [string is double [lindex $Argv $No]] || ([string index [lindex $Argv $No] 0] ni { "-" "+" })) && $No < $Argc } {

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
      set vs  {}

      if { $Multi==3 } {
         set var True
      } else {
         set vs  -
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
      if { $Multi && $vs=="-" }  {
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

   if { $Version!="" && $env(SPI_VERSION)!="x.x.x" && ![package vsatisfies $env(SPI_VERSION) ${Version}-] } {
      Log::Print ERROR "The version of SPI provided ($env(SPI_VERSION)) does not meet the minimum requirement ($Version)"
      Log::End 1 True
   }
}

#----------------------------------------------------------------------------
# Nom      : <Log::Stack>
# Creation : Mars 2015- J.P. Gauthier - CMC/CMOE
#
# But      : Afficher le call stack, utile pour debugger
#
# Parametres  :
#
# Retour:
#
# Remarques :
#----------------------------------------------------------------------------

proc Log::Stack { } {

   set stack "(DEBUG) Stack trace:\n"

   for { set i 1 } { $i < [info level] } { incr i } {
      set lvl [info level -$i]
      set pname [lindex $lvl 0]
      append stack [string repeat "   " $i]$pname

      foreach value [lrange $lvl 1 end] arg [info args $pname] {
         if { $value eq "" } {
            info default $pname $arg value
         }
         append stack " $arg='$value'"
      }
      append stack \n
   }

   return $stack
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
   append Param(JobId) "-$Param(JobDate)"

   Log::Print MUST "-------------------------------------------------------------------------------"
   Log::Print MUST "App/Script          : $Job"
   Log::Print MUST "Version             : $Version"
   catch { Log::Print MUST "SPI library         : [package version TclGeoEER]" }
   Log::Print MUST "Hostname            : [system info -name]"
   Log::Print MUST "Architecture        : [expr {[info exists env(ORDENV_PLAT)] ? $env(ORDENV_PLAT) : [exec uname -s -m]}]"
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
         Log::Print MUST "   Waiting time     : [clock format [expr $Param(SecTime)-${secs}] -format "%H:%M:%S" -timezone :UTC]"
      }
    } elseif { [info exists env(SGE_CELL)] } {
      Log::Print MUST "Queue Method        : sge"
      catch { Log::Print MUST "   Queue               : $env(QUEUE)" }
      catch { Log::Print MUST "   Job ID              : $env(JOB_ID)" }
      if { [file exists $Input] } {
         set secs [file mtime $Input]
         Log::Print MUST "   Waiting time     : [clock format [expr $Param(SecTime)-${secs}] -format "%H:%M:%S" -timezone :UTC]"
      }
    } elseif { [info exists env(PBS_JOBID)] } {
      Log::Print MUST "Queue Method        : pbs"
      catch { Log::Print MUST "   Queue               : $env(PBS_QUEUE)" }
      catch { Log::Print MUST "   Job ID              : $env(PBS_JOBID)" }
      if { [file exists $Input] } {
         set secs [file mtime $Input]
         Log::Print MUST "   Waiting time     : [clock format [expr $Param(SecTime)-${secs}] -format "%H:%M:%S" -timezone :UTC]"
      }
   }
   Log::Print MUST "Start time          : [clock format $Param(SecStart)]"
   Log::Print MUST "-------------------------------------------------------------------------------\n"

   Log::CheckSPI $Param(SPI)

   if { $Param(JobClass)=="INTERACTIVE" } {
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

   if { $Status==-1 } {
       set Status [expr {$Param(Error)>0 ? 1 : 0}]
   }

   set Param(SecEnd) [clock seconds]
   set runsec [expr $Param(SecEnd)-$Param(SecStart)]

   #----- For long running programs only (like EERDaemon)
   set runday [expr $runsec/86400]
   set runday [expr {$runday?"${runday}d ":""}]

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
   Log::Print MUST "Total running time  : [clock format $runsec -format "$runday%H:%M:%S" -timezone :UTC]"
   Log::Print MUST "-------------------------------------------------------------------------------\n"

   flush $Param(Out)

   if { $Param(JobClass)=="REPORT" } {
      if { $Param(Error)>0 } {
         Log::Mail "Job finished (ERROR ($Param(Error)))" $Param(OutFile)
      } elseif { $Param(Warning)>0 } {
         Log::Mail "Job finished (WARNING ($Param(Warning)))" $Param(OutFile)
      } elseif { $Param(JobReport)==True || $Param(JobReport)=="ALL" } {
         Log::Mail "Job finished (NORMAL)" $Param(OutFile)
      }
   } elseif { $Status==0 } {
      if { $Param(JobClass)=="INTERACTIVE" } {
         Log::Mail "Job finished (NORMAL)" $Param(OutFile)
      }
   } else {
      Log::Mail "Job finished (ERROR ($Param(Error)))" $Param(OutFile)
   }

   #----- End hook
   Log::HookEnd $Status

   if { $Param(Out) ni {stdout stderr} } {
      close $Param(Out)
   }

   if { $Exit } {
      exit $Status
   }
}


#----------------------------------------------------------------------------
# Nom      : <Log::Print>
# Creation : Octobre 2009 - J.P. Gauthier - CMC/CMOE
#
# But      : Afficher un message standard.
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

         set Param(OutFile) $Param(Path)/[clock format [clock seconds] -format "%Y%m%d%H%M" -timezone :UTC]-[pid].log
      } else {
         if { $Param(Rotate) && [file exists $Param(Out)] } {
            file rename -force $Param(Out) $Param(Out).[clock format [clock seconds] -format "%Y%m%d%H%M" -timezone :UTC]
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
         file rename -force $Param(OutFile) $Param(OutFile).[clock format $Param(SecLog) -format "%Y%m%d" -timezone :UTC]
         set Param(Out) [open $Param(OutFile) w]
         fconfigure $Param(Out) -buffering line
      }
      incr Param(SecLog) $Param(Rotate)

      #----- Rotate hook
      Log::HookRotate
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

      if { $Param(Color) } {
         set cstart $Color($Type)
         set cend   $Color(RESET)
      } else {
         set cstart ""
         set cend ""
      }

      if { $Type=="MUST" } {
         puts $Param(Out) "${Message}"
      } else {
         puts $Param(Out) "${cstart}${time}${id}(${Type}) ${proc}${Message}${vars}${cend}"
      }
   }
}

#----------------------------------------------------------------------------
# Nom      : <Log::Progress>
# Creation : Septembre 2014 - E. Legault-Ouellet - CMC/CMOE
#
# But      : Afficher le progret d'une tâche dans un format standard et
#            plus facile à parser automatiquement.
#
# Parametres  :
#    <Percent>: Le pourcentage d'avancement de la job
#    <Msg>    : Message supplémentaire
#
# Retour:
#
# Remarques :
#----------------------------------------------------------------------------

proc Log::Progress { Percent {Msg ""} } {
   variable Param
   variable Color

   if { $Param(Color) } {
       set cstart $Color(PROGRESS)
       set cend   $Color(RESET)
   } else {
       set cstart ""
       set cend ""
   }

   #----- Do we print the time
  if { $Param(Time) } {
     set time "([clock format [clock seconds]]) "
  } else {
     set time ""
  }

   puts $Param(Out) "${cstart}${time}(PROGRESS) \[[format %6.2f $Percent] %\] $Msg$cend"
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

   set err 0

   if { ![llength $Address] } {
      set Address $Param(MailTo)
   }

   if { [llength $Address] } {
      if { ![file exists $File] || ![file readable $File] } {
         set err [catch { exec -ignorestderr echo -e $File | mail -s "$Param(MailTitle) - ${Subject} ($Param(JobId))" {*}$Address } msg]
      } else {
         set err [catch { exec -ignorestderr mail -s "$Param(MailTitle) - ${Subject} ($Param(JobId))" {*}$Address < $File } msg]
      }
      if { $err } {
         Log::Print ERROR "Problems while mailing info to $Address:\n\n\t$msg"
      }
   }
}

#----------------------------------------------------------------------------
# Nom      : <Log::Hook*>
# Creation : Février 2021 - E. Legault-Ouellet - CMC/CMOE
#
# But      : Hooks called to allow scritps to put a trace at specific steps
#
# Parametres  :
#
# Retour:
#
# Remarques :
#     The main purpose of these hooks is so that other scripts can put a trace
#     on the execution of these function. It should not contain anything.
#----------------------------------------------------------------------------
proc Log::HookEnd { Status } { }
proc Log::HookRotate { } { }
