#===============================================================================
# Environnement Canada
# Centre Meteorologique Canadian
# 2100 Trans-Canadienne
# Dorval, Quebec
#
# Projet    : Librairie de gestion des traces
# Fichier   : Debug.tcl
# Creation  : Mars 1999 - J.P. Gauthier - CMC/CMOE
#
# Description: Definitions de diverses fonctions pour la gestion des traces.
#
# Fonctions:
#
#   Debug::Close           { }
#   Debug::Init            { Level Output Apps }
#   Debug::StackPrint      { }
#   Debug::TraceOutputProc { Level }
#   Debug::TraceOutputVar  { Name1 Name2 Mode }
#   Debug::TraceProc       { Trace args }
#   Debug::TraceVar        { Var Mode }
#   Debug::UnTraceVar      { Var Mode }
#
# Remarques :
#   Aucune
#
#===============================================================================

package provide Debug 1.1

proc IdDebug { Show } {

   if { $Show } {
      puts "(INFO) Loading Standard CMC/CMOE Package Debug Version 1.1"
   }
}

namespace eval Debug {
   variable Defs

   set Defs(Apps)      [info script]  ;#Identification de l'application
   set Defs(Level)     0              ;#Niveau de trace
   set Defs(Interface) False          ;#Interface active
   set Defs(Output)    stdout         ;#Canal de sortie de la trace
   set Defs(Seconds)   0              ;#Temps ecoule dans la trace (Application)
   set Defs(StackCall) ""             ;#Identification de l'appel au Stack info
   set Defs(Active)    True           ;#Activation des traces

   set Lbl(Title)      { "Debug" "Debug" }
   set Lbl(Version)    "1.0"
   set Lbl(Close)      { "Fermer" "Close" }
}

#-------------------------------------------------------------------------------
# Nom      : <Debug::Close>
# Creation : Mars 1999 - J.P. Gauthier - CMC/CMOE
#
# But      : Stoppe la trace
#
# Parametres :
#
# Remarques :
#    Aucune.
#
#-------------------------------------------------------------------------------

proc Debug::Close { } {
   variable Defs

   set spent [expr [clock seconds] - $Defs(Seconds)]
   set time  [clock format [clock seconds] -format "%Y%m%d %H:%M:%S" -gmt true]

   if { $Defs(Interface) } {
      .debug.proctrace.list.list insert end "$time Debug::Close for $Defs(Apps) (Time spent $spent seconds)"
   } else {
      puts $Defs(Output) "$time Debug::Close for $Defs(Apps) (Time spent $spent seconds)"
   }
}

#-------------------------------------------------------------------------------
# Nom      : <Debug::Create>
# Creation : Mars 1999 - J.P. Gauthier - CMC/CMOE
#
# But      : Cree une interface pour le debugger
#
# Parametres :
#
# Remarques :
#    Aucune.
#
#-------------------------------------------------------------------------------

proc Debug::Create { } {
   global GDefs
   variable Lbl
   variable Defs

   set Defs(Interface) True

   interp create .debug

   toplevel .debug -bg $GDefs(ColorLight)
   wm title .debug "[lindex $Lbl(Title) $GDefs(Lang)] $Lbl(Version)"

   frame .debug.proctrace -relief ridge -bd 1
      label .debug.proctrace.lbl -text "Procedure Traces" -relief raised -bd 1 -bg yellow
      frame .debug.proctrace.list
         listbox .debug.proctrace.list.list -yscrollcommand [list .debug.proctrace.list.scrolly set] \
            -height 5 -width 40 -relief sunken -bd 1 -bg $GDefs(ColorLight) -exportselection 0 -selectmode single
         scrollbar .debug.proctrace.list.scrolly -orient vertical -command [list .debug.proctrace.list.list yview]
         pack .debug.proctrace.list.list -side left -fill both -expand true
         pack .debug.proctrace.list.scrolly -side right -fill y
      pack .debug.proctrace.lbl -side top -fill x -ipady 2
      pack .debug.proctrace.list -side top -fill both -expand true
   pack .debug.proctrace -side top -fill both -expand true -pady 5 -padx 5

   frame .debug.stack -relief ridge -bd 1
      label .debug.stack.lbl -text "Stack info" -relief raised -bd 1  -bg yellow
      label .debug.stack.hdr -text "Lvl Procedure                 Params" -anchor w -relief raised -bd 1
      label .debug.stack.info -textvariable Debug::Defs(StackCall) -bg $GDefs(ColorLight) \
         -relief sunken -bd 1 -anchor w
      frame .debug.stack.list
         listbox .debug.stack.list.list -yscrollcommand [list .debug.stack.list.scrolly set] -height 5 -width 40  \
            -relief sunken -bd 1 -bg $GDefs(ColorLight) -exportselection 0 -selectmode single
         scrollbar .debug.stack.list.scrolly -orient vertical -command [list .debug.stack.list.list yview]
         pack .debug.stack.list.list -side left -fill both -expand true
         pack .debug.stack.list.scrolly -side right -fill y
      pack .debug.stack.lbl .debug.stack.hdr -side top -fill x -ipady 2
      pack .debug.stack.list -side top -fill both -expand true
      pack .debug.stack.info -side top -fill x -ipady 2
   pack .debug.stack -side top -fill both -expand true -pady 5 -padx 5

   frame .debug.commands
      button .debug.commands.close -text [lindex $Lbl(Close) $GDefs(Lang)] \
         -command "set Debug::Defs(Interface) False ; destroy .debug"
      pack .debug.commands.close -side left -fill x -expand true
   pack .debug.commands -side top -fill x
}

#-------------------------------------------------------------------------------
# Nom      : <Debug::Init>
# Creation : Mars 1999 - J.P. Gauthier - CMC/CMOE
#
# But      : Initialiser les parametres de traces
#
# Parametres :
#   <Level>  : Niveau de trace (0, 1, 2)
#   <Output> : Chemin de sortie des traces (stdout,stderr)
#   <Apps>   : Nom de l'application
#
# Remarques :
#    -Si le niveau de trace est -1 alors on utilise la variable globale GDefs(Debug).
#
#-------------------------------------------------------------------------------

proc Debug::Init { Level Output Apps } {
   global env
   global GDefs
   variable Defs

   set Defs(Output)  $Output
   set Defs(Apps)    $Apps
   set Defs(Seconds) [clock seconds]

   if { $Output == -1 } {
      set Defs(Output) $GDefs(DebugOutput)
   } else {
      set Defs(Output) $Output
   }

   if { $Level == -1 } {
      set Defs(Level) $GDefs(DebugLevel)
   } else {
      set Defs(Level) $Level
   }

   set time  [clock format [clock seconds] -format "%Y%m%d %H:%M:%S" -gmt true]
   set version "(tcl [info tclversion])"

   if { $Defs(Interface) } {
      .debug.proctrace.list.list insert end "$time Debug::Init for $Defs(Apps) $version"
   } else {
      if { $GDefs(Arch)!="Win32" } {
         puts $Defs(Output) "$time Debug::Init for $Defs(Apps) $version"
         puts $Defs(Output) "   USER    : $env(USER)"
         puts $Defs(Output) "   HOST    : [info hostname]"
         puts $Defs(Output) "   PID     : [pid]"

         #----- Il semble que le call a tty plante dans certain cas donc, on le catch

        catch { puts $Defs(Output) "   TTY     : [exec tty]" }
        puts $Defs(Output) "   DISPLAY : $env(DISPLAY)"
     }
   }
}

#-------------------------------------------------------------------------------
# Nom      : <Debug::StackPrint>
# Creation : Mars 1999 - J.P. Gauthier - CMC/CMOE
#
# But      : Afficher le contenu de la pile d'execution (Stack)
#
# Parametres :
#
# Remarques :
#    Aucune.
#
#-------------------------------------------------------------------------------

proc Debug::StackPrint { } {
   variable Defs

   #----- Recupere le niveau actuel de la pile d'execution (stack)

   set stack [info level]

   #----- Si l'interface existe, l'utiliser

   if { $Defs(Interface) } {
      .debug.stack.list.list delete 0 end
      set Defs(StackCall) "Stack call from [lindex [info level [expr $stack -1]] 0]"
   } else {
      puts $Defs(Output) "[Debug::TraceOutputProc 0] -Stack trace"
   }

   #----- Pour chaque niveau (on redescend dans la pile

   for { set i $stack } { $i > 0 } { incr i -1 } {

      set stacklevel  $i                        ;#Niveau actuel
      set stacklist   [info level $i]           ;#List de l'appel de procedure
      set stackproc   [lindex $stacklist 0]     ;#Nom de la procedure
      set stackparams [lrange $stacklist 1 end] ;#Parametres passe a la procedure
      set stackargs   [info args $stackproc]    ;#Arguments de la procedure
      set stackbody   [info body $stackproc]    ;#Corps de la procedure (Code)

      if { $Defs(Interface) } {
         .debug.stack.list.list insert end \
            "[format "%03i" $stacklevel] [format "%-25s" $stackproc] [format "%-25s" $stackparams]"
      } else {
         puts $Defs(Output) "      stacklevel $stacklevel:  $stackproc \{ $stackargs \} \"$stackparams\""
      }
   }
}

#-------------------------------------------------------------------------------
# Nom      : <Debug::TraceOutputProc>
# Creation : Mars 1999 - J.P. Gauthier - CMC/CMOE
#
# But      : Formattage de la trace selon le niveau definie.
#
# Parametres :
#   <Trace>  : Trace a sortir
#   <args>   : Niveau de trace pour le cas present
#
# Remarques :
#   -Si le niveau de trace (args) n'est pas specifie, alors on suppose le niveau
#    par defaut de l'initialisation.
#
#-------------------------------------------------------------------------------

proc Debug::TraceOutputProc { Level } {
   variable Defs

   if { !$Defs(Active) } {
      return
   }

   #----- Recupere les parametres de la procedure appelante

   set stacklevel [expr [info level] -2]
   set stacklist  [info level $stacklevel]
   set stackproc  [lindex $stacklist 0]
   set stackargs  [lrange $stacklist 1 end]

   #----- Calcule des parametres variables de la trace

   if { $Defs(Interface) } {
      set indent ""
   } else {
      set indent [format "%3s" ""]
   }
   set time  [clock format [clock seconds] -format "%Y%m%d %H:%M:%S" -gmt true]

   #----- Sortie de la trace

   #----- Niveau 0
   #----- Format: Apps Date Time -Traceinfo

   set trace "$indent$time $Defs(Apps)"

   #----- Niveau 1
   #----- Format: Apps Date Time StackLevel StackProc -Traceinfo

   if { $Level > 0 } {
      set trace "$trace SL$stacklevel \"$stackproc\""
   }

   #----- Niveau 2
   #----- Format: Apps Date Time StackLevel StackProc StackArgs -Traceinfo

   if { $Level > 1 } {
      set trace "$trace \{ $stackargs \}"
   }

   return "$trace"
}

#-------------------------------------------------------------------------------
# Nom      : <Debug::TraceOutputVar>
# Creation : Mars 1999 - J.P. Gauthier - CMC/CMOE
#
# But      : Sortie de la trace sur les variables
#
# Parametres :
#   <Name1>  : Partie 1 du nom de la variable
#   <Name2>  : Partie 2 du nom de la variable
#   <Mode>   : Mode d'activation de la trace (rwu)
#
# Remarques :
#    Aucune.
#
#-------------------------------------------------------------------------------

proc Debug::TraceOutputVar { Name1 Name2 Mode } {
   variable Defs
   global $Name1

   if { !$Defs(Active) } {
      return
   }

   #----- Determiner si la variable est un Array

   if { [array exists $Name1] } {
      eval set trace \"${Name1}($Name2) $Mode \$${Name1}($Name2)\"
   } else {
      eval set trace \"$Name1 $Mode \$$Name1\"
   }

   puts $Defs(Output) "[Debug::TraceOutputProc $Defs(Level)] -Variable $trace"
}

#-------------------------------------------------------------------------------
# Nom      : <Debug::TraceProc>
# Creation : Mars 1999 - J.P. Gauthier - CMC/CMOE
#
# But      : Sortie de la trace
#
# Parametres :
#   <Trace>  : Trace a sortir
#   <args>   : Niveau de trace pour le cas present
#
# Remarques :
#   -Si le niveau de trace (args) n'est pas specifie, alors on suppose le niveau
#    par defaut de l'initialisation.
#
#-------------------------------------------------------------------------------

proc Debug::TraceProc { Trace args } {
   variable Defs

   if { !$Defs(Active) } {
      return
   }

   #----- Verifier le niveau en argument

   if { $args != "" } {
      set level $args
   } else {
      set level $Defs(Level)
   }

   #----- Si l'interface existe, l'utiliser

   if { $Defs(Interface) } {
      .debug.proctrace.list.list insert end "[Debug::TraceOutputProc $level] -$Trace"
   } else {
      puts $Defs(Output) "[Debug::TraceOutputProc $level] -$Trace"
   }
}

#-------------------------------------------------------------------------------
# Nom      : <Debug::TraceVar>
# Creation : Mars 1999 - J.P. Gauthier - CMC/CMOE
#
# But      : Initialiser les parametres de trace sur les variables
#
# Parametres :
#   <Var>    : Nom de la variable
#   <Mode>   : Mode d'activation de la trace (rwu)
#
# Remarques :
#    Aucune.
#
#-------------------------------------------------------------------------------

proc Debug::TraceVar { Var Mode } {
   global $Var

   trace variable $Var $Mode { Debug::TraceOutputVar }
}

#-------------------------------------------------------------------------------
# Nom      : <Debug::UnTraceVar>
# Creation : Mars 1999 - J.P. Gauthier - CMC/CMOE
#
# But      : Supprime la trace sur la variable specifie
#
# Parametres :
#   <Var>    : Nom de la variable
#   <Mode>   : Mode d'activation de la trace (rwu)
#
# Remarques :
#    Aucune.
#
#-------------------------------------------------------------------------------

proc Debug::UnTraceVar { Var Mode } {
   global $Var
   variable Defs

   trace vdelete $Var $Mode Debug::TraceOutputVar
}
