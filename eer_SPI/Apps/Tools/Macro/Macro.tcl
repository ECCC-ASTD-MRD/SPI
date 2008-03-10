#===============================================================================
# Environnement Canada
# Centre Meteorologique Canadian
# 2100 Trans-Canadienne
# Dorval, Quebec
#
# Projet   : Librairie de "Widget" Tk.
# Fichier  : Macro.tcl
# Version  : 1.2
# Creation : Juillet 2005 - J.P.Gauthier - CMC/CMOE
#
# Description:
#    Interface de gestion de macros.
#
# Fonctions:
#
#
# Remarques :
#
# Modification:
#
#   Nom         :
#   Date        :
#   Description :
#
#===============================================================================

#----- Lire les sources d'execution

source $GDefs(Dir)/Apps/Tools/Macro/Macro.ctes
source $GDefs(Dir)/Apps/Tools/Macro/Macro.txt
source $GDefs(Dir)/Apps/Tools/Macro/Macro.int

#-------------------------------------------------------------------------------
# Nom      : <Macro::Cursor>
# Creation : Decembre 2006 - J.P. Gauthier - CMC/CMOE
#
# But      : Changer le curseur.
#
# Parametres :
#   <Cursor> : curseur a utiliser (watch, left_ptr)
#
# Retour    :
#
# Remarque :
#
# Modifications  :
#    Nom         : -
#    Date        : -
#    Description : -
#-------------------------------------------------------------------------------

proc Macro::Cursor { Cursor } {

   $Page::Data(Canvas) config -cursor $Cursor
   if { [winfo exists .macro] } {
      .macro config -cursor $Cursor
   }
   . config -cursor $Cursor
   update idletasks
}

#-------------------------------------------------------------------------------
# Nom      : <Macro::Close>
# Creation : Mai 2005 - J.P. Gauthier - CMC/CMOE
#
# But      : Ferme tout les fichiers ouvert et detruit la fenetre.
#
# Parametres :
#
# Retour    :
#
# Remarque :
#
# Modifications  :
#    Nom         : -
#    Date        : -
#    Description : -
#-------------------------------------------------------------------------------

proc Macro::Close { } {
   global GDefs
   variable Data
   variable Msg
   variable Lbl

   if { $Page::Data(ToolMode)=="Macro" } {
      SPI::ToolMode SPI
   }

   #----- Save the modified macros ?

   foreach macro $Data(List) {
      if { $Data(Save$macro) } {
         set nok [Dialog::CreateDefault . 300 [lindex $Lbl(Warning) $GDefs(Lang)] "[lindex $Msg(Save) $GDefs(Lang)]\n\n\tMacro::$macro" info 0 [lindex $Lbl(Yes) $GDefs(Lang)] [lindex $Lbl(No) $GDefs(Lang)]]

         if { !$nok } {
            Macro::Save $macro
         }
      }
   }

   set Data(Active) 0
   ListboxBubble::Destroy $Data(Tab).desc.data.list
   destroy .macro

   if { !$SPI::Param(Window) } { SPI::Quit }
}

#-------------------------------------------------------------------------------
# Nom      : <Macro::Error>
# Creation : Mai 2005 - J.P. Gauthier - CMC/CMOE
#
# But      : Afficher un message d'erreur.
#
# Parametres :
#   <Error>  : Message d'erreur dans les 2+ langues sous forme de liste
#
# Retour    :
#
# Remarque :
#
# Modifications  :
#    Nom         : -
#    Date        : -
#    Description : -
#-------------------------------------------------------------------------------

proc Macro::Error { Error } {
   global GDefs

   #----- Recuperer le nom de la procedure fautive

   set stacklist [info level -1]
   set stackproc [lindex $stacklist 0]

   Dialog::CreateError .macro "${stackproc}\n\n[lindex $Error $GDefs(Lang)]" $GDefs(Lang)
}

#-------------------------------------------------------------------------------
# Nom      : <Macro::Doing>
# Creation : Mai 2005 - J.P. Gauthier - CMC/CMOE
#
# But      : Afficher un message d'erreur.
#
# Parametres :
#   <Error>  : Message d'erreur dans les 2+ langues sous forme de liste
#
# Retour    :
#
# Remarque :
#
# Modifications  :
#    Nom         : -
#    Date        : -
#    Description : -
#-------------------------------------------------------------------------------

proc Macro::Doing { Msg } {
   variable Data

   #----- Recuperer le nom de la procedure fautive

   set Data(Job) $Msg
   update idletasks
}

#-------------------------------------------------------------------------------
# Nom      : <Macro::Info>
# Creation : Mai 2005 - J.P. Gauthier - CMC/CMOE
#
# But      : Afficher un message.
#
# Parametres :
#   <Info>   : Message
#
# Retour    :
#
# Remarque :
#
# Modifications  :
#    Nom         : -
#    Date        : -
#    Description : -
#-------------------------------------------------------------------------------

proc Macro::Info { Info } {

   set stacklist [info level -1]
   set stackproc [lindex $stacklist 0]

   Dialog::CreateInfo .macro "${stackproc}\n\n$Info"
}

#-------------------------------------------------------------------------------
# Nom      : <Macro::Id>
# Creation : Novembre 2005 - J.P. Gauthier - CMC/CMOE
#
# But      : Obtenir un id unique pour une macro.
#
# Parametres :
#   <Error>  : Message d'erreur dans les 2+ langues sous forme de liste
#
# Retour    :
#    <Id>   : Id unuque pour la macro
#
# Remarque :
#
# Modifications  :
#    Nom         : -
#    Date        : -
#    Description : -
#-------------------------------------------------------------------------------

proc Macro::Id { } {
   global GDefs

   return [uplevel 1 namespace current]
}

#-------------------------------------------------------------------------------
# Nom      : <Macro::Load>
# Creation : Mai 2005 - J.P. Gauthier - CMC/CMOE
#
# But      : Lire une liste de macro et les inserer dans la liste des macros .
#
# Parametres :
#   <Paths>  : Liste des fichiers macros
#
# Retour    :
#
# Remarque :
#    Chaques macro est teste pour une certaine validite
#
# Modifications  :
#    Nom         : -
#    Date        : -
#    Description : -
#-------------------------------------------------------------------------------

proc Macro::Load { Paths } {
   variable Data

   if { ![llength $Paths] } {
      return
   }

   foreach path $Paths {
      set macro [file tail [file rootname $path]]
      set Data(Time$macro) [file mtime $path]
      set Data(Path$macro) $path
      set Data(Code$macro) ""
      set Data(Save$macro) False

      #----- Keep the source code

      set f [open $Data(Path$macro) r]
      while { ![eof $f] } {
         append Data(Code$macro) [gets $f ]\n
      }

      #----- But still source it

      uplevel #0 source $path
      set idx [Macro::Define $macro]
   }

   if { [winfo exists $Data(Tab).desc.data.list] } {
      $Data(Tab).desc.data.list selection clear 0 end
      $Data(Tab).desc.data.list selection set $idx

      Macro::Select $macro
   }
}

#-------------------------------------------------------------------------------
# Nom      : <Macro::Check>
# Creation : Mai 2005 - J.P. Gauthier - CMC/CMOE
#
# But      : Verifier la validite d'une macro.
#
# Parametres :
#   <Macro>  : Macro a verifier
#
# Retour    :
#
# Remarque :
#
# Modifications  :
#    Nom         : -
#    Date        : -
#    Description : -
#-------------------------------------------------------------------------------

proc Macro::Check { Macro } {
   global GDefs
   variable Data
   variable Lbl
   variable Error

   #----- Check de la validite de la macro

   eval set proc \[info procs ::Macro::${Macro}::Execute\]
   if { $proc=="" } {
      if { [winfo exists .macro] } {
         Dialog::CreateDefault .macro 400 "Macro::[lindex $Lbl(Error) $GDefs(Lang)]  (Macro::$Macro)" [lindex $Error(Execute) $GDefs(Lang)] error 0 [lindex $Lbl(Ok) $GDefs(Lang)]
      } else {
         puts stderr "Macro::[lindex $Lbl(Error) $GDefs(Lang)]  (Macro::$Macro) : [lindex $Error(Execute) $GDefs(Lang)]"
      }
      return False
   }

   if { ![info exists ::Macro::${Macro}::Param(Info)] } {
      if { [winfo exists .macro] } {
         Dialog::CreateDefault .macro 400 "Macro::[lindex $Lbl(Error) $GDefs(Lang)]  (Macro::$Macro)" [lindex $Error(Info) $GDefs(Lang)] error 0 [lindex $Lbl(Ok) $GDefs(Lang)]
      } else {
         puts stderr "Macro::[lindex $Lbl(Error) $GDefs(Lang)]  (Macro::$Macro) : [lindex $Error(Info) $GDefs(Lang)]"
      }
      return False
   }
   return True
}

#-------------------------------------------------------------------------------
# Nom      : <Macro::Define>
# Creation : Mai 2005 - J.P. Gauthier - CMC/CMOE
#
# But      : Definir une nouvelle macro dans l'environnement.
#
# Parametres :
#   <Macro>  : Macro a verifier
#
# Retour    :
#
# Remarque :
#    Chaques macro est teste pour une certaine validite
#
# Modifications  :
#    Nom         : -
#    Date        : -
#    Description : -
#-------------------------------------------------------------------------------

proc Macro::Define { Macros } {
   global GDefs
   variable Data
   variable Lbl
   variable Error

   set idx -1

   foreach macro $Macros {
      if { [set idx [lsearch -exact $Data(List) $macro]]==-1 } {
         lappend Data(List) $macro
         set idx [expr [llength $Data(List)]-1]
      }
   }
   return $idx
}

#-------------------------------------------------------------------------------
# Nom      : <Macro::UnDefine>
# Creation : Mai 2005 - J.P. Gauthier - CMC/CMOE
#
# But      : Supprimer une macro de l'environnement.
#
# Parametres :
#   <Macros> : Liste des macros a supprimer
#
# Retour    :
#
# Remarque :
#
# Modifications  :
#    Nom         : -
#    Date        : -
#    Description : -
#-------------------------------------------------------------------------------

proc Macro::UnDefine { Macros } {
   variable Data

   set idx -1

   foreach macro $Macros {
      if { [set idx [lsearch -exact $Data(List) $macro]]!=-1 } {
         set Data(List) [lreplace $Data(List) $idx $idx]
         namespace delete ::Macro::$macro
      }
   }
   return $idx
}

#-------------------------------------------------------------------------------
# Nom      : <Macro::Execute>
# Creation : Mai 2005 - J.P. Gauthier - CMC/CMOE
#
# But      : Execution d'une macro.
#
# Parametres :
#   <Macro>  : Macro a executer
#
# Retour    :
#
# Remarque :
#
# Modifications  :
#    Nom         : -
#    Date        : -
#    Description : -
#-------------------------------------------------------------------------------

proc Macro::Execute { Macro } {
   global GDefs
   variable Lbl
   variable Msg
   variable Data

   Macro::Cursor watch

   #----- Check for previous macro cleanup function

   eval set proc \[info procs ::Macro::${Data(Current)}::Clean\]
   if { $proc!="" } {
      Macro::${Data(Current)}::Clean
   }

   #----- Check that it is still valid

   if { $Data(Time$Macro)<[file mtime $Data(Path$Macro)] } {
      set cont [Dialog::CreateDefault . 300 [lindex $Lbl(Warning) $GDefs(Lang)] "[lindex $Msg(Modified) $GDefs(Lang)]\n\n\tMacro::$Macro" info 0 [lindex $Lbl(Yes) $GDefs(Lang)] [lindex $Lbl(No) $GDefs(Lang)]]

      if { !$cont } {
         Macro::Load $Data(Path$Macro)
      } else {
         set Data(Time$Macro) [file mtime $Data(Path$Macro)]
      }
   }

   #----- Evaluate the source code

   if { [namespace exist ::Macro::$Macro] } {
      namespace delete ::Macro::$Macro
   }

   set Data(Code$Macro) [$Data(Tab).desc.text.list get 0.0 end]
   uplevel #0 $Data(Code$Macro)

   #----- If macro is valid

   if { [Macro::Check $Macro] } {

      #----- Launch the current macro

      set Data(Current) $Macro
      set Data(VP)      $Viewport::Data(VP)
      set Data(Frame)   $Page::Data(Frame)
      eval Macro::${Macro}::Execute

      Page::Activate $Data(Frame)
      Viewport::Activate $Data(Frame) $Data(VP)
   } else {
      set Data(Current) ""
   }

   Macro::Cursor left_ptr
}

#-------------------------------------------------------------------------------
# Nom      : <Macro::Reset>
# Creation : Janvier 2006 - J.P. Gauthier - CMC/CMOE
#
# But      : Reinitialiser la page.
#
# Parametres :
#
# Retour    :
#
# Remarque :
#
# Modifications  :
#    Nom         : -
#    Date        : -
#    Description : -
#-------------------------------------------------------------------------------

proc Macro::Reset { } {
   variable Data

   Macro::Cursor watch

   #----- Check for previous macro cleanup function

   eval set proc \[info procs ::Macro::${Data(Current)}::Clean\]
   if { $proc!="" } {
      Macro::${Data(Current)}::Clean
   }

   SPI::LayoutLoad $Data(Frame) SPI
   Macro::Cursor left_ptr
}

#-------------------------------------------------------------------------------
# Nom      : <Macro::Save>
# Creation : Mai 2005 - J.P. Gauthier - CMC/CMOE
#
# But      : Execution d'une macro.
#
# Parametres :
#   <Macro>  : Macro a executer
#
# Retour    :
#
# Remarque :
#
# Modifications  :
#    Nom         : -
#    Date        : -
#    Description : -
#-------------------------------------------------------------------------------

proc Macro::Save { Macro } {
   global GDefs
   variable Lbl
   variable Msg
   variable Data

   if { $Macro!="" } {

      #----- Check that it is still valid

      if { $Data(Time$Macro)<[file mtime $Data(Path$Macro)] } {
         set cont [Dialog::CreateDefault . 300 [lindex $Lbl(Warning) $GDefs(Lang)] "[lindex $Msg(Overwrite) $GDefs(Lang)]\n\n\tMacro::$Macro" info 1 [lindex $Lbl(Yes) $GDefs(Lang)] [lindex $Lbl(No) $GDefs(Lang)]]

         if { $cont } {
            return
         }
      }

      set Data(Code$Macro) [$Data(Tab).desc.text.list get 0.0 end]
      set Data(Save$Macro) False

      set f [open $Data(Path$Macro) w]
      puts $f $Data(Code$Macro)
      close $f

      set Data(Time$Macro) [file mtime $Data(Path$Macro)]
   }
}

#-------------------------------------------------------------------------------
# Nom      : <Macro::New>
# Creation : Juillet 2006 - J.P. Gauthier - CMC/CMOE
#
# But      : Creer une nouvelle macro.
#
# Parametres :
#   <Macro>  : Macro a inserer
#
# Retour    :
#
# Remarque :
#
# Modifications  :
#    Nom         : -
#    Date        : -
#    Description : -
#-------------------------------------------------------------------------------

proc Macro::New { Path } {
   variable Data

   if { $Path!="" } {

      set macro [file tail [file rootname $Path]]

      set f [open $Path w]
      puts $f "namespace eval Macro::${macro} { } {\n   variable Param\n   variable Data\n   variable Error\n\n   set Data(Something)  \"Some data value\"\n\n   set Error(Something) { \"Une erreur quelconque\" \"Some error\" }\n\n   set Param(Info)      { \"Description de la macro\" \"Macro description\" }\n}\n"
      puts $f "proc Macro::${macro}::Execute { } {\n   variable Data\n\n}\n"
      puts $f "proc Macro::${macro}::Clean { } {\n   variable Data\n\n}\n"

      close $f

      Macro::Load $Path
   }
}

#-------------------------------------------------------------------------------
# Nom      : <Macro::Select>
# Creation : Juillet 2006 - J.P. Gauthier - CMC/CMOE
#
# But      : Inserer le texte de la macro dans l'editeur .
#
# Parametres :
#   <Macro>  : Macro a inserer
#
# Retour    :
#
# Remarque :
#
# Modifications  :
#    Nom         : -
#    Date        : -
#    Description : -
#-------------------------------------------------------------------------------

proc Macro::Select { Macro } {
   variable Data

   $Data(Tab).head.save configure -state disabled
   bind $Data(Tab).desc.text.list <<Modified>>  { }

   if { $Macro!="" } {
      if { $Data(Edit)!="" && $Data(Edit)!=$Macro } {
         set Data(Code$Data(Edit)) [$Data(Tab).desc.text.list get 0.0 end]
      }
      set Data(Edit) $Macro

      $Data(Tab).desc.text.list delete 0.0 end
      $Data(Tab).desc.text.list insert end $Data(Code$Data(Edit))

      if { $Data(Save$Macro) } {
         $Data(Tab).head.save configure -state normal
      }
      $Data(Tab).desc.text.list edit modified False
      bind $Data(Tab).desc.text.list <<Modified>> "set Macro::Data(Save$Macro) True ;$Macro::Data(Tab).head.save configure -state normal"
   } else {
      $Data(Tab).desc.text.list delete 0.0 end
   }
}

#-------------------------------------------------------------------------------
# Nom      : <Macro::Current>
# Creation : Juillet 2006 - J.P. Gauthier - CMC/CMOE
#
# But      : Recuperer la macro courante.
#
# Parametres :
#
# Retour    :
#     <Macro> : Macro courante
#
# Remarque :
#
# Modifications  :
#    Nom         : -
#    Date        : -
#    Description : -
#-------------------------------------------------------------------------------

proc Macro::Current { } {
   variable Data

   if  { [set sel [$Data(Tab).desc.data.list curselection]]!="" } {
      return [$Data(Tab).desc.data.list get $sel]
   } else {
      return ""
   }
}

#-------------------------------------------------------------------------------
# Nom      : <Macro::Bubble>
# Creation : Mai 2005 - J.P. Gauthier - CMC/CMOE
#
# But      : Afficher une bulle d'information decrivant la macro.
#
# Parametres :
#   <Index>  : Index de la macro dans la liste des macro connues
#
# Retour    :
#
# Remarque :
#
# Modifications  :
#    Nom         : -
#    Date        : -
#    Description : -
#-------------------------------------------------------------------------------

proc Macro::Bubble { Index } {
   global GDefs
   variable Data

   if { [info exists ::Macro::[lindex $Data(List) $Index]::Param(Info)] } {
      eval set info \[lindex \$Macro::[lindex $Data(List) $Index]::Param(Info) \$GDefs(Lang)\]
      return $info
   }
}

#-------------------------------------------------------------------------------
# Nom      : <Macro::AsProject>
# Creation : Aout 2006 - J.P. Gauthier - CMC/CMOE
#
# But      : Sauvegarder l'etat de l'outils dans un projet SPI.
#
# Parametres :
#   <File>   : Descripteur de fichier ou ecrire les commandes
#
# Remarques :
#    - Le fichier est deja ouvert, il suffit d'y ecrire les commandes a executer
#      afin de re-instaurer l'outils dans son etat actuel.
#
# Modifications :
#
#    Nom         : -
#    Date        : -
#    Description : -
#
#-------------------------------------------------------------------------------

proc Macro::AsProject { File } {
   variable Data
   variable Param

   if { [winfo exists .macro] } {
      puts $File "#----- Tool: Macro\n"
      puts $File "set Macro::Param(Dock)   $Param(Dock)"
      puts $File "set Macro::Param(Geom)   [winfo geometry .macro]"
      puts $File "Macro::Window"
   }

   foreach macro $Data(List) {
      puts $File "Macro::Load $Data(Path$macro)"
   }
   puts $File "Macro::Select $Data(Edit)"
   puts $File "\n"
}
