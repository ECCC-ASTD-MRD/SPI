#===============================================================================
# Environnement Canada
# Centre Meteorologique Canadian
# 2100 Trans-Canadienne
# Dorval, Quebec
#
# Projet   : Librairie de "Widget" Tk.
# Fichier  : Macro.tcl
# Creation : Juillet 2005 - J.P.Gauthier - CMC/CMOE
#
# Description:
#    Interface de gestion de macros.
#
# Remarques :
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
         if { ![Dialog::Default . 300 WARNING $Msg(Save) "\n\n\tMacro::$macro" 0 $Lbl(Yes) $Lbl(No)] } {
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
#   <Extra>  : Texte supplementaire.
#
# Retour    :
#
# Remarque :
#
#-------------------------------------------------------------------------------

proc Macro::Error { Error { Extra "" } } {
   global GDefs

   uplevel 1 "Dialog::Error .macro [list $Error] \"$Extra\""
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
#-------------------------------------------------------------------------------

proc Macro::Doing { Msg } {
   variable Data

   #----- Recuperer le nom de la procedure fautive
   set stacklist [info level -1]
   set stackproc [lindex $stacklist 0]

   if { $Msg!="" } {
      puts stdout "(DOING) ${stackproc}: $Msg"
   }
   if { !$SPI::Param(Batch) } {
      set Data(Job) $Msg
      update idletasks
   }
}

#-------------------------------------------------------------------------------
# Nom      : <Macro::Info>
# Creation : Mai 2005 - J.P. Gauthier - CMC/CMOE
#
# But      : Afficher un message.
#
# Parametres :
#   <Info>   : Message
#   <Extra>  : Texte supplementaire.
#
# Retour    :
#
# Remarque :
#
#-------------------------------------------------------------------------------

proc Macro::Info { Info { Extra "" } } {

   uplevel 1 "Dialog::Info .macro [list $Info] \"$Extra\""
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
#-------------------------------------------------------------------------------

proc Macro::Check { Macro } {
   global GDefs
   variable Data
   variable Lbl
   variable Error

   #----- Check de la validite de la macro
   eval set proc \[info procs ::Macro::${Macro}::Execute\]
   if { $proc=="" } {
      Dialog::Error . $Error(Execute) " (Macro::$Macro)"
      return False
   }

   if { ![info exists ::Macro::${Macro}::Param(Info)] } {
      Dialog::Error . $Error(Info) " (Macro::$Macro)"
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
      catch { Macro::${Data(Current)}::Clean }
   }

   #----- Check that it is still valid
   if { $Data(Time$Macro)<[file mtime $Data(Path$Macro)] } {
      if { ![Dialog::Default . 300 WARNING $Msg(Modified) "\n\n\tMacro::$Macro" 0 $Lbl(Yes) $Lbl(No)] } {
         Macro::Load $Data(Path$Macro)
      } else {
         set Data(Time$Macro) [file mtime $Data(Path$Macro)]
      }
   }

   #----- Evaluate the source code
   if { [namespace exist ::Macro::$Macro] } {
      namespace delete ::Macro::$Macro
   }

   if { [winfo exists $Data(Tab).desc.text.list] } {
      set Data(Code$Macro) [$Data(Tab).desc.text.list get 0.0 end]
      uplevel #0 $Data(Code$Macro)
   }

   Macro::Run $Macro
   Macro::Cursor left_ptr
}

#-------------------------------------------------------------------------------
# Nom      : <Macro::Run>
# Creation : Mai 2005 - J.P. Gauthier - CMC/CMOE
#
# But      : Lancer une macro.
#
# Parametres :
#   <Macro>       : Macro a lancer
#   <Interactive> : Mode interactif
#
# Retour    :
#
# Remarque :
#
#-------------------------------------------------------------------------------

proc Macro::Run { Macro { Interactive True } } {
   global GDefs
   variable Lbl
   variable Msg
   variable Data

   set go 1

   #----- If macro is valid
   if { [Macro::Check $Macro] } {

      #----- Launch the current macro
      set Data(Current) $Macro
      set Data(VP)      $Viewport::Data(VP)
      set Data(Frame)   $Page::Data(Frame)

      #----- If Macro has an Args function, call it to get the args
      eval set proc \[info procs ::Macro::${Macro}::Args\]
      if { $proc!="" } {

         if { $Interactive } {
            set msg $Msg(Args)

            if { [info exists Macro:::${Macro}::Param(InfoArgs)] } {
               eval set args \$Macro:::${Macro}::Param(InfoArgs)
               if { [llength [lindex $args 0]] } {
                  set msg [list "[lindex $Msg(Args) 0]\n\n\t[join [lindex $args 0] \n\t]" "[lindex $Msg(Args) 1]\n\n[join [lindex $args 1] \n\t]"]
               }
            }

            #----- If needed args are entered, proceed
            if { [set go [llength [Dialog::Get .macro $Lbl(Args) $msg ::argv]]] } {
               set ::argc [llength $::argv]
               eval Macro::${Macro}::Args
            }
         } else {
            eval Macro::${Macro}::Args
         }
      }

      if { $go } {
         eval Macro::${Macro}::Execute

         Page::Activate $Data(Frame)
         Viewport::Activate $Data(Frame) $Data(VP)
      } else {
         set Data(Current) ""
      }
   } else {
      set Data(Current) ""
   }
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
#-------------------------------------------------------------------------------

proc Macro::Reset { } {
   variable Data

   Macro::Cursor watch

   #----- Check for previous macro cleanup function
   eval set proc \[info procs ::Macro::${Data(Current)}::Clean\]
   if { $proc!="" } {
      Macro::${Data(Current)}::Clean
   }

#   SPI::LayoutLoad $Data(Frame) SPI
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
#-------------------------------------------------------------------------------

proc Macro::Save { Macro } {
   global GDefs
   variable Lbl
   variable Msg
   variable Data

   if { $Macro!="" } {

      #----- Check that it is still valid
      if { $Data(Time$Macro)<[file mtime $Data(Path$Macro)] } {
         if { [Dialog::Default . 300 WARNING $Msg(Overwrite) "\n\n\tMacro::$Macro" 1 $Lbl(Yes) $Lbl(No)] } {
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
#-------------------------------------------------------------------------------

proc Macro::New { Path } {
   variable Data

   if { $Path!="" } {

      set macro [file tail [file rootname $Path]]

      set f [open $Path w]
      puts $f "namespace eval Macro::${macro} { } {\n   variable Param\n   variable Data\n   variable Error\n\n   set Data(Something)  \"Some data value\"\n\n   set Error(Something) { \"Une erreur quelconque\" \"Some error\" }\n\n   set Param(Info)      { \"Description de la macro\" \"Macro description\" }\n}\n"
      puts $f "proc Macro::${macro}::Execute { } {\n   variable Data\nvariable Param\n\n}\n"
      puts $f "proc Macro::${macro}::Clean { } {\n   variable Data\nvariable Param\n\n}\n"

      close $f

      Macro::Load $Path
   }
}

#-------------------------------------------------------------------------------
# Nom      : <Macro::Delete>
# Creation : Mai 2008 - J.P. Gauthier - CMC/CMOE
#
# But      : Supprimer une macro.
#
# Parametres :
#   <Macro>  : Macro a supprimer
#
# Retour    :
#
# Remarque :
#
#-------------------------------------------------------------------------------

proc Macro::Delete { Macro } {
   global GDefs
   variable Data
   variable Msg
   variable Lbl

   if { ![Dialog::Default . 300 WARNING $Msg(Delete) "\n\n\tMacro::$Macro" 0 $Lbl(Yes) $Lbl(No)] } {
      file delete -force $Data(Path$Macro)

      Macro::UnDefine $Macro
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
#-------------------------------------------------------------------------------

proc Macro::Bubble { Index } {
   global GDefs
   variable Data
   variable Lbl

   set macro [lindex $Data(List) $Index]
   if { [info exists ::Macro::${macro}::Param(Info)] } {
      eval set info \[lindex \$Macro::${macro}::Param(Info) \$GDefs(Lang)\]

      if { [info exists Macro:::${macro}::Param(InfoArgs)] } {
         eval set args \$Macro:::${macro}::Param(InfoArgs)
         if { [llength [lindex $args $GDefs(Lang)]] } {
            append info "\n\n[lindex $Lbl(Args) $GDefs(Lang)]:\n\t[join [lindex $args $GDefs(Lang)] \n\t]"
         }
      }
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
