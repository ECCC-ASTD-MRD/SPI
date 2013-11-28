#===============================================================================
# Environnement Canada
# Centre Meteorologique Canadian
# 2100 Trans-Canadienne
# Dorval, Quebec
#
# Projet   : SPI (Tools)
# Fichier  : VASIGMET.tcl
# Creation : Septembre 2001
#
# Description:
#    Redaction de message VASIGMET
#
#===============================================================================

namespace eval Writer::VASIGMET {
   variable Param
   variable Data

   set Data(Seconds) [clock seconds]

   set Param(Mail)    { }
   set Param(CCMail)  { }
}

#----------------------------------------------------------------------------
# Nom      : <Writer::VASIGMET::New>
# Creation : Septembre 2001 - J.P. Gauthier - CMC/CMOE
#
# But      : Creer un nouveau message
#
# Parametres :
#    <Pad>   : Descripteur du Pad
#
# Remarques :
#
#----------------------------------------------------------------------------

proc Writer::VASIGMET::New { Pad Mode } {
   variable Data

   Writer::VASIGMET::Init $Pad
   Writer::VASIGMET::PageInit $Pad
}

#----------------------------------------------------------------------------
# Nom      : <Writer::VASIGMET::Init>
# Creation : Septembre 2001 - J.P. Gauthier - CMC/CMOE
#
# But      : Vider toute l'information de l'interface
#
# Parametres :
#    <Pad>   : Descripteur du Pad
#
# Remarques :
#
#----------------------------------------------------------------------------

proc Writer::VASIGMET::Init { Pad } {
   variable Data

  #----- Definitions des variables internes

   set Data(File$Pad)    VASIGMET
   set Data(Handle$Pad)  YES
   set Data(Secs$Pad)    {}
   set Data(Sec$Pad)     0
}

#----------------------------------------------------------------------------
# Nom      : <Writer::VASIGMET::Open>
# Creation : Septembre 2001 - J.P. Gauthier - CMC/CMOE
#
# But      : Consulter un message
#
# Parametres :
#  <Pad>     : Identificateur du Pad
#  <File>    : Path complet du fichier
#
# Remarques :
#    Aucune.
#
#----------------------------------------------------------------------------

proc Writer::VASIGMET::Open { Pad File } {
   variable Data
   variable Msg
   variable Lbl

   if { [string length $File] == 0 } {
      return 0
   }

   Writer::VASIGMET::Init $Pad

   set Data(File$Pad)    [file tail $File]

   Writer::PadName $Pad "REA [string range $Data(File$Pad) 0 11]..."

   Writer::VASIGMET::Read $Pad $File
   Writer::VASIGMET::PageInit $Pad
}

#----------------------------------------------------------------------------
# Nom      : <Writer::VASIGMET::Update>
# Creation : Septembre 2001 - J.P. Gauthier - CMC/CMOE
#
# But      : Mise-a-jour d'un message
#
# Parametres :
#  <Pad>     : Identificateur du Pad
#
# Remarques :
#    Aucune.
#
#----------------------------------------------------------------------------

proc Writer::VASIGMET::Update { Pad } {
   variable Data
   variable Msg

}

#----------------------------------------------------------------------------
# Nom      : <Writer::VASIGMET::Format>
# Creation : Septembre 2001 - J.P. Gauthier - CMC/CMOE
#
# But      : Formatte le message dans un fichier texte
#
# Parametres :
#   <Pad>    : Identificateur du Pad
#
# Retour     :
#   <File>   : Fichier temporaire
#
# Remarques :
#    Aucune.
#
#----------------------------------------------------------------------------

proc Writer::VASIGMET::Format { Pad } {
   global   env
   variable Data

   set file $env(HOME)/.spi/Tmp/VASIGMET[pid]_[clock seconds].txt

   set f [open $file w]
   puts $f [$Pad.body get 0.0 end]
   close $f

   return $file
}

#----------------------------------------------------------------------------
# Nom      : <Writer::VASIGMET::FormatCoord>
# Creation : Sepetmbre 2001 - J.P.Gauthier - CMC/CMOE
#
# But      : Formater les coordonnes
#
# Parametres :
#    <Lat>   : Latitude
#    <Lon>   : Longitude
#
# Remarques :
#    Aucune.
#
#----------------------------------------------------------------------------

proc Writer::VASIGMET::FormatCoord { Lat Lon } {

   if { $Lat < 0 } {
      set Lat [expr -$Lat]
      set dir "S"
   } else {
      set dir "N"
   }

   set latm "[Convert::Decimal2Minute $Lat -0.5 True] $dir"
   set latm "[format "%02i" [lindex $latm 0]][lindex $latm 1]$dir"

   if { $Lon < 0 } {
      set Lon [expr -$Lon]
      set dir "W"
   } else {
      set dir "E"
   }
   set lonm "[Convert::Decimal2Minute $Lon -0.5 True] $dir"
   set lonm "[format "%03i" [lindex $lonm 0]][lindex $lonm 1]$dir"

   return "$latm$lonm"
}

#----------------------------------------------------------------------------
# Nom      : <Writer::VASIGMET::Clear>
# Creation : Septembre 2001 - J.P. Gauthier - CMC/CMOE
#
# But      : Supprimer tout ce qui a trait au "layout" precedent
#
# Parametres :
#  <Pad>    : Identificateur du Pad
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc Writer::VASIGMET::Clear { Pad } {
   variable Data

   destroy $Pad.body $Pad.vasigmet
}

#----------------------------------------------------------------------------
# Nom      : <Writer::VASIGMET::LayoutInit>
# Creation : Octobre 2001 - J.P. Gauthier - CMC/CMOE
#
# But      : Effectue l'affichage des parties fixe de la carte.
#
# Parametres :
#  <Pad>     : Identificateur du pad
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc Writer::VASIGMET::LayoutInit { Pad } {
   variable Data
   variable Bubble

   destroy $Pad.canvas

   label $Pad.vasigmet

   text $Pad.body -bg gray75 -height 40 -width 68 -font XFont12 -bd 0 -wrap word
   pack $Pad.body -side top -fill both -expand True
}

#----------------------------------------------------------------------------
# Nom      : <Writer::VASIGMET::PrintCommand>
# Creation : Mars 1999 - J.P. Gauthier - CMC/CMOE
#
# But      : Creer la commande d'impression "plugin" pour les cas RSMC a l'interieur
#            de la PrintBox.
#
# Parametres :
#  <Pad>     : Identificateur du Pad
#
# Retour:
#
# Remarques :
#   Cette fonction est appelee par PrintBox pour effectuer l'impression selon les
#   parametres du PrintBox lui-meme et du "Widget PlugIn" PrintBox::PlugInCommand
#   definit ci-haut.
#
#----------------------------------------------------------------------------

proc Writer::VASIGMET::PrintCommand { Pad } {
   variable Data

   set file [Writer::VASIGMET::Format $Pad]
   set PrintBox::Param(FullName) [string trimright $PrintBox::Param(FullName) ".$PrintBox::Print(Device)"]

   PrintBox::PrintTXT $file
   PrintBox::Destroy
}

#----------------------------------------------------------------------------
# Nom      : <Writer::VASIGMET::PageInit>
# Creation : Septembre 1999 - J.P. Gauthier - CMC/CMOE
#
# But      : Effectue l'affichage des parties fixe du resume.
#
# Parametres :
#  <Canvas>  : Identificateur du canvas
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc Writer::VASIGMET::PageInit { Pad } {
   variable Data

}

#----------------------------------------------------------------------------
# Nom      : <Writer::VASIGMET::Read>
# Creation : Septembre 2001 - J.P. Gauthier - CMC/CMOE
#
# But      : Lire le contenu d'un message
#
# Parametres :
#  <Pad>     : Identificateur du Pad
#  <File>    : Path complet du fichier
#
# Retour    :
#
# Remarques :
#
#----------------------------------------------------------------------------

proc Writer::VASIGMET::Read { Pad File} {
   variable Data

   set f [open $File r]

   gets $f Data(Secs$Pad)

   foreach sec $Data(Secs$Pad) {
      gets $f Data(Area$Pad$sec)
   }

   close $f
}

#-------------------------------------------------------------------------------
# Nom      : <Writer::VASIGMET::Send>
# Creation : Septembre 2001 - J.P. Gauthier - CMC/CMOE
#
# But      : Transmettre le message.
#
# Parametres :
#   <Pad>    : Identificateur du Pad
#   <Backup> : Methode de backup
#
# Remarques :
#   Aucune.
#
#-------------------------------------------------------------------------------

proc Writer::VASIGMET::Send { Pad { Backup 0 } } {
   variable Param

   #----- Sauvegarder le message

   set name [Writer::VASIGMET::Write $Pad 1]
   set file [Writer::VASIGMET::Format $Pad]

   #----- Transmettre le message avec le script operationnel.

   exec chmod 644 $file

   set err [catch { exec cat ${file} | mail -s "VA SIGMET List of coordinates" -c $Param(CCMail) $Param(Mail) } msg]
   if { $err } {
      Log::Print ERROR "Unable to send $file via mail.\n\n$msg"
   }
   file delete -force $file
}

#----------------------------------------------------------------------------
# Nom      : <Writer::VASIGMET::UpdateItems>
# Creation : Septembre 2001 - J.P. Gauthier - CMC/CMOE
#
# But      :
#
# Parametres :
#  <Frame>   : Identificateur de page#  <Pad>     : Identificateur du pad
#  <VP>      : Identificateur du viewport
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc Writer::VASIGMET::UpdateItems { Frame { VP "" } { Pad "" } } {
   variable Data

   set field [lindex [Viewport::Assigned $Frame $VP fstdfield] 0]

   set p $Writer::Data(Pad)

   if { [fstdfield is $field] } {
      set s [fstdstamp toseconds [fstdfield define $field -DATEV]]
      if { [info exists ::Writer::VASIGMET::Data(Area$p$s)] } {
         if { $s!=$Data(Sec$p) } {
            set Data(Sec$p) $s
            if { [winfo exists $Writer::Data(Canvas)] } {
               $Writer::Data(Canvas) delete VERTEXFOLLOW VASIGMET
            }
         }
      }
   } else {
      set Data(Sec$p) 0
   }
}

#----------------------------------------------------------------------------
# Nom      : <Writer::VASIGMET::ToolBar>
# Creation : Fevrier 2006 - J.P. Gauthier - CMC/CMOE
#
# But      : Installer les outils selon le type de message
#
# Parametres :
#  <Pad>     : Frame ou integrer les fonctions
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc Writer::VASIGMET::ToolBar { Pad } {
   global GDefs

   checkbutton $Pad.head.mode -variable Page::Data(ToolMode) -onvalue Writer::VASIGMET -offvalue SPI \
      -image ARROW -indicatoron 0 -relief sunken -bd 1 -overrelief raised -offrelief flat -selectcolor $GDefs(ColorFrame) \
      -command { SPI::ToolMode $Page::Data(ToolMode) Draw True }
   button $Pad.head.save -image OPEN -bd 0 -relief flat -overrelief raised \
      -command "Writer::VASIGMAT::Write $Pad 0"
   button $Pad.head.print -image PRINT -bd 0 -relief flat -overrelief raised \
      -command "PrintBox::Create $Pad PRINT Writer::VASIGMET"
   button $Pad.head.send -image ENVELOPE -bd 0 -relief flat -overrelief raised \
      -command "Writer::Send"
   button $Pad.head.close -image DELETE -bd 0 -relief flat -overrelief raised \
      -command "Writer::PadClose $Pad 1"
   pack $Pad.head.mode $Pad.head.save $Pad.head.print $Pad.head.send -side left -padx 2
   pack $Pad.head.close -side right -padx 2

   Bubble::Create $Pad.head.save  $Writer::Bubble(Save)
   Bubble::Create $Pad.head.print $Writer::Bubble(Print)
   Bubble::Create $Pad.head.send  $Writer::Bubble(Send)
   Bubble::Create $Pad.head.send2 $Writer::Bubble(SendBackup)
   Bubble::Create $Pad.head.close $Writer::Bubble(Close)
}

#----------------------------------------------------------------------------
# Nom      : <Writer::VASIGMET::Write>
# Creation : Septembre 2001 - J.P. Gauthier - CMC/CMOE
#
# But      : Ecrire le message dans un fichier selon un format specifique.
#
# Parametres :
#  <Pad>     : Identificateur du Pad
#  <Sent>    : Message envoye (-1 = autosave mode).
#
# Retour    :
#   <File>  : Nom du fichier
#
# Remarques :
#    - noter que le format est strict, ceci de facon a relire
#      le message correctement.
#
#    - traiter le cas ou l'usager entre un delimitateur... ou
#      simplement changer de delimitateur.
#      On peut simplement illiminer les lignes des textes qui
#      contiennent des points (.).
#
#----------------------------------------------------------------------------

proc Writer::VASIGMET::Write { Pad Sent } {
   global GDefs
   variable Data

   if { $Sent==-1 } {
      set file $Writer::Data(AutoSaveFile)
   } else {

      #----- Determine le nom du fichier.
      set date [clock format $Data(Seconds) -format "%Y%m%d-%H%MZ" -gmt True]

      set file ${date}.msg

      if { $Sent } {
         set file $file.sent
      }

      if { [file exists $Writer::Param(Path)/VASIGMET/$file] } {
         if { ![Dialog::Default .writer 300 WARNING $Writer::Msg(Exist) "\n\t$file\n" 0 $Writer::Lbl(No) $Writer::Lbl(Yes)] } {
            return
         }
      }
   }

   set f [open $Writer::Param(Path)/VASIGMET/$file w 0660]

   puts $f "$Data(Secs$Pad)"
   foreach sec $Data(Secs$Pad) {
      puts $f "$Data(Area$Pad$sec)"
   }

   close $f

   return $file
}

#----------------------------------------------------------------------------
# Nom      : <Writer::VASIGMET::AshUpdate>
# Creation : Septembre 2001 - J.P. Gauthier - CMC/CMOE
#
# But      : Mise a jour des regions de cendres pour une heure specifie
#            dans la description textuelle.
#
# Parametres :
#  <Pad>     : Identificateur du Pad
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc Writer::VASIGMET::AshUpdate { Pad } {
   variable Data

   set text ""

   foreach sec $Data(Secs$Pad) {
      if { [llength  $Data(Area$Pad$sec)] } {
         #----- Insert date
         append text "\nValid date-time: [clock format $sec -format "%d %b %Y, %H%M UTC" -timezone :UTC]\n\n"

         #----- Insert coordinates
         foreach { lat lon elev } $Data(Area$Pad$sec) {
            append text [Writer::VASIGMET::FormatCoord $lat $lon]\n
         }

         #----- Repeat first point
         append text [Writer::VASIGMET::FormatCoord [lindex $Data(Area$Pad$sec) 0] [lindex $Data(Area$Pad$sec) 1]]\n
      }
   }

   set Data(Text$Pad) $text
   $Pad.body delete 0.0 end
   $Pad.body insert 0.0 $text

   Writer::VASIGMET::PageInit $Pad
}

#----------------------------------------------------------------------------
# Nom      : <Writer::VASIGMET::VertexAdd>
# Creation : Septembvre 2001 - J.P. Gauthier - CMC/CMOE
#
# But      : Ajout d'un point au polygone.
#
# Parametres :
#  <Frame>   : Identificateur de Page
#  <VP>      : Identificateur du Viewport
#  <X>       : Coordonnee X de la souris
#  <Y>       : Coordonnee Y de la souris
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc Writer::VASIGMET::VertexAdd { Frame VP X Y } {
   variable Data

   if { $VP==-1 } {
      return
   }

   if { [winfo exists $Writer::Data(Canvas)] } {
      $Writer::Data(Canvas) delete VASIGMET
   }

   set field [lindex [Viewport::Assigned $Frame $VP fstdfield] 0]

   if { [fstdfield is $field] } {

      set loc [$VP -unproject $X $Y]
      set X   [lindex $loc 0]
      set Y   [lindex $loc 1]

      #----- si le vertex est valide on l'ajoute a la liste

      if { $X!=-999 && $Y!=-999 } {

         set p $Writer::Data(Pad)
         set s [fstdstamp toseconds [fstdfield define $field -DATEV]]

         set Data(Sec$p)    $s
         if { [lsearch -exact $Data(Secs$p) $s]==-1 } {
            lappend Data(Secs$p) $s
            set Data(Secs$p) [lsort -increasing -unique $Data(Secs$p)]
         }

         lappend Data(Area$p$s) $X $Y 0

         set Writer::Data(Canvas) $Page::Data(Canvas)
         set Writer::Data(Frame)  $Frame
         set Writer::Data(VP)     $VP

         Writer::VASIGMET::AshUpdate   $p
         Writer::VASIGMET::UpdateItems $Frame $VP $p
      }
   }
}

#----------------------------------------------------------------------------
# Nom      : <Writer::VASIGMET::VertexDelete>
# Creation : Septembvre 2001 - J.P. Gauthier - CMC/CMOE
#
# But      : Suppression d'un point au polygone.
#
# Parametres :
#  <Frame>   : Identificateur de Page
#  <VP>      : Identificateur du Viewport
#  <X>       : Coordonnee X de la souris
#  <Y>       : Coordonnee Y de la souris
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc Writer::VASIGMET::VertexDelete { Frame VP } {
   variable Data

   set field [lindex [Viewport::Assigned $Frame $VP fstdfield] 0]

   if { [winfo exists $Writer::Data(Canvas)] } {
      $Writer::Data(Canvas) delete VASIGMET
   }

   if { [fstdfield is $field] } {

      set p $Writer::Data(Pad)
      set s $Data(Sec$p)

      if { [info exists ::Writer::VASIGMET::Data(Area$p$s)] } {
         set Data(Area$p$s) [lreplace $Data(Area$p$s) end-2 end]

         set Writer::Data(Canvas) $Page::Data(Canvas)
         set Writer::Data(Frame)  $Frame
         set Writer::Data(VP)     $VP

         Writer::VASIGMET::AshUpdate   $p
         Writer::VASIGMET::UpdateItems $Frame $VP $p
      }
   }
}

#----------------------------------------------------------------------------
# Nom      : <Writer::VASIGMET::VertexFollow>
# Creation : Septembvre 2001 - J.P. Gauthier - CMC/CMOE
#
# But      : Affiche une ligne entre le dernier vertex creer et la position du
#            curseur de la souris.
#
# Parametres :
#  <Frame>   : Identificateur de Page
#  <VP>      : Identificateur du Viewport
#  <X>       : Coordonnee X de la souris
#  <Y>       : Coordonnee Y de la souris
#  <Scan>    : Mode Scan
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc Writer::VASIGMET::VertexFollow { Frame VP X Y Scan } {
   global   GDefs
   variable Data

   if { $VP==-1 } {
      return
   }

   if { [winfo exists $Writer::Data(Canvas)] } {
      $Writer::Data(Canvas) delete VERTEXFOLLOW
   }

   set field [lindex [Viewport::Assigned $Frame $VP fstdfield] 0]

   if { [fstdfield is $field] } {

      set loc [$VP -unproject $X $Y]
      set X [lindex $loc 0]
      set Y [lindex $loc 1]

      if { $X!=-999 && $Y!=-999 } {

         set p $Writer::Data(Pad)
         set s $Data(Sec$p)

         set Writer::Data(Canvas) $Page::Data(Canvas)
         set Writer::Data(Frame)  $Frame
         set Writer::Data(VP)     $VP

         if { [info exists ::Writer::VASIGMET::Data(Area$p$s)] } {
            set tmp $Data(Area$p$s)

            if { [llength $tmp] } {
               lappend tmp $X $Y 0 [lindex $tmp 0] [lindex $tmp 1] 0

               Viewport::DrawArea $Frame $VP $tmp "PAGE$VP VASIGMET VERTEXFOLLOW" VERTEXFOLLOW red red\
                  @$GDefs(Dir)/share/bitmap/raydiagleft08.xbm 0 2
            }
         }
      }
   }
}
