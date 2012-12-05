#===============================================================================
# Environnement Canada
# Centre Meteorologique Canadian
# 2100 Trans-Canadienne
# Dorval, Quebec
#
# Projet   : SPI (Tools)
# Fichier  : AACN.tcl
# Creation : Septembre 2001
#
# Description:
#    Redaction de message AACN
#
#===============================================================================

namespace eval Writer::AACN {
   variable Data

   #----- Chaines standard

   set Data(Top)   "WE HAVE BEEN NOTIFIED OF AN EMERGENCY AT THE NUCLEAR POWER PLANT IN"
   set Data(Time)  "STARTING AT"
   set Data(Body)  "THE CANADIAN FEDERAL NUCLEAR EMERGENCY PLAN IS BEING ACTIVATED. A TRAJECTORY FORECAST CHART HAS BEEN TRANSMITTED ON SATNET AND AWDS WITH HEADER A0948C GLB TRA P SPECIAL TRAJECTORY FCST. LOW LEVEL TRAJECTORIES INDICATE THAT ANY RADIOACTIVE MATERIAL RELEASED AT THE START TIME OF THE EMERGENCY WOULD BE MOVING EAST. ADDITIONAL INFORMATION WILL BE PROVIDED WHEN AVAILABLE. SHIFT SUPERVISOR CANADIAN METEOROLOGICAL CENTRE"

   set Data(Seconds) [clock seconds]
}

#----------------------------------------------------------------------------
# Nom      : <Writer::AACN::New>
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

proc Writer::AACN::New { Pad Mode } {
   variable Data

   Writer::AACN::Init $Pad
   Writer::AACN::Site "" UNKNOWN "" "" "" ""

   $Pad.we insert 0 "$Data(Top)"
   $Pad.body insert 0.0 "$Data(Body)"

   Writer::AACN::PageInit $Pad
}

#----------------------------------------------------------------------------
# Nom      : <Writer::AACN::Init>
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

proc Writer::AACN::Init { Pad } {
   variable Data

  #----- Definitions des variables internes

   set Data(No$Pad)       AACN11
   set Data(Id$Pad)       CWAO
   set Data(Cor$Pad)      ""

   set Data(Seconds)     [clock seconds]
   set Data(Time$Pad)    "$Data(Time) [string toupper [clock format $Data(Seconds) -format "%H%M UTC %d %B %Y"]]"
   set Data(Date$Pad)    [clock format $Data(Seconds) -format "%d%H%M" -gmt True]
   set Data(Site$Pad)    "UNKNOWN (00.00N 000.00W DECIMAL) (0000E 00000W DEGREES, MINUTES)"

   set Data(File$Pad)    AACN
   set Data(Handle$Pad)  YES
}

#----------------------------------------------------------------------------
# Nom      : <Writer::AACN::Open>
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

proc Writer::AACN::Open { Pad File } {
   variable Data
   variable Msg
   variable Lbl

   if { [string length $File] == 0 } {
      return 0
   }

   Writer::AACN::Init $Pad

   set Data(Handle$Pad)  YES
   set Data(File$Pad)    [file tail $File]

   Writer::PadName $Pad "REA [string range $Data(File$Pad) 0 11]..."

   Writer::AACN::Read $Pad $File
   Writer::AACN::PageInit $Pad
}

#----------------------------------------------------------------------------
# Nom      : <Writer::AACN::Correct>
# Creation : Octobre 2001 - J.P. Gauthier - CMC/CMOE
#
# But      : Correction d'un message.
#
# Parametres :
#  <Pad>     : Identificateur du Pad
#
# Remarques :
#    Aucune.
#
#----------------------------------------------------------------------------

proc Writer::AACN::Correct { Pad } {
   global   GDefs
   variable Data
   variable Msg
   variable Lbl

   if { $Pad=="" } {
      return
   }

   #----- On ne permet pas la correction d'un nouveau message

   if { $Data(File$Pad)=="AACN" } {
      Dialog::Error . $Writer::Msg(NEW)
      return
   }

   set Data(Cor$Pad)    "COR"
   set Data(Handle$Pad) "YES"
   set Data(Date$Pad)    [clock format $Data(Seconds) -format "%d%H%M" -gmt True]

   Writer::PadName $Pad "COR [string range $Data(File$Pad) 0 11]..."
   Writer::AACN::PageInit $Pad
}

#----------------------------------------------------------------------------
# Nom      : <Writer::FVCN::Update>
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

proc Writer::AACN::Update { Pad } {
   variable Data
   variable Msg

}

#----------------------------------------------------------------------------
# Nom      : <Writer::AACN::Format>
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

proc Writer::AACN::Format { Pad } {
   global   env
   variable Data

   set file $env(HOME)/.spi/Tmp/AACN[pid]_[clock seconds].txt

   set f [open $file w]

   #----- Header

   puts $f "$Data(No$Pad) $Data(Id$Pad) $Data(Date$Pad) $Data(Cor$Pad)"

   #----- Body

   set msg "[$Pad.we get] $Data(Site$Pad) $Data(Time$Pad)[$Pad.body get 0.0 end]"

   foreach line [Writer::TextExtract word 69  $msg] {
      puts $f $line
   }

   close $f

   return $file
}

#----------------------------------------------------------------------------
# Nom      : <Writer::AACN::FormatCoord>
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

proc Writer::AACN::FormatCoord { Lat Lon } {

   if { $Lat < 0 } {
      set Lat [expr -$Lat]
      set dir "S"
   } else {
      set dir "N"
   }

   set latm "[Convert::Decimal2Minute $Lat 0 True] $dir"
   set latm "[format "%02i" [lindex $latm 0]][lindex $latm 1]$dir"
   set latd "[format "%6.2f" $Lat]$dir"

   if { $Lon < 0 } {
      set Lon [expr -$Lon]
      set dir "W"
   } else {
      set dir "E"
   }
   set lonm "[Convert::Decimal2Minute $Lon 0 True] $dir"
   set lonm "[format "%03i" [lindex $lonm 0]][lindex $lonm 1]$dir"
   set lond "[format "%7.2f" $Lon]$dir"

   return "($latd $lond DECIMAL) ($latm $lonm DEGREES,MINUTES)"
}

#----------------------------------------------------------------------------
# Nom      : <Writer::AACN::Clear>
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

proc Writer::AACN::Clear { Pad } {
   variable Data

   trace vdelete SPI::Src(Info) w { Writer::AACN::Source }

   destroy $Pad.aacn $Pad.site $Pad.body $Pad.we $Pad.site $Pad.time $Pad.no $Pad.date $Pad.corid $Pad.optsite $Pad.optsite.menu
}

#----------------------------------------------------------------------------
# Nom      : <Writer::AACN::LayoutInit>
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

proc Writer::AACN::LayoutInit { Pad } {
   variable Data
   variable Bubble

   label $Pad.aacn

   label $Pad.no     -bg white -font XFont12 -bd 0 -anchor w -textvariable Writer::AACN::Data(No$Pad)
   label $Pad.date   -bg white -font XFont12 -bd 0 -anchor w -textvariable Writer::AACN::Data(Date$Pad)
   label $Pad.corid  -bg white -font XFont12 -bd 0 -anchor w -textvariable Writer::AACN::Data(Cor$Pad)

   entry $Pad.we   -bg gray75 -width 68 -font XFont12 -bd 0
   entry $Pad.site -bg gray75 -width 68 -font XFont12 -bd 0 -textvariable Writer::AACN::Data(Site$Pad)
   entry $Pad.time -bg gray75 -width 68 -font XFont12 -bd 0 -textvariable Writer::AACN::Data(Time$Pad)
   text  $Pad.body -bg gray75 -height 20 -width 68 -font XFont12 -bd 0 -wrap word

   #----- Menu d'options

   menubutton $Pad.optsite -bg white -bd 0 -menu $Pad.optsite.menu -image opt -width $Writer::Data(Height) -height $Writer::Data(Height)
   menu $Pad.optsite.menu -bd 1 -tearoff 0 -activeborderwidth 0 -bg white
      $Pad.optsite.menu add command -label Select -command "Locator::Window 1"
      $Pad.optsite.menu add separator
      $Pad.optsite.menu add command -label UNKNOWN -command { Writer::AACN::Site "" UNKNOWN "" "" "" "" }
}

#----------------------------------------------------------------------------
# Nom      : <Writer::AACN::PrintCommand>
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

proc Writer::AACN::PrintCommand { Pad } {
   variable Data

   set file [Writer::AACN::Format $Pad]
   set PrintBox::Param(FullName) [string trimright $PrintBox::Param(FullName) ".$PrintBox::Print(Device)"]

   PrintBox::PrintTXT $file
   PrintBox::Destroy
}

#----------------------------------------------------------------------------
# Nom      : <Writer::AACN::PageInit>
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

proc Writer::AACN::PageInit { Pad } {
   variable Data

   $Pad.canvas delete HEADER BODY

   #----- Numero de message

   set y 4
   set x [expr 5+$Writer::Data(Width)]

   $Pad.canvas create window $x 4 -anchor nw -tags HEADER -window $Pad.no
   $Pad.canvas create text [expr 5+$Writer::Data(Width)*8] $y -anchor nw -font XFont12 -tags HEADER -text "$Data(Id$Pad)"
   $Pad.canvas create window [expr 5+$Writer::Data(Width)*13] 4 -anchor nw -tags HEADER -window $Pad.date
   $Pad.canvas create window [expr 5+$Writer::Data(Width)*20] 4 -anchor nw -tags HEADER -window $Pad.corid
   incr y $Writer::Data(Height)

   #----- Body

   $Pad.canvas create window $x $y -anchor nw -tags BODY -window $Pad.we
   incr y $Writer::Data(Height)
   $Pad.canvas create window $x $y -anchor nw -tags BODY -window $Pad.site
   $Pad.canvas create window $x $y -anchor ne -tags BODY -window $Pad.optsite
   incr y $Writer::Data(Height)
   $Pad.canvas create window $x $y -anchor nw -tags BODY -window $Pad.time
   incr y $Writer::Data(Height)
   $Pad.canvas create window $x $y -anchor nw -tags BODY -window $Pad.body
}

#----------------------------------------------------------------------------
# Nom      : <Writer::AACN::Read>
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

proc Writer::AACN::Read { Pad File} {
   variable Data

   set f [open $File r]

   gets $f Data(No$Pad)
   gets $f Data(Id$Pad)
   gets $f Data(Date$Pad)
   gets $f Data(Cor$Pad)
   gets $f line ; $Pad.we insert 0 $line
   gets $f Data(Site$Pad)
   gets $f Data(Time$Pad)
   gets $f line ; $Pad.body insert 0.0 $line

   close $f
}

#-------------------------------------------------------------------------------
# Nom      : <Writer::AACN::Send>
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

proc Writer::AACN::Send { Pad { Backup 0} } {
   global   GDefs
   variable Data

   #----- Sauvegarder le message

   set name [Writer::AACN::Write $Pad 1]
   set file [Writer::AACN::Format $Pad]

   #----- Transmettre le message avec le script operationnel.

   exec chmod 644 $file

   if { $Backup } {
      set ErrCatch [catch  { exec $GDefs(Dir)/Script/CMOP_amxmit.ksh $file $GDefs(TransmitUser) opserv ncp1 } MsgCatch]
      if { $ErrCatch != 0 } {
         Log::Print ERROR "Unable to sent the $file via metmanager on opserv.\n\n$MsgCatch"
      }

   } else {
      set ErrCatch [catch  { exec /users/dor/afse/eer/script/CMOI_nanproc.ksh ${file} $GDefs(TransmitUser) $GDefs(TransmitHost) } MsgCatch ]
      if { $ErrCatch != 0 } {
         Log::Print ERROR "Unable to sent the $file via nanproc on $GDefs(TransmitHost).\n\n$MsgCatch"
      }
   }
   file delete -force $file
}

#----------------------------------------------------------------------------
# Nom      : <Writer::AACN::Source>
# Creation : Septembre 2001 - J.P. Gauthier - CMC/CMOE
#
# But      : Recuperer les informations d'une source et initialiser les
#            parametres qui en depande.
#
# Parametres :
#  <Array>   : Variable array
#  <Index>   : Index dans la variable Array
#  <Op>      : Operation effectuer sur la variable
#
# Retour:
#
# Remarques :
#   -Cette procedure repond a un "trace" sur une variable a l'interieur de SPI
#    afin de recuperer l'information necessaire automatiquement (SPI::Src(Info))
#
#----------------------------------------------------------------------------

proc Writer::AACN::Source { Array Index Op } {

   Writer::AACN::Site $SPI::Src(No) "[string toupper $SPI::Src(Name)]" $SPI::Src(Lat) $SPI::Src(Lon) $SPI::Src(Elev) "[string toupper $SPI::Src(Area)]"
}

#----------------------------------------------------------------------------
# Nom      : <Writer::AACN::Site>
# Creation : Septembre 2001 - J.P. Gauthier - CMC/CMOE
#
# But      : format le retour de la sous-routine de selection de source.
#
# Parametres :
#    Aucun.
#
# Remarques :
#    - le nom et la sous-region du volcan contenu dans la ligne
#      qui est retournee par le selecteur est encoder comme suit :
#      tout les blancs ont ete substituer par des underscores (_).
#
#----------------------------------------------------------------------------

proc Writer::AACN::Site { No Name Lat Lon Elev Area } {
   variable Data

   if { $Writer::Data(Type$Writer::Data(Pad))!="AACN" } {
      return
   }
   if  { $Name!="UNKNOWN" } {
      set Data(Site$Writer::Data(Pad))  "$Name, $Area [Writer::AACN::FormatCoord $Lat $Lon]"
   } else {
      set Data(Site$Writer::Data(Pad))  "UNKNOWN (00.00N 000.00W DECIMAL) (0000E 00000W DEGREES, MINUTES)"
   }
}

#----------------------------------------------------------------------------
# Nom      : <Writer::AACN::UpdateItems>
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

proc Writer::AACN::UpdateItems { Frame { VP "" } { Pad "" } } {

}

#----------------------------------------------------------------------------
# Nom      : <Writer::AACN::ToolBar>
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

proc Writer::AACN::ToolBar { Pad } {
   global GDefs

   button $Pad.head.save -image OPEN -bd 0 -relief flat -overrelief raised \
      -command "Writer::AACN::Write $Pad 0"
   button $Pad.head.print -image PRINT -bd 0 -relief flat -overrelief raised \
      -command "PrintBox::Create $Pad PRINT Writer::AACN"
   button $Pad.head.send -image ENVELOPE -bd 0 -relief flat -overrelief raised \
      -command "Writer::Send"
   button $Pad.head.send2 -image ENVELOPE2 -bd 0 -relief flat -overrelief raised \
      -command "Writer::Send 1"
   button $Pad.head.close -image DELETE -bd 0 -relief flat -overrelief raised \
      -command "Writer::PadClose $Pad 1"
   pack $Pad.head.save $Pad.head.print $Pad.head.send $Pad.head.send2 -side left -padx 2
   pack $Pad.head.close -side right -padx 2

   Bubble::Create $Pad.head.save  $Writer::Bubble(Save)
   Bubble::Create $Pad.head.print $Writer::Bubble(Print)
   Bubble::Create $Pad.head.send  $Writer::Bubble(Send)
   Bubble::Create $Pad.head.send2 $Writer::Bubble(SendBackup)
   Bubble::Create $Pad.head.close $Writer::Bubble(Close)
}
#----------------------------------------------------------------------------
# Nom      : <Writer::AACN::Write>
# Creation : Septembre 2001 - J.P. Gauthier - CMC/CMOE
#
# But      : Ecrire le messgae dans un fichier selon un format specifique.
#
# Parametres :
#  <Pad>     : Identificateur du PAd
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

proc Writer::AACN::Write { Pad Sent } {
   global GDefs
   variable Data

   if { $Sent==-1 } {
      set file $Writer::Data(AutoSaveFile)
   } else {
      #----- On s'assure que le nom du fichier soit qu'une seul chaine.

      regsub -all " " [lindex $Data(Site$Pad) 0] - file
      regsub -all "/." $file "" file

      #----- Determine le nom du fichier.

      set date [clock format $Data(Seconds) -format "%Y%m%d-%H%MZ" -gmt True]

      if { !$Sent } {
         set file $Data(No$Pad)_${file}.msg
      } else {
         set file ${date}_$Data(No$Pad)_${file}.msg
      }

      if { $Data(Cor$Pad)!="" } {
         set file $file.cor
      }

      if { $Sent } {
         set file $file.sent
      }

      if { [file exists $Writer::Param(Path)/AACN/$file] } {
         if { ![Dialog::Default .writer 300 WARNING $Writer::Msg(Exist) "\n\t$file\n" 0 $Writer::Lbl(No) $Writer::Lbl(Yes)] } {
            return
         }
      }
   }

   set f [open $Writer::Param(Path)/AACN/$file w 0660]

   puts $f "$Data(No$Pad)"
   puts $f "$Data(Id$Pad)"
   puts $f "$Data(Date$Pad)"
   puts $f "$Data(Cor$Pad)"
   puts $f "[$Pad.we get]"
   puts $f "$Data(Site$Pad)"
   puts $f "$Data(Time$Pad)"
   puts $f "[Writer::TextExtract none 47 "" $Pad.body]"

   close $f

   return $file
}
