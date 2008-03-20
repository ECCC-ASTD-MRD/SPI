#===============================================================================
# Environnement Canada
# Centre Meteorologique Canadian
# 2100 Trans-Canadienne
# Dorval, Quebec
#
# Projet   : Package d'interface pour SPI
# Fichier  : Messenger.tcl
# Version  : 1.1
# Creation : Mai 2000
#
# Description:
#    Permet d'ecrire des messages (FVCN, AACN, ...) en utilisant SPI pour faire
#    de l'entree de parametres
#
# Modifications :
#
#   Nom         : -
#   Date        : -
#   Description : -
#
#===============================================================================

#----- Lire les sources d'execution

source $GDefs(Dir)/Apps/Tools/Writer/Writer.ctes
source $GDefs(Dir)/Apps/Tools/Writer/Writer.txt
source $GDefs(Dir)/Apps/Tools/Writer/Writer.int

#----- Lire les formats de messages

foreach format [glob $GDefs(Dir)/Apps/Tools/Writer/Types/*.tcl] {
   source $format
   lappend Writer::Data(Types)  [file rootname [file tail $format]]
}

#-------------------------------------------------------------------------------
# Nom      : <Writer::Close>
# Creation : Mai 2000 - J.P. Gauthier - CMC/CMOE -
#
# But      : Ferme l'interface.
#
# Parametres :
#
# Remarques :
#    -On doit redefinir les fonctions de range des projections
#
# Modifications :
#
#   Nom         : -
#   Date        : -
#   Description : -
#
#-------------------------------------------------------------------------------

proc Writer::Close { } {
   variable Data

   #----- Liberer l'allocation des champs

   if { [lindex [split $Page::Data(ToolMode) ":"] 0]=="Writer" } {
      SPI::ToolMode SPI Zoom
   }

   $Data(Canvas) delete FVCN

   set Data(Active) 0
   destroy .writer

   if { !$SPI::Param(Window) } { SPI::Quit }
}

#----------------------------------------------------------------------------
# Nom      : <Writer::PadClose
# Creation : Octobre 2001 - J.P. Gauthier - CMC/CMOE
#
# But      : Supprime un onglet/message
#
# Parametres :
#   <Save>   : Demande de sauvegarde
#
# Retour:
#
# Remarques :
#
# Modifications :
#
#    Nom         : -
#    Date        : -
#    Description : -
#----------------------------------------------------------------------------

proc Writer::PadClose { Save } {
   global   GDefs
   variable Data
   variable Lbl
   variable Msg

   if { $Data(Pad)!="" } {

      #----- Verification de l'edition du message

      eval set upd \$Writer::$Data(Type)::Data(Handle$Data(Pad))

      #----- Sauvegarde

      if { $upd!="" && $Save } {
         set Save [Dialog::CreateDefault .writer 300 [lindex $Lbl(Warning) $GDefs(Lang)] [lindex $Msg(Save) $GDefs(Lang)] \
            info 0 [lindex $Lbl(No) $GDefs(Lang)] [lindex $Lbl(Yes) $GDefs(Lang)]]

         if { $Save } {
            Writer::${Data(Type)}::Write $Data(Pad) 0
         }
      }

      #----- Sortir du mode dessin

      if { [lindex [split $Page::Data(ToolMode) ":"] 0]=="Writer" } {
         SPI::ToolMode SPI Zoom
      }

      #----- Nettoyage du message

      Writer::$Data(Type)::Clear $Data(Pad)

      #----- Suppression de l'onget

      TabFrame::Delete .writer.pad 1 [string index $Data(Pad) end]
      set Data(Pad) ""
      TabFrame::Select .writer.pad [TabFrame::Current .writer.pad]
   }
}

#----------------------------------------------------------------------------
# Nom      : <Writer::PadNew>
# Creation : Octobre 2001 - J.P. Gauthier - CMC/CMOE
#
# But      : Initialise le canvas d'un nouvle onglet/message
#
# Parametres :
#   <Type>   : Type de message
#   <Mode>   : Mode de creation (New,Open)
#   <Layout> : Modele (NEW,RET)
#   <File>   : Nom du fichier (Inutile si Mode=New)
#
# Retour:
#
# Remarques :
#
# Modifications :
#
#    Nom         : -
#    Date        : -
#    Description : -
#----------------------------------------------------------------------------

proc Writer::PadNew { Type Mode Layout File } {
   global GDefs
   variable Lbl
   variable Bubble
   variable Data

   if { $File=="" && $Mode=="Open" } {
      return
   }

   if { $File=="" } {
      set title "$Layout $Type"
   } else {
      set title [file tail $File]
      set Type [string range [exec head -1 $File] 0 3]
   }

   set Data(Pad)  [TabFrame::Add .writer.pad 1 $title False]
   set Data(Type) $Type

   frame $Data(Pad).head
   pack $Data(Pad).head -side top -fill x

   canvas $Data(Pad).canvas -height 400 -width 500 -bg white -relief sunken -bd 1

   frame $Data(Pad).i
      label $Data(Pad).i.file -relief sunken -bd 1 -textvariable Writer::${Type}::Data(File$Data(Pad)) \
         -anchor w -bg $GDefs(ColorLight)
      pack  $Data(Pad).i.file -side left -fill both -expand true
   pack $Data(Pad).i -side bottom -fill x
   pack $Data(Pad).canvas -side left -fill both -expand true


   Writer::${Type}::LayoutInit $Data(Pad)
   trace variable SPI::Src(Info) w "Writer::${Type}::Source"

   if { $File!="" } {
      set ok [${Type}::${Mode} $Data(Pad) $File]
   } else {
      ${Type}::${Mode} $Data(Pad) $Layout
   }

   Writer::${Type}::ToolBar $Data(Pad).head
   TabFrame::Select .writer.pad [TabFrame::Current .writer.pad]
}

#----------------------------------------------------------------------------
# Nom      : <Writer::PadName>
# Creation : Octobre 2001 - J.P. Gauthier - CMC/CMOE
#
# But      : Callback du changement de nom d'onglets
#
# Parametres :
#   <Pad>    : Identificateur de l'onglets
#   <Name>   : Nouveau nom
#
# Retour:
#
# Remarques :
#
# Modifications :
#
#    Nom         : -
#    Date        : -
#    Description : -
#----------------------------------------------------------------------------

proc Writer::PadName { Pad Name } {

   TabFrame::Label .writer.pad 1 [string index $Pad end] $Name
}

#----------------------------------------------------------------------------
# Nom      : <Writer::PadSwitch>
# Creation : Octobre 2001 - J.P. Gauthier - CMC/CMOE
#
# But      : Callback du changement d'onglets
#
# Parametres :
#   <Tab>    : Identificateur de la boite d'onglets
#   <No>     : Numero de l'onglet selectionne
#
# Retour:
#
# Remarques :
#
# Modifications :
#
#    Nom         : -
#    Date        : -
#    Description : -
#----------------------------------------------------------------------------

proc Writer::PadSwitch { Tab No } {
   variable Data

   set Data(Pad)  $Tab.frame$No

   set Data(Type) ""

   if { [winfo exists $Data(Pad).fvcn] } {
      set Data(Type) FVCN
   }
   if { [winfo exists $Data(Pad).aacn] } {
      set Data(Type) AACN
   }
}


#----------------------------------------------------------------------------
# Nom      : <Writer::Send>
# Creation : Octobre 2001 - J.P. Gauthier - CMC/CMOE
#
# But      : Callback de transmission sur les circuits
#
# Parametres :
#   <Backup> : Methode backup
#
# Retour:
#
# Remarques :
#
# Modifications :
#
#    Nom         : -
#    Date        : -
#    Description : -
#----------------------------------------------------------------------------

proc Writer::Send { { Backup 0 } } {
   global   GDefs
   variable Lbl
   variable Msg
   variable Data

   #----- Confirmer la transmission

   set nosend [Dialog::CreateDefault .writer 300 [lindex $Lbl(Warning) $GDefs(Lang)] \
                 [lindex $Msg(Send) $GDefs(Lang)] warning 0 [lindex $Lbl(Send) $GDefs(Lang)] \
                 [lindex $Lbl(Cancel) $GDefs(Lang)]]
   if { $nosend } {
      return
   }

   Writer::$Data(Type)::Send $Data(Pad) $Backup
   Writer::PadClose 0

   Dialog::CreateDefault .writer 300 Info [lindex $Writer::Msg(Sent) $GDefs(Lang)] info 0 Ok
}

#----------------------------------------------------------------------------
# Nom      : <Writer::TextExtract>
# Creation : Septembre 2001 - J.P. Gauthier - CMC/CMOE
#
# But      : Extraire les lignes et les formatter.
#
# Parametres :
#  <Text>    : Identificateur du widget Text
#  <Type>    : Type de format (char,word)
#  <Len>     : Longueur des lignes a extraire
#  <args>    : Nom du widget text (si Text=="")
#
# Retour:
#
# Remarques :
#
# Modifications :
#
#    Nom         : -
#    Date        : -
#    Description : -
#----------------------------------------------------------------------------

proc Writer::TextExtract { Type Len Text args } {

   if { $Text=="" } {
      set Text [$args get 0.0 end]
   }

   set Text [string trim [string toupper $Text]]
   regsub -all "\n" $Text " " Text

   set idx   0
   set line  ""
   set lines {}

   switch "$Type" {

      char {

         while { $idx < [expr 1+[string length $Text]/$Len] } {
            lappend lines [string range $Text [expr $idx*$Len] [expr ($idx+1)*$Len-1]]
            incr idx
         }
      }

      word {

         set words [split $Text " "]

         foreach word $words {

            set word [string trim $word]

            if { $word!="" } {

               incr idx [string length $word]

               if { $idx < $Len } {
                  if { $line!="" } {
                     append line " "
                     incr idx 1
                  }
                  append line $word
               } else {
                  lappend lines $line
                  set idx [string length $word]
                  set line $word
               }
            }
         }
         lappend lines $line
      }

      none {
         set lines $Text
      }
   }
   return $lines
}

#----------------------------------------------------------------------------
# Nom      : <Writer::BlocFormat>
# Creation : Septembre 2001 - J.P. Gauthier - CMC/CMOE
#
# But      : Formatte le message dans un fichier texte
#
# Parametres :
#   <Header> : Entete du bloc
#   <Text>   : Corps du bloc
#   <Width>  : Largeur entetes
#
# Retour     :
#
# Remarques :
#    Aucune.
#
# Modifications  :
#
#    Nom         : -
#    Date        : -
#    Description : -
#
#----------------------------------------------------------------------------

proc Writer::BlocFormat { Header Text { Width 21 } } {

   set text {}
   foreach line $Text {
      lappend text "[format "%-${Width}s" ""] $line"
   }
   set text [string replace [join $text "\n"] 0 [expr $Width-1] [format "%-${Width}s" $Header]]
   return $text
}

#----------------------------------------------------------------------------
# Nom      : <Writer::TextExpand>
# Creation : Octobre 2001 - J.P. Gauthier - CMC/CMOE
#
# But      : Expansionner un Text widget afin que tout le texte soit visible.
#
# Parametres :
#  <Text>    : Identificateur du widget
#
# Retour:
#
# Remarques :
#
# Modifications :
#
#    Nom         : -
#    Date        : -
#    Description : -
#----------------------------------------------------------------------------

proc Writer::TextExpand { Text Len { Max 0 } } {
   variable Data

   if { $Max } {
      set text [$Text get 0.0 end]
      set len  [string length $text]
      if { $len>$Max } {
         bell
         set insert [$Text index insert]
         $Text delete 0.0 end
         $Text insert 0.0 [string range $text 0 [expr $Max-1]]
         $Text mark set insert $insert
      }
   }

   set nb [lindex [split [$Text index end] .] 0]
   set he 0

   for { set i 1 } { $i <$nb } { incr i } {
      set len [lindex [split [$Text index $i.end] .] 1]
      incr he [expr 1+int($len/$Len)]
   }

   $Text configure -height $he
   return $he
}

#-------------------------------------------------------------------------------
# Nom      : <Writer::ToolMode>
# Creation : Octobre 2001 - J.P. Gauthier - CMC/CMOE -
#
# But      : Mettre SPI en mode selection de Writer.
#
# Parametres :
#
# Remarques :
#
# Modifications :
#
#   Nom         : -
#   Date        : -
#   Description : -
#
#-------------------------------------------------------------------------------

proc Writer::ToolMode { args } {
   global GDefs
   variable Data
   variable Lbl

   if { $Page::Data(ToolMode)=="Writer" } {

      #----- Mode selection de donnees de la projection

      Page::ModeSelect Draw
      set Page::Data(DrawMode) Writer::FVCN
  } else {
      $Data(Canvas) delete FVCN VERTEXFOLLOW
      SPI::ToolMode Writer
  }
}

#----------------------------------------------------------------------------
# Nom      : <Writer::UpdateItems>
# Creation : Septembre 2001 - J.P. Gauthier - CMC/CMOE
#
# But      :
#
# Parametres :
#  <Frame>   : Identificateur de Page
#  <VP>      : Identificateur du viewport
#  <Pad>     : Identificateur du pad
#
# Retour:
#
# Remarques :
#
# Modifications :
#
#    Nom         : -
#    Date        : -
#    Description : -
#----------------------------------------------------------------------------

proc Writer::UpdateItems { Frame } {
   global GDefs
   variable Data

   Writer::$Data(Type)::UpdateItems $Frame "" $Data(Pad)
}

#-------------------------------------------------------------------------------
# Nom      : <Writer::AsProject>
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

proc Writer::AsProject { File } {
   variable Data
   variable Param


   if { [winfo exists .writer] } {
      puts $File "#----- Tool: Writer\n"
      puts $File "set Writer::Param(Dock)   $Param(Dock)"
      puts $File "set Writer::Param(Geom)   [winfo geometry .writer]"
      puts $File "Writer::Window"
      puts $File "\n"
   }
}
