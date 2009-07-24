#===============================================================================
# Environnement Canada
# Centre Meteorologique Canadian
# 2100 Trans-Canadienne
# Dorval, Quebec
#
# Projet   : Librairie de "Widget" Tk.
# Fichier  : Dialog.tk
# Creation : Octobre 1998 - J.P.Gauthier - CMC/CMOE
#
# Description:
#    Permet d'afficher diverses boite de dialogue standard.
#
# Fonctions:
#    Dialog::CreateDefault      { Master Loc Width Title Text Bitmap Default args }
#    Dialog::CreateInfo         { Master Text }
#    Dialog::CreateWait         { Master Text { Percent 0 } }
#    Dialog::DestroyWait        { }
#    Dialog::CreateError        { Master Text Lang }
#    Dialog::CreateErrorListing { Master Text List Lang }
#    Dialog::CreateMessage      { Master Text }
#    Dialog::CreateText         { Id Title File Width Height }
#    Text::Create               { Id Title File Width Height }
#    Text::Save                 { Text File }
#
# Remarques :
#    -Concu a partir de namespace donc utilisable seulement en TCL 8.0 et +
#
#===============================================================================

package provide Dialog 2.1

catch { SPI::Splash "Loading Widget Package Dialog 2.1" }

namespace eval Dialog { }

#-------------------------------------------------------------------------------
# Nom      : <Dialog::CreateDefault>
# Creation : Mai 1997 - J.P. Gauthier - CMC/CMOE
#
# But      : Permet de creer des boites de dialog de maniere standard.
#
# Parametres :
#   <Master> : Frame a qui la boite appartient.
#   <Width>  : Largeur de la boite
#   <Title>  : Titre de la boite.
#   <Text>   : Texte a afficher dans la boite.
#   <Bitmap> : Bitmap de la boite.
#   <Default>: Index du bouton par default (-1 si aucun).
#   <args>   : Liste des boutons a inserer dans la boite (Oui , Ok , ... ).
#
# Retour     :
#   <button> : Numero du bouton selectionne
#
# Remarques : La boite retourne l'index du boutton presse.
#
#-------------------------------------------------------------------------------

proc  Dialog::CreateDefault { Master Width Title Text Bitmap Default args } {
   global button

   toplevel .dg -class Dialog
   wm title .dg $Title
   wm resizable .dg 0 0
   wm protocol .dg WM_DELETE_WINDOW { }

   #----- Positionnement de la boite

   if { [winfo exists $Master] } {
      wm transient .dg $Master
      wm geom .dg +[expr [winfo rootx $Master]+50]+[expr [winfo rooty $Master]+50]
   }

   frame .dg.top -relief raised -bd 1
   pack .dg.top -side top -fill both -expand true
   frame .dg.bot
   pack .dg.bot -side bottom -fill x -expand true

   #----- Insertion du bitmap et du message

   message .dg.top.msg -width $Width -text $Text
   pack .dg.top.msg -side right -expand 1 -fill both -padx 3m -pady 3m

   if { $Bitmap != ""} {
      label .dg.top.bitmap -bitmap $Bitmap
      pack .dg.top.bitmap -side left -padx 3m -pady 3m
   }

   #----- Creation de la rangee de bouttons

   set i 0
   foreach but $args {
      button .dg.bot.button$i -text $but -command "set button $i"  -relief raised -bd 1
      if {$i == $Default} {
         .dg.bot.button$i configure -foreground green
      }
      pack .dg.bot.button$i -side left -expand 1 -fill x
      incr i
   }

   set oldFocus [focus]

   #----- Mise en place de la surveillance des bouttons

   if { $Default >= 0 } {
      bind .dg <Return> ".dg.bot.button$Default flash; \
         set button $Default"
      focus  .dg.bot.button$Default
   }

   update idletasks
   grab .dg

   #----- Attente de la selection du boutton

   tkwait variable button
   focus $oldFocus
   destroy .dg
   return $button
}

#----------------------------------------------------------------------------
# Nom      : <Dialog::CreateError>
# Creation : Juillet 97 - J.P. Gauthier - CMC/CMOE -
#
# But      : Afficher un message d'erreur.
#
# Parametres :
#    <Master> : Fenetre toplevel auquel l'aide est reliee.
#    <Text>   : Texte a afficher.
#    <Lang>   : Langue ( 0 Francais 1 Anglais ) .
#    <Aspect> : Aspect de la boite (default=1000).
#
# Remarques :
#    Aucune.
#
#----------------------------------------------------------------------------

proc Dialog::CreateError { Master Text Lang { Aspect 1000 } } {

   if { ![info exists ::tk_version] } {
      puts stderr "(ERROR) $Text"
      return
   }

   set previous [grab current]

   if { [winfo exists .error] == 1} {

      set oldtext [lindex [.error.haut.txt configure -text] 4]
      .error.haut.txt configure -text "$oldtext\n\n$Text"

   } else {

      toplevel .error
      wm title .error [lindex { "Erreur" "Error" } $Lang]
      wm protocol .error WM_DELETE_WINDOW { }
      if { [winfo exists $Master] } {
#         wm transient .error $Master
         wm geom .error +[expr [winfo rootx $Master]+50]+[expr [winfo rooty $Master]+50]
      }

      # ----- Afficher le frame du haut qui va contenir le message

      frame .error.haut -relief raised -bd 1
      pack .error.haut -side top -expand true -fill x

      label .error.haut.bitmap -bitmap error
      message .error.haut.txt -aspect $Aspect -text $Text
      pack .error.haut.bitmap -side left -padx 20 -pady 20
      pack .error.haut.txt -padx 20 -pady 20

      # ----- Afficher le frame du bas qui va contenir le bouton retour

      button .error.ok -text "OK" -command "catch { grab [lindex $previous 0] } ; destroy .error" -bd 1
      pack .error.ok -side bottom -ipadx 10 -fill x
      update idletasks
      grab .error
   }
}

#----------------------------------------------------------------------------
# Nom      : <Dialog::CreateErrorListing>
# Creation : Mars 2008 - J.P. Gauthier - CMC/CMOE -
#
# But      : Afficher un message d'erreur avec un liste deroulante pour l'info.
#
# Parametres :
#    <Master> : Fenetre toplevel auquel l'aide est reliee.
#    <Text>   : Texte a afficher.
#    <List>   : Text a ajouter a la liste deroulante
#    <Lang>   : Langue ( 0 Francais 1 Anglais ) .
#    <Aspect> : Aspect de la boite (default=1000).
#
# Remarques :
#    Aucune.
#
#----------------------------------------------------------------------------

proc Dialog::CreateErrorListing { Master Text List Lang } {
   global GDefs

   if { ![info exists ::tk_version] } {
      puts stderr "(ERROR) $Text"
      return
   }

   set previous [grab current]

   if { [winfo exists .errorlist] == 1} {

      .errorlist.haut.txt configure -text "$Text"
      .errorlist.bas.list insert end $List

   } else {

      toplevel .errorlist
      wm title .errorlist [lindex { "Erreur" "Error" } $Lang]
      wm protocol .errorlist WM_DELETE_WINDOW { }
      if { [winfo exists $Master] } {
         wm geom .errorlist +[expr [winfo rootx $Master]+50]+[expr [winfo rooty $Master]+50]
      }

      #----- Afficher le frame du haut qui va contenir le message

      frame .errorlist.haut -relief raised -bd 1
         label .errorlist.haut.bitmap -bitmap error
         message .errorlist.haut.txt -aspect 1000 -text $Text
         pack .errorlist.haut.bitmap -side left -padx 20 -pady 20
         pack .errorlist.haut.txt -padx 20 -pady 20
      pack .errorlist.haut -side top -fill x

      frame .errorlist.bas
         scrollbar .errorlist.bas.scroll -command ".errorlist.bas.list yview" -bd 1 -width 10
         text .errorlist.bas.list -relief sunken -yscrollcommand ".errorlist.bas.scroll set" \
            -exportselection 0 -background $GDefs(ColorLight) -bd 1
         pack .errorlist.bas.list -side left -fill both -expand True
         pack .errorlist.bas.scroll -side left -fill y
      pack  .errorlist.bas -side top -fill both -expand True

      .errorlist.bas.list insert 0.0 $List

      #----- Afficher le frame du bas qui va contenir le bouton retour

      button .errorlist.ok -text "OK" -command "catch { grab [lindex $previous 0] } ; destroy .errorlist" -bd 1
      pack .errorlist.ok -side bottom -ipadx 10 -fill x
      update idletasks
      grab .errorlist
   }
}

#----------------------------------------------------------------------------
# Nom      : <Dialog::CreateInfo>
# Creation : Mai 2000 - J.P. Gauthier - CMC/CMOE -
#
# But      : Afficher un message d'information.
#
# Parametres :
#    <Master> : Fenetre toplevel auquel l'aide est reliee.
#    <Text>   : Texte a afficher.
#    <Lang>   : Langue ( 0 Francais 1 Anglais ) .
#    <Aspect> : Aspect de la boite (default=1000).
#
# Remarques :
#    Aucune.
#
#----------------------------------------------------------------------------

proc Dialog::CreateInfo { Master Text { Aspect 1000 } } {

   if { ![info exists ::tk_version] } {
      puts "(INFO) $Text"
      return
   }

   if { [winfo exists .dlginfo] == 1} {

      set oldtext [lindex [.dlginfo.haut.txt configure -text] 4]
      .dlginfo.haut.txt configure -text "$oldtext\n\n$Text"

   } else {

      toplevel .dlginfo
      wm title .dlginfo Info
      wm protocol .dlginfo WM_DELETE_WINDOW { }
      if { [winfo exists $Master] } {
         wm transient .dlginfo $Master
         wm geom .dlginfo +[expr [winfo rootx $Master]+50]+[expr [winfo rooty $Master]+50]
      }

      # ----- Afficher le frame du haut qui va contenir le message

      frame .dlginfo.haut -relief raised -bd 1
      pack .dlginfo.haut -side top -expand true -fill x

      label .dlginfo.haut.bitmap -bitmap info
      message .dlginfo.haut.txt -aspect $Aspect -text $Text
      pack .dlginfo.haut.bitmap -side left -padx 20 -pady 20
      pack .dlginfo.haut.txt -padx 20 -pady 20

      # ----- Afficher le frame du bas qui va contenir le bouton retour

      button .dlginfo.ok -text "OK" -command "destroy .dlginfo" -bd 1
      pack .dlginfo.ok -side bottom -ipadx 10 -fill x
      update idletasks
      grab .dlginfo
   }
}

#----------------------------------------------------------------------------
# Nom      : <Dialog::CreateWait>
# Creation : Mai 2000 - J.P. Gauthier - CMC/CMOE -
#
# But      : Afficher un message d'information sur un processue en cours.
#
# Parametres :
#    <Master>  : Fenetre toplevel auquel l'aide est reliee.
#    <Text>    : Texte a afficher.
#    <Percent> : Pourcentage d'execution.
#
# Remarques :
#    Aucune.
#
#----------------------------------------------------------------------------

proc Dialog::CreateWait { Master Text { Percent 1000 } } {

   if { ![info exists ::tk_version] } {
      puts "(INFO) $Text"
      return
   }

   if { [winfo exists .dlgwait]==1 } {

      set oldtext [lindex [.dlgwait.txt configure -text] 4]
      .dlgwait.txt configure -text "$oldtext\n\n$Text"

   } else {

      toplevel .dlgwait
      wm overrideredirect .dlgwait true
      if { [winfo exists $Master] } {
         wm transient .dlgwait $Master
         wm geom .dlgwait +[expr [winfo rootx $Master]+[winfo width $Master]/2-300]+[expr [winfo rooty $Master]+[winfo height $Master]/2-50]
      }

      message .dlgwait.txt -aspect 1000 -text $Text -relief raised -bd 1
      pack .dlgwait.txt -ipadx 20 -ipady 20
      update idletasks

      grab .dlgwait
   }
   update idletasks
}

#----------------------------------------------------------------------------
# Nom      : <Dialog::CreateWait>
# Creation : Mai 2000 - J.P. Gauthier - CMC/CMOE -
#
# But      : Detruit le message d'information sur un processue en cours.
#
# Parametres :
#
# Remarques :
#    Aucune.
#
#----------------------------------------------------------------------------

proc Dialog::DestroyWait { } {

   if { ![info exists ::tk_version] } {
      return
   }

   destroy .dlgwait
   update idletasks
}

#----------------------------------------------------------------------------
# Nom      : <Dialog::CreateMessage>
# Creation : Juillet 98 - J.P. Gauthier - CMC/CMOE -
#
# But      : Afficher un message dynamique standard.
#
# Parametres  :
#    <Master> : Fenetre toplevel auquel l'aide est reliee.
#    <Text>   : Texte a afficher.
#    <Aspect> : Aspect de la boite (default=1000).
#
# Remarques :
#    Aucune.
#
#----------------------------------------------------------------------------

proc Dialog::CreateMessage { Master Text { Aspect 1000 } } {

   if { ![info exists ::tk_version] } {
      puts "(INFO) $Text"
      return
   }

   if { [winfo exists .msgbox] } {

      .msgbox.txt config -text $Text
      update idletasks
   } else {

      toplevel .msgbox
      wm title .msgbox "Message ([wm title $Master])"
      if { [winfo exists $Master] } {
         wm transient .msgbox $Master
         wm geom .msgbox +[expr [winfo rootx $Master]+50]+[expr [winfo rooty $Master]+50]
      }

      # ----- Creer et affiche le message.

      message .msgbox.txt -aspect $Aspect -relief raised -bd 1 -text $Text
      pack .msgbox.txt -ipadx 20 -ipady 20

      .msgbox config -cursor watch
      update idletasks
   }
   grab .msgbox
}

#----------------------------------------------------------------------------
# Nom      : <Text::Create>
# Creation : Novembre 98 - J.P. Gauthier - CMC/CMOE -
#
# But      : Ouvrir une fenetre contenant un fichier texte.
#
# Parametres  :
#    <Id>     : Identification de la fenetre
#    <Title>  : Titre de la fenetre
#    <File>   : Fichier texte
#    <Width>  : Largenu du widget text
#    <Height> : Hauteur du widget text
#
# Remarques :
#    Aucune.
#
#----------------------------------------------------------------------------

namespace eval Text { } {
   variable Bubble
   variable Data

   set Bubble(Print)      { "Impressiondu contenue" "Print window content" }
   set Bubble(Save)       { "Sauvegarde du contenu" "Save window content" }
}

proc Text::Create { Id Title File Width Height } {
   global GDefs
   variable Bubble
   variable Data

   if { [winfo exists $Id]} {

      wm title $Id $Title
      wm deiconify $Id
      wm transient $Id .
      raise $Id

      $Id.file.text configure -width $Width -height $Height
      $Id.file.text delete 0.0 end

   } else {

      toplevel $Id -class Dialog
      wm title $Id $Title
      wm transient $Id .

      frame $Id.command
         button $Id.command.ok -text [lindex { Fermer Close } $GDefs(Lang)] -relief raised -bd 1 \
            -command "destroy $Id"
         button $Id.command.print -image PRINT -relief raised -bd 1 -command "PrintBox::Create $Id.file.text PRINT Text"
         button $Id.command.save -image OPENDOC -relief raised -bd 1 -command "Text::Save $Id.file.text \[FileBox::Create $Id \"\" Save \[list \$FileBox::Type(TXT)\]\]"
         label $Id.command.line -relief raised -bd 1 -width 10 -textvariable Text::Data(Cursor$Id) -bg $GDefs(ColorLight)
         pack $Id.command.print $Id.command.save -side left
         pack $Id.command.ok -side left -fill both -expand true
         pack $Id.command.line -side left -fill y
      pack $Id.command -side bottom -fill x

      frame $Id.file -relief raised -bd 1
         text $Id.file.text -relief sunken -bd 1 -yscrollcommand "$Id.file.scrolly set"  -xscrollcommand "$Id.file.scrollx set" \
            -width $Width  -height $Height -bg $GDefs(ColorLight) -wrap none
         scrollbar $Id.file.scrolly -relief sunken -command "$Id.file.text yview" -bd 1 -width 10
         scrollbar $Id.file.scrollx -relief sunken -command "$Id.file.text xview" -bd 1 -width 10 -orient horizontal

         pack $Id.file.scrollx -side bottom -fill x -anchor s
         pack $Id.file.text -side left -expand true -fill both
         pack $Id.file.scrolly -side left -fill y
      pack $Id.file -side bottom -fill both -expand true


      Bubble::Create $Id.command.print [lindex $Bubble(Print) $GDefs(Lang)]
      Bubble::Create $Id.command.save  [lindex $Bubble(Save) $GDefs(Lang)]

      bind $Id.file.text <Any-KeyRelease> "set Text::Data(Cursor$Id) \[$Id.file.text index insert\]"
      bind $Id.file.text <Any-Button> "set Text::Data(Cursor$Id) \[$Id.file.text index insert\]"
   }

   #----- Inclure le fichier texte si il existe

   if { [file exists $File] } {

      set f [open $File]
      while { [gets $f ligne] >= 0 } {
         $Id.file.text insert end "$ligne\n"
      }
      close $f
   } else {
      $Id.file.text insert end $File
      set Text::Data(Nb$Id) [$Id.file.text index end]
   }

   #----- Traiter les directives si elles existent

   if { [file exists $File.dir] } {

      set f [open $File.dir]
      while { [gets $f ligne] >= 0 } {
         Text::Search $Id.file.text [lindex $ligne 0] [lindex $ligne 1] [lindex $ligne 2]
      }
      close $f
   }

   return $Id.file.text
}

proc Text::Save { Text File } {

   if { $File=="" } {
      return
   }

   set f [open $File w]
   puts $f [$Text get 0.0 end]
   close $f
}

proc Text::PrintCommand { Widget } {
   variable Data

   exec echo [$Widget get 0.0 end] > /tmp/[pid].txt
   PrintBox::PrintTXT /tmp/[pid].txt
   file delete /tmp/[pid].txt

   PrintBox::Destroy
}

proc Text::Search { Widget String Tag args } {

   if {$String == ""} {
      return
   }
   set cur 1.0
   while 1 {
      set cur [ $Widget search -count length $String $cur end]
      if { $cur == "" } {
         break
      }
   $Widget tag add $Tag $cur "$cur + $length char"
   set cur [$Widget index "$cur + $length char"]
   }
   eval $Widget tag configure $Tag [join $args " "]
}

#----------------------------------------------------------------------------
# Nom      : <Dialog::SearchText>
# Creation : Novembre 98 - J.P. Gauthier - CMC/CMOE -
#
# But      : Effectue une recherche sur une fenetre texte et y highlight les
#            occurences comme specifie.
#
# Parametres  :
#    <Path>   : Identification de la fenetre
#    <String> : Chaine a rechercher
#    <Tag>    : Tag a donner aux occurences
#    <args>   : Parametres a associee au tag
#
# Remarques :
#    Aucune.
#
#----------------------------------------------------------------------------

proc Dialog::SearchText { Path String Tag args } {
   eval Text::Search \$Path \$String \$Tag $args
}
