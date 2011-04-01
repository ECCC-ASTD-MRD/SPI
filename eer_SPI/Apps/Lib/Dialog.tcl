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
#    Dialog::Default      { Master Width Type Text Extra Default args }
#    Dialog::Info         { Master Text { Extra "" } }
#    Dialog::Wait         { Master Text { Extra "" } }
#    Dialog::WaitDestroy  { { Ask False } }
#    Dialog::Error        { Master Text { Extra "" } }
#    Dialog::ErrorListing { Master Text List }
#    Dialog::Message      { Master Text { Extra "" } }
#    Dialog::Get          { Master Title Text { Var "" } }
#    Dialog::Give         { Master Title Text Info }
#    Dialog::Text         { Id Title File Width Height }
#    Dialog::TextSave     { Text File }
#    Dialog::TextSearch   { Widget String Tag args }
#
# Remarques :
#    -Concu a partir de namespace donc utilisable seulement en TCL 8.0 et +
#    - Icon license
#         author : Aleksandra Wolska
#         e-mail : wolskaola@gmail.com
#         www    : olawolska.com, graffika.org
#         license: Attribution-Share Alike 3.0 Unported (http://creativecommons.org/licenses/by-sa/3.0/)
#===============================================================================

package provide Dialog 3.0
package require Logger

catch { SPI::Splash "Loading Widget Package Dialog 3.0" }

namespace eval Dialog { } {
   variable Data
   variable Lbl
   variable Bubble

   set Lbl(Ok)       { "Ok" "Ok" }
   set Lbl(Continue) { "Continuer" "Continue" }
   set Lbl(Cancel)   { "Annuler" "Cancel" }
   set Lbl(Clip)     { "Copier vers le presse-papier" "Copy to clipboard" }

   set Lbl(WARNING)  { "Avertissement" "Warning" }
   set Lbl(INFO)     { "Information" "Information" }
   set Lbl(MESSAGE)  { "Message" "Message" }
   set Lbl(ERROR)    { "Erreur" "Error" }
   set Lbl(QUESTION) { "Question ?" "Question ?" }

   set Lbl(Running)  { "Une processus est en cours, voulez-vous continuer et tuer ce processus ou annuler ?"
                       "A process is currently running, do you wich to continue and kill it or cancel ?" }

   set Bubble(Print) { "Impressiondu contenue" "Print window content" }
   set Bubble(Save)  { "Sauvegarde du contenu" "Save window content" }

   if { [info exists ::tk_version] } {
      image create photo DIALOG_ERROR    -file $GDefs(Dir)/Resources/Image/Icon/Dialog_Error.gif
      image create photo DIALOG_WAIT     -file $GDefs(Dir)/Resources/Image/Icon/Dialog_Timer.gif
      image create photo DIALOG_WARNING  -file $GDefs(Dir)/Resources/Image/Icon/Dialog_Alert.gif
      image create photo DIALOG_QUESTION -file $GDefs(Dir)/Resources/Image/Icon/Dialog_Tick.gif
      image create photo DIALOG_INFO     -file $GDefs(Dir)/Resources/Image/Icon/Dialog_Info.gif
      image create photo DIALOG_CLIP     -file $GDefs(Dir)/Resources/Image/Icon/Dialog_Clip.gif
   }
}

#-------------------------------------------------------------------------------
# Nom      : <Dialog::Default>
# Creation : Mai 1997 - J.P. Gauthier - CMC/CMOE
#
# But      : Permet de creer des boites de dialog de maniere standard.
#
# Parametres :
#   <Master> : Frame a qui la boite appartient.
#   <Width>  : Largeur de la boite
#   <Title>  : Titre bilingue de la boite.
#   <Text>   : Texte bilingue a afficher dans la boite.
#   <Extra>  : Texte suplementaire
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

proc  Dialog::Default { Master Width Type Text Extra Default args } {
   global GDefs
   global button
   variable Lbl

   switch $Type {
      "WARNING"  { set title [lindex $Lbl(WARNING)  $GDefs(Lang)]; set icon DIALOG_WARNING }
      "INFO"     { set title [lindex $Lbl(INFO)     $GDefs(Lang)]; set icon DIALOG_INFO }
      "ERROR"    { set title [lindex $Lbl(ERROR)    $GDefs(Lang)]; set icon DIALOG_ERROR }
      "MESSAGE"  { set title [lindex $Lbl(MESSAGE)  $GDefs(Lang)]; set icon DIALOG_QUESTION }
      "QUESTION" { set title [lindex $Lbl(QUESTION) $GDefs(Lang)]; set icon DIALOG_QUESTION }
      default    { set title [lindex $Type $GDefs(Lang)];          set icon DIALOG_QUESTION }
   }

   toplevel     .dlgdef -class Dialog
   wm title     .dlgdef $title
   wm resizable .dlgdef 0 0
   wm protocol  .dlgdef WM_DELETE_WINDOW { }

   #----- Positionnement de la boite
   if { [winfo exists $Master] } {
      wm transient .dlgdef $Master
      wm geom .dlgdef +[expr [winfo rootx $Master]+50]+[expr [winfo rooty $Master]+50]
   }

   frame .dlgdef.top -relief raised -bd 1
      label .dlgdef.top.bitmap -image $icon
      message .dlgdef.top.msg -width $Width -text "[lindex $Text $GDefs(Lang)]$Extra"
      pack .dlgdef.top.bitmap .dlgdef.top.msg -side left -expand 1 -fill both -padx 3m -pady 3m
    pack .dlgdef.top -side top -fill both -expand true

   frame .dlgdef.bot
   pack .dlgdef.bot -side bottom -fill x -expand true

   set i 0
   foreach but $args {
      button .dlgdef.bot.button$i -text [lindex $but $GDefs(Lang)] -command "set button $i"  -relief raised -bd 1
      if {$i == $Default} {
         .dlgdef.bot.button$i configure -foreground green
      }
      pack .dlgdef.bot.button$i -side left -expand 1 -fill x
      incr i
   }

   set oldFocus [focus]

   #----- Mise en place de la surveillance des bouttons
   if { $Default>=0 } {
      bind .dlgdef <Return> ".dlgdef.bot.button$Default flash; set button $Default"
      focus .dlgdef.bot.button$Default
   }

   update idletasks
   grab .dlgdef

   #----- Attente de la selection du boutton
   tkwait variable button
   focus $oldFocus
   destroy .dlgdef
   return $button
}

#----------------------------------------------------------------------------
# Nom      : <Dialog::Error>
# Creation : Juillet 97 - J.P. Gauthier - CMC/CMOE -
#
# But      : Afficher un message d'erreur.
#
# Parametres :
#    <Master> : Fenetre toplevel auquel l'aide est reliee.
#    <Text>   : Texte bilingue a afficher.
#    <Extra>  : Texte supplementaire.
#
# Remarques :
#    Aucune.
#
#----------------------------------------------------------------------------

proc Dialog::Error { Master Text { Extra "" } } {
   global GDefs
   global button
   variable Lbl

   uplevel 1 "Log::Print ERROR \{[lindex $Text $GDefs(Lang)]$Extra\}"

   if { ![info exists ::tk_version] } {
      return
   }

   set previous [grab current]

   if { [winfo exists .dlgerr] == 1} {

      set oldtext [lindex [.dlgerr.haut.txt configure -text] 4]
      .dlgerr.haut.txt configure -text "$oldtext\n\n[lindex $Text $GDefs(Lang)]$Extra"

   } else {

      toplevel .dlgerr -class Dialog
      wm title .dlgerr [lindex $Lbl(ERROR) $GDefs(Lang)]
      wm protocol .dlgerr WM_DELETE_WINDOW { }
      if { [winfo exists $Master] } {
         wm geom .dlgerr +[expr [winfo rootx $Master]+50]+[expr [winfo rooty $Master]+50]
      }

      # ----- Afficher le frame du haut qui va contenir le message

      frame .dlgerr.haut -relief raised -bd 1
      pack .dlgerr.haut -side top -expand true -fill x

      label .dlgerr.haut.bitmap -image DIALOG_ERROR
      message .dlgerr.haut.txt -aspect 1000 -text "[lindex $Text $GDefs(Lang)]$Extra"
      pack .dlgerr.haut.bitmap -side left -padx 10 -pady 10
      pack .dlgerr.haut.txt -padx 10 -pady 10

      # ----- Afficher le frame du bas qui va contenir le bouton retour

      button .dlgerr.ok -text [lindex $Lbl(Ok) $GDefs(Lang)] -command "catch { grab [lindex $previous 0] } ; destroy .dlgerr; set button 1" -bd 1
      pack .dlgerr.ok -side bottom -ipadx 10 -fill x
      update idletasks
      grab .dlgerr
      tkwait variable button
   }
}

#----------------------------------------------------------------------------
# Nom      : <Dialog::ErrorListing>
# Creation : Mars 2008 - J.P. Gauthier - CMC/CMOE -
#
# But      : Afficher un message d'erreur avec un liste deroulante pour l'info.
#
# Parametres :
#    <Master> : Fenetre toplevel auquel l'aide est reliee.
#    <Text>   : Texte a afficher.
#    <List>   : Text a ajouter a la liste deroulante
#
# Remarques :
#    Aucune.
#
#----------------------------------------------------------------------------

proc Dialog::ErrorListing { Master Text List } {
   global GDefs
   variable Lbl

   uplevel 1 "Log::Print ERROR \{[lindex $Text $GDefs(Lang)]\}"

   if { ![info exists ::tk_version] } {
      return
   }

   set previous [grab current]

   if { [winfo exists .dlgerrlist] == 1} {

      .dlgerrlist.haut.txt configure -text [lindex $Text $GDefs(Lang)]
      .dlgerrlist.bas.list insert end $List

   } else {

      toplevel .dlgerrlist -class Dialog
      wm title .dlgerrlist [lindex $Lbl(ERROR) $GDefs(Lang)]
      wm protocol .dlgerrlist WM_DELETE_WINDOW { }
      if { [winfo exists $Master] } {
         wm geom .dlgerrlist +[expr [winfo rootx $Master]+50]+[expr [winfo rooty $Master]+50]
      }

      #----- Afficher le frame du haut qui va contenir le message

      frame .dlgerrlist.haut -relief raised -bd 1
         label .dlgerrlist.haut.bitmap -image DIALOG_ERROR
         message .dlgerrlist.haut.txt -aspect 1000 -text [lindex $Text $GDefs(Lang)]
         pack .dlgerrlist.haut.bitmap -side left -padx 10 -pady 10
         pack .dlgerrlist.haut.txt -padx 10 -pady 10
      pack .dlgerrlist.haut -side top -fill x

      frame .dlgerrlist.bas
         scrollbar .dlgerrlist.bas.scroll -command ".dlgerrlist.bas.list yview" -bd 1 -width 10
         text .dlgerrlist.bas.list -relief sunken -yscrollcommand ".dlgerrlist.bas.scroll set" \
            -exportselection 0 -background $GDefs(ColorLight) -bd 1
         pack .dlgerrlist.bas.list -side left -fill both -expand True
         pack .dlgerrlist.bas.scroll -side left -fill y
      pack  .dlgerrlist.bas -side top -fill both -expand True

      .dlgerrlist.bas.list insert 0.0 $List

      #----- Afficher le frame du bas qui va contenir le bouton retour

      button .dlgerrlist.ok -text [lindex $Lbl(Ok) $GDefs(Lang)] -command "catch { grab [lindex $previous 0] } ; destroy .dlgerrlist" -bd 1
      pack .dlgerrlist.ok -side bottom -ipadx 10 -fill x
      update idletasks
      grab .dlgerrlist
   }
}

#----------------------------------------------------------------------------
# Nom      : <Dialog::Info>
# Creation : Mai 2000 - J.P. Gauthier - CMC/CMOE -
#
# But      : Afficher un message d'information.
#
# Parametres :
#    <Master> : Fenetre toplevel auquel l'aide est reliee.
#    <Text>   : Texte bilingue a afficher.
#    <Extra>  : Texte supplementaire.
#
# Remarques :
#    Aucune.
#
#----------------------------------------------------------------------------

proc Dialog::Info { Master Text { Extra "" } } {
   global GDefs
   variable Lbl

   uplevel 1 "Log::Print INFO \{[lindex $Text $GDefs(Lang)]$Extra\}"

   if { ![info exists ::tk_version] } {
      return
   }

   if { [winfo exists .dlginfo] == 1} {

      set oldtext [lindex [.dlginfo.haut.txt configure -text] 4]
      .dlginfo.haut.txt configure -text "$oldtext\n\n[lindex $Text $GDefs(Lang)]$Extra"

   } else {

      toplevel .dlginfo -class Dialog
      wm title .dlginfo Info
      wm protocol .dlginfo WM_DELETE_WINDOW { }
      if { [winfo exists $Master] } {
         wm transient .dlginfo $Master
         wm geom .dlginfo +[expr [winfo rootx $Master]+50]+[expr [winfo rooty $Master]+50]
      }

      # ----- Afficher le frame du haut qui va contenir le message

      frame .dlginfo.haut -relief raised -bd 1
      pack .dlginfo.haut -side top -expand true -fill x

      label .dlginfo.haut.bitmap -image DIALOG_INFO
      message .dlginfo.haut.txt -aspect 1000 -text "[lindex $Text $GDefs(Lang)]$Extra"
      pack .dlginfo.haut.bitmap .dlginfo.haut.txt -side left -padx 20 -pady 20

      # ----- Afficher le frame du bas qui va contenir le bouton retour

      button .dlginfo.ok -text [lindex $Lbl(Ok) $GDefs(Lang)] -command "destroy .dlginfo" -bd 1
      pack .dlginfo.ok -side bottom -ipadx 10 -fill x
      update idletasks
      grab .dlginfo
   }
}

#----------------------------------------------------------------------------
# Nom      : <Dialog::Wait>
# Creation : Mai 2000 - J.P. Gauthier - CMC/CMOE -
#
# But      : Afficher un message d'information sur un processue en cours.
#
# Parametres :
#    <Master>  : Fenetre toplevel auquel l'aide est reliee.
#    <Text>    : Texte bilingue a afficher.
#    <Extra>   : Texte supplementaire.
#
# Remarques :
#    Aucune.
#
#----------------------------------------------------------------------------

proc Dialog::Wait { Master Text { Extra "" } } {
   global GDefs

   uplevel 1 "Log::Print INFO \{[lindex $Text $GDefs(Lang)]$Extra\}"

   if { ![info exists ::tk_version] } {
      return
   }

   if { [winfo exists .dlgwait]==1 } {

       set oldtext [lindex [.dlgwait.fr.txt configure -text] 4]
      .dlgwait.fr.txt configure -text "$oldtext\n\n[lindex $Text $GDefs(Lang)]$Extra"

   } else {

      toplevel .dlgwait -class Dialog
      wm overrideredirect .dlgwait true
      if { [winfo exists $Master] } {
         wm transient .dlgwait $Master
         wm geom .dlgwait +[expr [winfo rootx $Master]+[winfo width $Master]/2-300]+[expr [winfo rooty $Master]+[winfo height $Master]/2-50]
      }

      frame .dlgwait.fr -relief raised -bd 1
         label .dlgwait.fr.bitmap -image DIALOG_WAIT
         message .dlgwait.fr.txt -aspect 1000 -text "[lindex $Text $GDefs(Lang)]$Extra"
         pack .dlgwait.fr.bitmap .dlgwait.fr.txt -side left -ipadx 10 -ipady 10
      pack .dlgwait.fr

      update idletasks

      grab .dlgwait
   }
   update idletasks
}

#----------------------------------------------------------------------------
# Nom      : <Dialog::WaitDestroy>
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

proc Dialog::WaitDestroy { { Ask False } } {
   variable Lbl

   if { ![info exists ::tk_version] } {
      return True
   }

   if { [winfo exists .dlgwait] } {

      if { $Ask } {
         if { [Dialog::Default . 400 INFO $Lbl(Running) "" 1 $Lbl(Continue) $Lbl(Cancel)] } {
            return False
         }
      }
      destroy .dlgwait
      update idletasks
   }
   return True
}

#----------------------------------------------------------------------------
# Nom      : <Dialog::Message>
# Creation : Juillet 98 - J.P. Gauthier - CMC/CMOE -
#
# But      : Afficher un message dynamique standard.
#
# Parametres  :
#    <Master> : Fenetre toplevel auquel l'aide est reliee.
#    <Text>   : Texte a afficher.
#    <Extra>   : Texte supplementaire.
#
# Remarques :
#    Aucune.
#
#----------------------------------------------------------------------------

proc Dialog::Message { Master Text { Extra "" } } {
   global GDefs

   uplevel 1 "Log::Print INFO \{[lindex $Text $GDefs(Lang)]$Extra\}"

   if { ![info exists ::tk_version] } {
      return
   }

   if { [winfo exists .dlgmsg] } {

      .dlgmsg.txt config -text $Text
      update idletasks
   } else {

      toplevel .dlgmsg -class Dialog
      wm title .dlgmsg "Message ([wm title $Master])"
      if { [winfo exists $Master] } {
         wm transient .dlgmsg $Master
         wm geom .dlgmsg +[expr [winfo rootx $Master]+50]+[expr [winfo rooty $Master]+50]
      }

      #----- Creer et affiche le message.
      frame .dlgmsg.msg  -relief raised -bd 1
         label .dlgmsg.msg.bitmap -image DIALOG_QUESTION
         message .dlgmsg.msg.txt -aspect 1000 -text "[lindex $Text $GDefs(Lang)]$Extra"
         pack .dlgmsg.msg.bitmap .dlgmsg.msg.txt -side left -ipadx 10 -ipady 10
      pack .dlgmsg.msg

      .dlgmsg config -cursor watch
      update idletasks
   }
   grab .dlgmsg
}

#----------------------------------------------------------------------------
# Nom      : <Dialog::Get>
# Creation : Octobre 2009 - J.P. Gauthier - CMC/CMOE -
#
# But      : Recuperer une valeur unique.
#
# Parametres  :
#    <Master> : Fenetre toplevel auquel l'aide est reliee.
#    <Title>  : Titre de la fenetre
#    <Text>   : Texte a afficher.
#    <Var>    : Variable
#    <File>   : Boutton de selection de fichier
#
# Retour      :
#    <Val>    : Valeur saisie
#
# Remarques :
#    Aucune.
#
#----------------------------------------------------------------------------

proc Dialog::Get { Master Title Text { Var "" } { File False } } {
   global GDefs
   global gettervalue
   variable Lbl

   toplevel .dlgget -class Dialog
   wm title .dlgget [lindex $Title $GDefs(Lang)]
   wm transient .dlgget $Master
   wm geom .dlgget +[expr [winfo rootx $Master]+50]+[expr [winfo rooty $Master]+50]

   frame .dlgget.msg -relief raised -bd 1
      label .dlgget.msg.bitmap -image DIALOG_QUESTION
      message .dlgget.msg.txt -aspect 1000 -text [lindex $Text $GDefs(Lang)]
      pack .dlgget.msg.bitmap -side left -ipadx 10 -ipady 10
      pack .dlgget.msg.txt -side left -fill x -expand True -ipadx 10 -ipady 10
   pack .dlgget.msg -side top  -fill x -expand True

   if { $Var!="" } {
      entry .dlgget.in -textvariable $Var  -relief flat -bd 1 -bg $GDefs(ColorLight)
   } else {
      entry .dlgget.in -relief flat -bd 1 -bg $GDefs(ColorLight)
   }

   if { $File } {
      button .dlgget.in.file -image OPEN -height 12  -command { .dlgget.in delete 0 end ; .dlgget.in insert 0 [FileBox::Create . "" Load [list $FileBox::Type(ALL)]] }
      pack .dlgget.in.file -side right -fill y
   }
   pack .dlgget.in -side top -fill x -ipady 2
   bind .dlgget.in <Return>  { .dlgget.ok invoke }

   set gettervalue ""
   frame .dlgget.cmd
      button .dlgget.ok -text [lindex $Lbl(Ok) $GDefs(Lang)] -command { set gettervalue [.dlgget.in get] } -bd 1 -foreground green
      button .dlgget.cancel -text [lindex $Lbl(Cancel) $GDefs(Lang)] -command { set gettervalue "" } -bd 1
      pack .dlgget.ok .dlgget.cancel -side left -fill x  -expand True
   pack .dlgget.cmd -side top -fill x

   focus .dlgget.in
   update idletasks
   grab .dlgget

   tkwait variable gettervalue
   destroy .dlgget
   return $gettervalue
}

#----------------------------------------------------------------------------
# Nom      : <Dialog::Give>
# Creation : Septembre 2010 - J.P. Gauthier - CMC/CMOE -
#
# But      : Aficher une valeur de retour + Presse-papier.
#
# Parametres  :
#    <Master> : Fenetre toplevel auquel l'aide est reliee.
#    <Title>  : Titre de la fenetre
#    <Text>   : Texte a afficher.
#    <Info>   : Information
#
# Retour      :
#
# Remarques :
#    Aucune.
#
#----------------------------------------------------------------------------

proc Dialog::Give { Master Title Text Info } {
   global GDefs
   global gettervalue
   variable Lbl

   toplevel .dlggive -class Dialog
   wm title .dlggive [lindex $Title $GDefs(Lang)]
   wm transient .dlggive $Master
   wm geom .dlggive +[expr [winfo rootx $Master]+50]+[expr [winfo rooty $Master]+50]

   frame .dlggive.msg -relief raised -bd 1
      label .dlggive.msg.bitmap -image DIALOG_CLIP
      message .dlggive.msg.txt -aspect 1000 -text [lindex $Text $GDefs(Lang)]
      pack .dlggive.msg.bitmap -side left -ipadx 10 -ipady 10
      pack .dlggive.msg.txt -side left -fill x -expand True -ipadx 10 -ipady 10
   pack .dlggive.msg -side top  -fill x -expand True

   text .dlggive.out -relief flat -bd 1 -bg $GDefs(ColorLight) -height 3
   .dlggive.out insert 0.0 $Info
   pack .dlggive.out -side top -fill both -ipady 2

   frame .dlggive.cmd
      button .dlggive.ok -text [lindex $Lbl(Ok) $GDefs(Lang)] -command { destroy .dlggive } -bd 1 -foreground green
      button .dlggive.copy -text [lindex $Lbl(Clip) $GDefs(Lang)] -command { clipboard clear; clipboard append [.dlggive.out get 0.0 end] } -bd 1
      pack .dlggive.copy .dlggive.ok -side left -fill x  -expand True
   pack .dlggive.cmd -side top -fill x

   focus .dlggive.out
   update idletasks
}

#----------------------------------------------------------------------------
# Nom      : <Dialog::Text>
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

proc Dialog::Text { Id Title File Width Height } {
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
         button $Id.command.print -image PRINT -relief raised -bd 1 -command "PrintBox::Create $Id.file.text PRINT Dialog"
         button $Id.command.save -image OPENDOC -relief raised -bd 1 -command "Dialog::TextSave $Id.file.text \[FileBox::Create $Id \"\" Save \[list \$FileBox::Type(TXT)\]\]"
         label $Id.command.line -relief raised -bd 1 -width 10 -textvariable Dialog::Data(Cursor$Id) -bg $GDefs(ColorLight)
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


      Bubble::Create $Id.command.print $Bubble(Print)
      Bubble::Create $Id.command.save  $Bubble(Save)

      bind $Id.file.text <Any-KeyRelease> "set Dialog::Data(Cursor$Id) \[$Id.file.text index insert\]"
      bind $Id.file.text <Any-Button> "set Dialog::Data(Cursor$Id) \[$Id.file.text index insert\]"
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
      set Dialog::Data(Nb$Id) [$Id.file.text index end]
   }

   #----- Traiter les directives si elles existent

   if { [file exists $File.dir] } {

      set f [open $File.dir]
      while { [gets $f ligne] >= 0 } {
         Dialog::TextSearch $Id.file.text [lindex $ligne 0] [lindex $ligne 1] [lindex $ligne 2]
      }
      close $f
   }

   return $Id.file.text
}

proc Dialog::TextSave { Text File } {

   if { $File=="" } {
      return
   }

   set f [open $File w]
   puts $f [$Text get 0.0 end]
   close $f
}

proc Dialog::PrintCommand { Widget } {
   variable Data

   exec echo [$Widget get 0.0 end] > /tmp/[pid].txt
   PrintBox::PrintTXT /tmp/[pid].txt
   file delete /tmp/[pid].txt

   PrintBox::Destroy
}

proc Dialog::TextSearch { Widget String Tag args } {

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
