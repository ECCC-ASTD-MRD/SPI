#===============================================================================
# Environnement Canada
# Centre Meteorologique Canadian
# 2100 Trans-Canadienne
# Dorval, Quebec
#
# Projet   : Widget de librairie.
# Fichier  : InfoFrame.tk
# Version  : 1.0 ($Revision: 1.3 $)
# Creation : Octobre 2000 - J.P. Gauthier - CMC/CMOE
#
# Description:
#    Definitions d'un frame d'information contenant un message d'execution et
#    un pourcentage d'execution
#
# Fonctions:
#   InfoFrame::Create    { Frame Msg Size }
#   InfoFrame::Incr      { Frame Value args }
#   InfoFrame::Msg        { Frame Msg }
#   InfoFrame::Set100    { Frame Value }
#
# Modifications :
#
#   Nom         : -
#   Date        : -
#   Description : -
#
#===============================================================================

package provide InfoFrame 1.0

proc IdInfoFrame { Show } {

   if { $Show } {
      puts "(INFO) Loading Standard CMC/CMOE Widget Package InfoFrame Version 1.0"
   }
}

namespace eval InfoFrame {
   global GEN
   variable Resources
   variable Data
   variable Bubble

   #----- Definitions des resources

   catch {
      set Resources(Background) $GDefs(ColorLight)
      set Resources(BarColor)   yellow
      set Resources(IcoInfo)    @$GDefs(Dir)/Resources/Bitmap/combobox_down.ico
   }

   #----- Definitions des variables internes

   set Data(MsgMax)  50
   set Data(MsgLen)  35

   #----- Definition des bulles d'aides

   set Bubble(List)  { "Liste des messages passes" "Past message list" }
   set Bubble(Msg)   { "Description du travail en cours" "Job being done" }
   set Bubble(Gage)  { "Pourcentage d'execution du travail" "Job percentage done" }
}

#----------------------------------------------------------------------------
# Nom      : <InfoFrame::Create>
# Creation : Octobre 2000 - J.P. Gauthier - CMC/CMOE
#
# But      : Creer le frame d'information.
#
# Parametres :
#  <Frame>   : Frame a creer
#  <Msg>     : Variable du message
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

proc InfoFrame::Create { Frame Msg Size args } {
   global GDefs
   variable Resources
   variable Data
   variable Bubble

   eval frame $Frame $args
      menubutton $Frame.lst -relief flat -bd 0 -menu $Frame.lst.line -bitmap $Resources(IcoInfo)
      label $Frame.lbl -textvariable $Msg -relief sunken -bd 1 -bg $Resources(Background) -width 20 -anchor w
      canvas $Frame.gage -width 100 -height 17 -bd 1 -relief sunken -bd 1
      pack $Frame.lst -side left -fill y
      pack $Frame.lbl -side left -fill x -expand true
      pack $Frame.gage -side left -padx 1 -pady 1

      $Frame.gage create rectangle 1 1 1 [winfo reqheight $Frame.gage] -outline lightgray -fill $Resources(BarColor) -tags BAR

   #----- Liste des commandes passees

   menu $Frame.lst.line -tearoff 0
      frame $Frame.lst.line.list
         listbox $Frame.lst.line.list.box -relief sunken -bd 1 -width $Data(MsgLen) -height 5 -bg $Resources(Background) \
            -yscrollcommand "$Frame.lst.line.list.scroll set"
         scrollbar $Frame.lst.line.list.scroll -bd 1 -width 10 -command "$Frame.lst.line.list.box yview"
         pack $Frame.lst.line.list.box  $Frame.lst.line.list.scroll -side left -fill both
      pack $Frame.lst.line.list -side top -fill both

   set Data(100$Frame)  1
   set Data(Size$Frame) $Size
   set Data(Var$Frame)  $Msg

   Bubble::Create $Frame.lbl  [lindex $Bubble(Msg)  $GDefs(Lang)]
   Bubble::Create $Frame.gage [lindex $Bubble(Gage) $GDefs(Lang)]
   Bubble::Create $Frame.lst  [lindex $Bubble(List) $GDefs(Lang)]
}

#----------------------------------------------------------------------------
# Nom      : <InfoFrame::Incr>
# Creation : Octobre 2000 - J.P. Gauthier - CMC/CMOE
#
# But      : Incremente le pourcentage de la job en cours.
#
# Parametres :
#  <Frame>   : Frame d'information
#  <Value>   : Valeur de l'increment
#  <args>    : Texte d'information
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

proc InfoFrame::Incr { Frame Value args } {
   variable Data
   variable Resources

   if { ![winfo exists $Frame] } {
      return
   }

   if { $args != "" } {
      InfoFrame::Msg $Frame [join $args]
   }

   if { $Value == 0 } {
      set Data(XXX$Frame) 0
   } else {
      incr Data(XXX$Frame) $Value
   }

   #----- Calcul du pourcentage actuel

   if { $Data(XXX$Frame) > $Data(100$Frame) } {
      set Data(XPC$Frame) 1.0
   } else {
      set Data(XPC$Frame) [expr double($Data(XXX$Frame))/$Data(100$Frame)]
   }

   #----- Calcul de la longueur de la barre

   set xpc [expr int($Data(Size$Frame)*$Data(XPC$Frame))]
   $Frame.gage coords BAR 1 1 $xpc [winfo reqheight $Frame.gage]
   update idletasks
}

#----------------------------------------------------------------------------
# Nom      : <InfoFrame::Msg>
# Creation : Fevrier 2001 - J.P. Gauthier - CMC/CMOE
#
# But      : Changer le message d'information.
#Data
# Parametres :
#  <Frame>   : Frame d'information
#  <Msg>     : Texte d'information
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

proc InfoFrame::Msg { Frame Msg } {
   variable Data

   if { ![winfo exists $Frame] } {
      return
   }

   if { $Msg != "" } {

      #----- Verifier le nombre de message dans la liste

      eval set $Data(Var$Frame) \"$Msg\"
      if { [$Frame.lst.line.list.box index end] > $Data(MsgMax) } {
         $Frame.lst.line.list.box delete end
      }

      #----- Verifier la dimension de la boite

      set len [string length $Msg]
      if { $len > $Data(MsgLen) } {
         set Data(MsgLen) $len
         $Frame.lst.line.list.box configure -width $len
      }

      #----- Inserer le messgae
      eval $Frame.lst.line.list.box insert 0 \"$Msg\"
   }
   update idletasks
}

#----------------------------------------------------------------------------
# Nom      : <InfoFrame::Set100>
# Creation : Octobre 2000 - J.P. Gauthier - CMC/CMOE
#
# But      : Initialiser la valeur de 100 % et de 0 %.
#
# Parametres :
#  <Frame>   : Frame d'information
#  <Value>   : Valeur du 100 %
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

proc InfoFrame::Set100 { Frame Value } {
   variable Data

   set Data(100$Frame)   $Value
   set Data(XXX$Frame)   0
   set Data(XPC$Frame)   0
}

