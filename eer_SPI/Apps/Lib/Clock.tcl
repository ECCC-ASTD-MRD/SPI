#===============================================================================
# Environnement Canada
# Centre Meteorologique Canadian
# 2100 Trans-Canadienne
# Dorval, Quebec
#
# Projet    : Interface de selection de date.
# Fichier   : Clock.tk
# Creation  : Mars 2001 - J.P. Gauthier - CMC/CMOE
#
# Description: Permet grace a une interface de slelectionner l'heure.
#
# Fonctions:
#   Clock::Create  { Frame Label Var args }
#   Clock::Disable { Frame }
#   Clock::Enable  { Frame }
#   Clock::Incr    { Widget Inc Fill }
#   Clock::Check   { Val Fill Min Max }
#
# Remarques :
#   Aucune
#
#===============================================================================

package provide Clock 1.0

catch { SPI::Splash "Loading Widget Package Clock 1.0" }

namespace eval Clock {
   global GDefs
   variable Data
   variable Resources

   catch {
      set Resources(Up)   $GDefs(Dir)/share/bitmap/up.xbm
      set Resources(Down) $GDefs(Dir)/share/bitmap/down.xbm
   }
}

#----------------------------------------------------------------------------
# Nom      : <Clock::Create>
# Creation : Mars 2001 - J.P. Gauthier - CMC/CMOE -
#
# But      : Affiche une interface de selection d'heure.
#
# Parametres  :
#   <Frame>   : Identificateur du Frame.
#   <Label>   : Libelle
#   <Var>     : Variable contenant les heures
#   <args>    : Variable contenant les minutes.
#
# Retour      :
#
# Remarques :
#    Aucune.
#
#----------------------------------------------------------------------------

proc Clock::Create { Frame Label Var args } {
   global GDefs
   variable Data
   variable Resources

   set Data(HH$Frame) $Var

   if { $args!="" } {
      set Data(MM$Frame) $args
   } else {
      set Data(MM$Frame) 0
   }

   upvar #0 $Data(HH$Frame) hh
   upvar #0 $Data(MM$Frame) mm

   frame $Frame
      if { $Label!="" } {
         label $Frame.l -relief flat -anchor w -text $Label
         pack $Frame.l -side left
      }
      entry $Frame.hh -width 2 -bd 1 -bg $GDefs(ColorLight) -textvariable $Data(HH$Frame) -state disabled \
         -disabledbackground $GDefs(ColorLight) -disabledforeground black
      frame $Frame.inc -relief sunken -bd 1
         button $Frame.inc.up -bitmap @$Resources(Up) -bd 1 -command "Clock::Incr $Frame 1 1"
         button $Frame.inc.down -bitmap @$Resources(Down) -bd 1 -command "Clock::Incr $Frame -1 1"
         pack $Frame.inc.up $Frame.inc.down -side top -fill y -expand true
      pack $Frame.hh -side left
      pack $Frame.inc -side left -fill y -pady 1

   #----- A la demande generale, le menu des heures

   menu $Frame.hh.lst -tearoff 0 -bd 1
   foreach hour { 00 01 02 03 04 05 06 07 08 09 10 11 12 13 14 15 16 17 18 19 20 21 22 23 } {
      $Frame.hh.lst add command -label $hour -command "set $Data(HH$Frame) $hour"
   }

   #----- Ya tu des minutes ????

   if { $args!="" } {
      label $Frame.sep -text ":"
      entry $Frame.mm -width 2 -bd 1 -bg $GDefs(ColorLight) -textvariable $Data(MM$Frame)
      pack $Frame.sep $Frame.mm -after $Frame.hh -side left

      set mm [Clock::Check $mm 1 0 59]

      bind $Frame.mm <Any-KeyRelease>   "Clock::Incr $Frame 0 0"
      bind $Frame.mm <ButtonRelease-1>  "$Frame.mm selection range 0 end"
   }

   set hh [Clock::Check $hh 1 0 23]

   bind $Frame.hh <Any-KeyRelease>   "Clock::Incr $Frame 0 0"
#   bind $Frame.hh <ButtonRelease-1>  "$Frame.hh selection range 0 end"
   bind $Frame.hh <ButtonRelease-1>  "tk_popup $Frame.hh.lst \[winfo rootx $Frame\] \[expr \[winfo rooty $Frame\]+\[winfo height $Frame\]\]"
}

#----------------------------------------------------------------------------
# Nom      : <Clock::Disable>
# Creation : Juillet 2010 - E. Legault-Ouellet - CMC/CMOE
#
# But      : Disable le widget.
#
# Parametres  :
#   <Frame>   : Identificateur du Frame.
#
# Retour      :
#
# Remarques :
#    Aucune.
#
#----------------------------------------------------------------------------

proc Clock::Disable { Frame } {
   global GDefs

   if { ![winfo exists $Frame] } {
      return
   }

   $Frame.inc.up configure -state disabled
   $Frame.inc.down configure -state disabled

   bind $Frame.hh <ButtonRelease-1> ""
   if { [winfo exists $Frame.mm] } {
      $Frame.mm configure -state disabled
   }
   $Frame.hh configure -disabledbackground $GDefs(ColorFrame) -disabledforeground $GDefs(ColorOff)
}

#----------------------------------------------------------------------------
# Nom      : <Clock::Enable>
# Creation : Juillet 2010 - E. Legault-Ouellet - CMC/CMOE
#
# But      : Enable le widget.
#
# Parametres  :
#   <Frame>   : Identificateur du Frame.
#
# Retour      :
#
# Remarques :
#    Aucune.
#
#----------------------------------------------------------------------------

proc Clock::Enable { Frame } {
   global GDefs

   if { ![winfo exists $Frame] } {
      return
   }

   $Frame.inc.up configure -state normal
   $Frame.inc.down configure -state normal

   bind $Frame.hh <ButtonRelease-1> "tk_popup $Frame.hh.lst \[winfo rootx $Frame\] \[expr \[winfo rooty $Frame\]+\[winfo height $Frame\]\]"
   if { [winfo exists $Frame.mm] } {
      $Frame.mm configure -state normal
   }
   $Frame.hh configure -disabledbackground $GDefs(ColorLight) -disabledforeground black
}

#----------------------------------------------------------------------------
# Nom      : <Clock::Incr>
# Creation : Mars 2001 - J.P. Gauthier - CMC/CMOE -
#
# But      : Incrementre l'heure ou les minutes.
#
# Parametres  :
#   <Frame>  : Identificateur du parent.
#   <Incr>    : Increment.
#   <Fill>    : Pad avec 0
#
# Retour      :
#
# Remarques :
#    Aucune.
#
#----------------------------------------------------------------------------

proc Clock::Incr { Widget Inc Fill } {
   variable Data

   upvar #0 $Data(HH$Widget) hh
   upvar #0 $Data(MM$Widget) mm

   set wdg [focus]

   if { "$wdg" == "$Widget.hh" || $Data(MM$Widget)==0 } {

      #------ Recuperer l'heure courante

      set h [string trimleft $hh 0]
      if { $h=="" } {
         set h 0
      }

      #------ Effectuer l'increment

      incr h $Inc
      set hh [Clock::Check $h $Fill 0 23]

   } elseif  { "$wdg" == "$Widget.mm" } {

      #------ Recuperer la minute courante

      set m [string trimleft $mm 0]
      if { $m=="" } {
         set m 0
      }

      #------ Effectuer l'increment

      incr m $Inc
      set mm [Clock::Check $m $Fill 0 59]
   }
}

#----------------------------------------------------------------------------
# Nom      : <Clock::Check>
# Creation : Mars 2001 - J.P. Gauthier - CMC/CMOE -
#
# But      : Formater les hueres ou les minutes.
#
# Parametres  :
#   <Val>     : Valeur a formater.
#   <Fill>    : Pad avec 0
#   <Min>     : Minimum acceptable.
#   <Max>     : Maximum acceptable.
#
# Retour      :
#
# Remarques :
#    Aucune.
#
#----------------------------------------------------------------------------

proc Clock::Check { Val Fill Min Max } {

   set Val [string trimleft $Val 0]
   if { $Val=="" } {
      set Val 0
   }

   if { $Val < $Min } {
      set Val $Max
   }
   if { $Val > $Max } {
      set Val $Min
   }

   if { $Val< 10 && $Fill } {
      return "0$Val"
   } else {
      return  $Val
   }
}
