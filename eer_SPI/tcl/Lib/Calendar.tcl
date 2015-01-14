#===============================================================================
# Environnement Canada
# Centre Meteorologique Canadian
# 2100 Trans-Canadienne
# Dorval, Quebec
#
# Projet    : Interface de selection de date.
# Fichier   : Calendar.tk
# Creation  : Mars 2001 - J.P. Gauthier - CMC/CMOE
#
# Description: Permet grace a une interface de type calendrier de selectionner
#              une date.
#
# Fonctions:
#
#     Calendar::Create      { Frame Label Var Width { Cmd "" } }
#     Calendar::Decrease    { Frame }
#     Calendar::Increase    { Frame }
#     Calendar::Enable      { Frame }
#     Calendar::Disable     { Frame }
#     Calendar::Invoke      { Frame Second }
#     Calendar::Update      { Frame  }
#     Calendar::Select      { Frame { Day "" } }
#     Calendar::Set         { Frame Sec args }
#     Calendar::UnSelect    { Day }
#     Calendar::CheckFeb    { Year }
#     Calendar::MonthLength { Year Month }
#
# Remarques :
#   Aucune
#
#===============================================================================

package provide Calendar 1.0

catch { SPI::Splash "Loading Widget Package Calendar 1.0" }

namespace eval Calendar {
   global GDefs
   variable Lbl
   variable Data
   variable Resources

   set Lbl(Day)     " S M T W T F S"

   set Data(Second) [clock seconds]
   set Data(Year)   [clock format $Data(Second) -format "%Y" -timezone :UTC]
   set Data(Month)  [string trimleft [clock format $Data(Second) -format "%m" -timezone :UTC] 0]
   set Data(Day)    [clock format $Data(Second) -format "%d" -timezone :UTC
   set Data(Result) ""

   catch {
      set Resources(MDec) $GDefs(Dir)/share/bitmap/MDec.xbm
      set Resources(YDec) $GDefs(Dir)/share/bitmap/YDec.xbm
      set Resources(MInc) $GDefs(Dir)/share/bitmap/MInc.xbm
      set Resources(YInc) $GDefs(Dir)/share/bitmap/YInc.xbm
   }
}

#----------------------------------------------------------------------------
# Nom      : <Calendar::Set>
# Creation : Mars 2001 - J.P. Gauthier - CMC/CMOE -
#
# But      : Ajuster la date du calendrier.
#
# Parametres  :
#   <Frame>   : Identificateur du Frame.
#   <Sec>     : Seconde a appliquer au calendrier
#
# Retour      :
#
# Remarques :
#
#----------------------------------------------------------------------------

proc Calendar::Set { Frame Sec args } {
   global GDefs
   variable Data

   set Data(Second) $Sec
   set Data(Date$Frame) [DateStuff::StringDateOnlyFromSeconds $Data(Second) $GDefs(Lang)]
}

#----------------------------------------------------------------------------
# Nom      : <Calendar::Create>
# Creation : Mars 2001 - J.P. Gauthier - CMC/CMOE -
#
# But      : Affiche une interface de calendrier.
#
# Parametres  :
#   <Frame>   : Identificateur du Frame.
#   <Label>   : Libelle
#   <Var>     : Variable contenant les secondes
#   <Width>   : Largeur du widget (-1=fill)
#
# Retour      :
#
# Remarques :
#
#----------------------------------------------------------------------------

proc Calendar::Create { Frame Label Var Width { Cmd "" } } {
   global GDefs
   variable Data

   eval set sec \$$Var
   if { $sec!="" && [string is integer $sec] } {
       set Data(Second) $sec
   }
   set Data(Date$Frame) [DateStuff::StringDateOnlyFromSeconds $Data(Second) $GDefs(Lang)]

   frame $Frame
      if { $Label!="" } {
         label $Frame.l -relief flat -anchor w -text $Label
         pack $Frame.l -side left
      }
      entry $Frame.e -relief sunken -bd 1 -width $Width -textvariable Calendar::Data(Date$Frame) -bg $GDefs(ColorLight)\
         -disabledbackground $GDefs(ColorLight) -disabledforeground black
      button $Frame.b -relief groove -bd 2 -bitmap @$GDefs(Dir)/share/bitmap/down.xbm -width 7 \
         -command "set $Var \[Calendar::Invoke $Frame \$$Var\]; $Cmd"
      pack $Frame.b -side left -fill y

      bind $Frame.e <Return>  "set $Var \[clock scan \$Calendar::Data(Date$Frame) -timezone :UTC]; set Data(Date$Frame) \[DateStuff::StringDateOnlyFromSeconds \$$Var $GDefs(Lang)\]; $Cmd"

   if { $Width==-1 } {
      pack $Frame.e -side left -fill both -expand true
   } else {
      pack $Frame.e -side left -fill y
   }

   trace variable $Var w "Calendar::Set $Frame \$$Var"
}

#----------------------------------------------------------------------------
# Nom      : <Calendar::Disable>
# Creation : Juillet 2010 - E. Legault-Ouellet - CMC/CMOE -
#
# But      : Disable le widget.
#
# Parametres  :
#   <Frame>   : Identificateur du Frame.
#
# Retour      :
#
# Remarques :
#
#----------------------------------------------------------------------------

proc Calendar::Disable { Frame } {
   global GDefs

   if { ![winfo exists $Frame] } {
      return
   }

   $Frame.e configure -state disabled -disabledbackground $GDefs(ColorFrame) -disabledforeground $GDefs(ColorOff)
   $Frame.b configure -state disabled
}

#----------------------------------------------------------------------------
# Nom      : <Calendar::Enable>
# Creation : Juillet 2010 - E. Legault-Ouellet - CMC/CMOE -
#
# But      : Affiche une interface de calendrier.
#
# Parametres  :
#   <Frame>   : Identificateur du Frame.
#
# Retour      :
#
# Remarques :
#
#----------------------------------------------------------------------------

proc Calendar::Enable { Frame } {
   global GDefs

   if { ![winfo exists $Frame] } {
      return
   }

   $Frame.e configure -state normal -disabledbackground $GDefs(ColorLight) -disabledforeground black
   $Frame.b configure -state normal
}

#----------------------------------------------------------------------------
# Nom      : <Calendar::Invoke>
# Creation : Mars 2001 - J.P. Gauthier - CMC/CMOE -
#
# But      : Active l'affichage de l'interface de calendrier.
#
# Parametres  :
#   <Frame>   : Identificateur du Frame.
#   <Second>  : Seconde a appliquer au calendrier
#
# Retour      :
#
# Remarques :
#
#----------------------------------------------------------------------------

proc Calendar::Invoke { Frame Second } {
   global GDefs
   variable Lbl
   variable Resources
   variable Data

   if { [winfo exists .cal] } {
      destroy .cal
      return
   }

   if { $Second!="" } {
      set Data(Second) $Second
   } else {
      set Data(Second) [clock seconds]
   }
   set Data(Year)   [clock format $Data(Second) -format %Y -timezone :UTC]
   set Data(Month)  [string trimleft [clock format $Data(Second) -format %m -timezone :UTC] 0]
   set Data(Day)    [clock format $Data(Second) -format %e -timezone :UTC]

   set Data(Result) $Data(Second)

   toplevel .cal
   wm geom  .cal =+[winfo rootx $Frame.b]+[expr [winfo rooty $Frame.b]+[winfo height $Frame.b]]
   wm overrideredirect .cal 1

   canvas .cal.date -width 140 -height 118 -cursor hand2 -relief raised -bd 1
   pack .cal.date -side top

   #----- Afficher le calendrier

   .cal.date create bitmap 2 2 -bitmap @$Resources(YDec) -anchor nw -tags YDEC
   .cal.date create bitmap 14 2 -bitmap @$Resources(MDec) -anchor nw -tags MDEC
   .cal.date create bitmap 130 2 -bitmap @$Resources(MInc) -anchor ne -tags MINC
   .cal.date create bitmap 142 2 -bitmap @$Resources(YInc) -anchor ne -tags YINC

   foreach tag "MDEC MINC YDEC YINC" {
      .cal.date bind $tag <Enter>           ".cal.date itemconf $tag -foreground yellow"
      .cal.date bind $tag <Leave>           ".cal.date itemconf $tag -foreground black"
   }

   .cal.date bind MDEC <ButtonRelease-1>  "Calendar::Decrease $Frame"
   .cal.date bind MINC <ButtonRelease-1>  "Calendar::Increase $Frame"
   .cal.date bind YDEC <ButtonRelease-1>  "incr Calendar::Data(Year) -1; Calendar::Update $Frame"
   .cal.date bind YINC <ButtonRelease-1>  "incr Calendar::Data(Year)  1; Calendar::Update $Frame"

   set x 20
   foreach letter $Lbl(Day) {
      .cal.date create text $x 22 -text $letter -anchor e -font $GDefs(Font)
      incr x 18
   }
   .cal.date create line 2 27 140 27 -fill black -width 1
   .cal.date create line 2 103 140 103 -fill black -width 1
   .cal.date create text 72 2 -text "" -anchor n -font $GDefs(Font) -tags CALDATE
   .cal.date create text 70 112 -text "" -font $GDefs(Font) -tags DATE

   Calendar::Update $Frame
   Calendar::Select $Frame $Data(Day)

   grab .cal
   tkwait variable Calendar::Data(Result)
   destroy .cal
   return $Calendar::Data(Result)
}

#----------------------------------------------------------------------------
# Nom      : <Calendar::Decrease>
# Creation : Mars 2001 - J.P. Gauthier - CMC/CMOE -
#
# But      : Diminuer le mois.
#
# Parametres  :
#   <Frame>   : Identificateur du Frame.
#
# Remarques :
#
#----------------------------------------------------------------------------

proc Calendar::Decrease { Frame } {
   variable Data

   incr Data(Month) -1
   if { $Data(Month) < 1 } {
      incr Data(Year) -1
      set Data(Month) 12
   }

   Calendar::Update $Frame
}

#----------------------------------------------------------------------------
# Nom      : <Calendar::Increase>
# Creation : Mars 2001 - J.P. Gauthier - CMC/CMOE -
#
# But      : Augmenter le mois.
#
# Parametres  :
#   <Frame>   : Identificateur du Frame.
#
# Remarques :
#
#----------------------------------------------------------------------------

proc Calendar::Increase { Frame } {
   variable Data

   incr Data(Month) 1
   if { $Data(Month) > 12 } {
      incr Data(Year) 1
      set Data(Month) 1
   }

   Calendar::Update $Frame
}

#----------------------------------------------------------------------------
# Nom      : <Calendar::Update>
# Creation : Mars 2001 - J.P. Gauthier - CMC/CMOE -
#
# But      : Mettre a jour le calendrier.
#
# Parametres  :
#   <Frame>   : Identificateur du Frame.
#
# Remarques :
#
#----------------------------------------------------------------------------

proc Calendar::Update { Frame  } {
   global GDefs
   variable Data

   set day  1
   set y    36

   set sec  [clock scan "$Data(Year)$Data(Month)$day" -format "%Y%m%d" -timezone :UTC]
   set date [clock format $sec -format "%B %Y" -timezone :UTC]

   .cal.date delete CALDAY
   .cal.date itemconf CALDATE -text $date

   bind .cal.date <ButtonRelease-3>  "Calendar::Select $Frame ; set Calendar::Data(Result) \$Calendar::Data(Result)"

   while { $day <= [Calendar::MonthLength $Data(Year) $Data(Month)] } {

      set d [clock format $sec -format "%w" -timezone :UTC]
      set x [expr 22+$d*18]

      .cal.date create text $x $y -text $day -font $GDefs(Font) -anchor e -tags "CALDAY CALDAY$day"
      .cal.date bind CALDAY$day <Enter>           "Calendar::Select $Frame $day"
      .cal.date bind CALDAY$day <Leave>           "Calendar::UnSelect $day"
      .cal.date bind CALDAY$day <ButtonRelease-1> { set Calendar::Data(Result) $Calendar::Data(Second) }

      incr sec 86400

      if { $d == 6 } {
         incr y 12
      }
      incr day 1
   }
}

#----------------------------------------------------------------------------
# Nom      : <Calendar::Select>
# Creation : Mars 2001 - J.P. Gauthier - CMC/CMOE -
#
# But      : Mettre en couleur la journee sous le curseur.
#
# Parametres :
#   <Frame>   : Identificateur du Frame.
#   <Day>    : Journee
#
# Remarques :
#
#----------------------------------------------------------------------------

proc Calendar::Select { Frame { Day "" } } {
   global GDefs
   variable Data

   if { $Day=="" } {
      set Data(Second) $Data(Result)
      set Data(Year)   [clock format $Data(Second) -format "%Y" -timezone :UTC]
      set Data(Month)  [clock format $Data(Second) -format "%m" -timezone :UTC]
      set Data(Day)    [clock format $Data(Second) -format "%d" -timezone :UTC]
   } else {
      set Data(Second) [clock scan "$Data(Year)/$Data(Month)/$Day" -format "%Y/%m/%d" -timezone :UTC]
   }
   set Data(Date$Frame) [DateStuff::StringDateOnlyFromSeconds $Data(Second) $GDefs(Lang)]

   .cal.date itemconf CALDAY$Day -fill yellow
   .cal.date itemconf DATE -text "$Data(Date$Frame)"
}

#----------------------------------------------------------------------------
# Nom      : <Calendar::UnSelect>
# Creation : Mars 2001 - J.P. Gauthier - CMC/CMOE -
#
# But      : Mettre en mode normale la journee sous le curseur.
#
# Parametres :
#   <Day>    : Journee
#
# Remarques :
#
#----------------------------------------------------------------------------

proc Calendar::UnSelect { Day } {

   .cal.date itemconf CALDAY$Day -fill black
   .cal.date itemconf DATE -text ""
}

#----------------------------------------------------------------------------
# Nom      : <Calendar::CheckFeb>
# Creation : Mars 2001 - J.P. Gauthier - CMC/CMOE -
#
# But      : Obtenir le nombre de jour du mois de Fevrier.
#
# Parametres :
#   <Year>   : Annee
#
# Retour     :
#   <Jour>   : Nombre de jour du mois
#
# Remarques :
#
#----------------------------------------------------------------------------

proc Calendar::CheckFeb { Year } {

   if { (($Year % 4 == 0) && ($Year % 100 != 0)) || ($Year % 400 == 0) } {
      return 29
   } else {
      return 28
   }
}

#----------------------------------------------------------------------------
# Nom      : <Calendar::MonthLength>
# Creation : Mars 2001 - J.P. Gauthier - CMC/CMOE -
#
# But      : Obtenir le nombre de jour d'un mois.
#
# Parametres :
#   <Year>   : Annee
#   <Month>  : Mois
#
# Retour     :
#   <Jour>   : Nombre de jour du mois
#
# Remarques  :
#
#----------------------------------------------------------------------------

proc Calendar::MonthLength { Year Month } {

   set jour 0

   if { $Month==2 } {
      set jour [Calendar::CheckFeb $Year]
   } elseif { $Month==4 || $Month==6 || $Month==9 || $Month==11 } {
      set jour 30
   } else {
      set jour 31
   }
   return $jour
}
