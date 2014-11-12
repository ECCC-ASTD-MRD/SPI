#===============================================================================
# Environnement Canada
# Centre Meteorologique Canadian
# 2100 Trans-Canadienne
# Dorval, Quebec
#
# Projet   : Widget de selection de couleur.
# Fichier  : ColorBox.tk
# Creation : Mars 2001 - J.P. Gauthier - CMC/CMOE
#
# Description:
#   -Definition d'un widget de selection de couleur qui agit selon la profondeur de l'ecran.
#
# Fonctions:
#   ColorBox::CreateSel  { Widget Var args }
#   ColorBox::Create     { Parent args }
#   ColorBox::Update     { X Y }
#   ColorBox::UpdateA    { args }
#   ColorBox::UpdateRGB  { Image args }
#   ColorBox::UpdateHSV  { args }
#
#===============================================================================

package provide ColorBox 2.0

catch { SPI::Splash "Loading Widget Package ColorBox 2.0" }

namespace eval ColorBox {
   global GDefs
   variable Data
   variable Lbl
   variable Bubble
   variable Resources

   set Data(Alpha)    False    ;#Activation de la selection de la transparence
   set Data(Current)  ""       ;#Couleur courante
   set Data(Result)   ""       ;#Couleur resultante
   set Data(State)    0        ;#Etat pour empecher l'execution avant la fin du packing

   set Data(H)   0             ;#Hue
   set Data(S)   100           ;#Saturation
   set Data(V)   100           ;#Variation
   set Data(R)   0             ;#Red
   set Data(G)   0             ;#Green
   set Data(B)   0             ;#Blue
   set Data(A)   255           ;#Alpha

   catch {
      set Resources(Fill)   $GDefs(Dir)/share/bitmap/fill.xbm
      set Resources(Colors) $GDefs(Colors)
   }

   set Lbl(Cancel) { "Annuler" "Cancel" }
   set Lbl(Apply)  { "Appliquer" "Apply" }
   set Lbl(Ok)     { "Ok" "Ok" }
   set Lbl(Clear)  { "Aucune" "None" }

   set Bubble(H)     { "Teinte" "Hue" }
   set Bubble(S)     { "Saturation" "Saturation" }
   set Bubble(V)     { "Variation" "Variation" }
   set Bubble(R)     { "Rouge" "Red" }
   set Bubble(G)     { "Vert" "Green" }
   set Bubble(B)     { "Bleue" "Blue" }
   set Bubble(A)     { "Alpha (Transparence)" "Alpha (Transparency)" }
   set Bubble(Hex)   { "Representation hexadecimale de la couleur" "Hexadecimal color representation" }
   set Bubble(ImgA)  { "Affichage/Selection de la transparence" "Display/Select the curent transparency" }
   set Bubble(ImgH)  { "Affichage/Selection de la teinte courante" "Display/Select the curent hue" }
   set Bubble(ImgSV) { "Affichage/Selection de la saturation et variation" "Display/Select the curent saturation and variation" }

   if { [winfo depth .] > 8 } {
      image create photo hsv   -width 120 -height 120
      image create photo hsv_h -width 10  -height 120
      image create photo hsv_a -width 10  -height 120

      colorsel image -sv hsv 360
      colorsel image -h  hsv_h
      colorsel image -a  hsv_a
   }
}

proc ColorBox::ConfigNoColor { Widget Color } {
   global GDefs

   if { [catch { $Widget configure -fg $Color } ] } {
      $Widget configure -fg $GDefs(ColorFrame)
   }
}

#------------------------------------------------------------------------------
# Nom      : <ColorBox::CreateSel>
# Creation : Mai 2001 - J.P. Gauthier - CMC/CMOE -
#
# But     : Creer un widget de selection de couleur
#
# Parametres :
#   <Widget> : Path du widget
#   <Var>    : Variable a assigner la couleur
#   <args>   : Commande a executer apres la selection de la couleur
#
# Retour:
#
# Remarques :
#
#-------------------------------------------------------------------------------

proc ColorBox::CreateSel { Widget Var args } {
   global GDefs
   variable Data
   variable Resources

   label $Widget -relief groove -bd 2 -bitmap @$Resources(Fill)

   if { [llength $Var]==2 } {
      set alpha [lindex $Var 1]
      set Var   [lindex $Var 0]
   } else {
      set alpha ""
   }
   
   eval set fg \$$Var
   if { $fg!="" } {
      $Widget configure -fg $fg
   } else {
      $Widget configure -fg $GDefs(ColorFrame)
   }

   bind $Widget <ButtonRelease-1> "ColorBox::Create $Widget $Var $alpha; if { \$$Var!=\"\" } { catch { $Widget config -fg \$$Var } } else { $Widget configure -fg $GDefs(ColorFrame) } ;eval $args"
}

#------------------------------------------------------------------------------
# Nom      : <ColorBox::Create>
# Creation : Mai 2001 - J.P. Gauthier - CMC/CMOE -
#
# But     : Creer un widget de palette de couleur
#
# Parametres :
#   <Parent> : Path du parent
#   <Color>  : Variable Couleur initiale
#   <Alpha>  : Variable Alpha
#
# Retour:
#
# Remarques :
#   -La methode utilisee differe selon la profondeur disponible
#
#-------------------------------------------------------------------------------

proc ColorBox::Create { Parent { Color "" } { Alpha "" } } {
   global   GDefs
   variable Data
   variable Lbl
   variable Bubble
   variable Resources

   if { [winfo exists .colbox] } {
      destroy .colbox
      return
   }

   #----- Couleur initiale
   set Data(Current) [set color #FFFFFF]
   
   if { $Color!="" } {
      eval set color \$$Color
      set Data(Current) [string toupper [string range $color 0 6]]
      if { [string length $color]>7 || $Alpha!="" } {
         set Data(Alpha) True
      } else {
         set Data(Alpha) False
      }
   }

   set Data(State)  0
   set Data(Result) $Data(Current)

   #----- Creer une fenetre sans frame en bas a gauche du parent

   toplevel     .colbox
   wm geom      .colbox =+[winfo rootx $Parent]+[winfo rooty $Parent]
   wm transient .colbox $Parent
   wm resizable .colbox 0 0
   wm title     .colbox "ColorBox 2.0"

   set pgrab [grab current]
   grab .colbox

   #----- Pour un affichage 8 bit

   if { [winfo depth .] < 16 } {
      foreach col $Resources(Colors) {
         button .colbox.c$col -bitmap @$Resources(Fill) -bd 1 -foreground $col -activeforeground $col \
            -command "set ColorBox::Data(Result) $col"
         pack .colbox.c$col -side top
      }
   } else {

      frame .colbox.opt
         frame .colbox.opt.sel -relief raised -bd 1
            frame .colbox.opt.sel.cv
               canvas .colbox.opt.sel.cv.hsv -width 120 -height 120 -relief sunken -bd 1
               canvas .colbox.opt.sel.cv.h -width 10 -height 120 -relief sunken -bd 1
               pack .colbox.opt.sel.cv.hsv .colbox.opt.sel.cv.h -side left -padx 2 -pady 2
            entry .colbox.opt.sel.hex -textvariable ColorBox::Data(Current) -relief sunken -bd 1 -width 8
            pack .colbox.opt.sel.cv -side top -anchor sw
            pack .colbox.opt.sel.hex

         pack .colbox.opt.sel -side left -fill both -expand true

         .colbox.opt.sel.cv.hsv create image 0 0 -image hsv -anchor nw -tags HSV
         .colbox.opt.sel.cv.hsv create line 0 0 0 0 -fill black -width 1 -tags HSV_SBAR
         .colbox.opt.sel.cv.hsv create line 0 0 0 0 -fill black -width 1 -tags HSV_VBAR

         .colbox.opt.sel.cv.h create image 0 0 -image hsv_h -anchor nw -tags HSV_H
         .colbox.opt.sel.cv.h create line 0 0 0 0 -fill black -width 1 -tags HSV_HBAR


         bind .colbox.opt.sel.cv.h   <B1-Motion>  { set ColorBox::Data(H) [expr int(%y/120.0*360.0)]; ColorBox::UpdateRGB 1 }
         bind .colbox.opt.sel.cv.h   <Button-1>   { set ColorBox::Data(H) [expr int(%y/120.0*360.0)]; ColorBox::UpdateRGB 1 }

         bind .colbox.opt.sel.cv.hsv <Button-1>   "ColorBox::Update %x %y"
         bind .colbox.opt.sel.cv.hsv <B1-Motion>  "ColorBox::Update %x %y"

         if { $Data(Alpha) } {
            canvas .colbox.opt.sel.cv.a -width 10 -height 120 -relief sunken -bd 1
            .colbox.opt.sel.cv.a create image 0 0 -image hsv_a -anchor nw -tags HSV_A
            .colbox.opt.sel.cv.a create line 0 0 0 0 -fill black -width 1 -tags HSV_ABAR
            pack .colbox.opt.sel.cv.a -side left -padx 2 -pady 2

            bind .colbox.opt.sel.cv.a   <B1-Motion>  { set ColorBox::Data(A) [expr int(%y/120.0*255.0)]; ColorBox::UpdateA 1 }
            bind .colbox.opt.sel.cv.a   <Button-1>   { set ColorBox::Data(A) [expr int(%y/120.0*255.0)]; ColorBox::UpdateA 1 }
         }

         frame .colbox.opt.opt
            frame .colbox.opt.opt.h -relief raised -bd 1
               label .colbox.opt.opt.h.lbl -text "H"
               entry .colbox.opt.opt.h.val -relief flat -bd 1 -textvariable ColorBox::Data(H) -width 3
               scale .colbox.opt.opt.h.sca -bd 1 -relief flat -width 15 -sliderlength 10 -from 0 -to 360 -variable ColorBox::Data(H) \
                  -orient horizontal -command "ColorBox::UpdateRGB 1" -showvalue false
               bind .colbox.opt.opt.h.val <Any-KeyRelease> "ColorBox::UpdateRGB 1"
               pack .colbox.opt.opt.h.lbl .colbox.opt.opt.h.sca .colbox.opt.opt.h.val -side left -fill y
            frame .colbox.opt.opt.s -relief raised -bd 1
               label .colbox.opt.opt.s.lbl -text "S"
               entry .colbox.opt.opt.s.val -relief flat -bd 1 -textvariable ColorBox::Data(S) -width 3
               scale .colbox.opt.opt.s.sca -bd 1 -relief flat -width 15 -sliderlength 10 -from 0 -to 100 -variable ColorBox::Data(S) \
                  -orient horizontal -command "ColorBox::UpdateRGB 0" -showvalue false
               bind .colbox.opt.opt.s.val <Any-KeyRelease> "ColorBox::UpdateRGB 0"
               pack .colbox.opt.opt.s.lbl .colbox.opt.opt.s.sca .colbox.opt.opt.s.val -side left -fill y
            frame .colbox.opt.opt.v -relief raised -bd 1
               label .colbox.opt.opt.v.lbl -text "V"
               entry .colbox.opt.opt.v.val -relief flat -bd 1 -textvariable ColorBox::Data(V) -width 3
               scale .colbox.opt.opt.v.sca -bd 1 -relief flat -width 15 -sliderlength 10 -from 0 -to 100 -variable ColorBox::Data(V) \
                  -orient horizontal -command "ColorBox::UpdateRGB 0" -showvalue false
               bind .colbox.opt.opt.v.val <Any-KeyRelease> "ColorBox::UpdateRGB 0"
               pack .colbox.opt.opt.v.lbl .colbox.opt.opt.v.sca .colbox.opt.opt.v.val -side left -fill y
            frame .colbox.opt.opt.r -relief raised -bd 1
               label .colbox.opt.opt.r.lbl -text "R"
               entry .colbox.opt.opt.r.val -relief flat -bd 1 -textvariable ColorBox::Data(R) -width 3
               scale .colbox.opt.opt.r.sca -bd 1 -relief flat -width 15 -sliderlength 10 -from 0 -to 255 -variable ColorBox::Data(R) \
                  -orient horizontal -command "ColorBox::UpdateHSV" -showvalue false
               bind .colbox.opt.opt.r.val <Any-KeyRelease> "ColorBox::UpdateHSV"
               pack .colbox.opt.opt.r.lbl .colbox.opt.opt.r.sca .colbox.opt.opt.r.val -side left -fill y
            frame .colbox.opt.opt.g -relief raised -bd 1
               label .colbox.opt.opt.g.lbl -text "G"
               entry .colbox.opt.opt.g.val -relief flat -bd 1 -textvariable ColorBox::Data(G) -width 3
               scale .colbox.opt.opt.g.sca -bd 1 -relief flat -width 15 -sliderlength 10 -from 0 -to 255 -variable ColorBox::Data(G) \
                  -orient horizontal -command "ColorBox::UpdateHSV" -showvalue false
               bind .colbox.opt.opt.g.val <Any-KeyRelease> "ColorBox::UpdateHSV"
               pack .colbox.opt.opt.g.lbl .colbox.opt.opt.g.sca .colbox.opt.opt.g.val -side left -fill y
            frame .colbox.opt.opt.b -relief raised -bd 1
               label .colbox.opt.opt.b.lbl -text "B"
               entry .colbox.opt.opt.b.val -relief flat -bd 1 -textvariable ColorBox::Data(B) -width 3
               scale .colbox.opt.opt.b.sca -bd 1 -relief flat -width 15 -sliderlength 10 -from 0 -to 255 -variable ColorBox::Data(B) \
                  -orient horizontal -command "ColorBox::UpdateHSV" -showvalue false
               bind .colbox.opt.opt.b.val <Any-KeyRelease> "ColorBox::UpdateHSV"
               pack .colbox.opt.opt.b.lbl .colbox.opt.opt.b.sca .colbox.opt.opt.b.val -side left -fill y
            frame .colbox.opt.opt.a -relief raised -bd 1
               label .colbox.opt.opt.a.lbl -text "A"
               pack .colbox.opt.opt.a.lbl -side left
               if { $Data(Alpha) } {
                  entry .colbox.opt.opt.a.val -relief flat -bd 1 -textvariable ColorBox::Data(A) -width 3
                  scale .colbox.opt.opt.a.sca -bd 1 -relief flat -width 15 -sliderlength 10 -from 0 -to 255 -variable ColorBox::Data(A) \
                     -orient horizontal -command "ColorBox::UpdateA" -showvalue false
                  bind .colbox.opt.opt.a.val <Any-KeyRelease> "ColorBox::UpdateA"
                  pack .colbox.opt.opt.a.sca .colbox.opt.opt.a.val -side left -fill y
               }
           pack .colbox.opt.opt.h .colbox.opt.opt.s .colbox.opt.opt.v .colbox.opt.opt.r .colbox.opt.opt.g .colbox.opt.opt.b .colbox.opt.opt.a \
              -side top -fill both -expand true
         pack .colbox.opt.opt -side left -fill y
      pack .colbox.opt -side top

      frame .colbox.cmd
         button .colbox.cmd.no -text [lindex $Lbl(Cancel) $GDefs(Lang)] -bd 1 -relief raised\
            -command { set ColorBox::Data(Result) $ColorBox::Data(Result) }
         button .colbox.cmd.clr -text [lindex $Lbl(Clear) $GDefs(Lang)] -bd 1 -relief raised\
            -command { set ColorBox::Data(Result) "" }
         button .colbox.cmd.ok -text [lindex $Lbl(Ok) $GDefs(Lang)] -bd 1 -relief raised \
            -command { set ColorBox::Data(Result) $ColorBox::Data(Current) }
         pack .colbox.cmd.no .colbox.cmd.clr .colbox.cmd.ok -side left -fill x -expand true
      pack .colbox.cmd -side top -fill x

      Bubble::Create .colbox.opt.opt.h.lbl  $Bubble(H)
      Bubble::Create .colbox.opt.opt.s.lbl  $Bubble(S)
      Bubble::Create .colbox.opt.opt.v.lbl  $Bubble(V)
      Bubble::Create .colbox.opt.opt.r.lbl  $Bubble(R)
      Bubble::Create .colbox.opt.opt.g.lbl  $Bubble(G)
      Bubble::Create .colbox.opt.opt.b.lbl  $Bubble(B)
      Bubble::Create .colbox.opt.opt.a.lbl  $Bubble(A)
      Bubble::Create .colbox.opt.sel.cv.a   $Bubble(ImgA)
      Bubble::Create .colbox.opt.sel.cv.h   $Bubble(ImgH)
      Bubble::Create .colbox.opt.sel.cv.hsv $Bubble(ImgSV)
      Bubble::Create .colbox.opt.sel.hex    $Bubble(Hex)

      #----- Initialiser la couleur courante
      #----- l'update sert a forcer les bindings de scale a s'executer imediatement

      update
      set Data(State) 1
      scan $color "%1s%02x%02x%02x%02x" b Data(R) Data(G) Data(B) Data(A)

      if { $Alpha!="" } {
         eval scan \$$Alpha %02x Data(A)
      }
      set alpha $Data(A)
      
      ColorBox::UpdateHSV
   }

    catch { .colbox.opt.sel.hex configure -bg $ColorBox::Data(Result) }

   #----- Attendre la selection

   tkwait variable ColorBox::Data(Result)
   destroy .colbox
   if { $pgrab!="" } {
      grab $pgrab
   }
   
   if { $Color!="" } {
      set $Color $ColorBox::Data(Result)
   }
   if { $Alpha!="" } {
      set $Alpha [string toupper [format "%02x" $ColorBox::Data(A)]]
   }
   
   if { $color!=$ColorBox::Data(Result) || $alpha!=$Data(A) } {
      return True
   } else {
      return False
   }
}

#------------------------------------------------------------------------------
# Nom      : <ColorBox::Update>
# Creation : Mai 2001 - J.P. Gauthier - CMC/CMOE -
#
# But     : Mettre a jour les parametres selon les coordonnees du curseur
#
# Parametres :
#   <X>      : Coordonnee X du curseur
#   <Y>      : Coordonnee X du curseur
#
# Retour:
#
# Remarques :
#
#-------------------------------------------------------------------------------

proc ColorBox::Update { X Y } {
   variable Data
   variable Resources

   if { !$Data(State) } {
      return
   }

   if { $X<120 && $Y<120 && $X>0 && $Y>0} {
      eval set Data(Current) \[string toupper \[format \"#%02x%02x%02x\" [hsv get $X $Y]\]\]
   }

   set Data(V) [expr int($X/120.0*100.0)]
   set Data(S) [expr int($Y/120.0*100.0)]

   .colbox.opt.sel.cv.hsv coords HSV_SBAR 0 $Y 120 $Y
   .colbox.opt.sel.cv.hsv coords HSV_VBAR $X 0 $X 120

   ColorBox::UpdateRGB 0
}

#------------------------------------------------------------------------------
# Nom      : <ColorBox::UpdateA>
# Creation : Mai 2001 - J.P. Gauthier - CMC/CMOE -
#
# But     : Mettre a jour les parametres Alpha
#
# Parametres :
#   <args>   : Argument du scrollbar (inutilise)
#
# Retour:
#
# Remarques :
#
#-------------------------------------------------------------------------------

proc ColorBox::UpdateA { args } {
   variable Data

   if { !$Data(State) } {
      return
   }

   set y [expr $Data(A)/255.0*120]
   .colbox.opt.sel.cv.a coords HSV_ABAR 0 $y 10 $y
}

#------------------------------------------------------------------------------
# Nom      : <ColorBox::UpdateRGB>
# Creation : Mai 2001 - J.P. Gauthier - CMC/CMOE -
#
# But     : Mettre a jour les parametres RGB a partir des HSV
#
# Parametres :
#   <Image>  : Image de la palette
#   <args>   : Argument du scrollbar (inutilise)
#
# Retour:
#
# Remarques :
#
#-------------------------------------------------------------------------------

proc ColorBox::UpdateRGB { Image args } {
   global GDefs
   variable Data

   if { !$Data(State) } {
      return
   }
   set tri [colorsel hsv2rgb $Data(H) $Data(S) $Data(V)]

   set Data(R) [expr int([lindex $tri 0])]
   set Data(G) [expr int([lindex $tri 1])]
   set Data(B) [expr int([lindex $tri 2])]

   set y [expr $Data(H)/360.0*120]
   .colbox.opt.sel.cv.h coords HSV_HBAR 0 $y 10 $y

   set x [expr $Data(V)/100.0*120]
   set y [expr $Data(S)/100.0*120]

   .colbox.opt.sel.cv.hsv coords HSV_SBAR 0 $y 120 $y
   .colbox.opt.sel.cv.hsv coords HSV_VBAR $x 0 $x 120

   set Data(Current) [string toupper [format "#%02x%02x%02x" $Data(R) $Data(G) $Data(B)]]
   .colbox.opt.sel.hex configure -bg $Data(Current)

   if { $Image } {
      colorsel image -sv hsv $Data(H)
   }
}

#------------------------------------------------------------------------------
# Nom      : <ColorBox::UpdateHSV>
# Creation : Mai 2001 - J.P. Gauthier - CMC/CMOE -
#
# But     : Mettre a jour les parametres HSV a partir des RGB
#
# Parametres :
#   <args>   : Argument du scrollbar (inutilise)
#
# Retour:
#
# Remarques :
#
#-------------------------------------------------------------------------------

proc ColorBox::UpdateHSV { args } {
   global GDefs
   variable Data

   if { !$Data(State) } {
      return
   }

   set tri [colorsel rgb2hsv $Data(R) $Data(G) $Data(B)]

   set Data(H) [lindex $tri 0]
   set Data(S) [lindex $tri 1]
   set Data(V) [lindex $tri 2]

   colorsel image -sv hsv $Data(H)

   set y [expr $Data(H)/360.0*120]
   .colbox.opt.sel.cv.h coords HSV_HBAR 0 $y 10 $y

   set x [expr $Data(V)/100.0*120]
   set y [expr $Data(S)/100.0*120]

   .colbox.opt.sel.cv.hsv coords HSV_SBAR 0 $y 120 $y
   .colbox.opt.sel.cv.hsv coords HSV_VBAR $x 0 $x 120

   set Data(H) [expr int($Data(H))]
   set Data(S) [expr int($Data(S))]
   set Data(V) [expr int($Data(V))]
   set Data(R) [expr int($Data(R))]
   set Data(G) [expr int($Data(G))]
   set Data(B) [expr int($Data(B))]

   if { [expr $Data(R)+$Data(G)+$Data(B)+$Data(H)+$Data(S)+$Data(V)]==0 } {
#      set Data(Current) ""
     .colbox.opt.sel.hex configure -bg $GDefs(ColorFrame)
   } else {
      set Data(Current) [string toupper [format "#%02x%02x%02x" $Data(R) $Data(G) $Data(B)]]
     .colbox.opt.sel.hex configure -bg $Data(Current)
   }
}
