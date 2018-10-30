#===============================================================================
# Environnement Canada
# Centre Meteorologique Canadian
# 2100 Trans-Canadienne
# Dorval, Quebec
#
# Projet   : Widget de selection de couleur.
# Fichier  : FontBox.tk
# Creation : Mars 2001 - J.P. Gauthier - CMC/CMOE
#
# Description:
#   -Definition d'un widget de selection de police.
#
# Fonctions:
#   FontBox::Create { Parent args }
#   FontBox::Update { Font Show }
#
# Modifications :
#
#   Nom         : J.P. Gauthier - CMC/CMOE
#   Date        : Juillet 1995
#   Description : Tout les font sont maintenant definit avec des dimensions negatives
#
#===============================================================================

package provide FontBox 1.1

catch { SPI::Splash "Loading Widget Package FontBox 1.1" }

namespace eval FontBox {
   global GDefs
   variable Data
   variable Lbl
   variable Resources

   set Data(Family)     courier
   set Data(Weight)     normal
   set Data(Size)       14
   set Data(Slant)      roman
   set Data(Underline)  0
   set Data(Overstrike) 0

   set Lbl(Bold)   { "Gras" "Bold" }
   set Lbl(Italic) { "Italique" "Italic" }
   set Lbl(Under)  { "Souligne" "Underline" }
   set Lbl(Over)   { "Barre" "Overstrike" }
   set Lbl(Close)  { "Fermer" "Close" }
   set Lbl(Apply)  { "Appliquer" "Apply" }

   font create fontsel -family $Data(Family) -weight $Data(Weight) -size -$Data(Size) \
      -slant $Data(Slant) -underline $Data(Underline) -overstrike $Data(Overstrike)

   package require ComboBox
}

#------------------------------------------------------------------------------
# Nom      : <FontBox::Create>
# Creation : Juin 2001 - J.P. Gauthier - CMC/CMOE -
#
# But     : Creer un widget de selection de police
#
# Parametres :
#   <Parent> : Path du parent
#   <Apply>  : Commande a effectuer pour appliquer les changements
#   <args>   : Police initiale
#
# Remarques :
#
#-------------------------------------------------------------------------------

proc FontBox::Create { Parent Apply args } {
   global   GDefs
   variable Data
   variable Lbl
   variable Resources

   #----- font initial
   if { $args!="" } {
      set Data(Current)    $args
      set Data(Family)     [font actual $Data(Current) -family]
      set Data(Weight)     [font actual $Data(Current) -weight]
      set Data(Size)       [expr abs([font actual $Data(Current) -size])]
      set Data(Slant)      [font actual $Data(Current) -slant]
      set Data(Underline)  [font actual $Data(Current) -underline]
      set Data(Overstrike) [font actual $Data(Current) -overstrike]
   }

   toplevel     .fontbox
   wm geom      .fontbox =+[winfo rootx $Parent]+[winfo rooty $Parent]
   wm transient .fontbox .
   wm title     .fontbox "FontBox 1.1"

   frame .fontbox.opt -relief raised -bd 1
      ComboBox::Create .fontbox.opt.family FontBox::Data(Family) edit sorted nodouble -1 \
         [font families] 25 6 "FontBox::Update fontsel True"
      spinbox  .fontbox.opt.size -textvariable FontBox::Data(Size) -from 1 -to 72 -command "FontBox::Update fontsel True" \
         -bd 1 -bg $GDefs(ColorLight) -width 4
      checkbutton .fontbox.opt.weight -text [lindex $Lbl(Bold) $GDefs(Lang)] -variable FontBox::Data(Weight) \
         -onvalue bold -offvalue normal -indicatoron false -bd 1 -command "FontBox::Update fontsel True" \
         -relief sunken -overrelief raised -offrelief flat
      checkbutton .fontbox.opt.slant -text [lindex $Lbl(Italic) $GDefs(Lang)] -variable FontBox::Data(Slant) \
         -onvalue italic -offvalue roman -indicatoron false -bd 1 -command "FontBox::Update fontsel True"  \
         -relief sunken -overrelief raised -offrelief flat
      checkbutton .fontbox.opt.under -text [lindex $Lbl(Under) $GDefs(Lang)] -variable FontBox::Data(Underline) \
         -onvalue 1 -offvalue 0 -indicatoron false -bd 1 -command "FontBox::Update fontsel True"  \
         -relief sunken -overrelief raised -offrelief flat
      checkbutton .fontbox.opt.over -text [lindex $Lbl(Over) $GDefs(Lang)] -variable FontBox::Data(Overstrike) \
         -onvalue 1 -offvalue 0 -indicatoron false -bd 1 -command "FontBox::Update fontsel True"  \
         -relief sunken -overrelief raised -offrelief flat
      pack .fontbox.opt.family -side left -fill x
      pack  .fontbox.opt.size  .fontbox.opt.weight .fontbox.opt.slant .fontbox.opt.under .fontbox.opt.over -ipady 1 -ipadx 2 -side left
   pack .fontbox.opt -side top -fill x

   frame .fontbox.show -relief raised -bd 1
      canvas .fontbox.show.cv -width 300 -height 100 -relief sunken -bg white -bd 1
      pack .fontbox.show.cv -side left -fill both -expand true
   pack .fontbox.show -side top -fill both -expand true

   frame .fontbox.cmd
      button .fontbox.cmd.close -text [lindex $Lbl(Close) $GDefs(Lang)] -bd 1 -relief raised -command "destroy .fontbox"
      button .fontbox.cmd.apply -text [lindex $Lbl(Apply) $GDefs(Lang)] -bd 1 -relief raised -command "FontBox::Update $FontBox::Data(Current) False; $Apply"
      pack .fontbox.cmd.apply .fontbox.cmd.close -side left -fill x -expand true
   pack .fontbox.cmd -side top -fill x

   bind .fontbox.opt.size   <Return>  "FontBox::Update fontsel True"
   #----- Creer la chaine de demonstration

   .fontbox.show.cv create text 10 10 -fill black -tags TEXT -anchor nw \
      -text "abcdefghijklmnopqrstuvwxyz\nABCDEFGHIJKLMNOPQRSTUVWXYZ\n1234567890\n(){}[]<>~.,?!@#$%^&*_-+=\/"

   #----- Appliquer le font courant

   FontBox::Update fontsel True

   grab .fontbox
}

#------------------------------------------------------------------------------
# Nom      : <FontBox::Update>
# Creation : Juin 2001 - J.P. Gauthier - CMC/CMOE -
#
# But     : Mettre a jour les parametres de la police courante
#
# Parametres :
#   <Font>   : Identifiacteur de la police
#   <Show>   : Afficher les modiffications
#
# Remarques :
#
#-------------------------------------------------------------------------------

proc FontBox::Update { Font Show } {
   variable Data

   if { $Font!="" } {
      font configure $Font -family $Data(Family) -weight $Data(Weight) -size -$Data(Size) \
         -slant $Data(Slant) -underline $Data(Underline) -overstrike $Data(Overstrike)
       

      if { $Show } {
         .fontbox.show.cv itemconfigure TEXT -font $Font
      }
   }
}
