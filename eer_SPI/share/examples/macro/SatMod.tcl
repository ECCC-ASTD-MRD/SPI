#===============================================================================
# Environnement Canada
# Centre Meteorologique Canadian
# 2100 Trans-Canadienne
# Dorval, Quebec
#
# Projet   : Exemples de scripts de productions de cartes.
# Fichier  : SatMod.tcl
# Creation : Fevrier 2009 - J.P.Gauthier - CMC/CMOE
#
# Description:
#    Afficher une supperposition des vents radarsat et du modele numerique.
#
# Arguments  :
#
# Remarques :
#
#===============================================================================

namespace eval Macro::SatMod {} {
   variable Param
   variable Data
   variable Error

   set Param(Info) { "Afficher des vents Radarsat et modeles."
                     "Display Radarstat and modeled winds." }

   set Param(File) ""          ;#RPN file input (Script/DataIn/20091204_094244_radarsat)
   set Param(Info) "Lab Météo Menacante / Severe Weather Lab
Vitesse du vent / Wind speed Radarsat
Modèle numérique / Numerical model: Barbules / Wind barbs
Heure de validitée / Valid time:"

   set Param(InfoArgs) { { "Fichier dtandard" } { "Standard file" } }
}

proc Macro::SatMod::Execute { } {
   global env
   variable Data
   variable Error
   variable Param

   #----- Set product page size
   Page::Size $Page::Data(Frame) 1000 700
   Miniport::Create $Page::Data(Frame) 795 5 200 200 False 3

   Macro::Doing "Reading fields"
   fstdfile open STDFILE read $Param(File)
   fstdfield read RADARSAT STDFILE -1 "" -1 -1 -1 "" UV
   fstdfield read MODEL    STDFILE -1 "" -1 -1 -1 "" UU

   #----- Figure out limits
   set ll0 [fstdfield stat RADARSAT -project 0 0]
   set ll1 [fstdfield stat RADARSAT -project [fstdfield define RADARSAT -NI] [fstdfield define RADARSAT -NJ]]

   #----- Zoom on the data
   ProjCam::CloseUp $Page::Data(Frame) $Page::Data(Frame) $Viewport::Data(VP) \
      [lindex $ll0 0] [lindex $ll0 1] [lindex $ll1 0] [lindex $ll1 1] 0.1

   #----- Configure the data for display
   if { ![colormap is SATMODMAP] } {
      colormap create SATMODMAP
      colormap read SATMODMAP $env(HOME)/.spi/Colormap/REC_Col.std1.rgba
   }
   font create SATMODFONT -family Arial -size -12 -weight bold

   fstdfield configure RADARSAT -desc "UV" -unit "(noeuds)" -color black -colormap SATMODMAP -font SATMODFONT \
       -rendertexture 1 -value INTEGER 2 -min 2 -max 50 -intervalmode INTERVAL 2
   fstdfield configure MODEL -desc "Modèle" -color black -colormap SATMODMAP -font SATMODFONT \
       -rendertexture 0 -rendervector BARB -sample 5 -size 10 -width 2

   Macro::Doing "Creating product"
   Viewport::UnAssign $Page::Data(Frame) $Viewport::Data(VP)
   Viewport::Assign   $Page::Data(Frame) $Viewport::Data(VP) { RADARSAT MODEL }

   #----- Create colorbar and legend
   $Page::Data(Canvas) create colorbar -x 5 -y 5 -width 70 -height 400 \
         -data RADARSAT -tags SATMODCOLORBAR -anchor nw -barsplit 0 -barside right \
         -barborder 1 -barwidth 20 -bg white -transparency 75 -showfactor False

   set file [split [file tail $Param(File)] _]
   set date [clock scan "[lindex $file 0] [lindex $file 1]"]
   $Page::Data(Canvas) create rectangle 5 630 440 695 -width 1 -fill white -transparency 75
   $Page::Data(Canvas) create text 10 635 -anchor nw -font SATMODFONT \
      -text "$Param(Info) [clock format $date -format "%Y%m%d - %H:%M:%S" -gmt True]"

   Macro::Doing ""

   #----- If in batch mode, print the map and exit
   if { $SPI::Param(Batch) } {
      PrintBox::Image $Page::Data(Frame) png SatMod
      SPI::Quit
   }

   fstdfile close STDFILE
}

proc Macro::SatMod::Clean { } {

   Viewport::UnAssign $Page::Data(Frame) $Viewport::Data(VP)
   font delete SATMODFONT

   fstdfield free RADARSAT MODEL
}

proc Macro::SatMod::Args{ } {
   global argv argc
   variable Param

   #----- Lire les parametres si il y en a
   if { $argc>0 } { set Param(File) [lindex $argv 0] }
}







