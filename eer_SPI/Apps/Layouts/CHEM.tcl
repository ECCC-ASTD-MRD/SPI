#===============================================================================
# Environnement Canada
# Centre Meteorologique Canadian
# 2100 Trans-Canadienne
# Dorval, Quebec
#
# Projet   : SPI (Layouts)
# Fichier  : CHEM.tcl
# Creation : Aout 2001
#
# Description:
#    Information sur l'experiences
#
#===============================================================================

namespace eval CHEM {
   variable Data

   set Data(Fields)     { ZH CV FM MF DD DI WD WI IT }
   set Data(Units)      { "" "/m³" "/m³" "*s/m³" "/m²" "/m²" "/m²" "/m²" "/m²" }
   set Data(Desc)       { "Particle positions" "Layer concentrations" "Surface to 500 m mean concentration" "Time integrated surface to 500 m layer concentrations" \
                          "6 hours dry deposition" "Total dry deposition" "6 hours wet deposition" "Total wet deposition" "Total deposition" }
}

proc CHEM::Layout { Frame } {
   variable Data

   Page::Size $Frame 770 800

   #----- Affichage des Viewports
   set Data(VP) [Viewport::Create $Frame 5 5 760 560 1 0]

   $Page::Data(Canvas) create rectangle 5 5 475 40 -width 1 -fill white -transparency 75
#   $Page::Data(Canvas) create rectangle 5 520 265 565 -width 1 -fill white -transparency 75

   #----- Positionnement des ColorBars
   set ColorBar::Data(Active$Frame) 1
   set ColorBar::Data($Data(VP)0) [list 675 570 90 225 CBAR$Data(VP)0]

   set SPI::Data(ShowColorBar$Frame) 1
   set SPI::Data(ShowLOGO) 1
   SPI::DrawImage $Frame LOGO 8 7 nw 1 0 0

   #----- Affichage de l'echelle
   CVScale::Create $Frame 135 540 250
   set SPI::Data(ShowScale$Frame) 1

   #----- Affichage de l'horloge
   CVClock::Create $Frame  565  715
   set SPI::Data(ShowClock$Frame) 1

   $Page::Data(Canvas) create rectangle 5 570 670 795 -width 1
   $Page::Data(Canvas) create text 10 575 -anchor nw -font XFont12 -tags "CHEMTEXT CVTEXT" -text "Release                          :
Dispersion Model                 :
Source Name                      :
Source Location                  :
Release Starting Date-Time       :
Release Duration                 :
Total Release Quantity           :
Simulation Duration              :
Initial Max. Plume Height        :
Initial Horiz. Dispersion Radius :
NWP Meteorological Model         :
NWP Meteorological Data          :"

   Page::UpdateItems $Frame
}

proc CHEM::LayoutUpdate { Frame } {
   global   env
   variable Sim
   variable Data
   variable Error

   #----- We don't want to update when animating
   if { !$Animator::Play(Stop) } {
      return
   }

   #----- Is there a field displayed
   if { [set field [lindex [Viewport::Assigned $Frame $Data(VP) fstdfield] 0]]=="" } {
      return
   }

   #----- Get information on selected field
   set Data(FieldList) [FieldBox::GetContent -1]
   set nv [string trim [fstdfield define $field -NOMVAR]]

   #----- Get the simulation info
   if { [set info [Info::Read [fstdfield define $field -FID]]]=="" } {
      return
   }
   Info::Decode ::CHEM::Sim $info

   if { [set idx [lsearch -exact $Data(Fields) $nv]]!=-1 } {
      set desc [lindex $Data(Desc) $idx]
   } else {
      set desc "Unknown"
   }

   set date [clock format [clock scan "$Sim(AccYear)$Sim(AccMonth)$Sim(AccDay) $Sim(AccHour):$Sim(AccMin)" -gmt True] -gmt True]
   set text "$desc\n\nRelease                          :
Dispersion Model                 : $Sim(Model)
Source Name                      : $Sim(Name)
Source Location                  : [format "%.4f %.4f" $Sim(Lat) $Sim(Lon)]
Release Starting Date-Time       : $date\n"

   switch $Sim(Model) {
      MLDP0   { append text "Simulation Duration              : $Sim(Duration) Hr(s)
Release Duration                 : $Sim(EmTotalDuration) s
Total Release Quantity           : [format "%.3e" $Sim(EmIsoQuantity)] unit
Initial Max. Plume Height        : $Sim(EmHeight) m
Initial Horiz. Dispersion Radius : $Sim(EmRadius) m
NWP Meteorological Model         : $Sim(Meteo)
NWP Meteorological Data          : $Sim(Mode)"
               }
      MLDP1   { append text "Simulation Duration              : $Sim(Duration) Hr(s)
Release Duration                 : $Sim(EmTotalDuration) s
Total Release Quantity           : [format "%.3e" $Sim(EmIsoQuantity)] unit
Initial Max. Plume Height        : $Sim(EmHeight) m
Initial Horiz. Dispersion Radius : $Sim(EmRadius) m
NWP Meteorological Model         : $Sim(Meteo)
NWP Meteorological Data          : $Sim(Mode)"
              }
      MLCD    { append text "Simulation Duration              : $Sim(DurMin) min
Release Duration                 : $Sim(EmDurationMin) Min(s)
Total Release Quantity           : [format "%.3e" $Sim(EmMass)] unit
Initial Max. Plume Height        : $Sim(EmTop) m
Initial Horiz. Dispersion Radius : $Sim(EmRadius) m
NWP Meteorological Model         : $Sim(Meteo)
NWP Meteorological Data          : "
              }
      TRAJECT {  append text "Simulation Duration              : $Sim(Duration) Hr(s)
Release Duration                 : NIL
Total Release Quantity           : NIL
Initial Max. Plume Height        : NIL
Initial Horiz. Dispersion Radius : NIL
NWP Meteorological Model         : $Sim(Meteo)
NWP Meteorological Data          : $Sim(Mode)"
              }
   }

   $Frame.page.canvas itemconfigure CHEMTEXT -text $text

   SPI::LayoutUpdate $Frame
}
