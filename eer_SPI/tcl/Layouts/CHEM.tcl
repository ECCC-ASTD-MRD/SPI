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
   set Data(Desc)       { { "Position des particules" "Concentrations par couches" "Concentration moyenne de la surface à 500 m" "Concentration intégrée temporellement de la surfame à 500" \
                          "Déposition sèche sur 6 heures" "Déposition sèche totale" "Déposition humide sur 6 heures" "Déposition humide totale" "Déposition totale" }
                          { "Particle positions" "Layer concentrations" "Surface to 500 m mean concentration" "Time integrated surface to 500 m layer concentrations" \
                          "6 hours dry deposition" "Total dry deposition" "6 hours wet deposition" "Total wet deposition" "Total deposition" } }
        
   set Data(Locales) { fr_ca en_us }

   set Lbl(Unit)     { "Unité" "Unit" }
   set Lbl(Unknown)  { "Inconnu" "Unknown" }
   set Lbl(Release)  { "Relâchement" "Release" }
   set Lbl(Model)    { "Modèle de dispersion         :" "Dispersion Model             :" }
   set Lbl(Name)     { "Nom de la source             :" "Source Name                  :" }
   set Lbl(Location) { "Localisation de la source    :" "Source location              :" }
   set Lbl(Start)    { "Date du début du relâchement :" "Release Starting Date-Time   :" }
   set Lbl(SimDur)   { "Durée de la simulation       :" "Simulation Duration          :" }
   set Lbl(Duration) { "Durée du relâchement         :" "Release Duration             :" }
   set Lbl(Total)    { "Quantité totale relâchée     :" "Total Release Quantity       :" }
   set Lbl(Height)   { "Hauteur initiale maximale    :" "Initial Maximum Plume Height :" }
   set Lbl(Radius)   { "Rayon de dispersion initial  :" "Initial Dispersion Radius    :" }
   set Lbl(Meteo)    { "Modèle PNT météorologique    :" "NWP Meteorological Model     :" }
   set Lbl(Mode)     { "Données PNT météorologiques  :" "NWP Meteorological Data      :" }
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
   $Page::Data(Canvas) create text 10 575 -anchor nw -font XFont12 -tags "CHEMTEXT CVTEXT" -text ""

   Page::UpdateItems $Frame
}

proc CHEM::LayoutUpdate { Frame } {
   global   GDefs env
   variable Sim
   variable Data
   variable Error
   variable Lbl
   
   #----- We don't want to update when animating
   if { !$Animator::Play(Stop) } {
      return
   }

   #----- Is there a field displayed
   if { [set field [lindex [Viewport::Assigned $Frame $Data(VP) fstdfield] 0]]=="" } {
      return
   }

   #----- Get information on selected field
   set nv  [string trim [fstdfield define $field -NOMVAR]]
   set iso [string trim [fstdfield define $field -ETIKET]]

   #----- Get the simulation info
   if { [set info [Info::Read [fstdfield define $field -FID]]]=="" } {
      return
   }
   Info::Decode ::CHEM::Sim $info

   if { [set idx [lsearch -exact $Data(Fields) $nv]]!=-1 } {
      set desc [lindex [lindex $Data(Desc) $GDefs(Lang)] $idx]
   } else {
      set desc [lindex $Lbl(Unknown) $GDefs(Lang)]
   }

   if { [llength $Sim(Name)]>1 } {
      set coords ""
      foreach src $Sim(Name) lat $Sim(Lat) lon $Sim(Lon) {
         append coords [format "(%.2f %.2f) " $lat $lon]
      }
   } else {
      set coords [format "%.4f %.4f" $Sim(Lat) $Sim(Lon)]
   }
   
   set date [DateStuff::StringDateFromSeconds [clock scan "$Sim(AccYear)$Sim(AccMonth)$Sim(AccDay) $Sim(AccHour):$Sim(AccMin)"] $GDefs(Lang)]
   set text "$desc\n\n[lindex $Lbl(Release) $GDefs(Lang)]:\n
[lindex $Lbl(Model) $GDefs(Lang)] $Sim(Model)
[lindex $Lbl(Name) $GDefs(Lang)] $Sim(Name)
[lindex $Lbl(Location) $GDefs(Lang)] $coords
[lindex $Lbl(Start) $GDefs(Lang)] $date\n"

   switch $Sim(Model) {
      MLDP0   -
      MLDP1   {
        if { [string is double $Sim(EmIsoQuantity)] } {
           set q [format "%.3e [lindex $Lbl(Unit) $GDefs(Lang)]" $Sim(EmIsoQuantity)]
        } else {
            set q $Sim(EmIsoQuantity)
        }
        append text "[lindex $Lbl(SimDur) $GDefs(Lang)] $Sim(Duration) Hr(s)
[lindex $Lbl(Duration) $GDefs(Lang)] $Sim(EmTotalDuration) s
[lindex $Lbl(Total) $GDefs(Lang)] $q
[lindex $Lbl(Height) $GDefs(Lang)] $Sim(EmHeight) m
[lindex $Lbl(Radius) $GDefs(Lang)] $Sim(EmRadius) m
[lindex $Lbl(Meteo) $GDefs(Lang)] $Sim(Meteo)
[lindex $Lbl(Mode) $GDefs(Lang)] $Sim(Mode)"
              }
      MLDPn  {
        MLDPn::ScenarioDecode $Sim(SrcType) $Sim(Scenario) "|"
        if { [set idx [lsearch -nocase -exact $MLDPn::Sim(EmIsos) $iso]]!=-1 } {
           set q [format "%.3e [lindex $Lbl(Unit) $GDefs(Lang)]" [lindex $MLDPn::Sim(EmMassIsos) $idx]]
        } else {
           set q NIL
        }
        append text "[lindex $Lbl(SimDur) $GDefs(Lang)] $Sim(Duration) Hr(s)
[lindex $Lbl(Duration) $GDefs(Lang)] $MLDPn::Sim(EmTotalDuration) s
[lindex $Lbl(Total) $GDefs(Lang)] $q
[lindex $Lbl(Height) $GDefs(Lang)] $MLDPn::Sim(EmHeight.0) m
[lindex $Lbl(Radius) $GDefs(Lang)] $MLDPn::Sim(EmRadius.0) m
[lindex $Lbl(Meteo) $GDefs(Lang)] $Sim(Meteo)
[lindex $Lbl(Mode) $GDefs(Lang)] $Sim(Mode)"
              }
      MLCD    { append text "[lindex $Lbl(SimDur) $GDefs(Lang)] $Sim(DurMin) min(s)
[lindex $Lbl(Duration) $GDefs(Lang)] $Sim(EmDurationMin) min(s)
[lindex $Lbl(Total) $GDefs(Lang)] [format "%.3e" $Sim(EmMass)] unit
[lindex $Lbl(Height) $GDefs(Lang)] $Sim(EmTop) m
[lindex $Lbl(Radius) $GDefs(Lang)] $Sim(EmRadius) m
[lindex $Lbl(Meteo) $GDefs(Lang)] $Sim(Meteo)"
              }
      TRAJECT {  append text "[lindex $Lbl(SimDur) $GDefs(Lang)] $Sim(Duration) Hr(s)
[lindex $Lbl(Duration) $GDefs(Lang)] NIL
[lindex $Lbl(Total) $GDefs(Lang)] NIL
[lindex $Lbl(Height) $GDefs(Lang)] NIL
[lindex $Lbl(Radius) $GDefs(Lang)] NIL
[lindex $Lbl(Meteo) $GDefs(Lang)] $Sim(Meteo)
[lindex $Lbl(Mode) $GDefs(Lang)] $Sim(Mode)"
              }
   }

   $Frame.page.canvas itemconfigure CHEMTEXT -text $text

   SPI::LayoutUpdate $Frame
}
