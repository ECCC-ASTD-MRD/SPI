#===============================================================================
# Environnement Canada
# Centre Meteorologique Canadian
# 2100 Trans-Canadienne
# Dorval, Quebec
#
# Projet   : SPI (Layouts)
# Fichier  : FIRE.tcl
# Creation : Janvier 2014
#
# Description:
#    Layout pour les feux
#
#===============================================================================

namespace eval FIRE {
   variable Data

    catch {
      colormap create FIREMAPDEFAULT
      colormap read FIREMAPDEFAULT $env(HOME)/.spi/Colormap/OTH_White2Black.rgba
      colormap configure FIREMAPDEFAULT -RGBAratio 100 100 100 75
   }
   
   set Data(Field)      ""
        
   set Data(Locales) { fr_ca en_us }
   
   set Lbl(BlaBla) { "NOTE AUX USAGERS:\n\nLes teintes plus foncées indiquent une densité relative importante de fumée modélisée à partir d'un scénario d'émission hypothétique basé sur des informations disponibles pour cet événement. Aucunes unités de concentration ne sont assignées à ces intervalles de teinte. Les utilisateurs autorisés peuvent demander de l'aide dans l'interprétation en communiquant avec la section de la réponse aux urgences environnementales (SRUE) du Centre météorologique canadien (CMC)."
                     "NOTICE TO USERS:\n\nDarker shades indicate higher relative density of modelled smoke from a hypothetical emission scenario based on available information for this event. No concentration units are assigned to these shading intervals. Authorized users may request assistance in interpretation by contacting the environmental emergency response section (EERS) at the Canadian Meteorological Centre (CMC)." }

   set Lbl(Less)     { "Moins" "Less" }
   set Lbl(More)     { "Plus"  "More" }
   set Lbl(Unit)     { "unité" "unit" }
   set Lbl(Unknown)  { "Inconnu" "Unknown" }
   set Lbl(Model)    { "Modèle de dispersion         :" "Dispersion Model             :" }
   set Lbl(Meteo)    { "Modèle météorologique PNT    :" "NWP Meteorological Model     :" }
   set Lbl(Name)     { "Nom de la source             :" "Source Name                  :" }
   set Lbl(Location) { "Localisation de la source    :" "Source location              :" }
   set Lbl(Start)    { "Date du début du relâchement :" "Release Starting Date-Time   :" }
   set Lbl(SimDur)   { "Durée de la simulation       :" "Simulation Duration          :" }
   set Lbl(Duration) { "Durée du relâchement         :" "Release Duration             :" }
   set Lbl(Total)    { "Quantité totale relâchée     :" "Total Release Quantity       :" }
   set Lbl(Height)   { "Hauteur initiale maximale    :" "Initial Maximum Plume Height :" }
   set Lbl(Radius)   { "Rayon de dispersion initial  :" "Initial Dispersion Radius    :" }
}

proc FIRE::Layout { Frame } {
   global GDefs
   variable Data
   variable Lbl

   Page::Size $Frame 970 830

   set Viewport::Map(Draw) 0 ;#Do not draw built-in geo
   
   #----- Affichage des Viewports
   set Data(VP) [Viewport::Create $Frame 5 5 960 560 1 0]

   $Page::Data(Canvas) create rectangle 5 5 475 40 -width 1 -fill white -transparency 75

   #----- Positionnement des ColorBars
   set ColorBar::Param(Width)  20
   set ColorBar::Data(Active$Frame) 1
   set ColorBar::Data($Data(VP)0) [list 5 570 960 75 CBAR$Data(VP)0]

   set SPI::Data(ShowColorBar$Frame) 1
   set SPI::Data(ShowLOGO) 1
   SPI::DrawImage $Frame LOGO 8 7 nw 1 0 0

   #----- Affichage de l'echelle
   CVScale::Create $Frame 135 540 250
   set SPI::Data(ShowScale$Frame) 1

   #----- Affichage de l'horloge
   CVClock::Create $Frame  860 485
   set SPI::Data(ShowClock$Frame) 1

   #----- Affichage de la boite de texte
   set Data(TextSim)  [CVText::Create $Frame 5   650 475 175]
   set Data(TextWarn) [CVText::Create $Frame 485 650 480 175 [lindex $Lbl(BlaBla) $GDefs(Lang)]]
   
   Mapper::DepotWare::TMS::Load OpenStreetMap 2

   Page::UpdateItems $Frame
}

proc FIRE::LayoutUpdate { Frame } {
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
   set ip1 [string trim [fstdfield define $field -IP1]]
   set ip3 [string trim [fstdfield define $field -IP3]]
   set tv  [string trim [fstdfield define $field -TYPVAR]]

   #----- If the field is changed, updtae level list
   if { $Data(Field)!="$nv $tv $iso $ip1 $ip3" } {
      set Data(Field) "$nv $tv $iso $ip1 $ip3"
      
      #----- Define intervals
      set mm [FieldFunc::MinMax $field]
      set log [expr int(floor(log10([lindex $mm 1])))]
      for { set i 5 } { $i } { incr i -1 } {
         lappend inters 1e$log
         incr log -1
      }
         
      fstdfield configure $field -rendertexture 1 -rendercontour 1 -mapall False -color #000000 -dash "" -desc " " \
         -font XFont12 -min [lindex $mm 0] -max [lindex $mm 1] -intervals [lreverse $inters] -colormap FIREMAPDEFAULT    
   
      #----- Get the simulation info
      if { [set info [Info::Read [fstdfield define $field -FID]]]=="" } {
         return
      }
      Info::Decode ::FIRE::Sim $info

      if { [llength $Sim(Name)]>1 } {
         set coords ""
         foreach src $Sim(Name) lat $Sim(Lat) lon $Sim(Lon) {
            append coords [format "(%.2f %.2f) " $lat $lon]
         }
      } else {
         set coords [format "%.4f %.4f" $Sim(Lat) $Sim(Lon)]
      }
      
      set date [DateStuff::StringDateFromSeconds [clock scan "$Sim(AccYear)$Sim(AccMonth)$Sim(AccDay) $Sim(AccHour):$Sim(AccMin)"] $GDefs(Lang)]
      set text "[lindex $Lbl(Model) $GDefs(Lang)] $Sim(Model)
[lindex $Lbl(Name) $GDefs(Lang)] $Sim(Name)
[lindex $Lbl(Location) $GDefs(Lang)] $coords
[lindex $Lbl(Start) $GDefs(Lang)] $date
[lindex $Lbl(Meteo) $GDefs(Lang)] $Sim(Meteo)\n"

      switch $Sim(Model) {
         MLDP0   -
         MLDP1   {
         if { [string is double $Sim(EmIsoQuantity)] } {
            set q [format "%.3e [lindex $Lbl(Unit) $GDefs(Lang)]" $Sim(EmIsoQuantity)]
         } else {
               set q $Sim(EmIsoQuantity)
         }
         append text "[lindex $Lbl(SimDur) $GDefs(Lang)] $Sim(Duration) h
[lindex $Lbl(Duration) $GDefs(Lang)] $Sim(EmTotalDuration) s
[lindex $Lbl(Total) $GDefs(Lang)] $q
[lindex $Lbl(Height) $GDefs(Lang)] $Sim(EmHeight) m
[lindex $Lbl(Radius) $GDefs(Lang)] $Sim(EmRadius) "
               }
         MLDPn  {
         MLDPn::ScenarioDecode $Sim(SrcType) $Sim(Scenario) "|"
         if { [set idx [lsearch -nocase -exact $MLDPn::Sim(EmIsos) $iso]]!=-1 } {
            set q [format "%.3e [lindex $Lbl(Unit) $GDefs(Lang)]" [lindex $MLDPn::Sim(EmMassIsos) $idx]]
         } else {
            set q NIL
         }
         append text "[lindex $Lbl(SimDur) $GDefs(Lang)] $Sim(Duration) h
[lindex $Lbl(Duration) $GDefs(Lang)] $MLDPn::Sim(EmTotalDuration) s
[lindex $Lbl(Total) $GDefs(Lang)] $q
[lindex $Lbl(Height) $GDefs(Lang)] $MLDPn::Sim(EmHeight.0) m
[lindex $Lbl(Radius) $GDefs(Lang)] $MLDPn::Sim(EmRadius.0) m"
               }
         MLCD    { append text "[lindex $Lbl(SimDur) $GDefs(Lang)] $Sim(DurMin) min
[lindex $Lbl(Duration) $GDefs(Lang)] $Sim(EmDurationMin) min(s)
[lindex $Lbl(Total) $GDefs(Lang)] [format "%.3e" $Sim(EmMass)] unit
[lindex $Lbl(Height) $GDefs(Lang)] $Sim(EmTop) m
[lindex $Lbl(Radius) $GDefs(Lang)] $Sim(EmRadius) m"
               }
         TRAJECT {  append text "[lindex $Lbl(SimDur) $GDefs(Lang)] $Sim(Duration) h"
               }
      }
      CVText::Update $Frame $Data(TextSim) $text
   }

   #----- Define labels
   set lbls [lrepeat [llength [fstdfield configure $field -intervals]] ""]
   lset lbls 0   [lindex $Lbl(Less) $GDefs(Lang)]
   lset lbls end [lindex $Lbl(More) $GDefs(Lang)]
   
   fstdfield configure $field -interlabels $lbls
         
   SPI::LayoutUpdate $Frame
}
