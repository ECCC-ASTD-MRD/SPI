#===============================================================================
# Environnement Canada
# Centre Meteorologique Canadian
# 2100 Trans-Canadienne
# Dorval, Quebec
#
# Projet   : Exemples de script.
# Fichier  : Scenario.tcl
# Creation : Janvier 2018 - J.P.Gauthier - CMC/CMOE
#
# Description:
#    Scenario pour produire une animation
#
# Arguments  :
#
# Remarques :
#
#===============================================================================

namespace eval Macro::Scenario {} {
   global env
   variable Param
   variable Data

   colormap create FIREWORK_MAP -file $env(HOME)/.spi/Colormap/OTH_FIREWORK.rgba
   colormap create RIVER_MAP    -file $env(HOME)/.spi/Colormap/REC_Col.std1.rgba
   colormap create DISC_MAP     -file $env(HOME)/.spi/Colormap/HYDRO_Discharge.rgba
   colormap create CLOUDG_MAP   -file $env(HOME)/.spi/Colormap/OTH_Clouds_Greyish.rgba
   colormap create BIAS_MAP     -file $env(HOME)/.spi/Colormap/OTH_Bias.rgba

   set Param(Models) { FIREWORK SPILL_VC_HARBOUR MLDPN-ZH MLDPN-CV HYDRO PANAM  }
   
   set Param(LabelFIREWORK)  "FIREWORK - 10km"
   set Param(ToFIREWORK)     SC_FIREWORK
   set Param(VarFIREWORK)    { AF }
   set Param(LoopFIREWORK)   1
   set Param(DescFIREWORK)   "Simulations des feux de forêts du mois d'Août 2018 sur la Colombie Britanique\nForest fire simulation for August 2018 over British Columbia"                  
   set Param(ParamFIREWORK)  {{ -colormap FIREWORK_MAP -font LABEL -rendertexture 1 -color black -min 1e-7 -max 1e-4 -desc "PM 2.5" -unit µg/m³ -interlabels { 1e-7 1e-6 1e-5 1e-4} -showmap True }} 

   set Param(LabelSPILL_VC_HARBOUR)  "COSMoS - 5m"
   set Param(ToSPILL_VC_HARBOUR)     SC_SPILL_VC_HARBOUR
   set Param(VarSPILL_VC_HARBOUR)    { ZH }
   set Param(LoopSPILL_VC_HARBOUR)   1
   set Param(DescSPILL_VC_HARBOUR)   "Simulations sur 48 heures de dispersion de nappes d'huiles à Vancouver de la suite de Modélisation de Déversement d'Huile du Canada\nOil spill dispersion simulations over 48 hours in Vancouver from the Canadian Oil Spill Modelling Suite"                  
   set Param(ParamSPILL_VC_HARBOUR)  {{ -colormap RIVER_MAP -font LABEL -rendergrid 3 -color black -showmap False }} 

   set Param(LabelMLDPN-ZH)   "MLDPn - 25km"
   set Param(ToMLDPN-ZH)       SC_EYA
   set Param(VarMLDPN-ZH)      { ZH }
   set Param(LoopMLDPN-ZH)     1
   set Param(DescMLDPN-ZH)     "Simulation sur 5 jour de l'éruption du volcan Eyjafjallajökull ayant perturbé l'espace aérien de Mars à Juin 2010\nSimulation over 5 days of the Eyjafjallajökull volcano eruption which affected flight from March to June 2010"
   set Param(ParamMLDPN-ZH)    {{ -colormap RIVER_MAP -font LABEL -renderparticle 3 -topographyfactor 60 -rendergrid 0 -min 1 -max 10000 -interlabels { 0 2000 4000 6000 8000 10000 } -value INTEGER 0 -desc "Hauteur/Height" -unit "m" -showmap True }}
  
   set Param(LabelMLDPN-CV)   "MLDPn - 25km"
   set Param(ToMLDPN-CV)       SC_EYA
   set Param(VarMLDPN-CV)      { CV }
   set Param(LoopMLDPN-CV)     1
   set Param(DescMLDPN-CV)     "Simulation sur 5 jour de l'éruption du volcan Eyjafjallajökull ayant perturbé l'espace aérien de Mars à Juin 2010\nSimulation over 5 days of the Eyjafjallajökull volcano eruption which affected flight from March to June 2010"
   set Param(ParamMLDPN-CV)    {{ -colormap RIVER_MAP -font LABEL -rendertexture True -rendervolume True -intervals { 10 100 1000 } -transparency 50 -topographyfactor 60 -value INTEGER 0 -desc "Concentrations" -unit µg/m³ -showmap True }}
   
   set Param(LabelHYDRO)   "WATROUTE - 700m, NEMO - 2.5km" 
   set Param(ToHYDRO)       SC_HYDRO
   set Param(VarHYDRO)      { DISC LLA }
   set Param(LoopHYDRO)     1
   set Param(DescHYDRO)     "Simulation du bassin hydrologique des Grands Lacs\nGreat Lakes hydrological basin simulation"
   set Param(ParamHYDRO)    {{ -colormap DISC_MAP -font LABEL -rendertexture True -desc Débit/Discharge -unit m³ -intervals { 1e-4 1e-3 1e-2 1e-2 1 1e1 1e2 1e3 } -showmap True } { -colormap BIAS_MAP -font LABEL -factor 100 -rendertexture True -value INTEGER 0 -desc Niveau/Level -unit cm -intervals { -120 -100 -75 -50 -40 -30 -20 -10 -5 -2 -1 1 2 5 10 20 30 40 50 75 100 } -interlabels { -120 "" -75 "" -40 "" -20 "" -5 "" -1 "" 2 "" 10 "" 30 "" 50 "" 100 } -showmap True }}

   set Param(LabelPANAM)   "Panam - 250m" 
   set Param(ToPANAM)       SC_PANAM
   set Param(VarPANAM)      { J8 NT }
   set Param(LoopPANAM)     1
   set Param(DescPANAM)     "Température de surface et couvert nuageux d'une simulation pour les jeux Panaméricain de 2014\nSimulation of the surface temperature and cloud cover for the Panamerican games of 2014"
   set Param(ParamPANAM)    {{ -colormap RIVER_MAP -font LABEL -rendertexture 1 -min 285 -max 325 -interlabels "285 295 305 315 325" -value INTEGER 0 -desc "Temp surface" -unit K -mapbelow True -mapabove True -showmap True } { -colormap CLOUDG_MAP -rendertexture 1 -showmap False}} 
   set Param(TimePANAM)     { "18/07/2014 15:00" "18/07/2014 23:00" }

}

proc Macro::Scenario::PostManual { } {

   #----- Urban 3D   
   $Page::Data(Canvas) itemconfigure COLORBAR -data ""
   glrender -zbuffer True
   ProjCam::Select $Page::Data(Frame) $Page::Data(Frame) SC_TOR_Z
   Macro::Scenario::Appears "" "" "CUDM - 5m" "Dispersion en milieu urbain sur Toronto du Système de Modélisation Urbain Canadien\nUrban modeling dispersion in Toronto fromt the Canadian Urban Dispersion Modeling System"
   projection configure $Page::Data(Frame) -draw 0

   set layers [ogrfile open TORFILE read /home/binops/afse/eer/links/geo/Vector/Cities/Toronto/Toronto_DT_2013_mtm3degree_v2.shp]
   eval ogrlayer read TOR [lindex $layers 0]
   ogrlayer configure TOR -outline #000000 -fill #AAAAAA -width 1 
   Viewport::Assign $Page::Data(Frame) $Page::Data(VP) TOR
   for { set a 0 } { $a <= 1.0} { set a [expr $a+0.05] } {
       ogrlayer configure TOR -extrude EleZ -extrudefactor $a
       Macro::Scenario::Print
   }
   
   ProjCam::Select $Page::Data(Frame) $Page::Data(Frame) SC_TOR_TOP
   
   #----- Urban Model CUDM  
   fstdfile open MODEL read /users/dor/afsr/005/links/storage/Projects/UrbanX/Toronto/flow.fstd
   fstdfield read UV MODEL -1 "" -1 -1 -1 "" "U"
   fstdfield configure UV -colormap RIVER_MAP -transparency 0 -rendervector ARROW -sample 1 -color black -mapall True -min 0 -max 2 -mapabove True
   Macro::Scenario::Appears UV fstdfield
   fstdfile close MODEL

   Macro::Scenario::Pause 10
   CVProgressBar::Set $Page::Data(Frame) 0
   
   #----- Show parcels from top
   fstdfile open MODEL read /users/dor/afsr/005/links/storage/Projects/UrbanX/Toronto/ppxy.fstd
   set dates [fstdfile info MODEL DATEV ZH]
   foreach date $dates {
      CVProgressBar::Incr $Page::Data(Frame) [expr 1.0/([llength $dates]*2)]
      fstdfield read ZH MODEL [fstdstamp fromseconds $date] "" -1 -1 -1 "" "ZH"
      fstdfield configure ZH -colormap RIVER_MAP -renderparticle 3 -color black -min 0 -max 300 -font LABEL -desc Hauteur/Height -unit m -interlabels { 0 50 100 150 200 250 300 } -showmap True
      Viewport::Assign $Page::Data(Frame) $Page::Data(VP) { ZH }
      $Page::Data(Canvas) itemconfigure COLORBAR -data ZH -transparency 75
      Macro::Scenario::Print
   }
    
   Macro::Scenario::Pause 10
   
   #----- Show Parcel 3D
   ProjCam::Select $Page::Data(Frame) $Page::Data(Frame) SC_TOR_ZH
   Macro::Scenario::Disappears { UV ZH } fstdfield "" "Dispersion en milieu urbain sur Toronto du Système de Modélisation Urbain Canadien\nUrban modeling dispersion in Toronto fromt the Canadian Urban Dispersion Modeling System"
   
   foreach date $dates {
      CVProgressBar::Incr $Page::Data(Frame) [expr 1.0/([llength $dates]*2)]
      fstdfield read ZH MODEL [fstdstamp fromseconds $date] "" -1 -1 -1 "" "ZH"
      fstdfield configure ZH -colormap RIVER_MAP -transparency 100 -renderparticle 3 -color black -min 0 -max 300 -font LABEL -desc Hauteur/Height -unit m -interlabels { 0 50 100 150 200 250 300 } -showmap True
      Viewport::Assign  $Page::Data(Frame) $Page::Data(VP) { ZH }
      $Page::Data(Canvas) itemconfigure COLORBAR -data ZH -transparency 75
      Macro::Scenario::Print
   }
   fstdfile close MODEL

   Macro::Scenario::Pause
   Macro::Scenario::Disappears ZH
   
   #----- Show volume 
   $Page::Data(Canvas) itemconfigure COLORBAR -data "" 
   fstdfile open MODEL read /users/dor/afsr/005/links/storage/Projects/UrbanX/Toronto/concgrid.fstd
   fstdfield read CV MODEL -1 "" -1 -1 -1 "" "CV"
   fstdfield configure CV -colormap RIVER_MAP -rendertexture True -rendervolume True -intervals { 1e-8 1e-7 1e-6 } -transparency 75
   Viewport::Assign  $Page::Data(Frame) $Page::Data(VP) { CV }
   
   fstdfile close MODEL

   Macro::Scenario::Pause

   #----- Fly around the volume 360
   set flightplan {{{-0.3525334878582205 0.29581071963947125 1.887815385136401} { 0.0 0.0 1.0 } {0.7079653238552617 -0.536114222874593 0.4597462781253218} 12590.081485111406} {{-0.04010905405515031 0.45844858566463587 1.887815385136401} {0.0 0.0 1.0} {0.12151707885780438 -0.8796970838281667 0.4597462781253215} 12590.081485111406} {{0.29581071963947125 0.3525334878582205 1.887815385136401} {0.0 0.0 1.0} {-0.536114222874593 -0.7079653238552617 0.4597462781253218} 12590.081485111406} {{0.45844858566463587 0.04010905405514994 1.887815385136401} {0.0 0.0 1.0} {-0.8796970838281671 -0.12151707885780254 0.45974627812532165} 12590.081485111406} {{0.3525334878582202 -0.2958107196394716 1.887815385136401} {0.0 0.0 1.0} {-0.7079653238552615 0.5361142228745938 0.4597462781253213} 12590.081485111406} {{0.04010905405514996 -0.45844858566463587 1.887815385136401} {0.0 0.0 1.0} {-0.12151707885780255 0.8796970838281671 0.45974627812532165} 12590.081485111406} {{-0.29581071963947153 -0.3525334878582202 1.887815385136401} {0.0 0.0 1.0} {0.5361142228745952 0.7079653238552606 0.45974627812532115} 12590.081485111406} {{-0.45844858566463587 -0.040109054055149994 1.887815385136401} {0.0 0.0 1.0} {0.8796970838281671 0.12151707885780255 0.45974627812532165} 12590.081485111406} {{-0.3525334878582205 0.29581071963947125 1.887815385136401} { 0.0 0.0 1.0 } {0.7079653238552617 -0.536114222874593 0.4597462781253218} 12590.081485111406}}

   projcam define $Page::Data(Frame) -path $flightplan
   for { set pos 0.0 } { $pos<[llength $flightplan] } { set pos [expr $pos+0.025] } {
      projcam define $Page::Data(Frame) -fly $pos
      Macro::Scenario::Print
   }
}