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
#   set Param(Models)    List of models to include, then for each model
#      set Param(File...)   RPN file for the model
#      set Param(Label...)  Label for the model
#      set Param(To...)     Camera name to use to zoom to model
#      set Param(Fly...)    Flight path around the model                                           [Optional]
#      set Param(Rotate...) Number of degrees by which to rotate the globe 360                     [Optional]
#      set Param(Time...)   Time range to animate { "YYYY/MM/DD HH:MM" "YYYY/MM/DD HH:MM" }        [Optional]
#      set Param(Var...)    FSTD variable list
#      set Param(OGR...)    OGR data to display                                                    [Optional]
#      set Param(Loop...)   Number of loop over time
#      set Param(Desc...)   Long description of the model                  
#      set Param(Param...)  FSTD configuration parameter for each var 
#===============================================================================

namespace eval Macro::Scenario {} {
   global env
   variable Param
   variable Data

   colormap create MACH_MAP     -file $env(HOME)/.spi/Colormap/OTH_DIRT.rgba
   colormap create FIREWORK_MAP -file $env(HOME)/.spi/Colormap/OTH_FIREWORK.rgba
   colormap create RIVER_MAP    -file $env(HOME)/.spi/Colormap/REC_Col.std1.rgba
   colormap create DISC_MAP     -file $env(HOME)/.spi/Colormap/HYDRO_Discharge.rgba
   colormap create CLOUDG_MAP   -file $env(HOME)/.spi/Colormap/OTH_Clouds_Greyish.rgba
   colormap create BIAS_MAP     -file $env(HOME)/.spi/Colormap/OTH_Bias.rgba

   set layers [ogrfile open AVRFILE read /fs/cetusops/fs1/prod/cmoe/geo/Vector/Global/Aviation/airroutes.shp]
   eval ogrlayer read AVR [lindex $layers 0]
   ogrlayer configure AVR -outline #009400 -fill #AAAAAA -width 1 -transparency 4

   set layers [ogrfile open TORFILE read /home/binops/afse/eer/links/geo/Vector/Cities/Toronto/Toronto_DT_2013_mtm3degree_v2.shp]
   eval ogrlayer read TOR [lindex $layers 0]
   ogrlayer configure TOR -outline #000000 -fill #AAAAAA -width 1 -extrude EleZ

   set Param(Loop)   False
   set Param(Models) { RAQDPS FIREWORK MLDPN-ZH MLDPN-CV HYDRO PANAM CUDM-ZH CUDM-ZH2 CUDM-CV SPILL_VC_HARBOUR }
#   set Param(Models) { SPILL_VC_HARBOUR }
   
   set Param(LabelRAQDPS)  "RAQDPS - 10km"
   set Param(FileRAQDPS)   /fs/cetus/fs2/ops/cmoe/afsr005/Scenario/data/RAQDPS.fstd
   set Param(ToRAQDPS)     SC_MACH
   set Param(VarRAQDPS)    { AF }
   set Param(LoopRAQDPS)   1
   set Param(DescRAQDPS)   "Simulations de particules fines du mois d'Août 2018 du Système Régional de Prévision Déterministe de la Qualité de l'Air \nFine particulate simulation for August 2018 from the Regional Air Quality Deterministic Prediction System"                  
   set Param(ParamRAQDPS)  {{ -colormap MACH_MAP -font LABEL -rendertexture 1 -color black -min 1 -max 50 -desc "PM 2.5" -unit µg/m³ -interlabels { 1 10 20 30 40 50 } -showmap True }} 

   set Param(LabelFIREWORK)  "FIREWORK - 10km"
   set Param(FileFIREWORK)   /fs/cetus/fs2/ops/cmoe/afsr005/Scenario/data/FIREWORK.fstd
   set Param(ToFIREWORK)     SC_FIREWORK
   set Param(VarFIREWORK)    { AF }
   set Param(LoopFIREWORK)   1
   set Param(DescFIREWORK)   "Simulations des feux de forêts du mois d'Août 2018, côte ouest de l'Amérique du Nord\nForest fire simulation for August 2018 over North American west coast"                  
   set Param(ParamFIREWORK)  {{ -colormap FIREWORK_MAP -font LABEL -rendertexture 1 -color black -min 1e-7 -max 1e-4 -desc "PM 2.5" -unit µg/m³ -interlabels { 1e-7 1e-6 1e-5 1e-4 } -showmap True }} 

   set Param(LabelSPILL_VC_HARBOUR)  "COSMoS - 5m"
   set Param(FileSPILL_VC_HARBOUR)   /fs/cetus/fs2/ops/cmoe/afsr005/Scenario/data/SPILL_VC_HARBOUR.fstd
#   set Param(FlySPILL_VC_HARBOUR)   {{{-0.3525334878582205 0.29581071963947125 1.887815385136401} { 0.0 0.0 1.0 } {0.7079653238552617 -0.536114222874593 0.4597462781253218} 12590.081485111406}
#{{ 0.0 0.0 2.0 } { 0.0 0.0 1.0 } { 0.0 1.0 0.0 } 49.819425290903496 -98.27496009883939 }
#{{ 0.0 0.0 2.0 } { 0.0 0.0 1.0 } { 0.0 1.0 0.0 } 760.0760681620733 49.31098208213309 -123.11603077194742}}

#ProjCam::FlyTo $Page::Data(Frame) {{{-0.3525334878582205 0.29581071963947125 1.887815385136401} { 0.0 0.0 1.0 } {0.7079653238552617 -0.536114222874593 0.4597462781253218} 12590.081485111406 43.65108083377559 -79.38043021925478 }
#                                  {{ 0.0 0.0 2.0 } { 0.0 0.0 1.0 } { 0.0 1.0 0.0 } 4.756828460010884 49.819425290903496 -98.27496009883939}
#                                  {{ 0.0 0.0 2.0 } { 0.0 0.0 1.0 } { 0.0 1.0 0.0 } 760.0760681620733 49.31098208213309 -123.11603077194742}}

   set Param(ToSPILL_VC_HARBOUR)     SC_SPILL_VC_HARBOUR
   set Param(VarSPILL_VC_HARBOUR)    { ZH }
   set Param(LoopSPILL_VC_HARBOUR)   1
   set Param(DescSPILL_VC_HARBOUR)   "Simulations sur 48 heures de dispersion de nappes d'huiles à Vancouver de la suite de Modélisation de Déversement d'Huile du Canada\nOil spill dispersion simulations over 48 hours in Vancouver from the Canadian Oil Spill Modelling Suite"                  
   set Param(ParamSPILL_VC_HARBOUR)  {{ -colormap RIVER_MAP -font LABEL -rendergrid 3 -transparency 25 -color black -showmap False }} 

   set Param(LabelMLDPN-ZH)   "MLDPn - 25km"
   set Param(FileMLDPN-ZH)     /fs/cetus/fs2/ops/cmoe/afsr005/Scenario/data/MLDPN-ZH.fstd
   set Param(ToMLDPN-ZH)       SC_EYA
   set Param(VarMLDPN-ZH)      { ZH }
   set Param(LoopMLDPN-ZH)     1
   set Param(DescMLDPN-ZH)     "Simulation sur 5 jour de l'éruption du volcan Eyjafjallajökull ayant perturbé l'espace aérien de Mars à Juin 2010\nSimulation over 5 days of the Eyjafjallajökull volcano eruption which affected flight from March to June 2010"
   set Param(ParamMLDPN-ZH)    {{ -colormap RIVER_MAP -font LABEL -renderparticle 3 -topographyfactor 60 -rendergrid 0 -min 1 -max 10000 -interlabels { 0 2000 4000 6000 8000 10000 } -value INTEGER 0 -desc "Hauteur/Height" -unit "m" -showmap True }}
   set Param(OGRMLDPN-ZH)      AVR
  
   set Param(LabelMLDPN-CV)   "MLDPn - 25km"
   set Param(FileMLDPN-CV)     /fs/cetus/fs2/ops/cmoe/afsr005/Scenario/data/MLDPN-CV.fstd
   set Param(ToMLDPN-CV)       SC_EYA
   set Param(VarMLDPN-CV)      { CV }
   set Param(LoopMLDPN-CV)     1
   set Param(DescMLDPN-CV)     "Simulation sur 5 jour de l'éruption du volcan Eyjafjallajökull ayant perturbé l'espace aérien de Mars à Juin 2010\nSimulation over 5 days of the Eyjafjallajökull volcano eruption which affected flight from March to June 2010"
   set Param(ParamMLDPN-CV)    {{ -colormap RIVER_MAP -font LABEL -rendertexture True -rendervolume True -intervals { 10 100 1000 } -transparency 50 -topographyfactor 60 -value INTEGER 0 -desc "Concentrations" -unit µg/m³ -showmap True }}
   set Param(OGRMLDPN-CV)      AVR
   
   set Param(LabelHYDRO)   "WATROUTE - 700m, NEMO - 2.5km" 
   set Param(FileHYDRO)     /fs/cetus/fs2/ops/cmoe/afsr005/Scenario/data/HYDRO.fstd
   set Param(ToHYDRO)       SC_HYDRO
   set Param(VarHYDRO)      { DISC LLA }
   set Param(LoopHYDRO)     1
   set Param(DescHYDRO)     "Simulation du bassin hydrologique des Grands Lacs\nGreat Lakes hydrological basin simulation"
   set Param(ParamHYDRO)    {{ -colormap DISC_MAP -font LABEL -rendertexture True -desc Débit/Discharge -unit m³ -intervals { 1e-4 1e-3 1e-2 1e-2 1 1e1 1e2 1e3 } -showmap True } { -colormap BIAS_MAP -font LABEL -factor 100 -rendertexture True -value INTEGER 0 -desc Niveau/Level -unit cm -intervals { -120 -100 -75 -50 -40 -30 -20 -10 -5 -2 -1 1 2 5 10 20 30 40 50 75 100 } -interlabels { -120 "" -75 "" -40 "" -20 "" -5 "" -1 "" 2 "" 10 "" 30 "" 50 "" 100 } -showmap True }}

   set Param(LabelPANAM)    "Panam - 250m" 
   set Param(FilePANAM)     /fs/cetus/fs2/ops/cmoe/afsr005/Scenario/data/PANAM.fstd
   set Param(ToPANAM)       SC_PANAM
   set Param(VarPANAM)      { J8 NT }
   set Param(LoopPANAM)     1
   set Param(DescPANAM)     "Température de surface et couvert nuageux d'une simulation pour les jeux Panaméricain de 2014\nSimulation of the surface temperature and cloud cover for the Panamerican games of 2014"
   set Param(ParamPANAM)    {{ -colormap RIVER_MAP -font LABEL -rendertexture 1 -min 285 -max 325 -interlabels "285 295 305 315 325" -value INTEGER 0 -desc "Temp surface" -unit K -mapbelow True -mapabove True -showmap True } { -colormap CLOUDG_MAP -rendertexture 1 -showmap False}} 
   set Param(TimePANAM)     { "18/07/2014 15:00" "18/07/2014 23:00" }

   set Param(LabelCUDM-ZH)  "CUDM - 5m" 
   set Param(FileCUDM-ZH)   /fs/cetus/fs2/ops/cmoe/afsr005/Scenario/data/CUDM-ZH.fstd
   set Param(ToCUDM-ZH)     SC_TOR_TOP
   set Param(VarCUDM-ZH)    { U ZH }
   set Param(LoopCUDM-ZH)   1
   set Param(DescCUDM-ZH)   "Dispersion en milieu urbain sur Toronto du Système de Modélisation Urbain Canadien\nUrban modeling dispersion in Toronto fromt the Canadian Urban Dispersion Modeling System"
   set Param(ParamCUDM-ZH)  {{ -colormap RIVER_MAP -transparency 0 -rendervector ARROW -sample 1 -color black -mapall True -min 1e-5 -max 2 -mapabove True -showmap False } { -colormap RIVER_MAP -renderparticle 3 -color black -min 0 -max 300 -font LABEL -desc Hauteur/Height -unit m -interlabels { 0 50 100 150 200 250 300 } -showmap True}} 
   set Param(OGRCUDM-ZH)    TOR
   
   set Param(LabelCUDM-ZH2) "CUDM - 5m" 
   set Param(FileCUDM-ZH2)  /fs/cetus/fs2/ops/cmoe/afsr005/Scenario/data/CUDM-ZH.fstd
   set Param(ToCUDM-ZH2)    SC_TOR_ZH
   set Param(VarCUDM-ZH2)   { ZH }
   set Param(LoopCUDM-ZH2)  1
   set Param(DescCUDM-ZH2)  "Dispersion en milieu urbain sur Toronto du Système de Modélisation Urbain Canadien\nUrban modeling dispersion in Toronto fromt the Canadian Urban Dispersion Modeling System"
   set Param(ParamCUDM-ZH2) {{ -colormap RIVER_MAP -renderparticle 3 -color black -min 0 -max 300 -topographyfactor 1.0 -font LABEL -desc Hauteur/Height -unit m -interlabels { 0 50 100 150 200 250 300 } -showmap True}} 
   set Param(OGRCUDM-ZH2)   TOR
   
   set Param(LabelCUDM-CV) "CUDM - 5m" 
   set Param(FileCUDM-CV)  /fs/cetus/fs2/ops/cmoe/afsr005/Scenario/data/CUDM-CV.fstd
   set Param(ToCUDM-CV)    SC_TOR_ZH
   set Param(FlyCUDM-CV)   {{{-0.3525334878582205 0.29581071963947125 1.887815385136401} { 0.0 0.0 1.0 } {0.7079653238552617 -0.536114222874593 0.4597462781253218} 12590.081485111406} {{-0.04010905405515031 0.45844858566463587 1.887815385136401} {0.0 0.0 1.0} {0.12151707885780438 -0.8796970838281667 0.4597462781253215} 12590.081485111406} {{0.29581071963947125 0.3525334878582205 1.887815385136401} {0.0 0.0 1.0} {-0.536114222874593 -0.7079653238552617 0.4597462781253218} 12590.081485111406} {{0.45844858566463587 0.04010905405514994 1.887815385136401} {0.0 0.0 1.0} {-0.8796970838281671 -0.12151707885780254 0.45974627812532165} 12590.081485111406} {{0.3525334878582202 -0.2958107196394716 1.887815385136401} {0.0 0.0 1.0} {-0.7079653238552615 0.5361142228745938 0.4597462781253213} 12590.081485111406} {{0.04010905405514996 -0.45844858566463587 1.887815385136401} {0.0 0.0 1.0} {-0.12151707885780255 0.8796970838281671 0.45974627812532165} 12590.081485111406} {{-0.29581071963947153 -0.3525334878582202 1.887815385136401} {0.0 0.0 1.0} {0.5361142228745952 0.7079653238552606 0.45974627812532115} 12590.081485111406} {{-0.45844858566463587 -0.040109054055149994 1.887815385136401} {0.0 0.0 1.0} {0.8796970838281671 0.12151707885780255 0.45974627812532165} 12590.081485111406} {{-0.3525334878582205 0.29581071963947125 1.887815385136401} { 0.0 0.0 1.0 } {0.7079653238552617 -0.536114222874593 0.4597462781253218} 12590.081485111406}}
   set Param(VarCUDM-CV)   { CV }
   set Param(LoopCUDM-CV)  1
   set Param(DescCUDM-CV)  "Dispersion en milieu urbain sur Toronto du Système de Modélisation Urbain Canadien\nUrban modeling dispersion in Toronto fromt the Canadian Urban Dispersion Modeling System"
   set Param(ParamCUDM-CV) {{ -colormap RIVER_MAP -rendertexture True -rendervolume True -intervals { 1e-8 1e-7 1e-6 } -topographyfactor 1.0 -transparency 0 -showmap False}} 
   set Param(OGRCUDM-CV)   TOR 
}
