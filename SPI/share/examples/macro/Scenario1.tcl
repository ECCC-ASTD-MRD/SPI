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

   colormap create DEPTH_MAP  -file $env(HOME)/.spi/Colormap/REC_Col.std1.rgba; colormap configure DEPTH_MAP -min rgba 20  
   colormap create RIVER_MAP  -file $env(HOME)/.spi/Colormap/REC_Col.std1.rgba
   colormap create CLOUD_MAP  -file $env(HOME)/.spi/Colormap/OTH_EI.rgba
   colormap create WIND_MAP   -file $env(HOME)/.spi/Colormap/OTH_UUonEI.rgba
   colormap create ICE_MAP    -file $env(HOME)/.spi/Colormap/OTH_ICE.rgba
   colormap create SST_MAP    -file $env(HOME)/.spi/Colormap/OTH_SST.rgba

   set Param(Loop)   True
   set Param(Models) { GDPS RDPS HRDPS HRDPS-BC CIOPS CAPS RMPS-GSL RIOPS }
#    set Param(Models) { RIOPS }
    
   set Param(LabelGDPS)    "GDPS - 25km"
   set Param(FileGDPS)     /fs/cetus/fs2/ops/cmoe/afsr005/Scenario/data/GDPS.fstd
   set Param(ToGDPS)       "SC_GDPS"
   set Param(VarGDPS)      { EI UU }
   set Param(LoopGDPS)     1
   set Param(DescGDPS)     "Simulation sur 120 heures du Système Global de Prédiction Déterministe\n120 hours simulation from the Global Deterministic Prediction System"                 
   set Param(ParamGDPS)    {{ -outline black -rendertexture 1 -colormap CLOUD_MAP  -min 70 -max 320  -mapbelow True -mapabove True -showmap False } { -rendertexture 1 -colormap WIND_MAP  -min 1 -max 60 -mapbelow True -mapabove True -showmap False}}

   set Param(LabelRDPS)    "RDPS - 10km"
   set Param(FileRDPS)     /fs/cetus/fs2/ops/cmoe/afsr005/Scenario/data/RDPS.fstd
   set Param(ToRDPS)       SC_RDPS
   set Param(VarRDPS)      { EI UU }
   set Param(LoopRDPS)     1
   set Param(DescRDPS)     "Simulation sur 84 heures du Système Régional de Prédiction Déterministe\n84 hours simulation from the  Regional Deterministic Prediction System" 
   set Param(ParamRDPS)    {{ -outline black -rendertexture 1 -colormap CLOUD_MAP  -min 70 -max 320  -mapbelow True -mapabove True -showmap False } { -rendertexture 1 -colormap WIND_MAP  -min 1 -max 60 -mapbelow True -mapabove True -showmap False }}

   set Param(LabelHRDPS)    "HRDPS - 2.5km"
   set Param(FileHRDPS)     /fs/cetus/fs2/ops/cmoe/afsr005/Scenario/data/HRDPS.fstd
   set Param(ToHRDPS)       SC_HRDPS
   set Param(VarHRDPS)      { EI UU }
   set Param(LoopHRDPS)     2
   set Param(DescHRDPS)     "Simulation sur 48 heures du Système à Haute Résolution de Prédiction Déterministe sur le Canada\n48 hours simulation from the  High Resolution Deterministic Prediction System over Canada"
   set Param(ParamHRDPS)    {{ -outline black -rendertexture 1 -colormap CLOUD_MAP  -min 120 -max 250 -mapbelow True -mapabove True -showmap False } { -rendertexture 1 -colormap WIND_MAP  -min 1 -max 60 -mapbelow True -mapabove True -showmap False }}  

   set Param(LabelHRDPS-BC) "HRDPS BC - 1km"
   set Param(FileHRDPS-BC)     /fs/cetus/fs2/ops/cmoe/afsr005/Scenario/data/HRDPS-BC.fstd
   set Param(ToHRDPS-BC)    SC_HRDPS_BC
   set Param(VarHRDPS-BC)   { EI UU }
   set Param(LoopHRDPS-BC)  2
   set Param(DescHRDPS-BC)  "Simulation sur 24 heures du Système à Haute Résolution de Prédiction Déterministe sur la Colombie Britanique\n24 hours simulation from the  High Resolution Deterministic Prediction System over British columbia"
   set Param(ParamHRDPS-BC) {{ -outline black -rendertexture 1 -colormap CLOUD_MAP  -min 120 -max 250 -mapbelow True -mapabove True -showmap False } { -rendertexture 1 -colormap WIND_MAP  -min 1 -max 60 -mapbelow True -mapabove True -showmap False }}

   set Param(LabelCIOPS)    "CIOPS - 2km"
   set Param(FileCIOPS)     /fs/cetus/fs2/ops/cmoe/afsr005/Scenario/data/CIOPS.fstd
   set Param(ToCIOPS)       SC_CIOPS
   set Param(VarCIOPS)      { TMGR }
   set Param(LoopCIOPS)     1
   set Param(DescCIOPS)     "Simulation du gradient de température à 40m du Système de Prédiction Côtière Glace et Ocean sur une année\nSimulation of the temperature gradient at 40m from the Coastal Ice Ocean Prediction System over a year"
   set Param(ParamCIOPS)    {{ -colormap RIVER_MAP -rendertexture True -min 0 -max 0.2 -showmap False }}

   set Param(LabelCAPS)     "CAPS - 3km"
   set Param(FileCAPS)      /fs/cetus/fs2/ops/cmoe/afsr005/Scenario/data/CAPS.fstd
   set Param(ToCAPS)        SC_CAPS
   set Param(VarCAPS)       { GL PN }
   set Param(LoopCAPS)      2
   set Param(DescCAPS)      "Simulation de 48 heures du couvert de glace du Système Canadien de Prévision de l'Arctique\n48 hours ice coverage simulation from the Canadian Arctic Prediction System "
   set Param(ParamCAPS)     {{ -colormap ICE_MAP -rendertexture True -showmap False } { -outline black -width 2 -rendercontour 1 -renderlabel -1 -font LABEL -colormap RIVER_MAP -mapall True -intervalmode INTER 5 -value INTEGER 0 -showmap False }}

   set Param(LabelRMPS-GSL) "RMPS-GSL - 500m" 
   set Param(FileRMPS-GSL)  /fs/cetus/fs2/ops/cmoe/afsr005/Scenario/data/RMPS-GSL.fstd
   set Param(ToRMPS-GSL)    SC_RMPS-GSL
   set Param(VarRMPS-GSL)   { GL } 
   set Param(LoopRMPS-GSL)  1
   set Param(DescRMPS-GSL)  "Simulation du couvert de glace du Système Régional de Prévision Marine du Golfe du Saint-Laurent de Janvier à Mars 2015\nIce coverage simulation fromt the Regional Marine Prediction System for the Gulf of St. Lawrence from January to March 2015"
   set Param(ParamRMPS-GSL) {{ -colormap ICE_MAP -rendertexture True -showmap False }}

   set Param(LabelRIOPS)    "RIOPS ~ 6km"
   set Param(FileRIOPS)     /fs/cetus/fs2/ops/cmoe/afsr005/Scenario/data/RIOPS.fstd
   set Param(ToRIOPS)       SC_RIOPS
   set Param(VarRIOPS)      { TM }
   set Param(LoopRIOPS)     1
   set Param(DescRIOPS)     "Simulation de la température de surface du Système de Prédiction Régional Glace et Ocean sur une année\nSimulation of the surface temperature from the Regional Ice Ocean Prediction System over a year"
   set Param(ParamRIOPS)    {{ -colormap SST_MAP -rendertexture 1 -min -2 -max 30 -interlabels { -2 0 2 4 6 8 10 12 14 16 18 20 22 24 26 28 30 } -font LABEL -value INTEGER 0 -desc "Temp. surface" -unit "°C" -mapbelow True -mapabove True -showmap True }} 
}
