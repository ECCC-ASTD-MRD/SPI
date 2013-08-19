#===============================================================================
# Environnement Canada
# Centre Meteorologique Canadian
# 2100 Trans-Canadienne
# Dorval, Quebec
#
# Projet   : Exemples de scripts.
# Fichier  : ModelCityGML.tcl
# Creation : Fevrier 2001 - J.P.Gauthier - CMC/CMOE
#
# Description:
#    Inclure une scène 3D de la ville de Vancouver.
#
# Arguments  :
#
# Remarques :
#
#===============================================================================

namespace eval Macro::ModelCityGML {} {
   variable Param
   variable Data
   variable Error

   set Param(In)   ""
   set Param(Info) { "Inclure une scène 3D CityGML."
                     "Include a CityGML 3D scene." }
}

proc Macro::ModelCityGML::Execute { } {
   global GDefs
   variable Data
   variable Error

   Macro::Doing "Reading model data"

   model read GML /local/disk1/afsr005/3DModel/CityGML/CityGML_British_Ordnance_Survey_v1.0.0.xml
#   model read GML /local/disk1/afsr005/3DModel/CityGML/Berlin_Alexanderplatz_v1.0.0.xml
#   model read GML /local/disk1/afsr005/3DModel/CityGML/GTA-Munich-1_0_0/Munich_v_1_0_0.xml
#   model read GML /local/disk1/afsr005/3DModel/CityGML/Frankfurt_Street_Setting_LOD3/Frankfurt_Street_Setting_LOD3.gml
#   model read GML /local/disk1/afsr005/3DModel/CityGML/Berlin_Pariser_Platz_v1.0.0/Berlin_Pariser_Platz_v1.0.0.xml
#   model read GML /local/disk1/afsr005/3DModel/CityGML/Kreuz_Leverkusen_2008-03-05/080305SIG3D_Building_Levkreuz.xml
#   model read GML /local/disk1/afsr005/3DModel/CityGML/Stadt-Ettenheim-LoD3_edited_v1.0.0.gml
   Mapper::UpdateData $Page::Data(Frame) GML
}

proc Macro::ModelCityGML::Clean { } {

}

