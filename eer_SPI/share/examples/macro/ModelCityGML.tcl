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

   model read GML "/cnfs/ops/production/cmoe/geo/Vector/Cities/Montreal/CityGML_LOD2_20140115/VM09_2009.gml"
# { PROJCS["NAD_1983_CSRS_MTM_8",GEOGCS["GCS_North_American_1983_CSRS",DATUM["D_North_American_1983_CSRS",SPHEROID["GRS_1980",6378137.0,298.257222101]],PRIMEM["Greenwich",0.0],UNIT["Degree",0.0174532925199433]],PROJECTION["Transverse_Mercator"],PARAMETER["False_Easting",304800.0],PARAMETER["False_Northing",0.0],PARAMETER["Central_Meridian",-73.5],PARAMETER["Scale_Factor",0.9999],PARAMETER["Latitude_Of_Origin",0.0],UNIT["Meter",1.0]],VERTCS["CGVD_1928",VDATUM["Canadian_Geodetic_Vertical_Datum_of_1928"],PARAMETER["Vertical_Shift",0.0],PARAMETER["Direction",1.0],UNIT["Meter",1.0]] }

#   model read GML /users/dor/afsr/005/Links/devfs/Data/CityGML/CityGML_British_Ordnance_Survey_v1.0.0.xml
   georef create REF { PROJCS["NAD_1983_MTM_8",GEOGCS["GCS_North_American_1983",DATUM["D_North_American_1983",SPHEROID["GRS_1980",6378137.0,298.257222101]],PRIMEM["Greenwich",0.0],UNIT["Degree",0.0174532925199433]],PROJECTION["Transverse_Mercator"],PARAMETER["False_Easting",304800.0],PARAMETER["False_Northing",0.0],PARAMETER["Central_Meridian",-73.5],PARAMETER["Scale_Factor",0.9999],PARAMETER["Latitude_Of_Origin",0.0],UNIT["Meter",1.0]]}
#   model read GML "/local/disk2/afsr005/PortMtl-quaiAlexandra/Port.gml"
   model define GML -georef REF

#   model read GML /users/dor/afsr/005/Links/devfs/Data/CityGML/Berlin_Alexanderplatz_v1.0.0.xml
#   model read GML /users/dor/afsr/005/Links/devfs/Data/CityGML/GTA-Munich-1_0_0/Munich_v_1_0_0.xml
#   model read GML /users/dor/afsr/005/Links/devfs/Data/CityGML/Frankfurt_Street_Setting_LOD3/Frankfurt_Street_Setting_LOD3.gml
#   model read GML /users/dor/afsr/005/Links/devfs/Data/CityGML/Berlin_Pariser_Platz_v1.0.0/Berlin_Pariser_Platz_v1.0.0.xml
#   model read GML /users/dor/afsr/005/Links/devfs/Data/CityGML/Kreuz_Leverkusen_2008-03-05/080305SIG3D_Building_Levkreuz.xml
#   model read GML /users/dor/afsr/005/Links/devfs/Data/CityGML/Stadt-Ettenheim-LoD3_edited_v1.0.0.gml
   Mapper::UpdateData $Page::Data(Frame) GML
}

proc Macro::ModelCityGML::Clean { } {

}

