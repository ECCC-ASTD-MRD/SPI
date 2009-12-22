#===============================================================================
# Environnement Canada
# Centre Meteorologique Canadian
# 2100 Trans-Canadienne
# Dorval, Quebec
#
# Projet   : Exemples de script.
# Fichier  : ModelQuebec.tcl
# Creation : Fevrier 2001 - J.P.Gauthier - CMC/CMOE
#
# Description:
#    Inclure une scène 3D de la villle de Québec.
#
# Arguments  :
#
# Remarques :
#
#===============================================================================

namespace eval Macro::ModelQuebec {} {
   variable Param
   variable Data
   variable Error

   set Param(In)   ""
   set Param(Info) { "Inclure une scène 3D de la ville de Québec."
                     "Include a 3D scene of Quebec city." }
}

proc Macro::ModelQuebec::Execute { } {
   global GDefs
   variable Data
   variable Error

   Macro::Doing "Reading model data"
   model read Ground    /data/cmoe/afsr005/Data/3DModel/Quebec/terrain.mdl
   model read Building1 /data/cmoe/afsr005/Data/3DModel/Quebec/public.mdl
   model read Building2 /data/cmoe/afsr005/Data/3DModel/Quebec/maison.mdl
   model read Building3 /data/cmoe/afsr005/Data/3DModel/Quebec/mur.mdl

   model material Ground -transparency  0.9
   model material Ground -emissive #0F1E00

   georef create 3DREF
   georef define 3DREF -projection {PROJCS["NAD83 UTM, Zone 18 North, Meter",GEOGCS["North_American_Datum_1983",DATUM["D_North_American_1983",SPHEROID["Geodetic_Reference_System_of_1980",6378137,298.2572221009113]],PRIMEM["Greenwich",0],UNIT["Degree",0.017453292519943295]],PROJECTION["Transverse_Mercator"],PARAMETER["latitude_of_origin",0.0],PARAMETER["central_meridian",-69.0],PARAMETER["scale_factor",0.9996],PARAMETER["false_easting",500000],PARAMETER["false_northing",0],UNIT["Meter",1]]}

   model define Building1 -georef 3DREF
   model define Building2 -georef 3DREF
   model define Building3 -georef 3DREF
   model define Ground    -georef 3DREF

   model read Helico /data/cmoe/afsr005/Data/3DModel/Quebec/hughe500.mdl
   model read F18    /data/cmoe/afsr005/Data/3DModel/Quebec/F18.mdl
   model read 747    /data/cmoe/afsr005/Data/3DModel/Quebec/747.mdl

   model matrix Helico -locate 46.8086773762 -71.2179046536 125
   model matrix Helico -rotate -90.0 0.0 80.0
   model matrix Helico -scale 0.3 0.3 0.3

   model matrix F18 -locate 46.8032 -71.2122 150
   model matrix F18 -rotate 0.0 0.0 0.0
   model matrix F18 -scale 3.0 3.0 3.0

   model matrix 747 -locate 46.8093 -71.2055 200
   model matrix 747 -rotate 90.0 0.0 90.0
   model matrix 747 -scale 0.3 0.3 0.3

   model material Helico -emissive #7F7FFF
   model material Helico -diffuse #7F7FFF
   model material Helico -specular #FFFFFF
   model material Helico -shininess 0.9
   model material Helico -transparency  1.0

   model read 911 /data/cmoe/afsr005/Data/3DModel/Quebec/747.mdl
   model matrix 911 -locate 46.80794262 -71.217946 175
   model matrix 911 -rotate 70.0 0.0 0.0
   model matrix 911 -scale 0.3 0.3 0.3

#   model read TST3DS /data/cmoe/afsr005/Data/3DModel/breguet/breguet.3DS
#   model read TST3DS /data/cmoe/afsr005/Data/3DModel/Semi/a3dsemi.3ds
#   model matrix TST3DS -scale 10000 10000 10000

   Mapper::UpdateData $Page::Data(Frame) Ground Building1 Building2 Building3 Helico F18 747 911
}

proc Macro::ModelQuebec::Clean { } {

}
