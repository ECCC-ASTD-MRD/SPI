#===============================================================================
# Environnement Canada
# Centre Meteorologique Canadian
# 2100 Trans-Canadienne
# Dorval, Quebec
#
# Projet   : Exemples de scripts.
# Fichier  : ModelCalgary.tcl
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

namespace eval Macro::ModelCalgary {} {
   variable Param
   variable Data
   variable Error

   set Param(In)   ""
   set Param(Info) { "Inclure une scène 3D de la ville de Vancouver."
                     "Include a 3D scene of Vancouver city." }
}

proc Macro::ModelCalgary::Execute { } {
   global GDefs
   variable Data
   variable Error

   Macro::Doing "Reading model data"
   georef create VANREF
   georef define VANREF -projection {PROJCS["NAD83 UTM, Zone 10 North, Meter",GEOGCS["WGS 84",DATUM["WGS_1984",SPHEROID["WGS 84",6378137,298.257223563]],PRIMEM["Greenwich",0],UNIT["Degree",0.017453292519943295]],PROJECTION["Transverse_Mercator"],PARAMETER["latitude_of_origin",0.0],PARAMETER["central_meridian",-123.0],PARAMETER["scale_factor",0.9996],PARAMETER["false_easting",8193],PARAMETER["false_northing",-5459442.0],UNIT["Meter",1]]}

#   model read CALG3D "/users/dor/afsr/005/Links/devfs/Obj_city_export/separated objs/calgary_tower.obj"
   model read CALG3D "/users/dor/afsr/005/Links/devfs/Obj_city_export/combined obj/city non textured combined.obj"
#   model read CALG3D /users/dor/afsr/005/Links/devfs/calgary_city_non_textured.flt
#   model define CALG3D -georef VANREF
   model matrix CALG3D -locate 51.05242 -114.0718 0.0
   model matrix CALG3D -rotate 90.0 0.0 0.0
   model matrix CALG3D -scale 1.0 1.0 1.0
   
   Mapper::UpdateData $Page::Data(Frame) CALG3D
}

proc Macro::ModelCalgary::Clean { } {

}

