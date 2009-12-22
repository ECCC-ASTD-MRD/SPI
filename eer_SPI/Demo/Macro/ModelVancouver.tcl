#===============================================================================
# Environnement Canada
# Centre Meteorologique Canadian
# 2100 Trans-Canadienne
# Dorval, Quebec
#
# Projet   : Exemples de scripts.
# Fichier  : ModelVancouver.tcl
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

namespace eval Macro::ModelVancouver {} {
   variable Param
   variable Data
   variable Error

   set Param(In)   ""
   set Param(Info) { "Inclure une scène 3D de la ville de Vancouver."
                     "Include a 3D scene of Vancouver city." }
}

proc Macro::ModelVancouver::Execute { } {
   global GDefs
   variable Data
   variable Error

   Macro::Doing "Reading model data"
   georef create VANREF
   georef define VANREF -projection {PROJCS["NAD83 UTM, Zone 10 North, Meter",GEOGCS["WGS 84",DATUM["WGS_1984",SPHEROID["WGS 84",6378137,298.257223563]],PRIMEM["Greenwich",0],UNIT["Degree",0.017453292519943295]],PROJECTION["Transverse_Mercator"],PARAMETER["latitude_of_origin",0.0],PARAMETER["central_meridian",-123.0],PARAMETER["scale_factor",0.9996],PARAMETER["false_easting",8193],PARAMETER["false_northing",-5459442.0],UNIT["Meter",1]]}

   model read VANC3D /data/cmoe/afsr005/Data/3DModel/Vancouver/CRTI_Vanc_Mod_Final5_Apr16_BLDG_ONLY.flt
   model define VANC3D -georef VANREF
   Mapper::UpdateData $Page::Data(Frame) VANC3D

   Macro::Doing "Reading 10cm imagery"
   Mapper::Read /data/cmoe/afsr005/Data/3DModel/Vancouver/Images_10cm/Vancouver_10cm.tif

}

proc Macro::ModelVancouver::Clean { } {

}

