#===============================================================================
# Environnement Canada
# Centre Meteorologique Canadian
# 2100 Trans-Canadienne
# Dorval, Quebec
#
# Projet   : Exemples de scripts.
# Fichier  : ModelQuebec2.tcl
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

namespace eval Macro::ModelQuebec2 {} {
   variable Param
   variable Data
   variable Error

   set Param(In)   ""
   set Param(Info) { "Inclure une scène 3D de la ville de Vancouver."
                     "Include a 3D scene of Vancouver city." }
}

proc Macro::ModelQuebec2::Execute { } {
   global GDefs
   variable Data
   variable Error

   Macro::Doing "Reading model data"
   georef create QCREF
   georef define QCREF -projection {PROJCS["NAD83 UTM, Zone 18 North, Meter",GEOGCS["North_American_Datum_1983",DATUM["D_North_American_1983",SPHEROID["Geodetic_Reference_System_of_1980",6378137,298.2572221009113]],PRIMEM["Greenwich",0],UNIT["Degree",0.017453292519943295]],PROJECTION["Transverse_Mercator"],PARAMETER["latitude_of_origin",47.0],PARAMETER["central_meridian",-64.66],PARAMETER["scale_factor",0.9996],PARAMETER["false_easting",500000],PARAMETER["false_northing",0],UNIT["Meter",1]]}

   model read QCC3D /local/disk1/afsr005/3DModel/Quebec/Quebec_RDDC_flt/quebec.flt
   model define QCC3D -georef QCREF
   Mapper::UpdateData $Page::Data(Frame) QCC3D
}

proc Macro::ModelQuebec2::Clean { } {

}

