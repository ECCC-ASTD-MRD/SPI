#===============================================================================
# Environnement Canada
# Centre Meteorologique Canadian
# 2100 Trans-Canadienne
# Dorval, Quebec
#
# Projet   : Exemples de Macro
# Fichier  : GDAL.tcl
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

namespace eval Macro::GDAL {} {
   variable Param
   variable Data
   variable Error

   set Param(In)   ""
   set Param(Info) { ""
                     "." }
}

proc Macro::GDAL::Execute { } {
   global GDefs
   variable Data
   variable Error
   
#   set bands [gdalfile open FILE read DataIn/srtm_n045w074_badmedian3x3]
   set bands [gdalfile open FILE read /cnfs/dev/cmoe/afsralx/National-SAR-Winds/sample/RS2_OK35806_PK347563_DK308097_SCWA_20121210_145447_VV_VH_SGF/RS2_OK35806_PK347563_DK308097_SCWA_20121210_145447_VV_VH_SGF_r.tif]
   gdalband read BANDR [list [lindex $bands 0]]
   gdalband read BANDG [list [lindex $bands 1]]
   gdalband read BANDB [list [lindex $bands 2]]
   
   vexpr (UByte)DATA  BANDR+BANDG+BANDB
   vexpr (UByte)BANDR ifelse(!DATA,255,BANDR);
   vexpr (UByte)BANDG ifelse(!DATA,255,BANDG);
   vexpr (UByte)BANDB ifelse(!DATA,255,BANDB);
   vexpr (UByte)BANDA ifelse(!DATA,0,255);
   
   gdalband create BAND
   gdalband combine BAND BANDR BANDG BANDB
 
   file delete -force toto.tif
   gdalfile open FILEO write toto.tif "GeoTIFF"
   gdalband write BAND FILEO
#   gdalfile close FILEO
   
#   set bands [gdalfile open FILE2 read toto.tif]
#   gdalband read BAND $bands
#   gdalband stats BAND -nodata 255
 puts stderr [gdalband stats BAND -stretch 0 10 90]
 puts stderr [gdalband stats BAND -stretch 0 30 70]
 puts stderr [gdalband stats BAND -stretch 0 35 60]
#   set cmap [gdalband configure BAND -colormap]
#   colormap configure $cmap -min alpha 2000
   Viewport::Assign $Page::Data(Frame) $Viewport::Data(VP) BAND
}

proc Macro::GDAL::Clean { } {

}

