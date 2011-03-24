#!/bin/sh
# the next line restarts using tclsh \
exec $SPI_PATH/tclsh "$0" "$@"
#============================================================================
# Environnement Canada
# Centre Meteorologique Canadien
# 2100 Trans-Canadienne
# Dorval, Quebec
#
# Projet     : Exemple de scripts.
# Fichier    : FSTD_RPN2SHP.tcl
# Creation   : Mars 2011 - J.P. Gauthier - CMC/CMOE
# Description: Exporter des champs en shapefile
#
# Parametres :
#
# Retour:
#
# Remarques  :
#
#============================================================================

package require TclData

puts \n[file tail [info script]]

set file [lindex $argv 0]
set out  [lindex $argv 1]
set vars [lrange $argv 2 end]

fstdfile open FILEIN read $file
set path [pwd]

foreach datev [fstdfile info FILEIN DATEV] {

   set time [clock format $datev -format "%Y%m%d_%H%M" -gmt True]
   puts "   Found date $time"
   set n 0
   set fields {}

   foreach var $vars {
      puts "   Checking for variable $var"
      foreach field [lindex [fstdfield find FILEIN [fstdstamp fromseconds $datev] "" -1 -1 -1 "" $var] 0] {
         fstdfield read DATA$n FILEIN $field
         fstdfield configure DATA$n -desc ${var} -rendertexture 1 -min 1e-32
         lappend fields DATA$n
         incr n
      }
   }
   if { [llength $fields] } {
      puts "   Exporting fields $fields"
      cd ${out}
      ogrfile open FILE write ${time}.shp "ESRI Shapefile"

      georef create REF { PROJCS["WGS 84 / UPS North",GEOGCS["WGS 84",DATUM["WGS_1984",SPHEROID["WGS 84",6378137,298.257223563]],PRIMEM["Greenwich",0],UNIT["degree",0.0174532925199433]],PROJECTION["Polar_Stereographic"],PARAMETER["latitude_of_origin",90],PARAMETER["central_meridian",0],PARAMETER["scale_factor",0.994],PARAMETER["false_easting",2000000],PARAMETER["false_northing",2000000],UNIT["metre",1]] }
      ogrlayer create FILE LAYER ${time} REF
      ogrlayer import LAYER $fields

      ogrfile close FILE
      ogrlayer free LAYER
      eval fstdfield free $fields

      eval exec zip ${time}.zip ${time}.shp ${time}.shx ${time}.dbf ${time}.prj
      file delete ${time}.shp ${time}.shx ${time}.dbf ${time}.prj
      cd $path
   }
}
