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
# Fichier    : GRIB_Funcs.tcl
# Creation   : Janvier 2010 - J.P. Gauthier - CMC/CMOE
# Description: Test de lecture de fichier Collada DAE
#
# Parametres :
#
# Retour:
#
# Remarques  :
#   Aucune.
#
#============================================================================

package require TclData

puts \n[file tail [info script]]

#set fields [gribfile open FILE read DataIn/CMC_reg_4LFTX_SFC_0_ps15km_2010122000_P000.grib2]
#set fields [gribfile open FILE read DataIn/CMC_glb_ALBDO_SFC_0_latlon.6x.6_2011012000_P018.grib2]
set fields [gribfile open FILE read DataIn/CMC_reg_ABSV_ISBL_250_ps15km_2011012100_P000.grib]

puts "Found [llength $fields] field(s) $fields"

foreach field $fields {
   gribfield read FIELD FILE [lindex $field 1]

   puts "Center       : [gribfield define FIELD -CENTER]"
   puts "Var          : [gribfield define FIELD -NOMVAR]"
   puts "Desc         : [gribfield configure FIELD -desc]"
   puts "Dim          : [gribfield define FIELD -NI]x[gribfield define FIELD -NJ]x[gribfield define FIELD -NK]"
   puts "No data value: [gribfield stats FIELD -nodata]"
   puts "Min          : [gribfield stats FIELD -min]"
   puts "Max          : [gribfield stats FIELD -max]"

   puts "WKT String   : [gribfield define FIELD -projection]"
#   puts "Grid         : [gribfield stats FIELD -grid]"

   gribfield free FIELD
}
