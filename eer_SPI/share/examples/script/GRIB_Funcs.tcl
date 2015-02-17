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
# Description: Test des fonctions GRIB
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
#package require TclGeoEER
package require Logger

Log::Start [info script] 0.1

#set fields [gribfile open GRIBIN read DataIn/CMC_reg_4LFTX_SFC_0_ps15km_2010122000_P000.grib2]
#set fields [gribfile open GRIBIN read DataIn/CMC_glb_ALBDO_SFC_0_latlon.6x.6_2011012000_P018.grib2]
set fields [gribfile open GRIBIN read DataIn/CMC_reg_ABSV_ISBL_250_ps15km_2011012100_P000.grib]

Log::Print INFO "Found [llength $fields] field(s) $fields"

foreach field $fields {
   gribfield read GRIB GRIBIN [lindex $field 1]

   puts "   Center       : [gribfield define GRIB -key (String)centre]"
   puts "   Var          : [gribfield define GRIB -NOMVAR]"
   puts "   Desc         : [gribfield configure GRIB -desc]"
   puts "   Dim          : [gribfield define GRIB -NI]x[gribfield define GRIB -NJ]x[gribfield define GRIB -NK]"
   puts "   No data value: [gribfield stats GRIB -nodata]"
   puts "   Min          : [gribfield stats GRIB -min]"
   puts "   Max          : [gribfield stats GRIB -max]"

   puts "   WKT String   : [gribfield define GRIB -projection]"
   puts "   Keys         : [gribfield define GRIB -key]"
   puts "   Key centre                    : [gribfield define GRIB -key (String)centre]"
   puts "   Key shortName                 : [gribfield define GRIB -key (String)name]"
   puts "   Key longitudeOfFirstGridPoint : [gribfield define GRIB -key longitudeOfFirstGridPointInDegrees]"
   
#   puts "Grid         : [gribfield stats GRIB -grid]"

#   gribfield free GRIB
}

Log::Print INFO "Testing GRIB creation"

fstdfile open FSTDIN read DataIn/2005120600_012
fstdfield read FLD FSTDIN -1 "" -1 -1 -1 "" "O3"

#----- Use a read grib field
gribfield gridinterp GRIB FLD
gribfield define GRIB -key (String)centre cwao
gribfield define GRIB -key (String)shortName "tco3"

#----- Use a template file
gribfield create GRIBNEW 360 181 1 GRIB2
   puts "   Keys         : [gribfield define GRIBNEW -key]"
gribfield stats GRIBNEW -nodata 0.0
gribfield gridinterp GRIBNEW FLD

#----- One key at a time
gribfield define GRIBNEW -key discipline 0
gribfield define GRIBNEW -key parameterCategory 14
gribfield define GRIBNEW -key parameterNumber 0

#----- Or as a list of keys
gribfield define GRIBNEW -key { discipline 0 parameterCategory 14 parameterNumber 0 }

gribfile open GRIBOUT write DataOut/GRIB_Funcs.grib
gribfield write GRIB GRIBOUT 16
gribfield write GRIBNEW GRIBOUT 16

fstdfile close FSTDIN
gribfile close GRIBIN GRIBOUT
Log::End