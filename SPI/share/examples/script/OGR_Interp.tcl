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
# Fichier    : OGR_Interp.tcl
# Creation   : Fevrier 2008 - J.P. Gauthier - CMC/CMOE
# Description: Faire la somme des valeurs d'un champs par feature de shapefile
#
# Parametres :
#
# Retour:
#
# Remarques  :
#
#============================================================================

package require TclData
#package require TclGeoEER
package require Logger

Log::Start [info script] 0.1

#----- Open the shapefile
catch { eval file delete [glob $env(CI_DATA_OUT)/OGR_Interp2D.*] }

set layer [ogrfile open SHPFILE read $env(CI_DATA_IN)/land_bg_p.shp]
eval ogrlayer read LAYER [lindex $layer 0]

#----- Read the data to be summed
fstdfile open DATAFILE read $env(CI_DATA_IN)/2005102612_012
fstdfield read DATAFIELD DATAFILE -1 "" -1 -1 -1 "" "P0"

#----- Clear the file that will be used to sum the data
ogrlayer define LAYER -field PRES Real
ogrlayer clear LAYER PRES 0.0

#----- Open a file to save the index for future reuse for faster processing
if { [file exists $env(CI_DATA_OUT)/OGR_InterpIdx.bin] } {
   set f [open  $env(CI_DATA_OUT)/OGR_InterpIdx.bin { RDONLY BINARY }]
   set index [read $f]
   set gotindex True
} else {
   set f [open $env(CI_DATA_OUT)/OGR_InterpIdx.bin { WRONLY CREAT BINARY }]
   set gotindex False
}

#----- Do the sum in conservative mode splitting the grid cell in 1 segment
puts "   Interpolating field values into polygon layer"
#ogrlayer interp LAYER DATAFIELD ZONE CONSERVATIVE 1 True $f
ogrlayer interp LAYER DATAFIELD PRES AVERAGE 1 True index

puts "   Minimum: [ogrlayer stats LAYER -min PRES]"
puts "   Maximum: [ogrlayer stats LAYER -max PRES]"

puts "   Applying calculus log(LAYER.PRES+100)"
vexpr LAYER.HECTARES log(LAYER.PRES+100)

#puts "   Minimum: [ogrlayer stats LAYER -min ZONE]"
#puts "   Maximum: [ogrlayer stats LAYER -max ZONE]"

#binary scan $index f* data
#puts [llength $data]
#puts $data
if { !$gotindex } {
   puts $f $index
   close $f
}

ogrfile open SHPFILE2 write $env(CI_DATA_OUT)/OGR_Interp2D.shp "ESRI Shapefile"
ogrlayer write LAYER SHPFILE2

#ogrlayer sync LAYER
ogrfile close SHPFILE SHPFILE2


ogrfile open CSVFILE write $env(CI_DATA_OUT)/OGR_Interp2D.csv "CSV"
ogrlayer write LAYER CSVFILE
ogrfile close CSVFILE

#----- Test Point interpolation


#----- Open the shapefile
catch { eval file delete [glob $env(CI_DATA_OUT)/OGR_Interp1D.*] }

set layer [ogrfile open SHPFILE read $env(CI_DATA_IN)/TideStations.shp]
eval ogrlayer read LAYER [lindex $layer 0]

#----- Clear the file that will be used to sum the data
ogrlayer define LAYER -field PRES Real
ogrlayer clear LAYER PRES 0.0

#----- Do the linear interpolation
puts "   Interpolating field values into point layer"
ogrlayer interp LAYER DATAFIELD PRES LINEAR

ogrfile open SHPFILE2 write $env(CI_DATA_OUT)/OGR_Interp1D.shp "ESRI Shapefile"
ogrlayer write LAYER SHPFILE2
#ogrlayer sync LAYER
ogrfile close SHPFILE SHPFILE2

Log::End