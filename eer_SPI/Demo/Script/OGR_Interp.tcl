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

puts \n[file tail [info script]]

#----- Open the shapefile

catch { eval file delete [glob DataOut/OGR_Interp2D.*] }

file copy DataIn/land_bg_p.shp DataOut/OGR_Interp2D.shp
file copy DataIn/land_bg_p.dbf DataOut/OGR_Interp2D.dbf
file copy DataIn/land_bg_p.shx DataOut/OGR_Interp2D.shx

set layer [ogrfile open SHPFILE append DataOut/OGR_Interp2D.shp]
eval ogrlayer read LAYER [lindex $layer 0]

#----- Read the data to be summed

fstdfile open DATAFILE read DataIn/2005102612_012
fstdfield read DATAFIELD DATAFILE -1 "" -1 -1 -1 "" "P0"

#----- Clear the file that will be used to sum the data

ogrlayer define LAYER -field PRES Real
ogrlayer clear LAYER PRES 0.0

#----- Open a file to save the index for future reuse for faster processing
#      if the file is empty, it will be filled with the index
#      otherwise, it will be used as an index
set f [open DataOut/OGR_InterpIdx.txt {RDWR CREAT}]
#set f [open DataOut/OGR_InterpIdx.txt r]
#
#----- Do the sum in conservative mode splitting the grid cell in 1 segment
puts "   Interpolating field values into polygon layer"
#ogrlayer interp LAYER DATAFIELD ZONE CONSERVATIVE 1 True $f
ogrlayer interp LAYER DATAFIELD PRES AVERAGE 1 True $f

puts "   Minimum: [ogrlayer stats LAYER -min PRES]"
puts "   Maximum: [ogrlayer stats LAYER -max PRES]"

#puts "   Applying calculus log(LAYER.PRES+100)"
#vexpr LAYER.ZONE log(LAYER.PRES+100)

puts "   Minimum: [ogrlayer stats LAYER -min PRES]"
puts "   Maximum: [ogrlayer stats LAYER -max PRES]"

close $f

ogrlayer sync LAYER
ogrfile close SHPFILE


#----- Test Point interpolation


#----- Open the shapefile

catch { eval file delete [glob DataOut/OGR_Interp1D.*] }

file copy DataIn/TideStations.shp DataOut/OGR_Interp1D.shp
file copy DataIn/TideStations.dbf DataOut/OGR_Interp1D.dbf
file copy DataIn/TideStations.shx DataOut/OGR_Interp1D.shx

set layer [ogrfile open SHPFILE append DataOut/OGR_Interp1D.shp]
eval ogrlayer read LAYER [lindex $layer 0]

#----- Clear the file that will be used to sum the data

ogrlayer define LAYER -field PRES Real
ogrlayer clear LAYER PRES 0.0

#----- Do the linear interpolation
puts "   Interpolating field values into point layer"
ogrlayer interp LAYER DATAFIELD PRES LINEAR

ogrlayer sync LAYER
ogrfile close SHPFILE
