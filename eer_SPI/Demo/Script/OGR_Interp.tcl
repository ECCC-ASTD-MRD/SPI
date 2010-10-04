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

catch { eval file delete [glob DataOut/OGR_Interp.*] }

file copy DataIn/Volcano.shp DataOut/OGR_Interp.shp
file copy DataIn/Volcano.dbf DataOut/OGR_Interp.dbf
file copy DataIn/Volcano.shx DataOut/OGR_Interp.shx

set layer [ogrfile open SHPFILE append DataOut/OGR_Interp.shp]
eval ogrlayer read LAYER [lindex $layer 0]

#----- Read the data to be summed

fstdfile open DATAFILE read DataIn/2005102612_012c
fstdfield read DATAFIELD DATAFILE -1 "" -1 12 -1 "" "AV"

#----- Clear the file that will be used to sum the data

ogrlayer define LAYER -field ZONE Real
ogrlayer clear LAYER ZONE

#----- Open a file to save the index for future reuse for faster processing
#      if the file is empty, it will be filled with the index
#      otherwise, it will be used as an index
#set f [open DataOut/OGR_InterpIdx.txt {RDWR CREAT}]
set f [open DataOut/OGR_InterpIdx.txt r]
#
#----- Do the sum in conservative mode splitting the grid cell in 1 segment
puts "   Interpolating field values into layer"
#ogrlayer interp LAYER DATAFIELD ZONE CONSERVATIVE 1 True $f
ogrlayer interp LAYER DATAFIELD ZONE AVERAGE 1 True $f
ogrlayer interp LAYER DATAFIELD ZONE AVERAGE 1 True $f

puts "   Minimum: [ogrlayer stats LAYER -min ZONE]"
puts "   Maximum: [ogrlayer stats LAYER -max ZONE]"

puts "   Applying calculus log(LAYER.ZONE+100)"
vexpr LAYER.ZONE log(LAYER.ZONE+100)

puts "   Minimum: [ogrlayer stats LAYER -min ZONE]"
puts "   Maximum: [ogrlayer stats LAYER -max ZONE]"

close $f

ogrlayer sync LAYER
ogrfile close SHPFILE