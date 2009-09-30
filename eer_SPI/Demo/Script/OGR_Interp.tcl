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

set layer [ogrfile open OLDFILE read DataIn/timezone.shp]
eval ogrlayer read OLD [lindex $layer 0]

#----- Read the data to be summed

fstdfile open DATAFILE read DataIn/2005102612_012c
fstdfield read DATAFIELD DATAFILE -1 "" -1 12 -1 "" "AV"

#----- Clear the file that will be used to sum the data

ogrlayer clear OLD ZONE 0.0

#----- Do the sum in conservative mode splitting the grid cell in 1 segment
#      the list variable will be filled with the used indices

ogrlayer interp OLD DATAFIELD ZONE CONSERVATIVE 1 True list

puts "Minimum: [ogrlayer stats OLD -min ZONE]"
puts "Maximum: [ogrlayer stats OLD -max ZONE]"

#----- Save the index since it can be reused for faster processing

set f [open DataOut/OGR_Interp2.txt w]
puts $f $list
close $f

#----- Save the result in another file

catch { file delete  DataOut/OGR_Interp2.shp }
ogrfile open NEWFILE write DataOut/OGR_Interp2.shp "ESRI Shapefile"
ogrlayer write OLD NEWFILE
ogrfile close NEWFILE