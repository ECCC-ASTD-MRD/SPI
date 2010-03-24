#!/bin/sh
# the next line restarts using tclsh \
exec ${SPI_PATH}/tclsh "$0" "$@"
#============================================================================
# Environnement Canada
# Centre Meteorologique Canadien
# 2100 Trans-Canadienne
# Dorval, Quebec
#
# Projet     : Exemple de scripts.
# Fichier    : OGR_Simplify.tcl
# Creation   : Mai 2008 - J.P. Gauthier - CMC/CMOE
# Description: Simplifier un polygone
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

#----- Open the shapefile to symplify
set layer [ogrfile open SHPFILE read DataIn/ghy_000f06a_e.shp]
eval ogrlayer read SPHLAYER [lindex $layer 0]

#----- Open the destination file
catch { eval file delete -force [glob DataOut/OGR_Simplify.*]  }
ogrfile open NEWSHPFILE write DataOut/OGR_Simplify.shp "ESRI Shapefile"

#----- Simplify the polygons with 0.1 degrees of tolerance (file is in latlon)
ogrlayer stats SPHLAYER -simplify 0.01

#----- Save result
ogrlayer write SPHLAYER NEWSHPFILE

#----- Close the files
ogrfile close NEWSHPFILE
ogrfile close SHPFILE
