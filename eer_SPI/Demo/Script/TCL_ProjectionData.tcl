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
# Fichier    : TCL_ProjectionData
# Creation   : Mars 2005 - J.P. Gauthier - CMC/CMOE
# Description: Recupere l'information geographique de la projection (TOPO,MASK,...)
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

set lat 39
set lon -150

projection create PROJ
projection configure PROJ -mapres 32

puts "   TOPO = [projection data PROJ -TOPO $lat $lon]"
puts "   BATH = [projection data PROJ -BATH $lat $lon]"
puts "   TYPE = [projection data PROJ -TYPE $lat $lon]"
puts "   MASK = [projection data PROJ -MASK $lat $lon]"
