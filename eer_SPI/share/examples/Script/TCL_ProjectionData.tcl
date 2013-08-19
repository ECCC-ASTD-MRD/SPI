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
#package require TclGeoEER

puts \n[file tail [info script]]

set lat     39
set lon     -150
set dist    100000
set bearing 45
set box     {46.22564965180643 -127.4986157911365 59.11529498534855 -109.47756534627678}

projection create PROJ
projection configure PROJ -mapres 32

puts "   TOPO = [projection data PROJ -TOPO $lat $lon]"
puts "   BATH = [projection data PROJ -BATH $lat $lon]"
puts "   TYPE = [projection data PROJ -TYPE $lat $lon]"
puts "   MASK = [projection data PROJ -MASK $lat $lon]"

puts "   Moving $lat $lon $dist m at $bearing degrees: [projection function PROJ -circle $lat $lon $dist $bearing]"
puts "   Moving box $dist t m at $bearing degrees: [projection function PROJ -circle $lat $lon $dist $bearing $box]"