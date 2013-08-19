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
# Fichier    : FSTD_HYBRID2PRESSURE.tcl
# Creation   : Mai 2000 - J.P. Gauthier - CMC/CMOE
# Description: Interpoler des champs dans la verticale de hybride a pression
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

set file [lindex $argv 0]

file delete DataOut/FSTD_HYBRID2PRESSURE.fstd

#----- Open input and output files
set fields [fstdfile open IN read $file]
fstdfile open OUT write DataOut/FSTD_HYBRID2PRESSURE.fstd

set levels { 1000 850 500 250 }

#----- Copy grid descriptors
#fstdfield read TIC IN -1 "" -1 -1 -1 "" >>
#fstdfield read TAC IN -1 "" -1 -1 -1 "" ^^
#fstdfield write TIC OUT 0 True
#fstdfield write TAC OUT 0 True


fstdfield read P0 IN -1 "" -1 -1 -1 "" "P0"

fstdfield read TT IN -1 "" -1 -1 -1 "" "TT"
fstdfield readcube TT

# Set the pressure at the top of the ETA levels, since PT is not in the files

#fstdfield stats TT -top 1.0

fstdfield create TTPRES [fstdfield define TT -NI] [fstdfield define TT -NJ] [llength $levels]
fstdfield stats TTPRES -leveltype PRESSURE -levels $levels

fstdfield configure TTPRES -interpdegree LINEAR
fstdfield verticalinterp TTPRES TT - P0

fstdfield write TTPRES OUT 0 TRUE

