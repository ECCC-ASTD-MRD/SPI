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
# Fichier    : FSTD_GetPressure.tcl
# Creation   : Mars 2010 - J.P. Gauthier - CMC/CMOE
# Description: Recupere la pression aux point et niveaux d'unch champs
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

puts "   Processing pressure for hybrid levels"

#----- Open in and out files
fstdfile open FILEIN read DataIn/2006122900_000.hyb
fstdfile open FILEOUT write DataOut/2006122900_000.hyb.pres

#----- Read a cube of data
fstdfield read TTHYB FILEIN -1 "" -1 -1 -1 "" "TT"
fstdfield readcube TTHYB

#----- Read pressure at surface
fstdfield read P0 FILEIN -1 "" -1 -1 -1 "" "P0"

#----- Calculate pressure at each gridpoint of TTHYB
fstdgrid pressure TTHYB P0

#----- Write result
fstdfield read TIC FILEIN -1 "" -1 -1 -1 "" >>
fstdfield read TAC FILEIN -1 "" -1 -1 -1 "" ^^
fstdfield write TIC FILEOUT 0 True
fstdfield write TAC FILEOUT 0 True

fstdfield define TTHYB -NOMVAR PRES
fstdfield write TTHYB FILEOUT 0 True

fstdfile close FILEIN FILEOUT

puts "   Processing pressure for eta levels"

#----- Open in and out files
fstdfile open FILEIN read DataIn/2006122900_000.eta
fstdfile open FILEOUT write DataOut/2006122900_000.eta.pres

#----- Read a cube of data
fstdfield read TTETA FILEIN -1 "" -1 -1 -1 "" "TT"
fstdfield readcube TTETA

#----- Read pressure at surface
fstdfield read P0 FILEIN -1 "" -1 -1 -1 "" "P0"

#----- Force top pressure to 10mb since PT is not available in the file
fstdfield stats TTETA -top 10.0

#----- Calculate pressure at each gridpoint of TTHYB
fstdgrid pressure TTETA P0

#----- Write result
fstdfield read TIC FILEIN -1 "" -1 -1 -1 "" >>
fstdfield read TAC FILEIN -1 "" -1 -1 -1 "" ^^
fstdfield write TIC FILEOUT 0 True
fstdfield write TAC FILEOUT 0 True

fstdfield define TTETA -NOMVAR PRES
fstdfield write TTETA FILEOUT 0 True

fstdfield free TTHYB P0 TIC TAC
fstdfile close FILEIN FILEOUT
