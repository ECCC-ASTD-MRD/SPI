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
# Fichier    : FSTD_DWIntegrate.tcl
# Creation   : Fevrier 2011 - J.P. Gauthier - CMC/CMOE
# Description:
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

#----- Ouvrir les fichiers d'entree / sortie

set file [lindex $argv 0]   ;#File to process
set dt   [lindex $argv 1]   ;#Time step in seconds

set fields [fstdfile open IN read $file]
fstdfile open OUT write $file.out

#----- Recuperer les TICTAC

fstdfield read TIC IN -1 "" -1 -1 -1 "" >>
fstdfield read TAC IN -1 "" -1 -1 -1 "" ^^
fstdfield write TIC OUT 0 True
fstdfield write TAC OUT 0 True

#----- Get all of the DWs

set dws [fstdfield find IN -1 "" -1 -1 -1 "" DW]

#----- Get timestep in hours

set dt  [expr $dt/3600.0*0.5]

set n   -1
fstdfield read PDW IN [lindex $dws 0]
vexpr DWI $dt*PDW

foreach dw [lrange $dws 1 end] {

   Log::Print INFO "Processing step [incr n]"
   fstdfield read DW IN $dw

   vexpr DWI DWI+$dt*(DW+PDW)
   vexpr PDW DW

   fstdfield define DWI -NOMVAR DWI -DATEO [fstdfield define DW -DATEO]
   fstdfield write DWI OUT 0 False
}

fstdfile close IN
fstdfile close OUT

Log::End