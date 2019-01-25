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
# Fichier    : FSTD_IOAPI2RPN.tcl
# Creation   : MArs 2014 - J.P. Gauthier - CMC/CMOE
# Description: Interpoler des donnees IOAPI NetCDF dans des champs RPN
#
# Parametres :
#
# Retour:
#
# Remarques  :
#
#============================================================================

package require TclGeoEER
package require Logger

Log::Start [info script] 0.1

file delete DataOut/FSTD_IOAPI2RPN.fstd

#----- Ouvrir les fichiers d'entree (1) sortie (2)
fstdfile open OUT write DataOut/FSTD_IOAPI2RPN.fstd

#----- Creer un champs sur grille PS de 229x229 en specifiant les IG1 IG2 IG3 et IG4
fstdfield create GRID 229 229 1
fstdfield define GRID -GRTYP N 115.0 300.0 25000.0 360.0

#----- Boucler sur les champs a interpoler
foreach band [gdalfile open IN read DataIn/egts_s.20101212.1.12US1.aqmeii2.ncf] {
   Log::Print INFO "Processing $band"
   
   gdalband read RASTER [list $band]
 
   fstdfield gridinterp GRID RASTER CONSERVATIVE 1 index
   fstdfield define GRID -IP1 [lindex $band 1] -IP2 0 -NOMVAR [lindex [lindex $band 2] 1] -DEET 0 -NPAS 0 

   fstdfield write GRID OUT -32 True
}

gdalfile close IN
fstdfile close OUT

Log::End