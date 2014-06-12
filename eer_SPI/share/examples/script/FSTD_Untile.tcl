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
# Fichier    : FSTD_Untile.tcl
# Creation   : Juin 2014 - J.P. Gauthier - CMC/CMOE
# Description: Untile # grids (like bemol)
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

#----- Enable aumatic untiling
fstdfield autountile True

#----- Open in/out files
fstdfile open TILED read DataIn/2014051600_000_tiled
fstdfile open UNTILED write DataOut/2014051600_000_untiled

#----- Copy grid descriptors
fstdfield read TIC TILED -1 "" -1 -1 -1 "" >>
fstdfield read TAC TILED -1 "" -1 -1 -1 "" ^^
fstdfield write TIC UNTILED 0 True
fstdfield write TAC UNTILED 0 True

#----- Loop on all variables
foreach var [fstdfile info TILED NOMVAR] {
   Log::Print INFO "Processing $var"
   
   #----- Read 1st tile and rebuild grid
   foreach field [fstdfield find TILED -1 "" -1 -1 1 "" $var] {
      fstdfield read FLD TILED $field
      fstdfield define FLD -IP3 0 
      fstdfield write FLD UNTILED 0 True
   }
}

fstdfile close TILED UNTILED

Log::End