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
# Fichier    : OGR_OGR2FSTD.tcl
# Creation   : Mai 2000 - J.P. Gauthier - CMC/CMOE
# Description: Demonstration de la rasterization de donnnes OGR dans un champs RPN.
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

catch { file delete -force $env(CI_DATA_OUT)/OGR_OGR2FSTD.fstd }

#----- Open the files
fstdfile open FSTDIN read $env(CI_DATA_IN)/noire.fst
fstdfile open FSTDOUT write $env(CI_DATA_OUT)/OGR_OGR2FSTD.fstd
ogrfile open VECFILE read $env(CI_DATA_IN)/RADAR.shp

#----- Read the layers
ogrlayer read LAYER VECFILE 0

#----- Copier les TIC TACT
fstdfield read TIC FSTDIN -1 "" -1 -1 -1  "" ">>"
fstdfield read TAC FSTDIN -1 "" -1 -1 -1  "" "^^"
fstdfield write TIC FSTDOUT -32 False
fstdfield write TAC FSTDOUT -32 False

#----- Lire le champs de la grille de calcul
fstdfield read FLD FSTDIN -1 "" -1 -1 -1  "" "ELEV"

puts "Global extent    :[ogrlayer stats LAYER -extent]"
puts "XFT extent       :[ogrlayer stats LAYER -extent 175]"

#----- Selectionner le radar XFT
ogrlayer define LAYER -featureselect { { ID == XFT } }

puts "Selection extent :[ogrlayer stats LAYER -extent]"
foreach mode { FAST WITHIN INTERSECT CONSERVATIVE NORMALIZED_CONSERVATIVE ALIASED POINT_CONSERVATIVE LENGTH_CONSERVATIVE LENGTH_NORMALIZED_CONSERVATIVE LENGTH_ALIASED } {
   puts "Testing mode $mode."
   vexpr FLD FLD<<0
   fstdfield gridinterp FLD LAYER $mode 1
   fstdfield define FLD -ETIKET $mode
   fstdfield write FLD FSTDOUT -32 True
}
ogrfile close VECFILE

fstdfile close FSTDIN FSTDOUT

Log::End