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

puts \n[file tail [info script]]

catch { file delete -force DataOut/OGR_OGR2FSTD.fstd }

#----- Open the files
fstdfile open FSTDIN read DataIn/noire.fst
fstdfile open FSTDOUT write DataOut/OGR_OGR2FSTD.fstd
ogrfile open VECFILE read DataIn/RADAR.shp

#----- Read the layers
ogrlayer read LAYER VECFILE 0

#----- Copier les TIC TACT
fstdfield read TIC FSTDIN -1 "" -1 -1 -1  "" ">>"
fstdfield read TAC FSTDIN -1 "" -1 -1 -1  "" "^^"
fstdfield write TIC FSTDOUT -32 False
fstdfield write TAC FSTDOUT -32 False

#----- Lire le champs de la grille de calcul
fstdfield read FLD FSTDIN -1 "" -1 -1 -1  "" "ELEV"

#----- Selectionner le radar XFT
ogrlayer define LAYER -featureselect { { ID == XFT } }
foreach mode { FAST WITHIN INTERSECT CONSERVATIVE NORMALIZED_CONSERVATIVE ALIASED POINT_CONSERVATIVE LENGTH_CONSERVATIVE LENGTH_NORMALIZED_CONSERVATIVE LENGTH_ALIASED } {
   puts "Testing mode $mode."
   vexpr FLD FLD<<0
   fstdfield gridinterp FLD LAYER $mode 1
   fstdfield define FLD -ETIKET $mode
   fstdfield write FLD FSTDOUT -32 True
}
ogrfile close VECFILE

fstdfile close FSTDIN
fstdfile close FSTDOUT
