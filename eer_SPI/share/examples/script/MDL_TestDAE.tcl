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
# Fichier    : MDL_TestDAE.tcl
# Creation   : Janvier 2010 - J.P. Gauthier - CMC/CMOE
# Description: Test de lecture de fichier Collada DAE
#
# Parametres :
#
# Retour:
#
# Remarques  :
#   Aucune.
#
#============================================================================

package require TclData
#package require TclGeoEER

puts \n[file tail [info script]]

#model read DAE /users/dor/afsr/005/Data/model.dae
set models [glob -tails -directory /cnfs/ops/cmoe/afsr005/Projects/UrbanX/Collada *]
#set models TourSunlife
#set models AldredBuilding
foreach model $models {
   puts "   Reading $model"
   model read $model /cnfs/ops/cmoe/afsr005/Projects/UrbanX/Collada/$model/doc.kml
   model configure $model -outline blue -width 1
}

#model read DAE /cnfs/ops/cmoe/afsr005/Projects/UrbanX/Collada/StadeOlympiqueEtBiodome/models/model.dae
#model read DAE /cnfs/ops/cmoe/afsr005/Projects/UrbanX/Collada/StadeOlympiqueEtBiodome/doc.kml
#model configure DAE -outline black -width 1 -renderface False
#model matrix DAE -locate 46.8086773762 -71.2179046536 125
#model matrix DAE -scale 1 1 1

eval Mapper::UpdateData $Page::Data(Frame) $models

if { 0 } {
   file copy -force DataIn/Montreal.fstd DataOut/Montreal.fstd
   fstdfile open FILE append DataOut/Montreal.fstd
   fstdfield read FLD FILE -1 "" -1 -1 -1 "" "IBLK"
   fstdfield clear FLD 1.0

   foreach model $models {
      puts "   Rasterising $model"
      fstdfield gridinterp FLD $model FAST 0.0
   }
   fstdfield define FLD -NOMVAR MASK
   fstdfield write FLD FILE 0 True
   fstdfile close FILE
}
