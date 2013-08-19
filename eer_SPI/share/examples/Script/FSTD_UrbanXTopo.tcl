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
# Fichier    : FSTD_UrbanXTopo.tcl
# Creation   : Octobre 2007 - J.P. Gauthier - CMC/CMOE
# Description: Ajouter la topo aux champs de Urban(X)
#
# Parametres :
#    <concfile> : Fichier de concentrations
#    <partfile> : Fichier de particules
#    <topofile> : Fichier de topographie (GDAL)
#
# Retour:
#
# Remarques  :
#   -Pour le fichier de concentration on ajoute un champs GZ.
#   -Pour le fichier de particule, on ajoute la topo a chaque champs ZH
#
#============================================================================

package require TclData
#package require TclGeoEER

puts \n[file tail [info script]]

#----- Arguments
#   UrbanXTopo [concfile] [partfile] [topofile]

fstdfile open CONC write [lindex $argv 0]
fstdfile open PART write [lindex $argv 1]
gdalfile open GDAL read  [lindex $argv 2]

#----- Read in topo

gdalband read TOPO [list { GDAL 1 }]

#----- Process concentration file

fstdfield read FLD CONC -1 "" -1 -1 -1 "" "CV"

fstdfield stats FLD -nodata 0.0
fstdfield clear FLD
fstdfield gridinterp FLD TOPO
fstdfield define FLD -NOMVAR GZ -IP1 0 -IP2 0 -IP3 0
fstdfield write FLD CONC 0 True

#----- Process particle file

foreach field [fstdfield find PART -1 "" -1 -1 -1 "" "ZH"] {
   fstdfield read FLD PART $field
   fstdfield copy FLD2 FLD
   fstdfield gridinterp FLD2 TOPO
   vexpr FLD FLD+FLD2
   fstdfield write FLD PART 0 True
}

fstdfile close CONC
fstdfile close PART
gdalfile close GDAL