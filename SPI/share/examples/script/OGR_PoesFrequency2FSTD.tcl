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
# Fichier    : OGR_POESFrequency2FSTD.tcl
# Creation   : Octobre 2010 - Serge Trudel - CMC/CMOE
# Description: Cacluler un champs FSTD contenant le nombre de passe de POES
#              par point de grille.
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

file delete -force $env(CI_SPI_OUT)/OGR_POESFrequency2FSTD.fstd

#----- Open the files
fstdfile open FSTDOUT write $env(CI_SPI_OUT)/OGR_POESFrequency2FSTD.fstd
ogrfile open VECFILE read $env(CI_SPI_IN)/POES_edm.shp

#----- Read the layers
ogrlayer read LAYER VECFILE 0

#----- Create a PS grid over north hemisphere.
set NI    400
set NJ    400
set nhem  1
set dlat  53.12312
set dlon -113.52434
set dd60  1.0
set xg3   33000.0

fstdfield create GRID $NI $NJ 1

#---- Calculer les parametres xg necessaires
set xg4 [expr (270.0-$dlon+360.0)/360.0]
set xg4 [expr ($xg4-floor($xg4))*360.0]

set xy  [fstdgrid xyfll $dlat $dlon $dd60 $xg4 $nhem]

set xg1 [expr ((($NI-1.0)/2.0) * $xg3 - [lindex $xy 0]) / $xg3 + 1.0]
set xg2 [expr ((($NJ-1.0)/2.0) * $xg3 - [lindex $xy 1]) / $xg3 + 1.0]

#----- Define a PS grid.
fstdfield define GRID -GRTYP N $xg1 $xg2 $xg3 $xg4

#----- Initialiser certains paramtres
fstdfield define GRID -DEET 0 -NPAS 0 -IP1 0 -IP2 0 -IP3 0 -ETIKET "GRID" -NOMVAR GRID -TYPVAR X

#----- Initialise la variable.
vexpr POES GRID<<0

#----- Redifinir le NOMVAR en consequence
fstdfield define POES -NOMVAR "POES"

Log::Print INFO "Global extent    : [ogrlayer stats LAYER -extent]"

#----- Selectionner la couverture des POES
set features [ogrlayer define LAYER -featureselect { { NAME == avhrr_ch1 } }]
Log::Print INFO "Found Features   : $features"
Log::Print INFO "Selection extent : [ogrlayer stats LAYER -extent]"

foreach feature $features {

   #----- Reinitialiser le compteur a 0
   fstdfield clear GRID 0

   #----- Selectionner la feature a rasterizer
   ogrlayer define LAYER -featureselect [list [list - # $feature]]

   #----- Rasteriser la feature avec la valeur 1
   fstdfield gridinterp GRID LAYER FAST 1

   #----- Ajouter a la somme
   vexpr POES POES+GRID
}

fstdfield define POES -ETIKET FAST
fstdfield write POES FSTDOUT -32 True
fstdfile close FSTDOUT

ogrfile close VECFILE

Log::End