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
# Fichier    : OBS_ZKrig.tcl
# Creation   : Fevrier 2008 - J.P. Gauthier - CMC/CMOE
# Description: Demonstration des fonctions de calculs de krigging applique a une grille verticale
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

#----- Variables necessaires
set var    O3                                   ;#Observation variable to be used
set hres   1000                                 ;#Horizontal resolution
set vres   1000                                 ;#Vertical resolution
set vmin   1000                                 ;#Vertical minimum
set vmax   5500                                 ;#Vertical maximum
set coords { 42.823 -82.333 41.947 -82.893 }    ;#Start and endpoint
set gdesc  { 1 1 1 }                            ;#Grid descriptor, must change with each cut

#----- Recuperer les observations
Log::Print INFO "Loading dataset"
set obs [lsearch -inline -glob [observation load $env(CI_DATA_IN)/OBS_ZKrig.obs] $var*]

#----- Calculer les positions de grilles
Log::Print INFO "Calculating zgrid coordinates"
projection create PROJ
set coords [projection function PROJ -path $coords $hres]

set elevs  { }
for { set h $vmin } { $h<=$vmax } { set h [expr $h+$vres] } {
   lappend elevs $h
}

set ni [expr [llength $coords]/2]
set nj [llength $elevs]

Log::Print INFO "Creating zgrid (${ni}x${nj})"

#----- Creer la grille verticale (IP1=12001 -> MASL)
fstdfield create SECTION $ni $nj 1 Float32
fstdfield define SECTION -NOMVAR [observation configure $obs -desc] -TYPVAR O -GRTYP V -ETIKET XSECTION \
   -IP1 12001 -IG1 [lindex $gdesc 0] -IG2 [lindex $gdesc 1] -IG3 [lindex $gdesc 2] -IG4 0
fstdfield stats SECTION -leveltype MASL -levels $elevs -grid $coords

#----- Creer les descripteurs de grille
fstdfield create TIC $ni 1 1 Float32
fstdfield define TIC -NOMVAR ^^ -TYPVAR X -GRTYP L 0 0 1.0 1.0 -ETIKET XSECTION -IP1 [lindex $gdesc 0] -IP2 [lindex $gdesc 1] -IP3 [lindex $gdesc 2]
fstdfield create TAC 1 $ni 1 Float32
fstdfield define TAC -NOMVAR >> -TYPVAR X -GRTYP L 0 0 1.0 1.0 -ETIKET XSECTION -IP1 [lindex $gdesc 0] -IP2 [lindex $gdesc 1] -IP3 [lindex $gdesc 2]
fstdfield create TZH $nj 1 1 Float32
fstdfield define TZH -NOMVAR ^> -TYPVAR X -GRTYP X -ETIKET XSECTION -IP1 [lindex $gdesc 0] -IP2 [lindex $gdesc 1] -IP3 [lindex $gdesc 2]
fstdfield define TZH -DATA 0 [list $elevs]

Log::Print INFO "Check values: $elevs == [fstdfield define TZH -DATA 0]"

for { set i 0 } { $i < $ni } { incr i } {
   fstdfield stats TIC -gridvalue $i 0 [lindex $coords [expr $i*2]]
   fstdfield stats TAC -gridvalue 0 $i [lindex $coords [expr $i*2+1]]
}

#----- Effectuer le krigging
Log::Print INFO "Applying krigging over dataset"
fstdfield gridinterp SECTION $obs LINEAR 0.0 1.0 10

Log::Print INFO "Saving zgrid kriging"
fstdfile open  FILE write $env(CI_DATA_OUT)/OBS_ZKrig.fstd
fstdfield write SECTION FILE -32 True
fstdfield write TIC FILE -32 True
fstdfield write TAC FILE -32 True
fstdfield write TZH FILE -32 True
fstdfile close FILE

Log::End
