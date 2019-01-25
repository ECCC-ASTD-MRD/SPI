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
# Fichier    : FSTD_Sort.tcl
# Creation   : Mars 2005 - J.P. Gauthier - CMC/CMOE
# Description: Convertir une triangulation irreguliere en champs RPN (Grille M).
#              Pour visualisation dans SPI
#              Aussi applicable au grille icosahedrale.
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

file delete -force DataOut/FSTD_TIN2FSTD.fstd
fstdfile open 1 write  DataOut/FSTD_TIN2FSTD.fstd

set fnod [open [lindex $argv 0].nod r]
set fele [open [lindex $argv 0].ele r]
set fbat [open [lindex $argv 0].bat r]

#----- Triangles
Log::Print INFO "Processing Triangle meshe"
set n 0
set i -1
while { ![eof $fele] } {
   gets $fele line
   incr n
}
seek $fele 0

Log::Print INFO "Found $n triangles"

fstdfield create TR [expr $n*3] 1 1 UInt32
fstdfield define TR -NOMVAR ## -TYPVAR X -IP1 0 -IP2 0 -IP3 0 -GRTYP X -IG1 0 -IG2 0 -IG3 0 -IG4 0
while { ![eof $fele] } {
   gets $fele line
   if { $line!="" } {
      fstdfield stats TR -gridvalue [incr i] 0 [expr [lindex $line 1]-1]
      fstdfield stats TR -gridvalue [incr i] 0 [expr [lindex $line 2]-1]
      fstdfield stats TR -gridvalue [incr i] 0 [expr [lindex $line 3]-1]
   }
}
fstdfield write TR 1 -32 False

#----- Vertices

Log::Print INFO "Processing Vertices"

set n 0
while { ![eof $fnod] } {
   gets $fnod line
   incr n
}
seek $fnod 0

Log::Print INFO "Found $n vertices"

fstdfield create LA $n 1 1 Float32
fstdfield define LA -NOMVAR ^^ -TYPVAR X -IP1 0 -IP2 0 -IP3 0 -GRTYP L 0 0 1.0 1.0
fstdfield create LO $n 1 1 Float32
fstdfield define LO -NOMVAR >> -TYPVAR X -IP1 0 -IP2 0 -IP3 0 -GRTYP L 0 0 1.0 1.0
while { ![eof $fnod] } {
   gets $fnod line
   if { $line!="" } {
      fstdfield stats LA -gridvalue [expr [lindex $line 0]-1] 0 [lindex $line 2]
      fstdfield stats LO -gridvalue [expr [lindex $line 0]-1] 0 [lindex $line 1]
   }
}
fstdfield write LA 1 -32 False
fstdfield write LO 1 -32 False

#----- Values per vertices

Log::Print INFO "Processing Values per Vertices"

fstdfield create BA $n 1 1 Float32
fstdfield define BA -NOMVAR BA -TYPVAR S -IP1 0 -IP2 0 -IP3 0 -GRTYP M
fstdfield define BA -IG1 0 -IG2 0 -IG3 0 -IG4 0
while { ![eof $fbat] } {
   gets $fbat line
   if { $line!="" } {
      fstdfield stats BA -gridvalue [expr [lindex $line 0]-1] 0 [lindex $line 1]
   }
}
fstdfield write BA 1 -32 False

fstdfile close 1

Log::End
