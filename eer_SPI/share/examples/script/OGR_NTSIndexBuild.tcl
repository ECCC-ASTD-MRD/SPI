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
# Fichier    : OGR_NTSIndexBuild.tcl
# Creation   : Mars 2008 - J.P. Gauthier - CMC/CMOE
# Description: Creer un index NTS pour les diverses base de donnes Canadiennes
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

puts \n[file tail [info script]]

set res     [lindex $argv 0]
set DBPath  [lindex $argv 1]
set NTSPath /cnfs/ops/production/cmoe/geo/NTS

#----- Open original NTS index
set layers  [ogrfile open LAYERFILE read $NTSPath/${res}kindex.shp]
eval ogrlayer read LAYER [lindex $layers 0]

#----- Open indexed NTS index
set name    [file tail $DBPath]${res}kIndex
puts $name

file mkdir $DBPath/Index
file delete $DBPath/Index/${name}.shp
ogrfile open INDEXFILE write $DBPath/Index/${name}.shp "ESRI Shapefile"
ogrlayer create INDEXFILE INDEX ${name}

#----- copy the fields
foreach field [ogrlayer define LAYER -field] {
   puts "$field [ogrlayer define LAYER -field $field]"
   eval ogrlayer define INDEX -field $field [ogrlayer define LAYER -field $field]
}

#----- Insert our fields
ogrlayer define INDEX -field IDX_PATH String

#----- Allocate same feature number
ogrlayer define INDEX -nb [ogrlayer define LAYER -nb]

#----- For all of the features
for { set n 0 } { $n<[ogrlayer define LAYER -nb] } { incr n } {

   set fld  [ogrlayer define LAYER -feature $n SNRC]
   set file [glob -nocomplain -tails -directory $DBPath [string range $fld 0 2]/[string tolower [string index $fld 3]]/[string tolower $fld]/*.tif]

   #----- Copy the source fields
   foreach field [ogrlayer define LAYER -field] {
      ogrlayer define INDEX -feature $n $field [ogrlayer define LAYER -feature $n $field]
   }

   #----- Setup our fields
   if { $file != "" } {
      puts "   Found $file"
      ogrlayer define INDEX -feature $n IDX_PATH $file
   }

   #----- Copy the geometry
   ogrlayer define INDEX -geometry $n True [ogrlayer define LAYER -geometry $n]
}

ogrfile close INDEXFILE LAYERFILE

file copy -force $NTSPath/${res}kindex.prj $DBPath/Index/${name}.prj
