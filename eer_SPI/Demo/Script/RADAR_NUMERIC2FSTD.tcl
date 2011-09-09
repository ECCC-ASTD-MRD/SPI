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
# Fichier    : RADAR_NUMERCI2FSTD.tcl
# Creation   : Septembre 2011 - J.P. Gauthier - CMC/CMOE
# Description: Transformer un fichier NUMERIC en FSTD
#
# Parametres :
#    <numeric> : Fichier numeric
#
# Retour:
#
# Remarques  :
#
#============================================================================

package require TclData

puts \n[file tail [info script]]

set Param(N)        RIDX
set Param(DBZ)      RDBR
set Param(MM/HR)    RRAI
set Param(DBZ_SNOW) RDBS
set Param(CM/HR)    RSNO

set Data(Table) ""

set numeric [lindex $argv 0]

#----- Read the NUMERIC file
set f [open $numeric r]

while { ![eof $f] } {
   gets $f line

   switch [lindex $line 0] {
      Title -
      MajorProductType -
      ValidTime        -
      MinorProductType -
      MinorProductParameters -
      FieldType -
      Scale -
      Width -
      Height -
      LatCentre -
      LonCentre -
      Interpolation -
      Projection -
      ReferenceLongitude -
      TrueLatitude -
      TableLabels_PrecipitationRate-Reflectivity -
      DataFormat -
      SizeInBytes { set Data([lindex $line 0]) [lrange $line 1 end] }

      TableStart_PrecipitationRate-Reflectivity {
         while { [gets $f line] } {
            if { $line=="TableEnd_PrecipitationRate-Reflectivity" } {
               break
            }
            set Data(Table) [concat $Data(Table) [string trim $line \n]]
         }
      }
      Data {
         set Data(Data) [read $f $Data(SizeInBytes)]
         break;
      }
      default { puts stderr "(ERROR) Unrecognized header:\n\t $line"; exit 1 }
   }
}

#----- Build lookup tables
set Data(Table) [lrange [split $Data(Table) \;] 0 end-1]
foreach val $Data(Table) {
   set i 0
   foreach idx $val {
      if { ![vector is VECT$i] } {
         vector create VECT$i
      }
      vector append VECT$i $idx
      incr i
   }
}

close $f

#----- Process to FSTD
set fstd [string range $Data(ValidTime) 0 9]_[string range $Data(ValidTime) 10 11]ref_[expr int($Data(Scale))]km
file delete $fstd
fstdfile open FSTD write $fstd

#----- Create grid projection
switch $Data(Projection) {

   PolarStereographic {
      if { $Data(LatCentre)<=0 } {
         set grtyp SUD
         set nhem  2
         set xg4  [expr 90.0+$Data(LonCentre)]
         set xg4  [expr floor(fmod($xg4+360.0,360.0))]
      } else {
         set grtyp NORD
         set nhem  1
         set xg4   [expr (270.0-$Data(ReferenceLongitude)+360.0)/360.0]
         set xg4   [expr ($xg4-floor($xg4))*360.0]
      }
      set dd60 1.0
      set xy [fstdgrid xyfll $Data(LatCentre) $Data(LonCentre) $dd60 $xg4 $nhem]
      set xg3 [expr $Data(Scale)*1000]
      set xg1 [expr ((($Data(Width) -1.0)/2.0) * $xg3 - [lindex $xy 0]) / $xg3 + 1.0]
      set xg2 [expr ((($Data(Height)-1.0)/2.0) * $xg3 - [lindex $xy 1]) / $xg3 + 1.0]

      fstdfield create GRID $Data(Width) $Data(Height) 1
      fstdfield define GRID -NOMVAR GRID
      fstdfield define GRID -GRTYP $grtyp $xg1 $xg2 $xg3 $xg4
   }
}

#----- Get the data indexes
binary scan $Data(Data) c* Data(Indexes)

if { [llength $Data(Indexes)] != $Data(SizeInBytes) } {
   puts stderr "(WARNING) Mismatch between Data length ([llength $Data(Indexes)]) and specifid size $Data(SizeInBytes)"
}

#----- Create index field
set i 0
set j 0
foreach index $Data(Indexes) {
   fstdfield stats GRID -gridvalue $i $j $index

   if { [incr i]==$Data(Width) } {
      incr j
      set i 0
   }
}

#----- LUT and Save fields
set i 0
foreach field $Data(TableLabels_PrecipitationRate-Reflectivity) {
   vexpr FLD$field lut(GRID,VECT0,VECT$i)

   fstdfield define FLD$field -NOMVAR $Param($field) -ETIKET $field -DATEO [fstdstamp fromdate [string range $Data(ValidTime) 0 7] [string range $Data(ValidTime) 8 end]0000]
   fstdfield write FLD$field FSTD -32 True
   incr i
}

fstdfile close FSTD