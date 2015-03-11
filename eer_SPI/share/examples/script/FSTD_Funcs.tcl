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
# Fichier    : FSTD_Funcs.tcl
# Creation   : Juillet 2009 - J.P. Gauthier - CMC/CMOE
# Description: Test de fonctions variee
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

fstdfield ip1mode NEW

file delete DataOut/FSTD_Funcs.fstd
fstdfile open OUT write DataOut/FSTD_Funcs.fstd

set secs [clock seconds]
puts "Testing stamp functions for [clock format $secs]"
puts "   seconds: $secs"
puts "   stamp  : [set stamp [fstdstamp fromseconds $secs]]"
puts "   incr 24: [set stamp [fstdstamp incr $stamp 024]]"
puts "   r.date : [exec r.date $stamp]"
puts "   date   : [clock format [fstdstamp toseconds $stamp]]"

puts "\nTesting ip conversion :"
puts "   ip 12000        : [fstdgrid convip 12000]"
puts "   ip 176260768    : [fstdgrid convip 176260768]"
puts ""
puts "   1000.0 PRESSURE : [fstdgrid convip 1000.0 PRESSURE] -> [fstdgrid convip [fstdgrid convip 1000.0 PRESSURE]]"
puts "   1000.0 MASL     : [fstdgrid convip 1000.0 MASL] -> [fstdgrid convip [fstdgrid convip 1000.0 MASL]]"
puts "   1000.0 MAGL     : [fstdgrid convip 1000.0 MAGL] -> [fstdgrid convip [fstdgrid convip 1000.0 MAGL]]"
puts "   0.9    ETA      : [fstdgrid convip 0.9 ETA] -> [fstdgrid convip [fstdgrid convip 0.9 ETA]]"
puts "   0.9    SIGMA    : [fstdgrid convip 0.9 SIGMA] -> [fstdgrid convip [fstdgrid convip 0.9 SIGMA]]"
puts "   0.9    HYBRID   : [fstdgrid convip 0.9 HYBRID] -> [fstdgrid convip [fstdgrid convip 0.9 HYBRID]]"
puts "   10.5   HOUR     : [fstdgrid convip 10.5 HOUR] -> [fstdgrid convip [fstdgrid convip 10.5 HOUR]]"
puts "   10     COUNT    : [fstdgrid convip 10.0 COUNT] -> [fstdgrid convip [fstdgrid convip 10.0 COUNT]]"
puts "   10     IDX      : [fstdgrid convip 10.0 IDX] -> [fstdgrid convip [fstdgrid convip 10.0 IDX]]"
puts "   10     MPRES    : [fstdgrid convip 10.0 MPRES] -> [fstdgrid convip [fstdgrid convip 10.0 MPRES]]"
puts "   10     UNDEFINED: [fstdgrid convip 10.0 UNDEFINED] -> [fstdgrid convip [fstdgrid convip 10.0 UNDEFINED]]"

puts "\nTesting 3D field"
fstdfield create FLDINT 10 1 58 Float32
fstdfield define FLDINT -NOMVAR VTST -TYPVAR X -ETIKET XSECTION
fstdfield write FLDINT OUT -32 True

puts "\nTesting integer data"
fstdfield create FLDINT 10 10 1 UInt32
fstdfield stats FLDINT -gridvalue 5 5 20061231
puts "   20061231 = [fstdfield stats FLDINT -gridvalue 5 5]"

puts "\nTesting alias"
fstdfield copy FLDALIAS FLDINT True
puts "   Alias good = [fstdfield stats FLDALIAS -gridvalue 5 5]"
fstdfield free FLDINT 
fstdfield create FLDNEWINT 10 10 1 UInt32
puts "   Alias freed = [fstdfield stats FLDALIAS -gridvalue 5 5]"
fstdfield free FLDALIAS

puts "\nTesting link/unlink"
fstdfile open ETA read DataIn/2006122900_000.eta
fstdfile open HYB read DataIn/2006122900_000.hyb
set lnk  [fstdfile link { ETA HYB }]

fstdfield ip1mode OLD
set idxs [fstdfield find $lnk -1 "" { 1.0 SIGMA } -1 -1 "" "TT"]
puts "   Found [llength $idxs] fields from $lnk" 

foreach idx $idxs {
   fstdfield read FLD $lnk $idx
   puts "     Level [fstdfield stats FLD -level] [fstdfield stats FLD -leveltype]"
}
fstdfile unlink { ETA HYB }
fstdfile close ETA HYB

set inter { -20 0 20 }
puts "\nTesting contour extraction ($inter):"
fstdfile open 1 read DataIn/2005102612_012
fstdfield read TT 1 -1 "" 12000 -1 -1 "" "TT"

puts "\nTesting levels :"
puts "   IP1           : [fstdfield define TT -IP1]"
puts "   levelindex    : [fstdfield stat TT -levelindex]"
puts "   level         : [fstdfield stat TT -level]"
puts "   leveltype     : [fstdfield stat TT -leveltype]"

fstdfield configure TT -intervals $inter
fstdfield stats TT -limits { 10 10 0 100 100 0 }

set n 0
foreach contour [fstdfield stats TT -coordcontour] {
   puts "  Contour [incr n] length [llength $contour]"
}

puts "\nTesting insideness functions"
puts "   Range : 50 -150 60 -140"
puts "      Gridpoints within range : [fstdfield stats TT -within [list 50 -150 60 -140]]"
puts "      Minimum within range    : [fstdfield stats TT -min [list 50 -150 60 -140]]"
puts "      Avegage within range    : [fstdfield stats TT -avg [list 50 -150 60 -140]]"

set poly [list 50 -150 50 -140 60 -140 60 -150]
puts "   Polygon : $poly"
puts "      Gridpoints within polygon : [fstdfield stats TT -within $poly]"
puts "      Minimum within polygon    : [fstdfield stats TT -min $poly]"
puts "      Avegage within polygon    : [fstdfield stats TT -avg $poly]"

projection create PROJ
set poly [projection function PROJ -path $poly 10000]
puts "   Polygon : $poly"
puts "      Gridpoints within polygon : [fstdfield stats TT -within $poly]"
puts "      Minimum within polygon    : [fstdfield stats TT -min $poly]"
puts "      Avegage within polygon    : [fstdfield stats TT -avg $poly]"

fstdfile open AVGFILE read DataIn/average_PR_2011062200_2011082400
fstdfield read PR AVGFILE -1 "" -1 -1 -1 "" "PR"
puts "      Gridpoints within range : [llength [fstdfield stats PR -within [list 10.0 -180 10.1 180]]]"
puts "      Gridpoints within range : [llength [fstdfield stats PR -within [list 10.0 0.0  10.1 360.0]]]"
puts "      Gridpoints within range : [llength [fstdfield stats PR -within [list 10.0 -180 10.1 0.0]]]"
puts "      Gridpoints within range : [llength [fstdfield stats PR -within [list 10.0 0.01 10.1 180]]]"
puts "      Gridpoints within range : [fstdfield stats PR -within [list 10.0 0.1 10.1 360]]]"
#puts "      Gridpoints within range : [fstdfield stats PR -within [list 10.0 -20.0 10.1 20.0]]"
#puts "      Gridpoints within range : [fstdfield stats PR -within [list 10.0 20.0 10.1 -20.0]]"
#puts "      Gridpoints within range : [fstdfield stats PR -within [list 10.0 20.0 10.1 30.0]]"
#puts "      Gridpoints within range : [fstdfield stats PR -within [list 10.0 30.0 10.1 20.0]]"

puts "\nTesting parser's slicers"
puts "   min=[vexpr NIL smin(TT)] [fstdfield stats TT -min]"
puts "   min((10,20),(10,20))=[vexpr NIL smin(TT((10,20),(10,20)))]"

vexpr NIL TT((10,20),(10,20))
puts "   data=[join [fstdfield define NIL -DATA 0] \n]"

puts "\nTesting parser's bins"
set n [vexpr - win(-20,-10,TT)]
puts "   single bin: $n"

vector create BIN0 { -20 -10 0 10 20 }
vector create BIN1 { -10 0 10 20 30 }
set n [vexpr COUNT win(BIN0,BIN1,TT)]
puts "   multi bin: [vector get COUNT]"

puts "\nTesting vertical profile of wind"
fstdfield read UU 1 -1 "" -1 -1 -1 "" "UU"
fstdfield readcube UU

vexpr WS (\[UU\] * 0.51444)
vexpr WD @UU@
fstdfield vertical PROFIL_WS WS [list 50.0 -150.0]
fstdfield vertical PROFIL_WD WD [list 50.0 -150.0]

#----- Teste la creation d'un masque de grille plus petite dans un grille plus grande
fstdfile open 2 read DataIn/2006122900_000.eta
fstdfield read GZ 2 -1 "" 12000 -1 -1 "" "GZ"
fstdfield stats GZ -nodata 0.0
vexpr GZ GZ<<0
#vexpr TT TT<<1

fstdfield configure GZ -extrapdegree VALUE
fstdfield gridinterp GZ TT

fstdfile open 3 write DataOut/2006122900_000.eta.mask
fstdfield write GZ 3 -32 True
fstdfield read TIC 2 -1 "" -1 -1 -1 "" ">>"
fstdfield read TAC 2 -1 "" -1 -1 -1 "" "^^"
fstdfield write TIC 3 -32 True
fstdfield write TAC 3 -32 True
fstdfile close 1 2 3

puts "\nTesting Y grid coordinate interpolation"
fstdfile open 1 read DataIn/2013061419_024
fstdfield read ZH 1 -1 "" -1 -1 -1 "" "ZH"
fstdfield read CV 1 -1 "" -1 -1 -1 "" "CV"

puts "   I J  : [set ij [fstdfield stats ZH -coordpoint 39.021 -69.017]]"
puts "   Value: [fstdfield stats ZH -gridvalue 39.021 -69.017] [fstdfield stats ZH -gridvalue [lindex $ij 0] [lindex $ij 1]]"

puts "\nTesting Y grid field interpolation"
fstdfield gridinterp ZH CV
puts [fstdfield define ZH -DATA 0]

puts "\nTesting Y grid concatenation"
set data [fstdfield define ZH -DATA]
set ni   [fstdfield define ZH -NI]

fstdfield create NIL2 [expr $ni*2] 1 1 Float32
fstdfield define NIL2 -DATA $data$data

set data [fstdfield define NIL2 -DATA 0]
puts "  Values: [lrange [lindex $data 0] $ni [expr $ni+20]]"

puts "\nTesting read/write TIC/TAC/TOC"
fstdfile open GEM4 read  DataIn/2014061900_000.gem4

fstdfield read TIC  GEM4 -1 "" -1 -1  -1 "" ">>"
fstdfield read TAC  GEM4 -1 "" -1 -1  -1 "" "^^"
fstdfield read TOC  GEM4 -1 "" -1 -1  -1 "" "!!"
fstdfield write TIC OUT 0 True
fstdfield write TAC OUT 0 True
fstdfield write TOC OUT 0 True

puts "\nTesting mscale"
#fstdfield create SC 229 229 1
#fstdfield define SC -NOMVAR SCAL -DATYP 5 -GRTYP N 115.0 115.0 150000.0 8.59999942779541
fstdfield create SC 120 120 1
fstdfield define SC -NOMVAR SCAL -DATYP 5 -GRTYP N 60.4999999 2591.691 2000.0 348.724819
fstdgrid mscale SC
fstdfield write SC OUT -32 True

fstdfile close GEM4 OUT

#catch { fstdfile open BAD read DataOut/2012022412_TOT_ES.txt }
#fstdfile open OK read DataOut/2006122900_000.eta.mask

#----- Test un fichier trunque
puts "\nTesting truncated file:"
fstdfile open TRUNC read DataIn/truncated
if { [catch { fstdfield read BADFIELD TRUNC -1 "" -1 -1 -1 "" "SN" } msg] } {
   puts "\n   File is truncated: $msg"
}
fstdfile close TRUNC

#----- Test l'ouverture de plus de 1000 fichiers
puts "\nTesting multiple file open:"
for { set i 0 } { $i<=1001 } { incr i } {
   puts "   Opening file $i"
   fstdfile open FILE$i read DataIn/2005102612_012
   fstdfile close FILE$i
}

fstdfield free FLDINT NIL TT GZ TIC TAC

Log::End