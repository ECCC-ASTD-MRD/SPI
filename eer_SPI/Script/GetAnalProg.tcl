#!/bin/sh
# the next line restarts using tclsh \
exec $SPI_PATH/tclsh "$0" "$@"
#===============================================================================
# Environnement Canada
# Centre Meteorologique Canadian
# 2100 Trans-Canadienne
# Dorval, Quebec
#
# Projet   : Package d'interface pour le modele CANERM
# Fichier  : GetAnalProg.tcl
# Creation : Mars 2000
#
# Description:
#    Determine la date de passage des analyses aux prognostiques.
#
# Modifications :
#
#   Nom         : -
#   Date        : -
#   Description : -
#
#===============================================================================

set DirTmp  [lindex $argv 0]

cd ${DirTmp}

set data [exec cat ${DirTmp}/data_std_sim]

set anal 0
set prog 1e32

foreach file $data {

   set lst [split [lindex $file 0] /]

   if { [lsearch -exact $lst trial] != -1 } {
      set anal 1
   } elseif { [lsearch -exact $lst prog] != -1 } {
      set d   [lindex $lst end]
      set sec [clock scan "[string range $d 4 5]/[string range $d 6 7]/[string range $d 0 3] [string range $d 8 9]:00" -gmt true]
      set h   [string trimleft [string range $d 12 end] 0]

      if { $h!="" } {
         set sec [expr $sec+$h*3600]
      }

      set date [clock format $sec -format "%Y%m%d%H" -gmt true]
      if { $date < $prog } {
         set prog $date
      }
   }
}

if { $prog!=1e32 && $anal } {
   set date $prog
} else {
   set date 0
}

#----- Ecrire la date dans le fichier "traject.points"

file rename ${DirTmp}/traject.points ${DirTmp}/traject.points.ori

set p0 [open ${DirTmp}/traject.points.ori r]
set p1 [open ${DirTmp}/traject.points w]

set i 0
while { [eof $p0] != 1 } {

   incr i 1
   gets $p0 line
   if { $i == 10 } {
      puts $p1 "   $date"
   }
   puts $p1 $line
}

close $p0
close $p1

