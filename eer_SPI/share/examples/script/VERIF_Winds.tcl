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
# Fichier    : VERIF_Urban.tcl
# Creation   : Juin 2010 - J.P. Gauthier - CMC/CMOE
# Description: Vérification des vents pour le modèle urbain
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

set Param(ModelPath) $env(CMCGRIDF)/prog/lam/torg8g20.model/
set Param(ObsPath)   $env(CMCADE)/dbase/surface/metar
set Param(Stations)  { CYVV CYQA CYBN CYKZ CYYZ CYTZ CYKF CYHM CYXU CYSN CYPQ CYTR CYGK KBUF KROC }
set Param(Models)    { 15km 2.5km 1km }

#----- Process yesterday's data
set Param(Date) [lindex $argv 0]
set Param(Path) [lindex $argv 1]
set Param(Secs) [expr [clock scan "$Param(Date) 00:00:00" -gmt True]-86400]
set Param(Date) [clock format $Param(Secs) -format "%Y%m%d" -gmt True]

fstdfield ip1mode NEW

#----- Read the BURP (BUFR) Table
metobs table -readcmc

#----- Create obs and set maximum dt to 10 min
metobs create OBS
metobs define OBS -LAG 360

#----- Read 48 hours of metar
for { set i 0 } { $i< 36 } { incr i 6 } {
   set file [clock format [expr $Param(Secs)+$i*3600] -format "%Y%m%d%H"]_
   if { [file exists $Param(ObsPath)/$file] } {
      Log::Print INFO "Reading metar data $file"
      metobs read OBS $Param(ObsPath)/$file
   }
}

#----- Loop on models
foreach model $Param(Models) {
   Log::Print INFO "Processing $model"

   set Data(BiasSPD$model) 0
   set Data(BiasDIR$model) 0
   set Data(BiasTMP$model) 0
   set Data(EAMSPD$model) 0
   set Data(EAMDIR$model) 0
   set Data(EAMTMP$model) 0
   set Data(BiasSPDNB$model) 0
   set Data(BiasTMPNB$model) 0

   #----- Loop on files
   foreach file [glob $Param(ModelPath)/$Param(Date)??_???_${model}] {

      Log::Print INFO "   Processing $file"
      fstdfile open FSTDFILE read $file

      foreach fld [fstdfield find FSTDFILE -1 "" { 1.0 HYBRID } -1 -1 "" "UU"] {

         #----- Get fields
         fstdfield read MODELUU FSTDFILE $fld
         fstdfield read MODELTT FSTDFILE [fstdfield define MODELUU -DATEV] "" { 1.0 HYBRID } -1 -1 "" "TT"

         #----- Get it's validity date in seconds
         set sec [fstdstamp toseconds [fstdfield define MODELUU -DATEV]]
         lappend Data(Secs) $sec
         Log::Print INFO "      Processing $sec"

         #----- Loop on stations
         foreach id $Param(Stations) {

            if { [catch { set coo [metobs define OBS -COORD $id]}] } {
               Log::Print INFO "         No data for station $id"
               continue
            }

            #----- Get obs values
            set ospd [lindex [metobs define OBS -ELEMENT $id 11012 $sec] 0]
            set odir [lindex [metobs define OBS -ELEMENT $id 11011 $sec] 0]
            set otmp [lindex [metobs define OBS -ELEMENT $id 12004 $sec] 0]
            if { $ospd=="" || $odir=="" || $otmp=="" } {
               continue
            }
            set ospd [expr $ospd*1.94]
            set otmp [expr $otmp-273.15]

            #----- Get model values
            if { [set mod [fstdfield stats MODELUU -coordvalue [lindex $coo 0] [lindex $coo 1]]]=="-" } {
               set mspd -999
               set mdir -999
            } else {
               set mspd  [lindex $mod 0]
               set mdir  [lindex $mod 1]

               #----- For direction bias, we have to get the minimum compass diff.
               #      When we invert the diff (+-360) we have to flip the sign to keep
               #      bias direction coherency
               set dir [expr $mdir-$odir]
               set dir [expr $dir>180?-($dir-360.0):($dir<-180?-($dir+360):$dir)]

               set Data(BiasSPD$model) [expr $Data(BiasSPD$model)+($mspd-$ospd)]
               set Data(BiasDIR$model) [expr $Data(BiasDIR$model)+$dir]
               set Data(EAMSPD$model)  [expr $Data(EAMSPD$model)+abs($mspd-$ospd)]
               set Data(EAMDIR$model)  [expr $Data(EAMDIR$model)+abs($dir)]
               incr Data(BiasSPDNB$model)
            }

            if { [set mtmp [fstdfield stats MODELTT -coordvalue [lindex $coo 0] [lindex $coo 1]]]=="-" } {
               set mtmp -999
            } else {
                set Data(BiasTMP$model) [expr $Data(BiasTMP$model)+($mtmp-$otmp)]
                set Data(EAMTMP$model)  [expr $Data(EAMTMP$model)+abs($mtmp-$otmp)]
                incr Data(BiasTMPNB$model)
            }

            set Data($model$sec$id) [list $mspd $mdir $mtmp $ospd $odir $otmp]
         }
      }
      fstdfile close FSTDFILE
   }
}

#----- Print Output
set Data(Secs) [lsort -unique -increasing $Data(Secs)]

set f [open $Param(Path)/$Param(Date).txt w]

puts $f [format "%-8s %-14s %8s %8s %8s %8s %8s %8s %8s %8s %8s %8s %8s %8s" Station Date ObsSPD ObsDIR ObsTmp 15kmSPD 15kmDIR 15KmTmp 2.5kmSPD 2.5kmDIR 2.5KmTmp 1kmSPD 1kmDIR 1KmTmp]
foreach sec $Data(Secs) {
   foreach id $Param(Stations) {
      set str [format "%-8s %-14s" $id [clock format $sec -format "%Y%m%d %H:%M"]]
      set nb 0
      foreach model $Param(Models) {
         if { [info exists Data($model$sec$id)] } {
            if { $nb==0 } {
               append str [format " %8.3f %8.3f %8.3f" [lindex $Data($model$sec$id) 3] [lindex $Data($model$sec$id) 4] [lindex $Data($model$sec$id) 5]]
            }
            append str [format " %8.3f %8.3f %8.3f" [lindex $Data($model$sec$id) 0] [lindex $Data($model$sec$id) 1] [lindex $Data($model$sec$id) 2]]
            incr nb
         }
      }
      if { $nb==[llength $Param(Models)] } {
         puts $f $str
      }
   }
}

#----- Print Bias
set str [format "%-8s %-14s %8s %8s %8s" BIAS "" "" "" ""]
foreach model $Param(Models) {
   append str [format " %8.3f %8.3f %8.3f" [expr $Data(BiasSPD$model)/$Data(BiasSPDNB$model)] [expr $Data(BiasDIR$model)/$Data(BiasSPDNB$model)] [expr $Data(BiasTMP$model)/$Data(BiasTMPNB$model)]]
}
puts $f $str

#----- Print EAM
set str [format "%-8s %-14s %8s %8s %8s" EAM "" "" "" ""]
foreach model $Param(Models) {
   append str [format " %8.3f %8.3f %8.3f" [expr $Data(EAMSPD$model)/$Data(BiasSPDNB$model)] [expr $Data(EAMDIR$model)/$Data(BiasSPDNB$model)] [expr $Data(EAMTMP$model)/$Data(BiasTMPNB$model)]]
}
puts $f $str

close $f

Log::End