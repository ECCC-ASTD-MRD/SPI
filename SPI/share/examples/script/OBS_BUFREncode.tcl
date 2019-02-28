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
# Fichier    : OBS_BUFREncode.tcl
# Creation   : Mai 2008 - J.P. Gauthier - CMC/CMOE
# Description: Creer un message BUFR de profile du modele meteo dans le temps
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

#----- Define needed values
set Data(Levels)    { 0.9950 0.9850 0.9733 0.9467 0.8780 0.8104 0.7272 0.6646 0.5737 0.4883 0.4091 0.3369 0.2721 0.2522 0.2149 0.1667 }
set Data(Stations)  { { CYUL 1 45.47 -73.75 36 32 } { CYTZ 2 43.62 -79.38 77 32 } { CYVR 3 49.18 -123.17 5 32 } }

set Data(Path)   $env(CMCGRIDF)/prog/glbeta          ;#Model path
set Data(Length) 12                                 ;#Forecast period
set Data(Incr)   -3                                  ;#Time increment
set Data(Gen)    46                                  ;#Generating application
set Data(Year)   2011                                ;#Initial model run year
set Data(Month)  12                                  ;#Initial model run month
set Data(Day)    12                                  ;#Initial model run day
set Data(Hour)   00                                  ;#Initial model run hour
set Data(Minute) 00                                  ;#Initial model run minute

fstdfield vector { UU VV }

#----- Get the data first so we dont have to reload the fields every station while buliding the message
set t $Data(Length)
while { $t>=0 } {

   Log::Print INFO "Processing file : [set file $Data(Path)/$Data(Year)$Data(Month)$Data(Day)$Data(Hour)_[format "%03i" $t]]"
   fstdfile open FILE reas $file
   fstdfield read GZ0 FILE -1 "" 12000 -1 -1 "" "GZ"

   foreach level $Data(Levels) {
      Log::Print INFO "   Processing level : $level"
      fstdfield read WIND   FILE -1 "" "$level ETA" -1 -1 "" "UU"
      fstdfield read TEMPTT FILE -1 "" "$level ETA" -1 -1 "" "TT"
      fstdfield read TEMPTW FILE -1 "" "$level ETA" -1 -1 "" "ES"

      vexpr TM min(TEMPTT,-20.0)
      vexpr PV 10^(9.4041-2354.0/(TM+273.0))
      vexpr TEMPTD 10^(9.4041-2354.0/((TM-clamp(TEMPTW,0.0,20.0))+273.0))/PV

      foreach station $Data(Stations) {

         set lat  [lindex $station 2]
         set lon  [lindex $station 3]
         set gz($station) [expr [fstdfield stats GZ0 -coordvalue $lat $lon]*10.0]
         set prof {}

         lappend prof "007004 [expr ($level*(1000.0-10.0)+10.0)*100.0]"
         set wind [fstdfield stats WIND -coordvalue $lat $lon]
         lappend prof "011001 [expr round([lindex $wind 1])]"
         lappend prof "011002 [expr round([lindex $wind 0]*0.514444)]"
         lappend prof "012101 [expr [fstdfield stats TEMPTT -coordvalue $lat $lon]+273.15]"
         lappend prof "012102 [expr [fstdfield stats TEMPTW -coordvalue $lat $lon]+273.15]"
         lappend prof "012103 [expr [fstdfield stats TEMPTD -coordvalue $lat $lon]+273.15]"
         lappend data($station$t) $prof
      }
   }

   fstdfield free GZ0 WIND TEMPTT TEMPTW TEMPTD
   fstdfile close FILE
   incr t $Data(Incr)
}

#----- Read standard table set
metobs table -readcmc

#----- Create a new template and a new dataset based ont this template
bufrtemplate create TEMPLATE $env(CI_SPI_IN)/OBS_BUFREncode.template
bufrdataset create DATASET TEMPLATE

#----- Fill in the mesage
Log::Print INFO "Processing message"
foreach station $Data(Stations) {

   set lat [lindex $station 2]
   set lon [lindex $station 3]

   set dataset {}
   lappend dataset "001035 53"

   #------ Code 301090
   lappend dataset "301090" "301004" "001001 -1" "001002 [lindex $station 1]" "001015 [lindex $station 0]" "002001 -1" \
      "301011" "004001 $Data(Year)" "004002 $Data(Month)" "004003 $Data(Day)" "301012" "004004 $Data(Hour)" "004005 $Data(Minute)" \
      "301021" "005001 $lat" "006001 $lon" "007030 [lindex $station 4]" "007031 [lindex $station 5]"

   #------ Station info
   lappend dataset "206015" "007196 $gz($station)" "004014 [expr $Data(Length)-$Data(Incr)]" "001032 $Data(Gen)" "108000" "031001 [expr $Data(Length)/3+1]"

   #------ Loop on time steps
   set t $Data(Length)
   while { $t>=0 } {
      lappend dataset "004014 $Data(Incr)" 106[format "%03i" [llength $Data(Levels)]]

      #------ Loop on profiles
      foreach prof $data($station$t) {
         set dataset [concat $dataset $prof]
      }
      incr t $Data(Incr)
   }

   bufrdataset define DATASET -subsetadd $dataset
}

#----- Define various message parameters
bufrdataset define DATASET -BUFR_EDITION 4 -BUFR_MASTER_TABLE 0 -ORIG_CENTER 53 -ORIG_SUB_CENTER 11 -UPDATE_SEQUENCE 0 \
   -DATA_CATEGORY 13 -INTERN_SUB_CATEGORY 255 -LOCAL_SUB_CATEGORY 255 -MASTER_TABLE_VERSION 13 -LOCAL_TABLE_VERSION 50 \
   -YEAR $Data(Year) -MONTH $Data(Month) -DAY $Data(Day) -HOUR $Data(Hour) -MINUTE 0 -SECOND 0 -DATA_FLAG 0

catch { file delete -force $env(CI_SPI_OUT)/OBS_BUFREncode.bufr $env(CI_SPI_OUT)/OBS_BUFREncode.txt }
bufrdataset write DATASET $env(CI_SPI_OUT)/OBS_BUFREncode.bufr BUFR
bufrdataset write DATASET $env(CI_SPI_OUT)/OBS_BUFREncode.txt ASCII

Log::End