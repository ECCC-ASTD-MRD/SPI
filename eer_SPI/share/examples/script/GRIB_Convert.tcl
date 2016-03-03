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
# Fichier    : GRIB_Convert.tcl
# Creation   : Fevrier 2015 - J.P. Gauthier - CMC/CMOE
# Description: Convertir FSTD a GRIB
#
# Parametres :
#
# Retour:
#
# Remarques  :
#   Aucune.
#
#============================================================================

package require TclData
#package require TclGeoEER
package require Logger

Log::Start [info script] 0.1

fstdfile open FSTDIN read DataIn/2015040712_000.con

Log::Print INFO "Producing GRIB1"

#----- Use a template file GRIB1
#gribfield create GRIB1NEW 1 1 1 polar_stereographic_sfc_grib1

#----- Use an existing file 
puts stderr [gribfile open GRIB_FILE read DataIn/argos.grib]
gribfield read GRIB1NEW GRIB_FILE 0

#----- Product keys
set Keys(PRODUCT) {
   generatingProcessIdentifier 45 
   table2Version               133
   indicatorOfTypeOfLevel      105 
   level                       0
   timeRangeIndicator          10
}

gribfield define GRIB1NEW -key $Keys(PRODUCT)
puts "   Keys         : [gribfield define GRIB1NEW -key]"

#----- The isotope is encoded in the local setction of the PDS (bytes 40+)
#      It's kind of hack extracted directly from grib-cvt but adapted to fit into ecmwf's key system
namespace eval GRIB { 
   variable ARGOSId
   variable ARGOSKeys
   
   set ARGOSKeys { applicationIdentifier  type identificationNumber productIdentifier spatialSmoothingOfProduct ext01 ext02 ext03 ext04 ext05 ext06 ext07 ext08 ext09 ext10 }

   array set ARGOSId {
      KR-88 29
      I-131 148
      I-132  149
      XE-133 15
      CS-137 169
      AM-241 328
   }
}

proc GRIB::EncodeIso { FieldId Iso Date Time NProg NAnal } {
   variable ARGOSId
   variable ARGOSKeys
   
   #----- Calculate date related bytes
   set b10 [expr $Date%256]
   set Date [expr ($Date-$b10)>>8]
   set b09 [expr $Date%256]
   set Date [expr ($Date-$b09)>>8]
   set b08 [expr $Date%256]
   set Date [expr ($Date-$b08)>>8]
   set b07 [expr $Date%256]
   puts stderr "$Time.."
   #----- Calculate time related bytes
   set b12 [expr $Time%256]
   set Time [expr ($Time-$b12)>>8]
   set b11 [expr $Time%256]

   puts stderr "$b11 $b12"
   #----- Get the bynary encoding of theses bytes
   set bytes [binary format a4ccccccccccc EMER 1 [expr ($ARGOSId($Iso)&0x03ff)>>8] [expr $ARGOSId($Iso)] $b07 $b08 $b09 $b10 $b11 $b12 $NAnal $NProg]
   binary scan $bytes cu* t

   #----- Define key values
   foreach key $ARGOSKeys val $t {
      gribfield define $FieldId -key $key $val
   }
}

#----- Loop on fields to convert
foreach var { CV CVI DW WI } id { 200 202 201 205 } nm { conc dose depot wetd } {

   Log::Print INFO "Processing $var"
   gribfile open GRIBOUT write DataOut/GRIB_Convert_$nm.grib1
   
   foreach fld [fstdfield find FSTDIN -1 "" -1 -1 -1 "" $var] {

      fstdfield read FLD FSTDIN $fld

      gribfield import GRIB1NEW FLD    
      gribfield define GRIB1NEW -key indicatorOfParameter $id
      GRIB::EncodeIso GRIB1NEW [fstdfield define FLD -ETIKET] 20160208 1000 1 2
      gribfield write GRIB1NEW GRIBOUT 16
   }
   gribfile close GRIBOUT
}

exit

Log::Print INFO "Producing GRIB2"
 
#----- Use a template file GRIB2
gribfield create GRIB2NEW 1 1 1 polar_stereographic_sfc_grib2
# puts "   Keys         : [gribfield define GRIB2NEW -key]"

#----- Product keys
set Keys(PRODUCT) {
   generatingProcessIdentifier     45 
   productDefinitionTemplateNumber 40
   parameterCategory               18
   indicatorOfTypeOfLevel          105 
   level                           0
   timeRangeIndicator              10
}

gribfield define GRIB2NEW -key $Keys(PRODUCT)

#----- Loop on fields to convert
foreach var { CV CVI DW WI } id { 10 8 13 11 } nm { conc dose depot wetd } {

   Log::Print INFO "Processing $var"
   gribfile open GRIBOUT write DataOut/GRIB_Convert_$nm.grib2
   
   foreach fld [fstdfield find FSTDIN -1 "" -1 -1 -1 "" $var] {

      fstdfield read FLD FSTDIN $fld
      gribfield import GRIB2NEW FLD    
      gribfield define GRIB2NEW -key parameterNumber $id 
      gribfield define GRIB2NEW -key constituentType $iso([fstdfield define FLD -ETIKET])
      gribfield write GRIB2NEW GRIBOUT 16
   }
   gribfile close GRIBOUT
}

fstdfile close FSTDIN

Log::End

