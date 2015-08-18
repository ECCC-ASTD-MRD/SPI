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
gribfield create GRIB1NEW 1 1 1 polar_stereographic_sfc_grib1
#puts "   Keys         : [gribfield define GRIB1NEW -key]"

array set iso {
   KR-88 29
   I-131 148
   XE-133 158
   CS-137 169
   AM-241 328
}

#----- Product keys
# set Keys(PRODUCT) {
#    generatingProcessIdentifier 45 
#    table2Version               133
#    indicatorOfTypeOfLevel      105 
#    level                       0
#    timeRangeIndicator          10
#    applicationIdentifier       69
#    type                        77  
#    identificationNumber        69
#    productIdentifier           82
# }

# gribfield define GRIB1NEW -key $Keys(PRODUCT)
# 
# #----- Loop on fields to convert
# foreach var { CV CVI DW WI } id { 200 202 201 205 } nm { conc dose depot wetd } {
# 
#    Log::Print INFO "Processing $var"
#    gribfile open GRIBOUT write DataOut/GRIB_Convert_$nm.grib1
#    
#    foreach fld [fstdfield find FSTDIN -1 "" -1 -1 -1 "" $var] {
# 
#       fstdfield read FLD FSTDIN $fld
# 
#       gribfield import GRIB1NEW FLD    
#       gribfield define GRIB1NEW -key indicatorOfParameter $id
#       gribfield write GRIB1NEW GRIBOUT 16
#    }
#    gribfile close GRIBOUT
# }
 
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

