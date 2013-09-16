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
# Fichier    : OGR_TZCheck.tcl
# Creation   : Mars 2006 - J.P. Gauthier - CMC/CMOE
# Description: Determiner le TimeZone d'une coordonee
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

proc TimeZoneCheck { Lat Lon } {

   set tz 999

   if { [llength [set idx [ogrlayer pick TZ [list $Lat $Lon]]]] } {
      set tz [ogrlayer define TZ -feature $idx ZONE]
   }
   return $tz
}

#----- Open the TZ file
set layer [ogrfile open OGRFILE read DataIn/timezone.shp]
eval ogrlayer read TZ [lindex $layer 0]
ogrfile close OGRFILE

#----- Get the TZ for a latlon
Log::Print INFO "TimeZone is: [TimeZoneCheck 65.10 -123.3]"

Log::End