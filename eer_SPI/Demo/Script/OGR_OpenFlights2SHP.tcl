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
# Fichier    : OGR_OpenFlights2SHP.tcl
# Creation   : Septembre 2012 - J.P. Gauthier - CMC/CMOE
# Description: Create shpfiles from data from http://openflights.org/data.html
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

puts \n[file tail [info script]]

set Log::Param(Level) DEBUG

Log::Start [info script] 0.1

projection create PROJ

Log::Print INFO "Processing airports"

#----- Process airports (Ex: 1,"Goroka","Goroka","Papua New Guinea","GKA","AYGA",-6.081689,145.391881,5282,10,"U")

catch { file delete airports.shp airports.dbf airports.shx }
ogrfile open FILEAIRPORT write airports.shp "ESRI Shapefile"

#----- Creation du layer et des champs

ogrlayer create FILEAIRPORT AIRPORT "Airports"
ogrlayer define AIRPORT -field ID String
ogrlayer define AIRPORT -field NAME String
ogrlayer define AIRPORT -field COUNTRY String
ogrlayer define AIRPORT -field CODE String
ogrlayer define AIRPORT -field ICAO String
ogrlayer define AIRPORT -field TZ Real
ogrlayer define AIRPORT -field DST String

ogrgeometry create POINT "3D Point"

set nb 0
set f [open airports.dat]

while { ![eof $f] } {
   gets $f line

   set info [split $line ,]

   ogrlayer define AIRPORT -nb [incr nb]
   set no [expr $nb-1]

   Log::Print DEBUG "Processing airport [string trim [lindex $info 1] \"]"

   ogrlayer define AIRPORT -feature $no ID      [string trim [lindex $info 0] \"]
   ogrlayer define AIRPORT -feature $no NAME    [string trim [lindex $info 1] \"]
   ogrlayer define AIRPORT -feature $no COUNTRY [string trim [lindex $info 3] \"]
   ogrlayer define AIRPORT -feature $no CODE    [string trim [lindex $info 4] \"]
   ogrlayer define AIRPORT -feature $no ICAO    [string trim [lindex $info 5] \"]
   ogrlayer define AIRPORT -feature $no TZ      [string trim [lindex $info 9] \"]
   ogrlayer define AIRPORT -feature $no DST     [string trim [lindex $info 10] \"]

   #---- If no height specified, use 0
   set z [lindex $info 8]
   if { $z=="" } {set z 0 }

   ogrgeometry define POINT -points {}
   ogrgeometry define POINT -addpoint [lindex $info 7] [lindex $info 6] [expr $z*0.3048]

   ogrlayer define AIRPORT -geometry $no False POINT
}

Log::Print INFO "Processing routes"

#----- Process routes (Ex: 2B,410,CEK,2968,DME,4029,,0,CR2)

catch { file delete airroutes.shp airroutes.dbf airroutes.shx }
ogrfile open FILEROUTES write airroutes.shp "ESRI Shapefile"

#----- Creation du layer et des champs

ogrlayer create FILEROUTES ROUTE "Airports"
ogrlayer define ROUTE -field LINE String
ogrlayer define ROUTE -field LINE_ID String
ogrlayer define ROUTE -field SOURCE String
ogrlayer define ROUTE -field SOURCE_ID String
ogrlayer define ROUTE -field DEST String
ogrlayer define ROUTE -field DEST_ID String
ogrlayer define ROUTE -field CODE String
ogrlayer define ROUTE -field STOPS String
ogrlayer define ROUTE -field PLANE String

ogrgeometry create LINE "3D Line String"

set nb 0
set f [open routes.dat]

while { ![eof $f] } {
   gets $f line

   set info [split $line ,]

   #----- Find source and destination airport
   set airport_src [ogrlayer define AIRPORT -featureselect [list [list ID == [string trim [lindex $info 3] \"]]]]
   set airport_dst [ogrlayer define AIRPORT -featureselect [list [list ID == [string trim [lindex $info 5] \"]]]]

   if { [llength $airport_src] && [llength $airport_dst] } {

      Log::Print DEBUG "Processing route from  $airport_src to $airport_dst"

      ogrlayer define ROUTE -nb [incr nb]
      set no [expr $nb-1]

      ogrlayer define ROUTE -feature $no LINE      [string trim [lindex $info 0] \"]
      ogrlayer define ROUTE -feature $no LINE_ID   [string trim [lindex $info 1] \"]
      ogrlayer define ROUTE -feature $no SOURCE    [string trim [lindex $info 2] \"]
      ogrlayer define ROUTE -feature $no SOURCE_ID [string trim [lindex $info 3] \"]
      ogrlayer define ROUTE -feature $no DEST      [string trim [lindex $info 4] \"]
      ogrlayer define ROUTE -feature $no DEST_ID   [string trim [lindex $info 5] \"]
      ogrlayer define ROUTE -feature $no CODE      [string trim [lindex $info 6] \"]
      ogrlayer define ROUTE -feature $no STOPS     [string trim [lindex $info 7] \"]
      ogrlayer define ROUTE -feature $no PLANE     [string trim [lindex $info 8] \"]

      #----- Get source and destination coordinates
      set coord_src [ogrgeometry define [ogrlayer define AIRPORT -geometry $airport_src] -points]
      set coord_dst [ogrgeometry define [ogrlayer define AIRPORT -geometry $airport_dst] -points]

      #----- Project a great circel between the two at 10km sampling resolution
      ogrgeometry define LINE -points {}
      foreach { la lo } [projection  function PROJ -path [list [lindex $coord_src 1] [lindex $coord_src 0] [lindex $coord_dst 1] [lindex $coord_dst 0]] 10000] {
         ogrgeometry define LINE -addpoint $lo $la 0.0
      }
      ogrlayer define ROUTE -geometry $no False LINE
   }
}

ogrfile close FILEROUTES
ogrfile close FILEAIRPORT

Log::End