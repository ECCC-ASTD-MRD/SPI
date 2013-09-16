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
# Fichier    : OGR_Interp.tcl
# Creation   : Fevrier 2008 - J.P. Gauthier - CMC/CMOE
# Description: Démonstration de base des fonctions RADAR
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

#----- Open the radar file
set scans [radarfile open RADARSITE read DataIn/200803121320~~CONVOL:URP:XFT:RADAR:IRIS]

puts "   Available scans : $scans"

#----- Read the first scan
radarscan read SCAN1 RADARSITE 0

#----- Output some information about the scan
puts "   Radar type    : [radarscan define SCAN1 -TYPE]"
puts "   Date          : [radarscan define SCAN1 -DATE]"
puts "   Site ID       : [radarscan define SCAN1 -SITEID]"
puts "   Site Name     : [radarscan define SCAN1 -SITENAME]"
puts "   Location      : [radarscan define SCAN1 -LOCATION]"
puts "   Product       : [radarscan define SCAN1 -PRODUCT]"
puts "   Scan          : [radarscan define SCAN1 -SCAN]"
puts "   Azimuth (Deg) : [radarscan define SCAN1 -AZIMUTHRESOLUTION]"
puts "   Bin (Km)      : [radarscan define SCAN1 -BINRESOLUTION]"
puts "   Noise         : [radarscan define SCAN1 -NOISE]"
puts "   Filter        : [radarscan define SCAN1 -FILTER]"
puts "   Z Calibration : [radarscan define SCAN1 -ZCAL]"
puts "   Nyquist vel   : [radarscan define SCAN1 -NYQUIST]"
puts "   Sweep angles  : [radarscan define SCAN1 -SWEEPANGLE]"

#----- Set radar location since sometimes (most of it) it is not in the file itself
radarscan define SCAN1 -LOCATION 45.0445 -76.0642 122.0

puts "\n   Limits (value lat lon angle): min=[radarscan stats SCAN1 -min] max=[radarscan stats SCAN1 -max]"

radarscan stats SCAN1 -level 0
puts "\n   Radar sweep angle 0"
puts "   Projected lat lon (azimuth and bin)  : [radarscan stats SCAN1 -coordpoint 45.28 -74.74]"
puts "   UnProjected azimuth and bin (lat lon): [radarscan stats SCAN1 -gridpoint 75.3750167173 107.193774955]"
puts "   value (lat lon)        : [radarscan stats SCAN1 -coordvalue 45.28 -74.74]"
puts "   value (azimuth and bin): [radarscan stats SCAN1 -gridvalue 75.3750167173 107.193774955]"
puts "   height (lat lon)       : [radarscan stats SCAN1 -height 75.3750167173 107.193774955 0]"

radarscan stats SCAN1 -level 12
puts "   \nRadar sweep angle 12"
puts "   Projected lat lon (azimuth and bin)  : [radarscan stats SCAN1 -coordpoint 45.28 -74.74]"
puts "   UnProjected azimuth and bin (lat lon): [radarscan stats SCAN1 -gridpoint 75.3750167173 107.538344429]"
puts "   value (lat lon)        : [radarscan stats SCAN1 -coordvalue 45.28 -74.74]"
puts "   value (azimuth and bin): [radarscan stats SCAN1 -gridvalue 75.3750167173 107.538344429]"
puts "   height (lat lon)       : [radarscan stats SCAN1 -height 75.3750167173 107.193774955 12]"

Log::End