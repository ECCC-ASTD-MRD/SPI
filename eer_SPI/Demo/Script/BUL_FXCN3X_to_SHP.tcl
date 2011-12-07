#!/bin/sh
# the next line restarts using tclsh \
exec $SPI_PATH/tclsh "$0" "$@"
#============================================================================
# Environnement Canada
# Centre Meteorologique Canadien
# 2100 Trans-Canadienne
# Dorval, Quebec
#
# Projet     : Convert FXCN3X bulletin to GIS vector file (Shapefile).
# Fichier    : BUL_FXCN3X_to_SHP.tcl
# Creation   : Decembre 2011 - J.P. Gauthier - CMC/CMOE
# Description: Process an FXCN3X bulletin to produce 4 shapefiles
#                [name].pts.* Hurricane position forecast (point)
#                [name].lin.* Hurricane track forecast    (line)
#                [name].rad.* Wind radii forecast         (polygon)
#                [name].err.* Uncertainty error cone      (polygon)
#
# Parametres  :
#    <fxcn3x> : Bulletin file
#
# Funcitons:
#   Bulletin::FXCN3X::Read         { File }
#   Bulletin::FXCN3X::Process      { }
#   Bulletin::FXCN3X::ProcessRadii { Lat Lon Valid Force NE SE SW NW }
#
# Retour:
#
# Remarques  :
#   - This script uses CMC EER's SPI API's hence the environment variable SPI_PATH
#     has to be defined to the location of SPI. It has been tested with SPI 7.5.0
#   - This is largely based on the bash script createMappableProduct from
#     Kaveh Afshar and Thomas Gibson
#============================================================================

package require TclData
package require Logger

namespace eval Bulletin::FXCN3X {
   variable Data
   variable Param

   #----- General parameters
   set Param(Job)     [info script]   ;#Job name
   set Param(Version) 0.2             ;#Job version

   set Param(UncertaintyLUT) { 0.0 28.0 44.0 59.0 78.0 117.0 206.0 }    ;#Uncertainty lookup table
   set Param(ForecastHours)  { 0   12   24   36   48   72    120   }    ;#Forecats hours related to uncertainty
   set Param(Types)          { "TROPICAL DEPRESSION" "TROPICAL STORM" "SUBTROPICAL STORM" "POST-TROPICAL STORM" "HURRICANE" }
   set Param(Format)         "ESRI Shapefile"

   #----- Internal data extracted from bulletin
   set Data(Filename) ""  ;# Output filename
   set Data(Name)     ""  ;# Storm name
   set Data(Type)     ""  ;# Storm type
   set Data(TZ)       ""  ;# Time zone
   set Data(Issued)   ""  ;# Issued time in seconds UTC
   set Data(Track)    {}  ;# List of track points
   set Data(Radii)    {}  ;# List of radii info

   #----- Create cylindrical (latlon georeferential)
   georef create LATLONREF EPSG:4326

   #----- Create projection used for geo calculus
   projection create PROJ

   #----- Theses are temporary geometry containers
   ogrgeometry create POINT "Point"
   ogrgeometry create POLY  "Polygon"
   ogrgeometry create RING  "Linear Ring"
   ogrgeometry create LINE  "Line String"
}

#----------------------------------------------------------------------------
# Name     : <Bulletin::FXCN3X::Read>
# Creation : Decembre 2011 - J.P. Gauthier - CMC/CMOE
#
# Goal     : Read an FXCN3X message and extract it's relevant parts.
#
# Parameters :
#   <File>   : Bulletin file
#
# Return:
#
# Remarks :
#
#----------------------------------------------------------------------------
proc Bulletin::FXCN3X::Read { File } {
   variable Data
   variable Param

   Log::Print INFO "Reading bulletin $File"

   set f [open $File r]

   while { ![eof $f] } {
      gets $f line

      #----- Switch on first word of a line
      switch [lindex $line 0] {

         "FXCN31" { ;#----- Process header
            while {  [gets $f line]>0 } {
               append issued " $line"
            }
            #------ Some bulletin have : instead of . to separate hours from minutes
            set issued         [string map { : . } [lrange $issued end-6 end]]
            set Data(TZ)       [lindex $issued 2]
            set Data(TZL)      [::tcl::clock::ConvertLegacyTimeZone [string tolower $Data(TZ)]]
            set Data(Issued)   [clock scan $issued -format "%l.%M %p %Z %A %d %B %Y." -timezone :UTC]
         }

         "1." {  ;#----- Process section 1
            gets $f line

            while { [gets $f line]>0 } {

               #----- Get name
               if { $Data(Name)=="" && [set idx [lsearch -exact $line "WAS"]]!=-1 } {
                  set Data(Name) [lindex $line [expr $idx-1]]
               }
               foreach type $Param(Types) {
                  if { [string match *$type* $line] } {
                     set Data(Type) $type
                     break;
                  }
               }
               if { $Data(Type)!="" } {
                  break
               }
            }
         }

         "2." {  ;#----- Process section 2
            gets $f line
            gets $f line
            gets $f line
            while { [gets $f line]>0 } {
               lappend Data(Track) $line
            }
         }

         "C." {  ;#----- Process section C
            gets $f line
            gets $f line
            gets $f line
            while {  [gets $f line]>0 } {
               lappend Data(Radii) $line
            }
         }
      }
   }
   close $f

   #----- check for file validity (if it has FXCN31 in header)
   if { $Data(Issued)=="" } {
      Log::Print ERROR "Invalid file, no FXCN31 header found."
      Log::End 1
   }

   #----- Check storm name
   if { $Data(Name)=="" } {
      Log::Print ERROR "Could not find storm name."
      Log::End 1
   }

   #----- Figure output file name
   set Data(Filename) [clock format $Data(Issued) -format "%Y%m%d_%H%MZ" -timezone :UTC]_$Data(Name)
   file delete -force $Data(Filename)
   Log::Print INFO "Ouput filename is $Data(Filename)"
}

#----------------------------------------------------------------------------
# Name     : <Bulletin::FXCN3X::Process>
# Creation : Decembre 2011 - J.P. Gauthier - CMC/CMOE
#
# Goal     : Process the bulletin to produce the various shapefiles
#
# Parameters :
#
# Return:
#
# Remarks :
#
#----------------------------------------------------------------------------
proc Bulletin::FXCN3X::Process { } {
   variable Data
   variable Param

   Log::Print INFO "Processing track information"

   if { ![llength $Data(Track)] } {
      Log::Print ERROR "This FXCN3X file does not appear to have a forecast position table"
      Log::End 1
   }

   ogrfile open FILE write $Data(Filename) $Param(Format)

   #----- Creation of track points layer and fields
   ogrlayer create FILE POINTS "$Data(Filename).pts" LATLONREF

   ogrlayer define POINTS -field STORMNAME  String
   ogrlayer define POINTS -field STORMTYPE  Integer
   ogrlayer define POINTS -field BASIN      String
   ogrlayer define POINTS -field ADVDATE    String
   ogrlayer define POINTS -field STORMFORCE Integer
   ogrlayer define POINTS -field LAT        Real
   ogrlayer define POINTS -field LON        Real
   ogrlayer define POINTS -field VALIDTIME  String
   ogrlayer define POINTS -field TAU        Integer
   ogrlayer define POINTS -field MAXWIND    Integer
   ogrlayer define POINTS -field MSLP       Real
   ogrlayer define POINTS -field TCDVLP     String
   ogrlayer define POINTS -field DATELBL    String
   ogrlayer define POINTS -field TIMEZONE   String
   ogrlayer define POINTS -field ERRCT      Real
   ogrlayer define POINTS -field R34NE      Integer
   ogrlayer define POINTS -field R34SE      Integer
   ogrlayer define POINTS -field R34SW      Integer
   ogrlayer define POINTS -field R34NW      Integer
   ogrlayer define POINTS -field R48NE      Integer
   ogrlayer define POINTS -field R48SE      Integer
   ogrlayer define POINTS -field R48SW      Integer
   ogrlayer define POINTS -field R48NW      Integer
   ogrlayer define POINTS -field R64NE      Integer
   ogrlayer define POINTS -field R64SE      Integer
   ogrlayer define POINTS -field R64SW      Integer
   ogrlayer define POINTS -field R64NW      Integer

   #----- Creation of track line layer and fields
   ogrlayer create FILE LINE "$Data(Filename).lin" LATLONREF
   ogrlayer define LINE -field STORMNAME  String
   ogrlayer define LINE -field STORMTYPE  Integer
   ogrlayer define LINE -field BASIN      String

   #----- Creation of radii layer and fields
   if { [llength $Data(Radii)] } {
      ogrlayer create FILE RADIIS "$Data(Filename).rad" LATLONREF
      ogrlayer define RADIIS -field WINDFORCE Real
      ogrlayer define RADIIS -field VALIDTIME String
   }

   #----- Creation of the uncertainty cone layer and fields
   ogrlayer create FILE UNCERT "$Data(Filename).err" LATLONREF

   #----- Loop on the track points
   set nb 0
   set t0 -1
   foreach point $Data(Track) {

      set type ""

      #----- Extract line values
      scan $point "%s %02d %f %s %f%1s %f%1s %i %i %i %s" month date time apm lat latd lon lond mslp kts kmh type

      #----- Make necessary conversions
      set time  [expr ("$apm"=="PM")?$time+12:$time]
      set sec   [clock scan "$month $date $time" -format "%b %d %H.%M" -timezone $Data(TZL)]
      set t0    [expr $t0==-1?$sec:$t0]
      set valid [clock format $sec -format "%d/%H%M" -timezone :UTC]
      set tau   [expr ($sec-$t0)/3600]

      set lat [expr ("$latd"=="S")?-$lat:$lat]
      set lon [expr ("$lond"=="W")?-$lon:$lon]
      lappend track $lon $lat

      #----- Determine issue date
      set advdate [clock format $Data(Issued) -format "%y%m%d/%H%M" -timezone :UTC]

      #----- Determine type and scale number
      if { $kts>=136 } {
         set tcdvlp "HU"
         set force 5
      } elseif { $kts>=114 } {
         set tcdvlp "HU"
         set force 4
      } elseif { $kts>=96 } {
         set tcdvlp "HU"
         set force 3
      } elseif { $kts>=83 } {
         set tcdvlp "HU"
         set force 2
      } elseif { $kts>=65 } {
         set tcdvlp "HU"
         set force 1
      } elseif { $kts>=35 } {
         set tcdvlp "TSD"
         set force 0
      } else {
         set tcdvlp "TD"
         set force 0
      }

      #----- Determine uncertainty error
      set pu 0
      set pf 0
      foreach f $Param(ForecastHours) u $Param(UncertaintyLUT) {
         set errct $u

         if { $tau<=$f } {
            #----- interpolate between LUT values
            if { $tau<$f } {
               set errct [expr $pu+(($tau-$pf)*($u-$pu))/($f-$pf)]
            }
            break
         }
         set pu $u
         set pf $f
      }

      #----- Calculate track error cone
      if { $errct>0.0 } {
         set bearing [projection function PROJ -bearing $plat $plon $lat $lon]
         set dist    [expr $errct*1852.0]
         set ll0     [projection function PROJ -circle $lat $lon $dist [expr fmod(-$bearing+90.0,360)]]
         set ll1     [projection function PROJ -circle $lat $lon $dist [expr fmod(-$bearing-90.0,360)]]
         set cone0  "$cone0 [lindex $ll0 1] [lindex $ll0 0]"
         set cone1  "[lindex $ll1 1] [lindex $ll1 0] $cone1"
      } else {
         set cone0 "$lon $lat"
         set cone1 "$lon $lat"
      }
      set plat $lat
      set plon $lon

      Log::Print DEBUG "   Processing location $lat,$lon on [clock format $sec]"

      ogrgeometry define POINT -points { }
      ogrgeometry define POINT -addpoint $lon $lat

      ogrlayer define POINTS -nb [incr nb]
      set no [expr $nb-1]
      ogrlayer define POINTS -feature $no STORMNAME  $Data(Name)
      ogrlayer define POINTS -feature $no STORMTYPE  $Data(Type)
      ogrlayer define POINTS -feature $no BASIN      al

      ogrlayer define POINTS -feature $no STORMFORCE $force
      ogrlayer define POINTS -feature $no ADVDATE    [clock format $Data(Issued) -format "%y%m%d/%H%M" -timezone :UTC]
      ogrlayer define POINTS -feature $no LAT        $lat
      ogrlayer define POINTS -feature $no LON        $lon
      ogrlayer define POINTS -feature $no VALIDTIME  $valid
      ogrlayer define POINTS -feature $no TAU        $tau
      ogrlayer define POINTS -feature $no MAXWIND    $kts
      ogrlayer define POINTS -feature $no MSLP       $mslp
      ogrlayer define POINTS -feature $no TCDVLP     $tcdvlp
      ogrlayer define POINTS -feature $no DATELBL    [clock format $sec -format "%l:%M %p %a" -timezone $Data(TZL)]
      ogrlayer define POINTS -feature $no TIMEZONE   [string toupper $Data(TZ)]
      ogrlayer define POINTS -feature $no ERRCT      $errct

      #----- Process wind radii info from table
      if { [llength $Data(Radii)] } {

         set radii [lindex $Data(Radii) $no]

         ogrlayer define POINTS -feature $no R34NE [lindex $radii 1]
         ogrlayer define POINTS -feature $no R34SE [lindex $radii 2]
         ogrlayer define POINTS -feature $no R34SW [lindex $radii 3]
         ogrlayer define POINTS -feature $no R34NW [lindex $radii 4]
         ogrlayer define POINTS -feature $no R48NE [lindex $radii 5]
         ogrlayer define POINTS -feature $no R48SE [lindex $radii 6]
         ogrlayer define POINTS -feature $no R48SW [lindex $radii 7]
         ogrlayer define POINTS -feature $no R48NW [lindex $radii 8]
         ogrlayer define POINTS -feature $no R64NE [lindex $radii 9]
         ogrlayer define POINTS -feature $no R64SE [lindex $radii 10]
         ogrlayer define POINTS -feature $no R64SW [lindex $radii 11]
         ogrlayer define POINTS -feature $no R64NW [lindex $radii 12]

         ogrlayer define POINTS -geometry $no False POINT

         Bulletin::FXCN3X::ProcessRadii $lat $lon $valid 34 [lindex $radii 1] [lindex $radii 2]  [lindex $radii 3]  [lindex $radii 4]
         Bulletin::FXCN3X::ProcessRadii $lat $lon $valid 48 [lindex $radii 5] [lindex $radii 6]  [lindex $radii 7]  [lindex $radii 8]
         Bulletin::FXCN3X::ProcessRadii $lat $lon $valid 64 [lindex $radii 9] [lindex $radii 10] [lindex $radii 11] [lindex $radii 12]
      }
   }

   #----- Finalize track line
   ogrgeometry define LINE -points $track
   ogrlayer define LINE -nb 1
   ogrlayer define LINE -feature 0 STORMNAME  $Data(Name)
   ogrlayer define LINE -feature 0 STORMTYPE  $Data(Type)
   ogrlayer define LINE -feature 0 BASIN      al
   ogrlayer define LINE -geometry 0 False LINE

   #----- Finalize uncertainty cone
   set a [expr int(fmod(-$bearing+90.0,360))]
   for { set b 10 } { $b<=170 } { incr b 10 } {
      set ll [projection function PROJ -circle $lat $lon $dist [expr $a-$b]]
      append cone0 " [lindex $ll 1] [lindex $ll 0]"
   }

   ogrgeometry define RING -points "$cone0 $cone1"
   ogrgeometry define POLY -geometry False RING
   ogrlayer define UNCERT -nb 1
   ogrlayer define UNCERT -geometry 0 False POLY

   #----- Close files to make sure OGR's memory cache is flushed to files
   ogrfile close FILE

   #----- Zip an clean
   Log::Print INFO "Creating archive"

   exec zip -r $Data(Filename).zip $Data(Filename)
   file delete -force $Data(Filename)
}

#----------------------------------------------------------------------------
# Name     : <Bulletin::FXCN3X::ProcessRadii>
# Creation : Decembre 2011 - J.P. Gauthier - CMC/CMOE
#
# Goal     : Calculate the wind radii polygon coverage from the buletin
#            radii table
#
# Parameters :
#   <Lat>    : Latitude of forecast point
#   <Lon>    : Longitude of forecast point
#   <Valid>  : Time opf validity
#   <Force>  : Wind force for this radii
#   <NE>     : North east quadrant radius in nm
#   <SE>     : South east quadrant radius in nm
#   <SW>     : South west quadrant radius in nm
#   <NW>     : North west quadrant radius in nm
#
# Return:
#
# Remarks :
#
#----------------------------------------------------------------------------
proc Bulletin::FXCN3X::ProcessRadii { Lat Lon Valid Force NE SE SW NW } {
   variable Data
   variable Param

   #----- Calculate wind circles
   ogrgeometry define RING -points {}

   set t 0
   foreach d { NE SE SW NW } a { 0 90 180 270 } {

      #----- Convert nautical miles to meters
      eval set r \[expr \$$d*1852\]
      incr t $r

      if { $r==0 } {
         ogrgeometry define RING -addpoint $Lon $Lat
      } else {
         #----- Calculate circle every 10 degree
         for { set b $a } { $b<=[expr $a+90] } { incr b 10 } {
            set ll [projection function PROJ -circle $Lat $Lon $r $b]
            ogrgeometry define RING -addpoint [lindex $ll 1] [lindex $ll 0]
         }
      }
   }

   #----- If there is wind radii
   if { $t } {
      #----- Assign ring to polygon and force closing of it
      ogrgeometry define POLY -geometry False RING
      ogrgeometry stats POLY -close

      #----- Add polygon feature to layer
      set nb [ogrlayer define RADIIS -nb]
      ogrlayer define RADIIS -nb [incr nb]
      set no [expr $nb-1]
      ogrlayer define RADIIS -feature $no WINDFORCE $Force
      ogrlayer define RADIIS -feature $no VALIDTIME $Valid

      ogrlayer define RADIIS -geometry $no False POLY
   }
}

#----- This is where it all starts
set Log::Param(Level) DEBUG      ;#Log level
set Log::Param(Time)  False      ;#Print the time
set Log::Param(Proc)  False      ;#Print the calling proc

Log::Start $Bulletin::FXCN3X::Param(Job) $Bulletin::FXCN3X::Param(Version)

#----- Check for number of arguments
if { [llength $argv]==0 } {
   Log::Print ERROR "Invalid number of arguments:\n\tUsage: [info script] \[bulletin file\]"
   Log::End 1
}

#----- Get bulletin filename passed as first parameter
set fxcn3x [lindex $argv 0]

#----- Read and process it
Bulletin::FXCN3X::Read $fxcn3x
Bulletin::FXCN3X::Process

Log::End