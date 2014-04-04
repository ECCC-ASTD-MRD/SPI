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
# Fichier    : FSTD_RPN2GDAL.tcl
# Creation   : Mars 2011 - J.P. Gauthier - CMC/CMOE
# Description: Exporter des champs en format raster GDAL
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
package require Export
package require Logger

namespace eval RPN2GDAL { } {
   global env
   variable Param

   set Param(Version) 1.0

   set Param(Format)    "KMZ"
   set Param(Files)     {}
   set Param(IP1)       -1
   set Param(IP3)       -1
   set Param(Etiket)    ""
   set Param(ProjFile)  ""
   set Param(Intervals) {}
   set Param(Min)       0
   set Param(Max)       0
   set Param(Map)       "$env(HOME)/.spi/Colormap/REC_Col.std1.rgba"
   set Param(Vars)      {}
   set Param(Factors)   {}
   set Param(NoData)    0.0
   set Param(Out)       ./out
   set Param(Mode)      RGBA
   set Param(Res)       0.1
   set Param(BBox)      {}
   set Param(Lat0)      -999
   set Param(Lon0)      -999
   set Param(Lat1)      -999
   set Param(Lon1)      -999

   set Param(CommandLine) "   Command line otions are:\n
      -format : Output format (Default: \"$Param(Format)\")
      -nodata : No data value when out of domain (Default: \"$Param(NoData)\")
      -mode   : Image data ($Export::Raster::Param(Modes)) (Default: \"$Param(Mode)\")
      -map    : Colormap (Default: $Param(Map))
      -bbox   : Bounding box (Default: $Param(BBox))
      -res    : Image resolution in degrees (Default: $Param(Res))
      -var    : List of variables to process (Mandatory)
      -factor : List of factors per variables (Default:{})
      -inter  : List of intervals (Default: None)
      -min    : Minimum value (Default: None)
      -max    : Maximum value (Default: None)
      -fstd   : List of RPN files to process (Mandatory)
      -ip1    : IP1 to use (Default: $Param(IP1))
      -ip3    : IP3 to use (Default: $Param(IP3))
      -etiket : Etiket to use (Default: $Param(Etiket))
      -out    : Output directory (Default: $Param(Out))
      -help   : This information
      
      Available formats: \n\tKMZ [gdalfile format]"
}

proc RPN2GDAL::Run { } {
   variable Param

   set n 0
   set fields {}

   if { [llength $Param(BBox)]==4 } {
      set Param(Lat0) [lindex $Param(BBox) 0]
      set Param(Lon0) [lindex $Param(BBox) 1]
      set Param(Lat1) [lindex $Param(BBox) 2]
      set Param(Lon1) [lindex $Param(BBox) 3]
   }

   colormap create MAP -file $Param(Map)

   foreach file $Param(Files) {
      Log::Print INFO "Processign file $file"

      fstdfile open FILEIN read $file
      set path [pwd]

      foreach datev [fstdfile info FILEIN DATEV] {

         Log::Print DEBUG "   Found date [clock format $datev -format "%Y%m%d_%H%M" -gmt True]"

         set v 0
         foreach var $Param(Vars) {

            Log::Print DEBUG "   Checking for variable $var"

            foreach field [lindex [fstdfield find FILEIN [fstdstamp fromseconds $datev] $Param(Etiket) $Param(IP1) -1 $Param(IP3) "" $var] 0] {
               fstdfield read DATA$n FILEIN $field

               if { [llength $Param(BBox)]!=4 } {
                  set ll [georef limit [fstdfield define DATA$n -georef]]
                  set Param(Lat0) [lindex $ll 0]
                  set Param(Lon0) [lindex $ll 1]
                  set Param(Lat1) [lindex $ll 2]
                  set Param(Lon1) [lindex $ll 3]
               }

               fstdfield configure DATA$n -desc ${var} -rendertexture 1 -colormap MAP -min $Param(Min) -max $Param(Max) -intervals $Param(Intervals)
               fstdfield stats DATA$n -nodata $Param(NoData)
               
               if { [llength $Param(Factors)] } {
                  fstdfield configure DATA$n -factor [lindex $Param(Factors) $v]
               }

               lappend fields DATA$n
               incr n
               incr v
            }
         }
      }
   }
   
   if { [llength $fields]>0 } {
      Log::Print INFO "Exporting [llength $fields] field(s)"

      Export::Raster::Init $Param(Res) $Param(Lat0) $Param(Lon0) $Param(Lat1) $Param(Lon1)
      Export::Raster::Export $Param(Out) $Param(Format) $Param(Mode) $fields

      eval fstdfield free $fields
   } else {
      Log::Print ERROR "No field(s) found"
   }   
}

proc RPN2GDAL::ParseCommandLine { } {
   variable Param

   upvar argc gargc
   upvar argv gargv

   if { !$gargc } {
      Log::Print MUST "$Param(CommandLine)"
      Log::End 0
   }

   #----- Parse arguments
   for { set i 0 } { $i < $gargc } { incr i } {
      switch -exact [string trimleft [lindex $gargv $i] "-"] {
         "format"   { set i [Args::Parse $gargv $gargc $i VALUE RPN2GDAL::Param(Format)] }
         "nodata"   { set i [Args::Parse $gargv $gargc $i VALUE RPN2GDAL::Param(NoData)] }
         "mode"     { set i [Args::Parse $gargv $gargc $i VALUE RPN2GDAL::Param(Mode) $Export::Raster::Param(Modes)] }
         "map"      { set i [Args::Parse $gargv $gargc $i VALUE RPN2GDAL::Param(Map)] }
         "res"      { set i [Args::Parse $gargv $gargc $i VALUE RPN2GDAL::Param(Res)] }
         "bbox"     { set i [Args::Parse $gargv $gargc $i LIST RPN2GDAL::Param(BBox)] }
         "var"      { set i [Args::Parse $gargv $gargc $i LIST RPN2GDAL::Param(Vars)] }
         "factor"   { set i [Args::Parse $gargv $gargc $i LIST RPN2GDAL::Param(Factors)] }
         "inter"    { set i [Args::Parse $gargv $gargc $i LIST RPN2GDAL::Param(Intervals)] }
         "min"      { set i [Args::Parse $gargv $gargc $i VALUE RPN2GDAL::Param(Min)] }
         "max"      { set i [Args::Parse $gargv $gargc $i VALUE RPN2GDAL::Param(Max)] }
         "fstd"     { set i [Args::Parse $gargv $gargc $i LIST RPN2GDAL::Param(Files)] }
         "out"      { set i [Args::Parse $gargv $gargc $i VALUE RPN2GDAL::Param(Out)] }
         "ip1"      { set i [Args::Parse $gargv $gargc $i VALUE RPN2GDAL::Param(IP1)] }
         "ip3"      { set i [Args::Parse $gargv $gargc $i VALUE RPN2GDAL::Param(IP3)] }
         "etiket"   { set i [Args::Parse $gargv $gargc $i VALUE RPN2GDAL::Param(Etiket)] }

         "help"      { Log::Print MUST "$Param(CommandLine)"; Log::End 0 }
         default     { Log::Print ERROR "Invalid argument [lindex $gargv $i]\n\n$Param(CommandLine)"; Log::End 1 }
      }
   }
}

set Log::Param(Level) INFO       ;#Log level
set Log::Param(Time)  False      ;#Print the time
set Log::Param(Proc)  False      ;#Print the calling proc

Log::Start RPN2GDAL $RPN2GDAL::Param(Version)

RPN2GDAL::ParseCommandLine
RPN2GDAL::Run

Log::End