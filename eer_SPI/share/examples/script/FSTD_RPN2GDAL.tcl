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

   set Param(Version) 1.1

   set Param(Format)    "KMZ"
   set Param(Files)     {}
   set Param(TypVar)    ""
   set Param(IP1)       -1
   set Param(IP2)       -1
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
   set Param(Out)       ./export_%n_%d-%t
   set Param(Mode)      RGBA
   set Param(Interp)    LINEAR
   set Param(Res)       0.1
   set Param(BBox)      {}
   set Param(Lat0)      -999
   set Param(Lon0)      -999
   set Param(Lat1)      -999
   set Param(Lon1)      -999

   set Param(CommandInfo) "   Export RPN fields into GDAL raster files as values, rgb colors or index colors."
   
   set Param(CommandLine) "   Command line otions:\n
\t-format      : Output format (${APP_COLOR_GREEN}\"$Param(Format)\"${APP_COLOR_RESET})
\t-nodata      : No data value when out of domain (${APP_COLOR_GREEN}$Param(NoData)${APP_COLOR_RESET})
\t-mode        : Image data (INDEX,${APP_COLOR_GREEN}RGBA${APP_COLOR_RESET},DATA)
\t-map         : Colormap (${APP_COLOR_GREEN}$Param(Map)${APP_COLOR_RESET})
\t-bbox        : Bounding box
\t-res         : Image resolution in degrees (${APP_COLOR_GREEN}$Param(Res)${APP_COLOR_RESET})
\t-var         : List of variables to process (Mandatory)
\t-factor      : List of factors per variables
\t-inter       : List of intervals
\t-interp      : Data interpolation type (NEAREST,${APP_COLOR_GREEN}LINEAR${APP_COLOR_RESET})
\t-min         : Minimum value
\t-max         : Maximum value
\t-fstd        : List of RPN files to process (Mandatory)
\t-typvar      : TYPVAR to use (${APP_COLOR_GREEN}\"$Param(TypVar)\"${APP_COLOR_RESET})
\t-ip1         : IP1 to use (${APP_COLOR_GREEN}$Param(IP1)${APP_COLOR_RESET})
\t-ip2         : IP2 to use (${APP_COLOR_GREEN}$Param(IP2)${APP_COLOR_RESET})
\t-ip3         : IP3 to use (${APP_COLOR_GREEN}$Param(IP3)${APP_COLOR_RESET})
\t-etiket      : Etiket to use (${APP_COLOR_GREEN}\"$Param(Etiket)\"${APP_COLOR_RESET})
\t-out         : Output file (${APP_COLOR_GREEN}$Param(Out)${APP_COLOR_RESET}). 
\t                  Wildcards : %n nomvar, %l level, %h level type, %e etiket, %d date, %t time, %1 ip1, %2 ip2, %3 ip3
      
  Information parameters:\n
\t-help        : This information
\t-version     : Version
\t-verbose     : Trace level (ERROR,WARNING,${APP_COLOR_GREEN}INFO${APP_COLOR_RESET},DEBUG,EXTRA,0-4)
\t-verbosecolor: Use color for log messages

   Available formats: \n\n\t[lmap f $Export::Raster::Param(Formats) {lindex $f end-1}]"
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

   fstdfield ip1mode NEW
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

            foreach field [lindex [fstdfield find FILEIN [fstdstamp fromseconds $datev] $Param(Etiket) "$Param(IP1)" "$Param(IP2)" "$Param(IP3)" $Param(TypVar) $var] 0] {
               fstdfield read DATA$n FILEIN $field

               if { [llength $Param(BBox)]!=4 } {
                  set ll [georef limit [fstdfield define DATA$n -georef]]
                  set Param(Lat0) [lindex $ll 0]
                  set Param(Lon0) [lindex $ll 1]
                  set Param(Lat1) [lindex $ll 2]
                  set Param(Lon1) [lindex $ll 3]
               }

               fstdfield configure DATA$n -interpdegree $Param(Interp) -desc ${var} -rendertexture 1 -colormap MAP -min $Param(Min) -max $Param(Max) -intervals $Param(Intervals)
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
      fstdfile close FILEIN
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
      Log::Print MUST "$Param(CommandInfo)\n\n$Param(CommandLine)"
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
         "bbox"     { set i [Args::Parse $gargv $gargc $i LIST  RPN2GDAL::Param(BBox)] }
         "var"      { set i [Args::Parse $gargv $gargc $i LIST  RPN2GDAL::Param(Vars)] }
         "factor"   { set i [Args::Parse $gargv $gargc $i LIST  RPN2GDAL::Param(Factors)] }
         "inter"    { set i [Args::Parse $gargv $gargc $i LIST  RPN2GDAL::Param(Intervals)] }
         "interp"   { set i [Args::Parse $gargv $gargc $i VALUE RPN2GDAL::Param(Interp)] }
         "min"      { set i [Args::Parse $gargv $gargc $i VALUE RPN2GDAL::Param(Min)] }
         "max"      { set i [Args::Parse $gargv $gargc $i VALUE RPN2GDAL::Param(Max)] }
         "fstd"     { set i [Args::Parse $gargv $gargc $i LIST  RPN2GDAL::Param(Files)] }
         "out"      { set i [Args::Parse $gargv $gargc $i VALUE RPN2GDAL::Param(Out)] }
         "typvar"   { set i [Args::Parse $gargv $gargc $i VALUE RPN2GDAL::Param(TypVar)] }
         "ip1"      { set i [Args::Parse $gargv $gargc $i VALUE RPN2GDAL::Param(IP1)] }
         "ip2"      { set i [Args::Parse $gargv $gargc $i VALUE RPN2GDAL::Param(IP2)] }
         "ip3"      { set i [Args::Parse $gargv $gargc $i VALUE RPN2GDAL::Param(IP3)] }
         "etiket"   { set i [Args::Parse $gargv $gargc $i VALUE RPN2GDAL::Param(Etiket)] }

         "verbose"       { set i [Args::Parse $argv $argc $i VALUE         Log::Param(Level)] }
         "verbosecolor"  { set i [Args::Parse $argv $argc $i FLAG          Log::Param(Color)] }
         "help"          { Log::Print MUST "$Param(CommandInfo)\n\n$Param(CommandLine)"; Log::End 0 }
         "version"       { Log::Print MUST $Param(Version); Log::End 0 }
         default         { Log::Print ERROR "Invalid argument [lindex $gargv $i]\n\n$Param(CommandLine)"; Log::End 1 }
      }
   }
}

Log::Start RPN2GDAL $RPN2GDAL::Param(Version)

RPN2GDAL::ParseCommandLine
RPN2GDAL::Run

Log::End