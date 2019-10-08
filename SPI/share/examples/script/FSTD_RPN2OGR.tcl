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
# Fichier    : FSTD_RPN2OGR.tcl
# Creation   : Mars 2011 - J.P. Gauthier - CMC/CMOE
# Description: Exporter des champs en format vectoriel OGR
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

namespace eval RPN2OGR { } {
   variable Param

   set Param(Version) 1.1

   set Param(Format)    "ESRI Shapefile"
   set Param(Files)     {}
   set Param(Mode)      CELL
   set Param(Map)       "$env(HOME)/.spi/Colormap/REC_Col.std1.rgba"
   set Param(Etiket)    ""
   set Param(TypVar)    ""
   set Param(IP1)       -1
   set Param(IP2)       -1
   set Param(IP3)       -1
   set Param(Zip)       False
   set Param(ProjFile)  ""
   set Param(Intervals) {}
   set Param(Vars)      {}
   set Param(Factors)   {}
   set Param(Options)   {}
   set Param(Out)       ./export_%d-%t

   set Param(CommandInfo) "   Export RPN fields into OGR vectorial files as gridcell, gridpoint or contour."
   
   set Param(CommandLine) "   Command line otions:\n
\t-format      : Output format (${APP_COLOR_GREEN}\"$Param(Format)\"${APP_COLOR_RESET})
\t-var         : List of variables to process (Mandatory)
\t-factor      : List of factors per variables
\t-map         : Colormap (${APP_COLOR_GREEN}$Param(Map)${APP_COLOR_RESET})
\t-mode        : Feature type (POINT ${APP_COLOR_GREEN}CELL${APP_COLOR_RESET} CONTOUR POLYGON)
\t-inter       : List of contour to use
\t-fstd        : List of RPN files to process (Mandatory)
\t-typvar      : TYPVAR to use (${APP_COLOR_GREEN}\"$Param(TypVar)\"${APP_COLOR_RESET})
\t-ip1         : IP1 to use (${APP_COLOR_GREEN}$Param(IP1)${APP_COLOR_RESET})
\t-ip2         : IP2 to use (${APP_COLOR_GREEN}$Param(IP2)${APP_COLOR_RESET})
\t-ip3         : IP3 to use (${APP_COLOR_GREEN}$Param(IP3)${APP_COLOR_RESET})
\t-etiket      : Etiket to use (${APP_COLOR_GREEN}\"$Param(Etiket)\"${APP_COLOR_RESET})
\t-prj         : prj georeference file to use for output file (${APP_COLOR_GREEN}WGS84 latlon${APP_COLOR_RESET})
\t-opt         : Format specific creation options (see: http://www.gdal.org/ogr_formats.html)
\t-zip         : zip results into a single file
\t-out         : Output file (${APP_COLOR_GREEN}$Param(Out)${APP_COLOR_RESET}). 
\t                  Wildcards : %n nomvar, %l level, %h level type, %e etiket, %d date, %t time, %1 ip1, %2 ip2, %3 ip3
      
   Information parameters:\n
\t-help        : This information
\t-version     : Version
\t-verbose     : Trace level (ERROR,WARNING,${APP_COLOR_GREEN}INFO${APP_COLOR_RESET},DEBUG,EXTRA,0-4)
\t-verbosecolor: Use color for log messages

   Available formats: \n\n\t[lmap f $Export::Vector::Param(Formats) {lindex $f end-1}]"
}

proc RPN2OGR::Run { } {
   variable Param

   set n 0
   set outs {}
   set kmlfields {}

   fstdfield ip1mode NEW
   colormap create MAP -file $Param(Map)

   if { [file exists $Param(ProjFile)] } {
#      georef create REF { PROJCS["WGS 84 / UPS North",GEOGCS["WGS 84",DATUM["WGS_1984",SPHEROID["WGS 84",6378137,298.257223563]],PRIMEM["Greenwich",0],UNIT["degree",0.0174532925199433]],PROJECTION["Polar_Stereographic"],PARAMETER["latitude_of_origin",90],PARAMETER["central_meridian",0],PARAMETER["scale_factor",0.994],PARAMETER["false_easting",2000000],PARAMETER["false_northing",2000000],UNIT["metre",1]] }
      georef create REF [exec cat $Param(ProjFile)]
   }
   
   foreach file $Param(Files) {
      Log::Print INFO "Processing file $file"

      fstdfile open FILEIN read $file

      foreach datev [fstdfile info FILEIN DATEV] {

         set fields    {}
         set date [clock format $datev -format "%Y%m%d" -gmt True]
         set time [clock format $datev -format "%H%M" -gmt True]
         Log::Print INFO "   Found date $date $time"

         set v 0
         foreach var $Param(Vars) {
            Log::Print INFO "   Checking for variable $var"
            foreach field [lindex [fstdfield find FILEIN [fstdstamp fromseconds $datev] $Param(Etiket) "$Param(IP1)" "$Param(IP2)" "$Param(IP3)" $Param(TypVar) $var] 0] {

               fstdfield read DATA$n FILEIN $field
               fstdfield configure DATA$n -desc ${var} -colormap MAP -min -1e32 -intervals $Param(Intervals)
               
               if { [llength $Param(Factors)] } {
                  fstdfield configure DATA$n -factor [lindex $Param(Factors) $v]
               }
               
               switch $Param(Mode) {
                  "POINT"   { if { [fstdfield define DATA$n -GRTYP]=="Y" } { fstdfield configure DATA$n -renderparticle 1 } else { fstdfield configure DATA$n -rendergrid 1 } }
                  "CELL"    { fstdfield configure DATA$n -rendertexture 1 }
                  "CONTOUR" { fstdfield configure DATA$n -rendercontour 1 -mapall True -width 2 }
                  "POLYGON" { fstdfield configure DATA$n -rendertexture 1 -rendercontour 1 -mapall True -width 2 }
               }

               lappend fields DATA$n
               lappend kmlfields DATA$n
               incr n
               incr v
            }
         }

         if { [llength $fields] && $Param(Format)!="KMZ" && $Param(Mode)!="CONTOUR" && $Param(Mode)!="POLYGON" } {
            Log::Print INFO "   Exporting [llength $fields] field(s)"

            #----- Create filename
            set name [string map [list %d $date %t $time] $Param(Out)]
            if  { [set nb [llength [glob -nocomplain ${name}*.*]]] } {
                set name $name.[incr nb]
            }

            ogrfile open FILE write $name $Param(Format) 

            if { [georef is REF] } {
               ogrlayer create FILE LAYER ${date}_${time} REF $Param(Options)
            } else {
               ogrlayer create FILE LAYER ${date}_${time} "" $Param(Options)
            }

            ogrlayer import LAYER $fields

            ogrfile close FILE
            ogrlayer free LAYER
            eval fstdfield free $fields

            set outs [concat $outs [glob $name*]]
         }
      }
      fstdfile close FILEIN
   }


   if { [llength $kmlfields] && ($Param(Format)=="KMZ" || $Param(Mode)=="CONTOUR" || $Param(Mode)=="POLYGON") } {
      Export::Vector::Export $Param(Out) $Param(Format) $kmlfields $Param(Options)
      fstdfield free {*}$kmlfields
      if { $Param(Format)!="KMZ" } {
          set outs [glob [string map [list %n * %l * %h * %e * %d * %t * %1 * %2 * %3 *] $Param(Out)]*]
      }
   }
   
   if { $Param(Zip) &&  $Param(Format)!="KMZ" } {
     set name [file dirname $Param(Out)]/[regsub -all {[_.]?%[nlhedt123]} [file tail $Param(Out)] ""]
     Log::Print INFO "Zipping results to $name"
     exec zip -j -r $name.zip {*}$outs
     file delete -force {*}$outs
   }
   
   if { ![llength $kmlfields] } {
      Log::Print ERROR "No field(s) found"
   }
}

proc RPN2OGR::ParseCommandLine { } {
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
         "format"   { set i [Args::Parse $gargv $gargc $i VALUE RPN2OGR::Param(Format)] }
         "var"      { set i [Args::Parse $gargv $gargc $i LIST  RPN2OGR::Param(Vars)] }
         "factor"   { set i [Args::Parse $gargv $gargc $i LIST  RPN2OGR::Param(Factors)] }
         "map"      { set i [Args::Parse $gargv $gargc $i VALUE RPN2OGR::Param(Map)] }
         "mode"     { set i [Args::Parse $gargv $gargc $i VALUE RPN2OGR::Param(Mode) $Export::Vector::Param(Modes)] }
         "inter"    { set i [Args::Parse $gargv $gargc $i LIST  RPN2OGR::Param(Intervals)] }
         "fstd"     { set i [Args::Parse $gargv $gargc $i LIST  RPN2OGR::Param(Files)] }
         "prj"      { set i [Args::Parse $gargv $gargc $i VALUE RPN2OGR::Param(ProjFile)] }
         "opt"      { set i [Args::Parse $gargv $gargc $i LIST  RPN2OGR::Param(Options)] }
         "zip"      { set i [Args::Parse $gargv $gargc $i FLAG  RPN2OGR::Param(Zip)] }
         "out"      { set i [Args::Parse $gargv $gargc $i VALUE RPN2OGR::Param(Out)] }
         "typvar"   { set i [Args::Parse $gargv $gargc $i VALUE RPN2OGR::Param(TypVar)] }
         "ip1"      { set i [Args::Parse $gargv $gargc $i VALUE RPN2OGR::Param(IP1)] }
         "ip2"      { set i [Args::Parse $gargv $gargc $i VALUE RPN2OGR::Param(IP2)] }
         "ip3"      { set i [Args::Parse $gargv $gargc $i VALUE RPN2OGR::Param(IP3)] }
         "etiket"   { set i [Args::Parse $gargv $gargc $i VALUE RPN2OGR::Param(Etiket)] }

         "verbose"       { set i [Args::Parse $argv $argc $i VALUE         Log::Param(Level)] }
         "verbosecolor"  { set i [Args::Parse $argv $argc $i FLAG          Log::Param(Color)] }
         "help"          { Log::Print MUST "$Param(CommandInfo)\n\n$Param(CommandLine)"; Log::End 0 }
         "version"       { Log::Print MUST $Param(Version); Log::End 0 }
         default         { Log::Print ERROR "Invalid argument [lindex $gargv $i]\n\n$Param(CommandLine)"; Log::End 1 }
      }
   }
}

Log::Start RPN2OGR $RPN2OGR::Param(Version)

RPN2OGR::ParseCommandLine
RPN2OGR::Run

Log::End
