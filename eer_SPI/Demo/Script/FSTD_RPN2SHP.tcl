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
# Fichier    : FSTD_RPN2SHP.tcl
# Creation   : Mars 2011 - J.P. Gauthier - CMC/CMOE
# Description: Exporter des champs en shapefile
#
# Parametres :
#
# Retour:
#
# Remarques  :
#
#============================================================================

package require TclData
package require Logger

namespace eval RPN2FSTD { } {
   variable Param

   set Param(Version) 1.0

   set Param(Files)    {}
   set Param(IP1)      -1
   set Param(ProjFile) ""
   set Param(Contours) {}
   set Param(Vars)     {}
   set Param(Out)      .

   set Param(CommandLine) "   -version: Script version
   -var    : List of variables to process (Mandatory)
   -contour: List of coutour to use (Default: None)
   -fstd   : List of RPN files to process (Mandatory)
   -ip1    : IP1 to use (default: all)
   -prj    : prj georeference file to use for output file (default: WGS84 latlon)
   -out    : Output directory (default: .)
   -help   : This information"
}

proc RPN2FSTD::Run { } {
   variable Param

   foreach file $Param(Files) {
      Log::Print INFO "Processign file $file"

      fstdfile open FILEIN read $file
      set path [pwd]

      foreach datev [fstdfile info FILEIN DATEV] {

         set n 0
         set fields {}
         set time [clock format $datev -format "%Y%m%d_%H%M" -gmt True]
         Log::Print INFO "   Found date $time"

         foreach var $Param(Vars) {
            Log::Print INFO "   Checking for variable $var"
            foreach field [lindex [fstdfield find FILEIN [fstdstamp fromseconds $datev] "" $Param(IP1) -1 -1 "" $var] 0] {
               fstdfield read DATA$n FILEIN $field
               if { [llength $Param(Contours)] } {
                  fstdfield configure DATA$n -desc ${var} -rendercontour 1 -intervals $Param(Contours)
               } else {
                  fstdfield configure DATA$n -desc ${var} -rendertexture 1 -min 1e-32
               }
               lappend fields DATA$n
               incr n
            }
         }

         if { [llength $fields] } {
            Log::Print INFO "   Exporting fields $fields"
            cd $Param(Out)
            ogrfile open FILE write ${time}.shp "ESRI Shapefile"

            if { [file exists $Param(ProjFile)] } {
   #               georef create REF { PROJCS["WGS 84 / UPS North",GEOGCS["WGS 84",DATUM["WGS_1984",SPHEROID["WGS 84",6378137,298.257223563]],PRIMEM["Greenwich",0],UNIT["degree",0.0174532925199433]],PROJECTION["Polar_Stereographic"],PARAMETER["latitude_of_origin",90],PARAMETER["central_meridian",0],PARAMETER["scale_factor",0.994],PARAMETER["false_easting",2000000],PARAMETER["false_northing",2000000],UNIT["metre",1]] }
               georef create REF [exec cat $Param(ProjFile)]
               ogrlayer create FILE LAYER ${time} REF
            } else {
               ogrlayer create FILE LAYER ${time}
            }

            ogrlayer import LAYER $fields

            ogrfile close FILE
            ogrlayer free LAYER
            eval fstdfield free $fields

            eval exec zip ${time}.zip ${time}.shp ${time}.shx ${time}.dbf ${time}.prj
            file delete ${time}.shp ${time}.shx ${time}.dbf ${time}.prj
            cd $path
         }
      }
   }
}

proc RPN2FSTD::ParseCommandLine { } {
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
         "version"  { puts "$Param(Version)"; Log::End 0 }
         "var"      { set i [Args::Parse $gargv $gargc $i 2 RPN2FSTD::Param(Vars)] }
         "contour"  { set i [Args::Parse $gargv $gargc $i 2 RPN2FSTD::Param(Contours)] }
         "fstd"     { set i [Args::Parse $gargv $gargc $i 2 RPN2FSTD::Param(Files)] }
         "prj"      { set i [Args::Parse $gargv $gargc $i 2 RPN2FSTD::Param(ProjFile)] }
         "out"      { set i [Args::Parse $gargv $gargc $i 1 RPN2FSTD::Param(Out)] }
         "ip1"      { set i [Args::Parse $gargv $gargc $i 1 RPN2FSTD::Param(IP1)] }

         "help"      { Log::Print MUST "$Param(CommandLine)"; Log::End 0 }
         default     { Log::Print ERROR "Invalid argument [lindex $gargv $i]\n\n$Param(CommandLine)"; Log::End 1 }
      }
   }
}

set Log::Param(Level) DEBUG      ;#Log level
set Log::Param(Time)  False      ;#Print the time
set Log::Param(Proc)  False      ;#Print the calling proc

Log::Start RPN2FSTD $RPN2FSTD::Param(Version)

RPN2FSTD::ParseCommandLine
RPN2FSTD::Run

Log::End