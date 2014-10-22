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
# Fichier    : FSTD_Hull.tcl
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

namespace eval FSTD_Hull { } {
   global env
   variable Param

   set Param(Version) 1.0

   set Param(Format)    "ESRI Shapefile"
   set Param(ProjFile)  ""
   set Param(Min)       ""
   set Param(Max)       ""
   set Param(Buffer)    0
   set Param(Dist)      0
   set Param(Vars)      ""
   set Param(IP1)       -1
   set Param(IP3)       -1
   set Param(Etiket)    ""
   set Param(File)      ""
   set Param(Out)       ./out

   set Param(CommandInfo) "   Calculate convex hull of specific range values in an RPN field"
   
   set Param(CommandLine) "   Command line otions:\n
\t-format      : Output format (${APP_COLOR_GREEN}\"$Param(Format)\"${APP_COLOR_RESET})
\t-fstd        : RPN file (Mandatory)
\t-var         : List of variables to process (Mandatory)
\t-ip1         : IP1 to use (${APP_COLOR_GREEN}$Param(IP1)${APP_COLOR_RESET})
\t-ip3         : IP3 to use (${APP_COLOR_GREEN}$Param(IP3)${APP_COLOR_RESET})
\t-etiket      : Etiket to use (${APP_COLOR_GREEN}\"$Param(Etiket)\"${APP_COLOR_RESET})
\t-min         : Minimum value to contour
\t-max         : Maximum value to contour
\t-buffer      : Distance buffer around points (${APP_COLOR_GREEN}$Param(Buffer)${APP_COLOR_RESET})
\t-dist        : Distance between hull before merging (${APP_COLOR_GREEN}$Param(Dist)${APP_COLOR_RESET})
\t-prj         : prj georeference file to use for output file (${APP_COLOR_GREEN}WGS84 latlon${APP_COLOR_RESET})
\t-out         : Output directory (${APP_COLOR_GREEN}$Param(Out)${APP_COLOR_RESET})
      
   Information parameters:\n
\t-help        : This information
\t-version     : Version
\t-verbose     : Trace level (ERROR,WARNING,${APP_COLOR_GREEN}INFO${APP_COLOR_RESET},DEBUG,EXTRA,0-4)
\t-verbosecolor: Use color for log messages

   Available formats: \n\n\t[lmap f $Export::Vector::Param(Formats) {lindex $f end-1}]"
}

proc FSTD_Hull::Run { } {
   variable Param

   #----- check for format and extract extension if valid
   if { [set idx [lsearch -exact -index end-1 $Export::Vector::Param(Formats) $Param(Format)]]==-1 } {
      Log::Print ERROR "Wrong format, available formats: \n\t[lmap f $Export::Vector::Param(Formats) {lindex $f end-1}]"
   }
   set ext [file extension [lindex $Export::Vector::Param(Formats) $idx end 0]]
   
   if { [file exists $Param(ProjFile)] } {
      georef create REF [exec cat $Param(ProjFile)]
   }

   fstdfile open FILEIN read $Param(File)

   foreach var $Param(Vars) {
      foreach fld [fstdfield find FILEIN -1 $Param(Etiket) $Param(IP1) -1 $Param(IP3) "" $var] {
      
         fstdfield read FLD FILEIN $fld

         set date [clock format [fstdstamp toseconds [fstdfield define FLD -DATEV]] -format "%Y%m%d_%H%M" -timezone :UTC]
         Log::Print INFO "Processing $var for $date"

         #----- Configure range of value and export type
         if { $Param(Min)!="" } {
            fstdfield configure FLD -min $Param(Min) -mapbellow False
         }
         if { $Param(Max)!="" } {
            fstdfield configure FLD -max $Param(Max) -mapabove False
         }
         fstdfield configure FLD -rendertexture 1
         
         #----- Creer la couche avec le bon referential
         ogrlayer free OGRLAYER DISSOLVED BUFFERED
         if { [georef is REF] } {
            ogrlayer new OGRLAYER "${var}_${date}" "Polygon" REF
         } else {
            ogrlayer new OGRLAYER "${var}_${date}" "Polygon"
         }
         
         #----- Importer les donnees RPN dans la couche
         ogrlayer import OGRLAYER FLD

         #----- Si c'est vide, on retourne
         if { ![ogrlayer define OGRLAYER -nb] } {
            Log::Print WARNING "No data within value range"
            continue
         }

         #----- Dissolve into polygon masses
         ogrlayer stats OGRLAYER -dissolve DISSOLVED

         #----- Apply a distance buffer
         ogrlayer stats DISSOLVED -buffer $Param(Buffer) 5 BUFFERED
               
         #----- Iterate to merge touching hull
         set n 4
         set geom    [ogrlayer define BUFFERED -geometry 0]
         set newgeom ""
         
         while { [incr n -1] } {
         
            #----- Get the sub geoms
            set hulls [ogrgeometry define $geom -geometry]
            
            #----- If we're inside a polygon, break
            if { [ogrgeometry define [lindex $hulls 0] -type]=="Line String" } {
               break
            }
            
            #----- Loop on each convex hull and do a geographic union
            set newgeom {}
            foreach hull $hulls {
            
               #----- Check hull distances for merge
               foreach dgeom $hulls {
                  if { $Param(Dist)<0 || ($Param(Dist) && $hull!=$dgeom && [ogrgeometry stats $hull -dist $dgeom]<=$Param(Dist)) } {
                     set hull [ogrgeometry stat [ogrgeometry stat $hull -union $dgeom] -convexhull]
                  }
               }
               
               #-----Create convex hulls
               if { [ogrgeometry is $newgeom] } {
                  set newgeom [ogrgeometry stat $newgeom -union [ogrgeometry stat $hull -convexhull]]
               } else {
                  set newgeom [ogrgeometry stat $hull -convexhull]
               }
            }    
            set geom $newgeom
         }

         #----- Keep resulting hulls
         if { ![ogrgeometry is $newgeom] } {
            set newgeom [ogrgeometry stat $geom -convexhull]
         }
         ogrlayer define BUFFERED -geometry 0 False $newgeom
         
         #----- Save it all
         catch { eval file delete -force [glob $Param(Out)_${var}_${date}*] }   
         ogrfile open OGRFILE write $Param(Out)_${var}_${date}${ext} $Param(Format)
         ogrlayer write BUFFERED OGRFILE
         ogrfile close OGRFILE 
      }
   }
}

proc FSTD_Hull::ParseCommandLine { } {
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
         "format"   { set i [Args::Parse $gargv $gargc $i VALUE FSTD_Hull::Param(Format)] }
         "fstd"     { set i [Args::Parse $gargv $gargc $i VALUE FSTD_Hull::Param(File)] }
         "var"      { set i [Args::Parse $gargv $gargc $i LIST  FSTD_Hull::Param(Vars)] }
         "ip1"      { set i [Args::Parse $gargv $gargc $i VALUE FSTD_Hull::Param(IP1)] }
         "ip3"      { set i [Args::Parse $gargv $gargc $i VALUE FSTD_Hull::Param(IP3)] }
         "etiket"   { set i [Args::Parse $gargv $gargc $i VALUE FSTD_Hull::Param(Etiket)] }
         "min"      { set i [Args::Parse $gargv $gargc $i VALUE FSTD_Hull::Param(Min)] }
         "max"      { set i [Args::Parse $gargv $gargc $i VALUE FSTD_Hull::Param(Max)] }
         "buffer"   { set i [Args::Parse $gargv $gargc $i VALUE FSTD_Hull::Param(Buffer)] }
         "dist"     { set i [Args::Parse $gargv $gargc $i VALUE FSTD_Hull::Param(Dist)] }
         "out"      { set i [Args::Parse $gargv $gargc $i VALUE FSTD_Hull::Param(Out)] }

         "verbose"       { set i [Args::Parse $argv $argc $i VALUE         Log::Param(Level)] }
         "verbosecolor"  { set i [Args::Parse $argv $argc $i FLAG          Log::Param(Color)] }
         "help"          { Log::Print MUST "$Param(CommandInfo)\n\n$Param(CommandLine)"; Log::End 0 }
         "version"       { Log::Print MUST $Param(Version); Log::End 0 }
         default         { Log::Print ERROR "Invalid argument [lindex $gargv $i]\n\n$Param(CommandLine)"; Log::End 1 }
      }
   }
}

set Log::Param(SPI)   7.7.1

Log::Start FSTD_Hull $FSTD_Hull::Param(Version)

FSTD_Hull::ParseCommandLine
FSTD_Hull::Run

Log::End