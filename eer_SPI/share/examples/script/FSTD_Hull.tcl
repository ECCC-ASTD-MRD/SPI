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
package require Logger

namespace eval FSTD_Hull { } {
   global env
   variable Param

   set Param(Version) 1.0

   set Param(Min)     ""
   set Param(Max)     ""
   set Param(Buffer)  1
   set Param(Dist)    0
   set Param(Vars)    ""
   set Param(IP1)     -1
   set Param(IP3)     -1
   set Param(Etiket)  ""
   set Param(File)    ""
   set Param(Out)     ./out

   set Param(CommandInfo) "Calculate convex hull of specific range values in an RPN field and saves them in shapefile"
   
   set Param(CommandLine) "   Command line otions are:\n
      -fstd   : RPN file (Mandatory)
      -var    : List of variables to process (Mandatory)
      -ip1    : IP1 to use (Default: $Param(IP1))
      -ip3    : IP3 to use (Default: $Param(IP3))
      -etiket : Etiket to use (Default: $Param(Etiket))
      -min    : Minimum value to contour (Default: \"$Param(Min)\")
      -max    : Maximum value to contour (Default: \"$Param(Max)\")
      -buffer : Distance buffer arounf points (Default: \"$Param(Buffer)\"\")
      -dist   : Distance between hull befor merging (Default: \"$Param(Dist)\"\")
      -out    : Output directory (Default: $Param(Out))
      -help   : This information"
}

proc FSTD_Hull::Run { } {
   variable Param

   catch { eval file delete [glob DataOut/FSTD_Hull.*] }
   
   fstdfile open FILEIN read $Param(File)
 
   fstdfield read FLD FILEIN -1 $Param(Etiket) $Param(IP1) -1 $Param(IP3) "" $Param(Vars)

   #   fstdfield read FLD FILEIN $field

   #----- Configure range of value and export type
   if { $Param(Min)!="" } {
      fstdfield configure FLD -min $Param(Min) -mapbellow False
   }
   if { $Param(Max)!="" } {
      fstdfield configure FLD -max $Param(Max) -mapabove False
   }
   fstdfield configure FLD -rendertexture 1
   
   #----- Creer la couche avec le bon referential
   ogrlayer new OGRLAYER "Data" "Polygon"

   #----- Si c'est vide, on retourne
   if { ![ogrlayer define OGRLAYER -nb] } {
      Log::Print WARNING "No data within value range"
      return
   }

   #----- Importer les donnees RPN dans la couche
   ogrlayer import OGRLAYER FLD

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
            if { $Param(Dist) && $hull!=$dgeom && [ogrgeometry stats $hull -dist $dgeom]<=$Param(Dist) } {
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
   ogrfile open OGRFILE write $Param(Out) "ESRI Shapefile"
   ogrlayer write BUFFERED OGRFILE
   ogrfile close OGRFILE 
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

         "help"      { Log::Print MUST "$Param(CommandInfo)\n\n$$Param(CommandLine)"; Log::End 0 }
         default     { Log::Print ERROR "Invalid argument [lindex $gargv $i]\n\n$Param(CommandLine)"; Log::End 1 }
      }
   }
}

set Log::Param(Level) INFO       ;#Log level
set Log::Param(Time)  False      ;#Print the time
set Log::Param(Proc)  False      ;#Print the calling proc

Log::Start FSTD_Hull $FSTD_Hull::Param(Version)

FSTD_Hull::ParseCommandLine
FSTD_Hull::Run

Log::End