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

puts \n[file tail [info script]]

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
      puts "   Processign file $file"

      fstdfile open FILEIN read $file
      set path [pwd]

      foreach datev [fstdfile info FILEIN DATEV] {

         set n 0
         set fields {}
         set time [clock format $datev -format "%Y%m%d_%H%M" -gmt True]
         puts "   Found date $time"

         foreach var $Param(Vars) {
            puts "   Checking for variable $var"
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
            puts "   Exporting fields $fields"
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
      puts $Param(CommandLine)
      exit 0;
   }

   #----- Parse arguments
   for { set i 0 } { $i < $gargc } { incr i } {
      switch -exact [string trimleft [lindex $gargv $i] "-"] {
         "version"  { puts "$Param(Version)"; exit 0 }
         "var"      { set i [Args::Parse $gargv $gargc $i 2 RPN2FSTD::Param(Vars)] }
         "contour"  { set i [Args::Parse $gargv $gargc $i 2 RPN2FSTD::Param(Contours)] }
         "fstd"     { set i [Args::Parse $gargv $gargc $i 2 RPN2FSTD::Param(Files)] }
         "prj"      { set i [Args::Parse $gargv $gargc $i 2 RPN2FSTD::Param(ProjFile)] }
         "out"      { set i [Args::Parse $gargv $gargc $i 1 RPN2FSTD::Param(Out)] }
         "ip1"      { set i [Args::Parse $gargv $gargc $i 1 RPN2FSTD::Param(IP1)] }

         "help"      { puts $Param(CommandLine); exit 0 }
         default     { puts stderr "Invalid argument [lindex $gargv $i]\n\n$Param(CommandLine)"; exit 1 }
      }
   }
}

namespace eval Args { }

proc Args::Parse { Argv Argc No Multi Var { Values {} } } {

   upvar #0 $Var var

   if { !$Multi } {
      set var True
   } else {

      #----- Garder l'index de depart
      set idx [incr No]
      set var {}

      if { $Multi==3 } {
         set var True
      } else {
         set var {}
      }

      #----- Parcourir les arguments du token specifie
      while { ([string is double [lindex $Argv $No]] || [string index [lindex $Argv $No] 0]!="-") && $No<$Argc } {

         #----- Check for argument validity
         set vs [lindex $Argv $No]
         if { $Multi==2 } {
            set vs [split $vs +]
         }

         if { [llength $Values] } {
            foreach v $vs {
               if { [lsearch -exact $Values $v]==-1 } {
                  puts stderr "Invalid value ($v) for parameter [lindex $Argv [expr $No-1]], must be one of { $Values }"
                  exit 1;
               }
            }
         }
         if { $Multi==1 || $Multi==3 } {
            set var $vs
         } else {
            eval lappend var $vs
         }
         incr No
      }

      #----- Verifier le nombre de valeur
      if { $Multi && ![llength $var] }  {
         puts stderr "No value specified for parameter [lindex $Argv [expr $No-1]]"
         exit 1;
      }

      if { [string index [lindex $Argv $No] 0]=="-" } {
         incr No -1
      }
   }

   return $No
}

RPN2FSTD::ParseCommandLine
RPN2FSTD::Run