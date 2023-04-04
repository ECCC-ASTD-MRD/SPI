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
# Fichier    : OGR_ModelIndexBuild.tcl
# Creation   : Septembre 2010 - J.P. Gauthier - CMC/CMOE
# Description: Creer un index de couverture des diverses grilles GEM
#
# Parametres :
#
# Retour:
#
# Remarques  : Il faut setter les varibles d'environnement CI_DATA_IN et CI_DATA_OUT pour exécuter ce script.
#              export CI_DATA_IN=<pathto_eerenv_code>/data/SourceFilesForModelDomain
#              Nous également de la varibalbe d'env WEBTIDE_DATA (export WEBTIDE_DATA=/ssm/net/cmoe/eer/master/WebTide_0.7.1_all/share)
#============================================================================

package require TclData
#package require TclGeoEER
package require Logger

Log::Start [info script] 0.1

#------ Liste des shapefiles à créer
set ModelDomain  { ModelDomain ModelDomainWet ModelDomainWaves }
set type         { met         wet            waves }

#------ Liste des modèles Atmosphériques
set names(met)  {     "RDPS"        "HRDPS"     "HRDPS_NORD"     "HRDPS_WEST"     }
set vars(met)   {      P0            P0          P0               P0              }
set subs(met)   {      { "" }        { "" }      { "" }           { "" }          }
set models(met) [list  rdps.fstd     hrdps.fstd  hrdps_nord.fstd  hrdps_west.fstd ]

#------ Liste des modèles Aquatiques
set names(wet)  {     "RIOPS"     "CIOPS_WEST"     "CIOPS_EAST"    "SHOP"    "SHOPSMALL"   "WCPS"    "SALISH"   "WEBTIDE" }
set vars(wet)   {      GL          GL               GL              ""        ""            ""        ""         M2       }
set subs(wet)   {      { "" }      { "" }           { "" }          { "" }    { ""}         { "" }    { "" }     { arctic9 ne_pac4 nwatl stle400 sshelf h3o } }
set models(wet) [list  riops.fstd  ciops_west.fstd  ciops_east.fstd shop.txt  shopsmall.txt wcps.txt  salish.txt $env(WEBTIDE_DATA)]

#------ Liste des modèles de vagues
set names(waves)  {     "RDWPS (NEP)"  "RDWPS (SUP)"   "RDWPS (HUM)"    "RDWPS (ERI)"    "RDWPS (ONT)"   "RDWPS (NWA)"   }
set vars(waves)   {     WH              GL              GL               GL               GL              WH             }
set subs(waves)   {     { "" }          { "" }          { "" }           { "" }           { "" }          { "" }         }
set models(waves) [list rdwps_nep.txt   rdwps_sup.fstd  rdwps_hum.fstd   rdwps_eri.fstd   rdwps_ont.fstd  rdwps_nwa.txt  ]

foreach md $ModelDomain t $type {

   set nb 0
   foreach s $subs($t) { incr nb [llength $s] }

   #----- Open GEM index file
   eval file delete [glob -nocomplain $env(CI_DATA_OUT)/$md*]
   ogrfile open INDEXFILE($md) write $env(CI_DATA_OUT)/$md.shp "ESRI Shapefile"
   ogrlayer create INDEXFILE($md) INDEX($md) "Domain"

   ogrlayer define INDEX($md) -field NAME String
   ogrlayer define INDEX($md) -field ID   Integer
   ogrlayer define INDEX($md) -nb $nb

   #----- Initialiser la geometrie
   ogrgeometry create TIN($md)    "Multi Polygon"
   ogrgeometry create TRI($md)    "Polygon"
   ogrgeometry create POLY($md)   "Polygon"
   ogrgeometry create RING($md)   "Linear Ring"
   ogrgeometry create MPOINT($md) "Multi Point"
   ogrgeometry create POINT($md)  "Point"

   #----- Loop on the models
   set no  0
   set mdl 0

   foreach model $models($t) sub $subs($t) var $vars($t) name $names($t) {

      if {$name == "GLSOCE" || $name == "SHOP" || $name == "SHOPSMALL" || $name == "WCPS" || $name == "SALISH" || $name == "RDWPS (NEP)" || $name == "RDWPS (NWA)" } {
         Log::Print INFO "Processing $model $name"

         #------ Read lat/lon for glsoce model (Great Lakes)
         set file [open "$env(CI_DATA_IN)/ModelDomain/$model" r]
         set coords [gets $file]
         close $file

         ogrgeometry define RING($md) -points {}
         foreach coord $coords {
            set coord [split $coord ,]
            ogrgeometry define RING($md) -addpoint [lindex $coord 0] [lindex $coord 1]
         }
         ogrgeometry define POLY($md) -geometry False RING($md)
         set geom POLY($md)

         ogrlayer define INDEX($md) -feature $no NAME $name
         ogrlayer define INDEX($md) -feature $no ID $mdl
         ogrlayer define INDEX($md) -geometry $no False $geom
         incr no

      } else {

         foreach s $sub {

            Log::Print INFO "Processing $model $s"

            #----- Pick the last file
            if { $name=="WEBTIDE"} {
               set file [glob -nocomplain $model/$s/*.fstd]
            } else {
               set file $env(CI_DATA_IN)/ModelDomain/$model
            }
            if { ![llength $file] } {
               Log::Print WARNING "No data is available of model $model/$s"
               continue
            }

            fstdfile open RPNFILE read $file

            #----- Read P0 since it's always available
            if { [catch { fstdfield read VAR RPNFILE -1 "" -1 -1 -1 "" $var} ] } {
               fstdfile close RPNFILE
               continue
            }

            #----- Get the limits
            set ni  [fstdfield define VAR -NI]
            set nj  [fstdfield define VAR -NJ]
            set gr  [fstdfield define VAR -GRTYP]

            ogrgeometry define RING($md) -points {}

            switch $gr {
               "Y" {
                      fstdfield read LAT RPNFILE -1 "" -1 -1 -1 "" ^^
                      fstdfield read LON RPNFILE -1 "" -1 -1 -1 "" >>

                      ogrgeometry define MPOINT($md) -geometry False {}
                      for { set i 0 } { $i<$ni } { incr i } {
                         set lat [fstdfield stats LAT -gridvalue $i 0]

                         if { [fstdfield define LON -NJ]==1 } {
                            set lon [fstdfield stats LON -gridvalue $i 0]
                         } else {
                            set lon [fstdfield stats LON -gridvalue 0 $i]
                         }

                         ogrgeometry define POINT($md) -points [list $lon $lat]
                         ogrgeometry define MPOINT($md) -addgeometry False POINT($md)
                      }
                      set geom [ogrgeometry stats MPOINT($md) -convexhull]
                   }
               "M" {
                      fstdfield read IDX RPNFILE -1 "" -1 -1 -1 "" ##
                      fstdfield read LAT RPNFILE -1 "" -1 -1 -1 "" ^^
                      fstdfield read LON RPNFILE -1 "" -1 -1 -1 "" >>

                      ogrgeometry define TIN($md)    -geometry False {}
                      for { set i 0 } { $i<[fstdfield define IDX -NI] } { incr i 3 } {
                         set i0 [fstdfield stats IDX -gridvalue $i          0]
                         set i1 [fstdfield stats IDX -gridvalue [expr $i+1] 0]
                         set i2 [fstdfield stats IDX -gridvalue [expr $i+2] 0]

                         set lo0 [fstdfield stats LON -gridvalue $i0 0]; set lo0 [expr $lo0>90?$lo0-360:$lo0]
                         set la0 [fstdfield stats LAT -gridvalue $i0 0]
                         set lo1 [fstdfield stats LON -gridvalue $i1 0]; set lo1 [expr $lo1>90?$lo1-360:$lo1]
                         set la1 [fstdfield stats LAT -gridvalue $i1 0]
                         set lo2 [fstdfield stats LON -gridvalue $i2 0]; set lo2 [expr $lo2>90?$lo2-360:$lo2]
                         set la2 [fstdfield stats LAT -gridvalue $i2 0]

                         ogrgeometry define RING($md) -points [list $lo0 $la0 $lo1 $la1 $lo2 $la2 $lo0 $la0]
                         ogrgeometry define TRI($md) -geometry False RING($md)
                         ogrgeometry define TIN($md) -addgeometry False TRI($md)
                      }
                      set geom [ogrgeometry stats TIN($md) -dissolve]
         #            set geom [ogrgeometry stats TIN($md) -convexhull]
                   }
               default {
                      #----- Bottom
                      set j 0
                      for { set i 0 } { $i<$ni } { incr i } {
                         set ll [fstdfield stats VAR -gridpoint $i $j]
                         ogrgeometry define RING($md) -addpoint [lindex $ll 1] [lindex $ll 0]
                      }

                      #----- Right
                      set i [expr $ni-1]
                      for { set j 0 } { $j<$nj } { incr j } {
                         set ll [fstdfield stats VAR -gridpoint $i $j]
                         ogrgeometry define RING($md) -addpoint [lindex $ll 1] [lindex $ll 0]
                      }

                      #----- Top
                      set j [expr $nj-1]
                      for { set i [expr $ni -1] } { $i>=0 } { incr i -1 } {
                         set ll [fstdfield stats VAR -gridpoint $i $j]
                         ogrgeometry define RING($md) -addpoint [lindex $ll 1] [lindex $ll 0]
                      }

                      #----- Left
                      set i 0
                      for { set j [expr $nj-1] } { $j>=0 } { incr j -1 } {
                         set ll [fstdfield stats VAR -gridpoint $i $j]
                         ogrgeometry define RING($md) -addpoint [lindex $ll 1] [lindex $ll 0]
                      }

                      #----- Close the polygon
                      set ll [fstdfield stats VAR -gridpoint 0 0]
                      ogrgeometry define RING($md) -addpoint [lindex $ll 1] [lindex $ll 0]
                      ogrgeometry define POLY($md) -geometry False RING($md)
                      set geom POLY($md)
                }
             }
             fstdfile close RPNFILE

             if { $s!="" } {
                ogrlayer define INDEX($md) -feature $no NAME "$name ($s)"
             } else {
                ogrlayer define INDEX($md) -feature $no NAME $name
             }
             ogrlayer define INDEX($md) -feature $no ID $mdl
             ogrlayer define INDEX($md) -geometry $no False $geom

             incr no
          }
       }

       incr mdl
   }

   ogrfile close INDEXFILE($md)
}

Log::End
