# the next line restarts using tclsh \
exec $SPI_PATH/tclsh "$0" "$@"
#============================================================================
# Environnement Canada
# Centre Meteorologique Canadien
# 2100 Trans-Canadienne
# Dorval, Quebec
#
# Projet     : Exemple de scripts.
# Fichier    : CSV2OBS.tcl
# Creation   : Fevrier 2008 - J.P. Gauthier - CMC/CMOE
# Description: Convertir un fichier EXCEL csv en fichier SPI obs
#
# Parametres :
#
# Retour:
#
# Remarques  :
#   -Il faut modifier l'entete a la main car elle depend des donnees (Voir doc de SPI: Format Obs)
#   -Les donnees vides ("") sont remplacees par -999 (Missing data)
#
#============================================================================

puts \n[file tail [info script]]
package require Logger

Log::Start [info script] 0.1

set in  [open $env(CI_SPI_IN)/Flight4.csv r]
set out [open $env(CI_SPI_OUT)/OBS_CSV2OBS.obs w]

#----- Lire la premiere ligne
gets $in head

#----- Ecrire l'entete
puts $out "Obs 3.1"
puts $out "Flight4"

#----- Il faut au moins ID LAT LON et une donnee DATA.???
puts $out "Flight_No ID Paltkft LAT LON DATA.Wind DATA.H2OLIC DATA.CNcnc DATA.PCtconc DATA.PCmassL0_9 DATA.F300tconc DATA.O3 DATA.SO2HI DATA.SO2LO DATA.NO DATA.CO DATA.Org_ug_m3 DATA.NO3_ug_m3 DATA.SO4_ug_m3 DATA.NH4_ug_m3 DATA.water_ug_m3 DATA.Org43_ug_m3 DATA.Org44_ug_m3 DATA.Org57_ug_m3 DATA.m41_ug_m3 DATA.potassium_ug_m3 DATA.total_ug_m3 DATA.NO2 ELEV ELEVTYPE"

#----- Loop sur les lignes de donnees
while { ![eof $in] } {

   gets $in line
   set line [split $line ,]

   set obs ""
   set dir ""
   set idx 0

   foreach item $line {

      #----- Combiner les spd,dir des vents (index 5 et 6)
      if { $idx==5 } {
         set dir $item
      } elseif { $idx==6 } {
         append obs " { $item $dir }"
      } else {
         if { $item=="" } {
            append obs " -999"
         } elseif { [string is double $item] } {
            append obs " $item"
         } else {
            append obs " \"$item\""
         }
      }
      incr idx
   }

   #----- Ecrire la ligne en ajoutant le tye d'elevatio (0=MASL)
   puts $out "$obs 0"
}

close $in
close $out

Log::End