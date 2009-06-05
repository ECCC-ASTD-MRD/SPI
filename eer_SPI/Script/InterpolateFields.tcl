#!/bin/sh
# the next line restarts using tclsh \
exec $SPI_PATH/tclsh "$0" "$@"
#============================================================================
# Environnement Canada
# Centre Meteorologique Canadien
# 2100 Trans-Canadienne
# Dorval, Quebec
#
# Projet     : Interface pour le modele CANERM.
# Nom        : <InterpolateFieldsMulti.tcl
# Creation   :
#
# Parametres :
#   ${1}     : Le repertoire temporaire de travail.
#   ${2}     : Le fichier de climatologie .
#   ${3}     : Le type de modele ( glb ou reg ).
#   ${4}     : Frequence du modele.
#   ${5}     : La frequence des donnees d'entrees.
#   ${6}     : La frequence des donnees de sorties.
#   ${7}     : Nombre de processus.
#
# Retour     :
#             un premier fichier standard (tape20) pour les champs suivant :
#
#                  MN (montagne (NCAR)                 - valeur: metre),
#                  MG (masque ocean/continent          - valeur: 0 ou 1),
#               et ZP (longueur de rugosite (CRESSMAN) - valeur: log(metre)).
#
#              un deuxieme fichier standard (tape30) pour les champs suivant :
#
#                  ES (ecart du point de rosee         - valeur: celcius),
#                  GZ (geopotentiel                    - valeur: Dam),
#                  P0 (pression a la surface           - valeur: millibar),
#                  PT (pression au toit du modele      - valeur: millibar),
#                  TT (temperature                     - valeur: celcius),
#                  UV (vitesse du vent                 - valeur: noeuds),
#               et WE (mouvement vertical ETA          - valeur: s-1).
#
# Remarques  :
#   Aucune.
#
# Modifications :
#
#   Nom         : -
#   Date        : -
#   Description : -
#
#============================================================================

package require TclData

namespace eval MetData {}

# ----- Mise-a-jour des directives de PGSM_CLIMATO (climatologie).
#       les champs sont :
#                 MN (montagne (NCAR)                 - valeur: metre),
#                 MG (masque ocean/continent          - valeur: 0 ou 1),
#              et ZP (longueur de rugosite (CRESSMAN) - valeur: log(metre)).

proc MetData::GenClimato { GRID In Out } {

   fstdfile open 99 read $In
   fstdfile open 88 write $Out

   foreach var { MN MG ZP } {
      foreach field [fstdfield find 99 -1 "" -1 -1 -1 "" $var] {
         fstdfield read FLD 99 $field
         fstdfield gridinterp $GRID FLD

         if { $var=="ZP" } {
            vexpr $GRID ($GRID-3.0)*0.85
         }
         fstdfield define $GRID -IP3 1
         fstdfield write $GRID 88 -16 True
      }
   }

   fstdfile close 88
   fstdfile close 99
}

# ----- Mise-a-jour des directives de PGSM_ANAL_PREV (analyse/prevision).
#       les champs sont :
#                 ES (ecart du point de rosee    - valeur: celcius),
#                 GZ (geopotentiel               - valeur: Dam),
#                 P0 (pression a la surface      - valeur: millibar),
#                 PT (pression au toit du modele - valeur: millibar),
#                 TT (temperature                - valeur: celcius),
#                 UV (vitesse du vent            - valeur: noeuds),
#              et WE (mouvement vertical ETA     - valeur: s-1).

proc MetData::GenMeteoFile { GRID Model Dt In Out } {

   fstdfile open 99 read $In
   fstdfile open 88 write $Out

   set lvls {}

   switch $Model {
     "glb" { set lvls { 2510 2750 3010 3270 3550 3850 4190 4580 5020 5510 6050 6600 7160 7740 8310 8880 9440 9960 10420 10840 11220 11550 11800 11930 12000 } }
     "reg" { set lvls { 2374 2720 2897 3044 3248 3541 3976 4522 5144 5843 6348 6612 7446 8034 8646 9272 9845 10346 10780 11151 11467 11733 11850 11950 12000 } }
   }

   for { set ip2 0 } {  $ip2 < 144 } { incr ip2 $Dt } {

      foreach var { ES GZ TT UU WE } {
         foreach lvl $lvls {
            foreach field [fstdfield find 99 -1 "" $lvl $ip2 -1 "" $var] {
               fstdfield read FLD 99 $field
               fstdfield gridinterp $GRID FLD
               fstdfield define $GRID -IP3 1
               fstdfield write $GRID 88 0 True
            }
         }
      }

      foreach var { P0 PT } {
         foreach field [fstdfield find 99 -1 "" -1 $ip2 -1 "" $var] {
            fstdfield read FLD 99 $field
            fstdfield gridinterp $GRID FLD
            fstdfield define $GRID -IP3 1
            vexpr $GRID $GRID*100.0
            fstdfield write $GRID 88 0 True
         }
      }
   }

   fstdfield read FLD 99 -1 "" -1 -1 -1 "" HY
   fstdfield write FLD 88 0 True

   fstdfile close 88
   fstdfile close 99
}

proc MetData::FileStamp { File } {

   set File [file tail $File]

   set date [string range $File 0 7]
   set time [string range $File 8 9]000000
   set hour [string range $File 11 end]

   set st [fstdstamp fromdate $date $time]
   set st [fstdstamp incr $st $hour]

   return $st
}

proc MetData::GenMeteo { GRID Model Freq Dt Files } {

   set t0 [MetData::FileStamp [lindex $Files 0]]
# avec 7.2.deb
#   set f0 [fstdstamp todate $t0 "%4i%02i%02i%02i"]

   set f0 [fstdstamp todate $t0]
   set f0 [lindex $f0 0][lindex $f0 1][lindex $f0 2][lindex $f0 3]

   foreach file $Files {

      set t1  [MetData::FileStamp $file]
      set dt  [fstdstamp dif $t1 $t0]
      set ext [format "%03i" [expr int(ceil($dt/$Freq))*$Freq]]

      puts ": Using file ${f0}_${ext}"
      MetData::GenMeteoFile $GRID $Model $Dt $file ../meteos/${f0}_${ext}
   }
}

# ----- Recupere les arguments.

set path  [lindex $argv 0] ;# Le repertoire temporaire de travail.
set clim  [lindex $argv 1] ;# Le fichier de climatologie .
set model [lindex $argv 2] ;# Le type de modele ( glb ou reg ).
set dt    [lindex $argv 3] ;# Frequence du modele.
set fout  [lindex $argv 4] ;# La frequence des donnees de sorties.

cd $path

exec echo "doing" > sim.meteo

# ----- Lecture des parametres pour la directive GRILLE

set grid   [exec cat griddef]
set files  [exec cat data_std_sim.eta]
set params [split $grid ,]

fstdfield create GRID [lindex $params 0] [lindex $params 1] 1
fstdfield define GRID -GRTYP [lindex $params end] [lindex $params 2] [lindex $params 3] [lindex $params 4] [lindex $params 5]
fstdfield configure GRID -interpdegree CUBIC

MetData::GenClimato GRID $clim ../meteos/tape20
MetData::GenMeteo   GRID $model $fout $dt $files

exec echo "done" > sim.meteo
