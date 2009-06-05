#!/bin/sh
# the next line restarts using tclsh \
exec $SPI_PATH/tclsh "$0" "$@"
#=============================================================================
# Environnement Canada
# Centre Meteorologique Canadien
# Dorval, Quebec
#
# Projet     : Interface pour le modele CANERM.
# Nom        : <GenerateMetfields.tcl>
# Creation   : Avril 2000 - J.P. Gauthier - CMC/CMOE
#
# Description: Preparer les donnees des champs meteorologiques.
#
# Parametres :
#    ${0}    : Repertoire de travail temporaire.
#    ${1}    : Date de la premiere simulation.
#    ${2}    : Date de la simulation courante.
#
# Remarques  :
#    Aucune.
#
# Modifications  :
#    Nom         :
#    Date        :
#    Description :
#
#=============================================================================

package require TclData

#----- Recuperer les parametres

set DirTmp      [lindex $argv 0]
set Date0       [lindex $argv 1]
set Date1       [lindex $argv 2]
set MetDataFile [lindex $argv 3]

#----- Lire les donnees

cd $DirTmp

set stmp [exec r.date $Date0]
set grid [split [exec cat griddef] ","]
set file [open $MetDataFile r]
gets $file files
close $file

fstdfile open 1 w ../results/${Date1}_000m

#----- Creer le champs avec la grille de sortie

fstdfield create NEW [lindex $grid 0] [lindex $grid 1] 1
fstdfield define NEW -GRTYP [string range [lindex $grid 6] 0 0] [lindex $grid 2] [lindex $grid 3] [lindex $grid 4] [lindex $grid 5]

#----- Interpoler les champs

foreach file $files {
   if { [file exists $file] } {

      foreach field [fstdfile open 2 r $file] {

         switch -regexp [lindex $field 2] {
            "ES|GZ|TT|UU" {

               set IP1    [lindex $field 4]
               set IP2    [lindex $field 5]

               if { [expr $IP2%6]==0 && ($IP1==500 || $IP1==700 || $IP1==850 || $IP1==1000) } {

                  fstdfield read FLD 2 [lindex $field 1]

                  fstdfield gridinterp NEW FLD

                  #----- Determiner les nouveaux parametres temporels

                  set ip2   [exec r.date [fstdfield define NEW -DATEV] $stmp]
                  set npas  [expr int(double($ip2)/[fstdfield define NEW -DEET]*3600.0)]

                  #----- Definir le champs

                  fstdfield define NEW -IP2 $ip2 -DATEO $stmp -NPAS $npas
                  fstdfield write NEW 1 -16 True
               }
            }
         }
      }
      fstdfile close 2
   }
}

fstdfile close 1

#----- Calculer l'humidite relative

fstdfile open 1 w ../results/${Date1}_000m

foreach idx [fstdfield find 1 -1 "" -1 -1 -1 "" "ES"] {

   #----- Lire les champs d'entree

   fstdfield read ES 1 $idx
   fstdfield read TT 1 [fstdfield define ES -DATEV] [fstdfield define ES -ETIKET] [fstdfield define ES -IP1] \
      [fstdfield define ES -IP2] [fstdfield define ES -IP3] [fstdfield define ES -TYPVAR] "TT"

   #----- Effectuer les calculs

   vexpr TT min(TT,-20.0)
   vexpr ES clamp(ES,0.0,20.0)
   vexpr PV 10^(9.4041-2354.0/(TT+273.0))
   vexpr HR (10^(9.4041-2354.0/((TT-ES)+273.0)))/PV

   #----- Redefinir le NOMVAR en consequence

   fstdfield define PV -NOMVAR "PV"
   fstdfield define HR -NOMVAR "HR"

   #----- Ecrire les champs resultants

   fstdfield write PV 1 -16 True
   fstdfield write HR 1 -16 True
}

fstdfile close 1

catch { exec $env(SPI_PATH)/Bin/$env(ARCH)/CodeInfo -info sim.pool -fstd ../results/${Date1}_000m -ckey codef -nomvar INFO }
