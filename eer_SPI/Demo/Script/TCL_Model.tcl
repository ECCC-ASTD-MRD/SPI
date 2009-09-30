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
# Fichier    : TCL_Model.tcl
# Creation   : Septembre 2008 - J.P. Gauthier - CMC/CMOE
# Description: Test de lancement de simulation de trajectoire en tcl
#
# Parametres :
#
# Retour:
#
# Remarques  :
#   Aucune.
#
#============================================================================

package require TclData
package require TclSim

set date [clock scan "20080313 13:00" -gmt True]

set parth { { -98.6220000000 19.0230000000 700.0 Popocatepetl }
            { -98.6220000000 19.0230000000 500.0 Popocatepetl }
            { -98.6220000000 19.0230000000 250.0 Popocatepetl2 } }

set partm { { -98.6220000000 19.0230000000 100.0 Popocatepetl }
            { -98.6220000000 19.0230000000 5000.0 Popocatepetl }
            { -98.6220000000 19.0230000000 1000.0 Popocatepetl3 } }

set files [glob /data/goodenough/afseeer/dbase_64/prog/glbeta/2008031312_*]

#----- Test with remote RPN data
#for { set i 0 } { $i<72 } { incr i } {
#   lset files $i goodenough:[lindex $files $i]:
#}

#----- Create simulation object
simulation create TRAJ1 -type trajectory

#----- Define simulation parameters
simulation param TRAJ1 -title Popocatepetl -mode FORWARD -unit PRESSURE -timestep 3600 -sigt 0.15 -sigb 0.997 -ptop 10.0 \
   -date $date -particles $parth -data $files -output DataOut/TCL_Model.points -split True

simulation define TRAJ1 -loglevel 2 -logfile DataOut/TCL_Model.log

#----- Run simulation
simulation run TRAJ1

#----- Destroy simulation object
simulation destroy TRAJ1
