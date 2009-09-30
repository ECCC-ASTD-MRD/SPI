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
# Fichier    : TCL_Vector.tcl
# Creation   : Mars 2005 - J.P. Gauthier - CMC/CMOE
# Description: Demonstration de l'usage des vecteurs TCL
#
# Parametres :
#
# Retour:
#
# Remarques  :
#
#============================================================================

package require TclSystem

set fs [lindex $argv 0]

puts "File system info for $fs"
puts "   Type      : [system filesystem $fs -type]"
puts "   Blocks    : [system filesystem $fs -blocksize -blocks -blockfree -blockused]"
puts "   Size      : [system filesystem $fs -size]"
puts "   Free      : [system filesystem $fs -free]"
puts "   Used      : [system filesystem $fs -used]"

puts "\nSystem limit for process"
puts "   CPU Time   : [system limit -CPU]"
puts "   Data size  : [system limit -DATA]"
puts "   Stack size : [system limit -STACK]"

system limit -CPU unlimited
puts "Stack size :[system limit -CPU]"

#system fork
#system daemonize -lock /tmp/test.pid

