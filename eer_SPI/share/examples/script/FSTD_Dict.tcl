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
# Fichier    : FSTD_Dict.tcl
# Creation   : Juillet 2009 - J.P. Gauthier - CMC/CMOE
# Description: Test de fonctions d'acces au dictionnaire des variables RPN
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

Log::Start [info script] 0.1

fstddict load /users/dor/afsr/005/Projects/libeerUtils/data/stdf.variable_dictionary.xml
fstddict load DataIn/eer.variable_dictionary.xml

puts "Variables (All): [fstddict var]"
puts "Variables:(TT*): [fstddict var TT]"
puts "Types    : [fstddict type]\n"

puts "Var TT      : [fstddict varinfo TT]"
puts "Var TT      : [fstddict varinfo TT -lang fr -short -units]"
puts "Var FM      : [fstddict varinfo FM -lang fr -short -units]"
puts "Var FM      : [fstddict varinfo FM -lang fr -units -short -origin -nature]"
puts "Var VF (22) : [fstddict varinfo VF -lang fr -ip1 22 -short -units]"
puts "Var VF (33) : [fstddict varinfo VF -lang fr -ip1 33 -short -units]"

puts "Type C : [fstddict typeinfo C]"
puts "Type A : [fstddict typeinfo A -lang fr -short]"

Log::End 0
