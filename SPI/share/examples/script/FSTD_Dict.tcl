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

fstddict load $env(AFSISIO)/datafiles/constants/ops.variable_dictionary.xml
fstddict load $env(CI_SPI_IN)/eer.variable_dictionary.xml

puts "Variables (All) : [fstddict var]"
puts "Variables:(TT*) : [fstddict var TT]"
puts "Variables:(CMOE): [fstddict var -searchorigin CMOE]"
puts "Variables:(CV CMOE): [fstddict var CV -searchorigin CMOE]"
puts "Types    : [fstddict type]\n"

puts "Var TT        : [fstddict varinfo TT]"
puts "Var TT        : [fstddict varinfo TT -lang fr -short -units]"
puts "Var FM        : [fstddict varinfo FM]"
puts "Var FM        : [fstddict varinfo FM -lang fr -short -units]"
puts "Var FM        : [fstddict varinfo FM -lang fr -units -short -origin -nature]"
puts "Var CV        : [fstddict varinfo CV -searchorigin CMOE]"
puts "Var VF (1199) : [fstddict varinfo VF -lang fr -searchip1 1199 -short -units]"
puts "Var VF (33)   : [fstddict varinfo VF -lang fr -searchip1 33 -short -units]"
puts "Var VFDGD     : [fstddict varinfo VFDGD -lang fr -short -units]"
puts "Var PR (ERC25___NALL): [fstddict varinfo PR -lang fr -all -modifier ERC25___NALL -short -units]"
puts "Var TT (ERGE1___D010): [fstddict varinfo TT -lang fr -modifier ERGE1___NALL -short -units]"

fstddict varinfo TT -lang en -short "tototo tititi"
puts "Var TT new  : [fstddict varinfo TT -lang en -short]"

fstddict varinfo XXXX -lang en -short "toxxxxxx tixxxxx"
puts "Var XXXX new: [fstddict varinfo XXXX -lang en -short]"

puts "Type C      : [fstddict typeinfo C]"
puts "Type A      : [fstddict typeinfo A -lang fr -short]"

fstddict typeinfo XX -lang en -short "toxxxxxx tixxxxx"
puts "Type XX new : [fstddict typeinfo XX -lang en -short]"

puts -nonewline "Time of 100000 iterations: " 
puts [time {
for { set n 0 } { $n < 100000 } { incr n } {
   fstddict varinfo TT -lang en -short -units
}
}]

Log::End 0
