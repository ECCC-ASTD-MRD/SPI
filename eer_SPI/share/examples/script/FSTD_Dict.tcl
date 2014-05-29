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

fstddict load /users/dor/afsr/005/stdf.variable_dictionary.xml
fstddict load DataIn/eer.variable_dictionary.xml

puts "Variables: [fstddict var]"
puts "Types    : [fstddict type]"

puts "Var TT : [fstddict varinfo TT]"
puts "Var TT : [fstddict varinfo TT -lang fr -short -units]"
puts "Var FM : [fstddict varinfo FM -lang fr -short -units]"
puts "Var FM : [fstddict varinfo FM -lang fr -units -short]"

puts "Type C : [fstddict typeinfo C]"
puts "Type A : [fstddict typeinfo A -lang fr -short]"

Log::End 0
