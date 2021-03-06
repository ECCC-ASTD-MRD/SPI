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

package require TclData
#package require TclGeoEER
package require Logger

Log::Start [info script] 0.1

#----- Specifier la methode de redimentionnement (x2)
vector specification -alloc 2.0

#----- Creer un vecteur
vector create SPACETIME

#----- Specifier les dimensions du vecteur
vector dim SPACETIME { X Y Z T }

#----- Specifier un espace memoire initial
vector mem SPACETIME 10000

#---- Afficher le contenu du vecteur
puts "Before:"
puts "   SPACETIME=[vector get SPACETIME]"
puts "   SPACETIME.X=[vector get SPACETIME.X]"

#----- Definir l'item 0 dans chaque composante du vecteur
vector append SPACETIME { 10 5 11 13 }
vector append SPACETIME { 40 4 11 23 }
vector append SPACETIME { 20 3 12 43 }
vector append SPACETIME { 20 3 12 43 }
vector append SPACETIME { 20 3 12 43 }
vector append SPACETIME { 30 2 12 53 }
vector append SPACETIME { 10 1 13 63 }

puts "After:"
puts "   SPACETIME=[vector get SPACETIME]"
puts "   SPACETIME 0  =[vector get SPACETIME 0]"
puts "   SPACETIME end=[vector get SPACETIME end]"
puts "   SPACETIME.X  =[vector get SPACETIME.X end-1]"

#----- Trier selon l'item 1
vector sort -unique SPACETIME Y
puts "Vector after sort on Y:"
puts "   SPACETIME=[vector get SPACETIME]"
puts "   SPACETIME 0  =[vector get SPACETIME 0]"
puts "   SPACETIME end=[vector get SPACETIME end]"
puts "   SPACETIME.X  =[vector get SPACETIME.X end-1]"

#----- Definir l'item 1 dans chaque composante du vecteur
vector length SPACETIME 5
vector set SPACETIME(1) { 110 111 112 113 }
puts "   SPACETIME(1)=[vector get SPACETIME(1)]"

#----- Ajouter un gros paquet de valeur dans le vecteur
set x 1
set y 2
set z 3
set t 4

for { set i 0 } { $i < 1000 } { incr i } {
   vector append SPACETIME [list [incr x] [incr y] [incr z] [incr t]]
}

#----- copier le vecteur
vector copy SPACETIME2 SPACETIME

#----- Ajouter la copie a la fin du vecteur
vector append SPACETIME SPACETIME2

#----- Afficher la dimension du vecteur
puts "Vector length: [vector length SPACETIME.X]"

#----- Afficher l'espace memoire requise par le vecteur
puts "Vector mem   : [vector mem SPACETIME]"

#----- Tester les calculs
puts "Pre calc  :[vector get SPACETIME]"
vexpr WARP SPACETIME*1000
puts "Post calc :[vector get WARP]"

#----- Liberer le vecteur
vector free SPACETIME

#----- Creer un vecteur matriciel
vector create MATRIX { { 1 1 1 }
                       { 1 0 1 }
                       { 1 1 1 } }

puts "   MATRIX dim=[vector dim MATRIX]"
puts "   MATRIX=[vector get MATRIX]"

Log::End
