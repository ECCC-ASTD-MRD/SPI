#===============================================================================
# Environnement Canada
# Centre Meteorologique Canadian
# 2100 Trans-Canadienne
# Dorval, Quebec
#
# Projet   : Widget de librairie.
# Fichier  : Species.tcl
# Version  : 2.0
# Creation : Juillet 1998 - J.P. Gauthier - CMC/CMOE
#
# Description:
#    Definitions d'une boite de dialogue pour l'impresson de canvas
#
# Fonctions:
#
#===============================================================================

package provide Species 2.0

proc IdSpecies { Show } {

   if { $Show } {
      puts "(INFO) Loading Standard CMC/CMOE Package Species Version 2.0"
   }

}

namespace eval Species {
