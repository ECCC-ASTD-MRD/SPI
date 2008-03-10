#!/bin/sh
#Get the command name \
DIR=$0
#Get the dirname \
DIR=${DIR%SpecieSelector.tcl}
#Let's start up with the proper wish \
exec $DIR../../wish "$0" "$@"
#===============================================================================
# Environnement Canada
# Centre Meteorologique Canadian
# 2100 Trans-Canadienne
# Dorval, Quebec
#
# Projet    : Interface de selection d'especes d'isotopes.
# Fichier   : SpecieSelector.tcl
# Creation  : Fevrier 1998 - J.P. Gauthier  - CMC/CMOE
#
# Description:
#
#      Cette interface permet de selectionne une espece d'isotope
#
# Remarques :
#   aucune
#
# Modification:
#
#   Nom         : -
#   Date        : -
#   Description : -
#
#===============================================================================

set GDefs(Version) [lindex [split [lindex [split [file dirname [file normalize [info script]]] /] end-2] -] end]
set GDefs(Dir)     [file dirname [info script]]/../../

source $env(HOME)/.eer_ToolDefs/.eer_Defs-$GDefs(Version)

#----- Inclusion de fichiers complementaires

source $GDefs(Dir)/Process/SpecieSelector/SpecieSelector.ctes
source $GDefs(Dir)/Process/SpecieSelector/SpecieSelector.txt
source $GDefs(Dir)/Process/SpecieSelector/SpecieSelector.int

#-------------------------------------------------------------------------------
# Nom     : <ExecRequest>
# Creation: Mai 1998 - J.P.Gauthier - CMC/CMOE
#
# But     : Traite la requete au demarrage et termine le process
#
# Parametres  :
#   <Request> : Nom de la ou les sources a rechercher.
#
# Remarques :
#   aucune
#
# Modifications :
#
#   Nom         : -
#   Date        : -
#   Description : -
#
#-------------------------------------------------------------------------------

proc ExecRequest { Request } {
   global ISO

   set f [open $ISO(FichierSrc)]

   while { [gets $f ligne]  >= 0 } {

      if {[string index $ligne 0] != "C" && [string length $ligne] > 90 \
          && [string compare [lindex $ligne $ISO(Name)] $Request] == 0} {

         set list "[format "%-9s"  [lindex $ligne $ISO(Name)]]\
                   [format "%-11s" [lindex $ligne $ISO(Intensity)]]\
                   [format "%-10s" [lindex $ligne $ISO(HalfLife)]]\
                   [format "%-11s" [lindex $ligne $ISO(Dry)]]\
                   [format "%-10s" [lindex $ligne $ISO(Wet)]]\
                   [format "%-10s" [lindex $ligne $ISO(DryDepVel)]]\
                   [format "%-5s"  [lindex $ligne $ISO(Unit)]]"
         puts "data $list"
         break;
      }
   }
   close $f
   QuitSpecies
}

#-------------------------------------------------------------------------------
# Nom      : <ShowAllSpecies>
# Creation : Mai 97 - J.P. Gauthier - CMC/CMOE
#
# But      : Affiche la liste de tous les isotopes.
#
# Parametres :
#   <Parent> : Frame dans lequel tous les isotopes vont etre affiches.
#
# Remarques :
#   aucune
#
# Modifications :
#
#   Nom         : -
#   Date        : -
#   Description : -
#
#-------------------------------------------------------------------------------

proc ShowAllSpecies { Parent }  {
   global ISO
   global Txt_Erreur
   global GDefs

   #----- Copier le fichier source des isotopes dans un fichier de travail

   set f [open $ISO(FichierSrc)]

   while { [gets $f ligne]  >= 0 } {

      if {[string index $ligne 0] != "C" && [string length $ligne] > 90} {
         set list "[format "%-9s"  [lindex $ligne $ISO(Name)]]\
                   [format "%-11s" [lindex $ligne $ISO(Intensity)]]\
                   [format "%-10s" [lindex $ligne $ISO(HalfLife)]]\
                   [format "%-11s" [lindex $ligne $ISO(Dry)]]\
                   [format "%-10s" [lindex $ligne $ISO(Wet)]]\
                   [format "%-10s" [lindex $ligne $ISO(DryDepVel)]]\
                   [format "%-5s"  [lindex $ligne $ISO(Unit)]]"

         $Parent.box insert end $list
      }
   }
   close $f
}

#-------------------------------------------------------------------------------
# Nom      : <SpeciesDispatch>
# Creation : Janvier 1998 - J.P. Gauthier - CMC/CMOE -
#
# But      : Recoit la ligne de commander, en extrait la partie commande et la
#            partie data et appelle la fonction necessaire au traitement.
#
# Parametres :
#
# Remarques :
#    Aucune.
#
# Modifications :
#
#    Nom         : -
#    Date        : -
#    Description : -
#
#-------------------------------------------------------------------------------

proc SpeciesDispatch { } {

   gets stdin ligne_source

   set command [string range $ligne_source 0 3]
   set data    [string range $ligne_source 5 [string length $ligne_source]]

   if { $command == "quit"  } {
      QuitSpecies
   }
   if { $command == "show" } {
      wm deiconify .
      raise .
   }
}

#-------------------------------------------------------------------------------
# Nom      : <QuitSpecies>
# Creation : Janvier 1998 - J.P. Gauthier - CMC/CMOE
#
# But      : Permet de detruire le fichier temporaire servant a la manipulation
#            de la liste des sources et detruire la fenetre principale.
#
# Parametres :
#   Aucune.
#
# Remarques :
#   aucune
#
# Modifications :
#
#   Nom         : -
#   Date        : -
#   Description : -
#
#-------------------------------------------------------------------------------

proc QuitSpecies { } {
   global ISO

   # ----- Detruire la fenetre principale.

   close stdin
   destroy .
}

#-------------------------------------------------------------------------------
# Nom      : <ReturnSelect>
# Creation : Fevrier 1998 - J.P. Gauthier - CMC/CMOE
#
# But     : Retourne la ligne selectionne par le pipeline de commande
#
# Parametres :
#   <Parent> : Fenetre d'ou vient la selection.
#   <Pos>    : Position de la selection.
#
#
# Remarques :
#   aucune
#
# Modifications :
#
#   Nom         : -
#   Date        : -
#   Description : -
#
#-------------------------------------------------------------------------------

proc ReturnSelect { Parent Position } {

   puts "data [$Parent get [$Parent nearest $Position]]"
}

#----- Extraction des arguments

if { [lindex $argv 0] == 0 || [lindex $argv 0] == 1 } {

   set GDefs(Lang)  [lindex $argv 0]

   #----- Configurer le pipeline de commande

   fconfigure stdin -blocking false
   fileevent stdin readable SpeciesDispatch

   #----- Lancer l'interface du selecteur de sources

   SpeciesInterface

} else {

   #----- Sinon en mode requete

   set request [lindex $argv 0]

   #----- Lancer la requete

   ExecRequest $request
}
