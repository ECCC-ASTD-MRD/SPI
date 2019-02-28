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
# Fichier    : FSTD_InfoText.tcl
# Creation   : Decembre 2005 - J.P. Gauthier - CMC/CMOE
# Description: Demonstration de l'ecriture de champs info ou texte en ascii dans
#              un fichier standard.
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
package require MetData
package require Logger

Log::Start [info script] 0.2

file delete $env(CI_SPI_OUT)/FSTD_InfoText.fstd
fstdfile open TEXTFILE write $env(CI_SPI_OUT)/FSTD_InfoText.fstd

foreach string { "ceci est un test" "ceci est un test\n ok la\n\tééédfg dféédààà ççç \u306F." } type { ASCII UNICODE } {

   #----- Encode
   fstdfield write [MetData::TextCode $string] TEXTFILE 0 True

   #----- Decode
   fstdfield read FLD TEXTFILE -1 $type -1 -1 -1 "" TEXT
   Log::Print INFO "Decoded ($type): [MetData::TextDecode FLD]"
}

fstdfile close TEXTFILE

Log::End
