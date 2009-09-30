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
package require MetData

puts \n[file tail [info script]]

set String "ceci est un test\n ok la\n\tééédfg dféédààà ççç \u306F."

#----- Encode

fstdfile open TEXTFILE write DataOut/FSTD_InfoText.fstd
fstdfield write [MetData::TextCode $String] TEXTFILE 0 True
fstdfile close TEXTFILE

#----- Decode

fstdfile open TEXTFILE read DataOut/FSTD_InfoText.fstd

fstdfield read FLD TEXTFILE -1 "UNICODE" -1 -1 -1 "" TEXT
puts "   [MetData::TextDecode FLD]"

