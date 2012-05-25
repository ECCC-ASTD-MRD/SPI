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
# Fichier    : FSTD_Hyb-Thermo+Momentum2MAGL.tcl
# Creation   : Mars 2010 - Stephane Gaudreault, J.P. Gauthier - CMC/CMOE
# Description: Interpoler des champs dans la verticale de ETA a MASL et masquer les montagnes
#
# Parametres :
#
# Retour:
#
# Remarques  :
#
#============================================================================

package require TclData

puts \n[file tail [info script]]

catch { file delete DataOut/FSTD_Hyb-Thermo+Momentum2MAGL.fstd }

set ttlevels { 2.0  80.0 500.0 1000.0 5000.0 10000.0 }
set uvlevels { 10.0 80.0 }

fstdfield ip1mode NEW

#----- Open RPN in/out files
fstdfile open IN read DataIn/2010010103_006_2.5km
fstdfile open OUT write DataOut/FSTD_Hyb-Thermo+Momentum2MAGL.fstd

#----- Copy grid descriptors
fstdfield read TIC IN -1 "" -1 -1 -1 "" >>
fstdfield read TAC IN -1 "" -1 -1 -1 "" ^^
fstdfield write TIC OUT 0 True
fstdfield write TAC OUT 0 True

#----- Read TT to be interpolated
fstdfield read TT IN -1 "" -1 -1 -1  "" TT
fstdfield readcube TT False 0.8 1.0

#----- Read GZ to use as vertical reference only at TT's levels
fstdfield read GZ IN -1 "" [fstdfield define TT -IP1] -1 -1  "" GZ
fstdfield readcube GZ False [fstdfield stats TT -levels]

puts "   Using [fstdfield define GZ -NK] thermo levels : [fstdfield stats GZ -levels] "

#----- Create output grid in vertical MASL
fstdfield create TO [fstdfield define GZ -NI] [fstdfield define GZ -NJ] [llength $ttlevels]
fstdfield stats TO -leveltype MAGL -levels $ttlevels
fstdfield clear TO
fstdfield configure TO -interpdegree LINEAR

fstdfield verticalinterp TO TT GZ GZ
fstdfield write TO OUT 0 True

#----- Read UU to be interpolated
fstdfield read UV IN -1 "" -1 -1 -1  "" UU
fstdfield readcube UV False 0.8 1.0

#----- Read GZ to use as vertical reference only at TT's levels
fstdfield read GZ IN -1 "" [fstdfield define UV -IP1] -1 -1  "" GZ
fstdfield readcube GZ False [fstdfield stats UV -levels]

puts "   Using [fstdfield define GZ -NK] momentum levels : [fstdfield stats GZ -levels] "

#----- Create output grid in vertical MASL
fstdfield create TO [fstdfield define GZ -NI] [fstdfield define GZ -NJ] [llength $uvlevels]
fstdfield stats TO -leveltype MAGL -levels $uvlevels
fstdfield clear TO
fstdfield configure TO -interpdegree LINEAR

fstdfield verticalinterp TO UV GZ GZ
fstdfield write TO OUT 0 True

fstdfile close IN OUT
