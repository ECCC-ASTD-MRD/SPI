#===============================================================================
# Environnement Canada
# Centre Meteorologique Canadian
# 2100 Trans-Canadienne
# Dorval, Quebec
#
# Projet   : Exemples de scripts de productions de cartes.
# Fichier  : LLMap.tcl
# Creation : Fevrier 20016- J.P.Gauthier - CMC/CMOE
#
# Description:
#    Affiche un champ sur une projection cylindrique.
#
# Arguments  :
#
# Remarques :
#
#===============================================================================

namespace eval Macro::LLMap {} {
   variable Param

   set Param(Info)     { "Affiche un champ."
                         "Display a field." }
   set Param(InfoArgs) { { "Fichier standard" "Variable" "[IP1]" } { "Standard file" "Variable" "[IP1]" } }

   set Param(File)  ""   ;# Standard file
   set Param(Var)   ""   ;# NOMVAR to use
   set Param(IP1)   -1   ;# IP1 to use
}

proc Macro::LLMap::Execute { } {
   global env
   variable Param

   fstdfile open FILE read $Param(File)
   fstdfield read FIELD FILE -1 "" $Param(IP1) -1 -1 "" $Param(Var)

#   fstdfile open FILE2 read DataIn/2005102612_012
#   fstdfield read FIELD2 FILE2 -1 "" 12000 18 -1 "" TT

#   #----- Difference between fields
#   vexpr DIFF FIELD1-FIELD2

   #----- Create colormap and fonts
   if { ![colormap is CMAP] } {
      colormap create CMAP
      colormap read CMAP $env(HOME)/.spi/Colormap/REC_Col.std1.rgba
   }
   font create FFONT -family courier -size -12 -weight bold

   #----- Configure field for display
   fstdfield configure FIELD -color black -colormap CMAP -font FFONT -rendertexture 1 -value FLOAT 2 -intervalmode INTERVAL 4 \
      -rendercontour 1 -width 1 -renderlabel -1 -value INTEGER

   #----- Center at latlon 0,0
   Viewport::Rotate   $Page::Data(Frame) 0.0 0.0 1.0
   
   #----- Clear viewport and assign field
   Viewport::UnAssign $Page::Data(Frame) $Viewport::Data(VP)
   Viewport::Assign   $Page::Data(Frame) $Viewport::Data(VP) FIELD

   #----- Update dependent widgets (colorbar)
   Page::UpdateCommand $Page::Data(Frame)

   #----- If in batch mode, print the map and exit
   if { $SPI::Param(Batch) } {
      PrintBox::Image $Page::Data(Frame) png LLMap
      SPI::Quit
   }

   fstdfile close FILE
}

proc Macro::LLMap::Clean { } {

   Viewport::UnAssign $Page::Data(Frame) $Viewport::Data(VP)
   font delete FFONT

   fstdfield free FIELD
}

proc Macro::LLMap::Args { } {
   global argv argc
   variable Param

   if { $argc } {
      set Param(File)  [lindex $argv 0]
      set Param(Var)   [lindex $argv 1]
      
      #----- Optional arguments
      if { [llength $argc]>2 } { set Param(IP1)   [lindex $argv 2] }
   }
}








