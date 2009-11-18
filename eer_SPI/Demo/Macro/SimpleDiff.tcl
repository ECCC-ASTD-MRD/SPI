#===============================================================================
# Environnement Canada
# Centre Meteorologique Canadian
# 2100 Trans-Canadienne
# Dorval, Quebec
#
# Projet   : Exemples de scripts de productions de cartes.
# Fichier  : SimpleDiff.tcl
# Creation : Fevrier 2009 - J.P.Gauthier - CMC/CMOE
#
# Description:
#    Simple diff�rence entre deux champs.
#
# Arguments  :
#
# Remarques :
#
#===============================================================================

namespace eval Macro::SimpleDiff {} {
   variable Param
   variable Data
   variable Error

   set Param(Info) { "Simple diff�rence entre deux champs."
                     "Simple difference between two fields." }

   set Error(Field)  { "Aucun champs valide trouv�!\nV�rifiez le viewport actif."
                       "Could not find any valid field!\nCheck the active viewport." }
}

proc Macro::SimpleDiff::Execute { } {
   global GDefs
   variable Data
   variable Error
   variable Param

#   SPI::LayoutLoad $Page::Data(Frame) SimpleLayout
#   ProjCam::Set SimpleDiff { 0.0 0.0 1.0 } { 0.0 0.0 2.0 } { 0.0 1.0 0.0 } 2.42073170732 0 0 1 0 0 0 51.8451999041 -101.92600494
#   ProjCam::Select $Page::Data(Frame) $Page::Data(Frame) SimpleDiff

   Macro::Doing "Reading fields"
   fstdfile open DIFF_FILE1 read ../Script/DataIn/2005102612_012
   fstdfield read DIFF_FIELD1 DIFF_FILE1 -1 "" 12000 24 -1 "" ES

   fstdfile open DIFF_FILE2 read ../Script/DataIn/2005102612_012
   fstdfield read DIFF_FIELD2 DIFF_FILE2 -1 "" 12000 24 -1 "" TT

   Macro::Doing "Doing diff"
   vexpr DIFF DIFF_FIELD1-DIFF_FIELD2

   if { ![colormap is DIFF_MAP] } {
      colormap create DIFF_MAP
      colormap read DIFF_MAP $GDefs(DirEER)/Colormap/OTH_Bias.rgba
   }
   font create DIFF_FONT -family courier -size -12 -weight bold

   fstdfield configure DIFF -desc "Diff" -factor 1 -color black -colormap DIFF_MAP -font DIFF_FONT \
       -rendertexture 1 -rendercontour 2 -value FLOAT 2

   Macro::Doing "Creating product"
   Viewport::UnAssign $Page::Data(Frame) $Viewport::Data(VP)
   Viewport::Assign   $Page::Data(Frame) $Viewport::Data(VP) DIFF

#   DataBar::Create $Page::Data(Frame) $Viewport::Data(VP) 5 5 750 60 "Diff between field1 and field2"
#   ColorBar::Create $Page::Data(Frame) $Viewport::Data(VP) 5 150 100 250

   Page::UpdateCommand $Page::Data(Frame)

   Macro::Doing ""
   Macro::Cursor left_ptr

   #----- If in batch mode, print the map and exit

   if { $SPI::Param(Batch) } {
      PrintBox::Image $Page::Data(Frame) png SimpleDiff
      SPI::Quit
   }

   fstdfile close DIFF_FILE1
   fstdfile close DIFF_FILE2
}

proc Macro::SimpleDiff::Clean { } {

   Viewport::UnAssign $Page::Data(Frame) $Viewport::Data(VP)
   font delete DIFF_FONT

   fstdfield free DIFF_FIELD1 DIFF_FIELD2 DIFF
}










