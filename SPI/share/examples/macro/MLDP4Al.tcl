#===============================================================================
# Environnement Canada
# Centre Meteorologique Canadian
# 2100 Trans-Canadienne
# Dorval, Quebec
#
# Projet   : Exemples de scripts de productions de cartes.
# Fichier  : MLDP4Al.tcl
# Creation : August 2014 - J.P.Gauthier - CMC/CMOE
#
# Description:
#    Produce maps for MLDP
#
# Arguments  :
#
# Remarques :
#
#===============================================================================

namespace eval Macro::MLDP4Al {} {
   variable Param
   variable Error

   set Param(Info) { "Produire des cartes pour MLDP."
                     "Produce maps for MLDP." }

   set Param(Files) ""    ;# MLDP file to process
   set Param(Var)   ZH    ;# Variable to produce
   set Param(Lat)   50    ;# Central latitude
   set Param(Lon)   -103  ;# Central longitude
   set Param(Lens)  2.0   ;# Zoom factor
   set Param(Min)   0     ;# Minimum height to color
   set Param(Max)   20000 ;# Maximum height to color
}

proc Macro::MLDP4Al::Execute { } {
   global env
   variable Error
   variable Param

   colormap create MLDP_MAP -file $env(HOME)/.spi/Colormap/REC_Col.std1.rgba
   font create MLDP_FONT -family courier -size -12 -weight bold

   $Page::Data(Canvas) create colorbar -x 5 -y 150 -width 100 -height 250 -anchor nw -barsplit 5 -barside right \
         -barborder 1 -barwidth 30 -bg white -transparency 75 -showfactor False -tags MLDP_COLORBAR
       
   Viewport::GoTo $Page::Data(Frame) $Param(Lat) $Param(Lon) $Param(Lens) 

   foreach file $Param(Files) {
      Macro::Doing "Processing file $file"
      
      fstdfile open FILEIN read $file
      puts "fstdfile info FILEIN DATEV $Param(Var)  [fstdfile info FILEIN DATEV $Param(Var)] "
      foreach datev [fstdfile info FILEIN DATEV $Param(Var)] {

         set fields {}
         set i 0
         foreach field [fstdfield find FILEIN [fstdstamp fromseconds $datev] "" -1 -1 -1 "" $Param(Var)] {
            fstdfield read FLDIN$i FILEIN $field
            fstdfield configure FLDIN$i -color black -colormap MLDP_MAP -font MLDP_FONT -renderparticle 2 -value INTEGER 0 -min $Param(Min) -max $Param(Max)
            lappend fields FLDIN$i
            incr i
         }
         
         Viewport::UnAssign $Page::Data(Frame) $Viewport::Data(VP)
         Viewport::Assign   $Page::Data(Frame) $Viewport::Data(VP) $fields
         
         $Page::Data(Canvas) itemconfigure MLDP_COLORBAR -data [lindex $fields 0]

         Page::UpdateCommand $Page::Data(Frame)

         Macro::Doing ""

         #----- If in batch mode, print the map and exit
         if { $SPI::Param(Batch) } {
            set date [clock format $datev -format "%Y%m%d_%H%M" -timezone :UTC]
            PrintBox::Image $Page::Data(Frame) png $Param(Var)_$date
         }
         
         Viewport::UnAssign $Page::Data(Frame) $Viewport::Data(VP)
         eval fstdfield free $fields
      }
      fstdfile close FILEIN  
   }
   
   if { $SPI::Param(Batch) } {
      SPI::Quit
   }
}

proc Macro::MLDP4Al::Clean { } {

   Viewport::UnAssign $Page::Data(Frame) $Viewport::Data(VP)
   font delete MLDP_FONT
   colormap free MLDP_MAP
   fstdfield free FLDIN
}


proc Macro::MLDP4Al::Args { } {
   global argv argc
   variable Param

   #----- Lire les parametres si il y en a
   if { $argc>0 } { set Param(Files) $argv }
}








