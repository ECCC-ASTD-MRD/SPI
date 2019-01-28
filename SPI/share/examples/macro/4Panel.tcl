#===============================================================================
# Environnement Canada
# Centre Meteorologique Canadian
# 2100 Trans-Canadienne
# Dorval, Quebec
#
# Projet   : Analyse objective
# Fichier  : 4Panel.tcl
# Creation : Fevrier 2015 - J.P. Gauthier - CMC/CMOE
#
# Description:
#   Carte 4 Panneau standard.
#
# Remarques :
#
# Modification:
#===============================================================================

namespace eval Macro::4Panel {} {
   variable Param
   variable Data
   variable Error

   set Param(Info)     { "Carte 4 Panneau" "4 Panel chart. " }
   set Param(InfoArgs) { { "Modèle" "Run" "Heure" } { "Model file" "Run" "Hour" } }

   set Param(Model) glb
   set Param(Run)   00
   set Param(Hour)  000
}


proc Macro::4Panel::Execute { } {
   global env
   variable Param


   colormap create HRMAP
   colormap control HRMAP -add 0 255 255 255 000
   colormap control HRMAP -add 1 220 220 220 255 
   colormap control HRMAP -add 2 200 200 200 255
   
   colormap create TTMAP
   colormap control TTMAP -add 0 220 220 220 255 
   colormap control TTMAP -add 1 200 200 200 255

   colormap create PRMAP
   colormap control PRMAP -add 0 240 240 240 255
   colormap control PRMAP -add 1 200 200 200 255
   colormap control PRMAP -add 2 160 160 160 255
   colormap control PRMAP -add 3 120 120 120 255
   colormap control PRMAP -add 4 070 070 070 255
   colormap control PRMAP -add 5 010 010 010 255

   colormap create DZMAP
   colormap control DZMAP -add 0 220 220 220 255 
   
   catch { font create FONTHL -family arial -weight bold -size 20 -slant roman -underline 0 -overstrike 0 }
   catch { font create FONTLBL -family arial -weight bold -size 10 -slant roman -underline 0 -overstrike 0 }

   fstdfile open FILE read $env(CMCGRIDF)/prog/$Param(Model)pres/$Param(Run)_$Param(Hour)

   fstdfield read GZ500  FILE -1 "" "500 PRESSURE"  -1 -1 "" "GZ"
   fstdfield read QQ500  FILE -1 "" "500 PRESSURE"  -1 -1 "" "QQ"
   fstdfield alias HGZ500 GZ500

   fstdfield read GZ1000 FILE -1 "" "1000 PRESSURE" -1 -1 "" "GZ"
   fstdfield read PN     FILE -1 "" -1              -1 -1 "" "PN"
   vexpr DZ1000 GZ500-GZ1000
   fstdfield alias DZT1000 DZ1000
   fstdfield alias PNH PN
   
   fstdfield read GZ700  FILE -1 "" "700 PRESSURE"  -1 -1 "" "GZ"
   fstdfield read HR700  FILE -1 "" "700 PRESSURE"  -1 -1 "" "HR"
   fstdfield alias HGZ700 GZ700
   
   fstdfield read GZ850  FILE -1 "" "850 PRESSURE"  -1 -1 "" "GZ"
   fstdfield read TT850  FILE -1 "" "850 PRESSURE"  -1 -1 "" "TT"
   fstdfield read PR     FILE -1 "" -1              -1 -1 "" "PR"
   fstdfield alias HGZ850 GZ850
   fstdfield alias TT850T TT850
   fstdfield alias TT8500 TT850
   vexpr DZ700 GZ700-GZ1000
   
   fstdfield configure GZ500   -color black   -width 1 -font FONTLBL -value INTEGER 0 -rendercontour 1 -renderlabel 10 -intervalmode INTERVAL 6
   fstdfield configure HGZ500  -color black   -width 3 -font FONTHL  -value INTEGER 0 -rendercontour 1 -rendervalue 20 -intervals { 474 498 522 546 570 594 }
   fstdfield configure QQ500   -color #aaaaaa -width 1 -font FONTLBL -value INTEGER 0 -rendercontour 1 -factor 1e5 -intervals { 8 12 16 }

   fstdfield configure PN      -color black   -width 1 -font FONTLBL -value INTEGER 0 -rendercontour 1 -renderlabel 10 -intervalmode INTERVAL 4 
   fstdfield configure PNH     -color black   -width 3 -font FONTHL  -value INTEGER 0 -rendercontour 1 -rendervalue 20 -intervals { 952 976 1000 1024 1048 } 
   fstdfield configure DZ1000  -color #aaaaaa -width 1 -font FONTLBL -value INTEGER 0 -rendercontour 1 -renderlabel 10 -intervalmode INTERVAL 6
   fstdfield configure DZT1000 -color black   -width 1 -font FONTLBL -value INTEGER 0 -rendertexture 1 -colormap DZMAP -intervals { 534 540 } -mapabove False -mapbelow False
   
   fstdfield configure GZ700   -color black   -width 1 -font FONTLBL -value INTEGER 0 -rendercontour 1 -renderlabel 10 -intervalmode INTERVAL 6 
   fstdfield configure HGZ700  -color black   -width 3 -font FONTHL  -value INTEGER 0 -rendercontour 1 -rendervalue 20 -intervals { 204 228 252 276 300 324  }
   fstdfield configure HR700   -color #aaaaaa -width 1 -font FONTLBL -value INTEGER 0 -rendertexture 1 -rendercontour 1 -renderlabel -1 -colormap HRMAP -unit "%" -intervals { 50 70 90 } -factor 100.0

   fstdfield configure DZ700   -color black   -width 3 -font FONTHL  -value INTEGER 0 -rendercontour 1 -intervals { 284 }  -dash -
   fstdfield configure GZ850   -color black   -width 1 -font FONTLBL -value INTEGER 0 -rendercontour 1 -renderlabel 10 -intervalmode INTERVAL 6 
   fstdfield configure HGZ850  -color black   -width 3 -font FONTHL  -value INTEGER 0 -rendercontour 1 -rendervalue 20  -intervals { 102 126 150 174 } 
   fstdfield configure TT850   -color #aaaaaa -width 1 -font FONTLBL -value INTEGER 0 -rendercontour 1 -renderlabel 10 -intervalmode INTERVAL 5
   fstdfield configure TT850T  -color black   -width 1 -font FONTLBL -value INTEGER 0 -rendertexture 1 -colormap TTMAP -intervals { -2.5 2.5 } -mapbelow False -mapabove False
   fstdfield configure TT8500  -color black   -width 3 -font FONTLBL -value INTEGER 0 -rendercontour 1 -intervals 0 -dash .
   fstdfield configure PR      -color black   -width 1 -font FONTLBL -value FLOAT 1   -rendercontour 1 -rendertexture 1 -colormap PRMAP -factor 1000 -intervals { 0.5 5 10 25 50 100 } -mapbelow False -mapabove True

   Viewport::Assign $Page::Data(Frame) $4Panel::Data(Viewport001) { QQ500 GZ500 HGZ500 }
   Viewport::Assign $Page::Data(Frame) $4Panel::Data(Viewport002) { PNH PN DZ1000 DZT1000 }
   Viewport::Assign $Page::Data(Frame) $4Panel::Data(Viewport003) { GZ700 HGZ700 HR700 }

   if { $Param(Hour)=="000" } {
      Viewport::Assign $Page::Data(Frame) $4Panel::Data(Viewport004) { GZ850 HGZ850 TT850 TT850T TT8500 }
   } else {
      Viewport::Assign $Page::Data(Frame) $4Panel::Data(Viewport004) { DZ700 PR }
      $Page::Data(Canvas) itemconfigure CB850 -data PR
   }
   
   Page::UpdateCommand $Page::Data(Frame)

   if { $SPI::Param(Batch) } {
      PrintBox::Image $Page::Data(Frame) png 4Panel.png
      SPI::Quit
   }
}

proc Macro::4Panel::Clean { } {

   Viewport::UnAssign $Page::Data(Frame) $4Panel::Data(Viewport001)
   Viewport::UnAssign $Page::Data(Frame) $4Panel::Data(Viewport002)
   Viewport::UnAssign $Page::Data(Frame) $4Panel::Data(Viewport003)
   Viewport::UnAssign $Page::Data(Frame) $4Panel::Data(Viewport004)

   catch {
      fstdfile close FILE
      fstdfield free QQ500 GZ500 HGZ500 PNH PN DZ1000 DZT1000 GZ700 HGZ700 HR700 GZ850 HGZ850 TT850 TT850T DZ700 PR 
   }
}

proc Macro::4Panel::Args { } {
   global argv argc
   variable Param

   #----- Lire les parametres si il y en a
   if { $argc>0 } { set Param(Model) [lindex $argv 0] }
   if { $argc>1 } { set Param(Run)   [lindex $argv 1] }
   if { $argc>2 } { set Param(Hour)  [lindex $argv 2] }
}

