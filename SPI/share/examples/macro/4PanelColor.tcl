#===============================================================================
# Environnement Canada
# Centre Meteorologique Canadian
# 2100 Trans-Canadienne
# Dorval, Quebec
#
# Projet   : Analyse objective
# Fichier  : 4PanelColor.tcl
# Creation : Fevrier 2015 - J.P. Gauthier - CMC/CMOE
#
# Description:
#   Carte 4 Panneau standard.
#
# Remarques :
#
# Modification:
#===============================================================================

namespace eval Macro::4PanelColor {} {
   variable Param
   variable Data
   variable Error

   set Param(Info)     { "Carte 4 Panneau" "4 Panel chart. " }
   set Param(InfoArgs) { { "Modèle" "Run" "Heure" } { "Model file" "Run" "Hour" } }

   set Param(Model) glb
   set Param(Run)   00
   set Param(Hour)  000
}


proc Macro::4PanelColor::Execute { } {
   global env
   variable Param

   colormap create QQMAP
   colormap control QQMAP -add 0 255 254 003 255
   colormap control QQMAP -add 1 253 005 251 255
   colormap control QQMAP -add 2 254 010 012 255

   colormap create HRMAP
   colormap control HRMAP -add 0 195 255 255 255
   colormap control HRMAP -add 1 124 195 251 255
   colormap control HRMAP -add 2 058 124 184  255
   
   colormap create TTMAP
   colormap control TTMAP -add 0 253 121 133 255
   colormap control TTMAP -add 1 150 204 255 255
   
   colormap create PRMAP
   colormap control PRMAP -add 0 145 199 245 255
   colormap control PRMAP -add 1 013 151 235 255
   colormap control PRMAP -add 2 003 049 248 255
   colormap control PRMAP -add 3 004 249 106 255
   colormap control PRMAP -add 4 000 205 000 255
   colormap control PRMAP -add 5 000 154 000 255
   colormap control PRMAP -add 6 000 104 000 255
   colormap control PRMAP -add 7 250 250 054 255
   colormap control PRMAP -add 8 249 203 000 255
   colormap control PRMAP -add 9 249 155 000 255
   colormap control PRMAP -add 10 252 097 004 255
   colormap control PRMAP -add 11 253 000 000 255
   colormap control PRMAP -add 12 214 003 138 255
   colormap control PRMAP -add 13 141 043 185 255
   colormap control PRMAP -add 14 105 004 155 255
   colormap control PRMAP -add 15 150 150 150 255
   
   colormap create DZMAP
   colormap control DZMAP -add 0 154 205 255 255 
   colormap control DZMAP -add 1 000 000 000 000
   colormap control DZMAP -add 2 195 195 195 255
   colormap control DZMAP -add 3 000 000 000 000
   colormap control DZMAP -add 4 250 126 131 255
   
   catch { font create FONTHL -family arial -weight bold -size 20 -slant roman -underline 0 -overstrike 0 }
   catch { font create FONTLBL -family arial -weight bold -size 10 -slant roman -underline 0 -overstrike 0 }

   FieldBox::FileOpen [FieldBox::Create . "4Panel"] [glob  $env(CMCGRIDF)/prog/$Param(Model)pres/$Param(Run)_*]

   fstdfile open FILE read $env(CMCGRIDF)/prog/$Param(Model)pres/$Param(Run)_$Param(Hour)

   fstdfield read GZ500  FILE -1 "" "500 PRESSURE"  -1 -1 "" "GZ"
   fstdfield read UU500  FILE -1 "" "500 PRESSURE"  -1 -1 "" "UU"
   fstdfield read QQ500  FILE -1 "" "500 PRESSURE"  -1 -1 "" "QQ"
   fstdfield alias HGZ500 GZ500

   fstdfield read GZ1000 FILE -1 "" "1000 PRESSURE" -1 -1 "" "GZ"
   fstdfield read PN     FILE -1 "" -1              -1 -1 "" "PN"
   vexpr DZ1000 GZ500-GZ1000
   fstdfield alias DZT1000 DZ1000
   fstdfield alias PNH PN
   
   fstdfield read GZ700  FILE -1 "" "700 PRESSURE"  -1 -1 "" "GZ"
   fstdfield read UU700  FILE -1 "" "700 PRESSURE"  -1 -1 "" "UU"
   fstdfield read HR700  FILE -1 "" "700 PRESSURE"  -1 -1 "" "HR"
   fstdfield alias HGZ700 GZ700
   
   fstdfield read GZ850  FILE -1 "" "850 PRESSURE"  -1 -1 "" "GZ"
   fstdfield read UU850  FILE -1 "" "850 PRESSURE"  -1 -1 "" "UU"
   fstdfield read TT850  FILE -1 "" "850 PRESSURE"  -1 -1 "" "TT"
   fstdfield read PR     FILE -1 "" -1              -1 -1 "" "PR"
   
   fstdfield configure GZ500   -color black   -width 1 -font FONTHL  -value INTEGER 0 -rendercontour 1 -intervalmode INTERVAL 6 
   fstdfield configure HGZ500  -color black   -width 3 -font FONTLBL -value INTEGER 0 -rendercontour 1 -intervals { 474 498 522 546 570 594 }  
   fstdfield configure UU500   -color #808080 -width 1 -font FONTLBL -value INTEGER 0 -rendervector BARBULE 
   fstdfield configure QQ500   -color black   -width 1 -font FONTLBL -value INTEGER 0 -rendertexture 1 -colormap QQMAP -unit "/s" -intervals { 16 24 32 } -factor 1e5

   fstdfield configure DZ1000  -color black   -width 1 -font FONTLBL -value INTEGER 0 -rendercontour 1 -intervalmode INTERVAL 6 -dash - -renderlabel 10
   fstdfield configure DZT1000 -color black   -width 1 -font FONTLBL -value INTEGER 0 -rendertexture 1 -colormap DZMAP  -value INTEGER 0 -unit dm -min 510 -intervals { 510 516 534 540 570 }  
   fstdfield configure PN      -color black   -width 1 -font FONTHL  -value INTEGER 0 -rendercontour 1 -rendervalue 20 -intervalmode INTERVAL 4
   fstdfield configure PNH     -color black   -width 3 -font FONTLBL -value INTEGER 0 -rendercontour 1 -intervals { 952 976 1000 1024 1048 } 
   
   fstdfield configure GZ700   -color black   -width 1 -font FONTHL  -value INTEGER 0 -rendercontour 1 -rendervalue 20 -intervalmode INTERVAL 6 
   fstdfield configure UU700   -color #808080 -width 1 -font FONTLBL -value INTEGER 0 -rendervector BARBULE 
   fstdfield configure HR700   -color blue    -width 1 -font FONTLBL -value INTEGER 0 -rendertexture 1 -rendercontour 1 -colormap HRMAP -value INTEGER 0 -unit "%" -intervals { 50 70 90 } -factor 100.0
   fstdfield configure HGZ700  -color black   -width 3 -font FONTLBL -value INTEGER 0 -rendercontour 1 -intervals { 204 228 252 276 300 324  }  

   fstdfield configure GZ850   -color black   -width 1 -font FONTHL  -value INTEGER 0 -rendervalue 20 -rendercontour 1 -intervalmode INTERVAL 6 
   fstdfield configure UU850   -color #808080 -width 1 -font FONTLBL -value INTEGER 0 -rendervector BARBULE 
   fstdfield configure TT850   -color black   -width 1 -font FONTLBL -value INTEGER 0 -rendertexture 1 -colormap TTMAP -intervals { -2.5 0 2.5 } -mapbelow False -mapabove False
   fstdfield configure PR      -color black   -width 1 -font FONTLBL -value INTEGER 0 -rendertexture 1 -colormap PRMAP -value FLOAT 1 -unit "mm/hr" -factor 1000 -intervals { 0.1 0.5 1.0 2.5 5 7.5 10 15 20 25 30 40 50 75 100 150 250 } -mapbelow False -mapabove True

   Viewport::Assign $Page::Data(Frame) $4Panel::Data(Viewport001) { GZ500 HGZ500 QQ500 UU500 }
   Viewport::Assign $Page::Data(Frame) $4Panel::Data(Viewport002) { PN PNH  DZ1000 DZT1000 }
   Viewport::Assign $Page::Data(Frame) $4Panel::Data(Viewport003) { GZ700 HGZ700 HR700 UU700 }

   $Page::Data(Canvas) itemconfigure CB500  -data QQ500
   $Page::Data(Canvas) itemconfigure CB1000 -data DZT1000
   $Page::Data(Canvas) itemconfigure CB700  -data HR700

   if { $Param(Hour)=="000" } {
      Viewport::Assign $Page::Data(Frame) $4Panel::Data(Viewport004) { GZ850 TT850 UU850 }
      $Page::Data(Canvas) itemconfigure CB850 -data TT850
   } else {
      Viewport::Assign $Page::Data(Frame) $4Panel::Data(Viewport004) { PN PR }
      $Page::Data(Canvas) itemconfigure CB850 -data PR
   }
   
   Page::UpdateCommand $Page::Data(Frame)

   if { $SPI::Param(Batch) } {
      PrintBox::Image $Page::Data(Frame) png OA_4PAN.png
      SPI::Quit
   }
}

proc Macro::4PanelColor::Clean { } {

   Viewport::UnAssign $Page::Data(Frame) $4PanelColor::Data(Viewport001)
   Viewport::UnAssign $Page::Data(Frame) $4PanelColor::Data(Viewport002)
   Viewport::UnAssign $Page::Data(Frame) $4PanelColor::Data(Viewport003)
   Viewport::UnAssign $Page::Data(Frame) $4PanelColor::Data(Viewport004)

   catch {
      fstdfile close FILE
      fstdfield free GZ500 HGZ500 QQ500 UU500 PN PNH DZ1000 DZT1000 GZ700 HGZ700 HR700 UU700 GZ850 TT850 UU850 PN PR
   }
}

proc Macro::4PanelColor::Args { } {
   global argv argc
   variable Param

   #----- Lire les parametres si il y en a
   if { $argc>0 } { set Param(Model) [lindex $argv 0] }
   if { $argc>1 } { set Param(Run)   [lindex $argv 1] }
   if { $argc>2 } { set Param(Hour)  [lindex $argv 2] }
}

