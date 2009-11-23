#===============================================================================
# Environnement Canada
# Centre Meteorologique Canadian
# 2100 Trans-Canadienne
# Dorval, Quebec
#
# Projet   : Exemples de scripts de productions de cartes.
# Fichier  : URPMap.tcl
# Creation : Fevrier 2001 - J.P.Gauthier - CMC/CMOE
#
# Description:
#    Produire une image de radar au format URP
#
# Arguments  :
#   <Radar>  : Identificateur du radar (Ex:XFT)
#   <Date>   : Date pour l'affichage des donnees (Ex:200906161630)
#
# Remarques :
#===============================================================================

namespace eval Macro::URPMap {} {
   variable Param
   variable Data
   variable Error

   set Param(Radius)    242000                                       ;#Rayon du radar
   set Param(Date)      ""                                           ;#Rayon du radar
   set Param(Pixels)    600                                          ;#dimension de l'image en pixels
   set Param(Radars)    "WMB"                                        ;#Identificateur du radar
   set Param(Intervals) { 7 23 38 33 37 40 43 45 47 50 53 55 57 60 } ;#Liste des intervals
   set Param(Ranges)    { 40000 80000 120000 160000 200000 240000}   ;#Cercles de rayons

   set Param(Info)   { "Centrer et zoomer sur la position d'un radar"
                       "Center and zoom on a radar" }
}

proc Macro::URPMap::Execute { } {
   global env
   variable Data
   variable Error
   variable Param

   set Viewport::Map(Type)        orthographic
   set Viewport::Map(Coord)       0
   set Viewport::Map(CoordDef)    10.0
   set Viewport::Map(CoordNum)    0
   set Viewport::Map(Res)         0
   set Viewport::Map(Mask)        0
   set Viewport::Map(Coast)       1
   set Viewport::Map(Lake)        1
   set Viewport::Map(River)       0
   set Viewport::Map(Admin)       1
   set Viewport::Map(Road)        0
   set Viewport::Map(Rail)        0
   set Viewport::Map(Util)        0
   set Viewport::Map(Canal)       0
   set Viewport::Map(Topo)        0
   set Viewport::Map(Bath)        0
   set Viewport::Map(Text)        0
   set Viewport::Map(City)        1
   set Viewport::Resources(Bkg)       white
   set Viewport::Resources(FillCoast) #989866
   set Viewport::Resources(FillLake)  #333366
   set Viewport::Resources(Coast)     black
   set Viewport::Resources(Lake)      #333366
   set Viewport::Resources(River)     #0000ff
   set Viewport::Resources(Polit)     #ff0000
   set Viewport::Resources(Admin)     #ff0000
   set Viewport::Resources(City)      white
   set Viewport::Resources(Road)      #404040
   set Viewport::Resources(Rail)      #ff1493
   set Viewport::Resources(Util)      #ffff00
   set Viewport::Resources(Canal)     #00ffff
   set Viewport::Resources(Coord)     #000000

   font configure FONT$Page::Data(Frame) -weight normal -size 10
   Viewport::Do  $Page::Data(Frame)

   #----- Redimension page to specified size
   Page::Size $Page::Data(Frame) $Param(Pixels) $Param(Pixels)

   #----- Read the radar location file
   if { ![ogrlayer is RADARLAYER] } {
      ogrfile open RADARFILE read $GDefs(Dir)/Data/RADAR.shp
      ogrlayer read RADARLAYER RADARFILE 0
   }

   #----- Create the colormap
   if { ![colormap is RADAR_MAP] } {
      colormap create RADAR_MAP
      colormap read RADAR_MAP $env(HOME)/.spi/Colormap/URP.rgba
      colormap configure RADAR_MAP -interp False
   }

   foreach id $Param(Radars) {

      #----- find the specified radar location
      set f [ogrlayer define RADARLAYER -featureselect [list [list ID == $id]]]

      #----- If it exists, center and zoom on it
      if { [llength $f] } {
         set lat  [ogrlayer define RADARLAYER -feature $f LAT]
         set lon  [ogrlayer define RADARLAYER -feature $f LON]
         set elev [ogrlayer define RADARLAYER -feature $f ELEV]
         set name [ogrlayer define RADARLAYER -feature $f NAME]
         set lens [expr 6378140.0/$Param(Radius)]

         Viewport::Rotate $Page::Data(Frame) $lat $lon $lens

         #----- If a date is specified, display the radar data
         if { $Param(Date)!="" } {
            Viewport::UnAssign $Page::Data(Frame) $Viewport::Data(VP)
            radarfile open RADARSITE read /data/radar/rawdata/${id}/$Param(Date)~~CONVOL:URP:XFT:RADAR:IRIS
            radarscan read SCAN1 RADARSITE 0
            radarscan configure SCAN1 -desc "$name CAPPI 1.5km" -colormap RADAR_MAP  -color white \
               -intervals $Param(Intervals) -rendertexture 1 -ranges $Param(Ranges) -value INTEGER 0 -interpdegree NEAREST
            radarscan define SCAN1 -LOCATION $lat $lon $elev
            Viewport::Assign $Page::Data(Frame) $Viewport::Data(VP) SCAN1
         }
      }

      #----- If in batch mode, print image
      if { $SPI::Param(Batch) } {
         PrintBox::Image $Page::Data(Frame) png ./radar_$id
      }
   }

   #----- If in batch mode, exit
   if { $SPI::Param(Batch) } {
      SPI::Quit
   }
}

proc Macro::URPMap::Clean { } {

}

#----- Lire les parametres si il y en a
if { $argc>0 } { set Macro::URPMap::Param(Radars) [lindex $argv 0] }
if { $argc>1 } { set Macro::URPMap::Param(Date)   [lindex $argv 1] }
