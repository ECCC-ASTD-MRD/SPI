#CMOI_LEVEL tcl
#CMOI_PLATFORM op_f

namespace eval JetLayout { }

proc JetLayout::Layout { Frame } {
   variable Data
   global env

   set Viewport::Map(Mask)   DATA
   set Viewport::Map(Res)    8
   set Viewport::Map(Coast)  0                ;#Cotes
   set Viewport::Map(Lake)   1                ;#Lacs
   set Viewport::Map(River)  0                ;#Rivieres
   set Viewport::Map(Polit)  0                ;#Bordures politiques
   set Viewport::Map(Admin)  1                ;#Bordures politiques internes
   set Viewport::Map(City)   0                ;#Villes
   set Viewport::Map(Road)   0                ;#Routes
   set Viewport::Map(Rail)   0                ;#Chemin de fer
   set Viewport::Map(Util)   0                ;#Utilitaires
   set Viewport::Map(Canal)  0                ;#Canal/Aqueduc
   set Viewport::Map(Topo)   0                ;#Topographie
   set Viewport::Map(Bath)   0                ;#Bathymetrie
   set Viewport::Map(Text)   0                ;#Texture
   set Viewport::Map(Coord)  0                ;#Positionnement des latlon (<0=Ocean,>0=Partout)

   set Viewport::Resources(Bkg)       #E6EcFF
   set Viewport::Resources(Polit)     #020202
   set Viewport::Resources(Admin)     #020202
   set Viewport::Resources(Coast)     #010101
   set Viewport::Resources(Lake)      #000000
   set Viewport::Resources(FillCoast) ""
   set Viewport::Resources(FillLake) ""

   Page::Size $Frame 900 750

   #----- Setup the Geography mask
   set layers [ogrfile open MASKFILE read $env(CMCCONST)/img.SPI/jetmap/northamerica_adm0.shp]
   eval ogrlayer read MASKLAYER [lindex $layers 0]
   ogrlayer configure MASKLAYER -fill #010101 -outline #010101 -width -2
   ogrlayer define MASKLAYER -mask True

   projection configure $Page::Data(Frame) -data { MASKLAYER }

   #----- Definition de la camera
   set ProjCam::Data(Params[namespace current]) "{ 0.0 0.0 1.0 } { 0.0 0.0 2.0 } { 0.0 1.0 0.0 } 2.40774849159 0 0 1 0 0 0 60 -96"
   ProjCam::Select $Frame $Frame [namespace current]

   #----- Affichage des Viewports
   set Data(Viewport001) [Viewport::Create $Frame 0 0 901 571 1 1]

   Page::UpdateItems $Frame
}

proc JetLayout::LayoutUpdate { Frame } {
#   SPI::LayoutUpdate $Frame
}
