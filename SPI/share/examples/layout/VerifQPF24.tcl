namespace eval VerifQPF24 { }

proc VerifQPF24::Layout { Frame } {
   variable Data

   Page::Size $Frame 0 0

   #----- Definition de la camera

   ProjCam::Set VerifQPF24      { 0.0 0.0 1.0 } { 0.0 0.0 2.0 } { 0.0 1.0 0.0 } 2.42073170732 0 0 1 0 0 0 51.8451999041 -101.92600494
   ProjCam::Set VerifQPF24_WEST { 0.0 0.0 1.0 } { 0.0 0.0 2.0 } { 0.0 1.0 0.0 } 5.44317492889 0 0 1 0 0 0 49.3270542318 -118.581223592
   ProjCam::Set VerifQPF24_EAST { 0.0 0.0 1.0 } { 0.0 0.0 2.0 } { 0.0 1.0 0.0 } 6.72997751255 0 0 1 0 0 0 46.8057544504 -71.9475470875
   ProjCam::Select $Frame $Frame VerifQPF24

   #----- Affichage des Viewports

   set Viewport::Map(Type)        "orthographic"
   set Viewport::Map(Data)        ""
   set Viewport::Map(Elev)        "1.0"
   set Viewport::Map(Coord)       1
   set Viewport::Map(CoordDef)    10.0
   set Viewport::Map(CoordNum)    2
   set Viewport::Map(Res)         0
   set Viewport::Map(Mask)        0
   set Viewport::Map(Coast)       1
   set Viewport::Map(Lake)        0
   set Viewport::Map(River)       0
   set Viewport::Map(Admin)       1
   set Viewport::Map(Road)        0
   set Viewport::Map(Rail)        0
   set Viewport::Map(Util)        0
   set Viewport::Map(Canal)       0
   set Viewport::Map(Topo)        0
   set Viewport::Map(Bath)        0
   set Viewport::Map(Text)        0
   set Viewport::Map(TPolit)      1
   set Viewport::Map(TCity)       1
   set Viewport::Resources(Bkg)       "white"
#   set Viewport::Resources(FillCoast) "#CCCCCC"
   set Viewport::Resources(FillCoast) "#EEE8DF"
   set Viewport::Resources(FillLake)  "white"
   set Viewport::Resources(Coast)     "#878787"
   set Viewport::Resources(Lake)      "#0000ff"
   set Viewport::Resources(River)     "#0000ff"
   set Viewport::Resources(Polit)     "#ff0000"
   set Viewport::Resources(Admin)     "#ff0000"
   set Viewport::Resources(City)      "#ffa500"
   set Viewport::Resources(Road)      "#404040"
   set Viewport::Resources(Rail)      "#ff1493"
   set Viewport::Resources(Util)      "#ffff00"
   set Viewport::Resources(Canal)     "#00ffff"
   set Viewport::Resources(Coord)     "#000000"

   set Data(Viewport001) [Viewport::Create $Frame 1 1 1585 1076 1 1]

   Page::UpdateItems $Frame
}

proc VerifQPF24::LayoutUpdate { Frame } {
   SPI::LayoutUpdate $Frame
}
