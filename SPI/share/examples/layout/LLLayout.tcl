namespace eval LLLayout { }

proc LLLayout::Layout { Frame } {
   variable Data

   #----- Affichage des Viewports
   set Viewport::Map(Type)        "cylindric"
   set Viewport::Map(Data)        ""
   set Viewport::Map(Elev)        1.0
   set Viewport::Map(Delay)       1000
   set Viewport::Map(Damping)     1.070
   set Viewport::Map(Coord)       1
   set Viewport::Map(CoordDef)    20.0
   set Viewport::Map(CoordNum)    2
   set Viewport::Map(Res)         0
   set Viewport::Map(Mask)        0
   set Viewport::Map(Coast)       1
   set Viewport::Map(Lake)        1
   set Viewport::Map(River)       0
   set Viewport::Map(Admin)       1
   set Viewport::Map(Polit)       1
   set Viewport::Map(Place)       0
   set Viewport::Map(Road)        0
   set Viewport::Map(Rail)        0
   set Viewport::Map(Topo)        0
   set Viewport::Map(Bath)        0
   set Viewport::Map(Text)        0
   
   set Viewport::Resources(Bkg)       "white"
   set Viewport::Resources(FillCoast) "#F3F3F3"
   set Viewport::Resources(FillLake)  "#DCF6FF"
   set Viewport::Resources(Coast)     "#000000"
   set Viewport::Resources(Lake)      "#0000ff"
   set Viewport::Resources(River)     "#0000ff"
   set Viewport::Resources(Polit)     "#ff0000"
   set Viewport::Resources(Place)     "#000000"
   set Viewport::Resources(Admin)     "#ff0000"
   set Viewport::Resources(City)      "#000000"
   set Viewport::Resources(Road)      "#404040"
   set Viewport::Resources(Rail)      "#ff1493"
   set Viewport::Resources(Coord)     "#000000"

   Page::Size $Frame 1105 502
   set Data(Viewport001) [Viewport::Create $Frame 105 1 1000 500 0 0]

   #-----  Positionnement des ColorBars
   set ColorBar::Data(Active$Frame) 1
   set vp [Page::Registered $Frame Viewport]
   set ColorBar::Data(${Data(Viewport001)}0) [list 1 1 100 500 CBVP1TT:VP10]

   set SPI::Data(ShowColorBar$Frame) 1

   Page::UpdateItems $Frame
}
