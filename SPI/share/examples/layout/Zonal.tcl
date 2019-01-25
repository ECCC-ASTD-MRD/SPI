namespace eval Zonal { }

proc Zonal::Layout { Frame } {
   variable Data

   Page::Size $Frame 1602 502 0

   #----- Affichage des Viewports

   set Viewport::Map(Type)        "cylindric"
   set Viewport::Map(Lat)         0.0
   set Viewport::Map(Lon)         0.0
   set Viewport::Map(Perspective) "0"
   set Viewport::Map(Data)        ""
   set Viewport::Map(Elev)        "1.0"
   set Viewport::Map(Delay)       1000.0
   set Viewport::Map(Damping)     1.07
   set Viewport::Map(Coord)       0
   set Viewport::Map(CoordLoc)    1
   set Viewport::Map(CoordDef)    10.0
   set Viewport::Map(CoordNum)    2
   set Viewport::Map(Res)         0
   set Viewport::Map(Mask)        NONE
   set Viewport::Map(Coast)       1
   set Viewport::Map(Lake)        1
   set Viewport::Map(River)       0
   set Viewport::Map(Admin)       0
   set Viewport::Map(City)        0
   set Viewport::Map(Polit)       0
   set Viewport::Map(Place)       0
   set Viewport::Map(Road)        0
   set Viewport::Map(Rail)        0
   set Viewport::Map(Topo)        0
   set Viewport::Map(Bath)        0
   set Viewport::Map(Text)        0
   set Viewport::Resources(Bkg)       "white"
   set Viewport::Resources(FillCoast) ""
   set Viewport::Resources(FillLake)  ""
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
   set Data(Viewport001) [Viewport::Create $Frame 1 1 1000 500 1 0]

   #----- Affichage des graphs Time
   $Page::Data(Canvas) create graph -x 1100 -y 1 -width 500 -height 500 -xlegend 10 -ylegend 10 -anchor nw -fg black -fill #F2F2F2 -bg white -tags GRAPH -font XFont12

   #-----  Positionnement des ColorBars
   set ColorBar::Data(Active$Frame) 1
   set ColorBar::Data(${Data(Viewport001)}0) [list 1005 1 90 500 CBVP1PR:VP10 0 white 100 0 True 0 15 right]

   set SPI::Data(ShowColorBar$Frame) 1
   Page::UpdateItems $Frame
}

proc Zonal::LayoutUpdate { Frame } {
   SPI::LayoutUpdate $Frame
}
