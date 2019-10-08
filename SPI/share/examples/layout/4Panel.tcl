namespace eval 4Panel { }

proc 4Panel::Layout { Frame } {
   variable Data

   #----- Parametres des Viewports
   set Viewport::Map(Type)        "orthographic"
   set Viewport::Map(Data)        ""
   set Viewport::Map(Elev)        "1.0"
   set Viewport::Map(Delay)       1000
   set Viewport::Map(Damping)     1.070
   set Viewport::Map(Coord)       1
   set Viewport::Map(CoordDef)    10.0
   set Viewport::Map(CoordNum)    2
   set Viewport::Map(CoordLoc)    -1
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
   set Viewport::Resources(FillCoast) "#f9f9f9"
   set Viewport::Resources(FillLake)  ""
   set Viewport::Resources(Coast)     "#000000"
   set Viewport::Resources(Lake)      "#0000ff"
   set Viewport::Resources(River)     "#0000ff"
   set Viewport::Resources(Place)     "#000000"
   set Viewport::Resources(Polit)     "#cacaca"
   set Viewport::Resources(Admin)     "#cacaca"
   set Viewport::Resources(City)      "#000000"
   set Viewport::Resources(Road)      "#404040"
   set Viewport::Resources(Rail)      "#ff1493"
   set Viewport::Resources(Coord)     "#000000"

   Page::Size $Frame 0 0
   
   Viewport::Rotate $Frame 61.5248 -107.4441 1.9185 

   #----- Affichage des Viewports
   set Data(Viewport001) [Viewport::Create $Frame 1 1 1 1 0 0]
   set Data(Viewport002) [Viewport::Create $Frame 1 1 1 1 0 0]
   set Data(Viewport003) [Viewport::Create $Frame 1 1 1 1 0 0]
   set Data(Viewport004) [Viewport::Create $Frame 1 1 1 1 0 0]

   #-----  Affichage des ColorBars
   $Page::Data(Canvas) create colorbar -x 0 -y 0 -width 60 -height 250 -anchor se -barside left -barborder 1 -barwidth 10 -transparency 75 -showfactor False -tags CB500
   $Page::Data(Canvas) create colorbar -x 0 -y 0 -width 60 -height 250 -anchor se -barside left -barborder 1 -barwidth 10 -transparency 75 -showfactor False -tags CB1000
   $Page::Data(Canvas) create colorbar -x 0 -y 0 -width 60 -height 250 -anchor se -barside left -barborder 1 -barwidth 10 -transparency 75 -showfactor False -tags CB700
   $Page::Data(Canvas) create colorbar -x 0 -y 0 -width 60 -height 250 -anchor se -barside left -barborder 1 -barwidth 10 -transparency 75 -showfactor False -tags CB850

   LayoutFit $Frame
   
   Page::UpdateItems $Frame
}

proc 4Panel::LayoutFit { Frame } {
   variable Data

   set w [winfo width $Frame.page.canvas]
   set h [winfo height $Frame.page.canvas]
   
   set vw [expr ($w-5.0)/2.0]
   set vh [expr ($h-5.0)/2.0]
   
   $Frame.page.canvas itemconfigure $4Panel::Data(Viewport001) -x 1            -y 1            -width $vw -height $vh
   $Frame.page.canvas itemconfigure $4Panel::Data(Viewport002) -x [expr $vw+5] -y 1            -width $vw -height $vh
   $Frame.page.canvas itemconfigure $4Panel::Data(Viewport003) -x 1            -y [expr $vh+5] -width $vw -height $vh
   $Frame.page.canvas itemconfigure $4Panel::Data(Viewport004) -x [expr $vw+5] -y [expr $vh+5] -width $vw -height $vh
      
   $Frame.page.canvas itemconfigure CB500  -x [expr $vw+1]      -y [expr $vh+1]
   $Frame.page.canvas itemconfigure CB1000 -x [expr $vw+$vw+5]  -y [expr $vh+1]
   $Frame.page.canvas itemconfigure CB700  -x [expr $vw+1]      -y [expr $vh+$vh+5]
   $Frame.page.canvas itemconfigure CB850  -x [expr $vw+$vw+5]  -y [expr $vh+$vh+5]
   
}
proc 4Panel::LayoutUpdate { Frame } {
   SPI::LayoutUpdate $Frame
}
