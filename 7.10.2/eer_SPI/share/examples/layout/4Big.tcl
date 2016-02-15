namespace eval 4Big { }

proc 4Big::Layout { Frame } {
   variable Data

   Page::Size $Frame 1280 1024

   #----- Affichage des Viewports
   set Viewport::Map(Type) grid

   set Data(Viewport001) [Viewport::Create $Frame 100.0 95.0 530.0 450.0 1 0]
   set Data(Viewport002) [Viewport::Create $Frame 650.0 95.0 530.0 450.0 1 0]
   set Data(Viewport003) [Viewport::Create $Frame 100.0 565.0 530.0 450.0 1 0]
   set Data(Viewport004) [Viewport::Create $Frame 650.0 565.0 530.0 450.0 1 0]

   #-----  Positionnement des ColorBars
   set ColorBar::Data(Active$Frame) 1
   set vp [Page::Registered $Frame Viewport]
   set ColorBar::Data(${Data(Viewport001)}0) [list 10.0 95.0 80 451 CBVP6ES]
   set ColorBar::Data(${Data(Viewport002)}0) [list 1190.0 95.0 80 451 CBVP7ES]
   set ColorBar::Data(${Data(Viewport003)}0) [list 10.0 565.0 80 451 CBVP8TT]
   set ColorBar::Data(${Data(Viewport004)}0) [list 1190.0 565.0 80 451 CBVP9TT]

   set SPI::Data(ShowColorBar$Frame) 1

   Page::UpdateItems $Frame
}

proc 4Big::LayoutUpdate { Frame } {
   SPI::LayoutUpdate $Frame
}
