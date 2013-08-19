namespace eval Urban4Panel { }

proc Urban4Panel::Layout { Frame } {
   variable Data

   Page::Size $Frame 1215 815

   projcam create TOP
   projcam create 3D
   projcam create EAST
   projcam create SOUTH

   projcam configure TOP   -lens 12944.036853569572 -to { 0.0 0.0 1.0 } -from { 0.0 0.0 2.0 } -up { 0.0 1.0 0.0 }
   projcam configure 3D    -lens 22944.036853569572 -to { 0.0 0.0 1.0 } -from {-0.09192869798270249 -0.9375613893710083 1.335451569750254} -up {0.01602562509718757 0.33544141721531495 0.941924750156203}
   projcam configure EAST  -lens 22944.036853569572 -to { 0.0 0.0 1.0 } -from {0.994140263024224 -0.09037638788641261 1.0593063735759616} -up {-0.0593312445412697 0.0024137320257514748 0.9982354318089957}
   projcam configure SOUTH -lens 22944.036853569572 -to { 0.0 0.0 1.0 } -from {-0.07658398943579398 -0.9952977678139174 1.0593063735759616} -up {0.0015908749042470981 0.059359007725425475 0.9982354318089957}

   #----- Affichage des Viewports

   set Viewport::Map(Type)        "orthographic"
   set Viewport::Map(Data)        ""
   set Viewport::Map(Elev)        "1.0"
   set Viewport::Map(Delay)       1000
   set Viewport::Map(Damping)     1.070
   set Viewport::Map(Coord)       0
   set Viewport::Map(CoordDef)    10.0
   set Viewport::Map(CoordNum)    2
   set Viewport::Map(Res)         0
   set Viewport::Map(Mask)        0
   set Viewport::Map(Coast)       0
   set Viewport::Map(Lake)        0
   set Viewport::Map(River)       0
   set Viewport::Map(Admin)       0
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

#   set Data(VP3D)    [Viewport::Create $Frame 5 5 600 400 1 0]
#   set Data(VPTOP)   [Viewport::Create $Frame 610 5 600 400 1 0]
#   set Data(VPSOUTH) [Viewport::Create $Frame 5 410 600 400 1 0]
#   set Data(VPEAST)  [Viewport::Create $Frame 610 410 600 400 1 0]

   set Data(VPTOP)   [Viewport::Create $Frame 5 5 1205 805 1 0]
   $Page::Data(Canvas) create rectangle 0 0 310 310 -fill white -width 0
   $Page::Data(Canvas) create line 310 5 310 310 5 310 -fill black -width 1
   set Data(VP3D)    [Viewport::Create $Frame 5 5 300 300 1 0]

#   $Page::Data(Canvas) itemconfigure $Data(VPTOP) -camera TOP
#   $Page::Data(Canvas) itemconfigure $Data(VP3D) -camera 3D
#   $Page::Data(Canvas) itemconfigure $Data(VPEAST) -camera EAST
#   $Page::Data(Canvas) itemconfigure $Data(VPSOUTH) -camera SOUTH

   glrender -zbuffer True

   georef create VANREF
   georef define VANREF -projection {PROJCS["NAD83 UTM, Zone 10 North, Meter",GEOGCS["WGS 84",DATUM["WGS_1984",SPHEROID["WGS 84",6378137,298.257223563]],PRIMEM["Greenwich",0],UNIT["Degree",0.017453292519943295]],PROJECTION["Transverse_Mercator"],PARAMETER["latitude_of_origin",0.0],PARAMETER["central_meridian",-123.0],PARAMETER["scale_factor",0.9996],PARAMETER["false_easting",8193],PARAMETER["false_northing",-5459442.0],UNIT["Meter",1]]}

   model read VANC3D /data/cmoe/afsr005/Data/3DModel/Vancouver/CRTI_Vanc_Mod_Final5_Apr16_BLDG_ONLY.flt
   model configure VANC3D -rendertexture 0
   model define VANC3D -georef VANREF
   Mapper::UpdateData $Page::Data(Frame) VANC3D
   Mapper::Read /data/cmoe/afsr005/Data/3DModel/Vancouver/Images_10cm/Vancouver_10cm.tif

#   Page::UpdateItems $Frame
}

proc Urban4Panel::LayoutUpdate { Frame } {
   SPI::LayoutUpdate $Frame
}
