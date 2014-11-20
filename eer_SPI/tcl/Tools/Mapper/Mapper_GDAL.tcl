#===============================================================================
# Environnement Canada
# Centre Meteorologique Canadian
# 2100 Trans-Canadienne
# Dorval, Quebec
#
# Projet   : Fonctions de manipulation des donnees geographiques.
# Fichier  : Mapper_GDAL.tcl
# Creation : Janvier 2014 - J.P.Gauthier - CMC/CMOE
#
# Description:
#    Fontions de manipulation de donnees georeference OGR.
#
# Remarques :
#
#===============================================================================

namespace eval Mapper::GDAL { } {
   variable Data

   set Data(Styles)      {}
   set Data(Style)       ""
   set Data(Colors)      { RED GREEN BLUE ALPHA }
   set Data(Topos)       { NONE INTERNAL }
   set Data(Curve)       LINEAR
   set Data(Curves)      { EXPONENTIAL CUBIC SQUARE LINEAR SQUAREROOT CUBICROOT LOGARITHMIC QUADRATIC STEP16 STEP32 STEP64 STEP128 }
   set Data(Stretch)     ""
   set Data(Stretchs)    { MIN_MAX "PERCENT_CLIP 2 98" "PERCENT_CLIP 5 95" "PERCENT_CLIP 10 90" "STANDARD_DEV 1" "STANDARD_DEV 2" }
   set Data(Interp)      NEAREST
   set Data(Interps)     { NEAREST LINEAR }
   set Data(Mask)        False
   set Data(Resolution)  1
   set Data(Color)       black
   set Data(Lock)        False
   set Data(HistoShow)   False
   set Data(NoData)      ""

   set Data(Ops)     { }

   set Data(Formats) {
      {WMO GRIdded Binary GRIB1/GRIB2 {*.grb}}
      {USGS DOQ 1 & 2 {*.doq}}
      {USGS SDTS DEM {*.catd *.ddf}}
      {USGS ASCII DEM {*.dem}}
      {USGS LULC Composite Theme Grid {*.gz}}
      {USGS Astrogeology ISIS cube (Version 2 and 3) {*.lbl *.cub}}
      {Virtual Raster {*.vrt}}
      {GeoTIFF {*.tif *.gtif}}
      {Graphics Interchange Format {*.gif}}
      {National Imagery Transmission Format {*.nitf}}
      {Raster Product Format/RPF {*.toc}}
      {ECRG Table Of Contents format {*.xml}}
      {Erdas Imagine Images {*.img}}
      {Erdas 7.x {*.lan *.gis}}
      {Erdas Imagine Raw {*.raw}}
      {CEOS SAR Image {*}}
      {Arc/Info Binary Grid {*.adf}}
      {Arc/Info ASCII Grid {*}}
      {GRASS ASCII Grid {*}}
      {SDTS Raster {*.ddf}}
      {DTED Elevation Raster {*.dem}}
      {Portable Network Graphics {*.png}}
      {JPEG and JPEG2000 {*.jpg *.jp2 *.j2k}}
      {Netpbm {*.pgm *.ppm}}
      {Japanese DEM {*.mem}}
      {Maptech/NOAA BSB Nautical Chart Format {*.kap}}
      {X11 Pixmap {*.xpm}}
      {MS Windows Device Independent Bitmap {*.bmp}}
      {SPOT DIMAP {*.dim}}
      {RadarSat2 XML {*.xml}}
      {PCIDSK Database File {*.pci}}
      {PCRaster Raster File {*.map}}
      {ILWIS Raster Map {*.mpr}}
      {SGI Image File Format 1.0 {*.sgi}}
      {SRTM HGT Format {*.hgt}}
      {Leveller heightfield {*.ter}}
      {Terragen Heightfield {*.ter}}
      {Network Common Data Format {*.cdf}}
      {EarthWatch {*.til}}
      {ERMapper {*.ers}}
      {NOAA Polar Orbiter Level 1b Data Set {*.l1b}}
      {Raster Matrix Format {*.rsw *.mtw}}
      {EUMETSAT Archive native {*.nat}}
      {MSG HRIT Data {*.msg}}
      {Idrisi Raster {*.rst}}
      {Intergraph Raster {*}}
      {Golden Software ASCII and binary Grid File Format {*.gsag *.gsbg *.gs7bg}}
      {TerraSAR-X Product {*.tsx}}
      {R Object Data Store {*.r}}
      {OziExplorer {*.map}}
      {ENVI .hdr Labelled {*.hdr}}
      {ESRI .hdr Labelled {*.hdr}}
      {Atlantis MFF and MFF2 Raster {*.mff}}
      {EOSAT FAST Format {*.fst}}
      {VTP Binary Terrain Format {*.bt}}
      {Convair PolGASP {*.cpg}}
      {Image Display and Analysis {*.ida}}
      {NLAPS Data Format {*.h1 *.h2 *.hd}}
      {FARSITE v.4 Landscape File {*.lcp}}
      {NOAA Vertical Datum {*.gtx}}
      {NADCON Datum Grid Shift {*.los *.las}}
      {NTv2 Datum Grid Shift {*}}
      {ACE2 {*.dem}}
      {Snow Data Assimilation System {*.hdr}}
      {Azavea Raster Grid format {*.arg}}
      {Swedish Grid {*.rik}}
      {GeoSoft Grid Exchange Format {*.gxf}}
      {Bathymetry Attributed Grid {*.bag}}
      {HDF 4-5 Dataset {*.hdf}}
      {Northwood Numeric/Classified Grid Format {*.grc *.grd *.tab}}
      {ARC Digitized Raster Graphics {*.gen *.thf}}
      {Standard Raster Product (ASRP/USRP) {*.img}}
      {Magellan BLX Topo {*.blx *.xlb}}
      {Rasterlite {*.sqlite}}
      {SAGA GIS Binary Grid {*.sdat}}
      {Kml Super Overlay {*.kml}}
      {ASCII Gridded XYZ {*.xyz}}
      {HF2/HFZ heightfield raster {*.hf2,*.hfz}}
      {OziExplorer Image File {*.map}}
      {Arc/Info Export E00 GRID {*.e00}}
      {ZMap Plus Grid {*.zmap}}
      {NOAA NGS Geoid Height Grids {*.bin}}
      {MBTiles {*.mbtile}}
      {IRIS data {*.ppi *.cappi *.rain1 *.rainn *.tops *.vil *.max}}}

   set Data(WriteFormats) {
      { AAIGrid {*.grid}}
      { GTiff {*.tif *.gtif}}
      { HDF4 {*.hdf}}
      { netCDF {*.cdf}}
      { PDF {*.pdf}}
      { PNG {*.png}}
      { PNM {*.ppm}}
      { BMP {*.bmp}}
      { JPEG {*.jpg}}
      { JPEG2000 {*.jpg}}
      { DTED {*.dem}}
      { EHdr {*.hdr}}
      { ENVI {*.hdr}}
      { ERS  {*.ers}}
      { GMT  {*.cdf}}
      { USGSDEM {*.dem}}
      { VRT {*.vrt}}
      { XYZ {*.xyz}}}

   catch {
      image create photo GDALMAPImg -width 250 -height 15
   }
}

#-------------------------------------------------------------------------------
# Nom      : <Mapper::GDAL::Params>
# Creation : Juillet 2004 - J.P. Gauthier - CMC/CMOE
#
# But      : Parametres des objets de type GDAL
#
# Parametres :
#   <Object> : Donnee geographique a parametrer
#   <Tabs>   : Onglet par defaut
#
# Retour    :
#
# Remarque :
#
#-------------------------------------------------------------------------------

proc Mapper::GDAL::Params { Object { Tabs {} } } {
   global   GDefs
   variable Data

   if { [winfo exists .mapperparams] && $Mapper::Data(Mode)!="gdalband" } {
      set pos [wm geometry .mapperparams]
      destroy .mapperparams
   } else {
      set pos 530x475
   }

   set Mapper::Data(Mode)     gdalband
   set Mapper::Data(RealTime) [expr $OpenGL::Param(Res)<=1]

   if { ![winfo exists .mapperparams] } {
      toplevel .mapperparams

      wm title     .mapperparams "[lindex $Mapper::Lbl(GDAL) $GDefs(Lang)]"
      wm transient .mapperparams .
      wm geometry  .mapperparams =$pos
      wm protocol  .mapperparams WM_DELETE_WINDOW { Mapper::ParamsClose }

      TabFrame::Create .mapperparams.tab 1 Mapper::GDAL::TabSelect
      pack .mapperparams.tab -side top -fill both -expand true -padx 2 -pady 2

      set Data(Frame2) [TabFrame::Add .mapperparams.tab 1 [lindex $Mapper::Lbl(Display) $GDefs(Lang)] False ""]

         labelframe $Data(Frame2).col -text [lindex $Mapper::Lbl(ColorRef) $GDefs(Lang)]
            frame $Data(Frame2).col.sel
               checkbutton $Data(Frame2).col.sel.lock -image VCRLOCK -relief sunken -bd 1 -overrelief raised -offrelief flat -width 2 \
                  -variable Mapper::GDAL::Data(Lock) -indicatoron False -onvalue True -offvalue False \
                  -command { Mapper::GDAL::CurveDefine $Mapper::Data(Object) $Mapper::GDAL::Data(Bands$Mapper::Data(Object)) }
               radiobutton $Data(Frame2).col.sel.red -relief sunken -bd 1 -overrelief raised -offrelief flat -width 2 -bg red \
                  -value red -variable Mapper::GDAL::Data(Band) -indicatoron False -selectcolor red \
                  -command { Mapper::GDAL::CurveSelect $Mapper::GDAL::Data(Frame2).col.curve.cv $Mapper::Data(Object) $Mapper::GDAL::Data(Band) }
               radiobutton  $Data(Frame2).col.sel.green -relief sunken -bd 1 -overrelief raised -offrelief flat -width 2 -bg green \
                  -value green -variable Mapper::GDAL::Data(Band) -selectcolor green -indicatoron False \
                  -command { Mapper::GDAL::CurveSelect $Mapper::GDAL::Data(Frame2).col.curve.cv $Mapper::Data(Object) $Mapper::GDAL::Data(Band) }
               radiobutton  $Data(Frame2).col.sel.blue -relief sunken -bd 1 -overrelief raised -offrelief flat -width 2 -bg blue \
                  -value blue -variable Mapper::GDAL::Data(Band) -selectcolor blue -indicatoron False \
                  -command { Mapper::GDAL::CurveSelect $Mapper::GDAL::Data(Frame2).col.curve.cv $Mapper::Data(Object) $Mapper::GDAL::Data(Band) }
               radiobutton  $Data(Frame2).col.sel.alpha -relief sunken -bd 1 -overrelief raised -offrelief flat -width 2 -bg white \
                  -value alpha -variable Mapper::GDAL::Data(Band) -selectcolor white -indicatoron False \
                  -command { Mapper::GDAL::CurveSelect $Mapper::GDAL::Data(Frame2).col.curve.cv $Mapper::Data(Object) $Mapper::GDAL::Data(Band) }
               pack  $Data(Frame2).col.sel.lock -side top -padx 5 -fill x
               pack $Data(Frame2).col.sel.red $Data(Frame2).col.sel.green $Data(Frame2).col.sel.blue \
                  $Data(Frame2).col.sel.alpha -side top -fill y -expand true -padx 5 -pady 5

            frame $Data(Frame2).col.def
               frame $Data(Frame2).col.def.stretch
                  label $Data(Frame2).col.def.stretch.lbl -text [lindex $Mapper::Lbl(Stretch) $GDefs(Lang)] -width 14 -anchor w
                  ComboBox::Create $Data(Frame2).col.def.stretch.sel Mapper::GDAL::Data(Stretch) noedit unsorted nodouble -1 $Data(Stretchs) 1 8 { Mapper::GDAL::CurveSelect $Mapper::GDAL::Data(Frame2).col.curve.cv $Mapper::Data(Object) $Mapper::GDAL::Data(Band) }
                  pack $Data(Frame2).col.def.stretch.lbl -side left
                  pack $Data(Frame2).col.def.stretch.sel -side left -fill x -expand True
               frame $Data(Frame2).col.def.curve
                  label $Data(Frame2).col.def.curve.lbl -text [lindex $Mapper::Lbl(Curve) $GDefs(Lang)] -width 14 -anchor w
                  ComboBox::Create $Data(Frame2).col.def.curve.sel Mapper::GDAL::Data(Curve) noedit unsorted nodouble -1 $Data(Curves) 1 8 { Mapper::GDAL::CurveSet $Mapper::GDAL::Data(Frame2).col.curve.cv $Mapper::Data(Object) }
                  pack $Data(Frame2).col.def.curve.lbl -side left
                  pack $Data(Frame2).col.def.curve.sel -side left -fill x -expand True
                frame $Data(Frame2).col.def.invert
                  label $Data(Frame2).col.def.invert.lbl -text [lindex $Mapper::Lbl(Invert) $GDefs(Lang)] -width 14 -anchor w
                  checkbutton $Data(Frame2).col.def.invert.x -text " X " -width 1 -anchor w -indicatoron 0 \
                     -relief sunken -bd 1 -overrelief raised -offrelief flat -selectcolor $GDefs(ColorFrame) -variable Mapper::GDAL::Data(CurveInvertX) \
                     -onvalue 1 -offvalue 0 -command { Mapper::GDAL::CurveInvert $Mapper::GDAL::Data(Frame2).col.curve.cv $Mapper::Data(Object) $Mapper::GDAL::Data(Band) X $Mapper::GDAL::Data(CurveInvertX) }
                     pack $Data(Frame2).col.def.curve.lbl -side left
                  checkbutton $Data(Frame2).col.def.invert.y -text " Y " -width 1 -anchor w -indicatoron 0 \
                     -relief sunken -bd 1 -overrelief raised -offrelief flat -selectcolor $GDefs(ColorFrame) -variable Mapper::GDAL::Data(CurveInvertY) \
                     -onvalue 1 -offvalue 0 -command { Mapper::GDAL::CurveInvert $Mapper::GDAL::Data(Frame2).col.curve.cv $Mapper::Data(Object) $Mapper::GDAL::Data(Band) Y $Mapper::GDAL::Data(CurveInvertY) }
                  pack $Data(Frame2).col.def.invert.lbl -side left
                  pack $Data(Frame2).col.def.invert.x $Data(Frame2).col.def.invert.y -side left -fill x -expand True
              frame $Data(Frame2).col.def.tran
                  label $Data(Frame2).col.def.tran.lbl -text [lindex $Mapper::Lbl(Tran) $GDefs(Lang)] -width 14 -anchor w
                  label $Data(Frame2).col.def.tran.txt -textvariable Mapper::GDAL::Data(Tran) -width 3 -anchor w -relief sunken -bd 1
                  scale $Data(Frame2).col.def.tran.val -bd 1 -relief flat -width 15 -sliderlength 10 -from 0 -to 100 -variable Mapper::GDAL::Data(Tran) -orient horizontal \
                     -showvalue False -command { if { $Mapper::Data(RealTime) } { Mapper::GDAL::ParamsSet $Mapper::Data(Object) }; catch }
                  pack $Data(Frame2).col.def.tran.lbl $Data(Frame2).col.def.tran.txt -side left
                  pack $Data(Frame2).col.def.tran.val -side left -fill x -expand True
              frame $Data(Frame2).col.def.res
                  label $Data(Frame2).col.def.res.lbl -text [lindex $Mapper::Lbl(Resolution) $GDefs(Lang)] -width 14 -anchor w
                  label $Data(Frame2).col.def.res.txt -textvariable Mapper::GDAL::Data(Resolution) -width 3 -anchor w -relief sunken -bd 1
                  scale $Data(Frame2).col.def.res.val -bd 1 -relief flat -width 15 -sliderlength 10 -from 1 -to 8 -variable Mapper::GDAL::Data(Resolution) -orient horizontal -resolution 1\
                     -showvalue False -command { if { $Mapper::Data(RealTime) } { Mapper::GDAL::ParamsSet $Mapper::Data(Object) }; catch }
                  pack $Data(Frame2).col.def.res.lbl $Data(Frame2).col.def.res.txt -side left
                  pack $Data(Frame2).col.def.res.val -side left -fill x -expand True
               frame $Data(Frame2).col.def.interp
                  label $Data(Frame2).col.def.interp.lbl -text [lindex $Mapper::Lbl(Interp) $GDefs(Lang)] -width 14 -anchor w
                  ComboBox::Create $Data(Frame2).col.def.interp.val Mapper::GDAL::Data(Interp) noedit sorted nodouble -1 $Data(Interps) 1 8 "if { \$Mapper::Data(RealTime) } { Mapper::GDAL::ParamsSet \$Mapper::Data(Object) }"
                  pack $Data(Frame2).col.def.interp.lbl -side left
                  pack $Data(Frame2).col.def.interp.val -side left -fill x -expand True
               frame $Data(Frame2).col.def.nodata
                  label $Data(Frame2).col.def.nodata.lbl -text [lindex $Mapper::Lbl(NoData) $GDefs(Lang)] -width 14 -anchor w
                  entry $Data(Frame2).col.def.nodata.sel -textvariable Mapper::GDAL::Data(NoData) -bd 1 -bg $GDefs(ColorLight)
                  pack $Data(Frame2).col.def.nodata.lbl -side left
                  pack $Data(Frame2).col.def.nodata.sel -side left -fill x -expand True
               frame $Data(Frame2).col.def.style
                  label $Data(Frame2).col.def.style.lbl -text [lindex $Mapper::Lbl(Style) $GDefs(Lang)] -width 14 -anchor w
                  ComboBox::Create $Data(Frame2).col.def.style.sel Mapper::GDAL::Data(Style) noedit unsorted nodouble -1 $Data(Styles) 1 8 { Mapper::DepotWare::WMS::ReLoad $Mapper::Data(Object) $Mapper::GDAL::Data(Style) }
                  pack $Data(Frame2).col.def.style.lbl -side left
                  pack $Data(Frame2).col.def.style.sel -side left -fill x -expand True
               pack $Data(Frame2).col.def.stretch $Data(Frame2).col.def.curve $Data(Frame2).col.def.invert $Data(Frame2).col.def.tran $Data(Frame2).col.def.res \
                   $Data(Frame2).col.def.interp $Data(Frame2).col.def.nodata -side top -fill x
            frame $Data(Frame2).col.curve
               button $Data(Frame2).col.curve.map -bd 2 -relief groove -image GDALMAPImg \
                  -command  { MapBox::Create $Mapper::GDAL::Data(Frame2).col.curve.map "Mapper::Apply \$Mapper::Data(Object)" $Mapper::GDAL::Data(ColorMap) }
               canvas $Data(Frame2).col.curve.cv -relief sunken -bd 1 -bg $GDefs(ColorLight) -width 256 -height 128
               frame $Data(Frame2).col.curve.info
                  entry $Data(Frame2).col.curve.info.min -textvariable Mapper::GDAL::Data(CurveMinD) -width 8 -bd 1
                  entry $Data(Frame2).col.curve.info.now -textvariable Mapper::GDAL::Data(CurveValue) -width 1 -relief sunken -bd 1
                  label $Data(Frame2).col.curve.info.nb -textvariable Mapper::GDAL::Data(CurveNb) -width 6 -relief sunken -bd 1
                  entry $Data(Frame2).col.curve.info.max -textvariable Mapper::GDAL::Data(CurveMaxD) -width 8 -bd 1
                  pack $Data(Frame2).col.curve.info.min -side left
                  pack $Data(Frame2).col.curve.info.now -side left -fill x -expand True
                  pack $Data(Frame2).col.curve.info.nb -side left
                  pack $Data(Frame2).col.curve.info.max -side left
               frame $Data(Frame2).col.curve.limit
                  entry $Data(Frame2).col.curve.limit.min -textvariable Mapper::GDAL::Data(CurveMin) -width 8 -state disabled -bd 1
                  entry $Data(Frame2).col.curve.limit.max -textvariable Mapper::GDAL::Data(CurveMax) -width 8 -state disabled -bd 1
                  checkbutton $Data(Frame2).col.curve.limit.histo -variable Mapper::GDAL::Data(HistoShow) -onvalue True -offvalue False \
                     -text [lindex $Mapper::Lbl(Histo) $GDefs(Lang)] -relief sunken -bd 1 -overrelief raised -offrelief flat \
                     -indicatoron False -command { Mapper::GDAL::CurveHisto $Mapper::Data(Object) }
                  pack $Data(Frame2).col.curve.limit.min -side left
                  pack $Data(Frame2).col.curve.limit.histo -side left -fill x -expand True
                  pack $Data(Frame2).col.curve.limit.max -side right
               pack $Data(Frame2).col.curve.map -side top -fill x
               pack $Data(Frame2).col.curve.cv -side top -fill both
               pack $Data(Frame2).col.curve.info $Data(Frame2).col.curve.limit -side top -fill x
            pack $Data(Frame2).col.curve -side left -padx 5
            pack $Data(Frame2).col.sel -side left -fill y
            pack $Data(Frame2).col.def -side left -padx 5 -fill x -anchor nw -expand True
         pack $Data(Frame2).col -side top -fill x -padx 5 -pady 5 -ipady 5

         labelframe $Data(Frame2).band -text [lindex $Mapper::Lbl(Band) $GDefs(Lang)]
            frame $Data(Frame2).band.red
               label $Data(Frame2).band.red.lbl -text [lindex $Mapper::Lbl(Red) $GDefs(Lang)] -width 6 -anchor w
               ComboBox::Create $Data(Frame2).band.red.val Mapper::GDAL::Data(Red) edit sorted nodouble -1 "" 1 8 ""
               pack $Data(Frame2).band.red.lbl -side left
               pack $Data(Frame2).band.red.val -side left -fill x -expand true
            frame $Data(Frame2).band.green
               label $Data(Frame2).band.green.lbl -text [lindex $Mapper::Lbl(Green) $GDefs(Lang)] -width 6 -anchor w
               ComboBox::Create $Data(Frame2).band.green.val Mapper::GDAL::Data(Green) edit sorted nodouble -1 "" 1 8 ""
               pack $Data(Frame2).band.green.lbl -side left
               pack $Data(Frame2).band.green.val -side left -fill x -expand true
            frame $Data(Frame2).band.blue
               label $Data(Frame2).band.blue.lbl -text [lindex $Mapper::Lbl(Blue) $GDefs(Lang)] -width 6 -anchor w
               ComboBox::Create $Data(Frame2).band.blue.val Mapper::GDAL::Data(Blue) edit sorted nodouble -1 "" 1 8 ""
               pack $Data(Frame2).band.blue.lbl -side left
               pack $Data(Frame2).band.blue.val -side left -fill x -expand true
            frame $Data(Frame2).band.alpha
               label $Data(Frame2).band.alpha.lbl -text [lindex $Mapper::Lbl(Alpha) $GDefs(Lang)] -width 6 -anchor w
               ComboBox::Create $Data(Frame2).band.alpha.val Mapper::GDAL::Data(Alpha) edit sorted nodouble -1 "" 1 8 ""
               pack $Data(Frame2).band.alpha.lbl -side left
               pack $Data(Frame2).band.alpha.val -side left -fill x -expand true
            pack $Data(Frame2).band.red -side top -fill x -padx 5
            pack $Data(Frame2).band.green -side top -fill x -padx 5
            pack $Data(Frame2).band.blue -side top -fill x -padx 5
            pack $Data(Frame2).band.alpha -side top -fill x -padx 5
         pack $Data(Frame2).band -side top -fill x -padx 5 -pady 5 -ipady 2

         labelframe $Data(Frame2).pos -text [lindex $Mapper::Lbl(Position) $GDefs(Lang)]
            frame $Data(Frame2).pos.topo
               label $Data(Frame2).pos.topo.lbl -text  [lindex $Mapper::Lbl(Topo) $GDefs(Lang)] -width 12 -anchor nw
               ComboBox::Create $Data(Frame2).pos.topo.sel Mapper::GDAL::Data(Topo) noedit unsorted nodouble -1 "" 1 8 ""
               label $Data(Frame2).pos.topo.x -text "x" -width 1 -anchor nw
               entry $Data(Frame2).pos.topo.fac  -bg $GDefs(ColorLight) -textvariable Mapper::GDAL::Data(TopoFactor) -bd 1 -width 5
               pack $Data(Frame2).pos.topo.lbl $Data(Frame2).pos.topo.fac $Data(Frame2).pos.topo.x -side left -fill both
               pack $Data(Frame2).pos.topo.sel -side left -fill x -expand true
            frame $Data(Frame2).pos.sample
               label $Data(Frame2).pos.sample.lbl -text [lindex $Mapper::Lbl(Sample) $GDefs(Lang)] -width 12 -anchor w
               label $Data(Frame2).pos.sample.txt -textvariable Mapper::GDAL::Data(Sample) -width 5 -anchor w -relief sunken -bd 1
               scale $Data(Frame2).pos.sample.val -bd 1 -relief flat -width 15 -sliderlength 10 -from 2 -to 32 -variable Mapper::GDAL::Data(Sample) -orient horizontal -showvalue False
               pack $Data(Frame2).pos.sample.lbl $Data(Frame2).pos.sample.txt  -side left
               pack $Data(Frame2).pos.sample.val -side left -fill x -expand true
            pack  $Data(Frame2).pos.topo $Data(Frame2).pos.sample -side top -fill x -padx 5
            frame $Data(Frame2).pos.crop
               label $Data(Frame2).pos.crop.lbl -text [lindex $Mapper::Lbl(Cut) $GDefs(Lang)] -width 12 -anchor w
               checkbutton $Data(Frame2).pos.crop.active -text [lindex $Mapper::Lbl(Clip) $GDefs(Lang)] -width 1 -anchor w -indicatoron 0 \
                  -relief sunken -bd 1 -overrelief raised -offrelief flat -selectcolor $GDefs(ColorFrame) -variable Mapper::GDAL::Data(Cut) \
                  -onvalue True -offvalue False -command { Mapper::GDAL::ParamsSet $Mapper::Data(Object) }
               checkbutton $Data(Frame2).pos.crop.show -text [lindex $Mapper::Lbl(Show) $GDefs(Lang)] -anchor w -indicatoron 0 \
                  -relief sunken -bd 1 -overrelief raised -offrelief flat -selectcolor $GDefs(ColorFrame) -variable Mapper::GDAL::Data(CutShow) \
                  -onvalue True -offvalue False -command { Mapper::UpdateItems }
               checkbutton $Data(Frame2).pos.crop.mode -variable Page::Data(ToolMode) -onvalue Mapper::Cutter -offvalue SPI \
                  -image ARROW -indicatoron 0 -relief sunken -bd 1 -overrelief raised -offrelief flat -selectcolor $GDefs(ColorFrame) \
                  -command { SPI::ToolMode $Page::Data(ToolMode) Draw True; set Mapper::GDAL::Data(CutShow) True }
               pack $Data(Frame2).pos.crop.lbl $Data(Frame2).pos.crop.mode $Data(Frame2).pos.crop.show -side left -fill y
               pack $Data(Frame2).pos.crop.active -side left -fill both -expand True
            pack $Data(Frame2).pos.crop -side top -fill x -padx 5
         pack $Data(Frame2).pos -side top -fill x -padx 5 -pady 5 -ipady 2

         Bubble::Create $Data(Frame2).col.curve.map $Mapper::Bubble(ColorMap)
         Bubble::Create $Data(Frame2).col.sel $Mapper::Bubble(CurveBand)
         Bubble::Create $Data(Frame2).col.def.curve $Mapper::Bubble(Curve)
         Bubble::Create $Data(Frame2).col.def.tran $Mapper::Bubble(Transparency)
         Bubble::Create $Data(Frame2).col.def.res $Mapper::Bubble(Resolution)
         Bubble::Create $Data(Frame2).col.def.interp $Mapper::Bubble(Interp)
         Bubble::Create $Data(Frame2).col.def.nodata $Mapper::Bubble(NoData)
         Bubble::Create $Data(Frame2).col.curve.info.min $Mapper::Bubble(CurveMin)
         Bubble::Create $Data(Frame2).col.curve.limit.histo  $Mapper::Bubble(Histo)
         Bubble::Create $Data(Frame2).col.curve.info.now $Mapper::Bubble(CurveNow)
         Bubble::Create $Data(Frame2).col.curve.info.nb $Mapper::Bubble(CurveNb)
         Bubble::Create $Data(Frame2).col.curve.info.max $Mapper::Bubble(CurveMax)
         Bubble::Create $Data(Frame2).col.curve.limit.min $Mapper::Bubble(CurveLimitMin)
         Bubble::Create $Data(Frame2).col.curve.limit.max $Mapper::Bubble(CurveLimitMax)
         Bubble::Create $Data(Frame2).band $Mapper::Bubble(Band)
         Bubble::Create $Data(Frame2).pos.topo.fac $Mapper::Bubble(TopoFactor)
         Bubble::Create $Data(Frame2).pos.topo.sel $Mapper::Bubble(Topo)
         Bubble::Create $Data(Frame2).pos.sample.val $Mapper::Bubble(Sample)
         Bubble::Create $Data(Frame2).pos.crop.active $Mapper::Bubble(CropTool)
         Bubble::Create $Data(Frame2).pos.crop.show $Mapper::Bubble(CropShow)
         Bubble::Create $Data(Frame2).pos.crop.mode $Mapper::Bubble(CropDo)

      set Data(Frame1) [TabFrame::Add .mapperparams.tab 1 [lindex $Mapper::Lbl(Ref) $GDefs(Lang)] False ""]

        frame $Data(Frame1).projhead
            label $Data(Frame1).projhead.lbl -text [lindex $Mapper::Lbl(Projection) $GDefs(Lang)] -width 10 -anchor nw
            button $Data(Frame1).projhead.file -image OPEN -relief flat -bd 0 -overrelief raised \
               -command "Mapper::ProjFile $Data(Frame1).proj.val \[FileBox::Create . \"\" Load \[list \$FileBox::Type(PROJ) \$FileBox::Type(TXT)\]\]"
            button $Data(Frame1).projhead.tab -image INTEROGATE -relief flat -bd 0 -overrelief raised \
               -command { set Mapper::GDAL::Data(Proj) [Mapper::WKT::Param $Mapper::GDAL::Data(Proj)]; $Mapper::GDAL::Data(Frame1).proj.val delete 0.0 end ; $Mapper::GDAL::Data(Frame1).proj.val insert end $Mapper::GDAL::Data(Proj); Mapper::GDAL::ParamsSet $Mapper::Data(Object) }
            pack $Data(Frame1).projhead.lbl -side left
            pack $Data(Frame1).projhead.tab $Data(Frame1).projhead.file -side left -padx 2
         labelframe $Data(Frame1).proj -labelwidget $Data(Frame1).projhead
            text $Data(Frame1).proj.val -bd 1 -bg $GDefs(ColorLight) -height 5 -width 25
            pack $Data(Frame1).proj.val -side right -fill both -expand true -padx 5 -pady 5
         pack  $Data(Frame1).proj -side top -fill both -expand true -padx 5 -pady 5

         labelframe $Data(Frame1).time -text [lindex $Mapper::Lbl(Time) $GDefs(Lang)]
            scale $Data(Frame1).time.scale -from 0 -to 0 -resolution 1 -variable Mapper::GDAL::Data(Secs) -relief raised -bd 1 \
               -relief flat -orient horizontal -width 15 -sliderlength 10 -command { set Mapper::GDAL::Data(Time) [clock format $Mapper::GDAL::Data(Secs) -gmt True]; catch } -showvalue False
            label $Data(Frame1).time.lbl -textvariable Mapper::GDAL::Data(Time)
            pack  $Data(Frame1).time.scale $Data(Frame1).time.lbl -side top -fill x -expand True
         pack $Data(Frame1).time -side top -fill x -padx 5 -pady 5 -ipady 2

         labelframe $Data(Frame1).pos -text [lindex $Mapper::Lbl(Position) $GDefs(Lang)]
            frame $Data(Frame1).pos.x
               label $Data(Frame1).pos.x.lbl -text X -width 6 -anchor w
               ComboBox::Create $Data(Frame1).pos.x.val Mapper::GDAL::Data(BandX) edit sorted nodouble -1 "" 1 8 ""
               pack $Data(Frame1).pos.x.lbl -side left
               pack $Data(Frame1).pos.x.val -side left -fill x -expand true
            frame $Data(Frame1).pos.y
               label $Data(Frame1).pos.y.lbl -text Y -width 6 -anchor w
               ComboBox::Create $Data(Frame1).pos.y.val Mapper::GDAL::Data(BandY) edit sorted nodouble -1 "" 1 8 ""
               pack $Data(Frame1).pos.y.lbl -side left
               pack $Data(Frame1).pos.y.val -side left -fill x -expand true
            pack $Data(Frame1).pos.x -side top -fill x -padx 5
            pack $Data(Frame1).pos.y -side top -fill x -padx 5
         pack $Data(Frame1).pos -side top -fill x -padx 5 -pady 5 -ipady 2

         labelframe $Data(Frame1).trans -text [lindex $Mapper::Lbl(Trans) $GDefs(Lang)]
            frame $Data(Frame1).trans.fwd
               label $Data(Frame1).trans.fwd.lbl -text [lindex $Mapper::Lbl(Forward) $GDefs(Lang)] -width 8 -anchor nw
               entry $Data(Frame1).trans.fwd.entry -textvariable Mapper::GDAL::Data(Trans) -bd 1 -bg $GDefs(ColorLight) -width 25
               pack $Data(Frame1).trans.fwd.lbl -side left
               pack $Data(Frame1).trans.fwd.entry -fill x -expand true -side left
            frame $Data(Frame1).trans.bck
               label $Data(Frame1).trans.bck.lbl -text [lindex $Mapper::Lbl(Backward) $GDefs(Lang)] -width 8 -anchor nw
               entry $Data(Frame1).trans.bck.entry -textvariable Mapper::GDAL::Data(InvTrans) -bd 1 -bg $GDefs(ColorLight) -width 25
               pack $Data(Frame1).trans.bck.lbl -side left
               pack $Data(Frame1).trans.bck.entry -fill x -expand true -side left
            pack $Data(Frame1).trans.fwd $Data(Frame1).trans.bck -fill x -side top -padx 5
         pack $Data(Frame1).trans -side top -fill x -padx 5 -pady 5 -ipady 2

         Bubble::Create $Data(Frame1).projhead.file $Mapper::Bubble(ProjLoad)
         Bubble::Create $Data(Frame1).projhead.tab  $Mapper::Bubble(ProjRef)
         Bubble::Create $Data(Frame1).proj $Mapper::Bubble(Projection)
         Bubble::Create $Data(Frame1).trans $Mapper::Bubble(Transform)
         Bubble::Create $Data(Frame1).pos   $Mapper::Bubble(PosArray)

      set Data(Frame3) [TabFrame::Add .mapperparams.tab 1 [lindex $Mapper::Lbl(Meta) $GDefs(Lang)] False ""]
         frame $Data(Frame3).meta
            text $Data(Frame3).meta.text -relief sunken -bd 1 -wrap none -bg $GDefs(ColorLight) -yscrollcommand "$Data(Frame3).meta.scroll set" -width 30
            scrollbar $Data(Frame3).meta.scroll -relief sunken -command "$Data(Frame3).meta.text yview" -bd 1 -width 10
            pack $Data(Frame3).meta.text -side left -fill both -expand true
            pack $Data(Frame3).meta.scroll -side left -fill y
         pack $Data(Frame3).meta -side top -fill both -expand true -padx 5 -pady 5

#       set Data(Frame4) [TabFrame::Add .mapperparams.tab 1 [lindex $Mapper::Lbl(GeoLoc) $GDefs(Lang)] False ""]
#
#          frame $Data(Frame4).georef
#             frame $Data(Frame4).georef.tool
#                button $Data(Frame4).georef.tool.add -image PINADD -relief raised -bd 0 -overrelief raised -command Mapper::GeoLocator::CoordAdd
#                button $Data(Frame4).georef.tool.del -image PINDEL -relief raised -bd 0 -overrelief raised -command Mapper::GeoLocator::CoordDel
#                checkbutton $Data(Frame4).georef.tool.mode -variable Page::Data(ToolMode) -onvalue Mapper::GeoLocator -offvalue SPI \
#                   -image ARROW -indicatoron 0 -relief sunken -bd 1 -overrelief raised -offrelief flat -selectcolor $GDefs(ColorFrame) \
#                   -command { SPI::ToolMode $Page::Data(ToolMode) Data True }
#                button $Data(Frame4).georef.tool.calcul -text [lindex $Mapper::Lbl(Calcul) $GDefs(Lang)] -relief raised -bd 0 -overrelief raised -command Mapper::GeoLocator::CoordSet
#                ComboBox::Create $Data(Frame4).georef.tool.fit Mapper::GeoLocator::Data(Mode) noedit unsorted nodouble -1 $Mapper::GeoLocator::Data(Modes) 10 10 ""
#                pack $Data(Frame4).georef.tool.mode $Data(Frame4).georef.tool.add $Data(Frame4).georef.tool.del \
#                   $Data(Frame4).georef.tool.fit $Data(Frame4).georef.tool.calcul -side left -padx 2
#             pack $Data(Frame4).georef.tool -side top -fill x
#
#             frame $Data(Frame4).georef.pt
#                listbox $Data(Frame4).georef.pt.list -width 16 -height 1 -bd 1 -relief sunken -exportselection False \
#                   -listvariable Mapper::GeoLocator::Data(GCPS) -yscrollcommand "$Data(Frame4).georef.pt.scroll set" -selectmode single -bg white
#                bind $Data(Frame4).georef.pt.list <ButtonRelease-1> { Mapper::GeoLocator::CoordView }
#                scrollbar $Data(Frame4).georef.pt.scroll -relief sunken -command "$Data(Frame4).georef.pt.list yview" -bd 1 -width 10
#                pack $Data(Frame4).georef.pt.list -side left -fill y
#                pack $Data(Frame4).georef.pt.scroll -side right -fill y
#             pack $Data(Frame4).georef.pt -side left -fill y -padx 2 -pady 5
#
#             frame $Data(Frame4).georef.hfrm
#                Page::Create $Data(Frame4).georef.hfrm.map -1 -1
#                pack $Data(Frame4).georef.hfrm.map -side top -expand true -fill both
#
#                frame $Data(Frame4).georef.hfrm.info -relief sunken -bd 1
#                   frame $Data(Frame4).georef.hfrm.info.xy
#                      label $Data(Frame4).georef.hfrm.info.xy.n -relief raised -bd 1 -text "    "
#                      label $Data(Frame4).georef.hfrm.info.xy.x -relief raised -bd 1 -text X
#                      label $Data(Frame4).georef.hfrm.info.xy.y -relief raised -bd 1 -text Y
#                      pack $Data(Frame4).georef.hfrm.info.xy.n $Data(Frame4).georef.hfrm.info.xy.x $Data(Frame4).georef.hfrm.info.xy.y \
#                         -side top -fill both
#                   frame $Data(Frame4).georef.hfrm.info.img
#                      label $Data(Frame4).georef.hfrm.info.img.lbl -text [lindex $Mapper::Lbl(Image) $GDefs(Lang)] -width 5 -anchor w -relief raised -bd 1
#                      label $Data(Frame4).georef.hfrm.info.img.x -textvariable Mapper::GeoLocator::Data(X) -width 15 -bg $GDefs(ColorLight) -bd 1
#                      label $Data(Frame4).georef.hfrm.info.img.y -textvariable Mapper::GeoLocator::Data(Y) -width 15 -bg $GDefs(ColorLight) -bd 1
#                      pack  $Data(Frame4).georef.hfrm.info.img.lbl $Data(Frame4).georef.hfrm.info.img.x $Data(Frame4).georef.hfrm.info.img.y \
#                         -side top -fill x
#                   frame $Data(Frame4).georef.hfrm.info.ref
#                      label $Data(Frame4).georef.hfrm.info.ref.lbl -text [lindex $Mapper::Lbl(Ref) $GDefs(Lang)] -width 5 -anchor w -relief raised -bd 1
#                      label $Data(Frame4).georef.hfrm.info.ref.x -textvariable Mapper::GeoLocator::Data(RefX) -width 15 -bg $GDefs(ColorLight) -bd 1
#                      label $Data(Frame4).georef.hfrm.info.ref.y -textvariable Mapper::GeoLocator::Data(RefY) -width 15 -bg $GDefs(ColorLight) -bd 1
#                      pack  $Data(Frame4).georef.hfrm.info.ref.lbl $Data(Frame4).georef.hfrm.info.ref.x $Data(Frame4).georef.hfrm.info.ref.y \
#                         -side top -fill x
#                   frame $Data(Frame4).georef.hfrm.info.coo
#                      label $Data(Frame4).georef.hfrm.info.coo.lbl -text [lindex $Mapper::Lbl(Coord) $GDefs(Lang)] -width 5 -anchor w -relief raised -bd 1
#                      label $Data(Frame4).georef.hfrm.info.coo.x -textvariable Mapper::GeoLocator::Data(Lon) -width 15 -bg $GDefs(ColorLight) -bd 1
#                      label $Data(Frame4).georef.hfrm.info.coo.y -textvariable Mapper::GeoLocator::Data(Lat) -width 15 -bg $GDefs(ColorLight) -bd 1
#                      pack  $Data(Frame4).georef.hfrm.info.coo.lbl $Data(Frame4).georef.hfrm.info.coo.x $Data(Frame4).georef.hfrm.info.coo.y \
#                         -side top -fill x
#                   pack $Data(Frame4).georef.hfrm.info.xy -side left
#                   pack $Data(Frame4).georef.hfrm.info.img $Data(Frame4).georef.hfrm.info.ref $Data(Frame4).georef.hfrm.info.coo \
#                      -side left -fill x -expand True
#                pack $Data(Frame4).georef.hfrm.info -side top -fill x
#             pack $Data(Frame4).georef.hfrm -side left -fill both -expand True -padx 2 -pady 5
#          pack $Data(Frame4).georef -side top -expand true -fill both -padx 5 -pady 5
#
#          Bubble::Create $Data(Frame4).georef.tool.add    $Mapper::Bubble(RefAdd)
#          Bubble::Create $Data(Frame4).georef.tool.del    $Mapper::Bubble(RefDel)
#          Bubble::Create $Data(Frame4).georef.tool.mode   $Mapper::Bubble(RefMode)
#          Bubble::Create $Data(Frame4).georef.tool.calcul $Mapper::Bubble(RefCalcul)
#          Bubble::Create $Data(Frame4).georef.tool.fit    $Mapper::Bubble(RefFit)

      frame .mapperparams.com
         checkbutton .mapperparams.com.real -image DOCSEL -bd 1 -relief raised \
            -variable Mapper::Data(RealTime) -onvalue 1 -offvalue 0 -indicatoron false
         button .mapperparams.com.apply  -text [lindex $Mapper::Lbl(Apply) $GDefs(Lang)] -bd 1 -command { Mapper::GDAL::ParamsSet $Mapper::Data(Object) }
         button .mapperparams.com.cancel -text [lindex $Mapper::Lbl(Close) $GDefs(Lang)] -bd 1 -command { Mapper::ParamsClose }
         pack .mapperparams.com.cancel .mapperparams.com.apply .mapperparams.com.real -side right
      pack .mapperparams.com -side top -fill x -padx 2 -pady 2

      if { ![llength $Tabs] } {
         set Tabs 0
      }
   }

   if { [llength $Tabs] && [lsearch -exact $Tabs [TabFrame::Current .mapperparams.tab]]==-1 } {
      TabFrame::Select .mapperparams.tab [lindex $Tabs 0]
   }
   
   #----- WMS specifics
   set wms $Mapper::Data(Object)
   set Data(Styles) {}
   if { [info exists ::Mapper::DepotWare::WMS::Data($wms)] } {

      #----- Styles management
      foreach style [lindex $Mapper::DepotWare::WMS::Data($wms) 9] {
         lappend Data(Styles) [lindex $style 0]
      }
      ComboBox::DelAll  $Data(Frame2).col.def.style.sel False
      ComboBox::AddList $Data(Frame2).col.def.style.sel $Data(Styles)
      pack $Data(Frame2).col.def.style -after $Data(Frame2).col.def.nodata -side top -fill x

      set time [lindex $Mapper::DepotWare::WMS::Data($wms) 10]
      set Data(T0)    [lindex $time 0]
      set Data(T1)    [lindex $time 1]

      #----- Time management
      if { $Data(T0)=="" } {
         pack forget $Data(Frame1).time
      } else {
         eval set Data(TIncr) \[clock add 0 [lindex $time 2]\]
         $Data(Frame1).time.scale configure -from $Data(T0) -to $Data(T1) -resolution $Data(TIncr)
         set Data(Secs)  [gdalband define $Object -date]

         #----- Use bind instead of -command to activate only on button release
         bind $Data(Frame1).time.scale <ButtonRelease-1>  { Mapper::DepotWare::WMS::ReLoad $Mapper::Data(Object) $Mapper::GDAL::Data(Style) $Mapper::GDAL::Data(Secs); update idletasks }
         bind $Data(Frame1).time.scale <ButtonRelease-2>  { Mapper::DepotWare::WMS::ReLoad $Mapper::Data(Object) $Mapper::GDAL::Data(Style) $Mapper::GDAL::Data(Secs); update idletasks }
         pack $Data(Frame1).time -after $Data(Frame1).proj -side top -fill x -padx 5 -pady 5 -ipady 2
      }
    } else {
      pack forget $Data(Frame2).col.def.style
      pack forget $Data(Frame1).time
   }
   $Data(Frame1).proj.val delete 0.0 end
   $Data(Frame1).proj.val insert end $Data(Proj)

   $Data(Frame3).meta.text delete 0.0 end
   $Data(Frame3).meta.text insert 0.0 $Data(Meta)

   ComboBox::DelAll  $Data(Frame2).pos.topo.sel False
   ComboBox::AddList  $Data(Frame2).pos.topo.sel [concat $Mapper::GDAL::Data(Topos) $Mapper::GDAL::Data(Colors) [gdalband all]]

   ComboBox::DelAll $Data(Frame2).band.red.val False
   ComboBox::Add $Data(Frame2).band.red.val ""
   ComboBox::AddList $Data(Frame2).band.red.val $Data(Band$Object) False
   ComboBox::DelAll $Data(Frame2).band.green.val False
   ComboBox::Add $Data(Frame2).band.green.val ""
   ComboBox::AddList $Data(Frame2).band.green.val $Data(Band$Object) False
   ComboBox::DelAll $Data(Frame2).band.blue.val False
   ComboBox::Add $Data(Frame2).band.blue.val ""
   ComboBox::AddList $Data(Frame2).band.blue.val $Data(Band$Object) False
   ComboBox::DelAll $Data(Frame2).band.alpha.val False
   ComboBox::Add $Data(Frame2).band.alpha.val ""
   ComboBox::AddList $Data(Frame2).band.alpha.val $Data(Band$Object) False

   ComboBox::DelAll $Data(Frame1).pos.x.val False
   ComboBox::Add $Data(Frame1).pos.x.val ""
   ComboBox::AddList $Data(Frame1).pos.x.val $Data(Band$Object) False
   ComboBox::DelAll $Data(Frame1).pos.y.val False
   ComboBox::Add $Data(Frame1).pos.y.val ""
   ComboBox::AddList $Data(Frame1).pos.y.val $Data(Band$Object) False

   bind $Data(Frame2).col.curve.info.min <Any-KeyRelease>  { catch { set Mapper::GDAL::Data(Stretch) ""; Mapper::GDAL::CurveSelect $Mapper::GDAL::Data(Frame2).col.curve.cv $Mapper::Data(Object) $Mapper::GDAL::Data(Band) False } }
   bind $Data(Frame2).col.curve.info.max <Any-KeyRelease>  { catch { set Mapper::GDAL::Data(Stretch) ""; Mapper::GDAL::CurveSelect $Mapper::GDAL::Data(Frame2).col.curve.cv $Mapper::Data(Object) $Mapper::GDAL::Data(Band) False } }

   #----- Setup colormap

   set Data(ColorMap) [gdalband configure $Object -colormap]
   colormap image $Data(ColorMap) GDALMAPImg

   Mapper::GDAL::CurveDefine $Object $Data(Bands$Object)
}

#-------------------------------------------------------------------------------
# Nom      : <Mapper::GDAL::ParamsGet>
# Creation : Juillet 2004 - J.P. Gauthier - CMC/CMOE
#
# But      : Recuperer les parametres de donnees raster GDAL
#
# Parametres :
#   <Object> : Donnee geographique a parametrer
#
# Retour    :
#
# Remarque :
#
#-------------------------------------------------------------------------------

proc Mapper::GDAL::ParamsGet { Object } {
   global   GDefs
   variable Data

   set Data(Band)       red
   set Data(Sample)     [gdalband configure $Object -texsample]
   set Data(Texture)    [gdalband configure $Object -texsize]
   set Data(Resolution) [gdalband configure $Object -texres]
   set Data(Tran)       [gdalband configure $Object -transparency]
   set Data(Interp)     [gdalband configure $Object -interpolation]
   set Data(Topo)       [gdalband configure $Object -topography]
   set Data(TopoFactor) [gdalband configure $Object -topographyfactor]
   set Data(Mask)       [gdalband configure $Object -mask]
   set Data(ColorMap)   [gdalband configure $Object -colormap]
   set Data(Style)      [gdalband configure $Object -sizevar]
   set Data(Proj)       [gdalband define $Object -projection]
   set Data(Trans)      [gdalband define $Object -transform]
   set Data(InvTrans)   [gdalband define $Object -invtransform]
   set Data(NoData)     [gdalband stats $Object -nodata]

   if { $Data(Mask)!="" } {
      set Data(Cut) True
   }

   set Data(Meta)     [join [gdalfile metadata $Mapper::Data(Id$Object)] \n]

   set Data(Red)   $Data(Band0$Object)
   set Data(Green) $Data(Band1$Object)
   set Data(Blue)  $Data(Band2$Object)
   set Data(Alpha) $Data(Band3$Object)
   set Data(BandX) $Data(BandX$Object)
   set Data(BandY) $Data(BandY$Object)

   Mapper::UpdateItems
}

#-------------------------------------------------------------------------------
# Nom      : <Mapper::GDAL::ParamsSet>
# Creation : Juillet 2004 - J.P. Gauthier - CMC/CMOE
#
# But      : Parametrer un objet de type Model
#
# Parametres :
#   <Object> : Donnee geographique a parametrer
#
# Retour    :
#
# Remarque :
#
#-------------------------------------------------------------------------------

proc Mapper::GDAL::ParamsSet { Object { CheckData True } } {
   variable Data

   if { $Mapper::Data(Init) } {
      return
   }

   Mapper::Cursor watch

   if { $CheckData } {
      if { $Data(Band0$Object)!=$Data(Red) || $Data(Band1$Object)!=$Data(Green) || $Data(Band2$Object)!=$Data(Blue) || $Data(Band3$Object)!=$Data(Alpha) } {
      
         #----- Make sure band combination is legal (Grey,Grey-Alpha,RGB,RGBA)
         set nb [llength [set bands [lsearch -not -all -inline [list $Data(Red) $Data(Green) $Data(Blue) $Data(Alpha)] ""]]]
         
         switch $nb {
            1 { set Data(Red) [lindex $bands 0]; set Data(Green) ""; set Data(Blue) ""; set Data(Alpha) "" }
            2 { set Data(Red) [lindex $bands 0]; set Data(Green) ""; set Data(Blue) ""; set Data(Alpha) [lindex $bands 1] }
            3 { set Data(Red) [lindex $bands 0]; set Data(Green) [lindex $bands 1]; set Data(Blue) [lindex $bands 2]; set Data(Alpha) "" }
         }
         Mapper::GDAL::Read $Object [list $Data(Red) $Data(Green) $Data(Blue) $Data(Alpha)]
      }

      if { $Data(BandX$Object)!=$Data(BandX) || $Data(BandY$Object)!=$Data(BandY) } {
         Mapper::GDAL::ReadPos $Object $Data(BandX) $Data(BandY)
      }
   }

   gdalband configure $Object -texsample $Data(Sample) -texres $Data(Resolution) -texsize $Data(Texture) -transparency $Data(Tran) \
      -interpolation $Data(Interp) -topography $Data(Topo) -topographyfactor $Data(TopoFactor) -font XFont12 -sizevar $Data(Style)
   gdalband stats $Object -nodata $Data(NoData)

   if { $Data(Cut) && [ogrgeometry is MASK$Object] } {
      gdalband configure $Object -mask MASK$Object
   } else {
      gdalband configure $Object -mask ""
   }

   if { [winfo exists .mapper] } {
      set Data(Proj) [string trim [$Data(Frame1).proj.val get 0.0 end] "\n"]
      gdalband define $Object -projection $Data(Proj) -transform $Data(Trans)

      #----- Reassign positionnal if any
      if  { [gdalband is BandX$Object] && [gdalband is BandY$Object] } {
         gdalband define $Object -positional BandX$Object BandY$Object
      }

      Page::Update $Page::Data(Frame)
      Mapper::GDAL::CurveDefine $Object $Data(Bands$Object)
   }

   Mapper::Cursor left_ptr
}

proc Mapper::GDAL::TabSelect { Frame No } {
   variable Data

   if { $No==3 } {
      Mapper::GeoLocator::Activate $Data(Object)
   } else {
   }
}

proc Mapper::GDAL::CurveDefine { Object Bands } {
   variable Data

   set Data(MMSet) 0
   set Data(Band) red

   set canvas $Data(Frame2).col.curve.cv
   $Data(Frame2).col.sel.red   configure -state disabled -bg gray75
   $Data(Frame2).col.sel.green configure -state disabled -bg gray75
   $Data(Frame2).col.sel.blue  configure -state disabled -bg gray75
   $Data(Frame2).col.sel.alpha configure -state disabled -bg gray75

   #----- Setup colormap

   set map [gdalband configure $Object -colormap]
   colormap image $map GDALMAPImg

   #----- Setup curves

   $canvas delete all
   $canvas create polygon 0 0 0 0 -fill gray50 -outline gray25 -width 1 -tags "histo"

   if { ![set Data(Indexed) [gdalband define $Object -indexed]] } {
      if { $Data(Red)!="" } {
         $Data(Frame2).col.sel.red configure -state normal -bg red
         $canvas create line 0 0 0 0 -fill red -width 1 -tags "curve red"
      }
      if { $Data(Green)!="" && !$Data(Lock) } {
         $Data(Frame2).col.sel.green configure -state normal -bg green
         $canvas create line 0 0 0 0 -fill green -width 1 -tags "curve green"
      }
      if { $Data(Blue)!="" && !$Data(Lock) } {
         $Data(Frame2).col.sel.blue configure -state normal -bg blue
         $canvas create line 0 0 0 0 -fill blue -width 1 -tags "curve blue"
      }
      if { $Data(Alpha)!="" && !$Data(Lock) } {
         $Data(Frame2).col.sel.alpha configure -state normal -bg white
         $canvas create line 0 0 0 0 -fill white -width 1 -tags "curve alpha"
      }

      $canvas create line 1   0 1   128 1   64  10  64 -fill black  -width 2 -tags "min" -arrow last -arrowshape { 10 10 10 }
      $canvas create line 256 0 256 128 256 64  246 64 -fill black  -width 2 -tags "max" -arrow last -arrowshape { 10 10 10 }

      #----- Bind canvas events

      $canvas bind min <ButtonPress-1>   "set Mapper::GDAL::Data(MMSet) 1; $canvas configure -cursor hand1"
      $canvas bind max <ButtonPress-1>   "set Mapper::GDAL::Data(MMSet) 1; $canvas configure -cursor hand1"
      $canvas bind min <B1-Motion>       "Mapper::GDAL::CurveRange $canvas \$Mapper::Data(Object) \$Mapper::GDAL::Data(Band) min %x"
      $canvas bind max <B1-Motion>       "Mapper::GDAL::CurveRange $canvas \$Mapper::Data(Object) \$Mapper::GDAL::Data(Band) max %x"
      $canvas bind min <ButtonRelease-1> "set Mapper::GDAL::Data(MMSet) 0; $canvas configure -cursor left_ptr"
      $canvas bind max <ButtonRelease-1> "set Mapper::GDAL::Data(MMSet) 0; $canvas configure -cursor left_ptr"

      bind $canvas <B1-Motion>       "if { !\$Mapper::GDAL::Data(MMSet) } {  Mapper::GDAL::CurvePoint $canvas \$Mapper::Data(Object) \$Mapper::GDAL::Data(Band) %x %y }"
      bind $canvas <ButtonPress-2>   "set Mapper::GDAL::Data(CurveX) %x; $canvas configure -cursor hand1"
      bind $canvas <B2-Motion>       "if { !\$Mapper::GDAL::Data(MMSet) } { Mapper::GDAL::CurveTranslate $canvas \$Mapper::Data(Object) \$Mapper::GDAL::Data(Band) %x }"
      bind $canvas <ButtonRelease-2> "set Mapper::GDAL::Data(CurveX) %x; $canvas configure -cursor left_ptr"
   }
   bind $canvas <Motion> { Mapper::GDAL::CurveValue %x }

   Mapper::GDAL::CurveSelect $canvas $Object red
}

proc Mapper::GDAL::CurveInvert { Canvas Object Band Axis Value } {
   variable Data

   if { [llength $Data(Bands$Object)]==1 || $Data(Lock) } {
      set band rgba
   } else {
      set band $Band
   }

   set map [gdalband configure $Object -colormap]

   if { $Axis=="X" } {
      colormap configure $map -invertx $band $Value
   } else {
      colormap configure $map -inverty $band $Value
   }
   if { ![glrender -shaderavailable] } { gdalband clean $Object }
   Mapper::GDAL::Curve $Canvas $Object $Data(Band)
}

proc Mapper::GDAL::CurveHisto { Object } {
   variable Data

   Mapper::Cursor watch

   if { [set idx [lsearch -exact { red green blue alpha } $Data(Band)]]!=-1 } {
      if { ![info exists Data(Histo$Object$idx)] } {
         set Data(Histo$Object$idx) [gdalband stats $Object -histogram $idx]
      }
   }
   set Data(HistoShow) False
   Mapper::GDAL::CurveSelect $Data(Frame2).col.curve.cv $Mapper::Data(Object) $Data(Band)

   Mapper::Cursor left_ptr
}

proc Mapper::GDAL::CurveSelect { Canvas Object Band { MinMax True } } {
   variable Data

   if { [set idx [lsearch -exact { red green blue alpha } $Band]]!=-1 } {

      set map [gdalband configure $Object -colormap]

      #----- Reconfigure curve display
      $Canvas itemconfigure curve -width 1
      $Canvas itemconfigure $Band -width 2
      $Canvas raise $Band
      $Canvas raise min
      $Canvas raise max

      if { $Data(Stretch)!="" } {
         eval gdalband stats \$Object -stretch \$idx $Data(Stretch)
      }

      #----- Setup min-max
      set Data(CurveMin) [lindex [gdalband stats $Object -min $idx] 0]
      set Data(CurveMax) [lindex [gdalband stats $Object -max $idx] 0]
      
      if { $Data(CurveMin)>[colormap configure $map -min $Band] } {
         colormap configure $map -min $Band $Data(CurveMin)
         catch { unset Data(Histo$Object$idx) }
      }
      if { $Data(CurveMax)<[colormap configure $map -max $Band] } {
         colormap configure $map -max $Band $Data(CurveMax)
         catch { unset Data(Histo$Object$idx) }
      }

      if { $Data(CurveMin)=="" || $Data(CurveMax)=="" } {
         return
      }

      #----- Setup histogram
      set coords { }
      set Data(Histo)    {}
      set Data(HistoMin) 1e300
      set Data(HistoMax) -1e300

      if { [info exists Data(Histo$Object$idx)] } {
         set Data(Histo) $Data(Histo$Object$idx)
      }

      foreach h $Data(Histo) {
         set Data(HistoMax) [expr $Data(HistoMax)>$h?$Data(HistoMax):$h]
         set Data(HistoMin) [expr $Data(HistoMin)<$h?$Data(HistoMin):$h]
      }

      if { [llength $Data(Histo)] && $Data(HistoMax)!=$Data(HistoMin) } {
         set dy [expr 128.0/($Data(HistoMax)-$Data(HistoMin))]
         set x -1
         lappend coords 0 128
         foreach h $Data(Histo) {
            lappend coords [incr x] [expr 128.0-($h-$Data(HistoMin))*$dy]
         }
         lappend coords 256 128
      } else {
         set coords  { 0 0 0 0 0 0 0 0 }
      }

      $Canvas coords histo $coords

      #----- Setup curves
      set Data(CurveInvertX) [colormap configure $map -invertx $Band]
      set Data(CurveInvertY) [colormap configure $map -inverty $Band]

      if { $MinMax } {
         set Data(CurveMinD) [colormap configure $map -min $Band]
         set Data(CurveMaxD) [colormap configure $map -max $Band]
      } else {
         colormap configure $map -min $Band $Data(CurveMinD)
         colormap configure $map -max $Band $Data(CurveMaxD)
      }

      if { $Data(CurveMax)!=$Data(CurveMin) } {
         set d [expr 256.0/($Data(CurveMax)-$Data(CurveMin))]
         set Data(CurveMinX) [expr ($Data(CurveMinD)-$Data(CurveMin))*$d]
         $Canvas coords min $Data(CurveMinX) 1 $Data(CurveMinX) 128 $Data(CurveMinX) 64 [expr $Data(CurveMinX)+10] 64
         set Data(CurveMaxX) [expr ($Data(CurveMaxD)-$Data(CurveMin))*$d]
         $Canvas coords max $Data(CurveMaxX) 1 $Data(CurveMaxX) 128 $Data(CurveMaxX) 64 [expr $Data(CurveMaxX)-10] 64
      }
      set Data(Curve) [colormap configure $map -curve $Band]
      if { ![glrender -shaderavailable] } { gdalband clean $Object }

      Mapper::GDAL::Curve $Canvas $Object $Band
   }
}

proc Mapper::GDAL::CurveRange { Canvas Object Band Side X } {
   variable Data

   set Data(Stretch) ""
   set map [gdalband configure $Object -colormap]

   Mapper::GDAL::CurveValue $X

   if { [llength $Data(Bands$Object)]==1 || $Data(Lock) } {
      set band rgba
   } else {
      set band $Band
   }

   if { $X>=0 && $X<=256 } {
      if { $Side=="min" } {
         if { $Data(CurveValue) < [colormap configure $map -max $Band] } {
            set Data(CurveMinX) $X
            set Data(CurveMinD) $Data(CurveValue)
            $Canvas coords min $X 1 $X 128 $X 64 [expr $X+10] 64
            colormap configure $map -min $band $Data(CurveValue)
          }
      } else {
         if { $Data(CurveValue) > [colormap configure $map -min $Band] } {
            set Data(CurveMaxX) $X
            set Data(CurveMaxD) $Data(CurveValue)
            $Canvas coords max $X 1 $X 128 $X 64 [expr $X-10] 64
            colormap configure $map -max $band $Data(CurveValue)
         }
      }
      if { ![glrender -shaderavailable] } { gdalband clean $Object}
      Mapper::GDAL::Curve $Canvas $Object $band
   }
}

proc Mapper::GDAL::CurveTranslate { Canvas Object Band X } {
   variable Data

   Mapper::GDAL::CurveValue $X

   set dx [expr $X-$Data(CurveX)]

   if { [llength $Data(Bands$Object)]==1 || $Data(Lock) } {
      set band rgba
   } else {
      set band $Band
   }
   set map [gdalband configure $Object -colormap]

   if { [expr $Data(CurveMinX)+$dx]>=0 && [expr $Data(CurveMaxX)+$dx]<=256 } {
      set Data(CurveMinX) [expr $Data(CurveMinX)+$dx]
      set Data(CurveMaxX) [expr $Data(CurveMaxX)+$dx]
      $Canvas coords min $Data(CurveMinX) 1 $Data(CurveMinX) 128 $Data(CurveMinX) 64 [expr $Data(CurveMinX)+10] 64
      colormap configure $map -min $band [set Data(CurveMinD) [Mapper::GDAL::CurveValue $Data(CurveMinX)]]
      $Canvas coords max $Data(CurveMaxX) 1 $Data(CurveMaxX) 128 $Data(CurveMaxX) 64 [expr $Data(CurveMaxX)-10] 64
      colormap configure $map -max $band [set Data(CurveMaxD) [Mapper::GDAL::CurveValue $Data(CurveMaxX)]]

      if { ![glrender -shaderavailable] } { gdalband clean $Object }
      Mapper::GDAL::Curve $Canvas $Object $band
   }
   set Data(CurveX) $X
}

proc Mapper::GDAL::CurveValue { X } {
   variable Data

   set Data(CurveNb) [lindex $Data(Histo) [expr int($X)]]
   return [set Data(CurveValue) [expr $Data(CurveMin)+($Data(CurveMax)-$Data(CurveMin))*$X/256.0]]
}

proc Mapper::GDAL::CurvePoint { Canvas Object Band X Y } {
   variable Data

   Mapper::GDAL::CurveValue $X

   set Data(Curve) ""
   set map [gdalband configure $Object -colormap]
   colormap configure $map -curve $Band ""

   set d  [expr 256.0/($Data(CurveMaxX)-$Data(CurveMinX))]
   set dx [expr int($d)]
   set X  [expr int(($X-$Data(CurveMinX))*$d)-1]
   set Y  [expr ((128-$Y)*2)-1]

   if { [llength $Data(Bands$Object)]==1 || $Data(Lock) } {
      set band rgba
   } else {
      set band $Band
   }

   if { [expr $X-$dx]>=0 && [expr $X+$dx]<=255 && $Y>=0 && $Y<=255 } {
      for { set x [expr $X-$dx] } { $x<[expr $X+$dx] } { incr x } {
         colormap configure $map -curvepoint $band $x $Y
       }
   }
   if { ![glrender -shaderavailable] } { gdalband clean $Object }
   Mapper::GDAL::Curve $Canvas $Object $band
}

proc Mapper::GDAL::CurveSet { Canvas Object } {
   variable Data

   set map [gdalband configure $Object -colormap]
   colormap control $map -update

   if { [llength $Data(Bands$Object)]==1 || $Data(Lock) } {
      colormap configure $map -curve rgba $Data(Curve)
   } else {
      colormap configure $map -curve $Data(Band) $Data(Curve)
   }

   if { ![glrender -shaderavailable] } { gdalband clean $Object }
   Mapper::GDAL::Curve $Canvas $Object $Data(Band)
}

proc Mapper::GDAL::Curve { Canvas Object Bands } {
   variable Data

   if { $Bands=="rgba" } {
      set Bands { red green blue alpha }
   }
   set map [gdalband configure $Object -colormap]

   foreach band $Bands {
      set ys  [colormap configure $map -curvepoint $band]
      set ny  [llength $ys]
      set min [colormap configure $map -min $band]
      set max [colormap configure $map -max $band]

      #----- Calculate offset and scaling on range

      if { $Data(CurveMax)!=$Data(CurveMin) } {
         set d  [expr ($Data(CurveMax)-$Data(CurveMin))]
         set dx [expr ($min-$Data(CurveMin))*$ny/$d]
         set fx [expr ($max-$min)/$d]

         #----- Set curve coordinate

         set coords ""
         for { set i 0 } { $i < $ny } { incr i } {
            append coords "[expr $i*$fx+$dx] [expr 128.0-[lindex $ys $i]/2.0] "
         }
      } else {
          set coords { 0 0 0 0 }
      }
      eval $Canvas coords $band $coords
#      update

      if { $Mapper::Data(RealTime) } {
        Page::Update $Page::Data(Frame)
      }
   }
   colormap image $map GDALMAPImg
}

#-------------------------------------------------------------------------------
# Nom      : <Mapper::GDAL::Read>
# Creation : Juillet 2004 - J.P. Gauthier - CMC/CMOE
#
# But      : Lecture d'une donnee geographique GDAL.
#
# Parametres :
#   <File>   : Fichiers a lire
#   <Bands>  : Index of the bands to read
#   <Nb>     : Number of band to read
#   <Full>   : Read whole thin gor delay load
#
# Retour    :
#
# Remarque :
#
#-------------------------------------------------------------------------------

proc Mapper::GDAL::Read { File { Bands "" } { Nb 3 } { Full False } } {
   global GDefs errorInfo
   variable Data
   
   if { [winfo exists .mapbox] } {
      destroy .mapbox
   }
   #---- If an id is passed, use it
   if  { [info exists Mapper::Data(Id$File)] } {
      set obj $File
   } else {
      set obj [file tail [file rootname $File]]
      set no 1
      while { [gdalband is $obj] } {
         set obj $obj$no
         incr no
      }

      if  { ![info exists Mapper::Data(Id$obj)] } {
         set id GDAL[incr Data(IdNo)]
         set Mapper::Data(Id$obj) $id
      } else {
         set id $Mapper::Data(Id$obj)
      }

      gdalfile close $id
      set bands {}
      eval set bad [catch { set bands [gdalfile open $id read $File] }]

      if { $bad || ![llength $bands] } {
         return ""
      }
      set Data(Band$obj)  $bands
      set Data(BandX$obj) ""
      set Data(BandY$obj) ""
   }

   set Mapper::Data(Job) [lindex $Mapper::Msg(Read) $GDefs(Lang)]
   update idletasks;

   if { ![llength $Bands] } {
      set interp [gdalfile colorinterp $id]
      if { $interp=="Red"  } {
         set Bands [lrange $bands 0 $Nb]
         set Data(Interp) LINEAR
      } else {
         set Data(Interp) NEAREST
         set Bands [list [lindex $bands 0]]
      }
   }

   #----- Check for generic naming
   for { set i 0 } { $i<[llength $Bands] } { incr i } {
      if { [lindex [lindex $Bands $i] 0]=="" } {
         if { [llength [lindex $Bands $i]] } {
            lset Bands $i 0 $id
         }
      }
   }

   set Data(Bands$obj) {}
   set Data(Bands0$obj) ""
   set Data(Bands1$obj) ""
   set Data(Bands2$obj) ""
   set Data(Bands3$obj) ""
   
   foreach band $Bands index { 0 1 2 3 } channel { red green blue alpha } {
      set Data(Band$index$obj) $band
      
      if { $band!="" } {
         lappend Data(Bands$obj) $channel
      }
   }

   set er [catch { gdalband read $obj $Bands $Full } errmsg]

   if { $er } {
      error $errmsg $errorInfo
   }

   #----- Reassign positionnal if any
   if  { [gdalband is BandX$obj] && [gdalband is BandY$obj] } {
      gdalband define $obj -positional BandX$obj BandY$obj
   }

   set Data(ColorMap) [gdalband configure $obj -colormap]
  
   gdalband configure $obj -interpolation $Data(Interp)

   foreach min [gdalband stats $obj -min] band $Data(Bands$obj) {
      colormap configure $Data(ColorMap) -min $band [lindex $min 0]
   }
   foreach max [gdalband stats $obj -max] band $Data(Bands$obj) {
      colormap configure $Data(ColorMap) -max $band [lindex $max 0]
  }

   set Mapper::Data(Job) ""

   if { [lsearch -exact $Viewport::Data(Data$Page::Data(Frame)) $obj]==-1 } {
      lappend Viewport::Data(Data$Page::Data(Frame)) $obj
   }
  
   return $obj
}

proc Mapper::GDAL::ReadPos { Id BandX BandY } {
   global GDefs errorInfo
   variable Data

   if  { ![info exists Mapper::Data(Id$Id)] || $BandX=="" || $BandY=="" } {
      return
   }

   set Data(Job)   [lindex $Mapper::Msg(Read) $GDefs(Lang)]
   update idletasks;

   set err [catch { gdalband read BandX$Id [list $BandX] } errmsg ]
   if { $err } {
      error $errmsg $errorInfo
   }

   set err [catch { gdalband read BandY$Id [list $BandY] } errmsg ]
   if { $err } {
      error $errmsg $errorInfo
   }

   gdalband define $Id -positional BandX$Id BandY$Id

   set Data(Proj)       [gdalband define $Id -projection]
   set Data(Trans)      [gdalband define $Id -transform]
   set Data(InvTrans)   [gdalband define $Id -invtransform]

   set Data(BandX$Id) $BandX
   set Data(BandY$Id) $BandY
   
   set Mapper::Data(Job) ""
}

proc Mapper::GDAL::Apply { Object } {
   set map [gdalband configure $Object -colormap]
   colormap image $map GDALMAPImg
   Mapper::GDAL::ParamsSet $Object
   if { ![glrender -shaderavailable] || [gdalband define $Object -indexed] } { gdalband clean $Object }
}
