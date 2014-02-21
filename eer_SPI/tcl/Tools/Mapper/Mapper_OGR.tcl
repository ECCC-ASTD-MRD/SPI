# ::Add#===============================================================================
# Environnement Canada
# Centre Meteorologique Canadian
# 2100 Trans-Canadienne
# Dorval, Quebec
#
# Projet   : Fonctions de manipulation des donnees geographiques.
# Fichier  : Mapper_OGR.tcl
# Creation : Janvier 2014 - J.P.Gauthier - CMC/CMOE
#
# Description:
#    Fontions de manipulation de donnees georeference OGR.
#
# Remarques :
#
#===============================================================================

namespace eval Mapper::OGR { } {
   variable Data
   
   set Data(Geom) ""
   set Data(Ops)      { dissolve boundary convexhull buffer difference intersection simplify segmentize close flatten }
  
   set Data(Icons)       { NONE TRIANGLE SQUARE CIRCLE LOZENGE HBAR VBAR PENTAGON HEXAGON LIGHTNING X + }
   set Data(Intervals)   ""
   set Data(Topos)       { NONE INTERNAL }
   set Data(Topo)        NONE
   set Data(TopoFactor)  1.0
   set Data(Mask)        False
   set Data(TableSel)    ""
   set Data(TableSort)   ""
   set Data(TableUpdate) 0
   set Data(GeomOut)     "TEXT"
   set Data(GeomGet)     False
   set Data(Fill)        ""
   set Data(Color)       black
   set Data(Width)       0
   set Data(Burn)        1
   set Data(Extr)        ""
   set Data(ExtrFactor)  1.0
   set Data(Dash)        ""
   set Data(Snap)        10
   set Data(SelMode)     Locate
   set Data(Edit)        False
   set Data(3D)          False       
   set Data(GeomType)    "Point"
   set Data(GeomTypes)   { "Point" "Line String" "Polygon" "Multi Point" "Multi Line String" "Multi Polygon" "Geometry Collection" }
   set Data(FieldTypes)  { Integer IntegerList Real RealList String StringList Time Date DateTime Binary }

   set Data(Formats) {
      {Arc/Info Binary Coverage {*.bin}}      
      {Arc/Info ASCII Coverage {*.e00}}      
      {ESRI Shapefile {*.shp}}
      {ESRI ArcSDE {*.sde}}
      {ESRI Personal GeoDatabase {*.mdb}}
      {Géoconcept Export {*.gxt}}
      {Geography Markup Language {*.gml}}
      {GeoJSON {*.geojson}}
      {GeoRSS {*.xml}}
      {GeoMedia {*.mdb}}
      {GMT ASCII Vectors {*.gmt}}
      {GPS Exchange Format {*.gpx}}
      {GPSTrackMaker {*.gtm,*.gtz}}
      {GRASS 6+ {*}}
      {INTERLIS 1 {*.itf *.xml *.ili}}
      {Keyhole Markup Language {*.kml}}
      {MapInfo TAB and MIF/MID {*.mid *.mif *.tab *.dat *.map *.id *.ind}}
      {Microstation {*.dgn}}
      {OpenStreetMap {*.osm,*.pbf}}
      {PostgreSQL SQL dump {*.sql}}
      {SDTS {*.ddf}}
      {SQLite RDBMS {*.sql}}
      {Special Use Airspace {*.sua}}
      {OpenAir Special Use Airspace {*.txt}}
      {Planetary Data Systems {*.pds}}
      {EDIGEO {*.htf}}
      {IHO S-57 {*.000}}
      {Hydrographic Transfer Format {*.htf}}
      {UK National Transfer Format {*.ntf}}
      {U.S. Census TIGER/Line {*.rt*}}
      {Aeronav FAA {*.txt}}
      {Idrisi {*.vct *.adc *.avl}}
      {Spatial and Attribute Indexing {*.sbn *.sbx}}
      {Virtual Datasource {*.vrt}}
      {X-Plane/Flightgear aeronautical data {*.dat}}}

   set Data(WriteFormats) {
      {ESRI Shapefile {*.shp}}
      {GeoJSON {*.geojson}}
      {Geoconcept {*.gxt}}
      {GeoRSS {*.xml}}
      {GML {*.gml}}
      {GMT {*.gmt}}
      {GPX {*.gpx}}
      {KML {*.kml}}
      {MapInfo File {*.mif}}
      {PDF {*.pdf}} 
      {PGDump {*.sql}}
      {SQLite {*.sqlite}}}
   
   catch {
      image create photo OGRMAPImg  -width 256 -height 15
      font create OGRFONT -family courier -size -10 -weight bold

      colormap create OGRMAPDEFAULT
      colormap read OGRMAPDEFAULT $env(HOME)/.spi/Colormap/REC_Col.std1.rgba
   }
   
   georef create LLREF { GEOGCS["GCS_WGS_1984",DATUM["WGS_1984",SPHEROID["WGS84",6378137.0,298.257223563]],PRIMEM["Greenwich",0],UNIT["Degree",0.0174532925199432958]] }
}

#-------------------------------------------------------------------------------
# Nom      : <Mapper::OGR::Params>
# Creation : Juillet 2004 - J.P. Gauthier - CMC/CMOE
#
# But      : Parametres des objets de type OGR
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

proc Mapper::OGR::Params { Object { Tabs {} } } {
   global   GDefs
   variable Data

   if { [winfo exists .mapperparams] && $Mapper::Data(Mode)!="ogrlayer" } {
      set pos [wm geometry .mapperparams]
      Mapper::ParamsClose
      destroy .mapperparams
   } else {
      set pos 500x425
   }
   set Mapper::Data(Mode) ogrlayer
   
   if { ![winfo exists .mapperparams] } {
      toplevel .mapperparams

      wm title     .mapperparams "[lindex $Mapper::Lbl(OGR) $GDefs(Lang)]"
      wm transient .mapperparams .
      wm geometry  .mapperparams =$pos
      wm protocol  .mapperparams WM_DELETE_WINDOW { destroy .mapperparams }

      TabFrame::Create .mapperparams.tab 1 ""
      pack .mapperparams.tab -side top -fill both -expand true -padx 2 -pady 2

      set Data(Frame2) [TabFrame::Add .mapperparams.tab 1 [lindex $Mapper::Lbl(Display) $GDefs(Lang)] False ""]
         labelframe $Data(Frame2).pos -text [lindex $Mapper::Lbl(Position) $GDefs(Lang)]
            frame  $Data(Frame2).pos.extr
               label $Data(Frame2).pos.extr.lbl -text  [lindex $Mapper::Lbl(Extrude) $GDefs(Lang)] -width 14 -anchor nw
               label $Data(Frame2).pos.extr.x -text "x" -width 1 -anchor nw
               ComboBox::Create $Data(Frame2).pos.extr.sel Mapper::OGR::Data(Extr) noedit unsorted nodouble -1 "" 1 8 ""
               entry  $Data(Frame2).pos.extr.fac -bg $GDefs(ColorLight) -textvariable Mapper::OGR::Data(ExtrFactor) -bd 1 -width 5
               pack  $Data(Frame2).pos.extr.lbl $Data(Frame2).pos.extr.fac $Data(Frame2).pos.extr.x -side left -fill y
               pack  $Data(Frame2).pos.extr.sel -side left -fill both -expand true
            frame  $Data(Frame2).pos.topo
               label $Data(Frame2).pos.topo.lbl -text  [lindex $Mapper::Lbl(Topo) $GDefs(Lang)] -width 14 -anchor nw
               label $Data(Frame2).pos.topo.x -text "x" -width 1 -anchor nw
               entry  $Data(Frame2).pos.topo.fac  -bg $GDefs(ColorLight) -textvariable Mapper::OGR::Data(TopoFactor) -bd 1 -width 5
               ComboBox::Create $Data(Frame2).pos.topo.sel Mapper::OGR::Data(Topo) noedit unsorted nodouble -1 "" 1 8 ""
               pack  $Data(Frame2).pos.topo.lbl $Data(Frame2).pos.topo.fac $Data(Frame2).pos.topo.x -side left -fill y
               pack  $Data(Frame2).pos.topo.sel -side left -fill x -expand true
            pack $Data(Frame2).pos.extr $Data(Frame2).pos.topo -side top -fill x -padx 5

         labelframe $Data(Frame2).shape -text [lindex $Mapper::Lbl(Shape) $GDefs(Lang)]
            frame $Data(Frame2).shape.fill
               label $Data(Frame2).shape.fill.lbl -text [lindex $Mapper::Lbl(Fill) $GDefs(Lang)] -width 12 -anchor w
               ColorBox::CreateSel  $Data(Frame2).shape.fill.val Mapper::OGR::Data(Fill) Mapper::OGR::ParamsSet \$Mapper::Data(Object)
               checkbutton $Data(Frame2).shape.fill.sel -variable Mapper::OGR::Data(FillSel) -relief raised -bd 1 \
                  -bitmap @$GDefs(Dir)/share/bitmap/zeroth.xbm -indicatoron false \
                  -command { Mapper::OGR::ParamsSet $Mapper::Data(Object) } -selectcolor "" -relief groove -bd 1
               pack $Data(Frame2).shape.fill.lbl $Data(Frame2).shape.fill.val $Data(Frame2).shape.fill.sel -side left -anchor w -padx 5

            frame $Data(Frame2).shape.icon
               label $Data(Frame2).shape.icon.lbl -text [lindex $Mapper::Lbl(Icon) $GDefs(Lang)] -width 12 -anchor w
               IcoMenu::CreateDef $Data(Frame2).shape.icon.sel $GDefs(Dir)/share/bitmap \
                  "zeroth.xbm stri.xbm ssquare.xbm scircle.xbm slos.xbm shbar.xbm svbar.xbm spenta.xbm shexa.xbm slight.xbm sx.xbm s+.xbm" $Data(Icons) \
                  Mapper::OGR::Data(Icon) { Mapper::OGR::ParamsSet $Mapper::Data(Object) } $Mapper::OGR::Data(Icon) -relief groove -bd 2
               scale $Data(Frame2).shape.icon.sz -bd 1 -relief flat -width 15 -sliderlength 10 -from 1 -to 25 -variable Mapper::OGR::Data(Size) -orient horizontal -showvalue False
               ComboBox::Create $Data(Frame2).shape.icon.fld Mapper::OGR::Data(SizeVar) noedit sorted nodouble -1 "" 1 8 ""
               entry $Data(Frame2).shape.icon.val -bd 1 -textvariable Mapper::OGR::Data(Size) -bg $GDefs(ColorLight) -width 5
               pack $Data(Frame2).shape.icon.lbl $Data(Frame2).shape.icon.sel $Data(Frame2).shape.icon.val $Data(Frame2).shape.icon.sz -side left -anchor w -padx 5
               pack $Data(Frame2).shape.icon.fld -side left -fill x -expand True -padx 5

            frame $Data(Frame2).shape.out
               label $Data(Frame2).shape.out.lbl -text [lindex $Mapper::Lbl(Out) $GDefs(Lang)] -width 12 -anchor w
               ColorBox::CreateSel $Data(Frame2).shape.out.col Mapper::OGR::Data(Color) Mapper::OGR::ParamsSet \$Mapper::Data(Object)
               IcoMenu::Create $Data(Frame2).shape.out.width $GDefs(Dir)/share/bitmap \
                  "zeroth.xbm width1.xbm width2.xbm width3.xbm width4.xbm width5.xbm" "0 1 2 3 4 5" \
                  Mapper::OGR::Data(Width) { Mapper::OGR::ParamsSet $Mapper::Data(Object) } $Mapper::OGR::Data(Width) -relief groove -bd 2
               IcoMenu::CreateDef $Data(Frame2).shape.out.dash $GDefs(Dir)/share/bitmap \
                { dash0.xbm dash1.xbm dash2.xbm dash3.xbm dash4.xbm dash5.xbm } { "" . - .- .-- .-. } \
                Mapper::OGR::Data(Dash) { Mapper::OGR::ParamsSet $Mapper::Data(Object) } $Mapper::OGR::Data(Dash) -relief groove -bd 2
               pack $Data(Frame2).shape.out.lbl $Data(Frame2).shape.out.col $Data(Frame2).shape.out.width $Data(Frame2).shape.out.dash -side left -anchor w -padx 5

            frame $Data(Frame2).shape.mask
               label $Data(Frame2).shape.mask.lbl -text [lindex $Mapper::Lbl(Mask) $GDefs(Lang)] -width 12 -anchor w
               checkbutton $Data(Frame2).shape.mask.sel -variable Mapper::OGR::Data(Mask) -relief raised -bd 1 \
                  -bitmap @$GDefs(Dir)/share/bitmap/zeroth.xbm -indicatoron false \
                  -command { Mapper::OGR::ParamsSet $Mapper::Data(Object) } -selectcolor "" -relief groove -bd 1
               pack $Data(Frame2).shape.mask.lbl $Data(Frame2).shape.mask.sel -side left -anchor w -padx 5

            frame $Data(Frame2).shape.burn
               label $Data(Frame2).shape.burn.lbl -text [lindex $Mapper::Lbl(Burn) $GDefs(Lang)] -width 12 -anchor w
               checkbutton $Data(Frame2).shape.burn.sel -variable Mapper::OGR::Data(Burn) -relief raised -bd 1 \
                  -bitmap @$GDefs(Dir)/share/bitmap/zeroth.xbm -indicatoron false -onvalue -1 -offvalue 1 \
                  -command { Mapper::OGR::ParamsSet $Mapper::Data(Object) } -selectcolor "" -relief groove -bd 1
               pack $Data(Frame2).shape.burn.lbl $Data(Frame2).shape.burn.sel -side left -anchor w -padx 5

            frame $Data(Frame2).shape.tran
               label $Data(Frame2).shape.tran.lbl -text [lindex $Mapper::Lbl(Tran) $GDefs(Lang)] -width 12 -anchor w
               scale $Data(Frame2).shape.tran.sca -bd 1 -relief flat -width 15 -sliderlength 10 -from 0 -to 100 -variable Mapper::OGR::Data(Tran) -orient horizontal -showvalue False
               entry $Data(Frame2).shape.tran.val -bd 1 -textvariable Mapper::OGR::Data(Tran) -bg $GDefs(ColorLight) -width 5
               pack $Data(Frame2).shape.tran.lbl $Data(Frame2).shape.tran.val -side left  -padx 5
               pack $Data(Frame2).shape.tran.sca -side left -fill x -expand true -padx 5

             frame $Data(Frame2).shape.label
               label $Data(Frame2).shape.label.lbl -text [lindex $Mapper::Lbl(Label) $GDefs(Lang)] -width 12 -anchor w
               ComboBox::Create $Data(Frame2).shape.label.val Mapper::OGR::Data(LabelVar) noedit sorted nodouble -1 "" 1 8 ""
               pack $Data(Frame2).shape.label.lbl -side left -anchor w -padx 5
               pack $Data(Frame2).shape.label.val -side left -fill x -padx 5 -expand true

            pack $Data(Frame2).shape.out $Data(Frame2).shape.fill $Data(Frame2).shape.icon $Data(Frame2).shape.mask $Data(Frame2).shape.burn\
                $Data(Frame2).shape.tran $Data(Frame2).shape.label -side top -anchor w -fill x

         labelframe $Data(Frame2).inter -text [lindex $Mapper::Lbl(Interval) $GDefs(Lang)]
            frame $Data(Frame2).inter.order
               label $Data(Frame2).inter.order.lbl -text [lindex $Mapper::Lbl(Value) $GDefs(Lang)] -width 13 -anchor w
               ComboBox::Create $Data(Frame2).inter.order.val Mapper::OGR::Data(Order) noedit unsorted nodouble -1 $FSTD::Param(Orders) 1 4 ""
               spinbox $Data(Frame2).inter.order.prec -textvariable Mapper::OGR::Data(Mantisse) -width 2 -from 0 -to 30 -wrap 1 -bd 1 \
                  -command "" -bg $GDefs(ColorLight)
               button $Data(Frame2).inter.order.font -relief groove -bd 2 -bitmap @$GDefs(Dir)/share/bitmap/font.ico\
                  -command "FontBox::Create $Data(Frame2).inter.order.font \"Mapper::OGR::ParamsSet \$Mapper::Data(Object)\" OGRFONT"
               pack $Data(Frame2).inter.order.lbl -side left -padx 5
               pack $Data(Frame2).inter.order.font $Data(Frame2).inter.order.prec -side left
               pack $Data(Frame2).inter.order.val -side left -fill x -expand True -padx 5

            frame $Data(Frame2).inter.map
               label $Data(Frame2).inter.map.lbl -text [lindex $Mapper::Lbl(Map) $GDefs(Lang)] -width 12 -anchor w
               button $Data(Frame2).inter.map.val -bd 1 -relief flat -image OGRMAPImg -command { MapBox::Create $Mapper::OGR::Data(Frame2).inter.map.val \
                   "Mapper::Apply \$Mapper::Data(Object)" $Mapper::OGR::Data(ColorMap) }
               pack $Data(Frame2).inter.map.lbl $Data(Frame2).inter.map.val -side left -padx 5

            frame $Data(Frame2).inter.fld
               label $Data(Frame2).inter.fld.lbl -text [lindex $Mapper::Lbl(Field) $GDefs(Lang)] -width 12 -anchor w
               ComboBox::Create $Data(Frame2).inter.fld.val Mapper::OGR::Data(MapVar) noedit sorted nodouble -1 "" 1 8 ""
               pack $Data(Frame2).inter.fld.lbl -side left -padx 5
               pack $Data(Frame2).inter.fld.val -side left -fill x -padx 5 -expand true

            frame $Data(Frame2).inter.desc
              label $Data(Frame2).inter.desc.lbl -text [lindex $Mapper::Lbl(Interval) $GDefs(Lang)] -width 12 -anchor w
              ComboBox::Create $Data(Frame2).inter.desc.val Mapper::OGR::Data(Intervals) edit sorted nodouble -1 \
                  "" 1 6 "Mapper::OGR::ParamsSet \$Mapper::Data(Object)"
              pack $Data(Frame2).inter.desc.lbl -side left -padx 5
              pack $Data(Frame2).inter.desc.val -side left -padx 5 -fill x -expand true

            pack $Data(Frame2).inter.order $Data(Frame2).inter.map $Data(Frame2).inter.fld $Data(Frame2).inter.desc \
               -side top -fill x -expand true
         pack $Data(Frame2).shape $Data(Frame2).pos $Data(Frame2).inter -side top -padx 5 -pady 5 -ipady 2 -fill both

      set Data(Frame1) [TabFrame::Add .mapperparams.tab 1 [lindex $Mapper::Lbl(Ref) $GDefs(Lang)] False ""]
         frame $Data(Frame1).projhead
            label $Data(Frame1).projhead.lbl -text [lindex $Mapper::Lbl(Projection) $GDefs(Lang)] -width 10 -anchor nw
            button $Data(Frame1).projhead.file -image OPEN -relief flat -bd 0 -overrelief raised \
               -command "Mapper::ProjFile $Data(Frame1).proj.val \[FileBox::Create . \"\" Load \[list \$FileBox::Type(PROJ) \$FileBox::Type(TXT)\]\]"
            button $Data(Frame1).projhead.tab -image INTEROGATE -relief flat -bd 0 -overrelief raised \
               -command { set Mapper::OGR::Data(Proj) [Mapper::WKT::Param $Mapper::OGR::Data(Proj)]; $Mapper::OGR::Data(Frame1).proj.val delete 0.0 end ; $Mapper::OGR::Data(Frame1).proj.val insert end $Mapper::OGR::Data(Proj); Mapper::OGR::ParamsSet $Mapper::Data(Object) }
            pack $Data(Frame1).projhead.lbl -side left
            pack $Data(Frame1).projhead.tab $Data(Frame1).projhead.file  -side left -padx 2
         labelframe $Data(Frame1).proj -labelwidget $Data(Frame1).projhead
            text $Data(Frame1).proj.val -bd 1 -bg $GDefs(ColorLight) -height 5 -width 25
            pack $Data(Frame1).proj.val -side right -fill both -expand true -padx 5 -pady 5
         pack  $Data(Frame1).proj -side top -fill both -expand true -padx 5 -pady 5

         Bubble::Create $Data(Frame1).projhead.file $Mapper::Bubble(ProjLoad)
         Bubble::Create $Data(Frame1).projhead.tab  $Mapper::Bubble(ProjRef)

      set Data(Frame3) [TabFrame::Add .mapperparams.tab 1 [lindex $Mapper::Lbl(Feature) $GDefs(Lang)] False ""]

         frame $Data(Frame3).sel
            checkbutton $Data(Frame3).sel.zoom -variable Mapper::OGR::Data(SelMode) -onvalue Zoom -offvalue None -image MODEZOOM -indicatoron 0 -relief sunken -bd 1 -anchor w -overrelief raised -offrelief flat -command { Mapper::OGR::FeatureGoTo $Mapper::OGR::Data(Index) }
            checkbutton $Data(Frame3).sel.move -variable Mapper::OGR::Data(SelMode) -onvalue Locate -offvalue None -image FINGER -indicatoron 0 -relief sunken -bd 1 -anchor w -overrelief raised -offrelief flat -command { Mapper::OGR::FeatureGoTo $Mapper::OGR::Data(Index) }
            checkbutton $Data(Frame3).sel.mode -variable Mapper::OGR::Data(Edit) -onvalue True -offvalue False -width 30 -anchor w \
               -image ARROWPOLY -indicatoron 0 -relief sunken -bd 1 -relief flat -overrelief raised -overrelief raised -offrelief flat -selectcolor $GDefs(ColorFrame) \
               -command { Mapper::OGR::VertexInit $Page::Data(Frame) $Mapper::Data(Object) $Mapper::OGR::Data(Index) }
            menubutton $Data(Frame3).sel.mode.opt -image OPTIONS -relief flat -bd 0 -menu $Data(Frame3).sel.mode.opt.menu
            place $Data(Frame3).sel.mode.opt -relx 1.0 -rely 0.0 -anchor ne -relheight 1.0
            menu $Data(Frame3).sel.mode.opt.menu
            $Data(Frame3).sel.mode.opt.menu add command -label [lindex $Mapper::Lbl(AddRing) $GDefs(Lang)] -command { Mapper::OGR::VertexAddItem $Mapper::OGR::Data(Geom) }
            $Data(Frame3).sel.mode.opt.menu add command -label [lindex $Mapper::Lbl(AddGeom) $GDefs(Lang)] -command { Mapper::OGR::VertexAddItem $Mapper::OGR::Data(Geom) }
            $Data(Frame3).sel.mode.opt.menu add separator
            $Data(Frame3).sel.mode.opt.menu add command -label [lindex $Mapper::Lbl(DelRing) $GDefs(Lang)] -command { Mapper::OGR::VertexDelItem $Mapper::OGR::Data(Geom) }
            $Data(Frame3).sel.mode.opt.menu add command -label [lindex $Mapper::Lbl(DelGeom) $GDefs(Lang)] -command { Mapper::OGR::VertexDelItem $Mapper::OGR::Data(Geom) }
            $Data(Frame3).sel.mode.opt.menu add separator
            $Data(Frame3).sel.mode.opt.menu add radiobutton -label [lindex $Mapper::Lbl(SnapOff) $GDefs(Lang)] -variable Mapper::OGR::Data(Snap) -value 0
            $Data(Frame3).sel.mode.opt.menu add radiobutton -label [lindex $Mapper::Lbl(Snap10) $GDefs(Lang)] -variable Mapper::OGR::Data(Snap) -value 10
            $Data(Frame3).sel.mode.opt.menu add radiobutton -label [lindex $Mapper::Lbl(Snap20) $GDefs(Lang)] -variable Mapper::OGR::Data(Snap) -value 20
            $Data(Frame3).sel.mode.opt.menu add separator
            $Data(Frame3).sel.mode.opt.menu add command -label [lindex $Mapper::Lbl(Invalid) $GDefs(Lang)] \
               -command { ogrlayer define $Mapper::Data(Object) -featurehighlight [ogrlayer stats $Mapper::Data(Object) -invalid]; Page::Update $Page::Data(Frame) }

            button $Data(Frame3).sel.add -image PLUS -relief flat -bd 1 -overrelief raised -command { Mapper::OGR::FeatureAdd $Mapper::Data(Object) }
            button $Data(Frame3).sel.del -image DELETE -relief flat -bd 1 -overrelief raised -command { Mapper::OGR::FeatureDel $Mapper::Data(Object) $Mapper::OGR::Data(Index) }
            label $Data(Frame3).sel.lbl -text [lindex $Mapper::Lbl(Index) $GDefs(Lang)] -width 7 -anchor w
            spinbox $Data(Frame3).sel.val -textvariable Mapper::OGR::Data(Index) -width 20 -from 0 -to 0 -wrap True -bd 1 \
               -command { after 1 "Mapper::OGR::Feature $Mapper::Data(Object) %s True" } -bg $GDefs(ColorLight)
            pack $Data(Frame3).sel.lbl -side left
            pack $Data(Frame3).sel.val -side left -fill x -padx 5 -expand True
            pack $Data(Frame3).sel.add $Data(Frame3).sel.del $Data(Frame3).sel.mode $Data(Frame3).sel.move $Data(Frame3).sel.zoom -side left
         labelframe $Data(Frame3).fields -text [lindex $Mapper::Lbl(Field) $GDefs(Lang)]
            scrollbar $Data(Frame3).fields.scrolly -relief sunken -command "$Data(Frame3).fields.table yview" -bd 1 -width 10
            table $Data(Frame3).fields.table -relief sunken -bd 1 -bg $GDefs(ColorLight) -titlecols 1 -height 1 -variable Mapper::OGR::TableField\
               -highlightbackground $GDefs(ColorHighLight) -rows 0 -cols 0 -colwidth 13 -colstretchmode last -multiline False -drawmode fast \
               -yscrollcommand "$Data(Frame3).fields.scrolly set" -resizeborders none -selectmode single -ellipsis ... -anchor w \
               -validate 1 -vcmd { Mapper::OGR::FeatureField $Mapper::Data(Object) $Mapper::OGR::Data(Index) %r %S }
            pack $Data(Frame3).fields.scrolly -side left -fill y
            pack $Data(Frame3).fields.table -side left -fill both -expand true -before $Data(Frame3).fields.scrolly
            
        frame $Data(Frame3).geomtype
            checkbutton $Data(Frame3).geomtype.lbl -text "[lindex $Mapper::Lbl(Shape) $GDefs(Lang)]:" -anchor w -variable Mapper::OGR::Data(GeomGet) -onvalue True -offvalue False \
               -command { Mapper::OGR::Feature $Mapper::Data(Object) $Mapper::OGR::Data(Index) }
            menubutton $Data(Frame3).geomtype.type -textvariable Mapper::OGR::Data(GeomOut) -menu $Data(Frame3).geomtype.type.menu -relief groove -bd 2
            menu $Data(Frame3).geomtype.type.menu
               $Data(Frame3).geomtype.type.menu add radiobutton -variable Mapper::OGR::Data(GeomOut) -label TEXT -value TEXT -command { Mapper::OGR::Feature $Mapper::Data(Object) $Mapper::OGR::Data(Index) }
               $Data(Frame3).geomtype.type.menu add radiobutton -variable Mapper::OGR::Data(GeomOut) -label WKT -value WKT -command { Mapper::OGR::Feature $Mapper::Data(Object) $Mapper::OGR::Data(Index) }
               $Data(Frame3).geomtype.type.menu add radiobutton -variable Mapper::OGR::Data(GeomOut) -label KML -value KML -command { Mapper::OGR::Feature $Mapper::Data(Object) $Mapper::OGR::Data(Index) }
               $Data(Frame3).geomtype.type.menu add radiobutton -variable Mapper::OGR::Data(GeomOut) -label GML -value GML -command { Mapper::OGR::Feature $Mapper::Data(Object) $Mapper::OGR::Data(Index) }
               $Data(Frame3).geomtype.type.menu add radiobutton -variable Mapper::OGR::Data(GeomOut) -label JSON -value JSON -command { Mapper::OGR::Feature $Mapper::Data(Object) $Mapper::OGR::Data(Index) }
            pack  $Data(Frame3).geomtype.lbl $Data(Frame3).geomtype.type -side left

         labelframe $Data(Frame3).geom -labelwidget $Data(Frame3).geomtype
            text $Data(Frame3).geom.text -relief sunken -bd 1 -wrap none -bg $GDefs(ColorLight) -yscrollcommand "$Data(Frame3).geom.scroll set" -width 0 -height 0
            scrollbar $Data(Frame3).geom.scroll -relief sunken -command "$Data(Frame3).geom.text yview" -bd 1 -width 10
            pack $Data(Frame3).geom.text -side left -fill both -expand true
            pack $Data(Frame3).geom.scroll -side left -fill y
         pack $Data(Frame3).sel -side top -fill x -padx 5 -pady 5
         pack $Data(Frame3).fields -side top -fill both -expand true -padx 5 -pady 5
         pack $Data(Frame3).geom -side top -fill both -expand true -padx 5 -pady 5

         Bubble::Create $Data(Frame3).sel.mode $Mapper::Bubble(OGREdit)
         Bubble::Create $Data(Frame3).sel.add  $Mapper::Bubble(OGRAdd)
         Bubble::Create $Data(Frame3).sel.del  $Mapper::Bubble(OGRDel)
         Bubble::Create $Data(Frame3).sel.move $Mapper::Bubble(LocateSel)
         Bubble::Create $Data(Frame3).sel.zoom $Mapper::Bubble(ZoomSel)

      set Data(Frame5) [TabFrame::Add .mapperparams.tab 1 [lindex $Mapper::Lbl(Table) $GDefs(Lang)] False ""]

         frame $Data(Frame5).func
            button $Data(Frame5).func.add -image GRIDADD -bd 1 -relief flat -overrelief raised -command { Mapper::OGR::FieldAdd }
            button $Data(Frame5).func.del -image GRIDREM -bd 1 -relief flat -overrelief raised -command { Mapper::OGR::TableColumnDel $Mapper::Data(Object) $Mapper::OGR::Data(TableSort) }
            button $Data(Frame5).func.calc -image CALC -bd 1 -relief flat -overrelief raised -command { Mapper::OGR::FieldCalc }
            checkbutton $Data(Frame5).func.zoom -variable Mapper::OGR::Data(SelMode) -onvalue Zoom -offvalue None -image MODEZOOM -indicatoron 0 -relief sunken -bd 1 -anchor w -overrelief raised -offrelief flat -command { Mapper::OGR::FeatureGoTo $Mapper::OGR::Data(Index) }
            checkbutton $Data(Frame5).func.move -variable Mapper::OGR::Data(SelMode) -onvalue Locate -offvalue None -image FINGER -indicatoron 0 -relief sunken -bd 1 -anchor w -overrelief raised -offrelief flat -command { Mapper::OGR::FeatureGoTo $Mapper::OGR::Data(Index) }
            pack $Data(Frame5).func.add $Data(Frame5).func.del $Data(Frame5).func.calc $Data(Frame5).func.move $Data(Frame5).func.zoom -side left
         pack $Data(Frame5).func -side top -fill x -padx 5 -pady 5

         frame $Data(Frame5).search -relief sunken -bd 1 
            table $Data(Frame5).search.table -relief sunken -bd 1 -bg $GDefs(ColorLight) \
            -resizeborders none -anchor w -highlightbackground $GDefs(ColorHighLight) -titlecols 1 -rows 1 -cols 1 -colwidth 13 -multiline False -drawmode fast \
            -xscrollcommand "$Data(Frame5).pan.stat.scrollx set" -width 1 -height 1 -selectmode extended 
            pack $Data(Frame5).search.table -side left -fill both -expand true
         pack $Data(Frame5).search -side top -fill x -padx 5 
         
         button $Data(Frame5).search.table.clear -text [lindex $Mapper::Lbl(Select) $GDefs(Lang)]  -relief raised -bd 1 -command { Mapper::OGR::SelectClear $Mapper::Data(Object) }
         $Data(Frame5).search.table window configure 0,0 -window $Data(Frame5).search.table.clear -sticky nsew
        
         panedwindow $Data(Frame5).pan -orient vertical -showhandle False -opaqueresize False -relief flat -bd 0 -sashrelief sunken
     
         frame $Data(Frame5).pan.meta 
               
            frame $Data(Frame5).pan.meta.sub -relief sunken -bd 1
               scrollbar $Data(Frame5).pan.meta.sub.scrolly -relief sunken -command "Mapper::OGR::TableYScroll $Mapper::Data(Object)" -bd 1 -width 10
               table $Data(Frame5).pan.meta.sub.table -relief sunken -bd 1 -bg $GDefs(ColorLight) -titlecols 1 -titlerows 1 -variable Mapper::OGR::Table \
                  -resizeborders col -anchor w -highlightbackground $GDefs(ColorHighLight)  -rows 1 -cols 1 -colwidth 13 -multiline False -drawmode fast \
                  -yscrollcommand "$Data(Frame5).pan.meta.sub.scrolly set" -xscrollcommand "$Data(Frame5).pan.stat.scrollx set" -width 1 -height 1 -selectmode extended \
                  -validate 1 -vcmd { Mapper::OGR::FeatureFieldTable $Mapper::Data(Object) %r %c %S }
               pack $Data(Frame5).pan.meta.sub.scrolly -side left -fill y
               pack $Data(Frame5).pan.meta.sub.table -side left -fill both -expand true -before $Data(Frame5).pan.meta.sub.scrolly
            pack $Data(Frame5).pan.meta.sub -side top -fill both -expand True
            
         frame $Data(Frame5).pan.stat -relief sunken -bd 1 
            scrollbar $Data(Frame5).pan.stat.scrolly -relief sunken -command "$Data(Frame5).pan.stat.table yview " -bd 1 -width 10
            scrollbar $Data(Frame5).pan.stat.scrollx -relief sunken -command "Mapper::OGR::TableXScroll" -bd 1 -width 10 -orient horizontal
            table $Data(Frame5).pan.stat.table -relief sunken -bd 1 -bg $GDefs(ColorLight) -titlecols 1 -variable Mapper::OGR::Stats \
            -resizeborders none -anchor w -highlightbackground $GDefs(ColorHighLight)  -rows 1 -cols 1 -colwidth 13 -multiline False -drawmode fast \
            -yscrollcommand "$Data(Frame5).pan.stat.scrolly set" -xscrollcommand "$Data(Frame5).pan.stat.scrollx set" -width 1 -height 1 -selectmode extended 
            pack $Data(Frame5).pan.stat.scrollx -side bottom -fill x
            pack $Data(Frame5).pan.stat.scrolly -side left -fill y
            pack $Data(Frame5).pan.stat.table -side left -fill both -expand true -before $Data(Frame5).pan.stat.scrolly
         pack $Data(Frame5).pan -side top -fill both -expand true -padx 5 -pady 5

         $Data(Frame5).pan add $Data(Frame5).pan.meta -height 100 -stretch first
         $Data(Frame5).pan add $Data(Frame5).pan.stat 
      
      Bubble::Create $Data(Frame5).search.table.clear $Mapper::Lbl(SelectClear)
      Bubble::Create $Data(Frame5).func.add $Mapper::Bubble(ColAdd)
      Bubble::Create $Data(Frame5).func.del $Mapper::Bubble(ColDel)
      Bubble::Create $Data(Frame5).func.calc $Mapper::Bubble(ColCalc)
      Bubble::Create $Data(Frame5).func.move $Mapper::Bubble(LocateSel)
      Bubble::Create $Data(Frame5).func.zoom $Mapper::Bubble(ZoomSel)

      bind $Data(Frame5).pan.meta.sub.table <Expose>        { Mapper::OGR::Table $Mapper::Data(Object) False }
      bind $Data(Frame5).pan.meta.sub.table <Configure>     { Mapper::OGR::Table $Mapper::Data(Object) False }
      bind $Data(Frame5).pan.meta.sub.table <Home>          { Mapper::OGR::Table $Mapper::Data(Object) False }
      bind $Data(Frame5).pan.meta.sub.table <End>           { Mapper::OGR::Table $Mapper::Data(Object) False }
      bind $Data(Frame5).pan.meta.sub.table <Control-Home>       { Mapper::OGR::Table $Mapper::Data(Object) False }
      bind $Data(Frame5).pan.meta.sub.table <Control-End>        { Mapper::OGR::Table $Mapper::Data(Object) False }
      bind $Data(Frame5).pan.meta.sub.table <Shift-Control-Home> { Mapper::OGR::Table $Mapper::Data(Object) False }
      bind $Data(Frame5).pan.meta.sub.table <Shift-Control-End>  { Mapper::OGR::Table $Mapper::Data(Object) False }
      bind $Data(Frame5).pan.meta.sub.table <Up>            { Mapper::OGR::Table $Mapper::Data(Object) False }
      bind $Data(Frame5).pan.meta.sub.table <Down>          { Mapper::OGR::Table $Mapper::Data(Object) False }
      bind $Data(Frame5).pan.meta.sub.table <Shift-Up>      { Mapper::OGR::Table $Mapper::Data(Object) False }
      bind $Data(Frame5).pan.meta.sub.table <Shift-Down>    { Mapper::OGR::Table $Mapper::Data(Object) False }
      bind $Data(Frame5).pan.meta.sub.table <Shift-Up>      { Mapper::OGR::Table $Mapper::Data(Object) False }
      bind $Data(Frame5).pan.meta.sub.table <Shift-Down>    { Mapper::OGR::Table $Mapper::Data(Object) False }
      bind $Data(Frame5).pan.meta.sub.table <B1-Motion>     { Mapper::OGR::Table $Mapper::Data(Object) False }
      bind $Data(Frame5).pan.meta.sub.table <B2-Motion>     { Mapper::OGR::Table $Mapper::Data(Object) False }
      bind $Data(Frame5).pan.meta.sub.table <ButtonPress-4> { $Mapper::OGR::Data(Frame5).pan.meta.sub.table yview scroll -1 units; Mapper::OGR::Table $Mapper::Data(Object) False }
      bind $Data(Frame5).pan.meta.sub.table <ButtonPress-5> { $Mapper::OGR::Data(Frame5).pan.meta.sub.table yview scroll 1 units; Mapper::OGR::Table $Mapper::Data(Object) False }

      frame .mapperparams.com
         button .mapperparams.com.apply  -text [lindex $Mapper::Lbl(Apply) $GDefs(Lang)] -bd 1 -command { Mapper::OGR::ParamsSet $Mapper::Data(Object) }
         button .mapperparams.com.cancel -text [lindex $Mapper::Lbl(Close) $GDefs(Lang)] -bd 1 -command { Mapper::ParamsClose }
         pack .mapperparams.com.cancel .mapperparams.com.apply -side right
      pack .mapperparams.com -side top -fill x -padx 2 -pady 2

      bind $Data(Frame3).sel.val <Return> { Mapper::OGR::Feature $Mapper::Data(Object) $Mapper::OGR::Data(Index) True }

      if { ![llength $Tabs] } {
         set Tabs 0
      }
   }

   if { [llength $Tabs] && [lsearch -exact $Tabs [TabFrame::Current .mapperparams.tab]]==-1 } {
      TabFrame::Select .mapperparams.tab [lindex $Tabs 0]
   }

   $Data(Frame1).proj.val delete 0.0 end
   $Data(Frame1).proj.val insert end $Data(Proj)

   ColorBox::ConfigNoColor $Data(Frame2).shape.out.col $Data(Color)
   ColorBox::ConfigNoColor $Data(Frame2).shape.fill.val $Data(Fill)

   if { [set nb [ogrlayer define $Mapper::Data(Object) -nb]] } {
      if { $Data(Index)>$nb } {
         set Data(Index) 0
      }
      $Data(Frame3).sel.val configure  -state normal -to [expr $nb-1]
      $Data(Frame3).sel.mode configure -state normal
   } else {
      $Data(Frame3).sel.val configure -state disabled
      $Data(Frame3).sel.mode configure -state disabled
   }

   ComboBox::DelAll $Data(Frame2).shape.icon.fld False
   ComboBox::Add $Data(Frame2).shape.icon.fld {}
   ComboBox::AddList $Data(Frame2).shape.icon.fld $Mapper::OGR::Data(Fields)

   ComboBox::DelAll $Data(Frame2).shape.label.val False
   ComboBox::Add $Data(Frame2).shape.label.val {}
   ComboBox::AddList $Data(Frame2).shape.label.val $Mapper::OGR::Data(Fields)

   ComboBox::DelAll $Data(Frame2).inter.fld.val False
   ComboBox::Add $Data(Frame2).inter.fld.val {}
   ComboBox::AddList $Data(Frame2).inter.fld.val $Mapper::OGR::Data(Fields)

   ComboBox::DelAll  $Data(Frame2).pos.extr.sel False
   ComboBox::Add  $Data(Frame2).pos.extr.sel {}
   ComboBox::AddList  $Data(Frame2).pos.extr.sel $Mapper::OGR::Data(Fields)

   ComboBox::DelAll  $Data(Frame2).pos.topo.sel False
   ComboBox::AddList  $Data(Frame2).pos.topo.sel [concat $Mapper::OGR::Data(Topos) $Mapper::OGR::Data(Fields)]

   IcoMenu::Set $Data(Frame2).shape.out.width $Data(Width)
   catch { IcoMenu::Set $Data(Frame2).shape.out.dash  $Data(Dash) }

   set Data(ColorMap) [ogrlayer configure $Object -colormap]
   catch { colormap image $Data(ColorMap) OGRMAPImg }

   Mapper::OGR::Feature $Object $Data(Index)
   Mapper::OGR::Table $Object True $Data(Index) 
}

#-------------------------------------------------------------------------------
# Nom      : <Mapper::OGR::New>
# Creation : Fevrier 2014 - J.P. Gauthier - CMC/CMOE
#
# But      : Interface de création de nouvelles couches OGR
#
# Parametres :
#
# Retour    :
#
# Remarque :
#
#-------------------------------------------------------------------------------

proc Mapper::OGR::New { } {
   global GDefs
   variable Data
   
   toplevel .mappernew

   wm title     .mappernew "[lindex $Mapper::Lbl(NewLayer) $GDefs(Lang)]"
   wm transient .mappernew .mapper
   wm geometry  .mappernew =400x500
   wm protocol  .mappernew WM_DELETE_WINDOW { destroy .mappernew }

   frame .mappernew.opt -relief raised -bd 1
   
      labelframe .mappernew.opt.layer -text [lindex $Mapper::Lbl(Layer) $GDefs(Lang)]
         frame .mappernew.opt.layer.name
            label .mappernew.opt.layer.name.lbl -text [lindex $Mapper::Lbl(Name) $GDefs(Lang)] -width 10 -anchor nw
            entry .mappernew.opt.layer.name.sel -textvariable Mapper::OGR::Data(Name) -relief sunken -bg $GDefs(ColorLight)
            pack  .mappernew.opt.layer.name.lbl -side left 
            pack  .mappernew.opt.layer.name.sel -side left -fill both -expand true 
         pack .mappernew.opt.layer.name -side top -fill x -padx 2
      
      labelframe .mappernew.opt.geom -text [lindex $Mapper::Lbl(Shape) $GDefs(Lang)]
         frame .mappernew.opt.geom.type
            label .mappernew.opt.geom.type.lbl -text [lindex $Mapper::Lbl(Type) $GDefs(Lang)] -width 10 -anchor nw
            ComboBox::Create .mappernew.opt.geom.type.sel Mapper::OGR::Data(GeomType) noedit unsorted nodouble -1 $Mapper::OGR::Data(GeomTypes) 1 8 ""
            pack  .mappernew.opt.geom.type.lbl -side left 
            pack  .mappernew.opt.geom.type.sel -side left -fill both -expand true
         frame .mappernew.opt.geom.dim
            label .mappernew.opt.geom.dim.lbl -text [lindex $Mapper::Lbl(Dim) $GDefs(Lang)] -width 10 -anchor nw
            radiobutton .mappernew.opt.geom.dim.sel2 -relief sunken -bd 1 -overrelief raised -offrelief flat -indicatoron False -variable Mapper::OGR::Data(3D) -value False -text 2D
            radiobutton .mappernew.opt.geom.dim.sel3 -relief sunken -bd 1 -overrelief raised -offrelief flat -indicatoron False -variable Mapper::OGR::Data(3D) -value True -text 3D
            pack  .mappernew.opt.geom.dim.lbl -side left 
            pack  .mappernew.opt.geom.dim.sel2 .mappernew.opt.geom.dim.sel3 -side left -fill both -expand true
         pack .mappernew.opt.geom.type .mappernew.opt.geom.dim -side top -fill x -padx 2
         
      labelframe .mappernew.opt.att -text [lindex $Mapper::Lbl(Attribute) $GDefs(Lang)]
         frame .mappernew.opt.att.fields -relief sunken -bd 1 -bg $GDefs(ColorLight)
            frame .mappernew.opt.att.fields.head
               button .mappernew.opt.att.fields.head.add -text + -relief raised -bd 1 -command Mapper::OGR::FieldNew 
               label .mappernew.opt.att.fields.head.name -relief raised -bd 1 -text [lindex $Mapper::Lbl(Name) $GDefs(Lang)] -width 1
               label .mappernew.opt.att.fields.head.type -relief raised -bd 1 -text [lindex $Mapper::Lbl(Type) $GDefs(Lang)] -width 15
               label .mappernew.opt.att.fields.head.width -relief raised -bd 1 -text [lindex $Mapper::Lbl(Width) $GDefs(Lang)] -width 10
               pack .mappernew.opt.att.fields.head.add -side left 
               pack .mappernew.opt.att.fields.head.name -side left -fill both -expand True
               pack .mappernew.opt.att.fields.head.type .mappernew.opt.att.fields.head.width -side left -fill y
            pack .mappernew.opt.att.fields.head -side top -fill x
         pack .mappernew.opt.att.fields -side top -fill both -expand True -padx 2 -pady 2
      pack .mappernew.opt.layer -side top -fill x -padx 5 -pady 5
      pack .mappernew.opt.geom -side top -fill x -padx 5
      pack .mappernew.opt.att -side top -fill both -expand True -pady 5 -padx 5
      
      frame .mappernew.opt.cmd -relief sunken -bd 1
         button .mappernew.opt.cmd.cancel -relief raised -bd 1 -text [lindex $Mapper::Lbl(Cancel) $GDefs(Lang)] -command { destroy .mappernew }
         button .mappernew.opt.cmd.apply -relief raised -bd 1 -text [lindex $Mapper::Lbl(Apply) $GDefs(Lang)] -command { Mapper::OGR::Create }
         pack .mappernew.opt.cmd.cancel .mappernew.opt.cmd.apply -side left -fill x -expand True
      pack .mappernew.opt.cmd -side top -fill x -padx 5 -pady 5
      
   pack .mappernew.opt -side top -fill both -expand True
   
   set Data(FieldNo) -1
   
   grab .mappernew
}

#-------------------------------------------------------------------------------
# Nom      : <Mapper::OGR::FieldNew>
# Creation : Fevrier 2014 - J.P. Gauthier - CMC/CMOE
#
# But      : Ajouter les widgets de définition pour un nouveau champs dans l'interface
#
# Parametres :
#
# Retour    :
#
# Remarque :
#
#-------------------------------------------------------------------------------

proc Mapper::OGR::FieldNew { } {
   global GDefs
   variable Data
   
   incr Data(FieldNo)
   
   frame .mappernew.opt.att.fields.f$Data(FieldNo)
      button .mappernew.opt.att.fields.f$Data(FieldNo).add -text - -relief raised -bd 1 -command "destroy .mappernew.opt.att.fields.f$Data(FieldNo)"
      entry .mappernew.opt.att.fields.f$Data(FieldNo).name -relief raised -bd 1  -width 1 -bg $GDefs(ColorLight)
      menubutton .mappernew.opt.att.fields.f$Data(FieldNo).type -relief raised -bd 1  -width 14 -bg $GDefs(ColorLight) -textvariable Mapper::OGR::Data(FieldType$Data(FieldNo)) \
         -menu .mappernew.opt.att.fields.f$Data(FieldNo).type.menu
      menu .mappernew.opt.att.fields.f$Data(FieldNo).type.menu
      foreach type $Data(FieldTypes) {
         .mappernew.opt.att.fields.f$Data(FieldNo).type.menu add radiobutton -label $type -indicatoron false -variable Mapper::OGR::Data(FieldType$Data(FieldNo)) -value $type
      }
      entry .mappernew.opt.att.fields.f$Data(FieldNo).width -relief raised -bd 1  -width 10 -bg $GDefs(ColorLight)
      pack .mappernew.opt.att.fields.f$Data(FieldNo).add -side left
      pack .mappernew.opt.att.fields.f$Data(FieldNo).name -side left -fill both -expand True
      pack .mappernew.opt.att.fields.f$Data(FieldNo).type .mappernew.opt.att.fields.f$Data(FieldNo).width -fill y -side left
   pack .mappernew.opt.att.fields.f$Data(FieldNo) -side top -fill x
}

proc Mapper::OGR::FieldAdd { } {
   global GDefs
   variable Data
   
   toplevel .mappernew

   wm title     .mappernew "[lindex $Mapper::Lbl(ColAdd) $GDefs(Lang)]"
   wm transient .mappernew .mapper
   wm resizable .mappernew 0 0
   wm protocol  .mappernew WM_DELETE_WINDOW { destroy .mappernew }

   frame .mappernew.opt -relief raised -bd 1 
      label .mappernew.opt.spc -relief flat -text ""
      frame .mappernew.opt.lbl -relief sunken -bd 1
         label .mappernew.opt.lbl.name  -text [lindex $Mapper::Lbl(Name) $GDefs(Lang)] -width 15 -relief raised -bd 1 
         label .mappernew.opt.lbl.type  -text [lindex $Mapper::Lbl(Type) $GDefs(Lang)] -width 15 -relief raised -bd 1
         label .mappernew.opt.lbl.width -text [lindex $Mapper::Lbl(Width) $GDefs(Lang)] -width 10 -relief raised -bd 1
         pack .mappernew.opt.lbl.name  -side left -fill x -expand True -ipady 2
         pack .mappernew.opt.lbl.type .mappernew.opt.lbl.width -side left -ipady 2
         
      frame .mappernew.opt.sel -relief sunken -bd 1     
         entry .mappernew.opt.sel.name -relief raised -bd 1 -bg $GDefs(ColorLight) -textvariable Mapper::OGR::Data(FieldName) -width 15
         menubutton .mappernew.opt.sel.type -relief raised -bd 1 -bg $GDefs(ColorLight) -textvariable Mapper::OGR::Data(FieldType) -menu .mappernew.opt.sel.type.menu -width 14
         menu .mappernew.opt.sel.type.menu
         foreach type $Data(FieldTypes) {
            .mappernew.opt.sel.type.menu add radiobutton -label $type -indicatoron false -variable Mapper::OGR::Data(FieldType) -value $type
         }      
         entry .mappernew.opt.sel.width -relief raised -bd 1 -bg $GDefs(ColorLight) -textvariable Mapper::OGR::Data(FieldWidth) -width 10
         pack .mappernew.opt.sel.name -side left -fill both -expand True
         pack .mappernew.opt.sel.type .mappernew.opt.sel.width -side left -fill both
      pack .mappernew.opt.spc .mappernew.opt.lbl .mappernew.opt.sel -side top -fill x -padx 5
   pack .mappernew.opt -side top -fill x

   frame .mappernew.opt.cmd -relief sunken -bd 1
      button .mappernew.opt.cmd.cancel -relief raised -bd 1 -text [lindex $Mapper::Lbl(Cancel) $GDefs(Lang)] -command { destroy .mappernew }
      button .mappernew.opt.cmd.apply -relief raised -bd 1 -text [lindex $Mapper::Lbl(Apply) $GDefs(Lang)] -command { Mapper::OGR::TableColumnAdd $Mapper::Data(Object) $Mapper::OGR::Data(FieldName) $Mapper::OGR::Data(FieldType) $Mapper::OGR::Data(FieldWidth) }
      pack .mappernew.opt.cmd.cancel .mappernew.opt.cmd.apply -side left -fill x -expand True
   pack .mappernew.opt.cmd -side top -fill x -pady 5 -padx 5
    
    grab .mappernew
}

proc Mapper::OGR::FieldCalc { } {
   global GDefs
   variable Data
   
   toplevel .mappernew

   wm title     .mappernew "[lindex $Mapper::Lbl(Expr) $GDefs(Lang)]"
   wm transient .mappernew .mapper
   wm resizable .mappernew 1 0
   wm protocol  .mappernew WM_DELETE_WINDOW { destroy .mappernew }
   
   foreach field [ogrlayer define $Mapper::Data(Object) -field] { 
      set type [lindex [ogrlayer define $Mapper::Data(Object) -field $field] 0]
      if { $type=="Integer" || $type=="Real"  } {
         lappend list $field
      }
   }
   
   frame .mappernew.opt -relief raised -bd 1 
      label .mappernew.opt.spc -relief flat -text ""
      frame .mappernew.opt.expr
         label .mappernew.opt.expr.lbl -text [lindex $Mapper::Lbl(Expr) $GDefs(Lang)] -width 18 -anchor w
         entry .mappernew.opt.expr.sel -relief sunken -bd 1 -bg $GDefs(ColorLight) -textvariable Mapper::OGR::Data(FieldExpr) -width 30
         pack .mappernew.opt.expr.lbl -side left
         pack .mappernew.opt.expr.sel -side left -fill x -expand True     
      
      frame .mappernew.opt.field
         label .mappernew.opt.field.lbl -text [lindex $Mapper::Lbl(FieldExpr) $GDefs(Lang)] -width 18 -anchor w
         ComboBox::Create .mappernew.opt.field.sel Mapper::OGR::Data(FieldName) edit unsorted nodouble -1 $list 1 10 ""
         pack .mappernew.opt.field.lbl -side left
         pack .mappernew.opt.field.sel -side left -fill x -expand True     
   
      frame .mappernew.opt.cmd -relief sunken -bd 1
         button .mappernew.opt.cmd.cancel -relief raised -bd 1 -text [lindex $Mapper::Lbl(Cancel) $GDefs(Lang)] -command { destroy .mappernew }
         button .mappernew.opt.cmd.apply -relief raised -bd 1 -text [lindex $Mapper::Lbl(Apply) $GDefs(Lang)] -command { Mapper::OGR::TableColumnCalc $Mapper::Data(Object) $Mapper::OGR::Data(FieldName) $Mapper::OGR::Data(FieldExpr) }
         pack .mappernew.opt.cmd.cancel .mappernew.opt.cmd.apply -side left -fill x -expand True

      pack .mappernew.opt.spc .mappernew.opt.expr .mappernew.opt.field  -side top -fill x -padx 5
      pack .mappernew.opt.cmd -side top -fill x -pady 5 -padx 5
   pack .mappernew.opt -side top -fill x
    
   Bubble::Create .mappernew.opt.expr  $Mapper::Bubble(Calc)
   Bubble::Create .mappernew.opt.field $Mapper::Bubble(CalcRes)

   grab .mappernew
}

#-------------------------------------------------------------------------------
# Nom      : <Mapper::OGR::Create>
# Creation : Fevrier 2014 - J.P. Gauthier - CMC/CMOE
#
# But      : Création de la nouvelle couche OGR
#
# Parametres :
#
# Retour    :
#
# Remarque :
#
#-------------------------------------------------------------------------------

proc Mapper::OGR::Create { } {
   variable Data

   if { [set layer $Data(Name)]=="" } {
      Dialog::Error .mappernew $Mapper::Msg(LayerName)
      return
   }
   
   ogrlayer free $layer
   ogrlayer new $layer $Data(Name) $Data(GeomType)

   for { set n 0 } { $n <= $Data(FieldNo) } { incr n }  {
      if { [winfo exists .mappernew.opt.att.fields.f$n] } {
         set width [.mappernew.opt.att.fields.f$n.width get]
         if { $width=="" } {
            set width 0
         }
         ogrlayer define $layer -field [string toupper [.mappernew.opt.att.fields.f$n.name get]] $Data(FieldType$n) $width
      }
   }
   
   destroy .mappernew
   
   Mapper::OGR::Config $Page::Data(Frame) $layer
   Mapper::UpdateData $Page::Data(Frame)
}

#-------------------------------------------------------------------------------
# Nom      : <Mapper::OGR::Config>
# Creation : Fevrier 2014 - J.P. Gauthier - CMC/CMOE
#
# But      : Configuration de base pour l'affichage d'une couche
#
# Parametres :
#   <Frame>  : Page 
#   <Object  : Couche
#
# Retour    :
#
# Remarque :
#
#-------------------------------------------------------------------------------

proc Mapper::OGR::Config { Frame Object } {

   ogrlayer configure $Object -font OGRFONT -activeoutline yellow -width 1 -noselecttransparency 10

   if { [ogrlayer define $Object -space]==2 } {
      ogrlayer configure $Object -outline black -fill [Mapper::GetColor]
   } else {
      ogrlayer configure $Object -outline [Mapper::GetColor]
   }

   if { ![colormap is $Object] } {
      colormap create $Object
      colormap copy $Object OGRMAPDEFAULT
      ogrlayer configure $Object -colormap $Object
   }
   
   if { [lsearch -exact $Viewport::Data(Data$Frame) $Object]==-1 } {
      lappend Viewport::Data(Data$Frame) $Object
   }
}

#-------------------------------------------------------------------------------
# Nom      : <Mapper::OGR::ParamsGet>
# Creation : Juillet 2004 - J.P. Gauthier - CMC/CMOE
#
# But      : Recuperer les parametres de donnees vectorielle OGR
#
# Parametres :
#   <Object> : Donnee geographique a parametrer
#
# Retour    :
#
# Remarque :
#
#-------------------------------------------------------------------------------

proc Mapper::OGR::ParamsGet { Object } {
   global   GDefs
   variable Data

   set Data(Fill) black

#   set Data(Font)        [ogrlayer configure $Object -font]
   set Data(Dash)        [ogrlayer configure $Object -dash]
   set Data(Icon)        [ogrlayer configure $Object -icon]
   set Data(Size)        [ogrlayer configure $Object -size]
   set Data(LabelVar)    [ogrlayer configure $Object -labelvar]
   set Data(SizeVar)     [ogrlayer configure $Object -sizevar]
   set Data(MapVar)      [ogrlayer configure $Object -mapvar]
   set Data(Topo)        [ogrlayer configure $Object -topography]
   set Data(TopoFactor)  [ogrlayer configure $Object -topographyfactor]
   set Data(Extr)        [ogrlayer configure $Object -extrude]
   set Data(ExtrFactor)  [ogrlayer configure $Object -extrudefactor]
   set Data(ColorMap)    [ogrlayer configure $Object -colormap]
   set Data(Color)       [ogrlayer configure $Object -outline]
   set Data(Highlight)   [ogrlayer configure $Object -activeoutline]
   set Data(Width)       [ogrlayer configure $Object -width]
   if { $Data(Width)<0 } {
      set Data(Burn) -1
      set Data(Width) [expr -$Data(Width)]
   } else {
      set Data(Burn) 1
   }
   set Data(Tran)        [ogrlayer configure $Object -transparency]
   set Data(Proj)        [ogrlayer define $Object -projection]
   set Data(Mask)        [ogrlayer define $Object -mask]
   set Data(Fields)      [ogrlayer define $Object -field]

   set value             [ogrlayer configure $Object -value]
   set Data(Order)       [lindex $value 0]
   set Data(Mantisse)    [lindex $value 1]

    set fill              [ogrlayer configure $Object -fill]
#   set Data(Stipple)     [ogrlayer configure $Object -stipple]

   if { $fill!="" } {
      set Data(FillSel) 1
      set Data(Fill)    $fill
   } else {
      set Data(FillSel) 0
   }

   set Data(Intervals)   [ogrlayer configure $Object -intervals]
   set Data(Min)         [ogrlayer configure $Object -min]
   set Data(Max)         [ogrlayer configure $Object -max]

   if { $Data(Min)!=$Data(Max) } {
      set Data(Intervals) ""
      if { $Data(Min)!="" } {
         append Data(Intervals) "\[$Data(Min)"
      }
      if { $Data(Max)!="" } {
         append Data(Intervals) " $Data(Max)\]"
      }
   }

   if { [llength [set interlabels [ogrlayer configure $Object -interlabels]]] } {
      set inters $Data(Intervals)
      set Data(Intervals) ""
      foreach label $interlabels inter $inters {
         append Data(Intervals) "$inter ($label) "
      }
   }
}

#-------------------------------------------------------------------------------
# Nom      : <Mapper::OGR::ParamsSet>
# Creation : Juillet 2004 - J.P. Gauthier - CMC/CMOE
#
# But      : Parametrer un objet de donnees vectorielle OGR
#
# Parametres :
#   <Object> : Donnee geographique a parametrer
#
# Retour    :
#
# Remarque :
#
#-------------------------------------------------------------------------------

proc Mapper::OGR::ParamsSet { Object } {
   variable Data

   if { $Mapper::Data(Init) } {
      return
   }
   Mapper::Cursor watch

   #----- Verifier pour un range plutot que des niveaux

   set inter $Data(Intervals)
   set label {}
   set min   ""
   set max   ""

   if { [set from [string first "\[" $Data(Intervals)]]!=-1 } {
      set min [lindex [string range $Data(Intervals) [incr from] end] 0]
      set inter {}
   }

   if { [set to [string first "\]" $Data(Intervals)]]!=-1 } {
      set max [lindex [string range $Data(Intervals) 0 [incr to -1]] end]
      set inter {}
   }

   if { [string first "(" $Data(Intervals)]!=-1 } {
      set inter {}
      foreach { val } [split $Data(Intervals) )] {
         if { [llength [set val [split $val (]]]>1 } {
            lappend inter [lindex $val 0]
            lappend label [lindex $val 1]
         }
      }
   }

#   ogrlayer configure $Object -stipple $Data(Stipple)
#   ogrlayer configure $Object -font OGRFONT

   ogrlayer configure $Object -dash $Data(Dash) -colormap $Data(ColorMap) -outline $Data(Color) -activeoutline $Data(Highlight) \
      -icon $Data(Icon) -size $Data(Size) -sizevar $Data(SizeVar) -mapvar $Data(MapVar) -width [expr $Data(Width)*$Data(Burn)] -transparency $Data(Tran) \
      -min $min -max $max -intervals $inter -interlabels $label -value $Data(Order) $Data(Mantisse) -topography $Data(Topo) -topographyfactor $Data(TopoFactor) \
      -extrude $Data(Extr) -extrudefactor $Data(ExtrFactor) -labelvar $Data(LabelVar)

   if { $Data(FillSel) } {
      ogrlayer configure $Object -fill $Data(Fill)
   } else {
      ogrlayer configure $Object -fill ""
   }

   ogrlayer define $Object -mask $Data(Mask)

   if { [winfo exists .mapper] } {
      set Data(Proj) [string trim [$Data(Frame1).proj.val get 0.0 end] "\n"]
      ogrlayer define $Object -projection $Data(Proj)

      Mapper::OGR::SelectApply $Object
   }

   Page::Update     $Page::Data(Frame)
   ColorBar::Update $Page::Data(Frame)
   Mapper::Progress $Object
   Mapper::Cursor left_ptr
}

#-------------------------------------------------------------------------------
# Nom      : <Mapper::OGR::SelectApply>
# Creation : Juillet 2004 - J.P. Gauthier - CMC/CMOE
#
# But      : Appliquer la selection des features de donnees OGR
#
# Parametres :
#   <Object> : Donnee geographique a parametrer
#
# Retour    :
#
# Remarque :
#
#-------------------------------------------------------------------------------

proc Mapper::OGR::SelectApply { Object } {
   variable Select

   set Mapper::Data(Coo) ""
   set select {}
   foreach field [ogrlayer define $Object -field] {
      if { $Select(Op$Object$field)!="" && $Select(Val$Object$field)!="" } {
         lappend select [list $field $Select(Op$Object$field) $Select(Val$Object$field)]
      }
   }

   if { ![info exists Select(String$Object)] || $Select(String$Object)!=$select } {
      set Select(String$Object) $select
      ogrlayer define $Object -featureselect $select
      ogrlayer define $Object -featurehighlight {}
      Mapper::OGR::Table $Object True
   }
}

#-------------------------------------------------------------------------------
# Nom      : <Mapper::OGR::SelectClear>
# Creation : Juillet 2004 - J.P. Gauthier - CMC/CMOE
#
# But      : Anuler la selection des features de donnees OGR
#
# Parametres :
#   <Object> : Donnee geographique a parametrer
#
# Retour    :
#
# Remarque :
#
#-------------------------------------------------------------------------------

proc Mapper::OGR::SelectClear { Object } {
   variable Select

   set Mapper::Data(Coo) ""
   
   if { [ogrlayer is $Object] } {
      foreach field [ogrlayer define $Object -field] {
         set Select(Op$Object$field) ""
         set Select(Val$Object$field) ""
      }
      ogrlayer define $Object -featureselect {}
      ogrlayer define $Object -featurehighlight {}

      Mapper::OGR::Table $Object True
   }
   Page::Update $Page::Data(Frame)
}

#-------------------------------------------------------------------------------
# Nom      : <Mapper::OGR::FeatureGoTo>
# Creation : Fevrier 2014 - J.P. Gauthier - CMC/CMOE
#
# But      : Zoom or move to deature location
#
# Parametres :
#
# Retour    :
#
# Remarque :
#
#-------------------------------------------------------------------------------

proc Mapper::OGR::FeatureGoTo { Index } { 
   variable Data
 
   switch $Data(SelMode) {
      "Locate"  { Mapper::Locate $Index }
      "Zoom"    { Mapper::Zoom False $Index }
      default { Page::Update $Page::Data(Frame) }
   }
}

#-------------------------------------------------------------------------------
# Nom      : <Mapper::OGR::Feature>
# Creation : Juillet 2004 - J.P. Gauthier - CMC/CMOE
#
# But      : Recuperer les donnees d'une feature specifique
#
# Parametres :
#   <Object> : Donnee geographique a parametrer
#   <Index>  : Index de l'item
#   <Locate> : Centre sur l'item (Default False)
#
# Retour    :
#
# Remarque :
#
#-------------------------------------------------------------------------------

proc Mapper::OGR::Feature { Object Index { Locate False } } {
   global   GDefs
   variable Data

   $Data(Frame3).fields.table configure -rows 0 -cols 0
   $Data(Frame3).fields.table set 0,0 "" 
   $Data(Frame3).geom.text delete 0.0 end

   if { $Index=="" } {
      return
   }

   if { [winfo exists $Data(Frame5).pan.meta.sub.table] && $Data(TableSel)!=$Index } {
      $Data(Frame5).pan.meta.sub.table yview $Index
   }

   if { ![catch { set infos [ogrlayer define $Object -feature $Index] }] } {

      if { [llength [ogrlayer define $Object -field]] } {
         set i 0
         $Data(Frame3).fields.table configure -rows [llength $infos] -cols 2

         foreach info $infos {
            $Data(Frame3).fields.table set $i,0 [lindex $info 0] 
            $Data(Frame3).fields.table set $i,1 [lindex $info 1]        
            incr i
         }
      }

      if { $Data(GeomGet) } {
         switch $Data(GeomOut) {
            TEXT { Mapper::OGR::FeatureGeom [ogrlayer define $Object -geometry $Index True] 0 }
            WKT  { $Data(Frame3).geom.text insert end [ogrgeometry define [ogrlayer define $Object -geometry $Index True] -wkt] }
            KML  { $Data(Frame3).geom.text insert end [ogrgeometry define [ogrlayer define $Object -geometry $Index True] -kml] }
            GML  { $Data(Frame3).geom.text insert end [ogrgeometry define [ogrlayer define $Object -geometry $Index True] -gml] }
            JSON { $Data(Frame3).geom.text insert end [ogrgeometry define [ogrlayer define $Object -geometry $Index True] -json] }
         }
      }

      ogrlayer define $Object -featurehighlight $Index
      set coords [ogrlayer stats $Object -centroid $Index]

      if { [llength $coords] } {
         set coords [ogrlayer project $Object [lindex $coords 0] [lindex $coords 1]]
      }

      if { $Index!=$Data(Index) } {
         set Data(Index) $Index
      }
      set Data(TableSel) $Index

      if { $Data(Edit) } {
         Mapper::OGR::VertexStop $Page::Data(Frame) $Object
         Mapper::OGR::VertexInit $Page::Data(Frame) $Object $Index
      }
      
      if { $Locate && [llength $coords] } {
 
         Mapper::OGR::FeatureGoTo $Index
      } else {
         Page::Update $Page::Data(Frame)
      }
   }
}

#-------------------------------------------------------------------------------
# Nom      : <Mapper::OGR::FeatureField>
# Creation : Janvier 2014 - J.P. Gauthier - CMC/CMOE
#
# But      : Update a feature field
#
# Parametres :
#   <Object> : Layer object
#   <Index>  : Feature index
#   <Field>  : Field to change
#   <Value>  : Value
#
# Retour    :
#
# Remarque :
#
#-------------------------------------------------------------------------------

proc Mapper::OGR::FeatureField { Object Index Field Value } {
   variable Data
   
   ogrlayer define $Object -feature $Index [$Data(Frame3).fields.table get $Field,0] $Value
   
   return True
}

proc Mapper::OGR::FeatureFieldTable { Object Row Column Value } {
   variable Data
   
   ogrlayer define $Object -feature [$Data(Frame5).pan.meta.sub.table get $Row,0] [$Data(Frame5).pan.meta.sub.table get 0,$Column] $Value
   
   return True
}
#-------------------------------------------------------------------------------
# Nom      : <Mapper::OGR::FeatureGeom>
# Creation : Juillet 2004 - J.P. Gauthier - CMC/CMOE
#
# But      : Extraire la geometrie d'une featrue
#
# Parametres :
#   <Object> : Donnee geographique (feature)
#   <Depth>  : Profondeur de la geometrie courante (Recursion)
#
# Retour    :
#
# Remarque :
#
#-------------------------------------------------------------------------------

proc Mapper::OGR::FeatureGeom { Geom Depth } {
   variable Data

   if { $Geom!="" } {
      set w [expr $Depth*3]
      eval $Data(Frame3).geom.text insert end \[format \"%-${w}s%s\\n\" \"\" [ogrgeometry define $Geom -type]\]
      incr w 3

      incr Depth
      foreach geom [ogrgeometry define $Geom -geometry] {
         Mapper::OGR::FeatureGeom $geom $Depth
      }

      if { [ogrgeometry define $Geom -dimension]==2 } {
         foreach { cx cy } [ogrgeometry define $Geom -points] {
            eval $Data(Frame3).geom.text insert end \[format \"%-${w}s%.4f %.4f\\n\" \"\" $cx $cy\]
         }
      } else {
         foreach { cx cy cz } [ogrgeometry define $Geom -points] {
            eval $Data(Frame3).geom.text insert end \[format \"%-${w}s%.4f %.4f %.4f\\n\" \"\" $cx $cy $cz\]
         }
      }
   }
}

#-------------------------------------------------------------------------------
# Nom      : <Mapper::OGR::FeatureAdd>
# Creation : Janvier 2014 - J.P. Gauthier - CMC/CMOE
#
# But      : Ajouter une feature
#
# Parametres :
#   <Object> : Donnee geographique 
#
# Retour    :
#
# Remarque :
#
#-------------------------------------------------------------------------------

proc Mapper::OGR::FeatureAdd { Object } {
   variable Data
   
   set nb [ogrlayer define $Object -nb]
   ogrlayer define $Object -nb [expr $nb+1]

   $Data(Frame3).sel.val configure -state normal -to $nb
   $Data(Frame3).sel.mode configure -state normal
     
   Mapper::OGR::Feature $Object $nb
}

#-------------------------------------------------------------------------------
# Nom      : <Mapper::OGR::FeatureDel>
# Creation : Janvier 2014 - J.P. Gauthier - CMC/CMOE
#
# But      : Supprimet une feature
#
# Parametres :
#   <Object> : Donnee geographique 
#
# Retour    :
#
# Remarque :
#
#-------------------------------------------------------------------------------

proc Mapper::OGR::FeatureDel { Object Index } {
   variable Data

   ogrlayer define $Object -delfeature $Index
 
   set nb [ogrlayer define $Object -nb]
   $Data(Frame3).sel.val configure -to [expr $nb-1]
   
   Mapper::OGR::Feature $Object [expr $Index==0?0:$Index-1]
}

#-------------------------------------------------------------------------------
# Nom      : <Mapper::OGR::Table>
# Creation : Juillet 2010 - J.P. Gauthier - CMC/CMOE
#
# But      : Fill in the table with the layer's data
#
# Parametres :
#   <Object> : Layer object
#   <Update> : Layer object
#   <Scroll> : Index to scroll to
#
# Retour    :
#
# Remarque :
#
#-------------------------------------------------------------------------------

proc Mapper::OGR::Table { Object Update { Index -1 } } {
   global GDefs
   variable Data
   variable Table

   #----- Some bindings make for concurrent update
   if { $Data(TableUpdate) || ![winfo exists $Data(Frame5).pan.meta.sub.table] } {
      return
   }

   set Data(TableUpdate) 1
   
   $Data(Frame5).pan.meta.sub.table configure -cursor watch
   update idletasks

   #----- Resize stats column to fit with tabel column
   foreach col [$Data(Frame5).pan.meta.sub.table width] {
      eval $Data(Frame5).pan.stat.table width $col
      eval $Data(Frame5).search.table width $col
   }
   
   #----- Check if sort field is valid for the layer
   if { [catch { ogrlayer define $Object -feature 0 $Data(TableSort) }] } {
      set Data(TableSort) ""
   }

   #----- If no column selected, deactivate delete button
   if { $Data(TableSort)=="" } {
      $Data(Frame5).func.del configure -state disabled
   } else {
      $Data(Frame5).func.del configure -state normal
   }
   
   #----- Get current view limits
   set rs [lindex [$Data(Frame5).pan.meta.sub.table configure -rows] 4]
   set cs [lindex [$Data(Frame5).pan.meta.sub.table configure -cols] 4]
   set ys [$Data(Frame5).pan.meta.sub.table yview]
   set y0 [expr int(floor([lindex $ys 0]*$rs))]
   set y1 [expr int(ceil([lindex $ys 1]*$rs))-1]

   #----- Define table size
   set col [expr [llength [ogrlayer define $Object -field]]+1]
   set row [expr [llength [ogrlayer define $Object -featureselect]]+1]

   if { $col==1 || $row==1 } {
      #----- If no fields are defined, an infinite call loop occurs unless we forget the table variable
      $Data(Frame5).pan.meta.sub.table configure -variable ""
   } else {
      $Data(Frame5).pan.meta.sub.table configure -variable Mapper::OGR::Table
   }

   $Data(Frame5).pan.meta.sub.table configure -rows $row -cols $col
   
    #----- Scroll to specific index
   if { $Index>-1 } {
      for { set r 1 } { $r<[expr $row-1] } { incr r } {
         catch {
         if { $Mapper::OGR::Table($r,0)==$Index } {
            $Data(Frame5).pan.meta.sub.table yview [expr $r-1]
            break
         }
         }
      }
   }

   #----- Have to cleanup the widgets since the table does not clean them on resize
   for { set r [expr $y1+1] } { $r<[ogrlayer define $Object -nb] } { incr r } {
      destroy $Data(Frame5).pan.meta.sub.table.r$r
   }

   #----- Get data for the view limits
   ogrlayer stats $Object -table Mapper::OGR::Table $y0 $y1
   
   #----- Create header buttons (Sorter)
   for { set c 0 } { $c<$col } { incr c } {
      if {![winfo exists $Data(Frame5).pan.meta.sub.table.c$c] } {
         radiobutton $Data(Frame5).pan.meta.sub.table.c$c -variable Mapper::OGR::Data(TableSort) -indicatoron False -bd 1 \
            -command "Mapper::OGR::TableColumnSelect \$Mapper::Data(Object) $row $c"
      }
      $Data(Frame5).pan.meta.sub.table window configure 0,$c -window $Data(Frame5).pan.meta.sub.table.c$c -sticky nsew
      if { $c==0 } {
         $Data(Frame5).pan.meta.sub.table.c$c configure -text "" -value ""
      } else {
         $Data(Frame5).pan.meta.sub.table.c$c configure -text $Mapper::OGR::Table(0,$c) -value $Mapper::OGR::Table(0,$c)
      }
   }
   
   for { set c $c } { $c<$cs } { incr c } {
      destroy radiobutton $Data(Frame5).pan.meta.sub.table.c$c
   }

   for { set r [expr $y0<1?1:$y0] } { $r<=$y1 } { incr r } {
      if { ![winfo exists $Data(Frame5).pan.meta.sub.table.r$r] } {
         radiobutton $Data(Frame5).pan.meta.sub.table.r$r -variable Mapper::OGR::Data(TableSel) -indicatoron False -bd 1 \
            -command "Mapper::OGR::TableRowSelect \$Mapper::Data(Object) $r $col"
      }
      $Data(Frame5).pan.meta.sub.table window configure $r,0 -window $Data(Frame5).pan.meta.sub.table.r$r -sticky nsew
      catch { $Data(Frame5).pan.meta.sub.table.r$r configure -text $Mapper::OGR::Table($r,0) -value $Mapper::OGR::Table($r,0) }
   }

   $Data(Frame5).pan.meta.sub.table configure -cursor left_ptr
 
   if { $Update } {
      Mapper::OGR::TableStat $Object
      Mapper::OGR::TableSelect  $Object
   }
   
   set Data(TableUpdate) 0
}

#-------------------------------------------------------------------------------
# Nom      : <Mapper::OGR::TTableSelect>
# Creation : Juillet 2010 - J.P. Gauthier - CMC/CMOE
#
# But      : Fill in the selection table
#
# Parametres :
#   <Object> : Layer object
#
# Retour    :
#
# Remarque :
#
#-------------------------------------------------------------------------------

proc Mapper::OGR::TableSelect { Object } {
   global GDefs
   variable Data

   set ns     [lindex [$Data(Frame5).search.table configure -cols] 4]
   set fields [ogrlayer define $Object -field]
   
   #----- Cleanup widgets
   for { set n 1 } { $n<$ns } { incr n } {
      destroy $Data(Frame5).search.table.fr$n
      $Data(Frame5).search.table window configure 0,$n -window ""
   }   
   $Data(Frame5).search.table configure -rows 1 -cols [expr [llength $fields]+1]

   set n 1
  
   #----- Build widgets
   foreach field $fields {
      
      frame $Data(Frame5).search.table.fr$n -relief raised -bd 1
         entry $Data(Frame5).search.table.fr$n.sel -textvariable Mapper::OGR::Select(Val$Object$field) -bg $GDefs(ColorLight) -width 0 -relief sunken -bd 1
         menubutton $Data(Frame5).search.table.fr$n.op -textvariable Mapper::OGR::Select(Op$Object$field) -menu $Data(Frame5).search.table.fr$n.op.menu \
            -relief sunken -width 2 -bd 1
         pack $Data(Frame5).search.table.fr$n.op -side left -fill y
         pack $Data(Frame5).search.table.fr$n.sel -side left -fill both -expand True

         Bubble::Create $Data(Frame5).search.table.fr$n.op $Mapper::Bubble(Op)

      menu $Data(Frame5).search.table.fr$n.op.menu
         $Data(Frame5).search.table.fr$n.op.menu add command -label ""   -command "set Mapper::OGR::Select(Op$Object$field) \"\""
         $Data(Frame5).search.table.fr$n.op.menu add command -label "~=" -command "set Mapper::OGR::Select(Op$Object$field) ~="
         $Data(Frame5).search.table.fr$n.op.menu add command -label "==" -command "set Mapper::OGR::Select(Op$Object$field) =="
         $Data(Frame5).search.table.fr$n.op.menu add command -label "!=" -command "set Mapper::OGR::Select(Op$Object$field) !="
         $Data(Frame5).search.table.fr$n.op.menu add command -label "<"  -command "set Mapper::OGR::Select(Op$Object$field) <"
         $Data(Frame5).search.table.fr$n.op.menu add command -label "<=" -command "set Mapper::OGR::Select(Op$Object$field) <="
         $Data(Frame5).search.table.fr$n.op.menu add command -label ">"  -command "set Mapper::OGR::Select(Op$Object$field) >"
         $Data(Frame5).search.table.fr$n.op.menu add command -label ">=" -command "set Mapper::OGR::Select(Op$Object$field) >="
         $Data(Frame5).search.table.fr$n.op.menu add command -label "<>" -command "set Mapper::OGR::Select(Op$Object$field) <>"
         $Data(Frame5).search.table.fr$n.op.menu add command -label "\[\]" -command "set Mapper::OGR::Select(Op$Object$field) \\\[\\\]"

         $Data(Frame5).search.table window configure 0,$n -window $Data(Frame5).search.table.fr$n -sticky nsew
         
      incr n
   }
}

#-------------------------------------------------------------------------------
# Nom      : <Mapper::OGR::TableStat>
# Creation : Juillet 2010 - J.P. Gauthier - CMC/CMOE
#
# But      : Fill in the table statistics
#
# Parametres :
#   <Object> : Layer object
#
# Retour    :
#
# Remarque :
#
#-------------------------------------------------------------------------------

proc Mapper::OGR::TableStat { Object } {
   variable Data
   
   #----- Define table size
   set fields [ogrlayer define $Object -field]
   $Data(Frame5).pan.stat.table configure -rows 8 -cols [expr [llength $fields]+1]
   
   set c 1
   foreach field $fields {
      if { [catch { $Data(Frame5).pan.stat.table set 0,$c [vexpr NIL ssumx($Object.$field)] }] } {
          $Data(Frame5).pan.stat.table set 0,$c ""
          $Data(Frame5).pan.stat.table set 1,$c ""
          $Data(Frame5).pan.stat.table set 2,$c ""
          $Data(Frame5).pan.stat.table set 3,$c ""
          $Data(Frame5).pan.stat.table set 4,$c ""
          $Data(Frame5).pan.stat.table set 5,$c ""
          $Data(Frame5).pan.stat.table set 6,$c ""
          $Data(Frame5).pan.stat.table set 7,$c ""
      } else {
         catch { $Data(Frame5).pan.stat.table set 1,$c [vexpr NIL sminx()] }          
         catch { $Data(Frame5).pan.stat.table set 2,$c [vexpr NIL smaxx()] }    
         catch { $Data(Frame5).pan.stat.table set 3,$c [vexpr NIL smaxx()-sminx()] }
         catch { $Data(Frame5).pan.stat.table set 4,$c [vexpr NIL savgx()] }
         catch { $Data(Frame5).pan.stat.table set 5,$c [vexpr NIL ssdev()] }         
         catch { $Data(Frame5).pan.stat.table set 6,$c [vexpr NIL smed($Object.$field)] }
         catch { $Data(Frame5).pan.stat.table set 7,$c [vexpr NIL suniq($Object.$field)] }
      }
      incr c
   }
   
   $Data(Frame5).pan.stat.table set 0,0 Sum       
   $Data(Frame5).pan.stat.table set 1,0 Min       
   $Data(Frame5).pan.stat.table set 2,0 Max 
   $Data(Frame5).pan.stat.table set 3,0 Range 
   $Data(Frame5).pan.stat.table set 4,0 Mean       
   $Data(Frame5).pan.stat.table set 5,0 StdDev       
   $Data(Frame5).pan.stat.table set 6,0 Median
   $Data(Frame5).pan.stat.table set 7,0 Unique
}

#-------------------------------------------------------------------------------
# Nom      : <Mapper::OGR::TableYScroll>
# Creation : Juillet 2010 - J.P. Gauthier - CMC/CMOE
#
# But      : Scroll Y on feature table
#
# Parametres :
#   <Object> : Layer object
#
# Retour    :
#
# Remarque :
#
#-------------------------------------------------------------------------------

proc Mapper::OGR::TableYScroll { Object args } {
   variable Data

   eval $Data(Frame5).pan.meta.sub.table yview $args
   Mapper::OGR::Table $Object False
}

proc Mapper::OGR::TableXScroll { args } {
   variable Data
   
   eval $Data(Frame5).pan.meta.sub.table xview $args
   eval $Data(Frame5).pan.stat.table xview $args
   eval $Data(Frame5).search.table xview $args
}

#-------------------------------------------------------------------------------
# Nom      : <Mapper::OGR::TableColumnSelect>
# Creation : Juillet 2010 - J.P. Gauthier - CMC/CMOE
#
# But      : User selection of osorting mode in the table
#
# Parametres :
#   <Object> : Layer object
#
# Retour    :
#
# Remarque :
#
#-------------------------------------------------------------------------------

proc Mapper::OGR::TableColumnSelect { Object Row Column } {
   variable Data

   set reverse False

   for { set c 1 } { $c<=[llength [ogrlayer define $Object -field]] } { incr c } { 
      if { $c!=$Column } {
         $Data(Frame5).pan.meta.sub.table.c$c configure -image "" 
      }
   }
   
   if { $Data(TableSort)!="" } {
      switch [lindex [$Data(Frame5).pan.meta.sub.table.c$Column configure -image] end] {
         DOWN    { set img UP; set reverse True }
         default { set img DOWN }
      }
      $Data(Frame5).pan.meta.sub.table.c$Column configure -image $img -compound right
   }
  
   ogrlayer stats $Object -sort $Data(TableSort) $reverse
   Mapper::OGR::Table $Object False
}

proc Mapper::OGR::TableColumnAdd { Object Field Type Len } {

   ogrlayer define $Object -field $Field $Type $Len

   destroy .mappernew
   Mapper::OGR::Table $Object True
}

proc Mapper::OGR::TableColumnCalc { Object Field Expr } {

   #----- Check if field exist, otherwise create it
   set Field [string toupper $Field]
   if { [lsearch -exact [ogrlayer define $Object -field] $Field]==-1 } {
      ogrlayer define $Object -field $Field Real 32   
   }
   
   #----- Parcel expresion to add layer
   set n 0
   for { set c 0 } { $c<[string length $Expr] } { incr c } {
      set char [string index $Expr $c]
      if { [string is upper $char] } {
         if { !$n } {
            append calc $Object.
            set n 1
         } 
      } else {
         set n 0
      }   
      append calc $char
   }
   vexpr $Object.$Field $calc

   destroy .mappernew 
   Mapper::OGR::Table $Object True
}

proc Mapper::OGR::TableColumnDel { Object Field } {
   variable Data
   
   if { $Field!="" } {
      if { [Dialog::Default .mapper 300 WARNING $Mapper::Msg(FieldDel) "\n\n\t$Field" 0 $Mapper::Lbl(No) $Mapper::Lbl(Yes)] } {
         ogrlayer define $Object -delfield $Field
      }
   }
   set Data(TableSort) ""

   Mapper::OGR::Table $Object True
}

#-------------------------------------------------------------------------------
# Nom      : <Mapper::OGR::TableRowSelect>
# Creation : Juillet 2010 - J.P. Gauthier - CMC/CMOE
#
# But      : User selection of one feature in the table
#
# Parametres :
#   <Object> : Layer object
#
# Retour    :
#
# Remarque :
#
#-------------------------------------------------------------------------------

proc Mapper::OGR::TableRowSelect { Object Row Col } {
   variable Data

   $Data(Frame5).pan.meta.sub.table selection clear all;
   $Data(Frame5).pan.meta.sub.table selection set $Row,1 $Row,$Col;
   Mapper::OGR::Feature $Object $Mapper::OGR::Table($Row,0) True
}

proc Mapper::OGR::Apply { Object } {

   set map [ogrlayer configure $Object -colormap]
   colormap image $map OGRMAPImg
   Mapper::OGR::ParamsSet $Object
}

proc Mapper::OGR::VertexInit { Frame Object Feature } {
   variable Data
   
   if { $Data(Edit) } {
      if { $Page::Data(ToolMode)=="Mapper" } {
         SPI::ToolMode SPI Data True
      }
      Mapper::OGR::VertexStart $Frame $Object $Feature 
      Mapper::OGR::VertexShow  $Frame $Viewport::Data(VP) $Data(Geom)
      Mapper::OGR::VertexLine  $Frame $Viewport::Data(VP) $Data(GeomIdx)
      Mapper::OGR::VertexType  $Data(Geom) 
   } else {
      Mapper::OGR::VertexStop  $Frame $Object
      Mapper::OGR::Feature $Object $Data(Index)
   }
}

proc Mapper::OGR::VertexStart { Frame Object Feature } {
   variable Data
   
   #----- Get geometry object and reproject to latlon for vertices manipulation
   set Data(Edit)    True
   set Data(Feature) $Feature
   set Data(Geom)    [ogrlayer define $Object -geometry $Feature False]
   
   #----- If no geometry is defined
   if { $Data(Geom)=="" } {
   
      #----- Create geometry with same type as other features in the file
      ogrgeometry free OGRTMP     
      ogrgeometry create OGRTMP [ogrlayer define $Object -type]
     
      set Data(Geom) OGRTMP
      
      Mapper::OGR::VertexAddItem OGRTMP True
   }
      
   ogrgeometry stats $Data(Geom) -transform LLREF

   Mapper::OGR::VertexIndex $Data(Geom)
}

proc Mapper::OGR::VertexStop { Frame Object } {
   variable Data
   
   $Frame.page.canvas delete MAPPERVERTEX MAPPERLINE
   $Frame.page.canvas bind PAGE$Viewport::Data(VP) <Shift-ButtonRelease-1>   ""
   $Frame.page.canvas bind PAGE$Viewport::Data(VP) <Control-ButtonRelease-1> ""

   Mapper::OGR::VertexUpdate $Frame $Object
   
   set Data(Geom)    ""
   set Data(GeomIdx) ""    
}

proc Mapper::OGR::VertexUpdate { Frame Object { Clean True } } {
   variable Data

   if { [ogrgeometry is $Data(Geom)] } {
      ogrgeometry stats $Data(Geom) -transform [ogrlayer define $Object -georef]
      ogrlayer define $Object -geometry $Data(Feature) False $Data(Geom)
      ogrlayer clean $Object $Data(Feature)
      
      if { $Clean } {
         ogrgeometry free $Data(Geom)
      }
      
      Page::Update $Frame
   }
}

proc Mapper::OGR::VertexIndex { Geom } {
   variable Data

   switch [ogrgeometry define $Geom -type] {
      "Point"                  -
      "3D Point"               -        
      "Line String"            -
      "3D Line String"         -
      "Linear Ring"            { set Data(GeomIdx) {} }
      
      "Polygon"                -
      "3D Polygon"             -
      
      "Multi Point"            -
      "3D Multi Point"         -
      
      "Multi Line String"      -
      "3D Multi Line String"   { set Data(GeomIdx) { 0 } }
      
      "Multi Polygon"          -
      "3D Multi Polygon"       { set Data(GeomIdx) { 0 0 } }
   
      "Geometry Collection"    -
      "3D Geometry Collection" { }     
   }
}

proc Mapper::OGR::VertexType { Geom } {
   variable Data

   $Data(Frame3).sel.mode.opt.menu entryconfigure 0 -state disabled 
   $Data(Frame3).sel.mode.opt.menu entryconfigure 1 -state disabled 
   $Data(Frame3).sel.mode.opt.menu entryconfigure 3 -state disabled 
   $Data(Frame3).sel.mode.opt.menu entryconfigure 4 -state disabled 
   
   switch [ogrgeometry define $Data(Geom) -sub [lindex $Data(GeomIdx) end-1] -type] {
      "Point"                  -
      "3D Point"               -        
      "Line String"            -
      "3D Line String"         -
      "Linear Ring"            { }
      
      "Polygon"                -
      "3D Polygon"             -
      
      "Multi Point"            -
      "3D Multi Point"         -
      
      "Multi Line String"      -
      "3D Multi Line String"   { $Data(Frame3).sel.mode.opt.menu entryconfigure 0 -state normal 
                                 $Data(Frame3).sel.mode.opt.menu entryconfigure 3 -state normal  }
      
      "Multi Polygon"          -
      "3D Multi Polygon"       { $Data(Frame3).sel.mode.opt.menu entryconfigure 1 -state normal
                                 $Data(Frame3).sel.mode.opt.menu entryconfigure 4 -state normal }
   
      "Geometry Collection"    -
      "3D Geometry Collection" { }     
   }
}

proc Mapper::OGR::VertexAddItem { Geom { New False } } {
   variable Data

   if { !$Data(Edit) } {
      set Data(Edit) True
      Mapper::OGR::VertexInit $Page::Data(Frame) $Mapper::Data(Object) $Mapper::OGR::Data(Index) 
      set Geom $Mapper::OGR::Data(Geom)
   } else {
      if { !$New } {
         Mapper::OGR::VertexUpdate $Page::Data(Frame) $Mapper::Data(Object) False
      }
   }
   
   ogrgeometry free OGRTMP1 OGRTMP2
   
   #----- Create sub geometry with right type
   switch [ogrgeometry define $Geom -type] {
      "Point"                  -
      "3D Point"               -        
      "Line String"            -
      "3D Line String"         -
      "Linear Ring"            { }
      
      "Polygon"                -
      "3D Polygon"             { ogrgeometry create OGRTMP1 "Linear Ring" ; ogrgeometry define $Geom -addgeometry False OGRTMP1 }
      
      "Multi Point"            { ogrgeometry create OGRTMP1 "Point" ; ogrgeometry define $Geom -addgeometry False OGRTMP1 }
      "3D Multi Point"         { ogrgeometry create OGRTMP1 "Point3D" ; ogrgeometry define $Geom -addgeometry False OGRTMP1 }
      
      "Multi Line String"      { ogrgeometry create OGRTMP1 "Line String" ; ogrgeometry define $Geom -addgeometry False OGRTMP1 }
      "3D Multi Line String"   { ogrgeometry create OGRTMP1 "3D Line String" ; ogrgeometry define $Geom -addgeometry False OGRTMP1 }
      
      "Multi Polygon"          { ogrgeometry create OGRTMP2 "Polygon" ; ogrgeometry create OGRTMP1 "Linear Ring" ; ogrgeometry define OGRTMP2 -addgeometry False OGRTMP1; ogrgeometry define $Geom -addgeometry False OGRTMP2 }
      "3D Multi Polygon"       { ogrgeometry create OGRTMP2 "3D Polygon" ; ogrgeometry create OGRTMP1 "Linear Ring" ; ogrgeometry define OGRTMP2 -addgeometry False OGRTMP1; ogrgeometry define $Geom -addgeometry False OGRTMP2 }
   
      "Geometry Collection"    -
      "3D Geometry Collection" { }     
   }
   
   if { $New } {
      Mapper::OGR::VertexIndex $Geom
   } else {
      switch [ogrgeometry define $Geom -type] {
         "Point"                  -
         "3D Point"               -        
         "Line String"            -
         "3D Line String"         -
         "Linear Ring"            { }
         
         "Polygon"                -
         "3D Polygon"             -
         
         "Multi Point"            -
         "3D Multi Point"         -
         
         "Multi Line String"      -
         "3D Multi Line String"   { lset Data(GeomIdx) end [expr [ogrgeometry define $Geom -nbsub]-1] }
         
         "Multi Polygon"          -
         "3D Multi Polygon"       { lset Data(GeomIdx) end-1 [expr [ogrgeometry define $Geom -nbsub]-1]; lset Data(GeomIdx) end 0 }
      
         "Geometry Collection"    -
         "3D Geometry Collection" { }     
      }
   } 

   Mapper::OGR::VertexShow $Page::Data(Frame) $Viewport::Data(VP) $Data(Geom)
   Mapper::OGR::VertexLine $Page::Data(Frame) $Viewport::Data(VP) $Data(GeomIdx)
   Mapper::OGR::VertexType $Data(Geom) 
}

proc Mapper::OGR::VertexDelItem { Geom } {
   variable Data

   ogrgeometry define $Geom -sub [lrange $Data(GeomIdx) 0 end-1] -delgeometry [lindex $Data(GeomIdx) end]
   
   Mapper::OGR::Feature $Mapper::Data(Object) $Data(Index)
}

proc Mapper::OGR::VertexShow { Frame VP Geom { Id {} } } {
   global GDefs
   variable Data

   set no 0
   set cs {}
 
   #----- If this is the first call (no recursion yet)
   if { ![llength $Id] } {
      $Frame.page.canvas delete MAPPERVERTEX
      $Frame.page.canvas create line -999 -999 -999 -999  -width 3 -fill yellow -tags "MAPPERLINE" 
      
      $Frame.page.canvas bind PAGE$VP <Shift-ButtonRelease-1>   "Mapper::OGR::VertexAdd $Frame $VP \[$Frame.page.canvas canvasx %x\] \[$Frame.page.canvas canvasy %y\]"
      $Frame.page.canvas bind PAGE$VP <Control-ButtonRelease-1> "Mapper::OGR::VertexInsert $Frame $VP \[$Frame.page.canvas canvasx %x\] \[$Frame.page.canvas canvasy %y\]"

      $Frame.page.canvas bind MAPPERVERTEX <Enter>      "$Frame.page.canvas config -cursor hand1"
      $Frame.page.canvas bind MAPPERVERTEX <Leave>      "$Frame.page.canvas config -cursor left_ptr"
   }
   
   #----- Loop on the geometry points and create vertices
   foreach { lon lat } [ogrgeometry define $Geom -points] {
   
      if { [set xy [$VP -project $lat $lon 0.0]]!="" && [lindex $xy 2]>=0 } {
         set tid $Id
         lappend tid $no
         set tag [join $tid .]
         $Frame.page.canvas create bitmap [lindex $xy 0] [lindex $xy 1] -bitmap @$GDefs(Dir)/share/bitmap/cvscale.xbm -foreground yellow -tags "MAPPERVERTEX MAPPERVERTEX$tag" 
         
         $Frame.page.canvas bind MAPPERVERTEX$tag <ButtonPress-1>   "Mapper::OGR::VertexLine $Frame $VP $Id; Mapper::OGR::VertexType $Geom"
         $Frame.page.canvas bind MAPPERVERTEX$tag <ButtonPress-2>   "Mapper::OGR::VertexLine $Frame $VP $Id; Mapper::OGR::VertexType $Geom"
         $Frame.page.canvas bind MAPPERVERTEX$tag <B1-Motion>       "Mapper::OGR::VertexMove $Frame $VP $tag \[$Frame.page.canvas canvasx %x\] \[$Frame.page.canvas canvasy %y\]"
         $Frame.page.canvas bind MAPPERVERTEX$tag <ButtonRelease-2> "Mapper::OGR::VertexDel  $Frame $VP $tag"
      }
      incr no
   }
 
   #----- Loop on sub geometry
   set no -1
   foreach geom [ogrgeometry define $Geom -geometry] {  
      Mapper::OGR::VertexShow $Frame $VP $geom [concat $Id [incr no]]
   }
}

proc Mapper::OGR::VertexLine { Frame VP { Id "" } } {
   variable Data

   if { $Id!="" } {
      set Data(GeomIdx) $Id
   }
  
   set coords [ogrgeometry define $Data(Geom) -sub $Data(GeomIdx) -points]
   if { [llength $coords]>=4 } {
      foreach { lon lat } $coords {
         set xy [$VP -project $lat $lon 0.0 True]  
         lappend pix [lindex $xy 0] [lindex $xy 1]
      }
      eval $Frame.page.canvas coords MAPPERLINE $pix
   } else {
      eval $Frame.page.canvas coords MAPPERLINE -999 -999 -999 -999   
   }
}

proc Mapper::OGR::VertexSnap { Frame VP Object Id X Y } {
   variable Data
   
   if { $Data(Snap) } {
   
      #----- Rectangle pick
      set co0 [$VP -unproject [expr $X-$Data(Snap)] [expr $Y-$Data(Snap)]]
      set co1 [$VP -unproject [expr $X+$Data(Snap)] [expr $Y-$Data(Snap)]]
      set co2 [$VP -unproject [expr $X+$Data(Snap)] [expr $Y+$Data(Snap)]]
      set co3 [$VP -unproject [expr $X-$Data(Snap)] [expr $Y+$Data(Snap)]]
      set coords [list [lindex $co0 0] [lindex $co0 1] [lindex $co1 0] [lindex $co1 1] [lindex $co2 0] [lindex $co2 1] [lindex $co3 0] [lindex $co3 1] [lindex $co0 0] [lindex $co0 1]]
      
      if { [llength [set lxy [ogrlayer pick $Object $coords VERTEX]]] } {
         set ll [ogrlayer stats $Object -project [lindex $lxy 0] [lindex $lxy 1]]
         
         if { $Id!="" } {
            set xy [$VP -project [lindex $ll 0] [lindex $ll 1] 0] 
            $Frame.page.canvas coords MAPPERVERTEX$Id [lindex $xy 0] [lindex $xy 1]         
         }
      } else {
         set ll [$VP -unproject $X $Y]       
      }
   } else {
      set ll [$VP -unproject $X $Y] 
   }
   return $ll 
}

proc Mapper::OGR::VertexAdd { Frame VP X Y } {
   variable Data

   set ll [Mapper::OGR::VertexSnap $Frame $VP $Mapper::Data(Object) "" $X $Y]
      
   ogrgeometry define $Data(Geom) -sub $Data(GeomIdx) -addpoint [lindex $ll 1] [lindex $ll 0]
   
   Mapper::OGR::VertexShow $Frame $VP $Data(Geom)
   Mapper::OGR::VertexLine $Frame $VP
   Mapper::OGR::VertexType $Data(Geom) 
}

proc Mapper::OGR::VertexInsert { Frame VP X Y } {
   variable Data

   set ll [Mapper::OGR::VertexSnap $Frame $VP $Mapper::Data(Object) "" $X $Y]
   
   #----- Get distance vectors
   set segdist [ogrgeometry define $Data(Geom) -sub $Data(GeomIdx) -segmentdist [lindex $ll 1] [lindex $ll 0]]
   
   #----- Find closest point
   set i    0
   set vidx 0
   set min  1e32
      
   if { [llength $segdist] } {
      foreach seg $segdist {
         if { $seg<$min } {
            set min $seg
            set vidx $i
         }
         incr i
      }  
      incr vidx
   }
   
   ogrgeometry define $Data(Geom) -sub $Data(GeomIdx) -inspoint $vidx [lindex $ll 1] [lindex $ll 0]
   
   Mapper::OGR::VertexShow $Frame $VP $Data(Geom)
   Mapper::OGR::VertexLine $Frame $VP
}

proc Mapper::OGR::VertexDel { Frame VP Id } {
   variable Data

   $Frame.page.canvas delete MAPPERVERTEX$Id

   set ids      [split $Id .]
   set vidx     [lindex $ids end]
   set Data(GeomIdx) [lrange $ids 0 end-1]
   
   ogrgeometry define $Data(Geom) -sub $Data(GeomIdx) -delpoint $vidx
   
   Mapper::OGR::VertexShow $Frame $VP $Data(Geom)
   Mapper::OGR::VertexLine $Frame $VP
}

proc Mapper::OGR::VertexMove { Frame VP Id X Y } {
   variable Data

   $Frame.page.canvas coords MAPPERVERTEX$Id $X $Y

   #----- Check if close enough to another point for snapping / closing ring
   foreach item [$Frame.page.canvas find overlapping [expr $X-$Data(Snap)] [expr $Y-$Data(Snap)] [expr $X+$Data(Snap)] [expr $Y+$Data(Snap)]] {
      if { [lsearch [$Frame.page.canvas gettags $item] MAPPERVERTEX]!=-1 } {
         set coords [$Frame.page.canvas coords $item]
         $Frame.page.canvas coords MAPPERVERTEX$Id $coords 
         set X [lindex $coords 0]
         set Y [lindex $coords 1]
      }
   }
   
   set ids           [split $Id .]
   set vidx          [lindex $ids end]
   set Data(GeomIdx) [lrange $ids 0 end-1]
   
   #----- Get latlon coordinate and update geometry object
   set ll [Mapper::OGR::VertexSnap $Frame $VP $Mapper::Data(Object) $Id $X $Y]

   ogrgeometry define $Data(Geom) -sub $Data(GeomIdx) -setpoint $vidx [lindex $ll 1] [lindex $ll 0]
   
   Mapper::OGR::VertexLine $Frame $VP
}

#-------------------------------------------------------------------------------
# Nom      : <Mapper::OGR::Read>
# Creation : Juillet 2004 - J.P. Gauthier - CMC/CMOE
#
# But      : Lecture d'une donnee geographique OGR.
#
# Parametres :
#   <File>   : Fichiersa lire
#   <Index>  : Index into the layer
#   <SQL>    : Requete SQL
#
# Retour    :
#
# Remarque :
#
#-------------------------------------------------------------------------------

proc Mapper::OGR::Read { File { Index {} } { SQL "" } } {
   global GDefs
   variable Data

   set id OGR[incr Data(IdNo)]
  
   eval set bad [catch { set idxs [ogrfile open $id read $File] } msg]
   if { $bad } {
      Log::Pring ERROR $msg
      return ""
   }

   set Data(Job)   [lindex $Mapper::Msg(Read) $GDefs(Lang)]
   update idletasks;

   #----- If a layer index has been specified, use it
   if { [llength $Index] } {
      set idxs {}
      foreach idx $Index {
         lappend idxs [list $id [lindex $idx 0] [lindex $idx 1]]
      }
   }

   foreach idx $idxs {
      set obj [lindex $idx 2]

      if { ![ogrlayer is $obj] } {
         if { $SQL!="" } {
            ogrlayer sqlselect $objr $File $SQL
         } else {
            eval ogrlayer read \$obj $idx
         }

         if { [ogrlayer define $obj -nb]==0 } {
            Dialog::Error . $Mapper::Msg(NoFeature) $obj
            ogrlayer free $obj
            ogrfile close $File
            continue
         }
         Mapper::OGR::Config $Page::Data(Frame) $obj
         
         set Mapper::Data(Id$obj) $id
      }
   }
   set Mapper::Data(Job) ""
   Mapper::Progress $obj

   return $obj
}

#-------------------------------------------------------------------------------
# Nom      : <Mapper::OGR::Write>
# Creation : Fevrier 2014 - J.P. Gauthier - CMC/CMOE
#
# But      : Ecriture d'une donnee geographique OGR.
#
# Parametres :
#   <File>   : Fichiers a écrire
#   <Format> : Format d'enregistrement
#   <Object> : Couche à écrire
#
# Retour    :
#
# Remarque :
#
#-------------------------------------------------------------------------------

proc Mapper::OGR::Write { File Format Object } {

   if { $File!="" } {
      ogrfile open OGRFILETMP write $File [lrange $Format 0 end-1]
      ogrlayer write $Object OGRFILETMP
      ogrfile close OGRFILETMP
   }
}


