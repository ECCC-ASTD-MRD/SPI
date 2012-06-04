#============================================================================
# Environnement Canada
# Centre Meteorologique Canadien
# 2100 Trans-Canadienne
# Dorval, Quebec
#
# Projet     : Interface pour la gestion des experiences.
# Fichier    : <Export.tcl>
# Creation   : Novembre 2002 - J.P. Gauthier - CMC/CMOE
#
# Description: Interface d'exportation de donnees.
#
# Remarques  :
#
#============================================================================

namespace eval Export {
   variable Lbl
   variable Bubble
   variable Data
   variable Error

   set Data(Canvas) ""                    ;#Canvas Actif
   set Data(Frame)  ""                    ;#Frame actif
   set Data(VP)     ""                    ;#Viewport actif

   set Data(Lat0)  -90.0
   set Data(Lat1)   90.0
   set Data(Lon0)  -180.0
   set Data(Lon1)   180.0
   set Data(Res)    0.01
   set Data(Coo)    ""
   set Data(Path)   ""
   set Data(Type)   Vector
   set Data(Types)  { Vector Raster }
   set Data(Format) {ESRI Shape "ESRI Shapefile" {*.shp *.shx *.dbf}}

   set Lbl(Export)   { "Exporter" "Export" }
   set Lbl(File)     { "Fichier" "File" }
   set Lbl(Cancel)   { "Annuler" "Cancel" }
   set Lbl(Vector)   { "Vertoriel" "Vectorial" }
   set Lbl(Raster)   { "Matriciel" "Raster" }
   set Lbl(Host)     { "Hôte" "Hostname" }
   set Lbl(Port)     { "Port" "Port" }
   set Lbl(DBase)    { "Base de donnée" "Database" }
   set Lbl(User)     { "Usager" "User" }
   set Lbl(Password) { "Mot de passe" "Password" }
   set Lbl(Format)   { "Format" "Format" }
   set Lbl(Loc)      { "Localisation" "Localisation" }
   set Lbl(What)     { "Quoi" "What" }
   set Lbl(How)      { "Comment" "How" }
   set Lbl(Where)    { "Où" "Where" }
   set Lbl(Info)     { "Information" "Information" }

   set Bubble(Type)   { "Type d'exportation des données" "Export Type of data" }
   set Bubble(Format) { "Formats des données exportées" "Format of exported data" }
   set Bubble(Data)   { "Sélection des données a exporter" "Select data to export" }
   set Bubble(File)   { "Nom du fichier d'exportation" "Export filename" }
   set Bubble(DBase)  { "Paramêtres de connection a la base de donnée" "Database connexion parameters" }

   set Error(Legend)  { "Problème lors de la création de la légende (convert)" "Problem creating legend (convert)" }
   set Error(Path)    { "Le fichier d'exportation n'est pas spécifié" "Output file not specified" }
   set Error(Data)    { "Il n'y a aucune donnée RPN a exporter. Vous devez afficher les champs à exporter dans la vue active afin de pouvoir les exporter"
                        "No RPN data to export. You have to display the fields on the active viewport to be able to export them." }
}

namespace eval Export::Raster {
   variable Data
   variable Lbl
   variable Bubble
   variable Error
   variable Msg

   set Data(Image)   0
   set Data(Formats) { {GeoTIFF "GTiff" {*.tif}}
                       {KML "KMZ" {*.kmz}}
                       {Arc/Info ASCII Grid "AAIGrid" {*.adf}}
                       {ADRG/ARC Digitilized Raster Graphics "ADRG" {*.gen *.thf}}
                       {Military Elevation Data "DTED" {*.dt0 *.dt1}}
                       {Magellan BLX Topo "BLX" {*.blx *.xlb}}
                       {ERMapper ERS "ERS" { *.ers}}
                       {GMT Compatible NetCDF "GMT" {*.cdf}}
                       {Virtual Raster "VRT" {*.vrt}}
                       {National Imagery Transmission Format "NITF" {*.nitf}}
                       {Erdas Imagine Images "HFA" {*.img}}
                       {MS Windows Device Independent Bitmap "BMP" {*.bmp}}
                       {PCIDSK Database File "PCIDSK" {*.pci}}
                       {HDF4 Dataset "HDF4" {*.hdf}}
                       {HDF5 Dataset "HDF5" {*.hdf}}
                       {Image Display and Analysis "IDA" {*.ida}}
                       {JPEG and JPEG2000 "JPEG2000" {*.jpg *.jp2 *.j2k}}
                       {SRTM HGT Format "SRTMHGT" {*.hgt}}
                       {Raster Matrix Format "RMF" {*.rsw *.mtw}}
                       {ENVI .hdr Labelled "ENVI" {*.envi}}
                       {ESRI .hdr Labelled "EHdr"{*.hdr}}
                       {Atlantis MFF Raster "MFF" {*.mff}}
                       {Atlantis MFF2 (HKV) Raster "MFF2" {*.mff}}
                       {NASA ELAS "ELAS" {*}}
                       {NOAA .gtx vertical datum shift "GTX" {*.gtx}}
                       {HF2/HFZ heightfield raster "HF2" {*.hf2 *.hfz}}
                       {Image Display and Analysis (WinDisp) "IDA" {*.ida}}
                       {ILWIS Raster Map "ILWIS" {*.mpr *.mpl}}
                       {Intergraph Raster "INGR" {*.ingr}}
                       {USGS Astrogeology ISIS cube (Version 2) "ISIS2" {*.isis}}
                       {JPEG JFIF "JPEG" {*,jpg}}
                       {Vexcel MFF "MFF" {*.mff}}
                       {Vexcel MFF2 "MFF2" {*.HKV}}
                       {NTv2 Datum Grid Shift "NTv2" {*.nt}}
                       {PCRaster "PCRaster" {*}}
                       {Raster Matrix Format RMF {*.rsw *.mtw}}
                       {SAGA GIS Binary format "SAGA" {*.sag}}
                       {SGI Image Format "SGI" {*.sgi}}
                       {USGS ASCII DEM / CDED "USGSDEM" {*.dem}}
                       {ASCII Gridded XYZ "XYZ" {*.xyz}}
                       {ZMap Plus Grid"ZMap" {*.zmap}}
                       {VTP .bt (Binary Terrain) 1.3 Format "BT" {*.bt}}}

   set Lbl(Area)     { "Région" "Area" }
   set Lbl(Type)     { "Type" "Type" }
   set Lbl(Values)   { "Valeurs" "Values" }
   set Lbl(Res)      { "Résolution" "Resolution" }
   set Lbl(Data)     { "Données" "Data" }
   set Lbl(ImageRGB) { "RGBA" "RGBA" }
   set Lbl(ImageIDX) { "Palette" "Color index" }

   set Msg(Export)    { "Exportation de " "Exporting data " }

   set Error(Size)    { "Les dimensions sont invaldes, vérifié la résolution ou les coordonées." "Dimension is invalid, check resolution or coordinates." }

   set Bubble(Lat0)   { "Latitude du coin inférieur gauche" "Lower left corner latitude" }
   set Bubble(Lon0)   { "Longitude  du coin inférieur gauche" "Lower left corner longitude" }
   set Bubble(Lat1)   { "Latitude du coin supérieur droit" "Upper right corner latitude" }
   set Bubble(Lon1)   { "Longitude du coin supérieur droit" "Upper right corner longitude" }
   set Bubble(Res)    { "Résolution en degrés" "Resolution in degrees" }
   set Bubble(Values) { "Méthode d'exportation des valeurs" "Values exportation method" }
}

namespace eval Export::Vector {
   variable Data
   variable Lbl
   variable Bubble

   set Data(Formats) { {ESRI Shape "ESRI Shapefile" {*.shp *.shx *.dbf}}
                       {GeoJSON  "GeoJSON" {*.json}}
                       {KML "KML" {*.kml}}
                       {Géoconcept Export "Geoconcept" {*.gxt}}
                       {Geography Markup Language "GML" {*.gml}}
                       {GMT ASCII Vectors "GMT" {*.gmt}}
                       {GPS Exchange Format "GPX" {*.gpx}}
                       {GPSTrackMaker "GPSTrackMaker" {*.gtm *.gtz}}
                       {MapInfo Binary "MapInfo File" {*.mif *.mid}}
                       {SQLite/SpatiaLite "SQLite" {}}
                       {PostgreSQL "PostgreSQL" {}} }
}

#----------------------------------------------------------------------------
# Nom      : <Export::Raster::Legend>
# Creation : Juin 2012 - J.P. Gauthier - CMC/CMOE
#
# But      : Creer une image legende echelle de couleur
#
# Parametres :
#  <Path>         : Repertoire de sauvegarde du fichier
#  <Field>        : Champs RPN
#  <Height>       : Hauteur de la legende
#  <Width>        : Largeur de la legende
#  <FontColor>    : Couleur des polices (hexa: #ffffff)
#  <BGColor>      : Couleur de l'arrierre plan (hexa: #ffffff)
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc Export::Legend { Path Field Height Width FontColor { BGColor "" } } {
   variable Error

   set map    [fstdfield configure $Field -colormap]
   set inter  [fstdfield configure $Field -intervals]
   set min    [fstdfield configure $Field -min]
   set max    [fstdfield configure $Field -max]
   set factor [fstdfield configure $Field -factor]
   set unit   [fstdfield configure $Field -unit]

   if { $min=="" } {
      set min [lindex [fstdfield stats $Field -min] 0]
   }
   if { $max=="" } {
      set max [lindex [fstdfield stats $Field -max] 0]
   }

   set cwidth  24
   set cheight [expr $Height-10]
   set txtpos  [expr $Width-20-24]

   #----- Create the colormap image map
   gdalband create EXPORTLEGEND $cwidth $cheight 4 Byte
   gdalband configure EXPORTLEGEND -colormap $map -factor $factor -intervals $inter -min $min -max $max

   set vals [gdalband mapimage EXPORTLEGEND]
   gdalfile createcopy $Path.tmp EXPORTLEGEND png
   gdalband free EXPORTLEGEND

   if { [llength $inter] } {
      #----- Use intervals if there were any
      set vals $inter
   } elseif { $min==0.0 && $max==1.0 } {
      #----- Use Min-Max if no limits defined
      set vals { Min Max }
   }

   #----- Make sure we have the maximum number of values without overlap
   set maxvals [expr ($Height-10)/15]
   if { [llength $vals]>$maxvals } {
      set news {}
      set dy   [expr double([llength $vals]/$maxvals)]
      for { set i 0 } { $i < [llength $vals] } { set i [expr $i+$dy] } {
         lappend news [lindex $vals [expr round($i)]]
      }
      set vals $news
   }

   #----- Paste the colorbar into the legend
   if { $BGColor=="" } {
      set bg "transparent"
   } else {
      set bg $BGColor
   }

   set params [concat -size ${Width}x${Height} xc:$bg -weight bold -pointsize 13 -fill $FontColor \
      -draw \"image SrcOver $txtpos,5 $cwidth,$cheight '$Path.tmp'\"]

   #----- Place the numbers beside the colorbar.
   set lv [llength $vals]
   set offsetText [expr double($cheight-5)/($lv==2?1:$lv)]
   set offset [expr $Height-5]
   foreach val $vals {
      set xOffset [expr $txtpos-3 - ([string length $val] * 7)]
      set params  [concat $params -draw \"text $xOffset,$offset '$val'\"]
      set offset  [expr $offset-$offsetText]
   }

   #----- If a layer was passed, add info from the layer definition, otherwise use style
   set desc "[fstdfield configure $Field -desc] ([fstdfield configure $Field -unit])"
   set params [concat $params -rotate \"90\" -gravity \"South\" -draw \"text 0,0 '$desc'\" -rotate \"-90\" ]

   #----- Use ImageMagick to add the labels
   if { [catch { eval exec convert -depth 8 $params $Path } msg] } {
      Dialog::Error .export $Error(Legend)
   }
   file delete $Path.tmp
}

#----------------------------------------------------------------------------
# Nom      : <Export::Raster::Export>
# Creation : Novembre 2004 - J.P. Gauthier - CMC/CMOE
#
# But      : Exporter les donnees en format raster
#
# Parametres :
#  <Path>    : Repertoire de sauvegarde du fichier
#  <Format>  : Format du fichier
#
# Retour:
#
# Remarques :
#    - On exporte seulement les champs rasters selon les parametres
#
#----------------------------------------------------------------------------

proc Export::Raster::Export { Path Format } {
   global GDefs
   variable Data
   variable Error
   variable Msg

   set no   0
   set file [file rootname $Path]
   set ext  [file extension $Path]

   if { $Export::Data(DX)==0 || $Export::Data(DY)==0 } {
     Dialog::Error .export $Error(Size)
     return False
   }

   if { $Format=="KMZ" } {
      set f [open ${file}.kml w]
      puts $f "<kml xmlns=\"http://earth.google.com/kml/2.1\">
<Document>
   <ScreenOverlay>
      <name>Logo</name>
      <Icon>
         <href>Logo_SMC.png</href>
      </Icon>
      <overlayXY x=\"0\" y=\"1\" xunits=\"fraction\" yunits=\"fraction\"/>
      <screenXY x=\"0\" y=\"1\" xunits=\"fraction\" yunits=\"fraction\"/>
   </ScreenOverlay>"
   }

   foreach field $FSTD::Data(List) {

      if { $Export::Data($field) } {

         set nv      [fstdfield define $field -NOMVAR]
         set lvl     [lindex [fstdfield stats $field -levels] [fstdfield stats $field -level]]
         set lvltype [fstdfield stats $field -leveltype]
         set sec0    [fstdstamp toseconds [fstdfield define $field -DATEV]]
         set sec1    [expr $sec0+[fstdfield define $field -DEET]*[fstdfield define $field -NPAS]]

         set desc "$nv [clock format $sec0 -gmt True] $lvl $lvltype"
         Dialog::Wait .export $Msg(Export) $desc

         switch $Data(Image) {
            0 {  set map [fstdfield configure $field -colormap]
               fstdfield configure $field -colormap ""
               gdalband create BAND $Export::Data(DX) $Export::Data(DY) 1 Float32
            }
            1 { gdalband create BAND $Export::Data(DX) $Export::Data(DY) 4 Byte }
            2 { gdalband create BAND $Export::Data(DX) $Export::Data(DY) 1 Byte }
         }

         gdalband define BAND -transform [list $Export::Data(Lon0) $Export::Data(DLon) 0.0 $Export::Data(Lat1) 0.0 -$Export::Data(DLat)]
         gdalband import BAND $field

         if { $Format=="KMZ" } {
            Export::Legend ${file}_${no}_${nv}_legend.png $field 270 96 #FFFFFF

            puts $f "
   <ScreenOverlay>
      <TimeSpan>
         <begin>[ISO8601::FromSeconds $sec0]</begin>
         <end>[ISO8601::FromSeconds $sec1]</end>
      </TimeSpan>
      <name>Legend</name>
         <Icon>
            <href>[file tail ${file}_${no}_${nv}_legend.png]</href>
         </Icon>
      <overlayXY x=\"1\" y=\".5\" xunits=\"fraction\" yunits=\"fraction\"/>
      <screenXY x=\"1\" y=\".5\" xunits=\"fraction\" yunits=\"fraction\"/>
      <rotationXY x=\"0\" y=\"0\" xunits=\"fraction\" yunits=\"fraction\"/>
      <size x=\"0\" y=\"0\" xunits=\"fraction\" yunits=\"fraction\"/>
   </ScreenOverlay>
   <GroundOverlay>
      <TimeSpan>
         <begin>[ISO8601::FromSeconds $sec0]</begin>
         <end>[ISO8601::FromSeconds $sec1]</end>
      </TimeSpan>
      <name>[fstdfield configure $field -desc] at $lvl $lvltype</name>
      <description>Valid [clock format $sec0]</description>
      <LatLonBox>
         <north>$Export::Data(Lat1)</north>
         <south>$Export::Data(Lat0)</south>
         <east>$Export::Data(Lon0)</east>
         <west>$Export::Data(Lon1)</west>
      </LatLonBox>
      <Icon>
         <href>[file tail ${file}_${no}_${nv}.tif]</href>
      </Icon>
  </GroundOverlay>\n"
            gdalfile open FILE write ${file}_${no}_${nv}.tif GTiff

            lappend kmz ${file}_${no}_${nv}.tif ${file}_${no}_${nv}_legend.png
         } else {
            gdalfile open FILE write ${file}_${no}_${nv}${ext} $Format
         }
         gdalband write BAND FILE
         gdalfile close FILE
         gdalband free BAND


         if { $Data(Image)==0 } {
            fstdfield configure $field -colormap $map
         }

         incr no
      }
   }

   if { $Format=="KMZ" } {
      puts $f "</Document>\n</kml>"
      close $f
      file delete -force ${file}.kmz
      eval exec zip -j ${file}.kmz ${file}.kml $GDefs(Dir)/Resources/Image/Symbol/Logo/LOGO_SMC.png l/Logo/Logo_SMC.png $kmz
      eval file delete ${file}.kml $kmz
   }

   Dialog::WaitDestroy
   return True
}

#----------------------------------------------------------------------------
# Nom      : <Export::Vector::Export>
# Creation : Novembre 2004 - J.P. Gauthier - CMC/CMOE
#
# But      : Exporter les donnees en format vector
#
# Parametres :
#  <Path>    : Repertoire de sauvegarde du fichier
#  <Format>  : Format du fichier
#
# Retour:
#
# Remarques :
#    - On exporte seulement les champs rasters selon les parametres
#
#----------------------------------------------------------------------------

proc Export::Vector::Export { Path Format } {
   variable Data
   variable Msg

   set no 0

   foreach field $FSTD::Data(List) {

      if { $Export::Data($field) } {
         set nv      [fstdfield define $field -NOMVAR]
         set lvl     [lindex [fstdfield stats $field -levels] [fstdfield stats $field -level]]
         set lvltype [fstdfield stats $field -leveltype]
         set date    [clock format [fstdstamp toseconds [fstdfield define $field -DATEV]] -format "%Y%m%d_%H:%M" -gmt true]

         set desc "$nv [clock format $sec0 -gmt True] $lvl $lvltype"
         Dialog::Wait .export $Msg(Export) $desc

         set layer ${nv}_${date}_${lvl}_${lvltype}

         if { $Format=="PostgreSQL" } {
            set req "PG:"
            if { $Export::Data(Host)!="" }     { append req "host=$Export::Data(Host) " }
            if { $Export::Data(Port)!="" }     { append req "port=$Export::Data(Port) " }
            if { $Export::Data(User)!="" }     { append req "user=$Export::Data(User) " }
            if { $Export::Data(Password)!="" } { append req "password=$Export::Data(Password) " }
            if { $Export::Data(DBase)!="" }    { append req "dbname=$Export::Data(DBase) " }

            ogrfile open FILE write $req $Format
         } else {
            set file [file rootname $Path]
            set ext  [file extension $Path]
            set name ${file}_${no}_${nv}${ext}

            if { [file exists $name] } {
               file delete -force $name
            }

            if { $Format=="MapInfo File" } {
               ogrfile open FILE write $name $Format { FORMAT=MIF }
            } else {
               ogrfile open FILE write $name $Format
            }
         }

         ogrlayer create FILE LAYER $layer
         ogrlayer import LAYER $field
         ogrfile close FILE
         ogrlayer free LAYER
         incr no
      }
   }
   Dialog::WaitDestroy

   return True
}

#----------------------------------------------------------------------------
# Nom      : <Export::Vector::Option>
# Creation : Novembre 2004 - J.P. Gauthier - CMC/CMOE
#
# But      : Exporter les donnees en format vectoriel
#
# Parametres :
#  <Frame>   : Identificateur du frame parent
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc Export::Vector::Option { Frame } {

}

#----------------------------------------------------------------------------
# Nom      : <Export::Raster::Option>
# Creation : Novembre 2004 - J.P. Gauthier - CMC/CMOE
#
# But      : Exporter les donnees en format raster
#
# Parametres :
#  <Frame>   : Identificateur du frame parent
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc Export::Raster::Option { Frame } {
   global GDefs
   variable Lbl
   variable Bubble

   frame $Frame.area
      label $Frame.area.lbl -text [lindex $Lbl(Area) $GDefs(Lang)] -width 10 -anchor w
      checkbutton $Frame.area.mode -variable Page::Data(ToolMode) -onvalue Export \
         -offvalue SPI -image ARROW -relief sunken -bd 1 -overrelief raised -offrelief flat -indicatoron False \
         -command { SPI::ToolMode $Page::Data(ToolMode) Data True } -selectcolor  $GDefs(ColorLight)
      entry $Frame.area.lat0 -bd 1 -bg $GDefs(ColorLight) -width 7 -textvariable Export::Data(Lat0)
      entry $Frame.area.lon0 -bd 1 -bg $GDefs(ColorLight) -width 8 -textvariable Export::Data(Lon0)
      entry $Frame.area.lat1 -bd 1 -bg $GDefs(ColorLight) -width 7 -textvariable Export::Data(Lat1)
      entry $Frame.area.lon1 -bd 1 -bg $GDefs(ColorLight) -width 8 -textvariable Export::Data(Lon1)
      pack $Frame.area.lbl $Frame.area.lat0 $Frame.area.lon0 $Frame.area.lat1 $Frame.area.lon1 -side left -fill y
      pack $Frame.area.mode -side left -fill x -expand true
      bind $Frame.area.lat0 <Any-KeyRelease> { Export::SetDim $Export::Data(Res) }
      bind $Frame.area.lat1 <Any-KeyRelease> { Export::SetDim $Export::Data(Res) }
      bind $Frame.area.lon0 <Any-KeyRelease> { Export::SetDim $Export::Data(Res) }
      bind $Frame.area.lon1 <Any-KeyRelease> { Export::SetDim $Export::Data(Res) }

   frame $Frame.res
      label $Frame.res.lbl -text [lindex $Lbl(Res) $GDefs(Lang)] -width 10 -anchor w
      entry $Frame.res.sel  -bd 1 -bg $GDefs(ColorLight) -width 6 -textvariable Export::Data(Res) -validate focusout -validatecommand { Export::SetDim %P }
      label $Frame.res.dim -textvariable Export::Data(Dim) -anchor w
      pack $Frame.res.lbl $Frame.res.sel -side left
      pack $Frame.res.dim -side left -fill x -expand True
      bind $Frame.res.sel <Any-KeyRelease> { Export::SetDim $Export::Data(Res) }

   frame $Frame.type
      label $Frame.type.lbl -text [lindex $Lbl(Values) $GDefs(Lang)] -width 10 -anchor w
      radiobutton $Frame.type.data -text [lindex $Lbl(Data) $GDefs(Lang)] -variable Export::Raster::Data(Image) -value 0 \
         -relief sunken -bd 1 -overrelief raised -offrelief flat -indicatoron False
      radiobutton $Frame.type.rgb  -text [lindex $Lbl(ImageRGB) $GDefs(Lang)] -variable Export::Raster::Data(Image) -value 1 \
         -relief sunken -bd 1 -overrelief raised -offrelief flat -indicatoron False
      radiobutton $Frame.type.idx  -text [lindex $Lbl(ImageIDX) $GDefs(Lang)] -variable Export::Raster::Data(Image) -value 2 \
         -relief sunken -bd 1 -overrelief raised -offrelief flat -indicatoron False
      pack  $Frame.type.lbl -side left
      pack  $Frame.type.data $Frame.type.rgb $Frame.type.idx -side left -fill x -expand true
   pack $Frame.area $Frame.res $Frame.type -side top -fill both -expand true

   Bubble::Create $Frame.area.lat0 $Bubble(Lat0)
   Bubble::Create $Frame.area.lon0 $Bubble(Lon0)
   Bubble::Create $Frame.area.lat1 $Bubble(Lat1)
   Bubble::Create $Frame.area.lon1 $Bubble(Lon1)
   Bubble::Create $Frame.res       $Bubble(Res)
   Bubble::Create $Frame.type      $Bubble(Values)
}


#-------------------------------------------------------------------------------
# Nom      : <Export::Close>
# Creation : Juin 2002 - J.P. Gauthier - CMC/CMOE
#
# But      : Ferme tout les fichiers ouvert et detruit la fenetre.
#
# Parametres :
#
# Retour    :
#
# Remarque :
#
#-------------------------------------------------------------------------------

proc Export::Close { } {
   variable Data

   if { $Page::Data(ToolMode)=="Export" } {
      SPI::ToolMode SPI Zoom
   }
   set Data(Coo)    ""

   if { $Data(Canvas)!="" } {
      $Data(Canvas) delete RANGEEXPORT
   }
   destroy .export
}

#-------------------------------------------------------------------------------
# Nom      : <Export::Window>
# Creation : Novembre 2002 - J.P. Gauthier - CMC/CMOE
#
# But      : Initialise la fenetre d'exportation.
#
# Parametres :
#
# Remarques :
#
#-------------------------------------------------------------------------------

proc Export::Window { } {
   global GDefs
   variable Lbl
   variable Error
   variable Bubble
   variable Data

   if { ![llength $FSTD::Data(List)] } {
      Dialog::Error . $Error(Data)
      return
   }

   toplevel     .export -relief raised -bd 1
   wm title     .export [lindex $Lbl(Export) $GDefs(Lang)]
   wm protocol  .export WM_DELETE_WINDOW { Export::Close }
   wm resizable .export 0 0
   wm geom      .export +[winfo rootx .]+[winfo rooty .]
   wm transient .export .

   labelframe .export.what -text [lindex $Lbl(What) $GDefs(Lang)]
      foreach field $FSTD::Data(List) {
         set Export::Data($field) 1
         set desc "[fstdfield define $field -NOMVAR] [clock format [fstdstamp toseconds [fstdfield define $field -DATEV]] -format "%Y%m%d_%H:%M" -gmt true] \
             [lindex [fstdfield stats $field -levels] [fstdfield stats $field -level]] [fstdfield stats $field -leveltype]"
         checkbutton .export.what.f$field -text $desc -variable Export::Data($field) -relief sunken -bd 1 -overrelief raised -offrelief flat -indicatoron False
         pack .export.what.f$field -side top -fill x -expand True
      }

   labelframe .export.how -text [lindex $Lbl(How) $GDefs(Lang)]
      frame .export.how.type
         label .export.how.type.lbl -text Type -width 14 -anchor w
         radiobutton .export.how.type.vector -text [lindex $Lbl(Vector) $GDefs(Lang)] -variable Export::Data(Type) -value Vector \
            -relief sunken -bd 1 -overrelief raised -offrelief flat -indicatoron False \
            -command { Export::SetType $Export::Data(Type) }
         radiobutton .export.how.type.raster -text [lindex $Lbl(Raster) $GDefs(Lang)] -variable Export::Data(Type) -value Raster \
            -relief sunken -bd 1 -overrelief raised -offrelief flat -indicatoron False \
            -command { Export::SetType $Export::Data(Type) }
         pack .export.how.type.lbl -side left
         pack .export.how.type.vector .export.how.type.raster -side left -fill x -ipadx 30 -expand True

      frame .export.how.sel
         label .export.how.sel.lbl -text Format -width 14 -anchor w
         ComboBox::Create .export.how.sel.sel Export::Data(Format) edit sorted nodouble -1 {} 15 6 {Export::SetFormat $Export::Data(Format)}
         pack .export.how.sel.lbl -side left
         pack .export.how.sel.sel -side left -fill x -expand True
      pack .export.how.type .export.how.sel -side top -fill x -expand True

   labelframe .export.where -text [lindex $Lbl(Where) $GDefs(Lang)]
       frame .export.where.file
         label .export.where.file.lbl -text [lindex $Lbl(File) $GDefs(Lang)] -width 14 -anchor w
         entry .export.where.file.path -bd 1 -bg $GDefs(ColorLight) -width 1 -textvariable Export::Data(Path)
         button .export.where.file.sel -image OPEN -relief flat -bd 0 -overrelief raised \
            -command { set Export::Data(Path) [FileBox::Create .export $Export::Data(Path) Save [list $Export::Data(Format)]] }
         pack .export.where.file.lbl -side left
         pack .export.where.file.path -side left -fill both -expand True
         pack .export.where.file.sel -side left -fill y

      frame .export.where.db
         frame .export.where.db.host
            label .export.where.db.host.lbl -text [lindex $Lbl(Host) $GDefs(Lang)] -width 14 -anchor w
            entry .export.where.db.host.sel -bd 1 -bg $GDefs(ColorLight) -width 36 -textvariable Export::Data(Host)
            pack .export.where.db.host.lbl -side left
            pack .export.where.db.host.sel -side left -fill both -expand True
         frame .export.where.db.port
            label .export.where.db.port.lbl -text [lindex $Lbl(Port) $GDefs(Lang)] -width 14 -anchor w
            entry .export.where.db.port.sel -bd 1 -bg $GDefs(ColorLight) -width 36 -textvariable Export::Data(Port)
            pack .export.where.db.port.lbl -side left
            pack .export.where.db.port.sel -side left -fill both -expand True
         frame .export.where.db.name
            label .export.where.db.name.lbl -text [lindex $Lbl(DBase) $GDefs(Lang)] -width 14 -anchor w
            entry .export.where.db.name.sel -bd 1 -bg $GDefs(ColorLight) -width 36 -textvariable Export::Data(DBase)
            pack .export.where.db.name.lbl -side left
            pack .export.where.db.name.sel -side left -fill both -expand True
         frame .export.where.db.user
             label .export.where.db.user.lbl -text [lindex $Lbl(User) $GDefs(Lang)] -width 14 -anchor w
            entry .export.where.db.user.sel -bd 1 -bg $GDefs(ColorLight) -width 36 -textvariable Export::Data(User)
            pack .export.where.db.user.lbl -side left
            pack .export.where.db.user.sel -side left -fill both -expand True
        frame .export.where.db.pswd
            label .export.where.db.pswd.lbl -text [lindex $Lbl(Password) $GDefs(Lang)] -width 14 -anchor w
            entry .export.where.db.pswd.sel -bd 1 -bg $GDefs(ColorLight) -width 36 -textvariable Export::Data(Password) -show *
            pack .export.where.db.pswd.lbl -side left
            pack .export.where.db.pswd.sel -side left -fill both -expand True
         pack .export.where.db.host .export.where.db.port .export.where.db.name .export.where.db.user .export.where.db.pswd \
            -side top -fill x -expand True

   labelframe .export.info -text [lindex $Lbl(Info) $GDefs(Lang)]
      text .export.info.desc -bd 1 -bg $GDefs(ColorLight) -height 5 -width 30
      pack .export.info.desc -side top -fill both -expand True

   pack .export.what .export.how .export.where -side top -fill x -expand True -padx 5

   frame .export.cmd -relief sunken -bd 1
      button .export.cmd.ok -text [lindex $Lbl(Export) $GDefs(Lang)] -bd 1 \
         -command { Export::Do }
      button .export.cmd.cancel -text [lindex $Lbl(Cancel) $GDefs(Lang)] -bd 1  \
         -command { Export::Close }
      pack .export.cmd.ok .export.cmd.cancel -side left -fill x -expand true
   pack .export.cmd -side top -fill x -expand true -padx 5 -pady 5

   Bubble:::Create .export.what       [lindex $Bubble(Data) $GDefs(Lang)]
   Bubble:::Create .export.how.type   [lindex $Bubble(Type) $GDefs(Lang)]
   Bubble:::Create .export.how.sel    [lindex $Bubble(Format) $GDefs(Lang)]
   Bubble:::Create .export.where.file [lindex $Bubble(File) $GDefs(Lang)]
   Bubble:::Create .export.where.db   [lindex $Bubble(DBase) $GDefs(Lang)]

   set format $Data(Format)
   Export::SetType $Data(Type)
   Export::SetFormat $format
   Export::SetDim $Data(Res)
}

#----------------------------------------------------------------------------
# Nom      : <Export::SetDim>
# Creation : Decembre 2008 - J.P. Gauthier - CMC/CMOE
#
# But      : Calculer les parametres des dimensions de l'image raster
#
# Parametres :
#  <Res>     : Resolution en degree
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc Export::SetDim { Res } {
   variable Data

   set Data(Res) $Res
   set Data(Dim) " deg = -"
   set Data(DY)  0
   set Data(DX)  0

   if { ![string is double $Res] } {
      return True
   }

   set t [catch {

      set dlat [expr $Export::Data(Lat1)-$Export::Data(Lat0)]
      set dlon [expr $Export::Data(Lon1)-$Export::Data(Lon0)]

      set dy [expr abs(int($dlat/$Res))]
      set dx [expr abs(int($dlon/$Res))]

      if { $dx>1 && $dy>1 } {
         set Data(DLat) [expr $dlat/($dy-1)]
         set Data(DLon) [expr $dlon/($dx-1)]

         set Data(DY)  $dy
         set Data(DX)  $dx
         set Data(Dim) " deg = $Data(DX) x $Data(DY)"
      }
   }]
   return True
}

#----------------------------------------------------------------------------
# Nom      : <Export::SetFormat>
# Creation : Decembre 2008 - J.P. Gauthier - CMC/CMOE
#
# But      : Appliquer les parametre relatifs a la selection du format
#
# Parametres :
#  <Format>  : Format d'exportation
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc Export::SetFormat { Format } {
   variable Data

   set Data(Format) $Format

   if { [lindex $Format 0]=="PostgreSQL" } {
      pack forget .export.where.file
      pack .export.where.db -side top -fill x -expand True
   } else {
      pack forget .export.where.db
      pack .export.where.file -side top -fill x -expand True
   }
}

#----------------------------------------------------------------------------
# Nom      : <Export::SetType>
# Creation : Decembre 2008 - J.P. Gauthier - CMC/CMOE
#
# But      : Appliquer les parametre relatifs a la selection du type d'export
#
# Parametres :
#  <Type>    : Type d'exportation (Vector ou Raster)
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc Export::SetType { Type } {
   variable Data

   ComboBox::DelAll .export.how.sel.sel
   eval ComboBox::AddList .export.how.sel.sel \${Export::${Type}::Data(Formats)}

   destroy .export.option
   labelframe .export.option -text Options
   pack .export.option -side top -fill x -expand true -after .export.where -padx 5
   Export::${Type}::Option .export.option
}


#----------------------------------------------------------------------------
# Nom      : <Export::Do>
# Creation : Decembre 2008 - J.P. Gauthier - CMC/CMOE
#
# But      : Lancer le processus d'exportation
#
# Parametres :
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc Export::Do { } {
   global GDefs
   variable Data
   variable Error

   set format [lindex $Data(Format) end-1]

   if { $format=="PostgreSQL" } {

   } else {
      if { $Data(Path)=="" } {
         Dialog::Error .export $Error(Path)
         return
      }
   }

   .export configure -cursor watch
   update idletasks

   eval set proc Export::${Data(Type)}::Export
   set ok [$proc $Data(Path) $format]

   .export configure -cursor left_ptr

   if { $ok } {
      Export::Close
   }
}

#----------------------------------------------------------------------------
# Nom      : <Export::UpdateItems>
# Creation : Novembre 2002 - J.P. Gauthier - CMC/CMOE
#
# But      :
#
# Parametres :
#  <Canvas>  : Identificateur du canvas
#  <VP>      : Identificateur du viewport
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc Export::UpdateItems { Frame } {
   global GDefs
   variable Data

   if { $Data(Coo)!="" && $Frame==$Data(Frame) } {
     $Data(Canvas) delete RANGEEXPORT
     Viewport::DrawRange $Frame $Data(VP) $Data(Lat0) $Data(Lon0) $Data(Lat1) $Data(Lon1) RANGEEXPORT red
   }
}

#----------------------------------------------------------------------------
# Nom      : <Export::Draw...>
# Creation : Novembre 2002 - J.P. Gauthier - CMC/CMOE
#
# But      : Fonctions de manipulation sur la projection
#
# Parametres :
#  <Frame>   : Identificateur de Page
#  <VP>      : Identificateur du Viewport
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc Export::DrawInit  { Frame VP } {
   variable Data

   set Data(Color)  red
   set Data(Lat0)   $Viewport::Map(LatCursor)
   set Data(Lon0)   $Viewport::Map(LonCursor)
}

proc Export::Draw       { Frame VP } {
   variable Data

   if { $Data(Canvas)!="" } {
      $Data(Canvas) delete RANGEEXPORT
   }

   set Data(Lat1)   $Viewport::Map(LatCursor)
   set Data(Lon1)   $Viewport::Map(LonCursor)

   set Data(Canvas) $Page::Data(Canvas)
   set Data(Frame)  $Frame
   set Data(VP)     $VP

   Viewport::DrawRange $Frame $VP $Data(Lat0) $Data(Lon0) $Data(Lat1) $Data(Lon1) RANGEEXPORT red
   Export::SetDim $Data(Res)
}

proc Export::DrawDone { Frame VP } {
   variable Data

   if { $Data(Lat0)>$Data(Lat1) } {
      set tmp $Data(Lat1)
      set Data(Lat1) $Data(Lat0)
      set Data(Lat0) $tmp
   }

   if { $Data(Lon0)>$Data(Lon1) } {
      set tmp $Data(Lon1)
      set Data(Lon1) $Data(Lon0)
      set Data(Lon0) $tmp
   }

   if { $Data(Lat0)==$Data(Lat1) || $Data(Lon0)==$Data(Lon1) } {
      set Data(Coo) ""
   } else {
      set Data(Coo) "$Data(Lat0) $Data(Lon0) $Data(Lat1) $Data(Lon1)"
   }
   Export::SetDim $Data(Res)
}

proc Export::MoveInit { Frame VP } {
   variable Data

   set Data(LonD) $Viewport::Map(LonCursor)
   set Data(LatD) $Viewport::Map(LatCursor)
}

proc Export::Move { Frame VP } {
   variable Data

   #----- Effectuer la translation

   set lat0 [expr $Data(Lat0) + $Viewport::Map(LatCursor) - $Data(LatD)]
   set lat1 [expr $Data(Lat1) + $Viewport::Map(LatCursor) - $Data(LatD)]

   if { $lat0 > -90.0 && $lat0 < 90.0 && $lat1 > -90.0 && $lat1 < 90.0 } {

      set Data(Lat0) $lat0
      set Data(Lat1) $lat1
      eval set Data(Lon0) [Viewport::CheckCoord [expr $Data(Lon0) + $Viewport::Map(LonCursor) - $Data(LonD)]]
      eval set Data(Lon1) [Viewport::CheckCoord [expr $Data(Lon1) + $Viewport::Map(LonCursor) - $Data(LonD)]]
   }

   #----- Reaffecter le point de reference de translation

   if { $Data(Canvas)!="" } {
      $Data(Canvas) delete RANGEEXPORT
   }

   set Data(LonD) $Viewport::Map(LonCursor)
   set Data(LatD) $Viewport::Map(LatCursor)
   set Data(Canvas) $Page::Data(Canvas)
   set Data(Frame)  $Frame
   set Data(VP)     $VP

   Viewport::DrawRange $Frame $VP $Data(Lat0) $Data(Lon0) $Data(Lat1) $Data(Lon1) RANGEEXPORT red
}

proc Export::MoveDone { Frame VP } {
   variable Data

   Export::DrawDone $Frame $VP
}
