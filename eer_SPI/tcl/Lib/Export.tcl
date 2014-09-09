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

package provide Export 1.0

catch { SPI::Splash "Loading Package Export 1.0" }

package require Dialog
package require Convert

namespace eval Export {
   variable Lbl
   variable Bubble
   variable Param
   variable Data
   variable Msg
   variable Error

   set Data(Canvas) ""                    ;#Canvas Actif
   set Data(Frame)  ""                    ;#Frame actif
   set Data(VP)     ""                    ;#Viewport actif

   set Data(Coo)    ""
   set Data(Path)   ""
   set Data(Type)   Vector
   set Data(Types)  { Vector Raster }
   set Data(Format) {ESRI Shape "ESRI Shapefile" {*.shp *.shx *.dbf}}
   set Data(RPN)     1

   set Lbl(Export)   { "Exporter" "Export" }
   set Lbl(File)     { "Fichier" "File" }
   set Lbl(Cancel)   { "Annuler" "Cancel" }
   set Lbl(Vector)   { "Vectoriel" "Vectorial" }
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
   set Lbl(RPN)      { "Champs FSTD RPN" "RPN FSTD fields" }

   set Bubble(Type)   { "Type d'exportation des données" "Export Type of data" }
   set Bubble(Format) { "Formats des données exportées" "Format of exported data" }
   set Bubble(Data)   { "Sélection des données a exporter" "Select data to export" }
   set Bubble(File)   { "Nom du fichier d'exportation. Peut inclure des wildcards:\n\t%n nomvar\n\t%l level\n\t%h level type\n\t%e etiket\n\t%d date\n\t%t time\n\t%1 ip1\n\t%2 ip2\n\t%3 ip3" "Export filename. Can include wildcards:\n\t%n nomvar\n\t%l level\n\t%h level type\n\t%e etiket\n\t%d date\n\t%t time\n\t%1 ip1\n\t%2 ip2\n\t%3 ip3" }
   set Bubble(DBase)  { "Paramêtres de connection a la base de donnée" "Database connexion parameters" }

   set Msg(Export)    { "Exportation de " "Exporting data " }

   set Error(Legend)  { "Problème lors de la création de la légende (convert)" "Problem creating legend (convert)" }
   set Error(Path)    { "Le fichier d'exportation n'est pas spécifié" "Output file not specified" }
   set Error(Data)    { "Il n'y a aucune donnée RPN a exporter. Vous devez afficher les champs à exporter dans la vue active afin de pouvoir les exporter"
                        "No RPN data to export. You have to display the fields on the active viewport to be able to export them." }

   georef create EXPORT_PROJ { GEOGCS["GCS_WGS_1984",DATUM["D_WGS_1984",SPHEROID["WGS84",6378137,298.257223563]],PRIMEM["Greenwich",0],UNIT["Degree",0.017453292519943295]] }
}

namespace eval Export::Raster {
   variable Param
   variable Lbl
   variable Bubble
   variable Error

   set Param(Res)     0.1
   set Param(Dim)     " deg = -"
   set Param(DY)      0
   set Param(DX)      0
   set Param(DLat)    0
   set Param(DLon)    0
   set Param(Lat0)    -999
   set Param(Lon0)    -999
   set Param(Lat1)    -999
   set Param(Lon1)    -999
   set Param(Mode)    RGBA
   set Param(Modes)   { INDEX RGBA DATA }

   set Param(Formats) { {GeoTIFF "GTiff" {*.tif}}
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
                       {ESRI .hdr Labelled "EHdr" {*.hdr}}
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
                       {ZMap Plus Grid "ZMap" {*.zmap}}
                       {VTP .bt (Binary Terrain) 1.3 Format "BT" {*.bt}}}

   set Lbl(Area)     { "Région" "Area" }
   set Lbl(Type)     { "Type" "Type" }
   set Lbl(Values)   { "Valeurs" "Values" }
   set Lbl(Res)      { "Résolution" "Resolution" }
   set Lbl(Data)     { "Données" "Data" }
   set Lbl(ImageRGB) { "RGBA" "RGBA" }
   set Lbl(ImageIDX) { "Palette" "Color index" }

   set Error(Size)    { "Les dimensions sont invalides, vérifié la résolution ou les coordonées." "Dimension is invalid, check resolution or coordinates." }

   set Bubble(Lat0)   { "Latitude du coin inférieur gauche" "Lower left corner latitude" }
   set Bubble(Lon0)   { "Longitude  du coin inférieur gauche" "Lower left corner longitude" }
   set Bubble(Lat1)   { "Latitude du coin supérieur droit" "Upper right corner latitude" }
   set Bubble(Lon1)   { "Longitude du coin supérieur droit" "Upper right corner longitude" }
   set Bubble(Res)    { "Résolution en degrés" "Resolution in degrees" }
   set Bubble(Values) { "Méthode d'exportation des valeurs" "Values exportation method" }
}

namespace eval Export::Vector {
   variable Param

   set Param(Modes)   { POINT CELL CONTOUR }
   set Param(Formats) { {ESRI Shape "ESRI Shapefile" {*.shp *.shx *.dbf}}
                       {GeoJSON  "GeoJSON" {*.json}}
                       {KML "KMZ" {*.kml}}
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
# Nom      : <Export::Legend>
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
   set labels [fstdfield configure $Field -interlabels]
   set min    [fstdfield configure $Field -min]
   set max    [fstdfield configure $Field -max]
   set factor [fstdfield configure $Field -factor]
   set unit   [fstdfield configure $Field -unit]

   if { $map=="" } {
      return
   }

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

   if { [llength $labels] } {
      set vals $labels
   } elseif { [llength $inter] } {
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

proc Export::Raster::Export { Path Format Mode Fields } {
   global   env
   variable Param
   variable Msg
   variable Error

   if { ![llength $Fields] } {
      return True
   }

   set file [file rootname $Path]
   set ext  [file extension $Path]

   if { $Param(DX)==0 || $Param(DY)==0 } {
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

   foreach field $Fields {

      set nv      [fstdfield define $field -NOMVAR]
      set etiket  [fstdfield define $field -ETIKET]
      set ip1     [fstdfield define $field -IP1]
      set ip2     [fstdfield define $field -IP2]
      set ip3     [fstdfield define $field -IP3]
      set lvl     [fstdfield stats $field -level]
      set lvltype [fstdfield stats $field -leveltype]
      set sec0    [fstdstamp toseconds [fstdfield define $field -DATEV]]
      set sec1    [expr $sec0+[fstdfield define $field -DEET]]
      set date    [clock format $sec0 -format "%Y%m%d" -gmt true]
      set time    [clock format $sec0 -format "%H%M" -gmt true]
      set desc    "$nv [clock format $sec0 -gmt True] $lvl $lvltype"
     
      #----- Create filename 
      set name    [string map [list %n $nv %l $lvl %h ${lvltype} %e $etiket %d $date %t $time %1 $ip1 %2 $ip2 %3 $ip3] ${file}]

      if  { [set nb [llength [glob -nocomplain ${name}*${ext}]]] } {
         set name $name.[incr nb]
      }
      
      Dialog::Wait .export $Export::Msg(Export) $desc

      switch $Mode {
         DATA  { set map [fstdfield configure $field -colormap]
            fstdfield configure $field -colormap ""
            gdalband create BAND $Param(DX) $Param(DY) 1 Float32
            gdalband clear BAND [fstdfield stats $field -nodata]
         }
         RGBA  { gdalband create BAND $Param(DX) $Param(DY) 4 Byte }
         INDEX { gdalband create BAND $Param(DX) $Param(DY) 1 Byte }
      }
      gdalband define BAND -georef EXPORT_PROJ -transform [list $Param(Lon0) $Param(DLon) 0.0 $Param(Lat1) 0.0 -$Param(DLat)]
      gdalband import BAND $field

      if { $Format=="KMZ" } {
         Export::Legend ${name}_legend.png $field 270 120 #FFFFFF

         puts $f "
   <ScreenOverlay>
      <TimeSpan>
         <begin>[ISO8601::FromSeconds $sec0]</begin>
         <end>[ISO8601::FromSeconds $sec1]</end>
      </TimeSpan>
      <name>Legend</name>
         <Icon>
            <href>[file tail ${name}_legend.png]</href>
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
         <north>$Param(Lat1)</north>
         <south>$Param(Lat0)</south>
         <east>$Param(Lon1)</east>
         <west>$Param(Lon0)</west>
      </LatLonBox>
      <Icon>
         <href>[file tail ${name}.tif]</href>
      </Icon>
   </GroundOverlay>\n"
         gdalfile createcopy ${name}.tif BAND GTiff
         lappend kmz ${name}.tif ${name}_legend.png
      } else {
         gdalfile createcopy ${name}${ext} BAND $Format
      }
      gdalband free BAND

      if { $Mode=="INDEX" } {
         fstdfield configure $field -colormap $map
      }
   }

   if { $Format=="KMZ" } {
      puts $f "</Document>\n</kml>"
      close $f
      file delete -force ${name}.kmz
      eval exec zip -j ${name}.kmz ${name}.kml $env(SPI_PATH)/share/image/Symbol/Logo/Logo_SMC.png $kmz
      eval file delete ${name}.kml $kmz
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

proc Export::Vector::Export { Path Format Fields } {
   global env
   variable Msg

   if { ![llength $Fields] } {
      return True
   }

   set file [file rootname $Path]
   set ext  [file extension $Path]

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

   foreach field $Fields {

      set nv      [fstdfield define $field -NOMVAR]
      set etiket  [fstdfield define $field -ETIKET]
      set ip1     [fstdfield define $field -IP1]
      set ip2     [fstdfield define $field -IP2]
      set ip3     [fstdfield define $field -IP3]
      set lvl     [fstdfield stats $field -level]
      set lvltype [fstdfield stats $field -leveltype]
      set sec0    [fstdstamp toseconds [fstdfield define $field -DATEV]]
      set sec1    [expr $sec0+[fstdfield define $field -DEET]]
      set date    [clock format $sec0 -format "%Y%m%d" -gmt true]
      set time    [clock format $sec0 -format "%H%M" -gmt true]
      set desc    "$nv [clock format $sec0 -gmt True] $lvl $lvltype"

      #----- Create filename 
      set name    [string map [list  %n $nv %l $lvl %h ${lvltype} %e $etiket %d $date %t $time %1 $ip1 %2 $ip2 %3 $ip3] ${file}]

      if  { [set nb [llength [glob -nocomplain ${name}*${ext}]]] } {
         set name $name.[incr nb]
      }
      
      Dialog::Wait .export $Export::Msg(Export) $desc

      switch $Format {
         "PostgreSQL" {
            set req "PG:"
            if { $Export::Data(Host)!="" }     { append req "host=$Export::Data(Host) " }
            if { $Export::Data(Port)!="" }     { append req "port=$Export::Data(Port) " }
            if { $Export::Data(User)!="" }     { append req "user=$Export::Data(User) " }
            if { $Export::Data(Password)!="" } { append req "password=$Export::Data(Password) " }
            if { $Export::Data(DBase)!="" }    { append req "dbname=$Export::Data(DBase) " }

            ogrfile open FILE write $req $Format
         }
         "MapInfo File" {
            ogrfile open FILE write ${name}${ext} $Format { FORMAT=MIF }
         }
         "KMZ" {
            Export::Legend ${name}_legend.png $field 270 120 #FFFFFF
            puts $f "
   <Folder>
      <name>[fstdfield configure $field -desc] at $lvl $lvltype</name>
      <description>Valid [clock format $sec0]</description>
      <TimeSpan>
         <begin>[ISO8601::FromSeconds $sec0]</begin>
         <end>[ISO8601::FromSeconds $sec1]</end>
      </TimeSpan>
      <ScreenOverlay>
         <name>Legend</name>
            <Icon>
               <href>[file tail ${name}_legend.png]</href>
            </Icon>
         <overlayXY x=\"1\" y=\".5\" xunits=\"fraction\" yunits=\"fraction\"/>
         <screenXY x=\"1\" y=\".5\" xunits=\"fraction\" yunits=\"fraction\"/>
         <rotationXY x=\"0\" y=\"0\" xunits=\"fraction\" yunits=\"fraction\"/>
         <size x=\"0\" y=\"0\" xunits=\"fraction\" yunits=\"fraction\"/>
      </ScreenOverlay>
      <NetworkLink>
         <Link>
            <href>[file tail ${name}${ext}]</href>
         </Link>
      </NetworkLink>
   </Folder>\n"
            lappend kmz ${name}${ext} ${name}_legend.png
            ogrfile open FILE write $name${ext} KML
         }
         default {
            ogrfile open FILE write $name${ext} $Format
         }
      }
  
      ogrlayer create FILE LAYER [file tail $name] EXPORT_PROJ
      ogrlayer import LAYER $field
      ogrfile close FILE
      ogrlayer free LAYER
   }

   if { $Format=="KMZ" } {
      puts $f "</Document>\n</kml>"
      close $f
      file delete -force ${name}.kmz
      eval exec zip -j ${name}.kmz ${name}.kml $env(SPI_PATH)/share/image/Symbol/Logo/Logo_SMC.png $kmz
      eval file delete ${name}.kml $kmz
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
      checkbutton $Frame.area.mode -variable Page::Data(ToolMode) -onvalue Export::Raster \
         -offvalue SPI -image ARROW -relief sunken -bd 1 -overrelief raised -offrelief flat -indicatoron False \
         -command { SPI::ToolMode $Page::Data(ToolMode) Data True } -selectcolor  $GDefs(ColorLight)
      entry $Frame.area.lat0 -bd 1 -bg $GDefs(ColorLight) -width 7 -textvariable Export::Raster::Param(Lat0)
      entry $Frame.area.lon0 -bd 1 -bg $GDefs(ColorLight) -width 8 -textvariable Export::Raster::Param(Lon0)
      entry $Frame.area.lat1 -bd 1 -bg $GDefs(ColorLight) -width 7 -textvariable Export::Raster::Param(Lat1)
      entry $Frame.area.lon1 -bd 1 -bg $GDefs(ColorLight) -width 8 -textvariable Export::Raster::Param(Lon1)
      pack $Frame.area.lbl $Frame.area.lat0 $Frame.area.lon0 $Frame.area.lat1 $Frame.area.lon1 -side left -fill y
      pack $Frame.area.mode -side left -fill x -expand true
      bind $Frame.area.lat0 <Any-KeyRelease> { Export::Raster::Init $Export::Raster::Param(Res) }
      bind $Frame.area.lat1 <Any-KeyRelease> { Export::Raster::Init $Export::Raster::Param(Res) }
      bind $Frame.area.lon0 <Any-KeyRelease> { Export::Raster::Init $Export::Raster::Param(Res) }
      bind $Frame.area.lon1 <Any-KeyRelease> { Export::Raster::Init $Export::Raster::Param(Res) }

   frame $Frame.res
      label $Frame.res.lbl -text [lindex $Lbl(Res) $GDefs(Lang)] -width 10 -anchor w
      entry $Frame.res.sel  -bd 1 -bg $GDefs(ColorLight) -width 6 -textvariable Export::Raster::Param(Res) -validate focusout -validatecommand { Export::Raster::Init %P }
      label $Frame.res.dim -textvariable Export::Raster::Param(Dim) -anchor w
      pack $Frame.res.lbl $Frame.res.sel -side left
      pack $Frame.res.dim -side left -fill x -expand True
      bind $Frame.res.sel <Any-KeyRelease> { Export::Raster::Init $Export::Raster::Param(Res) }

   frame $Frame.type
      label $Frame.type.lbl -text [lindex $Lbl(Values) $GDefs(Lang)] -width 10 -anchor w
      radiobutton $Frame.type.data -text [lindex $Lbl(Data) $GDefs(Lang)] -variable Export::Raster::Param(Mode) -value DATA \
         -relief sunken -bd 1 -overrelief raised -offrelief flat -indicatoron False
      radiobutton $Frame.type.rgb  -text [lindex $Lbl(ImageRGB) $GDefs(Lang)] -variable Export::Raster::Param(Mode) -value RGBA \
         -relief sunken -bd 1 -overrelief raised -offrelief flat -indicatoron False
      radiobutton $Frame.type.idx  -text [lindex $Lbl(ImageIDX) $GDefs(Lang)] -variable Export::Raster::Param(Mode) -value INDEX \
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

   if { $Page::Data(ToolMode)=="Export::Raster" } {
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
      checkbutton .export.what.rpn -text [lindex $Export::Lbl(RPN) $GDefs(Lang)] -variable Export::Data(RPN) -relief sunken -bd 1 -overrelief raised -offrelief flat -indicatoron False
      pack .export.what.rpn -side top -fill x -expand True

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

   Bubble:::Create .export.what       $Bubble(Data)
   Bubble:::Create .export.how.type   $Bubble(Type)
   Bubble:::Create .export.how.sel    $Bubble(Format)
   Bubble:::Create .export.where.file $Bubble(File)
   Bubble:::Create .export.where.db   $Bubble(DBase)

   set format $Data(Format)
   Export::SetType $Data(Type)
   Export::SetFormat $format
   Export::Raster::Init $Export::Raster::Param(Res)
}

#----------------------------------------------------------------------------
# Nom      : <Export::Raster::Init>
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

proc Export::Raster::Init { Res { Lat0 -999 } { Lon0 -999 }  { Lat1 -999 } { Lon1 -999 } } {
   variable Param

   set Param(Res) $Res
   set Param(Dim) " deg = -"
   set Param(DY)   0
   set Param(DX)   0
   set Param(DLat) 0
   set Param(DLon) 0

   if { $Lat0!=-999 } {
      set Param(Lat0) [expr double($Lat0<$Lat1?$Lat0:$Lat1)]
      set Param(Lon0) [expr double($Lon0<$Lon1?$Lon0:$Lon1)]
      set Param(Lat1) [expr double($Lat0<$Lat1?$Lat1:$Lat0)]
      set Param(Lon1) [expr double($Lon0<$Lon1?$Lon1:$Lon0)]
   }
   
   set dlon [expr $Param(Lon1)-$Param(Lon0)]
   if { $dlon>180 && $dlon<358 } {
      set l $Param(Lon0)
      set Param(Lon0) [expr ($Param(Lon0)<0 && $Param(Lon1)>0 && ($dlon>180))?$Param(Lon0)-(360-$dlon):$Param(Lon1)]
      set $Param(Lon1) $l
   }

   if { ![string is double $Res] } {
      return True
   }

   set t [catch {

      set dlat [expr $Param(Lat1)-$Param(Lat0)]
      set dlon [expr $Param(Lon1)-$Param(Lon0)]

      set dy [expr abs(int($dlat/$Res))]
      set dx [expr abs(int($dlon/$Res))]

      if { $dx>1 && $dy>1 } {
         set Param(DLat) [expr $dlat/($dy-1)]
         set Param(DLon) [expr $dlon/($dx-1)]

         set Param(DY)  $dy
         set Param(DX)  $dx
         set Param(Dim) " deg = $Param(DX) x $Param(DY)"
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

   if { [lindex $Format 0]=="KML" } {
      set Export::Raster::Param(Mode) RGBA
      catch { .export.option.type.data configure -state disabled }
   } else {
      catch { .export.option.type.data configure -state normal }
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
   eval ComboBox::AddList .export.how.sel.sel \${Export::${Type}::Param(Formats)}

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
   set ok 1

   if { $format=="PostgreSQL" } {

   } else {
      if { $Data(Path)=="" } {
         Dialog::Error .export $Error(Path)
         return
      }
   }

   .export configure -cursor watch
   update idletasks

   switch $Data(Type) {
      "Raster" { set ok [Export::Raster::Export $Data(Path) $format $Export::Raster::Param(Mode) $FSTD::Data(List)] }
      "Vector" { set ok [Export::Vector::Export $Data(Path) $format $FSTD::Data(List)] }
   }

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
     Viewport::DrawRange $Frame $Data(VP) $Export::Raster::Param(Lat0) $Export::Raster::Param(Lon0) $Export::Raster::Param(Lat1) $Export::Raster::Param(Lon1) RANGEEXPORT red
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

proc Export::Raster::DrawInit  { Frame VP } {
   variable Param

   set Param(Lat0)   $Viewport::Map(LatCursor)
   set Param(Lon0)   $Viewport::Map(LonCursor)
}

proc Export::Raster::Draw       { Frame VP } {
   variable Param

   if { $Export::Data(Canvas)!="" } {
      $Export::Data(Canvas) delete RANGEEXPORT
   }

   set Param(Lat1)   $Viewport::Map(LatCursor)
   set Param(Lon1)   $Viewport::Map(LonCursor)

   set Export::Data(Canvas) $Page::Data(Canvas)
   set Export::Data(Frame)  $Frame
   set Export::Data(VP)     $VP

   Viewport::DrawRange $Frame $VP $Param(Lat0) $Param(Lon0) $Param(Lat1) $Param(Lon1) RANGEEXPORT red
   Export::Raster::Init $Export::Raster::Param(Res)
}

proc Export::Raster::DrawDone { Frame VP } {
   variable Param

   if { $Param(Lat0)>$Param(Lat1) } {
      set tmp $Param(Lat1)
      set Param(Lat1) $Param(Lat0)
      set Param(Lat0) $tmp
   }

   if { $Param(Lon0)>$Param(Lon1) && ([expr $Param(Lon0)*$Param(Lon1)]>0 || [expr 360-($Param(Lon0)-$Param(Lon1))]>180) } {
      set tmp $Param(Lon1)
      set Param(Lon1) $Param(Lon0)
      set Param(Lon0) $tmp
   }

   if { $Param(Lon0)>$Param(Lon1) } {
      set tmp $Param(Lon1)
      set Param(Lon1) $Param(Lon0)
      set Param(Lon0) $tmp
   }

   if { $Param(Lat0)==$Param(Lat1) || $Param(Lon0)==$Param(Lon1) } {
      set Param(Coo) ""
   } else {
      set Param(Coo) "$Param(Lat0) $Param(Lon0) $Param(Lat1) $Param(Lon1)"
   }
   Export::Raster::Init $Export::Raster::Param(Res)
}

proc Export::Raster::MoveInit { Frame VP } {
   variable Param

   set Param(LonD) $Viewport::Map(LonCursor)
   set Param(LatD) $Viewport::Map(LatCursor)
}

proc Export::Raster::Move { Frame VP } {
   variable Param

   #----- Effectuer la translation

   set lat0 [expr $Param(Lat0) + $Viewport::Map(LatCursor) - $Param(LatD)]
   set lat1 [expr $Param(Lat1) + $Viewport::Map(LatCursor) - $Param(LatD)]

   if { $lat0 > -90.0 && $lat0 < 90.0 && $lat1 > -90.0 && $lat1 < 90.0 } {

      set Param(Lat0) $lat0
      set Param(Lat1) $lat1
      eval set Param(Lon0) [Viewport::CheckCoord [expr $Param(Lon0) + $Viewport::Map(LonCursor) - $Param(LonD)]]
      eval set Param(Lon1) [Viewport::CheckCoord [expr $Param(Lon1) + $Viewport::Map(LonCursor) - $Param(LonD)]]
   }

   #----- Reaffecter le point de reference de translation

   if { $Export::Data(Canvas)!="" } {
      $Export::Data(Canvas) delete RANGEEXPORT
   }

   set Param(LonD) $Viewport::Map(LonCursor)
   set Param(LatD) $Viewport::Map(LatCursor)

   set Export::Data(Canvas) $Page::Data(Canvas)
   set Export::Data(Frame)  $Frame
   set Export::Data(VP)     $VP

   Viewport::DrawRange $Frame $VP $Param(Lat0) $Param(Lon0) $Param(Lat1) $Param(Lon1) RANGEEXPORT red
}

proc Export::Raster::MoveDone { Frame VP } {

   Export::Raster::DrawDone $Frame $VP
}
