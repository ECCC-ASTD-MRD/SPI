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
#   Aucune.
#
# Modifications :
#
#   Nom         : -
#   Date        : -
#   Description : -
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
   set Data(Res)    1000
   set Data(Coo)    ""
   set Data(Path)   ""
   set Data(Type)   Vector
   set Data(Types)  { Vector Raster }
   set Data(Format) {Shape "ESRI Shapefile" {*.shp *.shx *.dbf}}

   set Lbl(Export)         { "Exporter" "Export" }
   set Lbl(File)           { "Fichier" "File" }
   set Lbl(Format)         { "Format" "Format" }
   set Lbl(Cancel)         { "Annuler" "Cancel" }

   set Bubble(File)        { "Chemin complet du fichier d'exportation" "Full export path" }
   set Bubble(Path)        { "Sélection du repertoire" "Select path" }

   set Error(Path)         { "Le fichier d'exportation n'est pas spécifié" "Output file not specified" }
}

namespace eval Export::Raster {
   variable Data
   variable Lbl
   variable Bubble

   set Data(Image)   0
   set Data(Formats) { {GeoTIFF "GTiff" {*.tif}}
                       {Virtual Raster "VRT" {*.vrt}}
                       {National Imagery Transmission Format "NITF" {*.nitf}}
                       {Erdas Imagine Images "HFA" {*.img}}
                       {MS Windows Device Independent Bitmap "BMP" {*.bmp}}
                       {PCIDSK Database File "PCIDSK" {*.pci}}
                       {HDF4 Dataset "HDF4Image" {*.hdf}}
                       {ENVI .hdr Labelled "ENVI" {*.envi}}
                       {Atlantis MFF Raster "MFF" {*.mff}}
                       {Atlantis MFF2 (HKV) Raster "MFF2" {*.mff}}
                       {VTP .bt (Binary Terrain) 1.3 Format "BT" {*.bt}}}

   set Lbl(Area)     { "Région" "Area" }
   set Lbl(Type)     { "Type" "Type" }
   set Lbl(Data)     { "Données" "Data" }
   set Lbl(ImageRGB) { "RGB" "RGB" }
   set Lbl(ImageIDX) { "Palette" "Color index" }

   set Bubble(Lat0) { "Latitude du coin inférieur gauche" "Lower left corner latitude" }
   set Bubble(Lon0) { "Longitude  du coin inférieur gauche" "Lower left corner longitude" }
   set Bubble(Lat1) { "Latitude du coin supérieur droit" "Upper right corner latitude" }
   set Bubble(Lon1) { "Longitude du coin supérieur droit" "Upper right corner longitude" }
   set Bubble(Res)  { "Résolution maximale en pixels" "Maximum pixel resolution" }
}

namespace eval Export::Vector {
   variable Data
   variable Lbl
   variable Bubble

   set Data(Formats) { {Shape "ESRI Shapefile" {*.shp *.shx *.dbf}}
                       {MapInfo Binary "MapInfo File" {*.tab *.dat *.map *.id }}
                       {MapInfo Export "MapInfo File" {*.mif *.mid }}}

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
# Modifications :
#
#    Nom         : -
#    Date        : -
#    Description : -
#----------------------------------------------------------------------------

proc Export::Raster::Export { Path Format } {
   variable Data

   set no 0

   set dlat [expr $Export::Data(Lat1)-$Export::Data(Lat0)]
   set dlon [expr $Export::Data(Lon1)-$Export::Data(Lon0)]

   if { $dlat>$dlon } {
      set dy [expr int($Export::Data(Res))]
      set dx [expr int($dlon*$Export::Data(Res)/$dlat)]
   } else {
      set dy [expr int($dlat*$Export::Data(Res)/$dlon)]
      set dx [expr int($Export::Data(Res))]
   }

   set dlat [expr $dlat/($dy-1)]
   set dlon [expr $dlon/($dx-1)]

   set file [file rootname $Path]
   set ext  [file extension $Path]

   puts stderr "Export::Raster::Export: Output dimensions $dx ($dlon) x $dy ($dlat)"

   foreach field $FSTD::Data(List) {

      set desc ""
      set nv [fstdfield define $field -NOMVAR]

      switch $Data(Image) {
         0 {  set map [fstdfield configure $field -colormap]
              fstdfield configure $field -colormap ""
              gdalband create BAND $dx $dy 1 Float32
           }
         1 { gdalband create BAND $dx $dy 4 Byte }
         2 { gdalband create BAND $dx $dy 1 Byte }
      }

      # dx sx ry dy rx sy
      puts stderr "$Export::Data(Lon0) $dlon 0.0 $Export::Data(Lat1) 0.0 -$dlat"

      gdalband define BAND -transform [list $Export::Data(Lon0) $dlon 0.0 $Export::Data(Lat1) 0.0 -$dlat]
      gdalband import BAND $field
      gdalfile open FILE write $file.[incr no]_$nv$ext $Format
      gdalband write BAND FILE
      gdalfile close FILE
      gdalband free BAND

      if { $Data(Image)==0 } {
         fstdfield configure $field -colormap $map
      }

      incr no
   }
}

proc Export::Raster::Is { Format } {
   variable Data

   return [lsearch -exact $Data(Formats) $Format]
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
# Modifications :
#
#    Nom         : -
#    Date        : -
#    Description : -
#----------------------------------------------------------------------------

proc Export::Vector::Export { Path Format } {
   variable Data

   set no 0

   foreach field $FSTD::Data(List) {

      set nv   [fstdfield define $field -NOMVAR]
      set file [file rootname $Path]
      set ext  [file extension $Path]

      if { [file exists $file.[incr no]_$nv$ext] } {
         file delete -force $file.[incr no]_$nv$ext
      }
      ogrfile open FILE write $file.[incr no]_$nv$ext $Format
      ogrlayer create FILE LAYER $nv
      ogrlayer import LAYER $field
      ogrfile close FILE
      ogrlayer free LAYER
   }
}

proc Export::Vector::Is { Format } {
   variable Data

   return [lsearch -exact $Data(Formats) $Format]
}

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
# Modifications :
#
#    Nom         : -
#    Date        : -
#    Description : -
#----------------------------------------------------------------------------

proc Export::Raster::Option { Frame } {
   global GDefs
   variable Lbl
   variable Bubble

   frame $Frame.area
      label $Frame.area.lbl -text [lindex $Lbl(Area) $GDefs(Lang)] -width 7 -anchor w
      checkbutton $Frame.area.mode -variable Page::Data(ToolMode) -onvalue Export \
         -offvalue SPI -image ARROW -relief sunken -bd 1 -overrelief raised -offrelief flat -indicatoron False \
         -command "SPI::ToolMode Export Data True" -selectcolor  $GDefs(ColorLight)
      entry $Frame.area.lat0 -bd 1 -bg $GDefs(ColorLight) -width 7 -textvariable Export::Data(Lat0)
      entry $Frame.area.lon0 -bd 1 -bg $GDefs(ColorLight) -width 8 -textvariable Export::Data(Lon0)
      entry $Frame.area.lat1 -bd 1 -bg $GDefs(ColorLight) -width 7 -textvariable Export::Data(Lat1)
      entry $Frame.area.lon1 -bd 1 -bg $GDefs(ColorLight) -width 8 -textvariable Export::Data(Lon1)
      entry $Frame.area.res  -bd 1 -bg $GDefs(ColorLight) -width 4 -textvariable Export::Data(Res)
      pack $Frame.area.lbl $Frame.area.lat0 $Frame.area.lon0 $Frame.area.lat1 $Frame.area.lon1 $Frame.area.res -side left -fill y
      pack $Frame.area.mode -side left -fill x -expand true
   frame $Frame.type
      label $Frame.type.lbl -text [lindex $Lbl(Type) $GDefs(Lang)] -width 7 -anchor w
      radiobutton $Frame.type.data -text [lindex $Lbl(Data) $GDefs(Lang)] -variable Export::Raster::Data(Image) -value 0 \
         -relief sunken -bd 1 -overrelief raised -offrelief flat -indicatoron False
      radiobutton $Frame.type.rgb  -text [lindex $Lbl(ImageRGB) $GDefs(Lang)] -variable Export::Raster::Data(Image) -value 1 \
         -relief sunken -bd 1 -overrelief raised -offrelief flat -indicatoron False
      radiobutton $Frame.type.idx  -text [lindex $Lbl(ImageIDX) $GDefs(Lang)] -variable Export::Raster::Data(Image) -value 2 \
         -relief sunken -bd 1 -overrelief raised -offrelief flat -indicatoron False
      pack  $Frame.type.lbl -side left
      pack  $Frame.type.data $Frame.type.rgb $Frame.type.idx -side left -fill x -expand true
   pack $Frame.area $Frame.type -side top -fill both -expand true

   Bubble::Create $Frame.lat0 [lindex $Bubble(Lat0) $GDefs(Lang)]
   Bubble::Create $Frame.lon0 [lindex $Bubble(Lon0) $GDefs(Lang)]
   Bubble::Create $Frame.lat1 [lindex $Bubble(Lat1) $GDefs(Lang)]
   Bubble::Create $Frame.lon1 [lindex $Bubble(Lon1) $GDefs(Lang)]
   Bubble::Create $Frame.res  [lindex $Bubble(Res) $GDefs(Lang)]
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
# Modifications  :
#    Nom         : -
#    Date        : -
#    Description : -
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
#    Aucune.
#
# Modifications :
#
#    Nom         : -
#    Date        : -
#    Description : -
#
#-------------------------------------------------------------------------------

proc Export::Window { } {
   global GDefs
   variable Lbl
   variable Bubble
   variable Data

   toplevel     .export
   wm title     .export [lindex $Lbl(Export) $GDefs(Lang)]
   wm protocol  .export WM_DELETE_WINDOW { Export::Close }
   wm resizable .export 0 0
   wm geom      .export +[winfo rootx .]+[winfo rooty .]
   wm transient .export .

   frame .export.file -relief raised -bd 1
      label .export.file.lbl -text [lindex $Lbl(File) $GDefs(Lang)] -width 7 -anchor w
      entry .export.file.path -bd 1 -bg $GDefs(ColorLight) -width 36 -textvariable Export::Data(Path)
      button .export.file.sel -image OPEN -relief flat -bd 0 -overrelief raised \
         -command { Export::Set [FileBox::Create .export $Export::Data(Path) Save [concat $Export::Vector::Data(Formats) $Export::Raster::Data(Formats)]] }
      pack .export.file.lbl .export.file.path .export.file.sel -side left -fill y
   pack .export.file -side top -fill x -expand true

   frame .export.option -relief raised -bd 1
   pack .export.option -side top -fill x -expand true

   frame .export.cmd
      button .export.cmd.ok -text [lindex $Lbl(Export) $GDefs(Lang)] -bd 1 \
         -command { Export::Do }
      button .export.cmd.cancel -text [lindex $Lbl(Cancel) $GDefs(Lang)] -bd 1  \
         -command { Export::Close }
      pack .export.cmd.ok .export.cmd.cancel -side left -fill x -expand true
   pack .export.cmd -side top -fill x -expand true

   Export::Set $Data(Path)

   Bubble::Create .export.file.path [lindex $Bubble(File) $GDefs(Lang)]
   Bubble::Create .export.file.sel  [lindex $Bubble(Path) $GDefs(Lang)]
}

proc Export::Set { Path } {
   variable Data

   if { $Path!="" } {
      set Data(Path) $Path
      set Data(Format) [FileBox::GetType]
   }

   destroy .export.option

   foreach type $Data(Types) {
      if { [Export::${type}::Is $Data(Format)]!=-1 } {
         set Data(Type) $type
         frame .export.option -relief raised -bd 1
         pack .export.option -side top -fill x -expand true -after .export.file
         Export::${type}::Option .export.option
         break
      }
   }
}


proc Export::Do { } {
   global GDefs
   variable Data
   variable Error

   if { $Data(Path)=="" } {
      Dialog::CreateError .export [lindex $Error(Path) $GDefs(Lang)] $GDefs(Lang)
      return
   }

   .export configure -cursor watch
   update idletasks

   eval Export::${Data(Type)}::Export $Data(Path) \"[lindex $Data(Format) end-1]\"

   .export configure -cursor left_ptr
   Export::Close
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
# Modifications :
#
#    Nom         : -
#    Date        : -
#    Description : -
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
# Modifications :
#
#    Nom         : -
#    Date        : -
#    Description : -
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
