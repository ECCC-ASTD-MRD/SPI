#===============================================================================
# Environnement Canada
# Centre Meteorologique Canadian
# 2100 Trans-Canadienne
# Dorval, Quebec
#
# Projet   : Librairie de "Tools" Tk.
# Fichier  : Mapper_DepotWare_WMS.tcl
# Creation : Novembre 2007 - J.P.Gauthier - CMC/CMOE
#
# Description:
#    Fontions de manipulation et de gestion des depots de donnees georeference.
#
# Remarques :
#
#===============================================================================

namespace eval Mapper::DepotWare::WMS {
   variable Data
   variable Lbl
   variable Msg

   set Lbl(URL)      { "Addresse URL" "URL Address" }
   set Lbl(User)     { "Usager" "User" }
   set Lbl(Password) { "Mot de passe" "Password" }

   set Msg(Request) { "Problème dans la requète de capacitées WMS (GetCapabilities)" "Problem requesting capabilities WMS (GetCapabilities)" }

   set Data(URL)      ""
   set Data(User)     ""
   set Data(Password) ""
   set Data(BlockSize) 512
   set Data(Layers)    {}

   set Data(SizeX)        0
   set Data(SizeY)        0
   set Data(Version)      ""
   set Data(Identifier)   ""
   set Data(Format)       ""
   set Data(Title)        ""
   set Data(BBox)         ""
   set Data(Styles)       {}
   set Data(Times)        {}
   set Data(Time)         0
   set Data(Geographic)   ""
   set Data(Opaque)       1
   set Data(Meta)         {}
}

#-------------------------------------------------------------------------------
# Nom      : <Mapper::DepotWare::WMS::Params>
# Creation : Decembre 2008 - J.P. Gauthier - CMC/CMOE
#
# But      : Fenetre des parametres du type de DB Web Mapping Service (WMS).
#
# Parametres :
#  <Frame>   : Fenetre parent
#
# Retour    :
#
# Remarque :
#
#-------------------------------------------------------------------------------

proc Mapper::DepotWare::WMS::Params { Frame } {
   global GDefs
   variable Data
   variable Lbl

   frame $Frame.path
      label $Frame.path.lbl -anchor w -text [lindex $Lbl(URL) $GDefs(Lang)] -width 15
      entry $Frame.path.ent -width 1 -bd 1 -bg $GDefs(ColorLight) -textvariable Mapper::DepotWare::WMS::Data(URL)
      pack $Frame.path.lbl -side left
      pack $Frame.path.ent -side left  -fill x -expand True
   pack $Frame.path -fill x -expand True -anchor n
   
   frame $Frame.user
      label $Frame.user.lbl -text [lindex $Lbl(User) $GDefs(Lang)] -width 15 -anchor w
      entry $Frame.user.ent -textvariable Mapper::DepotWare::WMS::Data(User) -bd 1 -bg $GDefs(ColorLight)
      label $Frame.user.plbl -text [lindex $Lbl(Password) $GDefs(Lang)] -width 12 -anchor w
      entry $Frame.user.pent -textvariable Mapper::DepotWare::WMS::Data(Password) -bd 1 -bg $GDefs(ColorLight) -show *
      pack $Frame.user.lbl -side left
      pack $Frame.user.ent -side left -fill x -expand True
      pack $Frame.user.plbl $Frame.user.pent -side left
   pack $Frame.user -side top -fill x
}

#-------------------------------------------------------------------------------
# Nom      : <Mapper::DepotWare::WMS::Select>
# Creation : Decembre 2008 - J.P. Gauthier - CMC/CMOE
#
# But      : Effectuer la selection d'une branche.
#
# Parametres :
#  <Tree>    : Arbre
#  <Branch>  : Branche selectionnee
#  <Path>    : Chemin du fichier
#  <URL>     : Mode URL
#
# Retour    :
#
# Remarque :
#
#-------------------------------------------------------------------------------

proc  Mapper::DepotWare::WMS::Parse { Tree Branch } {
   global GDefs
   variable Data
   variable Msg

   set str [http::config -useragent]
   http::config -useragent "EC/CMC/CMOE SPI $GDefs(Version) (through $str)"

   set path [$Tree get $Branch path]
   
   set url      [lindex $path 0]
   set user     [lindex $path 1]
   set password [lindex $path 2]
   set head     [list Authorization "Basic [base64::encode $user:$password]"]

   if { [string first "?" ${url}]==-1 } {
      set req [http::geturl "${url}?SERVICE=WMS&REQUEST=GetCapabilities" -headers $head -blocksize 1048580]
   } else {
      set req [http::geturl "${url}SERVICE=WMS&REQUEST=GetCapabilities" -headers $head -blocksize 1048580]
   }
   upvar #0 $req state
#   puts stderr $state(charset)

   if { [catch { set doc [dom parse [http::data $req]] } msg ] } {
      Dialog::ErrorListing . $Msg(Request) "$msg\n[http::data $req]"
      return
   }
   set root [$doc documentElement]

   set Data(Version) 1.1.1
   set Data(Format) "image/gif"

   #----- If there's no map available
   if { [set getmap [lindex [$root getElementsByTagName GetMap] 0]]=="" } {
      Dialog::ErrorListing . $Msg(Request) [http::data $req]
      return
   }

   #----- Parse the available image formats
   foreach node [$getmap getElementsByTagName Format] {
      set Data(Format) [[$node firstChild] nodeValue]
      if { $Data(Format)=="image/png" || $Data(Format)=="image/jpeg" } {
         break
      }
   }

   #----- Parse the available layers
   set layer [lindex [$root getElementsByTagName Layer] 0]
   Mapper::DepotWare::WMS::ParseLayer $path $layer $Tree $Branch
   $doc delete
   http::cleanup $req
}

proc  Mapper::DepotWare::WMS::Select { Tree Branch { Select True } { SQL "" } } {
   global GDefs
   variable Data
   variable Msg

   if { $Select } {
      set path [$Tree get $Branch path]
      if { [set def [Mapper::DepotWare::WMS::BuildXMLDef $path]]!="" } {
         if { [lsearch -exact $Viewport::Data(Data$Page::Data(Frame)) $def]==-1 } {
            set band [Mapper::GDAL::Read $def "" 3]

            #----- Decrease effective resolution (WMS-TMS)
            gdalband configure $band -texres 2
            gdalband define $band -date $Data(Time)
            gdalband stats $band -approx True

            #----- Get legend
            Mapper::DepotWare::WMS::GetLegend $band [lindex [lindex $Data(Styles) 0] end]

            #----- Get associated metadata if any
            if { [llength $Data(Meta)] } {
               if { ![catch { set req [http::geturl $Data(Meta) -blocksize 1048580] }] } {
                  gdalfile metadata $Mapper::Data(Id$band) [list [http::data $req]]
                  http::cleanup $req
               }
            }
         }
      }
   } else {
   
   }
}

#-------------------------------------------------------------------------------
# Nom      : <Mapper::DepotWare::WMS::Request>
# Creation : Decembre 2008 - J.P. Gauthier - CMC/CMOE
#
# But      : Retourner la requete formatee selon le type de DB.
#
# Parametres :
#  <Tree>    : Arbre
#  <Branch>  : Branche selectionnee
#  <Path>    : Chemin du fichier
#  <URL>     : Mode URL
#
# Retour    :
#
# Remarque :
#
#-------------------------------------------------------------------------------

proc Mapper::DepotWare::WMS::Request { } {
   variable Data

   return [list $Data(URL) $Data(User) $Data(Password)]
}

#-------------------------------------------------------------------------------
# Nom      : <Mapper::DepotWare::WMS::Add>
# Creation : Novembre 2007 - J.P. Gauthier - CMC/CMOE
#
# But      : Ajouter une branche pour une couche WMS.
#
# Parametres :
#  <Tree>    : Arbre
#  <Branch>  : Branche
#  <Layer>   : Couche
#
# Retour    :
#
# Remarque :
#
#-------------------------------------------------------------------------------

proc Mapper::DepotWare::WMS::Add { Tree Branch Layer } {
   variable Data

   set branch [$Tree insert $Branch end]

   $Tree set $branch open False
#   $Tree set $branch box  False
   $Tree set $branch name ""
   $Tree set $branch path $Layer
   $Tree set $branch type WMS
   $Tree set $branch width  -1
   $Tree set $branch height -1
   $Tree set $branch bubble [lindex $Data($Layer) 2]
   

   set bbox  [lindex $Data($Layer) 4]
   $Tree set $branch 00 [list [lindex $bbox 1] [lindex $bbox 0]]
   $Tree set $branch 01 [list [lindex $bbox 3] [lindex $bbox 0]]
   $Tree set $branch 10 [list [lindex $bbox 1] [lindex $bbox 2]]
   $Tree set $branch 11 [list [lindex $bbox 3] [lindex $bbox 2]]

   return $branch
}

#-------------------------------------------------------------------------------
# Nom      : <Mapper::DepotWare::WMS::ParseLayer>
# Creation : Novembre 2007 - J.P. Gauthier - CMC/CMOE
#
# But      : Decoder du XML pour en extraire l'information des couches.
#
# Parametres :
#  <URL>     : URL du depot
#  <Node>    : Node XML
#  <Tree>    : Arbre
#  <Branch>  : Branche
#  <First>   : Premiere couche ?
#
# Retour    :
#
# Remarque :
#   - Certaines couches cont imbriquees alors cette procedure est recursive
#
#-------------------------------------------------------------------------------

proc Mapper::DepotWare::WMS::ParseLayer { URL Node Tree Branch { First True } } {
   variable Data

   if { $First } {
      set Data(Layers) {}
      set Data(SizeX)  864000
      set Data(SizeY)  432000
      set Data(Width)  512
      set Data(Height) 512
      set Data(Cache)  1
      set Data(Opaque) {}
      set Data(Meta)   {}
   }
   set Data(Styles)   {}
   set Data(Abstract) ""
   set Data(Times)    {}
   set childs         {}

   foreach node [$Node childNodes] {
      switch [$node nodeName] {
         Layer                    { if { [$node hasAttribute opaque] } {
                                       set Data(Opaque) [$node getAttribute opaque]
                                    }
                                    lappend childs $node }
         EX_GeographicBoundingBox { Mapper::DepotWare::WMS::ParseGeographic $node }
         LatLonBoundingBox        { Mapper::DepotWare::WMS::ParseLatLonBoundingBox $node }
         BoundingBox              { Mapper::DepotWare::WMS::ParseBoundingBox $node }
         Name                     { set Data(Identifier)  [[$node firstChild] nodeValue] }
         Title                    { set Data(Title) [encoding convertfrom utf-8 [[$node firstChild] nodeValue]] }
         Abstract                 { catch { set Data(Abstract) [encoding convertfrom utf-8 [[$node firstChild] nodeValue]] } }
         Dimension                { Mapper::DepotWare::WMS::ParseExtent $node; set Data(Cache) 0 }
         DataURL                  { }
         MetadataURL              { Mapper::DepotWare::WMS::ParseMeta $node }
         Style                    { Mapper::DepotWare::WMS::ParseStyle $node }
         Extent                   { Mapper::DepotWare::WMS::ParseExtent $node }
      }
   }
   
   if { $Data(Identifier)!="" } {
      set Data($Data(Title)) [list $URL $Data(Title) $Data(Abstract) $Data(Identifier) $Data(BBox) $Data(Geographic) $Data(SizeX) $Data(SizeY) $Data(Format) $Data(Styles) $Data(Times) $Data(Opaque) $Data(Cache) $Data(Meta)]
      lappend Data(Layers) $Data(Title)
   } else {
      set Data($Data(Title)) [list $URL $Data(Title) $Data(Abstract)]
   }
   
   set branch [Mapper::DepotWare::WMS::Add $Tree $Branch $Data(Title)]

   foreach node $childs {
      Mapper::DepotWare::WMS::ParseLayer $URL $node $Tree $branch False
   }

   set Data(Identifier) ""
   return $Data(Layers)
}

#-------------------------------------------------------------------------------
# Nom      : <Mapper::DepotWare::WMS::ParseMeta>
# Creation : Novembre 2007 - J.P. Gauthier - CMC/CMOE
#
# But      : Decoder du XML pour en extraire le metadata.
#
# Parametres :
#  <Node>    : Node XML
#
# Retour    :
#
# Remarque :
#
#-------------------------------------------------------------------------------

proc Mapper::DepotWare::WMS::ParseMeta { Node } {
   variable Data

   lappend Data(Meta) [[$Node getElementsByTagName OnlineResource] getAttribute xlink:href]
}

#-------------------------------------------------------------------------------
# Nom      : <Mapper::DepotWare::WMS::ParseStyle>
# Creation : Novembre 2007 - J.P. Gauthier - CMC/CMOE
#
# But      : Decoder du XML pour en extraire l'information des styles.
#
# Parametres :
#  <Node>    : Node XML
#
# Retour    :
#
# Remarque :
#
#-------------------------------------------------------------------------------

proc Mapper::DepotWare::WMS::ParseStyle { Node } {
   variable Data

   set name ""
   set title ""
   set abstract ""
   set legend ""

   foreach node [$Node childNodes] {
      switch [$node nodeName] {
         Name      { set name [[$node firstChild] nodeValue] }
         Title     { set title [encoding convertfrom utf-8 [[$node firstChild] nodeValue]] }
         Abstract  { set abstract [encoding convertfrom utf-8 [[$node firstChild] nodeValue]] }
         LegendURL { set legend [[$node getElementsByTagName OnlineResource] getAttribute xlink:href] }

      }
   }
   lappend Data(Styles)  [list $name $title $abstract $legend]
}

#-------------------------------------------------------------------------------
# Nom      : <Mapper::DepotWare::WMS::ParseExtent>
# Creation : Novembre 2007 - J.P. Gauthier - CMC/CMOE
#
# But      : Decoder du XML pour en extraire l'information des dimensions.
#
# Parametres :
#  <Node>    : Node XML
#
# Retour    :
#
# Remarque :
#
#-------------------------------------------------------------------------------

proc Mapper::DepotWare::WMS::ParseExtent { Node } {
   variable Data

   set type [$Node getAttribute name]
   set t0 0
   set t1 0
   set p  ""
   set l  ""

   if { [set node [$Node firstChild]]!="" } {  
      switch $type {
         "time" { set iso [$node nodeValue]
                  ISO8601::Decode $iso t0 t1 p l
                  set Data(Times) $l
               }
         "elevation" { }
      }
   }
}

#-------------------------------------------------------------------------------
# Nom      : <Mapper::DepotWare::WMS::ParseGeographic>
# Creation : Novembre 2007 - J.P. Gauthier - CMC/CMOE
#
# But      : Decoder du XML pour en extraire l'information des limites geographiques.
#
# Parametres :
#  <Node>    : Node XML
#
# Retour    :
#
# Remarque :
#
#-------------------------------------------------------------------------------

proc Mapper::DepotWare::WMS::ParseGeographic { Node } {
   variable Data

   set Data(Geographic) [list\
      [[[$Node getElementsByTagName westBoundLongitude] firstChild] nodeValue]\
      [[[$Node getElementsByTagName northBoundLatitude] firstChild] nodeValue]\
      [[[$Node getElementsByTagName eastBoundLongitude] firstChild] nodeValue]\
      [[[$Node getElementsByTagName southBoundLatitude] firstChild] nodeValue]]
}

#-------------------------------------------------------------------------------
# Nom      : <Mapper::DepotWare::WMS::ParseBoundingBox>
# Creation : Novembre 2007 - J.P. Gauthier - CMC/CMOE
#
# But      : Decoder du XML pour en extraire l'information des limites de la
#            boite de visibilite.
#
# Parametres :
#  <Node>    : Node XML
#
# Retour    :
#
# Remarque :
#
#-------------------------------------------------------------------------------

proc Mapper::DepotWare::WMS::ParseBoundingBox { Node } {
   variable Data

   set x0 [$Node getAttribute minx]
   set y0 [$Node getAttribute miny]
   set x1 [$Node getAttribute maxx]
   set y1 [$Node getAttribute maxy]

   if { [$Node hasAttribute resx] } {
      set rx [$Node getAttribute resx]
      set ry [$Node getAttribute resy]
   } else {
     set rx ""
     set ry ""
   }
   
   set epsg ""
   
   if { [$Node hasAttribute SRS] } {
      set epsg [$Node getAttribute SRS]
   }

   if { [$Node hasAttribute CRS] } {
      set epsg  [$Node getAttribute CRS]
   }

   set Data(BBox)  [list $x0 $y0 $x1 $y1]

   if { $rx!="" && $ry!="" } {
      set Data(SizeX) [expr int(($x1-$x0)/$rx)]
      set Data(SizeY) [expr int(($y1-$y0)/$ry)]
   } else {
      set Data(SizeX) 864000
      set Data(SizeY) 432000
   }
}

proc Mapper::DepotWare::WMS::ParseLatLonBoundingBox { Node } {
   variable Data

   set Data(Geographic) [list [$Node getAttribute minx] [$Node getAttribute maxy] \
      [$Node getAttribute maxx] [$Node getAttribute miny]]
}

#-------------------------------------------------------------------------------
# Nom      : <Mapper::DepotWare::WMS::ReLoad>
# Creation : Juin 2012 - J.P. Gauthier - CMC/CMOE
#
# But      : Relire une bande avec un nouveau style ou nouvelle dimension.
#
# Parametres :
#  <Layer>   : Couche
#  <Style>   : Style
#  <Time>    : Time in seconds
#
# Retour    :
#
# Remarque :
#
#-------------------------------------------------------------------------------

proc Mapper::DepotWare::WMS::ReLoad { Layer { Style "" } { Time "" } } {
   variable Data

   set def  [Mapper::DepotWare::WMS::BuildXMLDef $Layer $Style $Time]

   Mapper::GDAL::ParamsGet $Layer

   gdalband free $Layer
   gdalfile close $Mapper::Data(Id$Layer)
   unset Mapper::Data(Id$Layer)

   set band [Mapper::GDAL::Read $def "" 3]

   #----- Force configuration parameters
   set Mapper::GDAL::Data(Red)   $Mapper::GDAL::Data(Band0$band)
   set Mapper::GDAL::Data(Green) $Mapper::GDAL::Data(Band1$band)
   set Mapper::GDAL::Data(Blue)  $Mapper::GDAL::Data(Band2$band)
   set Mapper::GDAL::Data(Alpha) $Mapper::GDAL::Data(Band3$band)
   set Mapper::GDAL::Data(BandX) $Mapper::GDAL::Data(BandX$band)
   set Mapper::GDAL::Data(BandY) $Mapper::GDAL::Data(BandY$band)
   set Mapper::GDAL::Data(Style) $Style

   Mapper::GDAL::ParamsSet $band False

   #----- Decrease effective resolution (WMS-TMS)
   gdalband define $band -date $Data(Time)

   Mapper::GDAL::Params $band { 0 1 }

   #----- Get legend
   if { [set idx [lsearch -index 0 $Data(Styles) $Style]]!=-1 } {
      Mapper::DepotWare::WMS::GetLegend $band [lindex [lindex $Data(Styles) $idx] end]
   }

   Page::Update $Page::Data(Frame)
}

#-------------------------------------------------------------------------------
# Nom      : <Mapper::DepotWare::GetLegend>
# Creation : Juin 2012 - J.P. Gauthier - CMC/CMOE
#
# But      : Recuperer l'image de la legend et l'afficher.
#
# Parametres :
#  <Band>    : Bande
#  <URL>     : URL de la legende
#
# Retour    :
#
# Remarque :
#
#-------------------------------------------------------------------------------

proc Mapper::DepotWare::WMS::GetLegend { Band URL } {

   if { $URL!="" } {
   
      set URL [string map { " " + } $URL]
      
      #----- get the data from the url and save to tempdir
      set f [open /tmp/lg[pid] w]
      set req [http::geturl $URL -binary True -blocksize 1048580 -channel $f]
      close $f

      #----- Read the legend with gdal
      set bands [gdalfile open LGFILE read /tmp/lg[pid]]
      gdalband read LG $bands True

      #----- Extract tk image from gdal image
      set tag WMSLEGEND[string map { % "" . "" " " "" \' "" \" "" } $Band]

      if { [llength [$Page::Data(Canvas) find withtag $tag]] } {
         $Page::Data(Canvas) itemconfigure $tag -image ""
      } else {
         $Page::Data(Canvas) delete $tag
         Shape::UnBind $Page::Data(Canvas) $tag
      }
      image create photo $tag

      gdalband stats LG -image $tag
      gdalband free LG
      gdalfile close LGFILE

      #----- Get rid of temporary file
      file delete -force /tmp/lg[pid]

      #----- Display legend with bindings
      if { [llength [$Page::Data(Canvas) find withtag $tag]] } {
         $Page::Data(Canvas) itemconfigure $tag -image $tag
      } else {
         $Page::Data(Canvas) create image 5 5 -image $tag -anchor nw -tags $tag
         Shape::BindDestroy $Page::Data(Canvas) $tag
         Shape::BindAllMove $Page::Data(Canvas) $tag
         Shape::BindWidget  $Page::Data(Canvas) $tag
      }
   }
}

#-------------------------------------------------------------------------------
# Nom      : <Mapper::DepotWare::WMS::BuildXMLDef>
# Creation : Novembre 2007 - J.P. Gauthier - CMC/CMOE
#
# But      : Construction du fichier de definition XML necessaire a GDAL pour
#            lire les donnees.
#
# Parametres :
#  <Layer>   : Couche
#  <Style>   : Style
#  <Time>    : Time in seconds
#
# Retour    :
#
# Remarque :
#
#-------------------------------------------------------------------------------

proc Mapper::ToHTML { String } {
   variable Param

   set Param(HTMLCharNumbers) { &#13; \\n &#34; \# &#35; \" &#38; & &#39; ' &#60; < &#62; > &#161; ¡ &#162; ¢ &#163; £ &#164; ¤ &#165; ¥ &#166; ¦ &#167; § &#168; ¨ &#169; ©
      &#170; ª &#171; « &#172; ¬ &#174; ® &#175; ¯ &#176; ° &#177; ± &#178; ² &#179; ³ &#180; ´ &#181; µ &#182; ¶ &#183; · &#184; ¸ &#185; ¹ &#186; º
      &#187; » &#188;  ¼ &#189;  ½ &#190; ¾ &#191; ¿ &#192; À &#193; Á &#194; Â &#195; Ã &#196; Ä &#197; Å &#198; Æ &#199; Ç &#200; È
      &#201; É &#202; Ê &#203; Ë &#204; Ì &#205; Í &#206; Î &#207; Ï &#208; Ð &#209; Ñ &#210; Ò &#211; Ó &#212; Ô &#213; Õ &#214; Ö
      &#215; × &#216; Ø &#217; Ù &#218; Ú &#219; Û &#220; Ü &#221; Ý &#222; Þ &#223; ß &#224; à &#225; á &#226; â &#227; ã &#228; ä
      &#229; å &#230; æ &#231; ç &#232; è &#233; é &#234; ê &#235; ë &#236; ì &#237; í &#238; î &#239; ï &#240; ð &#241; ñ &#242; ò
      &#243; ó &#244; ô &#245; õ &#246; ö &#247; ÷ &#248; ø &#249; ù &#250; ú &#251; û &#252; ü &#253; ý &#254; þ &#255; ÿ }

      return [string map [lreverse $Param(HTMLCharNumbers)] $String]
}

proc Mapper::DepotWare::WMS::BuildXMLDef { Layer { Style "" } { Time "" } } {
   variable Data

   if { [llength $Data($Layer)] < 14 } {
      return
   }
   
   set url          [lindex $Data($Layer) 0 0]
   set user         [lindex $Data($Layer) 0 1]
   set password     [lindex $Data($Layer) 0 2]
   set layer        [lindex $Data($Layer) 3]
   set geog         [lindex $Data($Layer) 5]
   set sizex        [lindex $Data($Layer) 6]
   set sizey        [lindex $Data($Layer) 7]
   set format       [lindex $Data($Layer) 8]
   set Data(Styles) [lindex $Data($Layer) 9]
   set times        [lindex $Data($Layer) 10]
   set opaque       [lindex $Data($Layer) 11]
   set cache        [lindex $Data($Layer) 12]
   set Data(Meta)   [lindex $Data($Layer) 13]

   #----- Check for transparency
   if { $opaque==0 } {
      set bands 4
      set ttag TRUE
      set format "image/png"
    } else {
      set bands 3
      set ttag FALSE
   }

   #----- Process time parameter
   set Data(Time) 0
   if { $Time!="" } {
      set Data(Time) $Time
      set Time "TIME=[ISO8601::FromSeconds $Data(Time)]&"
   } elseif { [llength $times] } {
      set Data(Time) [lindex $times 0]
      set Time "TIME=[ISO8601::FromSeconds $Data(Time)]&"
  }

   if { ![file exists $Mapper::DepotWare::Data(CachePath)] } {
      file mkdir $Mapper::DepotWare::Data(CachePath)
   }


   #----- Build XML request file
   set layer    [string map { " " "%20" } $layer]
   set password [Mapper::ToHTML $password]
   set user     [Mapper::ToHTML $user]

   if { [gdalband is $Layer] } {
      set id $Layer
   } else {
      set id [string map { / "" ? "" : "" } "$Layer - $url"]
   }
   set file $Mapper::DepotWare::Data(CachePath)/$id.xml

   set Data($id) $Data($Layer)

   if { [string first "?" ${url}]==-1 } {
      set url ${url}?
   } else {
      set url $url
   }

   set xml "<GDAL_WMS>\n   <Service name=\"WMS\">\n      <Version>$Data(Version)</Version>\n"
   append xml "      <ServerUrl>${url}${Time}</ServerUrl>\n      <SRS>EPSG:4326</SRS>\n      <ImageFormat>$format</ImageFormat>\n"
   append xml "      <Layers>$layer</Layers>\n      <Transparent>$ttag</Transparent>\n      <Styles>${Style}</Styles>\n   </Service>\n"

   append xml "   <DataWindow>\n      <UpperLeftX>[lindex $geog 0]</UpperLeftX>\n      <UpperLeftY>[lindex $geog 1]</UpperLeftY>\n      <LowerRightX>[lindex $geog 2]</LowerRightX>\n      <LowerRightY>[lindex $geog 3]</LowerRightY>\n      <SizeX>$sizex</SizeX>\n      <SizeY>$sizey</SizeY>\n   </DataWindow>\n"
   append xml "   <Projection>EPSG:4326</Projection>\n   <BandsCount>$bands</BandsCount>\n   <BlockSizeX>$Data(BlockSize)</BlockSizeX>\n   <BlockSizeY>$Data(BlockSize)</BlockSizeY>\n"

   if { $cache && $Mapper::DepotWare::Data(CachePath)!="" } {
      append xml "   <Cache>\n   <Path> [file rootname $file]</Path>\n   <Depth>2</Depth>\n   </Cache>\n"
   }
   
   puts stderr .$url.$user.$password.
   if { $user!="" && $password!="" } {
      append xml "   <UserPwd>$user:$password</UserPwd>\n   <UnsafeSSL>true</UnsafeSSL>\n"
   }
   append xml "   <AdviseRead>true</AdviseRead>\n   <OfflineMode>false</OfflineMode>\n   <ZeroBlockHttpCodes>204,404</ZeroBlockHttpCodes>\n   <ZeroBlockOnServerException>true</ZeroBlockOnServerException>\n</GDAL_WMS>"

   set f [open $file w 0600]
   puts $f $xml
   close $f

   return $file
}
