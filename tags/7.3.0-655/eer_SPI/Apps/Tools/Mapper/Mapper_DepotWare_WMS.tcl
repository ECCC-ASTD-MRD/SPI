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

   set Lbl(URL) { "Addresse URL" "URL Address" }

   set Msg(Request) { "Problème dans la requète de capacitées WMS (GetCapabilities)" "Problem requesting capabilities WMS (GetCapabilities)" }

   set Data(URL) ""
   set Data(BlockSize) 512
   set Data(Layers)  {}

   set Data(SizeX)        0
   set Data(SizeY)        0
   set Data(Version)      ""
   set Data(Identifier)   ""
   set Data(Format)       ""
   set Data(Title)        ""
   set Data(BBox)         ""
   set Data(Styles)       ""
   set Data(Geographic)   ""
   set Data(Opaque)       1
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
   pack $Frame.path -fill x -expand True
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

proc  Mapper::DepotWare::WMS::Select { Tree Branch Path URL } {
   global GDefs
   variable Msg

   if { $URL=="URL" }  {
      if { [string first "?" ${Path}]==-1 } {
         set req [http::geturl "${Path}?&SERVICE=WMS&REQUEST=GetCapabilities"]
      } else {
         set req [http::geturl "${Path}&SERVICE=WMS&REQUEST=GetCapabilities"]
      }

      if { [catch { set doc [dom::parse [http::data $req]] } ] } {
         Dialog::CreateErrorListing . [lindex $Msg(Request) $GDefs(Lang)] [http::data $req] $GDefs(Lang)
         return
      }

      set Data(Version) 1.1.1
      set Data(Format) "image/gif"
      set getmap [lindex [set [dom::document getElementsByTagName $doc GetMap]] 0]
      foreach node [set [dom::document getElementsByTagName $getmap Format]] {
         set Data(Format) [dom::node cget  [dom::node children $node] -nodeValue]
         if { $Data(Format)=="image/png" || $Data(Format)=="image/jpeg" } {
            break
         }
      }

      set layer [lindex [set [dom::document getElementsByTagName $doc Layer]] 0]
      foreach layer [Mapper::DepotWare::WMS::ParseLayer $Path $layer] {
         Mapper::DepotWare::WMS::Add $Tree $Branch $layer
      }
      dom::destroy $doc
   } else {
      set def [Mapper::DepotWare::WMS::BuildXMLDef $Path]
      if { [lsearch -exact $Viewport::Data(Data$Page::Data(Frame)) $def]==-1 } {
         Mapper::ReadBand $def "" 3
      }
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

   return $Data(URL)
}

#-------------------------------------------------------------------------------
# Nom      : <Mapper::DepotWare::WMS::Add>
# Creation : Novembre 2007 - J.P. Gauthier - CMC/CMOE
#
# But      : Ajouter une branche pour une couche WMS.
#
# Parametres :
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
   $Tree set $branch name ""
   $Tree set $branch path $Layer
   $Tree set $branch type WMS
   $Tree set $branch width  -1
   $Tree set $branch height -1

   set bbox  [lindex $Data($Layer) 3]
   $Tree set $branch 00 [list [lindex $bbox 1] [lindex $bbox 0]]
   $Tree set $branch 01 [list [lindex $bbox 3] [lindex $bbox 0]]
   $Tree set $branch 10 [list [lindex $bbox 1] [lindex $bbox 2]]
   $Tree set $branch 11 [list [lindex $bbox 3] [lindex $bbox 2]]
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
#  <First>   : Premiere couche ?
#
# Retour    :
#
# Remarque :
#   - Certaines couches cont imbriquees alors cette procedure est recursive
#
#-------------------------------------------------------------------------------

proc Mapper::DepotWare::WMS::ParseLayer { URL Node { First True } } {
   variable Data

   if { $First } {
      set Data(Layers) {}
      set Data(SizeX) 864000
      set Data(SizeY) 432000
      set Data(Width) 512
      set Data(Height) 512
      set Data(Cache)  1
   }
   set Data(Style)  {}

   foreach node [set [dom::node configure $Node -childNodes]] {
      switch [dom::node configure $node  -nodeName] {
         Layer                    { set Data(Opaque) [dom::element getAttribute $node opaque]
                                    Mapper::DepotWare::WMS::ParseLayer $URL $node False }
         EX_GeographicBoundingBox { Mapper::DepotWare::WMS::ParseGeographic $node }
         LatLonBoundingBox        { Mapper::DepotWare::WMS::ParseLatLonBoundingBox $node }
         BoundingBox              { Mapper::DepotWare::WMS::ParseBoundingBox $node }
         Name                     { set Data(Identifier)  [dom::node cget [dom::node children $node] -nodeValue] }
         Title                    { set Data(Title) [dom::node cget [dom::node children $node] -nodeValue] }
         Dimension                { set Data(Cache) 0 }
         DataURL                  { }
         Style                    { Mapper::DepotWare::WMS::ParseStyle $node }
      }
   }

   if { $Data(Identifier)!="" } {
      set Data($Data(Title)) [list $URL $Data(Identifier) $Data(BBox) $Data(Geographic) $Data(SizeX) $Data(SizeY) $Data(Format) $Data(Style) $Data(Opaque) $Data(Cache)]
      lappend Data(Layers) $Data(Title)
   }
   set Data(Identifier) ""
   return $Data(Layers)
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

   lappend Data(Style) [dom::node cget [lindex [dom::node children $Node] 0] -nodeValue]
}

#-------------------------------------------------------------------------------
# Nom      : <Mapper::DepotWare::WMS::ParseDimension>
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

proc Mapper::DepotWare::WMS::ParseDimension { Node } {
   variable Data

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
      [dom::node cget [dom::node children [set [dom::document getElementsByTagName $Node westBoundLongitude]]] -nodeValue]\
      [dom::node cget [dom::node children [set [dom::document getElementsByTagName $Node northBoundLatitude]]] -nodeValue]\
      [dom::node cget [dom::node children [set [dom::document getElementsByTagName $Node eastBoundLongitude]]] -nodeValue]\
      [dom::node cget [dom::node children [set [dom::document getElementsByTagName $Node southBoundLatitude]]] -nodeValue]]
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

   set x0 [dom::element getAttribute $Node minx]
   set y0 [dom::element getAttribute $Node miny]
   set x1 [dom::element getAttribute $Node maxx]
   set y1 [dom::element getAttribute $Node maxy]
   set epsg [dom::element getAttribute $Node SRS]
   set crs  [dom::element getAttribute $Node CRS]

   set Data(BBox)  [list $x0 $y0 $x1 $y1]

   if { $epsg=="EPSG:4326" || $epsg=="EPSG:4269" || $crs=="CRS:84" } {
      set Data(SizeX) 864000
      set Data(SizeY) 432000
   } else {
      set Data(SizeX) [expr $x1-$x0]
      set Data(SizeY) [expr $y1-$y0]
   }
}

proc Mapper::DepotWare::WMS::ParseLatLonBoundingBox { Node } {
   variable Data

   set Data(Geographic) [list [dom::element getAttribute $Node minx] [dom::element getAttribute $Node maxy] \
      [dom::element getAttribute $Node maxx] [dom::element getAttribute $Node miny]]
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
#
# Retour    :
#
# Remarque :
#
#-------------------------------------------------------------------------------

proc Mapper::DepotWare::WMS::BuildXMLDef { Layer } {
   variable Data

   set url    [lindex $Data($Layer) 0]
   set layer  [lindex $Data($Layer) 1]
   set geog   [lindex $Data($Layer) 3]
   set sizex  [lindex $Data($Layer) 4]
   set sizey  [lindex $Data($Layer) 5]
   set format [lindex $Data($Layer) 6]
   set style  [lindex $Data($Layer) 7]
   set opaque [lindex $Data($Layer) 8]
   set cache  [lindex $Data($Layer) 9]

   if { $opaque==0 } {
      set bands 4
      set ttag TRANSPARENT=TRUE
      set format "image/png"
    } else {
      set bands 3
      set ttag TRANSPARENT=FALSE
   }

   if { ![file exists $Mapper::DepotWare::Data(CachePath)] } {
      file mkdir $Mapper::DepotWare::Data(CachePath)
   }

   set layer [string map { " " "%20" } $layer]
   set file $Mapper::DepotWare::Data(CachePath)/[string map { / "" ? "" " " "" : "" } $url$layer].xml
   if { [string first "?" ${url}]==-1 } {
      set url ${url}?
   } else {
      set url $url
   }

   set xml "<GDAL_WMS>\n   <Service name=\"WMS\">\n      <Version>$Data(Version)</Version>\n"
   append xml "      <ServerUrl>${url}${ttag}&</ServerUrl>\n      <SRS>EPSG:4326</SRS>\n      <ImageFormat>$format</ImageFormat>\n"
   append xml "      <Layers>$layer</Layers>\n      <Styles></Styles>\n   </Service>\n"

   append xml "   <DataWindow>\n      <UpperLeftX>[lindex $geog 0]</UpperLeftX>\n      <UpperLeftY>[lindex $geog 1]</UpperLeftY>\n      <LowerRightX>[lindex $geog 2]</LowerRightX>\n      <LowerRightY>[lindex $geog 3]</LowerRightY>\n      <SizeX>$sizex</SizeX>\n      <SizeY>$sizey</SizeY>\n   </DataWindow>\n"
   append xml "   <Projection>EPSG:4326</Projection>\n   <BandsCount>$bands</BandsCount>\n   <BlockSizeX>$Data(BlockSize)</BlockSizeX>\n   <BlockSizeY>$Data(BlockSize)</BlockSizeY>\n"

   if { $cache && $Mapper::DepotWare::Data(CachePath)!="" } {
      append xml "<Cache>\n   <Path> [file rootname $file]</Path>\n   <Depth>2</Depth>\n   </Cache>\n"
   }
   append xml "</GDAL_WMS>"

   set f [open $file w]
   puts $f $xml
   close $f

   return $file
}

