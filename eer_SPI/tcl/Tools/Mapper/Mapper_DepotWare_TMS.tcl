#===============================================================================
# Environnement Canada
# Centre Meteorologique Canadian
# 2100 Trans-Canadienne
# Dorval, Quebec
#
# Projet   : Librairie de "Tools" Tk.
# Fichier  : Mapper_DepotWare_TMS.tcl
# Creation : Janvier 2010 - J.P.Gauthier - CMC/CMOE
#
# Description:
#    Fontions de manipulation et de gestion des depots de donnees georeference.
#
# Remarques :
#
#===============================================================================

namespace eval Mapper::DepotWare::TMS {
   variable Param
   variable Lbl
   
   set Lbl(XML) { "Description XML" "XML description" }

   set Param(Depots) {
   {       OpenStreetMap_EC TMS { <GDAL_WMS>
   <Service name="TMS">
       <ServerUrl>http://osm1.cmc.ec.gc.ca/osm/${z}/${x}/${y}.png</ServerUrl>
   </Service>
   <DataWindow>
      <UpperLeftX>-20037508.34</UpperLeftX>
      <UpperLeftY>20037508.34</UpperLeftY>
      <LowerRightX>20037508.34</LowerRightX>
      <LowerRightY>-20037508.34</LowerRightY>
      <TileLevel>19</TileLevel>
      <TileCountX>1</TileCountX>
      <TileCountY>1</TileCountY>
      <YOrigin>top</YOrigin>
   </DataWindow>
   <Projection>EPSG:900913</Projection>
   <BlockSizeX>256</BlockSizeX>
   <BlockSizeY>256</BlockSizeY>
   <BandsCount>3</BandsCount>
</GDAL_WMS>} }

      { OpenStreetMap TMS { <GDAL_WMS>
   <Service name="TMS">
       <ServerUrl>http://tile.openstreetmap.org/${z}/${x}/${y}.png</ServerUrl>
   </Service>
   <DataWindow>
      <UpperLeftX>-20037508.34</UpperLeftX>
      <UpperLeftY>20037508.34</UpperLeftY>
      <LowerRightX>20037508.34</LowerRightX>
      <LowerRightY>-20037508.34</LowerRightY>
      <TileLevel>19</TileLevel>
      <TileCountX>1</TileCountX>
      <TileCountY>1</TileCountY>
      <YOrigin>top</YOrigin>
   </DataWindow>
   <Projection>EPSG:900913</Projection>
   <BlockSizeX>256</BlockSizeX>
   <BlockSizeY>256</BlockSizeY>
   <BandsCount>3</BandsCount>
</GDAL_WMS>} }

      { BlueMarble TMS { <GDAL_WMS>
   <Service name="TMS">
      <ServerUrl>http://s3.amazonaws.com/com.modestmaps.bluemarble/${z}-r${y}-c${x}.jpg</ServerUrl>
   </Service>
   <DataWindow>
      <UpperLeftX>-20037508.34</UpperLeftX>
      <UpperLeftY>20037508.34</UpperLeftY>
      <LowerRightX>20037508.34</LowerRightX>
      <LowerRightY>-20037508.34</LowerRightY>
      <TileLevel>9</TileLevel>
      <TileCountX>1</TileCountX>
      <TileCountY>1</TileCountY>
      <YOrigin>top</YOrigin>
   </DataWindow>
   <Projection>EPSG:900913</Projection>
   <BlockSizeX>256</BlockSizeX>
   <BlockSizeY>256</BlockSizeY>
   <BandsCount>3</BandsCount>
</GDAL_WMS> } }
      { MSVirtualEarth TMS { <GDAL_WMS>
   <Service name="VirtualEarth">
      <ServerUrl>http://a${server_num}.ortho.tiles.virtualearth.net/tiles/a${quadkey}.jpeg?g=90</ServerUrl>
   </Service>
   <MaxConnections>4</MaxConnections>
</GDAL_WMS> } }

      { Google-Maps TMS { <GDAL_WMS>
   <!--
   Data is subject to term of use detailed at http://code.google.com/intl/nl/apis/maps/terms.html and http://www.google.com/intl/en_ALL/help/terms_maps.html
   -->
   <Service name="TMS">
      <ServerUrl>http://mt.google.com/vt/lyrs=m&amp;x=${x}&amp;y=${y}&amp;z=${z}</ServerUrl>
   </Service>
   <DataWindow>
      <UpperLeftX>-20037508.34</UpperLeftX>
      <UpperLeftY>20037508.34</UpperLeftY>
      <LowerRightX>20037508.34</LowerRightX>
      <LowerRightY>-20037508.34</LowerRightY>
      <TileLevel>20</TileLevel>
      <TileCountX>1</TileCountX>
      <TileCountY>1</TileCountY>
      <YOrigin>top</YOrigin>
   </DataWindow>
   <Projection>EPSG:900913</Projection>
   <BlockSizeX>256</BlockSizeX>
   <BlockSizeY>256</BlockSizeY>
   <BandsCount>3</BandsCount>
   <MaxConnections>5</MaxConnections>
</GDAL_WMS> } }

      { Google-Satellite TMS { <GDAL_WMS>
   <!--
   Data is subject to term of use detailed at http://code.google.com/intl/nl/apis/maps/terms.html and http://www.google.com/intl/en_ALL/help/terms_maps.html
   -->
   <Service name="TMS">
      <ServerUrl>http://mt.google.com/vt/lyrs=s&amp;x=${x}&amp;y=${y}&amp;z=${z}</ServerUrl>
   </Service>
   <DataWindow>
      <UpperLeftX>-20037508.34</UpperLeftX>
      <UpperLeftY>20037508.34</UpperLeftY>
      <LowerRightX>20037508.34</LowerRightX>
      <LowerRightY>-20037508.34</LowerRightY>
      <TileLevel>20</TileLevel>
      <TileCountX>1</TileCountX>
      <TileCountY>1</TileCountY>
      <YOrigin>top</YOrigin>
   </DataWindow>
   <Projection>EPSG:900913</Projection>
   <BlockSizeX>256</BlockSizeX>
   <BlockSizeY>256</BlockSizeY>
   <BandsCount>3</BandsCount>
   <MaxConnections>5</MaxConnections>
</GDAL_WMS> } }

      { Google-SatelliteHybrid TMS { <GDAL_WMS>
   <!--
   Data is subject to term of use detailed at http://code.google.com/intl/nl/apis/maps/terms.html and http://www.google.com/intl/en_ALL/help/terms_maps.html
   -->
   <Service name="TMS">
      <ServerUrl>http://mt.google.com/vt/lyrs=y&amp;x=${x}&amp;y=${y}&amp;z=${z}</ServerUrl>
   </Service>
   <DataWindow>
      <UpperLeftX>-20037508.34</UpperLeftX>
      <UpperLeftY>20037508.34</UpperLeftY>
      <LowerRightX>20037508.34</LowerRightX>
      <LowerRightY>-20037508.34</LowerRightY>
      <TileLevel>20</TileLevel>
      <TileCountX>1</TileCountX>
      <TileCountY>1</TileCountY>
      <YOrigin>top</YOrigin>
   </DataWindow>
   <Projection>EPSG:900913</Projection>
   <BlockSizeX>256</BlockSizeX>
   <BlockSizeY>256</BlockSizeY>
   <BandsCount>3</BandsCount>
   <MaxConnections>5</MaxConnections>
</GDAL_WMS> } }

      { Google-Terrain TMS { <GDAL_WMS>
   <!--
   Data is subject to term of use detailed at http://code.google.com/intl/nl/apis/maps/terms.html and http://www.google.com/intl/en_ALL/help/terms_maps.html
   -->
   <Service name="TMS">
      <ServerUrl>http://mt.google.com/vt/lyrs=t&amp;x=${x}&amp;y=${y}&amp;z=${z}</ServerUrl>
   </Service>
   <DataWindow>
      <UpperLeftX>-20037508.34</UpperLeftX>
      <UpperLeftY>20037508.34</UpperLeftY>
      <LowerRightX>20037508.34</LowerRightX>
      <LowerRightY>-20037508.34</LowerRightY>
      <TileLevel>20</TileLevel>
      <TileCountX>1</TileCountX>
      <TileCountY>1</TileCountY>
      <YOrigin>top</YOrigin>
   </DataWindow>
   <Projection>EPSG:900913</Projection>
   <BlockSizeX>256</BlockSizeX>
   <BlockSizeY>256</BlockSizeY>
   <BandsCount>3</BandsCount>
   <MaxConnections>5</MaxConnections>
</GDAL_WMS> } }

      { Google-TerrainHybrid TMS { <GDAL_WMS>
   <!--
   Data is subject to term of use detailed at http://code.google.com/intl/nl/apis/maps/terms.html and http://www.google.com/intl/en_ALL/help/terms_maps.html
   -->
   <Service name="TMS">
      <ServerUrl>http://mt.google.com/vt/lyrs=p&amp;x=${x}&amp;y=${y}&amp;z=${z}</ServerUrl>
   </Service>
   <DataWindow>
      <UpperLeftX>-20037508.34</UpperLeftX>
      <UpperLeftY>20037508.34</UpperLeftY>
      <LowerRightX>20037508.34</LowerRightX>
      <LowerRightY>-20037508.34</LowerRightY>
      <TileLevel>20</TileLevel>
      <TileCountX>1</TileCountX>
      <TileCountY>1</TileCountY>
      <YOrigin>top</YOrigin>
   </DataWindow>
   <Projection>EPSG:900913</Projection>
   <BlockSizeX>256</BlockSizeX>
   <BlockSizeY>256</BlockSizeY>
   <BandsCount>3</BandsCount>
   <MaxConnections>5</MaxConnections>
</GDAL_WMS> } }

      { ESRI-WorldStreet TMS { <GDAL_WMS>
   <Service name="TMS">
   <ServerUrl>http://services.arcgisonline.com/ArcGIS/rest/services/World_Street_Map/MapServer/tile/${z}/${y}/${x}</ServerUrl>
   </Service>
   <DataWindow>
      <UpperLeftX>-20037508.34</UpperLeftX>
      <UpperLeftY>20037508.34</UpperLeftY>
      <LowerRightX>20037508.34</LowerRightX>
      <LowerRightY>-20037508.34</LowerRightY>
      <TileLevel>17</TileLevel>
      <TileCountX>1</TileCountX>
      <TileCountY>1</TileCountY>
      <YOrigin>top</YOrigin>
   </DataWindow>
   <Projection>EPSG:900913</Projection>
   <BlockSizeX>256</BlockSizeX>
   <BlockSizeY>256</BlockSizeY>
   <BandsCount>3</BandsCount>
   <MaxConnections>10</MaxConnections>
</GDAL_WMS> } }   

   { ESRI-WorldTopo TMS { <GDAL_WMS>
   <Service name="TMS">
   <ServerUrl>http://services.arcgisonline.com/ArcGIS/rest/services/World_Topo_Map/MapServer/tile/${z}/${y}/${x}</ServerUrl>
   </Service>
   <DataWindow>
      <UpperLeftX>-20037508.34</UpperLeftX>
      <UpperLeftY>20037508.34</UpperLeftY>
      <LowerRightX>20037508.34</LowerRightX>
      <LowerRightY>-20037508.34</LowerRightY>
      <TileLevel>17</TileLevel>
      <TileCountX>1</TileCountX>
      <TileCountY>1</TileCountY>
      <YOrigin>top</YOrigin>
   </DataWindow>
   <Projection>EPSG:900913</Projection>
   <BlockSizeX>256</BlockSizeX>
   <BlockSizeY>256</BlockSizeY>
   <BandsCount>3</BandsCount>
   <MaxConnections>10</MaxConnections>
</GDAL_WMS> } }
   }
}

#-------------------------------------------------------------------------------
# Nom      : <Mapper::DepotWare::TMS::Default>
# Creation : Octobre 2010 - J.P. Gauthier - CMC/CMOE
#
# But      : Insere les services TMS par défaut.
#
# Parametres :
#  <Frame>   : Fenetre parent
#
# Retour    :
#
# Remarque :
#
#-------------------------------------------------------------------------------

proc Mapper::DepotWare::TMS::Default { } {
   variable Param
   
   if { [lsearch -exact -index 0 $::Mapper::DepotWare::Data(Depots) [lindex [lindex $Param(Depots) 0] 0]]==-1 } {
      foreach depot $Mapper::DepotWare::TMS::Param(Depots) {    
         lappend ::Mapper::DepotWare::Data(Depots) $depot
      }
   }
}

#-------------------------------------------------------------------------------
# Nom      : <Mapper::DepotWare::TMS::Params>
# Creation : Decembre 2008 - J.P. Gauthier - CMC/CMOE
#
# But      : Fenetre des parametres du type de DB Tiled Mapping Service (WMS).
#
# Parametres :
#  <Frame>   : Fenetre parent
#
# Retour    :
#
# Remarque :
#
#-------------------------------------------------------------------------------

proc Mapper::DepotWare::TMS::Params { Frame } {
   global GDefs
   variable Data
   variable Lbl

   frame $Frame.xml
      label $Frame.xml.lbl -anchor w -text [lindex $Lbl(XML) $GDefs(Lang)] -width 15
      text $Frame.xml.ent -width 1 -height 5 -bd 1 -bg $GDefs(ColorLight) -relief sunken
      pack $Frame.xml.lbl -side left
      pack $Frame.xml.ent -side left -fill both -expand True
   pack $Frame.xml -fill both -expand True
}

#-------------------------------------------------------------------------------
# Nom      : <Mapper::DepotWare::TMS::Request>
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

proc Mapper::DepotWare::TMS::Request { } {
   variable Data

   return [.mapperdepot.params.xml.ent get 0.0 end]
}

#-------------------------------------------------------------------------------
# Nom      : <Mapper::DepotWare::TMS::Select>
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

proc  Mapper::DepotWare::TMS::SelectLayer { Layer } {

   Mapper::DepotWare::TMS::Select "" "" $Layer ""
   Mapper::UpdateData $Page::Data(Frame)
}

proc  Mapper::DepotWare::TMS::Select { Tree Branch { Select True } { SQL "" } } {
   variable Param

   set name [$Tree get $Branch name]
   set xml  [$Tree get $Branch path]
   
   if { ![file exists $Mapper::DepotWare::Data(CachePath)] } {
      file mkdir $Mapper::DepotWare::Data(CachePath)
   }

   set file $Mapper::DepotWare::Data(CachePath)/[string map { / "" ? "" " " "" : "" } $name].xml
   set f [open $file w]

   #----- Insert cache path
   set index [string first "</GDAL_WMS>" $xml]
   puts $f [string trim [string range $xml 0 [expr $index-1]]]
   puts $f "   <Cache>
      <Path>[file rootname $file]</Path>
      <Depth>2</Depth>
   </Cache>
</GDAL_WMS>"
   
   close $f

   set band [Mapper::GDAL::Read $file "" 3]

   #----- Decrease effective resolution (WMS-TMS)
   gdalband configure $band -texres 2
}

#-------------------------------------------------------------------------------
# Nom      : <Mapper::DepotWare::TMS::Load>
# Creation : Janvier 2014 - J.P. Gauthier - CMC/CMOE
#
# But      : Fonction usager pour afficher des donnees TMS.
#
# Parametres :
#  <Name>    : Nom de la donnes TMS
#  <Res>     : Facteur de resolution (Default:2)
#
# Retour    :
#
# Remarque :
#
#-------------------------------------------------------------------------------

proc  Mapper::DepotWare::TMS::Load { Name { Res 2 } } {
   variable Param
   variable Data

   if { [set idx [lsearch -exact -index 0 $Mapper::DepotWare::Data(Depots) $Name]]!=-1 } {
      set xml [lindex [lindex $Mapper::DepotWare::Data(Depots) $idx] 2]
   } else {
      return
   }
   
   if { ![file exists $Mapper::DepotWare::Data(CachePath)] } {
      file mkdir $Mapper::DepotWare::Data(CachePath)
   }

   set file $Mapper::DepotWare::Data(CachePath)/[string map { / "" ? "" " " "" : "" } $Name].xml
   set f [open $file w]

   #----- Insert cache path
   set index [string first "</GDAL_WMS>" $xml]
   puts $f [string trim [string range $xml 0 [expr $index-1]]]
   puts $f "   <Cache>
      <Path>[file rootname $file]</Path>
      <Depth>2</Depth>
   </Cache>
</GDAL_WMS>"
   
   close $f

   set band [Mapper::GDAL::Read $file "" 3]

   #----- Decrease effective resolution (WMS-TMS)
   gdalband configure $band -texres $Res

   Mapper::UpdateData $Page::Data(Frame)
   Viewport::UpdateData $Page::Data(Frame)    
   
   return $band
}
