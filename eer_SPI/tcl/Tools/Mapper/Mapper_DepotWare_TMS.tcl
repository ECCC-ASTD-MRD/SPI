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

   set Param(Depots) {
      { OpenStreetMap TMS OpenStreetMap }
      { MapQuest TMS MapQuest}
      { BlueMarble TMS BlueMarble }
      { OneMoon TMS OneMoon }
      { OneEarth TMS OneEarth }
      { MSVirtualEarth TMS MSVirtualEarth }
      { Google-Maps TMS Google-Maps }
      { Google-Satellite TMS Google-Satellite }
      { Google-SatelliteHybrid TMS Google-SatelliteHybrid }
      { Google-Terrain TMS Google-Terrain }
      { Google-TerrainHybrid TMS Google-TerrainHybrid }
      { "ESRI-Server" TMS "ESRI-Server" }}
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

proc  Mapper::DepotWare::TMS::Select { Tree Branch { Select True } } {
   variable Param

   set Layer [$Tree get $Branch path]
   
   if { ![file exists $Mapper::DepotWare::Data(CachePath)] } {
      file mkdir $Mapper::DepotWare::Data(CachePath)
   }

   set file $Mapper::DepotWare::Data(CachePath)/[string map { / "" ? "" " " "" : "" } $Layer].xml

   set Param(OpenStreetMapCMC) "<GDAL_WMS>
   <Service name=\"TMS\">
       <ServerUrl>http://geomet-dev-1.cmc.ec.gc.ca/cgi-bin/mapserv?/\${z}/\${x}/\${y}.png</ServerUrl>
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
   <Cache>
      <Path>[file rootname $file]</Path>
      <Depth>2</Depth>
   </Cache>
</GDAL_WMS>"

   set Param(OpenStreetMap) "<GDAL_WMS>
   <Service name=\"TMS\">
       <ServerUrl>http://tile.openstreetmap.org/\${z}/\${x}/\${y}.png</ServerUrl>
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
   <Cache>
      <Path>[file rootname $file]</Path>
      <Depth>2</Depth>
   </Cache>
</GDAL_WMS>"

   set Param(MapQuest) "<GDAL_WMS>
    <Service name=\"TMS\">
        <ServerUrl>http://otile1.mqcdn.com/tiles/1.0.0/osm/\${z}/\${x}/\${y}.png</ServerUrl>
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
    <Cache>
       <Path>[file rootname $file]</Path>
       <Depth>2</Depth>
    </Cache>
</GDAL_WMS>"


set Param(BlueMarble) "<GDAL_WMS>
   <Service name=\"TMS\">
      <ServerUrl>http://s3.amazonaws.com/com.modestmaps.bluemarble/\$\{z\}-r\$\{y\}-c\$\{x\}.jpg</ServerUrl>
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
   <Cache>
      <Path>[file rootname $file]</Path>
      <Depth>2</Depth>
   </Cache>
</GDAL_WMS>"

set Param(MetaCarta) "<GDAL_WMS>
   <Service name=\"TMS\">
      <ServerUrl>http://labs.metacarta.com/wms-c/Basic.py</ServerUrl>
      <Layer>basic</Layer>
      <Format>png</Format>
   </Service>
   <DataWindow>
      <UpperLeftX>-180.0</UpperLeftX>
      <UpperLeftY>90.0</UpperLeftY>
      <LowerRightX>180.0</LowerRightX>
      <LowerRightY>-90.0</LowerRightY>
      <TileLevel>19</TileLevel>
   <TileCountX>2</TileCountX>
   <TileCountY>1</TileCountY>
   </DataWindow>
   <Projection>EPSG:4326</Projection>
   <BlockSizeX>256</BlockSizeX>
   <BlockSizeY>256</BlockSizeY>
   <BandsCount>3</BandsCount>
   <Cache>
      <Path>[file rootname $file]</Path>
      <Depth>2</Depth>
   </Cache>
</GDAL_WMS>"

set Param(OneMoon) "<GDAL_WMS>
   <Service name=\"TiledWMS\">
   <ServerUrl>http://onmoon.jpl.nasa.gov/wms.cgi?</ServerUrl>
   <TiledGroupName>Clementine</TiledGroupName>
   </Service>
   <Cache>
      <Path>[file rootname $file]</Path>
      <Depth>2</Depth>
   </Cache>
</GDAL_WMS>"

set Param(OneEarth) "<GDAL_WMS>
   <Service name=\"TiledWMS\">
      <ServerUrl>http://onearth.jpl.nasa.gov/wms.cgi?</ServerUrl>
      <TiledGroupName>Daily Planet</TiledGroupName>
      <Change key=\"\$\{time\}\">2009-10-12</Change>
   </Service>
   <Cache>
      <Path>[file rootname $file]</Path>
      <Depth>2</Depth>
   </Cache>
</GDAL_WMS>"

set Param(OneEarthSRTM) "<GDAL_WMS>
   <Service name=\"TiledWMS\">
      <ServerUrl>http://onearth.jpl.nasa.gov/wms.cgi?</ServerUrl>
      <TiledGroupName>Global SRTM Elevation</TiledGroupName>
   </Service>
   <Cache>
      <Path>[file rootname $file]</Path>
      <Depth>2</Depth>
   </Cache>
</GDAL_WMS>"

set Param(MSVirtualEarth) "<GDAL_WMS>
   <Service name=\"VirtualEarth\">
      <ServerUrl>http://a\$\{server_num\}.ortho.tiles.virtualearth.net/tiles/a\$\{quadkey\}.jpeg?g=90</ServerUrl>
   </Service>
   <MaxConnections>4</MaxConnections>
   <Cache>
      <Path>[file rootname $file]</Path>
      <Depth>2</Depth>
   </Cache>
</GDAL_WMS>"

set Param(Google-Terrain) "<GDAL_WMS>
   <!--
   Data is subject to term of use detailed at http://code.google.com/intl/nl/apis/maps/terms.html and http://www.google.com/intl/en_ALL/help/terms_maps.html
   -->
   <Service name=\"TMS\">
      <ServerUrl>http://mt.google.com/vt/lyrs=t&amp;x=\$\{x\}&amp;y=\$\{y\}&amp;z=\$\{z\}</ServerUrl>
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
   <Cache>
      <Path>[file rootname $file]</Path>
      <Depth>2</Depth>
   </Cache>
</GDAL_WMS>"

set Param(Google-TerrainHybrid) "<GDAL_WMS>
   <!--
   Data is subject to term of use detailed at http://code.google.com/intl/nl/apis/maps/terms.html and http://www.google.com/intl/en_ALL/help/terms_maps.html
   -->
   <Service name=\"TMS\">
      <ServerUrl>http://mt.google.com/vt/lyrs=p&amp;x=\$\{x\}&amp;y=\$\{y\}&amp;z=\$\{z\}</ServerUrl>
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
   <Cache>
      <Path>[file rootname $file]</Path>
      <Depth>2</Depth>
   </Cache>
</GDAL_WMS>"

set Param(Google-Maps) "<GDAL_WMS>
   <!--
   Data is subject to term of use detailed at http://code.google.com/intl/nl/apis/maps/terms.html and http://www.google.com/intl/en_ALL/help/terms_maps.html
   -->
   <Service name=\"TMS\">
      <ServerUrl>http://mt.google.com/vt/lyrs=m&amp;x=\$\{x\}&amp;y=\$\{y\}&amp;z=\$\{z\}</ServerUrl>
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
    <Cache>
       <Path>[file rootname $file]</Path>
       <Depth>2</Depth>
    </Cache>
</GDAL_WMS>"

set Param(Google-Satellite) "<GDAL_WMS>
   <!--
   Data is subject to term of use detailed at http://code.google.com/intl/nl/apis/maps/terms.html and http://www.google.com/intl/en_ALL/help/terms_maps.html
   -->
   <Service name=\"TMS\">
      <ServerUrl>http://mt.google.com/vt/lyrs=s&amp;x=\$\{x\}&amp;y=\$\{y\}&amp;z=\$\{z\}</ServerUrl>
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
   <Cache>
      <Path>[file rootname $file]</Path>
      <Depth>2</Depth>
   </Cache>
</GDAL_WMS>"

set Param(Google-SatelliteHybrid) "<GDAL_WMS>
   <!--
   Data is subject to term of use detailed at http://code.google.com/intl/nl/apis/maps/terms.html and http://www.google.com/intl/en_ALL/help/terms_maps.html
   -->
   <Service name=\"TMS\">
      <ServerUrl>http://mt.google.com/vt/lyrs=y&amp;x=\$\{x\}&amp;y=\$\{y\}&amp;z=\$\{z\}</ServerUrl>
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
   <Cache>
      <Path>[file rootname $file]</Path>
      <Depth>2</Depth>
   </Cache>
</GDAL_WMS>"

set Param(ESRI-Server) "<GDAL_WMS>
   <Service name=\"TMS\">
   <ServerUrl>http://services.arcgisonline.com/ArcGIS/rest/services/World_Street_Map/MapServer/tile/\$\{z\}/\$\{y\}/\$\{x\}</ServerUrl>
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
   <Cache>
      <Path>[file rootname $file]</Path>
      <Depth>2</Depth>
   </Cache>
</GDAL_WMS>"

   set f [open $file w]
   puts $f $Param($Layer)
   close $f

   set band [Mapper::ReadBand $file "" 3]

   #----- Decrease effective resolution (WMS-TMS)
   gdalband configure $band -texres 3
}
