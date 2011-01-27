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

   set Param(Depots) { { OpenStreetMap TMS OpenStreetMap } { BlueMarble TMS BlueMarble } { MetaCarta TMS MetaCarta } }
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

proc  Mapper::DepotWare::TMS::Select { Tree Branch Path URL } {
   variable Param

   set Layer $Path
   if { ![file exists $Mapper::DepotWare::Data(CachePath)] } {
      file mkdir $Mapper::DepotWare::Data(CachePath)
   }

   set file $Mapper::DepotWare::Data(CachePath)/[string map { / "" ? "" " " "" : "" } $Layer].xml

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

set Param(BlueMarble) "<GDAL_WMS>
    <Service name=\"TileService\">
        <Version>1</Version>
        <ServerUrl>http://s0.tileservice.worldwindcentral.com/getTile?</ServerUrl>
        <Dataset>bmng.topo.bathy.200401</Dataset>
    </Service>
    <DataWindow>
        <UpperLeftX>-180.0</UpperLeftX>
        <UpperLeftY>90.0</UpperLeftY>
        <LowerRightX>180.0</LowerRightX>
        <LowerRightY>-90.0</LowerRightY>
   <SizeX>65536</SizeX>
   <SizeY>32768</SizeY>
   <TileLevel>7</TileLevel>
    </DataWindow>
    <Projection>EPSG:4326</Projection>
    <BlockSizeX>512</BlockSizeX>
    <BlockSizeY>512</BlockSizeY>
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

   set f [open $file w]
   puts $f $Param($Layer)
   close $f

   Mapper::ReadBand $file "" 3
}
