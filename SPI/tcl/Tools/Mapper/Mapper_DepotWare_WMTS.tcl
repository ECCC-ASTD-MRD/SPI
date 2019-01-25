#===============================================================================
# Environnement Canada
# Centre Meteorologique Canadian
# 2100 Trans-Canadienne
# Dorval, Quebec
#
# Projet   : Librairie de "Tools" Tk.
# Fichier  : Mapper_DepotWare_WMTS.tcl
# Creation : Janvier 2010 - J.P.Gauthier - CMC/CMOE
#
# Description:
#    Fontions de manipulation et de gestion des depots de donnees georeference.
#
# Remarques :
#
#===============================================================================

namespace eval Mapper::DepotWare::WMTS {
   variable Param
   variable Lbl
   
   set Lbl(XML) { "Description XML" "XML description" }

   set Param(Depots) {
   {                                   Test_WMTS WMTS { <GDAL_WMTS>
  <GetCapabilitiesUrl>http://maps.wien.gv.at/wmts/1.0.0/WMTSCapabilities.xml</GetCapabilitiesUrl>
  <Layer>lb</Layer>
  <Style>farbe</Style>
  <TileMatrixSet>google3857</TileMatrixSet>
  <DataWindow>
    <UpperLeftX>1800035.8827671</UpperLeftX>
    <UpperLeftY>6161931.622311067</UpperLeftY>
    <LowerRightX>1845677.148953537</LowerRightX>
    <LowerRightY>6123507.385072636</LowerRightY>
  </DataWindow>
  <BandsCount>4</BandsCount>
  <UnsafeSSL>true</UnsafeSSL>
  <ZeroBlockHttpCodes>404</ZeroBlockHttpCodes>
  <ZeroBlockOnServerException>true</ZeroBlockOnServerException>
</GDAL_WMTS>} }
   {                                   MapBox_Streets WMTS { <GDAL_WMTS>
  <GetCapabilitiesUrl>https://api.mapbox.com/styles/v1/mapbox/streets-v9/wmts?access_token=pk.eyJ1IjoiZ2F1dGhpZXJqcCIsImEiOiJjamFzYjdwNjE0bzM3MzNwbHo2aTNrNHUwIn0.MhhcnWJehKKV9QuVVermvw</GetCapabilitiesUrl>
  <Layer>streets-v9</Layer>
  <Style>default</Style>
  <TileMatrixSet>GoogleMapsCompatible</TileMatrixSet>
  <DataWindow>
      <UpperLeftX>-20037508.34</UpperLeftX>
      <UpperLeftY>20037508.34</UpperLeftY>
      <LowerRightX>20037508.34</LowerRightX>
      <LowerRightY>-20037508.34</LowerRightY>
  </DataWindow>
  <BandsCount>4</BandsCount>
  <UnsafeSSL>true</UnsafeSSL>
  <ZeroBlockHttpCodes>404</ZeroBlockHttpCodes>
  <ZeroBlockOnServerException>true</ZeroBlockOnServerException>
</GDAL_WMTS>} }
   {                                   MapBox_OutDoor WMTS { <GDAL_WMTS>
  <GetCapabilitiesUrl>https://api.mapbox.com/styles/v1/mapbox/outdoors-v9/wmts?access_token=pk.eyJ1IjoiZ2F1dGhpZXJqcCIsImEiOiJjamFzYjdwNjE0bzM3MzNwbHo2aTNrNHUwIn0.MhhcnWJehKKV9QuVVermvw</GetCapabilitiesUrl>
  <Layer>outdoors-v9</Layer>
  <Style>default</Style>
  <TileMatrixSet>GoogleMapsCompatible</TileMatrixSet>
  <DataWindow>
      <UpperLeftX>-20037508.34</UpperLeftX>
      <UpperLeftY>20037508.34</UpperLeftY>
      <LowerRightX>20037508.34</LowerRightX>
      <LowerRightY>-20037508.34</LowerRightY>
  </DataWindow>
  <BandsCount>4</BandsCount>
  <UnsafeSSL>true</UnsafeSSL>
  <ZeroBlockHttpCodes>404</ZeroBlockHttpCodes>
  <ZeroBlockOnServerException>true</ZeroBlockOnServerException>
</GDAL_WMTS>} }
   {                                   MapBox_Light WMTS { <GDAL_WMTS>
  <GetCapabilitiesUrl>https://api.mapbox.com/styles/v1/mapbox/light-v9/wmts?access_token=pk.eyJ1IjoiZ2F1dGhpZXJqcCIsImEiOiJjamFzYjdwNjE0bzM3MzNwbHo2aTNrNHUwIn0.MhhcnWJehKKV9QuVVermvw</GetCapabilitiesUrl>
  <Layer>light-v9</Layer>
  <Style>default</Style>
  <TileMatrixSet>GoogleMapsCompatible</TileMatrixSet>
  <DataWindow>
      <UpperLeftX>-20037508.34</UpperLeftX>
      <UpperLeftY>20037508.34</UpperLeftY>
      <LowerRightX>20037508.34</LowerRightX>
      <LowerRightY>-20037508.34</LowerRightY>
  </DataWindow>
  <BandsCount>4</BandsCount>
  <UnsafeSSL>true</UnsafeSSL>
  <ZeroBlockHttpCodes>404</ZeroBlockHttpCodes>
  <ZeroBlockOnServerException>true</ZeroBlockOnServerException>
</GDAL_WMTS>} }
   {                                   MapBox_Dark WMTS { <GDAL_WMTS>
  <GetCapabilitiesUrl>https://api.mapbox.com/styles/v1/mapbox/dark-v9/wmts?access_token=pk.eyJ1IjoiZ2F1dGhpZXJqcCIsImEiOiJjamFzYjdwNjE0bzM3MzNwbHo2aTNrNHUwIn0.MhhcnWJehKKV9QuVVermvw</GetCapabilitiesUrl>
  <Layer>dark-v9</Layer>
  <Style>default</Style>
  <TileMatrixSet>GoogleMapsCompatible</TileMatrixSet>
  <DataWindow>
      <UpperLeftX>-20037508.34</UpperLeftX>
      <UpperLeftY>20037508.34</UpperLeftY>
      <LowerRightX>20037508.34</LowerRightX>
      <LowerRightY>-20037508.34</LowerRightY>
  </DataWindow>
  <BandsCount>4</BandsCount>
  <UnsafeSSL>true</UnsafeSSL>
  <ZeroBlockHttpCodes>404</ZeroBlockHttpCodes>
  <ZeroBlockOnServerException>true</ZeroBlockOnServerException>
</GDAL_WMTS>} }
   {                                   MapBox_Satellite WMTS { <GDAL_WMTS>
  <GetCapabilitiesUrl>https://api.mapbox.com/styles/v1/mapbox/satellite-streets-v9/wmts?access_token=pk.eyJ1IjoiZ2F1dGhpZXJqcCIsImEiOiJjamFzYjdwNjE0bzM3MzNwbHo2aTNrNHUwIn0.MhhcnWJehKKV9QuVVermvw</GetCapabilitiesUrl>
  <Layer>satellite-streets-v9</Layer>
  <Style>default</Style>
  <TileMatrixSet>GoogleMapsCompatible</TileMatrixSet>
  <DataWindow>
      <UpperLeftX>-20037508.34</UpperLeftX>
      <UpperLeftY>20037508.34</UpperLeftY>
      <LowerRightX>20037508.34</LowerRightX>
      <LowerRightY>-20037508.34</LowerRightY>
  </DataWindow>
  <BandsCount>4</BandsCount>
  <UnsafeSSL>true</UnsafeSSL>
  <ZeroBlockHttpCodes>404</ZeroBlockHttpCodes>
  <ZeroBlockOnServerException>true</ZeroBlockOnServerException>
</GDAL_WMTS>} }
   }
}

#-------------------------------------------------------------------------------
# Nom      : <Mapper::DepotWare::WMTS::Default>
# Creation : Octobre 2010 - J.P. Gauthier - CMC/CMOE
#
# But      : Insere les services WMTS par défaut.
#
# Parametres :
#  <Frame>   : Fenetre parent
#
# Retour    :
#
# Remarque :
#
#-------------------------------------------------------------------------------

proc Mapper::DepotWare::WMTS::Default { } {
   variable Param
   
   if { [lsearch -exact -index 0 $::Mapper::DepotWare::Data(Depots) [lindex [lindex $Param(Depots) 0] 0]]==-1 } {
      foreach depot $Mapper::DepotWare::WMTS::Param(Depots) {    
         lappend ::Mapper::DepotWare::Data(Depots) $depot
      }
   }
}

#-------------------------------------------------------------------------------
# Nom      : <Mapper::DepotWare::WMTS::Params>
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

proc Mapper::DepotWare::WMTS::Params { Frame } {
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
# Nom      : <Mapper::DepotWare::WMTS::Request>
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

proc Mapper::DepotWare::WMTS::Request { } {
   variable Data

   return [.mapperdepot.params.xml.ent get 0.0 end]
}

#-------------------------------------------------------------------------------
# Nom      : <Mapper::DepotWare::WMTS::Select>
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

proc  Mapper::DepotWare::WMTS::SelectLayer { Layer } {

   Mapper::DepotWare::WMTS::Select "" "" $Layer ""
   Mapper::UpdateData $Page::Data(Frame)
}

proc  Mapper::DepotWare::WMTS::Select { Tree Branch { Select True } { SQL "" } } {
   variable Param

   set name [$Tree get $Branch name]
   set xml  [$Tree get $Branch path]
   
   if { ![file exists $Mapper::DepotWare::Data(CachePath)] } {
      file mkdir $Mapper::DepotWare::Data(CachePath)
   }

   set file $Mapper::DepotWare::Data(CachePath)/[string map { / "" ? "" " " "" : "" } $name].xml
   set f [open $file w]

   #----- Insert cache path
   set index [string first "</GDAL_WMTS>" $xml]
   puts $f [string trim [string range $xml 0 [expr $index-1]]]
   puts $f "   <Cache>
      <Path>[file rootname $file]</Path>
      <Depth>2</Depth>
   </Cache>
</GDAL_WMTS>"
   
   close $f

   set band [Mapper::GDAL::Read $file "" 3]

   #----- Decrease effective resolution (WMS-WMTS)
   gdalband configure $band -texres 2
}

#-------------------------------------------------------------------------------
# Nom      : <Mapper::DepotWare::WMTS::Load>
# Creation : Janvier 2014 - J.P. Gauthier - CMC/CMOE
#
# But      : Fonction usager pour afficher des donnees WMTS.
#
# Parametres :
#  <Name>    : Nom de la donnes WMTS
#  <Res>     : Facteur de resolution (Default:2)
#
# Retour    :
#
# Remarque :
#
#-------------------------------------------------------------------------------

proc  Mapper::DepotWare::WMTS::Load { Name { Res 2 } } {
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
   set index [string first "</GDAL_WMTS>" $xml]
   puts $f [string trim [string range $xml 0 [expr $index-1]]]
   puts $f "   <Cache>
      <Path>[file rootname $file]</Path>
      <Depth>2</Depth>
   </Cache>
</GDAL_WMTS>"
   
   close $f

   set band [Mapper::GDAL::Read $file "" 3]

   #----- Decrease effective resolution (WMS-WMTS)
   gdalband configure $band -texres $Res

   Mapper::UpdateData $Page::Data(Frame)
   Viewport::UpdateData $Page::Data(Frame)    
   
   return $band
}
