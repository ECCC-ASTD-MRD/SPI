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

namespace eval Mapper::DepotWare::WCS {
   variable Data
   variable Lbl
   variable Msg

   set Lbl(URL) { "Addresse URL" "URL Address" }

   set Msg(Request) { "Problème dans la requète de capacitées WCS (GetCapabilities)" "Problem requesting capabilities WCS (GetCapabilities)" }

   set Data(URL) ""
}

#-------------------------------------------------------------------------------
# Nom      : <Mapper::DepotWare::WCS::Params>
# Creation : Decembre 2008 - J.P. Gauthier - CMC/CMOE
#
# But      : Fenetre des parametres du type de DB Web Coverage Service (WCS).
#
# Parametres :
#  <Frame>   : Fenetre parent
#
# Retour    :
#
# Remarque :
#
#-------------------------------------------------------------------------------

proc Mapper::DepotWare::WCS::Params { Frame } {
   global GDefs
   variable Data
   variable Lbl

   frame $Frame.path
      label $Frame.path.lbl -anchor w -text [lindex $Lbl(URL) $GDefs(Lang)] -width 15
      entry $Frame.path.ent -width 1 -bd 1 -bg $GDefs(ColorLight) -textvariable Mapper::DepotWare::WCS::Data(URL)
      pack $Frame.path.lbl -side left
      pack $Frame.path.ent -side left  -fill x -expand True
      pack $Frame.path.open -side left
   pack $Frame.path -fill x -expand True
}

#-------------------------------------------------------------------------------
# Nom      : <Mapper::DepotWare::WCS::Select>
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

proc  Mapper::DepotWare::WCS::Select { Tree Branch Path URL } {
   global GDefs
   variable Msg

   if { $URL=="URL" }  {
      if { [string first "?" ${Path}]==-1 } {
         set req [http::geturl "${Path}?&SERVICE=WCS&REQUEST=GetCapabilities"]
      } else {
         set req [http::geturl "${Path}&SERVICE=WCS&REQUEST=GetCapabilities"]
      }
      if { [catch { set doc [dom::parse [http::data $req]] } ] } {
         Dialog::ErrorListing . $Msg(Request) [http::data $req]
         return
      }

      set WCS(Version) 1.1.0
      set layer [lindex [set [dom::document getElementsByTagName $doc CoverageSummary]] 0]
      foreach layer [Mapper::DepotWare::WCS:ParseLayer $Path $layer] {
         Mapper::DepotWare::WCS::Add $Tree $Branch $layer
      }
      dom::destroy $doc
   } else {
      set def [Mapper::DepotWare::WCS::BuildXMLDef $path]
      if { [lsearch -exact $Viewport::Data(Data$Page::Data(Frame)) $def]==-1 } {
         Mapper::ReadBand $def "" 3
      }
   }
}

#-------------------------------------------------------------------------------
# Nom      : <Mapper::DepotWare::WCS::Request>
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
# Nom      : <Mapper::DepotWare::WCS::Add>
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

proc Mapper::DepotWare::WCS::Add { Branch Layer } {
   variable Data

   set branch [$Tree insert $Branch end]

   $Tree set $branch open False
   $Tree set $branch name ""
   $Tree set $branch path $Layer
   $Tree set $branch type WCS
   $Tree set $branch width  -1
   $Tree set $branch height -1

   set bbox  [lindex $Data($Layer) 3]
   $Tree set $branch 00 [list [lindex $bbox 1] [lindex $bbox 0]]
   $Tree set $branch 01 [list [lindex $bbox 3] [lindex $bbox 0]]
   $Tree set $branch 10 [list [lindex $bbox 1] [lindex $bbox 2]]
   $Tree set $branch 11 [list [lindex $bbox 3] [lindex $bbox 2]]
}

#-------------------------------------------------------------------------------
# Nom      : <Mapper::DepotWare::WCS:ParseLayer>
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

proc Mapper::DepotWare::WCS::ParseLayer { URL Node { First True } } {
   variable Data

   if { $First } {
      set Data(Layers)     {}
      set Data(BBox)       {}
      set Data(Identifier) ""
      set Data(Format)     ""
      set Data(Geographic) ""
      set Data(SizeX)      0
      set Data(SizeY)      0
   }

   foreach node [set [dom::node configure $Node -childNodes]] {
      switch [dom::node configure $node  -nodeName] {
         CoverageSummary          { Mapper::DepotWare::WCS::ParseLayer $URL $node False }
         EX_GeographicBoundingBox { Mapper::DepotWare::WMS::ParseGeographic $node }
         LatLonBoundingBox        { Mapper::DepotWare::WMS::ParseLatLonBoundingBox $node }
         BoundingBox              { Mapper::DepotWare::WMS::ParseBoundingBox $node }
         Identifier               { set Data(Identifier) [dom::node cget [dom::node children $node] -nodeValue] }
         ows:Title                { set Data(Title) [dom::node cget [dom::node children $node] -nodeValue] }
      }
   }

   if { $Data(Identifier)!="" } {
      set Data($Data(Title)) [list $URL $Data(Identifier) $Data(BBox) $Data(Geographic) $Data(SizeX) $Data(SizeY) $Data(Format)]
      lappend Data(Layers) $Data(Title)
   }
   set Data(Identifier) ""
   return $Data(Layers)
}

#-------------------------------------------------------------------------------
# Nom      : <Mapper::DepotWare::WCS::BuildXMLDef>
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

proc Mapper::DepotWare::WCS::BuildXMLDef { Layer } {
   variable Data

   set url    [lindex $Data($Layer) 0]
   set layer  [lindex $Data($Layer) 1]

   set layer [string map { " " "%20" } $layer]
   if { [string first "?" ${url}]==-1 } {
      set url ${url}?
   } else {
      set url $url
   }

   set xml "<GDAL_WCS>\n"
   append xml "   <ServiceURL>${url}</ServiceURL>\n"
   append xml "   <CoverageName>$layer</CoverageName>"
   append xml "</GDAL_WCS>"

   set f [open $file w]
   puts $f $xml
   close $f

   return $file
}
