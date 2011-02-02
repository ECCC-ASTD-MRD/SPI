#===============================================================================
# Environnement Canada
# Centre Meteorologique Canadian
# 2100 Trans-Canadienne
# Dorval, Quebec
#
# Projet   : Librairie de "Tools" Tk.
# Fichier  : Mapper_DepotWare_PGS.tcl
# Creation : Fevrier 2010 - J.P.Gauthier - CMC/CMOE
#
# Description:
#    Fontions de manipulation et de gestion des depots de donnees georeference.
#
# Remarques :
#
#===============================================================================

namespace eval Mapper::DepotWare::WFS {
   variable Data
   variable Lbl
   variable Msg

   set Lbl(URL) { "Addresse URL" "URL Address" }

   set Msg(Request) { "Problème dans la requète de capacitées WFS (GetCapabilities)" "Problem requesting capabilities WFS (GetCapabilities)" }
}

#-------------------------------------------------------------------------------
# Nom      : <Mapper::DepotWare::WFS::Params>
# Creation : Fevrier 2010 2008 - J.P. Gauthier - CMC/CMOE
#
# But      : Fenetre des parametres du type de DB Web Feature Service (WFS).
#
# Parametres :
#  <Frame>   : Fenetre parent
#
# Retour    :
#
# Remarque :
#
#-------------------------------------------------------------------------------

proc Mapper::DepotWare::WFS::Params { Frame } {
   global GDefs
   variable Data
   variable Lbl

   frame $Frame.path
      label $Frame.path.lbl -anchor w -text [lindex $Lbl(URL) $GDefs(Lang)] -width 15
      entry $Frame.path.ent -width 1 -bd 1 -bg $GDefs(ColorLight) -textvariable Mapper::DepotWare::WFS::Data(URL)
      pack $Frame.path.lbl -side left
      pack $Frame.path.ent -side left  -fill x -expand True
   pack $Frame.path -fill x -expand True
}

#-------------------------------------------------------------------------------
# Nom      : <Mapper::DepotWare::WFS::Select>
# Creation : Fevrier 2010 - J.P. Gauthier - CMC/CMOE
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

proc  Mapper::DepotWare::WFS::Select { Tree Branch Path URL } {

   if { $URL=="URL" }  {
      foreach layer [Mapper::DepotWare::WFS::ParseLayer $Path] {
         Mapper::DepotWare::WFS::Add $Tree $Branch $layer
      }
   } else {
      set path  [$Tree get [$Tree parent $Branch] path]
      set layer [$Tree get $Branch path]
      Mapper::ReadLayer WFS:$path?VERSION=1.1.0&SERVICE=WFS [list $layer]
   }
}

#-------------------------------------------------------------------------------
# Nom      : <Mapper::DepotWare::WFS::Add>
# Creation : Fevrier 2010 - J.P. Gauthier - CMC/CMOE
#
# But      : Ajouter une branche pour une couche WFS.
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

proc Mapper::DepotWare::WFS::Add { Tree Branch Layer } {
   variable Data

   set branch [$Tree insert $Branch end]

   $Tree set $branch open False
   $Tree set $branch name ""
   $Tree set $branch path "[lindex $Layer 1] [lindex $Layer 2]"
   $Tree set $branch type WFS
   $Tree set $branch width  -1
   $Tree set $branch height -1

   $Tree set $branch 00 [list -90 -180.0]
   $Tree set $branch 01 [list 90 -180.0]
   $Tree set $branch 10 [list 90 180.0]
   $Tree set $branch 11 [list -90 180.0]
}

#-------------------------------------------------------------------------------
# Nom      : <Mapper::DepotWare::WFS::Request>
# Creation : Fevrier 2011 - J.P. Gauthier - CMC/CMOE
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

proc Mapper::DepotWare::WFS::Request { } {
   variable Data

   return $Data(URL)
}

#-------------------------------------------------------------------------------
# Nom      : <Mapper::DepotWare::WFS::ParseLayer>
# Creation : Fevrier 2010 - J.P. Gauthier - CMC/CMOE
#
# But      : Recuperer les informations sur les couches disponibles
#
# Parametres :
#  <Request> : Requete d'ouverture de la DB
#
# Retour    :
#
# Remarque :
#
#-------------------------------------------------------------------------------

proc Mapper::DepotWare::WFS::ParseLayer { Request } {
   global GDefs
   variable Msg
   variable Data

   set idxs {}
   eval set bad [catch { set idxs [ogrfile open OGRPARSE read WFS:$Request?VERSION=1.1.0&SERVICE=WFS] } msg ]
   ogrfile close OGRPARSE

   if { $bad } {
      Dialog::Error . $Msg(Request) \n\n\t$msg
   }

   return $idxs
}
