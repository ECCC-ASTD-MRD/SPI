#===============================================================================
# Environnement Canada
# Centre Meteorologique Canadian
# 2100 Trans-Canadienne
# Dorval, Quebec
#
# Projet   : Librairie de "Tools" Tk.
# Fichier  : Mapper_DepotWare_PGS.tcl
# Creation : Novembre 2007 - J.P.Gauthier - CMC/CMOE
#
# Description:
#    Fontions de manipulation et de gestion des depots de donnees georeference.
#
# Remarques :
#
#===============================================================================

namespace eval Mapper::DepotWare::PGS {
   variable Data
   variable Lbl
   variable Msg

   set Data(Host)     ""
   set Data(User)     ""
   set Data(Port)     ""
   set Data(Password) ""
   set Data(Name)     ""
   set Data(Request)  ""
   set Data(SQL)     ""

   set Lbl(DBase)    { "Base de données" "Database" }
   set Lbl(Host)     { "Hôte" "Host" }
   set Lbl(Port)     { "Port" "Port" }
   set Lbl(User)     { "Usager" "User" }
   set Lbl(Password) { "Mot de passe" "Password" }
   set Lbl(Name)     { "DBase" "DBase" }
   set Lbl(Request)  { "Requète SQL" "SQL request" }

   set Msg(Request) { "Problème dans la requète, Il n'y a aucune donnée disponible dans cette base de données" "Problem with request, there is no data available in this database" }
}

#-------------------------------------------------------------------------------
# Nom      : <Mapper::DepotWare::PGS::Params>
# Creation : Decembre 2008 - J.P. Gauthier - CMC/CMOE
#
# But      : Fenetre des parametres du type de DB PostGIS (PGS).
#
# Parametres :
#  <Frame>   : Fenetre parent
#
# Retour    :
#
# Remarque :
#
#-------------------------------------------------------------------------------

proc Mapper::DepotWare::PGS::Params { Frame } {
   global GDefs
   variable Data
   variable Lbl

   frame $Frame.host
      label $Frame.host.lbl -text [lindex $Lbl(Host) $GDefs(Lang)] -width 12 -anchor w
      entry $Frame.host.ent -textvariable Mapper::DepotWare::PGS::Data(Host) -bd 1 -bg $GDefs(ColorLight)
      label $Frame.host.plbl -text [lindex $Lbl(Port) $GDefs(Lang)] -width 4 -anchor w
      entry $Frame.host.pent -textvariable Mapper::DepotWare::PGS::Data(Port) -bd 1 -bg $GDefs(ColorLight)
      pack $Frame.host.lbl -side left
      pack $Frame.host.ent -side left -fill x -expand True
      pack $Frame.host.plbl $Frame.host.pent -side left
   pack $Frame.host -side top -fill x

   frame $Frame.user
      label $Frame.user.lbl -text [lindex $Lbl(User) $GDefs(Lang)] -width 12 -anchor w
      entry $Frame.user.ent -textvariable Mapper::DepotWare::PGS::Data(User) -bd 1 -bg $GDefs(ColorLight)
      label $Frame.user.plbl -text [lindex $Lbl(Password) $GDefs(Lang)] -width 12 -anchor w
      entry $Frame.user.pent -textvariable Mapper::DepotWare::PGS::Data(Password) -bd 1 -bg $GDefs(ColorLight) -show *
      pack $Frame.user.lbl -side left
      pack $Frame.user.ent -side left -fill x -expand True
      pack $Frame.user.plbl $Frame.user.pent -side left
   pack $Frame.user -side top -fill x

   frame $Frame.name
      label $Frame.name.lbl -text [lindex $Lbl(Name) $GDefs(Lang)] -width 12 -anchor w
      entry $Frame.name.ent -textvariable Mapper::DepotWare::PGS::Data(Name) -bd 1 -bg $GDefs(ColorLight)
      pack $Frame.name.lbl -side left
      pack $Frame.name.ent -side left -fill x -expand True
   pack $Frame.name -side top -fill x

#   frame $Frame.req
#      label $Frame.req.lbl -text [lindex $Lbl(Request) $GDefs(Lang)] -width 12 -anchor w
#      text $Frame.req.ent -bd 1 -bg $GDefs(ColorLight) -height 5 -width 1
#      pack $Frame.req.lbl -side top -fill x
#      pack $Frame.req.ent -side top -fill both
#   pack $Frame.req -side top -fill both
}

#-------------------------------------------------------------------------------
# Nom      : <Mapper::DepotWare::PGS::Select>
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

proc  Mapper::DepotWare::PGS::Parse { Tree Branch } {

   set path [$Tree get $Branch path]

   foreach layer [Mapper::DepotWare::PGS::ParseLayer $path] {
      Mapper::DepotWare::PGS::Add  $Tree $Branch $layer
   }
}

proc  Mapper::DepotWare::PGS::Select { Tree Branch { Select True } } {

   if { $Select } {
      set path  [$Tree get [$Tree parent $Branch] path]
      set layer [$Tree get $Branch path]
      Mapper::OGE::Read $path [list $layer]
   } else {
   
   }
}

#-------------------------------------------------------------------------------
# Nom      : <Mapper::DepotWare::PGS::Request>
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

proc Mapper::DepotWare::PGS::Request { } {
   variable Data

    set req "PG:"
    if { $Data(Host)!="" }     { append req "host=$Data(Host) " }
    if { $Data(Port)!="" }     { append req "port=$Data(Port) " }
    if { $Data(User)!="" }     { append req "user=$Data(User) " }
    if { $Data(Password)!="" } { append req "password=$Data(Password) " }
    if { $Data(Name)!="" }     { append req "dbname=$Data(Name) " }

    return $req
}

#-------------------------------------------------------------------------------
# Nom      : <Mapper::DepotWare::PGS::Add>
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

proc Mapper::DepotWare::PGS::Add { Tree Branch Layer } {
   variable Data

   set branch [$Tree insert $Branch end]

   $Tree set $branch open False
   $Tree set $branch name ""
   $Tree set $branch path "[lindex $Layer 1] [lindex $Layer 2]"
   $Tree set $branch type PGS
   $Tree set $branch width  -1
   $Tree set $branch height -1

   $Tree set $branch 00 [list -90 -180.0]
   $Tree set $branch 01 [list 90 -180.0]
   $Tree set $branch 10 [list 90 180.0]
   $Tree set $branch 11 [list -90 180.0]

   return $branch
}

#-------------------------------------------------------------------------------
# Nom      : <Mapper::DepotWare::PGS::ParseLayer>
# Creation : Decembre 2008 - J.P. Gauthier - CMC/CMOE
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

proc Mapper::DepotWare::PGS::ParseLayer { Request } {
   global GDefs
   variable Msg
   variable Data

   set idxs {}
   eval set bad [catch { set idxs [ogrfile open OGRPARSE read $Request] }]
   ogrfile close OGRPARSE

   if { $bad } {
      Dialog::Error . $Msg(Request)
   }

   return $idxs
}
