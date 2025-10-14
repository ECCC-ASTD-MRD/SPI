#===============================================================================
# Environnement Canada
# Centre Meteorologique Canadian
# 2100 Trans-Canadienne
# Dorval, Quebec
#
# Projet   : Librairie de "Tools" Tk.
# Fichier  : Mapper_DepotWare_TSF.tcl
#
# Description:
#    Fontions de manipulation et de gestion des depots de donnees georeference.
#
# Remarques :
#
#===============================================================================

namespace eval Mapper::DepotWare::TSF {
   variable Param
   variable Data
   variable Lbl

   set Lbl(Path) { "Fichier de tuiles" "Tileset file" }

   set Data(Path) ""

   set Param(Depots) {
      {OpenStreetMap_EER_EN         TSF   /home/smco600/links/ords/geo/OSM/OpenStreetMap_EER_EN.mbtiles}
      {OpenStreetMap_EER_EN_Canada  TSF   /home/smco600/links/ords/geo/OSM/OpenStreetMap_EER_EN_Canada.mbtiles}
   }

   set Param(Formats) {
      {MBTiles {*.mbtiles}}
   }
}

#-------------------------------------------------------------------------------
# Nom      : <Mapper::DepotWare::TSF::Default>
#
# But      : Insere les services TSF par défaut.
#
# Parametres :
#  <Frame>   : Fenetre parent
#
# Retour    :
#
# Remarque :
#
#-------------------------------------------------------------------------------
proc Mapper::DepotWare::TSF::Default { } {
   variable Param

   if { [llength $Param(Depots)] && [lsearch -exact -index 0 $::Mapper::DepotWare::Data(Depots) [lindex $Param(Depots) 0 0]]==-1 } {
      lappend ::Mapper::DepotWare::Data(Depots) {*}$Param(Depots)
   }
}

#-------------------------------------------------------------------------------
# Nom      : <Mapper::DepotWare::TSF::Params>
#
# But      : Fenetre des parametres du type de DB (MabBox Tiles)
#
# Parametres :
#  <Frame>   : Fenetre parent
#
# Retour    :
#
# Remarque :
#
#-------------------------------------------------------------------------------
proc Mapper::DepotWare::TSF::Params { Frame } {
   global GDefs
   variable Data
   variable Lbl

   frame $Frame.path
      label $Frame.path.lbl -anchor w -text [lindex $Lbl(Path) $GDefs(Lang)] -width 15
      button $Frame.path.open -image OPEN -bd 0 -relief flat -overrelief raised -relief raised \
         -command  { set ::Mapper::DepotWare::TSF::Data(Path) [FileBox::Create . "" Path [list ]] }
      entry $Frame.path.ent -width 1 -bd 1 -bg $GDefs(ColorLight) -textvariable ::Mapper::DepotWare::TSF::Data(Path)
      pack $Frame.path.lbl -side left
      pack $Frame.path.ent -side left  -fill x -expand True
      pack $Frame.path.open -side left
   pack $Frame.path -fill x -expand True -anchor n
}

#-------------------------------------------------------------------------------
# Nom      : <Mapper::DepotWare::TSF::Request>
#
# But      : Retourner la requete formatee selon le type de DB.
#
# Parametres :
#
# Retour    :
#
# Remarque :
#
#-------------------------------------------------------------------------------

proc Mapper::DepotWare::TSF::Request { } {
   variable Data

   return $Data(Path)
}

#-------------------------------------------------------------------------------
# Nom      : <Mapper::DepotWare::TSF::Select>
#
# But      : Effectuer la selection d'une branche.
#
# Parametres :
#  <Tree>    : Arbre
#  <Branch>  : Branche selectionnee
#  <Select>  : Non-applicable
#  <SQL>     : SQL (non-applicable)
#
# Retour    :
#
# Remarque :
#
#-------------------------------------------------------------------------------
proc  Mapper::DepotWare::TSF::Select { Tree Branch { Select True } { SQL "" } } {
   variable Param

   set file [$Tree get $Branch path]
   set band [Mapper::GDAL::Read $file]

   #----- Decrease effective resolution (WMS-TMS)
   gdalband configure $band -texres 2
}

#-------------------------------------------------------------------------------
# Nom      : <Mapper::DepotWare::TSF::Load>
#
# But      : Fonction usager pour afficher des donnees TSF.
#
# Parametres :
#  <Name>    : Nom de la donnes TSF
#  <Res>     : Facteur de resolution (Default:2)
#
# Retour    :
#
# Remarque :
#
#-------------------------------------------------------------------------------

proc  Mapper::DepotWare::TSF::Load { Name { Res 2 } } {
   variable Param
   variable Data

   set file [lindex [lsearch -exact -index 0 -inline [lsearch -inline -exact -index 1 -all $::Mapper::DepotWare::Data(Depots) TSF] $Name] 2]
   if { $file == "" } {
      return
   }

   set band [Mapper::GDAL::Read $file "" 3 0]

   #----- Decrease effective resolution (WMS-TMS)
   gdalband configure $band -texres $Res

   Mapper::UpdateData $Page::Data(Frame)
   Viewport::UpdateData $Page::Data(Frame)

   return $band
}
