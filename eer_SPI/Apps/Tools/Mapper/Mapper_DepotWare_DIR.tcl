#===============================================================================
# Environnement Canada
# Centre Meteorologique Canadian
# 2100 Trans-Canadienne
# Dorval, Quebec
#
# Projet   : Librairie de "Tools" Tk.
# Fichier  : Mapper_DepotWare_DIR.tcl
# Creation : Novembre 2007 - J.P.Gauthier - CMC/CMOE
#
# Description:
#    Fontions de manipulation et de gestion des depots de donnees georeference.
#
# Remarques :
#
#===============================================================================

namespace eval Mapper::DepotWare::DIR {
   variable Data
   variable Lbl

   set Lbl(Path) { "Répertoire" "Directory" }

   set Data(Path) ""
}

#-------------------------------------------------------------------------------
# Nom      : <Mapper::DepotWare::DIR::Params>
# Creation : Decembre 2008 - J.P. Gauthier - CMC/CMOE
#
# But      : Fenetre des parametres du type de DB directory (DIR).
#
# Parametres :
#  <Frame>   : Fenetre parent
#
# Retour    :
#
# Remarque :
#
#-------------------------------------------------------------------------------

proc Mapper::DepotWare::DIR::Params { Frame } {
   global GDefs
   variable Data
   variable Lbl

   frame $Frame.path
      label $Frame.path.lbl -anchor w -text [lindex $Lbl(Path) $GDefs(Lang)] -width 15
      button $Frame.path.open -image OPEN -bd 0 -relief flat -overrelief raised -relief raised \
         -command  { set Mapper::DepotWare::Data(Path) [FileBox::Create . "" LoadPath [concat [list $FileBox::Type(ALL)] $Mapper::Data(GDALFormats) $Mapper::Data(OGRFormats)]] }
      entry $Frame.path.ent -width 1 -bd 1 -bg $GDefs(ColorLight) -textvariable Mapper::DepotWare::DIR::Data(Path)
      pack $Frame.path.lbl -side left
      pack $Frame.path.ent -side left  -fill x -expand True
      pack $Frame.path.open -side left
   pack $Frame.path -fill x -expand True
}

#-------------------------------------------------------------------------------
# Nom      : <Mapper::DepotWare::DIR::Select>
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

proc  Mapper::DepotWare::DIR::Select { Tree Branch Path URL } {

   foreach file [lsort -dictionary -increasing [glob -nocomplain $Path/*]] {
      set branch [$Tree insert $Branch end]
      if { [file isdirectory $file] } {
         $Tree set $branch open False
         $Tree set $branch name ""
         $Tree set $branch path $file
         $Tree set $branch type DIR
      } elseif { [Mapper::DepotWare::DIR::AddGDAL $branch $file] || [Mapper::DepotWare::DIR::AddOGR $branch $file] } {
      } else {
         $Tree delete $branch
      }
   }
}

#-------------------------------------------------------------------------------
# Nom      : <Mapper::DepotWare::DIR::Request>
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

proc Mapper::DepotWare::DIR::Request { } {
   variable Data

   return $Data(Path)
}
#-------------------------------------------------------------------------------
# Nom      : <Mapper::DepotWare::DIR::AddGDAL>
# Creation : Novembre 2007 - J.P. Gauthier - CMC/CMOE
#
# But      : Ajouter une branche pour une couche GDAL.
#
# Parametres :
#  <Branch>  : Branche
#  <File>    : Fichier
#
# Retour    :
#
# Remarque :
#
#-------------------------------------------------------------------------------

proc Mapper::DepotWare::DIR::AddGDAL { Branch File } {
   variable Data

   if { [lsearch -exact $Data(GDALExclude) [string tolower [file extension $File]]]!=-1 } {
      return False
   }

   eval set bad [catch { set bands [gdalfile open GDALPARSE read $File] }]

   if { !$bad } {
      if { [llength $bands] } {
         set width  [gdalfile width  GDALPARSE]
         set height [gdalfile height GDALPARSE]

         TREE set $Branch open False
         TREE set $Branch name ""
         TREE set $Branch path $File
         TREE set $Branch type GDAL
         TREE set $Branch width  $width
         TREE set $Branch height $height
         TREE set $Branch 00 [gdalfile project GDALPARSE 1 1]
         TREE set $Branch 01 [gdalfile project GDALPARSE 1 $height]
         TREE set $Branch 10 [gdalfile project GDALPARSE $width 1]
         TREE set $Branch 11 [gdalfile project GDALPARSE $width $height]

         gdalfile close GDALPARSE
         return True
      }
      gdalfile close GDALPARSE
   }
   return False
}

#-------------------------------------------------------------------------------
# Nom      : <Mapper::DepotWare::DIR::AddOGR>
# Creation : Novembre 2007 - J.P. Gauthier - CMC/CMOE
#
# But      : Ajouter une branche pour une couche OGR.
#
# Parametres :
#  <Branch>  : Branche
#  <File>    : Fichier
#
# Retour    :
#
# Remarque :
#
#-------------------------------------------------------------------------------

proc Mapper::DepotWare::DIR::AddOGR { Branch File } {
   variable Data

   if { [lsearch -exact $Data(OGRExclude) [string tolower [file extension $File]]]!=-1 } {
      return False
   }

   set bad [catch { set layers [ogrfile open OGRPARSE read $File] }]

   if { !$bad } {
      if { [llength $layers] } {
         TREE set $Branch open False
         TREE set $Branch name ""
         TREE set $Branch path $File
         TREE set $Branch type OGR
         TREE set $Branch width  0
         TREE set $Branch height 0
         TREE set $Branch 00 [list -90 -180.0]
         TREE set $Branch 01 [list 90 -180.0]
         TREE set $Branch 10 [list 90 180.0]
         TREE set $Branch 11 [list -90 180.0]

         ogrfile close OGRPARSE
         return True
      }
      ogrfile close OGRPARSE
   }
   return False
}

