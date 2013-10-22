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

   set Data(GDALExclude) { .hdr .jgw .txt .met }  ;# Fichier a exclure
   set Data(OGRExclude)  { .dbf .shx .txt }  ;# Fichier a exclure
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
         -command  { set Mapper::DepotWare::DIR::Data(Path) [FileBox::Create . "" LoadPath [concat [list $FileBox::Type(ALL)] $Mapper::Data(GDALFormats) $Mapper::Data(OGRFormats)]] }
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

proc  Mapper::DepotWare::DIR::Select { Tree Branch { Select True } } {

   set path [$Tree get $Branch path]

   switch -glob [set type [$Tree get $Branch type]] {
      "GDAL" { if { [lsearch -exact $Viewport::Data(Data$Page::Data(Frame)) $path]==-1 } {
                  Mapper::ReadBand $path
               }
             }
      "OGR"  { if { [lsearch -exact $Viewport::Data(Data$Page::Data(Frame)) $path]==-1 } {
                  Mapper::ReadLayer $path
               }
             }
   }
}

proc  Mapper::DepotWare::DIR::Parse { Tree Branch } {

   set path [$Tree get $Branch path]

   foreach file [lsort -dictionary -increasing [glob -nocomplain $path/*]] {
      set branch [$Tree insert $Branch end]
      if { [file isdirectory $file] } {
         $Tree set $branch open False
         $Tree set $branch name ""
         $Tree set $branch path $file
         $Tree set $branch type DIR
      } elseif { [Mapper::DepotWare::DIR::AddGDAL $Tree $branch $file] || [Mapper::DepotWare::DIR::AddOGR $Tree $branch $file] } {
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

proc Mapper::DepotWare::DIR::AddGDAL { Tree Branch File } {
   variable Data

   if { [lsearch -exact $Data(GDALExclude) [string tolower [file extension $File]]]!=-1 } {
      return False
   }

   eval set bad [catch { set bands [gdalfile open GDALPARSE read $File] }]

   if { !$bad } {
      if { [llength $bands] } {
         set width  [gdalfile width  GDALPARSE]
         set height [gdalfile height GDALPARSE]

         $Tree set $Branch open False
         $Tree set $Branch name ""
         $Tree set $Branch path $File
         $Tree set $Branch type GDAL
         $Tree set $Branch width  $width
         $Tree set $Branch height $height
         $Tree set $Branch 00 [gdalfile project GDALPARSE 1 1]
         $Tree set $Branch 01 [gdalfile project GDALPARSE 1 $height]
         $Tree set $Branch 10 [gdalfile project GDALPARSE $width 1]
         $Tree set $Branch 11 [gdalfile project GDALPARSE $width $height]

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

proc Mapper::DepotWare::DIR::AddOGR { Tree Branch File } {
   variable Data

   if { [lsearch -exact $Data(OGRExclude) [string tolower [file extension $File]]]!=-1 } {
      return False
   }

   set bad [catch { set layers [ogrfile open OGRPARSE read $File] }]

   if { !$bad } {
      if { [llength $layers] } {
         $Tree set $Branch open False
         $Tree set $Branch name ""
         $Tree set $Branch path $File
         $Tree set $Branch type OGR
         $Tree set $Branch width  0
         $Tree set $Branch height 0
         $Tree set $Branch 00 [list -90 -180.0]
         $Tree set $Branch 01 [list 90 -180.0]
         $Tree set $Branch 10 [list 90 180.0]
         $Tree set $Branch 11 [list -90 180.0]

         ogrfile close OGRPARSE
         return True
      }
      ogrfile close OGRPARSE
   }
   return False
}

