#!/bin/sh
# the next line restarts using tclsh \
exec $SPI_PATH/tclsh "$0" "$@"
#============================================================================
# Environnement Canada
# Centre Meteorologique Canadien
# 2100 Trans-Canadienne
# Dorval, Quebec
#
# Projet     : Exemple de scripts.
# Fichier    : ProjCamIndex.tcl
# Creation   : Mai 2016 - J.P. Gauthier - CMC/CMOE
# Description: Creer un index de couverture des diverses camera d'un usager
#
# Parametres :
#
# Retour:
#
# Remarques  :
#
#============================================================================

namespace eval Macro::ProjCamIndex {} {
   variable Param
   variable Data
   variable Error

   set Param(Info) { "Creer un index de couverture des diverses camera d'un usager."
                     "Create an index for a user's projcams. " }

   set Data(Page) ""

}

proc Macro::ProjCamIndex::Execute { } {
   global GDefs


   #----- Open GEM index file
#   eval file delete [glob DataOut/GEM.*]
#   ogrfile open INDEXFILE write DataOut/GEM.shp "ESRI Shapefile"
   ogrlayer new CAMINDEX "ProjCam" "Polygon"
   ogrlayer define CAMINDEX -field NAME String
   ogrlayer define CAMINDEX -nb [llength $ProjCam::Data(Names)]

   #----- Initialiser la geometrie
   ogrgeometry create POLY "Polygon"
   ogrgeometry create RING "Linear Ring"

   set no 0
   set vp $Viewport::Data(VP)
   
   #----- Loop on the cameras
   foreach cam $ProjCam::Data(Names) {

      Log::Print INFO "Processing $cam"

      ProjCam::Select $Page::Data(Frame) $Page::Data(Frame) $cam True
      
      set lst {}
      foreach { la lo elev } [$vp -unproject 0 0 $Viewport::Data(Width$vp) 0 10.0] { lappend lst $lo $la }
      foreach { la lo elev } [$vp -unproject $Viewport::Data(Width$vp) 0 $Viewport::Data(Width$vp) $Viewport::Data(Height$vp) 10.0] { lappend lst $lo $la }
      foreach { elev lo la } [lreverse [$vp -unproject $Viewport::Data(Width$vp) $Viewport::Data(Height$vp) 0 $Viewport::Data(Height$vp) 10.0]] { lappend lst $lo $la }
      foreach { elev lo la } [lreverse [$vp -unproject 0 $Viewport::Data(Height$vp) 0 0 10.0]] { lappend lst $lo $la }

      if { [lsearch -real $lst -999.0]==-1 } {
         ogrgeometry define RING -points $lst
         ogrgeometry stats RING -close
         ogrgeometry define POLY -geometry False RING

         ogrlayer define CAMINDEX -feature $no NAME $cam
         ogrlayer define CAMINDEX -geometry $no False POLY
         ogrlayer configure CAMINDEX -outline black -width 5 -font XFont12 -labelvar NAME

         incr no
      }
   }
   
   Mapper::UpdateData $Page::Data(Frame) CAMINDEX
   ProjCam::Reset $Page::Data(Frame)

#   ogrfile close INDEXFILE
}
