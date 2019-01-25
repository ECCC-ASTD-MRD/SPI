#===============================================================================
# Environnement Canada
# Centre Meteorologique Canadian
# 2100 Trans-Canadienne
# Dorval, Quebec
#
# Projet   : Librairie de "Widget" Tk.
# Fichier  : Mapper_Cutter.tcl
# Creation : Fevrier 2007 - J.P.Gauthier - CMC/CMOE
#
# Description:
#    Fontions de decoupe dans une donnee raster.
#
# Remarques :
#
#===============================================================================

namespace eval Mapper::Cutter { }

#----------------------------------------------------------------------------
# Nom      : <Mapper::Cutter::VertexAdd>
# Creation : Septembvre 2001 - J.P. Gauthier - CMC/CMOE
#
# But      : Ajout d'un point au polygone.
#
# Parametres :
#  <Frame>   : Identificateur de Page
#  <VP>      : Identificateur du Viewport
#  <X>       : Coordonnee X de la souris
#  <Y>       : Coordonnee Y de la souris
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc Mapper::Cutter::VertexAdd { Frame VP X Y } {

   if { $VP==-1 } {
      return
   }

   if { $Mapper::Data(Canvas)!="" } {
      $Mapper::Data(Canvas) delete MAPPERCUTTER
   }

   set loc [$VP -unproject $X $Y]
   set lat [lindex $loc 0]
   set lon [lindex $loc 1]

   #----- si le vertex est valide on l'ajoute a la liste
   if { $lat!=-999 && $lon!=-999 } {

      #----- If mask exists but comes from somehwere else, free it
      if { ![ogrgeometry is MASKRING$Mapper::Data(Object)] && [ogrgeometry is MASK$Mapper::Data(Object)] } {
         ogrgeometry free  MASK$Mapper::Data(Object)
      }

      if { ![ogrgeometry is MASK$Mapper::Data(Object)] } {
         ogrgeometry create MASK$Mapper::Data(Object) "Polygon"
         ogrgeometry create MASKRING$Mapper::Data(Object) "Linear Ring"
         ogrgeometry define MASK$Mapper::Data(Object) -geometry True MASKRING$Mapper::Data(Object)
      }
      lappend Mapper::Data(Mask$Mapper::Data(Object)) $lat $lon 0

      set xy [gdalband stats $Mapper::Data(Object) -unproject $lat $lon]
      lappend Mapper::Data(Cut$Mapper::Data(Object)) [lindex $xy 0] [lindex $xy 1]
      ogrgeometry define MASKRING$Mapper::Data(Object) -points $Mapper::Data(Cut$Mapper::Data(Object))

      if { $Mapper::Data(Cut) && $Mapper::Data(RealTime) } {
         Mapper::ParamsGDALSet $Mapper::Data(Object)
      }
      Mapper::UpdateItems $Frame
   }
}

#----------------------------------------------------------------------------
# Nom      : <Mapper::Cutter::VertexDelete>
# Creation : Septembvre 2001 - J.P. Gauthier - CMC/CMOE
#
# But      : Suppression d'un point au polygone.
#
# Parametres :
#  <Frame>   : Identificateur de Page
#  <VP>      : Identificateur du Viewport
#  <X>       : Coordonnee X de la souris
#  <Y>       : Coordonnee Y de la souris
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc Mapper::Cutter::VertexDelete { Frame VP } {

   if { $Mapper::Data(Canvas)!="" } {
      $Mapper::Data(Canvas) delete MAPPERCUTTER
   }

   set Mapper::Data(Mask$Mapper::Data(Object)) [lreplace $Mapper::Data(Mask$Mapper::Data(Object)) end-2 end]
   set Mapper::Data(Cut$Mapper::Data(Object)) [lreplace $Mapper::Data(Cut$Mapper::Data(Object)) end-1 end]

   Log::Print DEBUG "$Mapper::Data(Cut$Mapper::Data(Object))"
   ogrgeometry define MASKRING$Mapper::Data(Object) -points $Mapper::Data(Cut$Mapper::Data(Object))

   if { $Mapper::Data(Cut) && $Mapper::Data(RealTime) } {
      Mapper::ParamsGDALSet $Mapper::Data(Object)
   }
   Mapper::UpdateItems $Frame
}

#----------------------------------------------------------------------------
# Nom      : <Mapper::Cutter::VertexFollow>
# Creation : Septembvre 2001 - J.P. Gauthier - CMC/CMOE
#
# But      : Affiche une ligne entre le dernier vertex creer et la position du
#            curseur de la souris.
#
# Parametres :
#  <Frame>   : Identificateur de Page
#  <VP>      : Identificateur du Viewport
#  <X>       : Coordonnee X de la souris
#  <Y>       : Coordonnee Y de la souris
#  <Scan>    : Mode Scan
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc Mapper::Cutter::VertexFollow { Frame VP X Y Scan } {
   global GDefs

   if { $VP==-1 } {
      return
   }

   if { $Mapper::Data(Canvas)!="" } {
      $Mapper::Data(Canvas) delete VERTEXFOLLOW
   }

   set loc [$VP -unproject $X $Y]
   set lat [lindex $loc 0]
   set lon [lindex $loc 1]

   if { $lat!=-999 && $lon!=-999 && [info exists Mapper::Data(Mask$Mapper::Data(Object))] } {
      set tmp $Mapper::Data(Mask$Mapper::Data(Object))
      lappend tmp $lat $lon 0 [lindex $tmp 0] [lindex $tmp 1] 0

      Viewport::DrawLine $Frame $VP $tmp "PAGE$VP MAPPERCUTTER VERTEXFOLLOW" red 2

      if { $Mapper::Data(Cut) && $Mapper::Data(RealTime) } {
         Mapper::ParamsGDALSet $Mapper::Data(Object)
      }
   }
}

