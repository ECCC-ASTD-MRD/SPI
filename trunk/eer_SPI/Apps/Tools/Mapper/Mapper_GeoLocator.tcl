#===============================================================================
# Environnement Canada
# Centre Meteorologique Canadian
# 2100 Trans-Canadienne
# Dorval, Quebec
#
# Projet   : Librairie de "Widget" Tk.
# Fichier  : Mapper_GeoLocator.tcl
# Creation : Fevrier 2007 - J.P.Gauthier - CMC/CMOE
#
# Description:
#    Fcontions de geolocalisation par point de controle.
#
# Remarques :
#
#===============================================================================

namespace eval Mapper::GeoLocator { } {
   variable Data

   set Data(X)    0
   set Data(Y)    0
   set Data(RefX) 0
   set Data(RefY) 0
   set Data(Lat)  0
   set Data(Lon)  0

   set Data(Modes)  { "Simple" "Polynomial 1st order" "Polynomial 2nd order" "Polynomial 3rd order" "Thin spline" }
   set Data(Mode)   [lindex $Data(Modes) 1]
   set Data(GCPS)   {}
   set Data(GCPNb)  0
}

proc Mapper::GeoLocator::Activate { Object } {
   global GDefs
   variable Data

   set Mapper::Data(RefVP) [Viewport::Create $Mapper::Data(Frame4).georef.hfrm.map 0 0 5 5 False True]
   Page::ModeSelect Zoom $Mapper::Data(Frame4).georef.hfrm.map

   Viewport::FollowerAdd Mapper::GeoLocator

   if { [georef is [set ref [gdalband define $Mapper::Data(Object) -georef]]] } {
      set Viewport::Data(Data$Mapper::Data(Frame4).georef.hfrm.map) $Mapper::Data(Object)
      projection configure $Mapper::Data(Frame4).georef.hfrm.map -geographic False -type grid -georef $ref -data $Object
      Viewport::ConfigGet $Mapper::Data(Frame4).georef.hfrm.map $Mapper::Data(RefVP)
      Viewport::Reset $Mapper::Data(Frame4).georef.hfrm.map
   } else {
      Dialog::CreateError . [lindex $Mapper::Msg(GeoRef) $GDefs(Lang)] $GDefs(Lang)
   }

   set Data(GCPS) {}
   foreach gcp [gdalband define $Object -gcps] {
      set item "[lindex $Mapper::Lbl(GCP) $GDefs(Lang)] $Data(GCPNb)"
      lappend Data(GCPS) "$item"
      set ll [gdalband stats $Object -gridpoint [lindex $gcp 0] [lindex $gcp 1]]
      set Data(GCP$item) [concat $gcp  [lindex $ll 0] [lindex $ll 1]]
      incr Data(GCPNb)
   }
}

#-------------------------------------------------------------------------------
# Nom      : <Mapper::GeoLocator::Follower>
# Creation : Janvier 2007 - Maxime Samuel - CMC/CMOE
#
# But      : Plugin au bindings de suivit de coordonnees du Viewport de Params
#
# Parametres :
#   <Frame>  : Page courante
#   <Canvas> : Canvas courant
#   <VP>     : Viewport courant
#   <Lat>    : Lattitude
#   <Lon>    : Longitude
#   <X>      : Pixel en X
#   <Y>      : Pixel en Y
#
# Retour    :
#
# Remarque :
#
#-------------------------------------------------------------------------------

proc Mapper::GeoLocator::Follower  { Frame Canvas VP Lat Lon X Y } {
   variable Data

   set Data(X)    "-"
   set Data(Y)    "-"
   set Data(RefX) "-"
   set Data(RefY) "-"
   set Data(Lat)  "-"
   set Data(Lon)  "-"

   if { [gdalband is $Mapper::Data(Object)] } {
      if { $Frame == "$Mapper::Data(Frame4).georef.hfrm.map" } {
         set x $Viewport::Map(GridICursor)
         set y $Viewport::Map(GridJCursor)

         if { $x>=0 && $y>=0 } {
            set Data(X) $x
            set Data(Y) $y

            set xy [gdalband stats $Mapper::Data(Object) -project $x $y]
            set xy [gdalband stats $Mapper::Data(Object) -unproject $Viewport::Map(LatCursor) $Viewport::Map(LonCursor)]
            set Data(RefX) [lindex $xy 0]
            set Data(RefY) [lindex $xy 1]
         }
      } elseif { $Viewport::Map(LatCursor) != -999.0 || $Viewport::Map(LonCursor) != -639.0 } {
         set Data(Lat) $Viewport::Map(LatCursor)
         set Data(Lon) $Viewport::Map(LonCursor)
      }
   }
}

#-------------------------------------------------------------------------------
# Nom      : <Mapper::GeoLocator::CoordAdd>
# Creation : Janvier 2007 - Maxime Samuel - CMC/CMOE
#
# But      : Ajout d'un point a la liste de points de controle
#
# Parametres :
#
# Retour    :
#
# Remarque :
#
#-------------------------------------------------------------------------------

proc Mapper::GeoLocator::CoordAdd { } {
   global   GDefs
   variable Data

   set item "[lindex $Mapper::Lbl(GCP) $GDefs(Lang)] $Data(GCPNb)"
   lappend Data(GCPS) "$item"
   set Data(GCP$item) [list -1 -1 -1 -1 -999 -999]
   incr Data(GCPNb)

   $Mapper::Data(Frame4).georef.pt.list selection clear 0 end
   $Mapper::Data(Frame4).georef.pt.list selection set end
   $Mapper::Data(Frame4).georef.tool.mode deselect
   $Mapper::Data(Frame4).georef.tool.mode invoke
}

#-------------------------------------------------------------------------------
# Nom      : <Mapper::GeoLocator::CoordDel>
# Creation : Janvier 2007 - Maxime Samuel - CMC/CMOE
#
# But      : Suppression d'un point de la liste de points de controle
#
# Parametres :
#
# Retour    :
#
# Remarque :
#
#-------------------------------------------------------------------------------

proc Mapper::GeoLocator::CoordDel { } {
   variable Data

   if { [llength [set sel [$Mapper::Data(Frame4).georef.hfrm2.dotfrm.dotlist curselection]]] } {
      set curselect [lindex $Data(GCPS) $sel]
      set Data(GCPS) [lreplace $Data(GCPS) $sel $sel]
      if { [info exists Mapper::Data(GCP$curselect)] } {
         unset Mapper::Data(GCP$curselect)
      }
   }
   $Mapper::Data(Frame4).georef.pt.list selection clear 0 end
   $Mapper::Data(Frame4).georef.pt.list selection set end

   Mapper::UpdateItems $Mapper::Data(Frame4).georef.hfrm.map
}

#-------------------------------------------------------------------------------
# Nom      : <Mapper::GeoLocator::CoordDel>
# Creation : Janvier 2007 - Maxime Samuel - CMC/CMOE
#
# But      : Afficher les coordonnees d'un point de controle
#
# Parametres :
#
# Retour    :
#
# Remarque :
#
#-------------------------------------------------------------------------------

proc Mapper::GeoLocator::CoordView { } {
   variable Data

   set xylatlon $Data(GCP[lindex $Data(GCPS) [$Mapper::Data(Frame4).georef.pt.list curselection]])
   set Data(X)    [lindex $xylatlon 0]
   set Data(Y)    [lindex $xylatlon 1]
   set Data(RefX) [lindex $xylatlon 2]
   set Data(RefY) [lindex $xylatlon 3]
   set Data(Lat)  [lindex $xylatlon 4]
   set Data(Lon)  [lindex $xylatlon 5]
}

#-------------------------------------------------------------------------------
# Nom      : <Mapper::GeoLocator::CoordDel>
# Creation : Janvier 2007 - Maxime Samuel - CMC/CMOE
#
# But      : Assigner les coordonnees d'un point de controle
#
# Parametres :
#
# Retour    :
#
# Remarque :
#
#-------------------------------------------------------------------------------

proc Mapper::GeoLocator::CoordSet { } {
   variable Data

   set listCoord {}

   foreach curindex $Data(GCPS) {
      set col [lindex $Data(GCP$curindex) 0]
      set line [lindex $Data(GCP$curindex) 1]
      set x [lindex $Data(GCP$curindex) 2]
      set y [lindex $Data(GCP$curindex) 3]
      if { $col>= 0 && $line>= 0 && $x!=-999.0 && $y!=-999.0 } {
         lappend listCoord [list $col $line $x $y]
      }
   }
   gdalband define $Mapper::Data(Object) -gcps $listCoord [lsearch -exact $Data(Modes) $Data(Mode)]
   set Mapper::Data(Trans)    [gdalband define $Mapper::Data(Object) -transform]
   set Mapper::Data(InvTrans) [gdalband define $Mapper::Data(Object) -invtransform]
}

#-------------------------------------------------------------------------------
# Nom      : <Mapper::GeoLocator::Draw...>
# Creation : Janvier 2007 - Maxime Samuel - CMC/CMOE
#
# But      : Fonctions de manipulations de la selection.
#
# Parametres :
#   <Frame   : Identificateur de Page
#   <VP>     : Identificateur du Viewport
#
# Remarques :
#    - Ces fonctions sont appele par le package Page au besoin.
#
#-------------------------------------------------------------------------------

proc Mapper::GeoLocator::Draw     { Frame VP } { }

proc Mapper::GeoLocator::DrawDone { Frame VP } { }

proc Mapper::GeoLocator::DrawInit { Frame VP } {
   variable Data

   set Mapper::Data(Frame)  $Frame

   if { [llength [set sel [$Mapper::Data(Frame4).georef.pt.list curselection]]] } {

      set curselect [lindex $Data(GCPS) $sel]
      if { $Frame == "$Mapper::Data(Frame4).georef.hfrm.map" } {
         if { $Viewport::Map(GridICursor)>=0 && $Viewport::Map(GridJCursor)>=0 } {
            lset Data(GCP$curselect) 0 $Viewport::Map(GridICursor)
            lset Data(GCP$curselect) 1 $Viewport::Map(GridJCursor)
         }
      } else {
         if { $Viewport::Map(LatCursor)>-90.0 && $Viewport::Map(LatCursor)<90.0 } {
            lset Data(GCP$curselect) 4 $Viewport::Map(LatCursor)
            lset Data(GCP$curselect) 5 $Viewport::Map(LonCursor)

            set xy [gdalband stats $Mapper::Data(Object) -unproject $Viewport::Map(LatCursor) $Viewport::Map(LonCursor)]
            lset Data(GCP$curselect) 2 [lindex $xy 0]
            lset Data(GCP$curselect) 3 [lindex $xy 1]
         }
      }
      Mapper::UpdateItems $Frame
   }
}

proc Mapper::GeoLocator::Move { Frame VP } { }

proc Mapper::GeoLocator::MoveDone { Frame VP } { }

proc Mapper::GeoLocator::MoveInit { Frame VP } { }
