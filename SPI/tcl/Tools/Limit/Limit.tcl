#===============================================================================
# Environnement Canada
# Centre Meteorologique Canadian
# 2100 Trans-Canadienne
# Dorval, Quebec
#
# Projet   : Interface de selection des limites
# Fichier  : Limit.tcl
# Creation : Juillet 2019
#
# Description:
#
#    Cette interface permet de selectionner les limites d'affichage d'un champs
#
#
#===============================================================================

#----- Lire les sources d'execution

source $GDefs(Dir)/tcl/Tools/Limit/Limit.ctes
source $GDefs(Dir)/tcl/Tools/Limit/Limit.txt
source $GDefs(Dir)/tcl/Tools/Limit/Limit.int
package require LimitBox

#-------------------------------------------------------------------------------
# Nom      : <Limit::Close>
# Creation : Juin 2003 - J.P. Gauthier - CMC/CMOE -
#
# But      : Ferme l'interface de l'outil.
#
# Parametres :
#
# Retour:
#
# Remarques :
#
#-------------------------------------------------------------------------------

proc Limit::Close { } {
   variable Data

   #----- Si le mode etait celui de l'outils, revert to SPI

   if { $Page::Data(ToolMode)=="Limit" } {
      SPI::ToolMode SPI Zoom
   }

   #----- Cleanup de l'outils

   set Data(Active) 0

   $Data(Canvas) delete LIMIT

   destroy .limit

   if { !$SPI::Param(Window) } { SPI::Quit }
}

#----------------------------------------------------------------------------
# Nom      : <Limit::Draw...>
# Creation : Juin 2003 - J.P. Gauthier - CMC/CMOE
#
# But      : Fonctions de manipulation sur la projection
#
# Parametres :
#  <Frame>   : Identificateur de Page
#  <VP>      : Identificateur du Viewport
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc Limit::DrawInit { Frame VP } {
   variable Data

   set Data(Lat0)   $Viewport::Map(LatCursor)
   set Data(Lon0)   $Viewport::Map(LonCursor)
}

proc Limit::Draw { Frame VP } {
   variable Data

   if { $Data(Canvas)!="" } {
      $Data(Canvas) delete LIMIT
   }

   set Data(Lat1)   $Viewport::Map(LatCursor)
   set Data(Lon1)   $Viewport::Map(LonCursor)

   set Data(Canvas) $Frame.page.canvas
   set Data(Frame)  $Frame
   set Data(VP)     $VP

   Limit::UpdateItems $Frame
}

proc Limit::DrawDone { Frame VP } {
   variable Data

   if { $Data(Lat0)>$Data(Lat1) } {
      set tmp $Data(Lat1)
      set Data(Lat1) $Data(Lat0)
      set Data(Lat0) $tmp
   }

   if { $Data(Lon0)>$Data(Lon1) } {
      set tmp $Data(Lon1)
      set Data(Lon1) $Data(Lon0)
      set Data(Lon0) $tmp
   }

   if { $Data(Lat0)==$Data(Lat1) || $Data(Lon0)==$Data(Lon1) } {
      set Data(Coo) ""
   } else {
      set Data(Coo) "$Data(Lat0),$Data(Lon0) - $Data(Lat1),$Data(Lon1)"
   }

   foreach field [concat $FSTD::Data(List) $FSTD::Data(ListTool)] {
      if { [FSTD::ParamGetMode $field]==$FSTD::Param(Spec) } {
         set initial [fstdfield stats $field -coordpoint $Data(Lat0) $Data(Lon0)]
         set end [fstdfield stats $field -coordpoint $Data(Lat1) $Data(Lon1)]
         if { $LimitBox::Data(Top) == 0 } {
            set LimitBox::Data(Top) [expr [fstdfield define $field -NK] - 1]
         }
         LimitBox::SetLimits [lindex $initial 0] [lindex $initial 1] 0 [lindex $end 0] [lindex $end 1] $LimitBox::Data(Top)
      }
   }
}

proc Limit::MoveInit { Frame VP } {
   variable Data

   set Data(LonD) $Viewport::Map(LonCursor)
   set Data(LatD) $Viewport::Map(LatCursor)
}

proc Limit::Move { Frame VP } {
   variable Data

   #----- Effectuer la translation

   set lat0 [expr $Data(Lat0) + $Viewport::Map(LatCursor) - $Data(LatD)]
   set lat1 [expr $Data(Lat1) + $Viewport::Map(LatCursor) - $Data(LatD)]

   if { $lat0 > -90.0 && $lat0 < 90.0 && $lat1 > -90.0 && $lat1 < 90.0 } {

      set Data(Lat0) $lat0
      set Data(Lat1) $lat1
      eval set Data(Lon0) [Viewport::CheckCoord [expr $Data(Lon0) + $Viewport::Map(LonCursor) - $Data(LonD)]]
      eval set Data(Lon1) [Viewport::CheckCoord [expr $Data(Lon1) + $Viewport::Map(LonCursor) - $Data(LonD)]]
   }

   #----- Reaffecter le point de reference de translation

   if { $Data(Canvas)!="" } {
      $Data(Canvas) delete LIMIT
   }

   set Data(LonD)   $Viewport::Map(LonCursor)
   set Data(LatD)   $Viewport::Map(LatCursor)

   set Data(Canvas) $Frame.page.canvas
   set Data(Frame)  $Frame
   set Data(VP)     $VP

   Limit::UpdateItems $Frame
}

proc Limit::MoveDone { Frame VP } {
   variable Data
   Limit::DrawDone $Frame $VP
}

#-------------------------------------------------------------------------------
# Nom      : <Limit::Update>
# Creation : Juin 2003 - J.P. Gauthier - CMC/CMOE
#
# But      : Effectuer le "Refresh" de l'outils apres une mise a jour dans SPI
#
# Parametres :
#   <Frame>  : Identificateur de Page
#
# Remarques :
#    - Cette fonctions est appele par SPI au besoin.
#
#-------------------------------------------------------------------------------

proc Limit::Update { Frame } {
   variable Data
}

#-------------------------------------------------------------------------------
# Nom      : <Limit::UpdateItems>
# Creation : Juin 2003 - J.P. Gauthier - CMC/CMOE
#
# But      : Effectuer le "Refresh" des items relatifs a cet outils sur
#            la projection.
#
# Parametres :
#   <Frame>  : Identificateur de Page
#   <VP>     : Identificateur du Viewport
#
# Remarques :
#    - Cette fonctions est appele par SPI au besoin.
#
#-------------------------------------------------------------------------------

proc Limit::UpdateItems { Frame } {
   global   GDefs
   variable Data

   $Data(Canvas) delete LIMIT

   if { $Data(VP)!="" } {
      Viewport::DrawRange $Data(Frame) $Data(VP) $Data(Lat0) $Data(Lon0) $Data(Lat1) $Data(Lon1) LIMIT red
   }
}

#-------------------------------------------------------------------------------
# Nom      : <Limit::PageActivate>
# Creation : Octobre 2003 - J.P. Gauthier - CMC/CMOE
#
# But      : Effectuer le "Refresh" des items relatifs a cet outils
#            lors d'un changement de page par l'usager.
#
# Parametres :
#   <Frame>  : Identificateur de Page
#
# Remarques :
#    - Cette fonctions est appele par SPI au besoin.
#
#-------------------------------------------------------------------------------

proc Limit::PageActivate { Frame } {
}

#-------------------------------------------------------------------------------
# Nom      : <Limit::AsProject>
# Creation : Aout 2006 - J.P. Gauthier - CMC/CMOE
#
# But      : Sauvegarder l'etat de l'outils dans un projet SPI.
#
# Parametres :
#   <File>   : Descripteur de fichier ou ecrire les commandes
#
# Remarques :
#    - Le fichier est deja ouvert, il suffit d'y ecrire les commandes a executer
#      afin de re-instaurer l'outils dans son etat actuel.
#
#-------------------------------------------------------------------------------

proc Limit::AsProject { File } {
   variable Data
   variable Param

   if { [winfo exists .limit] } {
      puts $File "#----- Tool: Limit\n"
      puts $File "set Limit::Param(Dock)   $Param(Dock)"
      puts $File "set Limit::Param(Geom)   [winfo geometry .limit]"
      puts $File "Limit::Window"
      puts $File "\n"
   }
}
