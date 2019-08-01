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
# Creation : Aout 2019 - A. Germain - CMC
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

   if { $Viewport::Map(Type)=="grid" } {
      set Data(X0)   $Viewport::Map(GridICursor)
      set Data(Y0)   $Viewport::Map(GridJCursor)
   } else {
      foreach field [concat $FSTD::Data(List) $FSTD::Data(ListTool)] {
         if { [FSTD::ParamGetMode $field]==$FSTD::Param(Spec) } {
            set temp [fstdfield stats $field -coordpoint $Viewport::Map(LatCursor) $Viewport::Map(LonCursor)]
         }
      }
      set Data(X0)   [lindex $temp 0]
      set Data(Y0)   [lindex $temp 1]
   }
}

proc Limit::Draw { Frame VP } {
   variable Data

   if { $Data(Canvas)!="" } {
      $Data(Canvas) delete LIMIT
   }

   if { $Viewport::Map(Type)=="grid" } {
      set Data(X1)   $Viewport::Map(GridICursor)
      set Data(Y1)   $Viewport::Map(GridJCursor)
   } else {
      foreach field [concat $FSTD::Data(List) $FSTD::Data(ListTool)] {
         if { [FSTD::ParamGetMode $field]==$FSTD::Param(Spec) } {
            set temp [fstdfield stats $field -coordpoint $Viewport::Map(LatCursor) $Viewport::Map(LonCursor)]
         }
      }
      set Data(X1)   [lindex $temp 0]
      set Data(Y1)   [lindex $temp 1]
   }
   set Data(Canvas) $Frame.page.canvas
   set Data(Frame)  $Frame
   set Data(VP)     $VP

   Limit::UpdateItems $Frame
}

proc Limit::DrawDone { Frame VP } {
   variable Data

   foreach field [concat $FSTD::Data(List) $FSTD::Data(ListTool)] {
      if { [FSTD::ParamGetMode $field]==$FSTD::Param(Spec) } {
         if { $LimitBox::Data(Top) == 0 } {
            set LimitBox::Data(Top) [expr [fstdfield define $field -NK] - 1]
         }
         set LimitBox::Data(North) $Data(Y1)
         set LimitBox::Data(South) $Data(Y0)
         set LimitBox::Data(East) $Data(X1)
         set LimitBox::Data(West) $Data(X0)
         LimitBox::SetLimits $LimitBox::Data(West) $LimitBox::Data(South) 0 $LimitBox::Data(East) $LimitBox::Data(North) $LimitBox::Data(Top)
         Page::Update $Page::Data(Frame)
         Page::UpdateCommand $Page::Data(Frame)
      }
   }
}

proc Limit::MoveInit { Frame VP } {
   variable Data

   if { $Viewport::Map(Type)=="grid" } {
      set Data(XD)   $Viewport::Map(GridICursor)
      set Data(YD)   $Viewport::Map(GridJCursor)
   } else {
      foreach field [concat $FSTD::Data(List) $FSTD::Data(ListTool)] {
         if { [FSTD::ParamGetMode $field]==$FSTD::Param(Spec) } {
            set temp [fstdfield stats $field -coordpoint $Viewport::Map(LatCursor) $Viewport::Map(LonCursor)]
         }
      }
      set Data(XD)   [lindex $temp 0]
      set Data(YD)   [lindex $temp 1]
   }
}

proc Limit::Move { Frame VP } {
   variable Data

   #----- Effectuer la translation

   if { $Viewport::Map(Type)=="grid" } {
      set deltaX [expr $Viewport::Map(GridICursor) - $Data(XD)]
      set deltaY [expr $Viewport::Map(GridJCursor) - $Data(YD)]
   } else {
      foreach field [concat $FSTD::Data(List) $FSTD::Data(ListTool)] {
         if { [FSTD::ParamGetMode $field]==$FSTD::Param(Spec) } {
            set temp [fstdfield stats $field -coordpoint $Viewport::Map(LatCursor) $Viewport::Map(LonCursor)]
         }
      }
      set deltaX [expr [lindex $temp 0] - $Data(XD)]
      set deltaY [expr [lindex $temp 1] - $Data(YD)]
   }

   if { $Data(Canvas)!="" } {
      $Data(Canvas) delete LIMIT
   }

   if { $Viewport::Map(Type)=="grid" } {
      set Data(XD)   $Viewport::Map(GridICursor)
      set Data(YD)   $Viewport::Map(GridJCursor)
   } else {
      foreach field [concat $FSTD::Data(List) $FSTD::Data(ListTool)] {
         if { [FSTD::ParamGetMode $field]==$FSTD::Param(Spec) } {
            set temp [fstdfield stats $field -coordpoint $Viewport::Map(LatCursor) $Viewport::Map(LonCursor)]
         }
      }
      set Data(XD)   [lindex $temp 0]
      set Data(YD)   [lindex $temp 1]
   }
   set Data(Canvas) $Frame.page.canvas
   set Data(Frame)  $Frame
   set Data(VP)     $VP
   Limit::MoveItems $Frame $deltaX $deltaY
}

proc Limit::MoveDone { Frame VP } {
   variable Data

   set Data(X0) [lindex $Data(PG0) 0]
   set Data(Y0) [lindex $Data(PG0) 1]
   set Data(X1) [lindex $Data(PG2) 0]
   set Data(Y1) [lindex $Data(PG2) 1]
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
# Creation : Aout 2019 - A. Germain - CMC
#
# But      : Effectuer le "Refresh" des items relatifs a cet outils sur
#            la projection.
#
# Parametres :
#   <Frame>  : Identificateur de Page
#
# Remarques :
#    - Cette fonctions est appele par SPI au besoin.
#
#-------------------------------------------------------------------------------

proc Limit::UpdateItems { Frame } {
   global   GDefs
   variable Data

   $Data(Canvas) delete LIMIT
   foreach field [concat $FSTD::Data(List) $FSTD::Data(ListTool)] {
      if { [FSTD::ParamGetMode $field]==$FSTD::Param(Spec) } {

         set Data(P0) [fstdfield stats $field -gridpoint $Data(X0) $Data(Y0)]
         set Data(P1) [fstdfield stats $field -gridpoint $Data(X1) $Data(Y0)]
         set Data(P2) [fstdfield stats $field -gridpoint $Data(X1) $Data(Y1)]
         set Data(P3) [fstdfield stats $field -gridpoint $Data(X0) $Data(Y1)]

      }
   }
   if { $Data(VP)!="" } {
      Viewport::DrawLine $Data(Frame) $Data(VP) "$Data(P0) 0 $Data(P1) 0 $Data(P2) 0 $Data(P3) 0 $Data(P0) 0" LIMIT blue 2 TRUE
   }
}

#-------------------------------------------------------------------------------
# Nom      : <Limit::MoveItems>
# Creation : Aout 2019 - A. Germain - CMC
#
# But      : Effectuer le "Refresh" des items relatifs a cet outils sur
#            la projection quand il bouge.
#
# Parametres :
#   <Frame>  : Identificateur de Page
#   <deltaX> : Deplacement en X
#   <deltaY> : Deplacement en Y
#
# Remarques :
#    - Cette fonctions est appele par SPI au besoin.
#
#-------------------------------------------------------------------------------

proc Limit::MoveItems { Frame deltaX deltaY } {
   global   GDefs
   variable Data

   foreach field [concat $FSTD::Data(List) $FSTD::Data(ListTool)] {
      if { [FSTD::ParamGetMode $field]==$FSTD::Param(Spec) } {

         set Data(PG0) [fstdfield stats $field -coordpoint [lindex $Data(P0) 0] [lindex $Data(P0) 1]]
         set Data(PG1) [fstdfield stats $field -coordpoint [lindex $Data(P1) 0] [lindex $Data(P1) 1]]
         set Data(PG2) [fstdfield stats $field -coordpoint [lindex $Data(P2) 0] [lindex $Data(P2) 1]]
         set Data(PG3) [fstdfield stats $field -coordpoint [lindex $Data(P3) 0] [lindex $Data(P3) 1]]

         lset Data(PG0) 0 [expr [lindex $Data(PG0) 0] + $deltaX]
         lset Data(PG0) 1 [expr [lindex $Data(PG0) 1] + $deltaY]
         lset Data(PG1) 0 [expr [lindex $Data(PG1) 0] + $deltaX]
         lset Data(PG1) 1 [expr [lindex $Data(PG1) 1] + $deltaY]
         lset Data(PG2) 0 [expr [lindex $Data(PG2) 0] + $deltaX]
         lset Data(PG2) 1 [expr [lindex $Data(PG2) 1] + $deltaY]
         lset Data(PG3) 0 [expr [lindex $Data(PG3) 0] + $deltaX]
         lset Data(PG3) 1 [expr [lindex $Data(PG3) 1] + $deltaY]

         set Data(P0) [fstdfield stats $field -gridpoint [lindex $Data(PG0) 0] [lindex $Data(PG0) 1]]
         set Data(P1) [fstdfield stats $field -gridpoint [lindex $Data(PG1) 0] [lindex $Data(PG1) 1]]
         set Data(P2) [fstdfield stats $field -gridpoint [lindex $Data(PG2) 0] [lindex $Data(PG2) 1]]
         set Data(P3) [fstdfield stats $field -gridpoint [lindex $Data(PG3) 0] [lindex $Data(PG3) 1]]

      }
   }

   $Data(Canvas) delete LIMIT

   if { $Data(VP)!="" } {
      Viewport::DrawLine $Data(Frame) $Data(VP) "$Data(P0) 0 $Data(P1) 0 $Data(P2) 0 $Data(P3) 0 $Data(P0) 0" LIMIT blue 2 TRUE
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
