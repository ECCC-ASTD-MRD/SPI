#===============================================================================
# Environnement Canada
# Centre Meteorologique Canadian
# 2100 Trans-Canadienne
# Dorval, Quebec
#
# Projet   : Exemples de scripts de productions de cartes.
# Fichier  : SimpleHull.tcl
# Creation : Fevrier 2009 - J.P.Gauthier - CMC/CMOE
#
# Description:
#    Produire des convex hull et les images associees
#
# Arguments  :
#
# Remarques :
#
#===============================================================================

namespace eval Macro::SimpleHull {} {
   variable Param
   variable Data
   variable Error

   
   set Param(Info)      { "Calcul de convex hull et images associées."
                          "Calculates cinvex hulls and associated images" }
}

proc Macro::SimpleHull::Execute { } {
   variable Data
   variable Error

   if { [fstdfield is [set field [lindex [Viewport::Assigned $Page::Data(Frame) $Viewport::Data(VP)] 0]]] } {

      set pts [FieldFunc::ConvexHull $field]
      puts stderr [expr [llength $pts]/3]:$pts
      Drawing::DrawPoly $Page::Data(Frame) $Viewport::Data(VP) $pts red 3 False "" "" False HULL
   }

   Macro::Doing ""
   Macro::Cursor left_ptr

}

proc Macro::SimpleHull::Clean { } {

}
