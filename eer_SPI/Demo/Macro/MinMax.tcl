#===============================================================================
# Environnement Canada
# Centre Meteorologique Canadian
# 2100 Trans-Canadienne
# Dorval, Quebec
#
# Projet   : Exemples de scripts de productions de cartes.
# Fichier  : MinMax.tcl
# Creation : Fevrier 2001 - J.P.Gauthier - CMC/CMOE
#
# Description:
#    Calculer les valeurs minimum et maximum d'un champs sur la période de temps couverte dans le fichier.
#
# Arguments  :
#
# Remarques :
#
#===============================================================================

namespace eval Macro::MinMax {} {
   variable Param
   variable Data
   variable Error

   set Param(In)   ""
   set Param(Info) { "Calculer les valeurs minimum et maximum\nd'un champs sur la période de temps couverte\ndans le fichier."
                     "Calculate minimum and maximum values\nof a field over a the time period\ncovered int the file. " }

   set Data(Page) ""

   set Error(Field)  { "Aucun champs valide trouvé!\nVérifiez le viewport actif."
                       "Could not find any valid field!\nCheck the active viewport." }
}

proc Macro::MinMax::Execute { } {
   global GDefs
   variable Data
   variable Error

   set field [lindex [Viewport::Assigned $Page::Data(Frame) $Viewport::Data(VP)] 0]

   if { ![fstdfield is $field] } {
      Macro::Error $Error(Field)
      return
   }

   #----- Get the field params to look for
   set fid    [fstdfield define $field -FID]
   set ip1    [fstdfield define $field -IP1]
   set ip3    [fstdfield define $field -IP3]
   set etiket [fstdfield define $field -ETIKET]
   set type   [fstdfield define $field -TYPVAR]
   set var    [fstdfield define $field -NOMVAR]

   set Data(Min) 1e302
   set Data(Max) -1e302

   #----- Loop on all fields and figure out min/max
   foreach fld [fstdfield find $fid -1 $etiket $ip1 -1 $ip3 $type $var] {
      fstdfield read Macro::MINMAX $fid $fld

      set min [lindex [fstdfield stats Macro::MINMAX -min] 0]
      set max [lindex [fstdfield stats Macro::MINMAX -max] 0]

      set Data(Min) [expr $min<$Data(Min)?$min:$Data(Min)]
      set Data(Max) [expr $max>$Data(Max)?$max:$Data(Max)]
   }

   Macro::Info "Min: $Data(Min)\nMax: $Data(Max)"
}

proc Macro::MinMax::Clean { } {
   fstdfield free Macro::MINMAX
}









