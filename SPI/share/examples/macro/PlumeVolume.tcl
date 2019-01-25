#===============================================================================
# Environnement Canada
# Centre Meteorologique Canadian
# 2100 Trans-Canadienne
# Dorval, Quebec
#
# Projet   : Exemples de scripts de productions de cartes.
# Fichier  : PlumeVolume.tcl
# Creation : Fevrier 2001 - J.P.Gauthier - CMC/CMOE
#
# Description:
#    Calculer le volume d'un panache.
#
# Arguments  :
#
# Remarques :
#
#===============================================================================

namespace eval Macro::PlumeVolume {} {
   variable Param
   variable Data
   variable Error
   variable Msg

   set Param(In)   ""
   set Param(Info) { "Calculer le volume d'un panache."
                     "Calculate a plume's volume. " }

   set Msg(Volume)  { "Le volume du panache est: " "Plume voulme: " }
   
   set Error(Field)  { "Aucun champs valide trouvé!\nVérifiez le viewport actif."
                       "Could not find any valid field!\nCheck the active viewport." }
}

proc Macro::PlumeVolume::Execute { } {
   global GDefs
   variable Data
   variable Error
   variable Msg

   set field [lindex [Viewport::Assigned $Page::Data(Frame) $Viewport::Data(VP)] 0]

   if { ![fstdfield is $field] } {
      Macro::Error $Error(Field)
      return
   }

   #----- Get the field params to look for
   set fid    [fstdfield define $field -FID]
   set datev  [fstdfield define $field -DATEV]
   set ip2    [fstdfield define $field -IP2]
   set etiket [fstdfield define $field -ETIKET]
   set type   [fstdfield define $field -TYPVAR]
   set var    [fstdfield define $field -NOMVAR]

   vexpr AREA darea($field)
   set   volume 0.0
  
   #----- Loop on all fields and figure out min/max

   foreach fld [fstdfield find $fid $datev $etiket -1 $ip2 -1 $type $var] {
      fstdfield read TMP $fid $fld

      set top    [lindex [fstdgrid convip [fstdfield define TMP -IP3]] 0]
      set bottom [lindex [fstdgrid convip [fstdfield define TMP -IP1]] 0]

      Macro::Doing "Processing from $bottom to $top "
      
      set volume [vexpr - $volume+ssum(ifelse(TMP,AREA,0)*($top-$bottom))]
   }
   
   Macro::Info $Msg(Volume) [format "%.4e m³" $volume]
}

proc Macro::PlumeVolume::Clean { } {
   fstdfield free TMP AREA
}
