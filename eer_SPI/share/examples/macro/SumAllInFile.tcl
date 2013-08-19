#===============================================================================
# Environnement Canada
# Centre Meteorologique Canadian
# 2100 Trans-Canadienne
# Dorval, Quebec
#
# Projet   : Exemples de scripts de productions de cartes.
# Fichier  : SumAllInFile.tcl
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

namespace eval Macro::SumAllInFile {} {
   variable Param
   variable Data
   variable Error

   set Param(In)   ""
   set Param(Info) { "Calculer la somme de tout les champs commun\ndans le fichier."
                     "Calculate the sum of all corresponding fields\nin the file. " }

   set Data(Page) ""

   set Error(Field)  { "Aucun champs valide trouvé!\nVérifiez le viewport actif."
                       "Could not find any valid field!\nCheck the active viewport." }
}

proc Macro::SumAllInFile::Execute { } {
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

   #----- Loop on all fields and figure out min/max
   foreach fld [fstdfield find $fid -1 $etiket $ip1 -1 $ip3 $type $var] {
      fstdfield read TMP $fid $fld

      if { [fstdfield is SUMALLINFILE] } {
         vexpr SUMALLINFILE SUMALLINFILE+TMP
      } else {
         fstdfield copy SUMALLINFILE TMP
      }
   }

   Viewport::UnAssign $Page::Data(Frame) $Viewport::Data(VP)
   Viewport::Assign $Page::Data(Frame) $Viewport::Data(VP) SUMALLINFILE
   Page::UpdateCommand $Page::Data(Frame)
}

proc Macro::SumAllInFile::Clean { } {
   Viewport::UnAssign $Page::Data(Frame) $Viewport::Data(VP) SUMALLINFILE
   fstdfield free SUMALLINFILE TMP
}
