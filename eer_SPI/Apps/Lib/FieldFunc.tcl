#===============================================================================
# Environnement Canada
# Centre Meteorologique Canadian
# 2100 Trans-Canadienne
# Dorval, Quebec
#
# Projet    : Librairie de definitions pour les domaines des projections
# Fichier   : FieldFunc.tcl
# Creation  : Janvier 2004 - J.P. Gauthier - CMC/CMOE
#
# Description: Diverse fonctions et calculs sur les champs
#
# Fonctions:
#
# Remarques :
#   Aucune
#
#===============================================================================

package provide FieldFunc 1.0

catch { SPI::Splash "Loading Widget Package FieldFunc 1.0" }

namespace eval FieldFunc { }

#----------------------------------------------------------------------------
# Nom      : <FieldFunc::TimeOfArrival>
# Creation : Fevrier 2010 - J.P. Gauthier - CMC/CMOE
#
# But      : Calculer le temps d'arrive d'un panache en secondes.
#
# Parametres  :
#   <Since>   : Initial date in seconds
#   <Fields>  : Field to look for or list of fields to process
#   <Treshold>: Concentration treshold value for arrival time (-1=Maximum)
#
# Retour:
#   <TOA>     : Time of arrival field
#
# Remarques :
#
#----------------------------------------------------------------------------

proc FieldFunc::TimeOfArrival { Since Fields { Treshold 0 } } {

   set fld  [lindex $Fields 0]
   set tags [fstdfield stats $fld -tag]
   set box  [lindex $tags 2]

   #----- Get field info to look for
   set ip1    [fstdfield define $fld -IP1]
   set ip3    [fstdfield define $fld -IP3]
   set etiket [fstdfield define $fld -ETIKET]
   set var    [fstdfield define $fld -NOMVAR]

   #----- Make a copy of the fields
   fstdfield copy  TOAMAX $fld
   fstdfield clear TOAMAX 0.0

   fstdfield copy  TOAFIELD $fld
   fstdfield stats TOAFIELD -nodata -1
   fstdfield clear TOAFIELD -1

   if { [llength $Fields]>1 } {

      foreach field $Fields {
         set t [expr [fstdstamp toseconds [fstdfield define $field -DATEV]]-$Since+1]

         if { $Treshold<0 } {
            fstdfield copy TOACHG TOAMAX
            vexpr TOAMAX max(TOAMAX,$field)
            vexpr TOAFIELD ifelse($field!=0.0 && $field==TOAMAX && TOACHG!=TOAMAX,$t,TOAFIELD)
         } else {
            vexpr TOAFIELD ifelse(TOAFIELD==-1.0 && $field>$Treshold,$t,TOAFIELD)
         }
      }
   } else {
      set fields [FieldBox::GetContent $box]
      set n 0
      set nx [llength $fields]

      foreach field $fields {

         set fid     [lindex $field 0]
         set idx     [lindex $field 1]
         set tvar    [lindex $field 2]
         set tip1    [lindex $field 4]
         set tip3    [lindex $field 6]
         set tetiket [string trim [lindex $field 7]]

         SPI::Progress [expr double([incr n])/$nx*100] "Processing fields ($idx)"

         if { $var==$tvar && $ip1==$tip1 && $ip3==$tip3 && $etiket==$tetiket } {
            fstdfield read TOATMP $fid $idx

            set t [expr [fstdstamp toseconds [fstdfield define TOATMP -DATEV]]-$Since+1]

            if { $Treshold<0 } {
               fstdfield copy TOACHG TOAMAX
               vexpr TOAMAX max(TOAMAX,TOATMP)
               vexpr TOAFIELD ifelse(TOAFIELD && TOATMP!=0.0 && TOATMP==TOAMAX && TOACHG!=TOAMAX,$t,TOAFIELD)
            } else {
               vexpr TOAFIELD ifelse(TOAFIELD==-1.0 && TOATMP>$Treshold,$t,TOAFIELD)
            }
         }
      }
   }

   #----- Make sure we can show from T0 by setting notime data to -1
   vexpr (Int32)TOAFIELD ifelse(TOAFIELD==0.0,-1,TOAFIELD)

   fstdfield define TOAFIELD -NOMVAR TOA -IP1 $ip1 -IP2 0 -IP3 $ip3 -ETIKET $etiket
   fstdfield free TOAMAX TOATMP TOACHG

   return TOAFIELD
}
