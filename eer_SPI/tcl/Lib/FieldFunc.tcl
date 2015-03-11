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
#    FieldFunc::MinMax        { Fields }
#    FieldFunc::TimeOfArrival { Since Fields { Treshold 0 } }
#
# Remarques :
#   Aucune
#
#===============================================================================

package provide FieldFunc 1.0

catch { SPI::Splash "Loading Package FieldFunc 1.0" }

namespace eval FieldFunc { } {
}

#----------------------------------------------------------------------------
# Nom      : <FieldFunc::ConvexHull>
# Creation : Juillet 2014 - J.P. Gauthier - CMC/CMOE
#
# But      : Calculer le convex hull d<un champs selon ses intervalles de contours ou minimum
#
# Parametres  :
#   <Field>   : Field to get hull for
#   <Res>     : Maximum Segment resolution
#   <Buffer>  : Buffer around the hull
#
# Retour:
#   <Coords>  : { lat lon elev lat lon elev ... }
#
# Remarques :
#
#----------------------------------------------------------------------------

proc FieldFunc::ConvexHull { Field { Res 0.5 } { Buffer 0.5 } } {
    
   ogrlayer free FFOGRLAYER FFHULL

   #----- Creer la couche avec le bon referentiel
   ogrlayer new FFOGRLAYER "Data" "Polygon"

   #----- Importer les donnees RPN dans la couche
   ogrlayer import FFOGRLAYER $Field True

   #----- Si c'est vide, on retourne
   if { ![ogrlayer define FFOGRLAYER -nb] } {
      return ""
   }

   #----- Dissolve into polygon masses
#      ogrlayer stats OGRLAYER -dissolve DISSOLVED
         
   ogrlayer stat FFOGRLAYER -convexhull FFHULL
   ogrlayer stat FFHULL -buffer $Buffer 1
   ogrlayer stat FFHULL -simplify $Res
   set coords {}
   catch {
      foreach { i j } [lrange [ogrgeometry define [ogrlayer define FFHULL -geometry 0 True] -sub 0 -points] 0 end-2] {
         set ll [fstdfield stats $Field -project $i $j]
         lappend coords [lindex $ll 0] [lindex $ll 1] 0.0
      }
   }
   ogrlayer free FFOGRLAYER FFHULL
   
   return $coords
}

#----------------------------------------------------------------------------
# Nom      : <FieldFunc::MinMax>
# Creation : Janvier 2014 - J.P. Gauthier - CMC/CMOE
#
# But      : Récupérer les min/max de tous les champs en paramêtres (sur une période)
#
# Parametres  :
#   <Fields>  : Field to look for or list of fields to process
#
# Retour:
#   <List>   : { min max }
#
# Remarques :
#
#----------------------------------------------------------------------------

proc FieldFunc::MinMax { Fields } {

   set fld [lindex $Fields 0]

   if { ![fstdfield is $fld] } {
      return
   }

   set min  1e32
   set max -1e32
 
   if { [llength $Fields]>1 } {
      foreach field $fields {
       
         set tmin [lindex [fstdfield stats MINMAXTMP -min] 0]
         set tmax [lindex [fstdfield stats MINMAXTMP -max] 0]

         set min [expr $min<$tmin?$min:$tmin]
         set max [expr $max>$tmax?$max:$tmax]         
      }
   } else {
      set tags [fstdfield stats $fld -tag]
      set box  [lindex $tags 2]

      #----- Get field info to look for
      set fid    [fstdfield define $fld -FID]
      set ip1    [fstdfield define $fld -IP1]
      set ip3    [fstdfield define $fld -IP3]
      set etiket [fstdfield define $fld -ETIKET]
      set var    [fstdfield define $fld -NOMVAR]
      
      #----- Get fields from fieldbox if interactive, else from file itself
      if { ![llength [set fields [FieldBox::GetContent $box]]] } {
         set fields [fstdfile info $fid SPI $var]
      }
      set n 0
      set nx [llength $fields]

      foreach field $fields {

         set fid     [lindex $field end-5]
         set idx     [lindex $field end-4]

         set tvar    [lindex $field 0]
         set tip1    [lindex $field end-3]
         set tip3    [lindex $field end-1]
         set tetiket [string trim [lindex $field 8]]

         SPI::Progress [expr double([incr n])/$nx*100] "Processing fields ($idx)"

         if { $var==$tvar && $ip1==$tip1 && $ip3==$tip3 && $etiket==$tetiket } {
            fstdfield read MINMAXTMP $fid $idx

            set tmin [lindex [fstdfield stats MINMAXTMP -min] 0]
            set tmax [lindex [fstdfield stats MINMAXTMP -max] 0]

            set min [expr $min<$tmin?$min:$tmin]
            set max [expr $max>$tmax?$max:$tmax]         
         }
      }
   }
   SPI::Progress 0
   
   return [list $min $max]
}

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

   set fld [lindex $Fields 0]

   if { ![fstdfield is $fld] } {
      return
   }

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
      set tags [fstdfield stats $fld -tag]
      set box  [lindex $tags 2]

      #----- Get field info to look for
      set ip1    [fstdfield define $fld -IP1]
      set ip3    [fstdfield define $fld -IP3]
      set etiket [fstdfield define $fld -ETIKET]
      set var    [fstdfield define $fld -NOMVAR]

      set fields [FieldBox::GetContent $box]
      set n 0
      set nx [llength $fields]

      foreach field $fields {

         set fid     [lindex $field end-5]
         set idx     [lindex $field end-4]

         set tvar    [lindex $field 0]
         set tip1    [lindex $field end-3]
         set tip3    [lindex $field end-1]
         set tetiket [string trim [lindex $field 8]]

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
      fstdfield define TOAFIELD -IP1 $ip1 -IP3 $ip3 -ETIKET $etiket
   }

   #----- Make sure we can show from T0 by setting notime data to -1
   vexpr (Int32)TOAFIELD ifelse(TOAFIELD==0.0,-1,TOAFIELD)

   fstdfield define TOAFIELD -NOMVAR TOA -IP2 0
   fstdfield free TOAMAX TOATMP TOACHG

   catch { SPI::Progress 0 }
   return TOAFIELD
}
