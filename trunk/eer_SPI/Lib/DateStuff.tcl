#===============================================================================
# Environnement Canada
# Centre Meteorologique Canadian
# 2100 Trans-Canadienne
# Dorval, Quebec
#
# Projet    : Librairie de definitions pour les trajectoires
# Fichier   : DateStuff.tcl
# Creation  : Octobre 1998 - J.P. Gauthier - CMC/CMOE
#
# Description: Definitions de fonctions relative aux manipulations de dates.
#
# Fonctions:
#      DateStuff::CompareDDMMMYYYY { Date1 Date2 }
#      DateStuff::CheckYear        { Year }
#      DateStuff::StringMonth      { NoMois Lang }
#      DateStuff::MonthLength      { Month Year }
#
# Remarques :
#   Aucune
#
# Modification:
#
#   Nom         : -
#   Date        : -
#   Description : -
#
#=============================================================================

package provide DateStuff 1.0

catch { SPI::Splash "Loading Package DateStuff 1.0" }

namespace eval DateStuff {
   variable Data
   variable Day
   variable Month

   set Day(0) { "Dimanche" "Sunday"    }
   set Day(1) { "Lundi"    "Monday"    }
   set Day(2) { "Mardi"    "Tuesday"   }
   set Day(3) { "Mercredi" "Wednesday" }
   set Day(4) { "Jeudi"    "Thursday"  }
   set Day(5) { "Vendredi" "Friday"    }
   set Day(6) { "Samedi"   "Saturday"  }


   set Month(01) { "Janvier"   "January"  }
   set Month(02) { "Fevrier"   "February"  }
   set Month(03) { "Mars"      "March"     }
   set Month(04) { "Avril"     "April"     }
   set Month(05) { "Mai"       "May"       }
   set Month(06) { "Juin"      "June"      }
   set Month(07) { "Juillet"   "July"      }
   set Month(08) { "Aout"      "August"    }
   set Month(09) { "Septembre" "September" }
   set Month(10) { "Octobre"   "October"   }
   set Month(11) { "Novembre"  "November"  }
   set Month(12) { "Decembre"  "December"  }

   set Data(Jan) 01
   set Data(Feb) 02
   set Data(Mar) 03
   set Data(Apr) 04
   set Data(May) 05
   set Data(Jun) 06
   set Data(Jul) 07
   set Data(Aug) 08
   set Data(Sep) 09
   set Data(Oct) 10
   set Data(Nov) 11
   set Data(Dec) 12
}

#----------------------------------------------------------------------------
# Nom      : <DateStuff::CompareDDMMMYYYY>
# Creation : Juin 97 - J.P. Gauthier - CMC/CMOE -
#
# But      : Compare deux date de format DDMMMYYYY (ex: 12Jan1998)
#            et envoie un resultat a la maniere de Tcl.
#
# Parametres :
#    <Date1> : Premiere date.
#    <Date2> : Deuxieme date.
#
# Retour     :
#           -1 si date1 < date2
#            0 si date1 = date2
#            1 si date1 > date2
#
# Remarques :
#    aucune
#
#----------------------------------------------------------------------------

proc DateStuff::CompareDDMMMYYYY { Date1 Date2 } {
   variable Data

   #----- Extraction des champs

   set day1 [string range $Date1 0 1]
   set month1 [string range $Date1 3 5]
   set year1 [string range $Date1 7 10]

   set day2 [string range $Date2 0 1]
   set month2 [string range $Date2 3 5]
   set year2 [string range $Date2 7 10]

   #----- Conversion du mois et reconstruction de la date

   set month1 $Data($month1)
   set month2 $Data($month2)

   set Date1 $year1$month1$day1
   set Date2 $year2$month2$day2

   #----- Test de comparaison

   if { $Date1 == $Date2 } {
      return 0
   } else {
      if { $Date1 < $Date2 } {
         return -1
      } else {
         return 1
      }
   }
}

#----------------------------------------------------------------------------
# Nom      : <DateStuff::CompareMMMYYYY>
# Creation : Juin 97 - J.P. Gauthier - CMC/CMOE -
#
# But      : Compare deux date de format MMMYYYY (ex: Jan1998)
#            et envoie un resultat a la maniere de Tcl.
#
# Parametres :
#    <Date1> : Premiere date.
#    <Date2> : Deuxieme date.
#
# Retour     :
#           -1 si date1 < date2
#            0 si date1 = date2
#            1 si date1 > date2
#
# Remarques :
#    aucune
#
#----------------------------------------------------------------------------

proc DateStuff::CompareMMM_YYYY { Date1 Date2 } {
   variable Data

   #----- Extraction des champs

   set month1 [string range $Date1 0 2]
   set year1 [string range $Date1 4 end]

   set month2 [string range $Date2 0 2]
   set year2 [string range $Date2 4 end]

   #----- Conversion du mois et reconstruction de la date

   set month1 $Data($month1)
   set month2 $Data($month2)

   set Date1 $year1$month1
   set Date2 $year2$month2

   #----- Test de comparaison

   if { $Date1 == $Date2 } {
      return 0
   } else {
      if { $Date1 < $Date2 } {
         return -1
      } else {
         return 1
      }
   }
}

#-------------------------------------------------------------------------------
# Nom      : <DateStuff::CheckYear>
# Creation : Juillet 1996 - E. Bilodeau - CMC/CMOE -
#
# But      : verifier si l'annee selectionnee est bissectile ou pas, et de
#            retourner le nombre de jour du mois de fevrier selon le cas.
#
# Parametres :
#    <Year>  : annee.
#
# Retour :
#    Le nombre de jour du mois de fevrier.
#
# Remarques :
#    Aucune.
#
#-------------------------------------------------------------------------------

proc DateStuff::CheckYear { Year } {

   if {(($Year % 4 == 0) && ($Year % 100 != 0)) || ($Year % 400 == 0)} {
      return 29
   } else {
      return 28
   }
}

#----------------------------------------------------------------------------
# Nom      : <DateStuff::StringDay>
# Creation : Fevrier 1999 - J.P. Gauthier - CMC/CMOE
#
# But      : Retourne le jour de la semaine en caractere.
#
# Parametres :
#  <No>      : numero correspondant au jour.
#  <Lang>    : Langue dans laquelle on veut le mois (0-Francais 1-Anglais).
#
# Remarques :
#
#----------------------------------------------------------------------------

proc DateStuff::StringDay { No Lang } {
   variable Day

   return [lindex $Day($No) $Lang]
}

#----------------------------------------------------------------------------
# Nom      : <DateStuff::StringMonth>
# Creation : Juillet 1998 - S. Trudel - CMC/CMOE
#
# But      : Retourne le mois en caractere.
#
# Parametres :
#  <No>      : numero correspondant au mois.
#  <Lang>    : Langue dans laquelle on veut le mois (0-Francais 1-Anglais).
#
# Remarques :
#
#----------------------------------------------------------------------------

proc DateStuff::StringMonth { No Lang } {
   variable Month

   return [lindex $Month($No) $Lang]
}

#-------------------------------------------------------------------------------
# Nom      : <DateStuff::MonthLength>
# Creation : Juillet 1996 - E. Bilodeau - CMC/CMOE - 421 4642
#
# But      : Calcule la longueur d'un mois selon l'annee et le mois.
#
# Parametres :
#    <Month> : Mois a verifier
#    <Year>  : Annee du mois a verifier
#
# Remarques :
#    Aucune.
#
#-------------------------------------------------------------------------------

proc DateStuff::MonthLength { Month Year } {

   set nb_jour 0
   switch $Month {
      1   {set nb_jour 31}
      2   {set nb_jour [DateStuff::CheckYear $Year]}
      3   {set nb_jour 31}
      4   {set nb_jour 30}
      5   {set nb_jour 31}
      6   {set nb_jour 30}
      7   {set nb_jour 31}
      8   {set nb_jour 31}
      9   {set nb_jour 30}
     10   {set nb_jour 31}
     11   {set nb_jour 30}
     12   {set nb_jour 31}
   }
   return $nb_jour
}

#----------------------------------------------------------------------------
# Nom      : <DateStuff::StringDateFromSeconds>
# Creation : Mai 1999 - J.P. Gauthier - CMC/CMOE
#
# But      : Retourne la date selon un format usuel complet.
#
# Parametres :
#  <Seconds> : Secondes ecoule depuis le debut des temps (de l'ordinateur).
#  <Lang>    : Langue dans laquelle on veut le mois (0-Francais 1-Anglais).
#  <Zone>    : Zone (Defaut=Z)
#
# Remarques :
#
#----------------------------------------------------------------------------

proc DateStuff::StringDateFromSeconds { Seconds Lang { Zone Z } } {

   set Seconds [expr int($Seconds)]

   set jour [DateStuff::StringDay   [clock format $Seconds -format "%w" -gmt true] $Lang]
   set mois [DateStuff::StringMonth [clock format $Seconds -format "%m" -gmt true] $Lang]

   if { $Lang==0 } {
      set date "[clock format $Seconds -format "$jour %d $mois %Y à %H:%M$Zone" -gmt true]"
   } else {
      set date "[clock format $Seconds -format "$jour $mois %d %Y at %H:%M$Zone" -gmt true]"
   }

   return $date
}

proc DateStuff::StringDateOnlyFromSeconds { Seconds Lang { Zone Z } } {

   set Seconds [expr int($Seconds)]

   set jour [DateStuff::StringDay   [clock format $Seconds -format "%w" -gmt true] $Lang]
   set mois [DateStuff::StringMonth [clock format $Seconds -format "%m" -gmt true] $Lang]

   if { $Lang==0 } {
      set date "[clock format $Seconds -format "$jour %d $mois %Y" -gmt true]"
   } else {
      set date "[clock format $Seconds -format "$jour $mois %d %Y" -gmt true]"
   }

   return $date
}

proc DateStuff::StringShortDateFromSeconds { Seconds Lang { Zone Z } } {

   set Seconds [expr int($Seconds)]

   set jour [DateStuff::StringDay   [clock format $Seconds -format "%w" -gmt true] $Lang]
   set mois [DateStuff::StringMonth [clock format $Seconds -format "%m" -gmt true] $Lang]

   if { $Lang==0 } {
      set date "[clock format $Seconds -format "%d $mois %Y à %H:%M$Zone" -gmt true]"
   } else {
      set date "[clock format $Seconds -format "$mois %d %Y at %H:%M$Zone" -gmt true]"
   }

   return $date
}

proc DateStuff::StringShortDateOnlyFromSeconds { Seconds Lang { Zone Z } } {

   set Seconds [expr int($Seconds)]

   set mois [DateStuff::StringMonth [clock format $Seconds -format "%m" -gmt true] $Lang]

   if { $Lang==0 } {
      set date "[clock format $Seconds -format "%d $mois %Y" -gmt true]"
   } else {
      set date "[clock format $Seconds -format "$mois %d %Y" -gmt true]"
   }

   return $date
}

proc DateStuff::StringTimeFromSeconds { Seconds { Zone Z } } {

   set Seconds [expr int($Seconds)]

   set time "[clock format $Seconds -format "%H$Zone" -gmt true]"

   return $time
}
