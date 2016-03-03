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
#      DateStuff::CompareDDMMMYYYY               { Date1 Date2 }
#      DateStuff::CompareMMM_YYYY                { Date1 Date2 }
#      DateStuff::IsLeapYear                     { Year }
#      DateStuff::StringDay                      { No Lang }
#      DateStuff::StringMonth                    { No Lang }
#      DateStuff::MonthLength                    { Month Year }
#      DateStuff::StringDateFromSeconds          { Seconds Lang { Zone Z } }
#      DateStuff::StringDateOnlyFromSeconds      { Seconds Lang { Zone Z } }
#      DateStuff::StringShortDateFromSeconds     { Seconds Lang { Zone Z } }
#      DateStuff::StringShortDateOnlyFromSeconds { Seconds Lang { Zone Z } }
#      DateStuff::StringTimeFromSeconds          { Seconds { Zone Z } }
#      DateStuff::SecsToHours                    { Secs }
#      DateStuff::MinsToHours                    { Mins }
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
   variable Const

   set Const(MonthLength) { "" 31 29 31 30 31 30 31 31 30 31 30 31 }
   
   set Day(0) { "Dimanche" "Sunday"    }
   set Day(1) { "Lundi"    "Monday"    }
   set Day(2) { "Mardi"    "Tuesday"   }
   set Day(3) { "Mercredi" "Wednesday" }
   set Day(4) { "Jeudi"    "Thursday"  }
   set Day(5) { "Vendredi" "Friday"    }
   set Day(6) { "Samedi"   "Saturday"  }


   set Month(01) { "Janvier"   "January"  }
   set Month(02) { "Février"   "February"  }
   set Month(03) { "Mars"      "March"     }
   set Month(04) { "Avril"     "April"     }
   set Month(05) { "Mai"       "May"       }
   set Month(06) { "Juin"      "June"      }
   set Month(07) { "Juillet"   "July"      }
   set Month(08) { "Aout"      "August"    }
   set Month(09) { "Septembre" "September" }
   set Month(10) { "Octobre"   "October"   }
   set Month(11) { "Novembre"  "November"  }
   set Month(12) { "Décembre"  "December"  }

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
# Nom      : <DateStuff::CompareMMM_YYYY>
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
# Nom      : <DateStuff::IsLeapYear>
# Creation : Juillet 1996 - E. Bilodeau - CMC/CMOE -
#
# But      : Verifier si l'annee selectionnee est bissectile ou pas.
#
# Parametres :
#    <Year>  : annee.
#
# Retour :
#    <Bool>  : Booleen.
#
# Remarques :
#    Aucune.
#
#-------------------------------------------------------------------------------

proc DateStuff::IsLeapYear { Year } {

   return [expr (($Year%4==0) && ($Year%100!=0)) || ($Year%400==0)]
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
   variable Const   
   
   if { $Month==2 } {
      set nb_jour [expr [DateStuff::IsLeapYear $Year]?29:28]
   } else {
      set nb_jour [lindex $Const(MonthLength) $Month]   
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

   set jour [DateStuff::StringDay   [clock format $Seconds -format "%w" -timezone :UTC] $Lang]
   set mois [DateStuff::StringMonth [clock format $Seconds -format "%m" -timezone :UTC] $Lang]

   if { $Lang==0 } {
      set date "[clock format $Seconds -format "$jour %d $mois %Y Ã  %H:%M$Zone" -timezone :UTC]"
      set date [string totitle $date 0 end-[string length $Zone]]
   } else {
      set date "[clock format $Seconds -format "$jour $mois %d %Y at %H:%M$Zone" -timezone :UTC]"
   }

   return $date
}

proc DateStuff::StringDateOnlyFromSeconds { Seconds Lang { Zone Z } } {

   set Seconds [expr int($Seconds)]

   set jour [DateStuff::StringDay   [clock format $Seconds -format "%w" -timezone :UTC] $Lang]
   set mois [DateStuff::StringMonth [clock format $Seconds -format "%m" -timezone :UTC] $Lang]

   if { $Lang==0 } {
      set date "[clock format $Seconds -format "$jour %d $mois %Y" -timezone :UTC]"
      set date [string totitle $date 0 end-[string length $Zone]]
   } else {
      set date "[clock format $Seconds -format "$jour $mois %d %Y" -timezone :UTC]"
   }

   return $date
}

proc DateStuff::StringShortDateFromSeconds { Seconds Lang { Zone Z } } {

   set Seconds [expr int($Seconds)]

   set jour [DateStuff::StringDay   [clock format $Seconds -format "%w" -timezone :UTC] $Lang]
   set mois [DateStuff::StringMonth [clock format $Seconds -format "%m" -timezone :UTC] $Lang]

   if { $Lang==0 } {
      set date "[clock format $Seconds -format "%d $mois %Y Ã  %H:%M$Zone" -timezone :UTC]"
      set date [string totitle $date 0 end-[string length $Zone]]
   } else {
      set date "[clock format $Seconds -format "$mois %d %Y at %H:%M$Zone" -timezone :UTC]"
   }

   return $date
}

proc DateStuff::StringShortDateOnlyFromSeconds { Seconds Lang { Zone Z } } {

   set Seconds [expr int($Seconds)]

   set mois [DateStuff::StringMonth [clock format $Seconds -format "%m" -timezone :UTC] $Lang]

   if { $Lang==0 } {
      set date "[clock format $Seconds -format "%d $mois %Y" -timezone :UTC]"
      set date [string totitle $date 0 end-[string length $Zone]]
   } else {
      set date "[clock format $Seconds -format "$mois %d %Y" -timezone :UTC]"
   }

   return $date
}

proc DateStuff::StringTimeFromSeconds { Seconds { Zone Z } } {

   set Seconds [expr int($Seconds)]

   set time "[clock format $Seconds -format "%H$Zone" -timezone :UTC]"

   return $time
}

proc DateStuff::SecsToHours { Secs } {
    return [string trimright [format "%.4f" [expr {$Secs/3600.0}]] ".0"]
}

proc DateStuff::MinsToHours { Mins } {
    return [string trimright [format "%.2f" [expr {$Mins/60.0}]] ".0"]
}
