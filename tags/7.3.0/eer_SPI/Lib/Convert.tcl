#===============================================================================
# Environnement Canada
# Centre Meteorologique Canadian
# 2100 Trans-Canadienne
# Dorval, Quebec
#
# Projet    : Librairie de definitions de filtres de conversions
# Fichier   : Convert.tcl
# Creation  : Octobre 1998 - J.P. Gauthier - CMC/CMOE
#
# Description: Definitions de diverses fonctions de conversions.
#
# Fonctions:
#    Convert::FormatCoord    { Lat Lon Prec }
#    Convert::FormatDist     { Dist Prec }
#    Convert::FormatGrid     { I J Prec }
#    Convert::Decimal2Minute { Value { Prec 3 } { List False } }
#    Convert::Meter2Millibar { Value }
#    Convert::Millibar2Meter { Value }
#    Convert::Minute2Decimal { Value }
#    Convert::ModuloVal      { Val Mod Sens }
#    Convert::Set2Digit      { Nb }
#
# Remarques :
#   Aucune
#
#===============================================================================

package provide Convert 1.0

proc IdConvert { Show } {

   if { $Show } {
      puts "(INFO) Loading Standard CMC/CMOE package Convert Version 1.0"
   }
}

namespace eval Convert { } {
   variable Lbl
   variable Unit

   set Lbl(1e24)  "Yotta"
   set Lbl(1e21)  "Zetta"
   set Lbl(1e18)  "Exa"
   set Lbl(1e15)  "Peta"
   set Lbl(1e12)  "Tera"
   set Lbl(1e9)   "Giga"
   set Lbl(1e6)   "Mega"
   set Lbl(1e3)   "Kilo"
   set Lbl(1e2)   "Hecto"
   set Lbl(1e1)   "Deca"
   set Lbl(1e-1)  "Deci"
   set Lbl(1e-2)  "Centi"
   set Lbl(1e-3)  "Milli"
   set Lbl(1e-6)  "Micro"
   set Lbl(1e-9)  "Nano"
   set Lbl(1e-12) "Pico"
   set Lbl(1e-15) "Femto"
   set Lbl(1e-18) "Atto"
   set Lbl(1e-21) "Zepto"
   set Lbl(1e-24) "Yocto"

   set Lbl(1852)  "Nautical mile"

   set Lbl(DistL)   { "metres" "meters" }
   set Lbl(DistU)   { "Metres" "Meters" }

   set Unit(1e24)  "Y"
   set Unit(1e21)  "Z"
   set Unit(1e18)  "E"
   set Unit(1e15)  "P"
   set Unit(1e12)  "T"
   set Unit(1e9)   "G"
   set Unit(1e6)   "M"
   set Unit(1e3)   "k"
   set Unit(1e2)   "h"
   set Unit(1e1)   "da"
   set Unit(1e0)   ""
   set Unit(1e-1)  "d"
   set Unit(1e-2)  "c"
   set Unit(1e-3)  "m"
   set Unit(1e-6)  "u"
   set Unit(1e-9)  "n"
   set Unit(1e-12) "p"
   set Unit(1e-15) "f"
   set Unit(1e-18) "a"
   set Unit(1e-21) "z"
   set Unit(1e-24) "Y"

   set Unit(1852)  "n"
}

#----------------------------------------------------------------------------
# Nom      : <Convert::FormatCoord>
# Creation : Septembre 1999 - J.P. Gauthier - CMC/CMOE
#
# But      : Formatte les coordonnees lat-lon +- en N S E W.
#
# Parametres :
#  <Lat>     : Coordonnne en latitude
#  <Lon>     : Coordonnne en longitude
#  <Type>    : Type de coordonnees (DEG ou MIN)
#  <Prec>    : Precision decimale
#
# Retour     :
#  <Coord>   : Chaine formattee des coordonnees
#
# Remarques :
#
#----------------------------------------------------------------------------

proc Convert::FormatCoord { Lat Lon Type { Prec 3 } } {

   #----- Si les coordonnees sont hors du globe

   if { $Lon<-180 } {
      set Lon [expr $Lon+360]
   }

   if { $Lon>180 } {
      set Lon [expr $Lon-360]
   }

   #----- Determiner la direction

   if { $Lat > 90.0 || $Lat < -90.0 || $Lon > 180.0 || $Lon < -180.0} {
      return " -      - "
   } else {
      if { $Lat < 0 } {
         set dir_lat "S"
      } else {
         set dir_lat "N"
      }
      if { $Lon < 0 } {
         set dir_lon "W"
      } else {
         set dir_lon "E"
      }

      #----- Remettre en positif

      set Lat [expr abs($Lat)]
      set Lon [expr abs($Lon)]

      #----- Formater au type de coordonnees

      if { $Type == "MIN" } {
         set Lat "[Convert::Decimal2Minute $Lat $Prec]"
         set Lon "[Convert::Decimal2Minute $Lon $Prec]"
      } else {
         eval set Lat \[format \"%.${Prec}f\" $Lat\]
         eval set Lon \[format \"%.${Prec}f\" $Lon\]
      }

      return "$Lat $dir_lat $Lon $dir_lon"
   }
}

#----------------------------------------------------------------------------
# Nom      : <Convert::FormatDist>
# Creation : Decembre 2002 - J.P. Gauthier - CMC/CMOE
#
# But      : Formatter une distance en metres
#
# Parametres :
#  <Dist>    : Distance en metres
#  <Prec>    : Precision decimale
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc Convert::FormatDist { Dist { Prec 2 } { Nautical False } } {
   variable Unit

   if { $Nautical } {
      set u 1852
   } else {
      #----- Calcul du plus proche gradient

      if { $Dist<1e-30 || $Dist>1e30} {
         return "0 m"
      }

      #----- Determiner l'amplitude

      set o "1e[expr int(log10($Dist))]"

      if { $o > 100 } {
         set u 1e3
      } elseif { $o > 1e0 } {
         set u 1e0
      } elseif { $o > 1e-2 } {
         set u 1e-2
      } elseif { $o > 1e-3 } {
         set u 1e-3
      } elseif { $o > 1e-6 } {
         set u 1e-6
      } elseif { $o > 1e-9 } {
         set u 1e-9
      } elseif { $o > 1e-12 } {
         set u 1e-12
      } elseif { $o > 1e-15 } {
         set u 1e-15
      } elseif { $o > 1e-18 } {
         set u 1e-18
      } elseif { $o > 1e-21 } {
         set u 1e-21
      } else {
         set u 1e-24
      }
   }

   eval set dist \[format \"%.${Prec}f\" [expr $Dist/$u]\]

   return "$dist $Unit($u)m"
}

#----------------------------------------------------------------------------
# Nom      : <Convert::FormatGrid>
# Creation : Decembre 2002 - J.P. Gauthier - CMC/CMOE
#
# But      : Formatter une coordonnee point de grille
#
# Parametres :
#  <I>       : Coordonnee en x
#  <J>       : Coordonnee en y
#  <Prec>    : Precision decimale
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc Convert::FormatGrid { I J { Prec 4 } } {

   if { $I==-1 || $J==-1 } {
      set coord " -      - "
   } else {
      eval set coord \[format \"%.${Prec}f %.${Prec}f\" $I $J\]
   }
   return $coord
}

#-------------------------------------------------------------------------------
# Nom      : <Convert::Decimal2Minute>
# Creation : Juillet 1996 - E. Bilodeau - CMC/CMOE - 421 4642
#
# But      : Convertir les degrees decimal en degrees minute.
#
# Parametres :
#   <Value>  : Valeur a convertir en minute
#   <Prec>   : Precision des secondes
#   <List>   : Format liste
#
# Retour     :
#   <val>    : Valeur en minute
#
# Remarques :
#    Aucune.
#
#-------------------------------------------------------------------------------

proc Convert::Decimal2Minute { Value { Prec 3 } { List False } } {

   # ----- Divise les deux avant le point et les deux chiffres apres le point.

   set deg [file rootname $Value]
   set dec 0[file extension $Value]

   if { $deg=="" } {
      set deg 0
   }
   #----- Conversion des deux chiffres apres le point.

   set min [expr $dec*60.0]
   set sec [format "%02.${Prec}f" [expr 0[file extension $min]*60.0]]
   set min [format "%02i" [file rootname $min]]

   #----- Reconstruire la valeur de la latitude a afficher.

   if { $List } {
      return "$deg $min $sec"
   } else {
      return "$deg°$min'$sec\""
   }
}

#-------------------------------------------------------------------------------
# Nom      : <Convert::Minute2Decimal>
# Creation : Juillet 1998 - J.P. Gauthier - CMC/CMOE - 421 4642
#
# But      : Convertir une valeur minute en valeur decimal pour les
#            latitudes et les longitudes.
#
# Parametres :
#   <Value>  : Valeur a convertir en decimal.
#
# Retour     :
#   <val>    : Valeur convertie en decimal.
#
# Remarques :
#    Aucune.
#
#-------------------------------------------------------------------------------

proc Convert::Minute2Decimal { Value } {

   set i 0

   #----- Extraire les deux parties

   set Value [split [string map { "°" " " \' " " \" " "} $Value]]
   set deg [string trimleft [lindex $Value 0] 0]
   set min [string trimleft [lindex $Value 1] 0]
   set sec [string trimleft [lindex $Value 2] 0]

   #----- Mettre a zero si inexistant

   if { $min=="" } {
      set min 0
   }
   if { $deg=="" } {
      set deg 0
   }
   if { $sec=="" } {
      set sec 0
   }

   #----- Calculer la valeur convertie

   return [expr $deg>=0?($deg+($min/60.0)+($sec/3600.0)):($deg-($min/60.0)-($sec/3600.0))]
}

#-------------------------------------------------------------------------------
# Nom      : <Convert::Meter2Millibar>
# Creation : Juillet 1996 - E. Bilodeau - CMC/CMOE - 421 4642
#
# But      : Conversion de la valeur de metre en millibar.
#
# Parametres :
#   <Value>  : Valeur en metres
#
# Retour     :
#   <result> : Valeur en millibars
#
# Remarques :
#    Aucune.
#
#-------------------------------------------------------------------------------

proc Convert::Meter2Millibar { Value } {

   if { $Value>32000 } {
      set res [expr 8.680157*pow(1+1.224579e-5*($Value-32000),-12.203121)]
   } elseif { $Value>20000 } {
      set res [expr 54.749*pow(1+4.61574e-6*($Value-20000),-34.16326)]
   } elseif { $Value>11000 } {
      set res [expr 226.3203*exp(-1.57688e-4*($Value-11000))]
   } else {
      set res [expr 1013.25*pow(1-2.25577e-5*$Value,5.255588)]
   }
   return $res
}

#-------------------------------------------------------------------------------
# Nom      : <Convert::Millibar2Meter>
# Creation : Juillet 1996 - E. Bilodeau - CMC/CMOE - 421 4642
#
# But      : Conversion de la valeur de millibar en metre.
#
# Parametres :
#   <Value>  : Valeur en millibars
#
# Retour     :
#   <result> : Valeur en metres
#
# Remarques :
#    Aucune.
#
#-------------------------------------------------------------------------------

proc Convert::Millibar2Meter { Value } {

   if { $Value>226.3203 } {
      set res [expr 44330.8*(1-pow($Value/1013.25,0.190263))]
   } elseif { $Value>54.749 } {
      set res [expr 11000-6341.624*ln($Value/226.3202)]
   } elseif { $Value>8.68 } {
      set res [expr 20000+216650*(pow($Value/54.749,-0.0292173)-1)]
   } elseif { $Value>0 } {
      set res [expr 32000+81660.7*(pow($Value/8.680157,-0.0819469)-1)]
   } else {
      set res 0
   }
   return $res
}

#-------------------------------------------------------------------------------
# Nom      : <Convert::ModuloVal>
# Creation : Janvier 1998 - J.P. Gauthier - CMC/CMOE - 421 4642
#
# But      : Ajuste par rapport a un modulo.
#
# Parametres :
#    <Val>   : Valeur a ajuste.
#    <Mod>   : Modulo par lequel ajuste.
#    <Sens>  : Ajuste par en haut (+), en bas (-) ou le plus pres (0).
#
# Retour     :
#    <val>   : Valeur ajustee
#
# Remarques :
#    Aucune.
#
#-------------------------------------------------------------------------------

proc Convert::ModuloVal { Val Mod Sens } {

   if { $Val != 0 } {
      set Val [string trimleft $Val 0]
   }
   set result [expr $Val - ($Val % $Mod)]

   if { [expr $Val - $result] != 0 } {
      if { $Sens == "+" } {
            set result [expr $result + $Mod]
      } elseif { $Sens == "0" } {
         if { [expr $Val % $Mod] >= [expr $Mod / 2] } {
            set result [expr $result + $Mod]
         }
      }
   }
   return $result
}

#-------------------------------------------------------------------------------
# Nom      : <Convert::Set2Digit>
# Creation : Janvier 98 - S. Trudel - CMC/CMOE
#
# But      : Formater un nombre sur 2 chiffre.
#
# Parametres :
#    <Nb>    : Nombre a formatte
#
# Retour     :
#    <Nb>    : Nombre formatte
#
# Remarques :
#    Aucune.
#
#-------------------------------------------------------------------------------

proc Convert::Set2Digit { Nb } {

   if { $Nb > 0 } {
      set Nb [string trimleft $Nb 0]
   }

   return [format "%02d" ${Nb}]
}
