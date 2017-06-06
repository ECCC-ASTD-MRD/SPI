#===============================================================================
# Environnement Canada
# Centre Meteorologique Canadian
# 2100 Trans-Canadienne
# Dorval, Quebec
#
# Projet    : Librairie de fonctions relatives aux fichiers meteorologiques
# Fichier   : MetData.tcl
# Creation  : Novembre 2001 - J.P. Gauthier - CMC/CMOE
#
# Description: Definitions de fonctions relatives aux fichiers meteorologiques.
#
# Fonctions:
#
#   MetData::TextCode         { Text }
#   MetData::TextDecode       { Field }
#   MetData::Duration         { List Idx0 Idx1 }
#   MetData::GetLatestRun     { Path }
#   MetData::GetLatestStamp   { Path }
#   MetData::GetClosestFile   { Path Sec { Max 3600 } { Cache False } }
#   MetData::StampFromFile    { File }
#   MetData::SecFromFile      { File }
#   MetData::StampModulo      { Stamp Sec }
#   MetData::File2Sec         { File }
#   MetData::Find             { FLD FID DATEV ETIKET IP1 IP2 IP3 TYPVAR NOMVAR }
#   MetData::FindAll          { FLD FID DATEV ETIKET IP1 IP2 IP3 TYPVAR NOMVAR }
#   MetData::FormatDATEV      { Field { Min False } }
#   MetData::GridDefineUTM    { Lat0 Lon0 Lat1 Lon1 Res { Field "" } }
#   MetData::GridDefineLL     { Lat0 Lon0 Lat1 Lon1 DLat DLon { ETIKET GRID }}
#   MetData::GridDefinePS     { Scale NI NJ Lat Lon { Field "" } }
#   MetData::GridLLDelta      { Lat0 Lon0 Lat1 Lon1 Delta }
#   MetData::ListIP2          { Index Var { Stamp 0 } }
#   MetData::Profile          { Stamp Var File Lat Lon { From 0 } { To end } }
#   MetData::Obukhov          { Stamp File Lat Lon }
#   MetData::ObukhovCalculate { Stamp File Lat Lon }
#
# Remarques :
#   Aucune
#
#===============================================================================

package provide MetData 1.2

catch { SPI::Splash "Loading Widget Package MetData 1.2" }

namespace eval MetData { } {
   global env
   variable Const

   #----- Lire les diverses definitions

   catch {
      source $GDefs(Dir)/data/AEGL.tcl
      source $GDefs(Dir)/data/ERPG.tcl
   }

   set Const(Deg2Rad)     0.017453292519943295474371680598
   set Const(PIRad)       3.141592653589793238462643383279503
   set Const(EarthRadius) 6371005.0
}

#----------------------------------------------------------------------------
# Nom      : <MetData::TextCode>
# Creation : Aout 2005 - J.P. Gauthier - CMC/CMOE
#
# But      : Encoder une chaine de caractere dans un champs
#
# Parametres :
#  <Text>    : Text a encoder
#
# Retour     :
#  <Field>   : Champs encode
#
# Remarques :
#
#----------------------------------------------------------------------------

proc MetData::TextCode { Text } {

   if { [set len [string length $Text]]<=16777215 } {
      fstdfield free METDATATEXTCODE

      if { [string is ascii $Text] } {
         fstdfield create METDATATEXTCODE $len 1 1 UByte
         fstdfield define METDATATEXTCODE -DEET 0 -NPAS 0 -IP1 0 -IP2 0 -IP3 0 -ETIKET "ASCII" -NOMVAR "TEXT" -TYPVAR X -EX3 0 -GRTYP X
      } else {
         fstdfield create METDATATEXTCODE $len 1 1 UInt32
         fstdfield define METDATATEXTCODE -DEET 0 -NPAS 0 -IP1 0 -IP2 0 -IP3 0 -ETIKET "UNICODE" -NOMVAR "TEXT" -TYPVAR X -EX3 1 -GRTYP X
      }
      fstdfield configure METDATATEXTCODE -interpdegree NEAREST
      for { set i 0 } { $i<$len } { incr i } {
         scan [string index $Text $i] "%c" code
         fstdfield stats METDATATEXTCODE -gridvalue $i 0 $code
      }
   }
   return METDATATEXTCODE
}

#----------------------------------------------------------------------------
# Nom      : <MetData::TextDeCode>
# Creation : Aout 2005 - J.P. Gauthier - CMC/CMOE
#
# But      : Encoder une chaine de caractere dans un champs
#
# Parametres :
#  <Field>   : Champs a decoder
#
# Retour     :
#  <Text>    : Text decode
#
# Remarques :
#
#----------------------------------------------------------------------------

proc MetData::TextDecode { Field } {

   set text ""
   set nbit [fstdfield define $Field -NBITS]
   
   for { set i 0 } { $i<[fstdfield define $Field -NI] } { incr i } {
      set v [fstdfield stats $Field -gridvalue $i 0]
      append text [format "%c" [expr int(($nbit==8 && $v>127)?$v-128:$v)]]
   }

   return $text
}

#----------------------------------------------------------------------------
# Nom      : <MetData::Duration>
# Creation : Mars 2003 - J.P. Gauthier - CMC/CMOE
#
# But      : Calculer le temps de simulation en heures.
#
# Parametres :
#  <List>    : Liste des donnees meteo
#  <Idx0>    : Index de depart dans la liste
#  <Idx1>    : Index de fin dans la liste
#
# Retour     :
#  <Hours>   : Nombre d'heure
#
# Remarques :
#
#----------------------------------------------------------------------------

proc MetData::Duration { List Idx0 Idx1 } {
   variable Sim

   if { $Idx0!="" && $Idx1!="" } {

      set stamp0 [lindex [lindex $List $Idx0] 0]
      set stamp1 [lindex [lindex $List $Idx1] 0]

      return [expr abs(int([fstdstamp diff $stamp1 $stamp0]))]
   } else {
      return 0
   }
}

#----------------------------------------------------------------------------
# Nom      : <MetData::GetLatestRun>
# Creation : Mai 2008 - J.P. Gauthier - CMC/CMOE
#
# But      : Recuperer la derniere run
#
# Parametres :
#  <Path>    : Path des donnees de la run a recuperer
#
# Retour     :
#  <run>     : Nom de fichier de la derniere run
#
# Remarques :
#
#----------------------------------------------------------------------------

proc MetData::GetLatestRun { Path } {

   return [lindex [split [lindex [lsort -dictionary -increasing [glob -nocomplain $Path/\[1-2\]*_???]] end] _] 0]
}

#----------------------------------------------------------------------------
# Nom      : <MetData::GetLatestStamp>
# Creation : Avril 2006 - J.P. Gauthier - CMC/CMOE
#
# But      : Recuperer le stamp de la derniere run
#
# Parametres :
#  <Path>    : Path des donnees de la run a recuperer
#
# Retour     :
#  <Stamp>   : Stamp de la date de validite du debut de la run
#
# Remarques :
#
#----------------------------------------------------------------------------

proc MetData::GetLatestStamp { Path } {

   set file  [lindex [lsort -dictionary [glob -tails -directory $Path \[1-2\]*_???]] end]
   return [MetData::StampFromFile $file]
}

#----------------------------------------------------------------------------
# Nom      : <MetData::StampFromFile>
# Creation : Mars 2010 - J.P. Gauthier - CMC/CMOE
#
# But      : Recuperer le stamp d'un fichier
#
# Parametres :
#  <File>    : Fichier de donnes meteo
#
# Retour     :
#  <Stamp>   : Stamp de la date de validite du debut de la run
#
# Remarques :
#
#----------------------------------------------------------------------------

proc MetData::StampFromFile { File } {

   set File [file tail $File]
   set stamp [fstdstamp fromseconds [clock scan "[string range $File 0 7] [string range $File 8 9]" -timezone :UTC]]
   return $stamp
}

proc MetData::SecFromFile { File } {
   set File [file tail $File]
   scan [string range $File 11 13] %d hours 
   
   return [clock add [clock scan "[string range $File 0 7] [string range $File 8 9]" -timezone :UTC] $hours hour]
}

proc MetData::GetClosestFile { Path Sec { Max 3600 } { Cache False } } {
   variable Data
   
   if { !$Cache || ![info exists ::MetData::Data($Path)] } {
      set Data($Path) {}
      foreach file [lsort -dictionary -decreasing [glob -nocomplain $Path/??????????_\[0-9\]\[0-9\]\[0-9\]]] {
         lappend Data($Path) [list $file [SecFromFile $file]]
      }
#      set Data($Path) [lsort -decreasing -index 1 $Data($Path)]
   }
   set dmin 1e32
   set file ""
   foreach data $Data($Path) {
      set dt [expr abs($Sec-[lindex $data 1])]

      if { $dt<$dmin } {
         set dmin $dt
         set file [lindex $data 0]
      }
   }
   
   if { $dmin>=$Max } {
      set file ""
   }
   return $file
}
#----------------------------------------------------------------------------
# Nom      : <MetData::StampModulo>
# Creation : Juin 2006 - J.P. Gauthier - CMC/CMOE
#
# But      : Recuperer le stamp le plus pres du stamp courant en dedans du temps specifie
#
# Parametres :
#  <Stamp>   : Date stamp
#  <Sec>     : Delai en sedondes
#
# Retour     :
#  <Stamp>   : Stamp de la date
#
# Remarques :
#
#----------------------------------------------------------------------------

proc MetData::StampModulo { Stamp Sec } {
   return [fstdstamp fromsecond [expr [fstdstamp tosecond $Stamp]/$Sec*$Sec]]
}

proc MetData::File2Sec { File } {

   set dh [split [file tail $File] _]
   set r  [lindex $dh 0]
   set ex [lindex $dh 1]

   return [clock scan "[string range $r 0 7] [string range $r 8 9] +[string range [lindex $dh 1] 0 2] hours" -timezone :UTC]
}

#----------------------------------------------------------------------------
# Nom      : <MetData::Find>
# Creation : Avril 2003 - J.P. Gauthier - CMC/CMOE
#
# But      : Trouver un champs dans une liste de fichiers et le lire.
#
# Parametres :
#   <FLD>    : Identificateur du champs a utilise
#   <FID>    : Liste de sidentificateurs de fichiers ou recherche
#   <DATEV>  : Parametre de recherche
#   <ETIKET> : Parametre de recherche
#   <IP1>    : Parametre de recherche
#   <IP2>    : Parametre de recherche
#   <IP3>    : Parametre de recherche
#   <TYPVAR> : Parametre de recherche
#   <NOMVAR> : Parametre de recherche
#
# Retour     :
#   <found>  : Trouve ???
#
# Remarques :
#    Aucune.
#
#----------------------------------------------------------------------------

proc MetData::Find { FLD FID DATEV ETIKET IP1 IP2 IP3 TYPVAR NOMVAR } {

   foreach fid $FID {
      set idx [fstdfield find $fid $DATEV $ETIKET $IP1 $IP2 $IP3 $TYPVAR $NOMVAR]
      if { [llength $idx] } {
         fstdfield read $FLD $fid [lindex $idx 0]
         return True
      }
   }
   return False
}

#----------------------------------------------------------------------------
# Nom      : <MetData::FindAll>
# Creation : Avril 2003 - J.P. Gauthier - CMC/CMOE
#
# But      : Trouver tous les champs dans une liste de fichiers et le lire.
#
# Parametres :
#   <FLD>    : Identificateur du champs a utilise
#   <FID>    : Liste de sidentificateurs de fichiers ou recherche
#   <DATEV>  : Parametre de recherche
#   <ETIKET> : Parametre de recherche
#   <IP1>    : Parametre de recherche
#   <IP2>    : Parametre de recherche
#   <IP3>    : Parametre de recherche
#   <TYPVAR> : Parametre de recherche
#   <NOMVAR> : Parametre de recherche
#
# Retour     :
#   <found>  : Trouve ???
#
# Remarques :
#    Aucune.
#
#----------------------------------------------------------------------------

proc MetData::FindAll { FLD FID DATEV ETIKET IP1 IP2 IP3 TYPVAR NOMVAR } {

   set i 0

   foreach fid $FID {
      set idxs [fstdfield find $fid $DATEV $ETIKET $IP1 $IP2 $IP3 $TYPVAR $NOMVAR 10000]
      foreach idx $idxs {
         fstdfield read $FLD$i $fid $idx
         lappend lst $FLD$i
         incr i
      }
   }
   return $lst
}

#-------------------------------------------------------------------------------
# Nom      : <MetData::FormatDATEV>
# Creation : Mars 1999 - J.P. Gauthier - CMC/CMOE
#
# But      : Recupere la date de validitee d'un champs.
#
# Parametres :
#   <Field>  : Identificateur du champs
#   <Min>    : Ajouter les minutes
#
# Retour    :
#   <Date>  : Date de validitee du champs formatee
#
# Remarque :
#
#-------------------------------------------------------------------------------

proc MetData::FormatDATEV { Field { Min False } } {

   #----- Obtenir la date reelle a partir de la date d'origine du champs (STAMP)
   #----- Transformer en secondes pour traiter la date

   set seconds [fstdstamp toseconds [fstdfield define $Field -DATEV]]

   #----- Retour de la date de validitee

   if { $Min } {
      return [clock format $seconds -format "%a %b %d %Y, %H:%M UTC" -timezone :UTC]
   } else {
      return [clock format $seconds -format "%a %b %d %Y, %H UTC" -timezone :UTC]
   }
}

#-------------------------------------------------------------------------------
# Nom      : <MetData::GridDefineUTM>
# Creation : Avril 2006 - J.P. Gauthier - CMC/CMOE
#
# But      : Definit une grille LatLon Z.
#
# Parametres:
#   <Lat0>   : Latitude du coin inferieur gauche
#   <Lon0>   : Longitude du coin inferieur gauche
#   <Lat1>   : Latitude du coin superieur droit
#   <Lon1>   : Longitude du coin superieur droit
#   <DLat>   : Delta en latitude
#   <DLon>   : Delta en longitude
#   <ETIKET> : Etiquette a assigner
#
# Remarques :
#    Aucune.
#
#-------------------------------------------------------------------------------

proc MetData::GridDefineUTM { Lat0 Lon0 Lat1 Lon1 Res { Field "" } } {

   set zone [expr int(ceil((180+(($Lon1+$Lon0)/2))/6))]
   set meri [expr -((180-($zone*6))+3)]

   georef create UTMREF
   eval georef define UTMREF -projection \{PROJCS\[\"NAD83 UTM, Zone $zone North, Meter\",GEOGCS\[\"NAD83\",DATUM\[\"North_American_Datum_1983\",SPHEROID\[\"GRS1980\",6378137,298.257222101\]\],PRIMEM\[\"Greenwich\",0\],UNIT\[\"degree\",0.0174532925199433\]\],PROJECTION\[\"Transverse_Mercator\"\],PARAMETER\[\"latitude_of_origin\",0\],PARAMETER\[\"central_meridian\",$meri\],PARAMETER\[\"scale_factor\",0.9996\],PARAMETER\[\"false_easting\",500000\],PARAMETER\[\"false_northing\",0\],UNIT\[\"Meter\",1\]\]\}

   set xy0 [georef unproject UTMREF $Lat0 $Lon0]
   set xy1 [georef unproject UTMREF $Lat1 $Lon1]
   set w   [expr int(ceil(([lindex $xy1 0]-[lindex $xy0 0])/$Res))]
   set h   [expr int(ceil(([lindex $xy1 1]-[lindex $xy0 1])/$Res))]

   #----- Si on a un champs

   if { $Field!="" } {
      if { ![fstdfield is $Field] } {
         fstdfield create $Field $NI $NJ 1
         fstdfield define $Field -NOMVAR GRID
      }
      fstdfield define $Field -georef UTMREF
   }
}

#---------------------------------------------------------------------------
# Name      : <gMetData::GridLLDelta>
# Creation  :  May 2006 - G. Mercier - CMC/CMOE.
#
# But      : compute the delta lat/lon for the central point in the grid.
#
# Parametres :
#   <Lat0>  : South-W corner lat
#   <Lon0>  : South-W corner lon
#   <Lat1>  : North-E corner lat
#   <Lon1>  : North-E corner lon
#   <Delta> : distance increment(meters) between two grid points
#
# Return : [list dlat dlon]
#
# Remark :
#
#---------------------------------------------------------------------------

proc MetData::GridLLDelta { Lat0 Lon0 Lat1 Lon1 Delta } {
   variable Const

   #----- Calculate ranges
   set dlat  [expr double([expr abs($Lat1 - $Lat0)])]
   set dlon  [expr double([expr abs($Lon1 - $Lon0)])]

   #----- Calculate distances between in km in the ranges (for X, we use average)
   set latavg [expr (($Lat0 + $Lat1)/ 2.0)*$Const(Deg2Rad)]

   set dx [expr ($dlon * $Const(Deg2Rad)) * $Const(EarthRadius) * cos($latavg)]
   set dy [expr ($dlat * $Const(Deg2Rad)) * $Const(EarthRadius)]

   #----- Get number of points along the axis
   set nx [expr int(ceil($dx / $Delta))]
   set ny [expr int(ceil($dy / $Delta))]

   return [list [expr ($dlat/$ny)] [expr ($dlon/$nx)]]
}

#-------------------------------------------------------------------------------
# Nom      : <MetData::GridDefineLL>
# Creation : Avril 2006 - J.P. Gauthier - CMC/CMOE
#
# But      : Definit une grille LatLon Z.
#
# Parametres:
#   <Lat0>   : Latitude du coin inferieur gauche
#   <Lon0>   : Longitude du coin inferieur gauche
#   <Lat1>   : Latitude du coin superieur droit
#   <Lon1>   : Longitude du coin superieur droit
#   <DLat>   : Delta en latitude
#   <DLon>   : Delta en longitude
#   <ETIKET> : Etiquette a assigner
#   <ID>     : Field name
#
# Remarques :
#    Aucune.
#
#-------------------------------------------------------------------------------

proc MetData::GridDefineLL { Lat0 Lon0 Lat1 Lon1 DLat DLon { ETIKET GRID } { ID "" } { Type Float32 } { GridNo 0 } } {

   #----- Order bounding box limits
  if { $Lat1<$Lat0 } { 
      set tmp $Lat0
      set Lat0 $Lat1
      set Lat1 $tmp
   }   

   if { $Lon1<$Lon0 } { 
      set tmp $Lon0
      set Lon0 $Lon1
      set Lon1 $tmp
   }   

   if { $ID=="" } {
      set ID GRIDLL
   }
   
   set ni [expr int(ceil(($Lon1-$Lon0)/$DLon))+1]
   set nj [expr int(ceil(($Lat1-$Lat0)/$DLat))+1]
   
   fstdfield create ${ID}TIC $ni 1 1
   fstdfield create ${ID}TAC 1 $nj 1

   fstdfield define ${ID}TIC -GRTYP L 0 0 1.0 1.0
   fstdfield define ${ID}TAC -GRTYP L 0 0 1.0 1.0

   fstdfield define ${ID}TIC -DEET 0 -NPAS 0 -IP1 $GridNo -IP2 0 -IP3 0 -ETIKET $ETIKET -NOMVAR ">>" -TYPVAR X
   fstdfield define ${ID}TAC -DEET 0 -NPAS 0 -IP1 $GridNo -IP2 0 -IP3 0 -ETIKET $ETIKET -NOMVAR "^^" -TYPVAR X

   #----- Compute tictic grid coordinates.
   set lon $Lon0
   for { set i 0 } { $i < $ni } { incr i } {
#      fstdfield stats ${ID}TIC -gridvalue $i 0 [expr $lon<0.0?$lon+360.0:$lon]
      fstdfield stats ${ID}TIC -gridvalue $i 0 $lon
      set lon [expr $lon+$DLon]
   }

   #----- Compute tactac grid coordinates.
   set lat $Lat0
   for { set j 0 } { $j < $nj } { incr j } {
      fstdfield stats ${ID}TAC -gridvalue 0 $j $lat
      set lat [expr $lat+$DLat]
   }

   fstdfield create ${ID} $ni $nj 1 ${Type}
   fstdfield define ${ID} -ETIKET $ETIKET -NOMVAR "GRID" -TYPVAR X -IP1 1200 -IP2 0 -IP3 0 -IG1 $GridNo -IG2 0 -IG3 0 -IG4 0 -GRTYP Z
   fstdfield define ${ID} -positional ${ID}TIC ${ID}TAC

   return ${ID}
}

#-------------------------------------------------------------------------------
# Nom      : <MetData::GridDefinePS>
# Creation : Octobre 1999 - J.P. Gauthier - CMC/CMOE
#
# But      : Definit une grille ps.
#
# Parametres:
#   <Scale> : Echelle (Nom de l'echelle ou nom plus resolution
#   <NI>    : Nombre de point de grille en I
#   <NJ>    : Nombre de point de grille en J
#   <Lat>   : Latitude du centre de la grille
#   <Lon>   : Longitude du centre de la grille
#   <Field> : Champs a configurer
#
# Remarques :
#    Aucune.
#
#-------------------------------------------------------------------------------

proc MetData::GridDefinePS { Scale NI NJ Lat Lon { Field "" } } {

   if { [llength $Scale]>1 } {
      set xg3 [lindex $Scale 1]
      set Scale [lindex $Scale 0]
   } else {
      switch $Scale {
         "HEMI"  { set xg3 150000.0 }
         "MESO"  { set xg3 50000.0 }
         "FINE"  { set xg3 25000.0 }
         "VFINE" { set xg3 10000.0 }
         "EFINE" { set xg3 5000.0  }
         default { set xg3 1.0 }
      }
   }

   #----- choix de l'hemisphere SUD on NORD
   if { $Lat<=0 } {
      set grtyp SUD
      set nhem  2
      set xg4  [expr 90.0+$Lon]
      set xg4  [expr floor(fmod($xg4+360.0,360.0))]
   } else {
      set grtyp NORD
      set nhem  1
      set xg4   [expr (270.0-$Lon+360.0)/360.0]
      set xg4   [expr ($xg4-floor($xg4))*360.0]
   }

   if { [string match "*HEMI*" $Scale] } {
      set dlon 0.0
      if { $grtyp=="SUD" } {
         set dlat -90.0
      } else {
         set dlat 90.0
      }
   } else {
      set dlat $Lat
      set dlon $Lon
   }

   set dd60 1.0
   set xy [fstdgrid xyfll $dlat $dlon $dd60 $xg4 $nhem]

   set xg1 [expr ((($NI-1.0)/2.0) * $xg3 - [lindex $xy 0]) / $xg3 + 1.0]
   set xg2 [expr ((($NJ-1.0)/2.0) * $xg3 - [lindex $xy 1]) / $xg3 + 1.0]

   #----- Si on a un champs
   if { $Field!="" } {
      fstdfield create $Field $NI $NJ 1
      fstdfield define $Field -NOMVAR GRID
      fstdfield define $Field -GRTYP [string index $grtyp 0] $xg1 $xg2 $xg3 $xg4
   }

   return [format "PS $NI $NJ %.7f %.7f %.7f %.7f $grtyp" $xg1 $xg2 $xg3 $xg4]
}

#-------------------------------------------------------------------------------
# Nom      : <MetData::ListIP2>
# Creation : Septembre 1999 - J.P. Gauthier - CMC/CMOE
#
# But      : Recupere la liste des heures (IP2) pour une variable specifique.
#
# Parametres  :
#   <Index>   : Index des champs du fichier (retourner par "fstdfile open")
#   <Var>     : Nom de la variable dont on veut les heures
#   <Stamp>   : Utiliser la difference avec le stamp specifie
#
# Retour    :
#   <List>  : Liste des heures (IP2) disponibles dans le fichier
#
# Remarque :
#
#-------------------------------------------------------------------------------

proc MetData::ListIP2 { Index Var { Stamp 0 } } {

   set ip2list ""

   foreach line $Index {

      if { [string trim [lindex $line 0]]==$Var } {
         if { $Stamp } {
            set datev [lindex $line 9]
            set datev [fstdstamp fromdate [string range $datev 0 7] [string range $datev 8 11]0000]
            lappend ip2list [expr int([fstdstamp diff $datev $Stamp])]
         } else {
            lappend ip2list [lindex $line end-2]
         }
      }
   }
   return [lsort -dictionary -unique $ip2list]
}

#----------------------------------------------------------------------------
# Nom      : <MetData::Profile>
# Creation : Novembre 2001 - J.P. Gauthier - CMC/CMOE
#
# But      : Recuperer un profil de valeur sur tout les niveaux en metres pour
#            une lat-lon psecifique.
#
# Parametres :
#  <Stamp>   : Date stamp des donnees
#  <Var>     : NomVar de la variable
#  <File>    : Fichier meteorologique
#  <Lat>     : Coordonnne en latitude
#  <Lon>     : Coordonnne en longitude
#  <From>    : Index du niveau de depart
#  <To>      : Index du niveau de fin
#
# Retour     :
#  <Profile> : Liste de valeur au format { { elev value } { ... } ... }
#
# Remarques :
#
#----------------------------------------------------------------------------

proc MetData::Profile { Stamp Var File Lat Lon { From 0 } { To end } } {

   #----- Recuperer le GZ au niveau 0

   set nofield [catch { fstdfield read GZ $File $Stamp "" 12000 -1 -1 "" GZ }]

   if { $nofield } {
      Log::Print ERROR "No field found"
      return ""
   }

   set gz [fstdfield stats GZ -coordvalue $Lat $Lon]

   if { $gz=="-" } {
       Log::Print ERROR "Off grid localisation"
       fstdfield free GZ
       return ""
   }

   #----- Recuperer tout les niveaux disponibles pour GZ
   set ip1s  [lsort -decreasing -integer [fstdfile info $File IP1 GZ]]
   set ip1ss [lrange $ip1s $From $To]
   Log::Print DEBUG "Using [llength $ip1ss] of the [llength $ip1s] levels for datestamp $Stamp"

   #----- Parcourir tout les niveaux et recupere le profil de vent
   foreach ip1 $ip1ss {
      fstdfield read GZ $File $Stamp "" $ip1 -1 -1 "" GZ
      fstdfield read VAR $File $Stamp "" $ip1 -1 -1 "" $Var

      set ele [expr ([fstdfield stats GZ -coordvalue $Lat $Lon]-$gz)*10.0]
      set val [fstdfield stats VAR -coordvalue $Lat $Lon]

      if { $Var=="UU" || $Var=="VV" } {
         if { $ele<=0.0 } {
            set ele 10.0
         }
         lappend prof [list $ele [expr [lindex $val 0]*0.515] [lindex $val 1]]
      } else {
         lappend prof [list $ele $val]
      }
   }
   fstdfield free GZ
   fstdfield free VAR

   Log::Print DEBUG "Description of the profile: $prof"
   return $prof
}

#----------------------------------------------------------------------------
# Nom      : <MetData::Obukhov>
# Creation : Novembre 2001 - J.P. Gauthier - CMC/CMOE
#
# But      : Recuperer la longueur d'Obukhov pour une date et une localisation
#            specifique.
#
# Parametres :
#  <Stamp>   : Date stamp des donnees
#  <File>    : Fichier meteorologique
#  <Lat>     : Coordonnne en latitude
#  <Lon>     : Coordonnne en longitude
#
# Retour     :
#
# Remarques :
#
#----------------------------------------------------------------------------

proc MetData::Obukhov { Stamp File Lat Lon } {

   #----- Find inverse Obukhov length (aggregated value) [m-1] record.
   set io [fstdfield find $File $Stamp "" 1195 -1 -1 "" IO]
   set ol 22856.0320937

   if { [llength $io] } {

      #----- Read inverse Obukhov length [m-1].
      fstdfield read FLD $File $io

      #----- Interpolate field at specific geographical coordinates.
      set io [fstdfield stats FLD -coordvalue $Lat $Lon]

      if { [expr abs($io)]>1e-32 } {
         #----- Compute Obukhov length [m].
         set ol [expr 1.0/double($io)]
      }
   } else {
      Log::Print WARNING "Missing inverse Obukhov length field 'IO'. Using a neutral atmosphere."
   }

   return $ol
}

#----------------------------------------------------------------------------
# Nom      : <MetData::ObukhovCalculate>
# Creation : Novembre 2001 - J.P. Gauthier - CMC/CMOE
#
# But      : Calcule la longueur d'Obukhov pour une date et une localisation
#            specifique avec les champs du fichier specifier.
#
# Parametres :
#  <Stamp>   : Date stamp des donnees
#  <File>    : Fichier meteorologique
#  <Lat>     : Coordonnne en latitude
#  <Lon>     : Coordonnne en longitude
#
# Retour     :
#
# Remarques :
#
#----------------------------------------------------------------------------

proc MetData::ObukhovCalculate { Stamp File Lat Lon } {

   set rgas  287.00
   set grav    9.81
   set karm    0.35
   set cp   1004.70

   #----- Calcul du 1/L.

   set p0 [fstdfield find $File $Stamp "" -1 -1 -1 "" P0]
   set fq [fstdfield find $File $Stamp "" -1 -1 -1 "" FQ]
   set fc [fstdfield find $File $Stamp "" 1195 -1 -1 "" FC]
   set tt [fstdfield find $File $Stamp "" 12000 -1 -1 "" TT]

   if { [llength $p0] && [llength $fq] && [llength $fc] && [llength $tt] } {

      fstdfield read OBVAR $File $p0
      set p0 [fstdfield stats OBVAR -coordvalue $Lat $Lon]

      fstdfield read OBVAR $File $fq
      set fq [fstdfield stats OBVAR -coordvalue $Lat $Lon]

      fstdfield read OBVAR $File $fc
      set fc [fstdfield stats OBVAR -coordvalue $Lat $Lon]

      fstdfield read OBVAR $File $tt
      set tt [fstdfield stats OBVAR -coordvalue $Lat $Lon]

      if { $p0!="-" && $fq!="-" && $fq!=0.0 && $fc!="-" && $tt!="-" } {
         set rho  [expr 100.0*$p0/($rgas*($tt+273.15))]
         set unsL [expr -$grav*$karm/($cp*($tt+273.15))*$fc*sqrt($rho/pow($fq,3))]

         #----- verifie pour le cas nul :
         #         si nul, la longueur devient 1,000,000.
         #         autrement, on prend l'inverse.

         if { $unsL == 0.0 } {
            set L 1000000
         } else {
            set L [expr 1.0/$unsL]
         }
      } else {
         Log::Print WARNING "MetData::ObukhovCalculate: Off grid localisation, will use default value"
         set L 22856.0320937
      }

      fstdfield free OBVAR
   } else {

      Log::Print WARNING "MetData::ObukhovCalculate: Missing fields, will use default value"

      #----- Si on ne peut calculer, mettre un atmosphere neurtre
      set L 22856.0320937
   }
   return $L
}
