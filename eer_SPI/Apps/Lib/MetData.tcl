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
#   MetData::Duration         { List Idx0 Idx1 }
#   MetData::File             { Date APath PPath Mode Mixed { Host "" } }
#   MetData::Find             { FLD FID NOMVAR TYPVAR DATEV IP1 IP2 IP3 ETIKET }
#   MetData::FindAll          { FLD FID DATEV ETIKET IP1 IP2 IP3 TYPVAR NOMVAR }
#   MetData::FormatDATEV      { Field { Min False } }
#   MetData::GetLatestRun     { Path }
#   MetData::GetLatestStamp   { Path }
#   MetData::GetStampFromFile { File }
#   MetData::GetMode          { Files }
#   MetData::StampModulo      { Stamp Sec }
#   MetData::GridDefineLL     { Lat0 Lon0 Lat1 Lon1 DLat DLon { ETIKET GRID }} {
#   MetData::GridDefinePS     { Scale NI NJ Lat Lon { Field "" } }
#   MetData::ListIP2          { Index Var { Stamp 0 } }
#   MetData::Path             { Level Model DiagVar ProgVar }
#   MetData::Profile          { Stamp Var File Lat Lon }
#   MetData::Obukhov          { Stamp File Lat Lon }
#   MetData::ObukhovCalculate { Stamp File Lat Lon }
#
# Remarques :
#   Aucune
#
#===============================================================================

package provide MetData 1.1

catch { SPI::Splash "Loading Widget Package MetData 1.1" }

namespace eval MetData { } {
   global env
   variable Data
   variable Param
   variable Const

   set Param(Path) ""
   catch { set Param(Path) $env(CMCGRIDF) }

   #----- Lire les diverses definitions

   catch {
      source $GDefs(Dir)/Data/AEGL.tcl
      source $GDefs(Dir)/Data/ERPG.tcl
   }

   set Data(ProgMax)  240   ;# Max prog usable
   set Data(T0)  -1         ;# First Metdata time available
   set Data(T1)  -1         ;# Last Metdata time available
   set Data(TA)  -1         ;# Last Analysys time available

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

   for { set i 0 } { $i<[fstdfield define $Field -NI] } { incr i } {
      append text [format "%c" [expr int([fstdfield stats $Field -gridvalue $i 0])]]
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
   return [MetData::GetStampFromFile $file]
}

#----------------------------------------------------------------------------
# Nom      : <MetData::GetStampFromFile>
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

proc MetData::GetStampFromFile { File } {

   set File [file tail $File]
   set stamp [fstdstamp fromseconds [clock scan "[string range $File 0 7] [string range $File 8 9]"]]
   return $stamp
}

#----------------------------------------------------------------------------
# Nom      : <MetData::GetMode>
# Creation : Octobre 2008 - J.P. Gauthier - CMC/CMOE
#
# But      : Determiner le mode selon la liste des fichiers selectionnes
#
# Parametres :
#  <Files>   : Liste des fichiers
#  <Mixed>   : Mode mixte possible
#
# Retour     :
#  <Mode>    : mode (prog, diag ou mixte)
#
# Remarques :
#
#----------------------------------------------------------------------------

proc MetData::GetMode { Files { Mixed True } } {

   #----- Count number of diagnostics and prognostics met files.
   set trials 0
   set progs  0

   foreach file $Files {

      set filename [lindex $file 2]

      if { [string match "*trial*" $filename] } {
         incr trials
      } elseif { [string match "*prog*" $filename] } {
         incr progs
      }
   }

   #----- Figure out mode.
   if { $trials } {
      if { $progs && $Mixed } {
         return "mixte"
      } else {
         return "diag"
      }
   } else {
      return "prog"
   }
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

#----------------------------------------------------------------------------
# Nom      : <MetData::File>
# Creation : Novembre 2001 - J.P. Gauthier - CMC/CMOE
#
# But      : Recuperer les fichiers meteorologiques disponibles.
#
# Parametres :
#  <Date>    : Date de depart
#  <APath>   : Repertoire des analyses
#  <PPath>   : Repertoire des prognostiques
#  <Mode>    : Mode de recuperation (B=Backward,F=Forward)
#  <Mixed>   : Mode mixed (anap+prog) (0=Non mixte,1=Mixte,-1=Mixte+ Dont care about the run)
#  <Delta>    : Delta T entre les pas de temps des donnees meteo
#
# Retour     :
#  <Fichiers>: Liste de fichiers ordonnee au format
#                 { { stamp date path } { ... } ... }
#
# Remarques :
#
#----------------------------------------------------------------------------

proc MetData::File2Sec { File } {

   set dh [split [file tail $File] _]
   set r  [lindex $dh 0]
   set ex [lindex $dh 1]

   return [clock scan "[string range $r 0 7] [string range $r 8 9] +[string range [lindex $dh 1] 0 2] hours" -gmt True]
}

proc MetData::File { Date APath PPath Mode Mixed { Delta { 1 } } } {
   global GDefs
   variable Data

   set Mode [string index $Mode 0]

   set Data(T0)  -1
   set Data(T1)  -1
   set Data(TA)  -1

   #----- Get all Analysis available
   set afile { }
   set astmp 0
   if { $APath!="" } {
      set lst [split $APath ":"]
      if { [llength $lst]>1 } {
         set host [lindex $lst 0]
         set path [join [lrange $lst 1 end] :]
         catch { set afile [exec ssh -l $GDefs(FrontEndUser) -n -x $host "ls $path/\[1-2\]*_000"] }
      } else {
         set afile [lsort -dictionary -increasing [glob -nocomplain $APath/\[1-2\]*_000]]
      }
   }

   #----- Get all Prognostics available
   set pfile { }
   set prun  0
   set pstmp 0
   if { $PPath!="" } {
      set lst [split $PPath ":"]
      if { [llength $lst]>1 } {
         set host [lindex $lst 0]
         set path [join [lrange $lst 1 end] :]
         catch { set pfile [exec ssh -l $GDefs(FrontEndUser) -n -x $host "ls $path/\[1-2\]*_???"] }
      } else {
         set pfile [lsort -dictionary -increasing [glob -nocomplain $PPath/\[1-2\]*_???]]
      }
   }

   if { [llength $afile]==0 && [llength $pfile]==0 } {
      Log::Print WARNING "No data found (Analysis=$APath, Prognostics=$PPath)"
      return ""
   }

   #----- Get time range available
   set data [concat $afile $pfile]
   set Data(T0)  [MetData::File2Sec [lindex $data 0]]
   set Data(T1)  [MetData::File2Sec [lindex $data end]]

   #----- Get the last run
   if { [llength $pfile] } {
      set prun  [lindex [split [file tail [lindex $pfile end]] _] 0]
      set pstmp [fstdstamp fromdate [string range $prun 0 7] [string range $prun 8 end]000000]
      Log::Print DEBUG "Last run  : $prun ($pstmp)"
   }

   #----- Get the last trial
   if { [llength $afile] } {
      set astmp [lindex [split [file tail [lindex $afile end]] _] 0]
      set astmp [fstdstamp fromdate [string range $astmp 0 7] [string range $astmp 8 end]000000]
      Log::Print DEBUG "Last trial: $astmp"
      set Data(TA) [fstdstamp toseconds $astmp]
   }

   Log::Print DEBUG "Found data:\n\tAnalysis:\n\t\t[join $afile "\n\t\t"]\n\tPrognostics:\n\t\t[join $pfile "\n\t\t"]"

   if { $Mixed || $Date>=$pstmp } {

      #----- Process Prognostics
      foreach f $pfile {
         set dh [split [file tail $f] _]
         set r  [lindex $dh 0]

         #----- Are we using the last run ??? or if no diags exists
         if { $Mixed!=-1 && ($r!=$prun && $APath!="") } {
            continue
         }

         set d  [string range $r 0 7]
         set h "[string range $r 8 9]000000"
         set ex [lindex $dh 1]

         if { $ex<=$Data(ProgMax) } {
            set st [fstdstamp fromdate $d $h]
            set st [fstdstamp incr $st $ex]

            #----- Prioritize analysis if there are any
            if { !$astmp || $st>$astmp } {
               if { [set hour [string trimleft [clock format [fstdstamp toseconds $st] -format %H -gmt True] 0]]=="" } { set hour 0 }

               #----- Keep if it is on the right time interval
               if { [expr $hour%$Delta]==0 } {
                  set met($st) $f
               }
            }
         }
      }
   }

   if { $Mixed || $Date<$pstmp } {

      #----- Process Analysis
      foreach f $afile {
         set dh [split [file tail $f] _]
         set r  [lindex $dh 0]

         set d  [string range $r 0 7]
         set h "[string range $r 8 9]000000"
         set ex [lindex $dh 1]

         set st [fstdstamp fromdate $d $h]
         set st [fstdstamp incr $st $ex]

         if { [set hour [string trimleft [clock format [fstdstamp toseconds $st] -format %H -gmt True] 0]]=="" } { set hour 0 }

         #----- Keep if it is on the right time interval
         if { [expr $hour%$Delta]==0 } {
            set met($st) $f
         }
      }

      #----- Dans le cas ou il manque un trial pour aller jusqu'au prog
      if { !$Mixed && $st<$pstmp } {
         set met($pstmp) $PPath/${prun}_000
      }
   }

   #------ Sort following mode specification
   set data ""
   set pidx ""

   set order [lsort -dictionary -increasing [array names met *]]
   if { ![llength $order] } {
      Log::Print WARNING "Available filtered data is empty"
      return ""
   }

   if { $Mode=="F" } {

      #------ Forward mode
      foreach idx $order {
         if { $Date < $idx } {
            if { $pidx=="" } {
               Log::Print INFO "Data starting later than $Date"
               return
            } else {
               lappend data [list $pidx [join [fstdstamp todate $pidx] ""] $met($pidx)]
            }
         }
         set pidx $idx
      }
      lappend data [list $pidx [join [fstdstamp todate $pidx] ""] $met($pidx)]
   } else {

      #------ Backward mode
      foreach idx $order {
         lappend data [list $idx [join [fstdstamp todate $idx] ""] $met($idx)]
         if { $Date <= $idx } {
            break
         }
      }
   }

   if { [llength $data] } {
      Log::Print DEBUG "Available processed and sorted files:\n\t[join $data "\n\t"]"
   } else {
      Log::Print INFO "No data available for this date"
   }

   return $data
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
      set idxs [fstdfield find $fid $DATEV $ETIKET $IP1 $IP2 $IP3 $TYPVAR $NOMVAR]
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
      return [clock format $seconds -format "%a %b %d %Y, %H:%M UTC" -gmt true]
   } else {
      return [clock format $seconds -format "%a %b %d %Y, %H UTC" -gmt true]
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
#
# Remarques :
#    Aucune.
#
#-------------------------------------------------------------------------------

proc MetData::GridDefineLL { Lat0 Lon0 Lat1 Lon1 DLat DLon { ETIKET GRID }} {

   set ni [expr int(ceil(($Lon1-$Lon0)/$DLon))]
   set nj [expr int(ceil(($Lat1-$Lat0)/$DLat))]

   fstdfield create GRIDLLTIC $ni 1 1
   fstdfield create GRIDLLTAC 1 $nj 1

   fstdfield define GRIDLLTIC -GRTYP L 0 0 1.0 1.0
   fstdfield define GRIDLLTAC -GRTYP L 0 0 1.0 1.0

   fstdfield define GRIDLLTIC -DEET 0 -NPAS 0 -IP1 0 -IP2 0 -IP3 0 -ETIKET $ETIKET -DATYP 2 -NOMVAR ">>" -TYPVAR X
   fstdfield define GRIDLLTAC -DEET 0 -NPAS 0 -IP1 0 -IP2 0 -IP3 0 -ETIKET $ETIKET -DATYP 2 -NOMVAR "^^" -TYPVAR X

   #----- Compute tictic grid coordinates.

   set lon $Lon0
   for { set i 0 } { $i < $ni } { incr i } {
      fstdfield stats GRIDLLTIC -gridvalue $i 0 $lon
      set lon [expr $lon+$DLon]
   }

   #----- Compute tactac grid coordinates.

   set lat $Lat0
   for { set j 0 } { $j < $nj } { incr j } {
      fstdfield stats GRIDLLTAC -gridvalue 0 $j $lat
      set lat [expr $lat+$DLat]
   }

   fstdfield create GRIDLLMEM $ni $nj 1
   fstdfield define GRIDLLMEM -ETIKET $ETIKET -NOMVAR "GRID" -TYPVAR X -IP1 1200 -IP2 0 -IP3 0 -IG1 0 -IG2 0 -IG3 0 -IG4 0 -GRTYP Z
   fstdfield define GRIDLLMEM -positional GRIDLLTIC GRIDLLTAC

   return GRIDLLMEM
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
      fstdfield define $Field -GRTYP $grtyp $xg1 $xg2 $xg3 $xg4
   }

   return [format "$NI $NJ %.7f %.7f %.7f %.7f $grtyp" $xg1 $xg2 $xg3 $xg4]
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

      if { [string trim [lindex $line 2]]==$Var } {
         if { $Stamp } {
            set datev [lindex $line 9]
            lappend ip2list [expr int([fstdstamp diff $datev $Stamp])]
         } else {
            lappend ip2list [lindex $line 5]
         }
      }
   }
   return [lsort -dictionary -unique $ip2list]
}

#----------------------------------------------------------------------------
# Nom      : <MetData::Path>
# Creation : Novembre 2001 - J.P. Gauthier - CMC/CMOE
#
# But      : Initialiser les path des fichiers de donnees meteo
#            d'analyse et de prognostique.
#
# Parametres :
#   <Level>  : Niveau utilise
#   <Model>  : Model utilise
#   <DiagVar>: Variable contenant le repertoires des fichiers diagnostiques
#   <ProgVar>: Variable contenant le repertoire des donnees prognostiques
#
# Retour     :
#   <DiagVar>: Variable contenant le repertoires des fichiers diagnostiques
#   <ProgVar>: Variable contenant le repertoire des donnees prognostiques
#
# Remarques :
#    Aucune.
#
#----------------------------------------------------------------------------

proc MetData::Path { Level Model DiagVar ProgVar } {
   variable Param

   upvar #0 $DiagVar diag
   upvar #0 $ProgVar prog

   switch $Level {
      "eta" {
         switch $Model {
            "glb" {
               set diag "$Param(Path)/trial/glbeta2"
               set prog "$Param(Path)/prog/glbeta"
            }
            "reg" {
               set diag "$Param(Path)/trial/regeta2"
               set prog "$Param(Path)/prog/regeta"
            }
            "lameast" {
               set diag ""
               set prog "$Param(Path)/prog/lam/east.eta"
            }
            "lamwest" {
               set diag ""
               set prog "$Param(Path)/prog/lam/west.eta"
            }
            "lamarct" {
               set diag ""
               set prog "$Param(Path)/prog/lam/arctic.eta"
            }
            "lammari" {
               set diag ""
               set prog "$Param(Path)/prog/lam/maritimes.eta"
            }
            default {
               set diag ""
               set prog ""
            }
         }
      }
      "pres" {
         switch $Model {
            "glb" {
               set diag "$Param(Path)/trial/glbpres2"
               set prog "$Param(Path)/prog/glbpres"
            }
            "reg" {
               set diag "$Param(Path)/trial/regpres2"
               set prog "$Param(Path)/prog/regpres"
            }
            default {
               set diag ""
               set prog ""
            }
         }
      }
   }
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

   if { [llength $io] } {

      #----- Read inverse Obukhov length [m-1].
      fstdfield read FLD $File $io

      #----- Interpolate field at specific geographical coordinates.
      set io [fstdfield stats FLD -coordvalue $Lat $Lon]

      #----- Compute Obukhov length [m].
      set ol [expr 1.0/double($io)]

   } else {

      Log::Print WARNING "Missing inverse Obukhov length field 'IO'. Using a neutral atmosphere."

      set ol 22856.0320937
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
         Log::Print ERROR "Off grid localisation"
         set L 22856.0320937
      }

      fstdfield free OBVAR
   } else {

      Log::Print ERROR "Missing fields"

      #----- Si on ne peut calculer, mettre un atmosphere neurtre
      set L 22856.0320937
   }
   return $L
}
