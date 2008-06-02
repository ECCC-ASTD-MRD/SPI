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
#   MetData::Duration       { List Idx0 Idx1 }
#   MetData::File           { Date APath PPath Mode Mixed { Host "" } }
#   MetData::Find           { FLD FID NOMVAR TYPVAR DATEV IP1 IP2 IP3 ETIKET }
#   MetData::FindAll        { FLD FID DATEV ETIKET IP1 IP2 IP3 TYPVAR NOMVAR }
#   MetData::FormatDATEV    { Field }
#   MetData::GetLatestRun   { Path }
#   MetData::GetLatestStamp { Path }
#   MetData::StampModulo    { Stamp Sec }
#   MetData::GridDefineLL   { Lat0 Lon0 Lat1 Lon1 DLat DLon { ETIKET GRID }} {
#   MetData::GridDefinePS   { Scale NI NJ Lat Lon { Field "" } }
#   MetData::ListIP2        { Index Var { Stamp 0 } }
#   MetData::Path           { Level Model DiagVar ProgVar }
#   MetData::Profile        { Stamp Var File Lat Lon }
#   MetData::Obukhov        { Stamp File Lat Lon }
#
# Remarques :
#   Aucune
#
#===============================================================================

package provide MetData 1.0

proc IdMetData { Show } {

   if { $Show } {
      puts "(INFO) Loading Standard CMC/CMOE Package MetData Version 1.0"
   }
}

namespace eval MetData { } {
   variable Data

   #----- Lire les diverses definitions

   catch {
      source $GDefs(Dir)/Data/AEGL.tcl
      source $GDefs(Dir)/Data/ERPG.tcl
   }

   set Data(ProgMax)  240   ;#Max prog usable
   set Data(IdAnal)   ""    ;#Analysis file name
   set Data(IdProg)   ""    ;#Prognosis file name
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

   return [lindex [split [lindex [lsort -dictionary -increasing [glob -nocomplain $Path/*_???]] end] _] 0]
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

   set file  [lindex [lsort -dictionary [glob -tails -directory $Path *_???]] end]
   set stamp [fstdstamp fromseconds [clock scan "[string range $file 0 7] [string range $file 8 9]"]]
   return $stamp
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

proc MetData::File { Date APath PPath Mode Mixed { Delta { 1 } } } {
   variable Data

   set Mode [string index $Mode 0]

   #----- Get all Analysis and Prognostics available

   set afile { }
   if { $APath!="" } {
      set lst [split $APath ":"]
      if { [llength $lst]>1 } {
         set host [lindex $lst 0]
         set path [join [lrange $lst 1 end] :]
         catch { set afile [exec rsh -n $host "ls $path/*_000"] }
      } else {
         set afile [lsort -dictionary -increasing [glob -nocomplain $APath/*_000]]
      }
   }

   set pfile { }
   if { $PPath!="" } {
      set lst [split $PPath ":"]
      if { [llength $lst]>1 } {
         set host [lindex $lst 0]
         set path [join [lrange $lst 1 end] :]
         catch { set pfile [exec rsh -n $host "ls $path/*_???"] }
      } else {
         set pfile [lsort -dictionary -increasing [glob -nocomplain $PPath/*_???]]
      }
   }

   if { [llength $afile]==0 && [llength $pfile]==0 } {
      Debug::TraceProc "No data found"
      return ""
   }

   #----- Get the last run

   set prun [lindex [split [file tail [lindex $pfile end]] _] 0]
   set srun [fstdstamp fromdate [string range $prun 0 7] [string range $prun 8 end]000000]

   Debug::TraceProc "Analysis found    :\n[join $afile \n]"
   Debug::TraceProc "Prognostics found :\n[join $pfile \n]"
   Debug::TraceProc "Last run found    : $prun ($srun)"

   if { $Mixed || $Date>=$srun } {

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

            if { [set hour [string trimleft [clock format [fstdstamp toseconds $st] -format %H -gmt True] 0]]=="" } { set hour 0 }
            if { [expr $hour%$Delta]==0 } {
               set met($st) $f
            }
         }
      }
   }

   if { $Mixed || $Date<$srun } {

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
         if { [expr $hour%$Delta]==0 } {
            set met($st) $f
         }
      }

      #----- Dans le cas ou il manque un trial pour aller jusqu'au prog

      if { !$Mixed && $st<$srun } {
         set met($srun) $PPath/${prun}_000
      }
   }

   #------ Sort following mode specification

   set data ""
   set pidx ""

   set order [lsort -dictionary -increasing [array names met *]]

   if { $Mode=="F" } {

      #------ Forward mode

      foreach idx $order {
         if { $Date < $idx } {
            if { $pidx=="" } {
               Debug::TraceProc "Data starting later than $Date"
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
      Debug::TraceProc "Available processed and sorted files: \n[join $data \n]"
   } else {
      Debug::TraceProc "No data available for this date"
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
# Parametres  :
#   <Field> : Identificateur du champs
#
# Retour    :
#   <Date>  : Date de validitee du champs formatee
#
# Remarque :
#
#-------------------------------------------------------------------------------

proc MetData::FormatDATEV { Field } {

   #----- Obtenir la date reelle a partir de la date d'origine du champs (STAMP)
   #----- Transformer en secondes pour traiter la date

   set seconds [fstdstamp toseconds [fstdfield define $Field -DATEV]]

   #----- Retour de la date de validitee

   return [clock format $seconds -format "%a %b %d %Y, %H UTC" -gmt true]
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

   fstdfield create GRIDTIC $ni 1 1
   fstdfield create GRIDTAC 1 $nj 1

   fstdfield define GRIDTIC -GRTYP L 0 0 1.0 1.0
   fstdfield define GRIDTAC -GRTYP L 0 0 1.0 1.0

   fstdfield define GRIDTIC -DEET 0 -NPAS 0 -IP1 0 -IP2 0 -IP3 0 -ETIKET $ETIKET -DATYP 2 -NOMVAR ">>" -TYPVAR X
   fstdfield define GRIDTAC -DEET 0 -NPAS 0 -IP1 0 -IP2 0 -IP3 0 -ETIKET $ETIKET -DATYP 2 -NOMVAR "^^" -TYPVAR X

   #----- Compute tictic grid coordinates.

   set lon $Lon0
   for { set i 0 } { $i < $ni } { incr i } {
      fstdfield stats GRIDTIC -gridvalue $i 0 $lon
      set lon [expr $lon+$DLon]
   }

   #----- Compute tactac grid coordinates.

   set lat $Lat0
   for { set j 0 } { $j < $nj } { incr j } {
      fstdfield stats GRIDTAC -gridvalue 0 $j $lat
      set lat [expr $lat+$DLat]
   }

   #----- Write tictic and tactac fields.

   fstdfile open GRIDDEF write /tmp/[pid].fstd
   fstdfield write GRIDTIC GRIDDEF -32 True
   fstdfield write GRIDTAC GRIDDEF -32 True

   fstdfield create NEW_GRID $ni $nj 1
   fstdfield define NEW_GRID -ETIKET $ETIKET -NOMVAR "GRID" -TYPVAR X -IG1 0 -IG2 0 -IG3 0 -IG4 0 -GRTYP Z

   #----- Write grid value field.

   fstdfield write NEW_GRID GRIDDEF -32 True

   #----- Close standard output file.

   fstdfile close GRIDDEF

   fstdfile open GRIDDEF read /tmp/[pid].fstd
   fstdfield read GRID GRIDDEF -1 "" -1 -1 -1 "" GRID
   fstdfile close GRIDDEF

   file delete /tmp/[pid].fstd
   return GRID
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

   if { $Scale=="HEMI" } {
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
      if { ![fstdfield is $Field] } {
         fstdfield create $Field $NI $NJ 1
         fstdfield define $Field -NOMVAR GRID
      }
      fstdfield define $Field -GRTYP $grtyp $xg1 $xg2 $xg3 $xg4
   }

   return "$grtyp $NI $NJ $xg1 $xg2 $xg3 $xg4"
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
   global GDefs

   upvar #0 $DiagVar diag
   upvar #0 $ProgVar prog

   set Data(IdAnal) trial
   set Data(IdProg) prog

   if { $Level == "eta" } {
      switch $Model {
         "glb" {
            set diag "$GDefs(DBMet)/trial/glbeta2"
            set prog "$GDefs(DBMet)/prog/glbeta"
         }
         "reg" {
            set diag "$GDefs(DBMet)/trial/regeta2"
            set prog "$GDefs(DBMet)/prog/regeta"
         }
         "lameast" {
            set diag ""
            set prog "$GDefs(DBMet)/prog/lam/east.eta"
         }
         "lamwest" {
            set diag ""
            set prog "$GDefs(DBMet)/prog/lam/west.eta"
         }
         "lamarct" {
            set diag ""
            set prog "$GDefs(DBMet)/prog/lam/arctic.eta"
         }
         "lammari" {
            set diag ""
            set prog "$GDefs(DBMet)/prog/lam/maritimes.eta"
         }
         default {
            set diag ""
            set prog ""
         }
      }
   } else {
      switch $Model {
         "glb" {
            set diag "$GDefs(DBMet)/trial/glbpres2"
            set prog "$GDefs(DBMet)/prog/glbpres"
         }
         "reg" {
            set diag "$GDefs(DBMet)/trial/regpres2"
            set prog "$GDefs(DBMet)/prog/regpres"
         }
         default {
            set diag ""
            set prog ""
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
#
# Retour     :
#  <Profile> : Liste de valeur au format { { elev value } { ... } ... }
#
# Remarques :
#
#----------------------------------------------------------------------------

proc MetData::Profile { Stamp Var File Lat Lon } {

   #----- Recuperer le GZ au niveau 0

   set nofield [catch { fstdfield read GZ $File $Stamp "" 12000 -1 -1 "" GZ }]

   if { $nofield } {
      Debug::TraceProc "No field found"
      return ""
   }

   set gz [fstdfield stats GZ -coordvalue $Lat $Lon]

   if { $gz=="-" } {
       Debug::TraceProc "Error, off grid localisation"
       fstdfield free GZ
       return ""
   }

   #----- Recuperer tout les niveaux disponibles pour GZ

   set idxs [fstdfield find $File $Stamp "" -1 -1 -1 "" GZ]

   Debug::TraceProc "Found [llength $idxs] levels for datestamp $Stamp"

   #----- Parcourir tout les niveaux et recupere le profil de vent

   foreach idx $idxs {
      fstdfield read GZ $File $idx
      fstdfield read VAR $File $Stamp "" [fstdfield define GZ -IP1] -1 -1 "" $Var

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
   set prof [lsort -increasing -real -index 0 $prof]

   fstdfield free GZ
   fstdfield free VAR

   Debug::TraceProc "Description of the profile: $prof"
   return $prof
}

#----------------------------------------------------------------------------
# Nom      : <MetData::Obukhov>
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

proc MetData::Obukhov { Stamp File Lat Lon } {

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
         Debug::TraceProc "Error, off grid localisation"
         set L 22856.0320937
      }

      fstdfield free OBVAR
   } else {

      Debug::TraceProc "Error, missing fields"

      #----- Si on ne peut calculer, mettre un atmosphere neurtre
      set L 22856.0320937
   }
   return $L
}
