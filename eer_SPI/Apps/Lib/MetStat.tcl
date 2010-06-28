#===============================================================================
# Environnement Canada
# Centre Meteorologique Canadian
# 2100 Trans-Canadienne
# Dorval, Quebec
#
# Projet    : Librairie de fonctions statistiques relatives aux fichiers meteorologiques
# Fichier   : MetStat.tcl
# Creation  : Janvier 2002 - J.P. Gauthier - CMC/CMOE
#
# Description: Definitions de fonctions statistiques relatives aux fichiers meteorologiques.
#
# Fonctions:
#
#   MetStat::Level        { Mode Min Max Nb }
#   MetStat::LevelLinear  { Min Max Mode Nb }
#   MetStat::LevelLog     { Min Max }
#   MetStat::LevelLogList { Min Max Mult }
#   MetStat::LevelMod     { Min Max Delta }
#   MetStat::LevelRSMC    { Min Max }
#   MetStat::RECRCAdd     { Var Desc Unit Factor Level Inter }
#   MetStat::RECRCEval    { Cmd }
#   MetStat::RECRCLoad    { File }
#   MetStat::Calculate    { X Y { Factor -1 } }
#
# Remarques :
#   Aucune
#
#===============================================================================

package provide MetStat 1.0

catch { SPI::Splash "Loading Widget Package MetStat 1.0" }

namespace eval MetStat { } {
   global env GDefs
   variable Data
   variable Stat
   variable Rec
   variable Msg

   set Stat(VARx)   0.0   ;#Variance X
   set Stat(VARy)   0.0   ;#Variance Y
   set Stat(VARxy)  0.0   ;#Covariance
   set Stat(STDxy)  0.0   ;#Standard deviation of x-y bias

   set Stat(SSxx)   0.0   ;#Sum of squared values X
   set Stat(SSyy)   0.0   ;#Sum of squared values Y
   set Stat(SSxy)   0.0   ;##Sum of squared values X-Y
   set Stat(S)      0.0   ;#
   set Stat(R2)     0.0   ;#Correlation coeficient
   set Stat(b)      0.0   ;#Regression coeficient (Slope of curve fitting)
   set Stat(a)      0.0   ;#Regression coeficient (Delta of curve fitting)
   set Stat(Eb)     0.0   ;#Standard Error for b
   set Stat(Ea)     0.0   ;#Standard Error for a
   set Stat(AVGx)   0.0   ;#Average of x
   set Stat(AVGy)   0.0   ;#Average of y
   set Stat(RMSE)   0.0   ;#Root mean square error
   set Stat(NRMSE)  0.0   ;#Normalized Root mean square error
   set Stat(ME)     0.0   ;#Mean error
   set Stat(NME)    0.0   ;#Normalized mean error
   set Stat(MNE)    0.0   ;#Mean normalized error
   set Stat(MB)     0.0   ;#Mean bias
   set Stat(NMB)    0.0   ;#Normalized mean bias
   set Stat(MNB)    0.0   ;#Mean normalized bias
   set Stat(MFB)    0.0   ;#Mean fractional bias
   set Stat(MFE)    0.0   ;#Mean fractional error
   set Stat(LMNB)   0.0   ;#Logarithmic mean normalized bias
   set Stat(LMNE)   0.0   ;#Logarithmic mean normalized error
   set Stat(n)      0     ;#Number of sample
   set Stat(NA)     0.0   ;#Number of station n.a. stations
   set Stat(RNA)    0.0   ;#ratio of sample availability

   set Data(No)    0   ;#Numero de champs resultant

   #----- Definitions des champs

   set Rec(TypeA) "Analyse"
   set Rec(TypeC) "Climatologie"
   set Rec(TypeD) "Données brutes aux stations"
   set Rec(TypeE) "Erreur mensuelle"
   set Rec(TypeK) "Constantes variées"
   set Rec(TypeM) "Matrice de vérification"
   set Rec(TypeO) "Observation"
   set Rec(TypeP) "Prévision"
   set Rec(TypeQ) "Diagnostic QPF"
   set Rec(TypeS) "Scores divers"
   set Rec(TypeT) "Série temporelle"
   set Rec(TypeX) "Divers"
   set Rec(Var)   ""
   set Rec(Desc)  ""
   set Rec(Level) {}
   set Rec(Inter) {}

   #----- Definition des messages

   set Msg(RECRC) { "Commande invalide dans le fichier .recrc" "Invalid command within the .recrcfile" }
}

#-------------------------------------------------------------------------------
# Nom      : <MetStat::Level>
# Creation : Fevrier 2003 - J.P. Gauthier - CMC/CMOE
#
# But      : Creer une liste de niveaux selon le mode specifie.
#
# Parametres :
#   <Mode>   : Mode de selection des niveaux
#   <Min>    : Minimum
#   <Max>    : Maximum
#   <Nb>     : Nombre de niveau
#
# Retour    :
#
# Remarque :
#
#-------------------------------------------------------------------------------

proc MetStat::Level { Mode Min Max Nb } {

   set levels ""

   switch $Mode {
      "LOGARITHMIC" { set levels [MetStat::LevelLog    $Min $Max] }
      "RSMC"        { set levels [MetStat::LevelRSMC   $Min $Max] }
      "LINEAR"      { set levels [MetStat::LevelLinear $Min $Max Double $Nb] }
      "INTERVAL"    { set levels [MetStat::LevelMod    $Min $Max $Nb] }
   }

   return $levels
}

#-------------------------------------------------------------------------------
# Nom      : <MetStat::LevelLinear>
# Creation : Fevrier 2003 - J.P. Gauthier - CMC/CMOE
#
# But      : Creer une liste de niveaux lineaire dans le range de valeurs.
#
# Parametres :
#   <Mode>   : Mode de selection des niveaux
#   <Min>    : Minimum
#   <Max>    : Maximum
#   <Nb>     : Nombre de niveau
#
# Retour    :
#
# Remarque :
#
#-------------------------------------------------------------------------------

proc MetStat::LevelLinear { Min Max Mode Nb } {

   incr Nb
   set incr [expr ($Max-$Min)/$Nb]
   set levels ""

   for { set i 1 } { $i < $Nb } { incr i } {
      if { $Mode == "Int" } {
         lappend levels [expr int($Min + $i*$incr)]
      } else {
         lappend levels [expr $Min + $i*$incr]
      }
   }
   return $levels
}

#-------------------------------------------------------------------------------
# Nom      : <MetStat::LevelLog>
# Creation : Fevrier 2003 - J.P. Gauthier - CMC/CMOE
#
# But      : Creer une liste de niveaux logarithmique a partir du minimum.
#
# Parametres :
#   <Min>    : Minimum
#   <Max>    : Maximum
#
# Retour    :
#
# Remarque :
#
#-------------------------------------------------------------------------------

proc MetStat::LevelLog { Min Max } {

   set levels { }

   #----- Si les valeurs sont negatives

   if { $Max<=0 } {
      set max [expr abs($Min)]
      set min [expr abs($Max)]

      set levels [MetStat::LevelLogList $min $max -1]

   #----- Si les valeurs vont du negatif au positifs

   } elseif { $Min<0 } {
      set levels [concat [MetStat::LevelLogList 0 [expr abs($Min)] -1] [MetStat::LevelLogList 0 $Max 1]]

   #----- Si les valeurs sont positives

   } else {
      set levels [MetStat::LevelLogList $Min $Max 1]
   }

   return [lsort -real -increasing -unique $levels]
}

#-------------------------------------------------------------------------------
# Nom      : <MetStat::LevelLogList>
# Creation : Mars 2003 - J.P. Gauthier - CMC/CMOE
#
# But      : Creer une liste de niveaux logarithmique a partir du minimum pour
#            un range negatif ou positif exclusif.
#
# Parametres :
#   <Min>    : Minimum
#   <Max>    : Maximum
#
# Retour    :
#
# Remarque :
#     - Si le min =0 et que le max est <=1, on met 5 niveaux puisque l'on ne peut
#       connaitre la limite d'arret de la boucle
#
#-------------------------------------------------------------------------------

proc MetStat::LevelLogList { Min Max Mult } {

   if { $Max==0.0 } {
      set max 0
   } else {
      set max [expr int(ceil(log10($Max)))]
   }

   if { $Min==0.0 } {
      if { $max<=0 } {
         set min [expr $max-5]
      } else {
         set min 0
      }
      set levels { 0 }
   } else {
      set min [expr int(log10($Min))]
   }
   set inc 1

   while { $min<=$max } {
      lappend levels [expr $Mult*pow(10,$min)]
      incr min $inc
   }

   return $levels
}

#-------------------------------------------------------------------------------
# Nom      : <MetStat::LevelMod>
# Creation : Fevrier 2003 - J.P. Gauthier - CMC/CMOE
#
# But      : Creer une liste de niveaux a interval specifique.
#
# Parametres :
#   <Min>    : Minimum
#   <Max>    : Maximum
#   <Delta>  : Interval
#
# Retour    :
#
# Remarque :
#
#-------------------------------------------------------------------------------

proc MetStat::LevelMod { Min Max Delta } {
   global GDefs

   if { $Delta==0.0 } {
      set levels ""
   } else {
      set nb  0

      set Min [expr $Min - fmod($Min,$Delta)]
      set levels ""

      for { set i $Min } { $i < $Max } { set i [expr $i+$Delta] } {
         incr nb
         lappend levels $i
         if { $nb>256 } {
            break
         }
      }
   }
   return $levels
}

#-------------------------------------------------------------------------------
# Nom      : <MetStat::LevelRSMC>
# Creation : Fevrier 2003 - J.P. Gauthier - CMC/CMOE
#
# But      : Creer une liste de niveaux aux standard RSMC
#
# Parametres :
#   <Min>    : Minimum
#   <Max>    : Maximum
#   <Delta>  : Interval
#
# Retour    :
#
# Remarque :
#
#-------------------------------------------------------------------------------

proc MetStat::LevelRSMC { Min Max } {

   if { $Max <= 0.0 } {
      set base 1
   } else {
      set base [expr int(log10($Max))]
   }

   #----- Definir l'exposant maximum

   set expmax [expr (4-1)*2+1]

   #----- Definir la liste des niveaux

   set levels ""

   for { set exp $expmax } { $exp >= 1 } { incr exp -2 } {
      lappend levels [expr pow(10,($base - $exp))]
   }
   return $levels
}

#-------------------------------------------------------------------------------
# Nom      : <MetStat::RECRCAdd>
# Creation : Juin 2003 - J.P. Gauthier - CMC/CMOE
#
# But      : Ajouter une nouvelle variable dans le recrc (in memory).
#
# Parametres :
#   <Desc>   : Description de la variable
#   <Unit>   : Unite de la variable
#   <Factor> : Facteur de conversion
#   <Level>  : Liste des niveaux
#   <Inter>  : Liste des intervalles
#
# Retour    :
#
# Remarque :
#
#-------------------------------------------------------------------------------

proc MetStat::RECRCAdd { Var Desc Unit Factor Level Inter } {
   variable Rec

   if { $Var!="" }   {

      lappend Rec(Var) $Var

      if { $Desc==""   && ![info exists MetStat::Rec(Desc$Var)] }   { set Rec(Desc$Var)   $Rec(Desc) }
      if { $Level==""  && ![info exists MetStat::Rec(Level$Var)] }  { set Rec(Level$Var)  $Rec(Level) }
      if { $Inter==""  && ![info exists MetStat::Rec(Inter$Var)] }  { set Rec(Inter$Var)  $Rec(Inter) }

      if { $Desc!=""   || ![info exists MetStat::Rec(Desc$Var)] }   { set Rec(Desc$Var)   $Desc }
      if { $Level!=""  || ![info exists MetStat::Rec(Level$Var)] }  { set Rec(Level$Var)  $Level }
      if { $Inter!=""  || ![info exists MetStat::Rec(Inter$Var)] }  { set Rec(Inter$Var)  $Inter }

      set Rec(Unit$Var)   $Unit
      set Rec(Factor$Var) $Factor
      set Rec(Var) [lsort -unique -dictionary $Rec(Var)]
   }
}

#-------------------------------------------------------------------------------
# Nom      : <MetStat::RECRCEval>
# Creation : Septembre 2002 - J.P. Gauthier - CMC/CMOE
#
# But      : Extraire les parametres d'une commande recrc.
#
# Parametres :
#
# Retour    :
#
# Remarque :
#
#-------------------------------------------------------------------------------

proc MetStat::RECRCEval { Cmd } {
   variable Rec

   #----- Creer un liste d'arguments

   set arg [split $Cmd ","]
   set arg [string map { defvar "" DEFVAR "" "(" "" ")" "" "'" \" "," " " "[" "{" "]" "}" } $Cmd]

   #----- Extraire les parametres specifiques

   set var [string toupper [lindex $arg 0]]
   lappend Rec(Var)    $var

   set Rec(Desc$var)   [lindex $arg 1]
   set Rec(Unit$var)   [lindex $arg 2]
   set Rec(Factor$var) [expr double(1.0/[lindex $arg 4])]
   set Rec(Level$var)  ""
   set Rec(Inter$var)  ""

   #----- Extraire les definitions de niveaux

   set inter 0

   foreach token [lrange $arg 6 end] {
      if { [llength $token] >1 } {
         lappend Rec(Level$var) $token
      } else {
         lappend Rec(Inter$var) $token
      }
   }
}


#-------------------------------------------------------------------------------
# Nom      : <MetStat::RECRCLoad>
# Creation : Septembre 2002 - J.P. Gauthier - CMC/CMOE
#
# But      : Lit les definitions du fichier .recrc.
#
# Parametres :
#   <File>   : Fichier de definitions
#
# Retour    :
#
# Remarque :
#
#-------------------------------------------------------------------------------

proc MetStat::RECRCLoad { File } {
   global GDefs
   variable Rec
   variable Msg

   if { ![file exists $File] } {
      return
   }

   set file [open $File r]
   set line ""

   while { ![eof $file] } {

      set first [string range $line 0 0]
      if { $first=="#" || $first=="*" } {
         gets $file line
         continue
      }

      #----- Check for "vecteur" directive
      if { [string first "VECTEUR" [string toupper $line]]!=-1 } {
          fstdfield vector [string map { vecteur "" VECTEUR "" "(" "" ")" "" "'" \" "," " " } $line]
          gets $file line
      }

      #----- Check for "defvar" directive
      if { [string first "DEFVAR" [string toupper $line]]!=-1 } {

         set cmd $line
         gets $file line

         while { [string first "DEFVAR" [string toupper $line]]==-1 && [string first "END" [string toupper $line]]==-1 && [string first "GRILLE" [string toupper $line]]==-1 && ![eof $file] } {

            #----- Concatener toutes les lignes en une commande
            set first [string range $line 0 0]

            if { $first!="#" && $first!="*" } {
               set cmd "$cmd[string trim $line]"
            }
            gets $file line
         }

         #----- Verifier la structure de la commande
         if { [catch {MetStat::RECRCEval $cmd}] } {
            Dialog::Error . $Msg(RECRC) "\n\n$cmd"
         }
      } else {
         gets $file line
      }
   }
   close $file
}

proc MetStat::FreeList { Fields } {

   foreach field $Fields {
      fstdfield free $field
   }
}

proc MetStat::GetLevelList { Field DeltaH EndH } {

   set fid [fstdfield define $Field -FID]
   set eti [fstdfield define $Field -ETIKET]
   set ip1 [fstdfield define $Field -IP1]
   set tvr [fstdfield define $Field -TYPVAR]
   set nvr [fstdfield define $Field -NOMVAR]

   set st0 [fstdfield define $Field -DATEV]
   set st1 [fstdstamp incr $st0 $EndH]

   set list {}
   for { set i $st0 } { $i<$st1 } { set i [fstdstamp incr $i $DeltaH] } {
      fstdfield read STAT$fid$i $fid $i $eti $ip1 -1 -1 $tvr $nvr
      lappend list STAT$fid$i
   }
   return $list
}

proc MetStat::Type { Field } {
   variable Data

   if { [fstdfield is $Field] } {
      fstdfield copy STAT_RES$Data(No) $Field
      set data STAT_RES$Data(No)
      incr Data(No)
   } else {
      set data $Field
   }
   return $data
}

proc MetStat::Avg { Fields } {

   set res [MetStat::Sum $Fields]
   set res [vexpr $res $res/[llength $Fields]]

   return $res
}

proc MetStat::Min { Fields } {

   set res [MetStat::Type [lindex $Fields 0]]

   foreach field [lrange $Fields 1 end] {
      set res [vexpr $res ifelse($res<$field,$res,$field)]
   }
   return $res
}

proc MetStat::Max { Fields } {

   set res [MetStat::Type [lindex $Fields 0]]

   foreach field [lrange $Fields 1 end] {
      set res [vexpr $res ifelse($res>$field,$res,$field)]
   }
   return $res
}

proc MetStat::Sum { Fields } {

   set res [MetStat::Type [lindex $Fields 0]]

   foreach field [lrange $Fields 1 end] {
      set res [vexpr $res $res+$field]
   }

   return $res
}

proc MetStat::Calculate { X Y { Factor -1 } } {
   variable Stat

   set Stat(VARx)  0.0   ;#Variance X
   set Stat(VARy)  0.0   ;#Variance Y
   set Stat(VARxy) 0.0   ;#Covariance
   set Stat(STDxy) 0.0   ;#Standard deviation of x-y bias

   set Stat(SSxx)  0.0   ;#Sum of squared values X
   set Stat(SSyy)  0.0   ;#Sum of squared values Y
   set Stat(SSxy)  0.0   ;##Sum of squared values X-Y
   set Stat(S)     0.0   ;#
   set Stat(R2)    0.0   ;#Correlation coeficient
   set Stat(b)     0.0   ;#Regression coeficient (Slope of curve fitting)
   set Stat(a)     0.0   ;#Regression coeficient (Delta of curve fitting)
   set Stat(Eb)    0.0   ;#Standard Error for b
   set Stat(Ea)    0.0   ;#Standard Error for a
   set Stat(AVGx)  0.0   ;#Average of x
   set Stat(AVGy)  0.0   ;#Average of y
   set Stat(RMSE)  0.0   ;#Root mean square error
   set Stat(NRMSE) 0.0   ;#Normalized Root mean square error
   set Stat(ME)    0.0   ;#Mean error
   set Stat(NME)   0.0   ;#Normalized mean error
   set Stat(MNE)   0.0   ;#Mean normalized error
   set Stat(MB)    0.0   ;#Mean bias
   set Stat(NMB)   0.0   ;#Normalized mean bias
   set Stat(MNB)   0.0   ;#Mean normalized bias
   set Stat(MFB)   0.0   ;#Mean fractional bias
   set Stat(MFE)   0.0   ;#Mean fractional error
   set Stat(LMNB)  0.0   ;#Logarithmic mean normalized bias
   set Stat(LMNE)  0.0   ;#Logarithmic mean normalized error
   set Stat(n)     0     ;#Number of sample
   set Stat(NA)    0     ;#Number of station n.a. stations
   set Stat(RNA)   0.0   ;#ratio of observation completeness
   set omi         0     ;#flag de station omitted or not

   if { ![llength $X] || ![llength $Y] } {
      return 0
   }

   foreach x $X y $Y {

      # La liste des observations (X) contien des "-" quand la valeur
      # a ete extractee par un [observation define ... -DATA ...] qui
      # aura ete au prealable une valeur "-999"

      if { $x!="-" && $y!="-" } {
         set Stat(AVGx)  [expr $Stat(AVGx)+$x]
         set Stat(AVGy)  [expr $Stat(AVGy)+$y]

         set Stat(SSxx) [expr $Stat(SSxx)+($x*$x)]
         set Stat(SSyy) [expr $Stat(SSyy)+($y*$y)]
         set Stat(SSxy) [expr $Stat(SSxy)+($x*$y)]
         set Stat(RMSE) [expr $Stat(RMSE)+($y-$x)*($y-$x)]
         set Stat(MB)   [expr $Stat(MB)+($y-$x)]
         set Stat(ME)   [expr $Stat(ME)+abs($y-$x)]

         if { $x!=0 } {
            set Stat(MNB) [expr $Stat(MNB)+(($y-$x)/$x)]
            set Stat(MNE) [expr $Stat(MNE)+abs(($y-$x)/$x)]

            if { [set ratio [expr $y/$x]]>0 } {
               set Stat(LMNB) [expr $Stat(LMNB)+log($ratio)]
               set Stat(LMNE) [expr $Stat(LMNE)+abs(log($ratio))]
            }
         }

         if { [expr $y+$x]!=0 } {
            set Stat(MFB)  [expr $Stat(MFB)+(($y-$x)/($y+$x))]
            set Stat(MFE)  [expr $Stat(MFE)+(abs($y-$x)/($y+$x))]
         }

         incr Stat(n)
      } else {
         incr Stat(NA)
      }
   }

   # Calcul du ratio de completion

   set Stat(RNA) [expr double($Stat(n))/double([llength $X])]

   # Si le ratio de completion est inferieur au facteur de completion
   # desire, on arrete la suite des calculs.

   if { $Factor!=-1 && [expr double($Stat(n))/double([llength $X])]<$Factor } {
      return 0
   }

   # Si la somme des x n'est pas egale a zero les erreurs et biais moyen
   # normalize sont mis a zero

   if { $Stat(AVGx)!=0 } {
      set Stat(NMB)   [expr $Stat(MB)/$Stat(AVGx)*100]
      set Stat(NME)   [expr $Stat(ME)/$Stat(AVGx)*100]
   } else {
      set Stat(NMB)   0
      set Stat(NME)   0
   }

   # Calcul des statistiques moyennees

   set Stat(MB)    [expr $Stat(MB)/$Stat(n)]
   set Stat(ME)    [expr $Stat(ME)/$Stat(n)]
   set Stat(MNB)   [expr $Stat(MNB)/$Stat(n)]
   set Stat(MNE)   [expr $Stat(MNE)/$Stat(n)]
   set Stat(MFB)   [expr $Stat(MFB)*2/$Stat(n)]
   set Stat(MFE)   [expr $Stat(MFE)*2/$Stat(n)]
   set Stat(LMNB)  [expr exp($Stat(LMNB)/$Stat(n))-1]
   set Stat(LMNE)  [expr exp($Stat(LMNE)/$Stat(n))-1]
   set Stat(AVGx)  [expr $Stat(AVGx)/$Stat(n)]
   set Stat(AVGy)  [expr $Stat(AVGy)/$Stat(n)]
   set Stat(SSxx)  [expr $Stat(SSxx)-($Stat(n)*$Stat(AVGx)*$Stat(AVGx))]
   set Stat(SSyy)  [expr $Stat(SSyy)-($Stat(n)*$Stat(AVGy)*$Stat(AVGy))]
   set Stat(SSxy)  [expr $Stat(SSxy)-($Stat(n)*$Stat(AVGx)*$Stat(AVGy))]

   # Le calcul des variances et ecart-type

   foreach x $X y $Y {
      if { $x!="-" && $y!="-" } {
         set Stat(VARx)  [expr $Stat(VARx)+($x-$Stat(AVGx))*($x-$Stat(AVGx))]
         set Stat(VARy)  [expr $Stat(VARy)+($y-$Stat(AVGy))*($y-$Stat(AVGy))]
         set Stat(VARxy) [expr $Stat(VARxy)+($y-$Stat(AVGy))*($x-$Stat(AVGx))]
         set Stat(STDxy) [expr $Stat(STDxy)+(($x-$y)-$Stat(MB))*(($x-$y)-$Stat(MB))]
      }
   }

   set Stat(VARx)  [expr $Stat(VARx)/$Stat(n)]
   set Stat(VARy)  [expr $Stat(VARy)/$Stat(n)]
   set Stat(VARxy) [expr $Stat(VARxy)/$Stat(n)]
   set Stat(STDxy) [expr sqrt($Stat(STDxy)/$Stat(n))]

   if { $Stat(SSxx)<=0 || $Stat(SSyy)<=0 } {
      return
   }

   if { [expr $Stat(SSyy)-($Stat(SSxy)*$Stat(SSxy))]>=0 } {
      set Stat(S)    [expr sqrt(($Stat(SSyy)-($Stat(SSxy)*$Stat(SSxy))/$Stat(SSxx))/($Stat(n)-2))]
   }

   set Stat(RMSE)  [expr sqrt($Stat(RMSE)/$Stat(n))]
   set Stat(NRMSE) [expr $Stat(RMSE)/$Stat(AVGx)]
   set Stat(R2)    [expr ($Stat(SSxy)*$Stat(SSxy))/($Stat(SSxx)*$Stat(SSyy))]
   set Stat(b)     [expr $Stat(SSxy)/$Stat(SSxx)]
   set Stat(a)     [expr $Stat(AVGy)-$Stat(b)*$Stat(AVGx)]
   set Stat(Eb)    [expr $Stat(S)/sqrt($Stat(SSxx))]
   set Stat(Ea)    [expr $Stat(S)*sqrt(1.0/$Stat(n)+($Stat(AVGx)*$Stat(AVGx))/$Stat(SSxx))]

   return 1
}

if { [info exist env(ARMNLIB)] } {
   MetStat::RECRCLoad $env(ARMNLIB)/data/dict_rec.[lindex { f e } $GDefs(Lang)]
}
MetStat::RECRCLoad $env(HOME)/.recrc
