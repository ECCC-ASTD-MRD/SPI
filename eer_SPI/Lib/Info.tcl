#===============================================================================
# Environnement Canada
# Centre Meteorologique Canadian
# 2100 Trans-Canadienne
# Dorval, Quebec
#
# Projet   : Librairie de fonctions Tcl relatives aux enregistrements info
# Fichier  : Info.tcl
# Creation : Avril 2003 - J.P.Gauthier - CMC/CMOE
#
# Description:
#    Definitions de diverses fonctionnalites relatives  aux enregistrements info.
#
# Fonctions:
#
#   Info::Code    { Var { Separator : } }
#   Info::Decode  { Var Info }
#   Info::Delete  { Path Info }
#   Info::Find    { Path Set args }
#   Info::Format  { Info }
#   Info::List    { Path }
#   Info::Path    { Info }
#   Info::Read    { Id }
#   Info::Request { Path }
#   Info::Set     { Path Info { State "" }}
#   Info::Strip   { Info Idx }
#
# Remarques :
#
#===============================================================================

package provide Info 2.0

catch { SPI::Splash "Loading Package Info 2.0" }

namespace eval Info {
   variable Lbl
   variable Msg
   variable Token

   set Token(NONE)    { Model State NoExp NoSim NoPrev NameExp Name Lat Lon }

   set Token(CANERM)  { Model State NoExp NoSim NoPrev NameExp Name Lat Lon Duration AccYear AccMonth AccDay AccHour AccMin \
                        SimYear SimMonth SimDay SimHour Mode Meteo Delta Scale Grid FreqOut EmHeight Event NbPer Dt ISauve \
                        DTIN DTIS FnVert FnTime EmDuration Delai IType1 IType2 IsoName IsoRelease IsoUnit IsoHalf IsoDry IsoWet }

   set Token(TRAJECT) { Model State NoExp NoSim NoPrev NameExp Name Lat Lon Duration AccYear AccMonth AccDay AccHour AccMin \
                        Backward Mode Meteo Delta Level LevelUnit TimeStep BatchStart }

   set Token(MLDP)    { Model State NoExp NoSim NoPrev NameExp Name Lat Lon Duration AccYear AccMonth AccDay AccHour AccMin
                        SimYear SimMonth SimDay SimHour Backward Mode Meteo Delta Scale Grid OutputTimeStepMin ModelTimeStepMin \
                        Event SrcType VerticalLevels VarMesoscale Timescale ReflectionLevel EmNumberParticles \
                        EmDensity EmHeight EmMass EmRadius EmSizeDist EmVerticalDist \
                        EmScenario EmNbIntervals EmTotalDuration EmEffectiveDuration EmNbIso EmIsoSymbol EmIsoQuantity }

   set Token(MLCD)    { Model State NoExp NoSim NoPrev NameExp Name Lat Lon DurMin AccYear AccMonth AccDay AccHour AccMin \
                        Meteo ObsNbLevels OutputTimeStepMin ModelTimeStepMin IsConc GridType GridAlgo GridDomain VerticalLevels IsSigma \
                        EmNumberParticles EmTotMass EmIsoName EmDepVel EmHalfLife EmWetScav EmDurationMin EmBottom EmTop EmRadius }

   set Msg(Info)                 { "Impossible de lire l'enregistrement d'informations de la simulation"
                                   "Could not read simulation information record" }

   set Lbl(Method)               { "Méthode" "Method" }
   set Lbl(Backward)             { "Mode arrière" "Backward mode" }
   set Lbl(Mode)                 { "Mode" "Mode" }
   set Lbl(Meteo)                { "Modèle météorologique" "Meteorological model" }
   set Lbl(Delta)                { "Interval de temps des fichiers météo (h)" "Time interval of meteo files (h)" }
   set Lbl(Start)                { "Départ" "Start" }
   set Lbl(Arrival)              { "Arrivée" "Arrival" }
   set Lbl(Level)                { "Niveau" "Level" }
   set Lbl(LevelUnit)            { "Unité" "Unit" }
   set Lbl(BatchStart)           { "Délai de démarrage (h)" "Batch start delay (h)" }
   set Lbl(Duration)             { "Durée de simulation (h)" "Simulation duration (h)" }
   set Lbl(DurMin)               { "Durée de simulation (min)" "Simulation duration (min)" }
   set Lbl(TimeStep)             { "Pas de temps interne (s)" "Internal time step (s)" }

   set Lbl(Output)               { "Sortie (min)" "Output (min)" }
   set Lbl(EmTotMass)            { "Masse totale relâchée (unité)" "Total released mass (unit)" }
   set Lbl(EmIsoName)            { "Nom de l'isotope radioactif" "Name of the radionuclide" }
   set Lbl(EmDepVel)             { "Vitesse de dépôt (m/s)" "Deposition velocity (m/s)" }
   set Lbl(EmHalfLife)           { "Période de demi-vie radioactive (s)" "Radioactive half-life period (s)" }
   set Lbl(EmWetScav)            { "Facteur de lessivage humide" "Wet scavenging coefficient" }
   set Lbl(EmDuration)           { "Durée (s)" "Duration (s)" }
   set Lbl(EmDurationMin)        { "Durée d'émission (min)" "Release duration (min)" }
   set Lbl(EmBottom)             { "Bas de la colonne (m)" "Column bottom (m)" }
   set Lbl(EmTop)                { "Haut de la colonne (m)" "Column top (m)" }
   set Lbl(EmRadius)             { "Rayon de la colonne (m)" "Column radius (m)" }
   set Lbl(ObsRough)             { "Rugosité (m)" "Roughness (m)" }
   set Lbl(ObsObukhov)           { "Obukhov (m)" "Obukhov (m)" }
   set Lbl(ObsPrecip)            { "Précipitation (mm/h)" "Precipitation (mm/h)" }
   set Lbl(Obs)                  { "Profil des vents" "Wind Profile" }
   set Lbl(ObsNbLevels)          { "Nb niveaux" "Nb of levels" }

   set Lbl(Model)                { "Modèle" "Model" }
   set Lbl(State)                { "État de la simulation" "Simulation state" }
   set Lbl(NoExp)                { "Numéro de l'expérience" "Experience number" }
   set Lbl(NoSim)                { "Numéro de la simulation" "Simulation number" }
   set Lbl(NoPrev)               { "Simulation précédente" "Previous simulation" }
   set Lbl(NameExp)              { "Nom de l'experience" "Experiment name" }
   set Lbl(Name)                 { "Nom de l'emplacement" "Location name" }
   set Lbl(Lat)                  { "Latitude" "Latitude" }
   set Lbl(Lon)                  { "Longitude" "Longitude" }
   set Lbl(Elev)                 { "Élévation" "Elevation" }
   set Lbl(AccYear)              { "Année de l'accident" "Accident year" }
   set Lbl(AccMonth)             { "Mois de l'accident" "Accident month" }
   set Lbl(AccDay)               { "Jour de l'accident" "Accident day" }
   set Lbl(AccHour)              { "Heure de l'accident" "Accident hour" }
   set Lbl(AccMin)               { "Minute de l'accident" "Accident minute" }
   set Lbl(SimYear)              { "Année de simulation" "Simulation year" }
   set Lbl(SimMonth)             { "Mois de simulation" "Simulation month" }
   set Lbl(SimDay)               { "Jour de simulation" "Simulation day" }
   set Lbl(SimHour)              { "Heure de simulation" "Simulation hour" }
   set Lbl(Scale)                { "Nom de la grille" "Grid name" }
   set Lbl(GridResolution)       { "Résolution de la grille (km)" "Grid resolution (km)" }
   set Lbl(GridSize)             { "Taille de la grille (NIxNJ)" "Grid size (NIxNJ)" }
   set Lbl(Grid)                 { "Définition de grille" "Grid definition" }

   set Lbl(IsConc)               { "Calcul des concentrations" "Concentrations calculations" }
   set Lbl(GridType)             { "Type de grille" "Type of grid" }
   set Lbl(GridAlgo)             { "Type d'algorithme" "Type of algorithm" }
   set Lbl(GridDomain)           { "Domaine (km)" "Domain (km)" }
   set Lbl(VerticalLevels)       { "Niveaux verticaux (m)" "Vertical Levels (m)" }
   set Lbl(IsSigma)              { "Fluctuations des vitesses à méso-échelle" "Mesoscale velocity fluctuations" }

   set Lbl(FreqOut)              { "Fréquence des fichiers" "File frequency" }
   set Lbl(SrcType)              { "Type de source" "Type of source" }
   set Lbl(VirusName)            { "Nom du virus" "Virus name" }
   set Lbl(Event)                { "Type d'événement" "Type of event" }
   set Lbl(Scenario)             { "Scénario" "Scenario" }
   set Lbl(NbPer)                { "Nombre de période" "Number of period" }
   set Lbl(Dt)                   { "Délai entre les périodes" "Delay between periods" }
   set Lbl(ISauve)               { "Intervalle de sortie" "Output interval" }
   set Lbl(DTIN)                 { "DTIN" "DTIN" }
   set Lbl(DTIS)                 { "DTIS" "DTIS" }
   set Lbl(FnVert)               { "Distribution verticale" "Vertical distribution" }
   set Lbl(FnTime)               { "Distribution temporelle" "Time distribution" }
   set Lbl(Delai)                { "Délai d'emission" "Emission delai" }
   set Lbl(IType1)               { "IType1" "IType1" }
   set Lbl(IType2)               { "IType2" "IType2" }

   set Lbl(IsoNb)                { "Nombre d'isotopes" "Number of isotopes" }
   set Lbl(IsoName)              { "Isotope" "Isotope" }
   set Lbl(IsoUnit)              { "Unité" "Unit" }
   set Lbl(IsoHalf)              { "Période de demi-vie radioactive (s)" "Radioactive half-life period (s)" }
   set Lbl(IsoDry)               { "Taux de lessivage sec (s -1)" "Dry scavenging rate (s -1)" }
   set Lbl(IsoWet)               { "Taux de lessivage humide (s -1)" "Wet scavenging rate (s -1)" }
   set Lbl(IsoRelease)           { "Quantité totale relâchée" "Total released quantity" }

   set Lbl(NbSpecies)            { "Nombre d'espèces" "Number of species" }
   set Lbl(Species)              { "Espèces" "Species" }
   set Lbl(OutputTimeStepMin)    { "Pas de temps de sortie (min)" "Output time step (min)" }
   set Lbl(ModelTimeStepMin)     { "Pas de temps du modèle (min)" "Model time step (min)" }
   set Lbl(VerticalLevels)       { "Niveaux verticaux (m)" "Vertical levels (m)" }
   set Lbl(VarMesoscale)         { "Variance des vitesses horiz. des vents (m²/s²)" "Horiz. wind velocity variance (m²/s²)" }
   set Lbl(Timescale)            { "Échelle de temps lagrangienne (s)" "Lagrangian time scale (s)" }
   set Lbl(ReflectionLevel)      { "Niveau inférieur de réflexion (hyb|eta|sig)" "Bottom reflection level (hyb|eta|sig)" }

   set Lbl(EmNumberParticles)    { "Nombre de particules" "Number of particles" }
   set Lbl(EmDensity)            { "Densité d'une particule (µg/m³)" "Density of a particle (µg/m³)" }
   set Lbl(EmHeight)             { "Hauteur maximale du panache (m)" "Maximum plume height (m)" }
   set Lbl(EmMass)               { "Masse totale relâchée (µg)" "Total released mass (µg)" }
   set Lbl(EmSizeDist)           { "Distribution de la taille des particules" "Size distribution of the particles" }
   set Lbl(EmVerticalDist)       { "Distribution verticale du panache" "Vertical plume distribution" }

   set Lbl(EmScenario)           { "Nom du scénario d'émission" "Name of the release scenario" }
   set Lbl(EmNbIntervals)        { "Nombre d'intervalles" "Number of intervals" }
   set Lbl(EmNbIso)              { "Nombre d'isotopes" "Number of isotopes" }
   set Lbl(EmIsoSymbol)          { "Isotopes" "Isotopes" }
   set Lbl(EmIsoQuantity)        { "Quantité totale relâchée" "Total release quantity" }
   set Lbl(EmTotalDuration)      { "Durée totale d'émission (s)" "Total release duration (s)" }
   set Lbl(EmEffectiveDuration)  { "Durée effective d'émission (s)" "Effective release duration (s)" }
   set Lbl(EmDuration)           { "Durée d'émission" "Emission duration" }

   set Lbl(EmLabel)              { "Type de période" "Type of period" }
   set Lbl(EmReleaseRates)       { "Taux de relâchement (unité/h)" "Release rate (unit/h)" }
}

#----------------------------------------------------------------------------
# Nom      : <Info::Code>
# Creation : Avril 2003 - J.P. Gauthier - CMC/CMOE
#
# But      : Coder une ligne info.
#
# Parametres :
#    <Var>       : Variable array a assigner
#    <Separator> : Token de separation
#
# Retour:
#
# Remarques :
#    Aucune.
#
#----------------------------------------------------------------------------

proc Info::Code { Var { Separator : } } {
   global   GDefs
   variable Lbl
   variable Token

   upvar $Var var

   set info {}
   set model [string trimright $var(Model) 01]

   foreach item $Token($model) {
      lappend info "[lindex $Lbl($item) $GDefs(Lang)]=$var($item)"
   }

   if { $Separator!="" } {
      return [join $info $Separator]
   } else {
      return $info
   }
}

#----------------------------------------------------------------------------
# Nom      : <Info::Decode>
# Creation : Avril 2003 - J.P. Gauthier - CMC/CMOE
#
# But      : Decoder une ligne info.
#
# Parametres :
#    <Var>   : Variable array a assigner
#    <Info>  : Ligne info dont il faut extraire quelque chose
#
# Retour:
#
# Remarques :
#    Aucune.
#
#----------------------------------------------------------------------------

proc Info::Decode { Var Info } {
   global   GDefs
   variable Lbl
   variable Token

   upvar $Var var

   set infos [split $Info :]
   set model [string trimright [lindex [split [lindex $infos 0] =] 1] 01]

   foreach info $infos {
      set list  [split $info =]
      set token [lindex $list 0]
      set value [lindex $list 1]
      set found 0

      foreach item $Token($model) {
         foreach description $Lbl($item) {

            if { "$token"=="$description" } {
               set var($item) $value
               set found 1
               break
            }
         }

         if { $found } {
            break
         }
      }
   }
}

#----------------------------------------------------------------------------
# Nom      : <Info::Delete>
# Creation : Avril 2003 - J.P. Gauthier - CMC/CMOE
#
# But      : Supprime une ligne du fichier Info.
#
# Parametres :
#    <Path>  : Path complet du fichier info
#    <Info>  : Ligne a supprimer
#
# Retour:
#
# Remarques :
#    Aucune.
#
#----------------------------------------------------------------------------

proc Info::Delete { Path Info } {

   if { [file exists $Path] } {
      file rename -force $Path "$Path.old"
      catch { exec grep -v "$Info" "$Path.old" > $Path }
   }
}

#----------------------------------------------------------------------------
# Nom      : <Info::Find>
# Creation : Avril 2003 - J.P. Gauthier - CMC/CMOE
#
# But      : Lit le fichier info et retourne une liste comprenant toute
#            les lignes se comformant a la liste des parametres passe.
#
# Parametres :
#    <Path>  : Path complet du fichier info
#    <Set>   : Set de token a utiliser
#    <args>  : Liste des parametres de recherches
#
# Retour:
#    <list>  : Liste des lignes info correspondantes
#
# Remarques :
#    -Seul les 15 premiers items du pool peuvent servir de token de recherche,
#     ce sont les seuls communs a tous les modeles
#
#----------------------------------------------------------------------------

proc Info::Find { Path Set args } {
   variable Token

   #----- Initialiser le tableau d'arguments de recherche
   set line [list .* .* .* .* .* .* .* .* .* .* .* .* .* .* .*]

   #----- Initialiser les arguments de recherche specifies
   foreach { item value } $args {
      set idx [lsearch -exact $Token($Set) $item]

      if { $idx!=-1 && $idx<=15 } {
         set line [lreplace $line $idx $idx ".*=$value"]
      }
   }

   #----- Effectuer la recherche correspondant aux specifications
   if { ![catch { set list [exec egrep "^[join $line :]" $Path] }] } {
      return [split $list \n]
   } else {
      return ""
   }
}

#----------------------------------------------------------------------------
# Nom      : <Info::Format>
# Creation : Avril 2003 - J.P. Gauthier - CMC/CMOE
#
# But      : Format une ligne info pour affichage.
#
# Parametres :
#    <Info>  : Ligne info dont il faut extraire quelque chose
#
# Retour:
#    <ligne> : ligne formatee
#
# Remarques :
#    Aucune.
#
#----------------------------------------------------------------------------

proc Info::Format { Info } {

   set list [split $Info ":"]

   set l 0
   foreach item $list {
      set ltx [split $item "="]
      set len [string length [lindex $ltx 0]]
      set l   [expr $l<$len?$len:$l]
   }
   set text ""
   foreach item $list {
      set ltx [split $item "="]
      set text "$text[format "%-${l}s" [lindex $ltx 0]] = [lindex $ltx 1]\n"
   }
   return $text
}

#----------------------------------------------------------------------------
# Nom      : <Info::List>
# Creation : Avril 2003 - J.P. Gauthier - CMC/CMOE
#
# But      : Lit le fichier info et retourne une liste comprenant toute
#            les lignes.
#
# Parametres :
#    <Path>  : Path complet du fichier pool
#
# Retour:
#    <list>  : Contenu du fichier sous forme de liste
#
# Remarques :
#    Aucune.
#
#----------------------------------------------------------------------------

proc Info::List { Path } {

   set list ""

   if { [file exists $Path] } {
      set file [open $Path r]

      while { ![eof $file] } {
         gets $file line
         set line  [string trim $line]
         if { [string length $line] && [string index $line 0]!="#" } {
            lappend list $line
         }
      }
      close $file
   }
   return $list
}

#----------------------------------------------------------------------------
# Nom      : <Info::Path>
# Creation : Avril 2003 - J.P. Gauthier - CMC/CMOE
#
# But      : Decoder le path d'une simulation a partir d'une ligne info.
#
# Parametres :
#    <Info>  : Ligne info dont il faut extraire quelque chose
#
# Retour:
#    <path>  : Path correspondant
#
# Remarques :
#    Aucune.
#
#----------------------------------------------------------------------------

proc Info::Path { Info } {
   variable Tmp

   Info::Decode ::Info::Tmp $Info
   return "$Tmp(Model).$Tmp(NoSim).$Tmp(AccYear)$Tmp(AccMonth)$Tmp(AccDay).$Tmp(AccHour)$Tmp(AccMin)"
}

#----------------------------------------------------------------------------
# Nom      : <Info::Read>
# Creation : Avril 2003 - J.P. Gauthier - CMC/CMOE
#
# But      : Decode la ligne info codee dans un fichier standard.
#
# Parametres :
#    <Id>    : Id du fichier
#
# Retour:
#    <pool>  : Ligne de pool decodee
#
# Remarques :
#    Aucune.
#
#----------------------------------------------------------------------------

proc Info::Read { Id } {
   global   GDefs
   variable Msg

   if { $Id<0 } {
      return ""
   }

   #----- Recuperer le champs OL ou INFO

   if { [catch { fstdfield read INFO $Id -1 "" -1 -1 -1 "" "OL" }] } {
      if { [catch { fstdfield read INFO $Id -1 "" -1 -1 -1 "" "INFO" }] } {
         Dialog::CreateError . $Msg(Info)
         return ""
      }
   }

   #----- Extraire les donnees

   set info ""
   foreach i [join [fstdfield define INFO -DATA]] {
      append info [format "%c" [expr int($i)]]
   }

   fstdfield free INFO
   return $info
}

#----------------------------------------------------------------------------
# Nom      : <Info::Request>
# Creation : Avril 2003 - J.P. Gauthier - CMC/CMOE
#
# But      : Execute une requete de numero de simulation en recherchant un
#            numero libre dans le fichier info.
#
# Parametres :
#    <Path>  : Path complet du fichier info
#
# Retour    :
#    <No>   : Numero de simulation valide
#
# Remarques :
#    Aucune.
#
#----------------------------------------------------------------------------

proc Info::Request { Path } {

   #----- Lire le fichiers d'experiences

   set sims [Info::List $Path]

   #----- Extraire les numero d'experience (idx=3)

   set nos ""
   foreach sim $sims {
      lappend nos [Info::Strip $sim NoSim]
   }

   #----- Rechercher un numero libre

   set no 0
   while { [lsearch -exact $nos $no]!=-1 } {
      incr no
   }
   return $no
}

#----------------------------------------------------------------------------
# Nom      : <Info::Set>
# Creation : Avril 2003 - J.P. Gauthier - CMC/CMOE
#
# But      : Change l'etat d'un simulation dans le fichier info.
#
# Parametres  :
#    <Path>   : Path complet du fichier pool
#    <Info>   : Ligne de description de la simulation
#    <State>  : Etat de la simulation
#
# Retour:
#
# Remarques :
#    Aucune.
#
#----------------------------------------------------------------------------

proc Info::Set { Path Info { State "" } } {
   global GDefs
   variable Lbl

   set list [split $Info :]

   if { [file exists $Path] } {
      file rename -force $Path $Path.old
      catch { exec grep -v "^[lindex $list 0]:.*:[join [lrange $list 2 end] :]" $Path.old > $Path }
   }

   if { $State!="" } {
      set Info "[lindex $list 0]:[lindex $Lbl(State) $GDefs(Lang)]=${State}:[join [lrange $list 2 end] :]"
   }
   exec echo $Info >> $Path
}

#----------------------------------------------------------------------------
# Nom      : <Info::Strip>
# Creation : Avril 2003 - J.P. Gauthier - CMC/CMOE
#
# But      : Extrait un parametre d'une ligne pool par le libelle.
#
# Parametres :
#    <Info>  : Ligne info dont il faut extraire quelque chose
#    <Token> : Nom du parametre a extraire
#
# Retour:
#    <extr>  : Parametre extrait
#
# Remarques :
#    Aucune.
#
#----------------------------------------------------------------------------

proc Info::Strip { Info Token } {
   global GDefs
   variable Lbl

   set list  [split $Info :]

   foreach token $Lbl($Token) {
      if { [set idx [lsearch -glob $list "${token}=*"]]!=-1 } {
         return [lindex [split [lindex $list $idx] =] end]
      }
   }
   return ""
}
