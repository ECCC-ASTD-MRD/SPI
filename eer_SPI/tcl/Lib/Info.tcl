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
#   Info::Delete  { Path Info { Safe True } }
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

package provide Info 2.2

catch { SPI::Splash "Loading Package Info 2.2" }

namespace eval Info {
   variable Lbl
   variable Msg
   variable Token

   set Token(TRAJECT) { Model State NoExp NoSim NoPrev NameExp Name Lat Lon Event By Blame Click AccSecs SimSecs MetSecs Duration \
                        Backward Mode Meteo Delta Level LevelUnit TimeStep BatchStart }

   set Token(MLDP)    { Model State NoExp NoSim NoPrev NameExp Name Lat Lon Event By Blame Click AccSecs SimSecs MetSecs Duration
                        Backward Mode Meteo Delta Scale Grid OutputTimeStepMin ModelTimeStepMin \
                        SrcType VerticalLevels VarMesoscale Timescale ReflectionLevel EmNumberParticles \
                        EmDensity EmHeight EmMass EmMassMode EmRadius EmSizeDist EmVerticalDist \
                        EmScenario EmNbIntervals EmTotalDuration EmEffectiveDuration EmNbIso EmIsoSymbol EmIsoQuantity }

   set Token(MLDPn)   { Model State NoExp NoSim NoPrev NameExp Name Coords Height Event By Blame Click AccSecs SimSecs Sim0Secs MetSecs Duration
                        Backward Mode Meteo Delta Scale Grid DiffKernel CritAge OutputTimeStepMin ModelTimeStep IsoChain \
                        SrcType OutVar OutCV OutAV VarMesoscale Timescale ReflectionLevel Seed Scenario 
                        Aerosol WetScaMode DryDepMode
                        OilFate OilBeach OilEntrainment OilEmulsion Depth WebTide }

   set Token(MLCD)    { Model State NoExp NoSim NoPrev NameExp Name Lat Lon Event By Blame Click AccSecs MetSecs DurMin \
                        Meteo ObsNbLevels OutputTimeStepMin ModelTimeStepMin IsConc GridType GridAlgo GridDomain VerticalLevels IsSigma \
                        ReflectionLevel EmNumberParticles EmMass EmIsoName EmDepVel EmHalfLife EmWetScav EmDurationMin EmBottom EmTop EmRadius FFModule }

   set Token(UrbanLS) { Model State NoExp NoSim NoPrev NameExp Name Coords Event By Blame Click AccSecs \
                        Backward Order DurMin OutputTimeStepMin IsotropicK TurbSchmidt LimDetRey SVDLimit EscapeTest DetRC Alpha KC0 \
                        DtSurTl ResetVeloc TolFactor NudgeWall NudgeGrnd NudgeCell ReleaseType WindDBGrid OutVars \
                        WindHeight WindSpd WindDir EmNumberParticles EmMass EmDepVel EmDurationMin EmBottom EmTop EmRadius EmDiffuse }

   set Token(NONE)    { Model State NoExp NoSim NoPrev NameExp Name Lat Lon Coords }
   set Token(ALL)     [lsort -unique [concat $Token(TRAJECT) $Token(MLDP) $Token(MLDPn) $Token(MLCD)]]

   set Msg(Info)                 { "Impossible de lire l'enregistrement d'informations de la simulation"
                                   "Could not read simulation information record" }

   set Lbl(AccSecs)              { "Date de l'accident" "Accident date" }
   set Lbl(SimSecs)              { "Date de simulation" "Simulation date" }
   set Lbl(Sim0Secs)             { "Date de simulation initiale" "Initial simulation date" }
   set Lbl(MetSecs)              { "Date initiale du modèle météo" "Initial meteorological model data" }
   set Lbl(Click)                { "Date de lancement" "Launch date" }
   set Lbl(Blame)                { "Lancé par" "Launch by" }

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
   set Lbl(DiffKernel)           { "Kernel de diffusion" "Diffusion kernel" }
   set Lbl(CritAge)              { "Changement d'ordre (s)" "Order switch (s)" }
   set Lbl(Seed)                 { "Semence initiale" "Initial seed" }
   set Lbl(Aerosol)              { "Type d'aérosol" "Aerosol type" }
   set Lbl(WetScaMode)           { "Mode de lessivage humide" "Wet scavenging mode" }
   set Lbl(DryDepMode)           { "Mode de dépot sec" "Dry deposition mode" }

   set Lbl(Output)               { "Sortie (min)" "Output (min)" }
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
   set Lbl(Coords)               { "Coordonnées" "Coordinates" }
   set Lbl(Elev)                 { "Élévation" "Elevation" }
   set Lbl(Height)               { "Altitude" "Altitude" }
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
   set Lbl(OutVar)               { "Variables à sauvegarder" "Variables to save" }
   set Lbl(OutCV)                { "Niveaux verticaux concentrations (m)" "Concentration vertical Levels (m)" }
   set Lbl(OutAV)                { "Niveaux verticaux aviation (ft)" "Aviations vertical Levels (ft)" }
   set Lbl(IsSigma)              { "Fluctuations des vitesses à méso-échelle" "Mesoscale velocity fluctuations" }

   set Lbl(FreqOut)              { "Fréquence des fichiers" "File frequency" }
   set Lbl(SrcType)              { "Type de source" "Type of source" }
   set Lbl(VirusName)            { "Nom du virus" "Virus name" }
   set Lbl(Event)                { "Type d'événement" "Type of event" }
   set Lbl(By)                   { "Requête par" "Request by" }
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
   set Lbl(IsoHalf)              { "Période de demi-vie radioactive (s)" "Radioactive half-life period (s)" }
   set Lbl(IsoDry)               { "Taux de lessivage sec (s -1)" "Dry scavenging rate (s -1)" }
   set Lbl(IsoWet)               { "Taux de lessivage humide (s -1)" "Wet scavenging rate (s -1)" }
   set Lbl(IsoRelease)           { "Quantité totale relâchée" "Total released quantity" }
   set Lbl(IsoChain)             { "Chaine de décomposition" "Decay chain" }

   set Lbl(NbSpecies)            { "Nombre d'espèces" "Number of species" }
   set Lbl(Species)              { "Espèces" "Species" }
   set Lbl(OutputTimeStepMin)    { "Pas de temps de sortie (min)" "Output time step (min)" }
   set Lbl(ModelTimeStepMin)     { "Pas de temps du modèle (min)" "Model time step (min)" }
   set Lbl(ModelTimeStep)        { "Pas de temps du modèle (s)" "Model time step (s)" }
   set Lbl(VerticalLevels)       { "Niveaux verticaux (m)" "Vertical levels (m)" }
   set Lbl(VarMesoscale)         { "Variance des vitesses horiz. des vents (m²/s²)" "Horiz. wind velocity variance (m²/s²)" }
   set Lbl(Timescale)            { "Échelle de temps lagrangienne (s)" "Lagrangian time scale (s)" }
   set Lbl(ReflectionLevel)      { "Niveau inférieur de réflexion (hyb|eta|sig)" "Bottom reflection level (hyb|eta|sig)" }

   set Lbl(EmMass)               { "Masse totale relâchée (unité)" "Total released mass (unit)" }
   set Lbl(EmIsoName)            { "Nom de l'isotope radioactif" "Name of the radionuclide" }
   set Lbl(EmDepVel)             { "Vitesse de dépôt (m/s)" "Deposition velocity (m/s)" }
   set Lbl(EmHalfLife)           { "Période de demi-vie radioactive (s)" "Radioactive half-life period (s)" }
   set Lbl(EmWetScav)            { "Facteur de lessivage humide" "Wet scavenging coefficient" }
   set Lbl(EmDuration)           { "Durée (s)" "Duration (s)" }
   set Lbl(EmDurationMin)        { "Durée d'émission (min)" "Release duration (min)" }
   set Lbl(EmBottom)             { "Bas de la colonne (m)" "Column bottom (m)" }
   set Lbl(EmTop)                { "Haut de la colonne (m)" "Column top (m)" }
   set Lbl(EmRadius)             { "Rayon de la colonne (m)" "Column radius (m)" }

   set Lbl(EmNumberParticles)    { "Nombre de particules" "Number of particles" }
   set Lbl(EmDensity)            { "Densité d'une particule (µg/m³)" "Density of a particle (µg/m³)" }
   set Lbl(EmHeight)             { "Hauteur maximale du panache (m)" "Maximum plume height (m)" }
   set Lbl(EmSizeDist)           { "Distribution de la taille des particules" "Size distribution of the particles" }
   set Lbl(EmVerticalDist)       { "Distribution verticale du panache" "Vertical plume distribution" }

   set Lbl(Scenario)             { "Scénario d'émission" "Release scenario" }
   set Lbl(EmScenario)           { "Nom du scénario d'émission" "Name of the release scenario" }
   set Lbl(EmNbIntervals)        { "Nombre d'intervalles" "Number of intervals" }
   set Lbl(EmNbIso)              { "Nombre d'isotopes" "Number of isotopes" }
   set Lbl(EmIsoSymbol)          { "Isotopes" "Isotopes" }
   set Lbl(EmIsoQuantity)        { "Quantité totale relâchée" "Total release quantity" }
   set Lbl(EmTotalDuration)      { "Durée totale d'émission (s)" "Total release duration (s)" }
   set Lbl(EmEffectiveDuration)  { "Durée effective d'émission (s)" "Effective release duration (s)" }
   set Lbl(EmMaxAge)             { "Age maximum (s)" "Maximum age (s)" }
   set Lbl(EmMinMass)            { "Masse minimale (unité)" "Minimum mass (unit)" }

   set Lbl(EmLabel)              { "Type de période" "Type of period" }
   set Lbl(EmReleaseRates)       { "Taux de relâchement (unité/h)" "Release rate (unit/h)" }

   set Lbl(FFModule)             { "Scénario Incendie/Carburant" "Fire/Fuel Scenario" }

   set Lbl(Order)                { "Ordre de la diffusion" "Diffusion order" }
   set Lbl(IsotropicK)           { "K isotropique" "Isotropic K" }
   set Lbl(TurbSchmidt)          { "Nombre de Schmidt" "Schmidt number" }
   set Lbl(LimDetRey)            { "Limite déterminant Rey" "Rey determinant limit" }
   set Lbl(SVDLimit)             { "Limite (DVS)" "Limit (SVD)" }
   set Lbl(EscapeTest)           { "Seuil d'évasion" "Escape threshold" }
   set Lbl(DetRC)                { "Cible déterminant Rey" "Rey determinant target"}
   set Lbl(Alpha)                { "Alpha" "Alpha"}
   set Lbl(KC0)                  { "Constante de Kolmogorov" "Kolmogorov constant"}

   set Lbl(DtSurTl)              { "DT/TL" "DT/TL" }
   set Lbl(ResetVeloc)           { "Correction vitesse" "Reset velocity" }
   set Lbl(TolFactor)            { "Facteur de tolérance" "Tolerence factor" }
   set Lbl(NudgeWall)            { "Ajustement (murs)" "Nudge (wall)"}
   set Lbl(NudgeGrnd)            { "Ajustement (sol)" "Nudge (ground)"}
   set Lbl(NudgeCell)            { "Ajustement (cellules)" "Nudge (cell)"}
   set Lbl(ReleaseType)          { "Type de relâchement" "Release type" }

   set Lbl(ClimatoGrid)          { "Grille climato" "Climato Grid" }
   set Lbl(OutVars)              { "Variables" "Variables" }
   set Lbl(WindHeight)           { "Hauteur du vent (m)" "Height (m)" }
   set Lbl(WindSpd)              { "Vitesse du vent (m/s)" "Speed (m/s)" }
   set Lbl(WindDir)              { "Direction (deg)" "Direction (deg)" }

   set Lbl(EmDiffuse)            { "Diffuse" "Diffuse" }
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
      if { [info exists var($item)] } {
         lappend info "$item=$var($item)"
      } else {
         lappend info "$item="
      }
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

   #----- Force unset on coords for aftercheck (not all models have Coords array)
   set var(Coords) {}
   set var(Lat)    {}
   set var(Lon)    {}
   
   foreach info $infos {
      set list  [split $info =]
      set token [lindex $list 0]
      set value [lindex $list 1]
      set found 0

      foreach { item descs } [array get Lbl] {
         foreach desc $descs {

            if { "$token"=="$desc" } {
               set var($item) $value
               set found 1
               break
            }
         }

         if { $found } {
            break
         }
      }

      if { !$found } {
         set var($token) $value
      }
   }

   #----- Fill up missing variables
   if { ![llength $var(Coords)] } {
      foreach lat $var(Lat) lon $var(Lon) {
         lappend var(Coords) [list $lat $lon]
      }
   }
   if { [llength $var(Lat)]<[llength $var(Coords)] } {
      foreach coord $var(Coords) {
         lappend var(Lat) [lindex $coord 0]
         lappend var(Lon) [lindex $coord 1]
      }
   }

   if { [info exists var(AccSecs)] } {
      set secs [lindex $var(AccSecs) 0]
      set var(AccYear)  [clock format $secs -format "%Y" -timezone :UTC]
      set var(AccMonth) [clock format $secs -format "%m" -timezone :UTC]
      set var(AccDay)   [clock format $secs -format "%d" -timezone :UTC]
      set var(AccHour)  [clock format $secs -format "%H" -timezone :UTC]
      set var(AccMin)   [clock format $secs -format "%M" -timezone :UTC]
   }
   if { [info exists var(SimSecs)] } {
      set var(SimYear)  [clock format $var(SimSecs) -format "%Y" -timezone :UTC]
      set var(SimMonth) [clock format $var(SimSecs) -format "%m" -timezone :UTC]
      set var(SimDay)   [clock format $var(SimSecs) -format "%d" -timezone :UTC]
      set var(SimHour)  [clock format $var(SimSecs) -format "%H" -timezone :UTC]
      set var(SimMin)   [clock format $var(SimSecs) -format "%M" -timezone :UTC]
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
#    <Safe>  : Keep backup
#
# Retour:
#
# Remarques :
#    Aucune.
#
#----------------------------------------------------------------------------

proc Info::Delete { Path Info { Safe True } } {

   if { [file exists $Path] } {
      file rename -force $Path $Path.del
      catch { exec grep -v "$Info" "$Path.del" > $Path }
      if { !$Safe } {
         file delete -force $Path.del
      }
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
   set line [lrepeat 15 {[^:]*}]

   #----- Initialiser les arguments de recherche specifies
   foreach { item value } $args {
      set idx [lsearch -exact $Token($Set) $item]

      if { $idx!=-1 && $idx<=15 } {
         set line [lreplace $line $idx $idx "\[^:\]*=$value"]
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
# Parametres  :
#    <Info>   : Ligne info dont il faut extraire quelque chose
#    <Length> : Longeur max des lignes (0= no limit)
#
# Retour:
#    <ligne> : ligne formatee
#
# Remarques :
#    Aucune.
#
#----------------------------------------------------------------------------

proc Info::Format { Info { Length 0 } } {
   global GDefs
   variable Lbl

   set list [split $Info ":"]

   set l 0
   foreach item $list {
      set ltx [split $item "="]
      set tok [lindex $ltx 0]
      catch { set tok [lindex $Lbl($tok) $GDefs(Lang)] }

      set len [string length $tok]
      set l   [expr $l<$len?$len:$l]
   }

   set text ""
   foreach item $list {
      set ltx [split $item "="]
      set tok [lindex $ltx 0]
      set val [lindex $ltx 1]

      set lbl $tok
      catch { set lbl [lindex $Lbl($tok) $GDefs(Lang)] }

      switch $tok {
         "Click" -
         "Sim0Secs" -
         "SimSecs" -
         "AccSecs" { set val [clock format [lindex $val 0] -timezone :UTC] }
      }

      #----- For scenario, only show the name (1st item)
      if { $tok=="Scenario" } {
         set val [lindex [split $val |] 0]
      }
      if { $Length && [string length $val]>$Length } {
         set val "[string range $val 0 $Length] ..."
      }
      set text "$text[format "%-${l}s" $lbl] = $val\n"
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

   if { [info exists ::Info::Tmp(AccSecs)] } {
      return "$Tmp(Model).$Tmp(NoSim).[clock format $Tmp(AccSecs) -format %Y%m%d.%H%M -timezone :UTC]"
   } else {
      return "$Tmp(Model).$Tmp(NoSim).$Tmp(AccYear)$Tmp(AccMonth)$Tmp(AccDay).$Tmp(AccHour)$Tmp(AccMin)"
   }
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

   #----- Recuperer le champs INFO
   if { [catch { fstdfield read INFO $Id -1 "" -1 -1 -1 "" "INFO" }] } {
      Dialog::Error . $Msg(Info)
      return ""
   }


   #----- Extraire les donnees

   set info ""
   set nbit [fstdfield define INFO -NBITS]
   foreach i [join [fstdfield define INFO -DATA 0]] {
      append info [format "%c" [expr int(($nbit==8 && $i>127)?$i-128:$i)]]
   }
   fstdfield free INFO
   return [string trimright $info \n]
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
      set Info "[lindex $list 0]:State=${State}:[join [lrange $list 2 end] :]"
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

   if { [set idx [lsearch -glob $list "${Token}=*"]]!=-1 } {
      return [lindex [split [lindex $list $idx] =] end]
   }
   return ""
}
