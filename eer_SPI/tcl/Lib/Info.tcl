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

   set Token(TRAJECT) { Model State NoExp NoSim NoPrev NameExp Name Lat Lon Event By Blame Click AccSecs SimSecs Duration \
                        Backward Mode Meteo Delta Level LevelUnit TimeStep BatchStart }

   set Token(MLDP)    { Model State NoExp NoSim NoPrev NameExp Name Lat Lon Event By Blame Click AccSecs SimSecs Duration
                        Backward Mode Meteo Delta Scale Grid OutputTimeStepMin ModelTimeStepMin \
                        SrcType VerticalLevels VarMesoscale Timescale ReflectionLevel EmNumberParticles \
                        EmDensity EmHeight EmMass EmMassMode EmRadius EmSizeDist EmVerticalDist \
                        EmScenario EmNbIntervals EmTotalDuration EmEffectiveDuration EmNbIso EmIsoSymbol EmIsoQuantity }

   set Token(MLDPn)   { Model State NoExp NoSim NoPrev NameExp Name Coords Height Event By Blame Click AccSecs SimSecs Sim0Secs Duration
                        Backward Mode Meteo Delta Scale Grid DiffKernel OutputTimeStepMin ModelTimeStepMin IsoChain \
                        SrcType Aerosol OutVar OutCV OutAV VarMesoscale Timescale ReflectionLevel Seed Scenario Aerosol WetScaMode DryDepMode }

   set Token(MLCD)    { Model State NoExp NoSim NoPrev NameExp Name Lat Lon Event By Blame Click AccSecs DurMin \
                        Meteo ObsNbLevels OutputTimeStepMin ModelTimeStepMin IsConc GridType GridAlgo GridDomain VerticalLevels IsSigma \
                        EmNumberParticles EmMass EmIsoName EmDepVel EmHalfLife EmWetScav EmDurationMin EmBottom EmTop EmRadius }

   set Token(NONE)    { Model State NoExp NoSim NoPrev NameExp Name Lat Lon Coords }
   set Token(ALL)     [lsort -unique [concat $Token(TRAJECT) $Token(MLDP) $Token(MLDPn) $Token(MLCD)]]

   set Msg(Info)                 { "Impossible de lire l'enregistrement d'informations de la simulation"
                                   "Could not read simulation information record" }

   set Lbl(AccSecs)              { "Date de l'accident" "Accident date" }
   set Lbl(SimSecs)              { "Date de simulation" "Simulation date" }
   set Lbl(Sim0Secs)             { "Date de simulation initiale" "Initial simulation date" }
   set Lbl(Click)                { "Date de lancement" "Launch date" }
   set Lbl(Blame)                { "Lanc� par" "Launch by" }

   set Lbl(Method)               { "M�thode" "Method" }
   set Lbl(Backward)             { "Mode arri�re" "Backward mode" }
   set Lbl(Mode)                 { "Mode" "Mode" }
   set Lbl(Meteo)                { "Mod�le m�t�orologique" "Meteorological model" }
   set Lbl(Delta)                { "Interval de temps des fichiers m�t�o (h)" "Time interval of meteo files (h)" }
   set Lbl(Start)                { "D�part" "Start" }
   set Lbl(Arrival)              { "Arriv�e" "Arrival" }
   set Lbl(Level)                { "Niveau" "Level" }
   set Lbl(LevelUnit)            { "Unit�" "Unit" }
   set Lbl(BatchStart)           { "D�lai de d�marrage (h)" "Batch start delay (h)" }
   set Lbl(Duration)             { "Dur�e de simulation (h)" "Simulation duration (h)" }
   set Lbl(DurMin)               { "Dur�e de simulation (min)" "Simulation duration (min)" }
   set Lbl(TimeStep)             { "Pas de temps interne (s)" "Internal time step (s)" }
   set Lbl(DiffKernel)           { "Kernel de diffusion" "Diffusion kernel" }
   set Lbl(Seed)                 { "Graine al�atoire" "Random seed" }
   set Lbl(Aerosol)              { "Type d'a�rosol" "Aerosol type" }
   set Lbl(WetScaMode)           { "Mode de lessivage humide" "Wet scavenging mode" }
   set Lbl(DryDepMode)           { "Mode de d�pot sec" "Dry deposition mode" }

   set Lbl(Output)               { "Sortie (min)" "Output (min)" }
   set Lbl(ObsRough)             { "Rugosit� (m)" "Roughness (m)" }
   set Lbl(ObsObukhov)           { "Obukhov (m)" "Obukhov (m)" }
   set Lbl(ObsPrecip)            { "Pr�cipitation (mm/h)" "Precipitation (mm/h)" }
   set Lbl(Obs)                  { "Profil des vents" "Wind Profile" }
   set Lbl(ObsNbLevels)          { "Nb niveaux" "Nb of levels" }

   set Lbl(Model)                { "Mod�le" "Model" }
   set Lbl(State)                { "�tat de la simulation" "Simulation state" }
   set Lbl(NoExp)                { "Num�ro de l'exp�rience" "Experience number" }
   set Lbl(NoSim)                { "Num�ro de la simulation" "Simulation number" }
   set Lbl(NoPrev)               { "Simulation pr�c�dente" "Previous simulation" }
   set Lbl(NameExp)              { "Nom de l'experience" "Experiment name" }
   set Lbl(Name)                 { "Nom de l'emplacement" "Location name" }
   set Lbl(Lat)                  { "Latitude" "Latitude" }
   set Lbl(Lon)                  { "Longitude" "Longitude" }
   set Lbl(Coords)               { "Coordonn�es" "Coordinates" }
   set Lbl(Elev)                 { "�l�vation" "Elevation" }
   set Lbl(Height)               { "Altitude" "Altitude" }
   set Lbl(AccYear)              { "Ann�e de l'accident" "Accident year" }
   set Lbl(AccMonth)             { "Mois de l'accident" "Accident month" }
   set Lbl(AccDay)               { "Jour de l'accident" "Accident day" }
   set Lbl(AccHour)              { "Heure de l'accident" "Accident hour" }
   set Lbl(AccMin)               { "Minute de l'accident" "Accident minute" }
   set Lbl(SimYear)              { "Ann�e de simulation" "Simulation year" }
   set Lbl(SimMonth)             { "Mois de simulation" "Simulation month" }
   set Lbl(SimDay)               { "Jour de simulation" "Simulation day" }
   set Lbl(SimHour)              { "Heure de simulation" "Simulation hour" }
   set Lbl(Scale)                { "Nom de la grille" "Grid name" }
   set Lbl(GridResolution)       { "R�solution de la grille (km)" "Grid resolution (km)" }
   set Lbl(GridSize)             { "Taille de la grille (NIxNJ)" "Grid size (NIxNJ)" }
   set Lbl(Grid)                 { "D�finition de grille" "Grid definition" }

   set Lbl(IsConc)               { "Calcul des concentrations" "Concentrations calculations" }
   set Lbl(GridType)             { "Type de grille" "Type of grid" }
   set Lbl(GridAlgo)             { "Type d'algorithme" "Type of algorithm" }
   set Lbl(GridDomain)           { "Domaine (km)" "Domain (km)" }
   set Lbl(VerticalLevels)       { "Niveaux verticaux (m)" "Vertical Levels (m)" }
   set Lbl(OutVar)               { "Variables � sauvegarder" "Variables to save" }
   set Lbl(OutCV)                { "Niveaux verticaux concentrations (m)" "Concentration vertical Levels (m)" }
   set Lbl(OutAV)                { "Niveaux verticaux aviation (ft)" "Aviations vertical Levels (ft)" }
   set Lbl(IsSigma)              { "Fluctuations des vitesses � m�so-�chelle" "Mesoscale velocity fluctuations" }

   set Lbl(FreqOut)              { "Fr�quence des fichiers" "File frequency" }
   set Lbl(SrcType)              { "Type de source" "Type of source" }
   set Lbl(VirusName)            { "Nom du virus" "Virus name" }
   set Lbl(Event)                { "Type d'�v�nement" "Type of event" }
   set Lbl(By)                   { "Requ�te par" "Request by" }
   set Lbl(Scenario)             { "Sc�nario" "Scenario" }
   set Lbl(NbPer)                { "Nombre de p�riode" "Number of period" }
   set Lbl(Dt)                   { "D�lai entre les p�riodes" "Delay between periods" }
   set Lbl(ISauve)               { "Intervalle de sortie" "Output interval" }
   set Lbl(DTIN)                 { "DTIN" "DTIN" }
   set Lbl(DTIS)                 { "DTIS" "DTIS" }
   set Lbl(FnVert)               { "Distribution verticale" "Vertical distribution" }
   set Lbl(FnTime)               { "Distribution temporelle" "Time distribution" }
   set Lbl(Delai)                { "D�lai d'emission" "Emission delai" }
   set Lbl(IType1)               { "IType1" "IType1" }
   set Lbl(IType2)               { "IType2" "IType2" }

   set Lbl(IsoNb)                { "Nombre d'isotopes" "Number of isotopes" }
   set Lbl(IsoName)              { "Isotope" "Isotope" }
   set Lbl(IsoHalf)              { "P�riode de demi-vie radioactive (s)" "Radioactive half-life period (s)" }
   set Lbl(IsoDry)               { "Taux de lessivage sec (s -1)" "Dry scavenging rate (s -1)" }
   set Lbl(IsoWet)               { "Taux de lessivage humide (s -1)" "Wet scavenging rate (s -1)" }
   set Lbl(IsoRelease)           { "Quantit� totale rel�ch�e" "Total released quantity" }
   set Lbl(IsoChain)             { "Chaine de d�composition" "Decay chain" }

   set Lbl(NbSpecies)            { "Nombre d'esp�ces" "Number of species" }
   set Lbl(Species)              { "Esp�ces" "Species" }
   set Lbl(OutputTimeStepMin)    { "Pas de temps de sortie (min)" "Output time step (min)" }
   set Lbl(ModelTimeStepMin)     { "Pas de temps du mod�le (min)" "Model time step (min)" }
   set Lbl(VerticalLevels)       { "Niveaux verticaux (m)" "Vertical levels (m)" }
   set Lbl(VarMesoscale)         { "Variance des vitesses horiz. des vents (m�/s�)" "Horiz. wind velocity variance (m�/s�)" }
   set Lbl(Timescale)            { "�chelle de temps lagrangienne (s)" "Lagrangian time scale (s)" }
   set Lbl(ReflectionLevel)      { "Niveau inf�rieur de r�flexion (hyb|eta|sig)" "Bottom reflection level (hyb|eta|sig)" }

   set Lbl(EmMass)               { "Masse totale rel�ch�e (unit�)" "Total released mass (unit)" }
   set Lbl(EmIsoName)            { "Nom de l'isotope radioactif" "Name of the radionuclide" }
   set Lbl(EmDepVel)             { "Vitesse de d�p�t (m/s)" "Deposition velocity (m/s)" }
   set Lbl(EmHalfLife)           { "P�riode de demi-vie radioactive (s)" "Radioactive half-life period (s)" }
   set Lbl(EmWetScav)            { "Facteur de lessivage humide" "Wet scavenging coefficient" }
   set Lbl(EmDuration)           { "Dur�e (s)" "Duration (s)" }
   set Lbl(EmDurationMin)        { "Dur�e d'�mission (min)" "Release duration (min)" }
   set Lbl(EmBottom)             { "Bas de la colonne (m)" "Column bottom (m)" }
   set Lbl(EmTop)                { "Haut de la colonne (m)" "Column top (m)" }
   set Lbl(EmRadius)             { "Rayon de la colonne (m)" "Column radius (m)" }

   set Lbl(EmNumberParticles)    { "Nombre de particules" "Number of particles" }
   set Lbl(EmDensity)            { "Densit� d'une particule (�g/m�)" "Density of a particle (�g/m�)" }
   set Lbl(EmHeight)             { "Hauteur maximale du panache (m)" "Maximum plume height (m)" }
   set Lbl(EmSizeDist)           { "Distribution de la taille des particules" "Size distribution of the particles" }
   set Lbl(EmVerticalDist)       { "Distribution verticale du panache" "Vertical plume distribution" }

   set Lbl(Scenario)             { "Sc�nario d'�mission" "Release scenario" }
   set Lbl(EmScenario)           { "Nom du sc�nario d'�mission" "Name of the release scenario" }
   set Lbl(EmNbIntervals)        { "Nombre d'intervalles" "Number of intervals" }
   set Lbl(EmNbIso)              { "Nombre d'isotopes" "Number of isotopes" }
   set Lbl(EmIsoSymbol)          { "Isotopes" "Isotopes" }
   set Lbl(EmIsoQuantity)        { "Quantit� totale rel�ch�e" "Total release quantity" }
   set Lbl(EmTotalDuration)      { "Dur�e totale d'�mission (s)" "Total release duration (s)" }
   set Lbl(EmEffectiveDuration)  { "Dur�e effective d'�mission (s)" "Effective release duration (s)" }

   set Lbl(EmLabel)              { "Type de p�riode" "Type of period" }
   set Lbl(EmReleaseRates)       { "Taux de rel�chement (unit�/h)" "Release rate (unit/h)" }
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

   #----- Force unset on coords for aftercheck (not all modesl have Coords array)
   unset -nocomplain var(Coords)
   unset -nocomplain var(Lat)
   unset -nocomplain var(Lon)

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
   if { ![info exists var(Coords)] } {
      foreach lat $var(Lat) lon $var(Lon) {
         lappend var(Coords) [list $lat $lon]
      }
   }
   if { ![info exists var(Lat)] } {
      foreach coord $var(Coords) {
         lappend var(Lat) [lindex $coord 0]
         lappend var(Lon) [lindex $coord 1]
      }
   }

   if { [info exists var(AccSecs)] } {
      set var(AccYear)  [clock format $var(AccSecs) -format "%Y" -timezone :UTC]
      set var(AccMonth) [clock format $var(AccSecs) -format "%m" -timezone :UTC]
      set var(AccDay)   [clock format $var(AccSecs) -format "%d" -timezone :UTC]
      set var(AccHour)  [clock format $var(AccSecs) -format "%H" -timezone :UTC]
      set var(AccMin)   [clock format $var(AccSecs) -format "%M" -timezone :UTC]
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
         "AccSecs" { set val [clock format $val] }
      }

      if { $tok!="Scenario" } {
         set text "$text[format "%-${l}s" $lbl] = $val\n"
      }
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

   #----- Recuperer le champs OL ou INFO

   if { [catch { fstdfield read INFO $Id -1 "" -1 -1 -1 "" "OL" }] } {
      if { [catch { fstdfield read INFO $Id -1 "" -1 -1 -1 "" "INFO" }] } {
         Dialog::Error . $Msg(Info)
         return ""
      }
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
