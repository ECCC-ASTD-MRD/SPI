#===============================================================================
# Environnement Canada
# Centre Meteorologique Canadian
# 2100 Trans-Canadienne
# Dorval, Quebec
#
# Projet   : Librairie de "Widget" Tk.
# Fichier  : Model-*.*.tcl
# Version  : 3.0
# Creation : Octobre 1999 - J.P.Gauthier - CMC/CMOE
#
# Description:
#    Permet d'afficher une arborescence d'experiences.
#
# Remarques :
#
#===============================================================================

package require Dialog
package require CanvasBubble
package require Calendar
package require Clock
package require MetData
package require Tree
package require IsoBox

#----- If the Global path does not exits, set it to env
if { ![info exists GDefs(Dir)] } {
   set GDefs(Dir) $env(SPI_PATH)
}

namespace eval Model {
   global GDefs
   global env
   variable Lbl
   variable Error
   variable Warning
   variable Bubble
   variable Title
   variable Msg
   variable Data
   variable Param
   variable Resources
   variable Param

   set Param(Dock) True
   set Param(Geom) { 350x630+[winfo rootx .]+[winfo rooty .] }

   set Param(Delay)   60000                            ;#Delai de refresh des experiences (millisecondes)
   set Param(Handle)  ""                               ;#Numero de tache du refresh (Pour la cancellation)
   set Param(Unit)    "DDD.CC"                         ;#Unite des coordonnees de l'experience selectionnee
   set Param(Show)    False
   set Param(Job)     ""                               ;#Current processing

   set Param(Auto)      False                          ;#Running mode
   set Param(Pool)      1                              ;#Manage pool
   set Param(App)       SPI                            ;#Calling application
   set Param(LogLevel)  INFO                           ;#Log level
   set Param(LogTime)   0                              ;#Log Timing
   set Param(Arch)      ""                             ;#Host architecture
   set Param(Host)      ""                             ;#Host
   set Param(Queue)     ""                             ;#Queue
   set Param(EMail)     ""                             ;#Username email address.
   set Param(EMailSet)  "$env(USER)@ec.gc.ca"          ;#Username email address.
   set Param(ListEMail) $Param(EMailSet)               ;#List of email addresses.
   set Param(IsEMail)   0                              ;#Flag indicating if sending email to user for monitoring entire job (1) or not (0).
   set Param(Listings) $env(HOME)/listings/eer_Experiment
   set Param(DBaseType) "eta"                          ;#Type od metdata
   set Param(DBaseLocal) False                         ;#Is the metdata local
   set Param(DBaseDiag) ""                             ;#Path for diag metdata
   set Param(DBaseProg) ""                             ;#Path for prog metdata
   set Param(NbCPUsMeteo)       1
   set Param(ListNbCPUsMeteo)   { 1 }
   set Param(NbMPItasks)        1
   set Param(ListNbMPItasks)    { 1 }
   set Param(NbOMPthreads)      1
   set Param(ListNbOMPthreads)  { 1 }
   set Param(OMPthreadFact)     1                     ;#Integer multiplicative factor to apply to number of OpenMP threads [1|2].
   set Param(ListOMPthreadFact) { 1 }
   set Param(Submit)            ord_soumet            ;#Queue launcher

   set Data(Job)    ""                    ;#Texte de processus
   set Data(Active) 0                     ;#Flag d'activation de l'outils
   set Data(Canvas) ""                    ;#Canvas Actif
   set Data(Frame)  ""                    ;#Frame actif
   set Data(VP)     ""                    ;#Viewport actif

   #----- Labels
   set Title           { "Modélisation" "Modeling" }

   set Lbl(Launch)              { "Lancement" "Launch" }
   set Lbl(LaunchModel)         { "Lancer" "Launch" }
   set Lbl(Params)              { "Paramètres" "Parameters" }
   set Lbl(Host)                { "Nom de l'hôte  " "Host name      " }
   set Lbl(Queue)               { "Type de queue  " "Queue type     " }
   set Lbl(MetCPU)              { "Nb CPUs météo  " "Nb CPUs meteo  " }
   set Lbl(NbMPItasks)          { "Nb tâches MPI  " "Nb MPI tasks   " }
   set Lbl(NbOMPthreads)        { "Nb threads OMP " "Nb OMP threads " }
   set Lbl(OMPthreadFact)       { "Facteur OMP    " "OMP Factor     " }
   set Lbl(IsEMail)             { "Surveillance par courriel" "E-mail monitoring" }
   set Lbl(EMail)               { "   Adresse     " "   Address     " }
   set Lbl(Close)               { "Fermer" "Close" }
   set Lbl(Yes)                 { "Oui" "Yes" }
   set Lbl(No)                  { "Non" "No " }

   set Lbl(Name)       { "Nom" "Name" }
   set Lbl(Diag)       { "Diagnostiques" "Diagnostics" }
   set Lbl(Prog)       { "Prognostiques" "Prognostics" }
   set Lbl(Emerg)      { "Urgences" "Emergencies" }
   set Lbl(Source)     { "Sélecteur de source" "Source selector" }
   set Lbl(Lat)        { "Latitude  (Nord+ Sud-) :" "Latitude  (North+ South-):" }
   set Lbl(Lon)        { "Longitude (Est+ Ouest-):" "Longitude (East+ West-)  :" }
   set Lbl(Type)       { "Type d'expérience      :" "Experiment type          :" }
   set Lbl(Create)     { "Créer" "Create" }
   set Lbl(Cancel)     { "Annuler" "Cancel" }
   set Lbl(Checked)    { "Rafraîchissement effectué à" "Refresh done at" }
   set Lbl(Checking)   { "Rafraîchissement en cours..." "Refreshing simulations..." }
   set Lbl(Select)     { "Sélectionner" "Select" }
   set Lbl(Watch)      { "Veilles" "Watch" }
   set Lbl(MetPath)    { "Répertoire des données météorologiques" "Meteorological data path" }
   set Lbl(EmHeight)   { "Hauteur explosive maximale" "Maximum explosive height" }

   #----- Bulles d'aide

   set Bubble(Host)          { "Hôte où le prétraitement météorologique et le modèle seront exécutés." \
                               "Host where meteorological preprocessing and model will be executed." }
   set Bubble(Queue)         { "Type de queue de lancement." "Type of launching queue." }
   set Bubble(MetCPU)        { "Nombre de processus servant à l'exécution du\nprétraitement météorologique sur l'hôte sélectionné." "Number of processes to use for running\nmeteorological preprocessing on selected host." }
   set Bubble(NbMPItasks)    { "Nombre de tâches MPI définissant la configuration du nombre de\nCPUs (MPIxOMP) pour l'exécution du modèle sur l'hôte sélectionné." \
                              "Number of MPI tasks which defines the CPU configuration (MPIxOMP)\nfor running the model on selected host." }
   set Bubble(NbOMPthreads)  { "Nombre de threads OMP par tâche MPI définissant la configuration du nombre de\nCPUs (MPIxOMP) pour l'exécution du modèle sur l'hôte sélectionné." \
                               "Number of OMP threads per MPI task which defines the CPU configuration (MPIxOMP)\nfor running the model on selected host." }
   set Bubble(OMPthreadFact) { "Facteur multiplicatif entier à appliquer\nau nombre de threads sélectionnés." "Integer multiplicative factor to apply\nto number of selected OMP threads." }
   set Bubble(IsEMail)       { "Option permettant d'activer ou de désactiver la surveillance (le monitoring)\nde la simulation par courrier électronique." "Option to enable or disable the e-mail monitoring of simulation." }
   set Bubble(EMail)         { "Adresse de courrier électronique." "E-mail address." }
   set Bubble(LaunchModel)   { "Lancer le modèle." "Launch model." }

   set Bubble(PathSel) { "Liste des dépôts d'expériences disponibles" "List of experiments available" }
   set Bubble(PathAdd) { "Ajouter un répertoire à la liste\ndes dépots d'expériences disponibles" "Add a path to the list of experiments available" }
   set Bubble(Dock)    { "Attacher/Détacher l'interface de la fenêtre principale" "Dock/Undock window from main window" }
   set Bubble(Close)   { "Fermer l'outil" "Close the tool" }
   set Bubble(Bubble)  { "Activer les bulles d'informations" "Activate information bubbles" }
   set Bubble(Plus)    { "Ouvrir toutes les branches" "Open up all branches" }
   set Bubble(Minus)   { "Fermer toutes les branches" "Close all branches" }
   set Bubble(SRC)     { "Ouvrir le sélecteur de sources" "Open the source selector" }
   set Bubble(Name)    { "Nom de l'expérience\nCe nom sera utilisé sur tous les produits"
                         "Experiment name\nThis name will be used on every product" }
   set Bubble(Info)    { "Informations de positionnement (id,lat,lon)" "Position information (id,lat,lon)" }
   set Bubble(Coord)   { "Permet de changer ou de convertir\nle type de coordonnées en degrées-minutes\nou degrées centième"
                         "Use to change or convert the coordinate format in degree-minute or degree-hundreth" }
   set Bubble(Create)  { "Créer l'expérience" "Create the experiment" }
   set Bubble(Cancel)  { "Annuler, retour à la liste des expériences" "Cancel, return to the experiment list" }
   set Bubble(Type0)   { "Type de source volcanique" "Volcanic source type" }
   set Bubble(Type1)   { "Type de source nucléaire"  "Nuclear source type" }
   set Bubble(Type2)   { "Type de source CTBT" "CTBT source type" }
   set Bubble(Type3)   { "Type de source feu" "Fire source type" }
   set Bubble(Type4)   { "Type de source épidemie" "Epidemic source type" }
   set Bubble(Type5)   { "Type de source déversement" "Spill source type" }
   set Bubble(Type6)   { "Autres Types de sources" "Other source type" }

   set Error(Host)                { "L'hôte sélectionné ne peut être rejoint. Par conséquent, l'hôte local sera utilisé." "Selected host cannot be reached, using local host." }
   set Error(EMail)               { "L'adresse électronique est invalide. Veuillez corriger l'adresse spécifiée.\n\n\tCourriel :" "The electronic mail address is invalid. Please correct this email address.\n\n\tE-mail :" }
   set Error(MetDBase)            { "Erreur! Les répertoires des bases de données météorologiques diagnostiques et/ou pronostiques ne sont pas définis. Veuillez spécifier les répertoires de ces bases de données." \
                                    "Error! The directories for diagnostic and/or prognostic meteorological databases are undefined. Please specify the path for these databases." }
   set Error(MetFiles)            { "Le nombre de fichiers disponibles dans la base de données météorologique localisée sur l'hôte sélectionné est insuffisant pour exécuter le modèle à partir de la date et du temps d'émission de l'accident. Veuillez modifier la date et/ou le temps d'émission de l'accident ou l'hôte."
                                    "The number of available files in the meteorological database located on the selected host is not enough to run the model according to accident release date-time. Please modify the accident release date-time or the host." }
   set Error(DateTimeEmission)    { "\tDate/Temps de l'émission :" "\tRelease date-time         :" }
   set Error(FirstMetDateTime)    { "\tPremier temps disponible :" "\tFirst available date-time :" }
   set Error(LastMetDateTime)     { "\tDernier temps disponible :" "\tLast available date-time  :" }
   set Error(DateTimeMetFiles)    { "La date et le temps d'émission de l'accident ne sont pas cohérents avec les données météorologiques disponibles. Veuillez modifier la date et/ou le temps d'émission de l'accident." \
                                    "The release accident date-time is not consistent according to avaible meteorological data. Please modify the accident release date-time." }
   set Error(Path)                { "Le répertoire de simulation n'est pas accessible sur l'hôte d'exécution." "Simulation path is not accessible on remote host." }
   set Error(EmHeight)            { "La masse doit être positive." "Mass must be positive." }

   set Warning(SimDuration1)      { "La durée de simulation sera réinitialisée en fonction des données météorologiques disponibles dans la base de données." \
                                    "The simulation duration will be re-initialized according to available meteorological data in database." }
   set Warning(SimDuration2)      { "\tAncienne durée de simulation :" "\tOld simulation duration :" }
   set Warning(SimDuration3)      { "\tNouvelle durée de simulation :" "\tNew simulation duration :" }

   set Warning(Queue)             { "Vous êtes sur le point de lancer le modèle en classe haute priorité sur le superordinateur du CMC. Ceci peut occasionner des répercussions importantes sur les passes opérationnelles et parallèles en cours.\n\nVeuillez consulter le superviseur de quart 24/7 à la section des Analyses et Pronostics (A&P) de la direction des opérations du CMC en personne ou par téléphone au 514-421-4635.\n\nVoulez-vous tout de même lancer le modèle via la queue opérationnelle de production?" \
                                    "You are about to launch model with highest priority on CMC's supercomputer. This may produce some important impacts on the current operational and parallel runs.\n\nPlease consult the 24/7 shift supervisor at Analysis and Prognosis (A&P) Section from CMC's Operations Branch in person or by phone at 514-421-4635.\n\nDo you still wish to launch the model through the operational production queue?" }
   set Warning(EMail)             { "L'adresse électronique est différente de celle par défaut. Voulez-vous surveiller la progression de la simulation par courriel avec cette nouvelle adresse?" \
                                    "The electronic email address is different than the default one. Do you wish to monitor the progress of the simulation by email with this new address?" }
   set Warning(EMail2)            { "\tNouveau courriel    :" "\tNew email     :" }
   set Warning(EMail3)            { "\tCourriel par défaut :" "\tDefault email :" }

   set Warning(DiskSpace)         { "L'espace disque disponible associé au répertoire principal de l'expérience est inférieur à la valeur critique d'espace disque.\n\nVoulez-vous tout de même exécuter le modèle?" \
                                    "The available disk space associated to the main experiment diretory is less than the critical disk space value.\n\nDo you still wish to execute the model?" }
   set Warning(DiskPath)          { "\tRépertoire principal    " "\tMain directory      " }
   set Warning(DiskCritical)      { "\tEspace disque critique  " "\tCritical disk space " }
   set Warning(DiskAvailable)     { "\tEspace disque disponible" "\tAvailable disk space" }
   set Warning(DiskUsed)          { "\tEspace disque utilisé   " "\tUsed disk space     " }

   set Msg(EmHeight)     { "Veuillez spécifier la masse d'explosif en kilogrammes." "Please enter explosive mass in kilograms." }
   set Msg(Exist)        { "Veuillez compléter le lancement de modèle en cours avant de procéder à un autre." "Please complete the current model launch before proceeding with another one." }
   set Msg(Delete)       { "Voulez-vous vraiment supprimer cette simulation ?" "Do you really want to delete this simulation ?" }

   catch {
      set Resources(Icos)  "ICO_VOLC ICO_NUCL ICO_CTBT ICO_FIRE ICO_BIO ICO_SPILL ICO_OTHE"
      set Resources(Acts)  "ACT_VOLC ACT_NUCL ACT_CTBT ACT_FIRE ACT_BIO ACT_SPILL ACT_OTHE"
      set Resources(Bads)  "BAD_VOLC BAD_NUCL BAD_CTBT BAD_FIRE BAD_BIO BAD_SPILL BAD_OTHE"

      set Resources(Plus)  "@$GDefs(Dir)/Resources/Bitmap/plus.ico"
      set Resources(Minus) "@$GDefs(Dir)/Resources/Bitmap/minus.ico"
   }
}

#----- Inclure les type d'experiences
source $GDefs(Dir)/Apps/Models/Meteo.tcl
source $GDefs(Dir)/Apps/Models/Exp.tcl
source $GDefs(Dir)/Apps/Models/Watch.tcl

#----- Inclure les types de modeles
source $GDefs(Dir)/Apps/Models/Types/TRAJECT.tcl
source $GDefs(Dir)/Apps/Models/Types/SATDATA.tcl
source $GDefs(Dir)/Apps/Models/Types/MLCD.tcl
source $GDefs(Dir)/Apps/Models/Types/MLDP.tcl
source $GDefs(Dir)/Apps/Models/Types/MLDPn.tcl

#----------------------------------------------------------------------------
# Nom        : <Model::ComputeKaboomHeight>
# Creation   : Octobre 2009 - J.P. gauthier - CMC/CMOE
#
# But        : Calculate source term height from explosive mass
#              according to Real's formula.
#
# Parametres :
#
# Retour     :
#
# Remarques  :
#
#----------------------------------------------------------------------------

proc Model::ComputeKaboomHeight { Model } {
   variable Lbl
   variable Msg
   variable Error

   upvar ${Model}::Sim sim

   if { $Model == "MLDP" } {
      set height $sim(EmHeightOld)
   } elseif { $Model == "MLCD" } {
      set height $sim(EmTopOld)
   }

   if { [set boom [Dialog::Get . $Lbl(EmHeight) $Msg(EmHeight)]]!="" } {
      if { [string is double -strict $boom] && $boom>0 } {
         return [expr int(3160.0*(pow($boom/1000000.0,0.268)))]
      } else {
         Dialog::Error .modelnew $Error(EmHeight)
      }
   }
   return $height
}

proc Model::GetMetData { Model } {
   Model::ParamsMetDataDir ${Model}
   if { ![${Model}::GetMetData] } {
      return False
   }
   Model::ParamsMeteoInput ${Model}
   return True
}

proc Model::CreateInput { Model } {
   catch { ${Model}::EmissionRead }
   ${Model}::CreateModelInput
   ${Model}::CreateScriptInput

   #----- Copy files to run host if needed
   Model::ParamsCopy ${Model}
}

#-------------------------------------------------------------------------------
# Nom      : <Model::Delete>
# Creation : Octobre 1999 - J.P. Gauthier - CMC/CMOE
#
# But      : Supprimer une simulation.
#
# Parametres  :
#   <Model>   : Model
#   <Info>    : Identificateur de la simulation
#
# Retour :
#
# Remarques :
#
#-------------------------------------------------------------------------------

proc Model::Delete { Info } {
   global GDefs
   variable Msg
   variable Lbl
   variable Sim

   . config -cursor watch
   update idletasks

   #----- Verifier la validitee des parametres
   if { [Dialog::Default . 400 WARNING $Msg(Delete) "" 0 $Lbl(Yes) $Lbl(No)] } {
      return
   }

   #----- Arreter l'execution en cours  si ily a lieu
   Exp::ThreadKill [Exp::Id $Info]

   set path      [Exp::Path]
   set model     [Info::Strip $Info Model]
   set modelbase [string trimright [string trimright $model 0] 1]

   #----- Delete simulation (continued included)
   while { $Info!="" } {

      #----- Extraire les informations sur l'experience.
      Info::Decode ::Model::Sim $Info

      #----- Determiner la localisation du fichier
      set simpath [Info::Path $Info]

      #----- Supprimer les donnees sur le serveur.
      file delete -force $path/$simpath

      Info::Delete $path/$model.pool $Info

      #----- Recherche de simulation precedente
      set Info [lindex [Info::Find $path/$model.pool $modelbase NoPrev $Sim(NoSim)] 0]
   }

   #----- Relire les experiences
   . config -cursor left_ptr
   Model::Check 0
}

#----------------------------------------------------------------------------
# Nom      : <Model::ParamsMetPath>
# Creation : Juin 2001 - J.P. Gauthier - CMC/CMOE
#
# But      : Permet de selectionner les path des fichiers de donnees meteo
#            d'analyse et de prognostique.
#
# Parametres :
#
# Retour     :
#   <Valid>  : True ou False.
#
# Remarques :
#
#----------------------------------------------------------------------------

proc Model::ParamsMetPath { } {
   global GDefs
   variable Param
   variable Lbl

   set Data(ShowPath) 0
   set diag $Model::Param(DBaseDiag)
   set prog $Model::Param(DBaseProg)

   toplevel     .metpath

   wm title     .metpath [lindex $Lbl(MetPath) $GDefs(Lang)]
   wm resizable .metpath 0 0
   wm transient .metpath .modelnew
   wm geom      .metpath +[expr [winfo rootx .modelnew]+50]+[expr [winfo rooty .modelnew]+50]

   frame .metpath.diag -relief raised -bd 1
     button .metpath.diag.select -text [lindex $Lbl(Diag) $GDefs(Lang)] -relief groove -bd 2 \
        -command { if { [set path [FileBox::Create . $Model::Param(DBaseDiag) Path "" ]]!="" } { set Model::Param(DBaseType) user; set Model::Param(DBaseDiag) $path } }
     entry .metpath.diag.path -bg $GDefs(ColorLight) -textvariable Model::Param(DBaseDiag) -relief sunken -bd 1 -width 40
     pack .metpath.diag.select .metpath.diag.path -side left -fill y

   frame .metpath.prog -relief raised -bd 1
     button .metpath.prog.select -text [lindex $Lbl(Prog) $GDefs(Lang)] -relief groove -bd 2 \
        -command { if { [set path [FileBox::Create . $Model::Param(DBaseProg) Path "" ]]!="" } { set Model::Param(DBaseType) user; set Model::Param(DBaseProg) $path } }
     entry .metpath.prog.path -bg $GDefs(ColorLight) -textvariable Model::Param(DBaseProg) -relief sunken -bd 1 -width 40
     pack .metpath.prog.select .metpath.prog.path -side left -fill y

   frame .metpath.command
     button .metpath.command.ok -text "Ok" -relief raised -bd 1 -command "set Model::Data(ShowPath) 1"
     pack .metpath.command.ok -side top -fill both

   pack .metpath.diag .metpath.prog .metpath.command -side top -fill x

   #----- Attendre la selection
   grab .metpath
   tkwait variable Model::Data(ShowPath)

   if { $diag!=$Model::Param(DBaseDiag) || $prog!=$Model::Param(DBaseProg) } {
      set  Model::Param(DBaseType) user
   }
   destroy .metpath
}

#----------------------------------------------------------------------------
# Nom        : <Model::ParamsGridDefine>
# Creation   : Mai 2000 - J.P. Gauthier - CMC/CMOE
#
# But        : Creer une grille pour l'execution du modele
#
# Parametres :
#
# Retour     :
#
# Remarques  :
#
#----------------------------------------------------------------------------

proc Model::ParamsGridDefine { Model { Mode NEW } } {
   variable Data

   if { $Model=="TRAJECT" || $Model=="MLCD" } {
      return
   }

   upvar ${Model}::Sim sim

   set Data(Frame) $Page::Data(Frame)
   set Data(VP)    $Viewport::Data(VP)

   if { $Mode!="NEW" } {
      set sim(NI)     [lindex $sim(Grid) 1]
      set sim(NJ)     [lindex $sim(Grid) 2]

      fstdfield free MODELGRID
      fstdfield create MODELGRID $sim(NI) $sim(NJ) 1
      fstdfield define MODELGRID -GRTYP [lindex $sim(Grid) 0] [lindex $sim(Grid) 3] [lindex $sim(Grid) 4] [lindex $sim(Grid) 5] [lindex $sim(Grid) 6]

      set grid [fstdfield stats MODELGRID -gridpoint [expr $sim(NI)/2+1] [expr $sim(NJ)/2+1]]
      set sim(GridLat) [lindex $grid 0]
      set sim(GridLon) [lindex $grid 1]
   } else {
      if { [llength $sim(Scale)] > 1 } {
         set sim(GridRes)  [string trimleft  [lindex $sim(Scale) 1] "("] ; #----- Grid scale resolution [km].
         set sim(GridSize) [string trimright [lindex $sim(Scale) 3] ")"] ; #----- Grid size NIxNJ.
         set sim(Scale)    [lindex $sim(Scale) 0]                        ; #----- Grid scale name.
      } else {
         set idx [lsearch -regexp $sim(ListScale) "^$sim(Scale) *"]
         if { $idx != -1 } {
            set string [lindex $sim(ListScale) $idx]
            set sim(GridRes)  [string trimleft  [lindex $string 1] "("] ; #----- Grid scale resolution [km].
            set sim(GridSize) [string trimright [lindex $string 3] ")"] ; #----- Grid size NIxNJ.
         }
      }

      set idx [string first "x" $sim(GridSize)]
      if { $idx != -1 } {
         set sim(NI) [string range $sim(GridSize) 0 [expr $idx - 1]]
         set sim(NJ) [string range $sim(GridSize) [expr $idx + 1] end]
      }

      set sim(GridRes) [expr $sim(GridRes)*1000]; #----- Convert grid resolution from [km] to [m].
      set sim(Grid) [MetData::GridDefinePS [list $sim(Scale) $sim(GridRes)] $sim(NI) $sim(NJ) $sim(GridLat) $sim(GridLon) MODELGRID]
   }
   set sim(NK) 25 ;#----- Number of vertical levels in the model (MLDP).

   fstdfield define MODELGRID -NOMVAR GRID
   fstdfield configure MODELGRID -rendergrid 1 -colormap FLDMAPDefault -color black -font XFont10

   Viewport::Assign $Data(Frame) $Data(VP) MODELGRID
  Viewport::UpdateData $Data(Frame)
}

#----------------------------------------------------------------------------
# Nom        : <Model::ParamsCopy>
# Creation   : Juillet 2009 - J.P. Gauthier - CMC/CMOE
#
# But        : Creer l'arborescence d'execution et les fichiers de parametres
#              de la simulation sur l'hote d'execution
#
# Parametres :
#  <Model>   : Model
#
# Retour     :
#  <Bool>    : True ou False.
#
# Remarques  :
#
#----------------------------------------------------------------------------

proc Model::ParamsCopy { Model  } {
   global GDefs
   variable Param

   upvar ${Model}::Sim sim

   #----- Run will be remote, setup what's needed on remote host.
   if { $Param(Remote) } {

      #----- Create simulation directories .
      set ErrorCode [catch { exec ssh -l $GDefs(FrontEndUser) -n -x $Param(Host) mkdir -p $sim(PathRun) $sim(PathRun)/meteo $sim(PathRun)/results $sim(PathRun)/tmp 2>@1 } msg]
      if { $ErrorCode != 0 } {
         Log::Print ERROR "Unable to create simulation directories on $Param(Host).\n\n$msg"
         return False
      }

      #----- Copy needed files.
      set ErrorCode [catch { eval exec scp -p $sim(Path)/tmp/sim.pool [glob $sim(Path)/tmp/*.in] $GDefs(FrontEndUser)@$Param(Host):$sim(PathRun)/tmp } Message]
      if { $ErrorCode != 0 } {
         Log::Print ERROR "Copying meteorological preprocessing input file and script on ($Param(Host)) has failed.\n\n$Message"
         return False
      }
      Log::Print INFO "Meteorological preprocessing input files and script have been copied on ($Param(Host)) successfully."
   }
}

#----------------------------------------------------------------------------
# Nom        : <Model::ParamsCheck>
# Creation   : 27 August 2007 - A. Malo - CMC/CMOE
#
# But        : Check launch parameters.
#
# Parametres :
#  <Model>   : Model
#  <Get>     : Extraire les donnees meteo
#
# Retour     :
#  <Bool>    : True ou False.
#
# Remarques  :
#
#----------------------------------------------------------------------------

proc Model::ParamsCheck { Model { Get True } } {
   global   GDefs
   global   env
   variable Param
   variable Error

   #----- Set host architecture.
   if { [catch { set Param(Arch) [exec ssh -l $GDefs(FrontEndUser) -n -x $Param(Host) uname -s] } ] } {
      Dialog::Error . $Error(Host)
      set Param(Host) $GDefs(Host)
      set Param(Arch) [exec  uname -s]
   }

   #----- Set flag indicating if using 'soumet' command or not.
   if { ![info exists GDefs(Host_$Param(Host))] || $Param(Host)==$GDefs(Host) } {
      set Param(IsUsingSoumet) 0
   } else {
      set Param(IsUsingSoumet) 1

      #----- Create listing directory.
      if { ![file isdirectory $env(HOME)/listings/eer_Experiment] } {
         file mkdir $env(HOME)/listings/eer_Experiment
      }
   }

   Model::ParamsQueues
   Model::ParamsCPUModel
   Model::ParamsMetDataDir $Model

   #----- Remember selected host for this model
   set Param(Host$Model) $Param(Host)

   if { $Get && $Model!="MLCD" && [info proc ::${Model}::GetMetData]!="" } {
      if { ![${Model}::GetMetData] } {
         return False
      }
   }
   return True
}

#----------------------------------------------------------------------------
# Nom        : <Model::ParamsMetData>
# Creation   : 27 August 2007 - A. Malo - CMC/CMOE
#
# But        : Extract relevant met files according to available
#              meteorological data files and simulation duration.
#
# Parametres :
#  <Model>   : Model
#
# Retour     :
#  <Bool>    : True ou False.
#
# Remarques  :
#
#----------------------------------------------------------------------------

proc Model::ParamsMetData { Model } {
   global GDefs
   variable Error
   variable Warning
   variable Lbl
   variable Param

   upvar ${Model}::Sim sim

   #----- Verify if diagnostic and prognostic databases are defined.
   if { $Param(DBaseDiag) == "" || $Param(DBaseProg) == "" } {
      Dialog::Error . $Error(MetDBase)
      return False
   }

   if { ![llength $sim(Data)] } {
      Dialog::Error . $Error(MetFiles)
      return False
   }

   #----- Set starting simulation date-time using first meteorological file available in the database.
   set firststamp [lindex [lindex $sim(Data) 0] 0]
   set firstdate  [fstdstamp toseconds $firststamp]

   #----- Get last meteorological file
   set laststamp [lindex [lindex $sim(Data) end] 0]
   set lastdate  [fstdstamp toseconds $laststamp]

   set rundate   [fstdstamp toseconds $sim(RunStamp)]
   set rundate   [clock format $rundate -format "%Y-%m-%d %T UTC" -gmt True]

   #----- Compute simulation duration [hr].
   set simdur [expr int([fstdstamp diff $laststamp $firststamp] + 0.5)]

   if { $simdur<=0 } {
      set first [clock format $firstdate -format "%Y-%m-%d %T UTC" -gmt True]
      set last  [clock format $lastdate -format "%Y-%m-%d %T UTC" -gmt True]
      Dialog::Error . $Error(MetFiles) "\n\n[lindex $Error(DateTimeEmission) $GDefs(Lang)] $rundate.\n[lindex $Error(FirstMetDateTime) $GDefs(Lang)] $first.\n[lindex $Error(LastMetDateTime) $GDefs(Lang)] $last."
      return False
   }

   if { $sim(Duration) == 0 } {

      #----- Define simulation duration [hr] according to available met files.
      set sim(Duration) $simdur

   } else {

      if { $sim(Duration) > $simdur } {

         #----- Here, simulation duration set as input parameter is greater than (or equal to) simulation duration
         #----- computed according to available met files. Thus, simulation duration will be re-initialized.
         set oldsimdur     $sim(Duration)
         set sim(Duration) $simdur
         Dialog::Info . $Warning(SimDuration1) "\n\n[lindex $Warning(SimDuration2) $GDefs(Lang)] $oldsimdur h.\n[lindex $Warning(SimDuration3) $GDefs(Lang)] $sim(Duration) h."

      } else {

         #----- Here, simulation duration set as input parameter is less than simulation duration
         #----- computed according to available met files.

         #----- Compute new ending simulation date-time [s] according to starting simulation date-time and simulation duration.
         #----- Redefine list of available meteorological data files according to simulation duration set as input parameter.
         if { $sim(Backward) } {
            set stamp [fstdstamp fromseconds [expr $lastdate-$sim(Duration)*3600]]
         } else {
            set stamp [fstdstamp fromseconds [expr $firstdate+$sim(Duration)*3600]]
         }

         set idx 0
         foreach data $sim(Data) {
            if { $stamp<=[lindex $data 0] } {
               break
            }
            incr idx
         }
         if { $sim(Backward) } {
            set sim(Data) [lrange $sim(Data) $idx end]
         } else {
            set sim(Data) [lrange $sim(Data) 0 $idx]
         }
      }
   }

   #----- checki if we have enough data
   if { [llength $sim(Data)]<2 } {
      Dialog::Error . $Error(MetFiles)
      return False
   }

   #----- List of met data files.
   set sim(MeteoDataFiles) {}
   foreach data $sim(Data) {
      lappend sim(MeteoDataFiles) [lindex $data 2]
   }

   #----- Set simulation date-time.
   if { $sim(Backward) } {
      set simdate   $lastdate
   } else {
      set simdate   $firstdate
   }
   set sim(SimYear)     [clock format $simdate -format "%Y" -gmt True]
   set sim(SimMonth)    [clock format $simdate -format "%m" -gmt True]
   set sim(SimDay)      [clock format $simdate -format "%d" -gmt True]
   set sim(SimHour)     [clock format $simdate -format "%H" -gmt True]

   #----- Validate emission time according to available meteorological data files.
   set first [lindex [lindex $sim(Data) 0] 0]
   set last  [lindex [lindex $sim(Data) end] 0]

   if { $sim(RunStamp)<$first || $sim(RunStamp)>$last } {
      Dialog::Error . $Error(DateTimeMetFiles)
      return False
   }

   set sim(Mode) [MetData::GetMode $sim(Data)]

   return True
}

#----------------------------------------------------------------------------
# Nom        : <Model::ParamsMetDataDir>
# Creation   : 28 August 2007 - A. Malo - CMC/CMOE
#
# But        : Set (diagnostics and prognostics) meteorological data
#              directories.
#
# Parametres :
#  <Model>   : Model
#
# Retour     :
#
# Remarques  :
#
#----------------------------------------------------------------------------

proc Model::ParamsMetDataDir { Model } {
   global GDefs
   global env
   variable Param

   upvar ${Model}::Sim sim

   if { $Param(DBaseType)!="user" } {

      #----- Define meto path
      set dbops $env(CMCGRIDF)
      set dbeer $env(CMCGRIDF)

      if { [info exists GDefs(Host_$Param(Host))] } {
         set dbops $Param(Host):[lindex $GDefs(Host_$Param(Host)) 1]
         set dbeer $Param(Host):[lindex $GDefs(Host_$Param(Host)) 2]
      }
      if { $sim(Model)=="MLDP1" && $sim(Meteo)=="reg"} {
         set MetData::Param(Path) $dbeer
      } else {
         set MetData::Param(Path) $dbops
      }

      #----- Set met database by default.
      MetData::Path $Param(DBaseType) $sim(Meteo) Model::Param(DBaseDiag) Model::Param(DBaseProg)

      #----- Set met database by default.
      if { $sim(Model)=="MLDP1" } {
         set Param(DBaseDiag) $Param(DBaseProg)
      }
   }

   #----- Check for remote path
   set Param(DBaseLocal) [catch { exec ssh -l $GDefs(FrontEndUser) -n -x $Param(Host) ls [lindex [split $Param(DBaseDiag) :] end] }]
}

#----------------------------------------------------------------------------
# Nom        : <Model::ParamsCPUModel>
# Creation   : 22 October 2007 - A. Malo - CMC/CMOE
#
# But        : Set default CPU configuration (number of MPI tasks and number
#              of OMP threads per MPI task) for model and list of available
#              CPU configurations.
#
# Parametres :
#
# Retour     :
#
# Remarques  :
#
#----------------------------------------------------------------------------

proc Model::ParamsCPUModel { } {
   variable Param

   #----- Set CPU configuration for model according to architecture.
   switch $Param(Arch) {
      "Linux"  {
         set Param(NbMPItasks)        1
         set Param(ListNbMPItasks)    { 1 }
         set Param(NbOMPthreads)      2
         set Param(ListNbOMPthreads)  { 1 2 4 8 }
         set Param(OMPthreadFact)     1 ; #----- Integer multiplicative factor to apply to number of OpenMP threads [1|2].
         set Param(ListOMPthreadFact) { 1 }
         set Param(NbCPUsMeteo)       1
         set Param(ListNbCPUsMeteo)   { 1 2 4 }
      }
      "AIX"    {
         set Param(NbMPItasks)        2
         set Param(ListNbMPItasks)    { 1 2 3 4 5 8 10 16 }
         set Param(NbOMPthreads)      16
         set Param(ListNbOMPthreads)  { 16 }
         set Param(OMPthreadFact)     2 ; #----- Integer multiplicative factor to apply to number of OpenMP threads [1|2].
         set Param(ListOMPthreadFact) { 1 2 }
         set Param(NbCPUsMeteo)       16
         set Param(ListNbCPUsMeteo)   { 1 2 4 8 16 }
      }
   }

   if { $Param(IsUsingSoumet) } {

      if { $Param(NbCPUsMeteo) > $Param(NbOMPthreads) } {

         #----- Update number of OMP threads for model.
         set Param(NbOMPthreads) $Param(NbCPUsMeteo)
      }

      #----- Update list of available number of OMP threads.
      set idx [lsearch -exact $Param(ListNbCPUsMeteo) $Param(NbCPUsMeteo)]
      set Param(ListNbOMPthreads) [lrange $Param(ListNbCPUsMeteo) $idx end]
   }

   #----- Update list of available MPI tasks, OMP threads and OMP factor for model.
   catch {
      Option::Set $Param(Frame).params.mpi    $Param(ListNbMPItasks)
      Option::Set $Param(Frame).params.omp    $Param(ListNbOMPthreads)
      Option::Set $Param(Frame).params.smt    $Param(ListOMPthreadFact)
      Option::Set $Param(Frame).params.metcpu $Param(ListNbCPUsMeteo)
   }
}

#----------------------------------------------------------------------------
# Nom        : <Model::ParamsQueues>
# Creation   : 2 October 2007 - A. Malo - CMC/CMOE
#
# But        : Set default queue and list of available queues.
#
# Parametres :
#
# Retour     :
#
# Remarques  :
#
#----------------------------------------------------------------------------

proc Model::ParamsQueues { } {
   global GDefs
   variable Param

   if { [info exists GDefs(Host_$Param(Host))] } {
      set Param(Queues) [lindex $GDefs(Host_$Param(Host)) 0]
   } else {
      set Param(Queues)  ""
   }
   set Param(Queue) [lindex $Param(Queues) 0]

   catch {
      Option::Set $Param(Frame).params.queue $Param(Queues)

      if { [llength $Param(Queues)] } {
         Option::Enable $Param(Frame).params.queue
      } else {
         Option::Disable $Param(Frame).params.queue
      }
   }
}

#----------------------------------------------------------------------------
# Nom        : <Model::ParamsClose>
# Creation   : Juillet 2009 - J.P. Gauthier - CMC/CMOE
#
# But        : Finaliser la fermeture de la fenetre des parametres des modeles.
#
# Parametres :
#  <Model>   : Model
#  <Frame>   : Fenetre parent
#
# Retour     :
#
# Remarques  :
#
#----------------------------------------------------------------------------

proc Model::ParamsClose { Model } {
   variable Data

   if { $Page::Data(ToolMode) == "$Model" } {
      SPI::ToolMode SPI Zoom
   }
   Viewport::UnAssign $Data(Frame) $Data(VP) MODELGRID

   destroy .modelnew
}

#----------------------------------------------------------------------------
# Nom        : <Model::InitNew>
# Creation   : Juillet 2009 - J.P. Gauthier - CMC/CMOE
#
# But        : Initialiser les variables communes des modeles.
#
# Parametres :
#   <Model>  : Model
#   <No>     : Numero de simulation
#   <Name>   : Nom de l'experience
#   <Pos>    : Liste des localisations
#
# Retour     :
#   <Base>   : Namespace de base du model
#
# Remarques  :
#
#----------------------------------------------------------------------------

proc Model::InitNew { Model { No -1 } { Name "" } { Pos {} } } {
   variable Data

   set modelbase [string trimright [string trimright $Model 0] 1]
   upvar ${modelbase}::Sim sim

   #----- Initialize release date-time.
   set sim(AccSeconds) [clock scan [clock format [clock seconds] -format "%Y%m%d %H:%M" -gmt True]]
   set sim(AccDate)    [clock scan [clock format $sim(AccSeconds) -format "%Y%m%d" -gmt True]]
   set sim(AccYear)    [set sim(SimYear)  [clock format $sim(AccSeconds) -format "%Y" -gmt True]]
   set sim(AccMonth)   [set sim(SimMonth) [clock format $sim(AccSeconds) -format "%m" -gmt True]]
   set sim(AccDay)     [set sim(SimDay)   [clock format $sim(AccSeconds) -format "%d" -gmt True]]
   set sim(AccHour)    [set sim(SimHour)  [clock format $sim(AccSeconds) -format "%H" -gmt True]]
   set sim(AccMin)     [set sim(SimMin)   [clock format $sim(AccSeconds) -format "%M" -gmt True]]

   set sim(Model)     $Model
   set sim(State)     1
   set sim(NoPrev)    -1
   set sim(NoSim)     -1
   set sim(NoExp)     $No
   set sim(Pos)       $Pos
   set sim(NameExp)   $Name

   set sim(Backward)  False
   set sim(Mode)      prog
   set sim(Scale)     ""
   set sim(Event)     ""

   set sim(ReNewMeteo) ""
   set sim(GridChanged) 0
   set sim(Grid) 0

   set sim(Name)   {}
   set sim(Lat)    {}
   set sim(Lon)    {}
   foreach src $sim(Pos) {
      lappend sim(Name) [lindex $src 0]                 ;#----- List of source names.
      lappend sim(Lat)  [format "%.6f" [lindex $src 1]] ;#----- Latitude.
      lappend sim(Lon)  [format "%.6f" [lindex $src 2]] ;#----- Longitude.
   }
   set sim(GridSrc)      [lindex $sim(Pos) 0]
   set sim(GridLat)      [lindex $sim(GridSrc) 1]
   set sim(GridLon)      [lindex $sim(GridSrc) 2]

   #----- Initialize grid related stuff
   catch {
      fstdfield free MODELGRID
      set Data(Frame) $Page::Data(Frame)
      set Data(VP)    $Viewport::Data(VP)
   }
   return ${modelbase}
}

#----------------------------------------------------------------------------
# Nom        : <Model::FitAccTime>
# Creation   : 22 March 2004 - A. Malo - CMC/CMOE
#
# But        : Fit accident time using model time step.
#
# Parametres :
#  <Model>   : Model
#
# Retour     :
#
# Remarques  :
#
#----------------------------------------------------------------------------

proc Model::FitAccTime { Model } {
   variable Sim

   set modelbase [string trimright [string trimright $Model 0] 1]
   upvar ${modelbase}::Sim sim

   set sec [clock scan "$sim(AccYear)$sim(AccMonth)$sim(AccDay) $sim(AccHour):00" -gmt true]

   set min [string trimleft $sim(AccMin) 0]
   if { $min=="" } {
      set min 0
   }
   set min [expr int(double($min)/double($sim(ModelTimeStepMin))+0.5) * $sim(ModelTimeStepMin)]

   set sim(AccSeconds) [expr $sec + $min*60]
   set sim(AccMin)     [clock format $sim(AccSeconds) -format "%M" -gmt true]
   set sim(AccHour)    [clock format $sim(AccSeconds) -format "%H" -gmt true]
   set sim(AccDay)     [clock format $sim(AccSeconds) -format "%d" -gmt true]
   set sim(AccMonth)   [clock format $sim(AccSeconds) -format "%m" -gmt true]
   set sim(AccYear)    [clock format $sim(AccSeconds) -format "%Y" -gmt true]
}

#----------------------------------------------------------------------------
# Nom        : <Model::ParamsWindow>
# Creation   : 2 October 2007 - A. Malo - CMC/CMOE
#
# But        : Interface de lancement des modeles.
#
# Parametres :
#  <Model>   : Model
#  <Frame>   : Fenetre parent
#
# Retour     :
#
# Remarques  :
#
#----------------------------------------------------------------------------

proc Model::ParamsWindow { Model { Mode NEW } } {
   global GDefs
   variable Lbl
   variable Msg
   variable Param

   if { [winfo exists .modelnew] } {
      Dialog::Info .modelnew $Msg(Exist)
      return
   }
   set Data(Modelbase) [Model::InitNew $Model $Exp::Data(No) $Exp::Data(Name) $Exp::Data(Pos)]
   set Data(Model)     $Model
   set ${Data(Modelbase)}::Sim(Auto) False

   #----- Create model param window
   toplevel     .modelnew
   wm title     .modelnew "Model $Model: $Exp::Data(Name)"
   wm transient .modelnew .
   wm resizable .modelnew 0 0
   wm geom      .modelnew =300x350+[winfo rootx .]+[expr [winfo rooty .]+30]
   wm protocol  .modelnew WM_DELETE_WINDOW "Model::ParamsClose $Data(Modelbase)"

   TabFrame::Create .modelnew.params 1 ${Data(Modelbase)}::ParamsCheck
   pack .modelnew.params -side top -fill both -expand true -padx 5 -pady 5

   #----- Define default host if not already done
   eval set hosts \${${Data(Modelbase)}::Sim(Hosts)}
   if { ![info exists ::Model::Param(Host$Data(Modelbase))] || [lsearch -exact $hosts $Param(Host$Data(Modelbase))]==-1 } {
       set Param(Host$Data(Modelbase)) [lindex $hosts 0]
   }
   #----- Keep last host selected per model.
   set Param(Host) $Param(Host$Data(Modelbase))

   #----- Model specific parameters
   switch $Mode {
      "NEW" {
         ${Data(Modelbase)}::InitNew $Exp::Data(Type)
         ${Data(Modelbase)}::ParamsNew .modelnew.params
      }
      "CONT" {
         #----- For this, we have to get the parametres from the previous simulation
         Info::Decode ::${Data(Modelbase)}::Sim $Exp::Data(SelectSim)
         ${Data(Modelbase)}::InitCont $Exp::Data(Type)
         ${Data(Modelbase)}::ParamsCont .modelnew.params
      }
      "RENEW" {
         ${Data(Modelbase)}::InitNew $Exp::Data(Type)
         #----- For this, we have to point the meteo the renewed simulation's meteo
         Info::Decode ::${Data(Modelbase)}::Sim $Exp::Data(SelectSim)
         set ::${Data(Modelbase)}::Sim(ReNewMeteo) [Exp::Path]/[Info::Path $Exp::Data(SelectSim)]/meteo
      }
   }

   if { [info proc ::${Data(Modelbase)}::ParamsEmission]!="" } {
      ${Data(Modelbase)}::ParamsEmission .modelnew.params $Mode
   }
   Model::ParamsGridDefine $Data(Modelbase) $Mode

   #----- Launching Tab.
   Model::ParamsLaunch $Data(Modelbase) .modelnew.params

   button .modelnew.close -text [lindex $Lbl(Close) $GDefs(Lang)] -bd 1 -command "Model::ParamsClose $Data(Modelbase)"
   pack .modelnew.close -side bottom -anchor e -padx 5 -pady 5

   TabFrame::Select .modelnew.params 0
}

proc Model::ParamsLaunch { Model Frame } {
   global GDefs
   variable Lbl
   variable Bubble

   upvar ${Model}::Sim sim

   Model::ParamsCheck $Model False

   #----- Launching Tab.
   set Model::Param(Frame) [set tabframe [TabFrame::Add $Frame 1 "[lindex $Lbl(Launch) $GDefs(Lang)]" False]]

   labelframe $tabframe.params -text "[lindex $Lbl(Params) $GDefs(Lang)]"

   #----- Host.
   Option::Create $tabframe.params.host [lindex $Lbl(Host) $GDefs(Lang)] Model::Param(Host) 0 -1 $sim(Hosts) "Model::ParamsCheck $Model"
   pack $tabframe.params.host -side top -anchor w -padx 2 -fill x
   Bubble::Create $tabframe.params.host $Bubble(Host)

   #----- Queue.
   Option::Create $tabframe.params.queue [lindex $Lbl(Queue) $GDefs(Lang)] Model::Param(Queue) 0 -1 $Model::Param(Queues) ""
   pack $tabframe.params.queue -side top -anchor w -padx 2 -fill x
   Bubble::Create $tabframe.params.queue $Bubble(Queue)

   if { $sim(Model)=="MLDP1" || $sim(Model)=="MLDP0" || $sim(Model)=="MLDPn" } {
      #----- Nb CPUs for meteorological preprocessing.
      Option::Create $tabframe.params.metcpu [lindex $Lbl(MetCPU) $GDefs(Lang)] Model::Param(NbCPUsMeteo) 0 -1 $Model::Param(ListNbCPUsMeteo) ""
      pack $tabframe.params.metcpu -side top -anchor w -padx 2 -fill x
      Bubble::Create $tabframe.params.metcpu $Bubble(MetCPU)
   }

   if { $sim(Model)=="MLDP1" || $sim(Model)=="MLDPn" } {

      #----- Nb MPI tasks for model.
      Option::Create $tabframe.params.mpi [lindex $Lbl(NbMPItasks) $GDefs(Lang)] Model::Param(NbMPItasks) 0 -1 $Model::Param(ListNbMPItasks) ""
      pack $tabframe.params.mpi -side top -anchor w -padx 2 -fill x
      Bubble::Create $tabframe.params.mpi $Bubble(NbMPItasks)

      #----- Nb OMP threads for model.
      Option::Create $tabframe.params.omp [lindex $Lbl(NbOMPthreads) $GDefs(Lang)] Model::Param(NbOMPthreads) 0 -1 $Model::Param(ListNbOMPthreads) ""
      pack $tabframe.params.omp -side top -anchor w -padx 2 -fill x
      Bubble::Create $tabframe.params.omp $Bubble(NbOMPthreads)
   }

   if { $sim(Model)=="MLDP1" } {
      #----- OMP thread factor.
      Option::Create $tabframe.params.smt [lindex $Lbl(OMPthreadFact) $GDefs(Lang)] Model::Param(OMPthreadFact) 0 -1 $Model::Param(ListOMPthreadFact) ""
      pack $tabframe.params.smt -side top -anchor w -padx 2 -fill x
      Bubble::Create $tabframe.params.smt $Bubble(OMPthreadFact)
   }

   #----- Enabling/Disabling email monitoring option.
   checkbutton $tabframe.params.emonitor -anchor w -text "[lindex $Lbl(IsEMail) $GDefs(Lang)]" -offvalue 0 -onvalue 1 \
       -variable Model::Param(IsEMail) -indicatoron true \
      -command "if { \$Model::Param(IsEMail) } {
                   pack $tabframe.params.email -after $tabframe.params.emonitor -side top -anchor w -padx 2 -fill x
                } else {
                   pack forget $tabframe.params.email
                }"
   pack $tabframe.params.emonitor -side top -anchor w
   Bubble::Create $tabframe.params.emonitor $Bubble(IsEMail)

   #----- Email address.
   Option::Create $tabframe.params.email [lindex $Lbl(EMail) $GDefs(Lang)] Model::Param(EMailSet) 1 -1 $Model::Param(ListEMail) ""
   if { $Model::Param(IsEMail) } {
      pack $tabframe.params.email -side top -anchor w -padx 2 -fill x
   }
   Bubble::Create $tabframe.params.email $Bubble(EMail)

   #----- Button.
   button $tabframe.params.launch -text "[lindex $Lbl(LaunchModel) $GDefs(Lang)]" -bd 1 -command "Model::Launch ${Model}"
   pack $tabframe.params.launch -side top -anchor w -padx 2 -pady 2 -anchor e
   Bubble::Create $tabframe.params.launch $Bubble(LaunchModel)

   pack $tabframe.params -side top -padx 5 -pady 5 -fill x
}

#----------------------------------------------------------------------------
# Nom        : <Model::Launch>
# Creation   : Juin 2009 - J.P. Gauthier - CMC/CMOE
#
# But        : Validate and launch model run.
#
# Parametres :
#
# Retour     :
#   <Idx>    : Flag indicating if validation has succeeded (1) or not (0).
#
# Remarques  :
#
#----------------------------------------------------------------------------

proc Model::Launch { Model } {
   variable Param

   upvar ${Model}::Sim sim

   if { ![Model::ParamValidateQueue] } {
      return False
   }
   if { ![Model::ParamValidateEmail] } {
      return False
   }

   #----- Check user params
   if { ![Exp::Params . ${Model}] } {
      return False
   }

   #----- Check available disk space
   if { ![Model::ParamsCheckDiskSpace $Exp::Param(Path) 5.0] } {
      return False
   }

   #----- Define simulations paths
   if { [Model::ParamsPath ${Model}]=="" } {
      return False
   }

   . config -cursor watch
   update idletasks
   set sim(State) 2

   #----- Try to lauch the model
   if { [${Model}::Launch] } {
      destroy [winfo toplevel $Param(Frame)]
      Info::Set $sim(Path)/../$sim(Model).pool [Info::Code ${Model}::Sim]
      Model::Check 0

      Model::ParamsClose ${Model}
   }

   . config -cursor left_ptr
   return True
}

#----------------------------------------------------------------------------
# Nom        : <Model::ParamsCheckDiskSpace>
# Creation   : 16 January 2008 - A. Malo - CMC/CMOE
#
# But        : Display warning message if available disk space if lower
#              than critical value.
#
# Parametres :
#   <Path>   : Repertoire
#   <Max>    : Espace disque critique (en Giga-Octets)
#
# Retour     :
#   <Idx>    : Flag indicating if validation has succeeded (1) or not (0).
#
# Remarques  :
#
#----------------------------------------------------------------------------

proc Model::ParamsCheckDiskSpace { Path Max } {
   global   GDefs
   variable Warning
   variable Lbl

   #----- Get disk space information.
   set fsinfo [system filesystem $Path -free -used]
   set free [lindex $fsinfo 0]
   set used [lindex $fsinfo 1]

   if { [expr $free/(1024.0*1024.0)]<$Max } {
      set Max [expr $Max*1024.0*1024.0] ; #----- Convert GB to KB.
      set info "\n\n[lindex $Warning(DiskPath) $GDefs(Lang)] : $Path\n[lindex $Warning(DiskCritical) $GDefs(Lang)] : [Convert::KBytes2Human $Max]\n[lindex $Warning(DiskAvailable) $GDefs(Lang)] : [Convert::KBytes2Human $free]\n[lindex $Warning(DiskUsed) $GDefs(Lang)] : [Convert::KBytes2Human $used]"
      if { [Dialog::Default .modelnew 700 WARNING $Warning(DiskSpace) "$info" 1 $Lbl(Yes) $Lbl(No)] } {
         return False
      }
   }
   return True
}

#----------------------------------------------------------------------------
# Nom        : <Model::ParamsPath>
# Creation   : Juillet 2009 - A. Malo - CMC/CMOE
#
# But        : Construire les paths locaux et remote pour la simulation courante.
#
# Parametres :
#  <Model>   : Model
#  <ReqNo>   : Request simulation number
#
# Retour     :
#
# Remarques  :
#
#----------------------------------------------------------------------------

proc Model::ParamsPath { Model { ReqNo True } } {
   global GDefs
   variable Param
   variable Error

   upvar ${Model}::Sim sim

   #----- Define simulation path.
   if { $ReqNo } {
      set sim(NoSim) [Info::Request $Exp::Param(Path)/$sim(NoExp)_$sim(NameExp)/$sim(Model).pool]
   }
   set expp       "$sim(NoExp)_$sim(NameExp)"
   set simp       "$sim(Model).$sim(NoSim).$sim(AccYear)$sim(AccMonth)$sim(AccDay).$sim(AccHour)$sim(AccMin)"
   set prevp      "$sim(Model).$sim(NoPrev).$sim(AccYear)$sim(AccMonth)$sim(AccDay).$sim(AccHour)$sim(AccMin)"

   set sim(Path)     "$Exp::Param(Path)/${expp}/${simp}"
   set sim(PathPrev) "$Exp::Param(Path)/${expp}/${prevp}"

   if { [file exists $sim(Path)] } {
      file delete -force $sim(Path)
   }
   file mkdir $sim(Path) $sim(Path)/results $sim(Path)/meteo $sim(Path)/tmp $sim(Path)/products

   #----- Check for remote path::${Model}::Sim
   if { $Param(Arch) == "AIX" } {
      set Param(Remote) True
   } else {
      set Param(Remote) [catch { exec ssh -l $GDefs(FrontEndUser) -n -x $Param(Host) ls $sim(Path) }]
   }

   if { $Param(Remote) } {
      if { $Param(Arch) == "AIX" } {
         set sim(PathRun)  "[lindex $GDefs(Host_$Param(Host)) 3]/eer_Experiment/${expp}_${simp}"
         set sim(PathPrev) "[lindex $GDefs(Host_$Param(Host)) 3]/eer_Experiment/${expp}_${prevp}"
      } else {
         Dialog::Error . $Error(Path) "\n\t($Param(Host):$sim(Path))"
         set sim(Path)     ""
         set sim(PathRun)  ""
         set sim(PathPrev) ""
      }
   } else {
      set sim(PathRun) $sim(Path)
   }

   #----- Save simulation pool information.
   exec echo "[Info::Code ${Model}::Sim]" > $sim(Path)/tmp/sim.pool
   return $sim(Path)
}

#----------------------------------------------------------------------------
# Nom        : <Model::ParamsMeteoInput>
# Creation   : 30 August 2007 - A. Malo - CMC/CMOE
#
# But        : Create meteorological input files.
#                - Input file containing list of meteorological files.
#                - Trace information output file containing list of meteorological standard files for simulation.
#                - Input file containing grid parameters.
#
# Parametres :
#  <Model>   : Model
#
# Retour     :
#  <Bool>    : True ou False.
#
# Remarques  :
#
#----------------------------------------------------------------------------

proc Model::ParamsMeteoInput { Model } {
   global env
   variable Param

   upvar ${Model}::Sim sim

   if { $sim(ReNewMeteo)!="" } {
      file copy -force $sim(ReNewMeteo)/../tmp/data_std_eta.in $sim(Path)/tmp/data_std_eta.in
      set sim(MeteoDataFiles) [exec cat $sim(Path)/tmp/data_std_eta.in]
      set Param(DBaseProg) [set Param(DBaseDiag) [file dirname [lindex $sim(MeteoDataFiles) 0]]]
   } else {
      set file [open $sim(Path)/tmp/data_std_eta.in w 0644]
      puts $file $sim(MeteoDataFiles)
      close $file
   }

   #----- Create ASCII file containing list of meteorological files for RSMC response.
   if { [regexp "/gridpt/" $Param(DBaseProg)] && [regexp "/gridpt/" $Param(DBaseDiag)] } {

      set files {}

      if { $sim(Meteo)=="reg" } { #----- Regional NWP met model.
         regsub -all "/fs/ops/cmo/gridpt/dbase" $sim(MeteoDataFiles) "$env(CMCGRIDF)" files
         regsub -all "/regeta/"    $files               "/regpres/"  files
         regsub -all "/reghyb/"    $files               "/regpres/"  files
         regsub -all "/regeta2/"   $files               "/regpres2/" files
         regsub -all "/reghyb2/"   $files               "/regpres2/" files
      } elseif { $sim(Meteo) == "glb" } { #----- Global NWP met model.
         regsub -all "/fs/ops/cmo/gridpt/dbase" $sim(MeteoDataFiles) "$env(CMCGRIDF)"      files
         regsub -all "/glbeta/"    $files               "/glbpres/"  files
         regsub -all "/glbhyb/"    $files               "/glbpres/"  files
         regsub -all "/glbeta2/"   $files               "/glbpres2/" files
         regsub -all "/glbhyb2/"   $files               "/glbpres2/" files
      }

      if { [llength $files] } {
         set file [open $sim(Path)/tmp/data_std_pres.in w 0644]
         puts $file $files
         close $file
      }
   }

   #----- Create ASCII file containing grid parameters.
   if { [llength $sim(Grid)]>5 } {
      exec echo [format "%.0f,%.0f,%.7f,%.7f,%.7f,%.7f,%s" \
         [lindex $sim(Grid) 1] [lindex $sim(Grid) 2] [lindex $sim(Grid) 3] [lindex $sim(Grid) 4] \
         [lindex $sim(Grid) 5] [lindex $sim(Grid) 6] [lindex $sim(Grid) 0]] > $sim(Path)/tmp/griddef.in
   }
}

#----------------------------------------------------------------------------
# Nom        : <Model::ParamValidateEmail>
# Creation   : 2 November 2007 - A. Malo - CMC/CMOE
#
# But        : Validate email address.
#
# Parametres :
#
# Retour     :
#   <Idx>    : Flag indicating if validation has succeeded (1) or not (0).
#
# Remarques  :
#
#----------------------------------------------------------------------------

proc Model::ParamValidateEmail { } {
   global GDefs
   global env
   variable Error
   variable Param
   variable Lbl
   variable Warning

   if { $Param(IsEMail) } {

      set err 0
      if { $Param(EMailSet)=="" } {
         set err 1
      }

      if { [set idx [string last "@" $Param(EMailSet)]]==-1 } {
         set err 1
      } else {
         set name [string range $Param(EMailSet) 0 [expr $idx-1]]
         foreach char { \  , ; : ~ ` ! @ \# $ % ^ & * ? \( \) + / = < > \" \\ [ ] \{ \} | é è ê ë à â ä ì î ï ç É È Ê Ë À Â Ä Ì Î Ï Ç } {
            if { [string last "${char}" $name]!=-1 } {
               set err 1
               break
            }
         }
         if { $name=="" } {
            set err 1
         }
      }

      if { $err } {
         Dialog::Error $Param(Frame) $Error(EMail) $Param(EMailSet)
         focus $Param(Frame).params.email.e
         return 0
      }

      #----- Display warning if email is different than default one.
      if { $Param(EMailSet)!="$env(USER)@ec.gc.ca" } {
         set answer [Dialog::Default $Param(Frame) 400 WARNING $Warning(EMail) "\n\n[lindex $Warning(EMail2) $GDefs(Lang)] $Param(EMailSet)\n[lindex $Warning(EMail3) $GDefs(Lang)] $env(USER)@ec.gc.ca" 1 $Lbl(Yes) $Lbl(No)]
         if { $answer } {
            focus $Param(Frame).params.email.e
            return 0
         }
      }
      set Param(EMail) $Param(EMailSet)
   } else {
      set Param(EMail) ""
   }
   return 1
}

#----------------------------------------------------------------------------
# Nom        : <Model::ParamValidateQueue>
# Creation   : 7 February 2008 - A. Malo - CMC/CMOE
#
# But        : Validate type of submitting queue/class.
#
# Parametres :
#
# Retour     :
#   <Idx>    : Flag indicating if validation has succeeded (1) or not (0).
#
# Remarques  :
#
#----------------------------------------------------------------------------

proc Model::ParamValidateQueue { } {
   global GDefs
   variable Param
   variable Lbl
   variable Warning

   set Param(Op) ""
   if { $Param(Queue)=="production" || [string range $Param(Queue) 0 1]=="op" } {
      set Param(Op) -op
      if { [info exists ::tk_version] } {
         if { [Dialog::Default $Param(Frame) 400 WARNING $Warning(Queue) "" 1 $Lbl(Yes) $Lbl(No)] } {
#            set Param(Queue) "development"
            return 0
         }
      }
   }
   return 1
}

#-------------------------------------------------------------------------------
# Nom      : <Model::DrawCurrent>
# Creation : Mai 2000 - J.P. Gauthier - CMC/CMOE
#
# But      : Afficher l'icone de l'experience courante sur la projection.
#
# Parametres :
#
# Remarques :
#
#-------------------------------------------------------------------------------

proc Model::DrawCurrent { } {
   variable Data
   variable Resources

   set oka [catch { set h [expr int($SPI::Src(Lat))] }]
   set oko [catch { set h [expr int($SPI::Src(Lon))] }]

   if { !$oka && !$oko } {
      SPI::IcoAdd $Page::Data(Frame) SOURCE "" [list [list "$SPI::Src(Name)" $SPI::Src(Lat) $SPI::Src(Lon) 0 [lindex $Resources(Icos) $SPI::Src(Type)]]]
   }
}

#----------------------------------------------------------------------------
# Nom      : <Model::Window>
# Creation : Aout 2001 - J.P. Gauthier - CMC/CMOE
#
# But      : Creer une boite d'experiences.
#
# Parametres :
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc Model::Window { { Show "" } } {
   global   GDefs
   variable Lbl
   variable Bubble
   variable Data
   variable Title
   variable Param

   if { $Show!="" } {
      set Param(Show) $Show
   }

   if { ![winfo exists .model] } {

      if { $Param(Dock) } {
         frame .model
         SPI::Dock .model
      } else {
         toplevel          .model
         wm title          .model "[lindex $SPI::Title(SPI) $GDefs(Lang)] $GDefs(Version) ([lindex $Title $GDefs(Lang)])"
         wm transient      .model .
         eval wm geometry  .model $Param(Geom)
         wm protocol       .model WM_DELETE_WINDOW { set Model::Param(Show) False; Model::Destroy }
      }

      frame .model.dock -relief raised -bd 1
         button .model.dock.sel -image DOCK -anchor w -relief flat -bd 0 -overrelief raised -command Model::Dock
         button .model.dock.del -image DOCKDELETE -anchor w -relief flat -bd 0 -overrelief raised -command { set Model::Param(Show) False; Model::Destroy }
         label .model.dock.info -textvariable Model::Param(Job) -relief sunken -bd 1 -anchor w -width 21 -bg $GDefs(ColorLight)
         pack .model.dock.sel .model.dock.del -side left
         pack .model.dock.info -side left -fill x -expand true
      pack .model.dock -side bottom -fill x

      Bubble::Create .model.dock.sel $Bubble(Dock)
      Bubble::Create .model.dock.del $Bubble(Close)

      TabFrame::Create .model.tab 1 Model::TypeSelect 1 350
      pack .model.tab -side top -fill both -expand true

      Meteo::Create [TabFrame::Add .model.tab 1 "Meteo" False]
      Exp::Create   [TabFrame::Add .model.tab 1 [lindex $Lbl(Emerg) $GDefs(Lang)] False]
      Watch::Create [TabFrame::Add .model.tab 1 [lindex $Lbl(Watch) $GDefs(Lang)] False]
   }

   if { !$Param(Show) } {
      Model::Destroy
   }

   #----- Preparer la liste.
   Model::Check $Param(Delay)

   TabFrame::Select .model.tab 1

   return .model
}

#----------------------------------------------------------------------------
# Nom      : <Model::Dock>
# Creation : Mars 2004 - J.P. Gauthier - CMC/CMOE
#
# But      : Inserer la boite dans l'interface.
#
# Parametres :
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc Model::Dock { } {
   variable Param

   if { $Param(Dock) } {
      set Param(Dock) False
   } else {
      set Param(Dock) True
   }

   Model::Destroy
   Model::Window
}

#-------------------------------------------------------------------------------
# Nom      : <Model::Check>
# Creation : Fevrier 1999 - J.P. Gauthier - CMC/CMOE
#
# But      : Verifie les repertoires toute les "ms" millisecondes.
#
# Parametres :
#   <MS>     : Nombre de millisecondes d'interval.
#
# Remarques :
#
#-------------------------------------------------------------------------------

proc Model::Check { MS } {
   global GDefs
   variable Param
   variable Lbl

   if { [winfo exists .model] } {

      set Param(Job) "[lindex $Lbl(Checking) $GDefs(Lang)]"
      .model config -cursor watch
      update idletask

      Exp::Read
      Exp::CreateTree

      Watch::Read
      Watch::CreateTree

      set Param(Job) "[lindex $Lbl(Checked) $GDefs(Lang)] [clock format [clock seconds] -format %T -gmt true]"

      .model config -cursor left_ptr
   }

   #----- Instauration de l'evenement de verification de repertoires.
   if { $MS != 0 } {
      set Param(Handle) [after $MS [list Model::Check $MS]]
   }
}

#----------------------------------------------------------------------------
# Nom      : <Model::Destroy>
# Creation : Aout 2001 - J.P. Gauthier - CMC/CMOE
#
# But      : Detruire la boite d'experiences.
#
# Parametres :
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc Model::Destroy { } {
   variable Param

   #----- Ajuster la geometrie
   TabFrame::Destroy .model.tab
   destroy .model

   #----- Supprimer le refresh.
   after cancel $Param(Handle)

   #----- Supprimer l'affichage des icones
   SPI::IcoDel WATCH
   SPI::IcoDel EXPERIMENT
}

#-------------------------------------------------------------------------------
# Nom      : <Model::New>
# Creation : Aout 2001 - J.P. Gauthier - CMC/CMOE -
#
# But      : Creation d'une nouvelle experience.
#
# Parametres :
#   <Parent> : Identificateur de la fenetre parent.
#
# Retour :
#
# Remarques :
#    -On utilise les variables interne de SPI SPI::Src(*) pour recuperer directement
#     les informations provenant du selecteur de source et du dictionnaire.
#
#-------------------------------------------------------------------------------

proc Model::New { Parent Command Label Single } {
   global GDefs
   variable Lbl
   variable Bubble
   variable Data
   variable Resources

   if { [winfo exist .expnew] } {
      return
   }

   toplevel     .expnew
   wm transient .expnew $Parent
   wm geom      .expnew +[expr [winfo rootx $Parent]+50]+[expr [winfo rooty $Parent]+50]
   wm protocol  .expnew WM_DELETE_WINDOW { }
   wm resizable .expnew 0 0
   wm title     .expnew "$Label"

   #----- Initialiser les variables de localisations
   set Data(Type)  0
   set Data(Name)  "New experiment"
   set Data(Pos)   1
   set Data(Id1)   "-"
   set Data(Name1) "New location"
   set Data(Lat1)  0.0
   set Data(Lon1)  0.0

   for { set i 2 } { $i <=50 } { incr i } {
      set Data(Id$i)   "-"
      set Data(Name$i) ""
      set Data(Lat$i)  ""
      set Data(Lon$i)  ""
   }

   frame .expnew.hd
      checkbutton .expnew.hd.mode -variable Page::Data(ToolMode) -onvalue Model -offvalue SPI -image ARROW -indicatoron 0 \
         -bd 1 -selectcolor $GDefs(ColorFrame) -command { SPI::ToolMode $Page::Data(ToolMode) Data True }
      button .expnew.hd.src -text "Identification      Lat          Lon" -anchor w -bd 1 -command "Locator::Window 0"
      pack .expnew.hd.mode -side left -fill y
      pack .expnew.hd.src -side left -fill both -expand true
   pack .expnew.hd -side top -fill x -expand true

   frame .expnew.info -bd 1 -relief raised
      button .expnew.info.coord -relief groove -bd 2 -textvariable Model::Param(Unit)\
         -command "Model::SwitchCoord"

      frame .expnew.info.l
      if { $Single } {
         frame .expnew.info.l.loc1
            radiobutton .expnew.info.l.loc1.sel -text "  " -indicatoron false -bd 1 \
               -variable Model::Data(Pos) -value 1 -selectcolor $GDefs(ColorHighLight)
            EntryVar::Create .expnew.info.l.loc1.name Model::Data(Name) string 25 "Model::DrawCurrent" \
               -bd 1 -bg $GDefs(ColorLight)
            EntryVar::Create .expnew.info.l.loc1.lat Model::Data(Lat1) coordinate 0 "Model::DrawCurrent" \
               -bd 1 -width 7 -bg $GDefs(ColorLight) -justify right
            EntryVar::Create .expnew.info.l.loc1.lon Model::Data(Lon1) coordinate 0 "Model::DrawCurrent" \
               -bd 1 -width 7 -bg $GDefs(ColorLight) -justify right
            pack .expnew.info.l.loc1.sel -ipadx 2  -side left
            pack .expnew.info.l.loc1.name .expnew.info.l.loc1.lat .expnew.info.l.loc1.lon -side left -fill y -expand true
         pack .expnew.info.l.loc1 -side top -fill y -expand true

         pack .expnew.info.l .expnew.info.coord -side left -fill y -expand true
         pack .expnew.info -side top
      } else {

         foreach i "1 2 3" {
            frame .expnew.info.l.loc$i
               radiobutton .expnew.info.l.loc$i.sel -text "0$i" -indicatoron false -bd 0 \
                  -variable Model::Data(Pos) -value $i -selectcolor $GDefs(ColorHighLight)
               EntryVar::Create .expnew.info.l.loc$i.name Model::Data(Name$i) string 25 "Model::DrawCurrent" \
                  -bd 1 -bg $GDefs(ColorLight)
               EntryVar::Create .expnew.info.l.loc$i.lat Model::Data(Lat$i) coordinate 0 "Model::DrawCurrent" \
                  -bd 1 -width 12 -bg $GDefs(ColorLight) -justify right
               EntryVar::Create .expnew.info.l.loc$i.lon Model::Data(Lon$i) coordinate 0 "Model::DrawCurrent" \
                  -bd 1 -width 12 -bg $GDefs(ColorLight) -justify right
               pack .expnew.info.l.loc$i.sel -ipadx 2 -side left
               pack .expnew.info.l.loc$i.name .expnew.info.l.loc$i.lat .expnew.info.l.loc$i.lon -side left
            pack .expnew.info.l.loc$i -side top
         }
         scale .expnew.info.sc -orient vertical -command "Model::Scroll" -relief flat -sliderlength 10 -width 10 -bd 1\
            -showvalue false -length 50 -from 1 -to 48 -resolution 1
         pack .expnew.info.l .expnew.info.sc .expnew.info.coord -side left -fill y
         pack .expnew.info -side top


         frame .expnew.group -bd 1 -relief raised
            label .expnew.group.lbl -text "Id"
            entry .expnew.group.ent -textvariable Model::Data(Name) -width 20 -bd 1 -bg $GDefs(ColorLight)

            frame .expnew.group.type -bd 1 -relief sunken
               foreach type { 0 1 2 3 4 5 6 } ico { VAAC RSMC CTBT FIRE BIO SPILL SPCL } {
                  radiobutton .expnew.group.type.t$ico -image [lindex $Resources(Icos) $type] -variable Model::Data(Type) \
                     -value $type -indicatoron False -selectcolor $GDefs(ColorFrame) -command "Model::DrawCurrent" -bd 1
                  pack .expnew.group.type.t$ico -side left -fill x -expand true

                  Bubble::Create .expnew.group.type.t$ico $Bubble(Type$type)
               }
            pack .expnew.group.lbl -side left -anchor w
            pack .expnew.group.ent -side left -fill y
            pack .expnew.group.type -side left -fill both -expand true

         pack .expnew.group -side top -fill x
      }

   frame .expnew.commands
      button .expnew.commands.create -text [lindex $Lbl(Create) $GDefs(Lang)] \
         -command "if { \[$Command\] } { Model::NewClose }" \
         -relief raised -bd 1
      button .expnew.commands.cancel -text [lindex $Lbl(Cancel) $GDefs(Lang)] \
         -command "Model::NewClose" -relief raised -bd 1
      pack .expnew.commands.create .expnew.commands.cancel -side left -fill x -expand true
   pack .expnew.commands -side top -fill x

   Bubble::Create .expnew.group.ent       $Bubble(Name)
   Bubble::Create .expnew.info.coord      $Bubble(Coord)
   Bubble::Create .expnew.info            $Bubble(Info)
   Bubble::Create .expnew.commands.create $Bubble(Create)
   Bubble::Create .expnew.commands.cancel $Bubble(Cancel)

   trace variable SPI::Src(Info) w "Model::Source"
}

#-------------------------------------------------------------------------------
# Nom      : <Model::NewClose>
# Creation : Novembre 2002 - J.P. Gauthier - CMC/CMOE -
#
# But      : Terminer la selection de source pour une experience
#
# Parametres :
#
# Retour :
#
# Remarques :
#    -On remet en mode zoom
#    -On supprime les icones
#    -On supprime la trace des sources
#    -On detruit la fenetre
#
#-------------------------------------------------------------------------------

proc Model::NewClose { } {

   if { $Page::Data(ToolMode)=="Model" } {
      SPI::ToolMode SPI Zoom
   }
   SPI::IcoDel SOURCE

   trace vdelete SPI::Src(Info) w { Model::Source }
   destroy .expnew
}

#----------------------------------------------------------------------------
# Nom      : <Model::Source>
# Creation : Mars 2002 - J.P. Gauthier - CMC/CMOE
#
# But      : Recuperer la selection des outils de localisations.
#
# Parametres :
#    <>      : Parametres de renvoie des trace
#
# Retour :
#
# Remarques :
#
#----------------------------------------------------------------------------

proc Model::Source { Array Index Op } {
   variable Data

   if { $SPI::Src(Info)!="" } {
      set Data(Type)           $SPI::Src(Type)

      set Data(Id$Data(Pos))   $SPI::Src(No)
      set Data(Name$Data(Pos)) "$SPI::Src(Name)"
      set Data(Lat$Data(Pos))  $SPI::Src(Lat)
      set Data(Lon$Data(Pos))  $SPI::Src(Lon)

      set Data(Name) $Data(Name1)
   }
}

#----------------------------------------------------------------------------
# Nom      : <Model::Scroll>
# Creation : Mars 2002 - J.P. Gauthier - CMC/CMOE
#
# But      : Effectuer un "scroll" de la liste de localisations.
#
# Parametres    :
#    <Frame>    : Identificateur du frame
#    <Val>      : Position actuelle
#
# Retour :
#
# Remarques :
#
#----------------------------------------------------------------------------

proc Model::Scroll { Val } {
   global GDefs
   variable Lbl

   .expnew.info.l.loc1.sel  configure -text "[format "%02i" $Val]" -value $Val
   .expnew.info.l.loc1.name configure -textvariable Model::Data(Name$Val)
   .expnew.info.l.loc1.lat  configure -textvariable Model::Data(Lat$Val)
   .expnew.info.l.loc1.lon  configure -textvariable Model::Data(Lon$Val)

   incr Val
   .expnew.info.l.loc2.sel  configure -text "[format "%02i" $Val]" -value $Val
   .expnew.info.l.loc2.name configure -textvariable Model::Data(Name$Val)
   .expnew.info.l.loc2.lat  configure -textvariable Model::Data(Lat$Val)
   .expnew.info.l.loc2.lon  configure -textvariable Model::Data(Lon$Val)

   incr Val
   .expnew.info.l.loc3.sel  configure -text "[format "%02i" $Val]" -value $Val
   .expnew.info.l.loc3.name configure -textvariable Model::Data(Name$Val)
   .expnew.info.l.loc3.lat  configure -textvariable Model::Data(Lat$Val)
   .expnew.info.l.loc3.lon  configure -textvariable Model::Data(Lon$Val)
}

#-------------------------------------------------------------------------------
# Nom      : <Model::SwitchCoord>
# Creation : Octobre 1999 - J.P. Gauthier - CMC/CMOE
#
# But      : Faire la conversion en degre ou en minute.
#
# Parametres :
#
# Retour     :
#
# Remarques :
#
#-------------------------------------------------------------------------------

proc Model::SwitchCoord { } {
   variable Data
   variable Param

   if { "$Param(Unit)" == "DDD MM" } {
      set Param(Unit) "DDD.CC"
      for { set i 1 } { $i <= 50 } { incr i } {
         if { $Data(Lat$i)!="" } {
            set Data(Lat$i) [Convert::Minute2Decimal $Data(Lat$i)]
         }
         if { $Data(Lon$i)!="" } {
            set Data(Lon$i) [Convert::Minute2Decimal $Data(Lon$i)]
         }
      }
   } else {
      set Param(Unit) "DDD MM"
      for { set i 1 } { $i <= 50 } { incr i } {
      if { $Data(Lat$i)!="" } {
            set Data(Lat$i) [Convert::Decimal2Minute $Data(Lat$i) 5]
         }
         if { $Data(Lon$i)!="" } {
            set Data(Lon$i) [Convert::Decimal2Minute $Data(Lon$i) 5]
         }
      }
   }
}

#----------------------------------------------------------------------------
# Nom      : <Model::TypeSelect>
# Creation : Aout 2001 - J.P. Gauthier - CMC/CMOE
#
# But      : Mettre en place les parametres selon l'onglet (type)
#            selectionne.
#
# Parametres :
#   <Frame>  : Frame de l'onglet
#   <No>     : Numero de l'onglet
#   <Loc>    : Localisation specifique
#   <Group>  : Groupe cpecifique
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc Model::TypeSelect { Frame No { Loc "" } { Group "" } } {
   variable Resources

   set icos ""

   SPI::IcoDel WATCH
   SPI::IcoDel EXPERIMENT

   switch $No {
      1 {
         foreach exp $Exp::Data(List) {
            if { $Loc=="" || $Loc=="[lindex $exp 0]_[lindex $exp 1]" } {
               foreach loc [lindex $exp 3] {
                  lappend icos "\"[lindex $loc 0] [lindex $exp 0]:[lindex $exp 1]\"\
                     [lindex $loc 1] [lindex $loc 2] 0 [lindex $Resources(Icos) [lindex $exp 2]]"
               }
            }
         }
         SPI::IcoAdd $Page::Data(Frame) EXPERIMENT "" $icos
         Exp::CreateTree
      }
      2 {
         foreach proj $Watch::Data(Projects) {
            if { $Group=="" || $proj==$Group } {
               set ico [Watch::GetIcon $proj]
               foreach watch [join $Watch::Data(Sources$proj)] {
                  if { $Loc=="" || [lindex $watch 0]==$Loc } {
                     lappend icos "[lindex $watch 0] [lindex $watch 1] [lindex $watch 2] 0 $ico"
                  }
               }
            }
         }
         SPI::IcoAdd $Page::Data(Frame) WATCH "" $icos
      }
   }
}

#----------------------------------------------------------------------------
# Nom      : <Model::Draw...>
# Creation : Octobre 2002 - J.P. Gauthier - CMC/CMOE
#
# But      : Fonctions de manipulation sur la projection
#
# Parametres :
#  <Frame>   : Identificateur de Page
#  <VP>      : Identificateur du Viewport
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc Model::DrawInit { Frame VP } {
   variable Data

   set Data(Id$Data(Pos))   "-"
   set Data(Name$Data(Pos)) "MousePoint"
   set Data(Lat$Data(Pos))   $Viewport::Map(LatCursor)
   set Data(Lon$Data(Pos))   $Viewport::Map(LonCursor)
}

proc Model::Draw { Frame VP } {
   variable Data

   set Data(Id$Data(Pos))   "-"
   set Data(Name$Data(Pos)) "MousePoint"
   set Data(Lat$Data(Pos))   $Viewport::Map(LatCursor)
   set Data(Lon$Data(Pos))   $Viewport::Map(LonCursor)
}

proc Model::DrawDone { Frame VP } { }
proc Model::MoveInit { Frame VP } { }
proc Model::Move     { Frame VP } { }
proc Model::MoveDone { Frame VP } { }
