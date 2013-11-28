#===============================================================================
# Environnement Canada
# Centre Meteorologique Canadian
# 2100 Trans-Canadienne
# Dorval, Quebec
#
# Projet   : Librairie de "Widget" Tk.
# Fichier  : Exp.tcl
# Creation : Septembre 2001 - J.P.Gauthier - CMC/CMOE
#
# Description:
#    Fonctions de manipualtions des Experiences.
#
# Remarques :
#
#===============================================================================

namespace eval Exp {
   global GDefs
   variable Data
   variable Param
   variable Lbl
   variable Msg

   set Param(Paths)     {}                                  ;#Liste des fichier d'experiences
   set Param(Path)      ""                                  ;#Fichier d'experiences courant
   set Param(StoreHost) ""
   set Param(StorePath) /tmp
   set Param(StoreLog)  /dev/null
   set Param(Proc)      $GDefs(Dir)/tcl/Models/procedures_0

   #----- Variables relatives aux experiences

   set Data(Models)    "SATDATA TRAJECT MLDP0 MLDP1 MLCD MLDPn"  ;#Liste des modeles disponibles

   set Data(ParsingTree) 0
   set Data(Branch)    ""                                 ;#Liste des branches d'experiences ouvertes
   set Data(BranchSim) ""                                 ;#Liste des branches de simulation d'experiences ouvertes
   set Data(Frame)     ""                                 ;#Container
   set Data(No)        ""                                 ;#Numero de l'experience selectionnee
   set Data(NoSim)     ""                                 ;#Numero de la simulation
   set Data(NoPrev)    "-1"                               ;#Numero de la simulation precedente
   set Data(Name)      ""                                 ;#Nom de l'experience selectionnee
   set Data(Lat)       ""                                 ;#Latitude de l'experience selectionnee
   set Data(Lon)       ""                                 ;#Longitude de l'experience selectionnee
   set Data(Type)      0                                  ;#Type de l'experience selectionnee
   set Data(List)      ""                                 ;#Liste des experiences affichees
   set Data(Select)    ""                                 ;#Experience selectionnee
   set Data(SelectSim) ""                                 ;#Simulation selectionnee
   set Data(State)     ""                                 ;#Etat de la simulation
   set Data(StoreID)   ""                                 ;#Identificateur d'archive

   #----- RSMC stuff.

   set Bubble(RSMCHelp1)    { "Ces boutons permettent de remplacer les produits existants des RSMCs\nsélectionnés par des produits vides sur la page web du RSMC Montréal (gris)\net/ou sur toutes les pages web miroirs des RSMCs (rouge)." "These buttons allow replacing the existing products from the selected RSMCs by\nblank products on RSMC Montreal's web page (gray) and/or on all RSMC's mirror\nweb pages (red)." }
   set Bubble(RSMCHelp2)    { "Ces boutons permmettent de remplacer les 'joint statements' existants des\nassociations régionales des RSMCs sélectionnées par des 'joint statements'\nvides sur la page web du RSMC Montréal (gris) et/ou toutes les pages web\nmiroirs des RSMCs (rouge)" "These buttons allow replacing the existing joint statements from the selected\nRSMCs regional associations by blank joint statements on RSMC Montreal's web\npage (gray) and/or on all RSMCs' mirror web pages (red)." }
   set Bubble(RSMCHelp3)    { "Ces boutons permettent de modifier la désignation du RSMC lead qui sera\nvisible sur toutes les pages web miroirs des RSMCs (rouge).  Le RSMC lead est\ndésigné/défini selon l'association régionale sélectionnée des RSMCs.  Le bouton\n'aucun' permet de ne pas désigner de RSMC lead.  Le bouton 'n.a.' permet de ne\npas faire de changement à la désignation du RSMC lead." "These buttons allow modifying the designation of the lead RSMC that will be\nvisible on all RSMCs' mirror web pages (red).  The lead RSMC is\ndesignated/defined according to the selected RSMCs regional association.\nThe button 'none' allows to not designate a lead RSMC.  The button 'n.a.'\nallows to not make any changes to the lead RSMC designation." }
   set Bubble(RSMCHelp4)    { "Ce bouton permet de supprimer toutes les cartes météorologiques du RSMC\nMontréal sur sa page web." "This button allows deleting the all meteorological charts from RSMC Montreal\non its web page." }

   set Lbl(Cancel)        { "Annuler" "Cancel" }
   set Lbl(Send)          { "Transmettre" "Transmit" }
   set Lbl(TransmitBlank) { "Transmission de produits vides" "Transmission of blank products" }

   set Lbl(RSMCLead)      { "Désignation du RSMC lead" "Designation of lead RSMC" }
   set Lbl(RSMCLeadNull)  { "aucun" "none" }
   set Lbl(RSMCMeteo)     { "Cartes météorologiques" "Meteorological charts" }
   set Lbl(RSMCWeb)       { "Produits des RSMCs" "RSMC products" }

   #----- Definitions des constantes relatives aux RSMC.

   set Data(RSMC_AU) 0
   set Data(RSMC_CA) 0
   set Data(RSMC_CN) 0
   set Data(RSMC_FR) 0
   set Data(RSMC_JP) 0
   set Data(RSMC_RU) 0
   set Data(RSMC_UK) 0
   set Data(RSMC_US) 0

   set Data(JntStat34) 0
   set Data(JntStat5)  0
   set Data(JntStat16) 0
   set Data(JntStat2)  0

   set Data(RSMCLead)  0
   set Data(RSMCMeteo) 0        ;#Carte meteo de CMC.

   #----- Labels

   set Lbl(New)                { "Nouveau" "New" }
   set Lbl(Product)            { "Produits" "Products" }
   set Lbl(RSMCFax)            { "Génération du FAX RSMC" "Generate RSMC FAX" }
   set Lbl(RSMCJointData)      { "Transmission des sorties RSMC vers les pages webs communes" "Transmit RSMC output to common web pages" }
   set Lbl(RSMCJointStatement) { "Transmission du Joint Statement RSMC vers les pages webs communes" "Transmit RSMC joint statement to common web pages" }
   set Lbl(RSMCBlank)          { "Transmission des produits RSMC vides vers les pages webs communes..." "Transmit RSMC blank products to common web pages..." }
   set Lbl(Store)              { "Archiver" "Store" }
   set Lbl(Suppress)           { "Supprimer" "Delete" }
   set Lbl(Yes)                { "Oui" "Yes" }
   set Lbl(No)                 { "Non" "No" }
   set Lbl(Params)             { "Paramètres" "Parameters" }

   #----- Messages

   set Msg(DoingStore)    { "Archivage en cours ..." "Archiving ..." }
   set Msg(DoingCopy)     { "Copie en cours ..." "Copying ..." }
   set Msg(Stored)        { "L'expérience à été archivée avec succès." "The experiment has been archived with success." }
   set Msg(Store)         { "Veuillez entrer le nom du rapport d'incident afin d'identifier l'expérience en archives."
                            "Please enter the incident report name to identify the experiment to be archived." }
   set Msg(StoreExist)    { "Une archive avec ce nom existe déjà, Voulez-vous l'écraser ?"
                            "An archive by this name already exist, do you want to overwrit it ?" }
   set Msg(Path)          { "Répertoire d'expériences invalides.\nImpossible de trouver le fichier eer_ExpList"
                            "Invalid experiment path.\n Unable to find the eer_ExpList file" }
   set Msg(Pos)           { "Auncune localisation n'à été spécifiée"
                            "No localisation have been specified" }
   set Msg(Fax)           { "Génération du fax dans le fichier suivant:"
                            "Generating fax to the following file:" }
   set Msg(JointData)     { "Êtes-vous certain de vouloir transférer les cartes RSMC commune ?" \
                            "Do you really want to send the RSMC joint format maps ?" }
   set Msg(JointStatement) { "Êtes-vous certain de vouloir transférer le joint statement ?" \
                            "Do you really want to send the joint statement ?" }
   set Msg(SendProducts)  { "Transfert des produits RSMC en cours" "Transferring RSMC products" }
   set Msg(SendJoint)     { "Transfert des message commun en cours" "Transferring joint statement" }
   set Msg(SetLead)       { "Définition du leader RSMC" "Defining lead RSMC" }
   set Msg(SuppressExp)   { "La suppression de l'expérience supprimera definitivement toutes les simulations\
                             qui y son contenue.\n\nVoulez-vous supprimer cette expérience ?" \
                            "Deleting this experiment will definitively erase all the simulations in it.\n\n\
                             Do you really want to delete this experiment ?" }
   set Msg(SuppressDone)  { "Suppression terminée !" \
                            "Suppressing done !" }
   set Msg(SuppressError) { "Impossible de supprimer les répertoires de l'expérience suivante:" \
                            "Unable to suppress the following experiment directory" }
   set Msg(Kill)          { "Arrêt de la simulation" "Terminating simulation" }

   set Error(SendJoint)   { "Il y a eu un problème pendant le transfert des message commun:" "There were problems while transferring joint statement:" }
}

#-------------------------------------------------------------------------------
# Nom      : <Exp::Id>
# Creation : October 2008 - J.P. Gauthier - CMC/CMOE
#
# But      : Construire un identificateur unique a partir de l'info de la simulation.
#
# Parametres :
#   <Info>   : Information des parametres de la simulation
#
# Retour:
#   <Id>    : Identificateur unique

# Remarques :
#
#-------------------------------------------------------------------------------

proc Exp::Id { Info } {
   regsub -all "\[^a-zA-Z0-9\]" $Info "" id
   return [string range $id 0 100]
}

#-------------------------------------------------------------------------------
# Nom      : <Exp::AllClose>
# Creation : Aout 2001 - J.P. Gauthier - CMC/CMOE
#
# But      : Fermer toutes les branches.
#
# Parametres :
#
# Retour:
#
# Remarques :
#
#-------------------------------------------------------------------------------

proc Exp::AllClose { } {
   variable Data

   set Data(Branch)    ""
   set Data(BranchSim) ""
   Exp::CreateTree
}

#-------------------------------------------------------------------------------
# Nom      : <Exp::AllOpen>
# Creation : Aout 2001 - J.P. Gauthier - CMC/CMOE
#
# But      : Ouvrir toutes les branches.
#
# Parametres :
#
# Retour:
#
# Remarques :
#
#-------------------------------------------------------------------------------

proc Exp::AllOpen { } {
   variable Data

   foreach exp $Data(List) {
      set no   [lindex $exp 0]
      foreach model $Data(Models) {
         lappend Data(BranchSim) $model$no
      }
      lappend Data(Branch) $no
   }

   Exp::CreateTree
}

#----------------------------------------------------------------------------
# Nom      : <Exp::Create>
# Creation : Aout 2001 1999 - J.P. Gauthier - CMC/CMOE
#
# But      : Affichage de l'arborescence des experiences.
#
# Parametres :
#   <Frame>       : Frame du widget
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc Exp::Create { Frame } {
   global GDefs
   variable Data
   variable Lbl
   variable Bubble

   set Data(Frame) $Frame

   #----- Arborescence des experiences

   panedwindow $Frame.info -orient vertical -showhandle False -opaqueresize True -bd 1
      frame $Frame.info.exp
         canvas $Frame.info.exp.canvas -bg white -relief sunken -bd 1 -yscrollcommand "$Frame.info.exp.scroll set" \
            -scrollregion "1 1 280 5000" -width 200 -height 1
         scrollbar $Frame.info.exp.scroll -orient vertical -bd 1 -width 10 -command "$Frame.info.exp.canvas yview"
         pack $Frame.info.exp.canvas -side left -fill both -expand true
         pack $Frame.info.exp.scroll -side left -fill y

      bind $Frame.info.exp.canvas <Button-4> "$Frame.info.exp.canvas yview scroll -1 units"
      bind $Frame.info.exp.canvas <Button-5> "$Frame.info.exp.canvas yview scroll 1 units"

      frame $Frame.info.proc
         text $Frame.info.proc.text -relief sunken -yscrollcommand "$Frame.info.proc.scroll set" \
            -width 1 -height 1 -bg white -highlightthickness 0 -bd 1 -wrap word
         scrollbar $Frame.info.proc.scroll -relief sunken -command "$Frame.info.proc.text yview" -highlightthickness 0 -bd 1 \
            -width 10
         pack $Frame.info.proc.text -side left -expand true -fill both
         pack $Frame.info.proc.scroll -side left -fill y

      $Frame.info add $Frame.info.exp -height 500
      $Frame.info add $Frame.info.proc -minsize 10

   frame $Frame.opt
      button $Frame.opt.open -image PLUS -relief flat -bd 0 -overrelief raised -command "Exp::AllOpen"
      button $Frame.opt.close -image MINUS -relief flat -bd 0 -overrelief raised -command "Exp::AllClose"
      checkbutton $Frame.opt.bubble -image INFO -relief sunken -bd 1 -overrelief raised -offrelief flat -indicatoron False  -selectcolor $GDefs(ColorLight) \
         -onvalue 1 -offvalue 0 -variable CanvasBubble::Data(State$Frame.info.exp.canvas) -command "CanvasBubble::Activate $Frame.info.exp.canvas"
      button $Frame.opt.writer -image DOCWRITE -relief flat -bd 0 -overrelief raised -command "Writer::Window"
      button $Frame.opt.bulletin -image CAUTION -relief flat -bd 0 -overrelief raised -command "Bulletin::Window"
      button $Frame.opt.new -compound left -image BOMB -text [lindex $Lbl(New) $GDefs(Lang)] -relief flat -bd 0 -overrelief raised \
         -command "Model::New $Frame Exp::New"
      pack $Frame.opt.open $Frame.opt.close $Frame.opt.bubble $Frame.opt.new  -side left -padx 2
      pack $Frame.opt.writer $Frame.opt.bulletin -side right -padx 2

   frame $Frame.path
      button $Frame.path.sel -image OPEN -relief flat -bd 0 -overrelief raised \
         -command { Exp::ReadPath [FileBox::Create . "" LoadPath "" ] }
      ComboBox::Create $Frame.path.list Exp::Param(Path) noedit unsorted \
         nodouble -1 $Exp::Param(Paths) 21 5 "Exp::Read; Model::TypeSelect none 1"

      pack $Frame.path.sel -side left -padx 2
      pack $Frame.path.list -side left -fill x -expand true

   pack $Frame.opt -side top -fill x -padx 2
   pack $Frame.path -side top -fill x -padx 2
   pack $Frame.info -side top -fill both -expand true -padx 2 -pady 2

   Exp::Procedure $Frame.info.proc.text

   Bubble::Create $Frame.path.sel     $Model::Bubble(PathAdd)
   Bubble::Create $Frame.path.list    $Model::Bubble(PathSel)

   Bubble::Create $Frame.opt.open     $Model::Bubble(Plus)
   Bubble::Create $Frame.opt.close    $Model::Bubble(Minus)
   Bubble::Create $Frame.opt.new      $Model::Bubble(New)
   Bubble::Create $Frame.opt.writer   $Writer::Param(Title)
   Bubble::Create $Frame.opt.bulletin $Bulletin::Param(Title)
   Bubble::Create $Frame.opt.bubble   $Model::Bubble(Bubble)
}

proc Exp::Procedure { Text  } {
   variable Param

   #----- Inclure le fichier texte si il existe

   if { [file exists $Param(Proc).txt] } {

      set f [open $Param(Proc).txt]
      while { [gets $f ligne] >= 0 } {
         $Text insert end "$ligne\n"
      }
      close $f
   }

   #----- Traiter les directives si elles existent

   if { [file exists $Param(Proc).dir] } {

      set f [open $Param(Proc).dir]
      while { [gets $f ligne] >= 0 } {
         Dialog::TextSearch $Text -1 [lindex $ligne 0] [lindex $ligne 1] [lindex $ligne 2]
      }
      close $f
   }
}

#-------------------------------------------------------------------------------
# Nom      : <Exp::CreateBranch>
# Creation : Aout 2001 - J.P. Gauthier - CMC/CMOE
#
# But      : Creer les branches des simulations.
#
# Parametres :
#   <Canvas> : Identificateur du Canvas
#   <Model>  : Type de modele
#   <Prev>   : No de la simulation precedente
#   <No>     : No de l'experience
#   <Name>   : Nom de l'experience
#   <Deep>   : Coordonnee de la profondeur de la branche en y
#   <Branch> : Cordonnee de la profondeur de la branche
#   <List>   : Liste des simulations
#   <Open>   : Is the branch openned
#
# Retour     :
#   <Deep>   : Coordonnee de la profondeur de la derniere branche en y
#
# Remarques :
#    Cet algorithme est recursif afin de creer les sous-branches.
#
#-------------------------------------------------------------------------------

proc Exp::CreateBranch { Canvas Model Prev No Name Deep Branch List { Open True } } {
   global GDefs
   variable Data

   set x [expr 20+$Branch*20]
   set branch [expr $Deep+10]

   #----- Construire les branches
   foreach sim $List {
      set Data(Desc) [Exp::PoolFormat ${Model} $sim]

      if { $Data(NoPrev)==$Prev } {

         #----- Creer le nom du widget (unique)
         set id [Exp::Id $sim]

         #----- Code de couleur
         switch -- $Data(State) {
            -1 { set fg #B6B6B6 ; set Data(StateExp) [expr $Data(StateExp)==2?$Data(StateExp):-1]} ;# Problems
            0  { set fg black }                          ;# Restartable
            1  { set fg black }                          ;# Done
            2  { set fg red; set Data(StateExp) 2 }      ;# Running
            3  { set fg darkgray }                       ;# Suspended
         }

         if { $Open } {
            incr Deep 20

            #----- Creer l'identificateur de l'experience
            $Canvas create text [expr $x+5] $Deep -text "$Data(Desc)" -anchor w -tags "SIM SIM$id" -fill $fg -font $GDefs(Font)
            $Canvas create line [expr $x-10] $Deep $x $Deep -width 1 -fill black -tags TREE

            $Canvas create line [expr $x-10] $Deep $x $Deep -width 1 -fill black -tags TREE
            $Canvas create line [expr $x-10] $branch [expr $x -10] $Deep -width 1 -fill black -tags TREE

            $Canvas bind SIM$id <ButtonPress-3> "set Exp::Data(No) $No; set Exp::Data(Name) $Name; set Exp::Data(Pos) {}; Exp::SelectSim \"$sim\" ; ${Model}::PopUp %X %Y"
            $Canvas bind SIM$id <ButtonPress-1> "set Exp::Data(No) $No; set Exp::Data(Name) $Name; set Exp::Data(Pos) {}; Exp::SelectSim \"$sim\""

            CanvasBubble::Create $Canvas SIM$id "[Info::Format $sim]"
         }

         #----- La branche est elle en execution
         if { [info exists Data(Job$id)] } {
            set Data(StateExp) 2

            if { $Open } {
               Exp::LaunchUpdate $id 0
            }
         }

         #----- Appel recursif des sous branches
         set Deep [Exp::CreateBranch $Canvas $Model $Data(NoSim) $No $Name $Deep [expr $Branch+1] $List $Open]
      }
   }
   return $Deep
}

#-------------------------------------------------------------------------------
# Nom      : <Exp::CreateTree>
# Creation : Aout 2001 - J.P. Gauthier - CMC/CMOE
#
# But      : Creer l'arborescence des experiences.
#
# Parametres :
#
# Retour:
#
# Remarques :
#
#-------------------------------------------------------------------------------

proc Exp::CreateTree { } {
   global GDefs
   variable Data
   variable Param

   if { $Data(ParsingTree) } {
      return
   }

   set Data(ParsingTree) 1
   set canvas $Data(Frame).info.exp.canvas
   set y 15

   #----- Cleanup du canvas
   $canvas delete TREE SIGN EXP SIM SIMSELECT EXEC
   $canvas create rectangle -10 -10 -10 -10 -outline black -fill $GDefs(ColorHighLight) -tags "SIMSELECT"

   foreach exp $Data(List) {

      set Data(StateExp) 0

      set no   [lindex $exp 0]
      set name [lindex $exp 1]
      set type [lindex $exp 2]
      set ico  [lindex $Model::Resources(Icos) $type]

      $canvas create bitmap 10 $y -bitmap $Model::Resources(Plus) -tags "SIGN PEXP$no"
      $canvas create image 30 $y -image $ico -tags "EXP IEXP$no"
      $canvas create text 43 $y -text "$no $name" -anchor w -tags "SIGN EXP EXP$no" -font $GDefs(Font) -fill black

      set str ""
      foreach loc [lindex $exp 3] {
         lappend str [lrange $loc 0 2]
      }
      CanvasBubble::Create $canvas EXP$no [join $str \n]
      $canvas bind EXP$no  <ButtonPress-3> "Exp::Select { $exp } ; Exp::PopUp %X %Y"
      $canvas bind EXP$no  <ButtonPress-1> "Exp::SelectBranch $no ; Exp::Select { $exp }"
      $canvas bind PEXP$no <ButtonPress-1> "Exp::SelectBranch $no"

      set y0 [set y1 [expr $y+10]]

      if { [lsearch -exact $Exp::Data(Branch) $no] != -1 } {

         #----- On creer les branches des modeles si necesaire
         $canvas itemconfigure PEXP$no -bitmap $Model::Resources(Minus)

         foreach model $Data(Models) {

            if { $model=="SATDATA" && [file exists $Param(Path)/${no}_${name}/SatData] } {
               set simlist SATDATA
            } else {
               set simlist [Info::List $Param(Path)/${no}_${name}/${model}.pool]
            }

            if { [llength $simlist] > 0 } {
               set y1 [incr y 21]

               $canvas create line 10 $y 20 $y -tags TREE
               $canvas create text 40 $y -text $model -font $GDefs(Font) -anchor w -tags "TREE TREE$model$no"
               $canvas create bitmap 30 $y -bitmap $Model::Resources(Plus) -tags "SIGN $model$no"
               $canvas bind $model$no      <ButtonPress-1> "Exp::SelectBranchSim $model$no"
               $canvas bind TREE$model$no <ButtonPress-1> "Exp::SelectBranchSim $model$no"

               if { [lsearch -exact $Exp::Data(BranchSim) $model$no] != -1 } {
                  $canvas itemconfigure $model$no -bitmap $Model::Resources(Minus)
                  set y [Exp::CreateBranch $canvas $model -1 $no $name $y 1 $simlist True]
               } else {
                  set y [Exp::CreateBranch $canvas $model -1 $no $name $y 1 $simlist False]
               }
            }
         }
         $canvas create line 10 $y0 10 $y1 -tags TREE
      } else {
         #----- Mais on parse quand meme pour verifier si il y a une execution en cours
         foreach model $Data(Models) {
            if { $model=="SATDATA" && [file exists $Param(Path)/${no}_${name}/SatData] } {
               set simlist SATDATA
            } else {
               set simlist [Info::List $Param(Path)/${no}_${name}/${model}.pool]
            }
            Exp::CreateBranch $canvas $model -1 $no $name $y 1 $simlist False
        }
      }
      incr y 21

      #----- Change icon on experiment state
      if { $Data(StateExp)==-1 } {
         set ico [lindex $Model::Resources(Bads) $type]
      }
      if { $Data(StateExp)==2 } {
         set ico [lindex $Model::Resources(Acts) $type]
      }
      $canvas itemconfigure IEXP$no -image $ico
      if { [info exists SPI::Ico(DefEXPERIMENT)] } {
         set idx 0
         foreach def $SPI::Ico(DefEXPERIMENT) {
            if { [string match "* $no:$name" [lindex $def 0]] } {
               lset SPI::Ico(DefEXPERIMENT) $idx end $ico
            }
            incr idx
         }
      }
   }

   Exp::SelectSim $Data(SelectSim)
   SPI::IcoDraw   $Page::Data(Frame)

   $canvas bind SIGN <Enter> "$canvas config -cursor hand1"
   $canvas bind SIGN <Leave> "$canvas config -cursor left_ptr"

   set Data(ParsingTree) 0
}

#----------------------------------------------------------------------------
# Nom        : <Exp::PoolFormat>
# Creation   : Octobre 2001 - J.P. Gauthier - CMC/CMOE
#
# But        : Recuperer l'information descriptive d'une ligne pool.
#
# Parametres :
#   <Model>  : Modele en cause
#   <Info>   : Ligne non modifiee du fichier pool
#
# Retour     :
#
# Remarques  :
#----------------------------------------------------------------------------

proc Exp::PoolFormat { Model Info } {
   variable Data

   if { $Model=="SATDATA" } {
      set Data(NoSim)  0
      set Data(NoPrev) -1
      set Data(State)  0
      return SATDATA
   } else {
      set Data(NoSim)  [Info::Strip $Info NoSim]
      set Data(NoPrev) [Info::Strip $Info NoPrev]
      set Data(State)  [Info::Strip $Info State]
      set Data(Delta)  [Info::Strip $Info Delta]

      if { [set dur [Info::Strip $Info Duration]]!="" } {
         set unit Hrs
      } else {
         set dur [Info::Strip $Info DurMin]
         set unit Min
      }
      set dt    ""
      set no    "[Info::Strip $Info NoSim]"
      set model "[Info::Strip $Info Meteo][Info::Strip $Info Delta][Info::Strip $Info Mode]"
      set date  "[Info::Strip $Info AccYear]-[Info::Strip $Info AccMonth]-[Info::Strip $Info AccDay] [Info::Strip $Info AccHour]:[Info::Strip $Info AccMin]"

      #----- Use click date if available
      catch { set date  [clock format [Info::Strip $Info Click] -format "%Y-%m-%d %H:%M" -gmt True] }

      #----- Set from to hours if available
      catch { set t0 [expr [Info::Strip $Info SimSecs]-[Info::Strip $Info Sim0Secs]]
              set t1 [expr $t0+$dur*3600]
              set dur [format "%02i-%02i" [expr $t0/3600] [expr $t1/3600]]
       }


      return "$dur $unit$dt $model $date ($no)"
   }
}

#-------------------------------------------------------------------------------
# Nom      : <Exp::ThreadUpdate>
# Creation : Octobre 2008 - J.P. Gauthier - CMC/CMOE
#
# But      : Surveiller l'evolution d'une thread
#
# Parametres :
#   <Id>     : Identificateur de la simulation
#   <Path>   : Path de l'experience
#   <Objs>   : Objets donnees resultants
#
# Retour:
#
# Remarques :
#
#-------------------------------------------------------------------------------

proc Exp::ThreadUpdate { Id Path { Objs { } } } {
   global   GDefs
   variable Data
   variable Msg

   if { [simulation is $Id] } {

      #----- If simulation is finished, destroy it
      if { [set pc [simulation define $Id -percent]]==100 } {
         Info::Set $Path [simulation define $Id -tag] 1
         Viewport::UnAssign $Page::Data(Frame) $Viewport::Data(VP) $Objs True
         Page::UpdateCommand $Page::Data(Frame)
         simulation destroy $Id
         trajectory free $Objs
      } else {
         #----- If we have associated data objects
         if { $pc<100 && [llength $Objs] } {
            Viewport::Assign $Page::Data(Frame) $Viewport::Data(VP) $Objs True
            Page::UpdateCommand $Page::Data(Frame)
         }

         #----- Recheck in 5 seconds
         after 5000 [list Exp::ThreadUpdate $Id $Path $Objs]
      }

      #----- Adjust percentage bar
      if { [winfo exists $Data(Frame).info.exp.canvas] } {

         $Data(Frame).info.exp.canvas delete EXEC$Id

         if { $pc<100 && [llength [set coords [$Data(Frame).info.exp.canvas bbox SIM$Id]]] } {
            eval $Data(Frame).info.exp.canvas create rectangle $coords -outline #FF9E9E -fill white -tags \"EXEC EXEC$Id\"
            lset coords 2 [expr [lindex $coords 0]+[expr double([lindex $coords 2] - [lindex $coords 0])*$pc/100.0]]
            eval $Data(Frame).info.exp.canvas create rectangle $coords -outline #FF9E9E -fill #FF9E9E -tags \"EXEC EXEC$Id\"
            $Data(Frame).info.exp.canvas raise EXEC$Id SIMSELECT
            $Data(Frame).info.exp.canvas raise SIM$Id EXEC$Id
         }
      }
   }
}

#-------------------------------------------------------------------------------
# Nom      : <Exp::ThreadKill>
# Creation : Octobre 2008 - J.P. Gauthier - CMC/CMOE
#
# But      : Supprimer l'execution d'une simulation dans une thread
#
# Parametres :
#   <Id>     : Identificateur de la simulation
#
# Retour:
#
# Remarques :
#
#-------------------------------------------------------------------------------

proc Exp::ThreadKill { Id } {
   global GDefs
   variable Msg
   variable Data

   if { [simulation is $Id] } {

      set ::Model::Param(Job) "[lindex $Msg(Kill) $GDefs(Lang)]"
      Dialog::Wait $Data(Frame) $Msg(Kill)
      update idletasks

      #----- Signal simulation to finish and wait for it to do so
      simulation define $Id -state DONE
      set t 0
      while { [incr t]<1e6 && [simulation define $Id -percent]!=100 } { }
      simulation destroy $Id

      set ::Model::Param(Job) ""
      Dialog::WaitDestroy

      #----- Suppression de la barre d'execution
      $Data(Frame).info.exp.canvas delete EXEC$Id
   }
}

#-------------------------------------------------------------------------------
# Nom      : <Exp::New>
# Creation : Aout 2001 - J.P. Gauthier - CMC/CMOE
#
# But      : Creer une nouvelle Experience dans la liste existante.
#
# Parametres:
#
# Retour:
#
# Remarques :
#
#-------------------------------------------------------------------------------

proc Exp::New { { Edit False } } {
   global GDefs
   variable Data
   variable Param

   #----- Verifier la validitee des parametres
   if { $Model::Data(Name)=="" } {
       Dialog::Error .expnew $Model::Error(Name)
       return 0
   }

   regsub -all "\[^a-zA-Z0-9\]" $Model::Data(Name) "_" Model::Data(Name)

   set Data(Name) $Model::Data(Name)
   set Data(Type) $Model::Data(Type)
   set info ""

   foreach src $Model::Data(Srcs) coords $Model::Data(Coords) id $Model::Data(Ids) {

      regsub -all "\[^a-zA-Z0-9\]" $src "_" src

      if { $src=="" } {
         Dialog::Error .expnew $Model::Error(Name)
         return 0
      }

      if { ![llength $coords] } {
         Dialog::Error .expnew $Model::Error(Coord) "\n\n\t$src\n"
         return 0
      }

      set dmcoords {}
      foreach { lat lon } $coords {

         #----- Forcer le format degree centiemme
         set lat [Convert::Minute2Decimal $lat 5]
         set lon [Convert::Minute2Decimal $lon 5]

         if { $lat<-90.0 || $lat>90.0 || $lon<-180 || $lon>360 } {
            Dialog::Error .expnew $Model::Error(Coord) "\n\n\t$src $lat $lon\n"
            return 0
         }
         lappend dmcoords $lat $lon
      }

      eval lappend info \[list $src $dmcoords $id\]
   }

   if { [llength $info]==0 } {
       Dialog::Error .expnew $Msg(Pos)
       return 0
   }

   #----- Relire les experiences
   Model::Check 0

   #----- Extraire les numero d'experience
   set listeno ""
   foreach exp $Data(List) {
      lappend listeno [lindex $exp 0]
   }

   #----- Rechercher un numero libre
   set ok 0
   set no 0

   while { $ok == 0 } {

      set noexp [Convert::Set2Digit $no]
      if { [lsearch -exact $listeno $noexp] < 0 } {
         set ok 1
      } else {
         incr no 1
      }
   }

   #----- Creer l'enregistrement de l'experience
   set line "$noexp $Data(Name) $Data(Type) { $info }"
   Log::Print INFO "Creating new Experiment ($line)"
   exec echo "$line" >> $Param(Path)/eer_ExpList

   file mkdir $Param(Path)/${noexp}_$Data(Name)
   file mkdir $Param(Path)/${noexp}_$Data(Name)/Output

   #----- dans le cas RSMC, initialiser les images
   if { $Data(Type)==1 } {

      Log::Print INFO "Copying RSMC joint data to directory $Param(Path)/${noexp}_$Data(Name)/Output/RSMCJoin"
      file mkdir $Param(Path)/${noexp}_$Data(Name)/Output/RSMCJoin
      file copy $GDefs(Dir)/data/na_RSMCProduct.gif $Param(Path)/${noexp}_$Data(Name)/Output/RSMCJoin/CVRCA.gif
      file copy $GDefs(Dir)/data/na_rsmc.ps         $Param(Path)/${noexp}_$Data(Name)/Output/RSMCJoin/CVRCA.ps
      file copy $GDefs(Dir)/data/na_RSMCProduct.gif $Param(Path)/${noexp}_$Data(Name)/Output/RSMCJoin/LICCA_01.gif
      file copy $GDefs(Dir)/data/na_RSMCProduct.gif $Param(Path)/${noexp}_$Data(Name)/Output/RSMCJoin/LICCA_02.gif
      file copy $GDefs(Dir)/data/na_RSMCProduct.gif $Param(Path)/${noexp}_$Data(Name)/Output/RSMCJoin/LICCA_03.gif
      file copy $GDefs(Dir)/data/na_RSMCProduct.gif $Param(Path)/${noexp}_$Data(Name)/Output/RSMCJoin/LICCA_04.gif
      file copy $GDefs(Dir)/data/na_RSMCProduct.gif $Param(Path)/${noexp}_$Data(Name)/Output/RSMCJoin/LTDCA_01.gif
      file copy $GDefs(Dir)/data/na_RSMCProduct.gif $Param(Path)/${noexp}_$Data(Name)/Output/RSMCJoin/LTDCA_02.gif
      file copy $GDefs(Dir)/data/na_RSMCProduct.gif $Param(Path)/${noexp}_$Data(Name)/Output/RSMCJoin/LTDCA_03.gif
      file copy $GDefs(Dir)/data/na_RSMCProduct.gif $Param(Path)/${noexp}_$Data(Name)/Output/RSMCJoin/LTDCA_04.gif
      file copy $GDefs(Dir)/data/na_RSMCProduct.gif $Param(Path)/${noexp}_$Data(Name)/Output/RSMCJoin/LTJCA.gif
      file copy $GDefs(Dir)/data/na_rsmc.ps         $Param(Path)/${noexp}_$Data(Name)/Output/RSMCJoin/LICCA_01.ps
      file copy $GDefs(Dir)/data/na_rsmc.ps         $Param(Path)/${noexp}_$Data(Name)/Output/RSMCJoin/LICCA_02.ps
      file copy $GDefs(Dir)/data/na_rsmc.ps         $Param(Path)/${noexp}_$Data(Name)/Output/RSMCJoin/LICCA_03.ps
      file copy $GDefs(Dir)/data/na_rsmc.ps         $Param(Path)/${noexp}_$Data(Name)/Output/RSMCJoin/LICCA_04.ps
      file copy $GDefs(Dir)/data/na_rsmc.ps         $Param(Path)/${noexp}_$Data(Name)/Output/RSMCJoin/LTDCA_01.ps
      file copy $GDefs(Dir)/data/na_rsmc.ps         $Param(Path)/${noexp}_$Data(Name)/Output/RSMCJoin/LTDCA_02.ps
      file copy $GDefs(Dir)/data/na_rsmc.ps         $Param(Path)/${noexp}_$Data(Name)/Output/RSMCJoin/LTDCA_03.ps
      file copy $GDefs(Dir)/data/na_rsmc.ps         $Param(Path)/${noexp}_$Data(Name)/Output/RSMCJoin/LTDCA_04.ps
      file copy $GDefs(Dir)/data/na_rsmc.ps         $Param(Path)/${noexp}_$Data(Name)/Output/RSMCJoin/LTJCA.ps
      file copy $GDefs(Dir)/data/na_RSMCProduct.gif $Param(Path)/${noexp}_$Data(Name)/Output/RSMCJoin/SICCA_01.gif
      file copy $GDefs(Dir)/data/na_RSMCProduct.gif $Param(Path)/${noexp}_$Data(Name)/Output/RSMCJoin/SICCA_02.gif
      file copy $GDefs(Dir)/data/na_RSMCProduct.gif $Param(Path)/${noexp}_$Data(Name)/Output/RSMCJoin/SICCA_03.gif
      file copy $GDefs(Dir)/data/na_RSMCProduct.gif $Param(Path)/${noexp}_$Data(Name)/Output/RSMCJoin/SICCA_04.gif
      file copy $GDefs(Dir)/data/na_RSMCProduct.gif $Param(Path)/${noexp}_$Data(Name)/Output/RSMCJoin/STDCA_01.gif
      file copy $GDefs(Dir)/data/na_RSMCProduct.gif $Param(Path)/${noexp}_$Data(Name)/Output/RSMCJoin/STDCA_02.gif
      file copy $GDefs(Dir)/data/na_RSMCProduct.gif $Param(Path)/${noexp}_$Data(Name)/Output/RSMCJoin/STDCA_03.gif
      file copy $GDefs(Dir)/data/na_RSMCProduct.gif $Param(Path)/${noexp}_$Data(Name)/Output/RSMCJoin/STDCA_04.gif
      file copy $GDefs(Dir)/data/na_RSMCProduct.gif $Param(Path)/${noexp}_$Data(Name)/Output/RSMCJoin/STJCA.gif

      exec echo "Unavailable" >> $Param(Path)/${noexp}_$Data(Name)/Output/RSMCJoin/RUN.txt
      exec echo "Unavailable" >> $Param(Path)/${noexp}_$Data(Name)/Output/RSMCJoin/traject.points
      exec echo "01 02 03 04" > $Param(Path)/${noexp}_$Data(Name)/Output/RSMCJoin/IP2List.txt
  }

   exec chmod -R 755 $Param(Path)/${noexp}_$Data(Name)

   Model::Check 0
   Model::TypeSelect none 1
   return 1
}

#-------------------------------------------------------------------------------
# Nom      : <Exp::Path>
# Creation : Aout 2001 - J.P. Gauthier - CMC/CMOE
#
# But      : Assembler le path de l'experience.
#
# Parametres:
#
# Retour:
#
# Remarques :
#
#-------------------------------------------------------------------------------

proc Exp::Path { } {
   global   GDefs
   variable Data
   variable Param

   return $Param(Path)/$Data(No)_$Data(Name)
}

#----------------------------------------------------------------------------
# Nom      : <Exp::PopUp>
# Creation : Aout 2001 - J.P. Gauthier - CMC/CMOE
#
# But      : Afficher le popup contextuel des experiences.
#
# Parametres    :
#    <X>        : ...
#    <Y>        : ...
#
# Retour :
#
# Remarques :
#
#----------------------------------------------------------------------------

proc Exp::PopUp { X Y } {
   global   GDefs
   variable Data
   variable Lbl

   if { ![winfo exists .exppop] } {

      menu .exppop -tearoff 0 -bd 1 -type normal -activeborderwidth 1
         .exppop add command -label ""  -command "Model::TypeSelect none 1 \${Exp::Data(No)}_\${Exp::Data(Name)}; Exp::CreateTree; SPI::Locate \$Exp::Data(Lat) \$Exp::Data(Lon)" \
             -background $GDefs(ColorHighLight) -activebackground $GDefs(ColorHighLight)
         .exppop add cascade -label [lindex $Lbl(New) $GDefs(Lang)] -menu .exppop.new
         .exppop add separator
         .exppop add cascade -label [lindex $Lbl(Product) $GDefs(Lang)] -menu .exppop.product -state disabled
         .exppop add separator
         .exppop add command -label [lindex $Lbl(Suppress) $GDefs(Lang)] -command "Exp::Suppress"
         .exppop add command -label [lindex $Lbl(Store) $GDefs(Lang)] -command { Exp::Store [Dialog::Get .model $Exp::Lbl(Store) $Exp::Msg(Store) Exp::Data(StoreID) True] }

      #----- Menu des modeles
      menu  .exppop.new -tearoff 0 -bd 1 -type normal -activeborderwidth 1
      foreach model $Data(Models) {
         if { $model=="SATDATA" } {
            .exppop.new add command -label $model -command "SATDATA::New .exp"
         } else {
            .exppop.new add command -label $model -command "Model::ParamsWindow $model NEW"
         }
      }

      menu  .exppop.product -tearoff 0 -bd 1 -type normal -activeborderwidth 1
         .exppop.product add command -label [lindex $Lbl(RSMCFax) $GDefs(Lang)] -command  { Exp::ProductRSMCFax }
         .exppop.product add command -label [lindex $Lbl(RSMCJointData) $GDefs(Lang)] -command  {Exp::ProductRSMCJointData}
         .exppop.product add command -label [lindex $Lbl(RSMCJointStatement) $GDefs(Lang)] -command { Exp::ProductRSMCJointStatement [FileBox::Create . "" Load ""] }
         .exppop.product add separator
         .exppop.product add command -label [lindex $Lbl(RSMCBlank) $GDefs(Lang)] -command { Exp::ProductRSMCBlank }
   }

   #----- Dans le cas RSMC, activer la transmision de produits
   if { $Data(Type) == 1 } {
      .exppop entryconfigure 3 -state normal
   } else {
      .exppop entryconfigure 3 -state disabled
   }

   .exppop entryconfigure 0 -label "$Data(Name)"
   tk_popup .exppop $X $Y 0
}

#----------------------------------------------------------------------------
# Nom      : <Exp::BlankTransmit>
# Creation : Avril 2011 - S. Trudel - CMC/CMOE
#
# But      : Transfert les pages vides RSMC.
#
# Parametres :
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc Exp::BlankTransmit { } {
   global GDefs env
   variable Print
   variable Sim
   variable Error
   variable Data
   variable Param

   $Page::Data(Frame).page.canvas config -cursor watch
   update idletasks

   #----- On considere les produits de chacun des centres.
   #         Data(RSMC_CN) (RSMC_UK) Data(RSMC_AU) Data(RSMC_CA) Data(RSMC_RU) Data(RSMC_JP) Data(RSMC_FR) Data(RSMC_US)

   set rsmc_arg ""

   if { $Exp::Data(RSMC_AU)==1 } {
      set rsmc_arg "${rsmc_arg}AU "
   }

   if { $Exp::Data(RSMC_CA)==1 } {
      set rsmc_arg "${rsmc_arg}CA "
   }

   if { $Exp::Data(RSMC_CN)==1 } {
      set rsmc_arg "${rsmc_arg}CN "
   }

   if { $Exp::Data(RSMC_FR)==1 } {
      set rsmc_arg "${rsmc_arg}FR "
   }

   if { $Exp::Data(RSMC_JP)==1 } {
      set rsmc_arg "${rsmc_arg}JP "
   }

   if { $Exp::Data(RSMC_RU)==1 } {
      set rsmc_arg "${rsmc_arg}RU "
   }

   if { $Exp::Data(RSMC_UK)==1 } {
      set rsmc_arg "${rsmc_arg}UK "
   }

   if { $Exp::Data(RSMC_US)==1 } {
      set rsmc_arg "${rsmc_arg}US "
   }

   #----- On considere les joint statements, le lead ainsi que la meteo CMC.
   #         Data(JntStat34) Data(JntStat5) Data(JntStat16) Data(JntStat2)
   #         Data(RSMCLead)
   #         Data(RSMCMeteo)

   if { $Exp::Data(JntStat34)==1 } {
      set rsmc_arg "${rsmc_arg}jntreg34.html "
   }

   if { $Exp::Data(JntStat5)==1 } {
      set rsmc_arg "${rsmc_arg}jntreg5.html "
   }

   if { $Exp::Data(JntStat16)==1 } {
      set rsmc_arg "${rsmc_arg}jntreg16.html "
   }

   if { $Exp::Data(JntStat2)==1 } {
      set rsmc_arg "${rsmc_arg}jntreg2.html "
   }

   if { $Exp::Data(RSMCLead)!=99 } {
      set rsmc_arg "${rsmc_arg}leadrsmc$Exp::Data(RSMCLead).txt "
   }

   if { $Exp::Data(RSMCMeteo)==1 } {
      set rsmc_arg "${rsmc_arg}meteo "
   }

   puts stderr "set ErrCatch $env(EER_DIRSCRIPT)/RSMCTransferBlank.sh ... ${rsmc_arg}."

   #----- setup le repertoire et le fichier concernant le joint statement.
   set path "$Param(Path)/$Data(No)_$Data(Name)/Output/RSMCJoin"

   set tokenarchiversmc [clock format [clock seconds] -format "%Y%m%d.%H%M%S" -gmt true]

   if { $rsmc_arg!="" } {

      #----- effacer les produits du RSMC Montreal sur notre site web.

      set err [catch { exec ssh $GDefs(FrontEnd) -x -l $GDefs(FrontEndUser) $env(EER_DIRSCRIPT)/RSMCTransferBlank.sh $GDefs(Dir)/Data ${path} ${rsmc_arg} 2>@1 } msg]
      if { $err } {
         Log::Print ERROR "Problem to copy the blank products for ( $rsmc_arg ) on the RSMC commun web page.\n\n$msg"
      }

      #----- effacer les produits du RSMC Montreal sur les sites mirroirs.

      if { $Exp::Data(RSMC_CA)==1 } {
         set err [catch  { exec ssh $GDefs(FrontEnd) -x -l $GDefs(FrontEndUser) $env(EER_DIRSCRIPT)/RSMCTransferProducts.sh $path 3 $tokenarchiversmc 2>@1 } msg]
         if { $err } {
            Log::Print ERROR "Problem to delete RSMC Montreal products on the mirror RSMC web pages.\n\n$msg"
         }
      }

      #----- effacer le joint statement jntreg34.html sur les sites mirroirs.

      if { $Exp::Data(JntStat34)==1 } {
         set err [catch  { exec ssh $GDefs(FrontEnd) -x -l $GDefs(FrontEndUser) $env(EER_DIRSCRIPT)/RSMCTransferJoint.sh $GDefs(Dir)/data/na_jntreg34.html jntreg34.html $path $tokenarchiversmc 2>@1 } msg]
         if { $err } {
            Log::Print ERROR "Problem to delete the joint statement jntreg34.html on the mirror RSMC web pages.\n\n$msg"
         }
      }

      #----- imposer le lead sur tout les sites mirroirs.

      if { $Exp::Data(RSMCLead)!=99 } {
         set err [catch  { exec ssh $GDefs(FrontEnd) -x -l $GDefs(FrontEndUser) $env(EER_DIRSCRIPT)/RSMCTransferJoint.sh $GDefs(Dir)/data/leadrsmc$Exp::Data(RSMCLead).txt leadrsmc.txt $path $tokenarchiversmc 2>@1 } msg]
         if { $err } {
            Log::Print ERROR "Problem to copy the lead ( $Exp::Data(RSMCLead) ) on the mirror RSMC web pages.\n\n$msg"
         }
      }
   }

   $Page::Data(Frame).page.canvas config -cursor left_ptr
}

#----------------------------------------------------------------------------
# Nom      : <Exp::ProductRSMCBlank>
# Creation : Avril 2011 - S. Trudel - CMC/CMOE
#
# But      : Selection des parametres pour les produits vides RSMC.
#
# Parametres :
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc Exp::ProductRSMCBlank { } {
   global GDefs
   variable Lbl
   variable Data

   toplevel     .blank

   wm title     .blank [lindex $Lbl(TransmitBlank) $GDefs(Lang)]
   wm transient .blank .
   wm resizable .blank 0 0
   wm geometry  .blank =320x460+[expr [winfo rootx .]+10]+[expr [winfo rooty .]+10]
   grab .blank

   #----- on selectionne les centres RSMC.

   labelframe .blank.c -text [lindex $Lbl(RSMCWeb) $GDefs(Lang)]

      frame .blank.c.l -relief sunken -bd 1
         checkbutton .blank.c.l.cn -variable Exp::Data(RSMC_CN) -text "Beijing"    -onvalue 1 -offvalue 0 -indicatoron true -bd 1 -relief raised -overrelief groove
         checkbutton .blank.c.l.uk -variable Exp::Data(RSMC_UK) -text "Exeter"     -onvalue 1 -offvalue 0 -indicatoron true -bd 1 -relief raised -overrelief groove
         checkbutton .blank.c.l.au -variable Exp::Data(RSMC_AU) -text "Melbourne"  -onvalue 1 -offvalue 0 -indicatoron true -bd 1 -relief raised -overrelief groove
         checkbutton .blank.c.l.ca -variable Exp::Data(RSMC_CA) -text "Montreal"   -onvalue 1 -offvalue 0 -indicatoron true -bd 1 -relief raised -overrelief groove -background  #FF0000 -activebackground #FF0000
         checkbutton .blank.c.l.ru -variable Exp::Data(RSMC_RU) -text "Obninsk"    -onvalue 1 -offvalue 0 -indicatoron true -bd 1 -relief raised -overrelief groove
         checkbutton .blank.c.l.jp -variable Exp::Data(RSMC_JP) -text "Tokyo"      -onvalue 1 -offvalue 0 -indicatoron true -bd 1 -relief raised -overrelief groove
         checkbutton .blank.c.l.fr -variable Exp::Data(RSMC_FR) -text "Toulouse"   -onvalue 1 -offvalue 0 -indicatoron true -bd 1 -relief raised -overrelief groove
         checkbutton .blank.c.l.us -variable Exp::Data(RSMC_US) -text "Washington" -onvalue 1 -offvalue 0 -indicatoron true -bd 1 -relief raised -overrelief groove
         pack .blank.c.l.cn .blank.c.l.uk .blank.c.l.au .blank.c.l.ca .blank.c.l.ru .blank.c.l.jp .blank.c.l.fr .blank.c.l.us \
            -side top -fill x -ipady 2
      pack .blank.c.l -side top -fill x -padx 5 -pady 5
   pack .blank.c -side top -fill x -padx 5 -pady 5

   #----- on selectionne les joint statements.

   labelframe .blank.p -text "Joint Statement"
      frame .blank.p.l -relief sunken -bd 1
         checkbutton .blank.p.l.pjnt16 -variable Exp::Data(JntStat16) -text "Region I/VI"   -onvalue 1 -offvalue 0 -indicatoron true -bd 1 -relief raised -overrelief groove
         checkbutton .blank.p.l.pjnt2  -variable Exp::Data(JntStat2)  -text "Region II"     -onvalue 1 -offvalue 0 -indicatoron true -bd 1 -relief raised -overrelief groove
         checkbutton .blank.p.l.pjnt34 -variable Exp::Data(JntStat34) -text "Region III/IV" -onvalue 1 -offvalue 0 -indicatoron true -bd 1 -relief raised -overrelief groove -background #FF0000 -activebackground #FF0000
         checkbutton .blank.p.l.pjnt5  -variable Exp::Data(JntStat5)  -text "Region V"      -onvalue 1 -offvalue 0 -indicatoron true -bd 1 -relief raised -overrelief groove
         pack .blank.p.l.pjnt34 .blank.p.l.pjnt5 .blank.p.l.pjnt16 .blank.p.l.pjnt2 -side top -fill x -ipady 2

      pack .blank.p.l -side top -fill x -padx 5 -pady 5
   pack .blank.p -side top -padx 5 -fill both -expand true

   #----- on selectionne le lead.

   labelframe .blank.l -text [lindex $Lbl(RSMCLead) $GDefs(Lang)]
      frame .blank.l.r -relief sunken -bd 1
         radiobutton .blank.l.r.lead99 -variable Exp::Data(RSMCLead) -text "n.a."   -value 99 -indicatoron true -bd 1 -relief raised -overrelief raised -background #FF0000 -activebackground #FF0000
         radiobutton .blank.l.r.lead16 -variable Exp::Data(RSMCLead) -text "I/VI"   -value 16 -indicatoron true -bd 1 -relief raised -overrelief raised -background #FF0000 -activebackground #FF0000
         radiobutton .blank.l.r.lead2  -variable Exp::Data(RSMCLead) -text "II"     -value 2  -indicatoron true -bd 1 -relief raised -overrelief raised -background #FF0000 -activebackground #FF0000
         radiobutton .blank.l.r.lead34 -variable Exp::Data(RSMCLead) -text "III/IV" -value 34 -indicatoron true -bd 1 -relief raised -overrelief raised -background #FF0000 -activebackground #FF0000
         radiobutton .blank.l.r.lead5  -variable Exp::Data(RSMCLead) -text "V "     -value 5  -indicatoron true -bd 1 -relief raised -overrelief raised -background #FF0000 -activebackground #FF0000
         radiobutton .blank.l.r.lead0  -variable Exp::Data(RSMCLead) -text [lindex $Lbl(RSMCLeadNull) $GDefs(Lang)] -value 0  -indicatoron true -bd 1 -relief raised -overrelief raised -background #FF0000 -activebackground #FF0000
         .blank.l.r.lead99 select

         pack .blank.l.r.lead99 .blank.l.r.lead16 .blank.l.r.lead2 .blank.l.r.lead34 .blank.l.r.lead5 .blank.l.r.lead0 -side left -ipady 2

      pack .blank.l.r -side top -fill x -padx 5 -pady 5
   pack .blank.l -side top -padx 5 -fill both -expand true

   #----- on selectionne la meteo du CMC.

   labelframe .blank.m -text [lindex $Lbl(RSMCMeteo) $GDefs(Lang)]
      frame .blank.m.l -relief sunken -bd 1
         checkbutton .blank.m.l.lead -variable Exp::Data(RSMCMeteo) -text "Montreal" -onvalue 1 -offvalue 0 -indicatoron true -bd 1 -relief raised -overrelief groove
         pack .blank.m.l.lead -side top -fill x -ipady 2

      pack .blank.m.l -side top -fill x -padx 5 -pady 5
   pack .blank.m -side top -padx 5 -fill both -expand true

   frame .blank.command
      button .blank.command.cancel -text [lindex $Lbl(Cancel) $GDefs(Lang)] -bd 1 -command "destroy .blank"
      button .blank.command.send -text [lindex $Lbl(Send) $GDefs(Lang)] -bd 1 -command "destroy .blank ; Exp::BlankTransmit"
      pack .blank.command.cancel .blank.command.send -side left -fill x -expand true
   pack .blank.command -side top -padx 5 -pady 5 -fill x

   Bubble::Create .blank.c.l $Exp::Bubble(RSMCHelp1)
   Bubble::Create .blank.p.l $Exp::Bubble(RSMCHelp2)
   Bubble::Create .blank.l.r $Exp::Bubble(RSMCHelp3)
   Bubble::Create .blank.m.l $Exp::Bubble(RSMCHelp4)
}

#----------------------------------------------------------------------------
# Nom      : <Exp::ProductRSMCFax>
# Creation : Juillet 2001 - J.P. Gauthier - CMC/CMOE
#
# But      : Generer la FAX standard.
#
# Parametres :
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc Exp::ProductRSMCFax { } {
   global GDefs
   variable Data
   variable Param
   variable Msg

   . config -cursor watch
   update idletasks

   set path $Param(Path)/$Data(No)_$Data(Name)/Output/RSMCJoin

   Dialog::Wait . $Msg(Fax) "\n\n\t$path/rsmc.ps"

   set nbre [lindex [exec wc -w $path/IP2List.txt] 0]

   set LIC ""
   set LTD ""

   for { set no 1 } { $no <= $nbre } { incr no } {
      set LIC "$LIC $path/LICCA_0${no}.ps"
      set LTD "$LTD $path/LTDCA_0${no}.ps"
   }

   eval exec cat $path/CVRCA.ps $path/LTJCA.ps $LIC $LTD > $path/rsmc.ps

   exec chmod 644 $path/rsmc.ps

   Dialog::WaitDestroy
   . config -cursor left_ptr
}

#----------------------------------------------------------------------------
# Nom      : <Exp::ProductRSMCJointData>
# Creation : Avril 2000 - J.P. Gauthier - CMC/CMOE
#
# But      : Transfer vers le site RSMC commun.
#
# Parametres :
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc Exp::ProductRSMCJointData { } {
   global GDefs env
   variable Data
   variable Param
   variable Msg
   variable Lbl

   . config -cursor watch
   update idletasks

   if { [Dialog::Default . 400 WARNING $Msg(JointData) "" 0 $Lbl(Yes) $Lbl(No)] } {
      return
   }

   #----- setup le repertoire et le fichier concernant la run du modele meteo utilise.
   set path "$Param(Path)/$Data(No)_$Data(Name)/Output/RSMCJoin"
   set run [exec cat $path/RUN.txt]

   #----- on recupere l'index de la region afin de determiner la region RSMC correspondante.
   set region [expr [ogrlayer pick RSMC "$Exp::Data(Lat) $Exp::Data(Lon) "] + 1]

   Dialog::Wait . $Msg(SetLead)

   set tokenarchiversmc [clock format [clock seconds] -format "%Y%m%d.%H%M%S" -gmt true]

   if { $region == "3" || $region == "4" } {
      exec echo "34" > $path/leadrsmc.txt
      set err [catch { exec ssh $GDefs(FrontEnd) -x -l $GDefs(FrontEndUser) $env(EER_DIRSCRIPT)/RSMCTransferJoint.sh $path/leadrsmc.txt leadrsmc.txt $path $tokenarchiversmc 2>@1 } msg]
      if { $err != 0 } {
         Log::Print ERROR "Problem sending RSMC joint statement on the mirror RSMC web pages.\n\n$msg"
      }
   } else {
      file delete -force $path/leadrsmc.txt
   }

   Dialog::Wait . $Msg(SendProducts)

   #----- cree le fichier pour la date.
   if { $run == "Unavailable" } {
      exec echo "Unavailable" > $path/CA_DATE.TXT
   } else {
      exec echo [clock format [clock seconds] -format "%Y%m%d${run}_%H%M" -gmt true] > $path/CA_DATE.TXT
   }
   set nbip2 [lindex [exec wc -w  $path/IP2List.txt] 0]

   set err [catch  { exec ssh $GDefs(FrontEnd) -x -l $GDefs(FrontEndUser) $env(EER_DIRSCRIPT)/RSMCTransferProducts.sh $path $nbip2 $tokenarchiversmc 2>@1 } msg]
   if { $err } {
      Log::Print ERROR "Problem sending RSMC Montreal products on the mirror RSMC web pages.\n\n$msg"
   }

   Dialog::WaitDestroy
   . config -cursor left_ptr
}

#----------------------------------------------------------------------------
# Nom      : <Exp::ProductRSMCJointStatement>
# Creation : Avril 2007 - J.P. Gauthier - CMC/CMOE
#
# But      : Transfer le joint statement vers le site RSMC commun.
#
# Parametres :
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc Exp::ProductRSMCJointStatement { File } {
   global GDefs env
   variable Data
   variable Param
   variable Msg
   variable Lbl
   variable Error

   if { $File=="" } {
      return
   }

   . config -cursor watch
   update idletasks

   #----- setup le repertoire et le fichier concernant le joint statement.
   set path "$Param(Path)/$Data(No)_$Data(Name)/Output/RSMCJoin"
   file copy -force $File $path/joint_statement_b.html

   if { [Dialog::Default . 400 WARNING $Msg(JointStatement) "" 0 $Lbl(Yes) $Lbl(No)] } {
      return
   }

   set tokenarchiversmc [clock format [clock seconds] -format "%Y%m%d.%H%M%S" -gmt true]

   Dialog::Wait . $Msg(SendJoint)
   set err [catch { exec ssh $GDefs(FrontEnd) -x -l $GDefs(FrontEndUser) $env(EER_DIRSCRIPT)/RSMCTransferJoint.sh $path/joint_statement_b.html jntreg34.html $path $tokenarchiversmc 2>@1 } msg]
   Dialog::WaitDestroy

   if { $err } {
      Dialog::Error . $Error(SendJoint) $msg
   }
   . config -cursor left_ptr
}

#---------------------------------------------------------------------------
# Nom      : <Exp::Read>
# Creation : Aout 2001 - J.P. Gauthier - CMC/CMOE
#
# But      : Lire la liste des experiences.
#
# Parametres :
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc Exp::Read { } {
   variable Data
   variable Param

   set Data(List) ""

   if { [file exist $Param(Path)/eer_ExpList] } {

      set file [open $Param(Path)/eer_ExpList]
      while { ![eof $file] } {
         gets $file line
         if { $line != "" } {
            lappend Data(List) $line
         }
      }
      close $file
      set Data(List) [lsort -dictionary -index 1 $Data(List)]
   }
}

#---------------------------------------------------------------------------
# Nom      : <Exp::ReadPath>
# Creation : Octobre 2006 - J.P. Gauthier - CMC/CMOE
#
# But      : Ajoute un path a la liste des listde d'experiences.
#
# Parametres :
#   <Path>   : Path a ajouter
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc Exp::ReadPath { Path } {
   global GDefs
   variable Data
   variable Param
   variable Msg

   if { $Path!="" } {
      if { [file exists $Path/eer_ExpList] } {
         Exp::AllClose

         ComboBox::Add $Data(Frame).path.list $Path

         set Param(Path) $Path
         Exp::Read
         Exp::CreateTree
      } else {
         Dialog::Error . $Msg(Path)
      }
   }
}

#---------------------------------------------------------------------------
# Nom      : <Exp::Select>
# Creation : Aout 2001 - J.P. Gauthier - CMC/CMOE
#
# But      : Selection d'une experience.
#
# Parametres :
#   <Pool>   : Descripteur de la simulation
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc Exp::Select { Pool } {
   variable Data

   set Data(No)   [lindex $Pool 0]
   set Data(Name) [lindex $Pool 1]
   set Data(Type) [lindex $Pool 2]

   set Data(Pos)  [lindex $Pool 3]
   set Data(Lat)  [lindex [lindex $Data(Pos) 0] 1]
   set Data(Lon)  [lindex [lindex $Data(Pos) 0] 2]

   #----- Selectionner la nouvelle experience
   set Data(Select) $Pool

   set Data(StoreID) [clock format [clock seconds] -format "%Y%m%d" -gmt True]_$Data(Name)
}

#-------------------------------------------------------------------------------
# Nom      : <Exp::SelectBranch>
# Creation : Aout 2001 - J.P. Gauthier - CMC/CMOE
#
# But      : Effectuer la selection de la branche.
#
# Parametres :
#   <No>     : Numero de la branche.
#
# Retour:
#
# Remarques :
#
#-------------------------------------------------------------------------------

proc Exp::SelectBranch { No } {
   variable Data

   set idx [lsearch -exact $Data(Branch) $No]

   if { $idx == -1 } {
      lappend Data(Branch) $No
   } else {
      set Data(Branch) [lreplace $Data(Branch) $idx $idx]
   }
   Exp::CreateTree
}

#-------------------------------------------------------------------------------
# Nom      : <Exp::SelectBranchSim>
# Creation : Aout 2001 - J.P. Gauthier - CMC/CMOE
#
# But      : Effectuer la selection de la branche de simulation.
#
# Parametres :
#   <Tag>    : Descripteur de la simulation.
#
# Retour:
#
# Remarques :
#
#-------------------------------------------------------------------------------

proc Exp::SelectBranchSim { Tag } {
   variable Data

   set idx [lsearch -exact $Data(BranchSim) $Tag]

   if { $idx == -1 } {
      lappend Data(BranchSim) $Tag
   } else {
      set Data(BranchSim) [lreplace $Data(BranchSim) $idx $idx]
   }

   Exp::CreateTree
}

#---------------------------------------------------------------------------
# Nom      : <Exp::SelectSim>
# Creation : Aout 2001 - J.P. Gauthier - CMC/CMOE
#
# But      : Selection d'une simulation.
#
# Parametres :
#   <Info>   : Descripteur de la simulation
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc Exp::SelectSim { Info } {
   variable Data

   if { $Info!="" } {

      #----- Selectionner la nouvelle simulation
      set id [Exp::Id $Info]
      eval $Data(Frame).info.exp.canvas coords SIMSELECT [$Data(Frame).info.exp.canvas bbox SIM$id]

      set Data(SelectSim) $Info
   }
}

#-------------------------------------------------------------------------------
# Nom      : <Exp::Store>
# Creation : Septembre 2009 - J.P. Gauthier - CMC/CMOE
#
# But      : Archiver une experience.
#
# Parametres:
#   <Id>    : Identificateur du rapport d'incident associe
#
# Retour:
#
# Remarques :
#-------------------------------------------------------------------------------

proc Exp::Store { Id } {
   global GDefs
   variable Param
   variable Lbl
   variable Msg

   set code  True
   set cpath [pwd]
   set Id    [file rootname [file tail $Id]]

   if { $Id!="" } {

      #----- Check if an archive already exists
      set err [catch { exec ssh $Param(StoreHost) ls -1 $Param(StorePath)/$Id.cmc 2>@1 } msg]
      if { !$err } {
         if { [Dialog::Default .model 400 WARNING $Msg(StoreExist) "" 0 $Lbl(Yes) $Lbl(No)] } {
            return False
         }
      }

      Dialog::Wait .model $Msg(DoingStore)

      #----- Build the archive
      cd  [set path [Exp::Path]]/../

      set arch $Id.cmcarc
#      set arch $Id.tgz
      file delete -force /tmp/$arch
      set err [catch { exec cmcarc -a [file tail $path] -f /tmp/$arch --md5 --dereference >$Param(StoreLog) } msg]
#      set err [catch { exec tar -zcvf /tmp/$arch [file tail $path] 2>@1 } msg]
      if { $err } {
         Dialog::Error .model [list $msg $msg]
         set code False
      } else {
         #----- Copy it to archive host
         Dialog::Wait .model  $Msg(DoingCopy)
         set err [catch { exec srcp /tmp/$arch $Param(StoreHost):$Param(StorePath)/$arch 2>@1 } msg]
         if { $err } {
            Dialog::Error .model [list $msg $msg]
            set code False
         } else {
            #----- Remove local copy
            file delete -force /tmp/$arch

            Dialog::Info .model $Msg(Stored)
         }
      }

      Dialog::WaitDestroy
      cd $cpath
   }
   return $code
}

#-------------------------------------------------------------------------------
# Nom      : <Exp::Suppress>
# Creation : Aout 2001 - J.P. Gauthier - CMC/CMOE
#
# But      : Supprimer une experience ainsi que toutes les simulations qu'elle
#            contient.
#
# Parametres:
#
# Retour:
#
# Remarques :
#
#-------------------------------------------------------------------------------

proc Exp::Suppress { } {
   global GDefs
   variable Data
   variable Param
   variable Lbl
   variable Msg

   #----- Verifier la validitee des parametres
   if { [Dialog::Default . 400 WARNING $Msg(SuppressExp) "\n\n\t($Data(No)) $Data(Name)" 0 $Lbl(Yes) $Lbl(No)] } {
      return
   }

   . config -cursor watch

   #----- Supprimer le repertoire de l'experience
   file delete -force $Param(Path)/$Data(No)_$Data(Name)

   if { [file exists $Param(Path)/$Data(No)_$Data(Name)] } {
      Dialog::Error .  $Msg(SuppressError) "\n\n\t$Param(Path)/$Data(No)_$Data(Name)"
      Log::Print ERROR "Unable to suppress experiment: $Data(No) $Data(Name)"
   } else {

      Log::Print INFO "Suppressing experiment $Data(No) $Data(Name)"
      #----- Supprimer l'information de cette experience dans le pool
      file copy -force $Param(Path)/eer_ExpList $Param(Path)/eer_ExpList.old

      set fi [open $Param(Path)/eer_ExpList.old r]
      set fo [open $Param(Path)/eer_ExpList w]

      while { ![eof $fi] } {
         gets $fi line
         if { $line!="" && [lindex $line 0]!=$Data(No) } {
            puts $fo $line
         }
      }
      close $fi
      close $fo
   }

   #----- Relire les experiences
   Model::Check 0
   Model::TypeSelect none 1
  . config -cursor left_ptr
}

