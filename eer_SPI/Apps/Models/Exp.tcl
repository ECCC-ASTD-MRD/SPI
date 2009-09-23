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

   set Param(Paths)     [list $GDefs(DirData)]              ;#Liste des fichier d'experiences
   set Param(Path)      [lindex $Param(Paths) 0]            ;#Fichier d'experiences courant
   set Param(StoreHost) goodenough
   set Param(StorePath) /tmp/ArchiveTest
   set Param(Proc)      $GDefs(Dir)/Apps/Models/procedures_0

   #----- Variables relatives aux experiences

   set Data(Models)    "SATDATA CANERM TRAJECT MLDP0 MLDP1 MLCD"  ;#Liste des modeles disponibles

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

   #----- Labels

   set Lbl(New)                { "Nouveau" "New" }
   set Lbl(Product)            { "Produits" "Products" }
   set Lbl(RSMCFax)            { "Generation du FAX RSMC" "Generate RSMC FAX" }
   set Lbl(RSMCJointData)      { "Transmission des sorties RSMC vers les pages webs communes" "Transmit RSMC output to common web pages" }
   set Lbl(RSMCJointStatement) { "Transmission du Joint Statement RSMC vers les pages webs communes" "Transmit RSMC joint statement to common web pages" }
   set Lbl(Store)              { "Archiver" "Store" }
   set Lbl(Suppress)           { "Supprimer" "Delete" }
   set Lbl(Yes)                { "Oui" "Yes" }
   set Lbl(No)                 { "Non" "No" }
   set Lbl(Warning)            { "Attention" "Warning" }
   set Lbl(Params)             { "Parametres" "Parameters" }
   set Lbl(Name)               { "Nom" "Name" }

   #----- Messages

   set Msg(DoingStore)    { "Archivage en cours ..." "Archiving ..." }
   set Msg(DoingCopy)     { "Copie en cours ..." "Copying ..." }
   set Msg(Store)         { "Veuillez entrer le nom du rapport d'incident afin d'identifier l'experience en archives."
                            "Please enter the incident report name to identify the experiment to be archived." }
   set Msg(StoreExist)    { "Une archive avec ce nom existe déja, Voulez-vous l'écraser ?"
                            "An archive by this name already exist, do you want to overwrit it ?" }
   set Msg(Path)          { "Répertoire d'expériences invalides.\nImpossible de trouver le fichier eer_ExpList"
                            "Invalid experiment path.\n Unable to find the eer_ExpList file" }
   set Msg(Coord)         { "Les coordonnées de la source sont invalides"
                            "The specified source coordinates are invalid" }
   set Msg(Pos)           { "Auncune localisation n'a ete specifiee"
                            "No localisation have been specified" }
   set Msg(Name)          { "Le nom de l'experience ou de l'une des localisations n'a pas ete specifie"
                            "The experiment name or the name of one the localisation has not been specified" }
   set Msg(Fax)           { "Generation du fax dans le fichier suivant:"
                            "Generating fax to the following file:" }
   set Msg(JointData)     { "Etes-vous certain de vouloir transferer les cartes RSMC commune ?" \
                            "Do you really want to send the RSMC joint format maps ?" }
   set Msg(JointClear)    { "Voulez vous supprimer le joint statement ?" \
                            "Do you want to delete the joint statement ?" }
   set Msg(JointStatement) { "Etes-vous certain de vouloir transferer le joint statement ?" \
                            "Do you really want to send the joint statement ?" }
   set Msg(SendJoint)     { "Transfert en cours" "Transferring data" }
   set Msg(SuppressExp)   { "La suppression de l'experience supprimera definitivement toutes les simulations\
                             qui y son contenue.\n\nVoulez-vous supprimer cette experience ?" \
                            "Deleting this experiment will definitively erase all the simulations in it.\n\n\
                             Do you really want to delete this experiment ?" }
   set Msg(SuppressDone)  { "Suppression terminee !" \
                            "Suppressing done !" }
   set Msg(SuppressError) { "Impossible de supprimer les repertoires de l'experience suivante:" \
                            "Unable to suppress the following experiment directory" }
   set Msg(Correct0)      { "Voulez-vous lancer le modèle" "Do you wish to launch" }
   set Msg(Correct1)      { "à partir de ces paramètres d'entrée ci-haut?" "model with the above input parameters?" }
   set Msg(Kill)          { "Arrêt de la simulation" "Terminating simulation" }
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
            -width 1 -height 1 -bg white -highlightthickness 0 -bd 1
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
         -command "Model::New $Frame Exp::New \"[lindex $Lbl(New) $GDefs(Lang)]\" 0"
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

   Bubble::Create $Frame.path.sel     [lindex $Model::Bubble(PathAdd) $GDefs(Lang)]
   Bubble::Create $Frame.path.list    [lindex $Model::Bubble(PathSel) $GDefs(Lang)]

   Bubble::Create $Frame.opt.open     [lindex $Model::Bubble(Plus) $GDefs(Lang)]
   Bubble::Create $Frame.opt.close    [lindex $Model::Bubble(Minus) $GDefs(Lang)]
   Bubble::Create $Frame.opt.writer   [lindex $Writer::Param(Title) $GDefs(Lang)]
   Bubble::Create $Frame.opt.bulletin [lindex $Bulletin::Param(Title) $GDefs(Lang)]
   Bubble::Create $Frame.opt.bubble   [lindex $Model::Bubble(Bubble) $GDefs(Lang)]
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
         Dialog::SearchText $Text [lindex $ligne 0] [lindex $ligne 1] [lindex $ligne 2]
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
            0  { set fg black }                          ;# New
            1  { set fg black }                          ;# Done
            2  { set fg red; set Data(StateExp) 2 }      ;# Running
            3  { set fg darkgray }                       ;# Suspended
            4  { set fg blue  }                          ;# Continuation
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
         set coo [format "(%.5f,%.5f)" [lindex $loc 1] [lindex $loc 2]]
         lappend str "[lindex $loc 0] $coo"
      }
      CanvasBubble::Create $canvas EXP$no [join $str \n]
      $canvas bind EXP$no  <ButtonPress-3> "Exp::Select { $exp } ; Exp::PopUp %X %Y"
      $canvas bind EXP$no  <ButtonPress-1> "Exp::SelectBranch $no ; Exp::Select { $exp } "
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
               $canvas create text 40 $y -text $model -font $GDefs(Font) -anchor w -tags "TREE"
               $canvas create bitmap 30 $y -bitmap $Model::Resources(Plus) -tags "SIGN $model$no"
               $canvas bind $model$no <ButtonPress-1> "Exp::SelectBranchSim $model$no"

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
               lset SPI::Ico(DefEXPERIMENT) $idx 4 $ico
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
      set date  "[Info::Strip $Info AccYear]-[Info::Strip $Info AccMonth]-[Info::Strip $Info AccDay] [Info::Strip $Info AccHour]:[Info::Strip $Info AccMin]"
      set model "[Info::Strip $Info Meteo][Info::Strip $Info Delta][Info::Strip $Info Mode]"

      return "$dur $unit $date $model ($Exp::Data(NoSim))"
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
      Dialog::CreateWait $Data(Frame) [lindex $Msg(Kill) $GDefs(Lang)]
      update idletasks

      #----- Signal simulation to finish and wait for it to do so
      simulation define $Id -state DONE
      while { [simulation define $Id -percent]!=100 } { }
      simulation destroy $Id

      set ::Model::Param(Job) ""
      Dialog::DestroyWait

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

proc Exp::New { } {
   global GDefs
   variable Data
   variable Param
   variable Msg

   #----- Verifier la validitee des parametres
   if { $Model::Data(Name)=="" } {
       Dialog::CreateError .expnew [lindex $Msg(Name) $GDefs(Lang)] $GDefs(Lang)
       return 0
   }

   #----- Forcer le format degree centiemme
   if { "$Model::Param(Unit)" == "DDD MM" } {
      Model::SwitchCoord
   }

   regsub -all "\[^a-zA-Z0-9\]" $Model::Data(Name) "_" Model::Data(Name)

   set Data(Name) $Model::Data(Name)
   set Data(Type) $Model::Data(Type)
   set info ""

   for { set i 1 } { $i <=50 } { incr i } {

      if { $Model::Data(Lat$i)=="" || $Model::Data(Lon$i)=="" } {
         break
      }

      if { $Model::Data(Lat$i)<-90.0 || $Model::Data(Lat$i)>90.0 || $Model::Data(Lon$i)<-180 || $Model::Data(Lon$i)>360 } {
          Dialog::CreateError .expnew "[lindex $Msg(Coord) $GDefs(Lang)]\n\n\t$Model::Data(Name$i) $Model::Data(Lat$i) $Model::Data(Lon$i)\n" $GDefs(Lang)
          return 0
      }
      if { $Model::Data(Name$i)=="" } {
          Dialog::CreateError .expnew [lindex $Msg(Name) $GDefs(Lang)] $GDefs(Lang)
          return 0
      }

      set Data(Lat)  [format "%.10f" $Model::Data(Lat$i)]
      set Data(Lon)  [format "%.10f" $Model::Data(Lon$i)]

      regsub -all "\[^a-zA-Z0-9\]" $Model::Data(Name$i) "_" Model::Data(Name$i)

      lappend info [list $Model::Data(Name$i) $Data(Lat) $Data(Lon) $Model::Data(Id$i)]
   }

   if { [llength $info]==0 } {
       Dialog::CreateError .expnew [lindex $Msg(Pos) $GDefs(Lang)] $GDefs(Lang)
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
   Debug::TraceProc "Creating new Experiment : $line"
   exec echo "$line" >> $Param(Path)/eer_ExpList

   file mkdir $Param(Path)/${noexp}_$Data(Name)
   file mkdir $Param(Path)/${noexp}_$Data(Name)/Output

   #----- dans le cas RSMC, initialiser les images
   if { $Data(Type)==1 } {

      Debug::TraceProc "Copying RSMC joint data to directory $Param(Path)/${noexp}_$Data(Name)/Output/RSMCJoin"
      file mkdir $Param(Path)/${noexp}_$Data(Name)/Output/RSMCJoin
      file copy $GDefs(Dir)/Resources/Image/System/CVNOCA.gif $Param(Path)/${noexp}_$Data(Name)/Output/RSMCJoin/CVRCA.gif
      file copy $GDefs(Dir)/Resources/Image/System/CVNOCA.ps  $Param(Path)/${noexp}_$Data(Name)/Output/RSMCJoin/CVRCA.ps
      file copy $GDefs(Dir)/Resources/Image/System/LCNOCA.gif $Param(Path)/${noexp}_$Data(Name)/Output/RSMCJoin/LICCA_01.gif
      file copy $GDefs(Dir)/Resources/Image/System/LCNOCA.gif $Param(Path)/${noexp}_$Data(Name)/Output/RSMCJoin/LICCA_02.gif
      file copy $GDefs(Dir)/Resources/Image/System/LCNOCA.gif $Param(Path)/${noexp}_$Data(Name)/Output/RSMCJoin/LICCA_03.gif
      file copy $GDefs(Dir)/Resources/Image/System/LCNOCA.gif $Param(Path)/${noexp}_$Data(Name)/Output/RSMCJoin/LICCA_04.gif
      file copy $GDefs(Dir)/Resources/Image/System/LCNOCA.gif $Param(Path)/${noexp}_$Data(Name)/Output/RSMCJoin/LTDCA_01.gif
      file copy $GDefs(Dir)/Resources/Image/System/LCNOCA.gif $Param(Path)/${noexp}_$Data(Name)/Output/RSMCJoin/LTDCA_02.gif
      file copy $GDefs(Dir)/Resources/Image/System/LCNOCA.gif $Param(Path)/${noexp}_$Data(Name)/Output/RSMCJoin/LTDCA_03.gif
      file copy $GDefs(Dir)/Resources/Image/System/LCNOCA.gif $Param(Path)/${noexp}_$Data(Name)/Output/RSMCJoin/LTDCA_04.gif
      file copy $GDefs(Dir)/Resources/Image/System/LTNOCA.gif $Param(Path)/${noexp}_$Data(Name)/Output/RSMCJoin/LTJCA.gif
      file copy $GDefs(Dir)/Resources/Image/System/LCNOCA.ps  $Param(Path)/${noexp}_$Data(Name)/Output/RSMCJoin/LICCA_01.ps
      file copy $GDefs(Dir)/Resources/Image/System/LCNOCA.ps  $Param(Path)/${noexp}_$Data(Name)/Output/RSMCJoin/LICCA_02.ps
      file copy $GDefs(Dir)/Resources/Image/System/LCNOCA.ps  $Param(Path)/${noexp}_$Data(Name)/Output/RSMCJoin/LICCA_03.ps
      file copy $GDefs(Dir)/Resources/Image/System/LCNOCA.ps  $Param(Path)/${noexp}_$Data(Name)/Output/RSMCJoin/LICCA_04.ps
      file copy $GDefs(Dir)/Resources/Image/System/LCNOCA.ps  $Param(Path)/${noexp}_$Data(Name)/Output/RSMCJoin/LTDCA_01.ps
      file copy $GDefs(Dir)/Resources/Image/System/LCNOCA.ps  $Param(Path)/${noexp}_$Data(Name)/Output/RSMCJoin/LTDCA_02.ps
      file copy $GDefs(Dir)/Resources/Image/System/LCNOCA.ps  $Param(Path)/${noexp}_$Data(Name)/Output/RSMCJoin/LTDCA_03.ps
      file copy $GDefs(Dir)/Resources/Image/System/LCNOCA.ps  $Param(Path)/${noexp}_$Data(Name)/Output/RSMCJoin/LTDCA_04.ps
      file copy $GDefs(Dir)/Resources/Image/System/LTNOCA.ps  $Param(Path)/${noexp}_$Data(Name)/Output/RSMCJoin/LTJCA.ps
      file copy $GDefs(Dir)/Resources/Image/System/SCNOCA.gif $Param(Path)/${noexp}_$Data(Name)/Output/RSMCJoin/SICCA_01.gif
      file copy $GDefs(Dir)/Resources/Image/System/SCNOCA.gif $Param(Path)/${noexp}_$Data(Name)/Output/RSMCJoin/SICCA_02.gif
      file copy $GDefs(Dir)/Resources/Image/System/SCNOCA.gif $Param(Path)/${noexp}_$Data(Name)/Output/RSMCJoin/SICCA_03.gif
      file copy $GDefs(Dir)/Resources/Image/System/SCNOCA.gif $Param(Path)/${noexp}_$Data(Name)/Output/RSMCJoin/SICCA_04.gif
      file copy $GDefs(Dir)/Resources/Image/System/SCNOCA.gif $Param(Path)/${noexp}_$Data(Name)/Output/RSMCJoin/STDCA_01.gif
      file copy $GDefs(Dir)/Resources/Image/System/SCNOCA.gif $Param(Path)/${noexp}_$Data(Name)/Output/RSMCJoin/STDCA_02.gif
      file copy $GDefs(Dir)/Resources/Image/System/SCNOCA.gif $Param(Path)/${noexp}_$Data(Name)/Output/RSMCJoin/STDCA_03.gif
      file copy $GDefs(Dir)/Resources/Image/System/SCNOCA.gif $Param(Path)/${noexp}_$Data(Name)/Output/RSMCJoin/STDCA_04.gif
      file copy $GDefs(Dir)/Resources/Image/System/STNOCA.gif $Param(Path)/${noexp}_$Data(Name)/Output/RSMCJoin/STJCA.gif

      exec echo "Unavailable" >> $Param(Path)/${noexp}_$Data(Name)/Output/RSMCJoin/RUN.txt
      exec echo "Unavailable" >> $Param(Path)/${noexp}_$Data(Name)/Output/RSMCJoin/traject.points
      exec echo "01 02 03 04" > $Param(Path)/${noexp}_$Data(Name)/Output/RSMCJoin/IP2List.txt
  }

   exec chgrp -R $GDefs(Group) $Param(Path)/${noexp}_$Data(Name)
   exec chmod -R 755 $Param(Path)/${noexp}_$Data(Name)

   Model::Check 0
   Model::TypeSelect none 1
   return 1
}

#----------------------------------------------------------------------------
# Nom      : <Exp::Params>
# Creation : Octobre 1999 - J.P. Gauthier - CMC/CMOE
#
# But      : Permet de confirmer les parametres de la simulation.
#
# Parametres :
#   <Parent> : Identificateur de la fenetre parent
#
# Retour     :
#   <Valid>  : True ou False.
#
# Remarques :
#
#----------------------------------------------------------------------------

proc Exp::Params { Parent Model } {
   global   GDefs correct
   variable Lbl
   variable Msg
   variable Sim

   toplevel .simparams -bg $GDefs(ColorLight)

   wm title     .simparams [lindex $Lbl(Params) $GDefs(Lang)]
   wm resizable .simparams 1 1
   wm transient .simparams $Parent
   wm geom      .simparams 700x500+[expr [winfo rootx $Parent]+50]+[expr [winfo rooty $Parent]+50]
   wm protocol  .simparams WM_DELETE_WINDOW { }

   #----- Afficher la liste des parametres de l'experience.
   set correct False

   frame .simparams.desc
      scrollbar .simparams.desc.scroll -command ".simparams.desc.list yview" -bd 1 -width 10
      listbox .simparams.desc.list -relief sunken -yscrollcommand ".simparams.desc.scroll set" \
         -selectmode extended -exportselection 0 -background $GDefs(ColorLight) -bd 1
      pack .simparams.desc.list -side left -fill both -expand True
      pack .simparams.desc.scroll -side left -fill y
   pack  .simparams.desc -side top -fill both -expand True

   #----- Demander de confirmer la selection faite par l'usager.
   label .simparams.que -relief raised -bd 1 -text "[lindex $Msg(Correct0) $GDefs(Lang)] $Model [lindex $Msg(Correct1) $GDefs(Lang)]"
   pack .simparams.que -anchor w -ipadx 5  -ipady 5 -fill x

   frame .simparams.confirm
      button .simparams.confirm.yes -text [lindex $Lbl(Yes) $GDefs(Lang)] -command "set correct True" -relief raised -bd 1
      button .simparams.confirm.no -text [lindex $Lbl(No) $GDefs(Lang)] -command "set correct False" -relief raised -bd 1
      pack .simparams.confirm.yes .simparams.confirm.no -side left -fill x -expand true
   pack .simparams.confirm -side top  -fill x

   eval .simparams.desc.list insert end [split [Info::Format [Info::Code ::${Model}::Sim]] \n]

   grab .simparams
   tkwait variable correct

   destroy .simparams
   return $correct
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
         .exppop add command -label ""  -command "Model::TypeSelect none 1 \$Exp::Data(Name); Exp::CreateTree; SPI::Locate \$Exp::Data(Lat) \$Exp::Data(Lon)" \
             -background $GDefs(ColorHighLight) -activebackground $GDefs(ColorHighLight)
         .exppop add cascade -label [lindex $Lbl(New) $GDefs(Lang)] -menu .exppop.new
         .exppop add separator
         .exppop add cascade -label [lindex $Lbl(Product) $GDefs(Lang)] -menu .exppop.product -state disabled
         .exppop add separator
         .exppop add command -label [lindex $Lbl(Suppress) $GDefs(Lang)] -command "Exp::Suppress"
         .exppop add command -label [lindex $Lbl(Store) $GDefs(Lang)] -command "Exp::Store .model"

      #----- Menu des modeles
      menu  .exppop.new -tearoff 0 -bd 1 -type normal -activeborderwidth 1
      foreach model $Data(Models) {
         .exppop.new add command -label $model -command "Model::ParamsWindow $model NEW"
      }

      menu  .exppop.product -tearoff 0 -bd 1 -type normal -activeborderwidth 1
         .exppop.product add command -label [lindex $Lbl(RSMCFax) $GDefs(Lang)] -command  { Exp::ProductRSMCFax }
         .exppop.product add command -label [lindex $Lbl(RSMCJointData) $GDefs(Lang)] -command  {Exp::ProductRSMCJointData}
         .exppop.product add command -label [lindex $Lbl(RSMCJointStatement) $GDefs(Lang)] -command { Exp::ProductRSMCJointStatement [FileBox::Create . "" Load ""] }
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

   Dialog::CreateMessage . "[lindex $Msg(Fax) $GDefs(Lang)]\n\n\t$path/rsmc_fax.ps"

   set nbre [lindex [exec wc -w $path/IP2List.txt] 0]

   set LIC ""
   set LTD ""

   for { set no 1 } { $no <= $nbre } { incr no } {
      set LIC "$LIC $path/LICCA_0${no}.ps"
      set LTD "$LTD $path/LTDCA_0${no}.ps"
   }

   eval exec cat $path/CVRCA.ps $path/LTJCA.ps $LIC $LTD > $path/rsmc_fax.ps

   exec chmod 644 $path/rsmc_fax.ps

   destroy .msgbox
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
   global GDefs
   variable Data
   variable Param
   variable Msg
   variable Lbl

   . config -cursor watch
   update idletasks

   set send [Dialog::CreateDefault . 400 [lindex $Lbl(Warning) $GDefs(Lang)] [lindex $Msg(JointData) $GDefs(Lang)] \
      warning 0 [lindex $Lbl(Yes) $GDefs(Lang)] [lindex $Lbl(No) $GDefs(Lang)]]

   if { $send } {
      return
   }

   set join [Dialog::CreateDefault . 400 [lindex $Lbl(Warning) $GDefs(Lang)] [lindex $Msg(JointClear) $GDefs(Lang)] \
      warning 0 [lindex $Lbl(Yes) $GDefs(Lang)] [lindex $Lbl(No) $GDefs(Lang)]]

   #----- setup le repertoire et le fichier concernant la run du modele meteo utilise.
   set path "$Param(Path)/$Data(No)_$Data(Name)/Output/RSMCJoin"
   set run [exec cat $path/RUN.txt]

   #----- on recupere l'index de la region afin de determiner la region RSMC correspondante.
   set region [expr [ogrlayer pick RSMC "$Exp::Data(Lat) $Exp::Data(Lon) "] + 1]

   if { $region == "3" || $region == "4" } {
      exec echo "34" > $path/leadrsmc.txt
      catch { exec ssh $GDefs(FrontEnd) -x -l afseeer $GDefs(Dir)/Script/JNT_SEND.sh $path/leadrsmc.txt leadrsmc.txt }
   } elseif { $region == "5" } {
      exec echo "345" > $path/leadrsmc.txt
      catch { exec ssh $GDefs(FrontEnd) -x -l afseeer $GDefs(Dir)/Script/JNT_SEND.sh $path/leadrsmc.txt leadrsmc.txt }
   } else {
      file delete -force $path/leadrsmc.txt
   }

   #----- cree le fichier pour la date.
   if { $run == "Unavailable" } {
      exec echo "Unavailable" > $path/CA_DATE.TXT
   } else {
      exec echo [clock format [clock seconds] -format "%Y%m%d${run}_%H%M" -gmt true] > $path/CA_DATE.TXT
   }

   Dialog::CreateMessage . [lindex $Msg(SendJoint) $GDefs(Lang)]

   set nbip2 [lindex [exec wc -w  $path/IP2List.txt] 0]

   catch  { exec ssh $GDefs(FrontEnd) -x -l $GDefs(FrontEndUser) $GDefs(Dir)/Script/RSMCJointTransfer.sh $path $nbip2 }

   if { !$join } {
      catch { exec ssh $GDefs(FrontEnd) -x -l afseeer $GDefs(Dir)/Script/JNT_SEND.sh $GDefs(Dir)/Data/jntreg34.html jntreg34.html }
   }

   destroy .msgbox
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
   global GDefs
   variable Data
   variable Param
   variable Msg
   variable Lbl

   if { $File=="" } {
      return
   }

   . config -cursor watch
   update idletasks

   #----- setup le repertoire et le fichier concernant le joint statement.
   set path "$Param(Path)/$Data(No)_$Data(Name)/Output/RSMCJoin/joint_statement_b.html"
   file copy -force $File $path

   set send [Dialog::CreateDefault . 400 [lindex $Lbl(Warning) $GDefs(Lang)] [lindex $Msg(JointStatement) $GDefs(Lang)] \
      warning 0 [lindex $Lbl(Yes) $GDefs(Lang)] [lindex $Lbl(No) $GDefs(Lang)]]

   if { $send } {
      return
   }

   catch  { exec ssh $GDefs(FrontEnd) -x -l afseeer $GDefs(Dir)/Script/JNT_SEND.sh $path jntreg34.html }
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
      set Data(List) [lsort -dictionary $Data(List)]
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
         Dialog::CreateError . [lindex $Msg(Path) $GDefs(Lang)] $GDefs(Lang)
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
   set Data(Lat)  [lindex [lindex [lindex $Pool 3] 0] 1]
   set Data(Lon)  [lindex [lindex [lindex $Pool 3] 0] 2]

   #----- Selectionner la nouvelle experience
   set Data(Select) $Pool
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
# But      : Interface d'archivage
#
# Parametres:
#
# Retour:
#
# Remarques :
#
#-------------------------------------------------------------------------------

proc Exp::Store { Master } {
   global GDefs
   variable Lbl
   variable Msg

   toplevel .dlgstore
   wm title .dlgstore [lindex $Lbl(Store) $GDefs(Lang)]
   wm protocol .dlgstore WM_DELETE_WINDOW { }
   if { [winfo exists $Master] } {
      wm transient .dlgstore $Master
      wm geom .dlgstore +[expr [winfo rootx $Master]+50]+[expr [winfo rooty $Master]+50]
   }

   #----- Afficher le frame du haut qui va contenir le message
   frame .dlgstore.msg -relief raised -bd 1
      label .dlgstore.msg.bitmap -bitmap info
      message .dlgstore.msg.txt -aspect 1000 -text [lindex $Msg(Store) $GDefs(Lang)]
      pack .dlgstore.msg.bitmap -side left -padx 20 -pady 20
      pack .dlgstore.msg.txt -padx 20 -pady 20
   pack .dlgstore.msg -side top -expand true -fill x

   frame .dlgstore.id -relief raised -bd 1
      label .dlgstore.id.lbl -text [lindex $Lbl(Name) $GDefs(Lang)] -anchor w -width 5
      entry .dlgstore.id.def -relief sunken -bd 1 -bg $GDefs(ColorLight) -textvariable Exp::Data(StoreID)
      pack .dlgstore.id.lbl  -side left
      pack .dlgstore.id.def -side left -expand true -fill x -ipady 2
   pack .dlgstore.id -side top -expand true -fill x

   #----- Afficher le frame du bas qui va contenir le bouton retour
   frame .dlgstore.cmd -relief flat
      button .dlgstore.cmd.ok -text "Ok" -command { if { [Exp::StoreIt $Exp::Data(StoreID)] } { destroy .dlgstore } } -bd 1
      button .dlgstore.cmd.cancel -text "Cancel" -command "destroy .dlgstore" -bd 1
      pack .dlgstore.cmd.ok .dlgstore.cmd.cancel -side left -ipadx 10 -fill x  -expand true
   pack .dlgstore.cmd -side top -fill x

   update idletasks
   grab .dlgstore
}

#-------------------------------------------------------------------------------
# Nom      : <Exp::StoreIt>
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

proc Exp::StoreIt { Id } {
   global GDefs
   variable Param
   variable Lbl
   variable Msg

   set code  True
   set cpath [pwd]

   #----- Check if an archive already exists
   set ErrorCode [catch { exec ssh $Param(StoreHost) ls -1 $Param(StorePath)/$Id.cmc } Message]
   if { !$ErrorCode } {
      set notoverwrite [Dialog::CreateDefault .dlgstore 400 [lindex $Lbl(Warning) $GDefs(Lang)] [lindex $Msg(StoreExist) $GDefs(Lang)] \
         warning 0 [lindex $Lbl(Yes) $GDefs(Lang)] [lindex $Lbl(No) $GDefs(Lang)]]

      if { $notoverwrite } {
         return False
      }
   }

   Dialog::CreateWait .dlgstore [lindex $Msg(DoingStore) $GDefs(Lang)]

   #----- Build the archive
   cd  [set path [Exp::Path]]/../
#   set ErrorCode [catch { exec cmcarc -a $path -f /tmp/$Id.cmc --md5 --dereference } Message]
   set ErrorCode [catch { exec tar -zcvf /tmp/$Id.tgz $path } Message]
   if { $ErrorCode } {
      Dialog::CreateError .dlgstore $Message $GDefs(Lang)
      set code False
   } else {
      #----- Copy it to CFS
      Dialog::CreateWait .dlgstore  [lindex $Msg(DoingCopy) $GDefs(Lang)]
      set ErrorCode [catch { exec scp /tmp/$Id.cmc $Param(StoreHost):$Param(StorePath)/$Id.cmc } Message]
      if { $ErrorCode } {
         Dialog::CreateError .dlgstore $Message $GDefs(Lang)
         set code False
      } else {
         #----- Remove local copy
         file delete -force /tmp/$Id.cmc
      }
   }

   Dialog::DestroyWait
   cd $cpath

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
   set answer [Dialog::CreateDefault . 400 "Message" "[lindex $Msg(SuppressExp) $GDefs(Lang)]\n\n\t($Data(No)) $Data(Name)" \
     warning 0 [lindex $Lbl(Yes) $GDefs(Lang)] [lindex $Lbl(No) $GDefs(Lang)]]

   if { $answer == 1 } {
      return
   }

   . config -cursor watch

   Debug::TraceProc "Suppressing experiment : $Data(No) $Data(Name)"

   #----- Supprimer le repertoire de l'experience
   file delete -force $Param(Path)/$Data(No)_$Data(Name)

   if { [file exists $Param(Path)/$Data(No)_$Data(Name)] } {
      Dialog::CreateError .  "[lindex $Msg(SuppressError) $GDefs(Lang)]\n\n\t$Param(Path)/$Data(No)_$Data(Name)" $GDefs(Lang)
      Debug::TraceProc "Unable to suppress experiment : $Data(No) $Data(Name)"
   } else {

      #----- Supprimer l'information de cette experience dans le pool
      Debug::TraceProc "Suppressing experiment : $Data(No) $Data(Name)"
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

