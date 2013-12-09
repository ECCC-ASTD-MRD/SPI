#===============================================================================
# Environnement Canada
# Centre Meteorologique Canadian
# 2100 Trans-Canadienne
# Dorval, Quebec
#
# Projet   : Librairie de "Widget" Tk.
# Fichier  : Watch.tcl
# Creation : Septembre 2001 - J.P.Gauthier - CMC/CMOE
# Modif    : (Majeur) Juillet 2009 - E. Legault-Ouellet - CMC/CMOE
#
# Description:
#    Fonctions de manipulations des Watchs.
#
# Remarques :
#
#===============================================================================

package require IsoBox

namespace eval Watch {
   global   GDefs
   variable Data
   variable Lbl
   variable Msg
   variable Error
   variable Bubble
   variable Param

   set Param(Path)        ""
   set Param(Projects)    { VAAC RSMC CTBT FIRE BIO SPILL }

   #----- Variables relatives aux Watchs
   set Data(Frame)         ""
   set Data(Select)        ""                                              ;#Experience selectionnee
   set Data(Projects)      {}

   #----- Liste des branches ouvertes pour chacun des niveaux
   set Data(BranchWatch)   ""
   set Data(BranchModel)   ""
   set Data(BranchProject) ""
   set Data(BranchSim)     ""
   #----- Labels
   set Lbl(New)         { "Nouveau" "New" }
   set Lbl(Yes)         { "Oui" "Yes" }
   set Lbl(No)          { "Non" "No" }
   set Lbl(Params)      { "Parametres" "Parameters" }
   set Lbl(Suppress)    { "Supprimer" "Delete" }
   set Lbl(Close)       { "Fermer" "Close" }
   set Lbl(Result)      { "Resultats" "Results" }
   set Lbl(Auto)        { "Automatisation" "Automatisation" }
   set Lbl(AddSim)      { "Ajouter la simulation" "Add simulation" }
   set Lbl(Edit)        { "Modifier" "Edit" }
   set Lbl(EditSim)     { "Modifier la simulation" "Edit simulation" }
   set Lbl(OpenBranch)  { "Ouvrir branches" "Open branches" }
   set Lbl(CloseBranch) { "Fermer branches" "Close branches" }

   #--- Definitions des messages
   set Msg(Suppress)      { "Voulez-vous reellement desactiver cette veille ?"
                            "Do you really want to desactivate this watch ?" }
   set Msg(SuppressData)  { "Voulez-vous supprimer les r�sultats de simulation de cette veille ?"
                            "Do you want to delete this watch's simulation results ?" }
}

#-------------------------------------------------------------------------------
# Nom      : <Watch::AllOpenItem>
# Creation : Juillet 2009 - E. Legault-Ouellet - CMC/CMOE
#
# But      : Ouvrir toutes les branches d'un item (projet, watch, model, sim).
#
# Parametres :
#     <Tag>    : Le tag permettant d'avoir les informations sur la branche a ouvrir.
#     <Level>  : Le niveau de la branche a ouvrir.
#
# Retour :
#
# Remarques :
#
#     Tag :
#        Projet --> nom du projet
#        Watch  --> nom de la Watch ($project$name)
#        Modele --> nom de la watch et nom du modele ($project$name$model)
#        Sim    --> nom de la watch et du model ainsi que numero de la sim
#                   ($project$name$model$no)
#
#     Level :
#        0 --> Ouvre un projet et tous ses enfants
#        1 --> Ouvre une watch et tous ses enfants
#        2 --> Ouvre un model et tous ses enfants
#        3 --> Ouvre une simulation et tous ses enfants
#
#-------------------------------------------------------------------------------

proc Watch::AllOpenItem { Tag Level } {
   variable Data

   while { $Level<3 } {
      set tags {}

      switch $Level {
         0 {
            #----- Only on project open at a time
            Watch::AllCloseItem $Data(BranchProject) 0

            set Data(BranchProject) [concat $Data(BranchProject) $Tag]

            if { [info exists Data(Exps$Tag)] } {
               foreach exp $Data(Exps$Tag) {
                  lappend tags $Tag$exp
               }
               set Tag $tags
            }
         }
         1 {
            set Data(BranchWatch) [concat $Data(BranchWatch) $Tag]

            foreach tag $Tag {
               foreach model $Data(Models$tag) {
                  lappend tags $tag$model
               }
            }
            set Tag $tags
         }
         2 {
            set Data(BranchModel) [concat $Data(BranchModel) $Tag]

            foreach tag $Tag {
               foreach sim $Data(Sims$tag) {
                  lappend tags $tag[lindex $sim 0]
               }
            }
            set Tag $tags
         }
         3 {
            set Data(BranchSim) [concat $Data(BranchSim) $Tag]
         }
         default {
            return
         }
      }
      incr Level
   }
   Watch::CreateTree
}

#-------------------------------------------------------------------------------
# Nom      : <Watch::AllCloseItem>
# Creation : Juillet 2009 - E. Legault-Ouellet - CMC/CMOE
#
# But      : Fermer toutes les branches d'un item (projet, watch, model, sim).
#
# Parametres :
#     <Tag>    : Le tag permettant d'avoir les informations sur la branche a fermer.
#     <Level>  : Le niveau de la branche a fermer.
#
# Retour :
#
# Remarques :
#
#-------------------------------------------------------------------------------

proc Watch::AllCloseItem { Tag Level } {
   variable Data

   set branch ""

   #----- Loop on branch levels
   foreach branch [lrange { BranchProject BranchWatch BranchModel BranchSim } $Level end] {

      #----- Remove every occurence of the tag in the branch level
      foreach id [lsort -integer -decreasing [lsearch -all $Data($branch) $Tag*]] {
         set Data($branch) [lreplace $Data($branch) $id $id]
      }
   }
   Watch::CreateTree
}

#----------------------------------------------------------------------------
# Nom      : <Watch::Create>
# Creation : Septembre 2001 - J.P. Gauthier - CMC/CMOE
#
# But      : Creer le widget des watches.
#
# Parametres :
#   <Frame>  : Frame dans lequel creer le widget
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc Watch::Create { Frame } {
   global GDefs
   variable Lbl
   variable Bubble
   variable Data

   set Data(Frame) $Frame

   frame $Frame.info
      canvas $Frame.info.canvas -bg white -relief sunken -bd 1 -yscrollcommand "$Frame.info.scroll set" \
         -scrollregion "1 1 280 5000" -width 200 -height 1
      scrollbar $Frame.info.scroll -orient vertical -bd 1 -width 10 \
         -command "$Frame.info.canvas yview"
      pack $Frame.info.canvas -side left -fill both -expand true
      pack $Frame.info.scroll -side left -fill y

      bind $Frame.info.canvas <Button-4> "$Frame.info.canvas yview scroll -1 units"
      bind $Frame.info.canvas <Button-5> "$Frame.info.canvas yview scroll 1 units"

   pack $Frame.info -side top -fill both -expand true  -padx 2 -pady 2

   Bubble::Create $Frame.opt.open   $Model::Bubble(Plus)
   Bubble::Create $Frame.opt.close  $Model::Bubble(Minus)
   Bubble::Create $Frame.opt.bubble $Model::Bubble(Bubble)
}

#-------------------------------------------------------------------------------
# Nom      : <Watch::CreateTree>
# Creation : Aout 2001 - J.P. Gauthier - CMC/CMOE
#            Fortement modifie en Juillet 2009 par E. Legault-Ouellet
#
# But      : Creer l'arborescence des watchs.
#
# Parametres :
#
# Retour :
#
# Remarques :
#
#-------------------------------------------------------------------------------

proc Watch::CreateTree { } {
   global GDefs
   variable Data

   set canvas $Data(Frame).info.canvas
   set y 15
   set x 10

   #----- Cleanup du canvas
   $canvas delete TREE SIGN WATCH SIM RESULTSELECT PROJECT MODEL RESULT
   $canvas create rectangle -10 -10 -10 -10 -outline black -fill $GDefs(ColorHighLight) -tags "RESULTSELECT"

   #----- Creation des branches des differents projets
   foreach proj [lsort $Data(Projects)] {
      set y [Watch::CreateBranchProject $canvas $proj $x $y]
      incr y 21
   }

   $canvas configure -scrollregion "1 1 280 $y"

   $canvas bind SIGN <Enter> "$canvas config -cursor hand1"
   $canvas bind SIGN <Leave> "$canvas config -cursor left_ptr"
}

#-------------------------------------------------------------------------------
# Nom      : <Watch::CreateBranchProject>
# Creation : Juillet 2009 - E. Legault-Ouellet - CMC/CMOE
#
# But      : Creer l'arborescence d'un projet.
#
# Parametres :
#     <Canvas>    : Le Frame dans lequel dessiner l'arbre
#     <Project>   : Le nom du projet
#     <X>         : La position en X o� commencer l'arborescence
#     <Y>         : La position en Y o� commencer l'arborescence
#
# Retour : La position en Y o� est rendu l'arborescence
#
# Remarques :
#
#-------------------------------------------------------------------------------

proc Watch::CreateBranchProject { Canvas Project X Y } {
   global GDefs
   variable Data

   set y1 [set y0 [expr $Y+10]]

   #----- Creation de la ligne descriptive du projet
   $Canvas create bitmap $X $Y -bitmap $Model::Resources(Plus) -tags "SIGN PPROJECT$Project"
   $Canvas create image [expr $X+20] $Y -image [Watch::GetIcon $Project] -tags "PROJECT"
   $Canvas create text [expr $X+33] $Y -text "$Project" -anchor w -tags "PROJECT PROJECT$Project" -font $GDefs(Font) -fill black

   $Canvas bind PPROJECT$Project <ButtonPress-1> "set Watch::Data(Project) $Project; Watch::ReadProject $Project; Watch::SelectBranch $Project BranchProject True; Model::TypeSelect none 2 \"\" $Project"
   $Canvas bind PROJECT$Project  <ButtonPress-1> "set Watch::Data(Project) $Project; Watch::ReadProject $Project; Watch::SelectBranch $Project BranchProject True; Model::TypeSelect none 2 \"\" $Project"
   $Canvas bind PROJECT$Project  <ButtonPress-3> "set Watch::Data(Project) $Project; Watch::PopUpProject %X %Y"

   #----- On cree les branches des watchs seulement s'il faut les afficher
   if { [lsearch $Data(BranchProject) $Project] != -1 } {
      $Canvas itemconfigure PPROJECT$Project -bitmap $Model::Resources(Minus)

      #----- On verifie qu'il y a quelque chose a afficher
      if { [info exists Data(Exps$Project)] } {

         foreach exp [lsort $Data(Exps$Project)] {
            set y1 [incr Y 21]
            set Y [Watch::CreateBranchWatch $Canvas $Project "$exp" [expr $X+20] $Y]
         }

         #----- Creation de la ligne verticale de l'arbre
         $Canvas create line $X $y0 $X $y1 -tags TREE
      }
   }

   return $Y
}

#-------------------------------------------------------------------------------
# Nom      : <Watch::CreateBranchWatch>
# Creation : Juillet 2009 - E. Legault-Ouellet - CMC/CMOE
#
# But      : Creer l'arborescence d'une watch.
#
# Parametres :
#     <Canvas>    : Le Frame dans lequel dessiner l'arbre
#     <Project>   : Le nom du projet
#     <Exp>       : Une liste contenant les informations de la watch
#     <X>         : La position en X o� commencer l'arborescence
#     <Y>         : La position en Y o� commencer l'arborescence
#
# Retour : La position en Y o� est rendu l'arborescence
#
# Remarques :
#
#-------------------------------------------------------------------------------

proc Watch::CreateBranchWatch { Canvas Project Exp X Y } {
   global GDefs
   variable Data

   set tag $Project$Exp
   set coords {}

   foreach src $Data(Sources$tag) {
      lappend coords "[lindex $src 0] ([lindex $src 1],[lindex $src 2])"
   }

   set y1 [set y0 [expr $Y+10]]

   #----- Creation de la ligne descriptive des watch
   $Canvas create bitmap $X $Y -bitmap $Model::Resources(Plus) -tags "SIGN PWATCH$tag"
   $Canvas create text [expr $X+10] $Y -text "$Exp" -anchor w -tags "WATCH WATCH$tag" -font $GDefs(Font) -fill black
   $Canvas create line [expr $X-20] $Y [expr $X-10] $Y -tags TREE

   CanvasBubble::Create $Canvas WATCH$tag "[join $coords "\n"]"
   $Canvas bind WATCH$tag  <ButtonPress-3> "Watch::Select \"$Exp\" $Project ; Watch::PopUpWatch %X %Y"
   $Canvas bind PWATCH$tag <ButtonPress-1> "Watch::SelectBranch $tag BranchWatch"

   #----- On creer les branches des modeles seulement s'il faut les afficher
   if { [lsearch -exact $Data(BranchWatch) $tag] != -1 } {
      $Canvas itemconfigure PWATCH$tag -bitmap $Model::Resources(Minus)
      foreach model [lsort $Data(Models$tag)] {
         if { $model!="NONE" } {
            set y1 [incr Y 21]
            set Y [Watch::CreateBranchModel $Canvas $Project "$Exp" $model [expr $X+20] $Y]
         }
      }

      $Canvas create line $X $y0 $X $y1 -tags TREE
   }

   return $Y
}

#-------------------------------------------------------------------------------
# Nom      : <Watch::CreateBranchModel>
# Creation : Juillet 2009 - E. Legault-Ouellet - CMC/CMOE
#
# But      : Creer l'arborescence d'un model.
#
# Parametres :
#     <Canvas>    : Le Frame dans lequel dessiner l'arbre
#     <Project>   : Le nom du projet
#     <Exp>       : Une liste contenant les informations de la watch
#     <Model>     : Le nom du model
#     <X>         : La position en X o� commencer l'arborescence
#     <Y>         : La position en Y o� commencer l'arborescence
#
# Retour : La position en Y o� est rendu l'arborescence
#
# Remarques :
#
#-------------------------------------------------------------------------------

proc Watch::CreateBranchModel { Canvas Project Exp Model X Y } {
   global GDefs
   variable Data

   set y1 [set y0 [expr $Y+10]]
   set tag  $Project$Exp$Model

   #----- Creation de la ligne descriptive des modeles
   $Canvas create line [expr $X-20] $Y [expr $X-10] $Y -tags TREE
   $Canvas create text [expr $X+10] $Y -text $Model -font $GDefs(Font) -anchor w -tags "MODEL$tag MODEL"
   $Canvas create bitmap $X $Y -bitmap $Model::Resources(Plus) -tags "SIGN PMODEL$tag"
   $Canvas bind PMODEL$tag <ButtonPress-1> "Watch::SelectBranch $tag BranchModel"
   $Canvas bind MODEL$tag <ButtonPress-3> "Watch::Select \"$Exp\" $Project ; Watch::PopUpModel %X %Y $Model"

   #----- On creer les branches des simulations seulement s'il faut les afficher
   if { [lsearch -exact $Watch::Data(BranchModel) $tag] != -1 } {
      $Canvas itemconfigure PMODEL$tag -bitmap $Model::Resources(Minus)

      foreach sim [lsort -index 0 $Data(Sims$tag)] {
         set y1 [incr Y 21]
         set Y [Watch::CreateBranchSim $Canvas $Project "$Exp" $Model "$sim" [expr $X+20] $Y]
      }

      #----- Ligne verticale de la sous-branche
      $Canvas create line $X $y0 $X $y1 -tags TREE
   }

   return $Y
}

#-------------------------------------------------------------------------------
# Nom      : <Watch::CreateBranchSim>
# Creation : Juillet 2009 - E. Legault-Ouellet - CMC/CMOE
#
# But      : Creer l'arborescence d'une simulation.
#
# Parametres :
#     <Canvas>    : Le Frame dans lequel dessiner l'arbre
#     <Project>   : Le nom du projet
#     <Exp>       : Une liste contenant les informations de la watch
#     <Model>     : Le nom du model
#     <Sim>       : La liste d'une simulation
#     <X>         : La position en X o� commencer l'arborescence
#     <Y>         : La position en Y o� commencer l'arborescence
#
# Retour : La position en Y o� est rendu l'arborescence
#
# Remarques :
#
#-------------------------------------------------------------------------------

proc Watch::CreateBranchSim { Canvas Project Exp Model Sim X Y } {
   global GDefs
   variable Data
   variable Param

   set info  [lindex $Sim 1]
   set mode  [Info::Strip $info Mode]
   set meteo [Info::Strip $info Meteo]
   set delta [Info::Strip $info Delta]
   set back  [Info::Strip $info Backward]
   set no    [lindex $Sim 0]

   if { [set dur [Info::Strip $info Duration]]!="" } {
      set unit Hrs
   } else {
      set dur [Info::Strip $info DurMin]
      set unit Min
   }
   if { $back==1 } {
      set txt "$dur $unit back $meteo$delta ($no)"
   } else {
      set txt "$dur $unit $meteo$delta ($no)"
   }
   set tag "$Project$Exp$Model$no"

   set y1 [set y0 [expr $Y+10]]

   #----- Creation de la ligne descriptive de la simulation
   $Canvas create text [expr $X+10] $Y -text "$txt" -anchor w -tags "SIM SIM$tag" -font $GDefs(Font)
   $Canvas create line [expr $X-20] $Y [expr $X-10] $Y -tags TREE
   $Canvas create bitmap $X $Y -bitmap $Model::Resources(Plus) -tags "SIGN PSIM$tag"

   $Canvas bind SIM$tag <ButtonPress-3> "Watch::SelectSim $Model \"$info\" \"$Exp\" $Project ; Watch::PopUpSim %X %Y"
   $Canvas bind PSIM$tag <ButtonPress-1> "Watch::SelectBranch $tag BranchSim"

   CanvasBubble::Create $Canvas SIM$tag [Info::Format $info]

   #----- Check for AutoSim version
   if { [file isdirectory $Param(Path)/../data] } {
      set path $Param(Path)/../data/$Project
   } else {
      set path $Param(Path)/$Project/data
   }
   
   #----- On cree les branches des resultats seulement s'il faut les afficher
   if { [lsearch -exact $Watch::Data(BranchSim) $tag] != -1 } {
      $Canvas itemconfigure PSIM$tag -bitmap $Model::Resources(Minus)

      #----- Loop on met runs
      foreach run [lsort [glob -nocomplain -type d -directory $path -tails *]] {
      
         #----- Trouve tous les dossiers des resultats des simulations
         foreach result [lsort [glob -nocomplain $path/${run}/*_$Exp/$Model.$no.*]] {
            set y1 [incr Y 21]
            set Y [Watch::CreateBranchResult $Canvas $Project "$Exp" $run $Model "$Sim" $result $X $Y]
         }
      }

      #----- Ligne verticale de la sous-branche
      $Canvas create line $X $y0 $X $y1 -tags TREE
   }

   return $Y
}

#-------------------------------------------------------------------------------
# Nom      : <Watch::CreateBranchResult>
# Creation : Juillet 2009 - E. Legault-Ouellet - CMC/CMOE
#
# But      : Creer l'arborescence d'un resultat.
#
# Parametres :
#     <Canvas>    : Le Frame dans lequel dessiner l'arbre
#     <Project>   : Le nom du projet
#     <Exp>       : Une liste contenant les informations de la watch
#     <Run>       : Meteo run
#     <Model>     : Le nom du model
#     <Sim>       : La liste d'une simulation
#     <Result>    : Nom complet du fichier resultat (path complet)
#     <X>         : La position en X o� commencer l'arborescence
#     <Y>         : La position en Y o� commencer l'arborescence
#
# Retour : La position en Y o� est rendu l'arborescence
#
# Remarques :
#
#-------------------------------------------------------------------------------

proc Watch::CreateBranchResult { Canvas Project Exp Run Model Sim Result X Y } {
   global GDefs
   variable Data

   set info   [lindex $Sim 1]
   set no     [lindex $Sim 0]
   set result [split [file tail $Result] .]

   #----- Creation du texte de lA'ffichage sous la forme YYYY-MM-DD hh:mm
   set date [lindex $result 2]
   set time [lindex $result 3]
   set txt "($Run) [string range $date 0 3]-[string range $date 4 5]-[string range $date 6 7] [string range $time 0 1]:[string range $time 2 3]"
   set tag "$Project$Exp$Run$Model$no[lindex $result 2][lindex $result 3]"

   $Canvas create text [expr $X+13] $Y -text "$txt" -anchor w -tags "RESULT RESULT$tag" -font $GDefs(Font)
   $Canvas create line $X $Y [expr $X+10] $Y -tags TREE

   $Canvas bind RESULT$tag <ButtonPress-3> "Watch::SelectResult RESULT$tag $Model \"$info\" \"$Exp\" $Project $Result ; Watch::PopUpResult \"$Result\" \"$txt\" %X %Y"
   $Canvas bind RESULT$tag <ButtonPress-1> "Watch::SelectResult RESULT$tag $Model \"$info\" \"$Exp\" $Project $Result"

   return $Y
}

#-------------------------------------------------------------------------------
# Nom      : <Watch::GetIcon>
# Creation : Juillet 2009 - E. Legault-Ouellet - CMC/CMOE
#
# But      : Retourne le numero du type correspondant au projet.
#
# Parametres :
#     <Project>   : Le projet dont on veut le numero du type
#
# Retour : Le numero du type correspondant au projet (default : 6)
#
# Remarques :
#
#-------------------------------------------------------------------------------

proc Watch::GetType { Project } {
   variable Param

   foreach proj $Param(Projects) no { 0 1 2 3 4 5 } {
      if { [string first $proj [string toupper $Project]]!=-1 } {
         return $no
      }
   }
   return 6
}

proc Watch::GetIcon { Project } {
   return [lindex $Model::Resources(Icos) [Watch::GetType $Project]]
}

#-------------------------------------------------------------------------------
# Nom      : <Watch::New>
# Creation : Aout 2001 - J.P. Gauthier - CMC/CMOE
# Modif    : Juillet 2009 - E. Legault-Ouellet - CMC/CMOE
#
# But      : Creer une nouvelle Watch dans la liste existante.
#
# Parametres:
#
# Retour :
#
# Remarques :
#
#-------------------------------------------------------------------------------

proc Watch::New { { Edit False } } {
   global GDefs
   variable Data
   variable Param
   variable Sim
   variable Tmp

   #----- Verifier la validitee des parametres
   if { $Model::Data(Name)=="" } {
       Dialog::Error .expnew $Model::Error(Name)
       return 0
   }

   regsub -all "\[^a-zA-Z0-9\]" $Model::Data(Name) "_" Data(Exp)
   regsub -all "\[-\]\[-\]*" $Data(Exp) "_" Data(Exp)

   #----- On verifie que le nom n'existe pas deja
   if { !$Edit && [lsearch -exact $Data(Exps$Data(Project)) $Data(Exp)]!=-1 } {
      Dialog::Error . $Model::Error(Exist)
      return 0
   }
   set srcs {}
   set lats {}
   set lons {}
   set coss {}

   foreach src $Model::Data(Srcs) coords $Model::Data(Coords) id $Model::Data(Ids) {

      regsub -all "\[^a-zA-Z0-9\]" $src "_" src

      if { $src=="" || [lsearch -exact $srcs $src]!=-1 } {
         Dialog::Error .expnew $Model::Error(Name)
         return 0
      }

      if { ![llength $coords] } {
         Dialog::Error .expnew $Model::Error(Coord) "\n\n\t$src\n"
         return 0
      }

      set cos {}
      foreach { lat lon } $coords {

         #----- Forcer le format degree centiemme
         set lat [Convert::Minute2Decimal $lat 6]
         set lon [Convert::Minute2Decimal $lon 6]

         if { $lat<-90.0 || $lat>90.0 || $lon<-180 || $lon>360 } {
            Dialog::Error .expnew $Model::Error(Coord) "\n\n\t$src $lat $lon\n"
            return 0
         }
         lappend cos $lat $lon
      }
      lappend srcs $src
      lappend lats [lindex $cos 0]
      lappend lons [lindex $cos 1]
      lappend coss $cos
   }

   #----- Parametres d'experience
   set Sim(Model)   NONE
   set Sim(State)   0
   set Sim(NoExp)   0
   set Sim(NoSim)   0
   set Sim(NoPrev)  -1
   set Sim(NameExp) $Data(Exp)

   #----- Supprime la precendente si elle existe
   set Sim(Name)   .*
   set Sim(Lat)    .*
   set Sim(Lon)    .*
   set Sim(Coords) .*

   #----- Backup pool
   if { [file exists $Param(Path)/$Data(Project)/sim.pool] } {
      file stat $Param(Path)/$Data(Project)/sim.pool attr
      file copy -force $Param(Path)/$Data(Project)/sim.pool $Param(Path)/$Data(Project)/sim.pool.[clock format $attr(ctime) -format "%Y%m%d" -timezone :UTC]
   
      Info::Delete $Param(Path)/$Data(Project)/sim.pool [Info::Code Watch::Sim] False
   }

   #----- On ajoute l'exxperience
   set Sim(Name)   $srcs
   set Sim(Lat)    $lats
   set Sim(Lon)    $lons
   set Sim(Coords) $coss

   exec echo "[Info::Code Watch::Sim]" >> $Param(Path)/$Data(Project)/sim.pool

   set Data(Lat)  [lindex $Sim(Lat) 0]
   set Data(Lon)  [lindex $Sim(Lon) 0]

   if { $Edit } {

      #----- Rebuild simulation pools with new source list
      foreach info [Info::List $Param(Path)/$Data(Project)/sim.pool] {

         Info::Decode ::Watch::Tmp $info

         #----- If this pool depends on the edited experiment, change its locations
         if { "$Tmp(NameExp)"=="$Sim(NameExp)" } {
            set Tmp(Name)   $Sim(Name)
            set Tmp(Lat)    $Sim(Lat)
            set Tmp(Lon)    $Sim(Lon)
            set Tmp(Coords) $Sim(Coords)
         }
         exec echo "[Info::Code ::Watch::Tmp]" >> $Param(Path)/$Data(Project)/sim.pool.edit
      }
      file rename -force $Param(Path)/$Data(Project)/sim.pool.edit $Param(Path)/$Data(Project)/sim.pool
   }

   Model::Check 0
   Model::TypeSelect none 2 "" $Data(Project)
   return 1
}

#-------------------------------------------------------------------------------
# Nom      : <Watch::Edit>
# Creation : Mai 2012 - J.P. Gauthier - CMC/CMOE
#
# But      : Modifier une Watch de la liste existante.
#
# Parametres:
#
# Retour :
#
# Remarques :
#
#-------------------------------------------------------------------------------

proc Watch::Edit { } {
   variable Data

   #----- Extract current experiment info for Model::New interface
   set Model::Data(Ids)    { }
   set Model::Data(Srcs)   { }
   set Model::Data(Coords) { }

   foreach src $Data(Sources$Data(Project)$Data(Exp)) {
      lappend Model::Data(Ids)    -
      lappend Model::Data(Srcs)   [lindex $src 0]
      lappend Model::Data(Coords) [lindex $src 3]
   }

   set Model::Data(Id)    [lindex $Model::Data(Ids) 0]
   set Model::Data(Src)   [lindex $Model::Data(Srcs) 0]
   set Model::Data(Coord) [lindex $Model::Data(Coords) 0]
   set Model::Data(Type)  $Data(Type)
   set Model::Data(Name)  $Data(Exp)
   set Model::Param(Unit) "DDD.CC"

   Model::New $Watch::Data(Frame) Watch::New False False
}

#----------------------------------------------------------------------------
# Nom      : <Watch::PopUpProject>
# Creation : Juillet 2009 - E. Legault-Ouellet - CMC/CMOE
#
# But      : Afficher le popup contextuel des projets.
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

proc Watch::PopUpProject { X Y } {
   global GDefs
   variable Data
   variable Lbl

   if { ![winfo exists .watchpopproj] } {

      menu .watchpopproj -tearoff 0 -bd 1 -type normal -activeborderwidth 1
         .watchpopproj add command -label ""  -command "" -background $GDefs(ColorHighLight) -activebackground $GDefs(ColorHighLight)
         .watchpopproj add command -label [lindex $Lbl(OpenBranch) $GDefs(Lang)] -command "Watch::ReadProject \$Watch::Data(Project); Watch::AllOpenItem \$Watch::Data(Project) 0"
         .watchpopproj add command -label [lindex $Lbl(CloseBranch) $GDefs(Lang)] -command "Watch::AllCloseItem \$Watch::Data(Project) 0"
         .watchpopproj add separator
         .watchpopproj add command -label [lindex $Lbl(New) $GDefs(Lang)] -command "Model::New \$Watch::Data(Frame) Watch::New False"
   }

   .watchpopproj entryconfigure 0 -label "$Data(Project)"

   tk_popup .watchpopproj $X $Y 0
}

#----------------------------------------------------------------------------
# Nom      : <Watch::PopUpWatch>
# Creation : Juillet 2009 - E. Legault-Ouellet - CMC/CMOE
#
# But      : Afficher le popup contextuel des Watchs.
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

proc Watch::PopUpWatch { X Y } {
   global   GDefs
   variable Data
   variable Lbl

   if { ![winfo exists .watchpopwatch] } {

      menu .watchpopwatch -tearoff 0 -bd 1 -type normal -activeborderwidth 1
         .watchpopwatch add command -label ""  -command "Model::TypeSelect none 2 \$Watch::Data(Exp) \$Watch::Data(Project); SPI::Locate \$Watch::Data(Lat) \$Watch::Data(Lon)" \
             -background $GDefs(ColorHighLight) -activebackground $GDefs(ColorHighLight)
         .watchpopwatch add command -label [lindex $Lbl(OpenBranch) $GDefs(Lang)] -command "Watch::AllOpenItem \$Watch::Data(Project)\$Watch::Data(Exp) 1"
         .watchpopwatch add command -label [lindex $Lbl(CloseBranch) $GDefs(Lang)] -command "Watch::AllCloseItem \$Watch::Data(Project)\$Watch::Data(Exp) 1"
         .watchpopwatch add separator
         .watchpopwatch add cascade -label [lindex $Lbl(New) $GDefs(Lang)] -menu .watchpopwatch.new
         .watchpopwatch add command -label [lindex $Lbl(Edit) $GDefs(Lang)] -command "Watch::Edit"
         .watchpopwatch add separator
         .watchpopwatch add command -label [lindex $Lbl(Suppress) $GDefs(Lang)] -command "Watch::Suppress"

      menu .watchpopwatch.new -tearoff 0 -bd 1 -type normal -activeborderwidth 1
      foreach model $Exp::Data(Models) {
         if { $model!="SATDATA" } {
            .watchpopwatch.new add command -label $model -command "Watch::ParamsWindow $model"
         }
      }
   }

   .watchpopwatch entryconfigure 0 -label "$Data(Exp)"
   tk_popup .watchpopwatch $X $Y 0
}

#----------------------------------------------------------------------------
# Nom      : <Watch::PopUpModel>
# Creation : Juillet 2009 - E. Legault-Ouellet - CMC/CMOE
#
# But      : Afficher le popup contextuel des modeles.
#
# Parametres    :
#     <X>      : ...
#     <Y>      : ...
#     <Model>  : Le modele.
#
# Retour :
#
# Remarques :
#
#----------------------------------------------------------------------------

proc Watch::PopUpModel { X Y Model } {
   global GDefs
   variable Data
   variable Lbl

   if { ![winfo exists .watchpopmodel] } {
      menu .watchpopmodel -tearoff 0 -bd 1 -type normal -activeborderwidth 1
            .watchpopmodel add command -label ""  -command "Model::TypeSelect none 2 \$Watch::Data(Exp) \$Watch::Data(Project); SPI::Locate \$Watch::Data(Lat) \$Watch::Data(Lon)" \
                -background $GDefs(ColorHighLight) -activebackground $GDefs(ColorHighLight)
            .watchpopmodel add command -label [lindex $Lbl(OpenBranch) $GDefs(Lang)] -command ""
            .watchpopmodel add command -label [lindex $Lbl(CloseBranch) $GDefs(Lang)] -command ""
            .watchpopmodel add separator
            .watchpopmodel add command -label [lindex $Lbl(New) $GDefs(Lang)] -command ""
   }

   .watchpopmodel entryconfigure 0 -label $Model
   .watchpopmodel entryconfigure 1 -command "Watch::AllOpenItem \$Watch::Data(Project)\$Watch::Data(Exp)$Model 2"
   .watchpopmodel entryconfigure 2 -command "Watch::AllCloseItem \$Watch::Data(Project)\$Watch::Data(Exp)$Model 2"
   .watchpopmodel entryconfigure 4 -command "Watch::ParamsWindow $Model"
   tk_popup .watchpopmodel $X $Y 0
}

#----------------------------------------------------------------------------
# Nom      : <Watch::PopUpSim>
# Creation : Aout 2001 - J.P. Gauthier - CMC/CMOE
#
# But      : Afficher le popup contextuel des simulations Watchs.
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

proc Watch::PopUpSim { X Y } {
   global   GDefs
   variable Data
   variable Lbl

   if { ![winfo exists .watchpopsim] } {

      menu .watchpopsim -tearoff 0 -bd 1 -type normal -activeborderwidth 1
         .watchpopsim add command -label "" -command "SPI::Locate \$Watch::Data(Lat) \$Watch::Data(Lon)" \
            -background $GDefs(ColorHighLight) -activebackground $GDefs(ColorHighLight)
         .watchpopsim add command -label [lindex $Lbl(Edit) $GDefs(Lang)] -command "Watch::ParamsWindow \$Watch::Data(Model) False"
         .watchpopsim add separator
         .watchpopsim add command -label [lindex $Lbl(Suppress) $GDefs(Lang)] -command "Watch::SimSuppress"
   }

   .watchpopsim entryconfigure 0 -label $Data(Exp)
   tk_popup .watchpopsim $X $Y 0
}

#----------------------------------------------------------------------------
# Nom      : <Watch::PopUpResult>
# Creation : Aout 2001 - J.P. Gauthier - CMC/CMOE
#
# But      : Afficher le popup contextuel des resultats d'une simulation.
#
# Parametres    :
#     <Result> : Path complet du dossier de la simulation
#     <Title>    : Titre du popup
#     <X>      : ...
#     <Y>      : ...
#
# Retour :
#
# Remarques :
#
#----------------------------------------------------------------------------

proc Watch::PopUpResult { Result Title X Y } {
   global   GDefs
   variable Data
   variable Lbl

   if { ![winfo exists .watchpopresult] } {

      menu .watchpopresult -tearoff 0 -bd 1 -type normal -activeborderwidth 1
         .watchpopresult add command -label "" -command "SPI::Locate \$Watch::Data(Lat) \$Watch::Data(Lon)" \
            -background $GDefs(ColorHighLight) -activebackground $GDefs(ColorHighLight)
         .watchpopresult add command -label [lindex $Lbl(Result) $GDefs(Lang)] -command ""
   }

   if { "$Data(Model)"=="TRAJECT" } {
      set files [glob -nocomplain $Result/results/*.points]
      .watchpopresult entryconfigure 1 \
         -command "SPI::FileOpen NEW TrajBox \"\" [list $FileBox::Type(TRAJ)] \"$files\" ;\
         SPI::Locate \$Watch::Data(Lat) \$Watch::Data(Lon)"
   } else {
      set files [glob -nocomplain $Result/results/*]
      .watchpopresult entryconfigure 1 \
         -command "SPI::FileOpen NEW FieldBox \"\" [list $FileBox::Type(FSTD)] \"$files\" ;\
         SPI::Locate \$Watch::Data(Lat) \$Watch::Data(Lon)"
   }

   if { [llength $files] } {
      .watchpopresult entryconfigure 1 -state normal
   } else {
      .watchpopresult entryconfigure 1 -state disabled
   }

   .watchpopresult entryconfigure 0 -label "$Title"
   tk_popup .watchpopresult $X $Y 0
}

#-------------------------------------------------------------------------------
# Nom      : <Watch::Select>
# Creation : Juillet 2009 - E. Legault-Ouellet - CMC/CMOE
#
# But      : Selectionne une watch.
#
# Parametres :
#     <Source>    : La ligne descriptive de la source (watch)
#     <Project>   : Le projet.
#
# Retour :
#
# Remarques :
#
#-------------------------------------------------------------------------------

proc Watch::Select { Exp Project } {
   variable Data

   set Data(Project) $Project
   set Data(Exp)     $Exp

   set Data(Lat)     [lindex $Data(Sources$Project$Exp) 0 1]
   set Data(Lon)     [lindex $Data(Sources$Project$Exp) 0 2]
   set Data(Type)    [Watch::GetType $Project]
}

#-------------------------------------------------------------------------------
# Nom      : <Watch::SelectBranch>
# Creation : Octobre 2000 - J.P. Gauthier - CMC/CMOE
# Modif    : Juillet 2009 - E. Legault-Ouellet - CMC/CMOE
#
# But      : Effectuer la selection de la branche.
#
# Parametres :
#     <Tag>    : Tag de la branche.
#     <Tree>   : Nom de la liste contenant le niveau de la branche a rajouter.
#     <Single> : Single branch active at a time mode.
#
# Retour :
#
# Remarques :
#     Il y a trois niveau de branche :
#        - BranchProject
#        - BranchWatch
#        - BranchModel
#
#-------------------------------------------------------------------------------

proc Watch::SelectBranch { Tag Tree { Single False } } {
   variable Data

   set idx [lsearch -exact $Data($Tree) $Tag]

   if { $idx==-1 } {
      if { $Single } {
         set Data($Tree) $Tag
      } else {
         lappend Data($Tree) $Tag
      }
   } else {
      set Data($Tree) [lreplace $Data($Tree) $idx $idx]

      while { [set idx [lsearch -exact $Data($Tree) $Tag]] != -1 } {
         set Data($Tree) [lreplace $Data($Tree) $idx $idx]
      }
   }
   Watch::CreateTree
}

#---------------------------------------------------------------------------
# Nom      : <Watch::SelectSim>
# Creation : Aout 2001 - J.P. Gauthier - CMC/CMOE
# Modif    : Juillet 2009 - E. Legault-Ouellet - CMC/CMOE
#
# But      : Selection d'une simulation.
#
# Parametres :
#     <Tag>       : Tag associe
#     <Model>     : Nom du model de la simulation
#     <Info>      : Ligne descriptive de la simulation (pool)
#     <Exp>     : Ligne descriptive de la source (watch)
#     <Project>   : Nom du projet
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc Watch::SelectSim { Model Info Exp Project } {
   variable Data

   Watch::Select "$Exp" $Project
   set Data(Model) $Model
   set Data(Modelbase) [string trimright $Model "01"]
   set Data(Info) "$Info"
}

#---------------------------------------------------------------------------
# Nom      : <Watch::SelectResult>
# Creation : Aout 2001 - J.P. Gauthier - CMC/CMOE
# Modif    : Juillet 2009 - E. Legault-Ouellet - CMC/CMOE
#
# But      : Selection d'une simulation.
#
# Parametres :
#     <Tag>       : Tag associe
#     <Model>     : Nom du model de la simulation
#     <Info>      : Ligne descriptive de la simulation (pool)
#     <Watch>     : Ligne descriptive de la source (watch)
#     <Project>   : Nom du projet
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc Watch::SelectResult { Tag Model Info Watch Project Result } {
   variable Data

   if { $Tag != ""} {
      eval $Data(Frame).info.canvas coords RESULTSELECT [$Data(Frame).info.canvas bbox $Tag]
   }

   Watch::SelectSim $Model "$Info" "$Watch" $Project
}

#-------------------------------------------------------------------------------
# Nom      : <Watch::SimEdit>
# Creation : Juillet 2009 - E. Legault-Ouellet - CMC/CMOE
#
# But      : Modifie une simulation.
#
# Parametres  :
#
# Retour :
#
# Remarques :
#
#-------------------------------------------------------------------------------

proc Watch::SimEdit { } {
   variable Data
   variable Param

   #----- Suppression de l'ancienne ligne de pool
   set path $Param(Path)/$Data(Project)/sim.pool
   Info::Delete $path "$Data(OldInfo)"

   #----- Ajout de la nouvelle ligne de pool
   Watch::Write $Data(Modelbase)
}

#-------------------------------------------------------------------------------
# Nom      : <Watch::SimSuppress>
# Creation : Juillet 2009 - E. Legault-Ouellet - CMC/CMOE
#
# But      : Supprimer une simulation.
#
# Parametres  :
#
# Retour :
#
# Remarques :
#
#-------------------------------------------------------------------------------

proc Watch::SimSuppress { } {
   global GDefs
   variable Param
   variable Data
   variable Lbl
   variable Msg

   #----- Demande de confirmation
   if { [Dialog::Default . 400 WARNING $Model::Msg(SimSuppress) "\n\n$Data(Exp)" 0 $Lbl(Yes) $Lbl(No)] } {
      return
   }

   #----- Suppression de la simulation dans le pool
   set path $Param(Path)/$Data(Project)/sim.pool
   Info::Delete $path "$Data(Info)"

   set model [Info::Strip $Data(Info) Model]
   set name  [Info::Strip $Data(Info) NameExp]
   set no    [Info::Strip $Data(Info) NoSim]

   #----- Supprimer tous les resultats pour la source
   foreach run [glob -nocomplain -type d -directory $Param(Path)/$Data(Project)/data -tails *] {    
      if { [llength [set files [glob -nocomplain $Param(Path)/$Data(Project)/data/$run/*_${name}/${model}.${no}*]]] } {
         eval file delete -force $files
      }
   }

   #----- Mise a jour de la liste des experiences
   Model::Check 0
   Model::TypeSelect none 2 $Data(Exp) $Data(Project)
}

#-------------------------------------------------------------------------------
# Nom      : <Watch::Suppress>
# Creation : Aout 2001 - J.P. Gauthier - CMC/CMOE
# Modif    : Juillet 2009 - E. Legault-Ouellet - CMC/CMOE
#
# But      : Supprimer une Watch.
#
# Parametres  :
#
# Retour :
#
# Remarques :
#
#-------------------------------------------------------------------------------

proc Watch::Suppress { } {
   global   GDefs
   variable Data
   variable Param
   variable Msg
   variable Lbl

   if { [Dialog::Default . 400 WARNING $Msg(Suppress) "\n\n$Data(Exp)" 0 $Lbl(Yes) $Lbl(No)] } {
      return
   }

   #----- Enlever la watch du pool
   set path $Param(Path)/$Data(Project)/sim.pool
   if { [file exists $path] } {
      file rename -force $path "$path.old"
      catch { exec egrep -i -v ".*:NameExp=$Data(Exp):.*" "$path.old" > $path }
   }

   #----- Supprimer tous les resultats pour la source
   if { ![Dialog::Default . 400 WARNING $Msg(SuppressData) "\n\n$Data(Exp)" 0 $Lbl(Yes) $Lbl(No)] } {
      foreach run [glob -nocomplain -type d -directory $Param(Path)/$Data(Project)/data -tails *] {     
         if { [llength [set files [glob -nocomplain $Param(Path)/$Data(Project)/data/$run/*_$Data(Exp)]]] } {
            eval file delete -force $files
         }
      }
   }

   #----- Relire les experiences
   Model::Check 0
   Model::TypeSelect none 2 "" $Data(Project)
}

#---------------------------------------------------------------------------
# Nom      : <Watch::Read>
# Creation : Juillet 2009 - E. Legault-Ouellet - CMC/CMOE
#
# But      : Lire la liste des projects.
#
# Parametres :
#
# Retour:
#
# Remarques :
#    - Chaque projet a sa liste de noms de watch sous Data(Sources$Projet$Exp)
#    - Chaque watch a sa liste de modeles sous Data(Models$Project$Watch)
#    - Chaque modele de chaque watch a sa liste de simulation sous Data(Sims$Project$Watch$Model)
#----------------------------------------------------------------------------

proc Watch::Read { } {
   variable Data
   variable Param

   set Data(Projects) [glob -nocomplain -types d -directory $Param(Path) -tails *]

   if { $Data(BranchProject)!="" } {
      Watch::ReadProject $Data(BranchProject)
   }
}

#---------------------------------------------------------------------------
# Nom      : <Watch::ReadProject>
# Creation : Juillet 2009 - E. Legault-Ouellet - CMC/CMOE
#
# But      : Lire la liste des sources/sims pour un projets.
#
# Parametres :
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc Watch::ReadProject { Project } {
   global GDefs
   variable Data
   variable Param
   variable Sim

   array unset Data Models*
   array unset Data Sims*

   set Model::Param(Job) "[lindex $Model::Lbl(Checking) $GDefs(Lang)]"
   .model config -cursor watch
   update idletask

   #----- Re-initialize arrays
   set Data(Exps$Project) {}

   foreach name [array names Data -glob Sources$Project*] {
      unset Data($name)
   }

   #----- Verifie que les fichiers necessaires sont presents
   if { [file exists $Param(Path)/$Project/sim.pool] } {

      #----- Trouve les lignes de pool
      foreach info [Info::List $Param(Path)/$Project/sim.pool] {
         Info::Decode ::Watch::Sim $info

         if { ![info exists Data(Sources$Project$Sim(NameExp))] } {
            set Data(Sources$Project$Sim(NameExp)) {}
         }

         foreach src $Sim(Name) lat $Sim(Lat) lon $Sim(Lon) coords $Sim(Coords) {
           lappend Data(Sources$Project$Sim(NameExp)) [list $src [format "%.6f" $lat] [format "%.6f" $lon] $coords]
         }
         lappend Data(Exps$Project) $Sim(NameExp)

         if { ![info exists Data(Models$Project$Sim(NameExp))] } {
            set Data(Models$Project$Sim(NameExp)) ""
         }
         if { [lsearch $Data(Models$Project$Sim(NameExp)) $Sim(Model)]==-1 } {
            lappend Data(Models$Project$Sim(NameExp)) $Sim(Model)
            set Data(Sims$Project$Sim(NameExp)$Sim(Model)) ""
         }
         lappend Data(Sims$Project$Sim(NameExp)$Sim(Model)) "$Sim(NoSim) \"$info\""

         #----- Remove double of sources since multiple pools have same sources
         set Data(Sources$Project$Sim(NameExp)) [lsort -unique -index 0 $Data(Sources$Project$Sim(NameExp))]
      }
      #----- Remove double of experiments since multiple pools have same experiment name
      set Data(Exps$Project) [lsort -unique $Data(Exps$Project)]
   }
   set Data(Type) [Watch::GetType $Project]

   set Model::Param(Job) ""
   .model config -cursor left_ptr
   update idletask
}

#-------------------------------------------------------------------------------
# Nom      : <Watch::Write>
# Creation : Juillet 2009 - E. Legault-Ouellet - CMC/CMOE
#
# But      : Inscrit les informations de simulation dans le pool
#
# Parametres :
#
# Retour :
#
# Remarques :
#
#-------------------------------------------------------------------------------

proc Watch::Write { Modelbase } {
   global GDefs
   variable Param
   variable Data
   variable Error

   upvar ${Modelbase}::Sim sim

   #----- Creation de la ligne de pool
   set pool [Info::Code ${Modelbase}::Sim]

   #----- Regarder s'il existe deja une ligne de pool concernant cette watch
   set path $Param(Path)/$Data(Project)/sim.pool
   set oldpool [Info::Find $path $Modelbase Model $sim(Model) NoSim $sim(NoSim) NameExp $sim(NameExp)]

   if { $oldpool!="" } {
      Info::Delete $path $oldpool
   }
   exec echo $pool >> $path

   #----- Relire les experiences
   Model::Check 0
}

#-------------------------------------------------------------------------------
# Nom      : <Watch::GetNo>
# Creation : Juillet 2009 - E. Legault-Ouellet - CMC/CMOE
#
# But      : Executes les operations necessaires a la creation d'une nouvelle
#            simulation.
#
# Parametres :
#     <Model>  : Le nom du modele de la simulation.
#
# Retour :
#
# Remarques :
#
#-------------------------------------------------------------------------------

proc Watch::GetNo { Model } {
   variable Data

   #----- Trouve un numero de simulation unique pour ce model
   set model $Data(Project)$Data(Exp)
   set sim   $Data(Project)$Data(Exp)$Model

   if { [info exists Data(Models$model)] && [info exists Data(Sims$sim)] } {
      set nos ""
      foreach sim $Data(Sims$sim) {
         lappend nos [lindex $sim 0]
      }
      set i 0
      while { [lsearch $nos $i] != -1 } {
         incr i
      }
      set Data(No) $i
   } else {
      set Data(No) 0
   }
}

#-------------------------------------------------------------------------------
# Nom      : <Watch::ParamsWindow>
# Creation : Juillet 2009 - E. Legault-Ouellet - CMC/CMOE
#
# But      : Affiche l'interface de creation/edition d'une simulation.
#
# Parametres :
#     <Model>  : Le nom du model de la simulation.
#     <New>    : 'True' s'il y a creation d'une nouvelle simulation
#                'False' si c'est une edition d'une simulation.
#
# Retour :
#
# Remarques :
#
#-------------------------------------------------------------------------------

proc Watch::ParamsWindow { Model { Mode NEW } } {
   global GDefs
   variable Lbl
   variable Msg
   variable Data

   if { [winfo exists .modelnew] } {
      Dialog::Info .modelnew $Model::Msg(Exist)
      return
   }

   set Data(Modelbase) [Model::InitNew $Model 0 $Data(Exp) $Data(Sources$Data(Project)$Data(Exp))]
   set Data(Model)     $Model

   if { $Mode=="NEW" } {
      Watch::GetNo $Model
   }

   toplevel     .modelnew
   wm title     .modelnew "Model $Model: $Watch::Data(Exp)"
   wm transient .modelnew .
   wm resizable .modelnew 0 0
   wm geom      .modelnew =300x350+[winfo rootx .]+[expr [winfo rooty .]+30]
   wm protocol  .modelnew WM_DELETE_WINDOW "Model::ParamsClose $Data(Modelbase)"

   TabFrame::Create .modelnew.params 1 $Data(Modelbase)::ParamsCheck
   pack .modelnew.params -side top -fill both -expand true -padx 5 -pady 5

   #----- Run parameters
   set $Data(Modelbase)::Sim(ReNewMeteo) "None"
   set $Data(Modelbase)::Sim(Auto) True

   $Data(Modelbase)::InitNew $Data(Type)
   catch { eval set ${Data(Modelbase)}::Sim(Grids) \[concat \$${Data(Modelbase)}::Sim(Grids) \$Param(Grids)\] }

   switch $Mode {
      "NEW" {
         $Data(Modelbase)::ParamsNew .modelnew.params True
         set $Data(Modelbase)::Sim(NoSim) $Data(No)
         set $Data(Modelbase)::Sim(Event) "AUTOMATED"
      }
      default {
         set Data(OldInfo) "$Data(Info)"
         Info::Decode $Data(Modelbase)::Sim "$Data(Info)"
         $Data(Modelbase)::ParamsNew .modelnew.params True
      }
   }

   if { [info proc ::${Data(Modelbase)}::ParamsEmission]!="" } {
      ${Data(Modelbase)}::ParamsEmission .modelnew.params $Mode
   }
   Model::ParamsGridDefine $Data(Modelbase) $Mode

   #----- Launching Tab.
   Watch::ParamsAutoWatch $Data(Modelbase) .modelnew.params $Mode

   button .modelnew.close -text [lindex $Lbl(Close) $GDefs(Lang)] -bd 1 -command "Model::ParamsClose $Data(Modelbase)"
   pack .modelnew.close -side bottom -anchor e -padx 5 -pady 5

   TabFrame::Select .modelnew.params 0
}

#-------------------------------------------------------------------------------
# Nom      : <Watch::ParamsAutoWatch>
# Creation : Juillet 2009 - E. Legault-Ouellet - CMC/CMOE
#
# But      : Cree le tab 'Automatisation' propre aux watchs.
#
# Parametres :
#     <Modelbase> : Le nom de base du modele (MLDP1 ou 0 serait MLDP)
#     <Frame>     : Le tag du Frame dans lequel ajouter le tab.
#     <Mode>      : 'True' s'il y a creation d'une nouvelle simulation
#                   'False' si c'est une edition d'une simulation.
#
# Retour :
#
# Remarques :
#
#-------------------------------------------------------------------------------

proc Watch::ParamsAutoWatch { Model Frame Mode } {
   global GDefs
   variable Lbl
   variable Bubble
   variable Data

   #----- Automatisation tab
   set tabframe [TabFrame::Add $Frame 1 "[lindex $Lbl(Auto) $GDefs(Lang)]" False]

   #----- Identification params
   labelframe $tabframe.request -text [lindex $Model::Lbl(Request) $GDefs(Lang)]
   Option::Create $tabframe.request.by    [lindex $Model::Lbl(By) $GDefs(Lang)]    ${Model}::Sim(By)    1 -1 $Model::Param(Bys) ""
   Option::Create $tabframe.request.event [lindex $Model::Lbl(Event) $GDefs(Lang)] ${Model}::Sim(Event) 1 -1 $Model::Param(Events) ""
   pack $tabframe.request.by $tabframe.request.event -side top -anchor w -padx 2 -fill x
   Bubble::Create $tabframe.request.by    $Model::Bubble(By)
   Bubble::Create $tabframe.request.event $Model::Bubble(Event)

   labelframe $tabframe.user -text [lindex $Model::Lbl(Id) $GDefs(Lang)]
   Option::Create $tabframe.user.id [lindex $Model::Lbl(User) $GDefs(Lang)] ${Model}::Sim(Blame) 1 -1 $Model::Param(Users) ""
   Bubble::Create $tabframe.user.id $Model::Bubble(User)
   pack $tabframe.user.id -side top -anchor w -padx 2 -fill x

   #----- Parametres
   labelframe $tabframe.params -text "[lindex $Lbl(Params) $GDefs(Lang)]"
   pack $tabframe.request $tabframe.user $tabframe.params -side top -padx 5 -pady 5 -fill x

   #----- Si c'est une edition de simulation (Il faut enlever l'ancienne ligne de pool avant d'ajouter la nouvelle)
   if { $Mode!="NEW" } {
      button $tabframe.add -text "[lindex $Lbl(EditSim) $GDefs(Lang)]" -bd 1 -command "Watch::SimEdit; Model::ParamsClose $Model"
   } else {
      button $tabframe.add -text "[lindex $Lbl(AddSim) $GDefs(Lang)]" -bd 1 -command "Watch::Write $Model; Model::ParamsClose $Model"
   }
   pack $tabframe.add -side bottom -anchor e -padx 5 -pady 5
}
