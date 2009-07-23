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
   variable LData

   #----- Variables relatives aux Watchs

   set Data(Models)        "CANERM TRAJECT SATDATA MLDP0 MLDP1 MLCD"       ;#Liste des modeles disponibles
   set Data(Frame)         ""
   set Data(ProjectsPath)  "/data/cmoex6/afsrelo/experiment/temp_Watch"    
   set Data(Select)        ""                                              ;#Experience selectionnee
   set Data(Types)         "VAAC RSMC CTBT FIRE BIO SPILL SPCL"            ;#Liste des types

   #----- Liste des branches ouvertes pour chacun des niveaux

   set Data(BranchWatch)   ""
   set Data(BranchModel)   ""
   set Data(BranchProject) ""
   set Data(BranchSim)     ""

   #----- Liste des produits possibles par projet dans l'onglet "Auto"

   set Data(ProjectVAAC)   "VAAC"
   set Data(ProjectRSMC)   "RSMC"
   set Data(ProjectCTBT)   "CTBT"
   set Data(ProjectFIRE)   "FIRE"
   set Data(ProjectBIO)    "BIO"
   set Data(ProjectSPILL)  "SPILL"
   set Data(ProjectSPCL)   "SPCL"

   #----- Labels

   set Lbl(New)         { "Nouveau" "New" }
   set Lbl(Yes)         { "Oui" "Yes" }
   set Lbl(No)          { "Non" "No" }
   set Lbl(Params)      { "Parametres" "Parameters" }
   set Lbl(Suppress)    { "Supprimer" "Delete" }
   set Lbl(Close)       { "Fermer" "Close" }
   set Lbl(Result)      { "Resultats" "Results" }
   set Lbl(Auto)        { "Automatisation" "Automatisation" }
   set Lbl(Prod)        { "Produit" "Product" }
   set Lbl(AddWatch)    { "Ajouter veille" "Add watch" }
   set Lbl(Edit)        { "Modifier" "Edit" }
   set Lbl(EditWatch)   { "Modifier veille" "Edit watch" }
   set Lbl(OpenBranch)  { "Ouvrir branches" "Open branches" }

   #--- Definitions des messages

   set Msg(Suppress)    { "Voulez-vous reellement desactiver cette veille ?"
                         "Do you really want to desactivate this watch ?" }
   set Msg(SimSuppress) { "Voulez-vous vraiment supprimer cette simulation ?"
                         "De you really want to suppress this simulation ?" }
   set Msg(Exists)      { "Cette fenetre est deja ouverte."
                         "This window is already openned!" }

   #----- Definitions des messages d'erreurs

   set Error(Exist)     { "Une source portant ce nom est deja activee."
                          "There is already an automated source by that name." }
   set Error(Info)      { "Certaine informations son manquantes."
                          "Missing information." }

   #----- Definitions des bulles d'aides

}

#-------------------------------------------------------------------------------
# Nom      : <Watch::AllClose>
# Creation : Juillet 2009 - E. Legault-Ouellet - CMC/CMOE
#
# But      : Fermer toutes les branches.
#
# Parametres :
#
# Retour :
#
# Remarques :
#
#-------------------------------------------------------------------------------

proc Watch::AllClose { } {
   variable Data

   set Data(BranchProject) ""
   set Data(BranchModel)   ""
   set Data(BranchWatch)   ""
   set Data(BranchSim)     ""
   Watch::CreateTree
}

#-------------------------------------------------------------------------------
# Nom      : <Watch::AllOpen>
# Creation : Juillet 2009 - E. Legault-Ouellet - CMC/CMOE
#
# But      : Ouvrir toutes les branches.
#
# Parametres :
#
# Retour :
#
# Remarques :
#
#-------------------------------------------------------------------------------

proc Watch::AllOpen { } {
   variable Data
   variable LData

   #----- On evite de rajouter une meme branche deux fois

   set Data(BranchProject) ""
   set Data(BranchModel)   ""
   set Data(BranchWatch)   ""
   set Data(BranchSim)     ""

   #----- On ajoute toutes les branches possibles

   foreach proj $Data(Projects) {
      lappend Data(BranchProject) $proj
      if { [info exists LData(proj-$proj)] } {

         foreach watch $LData(proj-$proj) {
            set name [lindex $watch 0]
            lappend Data(BranchWatch) $name
            if { [info exists LData($name)] } {

               foreach model $LData($name) {
                  lappend Data(BranchModel) $name$model

                  foreach item $LData($name$model) {
                     set no [lindex $item 0]
                     lappend Data(BranchSim) $name$model$no
                  }
               }
            }
         }
      }
   }

   Watch::CreateTree
}

#-------------------------------------------------------------------------------
# Nom      : <Watch::AllOpen>
# Creation : Juillet 2009 - E. Legault-Ouellet - CMC/CMOE
#
# But      : Ouvrir toutes les branchesi d'un projet en particulier.
#
# Parametres :
#     <Project>   : Le nom du projet dont il faut ouvrir les branches.
#
# Retour :
#
# Remarques :
#
#-------------------------------------------------------------------------------

proc Watch::AllOpenProject { Project } {
   variable LData
   variable Data

   if { [lsearch $Data(BranchProject) $Project] == -1 } {
      lappend Data(BranchProject) $Project
   }

   if { [info exists LData(proj-$Project)] } {

      foreach watch $LData(proj-$Project) {
         set name [lindex $watch 0]

         if { [lsearch $Data(BranchWatch) $name] == -1 } {
            lappend Data(BranchWatch) $name
         }

         if { [info exists LData($name)] } {

            foreach model $LData($name) {
               if { [lsearch $Data(BranchModel) $name$model] == -1 } {
                  lappend Data(BranchModel) $name$model

                  foreach item $LData($name$model) {
                     set no [lindex $item 0]
                     if { [lsearch $Data(BranchSim) $name$model$no] } {
                        lappend Data(BranchSim) $name$model$no
                     }
                  }
               }
            }
         }
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
         -scrollregion "1 1 280 7000" -width 200 -height 1
      scrollbar $Frame.info.scroll -orient vertical -bd 1 -width 10 \
         -command "$Frame.info.canvas yview"
      pack $Frame.info.canvas -side left -fill both -expand true
      pack $Frame.info.scroll -side left -fill y

      bind $Frame.info.canvas <Button-4> "$Frame.info.canvas yview scroll -1 units"
      bind $Frame.info.canvas <Button-5> "$Frame.info.canvas yview scroll 1 units"

   frame $Frame.opt
      button $Frame.opt.open -image PLUS -relief flat -bd 0 -overrelief raised -command "Watch::AllOpen"
      button $Frame.opt.close -image MINUS -relief flat -bd 0 -overrelief raised -command "Watch::AllClose"
      checkbutton $Frame.opt.bubble -image INFO -relief sunken -bd 1 -overrelief raised -offrelief flat -indicatoron False  -selectcolor $GDefs(ColorLight) \
         -onvalue 1 -offvalue 0 -variable CanvasBubble::Data(State$Frame.info.canvas) -command "CanvasBubble::Activate $Frame.info.canvas"
      button $Frame.opt.new -compound left -image BOMB -text [lindex $Lbl(New) $GDefs(Lang)] -relief flat -bd 0 -overrelief raised \
         -command "Model::New $Frame Watch::New \"[lindex $Lbl(New) $GDefs(Lang)]\" 0"
      pack $Frame.opt.open $Frame.opt.close $Frame.opt.bubble $Frame.opt.new -side left -padx 2

   pack $Frame.opt -side top -fill x -padx 2
   pack $Frame.info -side top -fill both -expand true  -padx 2 -pady 2

   Bubble::Create $Frame.opt.open   [lindex $Model::Bubble(Plus) $GDefs(Lang)]
   Bubble::Create $Frame.opt.close  [lindex $Model::Bubble(Minus) $GDefs(Lang)]
   Bubble::Create $Frame.opt.bubble [lindex $Model::Bubble(Bubble) $GDefs(Lang)]
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
   variable LData

   set canvas $Data(Frame).info.canvas
   set y 15
   set x 10

   #----- Cleanup du canvas

   $canvas delete TREE SIGN WATCH SIM SIMSELECT PROJECT MODEL RESULT
   $canvas create rectangle -10 -10 -10 -10 -outline black -fill $GDefs(ColorHighLight) -tags "SIMSELECT"

   #----- Creation des branches des differents projets

   foreach proj $Data(Projects) { 
      set y [Watch::CreateBranchProject $canvas $proj $x $y]
      incr y 21
   }

   $canvas itemconfigure -scrollregion "1 1 280 $y"
   puts "ERIC : le y est de >$y<"

   $canvas bind SIGN <Enter> "$canvas config -cursor hand1"
   $canvas bind SIGN <Leave> "$canvas config -cursor left_ptr"
}

#-------------------------------------------------------------------------------
# Nom      : <Watch::CreateBranchSim>
# Creation : Juillet 2009 - E. Legault-Ouellet - CMC/CMOE
#
# But      : Creer l'arborescence des watchs.
#
# Parametres :
#     <Canvas>    : Le Frame dans lequel dessiner l'arbre
#     <Project>   : Le nom du projet
#     <Watch>     : Une liste contenant les informations de la watch
#     <Name>      : Le nom de la watch
#     <Model>     : Le nom du model
#     <X>         : La position en X où commencer l'arborescence
#     <Y>         : La position en Y où commencer l'arborescence
#
# Retour : La position en Y où est rendu l'arborescence
#
# Remarques :
#
#-------------------------------------------------------------------------------

proc Watch::CreateBranchProject { Canvas Project X Y } {
   variable Data
   variable LData
   global GDefs

   set y1 [set y0 [expr $Y+10]]

   #----- Creation de la ligne descriptive du projet

   $Canvas create bitmap $X $Y -bitmap $Model::Resources(Plus) -tags "SIGN PPROJECT$Project"
   $Canvas create image [expr $X+20] $Y -image [lindex $Model::Resources(Icos) [Watch::GetType $Project]] -tags "PROJECT"
   $Canvas create text [expr $X+33] $Y -text "$Project" -anchor w -tags "PROJECT PROJECT$Project" -font $GDefs(Font) -fill black

   $Canvas bind PPROJECT$Project <ButtonPress-1> "Watch::SelectBranch $Project BranchProject"
   $Canvas bind PROJECT$Project  <ButtonPress-3> "set Watch::Data(Project) $Project ; Watch::PopUpProject %X %Y"

   #----- On cree les branches des watchs seulement s'il faut les afficher

   if { [lsearch $Data(BranchProject) $Project] != -1 } {
      $Canvas itemconfigure PPROJECT$Project -bitmap $Model::Resources(Minus)

      #----- On verifie qu'il y a quelque chose a afficher

      if { [info exists LData(proj-$Project)] } {

         foreach watch [lsort $LData(proj-$Project)] {
            set y1 [incr Y 21]
            set Y [Watch::CreateBranchWatch $Canvas $Project "$watch" [expr $X+20] $Y]
         }
         
         #----- Creation de la ligne verticale de l'arbre

         $Canvas create line $X $y0 $X $y1 -tags TREE
      }
   }
   
   return $Y
}



#-------------------------------------------------------------------------------
# Nom      : <Watch::CreateBranchModel>
# Creation : Juillet 2009 - E. Legault-Ouellet - CMC/CMOE
#
# But      : Creer l'arborescence des watchs.
#
# Parametres :
#     <Canvas>    : Le Frame dans lequel dessiner l'arbre
#     <Project>   : Le nom du projet
#     <Watch>     : Une liste contenant les informations de la watch
#     <Name>      : Le nom de la watch
#     <Model>     : Le nom du model
#     <X>         : La position en X où commencer l'arborescence
#     <Y>         : La position en Y où commencer l'arborescence
#
# Retour : La position en Y où est rendu l'arborescence
#
# Remarques :
#
#-------------------------------------------------------------------------------

proc Watch::CreateBranchWatch { Canvas Project Watch X Y } {
   variable Data
   variable LData
   global GDefs

   set name [lindex $Watch 0]
   set lat  [lindex $Watch 1]
   set lon  [lindex $Watch 2]
   set type [lindex $Watch 3]

   set y1 [set y0 [expr $Y+10]]

   #----- Creation de la ligne descriptive des watch

   $Canvas create bitmap $X $Y -bitmap $Model::Resources(Plus) -tags "SIGN PWATCH$name"
   $Canvas create text [expr $X+10] $Y -text "$name" -anchor w -tags "WATCH WATCH$name" -font $GDefs(Font) -fill black
   $Canvas create line [expr $X-20] $Y [expr $X-10] $Y -tags TREE

   CanvasBubble::Create $Canvas WATCH$name "Coord: ($lat , $lon)"
   $Canvas bind WATCH$name  <ButtonPress-3> "Watch::Select \"$Watch\" $Project ; Watch::PopUpWatch %X %Y"
   $Canvas bind PWATCH$name <ButtonPress-1> "Watch::SelectBranch $name BranchWatch"

   #----- On creer les branches des modeles seulement s'il faut les afficher

   if { [lsearch -exact $Data(BranchWatch) $name] != -1 } {
      $Canvas itemconfigure PWATCH$name -bitmap $Model::Resources(Minus)

      #----- On verifie qu'il y a des modeles a afficher

      if { [info exists LData($name)] } {

         foreach model [lsort $LData($name)] {
            set y1 [incr Y 21]
            set Y [Watch::CreateBranchModel $Canvas $Project "$Watch" $model [expr $X+20] $Y]
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
# But      : Creer l'arborescence des watchs.
#
# Parametres :
#     <Canvas>    : Le Frame dans lequel dessiner l'arbre
#     <Project>   : Le nom du projet
#     <Watch>     : Une liste contenant les informations de la watch
#     <Name>      : Le nom de la watch
#     <Model>     : Le nom du model
#     <X>         : La position en X où commencer l'arborescence
#     <Y>         : La position en Y où commencer l'arborescence
#
# Retour : La position en Y où est rendu l'arborescence
#
# Remarques :
#
#-------------------------------------------------------------------------------

proc Watch::CreateBranchModel { Canvas Project Watch Model X Y} {
   variable Data
   variable LData
   global GDefs

   set name [lindex $Watch 0]
   set y1 [set y0 [expr $Y+10]]

   #----- Creation de la ligne descriptive des modeles

   $Canvas create line [expr $X-20] $Y [expr $X-10] $Y -tags TREE
   $Canvas create text [expr $X+10] $Y -text $Model -font $GDefs(Font) -anchor w -tags "MODEL$name$Model MODEL"
   $Canvas create bitmap $X $Y -bitmap $Model::Resources(Plus) -tags "SIGN PMODEL$name$Model"
   $Canvas bind PMODEL$name$Model <ButtonPress-1> "Watch::SelectBranch $name$Model BranchModel"
   $Canvas bind MODEL$name$Model <ButtonPress-3> "Watch::Select \"$Watch\" $Project ; Watch::PopUpModel %X %Y $Model"

   #----- On creer les branches des simulations seulement s'il faut les afficher

   if { [lsearch -exact $Watch::Data(BranchModel) $name$Model] != -1 } {
      $Canvas itemconfigure PMODEL$name$Model -bitmap $Model::Resources(Minus)

      foreach item [lsort -index 0 $LData($name$Model)] {
         set y1 [incr Y 21]
         set Y [Watch::CreateBranchSim $Canvas $Project "$Watch" $Model "$item" [expr $X+20] $Y]
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
# But      : Creer l'arborescence des watchs.
#
# Parametres :
#     <Canvas>    : Le Frame dans lequel dessiner l'arbre
#     <Project>   : Le nom du projet
#     <Watch>     : Une liste contenant les informations de la watch
#     <Name>      : Le nom de la watch
#     <Model>     : Le nom du model
#     <X>         : La position en X où commencer l'arborescence
#     <Y>         : La position en Y où commencer l'arborescence
#
# Retour : La position en Y où est rendu l'arborescence
#
# Remarques :
#
#-------------------------------------------------------------------------------

proc Watch::CreateBranchSim { Canvas Project Watch Model Item X Y } {
   variable Data
   variable LData
   global GDefs

   set name [lindex $Watch 0]
   set info    [lindex $Item 1]
   set mode    [Info::Strip $info Mode]
   set meteo   [Info::Strip $info Meteo]
   set no      [lindex $Item 0]

   set txt "$meteo$mode ($no)"
   set tag "$name$Model$no"

   set y1 [set y0 [expr $Y+10]]

   #----- Creation de la ligne descriptive de la simulation

   $Canvas create text [expr $X+10] $Y -text "$txt" -anchor w -tags "SIM SIM$tag" -font $GDefs(Font)
   $Canvas create line [expr $X-20] $Y [expr $X-10] $Y -tags TREE
   $Canvas create bitmap $X $Y -bitmap $Model::Resources(Plus) -tags "SIGN PSIM$tag"

   $Canvas bind SIM$tag <ButtonPress-3> "Watch::SelectSim SIM$tag $Model \"$info\" \"$Watch\" $Project ; Watch::PopUpSim %X %Y"
   $Canvas bind SIM$tag <ButtonPress-1> "Watch::SelectSim SIM$tag $Model \"$info\" \"$Watch\" $Project"
   $Canvas bind PSIM$tag <ButtonPress-1> "Watch::SelectBranch $tag BranchSim"

   CanvasBubble::Create $Canvas SIM$tag "[Info::Format \"$info\"]"

   #----- On cree les branches des resultats seulement s'il faut les afficher

   if { [lsearch -exact $Watch::Data(BranchSim) $tag] != -1 } {
      $Canvas itemconfigure PSIM$tag -bitmap $Model::Resources(Minus)

      #----- On regarde s'il y a quelque chsoe a afficher
      
      if { [info exists LData(result-$tag)] && [llength $LData(result-$tag)] > 0} {
         foreach result [lsort $LData(result-$tag)] {
            set y1 [incr Y 21]
            set Y [Watch::CreateBranchResult $Canvas $Project "$Watch" $Model "$Item" $result $X $Y]
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
# But      : Creer l'arborescence des watchs.
#
# Parametres :
#     <Canvas>    : Le Frame dans lequel dessiner l'arbre
#     <Project>   : Le nom du projet
#     <Watch>     : Une liste contenant les informations de la watch
#     <Name>      : Le nom de la watch
#     <Model>     : Le nom du model
#     <X>         : La position en X où commencer l'arborescence
#     <Y>         : La position en Y où commencer l'arborescence
#
# Retour : La position en Y où est rendu l'arborescence
#
# Remarques :
#
#-------------------------------------------------------------------------------

proc Watch::CreateBranchResult { Canvas Project Watch Model Item Result X Y } {
   variable Data
   variable LData
   global GDefs

   set name [lindex $Watch 0]
   set info    [lindex $Item 1]
   set no      [lindex $Item 0]
   set result [split [file tail $Result] .]

   set date [lindex $result 2]
   set time [lindex $result 3]
   set txt "[string range $date 0 3]-[string range $date 4 5]-[string range $date 6 7] [string range $time 0 1]:[string range $time 2 3]"
   set tag "$name$Model$no[lindex $result 2][lindex $result 3]"

   $Canvas create text [expr $X+13] $Y -text "$txt" -anchor w -tags "RESULT RESULT$tag" -font $GDefs(Font)
   $Canvas create line $X $Y [expr $X+10] $Y -tags TREE

   return $Y
}

#-------------------------------------------------------------------------------
# Nom      : <Watch::GetType>
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

   set type ""

   switch $Project {
      "VAAC"  { set type 0 }
      "RSMC"  { set type 1 }
      "CTBT"  { set type 2 }
      "FIRE"  { set type 3 }
      "BIO"   { set type 4 }
      "SPILL" { set type 5 }
      "SPCL"  { set type 6 }
      default { set type 6 }
   }

   return $type
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

proc Watch::Select { Source Project } {
   variable Data

   set Data(Project) $Project
   set Data(Name)    [lindex $Source 0]
   set Data(Lat)     [lindex $Source 1]
   set Data(Lon)     [lindex $Source 2]
   set Data(Type)    [lindex $Source 3]
   set Data(Pos)     [lindex $Source 4]
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

proc Watch::New { } {
   global GDefs
   variable Data
   variable Error
   variable LData

   #----- Verifier la validitee des parametres

   if { $Model::Data(Name)=="" || $Model::Data(Lat1)=="" || $Model::Data(Lon1)=="" } {
       Dialog::CreateError .expnew [lindex $Error(Info) $GDefs(Lang)] $GDefs(Lang)
       return 0
   }

   if { "$Model::Param(Unit)" == "DDD MM" } {
      Model::SwitchCoord
   }

   #----- Recuperer les parametres

   set Data(Lat)  [format "%2.6f" $Model::Data(Lat1)]
   set Data(Lon)  [format "%2.6f" $Model::Data(Lon1)]
   set Data(Type) $Model::Data(Type)
   set Data(Pos)  $Model::Data(Pos)
   set Data(Name) $Model::Data(Name)
   set Data(Project) [lindex $Data(Types) $Data(Type)]
   set info ""
   lappend info [list $Model::Data(Name1) $Data(Lat) $Data(Lon) $Model::Data(Id1)]

   regsub -all "\[^a-zA-Z0-9\]" $Model::Data(Name) "_" Data(Name)
   regsub -all "\[-\]\[-\]*" $Data(Name) "_" Data(Name)
   set Data(Name) [string toupper $Data(Name)]

   #----- On verifie que le nom n'existe pas deja

   foreach proj $Data(Projects) {
      if { [info exists LData(proj-$proj)] } {
         foreach item $LData(proj-$proj) {
            if { "[lindex $item 0]"=="$Data(Name)" } {
               Dialog::CreateError . [lindex $Error(Exist) $GDefs(Lang)] $GDefs(Lang)
               return 0
            }
         }
      }
   }

   #----- On ajoute la source

   set line "$Data(Name) $Data(Lat) $Data(Lon) $Data(Type) { $info }"
   Debug::TraceProc "Adding automated source : $line"
   exec echo "$line" >> $Data(ProjectsPath)/$Data(Project)/sources.src

   Model::Check 0
   Model::TypeSelect none 2
   return 1
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
         .watchpopproj add command -label ""  -command "" \
             -background $GDefs(ColorHighLight) -activebackground $GDefs(ColorHighLight)
         .watchpopproj add separator
         .watchpopproj add command -label [lindex $Lbl(OpenBranch) $GDefs(Lang)] -command "Watch::AllOpenProject $Data(Project)"
         .watchpopproj add command -label [lindex $Lbl(New) $GDefs(Lang)] -command "Model::New \$Data(Frame) Watch::New \"[lindex $Lbl(New) $GDefs(Lang)]\" 0"
   }

   .watchpopproj entryconfigure 0 -label "$Data(Project)"

   .watchpopproj entryconfigure 3 -state disable

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
         .watchpopwatch add command -label ""  -command "Model::TypeSelect none 2 \$Watch::Data(Name); SPI::Locate \$Watch::Data(Lat) \$Watch::Data(Lon)" \
             -background $GDefs(ColorHighLight) -activebackground $GDefs(ColorHighLight)
         .watchpopwatch add cascade -label [lindex $Lbl(New) $GDefs(Lang)] -menu .watchpopwatch.new
         .watchpopwatch add separator
         .watchpopwatch add command -label [lindex $Lbl(Suppress) $GDefs(Lang)] -command "Watch::Suppress"

      menu .watchpopwatch.new -tearoff 0 -bd 1 -type normal -activeborderwidth 1
      foreach model $Data(Models) {
         .watchpopwatch.new add command -label $model -command "Watch::ParamsWindow $model"
      }
   }

   .watchpopwatch entryconfigure 0 -label "$Data(Name)"
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
            .watchpopmodel add command -label ""  -command "Model::TypeSelect none 2 \$Watch::Data(Name); SPI::Locate \$Watch::Data(Lat) \$Watch::Data(Lon)" \
                -background $GDefs(ColorHighLight) -activebackground $GDefs(ColorHighLight)
            .watchpopmodel add command -label [lindex $Lbl(New) $GDefs(Lang)] -command ""
   }

   .watchpopmodel entryconfigure 0 -label $Model
   .watchpopmodel entryconfigure 1 -command "Watch::ParamsWindow $Model"
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
         .watchpopsim add command -label [lindex $Lbl(Result) $GDefs(Lang)] -command ""
         .watchpopsim add separator
         .watchpopsim add command -label [lindex $Lbl(Suppress) $GDefs(Lang)] -command "Watch::SimSuppress"
   }

   .watchpopsim entryconfigure 0 -label $Data(Name)

   #----- Initialiser le menu
   set pattern "$Data(ProjectsPath)/$Data(Project)/data/*_$Data(Name)/[Info::Path $Data(Modelbase) $Data(Info)]/results/*"
   puts "ERIC : le super pattern est >>>>$pattern<<<<"
   if { ![catch { set files [glob $pattern] }] && $files != "" } {
      puts "ERIC : les files globbe sont : >>>>$files<<<<"
      .watchpopsim entryconfigure 3 -state normal
      if { "$Data(Model)"=="TRAJECT" } {
         .watchpopsim entryconfigure 3 -command "SPI::FileOpen NEW TrajBox \"\" [list $FileBox::Type(TRAJ)] $files"
      } else {
         .watchpopsim entryconfigure 3 -command "SPI::FileOpen NEW FieldBox \"\" [list $FileBox::Type(FSTD)] $files"
      }
   } else {
         .watchpopsim entryconfigure 3 -state disabled
   }

   tk_popup .watchpopsim $X $Y 0
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

proc Watch::SelectBranch { Tag Tree } {
   variable Data

   set idx [lsearch -exact $Data($Tree) $Tag]

   if { $idx == -1 } {
      lappend Data($Tree) $Tag
   } else {
      set Data($Tree) [lreplace $Data($Tree) $idx $idx]
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
#     <Watch>     : Ligne descriptive de la source (watch)
#     <Project>   : Nom du projet
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc Watch::SelectSim { Tag Model Info Watch Project} {
   variable Data

   if { $Tag != "" } {
      eval $Data(Frame).info.canvas coords SIMSELECT [$Data(Frame).info.canvas bbox $Tag]
   }

   Watch::Select "$Watch" $Project
   set Data(Model) $Model
   set Data(Modelbase) [string trimright $Model "01"]
   set Data(Info) "$Info"
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

   #----- Suppression de l'ancienne ligne de pool

   set path $Data(ProjectsPath)/$Data(Project)/sim.pool
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
   variable Data
   variable Lbl
   variable Msg

   #----- Demande de confirmation

   set nodel [Dialog::CreateDefault . 400 "Message" "[lindex $Msg(SimSuppress) $GDefs(Lang)]\n\n$Data(Name)" \
      warning 0 [lindex $Lbl(Yes) $GDefs(Lang)] [lindex $Lbl(No) $GDefs(Lang)]]

   if { $nodel } {
      return
   }

   #----- Suppression de la simulation dans le pool

   set path $Data(ProjectsPath)/$Data(Project)/sim.pool
   Info::Delete $path "$Data(Info)"

   #----- Mise a jour de la liste des experiences

   Model::Check 0
   Model::TypeSelect none 2
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
   variable Msg
   variable Lbl
   variable LData

   set nodel [Dialog::CreateDefault . 400 "Message" "[lindex $Msg(Suppress) $GDefs(Lang)]\n\n$Data(Name)" \
      warning 0 [lindex $Lbl(Yes) $GDefs(Lang)] [lindex $Lbl(No) $GDefs(Lang)]]

   if { $nodel } {
      return
   }

   #----- Supprimer la watch de la liste de sources

   Debug::TraceProc "Suppressing watch:  $Data(Name)"

   set path $Data(ProjectsPath)/$Data(Project)/sources.src

   file rename -force $path $path.old
   catch { exec grep -i -v $Data(Name).* $path.old > $path }
   exec chmod 664           $path
   exec chgrp $GDefs(Group) $path

   #----- Enlever la watch du pool

   if { [lsearch $LData(Watches) $Data(Name)] != -1 } {
      set path $Data(ProjectsPath)/$Data(Project)/sim.pool
      if { [file exists $path] } {
         file rename -force $path "$path.old"
         catch { exec egrep -i -v ".*$Data(Name).*" "$path.old" > $path }
      }
   }

   #----- Supprimer tous les resultats pour la source

   #set name [string tolower $Data(Name)]
   #catch { eval file delete -force [glob $Sim(TRAJPathData)/*${name}_*] }
   #catch { eval file delete -force [glob $Sim(CANPathData)/${name}_*] }
   #catch { eval file delete -force [glob $Sim(SATPathData)/${name}_*] }

   #----- Relire les experiences

   Model::Check 0
   Model::TypeSelect none 2
}

#---------------------------------------------------------------------------
# Nom      : <Watch::Read>
# Creation : Juillet 2009 - E. Legault-Ouellet - CMC/CMOE
#
# But      : Lire la liste des watchs.
#
# Parametres :
#
# Retour:
#
# Remarques :
#              - LData(Watches) Contient la liste des noms de toutes les watchs
#              - Chaque projet a sa liste de noms de watch sous LData(proj-$Projet)
#              - Chaque watch a sa liste de modeles sous LData($Watch)
#              - Chaque modele de chaque watch a sa liste de simulation sous
#                LData($Watch$Model)
#
#              La raison pour laquelle les projets ont 'proj-$proj' comme nom
#              est que cela permet ainsi de nommer une source du meme nom qu'un
#              projet sans probleme. Les nom ne pouvant contenir de caracteres
#              speciaux, sauf '_', le '-' evite tous conflit.
#
#----------------------------------------------------------------------------

proc Watch::Read { } {
   variable LData
   variable Data

   set Data(Projects) ""
   array unset LData *
   set LData(Watches) ""

   #----- Trouve la liste des noms de tous les projets (un projet = un dossier (directory))

   set Data(Projects) [exec ls -p $Data(ProjectsPath) | grep "/" | sed s:/::]

   #----- Traverse l'arborescence des projets pour trouver les pool voulus

   if { "$Data(Projects)" != "" } {
      foreach proj $Data(Projects) {

         #----- Verifie que les fichiers necessaires sont presents
         
         set path $Data(ProjectsPath)/$proj
         if { ![file exists $path/sim.pool] || ![file exists $path/sources.src] } {
            continue
         }

         #----- Trouve les sources de ce projet

         set LData(proj-$proj) ""
         set f [open $path/sources.src r]
         gets $f line
         while { ![eof $f] } {
            if { [string length $line] > 0 } {
               lappend LData(proj-$proj) "$line"
            }
            gets $f line
         }

         #----- Trouve les lignes de pool de ces sources

         set infos [Info::List $path/sim.pool]

         foreach info $infos {
            set model   [Info::Strip $info Model]
            set name    [Info::Strip $info NameExp]
            set nosim      [Info::Strip $info NoSim]

            if { [lsearch $LData(Watches) $name] == -1 } {
               lappend LData(Watches) $name
               set LData($name) ""
            }
            if { [lsearch $LData($name) $model] == -1 } {
               lappend LData($name) $model
               set LData($name$model) ""
            }
            lappend LData($name$model) "$nosim \"$info\"" 

            #----- Trouve tous les dossiers des resultats des simulations

            set LData(result-$name$model$nosim) [glob -nocomplain $path/data/*_$name/${model}.${nosim}.*]
         }
      }
   }
}

#-------------------------------------------------------------------------------
# Nom      : <Watch::SwitchElev>
# Creation : Octobre 1999 - J.P. Gauthier - CMC/CMOE
#
# But      : Faire la conversion en metre ou en millibar.
#
# Parametres :
#
# Retour :
#
# Remarques :
#
#-------------------------------------------------------------------------------

proc Watch::SwitchElev { } {
   variable Data

   if { [string compare $Data(TRAJUnit) "MILLIBARS"] == 0 } {

      set Data(TRAJUnit) "METRES"
      set Data(TRAJLevel1) [format "%0.1f" [Convert::Millibar2Meter $Data(TRAJLevel1)]]
      set Data(TRAJLevel2) [format "%0.1f" [Convert::Millibar2Meter $Data(TRAJLevel2)]]
      set Data(TRAJLevel3) [format "%0.1f" [Convert::Millibar2Meter $Data(TRAJLevel3)]]

   } else {

      set Data(TRAJUnit) "MILLIBARS"
      set Data(TRAJLevel1) [format "%0.1f" [Convert::Meter2Millibar $Data(TRAJLevel1)]]
      set Data(TRAJLevel2) [format "%0.1f" [Convert::Meter2Millibar $Data(TRAJLevel2)]]
      set Data(TRAJLevel3) [format "%0.1f" [Convert::Meter2Millibar $Data(TRAJLevel3)]]
   }
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
   variable Data
   variable Error

   upvar ${Modelbase}::Sim sim

   #----- Verification des valeurs
   

   #----- Creation de la ligne de pool 

   set pool [Info::Code ${Modelbase}::Sim $Modelbase :]

   #----- Regarder s'il existe deja une ligne de pool concernant cette watch

   set path $Data(ProjectsPath)/$Data(Project)/sim.pool
   set oldpool [Info::Find $path $Modelbase Model $sim(Model) NoSim $sim(NoSim) NameExp $sim(NameExp)]

   if { $oldpool != "" } {
      Info::Delete $path $oldpool
   }

   exec echo $pool >> $path
   exec chmod 664           $path
   exec chgrp $GDefs(Group) $path
   
   #----- Relire les experiences

   Model::Check 0
}

#-------------------------------------------------------------------------------
# Nom      : <Watch::InitNew>
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

proc Watch::InitNew { Model } {
   variable Data
   variable LData

   set Data(Modelbase) [Model::InitNew $Model -1 $Data(Name) $Data(Pos)]
   set Data(Model) $Model

   #----- Trouve un numero de simulation unique pour ce model

   set namemodel $Data(Name)$Model

   if { [info exists LData($Data(Name))] && [info exists LData($namemodel)] } {
      set nos ""
      foreach item $LData($namemodel) {
         lappend nos [lindex $item 0]
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

proc Watch::ParamsWindow { Model { New True } } {
   global GDefs
   variable Lbl
   variable Msg
   variable Data

   if { [winfo exists .modelnew] } {
      Dialog::CreateInfo .modelnew "[lindex $Msg(Exist) $GDefs(Lang)]"
      return
   }

   if { $New == True } {
      Watch::InitNew $Model
   } else {
      Model::InitNew $Model -1 $Data(Name) $Data(Pos)
   }

   toplevel     .modelnew
   wm title     .modelnew "Model $Model: $Watch::Data(Name)"
   wm transient .modelnew .
   wm resizable .modelnew 0 0
   wm geom      .modelnew =300x350+[winfo rootx .]+[expr [winfo rooty .]+30]
   wm protocol  .modelnew WM_DELETE_WINDOW "Model::ParamsClose $Data(Modelbase)"

   TabFrame::Create .modelnew.params 1 $Data(Modelbase)::ParamsCheck
   pack .modelnew.params -side top -fill both -expand true -padx 5 -pady 5

   #----- Run parameters

   set $Data(Modelbase)::Sim(ReNewMeteo) "None"
   $Data(Modelbase)::InitNew $Data(Type)

   if { $New == True } {
      $Data(Modelbase)::ParamsNew .modelnew.params
      if { [info procs ::$Data(Modelbase)::ParamsEmission] != "" } {
         $Data(Modelbase)::ParamsEmission .modelnew.params
      }
      Model::ParamsGridDefine $Data(Modelbase) NEW
      set $Data(Modelbase)::Sim(NoSim) $Data(No)
   } else {
      set Data(OldInfo) "$Data(Info)"
      Info::Decode $Data(Modelbase)::Sim $Data(Modelbase) "$Data(Info)"
      $Data(Modelbase)::ParamsNew .modelnew.params
      if { [info procs ::$Data(Modelbase)::ParamsEmission] != "" } {
         $Data(Modelbase)::ParamsEmission .modelnew.params
      }
   }

   #----- Launching Tab.

   Watch::ParamsAutoWatch $Data(Modelbase) .modelnew.params $New

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
#     <New>       : 'True' s'il y a creation d'une nouvelle simulation
#                   'False' si c'est une edition d'une simulation.
#
# Retour :
#
# Remarques :
#
#-------------------------------------------------------------------------------

proc Watch::ParamsAutoWatch { Modelbase Frame New } {
   upvar ${Modelbase}::Sim sim
   variable Lbl
   variable Bubble
   global GDefs
   variable Data

   #----- Automatisation tab

   set tabframe [TabFrame::Add $Frame 1 "[lindex $Lbl(Auto) $GDefs(Lang)]" False]
   labelframe $tabframe.params -text "[lindex $Lbl(Params) $GDefs(Lang)]"

   #----- Products

   set proj [lindex $Data(Types) $Data(Type)]
   set Data(Product) [lindex $Data(Project$proj) 0]
   Option::Create $tabframe.params.projet [lindex $Lbl(Prod) $GDefs(Lang)] Watch::Data(Project) 0 -1 $Data(Project$proj) "" 
   pack $tabframe.params.projet -side top -anchor w -padx 2 -fill x
   #Bubble::Create $tabframe.params.prod "[lindex $Bubble(Prod) $GDefs(Lang)]"
   button $tabframe.params.add -text "[lindex $Lbl(AddWatch) $GDefs(Lang)]" -bd 1 -command "Watch::Write $Modelbase ; Model::ParamsClose $Modelbase"
   pack $tabframe.params.add -side top -anchor e -padx 5 -pady 5

   pack $tabframe.params -side top -padx 5 -pady 5 -fill x

   #----- On change la commande du bouton add si c'est une edition de simulation
   #      (Il faut enlever l'ancienne ligne de pool avant d'ajouter la nouvelle)

   if { $New == False } {
      $tabframe.params.add configure \
         -command "Watch::SimEdit ; Model::ParamsClose $Modelbase" \
         -text [lindex $Lbl(EditWatch) $GDefs(Lang)]
   }
}

