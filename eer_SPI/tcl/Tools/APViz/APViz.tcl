#===============================================================================
# Environnement Canada
# Centre Meteorologique Canadian
# 2100 Trans-Canadienne
# Dorval, Quebec
#
# Projet   : Package d'interface pour SPI
# Fichier  : APViz.tcl
# Creation : Juin 2003
#
# Description:
#    Coquille vide demontrant les points d'entree pour la creation de nouveaux outils
#    a l'interface SPI. Ces fichiers representent la structure standard de SPI. Une
#    fonctionnalite de base de selection est implantee pour fin de demonstration.
#
#    Toutes les fonctions decrites sont le minimum necessaire au fonctionnement d'un
#    outils a travers l'interface SPI
#
#    Pour creer un nouvel outil, il suffit de renommer ces fichiers (tcl,int,txt,ctes) au nom
#    de l'outils que vous desirez et de remplacer "YourToolHere" et "yourtoolhere" par le
#    meme nom.
#
#    Par la suite il suffit d'inserer la ligne suivante dans le fichier $HOME/.spi/SPI
#
#       SPI::ToolDef <path>/APViz.tcl
#
#    et de modifier les references au <path> dans les 3 lignes ci-bas pour sourcer le tout.
#
#===============================================================================

#----- Lire les sources d'execution
source $GDefs(Dir)/tcl/Tools/APViz/APViz.ctes
source $GDefs(Dir)/tcl/Tools/APViz/APViz_Data.tcl
source $GDefs(Dir)/tcl/Tools/APViz/APViz.txt
source $GDefs(Dir)/tcl/Tools/APViz/APViz.int

#-------------------------------------------------------------------------------
# Nom      : <APViz::Close>
# Creation : Juin 2003 - J.P. Gauthier - CMC/CMOE -
#
# But      : Ferme l'interface de l'outil.
#
# Parametres :
#
# Retour:
#
# Remarques :
#
#-------------------------------------------------------------------------------

proc APViz::Close { } {
   variable Data

   #----- Si le mode etait celui de l'outils, revert to SPI

   if { $Page::Data(ToolMode)=="APViz" } {
      SPI::ToolMode SPI Zoom
   }

   #----- Cleanup de l'outils
   APViz::CloseFiles
   APViz::ReinitializeVP
   
   if {$Data(AutoUpdateEventID) ne ""} {
    after cancel $Data(AutoUpdateEventID)
    set Data(AutoUpdateEventID) ""
   }

   set Data(Active) 0

   $Data(Canvas) delete APVIZ

   destroy .apviz

   if { !$SPI::Param(Window) } { SPI::Quit }
}

#----------------------------------------------------------------------------
# Nom      : <APViz::Draw...>
# Creation : Juin 2003 - J.P. Gauthier - CMC/CMOE
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

proc APViz::DrawInit { Frame VP } {
   variable Data

   set Data(Lat0)   $Viewport::Map(LatCursor)
   set Data(Lon0)   $Viewport::Map(LonCursor)
}

proc APViz::Draw { Frame VP } {
   variable Data

   if { $Data(Canvas)!="" } {
      $Data(Canvas) delete APVIZ
   }

   set Data(Lat1)   $Viewport::Map(LatCursor)
   set Data(Lon1)   $Viewport::Map(LonCursor)

   set Data(Canvas) $Frame.page.canvas
   set Data(Frame)  $Frame
   set Data(VP)     $VP

   APViz::UpdateItems $Frame
}

proc APViz::DrawDone { Frame VP } {
   variable Data

   if { $Data(Lat0)>$Data(Lat1) } {
      set tmp $Data(Lat1)
      set Data(Lat1) $Data(Lat0)
      set Data(Lat0) $tmp
   }

   if { $Data(Lon0)>$Data(Lon1) } {
      set tmp $Data(Lon1)
      set Data(Lon1) $Data(Lon0)
      set Data(Lon0) $tmp
   }

   if { $Data(Lat0)==$Data(Lat1) || $Data(Lon0)==$Data(Lon1) } {
      set Data(Coo) ""
   } else {
      set Data(Coo) "$Data(Lat0),$Data(Lon0) - $Data(Lat1),$Data(Lon1)"
   }
}

proc APViz::MoveInit { Frame VP } {
   variable Data

   set Data(LonD) $Viewport::Map(LonCursor)
   set Data(LatD) $Viewport::Map(LatCursor)
}

proc APViz::Move { Frame VP } {
   variable Data

   #----- Effectuer la translation

   set lat0 [expr $Data(Lat0) + $Viewport::Map(LatCursor) - $Data(LatD)]
   set lat1 [expr $Data(Lat1) + $Viewport::Map(LatCursor) - $Data(LatD)]

   if { $lat0 > -90.0 && $lat0 < 90.0 && $lat1 > -90.0 && $lat1 < 90.0 } {

      set Data(Lat0) $lat0
      set Data(Lat1) $lat1
      eval set Data(Lon0) [Viewport::CheckCoord [expr $Data(Lon0) + $Viewport::Map(LonCursor) - $Data(LonD)]]
      eval set Data(Lon1) [Viewport::CheckCoord [expr $Data(Lon1) + $Viewport::Map(LonCursor) - $Data(LonD)]]
   }

   #----- Reaffecter le point de reference de translation

   if { $Data(Canvas)!="" } {
      $Data(Canvas) delete APVIZ
   }

   set Data(LonD)   $Viewport::Map(LonCursor)
   set Data(LatD)   $Viewport::Map(LatCursor)

   set Data(Canvas) $Frame.page.canvas
   set Data(Frame)  $Frame
   set Data(VP)     $VP

   APViz::UpdateItems $Frame
}

proc APViz::MoveDone { Frame VP } {
   variable Data

   APViz::DrawDone $Frame $VP
}

#-------------------------------------------------------------------------------
# Nom      : <APViz::Update>
# Creation : Juin 2003 - J.P. Gauthier - CMC/CMOE
#
# But      : Effectuer le "Refresh" de l'outils apres une mise a jour dans SPI
#
# Parametres :
#   <Frame>  : Identificateur de Page
#
# Remarques :
#    - Cette fonctions est appele par SPI au besoin.
#
#-------------------------------------------------------------------------------

proc APViz::Update { Frame } {
   variable Data
}

#-------------------------------------------------------------------------------
# Nom      : <APViz::UpdateItems>
# Creation : Juin 2003 - J.P. Gauthier - CMC/CMOE
#
# But      : Effectuer le "Refresh" des items relatifs a cet outils sur
#            la projection.
#
# Parametres :
#   <Frame>  : Identificateur de Page
#   <VP>     : Identificateur du Viewport
#
# Remarques :
#    - Cette fonctions est appele par SPI au besoin.
#
#-------------------------------------------------------------------------------

proc APViz::UpdateItems { Frame } {
   global   GDefs
   variable Data

   $Data(Canvas) delete APVIZ

   if { $Data(VP)!="" } {
      Viewport::DrawRange $Data(Frame) $Data(VP) $Data(Lat0) $Data(Lon0) $Data(Lat1) $Data(Lon1) APVIZ red
   }
}

#-------------------------------------------------------------------------------
# Nom      : <APViz::PageActivate>
# Creation : Octobre 2003 - J.P. Gauthier - CMC/CMOE
#
# But      : Effectuer le "Refresh" des items relatifs a cet outils
#            lors d'un changement de page par l'usager.
#
# Parametres :
#   <Frame>  : Identificateur de Page
#
# Remarques :
#    - Cette fonctions est appele par SPI au besoin.
#
#-------------------------------------------------------------------------------

proc APViz::PageActivate { Frame } {
}

#-------------------------------------------------------------------------------
# Nom      : <APViz::AsProject>
# Creation : Aout 2006 - J.P. Gauthier - CMC/CMOE
#
# But      : Sauvegarder l'etat de l'outils dans un projet SPI.
#
# Parametres :
#   <File>   : Descripteur de fichier ou ecrire les commandes
#
# Remarques :
#    - Le fichier est deja ouvert, il suffit d'y ecrire les commandes a executer
#      afin de re-instaurer l'outils dans son etat actuel.
#
#-------------------------------------------------------------------------------

proc APViz::AsProject { File } {
   variable Data
   variable Param

   if { [winfo exists .visualizer] } {
      puts $File "#----- Tool: APViz\n"
      puts $File "set APViz::Param(Dock)   $Param(Dock)"
      puts $File "set APViz::Param(Geom)   [winfo geometry .visualizer]"
      puts $File "APViz::Window"
      puts $File "\n"
   }
}

#-------------------------------------------------------------------------------
# Nom      : <APViz::MacroCategory>
# Creation : Mai 2018 - C. Nguyen - CMC/CMOE -
#
# But      : Changement des parametres de l'interface selon le type de grille.
#
# Parametres :
#
# Retour:
#
# Remarques :
#
#-------------------------------------------------------------------------------

proc APViz::MacroCategory { } {
   variable Data
   
   set macroCategory $Data(MacroCategory)
   if {[info exists Data($macroCategory,Files)]} {
    set Data(MacroTypes) $Data($macroCategory,Files)
   }
}


#-------------------------------------------------------------------------------
# Nom      : <APViz::AddToListBox>
# Creation : Mai 2018 - C. Nguyen - CMC/CMOE -
#
# But      : Ajouter (afficher) une liste d'elements a la liste
#
# Parametres 	:
#	<lst>	: Liste d'elements a ajouter dans la listBox 
#
# Retour:
#
# Remarques :
#
#-------------------------------------------------------------------------------

proc APViz::AddToListBox { lst } {
  variable Data

  set Data(MacroTypes) [split $lst]
}

#-------------------------------------------------------------------------------
# Nom      : <APViz::Source>
# Creation : Mai 2018 - C. Nguyen - CMC/CMOE -
#
# But      : Parse le fichier de config et afficher l'interface correspondante
#
# Parametres 	:
#	<Path>	: Path vers le fichier de config
#	<Widget>: Nom du widget parent dans lequel afficher l'interface
# Retour:
#
# Remarques :
#
#-------------------------------------------------------------------------------

proc APViz::Source { Path Widget } {
   variable Data
   set product [file rootname [file tail $Path]]

   #----- Create product namespace
   namespace eval $product {
      variable Layers
      variable Range
      variable DefaultValues
      variable Label
      variable Value
      variable Params
      variable RowID
      
      set Label(AddCalculation)	{ "Ajouter une couche de calcul" "Add calculation layer" }
      set Label(AddLayer)	{ "Ajouter une couche" "Add a Layer" }
      set Label(Calcul)		{ "Couches de calcul" "Calculation Layers" }
      set Label(Hour)		{ "Heure" "Hour" }
      set Label(Layer)		{ "Couches" "Layers" }
      set Label(Level)		{ "Niveau" "Level" }	
      set Label(Model)		{ "Data" "Data" }
      set Label(Source)		{ "Source" "Source" }
      set Label(Run)		{ "Run" "Run" }
      set Label(Type)		{ "Type" "Type" }
      set Label(Variable)	{ "NomVar" "VarName" }
      
      set Value(NbLayers)	0	; # Nombre de couches creees au total
      set Value(NbCalcLayers)	0	; # Nombre de couches de calcul
      
      set Value(Formula)	 ""	; # Formule de la couche de calcul (textvariable du entry pour la selection de formule)
      set Value(UneditedFormula) ""	; # Formule sans remplacement de variable
      
      set RowID(LayerAdjustment)	0	; # Ajustement pour le calcul du rowID pour les couches
      set RowID(CalcAdjustment)		0	; # Ajustement pour le calcul du rowID pour les couches de calcul
      
      #-------------------------------------------------------------------------------
      # Nom      : <APViz::$product::Load>
      # Creation : Mai 2018 - C. Nguyen - CMC/CMOE -
      #
      # But      : Lire le fichier de configuration et creer l'interface des ranges
      #
      # Parametres 	  :
      #		<Path>	  : Path vers le fichier de config
      #		<Widget>  : Nom du widget parent dans lequel afficher l'interface
      #		<Product> : Produit a afficher
      # Retour:
      #
      # Remarques :
      #
      #-------------------------------------------------------------------------------
      
      proc Load { Path Product Widget } {
         global GDefs
         variable Label
         variable Value
         variable Range
         variable Params
         
         #----- Default Geography parameters
         set Map(Cameras)    {}
         set Map(Projection) Orthographic

         #----- Source product definition
         source $Path
         
         #----- Get nb of VPs
         if {[info exists Params(ViewportNb)]} {
            set ::QuickLayout::Param(NbVP) $Params(ViewportNb)
            set ::APViz::Data(VPCount) $Params(ViewportNb)
         } else {       
            set ::QuickLayout::Param(NbVP) 1
            set ::APViz::Data(VPCount) 1
         }
         
         #----- Get frame params
         set ::QuickLayout::Data(Frame) ${::APViz::Data(Frame)}
         set ::QuickLayout::Param(Width)  [Page::CanvasWidth ${::APViz::Data(Frame)}]
         set ::QuickLayout::Param(Height) [Page::CanvasHeight ${::APViz::Data(Frame)}]
         ::QuickLayout::LayoutGrid
         
         
         if {[info exists Params(Projection)]} {
             eval projection configure ${::APViz::Data(Frame)} $Params(Projection)
         }

         if {[info exists Params(Viewport)]} {
            foreach vp [Page::Registered ${::APViz::Data(Frame)} Viewport] {
               eval ${::APViz::Data(Frame)}.page.canvas itemconfigure $vp $Params(Viewport)
            }
         }
         
         #----- Refleter les valeurs dans l'interface de configuration des parametres
         # TODO: GERER MULTIPLE VPS
         ::Viewport::ConfigGet ${::APViz::Data(Frame)} ${Viewport::Data(VP)}
         ::Viewport::ConfigPut ${::APViz::Data(Frame)} ${Viewport::Data(VP)}
      
         #----- Build product layer interface
         ::APViz::DeleteWidget $Widget.range	; # Liberer le widget
         
         labelframe $Widget.range -text [lindex $Label(Layer) $GDefs(Lang)]
         
         frame $Widget.range.variableGrid	; #Frame pour le grid
         
         #----- Column titles
         label $Widget.range.variableGrid.mod 	-text [lindex $Label(Model) $GDefs(Lang)]
         label $Widget.range.variableGrid.var 	-text [lindex $Label(Variable) $GDefs(Lang)]
         label $Widget.range.variableGrid.lev 	-text [lindex $Label(Level) $GDefs(Lang)] 
         label $Widget.range.variableGrid.src 	-text [lindex $Label(Source) $GDefs(Lang)]
         label $Widget.range.variableGrid.ip3   -text "IP3"
         label $Widget.range.variableGrid.vp    -text "VP"
         
         checkbutton $Widget.range.variableGrid.runLock -variable ::APViz::${Product}::Value(RunLock) -onvalue True -offvalue False \
               -text [lindex $Label(Run) $GDefs(Lang)] -indicatoron 0 -relief sunken -bd 1 -overrelief raised -offrelief flat -selectcolor IndianRed1
               
         checkbutton $Widget.range.variableGrid.hrLock -variable ::APViz::${Product}::Value(HourLock) -onvalue True -offvalue False \
               -text [lindex $Label(Hour) $GDefs(Lang)] -indicatoron 0 -relief sunken -bd 1 -overrelief raised -offrelief flat -selectcolor IndianRed1
               
         #----- Add help bubbles
         Bubble::Create $Widget.range.variableGrid.runLock ${APViz::Bubble(Lock)}
         Bubble::Create $Widget.range.variableGrid.hrLock ${APViz::Bubble(Lock)}
         
         grid $Widget.range.variableGrid 	-column 0 -row 1 -padx 0.2
         grid $Widget.range.variableGrid.mod 	-column 1 -row 0 -padx 0.2
         grid $Widget.range.variableGrid.runLock -column 2 -row 0 -padx 0.2
         grid $Widget.range.variableGrid.hrLock -column 3 -row 0 -padx 0.2
         grid $Widget.range.variableGrid.src 	-column 4 -row 0 -padx 0.2
         grid $Widget.range.variableGrid.var 	-column 5 -row 0 -padx 0.2
         grid $Widget.range.variableGrid.lev 	-column 6 -row 0 -padx 0.2
         grid $Widget.range.variableGrid.ip3    -column 7 -row 0 -padx 0.2
         grid $Widget.range.variableGrid.vp    -column 8 -row 0 -padx 0.2
         
         #----- Creation des couches         
         if {[info exists DefaultValues]} {
            #----- Default values exist
            if {[llength $Layers] eq [llength $DefaultValues]} {
               CreateLayers $Product $Layers $Widget $DefaultValues
            } else {
               CreateLayers $Product $Layers $Widget
            }
         } else {
            CreateLayers $Product $Layers $Widget
         }
         
         pack $Widget.range -side top -fill x -anchor nw

         set dateList [APViz::FetchAllDates $Product]
         set APViz::Data(Date) [lindex $dateList [expr [llength $dateList] - 1]]
         
         ::APViz::DeleteWidget $Widget.add	; # Liberer le widget
         
         menubutton $Widget.add -image PLUS -text [lindex $Label(AddLayer) $GDefs(Lang)] -compound left -bd 1 -menu $Widget.add.menu
         
         menu $Widget.add.menu
         set no 0
         foreach layer $Layers {
            #----- Layer description
            regsub \(True:|False:\) $layer "" desc
            $Widget.add.menu add command -label "Type$no: $desc" -command "APViz::${Product}::CreateLayers $Product $layer $Widget {} True $no"
            lappend ::APViz::Data(Layers) $layer		; #save layer configs
            incr no
         }
         $Widget.add.menu add separator
         $Widget.add.menu add command -label "Ajouter couche de calcul" -command "APViz::${Product}::AddCalcLayer $Product $Widget"
         
         pack $Widget.add -side top -padx 2 -pady 2 -anchor nw
         
         ::APViz::DeleteWidget $Widget.calc	; # Liberer le widget
         
         labelframe $Widget.calc -text [lindex $Label(Calcul) $GDefs(Lang)]
         pack $Widget.calc -side bottom -fill both -expand True
      }
      
      #-------------------------------------------------------------------------------
      # Nom      : <APViz::$product::CreateRangeWidget>
      # Creation : Mai 2018 - C. Nguyen - CMC/CMOE -
      #
      # But      : Creer un widget pour un range specifique. Peut etre soit un spinbox ou un combobox
      #
      # Parametres 	    :
      #		<Product>   : Produit a afficher (aussi le namespace)
      #		<Path>	    : Path vers le widget
      #		<Index>     : L'index de la couche
      #		<Options>   : Option (Colonne)
      #		<IsSpinbox> : Boolean indiquant si on cree un spinbox 
      #		<Width>	    : Width of the widget
      # Retour: La valeur par defaut
      #
      # Remarques :
      #
      #-------------------------------------------------------------------------------
      
      proc CreateRangeWidget { Product Style Path Index Options IsSpinBox Width Default} {
         variable Range
         variable Value
         
         if { [string index $Style 0] eq "<" } {
            set rangeType [string trim $Style {< >}]
            if $IsSpinBox {
               spinbox $Path -values $Range($rangeType) -width $Width -textvariable APViz::${Product}::Value($Options,$Index) \
               -command "::APViz::${Product}::AdjustLockedValues $Options $Index $Product ; APViz::AssignVariable $Product $Index " 
               
               #----- Bind with return key
               bind $Path <Return> "::APViz::${Product}::AdjustLockedValues $Options $Index $Product ; APViz::AssignVariable $Product $Index "
               
            } else {
               regsub .range\[a-z,A-Z,0-9,.,_\]* $Path "" widget	; # Pour la mise a jour des spinbox
               ComboBox::Create $Path APViz::${Product}::Value($Options,$Index) noedit unsorted nodouble -1 $Range($rangeType) $Width 6 \
               "APViz::AssignVariable $Product $Index ; ::APViz::${Product}::AdjustIDBubble $Path $Index ; ::APViz::${Product}::AdjustSpinboxValues $widget False"
            }
            
            #----- Default Values
            if {$Default eq ""} {
               set ranges [split $Range($rangeType)]
               if { [lindex $ranges 0] ne "" } {
                  set APViz::${Product}::Value($Options,$Index) [lindex [split $Range($rangeType)] 0]
               } else {
                  set APViz::${Product}::Value($Options,$Index) [lindex [split $Range($rangeType)] 1]      ; # In case first elem in Ranges (after split)is a space
               }
            } else {
               set APViz::${Product}::Value($Options,$Index) $Default
            }

         } else {
            label $Path -width $Width -text $Style -textvariable APViz::${Product}::Value($Options,$Index)
            if {$Default eq ""} {
               set APViz::${Product}::Value($Options,$Index) $Style
            } else {
               set APViz::${Product}::Value($Options,$Index) $Default
            }
            
         }
         eval set defaultValue \$APViz::${Product}::Value($Options,$Index)
         return $defaultValue
      }
      
      #-------------------------------------------------------------------------------
      # Nom      : <APViz::$product::CreateLayers>
      # Creation : Mai 2018 - C. Nguyen - CMC/CMOE -
      #
      # But      : Creer une couche pour ajouter au viewport
      #
      # Parametres 	        :
      #		<Product>       : Produit a afficher (aussi le namespace)
      #		<Layers>        : Information sur les ranges de la couche ayant la forme:
      #					On:Model:Var:Level:Hour:Interval:Run:Source
      #         <DefaultValues> : Valeurs par defauts les layers
      #		<Widget>        : Path vers le widget
      #		<IsAddedLayer>  : Boolean indiquant s'il s'agit d'une couche additionnelle
      # Retour:
      #
      # Remarques : Lorsque la variable est entre <>, cela signifie que c'est un range
      #
      #-------------------------------------------------------------------------------

      proc CreateLayers { Product Layers Widget { DefaultValues {} } {IsAddedLayer False} {LayerType 0}} {
         variable Value
         variable RowID
         
         set no $Value(NbLayers)
         foreach layer $Layers default $DefaultValues {
            #----- Extract layer parts
            lassign [split $layer :] toggle model run hour dataSrc var level ip3 vp
            lassign [split $default :] defaultToggle defaultModel defaultRun defaultHour defaultDataSrc defaultVar defaultLevel defaultIP3 defaultVP
            
            #----- Toggle On/Off
            checkbutton $Widget.range.variableGrid.layer${no}_toggle -anchor w -var APViz::${Product}::Value(Toggle,$no) \
               -command "APViz::Check $Product $no"
               
            if {$defaultToggle eq ""} {
               set defaultToggle $toggle
            }

            if {!$defaultToggle} {
               $Widget.range.variableGrid.layer${no}_toggle deselect
            } elseif {!$IsAddedLayer} {
               $Widget.range.variableGrid.layer${no}_toggle select
            }
            
            #----- CreateRangeWidget { Product Style Path Index Options IsSpinBox Width Default}
            CreateRangeWidget $Product $model   $Widget.range.variableGrid.layer${no}_model     $no Models true 5 $defaultModel
            CreateRangeWidget $Product $hour    $Widget.range.variableGrid.layer${no}_hour      $no Hours true 4 $defaultHour
            CreateRangeWidget $Product $run     $Widget.range.variableGrid.layer${no}_run       $no Runs true 3 $defaultRun
            CreateRangeWidget $Product $ip3  $Widget.range.variableGrid.layer${no}_ip3       $no IP3 true 2 $defaultIP3
            # ICIIII!!!
            CreateRangeWidget $Product $vp  $Widget.range.variableGrid.layer${no}_vp       $no VP false 2 $defaultVP
            set defaultVariable [CreateRangeWidget $Product $var     $Widget.range.variableGrid.layer${no}_var       $no Vars false -1 $defaultVar]
            set defaultSrc [CreateRangeWidget $Product $dataSrc $Widget.range.variableGrid.layer${no}_dataSrc   $no Sources false -1 $defaultDataSrc]
            
            #----- For DZ, level choices are LEV1-LEV2
            if {$defaultVariable eq "DZ"} {
               set levelWidth 8
            } else {
               set levelWidth 5
            }
            
            CreateRangeWidget $Product $level $Widget.range.variableGrid.layer${no}_level $no Levels false -1 $defaultLevel
            
            #----- Definir le numero de tab a ouvrir dans la fenetre de configuration
            set tab 1                                   ; # 1: Tab Champs
            if {[expr {"$defaultSrc" eq "BURP"}]} {
               set tab 2                                ; # 2: Tab Observations
            }
            
            button $Widget.range.variableGrid.layer${no}_delete -image DELETE -bd 1 -relief flat -overrelief raised -command "APViz::${Product}::DeleteLayer $Widget $no $Product $defaultSrc"
            button $Widget.range.variableGrid.layer${no}_param  -image PARAMS -bd 1 -relief flat -overrelief raised -command "APViz::SetParam $no $Product ; SPI::Params . $tab"
            
            set RowID(Layer$no) [expr $no - $RowID(LayerAdjustment)]
            
            #----- Place widgets in grid        
            set itemList [list toggle model run hour dataSrc var level ip3 vp param delete]
            set colNb 0
            foreach item $itemList {
               grid $Widget.range.variableGrid.layer${no}_$item      -column $colNb -row [expr $no + 1] -padx 0.1
               set fieldIDTemp FLD$RowID(Layer$no)_$defaultVariable
               Bubble::Create $Widget.range.variableGrid.layer${no}_$item $fieldIDTemp
               incr colNb
            }
            
            if {$IsAddedLayer} {
               APViz::AssignVariable $Product $no
               set APViz::${Product}::Value(LayerType,$no) $LayerType
            } else {
               set APViz::${Product}::Value(LayerType,$no) [expr $no - $Value(NbLayers)]
            }
            
            incr no
         }
         set Value(NbLayers) $no
         
         AdjustSpinboxValues $Widget False
      }

      #-------------------------------------------------------------------------------
      # Nom      : <APViz::$product::DeleteLayer>
      # Creation : Mai 2018 - C. Nguyen - CMC/CMOE -
      #
      # But      : Supprimer une couche et ajustement les numeros de rangees
      #
      # Parametres 	   :
      #		<Widget>   : Path vers le widget
      #		<Index>	   : L'index de la couche a supprimer
      #		<Product>  : Produit a afficher (aussi le namespace)
      # Retour:
      #
      # Remarques :
      #
      #-------------------------------------------------------------------------------
      
      proc DeleteLayer { Widget Index Product Src } {
         variable RowID
         variable Value
         
         #----- # Enlever variable du Viewport
         APViz::RemoveVariableFromVP ${::APViz::Data(LayerIDs)} $RowID(Layer$Index)
         
         #----- Ajustement du rowID
         incr RowID(LayerAdjustment)
         set itemList [list toggle model var level run hour dataSrc ip3 vp param delete]
            
         #----- Adjust all rowIds below range.variableGrid.layer${no}_toggle
         for {set i 0} {$i < $Value(NbLayers)} {incr i} {
            if {$RowID(Layer$i) > $RowID(Layer$Index)} {
               set RowID(Layer$i) [expr $RowID(Layer$i) - 1]
               #$Widget.range.variableGrid.layer${i}_rowID configure -text $RowID(Layer$i)
               APViz::AssignVariable $Product $i
               AdjustIDBubble $Widget.range.variableGrid.layer${i} $i		; # Change row id in bubble
            }
         }

         #----- Enlever la derniere variable de Data(LayerIDs) vu qu'on a decale tous les fielIds
         set lastIndex [expr [llength ${::APViz::Data(LayerIDs)}] - 1]
         if {$lastIndex != $RowID(Layer$Index)} {
            APViz::RemoveVariableFromVP ${::APViz::Data(LayerIDs)} $lastIndex		; # Unassign ssi l'index a supprimer n'est pas la derniere rangee
         }
         set oldRowID $RowID(Layer$Index)
         set RowID(Layer$Index) -1							; # Index supprime, n'est plus affiche
         set ::APViz::Data(LayerIDs) [lreplace ${::APViz::Data(LayerIDs)} $lastIndex $lastIndex ]
         
         #----- Detruire les widgets
         if [winfo exists $Widget.range.variableGrid] {	  
            foreach item $itemList {
               destroy $Widget.range.variableGrid.layer${Index}_$item
            }
         }
        
         AdjustSpinboxValues $Widget True $oldRowID
      }
      
      #-------------------------------------------------------------------------------
      # Nom      : <APViz::$product::DeleteCalcLayer>
      # Creation : Mai 2018 - C. Nguyen - CMC/CMOE -
      #
      # But      : Supprimer une couche de calcul et ajustement les numeros de rangees
      #
      # Parametres 	   :
      #		<Widget>   : Path vers le widget
      #		<Index>	   : L'index de la couche a supprimer
      #		<Product>  : Produit a afficher (aussi le namespace)
      # Retour:
      #
      # Remarques :
      #
      #-------------------------------------------------------------------------------
      
      proc DeleteCalcLayer { Widget Index Product } {
         variable RowID
         variable Value
         
         #----- Remove calc var from VP
         APViz::RemoveVariableFromVP ${::APViz::Data(CalcIDs)} $RowID(Calc$Index)	; # Enlever variable du Viewport
         
         incr RowID(CalcAdjustment)							; # Ajuster le rowID
            
         #----- Adjust all rowIds below range.variableGrid.layer${no}_toggle
         for {set i 0} {$i < $Value(NbCalcLayers)} {incr i} {
            if {$RowID(Calc$i) > $RowID(Calc$Index)} {
               set RowID(Calc$i) [expr $RowID(Calc$i) - 1]
               #----- Reattribute id with formula
               ::APViz::CalculateExpression $Product $i
            }
         }

         #----- Enlever la derniere variable de Data(CalcIDs) vu qu'on a decale tous les fielIds
         set lastIndex [expr [llength ${::APViz::Data(CalcIDs)}] - 1]
         if {$lastIndex != $RowID(Calc$Index)} {
            #----- Remove last index from VP
            APViz::RemoveVariableFromVP ${::APViz::Data(CalcIDs)} $lastIndex		; # Unassign ssi l'index a supprimer n'est pas la derniere rangee
         }
         set RowID(Calc$Index) -1							; # Index supprime, n'est plus affiche
         set ::APViz::Data(CalcIDs) [lreplace ${::APViz::Data(CalcIDs)} $lastIndex $lastIndex ]
         
         #----- Detruire les widgets
         ::APViz::DeleteWidget $Widget
      }
      
      #-------------------------------------------------------------------------------
      # Nom      : <APViz::$product::AdjustIDBubble>
      # Creation : Juin 2018 - C. Nguyen - CMC/CMOE -
      #
      # But      : Ajuster le message dans la bulle d'aide pour que le bon fieldID soit affiche
      #
      # Parametres 	   :
      #		<Widget>   : Path vers le widget
      #		<Index>	   : L'index de la couche
      #
      # Retour:
      #
      # Remarques :
      #
      #-------------------------------------------------------------------------------
      
      proc AdjustIDBubble { Widget Index } {
         variable RowID
         
         set itemList [list toggle model var level run hour dataSrc param delete]	
         set Widget [lindex [split $Widget _] 0]

         foreach item $itemList {
            if {[winfo exists ${Widget}_$item]} {
               Bubble::Create ${Widget}_$item [lindex ${::APViz::Data(LayerIDs)} $RowID(Layer$Index)]
            }
         }
      }
      
      #-------------------------------------------------------------------------------
      # Nom      : <APViz::$product::AdjustLockedValues>
      # Creation : Juin 2018 - C. Nguyen - CMC/CMOE -
      #
      # But      : Ajuster les runs et les heures si ces-derniers sont locked
      #
      # Parametres 	   :
      #		<Option>   : Nom de la colonne
      #		<Index>	   : L'index de la couche
      #		<Product>  : Produit a afficher (aussi le namespace)
      #
      # Retour:
      #
      # Remarques :
      #
      #-------------------------------------------------------------------------------
      
      proc AdjustLockedValues { Option Index Product } {
         variable Value
         variable RowID
         
         switch $Option {
            "Runs" 	{ set lockType Run }
            "Hours"	{ set lockType Hour }
            default 	{ return }
         }
         
         if {$Value(${lockType}Lock)} {
            set newValue $Value($Option,$Index)
            #----- Change all other values
            for {set i 0} {($i < $Value(NbLayers))} {incr i} {
               if {($i != $Index) && ($RowID(Layer$i) >= 0)} {
                  set Value($Option,$i) $newValue
                  APViz::AssignVariable $Product $i
               }
            }
         }
      }
      
      #-------------------------------------------------------------------------------
      # Nom      : <APViz::$product::AdjustSpinboxValues>
      # Creation : Juin 2018 - C. Nguyen - CMC/CMOE -
      #
      # But      : Ajuster les valeurs de la spinbox pour afficher les bons fieldIDs
      #
      # Parametres 	   :
      #		<Widget>   : Path vers le widget
      #
      # Retour:
      #
      # Remarques :
      #
      #-------------------------------------------------------------------------------
      
      proc AdjustSpinboxValues { Widget IsDueToLayerDeletion { DeletedPrevRowID 0 } } {
         variable Value
         variable RowID
         
         #----- Ajuster la valeur des spinbox pour les couches de calcul 
         for {set i 0} {$i < $Value(NbCalcLayers)} {incr i} {
            if {[winfo exists $Widget.calc.$i]} {
            
               #----- Recuperer les valeurs avant l'ajustement
               set a $Value(VarA,$i)
               set b $Value(VarB,$i)
               
               $Widget.calc.$i.varA configure -value ${::APViz::Data(LayerIDs)}
               $Widget.calc.$i.varB configure -value ${::APViz::Data(LayerIDs)}
               
               if {!$IsDueToLayerDeletion} {
                  #----- Values stay the same
                  set Value(VarA,$i) $a
                  set Value(VarB,$i) $b
               } else {
                  #----- Values only change if RowID of selected var has changed
                  
                  set values [list $a $b] 
                  set vars 	 [list A B]
                  
                  #TODO: Gerer pour 2 digits
                  foreach value $values var $vars {
                  
                     #----- If value points to deleted var, set value to first of layerIDs
                     if {[string match \[A-Z\]+${DeletedPrevRowID}_\[A-Z\]+ $value]} {
                        set Value(Var${var},$i) [lindex ${::APViz::Data(LayerIDs)} 0]
                     } else {
                        #----- Value will be affected only if rowNb is higher than deleted's
                        set lastIndex [expr [string first _ $value] - 1]
                        set rowNb [string range $value 3 $lastIndex]
                        if {$rowNb > $DeletedPrevRowID} {
                           set newRowID [expr $rowNb - 1]
                           if {$newRowID >= 0} {
                              set Value(Var${var},$i) [lindex ${::APViz::Data(LayerIDs)} $newRowID]
                           }
                        } else {
                           set Value(Var${var},$i) $value
                        }
                     }
                  }
               }
            }
         }
      }

      #-------------------------------------------------------------------------------
      # Nom      : <APViz::$product::AddManualCalcLayer>
      # Creation : Juin 2018 - C. Nguyen - CMC/CMOE -
      #
      # But      : Ajouter une couche de calcul manuel
      #
      # Parametres 	   :
      #		<Widget>   : Path vers le widget
      #		<Index>	   : L'index de la couche a supprimer
      #		<Product>  : Produit a afficher (aussi le namespace)
      # Retour:
      #
      # Remarques :
      #
      #-------------------------------------------------------------------------------
      
      proc AddCalcLayer { Product Widget } {
         variable Value
         variable RowID
         
         set no $Value(NbCalcLayers)
         frame $Widget.calc.$no
            checkbutton $Widget.calc.$no.check -anchor w -var APViz::${Product}::Value(CalcToggle,$no) -command "APViz::Check $Product $no True"
            
            Option::Create $Widget.calc.$no.formula "" "APViz::${Product}::Value(Formula,$no) APViz::${Product}::Value(Formula,$no)" 1 15 ${::APViz::Data(FormulaNames)} \
               "eval set APViz::${Product}::Value(UneditedFormula,$no) \${APViz::${Product}::Value(Formula,$no)} ; APViz::${Product}::SetFormula $no True ; APViz::CalculateExpression $Product $no" \
               ${::APViz::Data(Formulas)}
            
            set Value(UneditedFormula,$no) ""
            catch { Bubble::Create $Widget.calc.$no.formula.e "Formule"}
            
            bind $Widget.calc.$no.formula.e <Return> "eval set APViz::${Product}::Value(UneditedFormula,$no) \${APViz::${Product}::Value(Formula,$no)} ;\
               APViz::${Product}::SetFormula $no ; APViz::CalculateExpression $Product $no"	; # Calculer l'expression lorsque Return
            
            label $Widget.calc.$no.lblA	-text "A:"
            label $Widget.calc.$no.lblB	-text "B:"
            
            spinbox $Widget.calc.$no.varA	-values ${::APViz::Data(LayerIDs)} -width 7 -textvariable APViz::${Product}::Value(VarA,$no) -wrap True \
               -command "APViz::${Product}::SetFormula $no ; APViz::CalculateExpression $Product $no"
            spinbox $Widget.calc.$no.varB	-values ${::APViz::Data(LayerIDs)} -width 7 -textvariable APViz::${Product}::Value(VarB,$no) -wrap True \
               -command "APViz::${Product}::SetFormula $no ; APViz::CalculateExpression $Product $no"
               
            label $Widget.calc.$no.lblVP -text "VP:"
            set vpLst {}
            for {set i 1} {$i <= $APViz::Data(VPCount)} {incr i} {
               lappend vpLst $i
            }
            puts "LIST: $vpLst"
            spinbox $Widget.calc.$no.vp -values $vpLst -width 1 -textvariable APViz::${Product}::Value(CalcVP,$no) -wrap True \
               -command "APViz::CalculateExpression $Product $no"
            
            Bubble::Create $Widget.calc.$no.varA "VarA"
            Bubble::Create $Widget.calc.$no.varB "VarB"
            button $Widget.calc.$no.param -image PARAMS -bd 1 	-relief flat -overrelief raised -command "APViz::SetParam $no $Product True ; SPI::Params . 1"
            button $Widget.calc.$no.delete -image DELETE -bd 1 	-relief flat -overrelief raised -command "APViz::${Product}::DeleteCalcLayer $Widget.calc.$no $no $Product"
            
            set RowID(Calc$no) [expr $no - $RowID(CalcAdjustment)]
            
            pack $Widget.calc.$no.check $Widget.calc.$no.formula $Widget.calc.$no.lblA $Widget.calc.$no.varA \
               $Widget.calc.$no.lblB $Widget.calc.$no.varB $Widget.calc.$no.lblVP $Widget.calc.$no.vp $Widget.calc.$no.param $Widget.calc.$no.delete -side left -fill x -expand true
            
         pack $Widget.calc.$no -side top -fill x
         incr Value(NbCalcLayers)
         
         #----- Ajouter le ID a la liste
         lappend ${::APViz::Data(CalcIDs)} CALC$RowID(Calc$no)
      }
      
      #-------------------------------------------------------------------------------
      # Nom      : <APViz::$product::SetFormula>
      # Creation : Juin 2018 - C. Nguyen - CMC/CMOE -
      #
      # But      : Remplace les variables A et B par les valeurs correspondantes
      # Parametres 	 :
      #		<Index>	 : L'index de la couche de calcul
      #
      # Retour:
      #
      # Remarques :
      #
      #-------------------------------------------------------------------------------
      
      proc SetFormula { Index {IsInit False}} {
         variable Value
         #----- Verifier si un calcul a ete selectionne
         if {[info exists Value(UneditedFormula,$Index)]} {
            #----- Verifier si on initialise la formule a partir de formules predefinies
            if {$IsInit} {
               if {![regexp ALL $Value(UneditedFormula,$Index)]} {
                  regsub A $Value(UneditedFormula,$Index) $Value(VarA,$Index) Value(Formula,$Index)		; # TODO: Gerer le cas ou presence de ALL et A
               }
               regsub B $Value(Formula,$Index) $Value(VarB,$Index) Value(Formula,$Index)
            } else {
               #----- Ordre : A toujours avant B
               set IDRegExpr \(\(FLD\)|\(OBS\)\)\[0-9\]+\(_\)\[A-Z\]+
               set IDCount [regexp -all $IDRegExpr $Value(Formula,$Index)]
               set subFormula ""
               
               #----- On tient compte de 2 variables slmnt : A et B
               if {$IDCount >= 1} {
                  #----- Remplacer le A
                  regsub $IDRegExpr $Value(Formula,$Index) $Value(VarA,$Index) Value(Formula,$Index)
                  
                  #----- Recuperer les indices de A et retirer A de la formule pour trouver les indices de B
                  regexp -indices $IDRegExpr $Value(Formula,$Index) indexesA
                  set subFormula [string range $Value(Formula,$Index) [lindex $indexesA 1] [string length $Value(Formula,$Index)]]
               }
               
               if {$IDCount == 2} {
                  if {$subFormula ne ""} {
                     #----- Trouver les indices de B
                     regexp -indices $IDRegExpr $subFormula indexesB
                     set startIndex [expr [lindex $indexesB 0] + [lindex $indexesA 1]]
                     
                     #----- Remplacer B dans la formule complete
                     regsub -start $startIndex $IDRegExpr $Value(Formula,$Index) $Value(VarB,$Index) Value(Formula,$Index)	 
                  }
               }
            }
         }
      }
   }
   
   set Data(CurrentProduct) $product
   ${product}::Load $Path $product $Widget
   
   set Data(AutoUpdateEventID) [after [expr {1000*60*10}] APViz::UpdateAvailableDates $product]	; # Update a chaque 10min:1000*60*10
   
   APViz::InitializeVars
}

#-------------------------------------------------------------------------------
# Nom      : <APViz::AssignVariable>
# Creation : Mai 2018 - C. Nguyen - CMC/CMOE -
#
# But      : Ajouter (afficher) une liste d'elements a la liste
#
# Parametres 		:
#	<Product>	: Le nom du produit selectionne (aussi le namespace) 
#	<Index>		: Index de la couche 
#	
# Retour:
#
# Remarques :
#
#-------------------------------------------------------------------------------

proc APViz::AssignVariable { Product Index } {
   global env
   variable Data
   variable DataSrc
   variable Lbl
   variable ${Product}::Value
   variable ${Product}::Params
   variable ${Product}::RowID

   #----- Get layer values
   set model	$Value(Models,$Index)
   set var	$Value(Vars,$Index)
   set lev	$Value(Levels,$Index)
   set run	$Value(Runs,$Index)
   set hour	$Value(Hours,$Index)
   set src	$Value(Sources,$Index)
   set vp       $Value(VP,$Index)
   #set date	[clock format [clock seconds] -format %Y%m%d]				; # Today's date in format AAAAMMDD
   
   set date $Data(Date)

   if {![APViz::ValidateDate $date]} {
      return
   }
   
   #----- Verifier si tous les champs sont remplis
   if {[ APViz::AreFieldsFilled $model $var $lev $run $hour $src $date ]} {
      set vpID [APViz::GetVPId $vp]
   
      if {$src eq "BURP"} {
         set timestamp ${date}${run}_
         set filepath $DataSrc(OBS,$model)/$timestamp
         puts "BURP Filepath for $var: $filepath"
         
         #----- Liberer l'observation
         if {[metobs is [lindex $Data(LayerIDs) $RowID(Layer$Index)]]} {
            puts "Removing [lindex $Data(LayerIDs) $RowID(Layer$Index)] from vp"
            APViz::RemoveVariableFromVP $Data(LayerIDs) $RowID(Layer$Index)
         }

         set obsID OBS$RowID(Layer$Index)_${var}
         
         #----- In case id alreayd exists
         if {[metobs is $obsID]} {
            metobs free $obsID
         }
         
         metobs create $obsID $filepath
         if {$Data(PR_timestamp) ne ""} {
            metobs define $obsID -VALID $Data(PR_timestamp) 0
         }

         dataspec create $obsID
         
         if { [info exist Params(${var}$lev)] } {
            catch { 
               eval dataspec configure $obsID $Params(${var}$lev)
               puts "Configured OBS: $var"
            }
         } elseif { [info exist Params($var)] } {
            catch { 
               eval dataspec configure $obsID $Params($var) 
            }
         } else {
            #----- Configurations par defaut
            dataspec configure $obsID -size 10 -icon CIRCLE -color black -colormap COB_Seq_MHue_RdPu \
               -mapall True -rendertexture 1 -rendercontour 1 -rendervalue 1 -font XFont12 -intervals { 1 5 10 15 20 30 40 50 75 100 125 150 200 } -active $Value(Toggle,$Index)
         }
         
         dataspec configure $obsID -desc "$model (${timestamp}_)" -active $Value(Toggle,$Index)

         set lst [list [list 0 0 $var { }]]
         metmodel define [metobs define $obsID -MODEL] -items $lst -spacing 10
         metmodel configure [metobs define $obsID -MODEL] $var -dataspec $obsID
         set Data(LayerIDs) [lreplace $Data(LayerIDs) $RowID(Layer$Index) $RowID(Layer$Index) $obsID]
         
         if {[lsearch -exact [Viewport::Assigned $Data(Frame) $vpID] $obsID] eq -1} {
            Viewport::Assign $Data(Frame) $vpID $obsID 1
         }      
      } else {
                  
         #----- Pas un fichier BURP
         set timestamp ${date}${run}_$hour	
         set filepath $DataSrc($model,$src)/$timestamp					; # Format: AAAAMMDDRR_HHH
         set fileID FILE_${model}_${src}_$timestamp
         
         #----- Verifier la validite du fichier standard
         if {[fstdfile is $filepath]} {
            if {[catch { fstdfile open $fileID read $filepath }] == 0} {
               lappend Data(OpenedFiles) $fileID
               puts "STANDARD FILE - Opening $fileID	$filepath"
            }
            
            if {[fstdfield is [lindex $Data(LayerIDs) $RowID(Layer$Index)]]} {
               APViz::RemoveVariableFromVP $Data(LayerIDs) $RowID(Layer$Index)  	; # Enlever la variable courante du VP pour cette couche
            }
            
            set levelType [ APViz::GetLevelType $src ]
            set fieldID FLD$RowID(Layer$Index)_${var}
            
            switch $var {
               "DZ"     { APViz::AssignDZ $Product $Index $model $var $lev $fileID $fieldID $levelType }
               
               "PR"     { 
                           if {[catch {fstdfield read $fieldID $fileID -1 "" -1 -1 $Value(IP3,$Index) "" $var }]} {
                              ::Dialog::Info . $Lbl(InvalidField)
                              set Data(LayerIDs) [lreplace $Data(LayerIDs) $RowID(Layer$Index) $RowID(Layer$Index) FLD$RowID(Layer$Index)]
                              return
                           } else {
                              fstdfield configure $fieldID -factor 1e3
                              set Data(PR_timestamp) [fstdstamp toseconds [fstdfield define $fieldID -DATEV]]
                              set Data(LayerIDs) [lreplace $Data(LayerIDs) $RowID(Layer$Index) $RowID(Layer$Index) $fieldID]
                           }
                        }
               
               default  {  
                           if {[catch {fstdfield read $fieldID $fileID -1 "" [subst {$lev $levelType}] -1 -1 "" $var }]} {
                              ::Dialog::Info . $Lbl(InvalidField)
                              set Data(LayerIDs) [lreplace $Data(LayerIDs) $RowID(Layer$Index) $RowID(Layer$Index) FLD$RowID(Layer$Index)]
                              return
                           } else {
                              set Data(LayerIDs) [lreplace $Data(LayerIDs) $RowID(Layer$Index) $RowID(Layer$Index) $fieldID]
                           }
                        }
            }

            #----- In case DZ assignation failed
            if {![fstdfield is $fieldID]} {
               return
            }
            
            #----- Verify if a variable param config has been saved for this row
            if {[dict exists $Data(VarParamsDict) ${var}$RowID(Layer$Index)]} {
               set index [dict get $Data(VarParamsDict) ${var}$RowID(Layer$Index)]
            } else {
               set index [APViz::GetVarsNb Data(VarsDict) $var]
               dict set Data(VarParamsDict) ${var}$RowID(Layer$Index) $index
            }
            
            #----- Increment number of vartype
            dict incr Data(VarsDict) $var
            
            #----- Apply variable configs from config file 
            if { [info exist Params(${var}$index)] } {
               catch { 
                  eval fstdfield configure $fieldID $Params(${var}$index)
               }
            } elseif { [info exist Params($var)] } {
               catch { 
                  eval fstdfield configure $fieldID $Params($var)
               }
            }
            
            #----- Si la colormap n'existe pas deja, creer la bonne colormap
            set colormapName $var$Index
            if {[lsearch -exact $Data(Colormaps) $colormapName] < 0} {
               set configColormap [fstdfield configure $fieldID -colormap]
               colormap create $colormapName
               colormap copy $colormapName $configColormap
               
               #----- Ajouter dans la liste des colormaps pour retrouver l'original
               lappend Data(ColormapPairs) [list $colormapName $configColormap]
               lappend Data(Colormaps) $colormapName
            }
            
            fstdfield configure $fieldID -colormap $colormapName
            fstdfield configure $fieldID -active $Value(Toggle,$Index)
            
            #----- Assigner seulement si n'est pas assigne
            if {[lsearch -exact [Viewport::Assigned $Data(Frame) $vpID] $fieldID] eq -1} {
               Viewport::Assign $Data(Frame) $vpID $fieldID 1
            }

         } else {
            puts "File $filepath not available."
            set Data(LayerIDs) [lreplace $Data(LayerIDs) $RowID(Layer$Index) $RowID(Layer$Index) FLD$RowID(Layer$Index)]
            
            #----- Concatener le path du fichier au message d'erreur
            set messages {}
            foreach msg $Lbl(InvalidFile) {
               lappend messages ${msg}$filepath
            }
            ::Dialog::Info . $messages
         }
      }
   } else {
      puts "Missing values"
   }
}

#-------------------------------------------------------------------------------
# Nom      : <APViz::GetVPId>
# Creation : Juillet 2018 - C. Nguyen - CMC/CMOE -
#
# But      :    Retourner l'id du VP
#
# Parametres      :
#       <VPno>    : Numero du vp lu dans le fichier de config
#       
# Retour:
#
# Remarques :
#
#-------------------------------------------------------------------------------

proc APViz::GetVPId { VPno } {
   variable Data
   
   if {[expr {$Data(VPCount) eq 1}] || [expr $VPno > $Data(VPCount)] || [expr $VPno < 0]} {
      return $Viewport::Data(VP)
   } else {
      set vpNumber VP[expr $Viewport::Data(VPNb) - [expr $Data(VPCount) - $VPno]]
      return $vpNumber
   }
} 

#-------------------------------------------------------------------------------
# Nom      : <APViz::GetVarsNb>
# Creation : Juillet 2018 - C. Nguyen - CMC/CMOE -
#
# But      :    Retourner le nombre de variables assignees au VP d'un type
#
# Parametres      :
#       <Dict>    : Dictionnaire contenant le nombre de varaible pour chaque type de variable
#       <VarType> : Type de variable (ex: GZ, TT, UU)
#       
# Retour:
#
# Remarques :
#
#-------------------------------------------------------------------------------

proc APViz::GetVarsNb { Dict VarType } {
   upvar 1 $Dict vDict
   
   set nb ""
   catch {
      if {[dict exists $vDict $VarType]} {
         set nb [dict get $vDict $VarType]
      }
   }
   
   return $nb
}

#-------------------------------------------------------------------------------
# Nom      : <APViz::AssignDZ>
# Creation : Juillet 2018 - C. Nguyen - CMC/CMOE -
#
# But      :    Verifier si tous les champs ont ete remplis
#
# Parametres      :
#       <Model>   : Nom du modele meteorologique
#       <Var>     : Variable meteorologique
#       <Level>   : Niveau
#       <Run>     : Le numero de la run
#       <Source>  : La provenance des donnees
#       <Date>    : La date de validite
#       
# Retour:
#
# Remarques :
#
#-------------------------------------------------------------------------------

proc APViz::AssignDZ { Product Index Model Var Lev FileID FieldID LevelType } {
   variable Data
   variable ${Product}::Value
   variable ${Product}::RowID

   #----- Get GZ levels: lev1 and lev2
   lassign [split $Lev -] lev1 lev2
   
   #----- Create GZ fields : GZ1 and GZ2 ---- will not be added to viewport
   set fieldIDGZ1 DZ$RowID(Layer$Index)_GZ1
   set fieldIDGZ2 DZ$RowID(Layer$Index)_GZ2
   
   #----- Free those fields if already used
   if {[fstdfield is $fieldIDGZ1]} {
      fstdfield free $fieldIDGZ1
   }
   
   if {[fstdfield is $fieldIDGZ2]} {
      fstdfield free $fieldIDGZ2
   }

   if {[catch {fstdfield read $fieldIDGZ1 $FileID -1 "" [subst {$lev1 $LevelType}] -1 -1 "" GZ}]} {
      puts "fieldIDGZ1: $fieldIDGZ1 failed for level $lev1"
   } elseif {[catch {fstdfield read $fieldIDGZ2 $FileID -1 "" [subst {$lev2 $LevelType}] -1 -1 "" GZ}]} {
      puts "fieldIDGZ2: $fieldIDGZ2 failed for level $lev2"
   } else {
      lappend Data(DZ_GZpairs) [list $fieldIDGZ1 $fieldIDGZ2]
   }
   
   #----- Calcul: GZ1 - GZ2
   vexpr $FieldID "$fieldIDGZ1-$fieldIDGZ2"
   
   if {[fstdfield is $FieldID]} {      
      #----- Add ID to Data(LayersID)
      set Data(LayerIDs) [lreplace $Data(LayerIDs) $RowID(Layer$Index) $RowID(Layer$Index) $FieldID]
   } else {
      set Data(LayerIDs) [lreplace $Data(LayerIDs) $RowID(Layer$Index) $RowID(Layer$Index) FLD$RowID(Layer$Index)]
      puts "DZ FAILED"
      ::Dialog::Info . $Lbl(InvalidField)
   }
}

#-------------------------------------------------------------------------------
# Nom      : <APViz::AreFieldsFilled>
# Creation : Mai 2018 - C. Nguyen - CMC/CMOE -
#
# But      : 	Verifier si tous les champs ont ete remplis
#
# Parametres 	  :
#	<Model>	  : Nom du modele meteorologique
#	<Var>	  : Variable meteorologique
#	<Level>	  : Niveau
#	<Run>	  : Le numero de la run
#	<Source>  : La provenance des donnees
#	<Date>	  : La date de validite
#	
# Retour:
#
# Remarques :
#
#-------------------------------------------------------------------------------

proc APViz::AreFieldsFilled { Model Var Level Run Hour Source Date } {
   set valueList [list $Model $Var $Level $Run $Hour $Source $Date]
   foreach value $valueList {
      if {$value eq ""} {
         return false
      }
   }
   return true
}

#-------------------------------------------------------------------------------
# Nom      : <APViz::AttributeColor>
# Creation : Mai 2018 - C. Nguyen - CMC/CMOE -
#
# But      : 	Attibuer une differente a un field si celui-ci n'est pas le seul de 
#		sa categorie de variable. Permet de distinguer plus facilement les courbes
#
# Parametres 	  :
#	<Var>	  : Variable meteorologique
#	<FieldID> : ID du field
#	
# Retour:
#
# Remarques :
#
#-------------------------------------------------------------------------------

proc APViz::AttributeColor { Var FieldID } {
   variable Data

   #----- Verifier si un autre field du meme type est assigned
   set varList [eval lsearch -glob -all \$Data(LayerIDs) *$Var]

   if {[llength $varList] > 1} {
      set i [string index $FieldID 3]			; # TODO: gerer ID > 9

      set index [lsearch -exact $varList $i]		; # Le nieme var dans la liste
      if {$index >= 0} {
         set nbColors [llength [split $Data(Colors)]]
         set index [expr $index % $nbColors]
         fstdfield configure $FieldID -color [lindex $Data(Colors) $index]
      }
   }
}

#-------------------------------------------------------------------------------
# Nom      : <APViz::CalculateExpression>
# Creation : Juin 2018 - C. Nguyen - CMC/CMOE -
#
# But      : 	Calculer une expression de fields et l'afficher sur le VP
#
# Parametres 	  :
#	<Product> : Le nom du produit selectionne (aussi le namespace) 
#	<Index>	  : Index de la couche 
#	
# Retour:
#
# Remarques :
#
#-------------------------------------------------------------------------------

proc APViz::CalculateExpression { Product Index} {
   variable Data
   variable Lbl
   variable ${Product}::Value
   variable ${Product}::RowID

   set expression $Value(Formula,$Index)
   puts "-------------- Calculating Expr: $expression"

   if {$expression ne ""} {

      #----- Substitute expressions
      puts "transformed expr: [set expression [TranslateExpression $Product $expression]]"
      
      if {[fstdfield is [lindex $Data(CalcIDs) $RowID(Calc$Index)]]} {
         APViz::RemoveVariableFromVP $Data(CalcIDs) $RowID(Calc$Index)		; # Enlever la variable courante du VP pour cette couche
      }
      
      if {[regexp const $expression]} {
         ::Dialog::Info . $Lbl(MissingConst)
         return
      }
      
      #----- Validate IDs in expression
      set hasValidFlds [CheckExpression $expression FLD]
      set hasValidObs [CheckExpression $expression OBS]
      
      #----- Verifier la presence de field ou de metobs dans l'expression
      if {[expr $hasValidFlds || $hasValidObs]} {
         #----- Creer un id unique
         set formulaName ""
         set formulaID [lsearch -exact $Data(Formulas) $Value(UneditedFormula,$Index)]
         if {$formulaID >= 0} {
            set formulaName [lindex $Data(FormulaNames) $formulaID]
         }
         
         set resultFieldID CALC$RowID(Calc$Index)_$formulaName
         #----- Calculer l'expression
         vexpr $resultFieldID $expression
         
         if {[fstdfield is $resultFieldID]} {
            set isActivated $Value(CalcToggle,$Index)
            
            set vpID [GetVPId $Value(CalcVP,$Index)]
            #----- Assigner au ViewPort
            if {[lsearch -exact [Viewport::Assigned $Data(Frame) $vpID] $resultFieldID] eq -1} {
               Viewport::Assign $Data(Frame) $vpID $resultFieldID
            }
            
            fstdfield configure $resultFieldID -active $isActivated
            Viewport::UpdateData $Data(Frame) $vpID
            set Data(CalcIDs) [lreplace $Data(CalcIDs) $RowID(Calc$Index) $RowID(Calc$Index) $resultFieldID]
         }
      } else {
         ::Dialog::Info . $Lbl(MissingFields)
         return
      }
   }
}

#-------------------------------------------------------------------------------
# Nom      : <APViz::GetAllFieldsWithOp>
# Creation : Juin 2018 - C. Nguyen - CMC/CMOE -
#
# But      : 	Calculer une expression de fields et l'afficher sur le VP
#
# Parametres 	      :
#	<Operator>    : L'operateur de calcul (+-*/)
#	<OnlyChecked> : Bool indiquand si on n'inclut que les variables selectionnees 
#	
# Retour:
#
# Remarques :
#
#-------------------------------------------------------------------------------

proc APViz::GetAllFieldsWithOp { Product Operator {OnlyChecked False} } {
   variable Data
   variable ${Product}::Value
   variable ${Product}::RowID

   set fieldString ""

   if {!$OnlyChecked} {
      foreach ID $Data(LayerIDs) {
         if {[fstdfield is $ID]} { 
            set fieldString [string cat $fieldString $ID$Operator] 
         }
      }
   } else {
      for {set i 0} {$i < $Value(NbLayers)} {incr i} {
         if { ($RowID(Layer$i) >= 0) && $Value(Toggle,$i)} {
            set ID [lindex $Data(LayerIDs) $RowID(Layer$i)]
            if {[fstdfield is $ID]} {
               set fieldString [string cat $fieldString $ID$Operator]
            }
         }
      }
   }

   set fieldString [string range $fieldString 0 [expr [string length $fieldString] - 2]]
   return $fieldString
}

#-------------------------------------------------------------------------------
# Nom      : <APViz::Check>
# Creation : Mai 2018 - C. Nguyen - CMC/CMOE -
#
# But      : 	Permet d'ajouter ou d'enlever une variable du Viewport  
#
# Parametres 	 :
#	<Product> : Le nom du produit selectionne (aussi le namespace) 
#       <Index>   : Index de l'item choisi
#       <IsCalc>  : Boolean indiquant s'il s'agit d'une couche de calcul
#	
# Retour:
#
# Remarques :
#
#-------------------------------------------------------------------------------

proc APViz::Check { Product Index {IsCalc False}} {
   variable ${Product}::Value
   variable ${Product}::RowID
   variable Data

   if {$IsCalc} {
      set ID [lindex $Data(CalcIDs) $RowID(Calc$Index)] 
      set isActivated $Value(CalcToggle,$Index)
      set vpID [APViz::GetVPId $Value(CalcVP,$Index)]
   } else {
      set ID [lindex $Data(LayerIDs) $RowID(Layer$Index)] 
      set isActivated $Value(Toggle,$Index)
      set vpID [APViz::GetVPId $Value(VP,$Index)]
   }

   if {[fstdfield is $ID]} {
      fstdfield configure $ID -active $isActivated
   } elseif {[metobs is $ID]} {
      set startIndex [expr [string first _ $ID] + 1]
      set var [string range $ID $startIndex [string length $ID]]
      metmodel configure [metobs define $ID -MODEL] $var -active $isActivated 
   }

   Viewport::UpdateData $Data(Frame) $vpID
   #Viewport::UpdateData $Data(Frame) $Viewport::Data(VP)
}

#-------------------------------------------------------------------------------
# Nom      : <APViz::CheckExpression>
# Creation : Juin 2018 - C. Nguyen - CMC/CMOE -
#
# But      : 	Verifier si des IDs de type VarType est contenu dans l'expression 
#		et s'ils sont valides 
#
# Parametres  :
#   <Expr>    : Expression a evaluer
#   <VarType> : Type de variable (FLD ou OBS)
#	
# Retour: 	Booleen indiquant si des ID de ce type contenus dans l'expression sont valides
#
# Remarques : 	Le nom des ID (fstdfield et metobs) commencent par FLD ou OBS
#
#-------------------------------------------------------------------------------

proc APViz::CheckExpression { Expr VarType } {
   variable Data

   set IDCount [regexp -all $VarType $Expr]

   if { $IDCount <= 0 } {
      return False
   }

   set offset 0
   set IDList { }

   while { [llength $IDList] < $IDCount } {
      set firstIndex [string first $VarType $Expr $offset]
      #----- Verify if more than 1 digit (usual case: FLD#_VV)
      if {$firstIndex >= 0} {
         set lastIndex [expr $firstIndex + 6]			; # Max of 3 digits: FLD###_VV
         while {$lastIndex < [string length $Expr]} {
            set c [string index $Expr [expr $lastIndex + 1]]

            if {[string is alpha $c]} {
               incr lastIndex
            } else {
               break
            }
         }
         lappend IDList [string range $Expr $firstIndex $lastIndex]      
         set offset [expr $firstIndex + 1]
      } else {
         puts "Not all IDs found"
         break
      }
   }

   if {[llength $IDList] > 0} {
      foreach ID $IDList {
         #----- Si ce n'est ni un fstdfield ni un metobs, retourner faux
         if {![fstdfield is $ID] } {
            if {![metobs is $ID]} {
               return false
            }
         }
      }
   }

   return True
}

#-------------------------------------------------------------------------------
# Nom      : <APViz::CloseFiles>
# Creation : Mai 2018 - C. Nguyen - CMC/CMOE -
#
# But      : 	Fermer les FILES contenant les fichiers standards
#
# Parametres :
#	
# Retour:
#
# Remarques :
#
#-------------------------------------------------------------------------------

proc APViz::CloseFiles { } {
   variable Data

   foreach FILE $Data(OpenedFiles) {
      if { $FILE ne "" } {
         fstdfile close $FILE
      }
   }

   set $Data(OpenedFiles) {}
}

#-------------------------------------------------------------------------------
# Nom      : <APViz::CreateColormaps>
# Creation : Juillet 2018 - C. Nguyen - CMC/CMOE -
#
# But      :    Creation des colormaps contenues dans le dossier de config
#
# Parametres :
#       
# Retour:
#
# Remarques :
#
#-------------------------------------------------------------------------------

proc APViz::CreateColormaps { } {
   global env
   variable DataSrc
   variable ::MapBox::Param

   set path $DataSrc(Colormaps)
   set colormapLst [glob -nocomplain -tails -path $path *.rgba]

   #----- Create colormaps
   foreach colormap $colormapLst {
      regsub .rgba $colormap "" colormapName
      colormap create $colormapName -file ${path}/$colormap
   }
   
   #----- Add folder to MapBox paths
   regsub /Colormap/ $path "" colormapPath
   set Param(Paths) [concat $colormapPath $Param(Paths)]
}

#----------------------------------------------------------------------------
# Nom      : <APViz::CreateRangeInterface>
# Creation : Mai 2018 - C. Nguyen - CMC/CMOE
#
# But      : 	Creer l'onterface des variables ranges
#
# Parametres	:
#	<Lst>  	: Liste des differents Types
#	<Index>	: Index de l'item choisi
#	<Dir>	: Dossier dans lequel se trouve le fichier de config
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc APViz::CreateRangeInterface { Lst Index Dir } {
   global GDefs
   variable Data

   APViz::ReinitializeVP
   APViz::CloseFiles

   if {$Data(AutoUpdateEventID) ne ""} {
      after cancel $Data(AutoUpdateEventID)
      set Data(AutoUpdateEventID) ""
   }

   set selected [lindex $Lst $Index]
   set filepath "$GDefs(Dir)/tcl/Tools/APViz/Config/${Data(Folder)}/$Dir/$selected.tcl"
   if {[file isfile $filepath]} {
      APViz::Source $filepath $Data(Tab)
      set Data(ConfigPath) $filepath
   } else {
      puts "$filepath"
   }
}

#----------------------------------------------------------------------------
# Nom      : <APViz::DateBinding>
# Creation : Juillet 2018 - C. Nguyen - CMC/CMOE
#
# But      : Valider la date et initialiser les variables si valide
#
# Parametres :
#	<Product> : Le nom du produit selectionne (aussi le namespace) 
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc APViz::DateBinding { } {
   variable Data
   
   set result [APViz::ValidateDate $Data(Date)] 
   if {$result} {
      APViz::InitializeVars
   }
}

#----------------------------------------------------------------------------
# Nom      : <APViz::DeleteWidget>
# Creation : Mai 2018 - C. Nguyen - CMC/CMOE
#
# But      : 	Supprime le widget et ses enfants
#
# Parametres	:
#	<Widget>  	: Widget a supprimer
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc APViz::DeleteWidget { Widget } {
   if [winfo exists $Widget] {
      eval destroy [winfo children $Widget]
      destroy $Widget
   }
}

#----------------------------------------------------------------------------
# Nom      : <APViz::FetchConfigFiles>
# Creation : Juillet 2018 - C. Nguyen - CMC/CMOE
#
# But      : 	Construire les listes pour les fichiers de configuration
#
# Parametres	:
#	<Widget>  	: Widget a supprimer
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc APViz::FetchConfigFiles { } {
   global GDefs
   variable Data

   set dir Config
   set path $GDefs(Dir)/tcl/Tools/APViz/

   APViz::FetchFiles $path $dir
}

#----------------------------------------------------------------------------
# Nom      : <APViz::FetchFiles>
# Creation : Juillet 2018 - C. Nguyen - CMC/CMOE
#
# But      : 	Construire les listes pour les fichiers de configuration de facon recursive
#
# Parametres	:
#	<Path>  : Path du parent du dossier
#	<Name>	: Nom du dossier
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc APViz::FetchFiles { Path Name } {
   variable Data

   set Data($Name,Files)   {}
   set Data($Name,Folders) {}
   set Path ${Path}${Name}/
   set fileList [glob -nocomplain -tails -path $Path *]

   foreach file $fileList {
      if {[file isdirectory ${Path}$file] && ($file ne "Colormap")} {
         lappend Data($Name,Folders) $file
         
         #----- Appel recursif
         APViz::FetchFiles $Path $file
      } else {
         lappend Data($Name,Files) [lindex [split $file .] 0]
      }
   }
}

#----------------------------------------------------------------------------
# Nom      : <APViz::FetchDates>
# Creation : Juillet 2018 - C. Nguyen - CMC/CMOE
#
# But      : 	Construire la liste de dates disponibles pour afficher dans l'interface
#
# Parametres	:
#	<Path>  : Path du parent du dossier
#	<Name>	: Nom du dossier
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc APViz::FetchDates { Product Model Src } {
   variable Data
   variable DataSrc
   
   set dateList {}

   if {$Product ne ""} {
      if {$Src eq "BURP"} {
         set path $DataSrc(OBS,$Model)/
      } else {
         set path $DataSrc(${Model},${Src})/
      }

      set fileList [glob -nocomplain -tails -path $path *]
    
      #----- Get today's date in format AAAAMMDD
      set date [clock format [clock seconds] -format %Y%m%d]
      
      #---- Verify if today's date is valid (in case of transfer problem)
      if {[lsearch -glob $fileList $date*] >= 0} {
         lappend dateList $date
      }
      
      #----- Set to previous date (yesterday)
      incr date -1
      
      set no 0
      while {([lsearch -glob $fileList $date*] >= 0) || ($no < 10)} {
         lappend dateList $date
         incr date -1
         incr no
      }

      if {[llength $dateList] <= 1} {
         foreach file $fileList {
            set date [string range $file 0 7]
            if {[lsearch -exact $dateList $date] < 0} {
               lappend dateList $date
            }
         }
      }
      set dateList [lreplace [lsort $dateList] 0 0] ; #Furthest dates doesnt have all runs
   }
   
   if {($Data(DateCBWidget) ne "") && [winfo exists $Data(DateCBWidget)]} {
      ComboBox::DelAll $Data(DateCBWidget)
      ComboBox::AddList $Data(DateCBWidget) $dateList
   }

   set Data(Dates) $dateList
}

proc APViz::FetchAllDates { Product } {
   variable Data
   
   if {$Product ne ""} {
      variable ${Product}::Value
      variable ${Product}::RowID

      set  sourceLst {}
      for {set i 0} {$i < $Value(NbLayers)} {incr i} {
         if {[set rowID $RowID(Layer$i)] >= 0} {
            set model $Value(Models,$i)
            set src $Value(Sources,$i)
            
            #----- Verify if the model and src have already been treated
            if {[lsearch -exact $sourceLst $model$src] < 0} {
               lappend dateLst [APViz::FetchDates $Product $model $src]
               lappend sourceLst $model$src
            }
         }
      }

      if {[set nbDateLst [llength $dateLst]] > 1} {
         
         #----- Find the shortest list
         set shortestLst [lindex $dateLst 0]
         set finalDateLst {}
         
         foreach dates $dateLst {
            if {[llength $shortestLst] > [llength $dates] } {
               set shortestLst $dates
            }
         }

         #----- For all dates contained in the shortest date list, verify if contained in all other lists
         foreach date $shortestLst {
            set isValid 1
            for {set j 0} {$j < $nbDateLst} {incr j} {
               if {[lsearch -exact [lindex $dateLst $j] $date] >= 0} {
                  set isValid [expr $isValid && 1]
               } else {
                  set isValid [expr $isValid && 0]
               }
            }
            
            #----- Append date to list only if contained in all lists
            if {$isValid} {
               lappend finalDateLst $date
            }
         }
      } else {
         set finalDateLst [lindex $dateLst 0]
      }
      
      if {($Data(DateCBWidget) ne "") && [winfo exists $Data(DateCBWidget)]} {
         ComboBox::DelAll $Data(DateCBWidget)
         ComboBox::AddList $Data(DateCBWidget) $finalDateLst
      }

      set Data(Dates) $finalDateLst
   }
}

#----------------------------------------------------------------------------
# Nom      : <APViz::FilePathDefine>
# Creation : Mai 2018 - C. Nguyen - CMC/CMOE
#
# But      : Definir le path du fichier
#
# Parametres 	:
#	<Path>	: Path du fichier a sauvegarder
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc APViz::FilePathDefine { Path } {
   variable Param
   set Param(Path)     [file dirname $Path]
   set Param(Filename) [file tail $Path]
   set Param(FullName) $Path
}

#----------------------------------------------------------------------------
# Nom      : <APViz::GenerateConfigFile>
# Creation : Juillet 2018 - C. Nguyen - CMC/CMOE
#
# But      : Generer un fichier de configuration
#
# Parametres 	:
#	<Path>	: Path du fichier a sauvegarder
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc APViz::GenerateConfigFile { Path } {
   variable Data
   variable DataSrc

   if {[set product $Data(CurrentProduct)] ne ""} {
      variable ${product}::Value
      variable ${product}::RowID
      
      #----- Verify if colormaps have changed
      set colormapLst [APViz::ManageColormaps $product]
      
      set filename [file tail $Path]
      set fileID [open $Path w]
      
      #----- Get data from original config file
      set origFileID [open $Data(ConfigPath) r]
      set origFileData [read $origFileID]
      close $origFileID
      
      set origData [split $origFileData "\n"]
      
      #----- Get section indexes
      set geoStartIndex [lsearch -glob $origData "*\#----- Geography*"]
      set styleStartIndex [lsearch -glob $origData "*Style*"]
      set rangeStartIndex [lsearch -glob $origData "*\#----- Ranges*"]
      set layerStartIndex [lsearch -glob $origData "*\#----- Layers*"]

      
      #----- Copy from original til Geo configs
      APViz::WriteConfigSection $fileID [lrange $origData 0 $geoStartIndex]
      
      #----- Write Geo params
      #----- COPY Camera - TODO: Write Cameras
      puts $fileID [lindex $origData [expr $geoStartIndex + 1]]
      APViz::WriteProjectionConfigs $fileID
      APViz::WriteViewportConfigs $fileID

      #----- Variable Style Configs
      puts -nonewline $fileID "\n"
      APViz::WriteVariableConfigs $product $fileID [lrange $origData $styleStartIndex [expr $rangeStartIndex - 1]]  $colormapLst
      
      #----- Ranges :
      puts -nonewline $fileID "\n"
      APViz::WriteConfigSection $fileID [lrange $origData $rangeStartIndex [expr $layerStartIndex - 1]]

      #----- Write Layers
      APViz::WriteLayers $product $fileID
      
      #----- Write Default Values
      puts -nonewline $fileID "\n"
      APViz::WriteDefaultValues $product $fileID
      
      close $fileID
      
      APViz::UpdateProductInterface
   }
}

#----------------------------------------------------------------------------
# Nom      : <APViz::WriteVariableConfigs>
# Creation : Juillet 2018 - C. Nguyen - CMC/CMOE
#
# But      : Ecrire la section des configurations de variables dans le fichier de config
#
# Parametres    :
#       <Product>     : Le nom du produit selectionne (aussi le namespace) 
#       <FileID>      : Identifiant du fichier dans lequel ecrire
#       <DataSource>  : Liste contenant le contenu du fichier de config source
#       <ColormapLst> : Liste des colormaps associees aux variables
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc APViz::WriteVariableConfigs { Product FileID DataSource ColormapLst } {
   #----- Get current configs
   puts $FileID "\#----- Variable Style Configurations"
   set configLst [APViz::GetVariableConfigs $Product $ColormapLst]
   
   foreach config $configLst {
      puts $FileID $config
   }
   
   #----- Copy the general configs
   foreach content $DataSource {
      #----- Copy only if not already defined
      set configName [string range $content [string first \( $content] [string first \) $content]]
      set index [lsearch -glob $configLst "*$configName*"]
      
      if {($index < 0) && ($content ne "")} {
         puts $FileID $content
      }
   }
}


#----------------------------------------------------------------------------
# Nom      : <APViz::WriteLayers>
# Creation : Juillet 2018 - C. Nguyen - CMC/CMOE
#
# But      :    Recupere les parametres de projection et les ecrire dans le 
#               fichier de config
#
# Parametres    :
#       <Product>  : Le nom du produit selectionne (aussi le namespace) 
#       <FileID>     : Identifiant du fichier dans lequel ecrire
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc APViz::WriteLayers { Product FileID } {
   variable Data
   variable ${Product}::Value
   variable ${Product}::RowID

   puts $FileID "\#----- Layers (On:Model:Run:Hour:Source:Var:Level)"
   puts $FileID "set Layers \{"
   #----- Layers (On:Model:Var:Level:Hour:Interval:Run:Source)
   for {set i 0} {$i < $Value(NbLayers)} {incr i} {
      if {[set rowID $RowID(Layer$i)] >= 0} {
         puts $FileID "   [lindex $Data(Layers) $Value(LayerType,$rowID)]"
      }
   }
   puts $FileID "\}"
}

#----------------------------------------------------------------------------
# Nom      : <APViz::WriteDefaultValues>
# Creation : Juillet 2018 - C. Nguyen - CMC/CMOE
#
# But      :    Recupere les parametres de projection et les ecrire dans le 
#               fichier de config
#
# Parametres    :
#       <Product>  : Le nom du produit selectionne (aussi le namespace) 
#       <FileID>     : Identifiant du fichier dans lequel ecrire
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc APViz::WriteDefaultValues { Product FileID } {
   variable ${Product}::Value
   variable ${Product}::RowID
   
   puts $FileID "\#----- Default Values"
   puts $FileID "set DefaultValues \{"
   
   for {set i 0} {$i < $Value(NbLayers)} {incr i} {
      if {[set rowID $RowID(Layer$i)] >= 0} {
         #----- Ajouter Layer
         set checked 	[expr {$Value(Toggle,$i)?True:False}]
         set model 	$Value(Models,$i)
         set var	$Value(Vars,$i)
         set lev	$Value(Levels,$i)
         set run	$Value(Runs,$i)
         set hour	$Value(Hours,$i)
         set src	$Value(Sources,$i)
         
         if {[info exists Value(IP3,$i)]} {
            set ip3 $Value(IP3,$i)
         } else {
            set ip3 ""
         }
         
         if {[info exists Value(VP,$i)]} {
            set vp $Value(VP,$i)
         } else {
            set vp ""
         }
         
         set layer "   $checked:$model:$run:$hour:$src:$var:$lev:$ip3:$vp"
         puts $FileID $layer
      }
   }
   
   puts $FileID "\}"
}

#----------------------------------------------------------------------------
# Nom      : <APViz::ManageColormaps>
# Creation : Juillet 2018 - C. Nguyen - CMC/CMOE
#
# But      :    Gerer les colormaps pour les fichiers de config et en creer des 
#               nouvelles s'il y a eu des modifications
#
# Parametres    :
#       <Product>  : Le nom du produit selectionne (aussi le namespace) 
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc APViz::ManageColormaps { Product } {
   global env
   variable Data
   variable DataSrc
   variable ${Product}::Value
   variable ${Product}::RowID

   set colorLst {}

   for {set i 0} {$i < $Value(NbLayers)} {incr i} {
      if {[set rowID $RowID(Layer$i)] >= 0} {
         set ID [lindex $Data(LayerIDs) $rowID]
         if {[fstdfield is $ID]} {
            set name [fstdfield configure $ID -colormap]	
            set isModified [colormap modified $name]
            set initialColormap [APViz::GetInitialColormap $name]
            
            if {$isModified} {
               #-----Create new name by adding nunmber or changing number at the end of the original name
               set derivatives [glob -nocomplain -tails -path $DataSrc(Colormaps) $initialColormap*.rgba]

               set nbr 1
               set newName ${initialColormap}$nbr
               while {[lsearch -exact $derivatives $newName.rgba] >= 0} {
                  incr nbr
                  set newName ${initialColormap}$nbr
               }

               colormap create $newName
               colormap copy $newName $name
               colormap write $newName $DataSrc(Colormaps)/${newName}.rgba
               
               #----- Append name to the list
               lappend colorLst $newName
            } else {
               lappend colorLst $initialColormap
            }
         }
      }
   }

   return $colorLst
}

#-------------------------------------------------------------------------------
# Nom      : <APViz::GetInitialColormap>
# Creation : Juillet 2018 - C. Nguyen - CMC/CMOE -
#
# But      :    Retourner la colormap originalement lue dans le fichier de config
#
# Parametres      :
#       <Colormap>   : Identifiant de la colormap
#       
# Retour:
#
# Remarques :
#
#-------------------------------------------------------------------------------

proc APViz::GetInitialColormap { Colormap } {
   variable Data
   
   foreach pair $Data(ColormapPairs) {
      if {[lindex $pair 0] eq $Colormap} {
         return [lindex $pair 1]
      }
   }
}

#----------------------------------------------------------------------------
# Nom      : <APViz::GetVariableConfigs>
# Creation : Juillet 2018 - C. Nguyen - CMC/CMOE
#
# But      : Retourner une liste des configurations pour chaque variable
#
# Parametres    :
#       <Product>   : Le nom du produit selectionne (aussi le namespace) 
#       <ColorMaps> : Liste des colormaps associees aux variables
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc APViz::GetVariableConfigs { Product ColorMaps } {
   variable Data
   variable ${Product}::Value
   variable ${Product}::RowID

   set configLst {}

   for {set i 0} {$i < $Value(NbLayers)} {incr i} {
      if {[set rowID $RowID(Layer$i)] >= 0} {
         set var $Value(Vars,$i)
         set level $Value(Levels,$i)
         set ID [lindex $Data(LayerIDs) $rowID]
         
         #---- Set command depending on type
         if {[fstdfield is $ID]} {
            set isFstdField True
         } elseif {[metobs is $ID]} {
            set isFstdField False
            set model [metobs define $ID -MODEL]
         } else {
            puts "Sortir de la boucle"
            break
         }

         set index [APViz::GetVarsNb vDict $var]
         dict incr vDict $var
         
         set params "set Params(${var}$index) \""
         set paramLst [list colormap color font width dash rendercontour rendertexture rendervalue rendergrid renderlabel renderparticle rendervector mapall intervalmode interlabels extrema value]

         foreach param $paramLst {
            if {$param eq "colormap"} {
               set value [lindex $ColorMaps $RowID(Layer$i)]
            } else {
               if {$isFstdField} {
                  set value [fstdfield configure $ID -$param]
               } else {
                  set value [metmodel configure $model $var -$param]
               }
               
            }
            
            #----- Can only configure intervalmode OR intervals
            if {[expr {$param eq "intervalmode"}] && [expr {$value eq "NONE 0.0"}]} {
               if {$isFstdField} {
                  set params [concat $params "-intervals \{[fstdfield configure $ID -intervals]\}"]
               } else {
                  set params [concat $params "-intervals \{[metmodel configure $model $var -intervals]\}"]
               }
            } elseif {$value ne "" } {
               set params [concat $params "-$param $value"]
            }
         }
         
         set params [concat $params "\""]
         lappend configLst $params
      }
   }
   return $configLst
}

#----------------------------------------------------------------------------
# Nom      : <APViz::WriteProjectionConfigs>
# Creation : Juillet 2018 - C. Nguyen - CMC/CMOE
#
# But      :    Recupere les parametres de projection et les ecrire dans le 
#               fichier de config
#
# Parametres    :
#       <FileID>     : Identifiant du fichier dans lequel ecrire
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc APViz::WriteProjectionConfigs { FileID } {
   variable Data

   set projID ${::APViz::Data(Frame)}
   set params "set Params(Projection) \{"
   set paramLst [list type scale mask mapcoast maplake mapriver mappolit mapadmin mapcity  maproad  mapplace maptopo mapbath maptext mapcoord minsize]
   
   foreach param $paramLst {
      set value [projection configure $projID -$param]
      set paramConfig "-$param $value"
      set params [concat $params $paramConfig]
   }
   
   set params [concat $params "\}"]
   puts $FileID $params
}

#----------------------------------------------------------------------------
# Nom      : <APViz::WriteProjectionConfigs>
# Creation : Juillet 2018 - C. Nguyen - CMC/CMOE
#
# But      :    Recupere les parametres de projection et les ecrire dans le 
#               fichier de config
#
# Parametres    :
#       <FileID>     : Identifiant du fichier dans lequel ecrire
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc APViz::WriteViewportConfigs { FileID } {
   variable Data
   
   set params "set Params(Viewport) \{"
   set paramLst [list crowd font bg bd colorcoast colorlake colorfillcoast colorfilllake colorriver colorpolit coloradmin colorcity colorroad colorplace colorcoord]
   set vp [lindex [Page::Registered ${::APViz::Data(Frame)} Viewport] 0]

   foreach param $paramLst {
      set value [${::APViz::Data(Frame)}.page.canvas itemconfigure $vp -$param]
      set paramConfig "-$param [lindex $value [expr [llength $value] - 1]]"
      set params [concat $params $paramConfig]
   }
   
   set params [concat $params "\}"]   
   puts $FileID $params
   
   puts $FileID "set Params(ViewportNb) $Data(VPCount)"
}

#----------------------------------------------------------------------------
# Nom      : <APViz::WriteConfigSection>
# Creation : Juillet 2018 - C. Nguyen - CMC/CMOE
#
# But      : Retourner une liste des configurations pour chaque variable
#
# Parametres    :
#       <FileID>     : Identifiant du fichier dans lequel ecrire
#       <DataSource> : Liste contenant le contenu du fichier de config source
#       <StartIndex> : Index a partir duquel copier les lignes dans le nouveau fichier
#       <EndIndex>   : Index de la derniere ligne a recopier dans le nouveau fichier
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc APViz::WriteConfigSection { FileID DataSource } { 
   #puts "Start index at: $StartIndex for line: [lindex $DataSource $StartIndex] til [lindex $DataSource $EndIndex]"
   
   foreach line $DataSource {
      puts $FileID $line
   }
   
   set comment {
   for {set i $StartIndex} {$i < $EndIndex} {incr i} {
      puts $FileID [lindex $DataSource $i]
   }
   }
}

#-------------------------------------------------------------------------------
# Nom      : <APViz::GetLevelType>
# Creation : Mai 2018 - C. Nguyen - CMC/CMOE -
#
# But      : 	Retourner le type de niveau correspondant 
#
# Parametres 	 :
#	<Source> : Provenance des donnees
#	
# Retour:
#
# Remarques :
#
#-------------------------------------------------------------------------------

proc APViz::GetLevelType { Source } {
   switch $Source {
      "pres"	{ return PRESSURE }
      "eta"	{ return ETA }
      "hyb"	{ return HYBRID }
      "diag"	{ return PRESSURE }
   }

   puts "Cannot determine level with source $Source."
   return ""	; # Format non reconnu
}

#-------------------------------------------------------------------------------
# Nom      : <APViz::InitializeVars>
# Creation : Mai 2018 - C. Nguyen - CMC/CMOE -
#
# But      : 	Verifier si tous les champs ont ete remplis
#
# Parametres 	  :
#	<Product> : Le nom du produit selectionne (aussi le namespace) 
#	
# Retour:
#
# Remarques :
#
#-------------------------------------------------------------------------------

proc APViz::InitializeVars { } {
   variable Data
   set product $Data(CurrentProduct)
   
   if {$product eq ""} {
      return
   }
   
   variable ${product}::Value
   variable ${product}::RowID

   for {set idx 0} {$idx < $Value(NbLayers)} {incr idx} {
      if {$RowID(Layer$idx) >= 0} {
         APViz::AssignVariable $product $idx
      }	
   }
}

#----------------------------------------------------------------------------
# Nom      : <APViz::ReinitializeVP>
# Creation : Mai 2018 - C. Nguyen - CMC/CMOE
#
# But      : Enlever une variable du Viewport et libere le fieldId qui lui est associe
#
# Parametres 	    :
#	<Index>	    : Indice de rangee (RowID) de la variable a enlever 
#	<IsFSTDField> : Bool indiquant s'il s'agit d'un fstdfield
#
# Retour:
#
# Remarques : TODO: RENAME of clean this up!!!! (Not just vp that is reinitialised here)
#
#----------------------------------------------------------------------------

proc APViz::ReinitializeVP { } {
   variable Data

   for {set i 1} {$i <= $Data(VPNb)} {incr i} {
      Viewport::UnAssign $Data(Frame) VP$i
   }
   
   #----- Liberer les ID
   foreach ID $Data(LayerIDs) {
      if {[regexp FLD $ID]} {
         fstdfield free $ID
      } else {
         metobs free $ID
      }
   }

   #----- Liberer les ID de couches de calcul
   foreach calcID $Data(CalcIDs) {
      if {[fstdfield is $calcID]} {
         fstdfield free $calcID
      }
   }

   set product $Data(CurrentProduct)
   if {$product ne ""} {
      variable ${product}::Value
      variable ${product}::Params
      #----- Reinitialiser les textvariables pour calcul
      for {set i 0} {$i < $Value(NbCalcLayers)} {incr i} {
         set Value(Formula,$i) ""
      }
      
      if {[array exists Params]} {
         array unset Params
      }
   }
   
   foreach colormap $Data(Colormaps) {
      colormap free $colormap
   }
  
   #----- Free GZ pairs for DZ
   foreach pair $Data(DZ_GZpairs) {
      foreach gzID $pair {
         if {[fstdfield is $gzID]} {
            fstdfield free $gzID
         }
      }
   }

   #----- Reinitialize values
   set Data(LayerIDs) {}
   set Data(CalcIDs) {}
   set Data(CurrentProduct) ""
   set APViz::Data(Layers) {}
   set Data(Colormaps) {}
   set Data(ColormapPairs) {}
   set Data(DZ_GZpairs) {}
   set Data(VarsDict) ""
   set Data(VPNb) 1
}

#----------------------------------------------------------------------------
# Nom      : <APViz::RemoveVariableFromVP>
# Creation : Mai 2018 - C. Nguyen - CMC/CMOE
#
# But      : Enlever une variable du Viewport et libere le fieldId qui lui est associe
#
# Parametres 	    :
#	<Index>	    : Indice de rangee (RowID) de la variable a enlever 
#	<IsFSTDField> : Bool indiquant s'il s'agit d'un fstdfield (pour differencier des Observations)
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc APViz::RemoveVariableFromVP { IDList Index } {
   variable Data

   set ID [lindex $IDList $Index]
   puts "========"

   set dataType ""
   set varType [string range $ID [expr [string first "_" $ID] + 1] [string length $ID]]
   
   #----- Decrement from vartype total in VarsDict
   dict incr Data(VarsDict) $varType -1

   if {[fstdfield is $ID]} {
      set vp [lindex [fstdfield stats $ID -tag] 1]
      Viewport::UnAssign $Data(Frame) $vp $ID 1     ; # Enlever variable du viewport
      fstdfield free $ID
      set dataType FLD
   } elseif {[metobs is $ID]} {
      set vp [lindex [metobs stats $ID -tag] 1]
      Viewport::UnAssign $Data(Frame) $vp $ID 1     ; # Enlever variable du viewport
      metobs free $ID
      set dataType OBS
   }
   
   puts "Removing $ID from $vp"
   if {[regexp CALC $ID]} {
      set Data(CalcIDs) [lreplace $IDList $Index $Index CALC$Index]
   } else {
      set Data(LayerIDs) [lreplace $IDList $Index $Index ${dataType}$Index]
   }
}

#----------------------------------------------------------------------------
# Nom      : <APViz::SaveConfigFile>
# Creation : Mai 2018 - C. Nguyen - CMC/CMOE
#
# But      : Sauvegarder un fichier de configuration
#
# Parametres 	    :
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc APViz::SaveConfigFile { } {
   variable Data
   variable Lbl
   variable Param

   #----- Verify if product selected
   if {$Data(CurrentProduct) eq ""} {
      ::Dialog::Info . $Lbl(SelectProduct)
   } else {
      if {![regexp "tcl" [file tail $Param(FullName)]]} {
         set Param(FullName) $Param(FullName).tcl
      }
      APViz::GenerateConfigFile $Param(FullName)
   }
}

#----------------------------------------------------------------------------
# Nom      : <APViz::SelectFolder>
# Creation : July 2018 - C. Nguyen - CMC/CMOE
#
# But      : Selectionner le dossier et afficher les choix de macro categories correspondantes
#
# Parametres :
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc APViz::SelectFolder { } {
   variable Data

   set selectedFolder $Data(Folder)
   if {[info exists Data($selectedFolder,Folders)]} {
      set APViz::Data(MacroCategories) $Data($selectedFolder,Folders)
      ComboBox::DelAll $Data(MacroCatDropdown)
      ComboBox::AddList $Data(MacroCatDropdown) $Data($selectedFolder,Folders)
   }
}

#----------------------------------------------------------------------------
# Nom      : <APViz::SetParam>
# Creation : Juin 2018 - C. Nguyen - CMC/CMOE
#
# But      : Permet de choisir la variable a l'interieur de la boite de configuration 
#	     de parametres
#
# Parametres 	:
#	<Index>	: Indice de la variable choisie
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc APViz::SetParam { Index Product {IsCalcLayer False}} {
   variable Data
   variable ${Product}::RowID

   if {$IsCalcLayer} {
      set id [lindex $Data(CalcIDs) $RowID(Calc$Index)]
   } else {
      set id [lindex $Data(LayerIDs) $RowID(Layer$Index)]
   }
   
   if {[fstdfield is $id]} {
      ::FSTD::ParamUpdate $id
   } elseif {[metobs is $id]} {
      ::Obs::ParamUpdate $id
   }   
}

#-------------------------------------------------------------------------------
# Nom      : <APViz::TranslateExpression>
# Creation : Juin 2018 - C. Nguyen - CMC/CMOE -
#
# But      : 	Calculer une expression de fields et l'afficher sur le VP
#
# Parametres 	  :
#	<Product> : Le nom du produit selectionne (aussi le namespace) 
#	<Index>	  : Index de la couche 
#	
# Retour:
#
# Remarques :
#
#-------------------------------------------------------------------------------

proc APViz::TranslateExpression { Product Expr } {
   variable Data
   variable ${Product}::Value
   variable ${Product}::RowID

   set totalLayerIDs [llength $Data(LayerIDs)]
   if {$totalLayerIDs <= 0} { set totalLayerIDs 1 }

   set totalCheckedLayerIDs 0
   for {set i 0} {$i < $Value(NbLayers)} {incr i} {
      if { ($RowID(Layer$i) >= 0) && $Value(Toggle,$i)} {
         incr totalCheckedLayerIDs
      }
   }

   if {[regexp {sum\(ALL\)} $Expr]}     { regsub -all {sum\(ALL\)} $Expr 	[GetAllFieldsWithOp $Product +] Expr }
   if {[regexp {sum\(CHECKED\)} $Expr]} { regsub -all {sum\(CHECKED\)} $Expr 	[GetAllFieldsWithOp $Product + True] Expr }
   if {[regexp {avg\(ALL\)} $Expr]} 	{ regsub -all {avg\(ALL\)} $Expr	\([GetAllFieldsWithOp $Product +]\)/$totalLayerIDs Expr }
   if {[regexp {avg\(CHECKED\)} $Expr]} { regsub -all {avg\(CHECKED\)} $Expr	\([GetAllFieldsWithOp $Product + True]\)/$totalCheckedLayerIDs Expr }

   return $Expr
}

#----------------------------------------------------------------------------
# Nom      : <APViz::UpdateProductInterface>
# Creation : Mai 2018 - C. Nguyen - CMC/CMOE
#
# But      : Mise a jour de l'interface
#
# Parametres :
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc APViz::UpdateProductInterface { } { 
   variable Data

   #----- Mise a jour des listes
   APViz::FetchConfigFiles
   APViz::MacroCategory
}

#----------------------------------------------------------------------------
# Nom      : <APViz::UpdateRange>
# Creation : Mai 2018 - C. Nguyen - CMC/CMOE
#
# But      : Mise a jour de la section Range de l'interface
#
# Parametres :
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc APViz::UpdateRange { } { 
   variable Data

   set selection [$Data(TypeListBox) curselection]	; # Index de l'option selectionne
   set macroCategory $Data(MacroCategory)
   if {[info exists Data($macroCategory,Files)] && ([llength $Data($macroCategory,Files)] > 0) } {
      APViz::CreateRangeInterface $Data($macroCategory,Files) $selection $macroCategory
   }
}

#----------------------------------------------------------------------------
# Nom      : <APViz::ValidateDate>
# Creation : Juillet 2018 - C. Nguyen - CMC/CMOE
#
# But      : Valider la date
#
# Parametres :
#	<Date>	  : La date a valider
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc APViz::ValidateDate { Date } {
   variable Lbl
   variable Data

   set result [expr [lsearch -exact $Data(Dates) $Date] >= 0]

   if {!$result} {
      ::Dialog::Info . $Lbl(WrongDate)
   }

   return $result
}

#----------------------------------------------------------------------------
# Nom      : <APViz::UpdateAvailableDates>
# Creation : Juillet 2018 - C. Nguyen - CMC/CMOE
#
# But      : Mise a jour automatique de la liste de dates disponibles et de la 
#	     date la plus courante
#
# Parametres :
#	<Product> : Le nom du produit selectionne (aussi le namespace) 
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc APViz::UpdateAvailableDates { Product } {
   global GDefs

   variable ${Product}::Value
   variable ${Product}::RowID
   variable Data
   variable DataSrc
   variable Lbl
   
   for {set i 0} {$i < $Value(NbLayers)} {incr i} {
      if {$RowID(Layer$i) >= 0} {
         set model $Value(Models,$i)
         set src $Value(Sources,$i)
         
         #----- Recuperer le path de la source
         if {$src eq "BURP"} {
            set path $DataSrc(OBS,$model)
         } else {
            set path $DataSrc($model,$src)
         }

         #----- Verifier s'il y a eu des changements dans le dossier
         set lastModifiedDate [clock format [file mtime $path] -format %Y%m%d]

         if {$lastModifiedDate > [lindex $Data(Dates) [expr [llength $Data(Dates)] -1 ]]} {
            #----- Afficher message
            .apviz.dock.coo insert 0 [lindex $Lbl(FetchingDates) $GDefs(Lang)]
            APViz::FetchAllDates $Product
            after [expr {1000*30}] ".apviz.dock.coo delete 0 [string length [.apviz.dock.coo get]]"
            break
         }
      }
   }

   if {!$Data(dateLock) && ($Data(Date) ne [lindex $Data(Dates) [expr [llength $Data(Dates)] -1 ]])} {
      set Data(Date) [lindex $Data(Dates) [expr [llength $Data(Dates)] -1 ]]
      #----- Afficher message
      .apviz.dock.coo insert 0 [lindex $Lbl(UpdatingDate) $GDefs(Lang)]
      after [expr {1000*30}] ".apviz.dock.coo delete 0 [string length [.apviz.dock.coo get]]"
      APViz::InitializeVars
   }

   set Data(AutoUpdateEventID) [after [expr {1000*60*10}] APViz::UpdateAvailableDates $Product] ; # Update a chaque 10min:1000*60*10
}

proc APViz::UpdateAvailableDates1 { Product } {
   global GDefs

   variable ${Product}::Value
   variable ${Product}::RowID
   variable Data
   variable DataSrc
   variable Lbl

   #----- Recuperer le premier index qui na pas ete supprime
   set i 0
   while {$RowID(Layer$i) == -1 } {
      incr i
   }

   #----- Recuperer le path de la source
   #TODO: Quelle source prendre? En ce moment: Celle de la premiere rangee
   set model $Value(Models,$i)
   set src $Value(Sources,$i)
   if {$src eq "BURP"} {
      set path $DataSrc(OBS,$model)
   } else {
      set path $DataSrc($model,$src)
   }

   #----- Verifier s'il y a eu des changements dans le dossier
   set lastModifiedDate [clock format [file mtime $path] -format %Y%m%d]

   if {$lastModifiedDate > [lindex $Data(Dates) [expr [llength $Data(Dates)] -1 ]]} {
      #----- Afficher message
      .apviz.dock.coo insert 0 [lindex $Lbl(FetchingDates) $GDefs(Lang)]
      APViz::FetchAllDates $Product
      after [expr {1000*30}] ".apviz.dock.coo delete 0 [string length [.apviz.dock.coo get]]"
   }

   if {!$Data(dateLock) && ($Data(Date) ne [lindex $Data(Dates) [expr [llength $Data(Dates)] -1 ]])} {
      set Data(Date) [lindex $Data(Dates) [expr [llength $Data(Dates)] -1 ]]
      #----- Afficher message
      .apviz.dock.coo insert 0 [lindex $Lbl(UpdatingDate) $GDefs(Lang)]
      after [expr {1000*30}] ".apviz.dock.coo delete 0 [string length [.apviz.dock.coo get]]"
      APViz::InitializeVars
   }

   set Data(AutoUpdateEventID) [after [expr {1000*60*10}] APViz::UpdateAvailableDates $Product] ; # Update a chaque 10min:1000*60*10
}