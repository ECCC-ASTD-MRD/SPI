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
   set i 0
   set fieldsToUnAssign [Viewport::Assigned $Data(Frame) $Viewport::Data(VP)]
   Viewport::UnAssign $Data(Frame) $Viewport::Data(VP) $fieldsToUnAssign 1
   
   set Data(Fields) ""

   set Data(Active) 0

   $Data(Canvas) delete APVIZ

   destroy .visualizer

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
   set Data(VP)     $VP			; # Data(VP)

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
   set Data(VP)     $VP			; # Data(VP)

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

   if { $Data(VP)!="" } {		; # Data(VP)
      Viewport::DrawRange $Data(Frame) $Data(VP) $Data(Lat0) $Data(Lon0) $Data(Lat1) $Data(Lon1) APVIZ red	; # Data(VP)
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
   # Selon les valeurs de Data(MacroCategories)
   
   switch $Data(MacroCategory) {
      "SATELLITE" 	{ puts "Satellite"
			  set Data(MacroTypes) {}
			  APViz::AddToListBox {}
			  APViz::UpdateRange  ; #Appel de la fonction, car pas de choix dans la listBox
			}
      "ANALYSIS"  	{ puts "Analysis"
			  # Remplacer la liste des options au niveau 2
			  set $Data(MacroTypes) $Data(AnalysisTypes)
			  APViz::AddToListBox $Data(AnalysisTypes)
			}
      "FORECASTS" 	{ puts "Forecasts"
			  set Data(MacroTypes) [list $Data(ForecastsTypes)]
			  APViz::AddToListBox $Data(ForecastsTypes)
			}
      "VERIFICATIONS"	{ puts "Verifications"
			  set Data(MacroTypes) $Data(VerificationTypes)
			  APViz::AddToListBox $Data(VerificationTypes)
			}
      "DIAGNOSTIC"  	{ puts "Diagnostic"
			  set Data(MacroTypes) $Data(DiagnosticTypes)
			  APViz::AddToListBox $Data(DiagnosticTypes)
			}
      "VAAC"  		{ puts "Vaac"
			  set Data(MacroTypes) $Data(VAACTypes)
			  APViz::AddToListBox $Data(VAACTypes)
			}
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
  
  # Vider et repopuler la liste
  $Data(TypeListBox) delete 0 end
  set index 0
  foreach item $lst {
    $Data(TypeListBox) insert $index $item
    incr index
  }
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
      variable Label
      variable Value
      variable Params
      variable RowID
      
      set Label(AddCalculation)	{ "Ajouter une couche de calcul" "Add calculation layer"}
      set Label(AddLayer)	{ "Ajouter une couche" "Add a Layer"}
      set Label(Calcul)		{ "Couches de calcul" "Calculation Layers"}
      set Label(Hour)		{ "Heure" "Hour" }
      set Label(Level)		{ "Niveau" "Level"}	
      set Label(Model)		{ "Modèle" "Model" }
      set Label(Source)		{ "Source" "Source" }
      set Label(Run)		{ "Run" "Run" }
      set Label(Type)		{ "Type" "Type" }
      set Label(Variable)	{ "NomVar" "VarName" }
      
      set Value(Models)		""
      set Value(Vars)		""
      set Value(Levels)		""
      set Value(Sources)	""
      set Value(Runs)		""
      set Value(Hours)		""  
      set Value(Toggle)		""
      set Value(NbLayers)	0
      set Value(NbDispLayers)	0
      set Value(NbCalcLayers)	0
      set Value(Formula)	""
      set Value(FormulaName)	""
      
      set RowID(Adjustment)	0
      set RowID(Total)		0
      
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
	
	#----- Default Geography parameters
	set Map(Cameras)    {}
	set Map(Projection) Orthographic

	#----- Source product definition
	source $Path
	
	eval projection configure ${::APViz::Data(Frame)} $Params(Projection)
 
	foreach vp [Page::Registered ${::APViz::Data(Frame)} Viewport] {
	  eval ${::APViz::Data(Frame)}.page.canvas itemconfigure $vp $Params(Viewport)
	}
	
	#----- Refleter les valeurs dans l'interface de configuration des parametres
	::Viewport::ConfigGet ${::APViz::Data(Frame)} ${Viewport::Data(VP)}
	::Viewport::ConfigPut ${::APViz::Data(Frame)} ${Viewport::Data(VP)}
    
	#----- Build product layer interface
	::APViz::DeleteWidget $Widget.range	; # Liberer le widget
	
	frame $Widget.range
	frame $Widget.range.variableGrid	; #Frame pour le grid
	
	#----- Column titles
	label $Widget.range.variableGrid.mod 	-text [lindex $Label(Model) $GDefs(Lang)] 	-width 7
	label $Widget.range.variableGrid.var 	-text [lindex $Label(Variable) $GDefs(Lang)] 	-width 7
	label $Widget.range.variableGrid.lev 	-text [lindex $Label(Level) $GDefs(Lang)] 	-width 7
	label $Widget.range.variableGrid.src 	-text [lindex $Label(Source) $GDefs(Lang)] 	-width 7
	label $Widget.range.variableGrid.run 	-text [lindex $Label(Run) $GDefs(Lang)] 	-width 7
	label $Widget.range.variableGrid.hour 	-text [lindex $Label(Hour) $GDefs(Lang)] 	-width 7
	
	grid $Widget.range.variableGrid 	-column 0 -row 0 -padx 0.2
	grid $Widget.range.variableGrid.mod 	-column 1 -row 0 -padx 0.2
	grid $Widget.range.variableGrid.var 	-column 2 -row 0 -padx 0.2
	grid $Widget.range.variableGrid.lev 	-column 3 -row 0 -padx 0.2
	grid $Widget.range.variableGrid.run 	-column 4 -row 0 -padx 0.2
	grid $Widget.range.variableGrid.hour 	-column 5 -row 0 -padx 0.2
	grid $Widget.range.variableGrid.src 	-column 6 -row 0 -padx 0.2
	
	CreateLayers $Product $Layers $Widget	; # Creation des couches
	pack $Widget.range -side top -fill x -anchor nw

	::APViz::DeleteWidget $Widget.add	; # Liberer le widget
	
	menubutton $Widget.add -image PLUS -text [lindex $Label(AddLayer) $GDefs(Lang)] -compound left -bd 1 -menu $Widget.add.menu
	
	menu $Widget.add.menu
	  set no 0
	  foreach layer $Layers {
	    if {[string index $layer 0] eq "T"} {
	      regsub True: $layer "" desc
	    } else { 
	      regsub False: $layer "" desc
	    }
	    
	    $Widget.add.menu add command -label "Type$no: $desc" -command "APViz::${Product}::CreateLayers $Product $layer $Widget True"
	    incr no
	  }
	  $Widget.add.menu add separator
	  $Widget.add.menu add command -label "Ajouter calcul manuel" -command "APViz::${Product}::AddCalcLayer $Product $Widget"
	  $Widget.add.menu add command -label "Ajouter calcul prédéfini" -command "APViz::${Product}::AddCalcLayer $Product $Widget False"
	
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
      # Retour:
      #
      # Remarques :
      #
      #-------------------------------------------------------------------------------
      
      proc CreateRangeWidget { Product Style Path Index Options IsSpinBox Width} {
	variable Range
	variable Value
	
	if { [string index $Style 0] eq "<" } {
	  set rangeType [string trim $Style {< >}]
	  if $IsSpinBox {
	    spinbox $Path -values $Range($rangeType) -width $Width -textvariable APViz::${Product}::Value($Options,$Index) \
	      -command "APViz::AssignVariable $Product $Index"	; #-var APViz::${Product}::Range(Variables) 
	  } else {
	    ComboBox::Create $Path APViz::${Product}::Value($Options,$Index) noedit unsorted nodouble -1 $Range($rangeType) $Width 8 \
	      "APViz::AssignVariable $Product $Index"
	  }
	  
	  set ranges [split $Range($rangeType)]
	  if { [lindex $ranges 0] ne "" } {
	    set APViz::${Product}::Value($Options,$Index) [lindex [split $Range($rangeType)] 0]
	  } else {
	    set APViz::${Product}::Value($Options,$Index) [lindex [split $Range($rangeType)] 1]	; # In case first elem in Ranges (after split)is a space
	  }
	  
	  #eval set val \$APViz::${Product}::Value($Options,$Index)
	} else {
	    label $Path -width $Width -text $Style -textvariable APViz::${Product}::Value($Options,$Index)
	    set APViz::${Product}::Value($Options,$Index) $Style
	}
      }
      
      #-------------------------------------------------------------------------------
      # Nom      : <APViz::$product::CreateLayers>
      # Creation : Mai 2018 - C. Nguyen - CMC/CMOE -
      #
      # But      : Creer une couche pour ajouter au viewport
      #
      # Parametres 	       :
      #		<Product>      : Produit a afficher (aussi le namespace)
      #		<Layers>       : Information sur les ranges de la couche ayant la forme:
      #					On:Model:Var:Level:Hour:Interval:Run:Source
      #		<Widget>       : Path vers le widget
      #		<IsAddedLayer> : Boolean indiquant s'il s'agit d'une couche additionnelle
      # Retour:
      #
      # Remarques : Lorsque la variable est entre <>, cela signifie que c'est un range
      #
      #-------------------------------------------------------------------------------
      
      proc CreateLayers { Product Layers Widget {IsAddedLayer False} } {
	variable Value
	variable RowID
	
	set no $Value(NbLayers)
	foreach layer $Layers {
	  #----- Extract layer parts
	  lassign [split $layer :] toggle model var level hour interval run dataSrc
	  
	  #----- Toggle On/Off
	  checkbutton $Widget.range.variableGrid.layer${no}_toggle -anchor w -var APViz::${Product}::Value(Toggle,$no) \
	    -command "APViz::Check $Product $no"
	  if {!$toggle} {
	    $Widget.range.variableGrid.layer${no}_toggle deselect
	  } elseif {!$IsAddedLayer} {
	    $Widget.range.variableGrid.layer${no}_toggle select
	  }
	  
	  # CreateRangeWidget { Product Style Path Index Options IsSpinBox }
	  CreateRangeWidget $Product $model 	$Widget.range.variableGrid.layer${no}_model 	$no Models true 5
	  CreateRangeWidget $Product $var 	$Widget.range.variableGrid.layer${no}_var 	$no Vars false 5
	  CreateRangeWidget $Product $level	$Widget.range.variableGrid.layer${no}_level 	$no Levels true 5
	  CreateRangeWidget $Product $hour 	$Widget.range.variableGrid.layer${no}_hour 	$no Hours true 5
	  CreateRangeWidget $Product $run 	$Widget.range.variableGrid.layer${no}_run 	$no Runs true 5
	  CreateRangeWidget $Product $dataSrc 	$Widget.range.variableGrid.layer${no}_dataSrc 	$no Sources false 5
	  
	  button $Widget.range.variableGrid.layer${no}_delete 	-image DELETE -bd 1 	-relief flat -overrelief raised -command "APViz::${Product}::DeleteLayer $Widget $no $Product"
	  button $Widget.range.variableGrid.layer${no}_param 	-image PARAMS -bd 1 	-relief flat -overrelief raised -command "APViz::SetParam $no ; SPI::Params . 1"
	  
	  set RowID($no) [expr $no - $RowID(Adjustment)]
	  label $Widget.range.variableGrid.layer${no}_rowID	-text $RowID($no)
	  
	  #----- Place widgets in grid	
	  set itemList [list toggle model var level run hour dataSrc param delete]
	  set colNb 0
	  foreach item $itemList {
	    grid $Widget.range.variableGrid.layer${no}_$item	-column $colNb -row [expr $no + 1] -padx 0.1
	    Bubble::Create $Widget.range.variableGrid.layer${no}_$item $RowID($no)
	    incr colNb
	  }
	  
	  if {$IsAddedLayer} {
	    APViz::AssignVariable $Product $no
	  }
	  
	  incr no
	  incr Value(NbDispLayers)
	}
	set Value(NbLayers) $no
	
	AdjustSpinboxMaxValue $Widget
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
      
      proc DeleteLayer { Widget Index Product } {
	variable RowID
	variable Value
	
	APViz::RemoveVariableFromVP $Index	; # Enlever variable du Viewport
	#---- Ajuster le rowID
	incr RowID(Adjustment)
	
	set itemList [list toggle model var level run hour dataSrc param delete]
	  
	#---- Adjust all rowIds below range.variableGrid.layer${no}_toggle
	for {set i 0} {$i < $Value(NbLayers)} {incr i} {
	  if {$RowID($i) > $RowID($Index)} {
	    if {[winfo exists $Widget.range.variableGrid.layer${i}_rowID]} {
	      set oldRowID $RowID($i)
	      set RowID($i) [expr $RowID($i) - 1]
	      #---- Change row id in bubble
	      foreach item $itemList {
		Bubble::Create $Widget.range.variableGrid.layer${i}_$item $RowID($i)
	      }
	      #$Widget.range.variableGrid.layer${i}_rowID configure -text $RowID($i)
	      APViz::AssignVariable $Product $i
	    }
	  }
	}
	set RowID($Index) -1
	
	#---- Detruire les widgets
	if [winfo exists $Widget.range.variableGrid] {	  
	  foreach item $itemList {
	    destroy $Widget.range.variableGrid.layer${Index}_$item
	  }
	}
	
	incr Value(NbDispLayers) -1
	AdjustSpinboxMaxValue $Widget
	
      }
      
      proc AdjustSpinboxMaxValue { Widget } {
	variable Value
	
	#--- Ajuster la valeur max des spinbox pour les couches de calcul 
	for {set i 0} {$i < $Value(NbCalcLayers)} {incr i} {
	  if {[winfo exists $Widget.calc.$i]} {
	    $Widget.calc.$i.varA configure -to [expr $Value(NbDispLayers) - 1]
	    $Widget.calc.$i.varB configure -to [expr $Value(NbDispLayers) - 1]
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
      
      proc AddCalcLayer { Product Widget {IsManual True} } {
	variable Value
	set no $Value(NbCalcLayers)
	puts "Adding calc layer$no"
	frame $Widget.calc.$no
	  checkbutton $Widget.calc.$no.check -anchor w -var APViz::${Product}::Value(CalcToggle,$no)
	  
	  set formulaNames [list Difference Abs_difference Sum]
	  set formulas [list A-B abs(A-B) A+B]
	  
	  Option::Create $Widget.calc.$no.formula "Formule:" "::APViz::${Product}::Value(FormulaName) APViz::${Product}::Value(Formula)" 1 15 $formulaNames "APViz::${Product}::TEST $Widget.calc.$no.formula" $formulas 
	  
	  set comment {
	  if {$IsManual} {
	    entry $Widget.calc.$no.formula -textvariable APViz::${Product}::Value(Formula,$no) -width 12
	  } else {
	    ComboBox::Create $Widget.calc.$no.formula APViz::${Product}::Value(Formula,$no) editclose unsorted nodouble -1 ${::APViz::Data(Calcul)} 10 10 {}
	  }
	  }
	  
	  label $Widget.calc.$no.lblA	-text "A:"
	  label $Widget.calc.$no.lblB	-text "B:"
	  
	  spinbox $Widget.calc.$no.varA	-from 0 -to [expr $Value(NbDispLayers) - 1] -increment 1 -width 2 -textvariable Value(VarA,$no)
	  spinbox $Widget.calc.$no.varB	-from 0 -to [expr $Value(NbDispLayers) - 1] -increment 1 -width 2 -textvariable Value(VarB,$no)
	  
	  button $Widget.calc.$no.apply -image CALC -bd 1 	-relief flat -overrelief raised ;#-command "APViz::${Product}::DeleteCalcLayer $Widget $no $Product"
	  button $Widget.calc.$no.delete -image DELETE -bd 1 	-relief flat -overrelief raised ;#-command "APViz::${Product}::DeleteCalcLayer $Widget $no $Product"
	  
	  pack $Widget.calc.$no.check $Widget.calc.$no.formula $Widget.calc.$no.lblA $Widget.calc.$no.varA \
	      $Widget.calc.$no.lblB $Widget.calc.$no.varB $Widget.calc.$no.apply $Widget.calc.$no.delete -side left -fill x -expand true
	  
	pack $Widget.calc.$no -side top -fill x
	incr Value(NbCalcLayers)
      }
      
      proc TEST { Widget } {
	variable Value
	#set Value(FormulaName) $Value(Formula)
	#set Value(Formula) $Value(FormulaName)
	
	puts "Formula: $Value(Formula)"
	puts "FormulaName : $Value(FormulaName)"
	
	set Value(FormulaName) $Value(Formula)
      }
      
      proc AddCalcLayer_grid { Product Widget {IsManual True} } {
	variable Value
	set no $Value(NbCalcLayers)
	
	if {$no eq 0} {
	  #----- Premiere fois
	  frame $Widget.calc.grid
	  label $Widget.calc.grid.form -text "Formule" 	; #-width 7
	  label $Widget.calc.grid.varA -text "A" 	; #-width 7
	  label $Widget.calc.grid.varB -text "B" 	; #-width 7
	  label $Widget.calc.grid.const -text "Const" 	; #-width 7
	  
	  grid $Widget.calc.grid 	-column 0 -row 0 -padx 0.2
	  grid $Widget.calc.grid.form 	-column 1 -row 0 -padx 0.2
	  grid $Widget.calc.grid.varA 	-column 2 -row 0 -padx 0.2
	  grid $Widget.calc.grid.varB	-column 3 -row 0 -padx 0.2
	  grid $Widget.calc.grid.const	-column 4 -row 0 -padx 0.2
	  
	}
	
	checkbutton $Widget.calc.grid.layer${no}_check -anchor w -var APViz::${Product}::Value(CalcToggle,$no)
	
	if {$IsManual} {
	  entry $Widget.calc.grid.layer${no}_formula -textvariable APViz::${Product}::Value(Formula,$no) -width 22
	} else {
	  ComboBox::Create $Widget.calc.grid.layer${no}_formula APViz::${Product}::Value(Formula,$no) editclose unsorted nodouble -1 ${::APViz::Data(Calcul)} 20 10 {}
	}
	
	spinbox $Widget.calc.grid.layer${no}_varA	-from 0 -to [expr $Value(NbDispLayers) - 1] -increment 1 -width 2
	spinbox $Widget.calc.grid.layer${no}_varB	-from 0 -to [expr $Value(NbDispLayers) - 1] -increment 1 -width 2
	
	button $Widget.calc.grid.layer${no}_apply 	-image CALC -bd 1 -relief flat -overrelief raised ;#-command "APViz::${Product}::DeleteCalcLayer $Widget $no $Product"
	button $Widget.calc.grid.layer${no}_delete 	-image DELETE -bd 1 -relief flat -overrelief raised ;#-command "APViz::${Product}::DeleteCalcLayer $Widget $no $Product"
	button $Widget.calc.grid.layer${no}_param 	-image PARAMS -bd 1 	-relief flat -overrelief raised -command "APViz::SetParam $no ; SPI::Params . 1"
	
	grid $Widget.calc.grid.layer${no}_check		-column 0 -row [expr $no + 1] -padx 1.0
	grid $Widget.calc.grid.layer${no}_formula	-column 1 -row [expr $no + 1] -padx 1.0
	grid $Widget.calc.grid.layer${no}_varA		-column 2 -row [expr $no + 1] -padx 1.0
	grid $Widget.calc.grid.layer${no}_varB		-column 3 -row [expr $no + 1] -padx 1.0
	grid $Widget.calc.grid.layer${no}_apply		-column 5 -row [expr $no + 1] -padx 0.2
	grid $Widget.calc.grid.layer${no}_param		-column 6 -row [expr $no + 1] -padx 0.2
	grid $Widget.calc.grid.layer${no}_delete	-column 7 -row [expr $no + 1] -padx 0.2
	
	if {!$IsManual} {
	  entry $Widget.calc.grid.layer${no}_const -width 3
	  grid $Widget.calc.grid.layer${no}_const -column 4 -row [expr $no + 1] -padx 1.0
	}

	incr Value(NbCalcLayers)
      }
      
      #-------------------------------------------------------------------------------
      # Nom      : <APViz::$product::AddPredefinedCalcLayer>
      # Creation : Juin 2018 - C. Nguyen - CMC/CMOE -
      #
      # But      : Ajouter une couche de calcul predefini
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
      proc AddPredefinedCalcLayer { Widget } {
	
      }
   }
   
   ${product}::Load $Path $product $Widget
   
   APViz::InitializeVars $product
}

#-------------------------------------------------------------------------------
# Nom      : <APViz::AssignVariable>
# Creation : Mai 2018 - C. Nguyen - CMC/CMOE -
#
# But      : Ajouter (afficher) une liste d'elements a la liste
#
# Parametres 		:
#	<Product>	: Le nom du produit selecitonne (aussi le namespace) 
#	<Index>		: Index de la couche 
#	
# Retour:
#
# Remarques :
#
#-------------------------------------------------------------------------------

proc APViz::AssignVariable { Product Index } {
  global env
  variable FileNb
  variable Data
  variable DataSrc
  variable Lbl
  variable ${Product}::Value
  variable ${Product}::Params
  variable ${Product}::RowID
  
  #set ::Viewport::Ressources(Coast) white
  #Viewport::ConfigPut $Data(Frame) $Viewport::Data(VP)

  #----- Get layer values
  set model	$Value(Models,$Index)
  set var	$Value(Vars,$Index)
  set lev	$Value(Levels,$Index)
  set run	$Value(Runs,$Index)
  set hour	$Value(Hours,$Index)
  set src	$Value(Sources,$Index)
  set date	[clock format [clock seconds] -format %Y%m%d]				; # Today's date in format AAAAMMDD
  
  if {[ APViz::AreFieldsFilled $model $var $lev $run $hour $src $date ]} {		; # Verifier si tous les champs sont remplis
    set timestamp ${date}${run}_$hour	
    set filepath $DataSrc($model,$src)/$timestamp					; # Format: AAAAMMDDRR_HHH
    set fileID FILE_${model}_${src}_$timestamp
		    
    if {[fstdfile is $filepath]} {							; # Verifier la validite du fichier standard
      if {[catch { fstdfile open $fileID read $filepath }]} {
	puts "File already opened"
      } else {
	lappend Data(OpenedFiles) $fileID
	puts "STANDARD FILE - Opening $fileID	$filepath"
	puts "New file opened, added to Data(OpenedFiles) : $Data(OpenedFiles)"
      }
      
      if {[fstdfield is [lindex $Data(Fields) $Index]]} {
	APViz::RemoveVariableFromVP $Index						; # Enlever la variable courante du VP pour cette couche
      }
      
      set levelType [ APViz::GetLevelType $src ]
      set fieldID FLD$RowID($Index)_${var}
      set Data(Fields) [lreplace $Data(Fields) $Index $Index $fieldID]
      if {[catch {fstdfield read $fieldID $fileID -1 "" [subst {$lev $levelType}] -1 -1 "" $var }]} {
	::Dialog::Info . $Lbl(InvalidField)
	return
      }
      
      if { [info exist Params($var)] } {
	#catch { 
	  eval fstdfield configure $fieldID $Params($var) 
	#}
      } elseif { [info exist Params(${var}$lev)] } {
	#catch { 
	  eval fstdfield configure $fieldID $Params(${var}$lev) 
	#}
      }      
      
      fstdfield configure $fieldID -active $Value(Toggle,$Index)
      
      #---- Assigner seulement si n'est pas assigne
      if {[lsearch -exact [Viewport::Assigned $Data(Frame) $Viewport::Data(VP)] $fieldID] eq -1} {
	puts "Assigner la variable"
	Viewport::Assign $Data(Frame) $Viewport::Data(VP) $fieldID 1
      }      
    } else {
      puts "File $filepath not available."
    }
  } else {
    puts "Missing values"
  }
}

#-------------------------------------------------------------------------------
# Nom      : <APViz::AddFieldLayer>
# Creation : Mai 2018 - C. Nguyen - CMC/CMOE -
#
# But      : 	Ajouter un identifiant fld par defaut
#
# Parametres 	  :
#	
# Retour:
#
# Remarques :
#
#-------------------------------------------------------------------------------

proc APViz::AddFieldLayer { } {
  variable Data
  
  set nbLayers [llength $Data(Fields)]
  lappend Data(Fields) FLD$nbLayers
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

proc APViz::CalculateExpression { Expr Index} {
  variable Data
  
  #---- Creer un id unique
  set resultFieldID CALC$Index
  
  if {$Expr ne ""} {
    if {[fstdfield is resultFieldID]} {
      fstdfield free resultFieldID
    }
    vexpr resultFieldID $Expr
  }
}

proc APViz::TranslateExpression { Product Expr Index} {
  variable Data
  variable ${Product}::Value
  
  set translatedExpr $Expr
  set alphaValues { A B }
  
  #---- Si l'expression contient des lettres, remplacer par le field associe
  #TODO: Traiter le cas de la constante
  for {set i 0} {$i < [string length $Expr]} {incr i} {
    set charIndex [lsearch -exact $alphaValues [string index $Expr $i]]
    if {charIndex >= 0} {
      set var [lindex alphaValues charIndex]
      set fieldID [lindex $Data(Fields) $Value(Var$var,$Index)]
      puts "Field selected for $var : $fieldID"
      
      set translatedExpr [lreplace $translatedExpr $i $i $fieldID]
    }
  }
  
  return $translatedExpr
}

#-------------------------------------------------------------------------------
# Nom      : <APViz::Check>
# Creation : Mai 2018 - C. Nguyen - CMC/CMOE -
#
# But      : 	Permet d'ajouter ou d'enlever une variable du Viewport  
#
# Parametres 	 :
#	<Source> : Provenance des donnees
#	
# Retour:
#
# Remarques :
#
#-------------------------------------------------------------------------------

proc APViz::Check { Product Index } {
  variable ${Product}::Value
  variable Data
  
  set fieldID [lindex $Data(Fields) $Index] 
  if {[fstdfield is $fieldID]} {
    fstdfield configure $fieldID -active $Value(Toggle,$Index)
    Viewport::UpdateData $Data(Frame) $Viewport::Data(VP)
  }
}

#-------------------------------------------------------------------------------
# Nom      : <APViz::CloseFiles>
# Creation : Mai 2018 - C. Nguyen - CMC/CMOE -
#
# But      : 	Fermer les FILES contenant les fichiers standards et reinitialiser 
 #		le FileNb (utilise pour creer des nouveaux id)
#
# Parametres :
#	
# Retour:
#
# Remarques :
#
#-------------------------------------------------------------------------------

proc APViz::CloseFiles { } {
  global FileNb
  variable Data

  foreach FILE $Data(OpenedFiles) {
    if { $FILE ne "" } {
      fstdfile close $FILE
    }
  }
  
  set $Data(OpenedFiles) {}
  set FileNb 0
}

#----------------------------------------------------------------------------
# Nom      : <APViz::CreateRangeInterface>
# Creation : Mai 2018 - C. Nguyen - CMC/CMOE
#
# But      : 	Creer l'onterface des variables ranges
#
# Parametres	:
#	<Lst>  	: Liste des differents AnalysisTypes
#	<Index>	: Index de l'item choisir
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
  
  set selected [lindex $Lst $Index]
  set filepath "$GDefs(Dir)/tcl/Tools/APViz/APViz/Operational/$Dir/$selected.tcl"
  APViz::Source $filepath $Data(Tab)
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
#	<Product> : Le nom du produit selecitonne (aussi le namespace) 
#	
# Retour:
#
# Remarques :
#
#-------------------------------------------------------------------------------

proc APViz::InitializeVars { Product } {
  variable ${Product}::Value
  variable Data
  
  for {set idx 0} {$idx < $Value(NbLayers)} {incr idx} {
    APViz::AssignVariable $Product $idx
  }
}

#----------------------------------------------------------------------------
# Nom      : <APViz::RemoveVariableFromVP>
# Creation : Mai 2018 - C. Nguyen - CMC/CMOE
#
# But      : Enlever une variable du Viewport et libere le fieldId qui lui est associe
#
# Parametres 	:
#	<Index>	: Indice de la variable a enlever 
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc APViz::RemoveVariableFromVP { Index } {
  variable Data
  Viewport::UnAssign $Data(Frame) $Viewport::Data(VP) [lindex $Data(Fields) $Index]	; # Enlever variable du viewport ---- was Viewport::Data(VP)
  fstdfield free [lindex $Data(Fields) $Index]
  
  set Data(Fields) [lreplace $Data(Fields) $Index $Index FLD$Index]
}

#----------------------------------------------------------------------------
# Nom      : <APViz::UpdateRange>
# Creation : Mai 2018 - C. Nguyen - CMC/CMOE
#
# But      : Mise a jour de la section Range de l'interface pour le type Analysis
#
# Parametres :
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc APViz::UpdateRange { } { 
   global GDefs
   variable Data

   puts "===== Range update ====="
   # APViz::InitializeVariables $Data(Tab).range	; #Deselect buttons
   
   set selection [$Data(TypeListBox) curselection]	; # Index de l'option selectionne
   switch $Data(MacroCategory) {
      "SATELLITE" 	{ puts "Satellite" }
      "ANALYSIS"  	{ APViz::CreateRangeInterface [split $Data(AnalysisTypes)] $selection Analysis }
      "FORECASTS" 	{ puts "Forecasts" }
      "VERIFICATIONS"	{ APViz::CreateRangeInterface [split $Data(VerificationTypes)] $selection Verifications }
      "DIAGNOSTIC"  	{ puts "Diagnostic" }
      "VAAC"  		{ puts "Vaac" }
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

proc APViz::SetParam { Index } {
  variable Data
  
  set field [lindex $Data(Fields) $Index]
  if {[lsearch -exact [Viewport::Assigned $Data(Frame) $Viewport::Data(VP)] $field] ne -1} {
    ::FSTD::ParamUpdate $field
    set comment {
    set ::FSTD::Param(Spec) $field
    ::FSTD::ParamSet $field
    ::FSTD::ParamGet $field
    ::FSTD::ParamPut True
    }
  }
}

