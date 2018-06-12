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
   set fieldsToUnAssign ""
   foreach fld $Data(Fields) {
     if {$fld ne "FLD$i"} {
	lappend fieldsToUnAssign $fld 
	fstdfield free $fld
     }
     incr i
   }
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
      
      set Label(AddCalculation)	{ "Ajouter une couche de calcul" "Add calculation layer"}
      set Label(AddLayer)	{ "Ajouter une couche" "Add a Layer"}
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
      
      proc Load { Path Product Widget } {
	global GDefs
	variable Label
	variable Value
	
	#----- Default Geography parameters
	set Map(Cameras)    {}
	set Map(Projection) Orthographic

	#----- Source product definition
	source $Path
    
	#----- Build product layer interface
	if [winfo exists $Widget.range] {
	  eval destroy [winfo children $Widget.range]
	  destroy $Widget.range
	}
	
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
	  $Widget.add.menu add cascade -label [lindex $Label(AddCalculation) $GDefs(Lang)] -menu $Widget.add.menu.calcMenu
	  
	menu $Widget.add.menu.calcMenu
	  $Widget.add.menu.calcMenu add command -label "Calcul manuel"
	  $Widget.add.menu.calcMenu add command -label "Calcul prédéfini"
	  
	
	pack $Widget.add -side top -padx 2 -pady 2 -anchor nw
      }
      
      proc CreateRangeWidget { Product Style Path Index Options IsSpinBox } {
	variable Range
	variable Value
	
	if { [string index $Style 0] eq "<" } {
	  set rangeType [string trim $Style {< >}]
	  if $IsSpinBox {
	    spinbox $Path -values $Range($rangeType) -width 5 -textvariable APViz::${Product}::Value($Options,$Index) \
	      -command "APViz::DisplayVariable $Product $Index"	; #-var APViz::${Product}::Range(Variables) 
	  } else {
	    ComboBox::Create $Path APViz::${Product}::Value($Options,$Index) noedit unsorted nodouble -1 $Range($rangeType) 5 8 \
	      "APViz::DisplayVariable $Product $Index"
	  }
	  
	  set ranges [split $Range($rangeType)]
	  if { [lindex $ranges 0] ne "" } {
	    set APViz::${Product}::Value($Options,$Index) [lindex [split $Range($rangeType)] 0]
	  } else {
	    set APViz::${Product}::Value($Options,$Index) [lindex [split $Range($rangeType)] 1]	; # In case first elem in Ranges (after split)is a space
	  }
	  
	  eval set val \$APViz::${Product}::Value($Options,$Index)
	} else {
	    label $Path -width 5 -text $Style -textvariable APViz::${Product}::Value($Options,$Index)
	    set APViz::${Product}::Value($Options,$Index) $Style
	}
      }
      
      proc CreateLayers { Product Layers Widget {IsAddedLayer False} } {
	variable Value
	
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
	  CreateRangeWidget $Product $model 	$Widget.range.variableGrid.layer${no}_model 	$no Models true
	  CreateRangeWidget $Product $var 	$Widget.range.variableGrid.layer${no}_var 	$no Vars false
	  CreateRangeWidget $Product $level	$Widget.range.variableGrid.layer${no}_level 	$no Levels true
	  CreateRangeWidget $Product $hour 	$Widget.range.variableGrid.layer${no}_hour 	$no Hours true
	  CreateRangeWidget $Product $run 	$Widget.range.variableGrid.layer${no}_run 	$no Runs true
	  CreateRangeWidget $Product $dataSrc 	$Widget.range.variableGrid.layer${no}_dataSrc 	$no Sources false
	  
	  button $Widget.range.variableGrid.layer${no}_delete 	-image DELETE -bd 1 	-relief flat -overrelief raised -command "APViz::${Product}::DeleteLayer $Widget $no"
	  button $Widget.range.variableGrid.layer${no}_param 	-image PARAMS -bd 1 	-relief flat -overrelief raised -command { SPI::Params . 1 }
	  #button $Widget.range.variableGrid.layer${no}_add 	-image PLUS -bd 1 	-relief flat -overrelief raised \
	  #  -command "APViz::${Product}::CreateLayers $Product $layer $Widget True"
	  
	  
	  #----- Place widgets in grid	
	  grid $Widget.range.variableGrid.layer${no}_toggle	-column 0 -row [expr $no + 1] -padx 0.1
	  grid $Widget.range.variableGrid.layer${no}_model	-column 1 -row [expr $no + 1] -padx 0.1
	  grid $Widget.range.variableGrid.layer${no}_var	-column 2 -row [expr $no + 1] -padx 0.1
	  grid $Widget.range.variableGrid.layer${no}_level	-column 3 -row [expr $no + 1] -padx 0.1
	  grid $Widget.range.variableGrid.layer${no}_run	-column 4 -row [expr $no + 1] -padx 0.1
	  grid $Widget.range.variableGrid.layer${no}_hour	-column 5 -row [expr $no + 1] -padx 0.1
	  grid $Widget.range.variableGrid.layer${no}_dataSrc	-column 6 -row [expr $no + 1] -padx 0.1
	  grid $Widget.range.variableGrid.layer${no}_param	-column 7 -row [expr $no + 1] -padx 0.1	
	  #grid $Widget.range.variableGrid.layer${no}_add	-column 8 -row [expr $no + 1] -padx 0.1	
	  grid $Widget.range.variableGrid.layer${no}_delete	-column 9 -row [expr $no + 1] -padx 0.1	
	  
	  if {$IsAddedLayer} {
	    APViz::AddFieldLayer
	  }
	  
	  incr no
	}
	set Value(NbLayers) $no

      }
      
      proc DeleteLayer { Widget Index } {
	APViz::RemoveVariableFromVP $Index	; # Enlever variable du Viewport
	
	if [winfo exists $Widget.range.variableGrid] {
	  puts "Destroying layer $Index"
	  destroy $Widget.range.variableGrid.layer${Index}_toggle	
	  destroy $Widget.range.variableGrid.layer${Index}_model	
	  destroy $Widget.range.variableGrid.layer${Index}_var	
	  destroy $Widget.range.variableGrid.layer${Index}_level	
	  destroy $Widget.range.variableGrid.layer${Index}_run	
	  destroy $Widget.range.variableGrid.layer${Index}_hour	
	  destroy $Widget.range.variableGrid.layer${Index}_dataSrc	
	  destroy $Widget.range.variableGrid.layer${Index}_param	
	  destroy $Widget.range.variableGrid.layer${Index}_add	
	  destroy $Widget.range.variableGrid.layer${Index}_delete
	}
      }      
   }
   
   ${product}::Load $Path $product $Widget
   
   APViz::InitializeVars $product
}

#-------------------------------------------------------------------------------
# Nom      : <APViz::DisplayVariable>
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

proc APViz::DisplayVariable { Product Index } {
  global env
  variable FileNb
  variable Data
  variable DataSrc
  variable ${Product}::Value
  variable ${Product}::Params
  
  #set ::Viewport::Ressources(Coast) white
  #Viewport::ConfigPut $Data(Frame) $Viewport::Data(VP)

  if {$Value(Toggle,$Index)} {								; # Appliquer les changements ssi le toggle est active
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
		      
      if {[fstdfile is $filepath]} {							; # Verifier la validite du fichier standard
	fstdfile open FILE$FileNb read $filepath
	lappend Data(OpenedFiles) FILE$FileNb
	puts "STANDARD FILE - Opening FILE$FileNb	$filepath"
	
	if {[fstdfield is [lindex $Data(Fields) $Index]]} {
	  APViz::RemoveVariableFromVP $Index						; # Enlever la variable courante du VP pour cette couche
	}
	
	set levelType [ APViz::GetLevelType $src ]
	set Data(Fields) [lreplace $Data(Fields) $Index $Index FLD${Index}-${var}_$timestamp]
	fstdfield read FLD${Index}-${var}_$timestamp FILE$FileNb -1 "" [subst {$lev $levelType}] -1 -1 "" $var
	
	if { [info exist Params($var)] } {
	  #catch { 
	    eval fstdfield configure FLD${Index}-${var}_$timestamp $Params($var) 
	  #}
	} elseif { [info exist Params(${var}$lev)] } {
	  #catch { 
	    eval fstdfield configure FLD${Index}-${var}_$timestamp $Params(${var}$lev) 
	  #}
	}
	
	#---- Assigner seulement si valeur nest pas une valeur par defaut
	set i 0
	set fieldsToAssign ""
	foreach fld $Data(Fields) {
	  if {$fld ne "FLD$i"} {
	    lappend fieldsToAssign $fld 
	  }
	  incr i
	}
	
	Viewport::Assign $Data(Frame) $Viewport::Data(VP) $fieldsToAssign 1
	puts "Assigned: [Viewport::Assigned $Data(Frame) $Viewport::Data(VP) ]"
	incr FileNb
      } else {
	puts "File $filepath not available."
      }
    } else {
      puts "Missing values"
    }
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
  
  if {$Value(Toggle,$Index)} {
    # Ajouter au Viewport
    APViz::DisplayVariable $Product $Index
  } else {
    APViz::RemoveVariableFromVP $Index
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
    lappend Data(Fields) FLD$idx
    if {$Value(Toggle,$idx)} {
      APViz::DisplayVariable $Product $idx
    }
  }
  
  puts "Initialized Data(Fields): $Data(Fields)"
}

proc APViz::RemoveVariableFromVP { Index } {
  variable Data
  Viewport::UnAssign $Data(Frame) $Viewport::Data(VP) [lindex $Data(Fields) $Index]	; # Enlever variable du viewport
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

