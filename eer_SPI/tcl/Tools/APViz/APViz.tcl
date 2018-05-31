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
##-------------------------------------------------------------------------------
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
      variable Toggle
      
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
      
      proc Load { Path Product Widget } {
	global GDefs
	variable Label
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
	
	set no 0
	foreach layer $Layers {
	  #----- Extract layer parts
	  lassign [split $layer :] toggle model var level hour interval run dataSource
	  
	  #----- Toggle On/Off
	  checkbutton $Widget.range.variableGrid.layer${no}_toggle -anchor w -var APViz::${Product}::Toggle($no)
	  if {$toggle} {
	    $Widget.range.variableGrid.layer${no}_toggle select
	  } else {
	    $Widget.range.variableGrid.layer${no}_toggle deselect
	  }
	  
	  # CreateRangeWidget { Style Path Index Options IsSpinBox }
	  CreateRangeWidget $Product $model $Widget.range.variableGrid.layer${no}_model $no Models true		; #Range names have to be the same in all config files
	  CreateRangeWidget $Product $var $Widget.range.variableGrid.layer${no}_var $no Vars false
	  CreateRangeWidget $Product $level $Widget.range.variableGrid.layer${no}_level $no Levels true
	  CreateRangeWidget $Product $hour $Widget.range.variableGrid.layer${no}_hour $no Hours true
	  CreateRangeWidget $Product $run $Widget.range.variableGrid.layer${no}_run $no Runs true
	  CreateRangeWidget $Product $dataSource $Widget.range.variableGrid.layer${no}_dataSource $no Sources true
	  
	  button $Widget.range.variableGrid.layer${no}_delete -image DELETE -bd 1 -relief flat -overrelief raised
	  button $Widget.range.variableGrid.layer${no}_param -image PARAMS -bd 1 -relief flat -overrelief raised -command { SPI::Params }
	  
	  
	  #----- Place widgets in grid	
	  grid $Widget.range.variableGrid.layer${no}_toggle	-column 0 -row [expr $no + 1] -padx 0.1
	  grid $Widget.range.variableGrid.layer${no}_model	-column 1 -row [expr $no + 1] -padx 0.1
	  grid $Widget.range.variableGrid.layer${no}_var	-column 2 -row [expr $no + 1] -padx 0.1
	  grid $Widget.range.variableGrid.layer${no}_level	-column 3 -row [expr $no + 1] -padx 0.1
	  grid $Widget.range.variableGrid.layer${no}_run	-column 4 -row [expr $no + 1] -padx 0.1
	  grid $Widget.range.variableGrid.layer${no}_hour	-column 5 -row [expr $no + 1] -padx 0.1
	  grid $Widget.range.variableGrid.layer${no}_dataSource	-column 6 -row [expr $no + 1] -padx 0.1
	  grid $Widget.range.variableGrid.layer${no}_param	-column 7 -row [expr $no + 1] -padx 0.1	
	  grid $Widget.range.variableGrid.layer${no}_delete	-column 8 -row [expr $no + 1] -padx 0.1	

	  incr no
	}
	pack $Widget.range -side bottom -fill both -expand True
      }
      
      proc CreateRangeWidget { Product Style Path Index Options IsSpinBox } {
	variable Range
	variable Value
	
	if { [string index $Style 0] eq "<" } {
	  if $IsSpinBox {
	    spinbox $Path -values $Range($Options) -width 5 -textvariable APViz::${Product}::Value($Options,$Index) -command "APViz::DisplayVariable $Product $Options $Index"	; #-var APViz::${Product}::Range(Variables) 
	  } else {
	    ComboBox::Create $Path APViz::${Product}::Value($Options,$Index) noedit unsorted nodouble -1 $Range($Options) 5 8 "APViz::DisplayVariable $Product $Options $Index" 
	  }
	} else {
	    label $Path -width 5 -text $Style
	}
      }
   }
   
   ${product}::Load $Path $product $Widget
}

#-------------------------------------------------------------------------------
# Nom      : <APViz::DisplayVariable>
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

proc APViz::DisplayVariable { Product Option Index } {
  variable Data
  variable Value
  
  eval set tog \$APViz::${Product}::Toggle(0)
  eval set val \$APViz::${Product}::Value($Option,$Index)
  puts "Display: $val , $tog"
}