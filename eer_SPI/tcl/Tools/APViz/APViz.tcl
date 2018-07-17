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
      set Label(Model)		{ "Données" "Data" }
      set Label(Source)		{ "Source" "Source" }
      set Label(Run)		{ "Run" "Run" }
      set Label(Type)		{ "Type" "Type" }
      set Label(Variable)	{ "NomVar" "VarName" }
      
      set Value(NbLayers)	0	; # Nombre de couches creees au total
      set Value(NbCalcLayers)	0	; # Nombre de couches de calcul
      
      set Value(Formula)	 ""	; # Formule de la couche de calcul (textvariable du entry pour la selection de formule)
      set Value(UneditedFormula) ""	; # Formule sans remplacement de variable
      set Value(Date)		 ""
      set Value(HardcodedDate)	 {20180704 20180705 20180706}	; #TODO: FETCH DATES FROM FOLDER
      
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
         
         eval projection configure ${::APViz::Data(Frame)} $Params(Projection)
   
         foreach vp [Page::Registered ${::APViz::Data(Frame)} Viewport] {
            eval ${::APViz::Data(Frame)}.page.canvas itemconfigure $vp $Params(Viewport)
         }
         
         #----- Refleter les valeurs dans l'interface de configuration des parametres
         ::Viewport::ConfigGet ${::APViz::Data(Frame)} ${Viewport::Data(VP)}
         ::Viewport::ConfigPut ${::APViz::Data(Frame)} ${Viewport::Data(VP)}
      
         #----- Build product layer interface
         ::APViz::DeleteWidget $Widget.range	; # Liberer le widget
         
         labelframe $Widget.range -text [lindex $Label(Layer) $GDefs(Lang)]
         
         frame $Widget.range.variableGrid	; #Frame pour le grid
         
         #----- Column titles
         label $Widget.range.variableGrid.mod 	-text [lindex $Label(Model) $GDefs(Lang)] 	-width 8
         label $Widget.range.variableGrid.var 	-text [lindex $Label(Variable) $GDefs(Lang)] 	-width 7
         label $Widget.range.variableGrid.lev 	-text [lindex $Label(Level) $GDefs(Lang)] 	-width 7
         label $Widget.range.variableGrid.src 	-text [lindex $Label(Source) $GDefs(Lang)] 	-width 7
         label $Widget.range.variableGrid.run 	-text [lindex $Label(Run) $GDefs(Lang)] 	-width 4
         label $Widget.range.variableGrid.hour 	-text [lindex $Label(Hour) $GDefs(Lang)] 	-width 5
         
         checkbutton $Widget.range.variableGrid.runLock -variable ::APViz::${Product}::Value(RunLock) -onvalue True -offvalue False \
               -image VCRLOCK -indicatoron 0 -relief sunken -bd 1 -overrelief raised -offrelief flat -selectcolor $GDefs(ColorFrame)
               
         checkbutton $Widget.range.variableGrid.hrLock -variable ::APViz::${Product}::Value(HourLock) -onvalue True -offvalue False \
               -image VCRLOCK -indicatoron 0 -relief sunken -bd 1 -overrelief raised -offrelief flat -selectcolor $GDefs(ColorFrame)
         
         grid $Widget.range.variableGrid 	-column 0 -row 1 -padx 0.2
         grid $Widget.range.variableGrid.mod 	-column 1 -row 0 -padx 0.2
         grid $Widget.range.variableGrid.run 	-column 2 -row 0 -padx 0.2
         grid $Widget.range.variableGrid.runLock -column 3 -row 0 -padx 0.2
         grid $Widget.range.variableGrid.hour 	-column 4 -row 0 -padx 0.2
         grid $Widget.range.variableGrid.hrLock 	-column 5 -row 0 -padx 0.2
         grid $Widget.range.variableGrid.src 	-column 6 -row 0 -padx 0.2
         grid $Widget.range.variableGrid.var 	-column 7 -row 0 -padx 0.2
         grid $Widget.range.variableGrid.lev 	-column 8 -row 0 -padx 0.2
         
         CreateLayers $Product $Layers $Widget	; # Creation des couches

         pack $Widget.range -side top -fill x -anchor nw
         
         #----- Configurable date
         
         frame $Widget.range.dateConfig
         label $Widget.range.dateConfig.lbl -text "Date: " -width 36 -anchor e 
         ComboBox::Create $Widget.range.dateConfig.cb APViz::${Product}::Value(Date) editclose sorted nodouble -1 {} 14 5 "APViz::InitializeVars $Product"
         set APViz::Data(DateCBWidget) $Widget.range.dateConfig.cb
         set dateList [APViz::FetchDates $Product $Value(Models,0) $Value(Sources,0)]
         set APViz::${Product}::Value(Date) [lindex $dateList [expr [llength $dateList] - 1]]
         
         checkbutton $Widget.range.dateConfig.dateLock -variable ::APViz::${Product}::Value(dateLock) -onvalue True -offvalue False \
               -image VCRLOCK -indicatoron 0 -relief sunken -bd 1 -overrelief raised -offrelief flat -selectcolor $GDefs(ColorFrame) 

         bind $Widget.range.dateConfig.cb.select <Return> "APViz::DateBinding $Product"
         
         grid $Widget.range.dateConfig 		-column 0 -row 0 -padx 1
         grid $Widget.range.dateConfig.lbl 	-column 1 -row 0 -padx 1
         grid $Widget.range.dateConfig.cb 	-column 2 -row 0 -padx 1
         grid $Widget.range.dateConfig.dateLock 	-column 3 -row 0 -padx 1
         
         Bubble::Create $Widget.range.dateConfig.dateLock $APViz::Bubble(BlockDate)

         ::APViz::DeleteWidget $Widget.add	; # Liberer le widget
         
         menubutton $Widget.add -image PLUS -text [lindex $Label(AddLayer) $GDefs(Lang)] -compound left -bd 1 -menu $Widget.add.menu
         
         menu $Widget.add.menu
         set no 0
         foreach layer $Layers {
            #----- Layer description
            regsub \(True:|False:\) $layer "" desc
            $Widget.add.menu add command -label "Type$no: $desc" -command "APViz::${Product}::CreateLayers $Product $layer $Widget True $no"
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
      
      proc CreateRangeWidget { Product Style Path Index Options IsSpinBox Width} {
         variable Range
         variable Value
         
         if { [string index $Style 0] eq "<" } {
            set rangeType [string trim $Style {< >}]
            if $IsSpinBox {
               spinbox $Path -values $Range($rangeType) -width $Width -textvariable APViz::${Product}::Value($Options,$Index) \
               -command "::APViz::${Product}::AdjustLockedValues $Options $Index $Product ; APViz::AssignVariable $Product $Index " 
            } else {
               regsub .range\[a-z,A-Z,0-9,.,_\]* $Path "" widget	; # Pour la mise a jour des spinbox
               ComboBox::Create $Path APViz::${Product}::Value($Options,$Index) noedit unsorted nodouble -1 $Range($rangeType) $Width 8 \
               "APViz::AssignVariable $Product $Index ; ::APViz::${Product}::AdjustIDBubble $Path $Index ; ::APViz::${Product}::AdjustSpinboxValues $widget False"
            }
            
            set ranges [split $Range($rangeType)]
            if { [lindex $ranges 0] ne "" } {
               set APViz::${Product}::Value($Options,$Index) [lindex [split $Range($rangeType)] 0]
            } else {
               set APViz::${Product}::Value($Options,$Index) [lindex [split $Range($rangeType)] 1]	; # In case first elem in Ranges (after split)is a space
            }
         } else {
            label $Path -width $Width -text $Style -textvariable APViz::${Product}::Value($Options,$Index)
            set APViz::${Product}::Value($Options,$Index) $Style
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
      
      proc CreateLayers { Product Layers Widget {IsAddedLayer False} {LayerType 0}} {
         variable Value
         variable RowID
         
         set no $Value(NbLayers)
         foreach layer $Layers {
            #----- Extract layer parts
            lassign [split $layer :] toggle model var level hour run dataSrc
            
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
            CreateRangeWidget $Product $level	$Widget.range.variableGrid.layer${no}_level 	$no Levels true 5
            CreateRangeWidget $Product $hour 	$Widget.range.variableGrid.layer${no}_hour 	$no Hours true 5
            CreateRangeWidget $Product $run 	$Widget.range.variableGrid.layer${no}_run 	$no Runs true 5
            set defaultVar [CreateRangeWidget $Product $var 	$Widget.range.variableGrid.layer${no}_var 	$no Vars false 5]
            set defaultSrc [CreateRangeWidget $Product $dataSrc 	$Widget.range.variableGrid.layer${no}_dataSrc 	$no Sources false 5]
            
            #----- Definir le numero de tab a ouvrir dans la fenetre de configuration
            set tab 1					; # 1: Tab Champs
            if {[expr {"$defaultSrc" eq "BURP"}]} {
               set tab 2					; # 2: Tab Observations
            }
            
            button $Widget.range.variableGrid.layer${no}_delete 	-image DELETE -bd 1 	-relief flat -overrelief raised -command "APViz::${Product}::DeleteLayer $Widget $no $Product $defaultSrc"
            button $Widget.range.variableGrid.layer${no}_param 	-image PARAMS -bd 1 	-relief flat -overrelief raised -command "APViz::SetParam $no $Product ; SPI::Params . $tab"
            
            set RowID(Layer$no) [expr $no - $RowID(LayerAdjustment)]
            #label $Widget.range.variableGrid.layer${no}_rowID	-text $RowID(Layer$no)
            
            #----- Place widgets in grid	
            set itemList [list toggle model run hour dataSrc var level param delete]
            set colNb 0
            foreach item $itemList {
               if {($item eq "run") || ($item eq "hour")} {
                  grid $Widget.range.variableGrid.layer${no}_$item	-column $colNb -row [expr $no + 1] -columnspan 2 -padx 0.1
                  incr colNb
               } else {
                  grid $Widget.range.variableGrid.layer${no}_$item	-column $colNb -row [expr $no + 1] -padx 0.1
               }
               set fieldIDTemp FLD$RowID(Layer$no)_$defaultVar
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

         APViz::RemoveVariableFromVP ${::APViz::Data(LayerIDs)} $RowID(Layer$Index) [expr {"$Src" ne "BURP"}]	; # Enlever variable du Viewport
         
         incr RowID(LayerAdjustment)							; # Ajuster le rowID
         set itemList [list toggle model var level run hour dataSrc param delete]
            
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
            APViz::RemoveVariableFromVP ${::APViz::Data(LayerIDs)} $lastIndex [expr {"$Src" ne "BURP"}]		; # Unassign ssi l'index a supprimer n'est pas la derniere rangee
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
               #AdjustIDBubble $Widget.range.variableGrid.layer${i} $i			; # Change row id in bubble
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
            
            Option::Create $Widget.calc.$no.formula "" "APViz::${Product}::Value(Formula,$no) APViz::${Product}::Value(Formula,$no)" 1 22 ${::APViz::Data(FormulaNames)} \
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
            
            Bubble::Create $Widget.calc.$no.varA "VarA"
            Bubble::Create $Widget.calc.$no.varB "VarB"
            button $Widget.calc.$no.param -image PARAMS -bd 1 	-relief flat -overrelief raised -command "APViz::SetParam $no $Product True ; SPI::Params . 1"
            button $Widget.calc.$no.delete -image DELETE -bd 1 	-relief flat -overrelief raised -command "APViz::${Product}::DeleteCalcLayer $Widget.calc.$no $no $Product"
            
            set RowID(Calc$no) [expr $no - $RowID(CalcAdjustment)]
            
            pack $Widget.calc.$no.check $Widget.calc.$no.formula $Widget.calc.$no.lblA $Widget.calc.$no.varA \
               $Widget.calc.$no.lblB $Widget.calc.$no.varB $Widget.calc.$no.param $Widget.calc.$no.delete -side left -fill x -expand true
            
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
   
   APViz::InitializeVars $product
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
   #set date	[clock format [clock seconds] -format %Y%m%d]				; # Today's date in format AAAAMMDD
   
   set date $Value(Date)

   if {![APViz::ValidateDate $Product $date]} {
      return
   }
   
   #----- Verifier si tous les champs sont remplis
   if {[ APViz::AreFieldsFilled $model $var $lev $run $hour $src $date ]} {
   
      if {$src eq "BURP"} {
         set timestamp ${date}${run}_
         set filepath $DataSrc(OBS,$model)/$timestamp
         puts "BURP Filepath for $var: $filepath"
         
         #----- Liberer l'observation
         if {[metobs is [lindex $Data(LayerIDs) $RowID(Layer$Index)]]} {
            APViz::RemoveVariableFromVP $Data(LayerIDs) $RowID(Layer$Index) False
         }

         set obsID OBS$RowID(Layer$Index)_${var}
         
         #TODO: move to config files
         metobs create $obsID $filepath
         dataspec create $obsID
         dataspec configure $obsID -desc "$model (${timestamp}_)" -size 10 -icon CIRCLE -color black -colormap CM0 \
            -mapall True -rendertexture 1 -rendercontour 1 -rendervalue 1 -font XFont12 -intervals { 1 5 10 15 20 30 40 50 75 100 125 150 200 } -active $Value(Toggle,$Index)

         set lst [list [list 0 0 $var { }]]
         metmodel define [metobs define $obsID -MODEL] -items $lst -spacing 10
         metmodel configure [metobs define $obsID -MODEL] $var -dataspec $obsID
         set Data(LayerIDs) [lreplace $Data(LayerIDs) $RowID(Layer$Index) $RowID(Layer$Index) $obsID]
         
         if {[lsearch -exact [Viewport::Assigned $Data(Frame) $Viewport::Data(VP)] $obsID] eq -1} {
            Viewport::Assign $Data(Frame) $Viewport::Data(VP) $obsID 1
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
               APViz::RemoveVariableFromVP $Data(LayerIDs) $RowID(Layer$Index)		; # Enlever la variable courante du VP pour cette couche
            }
            
            set levelType [ APViz::GetLevelType $src ]
            set fieldID FLD$RowID(Layer$Index)_${var}
            
            if {[catch {fstdfield read $fieldID $fileID -1 "" [subst {$lev $levelType}] -1 -1 "" $var }]} {
               ::Dialog::Info . $Lbl(InvalidField)
               return
            } else {
               set Data(LayerIDs) [lreplace $Data(LayerIDs) $RowID(Layer$Index) $RowID(Layer$Index) $fieldID]
            }
            
            #TODO: Cleanup
            if { [info exist Params($var)] } {
               catch { 
                  eval fstdfield configure $fieldID $Params($var) 
               }
            } elseif { [info exist Params(${var}$lev)] } {
               catch { 
                  eval fstdfield configure $fieldID $Params(${var}$lev) 
               }
            } elseif { [info exist Params(${var}$hour)] } {
               catch { 
                  eval fstdfield configure $fieldID $Params(${var}$hour) 
               }
            }
            
            #----- Si la colormap n'existe pas deja, creer la bonne colormap
            set colormapName $var$Index
            if {[lsearch -exact $Data(Colormaps) $colormapName] < 0} {
               set configColormap [fstdfield configure $fieldID -colormap]
               colormap create $colormapName
               colormap copy $colormapName $configColormap
               
               #----- Ajouter dans la liste des colormaps pour retrouver l'original
               puts "Creating new colormap id for pair : [list $colormapName $configColormap]"
               lappend Data(ColormapPairs) [list $colormapName $configColormap]
               lappend Data(Colormaps) $colormapName
            }
            
            fstdfield configure $fieldID -colormap $colormapName
            
            fstdfield configure $fieldID -active $Value(Toggle,$Index)
            AttributeColor $var $fieldID						; # Changer la couleur si la variable a deja ete assignee
            
            #----- Assigner seulement si n'est pas assigne
            if {[lsearch -exact [Viewport::Assigned $Data(Frame) $Viewport::Data(VP)] $fieldID] eq -1} {
               Viewport::Assign $Data(Frame) $Viewport::Data(VP) $fieldID 1
            }

         } else {
            puts "File $filepath not available."
            
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

proc APViz::GetInitialColormap { Colormap } {
   variable Data
   
   foreach pair $Data(ColormapPairs) {
      puts "Analyzing pair : $pair"
      if {[lindex $pair 0] eq $Colormap} {
         puts "Found corresponding colormap to $Colormap : [lindex $pair 1]"
         return [lindex $pair 1]
      }
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
            
            #----- Assigner au ViewPort
            if {[lsearch -exact [Viewport::Assigned $Data(Frame) $Viewport::Data(VP)] $resultFieldID] eq -1} {
               Viewport::Assign $Data(Frame) $Viewport::Data(VP) $resultFieldID
            }
            
            fstdfield configure $resultFieldID -active $isActivated
            Viewport::UpdateData $Data(Frame) $Viewport::Data(VP)
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
   } else {
      set ID [lindex $Data(LayerIDs) $RowID(Layer$Index)] 
      set isActivated $Value(Toggle,$Index)
   }

   if {[fstdfield is $ID]} {
      fstdfield configure $ID -active $isActivated
   } elseif {[metobs is $ID]} {
      set startIndex [expr [string first _ $ID] + 1]
      set var [string range $ID $startIndex [string length $ID]]
      metmodel configure [metobs define $ID -MODEL] $var -active $isActivated 
   }

   Viewport::UpdateData $Data(Frame) $Viewport::Data(VP)
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
         if {[lsearch -exact [Viewport::Assigned $Data(Frame) $Viewport::Data(VP)] $ID] eq -1} {
            return False
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
   APViz::Source $filepath $Data(Tab)
   set Data(ConfigPath) $filepath
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

proc APViz::DateBinding { Product } {
   variable ${Product}::Value

   set result [APViz::ValidateDate $Product $Value(Date)] 
   if {$result} {
      APViz::InitializeVars $Product
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
   variable ${Product}::Value

   puts "FETCHING DATES"
   if {$Src eq "BURP"} {
      set path $DataSrc(OBS,$Model)/
   } else {
      set path $DataSrc(${Model},${Src})/
   }

   set fileList [glob -nocomplain -tails -path $path *]
   set dateList {}

   foreach file $fileList {
      if {![file isdirectory ${path}$file]} {
         set date [string range $file 0 7]		; # Format nomFichier: AAAAMMDDRR_HHH
         if {[lsearch -exact $dateList $date] eq -1} {
            lappend dateList $date
         }
      }
      
      #----- Speed up dateList construction, typically: containing 7 dates
      if {[llength $dateList] > 6} {
         puts "ALL DATES FOUND! : $dateList"
         break
      }
   }

   set dateList [lreplace [lsort $dateList] 0 0]
   puts "New dates: $dateList"

   if {($Data(DateCBWidget) ne "") && [winfo exists $Data(DateCBWidget)]} {
      ComboBox::DelAll $Data(DateCBWidget)
      ComboBox::AddList $Data(DateCBWidget) $dateList
   }

   set Value(Dates) $dateList
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

   if {$Data(CurrentProduct) ne ""} {
      variable $Data(CurrentProduct)::Value
      variable $Data(CurrentProduct)::RowID
      
      #----- Verify if colormaps have changed
      set colormapLst [APViz::ManageColormaps $Data(CurrentProduct)]
      puts "ColormapList: $colormapLst"
      
      set filename [file tail $Path]
      puts "PATH: $Path"
      set fileID [open $Path w]
      
      #----- Get data from original config file
      set origFileID [open $Data(ConfigPath) r]
      set origFileData [read $origFileID]
      close $origFileID
      
      set origData [split $origFileData "\n"]

      set varConfigsIndex [lsearch -glob $origData "*rendercontour*"]
      set rangesIndex [lsearch -glob $origData "*Range*"]
      set layersIndex [lsearch -exact $origData "set Layers \{"] 
      
      #TODO: fetch all current data
      #TEMPO: copy all settings before layers
      set comment {
      for {set i 0} {$i < $layersIndex} {incr i} {
      puts $fileID \t[lindex $origData $i]
      }
      }
      
      #----- COPY TIL VARCONFIGS
      APViz::WriteConfigSection $fileID $origData 0 $varConfigsIndex

      #----- Variable Style Configs     -> what to do when several vars of same type?
      set configLst [APViz::GetVariableConfigs $Data(CurrentProduct) $colormapLst]
      foreach config $configLst {
         puts $fileID $config
      }
      
      #----- Ranges
      #----- COPY Ranges section
      #TODO DEFAULT VALUES
      puts "copying Range section: From $rangesIndex to $layersIndex" 
      APViz::WriteConfigSection $fileID $origData $rangesIndex $layersIndex
      
      puts $fileID "set Layers \{"
      #----- Layers (On:Model:Var:Level:Hour:Interval:Run:Source)
      for {set i 0} {$i < $Value(NbLayers)} {incr i} {
         if {[set rowID $RowID(Layer$i)] >= 0} {
            puts $fileID \t[lindex $Data(Layers) $Value(LayerType,$rowID)]
         }
      }
      puts $fileID "\}"
      
      close $fileID
      
      APViz::UpdateProductInterface
   }
}

##### NOT USED RIGHT NOW
proc APViz::WriteRangesConfigs { FileID DataSource } {
   #GetDefaultValues
   for {set i 0} {$i < $Value(NbLayers)} {incr i} {
      if {[set rowID $RowID(Layer$i)] >= 0} {
         #----- Ajouter Layer
         set checked 	[expr {$Value(Toggle,$i)?True:False}]
         set model 	$Value(Models,$i)
         set var		$Value(Vars,$i)
         set lev		$Value(Levels,$i)
         set run		$Value(Runs,$i)
         set hour	$Value(Hours,$i)
         set src		$Value(Sources,$i)
         
         set layer "\t$checked:$model:$var:$lev:$hour:$run:$src"
         puts $fileID $layer
      }
   }

   #----- Deplacer la valeur au debut de la liste
   set rangeIndexes [lsearch -glob -all $DataSource "*Range(*)*"]

   foreach index $rangeIndexes {
      puts "Treating "
   }
}

#----------------------------------------------------------------------------
# Nom      : <APViz::ManageColormaps>
# Creation : Juillet 2018 - C. Nguyen - CMC/CMOE
#
# But      : Generer un fichier de configuration
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
            puts "IS $name MODIFIED? [set isModified [colormap modified $name]]"
            
            set initialColormap [APViz::GetInitialColormap $name]
            
            if {$isModified} {
               #-----Create new name by adding nunmber or changing number at the end of the original name
               set derivatives [glob -nocomplain -tails -path $DataSrc(Colormaps) $name*.rgba]
               puts "Derivatives for $name: $derivatives"
               
               set nbr 1
               set newName ${initialColormap}$nbr
               while {[lsearch -exact $derivatives $newName.rgba] >= 0} {
                  incr nbr
                  set newName ${initialColormap}$nbr
               }

               colormap create $newName
               colormap copy $newName $name
               colormap write $newName $DataSrc(Colormaps)/${newName}.rgba
               puts "Creating new colormap $newName    ---   [colormap is $newName]"
               
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
         set var	$Value(Vars,$i)
         set hour	$Value(Hours,$i)
         set ID [lindex $Data(LayerIDs) $rowID]
         
         #---- TODO: GERER METOBS
         set comment {
         if {[fstdfield is $ID]} {
            set command fstdfield
         } else {
            set command metobs
         }
         }
         
         #----- Recuperate field configurations
         #set colorMap 	[fstdfield configure $ID -colormap]
         set colorMap           [lindex $ColorMaps $RowID(Layer$i)]
         set color 	        [fstdfield configure $ID -color]
         set renderContour      [fstdfield configure $ID -rendercontour]
         set mapAll 	        [fstdfield configure $ID -mapall]
         set intervalMode 	[fstdfield configure $ID -intervalmode]
         set width 	        [fstdfield configure $ID -width]

         set params "set Params(${var}$hour) \"-colormap $colorMap -color $color -font XFont12 -width $width -rendercontour $renderContour -mapall $mapAll -intervalmode $intervalMode \""
         lappend configLst $params
         puts $params
      }
   }
   return $configLst
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

proc APViz::WriteConfigSection { FileID DataSource StartIndex EndIndex } { 
   for {set i $StartIndex} {$i < $EndIndex} {incr i} {
      puts $FileID [lindex $DataSource $i]
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

proc APViz::InitializeVars { Product } {
   variable ${Product}::Value
   variable ${Product}::RowID
   variable Data

   for {set idx 0} {$idx < $Value(NbLayers)} {incr idx} {
      if {$RowID(Layer$idx) >= 0} {
         APViz::AssignVariable $Product $idx
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
# Remarques :
#
#----------------------------------------------------------------------------

proc APViz::ReinitializeVP { } {
   variable Data

   Viewport::UnAssign $Data(Frame) $Viewport::Data(VP)	; # Enlever toutes les variables du viewport
   puts "===================== REINITIALIZING ========================="
   puts "DataLAYERS: $Data(LayerIDs) "
   #----- Liberer les ID
   foreach ID $Data(LayerIDs) {
      if {[regexp FLD $ID]} {
         puts "Freeing $ID"
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
      #----- Reinitialiser les textvariables pour calcul
      for {set i 0} {$i < $Value(NbCalcLayers)} {incr i} {
         set Value(Formula,$i) ""
      }
   }
   
   puts "COLORMAPS: $Data(Colormaps)"
   foreach colormap $Data(Colormaps) {
      puts "FREEING $colormap"
      colormap free $colormap
   }

   set Data(LayerIDs) {}
   set Data(CalcIDs) {}
   set Data(CurrentProduct) ""
   set APViz::Data(Layers) {}
   set Data(Colormaps) {}
   set Data(ColormapPairs) {}

   puts "=================== DONE REINITIALIZING ======================="
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

proc APViz::RemoveVariableFromVP { IDList Index {IsFSTDField True} } {
   variable Data

   set ID [lindex $IDList $Index]

   Viewport::UnAssign $Data(Frame) $Viewport::Data(VP) $ID	; # Enlever variable du viewport
   set dataType ""

   if {$IsFSTDField} {
      fstdfield free $ID
      set dataType FLD
   } else {
      metobs free $ID
      set dataType OBS
   }

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

   if {[lsearch -exact [Viewport::Assigned $Data(Frame) $Viewport::Data(VP)] $id] ne -1} {
      if {[string equal -length 3 $id OBS ]} {
         ::Obs::ParamUpdate $id
      } else {
         ::FSTD::ParamUpdate $id
      }
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
#	<Product> : Le nom du produit selectionne (aussi le namespace) 
#	<Date>	  : La date a valider
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc APViz::ValidateDate { Product Date } {
   variable Lbl
   variable ${Product}::Value

   set result [expr [lsearch -exact $Value(Dates) $Date] >= 0]

   if {!$result} {
      puts "Looking for $Date in $Value(Dates)   FAILED"
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

   puts "UPDATE CHECK $lastModifiedDate"

   if {$lastModifiedDate > [lindex $Value(Dates) [expr [llength $Value(Dates)] -1 ]]} {
      puts "Updating dates   || old date: [lindex $Value(Dates) [expr [llength $Value(Dates)] -1 ]]"
      #----- Afficher message
      .apviz.dock.coo insert 0 [lindex $Lbl(FetchingDates) $GDefs(Lang)]
      APViz::FetchDates $Product $model $src
      after [expr {1000*30}] ".apviz.dock.coo delete 0 [string length [.apviz.dock.coo get]]"
   }

   if {!$Value(dateLock) && ($Value(Date) ne [lindex $Value(Dates) [expr [llength $Value(Dates)] -1 ]])} {
      puts "Changing last modified date   ---   $Value(Date) to $lastModifiedDate"
      puts "Dates: $Value(Dates)"
      set Value(Date) [lindex $Value(Dates) [expr [llength $Value(Dates)] -1 ]]
      #----- Afficher message
      .apviz.dock.coo insert 0 [lindex $Lbl(UpdatingDate) $GDefs(Lang)]
      after [expr {1000*30}] ".apviz.dock.coo delete 0 [string length [.apviz.dock.coo get]]"
      APViz::InitializeVars $Product
   }

   set Data(AutoUpdateEventID) [after [expr {1000*60*60}] APViz::UpdateAvailableDates $Product]	; # Update a chaque 10min:1000*60*10

   puts "FINISHED CHECKING"
}