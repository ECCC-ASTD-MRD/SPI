#===============================================================================
# Environnement Canada
# Centre Meteorologique Canadian
# 2100 Trans-Canadienne
# Dorval, Quebec
#
# Projet   : Package d'interface pour SPI
# Fichier  : APViz.tcl
# Creation : May 2018
#
# Description:
#   Outil développé pour A&P permettant de visualiser les données météorologiques 
#   de façon dynamique.
# 
# Fonctions:
#
#   APViz::Source                 { Path Widget }
#   APViz::AreFieldsFilled        { Model Var Level Run Hour Source Date }
#   APViz::AssignVariable         { Product Index }
#   APViz::AssignDZ               { Product Index Model Var Lev FileID FieldID LevelType }
#   APViz::CalculateExpression    { Product Index}
#   APViz::Check                  { Product Index { IsCalc False }}
#   APViz::Close                  { }
#   APViz::CloseFiles             { }
#   APViz::ConstructTreeBranches  { Tree ParentNode Path Level }
#   APViz::CreateColormaps        { }
#   APViz::CreateFileTree         { Path }
#   APViz::CreateFormulaLists     { }
#   APViz::DateBinding            { }
#   APViz::DeleteWidget           { Widget }
#   APViz::FetchAllDates          { Product }
#   APViz::FetchDates             { Product Model Src }
#   APViz::FilePathDefine         { Path }
#   APViz::GenerateConfigFile     { Path }
#   APViz::GetAllFieldsWithOp     { Product Nb Operator { OnlyChecked False } }
#   APViz::GetImageDirPath        { }
#   APViz::GetInitialColormap     { Colormap }
#   APViz::GetLevelType           { Source }
#   APViz::GetTreeId              { Tree Branch Leaf }
#   APViz::GetVariableConfigs     { Product ColorMaps }
#   APViz::GetVPId                { VPno }
#   APViz::InitializeVars         { }
#   APViz::ManageColormaps        { Product Path }
#   APViz::Refresh                { Product }
#   APViz::ReinitializeVP         { }
#   APViz::RemoveVariableFromVP   { IDList Index }
#   APViz::SaveConfigFile         { }
#   APViz::SaveImg                { }
#   APViz::SelectFiletreeBranch   { Tree Branch Open }
#   APViz::SetParam               { Index Product { IsCalcLayer False }}
#   APViz::Start                  { }
#   APViz::StartBatch             { }
#
#
# Fonctions pour le produit Product:
#
#   APViz::Product::AddCalcLayer         { Product Widget }
#   APViz::Product::AdjustIDBubble       { Widget Index }
#   APViz::Product::AdjustLockedValues   { Option Index Product }
#   APViz::Product::CreateCalcLayers     { Product Widget }
#   APViz::Product::CreateLayers         { Product Layers Widget {IsAddedLayer False} {LayerType 0}}
#   APViz::Product::CreateRangeWidget    { Product Style Path Index Options IsSpinBox Width }
#   APViz::Product::CreateVariableRanges { }
#   APViz::Product::DeleteCalcLayer      { Widget Index Product }
#   APViz::Product::DeleteLayer          { Widget Index Product Src }
#   APViz::Product::Load                 { Path }
#   APViz::Product::Build                { Product Widget }
#   APViz::Product::RemoveFromAlphaDict  { Index {IsLayer true} }
#   APViz::Product::SetEtiketBubble      { Widget Index }
#
#===============================================================================

#----- Lire les sources d'execution
source $GDefs(Dir)/tcl/Tools/APViz/APViz.ctes
source $GDefs(Dir)/tcl/Tools/APViz/APViz.txt
source $GDefs(Dir)/tcl/Tools/APViz/APViz.int

namespace eval APViz {
   ::struct::tree FILETREE
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
      variable CalcLayers
      variable Label
      variable Value
      variable Param
      
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
      set Label(Variable)	{ "Var" "Var" }
      
      set Param(Layout)        ""
      set Param(Title)         ""
      set Param(Description)   ""
      
      set Value(NbLayers)        0   ; # Nombre de couches creees au total
      set Value(NbCalcLayers)    0   ; # Nombre de couches de calcul      
      set Value(Formula)	 ""  ; # Formule de la couche de calcul (textvariable du entry pour la selection de formule)
      set Value(RowIDLayer)      0    ; # Ajustement pour le calcul du rowID pour les couches
      set Value(RowIDCalc)       0    ; # Ajustement pour le calcul du rowID pour les couches de calcul
      
      #----- Initialize default values
      array unset Range
      
      #-------------------------------------------------------------------------------
      # Nom      : <APViz::$product::AddCalcLayer>
      # Creation : Juin 2018 - C. Nguyen - CMC/CMOE -
      #
      # But      : Ajouter une couche de calcul manuel
      #
      # Parametres         :
      #         <Product>  : Produit a afficher (aussi le namespace)
      #         <Widget>   : Path vers le widget
      # Retour:
      #
      # Remarques :
      #
      #-------------------------------------------------------------------------------
      
      proc AddCalcLayer { Product Widget } {
         variable Value
         
         set no $Value(NbCalcLayers)
         frame $Widget.calc.$no
            set alpha [lindex $APViz::Data(Alphas) [expr $no + $Value(NbLayers)]]
            set Value(CLetter,$no) $alpha
            dict set APViz::Data(AlphaDict) $alpha C$no         
            checkbutton $Widget.calc.$no.check -anchor w -var APViz::${Product}::Value(CalcToggle,$no) -text $alpha -command "APViz::Check $Product $no True" \
               -indicatoron False -overrelief raised -offrelief flat
            
            Option::Create $Widget.calc.$no.formula "" "APViz::${Product}::Value(Formula,$no) APViz::${Product}::Value(Formula,$no)" 1 -1 ${::APViz::Data(FormulaNames)} \
               "eval set APViz::${Product}::Value(Formula,$no) \${APViz::${Product}::Value(Formula,$no)}; APViz::Refresh $Product" \
                #${::APViz::Data(Formulas)}
            
            catch { Bubble::Create $Widget.calc.$no.formula.e "Formule"}
            
            bind $Widget.calc.$no.formula.e <Return> "APViz::Refresh $Product"
                           
            label $Widget.calc.$no.lblVP -text "VP:"
            set vpLst {}
            for { set i 1 } { $i <= [llength [Page::Registered ${::APViz::Data(Frame)} Viewport]] } {incr i} {
               lappend vpLst $i
            }

            spinbox $Widget.calc.$no.vp -values $vpLst -width 1 -textvariable APViz::${Product}::Value(CalcVP,$no) -wrap True \
               -command "APViz::Refresh $Product"
            
            button $Widget.calc.$no.param  -image PARAMS -bd 1 -height 16 -relief flat -overrelief raised -command "APViz::SetParam $no $Product True ; SPI::Params . 1"
            button $Widget.calc.$no.delete -image DELETE -bd 1 -height 16 -relief flat -overrelief raised -command "APViz::${Product}::DeleteCalcLayer $Widget.calc.$no $no $Product; "
            
            set Value(RowIDCalc$no) [expr $no - $Value(RowIDCalc)]
            
            pack $Widget.calc.$no.check -side left 
            pack $Widget.calc.$no.formula -side left -fill x -expand true
            pack $Widget.calc.$no.lblVP $Widget.calc.$no.vp $Widget.calc.$no.param $Widget.calc.$no.delete -side left 
            
         pack $Widget.calc.$no -side top -fill x
         incr Value(NbCalcLayers)
         
         #----- Ajouter le ID a la liste
         lappend ${::APViz::Data(CalcIDs)} CALC$Value(RowIDCalc$no)
         return $no
      }
      
      #-------------------------------------------------------------------------------
      # Nom      : <APViz::$product::AdjustIDBubble>
      # Creation : Juin 2018 - C. Nguyen - CMC/CMOE -
      #
      # But      : Ajuster le message dans la bulle d'aide pour que le bon fieldID soit affiche
      #
      # Parametres         :
      #         <Widget>   : Path vers le widget
      #         <Index>    : L'index de la couche
      #
      # Retour:
      #
      # Remarques :
      #
      #-------------------------------------------------------------------------------
      
      proc AdjustIDBubble { Widget Index } {
         variable Value
         
         set itemList [list toggle model var level run hour dataSrc param ip3 vp delete]       
         set Widget [lindex [split $Widget _] 0]

         foreach item $itemList {
            if {[winfo exists ${Widget}_$item]} {
               Bubble::Create ${Widget}_$item [lindex ${::APViz::Data(LayerIDs)} $Value(RowIDLayer$Index)]
            }
         }
      }
      
      #-------------------------------------------------------------------------------
      # Nom      : <APViz::$product::AdjustLockedValues>
      # Creation : Juin 2018 - C. Nguyen - CMC/CMOE -
      #
      # But      : Ajuster les runs et les heures si ces-derniers sont locked
      #
      # Parametres         :
      #         <Option>   : Nom de la colonne
      #         <Index>    : L'index de la couche
      #         <Product>  : Produit a afficher (aussi le namespace)
      #
      # Retour:
      #
      # Remarques :
      #
      #-------------------------------------------------------------------------------
      
      proc AdjustLockedValues { Option Index Product } {
         variable Value
         variable Range
         variable Layers
         
         switch $Option {
            "Models"    { set lockType Model; set idx 1 }
            "Runs"      { set lockType Run;   set idx 2 }
            "Hours"     { set lockType Hour;  set idx 3 }
            "Levels"    { set lockType Level; set idx 6 }
            "Vars"      { set lockType Var;   set idx 5 }
            default     {  APViz::AssignVariable $Product $Index
                           return }
         }
         
         if { $Range(${lockType}Lock) } {
            set newValue $Value($Option,$Index)
            #----- Change all other values
            for { set i 0 } { $i < $Value(NbLayers) } { incr i } {
               set opts [split [lindex $Layers $i] :] 
               if { [string index [lindex $opts $idx] 0]=="<" && ($i!=$Index) && ($Value(RowIDLayer$i)>=0) } {
                  #----- Check if a delta is defined (ex:<Hours>+06)
                 if { [info exists ::APViz::${Product}::Value(Delta$Option,$i)] } {
                     eval set Value($Option,$i) \[format %0${idx}i \[expr [string trimleft $newValue 0]$Value(Delta$Option,$i)\]\]
                  } else {
                     set Value($Option,$i) $newValue
                  }
#                  APViz::AssignVariable $Product $i
               }
            }
            for { set i 0 } { $i < $Value(NbLayers) } { incr i } {
               APViz::AssignVariable $Product $i
            }
         } else {
            APViz::AssignVariable $Product $Index
         }
      }
            
      #-------------------------------------------------------------------------------
      # Nom      : <APViz::$product::CreateCalcLayers>
      # Creation : Aout 2018 - C. Nguyen - CMC/CMOE -
      #
      # But      : Creer l'interface des couches de calcul
      #
      # Parametres        :
      #         <Product> : Produit a afficher
      #         <Widget>  : Nom du widget parent dans lequel afficher l'interface
      # Retour:
      #
      # Remarques :
      #
      #-------------------------------------------------------------------------------
      
      proc CreateCalcLayers { Product Widget } {
         variable Value
         variable CalcLayers
         
         if { ![info exists CalcLayers] } {
            return
         }
         
         foreach layer $CalcLayers {
            set calcIndex [AddCalcLayer $Product $Widget]
            lassign [split $layer :] toggle formulaName vp

            #----- Set defaults
            if { [winfo exists $Widget.calc.$calcIndex.check] } {
               if { $toggle } {
                  $Widget.calc.$calcIndex.check select
               } else {
                  $Widget.calc.$calcIndex.check deselect
               }
            }

            if { ($vp <= [llength [Page::Registered ${::APViz::Data(Frame)} Viewport]]) && [winfo exists $Widget.calc.$calcIndex.vp] } {
               set APViz::${Product}::Value(CalcVP,$calcIndex) $vp
            }

            #----- Verify if formula is defined
            if { [set index [lsearch -exact ${APViz::Data(FormulaNames)} $formulaName]] >= 0 } {
               set APViz::${Product}::Value(Formula,$calcIndex) [lindex ${APViz::Data(Formulas)} $index]
            } else {
               set APViz::${Product}::Value(Formula,$calcIndex) $formulaName
            }
            
            #----- Update formula and create calc layer
            APViz::CalculateExpression $Product $calcIndex
         }
      }
    
      #-------------------------------------------------------------------------------
      # Nom      : <APViz::$product::CreateLayers>
      # Creation : Mai 2018 - C. Nguyen - CMC/CMOE -
      #
      # But      : Creer une couche pour ajouter au viewport
      #
      # Parametres              :
      #         <Product>       : Produit a afficher (aussi le namespace)
      #         <Layers>        : Information sur les ranges de la couche ayant la forme:
      #                                 On:Model:Var:Level:Hour:Interval:Run:Source
      #         <Widget>        : Path vers le widget
      #         <IsAddedLayer>  : Boolean indiquant s'il s'agit d'une couche additionnelle
      #         <LayerType>     : Type (numero) du layer a ajouter
      # Retour:
      #
      # Remarques : Lorsque la variable est entre <>, cela signifie que c'est un range
      #
      #-------------------------------------------------------------------------------

      proc CreateLayers { Product Layers Widget {IsAddedLayer False} {LayerType 0}} {
         global GDefs
         variable Value
         variable Range
         
         set no $Value(NbLayers)
         
         #----- Find default values
         foreach range [array names Range] {
            set def [string range $range 0 end-1]       
            if  { [string index $range end]=="s" && [array names Range $def]=="" } {
               set Range($def) [lindex $Range(${range}) 0]
            }
         }
         
         foreach layer $Layers {
            if  { $layer=="" } {
               break
            }
            
            #----- Extract layer parts
            lassign [split $layer :] toggle model run hour dataSrc var level ip3 vp etiket
                        
            #----- Toggle On/Off            
            set alpha [lindex $APViz::Data(Alphas) [expr $no + $Value(NbCalcLayers)]]
            dict set APViz::Data(AlphaDict) $alpha L$no
            checkbutton $Widget.range.variableGrid.layer${no}_toggle -anchor w -var APViz::${Product}::Value(Toggle,$no) -text $alpha \
               -command "APViz::Check $Product $no" -indicatoron False -overrelief raised -offrelief flat
            
            set Value(Letter,$no) $alpha
            
            if { !$toggle } {
               $Widget.range.variableGrid.layer${no}_toggle deselect
            } elseif {!$IsAddedLayer} {
               $Widget.range.variableGrid.layer${no}_toggle select
            }

            #----- CreateRangeWidget { Product Style Path Index Options IsSpinBox Width Default}
            CreateRangeWidget $Product $model   $Widget.range.variableGrid.layer${no}_model     $no Models false 5 
            CreateRangeWidget $Product $hour    $Widget.range.variableGrid.layer${no}_hour      $no Hours true 3 
            CreateRangeWidget $Product $run     $Widget.range.variableGrid.layer${no}_run       $no Runs true 2 
            CreateRangeWidget $Product $ip3     $Widget.range.variableGrid.layer${no}_ip3       $no IP3 true 2 
            CreateRangeWidget $Product $vp      $Widget.range.variableGrid.layer${no}_vp        $no Viewports false 2

            set defaultVariable [CreateRangeWidget $Product $var $Widget.range.variableGrid.layer${no}_var       $no Vars false -1]
            set defaultSrc [CreateRangeWidget $Product $dataSrc $Widget.range.variableGrid.layer${no}_dataSrc   $no Sources false -1]
            
            CreateRangeWidget $Product $level $Widget.range.variableGrid.layer${no}_level $no Levels true -1
            
            #----- Definir le numero de tab a ouvrir dans la fenetre de configuration
            set tab 1                                   ; # 1: Tab Champs
            if {[expr {"$defaultSrc" eq "BURP"}]} {
               set tab 2                                ; # 2: Tab Observations
            }
            
            button $Widget.range.variableGrid.layer${no}_delete -image DELETE -bd 1 -height 16 -relief flat -overrelief raised -command "APViz::${Product}::DeleteLayer $Widget $no $Product $defaultSrc"
            button $Widget.range.variableGrid.layer${no}_param  -image PARAMS -bd 1 -height 16 -relief flat -overrelief raised -command "APViz::SetParam $no $Product ; SPI::Params . $tab"
            
            #----- Create entry for etiket and bind
            entry $Widget.range.variableGrid.layer${no}_etiket -width 5 -textvariable APViz::${Product}::Value(Etiket,$no) -bg $GDefs(ColorLight)
            set APViz::${Product}::Value(EtiketWidget,$no) $Widget.range.variableGrid.layer${no}_etiket
            bind $Widget.range.variableGrid.layer${no}_etiket <Return> "APViz::${Product}::SetEtiketBubble $Widget.range.variableGrid.layer${no}_etiket $no; APViz::AssignVariable $Product $no; APViz::Refresh $Product"
            
            set Value(RowIDLayer$no) [expr $no - $Value(RowIDLayer)]
            
            #----- Place widgets in grid        
            set itemList [list toggle model run hour dataSrc var level ip3 vp etiket param delete]
            set colNb 0
            foreach item $itemList {
               grid $Widget.range.variableGrid.layer${no}_$item      -column $colNb -row [expr $no + 1] -padx 0.1
               set fieldIDTemp FLD$Value(RowIDLayer$no)_$defaultVariable
               if {$item ne "etiket"} {
                  Bubble::Create $Widget.range.variableGrid.layer${no}_$item $fieldIDTemp
               }
               incr colNb
            }
            
            if {$IsAddedLayer} {
               APViz::AssignVariable $Product $no
               set APViz::${Product}::Value(LayerType,$no) $LayerType
               APViz::FetchAllDates $Product
            } else {
               set APViz::${Product}::Value(LayerType,$no) [expr $no - $Value(NbLayers)]
            }
            
            incr no
         }
         
         set Value(NbLayers) $no
         if {$IsAddedLayer} {
            APViz::Refresh $Product
         }
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
      
      proc CreateRangeWidget { Product Style Path Index Options IsSpinBox Width } {
         global GDefs
         variable Range
         variable Value

         if { [string index $Style 0] eq "<" } {
            #----- Check for hour delta
            catch { unset Value(Delta$Options,$Index) }
            if { [set delta [lindex [set st [split $Style +]] end]]!=$Style } {
               set Value(Delta$Options,$Index) +[string trimleft $delta 0]
               set Style [lindex $st 0]
            }
            if { [set delta [lindex [set st [split $Style -]] end]]!=$Style } {
               set Value(Delta$Options,$Index) -[string trimleft $delta 0]
               set Style [lindex $st 0]
            }
            
            set rangeType [string trim $Style {< >}]
            # Ne fonctionne que si l'usager suit la regle
            set range     [string range $rangeType 0 end-1]
            
            if { [lsearch -exact [dict get $APViz::Data(RangeNames) $Options] $rangeType] < 0} {
               dict lappend APViz::Data(RangeNames) $Options $rangeType
               dict lappend APViz::Data(Ranges) $Options \{$Range($rangeType)\}
            }
            if { [info exists ::APViz::${Product}::Value(Delta$Options,$Index)] } {
               entry $Path -textvariable APViz::${Product}::Value($Options,$Index) -width [expr $Width+1] -bg $GDefs(ColorLight) -state disabled
            } elseif { $IsSpinBox } {
               spinbox $Path -values $Range($rangeType) -width $Width -textvariable APViz::${Product}::Value($Options,$Index) -bg $GDefs(ColorLight) \
               -command "::APViz::${Product}::AdjustLockedValues $Options $Index $Product; APViz::Refresh $Product" 
               
               #----- Bind with return key
               bind $Path <Return> "::APViz::${Product}::AdjustLockedValues $Options $Index $Product; APViz::Refresh $Product"
               
            } else {
               regsub .range\[a-z,A-Z,0-9,.,_\]* $Path "" widget	; # Pour la mise a jour des spinbox
               ComboBox::Create $Path APViz::${Product}::Value($Options,$Index) noedit unsorted nodouble -1 $Range($rangeType) $Width 6 \
               "::APViz::${Product}::AdjustLockedValues $Options $Index $Product; APViz::Refresh $Product; ::APViz::${Product}::AdjustIDBubble $Path $Index"
            }
           
            #----- Default Values
            set idx [expr $Index-1]
            if { $Index>[expr [llength $Range($range)]-1] } {
               set APViz::${Product}::Value($Options,$Index) [lindex $Range($range) end]
            } else {
               set APViz::${Product}::Value($Options,$Index) [lindex $Range($range) $Index]
            }
            
            #----- Add increment if defined
            if { [info exists ::APViz::${Product}::Value(Delta$Options,$Index)] } {
               if { $Options=="Runs" } { set w 2 } else { set w 3 }
               eval set Value($Options,$Index) \[format %0${w}i \[expr [string trimleft $Value($Options,$Index) 0]$Value(Delta$Options,$Index)\]\]
            }

         } else {
            label $Path -width $Width -text $Style -textvariable APViz::${Product}::Value($Options,$Index)
            set APViz::${Product}::Value($Options,$Index) $Style
         }
         eval set defaultValue \$\{APViz::${Product}::Value($Options,$Index)\}
         return $defaultValue
      }
      
      #-------------------------------------------------------------------------------
      # Nom      : <APViz::$product::CreateVariableRanges>
      # Creation : Aout 2018 - C. Nguyen - CMC/CMOE -
      #
      # But      : Creer les ranges pour les field lists lus dans APViz_Data.tcl
      #
      # Parametres :
      # Retour:
      #
      # Remarques :
      #
      #-------------------------------------------------------------------------------
      
      proc CreateVariableRanges { } {
         variable ::APViz::FieldList
         variable Range

         foreach variableType [array names FieldList] {
            set Range($variableType) $FieldList($variableType)
            #----- Add to dictionary
            if {[lsearch -exact [dict get $APViz::Data(RangeNames) Vars] $variableType] < 0} {
               dict lappend APViz::Data(RangeNames) Vars $variableType
               dict lappend APViz::Data(Ranges) Vars $variableType
            }
         }
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
         variable Value
         
         #----- Remove calc var from VP
         APViz::RemoveVariableFromVP ${::APViz::Data(CalcIDs)} $Value(RowIDCalc$Index)	; # Enlever variable du Viewport
         
         incr Value(RowIDCalc)							; # Ajuster le rowID
            
         #----- Adjust all rowIds below range.variableGrid.layer${no}_toggle
         for {set i 0} {$i < $Value(NbCalcLayers)} {incr i} {
            if {$Value(RowIDCalc$i) > $Value(RowIDCalc$Index)} {
               set Value(RowIDCalc$i) [expr $Value(RowIDCalc$i) - 1]
            }
         }

         #----- Enlever la derniere variable de Data(CalcIDs) vu qu'on a decale tous les fielIds
         set lastIndex [expr [llength ${::APViz::Data(CalcIDs)}] - 1]
         if {$lastIndex != $Value(RowIDCalc$Index)} {
            #----- Remove last index from VP
            APViz::RemoveVariableFromVP ${::APViz::Data(CalcIDs)} $lastIndex		; # Unassign ssi l'index a supprimer n'est pas la derniere rangee
         }
         set Value(RowIDCalc$Index) -1							; # Index supprime, n'est plus affiche
         set ::APViz::Data(CalcIDs) [lreplace ${::APViz::Data(CalcIDs)} $lastIndex $lastIndex ]
         
         #----- Elever la lettre attribuee au field du dictionnaire AlphaDict
         RemoveFromAlphaDict $Index false 
         
         #----- Detruire les widgets
         ::APViz::DeleteWidget $Widget
      }
      
      #-------------------------------------------------------------------------------
      # Nom      : <APViz::$product::DeleteLayer>
      # Creation : Mai 2018 - C. Nguyen - CMC/CMOE -
      #
      # But      : Supprimer une couche et ajustement les numeros de rangees
      #
      # Parametres         :
      #         <Widget>   : Path vers le widget
      #         <Index>    : L'index de la couche a supprimer
      #         <Product>  : Produit a afficher (aussi le namespace)
      # Retour:
      #
      # Remarques :
      #
      #-------------------------------------------------------------------------------
      
      proc DeleteLayer { Widget Index Product Src } {
         variable Value
         
         #----- # Enlever variable du Viewport
         APViz::RemoveVariableFromVP ${::APViz::Data(LayerIDs)} $Value(RowIDLayer$Index)
         
         #----- Ajustement du rowID
         incr Value(RowIDLayer)
         set itemList [list toggle model var level run hour dataSrc ip3 vp etiket param delete]
            
         #----- Adjust all rowIds below range.variableGrid.layer${no}_toggle
         for {set i 0} {$i < $Value(NbLayers)} {incr i} {
            if {$Value(RowIDLayer$i) > $Value(RowIDLayer$Index)} {
               set Value(RowIDLayer$i) [expr $Value(RowIDLayer$i) - 1]
               #$Widget.range.variableGrid.layer${i}_rowID configure -text $Value(RowIDLayer$i)
               APViz::AssignVariable $Product $i
               APViz::Refresh $Product
               AdjustIDBubble $Widget.range.variableGrid.layer${i} $i           ; # Change row id in bubble
            }
         }

         #----- Enlever la derniere variable de Data(LayerIDs) vu qu'on a decale tous les fielIds
         set lastIndex [expr [llength ${::APViz::Data(LayerIDs)}] - 1]
         if {$lastIndex != $Value(RowIDLayer$Index)} {
            APViz::RemoveVariableFromVP ${::APViz::Data(LayerIDs)} $lastIndex           ; # Unassign ssi l'index a supprimer n'est pas la derniere rangee
         }
         set oldRowID $Value(RowIDLayer$Index)
         set Value(RowIDLayer$Index) -1                                                      ; # Index supprime, n'est plus affiche
         set ::APViz::Data(LayerIDs) [lreplace ${::APViz::Data(LayerIDs)} $lastIndex $lastIndex ]
         
         #----- Detruire les widgets
         if [winfo exists $Widget.range.variableGrid] {   
            foreach item $itemList {
               destroy $Widget.range.variableGrid.layer${Index}_$item
            }
         }
         
         #----- Enlever du dictionnaire AlphaDict
         RemoveFromAlphaDict $Index
                 
         APViz::FetchAllDates $Product
      }
      
      #-------------------------------------------------------------------------------
      # Nom      : <APViz::$product::Load>
      # Creation : Mai 2018 - C. Nguyen - CMC/CMOE -
      #
      # But      : Lire le fichier de configuration
      #
      # Parametres        :
      #         <Path>    : Path vers le fichier de config
      # Retour:
      #
      # Remarques :
      #
      #-------------------------------------------------------------------------------
      
      proc Load { Path } {
         variable Layers
         variable CalcLayers
         variable Range
         variable Param
         
         #----- Default Geography parameters
         set Map(Cameras)    {}
         set Map(Projection) Orthographic

         #----- Source product definition
         source $Path
         
         #----- Load Layout
         if { [file exists $Param(Layout)] } {
            #----- If the full path was specified
            set layout $Param(Layout)
         } else {
            #----- Otherwise look back up in the tree
            set path [lrange [file split $Path] 0 end-1]
            while { [llength $path] } {
               eval set layout \[file join $path\]/Layout/$Param(Layout) 
               if { [file exists $layout] } {
                  break
               } else {
                  set path [lreplace $path end end]
                  set layout ""
               }
            }
         }
         SPI::LayoutLoad ${::APViz::Data(Frame)} $layout
      }
      
      #-------------------------------------------------------------------------------
      # Nom      : <APViz::$product::Build>
      # Creation : Mai 2018 - C. Nguyen - CMC/CMOE -
      #
      # But      : Creer l'interface des ranges
      #
      # Parametres        :
      #         <Product> : Produit a afficher
      #         <Widget>  : Nom du widget parent dans lequel afficher l'interface
      # Retour:
      #
      # Remarques :
      #
      #-------------------------------------------------------------------------------
      proc Build { Product Widget } {
         global GDefs
         variable Layers
         variable Label
         variable Value
         variable Range
         variable Param
                  
         #----- Set Geography parameters
         if { [info exists Param(Projection)] } {
             eval projection configure ${::APViz::Data(Frame)} $Param(Projection)
         }

         if { [info exists Param(Viewport)] } {
            foreach vp [Page::Registered ${::APViz::Data(Frame)} Viewport] {
               eval ${::APViz::Data(Frame)}.page.canvas itemconfigure $vp $Param(Viewport)
            }
         }
         
         if { [info exists Param(Camera)] } {
            ::ProjCam::Select ${::APViz::Data(Frame)} ${::APViz::Data(Frame)} $Param(Camera)
         }
         
         $::APViz::Data(Tab).desc.message configure -text [lindex $Param(Description) $GDefs(Lang)]
         
         #----- Refleter les valeurs dans l'interface de configuration des parametres
         ::Viewport::ConfigGet ${::APViz::Data(Frame)} ${Viewport::Data(VP)}
         ::Viewport::ConfigPut ${::APViz::Data(Frame)} ${Viewport::Data(VP)}
      
         #----- Build product layer interface
         ::APViz::DeleteWidget $Widget.range    ; # Liberer le widget
         
         labelframe $Widget.range -text [lindex $Label(Layer) $GDefs(Lang)]
         
         frame $Widget.range.variableGrid       ; #Frame pour le grid
         
         #----- Column titles
         label $Widget.range.variableGrid.src    -text [lindex $Label(Source) $GDefs(Lang)]
         label $Widget.range.variableGrid.ip3    -text "IP3"
         label $Widget.range.variableGrid.vp     -text "VP"
         label $Widget.range.variableGrid.etiket -text "Etiket"
         
         checkbutton $Widget.range.variableGrid.mod -variable ::APViz::${Product}::Range(ModelLock) -onvalue True -offvalue False \
               -text [lindex $Label(Model) $GDefs(Lang)] -indicatoron 0 -relief sunken -bd 1 -overrelief raised -offrelief flat 
         checkbutton $Widget.range.variableGrid.runLock -variable ::APViz::${Product}::Range(RunLock) -onvalue True -offvalue False \
               -text [lindex $Label(Run) $GDefs(Lang)] -indicatoron 0 -relief sunken -bd 1 -overrelief raised -offrelief flat 
         $Widget.range.variableGrid.runLock select
               
         checkbutton $Widget.range.variableGrid.hrLock -variable ::APViz::${Product}::Range(HourLock) -onvalue True -offvalue False \
               -text [lindex $Label(Hour) $GDefs(Lang)] -indicatoron 0 -relief sunken -bd 1 -overrelief raised -offrelief flat 
         $Widget.range.variableGrid.hrLock select
               
         checkbutton $Widget.range.variableGrid.lvlLock -variable ::APViz::${Product}::Range(LevelLock) -onvalue True -offvalue False \
               -text [lindex $Label(Level) $GDefs(Lang)] -indicatoron 0 -relief sunken -bd 1 -overrelief raised -offrelief flat

         checkbutton $Widget.range.variableGrid.varLock -variable ::APViz::${Product}::Range(VarLock) -onvalue True -offvalue False \
               -text [lindex $Label(Variable) $GDefs(Lang)] -indicatoron 0 -relief sunken -bd 1 -overrelief raised -offrelief flat

               #----- Add help bubbles
         Bubble::Create $Widget.range.variableGrid.runLock ${APViz::Bubble(Lock)}
         Bubble::Create $Widget.range.variableGrid.hrLock ${APViz::Bubble(Lock)}
         Bubble::Create $Widget.range.variableGrid.lvlLock ${APViz::Bubble(Lock)}
         
         grid $Widget.range.variableGrid          -column 0 -row 1 -padx 0.2
         grid $Widget.range.variableGrid.mod      -column 1 -row 0 -padx 0.2 -sticky ew
         grid $Widget.range.variableGrid.runLock  -column 2 -row 0 -padx 0.2 -sticky ew
         grid $Widget.range.variableGrid.hrLock   -column 3 -row 0 -padx 0.2 -sticky ew
         grid $Widget.range.variableGrid.src      -column 4 -row 0 -padx 0.2
         grid $Widget.range.variableGrid.varLock  -column 5 -row 0 -padx 0.2 -sticky ew
         grid $Widget.range.variableGrid.lvlLock  -column 6 -row 0 -padx 0.2 -sticky ew
         grid $Widget.range.variableGrid.ip3      -column 7 -row 0 -padx 0.2
         grid $Widget.range.variableGrid.vp       -column 8 -row 0 -padx 0.2
         grid $Widget.range.variableGrid.etiket   -column 9 -row 0 -padx 0.2
         
         #----- Creation des ranges de variables
         CreateVariableRanges
         
         #----- Creation des couches         
         CreateLayers $Product $Layers $Widget
         
         pack $Widget.range -side top -fill x -anchor nw

         set dateList [APViz::FetchAllDates $Product]
         set APViz::Data(Date) [lindex $dateList [expr [llength $dateList] - 1]]
         
         ::APViz::DeleteWidget $Widget.add      ; # Liberer le widget
         
         menubutton $Widget.add -image PLUS -text [lindex $Label(AddLayer) $GDefs(Lang)] -compound left -bd 1 -menu $Widget.add.menu
         
         menu $Widget.add.menu
         set no 0
         foreach layer $Layers {
            #----- Layer description
            regsub \(True:|False:\) $layer "" desc
            $Widget.add.menu add command -label "Type$no: $desc" -command "APViz::${Product}::CreateLayers $Product $layer $Widget True $no"
            lappend ::APViz::Data(Layers) $layer                ; #save layer configs
            incr no
         }
         $Widget.add.menu add command -label [lindex ${APViz::Lbl(CreateNewLayer)} $GDefs(Lang)] -command "APViz::AddLayerWindow"
         
         $Widget.add.menu add separator
         $Widget.add.menu add command -label "Ajouter couche de calcul" -command "APViz::${Product}::AddCalcLayer $Product $Widget"
         
         pack $Widget.add -side top -padx 2 -pady 2 -anchor nw
         
         ::APViz::DeleteWidget $Widget.calc     ; # Liberer le widget
         
         labelframe $Widget.calc -text [lindex $Label(Calcul) $GDefs(Lang)]
         pack $Widget.calc -side bottom -fill both -expand True
         
         #----- Create formula lists
         APViz::CreateFormulaLists
      }
      
      #-------------------------------------------------------------------------------
      # Nom      : <APViz::$product::SetEtiketBubble>
      # Creation : Aout 2018 - C. Nguyen - CMC/CMOE -
      #
      # But      : Ajouter une bulle de description de l'etiket
      # Parametres       :
      #         <Widget> : Widget du entry etiket       
      #         <Index>  : L'index de la couche
      #
      # Retour:
      #
      # Remarques :
      #
      #-------------------------------------------------------------------------------
      
      proc SetEtiketBubble { Widget Index } {
         variable Value
         if {[winfo exists $Widget]} {
            #----- Set message
            Bubble::Create $Widget $Value(Etiket,$Index)
         }
      }

      #-------------------------------------------------------------------------------
      # Nom      : <APViz::$product::RemoveFromAlphaDict>
      # Creation : Octobre 2018 - C. Nguyen - CMC/CMOE -
      #
      # But      : Enlever la lettre du dictionnaire (AlphaDict) utilise pour remplacer
      #            la valeur des champs dans les formules
      # Parametres       :
      #         <Lettre> : Lettre a enlever du dictionnaire
      #
      # Retour:
      #
      # Remarques :
      #
      #-------------------------------------------------------------------------------
      proc RemoveFromAlphaDict { Index {IsLayer true} } {
         #---- Recuper la lettre associee au field 
         set type [string cat [expr $IsLayer?"L":"C"] $Index]
         
         dict for {alpha layerNo} $::APViz::Data(AlphaDict) {
            if {$layerNo eq $type} {
                #---- Enlever la lettre du dictionnaire
                dict unset ::APViz::Data(AlphaDict) $alpha
            }
         }
      }
   }
   
   ${product}::Load $Path

   set Data(ConfigPath) $Path        
   set Data(CurrentProduct) $product
   set Data(ParentWidget) $Widget  
   set Data(AutoUpdateEventID) [after [expr {1000*60*10}] APViz::UpdateAvailableDates $product] ; # Update a chaque 10min:1000*60*10
   
   return $product
}

proc APViz::Execute { Product Widget } {   
   variable Data
   
   Animator::EmptyPlayList

   ${Product}::Build $Product $Widget
   APViz::InitializeVars
   ${Product}::CreateCalcLayers $Product $Widget
   APViz::Refresh $Product

   set Param(Layout)        ""
   eval set proc \[info procs ::APViz::${Product}::Post\]
   if { $proc!="" } {
      eval $proc
   }
}

proc APViz::Refresh { Product } {
   variable Data
   variable ${Product}::Value

   for { set c 0 } { $c < $Value(NbCalcLayers) } { incr c } {
      APViz::CalculateExpression $Product $c
   }
   
   Viewport::UpdateData $Data(Frame)
   Page::UpdateCommand $Data(Frame)
   Page::UpdateItems $Data(Frame)
   
   return $Data(Secs)
}

proc APViz::GetPlayList { Product } {   
   variable Data
      
   variable ${Product}::Range
   variable ${Product}::Value

   if { ![llength $Animator::Play(Frames)] } {
      foreach hour $Range(Hours) {
         set anim "Viewport::UnAssign $Data(Frame) $Viewport::Data(VP) {} -1; "
         for { set idx 0 } { $idx < $Value(NbLayers) } { incr idx } {
            if { $Value(RowIDLayer$idx) >= 0 } {
               append anim "set APViz::${Product}::Value(Hours,$idx) $hour; APViz::AssignVariable $Product $idx;"
            } 
         }
         lappend Animator::Play(Frames) "$anim; APViz::Refresh $Product"
      }
      set Animator::Play(VPs)    [lsort -unique $Animator::Play(VPs)]
      set Animator::Play(Length) [expr [llength $Animator::Play(Frames)]-1]
      set Animator::Play(Cycle)  1
      set Animator::Play(Type)   DATE
      Animator::Limits
   }
}

#-------------------------------------------------------------------------------
# Nom      : <APViz::AreFieldsFilled>
# Creation : Mai 2018 - C. Nguyen - CMC/CMOE -
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

proc APViz::AssignVariable { Product Index { Refresh True } } {
   global GDefs env
   variable Data
   variable DataSrc
   variable Etiket
   variable Lbl
   variable ${Product}::Param
   variable ${Product}::Value

   #----- Get layer values
   set model	$Value(Models,$Index)
   set var	$Value(Vars,$Index)
   set lev	$Value(Levels,$Index)
   set run	$Value(Runs,$Index)
   set hour	$Value(Hours,$Index)
   set src	$Value(Sources,$Index)
   set vp       $Value(Viewports,$Index)
   set ip3      $Value(IP3,$Index)
   set etiket   $Value(Etiket,$Index)
   
   puts "Assigning variable for $model $var"
   
   #----- check if we need to increment the date on a run increment
   set date $Data(Date)
   if { [info exists Value(DeltaRuns,$Index)] } {
      if { $run<0 || $run>=24 } {
         set date [clock format [expr [clock scan $Data(Date) -format "%Y%m%d" -timezone :UTC]+$Value(DeltaRuns,$Index)] -format "%Y%m%d" -timezone :UTC]
         set run [format %02i [expr $run-$Value(DeltaRuns,$Index)]]
      }
   }
   
   if { ![APViz::ValidateDate $date] } {
      return
   }

   #----- Setting optional IP3
   if { ($ip3 eq "-") || ($ip3 eq "") } {
      set ip3 -1
   }
   
   #----- Assign etiket if not set
#    if { ($etiket eq "-") || ($etiket eq "") } {
#       #----- Verify if defined
#       if { [info exists Etiket(${model}${src})] } {
#          set etiket $Etiket(${model}${src})
#       } elseif { [info exists Etiket(${model})] } {
#          set etiket $Etiket($model)
#       }
#       
#       #----- Display in widget
#       if { [winfo exists $Value(EtiketWidget,$Index)] } {
#          $Value(EtiketWidget,$Index) delete 0 end
#          $Value(EtiketWidget,$Index) insert 0 $etiket
#          
#          #----- Create bubble
#          Bubble::Create $Value(EtiketWidget,$Index) $etiket
#       }
#    }
   
   #----- Verifier si tous les champs sont remplis
   if { [APViz::AreFieldsFilled $model $var $lev $run $hour $src $date] } {
      set vpID [APViz::GetVPId $vp]
      
      if { $src eq "BURP" } {
         set timestamp ${date}${run}_
         set filepath $DataSrc(OBS,$model)/$timestamp
         
         if { !$Animator::Play(Stop) } {
            set obsID obs$Value(RowIDLayer$Index)_$timestamp
         } else {
            set obsID obs$Value(RowIDLayer$Index)
         }
         
         #----- Liberer l'observation
         if { [metobs is $obsID] } {
            if { !$Animator::Play(Stop) } {
               set Data(LayerIDs) [lreplace $Data(LayerIDs) $Value(RowIDLayer$Index) $Value(RowIDLayer$Index) $obsID]
               if { $Data(Secs) } { metobs define $obsID -VALID $Data(Secs) 0 }
               Viewport::Assign $Data(Frame) $vpID $obsID -1
               return
            } else {
               APViz::RemoveVariableFromVP $Data(LayerIDs) $Value(RowIDLayer$Index)
            }
         }
         puts "BURP FILE for $var: $filepath"
         
         #----- In case id alreayd exists
         if { [metobs is $obsID] } {
            metobs free $obsID
         }       
         metobs create $obsID $filepath
         
         if { $Data(Secs) } { metobs define $obsID -VALID $Data(Secs) 0 }

         dataspec create $obsID
         
         #----- Apply variable configs from config file 
         if { [catch {eval dataspec configure $obsID $Param(${var}:$Value(Letter,$Index))}] } {
            if { [catch {eval dataspec configure $obsID $Param($var) }] } {
               #----- Configurations par defaut TODO: Choose a colormap that exists
               dataspec configure $obsID -desc "$model (${timestamp}_)" -size 10 -icon CIRCLE -color black -colormap $Data(DefaultColormap) \
                  -mapall True -rendertexture 1 -rendercontour 1 -rendervalue 1 -font XFont12 -intervals { 1 5 10 15 20 30 40 50 75 100 125 150 200 } -active $Value(Toggle,$Index)
            }
         }
         
         dataspec configure $obsID -active $Value(Toggle,$Index)

         set lst [list [list 0 0 $var { }]]
         metmodel define [metobs define $obsID -MODEL] -items $lst -spacing 10
         metmodel configure [metobs define $obsID -MODEL] $var -dataspec $obsID
         set Data(LayerIDs) [lreplace $Data(LayerIDs) $Value(RowIDLayer$Index) $Value(RowIDLayer$Index) $obsID]
         
         Viewport::Assign $Data(Frame) $vpID $obsID 1    
      } else {
                  
         #----- Pas un fichier BURP
         set Data(timestamp) ${date}${run}_$hour
         set filepath $DataSrc($model,$src)/$Data(timestamp)      ; # Format: AAAAMMDDRR_HHH
         set fileID FILE_${model}_${src}_$Data(timestamp)
         
         if { !$Animator::Play(Stop) } {
            set fieldID fld$Value(RowIDLayer$Index)_$Data(timestamp)
         } else {
            set fieldID fld$Value(RowIDLayer$Index)
         }
         
         if { [fstdfield is $fieldID] } {
            if { !$Animator::Play(Stop) } {
               set Data(Secs) [fstdstamp toseconds [fstdfield define $fieldID -DATEV]]
               set Data(LayerIDs) [lreplace $Data(LayerIDs) $Value(RowIDLayer$Index) $Value(RowIDLayer$Index) $fieldID]
               Viewport::Assign $Data(Frame) $vpID $fieldID -1
               return
            } else {
               APViz::RemoveVariableFromVP $Data(LayerIDs) $Value(RowIDLayer$Index)
            }
         }
         puts "STANDARD FILE - $fileID    $filepath"
         
         #----- Verifier la validite du fichier standard
         if { [fstdfile is $filepath] } {
            if { [catch { fstdfile open $fileID read $filepath }] == 0 } {
               lappend Data(OpenedFiles) $fileID
               puts "STANDARD FILE - Opening $fileID	$filepath"
            }
            
            if { $lev>32768 } {
               set lvl $lev
            } else {      
               set lvl [subst {$lev [APViz::GetLevelType $src]}]
            }

            switch $var {
               "DZ"     { APViz::AssignDZ $Product $Index $src $model $var $lev $fileID $fieldID $ip3 $etiket}
               
               default  {  
                           if {[catch {fstdfield read $fieldID $fileID -1 $etiket $lvl -1 $ip3 "" $var }]} {                                       
                              APViz::LayerToggle ${Index} Layer False
                              set Data(Msg) "[lindex $Lbl(InvalidField) $GDefs(Lang)]: $Value(RowIDLayer$Index)"
                              set Data(LayerIDs) [lreplace $Data(LayerIDs) $Value(RowIDLayer$Index) $Value(RowIDLayer$Index) $fieldID]
                              return
                           } else {
                              APViz::LayerToggle ${Index} Layer True
                              set Data(LayerIDs) [lreplace $Data(LayerIDs) $Value(RowIDLayer$Index) $Value(RowIDLayer$Index) $fieldID]
                           }
                        }
            }
            
            #----- In case DZ assignation failed
            if { ![fstdfield is $fieldID] } {
               return
            }

            #----- Apply variable configs from config file 
            if { [catch {eval fstdfield configure $fieldID $Param(${var}:$Value(Letter,$Index))}] } {
               if { [catch {eval fstdfield configure $fieldID $Param($var)}] } {
                  #----- Valeur par defaut
                  fstdfield configure $fieldID -colormap $Data(DefaultColormap) -color black -font XFont12 -width 1 -rendertexture 1 -mapall True
               }
            }

            #----- Si la colormap n'existe pas deja, creer la bonne colormap
            set colormapName $var$Index
            if { [lsearch -exact $Data(Colormaps) $colormapName] < 0 } {
               set configColormap [fstdfield configure $fieldID -colormap]
               if {$configColormap eq ""} {
                  fstdfield configure $fieldID -colormap $Data(DefaultColormap)
                  set configColormap $Data(DefaultColormap)
               }
               
               colormap create $colormapName
               if {[catch {eval colormap copy $colormapName $configColormap}]} {
                  puts "Colormap $configColormap does not exist"
               }

               #----- Ajouter dans la liste des colormaps pour retrouver l'original
               lappend Data(ColormapPairs) [list $colormapName $configColormap]
               lappend Data(Colormaps) $colormapName
            }
            
            fstdfield configure $fieldID -colormap $colormapName
            fstdfield configure $fieldID -active $Value(Toggle,$Index)
            
            set Data(Secs) [fstdstamp toseconds [fstdfield define $fieldID -DATEV]]
            if { !$Animator::Play(Stop) } {
               lappend Animator::Play(Frames) $Data(Secs)
               lappend Animator::Play($vpID$Data(Secs)) $fieldID
               lappend Animator::Play(VPs) $vpID
            } else {
              Viewport::Assign $Data(Frame) $vpID $fieldID 1
            }
         } else {
            APViz::LayerToggle ${Index} Layer False
            set Data(LayerIDs) [lreplace $Data(LayerIDs) $Value(RowIDLayer$Index) $Value(RowIDLayer$Index) $fieldID]
            
            #----- Concatener le path du fichier au message d'erreur
            set Data(Msg) "[lindex $Lbl(InvalidFile) $GDefs(Lang)] $filepath"
#            ::Dialog::Error . $Lbl(InvalidFile) $filepath
         }
      }
   } else {
      puts "Missing values"
   }
}

proc APViz::LayerToggle { Index Type Active } {
   global GDefs
   variable Data
   
   if { $Type=="Layer" } {
      set widget  $Data(Tab).range.variableGrid.layer${Index}_toggle
   } else {
      set widget  $Data(Tab).calc.$Index.check 
   }
   
   if { $Active } {
      $widget configure -state normal -background $GDefs(ColorFrame) -selectcolor $GDefs(ColorHighLight)
   } else {
      $widget configure -state disabled -selectcolor red -background red
   }
}

#-------------------------------------------------------------------------------
# Nom      : <APViz::AssignDZ>
# Creation : Juillet 2018 - C. Nguyen - CMC/CMOE -
#
# But      :    Verifier si tous les champs ont ete remplis
#
# Parametres      :
#       <Source>  : Source de donnees
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

proc APViz::AssignDZ { Product Index Source Model Var Lev FileID FieldID Ip3 Etiket } {
   global GDefs
   variable Data
   variable ${Product}::Value

   #----- Get GZ levels: lev1 and lev2
   lassign [split $Lev -] lev1 lev2
   
   #----- Create GZ fields : GZ1 and GZ2 ---- will not be added to viewport
   set fieldIDGZ1 DZ$Value(RowIDLayer$Index)_GZ1
   set fieldIDGZ2 DZ$Value(RowIDLayer$Index)_GZ2
   
   #----- Free those fields if already used
   if {[fstdfield is $fieldIDGZ1]} {
      fstdfield free $fieldIDGZ1
   }
   
   if {[fstdfield is $fieldIDGZ2]} {
      fstdfield free $fieldIDGZ2
   }
   
   #----- GZ1 field
   set lvl [subst {$lev1 [APViz::GetLevelType $Source]}]
   if {[catch {fstdfield read $fieldIDGZ1 $FileID -1 $Etiket $lvl -1 $Ip3 "" GZ}]} {
      #----- Etiket might be wrong
      puts "Etiket was wrong for DZ, changing to blank"
      set Etiket ""
      if {[catch {fstdfield read $fieldIDGZ1 $FileID -1 $Etiket $lvl -1 $Ip3 "" GZ}]} {
         puts "fieldIDGZ1: $fieldIDGZ1 failed for level $lev1"
         ::Dialog::Error . $Lbl(InvalidField)
         set Data(LayerIDs) [lreplace $Data(LayerIDs) $Value(RowIDLayer$Index) $Value(RowIDLayer$Index) FLD$Value(RowIDLayer$Index)]
         return
      }
   } 
   
   #----- GZ2 field
   set lvl [subst {$lev2 [APViz::GetLevelType $Source]}]
   if {[catch {fstdfield read $fieldIDGZ2 $FileID -1 $Etiket $lvl -1 $Ip3 "" GZ}]} {
      puts "fieldIDGZ2: $fieldIDGZ2 failed for level $lev2"
      ::Dialog::Error . $Lbl(InvalidField)
      set Data(LayerIDs) [lreplace $Data(LayerIDs) $Value(RowIDLayer$Index) $Value(RowIDLayer$Index) FLD$Value(RowIDLayer$Index)]
      return
   } else {
      lappend Data(DZ_GZpairs) [list $fieldIDGZ1 $fieldIDGZ2]
   }
   
   #----- Calcul: GZ1 - GZ2
   vexpr $FieldID "$fieldIDGZ1-$fieldIDGZ2"
   
   if { [fstdfield is $FieldID] } {      
      APViz::LayerToggle ${Index} Layer True
      #----- Add ID to Data(LayersID)
      set Data(LayerIDs) [lreplace $Data(LayerIDs) $Value(RowIDLayer$Index) $Value(RowIDLayer$Index) $FieldID]
   } else {
      APViz::LayerToggle ${Index} Layer False
      set Data(LayerIDs) [lreplace $Data(LayerIDs) $Value(RowIDLayer$Index) $Value(RowIDLayer$Index) FLD$Value(RowIDLayer$Index)]
      puts "DZ FAILED"
      set Data(Msg) "[lindex $Lbl(InvalidField) $GDefs(Lang)]: $Value(RowIDLayer$Index)"
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

proc APViz::CalculateExpression { Product Index } {
   variable Data
   variable Lbl
   variable ${Product}::Value
   variable ${Product}::Param

   APViz::LayerToggle ${Index} Calc True
   set expression $Value(Formula,$Index)

   if { $expression ne "" } {
    
      #----- Substitute expressions
      set expression [TranslateExpression $Product $expression]
      set vpID       [GetVPId $Value(CalcVP,$Index)]

      if { !$Animator::Play(Stop) } {
         set fieldID CALC$Value(RowIDLayer$Index)_$Data(timestamp)
      } else {
         set fieldID CALC$Value(RowIDLayer$Index)
      }
      
      if { [fstdfield is $fieldID] } {
         if { !$Animator::Play(Stop) } {
            Viewport::Assign $Data(Frame) $vpID $fieldID -1
            return
         } else {
            APViz::RemoveVariableFromVP $Data(CalcIDs) $Value(RowIDCalc$Index)		; # Enlever la variable courante du VP pour cette couche
         }
      }
      
      if { [regexp const $expression] } {
         ::Dialog::Error . $Lbl(MissingConst)
         return
      }
          
      #----- Creer un id unique (TODO: check to use letter name instead)
      set formulaName ""
      set formulaID [lsearch -exact $Data(Formulas) $Value(Formula,$Index)]
      if {$formulaID >= 0} {
         set formulaName [lindex $Data(FormulaNames) $formulaID]
      }
      
      
      #----- Calculer l'expression
      if { [catch { vexpr $fieldID $expression } err] } {
         APViz::LayerToggle ${Index} Calc False
      }
      
      if { [fstdfield is $fieldID] } {
         set isActivated $Value(CalcToggle,$Index)
         
         set var [fstdfield define $fieldID -NOMVAR]
         
         #----- Apply variable configs from config file 
         #----- Verify if configs for calc was specified first
         if { [catch {eval fstdfield configure $fieldID $Param(CALC)} ]} {
            if { [catch {eval fstdfield configure $fieldID $Param(${var}:$Value(CLetter,$Index))} ]} {
               if { [catch {eval fstdfield configure $fieldID $Param($var)}] } {
                  #----- Valeur par defaut
                  fstdfield configure $fieldID -font XFont12 -width 1 -rendertexture 1 -mapall True -colormap $Data(DefaultColormap) -color black
               }
            }
         }
         
         fstdfield configure $fieldID -active $isActivated -set 1
         
         Viewport::Assign $Data(Frame) $vpID $fieldID
                    
         set Data(CalcIDs) [lreplace $Data(CalcIDs) $Value(RowIDCalc$Index) $Value(RowIDCalc$Index) $fieldID]
      }
   }
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

proc APViz::Check { Product Index {IsCalc False} } {
   variable ${Product}::Value
   variable Data

   if {$IsCalc} {
      set ID [lindex $Data(CalcIDs) $Value(RowIDCalc$Index)] 
      set isActivated $Value(CalcToggle,$Index)
   } else {
      set ID [lindex $Data(LayerIDs) $Value(RowIDLayer$Index)] 
      set isActivated $Value(Toggle,$Index)
   }

   if {[fstdfield is $ID]} {
      fstdfield configure $ID -active $isActivated
   } elseif {[metobs is $ID]} {
      set startIndex [expr [string first _ $ID] + 1]
      set var [string range $ID $startIndex [string length $ID]]
      metmodel configure [metobs define $ID -MODEL] $var -active $isActivated 
   }
   
   APViz::Refresh $Product
}


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

#----------------------------------------------------------------------------
# Nom      : <APViz::ConstructTreeBranches>
# Creation : Aout 2018 - C. Nguyen - CMC/CMOE
#
# But      :    Construire les embranchements de l'arborescence de fichiers
#
# Parametres    :
#       <Tree>       : Identifiant de l'arbre
#       <ParentNode> : Le noeud parent
#       <Path>       : Le path vers le fichier
#       <Level>      : Niveau de l'arborescence (Pour determiner si open est a True ou False)
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc APViz::ConstructTreeBranches { Tree ParentNode Path Level } {
   variable Data

   #----- Get file and folders list
   set fileList [glob -nocomplain -tails -path $Path/ *]
   
   foreach file $fileList {
      if {($file ni { "Colormap" "Layout" "APViz_Data.tcl" })} {
         #----- Create node
         $Tree insert $ParentNode end $file

         #----- Set node attributes
         $Tree set $file name $file
         
         if {$Level < 3} {
            $Tree set $file open True
         } else {
            $Tree set $file open False
         }
         
         if {[file isdirectory [set newPath ${Path}/$file]]} {
            #----- Appel recursif
            APViz::ConstructTreeBranches $Tree $file $newPath [expr $Level + 1]
            $Tree set $file path ""
         } else {
            $Tree set $file path ${Path}/$file
             lappend Data(ProductPaths) $Path/$file
         }
      }
   }
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
   variable Data
   variable ::MapBox::Param
   
   if {[info exists DataSrc(Colormaps)]} {
      foreach path $DataSrc(Colormaps) {
         set colormapLst [glob -nocomplain -tails -path $path *.rgba]

         #----- Create colormaps
         foreach colormap $colormapLst {
            regsub .rgba $colormap "" colormapName
            if {![colormap is $colormapName]} {
               colormap create $colormapName -file ${path}/$colormap
               if {$Data(DefaultColormap) eq ""} {
                  set Data(DefaultColormap) $colormapName
               }
            }
         }
         
         #----- Add folder to MapBox paths
         regsub /Colormap/ $path "" colormapPath
         set Param(Paths) [concat $colormapPath $Param(Paths)]
      }
   }
   
   #----- Create the ones in .spi 
   set spiPath $env(HOME)/.spi/Colormap/
   if {[file isdirectory $spiPath]} {
      set spiColormapLst [glob -nocomplain -tails -path $spiPath *.rgba]
      foreach colormap $spiColormapLst {
         regsub .rgba $colormap "" colormapName
         if {![colormap is $colormapName]} {
            colormap create $colormapName -file ${spiPath}/$colormap
            if {$Data(DefaultColormap) eq ""} {
               set Data(DefaultColormap) $colormapName
            }
         }
      }
   }
}

#----------------------------------------------------------------------------
# Nom      : <APViz::CreateFileTree>
# Creation : Mai 2018 - C. Nguyen - CMC/CMOE
#
# But      :    Construire l'arborescence de fichiers de config selon le 
#               dossier selectionne
#
# Parametres    :
#       <Path>  : Path vers le fichier de config
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc APViz::CreateFileTree { Path } {
   variable Data
   variable Param 
   variable DataSrc
   
   #----- Verify if tree already exist
   if { [llength [FILETREE children root]] } {
      #----- Reinitialize tree
      FILETREE destroy
      struct::tree FILETREE
   }
   
   #----- Source APViz_Data.tcl file
   set oldDefaultColormap $Data(DefaultColormap)
   if {[catch { source ${Path}/APViz_Data.tcl }]} {
      lappend msg "Fichier APViz_Data.tcl manquant de $Path"
      lappend msg "File APViz_Data.tcl containing paths missing from $Path"
      ::Dialog::Error . $msg
   } else {
      if {$Data(DefaultColormap) eq ""} {
         #----- Find an existing colormap
         set Data(DefaultColormap) $oldDefaultColormap
      }
   }

   #----- Construct tree with all files and directories from configs path
   APViz::ConstructTreeBranches FILETREE root $Path 0

   CVTree::Create $Data(Tab).filetree.canvas APViz::FILETREE \
      IdCmd APViz::GetTreeId \
      SelectCmd APViz::SelectFiletreeBranch
}

#----------------------------------------------------------------------------
# Nom      : <APViz::CreateFormulaLists>
# Creation : Mai 2018 - C. Nguyen - CMC/CMOE
#
# But      :    Construire les listes de formules Data(FormulaNames) et Data(Formulas)
#               selon la liste de paires {Formula_name Formula}
#
# Parametres    :
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc APViz::CreateFormulaLists { } {
   variable Data

   #----- Construct formula lists
   set Data(FormulaNames) {}
   set Data(Formulas) {}

   foreach formulaPair $Data(FormulaDefinitions) {
      if {[lsearch -exact $Data(FormulaNames) [lindex $formulaPair 0]] < 0} {
         lappend Data(FormulaNames) [lindex $formulaPair 0]
         lappend Data(Formulas) [lindex $formulaPair 1]
      }
   }
}

#----------------------------------------------------------------------------
# Nom      : <APViz::DateBinding>
# Creation : Juillet 2018 - C. Nguyen - CMC/CMOE
#
# But      : Valider la date et initialiser les variables si valide
#
# Parametres :
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
# But      :    Supprime le widget et ses enfants
#
# Parametres    :
#       <Widget>        : Widget a supprimer
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
# Nom      : <APViz::FetchAllDates>
# Creation : Juillet 2018 - C. Nguyen - CMC/CMOE
#
# But      :    Construire la liste de dates disponibles pour afficher dans l'interface
#               en considerant toutes les sources
#
# Parametres    :
#       <Product> : Le nom du produit selectionne (aussi le namespace) 
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc APViz::FetchAllDates { Product } {
   variable Data
   
   if { $Product ne "" } {
      variable ${Product}::Value
      
      set selectedDate $Data(Date)

      set sourceLst {}
      set dateLst   {}
      for {set i 0} {$i < $Value(NbLayers)} {incr i} {
         if {[set rowID $Value(RowIDLayer$i)] >= 0} {
            set model $Value(Models,$i)
            set src $Value(Sources,$i)
            
            #----- Verify if the model and src have already been treated
            if {[lsearch -exact $sourceLst $model$src] < 0} {
               lappend dateLst [APViz::FetchDates $Product $model $src]
               lappend sourceLst $model$src
            }
         }
      }

      if { [set nbDateLst [llength $dateLst]] > 1 } {
         
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
      
      #----- Display dates in Available Dates combo box
      if {($Data(DateCBWidget) ne "") && [winfo exists $Data(DateCBWidget)]} {
         ComboBox::DelAll $Data(DateCBWidget)
         ComboBox::AddList $Data(DateCBWidget) $finalDateLst
      }

      if { ($Data(Date) eq "") && ([lsearch -exact $Data(Dates) $selectedDate] >= 0)} {
         if { $selectedDate ne "" } {
            set Data(Date) $selectedDate
         } else {
            set Data(Date) [lindex $Data(Dates) end]
         }
      }
      
      set Data(Dates) $finalDateLst
   }
}

#----------------------------------------------------------------------------
# Nom      : <APViz::FetchDates>
# Creation : Juillet 2018 - C. Nguyen - CMC/CMOE
#
# But      :    Construire la liste de dates disponibles pour afficher dans l'interface
#
# Parametres    :
#       <Product> : Le nom du produit selectionne (aussi le namespace) 
#       <Path>  : Path du parent du dossier
#       <Name>  : Nom du dossier
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

   if { $Product ne "" } {
      if { $Src eq "BURP" } {
         set path $DataSrc(OBS,$Model)/
      } else {
         set path $DataSrc(${Model},${Src})/
      }
      if { ![llength [set fileList [glob -nocomplain -tails -path $path *_000]]] } {
         set fileList [glob -nocomplain -tails -path $path ??????????_*]
         set dates    [lsort [lmap a $fileList {string range $a 0 7}]] 
      } else {
         set dates    [lsort [lmap a $fileList {string range $a 0 7}]]
      }
   }
   return $dates
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
      
      #----- Verify if colormaps have changed
      set colormapLst [APViz::ManageColormaps $product $Path]
      
      #----- Get data from original config file
      set origFileID [open $Data(ConfigPath) r]
      set origFileData [read $origFileID]
      close $origFileID
      
      set origData [split $origFileData "\n"]
      
      #----- Get section indexes
      set geoStartIndex   [lsearch -glob $origData "*\#*Geography*"]
      set styleStartIndex [lsearch -glob $origData "*\#*Style*"]
      set rangeStartIndex [lsearch -glob $origData "*\#*Ranges*"]
      
      set filename [file tail $Path]
      set fileID [open $Path w]
      
      #----- Copy from original til Geo configs
      APViz::WriteConfigSection $fileID [lrange $origData 0 $geoStartIndex]

      #----- Write Geo params
      APViz::WriteCameraConfigs $fileID
      APViz::WriteProjectionConfigs $fileID
      APViz::WriteViewportConfigs $product $fileID

      #----- Variable Style Configs
      puts -nonewline $fileID "\n"
      APViz::WriteVariableConfigs $product $fileID [lrange $origData $styleStartIndex [expr $rangeStartIndex - 1]]  $colormapLst
      
      #----- Ranges :
      puts -nonewline $fileID "\n"
      APViz::WriteRanges $product $fileID

      #----- Write Layers
      puts -nonewline $fileID "\n"
      APViz::WriteLayers $product $fileID
            
      #----- Write CalcLayers
      puts -nonewline $fileID "\n"
      APViz::WriteCalcLayers $product $fileID

      close $fileID
      
      #----- Update file tree
      APViz::UpdateProductInterface $filename $Path
   }
}

#-------------------------------------------------------------------------------
# Nom      : <APViz::GetAllFieldsWithOp>
# Creation : Juin 2018 - C. Nguyen - CMC/CMOE -
#
# But      :    Calculer une expression de fields et l'afficher sur le VP
#
# Parametres          :
#       <Product>     : Le nom du produit selectionne (aussi le namespace) 
#       <Nb>          : Nombre de champs trouve
#       <Operator>    : L'operateur de calcul (+-*/)
#       <OnlyChecked> : Bool indiquand si on n'inclut que les variables selectionnees 
#       
# Retour:
#
# Remarques :
#
#-------------------------------------------------------------------------------

proc APViz::GetAllFieldsWithOp { Product Nb Operator { OnlyChecked False } } {
   variable Data
   variable ${Product}::Value
   upvar $Nb nb

   set fieldString ""
   set nb 0

   for { set i 0 } { $i < $Value(NbLayers) } { incr i } {
      if { !$OnlyChecked || (($Value(RowIDLayer$i)>=0) && $Value(Toggle,$i)) } {
         set ID [lindex $Data(LayerIDs) $Value(RowIDLayer$i)]
         if { [fstdfield is $ID] } {
            incr nb
            append fieldString $Value(Letter,$i)$Operator
         }
      }
   }
   return [string range $fieldString 0 [expr [string length $fieldString] - 2]]
}

#----------------------------------------------------------------------------
# Nom      : <APViz::GetFormulaName>
# Creation : Aout 2018 - C. Nguyen - CMC/CMOE
#
# But      :    Retourner le nom de la formule
#
# Parametres    :
#       <Formula> : Formule non modifiee
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc APViz::GetFormulaName { Formula } {
   variable Data
   
   set formulaName ""
   set index [lsearch -exact $Data(Formulas) $Formula]
   if { $index >= 0 } {
      set formulaName [lindex $Data(FormulaNames) $index]
   }
   
   return $formulaName
}

#-------------------------------------------------------------------------------
# Nom      : <APViz::GetImageDirPath>
# Creation : Aout 2018 - C. Nguyen - CMC/CMOE
#
# But      : Retourner le chemin vers le dossier d'image
#
# Parametres :
#  <Tree>    : Arbre
#  <Branch>  : Branche
#
# Retour    :
#  <Id>     : Identification
#
# Remarque :
#
#-------------------------------------------------------------------------------

proc APViz::GetImageDirPath { } {
   variable Data
   variable Lbl

   set path ""
   
   #----- Verify if exists
   if {![info exists Data(SaveImageDir)]} {
      #---- TODO: If missing, ask if in EC or SCIENCE : set default paths
      ::Dialog::Error . $Lbl(MissingImgPath)
   }
   
   #----- Get path to image directory 
   if { [catch {set path [file readlink $Data(SaveImageDir)]}] } {
      #----- If not a symbolic link, verify if is folder
      if {[file isdirectory $Data(SaveImageDir)]} {
         set path $Data(SaveImageDir)
      } else {
         set message {}
         foreach msg $Lbl(InvalidImgPath) {
            lappend message [concat $msg $Data(SaveImageDir)]
         }
         ::Dialog::Error . $message
      }
   }
   
   return $path
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
      "PRES"	{ return PRESSURE }
      "ETA"	{ return ETA }
      "HYB"	{ return HYBRID }
      "DIAG"	{ return PRESSURE }
   }

   puts "Cannot determine level with source $Source."
   return ""	; # Format non reconnu
}

#-------------------------------------------------------------------------------
# Nom      : <APViz::GetTreeId>
# Creation : Aout 2018 - C. Nguyen - CMC/CMOE
#
# But      : Creer l'identification de la branche
#
# Parametres :
#  <Tree>    : Arbre
#  <Branch>  : Branche
#
# Retour    :
#  <Id>     : Identification
#
# Remarque :
#
#-------------------------------------------------------------------------------

proc APViz::GetTreeId { Tree Branch Leaf } {

   upvar $Leaf leaf
   set leaf [$Tree isleaf $Branch]
   
   set id [$Tree get $Branch name]
   return $id
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

   set configLst {}

   for { set i 0 } { $i < $Value(NbLayers) } { incr i } {
      if { [set rowID $Value(RowIDLayer$i)] >= 0 } {
         set var $Value(Vars,$i)
         set level $Value(Levels,$i)
         set ID [lindex $Data(LayerIDs) $rowID]
         set alpha $Value(Letter,$i)

         
         #---- Set command depending on type
         if { [fstdfield is $ID] } {
            set isFstdField True
         } elseif { [metobs is $ID] } {
            set isFstdField False
            set model [metobs define $ID -MODEL]
         } else {
            puts "Sortir de la boucle"
            break
         }

         set params "set Param(${var}:$alpha) \{"
         set paramLst [list font efont min max width dash rendercontour rendertexture rendervalue rendergrid renderlabel renderparticle rendervector mapall intervalmode interlabels interspecs extrema value colormap color showmap]

         foreach param $paramLst {
            if { $param eq "colormap" } {
               set value [lindex $ColorMaps $Value(RowIDLayer$i)]
            } else {
               if { $isFstdField } {
                  set value [fstdfield configure $ID -$param]
               } else {
                  set value [metmodel configure $model $var -$param]
               }              
            }
            
            #----- Can only configure intervalmode OR intervals
            if { [expr { $param eq "intervalmode" }] && [expr { $value eq "NONE 0.0" }] } {
               if {$isFstdField} {
                  set params [concat $params "-intervals \{[fstdfield configure $ID -intervals]\}"]
               } else {
                  set params [concat $params "-intervals \{[metmodel configure $model $var -intervals]\}"]
               }
            } elseif { $value ne "" } {
               if { [expr { $param eq "interspecs" }] } {
                  set params [concat $params "-$param \{$value\}"]
               } else {
                  set params [concat $params "-$param $value"]
               }
            }
         }
         
         set params [concat $params "\}"]
         lappend configLst $params
      }
   }
   return $configLst
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
# Remarques :   Si le parametre recu n'est pas un chiffre, renvoyer l'identifiant du 
#               vp courant
#
#-------------------------------------------------------------------------------

proc APViz::GetVPId { VPno } {
   variable Data

   set vps [Page::Registered $Data(Frame) Viewport]
   
   if { $VPno=="" || [incr VPno -1]>[llength $vps] } {
      return [lindex $vps end]
   } else {
      return [lindex $vps $VPno]
   }
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
   
   if { $product eq "" } {
      return
   }
   
   variable ${product}::Value

   for { set idx 0 } { $idx < $Value(NbLayers) } { incr idx } {
      if { $Value(RowIDLayer$idx) >= 0 } {
         APViz::AssignVariable $product $idx
      }	
   }
   APViz::Refresh $product
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

proc APViz::ManageColormaps { Product Path } {
   global env
   variable Data
   variable DataSrc
   variable ${Product}::Value

   set colorLst {}

   for {set i 0} {$i < $Value(NbLayers)} {incr i} {
      if {[set rowID $Value(RowIDLayer$i)] >= 0} {
         set ID [lindex $Data(LayerIDs) $rowID]
         if {[fstdfield is $ID]} {
            set name [fstdfield configure $ID -colormap]        
            set isModified [colormap modified $name]
            set initialColormap [APViz::GetInitialColormap $name]
            
            if {$isModified} {
               #-----Create new name by adding number or changing number at the end of the original name
               #----- Find same colormap folder to save modified colormap
               puts "---- Looking for Colormap in $Path"

               set colormapPath [file dirname $Path]
               
               #----- What if no Colormap directory was found?
               while {![file isdirectory ${colormapPath}/Colormap] && ([string length colormapPath] > 0)} {
                  if {![file isdirectory $colormapPath]} {
                     set colormapPath [file dirname $colormapPath]
                  } else {
                     #----- Remove / if ends with /
                     if {[string index $colormapPath [expr [string length $colormapPath] -1]] eq "/"} {
                        set colormapPath [string range $colormapPath 0 [expr [string length $colormapPath] -2]]
                     }
                     set lastSeperator [string last "/" $colormapPath]
                     regsub -start $lastSeperator [file tail $colormapPath] $colormapPath "" colormapPath
                  }
                  
                  if {$colormapPath eq "."} {
                     puts "Saving new colormap in .spi/Colormap"
                     set colormapPath $env(HOME)/.spi
                     break
                  }
               }
               
               set path ${colormapPath}/Colormap
               set derivatives [glob -nocomplain -tails -path $path $initialColormap*.rgba]
               
               set nbr 1
               set newName ${initialColormap}$nbr
               while {[lsearch -exact $derivatives $newName.rgba] >= 0} {
                  incr nbr
                  set newName ${initialColormap}$nbr
               }

               colormap create $newName
               colormap copy $newName $name
               colormap write $newName $path/${newName}.rgba
               
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
   variable FieldList

   foreach vp [Page::Registered $Data(Frame) Viewport] {
      Viewport::UnAssign $Data(Frame) $vp
   }
   
   #----- Liberer les ID
   foreach ID $Data(LayerIDs) {
      if {[fstdfield is $ID]} {
         fstdfield free $ID
      } elseif {[metobs is $ID]} {
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
      variable ${product}::Param
      variable ${product}::Value
      #----- Reinitialiser les textvariables pour calcul
      for {set i 0} {$i < $Value(NbCalcLayers)} {incr i} {
         set Value(Formula,$i) ""
      }
      
      if {[array exists Param]} {
         array unset Param
      }
      
      #----- Reset etiket value
      for {set i 0} {$i < $Value(NbLayers)} {incr i} {
         set Value(Etiket,$i) ""
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
   
   #----- Reinitialize dicts
   set Data(RangeNames) ""
   set Data(Ranges) ""
   set Data(AlphaDict) ""
   
   foreach column $Data(ColNames) {
      dict lappend Data(RangeNames) $column NONE
      dict lappend Data(Ranges) $column NONE
      
      if {$column eq "Vars"} {
         dict lappend Data(RangeNames) $column "ALL"
         dict lappend Data(Ranges) $column "ALL"
      }
   }
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
   set dataType ""

   if { [fstdfield is $ID] } {
      set vp [lindex [fstdfield stats $ID -tag] 1]
      Viewport::UnAssign $Data(Frame) $vp $ID 1     ; # Enlever variable du viewport
      fstdfield free $ID
      set dataType FLD
   } elseif { [metobs is $ID] } {
      set vp [lindex [metobs stats $ID -tag] 1]
      Viewport::UnAssign $Data(Frame) $vp $ID 1     ; # Enlever variable du viewport
      metobs free $ID
      set dataType OBS
   }

   if { [regexp CALC $ID] } {
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
      ::Dialog::Error . $Lbl(SelectProduct)
   } else {
      if {![regexp "tcl" [file tail $Param(FullName)]]} {
         set Param(FullName) $Param(FullName).tcl
      }
      APViz::GenerateConfigFile $Param(FullName)
   }
}

#----------------------------------------------------------------------------
# Nom      : <APViz::SaveImg>
# Creation : Aout 2018 - C. Nguyen - CMC/CMOE
#
# But      : Sauvegarder une image dans le path fourni dans le fichier APViz_Data.tcl
#
# Parametres        :
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc APViz::SaveImg { {FileName ""} } {
   global GDefs

   variable Data
   variable Lbl
   
   set path [APViz::GetImageDirPath]
   
   if {$path eq ""} {
      return
   }
   
   #----- Generate name (if necessary)
   if {$FileName eq ""} {
      set FileName $Data(CurrentProduct)_[clock format [clock seconds] -format %Y%m%d_%H%M%S]
   }
   
   #----- Set parameters to save image as using the PrintBox::Print procedure
   set ::PrintBox::Param(Path)     $path
   set ::PrintBox::Param(FullName) $path/${FileName}.png
   set ::PrintBox::Print(Type)     SAVE
   set ::PrintBox::Print(Device)   png
   set ::PrintBox::Print(WEBSite)  ""
   
   #----- Save image to path
   puts "Saving image to: $path"
   if { [::PrintBox::Print $Data(Frame) 0 0 [Page::CanvasWidth $Data(Frame)] [Page::CanvasHeight $Data(Frame)]] } {
      .apviz.dock.coo delete 0 [string length [.apviz.dock.coo get]]
      .apviz.dock.coo insert 0 [lindex $Lbl(SavingImage) $GDefs(Lang)]
      after [expr {1000*30}] ".apviz.dock.coo delete 0 [string length [.apviz.dock.coo get]]"
   }
}

#-------------------------------------------------------------------------------
# Nom      : <APViz::SelectFiletreeBranch>
# Creation : Aout 2018 - C. Nguyen - CMC/CMOE
#
# But      : Selection d'une branche de l'arborescence
#
# Parametres :
#  <Tree>    : Arbre
#  <Branch>  : Branche
#
# Retour    :
#  <Id>     : Identification
#
# Remarque :
#
#-------------------------------------------------------------------------------

proc APViz::SelectFiletreeBranch { Tree Branch Open } {
   global GDefs
   variable Data
   variable Lbl

   if { ![info exist Viewport::Data(Data$Page::Data(Frame))] } {
      return
   }

   set Data(Select) $Branch
   set filepath [$Tree get $Branch path]
   
   if {$filepath ne ""} {
      #----- Call selection function
      APViz::ReinitializeVP
      APViz::CloseFiles

      if {$Data(AutoUpdateEventID) ne ""} {
         after cancel $Data(AutoUpdateEventID)
         set Data(AutoUpdateEventID) ""
      }

      if {[file isfile $filepath]} {         
         APViz::Execute [APViz::Source $filepath $Data(Tab)] $Data(Tab)
      } else {
         puts "$filepath not a file"
      }
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
   variable ${Product}::Value

   if {$IsCalcLayer} {
      set id [lindex $Data(CalcIDs) $Value(RowIDCalc$Index)]
   } else {
      set id [lindex $Data(LayerIDs) $Value(RowIDLayer$Index)]
   }
   
   if {[fstdfield is $id]} {
      ::FSTD::ParamUpdate $id
   } elseif {[metobs is $id]} {
      ::Obs::ParamUpdate $id
   }   
}

#-------------------------------------------------------------------------------
# Nom      : <APViz::Start>
# Creation : Aout 2018 - C. Nguyen - CMC/CMOE -
#
# But      : Sourcer les fichiers APViz_Data.tcl des repertoires de config 
#
# Parametres :
#
# Retour:
#
# Remarques :
#
#-------------------------------------------------------------------------------

proc APViz::Start { } {
   global env
   variable Param
   variable DataSrc
   variable Lbl
   
   if { [info exists env(SPI_APVIZ)] } {
      foreach path [split $env(SPI_APVIZ) :] {
         lappend Param(ConfigPath) $path
         #----- Getting config files path
         if {[file isfile ${path}/APViz_Data.tcl]} {
            if {[catch {source ${path}/APViz_Data.tcl} error]} {
               lappend msg "Erreur de lecture dans ${path}/APViz_Data.tcl  : $error"
               lappend msg "Sourcing error in ${path}/APViz_Data.tcl : $error"
               ::Dialog::Error . $msg
            }
         } else {
            lappend msg "Fichier APViz_Data.tcl manquant de $path"
            lappend msg "File APViz_Data.tcl containing paths missing from $path"
            ::Dialog::Error . $msg
         }
         
         #----- Getting colormaps
         if {[file isdirectory ${path}/Colormap/]} {
            lappend DataSrc(Colormaps) ${path}/Colormap/
         }
      }
      
      if {[llength $Param(ConfigPath)] <= 0} {
         ::Dialog::Error . $Lbl(envSPI_APVIZ)
      }
   } else {
      ::Dialog::Error . $Lbl(envSPI_APVIZ)
   }
}

proc APViz::StartBatch {} {
   variable Param
   variable Data
   variable Error

   #----- Process specified products
   if { [info exists ::SPI::Args(Product)] } {
   
      #----- Check if te whole path or the name references an existing product
      if { [file exist [set path $SPI::Args(Product)]] || [set path [lsearch -inline -glob $Data(ProductPaths) */$SPI::Args(Product).tcl]]=="" } {
         Dialog::Error . $Error(Product) "\n\n$SPI::Args(Product)"
         return
      }

      APViz::ReinitializeVP
      APViz::CloseFiles

      if { $Data(AutoUpdateEventID) ne "" } {
         after cancel $Data(AutoUpdateEventID)
         set Data(AutoUpdateEventID) ""
      }

      set product [APViz::Source $path $Data(Tab)]
         
      foreach arg [array names ::SPI::Args] {
         set APViz::${product}::Range($arg) $SPI::Args($arg)
      }         
      
      APViz::Execute $product $Data(Tab)
   }
}

#-------------------------------------------------------------------------------
# Nom      : <APViz::TranslateExpression>
# Creation : Juin 2018 - C. Nguyen - CMC/CMOE -
#
# But      : 	Translate an expression by replacing the letters with the 
#               corresponding fields
#
# Parametres 	  :
#	<Product> : Le nom du produit selectionne (aussi le namespace) 
#	<Expr>	  : Expression to translate 
#	
# Retour:
#
# Remarques :
#
#-------------------------------------------------------------------------------

proc APViz::TranslateExpression { Product Expr } {
   variable Data
   variable ${Product}::Value
   
   set nb 0
   set interp ""

   #----- check for global interpolator operator
   if { [regexp {interp\([A-Z]\)} $Expr] } { set interp [string index $Expr 7]; regsub -all {interp\([A-Z]\)} $Expr "" Expr }
   
   #----- Apply predefined grouping
   if { [regexp {sum\(ALL\)} $Expr] }     { regsub -all {sum\(ALL\)} $Expr      [GetAllFieldsWithOp $Product nb +] Expr }
   if { [regexp {sum\(CHECKED\)} $Expr] } { regsub -all {sum\(CHECKED\)} $Expr  [GetAllFieldsWithOp $Product nb + True] Expr }
   if { [regexp {avg\(ALL\)} $Expr] }     { regsub -all {avg\(ALL\)} $Expr      \([GetAllFieldsWithOp $Product nb +]\)/$nb Expr }
   if { [regexp {avg\(CHECKED\)} $Expr] } { regsub -all {avg\(CHECKED\)} $Expr  \([GetAllFieldsWithOp $Product nb + True]\)/$nb Expr }

   #----- Replace letters by fields
   set interp [lindex $Data(LayerIDs) [lsearch -exact $Data(Alphas) $interp]]
   
   #---- For all key-value from AlphaDict, replace letters with field ids (alpha is dictionary key)
   #---- Parcourir le dictionaire AlphaDict et remplacer chaque terme
   dict for {alpha layerNo} $APViz::Data(AlphaDict) {
      set no [string range $layerNo 1 [string length $layerNo]]
      
      if {[string equal L [string index $layerNo 0]]} {
         set id [lindex $Data(LayerIDs) $Value(RowIDLayer$no)]
      } else {
         set id [lindex $Data(CalcIDs) $Value(RowIDCalc$no)]
      }
      
      if {[fstdfield is $id]} {
         if { $interp!="" } {
            regsub -all $alpha $Expr ($interp<<$id) Expr
         } else {
            regsub -all $alpha $Expr $id Expr
         }
      }
   }
   return $Expr
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

   if { $Data(CurrentProduct)!="" } {
   
      variable ${Data(CurrentProduct)}::Value
      variable ${Data(CurrentProduct)}::Param

      Legend::Delete $Frame  
      
      set no 0
      foreach fld $Data(LayerIDs) {
         set alpha $Value(Letter,$no)
         
         if { [fstdfield is $fld] && [fstdfield configure $fld -active] } {         
            set col  [fstdfield configure $fld -color]
            set eti  [fstdfield define $fld -ETIKET]
            set var  [fstdfield define $fld -NOMVAR]
            set lvl  [lrange [fstdgrid convip [fstdfield define $fld -IP1]] 0 1]
            set hr   [expr [fstdfield define $fld -DEET]*[fstdfield define $fld -NPAS]/3600]
            set secv [fstdstamp toseconds [fstdfield define $fld -DATEV]]
            set seco [fstdstamp toseconds [fstdfield define $fld -DATEO]]
            set lbl  [format "$alpha: %-5s %-13s %-4s %-8s %-3sH - valid:%s \[run:%s\]" $Value(Models,$no) $eti $var $lvl $hr [clock format $secv -format {%HZ %a %b %d,%Y} -timezone :UTC] [clock format $seco -format {%HZ %a %b %d,%Y} -timezone :UTC]]
            Legend::Add $Frame text UL -font XFont12 -fill $col -text $lbl
         } elseif { [metobs is $fld] && [dataspec configure $fld -active] } {         
            set col [dataspec configure $fld -color]
            set eti Obs
            set var [dataspec configure $fld -desc]
            set lvl Surface
            set lbl [format "$alpha: %-5s %-13s %-4s %-s" $Value(Models,$no) $eti $var $lvl]
            Legend::Add $Frame text UL -font XFont12 -fill $col -text $lbl
         }
         incr no
      }
      
      set no 0
      foreach fld $Data(CalcIDs) {
         set alpha $Value(CLetter,$no)
         
         if { [fstdfield is $fld] && [fstdfield configure $fld -active] } {         
            set col  [fstdfield configure $fld -color]
            set eti  [fstdfield define $fld -ETIKET]
            set var  [fstdfield define $fld -NOMVAR]
            set desc [fstdfield configure $fld -desc]
            #----- TODO: Recuperer la formule pour decrire le calcul
            set lvl [lrange [fstdgrid convip [fstdfield define $fld -IP1]] 0 1]
            set hr  [expr [fstdfield define $fld -DEET]*[fstdfield define $fld -NPAS]/3600]
            set lbl [format "$alpha: %s %s" $Value(Formula,$no) $desc]
            Legend::Add $Frame text UL -font XFont12 -fill $col -text $lbl
         }
         incr no
      }
      
      Legend::Add $Frame LOGO LL 
      Legend::Add $Frame text LL -font XFont12 -fill black   -text "CMC Environment Canada - $APViz::Data(CurrentProduct) - Produced [clock format [clock seconds]]" 
      Legend::Add $Frame text LL -font XFont12 -fill black   -text $Param(Title) 
     
      eval set proc \[info procs ::APViz::$Data(CurrentProduct)::UpdateItems\]
      if { $proc!="" } {
         eval $proc $Frame
      }
   }
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

proc APViz::UpdateProductInterface { FileName Path } { 
   variable Data
   
   set parentNode [file tail [file dirname $Path]]
   #----- Verify if already exist
   if {![FILETREE exists $FileName]} {
      #----- Add new node
      FILETREE insert $parentNode end $FileName
   }

   FILETREE set $FileName open False
   FILETREE set $FileName name $FileName
   FILETREE set $FileName path $Path

   lappend Data(ProductPaths) $Path
   
   CVTree::Render $Data(Tab).filetree.canvas APViz::FILETREE
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
      ::Dialog::Error . $Lbl(WrongDate)
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
   variable Data
   variable DataSrc
   variable Lbl
   
   for {set i 0} {$i < $Value(NbLayers)} {incr i} {
      if {$Value(RowIDLayer$i) >= 0} {
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

   if {(!$Data(dateLock) && ($Data(Date) ne [lindex $Data(Dates) [expr [llength $Data(Dates)] -1 ]])) || ($Data(Date) eq "")} {
      set Data(Date) [lindex $Data(Dates) [expr [llength $Data(Dates)] -1 ]]
      #----- Afficher message
      .apviz.dock.coo insert 0 [lindex $Lbl(UpdatingDate) $GDefs(Lang)]
      after [expr {1000*30}] ".apviz.dock.coo delete 0 [string length [.apviz.dock.coo get]]"
      APViz::InitializeVars
   }

   set Data(AutoUpdateEventID) [after [expr {1000*60*1}] APViz::UpdateAvailableDates $Product] ; # Update a chaque 10min:1000*60*10
}

#----------------------------------------------------------------------------
# Nom      : <APViz::WriteCalcLayers>
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

proc APViz::WriteCalcLayers { Product FileID } {
   variable Data
   variable ${Product}::Value

   puts $FileID "\#----- CalcLayers (On:FormulaName:VP)"
   puts $FileID "set CalcLayers \{"
   
   #----- CalcLayers (On:FormulaName:A_rowNb:B_rowNb:VP)
   for {set i 0} {$i < $Value(NbCalcLayers)} {incr i} {
      if {[set rowID $Value(RowIDCalc$i)] >= 0} {
         #----- Get all params
         set toggle             $Value(CalcToggle,$i)
         set vp                 $Value(CalcVP,$i)
         
         #----- Get formula name from formula
         set formulaName [APViz::GetFormulaName $Value(Formula,$i)]
         puts $FileID "   $toggle:$formulaName:$vp"
      }
   }
   puts $FileID "\}"
}

#----------------------------------------------------------------------------
# Nom      : <APViz::WriteCameraConfigs>
# Creation : Juillet 2018 - C. Nguyen - CMC/CMOE
#
# But      :    Recupere les parametres de camera et les ecrire dans le 
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

proc APViz::WriteCameraConfigs { FileID } {
   variable Data

   puts $FileID "set Param(Camera) \{${::ProjCam::Data(Name)}\}"
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

   foreach line $DataSource {
      puts $FileID $line
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

   puts $FileID "\#----- Layers (On:Model:Run:Hour:Source:Var:Level:IP3:VP)"
   puts $FileID "set Layers \{"
   for {set i 0} {$i < $Value(NbLayers)} {incr i} {
      if {[set rowID $Value(RowIDLayer$i)] >= 0} {
         puts $FileID "   [lindex $Data(Layers) $Value(LayerType,$rowID)]"
      }
   }
   puts $FileID "\}"
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
   set params "set Param(Projection) \{"
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
# Nom      : <APViz::WriteRanges>
# Creation : Juillet 2018 - C. Nguyen - CMC/CMOE
#
# But      : Ecrire la section des ranges dans le fichier de config
#
# Parametres    :
#       <Product>     : Le nom du produit selectionne (aussi le namespace) 
#       <FileID>      : Identifiant du fichier dans lequel ecrire
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc APViz::WriteRanges { Product FileID } {
   variable ${Product}::Range
   variable FieldList
   
   #----- Copy the range configs
   puts $FileID "\#----- Ranges"
   foreach rangeConfig [array names Range] {
      if { [lsearch -exact [array names FieldList] $rangeConfig] < 0 } {
         set rangeValues $Range($rangeConfig)
         puts $FileID "set Range($rangeConfig) \{$rangeValues\}"
      }
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
      
      if { ($index < 0) && ($content ne "") } {
         puts $FileID $content
      }
   }
}

#----------------------------------------------------------------------------
# Nom      : <APViz::WriteViewportConfigs>
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

proc APViz::WriteViewportConfigs { Product FileID } {
   variable ${Product}::Param
   variable Data
   
   set params "set Param(Viewport) \{"
   set paramLst [list crowd font bg bd colorcoast colorlake colorfillcoast colorfilllake colorriver colorpolit coloradmin colorcity colorroad colorplace colorcoord]
   set vp [lindex [Page::Registered ${::APViz::Data(Frame)} Viewport] 0]

   foreach param $paramLst {
      set value [${::APViz::Data(Frame)}.page.canvas itemconfigure $vp -$param]
      set paramConfig "-$param [lindex $value [expr [llength $value] - 1]]"
      set params [concat $params $paramConfig]
   }
   
   set params [concat $params "\}"]   
   puts $FileID $params
   puts $FileID "set Param(Layout) $Param(Layout)"
}

