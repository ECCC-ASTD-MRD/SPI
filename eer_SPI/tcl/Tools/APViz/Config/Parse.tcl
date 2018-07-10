namespace eval APViz {
  variable Value
  set Value(Models)	""
  set Value(Vars)	""
  set Value(Levels)	""
  set Value(Sources)	""
  set Value(Runs)	""
  set Value(Hours)	""
}

proc APViz::Source { Path Widget } {
   variable Data
   set product [file rootname [file tail $Path]]
   puts "Product: $product"
   puts "Path: $Path"

   #----- Create product namespace
   #namespace eval $product 
   namespace eval $product {
    
      variable Layers
      variable Range
      
      proc Load { Path Product Widget } {	
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
	label $Widget.range.variableGrid.mod 	-text "Modèle" 		-width 7
	label $Widget.range.variableGrid.var 	-text "NomVar"		-width 7
	label $Widget.range.variableGrid.lev 	-text "Niveau"		-width 7
	label $Widget.range.variableGrid.src 	-text "Source"		-width 7
	label $Widget.range.variableGrid.run 	-text "Run"		-width 7
	label $Widget.range.variableGrid.hour 	-text "Heure"		-width 7
	
	grid $Widget.range.variableGrid 	-column 0 -row 0 -padx 0.2
	grid $Widget.range.variableGrid.mod 	-column 1 -row 0 -padx 0.2
	grid $Widget.range.variableGrid.var 	-column 2 -row 0 -padx 0.2
	grid $Widget.range.variableGrid.lev 	-column 3 -row 0 -padx 0.2
	grid $Widget.range.variableGrid.run 	-column 4 -row 0 -padx 0.2
	grid $Widget.range.variableGrid.hour 	-column 5 -row 0 -padx 0.2
	grid $Widget.range.variableGrid.src 	-column 6 -row 0 -padx 0.2
	
	set no 0
	foreach layer $Layers {
	  puts "Iteration: $no"
	  #----- Extract layer parts
	  lassign [split $layer :] toggle model var level hour interval run dataSource
	  
	  #----- Toggle On/Off
	  checkbutton $Widget.range.variableGrid.layer${no}_toggle -anchor w -var APViz::${Product}::Toggle($no)
	  
	  # CreateRangeWidget { Style Path Index Options IsSpinBox }
	  CreateRangeWidget $model $Widget.range.variableGrid.layer${no}_model $no Models true		; #Range names have to be the same in all config files
	  CreateRangeWidget $var $Widget.range.variableGrid.layer${no}_var $no Vars false
	  CreateRangeWidget $level $Widget.range.variableGrid.layer${no}_level $no Levels true
	  CreateRangeWidget $hour $Widget.range.variableGrid.layer${no}_hour $no Hours true
	  CreateRangeWidget $run $Widget.range.variableGrid.layer${no}_run $no Runs true
	  CreateRangeWidget $dataSource $Widget.range.variableGrid.layer${no}_dataSource $no Sources true
	  
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
      
      proc CreateRangeWidget { Style Path Index Options IsSpinBox } {
      variable Range
	if { [string index $Style 0] eq "<" } {
	  if $IsSpinBox {
	    spinbox $Path -values $Range($Options) -width 5	; #-var APViz::${Product}::Range(Variables) 
	  } else {
	    ComboBox::Create $Path	APViz::Value($Options,$Index)	noedit unsorted nodouble -1 $Range($Options) 5 8 {}
	  }
	} else {
	    label $Path -width 5 -text $Style
	}
      }
   }
   
   ${product}::Load $Path $product $Widget
}


