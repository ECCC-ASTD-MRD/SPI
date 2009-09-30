#===============================================================================
# Environnement Canada
# Centre Meteorologique Canadian
# 2100 Trans-Canadienne
# Dorval, Quebec
#
# Projet   : Exemples de scripts de productions de cartes.
# Fichier  : Flux.tcl
# Creation : Fevrier 2001 - J.P.Gauthier - CMC/CMOE
#
# Description:
#    Calcul du flux traversant une coupe transversale.
#
# Arguments  :
#
# Remarques :
#
#===============================================================================

namespace eval Macro::Flux {} {
   variable Param
   variable Data
   variable Error
   variable Lbl

   set Param(In)      ""
   set Param(Info)   { "Calcul du flux traversant une coupe transversale"
                       "Calculate flux through a vertical cross section" }

   set Data(Page)        ""
   set Data(Segments)    { "Segment 1" "Segment 2" }
   set Data(Segment)     "Segment 1"
   set Data(Resolutions) { 100000 10000 1000 100 10 1 }
   set Data(Resolution)  "10000"
   set Data(Coords)      { { 17.74 -101.26 17.75 -98.59 16.95 -97.96 }
                         { 38 -120 6 -98  17 -74 } }
   set Data(Graph)       ""

   set Lbl(Select)     { "Selection" "Select" }
   set Lbl(Resolution) { "Resoultion (m)" "Sampling (m)" }
   set Error(Field)    { "Aucun champs valide trouvé\nvérifié le viewport actif"
                         "Could not find any valid field\nCheck the active viewport" }
}

proc Macro::Flux::Execute { } {
   global GDefs
   variable Lbl
   variable Data

   if { ![winfo exists $Data(Page)] } {
      set Data(Page)   [SPI::PageNew True "Macro FLux 1.0" 600x600]
      set Data(Canvas) $Data(Page).page.canvas
   } else {
      raise [winfo toplevel $Data(Page)]
      return
   }

   set top [winfo toplevel $Data(Page)]
   wm transient $top .

   frame $top.fr
      label $top.fr.lbl -text [lindex $Lbl(Select) $GDefs(Lang)];
      ComboBox::Create $top.fr.sel Macro::Flux::Data(Segment) noedit sorted nodouble -1 $Data(Segments) 20 5 Macro::Flux::Process
      label $top.fr.lblr -text [lindex $Lbl(Resolution) $GDefs(Lang)];
      ComboBox::Create $top.fr.selr Macro::Flux::Data(Resolution) noedit sorted nodouble -1 $Data(Resolutions) 20 5 Macro::Flux::Process
      pack $top.fr.lbl $top.fr.sel $top.fr.lblr $top.fr.selr -side left
   pack $top.fr -side top -before $Data(Page) -fill x
}

proc Macro::Flux::Process { } {
   global GDefs
   variable Data
   variable Error

   set field [lindex [Viewport::Assigned $Page::Data(Frame) $Viewport::Data(VP)] 0]
   set coords [lindex $Data(Coords) [lsearch -exact $Data(Segments) $Data(Segment)]]

   if { ![fstdfield is $field] } {
      Macro::Error $Error(Field)
      return
   }

   Macro::Flux::Clean

   set fid    [fstdfield define $field -FID]
   set datev  [fstdfield define $field -DATEV]
   set etiket [fstdfield define $field -ETIKET]
   set ip1    [fstdfield define $field -IP1]
   set ip2    [fstdfield define $field -IP2]
   set ip3    [fstdfield define $field -IP3]
   set typvar [fstdfield define $field -TYPVAR]
   set nomvar [fstdfield define $field -NOMVAR]

   set path [projection function $Page::Data(Frame) -path $coords $Data(Resolution)]
   set dist [projection function $Page::Data(Frame) -dist $path 0]

   fstdfield copy Macro::CONC $field
   fstdfield read Macro::WIND $fid $datev "" $ip1 $ip2 $ip3 "" "UU"

   fstdfield vertical Macro::XWIND Macro::WIND $path
   fstdfield vertical Macro::XCONC $field $path

   vexpr Macro::FLUX Macro::XWIND\[1\]*Macro::XCONC
   fstdfield define Macro::FLUX -NOMVAR FLUX
#   set total [expr $total+[vexpr FLUX ssum(FLUX)/(3600*3)]]

   FSTD::Register Macro::FLUX

  $Data(Canvas) create graph -x 1 -y 1 -width [Page::CanvasWidth $Data(Page)] -height [Page::CanvasHeight $Data(Page)] \
       -anchor nw -xlegend 5 -ylegend 5 -fg #000000 -bg #FFFFFF -fill #000000 -tags GRAPH -font XFont12 -title Flux
   bind $Data(Canvas) <Configure> "update idletasks ; $Data(Canvas) itemconfigure GRAPH -x 1 -y 1 -width \[Page::CanvasWidth $Data(Page)\] -height \[Page::CanvasHeight $Data(Page)\]"

   set lvl  [fstdfield stats Macro::FLUX -levels]
   set ymin [lindex $lvl 0]
   set ymax [lindex $lvl end]
   set xmin 0
   set xmax [fstdfield define Macro::FLUX -NI]

   graphaxis create Macro::AXISX
   graphaxis create Macro::AXISY
   graphaxis configure Macro::AXISX -type LINEAR -min $xmin -max $xmax -unit Distance -font XFont12 -gridcolor #010101 -gridwidth 1 -color #000000 -position LL -width 1
   graphaxis configure Macro::AXISY -type LINEAR -min $ymin -max $ymax -intervals $lvl -unit Level -font XFont12 -color #000000 -position LL -width 1

   graphitem create Macro::ITEM
   graphitem configure Macro::ITEM -xaxis Macro::AXISX -yaxis Macro::AXISY -data Macro::FLUX

   $Data(Canvas) itemconfigure GRAPH$Data(Graph) -item Macro::ITEM
}

proc Macro::Flux::Clean { } {
   variable Data

   fstdfield free Macro::CONC
   fstdfield free Macro::XCONC
   fstdfield free Macro::XWIND
   fstdfield free Macro::FLUX

   graphaxis free Macro::AXISX
   graphaxis free Macro::AXISY
   graphitem free Macro::ITEM
}





