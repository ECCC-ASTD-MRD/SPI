#===============================================================================
# Environnement Canada
# Centre Meteorologique Canadian
# 2100 Trans-Canadienne
# Dorval, Quebec
#
# Projet   : Librairie d'objects visuel interactifs
# Fichier  : Graph_Scatter.tcl
# Creation : Octobre 2003 - J.P. Gauthier - CMC/CMOE
#
# Description: Ce package s'occupe de l'affichage et de la manipulation de
#              ddiagramme de dispersion
#
# Fonctions:
#
#    Graph::Scatter::Create        { Frame X0 Y0 Width Height Active Full }
#    Graph::Scatter::Coord         { Frame GR X Y }
#    Graph::Scatter::Clean         { GR }
#    Graph::Scatter::DrawInit      { Frame VP }
#    Graph::Scatter::Draw          { Frame VP }
#    Graph::Scatter::DrawDone      { Frame VP }
#    Graph::Scatter::MoveInit      { Frame VP }
#    Graph::Scatter::Move          { Frame VP }
#    Graph::Scatter::MoveDone      { Frame VP }
#    Graph::Scatter::Graph         { GR }
#    Graph::Scatter::Init          { Frame }
#    Graph::Scatter::Params        { Parent GR }
#    Graph::Scatter::Stats         { GR }
#    Graph::Scatter::ItemAdd       { GR Item }
#    Graph::Scatter::ItemDefault   { GR Item }
#    Graph::Scatter::ItemDel       { GR Item }
#    Graph::Scatter::ItemDefine    { GR Pos Coords { Update True } }
#    Graph::Scatter::ItemUnDefine  { GR Pos }
#    Graph::Scatter::ItemData      { GR Pos Item Data }
#    Graph::Scatter::Update        { Frame { GR {} } }
#    Graph::Scatter::Data          { GR { Data { } } { Files { } } }
#    Graph::Scatter::UpdateItems   { Frame { GR { } } }
#    Graph::Scatter::ItemDataField { Item Coords Data0 Data1 }
#    Graph::Scatter::ItemDataObs   { Item Coords Data0 Data1 }
#
#===============================================================================

namespace eval Graph::Scatter { } {
   variable Lbl

   set Lbl(Title)    { "Diagramme de dispersion" "Scatter plot" }
}

#----------------------------------------------------------------------------
# Nom      : <Graph::Scatter::Create>
# Creation : Octobre 2003 - J.P. Gauthier - CMC/CMOE
#
# But      : Creer l'object graph et ses structures
#
# Parametres :
#   <Frame>  : Identificateur de Page
#   <X0>     : Coordonee X du coin superieur gauche
#   <Y0>     : Coordonee Y du coin superieur gauche
#   <Width>  : Largeur du Graph
#   <Height> : Hauteur du Graph
#   <Active> : Fonction active (Deplacement,Agrandisement)
#   <Full>   : Mode Full screen
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc Graph::Scatter::Create { Frame X0 Y0 Width Height Active Full } {
   global GDefs
   variable Data
   variable Lbl

   set gr [Graph::Scatter::Init $Frame]
   set tag $Page::Data(Tag)$gr

   if { $Full } {
      set X0       0
      set Y0       0
      set Width   [winfo width  $Page::Data(Canvas)]
      set Height  [winfo height $Page::Data(Canvas)]
   }

   set Graph::Data(Full$gr)     $Full      ;#Mode FullCanvas
   set Graph::Data(Active$gr)   $Active    ;#Mode Active (Manipulation in place)
   set Graph::Data(X$gr)        $X0        ;#Offset en x
   set Graph::Data(Y$gr)        $Y0        ;#Offset en y
   set Graph::Data(Width$gr)    $Width     ;#Largeur du graph
   set Graph::Data(Height$gr)   $Height    ;#Hauteur du graph
   set Graph::Data(ToolMode$gr) Data       ;#Mode de selection
   set Graph::Data(Type$gr)     Scatter    ;#Type de graph

   upvar #0 Graph::Scatter::Scatter${gr}::Data  data
   upvar #0 Graph::Scatter::Scatter${gr}::Graph graph

   set data(Canvas)    $Frame.page.canvas
   set data(Frame)     $Frame

   $data(Canvas) bind GRAPHUPDATE$gr <Any-KeyRelease> "$data(Canvas) itemconfigure $gr -update True"
   set id [$data(Canvas) create text $X0 $Y0  -tags "$tag CVTEXT GRAPHUPDATE$gr" -text [lindex $Lbl(Title) $GDefs(Lang)] \
      -font $Graph::Font(Graph) -fill black -anchor nw -justify center]
   $data(Canvas) create graph -x $X0 -y $Y0 -width $Width -height $Height -anchor nw -xlegend 5 -ylegend 5 -command $gr \
       -fg black -bg $Graph::Color(Frame) -fill $Graph::Color(Graph) -tags "$tag $gr" -font $Graph::Font(Graph) -title $id
   $data(Canvas) raise $id

   #----- Creation des unite de l'echelle

   graphaxis create axisx$gr
   graphaxis create axisy$gr

   set id [$data(Canvas) create text -100 -100  -tags "$tag CVTEXT GRAPHUPDATE$gr" -text $graph(UnitX) \
      -font $Graph::Font(Axis) -fill $Graph::Color(Axis) -anchor nw -justify center]
   graphaxis configure axisx$gr -font $Graph::Font(Axis) -color $Graph::Color(Axis) -gridcolor $Graph::Grid(Color) \
      -dash $Graph::Grid(Dash) -position LL -width 1 -unit $id

   set id [$data(Canvas) create text -100 -100  -tags "$tag CVTEXT GRAPHUPDATE$gr" -text $graph(UnitY) \
      -font $Graph::Font(Axis) -fill $Graph::Color(Axis) -anchor nw -justify center]
   graphaxis configure axisy$gr -font $Graph::Font(Axis) -color $Graph::Color(Axis) -gridcolor $Graph::Grid(Color) \
      -dash $Graph::Grid(Dash) -position LL -width 1 -unit $id

   if { $Viewport::Data(VP)!="" } {
      set data(VP)        $Viewport::Data(VP)
      set data(FrameData) $Viewport::Data(Frame$data(VP))
      Graph::Scatter::Update $data(FrameData) $gr
   } else {
      set data(VP)        ""
      set data(FrameData) ""
   }

   Graph::Activate $Frame $gr Scatter
   Graph::Mode Scatter $gr True
   Graph::PosAdd $gr Scatter

   #----- Creer les fonction du mode actif

   if { $Active } {
      Page::ActiveWrapper Graph $Frame $gr $X0 $Y0 [expr $Width+$X0] [expr $Height+$Y0] Scatter
   }
   Page::Register $Frame Graph::Scatter $gr

   return $gr
}

#-------------------------------------------------------------------------------
# Nom      : <Graph::Scatter::Coord>
# Creation : Octobre 2002 - J.P. Gauthier - CMC/CMOE
#
# But      : Evalue les valeurs de concentrations a la position du curseur
#            dans le graph.
#
# Parametres :
#   <Frame>  : Identificateur de Page
#   <GR>     : Indentificateur du Graph
#   <X>      : Coordonnee X a l'interieur du canvas
#   <Y>      : Coordonnee Y a l'interieur du canvas
#
# Retour    :
#
# Remarque :
#
#-------------------------------------------------------------------------------

proc Graph::Scatter::Coord { Frame GR X Y } {
   global   GDefs

   upvar #0 Graph::Scatter::Scatter${GR}::Data  data
   upvar #0 Graph::Scatter::Scatter${GR}::Graph graph

   set Page::Data(Coord) ""
   set Page::Data(Value) ""

   if  { [llength [set items [lindex [$data(Canvas) itemconfigure $GR -item] end]]] } {
      set coords [$GR -unproject $X $Y False [lindex $items 0]]

      if { [llength $coords]==2 } {
         set Page::Data(Value) "$graph(UnitX): [lindex $coords 0]"
         set Page::Data(Value) "$Page::Data(Value) $graph(UnitY): [lindex $coords 1]"
      }
   }
}

#-------------------------------------------------------------------------------
# Nom      : <Graph::Scatter::Clean>
# Creation : Octobre 2002 - J.P. Gauthier - CMC/CMOE -
#
# But      : Supprimer les donnees associees aux coordonees.
#
# Parametres :
#   <GR>     : Identificateur du Graph
#
#-------------------------------------------------------------------------------

proc Graph::Scatter::Clean { GR } {

   upvar #0 Graph::Scatter::Scatter${GR}::Data  data

   if { $data(Stat)!="" && [llength [$Frame.page.canvas find withtag GRAPHSTAT$data(Stat)]] } {
      Graph::Destroy $data(Frame) $data(Stat) Stat
   }
}

#----------------------------------------------------------------------------
# Nom      : <Graph::Scatter::Draw...>
# Creation : Octobre 2002 - J.P. Gauthier - CMC/CMOE
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

proc Graph::Scatter::DrawInit { Frame VP } {

   upvar #0 Graph::Scatter::Scatter${Graph::Data(Graph)}::Data  data

   set data(Lat0) $Viewport::Map(LatCursor)
   set data(Lon0) $Viewport::Map(LonCursor)
   set data(Lat1) $Viewport::Map(LatCursor)
   set data(Lon1) $Viewport::Map(LonCursor)

   if { $data(FrameData)!="" } {
      $data(FrameData).page.canvas delete GRAPHSCATTER$Graph::Data(Graph)
   }

   if { $VP!=$data(VP) } {
      set data(VP)        $VP
      set data(FrameData) $Frame
      Graph::Contingency::Update $Frame $Graph::Data(Graph)
   }
}

proc Graph::Scatter::Draw { Frame VP } {
   global   GDefs
   variable Lbl

   upvar #0 Graph::Scatter::Scatter${Graph::Data(Graph)}::Data  data

   set data(Lat1)   $Viewport::Map(LatCursor)
   set data(Lon1)   $Viewport::Map(LonCursor)

   Graph::Scatter::ItemDefine $Graph::Data(Graph) $Graph::Data(Pos) [list $data(Lat0) $data(Lon0) $data(Lat1) $data(Lon1)] False
}

proc Graph::Scatter::DrawDone { Frame VP } {

   upvar #0 Graph::Scatter::Scatter${Graph::Data(Graph)}::Data  data

   if { $data(Lat0)>$data(Lat1) } {
      set tmp $data(Lat1)
      set data(Lat1) $data(Lat0)
      set data(Lat0) $tmp
   }

   if { $data(Lon0)>$data(Lon1) } {
      set tmp $data(Lon1)
      set data(Lon1) $data(Lon0)
      set data(Lon0) $tmp
   }

   if { $data(Lat0)==$data(Lat1) || $data(Lon0)==$data(Lon1) } {
      set data(Lat0) 0
      set data(Lat1) 0
      set data(Lon0) 0
      set data(Lon1) 0
      set data(Pos$Graph::Data(Graph)$Graph::Data(Item)) {}
   } else {
      set data(Pos$Graph::Data(Graph)$Graph::Data(Item)) [list $data(Lat0) $data(Lon0) $data(Lat1) $data(Lon1)]
   }

   Graph::Scatter::ItemDefine $Graph::Data(Graph) $Graph::Data(Pos) [list $data(Lat0) $data(Lon0) $data(Lat1) $data(Lon1)]
}

proc Graph::Scatter::MoveInit { Frame VP } {

   upvar #0 Graph::Scatter::Scatter${Graph::Data(Graph)}::Data  data

   set data(LonD) $Viewport::Map(LonCursor)
   set data(LatD) $Viewport::Map(LatCursor)

   if { $data(FrameData)!="" } {
      $data(FrameData).page.canvas delete GRAPHSCATTER$Graph::Data(Graph)
   }

   set data(VP)        $VP
   set data(FrameData) $Frame
}

proc Graph::Scatter::Move { Frame VP } {
   global   GDefs
   variable Lbl

   upvar #0 Graph::Scatter::Scatter${Graph::Data(Graph)}::Data  data

   #----- Effectuer la translation

   set lat0 [expr $data(Lat0) + $Viewport::Map(LatCursor) - $data(LatD)]
   set lat1 [expr $data(Lat1) + $Viewport::Map(LatCursor) - $data(LatD)]

   if { $lat0 > -90.0 && $lat0 < 90.0 && $lat1 > -90.0 && $lat1 < 90.0 } {

      set data(Lat0) $lat0
      set data(Lat1) $lat1
      eval set data(Lon0) [Viewport::CheckCoord [expr $data(Lon0) + $Viewport::Map(LonCursor) - $data(LonD)]]
      eval set data(Lon1) [Viewport::CheckCoord [expr $data(Lon1) + $Viewport::Map(LonCursor) - $data(LonD)]]
   }

   #----- Reaffecter le point de reference de translation

   set data(LonD) $Viewport::Map(LonCursor)
   set data(LatD) $Viewport::Map(LatCursor)

   Graph::Scatter::ItemDefine $Graph::Data(Graph) $Graph::Data(Pos) [list $data(Lat0) $data(Lon0) $data(Lat1) $data(Lon1)] False
}

proc Graph::Scatter::MoveDone { Frame VP } {

   Graph::Scatter::DrawDone $Frame $VP
}

#-------------------------------------------------------------------------------
# Nom      : <Graph::Scatter::Graph>
# Creation : Fevrier 1999 - J.P. Gauthier - CMC/CMOE
#
# But      : Affiche le graphique des series temporelles.
#
# Parametres :
#   <GR>     : Indentificateur du Graph
#
# Retour    :
#
# Remarque :
#
#-------------------------------------------------------------------------------

proc Graph::Scatter::Graph { GR } {
   variable Data

   upvar #0 Graph::Scatter::Scatter${GR}::Graph graph
   upvar #0 Graph::Scatter::Scatter${GR}::Data  data

   if { ![llength $data(Items)] } {
      return
   }

   #----- Recalculer les valeurs

   set data(XMin)  1e200
   set data(XMax) -1e200
   set data(YMin)  1e200
   set data(YMax) -1e200
   set xincr 0
   set yincr 0
   set mod True

   foreach item $data(Items) {
      set min [vector stats $item.X -min]
      set max [vector stats $item.X -max]
      set data(XMin) [expr $min<$data(XMin)?$min:$data(XMin)]
      set data(XMax) [expr $max>$data(XMax)?$max:$data(XMax)]

      set min [vector stats $item.Y -min]
      set max [vector stats $item.Y -max]
      set data(YMin) [expr $min<$data(YMin)?$min:$data(YMin)]
      set data(YMax) [expr $max>$data(YMax)?$max:$data(YMax)]

      graphitem configure $item -fit $graph(Fit)
   }

   #----- Verifier la selection de l'usager
   if { ![set l [llength $graph(YInter)]] } {
      set yinter ""
   } else {
      set data(YMin) [lindex $graph(YInter) 0]
      set data(YMax) [lindex $graph(YInter) end]
      if { $l==2 } {
         set yinter {}
      } else {
         set yinter $graph(YInter)
      }
   }

   #----- Verifier la selection de l'usager
   if { ![set l [llength $graph(XInter)]] } {
      set xinter ""
   } else {
      set data(XMin) [lindex $graph(XInter) 0]
      set data(XMax) [lindex $graph(XInter) end]
      if { $l==2 } {
         set xinter {}
      } else {
         set xinter $graph(XInter)
      }
   }

   if { [llength $graph(ZXInter)] } {
      set data(XMin) [lindex $graph(ZXInter) 0]
      set data(XMax) [lindex $graph(ZXInter) 1]
      set mod False
   }
   if { [llength $graph(ZYInter)] } {
      set data(YMin) [lindex $graph(ZYInter) 0]
      set data(YMax) [lindex $graph(ZYInter) 1]
      set mod False
   }

   if { $l } {
      set data(Min) $data(XMin)
      set data(Max) $data(XMax)
   } else {
      set data(Min) [expr $data(XMin)<$data(YMin)?$data(XMin):$data(YMin)]
      set data(Max) [expr $data(XMax)>$data(YMax)?$data(XMax):$data(YMax)]
   }

   set id [graphaxis configure axisx$GR -unit]
   if { $Graph::Data(Update) } {
      $data(Canvas) itemconfigure $id -text $graph(UnitX)
   }
   $data(Canvas) itemconfigure $id -font $Graph::Font(Axis) -fill $Graph::Color(Axis)
   set id [graphaxis configure axisy$GR -unit]
   if { $Graph::Data(Update) } {
      $data(Canvas) itemconfigure $id -text $graph(UnitY)
   }
   $data(Canvas) itemconfigure $id -font $Graph::Font(Axis) -fill $Graph::Color(Axis)

   if { $graph(Uniform) } {
      graphaxis configure axisx$GR -type $graph(XScale) -modulo $mod -min $data(Min) -max $data(Max) -intervals $xinter -increment $xincr -angle $Graph::Font(Angle) \
         -font $Graph::Font(Axis) -gridcolor $Graph::Grid(Color)  -dash $Graph::Grid(Dash) -gridwidth $Graph::Grid(Width) -color $Graph::Color(Axis)
      graphaxis configure axisy$GR -type $graph(XScale) -modulo $mod -min $data(Min) -max $data(Max) -intervals $xinter -increment $xincr -angle $Graph::Font(Angle) \
         -font $Graph::Font(Axis) -gridcolor $Graph::Grid(Color)  -dash $Graph::Grid(Dash) -gridwidth $Graph::Grid(Width) -color $Graph::Color(Axis)
   } else {
      graphaxis configure axisx$GR -type $graph(XScale) -modulo $mod -min $data(XMin) -max $data(XMax) -intervals $xinter -increment $xincr -angle $Graph::Font(Angle) \
         -font $Graph::Font(Axis) -gridcolor $Graph::Grid(Color)  -dash $Graph::Grid(Dash) -gridwidth $Graph::Grid(Width) -color $Graph::Color(Axis)
      graphaxis configure axisy$GR -type $graph(YScale) -modulo $mod -min $data(YMin) -max $data(YMax) -intervals $yinter -increment $yincr -angle $Graph::Font(Angle) \
         -font $Graph::Font(Axis) -gridcolor $Graph::Grid(Color)  -dash $Graph::Grid(Dash) -gridwidth $Graph::Grid(Width) -color $Graph::Color(Axis)
   }

   set id [lindex [$data(Canvas) itemconfigure $GR -title] end]
   $data(Canvas) itemconfigure $id -font $Graph::Font(Graph) -fill $Graph::Color(FG)
   $data(Canvas) itemconfigure $GR -item $data(Items) -bd $Graph::Width(Frame) \
      -fg $Graph::Color(FG) -bg $Graph::Color(BG) -fill $Graph::Color(Fill) -font $Graph::Font(Graph)

   if { $data(Stat)!="" && [llength [$data(Canvas) find withtag GRAPHSTAT$data(Stat)]] } {
      Graph::Stat::Graph $data(Stat)
   } else {
      set graph(Stat) False
   }

   update idletasks
   $data(Canvas) config -cursor left_ptr

#   Graph::Scatter::DrawPercent $GR
}

#----------------------------------------------------------------------------
# Nom      : <Graph::Scatter::Init>
# Creation : Octobre 2003 - J.P. Gauthier - CMC/CMOE
#
# But      : Initialisation des variables du graph.
#
# Parametres :
#   <Frame>  : Identificateur de Page
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc Graph::Scatter::Init { Frame } {
   global   GDefs
   variable Data

   set gr GR[incr Graph::Data(Nb)]

   namespace eval Scatter$gr {
      variable Data
      variable Graph

      set Data(Items)   {}         ;#Liste des items
      set Data(Pos)     {}         ;#Liste des positions
      set Data(Data)    {}         ;#Donnees du graph
      set Data(Lat0)     0         ;#Rectangle de selection
      set Data(Lat1)     0         ;#Rectangle de selection
      set Data(Lon0)     0         ;#Rectangle de selection
      set Data(Lon1)     0         ;#Rectangle de selection

      set Data(Stat)    ""         ;#Informations statistiques

      #----- Constantes relatives au Graph

      set Graph(UnitY)    "[lindex $Graph::Lbl(Unit) $GDefs(Lang)] Y"         ;#Descriptif de l'echelle des valeur en Y
      set Graph(UnitX)    "[lindex $Graph::Lbl(Unit) $GDefs(Lang)] X"         ;#Descriptif de l'echelle des valeur en X
      set Graph(Fit)      LINEAR                                              ;#Type de regression
      set Graph(XScale)   LINEAR                                              ;#Type d'echelle en X
      set Graph(YScale)   LINEAR                                              ;#Type d'echelle en Y
      set Graph(XInter)  ""               ;#Liste des niveau specifie par l'usager
      set Graph(YInter)  ""               ;#Liste des niveau specifie par l'usager
      set Graph(ZXInter)  ""               ;#Liste des Niveaux (Mode Zoom)
      set Graph(ZYInter)  ""               ;#Liste des Niveaux (Mode Zoom)

      #----- Constantes relatives au Graph

      set Graph(Percent)  50        ;#Pourcentage de selection
      set Graph(Stat)     False     ;#Statistiques
      set Graph(Uniform)  1         ;#Echelle equivalente
   }
   return $gr
}

#-------------------------------------------------------------------------------
# Nom      : <Graph::Scatter::Params>
# Creation : Octobre 2002 - J.P. Gauthier - CMC/CMOE -
#
# But      : Creer le frame des widgets des options
#
# Parametres :
#   <Parent> : Frame dans lequel creer les widgets
#   <GR>     : Identificateur du Graph
#
# Remarques :
#
#-------------------------------------------------------------------------------

proc Graph::Scatter::Params { Parent GR } {
   global   GDefs

   upvar #0 Graph::Scatter::Scatter${GR}::Graph graph

   Graph::ParamsPos  $Parent
   Graph::ParamsItem $Parent

   labelframe $Parent.scale -text [lindex $Graph::Lbl(Scale) $GDefs(Lang)]
      frame $Parent.scale.sel -relief sunken -bd 1
         checkbutton $Parent.scale.sel.stat -text [lindex $Graph::Lbl(Stat) $GDefs(Lang)] -variable Graph::Scatter::Scatter${GR}::Graph(Stat) \
            -onvalue True -offvalue False -command "Graph::Scatter::Stats $GR" -indicatoron false -bd 1
         checkbutton $Parent.scale.sel.sel -text [lindex $Graph::Lbl(Select) $GDefs(Lang)] -indicatoron false \
            -command "Graph::Scatter::DrawPercent $GR" -bd 1 -variable Graph::Scatter::Scatter${GR}::Graph(Range)
         pack  $Parent.scale.sel.stat -side top -fill x
      frame $Parent.scale.equiv -relief sunken -bd 1
         checkbutton $Parent.scale.equiv.same -text [lindex $Graph::Lbl(Same) $GDefs(Lang)] -indicatoron false \
            -command "Graph::ParamsScaleUniform Scatter $GR" -bd 1 -variable Graph::Scatter::Scatter${GR}::Graph(Uniform)
         pack $Parent.scale.equiv.same -side top -fill x
      frame $Parent.scale.valx -relief sunken -bd 1
         label $Parent.scale.valx.lbl -text "X"
         checkbutton $Parent.scale.valx.scale -text Log -indicatoron false \
            -command "Graph::Scatter::Graph $GR" -bd 1 \
            -variable Graph::Scatter::Scatter${GR}::Graph(XScale)  -onvalue LOGARITHMIC -offvalue LINEAR
         entry $Parent.scale.valx.list -textvariable Graph::Scatter::Scatter${GR}::Graph(XInter) -bg $GDefs(ColorLight) -relief flat -width 1
         pack $Parent.scale.valx.lbl -side left -fill y
         pack $Parent.scale.valx.list -side left -fill x  -expand true
         pack $Parent.scale.valx.scale -side left -fill y
      frame $Parent.scale.valy -relief sunken -bd 1
         label $Parent.scale.valy.lbl -text "Y"
         checkbutton $Parent.scale.valy.scale -text Log -indicatoron false \
            -command "Graph::Scatter::Graph $GR" -bd 1 \
            -variable Graph::Scatter::Scatter${GR}::Graph(YScale)  -onvalue LOGARITHMIC -offvalue LINEAR
         entry $Parent.scale.valy.list -textvariable Graph::Scatter::Scatter${GR}::Graph(YInter) -bg $GDefs(ColorLight) -relief flat -width 1
         pack $Parent.scale.valy.lbl -side left -fill y
         pack $Parent.scale.valy.list -side left -fill x  -expand true
         pack $Parent.scale.valy.scale -side left -fill y
      pack $Parent.scale.sel $Parent.scale.equiv $Parent.scale.valx -side top -padx 2 -pady 2 -fill x
   pack $Parent.scale -side top -fill x

   labelframe $Parent.fit -text [lindex $Graph::Lbl(Fit) $GDefs(Lang)]
      frame $Parent.fit.sel -relief sunken -bd 1
         checkbutton $Parent.fit.sel.linear -text [lindex $Graph::Lbl(Linear) $GDefs(Lang)] -indicatoron false \
            -command "Graph::Scatter::Graph $GR" -bd 1 -variable Graph::Scatter::Scatter${GR}::Graph(Fit) -onvalue LINEAR -offvalue ""
         pack $Parent.fit.sel.linear -side top -fill x
      pack $Parent.fit.sel -side top -fill x
   pack $Parent.fit -side top -fill x

   Graph::ParamsScaleUniform Scatter $GR

   Bubble::Create $Parent.scale.sel.stat $Graph::Bubble(Stat)
   Bubble::Create $Parent.scale.sel.sel  $Graph::Bubble(Select)
   Bubble::Create $Parent.scale.equiv    $Graph::Bubble(Uniform)
   Bubble::Create $Parent.scale.valx     $Graph::Bubble(ScaleX)
   Bubble::Create $Parent.scale.valy     $Graph::Bubble(ScaleY)
   Bubble::Create $Parent.fit.sel        $Graph::Bubble(Fit)

   bind $Parent.scale.valx.list <Return>     "Graph::Scatter::Graph $GR"
   bind $Parent.scale.valy.list <Return>     "Graph::Scatter::Graph $GR"
}

proc Graph::Scatter::Stats { GR } {

   upvar #0 Graph::Scatter::Scatter${GR}::Graph graph
   upvar #0 Graph::Scatter::Scatter${GR}::Data  data

   if { $graph(Stat) } {
      set data(Stat) [Graph::Stat::Create $data(Frame) 1 1 250 450 True 0]
      Graph::Stat::Graph $data(Stat)
  } else {
      Graph::Destroy $data(Frame) $data(Stat) Stat
      set data(Stat) ""
   }
}
#-------------------------------------------------------------------------------
# Nom      : <Graph::Scatter::ItemAdd>
# Creation : Avril 2005 - J.P. Gauthier - CMC/CMOE -
#
# But      : Creer un nouvel item de graph
#
# Parametres :
#   <GR>     : Identificateur du Graph
#   <Item>   : Identificateur de l'item positionnel a ajouter
#
# Remarques :
#
#-------------------------------------------------------------------------------

proc Graph::Scatter::ItemAdd { GR Item } {

   upvar #0 Graph::Scatter::Scatter${GR}::Data  data

   if { [lsearch -exact $data(Items) $Item]==-1 } {
      vector free   $Item
      vector create $Item
      vector dim    $Item { X Y }

      set id [$data(Canvas) create text -100 -100  -tags "$Page::Data(Tag)$GR CVTEXT GRAPHUPDATE$GR" -text $Item -anchor nw -justify left]

      graphitem create $Item
      graphitem configure $Item -xaxis axisx$GR -yaxis axisy$GR -xdata $Item.X -ydata $Item.Y -orient X -desc $id

      lappend data(Items) $Item
      Graph::Scatter::ItemDefault $GR $Item

      $data(Canvas) itemconfigure $GR -item $data(Items)
   }
}

#-------------------------------------------------------------------------------
# Nom      : <Graph::Scatter::ItemDefault>
# Creation : Avril 2005 - J.P. Gauthier - CMC/CMOE -
#
# But      : Configurer les parametres initiaux d'un items
#
# Parametres :
#   <GR>     : Identificateur du Graph
#   <Item>   : Identificateur de l'item positionnel a ajouter
#
# Remarques :
#
#-------------------------------------------------------------------------------

proc Graph::Scatter::ItemDefault { GR Item } {

   upvar #0 Graph::Scatter::Scatter${GR}::Data  data

   set idx [lsearch -exact $data(Items) $Item]

   set Graph::Item(Outline)     [lindex $Graph::Graph(Colors) [expr $idx%[llength $Graph::Graph(Colors)]]]
   set Graph::Item(FillColor)   #FFFFFF
   set Graph::Item(Tranparency) 100
   set Graph::Item(Width)       1
   set Graph::Item(Size)        3
   set Graph::Item(Value)       False
   set Graph::Item(Dash)        ""
   set Graph::Item(Type)        NONE
   set Graph::Item(Icon)        [lindex $Graph::Graph(Icons) [expr $idx%[llength $Graph::Graph(Icons)]+1]]
   set Graph::Item(Bitmap)      ""
   set Graph::Item(Stipple)     -1
   set Graph::Item(Image)       ""

   Graph::ItemConfigure $GR Scatter $Item
}

#-------------------------------------------------------------------------------
# Nom      : <Graph::Scatter::ItemDel>
# Creation : Avril 2005 - J.P. Gauthier - CMC/CMOE -
#
# But      : Supprimer un item de graph et ses resources
#
# Parametres :
#   <GR>     : Identificateur du Graph
#   <Item>   : Identificateur de l'item positionnel a ajouter
#
# Remarques :
#
#-------------------------------------------------------------------------------

proc Graph::Scatter::ItemDel { GR Item } {

   upvar #0 Graph::Scatter::Scatter${GR}::Data  data

   if { [set idx [lsearch -exact $data(Items) $Item]]!=-1 } {
      set data(Items) [lreplace $data(Items) $idx $idx]
      $data(Canvas) itemconfigure $GR -item $data(Items)
      $data(Canvas) delete [graphitem configure $Item -desc]

      vector free $Item
      graphitem free $Item
   }
}

#-------------------------------------------------------------------------------
# Nom      : <Graph::Scatter::ItemDefine>
# Creation : Avril 2005 - J.P. Gauthier - CMC/CMOE -
#
# But      : Gestion des items. Generer les itmes necessaires selon les donnees
#
# Parametres :
#   <GR>     : Identificateur du Graph
#   <Pos>    : Position
#   <Coords> : Coordonnees
#   <Update> : Mise a jour du graph
#
# Remarques :
#
#-------------------------------------------------------------------------------

proc Graph::Scatter::ItemDefine { GR Pos Coords { Update True } } {

   upvar #0 Graph::Scatter::Scatter${GR}::Data  data

   if { $Pos=="" } {
      return
   }

   if { [info exists Graph::Scatter::Scatter${GR}::Data(Items$Pos)] } {
      foreach item [lrange $data(Items$Pos) [llength $data(Data)] end] {
         Graph::Scatter::ItemDel $GR $item
      }
   }

   if { [lsearch -exact $data(Pos) $Pos]==-1 } {
      lappend data(Pos) $Pos
   }
   set data(Items$Pos) ${Pos}_Item0

   if { ![llength $Coords] } {
      set data(Pos$Pos)  { 0 0 0 0 }
   } else {
      set data(Pos$Pos)   $Coords
   }

   set item ${Pos}_Item0

   Graph::Idle $GR Scatter
   Graph::Scatter::ItemAdd $GR $item
   Graph::Scatter::UpdateItems $data(FrameData) $GR

   if { $Update } {
      Graph::Scatter::ItemData $GR $Pos $item $data(Data)
      Graph::Scatter::Graph $GR
  }
   Graph::UnIdle $GR Scatter
}

#-------------------------------------------------------------------------------
# Nom      : <Graph::Scatter::ItemUnDefine>
# Creation : Avril 2005 - J.P. Gauthier - CMC/CMOE -
#
# But      : Gestion des items. Supression des items relatif a une position
#
# Parametres :
#   <GR>     : Identificateur du Graph
#   <Pos>    : Position
#
# Remarques :
#
#-------------------------------------------------------------------------------

proc Graph::Scatter::ItemUnDefine { GR Pos } {

   upvar #0 Graph::Scatter::Scatter${GR}::Data  data

   if { [set idx [lsearch -exact $data(Pos) $Pos]]!=-1 } {
      foreach item $data(Items$Pos) {
         Graph::Scatter::ItemDel $GR $item
      }

      set data(Pos) [lreplace $data(Pos)  $idx $idx]
      set Graph::Data(Pos) [lindex $data(Pos) end]
   }
}

#-------------------------------------------------------------------------------
# Nom      : <Graph::Scatter::ItemData>
# Creation : Avril 2005 - J.P. Gauthier - CMC/CMOE -
#
# But      : Recuperer les donnees d'un item
#
# Parametres :
#   <GR>     : Identificateur du Graph
#   <Pos>    : Position
#   <Item>   : Identificateur de l'item positionnel a ajouter
#   <Data>   : Donnees a extraire
#
# Remarques :
#
#-------------------------------------------------------------------------------

proc Graph::Scatter::ItemData { GR Pos Item Data } {

   upvar #0 Graph::Scatter::Scatter${GR}::Data  data
   upvar #0 Graph::Scatter::Scatter${GR}::Graph graph

   #----- Pour tout les couples de donnees

   vector set $Item.X {}
   vector set $Item.Y {}

   if { [llength $Data]>=2 } {

      set data0 [lindex $Data 0]
      set data1 [lindex $Data 1]

      if { [fstdfield is $data0] && [fstdfield is $data1] } {
         Graph::Scatter::ItemDataField $Item $data(Pos$Pos) $data0 $data1
      } elseif { [observation is $data0] || [observation is $data1] } {
         Graph::Scatter::ItemDataObs $Item $data(Pos$Pos) $data0 $data1
      }
   }
}

#-------------------------------------------------------------------------------
# Nom      : <Graph::Scatter::Update>
# Creation : Octobre 2002 - J.P. Gauthier - CMC/CMOE
#
# But      : Effectuer le "Refresh" des items sur la projection.
#
# Parametres :
#   <Frame > : Identificateur de page
#   <GR>     : Liste des graphs a mettre a jour
#
# Remarques :
#    - Cette fonctions est appele par le package Page au besoin.
#
#-------------------------------------------------------------------------------

proc Graph::Scatter::Update { Frame { GR {} } } {
   variable Data

   if { ![llength $GR] } {
      set GR [Page::Registered All Graph::Scatter]
   }

   foreach gr $GR {
      upvar #0 Graph::Scatter::Scatter${gr}::Data data

      if { $data(FrameData)==$Frame } {

         catch {
            $data(FrameData).page.canvas configure -cursor watch
            $data(Canvas) configure -cursor watch
            update idletasks
         }

         #----- Recuperer les donnees

         if { [Page::Registered All Viewport $data(VP)]!=-1 } {
            Graph::Scatter::Data $gr [Viewport::Assigned $Viewport::Data(Frame$data(VP)) $data(VP) { fstdfield observation }]
         }

         #----- Update des items

         foreach pos $data(Pos) {
            Graph::Scatter::ItemDefine $gr $pos $data(Pos$pos)
         }
         Graph::PosSet $gr Scatter

         catch {
            $data(Canvas) configure -cursor left_ptr
            $data(FrameData).page.canvas configure -cursor left_ptr
         }
      }
   }
}

#-------------------------------------------------------------------------------
# Nom      : <Graph::Scatter::Data>
# Creation : Mai 2000 - J.P. Gauthier - CMC/CMOE
#
# But      : Recupere les champs correspondants disponibles.
#
# Parametres :
#   <GR>     : Indentificateur du Graph
#   <Data>   : Liste des donnees disponibles
#   <Files>  : Liste des fichiers a scanner
#
# Retour    :
#
# Remarque :
#
#-------------------------------------------------------------------------------

proc Graph::Scatter::Data { GR { Data { } } { Files { } } } {
   global   GDefs

   upvar #0 Graph::Scatter::Scatter${GR}::Data  data
   upvar #0 Graph::Scatter::Scatter${GR}::Graph graph

   SPI::Progress 0

   set data(Data) $Data

   SPI::Progress 0
}

#-------------------------------------------------------------------------------
# Nom      : <Graph::Scatter::UpdateItems>
# Creation : Octobre 2002 - J.P. Gauthier - CMC/CMOE
#
# But      : Effectuer le "Refresh" des items sur la projection.
#
# Parametres :
#   <Frame>  : Identificateur de Page
#   <GR>     : Liste des graphs a mettre a jour
#
# Remarques :
#    - Cette fonctions est appele par le package Page au besoin.
#
#-------------------------------------------------------------------------------

proc Graph::Scatter::UpdateItems { Frame { GR { } } } {
   global   GDefs
   variable Lbl
   variable Data

   if { ![llength $GR] } {
      set GR [Page::Registered All Graph::Scatter]
   }

   foreach gr $GR {

      upvar #0 Graph::Scatter::Scatter${gr}::Data data

      if { $data(VP)!="" && $data(FrameData)==$Frame } {
         $Frame.page.canvas delete GRAPHSCATTER$gr
         foreach pos $data(Pos) {
            if { [llength $data(Items$pos)] } {
               set id [graphitem configure [lindex $data(Items$pos) 0] -desc]
               set desc [lindex [$data(Canvas) itemconfigure $id -text] end]
               Graph::ItemPos $Frame $data(VP) $data(Pos$pos) "[lindex $Lbl(Title) $GDefs(Lang)]\n$desc" GRAPHSCATTER$gr RECTANGLE
            }
         }
      }
   }
}

#-------------------------------------------------------------------------------
# Nom      : <Graph::Scatter::DataField>
# Creation : Mars 2003 - J.P. Gauthier - CMC/CMOE
#
# But      : Recupere les valeurs aux points de grilles specifees parmi tous
#            les champs correspondants disponibles.
#
# Parametres :
#   <GR>     : Indentificateur du Graph
#   <Data0>  :
#   <Data1>  :
#
# Retour    :
#
# Remarque :
#
#-------------------------------------------------------------------------------

proc Graph::Scatter::ItemDataField { Item Coords Data0 Data1 } {

   eval set grids \[fstdfield stats $Data0 -within $Coords\]
   foreach grid $grids {
      set i [lindex $grid 0]
      set j [lindex $grid 1]
      set coo [fstdfield stats $Data0 -gridpoint $i $j]
      set lat [lindex $coo 0]
      set lon [lindex $coo 1]

      set val0 [lindex [fstdfield stats $Data0 -gridvalue $i $j] 0]
      if { [fstdfield define $Data0 -GRIDID]==[fstdfield define $Data1 -GRIDID] } {
         set val1 [lindex [fstdfield stats $Data1 -gridvalue $i $j] 0]
      } else {
         set val1 [lindex [fstdfield stats $Data1 -coordvalue $lat $lon] 0]
      }

      if { $val1!="-" } {
         vector append $Item.X $val0
         vector append $Item.Y $val1
      }
   }
}

#-------------------------------------------------------------------------------
# Nom      : <Graph::Scatter::DataObs>
# Creation : Mars 2003 - J.P. Gauthier - CMC/CMOE
#
# But      : Recupere les valeurs aux localisations specifees parmi tous
#            les champs correspondants disponibles.
#
# Parametres :
#   <GR>     : Indentificateur du Graph
#   <Data0>  :
#   <Data1>  :
#
# Retour    :
#
# Remarque :
#
#-------------------------------------------------------------------------------

proc Graph::Scatter::ItemDataObs { Item Coords Data0 Data1 } {

   #----- Dans le cas d'un champs, on en extrait les valeurs aux position de l'observation

   if { [fstdfield is $Data1] } {
      observation copy $Data1 $Data0
      observation extract $Data1 $Data1
      observation configure $Data1 -min [fstdfield configure $Data1 -min] -max [fstdfield configure $Data1 -max]
   }

   if { [fstdfield is $Data0] } {
      observation copy $Data0 $Data1
      observation extract $Data0 $Data0
      observation configure $Data0 -min [fstdfield configure $Data0 -min] -max [fstdfield configure $Data0 -max]
   }

   #----- Selection des limites

   set in   1

   eval set idxs \[observation stats $Data0 -within $Coords\]
   foreach idx $idxs {
      set val0 [observation define $Data0 -DATA $idx]
      set val1 [observation define $Data1 -DATA $idx]

      if { $val0!="-" && $val1!="-" } {
         vector append $Item.X $val0
         vector append $Item.Y $val1
      }
   }

   #----- Dans le cas d'un champs, suprimer l'observation temporaire

   if { [fstdfield is $Data1] } {
      observation free $Data1
   }
   if { [fstdfield is $Data0] } {
      observation free $Data0
   }
}