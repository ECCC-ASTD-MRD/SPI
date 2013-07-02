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

proc Graph::Scatter::Create { Frame X0 Y0 Width Height Active Full { Link True } } {
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
   set Graph::Data(Link$gr)     $Link      ;#Liaison des donnees a l'interface
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
   graphaxis configure axisx$gr -font $Graph::Font(Axis) -color $Graph::Color(Axis) -gridcolor $Graph::Grid(XColor) \
      -dash $Graph::Grid(XDash) -position LL -width 1 -unit $id

   set id [$data(Canvas) create text -100 -100  -tags "$tag CVTEXT GRAPHUPDATE$gr" -text $graph(UnitY) \
      -font $Graph::Font(Axis) -fill $Graph::Color(Axis) -anchor nw -justify center]
   graphaxis configure axisy$gr -font $Graph::Font(Axis) -color $Graph::Color(Axis) -gridcolor $Graph::Grid(YColor) \
      -dash $Graph::Grid(YDash) -position LL -width 1 -unit $id

   if { $Viewport::Data(VP)!="" } {
      set data(VP)        $Viewport::Data(VP)
      set data(FrameData) $Viewport::Data(Frame$data(VP))
      Graph::Scatter::Update $data(FrameData) $gr
   } else {
      set data(VP)        ""
      set data(FrameData) ""
   }

   Graph::Activate $Frame $gr Scatter
   Graph::Mode $gr Scatter True
   Graph::PosAdd $gr Scatter

   #----- Creer les fonction du mode actif

   if { $Active } {
      Page::ActiveWrapper Graph $Frame $gr $X0 $Y0 [expr $Width+$X0] [expr $Height+$Y0] Scatter
   } elseif { $Full } {
      Page::ActiveFull Graph $Frame $gr $Full
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

   if { $data(Stat)!="" && [llength [$data(Frame).page.canvas find withtag GRAPHSTAT$data(Stat)]] } {
      Graph::Destroy $data(Frame) $data(Stat) Stat
   }
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
      set xmin [lindex $graph(ZXInter) 0]
      set xmax [lindex $graph(ZXInter) 1]
      set mod False
   } else {
      set xmin $data(XMin)
      set xmax $data(XMax)
   }
   if { [llength $graph(ZYInter)] } {
      set ymin [lindex $graph(ZYInter) 0]
      set ymax [lindex $graph(ZYInter) 1]
      set mod False
   } else {
      set ymin $data(YMin)
      set ymax $data(YMax)
   }

   if { $l } {
      set min $xmin
      set max $xmax
   } else {
      set min [expr $xmin<$ymin?$xmin:$ymin]
      set max [expr $xmax>$ymax?$xmax:$ymax]
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
      graphaxis configure axisx$GR -type $graph(XScale) -modulo $mod -min $min -max $max -intervals $xinter -increment $xincr -angle $graph(XAngle) \
         -font $Graph::Font(Axis) -gridcolor $Graph::Grid(XColor)  -dash $Graph::Grid(XDash) -gridwidth $Graph::Grid(XWidth) -color $Graph::Color(Axis) \
      -format $graph(XFormat) -decimal $graph(XDecimals)
      graphaxis configure axisy$GR -type $graph(XScale) -modulo $mod -min $min -max $max -intervals $xinter -increment $xincr -angle $graph(XAngle) \
         -font $Graph::Font(Axis) -gridcolor $Graph::Grid(XColor)  -dash $Graph::Grid(XDash) -gridwidth $Graph::Grid(XWidth) -color $Graph::Color(Axis) \
      -format $graph(XFormat) -decimal $graph(XDecimals)
   } else {
      graphaxis configure axisx$GR -type $graph(XScale) -modulo $mod -min $xmin -max $xmax -intervals $xinter -increment $xincr -angle $graph(XAngle) \
         -font $Graph::Font(Axis) -gridcolor $Graph::Grid(XColor)  -dash $Graph::Grid(XDash) -gridwidth $Graph::Grid(XWidth) -color $Graph::Color(Axis) \
      -format $graph(XFormat) -decimal $graph(XDecimals)
      graphaxis configure axisy$GR -type $graph(YScale) -modulo $mod -min $ymin -max $ymax -intervals $yinter -increment $yincr -angle $graph(YAngle) \
         -font $Graph::Font(Axis) -gridcolor $Graph::Grid(YColor)  -dash $Graph::Grid(YDash) -gridwidth $Graph::Grid(YWidth) -color $Graph::Color(Axis) \
      -format $graph(YFormat) -decimal $graph(YDecimals)
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
      set Data(Coords)  {}         ;#Liste des coordonnees de coupe
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
      set Graph(XFormat)   NONE
      set Graph(YFormat)   NONE
      set Graph(XDecimals) 0
      set Graph(YDecimals) 0
      set Graph(XAngle)    0
      set Graph(YAngle)    0                                                   ;

      #----- Constantes relatives au Graph

      set Graph(Percent)  50        ;#Pourcentage de selection
      set Graph(Stat)     False     ;#Statistiques
      set Graph(Uniform)  True         ;#Echelle equivalente
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

   labelframe $Parent.par -text [lindex $Graph::Lbl(Params) $GDefs(Lang)]
      frame $Parent.par.sel -relief sunken -bd 1
         checkbutton $Parent.par.sel.stat -text [lindex $Graph::Lbl(Stat) $GDefs(Lang)] -variable Graph::Scatter::Scatter${GR}::Graph(Stat) \
            -onvalue True -offvalue False -command "Graph::Scatter::Stats $GR" -indicatoron false -bd 1
         checkbutton $Parent.par.sel.sel -text [lindex $Graph::Lbl(Select) $GDefs(Lang)] -indicatoron false \
            -command "Graph::Scatter::DrawPercent $GR" -bd 1 -variable Graph::Scatter::Scatter${GR}::Graph(Range)
         checkbutton $Parent.par.sel.same -text [lindex $Graph::Lbl(Same) $GDefs(Lang)] -indicatoron false -onvalue True -offvalue False \
            -command "Graph::ParamsAxisUniform Scatter $GR" -bd 1 -variable Graph::Scatter::Scatter${GR}::Graph(Uniform)
         checkbutton $Parent.par.sel.fitlinear -text [lindex $Graph::Lbl(FitLinear) $GDefs(Lang)] -indicatoron false \
            -command "Graph::Scatter::Graph $GR" -bd 1 -variable Graph::Scatter::Scatter${GR}::Graph(Fit) -onvalue LINEAR -offvalue ""
         pack $Parent.par.sel.stat $Parent.par.sel.same $Parent.par.sel.fitlinear -side top -fill x
      pack $Parent.par.sel -side top -fill x
   pack $Parent.par -side top -fill x -padx 5 -pady 5

   Graph::ParamsAxis $Parent $GR Scatter X
   Graph::ParamsAxis $Parent $GR Scatter Y

   Graph::ParamsAxisUniform Scatter $GR
   Graph::ModeSelect BOX BOX NIL

   Bubble::Create $Parent.par.sel.stat       $Graph::Bubble(Stat)
   Bubble::Create $Parent.par.sel.sel        $Graph::Bubble(Select)
   Bubble::Create $Parent.par.sel.same       $Graph::Bubble(Uniform)
   Bubble::Create $Parent.par.sel.fitlinear  $Graph::Bubble(Fit)
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
   set data(Coords$Pos) $data(Pos$Pos)

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
         $Frame.page.canvas delete GRAPHSELECT$gr
         foreach pos $data(Pos) {
            if { [llength $data(Items$pos)] } {
               set id [graphitem configure [lindex $data(Items$pos) 0] -desc]
               set desc [lindex [$data(Canvas) itemconfigure $id -text] end]
               Graph::ItemPos $Frame $data(VP) $data(Pos$pos) "[lindex $Lbl(Title) $GDefs(Lang)]\n$desc" GRAPHSELECT$gr BOX
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
   global GDefs

   set ijs [fstdfield stats $Data0 -within $Coords]
   set nij [llength $ijs]
   set n 0
   set df  [expr 100.0/($nij-1)]

   foreach ij $ijs {
      set i [lindex $ij 0]
      set j [lindex $ij 1]
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
         vector append $Item "$val0 $val1"
      }
      incr n
      SPI::Progress +$df "[lindex $Graph::Msg(Extracting) $GDefs(Lang)] ($n/$nij)"
   }
   SPI::Progress 0
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
   global GDefs

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

   set idxs [observation stats $Data0 -within $Coords]
   set nidx [llength $idxs]
   set n 0
   set df  [expr 100.0/([llength $idxs]-1)]

   foreach idx $idxs {
      set val0 [observation define $Data0 -DATA $idx]
      set val1 [observation define $Data1 -DATA $idx]

      if { $val0!="-" && $val1!="-" } {
         vector append $Item.X $val0
         vector append $Item.Y $val1
      }
      incr n
      SPI::Progress +$df "[lindex $Graph::Msg(Extracting) $GDefs(Lang)] ($n/$nidx)"
   }

   #----- Dans le cas d'un champs, suprimer l'observation temporaire

   if { [fstdfield is $Data1] } {
      observation free $Data1
   }
   if { [fstdfield is $Data0] } {
      observation free $Data0
   }
   SPI::Progress 0
}

proc Graph::Scatter::ItemDataMetObs { Item Coords Data0 Data1 } {
   global GDefs

   if { [fstdfield is $Data1] } {
      set field $Data1
      set obs   $Data0
   }

   if { [fstdfield is $Data0] } {
      set field $Data0
      set obs   $Data1
   }

   #----- Selection des limites

   set date [fstdstamp toseconds [fstdfield define $field -DATEV]]

   set idxs  [metobs define $obs -NB]
   set nidx [llength $idxs]
   set n 0
   set df  [expr 100.0/([llength $idxs]-1)]

   foreach id [metobs define $obs -ID] coords [metobs define $obs -COORD] {
      set val0  [metobs define $obs -ELEMENT $id $data(Elem$Pos) $date]
      set val1 [fstdfield stats $field -coordvalue [lindex $coords 0] [lindex $coords 1]]

      if { $val0!="-" && $val1!="-" } {
         vector append $Item.X $val0
         vector append $Item.Y $val1
      }
      incr n
      SPI::Progress +$df "[lindex $Graph::Msg(Extracting) $GDefs(Lang)] ($n/$nidx)"
   }

   SPI::Progress 0
}