#===============================================================================
# Environnement Canada
# Centre Meteorologique Canadian
# 2100 Trans-Canadienne
# Dorval, Quebec
#
# Projet   : Librairie d'objects visuel interactifs
# Fichier  : Graph_Profile.tcl
# Creation : Octobre 2003 - J.P. Gauthier - CMC/CMOE
#
# ription: Ce package s'occupe de l'affichage et de la manipulation de graphs
#              de profil verticaux
#
# Fonctions:
#
#    Graph::Profile::Create       { Frame X0 Y0 Width Height Active Full }
#    Graph::Profile::Coord        { Frame GR X Y }
#    Graph::Profile::Clean        { GR }
#    Graph::Profile::DrawInit     { Frame VP }
#    Graph::Profile::Draw         { Frame VP }
#    Graph::Profile::DrawDone     { Frame VP }
#    Graph::Profile::MoveInit     { Frame VP }
#    Graph::Profile::Move         { Frame VP }
#    Graph::Profile::MoveDone     { Frame VP }
#    Graph::Profile::Graph        { GR }
#    Graph::Profile::Init         { Frame }
#    Graph::Profile::Params       { Parent GR }
#    Graph::Profile::ItemAdd      { GR Item }
#    Graph::Profile::ItemDefault  { GR Item }
#    Graph::Profile::ItemDel      { GR Item }
#    Graph::Profile::ItemDefine   { GR Pos Coords { Update True } }
#    Graph::Profile::ItemUnDefine { GR Pos }
#    Graph::Profile::ItemData     { GR Item Data Coords  }
#    Graph::Profile::Update       { Frame { GR {} } }
#    Graph::Profile::UpdateItems  { Frame { GR  { } } }
#    Graph::Profile::Data         { GR Data }
#
#===============================================================================

namespace eval Graph::Profile { } {
   variable Lbl
   variable Msg

   set Lbl(Title)     { "Profil vertical" "Vertical profile" }
   set Lbl(Grid)      { "Grille" "Grid" }
   set Lbl(Pres)      { "Pression" "Pressure" }

   set Msg(Reading)   { "Lecture des données" "Reading data" }
}

#----------------------------------------------------------------------------
# Nom      : <Graph::Profile::Create>
# Creation : Octobre 2003 - J.P. Gauthier - CMC/CMOE
#
# But      : Crrer l'object graph et ses structures
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

proc Graph::Profile::Create { Frame X0 Y0 Width Height Active Full } {
   global GDefs
   variable Data
   variable Lbl

   set gr [Graph::Profile::Init $Frame]
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
   set Graph::Data(Type$gr)     Profile    ;#Type de graph

   upvar #0 Graph::Profile::Profile${gr}::Data  data
   upvar #0 Graph::Profile::Profile${gr}::Graph graph

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

   set id [$data(Canvas) create text -100 -1000  -tags "$tag CVTEXT GRAPHUPDATE$gr" -text $graph(UnitY) \
      -font $Graph::Font(Axis) -fill $Graph::Color(Axis) -anchor nw -justify center]
   graphaxis configure axisy$gr -font $Graph::Font(Axis) -color $Graph::Color(Axis) -gridcolor $Graph::Grid(Color) \
      -dash $Graph::Grid(Dash) -position LL -width 1 -unit $id

   if { $Viewport::Data(VP)!="" } {
      set data(VP)        $Viewport::Data(VP)
      set data(FrameData) $Viewport::Data(Frame$data(VP))
      Graph::Profile::Update $data(FrameData) $gr
   } else {
      set data(VP)        ""
      set data(FrameData) ""
   }

   Graph::Activate $Frame $gr Profile
   Graph::Mode Profile $gr True
   Graph::PosAdd $gr Profile

   #----- Creer les fonction du mode actif

   if { $Active } {
      Page::ActiveWrapper Graph $Frame $gr $X0 $Y0 [expr $Width+$X0] [expr $Height+$Y0] Profile
   }
   Page::Register $Frame Graph::Profile $gr

   return $gr
}

#-------------------------------------------------------------------------------
# Nom      : <Graph::Profile::Coord>
# Creation : Octobre 2002 - J.P. Gauthier - CMC/CMOE
#
# But      : Evalue les valeurs de concentrations et de date a la position du curseur
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

proc Graph::Profile::Coord { Frame GR X Y } {
   global   GDefs

   upvar #0 Graph::Profile::Profile${GR}::Data  data

   set Page::Data(Coord) ""
   set Page::Data(Value) ""

   if  { [llength [set items [lindex [$data(Canvas) itemconfigure $GR -item] end]]] } {
      set coords [$GR -unproject $X $Y False [lindex $items 0]]

      if { [llength $coords]==2 } {
         set Page::Data(Coord) "[lindex $Graph::Lbl(Level) $GDefs(Lang)]: [format "%1.3e" [lindex $coords 1]]"
         set Page::Data(Value) "[lindex $Graph::Lbl(Val) $GDefs(Lang)]: [format "%1.3e" [lindex $coords 0]]"
      }
   }
}

#-------------------------------------------------------------------------------
# Nom      : <Graph::Profile::Clean>
# Creation : Octobre 2002 - J.P. Gauthier - CMC/CMOE -
#
# But      : Supprimer les donnees associees aux coordonees.
#
# Parametres :
#   <GR>     : Identificateur du Graph
#
#-------------------------------------------------------------------------------

proc Graph::Profile::Clean { GR } {

   fstdfield free GRAPHPROFILE
}

#----------------------------------------------------------------------------
# Nom      : <Graph::Profile::Draw...>
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

proc Graph::Profile::DrawInit { Frame VP } {
   variable Data
   variable Lbl

   upvar #0 Graph::Profile::Profile${Graph::Data(Graph)}::Data  data

   if { $VP!=$data(VP) } {
      set data(VP)        $VP
      set data(FrameData) $Frame
      Graph::Profile::Update $Frame $Graph::Data(Graph)
   } else {
      Graph::Profile::ItemDefine $Graph::Data(Graph) $Graph::Data(Pos) [list $Viewport::Map(LatCursor) $Viewport::Map(LonCursor)]
   }
}

proc Graph::Profile::Draw { Frame VP } {
   Graph::Profile::DrawInit $Frame $VP
}

proc Graph::Profile::DrawDone { Frame VP } { }
proc Graph::Profile::MoveInit { Frame VP } { }
proc Graph::Profile::Move     { Frame VP } { }
proc Graph::Profile::MoveDone { Frame VP } { }

#-------------------------------------------------------------------------------
# Nom      : <Graph::Profile::Graph>
# Creation : Fevrier 1999 - J.P. Gauthier - CMC/CMOE
#
# But      : Affiche le graphique.
#
# Parametres :
#   <GR>     : Indentificateur du Graph
#
# Retour    :
#
# Remarque :
#
#-------------------------------------------------------------------------------

proc Graph::Profile::Graph { GR } {
   variable Data

   upvar #0 Graph::Profile::Profile${GR}::Data  data
   upvar #0 Graph::Profile::Profile${GR}::Graph graph

   if { ![llength $data(Items)] } {
      return
   }

   #----- Recalculer les valeurs
   set data(XMin)  1e200
   set data(XMax) -1e200
   set data(YMin)  1e200
   set data(YMax) -1e200
   set data(Levels) {}
   set yincr  0
   set xincr  0
   set mod    True

   #----- Extraire les limites des valeurs
   foreach item $data(Items) {
      set min [vector stats $item.X -min]
      set max [vector stats $item.X -max]
      set data(XMin) [expr $min<$data(XMin)?$min:$data(XMin)]
      set data(XMax) [expr $max>$data(XMax)?$max:$data(XMax)]
   }

   if { [vector length $item.Y] && [vector get $item.Y 0]<[vector get $item.Y end] } {
      set data(Order) -increasing
   } else {
      set data(Order) -decreasing
   }

   set data(Levels) [vector get $item.Y]

   if { [llength $data(Levels)] } {
      eval set data(Levels) \[lsort -unique -real $data(Order) \$data(Levels)\]
   }

   #----- Verifier la selection de l'usager
   if { ![set l [llength $graph(YInter)]] } {
      set data(YMin) [lindex $data(Levels) 0]
      set data(YMax) [lindex $data(Levels) end]

      if { $graph(YScale)=="LOGARITHMIC" } {
         set yinter ""
         set yincr 1
      } else {
         set yinter $data(Levels)
      }
   } else {
      set data(YMin) [lindex $graph(YInter) 0]
      set data(YMax) [lindex $graph(YInter) end]
      if { $l==2 } {
         set yinter {}
      } else {
         set yinter $graph(YInter)
      }
   }

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

   #----- Verifier le zoom
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

   set id [graphaxis configure axisx$GR -unit]
   if { $Graph::Data(Update) } {
      $data(Canvas) itemconfigure $id -text $graph(UnitX)
   }
   $data(Canvas) itemconfigure $id -font $Graph::Font(Axis) -fill $Graph::Color(Axis)
   graphaxis configure axisx$GR -type $graph(XScale) -modulo $mod -min $data(XMin) -max $data(XMax) -intervals $xinter  -increment $xincr -angle $Graph::Font(Angle) \
      -font $Graph::Font(Axis) -gridcolor $Graph::Grid(Color) -dash $Graph::Grid(Dash) -gridwidth $Graph::Grid(Width) -color $Graph::Color(Axis)

   set id [graphaxis configure axisy$GR -unit]
   if { $Graph::Data(Update) } {
      $data(Canvas) itemconfigure $id -text $graph(UnitY)
   }
   $data(Canvas) itemconfigure $id -font $Graph::Font(Axis) -fill $Graph::Color(Axis)

   graphaxis configure axisy$GR -type $graph(YScale) -modulo $mod -min $data(YMin) -max $data(YMax) -intervals $yinter -increment $yincr -angle $Graph::Font(Angle) \
      -font $Graph::Font(Axis) -gridcolor $Graph::Grid(Color) -dash $Graph::Grid(Dash) -gridwidth $Graph::Grid(Width) -color $Graph::Color(Axis)

   set id [lindex [$data(Canvas) itemconfigure $GR -title] end]
   $data(Canvas) itemconfigure $id -font $Graph::Font(Graph) -fill $Graph::Color(FG)
   $data(Canvas) itemconfigure $GR -item $data(Items) \
      -fg $Graph::Color(FG) -bg $Graph::Color(BG) -fill $Graph::Color(Fill) -font $Graph::Font(Graph)
}

#----------------------------------------------------------------------------
# Nom      : <Graph::Profile::Init>
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

proc Graph::Profile::Init { Frame } {
   global   GDefs
   variable Data

   set gr GR[incr Graph::Data(Nb)]

   namespace eval Profile$gr {
      variable Data
      variable Graph

      set Data(Items)           {}        ;#Liste des items
      set Data(Pos)             {}        ;#Liste des positions
      set Data(Data)            {}        ;#Liste des champs selectionnees
      set Data(ObsIds)          {}        ;#Liste des positions observations
      set Data(ObsToken)        ""        ;#Token de recherche

      #----- Constantes relatives au Graph

      set Graph(UnitY)    "[lindex $Graph::Lbl(Unit) $GDefs(Lang)] Y"         ;#Descriptif de l'echelle des valeur en Y
      set Graph(UnitX)    "[lindex $Graph::Lbl(Unit) $GDefs(Lang)] X"         ;#Descriptif de l'echelle des valeur en X
      set Graph(XScale)   LINEAR                                              ;#Type d'echelle en X
      set Graph(YScale)   LINEAR                                              ;#Type d'echelle en Y
      set Graph(XInter)   ""                                                  ;#Liste des niveau specifie par l'usager
      set Graph(YInter)   ""                                                  ;#Liste des niveau specifie par l'usager
      set Graph(ZXInter)  ""                                                  ;#Liste des Niveaux (Mode Zoom)
      set Graph(ZYInter)  ""                                                  ;#Liste des Niveaux (Mode Zoom)
      set Graph(ZType)    GRID                                                ;#Type de niveaux (GRID,PRESSSURE)
   }
   return $gr
}

#-------------------------------------------------------------------------------
# Nom      : <Graph::Profile::Params>
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

proc Graph::Profile::Params { Parent GR } {
   global   GDefs
   variable Lbl

   Graph::ParamsPos  $Parent
   Graph::ParamsItem $Parent

   labelframe $Parent.scale -text [lindex $Graph::Lbl(Scale) $GDefs(Lang)]
      frame $Parent.scale.valx -relief sunken -bd 1
         label $Parent.scale.valx.lbl -text "X"
         checkbutton $Parent.scale.valx.scale -text Log -indicatoron false \
            -command "Graph::Profile::Graph $GR" -bd 1 \
            -variable Graph::Profile::Profile${GR}::Graph(XScale) -onvalue LOGARITHMIC -offvalue LINEAR
         entry $Parent.scale.valx.list -textvariable Graph::Profile::Profile${GR}::Graph(XInter) -bg $GDefs(ColorLight) -relief flat -width 1
         pack $Parent.scale.valx.lbl -side left -fill y
         pack $Parent.scale.valx.list -side left -fill x -expand true
         pack $Parent.scale.valx.scale -side left -fill y
      frame $Parent.scale.valy -relief sunken -bd 1
         label $Parent.scale.valy.lbl -text "Y"
         checkbutton $Parent.scale.valy.scale -text Log -indicatoron false \
            -command "Graph::Profile::Graph $GR" -bd 1 \
            -variable Graph::Profile::Profile${GR}::Graph(YScale)  -onvalue LOGARITHMIC -offvalue LINEAR
         entry $Parent.scale.valy.list -textvariable Graph::Profile::Profile${GR}::Graph(YInter) -bg $GDefs(ColorLight) -relief flat -width 1
         pack $Parent.scale.valy.lbl -side left -fill y
         pack $Parent.scale.valy.list -side left -fill x  -expand true
         pack $Parent.scale.valy.scale -side left -fill y
      pack $Parent.scale.valx $Parent.scale.valy -side top -padx 2 -pady 2 -fill x
      frame $Parent.scale.type -relief sunken -bd 1
         radiobutton $Parent.scale.type.grid -text [lindex $Lbl(Grid) $GDefs(Lang)] -indicatoron false \
            -command "Graph::Profile::Update \$Graph::Profile::Profile${GR}::Data(FrameData) $GR" -bd 1 -variable Graph::Profile::Profile${GR}::Graph(ZType) -value GRID
         radiobutton $Parent.scale.type.pres -text [lindex $Lbl(Pres) $GDefs(Lang)] -indicatoron false \
            -command "Graph::Profile::Update \$Graph::Profile::Profile${GR}::Data(FrameData) $GR" -bd 1 -variable Graph::Profile::Profile${GR}::Graph(ZType) -value PRESSURE
         pack $Parent.scale.type.grid $Parent.scale.type.pres -side left -fill x -expand True
      pack $Parent.scale.type -side top -padx 2 -fill x
   pack $Parent.scale -side top -fill x -padx 5 -pady 5

   Graph::ParamsObs $Parent Profile $GR

   Bubble::Create $Parent.scale.valx $Graph::Bubble(ScaleX)
   Bubble::Create $Parent.scale.valy $Graph::Bubble(ScaleY)

   bind $Parent.scale.valx.list <Return> "Graph::Profile::Graph $GR"
   bind $Parent.scale.valy.list <Return> "Graph::Profile::Graph $GR"
}

#-------------------------------------------------------------------------------
# Nom      : <Graph::Profile::ItemAdd>
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

proc Graph::Profile::ItemAdd { GR Item } {

   upvar #0 Graph::Profile::Profile${GR}::Data  data

   if { [lsearch -exact $data(Items) $Item]==-1 } {
      vector free   $Item
      vector create $Item
      vector dim    $Item { X Y }

      set id [$data(Canvas) create text -100 -100 -tags "$Page::Data(Tag)$GR CVTEXT GRAPHUPDATE$GR" -text $Item -anchor nw -justify left]

      graphitem create $Item
      graphitem configure $Item -xaxis axisx$GR -yaxis axisy$GR -xdata $Item.X -ydata $Item.Y -orient Y -desc $id

      lappend data(Items) $Item
      Graph::Profile::ItemDefault $GR $Item
       $data(Canvas) itemconfigure $GR -item $data(Items)
  }
}

#-------------------------------------------------------------------------------
# Nom      : <Graph::Profile::ItemDefault>
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

proc Graph::Profile::ItemDefault { GR Item } {

   upvar #0 Graph::Profile::Profile${GR}::Data  data

   set idx [lsearch -exact $data(Items) $Item]

   set Graph::Item(Outline)     [lindex $Graph::Graph(Colors) [expr $idx%[llength $Graph::Graph(Colors)]]]
   set Graph::Item(FillColor)   #FFFFFF
   set Graph::Item(Tranparency) 100
   set Graph::Item(Width)       1
   set Graph::Item(Size)        0
   set Graph::Item(Value)       False
   set Graph::Item(Dash)        ""
   set Graph::Item(Type)        LINE
   set Graph::Item(Icon)        [lindex $Graph::Graph(Icons) [expr $idx%[llength $Graph::Graph(Icons)]]]
   set Graph::Item(Bitmap)      ""
   set Graph::Item(Stipple)     -1
   set Graph::Item(Image)       ""

   Graph::ItemConfigure $GR Profile $Item
}

#-------------------------------------------------------------------------------
# Nom      : <Graph::Profile::ItemDel>
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

proc Graph::Profile::ItemDel { GR Item } {

   upvar #0 Graph::Profile::Profile${GR}::Data  data

   if { [set idx [lsearch -exact $data(Items) $Item]]!=-1 } {
      set data(Items) [lreplace $data(Items) $idx $idx]
      $data(Canvas) delete [graphitem configure $Item -desc]
      $data(Canvas) itemconfigure $GR -item $data(Items)

      vector free $Item
      graphitem free $Item
   }
}

#-------------------------------------------------------------------------------
# Nom      : <Graph::Profile::ItemDefine>
# Creation : Avril 2005 - J.P. Gauthier - CMC/CMOE -
#
# But      : Gestion des items. Generer les items necessaires selon les donnees
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

proc Graph::Profile::ItemDefine { GR Pos Coords { Update True } } {

   upvar #0 Graph::Profile::Profile${GR}::Data  data

   if { $Pos=="" } {
      return
   }

   if { [info exists Graph::Profile::Profile${GR}::Data(Items$Pos)] } {
      foreach item [lrange $data(Items$Pos) [llength $data(Data)] end] {
         Graph::Profile::ItemDel $GR $item
      }
   }

   if { [lsearch -exact $data(Pos) $Pos]==-1 } {
      lappend data(Pos) $Pos
   }
   set data(Items$Pos) {}
   set data(Pos$Pos)   $Coords
   set i -1

   Graph::Idle $GR Profile

   foreach field $data(Data) {
      set item ${Pos}_Item[incr i]
      lappend data(Items$Pos) $item

      Graph::Profile::ItemAdd $GR $item
      Graph::Profile::ItemData $GR $Pos $item $field
   }

   Graph::Profile::UpdateItems $data(FrameData) $GR
   Graph::Profile::Graph $GR
   Graph::UnIdle $GR Profile
}

#-------------------------------------------------------------------------------
# Nom      : <Graph::Profile::ItemUnDefine>
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

proc Graph::Profile::ItemUnDefine { GR Pos } {

   upvar #0 Graph::Profile::Profile${GR}::Data  data

   if { [set idx [lsearch -exact $data(Pos) $Pos]]!=-1 } {
      foreach item $data(Items$Pos) {
         Graph::Profile::ItemDel $GR $item
      }

      set data(Pos) [lreplace $data(Pos)  $idx $idx]
      set Graph::Data(Pos) [lindex $data(Pos) end]
   }
}

#-------------------------------------------------------------------------------
# Nom      : <Graph::Profile::ItemData>
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

proc Graph::Profile::ItemData { GR Pos Item Data  } {

   upvar #0 Graph::Profile::Profile${GR}::Data  data
   upvar #0 Graph::Profile::Profile${GR}::Graph graph

   if { [graphitem is $Item]  && [llength $data(Pos$Pos)]} {

      if { [fstdfield is $Data] } {
         fstdfield vertical GRAPHPROFILE $Data $data(Pos$Pos)

         #----- Configure info label if allowed
         if { $Graph::Data(Update) } {
            set obj [graphitem configure $Item -desc]
            set type [fstdfield stats GRAPHPROFILE -leveltype]
            switch $type {
               "HYBRID" { $data(Canvas) itemconfigure $obj -text "[fstdfield define GRAPHPROFILE -NOMVAR] [fstdfield stats GRAPHPROFILE -leveltype] (ptop=[format %.2f [fstdfield stats GRAPHPROFILE -top]] pref=[format %.2f [fstdfield stats GRAPHPROFILE -ref]] rcoef=[format %.2f [fstdfield stats GRAPHPROFILE -coef]])" }
               "ETA"    { $data(Canvas) itemconfigure $obj -text "[fstdfield define GRAPHPROFILE -NOMVAR] [fstdfield stats GRAPHPROFILE -leveltype] (ptop=[format %.2f [fstdfield stats GRAPHPROFILE -top]])" }
               default  { $data(Canvas) itemconfigure $obj -text "[fstdfield define GRAPHPROFILE -NOMVAR]" }
            }
         }

         #----- Check for vertical coordinate selection
         if { $graph(ZType)=="PRESSURE" && [llength [set levels [fstdfield stats GRAPHPROFILE -pressurelevels]]] } {
            vector set $Item.Y $levels
            set graph(UnitY) Pressure
         } else {
            vector set $Item.Y [fstdfield stats GRAPHPROFILE -levels]
            set graph(UnitY) [fstdfield stats $Data -leveltype]
         }

         vector set $Item.X [fstdfield define GRAPHPROFILE -DATA]

         set graph(UnitX) [fstdfield configure $Data -unit]
      } elseif { [observation is $Data] } {

         set lst {}
         vector set $Item.X {}
         vector set $Item.Y {}

         foreach obs $data(Data$Data) {
            foreach idx [observation define $obs -IDX $data(Obs$Pos)] {
                if { [set val [lindex [observation define $obs -DATA $idx] 0]]!="-" } {
                  lappend lst [list $val [lindex [observation define $obs -COORD $idx] 2]]
               }
            }
         }
         set lst [lsort -index 0 -real -increasing $lst]
         foreach l $lst {
            vector append $Item $l
         }
      }
   }
}

#-------------------------------------------------------------------------------
# Nom      : <Graph::Profile::Update>
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

proc Graph::Profile::Update { Frame { GR {} } } {
   variable Data

   if { ![llength $GR] } {
      set GR [Page::Registered All Graph::Profile]
   }

   foreach gr $GR {

      upvar #0 Graph::Profile::Profile${gr}::Data  data

      if { $data(FrameData)==$Frame } {

         catch {
            $data(FrameData).page.canvas configure -cursor watch
            $data(Canvas) configure -cursor watch
            update idletasks
         }

         #----- Recuperer les donnees

         if { [Page::Registered All Viewport $data(VP)]!=-1 } {
            if { [info exist Animator::Play(Data$data(VP))] } {
               Graph::Profile::Data $gr $Animator::Play(Data$data(VP))
            } else {
               Graph::Profile::Data $gr $Viewport::Data(Data$data(VP))
               set list $Viewport::Data(Data$data(VP))
            }
         }
         #----- Update des items

         foreach pos $data(Pos) {
            Graph::Profile::ItemDefine $gr $pos $data(Pos$pos)
         }
         Graph::PosSet $gr Profile

         catch {
            $data(Canvas) configure -cursor left_ptr
            $data(FrameData).page.canvas configure -cursor left_ptr
         }
      }
   }
}

#-------------------------------------------------------------------------------
# Nom      : <Graph::Profile::UpdateItems>
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

proc Graph::Profile::UpdateItems { Frame { GR  { } } } {
   global GDefs
   variable Data
   variable Lbl

   if { ![llength $GR] } {
      set GR [Page::Registered All Graph::Profile]
   }

   foreach gr $GR {

      upvar #0 Graph::Profile::Profile${gr}::Data  data

      if { $data(VP)!="" && $data(FrameData)==$Frame } {

         $Frame.page.canvas delete GRAPHPROFILE$gr
         foreach pos $data(Pos) {
            if { [llength $data(Items$pos)] } {
               set id [graphitem configure [lindex $data(Items$pos) 0] -desc]
               set desc [lindex [$data(Canvas) itemconfigure $id -text] end]
               Graph::ItemPos $Frame $data(VP) $data(Pos$pos) "[lindex $Lbl(Title) $GDefs(Lang)]\n$desc" GRAPHPROFILE$gr
            }
        }
      }
   }
}

#-------------------------------------------------------------------------------
# Nom      : <Graph::Profile::Data>
# Creation : Mai 2000 - J.P. Gauthier - CMC/CMOE
#
# But      : Recupere les champs correspondants disponibles.
#
# Parametres :
#   <GR>     : Identificateur du Graph
#   <Data>   : Liste des donnees disponibles
#
# Retour    :
#
# Remarque :
#
#-------------------------------------------------------------------------------

proc Graph::Profile::Data { GR Data } {
   global   GDefs
   variable Msg

   upvar #0 Graph::Profile::Profile${GR}::Data  data

   SPI::Progress 0 [lindex $Msg(Reading) $GDefs(Lang)]

   #----- Recuperer les champs correspondants du viewport actif

   set data(Data)   {}
   set data(ObsIds) {}
   set nb [expr 95.0/([llength $Data]+1)]
   SPI::Progress +$nb

   foreach item $Data {
      if { [fstdfield is $item] && [fstdfield define $item -GRTYP]!="V" } {
         if { $Graph::Data(IP3) } {
            fstdfield readcube $item
         } else {
            set ip3 [fstdfield define $item -IP3]
            fstdfield define $item -IP3 -1
            fstdfield readcube $item
            fstdfield define $item -IP3 $ip3
         }
         lappend data(Data) $item
      } elseif { [observation is $item] } {
         if { [set box  [lindex [observation stats $item -tag] end]]=="" } {
             continue
         }
 #        set data(ObsIds) [concat $data(ObsIds) [observation define $item -ID]]

         set i 0
         set data(Data$item) {}
         foreach id  [ObsBox::GetContent $box] {
            if { [observation configure $item -desc]==[observation configure $id -desc] &&
                 [observation define $item -DATE]==[observation define $id -DATE] } {

               lappend data(Data$item) $id
               set data(ObsIds) [concat $data(ObsIds) [observation define $id -ID]]
            }
         }

         set data(Data$item) [lsort -integer -increasing -index 0 $data(Data$item)]
         set data(ObsIds)    [lsort -unique -dictionary -increasing $data(ObsIds)]
         lappend data(Data) $item
      }
      SPI::Progress +$nb  [lindex $Msg(Reading) $GDefs(Lang)]
   }
   Graph::ParamsObsSearch Profile $GR

   #----- Applique le calcul MACRO au cubes de donnees

   set data(Data) [FieldCalc::Operand $data(VP) $data(Data)]

   SPI::Progress 0
}
