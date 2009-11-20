#===============================================================================
# Environnement Canada
# Centre Meteorologique Canadian
# 2100 Trans-Canadienne
# Dorval, Quebec
#
# Projet   : Librairie d'objects visuel interactifs
# Fichier  : Graph_Compare.tcl
# Creation : Novembre 2003 - J.P. Gauthier - CMC/CMOE
#
# Description: Ce package s'occupe de l'affichage et de la manipulation de graphs
#              de profil verticaux
#
# Fonctions:
#
#    Graph::Compare::Create       { Frame X0 Y0 Width Height Active Full }
#    Graph::Compare::Coord        { Frame GR X Y }
#    Graph::Compare::Destroy      { Frame GR }
#    Graph::Compare::DrawInit     { Frame VP }
#    Graph::Compare::Draw         { Frame VP }
#    Graph::Compare::DrawDone     { Frame VP }
#    Graph::Compare::MoveInit     { Frame VP }
#    Graph::Compare::Move         { Frame VP }
#    Graph::Compare::MoveDone     { Frame VP }
#    Graph::Compare::Graph        { GR }
#    Graph::Compare::Init         { Frame }
#    Graph::Compare::Params       { Parent GR }
#    Graph::Compare::ItemAdd      { GR Item }
#    Graph::Compare::ItemDefault  { GR Item }
#    Graph::Compare::ItemDel      { GR Item }
#    Graph::Compare::ItemDefine   { GR Pos Coords { Update True } }
#    Graph::Compare::ItemUnDefine { GR Pos }
#    Graph::Compare::ItemData     { GR Pos Item Data }
#    Graph::Compare::Update       { Frame { GR {} } }
#    Graph::Compare::UpdateItems  { Frame { GR  { } } }
#    Graph::Compare::Data         { GR Data }
#
#===============================================================================

namespace eval Graph::Compare { } {
   variable Lbl

   set Lbl(Title)     { "Comparaison" "Comparison" }
}

#----------------------------------------------------------------------------
# Nom      : <Graph::Compare::Create>
# Creation : Novembre 2003 - J.P. Gauthier - CMC/CMOE
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

proc Graph::Compare::Create { Frame X0 Y0 Width Height Active Full } {
   global GDefs
   variable Data
   variable Lbl

   set gr [Graph::Compare::Init $Frame]
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
   set Graph::Data(Type$gr)     Compare    ;#Type de graph

   upvar #0 Graph::Compare::Compare${gr}::Data  data
   upvar #0 Graph::Compare::Compare${gr}::Graph graph

   set data(Canvas)    $Frame.page.canvas
   set data(Frame)     $Frame

   $data(Canvas) bind GRAPHUPDATE$gr <Any-KeyRelease> "$data(Canvas) itemconfigure GRAPH$gr  -update True"
   set id [$data(Canvas) create text $X0 $Y0  -tags "$tag CVTEXT GRAPHUPDATE$gr" -text [lindex $Lbl(Title) $GDefs(Lang)] \
      -font $Graph::Font(Graph) -fill black -anchor nw -justify center]
   $data(Canvas) create graph -x $X0 -y $Y0 -width $Width -height $Height -anchor nw -xlegend 5 -ylegend 5 -command $gr \
       -fg black -bg $Graph::Color(Frame) -fill $Graph::Color(Graph) -tags "$tag GRAPH$gr" -font $Graph::Font(Graph) -title $id
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
      Graph::Compare::Update $data(FrameData) $gr
   } else {
      set data(VP)        ""
      set data(FrameData) ""
   }

   Graph::Activate $Frame $gr Compare
   Graph::Mode Compare $gr True
   Graph::PosAdd $gr Compare

   #----- Creer les fonction du mode actif

   if { $Active } {
      Page::ActiveWrapper Graph $Frame $gr $X0 $Y0 [expr $Width+$X0] [expr $Height+$Y0] Compare
   }
   Page::Register $Frame Graph::Compare $gr

   return $gr
}

#-------------------------------------------------------------------------------
# Nom      : <Graph::Compare::Coord>
# Creation : Novembre 2003 - J.P. Gauthier - CMC/CMOE
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

proc Graph::Compare::Coord { Frame GR X Y } {
   global   GDefs

   upvar #0 Graph::Compare::Compare${GR}::Data  data

   set Page::Data(Coord) ""
   set Page::Data(Value) ""

   if  { [llength [set items [lindex [$data(Canvas) itemconfigure GRAPH$GR -item] end]]] } {
      set coords [$GR -unproject $X $Y False [lindex $items 0]]

      if { [llength $coords]==2 } {
         set Page::Data(Value) "[lindex $Graph::Lbl(Val) $GDefs(Lang)]: [lindex $coords 1]"
      }
   }
}

#----------------------------------------------------------------------------
# Nom      : <Graph::Compare::Destroy>
# Creation : Janvier 2003 - J.P. Gauthier - CMC/CMOE
#
# But      : Supprimer un viewport ainsi que tout ses widgets
#
# Parametres :
#   <Frame>  : Indentificateur de Page
#   <GR>     : Indentificateur du Graph
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc Graph::Compare::Destroy { Frame GR } {
   variable Data

   upvar #0 Graph::Compare::Compare${GR}::Data  data

   #----- Supprimer le graph et ses items

   $Frame.page.canvas delete $Page::Data(Tag)$GR
   if { $data(FrameData)!="" } {
      $data(FrameData).page.canvas delete GRAPHCOMPARE$GR
   }

   #----- Supprimer ses items

   foreach pos $data(Pos) {
      Graph::Compare::ItemUnDefine $GR $pos
   }
   namespace delete Compare$GR
}

#----------------------------------------------------------------------------
# Nom      : <Graph::Compare::Draw...>
# Creation : Novembre 2003 - J.P. Gauthier - CMC/CMOE
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

proc Graph::Compare::DrawInit { Frame VP } {
   variable Data

   upvar #0 Graph::Compare::Compare${Graph::Data(Graph)}::Data  data

   if { $VP!=$data(VP) } {
      set data(VP)        $VP
      set data(FrameData) $Frame
      Graph::Compare::Update $Frame $Graph::Data(Graph)
   } else {
      Graph::Compare::ItemDefine $Graph::Data(Graph) $Graph::Data(Pos) [list $Viewport::Map(LatCursor) $Viewport::Map(LonCursor)]
   }
}

proc Graph::Compare::Draw { Frame VP } {
   Graph::Compare::DrawInit $Frame $VP
}

proc Graph::Compare::DrawDone { Frame VP } { }

proc Graph::Compare::MoveInit { Frame VP } { }

proc Graph::Compare::Move { Frame VP } { }

proc Graph::Compare::MoveDone { Frame VP } { }

#-------------------------------------------------------------------------------
# Nom      : <Graph::Compare::Graph>
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

proc Graph::Compare::Graph { GR } {
   variable Data

   upvar #0 Graph::Compare::Compare${GR}::Graph graph
   upvar #0 Graph::Compare::Compare${GR}::Data  data

   if { ![llength $data(Items)] } {
      return
   }

   $data(Canvas) config -cursor watch
   update idletasks

   #----- Recalculer les valeurs

   set data(YMin)  1e200
   set data(YMax) -1e200
   set yincr 0
   set mod True

   #----- Afficher le graph

   foreach item $data(Items) {
      set min [vector stats $item.Y -min]
      set min 0
      set max [vector stats $item.Y -max]
      set data(YMin) [expr $min<$data(YMin)?$min:$data(YMin)]
      set data(YMax) [expr $max>$data(YMax)?$max:$data(YMax)]
   }

   set i -1
   set graph(XInter) {}
   foreach pos $data(DescPos) {
      lappend graph(XInter) [incr i]
   }

   set data(XMin)  -1
   set data(XMax)  [llength $graph(XInter)]

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

   if { [llength $graph(ZYInter)] } {
      set data(YMin) [lindex $graph(ZYInter) 0]
      set data(YMax) [lindex $graph(ZYInter) 1]
      set mod False
   }

   set id [graphaxis configure axisx$GR -unit]
   $data(Canvas) itemconfigure $id -font $Graph::Font(Axis) -fill $Graph::Color(Axis)
   graphaxis configure axisx$GR -type $graph(XScale) -modulo $mod -min $data(XMin) -max $data(XMax) -intervals $graph(XInter) -labels $data(DescPos) -angle $Graph::Font(Angle)  \
      -lowoffset 0.1 -highoffset 0.1 -font $Graph::Font(Axis) -gridcolor $Graph::Grid(Color) -dash $Graph::Grid(Dash) -gridwidth $Graph::Grid(Width) -color $Graph::Color(Axis)

   set id [graphaxis configure axisy$GR -unit]
   $data(Canvas) itemconfigure $id -font $Graph::Font(Axis) -fill $Graph::Color(Axis)
   graphaxis configure axisy$GR -type $graph(YScale)-modulo $mod -min $data(YMin) -max $data(YMax) -intervals $yinter -increment $yincr -angle $Graph::Font(Angle) \
      -highoffset 0.1 -font $Graph::Font(Axis) -gridcolor $Graph::Grid(Color) -dash $Graph::Grid(Dash) -gridwidth $Graph::Grid(Width) -color $Graph::Color(Axis)

   set id [lindex [$data(Canvas) itemconfigure GRAPH$GR -title] end]
   $data(Canvas) itemconfigure $id -font $Graph::Font(Graph) -fill $Graph::Color(FG)
   $data(Canvas) itemconfigure GRAPH$GR -item $data(Items) -bd $Graph::Width(Frame) \
      -fg $Graph::Color(FG) -bg $Graph::Color(BG) -fill $Graph::Color(Fill) -font $Graph::Font(Graph)

   update idletasks
   $data(Canvas) config -cursor left_ptr
}

#----------------------------------------------------------------------------
# Nom      : <Graph::Compare::Init>
# Creation : Novembre 2003 - J.P. Gauthier - CMC/CMOE
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

proc Graph::Compare::Init { Frame } {
   global   GDefs
   variable Data

   set gr GR[incr Graph::Data(Nb)]

   namespace eval Compare$gr {
      variable Data
      variable Graph

      set Data(Items)           {}        ;#Liste des items
      set Data(Pos)             {}        ;#Liste des positions
      set Data(PosPos)          {}
      set Data(DescPos)         {}
      set Data(Data)            {}        ;#Liste des champs selectionnees
      set Data(ObsIds)          {}        ;#Liste des positions observations
      set Data(ObsToken)        ""        ;#Token de recherche

      #----- Constantes relatives au Graph

      set Graph(UnitY)    "[lindex $Graph::Lbl(Unit) $GDefs(Lang)] Y"         ;#Descriptif de l'echelle des valeur en Y
      set Graph(UnitX)    ""                                                  ;#Descriptif de l'echelle des valeur en Y
      set Graph(XScale)   LINEAR                                              ;#Type d'echelle en X
      set Graph(YScale)   LINEAR                                              ;#Type d'echelle en Y
      set Graph(XInter)   ""                                                  ;#Liste des niveau specifie par l'usager
      set Graph(YInter)   ""                                                  ;#Liste des niveau specifie par l'usager
      set Graph(ZYInter)  ""                                                  ;#Liste des Niveaux (Mode Zoom)
   }
   return $gr
}

#-------------------------------------------------------------------------------
# Nom      : <Graph::Compare::Params>
# Creation : Novembre 2003 - J.P. Gauthier - CMC/CMOE -
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

proc Graph::Compare::Params { Parent GR } {
   global   GDefs

   upvar #0 Graph::Compare::Compare${GR}::Data  data

   Graph::ParamsPos  $Parent
   Graph::ParamsItem $Parent

   labelframe $Parent.scale -text [lindex $Graph::Lbl(Scale) $GDefs(Lang)]
      frame $Parent.scale.valy -relief sunken -bd 1
         label $Parent.scale.valy.lbl -text "Y"
         checkbutton $Parent.scale.valy.scale -text Log -indicatoron false \
            -command "Graph::Compare::Graph $GR" -bd 1 \
            -variable Graph::Compare::Compare${GR}::Graph(YScale)  -onvalue LOGARITHMIC -offvalue LINEAR
         entry $Parent.scale.valy.list -textvariable Graph::Compare::Compare${GR}::Graph(YInter) -bg $GDefs(ColorLight) -relief flat -width 1
         pack $Parent.scale.valy.lbl -side left -fill y
         pack $Parent.scale.valy.list -side left -fill x  -expand true
         pack $Parent.scale.valy.scale -side left -fill y
      pack $Parent.scale.valy -side top -padx 2 -pady 2 -fill x
   pack $Parent.scale -side top -fill x -padx 5

   Graph::ParamsObs $Parent Compare $GR

   Bubble::Create $Parent.scale.valy $Graph::Bubble(ScaleY)

   bind $Parent.scale.valy.list <Return>  "Graph::Compare::Graph $GR"
}

#-------------------------------------------------------------------------------
# Nom      : <Graph::Compare::ItemAdd>
# Creation : Avril 2005 - J.P. Gauthier - CMC/CMOE -
#
# But      : Creer un nouvel item de graph
#
# Parametres :
#   <GR>     : Identificateur du Graph
#   <Item>   : Identificateur de l'item positionnel a ajouter
#   <Desc>   : Description de l'item
#
# Remarques :
#
#-------------------------------------------------------------------------------

proc Graph::Compare::ItemAdd { GR Item } {

   upvar #0 Graph::Compare::Compare${GR}::Data  data

   if { [lsearch -exact $data(Items) $Item]==-1 } {
      vector free   $Item
      vector create $Item
      vector dim    $Item { X Y }

      set id [$data(Canvas) create text -100 -100  -tags "$Page::Data(Tag)$GR CVTEXT GRAPHUPDATE$GR" -text $Item -anchor nw -justify left]

      graphitem create $Item
      graphitem configure $Item -xaxis axisx$GR -yaxis axisy$GR -xdata $Item.X -ydata $Item.Y -orient X -desc $id

      lappend data(Items) $Item
      Graph::Compare::ItemDefault $GR $Item
      $data(Canvas) itemconfigure GRAPH$GR -item $data(Items)
   }
}

#-------------------------------------------------------------------------------
# Nom      : <Graph::Compare::ItemDefault>
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

proc Graph::Compare::ItemDefault { GR Item } {

   upvar #0 Graph::Compare::Compare${GR}::Data  data

   set idx [lsearch -exact $data(Items) $Item]

   set Graph::Item(Outline)     [lindex $Graph::Graph(Colors) [expr $idx%[llength $Graph::Graph(Colors)]]]
   set Graph::Item(FillColor)   #FFFFFF
   set Graph::Item(Tranparency) 100
   set Graph::Item(Width)       1
   set Graph::Item(Size)        0
   set Graph::Item(Value)       False
   set Graph::Item(Dash)        ""
   set Graph::Item(Type)        BAR
   set Graph::Item(Icon)        [lindex $Graph::Graph(Icons) [expr $idx%[llength $Graph::Graph(Icons)]]]
   set Graph::Item(Bitmap)      ""
   set Graph::Item(Stipple)     -1
   set Graph::Item(Image)       ""

   Graph::ItemConfigure $GR Compare $Item
}

#-------------------------------------------------------------------------------
# Nom      : <Graph::Compare::ItemDel>
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

proc Graph::Compare::ItemDel { GR Item } {

   upvar #0 Graph::Compare::Compare${GR}::Data  data

   if { [set idx [lsearch -exact $data(Items) $Item]]!=-1 } {
      set data(Items) [lreplace $data(Items) $idx $idx]
      $data(Canvas) delete [graphitem configure $Item -desc]
      $data(Canvas) itemconfigure GRAPH$GR -item $data(Items)

      vector free $Item
      graphitem free $Item
   }
}

#-------------------------------------------------------------------------------
# Nom      : <Graph::Compare::CompareDefine>
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

proc Graph::Compare::ItemDefine { GR Pos Coords { Update True } } {

   upvar #0 Graph::Compare::Compare${GR}::Data  data

   if { $Pos=="" } {
      return
   }

   if { [info exists Graph::Compare::Compare${GR}::Data(Items)] } {
      foreach item [lrange $data(Items) [llength $data(Data)] end] {
         Graph::Compare::ItemDel $GR $item
      }
   }

   if { [set idx [lsearch -exact $data(Pos) $Pos]]==-1 } {
      lappend data(Pos) $Pos
      lappend data(PosPos) $Coords
      lappend data(DescPos) $Desc
   } else {
      lset data(PosPos) $idx $Coords
      lset data(DescPos) $idx $Desc
   }

   set data(Items$Pos) {}

   Graph::Idle $GR Compare
   set i -1
   foreach field $data(Data)] {
      set item ${Pos}_Item[incr i]

      lappend data(Items$Pos) $item
      Graph::Compare::ItemAdd $GR $item
      Graph::Compare::ItemData $GR $Pos $item $field
   }

   Graph::Compare::UpdateItems $data(FrameData) $GR
   Graph::Compare::Graph $GR
   Graph::UnIdle $GR Compare
}

#-------------------------------------------------------------------------------
# Nom      : <Graph::Compare::ItemUnDefine>
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

proc Graph::Compare::ItemUnDefine { GR Pos } {

   upvar #0 Graph::Compare::Compare${GR}::Data  data

   if { [set idx [lsearch -exact $data(Pos) $Pos]]!=-1 } {
      foreach item $data(Items$Pos) {
         Graph::Compare::ItemDel $GR $item
      }

      set data(Pos)        [lreplace $data(Pos)     $idx $idx]
      set data(PosPos)     [lreplace $data(PosPos)  $idx $idx]
      set data(DescPos)    [lreplace $data(DescPos) $idx $idx]
      set Graph::Data(Pos) [lindex $data(Pos) end]
   }
}

#-------------------------------------------------------------------------------
# Nom      : <Graph::Compare::ItemData>
# Creation : Avril 2005 - J.P. Gauthier - CMC/CMOE -
#
# But      : Recuperer les donnees d'un item
#
# Parametres :
#   <GR>     : Identificateur du Graph
#   <Pos>    : Position
#   <Desc>   : Description de la position
#   <Coords> : Coordonnees
#
# Remarques :
#
#-------------------------------------------------------------------------------

proc Graph::Compare::ItemData { GR Pos Item Data } {

   upvar #0 Graph::Compare::Compare${GR}::Data  data
   upvar #0 Graph::Compare::Compare${GR}::Graph graph

   if { [graphitem is $Item] } {

      set no  -1
      set $Item.Label    {}
      vector set $Item.X {}
      vector set $Item.Y {}

      foreach desc $Desc coords $Coords {

         if { ![llength $coords] } {
            continue
         }
         set lat [lindex $coords 0]
         set lon [lindex $coords 1]

         if { [fstdfield is $Data] } {

            set val  [fstdfield stats  $Data -coordvalue $lat $lon]
            set spd  [lindex $val 0]
            set dir  [lindex $val 1]

            if { $spd!="-" } {
               lappend $Item.Label   $desc
               vector append $Item.X [incr no]
               vector append $Item.Y $spd
            }
            graphitem configure $Item -desc "FLD [fstdfield configure $Data -desc]"

         } elseif { [observation is $Data] } {

            if { [set idx [lindex [observation define $Data -IDX $desc] 0]]!="" } {

               set val  [observation define $Data -DATA $idx]
               set spd  [lindex $val 0]
               set dir  [lindex $val 1]

               if { $spd!="-" } {
                  lappend $Item.Label   $desc
                  vector append $Item.X [incr no]
                  vector append $Item.Y $spd
               }
               graphitem configure $Item -desc "OBS [observation configure $Data -desc]"
            }
         }
      }
   }
}

#-------------------------------------------------------------------------------
# Nom      : <Graph::Compare::Update>
# Creation : Novembre 2003 - J.P. Gauthier - CMC/CMOE
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

proc Graph::Compare::Update { Frame { GR {} } } {
   variable Data

   if { ![llength $GR] } {
      set GR [Page::Registered All Graph::Compare]
   }

   foreach gr $GR {

      upvar #0 Graph::Compare::Compare${gr}::Data  data

      if { $data(FrameData)==$Frame } {

         $data(FrameData).page.canvas configure -cursor watch
         $data(Canvas) configure -cursor watch
         update idletasks

         #----- Recuperer les donnees

         if { [Page::Registered All Viewport $data(VP)]!=-1 } {
            Graph::Compare::Data $gr [Viewport::Assigned $Viewport::Data(Frame$data(VP)) $data(VP) { fstdfield observation }]
         }
         #----- Update des items

         foreach pos $data(Pos) desc $data(DescPos) coords $data(PosPos) {
            Graph::Compare::ItemDefine $gr $pos $desc $coords
         }

         $data(Canvas) configure -cursor left_ptr
         $data(FrameData).page.canvas configure -cursor left_ptr
      }
   }
}

#-------------------------------------------------------------------------------
# Nom      : <Graph::Compare::UpdateItems>
# Creation : Novembre 2003 - J.P. Gauthier - CMC/CMOE
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

proc Graph::Compare::UpdateItems { Frame { GR  { } } } {
   global GDefs
   variable Data
   variable Lbl

   if { ![llength $GR] } {
      set GR [Page::Registered All Graph::Compare]
   }

   foreach gr $GR {

      upvar #0 Graph::Compare::Compare${gr}::Data  data

      if { $data(VP)!="" && $data(FrameData)==$Frame } {

         $Frame.page.canvas delete GRAPHCOMPARE$gr
         foreach pos $data(Pos) desc $data(DescPos) coords $data(PosPos) {
            Graph::ItemPos $Frame $data(VP) $coords "[lindex $Lbl(Title) $GDefs(Lang)]\n$desc" GRAPHCOMPARE$gr
         }
      }
   }
}

#-------------------------------------------------------------------------------
# Nom      : <Graph::Compare::Data>
# Creation : Mai 2000 - J.P. Gauthier - CMC/CMOE
#
# But      : Recupere les champs correspondants disponibles.
#
# Parametres :
#   <GR>     : Identificateur du Graph
#   <List>   : Liste des donnees disponibles
#
# Retour    :
#
# Remarque :
#
#-------------------------------------------------------------------------------

proc Graph::Compare::Data { GR Data } {
   global   GDefs
   variable Lbl

   upvar #0 Graph::Compare::Compare${GR}::Data  data
   upvar #0 Graph::Compare::Compare${GR}::Graph graph

   #----- Recuperer les champs correspondants du viewport actif

   set data(Data)   {}
   set data(ObsIds) {}

   foreach item $Data {
      if { [fstdfield is $item] } {
         lappend data(Data) $item
      }
      if { [observation is $item] } {
         lappend data(Obs) $item
         set data(ObsIds) [concat $data(ObsIds) [observation define $item -ID]]
      }
   }

   set data(ObsIds) [lsort -unique -dictionary -increasing $data(ObsIds)]

   Graph::ParamsScaleUniform Compare $GR
}
