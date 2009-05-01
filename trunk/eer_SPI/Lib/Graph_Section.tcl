#===============================================================================
# Environnement Canada
# Centre Meteorologique Canadian
# 2100 Trans-Canadienne
# Dorval, Quebec
#
# Projet   : Librairie d'objects visuel interactifs
# Fichier  : Graph_Section.tcl
# Creation : Octobre 2003 - J.P. Gauthier - CMC/CMOE
#
# Description: Ce package s'occupe de l'affichage et de la manipulation de graphs
#              de profil verticaux
#
# Fonctions:
#
#    Graph::Section::Create       { Frame X0 Y0 Width Height Active Full }
#    Graph::Section::Coord        { Frame GR X Y }
#    Graph::Section::Destroy      { Frame GR }
#    Graph::Section::Graph        { GR { Pos False } }
#    Graph::Section::Init         { Frame }
#    Graph::Section::Params       { Parent GR }
#    Graph::Section::ItemAdd      { GR Item }
#    Graph::Section::ItemDefault  { GR Item }
#    Graph::Section::ItemDel      { GR Item }
#    Graph::Section::ItemDefine   { GR Pos Coords { Update True } }
#    Graph::Section::ItemUnDefine { GR Pos }
#    Graph::Section::ItemData     { GR Pos Item Data }
#    Graph::Section::Update       { Frame { GR {} } }
#    Graph::Section::UpdateItems  { Frame { GR { } } }
#    Graph::Section::Data         { GR Data }
#    Graph::Section::Resolution   { VP Field }
#    Graph::Section::VertexAdd    { Frame VP X Y }
#    Graph::Section::VertexDelete { Frame VP }
#    Graph::Section::VertexFollow { Frame VP X Y Scan }
#    Graph::Section::Sample       { GR VP Coord { Res 0 } }
#    Graph::Section::FieldShow    { GR }
#
#===============================================================================

namespace eval Graph::Section { } {
   variable Lbl
   variable Msg

   set Lbl(Title)     { "Coupe verticale" "Vertical cross-section" }
   set Lbl(Grid)      { "Grille" "Grid" }
   set Lbl(Pres)      { "Pression" "Pressure" }

   set Msg(Reading)   { "Lecture des données" "Reading data" }
}

#----------------------------------------------------------------------------
# Nom      : <Graph::Section::Create>
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

proc Graph::Section::Create { Frame X0 Y0 Width Height Active Full { Link True } } {
   global GDefs
   variable Data
   variable Lbl

   set gr [Graph::Section::Init $Frame]
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
   set Graph::Data(ToolMode$gr) Draw       ;#Mode de selection
   set Graph::Data(Type$gr)     Section    ;#Type de graph

   upvar #0 Graph::Section::Section${gr}::Data  data
   upvar #0 Graph::Section::Section${gr}::Graph graph

   set data(Canvas)    $Frame.page.canvas
   set data(Frame)     $Frame

   $data(Canvas) bind GRAPHUPDATE$gr <Any-KeyRelease> "$data(Canvas) itemconfigure GRAPH$gr -update True"
   set id [$data(Canvas) create text $X0 $Y0  -tags "$tag CVTEXT GRAPHUPDATE$gr" -text [lindex $Lbl(Title) $GDefs(Lang)] \
      -font $Graph::Font(Graph) -fill black -anchor nw -justify center]
   $data(Canvas) create graph -x $X0 -y $Y0 -width $Width -height $Height -anchor nw -legend 0 -command $gr \
       -fg black -bg $Graph::Color(Frame) -fill $Graph::Color(Graph) -tags "$tag GRAPH$gr" -font $Graph::Font(Graph) \
       -legend False -title $id
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
      Graph::Section::Update $data(FrameData) $gr
   } else {
      set data(VP)        ""
      set data(FrameData) ""
   }

   Graph::Activate $Frame $gr Section

   if { $Graph::Data(Link$gr) } {
      Graph::Mode Section $gr True
      Graph::PosAdd $gr Section
   }

   #----- Creer les fonction du mode actif

   if { $Active } {
      Page::ActiveWrapper Graph $Frame $gr $X0 $Y0 [expr $Width+$X0] [expr $Height+$Y0] Section
   }
   Page::ActiveFull Graph $Frame $gr $Full
   Page::Register $Frame Graph::Section $gr

   return $gr
}

#-------------------------------------------------------------------------------
# Nom      : <Graph::Section::Coord>
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

proc Graph::Section::Coord { Frame GR X Y } {
   global   GDefs

   upvar #0 Graph::Section::Section${GR}::Data  data
   upvar #0 Graph::Section::Section${GR}::Graph graph

   set Page::Data(Coord) ""
   set Page::Data(Value) ""

   if  { [llength [set items [lindex [$data(Canvas) itemconfigure GRAPH$GR -item] end]]] } {

      set coords [$GR -unproject $X $Y False [lindex $items 0]]

      if { [llength $coords]>=2 } {
         set Page::Data(Coord) "[lindex $Graph::Lbl(Level) $GDefs(Lang)]: [format "%1.3e" [lindex $coords 1]] [lindex $Graph::Lbl(Pos) $GDefs(Lang)]:[format "%.3f" [lindex $coords 0]]"

         set Page::Data(Value) ""
         foreach item $items {
            set field [graphitem configure $item -data]
            if { [fstdfield is $field] } {
               append Page::Data(Value) "[fstdfield configure $field -desc]:[FSTD::FieldFormat $field [lindex $coords 2]] "
            }
         }
         if { $data(Proj) } {

         }
      }
   }
}

#----------------------------------------------------------------------------
# Nom      : <Graph::Section::Destroy>
# Creation : Janvier 2002 - J.P. Gauthier - CMC/CMOE
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

proc Graph::Section::Destroy { Frame GR } {
   variable Data

   upvar #0 Graph::Section::Section${GR}::Data  data

   #----- Liberer l'allocation des champs

   Animator::EmptyPlayList

   #----- Supprimer le graph et ses items

   $Frame.page.canvas delete $Page::Data(Tag)$GR ID$Page::Data(Tag)$GR

   if { $data(FrameData)!="" } {
      $data(FrameData).page.canvas delete GRAPHSECTION$GR
   }

   #----- Supprimer ses items

   foreach pos $data(Pos) {
      Graph::Section::ItemUnDefine $GR $pos
   }

   namespace delete Section$GR
}

#-------------------------------------------------------------------------------
# Nom      : <Graph::Section::Graph>
# Creation : Fevrier 1999 - J.P. Gauthier - CMC/CMOE
#
# But      : Affiche le graphique des series temporelles.
#
# Parametres :
#   <GR>     : Indentificateur du Graph
#   <Pos>    : Recalculer les positions
#
# Retour    :
#
# Remarque :
#
#-------------------------------------------------------------------------------

proc Graph::Section::Graph { GR { Pos False } } {
   variable Data

   upvar #0 Graph::Section::Section${GR}::Data  data
   upvar #0 Graph::Section::Section${GR}::Graph graph

   if { ![llength $data(Items)] } {
      return
   }

   if { $Pos } {
      Graph::Section::ItemDefine $Graph::Data(Graph) $Graph::Data(Pos) [Graph::Section::Sample $Graph::Data(Graph) $data(VP) $data(Coords)]
   }

   $data(Canvas) config -cursor watch
   update idletasks

   #----- Recalculer les valeurs
   set data(XMin)   1e200
   set data(XMax)  -1e200
   set data(YMin)   1e200
   set data(YMax)  -1e200
   set data(Levels) {}
   set yincr  0
   set xincr  0
   set mod True

   #----- Extraire les limites des valeurs
   foreach item $data(Items) {
      if { [fstdfield is GRAPHSECTION$item] } {
         #----- Check for vertical coordinate selection
         if { $graph(ZType)=="PRESSURE" && [llength [set levels [fstdfield stats GRAPHSECTION$item -pressurelevels]]] } {
            set data(Levels) $levels
            fstdfield configure GRAPHSECTION$item -ztype PRESSURE
         } else {
            set data(Levels) [fstdfield stats GRAPHSECTION$item -levels]
            fstdfield configure GRAPHSECTION$item -ztype NONE
         }
         set data(XMin)   0
         set data(XMax)   [expr [fstdfield define GRAPHSECTION$item -NI]-1]
      }
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

   set graph(XInter) $data(DCoords)
   set graph(XLabel) [lrange $Graph::Graph(Identitys) 0 [expr [llength $data(DCoords)]-1]]

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
   graphaxis configure axisx$GR -type $graph(XScale) -modulo $mod -min $data(XMin) -max $data(XMax) -intervals $graph(XInter) -labels $graph(XLabel) \
      -font $Graph::Font(Axis) -gridcolor $Graph::Grid(Color)  -dash $Graph::Grid(Dash) -gridwidth $Graph::Grid(Width) -color $Graph::Color(Axis)

   set id [graphaxis configure axisy$GR -unit]
   if { $Graph::Data(Update) } {
      $data(Canvas) itemconfigure $id -text $graph(UnitY)
   }
   $data(Canvas) itemconfigure $id -font $Graph::Font(Axis) -fill $Graph::Color(Axis)
   graphaxis configure axisy$GR -type $graph(YScale) -modulo $mod -min $data(YMin) -max $data(YMax) -intervals $yinter -increment $yincr -angle $Graph::Font(Angle) \
      -font $Graph::Font(Axis) -gridcolor $Graph::Grid(Color)  -dash $Graph::Grid(Dash) -gridwidth $Graph::Grid(Width) -color $Graph::Color(Axis)

   set id [lindex [$data(Canvas) itemconfigure GRAPH$GR -title] end]
   $data(Canvas) itemconfigure $id -font $Graph::Font(Graph) -fill $Graph::Color(FG)
   $data(Canvas) itemconfigure GRAPH$GR -item $data(Items) -bd $Graph::Width(Frame) \
      -fg $Graph::Color(FG) -bg $Graph::Color(BG) -fill $Graph::Color(Fill) -font $Graph::Font(Graph)

   if { $data(Proj) } {
      $data(FrameData).page.canvas itemconfigure $data(VP) -frame 0
   }

   update idletasks
   $data(Canvas) config -cursor left_ptr
}

#----------------------------------------------------------------------------
# Nom      : <Graph::Section::Init>
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

proc Graph::Section::Init { Frame } {
   global   GDefs
   variable Data

   set gr GR[incr Graph::Data(Nb)]

   namespace eval Section$gr {
      variable Data
      variable Graph

      set Data(Items)    {}           ;#Liste des items
      set Data(Pos)      {}           ;#Liste des positions
      set Data(Data)     {}           ;#Liste des champs selectionnees
      set Data(Proj)     0            ;#Mode projection
      set Data(ResBest)  True        ;#Selection de la resolution
      set Data(Res)      100000       ;#Resolution en metres
      set Data(DCoords)  {}           ;#Liste des longueur des segment
      set Data(Levels)   {}           ;#Liste des niveaux
      set Data(FCoords)  {}           ;#Liste des coordonnees de coupe (Follow mode)
      set Data(Coords)   {}           ;#Liste des coordonnees de coupe
      set Data(Field)    ""           ;#Champs de coupe

      #----- Constantes relatives au Graph

      set Graph(YScale)   LINEAR                                               ;#Type d'echelle en Y
      set Graph(XScale)   LINEAR                                               ;#Type d'echelle en Y
      set Graph(UnitY)    "[lindex $Graph::Lbl(Unit) $GDefs(Lang)] Y"          ;#Descriptif de l'echelle des valeur en Y
      set Graph(UnitX)    "[lindex $Graph::Lbl(Pos) $GDefs(Lang)]"             ;#Descriptif de l'echelle des valeur en X
      set Graph(XInter)   ""               ;#Liste des niveau specifie par l'usager
      set Graph(YInter)   ""               ;#Liste des niveau specifie par l'usager
      set Graph(ZXInter)  ""               ;#Liste des Niveaux (Mode Zoom)
      set Graph(ZYInter)  ""               ;#Liste des Niveaux (Mode Zoom)
      set Graph(ZType)    GRID             ;#Type de niveaux (GRID,PRESSSURE)
   }
   return $gr
}
#-------------------------------------------------------------------------------
# Nom      : <Graph::Section::Params>
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

proc Graph::Section::Params { Parent GR } {
   global   GDefs
   variable Lbl

   labelframe $Parent.disp -text [lindex $Graph::Lbl(Disp) $GDefs(Lang)]
      frame $Parent.disp.mode -relief sunken -bd 1
         checkbutton $Parent.disp.mode.proj  -text [lindex $Graph::Lbl(Proj) $GDefs(Lang)] -variable Graph::Section::Section${GR}::Data(Proj) -bd 1\
            -onvalue 1 -offvalue 0 -indicatoron false -command "Graph::Section::FieldShow $GR"
         pack $Parent.disp.mode.proj -side top -fill x
      pack $Parent.disp.mode -side top -fill x
   pack $Parent.disp -side top -fill x -pady 5

   labelframe $Parent.res -text [lindex $Graph::Lbl(Res) $GDefs(Lang)]
      entry $Parent.res.cur -textvariable Graph::Section::Section${GR}::Data(Res) -width 8 -bd 1 -bg $GDefs(ColorLight)
      scale $Parent.res.sc  -variable Graph::Section::Section${GR}::Data(Res) -resolution 1 -width 16 -sliderlength 8 -showvalue False\
         -orient horizontal -bd 1 -from 1 -to 100000
      checkbutton $Parent.res.auto -text * -indicatoron false -bd 1 -variable Graph::Section::Section${GR}::Data(ResBest) \
         -onvalue True -offvalue False -command "Graph::Section::Resolution; Graph::Section::Graph $GR True"
      pack  $Parent.res.sc -side left -fill x -expand true -padx 2
      pack  $Parent.res.cur $Parent.res.auto -side left -padx 2
   pack $Parent.res -side top -fill x -pady 5

   labelframe $Parent.scale -text [lindex $Graph::Lbl(Scale) $GDefs(Lang)]
      frame $Parent.scale.valy -relief sunken -bd 1
         label $Parent.scale.valy.lbl -text "Y"
         checkbutton $Parent.scale.valy.scale -text Log -indicatoron false \
            -command "Graph::Section::Graph $GR" -bd 1 \
            -variable Graph::Section::Section${GR}::Graph(YScale)  -onvalue LOGARITHMIC -offvalue LINEAR
         entry $Parent.scale.valy.list -textvariable Graph::Section::Section${GR}::Graph(YInter) -bg $GDefs(ColorLight) -relief flat -width 1
         pack $Parent.scale.valy.lbl -side left -fill y
         pack $Parent.scale.valy.list -side left -fill x  -expand true
         pack $Parent.scale.valy.scale -side left -fill y
      pack $Parent.scale.valy -side top -padx 2 -pady 2 -fill x
      frame $Parent.scale.type -relief sunken -bd 1
         radiobutton $Parent.scale.type.grid -text [lindex $Lbl(Grid) $GDefs(Lang)] -indicatoron false \
            -command "Graph::Section::Graph $GR" -bd 1 -variable Graph::Section::Section${GR}::Graph(ZType) -value GRID
         radiobutton $Parent.scale.type.pres -text [lindex $Lbl(Pres) $GDefs(Lang)] -indicatoron false \
            -command "Graph::Section::Graph $GR" -bd 1 -variable Graph::Section::Section${GR}::Graph(ZType) -value PRESSURE
         pack $Parent.scale.type.grid $Parent.scale.type.pres -side left -fill x -expand True
      pack $Parent.scale.type -side top -padx 2 -fill x
   pack $Parent.scale -side top -fill x -padx 5 -pady 5

   Bubble::Create $Parent.disp.mode       [lindex $Graph::Bubble(Viewport) $GDefs(Lang)]
   Bubble::Create $Parent.scale.valy      [lindex $Graph::Bubble(ScaleY) $GDefs(Lang)]
   Bubble::Create $Parent.res.list        [lindex $Graph::Bubble(Sample) $GDefs(Lang)]

   bind $Parent.res.cur         <Return>          "Graph::Section::Graph $GR True"
   bind $Parent.res.sc          <ButtonRelease-1> "Graph::Section::Graph $GR True"
   bind $Parent.scale.valy.list <Return>          "Graph::Section::Graph $GR"
}

#-------------------------------------------------------------------------------
# Nom      : <Graph::Section::ItemAdd>
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

proc Graph::Section::ItemAdd { GR Item } {

   upvar #0 Graph::Section::Section${GR}::Data  data

   if { [lsearch -exact $data(Items) $Item]==-1 } {
      set id [$data(Canvas) create text -100 -100  -tags "$Page::Data(Tag)$GR CVTEXT GRAPHUPDATE$GR" -text $Item -anchor nw -justify left]

      graphitem create $Item
      graphitem configure $Item -desc $id

      lappend data(Items) $Item
      Graph::Section::ItemDefault $GR $Item

      $data(Canvas) itemconfigure GRAPH$GR -item $data(Items)
   }
}

#-------------------------------------------------------------------------------
# Nom      : <Graph::Section::ItemDefault>
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

proc Graph::Section::ItemDefault { GR Item } {

   upvar #0 Graph::Section::Section${GR}::Data  data

   set Graph::Item(Outline)     ""
   set Graph::Item(FillColor)   #FFFFFF
   set Graph::Item(Tranparency) 100
   set Graph::Item(Width)       1
   set Graph::Item(Size)        0
   set Graph::Item(Value)       False
   set Graph::Item(Dash)        ""
   set Graph::Item(Type)        LINE
   set Graph::Item(Icon)        ""
   set Graph::Item(Bitmap)      ""
   set Graph::Item(Stipple)     -1
   set Graph::Item(Image)       ""

   Graph::ItemConfigure $GR Section $Item
}

#-------------------------------------------------------------------------------
# Nom      : <Graph::Section::ItemDel>
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

proc Graph::Section::ItemDel { GR Item } {

   upvar #0 Graph::Section::Section${GR}::Data  data

   if { [set idx [lsearch -exact $data(Items) $Item]]!=-1 } {
      set data(Items) [lreplace $data(Items) $idx $idx]
      $data(Canvas) itemconfigure GRAPH$GR -item $data(Items)
      $data(Canvas) delete [graphitem configure $Item -desc]

      graphitem free $Item
      fstdfield free GRAPHSECTION$Item

      FSTD::UnRegister GRAPHSECTION$Item

      set list [lindex [$data(FrameData).page.canvas itemconfigure $data(VP) -data] 4]
      if { [set idx [lsearch -exact $list GRAPHSECTION$Item]]!=-1 } {
         set list [lreplace $list $idx $idx]
         $data(FrameData).page.canvas itemconfigure $data(VP) -data $list
      }
   }
}

#-------------------------------------------------------------------------------
# Nom      : <Graph::Section::ItemDefine>
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

proc Graph::Section::ItemDefine { GR Pos Coords { Update True } } {

   upvar #0 Graph::Section::Section${GR}::Data  data

   if { $Pos=="" } {
      return
   }

   if { [info exists Graph::Section::Section${GR}::Data(Items$Pos)] } {
      foreach item [lrange $data(Items$Pos) [llength $data(Data)] end] {
         Graph::Section::ItemDel $GR $item
      }
   }

   if { [lsearch -exact $data(Pos) $Pos]==-1 } {
      lappend data(Pos) $Pos
   }

   set data(Items$Pos) {}
   set data(Pos$Pos)   $Coords
   set i -1

   Graph::Idle $GR Section

   foreach field $data(Data) {
      set item ${Pos}_Item[incr i]
      lappend data(Items$Pos) $item

      Graph::Section::ItemAdd $GR $item
      Graph::Section::ItemData $GR $Pos $item $field
   }

   Graph::Section::UpdateItems $data(FrameData) $GR
   Graph::Section::Graph $GR
   Graph::UnIdle $GR Section
}

#-------------------------------------------------------------------------------
# Nom      : <Graph::Section::ItemUnDefine>
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

proc Graph::Section::ItemUnDefine { GR Pos } {

   upvar #0 Graph::Section::Section${GR}::Data  data

   if { [set idx [lsearch -exact $data(Pos) $Pos]]!=-1 } {
      foreach item $data(Items$Pos) {
         Graph::Section::ItemDel $GR $item
      }

      set data(Pos) [lreplace $data(Pos)  $idx $idx]
      set Graph::Data(Pos) [lindex $data(Pos) end]
   }
}

#-------------------------------------------------------------------------------
# Nom      : <Graph::Section::ItemData>
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

proc Graph::Section::ItemData { GR Pos Item Data } {

   upvar #0 Graph::Section::Section${GR}::Data  data
   upvar #0 Graph::Section::Section${GR}::Graph graph

  if { [graphitem is $Item] } {
      if { [fstdfield is $Data] && [llength $data(Pos$Pos)] } {

         fstdfield vertical GRAPHSECTION$Item $Data $data(Pos$Pos)
         set graph(UnitY) [fstdfield stats GRAPHSECTION$Item -leveltype]

         FSTD::Register GRAPHSECTION$Item
         graphitem configure $Item -xaxis axisx$GR -yaxis axisy$GR -data GRAPHSECTION$Item
      } else {
         graphitem configure $Item -xaxis axisx$GR -yaxis axisy$GR -data ""
      }
   }
}

#-------------------------------------------------------------------------------
# Nom      : <Graph::Section::Update>
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

proc Graph::Section::Update { Frame { GR {} } } {
   variable Data

   if { ![llength $GR] } {
      set GR [Page::Registered All Graph::Section]
   }

   foreach gr $GR {

      upvar #0 Graph::Section::Section${gr}::Data  data

      if { $Graph::Data(Link$gr) && $data(FrameData)==$Frame } {

         $data(FrameData).page.canvas configure -cursor watch
         $data(Canvas) configure -cursor watch
         update idletasks

         #----- Recuperer les donnees

        if { [Page::Registered All Viewport $data(VP)]!=-1 } {
            if { [info exist Animator::Play(Data$data(VP))] } {
               Graph::Section::Data $gr $Animator::Play(Data$data(VP))
            } else {
               Graph::Section::Data $gr $Viewport::Data(Data$data(VP))
               set list $Viewport::Data(Data$data(VP))
            }
         }

         #----- Update des items

         foreach pos $data(Pos) {
            Graph::Section::ItemDefine $gr $pos $data(Pos$pos)
         }
         Graph::PosSet $gr Section
         Graph::Section::FieldShow $gr

         $data(Canvas) configure -cursor left_ptr
         $data(FrameData).page.canvas configure -cursor left_ptr
      }
   }
}

#-------------------------------------------------------------------------------
# Nom      : <Graph::Section::UpdateItems>
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

proc Graph::Section::UpdateItems { Frame { GR { } } } {
   global GDefs
   variable Data
   variable Lbl

   if { ![llength $GR] } {
      set GR [Page::Registered All Graph::Section]
   }

   foreach gr $GR {

      upvar #0 Graph::Section::Section${gr}::Data  data

      if { $data(VP)!="" && $data(FrameData)==$Frame } {

         $Frame.page.canvas delete GRAPHSECTION$gr
         foreach pos $data(Pos) {
            if { [llength $data(Items$pos)] } {
               set id [graphitem configure [lindex $data(Items$pos) 0] -desc]
               set desc [lindex [$data(Canvas) itemconfigure $id -text] end]
               Graph::ItemPos $Frame $data(VP) $data(Pos$pos) "[lindex $Lbl(Title) $GDefs(Lang)]\n$desc" GRAPHSECTION$gr SAMPLE $data(Coords)
            }
         }
      }
   }
}

#-------------------------------------------------------------------------------
# Nom      : <Graph::Section::Data>
# Creation : Octobre 2002 - J.P. Gauthier - CMC/CMOE -
#
# But      : Calculer un profil aux coordonees specifiees.
#
# Parametres :
#   <GR>     : Identificateur du Graph
#   <Data>   : Donnees a utilise
#
#-------------------------------------------------------------------------------

proc Graph::Section::Data { GR Data } {
   global GDefs
   variable Msg

   upvar #0 Graph::Section::Section${GR}::Data  data
   upvar #0 Graph::Section::Section${GR}::Graph graph

   SPI::Progress 0 [lindex $Msg(Reading) $GDefs(Lang)]

   #----- Recuperer les champs correspondants du viewport actif

   set data(Data)   {}

   set fields {}
   foreach field $Data {
      if { [fstdfield is $field] } {
         set grtyp [fstdfield define $field -GRTYP]
         if { $grtyp!="V" && $grtyp!="X"  && $grtyp!="Y" } {
            fstdfield readcube $field
            if { [fstdfield define $field -NK]>1 } {
               lappend fields $field
            }
         }
      }
   }
   SPI::Progress 0 ""

   #----- Applique le calcul MACRO au cubes de donnees

   set data(Data) [FieldCalc::Operand $data(VP) $fields]
   SPI::Progress 0
}

#----------------------------------------------------------------------------
# Nom      : <Graph::Section::Resolution>
# Creation : Janvier 2007 - J.P. Gauthier - CMC/CMOE
#
# But      : Determiner la resolution optimale du sampling des pointts de coupes.
#
# Parametres :
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc Graph::Section::Resolution { } {

   set GR $Graph::Data(Graph)
   upvar #0 Graph::Section::Section${GR}::Data  data

   if { $data(VP)=="" || $data(Field)=="" } {
      return
   }

   if { $data(ResBest) } {
      set i [expr int([fstdfield define $data(Field) -NI]/2)]
      set j [expr int([fstdfield define $data(Field) -NJ]/2)]

      if { [fstdfield define $data(Field) -GRTYP]=="R" } {
         set data(Res) 1000
      } else {
         #----- Calculate distances in gridpoint for grid projection otherwise, use meters
         if { [projection configure $data(FrameData) -type]=="grid" } {
            set data(Res) 1
         } else {
            set c0 [fstdfield stats $data(Field) -gridpoint $i $j]
            set c1 [fstdfield stats $data(Field) -gridpoint [incr i] $j]
            eval set data(Res) \[$data(VP) -distll $c0 $c1 0.0\]
         }
      }
      set data(Res) [format "%0.0f" $data(Res)]
   }
}

#----------------------------------------------------------------------------
# Nom      : <Graph::Section::VertexAdd>
# Creation : Ocotbre 2002 - J.P. Gauthier - CMC/CMOE
#
# But      : Ajout d'un point a la coupe.
#
# Parametres :
#  <Frame>   : Identificateur de Page
#  <VP>      : Identificateur du Viewport
#  <X>       : Coordonnee X de la souris
#  <Y>       : Coordonnee Y de la souris
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc Graph::Section::VertexAdd { Frame VP X Y } {

   set GR $Graph::Data(Graph)
   upvar #0 Graph::Section::Section${GR}::Data  data

   if { $VP==-1 } {
      return
   }

   if { $VP!=$data(VP) } {
      set data(VP)        $VP
      set data(FrameData) $Frame
      Graph::Section::Update $Frame $Graph::Data(Graph)
   }
   set data(Field) [lindex [Viewport::Assigned $Frame $VP fstdfield] 0]

   if { $VP==-1 || $data(Field)=="" } {
      return
   }

   #----- Si la grille et le vertex est valide on l'ajoute a la liste

   set grtyp [fstdfield define $data(Field) -GRTYP]
   if { $grtyp!="V" && $grtyp!="X" && $Viewport::Map(LatCursor)>-999 && $Viewport::Map(LonCursor)>-999 } {

      Graph::Section::Resolution
      lappend data(Coords) $Viewport::Map(LatCursor) $Viewport::Map(LonCursor)

      #----- Afficher la base de la coupes et en recuperer les coordonnees lat-lon

      Graph::Section::ItemDefine $Graph::Data(Graph) $Graph::Data(Pos) [Graph::Section::Sample $Graph::Data(Graph) $data(VP) $data(Coords)]
   }
}

#----------------------------------------------------------------------------
# Nom      : <Graph::Section::VertexDelete>
# Creation : Octobre 2002 - J.P. Gauthier - CMC/CMOE
#
# But      :Suppression d'un point a la coupe.
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

proc Graph::Section::VertexDelete { Frame VP } {

   set GR $Graph::Data(Graph)
   upvar #0 Graph::Section::Section${GR}::Data  data

   if { $VP!=-1 } {
      set data(Coords) [lreplace $data(Coords) end-1 end]

      Graph::Section::ItemDefine $Graph::Data(Graph) $Graph::Data(Pos) [Graph::Section::Sample $Graph::Data(Graph) $data(VP) $data(Coords)]
   }
   $data(Canvas) delete VERTEXFOLLOW
}

#----------------------------------------------------------------------------
# Nom      : <Graph::Section::VertexFollow>
# Creation : Octobre 2002 - J.P. Gauthier - CMC/CMOE
#
# But      : Affiche une ligne entre le dernier vertex creer et la position du
#            curseur de la souris.
#
# Parametres :
#  <Frame>   : Identificateur de Page
#  <VP>      : Identificateur du Viewport
#  <X>       : Coordonnee X de la souris
#  <Y>       : Coordonnee Y de la souris
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc Graph::Section::VertexFollow { Frame VP X Y Scan } {
   global GDefs
   variable Lbl

   set GR $Graph::Data(Graph)
   upvar #0 Graph::Section::Section${GR}::Data  data

   if { $VP==-1 } {
      if { $data(VP)=="" } {
         return
      } else {
         set VP $data(VP)
      }
   }

   if { $data(FrameData)!="" && [llength $data(Items$Graph::Data(Pos))] } {
      set data(FCoords) $data(Coords)

      if { $Viewport::Map(LatCursor)>-999 && $Viewport::Map(LonCursor)>-999 } {
         lappend data(FCoords) $Viewport::Map(LatCursor) $Viewport::Map(LonCursor)
      }

      $Frame.page.canvas delete GRAPHSECTION$Graph::Data(Graph)
      set coords [lrange [Graph::Section::Sample $Graph::Data(Graph) $VP $data(FCoords)] 0 end-4]
      set id [graphitem configure [lindex $data(Items$Graph::Data(Pos)) 0] -desc]
      set desc [lindex [$data(Canvas) itemconfigure $id -text] end]
      Graph::ItemPos $Frame $VP $coords "[lindex $Lbl(Title) $GDefs(Lang)]\n$desc" GRAPHSECTION$GR SAMPLE $data(FCoords)

      if { $Scan && [llength $data(FCoords)]>2 } {
         Graph::Section::ItemDefine $Graph::Data(Graph) $Graph::Data(Pos) $coords
      }
   }
}

#----------------------------------------------------------------------------
# Nom      : <Graph::Section::Sample>
# Creation : Octobre 2002 - J.P. Gauthier - CMC/CMOE
#
# But      : Calculer le path de coupe et points intermediaire selon la resolution.
#
# Parametres :
#  <GR>      : Identificateur du graph
#  <VP>      : Identificateur du Viewport
#  <Coord>   : Liste des coordonnee
#  <Res>     : Resolution des points intermediaire (Defaut 0 = aucun)
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc Graph::Section::Sample  { GR VP Coord { Res 0 } } {

   upvar #0 Graph::Section::Section${GR}::Data  data

   if { [llength $Coord]==2 } {
      return $Coord
   }

   set res  [expr $Res==0?$data(Res):$Res]
   set plat [lindex $Coord 0]
   set plon [lindex $Coord 1]

   set coords {}
   set data(DCoords) { 0 }

   foreach { lat lon } $Coord {

      if { $plat!=$lat || $plon!=$lon } {
         set coords [concat $coords [projection function $data(FrameData) -path [list $plat $plon $lat $lon] $res]]
         lappend data(DCoords) [expr [llength $coords]/2-1]
      }
      set plat $lat
      set plon $lon
   }

   return $coords
}

#-------------------------------------------------------------------------------
# Nom      : <Graph::Section::FieldShow>
# Creation : Octobre 2002 - J.P. Gauthier - CMC/CMOE -
#
# But      : Mise a jour de l'affichage des coupes.
#
# Parametres :
#   <GR>     : Identificateur du Graph
#
#-------------------------------------------------------------------------------

proc Graph::Section::FieldShow { GR } {

   upvar #0 Graph::Section::Section${GR}::Data  data

   if { $data(FrameData)!="" } {

      set list [lindex [$data(FrameData).page.canvas itemconfigure $data(VP) -data] 4]
      set did 0

      foreach item $data(Items) {
         set fld [graphitem configure $item -data]
         set idx [lsearch -exact $list $fld]

         if { $data(Proj) } {
            if { $idx==-1 && [fstdfield is $fld] } {
               lappend list $fld
               set did 1
            }
         } else {
            if { $idx!=-1 } {
               set list [lreplace $list $idx $idx]
               set did 1
            }
         }
      }
      if { $did } {
         $data(FrameData).page.canvas itemconfigure $data(VP) -frame 0 -data $list
      }
   }
}
