#===============================================================================
# Environnement Canada
# Centre Meteorologique Canadian
# 2100 Trans-Canadienne
# Dorval, Quebec
#
# Projet   : Librairie d'objects visuel interactifs
# Fichier  : Graph_Hovmoller.tcl
# Creation : Octobre 2003 - J.P. Gauthier - CMC/CMOE
#
# Description: Ce package s'occupe de l'affichage et de la manipulation de graphs
#              de serie temporelle
#
# Fonctions:
#
#    Graph::Hovmoller::Create       { Frame X0 Y0 Width Height Active Full }
#    Graph::Hovmoller::Coord        { Frame GR X Y }
#    Graph::Hovmoller::Clean        { GR }
#    Graph::Hovmoller::DrawInit     { Frame VP }
#    Graph::Hovmoller::Draw         { Frame VP }
#    Graph::Hovmoller::DrawDone     { Frame VP }
#    Graph::Hovmoller::MoveInit     { Frame VP }
#    Graph::Hovmoller::Move         { Frame VP }
#    Graph::Hovmoller::MoveDone     { Frame VP }
#    Graph::Hovmoller::Graph        { GR }
#    Graph::Hovmoller::Init         { Frame }
#    Graph::Hovmoller::Params       { Parent GR }
#    Graph::Hovmoller::ItemAdd      { GR Item }
#    Graph::Hovmoller::ItemDefault  { GR Item }
#    Graph::Hovmoller::ItemDel      { GR Item }
#    Graph::Hovmoller::ItemDefine   { GR Pos Coords { Update True } }
#    Graph::Hovmoller::ItemUnDefine { GR Pos }
#    Graph::Hovmoller::ItemData     { GR Pos Item Data }
#    Graph::Hovmoller::Update       { Frame { GR {} } }
#    Graph::Hovmoller::UpdateItems  { Frame { GR { } } }
#    Graph::Hovmoller::Data         { GR { Data { } } { Files { } } }
#
#===============================================================================

namespace eval Graph::Hovmoller { } {
   variable Lbl
   variable Msg

   set Lbl(Title)     { "Hovmoller" "Hovmoller" }

   set Msg(Reading)   { "Lecture des données" "Reading data" }
}

#----------------------------------------------------------------------------
# Nom      : <Graph::Hovmoller::Create>
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

proc Graph::Hovmoller::Create { Frame X0 Y0 Width Height Active Full } {
   global GDefs
   variable Data
   variable Lbl

   set gr [Graph::Hovmoller::Init $Frame]
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
   set Graph::Data(ToolMode$gr) Draw       ;#Mode de selection
   set Graph::Data(Type$gr)     Hovmoller  ;#Type de graph

   upvar #0 Graph::Hovmoller::Hovmoller${gr}::Data  data
   upvar #0 Graph::Hovmoller::Hovmoller${gr}::Graph graph

   set data(Canvas)    $Frame.page.canvas
   set data(Frame)     $Frame

   $data(Canvas) bind GRAPHUPDATE$gr <Any-KeyRelease> "$data(Canvas) itemconfigure $gr -update True"

   set id [$data(Canvas) create text $X0 $Y0  -tags "$tag CVTEXT GRAPHUPDATE$gr" -text [lindex $Lbl(Title) $GDefs(Lang)] \
      -font $Graph::Font(Graph) -fill black -anchor nw -justify center]
   $data(Canvas) create graph -x $X0 -y $Y0 -width $Width -height $Height -anchor nw -xlegend 5 -ylegend 5 -command $gr \
       -fg black -bg $Graph::Color(Frame) -fill $Graph::Color(Graph) -tags "$tag $gr" -font $Graph::Font(Graph) -title $id \
       -legend False -title $id
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
      Graph::Hovmoller::Update $data(FrameData) $gr
   } else {
      set data(VP)        ""
      set data(FrameData) ""
   }

   Graph::Activate $Frame $gr Hovmoller
   Graph::Mode Hovmoller $gr True
   Graph::PosAdd $gr Hovmoller

   #----- Creer les fonction du mode actif

   if { $Active } {
      Page::ActiveWrapper Graph $Frame $gr $X0 $Y0 [expr $Width+$X0] [expr $Height+$Y0] Hovmoller
   }
   Page::Register $Frame Graph::Hovmoller $gr

   return $gr
}

#-------------------------------------------------------------------------------
# Nom      : <Graph::Hovmoller::Coord>
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

proc Graph::Hovmoller::Coord { Frame GR X Y } {
   global   GDefs

   upvar #0 Graph::Hovmoller::Hovmoller${GR}::Data  data

   set Page::Data(Coord) ""
   set Page::Data(Value) ""

   if  { [llength [set items [lindex [$data(Canvas) itemconfigure $GR -item] end]]] } {
      set coords [$GR -unproject $X $Y False [lindex $items 0]]

      if { [llength $data(Dates)] && [llength $coords]>=2 } {
         catch {
            set idx  [lindex $coords 1]
            set sec0 [lindex $data(Dates) [expr int($idx)]]
            set sec1 [lindex $data(Dates) [expr int($idx)+1]]
            set sec  [expr $sec0+($idx-int($idx))*($sec1-$sec0)]
            set date [DateStuff::StringDateFromSeconds [expr $sec>1e31?0:$sec<1e-32?0:$sec] $GDefs(Lang)]
            set Page::Data(Coord) "$date"

            foreach item $items {
               set field [graphitem configure $item -data]
               if { [fstdfield is $field] } {
                  append Page::Data(Value) "[fstdfield configure $field -desc]:[FSTD::FieldFormat $field [lindex $coords 2]] "
               }
            }
         }
      }
   }
}

#-------------------------------------------------------------------------------
# Nom      : <Graph::Hovmoller::Clean>
# Creation : Octobre 2002 - J.P. Gauthier - CMC/CMOE -
#
# But      : Supprimer les donnees associees aux coordonees.
#
# Parametres :
#   <GR>     : Identificateur du Graph
#
#-------------------------------------------------------------------------------

proc Graph::Hovmoller::Clean { GR } {

   upvar #0 Graph::Hovmoller::Hovmoller${GR}::Data  data

   foreach item $data(Data) {
      foreach field $data(Data$item) {
         fstdfield free [lindex $field 1]
      }
   }
}

#-------------------------------------------------------------------------------
# Nom      : <Graph::Hovmoller::Graph>
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

proc Graph::Hovmoller::Graph { GR { Pos False } } {
   global GDefs
   variable Data

   upvar #0 Graph::Hovmoller::Hovmoller${GR}::Data  data
   upvar #0 Graph::Hovmoller::Hovmoller${GR}::Graph graph

   if { ![llength $data(Items)] } {
      return
   }

   if { $Pos } {
      Graph::Hovmoller::ItemDefine $Graph::Data(Graph) $Graph::Data(Pos) [Graph::Hovmoller::Sample $Graph::Data(Graph) $data(VP) $data(Coords)]
   }
   $data(Canvas) config -cursor watch
   update idletasks

   #----- Recalculer les valeurs
   set data(XMin)  1e200
   set data(XMax) -1e200
   set data(YMin)  1e200
   set data(YMax) -1e200
   set data(Dates) {}
   set yincr 0
   set xincr 0
   set mod True

   #----- Afficher le graph

   foreach item $data(Items) {
      set data(Dates) [concat $data(Dates) [vector get $item.Y]]
      set data(XMin)   0
      if { [fstdfield is GRAPHHOVMOLLER$item] } {
         set data(XMax)  [expr [fstdfield define GRAPHHOVMOLLER$item -NI]-1]
      }
   }

   set data(Dates) [lsort -unique -real -increasing $data(Dates)]
   set data(YMin)  [lindex $data(Dates) 0]
   set data(YMax)  [lindex $data(Dates) end]

   if { ![llength $data(Dates)] } {
      return
   }

   if { $data(Date0)!="" } {
      set data(YMin) [clock scan $data(Date0) -gmt True]
   }
   if { $data(Date1)!="" } {
      set data(YMax) [clock scan $data(Date1) -gmt True]
   }

   #----- Verifier la selection de l'usager
   if { ![llength $graph(YInter)] } {
      set dates $data(Dates)
   } else {
      set dates $graph(YInter)
      set data(YMin) [lindex $graph(YInter) 0]
      set data(YMax) [lindex $graph(YInter) end]
   }

   set graph(XInter) $data(DCoords)
   set graph(XLabel) [lrange $Graph::Graph(Identitys) 0 [expr [llength $data(DCoords)]-1]]

   if { $graph(XFormat)=="NONE" } {
     set diff [expr $data(YMax)-$data(YMin)]

      if { $diff <= 120 } {
         set graph(UnitY) "[lindex $Graph::Lbl(Since) $GDefs(Lang)] [DateStuff::StringDateFromSeconds $data(YMin) $GDefs(Lang)] ([lindex $Graph::Lbl(Sec) $GDefs(Lang)])"
         set data(Time)  S
      } elseif { $diff <=7200 } {
         set graph(UnitY) "[lindex $Graph::Lbl(Since) $GDefs(Lang)] [DateStuff::StringDateFromSeconds $data(YMin) $GDefs(Lang)] ([lindex $Graph::Lbl(Min) $GDefs(Lang)])"
         set data(Time)  M
      } elseif { $diff <=1296000 } {
         set graph(UnitY) "[lindex $Graph::Lbl(Since) $GDefs(Lang)] [DateStuff::StringDateFromSeconds $data(YMin) $GDefs(Lang)] ([lindex $Graph::Lbl(Hour) $GDefs(Lang)])"
         set data(Time)  H
      } else {
         set graph(UnitY) "[lindex $Graph::Lbl(Since) $GDefs(Lang)] [DateStuff::StringDateFromSeconds $data(YMin) $GDefs(Lang)] ([lindex $Graph::Lbl(Day) $GDefs(Lang)])"
         set data(Time)  D
      }

   } else {
      set data(Time)    DATE
      set graph(UnitY)  [lindex $Graph::Lbl(Date) $GDefs(Lang)]
      set graph(YAngle) 45
   }

   set yinter {}
   set ydates {}
   set i -1

   foreach date $dates {
      lappend ydates [Graph::TimeFormat $date $data(Time) $data(YMin)]
      lappend yinter [incr i]
   }
   set data(YMin)  0
   set data(YMax)  [expr [fstdfield define GRAPHHOVMOLLER$item -NJ]-1]

   if { [llength $graph(ZYInter)] } {
      set data(YMin) [lindex $graph(ZYInter) 0]
      set data(YMax) [lindex $graph(ZYInter) 1]
      set mod False
   }
   if { [llength $graph(ZXInter)] } {
      set data(XMin) [lindex $graph(ZXInter) 0]
      set data(XMax) [lindex $graph(ZXInter) 1]
      set mod False
   }

   set id [graphaxis configure axisx$GR -unit]
   if { $Graph::Data(Update) } {
      $data(Canvas) itemconfigure $id -text $graph(UnitX)
   }
   $data(Canvas) itemconfigure $id -font $Graph::Font(Axis) -fill $Graph::Color(Axis)
   graphaxis configure axisx$GR -type $graph(XScale) -modulo $mod -min $data(XMin) -max $data(XMax) -intervals $graph(XInter) -labels $graph(XLabel) \
      -font $Graph::Font(Axis) -gridcolor $Graph::Grid(XColor)  -dash $Graph::Grid(XDash) -gridwidth $Graph::Grid(XWidth) -color $Graph::Color(Axis) -angle $graph(XAngle) \
      -format $graph(XFormat) -decimal $graph(XDecimals)

   set id [graphaxis configure axisy$GR -unit]
   if { $Graph::Data(Update) } {
      $data(Canvas) itemconfigure $id -text $graph(UnitY)
   }
   $data(Canvas) itemconfigure $id -font $Graph::Font(Axis) -fill $Graph::Color(Axis)
   graphaxis configure axisy$GR -type $graph(YScale) -modulo $mod -min $data(YMin) -max $data(YMax) -intervals $yinter -labels $ydates \
      -font $Graph::Font(Axis) -gridcolor $Graph::Grid(YColor)  -dash $Graph::Grid(YDash) -gridwidth $Graph::Grid(YWidth) -color $Graph::Color(Axis) -angle $graph(YAngle) \
      -format $graph(YFormat) -decimal $graph(YDecimals)

   set id [lindex [$data(Canvas) itemconfigure $GR -title] end]
   $data(Canvas) itemconfigure $id -font $Graph::Font(Graph) -fill $Graph::Color(FG)
   $data(Canvas) itemconfigure $GR -item $data(Items) -bd $Graph::Width(Frame) \
      -fg $Graph::Color(FG) -bg $Graph::Color(BG) -fill $Graph::Color(Fill) -font $Graph::Font(Graph)

   update idletasks
   $data(Canvas) config -cursor left_ptr
}

#----------------------------------------------------------------------------
# Nom      : <Graph::Hovmoller::Init>
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

proc Graph::Hovmoller::Init { Frame } {
   global   GDefs
   variable Data

   set gr GR[incr Graph::Data(Nb)]

   namespace eval Hovmoller$gr {
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
      set Data(Date0)           ""
      set Data(Date1)           ""

      #----- Constantes relatives au Graph

      set Graph(YScale)    LINEAR                                               ;#Type d'echelle en Y
      set Graph(XScale)    LINEAR                                               ;#Type d'echelle en Y
      set Graph(UnitY)     "[lindex $Graph::Lbl(Unit) $GDefs(Lang)] Y"     ;#Descriptif de l'echelle des valeur en Y
      set Graph(UnitX)     "[lindex $Graph::Lbl(Pos) $GDefs(Lang)]"             ;#Descriptif de l'echelle des valeur en X
      set Graph(XInter)    ""               ;#Liste des niveau specifie par l'usager
      set Graph(YInter)    ""               ;#Liste des niveau specifie par l'usager
      set Graph(ZXInter)   ""               ;#Liste des Niveaux (Mode Zoom)
      set Graph(ZYInter)   ""               ;#Liste des Niveaux (Mode Zoom)
      set Graph(XFormat)   NONE
      set Graph(YFormat)   NONE
      set Graph(XDecimals) 0
      set Graph(YDecimals) 0
      set Graph(XAngle)    0
      set Graph(YAngle)    0                                                   ;
   }
   return $gr
}

#----------------------------------------------------------------------------
# Nom      : <Graph::Hovmoller::Params>
# Creation : Octobre 2003 - J.P. Gauthier - CMC/CMOE
#
# But      : Widgets des parametres
#
# Parametres :
#   <Parent> : Fenetre parent
#   <GR>     : Identificateur du Graph
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc Graph::Hovmoller::Params { Parent GR } {
   global   GDefs

   upvar #0 Graph::Hovmoller::Hovmoller${GR}::Data data

   labelframe $Parent.params -text [lindex $Graph::Lbl(Section) $GDefs(Lang)]
      frame $Parent.params.res
         label $Parent.params.res.lbl -text [lindex $Graph::Lbl(Res) $GDefs(Lang)] -width 14 -anchor w
         entry $Parent.params.res.val -textvariable Graph::Hovmoller::Hovmoller${GR}::Data(Res) -width 8 -bd 1 -bg $GDefs(ColorLight)
         checkbutton $Parent.params.res.auto -text * -indicatoron false -bd 1 -variable Graph::Hovmoller::Hovmoller${GR}::Data(ResBest) \
            -onvalue True -offvalue False -command "Graph::Hovmoller::Resolution; Graph::Hovmoller::Graph $GR True"
         pack $Parent.params.res.lbl -side left
         pack $Parent.params.res.val -side left -fill x -expand True
         pack $Parent.params.res.auto -side left
      pack $Parent.params.res -side top -fill x
   pack $Parent.params -side top -fill x -padx 5 -pady 5

   Graph::ParamsAxis $Parent $GR Hovmoller X MARK
   Graph::ParamsAxis $Parent $GR Hovmoller Y TIME

   Bubble::Create $Parent.params.res.val   $Graph::Bubble(Sample)

   bind $Parent.params.res.val <Return> "Graph::Hovmoller::Graph $GR True"
}

#-------------------------------------------------------------------------------
# Nom      : <Graph::Hovmoller::ItemAdd>
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

proc Graph::Hovmoller::ItemAdd { GR Item } {

   upvar #0 Graph::Hovmoller::Hovmoller${GR}::Data  data

   if { [lsearch -exact $data(Items) $Item]==-1 } {
      vector free   $Item
      vector create $Item
      vector dim    $Item { X Y }

      set id [$data(Canvas) create text -100 -100  -tags "$Page::Data(Tag)$GR CVTEXT GRAPHUPDATE$GR" -text $Item -anchor nw -justify left]

      graphitem create $Item
      graphitem configure $Item -desc $id

      lappend data(Items) $Item
      Graph::Hovmoller::ItemDefault $GR $Item

      $data(Canvas) itemconfigure $GR -item $data(Items)
   }
}

#-------------------------------------------------------------------------------
# Nom      : <Graph::Hovmoller::ItemDefault>
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

proc Graph::Hovmoller::ItemDefault { GR Item } {

   upvar #0 Graph::Hovmoller::Hovmoller${GR}::Data  data

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

   Graph::ItemConfigure $GR Hovmoller $Item
}

#-------------------------------------------------------------------------------
# Nom      : <Graph::Hovmoller::ItemDel>
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

proc Graph::Hovmoller::ItemDel { GR Item } {

   upvar #0 Graph::Hovmoller::Hovmoller${GR}::Data  data

   if { [set idx [lsearch -exact $data(Items) $Item]]!=-1 } {
      set data(Items) [lreplace $data(Items) $idx $idx]

      $data(Canvas) itemconfigure $GR -item $data(Items)
      $data(Canvas) delete [graphitem configure $Item -desc]

      vector    free $Item
      graphitem free $Item
      fstdfield free GRAPHHOVMOLLER$Item

      FSTD::UnRegister GRAPHHOVMOLLER$Item
   }
}

#-------------------------------------------------------------------------------
# Nom      : <Graph::Hovmoller::ItemDefine>
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

proc Graph::Hovmoller::ItemDefine { GR Pos Coords { Update True } } {

   upvar #0 Graph::Hovmoller::Hovmoller${GR}::Data  data

   if { $Pos=="" } {
      return
   }

   if { [info exists Graph::Hovmoller::Hovmoller${GR}::Data(Items$Pos)] } {
      foreach item [lrange $data(Items$Pos) [llength $data(Data)] end] {
         Graph::Hovmoller::ItemDel $GR $item
      }
   }

   if { [lsearch -exact $data(Pos) $Pos]==-1 } {
      lappend data(Pos) $Pos
   }
   set data(Items$Pos) {}
   set data(Pos$Pos)   $Coords
   set i -1

   Graph::Idle $GR Hovmoller

   foreach field $data(Data) {
      set item ${Pos}_Item[incr i]
      lappend data(Items$Pos) $item

      Graph::Hovmoller::ItemAdd $GR $item
      Graph::Hovmoller::ItemData $GR $Pos $item $field
   }

   Graph::Hovmoller::UpdateItems $data(FrameData) $GR
   Graph::Hovmoller::Graph $GR
   Graph::UnIdle $GR Hovmoller
}

#-------------------------------------------------------------------------------
# Nom      : <Graph::Hovmoller::ItemUnDefine>
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

proc Graph::Hovmoller::ItemUnDefine { GR Pos } {

   upvar #0 Graph::Hovmoller::Hovmoller${GR}::Data  data

   if { [set idx [lsearch -exact $data(Pos) $Pos]]!=-1 } {
      foreach item $data(Items$Pos) {
         Graph::Hovmoller::ItemDel $GR $item
      }

      set data(Pos) [lreplace $data(Pos)  $idx $idx]
      set Graph::Data(Pos) [lindex $data(Pos) end]
   }
}

#-------------------------------------------------------------------------------
# Nom      : <Graph::Hovmoller::ItemData>
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

proc Graph::Hovmoller::ItemData { GR Pos Item Data } {

   upvar #0 Graph::Hovmoller::Hovmoller${GR}::Data  data
   upvar #0 Graph::Hovmoller::Hovmoller${GR}::Graph graph

  if { [graphitem is $Item] } {
      if { [fstdfield is $Data] && [llength $data(Pos$Pos)] } {

         foreach field $data(Data$Data) {
            lappend fields [lindex $field 1]
            vector append $Item.Y [fstdstamp toseconds [fstdfield define [lindex $field 1] -DATEV]]
         }
         fstdfield vertical GRAPHHOVMOLLER$Item $fields $data(Pos$Pos)
         FSTD::Register GRAPHHOVMOLLER$Item
         graphitem configure $Item -xaxis axisx$GR -yaxis axisy$GR -data GRAPHHOVMOLLER$Item
      } else {
         graphitem configure $Item -xaxis axisx$GR -yaxis axisy$GR -data ""
      }
   }
}

#-------------------------------------------------------------------------------
# Nom      : <Graph::Hovmoller::Update>
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

proc Graph::Hovmoller::Update { Frame { GR {} } } {
   variable Data

   if { ![llength $GR] } {
      set GR [Page::Registered All Graph::Hovmoller]
   }

   foreach gr $GR {

      upvar #0 Graph::Hovmoller::Hovmoller${gr}::Data  data

      if { $data(FrameData)==$Frame } {

         catch {
            $data(FrameData).page.canvas configure -cursor watch
            $data(Canvas) configure -cursor watch
            update idletasks
         }
         #----- Recuperer les donnees

         if { [Page::Registered All Viewport $data(VP)]!=-1 } {
            Graph::Hovmoller::Data $gr [Viewport::Assigned $Viewport::Data(Frame$data(VP)) $data(VP) fstdfield]
         }

         #----- Update des items

         foreach pos $data(Pos) {
            Graph::Hovmoller::ItemDefine $gr $pos $data(Pos$pos)
         }
         Graph::PosSet $gr Hovmoller

         catch {
            $data(Canvas) configure -cursor left_ptr
            $data(FrameData).page.canvas configure -cursor left_ptr
         }
      }
   }
}

#-------------------------------------------------------------------------------
# Nom      : <Graph::Hovmoller::UpdateItems>
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

proc Graph::Hovmoller::UpdateItems { Frame { GR { } } } {
   global GDefs
   variable Data
   variable Lbl

   if { ![llength $GR] } {
      set GR [Page::Registered All Graph::Hovmoller]
   }

   foreach gr $GR {

      upvar #0 Graph::Hovmoller::Hovmoller${gr}::Data  data

      if { $data(VP)!="" && $data(FrameData)==$Frame } {

         $Frame.page.canvas delete GRAPHHOVMOLLER$gr

         foreach pos $data(Pos) {
            if { [llength $data(Items$pos)] } {
               set id [graphitem configure [lindex $data(Items$pos) 0] -desc]
               set desc [lindex [$data(Canvas) itemconfigure $id -text] end]
               Graph::ItemPos $Frame $data(VP) $data(Pos$pos) "[lindex $Lbl(Title) $GDefs(Lang)]\n$desc" GRAPHHOVMOLLER$gr SAMPLE $data(Coords)
            }
         }
      }
   }
}

#-------------------------------------------------------------------------------
# Nom      : <Graph::Hovmoller::Data>
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

proc Graph::Hovmoller::Data { GR { Data { } } { Files { } } } {
   global   GDefs
   variable Msg

   upvar #0 Graph::Hovmoller::Hovmoller${GR}::Data  data
   upvar #0 Graph::Hovmoller::Hovmoller${GR}::Graph graph

   SPI::Progress 0

   Graph::Hovmoller::Clean $GR

   SPI::Progress 5 [lindex $Msg(Reading) $GDefs(Lang)]

   #----- Recuperer la suite temporelle pour chaque champs

   set data(Data)   {}
   set data(ObsIds) {}
   set nb [expr 95.0/([llength $Data]+1)]
   set sec ""
   SPI::Progress +$nb [lindex $Msg(Reading) $GDefs(Lang)]

   foreach item $Data {

      if { ![llength $Files] } {
         if { [set box [lindex [fstdfield stats $item -tag] 2]]=="" } {
               continue
         }
         set fids [FieldBox::GetFID $box]
      } else {
         set fids $Files
      }

      if { $Graph::Data(IP3) } {
         set ip3 [fstdfield define $item -IP3]
      } else {
         set ip3 -1
      }

      set data(Data$item) [MetData::FindAll HOVMOLLER$GR$item $fids -1 [fstdfield define $item -ETIKET] [fstdfield define $item -IP1] \
         -1 $ip3 [fstdfield define $item -TYPVAR] [fstdfield define $item -NOMVAR]]

      #----- Set the interpolation degree to the same

      set interp [fstdfield configure $item -interpdegree]
      foreach field $data(Data$item) {
         fstdfield configure $field -interpdegree $interp
      }

      #---- Trier temporellement les champs

      set i 0
      foreach id $data(Data$item) {
         set sec [fstdstamp toseconds [fstdfield define $id -DATEV]]
         lset data(Data$item) $i "$sec $id"
         incr i
      }
      set data(Data$item) [lsort -integer -increasing -index 0 $data(Data$item)]
      lappend data(Data) $item

      SPI::Progress +$nb "[lindex $Msg(Reading) $GDefs(Lang)] $sec"
   }

   SPI::Progress 0
}

#----------------------------------------------------------------------------
# Nom      : <Graph::Hovmoller::Resolution>
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

proc Graph::Hovmoller::Resolution { } {

   set GR $Graph::Data(Graph)
   upvar #0 Graph::Hovmoller::Hovmoller${GR}::Data  data

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
# Nom      : <Graph::Hovmoller::VertexAdd>
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

proc Graph::Hovmoller::VertexAdd { Frame VP X Y } {

   set GR $Graph::Data(Graph)
   upvar #0 Graph::Hovmoller::Hovmoller${GR}::Data  data

   if { $VP==-1 } {
      return
   }

   if { $VP!=$data(VP) } {
      set data(VP)        $VP
      set data(FrameData) $Frame
      Graph::Hovmoller::Update $Frame $Graph::Data(Graph)
   }
   set data(Field) [lindex [Viewport::Assigned $Frame $VP fstdfield] 0]

   if { $VP==-1 || $data(Field)=="" } {
      return
   }

   #----- Si la grille et le vertex est valide on l'ajoute a la liste

   set grtyp [fstdfield define $data(Field) -GRTYP]
   if { $grtyp!="V" && $grtyp!="X" && $Viewport::Map(LatCursor)>-999 && $Viewport::Map(LonCursor)>-999 } {

      Graph::Hovmoller::Resolution
      lappend data(Coords) $Viewport::Map(LatCursor) $Viewport::Map(LonCursor)

      #----- Afficher la base de la coupes et en recuperer les coordonnees lat-lon

      Graph::Hovmoller::ItemDefine $Graph::Data(Graph) $Graph::Data(Pos) [Graph::Hovmoller::Sample $Graph::Data(Graph) $data(VP) $data(Coords)]
   }
}

#----------------------------------------------------------------------------
# Nom      : <Graph::Hovmoller::VertexDelete>
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

proc Graph::Hovmoller::VertexDelete { Frame VP } {

   set GR $Graph::Data(Graph)
   upvar #0 Graph::Hovmoller::Hovmoller${GR}::Data  data

   if { $VP!=-1 } {
      set data(Coords) [lreplace $data(Coords) end-1 end]

      Graph::Hovmoller::ItemDefine $Graph::Data(Graph) $Graph::Data(Pos) [Graph::Hovmoller::Sample $Graph::Data(Graph) $data(VP) $data(Coords)]
   }
   $data(Canvas) delete VERTEXFOLLOW
}

#----------------------------------------------------------------------------
# Nom      : <Graph::Hovmoller::VertexFollow>
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

proc Graph::Hovmoller::VertexFollow { Frame VP X Y Scan } {
   global GDefs
   variable Lbl

   set GR $Graph::Data(Graph)
   upvar #0 Graph::Hovmoller::Hovmoller${GR}::Data  data

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

      $Frame.page.canvas delete GRAPHHOVMOLLER$Graph::Data(Graph)
      set coords [lrange [Graph::Hovmoller::Sample $Graph::Data(Graph) $VP $data(FCoords)] 0 end-4]
      set id [graphitem configure [lindex $data(Items$Graph::Data(Pos)) 0] -desc]
      set desc [lindex [$data(Canvas) itemconfigure $id -text] end]
      Graph::ItemPos $Frame $VP $coords "[lindex $Lbl(Title) $GDefs(Lang)]\n$desc" GRAPHHOVMOLLER$GR SAMPLE $data(FCoords)

      if { $Scan && [llength $data(FCoords)]>2 } {
         Graph::Hovmoller::ItemDefine $Graph::Data(Graph) $Graph::Data(Pos) $coords
      }
   }
}

#----------------------------------------------------------------------------
# Nom      : <Graph::Hovmoller::Sample>
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

proc Graph::Hovmoller::Sample  { GR VP Coord { Res 0 } } {

   upvar #0 Graph::Hovmoller::Hovmoller${GR}::Data  data

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
