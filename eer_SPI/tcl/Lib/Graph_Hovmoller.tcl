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

   set Lbl(Title)     { "Hovmoller" "Hovmoller" }
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

proc Graph::Hovmoller::Create { Frame X0 Y0 Width Height Active Full { Link True } } {
   global GDefs
   variable Data
   variable Lbl

   set gr [Graph::Hovmoller::Init $Frame]
   set tag PAGE$gr

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

   if { $Viewport::Data(VP)!="" } {
      set data(VP)        $Viewport::Data(VP)
      set data(FrameData) $Viewport::Data(Frame$data(VP))
      Graph::Hovmoller::Update $data(FrameData) $gr
   }

   Graph::Activate $Frame $gr Hovmoller
   
   graphaxis create axisx$gr
   graphaxis create axisy$gr

   if { $Graph::Data(Link$gr) } {
      #----- Creation des unite de l'echelle

      set id [$data(Canvas) create text -100 -100  -tags "$tag CVTEXT GRAPHUPDATE$gr" -text $graph(UnitX) \
         -font $Graph::Font(Axis) -fill $Graph::Color(Axis) -anchor nw -justify center]
      graphaxis configure axisx$gr -font $Graph::Font(Axis) -color $Graph::Color(Axis) -gridcolor $Graph::Grid(XColor) \
         -dash $Graph::Grid(XDash) -position LL -width 1 -unit $id

      set id [$data(Canvas) create text -100 -100  -tags "$tag CVTEXT GRAPHUPDATE$gr" -text $graph(UnitY) \
         -font $Graph::Font(Axis) -fill $Graph::Color(Axis) -anchor nw -justify center]
      graphaxis configure axisy$gr -font $Graph::Font(Axis) -color $Graph::Color(Axis) -gridcolor $Graph::Grid(YColor) \
         -dash $Graph::Grid(YDash) -position LL -width 1 -unit $id

      Graph::Mode $gr Hovmoller True
      Graph::PosAdd $gr Hovmoller
   }
   
   #----- Creer les fonction du mode actif

   if { $Active } {
      Page::ActiveWrapper Graph $Frame $gr $X0 $Y0 [expr $Width+$X0] [expr $Height+$Y0] Hovmoller
   } elseif { $Full } {
      Page::ActiveFull Graph $Frame $gr $Full
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
   foreach field $data(Tmp) {
      fstdfield free $field
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
#
# Retour    :
#
# Remarque :
#
#-------------------------------------------------------------------------------

proc Graph::Hovmoller::Graph { GR } {
   global GDefs
   variable Data

   upvar #0 Graph::Hovmoller::Hovmoller${GR}::Data  data
   upvar #0 Graph::Hovmoller::Hovmoller${GR}::Graph graph

   if { ![llength $data(Items)] } {
      return
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
      if { [fstdfield is GRAPHSELECT$item] } {
         set data(XMax)  [expr [fstdfield define GRAPHSELECT$item -NI]-1]
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

   set yinter $dates
   set ydates {}

   if { $graph(YFormat)=="NONE" } {
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

      set yinter {}
      set i -1
      foreach date $dates {
         lappend ydates [Graph::TimeFormat $date $data(Time) $data(YMin)]
         lappend yinter [incr i]
      }
   } else {
      set data(Time)    DATE
      set graph(UnitY)  [lindex $Graph::Lbl(Date) $GDefs(Lang)]
   }

   set data(YMin)  0
   set data(YMax)  [expr [fstdfield define GRAPHSELECT$item -NJ]-1]

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

   set id [graphaxis configure axisx$GR -unit]
   if { $Graph::Data(Update) } {
      $data(Canvas) itemconfigure $id -text $graph(UnitX)
   }
   $data(Canvas) itemconfigure $id -font $Graph::Font(Axis) -fill $Graph::Color(Axis)
   graphaxis configure axisx$GR -type $graph(XScale) -modulo $mod -min $xmin -max $xmax -intervals $graph(XInter) -labels $graph(XLabel) \
      -font $Graph::Font(Axis) -gridcolor $Graph::Grid(XColor)  -dash $Graph::Grid(XDash) -gridwidth $Graph::Grid(XWidth) -color $Graph::Color(Axis) -angle $graph(XAngle) \
      -format $graph(XFormat) -decimal $graph(XDecimals)

   set id [graphaxis configure axisy$GR -unit]
   if { $Graph::Data(Update) } {
      $data(Canvas) itemconfigure $id -text $graph(UnitY)
   }
   $data(Canvas) itemconfigure $id -font $Graph::Font(Axis) -fill $Graph::Color(Axis)
   graphaxis configure axisy$GR -type $graph(YScale) -modulo $mod -min $ymin -max $ymax -intervals $yinter -labels $ydates \
      -font $Graph::Font(Axis) -gridcolor $Graph::Grid(YColor)  -dash $Graph::Grid(YDash) -gridwidth $Graph::Grid(YWidth) -color $Graph::Color(Axis) -angle $graph(YAngle) \
      -format $graph(YFormat) -decimal $graph(YDecimals) -relative True

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

      set Data(Items)     {}           ;#Liste des items
      set Data(Pos)       {}           ;#Liste des positions
      set Data(Data)      {}           ;#Liste des champs selectionnees
      set Data(Tmp)       {}           ;#Liste des champs temporaire
      set Data(Proj)      0            ;#Mode projection
      set Data(DCoords)   {}           ;#Liste des longueur des segment
      set Data(Levels)    {}           ;#Liste des niveaux
      set Data(Coords)    {}           ;#Liste des coordonnees de coupe
      set Data(Date0)     ""
      set Data(Date1)     ""
      set Data(Field)     ""           ;#Champs de donnees
      set Data(VP)        ""
      set Data(FrameData) ""

      #----- Constantes relatives au Graph

      set Graph(YScale)    LINEAR                                               ;#Type d'echelle en Y
      set Graph(XScale)    LINEAR                                               ;#Type d'echelle en Y
      set Graph(UnitY)     "[lindex $Graph::Lbl(Unit) $GDefs(Lang)] Y"          ;#Descriptif de l'echelle des valeur en Y
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
         entry $Parent.params.res.val -textvariable Graph::Data(Res) -width 8 -bd 1 -bg $GDefs(ColorLight)
         checkbutton $Parent.params.res.auto -text * -indicatoron false -bd 1 -variable Graph::Data(ResBest) \
            -onvalue True -offvalue False -command "Graph::VertexResolution Hovmoller $GR"
         pack $Parent.params.res.lbl -side left
         pack $Parent.params.res.val -side left -fill x -expand True
         pack $Parent.params.res.auto -side left
      pack $Parent.params.res -side top -fill x
   pack $Parent.params -side top -fill x -padx 5 -pady 5

   Graph::ParamsAxis $Parent $GR Hovmoller X MARK
   Graph::ParamsAxis $Parent $GR Hovmoller Y TIME
   Graph::ModeSelect LINE LINE NIL

   Bubble::Create $Parent.params.res.val   $Graph::Bubble(Sample)

   bind $Parent.params.res.val <Return> "Graph::VertexResolution Hovmoller $GR"
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

      set id [$data(Canvas) create text -100 -100  -tags "PAGE$GR CVTEXT GRAPHUPDATE$GR" -text $Item -anchor nw -justify left]

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
      fstdfield free GRAPHSELECT$Item

      FSTD::UnRegister GRAPHSELECT$Item
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
   set data(Coords$Pos) $Coords
   set data(Pos$Pos)    [Graph::VertexSample Hovmoller $GR $Coords]
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
         fstdfield vertical GRAPHSELECT$Item $fields $data(Pos$Pos)
         FSTD::Register GRAPHSELECT$Item
         graphitem configure $Item -xaxis axisx$GR -yaxis axisy$GR -data GRAPHSELECT$Item
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
            Graph::Hovmoller::Data $gr $Viewport::Data(Data$data(VP))
#            Graph::Hovmoller::Data $gr [Viewport::Assigned $Viewport::Data(Frame$data(VP)) $data(VP) fstdfield]
         }

         #----- Update des items

         foreach pos $data(Pos) {
            Graph::Hovmoller::ItemDefine $gr $pos $data(Coords$pos)
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

         $Frame.page.canvas delete GRAPHSELECT$gr

         foreach pos $data(Pos) {
            if { [llength $data(Items$pos)] } {
               set id [graphitem configure [lindex $data(Items$pos) 0] -desc]
               set desc [lindex [$data(Canvas) itemconfigure $id -text] end]
               Graph::ItemPos $Frame $data(VP) $data(Pos$pos) "[lindex $Lbl(Title) $GDefs(Lang)]\n$desc" GRAPHSELECT$gr LINE $data(Coords)
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

   upvar #0 Graph::Hovmoller::Hovmoller${GR}::Data  data
   upvar #0 Graph::Hovmoller::Hovmoller${GR}::Graph graph

   SPI::Progress 0

   Graph::Hovmoller::Clean $GR

   SPI::Progress 5 [lindex $Graph::Msg(Reading) $GDefs(Lang)]

   #----- Recuperer la suite temporelle pour chaque champs

   set data(Data)   {}
   set data(ObsIds) {}
   set nb     [expr 95.0/([llength $Data]+1)]
   set nbdata 0
   set sec    -1
   SPI::Progress +$nb [lindex $Graph::Msg(Reading) $GDefs(Lang)]

   foreach item $Data {

      if { [fstdfield is $item] } {

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
         eval lappend data(Tmp) $data(Data$item)

         #----- Check if number of time setp correspond when the calculatro is used
         if { $nbdata && [llength $data(Data$item)]!=$nbdata && [FieldCalc::IsOperand $data(VP)] } {
            Dialog::Error $data(Frame) $Graph::Error(NbData)
            break;
         }
         set nbdata [llength $data(Data$item)]

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
      }

      SPI::Progress +$nb "[lindex $Graph::Msg(Reading) $GDefs(Lang)] $sec"
   }

   #----- Applique le calcul MACRO au donnees
   if { [FieldCalc::IsOperand $data(VP)] } {

      #----- Loop on timesteps
      for { set n 0 } { $n < $nbdata } { incr n } {
         set lst {}

         foreach item $data(Data) {
            lappend lst [lindex $data(Data$item) $n 1]
         }
         #----- Apply expression to timestep
         set flds [FieldCalc::Operand $data(VP) $lst GRAPHTIME$n$data(VP)]

         #----- If first loop step, set data list
         if { $n==0 } {
            set datas $flds
            foreach f $datas {
               set data(Data$f) {}
            }
         }

         #----- Create per data time lists
         set i 0
         foreach f $datas {
            set id [lindex $flds $i]
            set sec [fstdstamp toseconds [fstdfield define $id -DATEV]]
            lappend data(Data$f) [list $sec $id]
            incr i
         }
      }
      set data(Data) $datas
   }

   SPI::Progress 0
}
