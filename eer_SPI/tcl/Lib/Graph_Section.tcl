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
#    Graph::Section::Clean        { GR }
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
#    Graph::Section::FieldShow    { GR }
#
#===============================================================================

namespace eval Graph::Section { } {
   variable Lbl

   set Lbl(Title)     { "Coupe verticale" "Vertical cross-section" }
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
   set Graph::Data(Type$gr)     Section    ;#Type de graph

   upvar #0 Graph::Section::Section${gr}::Data  data
   upvar #0 Graph::Section::Section${gr}::Graph graph

   set data(Canvas)    $Frame.page.canvas
   set data(Frame)     $Frame

   $data(Canvas) bind GRAPHUPDATE$gr <Any-KeyRelease> "$data(Canvas) itemconfigure $gr -update True"

   set id [$data(Canvas) create text $X0 $Y0  -tags "$tag CVTEXT GRAPHUPDATE$gr" -text [lindex $Lbl(Title) $GDefs(Lang)] \
      -font $Graph::Font(Graph) -fill black -anchor nw -justify center]
   $data(Canvas) create graph -x $X0 -y $Y0 -width $Width -height $Height -anchor nw -legend 0 -command $gr \
       -fg black -bg $Graph::Color(Frame) -fill $Graph::Color(Graph) -tags "$tag $gr" -font $Graph::Font(Graph) \
       -legend False -title $id
   $data(Canvas) raise $id

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
         
      Graph::Mode $gr Section True
      Graph::PosAdd $gr Section
   }

   #----- Creer les fonction du mode actif

   if { $Active } {
      Page::ActiveWrapper Graph $Frame $gr $X0 $Y0 [expr $Width+$X0] [expr $Height+$Y0] Section
   } elseif { $Full } {
      Page::ActiveFull Graph $Frame $gr $Full
   }
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

   set Page::Data(Coord) ""
   set Page::Data(Value) ""

   if  { [llength [set items [lindex [$data(Canvas) itemconfigure $GR -item] end]]] } {
      set coords [$GR -unproject $X $Y False [lindex $items 0]]

      if { [llength $coords]>=2 } {
         catch {
            set Page::Data(Coord) "[lindex $Graph::Lbl(Level) $GDefs(Lang)]: [format "%1.3e" [lindex $coords 1]] [lindex $Graph::Lbl(Pos) $GDefs(Lang)]:[format "%.3f" [lindex $coords 0]]"

            foreach item $items {
               set field [graphitem configure $item -data]
               if { [fstdfield is $field True] } {
                  append Page::Data(Value) "[fstdfield configure $field -desc]:[FSTD::FieldFormat $field [lindex $coords 2]] "
               }
            }
         }
         if { $data(Proj) } {

         }
      }
   }
}

#-------------------------------------------------------------------------------
# Nom      : <Graph::Section::Clean>
# Creation : Octobre 2002 - J.P. Gauthier - CMC/CMOE -
#
# But      : Supprimer les donnees associees aux coordonees.
#
# Parametres :
#   <GR>     : Identificateur du Graph
#
#-------------------------------------------------------------------------------

proc Graph::Section::Clean { GR } {

   Animator::EmptyPlayList
}

#-------------------------------------------------------------------------------
# Nom      : <Graph::Section::Graph>
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

proc Graph::Section::Graph { GR } {
   global GDefs
   variable Data

   upvar #0 Graph::Section::Section${GR}::Data  data
   upvar #0 Graph::Section::Section${GR}::Graph graph

   if { ![llength $data(Items)] } {
      return
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
      if { [fstdfield is GRAPHSELECT$item True] } {
         #----- Check for vertical coordinate selection
         switch $graph(ZType) {
            "PRESSURE" {
               set levels [fstdfield stats GRAPHSELECT$item -pressurelevels]
               if { ![llength $levels] } {
                  Dialog::Error . $Graph::Error(Pressure)
               }
               fstdfield configure GRAPHSELECT$item -ztype PRESSURE
               set data(Levels) $levels
               set graph(UnitY) [lindex $Graph::Lbl(PRESSURE) $GDefs(Lang)]
            }
            "MASL" {
               set levels [fstdfield stats GRAPHSELECT$item -meterlevels]
               if { ![llength $levels] } {
                  Dialog::Error . $Graph::Error(Meter)
               }
               fstdfield configure GRAPHSELECT$item -ztype MAGL
               set data(Levels) $levels
               set graph(UnitY) [lindex $Graph::Lbl(MASL) $GDefs(Lang)]
            }
            "GRID" {
                fstdfield configure GRAPHSELECT$item -ztype UNDEFINED
                set data(Levels) [fstdfield stats GRAPHSELECT$item -levels]
                set graph(UnitY) [fstdfield stats GRAPHSELECT$item -leveltype]
             }
         }
         set data(XMin)   0
         set data(XMax)   [expr [fstdfield define GRAPHSELECT$item -NI]-1]
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
   graphaxis configure axisy$GR -type $graph(YScale) -modulo $mod -min $ymin -max $ymax -intervals $yinter -increment $yincr -angle $graph(YAngle) \
      -font $Graph::Font(Axis) -gridcolor $Graph::Grid(YColor) -dash $Graph::Grid(YDash) -gridwidth $Graph::Grid(YWidth) -color $Graph::Color(Axis) \
      -format $graph(YFormat) -decimal $graph(YDecimals)

   set id [lindex [$data(Canvas) itemconfigure $GR -title] end]
   $data(Canvas) itemconfigure $id -font $Graph::Font(Graph) -fill $Graph::Color(FG)
   $data(Canvas) itemconfigure $GR -item $data(Items) -bd $Graph::Width(Frame) \
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
      set Data(Tmp)      {}           ;#Liste des champs temporaire
      set Data(Proj)     0            ;#Mode projection
      set Data(DCoords)  {}           ;#Liste des longueur des segment
      set Data(Levels)   {}           ;#Liste des niveaux
      set Data(Coords)   {}           ;#Liste des coordonnees de coupe
      set Data(Field)    ""           ;#Champs de coupe

      #----- Constantes relatives au Graph

      set Graph(YScale)    LINEAR                                               ;#Type d'echelle en Y
      set Graph(XScale)    LINEAR                                               ;#Type d'echelle en Y
      set Graph(UnitY)     "[lindex $Graph::Lbl(Unit) $GDefs(Lang)] Y"          ;#Descriptif de l'echelle des valeur en Y
      set Graph(UnitX)     "[lindex $Graph::Lbl(Pos) $GDefs(Lang)]"             ;#Descriptif de l'echelle des valeur en X
      set Graph(XInter)    ""               ;#Liste des niveau specifie par l'usager
      set Graph(YInter)    ""               ;#Liste des niveau specifie par l'usager
      set Graph(ZXInter)   ""               ;#Liste des Niveaux (Mode Zoom)
      set Graph(ZYInter)   ""               ;#Liste des Niveaux (Mode Zoom)
      set Graph(ZType)     GRID             ;#Type de niveaux (GRID,PRESSSURE)
      set Graph(XFormat)   NONE
      set Graph(YFormat)   NONE
      set Graph(XDecimals) 0
      set Graph(YDecimals) 0
      set Graph(XAngle)    0
      set Graph(YAngle)    0                                                   ;
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

   labelframe $Parent.params -text [lindex $Graph::Lbl(Section) $GDefs(Lang)]
      frame $Parent.params.res
         label $Parent.params.res.lbl -text [lindex $Graph::Lbl(Res) $GDefs(Lang)] -width 14 -anchor w
         entry $Parent.params.res.val -textvariable Graph::Data(Res) -width 8 -bd 1 -bg $GDefs(ColorLight)
         checkbutton $Parent.params.res.auto -text * -indicatoron false -bd 1 -variable Graph::Data(ResBest) \
            -onvalue True -offvalue False -command "Graph::VertexResolution Section $GR"
         pack $Parent.params.res.lbl -side left
         pack $Parent.params.res.val -side left -fill x -expand True
         pack $Parent.params.res.auto -side left

      frame $Parent.params.disp -relief sunken -bd 1
         checkbutton $Parent.params.disp.proj -text [lindex $Graph::Lbl(Proj) $GDefs(Lang)] -variable Graph::Section::Section${GR}::Data(Proj) -bd 1\
            -onvalue 1 -offvalue 0 -indicatoron false -command "Graph::Section::FieldShow $GR"
         pack $Parent.params.disp.proj -side top -fill x
      pack $Parent.params.res $Parent.params.disp -side top -fill x
   pack $Parent.params -side top -fill x -padx 5 -pady 5

   Graph::ParamsAxis $Parent $GR Section X MARK
   Graph::ParamsAxis $Parent $GR Section Y VERTICAL
   Graph::ModeSelect LINE LINE NIL

   Bubble::Create $Parent.params.disp.proj $Graph::Bubble(Viewport)
   Bubble::Create $Parent.params.res.val   $Graph::Bubble(Sample)

   bind $Parent.params.res.val <Return> "Graph::VertexResolution Section $GR"
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
      set id [$data(Canvas) create text -100 -100  -tags "PAGE$GR CVTEXT GRAPHUPDATE$GR" -text $Item -anchor nw -justify left]

      graphitem create $Item
      graphitem configure $Item -desc $id

      lappend data(Items) $Item
      Graph::Section::ItemDefault $GR $Item

      $data(Canvas) itemconfigure $GR -item $data(Items)
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

      $data(Canvas) itemconfigure $GR -item $data(Items)
      $data(Canvas) delete [graphitem configure $Item -desc]

      graphitem free $Item
      fstdfield free GRAPHSELECT$Item

      FSTD::UnRegister GRAPHSELECT$Item

      set list [lindex [$data(FrameData).page.canvas itemconfigure $data(VP) -data] 4]
      if { [set idx [lsearch -exact $list GRAPHSELECT$Item]]!=-1 } {
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
   set data(Items$Pos)  {}
   set data(Coords$Pos) $Coords
   set data(Pos$Pos)    [Graph::VertexSample Section $GR $Coords]
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

      set data(Pos) [lreplace $data(Pos) $idx $idx]
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
      if { [fstdfield is $Data True] && [llength $data(Pos$Pos)] } {
         fstdfield free GRAPHSELECT$Item

         if { $graph(ZType)=="GRID" } {
            fstdfield configure $Data -ztype UNDEFINED
         } else {
            fstdfield configure $Data -ztype $graph(ZType)
         }

         fstdfield vertical GRAPHSELECT$Item $Data $data(Pos$Pos)

         FSTD::Register GRAPHSELECT$Item
         graphitem configure $Item -xaxis axisx$GR -yaxis axisy$GR -data GRAPHSELECT$Item
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

         catch {
            $data(FrameData).page.canvas configure -cursor watch
            $data(Canvas) configure -cursor watch
            update idletasks
         }

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
            Graph::Section::ItemDefine $gr $pos $data(Coords$pos)
         }
         Graph::PosSet $gr Section
         Graph::Section::FieldShow $gr

         catch {
            $data(Canvas) configure -cursor left_ptr
            $data(FrameData).page.canvas configure -cursor left_ptr
         }
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

   upvar #0 Graph::Section::Section${GR}::Data  data
   upvar #0 Graph::Section::Section${GR}::Graph graph

   SPI::Progress 0 [lindex $Graph::Msg(Reading) $GDefs(Lang)]

   #----- Recuperer les champs correspondants du viewport actif

   set data(Data)   {}

   set fields {}
   foreach field $Data {
      if { [fstdfield is $field True] } {
         set grtyp [fstdfield define $field -GRTYP]
         if { $grtyp!="V" && $grtyp!="X"  && $grtyp!="Y" } {
            if { $Graph::Data(IP3) } {
               fstdfield readcube $field
            } else {
               set ip3 [fstdfield define $field -IP3]
               fstdfield define $field -IP3 -1
               fstdfield readcube $field
               fstdfield define $field -IP3 $ip3
            }
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
            if { $idx==-1 && [fstdfield is $fld True] } {
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
