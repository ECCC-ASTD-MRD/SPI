#===============================================================================
# Environnement Canada
# Centre Meteorologique Canadian
# 2100 Trans-Canadienne
# Dorval, Quebec
#
# Projet   : Librairie d'objects visuel interactifs
# Fichier  : Graph_Frequence.tcl
# Creation : Octobre 2003 - J.P. Gauthier - CMC/CMOE
#
# Description: Ce package s'occupe de l'affichage et de la manipulation de graphs
#              de Frequence
#
# Fonctions:
#
#    Graph::Frequence::Create        { Frame X0 Y0 Width Height Active Full }
#    Graph::Frequence::Coord         { Frame GR X Y }
#    Graph::Frequence::Graph         { GR }
#    Graph::Frequence::Init          { Frame }
#    Graph::Frequence::Params        { Parent GR }
#    Graph::Frequence::ItemAdd       { GR Item }
#    Graph::Frequence::ItemDefault   { GR Item }
#    Graph::Frequence::ItemDefine    { GR Pos Coords { Update True } }
#    Graph::Frequence::ItemDel       { GR Item }
#    Graph::Frequence::ItemUnDefine  { GR Pos }
#    Graph::Frequence::ItemData      { GR Item Data Coords }
#    Graph::Frequence::ItemDataField {  GR Item Coords Data }
#    Graph::Frequence::ItemDataObs   { GR Item Coords Data }
#    Graph::Frequence::Update        { Frame { GR {} } }
#    Graph::Frequence::UpdateItems   { Frame { GR { } } }
#    Graph::Frequence::Data          { GR Data }
#    Graph::Frequence::FitData       { GR }
#
#===============================================================================

namespace eval Graph::Frequence { } {
   variable Lbl

   #----- Definitions des labels

   set Lbl(Title)    { "Diagramme de frequence" "Frequence Diagram" }
}

#----------------------------------------------------------------------------
# Nom      : <Graph::Frequence::Create>
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

proc Graph::Frequence::Create { Frame X0 Y0 Width Height Active Full { Link True } } {
   global GDefs
   variable Data
   variable Lbl

   set gr [Graph::Frequence::Init $Frame]
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
   set Graph::Data(Type$gr)     Frequence  ;#Type de graph

   upvar #0 Graph::Frequence::Frequence${gr}::Data  data
   upvar #0 Graph::Frequence::Frequence${gr}::Graph graph

   set data(Canvas)    $Frame.page.canvas
   set data(Frame)     $Frame

   $data(Canvas) bind GRAPHUPDATE$gr <Any-KeyRelease> "$data(Canvas) itemconfigure $gr -update True"
   set id [$data(Canvas) create text $X0 $Y0  -tags "$tag CVTEXT GRAPHUPDATE$gr" -text [lindex $Lbl(Title) $GDefs(Lang)] \
      -font $Graph::Font(Graph) -fill black -anchor nw -justify center]
   $data(Canvas) create graph -x $X0 -y $Y0 -width $Width -height $Height -anchor nw -xlegend 5 -ylegend 5 -command $gr \
       -fg black -bg $Graph::Color(Frame) -fill $Graph::Color(Graph) -tags "$tag $gr" -font $Graph::Font(Graph) -title $id
   $data(Canvas) raise $id

   if { $Viewport::Data(VP)!="" } {
      set data(VP)        $Viewport::Data(VP)
      set data(FrameData) $Viewport::Data(Frame$data(VP))
      Graph::Frequence::Update $data(FrameData) $gr
   }

   Graph::Activate $Frame $gr Frequence
   
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
         
      Graph::Mode $gr Frequence True
      Graph::PosAdd $gr Frequence
   }
   
   #----- Creer les fonction du mode actif

   if { $Active } {
      Page::ActiveWrapper Graph $Frame $gr $X0 $Y0 [expr $Width+$X0] [expr $Height+$Y0] Frequence
   } elseif { $Full } {
      Page::ActiveFull Graph $Frame $gr $Full
   }
   Page::Register $Frame Graph::Frequence $gr

   return $gr
}

#-------------------------------------------------------------------------------
# Nom      : <Graph::Frequence::Coord>
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

proc Graph::Frequence::Coord { Frame GR X Y } {
   global   GDefs

   upvar #0 Graph::Frequence::Frequence${GR}::Data  data
   upvar #0 Graph::Frequence::Frequence${GR}::Graph graph

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
# Nom      : <Graph::Frequence::Graph>
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

proc Graph::Frequence::Graph { GR } {
   variable Data

   upvar #0 Graph::Frequence::Frequence${GR}::Graph graph
   upvar #0 Graph::Frequence::Frequence${GR}::Data  data

   if { ![llength $data(Items)] } {
      return
   }

   #----- Recalculer les valeurs

   set data(XMin)  1e200
   set data(XMax) -1e200
   set data(YMin)  1e200
   set data(YMax) -1e200
   set mod True

   foreach item $data(Items) {
      set i 0
      vector length $item [llength $graph(XInter)]

      foreach inter $graph(XInter) {
         vector set $item.Y($i) [llength $data(R$item$i)]
         vector set $item.X($i) $inter
         incr i
      }
      set min [vector stats $item.X -min]
      set max [vector stats $item.X -max]
      set data(XMin) [expr $min<$data(XMin)?$min:$data(XMin)]
      set data(XMax) [expr $max>$data(XMax)?$max:$data(XMax)]
      set min [vector stats $item.Y -min]
      set max [vector stats $item.Y -max]
      set data(YMin) [expr $min<$data(YMin)?$min:$data(YMin)]
      set data(YMax) [expr $max>$data(YMax)?$max:$data(YMax)]

      graphitem configure $item -fit GAUSS
   }

   set yincr [Graph::ValIncr $data(YMin) $data(YMax) 10 $graph(YScale)]

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
   $data(Canvas) itemconfigure $id -font $Graph::Font(Axis) -fill $Graph::Color(Axis)
   graphaxis configure axisx$GR -type $graph(XScale) -modulo $mod -min $xmin -max $xmax -intervals $graph(XInter) -angle $graph(XAngle) \
      -lowoffset 0.05 -highoffset 0.05 -font $Graph::Font(Axis) \
      -gridcolor $Graph::Grid(XColor) -dash $Graph::Grid(XDash) -gridwidth $Graph::Grid(XWidth) -color $Graph::Color(Axis) \
      -format $graph(XFormat) -decimal $graph(XDecimals)

   set id [graphaxis configure axisy$GR -unit]
   $data(Canvas) itemconfigure $id -font $Graph::Font(Axis) -fill $Graph::Color(Axis)
   graphaxis configure axisy$GR -type $graph(YScale) -modulo $mod -min $ymin -max $ymax -increment $yincr -angle $graph(YAngle) -highoffset 0.05 \
      -font $Graph::Font(Axis) -gridcolor $Graph::Grid(YColor) -dash $Graph::Grid(YDash) -gridwidth $Graph::Grid(YWidth) -color $Graph::Color(Axis) \
      -format $graph(YFormat) -decimal $graph(YDecimals)

   set id [lindex [$data(Canvas) itemconfigure $GR -title] end]
   $data(Canvas) itemconfigure $id -font $Graph::Font(Graph) -fill $Graph::Color(FG)
   $data(Canvas) itemconfigure $GR -item $data(Items) -bd $Graph::Width(Frame) \
      -fg $Graph::Color(FG) -bg $Graph::Color(BG) -fill $Graph::Color(Fill) -font $Graph::Font(Graph)

   update idletasks
   $data(Canvas) config -cursor left_ptr
}

#----------------------------------------------------------------------------
# Nom      : <Graph::Frequence::Init>
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

proc Graph::Frequence::Init { Frame } {
   global   GDef
   variable Data

   set gr GR[incr Graph::Data(Nb)]

   namespace eval Frequence$gr {
      variable Data
      variable Graph

      set Data(Items)     {}        ;#Liste des items
      set Data(Pos)       {}        ;#Liste des positions
      set Data(Coords)    {}        ;#Liste des coordonnees de coupe
      set Data(Data)      {}        ;#Donnees du graph
      set Data(Lat0)      0         ;#Rectangle de selection
      set Data(Lat1)      0         ;#Rectangle de selection
      set Data(Lon0)      0         ;#Rectangle de selection
      set Data(Lon1)      0         ;#Rectangle de selection
      set Data(Field)     ""        ;#Champs de donnees
      set Data(VP)        ""
      set Data(FrameData) ""

      set Data(Obs)      ""        ;#Liste des stations selectionnee
      set Data(Select)   ""        ;#Case selectionnee

      #----- Constantes relatives au Graph

      set Graph(UnitY)     "[lindex $Graph::Lbl(Unit) $GDefs(Lang)] Y"         ;#Descriptif de l'echelle des valeur en Y
      set Graph(UnitX)     "[lindex $Graph::Lbl(Unit) $GDefs(Lang)] X"         ;#Descriptif de l'echelle des valeur en X
      set Graph(XScale)    LINEAR                                              ;#Type d'echelle en X
      set Graph(YScale)    LINEAR                                              ;#Type d'echelle en Y
      set Graph(XInter)    "0 5 10 15 20 25 30 35 40 45 50 55 60"               ;#Liste des niveau specifie par l'usager
      set Graph(YInter)    ""               ;#Liste des niveau specifie par l'usager
      set Graph(ZXInter)   ""               ;#Liste des Niveaux (Mode Zoom)
      set Graph(ZYInter)   ""               ;#Liste des Niveaux (Mode Zoom)
      set Graph(Lines)     False
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
# Nom      : <Graph::Frequence::Params>
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

proc Graph::Frequence::Params { Parent GR } {

   Graph::ParamsPos  $Parent
   Graph::ParamsItem $Parent
   Graph::ParamsAxis $Parent $GR Frequence X
   Graph::ModeSelect BOX BOX NIL
}

#-------------------------------------------------------------------------------
# Nom      : <Graph::Frequence::ItemAdd>
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

proc Graph::Frequence::ItemAdd { GR Item } {

   upvar #0 Graph::Frequence::Frequence${GR}::Data  data

   if { [lsearch -exact $data(Items) $Item]==-1 } {
      vector free   $Item
      vector create $Item
      vector dim    $Item { X Y }

      set id [$data(Canvas) create text -100 -100  -tags "PAGE$GR CVTEXT GRAPHUPDATE$GR" -text $Item -anchor nw -justify left]

      graphitem free   $Item
      graphitem create $Item
      graphitem configure $Item -xaxis axisx$GR -yaxis axisy$GR -xdata $Item.X -ydata $Item.Y -orient X -desc $id

      lappend data(Items) $Item
      Graph::Frequence::ItemDefault $GR $Item

      $data(Canvas) itemconfigure $GR -item $data(Items)
  }
}

#-------------------------------------------------------------------------------
# Nom      : <Graph::Frequence::ItemDefault>
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

proc Graph::Frequence::ItemDefault { GR Item } {

   upvar #0 Graph::Frequence::Frequence${GR}::Data  data

   set idx [lsearch -exact $data(Items) $Item]

   set Graph::Item(Outline)     [lindex $Graph::Graph(Colors) [expr $idx%[llength $Graph::Graph(Colors)]]]
   set Graph::Item(FillColor)   #FFFFFF
   set Graph::Item(Tranparency) 100
   set Graph::Item(Width)       1
   set Graph::Item(Size)        0
   set Graph::Item(Value)       False
   set Graph::Item(Dash)        ""
   set Graph::Item(Type)        HISTOGRAM
   set Graph::Item(Icon)        [lindex $Graph::Graph(Icons) [expr $idx%[llength $Graph::Graph(Icons)]]]
   set Graph::Item(Bitmap)      ""
   set Graph::Item(Stipple)     -1
   set Graph::Item(Image)       ""

   Graph::ItemConfigure $GR Frequence $Item
}

#-------------------------------------------------------------------------------
# Nom      : <Graph::Frequence::ItemDel>
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

proc Graph::Frequence::ItemDel { GR Item } {

   upvar #0 Graph::Frequence::Frequence${GR}::Data  data

   if { [set idx [lsearch -exact $data(Items) $Item]]!=-1 } {
      set data(Items) [lreplace $data(Items) $idx $idx]
      $data(Canvas) itemconfigure $GR -item $data(Items)
      $data(Canvas) delete [graphitem configure $Item -desc]

      vector free $Item
      graphitem free $Item
   }
}

#-------------------------------------------------------------------------------
# Nom      : <Graph::Frequence::ItemDefine>
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

proc Graph::Frequence::ItemDefine { GR Pos Coords { Update True } } {

   upvar #0 Graph::Frequence::Frequence${GR}::Data  data

   if { $Pos=="" } {
      return
   }

   if { [info exists Graph::Frequence::Frequence${GR}::Data(Items$Pos)] } {
      foreach item [lrange $data(Items$Pos) [llength $data(Data)] end] {
         Graph::Frequence::ItemDel $GR $item
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

   set item $Pos

   Graph::Idle $GR Frequence
   Graph::Frequence::ItemAdd $GR $item
   Graph::Frequence::UpdateItems $data(FrameData) $GR

   if { $Update } {
      Graph::Frequence::ItemData $GR $Pos $item $data(Data)
      Graph::Frequence::Graph $GR
   }
   Graph::UnIdle $GR Frequence
}

#-------------------------------------------------------------------------------
# Nom      : <Graph::Frequence::ItemUnDefine>
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

proc Graph::Frequence::ItemUnDefine { GR Pos } {

   upvar #0 Graph::Frequence::Frequence${GR}::Data  data

   if { [set idx [lsearch -exact $data(Pos) $Pos]]!=-1 } {
      foreach item $data(Items$Pos) {
         Graph::Frequence::ItemDel $GR $item
      }

      set data(Pos) [lreplace $data(Pos)  $idx $idx]
      set Graph::Data(Pos) [lindex $data(Pos) end]
   }
}

#-------------------------------------------------------------------------------
# Nom      : <Graph::Frequence::ItemData>
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

proc Graph::Frequence::ItemData { GR Pos Item Data } {

   upvar #0 Graph::Frequence::Frequence${GR}::Data  data
   upvar #0 Graph::Frequence::Frequence${GR}::Graph graph

   #----- Pour tout les couples de donnees

   set data(Select) ""
   set nb [expr 100.0/([llength $data(Data)]+1)]
   SPI::Progress 0

   #----- Initialiser les listes

   set i 0
   foreach inter $graph(XInter) {
       set data(R$Item$i) {}
       incr i
   }

   Graph::Frequence::ItemDataObs $GR $Item $data(Pos$Pos) $data(Data)

   SPI::Progress 0
}

#-------------------------------------------------------------------------------
# Nom      : <Graph::Frequence::ItemDataField>
# Creation : Mars 2003 - J.P. Gauthier - CMC/CMOE
#
# But      : Calculer les blocs de donnees pour deux champs.
#
# Parametres :
#   <GR>     : Identificateur du Graph
#   <Field>  : Champs
#
# Retour    :
#
# Remarque :
#
#-------------------------------------------------------------------------------

proc Graph::Frequence::ItemDataField {  GR Item Coords Data } {

   upvar #0 Graph::Frequence::Frequence${GR}::Data  data
   upvar #0 Graph::Frequence::Frequence${GR}::Graph graph

   set r [expr [llength $graph(XInter)]-1]
   set data(YMax) 0

   for { set ni 0 } { $ni<[fstdfield define $Data -NI] } { incr ni 2 } {
      for { set nj 0 } { $nj<[fstdfield define $Data -NJ] } { incr nj 2 } {

         set coo [fstdfield stats $Data -gridpoint $ni $nj]
         eval set in \[Viewport::CheckInside $Coords [lindex $coo 0] [lindex $coo 1]\]

         if { $in } {

            set val [lindex [fstdfield stats $Data -gridvalue $ni $nj] 0]

            if { $val!="-"  } {
               for { set i 0 } { $i < $r } { incr i } {
                  if { $val>=[lindex $graph(XInter) $i] && $val<[lindex $graph(XInter) [expr $i+1]] } {
                     lappend data(R$i) $ni.$nj
                     break
                  }
               }
            }
         }
      }
   }
}

#-------------------------------------------------------------------------------
# Nom      : <Graph::Frequence::ItemDataObs>
# Creation : Mars 2003 - J.P. Gauthier - CMC/CMOE
#
# But      : Calculer les blocs de donnees pour observations.
#
# Parametres :
#   <GR>     : Identificateur du Graph
#   <Field>  : Champs
#
# Retour    :
#
# Remarque :
#
#-------------------------------------------------------------------------------

proc Graph::Frequence::ItemDataObs { GR Item Coords Data } {

   upvar #0 Graph::Frequence::Frequence${GR}::Data  data
   upvar #0 Graph::Frequence::Frequence${GR}::Graph graph

   set r [expr [llength $graph(XInter)]-1]
   set data(Val$Item) {}

   foreach obs $Data {

      for { set n 0 } { $n<[observation define $obs -NB] } { incr n } {

         set coo [observation define $obs -COORD $n]
         eval set in \[Viewport::CheckInside $Coords [lindex $coo 0] [lindex $coo 1]\]

         if { $in } {

            set val [observation define $obs -DATA $n]

            if { $val!="-"  } {
               lappend data(Val$Item) $val

               for { set i 0 } { $i < $r } { incr i } {
                  if { $val>=[lindex $graph(XInter) $i] && $val<[lindex $graph(XInter) [expr $i+1]] } {
                     lappend data(R$Item$i) $n
                     break
                  }
               }
            }
         }
      }
   }
}

#-------------------------------------------------------------------------------
# Nom      : <Graph::Frequence::Update>
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

proc Graph::Frequence::Update { Frame { GR {} } } {
   variable Data

   if { ![llength $GR] } {
      set GR [Page::Registered All Graph::Frequence]
   }

   foreach gr $GR {

      upvar #0 Graph::Frequence::Frequence${gr}::Data  data

      if { $data(FrameData)==$Frame && $Frame!="" } {

         catch {
            $data(FrameData).page.canvas configure -cursor watch
            $data(Canvas) configure -cursor watch
            update idletasks
         }

         if { [Page::Registered All Viewport $data(VP)]!=-1 } {
            Graph::Frequence::Data  $gr [Viewport::Assigned $Viewport::Data(Frame$data(VP)) $data(VP) { fstdfield observation }]
         }
         foreach pos $data(Pos) {
            Graph::Frequence::ItemDefine $gr $pos $data(Coords$pos)
         }
         Graph::PosSet $gr Frequence

         catch {
            $data(Canvas) configure -cursor left_ptr
            $data(FrameData).page.canvas configure -cursor left_ptr
         }
      }
   }
}

#-------------------------------------------------------------------------------
# Nom      : <Graph::Frequence::UpdateItems>
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

proc Graph::Frequence::UpdateItems { Frame { GR { } } } {
   global GDefs
   variable Lbl
   variable Data

   if { ![llength $GR] } {
      set GR [Page::Registered All Graph::Frequence]
   }

   foreach gr $GR {

      upvar #0 Graph::Frequence::Frequence${gr}::Data  data

      if { $data(VP)!="" && $data(FrameData)==$Frame } {
         $Frame.page.canvas delete GRAPHSELECT$gr

         foreach pos $data(Pos) {
            if { [llength $data(Items$pos)] } {
               set id [graphitem configure [lindex $data(Items$pos) 0] -desc]
               set desc [lindex [$data(Canvas) itemconfigure $id -text] end]
               Graph::ItemPos $Frame $data(VP) $data(Pos$pos) "[lindex $Lbl(Title) $GDefs(Lang)]\n$desc" GRAPHSELECT$gr BOX
            }
         }

         if { $data(Select)!="" && $data(Obs)!="" } {
            foreach idx $data(R$data(Select)) {
               set coo [observation define $data(Obs) -COORD $idx]
               if { [set pix [$data(VP) -project [lindex $coo 0] [lindex $coo 1] 0]]!="" && [lindex $pix 2]>0 } {
                  $data(FrameData).page.canvas create bitmap [expr [lindex $pix 0]-$Obs::Param(Size)] [lindex $pix 1] \
                     -bitmap @$GDefs(Dir)/share/bitmap/arrow.ico -tags "PAGE$data(VP) GRAPHSELECT$gr" \
                     -anchor e -foreground $Graph::Color(Select)
               }
            }
         }
      }
   }
}

#-------------------------------------------------------------------------------
# Nom      : <Graph::Frequence::Data>
# Creation : Mars 2003 - J.P. Gauthier - CMC/CMOE
#
# But      : Recupere les valeurs aux localisations specifees parmi tous
#            les champs correspondants disponibles.
#
# Parametres :
#   <GR>     : Identificateur du Graph
#   <Data>   : Liste des donnees
#
# Retour    :
#
# Remarque :
#
#-------------------------------------------------------------------------------

proc Graph::Frequence::Data { GR Data } {

   upvar #0 Graph::Frequence::Frequence${GR}::Data  data

   set data(Data) {}

   #----- Pour tout les couples de donnees

   foreach obs $Data {
      if { [observation is $obs] } {
         lappend data(Data) $obs
      }
   }
}

#-------------------------------------------------------------------------------
# Nom      : <Graph::Frequence::FitData>
# Creation : Mars 2003 - J.P. Gauthier - CMC/CMOE
#
# But      : Appliquer une courbe de distribution aux donnees.
#
# Parametres :
#   <GR>     : Identificateur du Graph
#
# Retour    :
#
# Remarque :
#
#-------------------------------------------------------------------------------

proc Graph::Frequence::FitData { GR } {

   upvar #0 Graph::Frequence::Frequence${GR}::Data  data
   upvar #0 Graph::Frequence::Frequence${GR}::Graph graph

   MetStat::Calculate $data(Val) $data(Val)

   set co {}

   for { set x [lindex $graph(XInter) 0] } { $x <= [lindex $graph(XInter) end] } { set x [expr double($x+1.0)] } {
      set o [expr sqrt($MetStat::Stat(VARx))]
      set n [expr ($x-$MetStat::Stat(AVGx))/$o]
      lappend co [DrawX $GR $x] [DrawY $GR [expr 1.0/(sqrt(2*3.1416)*$o)*exp(-0.5*($n*$n))*$data(Y1)*$MetStat::Stat(AVGx)]]
   }

   eval $data(Canvas) create line $co  -fill #AAAAAA -tags \"PAGE$GR DATA$GR\" -width 2
}
