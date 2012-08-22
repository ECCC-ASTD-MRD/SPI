#===============================================================================
# Environnement Canada
# Centre Meteorologique Canadian
# 2100 Trans-Canadienne
# Dorval, Quebec
#
# Projet   : Librairie d'objects visuel interactifs
# Fichier  : Graph_Time.tcl
# Creation : Octobre 2003 - J.P. Gauthier - CMC/CMOE
#
# Description: Ce package s'occupe de l'affichage et de la manipulation de graphs
#              de serie temporelle
#
# Fonctions:
#
#    Graph::Time::Create       { Frame X0 Y0 Width Height Active Full }
#    Graph::Time::Coord        { Frame GR X Y }
#    Graph::Time::Clean        { GR }
#    Graph::Time::Graph        { GR }
#    Graph::Time::Init         { Frame }
#    Graph::Time::Params       { Parent GR }
#    Graph::Time::ItemAdd      { GR Item }
#    Graph::Time::ItemDefault  { GR Item }
#    Graph::Time::ItemDel      { GR Item }
#    Graph::Time::ItemDefine   { GR Pos Coords { Update True } }
#    Graph::Time::ItemUnDefine { GR Pos }
#    Graph::Time::ItemData     { GR Pos Item Data }
#    Graph::Time::Update       { Frame { GR {} } }
#    Graph::Time::UpdateItems  { Frame { GR { } } }
#    Graph::Time::Data         { GR { Data { } } { Files { } } }
#
#===============================================================================

namespace eval Graph::Time { } {
   variable Lbl

   set Lbl(Title)     { "Série temporelle" "Time serie" }
}

#----------------------------------------------------------------------------
# Nom      : <Graph::Time::Create>
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

proc Graph::Time::Create { Frame X0 Y0 Width Height Active Full } {
   global GDefs
   variable Data
   variable Lbl

   set gr [Graph::Time::Init $Frame]
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
   set Graph::Data(Type$gr)     Time       ;#Type de graph

   upvar #0 Graph::Time::Time${gr}::Data  data
   upvar #0 Graph::Time::Time${gr}::Graph graph

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
      Graph::Time::Update $data(FrameData) $gr
   } else {
      set data(VP)        ""
      set data(FrameData) ""
   }

   Graph::Activate $Frame $gr Time
   Graph::Mode $gr Time True
   Graph::PosAdd $gr Time

   #----- Creer les fonction du mode actif

   if { $Active } {
      Page::ActiveWrapper Graph $Frame $gr $X0 $Y0 [expr $Width+$X0] [expr $Height+$Y0] Time
   }
   Page::Register $Frame Graph::Time $gr

   return $gr
}

#-------------------------------------------------------------------------------
# Nom      : <Graph::Time::Coord>
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

proc Graph::Time::Coord { Frame GR X Y } {
   global   GDefs

   upvar #0 Graph::Time::Time${GR}::Data  data

   set Page::Data(Coord) ""
   set Page::Data(Value) ""

   if  { [llength [set items [lindex [$data(Canvas) itemconfigure $GR -item] end]]] } {
      set coords [$GR -unproject $X $Y False [lindex $items 0]]

      if { [llength $coords]==2 } {
         catch {
            set Page::Data(Value) "[lindex $Graph::Lbl(Val) $GDefs(Lang)]: [format "%1.3e" [lindex $coords 1]]"

            set sec [lindex $coords 0]
            set Page::Data(Coord) [DateStuff::StringDateFromSeconds [expr $sec>1e31?0:$sec<1e-32?0:$sec] $GDefs(Lang)]
         }
      }
   }
}

#-------------------------------------------------------------------------------
# Nom      : <Graph::Time::Clean>
# Creation : Octobre 2002 - J.P. Gauthier - CMC/CMOE -
#
# But      : Supprimer les donnees associees aux coordonees.
#
# Parametres :
#   <GR>     : Identificateur du Graph
#
#-------------------------------------------------------------------------------

proc Graph::Time::Clean { GR } {

   upvar #0 Graph::Time::Time${GR}::Data  data

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
# Nom      : <Graph::Time::Graph>
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

proc Graph::Time::Graph { GR } {
   global   GDefs
   variable Data

   upvar #0 Graph::Time::Time${GR}::Data  data
   upvar #0 Graph::Time::Time${GR}::Graph graph

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
      set data(Dates) [concat $data(Dates) [vector get $item.X]]
      set min [vector stats $item.Y -min]
      set max [vector stats $item.Y -max]
      set data(YMin) [expr $min<$data(YMin)?$min:$data(YMin)]
      set data(YMax) [expr $max>$data(YMax)?$max:$data(YMax)]
   }

   set data(Dates) [lsort -unique -real -increasing $data(Dates)]
   set data(XMin)  [lindex $data(Dates) 0]
   set data(XMax)  [lindex $data(Dates) end]

   if { ![llength $data(Dates)] } {
      return
   }

   if { $data(Date0)!="" } {
      set data(XMin) [clock scan $data(Date0) -gmt True]
   }
   if { $data(Date1)!="" } {
      set data(XMax) [clock scan $data(Date1) -gmt True]
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

   if { ![llength $graph(XInter)] } {
      set dates $data(Dates)
   } else {
      set dates $graph(XInter)
      set data(XMin) [lindex $graph(XInter) 0]
      set data(XMax) [lindex $graph(XInter) end]
   }

   set xinter $dates
   set xdates {}

   if { $graph(XFormat)=="NONE" } {
      set diff [expr $data(XMax)-$data(XMin)]

      if { $diff <= 120 } {
         set graph(UnitX) "[lindex $Graph::Lbl(Since) $GDefs(Lang)] [DateStuff::StringDateFromSeconds $data(XMin) $GDefs(Lang)] ([lindex $Graph::Lbl(Sec) $GDefs(Lang)])"
         set data(Time)  S
      } elseif { $diff <=7200 } {
         set graph(UnitX) "[lindex $Graph::Lbl(Since) $GDefs(Lang)] [DateStuff::StringDateFromSeconds $data(XMin) $GDefs(Lang)] ([lindex $Graph::Lbl(Min) $GDefs(Lang)])"
         set data(Time)  M
      } elseif { $diff <=1296000 } {
         set graph(UnitX) "[lindex $Graph::Lbl(Since) $GDefs(Lang)] [DateStuff::StringDateFromSeconds $data(XMin) $GDefs(Lang)] ([lindex $Graph::Lbl(Hour) $GDefs(Lang)])"
         set data(Time)  H
      } else {
         set graph(UnitX) "[lindex $Graph::Lbl(Since) $GDefs(Lang)] [DateStuff::StringDateFromSeconds $data(XMin) $GDefs(Lang)] ([lindex $Graph::Lbl(Day) $GDefs(Lang)])"
         set data(Time)  D
      }

      foreach date $dates {
         lappend xdates [Graph::TimeFormat $date $data(Time) $data(XMin)]
      }
   } else {
      set data(Time)    DATE
      set graph(UnitX)  [lindex $Graph::Lbl(Date) $GDefs(Lang)]
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

   set id [graphaxis configure axisx$GR -unit]
   if { $Graph::Data(Update) } {
      $data(Canvas) itemconfigure $id -text $graph(UnitX)
   }
   $data(Canvas) itemconfigure $id -font $Graph::Font(Axis) -fill $Graph::Color(Axis)
   graphaxis configure axisx$GR -type $graph(XScale) -modulo $mod -min $xmin -max $xmax -intervals $xinter -labels $xdates -angle $graph(XAngle)\
      -font $Graph::Font(Axis) -gridcolor $Graph::Grid(XColor) -dash $Graph::Grid(XDash) -gridwidth $Graph::Grid(XWidth) -color $Graph::Color(Axis) \
      -format $graph(XFormat) -decimal $graph(XDecimals)

   set id [graphaxis configure axisy$GR -unit]
   if { $Graph::Data(Update) } {
      $data(Canvas) itemconfigure $id -text $graph(UnitY)
   }
   $data(Canvas) itemconfigure $id -font $Graph::Font(Axis) -fill $Graph::Color(Axis)
   graphaxis configure axisy$GR -type $graph(YScale) -modulo $mod -min $ymin -max $ymax -intervals $yinter -increment $yincr -angle $graph(YAngle) \
      -font $Graph::Font(Axis) -gridcolor $Graph::Grid(YColor)  -dash $Graph::Grid(YDash) -gridwidth $Graph::Grid(YWidth) -color $Graph::Color(Axis) \
      -format $graph(YFormat) -decimal $graph(YDecimals)

   set id [lindex [$data(Canvas) itemconfigure $GR -title] end]
   $data(Canvas) itemconfigure $id -font $Graph::Font(Graph) -fill $Graph::Color(FG)
   $data(Canvas) itemconfigure $GR -item $data(Items) -bd $Graph::Width(Frame) \
      -fg $Graph::Color(FG) -bg $Graph::Color(BG) -fill $Graph::Color(Fill) -font $Graph::Font(Graph)

   update idletasks
   $data(Canvas) config -cursor left_ptr
}

#----------------------------------------------------------------------------
# Nom      : <Graph::Time::Init>
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

proc Graph::Time::Init { Frame } {
   global   GDefs
   variable Data

   set gr GR[incr Graph::Data(Nb)]

   namespace eval Time$gr {
      variable Data
      variable Graph

      set Data(Items)           {}        ;#Liste des items
      set Data(Pos)             {}        ;#Liste des positions
      set Data(Coords)          {}        ;#Liste des coordonnees de coupe
      set Data(Data)            {}        ;#Liste des champs selectionnees
      set Data(Tmp)             {}        ;#Liste des champs temporaire
      set Data(ObsIds)          {}        ;#Liste des observations selectionnee
      set Data(ObsToken)        ""        ;#Token de recherche
      set Data(Time)            ""
      set Data(Date0)           ""
      set Data(Date1)           ""

      #----- Constantes relatives au Graph

      set Graph(UnitY)     "[lindex $Graph::Lbl(Unit) $GDefs(Lang)] Y"     ;#Descriptif de l'echelle des valeur en Y
      set Graph(UnitX)     "[lindex $Graph::Lbl(Unit) $GDefs(Lang)] X"     ;#Descriptif de l'echelle des valeur en X
      set Graph(YScale)    LINEAR                                          ;#Type d'echelle en Y
      set Graph(XScale)    LINEAR                                          ;#Type d'echelle en Y
      set Graph(XInter)    ""                                              ;#Liste des niveau specifie par l'usager
      set Graph(YInter)    ""                                              ;#Liste des niveau specifie par l'usager
      set Graph(ZXInter)   ""                                              ;#Liste des Niveaux (Mode Zoom)
      set Graph(ZYInter)   ""                                              ;#Liste des Niveaux (Mode Zoom)
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
# Nom      : <Graph::Time::Params>
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

proc Graph::Time::Params { Parent GR } {
   global   GDefs

   upvar #0 Graph::Time::Time${GR}::Data data

   Graph::ParamsPos  $Parent
   Graph::ParamsItem $Parent
   Graph::ParamsAxis $Parent $GR Time X TIME
   Graph::ParamsAxis $Parent $GR Time Y
   Graph::ParamsObs  $Parent $GR Time
   Graph::ModeSelect POINT { POINT BOX POLYGON }
}

#-------------------------------------------------------------------------------
# Nom      : <Graph::Time::ItemAdd>
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

proc Graph::Time::ItemAdd { GR Item } {

   upvar #0 Graph::Time::Time${GR}::Data  data

   if { [lsearch -exact $data(Items) $Item]==-1 } {
      vector free   $Item
      vector create $Item
      vector dim    $Item { X Y }

      set id [$data(Canvas) create text -100 -100  -tags "$Page::Data(Tag)$GR CVTEXT GRAPHUPDATE$GR" -text $Item -anchor nw -justify left]

      graphitem create $Item
      graphitem configure $Item -xaxis axisx$GR -yaxis axisy$GR -xdata $Item.X -ydata $Item.Y -orient X -desc $id

      lappend data(Items) $Item
      Graph::Time::ItemDefault $GR $Item
   }
   $data(Canvas) itemconfigure $GR -item $data(Items)
}

#-------------------------------------------------------------------------------
# Nom      : <Graph::Time::ItemDefault>
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

proc Graph::Time::ItemDefault { GR Item } {

   upvar #0 Graph::Time::Time${GR}::Data  data

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

   Graph::ItemConfigure $GR Time $Item
}

#-------------------------------------------------------------------------------
# Nom      : <Graph::Time::ItemDel>
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

proc Graph::Time::ItemDel { GR Item } {

   upvar #0 Graph::Time::Time${GR}::Data  data

   if { [set idx [lsearch -exact $data(Items) $Item]]!=-1 } {
      set data(Items) [lreplace $data(Items) $idx $idx]

      $data(Canvas) delete [graphitem configure $Item -desc]
      $data(Canvas) itemconfigure $GR -item $data(Items)

      vector free $Item
      graphitem free $Item
   }
}

#-------------------------------------------------------------------------------
# Nom      : <Graph::Time::ItemDefine>
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

proc Graph::Time::ItemDefine { GR Pos Coords { Update True } } {

   upvar #0 Graph::Time::Time${GR}::Data  data

   if { $Pos=="" } {
      return
   }

   if { [info exists Graph::Time::Time${GR}::Data(Items$Pos)] } {
      foreach item [lrange $data(Items$Pos) [llength $data(Data)] end] {
         Graph::Time::ItemDel $GR $item
      }
   }

   if { [lsearch -exact $data(Pos) $Pos]==-1 } {
      lappend data(Pos) $Pos
   }
   set data(Items$Pos) {}
   set data(Pos$Pos)   $Coords
   set i -1

   Graph::Idle $GR Time

   foreach field $data(Data) {
      set item ${Pos}_Item[incr i]
      lappend data(Items$Pos) $item

      Graph::Time::ItemAdd $GR $item
      if { $Update } {
         Graph::Time::ItemData $GR $Pos $item $field
      }
   }

   Graph::Time::UpdateItems $data(FrameData) $GR
   Graph::Time::Graph $GR
   Graph::UnIdle $GR Time
}

#-------------------------------------------------------------------------------
# Nom      : <Graph::Time::ItemUnDefine>
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

proc Graph::Time::ItemUnDefine { GR Pos } {

   upvar #0 Graph::Time::Time${GR}::Data  data

   if { [set idx [lsearch -exact $data(Pos) $Pos]]!=-1 } {
      foreach item $data(Items$Pos) {
         Graph::Time::ItemDel $GR $item
      }

      set data(Pos) [lreplace $data(Pos) $idx $idx]
      set Graph::Data(Pos) [lindex $data(Pos) end]
   }
}

#-------------------------------------------------------------------------------
# Nom      : <Graph::Time::ItemData>
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

proc Graph::Time::ItemData { GR Pos Item Data } {
   global GDefs

   upvar #0 Graph::Time::Time${GR}::Data  data
   upvar #0 Graph::Time::Time${GR}::Graph graph

   if { [graphitem is $Item] && [llength $data(Data$Data)] && [llength $data(Pos$Pos)] } {

      vector set $Item.X {}
      vector set $Item.Y {}

      set lat [lindex $data(Pos$Pos) 0]
      set lon [lindex $data(Pos$Pos) 1]
      set df  [expr 100.0/([llength $data(Data$Data)]-1)]

      if { [fstdfield is $Data] } {

         foreach field $data(Data$Data) {
            if { [llength $data(Pos$Pos)]>2 } {
               set n 0
               set val [fstdfield stats [lindex $field 1] -avg $data(Pos$Pos)]
               SPI::Progress +$df "[lindex $Graph::Msg(Extracting) $GDefs(Lang)]"
            } else {
               set val  [fstdfield stats [lindex $field 1] -coordvalue $lat $lon]
            }
            set spd  [lindex $val 0]
            set dir  [lindex $val 1]

            if { $spd!="-" } {
               vector append $Item.X [lindex $field 0]
               vector append $Item.Y $spd
            }
         }
         SPI::Progress 0
         set graph(UnitY) [fstdfield configure $Data -unit]
      } elseif { [observation is $Data] } {

         foreach obs $data(Data$Data) {
            if { [info exists data(Obs$Pos)] && [set idx [lindex [observation define [lindex $obs 1] -IDX $data(Obs$Pos)] 0]]!="" } {

               set val  [observation define [lindex $obs 1] -DATA $idx]
               set spd  [lindex $val 0]
               set dir  [lindex $val 1]

               if { $spd!="-" } {
                  vector append $Item.X [lindex $obs 0]
                  vector append $Item.Y $spd
               }
            }
            SPI::Progress +$df "[lindex $Graph::Msg(Extracting) $GDefs(Lang)]"
         }
         set graph(UnitY) [observation configure $Data -unit]
      } elseif { [metobs is $Data] } {

          foreach val [metobs define $Data -ELEMENT $data(Obs$Pos) $data(Elem$Pos)] {
             vector append $Item.X [lindex $val 0]
             vector append $Item.Y [lindex $val 1]
          }
          set graph(UnitY) [metobs table -unit $data(Elem$Pos)]
      }
   }
   SPI::Progress 0
}

#-------------------------------------------------------------------------------
# Nom      : <Graph::Time::Update>
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

proc Graph::Time::Update { Frame { GR {} } } {
   variable Data

   if { ![llength $GR] } {
      set GR [Page::Registered All Graph::Time]
   }

   foreach gr $GR {

      upvar #0 Graph::Time::Time${gr}::Data  data

      if { $data(FrameData)==$Frame } {

         catch {
            $data(FrameData).page.canvas configure -cursor watch
            $data(Canvas) configure -cursor watch
            update idletasks
         }
         #----- Recuperer les donnees

         if { [Page::Registered All Viewport $data(VP)]!=-1 } {
            Graph::Time::Data $gr $Viewport::Data(Data$data(VP))
         }

         #----- Update des items

         foreach pos $data(Pos) {
            Graph::Time::ItemDefine $gr $pos $data(Pos$pos)
         }
         Graph::PosSet $gr Time

         catch {
            $data(Canvas) configure -cursor left_ptr
            $data(FrameData).page.canvas configure -cursor left_ptr
         }
      }
   }
}

#-------------------------------------------------------------------------------
# Nom      : <Graph::Time::UpdateItems>
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

proc Graph::Time::UpdateItems { Frame { GR { } } } {
   global GDefs
   variable Data
   variable Lbl

   if { ![llength $GR] } {
      set GR [Page::Registered All Graph::Time]
   }

   foreach gr $GR {

      upvar #0 Graph::Time::Time${gr}::Data  data

      if { $data(VP)!="" && $data(FrameData)==$Frame } {

         $Frame.page.canvas delete GRAPHSELECT$gr

         foreach pos $data(Pos) {
            if { [llength $data(Items$pos)] } {
               set id [graphitem configure [lindex $data(Items$pos) 0] -desc]
               set desc [lindex [$data(Canvas) itemconfigure $id -text] end]
               switch [llength $data(Pos$pos)] {
                  2 { set type POINT }
                  4 { set type BOX }
                  default { set type POLYGON }
               }
               Graph::ItemPos $Frame $data(VP) $data(Pos$pos) "[lindex $Lbl(Title) $GDefs(Lang)]\n$desc" GRAPHSELECT$gr $type
            }
         }
      }
   }
}

#-------------------------------------------------------------------------------
# Nom      : <Graph::Time::Data>
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

proc Graph::Time::Data { GR { Data { } } { Files { } } } {
   global   GDefs

   upvar #0 Graph::Time::Time${GR}::Data  data
   upvar #0 Graph::Time::Time${GR}::Graph graph

   SPI::Progress 0

   Graph::Time::Clean $GR

   SPI::Progress 5 [lindex $Graph::Msg(Reading) $GDefs(Lang)]

   #----- Recuperer la suite temporelle pour chaque champs

   set data(Data)    {}
   set data(Tmp)     {}
   set data(ObsIds)  {}

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

         set data(Data$item) [MetData::FindAll TIME$GR$item $fids -1 [fstdfield define $item -ETIKET] [fstdfield define $item -IP1] \
            -1 $ip3 [fstdfield define $item -TYPVAR] [fstdfield define $item -NOMVAR]]
         eval lappend data(Tmp) $data(Data$item)

         #----- Check if number of time setp correspond when the calculatro is used
         if { $nbdata && [llength $data(Data$item)]!=$nbdata && [FieldCalc::IsOperand $data(VP)] } {
            Dialog::Error $data(Frame) $Graph::Error(NbData)
            break;
         }
         set nbdata [llength $data(Data$item)]

         FSTD::ParamUpdate $data(Data$item)

         #---- Trier temporellement les champs
         set i 0
         foreach id $data(Data$item) {
            set sec [fstdstamp toseconds [fstdfield define $id -DATEV]]
            lset data(Data$item) $i "$sec $id"
            incr i
         }
         set data(Data$item) [lsort -integer -increasing -index 0 $data(Data$item)]
         lappend data(Data) $item
      } elseif { [observation is $item] } {
         if { [set box  [lindex [observation stats $item -tag] end]]=="" } {
             continue
         }
         set i 0
         set data(Data$item) {}
         foreach id [ObsBox::GetContent $box] {
            if { [observation configure $item -desc]==[observation configure $id -desc] } {
               lappend data(Data$item) $id
               set data(ObsIds) [concat $data(ObsIds) [observation define $id -ID]]
            }
         }

         foreach id $data(Data$item) {
            set sec [observation define $id -DATE]
            lset data(Data$item) $i "$sec $id"
            incr i
         }
         set data(Data$item) [lsort -integer -increasing -index 0 $data(Data$item)]
         set data(ObsIds)    [lsort -unique -dictionary -increasing $data(ObsIds)]
         lappend data(Data)  $item
      } elseif { [metobs is $item] } {
         set data(Data$item) $item
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

   Graph::ParamsObsSearch Time $GR

   SPI::Progress 0
}
