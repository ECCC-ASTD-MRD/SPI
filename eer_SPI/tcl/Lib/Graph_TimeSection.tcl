#===============================================================================
# Environnement Canada
# Centre Meteorologique Canadian
# 2100 Trans-Canadienne
# Dorval, Quebec
#
# Projet   : Librairie d'objects visuel interactifs
# Fichier  : Graph_TimeSection.tcl
# Creation : Octobre 2003 - J.P. Gauthier - CMC/CMOE
#
# Description: Ce package s'occupe de l'affichage et de la manipulation de graphs
#              de serie temporelle
#
# Fonctions:
#
#    Graph::TimeSection::Create       { Frame X0 Y0 Width Height Active Full }
#    Graph::TimeSection::Coord        { Frame GR X Y }
#    Graph::TimeSection::Clean        { GR }
#    Graph::TimeSection::Graph        { GR }
#    Graph::TimeSection::Init         { Frame }
#    Graph::TimeSection::Params       { Parent GR }
#    Graph::TimeSection::ItemDefault  { GR Item }
#    Graph::TimeSection::ItemAdd      { GR Item }
#    Graph::TimeSection::ItemDel      { GR Item }
#    Graph::TimeSection::ItemDefine   { GR Pos Coords { Update True } }
#    Graph::TimeSection::ItemUnDefine { GR Pos }
#    Graph::TimeSection::ItemData     { GR Pos Item Data }
#    Graph::TimeSection::Update       { Frame { GR {} } }
#    Graph::TimeSection::UpdateItems  { Frame { GR { } } }
#    Graph::TimeSection::Data         { GR { Data { } } { Files { } } }
#
#===============================================================================

namespace eval Graph::TimeSection { } {
   variable Lbl

   set Lbl(Title)     { "Profil temporel" "Time profile" }
}

#----------------------------------------------------------------------------
# Nom      : <Graph::TimeSection::Create>
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

proc Graph::TimeSection::Create { Frame X0 Y0 Width Height Active Full { Link True } } {
   global GDefs
   variable Data
   variable Lbl

   set gr [Graph::TimeSection::Init $Frame]
   set tag PAGE$gr

   if { $Full } {
      set X0       0
      set Y0       0
      set Width   [winfo width  $Page::Data(Canvas)]
      set Height  [winfo height $Page::Data(Canvas)]
   }

   set Graph::Data(Full$gr)     $Full       ;#Mode FullCanvas
   set Graph::Data(Active$gr)   $Active     ;#Mode Active (Manipulation in place)
   set Graph::Data(X$gr)        $X0         ;#Offset en x
   set Graph::Data(Y$gr)        $Y0         ;#Offset en y
   set Graph::Data(Width$gr)    $Width      ;#Largeur du graph
   set Graph::Data(Height$gr)   $Height     ;#Hauteur du graph
   set Graph::Data(Link$gr)     $Link      ;#Liaison des donnees a l'interface
   set Graph::Data(Type$gr)     TimeSection ;#Type de graph

   upvar #0 Graph::TimeSection::TimeSection${gr}::Data  data
   upvar #0 Graph::TimeSection::TimeSection${gr}::Graph graph

   set data(Canvas)    $Frame.page.canvas
   set data(Frame)     $Frame

   $data(Canvas) bind GRAPHUPDATE$gr <Any-KeyRelease> "$data(Canvas) itemconfigure $gr -update True"
   set id [$data(Canvas) create text $X0 $Y0  -tags "$tag CVTEXT GRAPHUPDATE$gr" -text [lindex $Lbl(Title) $GDefs(Lang)] \
      -font $Graph::Font(Graph) -fill black -anchor nw -justify center]
   $data(Canvas) create graph -x $X0 -y $Y0 -width $Width -height $Height -legend 0 -command $gr \
       -fg black -bg $Graph::Color(Frame) -fill $Graph::Color(Graph) -tags "$tag $gr" -font $Graph::Font(Graph) \
       -legend False -title $id
   $data(Canvas) raise $id

   if { $Viewport::Data(VP)!="" } {
      set data(VP)        $Viewport::Data(VP)
      set data(FrameData) $Viewport::Data(Frame$data(VP))
      Graph::TimeSection::Update $data(FrameData) $gr
   }

   Graph::Activate $Frame $gr TimeSection
   
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

      Graph::Mode $gr TimeSection True
      Graph::PosAdd $gr TimeSection
   }
   
   #----- Creer les fonction du mode actif

   if { $Active } {
      Page::ActiveWrapper Graph $Frame $gr $X0 $Y0 [expr $Width+$X0] [expr $Height+$Y0] TimeSection
   } elseif { $Full } {
      Page::ActiveFull Graph $Frame $gr $Full
   }
   Page::Register $Frame Graph::TimeSection $gr

   return $gr
}

#-------------------------------------------------------------------------------
# Nom      : <Graph::TimeSection::Coord>
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

proc Graph::TimeSection::Coord { Frame GR X Y } {
   global   GDefs

   upvar #0 Graph::TimeSection::TimeSection${GR}::Data  data
   upvar #0 Graph::TimeSection::TimeSection${GR}::Graph graph

   set Page::Data(Coord) ""
   set Page::Data(Value) ""

   if  { [llength [set items [lindex [$data(Canvas) itemconfigure $GR -item] end]]] } {

      set coords [$GR -unproject $X $Y False [lindex $items 0]]

      if { [llength $data(Dates)] && [llength $coords]>=2 } {
         catch {
            set idx [lindex $coords 0]
            set sec0 [lindex $data(Dates) [expr int($idx)]]
            set sec1 [lindex $data(Dates) [expr int($idx)+1]]
            set sec [expr $sec0+($idx-int($idx))*($sec1-$sec0)]
            set date [DateStuff::StringDateFromSeconds [expr $sec>1e31?0:$sec<1e-32?0:$sec] $GDefs(Lang)]
            set Page::Data(Coord) "[lindex $Graph::Lbl(Level) $GDefs(Lang)]: [format "%1.3e" [lindex $coords 1]] $date"

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
# Nom      : <Graph::TimeSection::Clean>
# Creation : Octobre 2002 - J.P. Gauthier - CMC/CMOE -
#
# But      : Supprimer les donnees associees aux coordonees.
#
# Parametres :
#   <GR>     : Identificateur du Graph
#
#-------------------------------------------------------------------------------

proc Graph::TimeSection::Clean { GR } {

   upvar #0 Graph::TimeSection::TimeSection${GR}::Data  data

   foreach item $data(Data) {
      foreach field $data(Data$item) {
         fstdfield free [lindex $field 1]
      }
   }
}

#-------------------------------------------------------------------------------
# Nom      : <Graph::TimeSection::Graph>
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

proc Graph::TimeSection::Graph { GR } {
   global GDefs
   variable Data

   upvar #0 Graph::TimeSection::TimeSection${GR}::Graph graph
   upvar #0 Graph::TimeSection::TimeSection${GR}::Data  data

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
   set data(Dates)  {}
   set data(Levels) {}
   set yincr  0
   set xincr  0
   set mod True

   #----- Extraire les limites des valeurs
   foreach item $data(Items) {
      set data(Dates) [concat $data(Dates) [vector get $item.X]]

      if { [fstdfield is TIMESECTION$item] } {
         #----- Check for vertical coordinate selection
         switch $graph(ZType) {
            "PRESSURE" {
               set levels [fstdfield stats TIMESECTION$item -pressurelevels]
               if { ![llength $levels] } {
                  Dialog::Error . $Graph::Error(Pressure)
               }
               fstdfield configure TIMESECTION$item -ztype PRESSURE
               set data(Levels) $levels
               set graph(UnitY) [lindex $Graph::Lbl(PRESSURE) $GDefs(Lang)]
            }
            "MASL" {
               set levels [fstdfield stats TIMESECTION$item -meterlevels]
               if { ![llength $levels] } {
                  Dialog::Error . $Graph::Error(Meter)
               }
               fstdfield configure TIMESECTION$item -ztype MASL
               set data(Levels) $levels
               set graph(UnitY) [lindex $Graph::Lbl(MASL) $GDefs(Lang)]
            }
            "GRID" {
                fstdfield configure TIMESECTION$item -ztype UNDEFINED
                set data(Levels) [fstdfield stats TIMESECTION$item -levels]
                set graph(UnitY) [fstdfield stats TIMESECTION$item -leveltype]
            }
            default {
                set data(Levels) [fstdfield stats TIMESECTION$item -levels]
                set graph(UnitY) [fstdfield stats TIMESECTION$item -leveltype]
             }
         }
      } else {
         return
      }
   }

   set data(Dates) [lsort -unique -real -increasing $data(Dates)]
   set data(XMin)  [lindex $data(Dates) 0]
   set data(XMax)  [lindex $data(Dates) end]

   if { ![llength $data(Dates)] } {
      return
   }

   if { $data(Date0)!="" } {
      set data(XMin) [clock scan $data(Date0) -timezone :UTC]
   }
   if { $data(Date1)!="" } {
      set data(XMax) [clock scan $data(Date1) -timezone :UTC]
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

   if { ![llength $graph(XInter)] } {
      set dates $data(Dates)
   } else {
      set data(Dates) $graph(XInter)
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

      set xinter {}
      set i -1
      foreach date $dates {
         lappend xdates [Graph::TimeFormat $date $data(Time) $data(XMin)]
         lappend xinter [incr i]
      }

   } else {
      set data(Time)   DATE
      set graph(UnitX) [lindex $Graph::Lbl(Date) $GDefs(Lang)]
   }

   set data(XMin)  0
   set data(XMax)  [expr [fstdfield define TIMESECTION$item -NI]-1]

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
   graphaxis configure axisx$GR -type $graph(XScale) -modulo $mod -min $xmin -max $xmax -intervals $xinter -labels $xdates -angle $graph(XAngle) \
      -font $Graph::Font(Axis) -gridcolor $Graph::Grid(XColor)  -dash $Graph::Grid(XDash) -gridwidth $Graph::Grid(XWidth) -color $Graph::Color(Axis) \
      -format $graph(XFormat) -decimal $graph(XDecimals) -relative True

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
# Nom      : <Graph::TimeSection::Init>
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

proc Graph::TimeSection::Init { Frame } {
   global   GDefs
   variable Data

   set gr GR[incr Graph::Data(Nb)]

   namespace eval TimeSection$gr {
      variable Data
      variable Graph

      set Data(Items)    {}        ;#Liste des items
      set Data(Pos)      {}        ;#Liste des positions
      set Data(Coords)   {}        ;#Liste des coordonnees de coupe
      set Data(Data)     {}        ;#Liste des champs selectionnees
      set Data(Tmp)      {}        ;#Liste des champs temporaire

      set Data(Dates)    {}
      set Data(Levels)   {}
      set Data(Date0)    ""
      set Data(Date1)    ""
      set Data(Vertical) {}
      set Data(Field)     ""       ;#Champs de donnees
      set Data(VP)        ""
      set Data(FrameData) ""

      #----- Constantes relatives au Graph

      set Graph(UnitY)     "[lindex $Graph::Lbl(Unit) $GDefs(Lang)] Y"     ;#Descriptif de l'echelle des valeur en Y
      set Graph(UnitX)     "[lindex $Graph::Lbl(Unit) $GDefs(Lang)] X"     ;#Descriptif de l'echelle des valeur en X
      set Graph(YScale)    LINEAR                                          ;#Type d'echelle en Y
      set Graph(XScale)    LINEAR                                          ;#Type d'echelle en Y
      set Graph(XInter)    ""                                              ;#Liste des niveau specifie par l'usager
      set Graph(YInter)    ""                                              ;#Liste des niveau specifie par l'usager
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

#----------------------------------------------------------------------------
# Nom      : <Graph::TimeSection::Params>
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

proc Graph::TimeSection::Params { Parent GR } {
   global   GDefs
   variable Lbl

   upvar #0 Graph::TimeSection::TimeSection${GR}::Data  data

   Graph::ParamsPos  $Parent
   Graph::ParamsAxis $Parent $GR TimeSection X TIME
   Graph::ParamsAxis $Parent $GR TimeSection Y VERTICAL
   Graph::ModeSelect POINT { POINT BOX POLYGON } { AVG MIN MAX }
}

#-------------------------------------------------------------------------------
# Nom      : <Graph::TimeSection::ItemAdd>
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

proc Graph::TimeSection::ItemAdd { GR Item } {

   upvar #0 Graph::TimeSection::TimeSection${GR}::Data  data

   if { [lsearch -exact $data(Items) $Item]==-1 } {
      vector free   $Item
      vector create $Item
      vector dim    $Item { X Y }

      set id [$data(Canvas) create text -100 -100  -tags "PAGE$GR CVTEXT GRAPHUPDATE$GR" -text $Item -anchor nw -justify left]

      graphitem free   $Item
      graphitem create $Item
      graphitem configure $Item -xaxis axisx$GR -yaxis axisy$GR -xdata $Item.X -ydata $Item.Y -orient X -desc $id

      lappend data(Items) $Item
      Graph::TimeSection::ItemDefault $GR $Item
      $data(Canvas) itemconfigure $GR -item $data(Items)
   }
}

#-------------------------------------------------------------------------------
# Nom      : <Graph::TimeSection::ItemDefault>
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

proc Graph::TimeSection::ItemDefault { GR Item } {

   upvar #0 Graph::TimeSection::TimeSection${GR}::Data  data

   set Graph::Item(Outline)     ""
   set Graph::Item(FillColor)   #FFFFFF
   set Graph::Item(Tranparency) 100
   set Graph::Item(Width)       1
   set Graph::Item(Size)        0
   set Graph::Item(Value)       False
   set Graph::Item(Dash)        ""
   set Graph::Item(Type)        LINE
   set Graph::Item(Fit)         LINEAR
   set Graph::Item(Icon)        ""
   set Graph::Item(Bitmap)      ""
   set Graph::Item(Stipple)     -1
   set Graph::Item(Image)       ""

   Graph::ItemConfigure $GR TimeSection $Item
}

#-------------------------------------------------------------------------------
# Nom      : <Graph::TimeSection::ItemDel>
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

proc Graph::TimeSection::ItemDel { GR Item } {

   upvar #0 Graph::TimeSection::TimeSection${GR}::Data  data

   if { [set idx [lsearch -exact $data(Items) $Item]]!=-1 } {
      set data(Items) [lreplace $data(Items) $idx $idx]
      $data(Canvas) delete [graphitem configure $Item -desc]
      $data(Canvas) itemconfigure $GR -item $data(Items)

      vector    free $Item
      graphitem free $Item
      fstdfield free TIMESECTION$Item

      FSTD::UnRegister TIMESECTION$Item
   }
}

#-------------------------------------------------------------------------------
# Nom      : <Graph::TimeSection::ItemDefine>
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

proc Graph::TimeSection::ItemDefine { GR Pos Coords { Update True } } {

   upvar #0 Graph::TimeSection::TimeSection${GR}::Data  data

   #----- If it's invalid or a profile field
   if { $Pos=="" || [string match PROFILE_* $Pos] } {
      return
   }
   
   if { [info exists Graph::TimeSection::TimeSection${GR}::Data(Items$Pos)] } {
      foreach item [lrange $data(Items$Pos) [llength $data(Data)] end] {
         Graph::TimeSection::ItemDel $GR $item
      }
   }

   if { [lsearch -exact $data(Pos) $Pos]==-1 } {
      lappend data(Pos) $Pos
   }
   set data(Items$Pos)  {}
   set data(Coords$Pos) $Coords
   set data(Pos$Pos)    $Coords
   set data(ZTypes$Pos) $Graph::Param(AxisZs)
   set i -1

   Graph::Idle $GR TimeSection

   foreach field $data(Data) {
      set item ${Pos}_Item[incr i]
      lappend data(Items$Pos) $item

      Graph::TimeSection::ItemAdd $GR $item
      if { $Update } {
         Graph::TimeSection::ItemData $GR $Pos $item $field
      }
   }

   Graph::TimeSection::UpdateItems $data(FrameData) $GR
   Graph::TimeSection::Graph $GR
   Graph::UnIdle $GR TimeSection
}

proc Graph::TimeSection::ItemDefineV { GR { Update True } } {

   upvar #0 Graph::TimeSection::TimeSection${GR}::Data  data

   Graph::Idle $GR TimeSection
   
   #----- Cleanup previous items
   foreach pos $data(Pos) {
      if { [string match PROFILE_* $pos] } {
         Graph::TimeSection::ItemUnDefine $GR $pos
      }
   }
   
   set i 0
   foreach field $data(Data) {
      if { [fstdfield define $field -GRTYP]=="V" && [fstdfield define $field -FID]!="" } {

         set Pos TPROF_[fstdfield define $field -NOMVAR]_[incr i]
       
         if { [lsearch -exact $data(Pos) $Pos]==-1 } {
            lappend data(Pos) $Pos
         }
         set coords [fstdfield stats $field -grid]
         
         set data(Items$Pos)  {}
         set data(Coords$Pos) $coords
         set data(Pos$Pos)    $coords
         set data(ZTypes$Pos) [lsearch -all -inline -regexp [fstdfile info [fstdfield define $field -FID] NOMVAR] "^\\^{1}.."]

         set item ${Pos}_Item
         lappend data(Items$Pos) $item
         
         Graph::TimeSection::ItemAdd $GR $item
         Graph::TimeSection::Data $GR $field [fstdfield define $field -FID]
         
         if { $Update } {
            Graph::TimeSection::ItemData $GR $Pos $item $field
         }
      }
   }

   Graph::TimeSection::UpdateItems $data(FrameData) $GR
   Graph::TimeSection::Graph $GR
   Graph::UnIdle $GR TimeSection
}

#-------------------------------------------------------------------------------
# Nom      : <Graph::TimeSection::ItemUnDefine>
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

proc Graph::TimeSection::ItemUnDefine { GR Pos } {

   upvar #0 Graph::TimeSection::TimeSection${GR}::Data  data

   if { [set idx [lsearch -exact $data(Pos) $Pos]]!=-1 } {
      foreach item $data(Items$Pos) {
         Graph::TimeSection::ItemDel $GR $item
      }

      set data(Pos) [lreplace $data(Pos)  $idx $idx]
      set Graph::Data(Pos) [lindex $data(Pos) end]
   }
}

#-------------------------------------------------------------------------------
# Nom      : <Graph::TimeSection::ItemData>
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

proc Graph::TimeSection::ItemData { GR Pos Item Data } {
   global GDefs

   upvar #0 Graph::TimeSection::TimeSection${GR}::Data  data
   upvar #0 Graph::TimeSection::TimeSection${GR}::Graph graph

   if { [fstdfield is $Data] && [graphitem is $Item] && [llength $data(Data$Data)] && [llength $data(Pos$Pos)] } {

      if { [fstdfield define $Data -GRTYP]=="V" } {
         fstdfield free TIMESECTION$Item
         fstdfield create TIMESECTION$Item [llength $data(Data$Data)] [fstdfield define $Data -NJ] 1 Float32
         fstdfield copyhead TIMESECTION$Item $Data
         fstdfield define TIMESECTION$Item -georef [fstdfield define $Data -georef]
         set i 0
         foreach field $data(Data$Data) {
            vector append $Item.X [fstdstamp toseconds [fstdfield define [lindex $field 1] -DATEV]]
            vexpr TIMESECTION${Item} TIMESECTION${Item}($i)()()=[lindex $field 1]
            incr i
         }
         fstdfield configure TIMESECTION${Item} -extrude $graph(ZType)
      }  else {
      
      set fields {}
      foreach field $data(Data$Data) {
         if { $graph(ZType)=="GRID" } {
            fstdfield configure [lindex $field 1] -ztype UNDEFINED
         } else {
            fstdfield configure [lindex $field 1] -ztype $graph(ZType)
         }

         lappend fields [lindex $field 1]
         vector append $Item.X [fstdstamp toseconds [fstdfield define [lindex $field 1] -DATEV]]
      }

      if { [llength $data(Pos$Pos)]>2 } {
         set n   0
         set ijs [fstdfield stats $Data -within $data(Pos$Pos)]
         set df  [expr 100.0/([llength $ijs]-1)]

         fstdfield free TIMESECTION$Item
         foreach ij $ijs {
            fstdfield vertical TIMESECTION $fields [fstdfield stats $Data -gridpoint [lindex $ij 0] [lindex $ij 1]]
            if { [fstdfield is TIMESECTION$Item] } {
               switch $Graph::Param(SelectType) {
                  AVG { vexpr TIMESECTION$Item TIMESECTION$Item+TIMESECTION }
                  MIN { vexpr TIMESECTION$Item min(TIMESECTION$Item,TIMESECTION) }
                  MAX { vexpr TIMESECTION$Item max(TIMESECTION$Item,TIMESECTION) }
                  MED { }
                  STD { }
               }
            } else {
               fstdfield copy TIMESECTION$Item TIMESECTION
            }

            incr n
            SPI::Progress +$df "[lindex $Graph::Msg(Extracting) $GDefs(Lang)]"
         }
         if { !$n } {
            return
         }
         fstdfield free TIMESECTION
         if { $Graph::Param(SelectType)=="AVG" } {
            vexpr TIMESECTION$Item TIMESECTION$Item/$n
         }
         SPI::Progress 0
      } else {
         fstdfield vertical TIMESECTION$Item $fields $data(Pos$Pos)
      }
      }
      FSTD::Register TIMESECTION$Item

      graphitem configure $Item -xaxis axisx$GR -yaxis axisy$GR -data TIMESECTION$Item
   }
}

#-------------------------------------------------------------------------------
# Nom      : <Graph::TimeSection::Update>
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

proc Graph::TimeSection::Update { Frame { GR {} } } {
   variable Data

   if { ![llength $GR] } {
      set GR [Page::Registered All Graph::TimeSection]
   }

   foreach gr $GR {

      upvar #0 Graph::TimeSection::TimeSection${gr}::Data  data

      if { $data(FrameData)==$Frame } {

         catch {
            $data(FrameData).page.canvas configure -cursor watch
            $data(Canvas) configure -cursor watch
            update idletasks
         }

         #----- Recuperer les donnees
         if { [Page::Registered All Viewport $data(VP)]!=-1 } {
            Graph::TimeSection::Data $gr [Viewport::Assigned $Viewport::Data(Frame$data(VP)) $data(VP) fstdfield]
         }

         #----- Update des items
         foreach pos $data(Pos) {
            Graph::TimeSection::ItemDefine $gr $pos $data(Coords$pos)
         }
         Graph::TimeSection::ItemDefineV $gr
         Graph::PosSet $gr TimeSection

         catch {
            $data(Canvas) configure -cursor left_ptr
            $data(FrameData).page.canvas configure -cursor left_ptr
         }
      }
   }
}

#-------------------------------------------------------------------------------
# Nom      : <Graph::TimeSection::UpdateItems>
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

proc Graph::TimeSection::UpdateItems { Frame { GR { } } } {
   global GDefs
   variable Data
   variable Lbl

   if { ![llength $GR] } {
      set GR [Page::Registered All Graph::TimeSection]
   }

   foreach gr $GR {

      upvar #0 Graph::TimeSection::TimeSection${gr}::Data  data

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
# Nom      : <Graph::TimeSection::Data>
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

proc Graph::TimeSection::Data { GR { Data { } } { Files { } } } {
   global   GDefs

   upvar #0 Graph::TimeSection::TimeSection${GR}::Data  data
   upvar #0 Graph::TimeSection::TimeSection${GR}::Graph graph

   if { $data(Data)==$Data } {
      return
   }

   Graph::TimeSection::Clean $GR

   #----- Recuperer la suite temporelle pour chaque champs

   set data(Data)    {}
   set nb  1
   set sec ""

   foreach item $Data {

      if { [fstdfield is $item] } {
         if { ![llength $Files] } {
            if { [set box  [lindex [fstdfield stats $item -tag] 2]]=="" } {
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

         SPI::Progress 0 [lindex $Graph::Msg(Reading) $GDefs(Lang)]
         set lst [MetData::FindAll TIMESECTION$GR$item $fids -1 [fstdfield define $item -ETIKET] [fstdfield define $item -IP1] \
            -1 $ip3 [fstdfield define $item -TYPVAR] [fstdfield define $item -NOMVAR]]
         SPI::Progress 5 [lindex $Graph::Msg(Reading) $GDefs(Lang)]
         set nb [expr 95.0/[llength $lst]]

         #----- Set the interpolation degree to the same

         set interp [fstdfield configure $item -interpdegree]
         foreach field $lst {
            fstdfield configure $field -interpdegree $interp
         }

         #---- Trier temporellement les champs

         set data(Data$item) {}

         foreach id $lst {
            set grtyp [fstdfield define $id -GRTYP]
            if { $grtyp!="X"  && $grtyp!="Y" } {
               if { $grtyp!="V" } {
                  if { $Graph::Data(IP3) } {
                     fstdfield readcube $id
                  } else {
                     set ip3 [fstdfield define $id -IP3]
                     fstdfield define $id -IP3 -1
                     fstdfield readcube $id
                     fstdfield define $id -IP3 $ip3
                  }
               }
               set sec [fstdstamp toseconds [fstdfield define $id -DATEV]]
               lappend data(Data$item) "$sec $id"
            } else {
               fstdfield free $id
            }
            SPI::Progress +$nb "[lindex $Graph::Msg(Reading) $GDefs(Lang)] $sec"
         }
         set data(Data$item) [lsort -integer -increasing -index 0 $data(Data$item)]
         lappend data(Data) $item
      }
   }

   SPI::Progress 0
}
