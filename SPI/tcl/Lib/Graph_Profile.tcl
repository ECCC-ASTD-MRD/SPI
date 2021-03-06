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

   set Lbl(Title)     { "Profil vertical" "Vertical profile" }
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

proc Graph::Profile::Create { Frame X0 Y0 Width Height Active Full { Link True } } {
   global GDefs
   variable Data
   variable Lbl

   set gr [Graph::Profile::Init $Frame]
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
   set Graph::Data(Type$gr)     Profile    ;#Type de graph

   upvar #0 Graph::Profile::Profile${gr}::Data  data
   upvar #0 Graph::Profile::Profile${gr}::Graph graph

   set data(Canvas)    $Frame.page.canvas
   set data(Frame)     $Frame

   $data(Canvas) bind GRAPHUPDATE$gr <Any-KeyRelease> "$data(Canvas) itemconfigure $gr -update True"
   set id [$data(Canvas) create text $X0 $Y0  -tags "$tag CVTEXT GRAPHUPDATE$gr" -text [lindex $Lbl(Title) $GDefs(Lang)] \
      -font $Graph::Font(Graph) -fill black -anchor nw -justify center]
   $data(Canvas) create graph -x $X0 -y $Y0 -width $Width -height $Height -xlegend 5 -ylegend 5 -command $gr \
       -fg black -bg $Graph::Color(Frame) -fill $Graph::Color(Graph) -tags "$tag $gr" -font $Graph::Font(Graph) -title $id
   $data(Canvas) raise $id

   if { $Viewport::Data(VP)!="" } {
      set data(VP)        $Viewport::Data(VP)
      set data(FrameData) $Viewport::Data(Frame$data(VP))
      Graph::Profile::Update $data(FrameData) $gr
   }

   Graph::Activate $Frame $gr Profile

   graphaxis create axisx$gr
   graphaxis create axisy$gr

   if { $Graph::Data(Link$gr) } {
      #----- Creation des unite de l'echelle

      set id [$data(Canvas) create text -100 -100  -tags "$tag CVTEXT GRAPHUPDATE$gr" -text $graph(UnitX) \
         -font $Graph::Font(Axis) -fill $Graph::Color(Axis) -anchor nw -justify center]
      graphaxis configure axisx$gr -font $Graph::Font(Axis) -color $Graph::Color(Axis) -gridcolor $Graph::Grid(XColor) \
         -dash $Graph::Grid(XDash) -position LL -width 1 -unit $id

      set id [$data(Canvas) create text -100 -1000  -tags "$tag CVTEXT GRAPHUPDATE$gr" -text $graph(UnitY) \
         -font $Graph::Font(Axis) -fill $Graph::Color(Axis) -anchor nw -justify center]
      graphaxis configure axisy$gr -font $Graph::Font(Axis) -color $Graph::Color(Axis) -gridcolor $Graph::Grid(YColor) \
         -dash $Graph::Grid(YDash) -position LL -width 1 -unit $id

      Graph::Mode $gr Profile True
      Graph::PosAdd $gr Profile
   }
   #----- Creer les fonction du mode actif

   if { $Active } {
      Page::ActiveWrapper Graph $Frame $gr $X0 $Y0 [expr $Width+$X0] [expr $Height+$Y0] Profile
   } elseif { $Full } {
      Page::ActiveFull Graph $Frame $gr $Full
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

      if { [llength $coords]==2  } {
          catch {
            set Page::Data(Coord) "[lindex $Graph::Lbl(Level) $GDefs(Lang)]: [format "%1.3e" [lindex $coords 1]]"
            set Page::Data(Value) "[lindex $Graph::Lbl(Val) $GDefs(Lang)]: [FSTD::FieldFormat $data(Data) [lindex $coords 0]]"
         }
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
      set data(Levels) [concat $data(Levels) [vector get $item.Y]]
   }

   if { [vector length $item.Y] && [vector get $item.Y 0]<[vector get $item.Y end] } {
      set data(Order) -increasing
   } else {
      set data(Order) -decreasing
   }

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
   graphaxis configure axisx$GR -type $graph(XScale) -modulo $mod -min $xmin -max $xmax -intervals $xinter  -increment $xincr -angle $graph(XAngle) \
      -font $Graph::Font(Axis) -gridcolor $Graph::Grid(XColor) -dash $Graph::Grid(XDash) -gridwidth $Graph::Grid(XWidth) -color $Graph::Color(Axis) \
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

      set Data(Items)     {}        ;#Liste des items
      set Data(Pos)       {}        ;#Liste des positions
      set Data(Coords)    {}        ;#Liste des coordonnees de coupe
      set Data(Data)      {}        ;#Liste des champs selectionnees
      set Data(Tmp)       {}        ;#Liste des champs temporaire
      set Data(ObsIds)    {}        ;#Liste des positions observations
      set Data(ObsToken)  ""        ;#Token de recherche
      set Data(Field)     ""        ;#Champs de donnees
      set Data(VP)        ""
      set Data(FrameData) ""

      #----- Constantes relatives au Graph

      set Graph(UnitY)     "[lindex $Graph::Lbl(Unit) $GDefs(Lang)] Y"         ;#Descriptif de l'echelle des valeur en Y
      set Graph(UnitX)     "[lindex $Graph::Lbl(Unit) $GDefs(Lang)] X"         ;#Descriptif de l'echelle des valeur en X
      set Graph(XScale)    LINEAR                                              ;#Type d'echelle en X
      set Graph(YScale)    LINEAR                                              ;#Type d'echelle en Y
      set Graph(XInter)    ""                                                  ;#Liste des niveau specifie par l'usager
      set Graph(YInter)    ""                                                  ;#Liste des niveau specifie par l'usager
      set Graph(ZXInter)   ""                                                  ;#Liste des Niveaux (Mode Zoom)
      set Graph(ZYInter)   ""                                                  ;#Liste des Niveaux (Mode Zoom)
      set Graph(ZType)     GRID                                                ;#Type de niveaux (GRID,PRESSSURE)
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

   Graph::ParamsPos  $Parent
   Graph::ParamsItem $Parent
   Graph::ParamsAxis $Parent $GR Profile X
   Graph::ParamsAxis $Parent $GR Profile Y VERTICAL
   Graph::ParamsObs  $Parent $GR Profile
   Graph::ModeSelect POINT { POINT BOX POLYGON } { AVG MIN MAX }
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
      vector dim    $Item { X Y D }
      set id [$data(Canvas) create text -100 -100 -tags "PAGE$GR CVTEXT GRAPHUPDATE$GR" -text $Item -anchor nw -justify left]

      graphitem free   $Item
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
   set Graph::Item(Size)        3
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

   #----- If it's invalid or a profile field
   if { $Pos=="" || [string match DATA_* $Pos] } {
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
   set data(Items$Pos)  {}
   set data(Coords$Pos) $Coords
   set data(Pos$Pos)    $Coords
   set data(ZTypes$Pos) $Graph::Param(AxisZs)
   set i -1

   Graph::Idle $GR Profile

   foreach field $data(Data) {
      if { [fstdfield define $field -GRTYP]!="V"} {
         set item ${Pos}_Item[incr i]
         lappend data(Items$Pos) $item

         Graph::Profile::ItemAdd $GR $item
         if { $Update } {
            Graph::Profile::ItemData $GR $Pos $item $field
         }
      }
   }

   Graph::Profile::UpdateItems $data(FrameData) $GR
   Graph::Profile::Graph $GR
   Graph::UnIdle $GR Profile
}

proc Graph::Profile::ItemDefineV { GR { Update True } } {

   upvar #0 Graph::Profile::Profile${GR}::Data  data

   Graph::Idle $GR Profile

   #----- Cleanup previous items
   foreach pos $data(Pos) {
      if { [string match DATA_* $pos] } {
         Graph::Profile::ItemUnDefine $GR $pos
      }
   }

   set i 0
   foreach field $data(Data) {
      if { [fstdfield define $field -GRTYP]=="V" && [fstdfield define $field -FID]!="" } {

         set Pos DATA_[fstdfield define $field -NOMVAR]_[incr i]

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

         Graph::Profile::ItemAdd $GR $item
         if { $Update } {
            Graph::Profile::ItemData $GR $Pos $item $field
         }
      }
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

      set data(Pos) [lreplace $data(Pos) $idx $idx]
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
   global GDefs

   upvar #0 Graph::Profile::Profile${GR}::Data  data
   upvar #0 Graph::Profile::Profile${GR}::Graph graph

   if { [graphitem is $Item] && [llength $data(Pos$Pos)] } {

      if { [fstdfield is $Data] } {
         if { [fstdfield define $Data -GRTYP]=="V" } {
             fstdfield free GRAPHPROFILE
             fstdfield copy GRAPHPROFILE $Data
             fstdfield configure GRAPHPROFILE -extrude $graph(ZType)
         } else {
            if { $graph(ZType)=="GRID" } {
               fstdfield configure $Data -ztype UNDEFINED
            } else {
               fstdfield configure $Data -ztype $graph(ZType)
            }

            if { [llength $data(Pos$Pos)]>2 } {
               set n   0
               set ijs [fstdfield stats $Data -within $data(Pos$Pos)]
               set df  [expr 100.0/([llength $ijs]-1)]

               fstdfield free GRAPHPROFILE

               foreach ij $ijs {
                  fstdfield vertical GRAPHPROFILE_TMP $Data [fstdfield stats $Data -gridpoint [lindex $ij 0] [lindex $ij 1]] True
                  if { [fstdfield is GRAPHPROFILE] } {
                     switch $Graph::Param(SelectType) {
                        AVG { vexpr GRAPHPROFILE GRAPHPROFILE+GRAPHPROFILE_TMP }
                        MIN { vexpr GRAPHPROFILE min(GRAPHPROFILE,GRAPHPROFILE_TMP) }
                        MAX { vexpr GRAPHPROFILE max(GRAPHPROFILE,GRAPHPROFILE_TMP) }
                        MED { }
                        STD { }
                     }
                  } else {
                     fstdfield copy GRAPHPROFILE GRAPHPROFILE_TMP
                  }
                  incr n
                  SPI::Progress +$df "[lindex $Graph::Msg(Extracting) $GDefs(Lang)]"
               }
               if { !$n } {
                  return
               }
               fstdfield free GRAPHPROFILE_TMP

               if { $Graph::Param(SelectType)=="AVG" } {
                  vexpr GRAPHPROFILE GRAPHPROFILE/$n
               }
               SPI::Progress 0
            } else {
               fstdfield vertical GRAPHPROFILE $Data $data(Pos$Pos) True
            }
            FSTD::ParamUpdate GRAPHPROFILE
         }

         #----- Configure info label if allowed
         if { $Graph::Data(Update) } {
            set obj [graphitem configure $Item -desc]
            set type [fstdfield stats GRAPHPROFILE -leveltype]
            switch $type {
               "HYBRID" { $data(Canvas) itemconfigure $obj -text "[fstdfield define GRAPHPROFILE -NOMVAR] [fstdfield stats GRAPHPROFILE -leveltype] (ptop=[format %.2f [fstdfield stats GRAPHPROFILE -top]] pref=[format %.2f [fstdfield stats GRAPHPROFILE -ref]] rcoef=[format %.2f [lindex [fstdfield stats GRAPHPROFILE -coef] 0]],[format %.2f [lindex [fstdfield stats GRAPHPROFILE -coef] 1]])" }
               "ETA"    { $data(Canvas) itemconfigure $obj -text "[fstdfield define GRAPHPROFILE -NOMVAR] [fstdfield stats GRAPHPROFILE -leveltype] (ptop=[format %.2f [fstdfield stats GRAPHPROFILE -top]])" }
               default  { $data(Canvas) itemconfigure $obj -text "[fstdfield define GRAPHPROFILE -NOMVAR]" }
            }
         }

         #----- Check for vertical coordinate selection
         switch $graph(ZType) {
            "PRESSURE" {
               set levels [fstdfield stats GRAPHPROFILE -pressurelevels]
               if { ![llength $levels] } {
                  Dialog::Error . $Graph::Error(Pressure)
               }
               vector set $Item.Y $levels
               set graph(UnitY)  [lindex $Graph::Lbl(PRESSURE) $GDefs(Lang)]
            }
            "MASL" {
               set levels [fstdfield stats GRAPHPROFILE -meterlevels]
               if { ![llength $levels] } {
                  Dialog::Error . $Graph::Error(Meter)
               }
               vector set $Item.Y $levels
               set graph(UnitY) [lindex $Graph::Lbl(MASL) $GDefs(Lang)]
            }
            default {
               vector set $Item.Y [fstdfield stats GRAPHPROFILE -levels]
               if { [set ztype [fstdfield configure GRAPHPROFILE -extrude]]!="" } {
                  set graph(UnitY) $ztype
               } else {
                  set graph(UnitY) [fstdfield stats $Data -leveltype]
               }
            }
         }

         vector set $Item.X [fstdfield define GRAPHPROFILE -DATA 0]

         if { [set graph(UnitX) [fstdfield configure $Data -unit]]=="" } {
            set graph(UnitX) UNDEFINED
         }
         
         if { $Graph::Item(Icon)=="BARB" && [fstdfield stats GRAPHPROFILE -component]>1 } {
            vector set $Item.D [fstdfield define GRAPHPROFILE -DATA 1]
            graphitem configure $Item -speed $Item.X -dir $Item.D
         } else {
            graphitem configure $Item -speed "" -dir ""
         }

      } elseif { [observation is $Data] } {

         set lst {}
         vector set $Item.X {}
         vector set $Item.Y {}
         vector set $Item.D {}

         if { [info exists data(Obs$Pos)] } {
            foreach obs $data(Data$Data) {
               foreach idx [observation define $obs -IDX $data(Obs$Pos)] {
                  if { [set val [lindex [observation define $obs -DATA $idx] 0]]!="-" } {
                     lappend lst [list $val [lindex [observation define $obs -COORD $idx] 2]]
                  }
               }
            }
            set lst [lsort -index 1 -real -increasing $lst]
            foreach l $lst {
               vector append $Item $l
            }
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

   #----- Check if we're animating on vertical
   if { [namespace exists ::Animator] && $Animator::Play(Mode)=="IP1" } {
      return
   }

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
            }
         }

         #----- Update des items
         foreach pos $data(Pos) {
            Graph::Profile::ItemDefine $gr $pos $data(Coords$pos)
         }
         Graph::Profile::ItemDefineV $gr
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

   #----- Check if we're animating on vertical
   if { [namespace exists ::Animator] && $Animator::Play(Mode)=="IP1" } {
      return
   }

   if { ![llength $GR] } {
      set GR [Page::Registered All Graph::Profile]
   }

   foreach gr $GR {

      upvar #0 Graph::Profile::Profile${gr}::Data  data

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

   upvar #0 Graph::Profile::Profile${GR}::Data  data

   SPI::Progress 0 [lindex $Graph::Msg(Reading) $GDefs(Lang)]

   #----- Recuperer les champs correspondants du viewport actif

   set data(Data)   {}
   set fields       {}
   set data(ObsIds) {}
   set nb [expr 95.0/([llength $Data]+1)]
   SPI::Progress +$nb

   foreach item $Data {
      if { [fstdfield is $item] && [fstdfield define $item -FID]!="" } {
         if { [fstdfield define $item -GRTYP]!="V"} {
            if { $Graph::Data(IP3) } {
               fstdfield readcube $item
            } else {
               set ip3 [fstdfield define $item -IP3]
               fstdfield define $item -IP3 -1
               fstdfield readcube $item
               fstdfield define $item -IP3 $ip3
            }
         }
         lappend fields $item
      } elseif { [observation is $item] } {
         if { [set box  [lindex [observation stats $item -tag] end]]=="" } {
             continue
         }
 #        set data(ObsIds) [concat $data(ObsIds) [observation define $item -ID]]

         set i 0
         set data(Data$item) {}
         foreach id [ObsBox::GetContent $box] {
            if { [observation configure $item -desc]==[observation configure $id -desc] &&
                 [observation define $item -DATE]==[observation define $id -DATE] } {

               lappend data(Data$item) $id
               set data(ObsIds) [concat $data(ObsIds) [observation define $id -ID]]
            }
         }

         set data(Data$item) [lsort -dictionary -increasing -index 0 $data(Data$item)]
         set data(ObsIds)    [lsort -unique -dictionary -increasing $data(ObsIds)]
         lappend data(Data) $item
      }
      SPI::Progress +$nb  [lindex $Graph::Msg(Reading) $GDefs(Lang)]
   }
   Graph::ParamsObsSearch Profile $GR

   #----- Applique le calcul MACRO au cubes de donnees
   foreach field [concat $fields [FieldCalc::Operand $data(VP) $fields]] {
      if { [fstdfield configure $field -active] } {
        lappend data(Data) $field
      }
   }

   SPI::Progress 0
}
