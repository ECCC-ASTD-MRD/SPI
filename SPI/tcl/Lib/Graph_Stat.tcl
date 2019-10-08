#===============================================================================
# Environnement Canada
# Centre Meteorologique Canadian
# 2100 Trans-Canadienne
# Dorval, Quebec
#
# Projet   : Librairie d'objects visuel interactifs
# Fichier  : Graph_Stat.tcl
# Creation : Juillet 2005 - J.P. Gauthier - CMC/CMOE
#
# Description: Ce package s'occupe de l'affichage et de la manipulation de
#              statistiques de graph
#
# Fonctions:
#
#    Graph::Stat::Create  { Frame X0 Y0 Width Height Active Full }
#    Graph::Stat::Graph   { GR }
#    Graph::Stat::Init    { Frame }
#    Graph::Stat::Params  { Parent GR }
#    Graph::Stat::Update  { Frame { GR {} } }
#
#===============================================================================

namespace eval Graph::Stat { } {
   variable Lbl

   set Lbl(Title)    { "Statistiques" "Statistics" }
}

#----------------------------------------------------------------------------
# Nom      : <Graph::Stat::Create>
# Creation : Juillet 2005 - J.P. Gauthier - CMC/CMOE
#
# But      : Creer l'object graph et ses structures
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

proc Graph::Stat::Create { Frame X0 Y0 Width Height Active Full { Link True } } {
   global GDefs
   variable Data
   variable Lbl

   set gr [Graph::Stat::Init $Frame]
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
   set Graph::Data(Type$gr)     Stat       ;#Type de graph

   upvar #0 Graph::Stat::Stat${gr}::Data  data

   set data(Canvas)    $Frame.page.canvas
   set data(Frame)     $Frame

   $data(Canvas) create rectangle $X0 $Y0 [expr $X0+$Width] [expr $Y0+$Height] -outline black -fill $Graph::Color(Graph) \
        -tags "$tag $gr GRAPHOUT$gr" -width 1
   $data(Canvas) create text [expr $X0+5] [expr $Y0+5] -anchor nw -fill black -tags "$tag $gr GRAPHTEXT$gr" -font $Graph::Font(Graph)

   if { $Graph::Data(Graph)!="" && $Graph::Data(Type)!="Stat" } {
      set data(Graph)     $Graph::Data(Graph)
      set data(Type)      $Graph::Data(Type)
      Graph::Stat::Update $Frame $gr
   } else {
      set data(Graph)     ""
      set data(Type)      ""
   }

   Graph::Activate $Frame $gr Stat
   
   if { $Graph::Data(Link$gr) } {
      Graph::Mode $gr Stat False
   }
   #----- Creer les fonction du mode actif

   if { $Active } {
      Page::ActiveWrapper Graph $Frame $gr $X0 $Y0 [expr $Width+$X0] [expr $Height+$Y0] Stat
   } elseif { $Full } {
      Page::ActiveFull Graph $Frame $gr $Full
   }
   Page::Register $Frame Graph::Stat $gr

   return $gr
}

#-------------------------------------------------------------------------------
# Nom      : <Graph::Stat::Graph>
# Creation : Juillet 2005 - J.P. Gauthier - CMC/CMOE
#
# But      : Affiche le graphique de statistiques.
#
# Parametres :
#   <GR>     : Indentificateur du Graph
#
# Retour    :
#
# Remarque :
#
#-------------------------------------------------------------------------------

proc Graph::Stat::Graph { GR { Items {} } } {
   variable Data

   upvar #0 Graph::Stat::Stat${GR}::Data  data

   if { $data(Graph)=="" || $data(Type)=="" } {
      return
   }

   if { ![llength $Items] } {
      upvar #0 Graph::$data(Type)::$data(Type)$data(Graph)::Data  datagraph
      set Items $datagraph(Items)
   }

   #----- Recalculer les valeurs

   set text ""
   foreach item $Items {
      set text$item ""
   }

   foreach stat $Graph::Data(Stats) {
      if { $data(Sel$stat) } {
         set text "$text [format %-6s $stat]\n"
      }
   }

   $data(Canvas) itemconfigure GRAPHTOUT$GR -outline $Graph::Color(Frame) -fill $Graph::Color(Graph) -bd $Graph::Width(Frame) -width 2
   $data(Canvas) itemconfigure GRAPHTEXT$GR -text $text -fill black -font $Graph::Font(Graph)
   $data(Canvas) delete GRAPHITEM$GR

   set y [lindex [$data(Canvas) coords GRAPHTEXT$GR] 1]
   set x [lindex [$data(Canvas) bbox GRAPHTEXT$GR] 2]

   foreach item $Items {

      vexpr V sall($item.X,$item.Y)
      foreach stat $Graph::Data(Stats) func $Graph::Data(Funcs) {
         if { $data(Sel$stat) } {
            set val [vexpr V ${func}()]
            if { $val<1e-3 || $val>1e4 } {
               eval set text$item \"\$text$item \[format %.3e $val\]\\n\"
            } else {
               eval set text$item \"\$text$item \[format %.4f $val\]\\n\"
            }
         }
      }

      eval $data(Canvas) create text [expr $x+5] $y -text \$text$item -fill \[graphitem configure $item -outline\] -font $Graph::Font(Graph) \
         -tags \"PAGE$GR $GR GRAPHITEM$GR\" -anchor nw

      set x [lindex [$data(Canvas) bbox GRAPHITEM$GR] 2]
   }

   update idletasks
   $data(Canvas) config -cursor left_ptr
}

#----------------------------------------------------------------------------
# Nom      : <Graph::Stat::Init>
# Creation : Juillet 2005 - J.P. Gauthier - CMC/CMOE
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

proc Graph::Stat::Init { Frame } {
   global   GDefs
   variable Data

   set gr GR[incr Graph::Data(Nb)]

   namespace eval Stat$gr {
      variable Data
      variable Graph

      set Data(Pos)             {}        ;#Liste des positions

      set Graph(UnitY)    "" ;#Descriptif de l'echelle des valeur en Y
      set Graph(UnitX)    "" ;#Descriptif de l'echelle des valeur en X

      foreach stat $Graph::Data(Stats) {
         set Data(Sel$stat) 1
      }
   }
   return $gr
}

#-------------------------------------------------------------------------------
# Nom      : <Graph::Stat::Params>
# Creation : Juillet 2005 - J.P. Gauthier - CMC/CMOE -
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

proc Graph::Stat::Params { Parent GR } {
   global   GDefs

   labelframe $Parent.stat -text [lindex $Graph::Lbl(Stat) $GDefs(Lang)]
      frame $Parent.stat.list -relief sunken -bd 1
      pack $Parent.stat.list -side top -fill x
   pack $Parent.stat -side top -fill x -pady 5

   foreach stat $Graph::Data(Stats) {
      checkbutton $Parent.stat.list.s$stat -text $stat -bd 1 -variable Graph::Stat::Stat${GR}::Data(Sel$stat) \
         -onvalue 1 -offvalue 0 -indicatoron false -command "Graph::Stat::Graph $GR"
      pack $Parent.stat.list.s$stat -side top -fill x
   }

   Graph::ModeSelect NONE
}

#-------------------------------------------------------------------------------
# Nom      : <Graph::Stat::Update>
# Creation : Juillet 2005 - J.P. Gauthier - CMC/CMOE
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

proc Graph::Stat::Update { Frame { GR {} } } {
   variable Data

   if { ![llength $GR] } {
      set GR [Page::Registered All Graph::Stat]
   }

   foreach gr $GR {

      upvar #0 Graph::Stat::Stat${gr}::Data data

      if { $data(Graph)!="" && $data(Type)!="" } {

         eval set frame \$Graph::$data(Type)::$data(Type)$data(Graph)::Data(FrameData)

         if { $frame==$Frame } {

            catch {
               $frame.page.canvas configure -cursor watch
               $data(Canvas) configure -cursor watch
               update idletasks
            }

            Graph::Stat::Graph $GR

            catch {
               $data(Canvas) configure -cursor left_ptr
               $frame.page.canvas configure -cursor left_ptr
            }
         }
      }
   }
}
