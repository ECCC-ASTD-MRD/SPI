#==============================================================================
# Environnement Canada
# Centre Meteorologique Canadian
# 2100 Trans-Canadienne
# Dorval, Quebec
#
# Projet   : Package d'interface pour SPI
# Fichier  : Kriger.tcl
# Creation : Fevrier 2008
#
# Description:
#    Outils permettant d'effecture un kriging sur des observations qui permet de
#    definir la grille interactivement et les parametres de kriging
#
#===============================================================================

#----- Lire les sources d'execution

source $GDefs(Dir)/Apps/Tools/Kriger/Kriger.ctes
source $GDefs(Dir)/Apps/Tools/Kriger/Kriger.txt
source $GDefs(Dir)/Apps/Tools/Kriger/Kriger.int

#-------------------------------------------------------------------------------
# Nom      : <Kriger::Close>
# Creation : Fevrier 2008 - J.P. Gauthier - CMC/CMOE -
#
# But      : Ferme l'interface de l'outil.
#
# Parametres :
#
# Retour:
#
# Remarques :
#
#-------------------------------------------------------------------------------

proc Kriger::Close { } {
   variable Data

   #----- Si le mode etait celui de l'outils, revert to SPI

   if { $Page::Data(ToolMode)=="Kriger" } {
      SPI::ToolMode SPI Zoom
   }

   #----- Cleanup de l'outils

   set Data(Active) 0


   Graph::Destroy $Data(GraphFrame) $Data(Graph)
   Page::Destroy  $Data(GraphFrame)

   #----- Clear grid

   Viewport::UnAssign $Page::Data(Frame) $Viewport::Data(VP) KRIGGRID
   fstdfield free KRIGGRID KRIGTIC KRIGTAC KRIGTOC
   graphitem free KRIGITEM

   destroy .kriger

   if { !$SPI::Param(Window) } { SPI::Quit }
}

#-------------------------------------------------------------------------------
# Nom      : <Kriger::Process>
# Creation : Fevrier 2008 - J.P. Gauthier - CMC/CMOE -
#
# But      : Lance le calcul de krigage.
#
# Parametres :
#
# Retour:
#
# Remarques :
#
#-------------------------------------------------------------------------------

proc Kriger::Process { } {
   global GDefs
   variable Msg
   variable Lbl
   variable Data

   #----- Effectuer le krigging

   $Data(GraphFrame).page.canvas itemconfigure GRAPH$Data(Graph) -item {}

   if { [fstdfield is KRIGGRID] } {
      if { [fstdfield define KRIGGRID -NI]>500 || [fstdfield define KRIGGRID -NJ]>500 } {
         if { [Dialog::CreateDefault .kriger 200 WARNING $Msg(KrigSize) "" 1 $Lbl(Yes) $Lbl(No)] } {
            return
         }
      }
      set Data(Job)   [lindex $Msg(Krig) $GDefs(Lang)]
      update idletasks;

      if { ![observation is $Data(Obs)] } {
         Dialog::CreateError .kriger $Msg(ObsExist)
      } else {

         fstdfield gridinterp KRIGGRID $Data(Obs) $Data(Mode) $Data(Nugget) $Data(Sill) $Data(Range) $Data(Out)
#         fstdfield configure KRIGGRID -rendertexture 1 -colormap FLDMAPDefault -color black -font XFont10
#         FSTD::Register KRIGGRID

         if { ![graphitem is KRIGITEM] } {
            graphitem create KRIGITEM
         }
         graphaxis configure axisx$Data(Graph) -min 0 -max [fstdfield define KRIGGRID -NI] -increment 10
         switch $Data(GridType) {
            "Vertical" { graphaxis configure axisy$Data(Graph) -min $Data(VMin) -max $Data(VMax) -intervals [fstdfield stats KRIGGRID -levels]
                         Graph::Labels Section $Data(Graph) Krig "Grid X" [fstdfield stats KRIGGRID -leveltype]
                       }
            default    { graphaxis configure axisy$Data(Graph) -min 0 -max [fstdfield define KRIGGRID -NJ] -increment 10
                         Graph::Labels Section $Data(Graph) Krig "Grid X" "Grid Y"
                        }
         }
         graphitem configure KRIGITEM -xaxis axisx$Data(Graph) -yaxis axisy$Data(Graph) -data KRIGGRID

         $Data(GraphFrame).page.canvas itemconfigure GRAPH$Data(Graph) -item KRIGITEM
         Viewport::UpdateData $Page::Data(Frame)
      }
   } else {
      Dialog::CreateError .kriger $Msg(GridExist)
   }
   set Data(Job) ""
}

#-------------------------------------------------------------------------------
# Nom      : <Kriger::GridType>
# Creation : Fevrier 2008 - J.P. Gauthier - CMC/CMOE -
#
# But      : Changement des parametres de l'interface selon le type de grille.
#
# Parametres :
#
# Retour:
#
# Remarques :
#
#-------------------------------------------------------------------------------

proc Kriger::GridType { } {
   variable Data

   switch $Data(GridType) {
      "Vertical" { $Data(Tab).grid.vres.val configure -state normal
                   $Data(Tab).grid.vmin.val configure -state normal
                   $Data(Tab).grid.vmax.val configure -state normal
                 }
      "LatLon"   { $Data(Tab).grid.vres.val configure -state disabled
                   $Data(Tab).grid.vmin.val configure -state disabled
                   $Data(Tab).grid.vmax.val configure -state disabled
                  }
   }
   Kriger::Grid [set Data(Coords) {}]
}

#-------------------------------------------------------------------------------
# Nom      : <Kriger::Grid>
# Creation : Fevrier 2008 - J.P. Gauthier - CMC/CMOE -
#
# But      : Calcul de la grille destination.
#
# Parametres :
#
# Retour:
#
# Remarques :
#
#-------------------------------------------------------------------------------

proc Kriger::Grid { Coords } {
   global GDefs
   variable Data
   variable Lbl
   variable Msg

   set Data(Job)   [lindex $Msg(Grid) $GDefs(Lang)]
   update idletasks;

   #----- Clear previous grid

   Viewport::UnAssign $Page::Data(Frame) $Viewport::Data(VP) KRIGGRID
   fstdfield free KRIGGRID KRIGTIC KRIGTAC KRIGTOC
   $Data(GraphFrame).page.canvas itemconfigure GRAPH$Data(Graph) -item {}

   if { [llength $Coords]>2 } {

      switch $Data(GridType) {
         "Vertical" {
            set elevs  { }
            for { set h $Data(VMin) } { $h<=$Data(VMax) } { set h [expr $h+$Data(VResolution)] } {
               lappend elevs $h
            }
            set coords [projection function $Page::Data(Frame) -path $Coords $Data(HResolution)]

            set ni  [expr [llength $coords]/2]
            set nj  [llength $elevs]
            set nij [expr $ni+$nj]
            if { $ni==0 || $nj==0 } {
               return
               set Data(Job) ""
            }

            if { $ni>2000 || $nj>2000 } {
               Dialog::CreateError .kriger $Msg(GridSize) "\n\n\t$ni x $nj"
               set Data(Job) ""
               return
            }

            #----- Create new grid

            fstdfield create KRIGGRID $ni $nj 1 Float32
            fstdfield define KRIGGRID -NOMVAR KRIG -TYPVAR O -GRTYP V -ETIKET KRIG -IP1 12001 -IG1 $nij -IG2 $ni -IG3 $nj -IG4 0
            fstdfield stats KRIGGRID -leveltype MASL -levels $elevs -grid $coords

            fstdfield create KRIGTIC $ni 1 1 Float32
            fstdfield define KRIGTIC -NOMVAR ^^ -TYPVAR X -GRTYP L -ETIKET KRIG_GRIDV -IP1 $nij -IP2 $ni -IP3 $nj
            fstdfield create KRIGTAC 1 $ni 1 Float32
            fstdfield define KRIGTAC -NOMVAR >> -TYPVAR X -GRTYP L -ETIKET KRIG_GRIDV -IP1 $nij -IP2 $ni -IP3 $nj
            fstdfield create KRIGTOC $nj 1 1 Float32
            fstdfield define KRIGTOC -NOMVAR ^> -TYPVAR X -GRTYP X -ETIKET KRIG_GRIDV -IP1 $nij -IP2 $ni -IP3 $nj
            fstdfield define KRIGTOC -DATA [list $elevs]

            for { set i 0 } { $i < $ni } { incr i } {
               fstdfield stats KRIGTIC -gridvalue $i 0 [lindex $coords [expr $i*2]]
               fstdfield stats KRIGTAC -gridvalue 0 $i [lindex $coords [expr $i*2+1]]
            }
         }
         "LatLon" {
            set dlat [set dlon [expr $Data(HResolution)/(1852.0*60.0)]]
            set lat0 [lindex $Coords 0]
            set lat1 [lindex $Coords 2]
            set lon0 [lindex $Coords 1]
            set lon1 [lindex $Coords 3]
            if { $lat0>$lat1 } {
               set t $lat0
               set lat0 $lat1
               set lat1 $t
            }
            if { $lon0>$lon1 } {
               set t $lon0
               set lon0 $lon1
               set lon1 $t
            }

            set nj  [expr int(($lat1-$lat0)/$dlat)]
            set ni  [expr int(($lon1-$lon0)/$dlon)]
            set nij [expr $ni+$nj]
            if { $ni==0 || $nj==0 } {
               return
               set Data(Job) ""
            }

            if { $ni>2000 || $nj>2000 } {
               Dialog::CreateError .kriger $Msg(GridSize) "\n\n\t$ni x $nj"
              set Data(Job) ""
              return
            }

            fstdfield create KRIGGRID $ni $nj 1 Float32
            fstdfield define KRIGGRID -NOMVAR KRIG -TYPVAR O -ETIKET KRIG -IP1 12001 -IG1 $nij -IG2 $ni -IG3 $nj -IG4 0 -GRTYP Z

            fstdfield create KRIGTIC $ni 1 1 Float32
            fstdfield define KRIGTIC -NOMVAR >> -TYPVAR X -ETIKET KRIG_GRIDH -IP1 $nij -IP2 $ni -IP3 $nj -GRTYP L 0 0 1.0 1.0
            fstdfield create KRIGTAC 1 $nj 1 Float32
            fstdfield define KRIGTAC -NOMVAR ^^ -TYPVAR X -ETIKET KRIG_GRIDH -IP1 $nij -IP2 $ni -IP3 $nj -GRTYP L 0 0 1.0 1.0

            for { set i 0 } { $i < $ni } { incr i } {
               fstdfield stats KRIGTIC -gridvalue $i 0 [expr ($lon0+$dlon*$i)]
            }
            for { set j 0 } { $j < $nj } { incr j } {
               fstdfield stats KRIGTAC -gridvalue 0 $j [expr ($lat0+$dlat*$j)]
            }
            fstdfield define KRIGGRID -positional KRIGTIC KRIGTAC
         }
      }

      #----- Assign for display

      fstdfield configure KRIGGRID -rendertexture 1 -rendergrid 1 -colormap FLDMAPDefault -color black -font XFont10
      Viewport::Assign $Page::Data(Frame) $Viewport::Data(VP) KRIGGRID True
   }
   set Data(Job) ""
}
#-------------------------------------------------------------------------------
# Nom      : <Kriger::Save>
# Creation : Fevrier 2008 - J.P. Gauthier - CMC/CMOE -
#
# But      : Sauvegarde du resultat du krigage.
#
# Parametres :
#
# Retour:
#
# Remarques :
#
#-------------------------------------------------------------------------------

proc Kriger::Save { File } {
   variable Data

   if { $File!="" } {
      fstdfield define KRIGGRID -NOMVAR [string range [lindex [split $Data(Obs) .] 0] 0 3]
      FieldParams::Window KRIGGRID

      fstdfile open  KRIGFILE write $File
      switch $Data(GridType) {
         "Vertical" {
                    fstdfield write KRIGGRID KRIGFILE -32 True
                    fstdfield write KRIGTIC KRIGFILE -32 True
                    fstdfield write KRIGTAC KRIGFILE -32 True
                    fstdfield write KRIGTOC KRIGFILE -32 True
         }
         "LatLon" {
                    fstdfield write KRIGGRID KRIGFILE -32 True
                    fstdfield write KRIGTIC KRIGFILE -32 True
                    fstdfield write KRIGTAC KRIGFILE -32 True
         }
      }
      fstdfile close KRIGFILE
   }
}

#----------------------------------------------------------------------------
# Nom      : <Kriger::VertexAdd>
# Creation : Fevrier 2008 - J.P. Gauthier - CMC/CMOE
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

proc Kriger::VertexAdd { Frame VP X Y } {
   variable Data

   if { $VP==-1 } {
      return
   }

   set Data(Frame) $Page::Data(Frame)
   set Data(VP) $Viewport::Data(VP)

   if { $Data(GridType)=="Vertical" } {
      lappend Data(Coords) $Viewport::Map(LatCursor) $Viewport::Map(LonCursor)
   } else {
      if { [llength $Data(Coords)]==4 } {
         lset Data(Coords) 2 $Viewport::Map(LatCursor)
         lset Data(Coords) 3 $Viewport::Map(LonCursor)
      } else {
         lappend Data(Coords) $Viewport::Map(LatCursor) $Viewport::Map(LonCursor)
      }
   }
   Kriger::Grid $Data(Coords)
}

#----------------------------------------------------------------------------
# Nom      : <Kriger::VertexDelete>
# Creation : Fevrier 2008 - J.P. Gauthier - CMC/CMOE
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

proc Kriger::VertexDelete { Frame VP } {
   variable Data

   if { $VP==-1 } {
      return
   }

   if { $Data(GridType)=="Vertical" } {
      set Data(Coords) [lreplace $Data(Coords) end-1 end]
   } else {
      set Data(Coords) {}
   }
   Kriger::Grid $Data(Coords)
}

#----------------------------------------------------------------------------
# Nom      : <Kriger::VertexFollow>
# Creation : Fevrier 2008 - J.P. Gauthier - CMC/CMOE
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

proc Kriger::VertexFollow { Frame VP X Y Scan } {
   variable Data

   if { $VP==-1 || $Scan==0 } {
      return
   }

   if { ![llength $Data(Coords)] } {
      lappend Data(Coords) $Viewport::Map(LatCursor) $Viewport::Map(LonCursor)
   } else {
      if { $Data(GridType)=="Vertical" } {
         set coords $Data(Coords)
         lappend coords $Viewport::Map(LatCursor) $Viewport::Map(LonCursor)
      } else {
         set coords [lrange $Data(Coords) 0 1]
         lappend coords $Viewport::Map(LatCursor) $Viewport::Map(LonCursor)
      }
      Kriger::Grid $coords
   }
}

#-------------------------------------------------------------------------------
# Nom      : <Kriger::Update>
# Creation : Juin 2003 - J.P. Gauthier - CMC/CMOE
#
# But      : Effectuer le "Refresh" de l'outils apres une mise a jour dans SPI
#
# Parametres :
#   <Frame>  : Identificateur de Page
#
# Remarques :
#    - Cette fonctions est appele par SPI au besoin.
#
#-------------------------------------------------------------------------------

proc Kriger::Update { Frame } {
   variable Data

   ComboBox::DelAll $Data(Tab).krig.obs.val
   ComboBox::AddList $Data(Tab).krig.obs.val [observation all]
}

#-------------------------------------------------------------------------------
# Nom      : <Kriger::UpdateItems>
# Creation : Juin 2003 - J.P. Gauthier - CMC/CMOE
#
# But      : Effectuer le "Refresh" des items relatifs a cet outils sur
#            la projection.
#
# Parametres :
#   <Frame>  : Identificateur de Page
#   <VP>     : Identificateur du Viewport
#
# Remarques :
#    - Cette fonctions est appele par SPI au besoin.
#
#-------------------------------------------------------------------------------

proc Kriger::UpdateItems { Frame } {
   global   GDefs
   variable Data

   if { $Data(GraphFrame)!="" && [graphitem is KRIGITEM] } {
      catch { $Data(GraphFrame).page.canvas itemconfigure GRAPH$Data(Graph) -item KRIGITEM }
   }
}

#-------------------------------------------------------------------------------
# Nom      : <Kriger::PageActivate>
# Creation : Octobre 2003 - J.P. Gauthier - CMC/CMOE
#
# But      : Effectuer le "Refresh" des items relatifs a cet outils
#            lors d'un changement de page par l'usager.
#
# Parametres :
#   <Frame>  : Identificateur de Page
#
# Remarques :
#    - Cette fonctions est appele par SPI au besoin.
#
#-------------------------------------------------------------------------------

proc Kriger::PageActivate { Frame } {
}

#-------------------------------------------------------------------------------
# Nom      : <Kriger::AsProject>
# Creation : Aout 2006 - J.P. Gauthier - CMC/CMOE
#
# But      : Sauvegarder l'etat de l'outils dans un projet SPI.
#
# Parametres :
#   <File>   : Descripteur de fichier ou ecrire les commandes
#
# Remarques :
#    - Le fichier est deja ouvert, il suffit d'y ecrire les commandes a executer
#      afin de re-instaurer l'outils dans son etat actuel.
#
#-------------------------------------------------------------------------------

proc Kriger::AsProject { File } {
   variable Data
   variable Param

   if { [winfo exists .kriger] } {
      puts $File "#----- Tool: Kriger\n"
      puts $File "set Kriger::Param(Dock)   $Param(Dock)"
      puts $File "set Kriger::Param(Geom)   [winfo geometry .kriger]"
      puts $File "Kriger::Window"
      puts $File "\n"
   }
}