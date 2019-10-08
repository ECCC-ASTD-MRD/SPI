#===============================================================================
# Environnement Canada
# Centre Meteorologique Canadian
# 2100 Trans-Canadienne
# Dorval, Quebec
#
# Projet   : Serie temporelle de pseudo-concentrations
# Fichier  : ZonalGraph.tcl
# Creation : Novembre 2013 - J.P.Gauthier - CMC/CMOE
#
# Description:
#    Creer un graphe de moyenne zonal.
#
# Arguments :
#
# Remarques :
#   - Lancement: SPI -batch -layout Zonal -macro ZonalGraph.tcl -args [file] [var] [delta]
#===============================================================================

namespace eval Macro::ZonalGraph {
   variable Params

   set Param(Info)      { "Moyenne zonale" "Zonal average" }
   set Param(InfoArgs)  { { "Fichier" "Variable" "Delta" } { "File" "Variable" "Delta" } }

   set Param(File)      ""
   set Param(Var)       "PR"
   set Param(Delta)     1.0
   
   set Param(Color)     red
}

proc Macro::ZonalGraph::Init { } {
   global env
   
   Viewport::Rotate $Page::Data(Frame) 0.0 0.0

   graphaxis create AXISX
   graphaxis create AXISY

   graphaxis configure AXISX -font XFont12 -color black -gridcolor white -position LL -width 1 -type LINEAR -unit "Latitudes \[deg\]"
   graphaxis configure AXISY -font XFont12 -color black -gridcolor white -gridwidth 1 -position LL -width 1 -type LINEAR -highoffset 10 -unit "Precipitations \[mm\]"

   if { ![colormap is ZONAL_MAP] } {
      colormap create ZONAL_MAP
      colormap read ZONAL_MAP $env(HOME)/.spi/Colormap/REC_Col.std1.rgba
   }
}

proc Macro::ZonalGraph::Execute { } {
   variable Param
   variable Info
   
   #----- Initialize page and graph
   Macro::ZonalGraph::Init

   #----- Build coordinate list
   fstdfile open FSTDFILE read $Param(File)
   
   #----- Extraire les donnees
   foreach fld [fstdfield find FSTDFILE -1 "" -1 -1 -1 "" $Param(Var)] {

      vector create DATA
      vector dim    DATA { X Y }
      graphitem create ITEM

      fstdfield read FLD FSTDFILE $fld
      fstdfield configure FLD -factor 1000 -intervals { 0 1 2 3 5 8 16 32 } -colormap ZONAL_MAP -rendercontour 0 -width 1 -color black -rendertexture 1 -font XFont12 -value INTEGER 0
      
      for { set lat -90.0 } { $lat<90.0 } { set lat [expr $lat+$Param(Delta)] } {
         set coords {}
         for { set lon -180.0 } { $lon<180.0 } { set lon [expr $lon+$Param(Delta)] } {
            lappend coords $lat $lon
         }
         #----- Ge values along coordinate axis
         fstdfield vertical ZONAL FLD $coords
         
         #----- Build data vectors
         vector append DATA [list $lat [vexpr NIL savg(ZONAL*1000)]]       
      }
      
      set sec [fstdstamp toseconds [fstdfield define FLD -DATEV]]
   
      #----- Creer l'item de graph pour cette localisation
      graphitem configure ITEM -desc $Param(Var) -xaxis AXISX -yaxis AXISY -xdata DATA.X -ydata DATA.Y -orient X \
         -outline $Param(Color) -fill "" -transparency 100 -width 2 -size 3 -value False -type LINE -font XFont12 
            
      #----- Update des axes, limites, libelle, ...
      graphaxis configure AXISX -max 90 -min -90 -incr 10
   #   graphaxis configure AXISY -min 0 -max [vector stat DATA.Y -max] 
      
      #----- Update du graph avec tles donnees et les titres
      $Page::Data(Canvas) itemconf GRAPH -item ITEM -title "Zonal mean" 
      Viewport::Assign $Page::Data(Frame) $Viewport::Data(VP) FLD
      
      Page::UpdateCommand $Page::Data(Frame)

      #----- En mode batch
      if { $SPI::Param(Batch) } {
         set file ./zonal.png
            
         #----- Save image
         PrintBox::Image $Page::Data(Frame) png $file      
      }
      break
   }
   
   fstdfile close FSTDFILE
   
   if { $SPI::Param(Batch) } {
      SPI::Quit
   }
}

proc Macro::ZonalGraph::Clean { } {
   variable Data

   colormap free ZONAL_MAP
   vector free DATA
   fstdfield free FLD ZONAL
   graphitem free ITEM
   graphaxis free AXISX AXISY
}

proc Macro::ZonalGraph::Args { } {
   global argv argc
   variable Param

   #----- Lire les parametres si il y en a
   if { $argc>0 } { set Param(File)  [lindex $argv 0] }
   if { $argc>1 } { set Param(Var)   [lindex $argv 1] }
   if { $argc>2 } { set Param(Delta) [lindex $argv 2] }
}