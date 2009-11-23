#===============================================================================
# Environnement Canada
# Centre Meteorologique Canadian
# 2100 Trans-Canadienne
# Dorval, Quebec
#
# Projet   : Exemples de scripts de productions de cartes.
# Fichier  : XSection.tcl
# Creation : Fevrier 2001 - J.P.Gauthier - CMC/CMOE
#
# Description:
#    Calcul du flux traversant une coupe transversale.
#
# Arguments  :
#
# Remarques :
#
#===============================================================================

namespace eval Macro::XSection {} {
   variable Param
   variable Data
   variable Error
   variable Lbl

   set Param(In)      ""
   set Param(Info)   { "Creer une coupe verticale"
                       "Create a vertical cross-section" }

   set Data(Page)        ""
   set Data(Resolution)  50000
   set Data(Coords)      { 49.2 -123.17 45.48 -73.70 }

   set Error(Field)    { "Aucun champs valide trouvé\nvérifié le viewport actif"
                         "Could not find any valid field\nCheck the active viewport" }
}

proc Macro::XSection::Execute { } {
   global env
   variable Lbl
   variable Error
   variable Data

   set field [lindex [Viewport::Assigned $Page::Data(Frame) $Viewport::Data(VP)] 0]

   if { ![fstdfield is $field] } {
      Macro::Error $Error(Field)
      return
   }

   if { ![winfo exists $Data(Page)] } {
      set Data(Page)   [SPI::PageNew True "Macro XSection 1.0" 800x600]
      set Data(Canvas) $Data(Page).page.canvas
   }

   Macro::Doing "Initializing config"
   if { ![colormap is XSECTION_MAP] } {
      colormap create XSECTION_MAP
      colormap read XSECTION_MAP $env(HOME)/.spi/Colormap/REC_Col.std1.rgba
   }

   Macro::XSection::Clean

   set path [projection function $Page::Data(Frame) -path $Data(Coords) $Data(Resolution)]
   set dist [projection function $Page::Data(Frame) -dist $path 0]

   fstdfield vertical Macro::XSECTION $field $path

#   FSTD::Register Macro::XSECTION

   fstdfield configure Macro::XSECTION -ztype PRESSURE -rendertexture 1 -colormap XSECTION_MAP -font XFont12 -color black \
       -intervalmode INTERVAL 5
   set lvls [fstdfield stats Macro::XSECTION -pressurelevels]

   graphaxis create Macro::AXISX
   graphaxis create Macro::AXISY

   graphaxis configure Macro::AXISX -type LINEAR -intervals [list 0 [expr [llength $path]/2.0-1]] -labels {Vancouver Montreal} -unit Distance -font XFont12 -gridcolor #010101 -gridwidth 1 -color #000000 -position LL -width 1
   graphaxis configure Macro::AXISY -type LINEAR  -min 1000.0 -max 0.0 -unit "Level (Mb)" -font XFont12 -color #000000 -position LL -width 1

   graphitem create Macro::ITEM
   graphitem configure Macro::ITEM -xaxis Macro::AXISX -yaxis Macro::AXISY -data Macro::XSECTION

   $Data(Canvas) create graph -x 1 -y 1 -width [Page::CanvasWidth $Data(Page)] -height [Page::CanvasHeight $Data(Page)] \
       -anchor nw -legend False -fg #000000 -bg #FFFFFF -fill #000000 -tags GRAPH -font XFont12 -title XSection \
       -item Macro::ITEM

   Macro::Doing ""
}

proc Macro::XSection::Clean { } {
   variable Data

   fstdfield free Macro::XSECTION

   graphaxis free Macro::AXISX
   graphaxis free Macro::AXISY
   graphitem free Macro::ITEM
}





