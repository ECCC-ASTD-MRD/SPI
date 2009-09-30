#===============================================================================
# Environnement Canada
# Centre Meteorologique Canadian
# 2100 Trans-Canadienne
# Dorval, Quebec
#
# Projet   : Modeles de produits.
# Fichier  : CANERM_Watch.tcl
# Creation : Mars 2002 - J.P.Gauthier - CMC/CMOE
#
# Description:
#    Definition d'un format de page pour SPI pour les modeles de cartes
#    VAAC et RSMC automatiques. Donc, une projection, une colorbar et
#    un mesage d'avertissement.
#
# Remarques :
#
# Modifications :
#
#   Nom         : Jean-Philippe Gauthier
#   Date        : Janvier 2004
#   Description : Mise a jour pour SPI 7.1
#
#===============================================================================

namespace eval CANERM_Watch { }

proc CANERM_Watch::Layout { Frame } {
   global GDefs

   #----- Dimension du produit

   Page::Size $Frame 650 620

   set Viewport::Map(Coast)      1	      ;#Cotes
   set Viewport::Map(Lake)       1	      ;#Lacs
   set Viewport::Map(River)      1	      ;#Rivieres
   set Viewport::Map(Polit)      1	      ;#Bordures politiques
   set Viewport::Map(Admin)      1	      ;#Bordures politiques internes
   set Viewport::Map(City)       0            ;#Villes
   set Viewport::Map(Road)       0	      ;#Routes
   set Viewport::Map(Rail)       0	      ;#Chemin de fer
   set Viewport::Map(Util)       0	      ;#Utilitaires
   set Viewport::Map(Canal)      0	      ;#Canal/Aqueduc
   set Viewport::Map(Topo)       0	      ;#Topographie
   set Viewport::Map(Bath)       0	      ;#Bathymetrie
   set Viewport::Map(Text)       0	      ;#Texture
   set Viewport::Map(Coord)      1            ;#Positionnement des latlon (<0=Ocean,>0=Partout)
   set Viewport::Map(CoordDef)   10           ;#Intervale entre les latlon en degres
   set Viewport::Map(CoordNum)   2            ;#Numerotation des latlon
   set Viewport::Map(Elev)       1.0          ;#Facteur d'expansion des elevations
   set Viewport::Map(TPolit)     0            ;#Identifications provinces
   set Viewport::Map(TCity)      0            ;#Identifications villes
   set Viewport::Map(Type)       orthographic ;#Type de projection

   set Viewport::Resources(FillCoast) #C2D74B   ;#Cotes (Polygones)
   set Viewport::Resources(FillLake)  #00C0FF   ;#Lacs (Polygones)

   set ProjCam::Data(Lens) 2.5

   Viewport::Do $Frame

   #----- Affichage des layouts
   Viewport::Create $Frame 1 1 570 560 0 0 VP

   #-----  Positionnement des colorbars
   $Frame.page.canvas create colorbar -x 570 -y 1 -width 79 -height 561 -tags "CB" -anchor nw

   #----- Message d'avertissement
   image create photo LOGO -file $GDefs(Dir)/Resources/Image/Symbol/Logo/Logo_SMC.gif
   $Frame.page.canvas create image 1 561 -anchor nw -image LOGO

   $Frame.page.canvas create rectangle 1 561 649 620 -outline black -width 1 -tags "LEGEND"
   $Frame.page.canvas create text 5 595 -fill red -font XFont10 -anchor nw \
      -text "WARNING: This is not an official forecast and it must not be used as a substitute to official forecasts."
   $Frame.page.canvas create text 5 606 -fill red   -font XFont10 -anchor nw \
      -text "         Release date and time are hypothetical."
}
