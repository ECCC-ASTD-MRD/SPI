#===============================================================================
# Environnement Canada
# Centre Meteorologique Canadian
# 2100 Trans-Canadienne
# Dorval, Quebec
#
# Projet   : Impression en (x)batch de cartes en utilisant SPI.
# Fichier  : AutoTraj.tcl
# Creation : Fevrier 2008 - J.P.Gauthier - CMC/CMOE
#
# Description:
#    Creer une carte/produit de trajectoire.
#
# Arguments  :
#   <Path>   : Chemin dans lequel recuperer les fichiers trajectoires
#   <Type>   : Type de fichier image (defaut: png)
#
# Remarques :
#   - Ce script traite tous les fichiers de trajectoires dans le repertoire specifie
#   - Lancement: SPI -batch -layout TRAJ -script ../AutoTraj -args [path] [format] [legend] [height] [graph]
#===============================================================================

namespace eval Macro::AutoTraj {} {
   variable Param
   variable Data
   variable Error

   set Param(Info) { "Creer une carte/produit de trajectoire pour\nchaque fichier de trajectoires dans une repertoire."
                     "Create a trajectory map/product for each\n trajectory file in a directory." }

   set Param(Path)   ./     ;#Chemin par defaut pour les fichiers trajectoires
   set Param(Format) png    ;#Type de fichier image a generer
   set Param(Legend) True   ;#Affichage de la legende
   set Param(Height) True   ;#Affichage des hauteurs
   set Param(Graph)  True   ;#Affichage du graph

   set Error(Files)  { "Aucun fichier de trajectoire trouvé."
                       "Could not find any trajectory file." }
}

proc Macro::AutoTraj::Execute { } {
   global GDefs
   variable Data
   variable Error
   variable Param

   #----- Mettre les parametres du viewport en places
   set Viewport::Resources(FillCoast) #F1F1F1
   set Viewport::Resources(FillLake)  ""
   set Viewport::Map(Coast)      1          ;#Cotes
   set Viewport::Map(Lake)       1          ;#Lacs
   set Viewport::Map(River)      0          ;#Rivieres
   set Viewport::Map(Polit)      1          ;#Bordures politiques
   set Viewport::Map(Admin)      1          ;#Bordures politiques internes
   set Viewport::Map(City)       0          ;#Villes
   set Viewport::Map(Road)       0          ;#Routes
   set Viewport::Map(Rail)       0          ;#Chemin de fer
   set Viewport::Map(Util)       0          ;#Utilitaires
   set Viewport::Map(Canal)      0          ;#Canal/Aqueduc
   set Viewport::Map(Topo)       0          ;#Topographie
   set Viewport::Map(Bath)       0          ;#Bathymetrie
   set Viewport::Map(Text)       0          ;#Texture
   set Viewport::Map(Coord)      1          ;#Positionnement des latlon (<0=Ocean,>0=Partout)
   set Viewport::Map(CoordDef)   5          ;#Intervale entre les latlon en degres
   set Viewport::Map(CoordNum)   2          ;#Numerotation des latlon
   set Viewport::Map(Elev)       1.0        ;#Facteur d'expansion des elevations
   set Viewport::Map(Place)      0          ;#Endroits
   set Viewport::Map(Type)       cylindric  ;#Type de projection

   Viewport::Do $Page::Data(Frame)

   #----- Mettre les parametres generaux des trajectoires en places
   set Trajectory::Param(Width)     2       ;#Largeur des segments
   set Trajectory::Param(Size)      2       ;#Dimension des icones
   set Trajectory::Param(Mark)      24      ;#Remplir les icones
   set Trajectory::Param(Interval)  3       ;#Intervale de selection des donnees
   set Trajectory::Param(Mode)      LEVEL   ;#Mode de configuration par niveaux

   #----- Definit les attributs pour des niveaux specifiques.
   dataspec create 2500.00
   dataspec configure 2500.00 -fill white -color #006400 -icon CIRCLE

   dataspec create 750.00
   dataspec configure 750.00 -fill white -color #ff0000 -icon TRIANGLE

   dataspec create 800.00
   dataspec configure 800.00 -fill white -color #006400 -icon CIRCLE

   set SPI::Data(ShowTrajHeight$Page::Data(Frame)) $Param(Height)
   set SPI::Data(ShowTrajLegend$Page::Data(Frame)) $Param(Legend)
   set SPI::Data(ShowTrajGraph$Page::Data(Frame))  $Param(Graph)

   #----- Pour toutes les trajectoires du repertoire
   if { ![llength [set files [lsort -dictionary [glob -nocomplain $Param(Path)/*.points]]]] } {
      Macro::Error $Error(Files)
   } else {
      foreach file $files {

         Macro::Doing "Processing $file"
         set trajs [trajectory load $file]

         Trajectory::Locate $trajs 0.1
         Viewport::UnAssign $Page::Data(Frame) $Viewport::Data(VP)
         Viewport::Assign $Page::Data(Frame) $Viewport::Data(VP) $trajs

         #----- Update des items de pages
         TRAJ::LayoutUpdate $Page::Data(Frame)
#         SPI::LayoutUpdate $Page::Data(Frame)

         PrintBox::Image $Page::Data(Frame) $Param(Format) [file rootname $file]

         trajectory free $trajs
      }
   }

   Macro::Doing ""
   if { $SPI::Param(Batch) } {
      SPI::Quit
   }
}

proc Macro::AutoTraj::Clean { } {

}

#----- Lire les parametres si il y en a
if { $argc>0 } { set Macro::AutoTraj::Param(Path)   [lindex $argv 0] }
if { $argc>1 } { set Macro::AutoTraj::Param(Format) [lindex $argv 1] }
if { $argc>2 } { set Macro::AutoTraj::Param(Height) [lindex $argv 2] }
if { $argc>3 } { set Macro::AutoTraj::Param(Legend) [lindex $argv 3] }
if { $argc>4 } { set Macro::AutoTraj::Param(Graph)  [lindex $argv 4] }

