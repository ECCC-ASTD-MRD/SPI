#===============================================================================
# Environnement Canada
# Centre Meteorologique Canadian
# 2100 Trans-Canadienne
# Dorval, Quebec
#
# Projet   : Impression en (x)batch de cartes en utilisant SPI.
# Fichier  : PathWays.tcl
# Creation : Mars 2008 - J.P.Gauthier - CMC/CMOE
#
# Description:
#    Creer une carte/produit de trajectoire du parcours de l'air arrivant a diverses villes canadiennes.
#
# Arguments  :
#   <Path>   : Chemin dans lequel recuperer les fichiers trajectoires et entre
#   <Type>   : Type de fichier image (defaut: png)
#
# Remarques :
#   - Lancement: SPI -batch -layout TRAJ -script PathWays -args [path] [format]
#===============================================================================

namespace eval Macro::PathWays {} {
   variable Param
   variable Data
   variable Error

   set Param(Info) { "Creer une carte/produit du parcours de\nl'air arrivant a diverses villes canadiennes."
                     "Create a trajectory map/product for\nair arriving at several canadian cities." }
   set Param(InfoArgs) { { "Répertoire des fichiers trajectoires" "Format d'image" } { "Trajectory path" "Image format" } }

   set Param(Path)   ./     ;#Chemin par defaut pour les fichiers trajectoires
   set Param(Format) png    ;#Type de fichier image a generer
   set Param(Colors) { #008a00 #000000 #ca0000 #7a378b #00008a #7a378b #00008a #008a00 #000000 #ca0000 #ff7700 #00008a #008a00 #ff7700 #7a378b #ff7700 #ca0000 }

   set Data(Parcels) {}     ;#Trajectoires lues

   set Error(Files)  { "Aucun fichier de trajectoire trouvé."
                       "Could not find any trajectory file." }
}

proc Macro::PathWays::Execute { } {
   global GDefs
   variable Data
   variable Error
   variable Param

   #----- Dimension du produit
   Page::Size $Page::Data(Frame) 815 715

   #----- Mettre les parametres du viewport en places
   set Viewport::Resources(FillCoast) #F1F1F1
   set Viewport::Resources(FillLake)  ""
   set Viewport::Map(Coast)      1            ;#Cotes
   set Viewport::Map(Lake)       1            ;#Lacs
   set Viewport::Map(River)      1            ;#Rivieres
   set Viewport::Map(Polit)      1            ;#Bordures politiques
   set Viewport::Map(Admin)      1            ;#Bordures politiques internes
   set Viewport::Map(City)       0            ;#Villes
   set Viewport::Map(Road)       0            ;#Routes
   set Viewport::Map(Rail)       0            ;#Chemin de fer
   set Viewport::Map(Util)       0            ;#Utilitaires
   set Viewport::Map(Canal)      0            ;#Canal/Aqueduc
   set Viewport::Map(Topo)       0            ;#Topographie
   set Viewport::Map(Bath)       0            ;#Bathymetrie
   set Viewport::Map(Text)       0            ;#Texture
   set Viewport::Map(Coord)      0            ;#Positionnement des latlon (<0=Ocean,>0=Partout)
   set Viewport::Map(CoordDef)   10           ;#Intervale entre les latlon en degres
   set Viewport::Map(CoordNum)   2            ;#Numerotation des latlon
   set Viewport::Map(Elev)       1.0          ;#Facteur d'expansion des elevations
   set Viewport::Map(Place)      0            ;#Endroits
   set Viewport::Map(Admin)      0            ;#Bordures politiques internes
   set Viewport::Map(Type)       orthographic ;#Type de projection

   Viewport::Do $Page::Data(Frame)
   Viewport::Rotate $Page::Data(Frame) 62 -95 1.50

   #----- Mettre les parametres generaux des trajectoires en places
   set Trajectory::Param(Mode) PARCEL   ;#Mode de configuration par niveaux

   set Data(Parcels) [trajectory load $Param(Path)/traject.points]
   Viewport::Assign $Page::Data(Frame) $Viewport::Data(VP) $Data(Parcels)

   foreach parcel $Data(Parcels) color $Param(Colors)  {
      trajectory configure $parcel -width 3 -color $color -style 0 -icon NONE
   }

   Macro::Doing "Generating French product"
   Macro::PathWays::Legend Francais $Data(Parcels)
   PrintBox::Image $Page::Data(Frame) $Param(Format) $Param(Path)/traject.pointsf

   Macro::Doing "Generating English product"
   Macro::PathWays::Legend English $Data(Parcels)
   PrintBox::Image $Page::Data(Frame) $Param(Format) $Param(Path)/traject.pointse

   Macro::Doing ""
   if { $SPI::Param(Batch) } {
      SPI::Quit
   }
}

proc Macro::PathWays::Legend { Language Parcels } {
   variable Param

   #----- Extraire les noms de villes du fichier entre
   set data [split [exec cat $Param(Path)/entre] "\n"]
   set date [lrange $data [expr [llength $data]-4] end]
   set data [lrange $data 6 [expr [llength $data]-5]]

   set date "[lindex [lindex $date 1] 0]/[lindex [lindex $date 2] 0]/[lindex [lindex $date 0] 0] [lindex [lindex $date 3] 0]:00"

   set no 1
   set id 0
   set y  500

   #----- Pointer les villes
   foreach { line } $data parcel $Parcels {

      set color [trajectory configure $parcel -color]
      if { [set loc [$Viewport::Data(VP) -project [lindex $line 1] [lindex $line 0] 0]]!="" } {
         Shape::DrawCIRCLE $Page::Data(Canvas) $loc LOC $color 3 1
         $Page::Data(Canvas) create text [lindex $loc 0] [lindex $loc 1] -text " $no" -anchor w -fill black -font XFont10
      }
      $Page::Data(Canvas) create text 5 $y -text "$no [lindex $line 3] " -anchor w -fill $color -font XFont10
      incr no
      incr id 1
      incr y 12
   }

   #----- Afficher la legende
   $Page::Data(Canvas) delete LEGEND
   $Page::Data(Canvas) create rectangle 480 5 810 55 -tags LEGEND -fill white -outline black -transparency 90
   $Page::Data(Canvas) create image 5 5 -tags LEGEND -anchor nw -image LOGO

   switch $Language {
      "Francais" {
         set date [clock format [clock scan $date -gmt true] -format "%d %B %Y %H GMT" -gmt true]

         $Page::Data(Canvas) create text 645 10 -tags LEGEND -fill black -font XFont12 -anchor n -justify center \
            -text "Parcours de l'air arrivant a diverses villes\ncanadiennes le $date\n(5 jours de retro trajectoires a 925mb)"
         $Page::Data(Canvas) create text 5 715  -tags LEGEND -fill black -font XFont10 -anchor sw -text "Base sur le modele de trajectoire du CMC initialise par le modele GEM global"
      }
      "English" {
         set date [clock format [clock scan $date -gmt true] -format "%H GMT on %B %d, %Y" -gmt true]

         $Page::Data(Canvas) create text 645 10 -tags LEGEND -fill black -font XFont12 -anchor n -justify center \
            -text "Pathways for air arriving at several canadian\n cities at $date\n(5 days back trajectories at 925 mbar)"
         $Page::Data(Canvas) create text 5 715  -tags LEGEND  -fill black -font XFont10 -anchor sw -text "Based on CMC trajectory model initialized with GEM global model"
      }
   }
}

proc Macro::PathWays::Clean { } {
   variable Data

   trajectory free $Data(Parcels)
}

proc Macro::PathWays::Args { } {
   global argv argc
   variable Param

   #----- Lire les parametres si il y en a
   if { $argc>0 } { set Param(Path)   [lindex $argv 0] }
   if { $argc>1 } { set Param(Format) [lindex $argv 1] }
}