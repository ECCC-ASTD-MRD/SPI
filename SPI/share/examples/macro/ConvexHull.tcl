#===============================================================================
# Environnement Canada
# Centre Meteorologique Canadian
# 2100 Trans-Canadienne
# Dorval, Quebec
#
# Projet   : Exemples de scripts de productions de cartes.
# Fichier  : ConvexHull.tcl
# Creation : Fevrier 2009 - J.P.Gauthier - CMC/CMOE
#
# Description:
#    Produire des convex hull et les images associees
#
# Arguments  :
#
# Remarques :
#
#===============================================================================

namespace eval Macro::ConvexHull {} {
   variable Param
   variable Data
   variable Error

   set Param(File)  /cnfs/dev/cmoe/afsr005/public_html/Data/DataIn/20130602_tv_east
   set Param(Var)   "SWI"        ;# Variable to process
   set Param(Buffer) 0.2         ;# Buffer around the shapes for the convex hulls (in degrees)
   set Param(Dist)   2.0         ;# Distance before merging 2 hulls together (in degrees)
   set Param(Out)    ./out       ;# Output file name
   
   set Param(Info)      { "Calcul de convex hull et images associées."
                          "Calculates cinvex hulls and associated images" }
   set Param(InfoArgs)  { { "Fichier" "[Variable]" "[Buffer]" "[Distance]" "[Fichier de sortie]" } { "File" "[Variable]" "[Buffer]" "[Distance]" "[Output file]" } }
}

proc Macro::ConvexHull::Create { Field } {
   variable Param
   
   ogrlayer free OGRLAYER DISSOLVED BUFFERED
   
   #----- Creer la couche avec le bon referentiel
   ogrlayer new OGRLAYER "Data" "Polygon"

   #----- Importer les donnees RPN dans la couche
   ogrlayer import OGRLAYER $Field

   #----- Si c'est vide, on retourne
   if { ![ogrlayer define OGRLAYER -nb] } {
      return ""
   }

   #----- Dissolve into polygon masses
   ogrlayer stats OGRLAYER -dissolve DISSOLVED

   #----- Apply a distance buffer
   ogrlayer stats DISSOLVED -buffer $Param(Buffer) 5 BUFFERED         
         
   #----- Iterate to merge touching hull
   set n 4
   set geom    [ogrlayer define BUFFERED -geometry 0]
   set newgeom ""
   
   while { [incr n -1] } {
     
      #----- Get the sub geoms
      set hulls [ogrgeometry define $geom -geometry]
      
      #----- If we're inside a polygon, break
      if { [ogrgeometry define [lindex $hulls 0] -type]=="Line String" } {
         break
      }
      
      #----- Loop on each convex hull and do a geographic union
      set newgeom {}
      foreach hull $hulls {
      
         #----- Check hull distances for merge
         foreach dgeom $hulls {
            if { $Param(Dist) && $hull!=$dgeom && [ogrgeometry stats $hull -distance $dgeom]<=$Param(Dist) } {
               set hull [ogrgeometry stats [ogrgeometry stats $hull -union $dgeom] -convexhull]
            }
         }
         
         #-----Create convex hulls
         if { [ogrgeometry is $newgeom] } {
            set newgeom [ogrgeometry stats $newgeom -union [ogrgeometry stats $hull -convexhull]]
         } else {
            set newgeom [ogrgeometry stats $hull -convexhull]
         }
      }    
      set geom $newgeom
   }

   #----- Keep resulting hulls
   if { ![ogrgeometry is $newgeom] } {
      set newgeom [ogrgeometry stats $geom -convexhull]
   }
   ogrlayer define BUFFERED -geometry 0 False $newgeom
   
   #----- Save it all
   eval file delete [glob -nocomplain $Param(Out)-[fstdfield define $Field -IP3].*]
   ogrfile open OGRFILE write $Param(Out)-[fstdfield define $Field -IP3].shp "ESRI Shapefile"
   ogrlayer write BUFFERED OGRFILE
   ogrfile close OGRFILE 
   
   return BUFFERED
}

proc Macro::ConvexHull::Execute { } {
   global env
   variable Data
   variable Error
   variable Param

   if { ![colormap is RPNMAP] } {
      colormap create RPNMAP -file $env(HOME)/.spi/Colormap/REC_Col.std1.rgba
   }
   font create RPNFONT -family courier -size -12 -weight bold

   fstdfile open RPNFILE read $Param(File)

   foreach idx [fstdfield find RPNFILE -1 "" -1 -1 -1 "" $Param(Var)] {
      Macro::Doing "Processing $idx"

      fstdfield read RPNFIELD RPNFILE $idx
      fstdfield configure RPNFIELD -rendertexture 1 -intervals { 10 20 30 40 50 60 70 80 90 100 } -rendercontour 0

      if { [ogrlayer is [set layer [Macro::ConvexHull::Create RPNFIELD]]] } {
         ogrlayer configure $layer -color black -fill "" -width 2 -rendercontour 1
         projection configure $Page::Data(Frame) -data $layer
      }
      Macro::Doing "Creating product"

      fstdfield configure RPNFIELD -color black -colormap RPNMAP -font RPNFONT -rendertexture 1 -width 1 -interpdegree NEAREST -value INTEGER 0 
     
      Viewport::UnAssign $Page::Data(Frame) $Viewport::Data(VP)
      Viewport::Assign   $Page::Data(Frame) $Viewport::Data(VP) RPNFIELD

      Page::UpdateCommand $Page::Data(Frame)
      Page::Update $Page::Data(Frame)

      #----- If in batch mode, print the map and exit

      if { $SPI::Param(Batch) } {
         PrintBox::Image $Page::Data(Frame) png ConvexHull-[fstdfield define RPNFIELD -IP3]
      }
   }

   Macro::Doing ""
   Macro::Cursor left_ptr

   if { $SPI::Param(Batch) } {
      SPI::Quit
   }

   fstdfile close RPNFILE
}

proc Macro::ConvexHull::Clean { } {

   Viewport::UnAssign $Page::Data(Frame) $Viewport::Data(VP)
   font delete RPNFONT

   fstdfield free RPNFIELD
}

proc Macro::ConvexHull::Args { } {
   global argv argc
   variable Param

   set Param(File) [lindex $argv 0]
   
   #----- Optional args
   if { $argc>1 } { set Param(Var)    [lindex $argv 1] }
   if { $argc>2 } { set Param(Buffer) [lindex $argv 2] }
   if { $argc>3 } { set Param(Dist)   [lindex $argv 3] }
   if { $argc>4 } { set Param(Out)    [lindex $argv 4] }
}
