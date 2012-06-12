#===============================================================================
# Environnement Canada
# Centre Meteorologique Canadian
# 2100 Trans-Canadienne
# Dorval, Quebec
#
# Projet   : Exemples de scripts de productions de cartes.
# Fichier  : URPBackground.tcl
# Creation : Fevrier 2012 - J.P.Gauthier - CMC/CMOE
#
# Description:
#    Produire une image de radar de background a URP
#
# Arguments  :
#   <Geodef> : Fichier geodef
#
# Remarques :
#===============================================================================

namespace eval Macro::URPBackground {} {
   variable Param
   variable Data
   variable Error

   set Param(Geodef)    ""  ;#Geo definition file

   set Param(Info)   { "Creer un background pour un radar URP a partir d'un fichier geodef"
                       "Create an URP radar background from a geodef file" }
}

proc Macro::URPBackground::Execute { } {
   global env
   variable Data
   variable Error
   variable Param

   set Viewport::Map(Type)        "azimuthal equidistant"
   set Viewport::Map(Type)        "orthographic"
   set Viewport::Map(Type)        "grid"
   set Viewport::Map(Coord)       0
   set Viewport::Map(CoordDef)    10.0
   set Viewport::Map(CoordNum)    0
   set Viewport::Map(Res)         0
   set Viewport::Map(MinSize)     20
   set Viewport::Map(Mask)        0
   set Viewport::Map(Coast)       1
   set Viewport::Map(Lake)        1
   set Viewport::Map(River)       0
   set Viewport::Map(Admin)       1
   set Viewport::Map(Polit)       1
   set Viewport::Map(Road)        0
   set Viewport::Map(Rail)        0
   set Viewport::Map(Util)        0
   set Viewport::Map(Canal)       0
   set Viewport::Map(Topo)        0
   set Viewport::Map(Bath)        0
   set Viewport::Map(Text)        0
   set Viewport::Map(City)        0
   set Viewport::Resources(Bkg)       white
   set Viewport::Resources(FillCoast) white
   set Viewport::Resources(FillLake)  #333366
   set Viewport::Resources(Coast)     black
   set Viewport::Resources(Lake)      #333366
   set Viewport::Resources(River)     #0000ff
   set Viewport::Resources(Polit)     #ff0000
   set Viewport::Resources(Admin)     #ff0000
   set Viewport::Resources(City)      white
   set Viewport::Resources(Road)      #404040
   set Viewport::Resources(Rail)      #ff1493
   set Viewport::Resources(Util)      #ffff00
   set Viewport::Resources(Canal)     #00ffff
   set Viewport::Resources(Coord)     #000000

   georef create PSREF {  PROJCS["WGS 84 / UPS North",GEOGCS["WGS 84",DATUM["WGS_1984",SPHEROID["WGS 84",6378137,298.257223563]],PRIMEM["Greenwich",0],UNIT["degree",0.0174532925199433]],PROJECTION["Polar_Stereographic"], PARAMETER["latitude_of_origin",90],PARAMETER["central_meridian",0],PARAMETER["scale_factor",0.994],PARAMETER["false_easting",2000000],PARAMETER["false_northing",2000000],UNIT["metre",1]] }
   projection configure $Page::Data(Frame) -georef PSREF

   Viewport::Do $Page::Data(Frame)

   #----- Redimension page to specified size
   Page::Size $Page::Data(Frame) $Param(Width) $Param(Height)

   Viewport::Rotate $Page::Data(Frame) $Param(LatCentre) $Param(LonCentre) [expr (6378140*2)/1000.0/$Param(Width)]


   #----- If in batch mode, print image
   if { $SPI::Param(Batch) } {
      PrintBox::Image $Page::Data(Frame) png ./$Param(SiteID)
      SPI::Quit
   }
}

proc Macro::URPBackground::ParseGeoDef { File } {
   variable Param

   set f [open $File r]

   while { ![eof $f] } {
      gets $f line
      if { $line!="" } {
         set Param([lindex $line 0]) [lindex $line 1]
         puts stderr $Param([lindex $line 0])
      }
   }
}

proc Macro::URPBackground::Clean { } {

}

proc Macro::URPBackground::Args { } {
   global argv argc

   #----- Lire les parametres si il y en a
   if { $argc>0 } {  ParseGeoDef [lindex $argv 0] }
}