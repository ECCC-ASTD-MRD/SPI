#===============================================================================
# Environnement Canada
# Centre Meteorologique Canadian
# 2100 Trans-Canadienne
# Dorval, Quebec
#
# Projet   : Librairie de "Widget" Tk.
# Fichier  : GTour.tcl
# Creation : Avril 2006 - J.P.Gauthier - CMC/CMOE
#
# Description:
#    Outils de now-casting.
#
# Remarques :
#
#===============================================================================

#----- Lire les sources d'execution

source $GDefs(Dir)/tcl/Tools/GTour/GTour.ctes
source $GDefs(Dir)/tcl/Tools/GTour/GTour.txt
source $GDefs(Dir)/tcl/Tools/GTour/GTour.int

#-------------------------------------------------------------------------------
# Nom      : <GTour::Close>
# Creation : Avril 2006 - J.P. Gauthier - CMC/CMOE
#
# But      : Ferme tout les fichiers ouvert et detruit la fenetre.
#
# Parametres :
#
# Retour    :
#
# Remarque :
#
#-------------------------------------------------------------------------------

proc GTour::Close { } {
   variable Data

   if { [lindex [split $Page::Data(ToolMode) ":"] 0]=="GTour" } {
      SPI::ToolMode SPI Zoom
   }

   set Data(Active) 0
   set Data(Coo)    ""

   after cancel GTour::Now
   destroy .gtour

   if { !$SPI::Param(Window) } { SPI::Quit }
}

#-------------------------------------------------------------------------------
# Nom      : <GTour::AsProject>
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

proc GTour::AsProject { File } {
   variable Data
   variable Param

   if { [winfo exists .gtour] } {
      puts $File "#----- Tool: GTour\n"
      puts $File "set GTour::Param(Dock)   $Param(Dock)"
      puts $File "set GTour::Param(Geom)   [winfo geometry .gtour]"
      puts $File "GTour::Window"
      puts $File "\n"
   }
}

proc GTour::Add { File } {
   variable Param
   variable Data
   
   if { $File!="" } {
      lappend Data(Features) [set feature FEATURE[incr Data(FeatureNb)]]
      set Data($feature) [list $File 0.0 0.0 0 "Feature name" "Feature description"]
      
      GTour::Select $feature
   }
}

proc GTour::Select { Feature } {
   variable Param
   variable Data
   
   set Data(Feature) $Feature
   set Data(Name) [lindex $Data($Feature) 4]
   set Data(Desc) [lindex $Data($Feature) 5]
   
   $Data(Tab).params.desc.sel delete 0.0 end
   $Data(Tab).params.desc.sel insert 0.0 $Data(Desc) 
}

proc GTour::Save { } {
   global env
   variable Param
   variable Data
   
   set Param(KMZ) $env(HOME)/EC-GTour.kmz
   
   file delete -force $Param(Path)
   file mkdir -force $Param(Path)
   
   set kml "<?xml version=\"1.0\" encoding=\"UTF-8\"?>
<kml xmlns=\"http://www.opengis.net/kml/2.2\"
 xmlns:gx=\"http://www.google.com/kml/ext/2.2\">

   <Document>
      <name>GTour for [clock format [clock seconds]]</name>
      <open>1</open>
      <gx:Tour>
         <name>Start tour</name>
         <gx:Playlist>
            <gx:FlyTo>
               <gx:duration>[expr int($Param(TransitDuration)/2)]</gx:duration>
               <LookAt>
                  <longitude>-95.0</longitude>
                  <latitude>46.0</latitude>
                  <altitude>0.0</altitude>
                  <range>7000000.0</range>
                  <altitudeMode>relativeToGround</altitudeMode>     
                  <tilt>0.0</tilt>
               </LookAt>
            </gx:FlyTo>
            <gx:Wait>
               <gx:duration>5.0</gx:duration>
            </gx:Wait>"

   #----- Build tour
   foreach feature $Data(Features) {
   
      set lat   [lindex $Data($feature) 1]
      set lon   [lindex $Data($feature) 2]
      set elev  [lindex $Data($feature) 3]
      set name  [lindex $Data($feature) 4]
      set desc  [lindex $Data($feature) 5]

      set head -9.295926
      set tilt  60
      set dur   $Param(InfoDuration)
     
      append kml "
            <gx:FlyTo>
               <gx:duration>$Param(TransitDuration)</gx:duration>
               <LookAt>
                  <longitude>$lon</longitude>
                  <latitude>$lat</latitude>
                  <altitude>0.0</altitude>
                  <range>$Param(Elevation)</range>
                  <altitudeMode>relativeToGround</altitudeMode>     
                  <tilt>$tilt</tilt>
               </LookAt>
            </gx:FlyTo>
      
            <gx:AnimatedUpdate>
               <Update>
                  <targetHref/>
                  <Change>
                  <Placemark targetId=\"$feature\">
                     <gx:balloonVisibility>1</gx:balloonVisibility>
                  </Placemark>
                  </Change>
               </Update>
            </gx:AnimatedUpdate>

            <gx:Wait>
               <gx:duration>$dur</gx:duration>
            </gx:Wait>

            <gx:AnimatedUpdate>
            <Update>
               <targetHref/>
               <Change>
               <Placemark targetId=\"$feature\">
                  <gx:balloonVisibility>0</gx:balloonVisibility>
               </Placemark>
               </Change>
            </Update>
         </gx:AnimatedUpdate>"
   }
   
   append kml "
            <gx:FlyTo>
               <gx:duration>[expr int($Param(TransitDuration)/2)]</gx:duration>
               <LookAt>
                  <longitude>-95.0</longitude>
                  <latitude>46.0</latitude>
                  <altitude>0.0</altitude>
                  <range>7000000.0</range>
                  <altitudeMode>relativeToGround</altitudeMode>     
                  <tilt>0.0</tilt>
               </LookAt>
            </gx:FlyTo>
         </gx:Playlist>
      </gx:Tour>
   <Folder>"
    
   #----- Add feature's placemark
   foreach feature $Data(Features) {
      puts stderr $Data($feature)
      file copy [lindex $Data($feature) 0] $Param(Path)
      
      set lat   [lindex $Data($feature) 1]
      set lon   [lindex $Data($feature) 2]
      set elev  [lindex $Data($feature) 3]
      set name  [lindex $Data($feature) 4]
      set desc  [lindex $Data($feature) 5]
      
      set coords [format "%.5f,%.5f,%i\n" $lon $lat $elev]
      
      append kml "
      <Placemark id=\"$feature\">
         <name>$name</name>
         <description>
            <!\[CDATA\[
               <p align=\"left\">
                  <img src=\"[file tail [lindex $Data($feature) 0]]\" width=\"400\">
               </p>
               <br>[string map { "\n" "<br>" } $desc]<br>
            \]\]>
         </description>
         <Point>
            <altitudeMode>relativeToGround</altitudeMode>
            <coordinates>$coords</coordinates>
         </Point>
      </Placemark>"
      
#  append kml "     
#             <NetworkLink>
#                <name>RADAR</name>
#                <visibility>1</visibility>
#                <styleUrl>#checkHide</styleUrl>
#                <open>0</open>
#                <refreshVisibility>0</refreshVisibility>
#                <Link>
#                   <href>http://geo.weatheroffice.gc.ca/geomet//?Service=KML%26Version=2.2%26Request=GetMap%26layers=RADAR_RRAI%26elevation=-1%26styles=RADARURPPRECIP</href>
#                </Link>
#             </NetworkLink>"

}
   
   append kml "     </Folder>
   </Document>
</kml>"

   #-----Create KMZ
   set f [open $Param(Path)/doc.kml w]
   puts $f $kml
   close $f
   
   eval set err \[catch \{ exec zip -j $Param(KMZ) [glob $Param(Path)/*] \} msg\]
   if { $err } {
      Log::Print ERROR "Problems producing kmz:\n\n\t$msg"
   }
}

#----------------------------------------------------------------------------
# Nom      : <GTour::Draw...>
# Creation : Juin 2003 - J.P. Gauthier - CMC/CMOE
#
# But      : Fonctions de manipulation sur la projection
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

proc GTour::DrawInit { Frame VP } {
   variable Data

   set Data(Lat0)   $Viewport::Map(LatCursor)
   set Data(Lon0)   $Viewport::Map(LonCursor)
   
   lset Data($Data(Feature)) 1 $Data(Lat0)
   lset Data($Data(Feature)) 2 $Data(Lon0)
}

proc GTour::Draw { Frame VP } {
   variable Data
return
   
   if { $Data(Canvas)!="" } {
      $Data(Canvas) delete YOURTOOLHERE
   }

   set Data(Lat1)   $Viewport::Map(LatCursor)
   set Data(Lon1)   $Viewport::Map(LonCursor)

   set Data(Canvas) $Frame.page.canvas
   set Data(Frame)  $Frame
   set Data(VP)     $VP

   GTour::UpdateItems $Frame
}

proc GTour::DrawDone { Frame VP } {
   variable Data
return

   if { $Data(Lat0)>$Data(Lat1) } {
      set tmp $Data(Lat1)
      set Data(Lat1) $Data(Lat0)
      set Data(Lat0) $tmp
   }

   if { $Data(Lon0)>$Data(Lon1) } {
      set tmp $Data(Lon1)
      set Data(Lon1) $Data(Lon0)
      set Data(Lon0) $tmp
   }

   if { $Data(Lat0)==$Data(Lat1) || $Data(Lon0)==$Data(Lon1) } {
      set Data(Coo) ""
   } else {
      set Data(Coo) "$Data(Lat0),$Data(Lon0) - $Data(Lat1),$Data(Lon1)"
   }
}

proc GTour::MoveInit { Frame VP } {
   variable Data

   set Data(LonD) $Viewport::Map(LonCursor)
   set Data(LatD) $Viewport::Map(LatCursor)
}

proc GTour::Move { Frame VP } {
   variable Data

   #----- Effectuer la translation

   set lat0 [expr $Data(Lat0) + $Viewport::Map(LatCursor) - $Data(LatD)]
   set lat1 [expr $Data(Lat1) + $Viewport::Map(LatCursor) - $Data(LatD)]

   if { $lat0 > -90.0 && $lat0 < 90.0 && $lat1 > -90.0 && $lat1 < 90.0 } {

      set Data(Lat0) $lat0
      set Data(Lat1) $lat1
      eval set Data(Lon0) [Viewport::CheckCoord [expr $Data(Lon0) + $Viewport::Map(LonCursor) - $Data(LonD)]]
      eval set Data(Lon1) [Viewport::CheckCoord [expr $Data(Lon1) + $Viewport::Map(LonCursor) - $Data(LonD)]]
   }

   #----- Reaffecter le point de reference de translation

   if { $Data(Canvas)!="" } {
      $Data(Canvas) delete YOURTOOLHERE
   }

   set Data(LonD)   $Viewport::Map(LonCursor)
   set Data(LatD)   $Viewport::Map(LatCursor)

   set Data(Canvas) $Frame.page.canvas
   set Data(Frame)  $Frame
   set Data(VP)     $VP

   GTour::UpdateItems $Frame
}

proc GTour::MoveDone { Frame VP } {
   variable Data

   GTour::DrawDone $Frame $VP
}

#-------------------------------------------------------------------------------
# Nom      : <GTour::Update>
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

proc GTour::Update { Frame } {
   variable Data
}

#-------------------------------------------------------------------------------
# Nom      : <GTour::UpdateItems>
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

proc GTour::UpdateItems { Frame } {
   variable Data

   $Data(Canvas) delete GTOUR

   if { $Data(VP)!="" } {
      foreach feature $Data(Features) {
         puts stderr  $Data(Features)
         set lat   [lindex $Data($feature) 1]
         set lon   [lindex $Data($feature) 2]
         set elev  0.0
         
         #----- Projection des coordonnées
         if { [set xy [$Data(VP) -project $lat $lon $elev]]!="" && [lindex $xy 2]>=0 } {

            set x [lindex $xy 0]
            set y [lindex $xy 1]
            $Data(Canvas) create image $x $y -image $ico -tags "GTOUR GTOUR$feature"
         }
      }
   }
}
