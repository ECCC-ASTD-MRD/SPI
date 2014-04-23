#CMOI_LEVEL tcl
#CMOI_PLATFORM op_f
#===============================================================================
# Environnement Canada
# Centre Meteorologique Canadian
# 2100 Trans-Canadienne
# Dorval, Quebec
#
# Projet   : Carte Weather at a Glance de WXO
# Fichier  : JetMapper.tcl
# Creation : Fevrier 2004 - J.P.Gauthier - CMC/CMOE
#
# Description:
#    Creer une carte/produit de jetStream ave les types de precip pour WXO.
#
# Arguments :
#   <Run>   : Run to be used
#   <Hour>  : Valid time of map to be generated
#   <Dir>   : Where to save the maps
#   <Map>   : List of maps to be produced
#
# Remarques :
#   - Ce script traite tous les fichiers de trajectoires dans le repertoire specifie
#   - Lancement: SPI -batch -layout JetLayout -macro JetMapper.tcl -args [run] [hour] [dir]
#     Exemple :  SPI -batch -layout JetLayout -macro JetMapper.tcl -args  00 18 ./ WXO
#===============================================================================

namespace eval Macro::JetMapper { } {
   global env
   variable Param
   variable Data
   variable Icon
   variable Lbl

   set Param(Info)      { "Produit WXO JetMap" "WXO JetMap product" }
   set Param(InfoArgs)  { { "Run" "Heure" "Repertoire de sortie" } { "Run" "Hour" "Output directory" } }

   set Param(Run)   00              ;#run to be used (Default)
   set Param(Hour)  18              ;#Valid time of map to be generated (Default)
   set Param(Dir)   ./              ;#Where to save the maps (Default)
   set Param(Path)  $env(CMCCONST)/img.SPI/jetmap

   set Param(Radius)       20                                                          ;#Radius of the Highs and Lows
   set Param(StreamLevels) { 150 175 200 225 250 275 300 350 400 450 500 550 600 650 } ;#List of levels to use for jetstream
   set Param(StreamSpeed)  90                                                          ;#Minimal windspeed to which stop the streamline
   set Param(StreamLen)    29                                                          ;#Length of arrow sections
   set Param(StreamCut)    4                                                           ;#Length of arrow spacings
   set Param(Intervals)    { -100 -40 -30 -20 -10 0 10 20 30 40 }                      ;# Temp intervals for WXO

   set Lbl(Rain)    { "Pluie" "Rain" }
   set Lbl(Snow)    { "Neige" "Snow" }
   set Lbl(Freeze)  { "Verglas" "Freezing rain" }
   set Lbl(Thunder) { "Orage" "Thunderstorm" }
   set Lbl(Winds)   { "Vents > 90kts" "Winds > 90kts" }
   set Lbl(Adress)  { "meteo.gc.ca" "weather.gc.ca" }
   set Lbl(High)    { A H }
   set Lbl(Low)     { D L }
   set Lbl(Footer)  { "Carte du courant jet valide pour" "Jetstream map valid for" }

   set Icon(Flags)   { EnvCanada_Fra.gif EnvCanada_Eng.gif }
   set Icon(Rain)    @$Param(Path)/rain.xbm
   set Icon(Snow)    @$Param(Path)/snow.xbm
   set Icon(Freeze)  @$Param(Path)/freeze.xbm
   set Icon(Thunder) @$Param(Path)/thunder.xbm

   set Data(Rain)    0         ;#Rain existence flag
   set Data(Snow)    0         ;#Snow existence flag
   set Data(Freeze)  0         ;#Freezing rain existence flag
   set Data(Thunder) 0         ;#Thunderstorm existence flag
}

#----------------------------------------------------------------------------
# Name     : <Macro::JetMapper::PrecipGet>
# Creation : Fevrier 2004 - J.P. Gauthier - CMC/CMOE
#
# Goal     : Get precipitation type per pixel block.
#
# Parameters :
#   <X>      : X coordinate of pixel block
#   <Y>      : Y coordinate of pixel block
#   <Step>   : Size of pixel block
#
# Return:
#   <Type>  : Cominant percip type for the pixel block
#
# Remarks :
#
#----------------------------------------------------------------------------
proc Macro::JetMapper::PrecipGet { X Y Step } {

   set t(1) 0
   set t(2) 0
   set t(3) 0
   set t(4) 0
   set t(5) 0
   set t(6) 0
   set t(7) 0

   #----- Loop on pixel block of Step size
   foreach { lat lon elev } [$Viewport::Data(VP) -unproject [expr $X-$Step] [expr $Y-$Step] [expr $X+$Step-1] [expr $Y+$Step-1] 2] {

      set lift [fstdfield stats IL -coordvalue $lat $lon]
      set prec [fstdfield stats NW -coordvalue $lat $lon]

      if { $lift!="-" && $lift<=-4 && $prec!=6 } {
         incr t(7)
         break
      } elseif { $prec!="-" } {
         incr t([expr int($prec)])
      }
   }

   if { $t(7) } {
      return 7
   }

   set t(6) 0
   set max 6
   for { set i 5 } { $i > 0 } { incr i -1 } {
      if { $t($max) < $t($i) } {
         set max $i
      }
   }
   return $max
}

#----------------------------------------------------------------------------
# Name     : <Macro::JetMapper::PrecipPlot>
# Creation : Fevrier 2004 - J.P. Gauthier - CMC/CMOE
#
# Goal     : Draw the precipitation type blocks
#
# Parameters :
#   <Step>   : Size of pixel block
#
# Return:
#
# Remarks :
#
#----------------------------------------------------------------------------
proc Macro::JetMapper::PrecipPlot { Step } {
   variable Icon
   variable Data

   set half   [expr $Step/2]
   set width  [expr [Page::CanvasWidth $Page::Data(Frame)]-$Step]
   set height [expr [Page::CanvasHeight $Page::Data(Frame)]-$Step]

   for { set x $Step } { $x < $width } { incr x $Step } {
      for { set y $Step } { $y < $height } { incr y $Step } {

         switch [Macro::JetMapper::PrecipGet $x $y $half] {
            1 { set icon $Icon(Rain);    set Data(Rain)    1 ;# Rain }
            2 { set icon $Icon(Rain);    set Data(Rain)    1 ;# Snow/Rain }
            3 { set icon $Icon(Freeze);  set Data(Freeze)  1 ;# Freezing rain }
            4 { set icon $Icon(Freeze);  set Data(Freeze)  1 ;# Gresil }
            5 { set icon $Icon(Snow);    set Data(Snow)    1 ;# Snow }
            6 { set icon ""                                  ;# Noting }
            7 { set icon $Icon(Thunder); set Data(Thunder) 1 ;# Thunder }
         }
         if { $icon!="" } {
            $Page::Data(Canvas) create bitmap $x $y -bitmap $icon -foreground #646464
         }
      }
   }
}

#----------------------------------------------------------------------------
# Name     : <Macro::JetMapper::StreamGet>
# Creation : Fevrier 2004 - J.P. Gauthier - CMC/CMOE
#
# Goal     : Calculate streamline candidates for the jetstream
#
# Parameters :
#
# Return:
#   <Streams>: List of streamlines candidate for jetstream
#
# Remarks :
#   - We start streamlines from the highs of the max windspeed within a slice
#     of the upper atmosphere
#----------------------------------------------------------------------------
proc Macro::JetMapper::StreamGet { } {
   variable Param

   #----- Process the upper levels to figure out max winds
   foreach ip1 $Param(StreamLevels) {
      fstdfield read UUT 2 -1 "" $ip1 -1 -1 "" "UU"
      if { [fstdfield is UU] } {
         vexpr UU ifelse(\[UU\]<\[UUT\],UUT,UU)
      } else {
         fstdfield copy UU UUT
      }
   }
   
   #----- Save the computed wind for later inspection
#   file delete -force $Param(Dir)/winds.fstd
#   fstdfile open OUT write $Param(Dir)/winds.fstd

#   fstdfield write UU OUT -32 True
#   fstdfield read TIC 2 -1 "" [fstdfield define UU -IG1] [fstdfield define UU -IG2] [fstdfield define UU -IG3] "" ">>"
#   fstdfield write TIC OUT -32 True
#   fstdfield read TIC 2 -1 "" [fstdfield define UU -IG1] [fstdfield define UU -IG2] [fstdfield define UU -IG3] "" "^^"
#   fstdfield write TIC OUT -32 True
#   fstdfile close OUT

   set streams {}

   #----- Loop on the windspeed highs
   foreach high [lsort -real -decreasing -index 0 [fstdfield stats UU -high 10]] {
      set h [lindex $high 0]
      set i [lindex $high 1]
      set j [lindex $high 2]

      #----- If it's fast enough
      if { $h>=$Param(StreamSpeed) } {
         lappend streams [fstdfield stats UU -coordstream $i $j 1024 0.25 $Param(StreamSpeed) 8.0]
      }
   }

   if { ![llength $streams] } {
      Log::Print WARNING "Did not find any jetstream segments"
   }
   return $streams
}

#----------------------------------------------------------------------------
# Name     : <Macro::JetMapper::StreamPlot>
# Creation : Fevrier 2004 - J.P. Gauthier - CMC/CMOE
#
# Goal     : Draw the jetstream segments
#
# Parameters :
#
# Return:
#
# Remarks :
#
#----------------------------------------------------------------------------
proc Macro::JetMapper::StreamPlot { } {
   variable Param
   variable Data
   
   Macro::JetMapper::StreamGet

   foreach stream [Macro::JetMapper::StreamGet] {

      #----- If the stream is long enough
      if { [llength $stream]>$Param(StreamLen) } {

         #----- Split into arrow segments
         for { set n 0 } { $n<[llength $stream] } { incr n $Param(StreamLen) } {

            set n1 [expr $n+$Param(StreamLen)]
            if { $n1>=[llength $stream] } {
               set n1 end
            }

            #----- Project the segment
            if { [llength [set cc [lindex [$Viewport::Data(VP) -projectline NONE [lrange $stream $n $n1]] 0]]]>2 } {
               set x0 [expr int([lindex $cc 0])]
               set y0 [expr int([lindex $cc 1])]
               set x1 [expr int([lindex $cc end-1])]
               set y1 [expr int([lindex $cc end])]

               #----- If it's long enough and does not overlap other streams
               set items [$Page::Data(Canvas) find overlapping $x0 $y0 $x1 $y1]
               if { [llength $items]<2  && [expr abs(hypot($x1-$x0,$y1-$y0))]>30 } {
                  $Page::Data(Canvas) create line $cc -fill red -arrow last -arrowshape { 20 20 10 } -smooth True -width 10 -transparency 100 -tags STREAM
               }
               set xx0 [expr ($x0<$x1?$x0:$x1)-10]
               set xx1 [expr ($x0<$x1?$x1:$x0)+10]
               set yy0 [expr ($y0<$y1?$y0:$y1)-10]
               set yy1 [expr ($y0<$y1?$y1:$y0)+10]
               
               $Page::Data(Canvas) create rectangle $xx0 $yy0 $xx1 $yy1 -fill white -outline black -transparency 0 -tags STREAM
            }
            incr n $Param(StreamCut)
         }
      }
   }
}

#----------------------------------------------------------------------------
# Name     : <Macro::JetMapper::LegendPlot>
# Creation : Fevrier 2004 - J.P. Gauthier - CMC/CMOE
#
# Goal     : Draw the legend for the various map items
#
# Parameters :
#   <Field>    : Temperatur field
#   <Colormap> : Colormap to use
#   <Intervals>: Intervals to use       
#   <Lang>     : Language
#   <Do>       : Plot precpip
#
# Return:
#
# Remarks :
#
#----------------------------------------------------------------------------
proc Macro::JetMapper::LegendPlot { Field Colormap Intervals Lang Do } {
   variable Param
   variable Data
   variable Lbl
   variable Icon

   fstdfield configure $Field -color black -font FONT12 -rendertexture 1 -colormap $Colormap -intervals $Intervals

   $Page::Data(Canvas) delete LEGEND

   #----- Afficher le logo (FLAG)
   image create photo FLAG -file $Param(Path)/[lindex $Icon(Flags) $Lang]
   $Page::Data(Canvas) create rectangle 5 5 206 35 -fill white -outline black -transparency 70 -tag LEGEND
   $Page::Data(Canvas) create image 10 10 -image FLAG -anchor nw -tag LEGEND

   #----- High - Low
   $Page::Data(Canvas) itemconfigure HLH -text [lindex $Lbl(High) $Lang]
   $Page::Data(Canvas) itemconfigure HLL -text [lindex $Lbl(Low) $Lang]

   if { !$Do } {
      return
   }

   #----- Afficher l'echelle de temperature
   set width  [Page::CanvasWidth $Page::Data(Frame)]
   set height [Page::CanvasHeight $Page::Data(Frame)]

   set dy 20
   set dx 20
   set X0 5
   set Y0 [expr $height/2-125]
   set X1 [expr $X0+75]
   set Y1 [expr $Y0+[llength $Intervals]*($dy+5)+25]

   $Page::Data(Canvas) create rectangle $X0 $Y0 $X1 $Y1 -fill white -outline black -transparency 70 -tag LEGEND
   $Page::Data(Canvas) create text [expr $X0+5] [expr $Y0+5] -text "Temp °C" -font FONT12 -fill black -tag LEGEND -anchor nw

   set y [expr $Y1-5]
   set x [expr $X0+50]

   foreach interval $Intervals {
      $Page::Data(Canvas) create rectangle $x $y [expr $x+$dx] [expr $y-$dy] -fill #[fstdfield configure $Field -val2map $interval] -outline "" -tag LEGEND
      if { $interval != [lindex $Intervals 0] } {
         $Page::Data(Canvas) create text      $x [expr $y+5] -anchor e -text "$interval " -font FONT12 -fill black -tag LEGEND
      }
      incr y [expr -($dy+5)]
   }

   #----- Calculer la date
   set date [fstdstamp todate [fstdfield define UU -DATEV]]
   set secs [clock scan "[join [lrange $date 0 2] ""] [join [lrange $date 3 4] :]" -gmt true]
   set date [DateStuff::StringDateFromSeconds $secs $Lang ""]

   #----- Si francais, date en lowercase
   if { $Lang==0 } {
      set date [string tolower $date]
   }

   $Page::Data(Canvas) create rectangle [expr $width/2-340] [expr $height-50] [expr $width/2+340] [expr $height-5] -fill white -outline black -transparency 70 -tag LEGEND
   $Page::Data(Canvas) create text      [expr $width/2] [expr $height-45] -anchor n -text "[lindex $Lbl(Footer) $Lang] $date UTC" -font FONT12 -fill black -tag LEGEND
   $Page::Data(Canvas) create text      [expr $width/2] [expr $height-10] -anchor s -text [lindex $Lbl(Adress) $Lang] -font FONT10 -fill black -tag LEGEND

   set x [expr $width -26]
   set y 20
   
   #----- Afficher la legende dynamique
   foreach type { Rain Snow Freeze Thunder } {
      if { $Data($type) } {
         $Page::Data(Canvas) create bitmap $x $y -bitmap $Icon($type) -foreground #646464 -tag LEGEND
         incr x -21
         $Page::Data(Canvas) create text $x $y -text [lindex $Lbl($type) $Lang] -font FONT12 -fill black -tag LEGEND -anchor e
         set x [expr $x-[font measure FONT12 [lindex $Lbl($type) $Lang]]-40]
      }
   }

   #----- Afficher la legende des vents
   incr x 10
   $Page::Data(Canvas) create rectangle [expr $x-10] [expr $y-10] [expr $x+10] [expr $y+10] -fill #7D7D7D -transparency 20 -tag LEGEND
   incr x -21
   $Page::Data(Canvas) create text $x $y -text [lindex $Lbl(Winds) $Lang] -font FONT12 -fill black -tag LEGEND -anchor e
   set x [expr $x-[font measure FONT12 [lindex $Lbl(Winds) $Lang]]-40]
   
   $Page::Data(Canvas) create rectangle [expr $width-5] 5 [expr $x+35] 35 -fill white -outline black -transparency 70 -tag "LEGEND LOW"

   #----- Ordonner les items
   $Page::Data(Canvas) raise LEGEND
   $Page::Data(Canvas) lower LOW LEGEND
}

#----------------------------------------------------------------------------
# Name     : <Macro::JetMapper::HighLowPlot>
# Creation : Fevrier 2004 - J.P. Gauthier - CMC/CMOE
#
# Goal     : Draw the pressure high and lows
#
# Parameters :
#   <Field>  : Pressure field
#
# Return:
#
# Remarks :
#
#----------------------------------------------------------------------------
proc Macro::JetMapper::HighLowPlot { Field } {
   variable Param

   $Page::Data(Canvas) delete HL

   foreach high [fstdfield stats $Field -high 12] {
      set ll  [fstdfield stats $Field -gridpoint [lindex $high 1] [lindex $high 2]]
      if { [set xy [$Viewport::Data(VP) -project [lindex $ll 0] [lindex $ll 1] 0]]!= "" && [lindex $xy 2]>0 } {
         set x [lindex $xy 0]
         set y [lindex $xy 1]
         $Page::Data(Canvas) create oval [expr $x-$Param(Radius)] [expr $y-$Param(Radius)] [expr $x+$Param(Radius)] [expr $y+$Param(Radius)] -fill white -outline black -width 2 -transparency 70 -tag HL
         $Page::Data(Canvas) create text [expr $x+2] [expr $y-3] -font FONT16 -justify center -fill black -tag "HL HLH"
         $Page::Data(Canvas) create text [expr $x+2] [expr $y+10] -text "[format %.0f [expr round([lindex $high 0])]]" -font FONT8 -justify center -fill black -tag HL
      }
   }

   foreach low [fstdfield stats $Field -low 12] {
      set ll  [fstdfield stats $Field -gridpoint [lindex $low 1] [lindex $low 2]]
      if { [set xy [$Viewport::Data(VP) -project [lindex $ll 0] [lindex $ll 1] 0]]!= "" && [lindex $xy 2]>0 } {
         set x [lindex $xy 0]
         set y [lindex $xy 1]
         $Page::Data(Canvas) create oval [expr $x-$Param(Radius)] [expr $y-$Param(Radius)] [expr $x+$Param(Radius)] [expr $y+$Param(Radius)] -fill white -outline black -width 2 -transparency 70 -tag HL
         $Page::Data(Canvas) create text [expr $x+2] [expr $y-3] -font FONT16 -justify center -fill black -tag "HL HLL"
         $Page::Data(Canvas) create text [expr $x+2] [expr $y+10] -text "[format %.0f [expr round([lindex $low 0])]]" -font FONT8 -justify center -fill black -tag HL
      }
   }
}

#----------------------------------------------------------------------------
# Name     : <Macro::JetMapper::Print>
# Creation : Fevrier 2004 - J.P. Gauthier - CMC/CMOE
#
# Goal     : Save the products
#
# Parameters :
#
# Return:
#
# Remarks :
#
#----------------------------------------------------------------------------
proc Macro::JetMapper::Print { } {
   variable Param

   set file R1_north@america_I_SPI@JETSTREAM

   #----- Francais
   Macro::JetMapper::LegendPlot TTI CMAPWXO $Param(Intervals) 0 True
   PrintBox::Image $Page::Data(Frame) ppm $Param(Dir)/$file
   exec convert -antialias -resize 555x421+! $Param(Dir)/$file.ppm png:$Param(Dir)/${file}_fr@wxoffice_0$Param(Hour).png
   exec convert -antialias -resize 555x421+! $Param(Dir)/$file.ppm gif:$Param(Dir)/${file}_fr@wxoffice_0$Param(Hour).gif

   #----- English
   Macro::JetMapper::LegendPlot TTI CMAPWXO $Param(Intervals) 1 True
   PrintBox::Image $Page::Data(Frame) ppm $Param(Dir)/$file
   exec convert -antialias -resize 555x421+! $Param(Dir)/$file.ppm png:$Param(Dir)/${file}_en@wxoffice_0$Param(Hour).png
   exec convert -antialias -resize 555x421+! $Param(Dir)/$file.ppm gif:$Param(Dir)/${file}_en@wxoffice_0$Param(Hour).gif
}

#----------------------------------------------------------------------------
# Name     : <Macro::JetMapper::Execute>
# Creation : Fevrier 2004 - J.P. Gauthier - CMC/CMOE
#
# Goal     : Launch the macro
#
# Parameters :
#
# Return:
#
# Remarks :
#
#----------------------------------------------------------------------------
proc Macro::JetMapper::Execute { } {
   variable Param
   variable Data
   global env

   set date [string range [lindex [lsort -dictionary -increasing [glob -directory $env(CMCGRIDF)/prog/regeta/ -tails *$Param(Run)_???]] end] 0 7]

   colormap create CMAPWIND
   colormap control CMAPWIND -add 0 125 125 125 20
   colormap control CMAPWIND -add 2 125 125 125 20

   colormap create CMAPWXO
   colormap read CMAPWXO $Param(Path)/JetMap.rgba

   catch {
      font create FONT16  -family Arial -weight bold -size -18
      font create FONT12  -family Arial -weight bold -size -16
      font create FONT10  -family Arial -weight bold -size -12
      font create FONT8   -family Arial -weight bold -size -10
   }
   #----- Recuperer le champs
   fstdfile open 1 read $env(CMCGRIDF)/prog/regdiag/$date$Param(Run)_0$Param(Hour)
   fstdfile open 2 read $env(CMCGRIDF)/prog/regpres/$date$Param(Run)_0$Param(Hour)
   fstdfile open 3 read $env(CMCGRIDF)/prog/regeta/$date$Param(Run)_0$Param(Hour)
   fstdfile open 4 read $Param(Path)/grid.fstd

   fstdfield read IL  1 -1 "" 0     -1 -1 "" "IL"
   fstdfield read NW  1 -1 "" 0     -1  3 "" "NW"
   fstdfield read TT  3 -1 "" 12000 -1 -1 "" "TT"
   fstdfield read PN  2 -1 "" 0     -1 -1 "" "PN"
   fstdfield read TTI 4 -1 "" -1    -1 -1 "" ""

   fstdfield configure PN -color black -font FONT12 -rendertexture 0 -colormap CMAPWXO -rendercontour 3
   fstdfield configure NW -color black -font FONT12 -rendertexture 1 -colormap CMAPWXO -intervals "1 2 3 4 5 6" -interpdegree NEAREST -factor 0.001

   Macro::JetMapper::StreamPlot
   Macro::JetMapper::PrecipPlot 32
   Macro::JetMapper::HighLowPlot PN

   #----- Interpoler TT sur une grille a moindre resolution pour "smoother" le champs
   fstdfield gridinterp TTI TT
   fstdfield stats TTI -leveltype MASL -levels 0
   Viewport::Assign $Page::Data(Frame) $Viewport::Data(VP) { TTI }

   #----- Add high speed wind volume
   fstdfield read UU 2 -1 "" 250  -1 -1 "" "UU"
   fstdfield readcube UU True $Param(StreamLevels)
   fstdfield configure UU -color black -rendertexture 1 -rendervolume 1 -width 1 -colormap CMAPWIND -intervals $Param(StreamSpeed)
   Viewport::Assign $Page::Data(Frame) $Viewport::Data(VP) { UU }

   Macro::JetMapper::Print

   fstdfile close 1
   fstdfile close 2
   fstdfile close 3
   fstdfile close 4

   if { $SPI::Param(Batch) } {
      SPI::Quit
   }
}

#----------------------------------------------------------------------------
# Name     : <Macro::JetMapper::Clean>
# Creation : Fevrier 2004 - J.P. Gauthier - CMC/CMOE
#
# Goal     : Clear macro resources
#
# Parameters :
#
# Return:
#
# Remarks :
#
#----------------------------------------------------------------------------
proc Macro::JetMapper::Clean { } {

   fstdfield free IL NW TT PN TTI UU UUT BUF
   colormap  free CMAPWIND CMAPWXO
}

#----------------------------------------------------------------------------
# Name     : <Macro::JetMapper::Args>
# Creation : Fevrier 2004 - J.P. Gauthier - CMC/CMOE
#
# Goal     : Process macro arguments
#
# Parameters :
#
# Return:
#
# Remarks :
#
#----------------------------------------------------------------------------
proc Macro::JetMapper::Args { } {
   global argv argc
   variable Param

   if { $argc } {
      set Param(Run)   [lindex $argv 0]
      set Param(Hour)  [lindex $argv 1]
      set Param(Dir)   [lindex $argv 2]
   }
}
