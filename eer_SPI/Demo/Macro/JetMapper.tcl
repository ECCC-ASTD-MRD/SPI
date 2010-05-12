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
#    Creer une carte/produit de jetStream ave les types de precip pour WXO et PYR.
#
# Arguments :
#   <Run>   : Run to be used
#   <Hour>  : Valid time of map to be generated
#   <Dir>   : Where to save the maps
#   <Map>   : List of maps to be produced
#
# Remarques :
#   - Ce script traite tous les fichiers de trajectoires dans le repertoire specifie
#   - Lancement: SPI -batch -layout JetLayout -script JetMapper.tcl -args [run] [hour] [dir] [map]
#     Exemple :  SPI -batch -layout JetLayout -script JetMapper.tcl -args  00 18 ./ WXO+PYR
#===============================================================================

namespace eval Macro::JetMapper { } {
   global env
   variable Param
   variable Data
   variable Icon
   variable Lbl

   set Param(Info)      { "Produit WXO JetMap" "WXO JetMap product" }
   set Param(InfoArgs)  { { "Run" "Heure" "Repertoirede sortie" "Produit (WXO+PYR)" } { "Run" "Hour" "Output directory" "Product (WXO+PYR)" } }

   set Param(Run)   00              ;#Run to be used
   set Param(Hour)  18              ;#Valid time of map to be generated
   set Param(Dir)   ./              ;#Where to save the maps
   set Param(Map)   { WXO PYR }     ;#List of maps to be produced
   set Param(Path)  $env(CMCCONST)/img.SPI/jetmap

   set Param(Radius)       20       ;#Radius of the Highs and Lows
   set Param(StreamStop)   50       ;#Minimal windspeed to which stop the streamline
   set Param(StreamStart)  75       ;#Minimal windspeed to start a streamline
   set Param(StreamMask)   50       ;#Grid point width of mask aroound stream
   set Param(StreamLen)    29       ;#Length of arrow sections
   set Param(StreamCut)    4        ;#Length of arrow spacings
   set Param(I0)           150      ;#Gridpoint range into which to look for jetstream
   set Param(J0)           50
   set Param(I1)           450
   set Param(J1)           600
   set Param(Intervals)    { -100 -40 -30 -20 -10 0 10 20 30 40 }
   set Param(IntervalsPYR) { -100 -30 -20 -10 0 10 20 30 40 }

   set Lbl(Rain)    { "Pluie" "Rain" }
   set Lbl(Snow)    { "Neige" "Snow" }
   set Lbl(Freeze)  { "Verglas" "Freezing rain" }
   set Lbl(Thunder) { "Orage" "Thunderstorm" }
   set Lbl(Adress)  { "www.meteo.ec.gc.ca" "weatheroffice.ec.gc.ca" }
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

proc Macro::JetMapper::PrecipGet { X Y Step } {

   set x0 [expr $X-$Step]
   set x1 [expr $X+$Step]
   set y0 [expr $Y-$Step]
   set y1 [expr $Y+$Step]

   set t(1) 0
   set t(2) 0
   set t(3) 0
   set t(4) 0
   set t(5) 0
   set t(6) 0
   set t(7) 0

   for { set x $x0 } { $x < $x1 } { incr x 2 } {
      for { set y $y0 } { $y < $y1 } { incr y 2 } {
         set ll [$Viewport::Data(VP) -unproject $x $y]
         set lat [lindex $ll 0]
         set lon [lindex $ll 1]

         set lift [fstdfield stats IL -coordvalue $lat $lon]
         set prec [expr int([fstdfield stats NW -coordvalue $lat $lon])]

	      if { $lift!="-" && $lift<0 && $prec!=6} {
            incr t(7)
         } else {
            incr t($prec)
         }
      }
   }

   if { $t(7) } {
      return 7
   }

   set max 1
   for { set i 1 } { $i <=6 } { incr i } {
      if { $t($max) < $t($i) } {
         set max $i
      }
   }
   return $max
}

proc Macro::JetMapper::PrecipPlot { Step } {
   variable Icon
   variable Data

   set half  [ expr $Step/2]
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

proc Macro::JetMapper::StreamGet { } {
   variable Param

   #----- Process the upper levels to figure out max winds
   fstdfield read UU 2 -1 "" 100  -1 -1 "" "UU"
   foreach ip1 { 150 200 250 300 400 500 600 650 } {
      fstdfield read UUT 2 -1 "" $ip1 -1 -1 "" "UU"
      vexpr UU ifelse(\[UU\]<\[UUT\],UUT,UU)
   }

   #----- Save the computed wind for later inspection
#   fstdfile open OUT write ./winds.fstd
#   fstdfield write UU OUT -32 True
#   fstdfield read TIC 2 -1 "" -1  -1 -1 "" ">>"
#   fstdfield write TIC OUT -32 True
#   fstdfield read TIC 2 -1 "" -1  -1 -1 "" "^^"
#   fstdfield write TIC OUT -32 True
#   fstdfile close OUT

   vexpr BUF UU\[0\]<<0

   set streams {}

   foreach high [lsort -real -decreasing -index 0 [fstdfield stats UU -high 8]] {
      set h [lindex $high 0]
      set i [lindex $high 1]
      set j [lindex $high 2]

      if { $h>=$Param(StreamStart) && $i>=$Param(I0) && $i<=$Param(I1) && $j>=$Param(J0) && $j<=$Param(J1) } {
         set coords [fstdfield stats UU -coordstream $i $j 1024 0.25 $Param(StreamStop) 8.0]
         lappend streams [list [llength $coords] $i $j $coords]
      }
   }
   return $streams
}

proc Macro::JetMapper::StreamPlot { } {
   variable Param
   variable Data

   foreach stream [Macro::JetMapper::StreamGet] {

      set coords [lindex $stream 3]
      set i      [lindex $stream 1]
      set j      [lindex $stream 2]

      if { [llength $coords]>$Param(StreamLen) } {
         if { [fstdfield stats BUF -gridvalue $i $j]==1.0 } {
            continue
         }

         for { set n 0 } { $n<[llength $coords] } { incr n $Param(StreamLen) } {

            set ij  [fstdfield stats BUF -coordpoint [lindex $coords $n] [lindex $coords [expr $n+1]]]
            if { [fstdfield stats BUF -gridvalue [expr int([lindex $ij 0])] [expr int([lindex $ij 1])]]!=0 } {
               continue
            }

            set n1 [expr $n+$Param(StreamLen)]
            if { $n1>=[llength $coords] } {
               set n1 end
            }
            set co [lrange $coords $n $n1]
            catch {
               set cc [lindex [$Viewport::Data(VP) -projectline NONE $co] 0]
               $Page::Data(Canvas) create line $cc -fill red -arrow last -arrowshape { 20 20 10 } -smooth True -width 12 -transparency 100 -tags STREAM
            }
            incr n $Param(StreamCut)

         }

         foreach { lat lon v } $coords {
            set ij [fstdfield stats UU -coordpoint $lat $lon]
            set x [lindex $ij 0]
            set y [lindex $ij 1]

            set x0 [expr int($x)-$Param(StreamMask)]
            set x1 [expr int($x)+$Param(StreamMask)]
            set y0 [expr int($y)-$Param(StreamMask)]
            set y1 [expr int($y)+$Param(StreamMask)]

            set x0 [expr $x0<0?0:$x0]
            set x1 [expr $x1>$Param(I1)?$Param(I1):$x1]
            set y0 [expr $y0<0?0:$y0]
            set y1 [expr $y1>$Param(J1)?$Param(J1):$y1]

            vexpr BUF BUF(($x0,$x1),($y0,$y1))=1
         }
      }
   }
   $Page::Data(Canvas) lower STREAM HL
}

proc Macro::JetMapper::Legend { Field Colormap Intervals Lang Do } {
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
   $Page::Data(Canvas) create rectangle [expr $width-5] 5 [expr $x+35] 35 -fill white -outline black -transparency 70 -tag "LEGEND LOW"

   #----- Ordonner les items
   $Page::Data(Canvas) raise LEGEND
   $Page::Data(Canvas) lower LOW LEGEND
}

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

proc Macro::JetMapper::Print { } {
   variable Param

   set file [clock format [clock seconds] -format "%Y%m%d" -gmt true]$Param(Run)_054_R1_north@america_I_SPI@JETSTREAM

   if { [lsearch -exact $Param(Map) WXO]!=-1 } {

      #----- Francais
      Macro::JetMapper::Legend TTI CMAPWXO $Param(Intervals) 0 True
      PrintBox::Image $Page::Data(Frame) ppm $Param(Dir)/$file
      exec convert -antialias -resize 555x421+! $Param(Dir)/$file.ppm png:$Param(Dir)/${file}_fr@wxoffice_0$Param(Hour).png
      exec convert -antialias -resize 555x421+! $Param(Dir)/$file.ppm gif:$Param(Dir)/${file}_fr@wxoffice_0$Param(Hour).gif

      #----- English
      Macro::JetMapper::Legend TTI CMAPWXO $Param(Intervals) 1 True
      PrintBox::Image $Page::Data(Frame) ppm $Param(Dir)/$file
      exec convert -antialias -resize 555x421+! $Param(Dir)/$file.ppm png:$Param(Dir)/${file}_en@wxoffice_0$Param(Hour).png
      exec convert -antialias -resize 555x421+! $Param(Dir)/$file.ppm gif:$Param(Dir)/${file}_en@wxoffice_0$Param(Hour).gif
   }

   if { [lsearch -exact $Param(Map) PYR]!=-1 } {

      #----- English (PYR)
      Macro::JetMapper::Legend TTI CMAPPYR $Param(IntervalsPYR) 1 False
      PrintBox::Image $Page::Data(Frame) ppm $Param(Dir)/$file
      exec convert -antialias -resize 855x713+! $Param(Dir)/$file.ppm png:$Param(Dir)/${file}_en@media_0$Param(Hour).png
      exec convert -antialias -resize 855x713+! $Param(Dir)/$file.ppm gif:$Param(Dir)/${file}_en@media_0$Param(Hour).gif
   }
}

proc Macro::JetMapper::Execute { } {
   variable Param
   variable Data
   global env

   set date [string range [lindex [lsort -dictionary -increasing [glob -directory $env(CMCGRIDF)/prog/regeta/ -tails *$Param(Run)_???]] end] 0 7]

   colormap create CMAPWXO
   colormap read CMAPWXO $Param(Path)/JetMap.rgba

   colormap create CMAPPYR
   colormap read CMAPPYR $Param(Path)/JetMapPYR.rgba

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

   Macro::JetMapper::PrecipPlot 32
   Macro::JetMapper::HighLowPlot PN
   Macro::JetMapper::StreamPlot

   #----- Interpoler TT sur une grille a moindre resolution pour "smoohter" le champs
   fstdfield gridinterp TTI TT
   fstdfield stats TTI -leveltype MASL -levels 0
   Viewport::Assign $Page::Data(Frame) $Viewport::Data(VP) { TTI }

   Macro::JetMapper::Print

   fstdfile close 1
   fstdfile close 2
   fstdfile close 3
   fstdfile close 4
}

proc Macro::JetMapper::Clean { } {

   fstdfield free  IL NW TT PN TTI UU UUT BUF
   colormap free CMAPWXO CMAPPYR
}

proc Macro::JetMapper::Args { } {
   global argv argc
   variable Param

   if { $argc } {
      set Param(Run)   [lindex $argv 0]
      set Param(Hour)  [lindex $argv 1]
      set Param(Dir)   [lindex $argv 2]
      set Param(Map)   [split [lindex $argv 3] +]
   }
}



