#===============================================================================
# Environnement Canada
# Centre Meteorologique Canadian
# 2100 Trans-Canadienne
# Dorval, Quebec
#
# Projet    : Librairie de fonctions pour dessiner des formes dans un canvas
# Fichier   : CanvasShape.tcl
# Creation  : Octobre 1998 - J.P. Gauthier - CMC/CMOE
#
# Description: Definitions de diverses fonction s afin de dessiner des
#              formes standard dans un canvas.
#
# Fonctions:
#
#    CVCompass::Create     { Frame X Y }
#    CVCompass::Destroy    { Frame }
#    CVCompass::Update     { Frame Bearing Angle Distance { Speed 0 } }
#    CVCompass::Rotate     { Frame X Y }
#    CVCompass::Write      { Frame File }
#
#    CVClock::Create       { Frame X Y }
#    CVClock::Destroy      { Frame }
#    CVClock::Exist        { Frame }
#    CVClock::Time         { Frame Sec Total }
#    CVClock::Write        { Frame File }
#
#    CVScale::Create       { Frame X Y Size }
#    CVScale::Destroy      { Frame }
#    CVScale::Update       { Frame VP }
#    CVScale::Set          { Frame }
#    CVScale::Write        { Frame File }
#
#    CVGeoLegend::Create   { Frame X Y List }
#    CVGeoLegend::Destroy  { Frame }
#    CVGeoLegend::Update   { Frame List }
#    CVGeoLegend::Write    { Frame File }
#
#    CVText::Create        { Frame { X0 0 } { Y0 0 } { Width 0 } { Height 0 } { Text "" } }
#    CVText::Update        { Frame Tag Text }
#    CVText::Destroy       { Frame Tag }
#    CVText::Write         { Frame File }
#    CVText::Move          { Canvas Tag }
#    CVText::Scale         { Canvas Tag X Y }
#    CVText::Init          { Canvas }
#    CVText::Focus         { Canvas X Y }
#    CVText::Copy          { Canvas }
#    CVText::BackSpace     { Canvas }
#    CVText::Delete        { Canvas }
#    CVText::Drag          { Canvas X Y }
#    CVText::Erase         { Canvas }
#    CVText::Hit           { Canvas X Y { select 1 } }
#    CVText::Insert        { Canvas Char }
#    CVText::Go            { Canvas Incr }
#    CVText::GoEnd         { Canvas Pos }
#    CVText::Paste         { Canvas { X {} } { Y {} } }
#    CVText::Select        { Canvas }
#
#    CVMagnifier::Activate   { Canvas X Y }
#    CVMagnifier::DeActivate { Canvas }
#    CVMagnifier::Create     { Canvas }
#    CVMagnifier::Destroy    { Canvas }
#    CVMagnifier::Decrease   { Canvas X Y }
#    CVMagnifier::Increase   { Canvas X Y }
#    CVMagnifier::Move       { Canvas X Y }
#
#    CVTree::Create         { Canvas Tree args }
#    CVTree::ExecCallback   { Callback Canvas Tree Branch Open }
#    CVTree::Highlight      { Canvas Tree {Branch ""} }
#    CVTree::OnBoxClick     { Canvas Tree Branch Open }
#    CVTree::OnClick        { Canvas Tree Branch Open IsLeaf {Ctrl False} {Shift False} }
#    CVTree::OnDblClick     { Canvas Tree Branch Open IsLeaf }
#    CVTree::Render         { Canvas Tree }
#    CVTree::RenderBranch   { Canvas Tree Branch X Y }
#    CVTree::SelectBranch   { Canvas Tree Branch }
#    CVTree::SelectSetState { Selected Canvas Tree Branch {IsLeaf False} {Persistent False} }
#    CVTree::SelectionClear { Canvas Tree }
#    CVTree::SelectionGet   { Canvas Tree }

#    Shape::BindDestroy    { Canvas Tag { Command "" } }
#    Shape::BindFull       { Canvas Tag Var { Command "" } }
#    Shape::BindAllMove    { Canvas Tags { Command "" } }
#    Shape::BindMove       { Canvas Tags { Command "" } }
#    Shape::BindScale      { Canvas Tag { Command "" } }
#    Shape::BindWidget     { Canvas Tag }
#    Shape::UnBind         { Canvas Tag }
#    Shape::Full           { Canvas Tag args }
#    Shape::Move           { Canvas Tags X Y { Direct False } }
#    Shape::Scale          { Canvas Tag X Y args }
#    Shape::Set            { Canvas Tag X Y }
#    Shape::UnSet          { Canvas Tag }
#    Shape::Widget         { Canvas Tag X Y Visible }
#
#    Shape::DrawHBAR       { Canvas Pixel Tags Color Size Fill }
#    Shape::DrawVBAR       { Canvas Pixel Tags Color Size Fill }
#    Shape::DrawCIRCLE     { Canvas Pixel Tags Color Size Fill }
#    Shape::DrawLOZENGE    { Canvas Pixel Tags Color Size Fill
#    Shape::DrawSAND       { Canvas Pixel Tags Color Size Fill }
#    Shape::DrawSQUARE     { Canvas Pixel Tags Color Size Fill }
#    Shape::DrawSTAR       { Canvas Pixel Tags Color Size Fill }
#    Shape::DrawTRIANGLE   { Canvas Pixel Tags Color Size Fill }
#    Shape::DrawPENTAGON   { Canvas Pixel Tags Color Size Fill }
#    Shape::DrawHEXAGON    { Canvas Pixel Tags Color Size Fill }
#    Shape::DrawLIGHTNING  { Canvas Pixel Tags Color Size Fill }
#    Shape::DrawX          { Canvas Pixel Tags Color Size Fill }
#    Shape::Draw+          { Canvas Pixel Tags Color Size Fill }
#    Shape::DrawIcoMETF    { Canvas Pixel Tags Color Size Fill }
#    Shape::DrawIcoVAAC    { Canvas Pixel Tags Color Size Fill }
#    Shape::DrawStringTest { Canvas X Y Factor Tags Color }
#
# Remarques :
#   -Tous les points sont passer sous forme de liste { X Y }
#
#===============================================================================

package provide CanvasShape 1.6

catch { SPI::Splash "Loading Canvas Package CanvasShape 1.6" }

image create photo COMPASSFRAME   -file $GDefs(Dir)/share/image/System/CompassFrame.gif
image create photo COMPASSDIR     -file $GDefs(Dir)/share/image/System/CompassDir.gif
image create photo COMPASSDIST    -file $GDefs(Dir)/share/image/System/CompassDist.gif
image create photo COMPASSHEADING -file $GDefs(Dir)/share/image/System/CompassHeading.gif
image create photo CLOCKFRAME     -file $GDefs(Dir)/share/image/System/ClockFrame.gif
image create photo CLOCKTIME      -file $GDefs(Dir)/share/image/System/ClockTime.gif
image create photo CLOCKDATE      -file $GDefs(Dir)/share/image/System/ClockDate.gif

namespace eval CVCompass { }

#----------------------------------------------------------------------------
# Nom      : <CVCompass::Create>
# Creation : Mai 2003 - J.P. Gauthier - CMC/CMOE
#
# But      : Creer une rose des vents
#
# Parametres :
#  <Frame>   : Identificateur de page
#  <X>       : Position en X
#  <Y>       : Position en Y
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc CVCompass::Create { Frame X Y } {
   global   GDefs
   variable Data

   set Data(Size) 25
   set canvas $Frame.page.canvas

   $canvas create line $X $Y $X $Y -width 0 -fill white -tag "CVCOMP CVCOMPLOC"
   $canvas create rectangle $X $Y $X $Y -tag "CVCOMP CVCOMPBBOX VPINTRUDE" -width 0

   $canvas create image $X $Y -image COMPASSFRAME -tags "CVCOMP"

   $canvas create image $X [expr $Y+50] -image CLOCKTIME -tags "CVCOMP"
   $canvas create image $X [expr $Y+72] -image COMPASSDIST -tags "CVCOMP CVCOMPSPD"
   $canvas create image $X [expr $Y+72] -image COMPASSDIST -tags "CVCOMP"

   $canvas create text $X [expr $Y+50] -tags "CVCOMP CVCOMPANGLE" -font XFont12 -fill black
   $canvas create text $X [expr $Y+72] -tags "CVCOMP CVCOMPSPD CVCOMPSPDT" -font XFont12 -fill black
   $canvas create text $X [expr $Y+72] -tags "CVCOMP CVCOMPDIST" -font XFont12 -fill black

   $canvas create image $X $Y -image COMPASSHEADING -tags "CVCOMP CVCOMPH"
   $canvas create image $X [expr $Y-25] -image COMPASSDIR -tags "CVCOMP CVCOMPN"
   $canvas create image $X [expr $Y+25] -image COMPASSDIR -tags "CVCOMP CVCOMPS"
   $canvas create image [expr $X-25] $Y -image COMPASSDIR -tags "CVCOMP CVCOMPW"
   $canvas create image [expr $X+25] $Y -image COMPASSDIR -tags "CVCOMP CVCOMPE"

   $canvas create text $X $Y -tags "CVCOMPHT" -font XFont12 -fill black
   $canvas create text $X [expr $Y-25] -text N -tags "CVCOMPNT" -font XFont12
   $canvas create text $X [expr $Y+25] -text S -tags "CVCOMPST" -font XFont12
   $canvas create text [expr $X-25] $Y -text [lindex { O W } $GDefs(Lang)] -tags "CVCOMPWT" -font XFont12
   $canvas create text [expr $X+25] $Y -text E -tags "CVCOMPET" -font XFont12
   eval $canvas coords CVCOMPBBOX [$canvas bbox CVCOMP]

   Shape::BindAllMove $canvas { CVCOMP CVCOMPHT CVCOMPNT CVCOMPST CVCOMPWT CVCOMPET }

   #----- Binding de rotation

   foreach tag { CVCOMPNT CVCOMPST CVCOMPWT CVCOMPET } theta { 0 180 90 270 } {
      $canvas bind $tag <Enter>     "$canvas configure -cursor hand1"
      $canvas bind $tag <Leave>     "$canvas configure -cursor left_ptr"
      $canvas bind $tag <B1-Motion> "CVCompass::RotateDo $Frame %x %y $theta"
   }

   #----- Binding de translation

   $canvas bind CVCOMPHT <Enter>     "$canvas configure -cursor hand1"
   $canvas bind CVCOMPHT <Leave>     "$canvas configure -cursor left_ptr"
   $canvas bind CVCOMPHT <B1-Motion>       "CVCompass::TranslateDo $Frame %x %y"
   $canvas bind CVCOMPHT <ButtonRelease-1> "CVCompass::TranslateDone $Frame "

   Page::MaskItem $Frame
}

proc CVCompass::TranslateDo { Frame X Y  } {
   variable Data

   upvar #0 ProjCam::Data${Frame}::Cam  cam

   set xy [$Frame.page.canvas coords CVCOMPLOC]
   set dxy [expr hypot([lindex $xy 0]-$X,[lindex $xy 1]-$Y)]

   $Frame.page.canvas coords CVCOMPHT $X $Y
   $Frame.page.canvas coords CVCOMPH $X $Y

   if { [set vp [lindex [Page::Registered $Frame Viewport] 0]]!="" } {
      set bearing [expr atan2($Y-[lindex $xy 1],$X-[lindex $xy 0])*57.2957795+90.0+$cam(CFX)]
      set speed   [expr $dxy*[projcam stats $Frame -aspect]*0.00001]

      Viewport::GoAlong $Frame $speed $bearing $Viewport::Map(Lat) $Viewport::Map(Lon) False
   }
}

proc CVCompass::TranslateDone { Frame } {
   variable Data

   Viewport::GoAlong $Frame 0 0 $Viewport::Map(Lat) $Viewport::Map(Lon) False
   $Frame.page.canvas coords CVCOMPHT [lrange [$Frame.page.canvas coords CVCOMPLOC] 0 1]
   $Frame.page.canvas coords CVCOMPH  [lrange [$Frame.page.canvas coords CVCOMPLOC] 0 1]
}

#----------------------------------------------------------------------------
# Nom      : <CVCompass::RotateDo>
# Creation : Mai 2007 - J.P. Gauthier - CMC/CMOE
#
# But      : Rotation interactive de la rose des vents et camera
#
# Parametres :
#  <Frame>   : Identificateur de page
#  <X>       : Position en X
#  <Y>       : Position en Y
#  <Theta>   : Angle de reference
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc CVCompass::RotateDo { Frame X Y Theta } {

   upvar #0 ProjCam::Data${Frame}::Cam  cam

   set xy [$Frame.page.canvas coords CVCOMPLOC]

   if { [set vp [lindex [Page::Registered $Frame Viewport] 0]]!="" } {
      set bearing [expr -atan2($Y-[lindex $xy 1],$X-[lindex $xy 0])*57.2957795-90.0-$Theta]

      set cam(CFX) $bearing
      ProjCam::Do $Frame $Frame $vp
   }
}

#----------------------------------------------------------------------------
# Nom      : <CVCompass::Destroy>
# Creation : Mai 2003 - J.P. Gauthier - CMC/CMOE
#
# But      : Detruier la rose des vents
#
# Parametres :
#  <Frame>   : Identificateur de page
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc CVCompass::Destroy { Frame } {

   $Frame.page.canvas delete CVCOMP CVCOMPHT CVCOMPNT CVCOMPST CVCOMPWT CVCOMPET
}

#----------------------------------------------------------------------------
# Nom      : <CVCompass::Update>
# Creation : Mai 2003 - J.P. Gauthier - CMC/CMOE
#
# But      : Mise a jour de l'angle
#
# Parametres :
#  <Frame>   : Identificateur de page
#  <Bearing> : Direction
#  <Angle>   : Azimuth
#  <Distance>: Distance
#  <Speed>   : Vitesse
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc CVCompass::Update { Frame Bearing Angle Distance { Speed 0 } } {
   variable Data

   set canvas $Frame.page.canvas

   set co [$canvas coords CVCOMPLOC]
   set x0 [lindex $co 0]
   set y0 [lindex $co 1]

   set rad [expr $Bearing*0.01745329238474369049]
   set s   $Data(Size)
   set xs  [expr $s*sin($rad)]
   set ys  [expr $s*cos($rad)]

   incr s 5
   set x  [expr $s*sin($rad)]
   set y  [expr $s*cos($rad)]

   $canvas coords CVCOMPN   [expr $x0-$x] [expr $y0-$y]
   $canvas coords CVCOMPS   [expr $x0+$x] [expr $y0+$y]
   $canvas coords CVCOMPNT  [expr $x0-$x] [expr $y0-$y]
   $canvas coords CVCOMPST  [expr $x0+$x] [expr $y0+$y]
   $canvas coords CVCOMPNS  [expr $x0-$xs] [expr $y0-$ys] [expr $x0+$xs] [expr $y0+$ys]

   #----- Est-Ouest
   $canvas coords CVCOMPE   [expr $x0+$y] [expr $y0-$x]
   $canvas coords CVCOMPW   [expr $x0-$y] [expr $y0+$x]
   $canvas coords CVCOMPET  [expr $x0+$y] [expr $y0-$x]
   $canvas coords CVCOMPWT  [expr $x0-$y] [expr $y0+$x]
   $canvas coords CVCOMPEW  [expr $x0+$ys] [expr $y0-$xs] [expr $x0-$ys] [expr $y0+$xs]

   if { $Bearing<0 } {
      set Bearing [expr $Bearing+360.0]
   }
   $canvas itemconfigure CVCOMPHT -text [format "%03.0f" $Bearing]

   #----- If spinning, show speed
   if { $Speed!=0.0 } {
      $canvas coords CVCOMPSPD $x0 [expr $y0+94]
      $canvas coords CVCOMPSPDT $x0 [expr $y0+94]
      $canvas itemconfigure CVCOMPSPDT -text [format "%.0f m/s" [expr abs($Speed*1000)]]
   } else {
      $canvas coords CVCOMPSPD $x0 [expr $y0+72]
      $canvas coords CVCOMPSPDT $x0 [expr $y0+72]
      $canvas itemconfigure CVCOMPSPDT -text ""
   }

   set co [$canvas coords CVFLYBASE]
   set x0 [lindex $co 0]
   set y0 [lindex $co 1]

   set x  [expr $Data(Size)*sin($Angle*0.01745329)]
   set y  [expr $Data(Size)*cos($Angle*0.01745329)]
   set d  [expr $Data(Size)*0.5]

   $canvas itemconfigure CVCOMPANGLE -text [format "%.2f°" $Angle]
   $canvas itemconfigure CVCOMPDIST  -text [Convert::FormatDist $Distance]
}

#------------------------------------------------------------------------------
# Nom      : <CVCompass::Write>
# Creation : Novembre 2003 - J.P. Gauthier - CMC/CMOE -
#
# But     : Engeristrer les parametres du CVCompass dans un fichier Layout
#
# Parametres :
#   <Frame>  : Identificateur de Page
#   <File>   : Identificateur de Fichier
#
# Remarques :
#
#-------------------------------------------------------------------------------

proc CVCompass::Write { Frame File } {
   variable Data

   puts $File ""
   puts $File "   #----- Affichage du compas"
   set c [$Frame.page.canvas coords CVCOMPLOC]
   puts $File "   CVCompass::Create \$Frame [lindex $c 0] [lindex $c 1]"
}

#----------------------------------------------------------------------------
# Nom      : <CVClock::Create>
# Creation : Decembre 2000 - J.P. Gauthier - CMC/CMOE
#
# But      : Dessine une Horloge
#
# Parametres :
#  <Frame>   : Identificateur de page
#  <X>       : Position en X
#  <Y>       : Position en Y
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------
namespace eval CVClock {
   variable Param

   set Param(Size) 23
   set Param(Zone) UTC           ;#Default time zone (UTC,HADT,HAST,AKDT,AKST,PDT,PST,MDT,MST,CDT,CST,EDT,EST,ADT,AST,NDT,NST)

   set Param(Zones) {
      { { UTC UTC }    { "Temps universel coordonné" "Coordinated Universal Time" }                    0 }
      { { -  - }       -                                                                               - }  
      { { HAHA HADT }  { "Heure Avancée d'Hawaï-Aléoutiennes"     "Hawaii-Aleutian Daylight Time" }   -9 }
      { { HAA AKDT }   { "Heure Avancée de l'Alaska"              "Alaska Daylight Time" }            -8 }
      { { HAP PDT }    { "Heure Avancée du Pacifique"             "Pacific Daylight Time" }           -7 }
      { { HAR MDT }    { "Heure Avancée des Rocheuses"            "Mountain Daylight Time" }          -6 }
      { { HAC CDT }    { "Heure Avancée du Centre"                "Central Daylight Time" }           -5 }
      { { HAE EDT }    { "Heure Avancée de l'Est"                 "Eastern Daylight Time" }           -4 }
      { { HAA ADT }    { "Heure Avancée de l'Atlantique"          "Atlantic Daylight Time"  }         -3 }
      { { HAT NDT }    { "Heure Avancée de Terre-Neuve"           "Newfoundland Daylight Time" }      -2.5 }
      { { HAEG WGST }  { "Heure Avancée de l'Ouest du Groenland"  "Western Greenland Summer Time" }   -2 }
      { { HAOG EGST }  { "Heure Avancée de l'Est du Groenland"    "Eastern Greenland Summer Time" }   -1 } 
      { { - - }        -                                                                               - }  
      { { HNHA HAST }  { "Heure Normale d'Hawaï-Aléoutiennes"    "Hawaii-Aleutian Standard Time" }   -10 } 
      { { HNA AKST }   { "Heure Normale de l'Alaska"             "Alaska Standard Time" }             -9 } 
      { { HNP PST }    { "Heure Normale du Pacifique"            "Pacific Standard Time"  }           -8 } 
      { { HNR MST }    { "Heure Normale des Rocheuses"           "Mountain Standard Time"  }          -7 } 
      { { HNC CST }    { "Heure Normale du Centre"               "Central Standard Time" }            -6 } 
      { { HNE EST }    { "Heure Normale de l'Est"                "Eastern Standard Time" }            -5 } 
      { { HNA AST }    { "Heure Normale de l'Atlantique"         "Atlantic Standard Time"  }          -4 } 
      { { HNT NST }    { "Heure Normale de Terre-Neuve"          "Newfoundland Standard Time " }      -3.5 } 
      { { HNEG WGT }   { "Heure Normale de l'Ouest du Groenland" "West Greenland Time" }              -3 } 
      { { HNOG EGT }   { "Heure Normale de l'Est du Groenland"   "East Greenland Time" }              -1 } }
}

proc CVClock::Create { Frame X Y } {
   global GDefs
   variable Data
   variable Param

   set x0 [expr $X-27]
   set y0 [expr $Y-27]
   set x1 [expr $X+27]
   set y1 [expr $Y+27]

   set canvas $Frame.page.canvas

   $canvas create line $X $Y $X $Y -width 0 -fill white -tag "CVCLOCK CVCLOCKLOC"

   $canvas create arc $x0 $y0 $x1 $y1 -outline "" -fill #DEDEDE -tag "CVCLOCK CVCLOCKALPHA"  -start 90 -extent 359 -transparency 75
   $canvas create arc [expr $x0+2] [expr $y0+2] [expr $x1-2] [expr $y1-2] -outline "" -fill #FFFF00 -transparency 75 -width 3 \
      -start 90 -extent 0 -style pieslice -tag "CVCLOCK CVCLOCKALPHA CVCLOCKTOTAL"

   $canvas create image $X $Y -image CLOCKFRAME -tag CVCLOCK

   $canvas create image $X [expr $Y+67] -image CLOCKDATE -tag "CVCLOCK CVCLOCKDATEFR"
   $canvas create image $X [expr $Y+45] -image CLOCKTIME -tag "CVCLOCK CVCLOCKTIMEFR"
   $canvas create text $X [expr $Y+67] -font XFont12 -tag "CVCLOCK CVCLOCKDATE"
   $canvas create text $X [expr $Y+45] -font XFont12 -tag "CVCLOCK CVCLOCKTIME"

   $canvas create line $X $Y $X $y0  -fill black -width 3 -tag "CVCLOCK CVCLOCKMINUTE"
   $canvas create line $X $Y $X $y0  -fill black -width 3 -tag "CVCLOCK CVCLOCKHOUR"
   eval $canvas create rectangle [$canvas bbox CVCLOCK] -tag \"CVCLOCK CVCLOCKBBOX VPINTRUDE\" -width 0

   set Data(Sec$Frame)  0
   set Data(Zone$Frame) 0

   #----- Set zone to default
   if { [set zone [lsearch -index { 0 0 } $Param(Zones) $Param(Zone)]]!=-1 || [set zone [lsearch -index { 0 1 } $Param(Zones) $Param(Zone)]]!=-1 } {
       set Data(Zone$Frame) $zone
   }

   if { ![winfo exists $canvas.cvclock] } {
      menubutton $canvas.cvclock -bg $GDefs(ColorFrame) -bitmap @$GDefs(Dir)/share/bitmap/cvmenu.xbm -cursor hand1 -bd 1 \
         -relief raised -menu $canvas.cvclock.menu
      menu $canvas.cvclock.menu -tearoff 0 -bg $GDefs(ColorFrame)
      set z 0
      foreach zone $Param(Zones) {
         if { [lindex $zone end]=="-" } {
            $canvas.cvclock.menu add separator
         } else {
            $canvas.cvclock.menu add radiobutton -label "[lindex [lindex $zone 1] $GDefs(Lang)] ([lindex [lindex $zone 0] $GDefs(Lang)])" -variable CVClock::Data(Zone$Frame) -value $z \
               -command "CVClock::Time $Frame \$CVClock::Data(Sec$Frame) -1"
         }
         incr z
      }
   }

   $canvas create window [expr $X-43] [expr $Y+50] -window $canvas.cvclock -anchor se -tags "CVCLOCK OPCVCLOCK NOPRINT"

   Shape::BindAllMove $canvas CVCLOCK
   Shape::BindWidget  $canvas CVCLOCK
   
   Page::MaskItem $Frame
}

#----------------------------------------------------------------------------
# Nom      : <CVClock::Update>
# Creation : Avril 2004 - J.P. Gauthier - CMC/CMOE
#
# But      : Mettre a jour l'heure de l'Horloge
#
# Parametres :
#  <Frame>   : Identificateur de page
#  <Data>    : Donnee
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc CVClock::Update { Frame Data } {
   global GDefs

   if { ![CVClock::Exist $Frame] } {
      return
   }

   set sec 0
   
   if { $Data!="" } {
      if { [fstdfield is $Data] } {
         set sec [fstdstamp toseconds [fstdfield define $Data -DATEV]]
      } elseif { [gribfield is $Data] } {
         set sec [gribfield define $Data -DATEV]
      } elseif { [observation is $Data] } {
         set sec [observation define $Data -DATE]
      }
   }
   CVClock::Time $Frame $sec -1
}

#----------------------------------------------------------------------------
# Nom      : <CVClock::Destroy>
# Creation : Decembre 2000 - J.P. Gauthier - CMC/CMOE
#
# But      : Supprimer l'horloge
#
# Parametres :
#  <Frame>   : Identificateur de page
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc CVClock::Destroy { Frame } {

   $Frame.page.canvas delete CVCLOCK
}

#----------------------------------------------------------------------------
# Nom      : <CVClock::Exist>
# Creation : Juillet 2001 - J.P. Gauthier - CMC/CMOE
#
# But      : Verifier l'existence de l'horloge
#
# Parametres :
#  <Frame>   : Identificateur de page
#
# Retour:
#  <Exist>   : Booleen
#
# Remarques :
#
#----------------------------------------------------------------------------

proc CVClock::Exist { Frame } {

   return [llength [$Frame.page.canvas find withtag CVCLOCK]]
}

#----------------------------------------------------------------------------
# Nom      : <CVClock::Time>
# Creation : Decembre 2000 - J.P. Gauthier - CMC/CMOE
#
# But      : Mettre a jour l'heure et la date de l'horloge
#
# Parametres :
#  <Frame>   : Identificateur de page
#  <Sec>     : Date en secondes
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc CVClock::Time { Frame Sec Total } {
   global GDefs
   variable Param
   variable Data

   set canvas $Frame.page.canvas

   if { ![llength [$canvas find withtag CVCLOCK]] } {
      return
   }

   #----- Positionnes le quartier total

   if { $Total>-1 } {
      if { $Total>=100 } {
         set extent -359
      } else {
         set extent [expr -$Total*3.6]
      }
      $canvas itemconf CVCLOCKTOTAL -extent $extent
   }

   if { $Sec!="-1" } {

      #----- Ajustement pour le timezone

      set Data(Sec$Frame) $Sec
      set Sec [expr int($Sec+[lindex [lindex $Param(Zones) $Data(Zone$Frame)] 2]*3600)]

      set hour [clock format $Sec -format "%k" -gmt true]
      set min  [clock format $Sec -format "%M" -gmt true]
      set jour [DateStuff::StringDay   [clock format $Sec -format "%w" -gmt true] $GDefs(Lang)]
      set mois [DateStuff::StringMonth [clock format $Sec -format "%m" -gmt true] $GDefs(Lang)]

      #----- Positionner l'aiguille des heures

      set theta [expr (($hour>=12?$hour-12:$hour)-3)*0.52359876]
      set co [$canvas coords CVCLOCKHOUR]
      set x0 [lindex $co 0]
      set y0 [lindex $co 1]

      set x [expr $x0+($Param(Size)-6)*cos($theta)]
      set y [expr $y0+($Param(Size)-6)*sin($theta)]

      $canvas coords CVCLOCKHOUR $x0 $y0 $x $y

      #----- Positionner l'aiguille des minutes

      set m [string trimleft $min 0]
      if { $m=="" } {
         set m 0
      }

      set theta [expr ($m-15)*0.10471976]
      set co [$canvas coords CVCLOCKMINUTE]
      set x0 [lindex $co 0]
      set y0 [lindex $co 1]

      set x [expr $x0+$Param(Size)*cos($theta)]
      set y [expr $y0+$Param(Size)*sin($theta)]

      $canvas coords CVCLOCKMINUTE $x0 $y0  $x $y

      $canvas itemconf CVCLOCKDATE -text [clock format $Sec -format "$jour %d $mois %Y" -gmt true]
      $canvas itemconf CVCLOCKTIME -text "${hour}:${min} [lindex [lindex [lindex $Param(Zones) $Data(Zone$Frame)] 0] $GDefs(Lang)]"
   }

   $canvas raise CVCLOCK
}

#------------------------------------------------------------------------------
# Nom      : <CVClock::Write>
# Creation : Novembre 2003 - J.P. Gauthier - CMC/CMOE -
#
# But     : Engeristrer les parametres du CVClock dans un fichier Layout
#
# Parametres :
#   <Frame>  : Identificateur de Page
#   <File>   : Identificateur de Fichier
#
# Remarques :
#
#-------------------------------------------------------------------------------

proc CVClock::Write { Frame File } {
   variable Data

   puts $File ""
   puts $File "   #----- Affichage de l'horloge"
   set c [$Frame.page.canvas coords CVCLOCKLOC]
   puts $File "   CVClock::Create \$Frame  [lindex $c 0]  [lindex $c 1]"
}

#----------------------------------------------------------------------------
# Nom      : <CVGeoLegend::Create>
# Creation : Janvier 2001 - J.P. Gauthier - CMC/CMOE
#
# But      : Dessine une legende des items
#
# Parametres :
#   <Frame>  : Identificateur de Page
#   <X>      : Position en X
#   <Y>      : Position en Y
#   <List>   : Liste des items { { Width Color Text } ... { ... } }
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

namespace eval CVGeoLegend { }

proc CVGeoLegend::Create { Frame X Y List } {
   global GDefs
   variable Data

   set Data(List) $List

   set canvas $Frame.page.canvas
   $canvas create line $X $Y $X $Y -width 0 -fill white -tag "CVLEGEND CVLEGENDLOC"

   set y [expr int($Y+10)]
   set x [expr int($X+5)]

   foreach item $List {

      $canvas create line $x $y [expr $x+20] $y -width [lindex $item 0] -fill [lindex $item 1] -tag "CVLEGEND CVLEGENDDESC"
      $canvas create text [expr $x+25] $y -text "[lindex $item 2]" -font XFont10 -anchor w -tag "CVLEGEND CVLEGENDDESC"

      incr y 10
   }

   set coord [$canvas bbox CVLEGENDDESC]

   $canvas create rectangle $X $Y [expr [lindex $coord 2]+5] [expr [lindex $coord 3]+5] \
      -outline black -fill white -width 1 -tag "CVLEGEND CVLEGENDFRAME"
   $canvas lower CVLEGENDFRAME CVLEGENDDESC

   Shape::BindAllMove $canvas CVLEGEND
}

#----------------------------------------------------------------------------
# Nom      : <CVGeoLegend::Update>
# Creation : Janvier 2001 - J.P. Gauthier - CMC/CMOE
#
# But      : Mettre a jour la legende des items
#
# Parametres :
#   <Frame>  : Identificateur de Page
#   <List>   : Liste des items { { Width Color Text } ... { ... } }
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc CVGeoLegend::Update { Frame List } {
   global GDefs
   variable Data

   set co [$Frame.page.canvas coords CVLEGENDLOC]
   set x [lindex $co 0]
   set y [lindex $co 1]

   $Frame.page.canvas delete CVLEGEND
   CVGeoLegend::Create $Frame $x $y $List
}

#----------------------------------------------------------------------------
# Nom      : <CVGeoLegend::Destroy>
# Creation : Janvier 2001 - J.P. Gauthier - CMC/CMOE
#
# But      : Supprimet la legende
#
# Parametres :
#   <Frame>  : Identificateur de Page
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc CVGeoLegend::Destroy { Frame } {

   $Frame.page.canvas delete CVLEGEND
}

#------------------------------------------------------------------------------
# Nom      : <CVGeoLegend::Write>
# Creation : Novembre 2003 - J.P. Gauthier - CMC/CMOE -
#
# But     : Engeristrer les parametres du CVGeoLegend dans un fichier Layout
#
# Parametres :
#   <Frame>  : Identificateur de Page
#   <File>   : Identificateur de Fichier
#
# Remarques :
#
#-------------------------------------------------------------------------------

proc CVGeoLegend::Write { Frame File } {
   variable Data

   puts $File ""
   puts $File "   #----- Affichage de la legende"
   set c [$Frame.page.canvas coords CVLEGENDLOC]
   puts $File "   CVGeoLegend::Create \$Frame [lindex $c 0] [lindex $c 1] \[list $Data(List)\]"
}

#----------------------------------------------------------------------------
# Nom      : <CVScale::Create>
# Creation : Janvier 2001 - J.P. Gauthier - CMC/CMOE
#
# But      : Dessine une echelle de distance
#
# Parametres :
#  <Frame>   : Identificateur de page
#  <X>       : Position en X
#  <Y>       : Position en Y
#  <Size>    : Dimension (En pixel)
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------
namespace eval CVScale { } {
   variable Data
   variable Lbl

   set Data(BG)    False
   set Data(Scale) 0

   set Lbl(BG) { "Arrière opaque" "White background" }
}

proc CVScale::Create { Frame X Y Size } {
   global GDefs
   variable Data
   variable Lbl

   set canvas $Frame.page.canvas
   set Data(Size)   $Size

   set x0 [expr $X-$Size/2]
   set y0 [expr $Y]
   set x1 [expr $X+$Size/2]
   set y1 [expr $Y+10]

   $canvas create rectangle 0 0 0 0 -width 0 -tag "CVSBBOX CVSCALE VPINTRUDE"
   $canvas create line $X $Y $X $Y -width 0 -fill white -tag "CVSCLOC CVSCALE"
   $canvas create text $x0 $Y -text "" -font XFont12 -anchor sw -tag "CVSCTXT CVSCALE"
   $canvas create text $x1 $Y -text "" -font XFont12 -anchor se -tag "CVSCUNI CVSCALE"
   $canvas create text $x0 [expr $y1+3] -text "0" -font XFont12 -tag "CVSCT00 CVSCALE" -anchor nw

   #----- 1/10

   foreach i { 0 1 2 3 4 5 6 7 8 9 } {
      $canvas create rectangle $x0 $y0 $x0 $y0 -outline black -fill white -width 1 -tag "CVSC0$i CVSCALE"
   }

   #----- 10

   foreach i { 0 1 2 3 } col { black white black white } {
      $canvas create rectangle $x0 $y0 $x0 $y0 -outline black -fill $col -width 1 -tag "CVSC1$i CVSCALE"
      $canvas create text $x0 $y0 -text "" -font XFont12 -tag "CVSCT1$i CVSCALE" -anchor nw
   }

   if { ![winfo exists $canvas.cvscale] } {
      menubutton $canvas.cvscale -bg $GDefs(ColorFrame) -bitmap @$GDefs(Dir)/share/bitmap/cvmenu.xbm -cursor hand1 -bd 1 \
         -relief raised -menu $canvas.cvscale.menu
      menu $canvas.cvscale.menu -tearoff 0 -bg $GDefs(ColorFrame)
      foreach size {  10000000 1000000 500000 250000 100000 25000 24000 20000 10000 } {
         $canvas.cvscale.menu add radiobutton -label "1:${size}" -variable CVScale::Data(Scale) -value ${size}.0 \
            -command "CVScale::Set $Page::Data(Frame)" -indicatoron false
      }
      $canvas.cvscale.menu add separator
      $canvas.cvscale.menu add checkbutton -label [lindex $Lbl(BG) $GDefs(Lang)] -variable CVScale::Data(BG) -onvalue True -offvalue False \
            -command { CVScale::Update $Page::Data(Frame) $Page::Data(VP) }
    }

   $canvas create window [expr $x0+10] [expr $y0-1] -window $canvas.cvscale -anchor ne -tags "CVSCALE OPCVSCALE NOPRINT"

   Shape::BindAllMove $canvas CVSCALE
   Shape::BindWidget  $canvas CVSCALE

   Page::MaskItem $Frame

   CVScale::Update $Frame $Page::Data(VP)
}

#----------------------------------------------------------------------------
# Nom      : <CVScale::Destroy>
# Creation : Janvier 2001 - J.P. Gauthier - CMC/CMOE
#
# But      : Supprimer l'echelle de distance
#
# Parametres :
#  <Frame>   : Identificateur de page
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc CVScale::Destroy { Frame } {

   $Frame.page.canvas delete CVSCALE
   destroy $Frame.page.canvas.cvscale $Frame.page.canvas.cvscale.menu
}

#----------------------------------------------------------------------------
# Nom      : <CVScale::Set>
# Creation : Mai 2003 - J.P. Gauthier - CMC/CMOE
#
# But      : Appliquer une echelle specifique
#
# Parametres :
#  <Frame>   : Identificateur de Page
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc CVScale::Set { Frame } {
   variable Data

   set dxy    [expr 2.8e-4*double($Data(Scale))]
   set factor [$Page::Data(VP) -distpix $dxy]

   ProjCam::ZoomIn $Frame $Frame $Page::Data(VP) $factor
}

#------------------------------------------------------------------------------
# Nom      : <CVScale::Write>
# Creation : Novembre 2003 - J.P. Gauthier - CMC/CMOE -
#
# But     : Engeristrer les parametres du CVScale dans un fichier Layout
#
# Parametres :
#   <Frame>  : Identificateur de Page
#   <File>   : Identificateur de Fichier
#
# Remarques :
#
#-------------------------------------------------------------------------------

proc CVScale::Write { Frame File } {
   variable Data

   puts $File ""
   puts $File "   #----- Affichage de l'echelle"
   set c [$Frame.page.canvas coords CVSCLOC]
   puts $File "   CVScale::Create \$Frame [lindex $c 0] [lindex $c 1] $Data(Size)"
}

#----------------------------------------------------------------------------
# Nom      : <CVScale::Update>
# Creation : Janvier 2001 - J.P. Gauthier - CMC/CMOE
#
# But      : Mettre a jour l'echelle de distance
#
# Parametres :
#  <Frame>   : Identificateur de page
#  <VP>      : Identificateur du viewport
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc CVScale::Update { Frame VP } {
   global GDefs
   variable Data
   variable Lbl

   if { $VP=="" } {
      return
   }

   #----- On calcule la distance pour 1 pixel
   set dxy [$VP -distpix]
   if { $dxy<1e-30 || $dxy>1e30} {
      return
   }

   #----- Calcul du plus proche gradient
   set o "1e[expr int(log10($dxy))+1]"

   #----- Determiner l'amplitude
   set xy [expr $o/$dxy]

   set m  [expr int(($Data(Size)/5.0)/$xy)]
   set m  [expr $m<=0?1:$m]
   set xy [expr $xy*$m]

   if { $o > 100 } {
      set u 1e3
   } elseif { $o > 1e0 } {
      set u 1e0
   } elseif { $o > 1e-2 } {
      set u 1e-2
   } elseif { $o > 1e-3 } {
      set u 1e-3
   } elseif { $o > 1e-6 } {
      set u 1e-6
   } elseif { $o > 1e-9 } {
      set u 1e-9
   } elseif { $o > 1e-12 } {
      set u 1e-12
   } elseif { $o > 1e-15 } {
      set u 1e-15
   } elseif { $o > 1e-18 } {
      set u 1e-18
   } elseif { $o > 1e-21 } {
      set u 1e-21
   } else {
      set u 1e-24
   }

   #----- Coordonnees courantes
   set canvas $Frame.page.canvas
   set co [$canvas coords CVSC00]
   set x0 [lindex $co 0]
   set y0 [lindex $co 1]
   set y1 [expr $y0+10]

   #----- Update des 1/10
   foreach i { 0 1 2 3 4 5 6 7 8 9 } {
      set x1 [expr $x0+$xy/10.0]
      $canvas coords CVSC0$i $x0 $y0 $x1 $y1
      set x0 $x1
   }

   #----- Update des 10
   foreach i { 0 1 2 3 } {
      set x1 [expr $x0+$xy]
      $canvas coords CVSC1$i $x0 $y0 $x1 $y1
      $canvas coords CVSCT1$i $x0 [expr $y1+2]
      $canvas itemconfigure CVSCT1$i -text "[expr wide(((1+$i)*$o/$u))*$m]"
      set x0 $x1
   }

   #----- Update de l'echelle (1:???)
   set Data(Scale) [expr wide($dxy/2.8e-4)]

   if ($Data(Scale)==0) {
      set Data(Scale) [expr wide(2.8e-4/$dxy)]
      $canvas itemconfigure CVSCTXT -text "$Data(Scale):1"
   } else {
      $canvas itemconfigure CVSCTXT -text "1:$Data(Scale)"
   }
   $canvas coords CVSCUNI $x1 $y0

   if { $u==1e0 } {
      $canvas itemconfigure CVSCUNI -text "[lindex $Convert::Lbl(DistU) $GDefs(Lang)]"
   } else {
      $canvas itemconfigure CVSCUNI -text "$Convert::Lbl($u)[lindex $Convert::Lbl(DistL) $GDefs(Lang)]"
   }

   $canvas coords CVSBBOX $x0 $y0 $x0 $y0
   set bbox [$canvas bbox CVSCALE]
   $canvas coords CVSBBOX [expr [lindex $bbox 0]-2] [expr [lindex $bbox 1]-4] [expr [lindex $bbox 2]+12] [expr [lindex $bbox 3]+2] 
   if { $Data(BG) } {
      $canvas itemconfigure CVSBBOX -fill white -outline black -width 1
   } else {
      $canvas itemconfigure CVSBBOX -fill "" -outline "" -width 0
   }  
   
   $canvas raise CVSCALE
}

namespace eval CVText { } {
   variable Lbl
   variable Param
   variable Data

   set Param(Width)   500
   set Param(Height)  200
   set Param(BG)      white
   set Param(FG)      black
   set Param(Justify) left
   
   font create CVTEXTFONT -family courier -weight bold -size -12

   set Data(TagNo)   0
   
   set Lbl(Update)        { "Mise-à-jour automatique" "Auto update" }
   set Lbl(JustifyLeft)   { "Justification à gauche" "Justify left" }
   set Lbl(JustifyCenter) { "Justification au centre" "Justify center" }
   set Lbl(JustifyRight)  { "Justification à droitre" "Justify right" }
   set Lbl(Font)          { "Police ..." "Font ..." }
   set Lbl(FG)            { "Couleur de la police ..." "Font color ..." }
   set Lbl(BG)            { "Couleur de fond ..." "Background color ..." }
}

#----------------------------------------------------------------------------
# Nom      : <CVText::Create>
# Creation : Janvier 1014 - J.P. Gauthier - CMC/CMOE
#
# But      : Create a CVTEXT textbox in a canvas
#
# Parametres :
#  <Frame>   : Identificateur de page
#  <X0>      : Coordonee X0
#  <Y0>      : Coordonee Y0
#  <X1>      : Coordonee X1
#  <Y1>      : Coordonee Y1
#  <Text>    : Initial text
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc CVText::Create { Frame { X0 0 } { Y0 0 } { Width 0 } { Height 0 } { Text "" } } {
   global GDefs
   variable Param
   variable Data
   variable Lbl   
   
   set canvas $Frame.page.canvas
   set tag    CVTEXT$Data(TagNo)
   set x1     [expr $X0+$Width]
   set y1     [expr $Y0+$Height]
    
   #----- Set default parameters based on last CVTEXTBOX
   if { $Width==0 } {
      set cvt [$canvas coords [lindex [$canvas find withtag CVTEXTBOX] end]]
      if { [llength $cvt] } {
         set X0 [expr [lindex $cvt 0]+10]
         set Y0 [expr [lindex $cvt 1]+10]
         set x1 [expr [lindex $cvt 2]+10]
         set y1 [expr [lindex $cvt 3]+10]
      } else {
         set X0 0
         set Y0 0
         set x1 [expr 0+$Param(Width)]
         set y1 [expr 0+$Param(Height)]
      }
   }
  
   set Data(Update$tag)  1
   set Data(Justify$tag) $Param(Justify)
   set Data(BG$tag)      $Param(BG)
   set Data(FG$tag)      $Param(FG)
   
   $canvas create rectangle $X0 $Y0 $x1 $y1 -width 1 -tags "$tag CVTEXTBOX$tag CVTEXTBOX VPINTRUDE" -fill $Param(BG)
   $canvas create text [expr $X0+5] [expr $Y0+5] -anchor nw -font CVTEXTFONT -tags "$tag CVTEXTEDIT$tag CVTEXT" \
      -text $Text -width [expr $x1-$X0-10] -justify $Data(Justify$tag) -fill $Param(FG)
   $canvas icursor CVTEXTEDIT$tag 0
   
   $canvas bind CVTEXTBOX$tag <Enter> "%W configure -cursor xterm ; focus $canvas; $canvas focus CVTEXTEDIT$tag"
   $canvas bind CVTEXTBOX$tag <Leave> "%W configure -cursor left_ptr"
  
   Shape::BindScale   $canvas $tag "CVText::Scale $canvas $tag"
   Shape::BindMove    $canvas $tag "CVText::Move $canvas $tag"
   Shape::BindDestroy $canvas $tag "CVText::Destroy $Frame"
   Shape::BindWidget  $canvas $tag

   catch {
      menubutton $canvas.bo$tag -bg $GDefs(ColorFrame) -bitmap @$GDefs(Dir)/share/bitmap/cvmenu.xbm -cursor hand1 -bd 1 -relief raised \
         -menu $canvas.bo$tag.menu
      menu $canvas.bo$tag.menu -bg $GDefs(ColorFrame)
         $canvas.bo$tag.menu add radiobutton -label [lindex $Lbl(JustifyLeft) $GDefs(Lang)] -variable CVText::Data(Justify$tag) -value left \
            -command "$canvas itemconfigure CVTEXTEDIT$tag -justify \$CVText::Data(Justify$tag)"
         $canvas.bo$tag.menu add radiobutton -label [lindex $Lbl(JustifyCenter) $GDefs(Lang)] -variable CVText::Data(Justify$tag) -value center \
            -command "$canvas itemconfigure CVTEXTEDIT$tag -justify \$CVText::Data(Justify$tag)"
         $canvas.bo$tag.menu add radiobutton -label [lindex $Lbl(JustifyRight) $GDefs(Lang)] -variable CVText::Data(Justify$tag) -value right \
            -command "$canvas itemconfigure CVTEXTEDIT$tag -justify \$CVText::Data(Justify$tag)"
         $canvas.bo$tag.menu add separator
         $canvas.bo$tag.menu add command -label [lindex $Lbl(Font) $GDefs(Lang)] -command "FontBox::Create . {} CVTEXTFONT"
         $canvas.bo$tag.menu add command -label [lindex $Lbl(FG) $GDefs(Lang)] -command "ColorBox::Create . CVText::Data(FG$tag); $canvas itemconfigure CVTEXTEDIT$tag -fill \$CVText::Data(FG$tag)"
         $canvas.bo$tag.menu add command -label [lindex $Lbl(BG) $GDefs(Lang)] -command "ColorBox::Create . CVText::Data(BG$tag); $canvas itemconfigure CVTEXTBOX$tag -fill \$CVText::Data(BG$tag)"
         $canvas.bo$tag.menu add separator
         $canvas.bo$tag.menu add checkbutton -label [lindex $Lbl(Update) $GDefs(Lang)] -variable CVText::Data(Update$tag) -onvalue 1 -offvalue 0
   }      
   $canvas create window [expr $x1-22] $y1 -window $canvas.bo$tag -anchor se -tags "BO$tag NOPRINT"
  
   incr Data(TagNo)
   
   Page::MaskItem $Frame
   
   return $tag
}

proc CVText::Update { Frame Tag Text } {
   variable Data

   set tag CVTEXTEDIT$Tag

   if { $Data(Update$Tag) } {
       $Frame.page.canvas itemconfigure $tag -text $Text
   }
}

#------------------------------------------------------------------------------
# Nom      : <CVText::Destroy>
# Creation : Janvier 2014 - J.P. Gauthier - CMC/CMOE -
#
# But     : Supprimer un Colorbar
#
# Parametres :
#   <Frame>  : Identificateur de Page
#   <VP>     : Identificateur du Viewport
#   <No>     : Numero de champs
#
# Retour     :
#
# Remarques :
#
#-------------------------------------------------------------------------------

proc CVText::Destroy { Frame Tag } {
   variable Data

   Shape::UnBind $Frame.page.canvas $Tag
   $Frame.page.canvas delete $Tag
}

#------------------------------------------------------------------------------
# Nom      : <CVText::Write>
# Creation : Janvier 2014 - J.P. Gauthier - CMC/CMOE -
#
# But     : Engeristrer les parametres des CVText dans un fichier Layout
#
# Parametres :
#   <Frame>  : Identificateur de Page
#   <File>   : Identificateur de Fichier
#
# Remarques :
#
#-------------------------------------------------------------------------------

proc CVText::Write { Frame File } {
   variable Data

   for { set n 0 } { $n < $Data(TagNo) } { incr n } {
      set tag CVTEXT$n
      set cvt [$Frame.page.canvas find withtag CVTEXTBOX$tag]
      
      if { $cvt!="" } {
         set c [$Frame.page.canvas coords CVTEXTBOX$tag]
         set t [$Frame.page.canvas itemcget CVTEXTEDIT$tag -text]
         
         puts $File "\n   catch { font create CVTEXTFONT }"
         puts $File "   font configure CVTEXTFONT -family [font configure CVTEXTFONT -family] -weight [font configure CVTEXTFONT -weight] -size [font configure CVTEXTFONT -size]\
                  -slant [font configure CVTEXTFONT -slant] -underline [font configure CVTEXTFONT -underline] -overstrike [font configure CVTEXTFONT -overstrike]"
                  
         puts $File "   set CVText::Param(Justify) $Data(Justify$tag)"
         puts $File "   set CVText::Param(BG)      $Data(BG$tag)"
         puts $File "   set CVText::Param(FG)      $Data(FG$tag)"
         puts $File "   CVText::Create $Frame [lindex $c 0] [lindex $c 1] [expr [lindex $c 2]-[lindex $c 0]] [expr [lindex $c 3]-[lindex $c 1]] \"$t\""
      }
   }
}

#------------------------------------------------------------------------------
# Nom      : <CVText::Move>
# Creation : Janvier 2014 - J.P. Gauthier - CMC/CMOE -
#
# But     : Enregistrer le changement de position de la colorbar
#
# Parametres :
#   <Canvas> : Path du canvas
#   <Tag>    : Identificateur de la colorbar
#
# Retour:
#
# Remarques :
#
#-------------------------------------------------------------------------------

proc CVText::Move { Canvas Tag } {
   variable Data

   set Data(Full$Tag) False
}

#------------------------------------------------------------------------------
# Nom      : <CVText::Scale>
# Creation : Janvier 2014 - J.P. Gauthier - CMC/CMOE -
#
# But     : Changement de dimension de la colorbar
#
# Parametres :
#   <Canvas> : Path du canvas
#   <Tag>    : Identificateur de la colorbar
#   <X>      : Dimension en X
#   <Y>      : Dimension en Y
#
# Retour:
#
# Remarques :
#
#-------------------------------------------------------------------------------

proc CVText::Scale { Canvas Tag X Y } {
   variable Data

   set c [$Canvas coords CVTEXTBOX$Tag]
   set x [lindex $c 0]
   set y [lindex $c 1]
   set w [expr $X-$x]
   set h [expr $Y-$y]

   if { $w>25 && $h>25 } {
      $Canvas coords CVTEXTBOX$Tag $x $y $X $Y
      $Canvas itemconfigure CVTEXTEDIT$Tag -width [expr $X-$x-10]
      set Data(Full$Tag) False
      return True
   } else {
      return False
   }
}

#----------------------------------------------------------------------------
# Nom      : <CVText::Init>
# Creation : Novembre 2002 - J.P. Gauthier - CMC/CMOE
#
# But      : Initialiser les bindings de texte dynamique dans un canvas
#
# Parametres :
#  <Canvas>  : Identificateur du canvas
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc CVText::Init { Canvas } {

   $Canvas bind CVTEXT <Enter>            { %W configure -cursor xterm }
   $Canvas bind CVTEXT <Leave>            { %W configure -cursor left_ptr }
   $Canvas bind CVTEXT <Button-1>         { CVText::Focus     %W [%W canvasx %x] [%W canvasy %y] ; CVText::Hit %W [%W canvasx %x] [%W canvasy %y]}
   $Canvas bind CVTEXT <Double-Button-1>  { CVText::Select    %W }
   $Canvas bind CVTEXT <B1-Motion>        { CVText::Drag      %W [%W canvasx %x] [%W canvasy %y] }

   $Canvas bind CVTEXT <<Cut>>            { CVText::Copy      %W ; CVText::Delete %W }
   $Canvas bind CVTEXT <<Copy>>           { CVText::Copy      %W }
   $Canvas bind CVTEXT <<Paste>>          { CVText::Paste     %W }

   $Canvas bind CVTEXT <Delete>           { CVText::Delete    %W }
   $Canvas bind CVTEXT <Control-d>        { CVText::Delete    %W }
   $Canvas bind CVTEXT <Control-h>        { CVText::BackSpace %W }
   $Canvas bind CVTEXT <BackSpace>        { CVText::BackSpace %W }
   $Canvas bind CVTEXT <Control-Delete>   { CVText::Erase     %W }
   $Canvas bind CVTEXT <Key-Right>        { CVText::Go        %W 1 }
   $Canvas bind CVTEXT <Control-f>        { CVText::Go        %W 1 }
   $Canvas bind CVTEXT <Key-Left>         { CVText::Go        %W -1 }
   $Canvas bind CVTEXT <Control-b>        { CVText::Go        %W -1 }
   $Canvas bind CVTEXT <Key-Home>         { CVText::GoEnd     %W 0 }
   $Canvas bind CVTEXT <Key-End>          { CVText::GoEnd     %W end }
   $Canvas bind CVTEXT <Any-Key>          { CVText::Insert    %W %A %K }
}

#----------------------------------------------------------------------------
# Nom      : <CVText::Focus>
# Creation : Novembre 2002 - J.P. Gauthier - CMC/CMOE
#
# But      : Mettre le focus sur l'item sous le curseur dans le canvas
#
# Parametres :
#  <Canvas>  : Identificateur du canvas
#  <X>       : Coordonnee en X
#  <Y>       : Coordonnee en Y
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc CVText::Focus { Canvas X Y } {

   focus $Canvas
   set ids [$Canvas find overlapping $X $Y $X $Y]

   foreach id $ids {
      if { [lsearch -exact [$Canvas gettags $id] CVTEXT]!=-1 } {
         break
      }
   }
   $Canvas focus $id

}

#----------------------------------------------------------------------------
# Nom      : <CVText::Copy>
# Creation : Novembre 2002 - J.P. Gauthier - CMC/CMOE
#
# But      : Copier le text selectionne dans le clipboard
#
# Parametres :
#  <Canvas>  : Identificateur du canvas
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc CVText::Copy { Canvas } {

   set item  [$Canvas select item]
   set focus [$Canvas focus]

   if { $item!={} } {
      clipboard clear
      clipboard append [string range [$Canvas itemcget $item -text] [$Canvas index $item sel.first] [$Canvas index $item sel.last]]
   } elseif { $focus != {} } {
      clipboard clear
      clipboard append [$Canvas itemcget $focus -text]
   }
}

#----------------------------------------------------------------------------
# Nom      : <CVText::BackSpace>
# Creation : Novembre 2002 - J.P. Gauthier - CMC/CMOE
#
# But      : Fonction de backspace dans un item text du canvas
#
# Parametres :
#  <Canvas>  : Identificateur du canvas
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc CVText::BackSpace { Canvas } {

   set item  [$Canvas select item]
   set focus [$Canvas focus]

   if { $item != {} } {
      $Canvas dchars $item sel.first sel.last
   } elseif { $focus!={} } {
      $Canvas icursor $focus [expr [$Canvas index $focus insert]-1]
      $Canvas dchars $focus insert
   }
}

#----------------------------------------------------------------------------
# Nom      : <CVText::Delete>
# Creation : Novembre 2002 - J.P. Gauthier - CMC/CMOE
#
# But      : Fonction delete dans un item text du canvas
#
# Parametres :
#  <Canvas>  : Identificateur du canvas
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc CVText::Delete { Canvas } {

   set item  [$Canvas select item]
   set focus [$Canvas focus]

   if { $item != {} } {
      $Canvas dchars $item sel.first sel.last
   } elseif { $focus != {} } {
      $Canvas dchars $focus insert
   }
}

#----------------------------------------------------------------------------
# Nom      : <CVText::Drag>
# Creation : Novembre 2002 - J.P. Gauthier - CMC/CMOE
#
# But      : Effectuer une selection a partir du point initial
#
# Parametres :
#  <Canvas>  : Identificateur du canvas
#  <X>       : Coordonnee en X
#  <Y>       : Coordonnee en Y
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc CVText::Drag { Canvas X Y } {

   $Canvas select to current @$X,$Y
   $Canvas icursor current @$X,$Y
}

#----------------------------------------------------------------------------
# Nom      : <CVText::Erase>
# Creation : Novembre 2002 - J.P. Gauthier - CMC/CMOE
#
# But      : Fonction de suppression de tout le text
#
# Parametres :
#  <Canvas>  : Identificateur du canvas
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc CVText::Erase { Canvas } {
   $Canvas delete [$Canvas focus]
}

#----------------------------------------------------------------------------
# Nom      : <CVText::Hit>
# Creation : Novembre 2002 - J.P. Gauthier - CMC/CMOE
#
# But      : Determiner la position d'insertion du curseur
#
# Parametres :
#  <Canvas>  : Identificateur du canvas
#  <X>       : Coordonnee en X
#  <Y>       : Coordonnee en Y
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc CVText::Hit { Canvas X Y { select 1 } } {

   set focus [$Canvas focus]

   $Canvas icursor $focus @$X,$Y
   $Canvas select clear
   $Canvas select from current @$X,$Y
}

#----------------------------------------------------------------------------
# Nom      : <CVText::Insert>
# Creation : Novembre 2002 - J.P. Gauthier - CMC/CMOE
#
# But      : Inserer le caractere a la position du curseur
#
# Parametres :
#  <Canvas>  : Identificateur du canvas
#  <Char>    : Caractere
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc CVText::Insert { Canvas Char KeySim } {

   switch $KeySim {
      "Return" { set Char \n }
      "Tab"    { set Char \t }
   }
   $Canvas insert [$Canvas focus] insert "$Char"
}

#----------------------------------------------------------------------------
# Nom      : <CVText::Go>
# Creation : Novembre 2002 - J.P. Gauthier - CMC/CMOE
#
# But      : Deplacement dans le texte
#
# Parametres :
#  <Canvas>  : Identificateur du canvas
#  <Incr>    : Increment
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc CVText::Go { Canvas Incr } {

   set focus [$Canvas focus]
   $Canvas icursor $focus [expr [$Canvas index $focus insert]+$Incr]
}

#----------------------------------------------------------------------------
# Nom      : <CVText::GoEnd>
# Creation : Novembre 2002 - J.P. Gauthier - CMC/CMOE
#
# But      : Deplacement aux extremite du texte
#
# Parametres :
#  <Canvas>  : Identificateur du canvas
#  <Pos>     : Extremite (0 ou end)
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc CVText::GoEnd { Canvas Pos } {

   set focus [$Canvas focus]
   $Canvas icursor $focus $Pos
}

#----------------------------------------------------------------------------
# Nom      : <CVText::Paste>
# Creation : Novembre 2002 - J.P. Gauthier - CMC/CMOE
#
# But      : Coller le texte du clipboard a la position d'insertion
#
# Parametres :
#  <Canvas>  : Identificateur du canvas
#  <X>       : Coordonnee en X
#  <Y>       : Coordonnee en Y
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc CVText::Paste { Canvas { X {} } { Y {} } } {

   if { [catch { selection get } __s] && [catch { selection get -selection CLIPBOARD } __s] } {
      return
   }

   set focus [$Canvas focus]

   if { $focus=={} } {
      set focus [$Canvas find withtag current]
   }

   if { $focus=={} } {

      if { [string length $X]==0 } {
         set X [expr [winfo pointerx $Canvas] -[winfo rootx $Canvas]]
         set Y [expr [winfo pointery $Canvas] -[winfo rooty $Canvas]]
      }
      Focus $Canvas $X $Y
   } else {
      $Canvas focus $focus
   }
   $Canvas insert $focus insert $__s
}

#----------------------------------------------------------------------------
# Nom      : <CVText::Select>
# Creation : Novembre 2002 - J.P. Gauthier - CMC/CMOE
#
# But      : Selection de tout le texte
#
# Parametres :
#  <Canvas>  : Identificateur du canvas
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc CVText::Select { Canvas } {

   $Canvas select from current 0
   $Canvas select to current end
}

namespace eval CVMagnifier {
   variable Param
   variable Data

   set Param(Zoom)   2
   set Param(Size)   256
   set Data(Zooming) 0
}

#----------------------------------------------------------------------------
# Nom      : <CVMagnifier::Activate>
# Creation : Janvier 2009 - J.P. Gauthier - CMC/CMOE
#
# But      : Activer l'affichage de la loupe
#
# Parametres :
#  <Canvas>  : Identificateur du canvas
#  <X>       : Coordonnee en X
#  <Y>       : Coordonnee en Y
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc CVMagnifier::Activate { Canvas X Y } {
   variable Data
   variable Param

   set Data(Zooming) 1

   image create photo CANVASMAGNIFIER -width $Param(Size) -height $Param(Size)
   $Canvas magnify CANVASMAGNIFIER $X $Y $Param(Zoom)
   $Canvas create image $X $Y -image CANVASMAGNIFIER -tags CANVASMAGNIFIER
   update idletasks
}

#----------------------------------------------------------------------------
# Nom      : <CVMagnifier::DeActivate>
# Creation : Janvier 2009 - J.P. Gauthier - CMC/CMOE
#
# But      : Deactiver l'affichage de la loupe
#
# Parametres :
#  <Canvas>  : Identificateur du canvas
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc CVMagnifier::DeActivate { Canvas } {
   variable Data
   variable Param

   set Data(Zooming) 0

   catch {
      $Canvas delete CANVASMAGNIFIER
      image delete CANVASMAGNIFIER
   }
   update idletasks
}

#----------------------------------------------------------------------------
# Nom      : <CVMagnifier::Create>
# Creation : Janvier 2009 - J.P. Gauthier - CMC/CMOE
#
# But      : Creer une loupe de canvas en initialisant les bindings de controle
#
# Parametres :
#  <Canvas>  : Identificateur du canvas
#  <X>       : Coordonnee en X
#  <Y>       : Coordonnee en Y
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc CVMagnifier::Create { Canvas } {
   variable Param
   variable Data

   bind $Canvas <ButtonPress-1>   { CVMagnifier::Activate   %W %x %y }
   bind $Canvas <B1-Motion>       { CVMagnifier::Move       %W %x %y }
   bind $Canvas <ButtonRelease-1> { CVMagnifier::DeActivate %W }
   bind $Canvas <ButtonPress-4>   { CVMagnifier::Increase   %W %x %y }
   bind $Canvas <ButtonPress-5>   { CVMagnifier::Decrease   %W %x %y }
}

#----------------------------------------------------------------------------
# Nom      : <CVMagnifier::Destroy>
# Creation : Janvier 2009 - J.P. Gauthier - CMC/CMOE
#
# But      : Supprimer une loupe de canvas en supprimant les bindings de controle
#
# Parametres :
#  <Canvas>  : Identificateur du canvas
#  <X>       : Coordonnee en X
#  <Y>       : Coordonnee en Y
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc CVMagnifier::Destroy { Canvas } {
   variable Param
   variable Data

   bind $Canvas <ButtonPress-1>   {}
   bind $Canvas <B1-Motion>       {}
   bind $Canvas <ButtonRelease-1> {}
   bind $Canvas <ButtonPress-4>   {}
   bind $Canvas <ButtonPress-5>   {}
}

#----------------------------------------------------------------------------
# Nom      : <CVMagnifier::Decrease>
# Creation : Janvier 2009 - J.P. Gauthier - CMC/CMOE
#
# But      : Reduire le facteur de zoom de la loupe
#
# Parametres :
#  <Canvas>  : Identificateur du canvas
#  <X>       : Coordonnee en X
#  <Y>       : Coordonnee en Y
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc CVMagnifier::Decrease { Canvas X Y } {
   variable Data
   variable Param

   if { $Data(Zooming) && $Param(Zoom)>2 } {
      incr Param(Zoom) -1
   }

   CVMagnifier::Move $Canvas $X $Y
}

#----------------------------------------------------------------------------
# Nom      : <CVMagnifier::Increase>
# Creation : Janvier 2009 - J.P. Gauthier - CMC/CMOE
#
# But      : Augmenter le facteur de zoom de la loupe
#
# Parametres :
#  <Canvas>  : Identificateur du canvas
#  <X>       : Coordonnee en X
#  <Y>       : Coordonnee en Y
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc CVMagnifier::Increase { Canvas X Y } {
   variable Data
   variable Param

   if { $Data(Zooming) } {
      incr Param(Zoom) 1
   }

   CVMagnifier::Move $Canvas $X $Y
}

#----------------------------------------------------------------------------
# Nom      : <CVMagnifier::Move>
# Creation : Janvier 2009 - J.P. Gauthier - CMC/CMOE
#
# But      : Deplacer la loupe
#
# Parametres :
#  <Canvas>  : Identificateur du canvas
#  <X>       : Coordonnee en X
#  <Y>       : Coordonnee en Y
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc CVMagnifier::Move { Canvas X Y } {
   variable Data
   variable Param

   catch {
      $Canvas itemconf CANVASMAGNIFIER -state hidden
      $Canvas magnify CANVASMAGNIFIER $X $Y $Param(Zoom)

      set s2 [expr $Param(Size)/2.0]
      set X [expr ($X-$s2)<0?$s2:($X+$s2)>[winfo width $Canvas]?[winfo width $Canvas]-$s2:$X]
      set Y [expr ($Y-$s2)<0?$s2:($Y+$s2)>[winfo height $Canvas]?[winfo height $Canvas]-$s2:$Y]
      $Canvas coords CANVASMAGNIFIER $X $Y

      $Canvas itemconf CANVASMAGNIFIER -state normal
      update idletasks
   }
}

#-------------------------------------------------------------------------------
# Nom      : <CVTree::Create>
# Creation : Juin 2014 - E. Legault-Ouellet - CMC/CMOE
#
# But      : Afficher un arbre dans un canvas.
#
# Parametres :
#  <Canvas>      : Canvas ou afficher l'arbre
#  <Tree>        : Arbre a afficher
#  <args>        : Tous les autres arguments qui seront lu en paire {Param Value}
#
#                  Les params peuvent être :
#
#                  <IdCmd> <Fct> Fct est la fonction identitaire appelee lors du
#                       rendering de l'arbre
#                  <ParseCmd> <Fct> Fonction appellée lors de l'ouverture d'une
#                       branche parent)
#                  <PopUpCmd> <Fct> Fonction appellée lors d'un click droit sur
#                       l'arbre)
#                  <SelectCmd> <Fct> Fonction appellée lors de la sélection d'une
#                       branche de l'arbre
#                  <SingleSelectCmd> <Fct> Fonction appellée lors de la sélection d'une
#                       et une seule feuille de l'arbre --> même chose que SelectCmd en
#                       SingleSelectMode.
#                  <AllowMultSelect> <True|False> Option permettant la selection
#                       multiple de branches dans l'arbre. (Defaut : False) Cette
#                       option est incompatible avec l'option "DblClickSelect".
#                  <DblClickSelect> <True|False> Option déterminant si la sélection
#                       se fait en un simple ou un double click de la souris.
#                       (Défaut : True). Sera forcé à false si l'option
#                       "AllowMultSelect" est à "True".
#                  <AllowParentSelect> <True|False> Option déterminant si la
#                       sélection d'une branche ayant des enfants est possible.
#                       (Défaut : False). Sera forcé à false si l'option
#                       "AllowMultSelect" est à False
#                  <ColorBorder> <Color> Détermine la couleur de la bordure du
#                       rectangle de sélection. (Défaut : "black")
#                  <HighlightFullWidth> <True|False> Détermine si le retangle de
#                       sélection couvrira ou non la pleine largeure du canvas.
#                       (Défaut : False) Attention : si la taille du canvas change,
#                       le rectangle de sélection ne se mettra à jour qu'au prochain
#                       rafraîchissement de la sélection.
#                  <VariableTextColor> <True|False> Détermine s'il y aura des couleurs
#                       différentes pour chaque noeud. Si mis à True, le paramètre 
#                       "color" doit aussi être présent lors de l'appel à IdCmd.
#                       (Défaut : False)
#                  <VariableIcon> <True|False> Détermine s'il y aura des icones
#                       différentes pour chaque noeud. Si mis à True, le paramètre 
#                       "icon" doit aussi être présent lors de l'appel à IdCmd. Si
#                       l'option "VariableTextColor" est aussi mis à True, alors
#                       le paramètre "icon" apparaîtra après le paramètre color.
#                       L'appel ressemblera alors à "IdCmd Tree Branch IsLeaf Color Icon"
#                       (Défaut : False)
#                  <AllowHiddenBranch> <True|False> Détermine s'il peut y avoir des
#                       branches cachées. Si mis à True, le paramètre "hidden" doit aussi
#                       être présent lors de l'appel à IdCmd. Le paramètre "hidden"
#                       apparaîtra après les paramètre VariableColor et VariableIcon, le
#                       cas échéant.
#
#                  Notes:
#                   - Seul le paramètre IdCmd est obligatoire.
#                   - Les callbacks autre que PopUpCmd recevront les paramètres <Tree>,
#                     <Branch> et <Open> lors de leur appel. (Tree étant le nom de l'arbre
#                     de données, Branch étant la branche faisant l'objet du callback
#                     et Open étant un indicateur de l'état d'ouverture de la branche.)
#                     Le callback PopUpCmd recevra <Canvas> <X> <Y> <Branch> oû
#                     X et Y sont les coordonnées relativement à la fenêtre root
#                     (root window).
#
# Retour    :
#
# Remarque :
#   -Ceci est une extension au package ::struct::tree
#
#-------------------------------------------------------------------------------

namespace eval CVTree {
    global GDefs
    variable Param

    set Param(AllowMultSelect)      False
    set Param(DblClickSelect)       True
    set Param(AllowParentSelect)    False
    set Param(ColorBorder)          black
    set Param(ColorHighLight)       $GDefs(ColorHighLight)
    set Param(HighlightFullWidth)   False
    set Param(VariableTextColor)    False
    set Param(VariableIcon)         False
    set Param(AllowHiddenBranch)    False
}

proc CVTree::Create { Canvas Tree args } {
    variable Data
    variable Param

    #----- Default command callbacks

    set Data(IdCmd$Tree)            ""
    set Data(ParseCmd$Tree)         ""
    set Data(PopUpCmd$Tree)         ""
    set Data(SelectCmd$Tree)        ""
    set Data(SingleSelectCmd$Tree)  ""

    #----- Default params values

    foreach {opt val} [array get Param] {
        set Data($opt$Tree) $val
    }

    #----- Parse args

    foreach {opt val} $args {
        set Data($opt$Tree) $val
    }

    #----- Unmodifiable params

    set Data(SelectFrom$Tree)   ""
    set Data(Selection$Tree)    ""

    #----- Incompatibilities

    if { $Data(AllowMultSelect$Tree) } {
        set Data(DblClickSelect$Tree) False
    } else {
        set Data(AllowParentSelect$Tree) False
    }

    $Tree set [$Tree rootname] open True
    CVTree::Render $Canvas $Tree
}

#-------------------------------------------------------------------------------
# Nom      : <CVTree::ExecCallback>
# Creation : Février 2014 - E. Legault-Ouellet - CMC/CMOE
#
# But      : Exécute le callback enregistré si ce dernier a été fourni.
#
# Parametres :
#  <Callback>    : La fonction à exécuter
#  <Canvas>      : Canvas où est affiché l'arbre
#  <Tree>        : Arbre des données
#  <Branch>      : Branche qui fait l'objet du callback
#  <Open>        : Statut d'ouverture de la branche
#
# Retour    :
#
# Remarque :
#   -Ceci est une extension au package ::struct::tree
#
#-------------------------------------------------------------------------------
proc CVTree::ExecCallback { Callback Canvas Tree Branch Open } {
    if { $Callback!="" } {
        $Canvas configure -cursor watch
        update idletasks;
        eval $Callback $Tree $Branch $Open
        $Canvas configure -cursor left_ptr
    }
}

#-------------------------------------------------------------------------------
# Nom      : <CVTree::Highlight>
# Creation : Février 2014 - E. Legault-Ouellet - CMC/CMOE
#
# But      : Illumine les données sélectionnées
#
# Parametres :
#  <Canvas>      : Canvas où est affiché l'arbre
#  <Tree>        : Arbre des données
#  <Branch>      : Branche à illuminer (Utilisé en SingleSelectMode seulement)
#
# Retour    :
#
# Remarque :
#   -Ceci est une extension au package ::struct::tree
#
#-------------------------------------------------------------------------------
proc CVTree::Highlight { Canvas Tree {Branch ""} } {
    variable Data

    if { !$Data(AllowMultSelect$Tree) } {
        #----- Normal mode (Single select)

        lassign [$Canvas bbox CVTREETEXT$Branch] x1 y1 x2 y2
        if { $Data(HighlightFullWidth$Tree) } {
            set x1 0
            set x2 [expr {[winfo width $Canvas]-1}]
        }

        #----- Move the highlight box if it exists or create it if it doesn't

        if { ![llength [$Canvas find withtag CVTREESELECT$Tree]] } {
            eval $Canvas create rectangle [list $x1 $y1 $x2 $y2] -fill $Data(ColorHighLight$Tree) -outline $Data(ColorBorder$Tree) -width 1 -tags CVTREESELECT$Tree
            $Canvas lower CVTREESELECT$Tree
        } else {
            eval $Canvas coords CVTREESELECT$Tree [list $x1 $y1 $x2 $y2]
        }
    } else {
        #----- Multiselect mode

        $Canvas delete CVTREESELECT$Tree

        #----- Go through the whole tree and create a different BBox for each of the continous groups encountered

        set nodes {}
        $Tree walk [$Tree rootname] -type dfs -order pre node {lappend nodes $node; if { ![$Tree get $node open] } { struct::tree::prune }}

        set inHlGroup False
        foreach node $nodes {
            if { [$Tree keyexists $node selected] && [$Tree get $node selected] } {
                if { $inHlGroup } {
                    #----- We are already in a group; add this one to the global BBox

                    lassign [$Canvas bbox CVTREETEXT$node] nx1 ny1 nx2 ny2

                    set x1 [expr min($x1, $nx1)]
                    set y1 [expr min($y1, $ny1)]
                    set x2 [expr max($x2, $nx2)]
                    set y2 [expr max($y2, $ny2)]
                } else {
                    #----- Start the BBox

                    lassign [$Canvas bbox CVTREETEXT$node] x1 y1 x2 y2
                    set inHlGroup True
                }
            } elseif { $inHlGroup } {
                #----- A BBox is now complete, show it

                set inHlGroup False

                if { $Data(HighlightFullWidth$Tree) } {
                    set x1 0
                    set x2 [expr {[winfo width $Canvas]-1}]
                }

                $Canvas create rectangle [list $x1 $y1 $x2 $y2] -fill $Data(ColorHighLight$Tree) -outline $Data(ColorBorder$Tree) -width 1 -tags CVTREESELECT$Tree
                $Canvas lower CVTREESELECT$Tree
            }
        }
    }
}

#-------------------------------------------------------------------------------
# Nom      : <CVTree::OnBoxClick>
# Creation : Février 2014 - E. Legault-Ouellet - CMC/CMOE
#
# But      : Callback pour une action effectuée sur les checkbox
#
# Parametres :
#  <Canvas>      : Canvas où est affiché l'arbre
#  <Tree>        : Arbre des données
#  <Branch>      : Branche qui fait l'objet du callback
#  <Open>        : Statut d'ouverture de la branche
#
# Retour    :
#
# Remarque :
#   -Ceci est une extension au package ::struct::tree
#
#-------------------------------------------------------------------------------
proc CVTree::OnBoxClick { Canvas Tree Branch Open } {
    $Tree set $Branch box $Open

    #----- Transmit the parent box status to the children

    if { [$Tree keyexists $Branch box] } {
        set box [$Tree get $Branch box]
        foreach child [$Tree children $Branch] {
            if { [$Tree keyexists $child box] } {
                $Tree set $child box $box
            }
        } 
    }

    CVTree::Render $Canvas $Tree
}

#-------------------------------------------------------------------------------
# Nom      : <CVTree::OnClick>
# Creation : Février 2014 - E. Legault-Ouellet - CMC/CMOE
#
# But      : Callback lors d'un click sur une branche
#
# Parametres :
#  <Canvas>      : Canvas où est affiché l'arbre
#  <Tree>        : Arbre des données
#  <Branch>      : Branche qui fait l'objet du click
#  <Open>        : Statut d'ouverture de la branche
#  <IsLeaf>      : Si la branche est une feuille (True) ou pas (False)
#  <Ctrl>        : Si la touche "Control" était enfoncée au moment du click
#  <Shift>       : Si la touche "Shift" était enfoncée au moment du click
#
# Retour    :
#
# Remarque :
#   -Ceci est une extension au package ::struct::tree
#
#-------------------------------------------------------------------------------
proc CVTree::OnClick { Canvas Tree Branch Open IsLeaf {Ctrl False} {Shift False} } {
    variable Data

    if { !$Data(AllowMultSelect$Tree) } {
        #----- Normal mode

        if { !$IsLeaf } {
            #----- We have a parent

            $Tree set $Branch open $Open
            
            set Data(Selection$Tree) ""
            CVTree::Highlight $Canvas $Tree $Branch
            CVTree::ExecCallback $Data(ParseCmd$Tree) $Canvas $Tree $Branch $Open

            #----- Transmit the parent box status to the children

            if { [$Tree keyexists $Branch box] } {
                set box [$Tree get $Branch box]
                foreach child [$Tree children $Branch] {
                    if { [$Tree keyexists $child box] } {
                        $Tree set $child box $box
                    }
                } 
            }

            CVTree::Render $Canvas $Tree
        } elseif { !$Data(DblClickSelect$Tree) } {
            #----- We have a leaf that can be selected with a single click

            if { [$Tree keyexists $Branch box] } {
                $Tree set $Branch box $Open
            }

            set Data(Selection$Tree) $Branch
            CVTree::Highlight $Canvas $Tree $Branch
            CVTree::ExecCallback $Data(SelectCmd$Tree) $Canvas $Tree $Branch $Open
            CVTree::ExecCallback $Data(SingleSelectCmd$Tree) $Canvas $Tree $Branch $Open
            CVTree::Render $Canvas $Tree
        }
    } elseif { !$IsLeaf && (
        !$Data(AllowParentSelect$Tree)
        || $Data(SelectFrom$Tree)==""
        || $Data(SelectFrom$Tree)!="" && ![$Tree exists $Data(SelectFrom$Tree)]
        || !$Shift && !$Ctrl
    ) } {
        #----- Multiple Select mode on parent (non-leaf) node, but as simple selection

        $Tree set $Branch open $Open

        if { $Data(AllowParentSelect$Tree) } {
            CVTree::SelectionClear $Canvas $Tree
        }
        CVTree::SelectSetState True $Canvas $Tree $Branch $IsLeaf True
        CVTree::Highlight $Canvas $Tree
        CVTree::ExecCallback $Data(ParseCmd$Tree) $Canvas $Tree $Branch $Open
        CVTree::Render $Canvas $Tree
        CVTree::Highlight $Canvas $Tree
    } else {
        #----- Multiple Select mode on leaf node or on parent node in a case of multiple selection

        if { $Ctrl && !$Shift } {
            #----- Add or remove current branch to selection

            if { [catch {set s [$Tree get $Branch selected]}] } {
                set s False
            }
            if { $s } {
                CVTree::SelectSetState False $Canvas $Tree $Branch $IsLeaf False
            } else {
                CVTree::SelectSetState True $Canvas $Tree $Branch $IsLeaf True
            }
        } elseif { $Shift && $Data(SelectFrom$Tree)!="" && [$Tree exists $Data(SelectFrom$Tree)] } {
            if { !$Ctrl } {
                #----- Unselect the previously selected branches

                CVTree::SelectionClear $Canvas $Tree
            }

            set b1 $Data(SelectFrom$Tree)
            set b2 $Branch

            #----- Find the common parent (Not necessary but should be faster than walking the whole tree)

            foreach p1 [lreverse [$Tree ancestors $b1]] p2 [lreverse [$Tree ancestors $b2]] {
                if { $p1 == $p2 } {
                    set btop $p1
                } else {
                    break
                }
            }

            #----- Walk through the tree from that parent (We could start from root, but this should be faster than walking the whole tree)

            set nodes {}
            $Tree walk $btop -type dfs -order pre node {lappend nodes $node}

            #------ Get the branches index of our seleted nodes and only keep the subset that interest us

            set idx1 [lsearch -exact $nodes $b1]
            set idx2 [lsearch -exact $nodes $b2]
            if { $Ctrl } {
                set Data(SelectFrom$Tree) [lindex $nodes $idx2]
            }
            set nodes [lrange $nodes [expr min($idx1, $idx2)] [expr max($idx1, $idx2)]]

            #------ We now have the right subset, mark all the nodes appropriatly

            foreach node $nodes {
                CVTree::SelectSetState True $Canvas $Tree $node [$Tree get $node isLeaf] False
            }
        } else {
            #----- Unselect the previously selected branches

            CVTree::SelectionClear $Canvas $Tree

            #----- Select the current one

            CVTree::SelectSetState True $Canvas $Tree $Branch $IsLeaf True
            CVTree::ExecCallback $Data(SingleSelectCmd$Tree) $Canvas $Tree $Branch $Open
        }

        CVTree::Highlight $Canvas $Tree
        CVTree::Render $Canvas $Tree
        CVTree::Highlight $Canvas $Tree
    }
}

#-------------------------------------------------------------------------------
# Nom      : <CVTree::OnDblClick>
# Creation : Février 2014 - E. Legault-Ouellet - CMC/CMOE
#
# But      : Callback pour les double-click.
#
# Parametres :
#  <Canvas>      : Canvas où est affiché l'arbre
#  <Tree>        : Arbre des données
#  <Branch>      : Branche qui fait l'objet du double-click
#  <Open>        : Statut d'ouverture de la branche
#  <IsLeaf>      : Si la branche est une feuille (True) ou pas (False)
#
# Retour    :
#
# Remarque :
#   -Ceci est une extension au package ::struct::tree
#
#-------------------------------------------------------------------------------
proc CVTree::OnDblClick { Canvas Tree Branch Open IsLeaf } {
    variable Data

    if { $IsLeaf && $Data(DblClickSelect$Tree) } {
        #----- Mark the box according to the Open state

        if { [$Tree keyexists $Branch box] } {
            $Tree set $Branch box $Open
        }

        set Data(Selection$Tree) $Branch
        CVTree::Highlight $Canvas $Tree $Branch
        CVTree::ExecCallback $Data(SelectCmd$Tree) $Canvas $Tree $Branch $Open
        CVTree::ExecCallback $Data(SingleSelectCmd$Tree) $Canvas $Tree $Branch $Open
    }
}

#-------------------------------------------------------------------------------
# Nom      : <CVTree::Render>
# Creation : Juin 2008 - J.P. Gauthier - CMC/CMOE
# Modifie  : Fevrier 2014 - E. Legault-Ouellet - CMC/CMOE
#
# But      : Afficher un arbre dans un canvas.
#
# Parametres :
#  <Canvas>      : Canvas ou afficher l'arbre
#  <Tree>        : Arbre a afficher
#
# Retour    :
#
# Remarque :
#   -Ceci est une extension au package ::struct::tree
#
#-------------------------------------------------------------------------------
proc CVTree::Render { Canvas Tree } {
    variable Data

    $Canvas delete CVTREE$Tree

    set X 0
    set Y 0

    CVTree::RenderBranch $Canvas $Tree [$Tree rootname] X Y

    catch { $Canvas configure -scrollregion "0 0 [lrange [$Canvas bbox CVTREE$Tree] 2 end]" }
}

#-------------------------------------------------------------------------------
# Nom      : <CVTree::RenderBranch>
# Creation : Juin 2008 - J.P. Gauthier - CMC/CMOE
#
# But      : Afficher une branche d'un arbre dans un canvas.
#
# Parametres :
#  <Canvas>      : Canvas ou afficher l'arbre
#  <Tree>        : Arbre a afficher
#  <Branch>      : Branche a afficher
#  <X>           : Coordonnee en X
#  <Y>           : Coordonnee en Y
#
# Retour    :
#
# Remarque :
#   -Ceci est une extension au package ::struct::tree
#
#-------------------------------------------------------------------------------

proc CVTree::RenderBranch { Canvas Tree Branch X Y } {
    variable Data
    global GDefs

    upvar $X x
    upvar $Y y

    set dy 20
    set dx 10
    set db 0
    set di 0
    set y0 $y

    incr x $dx

    if { $Data(VariableIcon$Tree) } {
        set di 20
    }
    #if { [$Tree keyexists $Branch box] } {
    #    incr db 15
    #}

    foreach branch [$Tree children $Branch]  {
        set leaf True

        set addArgs leaf
        if { $Data(VariableTextColor$Tree) } {
            lappend addArgs color
        }
        if { $Data(VariableIcon$Tree) } {
            lappend addArgs icon
        }
        if { $Data(AllowHiddenBranch$Tree) } {
            lappend addArgs hidden
        }

        if { [set id [$Data(IdCmd$Tree) $Tree $branch {*}$addArgs]]!="" } {
            if { $Data(AllowHiddenBranch$Tree) && $hidden } {
                continue
            }
            set y0 [incr y $dy]

            #if { !$Data(AllowMultSelect$Tree) && [$Tree keyexists $branch box] } {
            #    switch [$Tree get $branch box] {
            #        True  { $Canvas create bitmap [expr $x+$dx+5] $y -bitmap @$GDefs(Dir)/share/bitmap/optcheck.xbm -tags "CVTREE$Tree CVTREEBOX$Tree$branch"
            #                $Canvas bind CVTREEBOX$Tree$branch <Button-1> "CVTree::OnBoxClick $Canvas $Tree $branch False" }
            #        False { $Canvas create bitmap [expr $x+$dx+5] $y -bitmap @$GDefs(Dir)/share/bitmap/optbox.xbm -tags "CVTREE$Tree CVTREEBOX$Tree$branch"
            #                $Canvas bind CVTREEBOX$Tree$branch <Button-1> "CVTree::OnBoxClick $Canvas $Tree $branch True" }
            #    }
            #}

            if { $Data(VariableIcon$Tree) } {
                $Canvas create image [expr $x+$dx+$db+8] $y -image $icon -tags "CVTREE$Tree CVTREETEXT$branch $branch"
            }

            if { $Data(VariableTextColor$Tree) } {
                $Canvas create text [expr $x+$dx+$db+$di] $y -text $id -anchor w -tags "CVTREE$Tree CVTREETEXT$branch $branch" -font $GDefs(Font) -fill $color
            } else {
                $Canvas create text [expr $x+$dx+$db+$di] $y -text $id -anchor w -tags "CVTREE$Tree CVTREETEXT$branch $branch" -font $GDefs(Font)
            }

            if { [$Tree keyexists $branch bubble] && [set bubble [$Tree get $branch bubble]]!="" } {
                CanvasBubble::Create $Canvas CVTREETEXT$branch $bubble 400
            }

            set isLeaf  ""
            set open    ""
            set bindTag ""
            if { $leaf && [$Tree isleaf $branch] } {
                if { [expr $x-$dx]>5 } {
                    $Canvas create line [expr $x-$dx] $y [expr $x+$dx-5] $y -width 1 -fill black -tags "CVTREE$Tree"
                }

                set bindTag CVTREETEXT$branch
                set isLeaf  True
                set open    True
            } else {
                if { [expr $x-$dx]>5 } {
                    $Canvas create line [expr $x-$dx] $y [expr $x-4] $y -width 1 -fill black -tags "CVTREE$Tree"
                }
                set bindTag $branch
                set isLeaf False
                if { [$Tree get $branch open] } {
                    set open False
                    $Canvas create bitmap $x $y -bitmap @$GDefs(Dir)/share/bitmap/minus.ico -tags "CVTREE$Tree $branch"
                    set x0 $x
                    set y0 $y
                    set yend [CVTree::RenderBranch $Canvas $Tree $branch x y]

                    set x $x0
                    $Canvas create line $x $yend $x [expr $y0+5] -width 1 -fill black -tags "CVTREE$Tree"
                } else {
                    set open True
                    $Canvas create bitmap $x $y -bitmap @$GDefs(Dir)/share/bitmap/plus.ico -tags "CVTREE$Tree $branch"
                }
            }

            $Tree set $branch isLeaf $isLeaf

            if { $Data(AllowMultSelect$Tree) } {
                $Canvas bind $bindTag   <ButtonRelease-1>               "CVTree::OnClick    $Canvas $Tree $branch $open $isLeaf False False"
                $Canvas bind $bindTag   <Control-ButtonRelease-1>       "CVTree::OnClick    $Canvas $Tree $branch $open $isLeaf True  False"
                $Canvas bind $bindTag   <Shift-ButtonRelease-1>         "CVTree::OnClick    $Canvas $Tree $branch $open $isLeaf False True"
                $Canvas bind $bindTag   <Control-Shift-ButtonRelease-1> "CVTree::OnClick    $Canvas $Tree $branch $open $isLeaf True  True"
            } else {
                $Canvas bind $bindTag   <ButtonRelease-1>               "CVTree::OnClick    $Canvas $Tree $branch $open $isLeaf False False"
            }

            if { $Data(DblClickSelect$Tree) } {
                $Canvas bind $bindTag   <Double-ButtonRelease-1>        "CVTree::OnDblClick $Canvas $Tree $branch $open $isLeaf"
            }

            if { $Data(PopUpCmd$Tree)!="" } {
                if { $Data(AllowMultSelect$Tree) } {
                    $Canvas bind $branch <Button-3> "$Data(PopUpCmd$Tree) $Canvas %X %Y $branch"
                } else {
                    $Canvas bind $branch <Button-3> "CVTree::Highlight $Canvas $Tree $branch; $Data(PopUpCmd$Tree) $Canvas %X %Y $branch"
                }
            }
        }
    }
    return $y0
}

#-------------------------------------------------------------------------------
# Nom      : <CVTree::SelectBranch>
# Creation : Février 2014 - E. Legault-Ouellet - CMC/CMOE
#
# But      : Ouvre et sélectionne un noeud (simule un click utilisateur)
#
# Parametres :
#  <Canvas>      : Canvas où est affiché l'arbre
#  <Tree>        : Arbre des données
#  <Branch>      : Branche a afficher
#
# Retour    :
#
# Remarque :
#   -Ceci est une extension au package ::struct::tree
#
#-------------------------------------------------------------------------------
proc CVTree::SelectBranch { Canvas Tree Branch } {
    variable Data

    if { $Data(DblClickSelect$Tree) } {
        CVTree::OnDblClick $Canvas $Tree $Branch True [$Tree get $Branch isLeaf]
    } else {
        CVTree::OnClick $Canvas $Tree $Branch True [$Tree get $Branch isLeaf]
    }
}

#-------------------------------------------------------------------------------
# Nom      : <CVTree::SelectSetState>
# Creation : Février 2014 - E. Legault-Ouellet - CMC/CMOE
#
# But      : Change le statut de sélection d'un branche en mode "multiselect"
#
# Parametres :
#  <Selected>    : Si la branche doit être marquée comme sélectionnée (True) ou non (False)
#  <Canvas>      : Canvas où est affiché l'arbre
#  <Tree>        : Arbre des données
#  <Branch>      : Branche a afficher
#  <IsLeaf>      : Si la branche est une feuille (True) ou pas (False)
#  <Persistent>  : Si la branche sélectionnée doit être marquée comme la dernière activée
#
# Retour    :
#
# Remarque :
#   -Ceci est une extension au package ::struct::tree
#
#-------------------------------------------------------------------------------
proc CVTree::SelectSetState { Selected Canvas Tree Branch {IsLeaf False} {Persistent False} } {
    variable Data

    if { $Selected } {
        if { $Data(AllowParentSelect$Tree) || $IsLeaf } {
            $Tree set $Branch selected      True

            CVTree::ExecCallback $Data(SelectCmd$Tree) $Canvas $Tree $Branch [$Tree get $Branch open]

            if { $Persistent } {
                set Data(SelectFrom$Tree) $Branch
            }
        } else {
            $Tree set $Branch selected      False
        }

        if { [$Tree keyexists $Branch box] } {
            $Tree set $Branch box [$Tree get $Branch open]
        }
    } else {
        $Tree set $Branch selected      False

        if {[$Tree keyexists $Branch box] } {
            $Tree set $Branch box [$Tree get $Branch open]
        }

        if { $Persistent } {
            set Data(SelectFrom$Tree) ""
        }
    }
}

#-------------------------------------------------------------------------------
# Nom      : <CVTree::SelectionClear>
# Creation : Février 2014 - E. Legault-Ouellet - CMC/CMOE
#
# But      : Désélectionne toutes les branches sélectionnées.
#
# Parametres :
#  <Canvas>      : Canvas où est affiché l'arbre
#  <Tree>        : Arbre des données
#
# Retour    :
#
# Remarque :
#   -Ceci est une extension au package ::struct::tree
#
#-------------------------------------------------------------------------------
proc CVTree::SelectionClear { Canvas Tree } {
    variable Data

    if { !$Data(AllowMultSelect$Tree) } {
        $Canvas delete CVTREESELECT$Tree
        set Data(Selection$Tree) ""
    } else {
        foreach b [dict keys [dict filter [$Tree attr selected] value True]] {
            CVTree::SelectSetState False $Canvas $Tree $b
        }
    }
}

#-------------------------------------------------------------------------------
# Nom      : <CVTree::SelectionGet>
# Creation : Février 2014 - E. Legault-Ouellet - CMC/CMOE
#
# But      : Retourne une liste de toutes les branches sélectionnés. Ne
#            fonctionne qu'en mode sélection multiple (AllowMultSelect=True)
#
# Parametres :
#  <Canvas>      : Canvas où est affiché l'arbre
#  <Tree>        : Arbre des données
#
# Retour    :
#
# Remarque :
#   -Ceci est une extension au package ::struct::tree
#
#-------------------------------------------------------------------------------
proc CVTree::SelectionGet { Canvas Tree } {
    variable Data

    if { $Data(AllowMultSelect$Tree) } {
        return [dict keys [dict filter [$Tree attr selected] value True]]
    } else {
        return $Data(Selection$Tree)
    }
}

namespace eval Shape {
   variable Data

   set Data(X0)    0
   set Data(Y0)    0
   set Data(Blend) 1
}

#----------------------------------------------------------------------------
# Nom      : <Shape::BindFull>
# Creation : Avril 2008 - J.P. Gauthier - CMC/CMOE
#
# But      : Initialiser les "bindings" de plein ecran/widget
#
# Parametres :
#  <Canvas>  : Identificateur du canvas
#  <Tags>    : Tags des objets
#  <Var>     : Variable de redimentionnement
#  <Command> : Commande de redimentionnement
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc Shape::BindFull { Canvas Tag Var { Command "" } } {
   global GDefs

   if { ![winfo exists $Canvas.bf$Tag] } {
      set box [$Canvas bbox $Tag]
      checkbutton $Canvas.bf$Tag -bg $GDefs(ColorFrame) -bitmap @$GDefs(Dir)/share/bitmap/cvfull.xbm -cursor hand1 -bd 1 \
         -indicatoron false -variable $Var -onvalue 1 -offvalue 0 -command "if { \$$Var } { Shape::Full $Canvas $Tag $Command }"
      $Canvas create window [expr [lindex $box 2]-11] [lindex $box 3] -window $Canvas.bf$Tag -anchor se -tags "BF$Tag NOPRINT"
   }
}

#----------------------------------------------------------------------------
# Nom      : <Shape::BindDestroy>
# Creation : Avril 2008 - J.P. Gauthier - CMC/CMOE
#
# But      : Initialiser les "bindings" de destruction de widget
#
# Parametres :
#  <Canvas>  : Identificateur du canvas
#  <Tags>    : Tags des objets
#  <Command> : Commande de redimentionnement
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc Shape::BindDestroy { Canvas Tag { Command "" } } {
   global GDefs

   if { ![winfo exists $Canvas.bd$Tag] } {
     set box [$Canvas bbox $Tag]

     button $Canvas.bd$Tag -bg $GDefs(ColorFrame) -bitmap @$GDefs(Dir)/share/bitmap/cvdel.xbm -cursor pirate -bd 1 -relief raised -command "$Canvas delete $Tag BD$Tag; destroy $Canvas.bd$Tag"

     if { $Command!="" } {
        eval $Canvas.bd$Tag configure -command \"$Command $Tag\; $Canvas delete $Tag BD$Tag\; destroy $Canvas.bd$Tag\"
     }

     $Canvas create window [lindex $box 2] [lindex $box 1] -window $Canvas.bd$Tag -anchor ne -tags "BD$Tag NOPRINT"
   }
}

#----------------------------------------------------------------------------
# Nom      : <Shape::BindAllMove>
# Creation : Decembre 2000 - J.P. Gauthier - CMC/CMOE
#
# But      : Initialiser les "bindings" de deplacement d'items
#
# Parametres :
#  <Canvas>  : Identificateur du canvas
#  <Tags>    : Tag des objets
#  <Command> : Commande a executer
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc Shape::BindAllMove { Canvas Tags { Command "" } } {

   set tag [lindex $Tags 0]

   $Canvas bind $tag <Enter>           "$Canvas configure -cursor fleur"
   $Canvas bind $tag <Leave>           "$Canvas configure -cursor left_ptr"
   $Canvas bind $tag <ButtonPress-1>   "Shape::Set $Canvas $tag %X %Y"
   $Canvas bind $tag <ButtonRelease-1> "Shape::UnSet $Canvas $tag"

   if { $Command!="" } {
      $Canvas bind $tag <B1-Motion>    "Shape::Move $Canvas \"$Tags\" %X %Y ; $Command"
   } else {
      $Canvas bind $tag <B1-Motion>    "Shape::Move $Canvas \"$Tags\" %X %Y"
   }
}

#----------------------------------------------------------------------------
# Nom      : <Shape::BindMove>
# Creation : Decembre 2000 - J.P. Gauthier - CMC/CMOE
#
# But      : Initialiser les "bindings" de deplacement d'items
#
# Parametres :
#  <Canvas>  : Identificateur du canvas
#  <Tags>    : Tag des objets
#  <Command> : Commande a executer
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc Shape::BindMove { Canvas Tag { Command "" } } {
   global GDefs

   if { ![winfo exists $Canvas.bm$Tag] } {
      set box [$Canvas bbox $Tag]
      label $Canvas.bm$Tag -bg $GDefs(ColorFrame) -bitmap @$GDefs(Dir)/share/bitmap/cvmove.xbm -cursor fleur -bd 1 -relief raised

      $Canvas create window [expr [lindex $box 2]-11] [lindex $box 3] -window $Canvas.bm$Tag -anchor se -tags "BM$Tag NOPRINT"
   }

   bind $Canvas.bm$Tag <ButtonPress-1>   "Shape::Set   $Canvas $Tag %X %Y"
   bind $Canvas.bm$Tag <ButtonRelease-1> "Shape::UnSet $Canvas $Tag"

   if { $Command!="" } {
      bind $Canvas.bm$Tag <B1-Motion>    "Shape::Move $Canvas $Tag %X %Y; $Command"
   } else {
      bind $Canvas.bm$Tag <B1-Motion>    "Shape::Move $Canvas $Tag %X %Y"
   }
}

#----------------------------------------------------------------------------
# Nom      : <Shape::BindScale>
# Creation : Decembre 2000 - J.P. Gauthier - CMC/CMOE
#
# But      : Initialiser les "bindings" de redimentionnement d'items
#
# Parametres :
#  <Canvas>  : Identificateur du canvas
#  <Tags>    : Tags des objets
#  <Command> : Commande de redimentionnement
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc Shape::BindScale { Canvas Tag { Command "" } } {
   global GDefs

   if { ![winfo exists $Canvas.bs$Tag] } {
      set box [$Canvas bbox $Tag]
      label $Canvas.bs$Tag -bg $GDefs(ColorFrame) -bitmap @$GDefs(Dir)/share/bitmap/cvscale.xbm -bd 0 -cursor sizing -bd 1 -relief raised

      $Canvas create window [lindex $box 2] [lindex $box 3] -window $Canvas.bs$Tag -anchor se -tags "BS$Tag NOPRINT"
   }
   
   bind $Canvas.bs$Tag <ButtonPress-1>   "Shape::Set   $Canvas $Tag %X %Y"
   bind $Canvas.bs$Tag <ButtonRelease-1> "Shape::UnSet $Canvas $Tag"
   bind $Canvas.bs$Tag <B1-Motion>       "Shape::Scale $Canvas $Tag %X %Y $Command"
}

#----------------------------------------------------------------------------
# Nom      : <Shape::BindWidget>
# Creation : Novembre 2010 - J.P. Gauthier - CMC/CMOE
#
# But      : Installation des evenements de gestion estion de
#               l'activation/desactivation des widgets actifs
#
# Parametres :
#   <Frame>  : Identificateur de canvas
#   <Tag>    : Tag de l'objet
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc Shape::BindWidget { Canvas Tag } {

   #----- bindings de placement des bouttons
   $Canvas bind $Tag <Leave> "+ Shape::Widget %W $Tag %x %y 0"
   $Canvas bind $Tag <Enter> "+ Shape::Widget %W $Tag %x %y 1"

   $Canvas lower NOPRINT
   Shape::Widget $Canvas {} 0 0 0
}

#----------------------------------------------------------------------------
# Nom      : <Shape::Widget>
# Creation : Novembre 2010 - J.P. Gauthier - CMC/CMOE
#
# But      : Gestion de l'acrivation/desactivation des widgets actifs
#
# Parametres :
#   <Canvas> : Identificateur de canvas
#   <Tag>    : Tag de l'objet
#   <X>      : Coordonnee en X du deplacement
#   <Y>      : Coordonnee en Y du deplacement
#   <Visible>: Visibilite ou non
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc Shape::Widget { Canvas Tag X Y Visible } {
   variable Data

   #----- If we entered a new widget, clear all options
   if { $Visible && !$Page::Param(WidgetOpt) } {
      $Canvas itemconfigure NOPRINT -state hidden
   }
   
   if { ![llength $Tag] } {
      #----- Global settings of widgets
      if { !$Visible && !$Page::Param(WidgetOpt) } {
         $Canvas itemconfigure NOPRINT -state hidden
      } elseif { $Page::Param(WidgetOpt) } {
         $Canvas itemconfigure NOPRINT -state normal
      }
   } else {
      #----- Per widget settings
      
      if { !$Visible && !$Page::Param(WidgetOpt) } {
         #----- Disable the options for the widget we just left
         set coords [$Canvas bbox $Tag]
         if { !($X>0 && $X>[lindex $coords 0] && $X<[lindex $coords 2] && $Y>[lindex $coords 1] && $Y<[lindex $coords 3]) } {
            glrender -xexpose -1
            
            $Canvas itemconfigure SCPAGE$Tag -state hidden
            $Canvas itemconfigure BSPAGE$Tag -state hidden
            $Canvas itemconfigure BMPAGE$Tag -state hidden
            $Canvas itemconfigure BFPAGE$Tag -state hidden
            $Canvas itemconfigure BDPAGE$Tag -state hidden
            $Canvas itemconfigure BOPAGE$Tag -state hidden
            $Canvas itemconfigure UDPAGE$Tag -state hidden

            $Canvas itemconfigure SC$Tag -state hidden
            $Canvas itemconfigure BS$Tag -state hidden
            $Canvas itemconfigure BM$Tag -state hidden
            $Canvas itemconfigure BF$Tag -state hidden
            $Canvas itemconfigure BD$Tag -state hidden
            $Canvas itemconfigure BO$Tag -state hidden
            $Canvas itemconfigure UD$Tag -state hidden
            $Canvas itemconfigure OP$Tag -state hidden

            glrender -xexpose 1
         }
      } else {
         #----- Enable options for the widget we just entered
         $Canvas itemconfigure SCPAGE$Tag -state normal
         $Canvas itemconfigure BSPAGE$Tag -state normal
         $Canvas itemconfigure BMPAGE$Tag -state normal
         $Canvas itemconfigure BFPAGE$Tag -state normal
         $Canvas itemconfigure BDPAGE$Tag -state normal
         $Canvas itemconfigure BOPAGE$Tag -state normal
         $Canvas itemconfigure UDPAGE$Tag -state normal

         $Canvas itemconfigure SC$Tag -state normal
         $Canvas itemconfigure BS$Tag -state normal
         $Canvas itemconfigure BM$Tag -state normal
         $Canvas itemconfigure BF$Tag -state normal
         $Canvas itemconfigure BD$Tag -state normal
         $Canvas itemconfigure BO$Tag -state normal
         $Canvas itemconfigure UD$Tag -state normal
         $Canvas itemconfigure OP$Tag -state normal
      }
   }
}

#----------------------------------------------------------------------------
# Nom      : <Shape::Full>
# Creation : Avril 2008- J.P. Gauthier - CMC/CMOE
#
# But      : Effectuer le "scaling" plein ecran/widget de la primitive
#
# Parametres :
#  <Canvas>  : Identificateur du canvas
#  <Tag>     : Tag des objets
#  <args>    : Commande de redimentionnement
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc Shape::Full { Canvas Tag args } {
   variable Data

   if { [llength [set xy [eval $args]]] } {
      set X [lindex $xy 0]
      set Y [lindex $xy 1]
      $Canvas coords BS$Tag $X $Y
      $Canvas coords BF$Tag [expr $X-11] $Y
      $Canvas coords BO$Tag [expr $X-22] $Y
      $Canvas coords BD$Tag $X [lindex [$Canvas coords $Tag] 1]
   }
}

#----------------------------------------------------------------------------
# Nom      : <Shape::Move>
# Creation : Janvier 2002 - J.P. Gauthier - CMC/CMOE
#
# But      : Effectuer le deplacement de la primitive
#
# Parametres :
#  <Canvas>  : Identificateur du canvas
#  <Tags>    : Tag des objets
#  <X>       : Coorconnee X du coin inferieur droit
#  <Y>       : Coorconnee Y du coin inferieur droit
#  <Direct>  : Utilise la coordonnes directment comme translation
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc Shape::Move { Canvas Tags X Y { Direct False } } {
   variable Data

   if { $Direct } {
      set dx $X
      set dy $Y
   } else {
      set X [$Canvas canvasx $X $Page::Param(Snap)]
      set Y [$Canvas canvasy $Y $Page::Param(Snap)]

      set dx [expr $X-$Data(X0)]
      set dy [expr $Y-$Data(Y0)]

      set Data(X0) $X
      set Data(Y0) $Y

   }
   foreach tag $Tags {
      $Canvas move $tag   $dx $dy
      $Canvas move BS$tag $dx $dy
      $Canvas move BF$tag $dx $dy
      $Canvas move BO$tag $dx $dy
      $Canvas move BD$tag $dx $dy
      $Canvas move BM$tag $dx $dy
   }

}

#----------------------------------------------------------------------------
# Nom      : <Shape::Scale>
# Creation : Janvier 2002 - J.P. Gauthier - CMC/CMOE
#
# But      : Effectuer le "scaling" de la primitive
#
# Parametres :
#  <Canvas>  : Identificateur du canvas
#  <Tag>     : Tag des objets
#  <X>       : Coorconnee X du coin inferieur droit
#  <Y>       : Coorconnee Y du coin inferieur droit
#  <args>    : Commande de redimentionnement
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc Shape::Scale { Canvas Tag X Y args } {
   variable Data

   set x [winfo rootx $Canvas]
   set y [winfo rooty $Canvas]

   if { $X<=$x || $Y<=$y || $X>=[expr [winfo width $Canvas]+$x] || $Y>=[expr [winfo height $Canvas]+$y] } {
      return
   }

   set X [$Canvas canvasx [expr $X-$x] $Page::Param(Snap)]
   set Y [$Canvas canvasy [expr $Y-$y] $Page::Param(Snap)]

   if { [eval $args $X $Y] } {
      $Canvas coords BS$Tag $X $Y
      $Canvas coords BF$Tag [expr $X-11] $Y
      $Canvas coords BM$Tag [expr $X-11] $Y
      $Canvas coords BO$Tag [expr $X-22] $Y
      $Canvas coords BD$Tag $X [lindex [$Canvas coords $Tag] 1]
   }
}

#----------------------------------------------------------------------------
# Nom      : <Shape::Set>
# Creation : Janvier 2002 - J.P. Gauthier - CMC/CMOE
#
# But      : Initialiser la position de reference
#
# Parametres :
#  <Canvas>  : Identificateur du canvas
#  <Tag>     : Tag des objets
#  <X>       : Coorconnee X du coin inferieur droit
#  <Y>       : Coorconnee Y du coin inferieur droit
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc Shape::Set { Canvas Tag X Y } {
   variable Data

   set Data(X0) [$Canvas canvasx $X $Page::Param(Snap)]
   set Data(Y0) [$Canvas canvasy $Y $Page::Param(Snap)]

   glrender -xexpose -1

   if { $Data(Blend) } {
      catch { set Data(Alpha)    [lindex [$Canvas itemconfigure ${Tag} -transparency] end] }
      catch { set Data(AlphaTag) [lindex [$Canvas itemconfigure ${Tag}ALPHA -transparency] end] }
      catch { $Canvas itemconfigure ${Tag} -transparency 50 }
   }
}

#----------------------------------------------------------------------------
# Nom      : <Shape::UnSet>
# Creation : Janvier 2002 - J.P. Gauthier - CMC/CMOE
#
# But      : Initialiser la position de reference
#
# Parametres :
#  <Canvas>  : Identificateur du canvas
#  <Tag>     : Tag des objets
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc Shape::UnSet { Canvas Tag } {
   variable Data

   set Data(X0) 0
   set Data(Y0) 0

   glrender -xexpose 1

   if { $Data(Blend) } {
      catch { $Canvas itemconfigure ${Tag} -transparency $Data(Alpha) }
      catch { $Canvas itemconfigure ${Tag}ALPHA -transparency $Data(AlphaTag) }
   }
}

#----------------------------------------------------------------------------
# Nom      : <Shape::UnBind>
# Creation : Decembre 2000 - J.P. Gauthier - CMC/CMOE
#
# But      : Supprimer les bindings de redimentionnement d'items
#
# Parametres :
#  <Canvas>  : Identificateur du canvas
#  <Tags>    : Tags des objets
#
# Retour:
#
# Remarques :
#    - On detruit les "bindings", l'item de canvas et le widget
#
#----------------------------------------------------------------------------

proc Shape::UnBind { Canvas Tag } {

   $Canvas delete BS$Tag BF$Tag BD$Tag BO$Tag
   catch { destroy $Canvas.bs$Tag }
   catch { destroy $Canvas.bf$Tag }
   catch { destroy $Canvas.bd$Tag }
   catch { destroy $Canvas.bo$Tag $Canvas.bo$Tag.menu }
}

#----------------------------------------------------------------------------
# Nom      : <Shape::DrawVBAR>
# Creation : Octobre 1998 - J.P. Gauthier - CMC/CMOE
#
# But      : Dessine la forme Barre Verticale
#
# Parametres :
#  <Canvas>  : Identificateur du canvas
#  <Pixel>   : Position de la forme
#  <Tags>    : Tags a appliquer a cette forme
#  <Color>   : Liste des couleurs avant et arriere pour cette forme
#  <Size>    : Grandeur de l'incone (Unite en pixel)
#  <Fill>    : Remplie ou non
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc Shape::DrawNONE { Canvas Pixel Tags Color Size Fill } {
}

proc Shape::DrawVBAR { Canvas Pixel Tags Color Size Fill } {

   if { $Fill } {
      set fillcolor $Color
      set Tags "$Tags Fill$Color"
   } else {
      set fillcolor white
   }

   $Canvas create rectangle [expr [lindex $Pixel 0] - $Size] [expr [lindex $Pixel 1] - 2 * $Size] \
      [expr [lindex $Pixel 0] + $Size] [expr [lindex $Pixel 1] + 2 * $Size] \
      -outline $Color -fill $fillcolor -width 1 -tag $Tags
}

#----------------------------------------------------------------------------
# Nom      : <Shape::DrawHBAR>
# Creation : Octobre 1998 - J.P. Gauthier - CMC/CMOE
#
# But      : Dessine la forme Barre Horizontale
#
# Parametres :
#  <Canvas>  : Identificateur du canvas
#  <Pixel>   : Position de la forme
#  <Tags>    : Tags a appliquer a cette forme
#  <Color>   : Liste des couleurs avant et arriere pour cette forme
#  <Size>    : Grandeur de l'incone (Unite en pixel)
#  <Fill>    : Remplie ou non
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc Shape::DrawHBAR { Canvas Pixel Tags Color Size Fill } {

   if { $Fill } {
      set fillcolor $Color
      set Tags "$Tags Fill$Color"
   } else {
      set fillcolor white
   }

   $Canvas create rectangle [expr [lindex $Pixel 0] - 2 * $Size] [expr [lindex $Pixel 1] - $Size] \
      [expr [lindex $Pixel 0] + 2 * $Size] [expr [lindex $Pixel 1] + $Size] \
      -outline $Color -fill $fillcolor -width 1 -tag $Tags
}

#----------------------------------------------------------------------------
# Nom      : <Shape::DrawCIRCLE>
# Creation : Octobre 1998 - J.P. Gauthier - CMC/CMOE
#
# But      : Dessine la forme Circle
#
# Parametres :
#  <Canvas>  : Identificateur du canvas
#  <Pixel>   : Position de la forme
#  <Tags>    : Tags a appliquer a cette forme
#  <Color>   : Liste des couleurs avant et arriere pour cette forme
#  <Size>    : Grandeur de l'incone (Unite en pixel)
#  <Fill>    : Remplie ou non
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc Shape::DrawCIRCLE { Canvas Pixel Tags Color Size Fill } {

   if { $Fill } {
      set fillcolor $Color
      set Tags "$Tags Fill$Color"
   } else {
      set fillcolor white
   }

   $Canvas create oval [expr [lindex $Pixel 0] - $Size] [expr [lindex $Pixel 1] - $Size] \
      [expr [lindex $Pixel 0] + $Size] [expr [lindex $Pixel 1] + $Size] \
      -outline $Color -fill $fillcolor -width 1 -tag $Tags
}

#----------------------------------------------------------------------------
# Nom      : <Shape::DrawLOZENGE>
# Creation : Octobre 1998 - J.P. Gauthier - CMC/CMOE
#
# But      : Dessine la forme du Losange
#
# Parametres :
#  <Canvas>  : Identificateur du canvas
#  <Pixel>   : Position de la forme
#  <Tags>    : Tags a appliquer a cette forme
#  <Color>   : Liste des couleurs avant et arriere pour cette forme
#  <Size>    : Grandeur de l'incone (Unite en pixel)
#  <Fill>    : Remplie ou non
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc Shape::DrawLOZENGE { Canvas Pixel Tags Color Size Fill } {

   if { $Fill } {
      set fillcolor $Color
      set Tags "$Tags Fill$Color"
   } else {
      set fillcolor white
   }

   $Canvas create polygon [expr [lindex $Pixel 0] - $Size] [lindex $Pixel 1] \
      [lindex $Pixel 0] [expr [lindex $Pixel 1] + $Size] \
      [expr [lindex $Pixel 0] + $Size] [lindex $Pixel 1] \
      [lindex $Pixel 0] [expr [lindex $Pixel 1] - $Size] \
      -fill $fillcolor -outline $Color -width 1 -tag $Tags
}

#----------------------------------------------------------------------------
# Nom      : <Shape::DrawSable>
# Creation : Octobre 1998 - J.P. Gauthier - CMC/CMOE
#
# But      : Dessine la forme du sablier
#
# Parametres :
#  <Canvas>  : Identificateur du canvas
#  <Pixel>   : Position de la forme
#  <Tags>    : Tags a appliquer a cette forme
#  <Color>   : Liste des couleurs avant et arriere pour cette forme
#  <Size>    : Grandeur de l'incone (Unite en pixel)
#  <Fill>    : Remplie ou non
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc Shape::DrawSAND { Canvas Pixel Tags Color Size Fill } {

   if { $Fill } {
      set fillcolor $Color
      set Tags "$Tags Fill$Color"
   } else {
      set fillcolor white
   }

   $Canvas create polygon [lindex $Pixel 0] [lindex $Pixel 1] \
      [expr [lindex $Pixel 0] - $Size] [expr [lindex $Pixel 1] - $Size] \
      [expr [lindex $Pixel 0] + $Size] [expr [lindex $Pixel 1] - $Size] \
      -fill $fillcolor -outline $Color -width 1 -tag $Tags
   $Canvas create polygon [lindex $Pixel 0] [lindex $Pixel 1] \
      [expr [lindex $Pixel 0] + $Size] [expr [lindex $Pixel 1] + $Size] \
      [expr [lindex $Pixel 0] - $Size] [expr [lindex $Pixel 1] + $Size] \
      -fill $fillcolor -outline $Color -width 1 -tag $Tags
}

#----------------------------------------------------------------------------
# Nom      : <Shape::DrawSQUARE>
# Creation : Octobre 1998 - J.P. Gauthier - CMC/CMOE
#
# But      : Dessine la forme Square
#
# Parametres :
#  <Canvas>  : Identificateur du canvas
#  <Pixel>   : Position de la forme
#  <Tags>    : Tags a appliquer a cette forme
#  <Color>   : Liste des couleurs avant et arriere pour cette forme
#  <Size>    : Grandeur de l'incone (Unite en pixel)
#  <Fill>    : Remplie ou non
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc Shape::DrawSQUARE { Canvas Pixel Tags Color Size Fill } {

   if { $Fill } {
      set fillcolor $Color
      set Tags "$Tags Fill$Color"
   } else {
      set fillcolor white
   }

   $Canvas create rectangle [expr [lindex $Pixel 0] - $Size] [expr [lindex $Pixel 1] - $Size] \
      [expr [lindex $Pixel 0] + $Size] [expr [lindex $Pixel 1] + $Size] \
      -outline $Color -fill $fillcolor -width 1 -tag $Tags
}

#----------------------------------------------------------------------------
# Nom      : <Shape::DrawTRIANGLE>
# Creation : Octobre 1998 - J.P. Gauthier - CMC/CMOE
#
# But      : Dessine la forme Triangle
#
# Parametres :
#  <Canvas>  : Identificateur du canvas
#  <Pixel>   : Position de la forme
#  <Tags>    : Tags a appliquer a cette forme
#  <Color>   : Liste des couleurs avant et arriere pour cette forme
#  <Size>    : Grandeur de l'incone (Unite en pixel)
#  <Fill>    : Remplie ou non
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc Shape::DrawTRIANGLE { Canvas Pixel Tags Color Size Fill } {

   if { $Fill } {
      set fillcolor $Color
      set Tags "$Tags Fill$Color"
   } else {
      set fillcolor white
   }

   $Canvas create polygon [lindex $Pixel 0] [expr [lindex $Pixel 1]-$Size] \
      [expr [lindex $Pixel 0]+$Size] [expr [lindex $Pixel 1]+$Size] \
      [expr [lindex $Pixel 0]-$Size] [expr [lindex $Pixel 1]+$Size] \
      -fill $fillcolor -outline $Color -width 1 -tag $Tags
}

#----------------------------------------------------------------------------
# Nom      : <Shape::DrawPENTAGON>
# Creation : Mai 2008 - J.P. Gauthier - CMC/CMOE
#
# But      : Dessine la forme Pentagone
#
# Parametres :
#  <Canvas>  : Identificateur du canvas
#  <Pixel>   : Position de la forme
#  <Tags>    : Tags a appliquer a cette forme
#  <Color>   : Liste des couleurs avant et arriere pour cette forme
#  <Size>    : Grandeur de l'incone (Unite en pixel)
#  <Fill>    : Remplie ou non
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc Shape::DrawPENTAGON { Canvas Pixel Tags Color Size Fill } {

   if { $Fill } {
      set fillcolor $Color
      set Tags "$Tags Fill$Color"
   } else {
      set fillcolor white
   }

   set s2 [expr $Size/2.0]
   set s1 [expr $Size/4.0]

   $Canvas create polygon [lindex $Pixel 0] [expr [lindex $Pixel 1]-$Size] \
      [expr [lindex $Pixel 0]-$Size] [expr [lindex $Pixel 1]-$s1] \
      [expr [lindex $Pixel 0]-$s2] [expr [lindex $Pixel 1]+$Size] \
      [expr [lindex $Pixel 0]+$s2] [expr [lindex $Pixel 1]+$Size] \
      [expr [lindex $Pixel 0]+$Size] [expr [lindex $Pixel 1]-$s1] \
      -fill $fillcolor -outline $Color -width 1 -tag $Tags
}

#----------------------------------------------------------------------------
# Nom      : <Shape::DrawHEXAGON>
# Creation : Mai 2008 - J.P. Gauthier - CMC/CMOE
#
# But      : Dessine la forme Hexagone
#
# Parametres :
#  <Canvas>  : Identificateur du canvas
#  <Pixel>   : Position de la forme
#  <Tags>    : Tags a appliquer a cette forme
#  <Color>   : Liste des couleurs avant et arriere pour cette forme
#  <Size>    : Grandeur de l'incone (Unite en pixel)
#  <Fill>    : Remplie ou non
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc Shape::DrawHEXAGON { Canvas Pixel Tags Color Size Fill } {

   if { $Fill } {
      set fillcolor $Color
      set Tags "$Tags Fill$Color"
   } else {
      set fillcolor white
   }

   set s2 [expr $Size/2.0]

   $Canvas create polygon [expr [lindex $Pixel 0]-$Size] [lindex $Pixel 1] \
      [expr [lindex $Pixel 0]-$s2] [expr [lindex $Pixel 1]+$Size] \
      [expr [lindex $Pixel 0]+$s2] [expr [lindex $Pixel 1]+$Size] \
      [expr [lindex $Pixel 0]+$Size] [expr [lindex $Pixel 1]] \
      [expr [lindex $Pixel 0]+$s2] [expr [lindex $Pixel 1]-$Size] \
      [expr [lindex $Pixel 0]-$s2] [expr [lindex $Pixel 1]-$Size] \
      -fill $fillcolor -outline $Color -width 1 -tag $Tags
}


#----------------------------------------------------------------------------
# Nom      : <Shape::DrawX>
# Creation : Octobre 1998 - J.P. Gauthier - CMC/CMOE
#
# But      : Dessine la forme d'un X
#
# Parametres :
#  <Canvas>  : Identificateur du canvas
#  <Pixel>   : Position de la forme
#  <Tags>    : Tags a appliquer a cette forme
#  <Color>   : Liste des couleurs avant et arriere pour cette forme
#  <Size>    : Grandeur de l'incone (Unite en pixel)
#  <Fill>    : Remplie ou non
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc Shape::DrawX { Canvas Pixel Tags Color Size Fill } {

   set Tags "$Tags Out$Color"
   $Canvas create line [expr [lindex $Pixel 0] - $Size] [expr [lindex $Pixel 1] - $Size] \
      [expr [lindex $Pixel 0] + $Size] [expr [lindex $Pixel 1] + $Size] \
      -fill $Color -width 1 -tag $Tags
   $Canvas create line [expr [lindex $Pixel 0] + $Size] [expr [lindex $Pixel 1] - $Size] \
      [expr [lindex $Pixel 0] - $Size] [expr [lindex $Pixel 1] + $Size] \
      -fill $Color -width 1 -tag $Tags
}

#----------------------------------------------------------------------------
# Nom      : <Shape::DrawSTAR>
# Creation : Octobre 1998 - J.P. Gauthier - CMC/CMOE
#
# But      : Dessine la forme d'un X
#
# Parametres :
#  <Canvas>  : Identificateur du canvas
#  <Pixel>   : Position de la forme
#  <Tags>    : Tags a appliquer a cette forme
#  <Color>   : Liste des couleurs avant et arriere pour cette forme
#  <Size>    : Grandeur de l'incone (Unite en pixel)
#  <Fill>    : Remplie ou non
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc Shape::DrawSTAR { Canvas Pixel Tags Color Size Fill } {

   set Tags "$Tags Out$Color"

   $Canvas create line [expr [lindex $Pixel 0] -$Size+1] [expr [lindex $Pixel 1] - $Size+1] \
      [expr [lindex $Pixel 0] + $Size-1] [expr [lindex $Pixel 1] + $Size-1] \
      -fill $Color -width 1 -tag $Tags
   $Canvas create line [expr [lindex $Pixel 0] + $Size-1] [expr [lindex $Pixel 1] - $Size+1] \
      [expr [lindex $Pixel 0] - $Size+1] [expr [lindex $Pixel 1] + $Size-1] \
      -fill $Color -width 1 -tag $Tags
   $Canvas create line [expr [lindex $Pixel 0] - $Size] [lindex $Pixel 1] \
      [expr [lindex $Pixel 0] + $Size] [lindex $Pixel 1] \
      -fill $Color -width 1 -tag $Tags
   $Canvas create line [lindex $Pixel 0] [expr [lindex $Pixel 1] - $Size] \
      [lindex $Pixel 0] [expr [lindex $Pixel 1] + $Size] \
      -fill $Color -width 1 -tag $Tags
}

#----------------------------------------------------------------------------
# Nom      : <Shape::DrawLIGHTNING>
# Creation : Octobre 1998 - J.P. Gauthier - CMC/CMOE
#
# But      : Dessine la forme d'un X
#
# Parametres :
#  <Canvas>  : Identificateur du canvas
#  <Pixel>   : Position de la forme
#  <Tags>    : Tags a appliquer a cette forme
#  <Color>   : Liste des couleurs avant et arriere pour cette forme
#  <Size>    : Grandeur de l'incone (Unite en pixel)
#  <Fill>    : Remplie ou non
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc Shape::DrawLIGHTNING { Canvas Pixel Tags Color Size Fill } {

   if { $Fill } {
      set fillcolor $Color
      set Tags "$Tags Fill$Color"
   } else {
      set fillcolor white
   }

   set s2 [expr $Size/2.0]

   set id [$Canvas create polygon [expr [lindex $Pixel 0]+1.0] [expr [lindex $Pixel 1]-1.0] \
      [expr [lindex $Pixel 0]-0.6] [expr [lindex $Pixel 1]-0.4] \
      [lindex $Pixel 0] [expr [lindex $Pixel 1]+0.2] \
      [expr [lindex $Pixel 0]-1.0] [expr [lindex $Pixel 1]+1.0] \
      [expr [lindex $Pixel 0]+0.6] [expr [lindex $Pixel 1]+0.4] \
      [lindex $Pixel 0] [expr [lindex $Pixel 1]-0.2] \
      -fill $fillcolor -outline $Color -width 1 -tag $Tags]

   $Canvas scale $id [lindex $Pixel 0] [lindex $Pixel 1] $Size $Size
}

#----------------------------------------------------------------------------
# Nom      : <Shape::Draw+>
# Creation : Octobre 1998 - J.P. Gauthier - CMC/CMOE
#
# But      : Dessine la forme d'un +
#
# Parametres :
#  <Canvas>  : Identificateur du canvas
#  <Pixel>   : Position de la forme
#  <Tags>    : Tags a appliquer a cette forme
#  <Color>   : Liste des couleurs avant et arriere pour cette forme
#  <Size>    : Grandeur de l'incone (Unite en pixel)
#  <Fill>    : Remplie ou non
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc Shape::Draw+ { Canvas Pixel Tags Color Size Fill } {

   set Tags "$Tags Out$Color"

   $Canvas create line [expr [lindex $Pixel 0] - $Size] [lindex $Pixel 1] \
      [expr [lindex $Pixel 0] + $Size] [lindex $Pixel 1] \
      -fill $Color -width 1 -tag $Tags
   $Canvas create line [lindex $Pixel 0] [expr [lindex $Pixel 1] - $Size] \
      [lindex $Pixel 0] [expr [lindex $Pixel 1] + $Size] \
      -fill $Color -width 1 -tag $Tags
}

#----------------------------------------------------------------------------
# Nom      : <Shape::DrawStringTest>
# Creation : Octobre 1998 - J.P. Gauthier - CMC/CMOE
#
# But      : Dessine la forme Triangle
#
# Parametres :
#  <Canvas>  : Identificateur du canvas
#  <X>       : Position en X de la forme
#  <Y>       : Position en Y de la forme
#  <Factor>  : Facteur de repetition
#  <Tags>    : Tags a appliquer a cette forme
#  <Color>   : Couleur
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc Shape::DrawStringTest { Canvas X Y Factor Tags Color } {
   global GDefs

   for { set i 0 } { $i < $Factor } { incr i } {

      $Canvas create bitmap [expr $X + 20 + $i * 114] [expr $Y - 10 - $i * 97] \
         -bitmap @$GDefs(Dir)/share/bitmap/string_test.xbm \
          -foreground $Color -tag $Tags
   }
}

#----------------------------------------------------------------------------
# Nom      : <Shape::DrawIcoMETF>
# Creation : Juillet 1999 - J.P. Gauthier - CMC/CMOE
#
# But      : Dessine le sigle des cartes des champs meteorologiques
#
# Parametres :
#  <Canvas>  : Identificateur du canvas
#  <Pixel>   : Position de la forme
#  <Tags>    : Tags a appliquer a cette forme
#  <Color>   : Liste des couleurs avant et arriere pour cette forme
#  <Size>    : Grandeur de l'incone (Unite en pixel)
#  <Fill>    : Remplie ou non
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc Shape::DrawIcoMETF { Canvas Pixel Tags Color Size Fill } {

   if { $Fill } {
      set fillcolor $Color
      set Tags "$Tags Fill$Color"
   } else {
      set fillcolor ""
   }

   $Canvas create arc [expr [lindex $Pixel 0] - $Size] [expr [lindex $Pixel 1] - $Size] \
      [expr [lindex $Pixel 0] + $Size] [expr [lindex $Pixel 1] + $Size] \
      -fill $fillcolor -outline $Color -width 2 -tag $Tags -extent 359 -style arc
   $Canvas create line [expr [lindex $Pixel 0] - $Size - 2] [lindex $Pixel 1] \
      [expr [lindex $Pixel 0] + $Size + 2] [lindex $Pixel 1] \
      -fill $Color -width 1 -tag $Tags
   $Canvas create line [lindex $Pixel 0] [expr [lindex $Pixel 1] - $Size - 2] \
      [lindex $Pixel 0] [expr [lindex $Pixel 1] + $Size + 2] \
      -fill $Color -width 1 -tag $Tags
}

#----------------------------------------------------------------------------
# Nom      : <Shape::DrawIcoVAAC>
# Creation : Octobre 1999 - J.P. Gauthier - CMC/CMOE
#
# But      : Dessine le sigle VAAC
#
# Parametres :
#  <Canvas>  : Identificateur du canvas
#  <Pixel>   : Position de la forme
#  <Tags>    : Tags a appliquer a cette forme
#  <Color>   : Liste des couleurs avant et arriere pour cette forme
#  <Size>    : Grandeur de l'incone (Unite en pixel)
#  <Fill>    : Remplie ou non
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc Shape::DrawIcoVAAC { Canvas Pixel Tags Color Size Fill } {

   if { $Fill } {
      set fillcolor $Color
      set Tags "$Tags Fill$Color"
   } else {
      set fillcolor ""
   }

   $Canvas create polygon [lindex $Pixel 0] [expr [lindex $Pixel 1] - 2 * $Size] \
      [expr [lindex $Pixel 0] + $Size] [lindex $Pixel 1] \
      [expr [lindex $Pixel 0] - $Size] [lindex $Pixel 1] \
      [lindex $Pixel 0] [expr [lindex $Pixel 1] - 2 * $Size] \
      -fill $fillcolor -outline $Color -width 2 -tag $Tags
   $Canvas create line [lindex $Pixel 0] [expr [lindex $Pixel 1] - $Size/2] [lindex $Pixel 0] [expr [lindex $Pixel 1] + $Size/2] \
      -fill $Color -width 2 -tag $Tags
}
