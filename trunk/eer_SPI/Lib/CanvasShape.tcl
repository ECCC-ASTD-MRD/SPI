#===============================================================================
# Environnement Canada
# Centre Meteorologique Canadian
# 2100 Trans-Canadienne
# Dorval, Quebec
#
# Projet    : Librairie de fonctions pour dessiner des formes dans un canvas
# Fichier   : CanvasShape.tcl
# Version   : 1.3 ($Revision: 1.7 $)
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
#    CVText::Init          { Canvas }
#    CVText::Focus         { Canvas X Y }
#    CVText::Copy          { Canvas }
#    CVText::BackSpace     { Canvas }
#    CVText::Delete        { Canvas }
#    CVText::Drag          { Canvas X Y }
#    CVText::Erase         { Canvas }
#    CVText::Hit           { Canvas X Y { select 1 } }
#    CVText::Insert        { Canvas Char }
#    CVText::Move          { Canvas Incr }
#    CVText::MoveEnd       { Canvas Pos }
#    CVText::Paste         { Canvas { X {} } { Y {} } }
#    CVText::Select        { Canvas }
#
#    Shape::BindMove       { Canvas Tags args }
#    Shape::BindScale      { Canvas Tag X1 Y1 Command }
#    Shape::UnBindScale    { Canvas Tag }
#    Shape::Move           { Canvas Tags X Y }
#    Shape::Scale          { Canvas Tag X Y args }
#    Shape::Set            { Canvas Tag X Y }
#    Shape::UnSet          { Canvas Tag }
#
#    Shape::DrawHBar       { Canvas Pixel Tags Color Size Fill }
#    Shape::DrawVBar       { Canvas Pixel Tags Color Size Fill }
#    Shape::DrawCircle     { Canvas Pixel Tags Color Size Fill }
#    Shape::DrawLosange    { Canvas Pixel Tags Color Size Fill
#    Shape::DrawSable      { Canvas Pixel Tags Color Size Fill }
#    Shape::DrawSquare     { Canvas Pixel Tags Color Size Fill }
#    Shape::DrawStar       { Canvas Pixel Tags Color Size Fill }
#    Shape::DrawTriangle   { Canvas Pixel Tags Color Size Fill }
#    Shape::DrawX          { Canvas Pixel Tags Color Size Fill }
#    Shape::Draw+          { Canvas Pixel Tags Color Size Fill }
#    Shape::DrawIcoMETF    { Canvas Pixel Tags Color Size Fill }
#    Shape::DrawIcoVAAC    { Canvas Pixel Tags Color Size Fill }
#    Shape::DrawStringTest { Canvas X Y Factor Tags Color }
#
# Remarques :
#   -Tous les points sont passer sous forme de liste { X Y }
#
# Modification:
#
#   Nom         : Jean-Philippe Gauthier
#   Date        : Mai 2006
#   Description : L'ajustement de l'horloge se fait maintenant en secondes
#
#===============================================================================

package provide CanvasShape 1.3

proc IdCanvasShape { Show } {
   global GDefs

   if { $Show } {
      puts "(INFO) Loading Standard CMC/CMOE Canvas Package CanvasShape Version 1.3"
   }

   image create photo COMPASSFRAME   -file $GDefs(Dir)/Resources/Image/System/CompassFrame.gif
   image create photo COMPASSDIR     -file $GDefs(Dir)/Resources/Image/System/CompassDir.gif
   image create photo COMPASSDIST    -file $GDefs(Dir)/Resources/Image/System/CompassDist.gif
   image create photo COMPASSHEADING -file $GDefs(Dir)/Resources/Image/System/CompassHeading.gif
   image create photo CLOCKFRAME     -file $GDefs(Dir)/Resources/Image/System/ClockFrame.gif
   image create photo CLOCKTIME      -file $GDefs(Dir)/Resources/Image/System/ClockTime.gif
   image create photo CLOCKDATE      -file $GDefs(Dir)/Resources/Image/System/ClockDate.gif
}

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
# Modifications :
#
#    Nom         : -
#    Date        : -
#    Description : -
#----------------------------------------------------------------------------

proc CVCompass::Create { Frame X Y } {
   global   GDefs
   variable Data

   set Data(Size) 25
   set canvas $Frame.page.canvas

   $canvas create line $X $Y $X $Y -width 0 -fill white -tag "CVCOMP CVCOMPLOC"

   $canvas create image $X $Y -image COMPASSHEADING -tags "CVCOMP"
   $canvas create image $X $Y -image COMPASSFRAME -tags "CVCOMP"

   $canvas create image $X [expr $Y+50] -image CLOCKTIME -tags "CVCOMP"
   $canvas create image $X [expr $Y+72] -image COMPASSDIST -tags "CVCOMP"

   $canvas create text $X $Y -tags "CVCOMP CVHEAD" -font XFont12 -fill black
   $canvas create text $X [expr $Y+50] -tags "CVCOMP CVCOMPANGLE" -font XFont12 -fill black
   $canvas create text $X [expr $Y+72] -tags "CVCOMP CVCOMPDIST" -font XFont12 -fill black

   $canvas create image -999 -999 -image COMPASSDIST -tags "CVCOMP CVCOMPSPD"
   $canvas create text -999 -999 -tags "CVCOMP CVCOMPSPD CVCOMPSPDT" -font XFont12 -fill black

   $canvas create image $X [expr $Y-25] -image COMPASSDIR -tags "CVCOMP CVCOMPN"
   $canvas create image $X [expr $Y+25] -image COMPASSDIR -tags "CVCOMP CVCOMPS"
   $canvas create image [expr $X-25] $Y -image COMPASSDIR -tags "CVCOMP CVCOMPW"
   $canvas create image [expr $X+25] $Y -image COMPASSDIR -tags "CVCOMP CVCOMPE"

   $canvas create text $X [expr $Y-25] -text N -tags "CVCOMPNT" -font XFont12
   $canvas create text $X [expr $Y+25] -text S -tags "CVCOMPST" -font XFont12
   $canvas create text [expr $X-25] $Y -text [lindex { O W } $GDefs(Lang)] -tags "CVCOMPWT" -font XFont12
   $canvas create text [expr $X+25] $Y -text E -tags "CVCOMPET" -font XFont12

   Shape::BindMove $canvas { CVCOMP CVCOMPNT CVCOMPST CVCOMPWT CVCOMPET }

   foreach tag { CVCOMPNT CVCOMPST CVCOMPWT CVCOMPET } theta { 0 180 90 270 } {
      $canvas bind $tag <Enter>     "$canvas configure -cursor hand1"
      $canvas bind $tag <Leave>     "$canvas configure -cursor left_ptr"
      $canvas bind $tag <B1-Motion> "CVCompass::Rotate $Frame %x %y $theta"
   }
}

#----------------------------------------------------------------------------
# Nom      : <CVCompass::Rotate>
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
# Modifications :
#
#    Nom         : -
#    Date        : -
#    Description : -
#----------------------------------------------------------------------------

proc CVCompass::Rotate { Frame X Y Theta } {

   set xy [$Frame.page.canvas coords CVCOMPLOC]

   if { [set vp [lindex [Page::Registered $Frame Viewport] 0]]!="" } {
      set bearing [expr -atan2($Y-[lindex $xy 1],$X-[lindex $xy 0])*57.2957795-90.0-$Theta]

      upvar #0 ProjCam::Data${Frame}::Cam  cam
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
# Modifications :
#
#    Nom         : -
#    Date        : -
#    Description : -
#----------------------------------------------------------------------------

proc CVCompass::Destroy { Frame } {

   $Frame.page.canvas delete CVCOMP CVCOMPNT CVCOMPST CVCOMPWT CVCOMPET
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
# Modifications :
#
#    Nom         : -
#    Date        : -
#    Description : -
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
   $canvas itemconfigure CVHEAD -text [format "%03.0f" $Bearing]

   if { $Speed!=0.0 } {
      $canvas coords CVCOMPSPD $x0 [expr $y0+94]
      $canvas coords CVCOMPSPDT $x0 [expr $y0+94]
      $canvas itemconfigure CVCOMPSPDT -text [format "%.0f m/s" [expr abs($Speed*1000)]]
   } else {
      $canvas coords CVCOMPSPD -999 -999
      $canvas coords CVCOMPSPDT -999 -999
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
# Modifications :
#
#   Nom         : -
#   Date        : -
#   Description : -
#
#-------------------------------------------------------------------------------

proc CVCompass::Write { Frame File } {
   variable Data

   puts $File ""
   puts $File "   #----- Affichage du compas"
   set c [$Frame.page.canvas coords CVCOMPLOC]
   puts $File "   CVCompass::Create \$Frame [lindex $c 0] [lindex $c 1]"
}

namespace eval CVText { }

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
# Modifications :
#
#    Nom         : -
#    Date        : -
#    Description : -
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
   $Canvas bind CVTEXT <Key-Right>        { CVText::Move      %W 1 }
   $Canvas bind CVTEXT <Control-f>        { CVText::Move      %W 1 }
   $Canvas bind CVTEXT <Key-Left>         { CVText::Move      %W -1 }
   $Canvas bind CVTEXT <Control-b>        { CVText::Move      %W -1 }
   $Canvas bind CVTEXT <Key-Home>         { CVText::MoveEnd   %W 0 }
   $Canvas bind CVTEXT <Key-End>          { CVText::MoveEnd   %W end }
   $Canvas bind CVTEXT <Any-Key>          { CVText::Insert    %W %A ;}
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
# Modifications :
#
#    Nom         : -
#    Date        : -
#    Description : -
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
# Modifications :
#
#    Nom         : -
#    Date        : -
#    Description : -
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
# Modifications :
#
#    Nom         : -
#    Date        : -
#    Description : -
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
# Modifications :
#
#    Nom         : -
#    Date        : -
#    Description : -
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
# Modifications :
#
#    Nom         : -
#    Date        : -
#    Description : -
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
# Modifications :
#
#    Nom         : -
#    Date        : -
#    Description : -
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
# Modifications :
#
#    Nom         : -
#    Date        : -
#    Description : -
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
# Modifications :
#
#    Nom         : -
#    Date        : -
#    Description : -
#----------------------------------------------------------------------------

proc CVText::Insert { Canvas Char } {
   $Canvas insert [$Canvas focus] insert $Char
}

#----------------------------------------------------------------------------
# Nom      : <CVText::Move>
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
# Modifications :
#
#    Nom         : -
#    Date        : -
#    Description : -
#----------------------------------------------------------------------------

proc CVText::Move { Canvas Incr } {

   set focus [$Canvas focus]
   $Canvas icursor $focus [expr [$Canvas index $focus insert]+$Incr]
}

#----------------------------------------------------------------------------
# Nom      : <CVText::MoveEnd>
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
# Modifications :
#
#    Nom         : -
#    Date        : -
#    Description : -
#----------------------------------------------------------------------------

proc CVText::MoveEnd { Canvas Pos } {

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
# Modifications :
#
#    Nom         : -
#    Date        : -
#    Description : -
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
# Modifications :
#
#    Nom         : -
#    Date        : -
#    Description : -
#----------------------------------------------------------------------------

proc CVText::Select { Canvas } {

   $Canvas select from current 0
   $Canvas select to current end
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
# Modifications :
#
#    Nom         : -
#    Date        : -
#    Description : -
#----------------------------------------------------------------------------
namespace eval CVClock { }

proc CVClock::Create { Frame X Y } {
   global GDefs
   variable Data

   set Data(Size) 23
   set Data(TimeWidth) 30
   set Data(DateWidth) 120

   set Data(Time) True
   set Data(Date) True

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

   Shape::BindMove $canvas CVCLOCK
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
# Modifications :
#
#    Nom         : -
#    Date        : -
#    Description : -
#----------------------------------------------------------------------------

proc CVClock::Update { Frame Data } {
   global GDefs

   if { ![CVClock::Exist $Frame] } {
      return
   }

   if { $Data!="" } {
      if { [fstdfield is $Data] } {
         CVClock::Time $Frame [fstdstamp toseconds [fstdfield define $Data -DATEV]] -1
       } else {
         CVClock::Time $Frame 0 -1
      }
   }
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
# Modifications :
#
#    Nom         : -
#    Date        : -
#    Description : -
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
# Modifications :
#
#    Nom         : -
#    Date        : -
#    Description : -
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
# Modifications :
#
#    Nom         : -
#    Date        : -
#    Description : -
#----------------------------------------------------------------------------

proc CVClock::Time { Frame Sec Total } {
   global GDefs
   variable Data

   set canvas $Frame.page.canvas

   if { ![llength [$canvas find withtag CVCLOCK]] } {
      return
   }

   #----- Positionnes lq quartier total

   if { $Total>-1 } {
      if { $Total>=100 } {
         set extent -359
      } else {
         set extent [expr -$Total*3.6]
      }
      $canvas itemconf CVCLOCKTOTAL -extent $extent
   }

   if { $Sec!="-1" } {

      set hour [clock format $Sec -format "%k" -gmt true]
      set min  [clock format $Sec -format "%M" -gmt true]
      set jour [DateStuff::StringDay   [clock format $Sec -format "%w" -gmt true] $GDefs(Lang)]
      set mois [DateStuff::StringMonth [clock format $Sec -format "%m" -gmt true] $GDefs(Lang)]

      #----- Positionner l'aiguille des heures

      set theta [expr (($hour>=12?$hour-12:$hour)-3)*0.52359876]
      set co [$canvas coords CVCLOCKHOUR]
      set x0 [lindex $co 0]
      set y0 [lindex $co 1]

      set x [expr $x0+($Data(Size)-6)*cos($theta)]
      set y [expr $y0+($Data(Size)-6)*sin($theta)]

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

      set x [expr $x0+$Data(Size)*cos($theta)]
      set y [expr $y0+$Data(Size)*sin($theta)]

      $canvas coords CVCLOCKMINUTE $x0 $y0  $x $y

      $canvas itemconf CVCLOCKDATE -text [clock format $Sec -format "$jour %d $mois %Y" -gmt true]
      $canvas itemconf CVCLOCKTIME -text "${hour}:${min}Z"
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
# Modifications :
#
#   Nom         : -
#   Date        : -
#   Description : -
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
# Modifications :
#
#    Nom         : -
#    Date        : -
#    Description : -
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

   Shape::BindMove $canvas CVLEGEND
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
# Modifications :
#
#    Nom         : -
#    Date        : -
#    Description : -
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
# Modifications :
#
#    Nom         : -
#    Date        : -
#    Description : -
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
# Modifications :
#
#   Nom         : -
#   Date        : -
#   Description : -
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
# Modifications :
#
#    Nom         : -
#    Date        : -
#    Description : -
#----------------------------------------------------------------------------
namespace eval CVScale { } { }

proc CVScale::Create { Frame X Y Size } {
   global GDefs
   variable Data

   set canvas $Frame.page.canvas
   set Data(Size)   $Size

   set x0 [expr $X-$Size/2]
   set y0 [expr $Y]
   set x1 [expr $X+$Size/2]
   set y1 [expr $Y+10]

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
      menubutton $canvas.cvscale -bg $GDefs(ColorFrame) -bitmap @$GDefs(Dir)/Resources/Bitmap/cvmenu.xbm -cursor hand1 -bd 1 \
         -relief raised -menu $canvas.cvscale.menu
      menu $canvas.cvscale.menu -tearoff 0 -bg $GDefs(ColorFrame)
      foreach size {  10000000 1000000 500000 250000 100000 25000 24000 20000 10000 } {
         $canvas.cvscale.menu add radiobutton -label "1:${size}" -variable CVScale::Data(Scale) -value ${size}.0 \
            -command "CVScale::Set $Page::Data(Frame)" -indicatoron false
      }
   }

   $canvas create window [expr $x0-3] [expr $y0-1] -window $canvas.cvscale -anchor se -tags "CVSCALE NOPRINT"

   Shape::BindMove $canvas CVSCALE
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
# Modifications :
#
#    Nom         : -
#    Date        : -
#    Description : -
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
# Modifications :
#
#    Nom         : -
#    Date        : -
#    Description : -
#----------------------------------------------------------------------------

proc CVScale::Set { Frame } {
   variable Data

   set dxy    [expr 2.8e-4*double($Data(Scale))]
   set factor [$Viewport::Data(VP) -distpix $dxy]

   ProjCam::ZoomIn $Frame $Frame $Viewport::Data(VP) $factor
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
# Modifications :
#
#   Nom         : -
#   Date        : -
#   Description : -
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
# Modifications :
#
#    Nom         : -
#    Date        : -
#    Description : -
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
   $canvas raise CVSCALE
}

namespace eval Shape {
   variable Data

   set Data(X0)    0
   set Data(Y0)    0
   set Data(Blend) 1
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
#  <args>    : Commande a executer
#
# Retour:
#
# Remarques :
#
# Modifications :
#
#    Nom         : -
#    Date        : -
#    Description : -
#----------------------------------------------------------------------------

proc Shape::BindMove { Canvas Tags args } {

   set tag [lindex $Tags 0]

   $Canvas bind $tag <Enter>           "$Canvas configure -cursor fleur"
   $Canvas bind $tag <Leave>           "$Canvas configure -cursor left_ptr"
   $Canvas bind $tag <ButtonPress-1>   "Shape::Set $Canvas $tag %X %Y"
   $Canvas bind $tag <ButtonRelease-1> "Shape::UnSet $Canvas $tag"

   if { $args!="" } {
      $Canvas bind $tag <B1-Motion>    "Shape::Move $Canvas \"$Tags\" %X %Y ; $args"
   } else {
      $Canvas bind $tag <B1-Motion>    "Shape::Move $Canvas \"$Tags\" %X %Y"
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
#  <X1>      : Coorconnee X du coin inferieur droit
#  <Y1>      : Coorconnee Y du coin inferieur droit
#  <Command> : Commande de redimentionnement
#
# Retour:
#
# Remarques :
#
# Modifications :
#
#    Nom         : -
#    Date        : -
#    Description : -
#----------------------------------------------------------------------------

proc Shape::BindScale { Canvas Tag X1 Y1 Command } {
   global GDefs

   if { ![winfo exists $Canvas.bs$Tag] } {
      label $Canvas.bs$Tag -bg $GDefs(ColorFrame) -bitmap @$GDefs(Dir)/Resources/Bitmap/cvscale.xbm -bd 0 -cursor sizing -bd 1 -relief raised

      $Canvas create window $X1 $Y1 -window $Canvas.bs$Tag -anchor se -tags "BS$Tag NOPRINT"
   }

#   bind $Canvas.bs$Tag <ButtonPress-1>   "Shape::Set   $Canvas $Tag \[winfo rootx $Canvas\] \[winfo rooty $Canvas\]"
   bind $Canvas.bs$Tag <ButtonPress-1>   "Shape::Set   $Canvas $Tag %X %Y"
   bind $Canvas.bs$Tag <ButtonRelease-1> "Shape::UnSet $Canvas $Tag"
   bind $Canvas.bs$Tag <B1-Motion>       "Shape::Scale $Canvas $Tag %X %Y $Command"
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
#
# Retour:
#
# Remarques :
#
# Modifications :
#
#    Nom         : -
#    Date        : -
#    Description : -
#----------------------------------------------------------------------------

proc Shape::Move { Canvas Tags X Y } {
   variable Data

   set X [$Canvas canvasx $X $Page::Data(Snap)]
   set Y [$Canvas canvasy $Y $Page::Data(Snap)]

   set dx [expr $X-$Data(X0)]
   set dy [expr $Y-$Data(Y0)]

   foreach tag $Tags {
      $Canvas move $tag   $dx $dy
      $Canvas move BS$tag $dx $dy
      $Canvas move BF$tag $dx $dy
   }

   set Data(X0) $X
   set Data(Y0) $Y
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
# Modifications :
#
#    Nom         : -
#    Date        : -
#    Description : -
#----------------------------------------------------------------------------

proc Shape::Scale { Canvas Tag X Y args } {
   variable Data

   set x [winfo rootx $Canvas]
   set y [winfo rooty $Canvas]

   if { $X<=$x || $Y<=$y || $X>=[expr [winfo width $Canvas]+$x] || $Y>=[expr [winfo height $Canvas]+$y] } {
      return
   }

   set X [$Canvas canvasx [expr $X-$x] $Page::Data(Snap)]
   set Y [$Canvas canvasy [expr $Y-$y] $Page::Data(Snap)]

   if { [eval $args $X $Y] } {
      $Canvas coords BS$Tag $X $Y
      $Canvas coords BF$Tag [expr $X-11] $Y
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
# Modifications :
#
#    Nom         : -
#    Date        : -
#    Description : -
#----------------------------------------------------------------------------

proc Shape::Set { Canvas Tag X Y } {
   variable Data

   set Data(X0) [$Canvas canvasx $X $Page::Data(Snap)]
   set Data(Y0) [$Canvas canvasy $Y $Page::Data(Snap)]

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
# Modifications :
#
#    Nom         : -
#    Date        : -
#    Description : -
#----------------------------------------------------------------------------

proc Shape::UnSet { Canvas Tag } {
   variable Data

   glrender -xexpose 1

   if { $Data(Blend) } {
      catch { $Canvas itemconfigure ${Tag} -transparency $Data(Alpha) }
      catch { $Canvas itemconfigure ${Tag}ALPHA -transparency $Data(AlphaTag) }
   }
}

#----------------------------------------------------------------------------
# Nom      : <Shape::UnBindScale>
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
# Modifications :
#
#    Nom         : -
#    Date        : -
#    Description : -
#----------------------------------------------------------------------------

proc Shape::UnBindScale { Canvas Tag } {

   $Canvas delete BS$Tag
   destroy $Canvas.bs$Tag
}

#----------------------------------------------------------------------------
# Nom      : <Shape::DrawVBar>
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
# Modifications :
#
#    Nom         : -
#    Date        : -
#    Description : -
#----------------------------------------------------------------------------

proc Shape::DrawVBar { Canvas Pixel Tags Color Size Fill } {

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
# Nom      : <Shape::DrawHBar>
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
# Modifications :
#
#    Nom         : -
#    Date        : -
#    Description : -
#----------------------------------------------------------------------------

proc Shape::DrawHBar { Canvas Pixel Tags Color Size Fill } {

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
# Nom      : <Shape::DrawCircle>
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
# Modifications :
#
#    Nom         : -
#    Date        : -
#    Description : -
#----------------------------------------------------------------------------

proc Shape::DrawCircle { Canvas Pixel Tags Color Size Fill } {

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
# Nom      : <Shape::DrawLosange>
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
# Modifications :
#
#    Nom         : -
#    Date        : -
#    Description : -
#----------------------------------------------------------------------------

proc Shape::DrawLosange { Canvas Pixel Tags Color Size Fill } {

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
# Modifications :
#
#    Nom         : -
#    Date        : -
#    Description : -
#----------------------------------------------------------------------------

proc Shape::DrawSable { Canvas Pixel Tags Color Size Fill } {

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
# Nom      : <Shape::DrawSquare>
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
# Modifications :
#
#    Nom         : -
#    Date        : -
#    Description : -
#----------------------------------------------------------------------------

proc Shape::DrawSquare { Canvas Pixel Tags Color Size Fill } {

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
# Nom      : <Shape::DrawTriangle>
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
# Modifications :
#
#    Nom         : -
#    Date        : -
#    Description : -
#----------------------------------------------------------------------------

proc Shape::DrawTriangle { Canvas Pixel Tags Color Size Fill } {

   if { $Fill } {
      set fillcolor $Color
      set Tags "$Tags Fill$Color"
   } else {
      set fillcolor white
   }

   $Canvas create polygon [expr [lindex $Pixel 0] - $Size] [lindex $Pixel 1] \
      [expr [lindex $Pixel 0] + $Size] [expr [lindex $Pixel 1] + $Size] \
      [expr [lindex $Pixel 0] + $Size] [expr [lindex $Pixel 1] - $Size] \
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
# Modifications :
#
#    Nom         : -
#    Date        : -
#    Description : -
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
# Nom      : <Shape::DrawStar>
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
# Modifications :
#
#    Nom         : -
#    Date        : -
#    Description : -
#----------------------------------------------------------------------------

proc Shape::DrawStar { Canvas Pixel Tags Color Size Fill } {

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
# Modifications :
#
#    Nom         : -
#    Date        : -
#    Description : -
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
# Modifications :
#
#    Nom         : -
#    Date        : -
#    Description : -
#----------------------------------------------------------------------------

proc Shape::DrawStringTest { Canvas X Y Factor Tags Color } {
   global GDefs

   for { set i 0 } { $i < $Factor } { incr i } {

      $Canvas create bitmap [expr $X + 20 + $i * 114] [expr $Y - 10 - $i * 97] \
         -bitmap @$GDefs(Dir)/Resources/Bitmap/string_test.xbm \
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
# Modifications :
#
#    Nom         : -
#    Date        : -
#    Description : -
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
# Modifications :
#
#    Nom         : -
#    Date        : -
#    Description : -
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
