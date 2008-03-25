#===============================================================================
# Environnement Canada
# Centre Meteorologique Canadian
# 2100 Trans-Canadienne
# Dorval, Quebec
#
# Projet   : SPI (Layouts)
# Fichier  : INFO.tcl
# Creation : Aout 2001
#
# Description:
#    Information sur l'experiences
#
#===============================================================================

namespace eval INFO {
   variable Lbl
   variable Msg
   variable Bubble
   variable Data
   variable Page

   #----- Definitions des labels

   set Lbl(File)         { "Fichier" "File" }
   set Lbl(Join)         { "Pr�parer les images pour transmission vers les pages webs communes" "Prepare images for transmission on common web pages" }
   set Lbl(No)           { "Non" "No" }
   set Lbl(OpenFile)     { "Ouvrir Fichier ..." "Open File ..." }
   set Lbl(OpenSim)      { "Ouvrir Simulation ..." "Open Simulation ..." }
   set Lbl(Print)        { "Sortie (Imprimante, Fichier, Web ) ..." "Output (Printer, File, Web) ..." }
   set Lbl(Products)     { "Produits" "Products" }
   set Lbl(Quit)         { "Quitter" "Quit" }
   set Lbl(Title)        { "Resume d'experience" "Experiment abstract" }
   set Lbl(Warning)      { "Attention" "Warning" }
   set Lbl(Yes)          { "Oui" "Yes" }

   #----- Definitions des messages

   set Msg(Join)         { "Etes-vous certain de vouloir generer les cartes de formats RSMC commun ?" \
                           "Do you really want to generate the RSMC joint format maps ?" }

   #----- Definitions des textes des bulles d'aides

   set Bubble(Name)      { "Selection de la denomination\ndu centre" "Select the center identification" }
   set Bubble(Products)  { "Selection des produits inclus\ndans l'experience" "Select the products\nincluded in the experiment" }
   set Bubble(Situation) { "Selection de la situation" "Select the situation" }
   set Bubble(Meteo)     { "Selection du type de meteorologie utilisee" "Select the meteorological data used" }

   #----- Definitions des variables internes

   set Data(Exercise)   "False"
   set Data(Mode)       "RSMC"
   set Data(Name)       "RSMC Montreal (Canadian Meteorological Centre)"
   set Data(Seconds)    [clock seconds]
   set Data(Meteo)      "Gem global"
   set Data(Situation)  "--- EXERCISE ONLY !!! ---"
   set Data(Product0)   0
   set Data(Product1)   0
   set Data(Product2)   0
   set Data(Product3)   0
   set Data(Product4)   0

   set Data(Names)      { "RSMC Montreal (Canadian Meteorological Centre)"
                          "VAAC Montreal (Canadian Meteorological Centre)"
                          "CMC (Canadian Meteorological Centre)" }
   set Data(Products)   { "Integrated concentration map(s)"
                          "Total deposition map(s)"
                          "Trajectory(ies)" }
   set Data(Meteos)     { "GEM global" "GEM Regional" "GEM HIMAP" }
   set Data(Situations) { "--- EXERCISE ONLY !!! ---" "--- THIS IS NOT AN EXERCISE !!! ---" }

   set Page(Border) 40
   set Page(Width)  680
   set Page(Height) 880
}

#----------------------------------------------------------------------------
# Nom      : <INFO::Layout>
# Creation : Aout 2001 - J.P. Gauthier - CMC/CMOE
#
# But      : Initialiser le modele INFO
#
# Parametres :
#  <Frame>   : Identificateur de Page
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc INFO::Layout { Frame } {
   variable Page

   #----- Initialisations des constantes relatives aux projections

   SPI::IcoClear

   Page::Size $Frame $Page(Width) $Page(Height)
   set Page(VP) [Viewport::Create $Frame [expr $Page(Border)+5] [expr $Page(Border)+312] 140 140 0 0]

   INFO::LayoutToolBar $Frame
   INFO::LayoutInit    $Frame
   INFO::LayoutUpdate  $Frame
   INFO::PageInit      $Frame
}

#----------------------------------------------------------------------------
# Nom      : <INFO::LayoutClear>
# Creation : Aout 2001 - J.P. Gauthier - CMC/CMOE
#
# But      : Supprimer tout ce qui a trait au "layout" precedent
#
# Parametres :
#  <Frame>   : Identificateur de Page
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc INFO::LayoutClear { Frame } {

   Viewport::Destroy $Frame

   destroy $Frame.bar $Frame.name $Frame.name.list $Frame.products $Frame.products.list \
      $Frame.meteo $Frame.meteo.list $Frame.situ $Frame.situ.list $Frame.blabla
}

#----------------------------------------------------------------------------
# Nom      : <INFO::LayoutToolBar>
# Creation : Aout 2001 - J.P. Gauthier - CMC/CMOE
#
# But      : Creer la barre d'outils INFO
#
# Parametres :
#  <Frame>   : Identificateur de Page
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc INFO::LayoutToolBar { Frame } {
   global   GDefs
   variable Lbl
   variable Data

   frame $Frame.bar -relief raised -bd 1
      label $Frame.bar.id -text " INFO " -relief sunken -bd 1

      menubutton $Frame.bar.prod -bd 1 -text [lindex $Lbl(Products) $GDefs(Lang)] -menu $Frame.bar.prod.menu
      pack $Frame.bar.id $Frame.bar.prod -side left -fill y

      menu $Frame.bar.prod.menu -tearoff 0 -bd 1 -activeborderwidth 1
         $Frame.bar.prod.menu add command -label [lindex $Lbl(Join) $GDefs(Lang)] \
            -command "INFO::RSMCJoin $Frame"
   pack $Frame.bar -side top -fill x -before $Frame.page
}

#----------------------------------------------------------------------------
# Nom      : <INFO::LayoutInit>
# Creation : Fevrier 2001 - J.P. Gauthier - CMC/CMOE
#
# But      : Effectue l'affichage des parties fixe de la carte.
#
# Parametres :
#  <Frame>   : Identificateur de Page
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc INFO::LayoutInit { Frame } {
   global   GDefs
   variable Data
   variable Bubble

   #----- Identification

   menubutton $Frame.name -textvariable INFO::Data(Name) -relief flat -bd 1 -bg $GDefs(ColorHighLight)\
       -font XFont20 -menu $Frame.name.list
   Bubble::Create $Frame.name  [lindex $Bubble(Name) $GDefs(Lang)]

   menu $Frame.name.list -tearoff 0 -bd 1 -bg $GDefs(ColorHighLight) -activeborderwidth 1
   foreach name $Data(Names) {
     $Frame.name.list add command -label $name -font XFont20 -command "set INFO::Data(Name) \"$name\"; INFO::PageInit $Frame"
   }

   #----- Liste des produits

   menubutton $Frame.products -text "Products :" -relief flat -bd 1 -bg $GDefs(ColorHighLight) \
      -font XFont10 -menu $Frame.products.list
   Bubble::Create $Frame.products  [lindex $Bubble(Products) $GDefs(Lang)]

   set no 0
   menu $Frame.products.list -tearoff 0 -bd 1 -activeborderwidth 1 -bg $GDefs(ColorHighLight)
   foreach product $Data(Products) {
     $Frame.products.list add checkbutton -variable INFO::Data(Product$no) -label $product -font XFont10 \
        -command "INFO::Product $Frame" -selectcolor red
      incr no
   }

   #----- Type de meteorologie

   menubutton $Frame.meteo -textvariable INFO::Data(Meteo) -relief flat -bd 1 -bg $GDefs(ColorHighLight) -font XFont10 -menu $Frame.meteo.list
   Bubble::Create $Frame.meteo  [lindex $Bubble(Meteo) $GDefs(Lang)]

   menu $Frame.meteo.list -tearoff 0 -bd 1 -activeborderwidth 1 -bg $GDefs(ColorHighLight)
   foreach meteo $Data(Meteos) {
     $Frame.meteo.list add command -label $meteo -font XFont10 -command "set INFO::Data(Meteo) \"$meteo\"; INFO::Detail $Frame"
   }

   #----- Type de situation d'experience

   menubutton $Frame.situ -textvariable INFO::Data(Situation) -relief flat -bd 1 -bg $GDefs(ColorHighLight)\
       -font XFont24 -menu $Frame.situ.list
   Bubble::Create $Frame.situ  [lindex $Bubble(Situation) $GDefs(Lang)]

   menu $Frame.situ.list -tearoff 0 -bd 1 -activeborderwidth 1 -bg $GDefs(ColorHighLight)
   foreach situation $Data(Situations) {
       $Frame.situ.list add command -label $situation -font XFont24 -command "set INFO::Data(Situation) \"$situation\""
   }

   #----- Details autres (blabla)

   text $Frame.blabla -height 30 -width 97 -bg white -relief flat -wrap word -highlightthickness 1 \
      -yscrollcommand "$Frame.blabla yview moveto 0.0 ; catch " -font XFont10
}

#----------------------------------------------------------------------------
# Nom      : <INFO::PageInit>
# Creation : Septembre 1999 - J.P. Gauthier - CMC/CMOE
#
# But      : Effectue l'affichage des parties fixe du resume.
#
# Parametres :
#  <Frame>   : Identificateur de Page
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc INFO::PageInit { Frame } {
   global   GDefs
   variable Data
   variable Page

   set canvas $Frame.page.canvas

   $canvas delete HEADER

   $canvas create bitmap [expr $Page(Width)/2] [expr 70+$Page(Border)] -bitmap "@$GDefs(Dir)/Resources/Bitmap/flag.xbm" \
      -foreground gray95 -tags HEADER

   #----- Creation de l'entete

   $canvas create line $Page(Border) $Page(Border) $Page(Border)  [expr $Page(Border)+140] [expr $Page(Width)-$Page(Border)] [expr $Page(Border)+140] \
       [expr $Page(Width)-$Page(Border)] $Page(Border) $Page(Border) $Page(Border) -fill black -tags HEADER

   #----- Who are we

   $canvas create text [expr $Page(Width)/2] [expr $Page(Border)+10] -text "Meteorological Service of Canada (MSC)" \
      -fill black -tags HEADER -font XFont20
   $canvas create text [expr $Page(Width)/2] [expr $Page(Border)+30] -text "$Data(Name)" \
      -fill black -tags HEADER -font XFont20

   #----- Where are we

   $canvas create text [expr $Page(Width)/2] [expr $Page(Border)+50] -tags HEADER -font XFont10 -anchor n -justify left \
      -text "2121 North Service Road\nTrans-Canada Highway\nDorval, Quebec Canada\nH9P 1J3" -fill black \

   #----- How to reach us

   $canvas create text [expr $Page(Border)+50] [expr $Page(Border)+120] -text "Tel (24 Hrs) : (514) 421-4635" \
      -fill black -tags HEADER -font XFont10 -anchor w
   $canvas create text [expr $Page(Width)-$Page(Border)-50] [expr $Page(Border)+120] -text "Fax (24 Hrs) : (514) 421-4639" \
      -fill black -tags HEADER -font XFont10 -anchor e
   $canvas create text [expr $Page(Width)-$Page(Border)-50] [expr $Page(Border)+130] -text "Fax Business : (514) 421-4679" \
      -fill black -tags HEADER -font XFont10 -anchor e

   #---- Options variables

   $canvas create window [expr $Page(Width)/2] [expr $Page(Border)+30] -window $Frame.name -tags "HEADER NOPRINT" -anchor c
}

#----------------------------------------------------------------------------
# Nom      : <INFO::Product>
# Creation : Septembre 1999 - J.P. Gauthier - CMC/CMOE
#
# But      : Effectue l'affichage de l'informations sur les produits.
#
# Parametres :
#  <Frame>   : Identificateur de Page
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc INFO::Product { Frame } {
   variable Data
   variable Page

   set canvas $Frame.page.canvas

   $canvas delete PRODUCT

   $canvas create line $Page(Border) [expr $Page(Border)+150] $Page(Border) [expr $Page(Border)+240] [expr $Page(Width)-$Page(Border)] [expr $Page(Border)+240] \
      [expr $Page(Width)-$Page(Border)]  [expr $Page(Border)+150] $Page(Border) [expr $Page(Border)+150] -fill black -tags PRODUCT

   $canvas create text [expr $Page(Width)/2] [expr $Page(Border)+160] -text "Issued [DateStuff::StringDateFromSeconds $Data(Seconds) 1]" \
      -fill black -tags PRODUCT -font XFont20

   $canvas create text [expr $Page(Border)+200] [expr $Page(Border)+174] -text "Products:" \
      -fill black -tags PRODUCT -font XFont10 -anchor n

   set products ""
   for { set i 0 } { $i < 5 } { incr i } {
      if { $Data(Product$i) } {
         lappend products "[lindex $Data(Products) $i]"
      }
   }
   $canvas create text [expr $Page(Border)+240] [expr $Page(Border)+174] -text "[join $products "\n"]" \
      -fill black -tags PRODUCT -font XFont10 -anchor nw

   $canvas create text [expr $Page(Width)/2] [expr $Page(Border)+230] -text "Please contact us if any problem arises with the described products." \
      -fill black -tags PRODUCT -font XFont10

   #---- Options variables

   $canvas create window [expr $Page(Border)+196] [expr $Page(Border)+180] -window $Frame.products -tags "PRODUCT NOPRINT"
}

#----------------------------------------------------------------------------
# Nom      : <INFO::Detail>
# Creation : Septembre 1999 - J.P. Gauthier - CMC/CMOE
#
# But      : Effectue l'affichage des details des produits.
#
# Parametres :
#  <Frame>   : Identificateur de Page
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc INFO::Detail { Frame } {
   variable Data
   variable Page
   variable Sim

   set canvas $Frame.page.canvas

   $canvas delete DETAIL

   $canvas create line $Page(Border) [expr $Page(Border)+250]  $Page(Border) [expr $Page(Height)-$Page(Border)] \
      [expr $Page(Width)-$Page(Border)] [expr $Page(Height)-$Page(Border)] [expr $Page(Width)-$Page(Border)] [expr $Page(Border)+250] \
      $Page(Border) [expr $Page(Border)+250] -fill black -tags DETAIL

   $canvas create rectangle $Page(Border) [expr $Page(Border)+273]  [expr $Page(Width)-$Page(Border)] [expr $Page(Border)+307] -fill gray \
      -outline black -tags DETAIL

   $canvas create text [expr $Page(Width)/2] [expr $Page(Border)+262] -text "Source term and dispersion model details" \
      -fill black -tags DETAIL -font XFont20

   #---- Fiction ou realite ???

   $canvas create text [expr $Page(Width)/2] [expr $Page(Border)+290] -text $Data(Situation) \
         -fill black -tags DETAIL -font XFont24

   #----- Details de la source

   if { $Data(Mode) == "VAAC" } {
      set coord [Convert::FormatCoord $Sim(Lat) $Sim(Lon) MIN]
   } else {
      set coord [Convert::FormatCoord $Sim(Lat) $Sim(Lon) DEG]
   }

   $canvas create text [expr $Page(Border)+160] [expr $Page(Border)+320] -text "Source               : $Sim(Name)" \
      -fill black -tags DETAIL -font XFont10 -anchor w
   $canvas create text [expr $Page(Border)+160] [expr $Page(Border)+340] -text "Location             : $coord , $Sim(EmHeight) Metres" \
      -fill black -tags DETAIL -font XFont10 -anchor w

   set seconds [clock scan "$Sim(AccMonth)/$Sim(AccDay)/$Sim(AccYear) $Sim(AccHour)00" -gmt true]
   $canvas create text [expr $Page(Border)+160] [expr $Page(Border)+360] -text "Release date/time    : [DateStuff::StringDateFromSeconds $seconds 1]" \
      -fill black -tags DETAIL -font XFont10 -anchor w

   #----- Details des produits

   $canvas create text [expr $Page(Border)+160] [expr $Page(Border)+380] -text "Meteorological model : $Data(Meteo)" \
      -fill black -tags DETAIL -font XFont10 -anchor w

   if { $Sim(IsoName) == "TRACER1" ||  $Sim(IsoName) == "TRACER2" ||  $Sim(IsoName) == "TRACER3"} {
      set unit "Units"
   } else {
      set unit "Bq"
   }
   $canvas create text [expr $Page(Border)+160] [expr $Page(Border)+400] -text "Emission             : $Sim(IsoRelease) $unit over $Sim(EmDuration) hours ($Sim(IsoName))" \
      -fill black -tags DETAIL -font XFont10 -anchor w

   if { $Sim(EmVerticalDist)=="" } {
      if { $Sim(FnVert) == 0.0 } {
         set Sim(EmVerticalDist) "Uniform between surface and 500m AGL"
      }
      if { $Sim(FnVert) < 0.0 } {
         set Sim(EmVerticalDist) "Gaussian"
      }
      if { $Sim(FnVert) > 0.0 } {
         set Sim(EmVerticalDist) "Empirical"
      }
   }

   $canvas create text [expr $Page(Border)+160] [expr $Page(Border)+420] -text "Vertical distribution: $Sim(EmVerticalDist)" \
      -fill black -tags DETAIL -font XFont10 -anchor w

   $canvas create text [expr $Page(Border)+160] [expr $Page(Border)+440] -text "Deposition           : Total (Wet and Dry)" \
      -fill black -tags DETAIL -font XFont10 -anchor w

   #---- Options variables

   $canvas create window [expr $Page(Width)/2] [expr $Page(Border)+290] -window $Frame.situ -tags "DETAIL NOPRINT" -anchor c
   $canvas create window [expr $Page(Border)+290] [expr $Page(Border)+380] -window $Frame.meteo -tags "DETAIL NOPRINT" -anchor w

   $canvas create window [expr $Page(Border)+5] [expr $Page(Border)+455] -window $Frame.blabla -tags "DETAIL NOPRINT" -anchor nw
   $canvas create text [expr $Page(Border)+20] [expr $Page(Border)+460] -fill black -tags TEXT -font XFont10 -anchor nw

   bind $Frame.blabla <Any-KeyRelease> "$canvas itemconfigure TEXT -text \[$Frame.blabla get 0.0 end\]"
   bind $Frame.blabla <FocusIn> "$canvas itemconfigure TEXT -text \[$Frame.blabla get 0.0 end\]"
}

#-------------------------------------------------------------------------------
# Nom      : <INFO::LayoutUpdate>
# Creation : Septembre 1999 - J.P. Gauthier - CMC/CMOE
#
# But      : Initialise les valeurs relatives a l'experience selectionee.
#
# Parametres :
#  <Frame>   : Identificateur de Page
#
# Retour    :
#
# Remarque :
#
#-------------------------------------------------------------------------------

proc INFO::LayoutUpdate { Frame } {
   global GDefs
   variable Sim
   variable Page
   variable Data

   set found 0
   set Sim(AccYear)        0
   set Sim(AccMonth)       01
   set Sim(AccDay)         01
   set Sim(AccHour)        00
   set Sim(Lat)            0
   set Sim(Lon)            0
   set Sim(Name)           ""
   set Sim(IsoName)        ""
   set Sim(IsoRelease)     0
   set Sim(FnTime)         0
   set Sim(FnVert)         0
   set Sim(EmHeight)       0
   set Sim(EmDuration)     0
   set Sim(EmVerticalDist) ""

   foreach field [FieldBox::GetContent] {
      if { [lindex $field 2]=="OL" || [lindex $field 2]=="INFO" } {
         set info [Info::Read [lindex $field 0]]
         eval Info::Decode ::INFO::Sim \$[Info::Strip $info Model]::Sim(Info) \$info
         set found 1
         break
      }
   }

   if { $found } {

      #----- Determiner les valeurs par defaur selon VAAC ou RSMC

      if { [lsearch -regexp $Sim(IsoName) VOLCAN?] == -1 } {
         set Data(Mode) RSMC
         set Data(Name) [lindex $Data(Names) 0]
         set Data(Ico)  @$GDefs(Dir)/Resources/Bitmap/nucleaire.ico
      } else {
         set Data(Mode) VAAC
         set Data(Name) [lindex $Data(Names) 1]
         set Data(Ico)  @$GDefs(Dir)/Resources/Bitmap/volcan.ico
      }

      INFO::Product     $Frame
      INFO::Detail      $Frame
      INFO::UpdateItems $Frame
   }
}

#----------------------------------------------------------------------------
# Nom      : <INFO::RSMCJoin>
# Creation : Avril 2000 - J.P. Gauthier - CMC/CMOE
#
# But      : Sortie au format RSMC commun.
#
# Parametres :
#  <Frame>   : Identificateur de Page
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc INFO::RSMCJoin { Frame } {
   global   GDefs
   variable Sim
   variable Lbl
   variable Msg

   if { $Sim(Name)=="" } {
      return
   }

   set create [Dialog::CreateDefault . 400 [lindex $Lbl(Warning) $GDefs(Lang)] [lindex $Msg(Join) $GDefs(Lang)] \
      warning 0 [lindex $Lbl(Yes) $GDefs(Lang)] [lindex $Lbl(No) $GDefs(Lang)]]

   if { $create } {
      return
   }

   set path "$GDefs(DirData)/$Sim(NoExp)_$Sim(Name)/Output/RSMCJoin"

   if { ![file exists $path] } {
      file mkdir $path
   }

   PrintBox::Image $Frame gif $path/CVRCA
   PrintBox::Postscript $Frame $path/CVRCA 0 0 [Page::CanvasWidth $Frame] [Page::CanvasHeight $Frame] portrait "8.5_x_11"
}

#----------------------------------------------------------------------------
# Nom      : <INFO::UpdateItems>
# Creation : Fevrier 2001 - J.P. Gauthier - CMC/CMOE
#
# But      : Affiche les icones sur la carte.
#
# Parametres :
#  <Frame>   : Identificateur de Page
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc INFO::UpdateItems { Frame } {
   variable Sim
   variable Data
   variable Page

   $Frame.page.canvas delete LAYOUTINFO

   if { $Sim(Name)!="" && [set xy [$Page(VP) -project $Sim(Lat) $Sim(Lon) 0]] != "" } {
      $Frame.page.canvas create bitmap [lindex $xy 0] [lindex $xy 1] -bitmap $Data(Ico) -foreground black -tags LAYOUTINFO
   }
}
