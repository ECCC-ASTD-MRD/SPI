#===============================================================================
# Environnement Canada
# Centre Meteorologique Canadian
# 2100 Trans-Canadienne
# Dorval, Quebec
#
# Projet   : SPI (Layouts)
# Fichier  : TOA.tcl
# Creation : Fevrier 2001 - J.P. Gauthier - CMC/CMOE
#
# Description:
#    Modeles de carte au format TOA (Time of Arrival)
#
#===============================================================================

namespace eval TOA {
   variable Lbl
   variable Error
   variable Msg
   variable Bubble
   variable Param
   variable Ico
   variable Sim

   #----- Definition des labels

   set Lbl(Warning)     { "Attention" "Warning" }
   set Lbl(Treshold)    { "Seuil" "Treshold" }
   set Lbl(Products)    { "Produits" "Products" }

   #----- Definitions des textes des bulles d'aides

   set Sim(Lat)          0
   set Sim(Lon)          0
   set Sim(Name)         ""

   set Param(Treshold)  0
   set Param(FieldList) {}
   set Param(NOMVAR)    ""
   set Param(IP2)       -1
   set Param(IP3)       -1
   set Param(ETICKET)   ""
   set Param(Max)       0

   catch {
      colormap create TOAMAPDEFAULT
      colormap read TOAMAPDEFAULT $env(HOME)/.spi/Colormap/REC_Col.inv.std1.rgba
   }
}

#-------------------------------------------------------------------------------
# Nom      : <TOA::DrawScale>
# Creation : Mars 1999 - J.P. Gauthier - CMC/CMOE
#
# But      : Affiche l'echelle des valeurs du champs courant.
#
# Parametres :
#  <Frame>   : Identificateur de Page
#
# Retour    :
#
# Remarque :
#
#-------------------------------------------------------------------------------

proc TOA::DrawScale { Frame } {

   $Frame.page.canvas delete TOASCALE

   if { [fstdfield is TOAFIELD] && [set n [llength [set inters [fstdfield configure TOAFIELD -intervals]]]] } {

      set x0 360
      set x1 790
      set y0 515
      set y1 540

      set x $x0
      set dx [expr ($x1-$x0-5*($n-1))/$n]
      Log::Print DEBUG $dx
      foreach inter $inters {

         $Frame.page.canvas create rectangle $x $y0 [incr x $dx] $y1 \
            -fill "#[fstdfield configure TOAFIELD -val2map $inter]" -outline black -tags "TOASCALE TOACOLORMAP"
         $Frame.page.canvas create text [expr $x-$dx+5] [expr $y1+25] \
            -text [format %.0f $inter] -font XFont16 -fill black -tags "TOASCALE" -anchor ne -angle -45
         incr x 5
      }
   }
}

#----------------------------------------------------------------------------
# Nom      : <TOA::Layout>
# Creation : Fevrier 2001 - J.P. Gauthier - CMC/CMOE
#
# But      : Initialiser le modele TOA
#
# Parametres :
#  <Frame>   : Identificateur de Page
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc TOA::Layout { Frame } {
   global GDefs
   variable Param

   #----- Initialisations des constantes relatives aux projections

   set Viewport::Resources(FillCoast) #F0F0F0
   set Viewport::Resources(FillLake)  ""
   set Viewport::Resources(Coast)     #000000      ;#Cotes
   set Viewport::Resources(Lake)      #0000ff      ;#Lacs
   set Viewport::Resources(Polit)     #8C8C8C      ;#Bordures politiques
   set Viewport::Resources(Admin)     #8C8C8C      ;#Bordures politiques
   set Viewport::Resources(Coord)     #000000      ;#Latlon

   set Viewport::Map(Coast)       1           ;#Cotes
   set Viewport::Map(Lake)        1           ;#Lacs
   set Viewport::Map(River)       0           ;#Rivieres
   set Viewport::Map(Polit)       1           ;#Bordures politiques
   set Viewport::Map(Admin)       1           ;#Bordures politiques internes
   set Viewport::Map(City)        0           ;#Villes
   set Viewport::Map(Road)        0           ;#Routes
   set Viewport::Map(Rail)        0           ;#Chemin de fer
   set Viewport::Map(Util)        0           ;#Utilitaires
   set Viewport::Map(Canal)       0           ;#Canal/Aqueduc
   set Viewport::Map(Topo)        0           ;#Topographie
   set Viewport::Map(Bath)        0           ;#Bathymetrie
   set Viewport::Map(Text)        0           ;#Texture
   set Viewport::Map(Coord)       1           ;#Positionnement des latlon (<0=Ocean,>0=Partout)
   set Viewport::Map(CoordDef)    10.0        ;#Intervale entre les latlon en degres
   set Viewport::Map(CoordNum)    2           ;#Numerotation des latlon

   Page::Size $Frame 800 630

   set Param(VP) [Viewport::Create $Frame 5 5 790 500 0 0]

   SPI::IcoClear

   set canvas $Frame.page.canvas
   $canvas create rectangle 5 510 795 625 -outline black
   $canvas create rectangle 5 510 350 545 -outline black
   $canvas create bitmap  10 515 -bitmap @$GDefs(Dir)/Resources/Bitmap/SMC_hor_small.xbm  -foreground red  -anchor nw -tags "FIX RED"
   $canvas create bitmap 260 515 -bitmap @$GDefs(Dir)/Resources/Bitmap/RSMC_hor_small.xbm -foreground red  -anchor nw -tags "FIX RED"

   #----- Afficher l'identification de la source

   $canvas create bitmap -100 -100 -bitmap @$GDefs(Dir)/Resources/Bitmap/nucleaire.ico -foreground black -tags TOAFIX
   $canvas create text 10 555 -font XFont12 -anchor w -text "" -tags TOASOURCE
   $canvas create text 10 570 -font XFont12 -anchor w -text "" -tags TOALOC
   $canvas create text 10 585 -font XFont12 -anchor w -text "" -tags TOARELEASEISO
   $canvas create text 10 600 -font XFont12 -anchor w -text "" -tags TOARELEASEQT
   $canvas create text 10 615 -font XFont12 -anchor w -text "" -tags TOARELEASEDUR

   $canvas create text 360 600 -font XFont16 -anchor w -text "" -justify center -tags TOATITLE

   TOA::LayoutToolBar $Frame

   FSTD::VarMode VAR
}

#----------------------------------------------------------------------------
# Nom      : <TOA::LayoutClear>
# Creation : Fevrier 2001 - J.P. Gauthier - CMC/CMOE
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

proc TOA::LayoutClear { Frame } {

   Viewport::Destroy $Frame

   fstdfield free TOATMP TOAFIELD
   destroy $Frame.bar
}

#----------------------------------------------------------------------------
# Nom      : <TOA::LayoutToolBar>
# Creation : Fevrier 2001 - J.P. Gauthier - CMC/CMOE
#
# But      : Creer la barre d'outils VAAC
#
# Parametres :
#  <Frame>   : Identificateur de Page
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc TOA::LayoutToolBar { Frame } {
   global   GDefs
   variable Lbl

   frame $Frame.bar -relief raised -bd 1
      label $Frame.bar.id -text " TOA " -relief sunken -bd 1
      menubutton $Frame.bar.prod -bd 1 -text [lindex $Lbl(Products) $GDefs(Lang)] -state disabled
      label $Frame.bar.lbl -text "[lindex $Lbl(Treshold) $GDefs(Lang)]: "
      entry $Frame.bar.tresh -textvariable TOA::Param(Treshold) -bd 1 -width 8 -bg $GDefs(ColorLight)
      bind $Frame.bar.tresh <Return> { TOA::LayoutUpdate $Page::Data(Frame) [lindex [Viewport::Assigned $Page::Data(Frame) $TOA::Param(VP) fstdfield] 0] }

      pack $Frame.bar.id $Frame.bar.prod $Frame.bar.lbl $Frame.bar.tresh -side left -fill y
   pack $Frame.bar -side top -fill x -before $Frame.page
}

#----------------------------------------------------------------------------
# Nom      : <TOA::LayoutUpdate>
# Creation : Fevrier 20001 - J.P. Gauthier - CMC/CMOE
#
# But      : Effectue les changements des informations relatives a l'experience.
#
# Parametres :
#  <Frame>   : Identificateur de Page
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc TOA::Process { Field } {
   variable Sim
   variable Param

   Viewport::UnAssign $Page::Data(Frame) $Viewport::Data(VP)

   set tags [fstdfield stats $Field -tag]
   set box  [lindex $tags 2]

   #----- Make a copy of the fields

   set t0     $Sim(Seconds)
   set ip1    [fstdfield define $Field -IP1]
   set ip3    [fstdfield define $Field -IP3]
   set etiket [fstdfield define $Field -ETIKET]
   set var    [fstdfield define $Field -NOMVAR]

   fstdfield copy TOAFIELD $Field
   fstdfield stats TOAFIELD -nodata -1
   fstdfield clear TOAFIELD -1
   set tend ""

   if { $box=="" } {
      set fid    [fstdfield define $Field -FID]
      set fields [fstdfield find $fid -1 $etiket $ip1 -1 $ip3 "" $var]
      foreach field $fields {
         fstdfield read TOATMP $fid $field
         set t [expr double([fstdstamp toseconds [fstdfield define TOATMP -DATEV]]-$t0+1)]
         set tend [expr $t/3600.0]

         vexpr TOAFIELD ifelse(TOAFIELD==-1.0 && TOATMP>$Param(Treshold),$t,TOAFIELD)
      }
   } else {
      set fields [FieldBox::GetContent $box]
      set n 0
      set nx [llength $fields]

      foreach field $fields {

         set fid     [lindex $field 0]
         set idx     [lindex $field 1]
         set tvar    [lindex $field 2]
         set tip1    [lindex $field 4]
         set tip3    [lindex $field 6]
         set tetiket [string trim [lindex $field 7]]

         SPI::Progress [expr double([incr n])/$nx*100] "Processing fields ($idx)"

         if { $var==$tvar && $ip1==$tip1 && $ip3==$tip3 && $etiket==$tetiket } {
            fstdfield read TOATMP $fid $idx

            set t [expr double([fstdstamp toseconds [fstdfield define TOATMP -DATEV]]-$t0+1)]
            set tend [expr $t/3600.0]

            vexpr TOAFIELD ifelse(TOAFIELD==-1.0 && TOATMP>$Param(Treshold),$t,TOAFIELD)
         }
      }
   }
   set dt [vexpr dt smin(TOAFIELD)]
   vexpr TOAFIELD ifelse(TOAFIELD!=0.0,TOAFIELD-$dt,TOAFIELD)
   vexpr (Float32)TOAFIELD ifelse(TOAFIELD==0.0,-1,TOAFIELD)

   set n [expr $tend<12?1:$tend<24?2:$tend<48?3:$tend<72?6:$tend<144?12:$tend<288?12:24]
   set inter { 0 }
   for { set i $n } { $i<$tend } { incr i $n } {
      lappend inter $i
   }
      lappend inter $tend

   fstdfield configure TOAFIELD -rendertexture 1 -rendercontour 0 -mapall False -value INTEGER 0 -color #000000 -dash "" \
      -desc "Time of Arrival" -unit "Hour" -interpdegree NEAREST -factor [expr 1.0/3600.0] -renderlabel 0 -font XFont12 \
      -intervals $inter -color black -colormap TOAMAPDEFAULT
   fstdfield define TOAFIELD -IP1 $ip1 -IP2 0 -IP3 $ip3 -ETIKET $etiket

   SPI::Progress 0 ""
   Viewport::Assign $Page::Data(Frame) $Viewport::Data(VP) TOAFIELD
}

proc TOA::LayoutUpdate { Frame { Field "" } } {
   global   env
   variable Sim
   variable Param
   variable Error

   Viewport::UnAssign $Frame $Param(VP) TOAFIELD

   if { $Field=="" } {
      set Field [lindex [Viewport::Assigned $Frame $Param(VP) fstdfield] 0]
   }
   Log::Print DEBUG $Field
   if { $Field=="" } {
      if { [fstdfield is TOAFIELD] } {
         Viewport::Assign $Page::Data(Frame) $Viewport::Data(VP) TOAFIELD
      }
      return
   }

   set canvas $Frame.page.canvas

   #----- Recuperer les informations sur le champs selectionne

   set Param(FieldList) [FieldBox::GetContent -1]
   set Param(NOMVAR)    [string trim [fstdfield define $Field -NOMVAR]]
   set Param(IP2)       [fstdfield define $Field -IP2]
   set Param(IP3)       [fstdfield define $Field -IP3]
   set Param(ETICKET)   [string trim [fstdfield define $Field -ETIKET]]
   set Param(Max)       [fstdfield stats $Field -max]

   #----- Recuperer la description de l'experience

   set Sim(Lat)            0
   set Sim(Lon)            0
   set Sim(Name)           ""
   set Sim(AccYear)        0
   set Sim(AccMonth)       01
   set Sim(AccDay)         01
   set Sim(AccHour)        00
   set Sim(IsoName)        ""
   set Sim(IsoRelease)     0
   set Sim(Scale)          0
   set Sim(FnTime)         0
   set Sim(FnVert)         0
   set Sim(Event)          ""
   set Sim(EmHeight)       0
   set Sim(EmDuration)     0
   set Sim(EmVerticalDist) ""

   if { [set info [Info::Read [fstdfield define $Field -FID]]]=="" } {
      return
   }

   Info::Decode ::TOA::Sim $info
   set Sim(Path) [Info::Path $info]

   #----- Position de recentrage

   set Viewport::Map(LatReset) $Sim(Lat)
   set Viewport::Map(LonReset) $Sim(Lon)

   #----- Calculer la date d'accident(release)

   set Sim(Seconds) [clock scan "$Sim(AccMonth)/$Sim(AccDay)/$Sim(AccYear) $Sim(AccHour)00" -gmt true]

   set unit ""
   if { $Param(ETICKET) == "TRACER1" || $Param(ETICKET) == "TRACER2" || $Param(ETICKET) == "TRACER3" } {
      set unit "Units"
   } else {
      set unit "Bq"
   }

   #----- Updater les informations

   set coord [Convert::FormatCoord $Sim(Lat) $Sim(Lon) DEG]
   $canvas itemconf TOASOURCE      -text "Source           : $Sim(Name)"
   $canvas itemconf TOALOC         -text "Location         : $coord"
   $canvas itemconf TOARELEASEISO  -text "Isotope          : $Sim(EmIsoSymbol)"
   $canvas itemconf TOARELEASEQT   -text "Total release    : [format "%.2f" [lindex $Sim(EmIsoQuantity) [lsearch -exact [string toupper $Sim(EmIsoSymbol)] $Param(ETICKET)]]] $unit"
   $canvas itemconf TOARELEASEDUR  -text "Release duration : [format "%.2f" [expr double($Sim(EmTotalDuration))/3600.0]] Hour(s)"
   $canvas itemconf TOATITLE       -text "Plume arrival time from initial release\n[DateStuff::StringDateFromSeconds $Sim(Seconds) 1]"

   #----- Afficher les informations complementaires

   TOA::Process $Field
   TOA::UpdateItems $Frame
   TOA::DrawScale   $Frame
}

#----------------------------------------------------------------------------
# Nom      : <TOA::PrintCommand>
# Creation : Mars 1999 - J.P. Gauthier - CMC/CMOE
#
# But      : Creer la commande d'impression "plugin" pour les cas TOA a l'interieur
#            de la PrintBox.
#
# Parametres :
#  <Frame>   : Identificateur de Page
#
# Retour:
#
# Remarques :
#   Cette fonction est appelee par PrintBox pour effectuer l'impression selon les
#   parametres du PrintBox lui-meme et du "Widget PlugIn" PrintBox::PlugInCommand
#   definit ci-haut.
#
#----------------------------------------------------------------------------

#proc TOA::PrintCommand { Frame } {
#}

#----------------------------------------------------------------------------
# Nom      : <TOA::PrintWidget>
# Creation : Fevrier 1999 - J.P. Gauthier - CMC/CMOE
#
# But      : Initialise un "widget" "plugin" pour les cas TOA a l'interieur
#            de la PrintBox.
#
# Parametres :
#  <Frame>   : Identificateur de Page
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

#proc TOA::PrintWidget { Frame } {
#}

#----------------------------------------------------------------------------
# Nom      : <TOA::JoinTransfert>
# Creation : Avril 2000 - J.P. Gauthier - CMC/CMOE
#
# But      : Sortie au format TOA commun.
#
# Parametres :
#  <Frame>   : Identificateur de Page
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc TOA::JoinTransfert { Frame } {
}

#----------------------------------------------------------------------------
# Nom      : <TOA::UpdateItems>
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

proc TOA::UpdateItems { Frame } {
   variable Ico
   variable Sim
   variable Param

   if { [set xy [$Param(VP) -project $Sim(Lat) $Sim(Lon) 0]]!="" && [lindex $xy 2]>0 } {
      $Frame.page.canvas coords TOAFIX [lindex $xy 0] [lindex $xy 1]
   } else {
      $Frame.page.canvas coords TOAFIX -100 -100
   }
}
