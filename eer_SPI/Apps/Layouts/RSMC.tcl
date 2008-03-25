#===============================================================================
# Environnement Canada
# Centre Meteorologique Canadian
# 2100 Trans-Canadienne
# Dorval, Quebec
#
# Projet   : SPI (Layouts)
# Fichier  : RSMC.tcl
# Creation : Fevrier 2001 - J.P. Gauthier - CMC/CMOE
#
# Description:
#    Modeles de carte au format RSMC
#
#===============================================================================

namespace eval RSMC {
   variable Lbl
   variable Error
   variable Msg
   variable Bubble
   variable Data
   variable Page
   variable Ico
   variable Sim

   #----- Definition des labels

   set Lbl(Field)       { "Champs" "Field" }
   set Lbl(Join)        { "Pr�parer les images pour transmission vers les pages webs communes" "Prepare images for transmission on common web pages" }
   set Lbl(No)          { "Non" "No" }
   set Lbl(Hour)        { "Heure" "Hour" }
   set Lbl(Specie)      { "Espece" "Specie" }
   set Lbl(PrintTitle)  { "Impression (RSMC)" "Map printing (RSMC)" }
   set Lbl(PrintPlugIn) { "Selection carte (RSMC)" "Map Selection (RSMC)" }
   set Lbl(Products)    { "Produits" "Products" }
   set Lbl(Yes)         { "Oui" "Yes" }
   set Lbl(Warning)     { "Attention" "Warning" }

   #----- Definitions des messages

   set Msg(Join)        { "Etes-vous certain de vouloir generer les cartes de formats RSMC commun ?" \
                          "Do you really want to generate the RSMC joint format maps ?" }

   #----- Definitions des textes des bulles d'aides

   set Ico(Nuclear)      "@$GDefs(Dir)/Resources/Bitmap/nucleaire.ico"
   set Ico(Flag)         "@$GDefs(Dir)/Resources/Bitmap/SMC_ver_small.xbm"
   set Ico(RSMC)         "@$GDefs(Dir)/Resources/Bitmap/RSMC_ver_small.xbm"
   set Ico(Contour)      "@$GDefs(Dir)/Resources/Bitmap/string_contour.xbm"

   set Sim(Lat)          0
   set Sim(Lon)          0
   set Sim(Name)         ""
   set Sim(EmHeight)     0

   set Data(Max)        { 0 0 0 }
   set Data(FieldList)  ""
   set Data(Fields)     { FM MF DD DI WD WI IT DOSE }
   set Data(Units)      { "/M3" "*S/M3" "/M2" "/M2" "/M2" "/M2" "/M2" }
   set Data(Desc)       { "Surface to 500 M mean concentration" "24-hr time integrated 0 to 500 M layer concentrations" \
                          "6 hours dry deposition" "Total dry deposition" "6 hours wet deposition" "Total wet deposition" "Total deposition" }

   set Data(DoseFields) { CI CC CG CT }
   set Data(DoseGroups) { "All ages" "3 month" "1 year" "5 year" "10 year" "15 year" "Adult" }
   set Data(DoseDesc)   { "Hrs time integrated dose for inhalation"
                          "Hrs time integrated dose for cloudshine"
                          "Hrs time integrated dose for groundshine"
                          "Hrs total integrated dose" }

   #---- Definitions des entetes et textes de la carte

   set Data(FT1)     "Isotope                :"
   set Data(FT2)     "Time function          :"
   set Data(FT3)     "Duration               :"
   set Data(FT4)     "Horizontal distribution:"
   set Data(FT5)     "Grid length            :"
   set Data(FT8)     "Total release           :"
   set Data(FT9)     "Height of release       :"
   set Data(FT10)    "Vertical distribution   :"
   set Data(FT11)    "Standard deviation (Hor):"
   set Data(FT12)    "Maximum value at o      :"

   #----- 8.5x11 (80dpi)

   set Page(Border) 15
   set Page(Width)  770
   set Page(Height) 830
}

#-------------------------------------------------------------------------------
# Nom      : <RSMC::DrawScale>
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

proc RSMC::DrawScale { Frame } {
   variable Data
   variable Page

   $Frame.page.canvas delete LAYOUTRSMCSCALE

   #----- Determine l'espacement et le point de depart de l'echelle

   set start [expr $Viewport::Data(Height$Page(VP)) + $Viewport::Data(Y$Page(VP))]
   set i 0
   set field [lindex [Viewport::Assigned $Frame $Page(VP) fstdfield] 0]

   foreach level [fstdfield configure $field -intervals] {

      $Frame.page.canvas create rectangle [expr $Viewport::Data(Width$Page(VP)) + $Viewport::Data(X$Page(VP)) + 5] [expr $start-10] \
      [expr $Viewport::Data(Width$Page(VP)) + $Viewport::Data(X$Page(VP)) + 40] [expr $start-30] \
         -fill "#[fstdfield configure $field -val2map $level]" -outline black -tags "LAYOUTRSMCSCALE LAYOUTRSMCCOLORMAP"
      $Frame.page.canvas create text [expr $Viewport::Data(Width$Page(VP)) + $Viewport::Data(X$Page(VP)) + 4] [expr $start] \
         -text [format %1.1e $level] -font XFont10 -anchor sw -fill black -tags "LAYOUTRSMCSCALE"

      set start [expr $start-40]
      incr i
   }

   $Frame.page.canvas create text [expr $Viewport::Data(Width$Page(VP)) + $Viewport::Data(X$Page(VP)) + 5] [expr $start-5] \
      -text $Data(Unit) -font XFont10 -anchor sw -fill black -tags "LAYOUTRSMCSCALE"
}

#----------------------------------------------------------------------------
# Nom      : <RSMC::Layout>
# Creation : Fevrier 2001 - J.P. Gauthier - CMC/CMOE
#
# But      : Initialiser le modele RSMC
#
# Parametres :
#  <Frame>   : Identificateur de Page
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc RSMC::Layout { Frame } {
   variable Page

   #----- Initialisations des constantes relatives aux projections

   set Viewport::Map(Border)   1
   set Viewport::Map(LatLon)   1

   Page::Size $Frame $Page(Width) $Page(Height)

   set Page(VP) [Viewport::Create $Frame [expr $Page(Border)+45] [expr $Page(Border)+45] 650 600 0 0]

   SPI::IcoClear

   RSMC::LayoutToolBar $Frame
   RSMC::LayoutInit    $Frame

   FSTD::VarMode VAR

   FSTD::Params MF REC_Col.std1 -intervalmode RSMC 4 -intervals { } -factor 1.0 -rendertexture 1 -rendervolume 0 \
      -rendercontour 2 -rendervector NONE -rendergrid 0 -renderlabel 0 -rendervalue 0 -color #000000 -dash "" -desc MF
   FSTD::Params FM REC_Col.std1 -intervalmode RSMC 4 -intervals { } -factor 1.0 -rendertexture 1 -rendervolume 0 \
      -rendercontour 2 -rendervector NONE -rendergrid 0 -renderlabel 0 -rendervalue 0 -color #000000 -dash "" -desc FM
   FSTD::Params DD REC_Col.std1 -intervalmode RSMC 4 -intervals { } -factor 1.0 -rendertexture 1 -rendervolume 0 \
      -rendercontour 2 -rendervector NONE -rendergrid 0 -renderlabel 0 -rendervalue 0 -color #000000 -dash "" -desc DD
   FSTD::Params DI REC_Col.std1 -intervalmode RSMC 4 -intervals { } -factor 1.0 -rendertexture 1 -rendervolume 0 \
      -rendercontour 2 -rendervector NONE -rendergrid 0 -renderlabel 0 -rendervalue 0 -color #000000 -dash "" -desc DI
   FSTD::Params WD REC_Col.std1 -intervalmode RSMC 4 -intervals { } -factor 1.0 -rendertexture 1 -rendervolume 0 \
      -rendercontour 2 -rendervector NONE -rendergrid 0 -renderlabel 0 -rendervalue 0 -color #000000 -dash "" -desc WD
   FSTD::Params WI REC_Col.std1 -intervalmode RSMC 4 -intervals { } -factor 1.0 -rendertexture 1 -rendervolume 0 \
      -rendercontour 2 -rendervector NONE -rendergrid 0 -renderlabel 0 -rendervalue 0 -color #000000 -dash "" -desc WI
   FSTD::Params IT REC_Col.std1 -intervalmode RSMC 4 -intervals { } -factor 1.0 -rendertexture 1 -rendervolume 0 \
      -rendercontour 2 -rendervector NONE -rendergrid 0 -renderlabel 0 -rendervalue 0 -color #000000 -dash "" -desc IT
   FSTD::Params CC REC_Col.std1 -intervalmode RSMC 4 -intervals { } -factor 1.0 -rendertexture 1 -rendervolume 0 \
      -rendercontour 2 -rendervector NONE -rendergrid 0 -renderlabel 0 -rendervalue 0 -color #000000 -dash "" -desc CC
   FSTD::Params CV REC_Col.std1 -intervalmode RSMC 4 -intervals { } -factor 1.0 -rendertexture 1 -rendervolume 0 \
      -rendercontour 2 -rendervector NONE -rendergrid 0 -renderlabel 0 -rendervalue 0 -color #000000 -dash "" -desc CV
   FSTD::Params CG REC_Col.std1 -intervalmode RSMC 4 -intervals { } -factor 1.0 -rendertexture 1 -rendervolume 0 \
      -rendercontour 2 -rendervector NONE -rendergrid 0 -renderlabel 0 -rendervalue 0 -color #000000 -dash "" -desc CG
   FSTD::Params CT REC_Col.std1 -intervalmode RSMC 4 -intervals { } -factor 1.0 -rendertexture 1 -rendervolume 0 \
      -rendercontour 2 -rendervector NONE -rendergrid 0 -renderlabel 0 -rendervalue 0 -color #000000 -dash "" -desc CT
   FSTD::Params CI REC_Col.std1 -intervalmode RSMC 4 -intervals { } -factor 1.0 -rendertexture 1 -rendervolume 0 \
      -rendercontour 2 -rendervector NONE -rendergrid 0 -renderlabel 0 -rendervalue 0 -color #000000 -dash "" -desc CI
}

#----------------------------------------------------------------------------
# Nom      : <RSMC::LayoutClear>
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

proc RSMC::LayoutClear { Frame } {

   Viewport::Destroy $Frame
   destroy $Frame.bar
}

#----------------------------------------------------------------------------
# Nom      : <RSMC::LayoutToolBar>
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

proc RSMC::LayoutToolBar { Frame } {
   global   GDefs
   variable Lbl
   variable Data

   frame $Frame.bar -relief raised -bd 1
      label $Frame.bar.id -text " RSMC " -relief sunken -bd 1
      menubutton $Frame.bar.prod -bd 1 -text [lindex $Lbl(Products) $GDefs(Lang)] -menu $Frame.bar.prod.menu
      menu $Frame.bar.prod.menu -tearoff 0 -bd 1 -activeborderwidth 1
         $Frame.bar.prod.menu add command -label [lindex $Lbl(Join) $GDefs(Lang)] \
            -command "RSMC::JoinTransfert $Frame"
      pack $Frame.bar.id $Frame.bar.prod -side left -fill y
   pack $Frame.bar -side top -fill x -before $Frame.page
}

#----------------------------------------------------------------------------
# Nom      : <RSMC::LayoutInit>
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

proc RSMC::LayoutInit { Frame } {
   variable Lbl
   variable Data
   variable Page
   variable Ico


   set canvas $Frame.page.canvas

   $canvas create line $Page(Border) $Page(Border) $Page(Border) [expr $Page(Height)-$Page(Border)] \
      [expr $Page(Width)-$Page(Border)] [expr $Page(Height)-$Page(Border)] [expr $Page(Width)-$Page(Border)] $Page(Border) \
      $Page(Border) $Page(Border) -fill black -tags defs

   #----- Afficher l'identification de la source

   $canvas create bitmap [expr $Page(Border)+220] [expr $Page(Border)+22] -bitmap $Ico(Nuclear) -foreground black -tags FIX
   $canvas create text [expr $Page(Border)+240] [expr $Page(Border)+10] -font XFont12 -anchor w -text "" -tags RSMCSOURCE
   $canvas create text [expr $Page(Border)+240] [expr $Page(Border)+22] -font XFont12 -anchor w -text "" -tags RSMCLOC
   $canvas create text [expr $Page(Border)+240] [expr $Page(Border)+34] -font XFont12 -anchor w -text "" -tags RSMCRELEASE

   #----- Afficher l'identification

   $canvas create bitmap [expr $Page(Border)+10] [expr $Viewport::Data(Height$Page(VP))+$Viewport::Data(Y$Page(VP))] \
      -bitmap $Ico(Flag) -foreground red  -anchor sw -tags "FIX RED"
   $canvas create bitmap [expr $Page(Border)+10] [expr $Viewport::Data(Height$Page(VP))+$Viewport::Data(Y$Page(VP))-250] \
      -bitmap $Ico(RSMC) -foreground red  -anchor sw -tags "FIX RED"

   #----- Afficher la legende du bas

   $canvas create text [expr $Page(Width)/2] [expr $Page(Border)+670] -font XFont20 -anchor s -text "" -tags HD1
   $canvas create text [expr $Page(Width)/2] [expr $Page(Border)+690] -font XFont20 -anchor s -text "" -tags HD2
   $canvas create text [expr $Page(Width)/2] [expr $Page(Border)+720] -font XFont20 -anchor s -text "Release scenario" -tags HD3

   $canvas create text [expr $Page(Border)+10] [expr $Page(Border)+740]  -font XFont12 -anchor sw -text "$Data(FT1)"  -tags FT1
   $canvas create text [expr $Page(Border)+10] [expr $Page(Border)+752]  -font XFont12 -anchor sw -text "$Data(FT2)"  -tags FT2
   $canvas create text [expr $Page(Border)+10] [expr $Page(Border)+764]  -font XFont12 -anchor sw -text "$Data(FT3)"  -tags FT3
   $canvas create text [expr $Page(Border)+10] [expr $Page(Border)+776]  -font XFont12 -anchor sw -text "$Data(FT4)"  -tags FT4
   $canvas create text [expr $Page(Border)+10] [expr $Page(Border)+788]  -font XFont12 -anchor sw -text "$Data(FT5)"  -tags FT5
   $canvas create text [expr $Page(Border)+10] [expr $Page(Border)+800]  -font XFont12 -anchor sw -text ""            -tags FT6
   $canvas create text [expr $Page(Border)+365] [expr $Page(Border)+740] -font XFont12 -anchor sw -text "$Data(FT8)"  -tags FT8
   $canvas create text [expr $Page(Border)+365] [expr $Page(Border)+752] -font XFont12 -anchor sw -text "$Data(FT9)"  -tags FT9
   $canvas create text [expr $Page(Border)+365] [expr $Page(Border)+764] -font XFont12 -anchor sw -text "$Data(FT10)" -tags FT10
   $canvas create text [expr $Page(Border)+365] [expr $Page(Border)+776] -font XFont12 -anchor sw -text "$Data(FT11)" -tags FT11
   $canvas create text [expr $Page(Border)+365] [expr $Page(Border)+788] -font XFont12 -anchor sw -text "$Data(FT12)" -tags FT12
   $canvas create text [expr $Page(Border)+365] [expr $Page(Border)+800] -font XFont12 -anchor sw -text ""            -tags FT13

   $canvas create text [expr $Page(Width)-1] [expr $Page(Height)-1] -font XFont10 -anchor se -text "[clock format [clock seconds] -format "%H%M %d/%m/%Y" -gmt true]" -tags FTID

   #----- Afficher l'entete d'echelle

   $canvas create bitmap [expr $Page(Width)-$Page(Border)-10] 60 -bitmap $Ico(Contour) -foreground black -tags FIX -anchor ne
}

#----------------------------------------------------------------------------
# Nom      : <RSMC::LayoutUpdate>
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

proc RSMC::LayoutUpdate { Frame } {
   global   env
   variable Sim
   variable Data
   variable Page
   variable Error

   set field [lindex [Viewport::Assigned $Frame $Page(VP) fstdfield] 0]

   if { $field=="" } {
      return
   }

   set canvas $Frame.page.canvas

   #----- Recuperer les informations sur le champs selectionne

   set Data(FieldList) [FieldBox::GetContent -1]
   set Data(NOMVAR)    [string trim [fstdfield define $field -NOMVAR]]
   set Data(IP2)       [fstdfield define $field -IP2]
   set Data(IP3)       [fstdfield define $field -IP3]
   set Data(ETICKET)   [string trim [fstdfield define $field -ETIKET]]
   set Data(Max)       [fstdfield stats $field -max]
   set idx             [lsearch -exact $Data(Fields) $Data(NOMVAR)]
   set idxdose         [lsearch -exact $Data(DoseFields) $Data(NOMVAR)]

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

   if { [set info [Info::Read [fstdfield define $field -FID]]]=="" } {
      return
   }

   eval set Sim(Info) \$[Info::Strip $info Model]::Sim(Info)
   Info::Decode ::RSMC::Sim $Sim(Info) $info
   set Sim(Path)    [Info::Path $Sim(Info) $info]

   set Sim(IsoName) [string toupper $Sim(IsoName)]

   #----- Position de recentrage

   set Viewport::Map(LatReset) $Sim(Lat)
   set Viewport::Map(LonReset) $Sim(Lon)

   #----- Update de la page

   switch $Sim(Scale) {
      "MESO"  { set lgrille "50 Km" }
      "FINE"  { set lgrille "25 Km" }
      "VFINE" { set lgrille "10 Km" }
      "EFINE" { set lgrille "5 Km" }
      default { set lgrille "150 Km" }
   }

   switch $Sim(FnTime) {
      "GAUSS"       { set fonction "GAUSSIAN"    ; set duree "Standard Deviation     :" }
      "EXPONENTIAL" { set fonction "EXPONENTIAL" ; set duree "Duration               :" }
      default       { set fonction "CONSTANT"    ; set duree "Duration               :" }
   }

   if { $Sim(EmVerticalDist)=="" } {
      if { $Sim(FnVert) == 0.0 } {
         set Sim(EmVerticalDist) "UNIFORM 0-500M"
      }
      if  { $Sim(FnVert) < 0.0 } {
         set Sim(EmVerticalDist) "GAUSSIAN"
      }
      if  { $Sim(FnVert) > 0.0 } {
         set Sim(EmVerticalDist) "EMPIRICAL"
      }
   }

   #----- Calculer la date d'accident(release)

   set seconds     [clock scan "$Sim(AccMonth)/$Sim(AccDay)/$Sim(AccYear) $Sim(AccHour)00" -gmt true]
   set daterelease [clock format $seconds -format "%a %b %d %Y, %H UTC" -gmt true]

   #----- Updater les informations

   set coord [Convert::FormatCoord $Sim(Lat) $Sim(Lon) DEG]

   $canvas itemconf RSMCSOURCE  -text "Source name     : $Sim(Name)"
   $canvas itemconf RSMCLOC     -text "Source location : $coord"
   $canvas itemconf RSMCRELEASE -text "Date of release : $daterelease"

   $canvas itemconf FT2 -text "$Data(FT2) ${fonction}"
   $canvas itemconf FT3 -text "${duree} $Sim(EmDuration) H"
   $canvas itemconf FT4 -text "$Data(FT4) GAUSSIAN"
   $canvas itemconf FT5 -text "$Data(FT5) ${lgrille}"
   $canvas itemconf FT6 -text "$Sim(Event)"
   $canvas itemconf FT9 -text "$Data(FT9) $Sim(EmHeight) METRES"
   $canvas itemconf FT10 -text "$Data(FT10) $Sim(EmVerticalDist)"
   $canvas itemconf FT11 -text "$Data(FT11) 1 Grid Length"

   #----- Definir le scenario

   if { $Sim(EmHeight)==0.0 && $Sim(FnVert)==0.0 && $Data(ETICKET)=="137-CS" && [lindex $Sim(IsoRelease) [lsearch -exact $Sim(IsoName) $Data(ETICKET)]]==1.0 } {
      $canvas itemconf FT13 -text "RESULTS BASED ON DEFAULT VALUES"
   } else {
      $canvas itemconf FT13 -text ""
   }

   set Data(Unit) ""

   if { $Data(ETICKET) == "TRACER1" || $Data(ETICKET) == "TRACER2" || $Data(ETICKET) == "TRACER3" } {
      set Data(Unit) "Units"
   } else {
      set Data(Unit) "Bq"
   }

   set datev     [fstdstamp todate [fstdfield define $field -DATEV]]
   set secv      [clock scan "[lindex $datev 1]/[lindex $datev 2]/[lindex $datev 0] [lindex $datev 3]:[lindex $datev 4]:[lindex $datev 5]" -gmt true]

   if { $Data(NOMVAR) == "IT" } {
      if { $Sim(AccHour) < "12" } {
         set secondsrelease [clock scan "$Sim(AccMonth)/$Sim(AccDay)/$Sim(AccYear) 0000" -gmt true]
      } else {
         set secondsrelease [clock scan "$Sim(AccMonth)/$Sim(AccDay)/$Sim(AccYear) 1200" -gmt true]
      }

      set date0 [clock format $secondsrelease -format "%a %b %d %Y, %H UTC" -gmt true]
      set date1 [clock format $secv -format "%a %b %d %Y, %H UTC" -gmt true]
      $canvas itemconf HD2  -text "from $date0 to $date1"
   } elseif { $Data(NOMVAR) == "MF" } {

      set date0 [clock format [expr $secv-(24*3600)] -format "%a %b %d %Y, %H UTC" -gmt true]
      set date1 [clock format $secv -format "%a %b %d %Y, %H UTC" -gmt true]

      $canvas itemconf HD2  -text "from $date0 to $date1"
   } else {
      $canvas itemconf HD2  -text "valid [clock format $secv -format "%a %b %d %Y, %H UTC" -gmt true]"
   }

   $canvas itemconf FT1  -text "$Data(FT1) $Data(ETICKET)"
   $canvas itemconf FT8  -text "$Data(FT8) [lindex $Sim(IsoRelease) [lsearch -exact $Sim(IsoName) $Data(ETICKET)]] $Data(Unit)"

   if { $idx != -1 } {
      $canvas itemconf HD1  -text "[lindex $Data(Desc) $idx]"
      set Data(Unit) "$Data(Unit)[lindex $Data(Units) $idx]"
   } elseif { $idxdose != -1 } {
      $canvas itemconf HD1  -text "$Data(IP2) [lindex $Data(DoseDesc) $idxdose] ([lindex $Data(DoseGroups) $Data(IP3)])"
      set Data(Unit) "Sv"
   } else {
      $canvas itemconf HD1  -text "$Data(NOMVAR)"
      set Data(Unit) ""
   }

   $canvas itemconf FT12 -text "$Data(FT12) [format "%1.2e" [lindex $Data(Max) 0]] $Data(Unit)"

   #----- Afficher les informations complementaires

   RSMC::UpdateItems $Frame
   RSMC::DrawScale   $Frame
}

#----------------------------------------------------------------------------
# Nom      : <RSMC::PrintCommand>
# Creation : Mars 1999 - J.P. Gauthier - CMC/CMOE
#
# But      : Creer la commande d'impression "plugin" pour les cas RSMC a l'interieur
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

proc RSMC::PrintCommand { Frame } {
   variable Sim
   variable Lbl
   variable Data
   variable Page
   variable Print

   if { $Sim(Name)=="" } {
      InfoFrame::Set100 .printbox.job 3
      PrintBox::Print $Frame 0 0 [Page::CanvasWidth $Frame]  [Page::CanvasHeight $Frame]
      PrintBox::Destroy
      return
   }

   #----- Calculer le pourcentage maximum

   set maxfield 0
   foreach field $Data(Fields) {
      if { $Print($field) } {
         incr maxfield
      }
   }
   set maxhour 0
   foreach hour $Data(Hours) {
      if { $Print($hour) } {
         incr maxhour
      }
   }
   set maxiso 0
   foreach iso $Sim(IsoName) {
      if { $Print($iso) } {
         incr maxiso
      }
   }

   set data [Viewport::Assigned $Frame $Page(VP) { fstdfield trajectory observation }]

   #----- Calculer le maximum en pourcentage (nombres de cartes * (2 + FTP?)

   InfoFrame::Set100 .printbox.job [expr $maxhour*$maxfield*$maxiso*5]

   #----- Pour toutes les heures selectionnees

   FieldBox::Clear

   foreach field $Data(Fields) {
      foreach iso $Sim(IsoName) {
         foreach hour $Data(Hours) {

            if { $Print($field) && $Print($hour) && $Print($iso) } {

               if { $field=="DOSE" } {

                  foreach nomvar "CI CC CG CT" {
                     foreach ip3 "0 1 2 3 4 5 6" {
                        if { [MetData::Find PRINT [FieldBox::GetFID -1] -1 [string toupper $iso] 0 $hour $ip3 "" $nomvar] } {

                           #----- Lire le champs selectionnee si il n'est pas deja en memoire

                           InfoFrame::Incr .printbox.job 1 "Reading Field for $field $ip3 $hour $iso"
                           Viewport::UnAssign $Frame $Page(VP)
                           Viewport::Assign $Frame $Page(VP) PRINT
                           RSMC::LayoutUpdate  $Frame

                           #----- Appeler la fonction d'impression du PrintBox

                           set PrintBox::Print(FullName) "$PrintBox::Print(Path)/$PrintBox::Print(Filename)_${nomvar}_${ip3}_${iso}.${hour}"
                           InfoFrame::Incr .printbox.job 1 "Generating RSMC Map $PrintBox::Print(FullName)"
                           PrintBox::Print $Frame 0 0 [Page::CanvasWidth $Frame]  [Page::CanvasHeight $Frame]
                        }
                     }
                  }
               } else {
                  if { [MetData::Find PRINT [FieldBox::GetFID -1] -1 [string toupper $iso] -1 $hour -1 "" $field] } {

                     #----- Lire le champs selectionnee si il n'est pas deja en memoire

                     InfoFrame::Incr .printbox.job 1 "Reading Field for $field $hour $iso"

                     Viewport::UnAssign $Frame $Page(VP)
                     Viewport::Assign $Frame $Page(VP) PRINT
                     RSMC::LayoutUpdate  $Frame

                     #----- Appeler la fonction d'impression du PrintBox

                     set PrintBox::Print(FullName) "$PrintBox::Print(Path)/$PrintBox::Print(Filename)_${field}_${iso}.${hour}"
                     InfoFrame::Incr .printbox.job 1 "Generating RSMC Map $PrintBox::Print(FullName)"
                     PrintBox::Print $Frame 0 0 [Page::CanvasWidth $Frame]  [Page::CanvasHeight $Frame]
                  }
               }
            }
         }
      }
   }

   Viewport::UnAssign $Frame $Page(VP)
   Viewport::Assign $Frame $Page(VP) $data 1
   RSMC::LayoutUpdate $Frame

   if { [fstdfield is PRINT] } {
      fstdfield free PRINT
   }

   PrintBox::Destroy
}

#----------------------------------------------------------------------------
# Nom      : <RSMC::PrintWidget>
# Creation : Fevrier 1999 - J.P. Gauthier - CMC/CMOE
#
# But      : Initialise un "widget" "plugin" pour les cas RSMC a l'interieur
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

proc RSMC::PrintWidget { Frame } {
   global GDefs
   variable Sim
   variable Lbl
   variable Print
   variable Data

   if { $Sim(Name) == "" } {
      return
   }

   wm geom  .printbox 335x595

   set Data(Hours) [MetData::ListIP2 $Data(FieldList) FM]

   if { $Data(Hours) == "" } {
      set Data(Hours) [MetData::ListIP2 $Data(FieldList) IT]
   }

   labelframe .printbox.par.map -text [lindex $Lbl(PrintPlugIn) $GDefs(Lang)]

      #----- Selection des heures

      frame .printbox.par.map.hours -relief  sunken -bd 1
         checkbutton .printbox.par.map.hours.lbl -text [lindex $Lbl(Hour) $GDefs(Lang)] -bd 1 \
            -indicatoron false -bg $GDefs(ColorHighLight) -variable RSMC::Print(Hours) \
            -command { foreach hour $RSMC::Data(Hours) { set RSMC::Print($hour) $RSMC::Print(Hours) }}
         pack .printbox.par.map.hours.lbl -side top -ipady 4 -fill x

         frame .printbox.par.map.hours.list
         set column 0
         set row 0

         foreach hour $Data(Hours) {
            set Print($hour) 0
            checkbutton .printbox.par.map.hours.list.$hour -variable RSMC::Print($hour) -text " [format "%02i" $hour] " \
               -indicatoron false -bd 1
            grid .printbox.par.map.hours.list.$hour -sticky ew -ipady 2 -ipadx 5 \
               -column $column -row $row

            if { $row >= 13 } {
               incr column
               set row 0
            } else {
               incr row
            }
         }
      pack .printbox.par.map.hours.list -side top -anchor w
      pack .printbox.par.map.hours -side left -padx 5 -pady 5 -anchor w -fill y

      #----- Selection des champs

      frame .printbox.par.map.fields -relief sunken -bd 1
         checkbutton .printbox.par.map.fields.lbl -text [lindex $Lbl(Field) $GDefs(Lang)] -bd 1 \
            -variable RSMC::Print(Fields) -indicatoron false -bg $GDefs(ColorHighLight) \
            -command { foreach field $RSMC::Data(Fields) { set RSMC::Print($field) $RSMC::Print(Fields) }}
         pack .printbox.par.map.fields.lbl -side top -ipady 4 -fill x
         foreach field $Data(Fields) {
            set Print($field) 0
            checkbutton .printbox.par.map.fields.f$field -variable RSMC::Print($field) -text $field \
               -indicatoron false -bd 1
            pack .printbox.par.map.fields.f$field -side top -ipady 2 -anchor nw -fill x
         }

      pack .printbox.par.map.fields -side left -pady 5 -anchor w -fill both -expand true

      #----- Selection des especes

      frame .printbox.par.map.isos -relief  sunken -bd 1
         checkbutton .printbox.par.map.isos.lbl -text [lindex $Lbl(Specie) $GDefs(Lang)] -bd 1 \
            -variable RSMC::Print(Isos) -indicatoron false -bg $GDefs(ColorHighLight) \
            -command { foreach iso $Sim(IsoName) { set RSMC::Print($iso) $RSMC::Print(Isos) }}
         pack .printbox.par.map.isos.lbl -side top -fill both -ipady 4
         foreach iso $Sim(IsoName) {
            set Print($iso) 0
            checkbutton .printbox.par.map.isos.s$iso -variable RSMC::Print($iso) -text $iso \
               -indicatoron false -bd 1
            pack .printbox.par.map.isos.s$iso -side top -ipady 2 -anchor nw -fill x
         }
      pack .printbox.par.map.isos -side left -padx 5 -pady 5 -anchor w -fill both -expand true
   pack .printbox.par.map -side top -padx 5 -pady 5 -fill both

   #----- Initialiser pour la carte active

   set Print($Data(IP2))              1
   set Print($Data(ETICKET))          1
   set Print($Data(NOMVAR)) 1

   #----- Constantes pour l'impression par PrintBox

   set PrintBox::Print(Filename) "$Sim(Name)"
   set PrintBox::Print(FullName) "$PrintBox::Print(Path)/$PrintBox::Print(Filename)"
}

#----------------------------------------------------------------------------
# Nom      : <RSMC::JoinTransfert>
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

proc RSMC::JoinTransfert { Frame } {
   global GDefs
   variable Sim
   variable Data
   variable Page
   variable Lbl
   variable Msg

   if { $Sim(Name) == "" } {
      return
   }

   set create [Dialog::CreateDefault . 400 [lindex $Lbl(Warning) $GDefs(Lang)] [lindex $Msg(Join) $GDefs(Lang)] \
      warning 0 [lindex $Lbl(Yes) $GDefs(Lang)] [lindex $Lbl(No) $GDefs(Lang)]]

   if { $create } {
      return
   }

   set data [Viewport::Assigned $Frame $Page(VP) { fstdfield trajectory observation }]
   set path "$GDefs(DirData)/$Sim(NoExp)_$Sim(Name)/Output/RSMCJoin"

   if { ![file exists $path] } {
      file mkdir $path
   }

   #----- Recuperer la RUN

   set file [exec cat $GDefs(DirData)/$Sim(NoExp)_$Sim(Name)/$Sim(Path)/tmp/data_std_sim.eta]
   set run  [string range [lindex [split [lindex $file end] "/"] end] 8 9]
   exec echo $run > $path/RUN.txt

   #----- Recuperer les ip2

   set ip2 [lrange [MetData::ListIP2 $Data(FieldList) MF] 0 3]
   exec echo $ip2 > $path/IP2List.txt

   #----- Demarrer la job

   SPI::Progress 0

   foreach field "MF IT" id "IC TD" {
      set no 1
      foreach hour $ip2 {

         SPI::Progress +0 "Reading Field $field at $hour"
         if { [MetData::Find PRINT [FieldBox::GetFID -1] -1 "" -1 $hour -1 ""  $field] } {

            SPI::Progress +5 "Setting up product for $field at $hour"

            Viewport::UnAssign $Frame $Page(VP)
            Viewport::Assign $Frame $Page(VP) PRINT
            RSMC::LayoutUpdate  $Frame

            SPI::Progress +5 "Printing product for $field at $hour"
            PrintBox::Image $Frame gif $path/L${id}CA_0$no
            exec convert $path/L${id}CA_0$no.gif -resize 280x280 $path/S${id}CA_0$no.gif

            PrintBox::Postscript $Frame $path/L${id}CA_0$no 0 0 [Page::CanvasWidth $Frame] [Page::CanvasHeight $Frame] portrait "8.5_x_11"
         }
         incr no
      }
   }

   Viewport::UnAssign $Frame $Page(VP)
   Viewport::Assign $Frame $Page(VP) $data 1
   RSMC::LayoutUpdate $Frame

   if { [fstdfield is PRINT] } {
      fstdfield free PRINT
   }

   SPI::Progress 0
}

#----------------------------------------------------------------------------
# Nom      : <RSMC::UpdateItems>
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

proc RSMC::UpdateItems { Frame } {
   variable Ico
   variable Sim
   variable Data
   variable Page

   $Frame.page.canvas delete LAYOUTRSMC

   if { [set xy [$Page(VP) -project $Sim(Lat) $Sim(Lon) 0]]!="" && [lindex $xy 2]>0 } {
      $Frame.page.canvas create bitmap [lindex $xy 0] [lindex $xy 1] -bitmap $Ico(Nuclear) -foreground black -tags LAYOUTRSMC
   }

   if { [set xy [$Page(VP) -project [lindex $Data(Max) 1] [lindex $Data(Max) 2] 0]]!="" && [lindex $xy 2]>0 } {
      $Frame.page.canvas create text [lindex $xy 0] [lindex $xy 1] -text "o" -fill black -tag LAYOUTRSMC
   }
}
