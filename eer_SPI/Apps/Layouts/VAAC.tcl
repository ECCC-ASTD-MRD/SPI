#===============================================================================
# Environnement Canada
# Centre Meteorologique Canadian
# 2100 Trans-Canadienne
# Dorval, Quebec
#
# Projet   : SPI (Layouts).
# Fichier  : VAAC.tcl
# Creation : Fevrier 2001 - J.P. Gauthier - CMC/CMOE
#
# Description:
#    Modeles de carte au format VAAC
#
#===============================================================================

namespace eval VAAC {
   variable Lbl
   variable Error
   variable Data
   variable Page

   #----- Definition des labels

   set Lbl(Cancel)      { "Annuler" "Cancel" }
   set Lbl(Hours)       { "Heures" "Hours" }
   set Lbl(Products)    { "Produits" "Products" }
   set Lbl(PrintPlugIn) { "Selection carte (VAAC)" "Map Selection (VAAC)" }
   set Lbl(PrintTitle)  { "Impression (VAAC)" "Map printing (VAAC)" }
   set Lbl(Send)        { "Transmettre" "Transmit" }
   set Lbl(Transmit)    { "Transmission" "Transmission" }
   set Lbl(Transmiting) { "Transmission en cours" "Transmitting chart" }
   set Lbl(WholeMap)    { "Complete" "Whole map" }
   set Lbl(Standard)    { "Standard" "Standard" }
   set Lbl(Left)        { "Gauche" "Left" }
   set Lbl(Right)       { "Droite" "Right" }

   #----- Definitions des messages d'erreurs

   set Error(Hours) { "Aucune heure n'a été sélectionnée." "You did not select any hour." }

   #---- Definitions des entetes et textes de la carte

   set Lbl(FL1VP1)  "FL600"
   set Lbl(FL2VP1)  "FL350"
   set Lbl(FL1VP2)  "FL600"
   set Lbl(FL2VP2)  "FL350"
   set Lbl(FL1VP3)  "FL350"
   set Lbl(FL2VP3)  "FL200"
   set Lbl(FL1VP4)  "FL350"
   set Lbl(FL2VP4)  "FL200"
   set Lbl(FL1VP5)  "FL200"
   set Lbl(FL2VP5)  "SURFACE"
   set Lbl(FL1VP6)  "FL200"
   set Lbl(FL2VP6)  "SURFACE"

   set Lbl(LEFT1)   "CMC"
   set Lbl(LEFT2)   ""
   set Lbl(MID3)    "CONCENTRATIONS"
   set Lbl(SCALEHD) { "LGT-BAS" "MDT-MOYEN" "HVY-ÉLEVÉ" }

   set Lbl(HD1)     "FORECAST OF VISUAL VOLCANIC ASH PLUME"
   set Lbl(HD2)     "PRÉVISION DU PANACHE VISIBLE DE CENDRES VOLCANIQUES"
   set Lbl(FT1)     "SOURCE       :"
   set Lbl(FT2)     "ERUPTION     :"
   set Lbl(FT3)     "DURATION     :"
   set Lbl(FT4)     "ASH CLOUD TOP:"
   set Lbl(FT5)     "CYCLE        :"
   set Lbl(FT6)     "FOR GUIDANCE ONLY"
   set Lbl(FT7)     "NOT AN OFFICIAL FORECAST"
   set Lbl(FT8)     "SEE CURRENT SIGMET FOR WARNING AREA"

   #----- Definitions des constantes relatives au standard VAAC

   set Data(Hour)     ""       ;#Heure selectionnee
   set Data(Hours)    ""       ;#Heures disponibles
   set Data(Index)    ""       ;#Fichier de donnees standard

   set Data(SendSAT)  0        ;#Transmission sur SATNET
   set Data(SendWAS)  0        ;#Transmission sur WAFS WashingtonT
   set Data(SendBRA)  0        ;#Transmission sur WAFS Bracknell
   set Data(SendINT)  0        ;#Transmission sur le reseau interne

   #----- 8.5x11 (80dpi)

   set Page(Border) 15
   set Page(Width)  680
   set Page(Height) 880
}

#-------------------------------------------------------------------------------
# Nom      : <VAAC::DrawScale>
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

proc VAAC::DrawScale { Frame } {
   variable Lbl
   variable Page

   $Frame.page.canvas delete SCALE

   if { [fstdfield is FLD1] && [llength [set levels [fstdfield configure FLD1 -intervals]]]} {

      #----- Determine l'espacement et le point de depart de l'echelle

      set midxloc   [expr $Viewport::Data(Width$Page(VP1))/[llength $levels]]
      set halfwidth [expr $midxloc/2-2]

      #----- Determiner les entetes d'echelle a utiliser

      if { [llength $levels]>3 } {
          set header $levels
      } else {
          set header $Lbl(SCALEHD)
      }

      #----- Pour chaque panneau

      foreach vp "$Page(VP1) $Page(VP2) $Page(VP3) $Page(VP4) $Page(VP5) $Page(VP6)" {

         set no 0

         #----- Creer l'echelle

         foreach level $levels {
            set mid [expr $no*$midxloc+2]

            $Frame.page.canvas create rectangle [expr $Viewport::Data(X$vp) + $mid] [expr $Viewport::Data(Y$vp) - 5 ] \
               [expr $Viewport::Data(X$vp) + $mid + 2*$halfwidth] [expr $Viewport::Data(Y$vp) - 15]  \
               -fill "" -outline black -tag "SCALE"
            $Frame.page.canvas create rectangle [expr $Viewport::Data(X$vp) + $mid + $halfwidth + 10] [expr $Viewport::Data(Y$vp) - 5 ] \
               [expr $Viewport::Data(X$vp) + $mid + 2*$halfwidth] [expr $Viewport::Data(Y$vp) - 15]  \
               -fill "#[fstdfield configure FLD1 -val2map $level]" -outline black -tag "SCALE COLORMAP"
            $Frame.page.canvas create text [expr $Viewport::Data(X$vp) + $mid-2] [expr $Viewport::Data(Y$vp) - 10] -text " [lindex $header $no]" \
               -font XFont8 -tag SCALE -anchor w
            incr no
         }
      }
   }
}

#----------------------------------------------------------------------------
# Nom      : <VAAC::LoadFields>
# Creation : Janvier 1999 - J.P. Gauthier - CMC/CMOE
#
# But      : Lecture et affichage des champs selectionees.
#
# Parametres :
#  <Frame>   : Identificateur de Page
#  <Hours>   : Heures a utiliser
#
# Retour:
#
# Remarques :
#    - Le code utilise comme IP3 et IP1 est 4 pour 350-600, 3 pour 200-350 et 2 pour sfc-200
#      et 1 pour le total.
#    - Le champs utilise est "AV" en MAJUSCULE.
#    - Les heures correspondent a IP2
#
#----------------------------------------------------------------------------

proc VAAC::LoadFields { Frame { Hours {} } } {
   variable Data
   variable Page
   variable Sim

   if { [llength $Hours] } {
      set Data(Hours_) $Hours
   }

   if { $Data(Hours_) == "" } {
      return
   }

   $Frame.page.canvas config -cursor watch
   update idletasks

   set FSTD::Data(List) {}

   Viewport::UnAssign $Frame $Page(VP1)
   Viewport::UnAssign $Frame $Page(VP2)
   Viewport::UnAssign $Frame $Page(VP3)
   Viewport::UnAssign $Frame $Page(VP4)
   Viewport::UnAssign $Frame $Page(VP5)
   Viewport::UnAssign $Frame $Page(VP6)

   set hours       [split $Data(Hours_) _]
   set Data(Hour0) [lindex $hours 0]
   set Data(Hour1) [lindex $hours 1]
   set fid         [FieldBox::GetFID $Data(FieldBox)]

   if { $Data(Hour0) != "" } {
      set stamp [fstdstamp incr $Sim(Stamp0) $Data(Hour0)]

      if { [MetData::Find FLD1 $fid $stamp "" -1 -1 4 "" AV] } {
         Viewport::Assign $Frame $Page(VP1) FLD1
      }

      if { [MetData::Find FLD3 $fid $stamp "" -1 -1 3 "" AV] } {
         Viewport::Assign $Frame $Page(VP3) FLD3
      }

      if { [MetData::Find FLD5 $fid $stamp "" -1 -1 2 "" AV] } {
         Viewport::Assign $Frame $Page(VP5) FLD5
      }
   }

   if { $Data(Hour1) != "" } {
      set stamp [fstdstamp incr $Sim(Stamp0) $Data(Hour1)]

      if { [MetData::Find FLD2 $fid $stamp "" -1 -1 4 "" AV] } {
         Viewport::Assign $Frame $Page(VP2) FLD2
      }
      if { [MetData::Find FLD4 $fid $stamp "" -1 -1 3 "" AV] } {
         Viewport::Assign $Frame $Page(VP4) FLD4
      }
      if { [MetData::Find FLD6 $fid $stamp "" -1 -1 2 "" AV] } {
         Viewport::Assign $Frame $Page(VP6) FLD6
      }
   }

   $Frame.page.canvas configure -cursor left_ptr
}

#-------------------------------------------------------------------------------
# Nom      : <VAAC::DataInit>
# Creation : Fevrier 2001 - J.P. Gauthier - CMC/CMOE
#
# But      : Initialise les donnees
#
# Parametres :
#  <Frame>   : Identificateur de Page
#
# Retour    :
#
# Remarque :
#
#-------------------------------------------------------------------------------

proc VAAC::DataInit { Frame } {
   variable Data
   variable Page
   variable Sim

   set Data(Index)          ""

   set Sim(AccYear)         0
   set Sim(AccMonth)        01
   set Sim(AccDay)          01
   set Sim(AccHour)         00
   set Sim(AccMin)          00
   set Sim(SimYear)         0
   set Sim(SimMonth)        01
   set Sim(SimDay)          01
   set Sim(SimHour)         00
   set Sim(Lat)             0
   set Sim(Lon)             0
   set Sim(Name)            ""
   set Sim(IsoRelease)      0
   set Sim(FnTime)          0
   set Sim(FnVert)          0
   set Sim(EmHeight)        0
   set Sim(EmDuration)      0
   set Sim(EmMass)          0
   set Sim(EmTotalDuration) 0
   set Sim(EmVerticalDist)  ""

   if { ![FieldBox::Exist $Data(FieldBox)] } {
      return
   }

   foreach field [FieldBox::GetContent $Data(FieldBox)] {
      if { [lindex $field 2] == "AV" } {
         lappend Data(Index) $field
      }
   }

   if { [set info [Info::Read [lindex [lindex $Data(Index) 0] 0]]]=="" } {
      return
   }
   eval Info::Decode ::VAAC::Sim \$info

   set Sim(model) [string tolower $Sim(Model)] ; #----- Dispersion model name in lower case.

   #----- Initialiser la liste des heures disponibles (On arrondit a l'heure la plus proche)

   set Sim(Stamp0) [fstdstamp fromdate $Sim(AccYear)$Sim(AccMonth)$Sim(AccDay) $Sim(AccHour)000000]
   if { $Sim(AccMin)>30 } {
      set Sim(Stamp0) [fstdstamp incr $Sim(Stamp0) 1]
   }
   set Sim(StampS) [fstdstamp fromdate $Sim(SimYear)$Sim(SimMonth)$Sim(SimDay) $Sim(SimHour)000000]
   set Data(HourD) [fstdstamp diff $Sim(Stamp0) $Sim(StampS)]

   Log::Print INFO "Difference between Simulation and accident hour: $Data(HourD)"

   set hours [MetData::ListIP2 $Data(Index) AV $Sim(StampS)]

   set vaachour ""
   for { set h 6 } { $h <= [lindex $hours end] } { incr h 6 } {
      if { [expr $h+$Data(HourD)]<=[lindex $hours end] } {
         lappend vaachour $h
      }
   }

   set allhour ""
   for { set h 1 } { $h <= [lindex $hours end] } { incr h } {
      if { [expr $h+$Data(HourD)]<=[lindex $hours end] } {
         lappend allhour $h
      }
   }

   set Data(Hours_) ""
   set Data(Hours)  ""
   foreach { hour1 hour2 } $vaachour {
      lappend Data(Hours) "${hour1}_${hour2}"
   }

   ComboBox::DelAll $Frame.bar.hour
   ComboBox::AddList $Frame.bar.hour $Data(Hours)

   ComboBox::DelAll $Frame.bar.hour0
   ComboBox::AddList $Frame.bar.hour0 $allhour

   ComboBox::DelAll $Frame.bar.hour1
   ComboBox::AddList $Frame.bar.hour1 $allhour

   #----- Initialiser la projection

   Viewport::UnAssign $Frame $Page(VP1)
   Viewport::UnAssign $Frame $Page(VP2)
   Viewport::UnAssign $Frame $Page(VP3)
   Viewport::UnAssign $Frame $Page(VP4)
   Viewport::UnAssign $Frame $Page(VP5)
   Viewport::UnAssign $Frame $Page(VP6)

   #----- Initialiser la localisation par defaut

   set Viewport::Map(Lat) $Sim(Lat)
   set Viewport::Map(Lon) $Sim(Lon)
   set Viewport::Map(LatReset) $Sim(Lat)
   set Viewport::Map(LonReset) $Sim(Lon)

   Viewport::Rotate $Frame

   #----- Parametres des champs

   FSTD::VarMode VAR
   FSTD::Params AV OTH_White2Black -intervalmode NONE 0 -intervals { 10 100 1000 } -factor 1.0 -rendertexture 1 -rendervolume 0 \
      -rendercontour 2 -rendervector NONE -rendergrid 0 -renderlabel 0 -rendervalue 0 -color #000000 -dash "" -desc AV

   VAAC::UpdateItems $Frame
}

#----------------------------------------------------------------------------
# Nom      : <VAAC::Layout>
# Creation : Janvier 2001 - J.P. Gauthier - CMC/CMOE
#
# But      : Initialiser le modele VAAC
#
# Parametres :
#  <Frame>   : Identificateur de Page
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc VAAC::Layout { Frame } {
   global GDefs
   variable Data
   variable Page

   #----- Initialisations des constantes relatives aux projections

   set Viewport::Resources(FillCoast) ""
   set Viewport::Resources(FillLake)  ""
   set Viewport::Resources(Coast)     #000000      ;#Cotes
   set Viewport::Resources(Lake)      #0000ff      ;#Lacs
   set Viewport::Resources(Polit)     #ff0000      ;#Bordures politiques
   set Viewport::Resources(Admin)     #ff0000      ;#Bordures politiques internes
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

   Page::Size $Frame $Page(Width) $Page(Height)

   set Page(VP1) [Viewport::Create $Frame [expr $Page(Border)+30]  [expr $Page(Border)+80] 280 200 0 0]
   set Page(VP2) [Viewport::Create $Frame [expr $Page(Border)+340] [expr $Page(Border)+80] 280 200 0 0]
   set Page(VP3) [Viewport::Create $Frame [expr $Page(Border)+30]  [expr $Page(Border)+335] 280 200 0 0]
   set Page(VP4) [Viewport::Create $Frame [expr $Page(Border)+340] [expr $Page(Border)+335] 280 200 0 0]
   set Page(VP5) [Viewport::Create $Frame [expr $Page(Border)+30]  [expr $Page(Border)+590] 280 200 0 0]
   set Page(VP6) [Viewport::Create $Frame [expr $Page(Border)+340] [expr $Page(Border)+590] 280 200 0 0]

   #----- Cacher les FieldBox

   set Data(FieldBox) $FieldBox::Data(Current)
   catch { wm withdraw .fieldbox$Data(FieldBox) }

   SPI::IcoClear
   VAAC::LayoutInit    $Frame
   VAAC::LayoutToolBar $Frame
   VAAC::DataInit      $Frame
}

#----------------------------------------------------------------------------
# Nom      : <VAAC::LayoutClear>
# Creation : Janvier 2001 - J.P. Gauthier - CMC/CMOE
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

proc VAAC::LayoutClear { Frame } {
   variable Data
   variable Page

   Viewport::Destroy $Frame
   destroy $Frame.bar

   fstdfield free FLD1
   fstdfield free FLD2
   fstdfield free FLD3
   fstdfield free FLD4
   fstdfield free FLD5
   fstdfield free FLD6

   #----- Ramener les FieldBox

   if { [FieldBox::Exist $Data(FieldBox)] } {
      wm deiconify .fieldbox$Data(FieldBox)
   }
}

#----------------------------------------------------------------------------
# Nom      : <VAAC::LayoutToolBar>
# Creation : Janvier 2001 - J.P. Gauthier - CMC/CMOE
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

proc VAAC::LayoutToolBar { Frame } {
   global   GDefs
   variable Lbl

   frame $Frame.bar -relief raised -bd 1
      label $Frame.bar.id -text " VAAC " -relief sunken -bd 1

      menubutton $Frame.bar.prod -bd 1 -text [lindex $Lbl(Products) $GDefs(Lang)] -menu $Frame.bar.prod.menu
      ComboBox::Create $Frame.bar.hour VAAC::Data(Hours_) noedit unsorted nodouble -1 "" 10 8 \
         "VAAC::LoadFields $Frame; VAAC::LayoutUpdate $Frame"

      label $Frame.bar.lbl0 -text [lindex $Lbl(Left) $GDefs(Lang)]
      label $Frame.bar.lbl1 -text [lindex $Lbl(Right) $GDefs(Lang)]
      ComboBox::Create $Frame.bar.hour0 VAAC::Data(Hour0) noedit unsorted nodouble -1 "" 5 8 \
         "VAAC::LoadFields $Frame \$VAAC::Data(Hour0)_\$VAAC::Data(Hour1); VAAC::LayoutUpdate $Frame"
      ComboBox::Create $Frame.bar.hour1 VAAC::Data(Hour1) noedit unsorted nodouble -1 "" 5 8 \
         "VAAC::LoadFields $Frame \$VAAC::Data(Hour0)_\$VAAC::Data(Hour1); VAAC::LayoutUpdate $Frame"

      pack $Frame.bar.id $Frame.bar.prod $Frame.bar.hour $Frame.bar.lbl0 $Frame.bar.hour0 $Frame.bar.lbl1 $Frame.bar.hour1 -side left -fill y

      menu $Frame.bar.prod.menu -tearoff 0 -bd 1 -activeborderwidth 1
         $Frame.bar.prod.menu add command -label "[lindex $Lbl(Transmit) $GDefs(Lang)] ..." \
            -command "VAAC::TransmitSelect $Frame"
   pack $Frame.bar -side top -fill x -before $Frame.page
}

#--------------------------------------.bar--------------------------------------
# Nom      : <VAAC::LayoutInit>
# Creation : Janvier 1999 - J.P. Gauthier - CMC/CMOE
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

proc VAAC::LayoutInit { Frame } {
   global   GDefs
   variable Lbl
   variable Page

   upvar #0 Viewport::Data data

   set canvas $Frame.page.canvas
   set sixth [expr $data(Width$Page(VP1))/6]

   #----- Afficher les parties fixes de la carte

   for { set i 1 } { $i<7 } { incr i } {

      set vp $Page(VP$i)

      #----- Dessiner le pourtour

      $canvas create line $data(X$vp) [expr $data(Y$vp)-50] [expr $data(X$vp)+$data(Width$vp)] [expr $data(Y$vp)-50] \
        [expr $data(X$vp)+$data(Width$vp)] $data(Y$vp)  $data(X$vp) $data(Y$vp) $data(X$vp) [expr $data(Y$vp)-50] -tag FIX

      $canvas create line $data(X$vp) [expr $data(Y$vp)-20] [expr $data(X$vp)+$data(Width$vp)] [expr $data(Y$vp)-20] -tag FIX
      $canvas create line [expr $data(X$vp)+50] [expr $data(Y$vp)-50] [expr $data(X$vp)+50] [expr $data(Y$vp)-20] -tag FIX
      $canvas create line [expr $data(X$vp)+$data(Width$vp)-50] [expr $data(Y$vp)-50] [expr $data(X$vp)+$data(Width$vp)-50] [expr $data(Y$vp)-20] -tag FIX

      #----- Afficher l'entete gauche

      $canvas create text [expr $data(X$vp)+25] [expr $data(Y$vp)-42] -text $Lbl(LEFT1) -font XFont10 -tag FIX
      $canvas create text [expr $data(X$vp)+25] [expr $data(Y$vp)-28] -text $Lbl(LEFT2) -font XFont10 -tag MDL

      #----- Afficher l'entete du centre

      $canvas create text [expr $data(X$vp)+$data(Width$vp)/2] [expr $data(Y$vp)-44] -text "" -font XFont8 -tag MID1$vp
      $canvas create text [expr $data(X$vp)+$data(Width$vp)/2] [expr $data(Y$vp)-34] -text "" -font XFont8 -tag MID2$vp
      $canvas create text [expr $data(X$vp)+$data(Width$vp)/2] [expr $data(Y$vp)-24] -text $Lbl(MID3) -font XFont8 -tag FIX

      #----- Afficher l'entete droite

      $canvas create text [expr $data(X$vp)+$data(Width$vp)-25] [expr $data(Y$vp)-42] -text $Lbl(FL1VP$i) -font XFont10 -tag FIX
      $canvas create text [expr $data(X$vp)+$data(Width$vp)-25] [expr $data(Y$vp)-28] -text $Lbl(FL2VP$i) -font XFont10 -tag FIX

   }

   #----- Afficher l'entete de page

   $canvas create text [expr $Page(Width)/2] [expr $Page(Border)+7] -text $Lbl(HD1) -font XFont20 -tag FIX
   $canvas create text [expr $Page(Width)/2] [expr $Page(Border)+20] -text $Lbl(HD2) -font XFont20 -tag FIX

   #----- Afficher le pied de page

   $canvas create text $data(X$Page(VP1)) [expr $Page(Border)+798] -text $Lbl(FT1) -font XFont10 -anchor w -tag FT1
   $canvas create text $data(X$Page(VP1)) [expr $Page(Border)+806] -text $Lbl(FT2) -font XFont10 -anchor w -tag FT2
   $canvas create text $data(X$Page(VP1)) [expr $Page(Border)+814] -text $Lbl(FT3) -font XFont10 -anchor w -tag FT3
   $canvas create text $data(X$Page(VP1)) [expr $Page(Border)+822] -text $Lbl(FT4) -font XFont10 -anchor w -tag FT4
   $canvas create text $data(X$Page(VP1)) [expr $Page(Border)+830] -text $Lbl(FT5) -font XFont10 -anchor w -tag FT5

   $canvas create text 485 [expr $Page(Border)+798] -text $Lbl(FT6) -font XFont10 -tag FIX
   $canvas create text 485 [expr $Page(Border)+814] -text $Lbl(FT7) -font XFont10 -tag FIX
   $canvas create text 485 [expr $Page(Border)+830] -text $Lbl(FT8) -font XFont10 -tag FIX

   #----- Identificateur du CMC

   $canvas create bitmap [expr $Page(Width)/2] 530 -bitmap @$GDefs(Dir)/Resources/Bitmap/SMC_ver_small.xbm -foreground red  -anchor s -tag "FIX RED"
   $canvas create bitmap [expr $Page(Width)/2] 760 -bitmap @$GDefs(Dir)/Resources/Bitmap/VAAC_ver_small.xbm -foreground red  -anchor s -tag "FIX RED"
   $canvas create bitmap [expr $Page(Width)/2] 280 -bitmap @$GDefs(Dir)/Resources/Bitmap/VAAC_ver_small.xbm -foreground red  -anchor s -tag "FIX RED"
}

#----------------------------------------------------------------------------
# Nom      : <VAAC::LayoutUpdate>
# Creation : Janvier 1999 - J.P. Gauthier - CMC/CMOE
#
# But      : Effectue les changements des parties variables de la cartes.
#
# Parametres :
#  <Frame>   : Identificateur de Page
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc VAAC::LayoutUpdate { Frame } {
   variable Sim
   variable Lbl
   variable Data
   variable Page

   if { ![info exists Sim(Model)] } {
      return
   }

   set canvas $Frame.page.canvas

   set fld0 [lindex [Viewport::Assigned $Frame $Page(VP1) fstdfield] 0]
   set fld1 [lindex [Viewport::Assigned $Frame $Page(VP2) fstdfield] 0]
   set hre0 -1
   set hre1 -1

   #----- Deterniner les heures selectionnees

   if { $fld0!="" } {
      set hre0  [expr int([fstdstamp diff [fstdfield define $fld0 -DATEV] $Sim(Stamp0)])]
   }
   if { $fld1!="" } {
      set hre1  [expr int([fstdstamp diff [fstdfield define $fld1 -DATEV] $Sim(Stamp0)])]
   }

   #----- Afficher les parametres de la premiere heure

   if { $hre0!=-1 } {
      $canvas itemconf MID1$Page(VP1) -text "${hre0}h FORECAST - PRÉVISION ${hre0}h"
      $canvas itemconf MID1$Page(VP3) -text "${hre0}h FORECAST - PRÉVISION ${hre0}h"
      $canvas itemconf MID1$Page(VP5) -text "${hre0}h FORECAST - PRÉVISION ${hre0}h"
      $canvas itemconf MID2$Page(VP1) -text [MetData::FormatDATEV $fld0]
      $canvas itemconf MID2$Page(VP3) -text [MetData::FormatDATEV $fld0]
      $canvas itemconf MID2$Page(VP5) -text [MetData::FormatDATEV $fld0]
   } else {
      $canvas itemconf MID1$Page(VP1) -text ""
      $canvas itemconf MID1$Page(VP3) -text ""
      $canvas itemconf MID1$Page(VP5) -text ""
      $canvas itemconf MID2$Page(VP1) -text ""
      $canvas itemconf MID2$Page(VP3) -text ""
      $canvas itemconf MID2$Page(VP5) -text ""
   }

   #----- Afficher les parametres de la deuxieme heure

   if { $hre1!=-1 } {
      $canvas itemconf MID1$Page(VP2) -text "${hre1}h FORECAST - PRÉVISION ${hre1}h"
      $canvas itemconf MID1$Page(VP4) -text "${hre1}h FORECAST - PRÉVISION ${hre1}h"
      $canvas itemconf MID1$Page(VP6) -text "${hre1}h FORECAST - PRÉVISION ${hre1}h"
      $canvas itemconf MID2$Page(VP2) -text [MetData::FormatDATEV $fld1]
      $canvas itemconf MID2$Page(VP4) -text [MetData::FormatDATEV $fld1]
      $canvas itemconf MID2$Page(VP6) -text [MetData::FormatDATEV $fld1]
   } else {
      $canvas itemconf MID1$Page(VP2) -text ""
      $canvas itemconf MID1$Page(VP4) -text ""
      $canvas itemconf MID1$Page(VP6) -text ""
      $canvas itemconf MID2$Page(VP2) -text ""
      $canvas itemconf MID2$Page(VP4) -text ""
      $canvas itemconf MID2$Page(VP6) -text ""
   }

   #----- Creation du pied de page

   set coord [Convert::FormatCoord $Sim(Lat) $Sim(Lon) MIN]

   $canvas itemconf FT1 -text ""
   $canvas itemconf FT2 -text ""
   $canvas itemconf FT3 -text ""
   $canvas itemconf FT4 -text ""
   $canvas itemconf FT5 -text ""

   #----- Definir la date de l'eruption
   set erupt [clock format [clock scan "$Sim(AccMonth)/$Sim(AccDay)/$Sim(AccYear) $Sim(AccHour)$Sim(AccMin)" -gmt true] -format "%a %b %d %Y, %H:%M UTC" -gmt true]

   $canvas itemconf FT1 -text "$Lbl(FT1) $Sim(Name) $coord"
   $canvas itemconf FT2 -text "$Lbl(FT2) $erupt"
   $canvas itemconf FT4 -text "$Lbl(FT4) FL[expr round($Sim(EmHeight)*0.032808398950131)]"
   $canvas itemconf MDL -text "$Sim(Model)"

   if { $Sim(Model)=="CANERM" } {
      set ReleaseDuration $Sim(EmDuration)         ; #----- Release duration [h].
      set mg   [expr int(log10($Sim(IsoRelease)))] ; #----- Total release mass [micrograms/m3].
      set time [string range $Sim(FnTime) 0 0]     ; #----- Release time function.

      #----- Inital vertical distribution.
      if { $Sim(FnVert) == 0.0 } {
         set vert "Co"
      }
      if { $Sim(FnVert) < 0.0 } {
         set vert "Ga"
      }
      if { $Sim(FnVert) > 0.0 } {
         set vert "Em"
      }

   }

   if { $Sim(Model)=="MLDP0" } {
      set ReleaseDuration [expr double($Sim(EmTotalDuration))/3600.0] ;#----- Convert total release duration from [s] to [h].
      set mg   [expr $Sim(EmMass)==0?0:int(log10($Sim(EmMass)))]      ;#----- Total release mass [micrograms/m3].
      set time "C"                                                    ;#----- Release time function. C: Constant.

      #----- Initial vertical distribution.
      if { [regexp Uniform $Sim(EmVerticalDist)] } {
         set vert "Un"
      } elseif { [regexp Exponenti $Sim(EmVerticalDist)] } {
         set vert "Ex"
      } elseif { $Sim(EmVerticalDist) == "Poisson" } {
         set vert "Po"
      } elseif { [regexp Coni $Sim(EmVerticalDist)] } {
         set vert "Co"
      } elseif { $Sim(EmVerticalDist) == "Champignon" || $Sim(EmVerticalDist) == "Umbrella" } {
         set vert "Um"
      }
   }

   #----- Format release duration.
   set ReleaseDuration [format "%.2f" $ReleaseDuration]
   set TmpRelDur [string trimright $ReleaseDuration "0"]
   if { [string range $TmpRelDur end end] == "." } {
      set indx [expr [string length $TmpRelDur] - 2]
      set ReleaseDuration [string range $TmpRelDur 0 $indx]
   }

   set elev [format "%2.1f" [expr $Sim(EmHeight)/1000.0]] ; #----- Convert maximum initial plume height from [m] to [km].

   $canvas itemconf FT3 -text "$Lbl(FT3) $ReleaseDuration h"
   $canvas itemconf FT5 -text "$Lbl(FT5) ($elev,$mg,T$time,V$vert)"

   VAAC::DrawScale $Frame
}

#----------------------------------------------------------------------------
# Nom      : <VAAC::Transmit>
# Creation : Fevrier 1999 - J.P. Gauthier - CMC/CMOE
#
# But      : Effectue le transfert de la carte sur le circuit SATNET.
#
# Parametres :
#  <Frame>   : Identificateur de Page
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc VAAC::Transmit { Frame } {
   global GDefs env
   variable Print
   variable Sim
   variable Lbl
   variable Error
   variable Data

   set file "$env(HOME)/.spi/Tmp/VAAC_[pid]_[clock seconds]"

   #----- Calculer le pourcentage maximum

   set maxhour 0
   foreach hour $Data(Hours) {
      if { $Print($hour) } {
         incr maxhour
      }
   }
   if { !$maxhour } {
      Dialog::Error . $Error(Hours)
      return
   }

   SPI::Progress 0
   set n [expr 100.0/($maxhour*3)]

   #----- Pour toutes les heures selectionnees

   foreach hour $Data(Hours) {

      if { $Print($hour) } {

         #----- Lire les champs selectionnees si il ne sont pas deja en memoire

         if { $Data(Hours_) != $hour } {
            SPI::Progress +0 "Reading Fields for $hour"
            set Data(Hours_) $hour
            VAAC::LoadFields   $Frame
            VAAC::LayoutUpdate $Frame
         }

         $Frame.page.canvas config -cursor watch
         update idletasks

         #----- Recuperer la RUN

         set run [lindex [fstdstamp todate [fstdfield define FLD1 -DATEO]] 3]

         #----- Generer le fichier image

         SPI::Progress +$n  "Generating postscript for VAAC Map $Data(Hours_)"
         $Frame.page.canvas postscript -x 0 -y 0 -width [Page::CanvasWidth $Frame] -height [Page::CanvasHeight $Frame] \
            -rotate false -colormode color -pageheight 2157p -file $file.ps
         SPI::Progress +$n "Converting to portable bitmap format (1728x2157)"
         exec grep -v showpage $file.ps | convert -density 72 -geometry 1728x2157! - $file.pbm
         catch { exec chmod 644 ${file}.pbm }

         #----- Obtenir le bon numero de transmission

         if { $Data(SentINT) } {
            set no ca0493c
         } else {
            set no [VAAC::TransmitNumber $run [lindex [split $Data(Hours_) _] 0]]
         }

         SPI::Progress +$n "Transmitting VAAC Map $Data(Hours_) with number $no"

         set prefix [clock format [clock seconds] -format "%Y%m%d-%H%MZ" -gmt true]

         if { $Data(SendWAS)==1 || $Data(SendBRA)==1 } {

            #----- On bloque les numeros pour les heures 48 a 66 parce que WAFS n'en veut pas
            #----- Ceci devrait etre gerer par ocxcarte mais ??????

            if { $no!="ca0281c" && $no!="ca0282c" && $no!="ca0287c" && $no!="ca0288c" } {
               SPI::Progress +0 "Sending over WAFS"

               set ErrCatch [catch  { exec $GDefs(Dir)/Script/CMOI_ocxcarte.ksh ${no} ${file}.pbm 0$Data(SendWAS)$Data(SendBRA) $GDefs(TransmitUser) $GDefs(TransmitHost) } MsgCatch ]

               if { $ErrCatch != 0 } {
                  Log::Print ERROR "Unable to send the $file.pbm over WAFS via $GDefs(TransmitUser)@$GDefs(TransmitHost).\n\n$MsgCatch"
               }

               #----- envoyer sur les sites web.

               exec convert ${file}.pbm -resize 680x880 ${file}.png
               catch { exec chmod 644 ${file}.png }
               set ErrCatch [catch  { exec $GDefs(Dir)/Script/CMOI_webprods.ksh ${file}.png eer/data/vaac/current/${prefix}_$Sim(Name)_[string tolower $Sim(Model)]_${hour}.png $GDefs(TransmitUser) $GDefs(TransmitHost) } MsgCatch ]

               if { $ErrCatch != 0 } {
                  Log::Print ERROR "Unable to transfert the $file.png on weatheroffice via $GDefs(TransmitUser)@$GDefs(TransmitHost).\n\n$MsgCatch"
               }

            }
         }
         if { $Data(SendSAT)==1 } {
            SPI::Progress +0 "Sending over SATNET"
            set ErrCatch [catch  { exec $GDefs(Dir)/Script/CMOI_ocxcarte.ksh ${no} ${file}.pbm $Data(SendSAT)00 $GDefs(TransmitUser) $GDefs(TransmitHost) } MsgCatch ]

            if { $ErrCatch != 0 } {
               Log::Print ERROR "Unable to send the $file.pbm over SATNET via $GDefs(TransmitUser)@$GDefs(TransmitHost).\n\n$MsgCatch"
            }

            #----- envoyer sur les sites web.

            exec convert ${file}.pbm -resize 680x880 ${file}.png
            catch { exec chmod 644 ${file}.png }
            set ErrCatch [catch  { exec $GDefs(Dir)/Script/CMOI_webprods.ksh ${file}.png eer/data/vaac/current/${prefix}_$Sim(Name)_[string tolower $Sim(Model)]_${hour}.png $GDefs(TransmitUser) $GDefs(TransmitHost) } MsgCatch ]

            if { $ErrCatch != 0 } {
               Log::Print ERROR "Unable to transfert the $file.png on weatheroffice via $GDefs(TransmitUser)@$GDefs(TransmitHost).\n\n$MsgCatch"
            }
         }
      }
   }

   #----- supprimer les residus

   SPI::Progress +0 "Suppressing temporary files"
   catch { file delete -force $file.ps }
   catch { file delete -force ${file}.png }
   catch { file delete -force ${file}.pbm }
   SPI::Progress 0

   $Frame.page.canvas config -cursor left_ptr
}

#-------------------------------------------------------------------------------
# Nom      : <VAAC::TransmitNumber>
# Creation : Fevrier 1999 - J.P. Gauthier - CMC/CMOE
#
# But      : Selectionner le numero de transmission des cartes sur le reseau
#            SATNET.
#
# Parametres   :
#    <SimHour> : Heure de la run
#    <Hour1>   : Herue de la carte
#
# Retour :
#
# Remarques :
#
#-------------------------------------------------------------------------------

proc VAAC::TransmitNumber { SimHour Hour1 } {

   if { $SimHour == "00" } {
      switch $Hour1 {
         6 { set nomet "ca0277c" }
         18 { set nomet "ca0278c" }
         30 { set nomet "ca0279c" }
         42 { set nomet "ca0280c" }
         54 { set nomet "ca0281c" }
         66 { set nomet "ca0282c" }
       }
   } else {
      switch $Hour1 {
         6 { set nomet "ca0283c" }
         18 { set nomet "ca0284c" }
         30 { set nomet "ca0285c" }
         42 { set nomet "ca0286c" }
         54 { set nomet "ca0287c" }
         66 { set nomet "ca0288c" }
       }
   }
   return $nomet
}

#----------------------------------------------------------------------------
# Nom      : <VAAC::TransmitSelect>
# Creation : Mars 1999 - J.P. Gauthier - CMC/CMOE
#
# But      : Selection des parametres de transmission.
#
# Parametres :
#  <Frame>   : Identificateur de Page
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc VAAC::TransmitSelect { Frame } {
   global GDefs
   variable Lbl
   variable Data
   variable Print

   toplevel     .transmit

   wm title     .transmit [lindex $Lbl(Transmit) $GDefs(Lang)]
   wm transient .transmit .
   wm resizable .transmit 0 0
   wm geometry  .transmit =210x360+[expr [winfo rootx .]+10]+[expr [winfo rooty .]+10]
   grab .transmit

   set Data(SentINT) 0

   labelframe .transmit.c -text Circuit

      frame .transmit.c.l -relief sunken -bd 1
         checkbutton .transmit.c.l.int -variable VAAC::Data(SentINT) -text "Interne" \
            -onvalue 1 -offvalue 0 -indicatoron false -bd 1 \
            -command "if { \$VAAC::Data(SentINT) } {
                      .transmit.c.l.sat configure -state disabled ; set VAAC::Data(SendSAT) 1
                      .transmit.c.l.was configure -state disabled ; set VAAC::Data(SendWAS) 0
                      .transmit.c.l.bra configure -state disabled ; set VAAC::Data(SendBRA) 0
                   } else {
                      .transmit.c.l.sat configure -state normal
                      .transmit.c.l.was configure -state normal
                      .transmit.c.l.bra configure -state normal
                   }"
         checkbutton .transmit.c.l.sat -variable VAAC::Data(SendSAT) -text "SATNET" -onvalue 1 -offvalue 0 -indicatoron false -bd 1
         checkbutton .transmit.c.l.was -variable VAAC::Data(SendWAS) -text "WAFS (NWS)" -onvalue 1 -offvalue 0 -indicatoron false -bd 1
         checkbutton .transmit.c.l.bra -variable VAAC::Data(SendBRA) -text "WAFS (UKmet)" -onvalue 1 -offvalue 0 -indicatoron false -bd 1
         pack .transmit.c.l.int .transmit.c.l.sat .transmit.c.l.was .transmit.c.l.bra \
            -side top -fill x -ipady 2
      pack .transmit.c.l -side top -fill x -padx 5 -pady 5
   pack .transmit.c -side top -fill x -padx 5 -pady 5

   labelframe .transmit.h -text [lindex $Lbl(Hours) $GDefs(Lang)]
      frame .transmit.h.l -relief sunken -bd 1

         set nb 0
         foreach hour $Data(Hours) {
            set Print($hour) 0

            #----- On ne peut transmettre que jusqua 72 heures

            if { $nb < 6 } {
               checkbutton .transmit.h.l.$hour -variable VAAC::Print($hour) -text $hour -onvalue 1 -offvalue 0 -indicatoron false -bd 1
              pack .transmit.h.l.$hour -side top -fill x -ipady 2
            }
            incr nb
         }
       pack .transmit.h.l -side top -fill x -padx 5 -pady 5
   pack .transmit.h -side top -padx 5 -fill both -expand true

   frame .transmit.command
      button .transmit.command.cancel -text [lindex $Lbl(Cancel) $GDefs(Lang)] -bd 1 -command "destroy .transmit"
      button .transmit.command.send -text [lindex $Lbl(Send) $GDefs(Lang)] -bd 1 -command "destroy .transmit ; VAAC::Transmit $Frame"
      pack .transmit.command.cancel .transmit.command.send -side left -fill x -expand true
   pack .transmit.command -side top -padx 5 -pady 5 -fill x
}

#----------------------------------------------------------------------------
# Nom      : <VAAC::PrintWidget>
# Creation : Mars 1999 - J.P. Gauthier - CMC/CMOE
#
# But      : Initialise un "widget" "plugin" pour les cas VAAC a l'interieur
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

proc VAAC::PrintWidget { Frame } {
   global GDefs
   variable Sim
   variable Print
   variable Data
   variable Lbl

   wm geom  .printbox 335x335

   labelframe .printbox.par.map -text [lindex $Lbl(PrintPlugIn) $GDefs(Lang)]

      #----- Selection des heures

      frame .printbox.par.map.hours -relief sunken -bd 1
         checkbutton .printbox.par.map.hours.lbl -text [lindex $Lbl(Hours) $GDefs(Lang)] \
            -indicatoron false -bg $GDefs(ColorHighLight) -bd 1 -variable VAAC::Data(PrintHours) \
            -command { foreach hour $VAAC::Data(Hours) { set VAAC::Print($hour) $VAAC::Data(PrintHours) }}
         pack .printbox.par.map.hours.lbl -side top -fill x

         frame .printbox.par.map.hours.list
         set column 0
         set row 0

         foreach hour $Data(Hours) {
            set Print($hour) 0
            checkbutton .printbox.par.map.hours.list.$hour -variable VAAC::Print($hour) -text $hour \
               -indicatoron false -bd 1 -onvalue 1 -offvalue 0
            grid .printbox.par.map.hours.list.$hour -sticky ew -ipady 2 -ipadx 2 \
               -column $column -row $row

            if { $row >= 2 } {
               incr column
               set row 0
            } else {
               incr row
            }
         }

      pack .printbox.par.map.hours.list -side top -anchor w
      pack .printbox.par.map.hours -side left -padx 5 -pady 5 -anchor w -fill both -expand true

      frame .printbox.par.map.panel -relief sunken -bd 1
         checkbutton .printbox.par.map.panel.tout -text [lindex $VAAC::Lbl(WholeMap) $GDefs(Lang)] \
            -variable VAAC::Print(All) -bd 1 -indicatoron false
         pack .printbox.par.map.panel.tout -side top -fill x

         frame .printbox.par.map.panel.niveau
            frame .printbox.par.map.panel.niveau.fl600
               checkbutton .printbox.par.map.panel.niveau.fl600.1 -text 1 -variable VAAC::Print(VP1) -bd 1 -indicatoron false
               checkbutton .printbox.par.map.panel.niveau.fl600.2 -text 2 -variable VAAC::Print(VP2) -bd 1 -indicatoron false
               pack .printbox.par.map.panel.niveau.fl600.1 .printbox.par.map.panel.niveau.fl600.2 -ipady 2 -side left -fill both -expand true
            frame .printbox.par.map.panel.niveau.fl350
               checkbutton .printbox.par.map.panel.niveau.fl350.1 -text 3 -variable VAAC::Print(VP3) -bd 1 -indicatoron false
               checkbutton .printbox.par.map.panel.niveau.fl350.2 -text 4 -variable VAAC::Print(VP4) -bd 1 -indicatoron false
               pack .printbox.par.map.panel.niveau.fl350.1 .printbox.par.map.panel.niveau.fl350.2 -ipady 2 -side left -fill both -expand true
            frame .printbox.par.map.panel.niveau.fl200
               checkbutton .printbox.par.map.panel.niveau.fl200.1 -text 5 -variable VAAC::Print(VP5) -bd 1 -indicatoron false
               checkbutton .printbox.par.map.panel.niveau.fl200.2 -text 6 -variable VAAC::Print(VP6) -bd 1 -indicatoron false
               pack .printbox.par.map.panel.niveau.fl200.1 .printbox.par.map.panel.niveau.fl200.2 -ipady 2 -side left -fill both -expand true
            pack .printbox.par.map.panel.niveau.fl600 .printbox.par.map.panel.niveau.fl350 \
               .printbox.par.map.panel.niveau.fl200 -fill both
         pack .printbox.par.map.panel.niveau -fill both
      pack .printbox.par.map.panel -side right -padx 5 -pady 5 -anchor e -fill y
   pack .printbox.par.map -side top -padx 5 -pady 5 -fill both

   #----- Initialiser pour la carte active

   set Print($Data(Hours_)) 1
   set Print(All)           1

   #----- Constantes pour l'impression par PrintBox

   set PrintBox::Param(Filename) "$Sim(Name)_[string tolower $Sim(Model)]"
   set PrintBox::Param(FullName) "$PrintBox::Param(Path)/$PrintBox::Param(Filename)"
}

#----------------------------------------------------------------------------
# Nom      : <VAAC::PrintCommand>
# Creation : Mars 1999 - J.P. Gauthier - CMC/CMOE
#
# But      : Creer la commande d'impression "plugin" pour les cas VAAC a l'interieur
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

proc VAAC::PrintCommand { Frame } {
   variable Lbl
   variable Print
   variable Data
   variable Page

   #----- Calculer le pourcentage maximum

   set maxhour 0
   foreach hour $Data(Hours) {
      if { $Print($hour) } {
         incr maxhour
      }
   }
   set maxvp 0
   foreach vp { All VP1 VP2 VP3 VP4 VP5 VP6 } {
      if { $Print($vp) } {
         incr maxvp
      }
   }

   InfoFrame::Set100 .printbox.job [expr ($maxhour-1)*6+$maxhour*$maxvp*5]

   #----- Pour toutes les heures selectionnees

   foreach hour $Data(Hours) {

      if { $Print($hour) } {

         #----- Lire les champs selectionnees si il ne sont pas deja en memoire

         if { $Data(Hours_) != $hour } {
            InfoFrame::Incr .printbox.job 1 "Reading Fields for $hour"
            set Data(Hours_) $hour
            VAAC::LoadFields $Frame
            InfoFrame::Incr .printbox.job 1 "Updating Fields for $hour"
            VAAC::LayoutUpdate $Frame
         }

         #----- Pour tout les panneaux selectionnee

         foreach vp { All VP1 VP2 VP3 VP4 VP5 VP6 } idx { 0 0 1 0 1 0 1 } {

            if { $Print($vp) } {

               if { $vp != "All" } {
                  set lbl [lindex [split $hour _] $idx]
                  InfoFrame::Incr .printbox.job 1 "Generating VAAC Map $Lbl(FL1$vp)_$Lbl(FL2$vp) ${lbl}"
                  set PrintBox::Param(FullName) "$PrintBox::Param(Path)/$PrintBox::Param(Filename)_$Lbl(FL1$vp)_$Lbl(FL2$vp)_${lbl}"
                  PrintBox::Print $Frame $Viewport::Data(X$Page($vp)) [expr $Viewport::Data(Y$Page($vp)) - 50]\
                     $Viewport::Data(Width$Page($vp)) [expr $Viewport::Data(Height$Page($vp)) + 50]
               } else {
                  InfoFrame::Incr .printbox.job 1 "Generating VAAC Map ${hour}"
                  set PrintBox::Param(FullName) "$PrintBox::Param(Path)/$PrintBox::Param(Filename)_${hour}"
                  PrintBox::Print $Frame 0 0 [Page::CanvasWidth $Frame] [Page::CanvasHeight $Frame]
               }
            }
         }
      }
   }
   PrintBox::Destroy
}

#----------------------------------------------------------------------------
# Nom      : <VAAC::UpdateItems>
# Creation : Janvier 1999 - J.P. Gauthier - CMC/CMOE
#
# But      : Affiche l'icone du volcan sur la carte.
#
# Parametres :
#  <Frame>   : Identificateur de Page
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc VAAC::UpdateItems { Frame } {
   variable Sim
   variable Page

   $Frame.page.canvas delete LAYOUTVAACLOC

   foreach vp "$Page(VP1) $Page(VP2) $Page(VP3) $Page(VP4) $Page(VP5) $Page(VP6)" {
      if { [set xy [$vp -project $Sim(Lat) $Sim(Lon) 0]]!="" && [lindex $xy 2]>0 } {
         Shape::DrawIcoVAAC $Frame.page.canvas $xy "LAYOUTVAACLOC" black 5 False
      }
   }
}
