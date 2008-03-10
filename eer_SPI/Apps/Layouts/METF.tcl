#===============================================================================
# Environnement Canada
# Centre Meteorologique Canadian
# 2100 Trans-Canadienne
# Dorval, Quebec
#
# Projet   : SPI (Layouts).
# Fchier  : METF_Map.tcl
# Creation : Juillet 2001 - J.P. Gauthier - CMC/CMOE
#
# Description:
#    Modeles de carte au format METF
#
# Modifications :
#
#   Nom         : -
#   Date        : -
#   Description : -
#
#===============================================================================

namespace eval METF {
   variable Lbl
   variable Data
   variable Page

   #----- Definition des labels

   set Lbl(Cancel)      { "Annuler" "Cancel" }
   set Lbl(Hours)       { "Heures" "Hours" }
   set Lbl(Products)    { "Produits" "Products" }
   set Lbl(PrintPlugIn) { "Selection carte (METF)" "Map Selection (METF)" }
   set Lbl(PrintTitle)  { "Impression (METF)" "Map printing (METF)" }
   set Lbl(WholeMap)    { "Complete" "Whole map" }

   #----- Definitions des constantes

   set Data(Hour)      ""                                    ;#Heure selectionnee
   set Data(Hours)     ""                                    ;#Liste des heures disponibles
   set Data(Index)     ""                                    ;#Fichier de donnees standard

   #---- Definitions des entetes et textes de la carte

   set Data(HD1)       "Meteorological fields"
   set Data(DESCVP1)   "GZ + Wind 850 HPA"
   set Data(DESCVP2)   "GZ + Wind 1000 HPA"
   set Data(DESCVP3)   "HR + GZ + Wind 700 HPA"
   set Data(DESCVP4)   "GZ + Wind 500 HPA"

   #----- 8.5x11 (80dpi)

   set Page(Border) 15
   set Page(Width)  790
   set Page(Height) 860
}

#-------------------------------------------------------------------------------
# Nom      : < METF::DrawScale>
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
# Modifications  :
#    Nom         : -
#    Date        : -
#    Description : -
#-------------------------------------------------------------------------------

proc  METF::DrawScale { Frame } {
   variable Page

   $Frame.page.canvas delete LAYOUTMETFSCALE

   #----- Determine l'espacement et le point de depart de l'echelle

   set start [expr $Viewport::Data(Height$Page(VP3)) + $Viewport::Data(Y$Page(VP3))]
   set i 0

   foreach level [fstdfield configure HR700 -intervals] {
      $Frame.page.canvas create rectangle [expr $Viewport::Data(Width$Page(VP3))+$Viewport::Data(X$Page(VP3))+5] [expr $start-10] \
      [expr $Viewport::Data(Width$Page(VP3))+$Viewport::Data(X$Page(VP3))+35] [expr $start-20] \
         -fill "#[fstdfield configure HR700 -val2map $level]" -outline black -tags "LAYOUTMETFSCALE LAYOUTMETFCOLORMAP"
      $Frame.page.canvas create text [expr $Viewport::Data(Width$Page(VP3))+$Viewport::Data(X$Page(VP3))+20] [expr $start] \
         -text [format "%1.2f" $level] -font XFont10 -anchor s -fill black -tags "LAYOUTMETFSCALE"

      set start [expr $start-30]
      incr i
   }
}

#-------------------------------------------------------------------------------
# Nom      : <METF::LoadFields>
# Creation : Mars 1999 - J.P. Gauthier - CMC/CMOE
#
# But      : Selectione le champs, en fait la lecture et le passe a la projection
#            pour affichage selon les parametres selectionne.
#
# Parametres :
#  <Frame>   : Identificateur de Page
#
# Retour    :
#
# Remarque :
#
# Modifications  :
#    Nom         : -
#    Date        : -
#    Description : -
#-------------------------------------------------------------------------------

proc METF::LoadFields { Frame } {
   variable Data
   variable Page

   #----- Si toutes les valeurs sont specifiees

   if { $Data(Hour) == "" } {
      return
   }

   $Frame.page.canvas config -cursor watch
   update idletasks

   #----- Lire les champs selectionne

   Viewport::UnAssign $Frame $Page(VP1)
   Viewport::UnAssign $Frame $Page(VP2)
   Viewport::UnAssign $Frame $Page(VP3)
   Viewport::UnAssign $Frame $Page(VP4)

   set ip2 $Data(Hour)

   foreach field $Data(Index) {
      if { [lindex $field 5] == $ip2 } {
         set file [lindex $field 0]
         break
      }
   }

   Debug::TraceProc "Reading field -1 ________ _ $ip2 -1 _ (GZ,UU,ES,HR)"

   fstdfield read GZ850  $file -1 "" 850  $ip2 -1 "" GZ
   fstdfield read WD850  $file -1 "" 850  $ip2 -1 "" UU
   fstdfield read GZ1000 $file -1 "" 1000 $ip2 -1 "" GZ
   fstdfield read WD1000 $file -1 "" 1000 $ip2 -1 "" UU
   fstdfield read GZ700  $file -1 "" 700  $ip2 -1 "" GZ
   fstdfield read WD700  $file -1 "" 700  $ip2 -1 "" UU
   fstdfield read HR700  $file -1 "" 700  $ip2 -1 "" HR
   fstdfield read GZ500  $file -1 "" 500  $ip2 -1 "" GZ
   fstdfield read WD500  $file -1 "" 500  $ip2 -1 "" UU

   Viewport::Assign $Frame $Page(VP1) "GZ850 WD850"
   Viewport::Assign $Frame $Page(VP2) "GZ1000 WD1000"
   Viewport::Assign $Frame $Page(VP3) "HR700 GZ700 WD700"
   Viewport::Assign $Frame $Page(VP4) "GZ500 WD500"

   $Frame.page.canvas config -cursor left_ptr
}

#-------------------------------------------------------------------------------
# Nom      : <METF::DataInit>
# Creation : Mars 1999 - J.P. Gauthier - CMC/CMOE
#
# But      : Initialise les valeurs relatives a l'experience en cours.
#
# Parametres :
#  <Frame>   : Identificateur de Page
#
# Retour    :
#
# Remarque :
#
# Modifications  :
#    Nom         : -
#    Date        : -
#    Description : -
#-------------------------------------------------------------------------------

proc METF::DataInit { Frame } {
   variable Sim
   variable Data
   variable Page

   set Data(Index) ""
   set Sim(Lat)          0
   set Sim(Lon)          0
   set Sim(Name)         ""

   if { ![FieldBox::Exist $Data(FieldBox)] } {
      return
   }

   foreach field [FieldBox::GetContent $Data(FieldBox)] {
      if { [lindex $field 2]=="HR" } {
         lappend Data(Index) $field
      }
      if { [lindex $field 2]=="OL" || [lindex $field 2]=="INFO" } {
         set info $field
      }
   }

   if { [set info [Info::Read [lindex [lindex $info 0] 0]]]=="" } {
      return
   }
   eval Info::Decode ::METF::Sim \$[Info::Strip $info Model]::Sim(Info) \$info

   #----- Updater les informations sur la source

   set coord [Convert::FormatCoord $Sim(Lat) $Sim(Lon) DEG]
   $Frame.page.canvas itemconf LAYOUTMETFSOURCE   -text "Source name     : $Sim(Name)"
   $Frame.page.canvas itemconf LAYOUTMETFCOORD    -text "Source location : $coord"

   #----- Initialiser les parametres des champs

   FSTD::VarMode VAR

   FSTD::Params HR OTH_White2Black -intervalmode NONE 0 -intervals { .7 .8 .9 } -factor 1.0 -rendertexture 1 -rendervolume 0 \
      -rendercontour 2 -rendervector NONE -rendergrid 0 -renderlabel 0 -rendervalue 0 -color #000000 -dash "" -desc HR
   FSTD::Params GZ OTH_White2Black -intervalmode INTERVAL 6 -intervals { } -factor 1.0 -rendertexture 0 -rendervolume 0 \
      -rendercontour 2 -rendervector NONE -rendergrid 0 -renderlabel 3 -rendervalue 0 -color #000000 -dash "" -value INTEGER 0 -desc GZ
   FSTD::Params UU OTH_White2Black -intervalmode NONE 0 -intervals { } -factor 1.0 -rendertexture 0 -rendervolume 0 \
      -rendercontour 0 -rendervector BARBULE -rendergrid 0 -renderlabel 0 -rendervalue 0 -color #000000 -dash "" -desc UU

   #----- Initialiser la projection

   Viewport::UnAssign $Frame $Page(VP1)
   Viewport::UnAssign $Frame $Page(VP2)
   Viewport::UnAssign $Frame $Page(VP3)
   Viewport::UnAssign $Frame $Page(VP4)

   #----- Initialiser les listes

   set Data(Hour) ""

   set Data(Hours) [MetData::ListIP2 $Data(Index) HR]
   ComboBox::DelAll $Frame.bar.hour
   ComboBox::AddList $Frame.bar.hour $Data(Hours)

   #----- Initialiser la localisation par defaut

   set Viewport::Map(Lat) $Sim(Lat)
   set Viewport::Map(Lon) $Sim(Lon)
   set Viewport::Map(LatReset) $Sim(Lat)
   set Viewport::Map(LonReset) $Sim(Lon)

   Viewport::Rotate $Frame

   METF::UpdateItems $Frame
}

#----------------------------------------------------------------------------
# Nom      : <METF::Layout>
# Creation : Juillet 2001 - J.P. Gauthier - CMC/CMOE
#
# But      : Initialise le modele METF
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

proc METF::Layout { Frame } {
   global GDefs
   variable Page
   variable Data

   #----- Initialisations des constantes relatives aux projections

   set Viewport::Map(Border)   1
   set Viewport::Map(LatLon)   1

   Page::Size $Frame $Page(Width) $Page(Height)
   set Page(VP1) [Viewport::Create $Frame [expr $Page(Border)+10]  [expr $Page(Border)+50] 350 350 0 0]
   set Page(VP2) [Viewport::Create $Frame [expr $Page(Border)+400] [expr $Page(Border)+50] 350 350 0 0]
   set Page(VP3) [Viewport::Create $Frame [expr $Page(Border)+10]  [expr $Page(Border)+430] 350 350 0 0]
   set Page(VP4) [Viewport::Create $Frame [expr $Page(Border)+400] [expr $Page(Border)+430] 350 350 0 0]

   #----- Cacher les FieldBox

   set Data(FieldBox) $FieldBox::Data(Current)
   catch { wm withdraw .fieldbox$Data(FieldBox) }

   SPI::IcoClear
   METF::LayoutToolBar $Frame
   METF::LayoutInit    $Frame
   METF::DataInit      $Frame
}

#----------------------------------------------------------------------------
# Nom      : <METF::LayoutClear>
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
# Modifications :
#
#    Nom         : -
#    Date        : -
#    Description : -
#----------------------------------------------------------------------------

proc METF::LayoutClear { Frame } {
   variable Data

   Viewport::Destroy $Frame
   destroy $Frame.bar

   fstdfield free GZ850
   fstdfield free WD850
   fstdfield free GZ1000
   fstdfield free WD1000
   fstdfield free GZ500
   fstdfield free WD500
   fstdfield free GZ700
   fstdfield free WD700
   fstdfield free HR700

   #----- Ramener les FieldBox

   if { [FieldBox::Exist $Data(FieldBox)] } {
      wm deiconify .fieldbox$Data(FieldBox)
   }
}

#----------------------------------------------------------------------------
# Nom      : <METF::LayoutToolBar>
# Creation : Mars 1999 - J.P. Gauthier - CMC/CMOE
#
# But      : Initialise l'affichage du menu
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

proc METF::LayoutToolBar { Frame } {
   global GDefs
   variable Lbl

   frame $Frame.bar -relief raised -bd 1
      label $Frame.bar.id -text " METF " -relief sunken -bd 1
      ComboBox::Create $Frame.bar.hour METF::Data(Hour) noedit unsorted nodouble -1 "" 10 6 \
         "METF::LoadFields $Frame ; METF::LayoutUpdate $Frame"
      pack $Frame.bar.id $Frame.bar.hour -side left -fill y
   pack $Frame.bar -side top -fill x -before $Frame.page
}

#----------------------------------------------------------------------------
# Nom      : <METF::LayoutInit>
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
# Modifications :
#
#    Nom         : -
#    Date        : -
#    Description : -
#----------------------------------------------------------------------------

proc METF::LayoutInit { Frame } {
   global   GDefs
   variable Data
   variable Page

   set canvas $Frame.page.canvas

   $canvas create line $Page(Border) $Page(Border) $Page(Border) [expr $Page(Height)-$Page(Border)] \
      [expr $Page(Width)-$Page(Border)] [expr $Page(Height)-$Page(Border)] [expr $Page(Width)-$Page(Border)] $Page(Border) \
      $Page(Border) $Page(Border) -fill black -tags defs

   for { set i 1 } { $i<5 } { incr i } {
      set vp $Page(VP$i)

      #----- Dessiner le pourtour

      $canvas create rectangle $Viewport::Data(X$vp) $Viewport::Data(Y$vp) [expr $Viewport::Data(X$vp)+$Viewport::Data(Width$vp)] [expr $Viewport::Data(Y$vp)-20] \
         -outline black -fill "" -tags defs
      $canvas create text [expr $Viewport::Data(X$vp) + 175] [expr $Viewport::Data(Y$vp) - 10] -text "$Data(DESCVP$i)" -fill black \
         -font XFont14 -tags defs
   }

   #----- Afficher l'identification de la source

   Shape::DrawIcoMETF $canvas [list [expr $Page(Border)+285] [expr $Page(Border)+15]] "FIX" black 8 False
   $canvas create text [expr $Page(Border)+305] [expr $Page(Border)+8] -font XFont12 -anchor w -text "" -tags LAYOUTMETFSOURCE
   $canvas create text [expr $Page(Border)+305] [expr $Page(Border)+22] -font XFont12 -anchor w -text "" -tags LAYOUTMETFCOORD

   #----- Afficher le drapeau

   $canvas create bitmap [expr $Page(Width)/2] [expr $Page(Height)/2] \
      -bitmap @$GDefs(Dir)/Resources/Bitmap/SMC_ver_small.xbm -foreground red  -tags "FIX RED"

   #----- Afficher la legende du bas

   $canvas create text [expr $Page(Width)/2] [expr $Page(Border)+800] -font XFont14 -anchor s -text $Data(HD1) -tags LAYOUTMETFHD1
   $canvas create text [expr $Page(Width)/2] [expr $Page(Border)+815] -font XFont14 -anchor s -text "" -tags LAYOUTMETFHD2
   $canvas create text [expr $Page(Width)/2] [expr $Page(Border)+830] -font XFont14 -anchor s -text "" -tags LAYOUTMETFHD3

   $canvas create text [expr $Page(Width)-1] [expr $Page(Height)-1] -font XFont10 -anchor se -text "[clock format [clock seconds] -format "%H%M %d/%m/%Y" -gmt true]" -tags FTID
}

#----------------------------------------------------------------------------
# Nom      : <METF::LayoutUpdate>
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
# Modifications :
#
#    Nom         : -
#    Date        : -
#    Description : -
#----------------------------------------------------------------------------

proc METF::LayoutUpdate { Frame } {
   variable Page

   set field [lindex [Viewport::Assigned $Frame $Page(VP1) fstdfield] 0]
   if { $field=="" } {
      return
   }

   $Frame.page.canvas itemconf LAYOUTMETFHD2 -text "T0+[fstdfield define $field -IP2]"
   $Frame.page.canvas itemconf LAYOUTMETFHD3 -text "(Valid [MetData::FormatDATEV $field])"

   #----- Afficher les informations complementaires

   METF::DrawScale $Frame
}

#----------------------------------------------------------------------------
# Nom      : <METF::PrintWidget>
# Creation : Fevrier 1999 - J.P. Gauthier - CMC/CMOE
#
# But      : Initialise un "widget" "plugin" pour les cas METF a l'interieur
#            de la PrintBox.
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

proc METF::PrintWidget { Frame } {
   global GDefs
   variable Sim
   variable Print
   variable Data
   variable Lbl

   wm geom  .printbox 335x400

   labelframe .printbox.par.map -text [lindex $Lbl(PrintPlugIn) $GDefs(Lang)]

     #----- Selection des heures

      frame .printbox.par.map.hours -relief sunken -bd 1
         checkbutton .printbox.par.map.hours.lbl -text [lindex $Lbl(Hours) $GDefs(Lang)] -bd 1 \
            -indicatoron false -bg $GDefs(ColorHighLight) -variable METF::Data(PrintHours) \
            -command { foreach hour $METF::Data(Hours) { set METF::Print($hour) $METF::Data(PrintHours) }}
         pack .printbox.par.map.hours.lbl -side top -fill x -ipady 4

         frame .printbox.par.map.hours.list
         set column 0
         set row 0

         foreach hour $Data(Hours) {
            set Print($hour) 0
            checkbutton .printbox.par.map.hours.list.$hour -variable METF::Print($hour) -text " $hour " \
               -indicatoron false -bd 1
            grid .printbox.par.map.hours.list.$hour  -sticky ew -ipady 2 -ipadx 5 \
               -column $column -row $row

            if { $row >= 5 } {
               incr column
               set row 0
            } else {
               incr row
            }
         }
      pack .printbox.par.map.hours.list -side top -anchor w
      pack .printbox.par.map.hours -side left -padx 5 -pady 5 -anchor w -fill both -expand true

      frame .printbox.par.map.panel -relief sunken -bd 1
         checkbutton .printbox.par.map.panel.tout -text [lindex $Lbl(WholeMap) $GDefs(Lang)] \
            -variable METF::Print(All) -bd 1 -indicatoron false
         pack .printbox.par.map.panel.tout -side top -fill x -ipady 4

         frame .printbox.par.map.panel.niveau
            frame .printbox.par.map.panel.niveau.l1
               checkbutton .printbox.par.map.panel.niveau.l1.1 -text 1 -variable METF::Print(VP1) \
                  -indicatoron false -bd 1
               checkbutton .printbox.par.map.panel.niveau.l1.2 -text 2 -variable METF::Print(VP2) \
                  -indicatoron false -bd 1
               pack .printbox.par.map.panel.niveau.l1.1 .printbox.par.map.panel.niveau.l1.2 \
                  -ipady 2 -side left -fill both -expand true
            pack .printbox.par.map.panel.niveau.l1 -fill both -expand true

            frame .printbox.par.map.panel.niveau.l2
               checkbutton .printbox.par.map.panel.niveau.l2.1 -text 3 -variable METF::Print(VP3) \
                  -indicatoron false -bd 1
               checkbutton .printbox.par.map.panel.niveau.l2.2 -text 4 -variable METF::Print(VP4) \
                  -indicatoron false -bd 1
               pack .printbox.par.map.panel.niveau.l2.1 .printbox.par.map.panel.niveau.l2.2 \
                  -ipady 2 -side left -fill both -expand true
            pack .printbox.par.map.panel.niveau.l2 -fill both -expand true

         pack .printbox.par.map.panel.niveau -fill x
      pack .printbox.par.map.panel -side right -padx 5 -pady 5 -anchor ne

   pack .printbox.par.map -side top -padx 5 -pady 5 -fill both

   #----- Initialiser pour la carte active

   set Print($Data(Hour))    1
   set Print(All)            1

   #----- Constantes pour l'impression par PrintBox

   set PrintBox::Print(Filename) "$Sim(Name)"
   set PrintBox::Print(FullName) "$PrintBox::Print(Path)/$PrintBox::Print(Filename)"
}

#----------------------------------------------------------------------------
# Nom      : <METF::PrintCommand>
# Creation : Mars 1999 - J.P. Gauthier - CMC/CMOE
#
# But      : Creer la commande d'impression "plugin" pour les cas METF a l'interieur
#            de la PrintBox.
#
# Parametres :
#   <Frame>  : Identificateur de Page
#
# Retour:
#
# Remarques :
#   Cette fonction est appelee par PrintBox pour effectuer l'impression selon les
#   parametres du PrintBox lui-meme et du "Widget PlugIn" PrintBox::PlugInCommand
#   definit ci-haut.
#
# Modifications :
#
#    Nom         : -
#    Date        : -
#    Description : -
#----------------------------------------------------------------------------

proc METF::PrintCommand { Frame } {
   global GDefs
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
   foreach vp { All VP1 VP2 VP3 VP4 } {
      if { $Print($vp) } {
         incr maxvp
      }
   }

   #----- Calculer le maximum en pourcentage (nombres de cartes * (2 + FTP?)

   InfoFrame::Set100 .printbox.job [expr $maxhour*$maxvp*5]

   #----- Pour toutes les heures selectionnees

   foreach hour $Data(Hours) {

      if { $Print($hour) } {

         #----- Lire le champs selectionnee si il n'est pas deja en memoire

          if { $Data(Hour) != $hour } {
            InfoFrame::Incr .printbox.job 1 "Reading Fields for hour $hour"
            set Data(Hour) $hour
            LoadFields $Frame
            LayoutUpdate $Frame
         }

         #----- Pour tout les panneaux selectionnee

         foreach vp { All VP1 VP2 VP3 VP4 } lbl { "" 850 1000 700 500 } {

            #----- Appeler la fonction d'impression du PrintBox

            if { $Print($vp) } {
               if { $vp != "All" } {
                  InfoFrame::Incr .printbox.job 1 "Generating METF Map for level ${lbl}"
                  set PrintBox::Print(FullName) "$PrintBox::Print(Path)/$PrintBox::Print(Filename).${lbl}.${hour}"
                  PrintBox::Print $Frame $Viewport::Data(X$Page($vp)) [expr $Viewport::Data(Y$Page($vp))-20] \
                     $Viewport::Data(Width$Page($vp)) [expr $Viewport::Data(Height$Page($vp))+20]
               } else {
                  set PrintBox::Print(FullName) "$PrintBox::Print(Path)/$PrintBox::Print(Filename).${hour}"
                  InfoFrame::Incr .printbox.job 1 "Generating METF Map $PrintBox::Print(FullName)"
                  PrintBox::Print $Frame 0 0 [Page::CanvasWidth $Frame] [Page::CanvasHeight $Frame]
               }
            }
         }
      }
   }
   PrintBox::Destroy
}

#----------------------------------------------------------------------------
# Nom      : <METF::UpdateItems>
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
# Modifications :
#
#    Nom         : -
#    Date        : -
#    Description : -
#----------------------------------------------------------------------------

proc METF::UpdateItems { Frame } {
   variable Sim
   variable Page

   $Frame.page.canvas delete LAYOUTMETFLOC

   foreach vp "$Page(VP1) $Page(VP2) $Page(VP3) $Page(VP4)" {
      if { [set xy [$vp -project $Sim(Lat) $Sim(Lon) 0]]!="" && [lindex $xy 2]>0 } {
         Shape::DrawIcoMETF $Frame.page.canvas $xy "LAYOUTMETFLOC" black 5 False
      }
   }
}
