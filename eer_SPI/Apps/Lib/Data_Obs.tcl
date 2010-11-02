#===============================================================================
# Environnement Canada
# Centre Meteorologique Canadian
# 2100 Trans-Canadienne
# Dorval, Quebec
#
# Projet    : Librairie de definitions pour les donnees de pointages
# Fichier   : Data_Obs.tcl
# Creation  : Novembre 1999 - J.P. Gauthier - CMC/CMOE
#
# Description: Definitions d'une structure de donnees et de procedures
#              relatives aux donnees a pointer
#
# Fonctions:
#
#   Obs::InfoGraph   { Obs Tag Elem }
#   Obs::ParamGet    { { Spec "" } }
#   Obs::ParamSet    { { Spec "" } }
#   Obs::ParamPut    { }
#   Obs::ParamInit   { Spec }
#   Obs::ParamFrame  { Frame Apply }
#   Obs::ParamUpdate { { Obs {} } }
#   Obs::UpdateItems { Frame }
#   Obs::Register    { ObsId { Update True } }
#   Obs::UnRegister  { ObsId { Update True } }
#
# Remarques :
#   Aucune
#
#===============================================================================

package provide Obs 3.4

catch { SPI::Splash "Loading Data Package Obs 3.4" }

namespace eval Obs {
   global GDefs env
   variable Data
   variable Param
   variable Params
   variable Resources
   variable Lbl

   font create OBSFONTDEFAULT -family courier -weight bold -size -10

   image create photo OBSMAPImg -width 170 -height 15
   colormap create OBSMAPDEFAULT
   colormap read OBSMAPDEFAULT $env(HOME)/.spi/Colormap/REC_Col.std1.rgba
   colormap image  OBSMAPDEFAULT OBSMAPImg

   dataspec create OBSDEFAULT
   dataspec configure OBSDEFAULT -factor 1.0 -delta 0.0 -value AUTO 0 -size 10 -width 1 -font OBSFONTDEFAULT -colormap OBSDMAPEFAULT \
      -icon CIRCLE -color #000000 -unit "" -rendervector NONE -rendertexture 1 \
      -rendervolume 0 -rendercoord 0 -rendervalue 0 -renderlabel 0 -style 0 -intervalmode NONE 0

   #----- Lecture des tables BUFR
   set code [ catch {
      if { $GDefs(Lang)==0 } {
         metobs table -readmaster B $GDefs(Dir)/Data/table_b_bufr_f
         metobs table -readmaster D $GDefs(Dir)/Data/table_d_bufr_f
      } else {
         metobs table -readmaster B $GDefs(Dir)/Data/table_b_bufr_e
         metobs table -readmaster D $GDefs(Dir)/Data/table_d_bufr_e
      }
   } error ]

   if { $code } {
      Log::Print ERROR "Problems while loading BUFR tables\n\n$error"
   }

   #----- Variables des structures de donnees

   set Data(Frame)       ""                                     ;#Frame contenant les definitions de parameters

   set Data(List)           {}
   set Data(ListTool)       {}
   set Data(BubbleGraph)    True

   #----- Parametres des obs

   set Param(Orders)        { AUTO EXPONENTIAL FLOAT INTEGER }     ;#Format d'affichage des valeurs
   set Param(IntervalModes) { NONE INTERVAL LINEAR LOGARITHMIC RSMC }
   set Param(Icons)         { NONE TRIANGLE SQUARE VBAR HBAR CIRCLE LOZENGE PENTAGON HEXAGON LIGHTNING X + }

   set Param(Spec)          ""                                     ;#Variable a parametrer
   set Param(Font)          OBSFONTDEFAULT                         ;#Police
   set Param(Map)           OBSDMAPEFAULT                          ;#Palette de couleur
   set Param(MapAll)        0                                      ;#Palette de couleur applique au vecteurs
   set Param(Size)          10                                     ;#Grandeur des icones
   set Param(Style)         0                                      ;#Affichage de la trajectoire
   set Param(IntervalMode)  NONE                                   ;#Type de selection de niveaux
   set Param(IntervalParam) 0                                      ;#Nombre de niveaux
   set Param(Intervals)     ""                                     ;#Liste de niveaux
   set Param(Icon)          NONE                                   ;#Icone
   set Param(Vector)        NONE                                   ;#Rendue vectoriel
   set Param(Topo)          ""                                     ;#Var 3D
   set Param(Color)         #000000                                ;#Couleur
   set Param(Factor)        1.0                                    ;#Facteur appliques aux donnees
   set Param(Delta)         0.0                                    ;#Facteur appliques aux donnees
   set Param(Texture)       1                                      ;#Utilise la palette
   set Param(Info)          0                                      ;#Affichage de l'information
   set Param(Label)         0                                      ;#Description
   set Param(Coord)         0                                      ;#Coordonnees
   set Param(Value)         0                                      ;#Affichage des valeurs
   set Param(Volume)        0                                      ;#Affichage 3D
   set Param(Unit)          ""                                     ;#Type d'unite
   set Param(Desc)          ""                                     ;#Description
   set Param(Mantisse)      0                                      ;#Format d'affichage des valeurs
   set Param(Order)         AUTO                                   ;#Format d'affichage des valeurs
   set Param(Width)         1                                      ;#Largeur des segments

   #----- Definitions des labels

   set Lbl(Map)            { "Palette" "Colormap" }
   set Lbl(Texture)        { "Texture" "Texture" }
   set Lbl(Data)           { "Données" "Data" }
   set Lbl(Obs)            { "Observations" "Observations" }
   set Lbl(Color)          { "Couleur  " "Color" }
   set Lbl(Icon)           { "Icônes" "Icons" }
   set Lbl(Intervals)      { "Intervalles" "Intervals" }
   set Lbl(Conv)           { "Conv " "Conv " }
   set Lbl(Value)          { "Valeur " "Values " }
   set Lbl(Unit)           { "Unité  " "Units  " }
   set Lbl(Desc)           { "Desc   " "Desc   " }
   set Lbl(Size)           { "Dimension" "Size" }
   set Lbl(Vector)         { "Vecteur " "Vector " }
   set Lbl(Topo)           { "Var 3D " "Var 3D " }
   set Lbl(Volume)         { "Volume" "Volume" }
   set Lbl(Coord)          { "Coord" "Coord" }
   set Lbl(Info)           { "Info" "Info" }
   set Lbl(Format)         { "Format" "Format" }
   set Lbl(Display)        { "Affichage" "Display" }
   set Lbl(Traj)           { "Trajectoire" "Trajectory" }

   #----- Definitions des Bulles d'aides

   set Bubble(Icon)     { "Affichage des icônes"
                          "Display icons" }
   set Bubble(Color)    { "Couleur d'affichage non-texturé"
                          "Non textured color" }
   set Bubble(Font)     { "Police de caractères pour l'information"
                          "Font used to display information" }
   set Bubble(Info)     { "Sélection de l'information a afficher pour la donnée"
                          "Sélection of the information you wish to plot for the data" }
   set Bubble(Coord)    { "Affichage des coordonnées spatiales"
                          "Display spatial coordinates" }
   set Bubble(Traj)     { "Affichage de la trajectoire"
                          "Display trajectory" }
   set Bubble(Texture)  { "Utilisation de la couleur pour représenter\nl'échelle de grandeur relative des valeurs"
                          "Use colors to display relative value scale difference" }
   set Bubble(Info)     { "Affichage de l'information\npour tous les points de données"
                          "Display information about all data point" }
   set Bubble(Format)   { "Format d'affichage de la valeur"
                          "Display format of the value" }
   set Bubble(Conv)      { "Conversion appliquée aux valeurs du champs (+ Delta x Facteur)"
                           "Conversion applied to the field values (+ Delta x Factor)" }
   set Bubble(Unit)     { "Unitées de la variable specifiée"
                          "Specified variable units" }
   set Bubble(Desc)     { "Description de la donnnée"
                          "Data description" }
   set Bubble(Size)     { "Dimension relatives des icônes en pixels"
                          "Relative pixel size of the icons" }
   set Bubble(Volume)   { "Afficher en 3D"
                          "Display in 3D" }
   set Bubble(Vector)   { "Type d'affichage vectoriel"
                          "Vectorial rendering type" }
   set Bubble(Map)      { "Palette utilisée pour les valeurs"
                          "Colormap used for values" }
   set Bubble(Intervals) { "Liste des intervals (1 2 3 ... ou [0 1])"
                          "Intervals description (1 2 3 ... ou [0 1])" }
}

#-------------------------------------------------------------------------------
# Nom      : <Obs::InfoGraph>
# Creation : Mai 2008 - J.P. Gauthier - CMC/CMOE
#
# But      : Creer un item de graph specifique pour les obs a inserer dans la bulle
#            informative de la page
#
# Parametres :
#   <Obs>    : Observation (metobs)
#   <TagOrId>: Identificateur ou tag de la localisation
#   <Elem>   : Element
#
# Retour     :
#   <graphitem> : Item de graph
#
# Remarque :
#
#-------------------------------------------------------------------------------

proc Obs::InfoGraph { Obs Tag Elem } {
   variable Data

   if { !$Data(BubbleGraph) } {
      return
   }

   #----- Create the needed objects if they are not created yet
   if { ![vector is PAGEOBSGRAPHDATA] } {
      graphaxis create PAGEOBSGRAPHAXISX
      graphaxis create PAGEOBSGRAPHAXISY
      graphitem create PAGEOBSGRAPHITEM

      vector create PAGEOBSGRAPHDATA
      vector dim    PAGEOBSGRAPHDATA { X Y }
      vector stat   PAGEOBSGRAPHDATA.X -nodata -999.0
      vector stat   PAGEOBSGRAPHDATA.Y -nodata -999.0

      graphaxis configure PAGEOBSGRAPHAXISX -font XFont10 -color black -position LL -width 1 -incr 3600
      graphaxis configure PAGEOBSGRAPHAXISY -font XFont10 -color black -gridcolor gray50 -position LL -width 1 -highoffset 2
      graphitem configure PAGEOBSGRAPHITEM -xaxis PAGEOBSGRAPHAXISX -yaxis PAGEOBSGRAPHAXISY -width 1 -tag PAGEOBSGRAPHITEM
   }

   #----- If we have enough elements to create a graph
   vector clear PAGEOBSGRAPHDATA
   set items [metobs define $Obs -ELEMENT $Tag $Elem]

   #----- Is it a profile ?
   if { [llength [lindex [lindex $items 0] 1]]>1 } {
      if { [set elem [metmodel define [metobs define $Obs -MODEL] -topography]]!="" } {
         set elevs [metobs define $Obs -ELEMENT $Tag $elem]
         foreach item [lindex [lindex $items 0] 1] elev [lindex [lindex $elevs 0] 1] {
            vector append PAGEOBSGRAPHDATA.X $item
            vector append PAGEOBSGRAPHDATA.Y $elev
         }
      } else {
         set i 0
         foreach item [lindex [lindex $items 0] 1] {
            vector append PAGEOBSGRAPHDATA.X $item
            vector append PAGEOBSGRAPHDATA.Y [incr i]
         }
      }
      vector sort PAGEOBSGRAPHDATA Y

      graphaxis configure PAGEOBSGRAPHAXISX -min [vector stats PAGEOBSGRAPHDATA.X -min] -max [vector stats PAGEOBSGRAPHDATA.X -max] \
         -intervals "[vector stats PAGEOBSGRAPHDATA.X -min] [vector stats PAGEOBSGRAPHDATA.X -max]" -format NONE
      graphaxis configure PAGEOBSGRAPHAXISY -min [vector get PAGEOBSGRAPHDATA.Y 0] -max [vector get PAGEOBSGRAPHDATA.Y end] \
         -intervals "[vector stats PAGEOBSGRAPHDATA.Y -min] [vector stats PAGEOBSGRAPHDATA.Y -max]" -format INTEGER
      graphitem configure PAGEOBSGRAPHITEM -xdata PAGEOBSGRAPHDATA.X -ydata PAGEOBSGRAPHDATA.Y \
         -desc [string range [lindex [metobs table -desc $Elem] 0] 0 20] -type LINE -orient Y -outline black -fill yellow

   #----- Then it is a time serie
   } else {
      foreach item $items {
         vector append PAGEOBSGRAPHDATA $item
      }

      graphaxis configure PAGEOBSGRAPHAXISX -min [vector stats PAGEOBSGRAPHDATA.X -min] -max [vector stats PAGEOBSGRAPHDATA.X -max] \
         -intervals "[vector stats PAGEOBSGRAPHDATA.X -min] [vector stats PAGEOBSGRAPHDATA.X -max]" -format T-HH
      graphaxis configure PAGEOBSGRAPHAXISY -min [vector stats PAGEOBSGRAPHDATA.Y -min] -max [vector stats PAGEOBSGRAPHDATA.Y -max] \
         -intervals "[vector stats PAGEOBSGRAPHDATA.Y -min] [vector stats PAGEOBSGRAPHDATA.Y -max]"
      graphitem configure PAGEOBSGRAPHITEM -xdata PAGEOBSGRAPHDATA.X -ydata PAGEOBSGRAPHDATA.Y \
         -desc [string range [lindex [metobs table -desc $Elem] 0] 0 20] -type LINE -orient X -outline black -fill yellow
   }

   if { [vector length PAGEOBSGRAPHDATA.X]>2 } {
      return PAGEOBSGRAPHITEM
   } else {
      return ""
   }
}

#-------------------------------------------------------------------------------
# Nom      : <Obs::ParamFrame>
# Creation : Mars 2000 - J.P. Gauthier - CMC/CMOE
#
# But      : Creer l'interface de selection des parametres de pointage.
#
# Parametres :
#  <Frame>   : Identificateur du frame
#  <Apply>   : Commande d'update de l'etat
#
# Retour     :
#
# Remarque :
#
#-------------------------------------------------------------------------------

proc Obs::ParamFrame { Frame Apply } {
   global GDefs
   variable Lbl
   variable Bubble
   variable Data
   variable Param
   variable Resources

   set Data(ApplyButton) $Apply
   set Data(Frame) [TabFrame::Add $Frame 2 [lindex $Lbl(Obs) $GDefs(Lang)] False ""]

   labelframe $Data(Frame).var -text [lindex $Lbl(Obs) $GDefs(Lang)]
      ComboBox::Create $Data(Frame).var.sel Obs::Param(Spec) noedit sorted nodouble -1 \
          "" 18 3 { Obs::ParamGet; Obs::ParamPut }
      label $Data(Frame).var.lbl -text "VAR "
      pack $Data(Frame).var.lbl -side left -fill x -padx 2 -pady 2
      pack $Data(Frame).var.sel -side left -fill x -expand True -padx 2 -pady 2

   frame $Data(Frame).def
      frame $Data(Frame).def.l

        labelframe $Data(Frame).def.l.val -text [lindex $Lbl(Data) $GDefs(Lang)]
            frame $Data(Frame).def.l.val.order
               label $Data(Frame).def.l.val.order.lbl -text [lindex $Lbl(Value) $GDefs(Lang)]
               ComboBox::Create $Data(Frame).def.l.val.order.sel Obs::Param(Order) noedit unsorted nodouble -1 \
                  $Obs::Param(Orders) 7 4 { Obs::ParamSet }
               spinbox $Data(Frame).def.l.val.order.prec -textvariable Obs::Param(Mantisse) -width 2 -from 0 -to 30 -wrap 1 -bd 1 \
                  -command { Obs::ParamSet } -bg $GDefs(ColorLight)
               button $Data(Frame).def.l.val.order.font -relief groove -bd 2 -bitmap @$GDefs(Dir)/Resources/Bitmap/font.ico\
                  -command "FontBox::Create $Data(Frame).def.l.val.order.font Obs::ParamSet \$Obs::Param(Font)"
               pack $Data(Frame).def.l.val.order.lbl -side left
               pack $Data(Frame).def.l.val.order.font $Data(Frame).def.l.val.order.prec -side left -fill x -expand true
               pack $Data(Frame).def.l.val.order.sel -side left -fill x -expand true
            frame $Data(Frame).def.l.val.fac
               label $Data(Frame).def.l.val.fac.lbl -text [lindex $Lbl(Conv) $GDefs(Lang)]
               label $Data(Frame).def.l.val.fac.lbld -text "+"
               entry $Data(Frame).def.l.val.fac.seld -textvariable Obs::Param(Delta) -bd 1 -width 7 -bg $GDefs(ColorLight)
               label $Data(Frame).def.l.val.fac.lblf -text "x"
               entry $Data(Frame).def.l.val.fac.self -textvariable Obs::Param(Factor) -bd 1 -width 7 -bg $GDefs(ColorLight)
               pack $Data(Frame).def.l.val.fac.lbl $Data(Frame).def.l.val.fac.lbld $Data(Frame).def.l.val.fac.seld $Data(Frame).def.l.val.fac.lblf -side left
               pack $Data(Frame).def.l.val.fac.self -side left -fill x -expand true
               bind $Data(Frame).def.l.val.fac.seld <Any-KeyRelease> { Obs::ParamSet }
               bind $Data(Frame).def.l.val.fac.self <Any-KeyRelease> { Obs::ParamSet }
                        frame $Data(Frame).def.l.val.unit
               label $Data(Frame).def.l.val.unit.lbl -text [lindex $Lbl(Unit) $GDefs(Lang)]
               entry $Data(Frame).def.l.val.unit.sel -textvariable Obs::Param(Unit) -bd 1 -width 12 -bg $GDefs(ColorLight)
               pack $Data(Frame).def.l.val.unit.lbl -side left
               pack $Data(Frame).def.l.val.unit.sel -side left -fill x -expand true
               bind $Data(Frame).def.l.val.unit.sel <Any-KeyRelease> { Obs::ParamSet }
            frame $Data(Frame).def.l.val.desc
               label $Data(Frame).def.l.val.desc.lbl -text [lindex $Lbl(Desc) $GDefs(Lang)]
               entry $Data(Frame).def.l.val.desc.sel -textvariable Obs::Param(Desc) -bd 1 -width 12 -bg $GDefs(ColorLight)
               pack $Data(Frame).def.l.val.desc.lbl -side left
               pack $Data(Frame).def.l.val.desc.sel -side left -fill x -expand true
               bind $Data(Frame).def.l.val.desc.sel <Any-KeyRelease> { Obs::ParamSet }
            pack $Data(Frame).def.l.val.order $Data(Frame).def.l.val.fac $Data(Frame).def.l.val.unit $Data(Frame).def.l.val.desc \
               -side top -padx 2 -anchor n -fill x

         labelframe $Data(Frame).def.l.pal -text [lindex $Lbl(Map) $GDefs(Lang)]
            button $Data(Frame).def.l.pal.cmap -bd 1 -relief flat -image OBSMAPImg \
               -command { MapBox::Create $Obs::Data(Frame).def.l.pal.cmap \
                   "colormap image \"$Obs::Param(Map)\" OBSMAPImg; Page::Update $Page::Data(Frame)" $Obs::Param(Map) }
            pack $Data(Frame).def.l.pal.cmap -side top -fill x -padx 2 -pady 2

         labelframe $Data(Frame).def.l.size -text [lindex $Lbl(Size) $GDefs(Lang)]
            scale $Data(Frame).def.l.size.si -from 0 -to 20 -resolution 1 -width 14 -sliderlength 8 -variable Obs::Param(Size) \
               -relief flat -bd 1 -orient horizontal -command { Obs::ParamSet; catch }
            pack $Data(Frame).def.l.size.si -side left -fill x -padx 2 -expand true

         pack $Data(Frame).def.l.val $Data(Frame).def.l.pal $Data(Frame).def.l.size -side top -fill x

      frame $Data(Frame).def.r

         labelframe $Data(Frame).def.r.disp -text [lindex $Lbl(Display) $GDefs(Lang)]

            frame $Data(Frame).def.r.disp.p
               IcoMenu::CreateDef $Data(Frame).def.r.disp.p.ico $GDefs(Dir)/Resources/Bitmap \
                 { zeroth.xbm stri.xbm ssquare.xbm svbar.xbm shbar.xbm scircle.xbm slos.xbm spenta.xbm shexa.xbm slight.xbm sx.xbm s+.xbm } \
                 $Param(Icons) Obs::Param(Icon) Obs::ParamSet 0 -relief groove -bd 2
               ColorBox::CreateSel $Data(Frame).def.r.disp.p.col Obs::Param(Color) Obs::ParamSet
               IcoMenu::Create $Data(Frame).def.r.disp.p.width $GDefs(Dir)/Resources/Bitmap \
                  "zeroth.xbm width1.xbm width2.xbm width3.xbm width4.xbm width5.xbm" "0 1 2 3 4 5" \
                  Obs::Param(Width) "Obs::ParamSet" 0 -relief groove -bd 2
               checkbutton $Data(Frame).def.r.disp.p.map -image COLORMAP -variable Obs::Param(MapAll) -onvalue 1 -offvalue 0 \
                  -relief sunken -bd 2 -overrelief raised -offrelief groove -command { Obs::ParamSet } -indicatoron false
               pack $Data(Frame).def.r.disp.p.map $Data(Frame).def.r.disp.p.col $Data(Frame).def.r.disp.p.width -side left
               pack $Data(Frame).def.r.disp.p.ico -side left -fill x -expand true

            frame $Data(Frame).def.r.disp.traj
               IcoMenu::Create $Data(Frame).def.r.disp.traj.sel $GDefs(Dir)/Resources/Bitmap \
                  "zeroth.xbm tstyle0.xbm tstyle1.xbm tstyle2.xbm tstyle3.xbm tstyle4.xbm" "0 1 2 3 4 5" \
                  Obs::Param(Style) Obs::ParamSet $Obs::Param(Style) -relief groove -bd 2
               label $Data(Frame).def.r.disp.traj.lbl -text " [lindex $Lbl(Traj) $GDefs(Lang)]" -anchor w
            pack  $Data(Frame).def.r.disp.traj.sel $Data(Frame).def.r.disp.traj.lbl -side left -fill x

            frame $Data(Frame).def.r.disp.vect
               label $Data(Frame).def.r.disp.vect.lbl -text " [lindex $Lbl(Vector) $GDefs(Lang)]"
               IcoMenu::Create $Data(Frame).def.r.disp.vect.sel $GDefs(Dir)/Resources/Bitmap \
                  "zeroth.xbm wind1.xbm wind2.xbm" "NONE BARBULE ARROW" Obs::Param(Vector) \
                  Obs::ParamSet 0 -relief groove -bd 2
               pack $Data(Frame).def.r.disp.vect.sel $Data(Frame).def.r.disp.vect.lbl -side left

            frame $Data(Frame).def.r.disp.tex
               label $Data(Frame).def.r.disp.tex.lbl -text " [lindex $Lbl(Texture) $GDefs(Lang)]"
               checkbutton $Data(Frame).def.r.disp.tex.sel -variable Obs::Param(Texture) -relief raised -bd 1 \
                  -bitmap @$GDefs(Dir)/Resources/Bitmap/zeroth.xbm -indicatoron false \
                  -command Obs::ParamSet -selectcolor "" -relief groove -bd 1
               pack $Data(Frame).def.r.disp.tex.sel -side left
               pack $Data(Frame).def.r.disp.tex.lbl -side left

            frame $Data(Frame).def.r.disp.vol
               label $Data(Frame).def.r.disp.vol.lbl -text " [lindex $Lbl(Volume) $GDefs(Lang)]"
               checkbutton $Data(Frame).def.r.disp.vol.sel -variable Obs::Param(Volume) -relief raised -bd 1 \
                  -bitmap @$GDefs(Dir)/Resources/Bitmap/zeroth.xbm -indicatoron false -onvalue 1 -offvalue 0\
                  -command { Obs::ParamSet } -selectcolor "" -relief groove -bd 1
               pack $Data(Frame).def.r.disp.vol.sel -side left
               pack $Data(Frame).def.r.disp.vol.lbl -side left

            frame $Data(Frame).def.r.disp.info
               label $Data(Frame).def.r.disp.info.lbl -text " [lindex $Lbl(Info) $GDefs(Lang)]"
               checkbutton $Data(Frame).def.r.disp.info.sel -variable Obs::Param(Label) -relief raised -bd 1 \
                  -bitmap @$GDefs(Dir)/Resources/Bitmap/zeroth.xbm -indicatoron false \
                  -command { Obs::ParamSet } -selectcolor "" -relief groove -bd 1
               pack $Data(Frame).def.r.disp.info.sel -side left
               pack $Data(Frame).def.r.disp.info.lbl -side left

            frame $Data(Frame).def.r.disp.coord
               label $Data(Frame).def.r.disp.coord.lbl -text " [lindex $Lbl(Coord) $GDefs(Lang)]"
               checkbutton $Data(Frame).def.r.disp.coord.sel -variable Obs::Param(Coord) -relief raised -bd 1 \
                  -bitmap @$GDefs(Dir)/Resources/Bitmap/zeroth.xbm -indicatoron false \
                  -command { Obs::ParamSet } -selectcolor "" -relief groove -bd 1
               pack $Data(Frame).def.r.disp.coord.sel -side left
               pack $Data(Frame).def.r.disp.coord.lbl -side left

            frame $Data(Frame).def.r.disp.value
               label $Data(Frame).def.r.disp.value.lbl -text " [lindex $Lbl(Value) $GDefs(Lang)]"
               checkbutton $Data(Frame).def.r.disp.value.sel -variable Obs::Param(Value) -relief raised -bd 1 \
                  -bitmap @$GDefs(Dir)/Resources/Bitmap/zeroth.xbm -indicatoron false \
                  -command { Obs::ParamSet } -selectcolor "" -relief groove -bd 1
               pack $Data(Frame).def.r.disp.value.sel -side left
               pack $Data(Frame).def.r.disp.value.lbl -side left

            pack  $Data(Frame).def.r.disp.p $Data(Frame).def.r.disp.tex $Data(Frame).def.r.disp.vol \
               $Data(Frame).def.r.disp.value $Data(Frame).def.r.disp.info $Data(Frame).def.r.disp.coord \
               $Data(Frame).def.r.disp.vect $Data(Frame).def.r.disp.traj -side top -padx 5 -anchor w
         pack $Data(Frame).def.r.disp -side top -fill x
      pack $Data(Frame).def.l $Data(Frame).def.r -side left -padx 5 -pady 5 -fill x -anchor n

   labelframe $Data(Frame).lev -text [lindex $Lbl(Intervals) $GDefs(Lang)]
      frame $Data(Frame).lev.select
         menubutton $Data(Frame).lev.select.mode -textvariable Obs::Param(IntervalMode) -bd 1 \
            -menu $Data(Frame).lev.select.mode.list -relief raised -relief ridge -width 11
         pack $Data(Frame).lev.select.mode -side left
      pack $Data(Frame).lev.select -side top -fill x -padx 2

      frame $Data(Frame).lev.desc
         ComboBox::Create $Data(Frame).lev.desc.edit Obs::Param(Intervals) editclose sorted nodouble -1 \
            "" 17 6 "set Obs::Param(IntervalMode) NONE; Obs::ParamSet"
         pack $Data(Frame).lev.desc.edit -side left -fill both -expand true
      pack $Data(Frame).lev.desc -side top -fill x -padx 2 -pady 2 -expand true

   pack $Data(Frame).var -side top -fill x -anchor n -padx 5 -pady 5
   pack $Data(Frame).def -side top -fill x -anchor n
   pack $Data(Frame).lev -side top -fill x -anchor n -padx 5

   bind $Data(Frame).lev.desc.edit.select <KeyRelease> {+ set Obs::Param(IntervalMode) NONE ; Obs::ParamSet }

   #----- Creation du menu de mode de niveaux

   menu $Data(Frame).lev.select.mode.list
      $Data(Frame).lev.select.mode.list add cascade -label [lindex $Param(IntervalModes) 1] \
         -menu $Data(Frame).lev.select.mode.list.inter
      $Data(Frame).lev.select.mode.list add separator
      $Data(Frame).lev.select.mode.list add cascade -label [lindex $Param(IntervalModes) 2] \
         -menu $Data(Frame).lev.select.mode.list.nb
      $Data(Frame).lev.select.mode.list add command -label [lindex $Param(IntervalModes) 3] \
         -command "set Obs::Param(Intervals) \"\" ; set Obs::Param(IntervalMode) [lindex $Param(IntervalModes) 3] ; Obs::ParamSet"
      $Data(Frame).lev.select.mode.list add command -label [lindex $Param(IntervalModes) 4] \
         -command "set Obs::Param(Intervals) \"\" ; set Obs::Param(IntervalMode) [lindex $Param(IntervalModes) 4] ; Obs::ParamSet"
      $Data(Frame).lev.select.mode.list add separator
      $Data(Frame).lev.select.mode.list add command -label [lindex $Param(IntervalModes) 0] \
         -command "set Obs::Param(Intervals) \"\" ; set Obs::Param(IntervalMode) [lindex $Param(IntervalModes) 0] ; Obs::ParamSet"

   menu $Data(Frame).lev.select.mode.list.inter
   menu $Data(Frame).lev.select.mode.list.nb
      for { set i 1 } { $i < 25 } { incr i } {
         $Data(Frame).lev.select.mode.list.nb add command -label "$i" \
            -command "set Obs::Param(Intervals) \"\"; set Obs::Param(IntervalMode) [lindex $Param(IntervalModes) 2]; set Obs::Param(IntervalParam) $i; Obs::ParamSet"
      }

   Bubble::Create $Data(Frame).def.l.val.order.font $Bubble(Font)
   Bubble::Create $Data(Frame).def.l.val.order      $Bubble(Format)
   Bubble::Create $Data(Frame).def.l.val.unit       $Bubble(Unit)
   Bubble::Create $Data(Frame).def.l.val.desc       $Bubble(Desc)
   Bubble::Create $Data(Frame).def.l.val.fac        $Bubble(Conv)
   Bubble::Create $Data(Frame).def.l.pal.cmap       $Bubble(Map)
   Bubble::Create $Data(Frame).def.r.disp.p.ico     $Bubble(Icon)
   Bubble::Create $Data(Frame).def.r.disp.p.col     $Bubble(Color)
   Bubble::Create $Data(Frame).def.r.disp.tex.sel   $Bubble(Texture)
   Bubble::Create $Data(Frame).def.r.disp.vol.sel   $Bubble(Volume)
   Bubble::Create $Data(Frame).def.r.disp.info.sel  $Bubble(Info)
   Bubble::Create $Data(Frame).def.r.disp.coord.sel $Bubble(Coord)
   Bubble::Create $Data(Frame).def.r.disp.traj.sel  $Bubble(Traj)
   Bubble::Create $Data(Frame).def.r.size.si        $Bubble(Size)
   Bubble::Create $Data(Frame).lev.desc.edit        $Bubble(Intervals)
}

#-------------------------------------------------------------------------------
# Nom      : <Obs::ParamGet>
# Creation : Mai 2006 - J.P. Gauthier - CMC/CMOE
#
# But      : Recuperer les parametres d'une observation.
#
# Parametres :
#   <Spec>   : Specification to configure
#
# Retour     :
#
# Remarque :
#
#-------------------------------------------------------------------------------

proc Obs::ParamGet { { Spec "" } } {
   variable Data
   variable Param
   variable Resources

   if { $Spec=="" } {
      set Spec $Param(Spec)
   }

   if { ![dataspec is $Spec] } {
      return
   }

   set val  [dataspec configure $Spec -value]
   set Param(Order)     [lindex $val 0]
   set Param(Mantisse)  [lindex $val 1]

   set mode [dataspec configure $Spec -intervalmode]
   set Param(IntervalMode)  [lindex $mode 0]
   set Param(IntervalParam) [lindex $mode 1]

   set Param(Font)      [dataspec configure $Spec -font]
   set Param(Factor)    [dataspec configure $Spec -factor]
   set Param(Delta)     [dataspec configure $Spec -delta]
   set Param(Unit)      [dataspec configure $Spec -unit]
   set Param(Desc)      [dataspec configure $Spec -desc]
   set Param(Map)       [dataspec configure $Spec -colormap]
   set Param(MapAll)    [dataspec configure $Spec -mapall]
   set Param(Size)      [dataspec configure $Spec -size]
   set Param(Icon)      [dataspec configure $Spec -icon]
   set Param(Style)     [dataspec configure $Spec -style]
   set Param(Color)     [dataspec configure $Spec -color]
   set Param(Vector)    [dataspec configure $Spec -rendervector]
   set Param(Topo)      [dataspec configure $Spec -topography]
   set Param(Texture)   [dataspec configure $Spec -rendertexture]
   set Param(Volume)    [dataspec configure $Spec -rendervolume]
   set Param(Coord)     [dataspec configure $Spec -rendercoord]
   set Param(Value)     [dataspec configure $Spec -rendervalue]
   set Param(Label)     [dataspec configure $Spec -renderlabel]
   set Param(Intervals) [dataspec configure $Spec -intervals]
   set Param(Min)       [dataspec configure $Spec -min]
   set Param(Max)       [dataspec configure $Spec -max]
   set Param(Width)     [dataspec configure $Spec -width]

   if { $Param(Min)!=$Param(Max) } {
      set Param(Intervals) ""
      if { $Param(Min)!="" } {
         append Param(Intervals) "\[$Param(Min)"
      }
      if { $Param(Max)!="" } {
         append Param(Intervals) " $Param(Max)\]"
      }
   }
}

#-------------------------------------------------------------------------------
# Nom      : <Obs::ParamSet>
# Creation : Mai 2006 - J.P. Gauthier - CMC/CMOE
#
# But      : Memoriser les parametres d'une observation.
#
# Parametres :
#   <Spec>   : Specification to configure
#
# Retour     :
#
# Remarque :
#
#-------------------------------------------------------------------------------

proc Obs::ParamSet { { Spec "" } } {
   variable Data
   variable Param
   variable Resources

   if { $Spec=="" } {
      set Spec $Param(Spec)
   }

   if { ![dataspec is $Spec] } {
      return
   }

   set inter $Param(Intervals)
   set min   ""
   set max   ""
   set var   [dataspec configure $Spec -desc]

   #----- Verifier pour un range plutot que des niveaux

   if { [set from [string first "\[" $Param(Intervals)]]!=-1 } {
      set min [lindex [string range $Param(Intervals) [incr from] end] 0]
      set inter {}
   }

   if { [set to [string first "\]" $Param(Intervals)]]!=-1 } {
      set max [lindex [string range $Param(Intervals) 0 [incr to -1]] end]
      set inter {}
   }

   if { $Param(IntervalMode)!="NONE" } {
      dataspec configure $Spec -min $min -max $max -intervalmode $Param(IntervalMode) $Param(IntervalParam)
   } else {
      dataspec configure $Spec -min $min -max $max -intervals $inter -intervalmode $Param(IntervalMode) $Param(IntervalParam)
   }

   dataspec configure $Spec -factor $Param(Factor) -delta $Param(Delta) -value $Param(Order) $Param(Mantisse) -size $Param(Size) -width $Param(Width) -font $Param(Font) -colormap $Param(Map) \
      -style $Param(Style) -icon $Param(Icon) -color $Param(Color) -unit $Param(Unit) -desc $Param(Desc) -rendervector $Param(Vector) -rendertexture $Param(Texture) \
      -rendervolume $Param(Volume) -rendercoord $Param(Coord) -rendervalue $Param(Value) -renderlabel $Param(Label) -mapall $Param(MapAll) -topography $Param(Topo)

   catch { $Data(ApplyButton) configure -state normal }
}

#-------------------------------------------------------------------------------
# Nom      : <Obs::ParamPut>
# Creation : Mai 2006 - J.P. Gauthier - CMC/CMOE
#
# But      : Inserer les parametres dans l'interface.
#
# Parametres :
#
# Retour     :
#
# Remarque :
#
#-------------------------------------------------------------------------------

proc Obs::ParamPut { } {
   variable Data
   variable Param

   IcoMenu::Set $Data(Frame).def.r.disp.vect.sel  $Param(Vector)
   IcoMenu::Set $Data(Frame).def.r.disp.p.ico     $Param(Icon)
   IcoMenu::Set $Data(Frame).def.r.disp.p.width   $Param(Width)

   ColorBox::ConfigNoColor $Data(Frame).def.r.disp.p.col $Param(Color)

   if { [colormap is $Param(Map)] } {
      colormap image $Param(Map) OBSMAPImg
   } else {
      OBSMAPImg blank
   }

   set inters $Param(Intervals)
   ComboBox::DelAll $Data(Frame).lev.desc.edit
   $Data(Frame).lev.select.mode.list.inter delete 0 end

   if { [lsearch -exact $MetStat::Rec(Var) $Param(Spec)]!=-1 } {
      ComboBox::AddList $Data(Frame).lev.desc.edit $MetStat::Rec(Level$Param(Spec))

      foreach inter $MetStat::Rec(Inter$Param(Spec)) {
         $Data(Frame).lev.select.mode.list.inter add command -label "$inter" \
            -command "set Obs::Param(IntervalParam) $inter; set Obs::Param(Intervals) \"\" ; set Obs::Param(IntervalMode) INTERVAL ;Obs::ParamSet"
      }
   }

   set Param(Intervals) $inters
}

#-------------------------------------------------------------------------------
# Nom      : <Obs::ParamInit>
# Creation : Mai 2006 - J.P. Gauthier - CMC/CMOE
#
# But      : Initialiser les nouvelles configurations.
#
# Parametres :
#   <Spec>   : Specification to configure
#
# Retour     :
#
# Remarque :
#
#-------------------------------------------------------------------------------

proc Obs::ParamInit { Obs { Spec "" } } {
   variable Param

   if { [dataspec is $Spec] } {
      set set  [dataspec configure $Spec -set]
      set var  [dataspec configure $Spec -desc]
      set unit [dataspec configure $Spec -unit]

      if { !$set } {
         dataspec copy $Spec OBSDEFAULT
         dataspec configure $Spec -desc $var -unit $unit
      }

      #----- Set a colormap if not done

      set map [dataspec configure $Spec -colormap]
      if { $map=="" } {
         set map OBSMAP$Spec
      } else {
        colormap copy OBSMAP$Spec $map
      }

      if { ![colormap is OBSMAP$Spec] } {
         colormap create OBSMAP$Spec
         colormap copy   OBSMAP$Spec OBSMAPDEFAULT
         dataspec configure $Spec -colormap OBSMAP$Spec
      }

      if { [lsearch -exact $MetStat::Rec(Var) $var]!=-1 } {
         dataspec configure $Spec -unit $MetStat::Rec(Unit$var)
      }
   }
}

#----------------------------------------------------------------------------
# Nom      : <Obs::ParamUpdate>
# Creation : Mai 2006 - J.P. Gauthier - CMC/CMOE
#
# But      : Mettre a jour la liste des observations selectionnees
#
# Parametres :
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc Obs::ParamUpdate { { Obs {} } } {
   variable Param
   variable Data

   if { ![llength $Obs] } {
      set Obs [concat $Data(List) $Data(ListTool)]
   }

   set current $Param(Spec)
   set exist 0
   set spec  ""

   #----- Inserer les items dans la liste de configuration

   ComboBox::DelAll $Data(Frame).var.sel

   foreach obs $Obs {
      if { [metobs is $obs] } {
         set model [metobs define $obs -MODEL]
         foreach item [metmodel define $model -items] {
            if { [lindex $item 2]!="" } {
               set spec [metmodel configure $model [lindex $item 2] -dataspec]
               if { "$spec"=="$current" } {
                  set Param(Spec) $current
                  set exist 1
               }
               ComboBox::Add $Data(Frame).var.sel $spec
               Obs::ParamInit $obs $spec
            }
         }
      }

      if { [observation is $obs] } {
         set spec [observation configure $obs -dataspec]
         if { "$spec"=="$current" } {
             set Param(Spec) $current
             set exist 1
         }
         ComboBox::Add $Data(Frame).var.sel $spec
         Obs::ParamInit $obs $spec
      }
   }

   if { !$exist && $spec!="" } {
      set Param(Spec) $spec
      Obs::ParamGet
      Obs::ParamPut
   }
}

#----------------------------------------------------------------------------
# Nom      : <Obs::Register>
# Creation : Janvier 2006 - J.P. Gauthier - CMC/CMOE
#
# But      : Enregistrer l'obs dans la liste des observations connus et configurable
#
# Parametres :
#   <ObsId>  : Identificateur de champs
#   <Update> : Mise a jour des parametres
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc Obs::Register { ObsId { Update True } } {
   variable Data

   if { [set idx [lsearch -exact $Data(ListTool) $ObsId]]==-1 } {
      lappend Data(ListTool) $ObsId

      if { $Update } {
         Obs::ParamUpdate
      }
   }

   return $idx
}

#----------------------------------------------------------------------------
# Nom      : <Obs::UnRegister>
# Creation : Janvier 2006 - J.P. Gauthier - CMC/CMOE
#
# But      : Supprimer l'obs de la liste des observations connus et configurable
#
# Parametres :
#   <ObsId>  : Identificateur de champs
#   <Update> : Mise a jour des parametres
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc Obs::UnRegister { ObsId { Update True } } {
   variable Data

   if { [set idx [lsearch -exact $Data(ListTool) $ObsId]]!=-1 } {
      set Data(ListTool) [lreplace $Data(ListTool) $idx $idx]

      if { $Update } {
         Obs::ParamUpdate
      }
   }

   return $idx
}
