#===============================================================================
# Environnement Canada
# Centre Meteorologique Canadian
# 2100 Trans-Canadienne
# Dorval, Quebec
#
# Projet   : Librairie de fonctions pour fichier standards.
# Fichier  : Data_FSTD.tcl
# Creation : Mai 2000 - J.P. Gauthier - CMC/CMOE
#
# Description: Ce package s'occupe des taches relatives a la configuration des
#              champs de fichiers standards en relations avec les projections.
#              Il est donc fortement lie au package Viewport.
#
# Fonctions:
#
#   FSTD::Data            { Field }
#   VectorBox::Create     { Parent Apply args }
#   FSTD::ParamFrame      { Frame Apply }
#   FSTD::FieldFormat     { Field Val }
#   FSTD::Follower        { Page Canvas VP Lat Lon X Y }
#   FSTD::IntervalSet     { { Select 0 } }
#   FSTD::IntervalSetMode { Mode { Par 0 } }
#   FSTD::ParamGet        { { Spec "" } }
#   FSTD::ParamSet        { { Spec "" } }
#   FSTD::ParamPut        { }
#   FSTD::ParamInit       { Field { Spec "" } }
#   FSTD::Register        { FieldId { Update True } }
#   FSTD::UnRegister      { FieldId { Update True } }
#   FSTD::Params          { Id Map args }
#   FSTD::ParamUpdate     { { Fields { } } }
#   FSTD::VarMode         { Mode }
#
#===============================================================================

package provide FSTD 3.5

catch { SPI::Splash "Loading Data Package FSTD 3.5" }

package require Bubble
package require MapBox
package require VectorBox
package require MetStat

namespace eval FSTD {
   global   env
   variable Data
   variable Param
   variable Params
   variable Lbl
   variable Error

   #----- Creer les parametres de la palette par defaut

   font create FLDFONTDEFAULT -family courier -size -12 -weight bold

   image create photo FLDMAPImg -width 185 -height 15
   colormap create FLDMAPDEFAULT
   colormap read FLDMAPDEFAULT $env(HOME)/.spi/Colormap/REC_Col.std1.rgba
   colormap image  FLDMAPDEFAULT FLDMAPImg

   dataspec create FLDDEFAULT
   dataspec configure FLDDEFAULT -factor 1.0 -delta 0.0 -value AUTO 0 -size 10 -width 1 -font FLDFONTDEFAULT -colormap FLDDMAPEFAULT \
      -color #000000 -unit "" -dash "" -rendercontour 0 -rendervector NONE -rendertexture 1 -renderparticle 0 -rendergrid 0 \
      -rendervolume 0 -rendercoord 0 -rendervalue 0 -renderlabel 0 -intervalmode NONE 0 -interpdegree LINEAR  -sample 2 \
      -intervals {}

   fstdfield vector { UU VV }
   fstdfield vector { UP VP }
   fstdfield vector { U V W }

   Viewport::FollowerAdd FSTD

   #----- Variable internes

   set Data(Frame)          ""          ;#Frame contenant les definitions de parameters
   set Data(Apply)          0           ;#Indicateur de changement des parametres
   set Data(Command)        ""          ;#Commande de reaffichage

   set Data(List)          {}          ;#Liste des champs ouvert
   set Data(ListTool)      {}          ;#Liste des champs ouvert par d'autres outils

   set Param(Spec)          ""                                     ;#Variable a parametrer

   #----- Parametres du champs

   set Param(Interpolator)   { NEAREST LINEAR }
   set Param(Extrapolator)   { NEUTRAL MAXIMUM MINIMUM }
   set Param(Orders)         { AUTO INTEGER FLOAT EXPONENTIAL }
   set Param(IntervalModes)  { NONE INTERVAL LINEAR LOGARITHMIC RSMC }

   set Param(Mode)          VAR            ;#Mode de selection des parametres
   set Param(Map)           FLDMAPDEFAULT  ;#Palette de couleur
   set Param(Font)          FLDFONTDEFAULT ;#Police
   set Param(Interp)        LINEAR         ;#Type d'interpolation
   set Param(Extrap)        NEUTRAL        ;#Type d'extrapolation
   set Param(Contour)       0              ;#Affichage des contours
   set Param(MapAll)        0              ;#Affichage des contours avec la colormap
   set Param(Grid)          0              ;#Affichage de la grille
   set Param(Vector)        NONE           ;#Affichage vectorise
   set Param(Texture)       1              ;#Affichage des donnees
   set Param(Volume)        0              ;#Affichage des donnees volumetriques
   set Param(Particle)      0              ;#Affichage de particules
   set Param(Label)         0              ;#Affichage des labels
   set Param(Value)         0              ;#Affichage des valeurs (centrales,min,max)
   set Param(Order)         AUTO           ;#Type d'affichage de valeur
   set Param(Mantisse)      0              ;#Dimension de la mantisse
   set Param(Color)         #000000        ;#Couleur des items vectoriels
   set Param(Dash)          ""             ;#Pattern des items vectoriels
   set Param(Topo)          ""             ;#Modulation 3D
   set Param(TopoFac)       1.0            ;#Facteur de modulation 3D
   set Param(Sample)        4              ;#Sampling des points vectoriels
   set Param(Step)          0.25           ;#Step de calcul des streamlines
   set Param(Size)          10.0           ;#Facteur de dimensionnemenr
   set Param(GridVec)       1              ;#Reference geographique des composantes de vecteurs
   set Param(Width)         1              ;#Largeur des segments

   set Param(Factor)        0              ;#Facteur d'ajustement
   set Param(Delta)         0              ;#Facteur d'ajustement
   set Param(Unit)          ""             ;#Type d'unite
   set Param(Desc)          ""             ;#Description
   set Param(Intervals)     {}             ;#Niveaux de contours
   set Param(IntervalMode)  "NONE"         ;#Mode de selection des niveaux
   set Param(IntervalParam) 0              ;#Nombre de niveaux a definir
   set Param(IntervalDef)   ""             ;#Nom du produit dans les listes  ERPG/AEGL

   set Param(Axis)          X
   set Param(X0)            1
   set Param(X1)            0
   set Param(Y0)            1
   set Param(Y1)            0
   set Param(Z0)            1
   set Param(Z1)            0

   #----- Definitions des labels

   set Lbl(Params)        { "Paramètres..." "Parameters..." }
   set Lbl(Color)         { "Couleur" "Color" }
   set Lbl(Contour)       { "Contour" "Contour" }
   set Lbl(Data)          { "Données" "Data" }
   set Lbl(Texture)       { "Texture" "Texture" }
   set Lbl(Volume)        { "Volume" "Volume" }
   set Lbl(Display)       { "Affichage" "Display" }
   set Lbl(Conv)          { "Conv  " "Conv  " }
   set Lbl(Interp)        { "Interp " "Interp " }
   set Lbl(Value)         { "Valeur " "Values " }
   set Lbl(Unit)          { "Unité  " "Units  " }
   set Lbl(Desc)          { "Desc   " "Desc   " }
   set Lbl(Field)         { "Champs" "Field" }
   set Lbl(Grid)          { "Grille" "Grid" }
   set Lbl(Particle)      { "Particule" "Particle" }
   set Lbl(Label)         { "Étiquette" "Labels" }
   set Lbl(Intervals)     { "Intervalles" "Intervals" }
   set Lbl(No)            { "Non" "No" }
   set Lbl(Map)           { "Palette" "Colormap" }
   set Lbl(Topo)          { "Var 3D " "Var 3D " }
   set Lbl(Vector)        { "Vecteur" "Vector" }
   set Lbl(Warning)       { "Attention" "Warning" }
   set Lbl(Yes)           { "Oui" "Yes" }

   #----- Definitions des bulles

   set Bubble(Format)    { "Sélection des paramêtres du format de l'affichage"
                           "Select the display parameters" }
   set Bubble(Unit)      { "Unitées de la variable specifie"
                           "Specified variable units" }
   set Bubble(Desc)      { "Description de la donnnée"
                           "Data description" }
   set Bubble(NomVar)    { "Sélection du champs dont\nvous désirez modifier les paramêtres"
                           "Select the field which you want to change the parameters" }
   set Bubble(Mode)      { "Méthode de sélection des niveaux"
                           "Level selection method" }
   set Bubble(Nb)        { "Nombre de niveaux déterminés"
                           "Number of levels" }
   set Bubble(Conv)      { "Conversion appliquée aux valeurs du champs (+ Delta x Facteur)"
                           "Conversion applied to the field values (+ Delta x Factor)" }
   set Bubble(Intervals) { "Liste des intervals (1 2 3 ... ou [0 1])"
                           "Intervals description (1 2 3 ... ou [0 1])" }
   set Bubble(Font)      { "Police de caractères pour l'information"
                           "Font used to display information" }
   set Bubble(Map)       { "Palette utilisée pour les valeurs"
                           "Colormap used for values" }
}

#-------------------------------------------------------------------------------
# Nom      : <FSTD::Data>
# Creation : Decembre 2002 - J.P. Gauthier - CMC/CMOE
#
# But      : Extraire les donnees du champs.
#
# Parametres  :
#
# Retour      :
#
# Remarque :
#
#-------------------------------------------------------------------------------

proc FSTD::Data { Field } {

   set nomvar [fstdfield define $Field -NOMVAR]
   set text   ""

   if { $nomvar=="INFO" || $nomvar=="OL" } {
      foreach i [join [fstdfield define $Field -DATA]] {
         append text [format "%c" [expr int($i)]]
      }
   } else {
      foreach line [fstdfield define $Field -DATA] {
         append text $line
      }
   }

   return $text
}

#----------------------------------------------------------------------------
# Nom      : <FSTD::ParamFrame>
# Creation : Mars 2000 - J.P. Gauthier - CMC/CMOE
#
# But      : Definition des parametres des options.
#
# Parametres :
#  <Frame>   : Identificateur du frame
#  <Apply>   : Commande d'update de l'etat
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc FSTD::ParamFrame { Frame Apply } {
   global GDefs
   variable Lbl
   variable Bubble
   variable Data
   variable Param

   set Data(ApplyButton) $Apply
   set Data(Frame) [TabFrame::Add $Frame 2 [lindex $Lbl(Field) $GDefs(Lang)] False ""]

   labelframe $Data(Frame).var -text [lindex $Lbl(Field) $GDefs(Lang)]
      ComboBox::Create $Data(Frame).var.sel FSTD::Param(Spec) noedit sorted nodouble -1 \
          "" 18 3 { FSTD::ParamGet; FSTD::ParamPut }
      menubutton $Data(Frame).var.lbl -textvariable FSTD::Param(Mode) -bd 0 -menu $Data(Frame).var.lbl.lst
      pack $Data(Frame).var.lbl -side left -fill x -padx 2 -pady 2
      pack $Data(Frame).var.sel -side left -fill x -expand True -padx 2 -pady 2

      menu $Data(Frame).var.lbl.lst
      foreach mode "FLD VAR TYP IP1 IP2 IP3 ETI DATEO FILE" {
         $Data(Frame).var.lbl.lst add command -label $mode -command "FSTD::VarMode $mode"
      }

   frame $Data(Frame).def
      frame $Data(Frame).def.l

        labelframe $Data(Frame).def.l.val -text [lindex $Lbl(Data) $GDefs(Lang)]
            frame $Data(Frame).def.l.val.interp
               label $Data(Frame).def.l.val.interp.lbl -text [lindex $Lbl(Interp) $GDefs(Lang)]
               ComboBox::Create $Data(Frame).def.l.val.interp.sel FSTD::Param(Interp) noedit sorted nodouble -1 \
                  $FSTD::Param(Interpolator) 7 3 "FSTD::ParamSet"
               pack $Data(Frame).def.l.val.interp.lbl -side left
               pack $Data(Frame).def.l.val.interp.sel -side left -fill x -expand true
            frame $Data(Frame).def.l.val.order
               label $Data(Frame).def.l.val.order.lbl -text [lindex $Lbl(Value) $GDefs(Lang)]
               ComboBox::Create $Data(Frame).def.l.val.order.sel FSTD::Param(Order) noedit unsorted nodouble -1 \
                  $FSTD::Param(Orders) 6 4 { FSTD::ParamSet }
               spinbox $Data(Frame).def.l.val.order.prec -textvariable FSTD::Param(Mantisse) -width 1 -from 0 -to 10 -wrap 1 -bd 1 \
                  -command { FSTD::ParamSet } -bg $GDefs(ColorLight)
               button $Data(Frame).def.l.val.order.font -relief groove -bd 2 -bitmap @$GDefs(Dir)/Resources/Bitmap/font.ico\
                  -command "FontBox::Create $Data(Frame).def.l.val.order.font FSTD::ParamSet \$FSTD::Param(Font)"
               pack $Data(Frame).def.l.val.order.lbl -side left
               pack $Data(Frame).def.l.val.order.font $Data(Frame).def.l.val.order.prec -side left -fill x -expand true
               pack $Data(Frame).def.l.val.order.sel -side left -fill x -expand true
            frame $Data(Frame).def.l.val.mod
               label $Data(Frame).def.l.val.mod.lbl -text [lindex $Lbl(Topo) $GDefs(Lang)]
               entry $Data(Frame).def.l.val.mod.fac -textvariable FSTD::Param(TopoFac) -bd 1 -width 7 -bg $GDefs(ColorLight)
               label $Data(Frame).def.l.val.mod.mul -text "x"
               entry $Data(Frame).def.l.val.mod.val -textvariable FSTD::Param(Topo) -bd 1 -width 7 -bg $GDefs(ColorLight)
               pack $Data(Frame).def.l.val.mod.lbl  $Data(Frame).def.l.val.mod.fac $Data(Frame).def.l.val.mod.mul -side left
               pack $Data(Frame).def.l.val.mod.val -side left -fill x -expand true
               bind $Data(Frame).def.l.val.mod.val <Any-KeyRelease> "FSTD::ParamSet"
               bind $Data(Frame).def.l.val.mod.fac <Any-KeyRelease> "FSTD::ParamSet"
            frame $Data(Frame).def.l.val.fac
               label $Data(Frame).def.l.val.fac.lbl -text [lindex $Lbl(Conv) $GDefs(Lang)]
               label $Data(Frame).def.l.val.fac.lbld -text "+"
               entry $Data(Frame).def.l.val.fac.seld -textvariable FSTD::Param(Delta) -bd 1 -width 7 -bg $GDefs(ColorLight)
               label $Data(Frame).def.l.val.fac.lblf -text "x"
               entry $Data(Frame).def.l.val.fac.self -textvariable FSTD::Param(Factor) -bd 1 -width 7 -bg $GDefs(ColorLight)
               pack $Data(Frame).def.l.val.fac.lbl $Data(Frame).def.l.val.fac.lbld $Data(Frame).def.l.val.fac.seld $Data(Frame).def.l.val.fac.lblf -side left
               pack $Data(Frame).def.l.val.fac.self -side left -fill x -expand true
               bind $Data(Frame).def.l.val.fac.seld <Any-KeyRelease> { FSTD::ParamSet }
               bind $Data(Frame).def.l.val.fac.self <Any-KeyRelease> { FSTD::ParamSet }
           frame $Data(Frame).def.l.val.unit
               label $Data(Frame).def.l.val.unit.lbl -text [lindex $Lbl(Unit) $GDefs(Lang)]
               entry $Data(Frame).def.l.val.unit.sel -textvariable FSTD::Param(Unit) -bd 1 -width 12 -bg $GDefs(ColorLight)
               pack $Data(Frame).def.l.val.unit.lbl -side left
               pack $Data(Frame).def.l.val.unit.sel -side left -fill x -expand true
               bind $Data(Frame).def.l.val.unit.sel <Any-KeyRelease> { FSTD::ParamSet }
            frame $Data(Frame).def.l.val.desc
               label $Data(Frame).def.l.val.desc.lbl -text [lindex $Lbl(Desc) $GDefs(Lang)]
               entry $Data(Frame).def.l.val.desc.sel -textvariable FSTD::Param(Desc) -bd 1 -width 12 -bg $GDefs(ColorLight)
               pack $Data(Frame).def.l.val.desc.lbl -side left
               pack $Data(Frame).def.l.val.desc.sel -side left -fill x -expand true
               bind $Data(Frame).def.l.val.desc.sel <Any-KeyRelease> { FSTD::ParamSet }
            pack $Data(Frame).def.l.val.interp $Data(Frame).def.l.val.order $Data(Frame).def.l.val.mod $Data(Frame).def.l.val.fac $Data(Frame).def.l.val.unit $Data(Frame).def.l.val.desc \
               -side top -padx 2 -anchor n -fill x

         labelframe $Data(Frame).def.l.pal -text [lindex $Lbl(Map) $GDefs(Lang)]
            button $Data(Frame).def.l.pal.cmap -bd 1 -relief flat -image FLDMAPImg \
               -command { MapBox::Create $FSTD::Data(Frame).def.l.pal.cmap \
                   "colormap image \"$FSTD::Param(Map)\" FLDMAPImg; Page::Update $Page::Data(Frame)" $FSTD::Param(Map) }
            pack $Data(Frame).def.l.pal.cmap -side top -fill x -padx 2 -pady 2

         pack $Data(Frame).def.l.val $Data(Frame).def.l.pal -side top -fill x

      frame $Data(Frame).def.r

         labelframe $Data(Frame).def.r.disp -text [lindex $Lbl(Display) $GDefs(Lang)]

         frame $Data(Frame).def.r.disp.p
            IcoMenu::CreateDef $Data(Frame).def.r.disp.p.st $GDefs(Dir)/Resources/Bitmap \
             { dash0.xbm dash1.xbm dash2.xbm dash3.xbm dash4.xbm dash5.xbm } { "" . - .- .-- .-. } \
               FSTD::Param(Dash) "FSTD::ParamSet" 0 -relief groove -bd 2
            IcoMenu::Create $Data(Frame).def.r.disp.p.width $GDefs(Dir)/Resources/Bitmap \
               "width1.xbm width2.xbm width3.xbm width4.xbm width5.xbm" "1 2 3 4 5" \
               FSTD::Param(Width) "FSTD::ParamSet" 0 -relief groove -bd 2
            ColorBox::CreateSel $Data(Frame).def.r.disp.p.col FSTD::Param(Color) FSTD::ParamSet
            checkbutton $Data(Frame).def.r.disp.p.map -image COLORMAP -variable FSTD::Param(MapAll) -onvalue 1 -offvalue 0 \
               -relief sunken -bd 2 -overrelief raised -offrelief groove -command { FSTD::ParamSet } -indicatoron false
            pack $Data(Frame).def.r.disp.p.map $Data(Frame).def.r.disp.p.col $Data(Frame).def.r.disp.p.width -side left
            pack $Data(Frame).def.r.disp.p.st -side left -fill x -expand true

         frame $Data(Frame).def.r.disp.cont
            label $Data(Frame).def.r.disp.cont.lbl -text " [lindex $Lbl(Contour) $GDefs(Lang)]"
            checkbutton $Data(Frame).def.r.disp.cont.sel -variable FSTD::Param(Contour) -relief raised -bd 1 \
               -bitmap @$GDefs(Dir)/Resources/Bitmap/zeroth.xbm -indicatoron false \
               -command "FSTD::ParamSet" -selectcolor "" -relief groove -bd 1
            pack $Data(Frame).def.r.disp.cont.sel -side left -ipadx 1
            pack $Data(Frame).def.r.disp.cont.lbl -side left  -fill y

         frame $Data(Frame).def.r.disp.tex
            label $Data(Frame).def.r.disp.tex.lbl -text " [lindex $Lbl(Texture) $GDefs(Lang)]"
            checkbutton $Data(Frame).def.r.disp.tex.sel -variable FSTD::Param(Texture) -relief raised -bd 1 \
               -bitmap @$GDefs(Dir)/Resources/Bitmap/zeroth.xbm -indicatoron false \
               -command "FSTD::ParamSet" -selectcolor "" -relief groove -bd 1
            pack $Data(Frame).def.r.disp.tex.sel -side left -ipadx 1
            pack $Data(Frame).def.r.disp.tex.lbl -side left -fill y

         frame $Data(Frame).def.r.disp.vol
            label $Data(Frame).def.r.disp.vol.lbl -text " [lindex $Lbl(Volume) $GDefs(Lang)]"
            checkbutton $Data(Frame).def.r.disp.vol.sel -variable FSTD::Param(Volume) -relief raised -bd 1 \
               -bitmap @$GDefs(Dir)/Resources/Bitmap/zeroth.xbm -indicatoron false \
               -command "FSTD::ParamSet" -selectcolor "" -relief groove -bd 1
            pack $Data(Frame).def.r.disp.vol.sel -side left -ipadx 1
            pack $Data(Frame).def.r.disp.vol.lbl -side left -fill y

         frame $Data(Frame).def.r.disp.grid
            label $Data(Frame).def.r.disp.grid.lbl -text " [lindex $Lbl(Grid) $GDefs(Lang)]"
            IcoMenu::Create $Data(Frame).def.r.disp.grid.sel $GDefs(Dir)/Resources/Bitmap \
               "zeroth.xbm size1.xbm size2.xbm size3.xbm size4.xbm size5.xbm" "0 1 2 3 4 5" \
               FSTD::Param(Grid) "FSTD::ParamSet" 0 -relief groove -bd 2
            pack $Data(Frame).def.r.disp.grid.sel $Data(Frame).def.r.disp.grid.lbl -side left

         frame $Data(Frame).def.r.disp.label
            label $Data(Frame).def.r.disp.label.lbl -text " [lindex $Lbl(Label) $GDefs(Lang)]"
            IcoMenu::Create $Data(Frame).def.r.disp.label.sel $GDefs(Dir)/Resources/Bitmap \
               "zeroth.xbm label-1.xbm label4.xbm label3.xbm label2.xbm label1.xbm" "0 -1 6 4 2 1" \
               FSTD::Param(Label) "FSTD::ParamSet" 0 -relief groove -bd 2
            pack $Data(Frame).def.r.disp.label.sel $Data(Frame).def.r.disp.label.lbl -side left

         frame $Data(Frame).def.r.disp.vect
            label $Data(Frame).def.r.disp.vect.lbl -text " [lindex $Lbl(Vector) $GDefs(Lang)]"
            IcoMenu::Create $Data(Frame).def.r.disp.vect.sel $GDefs(Dir)/Resources/Bitmap \
               "zeroth.xbm wind1.xbm wind2.xbm wind3.xbm wind4.xbm" "NONE BARBULE ARROW STREAMLINE STREAMLINE3D" FSTD::Param(Vector) \
               "FSTD::ParamSet" 0 -relief groove -bd 2
            pack $Data(Frame).def.r.disp.vect.sel $Data(Frame).def.r.disp.vect.lbl -side left
            $Data(Frame).def.r.disp.vect.sel.menu add separator
            $Data(Frame).def.r.disp.vect.sel.menu add command -label [lindex $Lbl(Params) $GDefs(Lang)] \
               -command "VectorBox::Create $Data(Frame).def.r.disp.vect \"FSTD::ParamSet ; Page::Update \$Page::Data(Frame); Page::UpdateCommand \$Page::Data(Frame)\""

         frame $Data(Frame).def.r.disp.val
            label $Data(Frame).def.r.disp.val.lbl -text " [lindex $Lbl(Value) $GDefs(Lang)]"
            IcoMenu::Create $Data(Frame).def.r.disp.val.sel $GDefs(Dir)/Resources/Bitmap \
               "zeroth.xbm valmm.xbm valhl.xbm valhls.xbm " "0 1 9 2" \
               FSTD::Param(Value) "FSTD::ParamSet" 0 -relief groove -bd 2
            pack $Data(Frame).def.r.disp.val.sel $Data(Frame).def.r.disp.val.lbl -side left

         frame $Data(Frame).def.r.disp.part
            label $Data(Frame).def.r.disp.part.lbl -text " [lindex $Lbl(Particle) $GDefs(Lang)]"
            IcoMenu::Create $Data(Frame).def.r.disp.part.sel $GDefs(Dir)/Resources/Bitmap \
               "zeroth.xbm size1.xbm size2.xbm size3.xbm size4.xbm size5.xbm" "0 1 2 3 4 5" \
               FSTD::Param(Particle) "FSTD::ParamSet" 0 -relief groove -bd 2
            pack $Data(Frame).def.r.disp.part.sel $Data(Frame).def.r.disp.part.lbl -side left
         pack $Data(Frame).def.r.disp.p $Data(Frame).def.r.disp.cont $Data(Frame).def.r.disp.tex $Data(Frame).def.r.disp.vol \
            $Data(Frame).def.r.disp.grid $Data(Frame).def.r.disp.vect $Data(Frame).def.r.disp.part $Data(Frame).def.r.disp.label \
            $Data(Frame).def.r.disp.val -side top -anchor w -padx 2 -fill x -expand true

          pack $Data(Frame).def.r.disp -side top -fill x
      pack $Data(Frame).def.l $Data(Frame).def.r -side left -padx 5 -pady 5 -fill x -anchor n

   labelframe $Data(Frame).lev -text [lindex $Lbl(Intervals) $GDefs(Lang)]
      frame $Data(Frame).lev.select
         menubutton $Data(Frame).lev.select.mode -textvariable FSTD::Param(IntervalMode) -bd 1 \
            -menu $Data(Frame).lev.select.mode.list -relief raised -width 11 -relief ridge
         ComboBox::Create $Data(Frame).lev.select.def FSTD::Param(IntervalDef) edit sorted nodouble -1 \
            "" 1 6 "FSTD::IntervalSet 1"
         pack $Data(Frame).lev.select.mode -side left
      pack $Data(Frame).lev.select -side top -fill x -padx 2

      frame $Data(Frame).lev.desc
         ComboBox::Create $Data(Frame).lev.desc.edit FSTD::Param(Intervals) editclose sorted nodouble -1 \
            "" 17 6 "FSTD::IntervalSetMode NONE"
         pack $Data(Frame).lev.desc.edit -side left -fill both -expand true
      pack $Data(Frame).lev.desc -side top -fill x -padx 2 -pady 2 -expand true

   pack $Data(Frame).var -side top -fill x -anchor n -padx 5 -pady 5
   pack $Data(Frame).def -side top -fill x -anchor n
   pack $Data(Frame).lev -side top -fill x -anchor n -padx 5

   #----- Creation du menu de mode de niveaux

   menu $Data(Frame).lev.select.mode.list
      $Data(Frame).lev.select.mode.list add cascade -label [lindex $Param(IntervalModes) 1] \
         -menu $Data(Frame).lev.select.mode.list.inter
      $Data(Frame).lev.select.mode.list add separator
      $Data(Frame).lev.select.mode.list add cascade -label [lindex $Param(IntervalModes) 2] \
         -menu $Data(Frame).lev.select.mode.list.nb
      $Data(Frame).lev.select.mode.list add command -label [lindex $Param(IntervalModes) 3] \
         -command "FSTD::IntervalSetMode [lindex $Param(IntervalModes) 3]"
      $Data(Frame).lev.select.mode.list add command -label [lindex $Param(IntervalModes) 4] \
         -command "FSTD::IntervalSetMode [lindex $Param(IntervalModes) 4]"
      $Data(Frame).lev.select.mode.list add separator

      $Data(Frame).lev.select.mode.list add command -label "AEGL(10min)" \
         -command "FSTD::IntervalSetMode AEGL(10min) -1"
      $Data(Frame).lev.select.mode.list add command -label "AEGL(30min)" \
         -command "FSTD::IntervalSetMode AEGL(30min) -1"
      $Data(Frame).lev.select.mode.list add command -label "AEGL(60min)" \
         -command "FSTD::IntervalSetMode AEGL(60min) -1"
      $Data(Frame).lev.select.mode.list add command -label "AEGL(4hr)" \
         -command "FSTD::IntervalSetMode AEGL(4hr) -1"
      $Data(Frame).lev.select.mode.list add command -label "AEGL(8hr)" \
         -command "FSTD::IntervalSetMode AEGL(8hr) -1"
      $Data(Frame).lev.select.mode.list add command -label "ERPG" \
         -command "FSTD::IntervalSetMode ERPG -1"

      $Data(Frame).lev.select.mode.list add separator
      $Data(Frame).lev.select.mode.list add command -label [lindex $Param(IntervalModes) 0] \
         -command "FSTD::IntervalSetMode [lindex $Param(IntervalModes) 0]"

   menu $Data(Frame).lev.select.mode.list.inter
   menu $Data(Frame).lev.select.mode.list.nb
      for { set i 1 } { $i < 25 } { incr i } {
         $Data(Frame).lev.select.mode.list.nb add command -label "$i" \
            -command "FSTD::IntervalSetMode [lindex $Param(IntervalModes) 2] $i"
      }

   bind $Data(Frame).lev.desc.edit.select <KeyRelease> {+ FSTD::IntervalSetMode NONE }

   #------ Creation des bulles d'aide

   Bubble::Create $Data(Frame).def.l.val.order.font $Bubble(Font)
   Bubble::Create $Data(Frame).def.l.val.order      $Bubble(Format)
   Bubble::Create $Data(Frame).def.l.val.unit       $Bubble(Unit)
   Bubble::Create $Data(Frame).def.l.val.desc       $Bubble(Desc)
   Bubble::Create $Data(Frame).def.l.pal.cmap       $Bubble(Map)
   Bubble::Create $Data(Frame).def.l.val.fac        $Bubble(Conv)
   Bubble::Create $Data(Frame).def.r.disp           $Bubble(Format)
   Bubble::Create $Data(Frame).var.sel              $Bubble(NomVar)
   Bubble::Create $Data(Frame).lev.select.mode      $Bubble(Mode)
   Bubble::Create $Data(Frame).lev.select.number    $Bubble(Nb)
   Bubble::Create $Data(Frame).lev.edit             $Bubble(Intervals)
}

#----------------------------------------------------------------------------
# Nom      : <FSTD::IntervalSetMode>
# Creation : Avril 2008 - J.P. Gauthier - CMC/CMOE
#
# But      : Instaurer des valeurs pour les intervalles selon les divers mode
#
# Parametres :
#   <Mode>   : Mode de definition des intervalles
#   <Par>    : Parametres du mode
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc FSTD::IntervalSetMode { Mode { Par 0 } } {
   variable Param
   variable Data

   if { $Mode!="NONE" } {
#      set Param(Intervals)      {}
   }
   set Param(IntervalMode)   $Mode
   set Param(IntervalParam)  $Par

   set mode [string range $Mode 0 3]

   if { $mode=="AEGL" || $mode=="ERPG" } {
      ComboBox::DelAll $Data(Frame).lev.select.def
      ComboBox::AddList $Data(Frame).lev.select.def [array names MetData::$mode]
      pack $Data(Frame).lev.select.def -side right -fill both -expand true

      if { $Par>-1 } {
         set Param(IntervalDef) [lindex [ComboBox::List $Data(Frame).lev.select.def] [expr int($Par)]]
      }
  } else {
      set Param(IntervalDef) ""
      pack forget $Data(Frame).lev.select.def
   }

   FSTD::IntervalSet
}

#----------------------------------------------------------------------------
# Nom      : <FSTD::IntervalSet>
# Creation : Avril 2008 - J.P. Gauthier - CMC/CMOE
#
# But      : Instaurer des valeurs specifiques pour les intervalles (AEGL,ERPG)
#
# Parametres :
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc FSTD::IntervalSet { { Select 0 } } {
   variable Param
   variable Data

   if { $Param(IntervalDef)!="" } {
      upvar #0  MetData::[string range $Param(IntervalMode) 0 3] inter

      switch  $Param(IntervalMode) {
         "AEGL(10min)" { set index 0 }
         "AEGL(30min)" { set index 1 }
         "AEGL(60min)" { set index 2 }
         "AEGL(4hr)"   { set index 3 }
         "AEGL(8hr)"   { set index 4 }
         "ERPG"        { set index 0 }
      }

      set Param(Intervals)     [lindex [lindex $inter($Param(IntervalDef)) 1] $index]
      set Param(IntervalParam) [ComboBox::Index $Data(Frame).lev.select.def exact $Param(IntervalDef)]

      if { $Select } {
         set Param(Unit)          [lindex $inter($Param(IntervalDef)) 0]
         set Param(Desc)          $Param(IntervalDef)
      }
   }
   FSTD::ParamSet
}

#----------------------------------------------------------------------------
# Nom      : <FSTD::FieldFormat>
# Creation : Octobre 2002 - J.P. Gauthier - CMC/CMOE
#
# But      : Formatter les valeur
#
# Parametres :
#   <Val>    : Valeur
#
# Retour:
#   <val>    : Valeur formatte
#
# Remarques :
#
#----------------------------------------------------------------------------

proc FSTD::FieldFormat { Field Val } {
   variable Param

   set val      [fstdfield configure $Field -value]
   set order    [lindex $val 0]
   set mantisse [lindex $val 1]

   if { $mantisse==0 } {
      set dec 2
   } else {
      set dec $mantisse
   }

   set bad [catch {
      if { $order=="AUTO" || $order=="EXPONENTIAL" } {
         eval set Val \[format \"%1.${dec}e\" $Val\]
      } elseif { $order == "INTEGER" } {
         catch { eval set Val \[format \"%i\" [expr int($Val)]\] }
      } elseif { $order == "FLOAT" } {
         eval set Val \[format \"%1.${dec}f\" $Val\]
      }
   }]

   return $Val
}

#----------------------------------------------------------------------------
# Nom      : <FSTD::Follower>
# Creation : Novembre 2004 - J.P. Gauthier - CMC/CMOE
#
# But      : Plugin au bindings de suivit de coordonnees du Viewport
#
# Parametres :
#   <Frame>  : Page courante
#   <Canvas> : Canvas courant
#   <VP>     : Viewport courant
#   <Lat>    : Lattitude
#   <Lon>    : Longitude
#   <X>      : Pixel en X
#   <Y>      : Pixel en Y
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc FSTD::Follower { Page Canvas VP Lat Lon X Y } {
   variable Data

   #----- Obtenir la valeur du champs a ces coordonnees

   set list {}

   foreach field [lindex [$Canvas itemconfigure $VP -data] 4] {

      if { [fstdfield is $field] && [fstdfield define $field -GRTYP]!="V" } {
         set ij [fstdfield stats $field -coordpoint $Lat $Lon]
         set pij [fstdfield stats $field -unproject $Lat $Lon]
         set value [fstdfield stats $field -coordvalue $Lat $Lon]
         set spd [lindex $value 0]
         set dir [lindex $value 1]

         if { $dir!="" } {
            catch { set value [FSTD::FieldFormat $field $spd]@[format "%1.2f" $dir] }
         } else {
            set value [FSTD::FieldFormat $field $spd]
         }
         lappend list [list [fstdfield configure $field -desc] $ij $pij $value]
         append Page::Data(Value) "[fstdfield configure $field -desc]:$value "
      }
   }
   return $list
}

#-------------------------------------------------------------------------------
# Nom      : <FSTD::ParamGet>
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

proc FSTD::ParamGet { { Spec "" } } {
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
   set Param(Color)     [dataspec configure $Spec -color]
   set Param(Dash)      [dataspec configure $Spec -dash]
   set Param(Width)     [dataspec configure $Spec -width]
   set Param(Contour)   [dataspec configure $Spec -rendercontour]
   set Param(Texture)   [dataspec configure $Spec -rendertexture]
   set Param(Volume)    [dataspec configure $Spec -rendervolume]
   set Param(Coord)     [dataspec configure $Spec -rendercoord]
   set Param(Value)     [dataspec configure $Spec -rendervalue]
   set Param(Label)     [dataspec configure $Spec -renderlabel]
   set Param(Grid)      [dataspec configure $Spec -rendergrid]
   set Param(Particle)  [dataspec configure $Spec -renderparticle]
   set Param(Intervals) [dataspec configure $Spec -intervals]
   set Param(Interp)    [dataspec configure $Spec -interpdegree]
   set Param(Extrap)    [dataspec configure $Spec -extrapdegree]
   set Param(Topo)      [dataspec configure $Spec -topography]
   set Param(TopoFac)   [dataspec configure $Spec -topographyfactor]
   set Param(Sample)    [dataspec configure $Spec -sample]
   set Param(Step)      [dataspec configure $Spec -step]
   set Param(Min)       [dataspec configure $Spec -min]
   set Param(Max)       [dataspec configure $Spec -max]
   set Param(GridVec)   [dataspec configure $Spec -gridvector]
   set Param(Vector)    [dataspec configure $Spec -rendervector]
   set Param(Axis)      [dataspec configure $Spec -axis]

   set plane            [dataspec configure $Spec -cube]
   set Param(X0)        [lindex $plane 0]
   set Param(Y0)        [lindex $plane 1]
   set Param(Z0)        [lindex $plane 2]
   set Param(X1)        [lindex $plane 3]
   set Param(Y1)        [lindex $plane 4]
   set Param(Z1)        [lindex $plane 5]

   if { $Param(Min)!=$Param(Max) } {
      set Param(Intervals) ""
      if { $Param(Min)!="" } {
         append Param(Intervals) "\[$Param(Min)"
      }
      if { $Param(Max)!="" } {
         append Param(Intervals) " $Param(Max)\]"
      }
   }
   FSTD::IntervalSetMode $Param(IntervalMode) $Param(IntervalParam)
}

#----------------------------------------------------------------------------
# Nom      : <FSTD::ParamSet>
# Creation : Mars 2000 - J.P. Gauthier - CMC/CMOE
#
# But      : Initialise les options de fichiers standards
#
# Parametres :
#   <Apply>  : Apply button
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc FSTD::ParamSet { { Spec "" } } {
   variable Param
   variable Data

   if { $Spec=="" } {
      set Spec $Param(Spec)
   }

   if { ![dataspec is $Spec] } {
      return
   }

   set inter $Param(Intervals)
   set label {}
   set min   ""
   set max   ""

   #----- Verifier pour un range plutot que des niveaux

   if { [set from [string first "\[" $Param(Intervals)]]!=-1 } {
      set min [lindex [string range $Param(Intervals) [incr from] end] 0]
      set inter {}
   }

   if { [set to [string first "\]" $Param(Intervals)]]!=-1 } {
      set max [lindex [string range $Param(Intervals) 0 [incr to -1]] end]
      set inter {}
   }

   if { [string first "(" $Param(Intervals)]!=-1 } {
      set inter {}
      foreach { val } [split $Param(Intervals) )] {
         if { [llength [set val [split $val (]]]>1 } {
            lappend inter [lindex $val 0]
            lappend label [lindex $val 1]
         }
      }
   }

   dataspec configure $Spec -factor $Param(Factor) -delta $Param(Delta) -value $Param(Order) $Param(Mantisse) -size $Param(Size) -font $Param(Font) -colormap $Param(Map) \
      -color $Param(Color) -dash $Param(Dash) -width $Param(Width) -unit $Param(Unit) -desc $Param(Desc) -rendercontour $Param(Contour) -mapall $Param(MapAll) \
      -rendervector $Param(Vector) -rendertexture $Param(Texture) -rendervolume $Param(Volume)  -rendervalue $Param(Value) -renderlabel $Param(Label) \
      -renderparticle $Param(Particle) -rendergrid $Param(Grid) -interpdegree $Param(Interp) -extrapdegree $Param(Extrap) -topography $Param(Topo) \
      -topographyfactor $Param(TopoFac) -sample $Param(Sample) -step $Param(Step) -gridvector $Param(GridVec) \
      -cube [list $Param(X0) $Param(Y0) $Param(Z0) $Param(X1) $Param(Y1) $Param(Z1)] -axis $Param(Axis) \
      -intervals $inter -interlabels $label -min $min -max $max -intervalmode $Param(IntervalMode) $Param(IntervalParam)

   catch { $Data(ApplyButton) configure -state normal }

   MetStat::RECRCAdd [dataspec configure $Spec -desc] "" $Param(Unit) $Param(Factor) "" ""
}

#-------------------------------------------------------------------------------
# Nom      : <FSTD::ParamPut>
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

proc FSTD::ParamPut { } {
   variable Data
   variable Param

   IcoMenu::Set $Data(Frame).def.r.disp.vect.sel   $Param(Vector)
   IcoMenu::Set $Data(Frame).def.r.disp.grid.sel   $Param(Grid)
   IcoMenu::Set $Data(Frame).def.r.disp.vect.sel   $Param(Vector)
   IcoMenu::Set $Data(Frame).def.r.disp.part.sel   $Param(Particle)

   IcoMenu::Set $Data(Frame).def.r.disp.p.st      $Param(Dash)
   IcoMenu::Set $Data(Frame).def.r.disp.p.width   $Param(Width)
   IcoMenu::Set $Data(Frame).def.r.disp.label.sel $Param(Label)
   IcoMenu::Set $Data(Frame).def.r.disp.val.sel   $Param(Value)

   ColorBox::ConfigNoColor $Data(Frame).def.r.disp.p.col $Param(Color)

   if { [colormap is $Param(Map)] } {
      colormap image $Param(Map) FLDMAPImg
   } else {
      FLDMAPImg blank
   }

   #----- Recuperer les niveaux du .recrc

   set inters $Param(Intervals)

   ComboBox::DelAll $Data(Frame).lev.desc.edit
   $Data(Frame).lev.select.mode.list.inter delete 0 end

   if { [dataspec is $Param(Spec)] } {
      set var [dataspec configure $Param(Spec) -desc]

      if { [lsearch -exact $MetStat::Rec(Var) $var]!=-1 } {
         ComboBox::AddList $Data(Frame).lev.desc.edit $MetStat::Rec(Level$var)
         set Param(Unit)   $MetStat::Rec(Unit$var)
         set Param(Factor) $MetStat::Rec(Factor$var)

         foreach inter $MetStat::Rec(Inter$var) {
            $Data(Frame).lev.select.mode.list.inter add command -label "$inter" \
               -command "FSTD::IntervalSetMode INTERVAL $inter"
         }
      }
   }
   set Param(Intervals) $inters
}

#-------------------------------------------------------------------------------
# Nom      : <FSTD::ParamInit>
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

proc FSTD::ParamInit { Field { Spec "" } } {
   variable Param

   if { [dataspec is $Spec] } {
      set set [dataspec configure $Spec -set]
      set var [dataspec configure $Spec -desc]

      if { !$set } {
         dataspec copy $Spec FLDDEFAULT
         dataspec configure $Spec -desc $var
      }

      #----- Set a colormap if not done
      set map [dataspec configure $Spec -colormap]
      if { $map=="" } {
         set map FLDMAP$Spec
      }

      if { ![colormap is $map] } {
         colormap create FLDMAP$Spec
         colormap copy   FLDMAP$Spec FLDMAPDEFAULT
         dataspec configure $Spec -colormap FLDMAP$Spec
      }

      if { !$set } {
         #----- Override particle fields params
         if { [fstdfield define $Field -GRTYP]=="Y" || [fstdfield define $Field -NOMVAR]=="ZH" } {
            dataspec configure $Spec -renderparticle 2
         }

         #----- Override vectorial fields params
         if { [fstdfield stats $Field -component]>1 } {
            dataspec configure $Spec -rendervector BARBULE -rendertexture 0
         }

         if { [lsearch -exact $MetStat::Rec(Var) $var]!=-1 } {
            dataspec configure $Spec -unit $MetStat::Rec(Unit$var) -factor $MetStat::Rec(Factor$var)
         }
      }
   }
}

#----------------------------------------------------------------------------
# Nom      : <FSTD::Register>
# Creation : Novembre 2005 - J.P. Gauthier - CMC/CMOE
#
# But      : Enregistrer le champs dans la liste des champs connus et configurable
#
# Parametres  :
#   <FieldId> : Identificateur de champs
#   <Update>  : Mise a jour des parametres
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc FSTD::Register { FieldId { Update True } } {
   variable Data
   variable Param

   if { [set idx [lsearch -exact $Data(ListTool) $FieldId]]==-1 } {
      lappend Data(ListTool) $FieldId
   }
   if { $Update } {
      FSTD::ParamUpdate
   }
   return $idx
}

proc FSTD::UnRegister { FieldId { Update True } } {
   variable Data

   if { [set idx [lsearch -exact $Data(ListTool) $FieldId]]!=-1 } {
      set Data(ListTool) [lreplace $Data(ListTool) $idx $idx]
   }
   if { $Update } {
      FSTD::ParamUpdate
   }
   return $idx
}

#-------------------------------------------------------------------------------
# Nom      : <FSTD::Params>
# Creation : Aout 1999 - J.P. Gauthier - CMC/CMOE
#
# But      : Initialise les parametres pour une type de champs.
#
# Parametres  :
#    <Id>     : Type de champs
#    <Params> : Parametres du champs
#    <args>   : Palette de couleur (Sinon, palette par defaut)
#
# Retour      :
#
# Remarque :
#
#-------------------------------------------------------------------------------

proc FSTD::Params { Id Map args } {
   global   env
   variable Param

   if { ![dataspec is $Id] } {
      dataspec create $Id
   }

   if { ![colormap is FLDMAP$Id] } {
      colormap create FLDMAP$Id
   }
   colormap read FLDMAP$Id $env(HOME)/.spi/Colormap/$Map.rgba

   eval dataspec configure $Id $args -font FLDFONTDEFAULT -colormap FLDMAP$Id
}

#----------------------------------------------------------------------------
# Nom      : <FSTD::ParamUpdate>
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

proc FSTD::ParamUpdate { { Fields { } } } {
   variable Param
   variable Data

   if { ![llength $Fields] } {
      set Fields [concat $Data(List) $Data(ListTool)]
   }

   set current $Param(Spec)
   set exist 0
   set var   ""

   #----- Inserer les items dans la liste de configuration

   ComboBox::DelAll $Data(Frame).var.sel

   foreach fld $Fields {
      if { [fstdfield is $fld] } {

          switch $Param(Mode) {
            "FLD"   { set var $fld }
            "VAR"   { set var [fstdfield configure $fld -desc] }
            "TYP"   { set var [fstdfield define $fld -TYPVAR] }
            "IP1"   { set var [fstdfield define $fld -IP1] }
            "IP2"   { set var [fstdfield define $fld -IP2] }
            "IP3"   { set var [fstdfield define $fld -IP3] }
            "ETI"   { set var [fstdfield define $fld -ETIKET] }
            "DATEO" { set var [fstdfield define $fld -DATEO] }
            "FILE"  { set var [fstdfield define $fld -FID] }
         }

         if { [set var [string trim $var]]=="" } {
            set var "<>"
         }

         if { ![dataspec is $var] } {
            set spec [fstdfield configure $fld -dataspec]
            dataspec copy $var $spec
            dataspec free $spec

            FSTD::ParamInit $fld $var
         }
         if { "$var"=="$current" } {
             set Param(Spec) $current
             set exist 1
         }

         fstdfield configure $fld -dataspec $var
         ComboBox::Add $Data(Frame).var.sel $var
      }
   }

   if { !$exist && $var!="" } {
      set Param(Spec) $var
      FSTD::ParamGet
      FSTD::ParamPut
   }
}

#----------------------------------------------------------------------------
# Nom      : <FSTD::FieldMode>
# Creation : Aout 2001 - J.P. Gauthier - CMC/CMOE
#
# But      : Modifier le mode de selection des parametres des champs
#
# Parametres :
#    <Mode>  : Mode de selection des parametres
#
# Retour:
#
# Remarques :
#     - Il y a differente methodes:
#
#         FLD  : Parametres par champs
#         VAR  : Parametres par NOMVAR
#         TYP  : Parametres par TYPVAR
#         IP1  : Parametres par IP1
#         IP2  : Parametres par IP2
#         IP3  : Parametres par IP3
#         ETI  : Parametres par ETIKET
#         DATEO: Parametres par DATEO
#         FILE  : Parametres par FILE
#
#----------------------------------------------------------------------------

proc FSTD::VarMode { Mode } {
   variable Param

   set Param(Mode) $Mode
   FSTD::ParamUpdate
}
