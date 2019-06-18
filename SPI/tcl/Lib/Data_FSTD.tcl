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
#   FSTD::ParamGetMode    { Field } 
#   FSTD::ParamSetDefault { Field args }
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
package require FieldFunc

namespace eval FSTD {
   global   env
   variable Data
   variable Param
   variable Params
   variable Lbl
   variable Error

   #----- Creer les parametres de la palette par defaut

   font create FLDFONTDEFAULT -family courrier -size -12 -weight bold
   font create FLDFONTEXTREMA -family helvetica -size -16 -weight bold

   image create photo FLDMAPImg -width 205 -height 15
   colormap create FLDMAPDEFAULT -file $env(HOME)/.spi/Colormap/REC_Col.std1.rgba
   colormap image  FLDMAPDEFAULT FLDMAPImg

   dataspec create FLDDEFAULT -set 2 -factor 1.0 -delta 0.0 -value AUTO -1 -size 10 -sizerange 2 -width 1 -font FLDFONTDEFAULT -efont FLDFONTEXTREMA \
      -color #000000 -unit "" -dash "" -rendercontour 0 -rendervector NONE -rendertexture 1 -renderparticle 0 -rendergrid 0 -renderboundary 0 \
      -rendervolume 0 -rendercoord 0 -rendervalue 0 -renderlabel 0 -intervalmode NONE 0 -interpdegree LINEAR  -sample 2 -sampletype PIXEL \
      -intervals {} -mapbelow False -mapabove True -transparency 100 -mask True
      
   geodata vector { UU   VV     }
   geodata vector { UP   VP     }
   geodata vector { UD   VD     }
   geodata vector { UU2W VV2W   }
   geodata vector { UUW  VVW    }
   geodata vector { U    V    W }

   if { ![llength [info command gribfile]] } {
      Log::Print WARNING "libSPI has not been build with libeccodes, grib (gribfile) will not be supported"
   }
   
   Viewport::FollowerAdd FSTD

   #----- Variable internes

   set Data(Frame)          ""          ;#Frame contenant les definitions de parameters
   set Data(Apply)          0           ;#Indicateur de changement des parametres
   set Data(Command)        ""          ;#Commande de reaffichage

   set Data(List)          {}          ;#Liste des champs ouvert
   set Data(ListTool)      {}          ;#Liste des champs ouvert par d'autres outils

   set Param(Spec)          ""                                     ;#Variable a parametrer

   #----- Parametres du champs

   set Param(AutoREC)        False
   set Param(Interpolator)   { NEAREST LINEAR }
   set Param(Extrapolator)   { NEUTRAL MAXIMUM MINIMUM }
   set Param(Orders)         { AUTO INTEGER FLOAT EXPONENTIAL }
   set Param(IntervalModes)  { NONE INTERVAL LINEAR LOGARITHMIC RSMC }
   set Param(GridIds)        { SUPER YIN YANG }

   catch { set Param(UnTile) [fstdfield autountile]  ;#Reconstruction automatique des grilles tuilees }

   set Param(Mode)          VAR            ;#Mode de selection des parametres
   set Param(Map)           FLDMAPDEFAULT  ;#Palette de couleur
   set Param(Font)          FLDFONTDEFAULT ;#Police
   set Param(EFont)         FLDFONTEXTREMA ;#Police extrema
   set Param(Interp)        LINEAR         ;#Type d'interpolation
   set Param(Extrap)        NEUTRAL        ;#Type d'extrapolation
   set Param(Contour)       0              ;#Affichage des contours
   set Param(MapAll)        0              ;#Affichage des contours avec la colormap
   set Param(MapAbove)      1              ;#Affichage de couleur au dessus du dernier interval
   set Param(MapBelow)      0              ;#Affichage de coule en dessous du premier interval
   set Param(Grid)          0              ;#Affichage de la grille
   set Param(Boundary)      0              ;#Affichage des limites géographiques
   set Param(Vector)        NONE           ;#Affichage vectorise
   set Param(Texture)       1              ;#Affichage des donnees
   set Param(Volume)        0              ;#Affichage des donnees volumetriques
   set Param(Particle)      0              ;#Affichage de particules
   set Param(Label)         0              ;#Affichage des labels
   set Param(Value)         0              ;#Affichage des valeurs (centrales,min,max)
   set Param(Order)         AUTO           ;#Type d'affichage de valeur
   set Param(Mantisse)      -1             ;#Dimension de la mantisse
   set Param(Color)         #000000        ;#Couleur des items vectoriels
   set Param(Dash)          ""             ;#Pattern des items vectoriels
   set Param(Topo)          ""             ;#Modulation 3D
   set Param(TopoFac)       1.0            ;#Facteur de modulation 3D
   set Param(Sample)        4              ;#Sampling des points vectoriels
   set Param(SampleType)    PIXEL          ;#Type de sampling des points vectoriels
   set Param(Step)          0.25           ;#Step de calcul des streamlines
   set Param(Size)          10.0           ;#Facteur de dimensionnemenr
   set Param(SizeRange)     2.0            ;#Facteur d'application de la dimension selon le range de valeur
   set Param(GridVec)       1              ;#Reference geographique des composantes de vecteurs
   set Param(Width)         1              ;#Largeur des segments
   set Param(GridNo)        0              ;#Grid to display no (grid/subgrid)
   set Param(GridId)        SUPER          ;#Grid to display name (grid/subgrid)
   set Param(Alpha)         FF             ;#Global transparency
   set Param(Mask)          True           ;#Grid space mask

   set Param(Inters)        {}
   set Param(Labels)        {}

   set Param(Factor)        0              ;#Facteur d'ajustement
   set Param(Delta)         0              ;#Facteur d'ajustement
   set Param(Unit)          ""             ;#Type d'unite
   set Param(Desc)          ""             ;#Description
   set Param(Intervals)     {}             ;#Niveaux de contours
   set Param(Intervals0)    {}             ;#Niveaux de contours
   set Param(Intervals1)    {}             ;#Niveaux de contours
   set Param(Intervals2)    {}             ;#Niveaux de contours
   set Param(IntervalMode)  "NONE"         ;#Mode de selection des niveaux
   set Param(IntervalParam) 0              ;#Nombre de niveaux a definir
   set Param(IntervalDef)   ""             ;#Nom du produit dans les listes  ERPG/AEGL
   set Param(Interspecs)    {}             ;#List de configuration specifiques de contours
   set Param(InterspecsNb)  3              ;#Nombre maximum de configuration specifiques de contours

   set Param(Axis)          X
   set Param(X0)            1
   set Param(X1)            0
   set Param(Y0)            1
   set Param(Y1)            0
   set Param(Z0)            1
   set Param(Z1)            0

   #----- Definitions des labels

   set Lbl(Params)        { "Paramètres..." "Parameters..." }
   set Lbl(Alpha)         { "Transparence" "Transparency" }
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
   set Lbl(Grid)          { "Grille " "Grid   " }
   set Lbl(Particle)      { "Particule" "Particle" }
   set Lbl(Label)         { "Étiquette" "Labels" }
   set Lbl(Intervals)     { "Intervalles" "Intervals" }
   set Lbl(No)            { "Non" "No" }
   set Lbl(Map)           { "Palette" "Colormap" }
   set Lbl(Topo)          { "Var 3D " "Var 3D " }
   set Lbl(Vector)        { "Vecteur" "Vector" }
   set Lbl(Warning)       { "Attention" "Warning" }
   set Lbl(Yes)           { "Oui" "Yes" }
   set Lbl(UnTile)        { "Dé #" "Un #" }
   set Lbl(Mask)          { "Masque" "Mask" }

   #----- Definitions des bulles

   set Bubble(Intervals)  { "Liste des intervals de contours"
                            "List of contour intervals" }
   set Bubble(InterSpecs) { "Configuration d'affichage spécifique à certains intervales de contour"
                            "Specific display intervals configuration" }
   set Bubble(Format)     { "Sélection du format de l'affichage des valeurs"
                            "Select the display format of values" }
   set Bubble(MapAll)     { "Appliquer la palette aux vecteurs, contours et points"
                            "Apply colormap to vectors, contours and points" }
   set Bubble(Color)      { "Sélection de la couleur des contours, points et libelles"
                            "Select color for contour, points and labels" }
   set Bubble(Width)      { "Sélection de la largeur des contours et lignes de courants"
                            "Select width of contour and streamline" }
   set Bubble(Dash)       { "Sélection du pointillé des countours"
                            "Selec contour dash type" }
   set Bubble(Contour)    { "Affichage de contour selon la précision sélectionnée"
                            "Display contours based on selected precision" }
   set Bubble(Texture)    { "Affichage du remplissage des cellules selon la palette"
                            "Display cell filling based on colormap" }
   set Bubble(Volume)     { "Affichage en iso-surface 3D des intervalles spécifiés"
                            "Display 3D isosurface of the selected intervals" }
   set Bubble(Grid)       { "Affichage des points de grilles selon la dimension sélectionnée"
                            "Display gridpoints based on selected dimension" }
   set Bubble(Vector)     { "Afichage des vecteurs selon le mode sélectionné (champs vectoriels)"
                            "Display vectors based on selected mode (vectorial fields)" }
   set Bubble(Particle)   { "Affichage des parcelles selon la dimension sélectionnée (nuages de points)"
                            "Display parcels based on selected dimension" }
   set Bubble(Label)      { "Affichage de libelle sur les contours"
                            "Display contour labels" }
   set Bubble(Value)      { "Affichage du min/max ou valeur centrales"
                            "Display min/max or central values" }
   set Bubble(Mask)       { "Appliquer le masque si existant (TYPVAR @@)"
                            "Apply mask if found (TYPVAR @@)" }                        
   set Bubble(Unit)       { "Unitées de la variable specifie"
                            "Specified variable units" }
   set Bubble(Desc)       { "Description de la donnnée"
                            "Data description" }
   set Bubble(NomVar)     { "Sélection du champs dont\nvous désirez modifier les paramêtres"
                            "Select the field which you want to change the parameters" }
   set Bubble(Mode)       { "Méthode de sélection des niveaux"
                            "Level selection method" }
   set Bubble(Nb)         { "Nombre de niveaux déterminés"
                            "Number of levels" }
   set Bubble(Conv)       { "Conversion appliquée aux valeurs du champs (+ Delta x Facteur)"
                            "Conversion applied to the field values (+ Delta x Factor)" }
   set Bubble(Intervals)  { "Liste des intervals (1 2 3 ... ou [0 1])"
                            "Intervals description (1 2 3 ... ou [0 1])" }
   set Bubble(Font)       { "Police de caractères pour l'information"
                            "Font used to display information" }
   set Bubble(Map)        { "Palette utilisée pour les valeurs"
                            "Colormap used for values" }
   set Bubble(MapAbove)   { "Palette utilisée pour les valeurs haut dessus du maximum spécifié"
                            "Colormap used for values above maximum specified" }
   set Bubble(MapBelow)   { "Palette utilisée pour les valeurs sous le minimum spécifié"
                            "Colormap used for values bellow minimum specified" }
   set Bubble(Interp)     { "Méthode d'interpolation des données (Lissage)"
                            "Interpolation method (Smoothing)" }
   set Bubble(Grid)       { "Sélection de sous-grilles ou grille maitre (GRTYP=U)"
                            "Sub-grid or master grid selection (GRTYP=U)" }
   set Bubble(Tile)       { "Sélectionnez pour reconstruire les grilles\ntuilées (GRTYP=#) en une seule grille"
                            "Select to rebuild tiled grids (GRTYP=#) into one" }
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
   set nbit   [fstdfield define $Field -NBITS]
   set text   ""

   if { $nomvar=="INFO" || $nomvar=="OL" } {
      foreach i [join [fstdfield define $Field -DATA 0]] {
         append text [format "%c" [expr int(($nbit==8 && $i>127)?$i-128:$i)]]
      }
   } else {
      foreach line [fstdfield define $Field -DATA 0] {
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
      menubutton $Data(Frame).var.lbl -textvariable FSTD::Param(Mode) -relief groove -bd 2 -menu $Data(Frame).var.lbl.lst
      pack $Data(Frame).var.lbl -side left -fill x -padx 2 -pady 1
      pack $Data(Frame).var.sel -side left -fill both -expand True -padx 2 -pady 2

      menu $Data(Frame).var.lbl.lst
      foreach mode "FLD VAR TYPVAR LEVEL IP1 IP2 IP3 ETIKET DATEO FILE" {
         $Data(Frame).var.lbl.lst add command -label $mode -command "FSTD::VarMode $mode"
      }

      pack [Styles::Widget $Data(Frame).var]  -side left -padx 2 -pady 2
   pack $Data(Frame).var -side top -fill x -anchor n -padx 5 -pady 5

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
               spinbox $Data(Frame).def.l.val.order.prec -textvariable FSTD::Param(Mantisse) -width 1 -from -1 -to 10 -wrap 1 -bd 1 \
                  -command { FSTD::ParamSet } -bg $GDefs(ColorLight)
               button $Data(Frame).def.l.val.order.font -relief groove -bd 2 -bitmap @$GDefs(Dir)/share/bitmap/font.ico\
                  -command "FontBox::Create $Data(Frame).def.l.val.order.font FSTD::ParamSet \$FSTD::Param(Font)"
               bind $Data(Frame).def.l.val.order.prec <Any-KeyRelease> "FSTD::ParamSet"
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
            frame $Data(Frame).def.l.val.grid
               label $Data(Frame).def.l.val.grid.lbl -text [lindex $Lbl(Grid) $GDefs(Lang)]
               ComboBox::Create $Data(Frame).def.l.val.grid.sel FSTD::Param(GridId) noedit unsorted nodouble -1 \
                  $FSTD::Param(GridIds) 7 3 "FSTD::ParamSet; FSTD::ParamUpdate; Viewport::ForceGrid \$Page::Data(Frame) True"
               checkbutton $Data(Frame).def.l.val.grid.tile -text [lindex $Lbl(UnTile) $GDefs(Lang)] -variable FSTD::Param(UnTile) -onvalue 1 -offvalue 0 \
                 -relief sunken -bd 2 -overrelief raised -offrelief groove -command { fstdfield autountile $FSTD::Param(UnTile) } -indicatoron false
               pack $Data(Frame).def.l.val.grid.lbl -side left
               pack $Data(Frame).def.l.val.grid.sel -side left -fill x -expand true
               pack $Data(Frame).def.l.val.grid.tile -side left
            pack $Data(Frame).def.l.val.interp $Data(Frame).def.l.val.order $Data(Frame).def.l.val.mod $Data(Frame).def.l.val.fac \
               $Data(Frame).def.l.val.unit $Data(Frame).def.l.val.desc $Data(Frame).def.l.val.grid -side top -padx 2 -anchor n -fill x

         labelframe $Data(Frame).def.l.pal -text [lindex $Lbl(Map) $GDefs(Lang)]
            button $Data(Frame).def.l.pal.cmap -bd 1 -relief flat -image FLDMAPImg \
               -command { MapBox::Create $FSTD::Data(Frame).def.l.pal.cmap \
                   "colormap image \"\$FSTD::Param(Map)\" FLDMAPImg; Page::Update $Page::Data(Frame)" $FSTD::Param(Map) }
            pack $Data(Frame).def.l.pal.cmap -side top -fill x -padx 2 -pady 2

         pack $Data(Frame).def.l.val $Data(Frame).def.l.pal -side top -fill x

      frame $Data(Frame).def.r

         labelframe $Data(Frame).def.r.disp -text [lindex $Lbl(Display) $GDefs(Lang)]

         frame $Data(Frame).def.r.disp.p
            IcoMenu::CreateDef $Data(Frame).def.r.disp.p.st $GDefs(Dir)/share/bitmap \
             { dash0.xbm dash1.xbm dash2.xbm dash3.xbm dash4.xbm dash5.xbm } { "" . - .- .-- .-. } \
               FSTD::Param(Dash) "FSTD::ParamSet" 0 -relief groove -bd 2
            IcoMenu::Create $Data(Frame).def.r.disp.p.width $GDefs(Dir)/share/bitmap \
               "width1.xbm width2.xbm width3.xbm width4.xbm width5.xbm" "1 2 3 4 5" \
               FSTD::Param(Width) "FSTD::ParamSet" 0 -relief groove -bd 2
            ColorBox::CreateSel $Data(Frame).def.r.disp.p.col { FSTD::Param(Color) FSTD::Param(Alpha) } FSTD::ParamSet
            checkbutton $Data(Frame).def.r.disp.p.map -image COLORMAP -variable FSTD::Param(MapAll) -onvalue 1 -offvalue 0 \
               -relief sunken -bd 2 -overrelief raised -offrelief groove -command { FSTD::ParamSet } -indicatoron false
            pack $Data(Frame).def.r.disp.p.map $Data(Frame).def.r.disp.p.col $Data(Frame).def.r.disp.p.width -side left
            pack $Data(Frame).def.r.disp.p.st -side left -fill x -expand true

         frame $Data(Frame).def.r.disp.cont
            label $Data(Frame).def.r.disp.cont.lbl -text " [lindex $Lbl(Contour) $GDefs(Lang)]"
            IcoMenu::Create $Data(Frame).def.r.disp.cont.sel $GDefs(Dir)/share/bitmap \
               "zeroth.xbm contour1.xbm contour2.xbm contour3.xbm" "0 1 2 3" \
               FSTD::Param(Contour) "FSTD::ParamSet" 0 -relief groove -bd 2
            pack $Data(Frame).def.r.disp.cont.sel -side left -ipadx 1
            pack $Data(Frame).def.r.disp.cont.lbl -side left  -fill y

         frame $Data(Frame).def.r.disp.tex
            label $Data(Frame).def.r.disp.tex.lbl -text " [lindex $Lbl(Texture) $GDefs(Lang)]"
            checkbutton $Data(Frame).def.r.disp.tex.sel -variable FSTD::Param(Texture) -relief raised -bd 1 \
               -bitmap @$GDefs(Dir)/share/bitmap/zeroth.xbm -indicatoron false \
               -command "FSTD::ParamSet" -selectcolor "" -relief groove -bd 1
            pack $Data(Frame).def.r.disp.tex.sel -side left -ipadx 1
            pack $Data(Frame).def.r.disp.tex.lbl -side left -fill y

         frame $Data(Frame).def.r.disp.vol
            label $Data(Frame).def.r.disp.vol.lbl -text " [lindex $Lbl(Volume) $GDefs(Lang)]"
            IcoMenu::Create $Data(Frame).def.r.disp.vol.sel $GDefs(Dir)/share/bitmap \
               "zeroth.xbm spline.xbm bound.xbm" "0 1 2" \
               FSTD::Param(Volume) "FSTD::ParamSet" 0 -relief groove -bd 2
            pack $Data(Frame).def.r.disp.vol.sel -side left -ipadx 1
            pack $Data(Frame).def.r.disp.vol.lbl -side left -fill y

         frame $Data(Frame).def.r.disp.grid
            label $Data(Frame).def.r.disp.grid.lbl -text " [lindex $Lbl(Grid) $GDefs(Lang)]"
            IcoMenu::Create $Data(Frame).def.r.disp.grid.sel $GDefs(Dir)/share/bitmap \
               "zeroth.xbm size1.xbm size2.xbm size3.xbm size4.xbm size5.xbm bound.xbm" "0 1 2 3 4 5 6" \
               FSTD::Param(Grid) "FSTD::ParamSet" 0 -relief groove -bd 2
             pack $Data(Frame).def.r.disp.grid.sel $Data(Frame).def.r.disp.grid.lbl -side left

         frame $Data(Frame).def.r.disp.label
            label $Data(Frame).def.r.disp.label.lbl -text " [lindex $Lbl(Label) $GDefs(Lang)]"
            IcoMenu::Create $Data(Frame).def.r.disp.label.sel $GDefs(Dir)/share/bitmap \
               "zeroth.xbm label-1.xbm label4.xbm label3.xbm label2.xbm label1.xbm" "0 -1 6 4 2 1" \
               FSTD::Param(Label) "FSTD::ParamSet" 0 -relief groove -bd 2
            pack $Data(Frame).def.r.disp.label.sel $Data(Frame).def.r.disp.label.lbl -side left

         frame $Data(Frame).def.r.disp.vect
            label $Data(Frame).def.r.disp.vect.lbl -text " [lindex $Lbl(Vector) $GDefs(Lang)]"
            IcoMenu::Create $Data(Frame).def.r.disp.vect.sel $GDefs(Dir)/share/bitmap \
               "zeroth.xbm wind1.xbm wind2.xbm wind3.xbm wind4.xbm wind5.xbm" "NONE BARB SPEAR ARROW STREAMLINE STREAMLINE3D" FSTD::Param(Vector) \
               "FSTD::ParamSet" 0 -relief groove -bd 2
            pack $Data(Frame).def.r.disp.vect.sel $Data(Frame).def.r.disp.vect.lbl -side left
            $Data(Frame).def.r.disp.vect.sel.menu add separator
            $Data(Frame).def.r.disp.vect.sel.menu add command -label [lindex $Lbl(Params) $GDefs(Lang)] \
               -command "VectorBox::Create $Data(Frame).def.r.disp.vect \"FSTD::ParamSet ; FSTD::ParamUpdate; Page::Update \$Page::Data(Frame); Page::UpdateCommand \$Page::Data(Frame)\""

         frame $Data(Frame).def.r.disp.val
            label $Data(Frame).def.r.disp.val.lbl -text " [lindex $Lbl(Value) $GDefs(Lang)]"
            IcoMenu::Create $Data(Frame).def.r.disp.val.sel $GDefs(Dir)/share/bitmap \
               "zeroth.xbm valmm.xbm valhl.xbm valhls.xbm " "0 1 20 2" \
               FSTD::Param(Value) "FSTD::ParamSet" 0 -relief groove -bd 2
            $Data(Frame).def.r.disp.val.sel.menu add separator   
            $Data(Frame).def.r.disp.val.sel.menu add command -bitmap @$GDefs(Dir)/share/bitmap/font.ico\
                  -command "FontBox::Create $Data(Frame).def.r.disp.val.sel FSTD::ParamSet \$FSTD::Param(EFont); puts stderr \$FSTD::Param(EFont)" 
            pack $Data(Frame).def.r.disp.val.sel $Data(Frame).def.r.disp.val.lbl -side left

         frame $Data(Frame).def.r.disp.part
            label $Data(Frame).def.r.disp.part.lbl -text " [lindex $Lbl(Particle) $GDefs(Lang)]"
            IcoMenu::Create $Data(Frame).def.r.disp.part.sel $GDefs(Dir)/share/bitmap \
               "zeroth.xbm size1.xbm size2.xbm size3.xbm size4.xbm size5.xbm" "0 1 2 3 4 5" \
               FSTD::Param(Particle) "FSTD::ParamSet" 0 -relief groove -bd 2
            pack $Data(Frame).def.r.disp.part.sel $Data(Frame).def.r.disp.part.lbl -side left

         frame $Data(Frame).def.r.disp.mask
            label $Data(Frame).def.r.disp.mask.lbl -text " [lindex $Lbl(Mask) $GDefs(Lang)]"
            checkbutton $Data(Frame).def.r.disp.mask.sel -variable FSTD::Param(Mask) -relief raised -bd 1 \
               -bitmap @$GDefs(Dir)/share/bitmap/zeroth.xbm -indicatoron false \
               -command "FSTD::ParamSet" -selectcolor "" -relief groove -bd 1
            pack $Data(Frame).def.r.disp.mask.sel $Data(Frame).def.r.disp.mask.lbl -side left -ipadx 1
            
         pack $Data(Frame).def.r.disp.p $Data(Frame).def.r.disp.cont $Data(Frame).def.r.disp.tex $Data(Frame).def.r.disp.vol \
            $Data(Frame).def.r.disp.grid $Data(Frame).def.r.disp.vect $Data(Frame).def.r.disp.part $Data(Frame).def.r.disp.label \
            $Data(Frame).def.r.disp.val $Data(Frame).def.r.disp.mask -side top -anchor w -padx 2 -fill x -expand true

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
         checkbutton $Data(Frame).lev.desc.bellow -bitmap @$GDefs(Dir)/share/bitmap/MDec.xbm -variable FSTD::Param(MapBelow) -onvalue 1 -offvalue 0 \
               -relief sunken -bd 2 -overrelief raised -offrelief groove -command { FSTD::ParamSet } -indicatoron false -selectcolor "" -width 10
         checkbutton $Data(Frame).lev.desc.above -bitmap @$GDefs(Dir)/share/bitmap/MInc.xbm -variable FSTD::Param(MapAbove) -onvalue 1 -offvalue 0 \
               -relief sunken -bd 2 -overrelief raised -offrelief groove -command { FSTD::ParamSet } -indicatoron false -selectcolor ""  -width 10
         ComboBox::Create $Data(Frame).lev.desc.edit FSTD::Param(Intervals) editclose sorted nodouble -1 \
            "" 17 6 "FSTD::IntervalSetMode NONE 0 True"
         pack $Data(Frame).lev.desc.bellow -side left -fill y
         pack $Data(Frame).lev.desc.edit -side left -fill both -expand true
         pack $Data(Frame).lev.desc.above -side left  -fill y
      pack $Data(Frame).lev.desc -side top -fill x -padx 2 -pady 2 -expand true

   pack $Data(Frame).def   -side top -fill x -anchor n
   pack $Data(Frame).lev   -side top -fill x -anchor n -padx 5

   #----- Creation du menu de mode de niveaux

   menu $Data(Frame).lev.select.mode.list
      $Data(Frame).lev.select.mode.list add cascade -label [lindex $Param(IntervalModes) 1] \
         -menu $Data(Frame).lev.select.mode.list.inter
      $Data(Frame).lev.select.mode.list add separator
      $Data(Frame).lev.select.mode.list add cascade -label [lindex $Param(IntervalModes) 2] \
         -menu $Data(Frame).lev.select.mode.list.nb
      $Data(Frame).lev.select.mode.list add command -label [lindex $Param(IntervalModes) 3] \
         -command "FSTD::IntervalSetMode [lindex $Param(IntervalModes) 3] 1"
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

   bind $Data(Frame).lev.desc.edit.select <KeyRelease>       {+ FSTD::IntervalSetMode NONE 0 True }
   bind $Data(Frame).lev.desc.edit.select <<Paste>>          {+ FSTD::IntervalSetMode NONE 0 True }
   bind $Data(Frame).lev.desc.edit.select <<PasteSelection>> {+ FSTD::IntervalSetMode NONE 0 True }

   for { set l 0 } { $l<$Param(InterspecsNb) } { incr l } {
      set Param(Color$l) black
      set Param(Alpha$l) FF
      set Param(Dash$l) ""
      set Param(Width$l) 1
      set Param(Intervals$l) {}
      frame $Data(Frame).lev.lev$l
         entry $Data(Frame).lev.lev$l.lst -textvariable FSTD::Param(Intervals$l) -bd 1 -width 12 -bg $GDefs(ColorLight)
         IcoMenu::CreateDef $Data(Frame).lev.lev$l.st $GDefs(Dir)/share/bitmap \
            { dash0.xbm dash1.xbm dash2.xbm dash3.xbm dash4.xbm dash5.xbm } { "" . - .- .-- .-. } \
            FSTD::Param(Dash$l) "FSTD::ParamSet" 0 -relief groove -bd 2
         IcoMenu::Create $Data(Frame).lev.lev$l.width $GDefs(Dir)/share/bitmap \
            "width1.xbm width2.xbm width3.xbm width4.xbm width5.xbm" "1 2 3 4 5" \
            FSTD::Param(Width$l) "FSTD::ParamSet" 0 -relief groove -bd 2
         ColorBox::CreateSel $Data(Frame).lev.lev$l.col [list FSTD::Param(Color$l) FSTD::Param(Alpha$l)] FSTD::ParamSet
         pack $Data(Frame).lev.lev$l.lst -side left -fill x -expand true
         pack $Data(Frame).lev.lev$l.col $Data(Frame).lev.lev$l.width $Data(Frame).lev.lev$l.st -side left
      pack $Data(Frame).lev.lev$l -side top -fill x -padx 2
      Bubble::Create $Data(Frame).lev.lev$l            $Bubble(InterSpecs) 
   } 
           
   #------ Creation des bulles d'aide
   Bubble::Create $Data(Frame).lev.desc.edit        $Bubble(Intervals)
   Bubble::Create $Data(Frame).def.l.val.interp.sel $Bubble(Interp)
   Bubble::Create $Data(Frame).def.l.val.grid.sel   $Bubble(Grid)
   Bubble::Create $Data(Frame).def.l.val.grid.tile  $Bubble(Tile)

   Bubble::Create $Data(Frame).def.l.val.order.font $Bubble(Font)
   Bubble::Create $Data(Frame).def.l.val.order      $Bubble(Format)
   Bubble::Create $Data(Frame).def.l.val.unit       $Bubble(Unit)
   Bubble::Create $Data(Frame).def.l.val.desc       $Bubble(Desc)
   Bubble::Create $Data(Frame).def.l.pal.cmap       $Bubble(Map)
   Bubble::Create $Data(Frame).def.l.val.fac        $Bubble(Conv)
   Bubble::Create $Data(Frame).var.sel              $Bubble(NomVar)
   Bubble::Create $Data(Frame).lev.select.mode      $Bubble(Mode)
   Bubble::Create $Data(Frame).lev.select.number    $Bubble(Nb)
   Bubble::Create $Data(Frame).lev.edit             $Bubble(Intervals)
   Bubble::Create $Data(Frame).lev.desc.bellow      $Bubble(MapBelow)
   Bubble::Create $Data(Frame).lev.desc.above       $Bubble(MapAbove)

   Bubble::Create $Data(Frame).def.r.disp.p.map   $Bubble(MapAll) 
   Bubble::Create $Data(Frame).def.r.disp.p.col   $Bubble(Color)
   Bubble::Create $Data(Frame).def.r.disp.p.width $Bubble(Width) 
   Bubble::Create $Data(Frame).def.r.disp.p.st    $Bubble(Dash) 
   Bubble::Create $Data(Frame).def.r.disp.cont    $Bubble(Contour) 
   Bubble::Create $Data(Frame).def.r.disp.tex     $Bubble(Texture) 
   Bubble::Create $Data(Frame).def.r.disp.vol     $Bubble(Volume)  
   Bubble::Create $Data(Frame).def.r.disp.grid    $Bubble(Grid)  
   Bubble::Create $Data(Frame).def.r.disp.vect    $Bubble(Vector) 
   Bubble::Create $Data(Frame).def.r.disp.part    $Bubble(Particle)  
   Bubble::Create $Data(Frame).def.r.disp.label   $Bubble(Label) 
   Bubble::Create $Data(Frame).def.r.disp.val     $Bubble(Value)  
   Bubble::Create $Data(Frame).def.r.disp.mask    $Bubble(Mask)
}
            
#----------------------------------------------------------------------------
# Nom      : <FSTD::IntervalSetMode>
# Creation : Avril 2008 - J.P. Gauthier - CMC/CMOE
#
# But      : Instaurer des valeurs pour les intervalles selon les divers mode
#
# Parametres :
#   <Mode>        : Mode de definition des intervalles
#   <Par>         : Parametres du mode
#   <Interactive> : Appel interactif
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc FSTD::IntervalSetMode { Mode { Par 0 } { Interactive False } } {
   variable Param
   variable Data

   if { $Mode=="NONE" && !$Interactive } {
      set Param(Intervals)   {}
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

      switch $Param(IntervalMode) {
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

   if { ![fstdfield is $Field] && ![gribfield is $Field] } {
      if { [string is double $Val] } {
         return [format "%1.3e" $Val]
      } else {
         return "-"
      }
   } 
   
   set val      [fstdfield configure $Field -value]
   set order    [lindex $val 0]
   set mantisse [lindex $val 1]

   if { $mantisse==-1 } {
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

      if { [fstdfield is $field True] && [fstdfield configure $field -active] } {
         if { ![projection configure $Page -geographic] } {
            set ij    [projection function $Page -gridcoord $Lat $Lon]
            set pij   $ij
            set value [fstdfield stats $field -gridvalue [lindex $ij 0] [lindex $ij 1]]
         } else {
            set ij    [fstdfield stats $field -coordpoint $Lat $Lon]
            set pij   [fstdfield stats $field -unproject $Lat $Lon]
            set value [fstdfield stats $field -coordvalue $Lat $Lon]
         }
         set spd [lindex $value 0]
         set dir [lindex $value 1]

         if { $dir!="" } {
            catch { set value [FSTD::FieldFormat $field $spd]@[format "%1.2f" $dir] }
         } else {
            set value [FSTD::FieldFormat $field $spd]
         }
         set desc [fstdfield define $field -NOMVAR]

         if { [fstdfield configure $field -interpdegree]=="NEAREST" } {
            #----- Catch out of grid or masked values
            catch {
               lset ij 0  [expr round([lindex $ij 0])]
               lset ij 1  [expr round([lindex $ij 1])]
               lset pij 0 [expr round([lindex $pij 0])]
               lset pij 1 [expr round([lindex $pij 1])]
            }
         }
         lappend list [list $desc $ij $pij $value]
         append Page::Data(Value) "$desc:$value "
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
   
   set spec $Spec
   if { $spec=="" } {
      set spec $Param(Spec)
   }

   if { ![dataspec is $spec] } {
      return
   }

   set val  [dataspec configure $spec -value]
   set Param(Order)     [lindex $val 0]
   set Param(Mantisse)  [lindex $val 1]

   set mode [dataspec configure $spec -intervalmode]
   set Param(IntervalMode)  [lindex $mode 0]
   set Param(IntervalParam) [lindex $mode 1]

   set Param(Desc)       [dataspec configure $spec -desc]
   set Param(Unit)       [dataspec configure $spec -unit]
   set Param(Factor)     [dataspec configure $spec -factor]
   set Param(Delta)      [dataspec configure $spec -delta]
   set Param(Font)       [dataspec configure $spec -font]
   set Param(EFont)      [dataspec configure $spec -efont]
   set Param(Map)        [dataspec configure $spec -colormap]
   set Param(MapAll)     [dataspec configure $spec -mapall]
   set Param(MapAbove)   [dataspec configure $spec -mapabove]
   set Param(MapBelow)   [dataspec configure $spec -mapbelow]
   set Param(Size)       [dataspec configure $spec -size]
   set Param(SizeRange)  [dataspec configure $spec -sizerange]
   set Param(Color)      [dataspec configure $spec -color]
   set Param(Alpha)      [dataspec configure $spec -transparency]
   set Param(Dash)       [dataspec configure $spec -dash]
   set Param(Width)      [dataspec configure $spec -width]
   set Param(Contour)    [dataspec configure $spec -rendercontour]
   set Param(Texture)    [dataspec configure $spec -rendertexture]
   set Param(Volume)     [dataspec configure $spec -rendervolume]
   set Param(Coord)      [dataspec configure $spec -rendercoord]
   set Param(Value)      [dataspec configure $spec -rendervalue]
   set Param(Label)      [dataspec configure $spec -renderlabel]
   set Param(Grid)       [dataspec configure $spec -rendergrid]
   set Param(Boundary)   [dataspec configure $spec -renderboundary]
   set Param(Particle)   [dataspec configure $spec -renderparticle]
   set Param(Intervals)  [dataspec configure $spec -intervals]
   set Param(Interp)     [dataspec configure $spec -interpdegree]
   set Param(Extrap)     [dataspec configure $spec -extrapdegree]
   set Param(Topo)       [dataspec configure $spec -topography]
   set Param(TopoFac)    [dataspec configure $spec -topographyfactor]
   set Param(Sample)     [dataspec configure $spec -sample]
   set Param(SampleType) [dataspec configure $spec -sampletype]
   set Param(Step)       [dataspec configure $spec -step]
   set Param(Min)        [dataspec configure $spec -min]
   set Param(Max)        [dataspec configure $spec -max]
   set Param(GridVec)    [dataspec configure $spec -gridvector]
   set Param(Vector)     [dataspec configure $spec -rendervector]
   set Param(Axis)       [dataspec configure $spec -axis]
   set Param(Mask)       [dataspec configure $spec -mask]
   set Param(Interspecs) [dataspec configure $spec -interspecs]

   if { $Param(Boundary) } {
      set Param(Grid) 6
   }
   
   set plane             [dataspec configure $spec -cube]
   set Param(X0)         [lindex $plane 0]
   set Param(Y0)         [lindex $plane 1]
   set Param(Z0)         [lindex $plane 2]
   set Param(X1)         [lindex $plane 3]
   set Param(Y1)         [lindex $plane 4]
   set Param(Z1)         [lindex $plane 5]

   set Param(Alpha)      [format %02x [expr int($Param(Alpha)/100.0*255.0)]]

   #----- Intervals and min-max selection are exclusive with priority to intervals
   if { (![llength $Param(Intervals)] || $Param(IntervalMode)!="") && $Param(Min)!=$Param(Max) } {
      set Param(Intervals) ""
      if { $Param(Min)!="" } {
         append Param(Intervals) "\[$Param(Min)"
      }
      if { $Param(Max)!="" } {
         append Param(Intervals) " $Param(Max)\]"
      }
   }

   if { [llength [set interlabels [dataspec configure $spec -interlabels]]] } {
      set inters $Param(Intervals)
      set Param(Intervals) ""
      foreach label $interlabels inter $inters {
         append Param(Intervals) "[string trim ${inter}($label)] "
      }
   }

   for { set i 0 }  { $i<$Param(InterspecsNb) } { incr i } {   
      if { [llength [lindex $Param(Interspecs) $i 0]] } {
         lassign [lindex $Param(Interspecs) $i] Param(Intervals$i) Param(Color$i) Param(Width$i) Param(Dash$i)
      } else {
         lassign { {} "" "" "" } Param(Intervals$i) Param(Color$i) Param(Width$i) Param(Dash$i)
      }         
   }

   FSTD::IntervalSetMode $Param(IntervalMode) $Param(IntervalParam) True
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
   
   if { $Param(Intervals)=="" } {
      set min ""
      set max ""
   } else {
      set min   [dataspec configure $Spec -min]
      set max   [dataspec configure $Spec -max]
   }

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

   set Param(GridNo) [lsearch -exact $Param(GridIds) $Param(GridId)]
   set alpha [expr int(0x$Param(Alpha)/255.0*100.0)]

   if { [glrender -direct] && ($Param(Vector)=="STREAMLINE" || ($Param(Vector)=="STREAMLINE3D" && !$Param(MapAll))) } {
      glrender -delay [set OpenGL::Param(Delay) 10]
   } else {
      glrender -delay [set OpenGL::Param(Delay) 1000]
   }
   
   if { [string is int $Param(Mantisse)] } {
      set mantisse $Param(Mantisse)
   } else {
      set mantisse -1   
   }
      
   if { $Param(Grid)==6 } {
      set Param(Boundary) 1
      set grid 0
   } else {
      set Param(Boundary) 0
      set grid $Param(Grid)
   }
   
   #----- interspecs
   set Param(InterSpecs) {}
   for { set i 0 }  { $i<$Param(InterspecsNb) } { incr i } {   
      if { [llength $Param(Intervals$i)] } {
         lappend Param(InterSpecs) [list $Param(Intervals$i) $Param(Color$i) $Param(Width$i) $Param(Dash$i)]
      }
   }

   #----- Set all params
   dataspec configure $Spec -set 2 -factor $Param(Factor) -delta $Param(Delta) -value $Param(Order) $mantisse -font $Param(Font) -efont $Param(EFont) -colormap $Param(Map) \
      -color $Param(Color) -dash $Param(Dash) -width $Param(Width) -unit $Param(Unit) -desc $Param(Desc) -rendercontour $Param(Contour) \
      -rendervector $Param(Vector) -rendertexture $Param(Texture) -rendervolume $Param(Volume)  -rendervalue $Param(Value) -renderlabel $Param(Label) \
      -renderparticle $Param(Particle) -rendergrid $grid -renderboundary $Param(Boundary) -interpdegree $Param(Interp) -extrapdegree $Param(Extrap) -topography $Param(Topo) \
      -topographyfactor $Param(TopoFac) -sample $Param(Sample) -sampletype $Param(SampleType) -step $Param(Step) -gridvector $Param(GridVec) \
      -cube [list $Param(X0) $Param(Y0) $Param(Z0) $Param(X1) $Param(Y1) $Param(Z1)] -axis $Param(Axis) -size $Param(Size) -sizerange $Param(SizeRange) \
      -transparency $alpha  -min $min -max $max -mapall $Param(MapAll) -mapabove $Param(MapAbove) -mapbelow $Param(MapBelow) -mask $Param(Mask) -interspecs $Param(InterSpecs)

   #----- Set intervals depending on interval mode
   if  { $Param(IntervalMode)=="INTERVAL" || $Param(IntervalMode)=="LINEAR" || $Param(IntervalMode)=="LOGARITHMIC" || $Param(IntervalMode)=="RSMC" } {
      dataspec configure $Spec -intervalmode $Param(IntervalMode) $Param(IntervalParam)
      set Param(Intervals) [dataspec configure $Spec -intervals]
   } else {
      dataspec configure $Spec -intervals $inter -interlabels $label -intervalmode $Param(IntervalMode) $Param(IntervalParam)
   }

   catch { $Data(ApplyButton) configure -state normal }

#   MetStat::RECRCAdd [dataspec configure $Spec -desc] "" $Param(Unit) $Param(Factor) "" ""
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

proc FSTD::ParamPut { { Update False } } {
   global GDefs
   variable Data
   variable Param

   IcoMenu::Set $Data(Frame).def.r.disp.cont.sel   $Param(Contour)
   IcoMenu::Set $Data(Frame).def.r.disp.vect.sel   $Param(Vector)
   IcoMenu::Set $Data(Frame).def.r.disp.grid.sel   $Param(Grid)
   IcoMenu::Set $Data(Frame).def.r.disp.vect.sel   $Param(Vector)
   IcoMenu::Set $Data(Frame).def.r.disp.part.sel   $Param(Particle)
   IcoMenu::Set $Data(Frame).def.r.disp.vol.sel    $Param(Volume)

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

   #----- Intervals will be reset due to widget bindings
   set inters $Param(Intervals)

   #----- Recuperer les niveaux du .recrc
   ComboBox::DelAll $Data(Frame).lev.desc.edit
   $Data(Frame).lev.select.mode.list.inter delete 0 end

   if { [dataspec is $Param(Spec)] } {
      set var $Param(Spec)

      if { [info exist ::MetStat::Rec(Level$var)] } {
         #----- Add intervals list to interface
         ComboBox::AddList $Data(Frame).lev.desc.edit $MetStat::Rec(Level$var)

         foreach inter $MetStat::Rec(Inter$var) {
            $Data(Frame).lev.select.mode.list.inter add command -label "$inter" \
               -command "FSTD::IntervalSetMode INTERVAL $inter"
         }
      } else {
         #----- Add default interval list to interface
         ComboBox::AddList $Data(Frame).lev.desc.edit $MetStat::Rec(Level)

         foreach inter $MetStat::Rec(Inter) {
            $Data(Frame).lev.select.mode.list.inter add command -label "$inter" \
               -command "FSTD::IntervalSetMode INTERVAL $inter"
         }
      }
      if {  $Update && [fstddict isvar $var] } {
         if { $Update } {
            set Param(Unit)   [fstddict varinfo $var -lang $GDefs(Lang) -units]
            set Param(Factor) [fstddict varinfo $var -lang $GDefs(Lang) -factor]
            set Param(Delta)  [fstddict varinfo $var -lang $GDefs(Lang) -delta]
            set Param(Desc)   [fstddict varinfo $var -lang $GDefs(Lang) -short]
         }
      }
   }
   #----- Set intervals to right values
   set Param(Intervals) $inters

   #----- Set interspecs params
   for { set l 0 } { $l<$Param(InterspecsNb) } { incr l } {
      if { [llength $Param(Intervals$l)] } {
         IcoMenu::Set $Data(Frame).lev.lev$l.st      [expr {[string length $Param(Dash$l)]?$Param(Dash$l):$Param(Dash)} ]  
         IcoMenu::Set $Data(Frame).lev.lev$l.width   [expr {[string length $Param(Width$l)]?$Param(Width$l):$Param(Width)}] 
         ColorBox::ConfigNoColor $Data(Frame).lev.lev$l.col [expr {[string length $Param(Color$l)]?$Param(Color$l):$Param(Color)}]
      } else {
         IcoMenu::Set $Data(Frame).lev.lev$l.st      ""
         IcoMenu::Set $Data(Frame).lev.lev$l.width   1
         ColorBox::ConfigNoColor $Data(Frame).lev.lev$l.col black
      
      }
   }
   MapBox::Select "" $Param(Map)
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
   global GDefs
   variable Param

   if { [dataspec is $Spec] } {
      set var  [fstdfield define $Field -NOMVAR]
#      set etik [fstdfield define $Field -ETIKET]
      set ip1  -1

      if { [fstdfield configure $Field -set]==0 } {
         #----- Copy default configuration but keep unit and description
         set unit  [fstdfield configure $Field -unit]
         set desc  [fstdfield configure $Field -desc]
         set inter [fstdfield configure $Field -intervals]
         dataspec copy $Spec FLDDEFAULT
         dataspec configure $Spec -unit $unit
         dataspec configure $Spec -desc $desc

          if  { $Param(AutoREC) && ![llength $inter] } {
             if { [info exist ::MetStat::Rec(Inter$Spec)] && [llength $MetStat::Rec(Inter$Spec)] } {
                dataspec configure $Spec -intervalmode INTERVAL [lindex $MetStat::Rec(Inter$Spec) 0]
             } elseif { [info exist ::MetStat::Rec(Level$Spec)] && [llength $MetStat::Rec(Level$Spec)] } {
                dataspec configure $Spec -intervals [lindex $MetStat::Rec(Level$Spec) 0]
             }
          }
      }
      
      #----- Set a colormap    
      if { ![colormap is FLDMAP$Spec] } {
         #----- Use previous on if not already defined
         set map [dataspec configure $Spec -colormap]
         if { ![colormap is $map] } {
            colormap copy FLDMAP$Spec FLDMAPDEFAULT
         } else {
            colormap copy FLDMAP$Spec $map
         }
      }
      dataspec configure $Spec -colormap FLDMAP$Spec
      
      #----- Override particle fields params
      if { [fstdfield is $Field] && ([fstdfield define $Field -GRTYP]=="Y" || [fstdfield define $Field -NOMVAR]=="ZH") } {
         dataspec configure $Spec -renderparticle 2
      }

      #----- Override vectorial fields params
      if { [fstdfield stats $Field -component]>1 } {
         dataspec configure $Spec -rendervector BARB -rendertexture 0
      }

# This fails because convip exit if a not covered type in old format is passed
#      if { [fstddict varinfo $var -ip1]!="" } {
#         set ip1 [fstdfield define $Field -IP1]
#      }

#      if { ![fstddict isvar $var] } {
#         fstddict varinfo $var -lang $GDefs(Lang) -searchip1 $ip1 -short $desc
#      }

#      if { [llength [set info [fstddict varinfo $var -lang $GDefs(Lang) -searchip1 $ip1 -short -units -factor -delta]]] } {
#         dataspec configure $Spec -desc [lindex $info 0] -unit [lindex $info 1] -factor [lindex $info 2] -delta [lindex $info 3]
#      }
      if { [fstdfield is $Field] && [lindex [set info [fstddict varinfo $var -lang $GDefs(Lang) -short -units -factor -delta]] 0]!="" } {
         dataspec configure $Spec -desc [lindex $info 0] -unit [lindex $info 1] -factor [lindex $info 2] -delta [lindex $info 3]
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

   set paths $env(HOME)/.spi
   if { [info exists env(SPI_TOOL)] } {
      set paths [concat [split $env(SPI_TOOL) :] $paths]
   }

   foreach path $paths {
      if { [file exists $path/Colormap/$Map.rgba] } {
         colormap read FLDMAP$Id $path/Colormap/$Map.rgba
         break
      }
   }
   eval dataspec configure $Id $args -font FLDFONTDEFAULT -colormap FLDMAP$Id
}

#----------------------------------------------------------------------------
# Nom      : <FSTD::ParamOwnership>
# Creation : Novembre 2014 - J.P. Gauthier - CMC/CMOE
#
# But      : Parse pre-configure fields (macro/script) and get ownership of them
#
# Parametres :
#   <Fields> : List of fields (Empty will use all known)
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc FSTD::ParamOwnership { { Fields { } } } {
   variable Data

   if { ![llength $Fields] } {
      set Fields [concat $Data(List) $Data(ListTool)]
   }

   set n 0

   #----- Parse all fields
   foreach fld $Fields {
      if {[fstdfield is $fld]} {
		#----- If they were configured outside SPI
	if { [fstdfield configure $fld -set]==1 } {
	  incr n

	  #----- Get ownership
	  fstdfield configure $fld -set 2
	}
      }
   }

   if { $n } {
      FSTD::ParamUpdate
   }
}

#----------------------------------------------------------------------------
# Nom      : <FSTD::ParamGetMode>
# Creation : Novemnre 2015 - J.P. Gauthier - CMC/CMOE
#
# But      : Recuperer l'identificateur des parametres selon le mode
#
# Parametres :
#   <Field>  : Champs dont on recupere l'identificateur
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc FSTD::ParamGetMode { Field } {
   variable Param

   switch $Param(Mode) {
      "FLD"    { set var $Field }
      "VAR"    { set var [fstdfield define $Field -NOMVAR] }
      "TYPVAR" { set var [fstdfield define $Field -TYPVAR] }
      "LEVEL"  { set var [fstdfield stats  $Field -level] }
      "IP1"    { set var [fstdfield define $Field -IP1] }
      "IP2"    { set var [fstdfield define $Field -IP2] }
      "IP3"    { set var [fstdfield define $Field -IP3] }
      "ETIKET" { set var [fstdfield define $Field -ETIKET] }
      "DATEO"  { set var [fstdfield define $Field -DATEO] }
      "FILE"   { set var [fstdfield define $Field -FID] }
   }
   
   return $var
}

#----------------------------------------------------------------------------
# Nom      : <FSTD::ParamSetDefault>
# Creation : Novemnre 2015 - J.P. Gauthier - CMC/CMOE
#
# But      : Definir les parametres par defaut pour un champ
#
# Parametres :
#   <Field>  : Champ
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc FSTD::ParamSetDefault { Field args } {
   variable Data
   
   set var [FSTD::ParamGetMode $Field]

   eval dataspec configure $var $args
   FSTD::ParamUpdate $Field
   FSTD::ParamGet $var
   FSTD::ParamPut False
}

#----------------------------------------------------------------------------
# Nom      : <FSTD::ParamUpdate>
# Creation : Mai 2006 - J.P. Gauthier - CMC/CMOE
#
# But      : Mettre a jour la liste des observations selectionnees
#
# Parametres :
#   <Fields> : List of fields (Empty will use all known)
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc FSTD::ParamUpdate { { Fields { } } } {
   variable Param
   variable Data

   set current $Param(Spec)
   set exist 0
   set var   ""

   if { ![llength $Fields] } {
      set Fields [concat $Data(List) $Data(ListTool)]
      ComboBox::DelAll $Data(Frame).var.sel
   } 

   foreach fld $Fields {

      if { [fstdfield is $fld True] } {

         set set [fstdfield configure $fld -set]
         catch { fstdfield define $fld -grid $Param(GridNo) }

         #----- Get configuration mode
         set var [FSTD::ParamGetMode $fld]

         #----- Do not process animator fields
         if { [string match ANI.* $var] } {
            continue
         }
         
         #----- If the var is empty
         if { [set var [string trim $var]]=="" } {
            set var "<>"
         }

         #----- Define default config
         set spec [fstdfield configure $fld -dataspec]
         if { ![dataspec is $var] } {
            if { $spec!=$var } {
               dataspec copy $var $spec

               if { $set==0 } {
                  dataspec configure $var -colormap ""
               }
               
               #----- If field has not been configured yet or is owned by SPI
               if { $set==0 || $set==2 } {
                  FSTD::ParamInit $fld $var
               }
            }
         }

         #----- If field has not been configured yet or is owned by SPI
         if { $set==0 || $set==2 } {
            dataspec copy $spec $var
         }
         
         #----- Release spec
         dataspec free $spec

         if { "$var"=="$current" } {
             set Param(Spec) $current
             set exist 1
         }
         ComboBox::Add $Data(Frame).var.sel $var
      }
   }

   if { !$exist && $var!="" } {
      set Param(Spec) $var
      FSTD::ParamGet
      FSTD::ParamPut True
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
#         FLD   : Parametres par champs
#         VAR   : Parametres par NOMVAR
#         TYPVAR: Parametres par TYPVAR
#         LEVEL : Parametres par niveaux
#         IP1   : Parametres par IP1
#         IP2   : Parametres par IP2
#         IP3   : Parametres par IP3
#         ETIKET: Parametres par ETIKET
#         DATEO : Parametres par DATEO
#         FILE  : Parametres par FILE
#
#----------------------------------------------------------------------------

proc FSTD::VarMode { Mode } {
   variable Param

   set Param(Mode) $Mode
   FSTD::ParamUpdate
}
