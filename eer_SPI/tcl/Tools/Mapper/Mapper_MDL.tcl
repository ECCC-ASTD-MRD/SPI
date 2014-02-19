#===============================================================================
# Environnement Canada
# Centre Meteorologique Canadian
# 2100 Trans-Canadienne
# Dorval, Quebec
#
# Projet   : Fonctions de manipulation des donnees geographiques.
# Fichier  : Mapper_MDL.tcl
# Creation : Janvier 2014 - J.P.Gauthier - CMC/CMOE
#
# Description:
#    Fontions de manipulation de donnees modele 3D.
#
# Remarques :
#
#===============================================================================

namespace eval Mapper::MDL { } {
   variable Data
   
   set Data(Color)       black
   set Data(Width)       0
   set Data(Dash)        ""
   set Data(Light)       0
}

#-------------------------------------------------------------------------------
# Nom      : <Mapper::MDL::Params>
# Creation : Juillet 2004 - J.P. Gauthier - CMC/CMOE
#
# But      : Parametres des objets de type Model
#
# Parametres :
#   <Object> : Donnee geographique a parametrer
#   <Tab>    : Onglet par defaut
#
# Retour    :
#
# Remarque :
#
#-------------------------------------------------------------------------------

proc Mapper::MDL::Params { Object { Tabs {} } } {
   global   GDefs
   variable Data

   if { [winfo exists .mapperparams] && $Mapper::Data(Mode)!="model" } {
      set pos [wm geometry .mapperparams]
      Mapper::ParamsClose
      destroy .mapperparams
   } else {
      set pos 500x425
   }
   set Mapper::Data(Mode) model

   if { ![winfo exists .mapperparams] } {
      toplevel .mapperparams

      wm title     .mapperparams "[lindex $Mapper::Lbl(Model) $GDefs(Lang)]"
      wm transient .mapperparams .
      wm geometry  .mapperparams =$pos
      wm protocol  .mapperparams WM_DELETE_WINDOW { destroy .mapperparams }

      TabFrame::Create .mapperparams.tab 1 ""
      pack .mapperparams.tab -side top -fill both -expand true -padx 2 -pady 2

      frame .mapperparams.com
         button .mapperparams.com.apply  -text [lindex $Mapper::Lbl(Apply) $GDefs(Lang)] -bd 1 -command { Mapper::MDL::ParamsSet $Mapper::Data(Object) }
         button .mapperparams.com.cancel -text [lindex $Mapper::Lbl(Close) $GDefs(Lang)] -bd 1 -command { destroy .mapperparams }
         pack .mapperparams.com.cancel .mapperparams.com.apply -side right
      pack .mapperparams.com -side top -fill x -padx 2 -pady 2

      set Data(Frame2) [TabFrame::Add .mapperparams.tab 1 [lindex $Mapper::Lbl(Display) $GDefs(Lang)] False ""]
         labelframe $Data(Frame2).mat -text [lindex $Mapper::Lbl(Material) $GDefs(Lang)]
            frame $Data(Frame2).mat.ambi
               label $Data(Frame2).mat.ambi.lbl -text [lindex $Mapper::Lbl(Ambi) $GDefs(Lang)] -width 12 -anchor w
               ColorBox::CreateSel $Data(Frame2).mat.ambi.col Mapper::Data(Ambi) Mapper::MDL::ParamsSet \$Mapper::Data(Object)
               pack $Data(Frame2).mat.ambi.lbl $Data(Frame2).mat.ambi.col -side left
            frame $Data(Frame2).mat.emis
               label $Data(Frame2).mat.emis.lbl -text [lindex $Mapper::Lbl(Emis) $GDefs(Lang)] -width 12 -anchor w
               ColorBox::CreateSel $Data(Frame2).mat.emis.col Mapper::Data(Emis) Mapper::MDL::ParamsSet \$Mapper::Data(Object)
               pack $Data(Frame2).mat.emis.lbl $Data(Frame2).mat.emis.col -side left
            frame $Data(Frame2).mat.diff
               label $Data(Frame2).mat.diff.lbl -text [lindex $Mapper::Lbl(Diff) $GDefs(Lang)] -width 12 -anchor w
               ColorBox::CreateSel $Data(Frame2).mat.diff.col Mapper::Data(Diff) Mapper::MDL::ParamsSet \$Mapper::Data(Object)
               pack $Data(Frame2).mat.diff.lbl $Data(Frame2).mat.diff.col -side left
            frame $Data(Frame2).mat.spec
               label $Data(Frame2).mat.spec.lbl -text [lindex $Mapper::Lbl(Spec) $GDefs(Lang)] -width 12 -anchor w
               ColorBox::CreateSel $Data(Frame2).mat.spec.col Mapper::Data(Spec) Mapper::MDL::ParamsSet \$Mapper::Data(Object)
               pack $Data(Frame2).mat.spec.lbl $Data(Frame2).mat.spec.col -side left
            frame $Data(Frame2).mat.shin
               label $Data(Frame2).mat.shin.lbl -text [lindex $Mapper::Lbl(Shin) $GDefs(Lang)] -width 12 -anchor w
               entry $Data(Frame2).mat.shin.val -bd 1 -textvariable Mapper::Data(Shin) -bg $GDefs(ColorLight) -width 5
               scale $Data(Frame2).mat.shin.sc -bd 1 -relief flat -width 15 -sliderlength 10 -resolution 0.1 -from 0 -to 1 -variable Mapper::Data(Shin) -orient horizontal \
                  -showvalue False -command { if { $Mapper::Data(RealTime) } { Mapper::MDL::ParamsSet $Mapper::Data(Object) }; catch }
               pack $Data(Frame2).mat.shin.lbl $Data(Frame2).mat.shin.val -side left
               pack $Data(Frame2).mat.shin.sc -side left -fill x -expand True
            frame $Data(Frame2).mat.tran
               label $Data(Frame2).mat.tran.lbl -text [lindex $Mapper::Lbl(Tran) $GDefs(Lang)] -width 12 -anchor w
               entry $Data(Frame2).mat.tran.val -bd 1 -textvariable Mapper::Data(Tram) -bg $GDefs(ColorLight) -width 5
               scale $Data(Frame2).mat.tran.sc -bd 1 -relief flat -width 15 -sliderlength 10 -resolution 0.1 -from 0 -to 1 -variable Mapper::Data(Tram) -orient horizontal \
                  -showvalue False -command { if { $Mapper::Data(RealTime) } { Mapper::MDL::ParamsSet $Mapper::Data(Object) }; catch }
               pack $Data(Frame2).mat.tran.lbl $Data(Frame2).mat.tran.val -side left
               pack $Data(Frame2).mat.tran.sc -side left -fill x -expand True
            pack $Data(Frame2).mat.ambi $Data(Frame2).mat.emis $Data(Frame2).mat.diff $Data(Frame2).mat.spec $Data(Frame2).mat.shin $Data(Frame2).mat.tran \
               -side top -padx 5 -fill x -anchor w

         labelframe $Data(Frame2).mode -text [lindex $Mapper::Lbl(Mode) $GDefs(Lang)]
            frame $Data(Frame2).mode.out
               label $Data(Frame2).mode.out.lbl -text [lindex $Mapper::Lbl(Out) $GDefs(Lang)] -width 12 -anchor w
               ColorBox::CreateSel $Data(Frame2).mode.out.col Mapper::Data(Color) Mapper::MDL::ParamsSet \$Mapper::Data(Object)
               IcoMenu::Create $Data(Frame2).mode.out.width $GDefs(Dir)/share/bitmap \
                  "zeroth.xbm width1.xbm width2.xbm width3.xbm width4.xbm width5.xbm" "0 1 2 3 4 5" \
                  Mapper::Data(Width) { Mapper::MDL::ParamsSet $Mapper::Data(Object) } $Mapper::Data(Width) -relief groove -bd 2
               IcoMenu::CreateDef $Data(Frame2).mode.out.dash $GDefs(Dir)/share/bitmap \
                { dash0.xbm dash1.xbm dash2.xbm dash3.xbm dash4.xbm dash5.xbm } { "" . - .- .-- .-. } \
                Mapper::Data(Dash) { Mapper::MDL::ParamsSet $Mapper::Data(Object) } $Mapper::Data(Dash) -relief groove -bd 2
               pack $Data(Frame2).mode.out.lbl $Data(Frame2).mode.out.col $Data(Frame2).mode.out.width $Data(Frame2).mode.out.dash -side left -anchor w
            frame $Data(Frame2).mode.text
               label $Data(Frame2).mode.text.lbl -text [lindex $Mapper::Lbl(Texture) $GDefs(Lang)] -width 12 -anchor w
               checkbutton $Data(Frame2).mode.text.val -variable Mapper::Data(Texture) -relief raised -bd 1 -onvalue 1 -offvalue 0  -selectcolor "" \
                  -bitmap @$GDefs(Dir)/share/bitmap/zeroth.xbm -indicatoron false -command { Mapper::MDL::ParamsSet $Mapper::Data(Object) }
               pack $Data(Frame2).mode.text.lbl $Data(Frame2).mode.text.val -side left
            frame $Data(Frame2).mode.light
               label $Data(Frame2).mode.light.lbl -text [lindex $Mapper::Lbl(Light) $GDefs(Lang)] -width 12 -anchor w
               checkbutton $Data(Frame2).mode.light.val -variable Mapper::Data(Light) -relief raised -bd 1 -onvalue 1 -offvalue 0  -selectcolor "" \
                  -bitmap @$GDefs(Dir)/share/bitmap/zeroth.xbm -indicatoron false -command { Mapper::MDL::ParamsSet $Mapper::Data(Object) }
               pack $Data(Frame2).mode.light.lbl $Data(Frame2).mode.light.val -side left
            pack $Data(Frame2).mode.out $Data(Frame2).mode.text $Data(Frame2).mode.light -side top -anchor w -padx 5
         pack $Data(Frame2).mat $Data(Frame2).mode -side top -fill x -padx 5 -pady 5 -ipady 2

      set Data(Frame1) [TabFrame::Add .mapperparams.tab 1 [lindex $Mapper::Lbl(Ref) $GDefs(Lang)] False ""]
         frame $Data(Frame1).projhead
            label $Data(Frame1).projhead.lbl -text [lindex $Mapper::Lbl(Projection) $GDefs(Lang)] -width 10 -anchor nw
            button $Data(Frame1).projhead.file -image OPEN -relief flat -bd 0 -overrelief raised \
               -command "Mapper::ProjFile $Data(Frame1).proj.val \[FileBox::Create . \"\" Load \[list \$FileBox::Type(PROJ) \$FileBox::Type(TXT)\]\]"
            button $Data(Frame1).projhead.tab -image INTEROGATE -relief flat -bd 0 -overrelief raised \
               -command { set Mapper::Data(Proj) [Mapper::WKT::Param $Mapper::Data(Proj)]; $Mapper::Data(Frame1).proj.val delete 0.0 end ; $Mapper::Data(Frame1).proj.val insert end $Mapper::Data(Proj); Mapper::MDL::ParamsSet $Mapper::Data(Object) }
            pack $Data(Frame1).projhead.lbl -side left
            pack $Data(Frame1).projhead.tab $Data(Frame1).projhead.file -side left -padx 2
         labelframe $Data(Frame1).proj -labelwidget $Data(Frame1).projhead
            text $Data(Frame1).proj.val -bd 1 -bg $GDefs(ColorLight) -height 5 -width 25
            pack $Data(Frame1).proj.val -side right -fill both -expand true -padx 5 -pady 5
         pack  $Data(Frame1).proj -side top -fill both -expand true -padx 5 -pady 5

         Bubble::Create $Data(Frame1).projhead.file $Mapper::Bubble(ProjLoad)
         Bubble::Create $Data(Frame1).projhead.tab  $Mapper::Bubble(ProjRef)

         labelframe $Data(Frame1).matrix -text [lindex $Mapper::Lbl(Matrix) $GDefs(Lang)]
            frame $Data(Frame1).matrix.loc
               label $Data(Frame1).matrix.loc.lbl -text [lindex $Mapper::Lbl(Translate) $GDefs(Lang)] -width 14 -anchor w
               entry $Data(Frame1).matrix.loc.lat -bd 1 -textvariable Mapper::Data(Lat) -bg $GDefs(ColorLight) -width 8
               entry $Data(Frame1).matrix.loc.lon -bd 1 -textvariable Mapper::Data(Lon) -bg $GDefs(ColorLight) -width 8
               entry $Data(Frame1).matrix.loc.ele -bd 1 -textvariable Mapper::Data(Ele) -bg $GDefs(ColorLight) -width 8
               pack $Data(Frame1).matrix.loc.lbl -side left
               pack $Data(Frame1).matrix.loc.lat $Data(Frame1).matrix.loc.lon $Data(Frame1).matrix.loc.ele -side left -fill x -expand True
            frame $Data(Frame1).matrix.rot
               label $Data(Frame1).matrix.rot.lbl -text [lindex $Mapper::Lbl(Rotation) $GDefs(Lang)] -width 14 -anchor w
               entry $Data(Frame1).matrix.rot.x -bd 1 -textvariable Mapper::Data(RX) -bg $GDefs(ColorLight) -width 8
               entry $Data(Frame1).matrix.rot.y -bd 1 -textvariable Mapper::Data(RY) -bg $GDefs(ColorLight) -width 8
               entry $Data(Frame1).matrix.rot.z -bd 1 -textvariable Mapper::Data(RZ) -bg $GDefs(ColorLight) -width 8
               pack $Data(Frame1).matrix.rot.lbl -side left
               pack $Data(Frame1).matrix.rot.x $Data(Frame1).matrix.rot.y $Data(Frame1).matrix.rot.z -side left -fill x -expand True
            frame $Data(Frame1).matrix.sca
               label $Data(Frame1).matrix.sca.lbl -text [lindex $Mapper::Lbl(Scale) $GDefs(Lang)] -width 14 -anchor w
               entry $Data(Frame1).matrix.sca.x -bd 1 -textvariable Mapper::Data(SX) -bg $GDefs(ColorLight) -width 8
               entry $Data(Frame1).matrix.sca.y -bd 1 -textvariable Mapper::Data(SY) -bg $GDefs(ColorLight) -width 8
               entry $Data(Frame1).matrix.sca.z -bd 1 -textvariable Mapper::Data(SZ) -bg $GDefs(ColorLight) -width 8
               pack $Data(Frame1).matrix.sca.lbl -side left
               pack $Data(Frame1).matrix.sca.x $Data(Frame1).matrix.sca.y $Data(Frame1).matrix.sca.z -side left -fill x -expand True
            pack $Data(Frame1).matrix.loc $Data(Frame1).matrix.rot $Data(Frame1).matrix.sca -side top -fill x -padx 5
         pack $Data(Frame1).matrix -side top -fill x -padx 5 -pady 5 -ipady 2 -anchor nw

      if { ![llength $Tabs] } {
         set Tabs 0
      }
   }

   if { [llength $Tabs] && [lsearch -exact $Tabs [TabFrame::Current .mapperparams.tab]]==-1 } {
      TabFrame::Select .mapperparams.tab [lindex $Tabs 0]
   }

   $Data(Frame1).proj.val delete 0.0 end
   $Data(Frame1).proj.val insert end $Data(Proj)

   ColorBox::ConfigNoColor $Data(Frame2).mat.ambi.col $Data(Ambi)
   ColorBox::ConfigNoColor $Data(Frame2).mat.emis.col $Data(Emis)
   ColorBox::ConfigNoColor $Data(Frame2).mat.diff.col $Data(Diff)
   ColorBox::ConfigNoColor $Data(Frame2).mat.spec.col $Data(Spec)
}

#-------------------------------------------------------------------------------
# Nom      : <Mapper::MDL::ParamsGet>
# Creation : Juillet 2004 - J.P. Gauthier - CMC/CMOE
#
# But      : Recuperer les parametres du modele
#
# Parametres :
#   <Object> : Donnee geographique a parametrer
#
# Retour    :
#
# Remarque :
#
#-------------------------------------------------------------------------------

proc Mapper::MDL::ParamsGet { Object } {
   variable Data

   set Data(Proj)    [model define $Object -projection]
   set Data(Width)   [model configure $Object -width]
   set Data(Color)   [model configure $Object -outline]
   set Data(Dash)    [model configure $Object -dash]
   set Data(Texture) [model configure $Object -rendertexture]
   set Data(Light)   [model configure $Object -light]

   set tmp [model matrix $Object -locate]
   set Data(Lat) [lindex $tmp 0]
   set Data(Lon) [lindex $tmp 1]
   set Data(Ele) [lindex $tmp 2]

   set tmp [model matrix $Object -rotate]
   set Data(RX) [lindex $tmp 0]
   set Data(RY) [lindex $tmp 1]
   set Data(RZ) [lindex $tmp 2]

   set tmp [model matrix $Object -scale]
   set Data(SX) [lindex $tmp 0]
   set Data(SY) [lindex $tmp 1]
   set Data(SZ) [lindex $tmp 2]

   set Data(Ambi) [model material $Object -ambient]
   set Data(Emis) [model material $Object -emissive]
   set Data(Diff) [model material $Object -diffuse]
   set Data(Spec) [model material $Object -specular]

   #----- There might be no material and if so, those scale bound variables won't be happy
   catch { set Data(Shin) [model material $Object -shininess] }
   catch { set Data(Tram) [model material $Object -transparency] }
}

#-------------------------------------------------------------------------------
# Nom      : <Mapper::MDL::ParamsSet>
# Creation : Juillet 2004 - J.P. Gauthier - CMC/CMOE
#
# But      : Parametrer un objet de type Model
#
# Parametres :
#   <Object> : Donnee geographique a parametrer
#
# Retour    :
#
# Remarque :
#
#-------------------------------------------------------------------------------

proc Mapper::MDL::ParamsSet { Object } {
   variable Data

   if { $Mapper::Data(Init) } {
      return
   }
   Mapper::Cursor watch

   set Data(Proj) [string trim [$Data(Frame1).proj.val get 0.0 end] "\n"]
   model define $Object -projection $Data(Proj)
   model configure $Object -width $Data(Width)
   model configure $Object -outline $Data(Color)
   model configure $Object -dash $Data(Dash)
   model configure $Object -rendertexture $Data(Texture)
   model configure $Object -light $Data(Light)

   model matrix $Object -locate $Data(Lat) $Data(Lon) $Data(Ele)
   model matrix $Object -rotate $Data(RX) $Data(RY) $Data(RZ)
   model matrix $Object -scale $Data(SX) $Data(SY) $Data(SZ)

   model material  $Object -ambient $Data(Ambi)
   model material  $Object -emissive $Data(Emis)
   model material  $Object -diffuse $Data(Diff)
   model material  $Object -specular $Data(Spec)
   model material  $Object -shininess $Data(Shin)
   model material  $Object -transparency $Data(Tram)

   Page::Update $Page::Data(Frame)

   Mapper::Cursor left_ptr
}
