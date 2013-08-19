#===============================================================================
# Environnement Canada
# Centre Meteorologique Canadian
# 2100 Trans-Canadienne
# Dorval, Quebec
#
# Projet   : Librairie de projection.
# Fichier  : OpenGL.tk
# Creation : MOctobre 2003 - J.P. Gauthier - CMC/CMOE
#
# Description:
#   -Definit un controle OpenGL permettant le parametrage et la recuperation
#    d'information relative au moteur de rendue
#
# Fonctions:
#
#    OpenGL::ParamFrame { Frame }
#    OpenGL::Update     { }
#
#===============================================================================

package provide OpenGL 1.2

catch { SPI::Splash "Loading Canvas Package OpenGL 1.2" }

package require TkglCanvas

namespace eval OpenGL {
   variable Param
   variable Stat
   variable Lbl

   #----- Parametres OpenGL

   set Param(Delay)       2000
   set Param(Res)         0
   set Param(Debug)       0
   set Param(Alias)       0
   set Param(FSAA)        4
   set Param(ZBuf)        0
   set Param(Info)        ""
   set Param(Shaders)     { Field FieldTex DataTex TopoTex }
   set Param(ShaderPath)  $GDefs(Dir)/share/shader
   set Param(Infos)       "GLX_VERSION GLX_VENDOR GLX_EXTENSIONS
                           GL_VERSION GL_VENDOR GL_EXTENSIONS GL_RENDERER
                           GLU_VERSION GLU_EXTENSIONS
                           X_DEPTH X_VISUAL"

   #----- Parametres de statistiques

   set Stat(RenderTime) -1   ;#Temps du rendue en secondes
   set Stat(FPS)         0    ;#Image par secondes
   set Stat(MemUsage)    0    ;#Espace en memoire residente requis

   set Lbl(Info)        { "Information" "Information" }
   set Lbl(Resources)   { "Ressources" "Resources" }
   set Lbl(Params)      { "Paramètres" "Parameters" }
   set Lbl(Alias)       { "Anti-aliasing" "Anti-aliasing" }
   set Lbl(ZBuf)        { "Cohérence de profondeur (ZBuffer)" "Depth coherence (ZBuffer)" }
   set Lbl(Low)         { "Interactivité dynamique" "Dynamic interactivity" }
   set Lbl(Time)        { "Temps du rendue" "Render time" }
   set Lbl(Frame)       { "Images par secondes" "Frame per second" }
   set Lbl(Memory)      { "Mémoire utilisée" "Memory usage" }
   set Lbl(Movement)    { "Déplacement" "Movement" }
   set Lbl(Fast)        { "Vite" "Fast" }
   set Lbl(Slow)        { "Lent" "Slow" }
   set Lbl(Delay)       { "Délai" "Delay" }
   set Lbl(Damping)     { "Amortissement" "Damping" }
   set Lbl(Dynamic)     { "Dynamique" "Dynamics" }

   glrender -shaderpath $Param(ShaderPath) -shaders $Param(Shaders)
   puts "(INFO) System: GLX [glrender -info GLX_VERSION] by [glrender -info GLX_VENDOR]"
}

#----------------------------------------------------------------------------
# Nom      : <OpenGL::ParamFrame>
# Creation : Octobre 2003 - J.P. Gauthier - CMC/CMOE
#
# But      : Definition des parametres des options OpenGL.
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

proc OpenGL::ParamFrame { Frame Apply } {
   global GDefs
   variable Bubble
   variable Lbl

   #----- Inserer l'onglet du renderer

   set frame [TabFrame::Add $Frame 1 "OpenGL" False ""]

   labelframe $frame.info -text [lindex $Lbl(Info) $GDefs(Lang)]
      ComboBox::Create $frame.info.sel OpenGL::Param(Info) noedit unsorted nodouble -1 \
          $OpenGL::Param(Infos) 16 4 \
          "$frame.info.txt delete 0 end ;$frame.info.txt insert end \[glrender -info \$OpenGL::Param(Info)\]"
      entry $frame.info.txt -width 20 -relief sunken -bd 1 -bg $GDefs(ColorLight)
      pack $frame.info.sel $frame.info.txt -side left -pady 2 -padx 2 -fill x -expand true
   pack $frame.info -side top -padx 5 -pady 5 -fill x

   labelframe $frame.res -text [lindex $Lbl(Resources) $GDefs(Lang)]
      frame $frame.res.lbl
         label $frame.res.lbl.time -text [lindex $Lbl(Time) $GDefs(Lang)] -anchor w
         label $frame.res.lbl.fps  -text [lindex $Lbl(Frame) $GDefs(Lang)] -anchor w
         label $frame.res.lbl.mem  -text [lindex $Lbl(Memory) $GDefs(Lang)] -anchor w
         pack $frame.res.lbl.time $frame.res.lbl.fps \
            $frame.res.lbl.mem -side top -anchor w
      pack $frame.res.lbl -side left -padx 2

      frame $frame.res.info
         label $frame.res.info.time -textvariable OpenGL::Stat(RenderTime) \
            -bg $GDefs(ColorLight) -relief sunken -bd 1 -width 14 -anchor w
         label $frame.res.info.fps  -textvariable OpenGL::Stat(FPS) \
            -bg $GDefs(ColorLight) -relief sunken -bd 1 -width 14 -anchor w
         label $frame.res.info.mem  -textvariable OpenGL::Stat(MemUsage) \
             -bg $GDefs(ColorLight) -relief sunken -bd 1  -width 14 -anchor w
         pack  $frame.res.info.time $frame.res.info.fps \
               $frame.res.info.mem -side top -anchor w -fill x -expand True
      pack $frame.res.info -side top -fill x -padx 2 -pady 2
   pack  $frame.res -padx 5 -pady 2 -side top -fill x

   labelframe $frame.params -text [lindex $Lbl(Params) $GDefs(Lang)]
      frame $frame.params.def -relief sunken -bd 1
         checkbutton $frame.params.def.debug  -text "Debug"          -variable OpenGL::Param(Debug) \
            -indicatoron false -command "glrender -debug \$OpenGL::Param(Debug); $Apply configure -state normal" -onvalue 1 -offvalue 0 -bd 1
         checkbutton $frame.params.def.alias  -text [lindex $Lbl(Alias) $GDefs(Lang)] -variable OpenGL::Param(Alias) \
            -indicatoron false -command "glrender -aliasing \$OpenGL::Param(Alias); $Apply configure -state normal" -onvalue 1 -offvalue 0 -bd 1
         checkbutton $frame.params.def.zbuf  -text [lindex $Lbl(ZBuf) $GDefs(Lang)] -variable OpenGL::Param(ZBuf) \
            -indicatoron false -command "glrender -zbuffer \$OpenGL::Param(ZBuf); $Apply configure -state normal" -onvalue 1 -offvalue 0 -bd 1
         checkbutton $frame.params.def.res -text [lindex $Lbl(Low) $GDefs(Lang)] -variable OpenGL::Param(Res) \
            -indicatoron false -onvalue 1 -offvalue 10 -bd 1
         pack $frame.params.def.debug $frame.params.def.alias $frame.params.def.zbuf $frame.params.def.res -side top -fill x
      pack $frame.params.def -side top -fill x
   pack  $frame.params -padx 5 -pady 2 -side top -fill x

   labelframe $frame.move -text [lindex $Lbl(Dynamic) $GDefs(Lang)]
      frame $frame.move.lbl
         label $frame.move.lbl.lbl -text "" -width 15 -anchor w
         label $frame.move.lbl.fast -text [lindex $Lbl(Fast) $GDefs(Lang)] -anchor w
         label $frame.move.lbl.slow -text [lindex $Lbl(Slow) $GDefs(Lang)] -anchor e
         pack $frame.move.lbl.lbl -side left
         pack $frame.move.lbl.fast $frame.move.lbl.slow  -side left -fill x -expand True

      frame $frame.move.disp
         label $frame.move.disp.lbl -text [lindex $Lbl(Movement) $GDefs(Lang)] -width 15 -anchor w
         scale $frame.move.disp.sc -orient horizontal -from 10 -to 5000 -showvalue False -variable Viewport::Map(Delay) -relief flat \
            -sliderlength 8  -bd 1 -resolution 10
         pack $frame.move.disp.lbl -side left
         pack $frame.move.disp.sc -side left -fill x -expand True
      frame $frame.move.damp
         label $frame.move.damp.lbl -text [lindex $Lbl(Damping) $GDefs(Lang)] -width 15 -anchor w
         scale $frame.move.damp.sc -orient horizontal -from 1.001 -to 1.2 -showvalue False -variable Viewport::Map(Damping) -relief flat \
            -sliderlength 8  -bd 1 -resolution 0.001
         pack $frame.move.damp.lbl -side left
         pack $frame.move.damp.sc -side left -fill x -expand True
      frame $frame.move.delay
         label $frame.move.delay.lbl -text [lindex $Lbl(Delay) $GDefs(Lang)] -width 15 -anchor w
         scale $frame.move.delay.sc -orient horizontal -from 0 -to 2000 -showvalue False -variable OpenGL::Param(Delay) -relief flat \
            -sliderlength 8  -bd 1 -resolution 10 -command "glrender -delay \$OpenGL::Param(Delay); $Apply configure -state normal; catch"
         pack $frame.move.delay.lbl -side left
         pack $frame.move.delay.sc -side left -fill x -expand True
      pack $frame.move.lbl $frame.move.disp $frame.move.damp $frame.move.delay -side top -fill x
   pack $frame.move -side top -padx 5 -pady 2 -fill x
}

#----------------------------------------------------------------------------
# Nom      : <OpenGL::Update>
# Creation : Octobre 2003 - J.P. Gauthier - CMC/CMOE
#
# But      : Recupere les informations sur les resources utilisees.
#
# Parametres   :
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc OpenGL::Update { } {
   variable Stat
   variable Param

   if { !$Param(Res) } {
      set Param(Res)  [expr [glrender -direct]?1:10]
   }

   set sec [glrender -time]

   set Stat(RenderTime)  [format "%6.4f sec" $sec]
   catch { set Stat(FPS) [format "%5.3f fps" [expr 1.0/$sec]] }
   set Stat(MemUsage)    [format "%2i Mb" [expr [system usage -rss]/1024]]
}
