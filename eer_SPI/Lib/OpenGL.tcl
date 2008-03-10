#===============================================================================
# Environnement Canada
# Centre Meteorologique Canadian
# 2100 Trans-Canadienne
# Dorval, Quebec
#
# Projet   : Librairie de projection.
# Fichier  : OpenGL.tk
# Version  : 1.1
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
# Modifications :
#
#   Nom         :
#   Date        :
#   Description :
#===============================================================================

package provide OpenGL 1.1

proc IdOpenGL { show } {
   global GDefs
   global env

   if { $show } {
      puts "(INFO) Loading Standard CMC/CMOE Canvas Package OpenGL Version 1.1"
   }

   load $GDefs(Dir)/Shared/$GDefs(Arch)/libTkglCanvas$GDefs(Ext) glCanvas

   puts stderr "(INFO) System: GLX [glrender -info GLX_VERSION] by [glrender -info GLX_VENDOR]"

   glrender -fsaa 4
}

namespace eval OpenGL {
   variable Param
   variable Stat
   variable Lbl

   #----- Parametres OpenGL

   set Param(Res)         0
   set Param(BBuf)        1
   set Param(Debug)       0
   set Param(Alias)       0
   set Param(Info)        ""
   set Param(Infos)       "GLX_VERSION GLX_VENDOR GLX_EXTENSIONS
                           GL_VERSION GL_VENDOR GL_EXTENSIONS GL_RENDERER
                           GLU_VERSION GLU_EXTENSIONS
                           X_DEPTH X_VISUAL"

   #----- Parametres de statistiques

   set Stat(RenderTime) -1   ;#Temps du rendue en secondes
   set Stat(FPS)        0    ;#Image par secondes
   set Stat(MemUsage)   0    ;#Espace en memoire residente requis

   set Lbl(Info)        { "Information" "Information" }
   set Lbl(Resources)   { "Ressources" "Resources" }
   set Lbl(Params)      { "Parametres" "Parameters" }
   set Lbl(Alias)       { "Anti-aliasing" "Anti-aliasing" }
   set Lbl(BBuf)        { "Rafraichissement par copie" "Back buffering refresh" }
   set Lbl(ZBuf)        { "Cohérence de profondeur (ZBuffer)" "Depth coherence (ZBuffer)" }
   set Lbl(Low)         { "Interactivité basse résolution" "Low resolution interactivity" }
   set Lbl(Time)        { "Temps du rendue" "Render time" }
   set Lbl(Frame)       { "Images par secondes" "Frame per second" }
   set Lbl(Memory)      { "Mémoire utilisée" "Memory usage" }
   set Lbl(Movement)    { "Déplacement" "Movement" }
   set Lbl(Fast)        { "Vite" "Fast" }
   set Lbl(Slow)        { "Lent" "Slow" }
   set Lbl(Delay)       { "Délai" "Delay" }
   set Lbl(Damping)     { "Amortissement" "Damping" }

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
# Modifications :
#
#    Nom         : -
#    Date        : -
#    Description : -
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
   pack $frame.info -side top -padx 5 -pady 2 -fill x

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
               $frame.res.info.mem -side top -anchor w
      pack $frame.res.info -side top -padx 2 -pady 2
   pack  $frame.res -padx 5 -pady 2 -side top -fill x

   labelframe $frame.params -text [lindex $Lbl(Params) $GDefs(Lang)]
      frame $frame.params.def -relief sunken -bd 1
         checkbutton $frame.params.def.debug  -text "Debug"          -variable OpenGL::Param(Debug) \
            -indicatoron false -command "glrender -debug \$OpenGL::Param(Debug); $Apply configure -state normal" -onvalue 1 -offvalue 0 -bd 1
         checkbutton $frame.params.def.alias  -text [lindex $Lbl(Alias) $GDefs(Lang)] -variable OpenGL::Param(Alias) \
            -indicatoron false -command "glrender -aliasing \$OpenGL::Param(Alias); $Apply configure -state normal" -onvalue 1 -offvalue 0 -bd 1
         checkbutton $frame.params.def.bbuf  -text [lindex $Lbl(BBuf) $GDefs(Lang)] -variable OpenGL::Param(BBuf) \
            -indicatoron false -command "$Apply configure -state normal" -onvalue 1 -offvalue 0 -bd 1
         checkbutton $frame.params.def.zbuf  -text [lindex $Lbl(ZBuf) $GDefs(Lang)] -variable OpenGL::Param(ZBuf) \
            -indicatoron false -command "glrender -zbuffer \$OpenGL::Param(ZBuf); $Apply configure -state normal" -onvalue 1 -offvalue 0 -bd 1
         checkbutton $frame.params.def.res -text [lindex $Lbl(Low) $GDefs(Lang)] -variable OpenGL::Param(Res) \
            -indicatoron false -onvalue 10 -offvalue 1 -bd 1
         pack $frame.params.def.debug $frame.params.def.alias $frame.params.def.bbuf $frame.params.def.zbuf $frame.params.def.res -side top -fill x
      pack $frame.params.def -side top -fill x
   pack  $frame.params -padx 5 -pady 2 -side top -fill x

   labelframe $frame.move -text [lindex $Lbl(Movement) $GDefs(Lang)]
      frame $frame.move.lbl
         label $frame.move.lbl.lbl -text "" -width 15 -anchor w
         label $frame.move.lbl.fast -text [lindex $Lbl(Fast) $GDefs(Lang)] -anchor w
         label $frame.move.lbl.slow -text [lindex $Lbl(Slow) $GDefs(Lang)] -anchor e
         pack $frame.move.lbl.lbl -side left
         pack $frame.move.lbl.fast $frame.move.lbl.slow  -side left -fill x -expand True

      frame $frame.move.delay
         label $frame.move.delay.lbl -text [lindex $Lbl(Delay) $GDefs(Lang)] -width 15 -anchor w
         scale $frame.move.delay.sc -orient horizontal -from 10 -to 5000 -showvalue False -variable Viewport::Map(Delay) -relief flat \
            -sliderlength 8  -bd 1 -resolution 10
         pack $frame.move.delay.lbl -side left
         pack $frame.move.delay.sc -side left -fill x -expand True
      frame $frame.move.damp
         label $frame.move.damp.lbl -text [lindex $Lbl(Damping) $GDefs(Lang)] -width 15 -anchor w
         scale $frame.move.damp.sc -orient horizontal -from 1.001 -to 1.2 -showvalue False -variable Viewport::Map(Damping) -relief flat \
            -sliderlength 8  -bd 1 -resolution 0.001
         pack $frame.move.damp.lbl -side left
         pack $frame.move.damp.sc -side left -fill x -expand True
      pack $frame.move.lbl $frame.move.delay $frame.move.damp -side top -fill x
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
# Modifications :
#
#    Nom         : -
#    Date        : -
#    Description : -
#----------------------------------------------------------------------------

proc OpenGL::Update { } {
   variable Stat
   variable Param

   set stat [glrender -stat]

   if { !$Param(Res) } {
      set OpenGL::Param(Res) [expr [glrender -direct]?1:10]
   }

   #----- si les informations resources sont affichees

   set Stat(RenderTime)  "[format "%2.3f" [lindex $stat 0]] sec"
   catch { set Stat(FPS) "[format "%1.3f" [expr 1.0/[lindex $stat 0]]] fps" }
   set Stat(MemUsage)    "[format "%2.3f" [lindex $stat 1]] Mb"

}

