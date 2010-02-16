#===============================================================================
# Environnement Canada
# Centre Meteorologique Canadian
# 2100 Trans-Canadienne
# Dorval, Quebec
#
# Projet   : Librairie de "Widget" Tk.
# Fichier  : Meteo.tcl
# Creation : Janvier 2004 - J.P.Gauthier - CMC/CMOE
#
# Description:
#    Fonctions de manipualtions des Experiences.
#
# Remarques :
#
#===============================================================================

namespace eval Meteo {
}

#----------------------------------------------------------------------------
# Nom      : <Meteo::Create>
# Creation : Aout 2001 1999 - J.P. Gauthier - CMC/CMOE
#
# But      : Affichage de l'arborescence meteorologique
#
# Parametres :
#   <Frame>       : Frame du widget
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc Meteo::Create { Frame } {
   global GDefs
   variable Data

   set Data(Frame) $Frame

   frame $Frame.info
      canvas $Frame.info.canvas -bg white -relief sunken -bd 1 -yscrollcommand "$Frame.info.scroll set" \
         -scrollregion "1 1 280 5000" -width 200 -height 1
      scrollbar $Frame.info.scroll -orient vertical -bd 1 -width 10 \
         -command "$Frame.info.canvas yview"
      pack $Frame.info.canvas -side left -fill both -expand true
      pack $Frame.info.scroll -side left -fill y

      bind $Frame.info.canvas <Button-4> "$Frame.info.canvas yview scroll -1 units"
      bind $Frame.info.canvas <Button-5> "$Frame.info.canvas yview scroll 1 units"
   pack $Frame.info -side top -fill both -expand true  -padx 2 -pady 2

   Meteo::Tree
   Tree::Render $Frame.info.canvas METEOTREE 10 10 $Meteo::Data(Tree) Meteo::BranchRender

   Bubble::Create $Frame.opt.open  $Model::Bubble(Plus)
   Bubble::Create $Frame.opt.close $Model::Bubble(Minus)
}

proc Meteo::Tree { } {
   variable Data

   set Data(Tree) {}

   Tree::Add Meteo::Data(Tree) {prog 0}
      Tree::Add Meteo::Data(Tree) {glbhyb 0}  "{prog 0}"
         Tree::Add Meteo::Data(Tree) {00Z fstdfield $MetData::Param(Path)/prog/glbhyb 00 *} "{glbhyb 0}"
         Tree::Add Meteo::Data(Tree) {12Z fstdfield $MetData::Param(Path)/prog/glbhyb 12 *} "{glbhyb 0}"
      Tree::Add Meteo::Data(Tree) {glbeta 0}  "{prog 0}"
         Tree::Add Meteo::Data(Tree) {00Z fstdfield $MetData::Param(Path)/prog/glbeta 00 *} "{glbeta 0}"
         Tree::Add Meteo::Data(Tree) {12Z fstdfield $MetData::Param(Path)/prog/glbeta 12 *} "{glbeta 0}"
      Tree::Add Meteo::Data(Tree) {glbpres 0}  "{prog 0}"
         Tree::Add Meteo::Data(Tree) {00Z fstdfield $MetData::Param(Path)/prog/glbpres 00 *} "{glbpres 0}"
         Tree::Add Meteo::Data(Tree) {12Z fstdfield $MetData::Param(Path)/prog/glbpres 12 *} "{glbpres 0}"
      Tree::Add Meteo::Data(Tree) {glbdiag 0}  "{prog 0}"
         Tree::Add Meteo::Data(Tree) {00Z fstdfield $MetData::Param(Path)/prog/glbdiag 00 *} "{glbdiag 0}"
         Tree::Add Meteo::Data(Tree) {12Z fstdfield $MetData::Param(Path)/prog/glbdiag 12 *} "{glbdiag 0}"
      Tree::Add Meteo::Data(Tree) {reghyb 0}  "{prog 0}"
         Tree::Add Meteo::Data(Tree) {00Z fstdfield $MetData::Param(Path)/prog/reghyb 00 *} "{reghyb 0}"
         Tree::Add Meteo::Data(Tree) {06Z fstdfield $MetData::Param(Path)/prog/reghyb 06 *} "{reghyb 0}"
         Tree::Add Meteo::Data(Tree) {12Z fstdfield $MetData::Param(Path)/prog/reghyb 12 *} "{reghyb 0}"
         Tree::Add Meteo::Data(Tree) {18Z fstdfield $MetData::Param(Path)/prog/reghyb 18 *} "{reghyb 0}"
      Tree::Add Meteo::Data(Tree) {regeta 0}  "{prog 0}"
         Tree::Add Meteo::Data(Tree) {00Z fstdfield $MetData::Param(Path)/prog/regeta 00 *} "{regeta 0}"
         Tree::Add Meteo::Data(Tree) {06Z fstdfield $MetData::Param(Path)/prog/regeta 06 *} "{regeta 0}"
         Tree::Add Meteo::Data(Tree) {12Z fstdfield $MetData::Param(Path)/prog/regeta 12 *} "{regeta 0}"
         Tree::Add Meteo::Data(Tree) {18Z fstdfield $MetData::Param(Path)/prog/regeta 18 *} "{regeta 0}"
      Tree::Add Meteo::Data(Tree) {regpres 0}  "{prog 0}"
         Tree::Add Meteo::Data(Tree) {00Z fstdfield $MetData::Param(Path)/prog/regpres 00 *} "{regpres 0}"
         Tree::Add Meteo::Data(Tree) {06Z fstdfield $MetData::Param(Path)/prog/regpres 06 *} "{regpres 0}"
         Tree::Add Meteo::Data(Tree) {12Z fstdfield $MetData::Param(Path)/prog/regpres 12 *} "{regpres 0}"
         Tree::Add Meteo::Data(Tree) {18Z fstdfield $MetData::Param(Path)/prog/regpres 18 *} "{regpres 0}"
      Tree::Add Meteo::Data(Tree) {regdiag 0}  "{prog 0}"
         Tree::Add Meteo::Data(Tree) {00Z fstdfield $MetData::Param(Path)/prog/regdiag 00 *} "{regdiag 0}"
         Tree::Add Meteo::Data(Tree) {12Z fstdfield $MetData::Param(Path)/prog/regdiag 12 *} "{regdiag 0}"
      Tree::Add Meteo::Data(Tree) {lameta 0}  "{prog 0}"
         Tree::Add Meteo::Data(Tree) {east fstdfield $MetData::Param(Path)/prog/lam/east.eta 12 *} "{lameta 0}"
         Tree::Add Meteo::Data(Tree) {west fstdfield $MetData::Param(Path)/prog/lam/west.eta 12 *} "{lameta 0}"
      Tree::Add Meteo::Data(Tree) {lampres 0}  "{prog 0}"
         Tree::Add Meteo::Data(Tree) {east fstdfield $MetData::Param(Path)/prog/lam/east.pres 12 *} "{lampres 0}"
         Tree::Add Meteo::Data(Tree) {west fstdfield $MetData::Param(Path)/prog/lam/west.pres 12 *} "{lampres 0}"
      Tree::Add Meteo::Data(Tree) {lamdiag 0}  "{prog 0}"
         Tree::Add Meteo::Data(Tree) {east fstdfield $MetData::Param(Path)/prog/lam/east.diag 12 *} "{lamdiag 0}"
         Tree::Add Meteo::Data(Tree) {west fstdfield $MetData::Param(Path)/prog/lam/west.diag 12 *} "{lamdiag 0}"
      Tree::Add Meteo::Data(Tree) {mach 0}  "{prog 0}"
         Tree::Add Meteo::Data(Tree) {00Z fstdfield $MetData::Param(Path)/prog/mach 00 *} "{mach 0}"
         Tree::Add Meteo::Data(Tree) {12Z fstdfield $MetData::Param(Path)/prog/mach 12 *} "{mach 0}"

#   Tree::Add Meteo::Data(Tree) {par 0}
#      Tree::Add Meteo::Data(Tree) {progpar 0} "{par 0}"
#         Tree::Add Meteo::Data(Tree) {mach 0}  "{progpar 0}"
#            Tree::Add Meteo::Data(Tree) {00Z fstdfield /data/gridpt/par/dbase/prog/mach 00 *} "{mach 0}"
#            Tree::Add Meteo::Data(Tree) {12Z fstdfield /data/gridpt/par/dbase/prog/mach 12 *} "{mach 0}"

   Tree::Add Meteo::Data(Tree) {anal 0}
      Tree::Add Meteo::Data(Tree) {glbeta1 fstdfield $MetData::Param(Path)/anal/glbhyb1 * *_???}  "{anal 0}"
      Tree::Add Meteo::Data(Tree) {glbeta2 fstdfield $MetData::Param(Path)/anal/glbhyb2 * *_???}  "{anal 0}"
      Tree::Add Meteo::Data(Tree) {glbeta1 fstdfield $MetData::Param(Path)/anal/glbeta1 * *_???}  "{anal 0}"
      Tree::Add Meteo::Data(Tree) {glbeta2 fstdfield $MetData::Param(Path)/anal/glbeta2 * *_???}  "{anal 0}"
      Tree::Add Meteo::Data(Tree) {regeta1 fstdfield $MetData::Param(Path)/anal/reghyb1 * *_???}  "{anal 0}"
      Tree::Add Meteo::Data(Tree) {regeta2 fstdfield $MetData::Param(Path)/anal/reghyb2 * *_???}  "{anal 0}"
      Tree::Add Meteo::Data(Tree) {regeta1 fstdfield $MetData::Param(Path)/anal/regeta1 * *_???}  "{anal 0}"
      Tree::Add Meteo::Data(Tree) {regeta2 fstdfield $MetData::Param(Path)/anal/regeta2 * *_???}  "{anal 0}"
      Tree::Add Meteo::Data(Tree) {foudre  fstdfield $MetData::Param(Path)/anal/foudre * *_???}   "{anal 0}"

   Tree::Add Meteo::Data(Tree) {trial 0}
      Tree::Add Meteo::Data(Tree) {glbhyb2 0}  "{trial 0}"
         Tree::Add Meteo::Data(Tree) {000 fstdfield $MetData::Param(Path)/trial/glbhyb2 * *_000}  "{glbhyb2 0}"
         Tree::Add Meteo::Data(Tree) {006 fstdfield $MetData::Param(Path)/trial/glbhyb2 * *_006}  "{glbhyb2 0}"
      Tree::Add Meteo::Data(Tree) {glbeta2 0}  "{trial 0}"
         Tree::Add Meteo::Data(Tree) {000 fstdfield $MetData::Param(Path)/trial/glbeta2 * *_000}  "{glbeta2 0}"
         Tree::Add Meteo::Data(Tree) {006 fstdfield $MetData::Param(Path)/trial/glbeta2 * *_006}  "{glbeta2 0}"
      Tree::Add Meteo::Data(Tree) {reghyb2 0}  "{trial 0}"
         Tree::Add Meteo::Data(Tree) {000 fstdfield $MetData::Param(Path)/trial/reghyb2 * *_000}  "{reghyb2 0}"
         Tree::Add Meteo::Data(Tree) {006 fstdfield $MetData::Param(Path)/trial/reghyb2 * *_006}  "{reghyb2 0}"
      Tree::Add Meteo::Data(Tree) {regeta2 0}  "{trial 0}"
         Tree::Add Meteo::Data(Tree) {000 fstdfield $MetData::Param(Path)/trial/regeta2 * *_000}  "{regeta2 0}"
         Tree::Add Meteo::Data(Tree) {006 fstdfield $MetData::Param(Path)/trial/regeta2 * *_006}  "{regeta2 0}"

   Tree::Add Meteo::Data(Tree) {sat 0}
      Tree::Add Meteo::Data(Tree) {goese fstdfield /data/sat/goese/fstd/imager/ * *_*}  "{sat 0}"
      Tree::Add Meteo::Data(Tree) {goesw fstdfield /data/sat/goesw/fstd/imager/ * *_*}  "{sat 0}"
}

proc Meteo::BranchData { Type Path Run Pattern } {

   if { $Run!="*" } {
      set run [lindex [split [lindex [lsort -dictionary -increasing [glob -nocomplain  -tails -directory $Path  *${Run}_???]] end] _] 0]
      set files [lsort -dictionary -increasing [glob -nocomplain  -directory $Path ${run}_???]]
      set title "$Path $Run"
   } else {
      set files [lsort -dictionary -increasing [glob -nocomplain  -directory $Path $Pattern]]
      set title "$Path $Pattern"
   }

   set box [FieldBox::Create . $title]
   FieldBox::FileOpen $box $files
}

proc Meteo::BranchRender { Canvas X Y Id Branch Path } {
   global GDefs

   set info [lindex $Branch end]

   $Canvas create text [expr $X+10] $Y -text [lindex $info 0] -anchor w -tags "METEOTREE $Id" -font $GDefs(Font)

   if { [llength [lindex $Branch 0]] } {
      if { [lindex $info 1] } {
         $Canvas create bitmap $X $Y -bitmap @$GDefs(Dir)/Resources/Bitmap/minus.ico -tags "METEOTREE $Id"
         $Canvas bind $Id <Button-1> "Meteo::BranchSelect \"$Path\" \"[lindex $Branch end]\" 0"
         return 1
      } else {
         $Canvas create bitmap $X $Y -bitmap @$GDefs(Dir)/Resources/Bitmap/plus.ico -tags "METEOTREE $Id"
         $Canvas bind $Id <Button-1> "Meteo::BranchSelect \"$Path\" \"[lindex $Branch end]\" 1"
         return 0
      }
   } else {
      $Canvas bind $Id <Button-1> "$Canvas configure -cursor watch; update idletasks; Meteo::BranchData [lindex $info 1] [lindex $info 2] [lindex $info 3] [lindex $info 4]; $Canvas configure -cursor left_ptr"
   }

   return F0
}

proc Meteo::BranchSelect { Path Branch Mode } {
   variable Data

   Tree::Set Meteo::Data(Tree) $Path "[lindex $Branch 0] $Mode"

   $Data(Frame).info.canvas delete METEOTREE
   Tree::Render $Data(Frame).info.canvas METEOTREE 10 10 $Meteo::Data(Tree) Meteo::BranchRender
   $Data(Frame).info.canvas configure -scrollregion "0 0 [lrange [$Data(Frame).info.canvas bbox METEOTREE] 2 end]"
}
