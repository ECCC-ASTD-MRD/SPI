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
   global GDefs
   variable Data
   variable Lbl
   variable Msg

   #----- Labels

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

   frame $Frame.opt
      button $Frame.opt.open -image PLUS -relief flat -bd 0 -overrelief raised -command "Meteo::All Open"
      button $Frame.opt.close -image MINUS -relief flat -bd 0 -overrelief raised -command "Meteo::All Close"
      pack $Frame.opt.open $Frame.opt.close -side left -padx 2

   pack $Frame.opt -side top -fill x -padx 2
   pack $Frame.info -side top -fill both -expand true  -padx 2 -pady 2

   Meteo::Tree
   Tree::Render $Frame.info.canvas METEOTREE 10 10 $Meteo::Data(Tree) Meteo::BranchRender

   Bubble::Create $Frame.opt.open  [lindex $Model::Bubble(Plus) $GDefs(Lang)]
   Bubble::Create $Frame.opt.close [lindex $Model::Bubble(Minus) $GDefs(Lang)]
}

proc Meteo::Tree { } {
   variable Data

   set Data(Tree) {}

   Tree::Add Meteo::Data(Tree) {prog 0}
      Tree::Add Meteo::Data(Tree) {glbeta 0}  "{prog 0}"
         Tree::Add Meteo::Data(Tree) {00Z fstdfield /data/gridpt/dbase/prog/glbeta 00 *} "{glbeta 0}"
         Tree::Add Meteo::Data(Tree) {12Z fstdfield /data/gridpt/dbase/prog/glbeta 12 *} "{glbeta 0}"
      Tree::Add Meteo::Data(Tree) {glbpres 0}  "{prog 0}"
         Tree::Add Meteo::Data(Tree) {00Z fstdfield /data/gridpt/dbase/prog/glbpres 00 *} "{glbpres 0}"
         Tree::Add Meteo::Data(Tree) {12Z fstdfield /data/gridpt/dbase/prog/glbpres 12 *} "{glbpres 0}"
      Tree::Add Meteo::Data(Tree) {regdiag 0}  "{prog 0}"
         Tree::Add Meteo::Data(Tree) {00Z fstdfield /data/gridpt/dbase/prog/regdiag 00 *} "{regdiag 0}"
         Tree::Add Meteo::Data(Tree) {12Z fstdfield /data/gridpt/dbase/prog/regdiag 12 *} "{regdiag 0}"
      Tree::Add Meteo::Data(Tree) {regeta 0}  "{prog 0}"
         Tree::Add Meteo::Data(Tree) {00Z fstdfield /data/gridpt/dbase/prog/regeta 00 *} "{regeta 0}"
         Tree::Add Meteo::Data(Tree) {12Z fstdfield /data/gridpt/dbase/prog/regeta 12 *} "{regeta 0}"
      Tree::Add Meteo::Data(Tree) {regpres 0}  "{prog 0}"
         Tree::Add Meteo::Data(Tree) {00Z fstdfield /data/gridpt/dbase/prog/regpres 00 *} "{regpres 0}"
         Tree::Add Meteo::Data(Tree) {12Z fstdfield /data/gridpt/dbase/prog/regpres 12 *} "{regpres 0}"
      Tree::Add Meteo::Data(Tree) {lameta 0}  "{prog 0}"
         Tree::Add Meteo::Data(Tree) {east fstdfield /data/gridpt/dbase/prog/lam/east.eta 12 *} "{lameta 0}"
         Tree::Add Meteo::Data(Tree) {west fstdfield /data/gridpt/dbase/prog/lam/west.eta 12 *} "{lameta 0}"
      Tree::Add Meteo::Data(Tree) {lampres 0}  "{prog 0}"
         Tree::Add Meteo::Data(Tree) {east fstdfield /data/gridpt/dbase/prog/lam/east.pres 12 *} "{lampres 0}"
         Tree::Add Meteo::Data(Tree) {west fstdfield /data/gridpt/dbase/prog/lam/west.pres 12 *} "{lampres 0}"
      Tree::Add Meteo::Data(Tree) {lamdiag 0}  "{prog 0}"
         Tree::Add Meteo::Data(Tree) {east fstdfield /data/gridpt/dbase/prog/lam/east.diag 12 *} "{lamdiag 0}"
         Tree::Add Meteo::Data(Tree) {west fstdfield /data/gridpt/dbase/prog/lam/west.diag 12 *} "{lamdiag 0}"
      Tree::Add Meteo::Data(Tree) {chronos 0}  "{prog 0}"
         Tree::Add Meteo::Data(Tree) {00Z fstdfield /data/gridpt/dbase/prog/chronos 00 *} "{chronos 0}"
         Tree::Add Meteo::Data(Tree) {12Z fstdfield /data/gridpt/dbase/prog/chronos 12 *} "{chronos 0}"

   Tree::Add Meteo::Data(Tree) {anal 0}
      Tree::Add Meteo::Data(Tree) {glbeta1 fstdfield /data/gridpt/dbase/anal/glbeta1 * *_???}  "{anal 0}"
      Tree::Add Meteo::Data(Tree) {glbeta2 fstdfield /data/gridpt/dbase/anal/glbeta2 * *_???}  "{anal 0}"
      Tree::Add Meteo::Data(Tree) {regeta1 fstdfield /data/gridpt/dbase/anal/regeta1 * *_???}  "{anal 0}"
      Tree::Add Meteo::Data(Tree) {regeta2 fstdfield /data/gridpt/dbase/anal/regeta2 * *_???}  "{anal 0}"
      Tree::Add Meteo::Data(Tree) {foudre  fstdfield /data/gridpt/dbase/anal/foudre * *_???}   "{anal 0}"

   Tree::Add Meteo::Data(Tree) {trial 0}
      Tree::Add Meteo::Data(Tree) {glbeta2 0}  "{trial 0}"
         Tree::Add Meteo::Data(Tree) {000 fstdfield /data/gridpt/dbase/trial/glbeta2 * *_000}  "{glbeta2 0}"
         Tree::Add Meteo::Data(Tree) {006 fstdfield /data/gridpt/dbase/trial/glbeta2 * *_006}  "{glbeta2 0}"
      Tree::Add Meteo::Data(Tree) {regeta2 0}  "{trial 0}"
         Tree::Add Meteo::Data(Tree) {000 fstdfield /data/gridpt/dbase/trial/regeta2 * *_000}  "{regeta2 0}"
         Tree::Add Meteo::Data(Tree) {006 fstdfield /data/gridpt/dbase/trial/regeta2 * *_006}  "{regeta2 0}"

   Tree::Add Meteo::Data(Tree) {sat 0}
      Tree::Add Meteo::Data(Tree) {goese fstdfield /data/sat/goese/fstd/imager/ * *_*}  "{sat 0}"
      Tree::Add Meteo::Data(Tree) {goesw fstdfield /data/sat/goesw/fstd/imager/ * *_*}  "{sat 0}"
#      Tree::Add Meteo::Data(Tree) {parallel1 0}  "{sat 0}"
#         Tree::Add Meteo::Data(Tree) {goesw 0}  "{parallel1 0}"
#            Tree::Add Meteo::Data(Tree) {fd fstdfield  /data/sat/parallel1/goesw/fstd/imager/fd/  * *_*}   "{goesw 0}"
#            Tree::Add Meteo::Data(Tree) {ncq fstdfield /data/sat/parallel1/goesw/fst/imager/ncq/ * *_*} "{goesw 0}"
#            Tree::Add Meteo::Data(Tree) {nwq fstdfield /data/sat/parallel1/goesw/fst/imager/nwq/ * *_*} "{goesw 0}"
#         Tree::Add Meteo::Data(Tree) {goese 0}  "{parallel1 0}"
#            Tree::Add Meteo::Data(Tree) {fd fstdfield /data/sat/parallel1/goesw/fst/imager/fd/ * *_*}   "{goese 0}"
#            Tree::Add Meteo::Data(Tree) {ncq fstdfield /data/sat/parallel1/goese/fst/imager/ncq/ * *_*} "{goese 0}"
#            Tree::Add Meteo::Data(Tree) {nwq fstdfield /data/sat/parallel1/goese/fst/imager/nwq/ * *_*} "{goese 0}"
}

proc Meteo::All { Mode } {
   variable Data

   if { $Mode=="Open" } {
      Tree::Parse $Meteo::Data(Tree) Meteo::AllOpen
   } else {
      Tree::Parse $Meteo::Data(Tree) Meteo::AllClose
   }

   $Data(Frame).info.canvas delete METEOTREE
   Tree::Render $Data(Frame).info.canvas METEOTREE 10 10 $Meteo::Data(Tree) Meteo::BranchRender
   $Data(Frame).info.canvas configure -scrollregion "0 0 [lrange [$Data(Frame).info.canvas bbox METEOTREE] 2 end]"

   Tree::ParseBranch $Meteo::Data(Tree)
}

proc Meteo::AllOpen { Branch Path } {

   if { [llength $Branch]==2 } {
      Tree::Set Meteo::Data(Tree) $Path "[lindex $Branch 0] 1"
   }
}

proc Meteo::AllClose { Branch Path } {

   if { [llength $Branch]==2 } {
      Tree::Set Meteo::Data(Tree) $Path "[lindex $Branch 0] 0"
   }
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
