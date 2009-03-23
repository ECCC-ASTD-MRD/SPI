#===============================================================================
# Environnement Canada
# Centre Meteorologique Canadian
# 2100 Trans-Canadienne
# Dorval, Quebec
#
# Projet   : SPI (Layouts).
# Fichier  : TRAJ.tcl
# Creation : Fevrier 2001 - J.P. Gauthier - CMC/CMOE
#
# Description:
#    Modeles de carte au format trajectoire
#
#===============================================================================

namespace eval TRAJ {
   variable Data
   variable Page
   variable OCXNO
   variable Lbl
   variable Msg

   set Lbl(Transmit)     { "Transmission" "Transmission" }
   set Lbl(Graph)        { "Graphique" "Graph" }
   set Lbl(Height)       { "Elevation" "Heights" }
   set Lbl(Join)         { "Préparer les images pour transmission vers les pages webs communes" "Prepare images for transmission on common web pages" }
   set Lbl(Legend)       { "Legende" "Legend" }
   set Lbl(No)           { "Non" "No" }
   set Lbl(Products)     { "Produits" "Products" }
   set Lbl(SATNET_Regs)  { "Transmission SATNET (Carte VAAC)"     "Transmit over SATNET (VAAC Map)" }
   set Lbl(SATNET_Spec)  { "Transmission SATNET (Carte Speciale)" "Transmit over SATNET (Special Map)" }
   set Lbl(SATNET_Test)  { "Transmission SATNET (Carte Test)"     "Transmit over SATNET (Test Map)" }
   set Lbl(Yes)          { "Oui" "Yes" }
   set Lbl(Warning)      { "Attention" "Warning" }

   #----- Definitions des messages

   set Msg(Transmit)     { "Voulez-vous vraiment transmettre cette carte ?"
                           "Do you really want to transmit this map ?" }
   set Msg(Join)         { "Voulez-vous produire la carte des trajectoires en format RSMC ?" \
                           "Do you want to generate the trajectories map in RSMC format ?" }
   set Msg(SATNET)       { "Transfert sur SATNET en cours ..." "Transferring on SATNET" }
   set Msg(FTP)          { "Transfert sur site FTP en cours ..." "Transferring on FTP site" }

   #----- Constantes

   set Data(Path)       $GDefs(DirData)  ;#Repertoire des trajectoires

   #----- Definitions des numeros de cartes

   set OCXNO(Test) ca0493c    ;#No d'envoi pour test
   set OCXNO(Spec) ca0948c    ;#No d'envoi special
   set OCXNO(Regs) ca0240c    ;#No d'envoi regulier (SATNET+NAVCAN)

   #----- Definitions des constantes de page

   set Page(VP)      ""

   #----- 8.5x11 (80dpi)

   set Page(Border) 15
   set Page(Width)  680
   set Page(Height) 880
}

#----------------------------------------------------------------------------
# Nom      : <TRAJ::Layout>
# Creation : Janvier 2001 - J.P. Gauthier - CMC/CMOE
#
# But      : Initialise l'affichage de la page
#
# Parametres :
#  <Frame>   : Identificateur de Page
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc TRAJ::Layout { Frame } {
   variable Page

   SPI::IcoClear

   #----- Initialisations des constantes relatives aux projections

   set Viewport::Map(Border)   1
   set Viewport::Map(LatLon)   1

   set SPI::Data(ShowTrajHeight$Frame) 1
   set SPI::Data(ShowTrajLegend$Frame) 1
   set SPI::Data(ShowTrajGraph$Frame)  1

   Page::Size $Frame $Page(Width) $Page(Height)
   set Page(VP) [Viewport::Create $Frame 0 0 0 0 0 0]

   TRAJ::LayoutToolBar $Frame
   TRAJ::LayoutUpdate  $Frame
}

#----------------------------------------------------------------------------
# Nom      : <TRAJ::LayoutUpdate>
# Creation : Janvier 2001 - J.P. Gauthier - CMC/CMOE
#
# But      : Mise a jour de l'affichage de la page
#
# Parametres :
#  <Frame>   : Identificateur de Page
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc TRAJ::LayoutUpdate { Frame } {
   variable Traject
   variable Page
   variable Data

   #----- Determiner le format de page selon la seletion

   set globex $Page(Border)
   if { $SPI::Data(ShowTrajHeight$Frame) } {
      incr globex 80
   }

   set legendy [expr $Page(Height)-$Page(Border)]
   if { $SPI::Data(ShowTrajLegend$Frame) } {
      incr legendy -80
   }

   if { !$SPI::Data(ShowTrajGraph$Frame) } {
      set graphy $legendy
   } else {
      set graphy [expr $Page(Height)-275]
   }

   Viewport::Resize $Frame $Page(VP) $globex $Page(Border) [expr $Page(Width)-$Page(Border)] $graphy True

   #----- Supprimer Tout sauf la projection

   $Frame.page.canvas delete TRAJGRAPH TRAJLEGEND TRAJHEIGHT FTID

   #----- Afficher la legende

   if { $SPI::Data(ShowTrajLegend$Frame) } {
      Trajectory::Legend $Frame $globex $legendy [expr $Page(Width)-$Page(Border)] [expr $Page(Height)-$Page(Border)] $Trajectory::Data(List)
   }

   #----- Afficher les hauteurs

   if { $SPI::Data(ShowTrajHeight$Frame) } {
      Trajectory::Height $Frame 15 15 $globex [expr $Page(Height)-$Page(Border)] $Trajectory::Data(List)
   }

   #----- Afficher le graphique des hauteurs

   if { $SPI::Data(ShowTrajGraph$Frame) } {
      Trajectory::Graph $Frame $globex $graphy [expr $Page(Width)-$Page(Border)] $legendy $Trajectory::Data(List)
   }

   if { [$Frame.page.canvas type TESTMODE] != "" } {
      $Frame.page.canvas raise TESTMODE
   }

   $Frame.page.canvas create text [expr $Page(Width)-1] [expr $Page(Height)-1] -font XFont10 -anchor se -text "[clock format [clock seconds] -format "%H%M %d/%m/%Y" -gmt true]" -tags FTID

   Page::Update $Frame
}

#----------------------------------------------------------------------------
# Nom      : <TRAJ::LayoutClear>
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

proc TRAJ::LayoutClear { Frame } {
   variable Page

   destroy $Frame.bar

   Viewport::Destroy $Frame

   set Page(VP) ""

   set SPI::Data(ShowTrajGraph$Frame)  0
   set SPI::Data(ShowTrajHeight$Frame) 0
   set SPI::Data(ShowTrajLegend$Frame) 0
}

#----------------------------------------------------------------------------
# Nom      : <TRAJ::PrintWidget>
# Creation : Mars 2002 - J.P. Gauthier - CMC/CMOE
#
# But      : Initialise un "widget" "plugin" pour les cas TRAJ a l'interieur
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

proc TRAJ::PrintWidget { Frame } {
   global GDefs

   #----- Constantes pour l'impression par PrintBox

   if { [llength $Trajectory::Data(List)] } {
      set PrintBox::Print(Filename) "[trajectory define [lindex $Trajectory::Data(List) 0] -ID]_traj"
   } else {
      set PrintBox::Print(Filename) "output"
   }
   set PrintBox::Print(FullName) "$PrintBox::Print(Path)/$PrintBox::Print(Filename)"
}

#----------------------------------------------------------------------------
# Nom      : <TRAJ::LayoutToolBar>
# Creation : Janvier 2001 - J.P. Gauthier - CMC/CMOE
#
# But      : Initialise les fonctions du layout
#
# Parametres :
#  <Frame>   : Identificateur de Page
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc TRAJ::LayoutToolBar { Frame } {
   global GDefs
   variable Lbl

  frame $Frame.bar -relief raised -bd 1
      label $Frame.bar.id -text " TRAJ " -relief sunken -bd 1

      menubutton $Frame.bar.prod -bd 1 -text [lindex $Lbl(Products) $GDefs(Lang)] -menu $Frame.bar.prod.menu
      button $Frame.bar.zoomto -image TARGET -relief flat -bd 0 -overrelief raised \
         -command { Trajectory::Locate [Viewport::Assigned $Page::Data(Frame) $Viewport::Data(VP) trajectory] 0.1 }

      checkbutton $Frame.bar.graph -text [lindex $Lbl(Graph) $GDefs(Lang)] -onvalue 1 -offvalue 0 \
         -relief sunken -bd 1 -overrelief raised -offrelief flat -indicatoron False\
         -variable SPI::Data(ShowTrajGraph$Frame) -command "TRAJ::LayoutUpdate $Frame"
      checkbutton $Frame.bar.height -text [lindex $Lbl(Height) $GDefs(Lang)] -onvalue 1 -offvalue 0 \
         -relief sunken -bd 1 -overrelief raised -offrelief flat -indicatoron False\
         -variable SPI::Data(ShowTrajHeight$Frame) -command "TRAJ::LayoutUpdate $Frame"
      checkbutton $Frame.bar.legend -text [lindex $Lbl(Legend) $GDefs(Lang)] -onvalue 1 -offvalue 0 \
         -relief sunken -bd 1 -overrelief raised -offrelief flat -indicatoron False\
         -variable SPI::Data(ShowTrajLegend$Frame) -command "TRAJ::LayoutUpdate $Frame"
      pack $Frame.bar.height $Frame.bar.graph $Frame.bar.legend -side right -ipadx 2 -ipady 1
      pack $Frame.bar.id $Frame.bar.prod $Frame.bar.zoomto -side left -fill y

      menu $Frame.bar.prod.menu -tearoff 0 -bd 1 -activeborderwidth 1
         $Frame.bar.prod.menu add command -label [lindex $Lbl(Join) $GDefs(Lang)] \
            -command "TRAJ::RSMCJoin $Frame"
         $Frame.bar.prod.menu add separator
         $Frame.bar.prod.menu add command -label [lindex $Lbl(SATNET_Regs) $GDefs(Lang)] \
            -command "TRAJ::SATNET $Frame regs"
         $Frame.bar.prod.menu add command -label [lindex $Lbl(SATNET_Test) $GDefs(Lang)] \
            -command "TRAJ::SATNET $Frame test"
         $Frame.bar.prod.menu add command -label [lindex $Lbl(SATNET_Spec) $GDefs(Lang)] \
            -command "TRAJ::SATNET $Frame spec"

   pack $Frame.bar -side top -fill x  -before $Frame.page
}

#----------------------------------------------------------------------------
# Nom      : <TRAJ::RSMCJoin>
# Creation : Avril 2000 - J.P. Gauthier - CMC/CMOE
#
# But      : Sortie au format RSMC commun.
#
# Parametres :
#  <Frame>   : Identificateur de Page
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc TRAJ::RSMCJoin { Frame } {
   global GDefs
   variable Lbl
   variable Msg
   variable Data

   #----- Pour la premiere Trajectoire selectionnee

   if { [set t [lindex $Trajectory::Data(List) 0]] == "" } {
      return
   }

   set create [Dialog::CreateDefault . 400 [lindex $Lbl(Warning) $GDefs(Lang)] [lindex $Msg(Join) $GDefs(Lang)] \
      warning 0 [lindex $Lbl(Yes) $GDefs(Lang)] [lindex $Lbl(No) $GDefs(Lang)]]

   if { $create } {
      return
   }

   $Frame.page.canvas config -cursor watch
   update idletasks

   set path "[file dirname [trajectory define $t -PATH]]/../Output/RSMCJoin"

   if { ![file exists $path] } {
      file mkdir $path
   }

   file copy -force [trajectory define $t -PATH] $path

   PrintBox::Image $Frame gif $path/LTJCA
   exec convert $path/LTJCA.gif -resize 280x280 $path/STJCA.gif

   PrintBox::Postscript $Frame $path/LTJCA 0 0 [Page::CanvasWidth $Frame] [Page::CanvasHeight $Frame] portrait "8.5_x_11"

   $Frame.page.canvas config -cursor left_ptr
}

#----------------------------------------------------------------------------
# Nom      : <TRAJ::SATNET>
# Creation : Novembre 1998 - J.P. Gauthier - CMC/CMOE
#
# But      : Effectue le transfert de la carte sur le circuit METSIS.
#
# Parametres :
#  <Frame>   : Identificateur de Page
#  <Mode>    : Mode d'execution (test=Mode test, real=transfert)
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc TRAJ::SATNET { Frame Mode } {
   global GDefs
   variable Lbl
   variable Msg
   variable OCXNO
   variable Page

   set transmit [Dialog::CreateDefault . 400 [lindex $Lbl(Warning) $GDefs(Lang)] [lindex $Msg(Transmit) $GDefs(Lang)] \
      warning 0 [lindex $Lbl(Yes) $GDefs(Lang)] [lindex $Lbl(No) $GDefs(Lang)]]

   if { $transmit } {
      return
   }

   $Frame.page.canvas config -cursor watch
   update idletasks

   SPI::Progress 0

   set file "$GDefs(DirEER)/eer_Tmp/TRAJ_[pid]_[clock seconds]"

   #----- Generer le fichier image

   SPI::Progress 5 "Creating postscript for $Frame"

   $Frame.page.canvas postscript -x 0 -y 0 -width [Page::CanvasWidth $Frame] -height [Page::CanvasHeight $Frame] \
      -rotate false -colormode color -pagewidth 7.5i -file $file.ps

   SPI::Progress 30 "Converting to GIF File"

   catch { exec grep -v showpage $file.ps | convert -density 100 - $file.gif }

   #----- Transferer le fichier image sur SATNET
   #      No pour test                        : ca0493c
   #      No d'envoi regulier (SATNET+NAVCAN) : ca0240c
   #      No d'envoi special                  : ca0948c

   switch $Mode {

      "test" { set no $OCXNO(Test) }
      "spec" { set no $OCXNO(Spec) }
      "regs" { set no $OCXNO(Regs) }
   }

   Debug::TraceProc "Sending $file.gif over SATNET"
   SPI::Progress 40 "Sending $file.gif over SATNET"

   catch  { exec ssh $GDefs(FrontEnd) -l -x $GDefs(FrontEndUser) "export OPERATIONAL=YES; export JOBNAME=r1; cd $GDefs(DirEER)/eer_Tmp/ ; /software/pub/bin/udo afsiops /usr/local/env/afsisio/scripts/op/ocxcarte -t -f $no -d difax -r systime -i ${file}.gif" }

   #----- envoyer sur les sites web.

   set name "[trajectory define [lindex $Trajectory::Data(List) 0] -ID]"
   exec convert ${file}.gif -resize $Page(Width)x$Page(Height) ${file}.png

   set prefix [clock format [clock seconds] -format "%Y%m%d-%H%MZ" -gmt true]
   catch  { exec ssh $GDefs(FrontEnd) -l -x $GDefs(FrontEndUser) ". ~/.profile; /software/pub/bin/udo afsiadm webprods -f ${file}.png -s weather -D 0 -p eer/data/vaac/current/${prefix}_${name}_traj_satnet.png"  }

   #----- supprimer les residus

   SPI::Progress 20 "Cleaning temporary files"
   file delete -force $file.ps $file.png $file.gif

   SPI::Progress 0
   $Frame.page.canvas config -cursor left_ptr
}
