#===============================================================================
# Environnement Canada
# Centre Meteorologique Canadian
# 2100 Trans-Canadienne
# Dorval, Quebec
#
# Projet   : Exemples de scripts de productions de cartes.
# Fichier  : Verif_UMOS.tcl
# Creation : Fevrier 2008 - J.P.Gauthier - CMC/CMOE
#
# Description:
#    Verifier le qpf 2448 du regional a (00/12Z)  avec pointage des shef.
#
# Arguments  :
#
# Remarques :
#
#===============================================================================

namespace eval Macro::Verif_UMOS {} {
   global env
   variable Param
   variable Data
   variable Error

   set Param(Pred)      ""
   set Param(Preds)     { spot3 wnd3s wnd3u wnd3v pop06 p0212 wnm6s wnm6u wnm6v }
   set Param(Anals)     { TT    VT    UU    VV    I3    I4    VT    UU    VV    }
   set Param(Anals)     { TT }
   set Param(PredPath)  /data/cmdw/jmo/umos/reg/series
   set Param(AnalPath)  $env(CMCGRIDF)/anal/reghyb3
   set Param(Date)      2010051300

   set Param(Info)     { "Verification UMOS"
                         "Verification of UMOS" }
   set Param(InfoArgs) { { "Date des données (YYYMMDDHH)" "Predicteur" } { "Date of data (YYYYMMDDHH)" "Predcitor" } }

   set Data(Page)    ""

   set Error(Field)  { "Aucun champs valide trouvé!\nVérifiez le viewport actif."
                       "Could not find any valid field!\nCheck the active viewport." }
}

proc Macro::Verif_UMOS::Execute { } {
   global GDefs env
   variable Data
   variable Error
   variable Param

   Macro::Cursor watch

   Macro::Doing "Creating Camera"
   ProjCam::Set Verif_UMOS      { 0.0 0.0 1.0 } { 0.0 0.0 2.0 } { 0.0 1.0 0.0 } 2.42838976879 0 0 1 0 0 0 68.5889576692 -94.5206776288
   ProjCam::Set Verif_UMOS_WEST { 0.0 0.0 1.0 } { 0.0 0.0 2.0 } { 0.0 1.0 0.0 } 5.44317492889 0 0 1 0 0 0 49.3270542318 -118.581223592
   ProjCam::Set Verif_UMOS_EAST { 0.0 0.0 1.0 } { 0.0 0.0 2.0 } { 0.0 1.0 0.0 } 6.72997751255 0 0 1 0 0 0 46.8057544504 -71.9475470875
   ProjCam::Select $Page::Data(Frame) $Page::Data(Frame) Verif_UMOS

   Macro::Doing "Initializing config"
   if { ![colormap is VERIF_MAP] } {
      colormap create VERIF_MAP
      colormap read VERIF_MAP $env(HOME)/.spi/Colormap/REC_Col.std1.rgba
   }

   font create VERIF_FONT -family courier -size -12 -weight bold

   foreach pred $Param(Preds) anal $Param(Anals) {
      Macro::Doing "Reading UMOS $pred data"
      set obs [observation load $Param(PredPath)/$pred/$Param(Date).obs]

      foreach ob $obs {
         if { [observation configure $ob -desc]=="umos" } {

            Macro::Doing "Processing $pred"
            observation configure $ob -desc "UMOS $pred" -size 10 -icon CIRCLE -width 1 -outline black -colormap VERIF_MAP  \
               -rendertexture 1 -rendercontour 1 -font VERIF_FONT -min 0 -max 30

            set sec   [observation define $ob -DATE]
            set stamp [fstdstamp fromseconds $sec]
            set file  [clock format $sec -format "%Y%m%d%H_000" -gmt True]

            if { [file exists $Param(AnalPath)/$file] } {
               Macro::Doing "Reading 2D field $Param(AnalPath)/$file"
               fstdfile open VERIF_ANALFILE read $Param(AnalPath)/$file
               fstdfield read VERIF_ANALFIELD VERIF_ANALFILE $stamp "" { 1.0 HYBRID } -1 -1 "" $anal

               fstdfield configure VERIF_ANALFIELD -desc "2D data" -colormap VERIF_MAP -font VERIF_FONT -color black \
                  -rendertexture 1 -rendercontour 0 -mapall True -value INTEGER 0

               Macro::Doing "Creating product"
               Viewport::UnAssign $Page::Data(Frame) $Viewport::Data(VP)
               Viewport::Assign   $Page::Data(Frame) $Viewport::Data(VP) VERIF_ANALFIELD
               Viewport::Assign   $Page::Data(Frame) $Viewport::Data(VP) $ob

               DataBar::Create $Page::Data(Frame) $Viewport::Data(VP) 5 5 550 50 "UMOS verif valid for [clock format $sec -gmt True]"
               ColorBar::Create $Page::Data(Frame) $Viewport::Data(VP) 5 500 60 250
            }
         }
         observation free $obs
      }
   }

   Macro::Doing ""
   Macro::Cursor left_ptr

   #----- If in batch mode, print the map and exit
   if { $SPI::Param(Batch) } {
      PrintBox::Image $Page::Data(Frame) png Verif_UMOS
      SPI::Quit
   }
}

proc Macro::Verif_UMOS::Product { Pred Date Delta } {

}

proc Macro::Verif_UMOS::Clean { } {
   variable Data

   Viewport::UnAssign $Page::Data(Frame) $Viewport::Data(VP)

   catch {
      fstdfile close VERIF_ANALFILE
      fstdfield free VERIF_ANALFIELD

      colormap free VERIF_MAP
      font delete VERIF_FONT
   }
}

proc Macro::Verif_UMOS::Args { } {
   global argv argc
   variable Param

   #----- Lire les parametres si il y en a
   if { $argc>0 } { set Param(Date)  [lindex $argv 0] }
   if { $argc>1 } { set Param(Preds) [lindex $argv 1] }
}















