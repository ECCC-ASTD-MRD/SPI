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
   variable Param
   variable Data
   variable Error

   set Param(Pred)      ""
   set Param(Preds)     { spot3 wnd3s wnd3u wnd3v pop06 p0212 wnm6s wnm6u wnm6vo3sp3 n2sp3 p2sp3 clds3 }
   set Param(PredPath)  /data/cmdw/jmo/umos/reg/series
   set Param(Anals)     { TT wnd3s wnd3u wnd3v pop06 p0212 wnm6s wnm6u wnm6vo3sp3 n2sp3 p2sp3 clds3 }
   set Param(AnalPath)  /data/gridpt/dbase/anal/reghyb3
   set Param(Date)      2010051300

   set Param(Info)     { "Verification UMOS"
                         "Verification of UMOS" }
   set Param(InfoArgs) { { "Date des données (YYYMMDDHH)" } { "Date of data (YYYYMMDDHH)" } }

   set Data(Page)    ""
   set Data(Preds)   {}

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
   ProjCam::Set VerifUMOS      { 0.0 0.0 1.0 } { 0.0 0.0 2.0 } { 0.0 1.0 0.0 } 2.42073170732 0 0 1 0 0 0 51.8451999041 -101.92600494
   ProjCam::Set VerifUMOS_WEST { 0.0 0.0 1.0 } { 0.0 0.0 2.0 } { 0.0 1.0 0.0 } 5.44317492889 0 0 1 0 0 0 49.3270542318 -118.581223592
   ProjCam::Set VerifUMOS_EAST { 0.0 0.0 1.0 } { 0.0 0.0 2.0 } { 0.0 1.0 0.0 } 6.72997751255 0 0 1 0 0 0 46.8057544504 -71.9475470875
   ProjCam::Select $Page::Data(Frame) $Page::Data(Frame) VerifUMOS

   Macro::Doing "Initializing config"
   if { ![colormap is VERIF_MAP] } {
      colormap create VERIF_MAP
      colormap read VERIF_MAP $env(HOME)/.spi/Colormap/REC_Col.std1.rgba
   }

   font create VERIF_FONT -family courier -size -12 -weight bold

   Macro::Doing "Reading UMOS $Param(Pred) data"
   set Data(Preds) [observation load $Param(PredPath)/$Param(Pred)/$Param(Date).obs]

   foreach pred $Data(Preds) {
      Macro::Doing "Processing $pred"
      observation configure $prev -desc "UMOS $Param(Pred)" -size 10 -icon CIRCLE -width 1 -outline black -colormap VERIF_MAP  \
         -rendertexture 1 -rendercontour 1 -font VERIF_FONT -min 0 -max 300
      set sec   [observation define $pred -DATE]
      set stamp [fstdstamp fromseconds $sec]
      set file  [clock format $sec -format "%Y%m%d%H_000" -gmt True]

      if { [file exists $Param(Anal)/$file] } {
         Macro::Doing "Reading 2D field $Param(Anal)/$file"
         fstdfile open VERIF_ANALFILE read $Param(Anal)/$file
         fstdfield read VERIF_ANALFIELD VERIF_ANALFILE $stamp "" { 1.0 HYBRID } -1 -1 A TT

         fstdfield configure VERIF_ANALFIELD -desc "2D data" -colormap VERIF_MAP -font VERIF_FONT -color black \
             -rendertexture 1 -rendercontour 0 -mapall True -value INTEGER 0

         Macro::Doing "Creating product"
         Viewport::UnAssign $Page::Data(Frame) $Viewport::Data(VP)
         Viewport::Assign   $Page::Data(Frame) $Viewport::Data(VP) VERIF_ANALFIELD
         Viewport::Assign   $Page::Data(Frame) $Viewport::Data(VP) $prev

         DataBar::Create $Page::Data(Frame) $Viewport::Data(VP) 5 5 750 60 "UMOS verif valid for [clock format $sec -gmt True]"
      break
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

proc Macro::Verif_UMOS::Clean { } {
   variable Data

   Viewport::UnAssign $Page::Data(Frame) $Viewport::Data(VP)

   catch {
      fstdfile close VERIF_ANALFILE
      fstdfield free VERIF_ANALFIELD

      colormap free VERIF_MAP
      font delete VERIF_FONT

      observation free $Data(Preds)
   }
}

proc Macro::Verif_UMOS::Args { } {
   global argv argc

   #----- Lire les parametres si il y en a
   if { $argc>0 } { set Macro::Verif_UMOS::Param(Date) [lindex $argv 0] }
}















