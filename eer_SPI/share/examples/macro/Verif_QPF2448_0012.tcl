#===============================================================================
# Environnement Canada
# Centre Meteorologique Canadian
# 2100 Trans-Canadienne
# Dorval, Quebec
#
# Projet   : Exemples de scripts de productions de cartes.
# Fichier  : Verif_QPF2448_0012.tcl
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

namespace eval Macro::Verif_QPF2448_0012 {} {
   variable Param
   variable Data
   variable Error

   set Param(In)   ""
   set Param(Intervals)  {1 5 10 15 20 30 40 50 75}

   set Param(Info) { "Verification des QPF 2448 avec\npointage des observation de pcpn de 24h."
                     "Verification of QPF 2448 width\npcpn observation plotting. " }

   set Data(Page) ""

   set Error(Field)  { "Aucun champs valide trouvé!\nVérifiez le viewport actif."
                       "Could not find any valid field!\nCheck the active viewport." }
}

proc Macro::Verif_QPF2448_0012::Execute { } {
   global GDefs env
   variable Data
   variable Error
   variable Param

   catch { set stamp   [exec ~afsfops/S/r.fnom] }
   catch { set stamp24 [exec ~afsfops/S/r.fnom 1] }
   catch { set stamp48 [exec ~afsfops/S/r.fnom 2] }

   SPI::LayoutLoad $Page::Data(Frame) VerifQPF24
   Macro::Cursor watch
   Macro::Doing "Creating Camera"
   ProjCam::Set VerifQPF24      { 0.0 0.0 1.0 } { 0.0 0.0 2.0 } { 0.0 1.0 0.0 } 2.42073170732 0 0 1 0 0 0 51.8451999041 -101.92600494
   ProjCam::Set VerifQPF24_WEST { 0.0 0.0 1.0 } { 0.0 0.0 2.0 } { 0.0 1.0 0.0 } 5.44317492889 0 0 1 0 0 0 49.3270542318 -118.581223592
   ProjCam::Set VerifQPF24_EAST { 0.0 0.0 1.0 } { 0.0 0.0 2.0 } { 0.0 1.0 0.0 } 6.72997751255 0 0 1 0 0 0 46.8057544504 -71.9475470875
   ProjCam::Select $Page::Data(Frame) $Page::Data(Frame) VerifQPF24

   Macro::Doing "Initializing config"
   if { ![colormap is VERIF_MAP] } {
      colormap create VERIF_MAP
      colormap read VERIF_MAP $env(HOME)/.spi/Colormap/REC_Col.std1.rgba
   }

   font create VERIF_FONT -family courier -size -12 -weight bold

   dataspec create VERIF_SPECSYNOP
   dataspec configure VERIF_SPECSYNOP -desc "SYNOP (${stamp}_pcp)" -size 10 -icon SQUARE -color black -colormap VERIF_MAP  \
      -mapall False -rendertexture 1 -rendercontour 1 -rendervalue 1 -font VERIF_FONT -intervals $Param(Intervals)

   dataspec create VERIF_SPECSHEF
   dataspec configure VERIF_SPECSHEF -desc "SHEF (${stamp}_)" -size 10 -icon CIRCLE -color black -colormap VERIF_MAP \
      -mapall True -rendertexture 1 -rendercontour 1 -rendervalue 1 -font XFont12 -intervals $Param(Intervals)

   Macro::Doing "Reading precip field"
   fstdfile open VERIF_REGFILE read $env(CMCGRIDF)/prog/regdiag/${stamp48}_048
   fstdfield read VERIF_REGFIELD VERIF_REGFILE -1 "" -1 48 24 P PR
   set valid [fstdstamp toseconds [fstdfield define VERIF_REGFIELD -DATEV]]

   fstdfield configure VERIF_REGFIELD -desc "reg GEM qpf 2448" -factor 1e3 -colormap VERIF_MAP -font VERIF_FONT -color black \
      -intervals $Param(Intervals) -rendertexture 0 -rendercontour 2 -mapall True -rendervalue 2 -value INTEGER 0


   Macro::Doing "Reading surface obs shef"
   metobs create VERIF_SHEF $env(CMCADE)/dbase/surface/shef/${stamp}_
   metobs define VERIF_SHEF -VALID $valid False
   metmodel define [metobs define VERIF_SHEF -MODEL] -items { { 0 0 13023 { } } } -spacing 10
   metmodel configure [metobs define VERIF_SHEF -MODEL] 13023 -dataspec VERIF_SPECSHEF


   Macro::Doing "Reading surface obs synop"
   metobs create VERIF_SYNOP $env(CMCADE)/dbase/surface/synop/${stamp}_pcp
   metobs define VERIF_SYNOP -VALID $valid False
   metmodel define [metobs define VERIF_SYNOP -MODEL] -items { { 0 0 13023 { } } } -spacing 10
   metmodel configure [metobs define VERIF_SYNOP -MODEL] 13023 -dataspec VERIF_SPECSYNOP

   Macro::Doing "Creating product"
   Viewport::UnAssign $Page::Data(Frame) $Viewport::Data(VP)
   Viewport::Assign   $Page::Data(Frame) $Viewport::Data(VP) VERIF_REGFIELD
   Viewport::Assign   $Page::Data(Frame) $Viewport::Data(VP) VERIF_SYNOP
   Viewport::Assign   $Page::Data(Frame) $Viewport::Data(VP) VERIF_SHEF

   DataBar::Create $Page::Data(Frame) $Viewport::Data(VP) 5 5 750 60 "QPF 2448 validation valid for ${stamp48} ($Param(Intervals))"

   Macro::Doing ""
   Macro::Cursor left_ptr

   Mapper::DepotWare::TMS::SelectLayer OpenStreetMap

   #----- If in batch mode, print the map and exit
   if { $SPI::Param(Batch) } {
      PrintBox::Image $Page::Data(Frame) png Verif_QPF2448_0012
      SPI::Quit
   }
}

proc Macro::Verif_QPF2448_0012::Clean { } {

   Viewport::UnAssign $Page::Data(Frame) $Viewport::Data(VP)
   font delete VERIF_FONT

   catch {
      fstdfile close VERIF_REGFILE
      fstdfield free VERIF_REGFIELD

      metobs free VERIF_SHEF
      metobs free VERIF_SYNOP
   }
}
