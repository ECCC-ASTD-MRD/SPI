#===============================================================================
# Environnement Canada
# Centre Meteorologique Canadian
# 2100 Trans-Canadienne
# Dorval, Quebec
#
# Projet   : Exemples de scripts de productions de cartes.
# Fichier  : WMOPlot.tcl
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

namespace eval Macro::WMOPlot {} {
   variable Param
   variable Data
   variable Error

   set Param(In)   ""
   set Param(Intervals)  {1 5 10 15 20 30 40 50 75}

   set Param(Info) { "WMO Plot."
                     "WMO Plot. " }

   set Data(Page) ""
}

proc Macro::WMOPlot::Execute { } {
   global GDefs env
   variable Data
   variable Error
   variable Param

   set stamp [exec ~afsfops/S/r.fnom]

   SPI::LayoutLoad $Page::Data(Frame) VerifQPF24
   Macro::Cursor watch
   Macro::Doing "Creating Camera"
   ProjCam::Set WMOPlot      { 0.0 0.0 1.0 } { 0.0 0.0 2.0 } { 0.0 1.0 0.0 } 2.42073170732 0 0 1 0 0 0 51.8451999041 -101.92600494
   ProjCam::Select $Page::Data(Frame) $Page::Data(Frame) VerifQPF24

   Macro::Doing "Initializing config"
   if { ![colormap is VERIF_MAP] } {
      colormap create VERIF_MAP
      colormap read VERIF_MAP $env(HOME)/.spi/Colormap/REC_Col.std1.rgba
   }

   font create VERIF_FONT -family courier -size -12 -weight bold

   dataspec create WMO_UV   -color black -wmo UV -size 12 -width 1.5 -rendervector BARBULE -factor 1.9438461717893492
   dataspec create WMO_WW   -color black -wmo WW
   dataspec create WMO_N    -color black -wmo N
   dataspec create WMO_CL   -color black -wmo CL
   dataspec create WMO_CM   -color black -wmo CM
   dataspec create WMO_CH   -color black -wmo CH
   dataspec create WMO_A    -color black -wmo A
   dataspec create WMO_AUTO -color black -wmo AUTO

   dataspec create WMO_TXT  -color black -font XFont10 -rendervalue 1 -value INTEGER 0
   dataspec create WMO_VIZ  -color black -font XFont10 -rendervalue 1 -value INTEGER 0 -factor 0.000539957
   dataspec create WMO_PPP  -color black -font XFont10 -rendervalue 1 -value INTEGER 0 -factor 0.1 -delta -100000
   dataspec create WMO_CPP  -color black -font XFont10 -rendervalue 1 -value INTEGER 0 -factor 0.1
   dataspec create WMO_TTT  -color black -font XFont10 -rendervalue 1 -value FLOAT 1 -delta -273.15
   dataspec create WMO_STN  -color black -font XFont10 -renderlabel 1

   set file [lindex [lsort -increasing [glob $env(CMCADE)/dbase/surface/metar/2*_]] end]
   Macro::Doing "Reading surface obs shef $file"
   metobs create METAR $file

   metobs define METAR -VALID [clock seconds] 0 -PERSISTANCE 10200

#[expr [clock scan "[string range 0 8 $stamp] [string range 9 end $stamp]"]+120] True

   set model [metobs define METAR -MODEL]
   metmodel define $model -items { {0 -3 10052 {}} {0 0 2001 {}} {0 0 11012 11011} {0 0 20206 {}} {-1 0 20229 {}} {0 1 20199 {}} {0 -1 20201 {}} {0 -2 20202 {}} \
       {2 0 10063 {}} {-2 -2 12017 {}} {-2 0 20001 {}} {-1 -2 12016 {}} {-1 1 12006 {}} {-1 -1 12004 {}} \
       {1 -1 10051 {}} {1 2 13019 {}} {1 0 10061 {}} } -spacing 20 -overspace 10 -flat True

   metmodel configure $model 10052 -dataspec WMO_STN
   metmodel configure $model 2001  -dataspec WMO_AUTO

   metmodel configure $model 11012 -dataspec WMO_UV
   metmodel configure $model 20206 -dataspec WMO_N
   metmodel configure $model 20229 -dataspec WMO_WW
   metmodel configure $model 20199 -dataspec WMO_CL
   metmodel configure $model 20201 -dataspec WMO_CM
   metmodel configure $model 20202 -dataspec WMO_CH
   metmodel configure $model 10063 -dataspec WMO_A

   metmodel configure $model 12017 -dataspec WMO_TTT
   metmodel configure $model 20001 -dataspec WMO_VIZ
   metmodel configure $model 12016 -dataspec WMO_TTT
   metmodel configure $model 12006 -dataspec WMO_TTT
   metmodel configure $model 12004 -dataspec WMO_TTT
   metmodel configure $model 10051 -dataspec WMO_PPP
   metmodel configure $model 10061 -dataspec WMO_CPP
   metmodel configure $model 13019 -dataspec WMO_TXT

   Macro::Doing "Creating product"
   Viewport::UnAssign $Page::Data(Frame) $Viewport::Data(VP)
   Viewport::Assign   $Page::Data(Frame) $Viewport::Data(VP) METAR

   Macro::Doing ""
   Macro::Cursor left_ptr

   #----- If in batch mode, print the map and exit
   if { $SPI::Param(Batch) } {
      PrintBox::Image $Page::Data(Frame) png WMOPlot
      SPI::Quit
   }
}

proc Macro::WMOPlot::Clean { } {

   Viewport::UnAssign $Page::Data(Frame) $Viewport::Data(VP)
   font delete VERIF_FONT

   catch {
      metobs free METAR
   }
}
