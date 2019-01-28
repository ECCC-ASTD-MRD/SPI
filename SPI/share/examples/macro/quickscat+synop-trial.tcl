namespace eval Macro::quickscat+synop-trial {} {
   variable Param
   variable Data
   variable Error

   set Param(Intervals)  {  }
   set Param(T0)         00
   set Param(Run)        r1

   set Param(Info)      { "Vents du modele  vs  quickscat"  }
   set Param(InfoArgs)  { { "Heure"  } { "Hour" } }

   set Data(Page) ""
}

#########################################################
#Verifier les vents du trial REG avec pointage des quickscat
#########################################################

proc Macro::quickscat+synop-trial::Execute { } {
   global env
   variable Data
   variable Error
   variable Param

   switch $Param(Run) {
      "r1" {
         set mode reg
         switch $Param(T0) {
            "00" { set path    trial
                  set r       2
                  set stamp   [exec ~afsfops/S/r.fnom00]
                  set stamptr [exec ~afsfops/S/r.fnom18] }
            "06" { set path    trial
                  set r       2
                  set stamp   [exec ~afsfops/S/r.fnom06]
                  set stamptr [exec ~afsfops/S/r.fnom00] }
            "12" { set path    trial
                  set r       2
                  set stamp   [exec ~afsfops/S/r.fnom12]
                  set stamptr [exec ~afsfops/S/r.fnom06] }
            "18" { set path    prog     ;#Prend le prog de 6 
                  set r       ""
                  set stamp   [exec ~afsfops/S/r.fnom18]
                  set stamptr [exec ~afsfops/S/r.fnom12] }
         }
      }
      "g2" {
         set mode glb
         switch $Param(T0) {
            "00" { set path    trial
                  set r       2
                  set stamp   [exec ~afsfops/S/r.fnom00]
                  set stamptr [exec ~afsfops/S/r.fnom00 6] }
            "06" { set path    trial
                  set r       2
                  set stamp   [exec ~afsfops/S/r.fnom00+ 6]
                 set stamptr [exec ~afsfops/S/r.fnom00] }
            "12" { set path    trial
                  set r       2
                  set stamp   [exec ~afsfops/S/r.fnom12]
                  set stamptr [exec ~afsfops/S/r.fnom12 6] }
            "18" { set path   trial
                  set r       2
                  set stamp   [exec ~afsfops/S/r.fnom12+ 6]
                  set stamptr [exec ~afsfops/S/r.fnom12] }
         }             
      }
   }
   
   Macro::Cursor watch

   Macro::Doing "Initializing config"
   if { ![colormap is VERIF_MAP] } {
      colormap create VERIF_MAP -file $env(HOME)/.spi/Colormap/REC_Col.std1.rgba
   }

   font create VERIF_FONT -family courier -size -12 -weight bold

   dataspec create VERIF_SPECscat
   dataspec configure VERIF_SPECscat -desc "scat (${stamp}_sc)" -size 15 -color black -colormap VERIF_MAP  \
      -mapall 1 -factor 1.98  -rendertexture 0 -intervalmode  INTERVAL 5 -min 0 -max 60   -rendervector BARBULE  -font XFont18 -value INTEGER 0

   dataspec create VERIF_SPECSYNOPwnd
   dataspec configure VERIF_SPECSYNOPwnd -desc "SYNOP (${stamp})" -size 15 -icon NONE -color black -colormap VERIF_MAP  \
      -mapall 1 -factor 1.98 -intervalmode  INTERVAL 5 -min 0 -max 60  -rendertexture 0 -rendervector ARROW  -font XFont18 -value INTEGER 0

   #----- TRIAL : VENT
   Macro::Doing "Reading wind field"
   fstdfile open VERIF_GLBFILE read $env(CMCGRIDF)/$path/${mode}pres$r/${stamptr}_006
   fstdfield read VERIF_GLBFIELD VERIF_GLBFILE -1 "" -1 06 -1 P UU
   set valid [fstdstamp toseconds [fstdfield define VERIF_GLBFIELD -DATEV]]


  fstdfield configure VERIF_GLBFIELD -desc "REG GEM UV 06" -factor 1 -colormap VERIF_MAP -font XFont12 -color black \
        -rendertexture 0  -intervalmode  INTERVAL 5 -min 0 -max 60 -rendervector BARBULE -mapall 0 -rendervalue 1 -value INTEGER 0

   #----- TRIAL : PN
   Macro::Doing "Reading PN field"
   fstdfile open VERIF_GLBFILEpn read $env(CMCGRIDF)/$path/${mode}eta$r/${stamptr}_006
   fstdfield read VERIF_GLBFIELDpn VERIF_GLBFILEpn -1 "" -1 06 -1 P PN
   set valid [fstdstamp toseconds [fstdfield define VERIF_GLBFIELDpn -DATEV]]


  fstdfield configure VERIF_GLBFIELDpn -desc "REG GEM PN 06" -factor 1 -colormap VERIF_MAP -font XFont12 -color black \
   -rendertexture 0  -intervalmode  INTERVAL 4 -min 940 -max 1072 -rendertexture 0 -rendercontour 1 -mapall 0 -rendervalue 0 -value INTEGER 0

   #----- SCAT :
   Macro::Doing "Reading surface obs scat"
   metobs create VERIF_SCAT_wnd $env(CMCADE)/banco/derialt/$Param(Run)/${stamp}_sc
   metobs define VERIF_SCAT_wnd -VALID $valid False
   metmodel define [metobs define VERIF_SCAT_wnd -MODEL] -items { { 0 0 11012 11011 } } -spacing 10
   metmodel configure [metobs define VERIF_SCAT_wnd -MODEL] 11012 -dataspec VERIF_SPECscat


   Macro::Doing "Reading surface obs synop"
   metobs create VERIF_SYNOPwnd $env(CMCADE)/dbase/surface/synop/${stamp}_
   metobs define VERIF_SYNOPwnd -VALID $valid False
   metmodel define [metobs define VERIF_SYNOPwnd -MODEL] -items { { 0 0 11012 11011 } } -spacing 10
   metmodel configure [metobs define VERIF_SYNOPwnd -MODEL] 11012 -dataspec VERIF_SPECSYNOPwnd

   Macro::Doing "Creating product"
   Viewport::UnAssign $Page::Data(Frame) $Viewport::Data(VP)
   Viewport::Assign   $Page::Data(Frame) $Viewport::Data(VP) VERIF_GLBFIELD
   Viewport::Assign   $Page::Data(Frame) $Viewport::Data(VP) VERIF_GLBFIELDpn
   Viewport::Assign   $Page::Data(Frame) $Viewport::Data(VP) VERIF_SCAT_wnd 
   Viewport::Assign   $Page::Data(Frame) $Viewport::Data(VP) VERIF_SYNOPwnd

   DataBar::Create $Page::Data(Frame) $Viewport::Data(VP) 5 5 1000 60 "[string toupper $mode] trial P6h VENTS validation pour ${stamp} ($Param(Intervals))"

   Macro::Doing ""
   Macro::Cursor left_ptr
}

proc Macro::quickscat+synop-trial::Clean { } {

   Viewport::UnAssign $Page::Data(Frame) $Viewport::Data(VP)
   font delete VERIF_FONT

   catch {
      fstdfile close VERIF_GLBFILE
      fstdfield free VERIF_GLBFIELD

      metobs free VERIF_SCAT_wnd
   }
}

proc Macro::quickscat+synop-trial::Args { } {
   global argv argc
   variable Param

   if { $argc } {
      set Param(T0)  [lindex $argv 0]
      set Param(Run  [lindex $argv 1]
   }
}
