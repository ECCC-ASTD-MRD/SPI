namespace eval Macro::Verif_QPF {} {
   variable Param
   variable Data
   variable Error

   set Param(Intervals)  { 1 5 10 15 20 30 40 50 75 100 125 150 200 }
   set Param(T0)         00
   set Param(T1)         24
   set Param(Obs)        SHEF+SYNOP

   set Param(Info)      { "Verification des QPF avec\npointage des observation de pcpn de 24h."
                          "Verification of QPF width\npcpn observation plotting. " }
   set Param(InfoArgs)  { { "Heure 0" "Heure 1" "Observations (SHEF,SYNOP)" } { "Hour 0" "Hour 1" "Observations (SHEF,SYNOP)" } }

   set Data(Page) ""

   set Error(Field)  { "Aucun champs valide trouvé!\nVérifiez le viewport actif."
                       "Could not find any valid field!\nCheck the active viewport." }
}

proc Macro::Verif_QPF::Execute { } {
   global env
   variable Data
   variable Error
   variable Param

   set stamp00 [exec ~afsfops/S/r.fnom12]
   set stamp24 [exec ~afsfops/S/r.fnom12 1]
   set stamp48 [exec ~afsfops/S/r.fnom12 2]

   eval set stampt0 \${stamp$Param(T0)}
   eval set stampt1 \${stamp$Param(T1)}
   
   Macro::Cursor watch

   Viewport::UnAssign $Page::Data(Frame) $Viewport::Data(VP)

   Macro::Doing "Initializing config"
   if { ![colormap is VERIF_MAP] } {
      colormap create VERIF_MAP -file $env(HOME)/.spi/Colormap/REC_Col.std1.rgba
   }

   font create VERIF_FONT -family courier -size -12 -weight bold

   dataspec create VERIF_SPECSYNOP
   dataspec configure VERIF_SPECSYNOP -desc "SYNOP (${stampt0}_pcp)" -size 10 -icon SQUARE -color black -colormap VERIF_MAP  \
      -mapall False -rendertexture 1 -rendercontour 1 -rendervalue 1 -font VERIF_FONT -intervals $Param(Intervals)

   dataspec create VERIF_SPECDERISFC
   dataspec configure VERIF_SPECDERISFC -desc "DERISFC (${stampt0})" -size 10 -icon SQUARE -color black -colormap VERIF_MAP  \
      -mapall False -rendertexture 1 -rendercontour 1 -rendervalue 1 -font VERIF_FONT -intervals $Param(Intervals)

   dataspec create VERIF_SPECSHEF
   dataspec configure VERIF_SPECSHEF -desc "SHEF (${stampt0}_)" -size 10 -icon CIRCLE -color black -colormap VERIF_MAP \
      -mapall True -rendertexture 1 -rendercontour 1 -rendervalue 1 -font XFont12 -intervals $Param(Intervals)

   Macro::Doing "Reading precip field"
   fstdfile open VERIF_REGFILE read  $env(CMCGRIDF)/prog/regdiag/${stampt1}_[format "%03i" $Param(T1)]
   fstdfield read VERIF_REGFIELD VERIF_REGFILE -1 "" -1 $Param(T1) 24 P PR
   set valid [fstdstamp toseconds [fstdfield define VERIF_REGFIELD -DATEV]]

   fstdfield configure VERIF_REGFIELD -desc "reg GEM qpf $Param(T0)$Param(T1)" -factor 1e3 -colormap VERIF_MAP -font VERIF_FONT -color black \
      -intervals $Param(Intervals) -rendertexture 0 -rendercontour 1 -width 2 -mapall True -rendervalue 8 -value INTEGER 0

   if { [string first SHEF $Param(Obs)]!=-1 } {
      Macro::Doing "Reading surface obs shef"
      metobs create VERIF_SHEF $env(CMCADE)/dbase/surface/shef/${stampt0}_
      metobs define VERIF_SHEF -VALID $valid False
      metmodel define [metobs define VERIF_SHEF -MODEL] -items { { 0 0 13023 { } } } -spacing 10
      metmodel configure [metobs define VERIF_SHEF -MODEL] 13023 -dataspec VERIF_SPECSHEF
      
      Viewport::Assign   $Page::Data(Frame) $Viewport::Data(VP) VERIF_SHEF
   }
   
   if { [string first SYNOP $Param(Obs)]!=-1 } {
      Macro::Doing "Reading surface obs synop"
      metobs create VERIF_SYNOP_pcp24 $env(CMCADE)/dbase/surface/synop/${stampt0}_pcp
      metobs define VERIF_SYNOP_pcp24 -VALID $valid False
      metmodel define [metobs define VERIF_SYNOP_pcp24 -MODEL] -items { { 0 0 13023 { } } } -spacing 10
      metmodel configure [metobs define VERIF_SYNOP_pcp24 -MODEL] 13023 -dataspec VERIF_SPECSYNOP

      Macro::Doing "Reading surface obs synop"
      metobs create VERIF_DERISFC $env(CMCADE)/banco/derisfc/g3/${stampt0}_
      metobs define VERIF_DERISFC -VALID $valid False
      metmodel define [metobs define VERIF_DERISFC -MODEL] -items { { 0 0 13023 { } } } -spacing 10
      metmodel configure [metobs define VERIF_DERISFC -MODEL] 13023 -dataspec VERIF_SPECDERISFC

      Viewport::Assign   $Page::Data(Frame) $Viewport::Data(VP) { VERIF_SYNOP_pcp24 VERIF_DERISFC }
   }
   
   Viewport::Assign   $Page::Data(Frame) $Viewport::Data(VP) VERIF_REGFIELD

   DataBar::Create $Page::Data(Frame) $Viewport::Data(VP) 5 5 1000 100 "reggem QPF $Param(T0)$Param(T1) validation for ${stampt0} ($Param(Intervals))"

   Macro::Doing ""
   Macro::Cursor left_ptr
}

proc Macro::Verif_QPF::Clean { } {

   Viewport::UnAssign $Page::Data(Frame) $Viewport::Data(VP)
   font delete VERIF_FONT

   catch {
      fstdfile close VERIF_REGFILE
      fstdfield free VERIF_REGFIELD

      metobs free VERIF_SHEF
      metobs free VERIF_SYNOP_pcp24
      metobs free VERIF_DERISFC
   }
}

proc Macro::Verif_QPF::Args { } {
   global argv argc
   variable Param

   if { $argc } {
      set Param(T0)  [lindex $argv 0]
      set Param(T1)  [lindex $argv 1]
      set Param(Obs) [lindex $argv 2]
   }
}
