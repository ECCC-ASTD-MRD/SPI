#----------------------------------------------------------------------------
# Nom        : <MLDP::ScenarioNew>
# Creation   : 25 February 2003 - A. Malo - CMC/CMOE
#
# But        : Create new window for a specific source type
#              (accident release, volcano eruption, virus).
#
# Parametres :
#   <Parent> : Parent window.
#
# Retour     :
#
# Remarques  :
#
#----------------------------------------------------------------------------

proc MLDP::ScenarioNew { Parent } {
   variable Sim

   switch $Sim(SrcType) {
      "volcano"  { MLDP::ScenarioVolcanNew $Parent }
      "accident" { MLDP::ScenarioAccidentNew $Parent }
      "virus"    { MLDP::ScenarioVirusNew $Parent }
   }
}

#----------------------------------------------------------------------------
# Nom        : <MLDP::ScenarioValidate>
# Creation   : 22 March 2004 - A. Malo - CMC/CMOE
#
# But        : Validate release scenario for specific source type.
#
# Parametres :
#
# Retour     :
#   <Idx>    : Flag indicating if validation has succeeded (1) or not (0).
#
# Remarques  :
#
#----------------------------------------------------------------------------

proc MLDP::ScenarioValidate { } {
   variable Sim

   if { ![MLDP::ScenarioValidateName] } {
      return False
   }

   switch $Sim(SrcType) {
      "accident" {
         if { ![MLDP::ValidateReleaseRatesAccident] } {
            return False
         }

         if { ![MLDP::ValidateNbIsotopes] } {
            return False
         }

         if { ![MLDP::ValidateDurationsRatesAccident] } {
            return False
         }

         MLDP::ScenarioAccidentUpdateEmission

         if { ![MLDP::ScenarioAccidentValidateTotalReleasedQuantity] } {
            return False
         }
         if { ![MLDP::ValidateLullPeriodsStartAccident] } {
            return False
         }
         if { ![MLDP::ValidateLullPeriodsEndAccident] } {
            return False
         }
      }
      "volcano"   {
         if { ![MLDP::ValidateDurationsReleasesVolcano] } {
            return False
         }
         MLDP::ScenarioVolcanUpdateEmission

         if { ![MLDP::ValidateLullPeriodsStartVolcan] } {
            return False
         }
         if { ![MLDP::ValidateLullPeriodsEndVolcan] } {
            return False
         }
      }
      "virus"    {
         if { ![MLDP::ValidateReleaseRatesVirus] } {
            return False
         }
         if { ![MLDP::ValidateDurationsRatesVirus] } {
            return False
         }

         MLDP::ScenarioVirusUpdateEmission

         if { ![MLDP::ScenarioVirusValidateTotalReleasedQuantity] } {
            return False
         }
         if { ![MLDP::ValidateLullPeriodsStartVirus] } {
            return False
         }
         if { ![MLDP::ValidateLullPeriodsEndVirus] } {
            return False
         }
      }
   }
   return True
}

#----------------------------------------------------------------------------
# Nom        : <MLDP::ScenarioValidateNameo>
# Creation   : 22 March 2004 - A. Malo - CMC/CMOE
#
# But        : Validate name of release scenario.
#
# Parametres :
#
# Retour     :
#   <Idx>    : Flag indicating if validation has succeeded (1) or not (0).
#
# Remarques  :
#
#----------------------------------------------------------------------------

proc MLDP::ScenarioValidateName { } {
   global GDefs
   variable Tmp
   variable Error

   if { $Tmp(Scenario)=="" } {
      Dialog::CreateError .newscenario $Error(ScenarioName)
      focus .newscenario.emint.fr.box1.data.name.ent
      return False
   }
   return True
}

#----------------------------------------------------------------------------
# Nom        : <MLDP::ScenarioValidateDurations>
# Creation   : 22 March 2004 - A. Malo - CMC/CMOE
#
# But        : Validate durations for each release interval.
#
# Parametres :
#
# Retour     :
#   <Idx>    : Flag indicating if validation has succeeded (1) or not (0).
#
# Remarques  :
#
#----------------------------------------------------------------------------

proc MLDP::ScenarioValidateDurations { } {
   variable Sim
   variable Tmp

   for { set i 0 } { $i < $Sim(EmMaxInterval) } { incr i } {
      if { ![MLDP::ValidateDurationValue $Tmp(Duration$i) $Parent $i] } {
         focus $MLDP::Sim(ReleaseRatesFrame).entry$i.dur
         return False
      }
   }
   return True
}

#----------------------------------------------------------------------------
# Nom        : <MLDP::ScenarioAccidentNew>
# Creation   : 25 February 2003 - A. Malo - CMC/CMOE
#
# But        : Create new window to edit a selected (or add a new)
#              emission scenario for an accident release.
#
# Parametres :
#   <Parent> : Parent window.
#
# Retour     :
#
# Remarques  :
#
#----------------------------------------------------------------------------

proc MLDP::ScenarioAccidentNew { Parent } {
   global GDefs
   variable Sim
   variable Tmp
   variable Msg
   variable Lbl
   variable Bubble

   toplevel     .newscenario
   wm title     .newscenario "MLDP: $Exp::Data(Name) - [lindex $Lbl(Scenario) $GDefs(Lang)]"
   wm transient .newscenario $Parent
   wm resizable .newscenario 0 0
   wm geom      .newscenario =715x435+[expr [winfo rootx $Parent]+330]+[expr [winfo rooty $Parent]+30]
   wm protocol  .newscenario WM_DELETE_WINDOW { }

   #----- Initialize variables for new accident type release scenario.
   MLDP::ScenarioAccidentInit

   #----- Emission scenario.
   frame .newscenario.emint

   labelframe .newscenario.emint.fr -text "[lindex $Lbl(Scenario) $GDefs(Lang)]"
   Bubble::Create .newscenario.emint.fr     "[lindex $Bubble(Scenario) $GDefs(Lang)]"

   frame .newscenario.emint.fr.box1
   frame .newscenario.emint.fr.box1.data

   #----- Release scenario name.
   frame .newscenario.emint.fr.box1.data.name
      label .newscenario.emint.fr.box1.data.name.lbl -relief flat -anchor w -text "[lindex $Lbl(ScenarioName) $GDefs(Lang)]"
      entry .newscenario.emint.fr.box1.data.name.ent -relief sunken -bd 1 -bg $GDefs(ColorLight) -textvariable MLDP::Tmp(Scenario) -width 20
      pack .newscenario.emint.fr.box1.data.name.lbl .newscenario.emint.fr.box1.data.name.ent -side left -padx 2
   pack .newscenario.emint.fr.box1.data.name -side top -anchor w -padx 2 -fill x
   Bubble::Create .newscenario.emint.fr.box1.data.name "[lindex $Bubble(ScenarioName) $GDefs(Lang)]"
   set MLDP::Sim(ScenarioNameEntry) .newscenario.emint.fr.box1.data.name.ent

   #----- Total emission duration [s].
   frame .newscenario.emint.fr.box1.data.totdur
      label .newscenario.emint.fr.box1.data.totdur.lbl -relief flat -anchor w -text "[lindex $Lbl(TotalDuration) $GDefs(Lang)]"
      entry .newscenario.emint.fr.box1.data.totdur.ent -relief sunken -bd 1 -textvariable MLDP::Tmp(TotalDuration) -width 20 -state disabled
      pack .newscenario.emint.fr.box1.data.totdur.lbl .newscenario.emint.fr.box1.data.totdur.ent -side left -padx 2
   pack .newscenario.emint.fr.box1.data.totdur -side top -anchor w -padx 2 -fill x
   Bubble::Create .newscenario.emint.fr.box1.data.totdur "[lindex $Bubble(TotalDuration2) $GDefs(Lang)]"

   #----- Effective emission duration [s].
   frame .newscenario.emint.fr.box1.data.effdur
      label .newscenario.emint.fr.box1.data.effdur.lbl -relief flat -anchor w -text "[lindex $Lbl(EffectiveDuration) $GDefs(Lang)]"
      entry .newscenario.emint.fr.box1.data.effdur.ent -relief sunken -bd 1 -textvariable MLDP::Tmp(EffectiveDuration) -width 20 -state disabled
      pack .newscenario.emint.fr.box1.data.effdur.lbl .newscenario.emint.fr.box1.data.effdur.ent -side left -padx 2
   pack .newscenario.emint.fr.box1.data.effdur -side top -anchor w -padx 2 -fill x
   Bubble::Create .newscenario.emint.fr.box1.data.effdur "[lindex $Bubble(EffectiveDuration2) $GDefs(Lang)]"

   pack .newscenario.emint.fr.box1.data -side left
   pack .newscenario.emint.fr.box1 -side top


   frame .newscenario.emint.fr.box2 -bd 1 -relief sunken
   set MLDP::Sim(ReleaseRatesFrame) .newscenario.emint.fr.box2

   #----- Headers.

   frame .newscenario.emint.fr.box2.header
      label .newscenario.emint.fr.box2.header.dur -relief raised -width 20 -bd 1 -text "[lindex $Lbl(Duration) $GDefs(Lang)]"
      pack .newscenario.emint.fr.box2.header.dur -side left -fill x -fill y
      Bubble::Create .newscenario.emint.fr.box2.header.dur "[lindex $Bubble(DurationInter) $GDefs(Lang)]"

      frame .newscenario.emint.fr.box2.header.rates
         label .newscenario.emint.fr.box2.header.rates.lbl -relief raised -height 3 -width 40 -bd 1 -text "[lindex $Lbl(ReleaseRateAccident) $GDefs(Lang)]"
         button .newscenario.emint.fr.box2.header.rates.add -text "[lindex $Lbl(Add) $GDefs(Lang)]" -relief raised -bd 1 -width 10 -state active \
            -command "IsoBox::Create .newscenario MLDP::SpeciesFormat"
         pack .newscenario.emint.fr.box2.header.rates.lbl .newscenario.emint.fr.box2.header.rates.add -side top -fill x -expand true
         Bubble::Create .newscenario.emint.fr.box2.header.rates.add "[lindex $Bubble(AddIso) $GDefs(Lang)]"
         Bubble::Create .newscenario.emint.fr.box2.header.rates.lbl "[lindex $Bubble(ReleaseRate) $GDefs(Lang)]"

         frame .newscenario.emint.fr.box2.header.rates.iso
         set MLDP::Sim(IsotopeLabels) .newscenario.emint.fr.box2.header.rates.iso
         for { set j 0 } { $j < $Sim(EmMaxIso) } { incr j } {
            button .newscenario.emint.fr.box2.header.rates.iso.name$j -relief raised -width 11 -bd 1 -textvariable MLDP::Tmp(Iso$j) \
               -command "MLDP::SpeciesDelete $j"
            label .newscenario.emint.fr.box2.header.rates.iso.kill$j -bitmap @$GDefs(Dir)/Resources/Bitmap/cvdel.xbm -bd 0
            pack .newscenario.emint.fr.box2.header.rates.iso.name$j -side left -ipadx 2 -fill x -expand true
            place .newscenario.emint.fr.box2.header.rates.iso.kill$j -in .newscenario.emint.fr.box2.header.rates.iso.name$j \
               -relx 1 -rely 0 -anchor ne -width 15 -height 15
            Bubble::Create .newscenario.emint.fr.box2.header.rates.iso.name$j "[lindex $Bubble(DeleteIso) $GDefs(Lang)]"
         }
         pack .newscenario.emint.fr.box2.header.rates.iso -side top -fill x -expand true
      pack .newscenario.emint.fr.box2.header.rates -side left -fill x -expand true
   pack .newscenario.emint.fr.box2.header -side top -anchor w -fill x -expand true

   #----- Entries.

   for { set i 0 } { $i < $Sim(EmMaxInterval) } { incr i } {

      frame .newscenario.emint.fr.box2.entry$i
         entry .newscenario.emint.fr.box2.entry$i.dur -relief sunken -width 20 -bd 1 -bg $GDefs(ColorLight) -textvariable MLDP::Tmp(Duration$i)
         pack .newscenario.emint.fr.box2.entry$i.dur -side left
         Bubble::Create .newscenario.emint.fr.box2.entry$i.dur "[lindex $Bubble(DurationInter) $GDefs(Lang)]"

      frame .newscenario.emint.fr.box2.entry$i.rates
         for { set j 0 } { $j < $Sim(EmMaxIso) } { incr j } {
            entry .newscenario.emint.fr.box2.entry$i.rates.iso$j -relief sunken -width 15 -bd 1 -bg $GDefs(ColorLight) -textvariable MLDP::Tmp(ReleaseRate$i.$j)
            pack .newscenario.emint.fr.box2.entry$i.rates.iso$j -side left
         }

      pack .newscenario.emint.fr.box2.entry$i.rates -side left -fill y
      pack .newscenario.emint.fr.box2.entry$i -side top -anchor w -fill x -expand true
      Bubble::Create .newscenario.emint.fr.box2.entry$i.rates "[lindex $Bubble(ReleaseRate2) $GDefs(Lang)]"
   }

   #----- Total released quantity.
   frame .newscenario.emint.fr.box2.quant -relief raised -bd 1
      label .newscenario.emint.fr.box2.quant.lbl -width 20 -pady 4 -bd 0 -text "[lindex $Lbl(TotalQuantity) $GDefs(Lang)]"
      pack .newscenario.emint.fr.box2.quant.lbl -side left -fill x -fill y

      frame .newscenario.emint.fr.box2.quant.tot
      for { set j 0 } { $j < $Sim(EmMaxIso) } { incr j } {
         entry .newscenario.emint.fr.box2.quant.tot.iso$j -relief sunken -width 15 -bd 1 -textvariable MLDP::Tmp(ReleaseQuantity$j) -state disabled
         pack .newscenario.emint.fr.box2.quant.tot.iso$j -side left -fill y
      }
      pack .newscenario.emint.fr.box2.quant.tot -side left -fill y
   pack .newscenario.emint.fr.box2.quant -side top -anchor w -fill x -fill y -expand true
   Bubble::Create .newscenario.emint.fr.box2.quant "[lindex $Bubble(TotalQuantityAccident) $GDefs(Lang)]"

   pack .newscenario.emint.fr.box2 -side top -anchor w -padx 5 -pady 10
   pack .newscenario.emint.fr -side top -anchor w -fill x
   pack .newscenario.emint -side top -anchor w -padx 5 -pady 5 -fill x

   frame .newscenario.button
      button .newscenario.button.cancel -text "[lindex $Lbl(Cancel) $GDefs(Lang)]" -bd 1 -command { destroy .newscenario }
      button .newscenario.button.apply -text "[lindex $Lbl(Apply) $GDefs(Lang)]" -bd 1 -command "MLDP::EmissionUpdate"
      pack .newscenario.button.apply .newscenario.button.cancel -side right
   pack .newscenario.button -side top -anchor w -padx 7 -fill x
   Bubble::Create .newscenario.button.cancel "[lindex $Bubble(Cancel) $GDefs(Lang)]"
   Bubble::Create .newscenario.button.apply "[lindex $Bubble(Apply) $GDefs(Lang)]"

   grab .newscenario
}

#----------------------------------------------------------------------------
# Nom        : <MLDP::ScenarioAccidentInit>
# Creation   : 25 February 2003 - A. Malo - CMC/CMOE
#
# But        : Initialize variables for new accident type release scenario.
#
# Parametres :
#
# Retour     :
#
# Remarques  :
#
#----------------------------------------------------------------------------

proc MLDP::ScenarioAccidentInit { } {
   variable Tmp
   variable Sim

   #----- Reset variables.

   for { set i 0 } { $i < $Sim(EmMaxInterval) } { incr i } {
      set Tmp(Duration$i) ""
      for { set j 0 } { $j < $Sim(EmMaxIso) } { incr j } {
         set Tmp(ReleaseRate$i.$j)  ""
      }
   }

   for { set j 0 } { $j < $Sim(EmMaxIso) } { incr j } {
      set Tmp(ReleaseQuantity$j) ""
      set Tmp(Iso$j)             ""
   }

   #----- Initialize variables.

   set Tmp(Scenario)          $Sim(EmScenario)
   set Tmp(NbIntervals)       $Sim(EmNbIntervals)
   set Tmp(TotalDuration)     $Sim(EmTotalDuration)
   set Tmp(EffectiveDuration) $Sim(EmEffectiveDuration)
   set Tmp(Iso)               $Sim(EmIso.$Sim(EmScenario))
   set Tmp(Inter)             $Sim(EmInter.$Sim(EmScenario))

   set i 0
   foreach inter $Tmp(Inter) {
      set Tmp(Duration$i) [lindex $inter 0]
      for { set j 0 } { $j < [llength $Tmp(Iso)] } { incr j } {
         set Tmp(ReleaseRate$i.$j) [lindex $inter [expr $j+1]]
      }
      incr i
   }

   for { set j 0 } { $j < [llength $Tmp(Iso)] } { incr j } {
      set Tmp(Iso$j) [lindex [lindex $Tmp(Iso) $j] 0]
   }

   #----- Update (total and effective) emission durations and total released quantities for each isotope.
   MLDP::ScenarioAccidentUpdateEmission
}

#----------------------------------------------------------------------------
# Nom        : <MLDP::ScenarioAccidentUpdateTotalQuantity>
# Creation   : 22 March 2004 - A. Malo - CMC/CMOE
#
# But        : Update (total and effective) emission durations and
#              total released quantities for each isotope.
#
# Parametres :
#
# Retour     :
#
# Remarques  :
#
#----------------------------------------------------------------------------

proc MLDP::ScenarioAccidentUpdateEmission { } {
   variable Sim
   variable Tmp
   variable Quant

   #----- Initialize variables.
   set total     0
   set effective 0
   set nbint     0

   for { set j 0 } { $j < [llength $Tmp(Iso)] } { incr j } {
      set Quant($j) 0
   }

   #----- Loop over number of emission durations.
   for { set i 0 } { $i < $Sim(EmMaxInterval) } { incr i } {

      set j 0

      if { $Tmp(Duration$i) != "" && $Tmp(Duration$i) > 0 } {

         #----- Increment total duration by emission duration value.
         set total                      [expr $total + $Tmp(Duration$i)]
         set hasFoundPositiveRate       0
         set hasFoundPositiveOrNullRate 0

         #----- Loop over isotopes.
         for { set j 0 } { $j < [llength $Tmp(Iso)] } { incr j } {

            if { $Tmp(ReleaseRate$i.$j) != "" && $Tmp(ReleaseRate$i.$j) >= 0.0 } {

               if { !$hasFoundPositiveRate } {
                  if { $Tmp(ReleaseRate$i.$j) > 0.0 } {
                     #----- Increment effective duration by emission duration value.
                     set effective            [expr $effective + $Tmp(Duration$i)]
                     set hasFoundPositiveRate 1
                  }
               }

               if { !$hasFoundPositiveOrNullRate } {
                  if { $Tmp(ReleaseRate$i.$j) >= 0.0 } {
                     #----- Increment number of emission intervals.
                     incr nbint
                     set hasFoundPositiveOrNullRate 1
                  }
               }

               #----- Compute total released quantity.
               set Quant($j) [expr $Quant($j) + double($Tmp(Duration$i))/3600.0 * double($Tmp(ReleaseRate$i.$j))]
            }
         }
      }

      #-----  Clear release rates fields for remaining empty isotpes buttons.
      for { set j $j } { $j < $Sim(EmMaxIso) } { incr j } {
         set Tmp(ReleaseRate$i.$j) ""
      }
   }

   #----- Clear total release quantity fields for remaining empty isotpes buttons.
   for { set j [llength $Tmp(Iso)] } { $j < $Sim(EmMaxIso) } { incr j } {
      set Tmp(ReleaseQuantity$j) ""
   }

   #----- Update total and effective emission durations, number of intervals and total released quantities.
   set Tmp(TotalDuration)     $total
   set Tmp(EffectiveDuration) $effective
   set Tmp(NbIntervals)       $nbint

   for { set j 0 } { $j < [llength $Tmp(Iso)] } { incr j } {
      set Tmp(ReleaseQuantity$j) [format "%.7e" $Quant($j)]
   }
}

#----------------------------------------------------------------------------
# Nom        : <MLDP::ScenarioVirusNew>
# Creation   : 3 March 2004 - A. Malo - CMC/CMOE
#
# But        : Create new window to edit a selected (or add a new)
#              emission scenario for a virus release.
#
# Parametres :
#   <Parent> : Parent window.
#
# Retour     :
#
# Remarques  :
#
#----------------------------------------------------------------------------

proc MLDP::ScenarioVirusNew { Parent } {
   global GDefs
   variable Sim
   variable Tmp
   variable Msg
   variable Lbl
   variable Bubble

   toplevel     .newscenario
   wm title     .newscenario "MLDP: $Exp::Data(Name) - [lindex $Lbl(Scenario) $GDefs(Lang)]"
   wm transient .newscenario $Parent
   wm resizable .newscenario 0 0
   wm geom      .newscenario =320x385+[expr [winfo rootx $Parent]+330]+[expr [winfo rooty $Parent]+30]
   wm protocol  .newscenario WM_DELETE_WINDOW { }

   #----- Initialize variables for new virus type release scenario.
   MLDP::ScenarioVirusInit

   #----- Emission scenario.
   frame .newscenario.emint

   labelframe .newscenario.emint.fr -text "[lindex $Lbl(Scenario) $GDefs(Lang)]"
   Bubble::Create .newscenario.emint.fr     "[lindex $Bubble(Scenario) $GDefs(Lang)]"

   frame .newscenario.emint.fr.box1
   frame .newscenario.emint.fr.box1.data

   #----- Release scenario name.
   frame .newscenario.emint.fr.box1.data.name
      label .newscenario.emint.fr.box1.data.name.lbl -relief flat -anchor w -text "[lindex $Lbl(ScenarioName) $GDefs(Lang)]"
      entry .newscenario.emint.fr.box1.data.name.ent -relief sunken -bd 1 -bg $GDefs(ColorLight) -textvariable MLDP::Tmp(Scenario) -width 20
      pack .newscenario.emint.fr.box1.data.name.lbl .newscenario.emint.fr.box1.data.name.ent -side left -padx 2
   pack .newscenario.emint.fr.box1.data.name -side top -anchor w -padx 2 -fill x
   Bubble::Create .newscenario.emint.fr.box1.data.name "[lindex $Bubble(ScenarioName) $GDefs(Lang)]"
   set MLDP::Sim(ScenarioNameEntry) .newscenario.emint.fr.box1.data.name.ent

   #----- Total emission duration [s].
   frame .newscenario.emint.fr.box1.data.totdur
      label .newscenario.emint.fr.box1.data.totdur.lbl -relief flat -anchor w -text "[lindex $Lbl(TotalDuration) $GDefs(Lang)]"
      entry .newscenario.emint.fr.box1.data.totdur.ent -relief sunken -bd 1 -textvariable MLDP::Tmp(TotalDuration) -width 20 -state disabled
      pack .newscenario.emint.fr.box1.data.totdur.lbl .newscenario.emint.fr.box1.data.totdur.ent -side left -padx 2
   pack .newscenario.emint.fr.box1.data.totdur -side top -anchor w -padx 2 -fill x
   Bubble::Create .newscenario.emint.fr.box1.data.totdur "[lindex $Bubble(TotalDuration2) $GDefs(Lang)]"

   #----- Effective emission duration [s].
   frame .newscenario.emint.fr.box1.data.effdur
      label .newscenario.emint.fr.box1.data.effdur.lbl -relief flat -anchor w -text "[lindex $Lbl(EffectiveDuration) $GDefs(Lang)]"
      entry .newscenario.emint.fr.box1.data.effdur.ent -relief sunken -bd 1 -textvariable MLDP::Tmp(EffectiveDuration) -width 20 -state disabled
      pack .newscenario.emint.fr.box1.data.effdur.lbl .newscenario.emint.fr.box1.data.effdur.ent -side left -padx 2
   pack .newscenario.emint.fr.box1.data.effdur -side top -anchor w -padx 2 -fill x
   Bubble::Create .newscenario.emint.fr.box1.data.effdur "[lindex $Bubble(EffectiveDuration2) $GDefs(Lang)]"

   pack .newscenario.emint.fr.box1.data -side left
   pack .newscenario.emint.fr.box1 -side top

   frame .newscenario.emint.fr.box2 -bd 1 -relief sunken
   set MLDP::Sim(ReleaseRatesFrame) .newscenario.emint.fr.box2

   #----- Headers.
   frame .newscenario.emint.fr.box2.header
      label .newscenario.emint.fr.box2.header.dur -relief raised -width 20 -bd 1 -text "[lindex $Lbl(Duration) $GDefs(Lang)]"
      pack .newscenario.emint.fr.box2.header.dur -side left -fill x -fill y
      Bubble::Create .newscenario.emint.fr.box2.header.dur "[lindex $Bubble(DurationInter) $GDefs(Lang)]"

      label .newscenario.emint.fr.box2.header.rates -relief raised -height 3 -width 30 -bd 1 -text "[lindex $Lbl(ReleaseRateVirus) $GDefs(Lang)]"
      pack .newscenario.emint.fr.box2.header.rates -side left -fill x -expand true
      Bubble::Create .newscenario.emint.fr.box2.header.rates "[lindex $Bubble(ReleaseRateVirus) $GDefs(Lang)]"
   pack .newscenario.emint.fr.box2.header -side top -anchor w -fill x -expand true

   #----- Entries.
   for { set i 0 } { $i < $Sim(EmMaxInterval) } { incr i } {

       frame .newscenario.emint.fr.box2.entry$i
          entry .newscenario.emint.fr.box2.entry$i.dur -relief sunken -width 20 -bd 1 -bg $GDefs(ColorLight) -textvariable MLDP::Tmp(Duration$i)
          pack .newscenario.emint.fr.box2.entry$i.dur -side left
          Bubble::Create .newscenario.emint.fr.box2.entry$i.dur "[lindex $Bubble(DurationInter) $GDefs(Lang)]"

          entry .newscenario.emint.fr.box2.entry$i.rates -relief sunken -width 20 -bd 1 -bg $GDefs(ColorLight) -textvariable MLDP::Tmp(ReleaseRate$i)
          pack .newscenario.emint.fr.box2.entry$i.rates -side left -fill x -expand true
          Bubble::Create .newscenario.emint.fr.box2.entry$i.rates "[lindex $Bubble(ReleaseRateVirus) $GDefs(Lang)]"
       pack .newscenario.emint.fr.box2.entry$i -side top -anchor w -fill x -expand true

    }

   #----- Total released quantity.
   frame .newscenario.emint.fr.box2.quant -relief raised -bd 1
      label .newscenario.emint.fr.box2.quant.lbl -width 20 -pady 4 -bd 0 -text "[lindex $Lbl(TotalQuantity) $GDefs(Lang)]"
      pack .newscenario.emint.fr.box2.quant.lbl -side left -fill x -fill y

      entry .newscenario.emint.fr.box2.quant.tot -relief sunken -width 30 -bd 1 -textvariable MLDP::Tmp(ReleaseQuantity) -state disabled
      pack .newscenario.emint.fr.box2.quant.tot -side left -fill y

    pack .newscenario.emint.fr.box2.quant -side top -anchor w -fill x -fill y -expand true
    Bubble::Create .newscenario.emint.fr.box2.quant "[lindex $Bubble(TotalQuantityVirus) $GDefs(Lang)]"

    pack .newscenario.emint.fr.box2 -side top -anchor w -padx 5 -pady 10
    pack .newscenario.emint.fr -side top -anchor w -fill x
    pack .newscenario.emint -side top -anchor w -padx 5 -pady 5 -fill x

    frame .newscenario.button
       button .newscenario.button.cancel -text "[lindex $Lbl(Cancel) $GDefs(Lang)]" -bd 1 -command { destroy .newscenario }
       button .newscenario.button.apply -text "[lindex $Lbl(Apply) $GDefs(Lang)]" -bd 1 -command "MLDP::EmissionUpdate"
       pack .newscenario.button.apply .newscenario.button.cancel -side right
    pack .newscenario.button -side top -anchor w -padx 7 -fill x
    Bubble::Create .newscenario.button.cancel "[lindex $Bubble(Cancel) $GDefs(Lang)]"
    Bubble::Create .newscenario.button.apply "[lindex $Bubble(Apply) $GDefs(Lang)]"

   grab .newscenario
}

#----------------------------------------------------------------------------
# Nom        : <MLDP::ScenarioVirusInit>
# Creation   : 3 March 2004 - A. Malo - CMC/CMOE
#
# But        : Initialize variables for new virus type release scenario.
#
# Parametres :
#
# Retour     :
#
# Remarques  :
#
#----------------------------------------------------------------------------

proc MLDP::ScenarioVirusInit { } {
   variable Tmp
   variable Sim

   #----- Reset variables.
   for { set i 0 } { $i < $Sim(EmMaxInterval) } { incr i } {
      set Tmp(Duration$i)     ""
      set Tmp(ReleaseRate$i)  ""
   }
   set Tmp(ReleaseQuantity) ""

   #----- Initialize variables.
   set Tmp(Scenario)          $Sim(EmScenario)
   set Tmp(NbIntervals)       $Sim(EmNbIntervals)
   set Tmp(TotalDuration)     $Sim(EmTotalDuration)
   set Tmp(EffectiveDuration) $Sim(EmEffectiveDuration)
   set Tmp(Iso)               $Sim(EmIso.$Sim(EmScenario))
   set Tmp(Inter)             $Sim(EmInter.$Sim(EmScenario))

   set i 0
   foreach inter $Tmp(Inter) {
      set Tmp(Duration$i)    [lindex $inter 0]
      set Tmp(ReleaseRate$i) [lindex $inter 1]
      incr i
   }

   #----- Update (total and effective) emission durations and total released quantity.
   MLDP::ScenarioVirusUpdateEmission
}

#----------------------------------------------------------------------------
# Nom        : <MLDP::ScenarioVirusUpdateEmission>
# Creation   : 22 March 2004 - A. Malo - CMC/CMOE
#
# But        : Update (total and effective) emission durations and
#              total released quantity.
#
# Parametres :
#
# Retour     :
#
# Remarques  :
#
#----------------------------------------------------------------------------

proc MLDP::ScenarioVirusUpdateEmission { } {
   variable Sim
   variable Tmp

   #----- Initialize variables.
   set total     0
   set effective 0
   set nbint     0
   set quant     0

   #----- Compute total and effective emission durations, number of intervals and total released quantity.
   for { set i 0 } { $i < $Sim(EmMaxInterval) } { incr i } {

      if { $Tmp(Duration$i) != "" && $Tmp(Duration$i) > 0 } {

         if { $Tmp(ReleaseRate$i) != "" && $Tmp(ReleaseRate$i) > 0.0 } {
            #----- Increment effective duration by emission duration value.
            set effective [expr $effective + $Tmp(Duration$i)]

            #----- Compute total released quantity.
            set quant [expr $quant + double($Tmp(Duration$i))/3600.0 * double($Tmp(ReleaseRate$i))]
         }

         if { $Tmp(ReleaseRate$i) != "" && $Tmp(ReleaseRate$i) >= 0.0 } {
            #----- Increment total duration by emission duration value.
            set total [expr $total + $Tmp(Duration$i)]

            #----- Increment number of emission intervals.
            incr nbint
         }
      }
   }

   #----- Update total and effective emission durations, number of intervals and total released quantity.
   set Tmp(TotalDuration)     $total
   set Tmp(EffectiveDuration) $effective
   set Tmp(NbIntervals)       $nbint
   set Tmp(ReleaseQuantity)   [format "%.7e" $quant]
}

#----------------------------------------------------------------------------
# Nom        : <MLDP::VolcanScenarioNew>
# Creation   : 25 February 2003 - A. Malo - CMC/CMOE
#
# But        : Create new window to edit a selected (or add a new)
#              emission scenario for a volcano eruption.
#
# Parametres :
#   <Parent> : Parent window.
#
# Retour     :
#
# Remarques  :
#
#----------------------------------------------------------------------------

proc MLDP::ScenarioVolcanNew { Parent } {
   global GDefs
   variable Sim
   variable Tmp
   variable Msg
   variable Lbl
   variable Bubble

   toplevel     .newscenario
   wm title     .newscenario "MLDP: $Exp::Data(Name) - [lindex $Lbl(Scenario) $GDefs(Lang)]"
   wm transient .newscenario $Parent
   wm resizable .newscenario 0 0
   wm geom      .newscenario =320x370+[expr [winfo rootx $Parent]+330]+[expr [winfo rooty $Parent]+30]
   wm protocol  .newscenario WM_DELETE_WINDOW { }

   #----- Initialize variables for new volcano type release scenario.
   MLDP::ScenarioVolcanInit

   frame .newscenario.emint
   labelframe .newscenario.emint.fr -text "[lindex $Lbl(Scenario) $GDefs(Lang)]"

   frame .newscenario.emint.fr.box1
   frame .newscenario.emint.fr.box1.data

   #----- Release scenario name.
   frame .newscenario.emint.fr.box1.data.name
      label .newscenario.emint.fr.box1.data.name.lbl -relief flat -anchor w -text "[lindex $Lbl(ScenarioName) $GDefs(Lang)]"
      entry .newscenario.emint.fr.box1.data.name.ent -relief sunken -bd 1 -bg $GDefs(ColorLight) -textvariable MLDP::Tmp(Scenario) -width 60
      pack .newscenario.emint.fr.box1.data.name.lbl .newscenario.emint.fr.box1.data.name.ent -side left -padx 2
   pack .newscenario.emint.fr.box1.data.name -side top -anchor w -padx 2 -fill x
   Bubble::Create .newscenario.emint.fr.box1.data.name "[lindex $Bubble(ScenarioName) $GDefs(Lang)]"
   set MLDP::Sim(ScenarioNameEntry) .newscenario.emint.fr.box1.data.name.ent

   #----- Total emission duration [s].
   frame .newscenario.emint.fr.box1.data.totdur
      label .newscenario.emint.fr.box1.data.totdur.lbl -relief flat -anchor w -text "[lindex $Lbl(TotalDuration) $GDefs(Lang)]"
      entry .newscenario.emint.fr.box1.data.totdur.ent -relief sunken -bd 1 -textvariable MLDP::Tmp(TotalDuration) -width 60 -state disabled
      pack .newscenario.emint.fr.box1.data.totdur.lbl .newscenario.emint.fr.box1.data.totdur.ent -side left -padx 2
   pack .newscenario.emint.fr.box1.data.totdur -side top -anchor w -padx 2 -fill x
   Bubble::Create .newscenario.emint.fr.box1.data.totdur "[lindex $Bubble(TotalDuration) $GDefs(Lang)]"

   #----- Effective emission duration [s].
   frame .newscenario.emint.fr.box1.data.effdur
      label .newscenario.emint.fr.box1.data.effdur.lbl -relief flat -anchor w -text "[lindex $Lbl(EffectiveDuration) $GDefs(Lang)]"
      entry .newscenario.emint.fr.box1.data.effdur.ent -relief sunken -bd 1 -textvariable MLDP::Tmp(EffectiveDuration) -width 60 -state disabled
      pack .newscenario.emint.fr.box1.data.effdur.lbl .newscenario.emint.fr.box1.data.effdur.ent -side left -padx 2
   pack .newscenario.emint.fr.box1.data.effdur -side top -anchor w -padx 2 -fill x
   Bubble::Create .newscenario.emint.fr.box1.data.effdur "[lindex $Bubble(EffectiveDuration) $GDefs(Lang)]"

   pack .newscenario.emint.fr.box1.data -side left
   pack .newscenario.emint.fr.box1 -side top

   frame .newscenario.emint.fr.box2 -bd 1 -relief sunken
   set MLDP::Sim(ReleaseRatesFrame) .newscenario.emint.fr.box2

   #----- Header.
   frame .newscenario.emint.fr.box2.header
      label .newscenario.emint.fr.box2.header.dur -relief raised -width 20 -bd 1 -text "[lindex $Lbl(Duration) $GDefs(Lang)]"
      pack .newscenario.emint.fr.box2.header.dur -side left -fill x -fill y
      Bubble::Create .newscenario.emint.fr.box2.header.dur "[lindex $Bubble(DurationInter) $GDefs(Lang)]"

      label .newscenario.emint.fr.box2.header.em  -relief raised -height 3 -width 30 -bd 1 -text "[lindex $Lbl(Period) $GDefs(Lang)]"
      pack .newscenario.emint.fr.box2.header.em -side left -fill x -expand true
      Bubble::Create .newscenario.emint.fr.box2.header.em  "[lindex $Bubble(Period) $GDefs(Lang)]"
   pack .newscenario.emint.fr.box2.header -side top -anchor w -fill x -expand true

   #----- Entries.
   for { set i 0 } { $i < $Sim(EmMaxInterval) } { incr i } {

      frame .newscenario.emint.fr.box2.entry$i
         entry .newscenario.emint.fr.box2.entry$i.dur -relief sunken -width 20 -bd 1 -bg $GDefs(ColorLight) -textvariable MLDP::Tmp(Duration$i)
         pack .newscenario.emint.fr.box2.entry$i.dur -side left
         Bubble::Create .newscenario.emint.fr.box2.entry$i.dur "[lindex $Bubble(DurationInter) $GDefs(Lang)]"

         Option::Create .newscenario.emint.fr.box2.entry$i.em "" [list MLDP::Tmp(Label$i) MLDP::Tmp(Value$i)] \
            0 -1 [lindex $Sim(ListEmissionLabel) $GDefs(Lang)] "" $Sim(ListEmissionValue)
      pack .newscenario.emint.fr.box2.entry$i.em -side left -fill x -expand true
         Bubble::Create .newscenario.emint.fr.box2.entry$i.em "[lindex $Bubble(Period) $GDefs(Lang)]"
      pack .newscenario.emint.fr.box2.entry$i -side top -anchor w -fill x -expand true
   }

   pack .newscenario.emint.fr.box2 -side top -anchor w -padx 5 -pady 10
   pack .newscenario.emint.fr -side top -anchor w -fill x
   pack .newscenario.emint -side top -anchor w -padx 5 -pady 5 -fill x

   frame .newscenario.button
      button .newscenario.button.cancel -text "[lindex $Lbl(Cancel) $GDefs(Lang)]" -bd 1 -command { destroy .newscenario }
      button .newscenario.button.apply -text "[lindex $Lbl(Apply) $GDefs(Lang)]" -bd 1 -command "MLDP::EmissionUpdate"
      pack .newscenario.button.apply .newscenario.button.cancel -side right
   pack .newscenario.button -side top -anchor w -padx 7 -fill x
   Bubble::Create .newscenario.button.cancel "[lindex $Bubble(Cancel) $GDefs(Lang)]"
   Bubble::Create .newscenario.button.apply "[lindex $Bubble(Apply) $GDefs(Lang)]"

   grab .newscenario
}

#----------------------------------------------------------------------------
# Nom        : <MLDP::VolcanInit>
# Creation   : 4 March 2004 - A. Malo - CMC/CMOE
#
# But        : Initialize variables for new volcano type release scenario.
#
# Parametres :
#
# Retour     :
#
# Remarques  :
#
#----------------------------------------------------------------------------

proc MLDP::ScenarioVolcanInit { } {
   global   GDefs
   variable Tmp
   variable Sim

   #----- Reset variables.
   for { set i 0 } { $i < $Sim(EmMaxInterval) } { incr i } {
      set Tmp(Duration$i) ""
      set Tmp(Label$i)    ""
      set Tmp(Value$i)    ""
   }

   #----- Initialize variables.
   set Tmp(Scenario)          $Sim(EmScenario)
   set Tmp(NbIntervals)       $Sim(EmNbIntervals)
   set Tmp(TotalDuration)     $Sim(EmTotalDuration)
   set Tmp(EffectiveDuration) $Sim(EmEffectiveDuration)
   set Tmp(Iso)               $Sim(EmIso.$Sim(EmScenario))
   set Tmp(Inter)             $Sim(EmInter.$Sim(EmScenario))

   set i      0
   set labels [lindex $Sim(ListEmissionLabel) $GDefs(Lang)]

   foreach inter $Tmp(Inter) {
      set Tmp(Duration$i) [lindex $inter 0]
      set Tmp(Value$i)    [lindex $inter 1]

      if { $Tmp(Value$i) == 1 } {
         set Tmp(Label$i) [lindex $labels 0]
      } elseif { $Tmp(Value$i) == 0 } {
         set Tmp(Label$i) [lindex $labels 1]
      }

      incr i
   }

   #----- Update (total and effective) emission durations.
   MLDP::ScenarioVolcanUpdateEmission
}

#----------------------------------------------------------------------------
# Nom        : <MLDP::ScenarioVolcanUpdateEmission>
# Creation   : 22 March 2004 - A. Malo - CMC/CMOE
#
# But        : Update (total and effective) emission durations.
#
# Parametres :
#
# Retour     :
#
# Remarques  :
#
#----------------------------------------------------------------------------

proc MLDP::ScenarioVolcanUpdateEmission { } {
   variable Tmp
   variable Sim

   #----- Initialize variables.
   set total     0
   set effective 0
   set nbint     0

   #----- Update emission durations.
   for { set i 0 } { $i < $Sim(EmMaxInterval) } { incr i } {

      if { $Tmp(Duration$i) != "" && $Tmp(Duration$i) > 0 } {

         if { $Tmp(Value$i) == 1 } {
            #----- Increment effective duration by emission duration value.
            set effective [expr $effective + $Tmp(Duration$i)]
         }

         if { $Tmp(Value$i) != -1 } {
            #----- Increment total duration by emission duration value.
            set total [expr $total + $Tmp(Duration$i)]

            #----- Increment number of emission intervals.
            incr nbint
         }
      }
   }

   #----- Update total and effective emission durations and number of intervals.
   set Tmp(TotalDuration)     $total
   set Tmp(EffectiveDuration) $effective
   set Tmp(NbIntervals)       $nbint
}

#----------------------------------------------------------------------------
# Nom        : <MLDP::ScenarioAccidentValidateTotalReleasedQuantity>
# Creation   : 22 March 2004 - A. Malo - CMC/CMOE
#
# But        : Validate total released quantity for accident source type.
#
# Parametres :
#
# Retour     :
#   <Idx>    : Flag indicating if validation has succeeded (1) or not (0).
#
# Remarques  :
#
#----------------------------------------------------------------------------

proc MLDP::ScenarioAccidentValidateTotalReleasedQuantity { } {
   global   GDefs
   variable Sim
   variable Error
   variable Tmp

   for { set j 0 } { $j < [llength $Tmp(Iso)] } { incr j } {

      if { $Tmp(ReleaseQuantity$j) == 0 } {

         #----- Total released quantity is null for this isotope.
         for { set i 0 } { $i < $Sim(EmMaxInterval) } { incr i } {
            if { $Tmp(ReleaseRate$i.$j) == 0 } {
               Dialog::CreateError .newscenario $Error(TotalQuantityAccident) "[lindex $Error(TotalQuantity2) $GDefs(Lang)] $Tmp(Iso$j).\n[lindex $Error(TotalQuantity3) $GDefs(Lang)] $Tmp(Duration$i) $Error(UnitSeconds)"
               focus $MLDP::Sim(ReleaseRatesFrame).entry$i.rates.iso$j
               return False
            }
         }
      }
   }
   return True
}

#----------------------------------------------------------------------------
# Nom        : <MLDP::ScenarioVirusValidateTotalReleasedQuantity>
# Creation   : 22 March 2004 - A. Malo - CMC/CMOE
#
# But        : Validate total released quantity for virus source type.
#
# Parametres :
#
# Retour     :
#   <Idx>    : Flag indicating if validation has succeeded (1) or not (0).
#
# Remarques  :
#
#----------------------------------------------------------------------------

proc MLDP::ScenarioVirusValidateTotalReleasedQuantity { } {
   global   GDefs
   variable Sim
   variable Error
   variable Tmp

   if { $Tmp(ReleaseQuantity) == 0 } {

      #----- Total released quantity is null.
      for { set i 0 } { $i < $Sim(EmMaxInterval) } { incr i } {
         if { $Tmp(ReleaseRate$i) == 0 } {
            Dialog::CreateError .newscenario $Error(TotalQuantityVirus) "[lindex $Error(TotalQuantity3) $GDefs(Lang)] $Tmp(Duration$i) $Error(UnitSeconds)"
            focus $MLDP::Sim(ReleaseRatesFrame).entry$i.rates
            return False
         }
      }
   }
   return True
}

#----------------------------------------------------------------------------
# Nom        : <MLDP::ValidateDurationValue>
# Creation   : 22 March 2004 - A. Malo - CMC/CMOE
#
# But        : Validate duration value.
#
# Parametres :
# <Duration> : Duration value.
# <Parent>   : Parent window.
# <Idx>      : Index of value in scenario interface (optional argument).
#
# Retour     :
#   <Idx>    : Flag indicating if validation has succeeded (1) or not (0).
#
# Remarques  :
#
#----------------------------------------------------------------------------

proc MLDP::ValidateDurationValue { Duration Parent { Idx -1 } } {
   global GDefs
   variable Sim
   variable Error
   variable Tmp

   #----- Verify if duration is positive.
   set number [string is integer -failindex idx $Duration]

   if { $number == 0 && $idx == -1 } {
      Dialog::CreateError $Parent $Error(DurationOutRange) " $Duration [lindex $Error(UnitSeconds) $GDefs(Lang)]"
      return False
   } elseif { $number == 0 || ($number == 1 && $Duration <= 0 && $Duration != "") } {
      Dialog::CreateError $Parent $Error(Duration) " $Duration $Error(UnitSeconds)"
      return False
   }

   #----- Compute  model time step [s].
   set timestepsec [expr $MLDP::Sim(ModelTimeStepMin)*60]

   #----- If the duration has a value.
   if { $Duration != "" } {

      #----- Verify if duration is greater or equal to the model time step.
      if { $Duration < $timestepsec } {
         Dialog::CreateError $Parent $Error(Duration2) "[lindex $Error(Duration4) $GDefs(Lang)] $Duration $Error(UnitSeconds)\n[lindex $Error(Duration5) $GDefs(Lang)] $timestepsec $Error(UnitSeconds) ($Sim(ModelTimeStepMin) $Error(UnitMinutes))"

         if { $Idx > -1 } {

            #----- Modify the emission duration to the nearest multiple of internal model time step.
            set Tmp(Duration$Idx) [expr int(double($Duration)/double($timestepsec)+0.5) * $timestepsec]

            if { $Tmp(Duration$Idx) < $timestepsec } {
               set Tmp(Duration$Idx) $timestepsec
            }
         }
         return False
      }

      #----- Verify if duration is an integer multiple of the model time step.
      if { [expr fmod($Duration, $timestepsec)] > $Sim(EmEpsilon) } {
         Dialog::CreateError $Parent $Error(Duration3) "[lindex $Error(Duration4) $GDefs(Lang)] $Duration $Error(UnitSeconds)\n[lindex $Error(Duration5) $GDefs(Lang)] $timestepsec $Error(UnitSeconds) ($Sim(ModelTimeStepMin) $Error(UnitMinutes))"

         if { $Idx > -1 } {

            #----- Modify the emission duration to the nearest multiple of internal model time step.
            set Tmp(Duration$Idx) [expr int(double($Duration)/double($timestepsec)+0.5) * $timestepsec]

            if { $Tmp(Duration$Idx) < $timestepsec } {
               set Tmp(Duration$Idx) $timestepsec
            }
         }
         return False
      }
   }

   return True
}

#----------------------------------------------------------------------------
# Nom        : <MLDP::ValidateDurationsRatesAccident>
# Creation   : 22 March 2004 - A. Malo - CMC/CMOE
#
# But        : Verify if emission durations and release rate fields
#              are filled simultaneously. Verify if there is no empty
#              fields in interface (Number of intervals > 0).
#
# Parametres :
#
# Retour     :
#   <Idx>    : Flag indicating if validation has succeeded (1) or not (0).
#
# Remarques  :
#
#----------------------------------------------------------------------------

proc MLDP::ValidateDurationsRatesAccident { } {
   global   GDefs
   variable Sim
   variable Tmp
   variable Error
   variable hasFoundRate

   #----- Verify if emission durations and release rate fields are filled simultaneously.
   for { set j 0 } { $j < [llength $Tmp(Iso)] } { incr j } {    #----- Loop over isotopes.
      for { set i 0 } { $i < $Sim(EmMaxInterval) } { incr i } { #----- Loop over emission durations.

         if { $Tmp(Duration$i) != "" && $Tmp(ReleaseRate$i.$j) == "" } {
            #----- Release rate field for this isotope is empty while emission duration field is filled.
            Dialog::CreateError .newscenario $Error(Scenario)
            focus $MLDP::Sim(ReleaseRatesFrame).entry$i.rates.iso$j
            return 0
         } elseif { $Tmp(Duration$i) == "" && $Tmp(ReleaseRate$i.$j) != "" } {
            #----- Emission duration field is empty while release rate field for this isotope is filled.
            Dialog::CreateError .newscenario $Error(Scenario)
            focus $MLDP::Sim(ReleaseRatesFrame).entry$i.dur
            return 0
         }
      }
   }

   #----- Verify if there is no empty fields (Number of intervals > 0).
   set hasFoundDuration 0
   for { set i 0 } { $i < $Sim(EmMaxInterval) } { incr i } {
      if { $Tmp(Duration$i) != "" } {
         set hasFoundDuration 1
         break
      }
   }

   for { set j 0 } { $j < [llength $Tmp(Iso)] } { incr j } {
      set hasFoundRate($j) 0
   }

   for { set j 0 } { $j < [llength $Tmp(Iso)] } { incr j } {
      for { set i 0 } { $i < $Sim(EmMaxInterval) } { incr i } {
         if { $Tmp(ReleaseRate$i.$j) != "" } {
            set hasFoundRate($j) 1
            break
         }
      }
   }

   for { set j 0 } { $j < [llength $Tmp(Iso)] } { incr j } {
      if { !$hasFoundDuration && !$hasFoundRate($j) } {

         #----- Emission duration and release rate for this isotope fields are both empty.
         Dialog::CreateError .newscenario $Error(Scenario)
         focus $MLDP::Sim(ReleaseRatesFrame).entry0.dur
         return 0
      }
   }

   return 1
}

#----------------------------------------------------------------------------
# Nom        : <MLDP::ValidateDurationsRatesVirus>
# Creation   : 22 March 2004 - A. Malo - CMC/CMOE
#
# But        : Verify if emission durations and release rate fields
#              are filled simultaneously. Verify if there is no empty
#              fields in interface (Number of intervals > 0).
#
# Parametres :
#
# Retour     :
#   <Idx>    : Flag indicating if validation has succeeded (1) or not (0).
#
# Remarques  :
#
#----------------------------------------------------------------------------

proc MLDP::ValidateDurationsRatesVirus { } {
   global   GDefs
   variable Sim
   variable Tmp
   variable Error

   #----- Verify if emission durations and release rate fields are filled simultaneously.
   for { set i 0 } { $i < $Sim(EmMaxInterval) } { incr i } {

      if { $Tmp(Duration$i) != "" && $Tmp(ReleaseRate$i) == "" } {
         #----- Release rate field is empty while emission duration field is filled.
         Dialog::CreateError .newscenario $Error(Scenario)
         focus $MLDP::Sim(ReleaseRatesFrame).entry$i.rates
         return 0
      } elseif { $Tmp(Duration$i) == "" && $Tmp(ReleaseRate$i) != "" } {
         #----- Emission duration field is empty while release rate field is filled.
         Dialog::CreateError .newscenario $Error(Scenario)
         focus $MLDP::Sim(ReleaseRatesFrame).entry$i.dur
         return 0
      }
   }

   #----- Verify if there is no empty fields (Number of intervals > 0).
   set hasFoundDuration 0
   for { set i 0 } { $i < $Sim(EmMaxInterval) } { incr i } {
      if { $Tmp(Duration$i) != "" } {
         set hasFoundDuration 1
         break
      }
   }

   set hasFoundRate 0
   for { set i 0 } { $i < $Sim(EmMaxInterval) } { incr i } {
      if { $Tmp(ReleaseRate$i) != "" } {
         set hasFoundRate 1
         break
      }
   }

   if { !$hasFoundDuration && !$hasFoundRate } {
      #----- Emission duration and release rate fields are both empty.
      Dialog::CreateError .newscenario $Error(Scenario)
      focus $MLDP::Sim(ReleaseRatesFrame).entry0.dur
      return 0
   }

   return 1
}

#----------------------------------------------------------------------------
# Nom        : <MLDP::ValidateDurationsReleasesVolcano>
# Creation   : 22 March 2004 - A. Malo - CMC/CMOE
#
# But        : Verify if emission durations and emission period type
#              fields are filled simultaneously. Verify if there is no
#              empty fields in interface (Number of intervals > 0).
#
# Parametres :
#
# Retour     :
#   <Idx>    : Flag indicating if validation has succeeded (1) or not (0).
#
# Remarques  :
#
#----------------------------------------------------------------------------

proc MLDP::ValidateDurationsReleasesVolcano { } {
   global   GDefs
   variable Sim
   variable Tmp
   variable Error

   #----- Verify if emission durations and emission period type fields are filled simultaneously.

   for { set i 0 } { $i < $Sim(EmMaxInterval) } { incr i } {

      if { $Tmp(Duration$i) == "" } {

         if { $Tmp(Value$i) == 1 || $Tmp(Value$i) == 0 } {
            #----- Emission duration field is empty while emission period type field is filled.
            Dialog::CreateError .newscenario $Error(Scenario)
            focus $MLDP::Sim(ReleaseRatesFrame).entry$i.dur
            return 0
         }

      } else {

         if { $Tmp(Value$i) == "" || $Tmp(Value$i) == -1 } {
            #----- Emission duration field is filled while emission period type field is empty.
            Dialog::CreateError .newscenario $Error(Scenario)
            focus $MLDP::Sim(ReleaseRatesFrame).entry$i.dur
            return 0
         }
      }
   }

   #----- Verify if there is no empty fields (Number of intervals > 0).
   set hasFoundDuration 0
   for { set i 0 } { $i < $Sim(EmMaxInterval) } { incr i } {
      if { $Tmp(Duration$i) != "" } {
         set hasFoundDuration 1
         break
      }
   }

   set hasFoundReleasePeriodType 0
   for { set i 0 } { $i < $Sim(EmMaxInterval) } { incr i } {
      if { $Tmp(Value$i) == 1 || $Tmp(Value$i) == 0 } {
         set hasFoundReleasePeriodType 1
         break
      }
   }

   if { !$hasFoundDuration && !$hasFoundReleasePeriodType } {
      #----- Emission duration and release period type fields are both empty.
      Dialog::CreateError .newscenario $Error(Scenario)
      focus $MLDP::Sim(ReleaseRatesFrame).entry0.dur
      return 0
   }
   return 1
}

#----------------------------------------------------------------------------
# Nom        : <MLDP::ValidateDurationsVsModelTimeStep>
# Creation   : 22 March 2004 - A. Malo - CMC/CMOE
#
# But        : Validate durations for each release interval according to
#              internal model time step.
#
# Parametres :
#
# Retour     :
#   <Idx>    : Flag indicating if validation has succeeded (1) or not (0).
#
# Remarques  :
#
#----------------------------------------------------------------------------

proc MLDP::ValidateDurationsVsModelTimeStep { } {
   variable Sim

   foreach interval $Sim(EmInter.$Sim(EmScenario)) {
      set duration [lindex $interval 0]

      #----- Validate emission duration value.
      if { ![MLDP::ValidateDurationValue $duration .modelnew] } {
         return 0
      }
   }
   return 1
}

#----------------------------------------------------------------------------
# Nom        : <MLDP::ValidateNbIsotopes>
# Creation   : 22 March 2004 - A. Malo - CMC/CMOE
#
# But        : Validate number of isotopes.
#
# Parametres :
#
# Retour     :
#   <Idx>    : Flag indicating if validation has succeeded (1) or not (0).
#
# Remarques  :
#
#----------------------------------------------------------------------------

proc MLDP::ValidateNbIsotopes { } {
   global   GDefs
   variable Sim
   variable Tmp
   variable Error

   #----- Verify if number of isotopes is greater than 0.
   if { [llength $Tmp(Iso)] < 1 } {

      Dialog::CreateError .newscenario $Error(NbIsotopes)

      for { set i 0 } { $i < $Sim(EmMaxInterval) } { incr i } {
         if { $Tmp(Duration$i) != "" } {
            focus $MLDP::Sim(ReleaseRatesFrame).entry$i.rates.iso0
            return 0
         }
      }
      focus $MLDP::Sim(ReleaseRatesFrame).entry0.rates.iso0
      return 0
   }
   return 1
}

#----------------------------------------------------------------------------
# Nom        : <MLDP::ValidateReleaseRatesAccident>
# Creation   : 22 March 2004 - A. Malo - CMC/CMOE
#
# But        : Validate rates for each release interval and each isotope
#              for accident source type.
#
# Parametres :
#
# Retour     :
#   <Idx>    : Flag indicating if validation has succeeded (1) or not (0).
#
# Remarques  :
#
#----------------------------------------------------------------------------

proc MLDP::ValidateReleaseRatesAccident { } {
   variable Sim
   variable Tmp

   for { set i 0 } { $i < $Sim(EmMaxInterval) } { incr i } {
      for { set j 0 } { $j < [llength $Tmp(Iso)] } { incr j } {

         #----- Validate release rate value for each interval and each isotope.
         if { [MLDP::ValidateReleaseRateValue $i $j] } {
            if { $Tmp(ReleaseRate$i.$j) != "" } {
               set Tmp(ReleaseRate$i.$j) [format "%.7e" $Tmp(ReleaseRate$i.$j)]
            }
         } else {
            focus $MLDP::Sim(ReleaseRatesFrame).entry$i.rates.iso$j
            return 0
         }
      }
   }
   return 1
}

#----------------------------------------------------------------------------
# Nom        : <MLDP::ValidateReleaseRatesVirus>
# Creation   : 22 March 2004 - A. Malo - CMC/CMOE
#
# But        : Validate rates for each release interval and
#              for virus source type.
#
# Parametres :
#
# Retour     :
#   <Idx>    : Flag indicating if validation has succeeded (1) or not (0).
#
# Remarques  :
#
#----------------------------------------------------------------------------

proc MLDP::ValidateReleaseRatesVirus { } {
   variable Sim
   variable Tmp

   for { set i 0 } { $i < $Sim(EmMaxInterval) } { incr i } {

      #----- Validate release rate value for each interval.
      if { [MLDP::ValidateReleaseRateValue $i] } {
         if { $Tmp(ReleaseRate$i) != "" } {
            set Tmp(ReleaseRate$i) [format "%.7e" $Tmp(ReleaseRate$i)]
         }
      } else {
         focus $MLDP::Sim(ReleaseRatesFrame).entry$i.rates
         return 0
      }
   }
   return 1
}

#----------------------------------------------------------------------------
# Nom        : <MLDP::ValidateReleaseRateValue>
# Creation   : 22 March 2004 - A. Malo - CMC/CMOE
#
# But        : Validate release rate value.
#
# Parametres :
# <Interval> : Index of release interval in interface.
# <Isotope>  : Index of isotope in interface (optional argument).
#
# Retour     :
#   <Idx>    : Flag indicating if validation has succeeded (1) or not (0).
#
# Remarques  :
#
#----------------------------------------------------------------------------

proc MLDP::ValidateReleaseRateValue { Interval { Isotope -1 } } {
   global GDefs
   variable Tmp
   variable Error

   if { $Isotope > -1 } { #----- Accident.
      set rate $Tmp(ReleaseRate$Interval.$Isotope)
   } else {               #----- Virus.
      set rate $Tmp(ReleaseRate$Interval)
   }

   #----- Verify if release rate is positive.
   set number [string is double -failindex idx $rate]
   if { $number == 0 && $idx == -1 } {
      Dialog::CreateError .newscenario $Error(ReleaseRateRange) " $rate [lindex $Error(UnitRate) $GDefs(Lang)]"
      return 0
   } elseif { $number == 0 || ($rate < 0 && $rate != "") } {
      Dialog::CreateError .newscenario $Error(ReleaseRate) " $rate [lindex $Error(UnitRate) $GDefs(Lang)]"
      return 0
   }
   return 1
}

#----------------------------------------------------------------------------
# Nom        : <MLDP::ValidateSimulationDuration>
# Creation   : 27 August 2007 - A. Malo - CMC/CMOE
#
# But        : Validate simulation duration according to time interval
#              between meteorological data files.
#
# Parametres :
#
# Retour     :
#   <Idx>    : Flag indicating if validation has succeeded (1) or not (0).
#
# Remarques  :
#
#----------------------------------------------------------------------------

proc MLDP::ValidateSimulationDuration { } {
   global GDefs
   variable Sim
   variable Error
   variable Warning
   variable Lbl

   #----- Verify if output time step is positive.
   set number [string is integer -strict -failindex idx $Sim(Duration)]
   if { $number == 0 && $idx == -1 } {
      Dialog::CreateError .modelnew $Error(SimDurationOutRange) " $Sim(Duration) $Error(UnitHours)"
      focus $Sim(SimDurationEnt)
      return 0
   } elseif { $number == 0 || ($number == 1 && $Sim(Duration) < 1) } {
      Dialog::CreateError .modelnew $Error(SimDuration) " $Sim(Duration) $Error(UnitHours)"
      focus $Sim(SimDurationEnt)
      return 0
   }

   #----- Verify if simulation duration is greater than (or equal to) time interval between met data files.
   if { $Sim(Duration) < $Sim(Delta) } {
      Dialog::CreateError .modelnew $Error(SimDurationDelta) "[lindex $Error(SimDurationDelta2) $GDefs(Lang)] $Sim(Duration) $Error(UnitHours)\n[lindex $Error(SimDurationDelta3) $GDefs(Lang)] $Sim(Delta) $Error(UnitHours)"
      focus $Sim(SimDurationEnt)
      return 0
   }

   #----- Verify if simulation duration is greater than output time step.
   set ots [expr double($Sim(OutputTimeStepMin))/double(60.0)] ; #----- Output time step [hr].
   if { $Sim(Duration) < $ots } {
      Dialog::CreateError .modelnew $Error(SimDurationOutputTimeStep) "[lindex $Error(SimDurationOutputTimeStep2) $GDefs(Lang)] $Sim(Duration) $Error(UnitHours)\n[lindex $Error(SimDurationOutputTimeStep3) $GDefs(Lang)] $ots $Error(UnitHours)"
      focus $Sim(SimDurationEnt)
      return 0
   }

   #----- Display warning if simulation duration > 72 hrs and output time step <= 60 min.
   if { !$Sim(IsResFileSizeChecked) } {
      if { $Sim(Duration) > 72 && $Sim(OutputTimeStepMin) <= 60 } {
         set yes [Dialog::CreateDefault .modelnew 400 WARNING $Warning(SimDuration4) "\n\n[lindex $Warning(SimDuration5) $GDefs(Lang)] $Sim(Duration) $Error(UnitHours)\n[lindex $Warning(SimDuration6) $GDefs(Lang)] $Sim(OutputTimeStepMin) $Error(UnitMinutes)" 0 $Lbl(No) $Lbl(Yes)]
         if { $yes } {
            focus $Sim(OutputTimeStepEnt)
            return 0
         } else {
            set Sim(IsResFileSizeChecked) 1
         }
      }
   }

   #----- Display warning if simulation duration > 72 hrs and time interval between met data files < 3.
   if { !$Sim(IsMetFileSizeChecked) } {
      if { $Sim(Duration) > 72 && $Sim(Delta) < 3 } {
         set yes [Dialog::CreateDefault .modelnew 400 WARNING $Warning(SimDuration7) "\n\n[lindex $Warning(SimDuration8) $GDefs(Lang)] $Sim(Duration) $Error(UnitHours)\n[lindex $Warning(SimDuration9) $GDefs(Lang)] $Sim(Delta) $Error(UnitHours)" 0 $Lbl(No) $Lbl(Yes)]
         if { $yes } {
            focus $Sim(SimDurationEnt)
            return 0
         } else {
            set Sim(IsMetFileSizeChecked) 1
         }
      }
   }
   return 1
}

#----------------------------------------------------------------------------
# Nom        : <MLDP::ValidateTimeSteps>
# Creation   : 22 March 2004 - A. Malo - CMC/CMOE
#
# But        : Validate output and model time steps.
#
# Parametres :
#
# Retour     :
#   <Idx>    : Flag indicating if validation has succeeded (1) or not (0).
#
# Remarques  :
#
#----------------------------------------------------------------------------

proc MLDP::ValidateTimeSteps { } {
   global GDefs
   variable Sim
   variable Error

   #----- Verify if output time step is positive.
   set number [string is integer -strict -failindex idx $MLDP::Sim(OutputTimeStepMin)]
   if { $number == 0 && $idx == -1 } {
      Dialog::CreateError .modelnew $Error(OutputTimeStepMinOutRange)] " $Sim(OutputTimeStepMin) $Error(UnitMinutes)"
      focus $Sim(OutputTimeStepEnt)
      return 0
   } elseif { $number == 0 || ($number == 1 && $MLDP::Sim(OutputTimeStepMin) <= 0) } {
      Dialog::CreateError .modelnew $Error(OutputTimeStepMin) " $Sim(OutputTimeStepMin) $Error(UnitMinutes)"
      focus $Sim(OutputTimeStepEnt)
      return 0
   }

   #----- Verify if model time step is positive.
   set number [string is integer -strict -failindex idx $MLDP::Sim(ModelTimeStepMin)]
   if { $number == 0 && $idx == -1 } {
      Dialog::CreateError .modelnew $Error(ModelTimeStepMinOutRange) " $Sim(ModelTimeStepMin) $Error(UnitMinutes)"
      focus $Sim(ModelTimeStepEnt)
      return 0
   } elseif { $number == 0 || ($number == 1 && $MLDP::Sim(ModelTimeStepMin) <= 0) } {
      Dialog::CreateError .modelnew $Error(ModelTimeStepMin) " $Sim(ModelTimeStepMin) $Error(UnitMinutes)"
      focus $Sim(ModelTimeStepEnt)
      return 0
   }

   #----- Verify if model time step is lower or equal to an hour (60 minutes).
   if { $MLDP::Sim(ModelTimeStepMin) > 60 } {
      Dialog::CreateError .modelnew $Error(ModelTimeStep1hour) " $Sim(ModelTimeStepMin) $Error(UnitMinutes)"
      focus $Sim(ModelTimeStepEnt)
      return 0
   }

   #----- Verify if model time step is an integer divisor of an hour (60 minutes).
   if { [expr fmod( 60, $MLDP::Sim(ModelTimeStepMin) )] > $MLDP::Sim(EmEpsilon) } {
      Dialog::CreateError .modelnew $Error(ModelTimeStep1hourDiv) " $Sim(ModelTimeStepMin) $Error(UnitMinutes)"
      focus $Sim(ModelTimeStepEnt)
      return 0
   }

   #----- Verify if model time step is lower or equal to the output time step.
   if { $MLDP::Sim(ModelTimeStepMin) > $MLDP::Sim(OutputTimeStepMin) } {
      Dialog::CreateError .modelnew $Error(ModelTimeStepOutputTimeStep) "[lindex $Error(ModelTimeStepOutputTimeStep2) $GDefs(Lang)] $Sim(ModelTimeStepMin) $Error(UnitMinutes)\n[lindex $Error(ModelTimeStepOutputTimeStep3) $GDefs(Lang)] $Sim(OutputTimeStepMin) $Error(UnitMinutes)"
      focus $Sim(ModelTimeStepEnt)
      return 0
   }

   #----- Verify if output time step is an integer multiple of the model time step.
   if { [expr fmod( $MLDP::Sim(OutputTimeStepMin), $MLDP::Sim(ModelTimeStepMin) )] > $MLDP::Sim(EmEpsilon) } {
      Dialog::CreateError .modelnew $Error(OutputTimeStepModelTimeStep) "[lindex $Error(OutputTimeStepModelTimeStep2) $GDefs(Lang)] $Sim(OutputTimeStepMin) $Error(UnitMinutes)\n[lindex $Error(OutputTimeStepModelTimeStep3) $GDefs(Lang)] $Sim(ModelTimeStepMin) $Error(UnitMinutes)"
      focus $Sim(OutputTimeStepEnt)
      return 0
   }

   #----- Update emission starting time.
   MLDP::UpdateEmissionStartingTime

   return 1
}

#----------------------------------------------------------------------------
# Nom        : <MLDP::ValidateLullPeriodsEndAccident>
# Creation   : 22 March 2004 - A. Malo - CMC/CMOE
#
# But        : Validate lull periods at the end of the release scenario
#              for accident source type.
#
# Parametres :
#
# Retour     :
#   <Idx>    : Flag indicating if validation has succeeded (1) or not (0).
#
# Remarques  :
#
#----------------------------------------------------------------------------

proc MLDP::ValidateLullPeriodsEndAccident { } {
   global   GDefs
   variable Sim
   variable Error
   variable Tmp

   #----- Validate lull periods at the end of the release scenario.
   for { set i [expr $Sim(EmMaxInterval) - 1] } { $i >= 0 } { incr i -1 } {

      if { $Tmp(Duration$i) != "" } {
         for { set j 0 } { $j < [llength $Tmp(Iso)] } { incr j } {
            if { $Tmp(ReleaseRate$i.$j) > 0 } {
               return 1 ; #----- No lull periods.
            }
         }

         Dialog::CreateError .newscenario $Error(LullPeriodEndAccident) " $Tmp(Duration$i) $Error(UnitSeconds)"
         focus $MLDP::Sim(ReleaseRatesFrame).entry$i.rates.iso0
         return 0
      }
   }
   return 0
}

#----------------------------------------------------------------------------
# Nom        : <MLDP::ValidateLullPeriodsEndVirus>
# Creation   : 22 March 2004 - A. Malo - CMC/CMOE
#
# But        : Validate lull periods at the end of the release scenario
#              for virus source type.
#
# Parametres :
#
# Retour     :
#   <Idx>    : Flag indicating if validation has succeeded (1) or not (0).
#
# Remarques  :
#
#----------------------------------------------------------------------------

proc MLDP::ValidateLullPeriodsEndVirus { } {
   global   GDefs
   variable Sim
   variable Error
   variable Tmp

   #----- Validate lull periods at the end of the release scenario.
   for { set i [expr $Sim(EmMaxInterval) - 1] } { $i >= 0 } { incr i -1 } {

      if { $Tmp(Duration$i) != "" } {
         if { $Tmp(ReleaseRate$i) > 0 } {
            return 1 ; #----- No lull periods.
         }

         Dialog::CreateError .newscenario $Error(LullPeriodEndVirus) " $Tmp(Duration$i) $Error(UnitSeconds)"
         focus $MLDP::Sim(ReleaseRatesFrame).entry$i.rates
         return 0
      }
   }
   return 0
}

#----------------------------------------------------------------------------
# Nom        : <MLDP::ValidateLullPeriodsEndVolcan>
# Creation   : 22 March 2004 - A. Malo - CMC/CMOE
#
# But        : Validate lull periods at the end of the release scenario
#              for volcano source type.
#
# Parametres :
#
# Retour     :
#   <Idx>    : Flag indicating if validation has succeeded (1) or not (0).
#
# Remarques  :
#
#----------------------------------------------------------------------------

proc MLDP::ValidateLullPeriodsEndVolcan { } {
   global   GDefs
   variable Sim
   variable Error
   variable Tmp

   #----- Validate lull periods at the end of the release scenario.
   for { set i [expr $Sim(EmMaxInterval) - 1] } { $i >= 0 } { incr i -1 } {

      if { $Tmp(Duration$i) != "" } {
         if { $Tmp(Value$i) == 1 } {
            return 1 ; #----- No lull periods.
         }

         Dialog::CreateError .newscenario  $Error(LullPeriodEndVolcan) " $Tmp(Duration$i) $Error(UnitSeconds)"
         focus $MLDP::Sim(ReleaseRatesFrame).entry$i.dur
         return 0
      }
   }
   return 0
}

#----------------------------------------------------------------------------
# Nom        : <MLDP::ValidateLullPeriodsStartAccident>
# Creation   : 22 March 2004 - A. Malo - CMC/CMOE
#
# But        : Validate lull periods at the beginning of the release
#              scenario for accident source type.
#
# Parametres :
#
# Retour     :
#   <Idx>    : Flag indicating if validation has succeeded (1) or not (0).
#
# Remarques  :
#
#----------------------------------------------------------------------------

proc MLDP::ValidateLullPeriodsStartAccident { } {
   global   GDefs
   variable Sim
   variable Error
   variable Tmp

   #----- Validate lull periods at the beginning of the release scenario.
   for { set i 0 } { $i < $Sim(EmMaxInterval) } { incr i } {

      if { $Tmp(Duration$i) != "" } {
         for { set j 0 } { $j < [llength $Tmp(Iso)] } { incr j } {
            if { $Tmp(ReleaseRate$i.$j) > 0 } {
               return 1 ; #----- No lull periods.
            }
         }

         Dialog::CreateError .newscenario $Error(LullPeriodStartAccident) " $Tmp(Duration$i) $Error(UnitSeconds)"
         focus $MLDP::Sim(ReleaseRatesFrame).entry$i.rates.iso0
         return 0
      }
   }
   return 0
}

#----------------------------------------------------------------------------
# Nom        : <MLDP::ValidateLullPeriodsStartVirus>
# Creation   : 22 March 2004 - A. Malo - CMC/CMOE
#
# But        : Validate lull periods at the beginning of the release
#              scenario for virus source type.
#
# Parametres :
#
# Retour     :
#   <Idx>    : Flag indicating if validation has succeeded (1) or not (0).
#
# Remarques  :
#
#----------------------------------------------------------------------------

proc MLDP::ValidateLullPeriodsStartVirus { } {
   global   GDefs
   variable Sim
   variable Error
   variable Tmp

   #----- Validate lull periods at the beginning of the release scenario.
   for { set i 0 } { $i < $Sim(EmMaxInterval) } { incr i } {

      if { $Tmp(Duration$i) != "" } {

         if { $Tmp(ReleaseRate$i) > 0 } {
            return 1 ; #----- No lull periods.
         }

         Dialog::CreateError .newscenario $Error(LullPeriodStartVirus) " $Tmp(Duration$i) $Error(UnitSeconds)"
         focus $MLDP::Sim(ReleaseRatesFrame).entry$i.rates
         return 0
      }
   }
   return 0
}

#----------------------------------------------------------------------------
# Nom        : <MLDP::ValidateLullPeriodsStartVolcan>
# Creation   : 22 March 2004 - A. Malo - CMC/CMOE
#
# But        : Validate lull periods at the beginning of the release
#              scenario for volcano source type.
#
# Parametres :
#
# Retour     :
#   <Idx>    : Flag indicating if validation has succeeded (1) or not (0).
#
# Remarques  :
#
#----------------------------------------------------------------------------

proc MLDP::ValidateLullPeriodsStartVolcan { } {
   global   GDefs
   variable Sim
   variable Error
   variable Tmp

   #----- Validate lull periods at the beginning of the release scenario.
   for { set i 0 } { $i < $Sim(EmMaxInterval) } { incr i } {

      if { $Tmp(Duration$i) != "" } {

         if { $Tmp(Value$i) == 1 } {
            return 1 ; #----- No lull periods.
         }

         Dialog::CreateError .newscenario $Error(LullPeriodStartVolcan) " $Tmp(Duration$i) $Error(UnitSeconds)"
         focus $MLDP::Sim(ReleaseRatesFrame).entry$i.dur
         return 0
      }
   }
   return 0
}
