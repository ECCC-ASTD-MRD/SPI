#----------------------------------------------------------------------------
# Nom        : <MLDPn::ValidateMassInputParams>
# Creation   : 22 March 2004 - A. Malo - CMC/CMOE
#
# But        : Validate input parameters for total release mass
#              calculation for volcano eruption.
#
# Parametres :
#     <Id>   : The id of the emission interval.
#
# Retour     :
#   <Idx>    : Flag indicating if validation has succeeded (1) or not (0).
#
# Remarques  :
#    Validate :
#    - particle density.
#    - Maximum plume height.
#    - Emission durations according to model time step.
#
#----------------------------------------------------------------------------

proc MLDPn::ValidateMassInputParams { Id } {
   variable Sim
   variable Tmp

   #----- Validate particle density.
   if { ![MLDPn::ValidateDensity] } {
      focus $Sim(EmissionGlobalFrame).density.ent
      return 0
   }

   #----- Validate maximum plume height.
   if { ![MLDPn::ValidatePlumeHeight $Id] } {
      focus $Sim(EmissionGlobalFrame).height.e
      return 0
   }

   #----- Validate emission duration
   if { ![MLDPn::ValidateDuration [expr $Tmp(EmInter.$Id)*$Sim(EmInterMode)] .emintdetails] } {
      focus $Sim(EmissionIntervalFrame).duration.ent
      return 0
   }

   return 1
}

#----------------------------------------------------------------------------
# Nom        : <MLDPn::ValidateEmissionIntervalDetails>
# Creation   : Juillet 2010 - E. Legault-Ouellet - CMC/CMOE
#
# But        : Validate all the parameters of the emission interval window.
#
# Parametres :
#     <Id>  : The id of the emission interval to validate.
#
# Retour     : 0 if validation fails, 1 if validation succeed.
#
# Remarques  :
#
#----------------------------------------------------------------------------

proc MLDPn::ValidateEmissionIntervalDetails { Id } {
   variable Warning
   variable Error
   variable Sim
   variable Tmp
   variable Lbl

   #----- Validate global emission parameters

   #----- Validate global number of particles (if needed).
   if { $Tmp(EmIsAutoNP) && ![MLDPn::ValidateNumberParticles] } {
      focus $Sim(EmissionGlobalFrame).nbpart.ent
      return 0
   }
   #----- Validate density (if needed).
   if { $Sim(SrcType) == "VOLCANO" } {
      if { ![MLDPn::ValidateDensity] } {
         focus $Sim(EmissionGlobalFrame).density.ent
         return 0
      }
   }

   #----- Validate emission interval parameters

   #----- Duration
   if { ![MLDPn::ValidateDuration [expr $Tmp(EmInter.$Id)*$Sim(EmInterMode)] .emintdetails] } {
      focus $Sim(EmissionIntervalFrame).duration.ent
      return 0
   }
   if { $Tmp(EmIsEm.$Id) } {
      #----- Number of particles
      if { !$Tmp(EmIsAutoNP) && ![MLDPn::ValidateNumberParticles $Id] } {
         focus $Sim(EmissionIntervalFrame).emdetails.nbpart.ent
         return 0
      }
      #----- Height
      if { ![MLDPn::ValidatePlumeHeight $Id] } {
         focus $Sim(EmissionIntervalFrame).emdetails.height.e
         return 0
      }
      #----- Radius
      if { ![MLDPn::ValidateRadius $Id] } {
         focus $Sim(EmissionIntervalFrame).emdetails.radius.ent
         return 0
      }

      switch $Sim(SrcType) {
         "VOLCANO" {
            #----- Mass
            ComputeMass $Id
            if { ![MLDPn::ValidateMass $Id] } {
               focus $Sim(EmissionIntervalFrame).emdetails.mass.e
               return 0
            }
         }
         "ACCIDENT" {
            #----- Number of isotopes
            if { $Tmp(EmNbIso) <= 0 } {
               Dialog::Error .emintdetails $Error(NbIsotopes)
               return 0
            }
            #----- Release rates
            set flag 0
            for { set i 0 } { $i < $Tmp(EmNbIso) } { incr i } {
               if { ![MLDPn::ValidateReleaseRate $Tmp(EmRate.$Id.$i)] } {
                  focus $Sim(EmissionIsoFrame).$i.ent
                  return 0
               }
               if { $Tmp(EmRate.$Id.$i) > 0.0 } {
                  set flag 1
                  #----- Convert from "unit" to "unit/h" if necessary
                  if { $Tmp(EmRateMode.$Id.$i) } {
                     MLDPn::EmRateModeToggle $Sim(EmissionIsoFrame) $Id $i
                  }
               }
            }
            #----- Check if it is a real emission
            if { !$flag } {
               if { ![Dialog::Default .emintdetails 400 WARNING $Warning(SwitchEm2LullA) "" 0 $Lbl(No) $Lbl(Yes)] } {
                  return 0
               }
               set Tmp(EmIsEm.$Id) 0
            }
         }
         "VIRUS" {
            #----- Release rate
            if { ![MLDPn::ValidateReleaseRate $Tmp(EmRate.$Id)] } {
               focus $Sim(EmissionIntervalFrame).emdetails.rate.ent
               return 0
            }
            #----- Check if it is a real emission
            if { $Tmp(EmRate.$Id) == 0.0 } {
               if { ![Dialog::Default .emintdetails 400 WARNING $Warning(SwitchEm2LullV) "" 0 $Lbl(No) $Lbl(Yes)] } {
                  return 0
               }
               set Tmp(EmIsEm.$Id) 0
            } else {
               #----- Convert from "unit" to "unit/h" if necessary
               if { $Tmp(EmRateMode.$Id) } {
                  MLDPn::EmRateModeToggle $Sim(EmissionIntervalFrame).emdetails.rate $Id
               }
            }
         }
      }
   }

   #----- Elements that sum up and have a maximum summed up value

   if { ![MLDPn::ValidateTotalNumberParticles $Id] } { return 0 }
   if { ![MLDPn::ValidateEmissionDuration     $Id] } { return 0 }

   return 1
}

#----------------------------------------------------------------------------
# Nom        : <MLDPn::ValidateVerticalLevels>
# Creation   : 29 June 2006 - A. Malo - CMC/CMOE
#
# But        : Validate vertical levels (m) for concentration calculations.
#
# Parametres :
#
# Retour     :
#   <Idx>    : Flag indicating if validation has succeeded (1) or not (0).
#
# Remarques  :
#
#----------------------------------------------------------------------------

proc MLDPn::ValidateVerticalLevels { } {
   global GDefs
   variable Error
   variable Lbl
   variable Warning
   variable Sim

   #----- Number of vertical levels for concentration calculations.
   set nb [llength $Sim(OutCV)]

   #----- Verify if number of concentration vertical levels is greater than 1
   if { $nb<2 } {
      Dialog::Error .modelnew $Error(OutCV1)
      return 0
   }

   #----- Verify if all concentration vertical levels are positive and sorted in increasing order.
   for { set i 0 } { $i < $nb } { incr i } {
      set level [lindex $Sim(OutCV) $i]

      set idx ""
      set number [string is double -strict -failindex idx $level]
      if { $number == 0 && $idx == -1 } {
         Dialog::Error .modelnew $Error(OutCVRange) " $level $Error(UnitMeters)"
         return 0
      } elseif { $number == 0 || ($number == 1 && $level < 0) } {
         Dialog::Error .modelnew $Error(OutCV2) " $level $Error(UnitMeters)"
         return 0
      }

      if { $i > 0 } {
         set prevlevel [lindex $Sim(OutCV) [expr $i - 1]]
         if { $level <= $prevlevel } {
            Dialog::Error .modelnew $Error(OutCV3) " $Sim(OutCV) $Error(UnitMeters)"
            return 0
         }
      } else {
         set firstlevel [lindex $Sim(OutCV) 0]

         if { $Sim(Model)=="MLDP1" && $firstlevel!=0.0 } {
            #----- Replace first level.
            set oldlist $Sim(OutCV)
            set firstlevel 0
            set Sim(OutCV) [lreplace $Sim(OutCV) 0 0 $firstlevel]
            Dialog::Default .modelnew 800 WARNING $Warning(OutCV1) "\n\n[lindex $Warning(OutCV2) $GDefs(Lang)] $oldlist $Error(UnitMeters)\n[lindex $Warning(OutCV3) $GDefs(Lang)] $Sim(OutCV) $Error(UnitMeters)" 0 "OK"
         }
         if { $Sim(Model)=="MLDP0" && $firstlevel!=1.0 } {
            #----- Replace first level.
            set oldlist $Sim(OutCV)
            set firstlevel 1
            set Sim(OutCV) [lreplace $Sim(OutCV) 0 0 $firstlevel]
            Dialog::Default .modelnew 800 WARNING $Warning(OutCV1) "\n\n[lindex $Warning(OutCV2) $GDefs(Lang)] $oldlist $Error(UnitMeters)\n[lindex $Warning(OutCV3) $GDefs(Lang)] $Sim(OutCV) $Error(UnitMeters)" 0 "OK"
         }
         if { $Sim(Model)=="MLDPn" && $firstlevel!=1.0 } {
            #----- Replace first level.
            set oldlist $Sim(OutCV)
            set firstlevel 1
            set Sim(OutCV) [lreplace $Sim(OutCV) 0 0 $firstlevel]
            Dialog::Default .modelnew 800 WARNING $Warning(OutCV1) "\n\n[lindex $Warning(OutCV2) $GDefs(Lang)] $oldlist $Error(UnitMeters)\n[lindex $Warning(OutCV3) $GDefs(Lang)] $Sim(OutCV) $Error(UnitMeters)" 0 "OK"
         }
      }
   }
   return 1
}

#----------------------------------------------------------------------------
# Nom        : <MLDPn::ValidateDensity>
# Creation   : 22 March 2004 - A. Malo - CMC/CMOE
#
# But        : Validate density.
#
# Parametres :
#
# Retour     :
#   <Idx>    : Flag indicating if validation has succeeded (1) or not (0).
#
# Remarques  :
#
#----------------------------------------------------------------------------

proc MLDPn::ValidateDensity { } {
   global GDefs
   variable Error
   variable Tmp

   #----- Verify if density is positive.

   set number [string is double -strict -failindex idx $Tmp(EmDensity)]

   if { $number==0 && $idx==-1 } {
      Dialog::Error .modelnew $Error(EmDensityOutRange) " $Tmp(EmDensity) [lindex $Error(UnitDensity) $GDefs(Lang)]."
      return 0
   } elseif { $number== 0 || $Tmp(EmDensity)<=0 } {
      Dialog::Error .modelnew $Error(EmDensity) " $Tmp(EmDensity) [lindex $Error(UnitDensity) $GDefs(Lang)]"
      return 0
   }

   return 1
}

#----------------------------------------------------------------------------
# Nom        : <MLDPn::ValidateMass>
# Creation   : 22 March 2004 - A. Malo - CMC/CMOE
#
# But        : Validate total mass released.
#
# Parametres :
#     <Id>  : The id of the emission interval to validate the mass from.
#
# Retour     :
#   <Idx>    : Flag indicating if validation has succeeded (1) or not (0).
#
# Remarques  :
#
#----------------------------------------------------------------------------

proc MLDPn::ValidateMass { Id } {
   global   GDefs
   variable Error
   variable Tmp

   #----- Verify if total mass released is positive.
   set number [string is double -strict -failindex idx $Tmp(EmMass.$Id)]

   if { $number == 0 && $idx == -1 } {
      Dialog::Error .emintdetails $Error(MassRange) " $Tmp(EmMass.$Id) [lindex $Error(UnitMass) $GDefs(Lang)]"
      return 0
   } elseif { $number == 0 || $Tmp(EmMass.$Id) <= 0 } {
      Dialog::Error .emintdetails $Error(Mass) " $Tmp(EmMass.$Id) [lindex $Error(UnitMass) $GDefs(Lang)]"
      return 0
   }

   return 1
}

#----------------------------------------------------------------------------
# Nom        : <MLDPn::ValidateNumberParticles>
# Creation   : 22 March 2004 - A. Malo - CMC/CMOE
#
# But        : Validate number of particles.
#
# Parametres :
#     <Id>  : The id of the emission interval to validate the number of
#             particles from. If none is specified, the global number of
#             particles to distribute is validated.
#
# Retour     :
#   <Idx>    : Flag indicating if validation has succeeded (1) or not (0).
#
# Remarques  :
#
#----------------------------------------------------------------------------

proc MLDPn::ValidateNumberParticles { {Id ""} } {
   global GDefs
   variable Error
   variable Tmp
   variable Sim

   if { $Tmp(EmIsAutoNP) } {
      set var "EmNumberParticles"
   } else {
      set var "EmNumberParticles.$Id"
   }

   #----- Verify if number of particles is positive and greater or equal to 1000.
   set number [string is integer -strict -failindex idx $Tmp($var)]

   if { $number == 0 && $idx == -1 } {
      Dialog::Error .emintdetails $Error(EmNumberParticlesOutRange) " $Tmp($var)."
      return 0
   } elseif { $number == 0 || $Tmp($var) < 1000 } {
      Dialog::Error .emintdetails $Error(EmNumberParticles) " $Tmp($var)."
      return 0
   }

   #----- Verify if number of particles is less than (or equal to) maximum number of particles.
   if { $Tmp($var) > $Sim(EmMaxNumberParticles) } {
      Dialog::Error .emintdetails $Error(EmNumberParticles3) " $Tmp($var).\n[lindex $Error(EmNumberParticles4) $GDefs(Lang)] $Sim(EmMaxNumberParticles)."
      return 0
   }

   return 1
}

#----------------------------------------------------------------------------
# Nom        : <MLDPn::ValidateOtherParams>
# Creation   : 29 June 2006 - A. Malo - CMC/CMOE
#
# But        : Validate other parameters:
#              - Vertical levels for concentration calculations [m].
#              - Horizontal wind velocity variance for mesoscale fluctuations [m2/s2].
#              - Lagrangian time scale for mesoscale fluctuations [s].
#              - Bottom reflection level of particles in the atmosphere [hyb|eta|sig].
#
# Parametres :
#
# Retour     :
#   <Idx>    : Flag indicating if validation has succeeded (1) or not (0).
#
# Remarques  :
#
#----------------------------------------------------------------------------

proc MLDPn::ValidateOtherParams { } {
   global GDefs
   variable Error
   variable Sim

   #----- Validate vertical levels for concentration calculations.
   if { ![MLDPn::ValidateVerticalLevels] } {
      focus $Sim(OutCVEnt)
      return 0
   }

   #----- Validate horizontal wind velocity variance for mesoscale fluctuations.
   if { ![MLDPn::ValidateVarianceMesoscale] } {
      focus $Sim(VarMesoscaleEnt)
      return 0
   }

   #----- Validate Lagrangian time scale for mesoscale fluctuations.
   if { ![MLDPn::ValidateTimescale] } {
      focus $Sim(TimescaleEnt)
      return 0
   }

   #----- Validate bottom reflection level of particles in the atmosphere.
   if { ![MLDPn::ValidateReflectionLevel] } {
      focus $Sim(ReflectionLevelEnt)
      return 0
   }

   #----- Update list of vertical levels according to reflection level.
   MLDPn::UpdateListVerticalLevels

   return 1
}

#----------------------------------------------------------------------------
# Nom        : <MLDPn::ValidatePlumeHeight>
# Creation   : 22 March 2004 - A. Malo - CMC/CMOE
#
# But        : Validate maximum plume height.
#
# Parametres :
#     <Id>  : The id of the emission interval to validate the height from.
#
# Retour     :
#   <Idx>    : Flag indicating if validation has succeeded (1) or not (0).
#
# Remarques  :
#
#----------------------------------------------------------------------------

proc MLDPn::ValidatePlumeHeight { Id } {
   global GDefs
   variable Error
   variable Tmp

   #----- Verify if maximum plume height is positive.
   set number [string is double -strict -failindex idx $Tmp(EmHeight.$Id)]

   if { $number == 0 && $idx == -1 } {
      Dialog::Error .emintdetails $Error(HeightRange) " $Tmp(EmHeight.$Id) $Error(UnitMeters)"
      return 0
   } elseif { $number == 0 || $Tmp(EmHeight.$Id) <= 0 } {
      Dialog::Error .emintdetails $Error(Height) " $Tmp(EmHeight.$Id) $Error(UnitMeters)"
      return 0
   }

   #----- Verify if maximum plume height is lower or equal to 30000 meters.
   if { $Tmp(EmHeight.$Id) > 30000.0 } {
      Dialog::Error .emintdetails $Error(Height2) " $Tmp(EmHeight.$Id) $Error(UnitMeters)"
      return 0
   }

   return 1
}

#----------------------------------------------------------------------------
# Nom        : <MLDPn::ValidateRadius>
# Creation   : 22 March 2004 - A. Malo - CMC/CMOE
#
# But        : Validate radius.
#
# Parametres :
#     <Id>  : The id of the emission interval to validate the radius from.
#
# Retour     :
#   <Idx>    : Flag indicating if validation has succeeded (1) or not (0).
#
# Remarques  :
#
#----------------------------------------------------------------------------

proc MLDPn::ValidateRadius { Id } {
   global GDefs
   variable Error
   variable Tmp

   #----- Verify if column radius is positive.
   set number [string is double -strict -failindex idx $Tmp(EmRadius.$Id)]

   if { $number == 0 && $idx == -1 } {
      Dialog::Error .emintdetails $Error(RadiusRange) " $Tmp(EmRadius.$Id) $Error(UnitMeters)"
      return 0
   } elseif { $number == 0 || $Tmp(EmRadius.$Id) < 0 } {
      Dialog::Error .emintdetails $Error(Radius) " $Tmp(EmRadius.$Id) $Error(UnitMeters)"
      return 0
   }

   return 1
}

#----------------------------------------------------------------------------
# Nom        : <MLDPn::ValidateReleaseRates>
# Creation   : Juillet 2010 - E. Legault-Ouellet - CMC/CMOE
#
# But        : Validate the release rate.
#
# Parametres :
#     <Rate> : The rate to be validated.
#
# Retour     :
#   <Idx>    : Flag indicating if validation has succeeded (1) or not (0).
#
# Remarques  :
#     - Cette fonction est essentiellement une reprise de code d'une fonction
#       écrite par Alain Malo.
#
#----------------------------------------------------------------------------

proc MLDPn::ValidateReleaseRate { Rate } {
   global GDefs
   variable Error

   set number [string is double -failindex idx $Rate]
   if { $number == 0 && $idx == -1 } {
      Dialog::Error .emintdetails $Error(ReleaseRateRange) " $Rate [lindex $Error(UnitRate) $GDefs(Lang)]"
      return 0
   } elseif { $number == 0 || ($Rate < 0 && $Rate != "") } {
      Dialog::Error .emintdetails $Error(ReleaseRate) " $Rate [lindex $Error(UnitRate) $GDefs(Lang)]"
      return 0
   }
   return 1
}

#----------------------------------------------------------------------------
# Nom        : <MLDPn::ValidateDuration>
# Creation   : Juillet 2010 - E. Legault-Ouellet - CMC/CMOE
#
# But        : Validate the duration of an emission interval.
#
# Parametres :
#     <Duration>  : The duration to be validated (in hours).
#     <Parent>    : The parent that called this function.
#
# Retour     :
#   <Idx>    : Flag indicating if validation has succeeded (1) or not (0).
#
# Remarques  :
#     - Cette fonction est essentiellement une reprise de code d'une fonction
#       écrite par Alain Malo.
#
#----------------------------------------------------------------------------

proc MLDPn::ValidateDuration { Duration Parent } {
   global GDefs
   variable Error
   variable Sim

   #----- Verify if duration is positive.
   set number [string is double -strict -failindex idx $Duration]
   set dur    [expr $Duration/3600.0]

   if { $number == 0 && $idx == -1 } {
      Dialog::Error $Parent $Error(DurationOutRange) " $dur [lindex $Error(UnitHours) $GDefs(Lang)]"
      return 0
   } elseif { $number == 0 || ($number == 1 && $dur <= 0 && $dur != "") } {
      Dialog::Error $Parent $Error(Duration) " $dur $Error(UnitHours)"
      return 0
   }

   #----- Compute  model time step [s].
   set timestephour [expr $Sim(ModelTimeStepMin)/60.0]

   #----- Verify if duration is greater or equal to the model time step.
   if { $dur < $timestephour } {
      Dialog::Error $Parent $Error(Duration2) "[lindex $Error(Duration4) $GDefs(Lang)] $dur $Error(UnitHours)\n[lindex $Error(Duration5) $GDefs(Lang)] $timestephour $Error(UnitHours) ($Sim(ModelTimeStepMin) $Error(UnitMinutes))"
      return 0
   }

   #----- Verify if duration is an integer multiple of the model time step.
   if { [expr fmod($dur, $timestephour)] > $Sim(EmEpsilon) } {
      Dialog::Error $Parent $Error(Duration3) "[lindex $Error(Duration4) $GDefs(Lang)] $dur $Error(UnitHours)\n[lindex $Error(Duration5) $GDefs(Lang)] $timestephour $Error(UnitHours) ($Sim(ModelTimeStepMin) $Error(UnitMinutes))"
      return 0
   }

   return 1
}

#----------------------------------------------------------------------------
# Nom        : <MLDPn::ValidateReflectionLevel>
# Creation   : 30 June 2006 - A. Malo - CMC/CMOE
#
# But        : Validate bottom reflection level [hyb|eta|sig] of particles
#              in the atmosphere.
#
# Parametres :
#
# Retour     :
#   <Idx>    : Flag indicating if validation has succeeded (1) or not (0).
#
# Remarques  :
#
#----------------------------------------------------------------------------

proc MLDPn::ValidateReflectionLevel { } {
   global GDefs
   variable Error
   variable Sim

   #----- Verify if reflection level is positive.
   set number [string is double -strict -failindex idx $Sim(ReflectionLevel)]

   if { $number == 0 && $idx == -1 } {
      Dialog::Error .modelnew $Error(ReflectionLevelRange) " $Sim(ReflectionLevel) $Error(UnitHybEtaSig)"
      return 0
   } elseif { $number == 0 || $Sim(ReflectionLevel) <= 0.0 } {
      Dialog::Error .modelnew $Error(ReflectionLevel) " $Sim(ReflectionLevel) $Error(UnitHybEtaSig)"
      return 0
   }

   #----- Verify if reflection level falls within the range [0.9900, 1.0000].
   if { $Sim(ReflectionLevel) > 1.0 || $Sim(ReflectionLevel) < 0.9900 } {
      Dialog::Error .modelnew $Error(ReflectionLevel2) " $Sim(ReflectionLevel) $Error(UnitHybEtaSig)"
      return 0
   }

   return 1
}

#----------------------------------------------------------------------------
# Nom        : <MLDPn::ValidateTimescale>
# Creation   : 16 November 2007 - A. Malo - CMC/CMOE
#
# But        : Validate Lagrangian time scale (s) for
#              mesoscale fluctuations.
#
# Parametres :
#
# Retour     :
#   <Idx>    : Flag indicating if validation has succeeded (1) or not (0).
#
# Remarques  :
#
#----------------------------------------------------------------------------

proc MLDPn::ValidateTimescale { } {
   global GDefs
   variable Error
   variable Sim

   #----- Verify if time scale is positive.
   set number [string is double -strict -failindex idx $Sim(Timescale)]

   if { $number == 0 && $idx == -1 } {
      Dialog::Error .modelnew $Error(TimescaleRange) " $Sim(Timescale) $Error(UnitSeconds)"
      return 0
   } elseif { $number == 0 || $Sim(Timescale) <= 0.0 } {
      Dialog::Error .modelnew $Error(Timescale) " $Sim(Timescale) $Error(UnitSeconds)"
      return 0
   }

   #----- Verify if timescale is lower or equal to 21600 s.
   if { $Sim(Timescale) > 21600.0 } {
      Dialog::Error .modelnew $Error(Timescale2) " $Sim(Timescale) $Error(UnitSeconds)"
      return 0
   }

   return 1
}

#----------------------------------------------------------------------------
# Nom        : <MLDPn::ValidateVarianceMesoscale>
# Creation   : 29 June 2006 - A. Malo - CMC/CMOE
#
# But        : Validate horizontal wind velocity variance (m2/s2) for
#              mesoscale fluctuations.
#
# Parametres :
#
# Retour     :
#   <Idx>    : Flag indicating if validation has succeeded (1) or not (0).
#
# Remarques  :
#
#----------------------------------------------------------------------------

proc MLDPn::ValidateVarianceMesoscale { } {
   global GDefs
   variable Error
   variable Sim

   #----- Verify if variance is positive.
   set number [string is double -strict -failindex idx $Sim(VarMesoscale)]

   if { $number == 0 && $idx == -1 } {
      Dialog::Error .modelnew $Error(VarMesoscaleRange) " $Sim(VarMesoscale) $Error(UnitM2PS2)"
      return 0
   } elseif { $number == 0 || $Sim(VarMesoscale) < 0.0 } {
      Dialog::Error .modelnew $Error(VarMesoscale) " $Sim(VarMesoscale) $Error(UnitM2PS2)"
      return 0
   }

   #----- Verify if variance is lower or equal to 10.0 m2/s2.
   if { $Sim(VarMesoscale) > 10.0 } {
      Dialog::Error .modelnew $Error(VarMesoscale2) " $Sim(VarMesoscale) $Error(UnitM2PS2)"
      return 0
   }

   return 1
}

#----------------------------------------------------------------------------
# Nom        : <MLDPn::ValidateSimulationDuration>
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

proc MLDPn::ValidateSimulationDuration { } {
   global GDefs
   variable Sim
   variable Error
   variable Warning
   variable Lbl

   #----- Verify if output time step is positive.
   set number [string is integer -strict -failindex idx $Sim(Duration)]
   if { $number == 0 && $idx == -1 } {
      Dialog::Error .modelnew $Error(SimDurationOutRange) " $Sim(Duration) $Error(UnitHours)"
      focus $Sim(SimDurationEnt)
      return 0
   } elseif { $number == 0 || ($number == 1 && $Sim(Duration) < 1) } {
      Dialog::Error .modelnew $Error(SimDuration) " $Sim(Duration) $Error(UnitHours)"
      focus $Sim(SimDurationEnt)
      return 0
   }

   #----- Verify if simulation duration is greater than (or equal to) time interval between met data files.
   if { $Sim(Duration) < $Sim(Delta) } {
      Dialog::Error .modelnew $Error(SimDurationDelta) "[lindex $Error(SimDurationDelta2) $GDefs(Lang)] $Sim(Duration) $Error(UnitHours)\n[lindex $Error(SimDurationDelta3) $GDefs(Lang)] $Sim(Delta) $Error(UnitHours)"
      focus $Sim(SimDurationEnt)
      return 0
   }

   #----- Verify if simulation duration is greater than output time step.
   set ots [expr double($Sim(OutputTimeStepMin))/double(60.0)] ; #----- Output time step [hr].
   if { $Sim(Duration) < $ots } {
      Dialog::Error .modelnew $Error(SimDurationOutputTimeStep) "[lindex $Error(SimDurationOutputTimeStep2) $GDefs(Lang)] $Sim(Duration) $Error(UnitHours)\n[lindex $Error(SimDurationOutputTimeStep3) $GDefs(Lang)] $ots $Error(UnitHours)"
      focus $Sim(SimDurationEnt)
      return 0
   }

   #----- Display warning if simulation duration > 72 hrs and output time step <= 60 min.
   if { !$Sim(IsResFileSizeChecked) } {
      if { $Sim(Duration) > 72 && $Sim(OutputTimeStepMin) <= 60 } {
         set yes [Dialog::Default .modelnew 400 WARNING $Warning(SimDuration4) "\n\n[lindex $Warning(SimDuration5) $GDefs(Lang)] $Sim(Duration) $Error(UnitHours)\n[lindex $Warning(SimDuration6) $GDefs(Lang)] $Sim(OutputTimeStepMin) $Error(UnitMinutes)" 0 $Lbl(No) $Lbl(Yes)]
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
         set yes [Dialog::Default .modelnew 400 WARNING $Warning(SimDuration7) "\n\n[lindex $Warning(SimDuration8) $GDefs(Lang)] $Sim(Duration) $Error(UnitHours)\n[lindex $Warning(SimDuration9) $GDefs(Lang)] $Sim(Delta) $Error(UnitHours)" 0 $Lbl(No) $Lbl(Yes)]
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
# Nom        : <MLDPn::ValidateTimeSteps>
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

proc MLDPn::ValidateTimeSteps { } {
   global GDefs
   variable Sim
   variable Error

   #----- Verify if output time step is positive.
   set number [string is integer -strict -failindex idx $MLDPn::Sim(OutputTimeStepMin)]
   if { $number == 0 && $idx == -1 } {
      Dialog::Error .modelnew $Error(OutputTimeStepMinOutRange)] " $Sim(OutputTimeStepMin) $Error(UnitMinutes)"
      focus $Sim(OutputTimeStepEnt)
      return 0
   } elseif { $number == 0 || ($number == 1 && $MLDPn::Sim(OutputTimeStepMin) <= 0) } {
      Dialog::Error .modelnew $Error(OutputTimeStepMin) " $Sim(OutputTimeStepMin) $Error(UnitMinutes)"
      focus $Sim(OutputTimeStepEnt)
      return 0
   }

   #----- Verify if model time step is positive.
   set number [string is integer -strict -failindex idx $MLDPn::Sim(ModelTimeStepMin)]
   if { $number == 0 && $idx == -1 } {
      Dialog::Error .modelnew $Error(ModelTimeStepMinOutRange) " $Sim(ModelTimeStepMin) $Error(UnitMinutes)"
      focus $Sim(ModelTimeStepEnt)
      return 0
   } elseif { $number == 0 || ($number == 1 && $MLDPn::Sim(ModelTimeStepMin) <= 0) } {
      Dialog::Error .modelnew $Error(ModelTimeStepMin) " $Sim(ModelTimeStepMin) $Error(UnitMinutes)"
      focus $Sim(ModelTimeStepEnt)
      return 0
   }

   #----- Verify if model time step is lower or equal to an hour (60 minutes).
   if { $MLDPn::Sim(ModelTimeStepMin) > 60 } {
      Dialog::Error .modelnew $Error(ModelTimeStep1hour) " $Sim(ModelTimeStepMin) $Error(UnitMinutes)"
      focus $Sim(ModelTimeStepEnt)
      return 0
   }

   #----- Verify if model time step is an integer divisor of an hour (60 minutes).
   if { [expr fmod( 60, $MLDPn::Sim(ModelTimeStepMin) )] > $MLDPn::Sim(EmEpsilon) } {
      Dialog::Error .modelnew $Error(ModelTimeStep1hourDiv) " $Sim(ModelTimeStepMin) $Error(UnitMinutes)"
      focus $Sim(ModelTimeStepEnt)
      return 0
   }

   #----- Verify if model time step is lower or equal to the output time step.
   if { $MLDPn::Sim(ModelTimeStepMin) > $MLDPn::Sim(OutputTimeStepMin) } {
      Dialog::Error .modelnew $Error(ModelTimeStepOutputTimeStep) "[lindex $Error(ModelTimeStepOutputTimeStep2) $GDefs(Lang)] $Sim(ModelTimeStepMin) $Error(UnitMinutes)\n[lindex $Error(ModelTimeStepOutputTimeStep3) $GDefs(Lang)] $Sim(OutputTimeStepMin) $Error(UnitMinutes)"
      focus $Sim(ModelTimeStepEnt)
      return 0
   }

   #----- Verify if output time step is an integer multiple of the model time step.
   if { [expr fmod( $MLDPn::Sim(OutputTimeStepMin), $MLDPn::Sim(ModelTimeStepMin) )] > $MLDPn::Sim(EmEpsilon) } {
      Dialog::Error .modelnew $Error(OutputTimeStepModelTimeStep) "[lindex $Error(OutputTimeStepModelTimeStep2) $GDefs(Lang)] $Sim(OutputTimeStepMin) $Error(UnitMinutes)\n[lindex $Error(OutputTimeStepModelTimeStep3) $GDefs(Lang)] $Sim(ModelTimeStepMin) $Error(UnitMinutes)"
      focus $Sim(OutputTimeStepEnt)
      return 0
   }

   #----- Update emission starting time.
   Model::FitAccTime MLDPn

   return 1
}

#----------------------------------------------------------------------------
# Nom        : <MLDPn::ValidateTotalNumberParticles>
# Creation   : Juillet 2010 - E. Legault-Ouellet - CMC/CMOE
#
# But        : Validate the total number of particles.
#
# Parametres :
#     <Id>  : The id of the current emission interval.
#
# Retour     : 0 if validation fails, 1 if validation succeed.
#
# Remarques  :
#
#----------------------------------------------------------------------------

proc MLDPn::ValidateTotalNumberParticles { Id } {
   global GDefs
   variable Sim
   variable Tmp
   variable Warning
   variable Lbl
   variable Error

   #----- Count Total number of particles
   if { $Tmp(EmIsAutoNP) } {
      set totnp $Sim(EmNumberParticles)
   } else {
      #----- Count previous particles from previous "stable" parameters
      set totnp 0
      for { set i $Sim(EmInterStart) } { $i < $Sim(EmNbIntervals) } { incr i } {
         if { $Sim(EmIsEm.$i) } {
            set totnp [expr $totnp + $Sim(EmNumberParticles.$i)]
         }
      }

      #----- Ajust based on current modifications
      if { $Id < $Sim(EmNbIntervals) } {
         set totnp [expr $totnp - ($Sim(EmIsEm.$Id)?$Sim(EmNumberParticles.$Id):0) + ($Tmp(EmIsEm.$Id)?$Tmp(EmNumberParticles.$Id):0)]
      } else {
         set totnp [expr $totnp + ($Tmp(EmIsEm.$Id)?$Tmp(EmNumberParticles.$Id):0)]
      }
   }

   #----- Check if number of particles is greater than the maximum allowed
   if { $totnp > $Sim(EmMaxNumberParticles) } {
      if { $Tmp(EmIsEm.$Id) } {
         #----- Calculate allowed number of particles
         set newnp [expr ($Tmp(EmIsAutoNP)?$Tmp(EmNumberParticles):$Tmp(EmNumberParticles.$Id)) + $Sim(EmMaxNumberParticles) - $totnp]
         if { $newnp <= 0 } {
            #----- We can't correct the problem at this level
            Dialog::Error .emintdetails $Error(EmNumberParticles3) "$totnp\n[lindex $Error(EmNumberParticles4) $GDefs(Lang)] $Sim(EmMaxNumberParticles)"
            return 0
         }

         #----- Ask user if we correct number of paricles
         if { [Dialog::Default .emintdetails 400 WARNING $Warning(NP_gt_Max) "\n$Tmp(EmNumberParticles.$Id) --> $newnp" 0 $Lbl(Yes) $Lbl(No)] } {
            return 0
         }

         #----- Correct number of particles
         if { $Tmp(EmIsAutoNP) } {
            set totnp $newnp
            set Tmp(EmNumberParticles) $newnp
         } else {
            set totnp [expr $totnp - $Tmp(EmNumberParticles.$Id) + $newnp]
            set Tmp(EmNumberParticles.$Id) $newnp
         }
      } else {
         #----- If there is no emission for this interval, we can't make the correction
         Dialog::Error .emintdetails $Error(EmNumberParticles3) "$totnp\n[lindex $Error(EmNumberParticles4) $GDefs(Lang)] $Sim(EmMaxNumberParticles)"
         return 0
      }
   }

   set Tmp(EmTotalNumberParticles) $totnp
   return 1
}

#----------------------------------------------------------------------------
# Nom        : <MLDPn::ValidateEmissionDuration>
# Creation   : Juillet 2010 - E. Legault-Ouellet - CMC/CMOE
#
# But        : Validate emission duration.
#
# Parametres :
#     <Id>  : The id of the current emission interval.
#
# Retour     : 0 if validation fails, 1 if validation succeed.
#
# Remarques  :
#
#----------------------------------------------------------------------------

proc MLDPn::ValidateEmissionDuration { Id } {
   variable Tmp
   variable Sim
   variable Lbl
   variable Warning
   variable Error

   #----- Calculate total and effective emission duration of previous intervals
   set totdur 0
   set effdur 0
   for { set i 0 } { $i < $Sim(EmNbIntervals) } { incr i } {
      set totdur [expr $totdur + $Sim(EmInter.$i)]
      if { $Sim(EmIsEm.$i) } {
         set effdur [expr $effdur + $Sim(EmInter.$i)]
      }
   }

   set em [expr $Tmp(EmInter.$Id)*$Sim(EmInterMode)]

   #----- Ajust with current modifications
   if { $Id < $Sim(EmNbIntervals) } { ;#----- Edition mode
      set totdur [expr $totdur - $Sim(EmInter.$Id) + $em]
      set effdur [expr $effdur - ($Sim(EmIsEm.$Id)?$Sim(EmInter.$Id):0) + ($Tmp(EmIsEm.$Id)?$em:0)]
   } else { ;#----- Adding mode
      set totdur [expr $totdur + $em]
      set effdur [expr $effdur + $Tmp(EmIsEm.$Id)?$em:0]
   }

   set Tmp(EmEffectiveDuration) $effdur
   set Tmp(EmTotalDuration)     $totdur
}

#----------------------------------------------------------------------------
# Nom        : <MLDPn::ValidateScenarioDuration>
# Creation   : Juillet 2010 - E. Legault-Ouellet - CMC/CMOE
#
# But        : Validate scenario's emission duration.
#
# Parametres :
#
# Retour     : 0 if validation fails, 1 if validation succeeds.
#
# Remarques  :
#
#----------------------------------------------------------------------------

proc MLDPn::ValidateScenarioDuration { } {
   global GDefs
   variable Error
   variable Sim

   #----- Calculate real emission duration
   set totdur 0
   set effdur 0
   for { set i 0 } { $i < $Sim(EmNbIntervals) } { incr i } {
      set totdur [expr $totdur + $Sim(EmInter.$i)]

      if { $Sim(EmIsEm.$i) } {
         set effdur [expr $effdur + $Sim(EmInter.$i)]
      }
   }

   set Sim(EmEffectiveDuration)  $effdur
   set Sim(EmTotalDuration)      $totdur

   return 1
}

#----------------------------------------------------------------------------
# Nom        : <MLDPn::ValidateDurationsVsModelTimeStep>
# Creation   : 22 March 2004 - A. Malo - CMC/CMOE
# Changed    : Juillet 2010 - E. Legault-Ouellet - CMC/CMOE
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

proc MLDPn::ValidateDurationsVsModelTimeStep { } {
   variable Sim

   for { set i $Sim(EmInterStart) } { $i < $Sim(EmNbIntervals) } { incr i } {

      #----- Validate emission duration value.
      if { ![MLDPn::ValidateDuration $Sim(EmInter.$i) .modelnew] } {
         return 0
      }
   }
   return 1
}

#----------------------------------------------------------------------------
# Nom        : <MLDPn::ValidateLullPeriods>
# Creation   : Juillet 2010 - E. Legault-Ouellet - CMC/CMOE
#
# But        : Validate the position of lull periods in the emission timeline.
#
# Parametres :
#
# Retour     : 0 if validation fails, 1 if validation succeed.
#
# Remarques  :
#
#----------------------------------------------------------------------------

proc MLDPn::ValidateLullPeriods { } {
   variable Sim
   variable Error

   set lst {}

   #----- Check for lull periods at the beginning of the scenario
   if { $Sim(Restart)=="" } {
      for { set i 0 } { $i < $Sim(EmNbIntervals) } { incr i } {
         if { $Sim(EmIsEm.$i) } {
            break
         } else {
            lappend lst [expr $i + 1]
         }
      }

      if { [llength $lst] } {
         Dialog::Error .modelnew $Error(LullAtStart) [join $lst ", "]
         return 0
      }

      #----- Check for lull periods at the end of the scenario
      for { set i [expr $Sim(EmNbIntervals)-1] } { $i >= $Sim(EmInterStart) } { incr i -1 } {
         if { $Sim(EmIsEm.$i) } {
            break
         } else {
            lappend lst [expr $i + 1]
         }
      }

      if { [llength $lst] } {
         Dialog::Error .modelnew $Error(LullAtEnd) [join $lst ", "]
         return 0
      }
   }

   return 1
}

#----------------------------------------------------------------------------
# Nom        : <MLDPn::ValidateEmissionQuantity>
# Creation   : Juillet 2010 - E. Legault-Ouellet - CMC/CMOE
#
# But        : Validate the total quantity emitted per isotope.
#
# Parametres :
#
# Retour     : 0 if validation fails, 1 if validation succeed.
#
# Remarques  :
#
#----------------------------------------------------------------------------

proc MLDPn::ValidateEmissionQuantity { } {
   global GDefs
   variable Sim
   variable Error

   set lst {}

   switch $Sim(SrcType) {
      "VOLCANO" {
         return 1
      }
      "VIRUS" {
         set quantity 0.0

         #----- Compute total release quantity.
         for { set i $Sim(EmInterStart) } { $i < $Sim(EmNbIntervals) } { incr i } {
            set quantity [expr $quantity + $Sim(EmInter.$i) * double($Sim(EmRate.$i))]
         }

         #----- Make sure we arrive at a positive value
         if { $quantity<0.0 || $quantity==0.0 && $Sim(Restart)=="" } {
            Dialog::Error .modelnew $Error(TotalQuantityVirus)
            return 0
         }

         set Sim(EmIsoQuantity) [format "%.7e" $quantity]
      }
      "ACCIDENT" {
         #----- Initialize total release quantity.
         for { set j 0 } { $j < $Sim(EmNbIso) } { incr j } {
            set quantity($j) 0
         }

         #----- Compute total release quantity for each isotope.
         for { set j 0 } { $j < $Sim(EmNbIso) } { incr j } {
            for { set i $Sim(EmInterStart) } { $i < $Sim(EmNbIntervals) } { incr i } {
               set quantity($j) [expr $quantity($j) + $Sim(EmInter.$i) * double($Sim(EmRate.$i.$j))]
            }

            if { $quantity($j)<0.0 || $quantity($j)==0.0 && $Sim(Restart)==""  } {
               Dialog::Error .modelnew $Error(TotalQuantityAccident) "[lindex $Error(TotalQuantity2) $GDefs(Lang)] [lindex [lindex $Sim(EmIsoInfo) $j] 0]"
               return 0
            }
         }

         #----- Build list of total release quantity for each isotope.
         set Sim(EmIsoQuantity) {}
         for { set j 0 } { $j < $Sim(EmNbIso) } { incr j } {
            lappend Sim(EmIsoQuantity) [format "%.7e" $quantity($j)]
         }
      }
   }

   return 1
}

