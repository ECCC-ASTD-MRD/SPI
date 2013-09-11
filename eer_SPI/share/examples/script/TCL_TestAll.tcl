#!/bin/sh
# the next line restarts using tclsh \
exec $SPI_PATH/tclsh "$0" "$@"
#============================================================================
# Environnement Canada
# Centre Meteorologique Canadien
# 2100 Trans-Canadienne
# Dorval, Quebec
#
# Projet     : Exemple de scripts.
# Fichier    : TCL_TestAll.tcl
# Creation   : Decembre 2009 - J.P. Gauthier - CMC/CMOE
# Description: Execution des test des API TCL
#
# Parametres :
#
# Retour:
#
# Remarques  :
#
#============================================================================

package require Logger

set env(AFSISIO) /home/binops/afsi/sio/env_linux+/afsisio

Log::Start [info script] 0.1
Log::Print INFO "Using $env(SPI_PATH)\n"
set tests {
   {FSTD_Head.tcl DataIn/2005102612_012c}
   {FSTD_Voir.tcl DataIn/2005102612_012c}
   {FSTD_Voir.tcl pedro:/home/afsr/005/public_html/SPI/Script/DataIn/2005102612_012c}
   {FSTD_CheckInsideness.tcl DataIn/2005102612_012c DataIn/latlon.txt}
   FSTD_Funcs.tcl
   FSTD_GridFunc.tcl
   FSTD_GetPressure.tcl
   FSTD_8HourAverage.tcl
   FSTD_CalcES2HR.tcl
   FSTD_DrainDensity.tcl
   FSTD_ExtractValue.tcl
   FSTD_WindDirProfile.tcl
   FSTD_Flux.tcl
   FSTD_ImagePreview.tcl
   FSTD_IncrIP2.tcl
   FSTD_InfoText.tcl
   FSTD_InterpCube.tcl
   FSTD_InterpHorizontal.tcl
   FSTD_InterpConservative.tcl
   FSTD_InterpTime.tcl
   FSTD_ETA2SIGMA.tcl
   FSTD_ETA2METER.tcl
   {FSTD_HYBRID2ETA.tcl DataIn/2006122900_000.hyb}
   {FSTD_ETA2HYBRID.tcl DataIn/2006122900_000.eta}
   {FSTD_HYBRID2PRESSURE.tcl DataIn/2012041712_024.zfst}
   FSTD_Hyb-Thermo+Momentum2MAGL.tcl
   FSTD_PSGrid.tcl
   FSTD_TestTypes.tcl
   {FSTD_TIN2FSTD.tcl DataIn/hudson}
   FSTD_Sort.tcl
   FSTD_Y2Z_SUM.tcl
   FSTD_ThreadDemo.tcl
   FSTD_Profiler.tcl
   GDAL_Basic.tcl
   GDAL_Functions.tcl
   GDAL_Slope.tcl
   {GDAL_PixelCoord.tcl DataIn/srtm_n045w074_badmedian3x3 0 0 100 100}
   OBS_BUFR.tcl
   OBS_BURP.tcl
   OBS_CSV2OBS.tcl
   OBS_Extract.tcl
   OBS_Interp.tcl
   OBS_Krig.tcl
   OBS_UTM2LL.tcl
   OBS_ZKrig.tcl
   OGR_Formats.tcl
   OGR_Basic.tcl
   OGR_Create.tcl
   OGR_Functions.tcl
   OGR_ASC2SHP.tcl
   OGR_Import.tcl
   OGR_Interp.tcl
   {OGR_LatLon.tcl 20.0}
   OGR_OBS2SHP.tcl
   OGR_FCST2SHP.tcl
   OGR_OGR2FSTD.tcl
   OGR_Rasterize.tcl
   OGR_Simplify.tcl
   OGR_TZCheck.tcl
   OGR_PoesFrequency2FSTD.tcl
   {OGR_Export2txt.tcl DataIn/land_bg_p.shp}
   RADAR_Functions.tcl
   RADAR_EchoTop.tcl
   {RADAR_NUMERIC2FSTD.tcl DataIn/201306031850~NUMERIC_PRECIPET_USACDN_PS_4KM~PRECIPET,125,18,MPRATE_QPE,PRECIPET_QC_PARAMETERS_CMC:URP:USACDN_PS_4km_full:RADAR:NUMERIC}
   TCL_ProjectionData.tcl
   TCL_Vector.tcl
   {TCL_System.tcl /tmp}
   {BUL_FXCN3X_to_SHP.tcl DataIn/FXCN3X/20111003175406.Ophelia.technical.txt.en}
   SIM_Traj.tcl
}

catch { file delete DataOut/TCL_TestAll.log }
fconfigure stdout -buffering none

set nok 0
set n   0
set nb  [llength $tests]

foreach test $tests {

   incr n
   puts -nonewline [format "%-35s %02i/%02i " [lindex $test 0] $n $nb]
   set s [clock seconds]

   if { [catch { eval exec ./$test >>& DataOut/TCL_TestAll.log }]  } {
      puts -nonewline "Failed"
      incr nok
   } else {
      puts -nonewline "Passed"
   }

   puts " ([expr [clock seconds]-$s]s)"
}

puts "\n$nok of $nb test failed\n"

Log::End 0