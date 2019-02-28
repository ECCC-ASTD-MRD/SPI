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

Log::Start [info script] 0.2
Log::Print INFO "Using $env(SPI_PATH)\n"

#----- Pending tests
#   {FSTD_Voir.tcl pollux:/home/afsr/005/public_html/SPI/Script/DataIn/2005102612_012c}
#   FSTD_ImagePreview.tcl
#   SIM_Traj.tcl

set tests {
   {FSTD_Head.tcl $env(CI_SPI_IN)/2005102612_012c}
   {FSTD_Voir.tcl $env(CI_SPI_IN)/2005102612_012c}
   {FSTD_CheckInsideness.tcl $env(CI_SPI_IN)/2005102612_012c DataIn/latlon.txt}
   FSTD_Funcs.tcl
   FSTD_GridFunc.tcl
   FSTD_GetPressure.tcl
   FSTD_8HourAverage.tcl
   FSTD_CalcES2HR.tcl
   FSTD_DrainDensity.tcl
   FSTD_ExtractValue.tcl
   FSTD_WindDirProfile.tcl
   FSTD_Flux.tcl
   FSTD_IncrIP2.tcl
   FSTD_InfoText.tcl
   FSTD_InterpCube.tcl
   FSTD_InterpHorizontal.tcl
   FSTD_InterpConservative.tcl
   FSTD_InterpTime.tcl
   FSTD_InterpYY.tcl
   FSTD_ETA2SIGMA.tcl
   FSTD_ETA2METER.tcl
   {FSTD_HYBRID2ETA.tcl $env(CI_SPI_IN)/2006122900_000.hyb}
   {FSTD_ETA2HYBRID.tcl $env(CI_SPI_IN)/2006122900_000.eta}
   {FSTD_HYBRID2PRESSURE.tcl $env(CI_SPI_IN)/2012041712_024.zfst}
   FSTD_Hyb-Thermo+Momentum2MAGL.tcl
   FSTD_PSGrid.tcl
   FSTD_TestTypes.tcl
   {FSTD_TIN2FSTD.tcl $env(CI_SPI_IN)/hudson}
   FSTD_Sort.tcl
   FSTD_Y2Z_SUM.tcl
   FSTD_ThreadDemo.tcl
   FSTD_Profiler.tcl
   FSTD_IOAPI2RPN.tcl
   FSTD_Untile.tcl
   FSTD_Dict.tcl
   {FSTD_Hull.tcl -fstd $env(CI_SPI_IN)/2005102612_012 -var ES -min 10 -max 30 -ip1 12000 -buffer 1 -dist 4 -out $env(CI_SPI_OUT)}
   {FSTD_RPN2GDAL.tcl -format GTiff -mode DATA -var TT -ip1 12000 -fstd $env(CI_SPI_IN)/2006122900_000.eta -out $env(CI_SPI_OUT)/FSTD_RPN2GDAL}
   {FSTD_RPN2OGR.tcl -format "ESRI Shapefile" -mode CONTOUR -var TT -ip1 12000 -fstd $env(CI_SPI_IN)/2006122900_000.eta -inter -20 -10 0 10 20 -out $env(CI_SPI_OUT)/FSTD_RPN2OGR}
   GRIB_Funcs.tcl
   GRIB_Convert.tcl
   GDAL_Basic.tcl
   GDAL_Functions.tcl
   GDAL_Slope.tcl
   GDAL_CoordTest.tcl
   {GDAL_PixelCoord.tcl $env(CI_SPI_IN)/srtm_n045w074_badmedian3x3 0 0 100 100}
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
   {OGR_Export2txt.tcl $env(CI_SPI_IN)/land_bg_p.shp}
   RADAR_Functions.tcl
   RADAR_EchoTop.tcl
   {RADAR_NUMERIC2FSTD.tcl $env(CI_SPI_IN)/201306031850~NUMERIC_PRECIPET_USACDN_PS_4KM~PRECIPET,125,18,MPRATE_QPE,PRECIPET_QC_PARAMETERS_CMC:URP:USACDN_PS_4km_full:RADAR:NUMERIC}
   MDL_Functions.tcl
   TCL_ProjectionData.tcl
   TCL_Vector.tcl
   {TCL_System.tcl /tmp}
   {BUL_FXCN3X_to_SHP.tcl $env(CI_SPI_IN)/FXCN3X/20111003175406.Ophelia.technical.txt.en}
   VEXPR_NaN.tcl
   VEXPR_Stats.tcl
}

catch { file delete TCL_TestAll*.log }
fconfigure stdout -buffering none

set nok 0
set n   0
set nb  [llength $tests]

set RED     "\x1b\[31m"
set GREEN   "\x1b\[32m"
set RESET   "\x1b\[0m"

foreach test $tests {

   incr n
   puts -nonewline [format "%-35s %02i/%02i " [lindex $test 0] $n $nb]
   set s [clock seconds]

   if { [catch { eval exec ./$test >>& TCL_TestAll.log }]  } {
      puts -nonewline "${RED}Failed${RESET}"
      incr nok
   } else {
      puts -nonewline "${GREEN}Passed${RESET}"
   }

   puts " ([expr [clock seconds]-$s]s)"
}

puts "\n$nok of $nb test failed\n"

Log::End $nok
