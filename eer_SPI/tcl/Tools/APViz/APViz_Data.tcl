#===============================================================================
# Environnement Canada
# Centre Meteorologique Canadian
# 2100 Trans-Canadienne
# Dorval, Quebec
#
# Projet   : Package d'interface pour SPI
# Fichier  : APViz_Data.tcl
# Creation : Juin 2018
#
# Description:
#
#    Descriptions des variables internes du module
#
#===============================================================================

namespace eval APViz {
   global env
   global GDefs
   variable Data
   variable DataSrc
   
   #----- Donnees pour les couches de calcul
   set Data(FormulaNames) 	[list Difference Abs_difference Sum Sum_All Sum_Checked Average_All Average_Checked]		; # Nom des formules
   set Data(Formulas)		[list A-B abs(A-B) A+B sum(ALL) sum(CHECKED) avg(ALL) avg(CHECKED)]				; # Formules correspondantes
   
   set Data(Colors)		{black pink blue green red}		; # Couleurs pour chaque variable additionnelle pour chaque type (GZ, HU, TT et UU)

   #----- Data Sources
   set DataSrc(Colormaps)	/users/dor/afsm/cng/APViz_Configs/Config/Colormap/
   
   set mirrorgrid 		$env(CMCPROD)/hubs/suites
   set mirrorsat 		$env(CMCPROD)/hubs/sat
   set mirrorradar 		$env(CMCPROD)/hubs/radar/post
   
   set DataSrc(RDPS,pres)	$mirrorgrid/ops/rdps/r1/gridpt.usr/prog/pres
   set DataSrc(RDPS,diag)	$mirrorgrid/ops/rdps/r1/gridpt.usr/prog/diag
   set DataSrc(RDPS,eta)	$mirrorgrid/ops/rdps/r1/gridpt.usr/prog/eta
   set DataSrc(RDPS,hyb)	$mirrorgrid/ops/rdps/r1/gridpt/prog/hyb   
   
   set DataSrc(GDPS,pres)	$mirrorgrid/ops/gdps/g1/gridpt.usr/prog/pres
   set DataSrc(GDPS,diag)	$mirrorgrid/ops/gdps/g1/gridpt.usr/prog/diag
   set DataSrc(GDPS,eta)	$mirrorgrid/ops/gdps/g1/gridpt.usr/prog/eta
   set DataSrc(GDPS,hyb)	$mirrorgrid/ops/gdps/g1/gridpt/prog/hyb
   
   set DataSrc(REPS,pres)	$mirrorgrid/ops/reps/gridpt/prog/ens.regpres
   set DataSrc(REPS,model)	$mirrorgrid/ops/reps/gridpt/prog/ens.regmodel
   set DataSrc(REPS,diag)	$mirrorgrid/ops/reps/gridpt/prog/ens.regdiag
   
   set DataSrc(GEPS,pres)	$mirrorgrid/ops/geps/e1/gridpt/prog/ens.glbclim/refcst/pres
   set DataSrc(GEPS,model)	$mirrorgrid/ops/geps/e1/gridpt/prog/ens.glbclim/refcst/model
   set DataSrc(GEPS,diag)	$mirrorgrid/ops/geps/e1/gridpt/prog/ens.glbclim/refcst/diag
   
   #----- Observations
   set DataSrc(OBS,SHEF)	$env(CMCADE)/dbase/surface/shef
   set DataSrc(OBS,SYNOP)	$env(CMCADE)/dbase/surface/synop
   set DataSrc(OBS,DERISFC)	$env(CMCADE)/banco/derisfc/g3

}