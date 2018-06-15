#===============================================================================
# Environnement Canada
# Centre Meteorologique Canadian
# 2100 Trans-Canadienne
# Dorval, Quebec
#
# Projet   : Package d'interface pour SPI
# Fichier  : APViz.ctes
# Creation : Juin 2003
#
# Description:
#    Coquille vide demontrant les points d'entree pour la creation de nouveaux outils
#    a l'interface SPI
#
#    Descriptions des variables internes du module
#
#===============================================================================

namespace eval APViz {
   global env
   variable Data
   variable DataSrc

   #----- Data Sources
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
   
   set Data(Calcul)		{Difference Abs_Difference Diff_of_Abs Sum Average Deviation_from_average}
   
}