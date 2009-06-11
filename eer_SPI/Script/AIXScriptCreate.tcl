#!/bin/sh
# the next line restarts using tclsh \
exec $SPI_PATH/tclsh "$0" "$@"
#===============================================================================================
# Environnement Canada
# Centre Meteorologique Canadien
# Dorval, Quebec
#
# Projet     : Interface pour le modele CANERM.
# Nom        : <AIXScriptCreate.tcl>
# Creation   : Fevrier 2001 - J.P. Gauthier - CMC/CMOE
#
# But        : Generer le script qui sera executer sur le BackEnd et creer les
#              fichiers necessaires a CANERM.
#
# Parametres :
#
# Script(s) Appelant(s) :
#    ExperimentManager.tcl.
#
# Remarques  :
#    Aucune.
#===============================================================================================

source $env(HOME)/.eer_ToolDefs/.eer_Defs-$env(SPI_VERSION)

#----- Recuperer les arguments.

set Host        [lindex $argv  0]
set Front       [lindex $argv  1]
set JobFile     [lindex $argv  2]
set Model       [lindex $argv  3]
set NoPrev      [lindex $argv  4]
set SimDate     [lindex $argv  5]
set AccDate     [lindex $argv  6]
set PathSim     [lindex $argv  7]
set PathScript  [lindex $argv  8]
set DT          [lindex $argv  9]
set Restart     [lindex $argv 10]
set PrevRestart [lindex $argv 11]
set Grid        [lindex $argv 12]
set FreqIn      [lindex $argv 13]
set FreqOut     [lindex $argv 14]
set IsoLst      [lrange $argv 15 end]

set DirHome [lindex $GDefs(BackEnd$Host) 0]
set DirTmp  [lindex $GDefs(BackEnd$Host) 1]

#----- Ecrire le corps du script.

set f [open ${PathSim}/tmp/${JobFile} w]

puts $f "set -x

. /usr/local/env/env_univ/profile_ksh_usr.005

umask 022
tmppath=r$JobFile
F_PROGINF=DETAIL
export F_PROGINF

echo \"Executing on \${HOST}(\${ARCH})\"

##--------------------------------------------------------------------------------
##     Initialisation des constantes utilisees dans le script.
##--------------------------------------------------------------------------------

FRONTEND=$Front                               ;## Server name (where to get/send files).

ARCH=\`uname -s\`
DIR_BIN=$DirHome/bin/\${ARCH}       ;## BackEnd Binary directory (Depend on architecture).
DIR_SCRIPT=$DirHome/canerm/script/\${ARCH} ;## BackEnd Script directory (Depend on architecture).
DIR_DATA=$DirHome/canerm/database          ;## Nom du fichier de la physique.
DIR_TMP=$DirTmp                            ;## BackEnd Temporary subdirectory
DIR_BACK=$DirTmp/eer_Backup                ;## Repertoire de sauvegarde.

##--------------------------------------------------------------------------------
##     Initialisation des variables utilisees dans le script.
##--------------------------------------------------------------------------------


AccDate=${AccDate}         ;## Date d'accident
SimDate=${SimDate}         ;## Date de la simulation.
DT=${DT}                   ;## Nb d'heure entre deux periodes.
Model=${Model}             ;## Type de modele (glb ou reg)
Grid=${Grid}               ;## Changement de grille
PathSim=${PathSim}         ;## Repertoire de la simulation.
PathScript=${PathScript}   ;## Repertoire des scripts.
PathBack=[exec echo ${PathSim} | tr "/" "\012" | tail -2 | tr "\012" "/"]
Result=${SimDate}_         ;## Nom des fichiers resultats.
Restart=${Restart}_000r    ;## Nom du fichier restart sur BackEnd.
PrevRestart=${PrevRestart} ;## Nom du fichier restart sur BackEnd.
FreqIn=${FreqIn}           ;## Frequence des resultats d'entree au modele
FreqOut=${FreqOut}         ;## Frequence des resultats de sortie du modele

##----- CD DIR_TMP ,create tmppath directory and get files.

cd    \${DIR_TMP}
mkdir \${tmppath}
cd    \${tmppath}
pwd

trap \"cd \${DIR_TMP} ; rm -r \${tmppath} ; exit 0\" 1 2 3 15 30

##--------------------------------------------------------------------------------
##     Recuperation des donnees necessaires
##--------------------------------------------------------------------------------

##----- Obtenir le ersinp du serveur.

rcp -p \${FRONTEND}:\${PathSim}/tmp/ersinp ersinp

##----- Obtenir la ligne de pool du serveur

rcp -p \${FRONTEND}:\${PathSim}/tmp/sim.pool sim.pool

##----- Obtenir la GRILLE du serveur pour les PGSM.

rcp -p \${FRONTEND}:\${PathSim}/tmp/griddef griddef

##----- Obtenir les noms des fichiers meteo necessaire.

rcp -p \${FRONTEND}:\${PathSim}/tmp/data_std_sim.eta data_std_sim.eta
"

#----- Test si on debute ou continue une experience.

if { ${NoPrev} >= 0 } {
   puts $f "
##----- Cette execution n'est pas la premiere (continue l'experience)
##      (alors on utilise le restart precedent).

echo \"Recuperation du restart sur \${HOST}\"

back=`echo \${PrevRestart} | tr \"/\" \"\\012\" | tail -4 | tr \"\\012\" \" \" | cut -d\" \" -f1,2,4 | tr \" \" \"/\"`

if \[ -r \${DIR_BACK}/\${back} \]
then
   ln -s \${DIR_BACK}/\${back} oldrst
else
   echo \"Le restart n'est pas sur \${HOST}\"
   echo \"Recuperation du restart sur \${FRONTEND}\"
   nrcp \${FRONTEND}:\${PrevRestart} oldrst

   if \[ \$? != 0 \]
   then
      echo \"Impossible de recuperer le restart\"
      exit 1
   fi
fi

if \[ \${Grid} -eq 1 \]
then
   \${DIR_SCRIPT}/InterpolateRestart.sh . oldrst
fi
"
} else {
   puts $f "
##----- Cette execution est la premiere (debute l'experience).
"
}

puts $f "
##--------------------------------------------------------------------------------
##     Recuperation des donnees meteo
##--------------------------------------------------------------------------------

##----- Verifier la disponibilite des fichiers meteo

data=0
for fich in `cat data_std_sim.eta`
do
   ls -l \${fich}
   if \[ \$? -ne 0 \]
   then
      data=`expr \$data + 1`
   fi
done

##----- Si disponible interpoler sur le BackEnd sinon recuperer du serveur

if \[ \${data} -eq 0 \]
then
   \${DIR_SCRIPT}/InterpolateFieldsMulti.sh . \${DIR_DATA}/climat_tape20 \${Model} \${FreqOut} 3
else

   rcp -p \${FRONTEND}:\${PathSim}/tmp/sim.meteo sim.meteo
   read < sim.meteo status

   while \[ \${status} != done \]
   do
      if \[ \${status} = fail \]
      then
         echo
         echo \"On n'a pas pu obtenir la climato et la meteo du serveur.\"
         exit 1
      fi
      sleep 10

      rcp -p \${FRONTEND}:\${PathSim}/tmp/sim.meteo sim.meteo
      read < sim.meteo status
   done

   ##----- Recuperer la meteo

   tape30=`rsh \${FRONTEND} -n ls -1 \${PathSim}/meteo/\${Result}*`
   for tape in \${tape30}
   do
      nrcp \${FRONTEND}:\${tape} `echo \${tape} | sed 's/^.*\\///g' | head -1`
      if \[ \$? != 0 \]
      then
         echo \"Impossible de recuperer le fichier de meteo \${tape}.\"
         exit 1
      fi
   done

   ##----- Recuperer la climato

   nrcp \${FRONTEND}:\${PathSim}/meteo/tape20 tape20
   if \[ \$? != 0 \]
   then
      echo \"Impossible de recuperer le fichier de climato \${PathSim}/meteo/tape20.\"
      exit 1
   fi

   echo
   echo \"On a recupere la climato et la meteo du serveur avec succes!\"
fi

sx_trap() {
   cd \${DIR_TMP}
   rm -r \${tmppath}
   rm \${tmppath}/../${JobFile}
   exit 0
}

trap 'sx_trap' 1 2 3 15 30

##--------------------------------------------------------------------------------
##     Lancement du modele
##--------------------------------------------------------------------------------

ls -l

\${DIR_BIN}/canerm -initial oldrst -climfld tape20 \\
    -restart newrst -i ersinp -in_freq \${FreqIn} -out_freq \${FreqOut} \\
    -l canerm.out -date OPRUN
"

if { $NoPrev >= 0 } {
   puts $f "
##--------------------------------------------------------------------------------
##     Recuperation des fichier precedent pour les post-traitements
##--------------------------------------------------------------------------------

for fich_prec in [exec cat ${PathSim}/tmp/previous]
do
   fich=`echo \${fich_prec} | sed 's/^.*\\///g' | head -1`
   back=`echo \${fich_prec} | tr \"/\" \"\\012\" | tail -4 | tr \"\\012\" \" \" | cut -d\" \" -f1,2,4 | tr \" \" \"/\"`

   if \[ -r \${DIR_BACK}/\${back} \]
   then
      ln -s \${DIR_BACK}/\${back} \${fich}
   else
      echo \"Le fichier \${fich} n'est pas \${HOST}\"
      echo \"Recuperation du fichier \${fich} sur \${FRONTEND}\"
      nrcp \${FRONTEND}:\${fich_prec} \${fich}
      if \[ \$? != 0 \]
      then
         echo \"Impossible de recuperer le fichier \${fich}.\"
         echo \"Les resultats des post-traitements seront invalide.\"
      fi
   fi
done
"
}

puts $f "
##--------------------------------------------------------------------------------
##     Calcul des post-traitements
##--------------------------------------------------------------------------------

##----- Liste des fichiers de resultats

filelst=`ls *_*c`
no=1
files=\"\"

for file in \${filelst}
do
   if \[ \${no} -lt 10 \]
   then
      nb=\"0\${no}\"
   else
      nb=\${no}
   fi

   files=\"\${files} -file\${nb} \${file}\"
   no=`expr \${no} + 1`
done

##----- Effectuer le calcul de doses.

for iso in ${IsoLst}
do
   if \[ \${iso} != TRACER1 -a \${iso} != TRACER2 -a \${iso} != TRACER3 -a \${iso} != VOLCAN \]
   then
      select=\"-dose 1 -conc 1 -depo 1\"
   else
      select=\"-dose 0 -conc 1 -depo 1\"
   fi

   \${DIR_BIN}/calc_dose_it_mf \\
      -fconst \${DIR_DATA}/Specie.src \\
      -iso \${iso} \\
      -etiketi \${iso} \\
      -datesim \${SimDate} \\
      -dateacc \${AccDate} \\
      \${select} \\
      \${files} \\
      -l listing_\${iso}.post -debug > output_\${iso}.post
done

##--------------------------------------------------------------------------------
##     Transfert des resultats vers le serveur
##--------------------------------------------------------------------------------

##----- Transfert des fichiers resultats.

standards=`ls -1 \${Result}*c`

for file in \${standards}
do
   \${DIR_BIN}/CodeInfo -info sim.pool -fstd \${file} -ckey codef -nomvar INFO
   nrcp \${file} \${FRONTEND}:\${PathSim}/results/\${file}
done

if \[ \$? = 0 \]
then
   echo \"Les fichiers resultats ont ete transfere sur le serveur dans\"
   echo \"   \${PathSim}/results !\"
else
   echo \"Les fichiers resultats n'ont pas ete transfere sur le serveur.\"
fi

##----- Transfert des fichiers de postraitement (concentration, depot et dose).

if \[ -r \${Result}000p \]
then
   \${DIR_BIN}/CodeInfo -info sim.pool -fstd \${Result}000p -ckey codef -nomvar INFO
   nrcp \${Result}000p \${FRONTEND}:\${PathSim}/results/\${Result}000p

   if \[ \$? = 0 \]
   then
      echo \"Les donnees de postraitement \${Result}000p ont ete transfere !\"
   else
      echo \"Les donnees de postraitement \${Result}000p n'ont pas ete transfere !\"
   fi
fi

##----- Transfert du fichier restart.

\${DIR_BIN}/CodeInfo -info sim.pool -fstd newrst -ckey codef -nomvar INFO
nrcp newrst \${FRONTEND}:\${PathSim}/results/\${Restart}

if \[ \$? = 0 \]
then
   echo \"Le restart \${Restart} a ete transfere !\"
else
   echo \"Le restart \${Restart} n'a pas ete transfere !\"
fi

##--------------------------------------------------------------------------------
##     Transfert des outputs vers le serveur
##--------------------------------------------------------------------------------

##----- Transferer les outputs.

rcp -p  canerm.out \${FRONTEND}:\${PathSim}/tmp
rcp -p  pgsm_*     \${FRONTEND}:\${PathSim}/tmp
rcp -p  *.post     \${FRONTEND}:\${PathSim}/tmp

##----- Changer l'etat de la simulation sur le serveur.

rsh \${FRONTEND} -n \${PathScript}/SimDone.sh ${PathSim}/../CANERM.pool ${PathSim}/tmp/sim.pool 0 &
rsh \${FRONTEND} -n \${PathScript}/SimTraceCANERM.sh \${PathSim} \${PathSim}/../../trace $JobFile &

##--------------------------------------------------------------------------------
##     Copie de sauvegarde des resultats
##--------------------------------------------------------------------------------

if \[ -r \${DIR_BACK} \]
then
   ##----- Creation des repertoire

   mkdir -p \${DIR_BACK}/\${PathBack}

   ##----- Copie des resultats.

   cp newrst \${DIR_BACK}/\${PathBack}\${Restart}
   cp \${Result}000p \${DIR_BACK}/\${PathBack}

   for file in \${standards}
   do
      cp \${file} \${DIR_BACK}/\${PathBack}
   done
fi

##----- Attention : tous les fichiers seront detruits ici.

ls -l
cd ..
rm -f -r \${tmppath}

ps -lp \$\$
JID=`ps -lp \$\$ | grep -v JID | cut -c24-28`
echo \"job id is \${JID}\"
acctcom -Mij -J \${JID} < /dev/null
exit 0
"

close $f

#----- S'assure que le script genere est bien executable.

exec chmod 755 ${PathSim}/tmp/${JobFile}
