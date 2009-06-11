#!/bin/sh
# the next line restarts using tclsh \
exec $SPI_PATH/tclsh "$0" "$@"
#===============================================================================================
# Environnement Canada
# Centre Meteorologique Canadien
# Dorval, Quebec
#
# Projet     : Interface pour le modele CANERM.
# Nom        : <SVScriptCreate.tcl>
# Creation   : Fevrier 2001 - J.P. Gauthier - CMC/CMOE
#
# But        : Generer le script qui sera executer sur le Serveur et creer les
#              fichiers necessaires a CANERM.
#
# Parametres :
#
# Remarques  :
#===============================================================================================

#----- Recuperer les arguments.

set Sim         [lindex $argv  0]
set Lib         [lindex $argv  1]
set Arch        [lindex $argv  2]
set JobFile     [lindex $argv  3]

set SimDate     [lindex $argv  4]
set AccDate     [lindex $argv  5]
set DT          [lindex $argv  6]
set Model       [lindex $argv  7]
set Grid        [lindex $argv  8]
set NoPrev      [lindex $argv  9]

set FreqIn      [lindex $argv 10]
set FreqOut     [lindex $argv 11]
set Restart     [lindex $argv 12]
set PrevRestart [lindex $argv 13]

set IsoLst      [lrange $argv 14 end]

#----- Inscrire l'entete.

set f [open ${Sim}/tmp/${JobFile} w]

puts $f "#!/bin/sh

set -x
umask 022

#--------------------------------------------------------------------------------
#     Initialisation des constantes utilisees dans le script.
#--------------------------------------------------------------------------------

DIR_BIN=$Lib/Bin/$Arch       ;# Binary directory (Depend on architecture).
DIR_SCRIPT=$Lib/Script       ;# Script directory (Depend on architecture).
DIR_DATA=$Lib/Data           ;# Data directory
DIR_TMP=$Sim                 ;# Temporary subdirectory

#--------------------------------------------------------------------------------
#     Initialisation des variables utilisees dans le script.
#--------------------------------------------------------------------------------

AccDate=${AccDate}         ;# Date d'accident
SimDate=${SimDate}         ;# Date de la simulation.
DT=${DT}                   ;# Nb d'heure entre deux periodes.
Model=${Model}             ;# Type de modele (glb ou reg)
Grid=${Grid}               ;# Changement de grille
PathSim=${Sim}             ;# Repertoire de la simulation.
Result=${SimDate}_         ;# Nom des fichiers resultats.
Restart=${Restart}_000r    ;# Nom du fichier restart sur le backend.
PrevRestart=${PrevRestart} ;# Nom du fichier restart sur le backend.
FreqIn=${FreqIn}           ;# Frequence des resultats d'entree au modele
FreqOut=${FreqOut}         ;# Frequence des resultats de sortie du modele

cd \${DIR_TMP}/meteo
"

#--------------------------------------------------------------------------------
#     Recuperation des donnees necessaires
#--------------------------------------------------------------------------------

#----- Test si on debute ou continue une experience.

if { ${NoPrev} >= 0 } {
   puts $f "
#----- Cette execution n'est pas la premiere (continue l'experience)
#      (alors on utilise le restart precedent).

if \[ -r \${PrevRestart} \]
then
   ln -s \${PrevRestart} oldrst
else
   echo \"Le restart n'est pas disponible\"
   exit 1
fi

if \[ \${Grid} -eq 1 \]
then
   read < ../tmp/griddef grid

   cat <<EOF_PGSM_RESTART> ../tmp/pgsm_restart.dir
 SORTIE(STD,500)
 GRILLE(PS,\${grid})
 EXTRAP(0.0)
 HEURE(-1)
 COMPAC=-32
 CONV(XX,0.0,1.0,\[0.0,1.0E37\])
 CONV(DI,0.0,1.0,\[0.0,1.0E37\])
 CONV(WI,0.0,1.0,\[0.0,1.0E37\])
 CHAMP(XX,TOUT)
 CHAMP(DI,TOUT)
 CHAMP(WI,TOUT)
EOF_PGSM_RESTART

   mv oldrst oldrst.ori

   pgsm2000 -iment oldrst.ori \
     -ozsrt oldrst \
     -i ../tmp/pgsm_restart.dir \
     -l ../tmp/pgsm_restart.err

   rm oldrst.ori
fi
"
} else {
   puts $f "
#----- Cette execution est la premiere (debute l'experience).
"
}

puts $f "

#----- Verifier si la meteo est generee

read < ../tmp/sim.meteo status

while \[ \${status} != done \]
do
   if \[ \${status} = fail \]
   then
      echo
      echo \"Erreur dans la production des champs meteo\"
      exit 1
   fi
   sleep 10
   read < ../tmp/sim.meteo status
done

echo \"Production des champs meteo terminee\"

#--------------------------------------------------------------------------------
#     Lancement du modele
#--------------------------------------------------------------------------------

\${DIR_BIN}/canerm -initial oldrst -climfld tape20 \\
    -restart \${Restart} -i ../tmp/ersinp -in_freq \${FreqIn} -out_freq \${FreqOut} \\
    -l ../tmp/canerm.out -date NOW
"

if { $NoPrev >= 0 } {
   puts $f "
#--------------------------------------------------------------------------------
#     Recuperation des fichier precedent pour les post-traitements
#--------------------------------------------------------------------------------

for fich_prec in `cat ../tmp/previous`
do
   fich=`echo \${fich_prec} | sed 's/^.*\\///g' | head -1`

   if \[ -r \${fich_prec} \]
   then
      ln -s \${fich_prec} \${fich}
   else
      echo \"Impossible de recuperer le fichier \${fich}.\"
      echo \"Les resultats des post-traitements seront invalide.\"
   fi
done
"
}

puts $f "
#--------------------------------------------------------------------------------
#     Calcul des post-traitements
#--------------------------------------------------------------------------------

#----- Liste des fichiers de resultats

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

#----- Effectuer le calcul de doses.

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
      -l ../tmp/listing_\${iso}.post -debug > ../tmp/output_\${iso}.post
done

#--------------------------------------------------------------------------------
#     Transfert des resultats
#--------------------------------------------------------------------------------

#----- Transfert des fichiers resultats.

standards=`ls -1 \${Result}*c`

for file in \${standards}
do
   \${DIR_BIN}/CodeInfo -info ../tmp/sim.pool -fstd \${file} -ckey codef -nomvar INFO
   mv \${file} ../results
done

#----- Transfert des fichiers de postraitement (concentration, depot et dose).

if \[ -r \${Result}000p \]
then
   \${DIR_BIN}/CodeInfo -info ../tmp/sim.pool -fstd \${Result}000p -ckey codef -nomvar INFO
   mv \${Result}000p ../results
fi

#----- Transfert du fichier restart.

\${DIR_BIN}/CodeInfo -info ../tmp/sim.pool -fstd \${Restart} -ckey codef -nomvar INFO
   mv \${Restart} ../results

#----- Changer l'etat de la simulation sur le serveur.

\${DIR_SCRIPT}/SimDone.sh ../tmp/sim.pool ../../CANERM.pool &
\${DIR_SCRIPT}/SimTraceCANERM.sh \${PathSim} \${PathSim}/../../trace $JobFile &

#----- Cleanup

rm oldrst
"

close $f

#----- S'assure que le script genere est bien executable.

exec chmod 755 ${Sim}/tmp/${JobFile}

