#!/bin/ksh
#===============================================================================
# Environnement Canada
# Centre Meteorologique Canadien
# Dorval, Quebec
#
# Projet     : Interface pour le modele CANERM.
# Nom        : <Model_MeteoCANERM.sh>
# Creation   : Septembre 1997 - S. Trudel - CMC/CMOE
#
# Description: Generer le fichier standard pour la climatologie et le fichier
#              standard pour les previsions/analyses.  Ces fichiers seront
#              crees a l'aide de PGSM pour une grille de dimension variable
#              selon les parametres passes a l'entree.
#
# Parametres :
#   ${1}     : Le repertoire temporaire de travail.
#   ${3}     : Le type de modele ( glb ou reg ).
#   ${4}     : La frequence des donnees de sorties.
#   ${5}     : Nombre de processus.
#
# Retour     : un premier fichier standard (tape20) pour les champs suivant :
#
#                  MN (montagne (NCAR)                 - valeur: metre),
#                  MG (masque ocean/continent          - valeur: 0 ou 1),
#               et ZP (longueur de rugosite (CRESSMAN) - valeur: log(metre)).
#
#              un deuxieme fichier standard (tape30) pour les champs suivant :
#
#                  ES (ecart du point de rosee         - valeur: celcius),
#                  GZ (geopotentiel                    - valeur: Dam),
#                  P0 (pression a la surface           - valeur: millibar),
#                  PT (pression au toit du modele      - valeur: millibar),
#                  TT (temperature                     - valeur: celcius),
#                  UV (vitesse du vent                 - valeur: noeuds),
#               et WE (mouvement vertical ETA          - valeur: s-1).
#
# Script(s) Appelant(s) :
#   canerm_procedure,...
#
# Remarques  :
#   - ATTENTION : pour le moment les parametres ne sont pas tenu en compte
#                 afin qu'ils le soient, il faut changer l'appel a <calcul_grille>
#                 pour determiner les arguments de la directive GRILLE.
#
#   - generalement, on s'attend a retrouver les donnees relatif a la
#     climatologie dans le fichier <tape20>; alors que les donnees relatif
#     aux previsions/analyses se retrouveront dans des fichiers YYYYMMDDHH_hhh.
#===============================================================================

#----- Source user's profile.
. ~/.profile >/dev/null 2>&1
. ~/.profile_eer >/dev/null 2>&1

#----- Load standard functions
. ${EER_DIRSCRIPT}/Logger.sh

Log_Start Model_MeteoCANERM.sh 2.0

#----- Recupere les arguments.
DirTmp="${1}"
Model=${2}
FreqOut=${3}
Process=${4}

cd ${DirTmp}

#----- Lecture des parametres pour la directive GRILLE
read < griddef.in grid

rm -f ../meteo/*

# ----- Mise-a-jour des directives de PGSM_CLIMATO (climatologie).
#       les champs sont :
#                 MN (montagne (NCAR)                 - valeur: metre),
#                 MG (masque ocean/continent          - valeur: 0 ou 1),
#              et ZP (longueur de rugosite (CRESSMAN) - valeur: log(metre)).

cat <<EOF_PGSM_CLIMATO > ${DirTmp}/pgsm_climato.dir
 SORTIE(STD,500)
 GRILLE(PS,${grid})
 HEURE(0)
 CONV(ZP,-3.0,0.85)
 IP3SRT=1
 CHAMP('MN',TOUT)
 CHAMP('MG',TOUT)
 CHAMP('ZP',TOUT)
EOF_PGSM_CLIMATO

pgsm2000 -iment ${EER_DIRDATA}/climato.fstd \
     -ozsrt ../meteo/tape20 \
     -i pgsm_climato.dir \
     -l pgsm_climato.out 2> pgsm_climato.err &

# ----- Mise-a-jour des directives de PGSM_ANAL_PREV (analyse/prevision).
#       les champs sont :
#                 ES (ecart du point de rosee    - valeur: celcius),
#                 GZ (geopotentiel               - valeur: Dam),
#                 P0 (pression a la surface      - valeur: millibar),
#                 PT (pression au toit du modele - valeur: millibar),
#                 TT (temperature                - valeur: celcius),
#                 UV (vitesse du vent            - valeur: noeuds),
#              et WE (mouvement vertical ETA     - valeur: s-1).

cat <<EOF_PGSM_METEO > pgsm_meteo.dir
 SORTIE(STD,500,A)
 GRILLE(PS,${grid})
 IP3ENT=0
C
C on multiplit la pression de surface par 100.0 pour compenser la
C sous-routine SGLTP dans le modele.
C
 CONV(P0,0.0,100.0)
 CONV(PT,0.0,100.0)
 IP3SRT=1
 HEURE(-1)
EOF_PGSM_METEO

if [ "${Model}" = "glb" ] #----- Meteorological fields from GEM Meso-Global 33 km.
then
   cat <<EOF_PGSM_METEOg33 >> pgsm_meteo.dir
C
C champs provenants du modele regional ou meso-global
C
 CHAMP(PT,TOUT)
 CHAMP(P0,TOUT)
 CHAMP(ES,2374,2720,2897,3044,3248,3541,3976,4522,5144,5843,
  6348,6612,7446,8034,8646,9272,9845,10346,10780,11151,
  11467,11733,11850,11950,12000)
 CHAMP(GZ,2374,2720,2897,3044,3248,3541,3976,4522,5144,5843,
  6348,6612,7446,8034,8646,9272,9845,10346,10780,11151,
  11467,11733,11850,11950,12000)
 CHAMP(TT,2374,2720,2897,3044,3248,3541,3976,4522,5144,5843,
  6348,6612,7446,8034,8646,9272,9845,10346,10780,11151,
  11467,11733,11850,11950,12000)
 CHAMP(UV,2374,2720,2897,3044,3248,3541,3976,4522,5144,5843,
  6348,6612,7446,8034,8646,9272,9845,10346,10780,11151,
  11467,11733,11850,11950,12000)
 CHAMP(WE,2374,2720,2897,3044,3248,3541,3976,4522,5144,5843,
  6348,6612,7446,8034,8646,9272,9845,10346,10780,11151,
  11467,11733,11850,11950,12000)
EOF_PGSM_METEOg33
elif [ "${Model}" = "glb100" ] #----- Meteorological fields from GEM Global 100 km.
then
   cat <<EOF_PGSM_METEOg100 >> pgsm_meteo.dir
C
C champs provenant du modele global.
C
 CHAMP(PT,TOUT)
 CHAMP(P0,TOUT)
 CHAMP(ES,2510,2750,3010,3270,3550,3850,4190,4580,5020,5510,
  6050,6600,7160,7740,8310,8880,9440,9960,10420,10840,
  11220,11550,11800,11930,12000)
 CHAMP(GZ,2510,2750,3010,3270,3550,3850,4190,4580,5020,5510,
  6050,6600,7160,7740,8310,8880,9440,9960,10420,10840,
  11220,11550,11800,11930,12000)
 CHAMP(TT,2510,2750,3010,3270,3550,3850,4190,4580,5020,5510,
  6050,6600,7160,7740,8310,8880,9440,9960,10420,10840,
  11220,11550,11800,11930,12000)
 CHAMP(UV,2510,2750,3010,3270,3550,3850,4190,4580,5020,5510,
  6050,6600,7160,7740,8310,8880,9440,9960,10420,10840,
  11220,11550,11800,11930,12000)
 CHAMP(WE,2510,2750,3010,3270,3550,3850,4190,4580,5020,5510,
  6050,6600,7160,7740,8310,8880,9440,9960,10420,10840,
  11220,11550,11800,11930,12000)
EOF_PGSM_METEOg100
elif [ "${Model}" = "reg" ] #----- Meteorological fields from GEM Regional 15 km.
then
   cat <<EOF_PGSM_METEOr15 >> pgsm_meteo.dir
C
C champs provenant du modele regional.
C
 CHAMP(PT,TOUT)
 CHAMP(P0,TOUT)
 CHAMP(ES,2374,2720,2897,3044,3248,3541,3976,4522,5144,5843,
  6348,6612,7446,8034,8646,9272,9845,10346,10780,11151,
  11467,11733,11850,11950,12000)
 CHAMP(GZ,2374,2720,2897,3044,3248,3541,3976,4522,5144,5843,
  6348,6612,7446,8034,8646,9272,9845,10346,10780,11151,
  11467,11733,11850,11950,12000)
 CHAMP(TT,2374,2720,2897,3044,3248,3541,3976,4522,5144,5843,
  6348,6612,7446,8034,8646,9272,9845,10346,10780,11151,
  11467,11733,11850,11950,12000)
 CHAMP(UV,2374,2720,2897,3044,3248,3541,3976,4522,5144,5843,
  6348,6612,7446,8034,8646,9272,9845,10346,10780,11151,
  11467,11733,11850,11950,12000)
 CHAMP(WE,2374,2720,2897,3044,3248,3541,3976,4522,5144,5843,
  6348,6612,7446,8034,8646,9272,9845,10346,10780,11151,
  11467,11733,11850,11950,12000)
EOF_PGSM_METEOr15
elif [ "${Model}" = "reg24" ] #----- Meteorological fields from GEM Global 100 km.
then
   cat <<EOF_PGSM_METEOr24 >> pgsm_meteo.dir
C
C champs provenant du modele regional.
C
 CHAMP(PT,TOUT)
 CHAMP(P0,TOUT)
 CHAMP(ES,2374,2720,2897,3044,3248,3541,3976,4522,5144,5843,
  6348,6612,7446,8034,8646,9272,9845,10346,10780,11151,
  11467,11733,11850,11950,12000)
 CHAMP(GZ,2374,2720,2897,3044,3248,3541,3976,4522,5144,5843,
  6348,6612,7446,8034,8646,9272,9845,10346,10780,11151,
  11467,11733,11850,11950,12000)
 CHAMP(TT,2374,2720,2897,3044,3248,3541,3976,4522,5144,5843,
  6348,6612,7446,8034,8646,9272,9845,10346,10780,11151,
  11467,11733,11850,11950,12000)
 CHAMP(UV,2374,2720,2897,3044,3248,3541,3976,4522,5144,5843,
  6348,6612,7446,8034,8646,9272,9845,10346,10780,11151,
  11467,11733,11850,11950,12000)
 CHAMP(WE,2374,2720,2897,3044,3248,3541,3976,4522,5144,5843,
  6348,6612,7446,8034,8646,9272,9845,10346,10780,11151,
  11467,11733,11850,11950,12000)
EOF_PGSM_METEOr24
fi

#-----On lis la liste de fichier a traiter.
read < data_std_eta.in stdfiles

#-----On Patch pour le GEMDM ( sortie aux heures ).
file1=`echo $stdfiles | cut -d " " -f1`
file1=`basename $file1`
base1=`echo $file1 | cut -d"_" -f1`
ext1=`echo $file1 | cut -d"_" -f2`

file1=`r.date $base1 +${ext1}`

file2=`echo $stdfiles | cut -d " " -f2`
file2=`basename $file2`
base2=`echo $file2 | cut -d"_" -f1`
ext2=`echo $file2 | cut -d"_" -f2`

file2=`r.date $base2 +${ext2}`

FreqIn=`r.date $file2 $file1`

#-----On obtient la date de simulation a partie du premier fichier dans la liste.
first=`echo $stdfiles | cut -d " " -f1 | tr '/' '\n' | tail -1`
date=`echo $first | cut -d "_" -f1`
hour=`echo $first | cut -d "_" -f2`

date=`r.date $date +$hour | cut -c0-10`

#-----On lance le PGSM a 0 heure.
pgsm2000  -iment `echo ${stdfiles} | cut -d" " -f1` \
      -ozsrt ../meteo/${date}_000 \
      -i pgsm_meteo.dir \
      -l pgsm_meteo_000.out 2> pgsm_meteo_000.err &

#-----On determine le nombre de fichier a traiter.
nbfiles=`echo ${stdfiles} | wc -w`
#nbfiles=`expr ${nbfiles} - 1`

#-----On determine le nombre de fichier qui serons passes a chaque PGSM.
nbfilesinput=`expr $FreqOut \/ $FreqIn`

#-----On determine le nombre d'execution de PGSM au totale.
nbfilesoutput=`expr $nbfiles \/ $nbfilesinput`
nbfilesmod=`expr $nbfiles \% $nbfilesinput`
if [ $nbfilesmod -ne 0 ]
then
   nbfilesoutput=`expr $nbfilesoutput + 1`
fi

#-----On boucle sur le nombre totale de PGSM suivi de sur chaque fichier par PGSM.
k=2
loop=1
hour=0
process=1

while [ ${loop} -le ${nbfilesoutput} ]
do

   i=1
   filelist=''

   while [ ${i} -le ${nbfilesinput} ]
   do

      file=`echo ${stdfiles} | cut -d" " -f$k`
      filelist="${filelist} $file"

      hour=`expr $hour + $FreqIn`

      #-----On formatte l'extension correctement.
       if [ $hour -lt 10 ]; then
          hour3="00${hour}"
       elif [ $hour -lt 100 ]; then
          hour3="0${hour}"
       else
          hour3="${hour}"
       fi

      #-----On tient compte du nom du deuxieme fichier car on devras inclure
      #     les resultats du premier PGSM a la fin. Cela n'est vraiment
      #     necessaire que l'orsque le premier fichier contient plus d'une
      #     heure de donnees a l'intereur.
      i=`expr $i + 1`
      k=`expr $k + 1`

   done

   #-----On lance le PGSM.
   pgsm2000 -iment ${filelist} \
        -ozsrt ../meteo/${date}_${hour3} \
        -i pgsm_meteo.dir \
        -l pgsm_meteo_${hour3}.out 2> pgsm_meteo_${hour3}.err &

   #-----On incremente le process actuel.
   process=`expr $process + 1`
   loop=`expr $loop + 1`

   if [ $process -gt $Process ]; then
      wait
      process=1
   fi
done

#-----On attend que toutes les procedures soit terminees.
wait

Log_End 0
