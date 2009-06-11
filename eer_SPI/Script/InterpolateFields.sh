#!/bin/sh
#===============================================================================
# Environnement Canada
# Centre Meteorologique Canadien
# Dorval, Quebec
#
# Projet     : Interface pour le modele CANERM.
# Nom        : <InterpolateFields.sh>
# Creation   : Septembre 1997 - S. Trudel - CMC/CMOE
#
# Description: Generer le fichier standard pour la climatologie et le fichier
#              standard pour les previsions/analyses.  Ces fichiers seront
#              crees a l'aide de PGSM pour une grille de dimension variable
#              selon les parametres passes a l'entree.
#
# Parametres :
#   ${1}     : Le repertoire temporaire de travail.
#   ${2}     : Le fichier de climatologie .
#   ${3}     : Le type de modele ( glb ou reg ).
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
#     aux previsions/analyses se retrouveront dans le fichier <tape30>.
#===============================================================================

# ----- Recupere les arguments.

DirTmp="${1}"
Climato="${2}"
Model=${3}

cd ${DirTmp}

echo "DWJOB Interpolation horizontale."

# ----- Lecture des parametres pour la directive GRILLE

read < griddef grid

# ----- Mise-a-jour des directives de PGSM_CLIMATO (climatologie).
#       les champs sont :
#                 MN (montagne (NCAR)                 - valeur: metre),
#                 MG (masque ocean/continent          - valeur: 0 ou 1),
#              et ZP (longueur de rugosite (CRESSMAN) - valeur: log(metre)).

cat <<EOF_PGSM_CLIMATO > pgsm_climato.dir
 SORTIE(STD,500)
 GRILLE(PS,${grid})
 HEURE(0)
 CONV(ZP,-3.0,0.85)
 IP3SRT=1
 CHAMP('MN',TOUT)
 CHAMP('MG',TOUT)
 CHAMP('ZP',TOUT)
EOF_PGSM_CLIMATO

# ----- Mise-a-jour des directives de PGSM_METEO (analyse/prevision).
#       les champs sont :
#                 ES (ecart du point de rosee    - valeur: celcius),
#                 GZ (geopotentiel               - valeur: Dam),
#                 P0 (pression a la surface      - valeur: millibar),
#                 PT (pression au toit du modele - valeur: millibar),
#                 TT (temperature                - valeur: celcius),
#                 UV (vitesse du vent            - valeur: noeuds),
#              et WE (mouvement vertical ETA     - valeur: s-1).

cat <<EOF_PGSM_METEO1 > pgsm_meteo.dir
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
 CHAMP(PT,TOUT)
 CHAMP(P0,TOUT)
EOF_PGSM_METEO1

if [ "${Model}" = "glb" ]
then
   cat <<EOF_PGSM_METEO2g >> pgsm_meteo.dir
C
C champs provenant du modele global.
C
 HEURE(0,6,12,18,24,30,36,42,48,54,60,66,72,78,84,90,96,102,108,114,
       120,126,132,138,144)
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
EOF_PGSM_METEO2g
else
   cat <<EOF_PGSM_METEO2r >> pgsm_meteo.dir
C
C champs provenant du modele regional.
C
 HEURE(-1)
 CHAMP(ES,2400,2610,2910,3310,3770,4220,4730,5280,5840,6440,
  7000,7550,8110,8660,9220,9730,10180,10590,10940,11250,
  11500,11700,11850,11950,12000)
 CHAMP(GZ,2400,2610,2910,3310,3770,4220,4730,5280,5840,6440,
  7000,7550,8110,8660,9220,9730,10180,10590,10940,11250,
  11500,11700,11850,11950,12000)
 CHAMP(TT,2400,2610,2910,3310,3770,4220,4730,5280,5840,6440,
  7000,7550,8110,8660,9220,9730,10180,10590,10940,11250,
  11500,11700,11850,11950,12000)
 CHAMP(UV,2400,2610,2910,3310,3770,4220,4730,5280,5840,6440,
  7000,7550,8110,8660,9220,9730,10180,10590,10940,11250,
  11500,11700,11850,11950,12000)
 CHAMP(WE,2400,2610,2910,3310,3770,4220,4730,5280,5840,6440,
  7000,7550,8110,8660,9220,9730,10180,10590,10940,11250,
  11500,11700,11850,11950,12000)
EOF_PGSM_METEO2r
fi

# ----- Lancement des commandes PGSM (2 fois).

echo "DWJOB Extraction des champs de climatologie"

pgsm2000 -iment ${Climato} -ozsrt ../meteo/tape20 \
     -i pgsm_climato.dir
     -l pgsm_climato.err

echo "DWJOB Extraction des analyses/previsions"

read < data_std_sim.eta stdfiles

pgsm2000 -iment ${stdfiles} -ozsrt ../meteo/tape30 \
     -i pgsm_meteo.dir
     -l pgsm_meteo.err
