#!/bin/sh
#===============================================================================
# Environnement Canada
# Centre Meteorologique Canadien
# Dorval, Quebec
#
# Projet     : RSMC.
# Nom        : <RSMCTransferBlank.sh>
# Creation   : Mai 2011 - S. Trudel - CMC/CMOE
#
# But        : Permet de transferer des fichiers <Unavailable> sur le site RSMC commun.
#
# Parametres :
#   ${1}     : path ou se retrouvent les fichiers <Unavailable>.
#   ${2}     : path ou se retrouvent les fichiers <Unavailable> mais avec la nomenclature RSMC ( contenant l'id ).
#   ${*}     : token(s) pour les actions a prendre, token valide :
#                 pays            : CA/US/CN/JP/RU/FR/UK/AU
#                 joint statement : jntreg34.html/jntreg5.html/jntreg16.html/jntreg2.html
#                 leadrsmc        : leadrsmc.txt
#                 meteo           : meteo
#
# Remarques  :
#   - Aucune.
#===============================================================================

#------ repertoire pour les fichiers <Unavailable>.
#       $SPI_PATH/Resources/Image/System

DirData=${1}
PathRSMC=${2}

cd ${DirData}

shift
shift

if [ $1 == "CA" -o $1 == "US" -o $1 == "CN" -o $1 == "JP" -o $1 == "RU" -o $1 == "FR" -o $1 == "UK" -o $1 == "AU" ]
then
   echo "On efface les produits relatif au RSMC ( $1 ).\n"
elif [ $1 == "jntreg34.html" -o $1 == "jntreg5.html" -o $1 == "jntreg16.html" -o $1 == "jntreg2.html" ]
then
   echo "On remplace le "joint statement" par <Unavailable>.\n"
elif [ $1 == "leadrsmc.txt" ]
then
   echo "On remplace l'indicateur "leadrsmc.txt" par 0.\n"
elif [ $1 == "meteo" ]
then
   echo "On efface les produits meteo relatif au RSMC Montreal.\n"
else
   echo "Ce RSMC n'existe pas ( $1 )."
   exit
fi

#------ effectue un backup des produits avant tout suppression.

datec=`date +"%Y%m%d.%H%M%SZ"`
ssh eer@accessdepot "cd /data/depot/eer ; tar cvf ~/backup/rsmc-${datec}-${1}.tar rsmc_*/incoming/* > ~/backup/rsmc-${datec}-${1}.out 2> ~/backup/rsmc-${datec}-${1}.err"

if [ $? -eq 0 ]
then
   echo "Le backup sur le serveur accessdepot a pu se faire."
else
   echo "Probleme : le backup sur le serveur accessdepot n'a pu se faire !"
fi

scp -p eer@accessdepot:backup/rsmc-${datec}-${1}.tar $env(HOME)/backup/accessdepot/rsmc-${datec}-${1}.tar

if [ $? -eq 0 ]
then
   echo "La copie interne a partir du serveur accessdepot a pu se faire."
else
   echo "Probleme : la copie interne a partir du serveur accessdepot n'a pu se faire."
fi

#----- on boucle sur la liste fournie.

for i in ${*}
do
   #------ on prepare a effacer les cartes du RSMC Montreal seulement.

   if [ $1 == "CA" ]
   then
      echo "Effacement des produits pour le RSMC Montreal ( $1 )...\n"

      #----- on s'assure qu'il n'y ait plus d'images associees aux scenarios de 96 hres.

      ssh eer@accessdepot rm -f incoming/LIC${1}_04.gif incoming/LTD${1}_04.gif incoming/SIC${1}_04.gif incoming/STD${1}_04.gif

      #----- on remplace les produits par les <Unavailable>.

      cp -p na_DATE.TXT        ${PathRSMC}/${1}_DATE.TXT
      cp -p na_RSMCProduct.gif ${PathRSMC}/CVR${1}.gif
      cp -p na_rsmc.ps         ${PathRSMC}/rsmc.ps

      for n in 01 02 03
      do
         cp -p na_RSMCProduct.gif ${PathRSMC}/LIC${1}_${n}.gif
         cp -p na_RSMCProduct.gif ${PathRSMC}/SIC${1}_${n}.gif
         cp -p na_RSMCProduct.gif ${PathRSMC}/LTD${1}_${n}.gif
         cp -p na_RSMCProduct.gif ${PathRSMC}/STD${1}_${n}.gif
      done

      cp -p na_RSMCProduct.gif ${PathRSMC}/LTJ${1}.gif
      cp -p na_RSMCProduct.gif ${PathRSMC}/STJ${1}.gif
      cp -p na_traj.txt        ${PathRSMC}/traj.txt
   fi

   #------ on efface les cartes du RSMC specifie.

   if [ $1 == "US" -o $1 == "CN" -o $1 == "JP" -o $1 == "RU" -o $1 == "FR" -o $1 == "UK" -o $1 == "AU" ]
   then
      echo "Effacement des produits pour le RSMC ( $1 )...\n"

      if [ $1 == "US" ]
      then
         username=rsmc_washington
      elif [ $1 == "CN" ]
      then
         username=rsmc_beijing
      elif [ $1 == "JP" ]
      then
         username=rsmc_tokyo
      elif [ $1 == "RU" ]
      then
         username=rsmc_obninsk
      elif [ $1 == "FR" ]
      then
         username=rsmc_toulouse
      elif [ $1 == "UK" ]
      then
         username=rsmc_exeter
      elif [ $1 == "AU" ]
      then
         username=rsmc_melbourne
      fi

      servername=depot

      #----- on s'assure qu'il n'y ait plus d'images associees aux scenarios de 96 hres.

      ssh ${username}@${servername} rm -f incoming/LIC${1}_04.gif incoming/LTD${1}_04.gif incoming/SIC${1}_04.gif incoming/STD${1}_04.gif

      #----- on remplace les produits par les <Unavailable>.

      scp -p na_DATE.TXT        ${username}@${servername}:incoming/${1}_DATE.TXT
      scp -p na_RSMCProduct.gif ${username}@${servername}:incoming/CVR${1}.gif
      scp -p na_rsmc.ps         ${username}@${servername}:incoming/rsmc.ps

      for n in 01 02 03
      do
         scp -p na_RSMCProduct.gif ${username}@${servername}:incoming/LIC${1}_${n}.gif
         scp -p na_RSMCProduct.gif ${username}@${servername}:incoming/SIC${1}_${n}.gif
         scp -p na_RSMCProduct.gif ${username}@${servername}:incoming/LTD${1}_${n}.gif
         scp -p na_RSMCProduct.gif ${username}@${servername}:incoming/STD${1}_${n}.gif
      done

      scp -p na_RSMCProduct.gif ${username}@${servername}:incoming/LTJ${1}.gif
      scp -p na_RSMCProduct.gif ${username}@${servername}:incoming/STJ${1}.gif
      scp -p na_traj.txt        ${username}@${servername}:incoming/traj.txt
   fi

   #----- on se prepare a remplacer le "joint statement" par <Unavailable> sur le site RSMC Montreal.

   if [ $1 == "jntreg34.html" ]
   then
      echo "- prepare a remplacer le joint statement : www/mandats/rsmc/usagers/jnt_rsmc/restrict/JNT_STMT/${1} sur le site RSMC Montreal."

      cp -p na_${1} ${PathRSMC}/${1}
   fi

   #----- on remplace le "joint statement" par <Unavailable> sur les sites RSMC.

   if [ $1 == "jntreg5.html" -o $1 == "jntreg16.html" -o $1 == "jntreg2.html" ]
   then
      echo "- remplace le joint statement : www/mandats/rsmc/usagers/jnt_rsmc/restrict/JNT_STMT/${1} sur le site RSMC Montreal uniquement."

      scp -p na_${1} rsmc_jnt@depot.cmc.ec.gc.ca:incoming/${1}
   fi

   #----- on remplace l'indicateur "leadrsmc.txt" par 0.

   if [ $1 == "leadrsmc.txt" ]
   then
      echo "- remplace l'indicateur leadrsmc.txt par 0 ( www/mandats/rsmc/usagers/jnt_rsmc/restrict/JNT_STMT/${1} )"

      scp -p na_${1} rsmc_jnt@depot.cmc.ec.gc.ca:incoming/${1}
   fi

   #------ on efface les cartes meteo du RSMC Montreal seulement.

   if [ $1 == "meteo" ]
   then
      echo "- rm -f www/mandats/rsmc/usagers/jnt_rsmc/restrict/CA/meteo/*"

      ssh eer@accessdepot rm -f www/mandats/rsmc/usagers/jnt_rsmc/restrict/CA/meteo/*
   fi

   shift

done

exit 0
