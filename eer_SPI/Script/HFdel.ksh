#!/bin/ksh
#===========================================================================
# Environnement Canada
# Centre Meteorologique Canadien
# Dorval, Quebec
#
# Projet     : Cleanup realtime pour EER.
# Nom        : <HFdel.ksh>
# Creation   : Mars 2006 - S. Trudel - CMC/CMOE
#
# But        : Effacer les fichiers contenu sur le host specifie en argument.
#
# Parametres :
#   <Host>         : nom du hostname.
#   <Files>        : liste des fichiers.
#   <FrontEnd>     : nom du frontend.
#   <TransmitUser> : nom de l'usager utilise pour la transmission.
#
# Remarques  :
#
#   - Ce script sera execute a partir de l'application HFManager sous SPI.
#
#   - Les sous-repertoires contenus sous ${PathWeatherOfficeEER} sont :
#
#        - current/
#        - FVCN_messages/
#        - realtime_auto_canerm/
#        - realtime_auto_sat/4m5/
#        - realtime_auto_sat/IR/
#        - realtime_auto_sat/VIS/
#        - realtime_auto_traj/
#
#===========================================================================

${HOME}/.profile > /dev/null 2>&1

#set -x

#----- Recupere les arguments.

Host=${1}
Files=${2}
FrontEnd=${3}
TransmitUser=${4}

#----- On s'assure de pouvoir se positionner dans
#      le bon repertoire d'EER sur le host
#      ainsi que de la presence du fichier qui
#      contient la liste des fichiers a afficher.

datef=`date -u '+%Y%m%d-%H%MZ'`

ssh ${FrontEnd} -l ${TransmitUser} -x "echo ${0} : ${datef} > /users/dor/afse/eer/.spi/Trace/HFdel.out 2>&1 "

mkdir -p ${HOME}/.spi/Trace/HFdel
cd ${HOME}/.spi/Trace/HFdel

echo $Files | tr ' ' '\012' | sed 's/data\/eer\/vaac\///g' | sed 's/^\/*//g' > realtime_eer_cleanup_${datef}.txt
chmod 644 realtime_eer_cleanup_${datef}.txt
rm -f realtime_eer_cleanup.txt
ln -s realtime_eer_cleanup_${datef}.txt realtime_eer_cleanup.txt

ssh ${FrontEnd} -l ${TransmitUser} -x ". ~/.profile > /dev/null 2>&1 ; webprods -f /users/dor/afse/eer/.spi/Trace/HFdel/realtime_eer_cleanup.txt -s weather -D 0 -p eer/data/vaac/realtime_eer_cleanup.txt >> /users/dor/afse/eer/.spi/Trace/HFdel.out 2>&1 "

if [ $? -eq 0 ]
then
   echo "Les fichiers ( $Files ) seront effaces sous ${Host} !"
else
   echo "Problemes avec la copie vers le PDS !!!"
fi
