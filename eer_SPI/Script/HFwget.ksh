#!/bin/ksh
#===========================================================================
# Environnement Canada
# Centre Meteorologique Canadien
# Dorval, Quebec
#
# Projet     : Cleanup realtime pour EER.
# Nom        : <HFwget.ksh>
# Creation   : Mars 2006 - S. Trudel - CMC/CMOE
#
# But        : Recuperer et formater les fichiers contenu sous le host et
#              repertoire specifie en argument.
#
# Parametres :
#   <Host>         : nom du hostname.
#   <Dir>          : nom du repertoire.
#   <FileIndexRes> : nom du fichier de sortie contenant les fichiers sur le <Host>://<Dir>.
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

#set -x

#----- Recupere les arguments.

Host=${1}
Dir=${2}
FileIndexRes=${3}

#----- On s'assure de pouvoir se positionner dans
#      le bon repertoire d'EER sur le host
#      ainsi que de la presence du fichier qui
#      contient la liste des fichiers a afficher.

echo ${0} : `date -u '+%Y%m%d - %H%M Z'` > ${HOME}/.spi/Trace/HFwget.out 2>&1

mkdir ${HOME}/.spi/Trace/HFwget_$$
cd ${HOME}/.spi/Trace/HFwget_$$

\rm -f index.*

wget http://${Host}/${Dir}/ >> ${HOME}/.spi/Trace/HFwget.out 2>&1

if [ $? -eq 0 ]
then
   mv index.html file.html

   # 19k 01-Mar-2006 00:46 veniaminof_watch_23.png
   egrep -i "^<IMG SRC" file.html | egrep -v Parent | sed 's/[ ][ ]*/ /g' | sed 's/^.*<a href="\(.*\)".*<\/a> \(.*-.*-.*\) \(.*:.*\) \(.*\) .*/ \4 \2 \3 \1/g' > file.res

   #cat file.res
   cp file.res ${FileIndexRes}
else
   echo "   Repertoire ${PathWeatherOfficeEER} inexistant ou weatheroffice sur le dos !!!"
fi
