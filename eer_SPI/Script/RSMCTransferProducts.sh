#!/bin/sh
#===============================================================================
# Environnement Canada
# Centre Meteorologique Canadien
# Dorval, Quebec
#
# Projet     : RSMC.
# Nom        : <RSMCTransferProducts.sh>
# Creation   : Mars 2000 - S. Trudel - CMC/CMOE
#
# But        : Permet de transferer les produits standards RSMC
#              sur les differents serveurs, dont les 'backup',
#              pour la page commune RSMC.
#
# Parametres :
#   ${1}     : Path pour les fichiers locaux.
#   ${2}     : Nombres d'heures correspondant aux ip2.
#   ${3}     : Nom du token utilise pour l'archivage des transferts de produits.
#
# Remarques  :
#   - pour les formats des images (gif) generees via la toolbox,
#     on sauvegarde l'image :
#
#        small (ex. S<nom> ) avec l'option dimension "4x4"
#        large (ex. L<nom> ) avec l'option dimension "image"
#
#   - pour la page "Resume d'experience", le nom utilise pour l'impression
#     est toujours la meme CVRCA.gif.
#===============================================================================

. /usr/local/env/profile_ksh_usr
. $HOME/.spi/.password

#----- recupere les parametres.

DirData=${1}
NbIp2=${2}
TokenArchiveRSMC=${3}

#----- se positionne dans le bon repertoire.

cd ${DirData}

#----- copie les produits sur le serveur www du CMC.

chmod 644 CA_DATE.TXT CVRCA* LTJCA* S*.gif L*.gif traject.points

#----- concatene nos fichiers postscripts en un seul fichier 'rsmc.ps'
#      pour les besoins de ARL et un fichier 'rsmc_fax.ps' pour
#      le faxage automatique.

i=1

listeic=""
listetd=""

while [ $i -le $NbIp2 ]
do
   nb=`echo $i | awk '{ printf("%02d", $1) }'`
   listeic="${listeic} LICCA_${nb}.ps"
   listetd="${listetd} LTDCA_${nb}.ps"
   i=`expr $i + 1`
done

cat LTJCA.ps CVRCA.ps $listeic $listetd > rsmc.ps
chmod 644 rsmc.ps

cat CVRCA.ps LTJCA.ps $listeic $listetd > rsmc_fax.ps
chmod 644 rsmc_fax.ps

#----- copie les produits sur notre serveur au CMC.

scp CA_DATE.TXT    eer@accessdepot:www/mandats/rsmc/usagers/jnt_rsmc/restrict/CA/CA_DATE.TXT > /dev/null
scp traject.points eer@accessdepot:www/mandats/rsmc/usagers/jnt_rsmc/restrict/CA/traj.txt > /dev/null
scp CVRCA.gif      eer@accessdepot:www/mandats/rsmc/usagers/jnt_rsmc/restrict/CA/CVRCA.gif > /dev/null
scp STJCA.gif      eer@accessdepot:www/mandats/rsmc/usagers/jnt_rsmc/restrict/CA/STJCA.gif > /dev/null
scp LTJCA.gif      eer@accessdepot:www/mandats/rsmc/usagers/jnt_rsmc/restrict/CA/LTJCA.gif > /dev/null

i=1

while [ $i -le $NbIp2 ]
do
   nb=`echo $i | awk '{ printf("%02d", $1) }'`

   scp SICCA_${nb}.gif   eer@accessdepot:www/mandats/rsmc/usagers/jnt_rsmc/restrict/CA/SICCA_${nb}.gif > /dev/null
   scp LICCA_${nb}.gif   eer@accessdepot:www/mandats/rsmc/usagers/jnt_rsmc/restrict/CA/LICCA_${nb}.gif > /dev/null
   scp STDCA_${nb}.gif   eer@accessdepot:www/mandats/rsmc/usagers/jnt_rsmc/restrict/CA/STDCA_${nb}.gif > /dev/null
   scp LTDCA_${nb}.gif   eer@accessdepot:www/mandats/rsmc/usagers/jnt_rsmc/restrict/CA/LTDCA_${nb}.gif > /dev/null

   i=`expr $i + 1`
done

scp rsmc.ps        eer@accessdepot:www/mandats/rsmc/usagers/jnt_rsmc/restrict/CA/rsmc.ps

#----- efface les anciens fichiers supplementaires.

while [ $i -le 4 ]
do
   nb=`echo $i | awk '{ printf("%02d", $1) }'`

   ssh eer@accessdepot rm -f www/mandats/rsmc/usagers/jnt_rsmc/restrict/CA/SICCA_${nb}.gif > /dev/null
   ssh eer@accessdepot rm -f www/mandats/rsmc/usagers/jnt_rsmc/restrict/CA/LICCA_${nb}.gif > /dev/null
   ssh eer@accessdepot rm -f www/mandats/rsmc/usagers/jnt_rsmc/restrict/CA/STDCA_${nb}.gif > /dev/null
   ssh eer@accessdepot rm -f www/mandats/rsmc/usagers/jnt_rsmc/restrict/CA/LTDCA_${nb}.gif > /dev/null

   i=`expr $i + 1`
done

#----- creer le repertoire externe pour les archives RSMC et en fait une copie.

ssh eer@accessdepot mkdir www/mandats/rsmc/usagers/jnt_rsmc/restrict/CA/arc/${TokenArchiveRSMC}/
scp -p eer@accessdepot:www/mandats/rsmc/usagers/jnt_rsmc/restrict/CA/* eer@accessdepot:www/mandats/rsmc/usagers/jnt_rsmc/restrict/CA/arc/${TokenArchiveRSMC}/

#----- creer le repertoire interne pour les archives RSMC et en fait une copie.

mkdir -p ${DirData}/../RSMCJoin.${TokenArchiveRSMC}/
cp -p * ${DirData}/../RSMCJoin.${TokenArchiveRSMC}/

#----- creation des directives communes pour les serveurs ftp.

cat <<EndFTP > ftp_dir
site umask 022
put CA_DATE.TXT
put traject.points traj.txt
put CVRCA.gif
put STJCA.gif
put LTJCA.gif
mdel SICCA_*.gif
mdel LICCA_*.gif
mdel STDCA_*.gif
mdel LTDCA_*.gif
dir
EndFTP

i=1

while [ $i -le $NbIp2 ]
do
   nb=`echo $i | awk '{ printf("%02d", $1) }'`

cat <<EndFTP >> ftp_dir
put SICCA_${nb}.gif
put LICCA_${nb}.gif
put STDCA_${nb}.gif
put LTDCA_${nb}.gif
EndFTP

   i=`expr $i + 1`
done

#----- copie les produits sur le serveur de Washington.

cat <<EndFTP > wash.ftp_dir
user rsmc02 ${arlftp}
bin
EndFTP

cat ftp_dir >> wash.ftp_dir

echo "put rsmc.ps rsmc.ps" >> wash.ftp_dir

ftp -ni arlftp.arlhq.noaa.gov < wash.ftp_dir

#----- copie les produits sur le serveur de Melbourne.

cat <<EndFTP > melb.ftp_dir
user bom050 ${bom050}
bin
cd CA
EndFTP

cat ftp_dir >> melb.ftp_dir

echo "put rsmc.ps rsmc.ps" >> melb.ftp_dir

ftp -ni ftp.bom.gov.au < melb.ftp_dir

#----- copie les produits sur le serveur d'Obninsk.

cat <<EndFTP > obninsk.ftp_dir
user eer-ftp ${eer-ftp}
bin
cd restrict/CA
EndFTP

cat ftp_dir >> obninsk.ftp_dir

echo "put rsmc.ps rsmc.ps" >> obninsk.ftp_dir

ftp -ni www.feerc.obninsk.org < obninsk.ftp_dir
