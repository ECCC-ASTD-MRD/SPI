#!/bin/sh
#===============================================================================
# Environnement Canada
# Centre Meteorologique Canadien
# Dorval, Quebec
#
# Projet     : RSMC.
# Nom        : <RSMCJointTransfer.sh>
# Creation   : Mars 2000 - S. Trudel - CMC/CMOE
#
# But        : Permet de transferer les produits standards RSMC
#              sur les differents serveurs, dont les 'backup',
#              pour la page commune RSMC.
#
# Parametres :
#   ${1}     : Repertoire temporaire (/users/dor/afse/eer/test_mensuel).
#   ${2}     : Nombres d'heures correspondant aux ip2.
#
# Remarques  :
#   - pour le 'joint statement' ( voir la NOTE IMPORTANTE ci-bas ):
#
#     cmc  : un scp sur eer@accessdepot dans
#
#             ~/www/mandats/rsmc/usagers/jnt_rsmc/restrict/JNT_STMT/jntreg34.html
#
#     noaa : le ftp 'gus.arlhq.noaa.gov' avec user ( voir ci-bas ) dans
#
#             restrict/JNT_STMT/jnttest.html
#             ( /pub/rsmc/restrict/JNT_STMT/jntreg34.html )
#
#     autr : le ftp 'ftp.bom.gov.au' avec user ( voir ci-bas ) dans
#
#             JNT_STMT/jnttest.html
#             ( /register2/bom050/JNT_STMT/jntreg34.html )
#
#     NOTE IMPORTANTE : lorsque vous copiez ce fichier ( jnttest.html ),
#                       assurez-vous d'avoir la bonne permission pour
#                       ce fichier, soit 644.
#
#   - pour les formats des images (gif) du trajectoire/CANERM generees
#     via la toolbox, on "imprime" l'image :
#
#        small (ex. S<nom> ) avec l'option dimension "4x4"
#        large (ex. L<nom> ) avec l'option dimension "image"
#
#   - pour la page "Resume d'experience", le nom utilise pour l'impression
#     est toujours la meme CVRCA.gif.
#
# Modifications :
#
#   Nom         : J.P. Gauthier - CMC/CMOE
#   Date        : Decembre 2000
#   Description : Transfert de fichiers postscript pour Washington
#
#   Nom         : S. Trudel - CMC/CMOE
#   Date        : Juin 2001
#   Description : Creation d'un fichier postscript unique pour Washington et
#                 notre provoyeur de fax.
#
#   Nom         : S. Trudel - CMC/CMOE
#   Date        : Mai 2002
#   Description : Creation de fichiers *_04 pour couvrir les scenarios 4 et 8.
#
#   Nom         : S. Trudel - CMC/CMOE
#   Date        : Juin 2006
#   Description : Changements des serveurs externes.
#                 ( www/ftp --> collaboration/depot )
#
#   Nom         : S. Trudel - CMC/CMOE
#   Date        : Aout 2007
#   Description : Changements pour le transfert du fichier 'leadrsmc.txt',
#                 le transfert se fait maintenant via le script JNT_SEND.sh.
#
#   Nom         : S. Trudel - CMC/CMOE
#   Date        : Novembre 2007
#   Description : Ajouts pour les transferts de fichiers pour RSMC Obninsk.
#
#===============================================================================

. /usr/local/env/profile_ksh_usr
. $HOME/.eer_ToolDefs/.password

#----- recupere les parametres.

DirTmp=${1}
NbIp2=${2}

#----- se positionne dans le bon repertoire.

cd ${DirTmp}

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

#cat LTJCA.ps CVRCA.ps LICCA_*.ps LTDCA_*.ps > rsmc.ps
cat LTJCA.ps CVRCA.ps $listeic $listetd > rsmc.ps
chmod 644 rsmc.ps

#cat CVRCA.ps LTJCA.ps LICCA_*.ps LTDCA_*.ps > rsmc_fax.ps
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
user rsmcftp ${rsmcftp}
bin
cd restrict/CA
EndFTP

cat ftp_dir >> wash.ftp_dir

echo "put rsmc.ps rsmc.ps" >> wash.ftp_dir

ftp -ni gus.arlhq.noaa.gov < wash.ftp_dir

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
