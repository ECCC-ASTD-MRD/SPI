#!/bin/sh
#===============================================================================
# Environnement Canada
# Centre Meteorologique Canadien
# Dorval, Quebec
#
# Projet     : RSMC.
# Nom        : <RSMCTransferJoint.sh>
# Creation   : Mars 2000 - J.P. Gauthier - CMC/CMOE
#
# But        : Permet de transferer un fichier specifique ( joint statement ou
#              leadrsmc.txt ) sur les differents serveurs, dont les 'backup',
#              pour la page commune RSMC.
#
# Parametres :
#   ${1}     : Path/fichier pour le fichier local.
#   ${2}     : Nom pour le fichier remote.
#   ${3}     : Nom du token utilise pour l'archivage des transferts de produits.
#   ${4}     : Path relatif pour l'archivage des transferts de produits.
#
# Remarques  :
#   - ce script permet de copier soit le fichier html du joint statement
#     ou le fichier leadrsmc.txt relatif au RSMC lead.  Notez que ce dernier
#     fichier sera present que si RSMC Montreal est le RSMC lead.
#
#   - pour le 'joint statement' ( voir la NOTE IMPORTANTE ci-bas ):
#
#     cmc  : un scp sur rsmc_jnt@depot.cmc.ec.gc.ca dans
#
#             /data/depot/sshusers/rsmc_jnt/incoming/jntreg34.html
#
#     noaa : le ftp 'arlftp.arlhq.noaa.gov' avec user ( voir ci-bas ) dans
#
#             jntreg34.html
#             ( /jntreg34.html )
#
#     autr : le ftp 'ftp.bom.gov.au' avec user ( voir ci-bas ) dans
#
#             JNT_STMT/jntreg34.html
#             ( /register2/bom050/JNT_STMT/jntreg34.html )
#
#     NOTE IMPORTANTE : lorsque vous copiez ce fichier ( jntreg34.html ),
#                       assurez-vous d'avoir la bonne permission pour
#                       ce fichier, soit 644.
#===============================================================================

. $HOME/.spi/.password

#----- recupere les parametres.

LocalFile=${1}
RemoteName=${2}
PathRSMC=${3}
TokenArchiveRSMC=${4}

#----- copie les produits sur le serveur www du CMC.

if [ -e ${LocalFile} ]
then
   chmod 644 ${LocalFile}
   
   mkdir -p ${PathRSMC}.${TokenArchiveRSMC}

   cp -p ${LocalFile} ${PathRSMC}.${TokenArchiveRSMC}/

   scp ${LocalFile} rsmc_jnt@depot.cmc.ec.gc.ca:incoming/${RemoteName}

   #----- copie les produits sur le serveur de Washington.

   ftp -n arlftp.arlhq.noaa.gov <<EndFTP
bin
user rsmc02 ${arlftp}
put ${LocalFile} ${RemoteName}
chmod 644 ${RemoteName}
EndFTP

   #----- copie les produits sur le serveur de Melbourne.
   #      ( a voir )

   ftp -n ftp.bom.gov.au <<EndFTP
bin
user bom050 ${bom050}
put ${LocalFile} JNT_STMT/${RemoteName}
chmod 644 JNT_STMT/${RemoteName}
EndFTP

   #----- copie les produits sur le serveur d'Obninsk.
   #      ( a voir )

   ftp -n www.feerc.obninsk.org <<EndFTP
bin
user eer-ftp ${eer-ftp}
put ${LocalFile} restrict/JNT_STMT/${RemoteName}
EndFTP

fi