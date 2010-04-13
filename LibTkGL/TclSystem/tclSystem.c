/*=============================================================================
 * Environnement Canada
 * Centre Meteorologique Canadian
 * 2100 Trans-Canadienne
 * Dorval, Quebec
 *
 * Projet    : Librairie Tcl controle des fonctions systemes.
 * Fichier   : tclSystem.c
 * Creation  : Mai 2009 - J.P. Gauthier - CMC/CMOE
 *
 * Description: Permet de faire des appels aux fonctions systemes non definie
 *              de maniere standard dans Tcl.
 *
 * Remarques :
 *
 * License   :
 *    This library is free software; you can redistribute it and/or
 *    modify it under the terms of the GNU Lesser General Public
 *    License as published by the Free Software Foundation,
 *    version 2.1 of the License.
 *
 *    This library is distributed in the hope that it will be useful,
 *    but WITHOUT ANY WARRANTY; without even the implied warranty of
 *    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 *    Lesser General Public License for more details.
 *
 *    You should have received a copy of the GNU Lesser General Public
 *    License along with this library; if not, write to the
 *    Free Software Foundation, Inc., 59 Temple Place - Suite 330,
 *    Boston, MA 02111-1307, USA.
 *
 *==============================================================================
*/
#include "tclSystem.h"

static int System_Cmd(ClientData clientData,Tcl_Interp *Interp,int Objc,Tcl_Obj *CONST Objv[]);
static int System_Deamon(Tcl_Interp *Interp,int Objc,Tcl_Obj *CONST Objv[]);
static int System_FileSystem(Tcl_Interp *Interp,int Objc,Tcl_Obj *CONST Objv[]);
static int System_Limit(Tcl_Interp *Interp,int Objc,Tcl_Obj *CONST Objv[]);
static int System_Process(Tcl_Interp *Interp,int Objc,Tcl_Obj *CONST Objv[]);

/*--------------------------------------------------------------------------------------------------------------
 * Nom          : <TclSystem_Init>
 * Creation     : Mai 2009 J.P. Gauthier
 *
 * But          : Initialisation des commandes Tcl pour les appels systeme
  *
 * Parametres   :
 *   <Interp>   : Interpreteur Tcl
 *
 * Retour       : Code de retour Tcl
 *
 * Remarques    :
 *
 *---------------------------------------------------------------------------------------------------------------
*/
int Tclsystem_Init(Tcl_Interp *Interp) {

   Tcl_CreateObjCommand(Interp,"system",System_Cmd,(ClientData)NULL,(Tcl_CmdDeleteProc *)NULL);

   Tcl_PkgProvide(Interp,"TclSystem",VERSION);
   return(TCL_OK);
}

/*----------------------------------------------------------------------------
 * Nom      : <System_Daemonize>
 * Creation : Mai 1009 - J.P. Gauthier - CMC/CMOE
 *
 * But      : Effectue les operations standards afin de mettre un process en deamon.
 *
 * Parametres     :
 *  <Interp>      : Interpreteur TCL
 *  <ForkOff>     : Fork le process
 *  <LockFile>    : Fidhier lock du process
 *
 * Retour:
 *  <TCL_...> : Code d'erreur de TCL.
 *
 * Remarques :
 *
 *----------------------------------------------------------------------------
*/
int System_Daemonize(Tcl_Interp *Interp,int ForkOff,const char *LockFile) {
   pid_t pid,sid;
   int   lfp=-1;
   char  buf[32];

   /*Fork off the parent process*/
   if (ForkOff) {
      /*already a daemon*/
      if (getppid()==1)
         return(TCL_OK);

      pid=fork();
      if (pid<0) {
         Tcl_AppendResult(Interp,"System_Daemonize: Unable to fork off parent process",(char*)NULL);
         return(TCL_ERROR);
      }
      /*If we got a good PID, then we can exit the parent process*/
      if (pid>0) {
         exit(0);
      }

      /*Create a new SID for the child process*/
      sid=setsid();
      if (sid<0) {
         Tcl_AppendResult(Interp,"System_Daemonize: Unable to create forked process SID",(char*)NULL);
         return(TCL_ERROR);
      }
   }
   /*At this point we are executing as the child process*/

   /*Create the lock file as the current user*/
   if (LockFile && LockFile[0]) {
      if ((lfp=open(LockFile,O_RDONLY))<0) {
         /*There is no lock file so try to create one*/
         if ((lfp=open(LockFile,O_RDWR|O_CREAT,0640))<0) {
            Tcl_AppendResult(Interp,"System_Daemonize: Unable to create lock file ",LockFile," (",strerror(errno),")",(char*)NULL);
            return(TCL_ERROR);
         }
         /*Try to lock the file
         if (lockf(lfp,F_TLOCK,0)<0) {
            Tcl_AppendResult(Interp,"System_Daemonize: Unable to lock file ",LockFile,",(char*)NULL);
            return(TCL_ERROR);
         }*/
      } else {
         read(lfp,buf,32);
         pid=atoi(buf);
         if (!kill(pid,0)) {
            sprintf(buf,"%i",pid);
            Tcl_AppendResult(Interp,"System_Daemonize: Process already exists with pid ",buf,(char*)NULL);
            return(TCL_ERROR);
         } else {
            if ((lfp=open(LockFile,O_RDWR|O_TRUNC,0640))<0) {
               Tcl_AppendResult(Interp,"System_Daemonize: Unable to create lock file ",LockFile," (",strerror(errno),")",(char*)NULL);
               return(TCL_ERROR);
            }
         }
      }
      sprintf(buf,"%i",getpid());
      write(lfp,buf,strlen(buf));
      close(lfp);
   }

   /* Cancel certain signals */
//    signal(SIGCHLD,SIG_DFL); /* A child process dies */
   signal(SIGTSTP,SIG_IGN); /* Various TTY signals */
   signal(SIGTTOU,SIG_IGN);
   signal(SIGTTIN,SIG_IGN);
   signal(SIGHUP, SIG_IGN); /* Ignore hangup signal */
   signal(SIGTERM,SIG_DFL); /* Die on SIGTERM */

   /*Change the file mode mask*/
   umask(0);

   /*Change the current working directory*/
   if ((chdir("/"))<0) {
      Tcl_AppendResult(Interp,"System_Daemonize: Unable to change directory to /",(char*)NULL);
      return(TCL_ERROR);
   }

   /*Redirect standard files to /dev/null*/
   freopen("/dev/null","r",stdin);
   freopen("/dev/null","w",stdout);
   freopen("/dev/null","w",stderr);

   return(TCL_OK);
}

/*----------------------------------------------------------------------------
 * Nom      : <System_Cmd>
 * Creation : Mai 2009 - J.P. Gauthier - CMC/CMOE
 *
 * But      : Appel des commandes relies aux appels system.
 *
 * Parametres     :
 *  <clientData>  : Donnees du module.
 *  <Interp>      : Interpreteur TCL.
 *  <Objc>        : Nombre d'arguments
 *  <Objv>        : Liste des arguments
 *
 * Retour:
 *  <TCL_...> : Code d'erreur de TCL.
 *
 * Remarques :
 *
 *----------------------------------------------------------------------------
*/
static int System_Cmd(ClientData clientData,Tcl_Interp *Interp,int Objc,Tcl_Obj *CONST Objv[]){

   int   idx,pid;

   static CONST char *sopt[] = { "daemonize","fork","limit","process","filesystem",NULL };
   enum               opt { DAEMONIZE,FORK,LIMIT,PROCESS,FILESYSTEM };

   Tcl_ResetResult(Interp);

   if (Objc<2) {
      Tcl_WrongNumArgs(Interp,1,Objv,"command ?arg arg ...?");
      return(TCL_ERROR);
   }

   if (Tcl_GetIndexFromObj(Interp,Objv[1],sopt,"command",0,&idx)!=TCL_OK) {
      return(TCL_ERROR);
   }

   switch ((enum opt)idx) {
      case PROCESS:
         return(System_Process(Interp,Objc-2,Objv+2));
         break;

      case DAEMONIZE:
         return(System_Deamon(Interp,Objc-2,Objv+2));
         break;

      case FORK:
         pid=fork();
         Tcl_SetObjResult(Interp,Tcl_NewIntObj(pid));
         return(TCL_OK);
         break;

      case LIMIT:
         if(Objc<3) {
            Tcl_WrongNumArgs(Interp,2,Objv,"?option?");
            return(TCL_ERROR);
         }
         return(System_Limit(Interp,Objc-2,Objv+2));
         break;

      case FILESYSTEM:
         return(System_FileSystem(Interp,Objc-2,Objv+2));
         break;

   }
   return(TCL_OK);
}

/*----------------------------------------------------------------------------
 * Nom      : <System_Deamon>
 * Creation : Mai 1009 - J.P. Gauthier - CMC/CMOE
 *
 * But      : Effectue la configuration des parametres de deamonisation.
 *
 * Parametres     :
 *  <Interp>      : Interpreteur TCL
 *  <Objc>        : Nombre d'arguments
 *  <Objv>        : Liste des arguments
 *
 * Retour:
 *  <TCL_...> : Code d'erreur de TCL.
 *
 * Remarques :
 *
 *----------------------------------------------------------------------------
*/
static int System_Deamon(Tcl_Interp *Interp,int Objc,Tcl_Obj *CONST Objv[]){

   int   i,idx,fork=0;
   char *file;
   static CONST char *sopt[] = { "-lock","-fork",NULL };
   enum               opt { LOCK,FORK };

   for(i=0;i<Objc;i++) {

      if (Tcl_GetIndexFromObj(Interp,Objv[i],sopt,"option",0,&idx)!=TCL_OK) {
         return(TCL_ERROR);
      }

      switch ((enum opt)idx) {
         case FORK:
            fork=1;
            break;
         case LOCK:
            file=Tcl_GetString(Objv[++i]);
            break;
         }
   }
   return(System_Daemonize(Interp,fork,file));
}

/*----------------------------------------------------------------------------
 * Nom      : <System_FileSystem>
 * Creation : Mai 1009 - J.P. Gauthier - CMC/CMOE
 *
 * But      : Effectue la configuration des parametres de filesystem.
 *
 * Parametres     :
 *  <Interp>      : Interpreteur TCL
 *  <Objc>        : Nombre d'arguments
 *  <Objv>        : Liste des arguments
 *
 * Retour:
 *  <TCL_...> : Code d'erreur de TCL.
 *
 * Remarques :
 *
 *----------------------------------------------------------------------------
*/
static int System_FileSystem(Tcl_Interp *Interp,int Objc,Tcl_Obj *CONST Objv[]){

   struct statfs fs;
   int           i,idx;
   Tcl_Obj      *obj;

   static CONST char *sopt[] = { "-type","-size","-free","-used","-blocksize","-blocks","-blockfree","-blockused","-files",NULL };
   enum               opt { TYPE,SIZE,FREE,USED,BLOCKSIZE,BLOCKS,BLOCKFREE,BLOCKUSED,FILES };

#ifdef _IRIX64_
   if ((statfs(Tcl_GetString(Objv[0]),&fs,sizeof(statfs),0))) {
#else
   if ((statfs(Tcl_GetString(Objv[0]),&fs))) {
#endif
      Tcl_AppendResult(Interp,"System_FileSystem: Unable to get filesystem info for ",Tcl_GetString(Objv[0]),"\n\t",strerror(errno),(char*)NULL);
      return(TCL_ERROR);
   }

   obj=Tcl_NewListObj(0,NULL);

   for(i=1;i<Objc;i++) {
      if (Tcl_GetIndexFromObj(Interp,Objv[i],sopt,"option",0,&idx)!=TCL_OK) {
         return(TCL_ERROR);
      }

      switch ((enum opt)idx) {
         case TYPE:
#ifdef _IRIX64_
            switch(fs.f_fstyp) {
#else
            switch(fs.f_type) {
#endif
               case 0xADFF:     Tcl_ListObjAppendElement(Interp,obj,Tcl_NewStringObj("AFFS",-1)); break;
               case 0x00414A53: Tcl_ListObjAppendElement(Interp,obj,Tcl_NewStringObj("EFS",-1)); break;
               case 0x137D:     Tcl_ListObjAppendElement(Interp,obj,Tcl_NewStringObj("EXT",-1)); break;
               case 0xEF51:     Tcl_ListObjAppendElement(Interp,obj,Tcl_NewStringObj("EXT2_OLD",-1)); break;
               case 0xEF53:     Tcl_ListObjAppendElement(Interp,obj,Tcl_NewStringObj("EXT2",-1)); break;
               case 0xF995E849: Tcl_ListObjAppendElement(Interp,obj,Tcl_NewStringObj("HPFS",-1)); break;
               case 0x9660:     Tcl_ListObjAppendElement(Interp,obj,Tcl_NewStringObj("ISOFS",-1)); break;
               case 0x137F:
               case 0x138F:     Tcl_ListObjAppendElement(Interp,obj,Tcl_NewStringObj("MINIX",-1)); break;
               case 0x2468:
               case 0x2478:     Tcl_ListObjAppendElement(Interp,obj,Tcl_NewStringObj("MINIX2",-1)); break;
               case 0x4d44:     Tcl_ListObjAppendElement(Interp,obj,Tcl_NewStringObj("MSDOS",-1)); break;
               case 0x564c:     Tcl_ListObjAppendElement(Interp,obj,Tcl_NewStringObj("NCP",-1)); break;
               case 0x6969:     Tcl_ListObjAppendElement(Interp,obj,Tcl_NewStringObj("NFS",-1)); break;
               case 0x9fa0:     Tcl_ListObjAppendElement(Interp,obj,Tcl_NewStringObj("PROC",-1)); break;
               case 0x517B:     Tcl_ListObjAppendElement(Interp,obj,Tcl_NewStringObj("SMB",-1)); break;
               case 0x012FF7B4: Tcl_ListObjAppendElement(Interp,obj,Tcl_NewStringObj("XENIX",-1)); break;
               case 0x012FF7B5: Tcl_ListObjAppendElement(Interp,obj,Tcl_NewStringObj("SYSV4",-1)); break;
               case 0x012FF7B6: Tcl_ListObjAppendElement(Interp,obj,Tcl_NewStringObj("SYSV2",-1)); break;
               case 0x012FF7B7: Tcl_ListObjAppendElement(Interp,obj,Tcl_NewStringObj("COH",-1)); break;
               case 0x00011954: Tcl_ListObjAppendElement(Interp,obj,Tcl_NewStringObj("UFS",-1)); break;
               case 0x58465342: Tcl_ListObjAppendElement(Interp,obj,Tcl_NewStringObj("XFS",-1)); break;
               case 0x012FD16D: Tcl_ListObjAppendElement(Interp,obj,Tcl_NewStringObj("XIAFS",-1)); break;
            }
            break;

         case SIZE:
            Tcl_ListObjAppendElement(Interp,obj,Tcl_NewWideIntObj(((double)fs.f_blocks*(fs.f_bsize/1024.0))));
            break;

         case FREE:
            Tcl_ListObjAppendElement(Interp,obj,Tcl_NewWideIntObj(((double)fs.f_bfree*(fs.f_bsize/1024))));
            break;

         case USED:
            Tcl_ListObjAppendElement(Interp,obj,Tcl_NewWideIntObj(((double)fs.f_blocks-fs.f_bfree)*(fs.f_bsize/1024)));
            break;

         case BLOCKSIZE:
            Tcl_ListObjAppendElement(Interp,obj,Tcl_NewWideIntObj(fs.f_bsize));
            break;

         case BLOCKS:
            Tcl_ListObjAppendElement(Interp,obj,Tcl_NewWideIntObj(fs.f_blocks));
            break;

         case BLOCKFREE:
            Tcl_ListObjAppendElement(Interp,obj,Tcl_NewWideIntObj(fs.f_bfree));
            break;

         case BLOCKUSED:
            Tcl_ListObjAppendElement(Interp,obj,Tcl_NewWideIntObj(fs.f_blocks-fs.f_bfree));
            break;

         case FILES:
            Tcl_ListObjAppendElement(Interp,obj,Tcl_NewWideIntObj(fs.f_files));
            break;
      }
   }
   Tcl_SetObjResult(Interp,obj);
   return(TCL_OK);
}
/*----------------------------------------------------------------------------
 * Nom      : <System_Limit>
 * Creation : Mai 1009 - J.P. Gauthier - CMC/CMOE
 *
 * But      : Effectue la configuration des parametres de limites system.
 *
 * Parametres     :
 *  <Interp>      : Interpreteur TCL
 *  <Objc>        : Nombre d'arguments
 *  <Objv>        : Liste des arguments
 *
 * Retour:
 *  <TCL_...> : Code d'erreur de TCL.
 *
 * Remarques :
 *
 *----------------------------------------------------------------------------
*/
int System_LimitGet(Tcl_Interp *Interp,int Resource,int Factor) {

   struct rlimit lim;

   getrlimit(Resource,&lim);
   if (lim.rlim_cur==RLIM_INFINITY) {
      Tcl_SetObjResult(Interp,Tcl_NewStringObj("unlimited",-1));
   } else {
      lim.rlim_cur/=Factor;
      Tcl_SetObjResult(Interp,Tcl_NewIntObj(lim.rlim_cur));
   }
}

int System_LimitSet(Tcl_Interp *Interp,int Resource,int Factor,Tcl_Obj *Value) {

   struct rlimit lim;
   int           val;

   if (Tcl_GetString(Value)[0]=='u') {
      lim.rlim_cur=RLIM_INFINITY;
   } else {
      Tcl_GetIntFromObj(Interp,Value,&val);
      lim.rlim_cur=val*Factor;
   }
   setrlimit(Resource,&lim);
}

static int System_Limit(Tcl_Interp *Interp,int Objc,Tcl_Obj *CONST Objv[]){

   int           i,idx;
   static CONST char *sopt[] = { "-VMEM","-CORE","-CPU","-DATA","-FILESIZE","-LOCK","-MSGQUEUE","-NICE",
                                 "-FILENO","-NPROC","-RMEM","-RTPRIO","-SIGPENDING","-STACK",NULL };
   enum        opt { VMEM,CORE,CPU,DATA,FILESIZE,LOCKS,MSGQUEUE,NICE,FILENO,NPROC,RMEM,RTPRIO,SIGPENDING,STACK };

   for(i=0;i<Objc;i++) {

      if (Tcl_GetIndexFromObj(Interp,Objv[i],sopt,"option",0,&idx)!=TCL_OK) {
         return(TCL_ERROR);
      }

      switch ((enum opt)idx) {

         case VMEM:
            if (Objc==1) {
               System_LimitGet(Interp,RLIMIT_AS,1024);
            } else {
               System_LimitSet(Interp,RLIMIT_AS,1024,Objv[++i]);
            }
            break;

         case STACK:
            if (Objc==1) {
               System_LimitGet(Interp,RLIMIT_STACK,1024);
            } else {
               System_LimitSet(Interp,RLIMIT_STACK,1024,Objv[++i]);
            }
            break;

          case DATA:
            if (Objc==1) {
               System_LimitGet(Interp,RLIMIT_DATA,1024);
            } else {
               System_LimitSet(Interp,RLIMIT_DATA,1024,Objv[++i]);
            }
            break;

          case RMEM:
            if (Objc==1) {
               System_LimitGet(Interp,RLIMIT_RSS,1);
            } else {
               System_LimitSet(Interp,RLIMIT_RSS,1,Objv[++i]);
            }
            break;

          case FILESIZE:
            if (Objc==1) {
               System_LimitGet(Interp,RLIMIT_FSIZE,1024);
            } else {
               System_LimitSet(Interp,RLIMIT_FSIZE,1024,Objv[++i]);
            }
            break;

          case FILENO:
            if (Objc==1) {
               System_LimitGet(Interp,RLIMIT_NOFILE,1);
            } else {
               System_LimitSet(Interp,RLIMIT_NOFILE,1,Objv[++i]);
            }
            break;

        case CORE:
            if (Objc==1) {
               System_LimitGet(Interp,RLIMIT_CORE,1024);
            } else {
               System_LimitSet(Interp,RLIMIT_CORE,1024,Objv[++i]);
            }
            break;

         case CPU:
            if (Objc==1) {
               System_LimitGet(Interp,RLIMIT_CPU,1);
            } else {
               System_LimitSet(Interp,RLIMIT_CPU,1,Objv[++i]);
            }
            break;

         case NPROC:
            if (Objc==1) {
               System_LimitGet(Interp,RLIMIT_NPROC,1);
            } else {
               System_LimitSet(Interp,RLIMIT_NPROC,1,Objv[++i]);
            }
            break;
       }
   }
   return(TCL_OK);
}

/*----------------------------------------------------------------------------
 * Nom      : <System_Process>
 * Creation : Mai 1009 - J.P. Gauthier - CMC/CMOE
 *
 * But      : Effectue la configuration des parametres des process.
 *
 * Parametres     :
 *  <Interp>      : Interpreteur TCL
 *  <Objc>        : Nombre d'arguments
 *  <Objv>        : Liste des arguments
 *
 * Retour:
 *  <TCL_...> : Code d'erreur de TCL.
 *
 * Remarques :
 *
 *----------------------------------------------------------------------------
*/
static int System_Process(Tcl_Interp *Interp,int Objc,Tcl_Obj *CONST Objv[]){

   int   i,idx,fork=0;
   char *file;
   static CONST char *sopt[] = { "-title",NULL };
   enum               opt { TITLE };

   for(i=0;i<Objc;i++) {

      if (Tcl_GetIndexFromObj(Interp,Objv[i],sopt,"option",0,&idx)!=TCL_OK) {
         return(TCL_ERROR);
      }

      switch ((enum opt)idx) {
         case TITLE:
            if (Objc==1) {
//               Tcl_SetObjResult(Interp,Tcl_NewStringObj(argv[0],-1));
            } else {
//               snprintf(argv[0],"%s",Tcl_GetString(Objv[++i]),strlen(argv[0]));
            }
            break;
         }
   }
   return(TCL_OK);
}
