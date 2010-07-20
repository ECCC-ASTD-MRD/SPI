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

   static CONST char *sopt[] = { "daemonize","fork","limit","process",NULL };
   enum               opt { DAEMONIZE,FORK,LIMIT,PROCESS };

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
static int System_Limit(Tcl_Interp *Interp,int Objc,Tcl_Obj *CONST Objv[]){

   struct rlimit lim;
   int           i,idx,ival;
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
               getrlimit(RLIMIT_AS,&lim);
               lim.rlim_cur/=1024;
               Tcl_SetObjResult(Interp,Tcl_NewIntObj(lim.rlim_cur));
            } else {
               Tcl_GetIntFromObj(Interp,Objv[++i],&ival);
               lim.rlim_cur=ival*1024;
               setrlimit(RLIMIT_AS,&lim);
            }
            break;

         case STACK:
            if (Objc==1) {
               getrlimit(RLIMIT_STACK,&lim);
               lim.rlim_cur/=1024;
               Tcl_SetObjResult(Interp,Tcl_NewIntObj(lim.rlim_cur));
            } else {
               Tcl_GetIntFromObj(Interp,Objv[++i],&ival);
               lim.rlim_cur=ival*1024;
               setrlimit(RLIMIT_STACK,&lim);
            }
            break;

          case DATA:
            if (Objc==1) {
               getrlimit(RLIMIT_DATA,&lim);
               lim.rlim_cur/=1024;
               Tcl_SetObjResult(Interp,Tcl_NewIntObj(lim.rlim_cur));
            } else {
               Tcl_GetIntFromObj(Interp,Objv[++i],&ival);
               lim.rlim_cur=ival*1024;
               setrlimit(RLIMIT_DATA,&lim);
            }
            break;

          case RMEM:
            if (Objc==1) {
               getrlimit(RLIMIT_RSS,&lim);
               Tcl_SetObjResult(Interp,Tcl_NewIntObj(lim.rlim_cur));
            } else {
               Tcl_GetIntFromObj(Interp,Objv[++i],&ival);
               lim.rlim_cur=ival;
               setrlimit(RLIMIT_RSS,&lim);
            }
            break;

          case FILESIZE:
            if (Objc==1) {
               getrlimit(RLIMIT_FSIZE,&lim);
               lim.rlim_cur/=1024;
               Tcl_SetObjResult(Interp,Tcl_NewIntObj(lim.rlim_cur));
            } else {
               Tcl_GetIntFromObj(Interp,Objv[++i],&ival);
               lim.rlim_cur=ival*1024;
               setrlimit(RLIMIT_FSIZE,&lim);
            }
            break;

          case FILENO:
            if (Objc==1) {
               getrlimit(RLIMIT_NOFILE,&lim);
               Tcl_SetObjResult(Interp,Tcl_NewIntObj(lim.rlim_cur));
            } else {
               Tcl_GetIntFromObj(Interp,Objv[++i],&ival);
               lim.rlim_cur=ival;
               setrlimit(RLIMIT_NOFILE,&lim);
            }
            break;

        case CORE:
            if (Objc==1) {
               getrlimit(RLIMIT_CORE,&lim);
               lim.rlim_cur/=1024;
               Tcl_SetObjResult(Interp,Tcl_NewIntObj(lim.rlim_cur));
            } else {
               Tcl_GetIntFromObj(Interp,Objv[++i],&ival);
               lim.rlim_cur=ival*1024;
               setrlimit(RLIMIT_CORE,&lim);
            }
            break;

         case CPU:
            if (Objc==1) {
               getrlimit(RLIMIT_CPU,&lim);
               Tcl_SetObjResult(Interp,Tcl_NewIntObj(lim.rlim_cur));
            } else {
               Tcl_GetIntFromObj(Interp,Objv[++i],&ival);
               lim.rlim_cur=ival;
               setrlimit(RLIMIT_CPU,&lim);
            }
            break;

         case NPROC:
            if (Objc==1) {
               getrlimit(RLIMIT_NPROC,&lim);
               Tcl_SetObjResult(Interp,Tcl_NewIntObj(lim.rlim_cur));
            } else {
               Tcl_GetIntFromObj(Interp,Objv[++i],&ival);
               lim.rlim_cur=ival;
               setrlimit(RLIMIT_NPROC,&lim);
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
