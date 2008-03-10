/*=========================================================
 * Environnement Canada
 * Centre Meteorologique Canadien
 * 2100 Trans-Canadienne
 * Dorval, Quebec
 *
 * Projet       : Lecture et traitements de fichiers de trajectoires
 * Fichier      : tclTraj.c
 * Creation     : Fevrier 2003 - J.P. Gauthier
 *
 * Description  : Fichier d'entete du module Traj.
 *
 * Remarques    :
 *
 * License      :
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
 * Modification :
 *
 *   Nom        :
 *   Date       :
 *   Description:
 *
 *=========================================================
 */

#include "tclTraj.h"
#include "Projection.h"

/* HashTable Tcl pour les trajectoires */
static Tcl_HashTable TrajTable;
static int TrajInit=0;
static int TrajNo=0;

static int Traj_Cmd(ClientData clientData,Tcl_Interp *Interp,int Objc,Tcl_Obj *CONST Objv[]);
static int Traj_Create(Tcl_Interp *Interp,char* Name);
static int Traj_Destroy(Tcl_Interp *Interp,char *Name);
static int Traj_Define(Tcl_Interp *Interp,char *Name,int Objc,Tcl_Obj *CONST Objv[]);
static int Traj_Stat(Tcl_Interp *Interp,char *Name,int Objc,Tcl_Obj *CONST Objv[]);

/*Vertex buffer*/
extern Vect3d GDB_VBuf[];
extern TIcon IconList[];

/*--------------------------------------------------------------------------------------------------------------
 * Nom          : <Traj_Init>
 * Creation     : Fevrier 2003 J.P. Gauthier
 *
 * But          : Initialisation des commandes Tcl pour utilisation des trajectoires
 *
 * Parametres   :
 *   <Interp>   : Interpreteur Tcl
 *
 * Retour       : Code de retour Tcl
 *
 * Remarques    :
 *
 * Modification   :
 *   Nom        :
 *   Date       :
 *   Description:
 *---------------------------------------------------------------------------------------------------------------
*/
int TclTraj_Init(Tcl_Interp *Interp) {

   if (!TrajInit++) {
      Tcl_InitHashTable(&TrajTable,TCL_STRING_KEYS);
   }
   Tcl_CreateObjCommand(Interp,"trajectory",Traj_Cmd,(ClientData)NULL,(Tcl_CmdDeleteProc*)NULL);

   return TCL_OK;
}

/*--------------------------------------------------------------------------------------------------------------
 * Nom          : <Traj_Cmd>
 * Creation      : Fevrier 2003 J.P. Gauthier
 *
 * But          : Effectuer les commandes du package
 *
 * Parametres    :
 *   <clientData>: Nom de la trajectoire
 *   <Interp>    : Interpreteur Tcl
 *   <Objc>      : Nombre d'arguments
 *   <Objv>      : Pointeur sur la liste des arguments
 *
 * Retour        : Code de retour standard TCL
 *
 * Remarques     :
 *
 * Modification    :
 *   Nom         :
 *   Date        :
 *   Description :
 *---------------------------------------------------------------------------------------------------------------
*/

static int Traj_Cmd(ClientData clientData,Tcl_Interp *Interp,int Objc,Tcl_Obj *CONST Objv[]) {

   TTraj       *traj;
   TDataSpec   *spec;

   int         idx;
   static CONST char *sopt[] = { "create","load","destroy","configure","define","stats","is","all","wipe",NULL };
   enum                opt { CREATE,LOAD,DESTROY,CONFIGURE,DEFINE,STATS,IS,ALL,WIPE };

   Tcl_ResetResult(Interp);

   if (Objc<2) {
      Tcl_WrongNumArgs(Interp,1,Objv,"command ?arg arg ...?");
      return(TCL_ERROR);
   }

   if (Tcl_GetIndexFromObj(Interp,Objv[1],sopt,"command",0,&idx)!=TCL_OK) {
      return(TCL_ERROR);
   }

   switch ((enum opt)idx) {
      case CREATE:
         if(Objc!=3) {
            Tcl_WrongNumArgs(Interp,2,Objv,"traj");
            return(TCL_ERROR);
         }
         return(Traj_Create(Interp,Tcl_GetString(Objv[2])));
         break;

      case LOAD:
         if(Objc!=3) {
            Tcl_WrongNumArgs(Interp,2,Objv,"file");
            return(TCL_ERROR);
         }
         if (!Traj_Load(Interp,Tcl_GetString(Objv[2]),NULL))
            return(TCL_ERROR);
         break;

      case DESTROY:
         if(Objc!=3) {
            Tcl_WrongNumArgs(Interp,2,Objv,"traj");
            return(TCL_ERROR);
         }
         return(Traj_Destroy(Interp,Tcl_GetString(Objv[2])));
         break;

      case CONFIGURE:
         if(Objc<3) {
            Tcl_WrongNumArgs(Interp,2,Objv,"traj ?option?");
            return TCL_ERROR;
         }
         traj=Traj_Get(Tcl_GetString(Objv[2]));
         if (!traj) {
            Tcl_AppendResult(Interp,"invalid traj",(char*)NULL);
            return TCL_ERROR;
         }

         if (strcmp(Tcl_GetString(Objv[3]),"-dataspec")==0) {
            if (Objc==4) {
               if (traj->Spec) {
                  traj->Spec->NRef++;
                  Tcl_SetObjResult(Interp,Tcl_NewStringObj(traj->Spec->Name,-1));
               }
            } else {
               if ((spec=DataSpec_Get(Tcl_GetString(Objv[4])))) {
                  if (traj->Spec) {
                     DataSpec_FreeHash(Interp,traj->Spec->Name);
                  }
                  traj->Spec=spec;
                  spec->NRef++;
               } else {
                  Tcl_AppendResult(Interp,"Traj_Cmd: invalid configuration object",(char*)NULL);
                  return(TCL_ERROR);
               }
            }
         } else {
            if (DataSpec_Config(Interp,traj->Spec,Objc-3,Objv+3)==TCL_OK) {
               return(TCL_OK);
            } else {
               return(TCL_ERROR);
            }
         }
         break;

      case DEFINE:
         if(Objc<3) {
            Tcl_WrongNumArgs(Interp,2,Objv,"traj ?option?");
            return(TCL_ERROR);
         }
         return(Traj_Define(Interp,Tcl_GetString(Objv[2]),Objc-3,Objv+3));
         break;

       case STATS:
         if(Objc<3) {
            Tcl_WrongNumArgs(Interp,2,Objv,"traj ?option?");
            return(TCL_ERROR);
         }
         return(Traj_Stat(Interp,Tcl_GetString(Objv[2]),Objc-3,Objv+3));
         break;

      case IS:
         if(Objc!=3) {
            Tcl_WrongNumArgs(Interp,2,Objv,"traj");
            return(TCL_ERROR);
         }
         if (Traj_Get(Tcl_GetString(Objv[2]))) {
            Tcl_SetObjResult(Interp,Tcl_NewBooleanObj(1));
         } else {
            Tcl_SetObjResult(Interp,Tcl_NewBooleanObj(0));
         }
         break;

      case ALL:
         Tcl_HashAll(Interp,&TrajTable);
         break;

      case WIPE:
         Traj_Wipe();
   }
   return TCL_OK;
}

/*----------------------------------------------------------------------------
 * Nom      : <Traj_Define>
 * Creation : Fevrier 2003 J.P. Gauthier
 *
 * But      : Recuperations des parametres de trajectoires
 *
 * Parametres :
 *  <Interp>  : Interpreteur Tcl
 *  <Name>    : Nom de la projection
 *  <Objc>    : Nombre d'arguments
 *  <Objv>    : Liste des arguments
 *
 * Retour     :
 *  <TCL_...> : Code de retour standard TCL
 *
 * Remarques :
 *
 * Modifications :
 *
 *    Nom         :
 *    Date        :
 *    Description :
 *----------------------------------------------------------------------------
*/
static int Traj_Define(Tcl_Interp *Interp,char *Name,int Objc,Tcl_Obj *CONST Objv[]){

   Tcl_Obj *obj,*sub;
   TTraj   *traj;
   int      i,j,idx;

   static CONST char *sopt[] = { "-DATE","-DATEAP","-MODEL","-ID","-PATH","-MODE","-LEVELTYPE","-BACKWARD","-MIN","-MAX","-PARCELNB","-PARCELS","-PARCEL",NULL };
   enum                opt { DATE,DATEAP,MODEL,ID,PATH,MODE,LEVELTYPE,BACKWARD,MIN,MAX,PARCELNB,PARCELS,PARCEL };

   traj=Traj_Get(Name);
   if (!traj) {
      Tcl_AppendResult(Interp,"\n   Traj_Define: Trajectory id unknown: \"",Name,"\"",(char *)NULL);
      return TCL_ERROR;
   }

   for(i=0;i<Objc;i++) {

      if (Tcl_GetIndexFromObj(Interp,Objv[i],sopt,"option",0,&idx)!=TCL_OK) {
         return TCL_ERROR;
      }

      switch ((enum opt)idx) {
         case DATE:
            if (Objc==1) {
               Tcl_SetObjResult(Interp,Tcl_NewLongObj(traj->Date));
            } else {
            }
            break;

         case DATEAP:
            if (Objc==1) {
               Tcl_SetObjResult(Interp,Tcl_NewLongObj(traj->AP));
            } else {
            }
            break;

         case MODEL:
            if (Objc==1) {
               Tcl_SetObjResult(Interp,Tcl_NewStringObj(traj->Model,-1));
            } else {
            }
            break;

         case ID:
            if (Objc==1) {
               Tcl_SetObjResult(Interp,Tcl_NewStringObj(traj->Id,-1));
            } else {
            }
            break;

         case PATH:
            if (Objc==1) {
               Tcl_SetObjResult(Interp,Tcl_NewStringObj(traj->Path,-1));
           } else {
            }
            break;

         case MODE:
            if (Objc==1) {
               Tcl_SetObjResult(Interp,Tcl_NewIntObj(traj->Mode));
            } else {
            }
            break;

         case LEVELTYPE:
            if (Objc==1) {
               Tcl_SetObjResult(Interp,Tcl_NewStringObj((char*)&traj->Type,1));
            } else {
            }
            break;

         case BACKWARD:
            if (Objc==1) {
               Tcl_SetObjResult(Interp,Tcl_NewIntObj(traj->Back));
            } else {
            }
            break;

         case MAX:
            if (Objc==1) {
               Tcl_SetObjResult(Interp,Tcl_NewDoubleObj(traj->Max));
            } else {
            }
            break;

         case MIN:
            if (Objc==1) {
               Tcl_SetObjResult(Interp,Tcl_NewDoubleObj(traj->Min));
            } else {
            }
            break;

         case PARCELNB:
            if (Objc==1) {
               Tcl_SetObjResult(Interp,Tcl_NewIntObj(traj->NPr));
            } else {
            }
            break;

          case PARCELS:
            if (Objc==1) {
               obj=Tcl_NewListObj(0,NULL);
               for (j=0;j<traj->NPr;j++) {
                  sub=Tcl_NewListObj(0,NULL);
                  Tcl_ListObjAppendElement(Interp,sub,Tcl_NewLongObj(traj->Pr[j].Date));
                  Tcl_ListObjAppendElement(Interp,sub,Tcl_NewDoubleObj(traj->Pr[j].Co.lat));
                  Tcl_ListObjAppendElement(Interp,sub,Tcl_NewDoubleObj(traj->Pr[j].Co.lon));
                  Tcl_ListObjAppendElement(Interp,sub,Tcl_NewDoubleObj(traj->Pr[j].Sig));
                  Tcl_ListObjAppendElement(Interp,sub,Tcl_NewDoubleObj(traj->Pr[j].Pres));
                  Tcl_ListObjAppendElement(Interp,sub,Tcl_NewDoubleObj(traj->Pr[j].Co.elev));
                  Tcl_ListObjAppendElement(Interp,sub,Tcl_NewDoubleObj(traj->Pr[j].X));
                  Tcl_ListObjAppendElement(Interp,sub,Tcl_NewDoubleObj(traj->Pr[j].Dist));
                  Tcl_ListObjAppendElement(Interp,sub,Tcl_NewDoubleObj(traj->Pr[j].Speed));
                  Tcl_ListObjAppendElement(Interp,obj,sub);
               }
               Tcl_SetObjResult(Interp,obj);
            } else {
            }
            break;

         case PARCEL:
            if (Objc==2) {
               i++;
               if (strcmp(Tcl_GetString(Objv[i]),"end")==0) {
                  j=traj->NPr-1;
               } else {
                  Tcl_GetIntFromObj(Interp,Objv[i],&j);
               }
               if (j<traj->NPr) {
                  sub=Tcl_NewListObj(0,NULL);
                  Tcl_ListObjAppendElement(Interp,sub,Tcl_NewLongObj(traj->Pr[j].Date));
                  Tcl_ListObjAppendElement(Interp,sub,Tcl_NewDoubleObj(traj->Pr[j].Co.lat));
                  Tcl_ListObjAppendElement(Interp,sub,Tcl_NewDoubleObj(traj->Pr[j].Co.lon));
                  Tcl_ListObjAppendElement(Interp,sub,Tcl_NewDoubleObj(traj->Pr[j].Sig));
                  Tcl_ListObjAppendElement(Interp,sub,Tcl_NewDoubleObj(traj->Pr[j].Pres));
                  Tcl_ListObjAppendElement(Interp,sub,Tcl_NewDoubleObj(traj->Pr[j].Co.elev));
                  Tcl_ListObjAppendElement(Interp,sub,Tcl_NewDoubleObj(traj->Pr[j].X));
                  Tcl_ListObjAppendElement(Interp,sub,Tcl_NewDoubleObj(traj->Pr[j].Dist));
                  Tcl_ListObjAppendElement(Interp,sub,Tcl_NewDoubleObj(traj->Pr[j].Speed));
                  Tcl_SetObjResult(Interp,sub);
               }
            } else {
            }
      }
   }

   return TCL_OK;
}

/*----------------------------------------------------------------------------
 * Nom      : <Traj_Stat>
 * Creation : Fevrier 2003 - J.P. Gauthier - CMC/CMOE
 *
 * But      : Effectue la configuration des statistiques et le retour des valeurs
 *            si il n'y a pas de valeur specifie (seulement le token).
 *
 * Parametres     :
 *  <Interp>      : Interpreteur TCL
 *  <Field>       : Pointeur sur le champs
 *  <Objc>        : Nombre d'arguments
 *  <Objv>        : Liste des arguments
 *
 * Retour:
 *  <TCL_...> : Code d'erreur de TCL.
 *
 * Remarques :
 *
 * Modifications :
 *
 *    Nom         :
 *    Date        :
 *    Description :
 *----------------------------------------------------------------------------
*/
static int Traj_Stat(Tcl_Interp *Interp,char *Name,int Objc,Tcl_Obj *CONST Objv[]){

   TTraj   *traj;
   int      i,idx;

   static CONST char *sopt[] = { "-tag",NULL };
   enum                opt { TAG };

   traj=Traj_Get(Name);
   if (!traj) {
      Tcl_AppendResult(Interp,"\n   Traj_Define: Trajectory id unknown: \"",Name,"\"",(char *)NULL);
      return TCL_ERROR;
   }

   for(i=0;i<Objc;i++) {
      if (Tcl_GetIndexFromObj(Interp,Objv[i],sopt,"option",0,&idx)!=TCL_OK) {
         return TCL_ERROR;
      }

      switch ((enum opt)idx) {
         case TAG:
            if (Objc==1) {
               if (traj->Tag) {
                  Tcl_SetObjResult(Interp,traj->Tag);
               }
            } else {
               if (traj->Tag) {
                  Tcl_DecrRefCount(traj->Tag);
               }
               traj->Tag=Objv[++i];
               Tcl_IncrRefCount(traj->Tag);
            }
            break;
      }
   }

   return TCL_OK;
}

/*--------------------------------------------------------------------------------------------------------------
 * Nom          : <Traj_Create>
 * Creation     : Fevrier 2003 J.P. Gauthier
 *
 * But          : Creation d'un objet trajectoire et insertion d'un nouveau nom dans la table.
 *
 * Parametres   :
 *   <Interp>   : Interpreteur Tcl
 *   <Name>     : Nom de la trajectoire a creer
 *
 * Retour       : Code de retour standard TCL
 *
 * Remarques    :
 *
 * Modification   :
 *   Nom        :
 *   Date       :
 *   Description:
 *---------------------------------------------------------------------------------------------------------------
*/

static int Traj_Create(Tcl_Interp *Interp,char *Name) {

   TTraj *traj=NULL;

   if (!(traj=(TTraj*)Tcl_HashPut(Interp,&TrajTable,Name,sizeof(TTraj)))) {
      return(TCL_ERROR);
   }

   if (!(traj->Spec=DataSpec_Create(Interp,NULL))) {
      return(TCL_ERROR);
   }

   /*Initialisation de la structure traj*/
   traj->Tag=NULL;

   return(TCL_OK);
}

/*--------------------------------------------------------------------------------------------------------------
 * Nom          : <Traj_Destroy>
 * Creation     : Fevrier 2003 J.P. Gauthier
 *
 * But          : Destruction d'une trajectoire a partir de son nom.
 *
 * Parametres   :
 *   <Interp>   : Interpreteur Tcl
 *   <Name>     : Nom de la trajectoire a detruire
 *
 * Retour       : Code de retour standard TCL
 *
 * Remarques :
 *
 * Modification   :
 *   Nom        :
 *   Date       :
 *   Description:
 *---------------------------------------------------------------------------------------------------------------
*/
static int Traj_Destroy(Tcl_Interp *Interp,char *Name) {

   TTraj *traj=NULL;

   if ((traj=(TTraj*)Tcl_HashDel(&TrajTable,Name))) {
      DataSpec_FreeHash(Interp,traj->Spec->Name);
      Traj_Free(traj);
   }
   return(TCL_OK);
}

/*--------------------------------------------------------------------------------------------------------------
 * Nom          : <Traj_Free>
 * Creation     : Juin 2003 J.P. Gauthier
 *
 * But          : Liberer la memoire associe a une trajectoire.
 *
 * Parametres   :
 *   <Traj>     : Trajectoire
 *
 * Retour       :
 *
 * Remarques :
 *
 * Modification   :
 *   Nom        :
 *   Date       :
 *   Description:
 *---------------------------------------------------------------------------------------------------------------
*/
void Traj_Free(TTraj *Traj) {

   if (Traj->Tag) Tcl_DecrRefCount(Traj->Tag);

   free(Traj->Pr);
   free(Traj);
}

/*--------------------------------------------------------------------------------------------------------------
 * Nom          : <Traj_Get>
 * Creation     : Fevrier 2003 J.P. Gauthier
 *
 * But          : Obtenir un objet trajectoire en fonction de son nom dans la table de Tcl TrajTable
 *
 * Parametres   :
 *   <Name>     : Nom de l'objet trajectoire a obtenir.
 *
 * Retour       : Une structure TTraj ou un pointeur NULL si rien trouve.
 *
 * Remarques    :
 *
 * Modification   :
 *   Nom        :
 *   Date       :
 *   Description:
 *---------------------------------------------------------------------------------------------------------------
*/
TTraj* Traj_Get(char *Name) {
   return((TTraj*)Tcl_HashGet(&TrajTable,Name));
}

/*--------------------------------------------------------------------------------------------------------------
 * Nom          : <Traj_Load>
 * Creation     : Fevrier 2003 J.P. Gauthier
 *
 * But          : Verifier le type de trajcetoire (ARL ou CMC) et proceder
 *                a la lecture.
 *
 * Parametres   :
 *   <Interp>   : L'interpreteur Tcl (IN)
 *   <File>     : Nom du fichier (IN)
 *   <Traj>     : Tableau de retour des trajectoires
 *
 * Retour       : Code d'erreur standard TCL
 *
 * Remarques    :
 *
 * Modification   :
 *   Nom        :
 *   Date       :
 *   Description:
 *---------------------------------------------------------------------------------------------------------------
*/
int Traj_Load(Tcl_Interp *Interp,char *File,TTraj **Traj) {

   FILE *stream;
   char  buf[4];
   int   nb=0;

   stream=fopen(File,"r");

   if (!stream) {
      if (Interp)
         Tcl_AppendResult(Interp,"\n   Traj_Load :  Could not open trajectory file ",File,(char*)NULL);
      return(0);
   }

   /*Read first line to figure out format*/
   fgets(buf,4,stream);
   rewind(stream);

   if (buf[0]=='\'' || buf[1]=='\'') {
      nb=Traj_LoadCMC(Interp,stream,File,Traj);
   } else if (buf[0]==' ' && buf[1]==' ' && buf[2]==' ') {
      nb=Traj_LoadARL(Interp,stream,File,Traj);
   } else {
      if (Interp)
         Tcl_AppendResult(Interp,"\n   Traj_Load :  Invalid trajectory file ",File,(char*)NULL);
   }
   fclose (stream);
   return(nb);
}

/*--------------------------------------------------------------------------------------------------------------
 * Nom          : <Traj_LoadCMC>
 * Creation     : Fevrier 2003 J.P. Gauthier
 *
 * But          : Chargement d'un fichier trajectoire.
 *
 * Parametres   :
 *   <Interp>   : L'interpreteur Tcl (IN)
 *   <Stream>   : Id du fichier (IN)
 *   <File>     : Nom du fichier (IN)
 *   <Traj>     : Tableau de retour des trajectoires
 *
 * Retour       : Code d'erreur standard TCL
 *
 * Remarques    :
 *
 * Modification   :
 *   Nom        :
 *   Date       :
 *   Description:
 *---------------------------------------------------------------------------------------------------------------
*/
int Traj_LoadCMC(Tcl_Interp *Interp,FILE *Stream,char *File,TTraj **Traj) {

   Tcl_Obj   *obj;
   TTraj     *traj=NULL,head;
   TDataSpec *spec;
   char       buf[512],name[16];
   int        i,j,year,month,day,hour,nb,ap,ntr=0;

   obj=Tcl_NewListObj(0,NULL);

   while(!feof(Stream)) {

      strcpy(head.Path,File);
      strcpy(head.Model,"CMC");

      /*Read header*/
      fgets(buf,512,Stream);
      if (buf[0]!='\'' && buf[1]!='\'') break;
      fgets(buf,512,Stream);strncpy(head.Id,buf,32);head.Id[31]='\0';
      i=strlen(head.Id);
      if (head.Id[i-1]=='\n') { head.Id[i-1]='\0'; }
      strtrim(head.Id,' ');
      fgets(buf,512,Stream);

      /*Read mode and level type*/
      fgets(buf,512,Stream);
      sscanf(buf,"%d,\'%c\' %s",&head.Mode,&head.Type,buf);

      /*Read backward token*/
      fgets(buf,512,Stream);
      if (strncmp(&buf[2],"FALSE",5)==0) {
         head.Back=0;
      } else {
         head.Back=1;
      }

      /*Read dates*/
      fgets(buf,512,Stream);sscanf(buf,"%i %s",&nb,buf);
      fgets(buf,512,Stream);sscanf(buf,"%i %s",&head.Lapse,buf);
      fgets(buf,512,Stream);sscanf(buf,"%i",&head.NPr);
      fgets(buf,512,Stream);sscanf(buf,"%i %i %i %i",&year,&month,&day,&hour);

      head.Date=System_DateTime2Seconds(year*10000+month*100+day,hour*10000);

      /*Check for AP date availability*/
      fgets(buf,512,Stream);
      if (strlen(buf)<20) {
         sscanf(buf,"%i",&head.AP);
         head.AP=System_DateTime2Seconds(head.AP/100,fmod(head.AP,100)*10000);
         ap=1;
      } else {
         head.AP=0;
         ap=0;
      }

      /*Incomplete file*/
      if (feof(Stream)) {
         if (Interp)
            Tcl_AppendResult(Interp,"\n   Traj_LoadCMC :  Invalid trajectory file ",name,(char*)NULL);
         return(0);
      }

      head.NPr--;
      head.Pr=NULL;
      head.Tag=NULL;
      head.Min=1e32;
      head.Max=-1e32;

      /*Read all the parcel info*/
      for(i=0;i<nb;i++) {

         if (Interp) {
            sprintf(name,"TRAJ%i",TrajNo++);
            if (Traj_Create(Interp,name)==TCL_OK) {
               traj=Traj_Get(name);
               spec=traj->Spec;
               Tcl_ListObjAppendElement(Interp,obj,Tcl_NewStringObj(name,-1));
            } else {
               Tcl_AppendResult(Interp,"\n   Traj_LoadCMC :  Could not create trajectory object link ",name,(char*)NULL);
               return(0);
            }
         } else {
            traj=(TTraj*)malloc(sizeof(TTraj));
         }

         memcpy(traj,&head,sizeof(head));
         traj->Spec=spec;
         traj->Pr=(TParcel*)malloc(traj->NPr*sizeof(TParcel));

         if (ap || i!=0)
            fgets(buf,512,Stream);

         for(j=0;j<traj->NPr;j++) {
            fgets(buf,512,Stream);
            sscanf(buf,"%i %lf %lf %f %f %lf %f %f %f",&traj->Pr[j].Date,
               &traj->Pr[j].Co.lat,&traj->Pr[j].Co.lon,&traj->Pr[j].Sig,&traj->Pr[j].Pres,
               &traj->Pr[j].Co.elev,&traj->Pr[j].X,&traj->Pr[j].Dist,&traj->Pr[j].Speed);

            traj->Min=traj->Pr[j].Co.elev<traj->Min?traj->Pr[j].Co.elev:traj->Min;
            traj->Max=traj->Pr[j].Co.elev>traj->Max?traj->Pr[j].Co.elev:traj->Max;
            traj->Pr[j].Co.lon=traj->Pr[j].Co.lon>180.0?traj->Pr[j].Co.lon-360.0:traj->Pr[j].Co.lon;
            traj->Pr[j].Co.lon=traj->Pr[j].Co.lon<-180.0?traj->Pr[j].Co.lon+360.0:traj->Pr[j].Co.lon;

            if (System_IsStamp(traj->Pr[j].Date)) {
               traj->Pr[j].Date=System_Stamp2Seconds(traj->Pr[j].Date);
            } else {
               traj->Pr[j].Date=System_DateTime2Seconds(traj->Pr[j].Date/100,fmod(traj->Pr[j].Date,100)*10000);
            }
         }
         /*Store pointer if asked to*/
         if (Traj) Traj[ntr]=traj;
         ntr++;
      }
   }

   Tcl_SetObjResult(Interp,obj);
   return(ntr);
}

/*--------------------------------------------------------------------------------------------------------------
 * Nom          : <Traj_LoadARL>
 * Creation     : Fevrier 2003 J.P. Gauthier
 *
 * But          : Chargement d'un fichier trajectoire.
 *
 * Parametres   :
 *   <Interp>   : L'interpreteur Tcl (IN)
 *   <Stream>   : Id du fichier (IN)
 *   <File>     : Nom du fichier (IN)
 *   <Traj>     : Tableau de retour des trajectoires
 *
 * Retour       : Code d'erreur standard TCL
 *
 * Remarques    :
 *
 * Modification   :
 *   Nom        :
 *   Date       :
 *   Description:
 *---------------------------------------------------------------------------------------------------------------
*/
int Traj_LoadARL(Tcl_Interp *Interp,FILE *Stream,char *File,TTraj **Traj) {

   Tcl_Obj *obj;
   TTraj    *traj[100];
   char     buf[512],name[16];
   int      i,j[10],mdl,year,month,day,hour,nb,nbp,nbt,ntr=0;
   float    lat,lon,s,h;
   long     pos;

   /*Skip models*/
   fgets(buf,512,Stream);sscanf(buf,"%i",&nb);
   for(i=0;i<nb;i++) {
      fgets(buf,512,Stream);
   }

   /*Read backward token*/
   fgets(buf,512,Stream);sscanf(buf,"%i%s",&nbt,buf);
   if (buf[0]=='F') {
      mdl=0;
   } else {
      mdl=1;
   }

   if (nb>100) {
      Tcl_AppendResult(Interp,"\n   Traj_LoadARL :  Too many parcel in same file (Max 100)",(char*)NULL);
      return(0);
   }

   /*Skip initial positions*/
   obj=Tcl_NewListObj(0,NULL);
   for (i=0;i<nbt;i++) {
      fgets(buf,512,Stream);sscanf(buf,"%i %i %i %i %f %f %f",&year,&month,&day,&hour,&lat,&lon,&h);
      if (Interp) {
         sprintf(name,"TRAJ%i",TrajNo++);
         if (Traj_Create(Interp,name)==TCL_OK) {
            traj[i]=Traj_Get(name);
            Tcl_ListObjAppendElement(Interp,obj,Tcl_NewStringObj(name,-1));
          } else {
            Tcl_AppendResult(Interp,"\n   Traj_LoadARL :  Could not create trajectory object link ",name,(char*)NULL);
            return(0);
         }
      } else {
         traj[i]=(TTraj*)malloc(sizeof(TTraj));
         if (Traj) Traj[ntr]=traj[i];
      }
      ntr++;

      /*Non Y2K format*/
      year=year>50?year+1900:year+2000;
      strcpy(traj[i]->Path,File);
      strcpy(traj[i]->Id,"ARL");
      strcpy(traj[i]->Model,"HYSPLIT");

      traj[i]->Back=mdl;
      traj[i]->AP=0;
      traj[i]->Lapse=0;
      traj[i]->Date=System_DateTime2Seconds(year*10000+month*100+day,hour*10000);
      traj[i]->NPr--;
      traj[i]->Pr=NULL;
      traj[i]->Tag=NULL;
      traj[i]->Min=1e32;
      traj[i]->Max=-1e32;
   }
   Tcl_SetObjResult(Interp,obj);

   fgets(buf,512,Stream);sscanf(buf,"%i%s",&nb,buf);
   for(i=0;i<nbt;i++)
      traj[i]->Type=buf[0];

   /*Parse file to get number of time step*/
   pos=ftell(Stream);
   nbp=0;
   while(!feof(Stream)) {
      fgets(buf,512,Stream);
      nbp++;
   }
   nbp/=nbt;

   for(i=0;i<nbt;i++) {
      traj[i]->NPr=nbp;
      traj[i]->Pr=(TParcel*)malloc(traj[i]->NPr*sizeof(TParcel));
      j[i]=0;
   }

   /*Rewind and read particles*/
   fseek(Stream,pos,SEEK_SET);

   while(!feof(Stream) && j[nbt-1]<nbp) {
      fgets(buf,512,Stream);sscanf(buf,"%i %i %i %i %i %i %i %i %f %f %f %f %f",
         &nb,&mdl,&year,&month,&day,&hour,&mdl,&mdl,&s,&lat,&lon,&h,&s);
      nb--;
      year=year>50?year+1900:year+2000;
      traj[nb]->Pr[j[nb]].Date=System_DateTime2Seconds(year*10000+month*100+day,hour*10000);
      traj[nb]->Pr[j[nb]].Co.lat=lat;
      traj[nb]->Pr[j[nb]].Co.lon=lon;
      traj[nb]->Pr[j[nb]].Sig=0.0;
      traj[nb]->Pr[j[nb]].Pres=s;
      traj[nb]->Pr[j[nb]].Co.elev=h;
      traj[nb]->Pr[j[nb]].X=0.0;
      traj[nb]->Pr[j[nb]].Dist=0.0;
      traj[nb]->Pr[j[nb]].Speed=0.0;
      traj[nb]->Min=traj[nb]->Pr[j[nb]].Co.elev<traj[nb]->Min?traj[nb]->Pr[j[nb]].Co.elev:traj[nb]->Min;
      traj[nb]->Max=traj[nb]->Pr[j[nb]].Co.elev>traj[nb]->Max?traj[nb]->Pr[j[nb]].Co.elev:traj[nb]->Max;
      j[nb]++;
   }

   return(ntr);
}

/*--------------------------------------------------------------------------------------------------------------
 * Nom          : <Traj_Render>
 * Creation     : Fevrier 2003 J.P. Gauthier
 *
 * But          : Rendu de la trajectoire a l'ecran.
 *
 * Parametres  :
 *   <Interp>   : L'interpreteur Tcl
 *   <Traj>     : Trajectoire a afficher
 *   <VP>       : Le viewport ou le rendu doit etre fait
 *   <Proj>     : La projection courante
 *   <GLMode>   : Mode de rendue
 *
 * Retour       :
 *
 * Remarques    :
 *
 * Modification   :
 *   Nom        :
 *   Date       :
 *   Description:
 *---------------------------------------------------------------------------------------------------------------
*/

int Traj_Render(Tcl_Interp *Interp,TTraj *Traj,ViewportItem *VP,Projection *Proj,GLuint GLMode) {

   TDataSpec *spec;
   Coord      co;
   int        i,n;
   char       buf[64];
   double     sz;

   if (!(spec=Traj->Spec))
      return(TCL_OK);

   if (spec->Width<=0)
      return TCL_OK;

   if (Interp) {
      glFeedbackInit(Traj->NPr*40,GL_2D);
      Tk_CanvasPsColor(Interp,VP->canvas,spec->Outline);
      sprintf(buf,"%i setlinewidth 1 setlinecap 1 setlinejoin\n",spec->Width+1);
      Tcl_AppendResult(Interp,buf,(char*)NULL);
   }

   glLineWidth(spec->Width+1);
   glEnable(GL_DEPTH_TEST);
   glEnableClientState(GL_VERTEX_ARRAY);

   glColor3us(0x00,0x00,0x00);
   /*Height markers*/
   if (spec->Mark==1 || spec->Mark==3) {
      for(i=0,n=0;i<Traj->NPr;i++) {
         if (Traj->Pr[i].Date<=Proj->Date || Proj->Date==0) {
            co.lat=Traj->Pr[i].Co.lat;
            co.lon=Traj->Pr[i].Co.lon;
            co.elev=0.0;
            Proj->Type->Project(Proj->Params,&Traj->Pr[i].Co,&GDB_VBuf[n*2],1);
            Proj->Type->Project(Proj->Params,&co,&GDB_VBuf[n*2+1],1);
            n++;
         }
      }
      Proj->Type->Render(Proj,0,GDB_VBuf,NULL,NULL,NULL,GL_LINES,n*2,NULL,NULL);
   }

   /*Shadow (Ground zero)*/
   if (spec->Mark==2 || spec->Mark==3) {
      for(i=0,n=0;i<Traj->NPr;i++) {
         if (Traj->Pr[i].Date<=Proj->Date || Proj->Date==0) {
            co.lat=Traj->Pr[i].Co.lat;
            co.lon=Traj->Pr[i].Co.lon;
            co.elev=0.0;
            Proj->Type->Project(Proj->Params,&co,&GDB_VBuf[n],1);
            n++;
         }
      }
      Proj->Type->Render(Proj,0,GDB_VBuf,NULL,NULL,NULL,GL_LINE_STRIP,n,NULL,NULL);
   }

   if (spec->Outline) {
      glColor3us(spec->Outline->red,spec->Outline->green,spec->Outline->blue);
   }

   /*Ribbon*/
   if (spec->Mark==4) {
      glPolygonMode(GL_FRONT_AND_BACK,GL_FILL);
      glDisable(GL_CULL_FACE);

      for(i=0,n=0;i<Traj->NPr;i++) {
         if (Traj->Pr[i].Date<=Proj->Date || Proj->Date==0) {
            co.lat=Traj->Pr[i].Co.lat;
            co.lon=Traj->Pr[i].Co.lon;
            co.elev=0.0;
            Proj->Type->Project(Proj->Params,&Traj->Pr[i].Co,&GDB_VBuf[n*2],1);
            Proj->Type->Project(Proj->Params,&co,&GDB_VBuf[n*2+1],1);
            n++;
         }
      }
      Proj->Type->Render(Proj,0,GDB_VBuf,NULL,NULL,NULL,GL_QUAD_STRIP,n*2,NULL,NULL);
      glEnable(GL_CULL_FACE);
   } else {

      /*Single Trajectory*/
      for(i=0,n=0;i<Traj->NPr;i++) {
         if (Traj->Pr[i].Date<=Proj->Date || Proj->Date==0) {
            Proj->Type->Project(Proj->Params,&Traj->Pr[i].Co,&GDB_VBuf[n],1);
            n++;
         }
      }
      if (!GLRender->GLZBuf) glDisable(GL_DEPTH_TEST);
      Proj->Type->Render(Proj,0,GDB_VBuf,NULL,NULL,NULL,GL_LINE_STRIP,n,NULL,NULL);
   }

   if (Interp)
      glFeedbackProcess(Interp,GL_2D);

   if (spec->Icon) {
      /*Single Trajectory*/
      glVertexPointer(2,GL_DOUBLE,0,IconList[spec->Icon].Co);

      for(i=0,n=0;i<Traj->NPr;i++) {
         if (Traj->Pr[i].Date<=Proj->Date || Proj->Date==0) {
            glPushName(i);
            glPushMatrix();
            Proj->Type->Locate(Proj,Traj->Pr[i].Co.lat,Traj->Pr[i].Co.lon,1);
            glTranslated(0.0,0.0,ZM(Proj,Traj->Pr[i].Co.elev));

            sz=VP->Ratio*(spec->Size+spec->Width);

            if (Interp) {
               glFeedbackInit(IconList[spec->Icon].Nb*8,GL_2D);
            }

            glScalef(sz,sz,1.0);
            glDrawArrays(IconList[spec->Icon].Type,0,IconList[spec->Icon].Nb);
            glPopMatrix();
            glPopName();

            if (Interp) {
               glFeedbackProcess(Interp,GL_2D);
            }
         }
      }
      if (!GLRender->GLZBuf) glDisable(GL_DEPTH_TEST);
      Proj->Type->Render(Proj,0,GDB_VBuf,NULL,NULL,NULL,GL_LINE_STRIP,n,NULL,NULL);
   }
   glDisableClientState(GL_VERTEX_ARRAY);
   glDisable(GL_DEPTH_TEST);

   return(1);
}

/*----------------------------------------------------------------------------
 * Nom      : <Traj_Wipe>
 * Creation : Juin 2003 - J.P. Gauthier - CMC/CMOE
 *
 * But      : Liberer toutes la memoire allouee par ce package.
 *
 * Parametres     :
 *
 * Retour:
 *
 * Remarques :
 *
 * Modifications :
 *
 *    Nom         :
 *    Date        :
 *    Description :
 *----------------------------------------------------------------------------
*/
void Traj_Wipe() {

   Tcl_HashSearch ptr;
   Tcl_HashEntry  *entry=NULL;

   printf("(INFO) Traj_Wipe: Wiping allocated memory\n");

   entry=Tcl_FirstHashEntry(&TrajTable,&ptr);

   while (entry) {
      Traj_Free((TTraj*)Tcl_GetHashValue(entry));
      Tcl_DeleteHashEntry(entry);
      entry=Tcl_FirstHashEntry(&TrajTable,&ptr);
   }

   Tcl_DeleteHashTable(&TrajTable);
}
