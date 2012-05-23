/*==============================================================================
 * Environnement Canada
 * Centre Meteorologique Canadian
 * 2100 Trans-Canadienne
 * Dorval, Quebec
 *
 * Projet    : Package de gestion des palette de couleur.
 * Fichier   : tclCMap.c
 * Creation  : Aout 98 - J.P. Gauthier - CMC/CMOE
 *
 * Description: Creation de palette de couleur et gestion de celles ci.
 *
 * Remarques :
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
 *==============================================================================
 */

#include "glStuff.h"
#include "tclCMap.h"
#include "tclData.h"

TCL_DECLARE_MUTEX(MUTEX_CMAP)

static Tcl_HashTable CMapTable;
static int  CMapInit=0;
static long CMapNo=0;

static int CMap_CmdMap(ClientData clientData,Tcl_Interp *Interp,int Objc,Tcl_Obj *CONST Objv[]);
static int CMap_CmdSel(ClientData clientData,Tcl_Interp *Interp,int Objc,Tcl_Obj *CONST Objv[]);
static int CMap_Config(Tcl_Interp *Interp,CMap_Rec *CMap,int Objc,Tcl_Obj *CONST Objv[]);
static int CMap_Control(Tcl_Interp *Interp,CMap_Rec *CMap,int Objc,Tcl_Obj *CONST Objv[]);

/*----------------------------------------------------------------------------
 * Nom      : <TclCMap_Init>
 * Creation : Juin 1998 - J.P. Gauthier - CMC/CMOE
 *
 * But      : Initialise la commande colormap pour Tcl.
 *
 * Parametres :
 *  <interp>  : Interpreteur Tcl
 *
 * Retour:
 *  <int>     : Code de reussite Tcl
 *
 * Remarques :
 *
 *----------------------------------------------------------------------------
*/
int TclCMap_Init(Tcl_Interp *Interp){

   if (!CMapInit++) {
      Tcl_InitHashTable(&CMapTable,TCL_STRING_KEYS);
   }

   Tcl_CreateObjCommand(Interp,"colormap",CMap_CmdMap,(ClientData)NULL,(Tcl_CmdDeleteProc *)NULL);
   Tcl_CreateObjCommand(Interp,"colorsel",CMap_CmdSel,(ClientData)NULL,(Tcl_CmdDeleteProc *)NULL);

   return(TCL_OK);
}

/*----------------------------------------------------------------------------
 * Nom      : <CMap_Get>
 * Creation : Juillet 2000 - J.P. Gauthier - CMC/CMOE
 *
 * But      : Permet de recuperer l'adresse de la palette selon son nom.
 *
 * Parametres    :
 *  <Name>       : Nom de la palette
 *
 * Retour        :
 *  <CMap_Rec>   : Pointeur sur les donnees de la palette
 *
 * Remarques :
 *
 *----------------------------------------------------------------------------
*/
CMap_Rec* CMap_Get(char *Name) {
   return((CMap_Rec*)TclY_HashGet(&CMapTable,Name));
}

/*----------------------------------------------------------------------------
 * Nom      : <CMap_New>
 * Creation : Janvier 2005 - J.P. Gauthier - CMC/CMOE
 *
 * But      : Allocation d'une nouvelle palette.
 *
 * Parametres  :
 *  <Name>     : Identificateur de la palette
 *  <Nb>       : Nombree de couleur maximal
 *
 * Retour:
 *  <Map>      : Nouvelle palette
 *
 * Remarques :
 *
 *----------------------------------------------------------------------------
*/
CMap_Rec* CMap_New(char* Name,int Nb) {

   CMap_Rec *cmap=NULL;
   char      buf[64];
   int       i;

   cmap=(CMap_Rec*)malloc(sizeof(CMap_Rec));

   if (cmap) {
      cmap->NRef=1;

      /*Definir le maximum de couleur disponibles*/
      cmap->NbPixels=(Nb<=0?CR_MAX:(Nb>CR_MAX?CR_MAX:Nb));

      if (!Name) {
         /*Check for non-existing name*/
         sprintf(buf,"COLORMAP_____%li",CMapNo++);
         while (TclY_HashGet(&CMapTable,buf)) {
            sprintf(buf,"COLORMAP_____%li",CMapNo++);
         }
         Name=buf;
      }
      cmap->Name=strdup(Name);

      /*Definir les parametres par defaut*/
      sprintf(cmap->Type[0],"LINEAR");
      sprintf(cmap->Type[1],"LINEAR");
      sprintf(cmap->Type[2],"LINEAR");
      sprintf(cmap->Type[3],"LINEAR");
      cmap->RatioMax = 100;
      cmap->RatioMin = 0;
      cmap->Alpha = 0;
      cmap->Interp = 1;
      cmap->Ratio[0]=cmap->Ratio[1]=cmap->Ratio[2]=cmap->Ratio[3]=100;
      cmap->Min[0]=cmap->Min[1]=cmap->Min[2]=cmap->Min[3]=0.0;
      cmap->Max[0]=cmap->Max[1]=cmap->Max[2]=cmap->Max[3]=0.0;
      cmap->InvertX[0]=cmap->InvertX[1]=cmap->InvertX[2]=cmap->InvertX[3]=0;
      cmap->InvertY[0]=cmap->InvertY[1]=cmap->InvertY[2]=cmap->InvertY[3]=0;

      /*Assigner les pixels*/
      for(i=0;i<CR_MAX;i++) {
         cmap->Color[i][0]=cmap->Color[i][1]=cmap->Color[i][2]=cmap->Color[i][3]=255;
      }
      for(i=0;i<cmap->NbPixels;i++) {
         cmap->Control[i][0] = 0;
         cmap->Control[i][1] = 0;
         cmap->Control[i][2] = 0;
         cmap->Control[i][3] = 0;
         cmap->Table[i][0]   = 0;
         cmap->Table[i][1]   = 0;
         cmap->Table[i][2]   = 0;
         cmap->Table[i][3]   = 255;
      }
   }
   return(cmap);
}

/*----------------------------------------------------------------------------
 * Nom      : <CMap_Put>
 * Creation : Janvier 2005 - J.P. Gauthier - CMC/CMOE
 *
 * But      : Inserer une palette dans la hash table.
 *
 * Parametres  :
 *  <Interp>   : Interpreteur TCL
 *  <Map>      : Palette a inserer
 *
 * Retour:
 *  <TCL_...>  : Code de reussite TCL
 *
 * Remarques :
 *
 *----------------------------------------------------------------------------
*/
int CMap_Put(Tcl_Interp *Interp,CMap_Rec *Map) {

   return (TclY_HashSet(Interp,&CMapTable,Map->Name,Map));
}

/*----------------------------------------------------------------------------
 * Nom      : <CMap_Create>
 * Creation : Juillet 2000 - J.P. Gauthier - CMC/CMOE
 *
 * But      : Creation une palette de couleur.
 *
 * Parametres  :
 *  <Interp>   : Interpreteur TCL
 *  <Name>     : Identificateur de la palette
 *
 * Retour:
 *  <TCL_...>  : Code de reussite TCL
 *
 * Remarques :
 *
 *----------------------------------------------------------------------------
*/
CMap_Rec* CMap_Create(Tcl_Interp *Interp,char *Name){

   Tcl_HashEntry *entry;
   CMap_Rec      *cmap;
   int           new;

   /*Allouer l'espace pour la structure*/
   entry=TclY_CreateHashEntry(&CMapTable,Name,&new);

   if (!new) {
      Tcl_AppendResult(Interp,"\n   CMap_Create: Colormap name already used: \"",Name, "\"",(char *)NULL);
      return((CMap_Rec*)Tcl_GetHashValue(entry));
   }

   cmap=CMap_New(Name,0);

   if (!cmap) {
      Tcl_AppendResult(Interp,"\n  CMap_Create: Could not allocate colormap structure",(char *)NULL);
      return(NULL);
   }

   Tcl_SetHashValue(entry,cmap);
   Tcl_SetObjResult(Interp,Tcl_NewStringObj(Name,-1));

   return(cmap);
}

/*----------------------------------------------------------------------------
 * Nom      : <CMap_Copy>
 * Creation : Aout 2001 - J.P. Gauthier - CMC/CMOE
 *
 * But      : Copier une palette dans une autre.
 *
 * Parametres  :
 *  <Interp>   : Interpreteur TCL
 *  <To>       : Identificateur de la palette receptrice
 *  <From>     : Identificateur de la palette a copier
 *
 * Retour:
 *  <TCL_...>  : Code de reussite TCL
 *
 * Remarques :
 *
 *----------------------------------------------------------------------------
*/
int CMap_Copy(Tcl_Interp *Interp,char *To,char *From) {

   CMap_Rec *to,*from;

   if (!strlen(To)) {
      return(TCL_ERROR);
   }

   from=CMap_Get(From);
   if (!from) {
      Tcl_AppendResult(Interp,"\n  CMap_Copy: Copy from an invalid palette \"",From,"\"",(char *)NULL);
      return(TCL_ERROR);
   }

   if (!(to=CMap_Get(To))) {
      if (!(to=TclY_HashPut(Interp,&CMapTable,To,sizeof(CMap_Rec)))) {
         return(TCL_ERROR);
      }
   }

   memcpy(to,from,sizeof(CMap_Rec));
   to->Name=strdup(To);

   return(TCL_OK);
}

/*----------------------------------------------------------------------------
 * Nom      : <CMap_CmdMap>
 * Creation : Juin 1998 - J.P. Gauthier - CMC/CMOE
 *
 * But      : Gestion des appels aux commandes de palette du package CMap.
 *
 * Parametres :
 *  <clientData> : Nom du viewpoint
 *  <Interp>     : Interpreteur TCL
 *  <Objc>       : Nombre d'arguments
 *  <Objv>       : Pointeur sur la liste des arguments
 *
 * Retour:
 *  <TCL_...>    : Code de reussite Tcl
 *
 * Remarques :
 *
 *----------------------------------------------------------------------------
*/
static int CMap_CmdMap(ClientData clientData,Tcl_Interp *Interp,int Objc,Tcl_Obj *CONST Objv[]){

   int         idx,c,n;
   CMap_Rec   *cmap;
   static CONST char *sopt[] = { "read","write","colorlist","image","create","copy","configure","control","free","is",NULL };
   enum                opt { READ,WRITE,COLORLIST,IMAGE,CREATE,COPY,CONFIGURE,CONTROL,FREE,IS };

   Tcl_ResetResult(Interp);

   if (Objc<2) {
      Tcl_WrongNumArgs(Interp,1,Objv,"command ?arg arg ...?");
      return(TCL_ERROR);
   }

   if (Tcl_GetIndexFromObj(Interp,Objv[1],sopt,"command",0,&idx)!=TCL_OK) {
      return(TCL_ERROR);
   }

   switch ((enum opt)idx) {
      case READ:
         if(Objc!=4) {
            Tcl_WrongNumArgs(Interp,2,Objv,"cmap filename");
            return(TCL_ERROR);
         }
         return(CMap_Read(Interp,CMap_Get(Tcl_GetString(Objv[2])),Tcl_GetString(Objv[3])));
         break;

      case WRITE:
         if(Objc!=4) {
            Tcl_WrongNumArgs(Interp,2,Objv,"cmap filename");
            return(TCL_ERROR);
         }
         return(CMap_Write(Interp,CMap_Get(Tcl_GetString(Objv[2])),Tcl_GetString(Objv[3])));
         break;

      case COLORLIST:
         if(Objc<3) {
            Tcl_WrongNumArgs(Interp,2,Objv,"cmap [red|green|blue|alpha]");
            return(TCL_ERROR);
         }
         if (Objc==3) {
            return(CMap_ColorList(Interp,CMap_Get(Tcl_GetString(Objv[2])),0,1));
         } else {
            switch(Tcl_GetString(Objv[3])[0]) {
               case 'r':c=1;break;
               case 'g':c=2;break;
               case 'b':c=3;break;
               case 'a':c=4;break;
            }
            return(CMap_ColorList(Interp,CMap_Get(Tcl_GetString(Objv[2])),c,1));
         }
         break;

      case IMAGE:
         if (Objc!=4 && Objc!=5) {
            Tcl_WrongNumArgs(Interp,2,Objv,"cmap image [limit]");
            return(TCL_ERROR);
         }
         c=0;
         if (Objc==5) {
            Tcl_GetBooleanFromObj(Interp,Objv[4],&c);
         }
         return(CMap_GetImage(Interp,CMap_Get(Tcl_GetString(Objv[2])),Tcl_GetString(Objv[3]),c));
         break;

      case CREATE:
         if(Objc!=3) {
            Tcl_WrongNumArgs(Interp,2,Objv,"cmap");
            return(TCL_ERROR);
         }
         if (!(cmap=CMap_Create(Interp,Tcl_GetString(Objv[2])))) {
            return(TCL_ERROR);
         }
         if (Objc>3) {
            if (CMapc_Config(Interp,cmap,Objc-3,Objv+3)==TCL_OK) {
               return(TCL_OK);
            } else {
               return(TCL_ERROR);
            }
         }
         break;

      case COPY:
         if(Objc!=4) {
            Tcl_WrongNumArgs(Interp,2,Objv,"cmapto cmapfrom");
            return(TCL_ERROR);
         }
         return(CMap_Copy(Interp,Tcl_GetString(Objv[2]),Tcl_GetString(Objv[3])));
         break;

      case CONFIGURE:
         if(Objc<3) {
            Tcl_WrongNumArgs(Interp,2,Objv,"cmap ?option?");
            return(TCL_ERROR);
         }
         return(CMap_Config(Interp,CMap_Get(Tcl_GetString(Objv[2])),Objc-3,Objv+3));
         break;

      case CONTROL:
         if(Objc<3) {
            Tcl_WrongNumArgs(Interp,2,Objv,"cmap ?option?");
            return(TCL_ERROR);
         }
         return(CMap_Control(Interp,CMap_Get(Tcl_GetString(Objv[2])),Objc-3,Objv+3));
         break;

      case FREE:
         if(Objc<3) {
            Tcl_WrongNumArgs(Interp,2,Objv,"cmap");
            return(TCL_ERROR);
         }
         for(n=2;n<Objc;n++) {
            CMap_Destroy(Interp,Tcl_GetString(Objv[n]));
         }
         return(TCL_OK);
         break;

      case IS:
         if(Objc!=3) {
            Tcl_WrongNumArgs(Interp,2,Objv,"cmap");
            return(TCL_ERROR);
         }
         if (CMap_Get(Tcl_GetString(Objv[2]))) {
            Tcl_SetObjResult(Interp,Tcl_NewBooleanObj(1));
         } else {
            Tcl_SetObjResult(Interp,Tcl_NewBooleanObj(0));
         }
         break;
   }
   return(TCL_OK);
}

/*----------------------------------------------------------------------------
 * Nom      : <CMap_Destroy>
 * Creation : Fevreir 2005 - J.P. Gauthier - CMC/CMOE
 *
 * But      : Libere la palette
 *
 * Parametres :
 *  <Name>    : Identificateur de la palette.
 *
 * Retour     :
 *  <TCL_...> : Code d'erreur de TCL.
 *
 * Remarques :
 *
 *----------------------------------------------------------------------------
*/
int CMap_Destroy(Tcl_Interp *Interp,char *Name) {

   Tcl_HashEntry *entry;
   CMap_Rec *map=NULL;

   if (Name) {
      entry=TclY_FindHashEntry(&CMapTable,Name);

      if (entry) {
         map=(CMap_Rec*)Tcl_GetHashValue(entry);
         if (CMap_Free(map))
            TclY_DeleteHashEntry(entry);
      }
   }
   return(TCL_OK);
}

int CMap_Free(CMap_Rec *Map) {

   if (!Map)
      return(0);

   Tcl_MutexLock(&MUTEX_CMAP);

   if (--Map->NRef) {
      Tcl_MutexUnlock(&MUTEX_CMAP);
      return(0);
   } else {
      if (Map->Name) free(Map->Name);
      free(Map);
   }
   Tcl_MutexUnlock(&MUTEX_CMAP);
   return(1);
}

int CMap_Incr(CMap_Rec *Map) {

   if (Map) {
      Tcl_MutexLock(&MUTEX_CMAP);
      Map->NRef++;
      Tcl_MutexUnlock(&MUTEX_CMAP);

      return(Map->NRef);
   } else {
      return(0);
   }
}

/*----------------------------------------------------------------------------
 * Nom      : <CMap_Config>
 * Creation : Aout 2001 - J.P. Gauthier - CMC/CMOE
 *
 * But      : Gestion des appels aux commandes de configuration des palettes
 *
 * Parametres :
 *  <Interp>  : Interpreteur TCL
 *  <CMap>    : Palette
 *  <Objc>    : Nombre d'arguments
 *  <Objv>    : Pointeur sur la liste des arguments
 *
 * Retour:
 *  <TCL_...> : Code de reussite Tcl
 *
 * Remarques :
 *
 *----------------------------------------------------------------------------
*/
static int CMap_Config(Tcl_Interp *Interp,CMap_Rec *CMap,int Objc,Tcl_Obj *CONST Objv[]){

   Tcl_Obj *obj;
   int      i,ii,idx,index;
   double   val;

   static CONST char *sopt[] = { "-RGBAratio","-MMratio","-curve","-curvepoint","-index","-min","-max","-invertx","-inverty","-interp",NULL };
   enum                opt { RGBARATIO,MMRATIO,CURVE,CURVEPOINT,INDEX,MIN,MAX,INVERTX,INVERTY,INTERP };

   if (!CMap) {
      return(TCL_ERROR);
   }

   for(i=0;i<Objc;i++) {

      if (Tcl_GetIndexFromObj(Interp,Objv[i],sopt,"option",0,&idx)!=TCL_OK) {
         return(TCL_ERROR);
      }

      switch ((enum opt)idx) {
         case RGBARATIO:
            if (Objc==1) {
               obj=Tcl_NewListObj(0,NULL);
               Tcl_ListObjAppendElement(Interp,obj,Tcl_NewIntObj(CMap->Ratio[0]));
               Tcl_ListObjAppendElement(Interp,obj,Tcl_NewIntObj(CMap->Ratio[1]));
               Tcl_ListObjAppendElement(Interp,obj,Tcl_NewIntObj(CMap->Ratio[2]));
               Tcl_ListObjAppendElement(Interp,obj,Tcl_NewIntObj(CMap->Ratio[3]));
               Tcl_SetObjResult(Interp,obj);
            } else {
               Tcl_GetIntFromObj(Interp,Objv[++i],&CMap->Ratio[0]);
               Tcl_GetIntFromObj(Interp,Objv[++i],&CMap->Ratio[1]);
               Tcl_GetIntFromObj(Interp,Objv[++i],&CMap->Ratio[2]);
               Tcl_GetIntFromObj(Interp,Objv[++i],&CMap->Ratio[3]);
            }
            break;

         case MMRATIO:
            if (Objc==1) {
               obj=Tcl_NewListObj(0,NULL);
               Tcl_ListObjAppendElement(Interp,obj,Tcl_NewIntObj(CMap->RatioMin));
               Tcl_ListObjAppendElement(Interp,obj,Tcl_NewIntObj(CMap->RatioMax));
               Tcl_SetObjResult(Interp,obj);
            } else {
               Tcl_GetIntFromObj(Interp,Objv[++i],&CMap->RatioMin);
               Tcl_GetIntFromObj(Interp,Objv[++i],&CMap->RatioMax);
            }
            break;

          case CURVE:
            if (Objc==1) {
               obj=Tcl_NewListObj(0,NULL);
               Tcl_ListObjAppendElement(Interp,obj,Tcl_NewStringObj(CMap->Type[0],-1));
               Tcl_ListObjAppendElement(Interp,obj,Tcl_NewStringObj(CMap->Type[1],-1));
               Tcl_ListObjAppendElement(Interp,obj,Tcl_NewStringObj(CMap->Type[2],-1));
               Tcl_ListObjAppendElement(Interp,obj,Tcl_NewStringObj(CMap->Type[3],-1));
               Tcl_SetObjResult(Interp,obj);
            } else if (Objc==2) {
               ++i;
               if (strcmp(Tcl_GetString(Objv[i]),"red")==0) {
                  Tcl_SetObjResult(Interp,Tcl_NewStringObj(CMap->Type[0],-1));
               } else if (strcmp(Tcl_GetString(Objv[i]),"green")==0) {
                  Tcl_SetObjResult(Interp,Tcl_NewStringObj(CMap->Type[1],-1));
               } else if (strcmp(Tcl_GetString(Objv[i]),"blue")==0) {
                  Tcl_SetObjResult(Interp,Tcl_NewStringObj(CMap->Type[2],-1));
               } else if (strcmp(Tcl_GetString(Objv[i]),"alpha")==0) {
                  Tcl_SetObjResult(Interp,Tcl_NewStringObj(CMap->Type[3],-1));
               } else if (strcmp(Tcl_GetString(Objv[i]),"rgba")==0) {
                  Tcl_ListObjAppendElement(Interp,obj,Tcl_NewStringObj(CMap->Type[0],-1));
                  Tcl_ListObjAppendElement(Interp,obj,Tcl_NewStringObj(CMap->Type[1],-1));
                  Tcl_ListObjAppendElement(Interp,obj,Tcl_NewStringObj(CMap->Type[2],-1));
                  Tcl_ListObjAppendElement(Interp,obj,Tcl_NewStringObj(CMap->Type[3],-1));
                  Tcl_SetObjResult(Interp,obj);
               } else {
                  Tcl_AppendResult(Interp,"CMap_Config: invalid color index must be red,green,blue,alpha or rgba",(char *) NULL);
                  return(TCL_ERROR);
               }
            } else {
               ++i;
               index=-1;
               if (strcmp(Tcl_GetString(Objv[i]),"red")==0)   { index=0; }
               if (strcmp(Tcl_GetString(Objv[i]),"green")==0) { index=1; }
               if (strcmp(Tcl_GetString(Objv[i]),"blue")==0)  { index=2; }
               if (strcmp(Tcl_GetString(Objv[i]),"alpha")==0) { index=3; }
               if (strcmp(Tcl_GetString(Objv[i]),"rgba")==0)  { index=4; }
               if (index==-1) {
                  Tcl_AppendResult(Interp,"CMap_Config: invalid color index must be red,green,blue or alpha",(char *) NULL);
                  return(TCL_ERROR);
               }

               if (Objc==2) {
                  Tcl_SetObjResult(Interp,Tcl_NewStringObj(CMap->Type[index==4?0:index],-1));
               } else {
                  if (index!=4) {
                     strncpy(CMap->Type[index],Tcl_GetString(Objv[++i]),16);
                  } else {
                     strncpy(CMap->Type[0],Tcl_GetString(Objv[++i]),16);
                     strncpy(CMap->Type[1],Tcl_GetString(Objv[i]),16);
                     strncpy(CMap->Type[2],Tcl_GetString(Objv[i]),16);
                     strncpy(CMap->Type[3],Tcl_GetString(Objv[i]),16);
                  }
               }
               CMap_CurveDefine(CMap);
            }
            break;

          case CURVEPOINT:
            if (Objc<2 || Objc>4) {
               Tcl_WrongNumArgs(Interp,2,Objv,"channel [index] [value]");
               return(TCL_ERROR);
            } else {

               i++;
               index=-1;
               if (strcmp(Tcl_GetString(Objv[i]),"red")==0)   { index=0; }
               if (strcmp(Tcl_GetString(Objv[i]),"green")==0) { index=1; }
               if (strcmp(Tcl_GetString(Objv[i]),"blue")==0)  { index=2; }
               if (strcmp(Tcl_GetString(Objv[i]),"alpha")==0) { index=3; }
               if (strcmp(Tcl_GetString(Objv[i]),"rgba")==0)  { index=4; }
               if (index==-1) {
                  Tcl_AppendResult(Interp,"CMap_Config: invalid color index must be red,green,blue or alpha",(char *) NULL);
                  return(TCL_ERROR);
               }

               if (Objc==2) {
                  obj=Tcl_NewListObj(0,NULL);
                  for(ii=0;ii<CMap->NbPixels;ii++) {
                     Tcl_ListObjAppendElement(Interp,obj,Tcl_NewDoubleObj(CMap->Curve[ii][index==4?0:index]));
                  }
                  Tcl_SetObjResult(Interp,obj);
               } else {
                  Tcl_GetIntFromObj(Interp,Objv[++i],&ii);
                  if (ii<0 || ii>(CMap->NbPixels-1)) {
                     Tcl_AppendResult(Interp,"CMap_Config: Curve index out of range",(char *) NULL);
                     return(TCL_ERROR);
                  }
                  if (Objc==3) {
                     Tcl_SetObjResult(Interp,Tcl_NewDoubleObj(CMap->Curve[ii][index==4?0:index]));
                  } else {
                     Tcl_GetDoubleFromObj(Interp,Objv[++i],&val);
                     if (index!=4) {
                        CMap->Type[0][index]='\0';
                        CMap->Curve[index][ii]=val;
                     } else {
                        CMap->Curve[ii][0]=CMap->Curve[ii][1]=CMap->Curve[ii][2]=CMap->Curve[ii][3]=val;
                        CMap->Type[0][0]=CMap->Type[1][0]=CMap->Type[2][0]=CMap->Type[3][0]='\0';
                     }
                  }
               }
            }
            break;

          case INVERTX:
            if (Objc==1) {
               obj=Tcl_NewListObj(0,NULL);
               Tcl_ListObjAppendElement(Interp,obj,Tcl_NewBooleanObj(CMap->InvertX[0]));
               Tcl_ListObjAppendElement(Interp,obj,Tcl_NewBooleanObj(CMap->InvertX[1]));
               Tcl_ListObjAppendElement(Interp,obj,Tcl_NewBooleanObj(CMap->InvertX[2]));
               Tcl_ListObjAppendElement(Interp,obj,Tcl_NewBooleanObj(CMap->InvertX[3]));
               Tcl_SetObjResult(Interp,obj);
            } else if (Objc==2) {
               ++i;
               if (strcmp(Tcl_GetString(Objv[i]),"red")==0) {
                  Tcl_SetObjResult(Interp,Tcl_NewBooleanObj(CMap->InvertX[0]));
               } else if (strcmp(Tcl_GetString(Objv[i]),"green")==0) {
                  Tcl_SetObjResult(Interp,Tcl_NewBooleanObj(CMap->InvertX[1]));
               } else if (strcmp(Tcl_GetString(Objv[i]),"blue")==0) {
                  Tcl_SetObjResult(Interp,Tcl_NewBooleanObj(CMap->InvertX[2]));
               } else if (strcmp(Tcl_GetString(Objv[i]),"alpha")==0) {
                  Tcl_SetObjResult(Interp,Tcl_NewBooleanObj(CMap->InvertX[3]));
               } else if (strcmp(Tcl_GetString(Objv[i]),"rgba")==0) {
                  Tcl_ListObjAppendElement(Interp,obj,Tcl_NewBooleanObj(CMap->InvertX[0]));
                  Tcl_ListObjAppendElement(Interp,obj,Tcl_NewBooleanObj(CMap->InvertX[1]));
                  Tcl_ListObjAppendElement(Interp,obj,Tcl_NewBooleanObj(CMap->InvertX[2]));
                  Tcl_ListObjAppendElement(Interp,obj,Tcl_NewBooleanObj(CMap->InvertX[3]));
                  Tcl_SetObjResult(Interp,obj);
               } else {
                  Tcl_AppendResult(Interp,"CMap_Config: invalid color index must be red,green,blue,alpha or rgba",(char *) NULL);
                  return(TCL_ERROR);
               }
           } else  {
               i++;
               index=-1;
               if (strcmp(Tcl_GetString(Objv[i]),"red")==0)   { index=0; }
               if (strcmp(Tcl_GetString(Objv[i]),"green")==0) { index=1; }
               if (strcmp(Tcl_GetString(Objv[i]),"blue")==0)  { index=2; }
               if (strcmp(Tcl_GetString(Objv[i]),"alpha")==0) { index=3; }
               if (strcmp(Tcl_GetString(Objv[i]),"rgba")==0)  { index=4; }
               if (index==-1) {
                  Tcl_AppendResult(Interp,"CMap_Config: invalid color index must be red,green,blue or alpha",(char *) NULL);
                  return(TCL_ERROR);
               }

               if (Objc==2) {
                  Tcl_SetObjResult(Interp,Tcl_NewBooleanObj(CMap->InvertX[index==4?0:index]));
               } else {
                  if (index!=4) {
                     Tcl_GetBooleanFromObj(Interp,Objv[++i],&CMap->InvertX[index]);
                   } else {
                     Tcl_GetBooleanFromObj(Interp,Objv[++i],&CMap->InvertX[0]);
                     CMap->InvertX[1]=CMap->InvertX[2]=CMap->InvertX[3]=CMap->InvertX[0];
                  }
                  CMap_CurveDefine(CMap);
               }
            }
            break;

          case INVERTY:
            if (Objc==1) {
               obj=Tcl_NewListObj(0,NULL);
               Tcl_ListObjAppendElement(Interp,obj,Tcl_NewBooleanObj(CMap->InvertY[0]));
               Tcl_ListObjAppendElement(Interp,obj,Tcl_NewBooleanObj(CMap->InvertY[1]));
               Tcl_ListObjAppendElement(Interp,obj,Tcl_NewBooleanObj(CMap->InvertY[2]));
               Tcl_ListObjAppendElement(Interp,obj,Tcl_NewBooleanObj(CMap->InvertY[3]));
               Tcl_SetObjResult(Interp,obj);
            } else if (Objc==2) {
               ++i;
               if (strcmp(Tcl_GetString(Objv[i]),"red")==0) {
                  Tcl_SetObjResult(Interp,Tcl_NewBooleanObj(CMap->InvertY[0]));
               } else if (strcmp(Tcl_GetString(Objv[i]),"green")==0) {
                  Tcl_SetObjResult(Interp,Tcl_NewBooleanObj(CMap->InvertY[1]));
               } else if (strcmp(Tcl_GetString(Objv[i]),"blue")==0) {
                  Tcl_SetObjResult(Interp,Tcl_NewBooleanObj(CMap->InvertY[2]));
               } else if (strcmp(Tcl_GetString(Objv[i]),"alpha")==0) {
                  Tcl_SetObjResult(Interp,Tcl_NewBooleanObj(CMap->InvertY[3]));
               } else if (strcmp(Tcl_GetString(Objv[i]),"rgba")==0) {
                  Tcl_ListObjAppendElement(Interp,obj,Tcl_NewBooleanObj(CMap->InvertY[0]));
                  Tcl_ListObjAppendElement(Interp,obj,Tcl_NewBooleanObj(CMap->InvertY[1]));
                  Tcl_ListObjAppendElement(Interp,obj,Tcl_NewBooleanObj(CMap->InvertY[2]));
                  Tcl_ListObjAppendElement(Interp,obj,Tcl_NewBooleanObj(CMap->InvertY[3]));
                  Tcl_SetObjResult(Interp,obj);
               } else {
                  Tcl_AppendResult(Interp,"CMap_Config: invalid color index must be red,green,blue,alpha or rgba",(char *) NULL);
                  return(TCL_ERROR);
               }
            } else {
               i++;
               index=-1;
               if (strcmp(Tcl_GetString(Objv[i]),"red")==0)   { index=0; }
               if (strcmp(Tcl_GetString(Objv[i]),"green")==0) { index=1; }
               if (strcmp(Tcl_GetString(Objv[i]),"blue")==0)  { index=2; }
               if (strcmp(Tcl_GetString(Objv[i]),"alpha")==0) { index=3; }
               if (strcmp(Tcl_GetString(Objv[i]),"rgba")==0)  { index=4; }
               if (index==-1) {
                  Tcl_AppendResult(Interp,"CMap_Config: invalid color index must be red,green,blue or alpha",(char *) NULL);
                  return(TCL_ERROR);
               }

               if (Objc==2) {
                  Tcl_SetObjResult(Interp,Tcl_NewBooleanObj(CMap->InvertY[index==4?0:index]));
               } else {
                  if (index!=4) {
                     Tcl_GetBooleanFromObj(Interp,Objv[++i],&CMap->InvertY[index]);
                  } else {
                     Tcl_GetBooleanFromObj(Interp,Objv[++i],&CMap->InvertY[0]);
                     CMap->InvertY[1]=CMap->InvertY[2]=CMap->InvertY[3]=CMap->InvertY[0];
                  }
                  CMap_CurveDefine(CMap);
               }
            }
            break;

          case MIN:
            if (Objc==1) {
               obj=Tcl_NewListObj(0,NULL);
               Tcl_ListObjAppendElement(Interp,obj,Tcl_NewDoubleObj(CMap->Min[0]));
               Tcl_ListObjAppendElement(Interp,obj,Tcl_NewDoubleObj(CMap->Min[1]));
               Tcl_ListObjAppendElement(Interp,obj,Tcl_NewDoubleObj(CMap->Min[2]));
               Tcl_ListObjAppendElement(Interp,obj,Tcl_NewDoubleObj(CMap->Min[3]));
               Tcl_SetObjResult(Interp,obj);
            } else if (Objc==2) {
               ++i;
               if (strcmp(Tcl_GetString(Objv[i]),"red")==0) {
                  Tcl_SetObjResult(Interp,Tcl_NewDoubleObj(CMap->Min[0]));
               } else if (strcmp(Tcl_GetString(Objv[i]),"green")==0) {
                  Tcl_SetObjResult(Interp,Tcl_NewDoubleObj(CMap->Min[1]));
               } else if (strcmp(Tcl_GetString(Objv[i]),"blue")==0) {
                  Tcl_SetObjResult(Interp,Tcl_NewDoubleObj(CMap->Min[2]));
               } else if (strcmp(Tcl_GetString(Objv[i]),"alpha")==0) {
                  Tcl_SetObjResult(Interp,Tcl_NewDoubleObj(CMap->Min[3]));
               } else if (strcmp(Tcl_GetString(Objv[i]),"rgba")==0) {
                  Tcl_ListObjAppendElement(Interp,obj,Tcl_NewDoubleObj(CMap->Min[0]));
                  Tcl_ListObjAppendElement(Interp,obj,Tcl_NewDoubleObj(CMap->Min[1]));
                  Tcl_ListObjAppendElement(Interp,obj,Tcl_NewDoubleObj(CMap->Min[2]));
                  Tcl_ListObjAppendElement(Interp,obj,Tcl_NewDoubleObj(CMap->Min[3]));
                  Tcl_SetObjResult(Interp,obj);
               } else {
                  Tcl_AppendResult(Interp,"CMap_Config: invalid color index must be red,green,blue or alpha",(char *) NULL);
                  return(TCL_ERROR);
               }
            } else {
               i++;
               index=-1;
               if (strcmp(Tcl_GetString(Objv[i]),"red")==0)   { index=0; }
               if (strcmp(Tcl_GetString(Objv[i]),"green")==0) { index=1; }
               if (strcmp(Tcl_GetString(Objv[i]),"blue")==0)  { index=2; }
               if (strcmp(Tcl_GetString(Objv[i]),"alpha")==0) { index=3; }
               if (strcmp(Tcl_GetString(Objv[i]),"rgba")==0)  { index=4; }
               if (index==-1) {
                  Tcl_AppendResult(Interp,"CMap_Config: invalid color index must be red,green,blue,alpha or rgba",(char *) NULL);
                  return(TCL_ERROR);
               }

               if (Objc==2) {
                 Tcl_SetObjResult(Interp,Tcl_NewDoubleObj(CMap->Min[index==4?0:index]));
               } else {
                  if (index!=4) {
                     Tcl_GetDoubleFromObj(Interp,Objv[++i],&CMap->Min[index]);
                  } else {
                     Tcl_GetDoubleFromObj(Interp,Objv[++i],&CMap->Min[0]);
                     CMap->Min[1]=CMap->Min[2]=CMap->Min[3]=CMap->Min[0];
                  }
               }
            }
            break;

          case MAX:
             if (Objc==1) {
               obj=Tcl_NewListObj(0,NULL);
               Tcl_ListObjAppendElement(Interp,obj,Tcl_NewDoubleObj(CMap->Max[0]));
               Tcl_ListObjAppendElement(Interp,obj,Tcl_NewDoubleObj(CMap->Max[1]));
               Tcl_ListObjAppendElement(Interp,obj,Tcl_NewDoubleObj(CMap->Max[2]));
               Tcl_ListObjAppendElement(Interp,obj,Tcl_NewDoubleObj(CMap->Max[3]));
               Tcl_SetObjResult(Interp,obj);
            } else if (Objc==2) {
               ++i;
               if (strcmp(Tcl_GetString(Objv[i]),"red")==0) {
                  Tcl_SetObjResult(Interp,Tcl_NewDoubleObj(CMap->Max[0]));
               } else if (strcmp(Tcl_GetString(Objv[i]),"green")==0) {
                  Tcl_SetObjResult(Interp,Tcl_NewDoubleObj(CMap->Max[1]));
               } else if (strcmp(Tcl_GetString(Objv[i]),"blue")==0) {
                  Tcl_SetObjResult(Interp,Tcl_NewDoubleObj(CMap->Max[2]));
               } else if (strcmp(Tcl_GetString(Objv[i]),"alpha")==0) {
                  Tcl_SetObjResult(Interp,Tcl_NewDoubleObj(CMap->Max[3]));
               } else if (strcmp(Tcl_GetString(Objv[i]),"rgba")==0) {
                  Tcl_ListObjAppendElement(Interp,obj,Tcl_NewDoubleObj(CMap->Max[0]));
                  Tcl_ListObjAppendElement(Interp,obj,Tcl_NewDoubleObj(CMap->Max[1]));
                  Tcl_ListObjAppendElement(Interp,obj,Tcl_NewDoubleObj(CMap->Max[2]));
                  Tcl_ListObjAppendElement(Interp,obj,Tcl_NewDoubleObj(CMap->Max[3]));
                  Tcl_SetObjResult(Interp,obj);
               } else {
                  Tcl_AppendResult(Interp,"CMap_Config: invalid color index must be red,green,blue,alpha or rgba",(char *) NULL);
                  return(TCL_ERROR);
               }
           } else {
               i++;
               index=-1;
               if (strcmp(Tcl_GetString(Objv[i]),"red")==0)   { index=0; }
               if (strcmp(Tcl_GetString(Objv[i]),"green")==0) { index=1; }
               if (strcmp(Tcl_GetString(Objv[i]),"blue")==0)  { index=2; }
               if (strcmp(Tcl_GetString(Objv[i]),"alpha")==0) { index=3; }
               if (strcmp(Tcl_GetString(Objv[i]),"rgba")==0)  { index=4; }
               if (index==-1) {
                  Tcl_AppendResult(Interp,"CMap_Config: invalid color index must be red,green,blue or alpha",(char *) NULL);
                  return(TCL_ERROR);
               }

               if (Objc==2) {
                 Tcl_SetObjResult(Interp,Tcl_NewDoubleObj(CMap->Max[index==4?0:index]));
               } else {
                  if (index!=4) {
                     Tcl_GetDoubleFromObj(Interp,Objv[++i],&CMap->Max[index]);
                  } else {
                     Tcl_GetDoubleFromObj(Interp,Objv[++i],&CMap->Max[0]);
                     CMap->Max[1]=CMap->Max[2]=CMap->Max[3]=CMap->Max[0];
                  }
               }
            }
            break;

          case INDEX:
            if (Objc!=2 && Objc!=6) {
               Tcl_WrongNumArgs(Interp,2,Objv,"index [red green blue alpha]");
               return(TCL_ERROR);
            }
            Tcl_GetIntFromObj(Interp,Objv[++i],&index);
            if (index<0 || index>=CR_MAX) {
               Tcl_AppendResult(Interp,"CMap_Config: Index out of range",(char *) NULL);
               return(TCL_ERROR);
            }
            if (Objc==2) {
               obj=Tcl_NewListObj(0,NULL);
               Tcl_ListObjAppendElement(Interp,obj,Tcl_NewIntObj(CMap->Color[index][0]));
               Tcl_ListObjAppendElement(Interp,obj,Tcl_NewIntObj(CMap->Color[index][1]));
               Tcl_ListObjAppendElement(Interp,obj,Tcl_NewIntObj(CMap->Color[index][2]));
               Tcl_ListObjAppendElement(Interp,obj,Tcl_NewIntObj(CMap->Color[index][3]));
               Tcl_SetObjResult(Interp,obj);
               return(TCL_OK);
            } else {
               Tcl_GetIntFromObj(Interp,Objv[++i],&ii);
               CMap->Color[index][0]=CMap->Table[index][0] = ii;
               Tcl_GetIntFromObj(Interp,Objv[++i],&ii);
               CMap->Color[index][1]=CMap->Table[index][1] = ii;
               Tcl_GetIntFromObj(Interp,Objv[++i],&ii);
               CMap->Color[index][2]=CMap->Table[index][2] = ii;
               Tcl_GetIntFromObj(Interp,Objv[++i],&ii);
               CMap->Color[index][3]=CMap->Table[index][3] = ii;
               return(TCL_OK);
            }
            break;

          case INTERP:
            if (Objc==1) {
               Tcl_SetObjResult(Interp,Tcl_NewBooleanObj(CMap->Interp));
            } else {
               Tcl_GetBooleanFromObj(Interp,Objv[++i],&CMap->Interp);
               CMap_ControlDefine(CMap);
            }
            break;
      }
   }
   CMap_RatioDefine(CMap);

   return(TCL_OK);
}

/*----------------------------------------------------------------------------
 * Nom      : <CMap_Control>
 * Creation : Decembre 2002 - J.P. Gauthier - CMC/CMOE
 *
 * But      : Gestion des points de controle de definition des palettes
 *
 * Parametres :
 *  <Interp>  : Interpreteur TCL
 *  <CMap>    : Palette
 *  <Objc>    : Nombre d'arguments
 *  <Objv>    : Pointeur sur la liste des arguments
 *
 * Retour:
 *  <TCL_...> : Code de reussite Tcl
 *
 * Remarques :
 *
 *----------------------------------------------------------------------------
*/
static int CMap_Control(Tcl_Interp *Interp,CMap_Rec *CMap,int Objc,Tcl_Obj *CONST Objv[]){

   Tcl_Obj *obj,*item;
   int     i,ii,index,idx,n,nn,nc;
   GLubyte cell[4]={ 0,0,0,0 };

   static CONST char *sopt[] = { "-add","-get","-list","-del","-move","-update",NULL };
   enum                opt { ADD,GET,LIST,DEL,MOVE,UPDATE };

   if (!CMap) {
      return(TCL_ERROR);
   }

   for(i=0;i<Objc;i++) {

      if (Tcl_GetIndexFromObj(Interp,Objv[i],sopt,"option",0,&idx)!=TCL_OK) {
         return(TCL_ERROR);
      }

      switch ((enum opt)idx) {
         case ADD:
            if (Objc!=6) {
               Tcl_WrongNumArgs(Interp,1,Objv,"index [red green blue alpha]");
               return(TCL_ERROR);
            } else {
               Tcl_GetIntFromObj(Interp,Objv[++i],&index);
               Tcl_GetIntFromObj(Interp,Objv[++i],&ii);
               if (index>=0 && index<CR_MAX) {
                  CMap->Control[index][0]=ii;
                  Tcl_GetIntFromObj(Interp,Objv[++i],&ii);
                  CMap->Control[index][1]=ii;
                  Tcl_GetIntFromObj(Interp,Objv[++i],&ii);
                  CMap->Control[index][2]=ii;
                  Tcl_GetIntFromObj(Interp,Objv[++i],&ii);
                  CMap->Control[index][3]=ii;
               }
            }
            break;

         case GET:
            if (Objc!=2) {
               Tcl_WrongNumArgs(Interp,1,Objv,"index");
               return(TCL_ERROR);
            } else {
               Tcl_GetIntFromObj(Interp,Objv[++i],&index);
               ii=-1;
               for(n=0;n<CMap->NbPixels;n++) {
                  if (CMap->Control[n][0]!=0 || CMap->Control[n][1]!=0 || CMap->Control[n][2]!=0 || CMap->Control[n][3]!=0) {
                     ii++;
                  }
                 if (ii==index) {
                     obj=Tcl_NewListObj(0,NULL);
                     Tcl_ListObjAppendElement(Interp,obj,Tcl_NewIntObj(CMap->Control[n][0]));
                     Tcl_ListObjAppendElement(Interp,obj,Tcl_NewIntObj(CMap->Control[n][1]));
                     Tcl_ListObjAppendElement(Interp,obj,Tcl_NewIntObj(CMap->Control[n][2]));
                     Tcl_ListObjAppendElement(Interp,obj,Tcl_NewIntObj(CMap->Control[n][3]));
                     Tcl_SetObjResult(Interp,obj);
                     return(TCL_OK);
                  }
               }
            }
            break;

         case LIST:
            if (Objc!=1 && Objc!=2) {
               Tcl_WrongNumArgs(Interp,1,Objv,"[list]");
               return(TCL_ERROR);
            }
            if (Objc==1) {
               return(CMap_ColorList(Interp,CMap,0,0));
            } else {
               Tcl_ListObjLength(Interp,Objv[++i],&n);
               for(ii=0;ii<n;ii++) {
                  Tcl_ListObjIndex(Interp,Objv[i],ii,&obj);
                  Tcl_ListObjLength(Interp,obj,&nn);
                  if (nn!=5) {
                     Tcl_AppendResult(Interp,"CMap_Control: Invalid number of items",(char*)NULL);
                     return(TCL_ERROR);
                  }
                  Tcl_ListObjIndex(Interp,obj,0,&item);
                  Tcl_GetIntFromObj(Interp,item,&nc);

                  Tcl_ListObjIndex(Interp,obj,1,&item);
                  Tcl_GetIntFromObj(Interp,item,&nn);CMap->Control[nc][0]=nn;
                  Tcl_ListObjIndex(Interp,obj,2,&item);
                  Tcl_GetIntFromObj(Interp,item,&nn);CMap->Control[nc][1]=nn;
                  Tcl_ListObjIndex(Interp,obj,3,&item);
                  Tcl_GetIntFromObj(Interp,item,&nn);CMap->Control[nc][2]=nn;
                  Tcl_ListObjIndex(Interp,obj,4,&item);
                  Tcl_GetIntFromObj(Interp,item,&nn);CMap->Control[nc][3]=nn;
               }
            }
            break;

         case DEL:
            if (Objc==1) {
               memset(CMap->Control,0,4*CR_MAX);
            } else if (Objc==2) {
               Tcl_GetIntFromObj(Interp,Objv[++i],&index);
               if (index>=0 && index<CMap->NbPixels)
                  memset(CMap->Control[index],0,4);
            } else {
               Tcl_WrongNumArgs(Interp,3,Objv,"[index]");
               return(TCL_ERROR);
            }

            break;

         case UPDATE:
            break;

         case MOVE:
            if (Objc!=3) {
               Tcl_WrongNumArgs(Interp,1,Objv,"to from");
               return(TCL_ERROR);
            }
            Tcl_GetIntFromObj(Interp,Objv[++i],&index);
            Tcl_GetIntFromObj(Interp,Objv[++i],&ii);

            if (memcmp(CMap->Control[index],cell,4)==0 && index>=0 && index<CR_MAX && ii>=0 && ii<CMap->NbPixels) {
               memcpy(CMap->Control[index],CMap->Control[ii],4);
               memset(CMap->Control[ii],0,4);
               Tcl_SetObjResult(Interp,Tcl_NewBooleanObj(1));
            } else {
               Tcl_SetObjResult(Interp,Tcl_NewBooleanObj(0));
            }
            break;
      }
   }

   CMap_ControlDefine(CMap);
   CMap_CurveDefine(CMap);
   CMap_RatioDefine(CMap);

   return(TCL_OK);
}

/*----------------------------------------------------------------------------
 * Nom      : <CMap_CmdSel>
 * Creation : Juin 2001 - J.P. Gauthier - CMC/CMOE
 *
 * But      : Gestion des appels aux commandes de selection de couleur du package CMap.
 *
 * Parametres :
 *  <clientData> : Nom du viewpoint
 *  <Interp>     : Interpreteur TCL
 *  <Objc>       : Nombre d'arguments
 *  <Objv>       : Pointeur sur la liste des arguments
 *
 * Retour:
 *  <TCL_...>    : Code de reussite Tcl
 *
 * Remarques :
 *
 *----------------------------------------------------------------------------
*/
static int CMap_CmdSel(ClientData clientData,Tcl_Interp *Interp,int Objc,Tcl_Obj *CONST Objv[]){

   Tcl_Obj *obj;
   double   h=0.0,s,v,r,g,b;

   int         idx;
   static CONST char *sopt[] = { "image","rgb2hsv","hsv2rgb",NULL };
   enum                opt { IMAGE,RGB2HSV,HSV2RGB };

   Tcl_ResetResult(Interp);

   if (Objc<2) {
      Tcl_WrongNumArgs(Interp,1,Objv,"command ?arg arg ...?");
      return(TCL_ERROR);
   }

   if (Tcl_GetIndexFromObj(Interp,Objv[1],sopt,"command",0,&idx)!=TCL_OK) {
      return(TCL_ERROR);
   }

   switch ((enum opt)idx) {
      case IMAGE:
         if(Objc==5) {
            Tcl_GetDoubleFromObj(Interp,Objv[4],&h);
         } else if (Objc!=4) {
            Tcl_WrongNumArgs(Interp,2,Objv,"[h sv a] image value");
            return(TCL_ERROR);
         }
         return CMap_SelImage(Interp,Tcl_GetString(Objv[2])[1],Tcl_GetString(Objv[3]),h);
         break;

      case RGB2HSV:
         if (Objc!=5) {
            Tcl_WrongNumArgs(Interp,2,Objv,"red green blue");
            return(TCL_ERROR);
         }
         Tcl_GetDoubleFromObj(Interp,Objv[2],&r);
         Tcl_GetDoubleFromObj(Interp,Objv[3],&g);
         Tcl_GetDoubleFromObj(Interp,Objv[4],&b);
         CMap_RGB2HSV(r,g,b,&h,&s,&v);

         obj=Tcl_NewListObj(0,NULL);
         Tcl_ListObjAppendElement(Interp,obj,Tcl_NewDoubleObj(h));
         Tcl_ListObjAppendElement(Interp,obj,Tcl_NewDoubleObj(s));
         Tcl_ListObjAppendElement(Interp,obj,Tcl_NewDoubleObj(v));
         Tcl_SetObjResult(Interp,obj);
         break;

      case HSV2RGB:
         if (Objc!=5) {
            Tcl_WrongNumArgs(Interp,2,Objv,"hue saturation variation");
            return(TCL_ERROR);
         }
         Tcl_GetDoubleFromObj(Interp,Objv[2],&h);
         Tcl_GetDoubleFromObj(Interp,Objv[3],&s);
         Tcl_GetDoubleFromObj(Interp,Objv[4],&v);
         CMap_HSV2RGB(&r,&g,&b,h,s,v);

         obj=Tcl_NewListObj(0,NULL);
         Tcl_ListObjAppendElement(Interp,obj,Tcl_NewDoubleObj(r));
         Tcl_ListObjAppendElement(Interp,obj,Tcl_NewDoubleObj(g));
         Tcl_ListObjAppendElement(Interp,obj,Tcl_NewDoubleObj(b));
         Tcl_SetObjResult(Interp,obj);
         break;
   }
   return(TCL_OK);
}

/*----------------------------------------------------------------------------
 * Nom      : <CMap_ColorList>
 * Creation : Fevrier 1999 - J.P. Gauthier - CMC/CMOE
 *
 * But      : Retourne la liste des couleurs de la palette.
 *
 * Parametres :
 *  <Interp>  : Interpreteur TCL
 *  <CMap>    : Palette
 *  <Comp>    : Component (0=all)
 *  <Mode>    : Mode (0=Control point, 1=couleur)
 *
 * Retour:
 *  <TCL_...> : Code de reussite Tcl
 *
 * Remarques :
 *
 *----------------------------------------------------------------------------
*/
int CMap_ColorList(Tcl_Interp *Interp,CMap_Rec *CMap,int Comp,int Mode) {

   Tcl_Obj *obj,*sub;
   int      i;

   if (!CMap) {
      return(TCL_ERROR);
   }

   obj=Tcl_NewListObj(0,NULL);

   for(i=0;i<CMap->NbPixels;i++){
      if (Comp) {
         if (Mode) {
            Tcl_ListObjAppendElement(Interp,obj,Tcl_NewIntObj(CMap->Color[i][Comp-1]));
         } else {
            Tcl_ListObjAppendElement(Interp,obj,Tcl_NewIntObj(CMap->Control[i][Comp-1]));
         }
      } else {
         if (Mode) {
            sub=Tcl_NewListObj(0,NULL);
            Tcl_ListObjAppendElement(Interp,sub,Tcl_NewIntObj(CMap->Color[i][0]));
            Tcl_ListObjAppendElement(Interp,sub,Tcl_NewIntObj(CMap->Color[i][1]));
            Tcl_ListObjAppendElement(Interp,sub,Tcl_NewIntObj(CMap->Color[i][2]));
            Tcl_ListObjAppendElement(Interp,sub,Tcl_NewIntObj(CMap->Color[i][3]));
            Tcl_ListObjAppendElement(Interp,obj,sub);
         } else {
            if (CMap->Control[i][0] || CMap->Control[i][1] || CMap->Control[i][2] || CMap->Control[i][3]) {
               sub=Tcl_NewListObj(0,NULL);
               Tcl_ListObjAppendElement(Interp,sub,Tcl_NewIntObj(i));
               Tcl_ListObjAppendElement(Interp,sub,Tcl_NewIntObj(CMap->Control[i][0]));
               Tcl_ListObjAppendElement(Interp,sub,Tcl_NewIntObj(CMap->Control[i][1]));
               Tcl_ListObjAppendElement(Interp,sub,Tcl_NewIntObj(CMap->Control[i][2]));
               Tcl_ListObjAppendElement(Interp,sub,Tcl_NewIntObj(CMap->Control[i][3]));
               Tcl_ListObjAppendElement(Interp,obj,sub);
            }
         }
      }
   }
   Tcl_SetObjResult(Interp,obj);
   return(TCL_OK);
}

/*----------------------------------------------------------------------------
 * Nom      : <CMap_GetColorString>
 * Creation : Fevrier 1999 - J.P. Gauthier - CMC/CMOE
 *
 * But      : Extrait la valeur de la couleur a l'index specifie et la retourne
 *            a Tcl sous forme de chaine.
 *
 * Parametres :
 *  <Interp>  : Interpreteur TCL
 *  <CMap>    : Palette
 *  <Index>   : Index de la couleur dans la palette
 *
 * Retour:
 *  <TCL_...> : Code de reussite Tcl
 *
 * Remarques :
 *
 *----------------------------------------------------------------------------
*/
int CMap_GetColorString(Tcl_Interp *Interp,CMap_Rec *CMap,int Index){

   char buf[8];

   if (!CMap) {
      return(TCL_ERROR);
   }

   if (Index<0 || Index >=CMap->NbPixels) {
      Tcl_AppendResult(Interp,"FFFFFF",(char*)NULL);
   } else {
      sprintf(buf,"%02x%02x%02x",CMap->Color[Index][0],CMap->Color[Index][1],CMap->Color[Index][2]);
      Tcl_AppendResult(Interp,buf,(char*)NULL);
   }
   return(TCL_OK);
}

/*----------------------------------------------------------------------------
 * Nom      : <CMap_GetImage>
 * Creation : Avril 2001 - J.P. Gauthier - CMC/CMOE
 *
 * But      : Emplir une image Tcl avec la palette specifie.
 *
 * Parametres :
 *  <Interp>  : Interpreteur TCL
 *  <CMap>    : Palette
 *  <Img>     : Image Tcl
 *  <Limit>   : Limit to NbPixels
 *
 * Retour:
 *  <TCL_...> : Code de reussite Tcl
 *
 * Remarques :
 *
 *----------------------------------------------------------------------------
*/
int CMap_GetImage(Tcl_Interp *Interp,CMap_Rec *CMap,char* Img,int Limit){

   double incr,cidx=0;
   int idx,x,y;

   Tk_PhotoImageBlock data;
   Tk_PhotoHandle     handle;

   if (!CMap) {
      Tcl_AppendResult(Interp,"CMap_GetImage: Invalid colormap",(char*)NULL);
      return(TCL_ERROR);
   }

   /*Recuperer le handle de l'image specifie*/
   handle=Tk_FindPhoto(Interp,Img);

   /*Definire les parametres du bock de donnees*/
   Tk_PhotoGetSize(handle,&data.width,&data.height);

   data.pitch=data.width*4;
   data.pixelSize=4;
   data.offset[0]=0;
   data.offset[1]=1;
   data.offset[2]=2;
   data.offset[3]=3;
   data.pixelPtr=(unsigned char*)malloc(data.width*data.height*4*sizeof(unsigned char));

   /*Creer l'image de la palette*/
   if (data.height>data.width) {
      if (Limit) {
         incr=(double)(CR_MAX)/data.height;
      } else {
         incr=(double)(CMap->NbPixels)/data.height;
      }

      for(y=0,idx=0,cidx=0;y<data.height;y++,cidx+=incr) {
         for(x=0;x<data.width;x++) {
            if (cidx>=CMap->NbPixels) {
               data.pixelPtr[idx++]=0;
               data.pixelPtr[idx++]=0;
               data.pixelPtr[idx++]=0;
               data.pixelPtr[idx++]=0;
            } else {
               data.pixelPtr[idx++]=CMap->Color[(int)cidx][0];
               data.pixelPtr[idx++]=CMap->Color[(int)cidx][1];
               data.pixelPtr[idx++]=CMap->Color[(int)cidx][2];
               data.pixelPtr[idx++]=CMap->Color[(int)cidx][3];
            }
         }
      }
   } else {
      if (Limit) {
         incr=(double)(CR_MAX)/data.width;
      } else {
         incr=(double)(CMap->NbPixels)/data.width;
      }
      for(y=0,idx=0;y<data.height;y++,cidx=0) {
         for(x=0;x<data.width;x++,cidx+=incr) {
            if (cidx>=CMap->NbPixels) {
               data.pixelPtr[idx++]=0;
               data.pixelPtr[idx++]=0;
               data.pixelPtr[idx++]=0;
               data.pixelPtr[idx++]=0;
            } else {
               data.pixelPtr[idx++]=CMap->Color[(int)cidx][0];
               data.pixelPtr[idx++]=CMap->Color[(int)cidx][1];
               data.pixelPtr[idx++]=CMap->Color[(int)cidx][2];
               data.pixelPtr[idx++]=CMap->Color[(int)cidx][3];
            }
         }
      }
   }

   /*Envoyer le data dans l'image Tk*/
   Tk_PhotoPutBlock(Interp,handle,&data,0,0,data.width,data.height,TK_PHOTO_COMPOSITE_SET);
   free(data.pixelPtr);

   return(TCL_OK);
}

/*----------------------------------------------------------------------------
 * Nom      : <CMap_Read>
 * Creation : Aout 1999 - J.P. Gauthier - CMC/CMOE
 *
 * But      : Lit une liste de definitions de couleurs et les inseres dans la palette
 *            courante du widget specifie.
 *
 * Parametres :
 *  <Interp>  : Interpreteur TCL
 *  <CMap>    : Palette
 *  <RGBAFile>: Fichier de definitions de couleurs
 *
 * Retour:
 *  <TCL_...> : Code de reussite Tcl
 *
 * Remarques :
 *    On lit toutes les couleurs du fichier mais on insere seulement un nombre
 *    dans la palette, tout depent du nombre de cellules allouee
 *
 *----------------------------------------------------------------------------
*/
int CMap_Read(Tcl_Interp *Interp,CMap_Rec *CMap,char *RGBAFile){

   FILE *fp;
   char buf[256];
   int  idx;

   if (!CMap) {
      Tcl_AppendResult(Interp,"CMap_Read: Invalid colormap",(char *)NULL);
      return(TCL_ERROR);
   }

   /*Clear control table*/
   memset(CMap->Control,0,4*CR_MAX);

   CMap->NbPixels=0;
   CMap->Interp=1;
   CMap->InvertX[0]=0;

   fp = fopen(RGBAFile,"r");
   if (fp == NULL) {
      Tcl_AppendResult(Interp,"CMap_Read: cannot open RGBA definition file\"",RGBAFile,(char *)NULL);
      return(TCL_ERROR);
   }

   /*Read parameters*/
   fgets(buf,256,fp);
   sscanf(buf,"%i %i %i %i %i %i %s %i %i",&CMap->RatioMin,&CMap->RatioMax,&CMap->Ratio[0],&CMap->Ratio[1],&CMap->Ratio[2],&CMap->Ratio[3],CMap->Type[0],&CMap->Interp,&CMap->InvertX[0]);
   strncpy(CMap->Type[1],CMap->Type[0],16);
   strncpy(CMap->Type[2],CMap->Type[0],16);
   strncpy(CMap->Type[3],CMap->Type[0],16);

   CMap->InvertX[3]=CMap->InvertX[2]=CMap->InvertX[1]=CMap->InvertX[0];

   /*Read control point definitions*/
   while (fgets(buf,256,fp)) {

      if (strlen(buf)>18) {
         idx=atoi(buf);
         if (idx>=CR_MAX) {
            Tcl_AppendResult(Interp,"CMap_Read: Index range overflow\"",RGBAFile,(char *)NULL);
            fclose(fp);
            return(TCL_ERROR);
         }

         CMap->Control[idx][0] = atoi(buf+4);
         CMap->Control[idx][1] = atoi(buf+8);
         CMap->Control[idx][2] = atoi(buf+12);
         CMap->Control[idx][3] = atoi(buf+16);
       }
   }


   fclose(fp);

   CMap_ControlDefine(CMap);
   CMap_CurveDefine(CMap);
   CMap_RatioDefine(CMap);

   return(TCL_OK);
}

/*----------------------------------------------------------------------------
 * Nom      : <CMap_Write>
 * Creation : Aout 2001 - J.P. Gauthier - CMC/CMOE
 *
 * But      : Ecrire la liste de definitions de couleurs dans un fichier.
 *
 * Parametres :
 *  <Interp>  : Interpreteur TCL
 *  <CMap>    : Palette
 *  <RGBAFile>: Fichier de definitions de couleurs
 *
 * Retour:
 *  <TCL_...> : Code de reussite Tcl
 *
 * Remarques :
 *
 *----------------------------------------------------------------------------
*/
int CMap_Write(Tcl_Interp *Interp,CMap_Rec *CMap,char *RGBAFile){

   FILE *fp;
   char buf[256];
   int  i;

   if (!CMap) {
      return(TCL_ERROR);
   }

   fp = fopen(RGBAFile,"w");
   if (fp == NULL) {
      Tcl_AppendResult(Interp,"CMap_Write: cannot open RGBA definition file\"",RGBAFile,(char *)NULL);
      return(TCL_ERROR);
   }

   /*Parametres de la palette*/
   sprintf(buf,"%i %i %i %i %i %i %s %i %i\n",CMap->RatioMin,CMap->RatioMax,CMap->Ratio[0],CMap->Ratio[1],CMap->Ratio[2],CMap->Ratio[3],CMap->Type[0],CMap->Interp,CMap->InvertX[0]);
   fputs(buf,fp);

   /*Points de controle*/
   sprintf(buf,"%03i %03i %03i %03i %03i\n",0,CMap->Control[0][0],CMap->Control[0][1],CMap->Control[0][2],CMap->Control[0][3]);
   fputs(buf,fp);

   for(i=1;i<CMap->NbPixels-1;i++) {
      if (CMap->Control[i][0]!=0 || CMap->Control[i][1]!=0 || CMap->Control[i][2]!=0 || CMap->Control[i][3]!=0) {
         sprintf(buf,"%03i %03i %03i %03i %03i\n",i,CMap->Control[i][0],CMap->Control[i][1],CMap->Control[i][2],CMap->Control[i][3]);
         fputs(buf,fp);
      }
   }
   sprintf(buf,"%03i %03i %03i %03i %03i\n",i,CMap->Control[i][0],CMap->Control[i][1],CMap->Control[i][2],CMap->Control[i][3]);
   fputs(buf,fp);

   fclose(fp);
   return(TCL_OK);
}
/*----------------------------------------------------------------------------
 * Nom      : <CMap_ControlDefine>
 * Creation : Decembre 2002 - J.P. Gauthier - CMC/CMOE
 *
 * But      : Applique les  points de controle pour generer la palette.
 *
 * Parametres :
 *  <CMap>    : Palette
 *
 * Retour:
 *  <TCL_...> : Code de reussite Tcl
 *
 * Remarques :
 *
 *----------------------------------------------------------------------------
*/
void CMap_ControlDefine(CMap_Rec *CMap) {

   int i,i0,i1,ie;
   float delta,r;
   GLubyte cell[4]={ 0,0,0,0 };

   if (!CMap) {
      return;
   }

   /*Find NbPixels which is the last control point*/
   for(i0=CR_MAX-1;i0>=0;i0--) {
      if (memcmp(CMap->Control[i0],cell,4)!=0) {
         break;
      }
   }
   CMap->NbPixels=i0+1;

   /*Find first control point and set all up to it*/
   for(i0=0;i0<CMap->NbPixels;i0++) {
      if (memcmp(CMap->Control[i0],cell,4)!=0) {
         break;
      }
   }
   if (i0<CMap->NbPixels-2) {
      for(i=0;i<i0;i++) {
         memcpy(CMap->Table[i],CMap->Control[i0],4);
      }
   } else {
      i0=0;
   }

   /*Interpolate between control point*/
   ie=i1=i0;
   while(i1<CMap->NbPixels) {
      memcpy(CMap->Table[i0],CMap->Control[i0],4);
      while(memcmp(CMap->Control[++i1],cell,4)==0 && i1<CMap->NbPixels);

      delta=(i1-i0);
      for(i=i0+1;i<i1;i++) {
         r=(float)(i-i0)/delta;
         if (CMap->Interp) {
            CMap->Table[i][0]=ILIN(CMap->Control[i0][0],CMap->Control[i1][0],r);
            CMap->Table[i][1]=ILIN(CMap->Control[i0][1],CMap->Control[i1][1],r);
            CMap->Table[i][2]=ILIN(CMap->Control[i0][2],CMap->Control[i1][2],r);
            CMap->Table[i][3]=ILIN(CMap->Control[i0][3],CMap->Control[i1][3],r);
         } else {
            CMap->Table[i][0]=CMap->Control[i0][0];
            CMap->Table[i][1]=CMap->Control[i0][1];
            CMap->Table[i][2]=CMap->Control[i0][2];
            CMap->Table[i][3]=CMap->Control[i0][3];
         }
      }
      ie=i0;
      i0=i1;
   }

   /*Set from last control point to end*/
   for(i=ie;i<CMap->NbPixels;i++) {
      memcpy(CMap->Table[i],CMap->Control[ie],4);
   }
}

/*----------------------------------------------------------------------------
 * Nom      : <CMap_RatioDefine>
 * Creation : Fevrier 1999 - J.P. Gauthier - CMC/CMOE
 *
 * But      : Modifie le ratio RGBA de la palette.
 *
 * Parametres :
 *  <CMap>    : Palette
 *
 * Retour:
 *
 * Remarques :
 *
 *----------------------------------------------------------------------------
*/
void CMap_RatioDefine(CMap_Rec *CMap){

   float  incr,max,min;
   int    i,mmcell,c;

   if (!CMap) {
      return;
   }

   /*Calculer les limites relatives a la palette*/
   max=CMap->RatioMax/100.0*CMap->NbPixels;
   min=CMap->RatioMin/100.0*CMap->NbPixels;

   /*Determiner l'increment dans la palette selon l'interval*/
   if (max-min<=0) {
      incr=CMap->NbPixels;
   } else {
      incr=CMap->NbPixels/(max-min);
   }

   /*Modification de la palette selon les ratio*/
   CMap->Alpha=0;

   for (c=0;c<4;c++) {
      for (i=0;i<CMap->NbPixels;i++) {

         /*Appliquer le min-max*/
         if (CMap->Curve[i][c]<=min) {
            mmcell=0;
         } else if (CMap->Curve[i][c]>=max) {
            mmcell=CMap->NbPixels-1;
         } else {
            mmcell=(CMap->Curve[i][c]-min)*incr;
         }

         /*Appliquer le ratio*/
         CMap->Color[i][c] = (int)((float)CMap->Table[mmcell][c] * CMap->Ratio[c]/100.0);

         /*Flag de transparence*/
         if (c==3 && CMap->Color[i][c]<255) {
            CMap->Alpha=1;
         }
      }
   }
}

/*----------------------------------------------------------------------------
 * Nom      : <CMap_CurveDefine>
 * Creation : Mars 2007 - J.P. Gauthier - CMC/CMOE
 *
 * But      : Modifie les courbes RGBA de la palette.
 *
 * Parametres :
 *  <CMap>    : Palette
 *
 * Retour:
 *
 * Remarques :
 *
 *----------------------------------------------------------------------------
*/
void CMap_CurveDefine(CMap_Rec *CMap){

   float  n2;
   int    i,it,c;

   if (!CMap) {
      return;
   }

   n2=CMap->NbPixels/2;

   /*Modification de la palette selon les ratio*/
   for (c=0;c<4;c++) {

      if (CMap->Type[c][0]=='\0')
         continue;

      for (i=0;i<CMap->NbPixels;i++) {

         /*Check inversion en X*/
         if (CMap->InvertX[c]) {
            it=CMap->NbPixels-i;
         } else {
            it=i;
         }

         /*Appliquer la courbe d'echantillonage*/
         if (strcmp(CMap->Type[c],"EXPONENTIAL")==0) {
            CMap->Curve[i][c]=pow(it,10)*(CMap->NbPixels/pow(CMap->NbPixels,10));
         } else if (strcmp(CMap->Type[c],"CUBIC")==0) {
            CMap->Curve[i][c]=pow(it,3)*(CMap->NbPixels/pow(CMap->NbPixels,3));
         } else if (strcmp(CMap->Type[c],"SQUARE")==0) {
            CMap->Curve[i][c]=pow(it,2)*(CMap->NbPixels/pow(CMap->NbPixels,2));
         } else if (strcmp(CMap->Type[c],"SQUAREROOT")==0) {
            CMap->Curve[i][c]=sqrt(it)*(CMap->NbPixels/sqrt(CMap->NbPixels));
         } else if (strcmp(CMap->Type[c],"CUBICROOT")==0) {
            CMap->Curve[i][c]=pow(it,0.333)*(CMap->NbPixels/pow(CMap->NbPixels,0.333));
         } else if (strcmp(CMap->Type[c],"LOGARITHMIC")==0) {
            CMap->Curve[i][c]=it==0?0:log10(it)*(CMap->NbPixels/log10(CMap->NbPixels));
         } else if (strcmp(CMap->Type[c],"QUADRATIC")==0) {
            CMap->Curve[i][c]=((it-n2)*(it-n2)+(it-n2))*(CMap->NbPixels/((n2*n2)+n2));
         } else if (strcmp(CMap->Type[c],"STEP16")==0) {
            CMap->Curve[i][c]=it/16*(CMap->NbPixels/16);
         } else if (strcmp(CMap->Type[c],"STEP32")==0) {
            CMap->Curve[i][c]=it/32*(CMap->NbPixels/8);
         } else if (strcmp(CMap->Type[c],"STEP64")==0) {
            CMap->Curve[i][c]=it/64*(CMap->NbPixels/4);
         } else if (strcmp(CMap->Type[c],"STEP128")==0) {
            CMap->Curve[i][c]=it/128*(CMap->NbPixels/2);
         } else {
            CMap->Curve[i][c]=it;
         }

         /* Check inversion en Y*/
         if (CMap->InvertY[c])
            CMap->Curve[i][c]=CMap->NbPixels-CMap->Curve[i][c];
      }
   }
}

/*----------------------------------------------------------------------------
 * Nom      : <CMap_SelImage>
 * Creation : Decembre 2002 - J.P. Gauthier - CMC/CMOE
 *
 * But      : Emplir une image Tcl avec la palette Alpha.
 *
 * Parametres :
 *  <Interp>  : Interpreteur TCL
 *  <Img>     : Image Tcl
 *
 * Retour:
 *  <TCL_...> : Code de reussite Tcl
 *
 * Remarques :
 *
 *----------------------------------------------------------------------------
*/
int CMap_SelImage(Tcl_Interp *Interp,char Mode,char *Img,double Val){

   double sinc,vinc,r,g,b;
   int x,y,idx=0;

   Tk_PhotoImageBlock data;
   Tk_PhotoHandle     handle;

   /*Recuperer le handle de l'image specifie*/

   handle=Tk_FindPhoto(Interp,Img);

   /*Definire les parametres du bock de donnees*/

   Tk_PhotoGetSize(handle,&data.width,&data.height);

   data.pitch=data.width*4;
   data.pixelSize=4;
   data.offset[0]=0;
   data.offset[1]=1;
   data.offset[2]=2;
   data.offset[3]=3;
   data.pixelPtr=(unsigned char*)malloc(data.width*data.height*4*sizeof(unsigned char));

   /*Creer l'image de la palette*/

   switch(Mode) {
      case 's': /*Saturation-Variation image*/
         sinc=(100.0/data.height);
         vinc=(100.0/data.width);

         for(y=0;y<data.height;y++) {
            for(x=0;x<data.width;x++) {
               CMap_HSV2RGB(&r,&g,&b,Val,y*sinc,x*vinc);
               data.pixelPtr[idx++]=(int)r;
               data.pixelPtr[idx++]=(int)g;
               data.pixelPtr[idx++]=(int)b;
               data.pixelPtr[idx++]=255;
            }
         }
         break;
      case 'h': /*Hue image*/
         sinc=360.0/data.height;

         for(y=0;y<data.height;y++) {
            for(x=0;x<data.width;x++) {
               CMap_HSV2RGB(&r,&g,&b,sinc*y,99,99);
               data.pixelPtr[idx++]=(int)r;
               data.pixelPtr[idx++]=(int)g;
               data.pixelPtr[idx++]=(int)b;
               data.pixelPtr[idx++]=255;
            }
         }
         break;
      case 'a': /*Alpha image*/
         sinc=255.0/data.height;

         for(y=0;y<data.height;y++) {
            for(x=0;x<data.width;x++) {
               data.pixelPtr[idx++]=sinc*y;
               data.pixelPtr[idx++]=sinc*y;
               data.pixelPtr[idx++]=sinc*y;
               data.pixelPtr[idx++]=255;
            }
         }
         break;
      default:
         Tcl_AppendResult(Interp,"CMap_SelImage: Invalide image type, must be\" -h -sv -a \"",(char *)NULL);
         return(TCL_ERROR);
   }

   /*Envoyer le data dans l'image Tk*/

   Tk_PhotoPutBlock(Interp,handle,&data,0,0,data.width,data.height,TK_PHOTO_COMPOSITE_SET);
   free(data.pixelPtr);

   return(TCL_OK);
}

/*----------------------------------------------------------------------------
 * Nom      : <CMap_RGB2HSV>
 * Creation : Juin 2001 - J.P. Gauthier - CMC/CMOE
 *
 * But      : Convertir des valeurs RGB en HSV.
 *
 * Parametres :
 *  <R>       : Valeur de rouge
 *  <G>       : Valeur de vert
 *  <B>       : Valeur de bleu
 *  <H>       : Hue correspondant
 *  <S>       : Saturation correspondant
 *  <V>       : Luminosite correspondante
 *
 * Retour:
 *  <>   :
 *
 * Remarques :
 *
 *----------------------------------------------------------------------------
 */
void CMap_RGB2HSV(double R,double G,double B,double *H,double *S,double *V){

   double min,max,del;

   R/=256.0;
   G/=256.0;
   B/=256.0;

   max=R>G?R:G;
   max=max>B?max:B;

   min=R<G?R:G;
   min=min<B?min:B;

   *V=max*100.0;
   *S=((max!=0.0)?((max-min)/max):0.0)*100.0;

   if (*S==0.0) {
      *H=0.0;
   } else {                  /*Evaluer les cas chromatique*/
      del=max-min;

      if (R==max)            /*Couleur entre jaune et magenta*/
         *H=(G-B)/del;
      else if (G==max)       /*Couleur entre cyan et jaune*/
         *H=2.0+(B-R)/del;
      else if (B==max)       /*Couleur entre magenta et cyan*/
         *H=4.0+(R-G)/del;

      *H*=60.0;              /*Convertir en degree*/

      if (*H<0.0)            /*Non negatif*/
         *H+=360.0;
   }
}

/*----------------------------------------------------------------------------
 * Nom      : <CMap_HSV2RGB>
 * Creation : Juin 2001 - J.P. Gauthier - CMC/CMOE
 *
 * But      : Convertir des valeurs HSV en RGB.
 *
 * Parametres :
 *  <R>       : Valeur de rouge correspondant
 *  <G>       : Valeur de vert correspondant
 *  <B>       : Valeur de bleu correspondant
 *  <H>       : Valeur de Hue
 *  <S>       : Valeur de Saturation
 *  <V>       : Valeur de Luminosite
 *
 * Retour:
 *  <>   :
 *
 * Remarques :
 *
 *----------------------------------------------------------------------------
 */
void CMap_HSV2RGB(double *R,double *G,double *B,double H,double S,double V){

   S/=100.0;
   V/=100.0;

   if (S==0.0) {             /*Noir et blanc*/
      *R=V;
      *G=V;
      *B=V;
   } else {
      double f,p,q,t;
      int    i;

      if (H==360.0)
          H=0.0;

      H/=60.0;
      i=floor(H);
      f=H-i;
      p=V*(1.0-S);
      q=V*(1.0-(S*f));
      t=V*(1.0-(S*(1.0-f)));

      switch(i) {            /*Evaluer les cas chromatique*/
         case 0:*R=V;*G=t;*B=p;break;
         case 1:*R=q;*G=V;*B=p;break;
         case 2:*R=p;*G=V;*B=t;break;
         case 3:*R=p;*G=q;*B=V;break;
         case 4:*R=t;*G=p;*B=V;break;
         case 5:*R=V;*G=p;*B=q;break;
      }
   }
   *R*=256.0;
   *G*=256.0;
   *B*=256.0;
}

/*----------------------------------------------------------------------------
 * Nom      : <CMap_PostscriptColor>
 * Creation : Fevrier 2002 - J.P. Gauthier - CMC/CMOE
 *
 * But      : Ecrire le PostScript de la couleur a l'index specifie dans la palette
 *            et retour a travers l'interpreteur TCL.
 *
 * Parametres :
 *  <Interp>  : Interpreteur TCL
 *  <CMap>    : Palette
 *  <Index>   : Index de la couleur dans la palette
 *
 * Retour:
 *  <TCL_...> : Code de reussite Tcl
 *
 * Remarques :
 *
 *----------------------------------------------------------------------------
*/
int CMap_PostscriptColor(Tcl_Interp *Interp,CMap_Rec *CMap,int Index){

   char buf[64];

   if (Index>0 && Index<CMap->NbPixels) {
      sprintf(buf,"%0.3f %0.3f %0.3f setrgbcolor AdjustColor\n",
         (float)CMap->Color[Index][0]/255,
         (float)CMap->Color[Index][1]/255,
         (float)CMap->Color[Index][2]/255);
      Tcl_AppendResult(Interp,buf,(char*)NULL);
      return(TCL_OK);
   } else {
     return(TCL_ERROR);
   }
}
