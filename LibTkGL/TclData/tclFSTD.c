/*=============================================================================
 * Environnement Canada
 * Centre Meteorologique Canadian
 * 2100 Trans-Canadienne
 * Dorval, Quebec
 *
 * Projet    : Librairie Tcl de fichiers standards.
 * Fichier   : tclFSTD.c
 * Creation  : Aout 1998 - J.P. Gauthier - CMC/CMOE
 *
 * Description: Utilisation des fichiers standards RPN dans des scripts Tcl et
 *              dans les projections.
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

#include "tclFSTD.h"
#include "tclObs.h"
#include "tclOGR.h"
#include "tclGDAL.h"
#include "tcl3DModel.h"
#include <sys/timeb.h>

/*Table contenant la liste des champs en memoire*/
static Tcl_HashTable FSTD_FileTable;
static int           FSTDInit=0;

static TFSTDVector FSTDVectorTable[256];
static int         FSTDVectorTableSize=0;

extern int ZREF_IP1MODE;

static int FSTD_FieldCmd(ClientData clientData,Tcl_Interp *Interp,int Objc,Tcl_Obj *CONST Objv[]);
static int FSTD_FileCmd(ClientData clientData,Tcl_Interp *Interp,int Objc,Tcl_Obj *CONST Objv[]);
static int FSTD_GridCmd(ClientData clientData,Tcl_Interp *Interp,int Objc,Tcl_Obj *CONST Objv[]);
static int FSTD_StampCmd(ClientData clientData,Tcl_Interp *Interp,int Objc,Tcl_Obj *CONST Objv[]);

TFSTDVector *FSTD_VectorTableCheck(char *Var,int *Idx) {

   register int i;

   for(i=0;i<FSTDVectorTableSize;i++) {
      if (FSTDVectorTable[i].UU && strcmp(Var,FSTDVectorTable[i].UU)==0) {
         if (Idx) *Idx=0;
         return(&FSTDVectorTable[i]);
      }
      if (FSTDVectorTable[i].VV && strcmp(Var,FSTDVectorTable[i].VV)==0) {
         if (Idx) *Idx=1;
         return(&FSTDVectorTable[i]);
      }
      if (FSTDVectorTable[i].WW && strcmp(Var,FSTDVectorTable[i].WW)==0) {
         if (Idx) *Idx=2;
         return(&FSTDVectorTable[i]);
      }
   }
   return(NULL);
}

/*----------------------------------------------------------------------------
 * Nom      : <FSTD_GridCmd>
 * Creation : Octobre 1999 - J.P. Gauthier - CMC/CMOE
 *
 * But      : Appel des commandes relies aux grilles de fichiers standards.
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
static int FSTD_GridCmd (ClientData clientData,Tcl_Interp *Interp,int Objc,Tcl_Obj *CONST Objv[]){

   Tcl_Obj    *obj;
   TData      *field,*p0;
   FSTD_Head  *head;
   float       dlat,dlon,dd60,dgrw,x,y,level=0.0;
   int         ip1,n,kind;
   char        buf[50];
   double      tmp;
   int         idx,idxk,i,j,k;

   double dxg1,dxg2,dxg3,dxg4;
   float  xg1,xg2,xg3,xg4;
   int    ig1,ig2,ig3,ig4;

   static CONST char *type[] = { "MASL","SIGMA","PRESSURE","UNDEFINED","MAGL","HYBRID","THETA","ETA","GALCHEN",NULL };
   static CONST char *sopt[] = { "xyfll","llfxy","convip","cxgaig","cigaxg","mscale","zgrid","zfilter","pressure",NULL };
   enum                opt { XYFLL,LLFXY,CONVIP,CXGAIG,CIGAXG,MSCALE,ZGRID,ZFILTER,PRESSURE };

   Tcl_ResetResult(Interp);

   if (Objc<2) {
      Tcl_WrongNumArgs(Interp,1,Objv,"command ?arg arg ...?");
      return(TCL_ERROR);
   }

   if (Tcl_GetIndexFromObj(Interp,Objv[1],sopt,"command",0,&idx)!=TCL_OK) {
      return(TCL_ERROR);
   }

   switch ((enum opt)idx) {
      case XYFLL:
         if(Objc!=7) {
            Tcl_WrongNumArgs(Interp,2,Objv,"dlat dlon dd60 dgrw nhem");
            return(TCL_ERROR);
         }
         Tcl_GetDoubleFromObj(Interp,Objv[2],&tmp);dlat=tmp;
         Tcl_GetDoubleFromObj(Interp,Objv[3],&tmp);dlon=tmp;
         Tcl_GetDoubleFromObj(Interp,Objv[4],&tmp);dd60=tmp;
         Tcl_GetDoubleFromObj(Interp,Objv[5],&tmp);dgrw=tmp;
         Tcl_GetIntFromObj(Interp,Objv[6],&n);
#ifdef LNK_FSTD
         f77name(xyfll)(&x,&y,&dlat,&dlon,&dd60,&dgrw,&n);
         obj=Tcl_NewListObj(0,NULL);
         Tcl_ListObjAppendElement(Interp,obj,Tcl_NewDoubleObj(x));
         Tcl_ListObjAppendElement(Interp,obj,Tcl_NewDoubleObj(y));
         Tcl_SetObjResult(Interp,obj);
#endif
         break;

      case LLFXY:
         if(Objc!=7) {
            Tcl_WrongNumArgs(Interp,2,Objv,"x y dd60 dgrw nhem");
            return(TCL_ERROR);
         }
         Tcl_GetDoubleFromObj(Interp,Objv[2],&tmp);x=tmp;
         Tcl_GetDoubleFromObj(Interp,Objv[3],&tmp);y=tmp;
         Tcl_GetDoubleFromObj(Interp,Objv[4],&tmp);dd60=tmp;
         Tcl_GetDoubleFromObj(Interp,Objv[5],&tmp);dgrw=tmp;
         Tcl_GetIntFromObj(Interp,Objv[6],&n);
#ifdef LNK_FSTD
         f77name(llfxy)(&dlat,&dlon,&x,&y,&dd60,&dgrw,&n);
         obj=Tcl_NewListObj(0,NULL);
         Tcl_ListObjAppendElement(Interp,obj,Tcl_NewDoubleObj(dlat));
         Tcl_ListObjAppendElement(Interp,obj,Tcl_NewDoubleObj(dlon));
         Tcl_SetObjResult(Interp,obj);
#endif
         break;

      case MSCALE:
         if(Objc!=3) {
            Tcl_WrongNumArgs(Interp,2,Objv,"field");
            return(TCL_ERROR);
         }
         if (!(field=Data_Get(Tcl_GetString(Objv[2])))) {
            Tcl_AppendResult(Interp,"invalid field :",Tcl_GetString(Objv[2]),(char*)NULL);
            return(TCL_ERROR);
         }
         head=((FSTD_Head*)(field->Head));
#ifdef LNK_FSTD
         f77name(cigaxg)(field->Ref->Grid,&xg1,&xg2,&xg3,&xg4,&head->IG1,&head->IG2,&head->IG3,&head->IG4);
         f77name(mscale)((float*)field->Def->Data[0],&xg3,&xg1,&xg2,&field->Def->NI,&field->Def->NJ);
#endif
         break;

      case PRESSURE:

         if(Objc!=4) {
            Tcl_WrongNumArgs(Interp,2,Objv,"field p0(pa)");
            return(TCL_ERROR);
         }

         if (!(field=Data_Get(Tcl_GetString(Objv[2])))) {
            Tcl_AppendResult(Interp,"invalid field :",Tcl_GetString(Objv[2]),(char*)NULL);
            return(TCL_ERROR);
         }
         if (!(p0=Data_Get(Tcl_GetString(Objv[3])))) {
            Tcl_AppendResult(Interp,"invalid pressure field :",Tcl_GetString(Objv[3]),(char*)NULL);
            return(TCL_ERROR);
         }

         if (field->Ref->ZRef.PTop==0.0 && field->Ref->ZRef.PRef==0.0) {
            if (!FSTD_DecodeRPNLevelParams(field)) {
               Tcl_AppendResult(Interp,"Could not find level paramaters from file",(char*)NULL);
               return(TCL_ERROR);
            }
         }
         for(k=0;k<field->Def->NK;k++) {
            idxk=k*FSIZE2D(field->Def);
            idx=0;
            for(j=0;j<field->Def->NJ;j++) {
               for(i=0;i<field->Def->NI;i++) {
                  idx++;
                  Def_Get(p0->Def,0,idx,tmp);
                  tmp=ZRef_K2Pressure(&field->Ref->ZRef,tmp,k);
                  Def_Set(field->Def,0,idxk+idx,tmp);
               }
            }
         }
         break;

      case CXGAIG:
         if(Objc!=7) {
            Tcl_WrongNumArgs(Interp,2,Objv,"grtyp xg1 xg2 xg3 xg4");
            return(TCL_ERROR);
         }
         Tcl_GetDoubleFromObj(Interp,Objv[3],&dxg1);
         Tcl_GetDoubleFromObj(Interp,Objv[4],&dxg2);
         Tcl_GetDoubleFromObj(Interp,Objv[5],&dxg3);
         Tcl_GetDoubleFromObj(Interp,Objv[6],&dxg4);
         xg1=dxg1;xg2=dxg2;xg3=dxg3;xg4=dxg4;
#ifdef LNK_FSTD
         f77name(cxgaig)(Tcl_GetString(Objv[2]),&ig1,&ig2,&ig3,&ig4,&xg1,&xg2,&xg3,&xg4);
         obj=Tcl_NewListObj(0,NULL);
         Tcl_ListObjAppendElement(Interp,obj,Tcl_NewIntObj(ig1));
         Tcl_ListObjAppendElement(Interp,obj,Tcl_NewIntObj(ig2));
         Tcl_ListObjAppendElement(Interp,obj,Tcl_NewIntObj(ig3));
         Tcl_ListObjAppendElement(Interp,obj,Tcl_NewIntObj(ig4));
         Tcl_SetObjResult(Interp,obj);
#endif
         break;

      case CIGAXG:
         if(Objc!=7) {
            Tcl_WrongNumArgs(Interp,2,Objv,"grtyp ig1 ig2 ig3 ig4");
            return(TCL_ERROR);
         }
         Tcl_GetIntFromObj(Interp,Objv[3],&ig1);
         Tcl_GetIntFromObj(Interp,Objv[4],&ig2);
         Tcl_GetIntFromObj(Interp,Objv[5],&ig3);
         Tcl_GetIntFromObj(Interp,Objv[6],&ig4);

#ifdef LNK_FSTD
         f77name(cigaxg)(Tcl_GetString(Objv[2]),&xg1,&xg2,&xg3,&xg4,&ig1,&ig2,&ig3,&ig4);
         obj=Tcl_NewListObj(0,NULL);
         Tcl_ListObjAppendElement(Interp,obj,Tcl_NewDoubleObj(xg1));
         Tcl_ListObjAppendElement(Interp,obj,Tcl_NewDoubleObj(xg2));
         Tcl_ListObjAppendElement(Interp,obj,Tcl_NewDoubleObj(xg3));
         Tcl_ListObjAppendElement(Interp,obj,Tcl_NewDoubleObj(xg4));
         Tcl_SetObjResult(Interp,obj);
#endif
         break;

      case CONVIP:
         if(Objc!=3 && Objc!=4) {
            Tcl_WrongNumArgs(Interp,2,Objv,"IP1 | Level Type");
            return(TCL_ERROR);
         }
         if(Objc==3) {
            Tcl_GetIntFromObj(Interp,Objv[2],&ip1);
            level=ZRef_IP2Level(ip1,&kind);
            switch(kind) {
               case LVL_MASL  : sprintf(buf,"%.1f  m (Meter above sea level)",level); break;
               case LVL_SIGMA : sprintf(buf,"%.4f sg (Sigma)",level); break;
               case LVL_PRES  : sprintf(buf,"%.1f mb (Pressure)",level); break;
               case LVL_UNDEF : sprintf(buf,"%.1f  - (Undefined)",level); break;
               case LVL_MAGL  : sprintf(buf,"%.1f  m (Meter above groud level)",level); break;
               case LVL_HYBRID: sprintf(buf,"%.6f hy (Hybrid)",level); break;
               case LVL_THETA : sprintf(buf,"%.4f th (Theta)",level); break;
            }
            Tcl_SetObjResult(Interp,Tcl_NewStringObj(buf,-1));
         } else {
            Tcl_GetDoubleFromObj(Interp,Objv[2],&tmp);

            if (Tcl_GetIndexFromObj(Interp,Objv[3],type,"type",0,&n)!=TCL_OK) {
               Tcl_AppendResult(Interp,"invalid level type, must be [ MASL SIGMA PRESSURE UNDEFINED MAGL HYBRID THETA ETA GALCHEN ]",(char*)NULL);
               return(TCL_ERROR);
            }
            ip1=ZRef_Level2IP(tmp,n);
            Tcl_SetObjResult(Interp,Tcl_NewIntObj(ip1));
         }
         break;

      case ZGRID:
         if(Objc!=5) {
            Tcl_WrongNumArgs(Interp,2,Objv,"tic tac settings");
            return(TCL_ERROR);
         }
         return (FSTD_ZGrid(Interp,Objv[2],Objv[3],Objv[4]));
         break;

      case ZFILTER:
         if(Objc!=4) {
            Tcl_WrongNumArgs(Interp,2,Objv,"field settings");
            return(TCL_ERROR);
         }
         return(FSTD_ZFilterTopo(Interp,Data_Get(Tcl_GetString(Objv[2])),Objv[3]));
         break;
   }
   return(TCL_OK);
}

/*----------------------------------------------------------------------------
 * Nom      : <FSTD_FieldCmd>
 * Creation : Aout 1998 - J.P. Gauthier - CMC/CMOE
 *
 * But      : Appel des commandes relies aux fichiers standards.
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

static int FSTD_FieldCmd (ClientData clientData,Tcl_Interp *Interp,int Objc,Tcl_Obj *CONST Objv[]){

   int           id,datev,ip1,ip2,ip3,npack,rewrite,ni,nj,nk,key,n,k,i,compress=0;
   unsigned long dk;
   int           i0=-1,j0=-1,i1=-1,j1=-1,npos,ok;
   long          time;
   double        tmpd,*table;
   double        c0,c1,a,x;

   TObs        *obs;
   TMetObs     *metobs;
   OGR_Layer   *layer;
   GDAL_Band   *band,*bandt;
   T3DModel    *model;
   Tcl_Obj     *obj;
   TData       *field0,*field1,*fieldt;
   TDataSpec   *spec;
   TFSTDVector *uvw;
   Vect3d      *pos;
   char        *field,imode,itype;

   int         idx;
   static CONST char *moderas[] = { "NEAREST","LINEAR","CUBIC","NORMALIZED_CONSERVATIVE","CONSERVATIVE","MAXIMUM","MINIMUM","SUM","AVERAGE","AVERAGE_VARIANCE","AVERAGE_SQUARE","NORMALIZED_COUNT","COUNT","LENGTH_CONSERVATIVE","LENGTH_ALIASED","LENGTH_NORMALIZED_CONSERVATIVE","NOP","ACCUM","BUFFER",NULL };
   static CONST char *modeogr[] = { "FAST","WITHIN","INTERSECT","CONSERVATIVE","NORMALIZED_CONSERVATIVE","ALIASED","POINT_CONSERVATIVE","LENGTH_CONSERVATIVE","LENGTH_NORMALIZED_CONSERVATIVE","LENGTH_ALIASED",NULL };
   static CONST char *type[] = { "MASL","SIGMA","PRESSURE","UNDEFINED","MAGL","HYBRID","THETA","ETA","GALCHEN",NULL };
   static CONST char *sopt[] = { "ip1mode","vector","read","readcube","head","find","write","export","create","vertical","gridinterp","verticalinterp",
                                 "timeinterp","define","sort",NULL };
   enum                opt { IP1MODE,VECTOR,READ,READCUBE,HEAD,FIND,WRITE,EXPORT,CREATE,VERTICAL,GRIDINTERP,VERTICALINTERP,TIMEINTERP,DEFINE,SORT };

   Tcl_ResetResult(Interp);

   if (Objc<2) {
      Tcl_WrongNumArgs(Interp,1,Objv,"command ?arg arg ...?");
      return(TCL_ERROR);
   }

   if (Tcl_GetIndexFromObj(Interp,Objv[1],sopt,"command",0,&idx)!=TCL_OK) {
      return(Data_FieldCmd(clientData,Interp,Objc,Objv));
   }

   switch ((enum opt)idx) {
      case VECTOR:
         if (Objc!=3) {
            Tcl_WrongNumArgs(Interp,2,Objv,"{ U [V] [W] [WFactor] }");
            return(TCL_ERROR);
         } else {
            Tcl_ListObjLength(Interp,Objv[2],&n);
            if (n) {
               Tcl_ListObjIndex(Interp,Objv[2],0,&obj);
               if (!(uvw=FSTD_VectorTableCheck(Tcl_GetString(obj),NULL))) {
                  uvw=&FSTDVectorTable[FSTDVectorTableSize++];
               }
               if (uvw->UU) free(uvw->UU);
               if (uvw->VV) free(uvw->VV);
               if (uvw->WW) free(uvw->WW);
               uvw->UU=strdup(Tcl_GetString(obj));
               uvw->VV=uvw->WW=NULL;
               uvw->WWFactor=0.0;
               Tcl_ListObjIndex(Interp,Objv[2],1,&obj);
               if (n>1) {
                  uvw->VV=strdup(Tcl_GetString(obj));
                  if (n>2) {
                     Tcl_ListObjIndex(Interp,Objv[2],2,&obj);
                     uvw->WW=strdup(Tcl_GetString(obj));
                     if (n>3) {
                        Tcl_ListObjIndex(Interp,Objv[2],3,&obj);
                        Tcl_GetDoubleFromObj(Interp,obj,&uvw->WWFactor);
                     }
                  }
               }
            }
         }
         break;

      case IP1MODE:
         if (Objc==2) {
            if (ZREF_IP1MODE==2) {
               Tcl_SetObjResult(Interp,Tcl_NewStringObj("NEW",-1));
            } else {
               Tcl_SetObjResult(Interp,Tcl_NewStringObj("OLD",-1));
            }
         } else {
            if (strcmp(Tcl_GetString(Objv[2]),"NEW")==0) {
               ZREF_IP1MODE=2;
            } else if  (strcmp(Tcl_GetString(Objv[2]),"OLD")==0) {
               ZREF_IP1MODE=3;
            } else {
               Tcl_AppendResult(Interp,"Wrong mode, must be NEW or OLD",(char*)NULL);
               return(TCL_ERROR);
            }
         }
         break;

      case READ:
         if(Objc!=11 && Objc!=5) {
            Tcl_WrongNumArgs(Interp,2,Objv,"fld id { datev eticket ip1 ip2 ip3 typvar nomvar } | Index");
            return TCL_ERROR;
         }
         if (Objc==11) {
            /*Check the level description*/
            if (Tcl_GetIntFromObj(Interp,Objv[6],&ip1)==TCL_ERROR) {
               Tcl_ResetResult(Interp);
               Tcl_ListObjLength(Interp,Objv[6],&n);
               if (n<2) {
                  Tcl_AppendResult(Interp,"invalid level description, must be IP1 or { level leveltype )",(char*)NULL);
                  return(TCL_ERROR);
               }
               Tcl_ListObjIndex(Interp,Objv[6],0,&obj);
               Tcl_GetDoubleFromObj(Interp,obj,&tmpd);

               Tcl_ListObjIndex(Interp,Objv[6],1,&obj);
               if (Tcl_GetIndexFromObj(Interp,obj,type,"type",0,&n)!=TCL_OK) {
                  Tcl_AppendResult(Interp,"invalid level type, must be [ MASL SIGMA PRESSURE UNDEFINED MAGL HYBRID THETA ETA GALCHEN ]",(char*)NULL);
                  return(TCL_ERROR);
               }
               ip1=ZRef_Level2IP(tmpd,n);
            }

            Tcl_GetIntFromObj(Interp,Objv[7],&ip2);
            Tcl_GetIntFromObj(Interp,Objv[8],&ip3);

            /*Get the bogus date*/
            TclY_Get0IntFromObj(Interp,Objv[4],&datev);

            return FSTD_FieldRead(Interp,Tcl_GetString(Objv[2]),Tcl_GetString(Objv[3]),-1,datev,Tcl_GetString(Objv[5]),ip1,ip2,ip3,Tcl_GetString(Objv[9]),Tcl_GetString(Objv[10]));
         } else {
            Tcl_GetIntFromObj(Interp,Objv[4],&key);
            return FSTD_FieldRead(Interp,Tcl_GetString(Objv[2]),Tcl_GetString(Objv[3]),key,-1,NULL,-1,-1,-1,NULL,NULL);
         }
         break;

      case READCUBE:
         if(Objc<3 || Objc>6) {
            Tcl_WrongNumArgs(Interp,2,Objv,"id [invert] [level from] [level to] | [levels]");
            return(TCL_ERROR);
         }
         field0=Data_Get(Tcl_GetString(Objv[2]));
         if (!field0) {
            Tcl_AppendResult(Interp,"invalid field",(char*)NULL);
            return(TCL_ERROR);
         }
         n=0;
         c0=c1=0.0;
         obj=NULL;
         if (Objc>3) {
            Tcl_GetBooleanFromObj(Interp,Objv[3],&n);
         }
         if (Objc>4) {
            Tcl_ListObjLength(Interp,Objv[4],&npos);
            if (npos==1) {
               Tcl_GetDoubleFromObj(Interp,Objv[4],&c0);
            } else {
               obj=Objv[4];
            }
         }
         if (Objc>5) {
            Tcl_GetDoubleFromObj(Interp,Objv[5],&c1);
         }
         if (field0->ReadCube) {
            if (!field0->ReadCube(Interp,field0,n,c0,c1,obj)) {
               return(TCL_ERROR);
            }
         }
         return(TCL_OK);
         break;

      case HEAD:
         if(Objc!=4) {
            Tcl_WrongNumArgs(Interp,2,Objv,"id index");
            return(TCL_ERROR);
         }
         Tcl_GetIntFromObj(Interp,Objv[3],&key);
         return(FSTD_FieldReadHead(Interp,Tcl_GetString(Objv[2]),key));
         break;

      case FIND:
         if(Objc!=10 && Objc!=11) {
            Tcl_WrongNumArgs(Interp,2,Objv,"id datev eticket ip1 ip2 ip3 typvar nomvar [max]");
            return(TCL_ERROR);
         }
         TclY_Get0IntFromObj(Interp,Objv[3],&datev);

         if (Tcl_GetIntFromObj(Interp,Objv[5],&ip1)==TCL_ERROR) {
            Tcl_ResetResult(Interp);
            Tcl_ListObjLength(Interp,Objv[5],&n);
            if (n<2) {
               Tcl_AppendResult(Interp,"invalid level description, must be IP1 or { level leveltype )",(char*)NULL);
               return(TCL_ERROR);
            }
            Tcl_ListObjIndex(Interp,Objv[5],0,&obj);
            Tcl_GetDoubleFromObj(Interp,obj,&tmpd);

            Tcl_ListObjIndex(Interp,Objv[5],1,&obj);
            if (Tcl_GetIndexFromObj(Interp,obj,type,"type",0,&n)!=TCL_OK) {
               Tcl_AppendResult(Interp,"invalid level type, must be [ MASL SIGMA PRESSURE UNDEFINED MAGL HYBRID THETA ETA GALCHEN ]",(char*)NULL);
               return(TCL_ERROR);
            }
            ip1=ZRef_Level2IP(tmpd,n);
         }

         Tcl_GetIntFromObj(Interp,Objv[6],&ip2);
         Tcl_GetIntFromObj(Interp,Objv[7],&ip3);
         n=1000;
         if (Objc==11) {
            Tcl_GetIntFromObj(Interp,Objv[10],&n);
         }
         return(FSTD_FieldFind(Interp,Tcl_GetString(Objv[2]),n,datev,Tcl_GetString(Objv[4]),ip1,ip2,ip3,Tcl_GetString(Objv[8]),Tcl_GetString(Objv[9])));
         break;

      case WRITE:
         if(Objc!=6 && Objc!=7) {
            Tcl_WrongNumArgs(Interp,2,Objv,"fld id npack rewrite [compress]");
            return(TCL_ERROR);
         }
         Tcl_GetIntFromObj(Interp,Objv[4],&npack);
         Tcl_GetBooleanFromObj(Interp,Objv[5],&rewrite);
         if (Objc==7) {
            Tcl_GetBooleanFromObj(Interp,Objv[6],&compress);
         }
         if (!(field0=Data_Get(Tcl_GetString(Objv[2])))) {
            Tcl_AppendResult(Interp,"FSTD_FieldCmd: Invalid field (",Tcl_GetString(Objv[2]),")",(char*)NULL);
            return(TCL_ERROR);
         }
         return(FSTD_FieldWrite(Interp,Tcl_GetString(Objv[3]),field0,npack,rewrite,compress));
         break;

      case EXPORT:
         if (strcmp(Tcl_GetString(Objv[2]),"HIRLAM")==0) {
            if (Objc!=10 && Objc!=14) {
               Tcl_WrongNumArgs(Interp,2,Objv,"HIRLAM fld filename desc info factor (mode [INT|EXP]) (grid [boolean]) ?I0 J0 I1 J1?");
               return(TCL_ERROR);
            } else {
               if (Objc==14) {
                   Tcl_GetIntFromObj(Interp,Objv[10],&i0);
                   Tcl_GetIntFromObj(Interp,Objv[11],&j0);
                   Tcl_GetIntFromObj(Interp,Objv[12],&i1);
                   Tcl_GetIntFromObj(Interp,Objv[13],&j1);
               }
               Tcl_GetDoubleFromObj(Interp,Objv[7],&tmpd);
               return(HIRLAM_Export(Interp,Data_Get(Tcl_GetString(Objv[3])),Tcl_GetString(Objv[5]),Tcl_GetString(Objv[6]),Tcl_GetString(Objv[4]),tmpd,Tcl_GetString(Objv[8]),Tcl_GetString(Objv[9]),i0,j0,i1,j1));
            }
         } else if (strcmp(Tcl_GetString(Objv[2]),"WIX")==0) {
            if (Objc!=5 && Objc!=9) {
               Tcl_WrongNumArgs(Interp,2,Objv,"WIX { FLDS } filename ?I0 J0 I1 J1?");
               return(TCL_ERROR);
            } else {
               return(WIX_Export(Interp,Objv[3],Tcl_GetString(Objv[4]),i0,j0,i1,j1));
            }
         } else {
            Tcl_AppendResult(Interp,"FSTD_FieldCmd: wrong export type, must be HIRLAM or WIX",(char *)NULL);
            return(TCL_ERROR);
         }
         break;

      case CREATE:
         if(Objc!=6 && Objc!=7) {
            Tcl_WrongNumArgs(Interp,2,Objv,"fld ni nj nk [Binary Byte UInt16 Int16 Uint32 Int32 Float32 Float64]");
            return(TCL_ERROR);
         }
         ni=nj=nk=-1;

         Tcl_GetIntFromObj(Interp,Objv[3],&ni);
         Tcl_GetIntFromObj(Interp,Objv[4],&nj);
         Tcl_GetIntFromObj(Interp,Objv[5],&nk);

         if (ni<=0 || nj<=0 || nk<=0) {
             Tcl_AppendResult(Interp,"FSTD_FieldCmd: wrong dimensions",(char *)NULL);
             return(TCL_ERROR);
         }

         if (Objc==7) {
            if (strcmp(Tcl_GetString(Objv[6]),"Binary")==0) {
               npack=1;
            } else if (strcmp(Tcl_GetString(Objv[6]),"UByte")==0) {
               npack=2;
            } else if (strcmp(Tcl_GetString(Objv[6]),"Byte")==0) {
               npack=3;
            } else if (strcmp(Tcl_GetString(Objv[6]),"UInt16")==0) {
               npack=4;
            } else if (strcmp(Tcl_GetString(Objv[6]),"Int16")==0) {
               npack=5;
            } else if (strcmp(Tcl_GetString(Objv[6]),"UInt32")==0) {
               npack=6;
            } else if (strcmp(Tcl_GetString(Objv[6]),"Int32")==0) {
               npack=7;
            } else if (strcmp(Tcl_GetString(Objv[6]),"Float32")==0) {
               npack=10;
            } else if (strcmp(Tcl_GetString(Objv[6]),"Float64")==0) {
               npack=11;
            } else {
               Tcl_AppendResult(Interp,"FSTD_FieldCmd: Invalid data type must be Binary Byte UInt16 Int16 Uint32 Int32 Float32 Float64",(char*)NULL);
               return(TCL_ERROR);
            }
         } else {
            npack=10;
         }
         if (!FSTD_FieldCreate(Interp,Tcl_GetString(Objv[2]),ni,nj,nk,npack))
            return(TCL_ERROR);
         else
            return(TCL_OK);
         break;

      case VERTICAL:
         if(Objc!=5) {
            Tcl_WrongNumArgs(Interp,2,Objv,"fld { fldfrom1 fldfrom2 ... } { lat lon lat lon ... ... }");
            return TCL_ERROR;
         } else {
            Tcl_Obj *obj;
            TData  **fields=NULL;
            double  *lat,*lon;
            int      nbc,nbf,nc,ncc;

            Tcl_ListObjLength(Interp,Objv[4],&nbc);
            if (nbc%2!=0 || nbc==0) {
               Tcl_AppendResult(Interp,"FSTD_FieldCmd: Invalid number of coordinates",(char*)NULL);
               return(TCL_ERROR);
            }

            lat=(double*)malloc(nbc/2*sizeof(double));
            lon=(double*)malloc(nbc/2*sizeof(double));
            for (nc=0,ncc=0;nc<nbc;nc+=2,ncc++){
               Tcl_ListObjIndex(Interp,Objv[4],nc,&obj);
               Tcl_GetDoubleFromObj(Interp,obj,&lat[ncc]);
               Tcl_ListObjIndex(Interp,Objv[4],nc+1,&obj);
               Tcl_GetDoubleFromObj(Interp,obj,&lon[ncc]);
            }

            Tcl_ListObjLength(Interp,Objv[3],&nbf);
            if (nbf) {
               fields=(TData**)malloc(nbf*sizeof(TData*));
               for (nc=0;nc<nbf;nc++){
                  Tcl_ListObjIndex(Interp,Objv[3],nc,&obj);
                  if (!(fields[nc]=Data_Get(Tcl_GetString(obj)))) {
                     Tcl_AppendResult(Interp,"FSTD_FieldCmd: Invalid field (",Tcl_GetString(obj),")",(char*)NULL);
                     free(fields);
                     return(TCL_ERROR);
                  }
               }
            }

            ok=Data_Cut(Interp,fields,Tcl_GetString(Objv[2]),lat,lon,nbf,nbc/2);

            free(lat);
            free(lon);
            if (fields) free(fields);
            return(ok);
         }
         break;

      case GRIDINTERP:
         if(Objc<4) {
            Tcl_WrongNumArgs(Interp,2,Objv,"fldto from [...]");
            return(TCL_ERROR);
         }
         field0=Data_Get(Tcl_GetString(Objv[2]));
         if (!field0) {
            Tcl_AppendResult(Interp,"invalid field :",Tcl_GetString(Objv[2]),(char*)NULL);
            return(TCL_ERROR);
         }
         if (field0->Ref && field0->Ref->Type|GRID_SPARSE)
            FSTD_FieldReadMesh(field0);

         n=-1;
         /*Interpolate a field*/
         field1=Data_Get(Tcl_GetString(Objv[3]));
         if (field1) {
            if (Objc>4) {
               if (Tcl_GetIndexFromObj(Interp,Objv[4],moderas,"mode",0,&n)!=TCL_OK) {
                  return(TCL_ERROR);
               }
            }

            if (field1->Ref && field1->Ref->Type|GRID_SPARSE) {
               FSTD_FieldReadMesh(field1);
            }

            /*If grids are the same and this is not a NOP,ACCUM or BUFFER call*/
            if (n<16 && field0->Ref->Id==field1->Ref->Id) {
               if (!Data_Copy(Interp,field1,Tcl_GetString(Objv[2]),1)) {
                  return(TCL_ERROR);
               } else {
                  return(TCL_OK);
               }
            }

            if (n==3 || n==4) {
               if (Objc>8 || Objc<6) {
                  Tcl_WrongNumArgs(Interp,2,Objv,"fldto fldfrom Type Split [Final] [Index list variable]");
                  return(TCL_ERROR);
               }
               if (Tcl_GetIntFromObj(Interp,Objv[5],&ni)==TCL_ERROR) {
                  return(TCL_ERROR);
               }
               nj=1;
               obj=NULL;
               if (Objc>6) {
                  if (Tcl_GetBooleanFromObj(Interp,Objv[6],&nj)==TCL_ERROR) {
                     obj=Objv[6];
                  }
               }
               if (Objc>7) {
                  obj=Objv[7];
               }
               /*Check compatibility between source and destination*/
               if (!DataDef_Compat(field0->Def,field1->Def)) {
                  field0->Ref=GeoRef_Resize(field0->Ref,field0->Def->NI,field0->Def->NJ,field0->Def->NK,field1->Ref->ZRef.Type,field1->Ref->ZRef.Levels);
               }
               field0->Ref->ZRef.Type=field1->Ref->ZRef.Type;
               FSTD_FieldSetTo(field0,field1);
               return(Data_GridConservative(Interp,field0->Ref,field0->Def,field1->Ref,field1->Def,Tcl_GetString(Objv[4])[0],nj,ni,obj));
            } else if (n>=5 && n<=18) {
               if (Objc<5 || Objc>7) {
                  Tcl_WrongNumArgs(Interp,2,Objv,"fldto fldfrom [Type] [Values] [Final]");
                  return(TCL_ERROR);
               }

               ni=1;
               table=NULL;
               fieldt=NULL;
               if (Objc>5) {
                  if (Tcl_GetBooleanFromObj(Interp,Objv[5],&ni)==TCL_ERROR) {
                     if (!(fieldt=Data_Get(Tcl_GetString(Objv[5])))) {
                        Tcl_ListObjLength(Interp,Objv[5],&nk);
                        table=(double*)malloc(nk*sizeof(double));
                        for(k=0;k<nk;k++) {
                           Tcl_ListObjIndex(Interp,Objv[5],k,&obj);
                           Tcl_GetDoubleFromObj(Interp,obj,&table[k]);
                        }
                        if (nk!=field0->Def->NK) {
                           field0->Def=DataDef_Resize(field0->Def,field0->Def->NI,field0->Def->NJ,nk);
                           for(key=0;key<FSIZE3D(field0->Def);key++) {
                              Def_Set(field0->Def,0,key,0);
                           }
                           GeoRef_Resize(field0->Ref,field0->Def->NI,field0->Def->NJ,nk,field0->Ref->ZRef.Type,field0->Ref->ZRef.Levels);
                        }
                     }
                  }
               }
               if(Objc==7) {
                  Tcl_GetBooleanFromObj(Interp,Objv[6],&ni);
               }
               key=Data_GridAverage(Interp,field0->Ref,field0->Def,field1->Ref,field1->Def,table,fieldt?fieldt->Def:NULL,n,ni);
               if (table)
                  free(table);
               return(key);
            } else {
               return(FSTD_FieldGridInterpolate(Interp,field0,field1,n));
            }
            break;
         }

         /*Interpolate a band*/
         band=GDAL_BandGet(Tcl_GetString(Objv[3]));
         if (band) {
            if (Objc>4) {
               if (Tcl_GetIndexFromObj(Interp,Objv[4],moderas,"mode",0,&n)!=TCL_OK) {
                  return(TCL_ERROR);
               }
            }

            if (n==3 || n==4) {
               if (Objc>8 || Objc<6) {
                  Tcl_WrongNumArgs(Interp,2,Objv,"fldto bandfrom [Type] [Split] [Final] [Index list variable]");
                  return(TCL_ERROR);
               }
               if (Tcl_GetIntFromObj(Interp,Objv[5],&ni)==TCL_ERROR) {
                  return(TCL_ERROR);
               }
               nj=1;
               obj=NULL;
               if (Objc>6) {
                  if (Tcl_GetBooleanFromObj(Interp,Objv[6],&nj)==TCL_ERROR) {
                     obj=Objv[6];
                  }
               }
               if (Objc>7) {
                  obj=Objv[7];
               }
               return(Data_GridConservative(Interp,field0->Ref,field0->Def,band->Ref,band->Def,Tcl_GetString(Objv[4])[0],nj,ni,obj));
            } else if (n>=5 && n<=18) {
               if (Objc<5 || Objc>7) {
                  Tcl_WrongNumArgs(Interp,2,Objv,"fldto bandfrom [Type] [Values] [Final]");
                  return(TCL_ERROR);
               }
               ni=1;
               table=NULL;
               bandt=NULL;
               if (Objc>5) {
                  if (Tcl_GetBooleanFromObj(Interp,Objv[5],&ni)==TCL_ERROR) {
                     if (!(bandt=GDAL_BandGet(Tcl_GetString(Objv[5])))) {
                        Tcl_ListObjLength(Interp,Objv[5],&nk);
                        table=(double*)malloc((nk+1)*sizeof(double));
                        for(k=0;k<nk;k++) {
                           Tcl_ListObjIndex(Interp,Objv[5],k,&obj);
                           Tcl_GetDoubleFromObj(Interp,obj,&table[k]);
                        }
                        table[k]=field0->Def->NoData;
                        if (nk!=field0->Def->NK) {
                           if (!(field0->Def=DataDef_Resize(field0->Def,field0->Def->NI,field0->Def->NJ,nk))) {
                              Tcl_AppendResult(Interp,"Unable to rellocate buffer",(char*)NULL);
                              return (TCL_ERROR);
                           }

                           for(dk=0;dk<FSIZE3D(field0->Def);dk++) {
                              Def_Set(field0->Def,0,dk,0);
                           }
                           GeoRef_Resize(field0->Ref,field0->Def->NI,field0->Def->NJ,nk,field0->Ref->ZRef.Type,field0->Ref->ZRef.Levels);
                        }
                     }
                  }
               }
               if (Objc==7) {
                  Tcl_GetBooleanFromObj(Interp,Objv[6],&ni);
               }
               key=Data_GridAverage(Interp,field0->Ref,field0->Def,band->Ref,band->Def,table,bandt?bandt->Def:NULL,n,ni);
               if (table)
                  free(table);
               return(key);
            } else {
               return(Data_GridInterpolate(Interp,Objv[4]?Tcl_GetString(Objv[4])[0]:'L',field0->Ref,field0->Def,band->Ref,band->Def));
            }
            break;
         }

         /*Interpolate a layer*/
         layer=OGR_LayerGet(Tcl_GetString(Objv[3]));
         if (layer) {
            if (Objc!=5 && Objc!=6) {
               Tcl_WrongNumArgs(Interp,2,Objv,"field layer type [field]");
               return(TCL_ERROR);
            }
            if (Tcl_GetIndexFromObj(Interp,Objv[4],modeogr,"mode",0,&n)!=TCL_OK) {
               return(TCL_ERROR);
            }
            field=NULL;
            x=1.0;

            if (Objc==6) {
               if (Tcl_GetDoubleFromObj(Interp,Objv[5],&x)==TCL_ERROR) {
                  field=Tcl_GetString(Objv[5]);
               }
            }
            imode=modeogr[n][0];
            itype='A';
            if (imode=='L') {
               imode=modeogr[n][7];
               itype='L';
            } else if (imode=='P') {
               imode=modeogr[n][6];
               itype='P';
            }
            return(Data_GridOGR(Interp,field0->Def,field0->Ref,layer,imode,itype,1,field,x));
         }

         /*Interpolate a model*/
         model=Model_Get(Tcl_GetString(Objv[3]));
         if (model) {
            if (Objc!=5 && Objc!=6) {
               Tcl_WrongNumArgs(Interp,2,Objv,"field model mode [field]");
               return(TCL_ERROR);
            }
            if (Tcl_GetIndexFromObj(Interp,Objv[4],modeogr,"mode",0,&n)!=TCL_OK) {
               return(TCL_ERROR);
            }
            x=1.0;

            if (Objc==6) {
               if (Tcl_GetDoubleFromObj(Interp,Objv[5],&x)==TCL_ERROR) {
                  field=Tcl_GetString(Objv[5]);
               }
            }
//            imode=modeogr[n][0];
            return(Model_Grid(Interp,field0,model,NULL));
         }

         /*Interpolate an observation*/
         obs=Obs_Get(Tcl_GetString(Objv[3]));
         if (obs) {
            if(Objc!=8 && Objc!=9) {
               Tcl_WrongNumArgs(Interp,2,Objv,"fldto obsfrom [SPHERICAL | EXPONENTIAL | GAUSSIAN | LINEAR] Nugget(C0) Sill(C1) Range(A) [Outside]");
               return(TCL_ERROR);
            }
            Tcl_GetDoubleFromObj(Interp,Objv[5],&c0);
            Tcl_GetDoubleFromObj(Interp,Objv[6],&c1);c1-=c0;
            Tcl_GetDoubleFromObj(Interp,Objv[7],&a);

            n=1;
            if (Objc==9) {
               Tcl_GetBooleanFromObj(Interp,Objv[8],&n);
            }

            switch(Tcl_GetString(Objv[4])[0]) {
               case 'S':id=1;break;
               case 'E':id=2;break;
               case 'G':id=3;break;
               case 'L':id=4;break;
            }
            if ((pos=Obs_Grid(field0->Ref,obs,&npos,n))) {
               ok=FFKrigging(field0->Ref,field0->Def,pos,npos,c0,c1,a,id);
               free(pos);
            } else {
               Tcl_AppendResult(Interp,"Unable to calculate position vector",(char*)NULL);
               return(TCL_ERROR);
            }

            if (!ok) {
               Tcl_AppendResult(Interp,"Obs krigging failed",(char*)NULL);
               return(TCL_ERROR);
            }
            if (field0->Stat) { free(field0->Stat); field0->Stat=NULL; }
            return(TCL_OK);
         }

         /*Interpolate a metobs*/
         metobs=MetObs_Get(Tcl_GetString(Objv[3]));
         if (metobs) {
            if(Objc!=10 && Objc!=11) {
               Tcl_WrongNumArgs(Interp,2,Objv,"fldto metobsfrom time element [SPHERICAL | EXPONENTIAL | GAUSSIAN | LINEAR] Nugget(C0) Sill(C1) Range(A) [Outside]");
               return(TCL_ERROR);
            }
            Tcl_GetLongFromObj(Interp,Objv[4],&time);

            Tcl_GetDoubleFromObj(Interp,Objv[7],&c0);
            Tcl_GetDoubleFromObj(Interp,Objv[8],&c1);
            Tcl_GetDoubleFromObj(Interp,Objv[9],&a);

            n=1;
            if(Objc==11) {
               Tcl_GetBooleanFromObj(Interp,Objv[10],&n);
            }

            switch(Tcl_GetString(Objv[6])[0]) {
               case 'S':id=1;break;
               case 'E':id=2;break;
               case 'G':id=3;break;
               case 'L':id=4;break;
            }


            if ((pos=MetObs_Grid(Interp,field0->Ref,metobs,time,Objv[5],&npos,n))) {
               ok=FFKrigging(field0->Ref,field0->Def,pos,npos,c0,c1,a,id);
               free(pos);
            } else {
               Tcl_AppendResult(Interp,"Unable to calculate position vector",(char*)NULL);
               return(TCL_ERROR);
            }

            if (!ok) {
               Tcl_AppendResult(Interp,"MetObs krigging failed",(char*)NULL);
               return(TCL_ERROR);
            }
            if (field0->Stat) { free(field0->Stat); field0->Stat=NULL; }
            return(TCL_OK);
         }

         /* If we get here, it has to be a NOP or ACCUM*/
         if (Objc>4) {
            if (Tcl_GetIndexFromObj(Interp,Objv[4],moderas,"mode",0,&n)!=TCL_OK) {
               return(TCL_ERROR);
            }
         }
         if (n==16 || n==17 || n==18) {
            return(Data_GridAverage(Interp,field0->Ref,field0->Def,NULL,NULL,NULL,NULL,n,1));
         } else {
            Tcl_AppendResult(Interp,"invalid data type",(char*)NULL);
            return(TCL_ERROR);
         }
         break;

      case VERTICALINTERP:
         if(Objc!=6) {
            Tcl_WrongNumArgs(Interp,2,Objv,"fldto fldfrom [GZTo | P0To] [GZFrom | P0From]");
            return TCL_ERROR;
         }
         return(FSTD_FieldVertInterpolate(Interp,Data_Get(Tcl_GetString(Objv[2])),Data_Get(Tcl_GetString(Objv[3])),Data_Get(Tcl_GetString(Objv[4])),Data_Get(Tcl_GetString(Objv[5]))));
         break;

      case TIMEINTERP:
         if(Objc!=6) {
            Tcl_WrongNumArgs(Interp,2,Objv,"fldto fld0 fld1 stamp");
            return TCL_ERROR;
         }
         Tcl_GetIntFromObj(Interp,Objv[5],&id);
         return(FSTD_FieldTimeInterpolate(Interp,id,Tcl_GetString(Objv[2]),Data_Get(Tcl_GetString(Objv[3])),Data_Get(Tcl_GetString(Objv[4]))));
         break;

      case DEFINE:
         if(Objc<3) {
            Tcl_WrongNumArgs(Interp,2,Objv,"fld ?option?");
            return(TCL_ERROR);
         }
         field0=Data_Get(Tcl_GetString(Objv[2]));
         if (!field0) {
            Tcl_AppendResult(Interp,"invalid field",(char*)NULL);
            return(TCL_ERROR);
         }

         return(FSTD_FieldDefine(Interp,field0,Objc-3,Objv+3));
         break;
   }
   return TCL_OK;
}

/*----------------------------------------------------------------------------
 * Nom      : <FSTD_FileCmd>
 * Creation : Aout 1998 - J.P. Gauthier - CMC/CMOE
 *
 * But      : Appel des commandes relies aux fichiers standards.
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
static int FSTD_FileCmd(ClientData clientData,Tcl_Interp *Interp,int Objc,Tcl_Obj *CONST Objv[]){

   int                n,idx,itype,type;
   FSTD_File         *file=NULL;

   static CONST char *types[] = { "ALL","NOMVAR","TYPVAR","DATEV","IP1" };
   static CONST char *sopt[] = { "is","open","close","filename","info",NULL };
   enum               opt { IS,OPEN,CLOSE,FILENAME,INFO };

   Tcl_ResetResult(Interp);

   if (Objc<2) {
      Tcl_WrongNumArgs(Interp,1,Objv,"command ?arg arg ...?");
      return(TCL_ERROR);
   }

   if (Tcl_GetIndexFromObj(Interp,Objv[1],sopt,"command",0,&idx)!=TCL_OK) {
      return(TCL_ERROR);
   }

   switch ((enum opt)idx) {
      case IS:
         if(Objc!=3) {
            Tcl_WrongNumArgs(Interp,2,Objv,"filename");
            return(TCL_ERROR);
         }
         type=f77name(wkoffit)(Tcl_GetString(Objv[2]),strlen(Tcl_GetString(Objv[2])));
         if (type==1 || type==2 || type==3 || type==33 || type==34) {
            Tcl_SetObjResult(Interp,Tcl_NewBooleanObj(1));
         } else {
            Tcl_SetObjResult(Interp,Tcl_NewBooleanObj(0));
         }
         return(TCL_OK);
         break;

      case OPEN:
         if(Objc!=5) {
            Tcl_WrongNumArgs(Interp,2,Objv,"id mode filename");
            return(TCL_ERROR);
         }
         return(FSTD_FileOpen(Interp,Tcl_GetString(Objv[2]),Tcl_GetString(Objv[3])[0],Tcl_GetString(Objv[4])));
         break;

      case CLOSE:
         if(Objc<3) {
            Tcl_WrongNumArgs(Interp,2,Objv,"fileid");
            return(TCL_ERROR);
         }
         for(n=2;n<Objc;n++) {
            FSTD_FileClose(Interp,Tcl_GetString(Objv[n]));
         }
         return(TCL_OK);
         break;

      case FILENAME:
         if(Objc!=3) {
            Tcl_WrongNumArgs(Interp,2,Objv,"id");
            return(TCL_ERROR);
         }
         if (!(file=FSTD_FileGet(Interp,Tcl_GetString(Objv[2])))) {
            return(TCL_ERROR);
         }
         Tcl_SetObjResult(Interp,Tcl_NewStringObj(file->Name,-1));
         return(TCL_OK);
         break;

      case INFO:
         if(Objc!=4 && Objc!=5) {
            Tcl_WrongNumArgs(Interp,2,Objv,"id mode [var]");
            return(TCL_ERROR);
         }
         if (!(file=FSTD_FileGet(Interp,Tcl_GetString(Objv[2])))) {
            return(TCL_ERROR);
         }
         if (Tcl_GetIndexFromObj(Interp,Objv[3],types,"type",0,&itype)!=TCL_OK) {
            return(TCL_ERROR);
         }
         return(FSTD_FieldList(Interp,file,itype,Objc==5?Tcl_GetString(Objv[4]):NULL));
         break;
   }
   return(TCL_OK);
}

/*----------------------------------------------------------------------------
 * Nom      : <FSTD_FileClose>
 * Creation : Aout 1998 - J.P. Gauthier - CMC/CMOE
 *
 * But      : Ferme le fichier standard a la RPN.
 *
 * Parametres :
 *  <Interp>  : Interpreteur TCL.
 *  <Id>      : Identificateur a donner au fichier
 *
 * Retour:
 *  <TCL_...> : Code de reussite.
 *
 * Remarques :
 *
 *----------------------------------------------------------------------------
*/
int FSTD_FileClose(Tcl_Interp *Interp,char *Id){

   FSTD_File *file;

   if ((file=(FSTD_File*)TclY_HashDel(&FSTD_FileTable,Id))) {
      file->Open=file->Open<0?1:file->Open;
      FSTD_FileUnset(Interp,file);
      cs_fstunlockid(file->Id);

      free(file->Name);
      free(file->CId);
      free(file);
   }
   return(TCL_OK);
}

/*----------------------------------------------------------------------------
 * Nom      : <FSTD_FileOpen>
 * Creation : Aout 1998 - J.P. Gauthier - CMC/CMOE
 *
 * But      : Effectue l'ouverture d'un fichier standard a la methode RPN.
 *
 * Parametres :
 *  <Interp>  : Interpreteur TCL.
 *  <Id>      : Identificateur a donner au fichier
 *  <Mode>    : Mode d'ouverture (R ou W)
 *  <Name>    : Non du fichier
 *
 * Retour:
 *  <TCL_...> : Code d'erreur de TCL.
 *  <tcl>     : Liste des parametres des enregistrements contenue dans le fichier
 *
 * Remarques :
 *
 *----------------------------------------------------------------------------
*/
int FSTD_FileOpen(Tcl_Interp *Interp,char *Id,char Mode,char *Name){

   Tcl_HashEntry *entry;
   FSTD_File     *file;
   int            new;
   char           buf[2048];

   /* Creer l'entree dans la liste table de fichiers standards */
   entry=TclY_CreateHashEntry(&FSTD_FileTable,Id,&new);
   if (!new) {
      Tcl_AppendResult(Interp,"FSTD_FileOpen: File already opened, cannot reuse openned file identificator ",Id,(char*)NULL);
      return(TCL_ERROR);
   }

   file=(FSTD_File*)malloc(sizeof(FSTD_File));
   file->Mode=Mode;
   file->CId=strdup(Id);
   file->Id=cs_fstlockid();
   file->Open=file->Id<1?-1:0;
   file->Name=strdup(Name);

   Tcl_SetHashValue(entry,file);

   return(FSTD_FieldList(Interp,file,FSTD_LISTALL,NULL));
}

/*----------------------------------------------------------------------------
 * Nom      : <FSTD_FileGet>
 * Creation : Juin 2003 - J.P. Gauthier - CMC/CMOE
 *
 * But      : Recuperer l'adresse de la structure du fichier correspondant
 *            a l'Id.
 *
 * Parametres :
 *  <Interp>  : Interpreteur TCL.
 *  <Id>      : Identificateur a donner au fichier
 *
 * Retour:
 *  <Bool>    : Code de reussite
 *
 * Remarques :
 *
 *----------------------------------------------------------------------------
*/
FSTD_File* FSTD_FileGet(Tcl_Interp *Interp,char *Id){

   Tcl_HashEntry *entry;

   entry=TclY_FindHashEntry(&FSTD_FileTable,Id);
   if (!entry) {
      if (Interp) Tcl_AppendResult(Interp,"FSTD_FileGet: Unknown file",(char *)NULL);
      return(NULL);
   }

   return((FSTD_File*)(Tcl_GetHashValue(entry)));
}

/*----------------------------------------------------------------------------
 * Nom      : <FSTD_FileSet>
 * Creation : Juin 2003 - J.P. Gauthier - CMC/CMOE
 *
 * But      : Effectue l'ouverture d'un fichier standard a la methode RPN.
 *
 * Parametres :
 *  <Interp>  : Interpreteur TCL.
 *  <File>    : Pointeur de la structure du fichier
 *
 * Retour:
 *  <Bool>    : Code de reussite
 *
 * Remarques :
 *
 *----------------------------------------------------------------------------
*/
int FSTD_FileSet(Tcl_Interp *Interp,FSTD_File *File){

   int ok=0,rem=0;

   if (!File)
      return(-1);

   if (index(File->Name,':') && File->Name[0]!=':') {
      rem=1;
   }

   EZLock_RPNFile();
   if (!File->Open || File->Open==-1) {
#ifdef LNK_FSTD

      if (File->Mode=='w' ||  File->Mode=='W') {                /*Write Mode*/
          if (rem)
            ok=c_fnom(&File->Id,File->Name,"STD+RND+R/W+REMOTE",0);
          else
            ok=c_fnom(&File->Id,File->Name,"STD+RND+R/W",0);
      } else if (File->Mode=='a' ||  File->Mode=='A') {         /*Append Mode*/
         if (rem)
            ok=c_fnom(&File->Id,File->Name,"STD+RND+OLD+R/W+REMOTE",0);
         else
            ok=c_fnom(&File->Id,File->Name,"STD+RND+OLD+R/W",0);
      } else {                                                  /*ReadOnly Mode*/
         if (rem) {
            ok=c_fnom(&File->Id,File->Name,"STD+RND+R/O+REMOTE",0);
         } else
            ok=c_fnom(&File->Id,File->Name,"STD+RND+R/O",0);
      }

      if (ok<0) {
         if (Interp) Tcl_AppendResult(Interp,"FSTD_FileSet: Unable to link standard file name, ",File->Name," (c_fnom failed)",(char *)NULL);
         ok=c_fclos(File->Id);
         EZUnLock_RPNFile();
         return(-1);
      }

      ok=c_fstouv(File->Id,"RND");
      if (ok<=-1) {
         // We should close the fid but c_fstouv keeps something opened and fstfrm blows up.
         // ok=c_fclos(File->Id);
         if (Interp) Tcl_AppendResult(Interp,"FSTD_FileSet: Unable to open standard file, ",File->Name," (c_fstouv)",(char *)NULL);
         EZUnLock_RPNFile();
         return(-1);
      }
   }
#endif
   File->Open=File->Open<0?-2:File->Open+1;
   EZUnLock_RPNFile();

   return(ok);
}

/*----------------------------------------------------------------------------
 * Nom      : <FSTD_FileUnset>
 * Creation : Juin 2003 - J.P. Gauthier - CMC/CMOE
 *
 * But      : Effectue la fermeture d'un fichier standard a la methode RPN.
 *
 * Parametres :
 *  <Interp>  : Interpreteur TCL.
 *  <File>    : Pointeur de la structure du fichier
 *
 * Retour:
 *  <Bool>    : Code de reussite
 *
 * Remarques :
 *
 *----------------------------------------------------------------------------
*/
int FSTD_FileUnset(Tcl_Interp *Interp,FSTD_File *File) {

   int ok=-1;

   if (!File)
      return(0);

   EZLock_RPNFile();
   File->Open--;

   if (!File->Open) {
#ifdef LNK_FSTD
      ok=c_fstfrm(File->Id);
      ok=c_fclos(File->Id);
#endif
      if (ok>=0){
         ok=1;
      } else {
         if (Interp) Tcl_AppendResult(Interp,"FSTD_FileUnset: Could not close file (c_fstfrm and/or c_fclos failed)",(char *)NULL);
         ok=0;
      }
   }

   EZUnLock_RPNFile();
   return(ok);
}

/*----------------------------------------------------------------------------
 * Nom      : <FSTD_StampCmd>
 * Creation : Juillet 2001 - J.P. Gauthier - CMC/CMOE
 *
 * But      : Appel des commandes relatives aux manipulations des "stamp cmc".
 *
 * Parametres     :
 *  <clientData>  : Donnees du module.
 *  <interp>      : Interpreteur TCL.
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
static int FSTD_StampCmd (ClientData clientData,Tcl_Interp *Interp,int Objc,Tcl_Obj *CONST Objv[]){

   Tcl_Obj      *obj,*list;
   char          buf[50];
   int           stamp,date=0,time,op;
   int           yyyy,mm,dd,hh,nn,ss;
   double        tmpd;
   long          sec;

   struct timeb  tb;
   unsigned long long tkey,pkey,pkey1,pkey2;
   int                n,nkey1,nkey2;

   int         idx;
   static CONST char *sopt[] = { "toseconds","fromseconds","todate","fromdate","incr","diff","key",NULL };
   enum                opt { TOSECONDS,FROMSECONDS,TODATE,FROMDATE,INCR,DIFF,KEY };

#ifdef LNK_FSTD

   extern time_t timezone;

   Tcl_ResetResult(Interp);

   if (Objc<2) {
      Tcl_WrongNumArgs(Interp,1,Objv,"command ?arg arg ...?");
      return TCL_ERROR;
   }

   if (Tcl_GetIndexFromObj(Interp,Objv[1],sopt,"command",0,&idx)!=TCL_OK) {
      return TCL_ERROR;
   }

   switch ((enum opt)idx) {

      case KEY:
        if(Objc!=3) {
            Tcl_WrongNumArgs(Interp,2,Objv,"[bit lenght list]");
            return(TCL_ERROR);
         }

         pkey1=pkey2=0;
         nkey1=0;
         nkey2=64;

         ftime(&tb);
         pkey1=tb.time;
         pkey1=pkey1*1000+tb.millitm;
//         pkey2=1+(int)(255.0*rand()/(RAND_MAX+1.0));

         Tcl_ListObjLength(Interp,Objv[2],&nn);
         list=Tcl_NewListObj(0,NULL);
         for(n=0;n<nn;n++) {
            Tcl_ListObjIndex(Interp,Objv[2],n,&obj);
            Tcl_GetIntFromObj(Interp,obj,&hh);

            pkey=pkey1>>nkey1+(pkey2<<nkey2);
//            pkey=pkey1>>(nkey1+2);
//         fprintf(stderr,"----------- %i = %i %i %llu \n",hh,nkey1,nkey2,pkey);
            tkey=(pkey<<(64-hh))>>(64-hh);
            Tcl_ListObjAppendElement(Interp,list,Tcl_NewWideIntObj(tkey));
            nkey1+=hh;
            nkey2-=hh;
         }
         Tcl_SetObjResult(Interp,list);
         break;

      case TOSECONDS:

        if(Objc!=3) {
            Tcl_WrongNumArgs(Interp,2,Objv,"stamp");
            return(TCL_ERROR);
         }
         TclY_Get0IntFromObj(Interp,Objv[2],&stamp);
         Tcl_SetObjResult(Interp,Tcl_NewLongObj(System_Stamp2Seconds(stamp)));
         return(TCL_OK);
         break;

      case FROMSECONDS:

         if(Objc!=3) {
            Tcl_WrongNumArgs(Interp,2,Objv,"seconds");
            return(TCL_ERROR);
         }
         Tcl_GetLongFromObj(Interp,Objv[2],&sec);
         sprintf(buf,"%09i",System_Seconds2Stamp(sec));
         Tcl_SetObjResult(Interp,Tcl_NewStringObj(buf,-1));
         return(TCL_OK);
         break;

      case TODATE:
         if(Objc!=3 && Objc!=4) {
            Tcl_WrongNumArgs(Interp,2,Objv,"stamp [format]");
            return(TCL_ERROR);
         }
         TclY_Get0IntFromObj(Interp,Objv[2],&stamp);

         System_StampDecode(stamp,&yyyy,&mm,&dd,&hh,&nn,&ss);

         if (Objc==4) {
            sprintf(buf,Tcl_GetString(Objv[3]),yyyy,mm,dd,hh,nn,ss);
         } else {
            sprintf(buf,"%i %02i %02i %02i %02i %02i",yyyy,mm,dd,hh,nn,ss);
         }
         Tcl_SetObjResult(Interp,Tcl_NewStringObj(buf,-1));
         return(TCL_OK);
         break;

      case FROMDATE:
         if(Objc!=4) {
            Tcl_WrongNumArgs(Interp,2,Objv,"yyyymmdd hhmmss00");
            return TCL_ERROR;
         }
         Tcl_GetIntFromObj(Interp,Objv[2],&date);
         TclY_Get0IntFromObj(Interp,Objv[3],&time);

         op=3;
         f77name(newdate)(&stamp,&date,&time,&op);

         sprintf(buf,"%09i",stamp);
         Tcl_SetObjResult(Interp,Tcl_NewStringObj(buf,-1));
         return(TCL_OK);
         break;

      case INCR:
         if(Objc!=4) {
            Tcl_WrongNumArgs(Interp,2,Objv,"stamp hours");
            return(TCL_ERROR);
         }
         TclY_Get0IntFromObj(Interp,Objv[2],&stamp);
         if (TclY_Get0IntFromObj(Interp,Objv[3],&hh)==TCL_ERROR) {
            Tcl_GetDoubleFromObj(Interp,Objv[3],&tmpd);
         } else {
            tmpd=hh;
         }
         f77name(incdatr)(&op,&stamp,&tmpd);

         sprintf(buf,"%09i",op);
         Tcl_SetObjResult(Interp,Tcl_NewStringObj(buf,-1));
         return(TCL_OK);
         break;

      case DIFF:
         if(Objc!=4) {
            Tcl_WrongNumArgs(Interp,2,Objv,"stamp1 stamp2");
            return(TCL_ERROR);
         }
         TclY_Get0IntFromObj(Interp,Objv[2],&stamp);
         TclY_Get0IntFromObj(Interp,Objv[3],&time);

         f77name(difdatr)(&stamp,&time,&tmpd);

         Tcl_SetObjResult(Interp,Tcl_NewDoubleObj(tmpd));
         return(TCL_OK);
         break;
   }
#endif
   return(TCL_OK);
}

/*----------------------------------------------------------------------------
 * Nom      : <tclFSTD_Init>
 * Creation : Aout 1998 - J.P. Gauthier - CMC/CMOE
 *
 * But      : Initialiser le package lors de l'inclusion par Tcl.
 *
 * Parametres :
 *  <Interp>  : Interpreteur TCL
 *
 * Retour:
 *
 * Remarques :
 *
 *----------------------------------------------------------------------------
*/
int TclFSTD_Init(Tcl_Interp *Interp) {

   Tcl_CreateObjCommand(Interp,"fstdfile",FSTD_FileCmd,(ClientData)NULL,(Tcl_CmdDeleteProc *)NULL);
   Tcl_CreateObjCommand(Interp,"fstdfield",FSTD_FieldCmd,(ClientData)NULL,(Tcl_CmdDeleteProc *)NULL);
   Tcl_CreateObjCommand(Interp,"fstdgrid",FSTD_GridCmd,(ClientData)NULL,(Tcl_CmdDeleteProc *)NULL);
   Tcl_CreateObjCommand(Interp,"fstdstamp",FSTD_StampCmd,(ClientData)NULL,(Tcl_CmdDeleteProc *)NULL);

   if (!FSTDInit++) {
      Tcl_InitHashTable(&FSTD_FileTable,TCL_STRING_KEYS);

      /*Force UU-VV relashionship*/
      FSTDVectorTable[0].UU=strdup("UU");
      FSTDVectorTable[0].VV=strdup("VV");
      FSTDVectorTable[0].WW=NULL;
      FSTDVectorTableSize++;

      c_fstopc("MSGLVL","WARNIN",0);
      c_fstopc("TOLRNC","SYSTEM",0);
   }

   return(TCL_OK);
}
