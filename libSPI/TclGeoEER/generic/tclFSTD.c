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

#ifdef HAVE_RMN

#include <sys/timeb.h>

#include "Dict.h"
#include "tclFSTD.h"
#include "tclObs.h"
#include "tclOGR.h"
#include "tclGDAL.h"
#include "tcl3DModel.h"
#include "Data_FF.h"

/*Table contenant la liste des champs en memoire*/
static Tcl_HashTable FSTD_FileTable;
static int           FSTDInit=0;

extern int      ZREF_IP1MODE;
extern int      FSTD_UNTILE;
extern Tcl_Obj *FSTD_HIDELIST;

int FSTD_FieldCmd(ClientData clientData,Tcl_Interp *Interp,int Objc,Tcl_Obj *CONST Objv[]);
static int FSTD_FileCmd(ClientData clientData,Tcl_Interp *Interp,int Objc,Tcl_Obj *CONST Objv[]);
static int FSTD_GridCmd(ClientData clientData,Tcl_Interp *Interp,int Objc,Tcl_Obj *CONST Objv[]);
static int FSTD_StampCmd(ClientData clientData,Tcl_Interp *Interp,int Objc,Tcl_Obj *CONST Objv[]);
static int FSTD_DictCmd(ClientData clientData,Tcl_Interp *Interp,int Objc,Tcl_Obj *CONST Objv[]);

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
   Tcl_CreateObjCommand(Interp,"fstddict",FSTD_DictCmd,(ClientData)NULL,(Tcl_CmdDeleteProc *)NULL);

   if (!FSTDInit++) {
      Tcl_InitHashTable(&FSTD_FileTable,TCL_STRING_KEYS);

      c_fstopc("MSGLVL","WARNIN",0);
      c_fstopc("TOLRNC","SYSTEM",0);
      c_ezsetopt("VERBOSE","NO");

   }

   return(TCL_OK);
}

/*----------------------------------------------------------------------------
 * Nom      : <FSTD_FieldIPGet>
 * Creation : Novembre 2012 - J.P. Gauthier - CMC/CMOE
 *
 * But      : Conversion des IP provenant de Tcl
 *
 * Parametres     :
 *  <Interp>      : Interpreteur TCL.
 *  <Obj>         : IP ou { Valeur Type }
 *  <ObjType>     : Type ou NULL
 *
 * Retour:
 *  <IP> : Valeur du IP.
 *
 * Remarques :
 *
 *----------------------------------------------------------------------------
*/
int FSTD_FieldIPGet(Tcl_Interp *Interp,Tcl_Obj *Obj,Tcl_Obj *ObjType) {

   Tcl_Obj *obj=NULL;
   int      n,type;
   double   val;

   /*Get Value and Type*/
   Tcl_ListObjLength(Interp,Obj,&n);
   if (n==2) {
      Tcl_ListObjIndex(Interp,Obj,0,&obj);
      Tcl_GetDoubleFromObj(Interp,obj,&val);

      Tcl_ListObjIndex(Interp,Obj,1,&obj);
   } else {
      Tcl_GetDoubleFromObj(Interp,Obj,&val);
      obj=ObjType;
   }

   if (obj && strlen(Tcl_GetString(obj))) {
      if (Tcl_GetIndexFromObj(Interp,obj,ZRef_LevelNames(),"type",TCL_EXACT,&type)!=TCL_OK) {
         return(-2);
      }
      return(ZRef_Level2IP(val,type,DEFAULT));
   } else {
      return(val);
   }
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
   TData      *field,*p0=NULL;
   TRPNHeader *head;
   float       dlat,dlon,dd60,dgrw,x,y,level=0.0;
   int         ip,n,kind;
   char        buf[50];
   double      tmp;
   int         idx,idxk,i,j,k;

   double dxg1,dxg2,dxg3,dxg4;
   float  xg1,xg2,xg3,xg4;
   int    ig1,ig2,ig3,ig4;

   static CONST char *sopt[] = { "xyfll","llfxy","convip","cxgaig","cigaxg","mscale","pressure",NULL };
   enum                opt { XYFLL,LLFXY,CONVIP,CXGAIG,CIGAXG,MSCALE,PRESSURE };

   Tcl_ResetResult(Interp);

   if (Objc<2) {
      Tcl_WrongNumArgs(Interp,1,Objv,"command ?arg arg ...?");
      return(TCL_ERROR);
   }

   if (Tcl_GetIndexFromObj(Interp,Objv[1],sopt,"command",TCL_EXACT,&idx)!=TCL_OK) {
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

         f77name(xyfll)(&x,&y,&dlat,&dlon,&dd60,&dgrw,&n);
         obj=Tcl_NewListObj(0,NULL);
         Tcl_ListObjAppendElement(Interp,obj,Tcl_NewDoubleObj(x));
         Tcl_ListObjAppendElement(Interp,obj,Tcl_NewDoubleObj(y));
         Tcl_SetObjResult(Interp,obj);
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

         f77name(llfxy)(&dlat,&dlon,&x,&y,&dd60,&dgrw,&n);
         obj=Tcl_NewListObj(0,NULL);
         Tcl_ListObjAppendElement(Interp,obj,Tcl_NewDoubleObj(dlat));
         Tcl_ListObjAppendElement(Interp,obj,Tcl_NewDoubleObj(dlon));
         Tcl_SetObjResult(Interp,obj);
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
         head=((TRPNHeader*)(field->Head));
         f77name(cigaxg)(field->GRef->Grid,&xg1,&xg2,&xg3,&xg4,&head->IG1,&head->IG2,&head->IG3,&head->IG4);
         f77name(mscale)((float*)field->Def->Data[0],&xg3,&xg1,&xg2,&field->Def->NI,&field->Def->NJ);
         break;

      case PRESSURE:

         if(Objc<3) {
            Tcl_WrongNumArgs(Interp,2,Objv,"field");
            return(TCL_ERROR);
         }

         if (!(field=Data_Get(Tcl_GetString(Objv[2])))) {
            Tcl_AppendResult(Interp,"invalid field :",Tcl_GetString(Objv[2]),(char*)NULL);
            return(TCL_ERROR);
         }
         
         if (field->ZRef->PTop==0.0 && field->ZRef->PRef==0.0) {
            if (!FSTD_DecodeRPNLevelParams(field)) {
               Tcl_AppendResult(Interp,"Could not find level parameters from file",(char*)NULL);
               return(TCL_ERROR);
            }
         }
         
         if (field->ZRef->Type!=LVL_PRES) {
            if (Objc!=4) {
               Tcl_WrongNumArgs(Interp,2,Objv,"field p0(mb)");
               return(TCL_ERROR);
            }
            
            if (!(p0=Data_Get(Tcl_GetString(Objv[3])))) {
               Tcl_AppendResult(Interp,"invalid pressure field :",Tcl_GetString(Objv[3]),(char*)NULL);
               return(TCL_ERROR);
            }
         }

         if (field->Def->Type==TD_Float32 && (!p0 || p0->Def->Type==TD_Float32)) {
            ZRef_KCube2Pressure(field->ZRef,p0?(float*)(p0->Def->Data[0]):NULL,FSIZE2D(field->Def),FALSE,(float*)(field->Def->Data[0])); 
         } else {
            for(k=0;k<field->Def->NK;k++) {
               idxk=k*FSIZE2D(field->Def);
               idx=0;
               for(j=0;j<field->Def->NJ;j++) {
                  for(i=0;i<field->Def->NI;i++) {
                     if (p0) Def_Get(p0->Def,0,idx,tmp);
                     tmp=ZRef_K2Pressure(field->ZRef,tmp,k);
                     Def_Set(field->Def,0,idxk+idx,tmp);
                     idx++;
                  }
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

         f77name(cxgaig)(Tcl_GetString(Objv[2]),&ig1,&ig2,&ig3,&ig4,&xg1,&xg2,&xg3,&xg4);
         obj=Tcl_NewListObj(0,NULL);
         Tcl_ListObjAppendElement(Interp,obj,Tcl_NewIntObj(ig1));
         Tcl_ListObjAppendElement(Interp,obj,Tcl_NewIntObj(ig2));
         Tcl_ListObjAppendElement(Interp,obj,Tcl_NewIntObj(ig3));
         Tcl_ListObjAppendElement(Interp,obj,Tcl_NewIntObj(ig4));
         Tcl_SetObjResult(Interp,obj);
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

         f77name(cigaxg)(Tcl_GetString(Objv[2]),&xg1,&xg2,&xg3,&xg4,&ig1,&ig2,&ig3,&ig4);
         obj=Tcl_NewListObj(0,NULL);
         Tcl_ListObjAppendElement(Interp,obj,Tcl_NewDoubleObj(xg1));
         Tcl_ListObjAppendElement(Interp,obj,Tcl_NewDoubleObj(xg2));
         Tcl_ListObjAppendElement(Interp,obj,Tcl_NewDoubleObj(xg3));
         Tcl_ListObjAppendElement(Interp,obj,Tcl_NewDoubleObj(xg4));
         Tcl_SetObjResult(Interp,obj);
         break;

      case CONVIP:
         if (Objc!=3 && Objc!=4) {
            Tcl_WrongNumArgs(Interp,2,Objv,"IP | Type");
            return(TCL_ERROR);
         }
         if (Objc==3) {
            Tcl_GetIntFromObj(Interp,Objv[2],&ip);
            level=ZRef_IP2Level(ip,&kind);
            switch(kind) {
               case LVL_MASL  : sprintf(buf,"%.1f  m (Meter above sea level)",level); break;
               case LVL_SIGMA : sprintf(buf,"%.4f sg (Sigma)",level); break;
               case LVL_PRES  : sprintf(buf,"%.1f mb (Pressure)",level); break;
               case LVL_UNDEF : sprintf(buf,"%.1f  - (Undefined)",level); break;
               case LVL_MAGL  : sprintf(buf,"%.1f  m (Meter above groud level)",level); break;
               case LVL_HYBRID: sprintf(buf,"%.5f hy (Hybrid)",level); break;
               case LVL_THETA : sprintf(buf,"%.4f th (Theta)",level); break;
               case LVL_NBR   : sprintf(buf,"%.4f nb (Number)",level); break;
               case LVL_HOUR  : sprintf(buf,"%.1f hr (Hours)",level); break;
               case LVL_INT   : sprintf(buf,"%.1f i  (Integer)",level); break;
               case LVL_IDX   : sprintf(buf,"%.1f x  (Index)",level); break;
               case LVL_MPRES : sprintf(buf,"%.1f mp (Meter-Pressure)",level); break;
            }
            Tcl_SetObjResult(Interp,Tcl_NewStringObj(buf,-1));
         } else {
            if ((ip=FSTD_FieldIPGet(Interp,Objv[2],Objv[3]))==-2) {
               return(TCL_ERROR);
            }
            Tcl_SetObjResult(Interp,Tcl_NewIntObj(ip));
         }
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

int FSTD_FieldCmd(ClientData clientData,Tcl_Interp *Interp,int Objc,Tcl_Obj *CONST Objv[]){

   int           id,datev,ip1,ip2,ip3,npack,rewrite,ni,nj,nk,halo,key,n,m,k,compress=0,nid,pnid;
   unsigned long dk;
   int           i0=-1,j0=-1,i1=-1,j1=-1,npos,ok,imode=0;
   long          time;
   double        tmpd,*table=NULL;
   double        c0,c1,a,x;
   float        *index=NULL;
   char         **indexptr=NULL;
   TDef         **lutdefs=NULL;

   TObs        *obs;
   OGR_Layer   *layer;
   GDAL_Band   *band,*bandt;
   T3DModel    *model;
   Tcl_Obj     *obj;
   TRPNFile    *file;
   TData       *field0,*field1,*fieldt;
   TDef_Type   type;
   Vect3d      *pos;
#ifdef HAVE_ECBUFR
   TMetObs *metobs;
#endif
   char        *field,rmn[128];

   int         idx;

   extern const char *TDef_InterpVString[];
   extern const char *TDef_InterpRString[];

   static CONST char *types[]   = { "Unknown","Binary","UByte","Byte","UInt16","Int16","UInt32","Int32","UInt64","Int64","Float32","Float64",NULL };
   static CONST char *sopt[]   = { "version","ip1mode","autountile","hide","read","readcube","head","find","write","writegeo","writetiled","copydesc","export","create","vertical","gridinterp","verticalinterp",
                                   "timeinterp", "fromband", NULL };
   static CONST char *cbs[]   = { "REPLACE","MIN","MAX","SUM","AVERAGE",NULL };
   
   enum                opt { VERSION,IP1MODE,AUTOUNTILE,HIDE,READ,READCUBE,HEAD,FIND,WRITE,WRITEGEO,WRITETILED,COPYDESC,EXPORT,CREATE,VERTICAL,GRIDINTERP,VERTICALINTERP,TIMEINTERP,FROMBAND };

   Tcl_ResetResult(Interp);

   if (Objc<2) {
      Tcl_WrongNumArgs(Interp,1,Objv,"command ?arg arg ...?");
      return(TCL_ERROR);
   }

   if (Tcl_GetIndexFromObj(Interp,Objv[1],sopt,"command",TCL_EXACT,&idx)!=TCL_OK) {
      return(Data_FieldCmd(clientData,Interp,Objc,Objv,TD_RPN));
   }

   switch ((enum opt)idx) {

      case VERSION:
         n=1;
         f77name(rmnlib_version)(rmn,&n,127);
         rmn[89]='\0';
         Tcl_AppendResult(Interp,rmn,(char*)NULL);
         return(TCL_OK);

      case HIDE:
         if (Objc==2) {
            if (FSTD_HIDELIST)
               Tcl_SetObjResult(Interp,FSTD_HIDELIST);
         } else {
            if (FSTD_HIDELIST)
               Tcl_DecrRefCount(FSTD_HIDELIST);

//            FSTD_HIDELIST=Tcl_DuplicateObj(Objv[2]);
            FSTD_HIDELIST=Objv[2];
            if (FSTD_HIDELIST)
               Tcl_IncrRefCount(FSTD_HIDELIST);
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

      case AUTOUNTILE:
         if (Objc==2) {
            Tcl_SetObjResult(Interp,Tcl_NewBooleanObj(FSTD_UNTILE));
         } else {
            Tcl_GetBooleanFromObj(Interp,Objv[2],&FSTD_UNTILE);
         }
         break;

      case READ:
         if(Objc!=11 && Objc!=5) {
            Tcl_WrongNumArgs(Interp,2,Objv,"fld id { datev eticket ip1 ip2 ip3 typvar nomvar } | Index");
            return TCL_ERROR;
         }
         if (Objc==11) {
            /*Get the IPs*/
            if ((ip1=FSTD_FieldIPGet(Interp,Objv[6],NULL))==-2) {
               return(TCL_ERROR);
            }
            if ((ip2=FSTD_FieldIPGet(Interp,Objv[7],NULL))==-2) {
              return(TCL_ERROR);
            }
            if ((ip3=FSTD_FieldIPGet(Interp,Objv[8],NULL))==-2) {
               return(TCL_ERROR);
            }

            /*Get the bogus date*/
            TclY_Get0IntFromObj(Interp,Objv[4],&datev);

            return(FSTD_FieldRead(Interp,Tcl_GetString(Objv[2]),Tcl_GetString(Objv[3]),-1,datev,Tcl_GetString(Objv[5]),ip1,ip2,ip3,Tcl_GetString(Objv[9]),Tcl_GetString(Objv[10])));
         } else {
            Tcl_GetIntFromObj(Interp,Objv[4],&key);
            return(FSTD_FieldRead(Interp,Tcl_GetString(Objv[2]),Tcl_GetString(Objv[3]),key,-1,NULL,-1,-1,-1,NULL,NULL));
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

         /*Get the IPs*/
         if ((ip1=FSTD_FieldIPGet(Interp,Objv[5],NULL))==-2) {
            return(TCL_ERROR);
         }
         if ((ip2=FSTD_FieldIPGet(Interp,Objv[6],NULL))==-2) {
           return(TCL_ERROR);
         }
         if ((ip3=FSTD_FieldIPGet(Interp,Objv[7],NULL))==-2) {
            return(TCL_ERROR);
         }

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
         
      case WRITEGEO:
         if(Objc!=4 && Objc!=5) {
            Tcl_WrongNumArgs(Interp,2,Objv,"fld id [etiket]");
            return(TCL_ERROR);
         }
         if (!(field0=Data_Get(Tcl_GetString(Objv[2])))) {
            Tcl_AppendResult(Interp,"FSTD_FieldCmd: Invalid field (",Tcl_GetString(Objv[2]),")",(char*)NULL);
            return(TCL_ERROR);
         }
         return(FSTD_FieldWriteGeo(Interp,Tcl_GetString(Objv[3]),field0,(Objc==5?Tcl_GetString(Objv[4]):NULL)));
         break;

      case WRITETILED:
         if(Objc!=9 && Objc!=10) {
            Tcl_WrongNumArgs(Interp,2,Objv,"fld id ni nj halo npack rewrite [compress]");
            return(TCL_ERROR);
         }
         Tcl_GetIntFromObj(Interp,Objv[4],&ni);
         Tcl_GetIntFromObj(Interp,Objv[5],&nj);
         Tcl_GetIntFromObj(Interp,Objv[6],&halo);
         Tcl_GetIntFromObj(Interp,Objv[7],&npack);
         Tcl_GetBooleanFromObj(Interp,Objv[8],&rewrite);
         if (Objc==7) {
            Tcl_GetBooleanFromObj(Interp,Objv[9],&compress);
         }
         if (!(field0=Data_Get(Tcl_GetString(Objv[2])))) {
            Tcl_AppendResult(Interp,"FSTD_FieldCmd: Invalid field (",Tcl_GetString(Objv[2]),")",(char*)NULL);
            return(TCL_ERROR);
         }
         return(FSTD_FieldTile(Interp,Tcl_GetString(Objv[3]),field0,ni,nj,halo,npack,rewrite,compress));
         break;

      case COPYDESC:
         if(Objc!=4) {
            Tcl_WrongNumArgs(Interp,2,Objv,"fld id");
            return(TCL_ERROR);
         }
         if (!(field0=Data_Get(Tcl_GetString(Objv[2])))) {
            Tcl_AppendResult(Interp,"FSTD_FieldCmd: Invalid field (",Tcl_GetString(Objv[2]),")",(char*)NULL);
            return(TCL_ERROR);
         }
         if (!(file=FSTD_FileGet(Interp,Tcl_GetString(Objv[3])))) {
            return(TCL_ERROR);
         }
         FSTD_FileSet(Interp,file);
         FSTD_FileSet(Interp,((TRPNHeader*)field0->Head)->File);
         if (!RPN_CopyDesc(file->Id,field0->Head)) {
            Tcl_AppendResult(Interp,"\nFSTD_FieldCmd: Descriptor not copied, source file is not open/accessible for field ",Tcl_GetString(Objv[2]),(char*)NULL);
            return(TCL_ERROR);           
         }
         FSTD_FileUnset(Interp,((TRPNHeader*)field0->Head)->File);
         FSTD_FileUnset(Interp,file);
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
            Tcl_WrongNumArgs(Interp,2,Objv,"fld ni nj nk [type]");
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

         type=TD_Float32;

         if (Objc==7) {
            if (Tcl_GetIndexFromObj(Interp,Objv[6],types,"type",TCL_EXACT,(int*)&type)!=TCL_OK) {
               return(TCL_ERROR);
            }
         }
         if (!FSTD_FieldCreate(Interp,Tcl_GetString(Objv[2]),ni,nj,nk,type==TD_Unknown?TD_Float32:type))
            return(TCL_ERROR);
         else
            return(TCL_OK);
         break;

      case VERTICAL:
         if(Objc!=5 && Objc!=6) {
            Tcl_WrongNumArgs(Interp,2,Objv,"fld { fldfrom1 fldfrom2 ... } { lat lon lat lon ... ... } [speed/dir]");
            return TCL_ERROR;
         } else {
            TData  **fields=NULL;
            double  *lat=NULL,*lon=NULL;
            int      nbc,nbf,nc,ncc;

            Tcl_ListObjLength(Interp,Objv[4],&nbc);
            if (nbc%2!=0 || nbc==0) {
               Tcl_AppendResult(Interp,"FSTD_FieldCmd: Invalid number of coordinates",(char*)NULL);
               return(TCL_ERROR);
            }

            lat=(double*)malloc(nbc/2*sizeof(double));
            lon=(double*)malloc(nbc/2*sizeof(double));
            if (lat && lon) {
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
               
               // Default it to reproject
               nc=FALSE;
               if (Objc==6) {
                  Tcl_GetBooleanFromObj(Interp,Objv[5],&nc);
               }
               ok=Data_Cut(Interp,fields,Tcl_GetString(Objv[2]),lat,lon,nbf,nbc/2,nc);
            } else {
               Tcl_AppendResult(Interp,"FSTD_FieldCmd: Unable to allocate memory for temporary buffer",(char*)NULL);
               return(TCL_ERROR);
            }


            if (lat) free(lat);
            if (lon) free(lon);
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
         if (field0->GRef && field0->GRef->Type|GRID_SPARSE)
            FSTD_FieldReadMesh(field0);

         // Process each sub grids independently
         pnid=field0->GRef->NId;
         ok=TCL_OK;

         for(nid=(pnid?pnid:(field0->GRef->NbId>1?1:0));nid<=(pnid?pnid:(field0->GRef->NbId>1?field0->GRef->NbId:0));nid++) {
            FSTD_FieldSubSelect(field0,nid);

            imode=-1;
            // Interpolate a field
            if ((field1=Data_Get(Tcl_GetString(Objv[3])))) {
               if (Objc>4) {
                  if (Tcl_GetIndexFromObj(Interp,Objv[4],TDef_InterpRString,"mode",TCL_EXACT,&imode)!=TCL_OK) {
                     ok=TCL_ERROR; break;
                  }
               }

               if (field1->GRef && field1->GRef->Type|GRID_SPARSE) {
                  FSTD_FieldReadMesh(field1);
               }

               // If grids are the same and this is not a NOP,ACCUM or BUFFER call
               if (imode<IR_NOP && field0->GRef->Ids && field1->GRef->Ids && field0->GRef->Ids[field0->GRef->NId]==field1->GRef->Ids[field1->GRef->NId]) {
                  if (!Data_Copy(Interp,field1,Tcl_GetString(Objv[2]),1,0)) {
                     ok=TCL_ERROR; break;
                  } else {
                     continue;
                  }
               }

               if (imode==IR_CONSERVATIVE || imode==IR_NORMALIZED_CONSERVATIVE) {
                  if (Objc>8 || Objc<6) {
                     Tcl_WrongNumArgs(Interp,2,Objv,"fldto fldfrom Type Split [Final] [Index variable]");
                     ok=TCL_ERROR; break;
                  }
                  if (Tcl_GetIntFromObj(Interp,Objv[5],&ni)==TCL_ERROR) {
                     ok=TCL_ERROR; break;
                  }
                  nj=1;
                  obj=NULL;
                  index=NULL;

                  if (Objc>6) {
                     if (Tcl_GetBooleanFromObj(Interp,Objv[6],&nj)==TCL_ERROR) {
                        obj=Objv[6];
                     }
                     Tcl_ResetResult(Interp);
                  }
                  if (Objc>7) {
                     obj=Objv[7];
                  }
                  // Check for index array
                  index=Data_IndexInit(Interp,&obj,field0->Def->NIJ*1024);

                  // Check compatibility between source and destination
                  if (!Def_Compat(field0->Def,field1->Def)) {
                     field0->GRef=GeoRef_Find(GeoRef_Resize(field0->GRef,field0->Def->NI,field0->Def->NJ));
                  }
                  ZRef_Free(field0->ZRef);
                  field0->ZRef=ZRef_Define(field1->ZRef->Type,field1->ZRef->LevelNb,field1->ZRef->Levels);
                  FSTD_FieldSetTo(field0,field1);

                  if (!(nk=Def_GridInterpConservative(field0->GRef,field0->Def,field1->GRef,field1->Def,Tcl_GetString(Objv[4])[0],nj,ni,index))) {
                     Tcl_AppendResult(Interp,App_ErrorGet(),(char*)NULL);
                     ok=TCL_ERROR;
                     break;
                  }

                  // Make index object persistent and of the right size
                  Data_IndexResize(Interp,&obj,nk);

               } else if (imode>=IR_MAXIMUM && imode<=IR_BUFFER) {
                  int nblut=0;
                  if (Objc<5 || Objc>7) {
                     Tcl_WrongNumArgs(Interp,2,Objv,"fldto fldfrom [Type] [Values] [Final]");
                     ok=TCL_ERROR; break;
                  }

                  ni=1;
                  fieldt=NULL;
                  if (Objc>5) {
                     if (Tcl_GetBooleanFromObj(Interp,Objv[5],&ni)==TCL_ERROR) {
                        if (!(fieldt=Data_Get(Tcl_GetString(Objv[5])))) {
                           TVector       *vec;
                           if ((vec=Vector_Get(Tcl_GetString(Objv[5])))) {
                              lutdefs = Vector_GetCompDefs( vec );
                              nblut = vec->N;
                              nk = vec->Cp[0]->V[0];
                              if ( Vector_ValidateLUT( Interp , vec )==TCL_ERROR) {
                                 ok=TCL_ERROR; break;
                              }
                           } else {
                              Tcl_ListObjLength(Interp,Objv[5],&nk);
                              if (!table && !(table=(double*)malloc(nk*sizeof(double)))) {
                                 Tcl_AppendResult(Interp,"FSTD_FieldCmd: Unable to allocate memory for temporary buffer",(char*)NULL);
                                 ok=TCL_ERROR; break;
                              }
                              for(k=0;k<nk;k++) {
                                 Tcl_ListObjIndex(Interp,Objv[5],k,&obj);
                                 Tcl_GetDoubleFromObj(Interp,obj,&table[k]);
                              }
                          }
                          if (nk!=field0->Def->NK) {
                              field0->Def=Def_Resize(field0->Def,field0->Def->NI,field0->Def->NJ,nk);
                              for(key=0;key<FSIZE3D(field0->Def);key++) {
                                 Def_Set(field0->Def,0,key,0);
                              }
                              ZRef_Free(field0->ZRef);
                              field0->ZRef=ZRef_Define(field0->ZRef->Type,nk,field0->ZRef->Levels);
                           }
                        }
                     }
                  }
                  if(Objc==7) {
                     Tcl_GetBooleanFromObj(Interp,Objv[6],&ni);
                  }
                  if (!Def_GridInterpAverage(field0->GRef,field0->Def,field1->GRef,field1->Def,table,lutdefs,nblut,fieldt?fieldt->Def:NULL,imode,ni)) {
                     Tcl_AppendResult(Interp,App_ErrorGet(),(char*)NULL);
                     ok=TCL_ERROR;
                     break;
                  }

                  if (lutdefs) free( lutdefs );
                  lutdefs = NULL;
               } else if (imode==IR_SUBNEAREST || imode==IR_SUBLINEAR) {
                  if (Objc<6) {
                     Tcl_WrongNumArgs(Interp,2,Objv,"fldto fldfrom Type Sampling");
                     ok=TCL_ERROR; break;
                  }
                  if (Tcl_GetIntFromObj(Interp,Objv[5],&ni)==TCL_ERROR) {
                     ok=TCL_ERROR; break;
                  }
                  if (ni!=field0->Def->SubSample && field0->Def->Sub) {
                     free(field0->Def->Sub); field0->Def->Sub=NULL;
                  }
                  field0->Def->SubSample=ni;
                  if (!Def_GridInterpSub(field0->GRef,field0->Def,field1->GRef,field1->Def,IR_SUBNEAREST?'N':'L')) {
                     Tcl_AppendResult(Interp,App_ErrorGet(),(char*)NULL);
                     ok=TCL_ERROR;
                     break;
                  }
               } else {
                
                  // Check for index array
                  index=NULL;
                  if (Objc==6) {
                     // Check for index array
                     obj=Objv[5];
                     index=Data_IndexInit(Interp,&obj,field0->Def->NIJ*2);
                   }
                   ok=FSTD_FieldGridInterpolate(Interp,field0,field1,imode,index);
               }
               if (ok==TCL_ERROR) break;
            } else if ((band=GDAL_BandGet(Tcl_GetString(Objv[3])))) {

               // Interpolate a band
               if (Objc>4) {
                  if (Tcl_GetIndexFromObj(Interp,Objv[4],TDef_InterpRString,"mode",TCL_EXACT,&imode)!=TCL_OK) {
                     ok=TCL_ERROR; break;
                  }
               }

               if (imode==IR_CONSERVATIVE || imode==IR_NORMALIZED_CONSERVATIVE) {
                  if (Objc>8 || Objc<6) {
                     Tcl_WrongNumArgs(Interp,2,Objv,"fldto bandfrom [Type] [Split] [Final] [Index variable]");
                     ok=TCL_ERROR; break;
                  }
                  if (Tcl_GetIntFromObj(Interp,Objv[5],&ni)==TCL_ERROR) {
                     ok=TCL_ERROR; break;
                  }
                  nj=1;
                  obj=NULL;
                  index=NULL;

                  if (Objc>6) {
                     if (Tcl_GetBooleanFromObj(Interp,Objv[6],&nj)==TCL_ERROR) {
                        obj=Objv[6];
                     }
                  }

                  if (Objc>7) {
                     obj=Objv[7];
                  }

                  // Check for index array
                  index=Data_IndexInit(Interp,&obj,field0->Def->NIJ*1024);

                  if (!(nk=Def_GridInterpConservative(field0->GRef,field0->Def,band->GRef,band->Def,Tcl_GetString(Objv[4])[0],nj,ni,index))) {
                     Tcl_AppendResult(Interp,App_ErrorGet(),(char*)NULL);
                     ok=TCL_ERROR;
                     break;
                  }

                  // Make index object persistent and of the right size
                  Data_IndexResize(Interp,&obj,nk);
                  
               } else if (imode>=IR_MAXIMUM && imode<=IR_BUFFER) {
                  int  nblut=0;
                  if (Objc<5 || Objc>7) {
                     Tcl_WrongNumArgs(Interp,2,Objv,"fldto bandfrom [Type] [Values] [Final]");
                     ok=TCL_ERROR; break;
                  }
                  ni=1;
                  bandt=NULL;
                  if (Objc>5) {
                     if (Tcl_GetBooleanFromObj(Interp,Objv[5],&ni)==TCL_ERROR) {
                        if (!(bandt=GDAL_BandGet(Tcl_GetString(Objv[5])))) {
                           TVector       *vec;
                           if ((vec=Vector_Get(Tcl_GetString(Objv[5])))) {
                              lutdefs = Vector_GetCompDefs( vec );
                              nblut = vec->N;
                              nk = vec->Cp[0]->V[0];
                              if ( Vector_ValidateLUT( Interp , vec )==TCL_ERROR) {
                                 ok=TCL_ERROR; break;
                              }
                           } else {
                              Tcl_ListObjLength(Interp,Objv[5],&nk);
                              if (!table && !(table=(double*)malloc((nk+1)*sizeof(double)))) {
                                 Tcl_AppendResult(Interp,"FSTD_FieldCmd: Unable to allocate memory for temporary buffer",(char*)NULL);
                                 ok=TCL_ERROR; break;
                              }
                              for(k=0;k<nk;k++) {
                                 Tcl_ListObjIndex(Interp,Objv[5],k,&obj);
                                 Tcl_GetDoubleFromObj(Interp,obj,&table[k]);
                              }
                              table[k]=field0->Def->NoData;
                           }
                           if (nk!=field0->Def->NK) {
                              if (!(field0->Def=Def_Resize(field0->Def,field0->Def->NI,field0->Def->NJ,nk))) {
                                 Tcl_AppendResult(Interp,"Unable to rellocate buffer",(char*)NULL);
                                 ok=TCL_ERROR; break;
                              }

                              for(dk=0;dk<FSIZE3D(field0->Def);dk++) {
                                 Def_Set(field0->Def,0,dk,0);
                              }
                              ZRef_Free(field0->ZRef);
                              field0->ZRef=ZRef_Define(field0->ZRef->Type,nk,field0->ZRef->Levels);
                           }
                        }
                     }
                  }
                  if (Objc==7) {
                     Tcl_GetBooleanFromObj(Interp,Objv[6],&ni);
                  }
                  if (!Def_GridInterpAverage(field0->GRef,field0->Def,band->GRef,band->Def,table,lutdefs,nblut,bandt?bandt->Def:NULL,imode,ni)) {
                     Tcl_AppendResult(Interp,App_ErrorGet(),(char*)NULL);
                     ok=TCL_ERROR;
                     break;
                  }
                  if (lutdefs) free( lutdefs );
                  lutdefs = NULL;
               } else if (imode==IR_SUBNEAREST || imode==IR_SUBLINEAR) {
                  if (Objc<6) {
                     Tcl_WrongNumArgs(Interp,2,Objv,"fldto bandfrom Type Sampling");
                     ok=TCL_ERROR; break;
                  }
                  if (Tcl_GetIntFromObj(Interp,Objv[5],&ni)==TCL_ERROR) {
                     ok=TCL_ERROR; break;
                  }
                  if (ni!=field0->Def->SubSample && field0->Def->Sub) {
                     free(field0->Def->Sub); field0->Def->Sub=NULL;
                  }
                  field0->Def->SubSample=ni;
                  if (!Def_GridInterpSub(field0->GRef,field0->Def,band->GRef,band->Def,IR_SUBNEAREST?'N':'L')) {
                     Tcl_AppendResult(Interp,App_ErrorGet(),(char*)NULL);
                     ok=TCL_ERROR;
                     break;
                  }
               } else {
                  if (!Def_GridInterp(field0->GRef,field0->Def,band->GRef,band->Def,Objv[4]?Tcl_GetString(Objv[4])[0]:'L')) {
                     Tcl_AppendResult(Interp,App_ErrorGet(),(char*)NULL);
                     ok=TCL_ERROR;
                     break;
                  }
               }
               if (ok==TCL_ERROR) break;
            } else if ((layer=OGR_LayerGet(Tcl_GetString(Objv[3])))) {

               /*Interpolate a layer*/
               if (Objc!=5 && Objc!=6 && Objc!=7 && Objc!=8) {
                  Tcl_WrongNumArgs(Interp,2,Objv,"field layer mode [field] [index] [REPLACE|MIN|MAX|SUM|AVERAGE]");
                  ok=TCL_ERROR; break;
               }
               if (Tcl_GetIndexFromObj(Interp,Objv[4],TDef_InterpVString,"mode",TCL_EXACT,&imode)!=TCL_OK) {
                  ok=TCL_ERROR; break;
               }

               field=NULL;
               obj=NULL;
               x=1.0;
               m=CB_REPLACE;

               if (Objc>=6) {
                  if (Tcl_GetDoubleFromObj(Interp,Objv[5],&x)==TCL_ERROR) {
                     field=Tcl_GetString(Objv[5]);
                  }
               }

               if (Objc==7 && strlen(Tcl_GetString(Objv[6]))) {
                  obj=Objv[6];
               }

               if (Objc==8) {
                  if (Tcl_GetIndexFromObj(Interp,Objv[7],cbs,"combine",TCL_EXACT,(int*)&m)!=TCL_OK) {
                     return(TCL_ERROR);
                  }
               }
               
               // Check for index array
               index=Data_IndexInit(Interp,&obj,field0->Def->NIJ*100);
               if (!(nk=Def_GridInterpOGR(field0->Def,field0->GRef,layer,layer->GRef,imode,1,field,x,m,index))) {
                  Tcl_AppendResult(Interp,App_ErrorGet(),(char*)NULL);
                  ok=TCL_ERROR;
                  break;
               }

               // Make index object persistent and of the right size
               Data_IndexResize(Interp,&obj,nk);

            } else if ((model=Model_Get(Tcl_GetString(Objv[3])))) {

               /*Interpolate a model*/
               if (Objc!=5 && Objc!=6) {
                  Tcl_WrongNumArgs(Interp,2,Objv,"field model mode [field]");
                  ok=TCL_ERROR; break;
               }
               if (Tcl_GetIndexFromObj(Interp,Objv[4],TDef_InterpVString,"mode",TCL_EXACT,&imode)!=TCL_OK) {
                  ok=TCL_ERROR; break;
               }
               x=1.0;

               if (Objc==6) {
                  if (Tcl_GetDoubleFromObj(Interp,Objv[5],&x)==TCL_ERROR) {
                     field=Tcl_GetString(Objv[5]);
                  }
               }
               ok=Model_Grid(Interp,field0,model,NULL,imode);
               if (ok==TCL_ERROR) break;
            } else if ((obs=Obs_Get(Tcl_GetString(Objv[3])))) {

               /*Interpolate an observation*/
               if(Objc!=8 && Objc!=9) {
                  Tcl_WrongNumArgs(Interp,2,Objv,"fldto obsfrom [SPHERICAL | EXPONENTIAL | GAUSSIAN | LINEAR] Nugget(C0) Sill(C1) Range(A) [Outside]");
                  ok=TCL_ERROR; break;
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
               if ((pos=Obs_Grid(field0->GRef,field0->ZRef,obs,&npos,n))) {
                  ok=FFKrigging(field0->GRef,field0->Def,pos,npos,c0,c1,a,id);
                  free(pos);
               } else {
                  Tcl_AppendResult(Interp,"Unable to calculate position vector",(char*)NULL);
                  ok=TCL_ERROR; break;
               }

               if (!ok) {
                  Tcl_AppendResult(Interp,"Obs krigging failed",(char*)NULL);
                  ok=TCL_ERROR; break;
               }
               if (field0->Stat) { Data_StatFree(field0->Stat); field0->Stat=NULL; }
               ok=TCL_OK;
#ifdef HAVE_ECBUFR
            } else if ((metobs=MetObs_Get(Tcl_GetString(Objv[3])))) {

              /*Interpolate a metobs*/
               if(Objc!=10 && Objc!=11) {
                  Tcl_WrongNumArgs(Interp,2,Objv,"fldto metobsfrom time element [SPHERICAL | EXPONENTIAL | GAUSSIAN | LINEAR] Nugget(C0) Sill(C1) Range(A) [Outside]");
                  ok=TCL_ERROR; break;
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


               if ((pos=MetObs_Grid(Interp,field0->GRef,metobs,time,Objv[5],&npos,n))) {
                  ok=FFKrigging(field0->GRef,field0->Def,pos,npos,c0,c1,a,id);
                  free(pos);
               } else {
                  Tcl_AppendResult(Interp,"Unable to calculate position vector",(char*)NULL);
                  ok=TCL_ERROR; break;
               }

               if (!ok) {
                  Tcl_AppendResult(Interp,"MetObs krigging failed",(char*)NULL);
                  ok=TCL_ERROR; break;
               }
               if (field0->Stat) { Data_StatFree(field0->Stat); field0->Stat=NULL; }
               ok=TCL_OK;
#endif
            } else {
               /* If we get here, it has to be a NOP or ACCUM*/
               if (Objc>4) {
                  if (Tcl_GetIndexFromObj(Interp,Objv[4],TDef_InterpRString,"mode",TCL_EXACT,&n)!=TCL_OK) {
                     ok=TCL_ERROR; break;
                  }
               }
               if (n==IR_NOP || n==IR_ACCUM || n==IR_BUFFER) {
                  if (!Def_GridInterpAverage(field0->GRef,field0->Def,NULL,NULL,NULL,NULL,0,NULL,n,1)) {
                     Tcl_AppendResult(Interp,App_ErrorGet(),(char*)NULL);
                     ok=TCL_ERROR;
                     break;
                  }
               } else {
                  Tcl_AppendResult(Interp,"invalid data type",(char*)NULL);
                  ok=TCL_ERROR; break;
               }
            }
         }
         if (table) free(table);

         FSTD_FieldSubSelect(field0,pnid);
         return(ok);
         break;

      case VERTICALINTERP:
         if(Objc!=6 && Objc!=7) {
            Tcl_WrongNumArgs(Interp,2,Objv,"fldto fldfrom [GZTo | P0To] [GZFrom | P0From] [Index]");
            return TCL_ERROR;
         }
         
         // Check for index array
         indexptr=NULL;
         if (Objc==7) {
            obj=Objv[6];
            indexptr=Data_IndexInitPtr(Interp,&obj);
         }

         return(FSTD_FieldVertInterpolate(Interp,Data_Get(Tcl_GetString(Objv[2])),Data_Get(Tcl_GetString(Objv[3])),Data_Get(Tcl_GetString(Objv[4])),Data_Get(Tcl_GetString(Objv[5])),indexptr));
         break;

      case TIMEINTERP:
         if(Objc!=6) {
            Tcl_WrongNumArgs(Interp,2,Objv,"fldto fld0 fld1 stamp");
            return TCL_ERROR;
         }
         Tcl_GetIntFromObj(Interp,Objv[5],&id);
         return(FSTD_FieldTimeInterpolate(Interp,id,Tcl_GetString(Objv[2]),Data_Get(Tcl_GetString(Objv[3])),Data_Get(Tcl_GetString(Objv[4]))));
         break;

      case FROMBAND :
         if (Objc!=6) {
            Tcl_WrongNumArgs(Interp,2,Objv,"fldto bandfrom bandIJ Type");
            return(TCL_ERROR);
         } else {
            int  mode=0;
            field0=Data_Get(Tcl_GetString(Objv[2]));
            if (!field0) {
               Tcl_AppendResult(Interp,"invalid field: ",Tcl_GetString(Objv[2]),(char*)NULL);
               return(TCL_ERROR);
            }

            bandt=GDAL_BandGet(Tcl_GetString(Objv[3]));
            band=GDAL_BandGet(Tcl_GetString(Objv[4]));
            if (!bandt) {
               Tcl_AppendResult(Interp,"invalid band: ",Tcl_GetString(Objv[3]),(char*)NULL);
               return(TCL_ERROR);
            }
            if (!band) {
               Tcl_AppendResult(Interp,"invalid band: ",Tcl_GetString(Objv[4]),(char*)NULL);
               return(TCL_ERROR);
            }

            if (strcmp(Tcl_GetString(Objv[5]), "MINIMUM") == 0)
               mode = -1;
            else if (strcmp(Tcl_GetString(Objv[5]), "MAXIMUM") == 0)
               mode = 1;
            else
               mode = 0;

            GDAL_BandToFieldWithPos(Interp,field0,bandt,band, mode );
         }
         break;
   }

   return(TCL_OK);
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

   int       n,idx,itype,type;
   TRPNFile *file=NULL;

   static CONST char *types[] = { "NONE","SPI","ALL","NOMVAR","TYPVAR","DATEV","LEVEL","IP1","IP2","IP3","ETIKET","IG1","IG2","IG3","IG4","EXTENDED" };
   static CONST char *sopt[] = { "is","open","close","flush","link","unlink","filename","mode","info",NULL };
   enum               opt { IS,OPEN,CLOSE,FLUSH,LINK,UNLINK,FILENAME,MODE,INFO };

   Tcl_ResetResult(Interp);

   if (Objc<2) {
      Tcl_WrongNumArgs(Interp,1,Objv,"command ?arg arg ...?");
      return(TCL_ERROR);
   }

   if (Tcl_GetIndexFromObj(Interp,Objv[1],sopt,"command",TCL_EXACT,&idx)!=TCL_OK) {
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
         if(Objc!=5 && Objc!=6) {
            Tcl_WrongNumArgs(Interp,2,Objv,"id mode filename [info]");
            return(TCL_ERROR);
         }
         itype=2;

         if(Objc==6) {
            if (Tcl_GetIndexFromObj(Interp,Objv[5],types,"type",TCL_EXACT,&itype)!=TCL_OK) {
               return(TCL_ERROR);
            }
         }
         return(FSTD_FileOpen(Interp,Tcl_GetString(Objv[2]),Tcl_GetString(Objv[3])[0],Tcl_GetString(Objv[4]),itype));
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

      case FLUSH:
         if(Objc<3) {
            Tcl_WrongNumArgs(Interp,2,Objv,"fileid");
            return(TCL_ERROR);
         }
         if (!(file=FSTD_FileGet(Interp,Tcl_GetString(Objv[2])))) {
            return(TCL_ERROR);
         }
         if (file->Open)
            cs_fstflush(file->Id);
         return(TCL_OK);
         break;
         
      case LINK:
         if(Objc!=3) {
            Tcl_WrongNumArgs(Interp,2,Objv,"fileids");
            return(TCL_ERROR);
         }

         return(FSTD_FileLink(Interp,Objv[2]));
         break;

      case UNLINK:
         if(Objc!=3) {
            Tcl_WrongNumArgs(Interp,2,Objv,"fileids");
            return(TCL_ERROR);
         }

         return(FSTD_FileUnLink(Interp,Objv[2]));
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

      case MODE:
         if(Objc!=3) {
            Tcl_WrongNumArgs(Interp,2,Objv,"id");
            return(TCL_ERROR);
         }
         if (!(file=FSTD_FileGet(Interp,Tcl_GetString(Objv[2])))) {
            return(TCL_ERROR);
         }
         Tcl_SetObjResult(Interp,Tcl_NewStringObj(&file->Mode,1));
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
         if (Tcl_GetIndexFromObj(Interp,Objv[3],types,"type",TCL_EXACT,&itype)!=TCL_OK) {
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
extern int Data_UnlinkFSTDFile(TRPNFile *File);

int FSTD_FileClose(Tcl_Interp *Interp,char *Id) {

   TRPNFile *file;
   
   if ((file=(TRPNFile*)TclY_HashDel(&FSTD_FileTable,Id))) {
      file->Open=file->Open>0?1:file->Open;
      FSTD_FileUnset(Interp,file);
      cs_fstunlockid(file->Id);

      // Nullify file link of field using this file
      Data_UnlinkFSTDFile(file);
     
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
 *  <Index>   : Format de l'index
 *
 * Retour:
 *  <TCL_...> : Code d'erreur de TCL.
 *  <tcl>     : Liste des parametres des enregistrements contenue dans le fichier
 *
 * Remarques :
 *
 *----------------------------------------------------------------------------
*/
int FSTD_FileOpen(Tcl_Interp *Interp,char *Id,char Mode,char *Name,int Index){

   Tcl_HashEntry *entry;
   TRPNFile      *file;
   int            new;

   /* Creer l'entree dans la liste table de fichiers standards */
   entry=TclY_CreateHashEntry(&FSTD_FileTable,Id,&new);
   if (!new) {
      Tcl_AppendResult(Interp,"FSTD_FileOpen: File already opened, cannot reuse openned file identificator ",Id,(char*)NULL);
      return(TCL_ERROR);
   }

   if (!(file=(TRPNFile*)malloc(sizeof(TRPNFile)))) {
      Tcl_AppendResult(Interp,"FSTD_FileOpen: Unable to allocate memory for file structure",Id,(char*)NULL);
      return(TCL_ERROR);
   }

   file->Mode=toupper(Mode);
   file->CId=strdup(Id);
   file->Id=cs_fstlockid();
   file->Open=0;
   file->NRef=1;
   file->Name=strdup(Name);

   Tcl_SetHashValue(entry,file);

   return(FSTD_FieldList(Interp,file,Index,NULL));
}

/*----------------------------------------------------------------------------
 * Nom      : <FSTD_FileLink>
 * Creation : Novembre 2013 - J.P. Gauthier - CMC/CMOE
 *
 * But      : Link multiple standard files as one.
 *
 * Parametres :
 *  <Interp>  : Interpreteur TCL.
 *  <Ids>     : Identificateur a donner au fichier
 *
 * Retour:
 *  <TCL_...> : Code d'erreur de TCL.
 *
 * Remarques :
 *
 *----------------------------------------------------------------------------
*/
int FSTD_FileLink(Tcl_Interp *Interp,Tcl_Obj *Ids){

   TRPNFile *file;
   Tcl_Obj  *obj;
   int       ier,n,nobj,*ids;

   Tcl_ListObjLength(Interp,Ids,&nobj);

   if (nobj) {
      if (!(ids=(int*)malloc(nobj*sizeof(int)))) {
         Tcl_AppendResult(Interp,"FSTD_FileLink: Unable to allocate link array",(char*)NULL);
         return(TCL_ERROR);
      }

      // Build file table
      for(n=0;n<nobj;n++) {
         Tcl_ListObjIndex(Interp,Ids,n,&obj);
         if (!(file=FSTD_FileGet(Interp,Tcl_GetString(obj)))) {
            return(TCL_ERROR);
         }
         // First file will be the file id to use
         if (n==0)
            Tcl_SetObjResult(Interp,Tcl_NewStringObj(file->CId,-1));

         FSTD_FileSet(Interp,file);
         ids[n]=file->Id;
      }

      ier=f77name(fstlnk)(ids,&n);
      if (ier<0) {
         Tcl_AppendResult(Interp,"FSTD_FieldRead: Could not link files (c_fstlnk failed)",(char*)NULL);
         return(TCL_ERROR);
      }
   }

   return(TCL_OK);
}

/*----------------------------------------------------------------------------
 * Nom      : <FSTD_FileUnLink>
 * Creation : Novembre 2013 - J.P. Gauthier - CMC/CMOE
 *
 * But      : Unlink multiple standard files.
 *
 * Parametres :
 *  <Interp>  : Interpreteur TCL.
 *  <Ids>     : Identificateur a donner au fichier
 *
 * Retour:
 *  <TCL_...> : Code d'erreur de TCL.
 *
 * Remarques :
 *
 *----------------------------------------------------------------------------
*/
int FSTD_FileUnLink(Tcl_Interp *Interp,Tcl_Obj *Ids){

   TRPNFile *file=NULL;
   Tcl_Obj  *obj;
   int       ier,n,nobj,*ids;

   Tcl_ListObjLength(Interp,Ids,&nobj);

   if (nobj) {
      if (!(ids=(int*)malloc(nobj*sizeof(int)))) {
         Tcl_AppendResult(Interp,"FSTD_FileLink: Unable to allocate link array",(char*)NULL);
         return(TCL_ERROR);
      }

      for(n=0;n<nobj;n++) {
         Tcl_ListObjIndex(Interp,Ids,n,&obj);
         if (!(file=FSTD_FileGet(Interp,Tcl_GetString(obj)))) {
            return(TCL_ERROR);
         }
         ids[n]=file->Id;
      }

      ier=f77name(fstunl)(ids,&n);
      if (ier<0) {
         Tcl_AppendResult(Interp,"FSTD_FieldRead: Could not unlink files (c_fstunlnk failed)",(char*)NULL);
         return(TCL_ERROR);
      }

      for(n=0;n<nobj;n++) {
         Tcl_ListObjIndex(Interp,Ids,n,&obj);
         FSTD_FileUnset(Interp,file);
      }
   }

   return(TCL_OK);
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
TRPNFile* FSTD_FileGet(Tcl_Interp *Interp,char *Id){

   Tcl_HashEntry *entry;

   entry=TclY_FindHashEntry(&FSTD_FileTable,Id);
   if (!entry) {
      if (Interp) Tcl_AppendResult(Interp,"FSTD_FileGet: Unknown file, ",Id,(char *)NULL);
      return(NULL);
   }

   return((TRPNFile*)(Tcl_GetHashValue(entry)));
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
int FSTD_FileSet(Tcl_Interp *Interp,TRPNFile *File){

   int ok=0,rem=0;
   char err[8];

   if (!File) {
      if (Interp) Tcl_AppendResult(Interp,"FSTD_FileSet: Unknown file (NULL)",(char *)NULL);
      return(-1);
   }

   if (index(File->Name,':') && File->Name[0]!=':') {
      rem=1;
   }

   RPN_FileLock();
   if (!File->Open) {

      if (File->Mode=='W') {                /*Write Mode*/
         if (rem)
            ok=c_fnom(&File->Id,File->Name,"STD+RND+R/W+REMOTE",0);
         else
            ok=c_fnom(&File->Id,File->Name,"STD+RND+R/W",0);
      } else if (File->Mode=='A') {         /*Append Mode*/
         if (rem)
            ok=c_fnom(&File->Id,File->Name,"STD+RND+OLD+R/W+REMOTE",0);
         else
            ok=c_fnom(&File->Id,File->Name,"STD+RND+OLD+R/W",0);
      } else {                                                  /*ReadOnly Mode*/
         if (rem) {
            ok=c_fnom(&File->Id,File->Name,"STD+RND+R/O+REMOTE",0);
         } else {
            // Make sure the file is an RPN one, otherwise we get fnom/fstouv problems later
            ok=f77name(wkoffit)(File->Name,strlen(File->Name));
            if (ok!=1 && ok!=2 && ok!=3 && ok!=33 && ok!=34) {
               if (Interp) Tcl_AppendResult(Interp,"FSTD_FileSet: File is not RPN, ",File->Name,(char*)NULL);
               RPN_FileUnlock();
               return(-1);
            }
            ok=c_fnom(&File->Id,File->Name,"STD+RND+R/O",0);
         }
      }
      
      if (ok<0) {
         snprintf(err,8,"%i",ok);
         if (Interp) Tcl_AppendResult(Interp,"FSTD_FileSet: Unable to link standard file name, ",File->Name," (c_fnom failed = ",err,")",(char *)NULL);
         RPN_FileUnlock();
         File->Open++;
         return(-1);
      }

      ok=c_fstouv(File->Id,"RND");
      if (ok<0) {
         // We should close the fid but c_fstouv keeps something opened and fstfrm blows up.
         // ok=c_fclos(File->Id);
         snprintf(err,8,"%i",ok);
         if (Interp) Tcl_AppendResult(Interp,"FSTD_FileSet: Unable to open standard file, ",File->Name," (c_fstouv failed = ",err,")",(char *)NULL);
         RPN_FileUnlock();
         File->Open++;
         return(-1);
      }
   }

   File->Open++;
   RPN_FileUnlock();

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
int FSTD_FileUnset(Tcl_Interp *Interp,TRPNFile *File) {

   int ok=-1;

   if (!File)
      return(0);

   RPN_FileLock();
   File->Open--;

   if (!File->Open) {
      ok=c_fstfrm(File->Id);
      ok=c_fclos(File->Id);

      if (ok>=0){
         ok=1;
      } else {
         if (Interp) Tcl_AppendResult(Interp,"FSTD_FileUnset: Could not close file (c_fstfrm and/or c_fclos failed)",(char *)NULL);
         ok=0;
      }
   }

   RPN_FileUnlock();
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

   extern time_t timezone;

   Tcl_ResetResult(Interp);

   if (Objc<2) {
      Tcl_WrongNumArgs(Interp,1,Objv,"command ?arg arg ...?");
      return TCL_ERROR;
   }

   if (Tcl_GetIndexFromObj(Interp,Objv[1],sopt,"command",TCL_EXACT,&idx)!=TCL_OK) {
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

            pkey=(pkey1>>nkey1)+(pkey2<<nkey2);
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
         TclY_Get0DoubleFromObj(Interp,Objv[3],&tmpd);
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

   return(TCL_OK);
}

static int FSTD_DictCmd (ClientData clientData,Tcl_Interp *Interp,int Objc,Tcl_Obj *CONST Objv[]);
static Tcl_Obj *FSTD_DictVarInfo(Tcl_Interp *Interp,TDictVar *Var,int Objc,Tcl_Obj *CONST Objv[]);
static int FSTD_DictTypeInfo(Tcl_Interp *Interp,TDictType *Var,int Objc,Tcl_Obj *CONST Objv[]);

/*----------------------------------------------------------------------------
 * Nom      : <FSTD_DictCmd>
 * Creation : Mai 2014 2001 - J.P. Gauthier - CMC/CMOE
 *
 * But      : Appel des commandes relatives aux manipulations du dictionnaire
 *            des variables.
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
static int FSTD_DictCmd (ClientData clientData,Tcl_Interp *Interp,int Objc,Tcl_Obj *CONST Objv[]){

   Tcl_Obj   *obj;
   TList     *iter=NULL;
   TDictVar  *var,*mod;
   TDictType *type;
   char       file[2048],*str,*origin,*etiket,*modifier;
   int        ip1,ip3,all=0;

   int         idx,tidx;
   static CONST char *sopt[] = { "load","var","type","isvar","istype","varinfo","typeinfo",NULL };
   enum                opt { LOAD,VAR,TYPE,ISVAR,ISTYPE,VARINFO,TYPEINFO };

   Tcl_ResetResult(Interp);

   Dict_SetSearch(DICT_GLOB,DICT_ALL,NULL,-1,-1,-1,NULL);

   if (Objc<2) {
      Tcl_WrongNumArgs(Interp,1,Objv,"command ?arg arg ...?");
      return TCL_ERROR;
   }

   if (Tcl_GetIndexFromObj(Interp,Objv[1],sopt,"command",TCL_EXACT,&idx)!=TCL_OK) {
      return TCL_ERROR;
   }

   switch ((enum opt)idx) {


      case LOAD:

         if (Objc!=2 && Objc!=3) {
            Tcl_WrongNumArgs(Interp,2,Objv,"[dictfile]");
            return(TCL_ERROR);
         }

         if (Objc>2) {
            if (!Dict_Parse(Tcl_GetString(Objv[2]),DICT_UTF8)) {
               Tcl_AppendResult(Interp,"FSTD_DictCmd: Problems loading dictionnary",(char*)NULL);
               return(TCL_ERROR);
            }
         } else {
            sprintf(file,"%s%s",getenv("AFSISIO"),"/datafiles/constants/ops.variable_dictionary.xml");
            if (!Dict_Parse(file,DICT_UTF8)) {
               Tcl_AppendResult(Interp,"FSTD_DictCmd: Problems loading dictionnary",(char*)NULL);
               return(TCL_ERROR);
            }
         }
         break;

      case VAR:
         if (Objc<2) {
            Tcl_WrongNumArgs(Interp,2,Objv,"[var] [-searchip1] [-searchip3] [-serachorigin]");
            return(TCL_ERROR);
         }

         str=NULL;

         if (Objc>2) {
            tidx=2;
            if (Tcl_GetString(Objv[2])[0]!='-') {
              str=Tcl_GetString(Objv[tidx++]);
            }

            ip1=ip3=-1;
            origin=etiket=NULL;
            for(;tidx<Objc;tidx++) {
               if (tidx+1<Objc && Tcl_GetString(Objv[tidx+1])[0]!='-') {
                  if (!strcmp(Tcl_GetString(Objv[tidx]),"-searchip1")) {
                     Tcl_GetIntFromObj(Interp,Objv[++tidx],&ip1);
                  }
                  if (!strcmp(Tcl_GetString(Objv[tidx]),"-searchip3")) {
                     Tcl_GetIntFromObj(Interp,Objv[++tidx],&ip3);
                  }
                  if (!strcmp(Tcl_GetString(Objv[tidx]),"-searchetiket")) {
                     etiket=Tcl_GetString(Objv[++tidx]);
                  }
                  if (!strcmp(Tcl_GetString(Objv[tidx]),"-searchorigin")) {
                     origin=Tcl_GetString(Objv[++tidx]);
                  }
               }
            }
            Dict_SetSearch(DICT_GLOB,DICT_ALL,origin,ip1,-1,ip3,etiket);
         }

         obj=Tcl_NewListObj(0,NULL);
         while((var=Dict_IterateVar(&iter,str))) {
            Tcl_ListObjAppendElement(Interp,obj,Tcl_NewStringObj(var->Name,-1));
         }
         Tcl_SetObjResult(Interp,obj);
         break;

      case ISVAR:
         if (Objc!=3) {
            Tcl_WrongNumArgs(Interp,2,Objv,"[var]");
            return(TCL_ERROR);
         }
         Dict_SetSearch(DICT_EXACT,DICT_ALL,NULL,-1,-1,-1,NULL);

         str=Tcl_GetString(Objv[2]);
         var=Dict_IterateVar(&iter,str);
         Tcl_SetObjResult(Interp,Tcl_NewBooleanObj(var?TRUE:FALSE));
         break;

      case TYPE:
         if (Objc!=2 && Objc!=3) {
            Tcl_WrongNumArgs(Interp,2,Objv,"[type]");
            return(TCL_ERROR);
         }

         str=NULL;
         if (Objc>2) {
            str=Tcl_GetString(Objv[2]);
         }

         obj=Tcl_NewListObj(0,NULL);
         while((type=Dict_IterateType(&iter,str))) {
            Tcl_ListObjAppendElement(Interp,obj,Tcl_NewStringObj(type->Name,-1));
         }
         Tcl_SetObjResult(Interp,obj);
         break;

      case ISTYPE:
         if (Objc!=3) {
            Tcl_WrongNumArgs(Interp,2,Objv,"[type]");
            return(TCL_ERROR);
         }

         str=Tcl_GetString(Objv[2]);
         type=Dict_IterateType(&iter,str);
         Tcl_SetObjResult(Interp,Tcl_NewBooleanObj(type?TRUE:FALSE));
         break;

      case VARINFO:

         if (Objc>2) {
            ip1=ip3=-1;
            origin=etiket=modifier=NULL;
            for(tidx=2;tidx<Objc;tidx++) {
               if (!strcmp(Tcl_GetString(Objv[tidx]),"-all")) {
                 all=1;
               }
               if (tidx+1<Objc && Tcl_GetString(Objv[tidx+1])[0]!='-') {
                  if (!strcmp(Tcl_GetString(Objv[tidx]),"-searchip1")) {
                     Tcl_GetIntFromObj(Interp,Objv[++tidx],&ip1);
                  }
                  if (!strcmp(Tcl_GetString(Objv[tidx]),"-searchip3")) {
                     Tcl_GetIntFromObj(Interp,Objv[++tidx],&ip3);
                  }
                  if (!strcmp(Tcl_GetString(Objv[tidx]),"-searchorigin")) {
                     origin=Tcl_GetString(Objv[++tidx]);
                  }
                  if (!strcmp(Tcl_GetString(Objv[tidx]),"-searchetiket")) {
                     etiket=Tcl_GetString(Objv[++tidx]);
                  }
                  if (!strcmp(Tcl_GetString(Objv[tidx]),"-modifier")) {
                     modifier=Tcl_GetString(Objv[++tidx]);
                  }
               }
            }

            Dict_SetSearch(DICT_EXACT,DICT_ALL,origin,ip1,-1,ip3,etiket);
            if (all) {
               obj=Tcl_NewListObj(0,NULL);
               while ((var=Dict_IterateVar(&iter,Tcl_GetString(Objv[2])))) {
                  // Apply modifier if any
                  if (!(mod=Dict_ApplyModifier(var,modifier))) {
                     return(TCL_ERROR);
                  }

                  Tcl_ListObjAppendElement(Interp,obj,FSTD_DictVarInfo(Interp,mod,Objc-3,Objv+3));
                  if (mod!=var) free(mod);
               }
            } else {
               if ((var=Dict_GetVar(Tcl_GetString(Objv[2])))) {
                  // Apply modifier if any
                  if (!(mod=Dict_ApplyModifier(var,modifier))) {
                     return(TCL_ERROR);
                  }

                  // Var exist
                  obj=FSTD_DictVarInfo(Interp,mod,Objc-3,Objv+3);

                  if (mod!=var) free(mod);
               } else {
                  // Var does not exist, add it
                  var=(TDictVar*)calloc(1,sizeof(TDictVar));
                  strncpy(var->Name,Tcl_GetString(Objv[2]),5);
                  Dict_AddVar(var);
                  obj=FSTD_DictVarInfo(Interp,var,Objc-3,Objv+3);
               }
            }
            if (!obj) {
               return(TCL_ERROR);
            }
            Tcl_SetObjResult(Interp,obj);

         }
         break;

      case TYPEINFO:

         if (Objc>2) {
            Dict_SetSearch(DICT_EXACT,DICT_ALL,NULL,-1,-1,-1,NULL);
            if ((type=Dict_GetType(Tcl_GetString(Objv[2])))) {
               // Var exist
               return(FSTD_DictTypeInfo(Interp,type,Objc-3,Objv+3));
            } else {
               // Var does not exist, add it
               type=(TDictType*)calloc(1,sizeof(TDictType));
               strncpy(type->Name,Tcl_GetString(Objv[2]),2);
               Dict_AddType(type);
               return(FSTD_DictTypeInfo(Interp,type,Objc-3,Objv+3));
             }
         }
         break;
   }

   return(TCL_OK);
}

/*----------------------------------------------------------------------------
 * Nom      : <FSTD_DictVarInfo>
 * Creation : Mai 2014 - J.P. Gauthier - CMC/CMOE
 *
 * But      : Gestion des parametres des variables
 *
 * Parametres :
 *  <Interp>  : Interpreteur TCL
 *  <Var>     : Variable
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
static Tcl_Obj *FSTD_DictVarInfo(Tcl_Interp *Interp,TDictVar *Var,int Objc,Tcl_Obj *CONST Objv[]){

   Tcl_Obj *obj;
   int      i,idx;
   char     lang=0,n=0;;

   static CONST char *sopt[] = { "-lang","-all","-modifier","-searchip1","-searchip3","-searchorigin","-searchetiket","-short","-long","-magnitude","-min","-max","-factor","-delta","-units","-nature","-origin","-state","-date","-ip1","-ip3",NULL };
   enum                opt { LANG,ALL,MODIFIER,SEARCHIP1,SEARCHIP3,SEARCHORIGIN,SEARCHETIKET,SHORT,LONG,MAGNITUDE,MIN,MAX,FACTOR,DELTA,UNITS,NATURE,ORIGIN,STATE,DATE,IP1,IP3 };

   if (!Var) {
      return(NULL);
   }

   obj=Tcl_NewListObj(0,NULL);

   for(i=0;i<Objc;i++) {

      if (Tcl_GetIndexFromObj(Interp,Objv[i],sopt,"option",TCL_EXACT,&idx)!=TCL_OK) {
         return(NULL);
      }

      switch ((enum opt)idx) {
         case ALL:
            break;

         case SEARCHIP1:
         case SEARCHIP3:
         case SEARCHETIKET:
         case SEARCHORIGIN:
         case MODIFIER:
            ++i;
            break;

         case LANG:
            lang=Tcl_GetString(Objv[++i])[0];
            if (lang=='e' || lang=='E' || lang=='1') {
               lang=1;
            } else {
               lang=0;
            }
            break;

         case SHORT:
            n++;
            if (i+1<Objc && Tcl_GetString(Objv[i+1])[0]!='-') {
               strncpy(Var->Short[(int)lang],Tcl_GetString(Objv[++i]),128);
            } else {
               Tcl_ListObjAppendElement(Interp,obj,Tcl_NewStringObj(Var->Short[(int)lang],-1));
            }
            break;

         case LONG:
            n++;
            if (i+1<Objc && Tcl_GetString(Objv[i+1])[0]!='-') {
               strncpy(Var->Long[(int)lang],Tcl_GetString(Objv[++i]),128);
            } else {
               Tcl_ListObjAppendElement(Interp,obj,Tcl_NewStringObj(Var->Long[(int)lang],-1));
            }
            break;

         case NATURE:
            n++;
            if (i+1<Objc && Tcl_GetString(Objv[i+1])[0]!='-') {
               switch (Tcl_GetString(Objv[++i])[0]) {
                  case 'I': Var->Nature|=DICT_INTEGER; break;
                  case 'R': Var->Nature|=DICT_REAL; break;
                  case 'L': Var->Nature|=DICT_LOGICAL; break;
                  case 'C': Var->Nature|=DICT_CODE; break;
                  default: Var->Nature=DICT_ALL; break;
               }
            } else {
               if (Var->Nature&DICT_INTEGER) {
                  Tcl_ListObjAppendElement(Interp,obj,Tcl_NewStringObj("Integer",-1));
               } else if (Var->Nature&DICT_REAL) {
                  Tcl_ListObjAppendElement(Interp,obj,Tcl_NewStringObj("Real",-1));
               } else if (Var->Nature&DICT_LOGICAL) {
                  Tcl_ListObjAppendElement(Interp,obj,Tcl_NewStringObj("Logical",-1));
               } else if (Var->Nature&DICT_CODE) {
                  Tcl_ListObjAppendElement(Interp,obj,Tcl_NewStringObj("Coded",-1));
               }
            }
            break;

         case UNITS:
            n++;
            if (i+1<Objc && Tcl_GetString(Objv[i+1])[0]!='-') {
               strncpy(Var->Units,Tcl_GetString(Objv[++i]),32);
            } else {
               Tcl_ListObjAppendElement(Interp,obj,Tcl_NewStringObj(Var->Units,-1));
            }
            break;

         case MIN:
            n++;
            if (i+1<Objc && (Tcl_GetString(Objv[i+1])[0]!='-' || isdigit(Tcl_GetString(Objv[i+1])[1]))) {
               Tcl_GetDoubleFromObj(Interp,Objv[++i],&Var->Min);
            } else {
               if (Var->Min!=DICT_NOTSET)
                  Tcl_ListObjAppendElement(Interp,obj,Tcl_NewDoubleObj(Var->Min));
            }
            break;

         case MAX:
            n++;
            if (i+1<Objc && (Tcl_GetString(Objv[i+1])[0]!='-' || isdigit(Tcl_GetString(Objv[i+1])[1]))) {
               Tcl_GetDoubleFromObj(Interp,Objv[++i],&Var->Max);
            } else {
               if (Var->Max!=DICT_NOTSET)
                  Tcl_ListObjAppendElement(Interp,obj,Tcl_NewDoubleObj(Var->Max));
            }
            break;

         case MAGNITUDE:
            n++;
            if (i+1<Objc && (Tcl_GetString(Objv[i+1])[0]!='-' || isdigit(Tcl_GetString(Objv[i+1])[1]))) {
               Tcl_GetDoubleFromObj(Interp,Objv[++i],&Var->Magnitude);
            } else {
               if (Var->Magnitude!=DICT_NOTSET)
                  Tcl_ListObjAppendElement(Interp,obj,Tcl_NewDoubleObj(Var->Magnitude));
            }
            break;

         case FACTOR:
            n++;
            if (i+1<Objc && (Tcl_GetString(Objv[i+1])[0]!='-' || isdigit(Tcl_GetString(Objv[i+1])[1]))) {
               Tcl_GetDoubleFromObj(Interp,Objv[++i],&Var->Factor);
            } else {
               if (Var->Factor!=DICT_NOTSET)
               Tcl_ListObjAppendElement(Interp,obj,Tcl_NewDoubleObj(Var->Factor));
            }
            break;

         case DELTA:
            n++;
            if (i+1<Objc && (Tcl_GetString(Objv[i+1])[0]!='-' || isdigit(Tcl_GetString(Objv[i+1])[1]))) {
               Tcl_GetDoubleFromObj(Interp,Objv[++i],&Var->Delta);
            } else {
               if (Var->Delta!=DICT_NOTSET)
               Tcl_ListObjAppendElement(Interp,obj,Tcl_NewDoubleObj(Var->Delta));
            }
            break;

          case ORIGIN:
            n++;
            if (i+1<Objc && (Tcl_GetString(Objv[i+1])[0]!='-' || isdigit(Tcl_GetString(Objv[i+1])[1]))) {
               strncpy(Var->Origin,Tcl_GetString(Objv[++i]),32);
            } else {
               Tcl_ListObjAppendElement(Interp,obj,Tcl_NewStringObj(Var->Origin,-1));
            }
            break;

          case IP1:
            n++;
            if (i+1<Objc && Tcl_GetString(Objv[i+1])[0]!='-') {
               Tcl_GetIntFromObj(Interp,Objv[++i],&Var->IP1);
            } else {
               if (Var->IP1>=0)
                  Tcl_ListObjAppendElement(Interp,obj,Tcl_NewIntObj(Var->IP1));
            }
            break;

          case IP3:
            n++;
            if (i+1<Objc && Tcl_GetString(Objv[i+1])[0]!='-') {
               Tcl_GetIntFromObj(Interp,Objv[++i],&Var->IP3);
            } else {
               if (Var->IP3>=0)
                  Tcl_ListObjAppendElement(Interp,obj,Tcl_NewIntObj(Var->IP3));
            }
            break;

          case DATE:
            n++;
            if (i+1<Objc && Tcl_GetString(Objv[i+1])[0]!='-') {
               Tcl_GetWideIntFromObj(Interp,Objv[++i],&Var->Date);
            } else {
               Tcl_ListObjAppendElement(Interp,obj,Tcl_NewWideIntObj(Var->Date));
            }
            break;

          case STATE:
            n++;
            if (i+1<Objc && Tcl_GetString(Objv[i+1])[0]!='-') {
               switch (Tcl_GetString(Objv[++i])[0]) {
                  case 'o': Var->Nature|=DICT_OBSOLETE; break;
                  case 'c': Var->Nature|=DICT_CURRENT; break;
                  case 'f': Var->Nature|=DICT_FUTURE; break;
                  case 'i': Var->Nature|=DICT_INCOMPLETE; break;
                  default: Var->Nature=DICT_ALL; break;
               }
            } else {
               if (Var->Nature&DICT_OBSOLETE) {
                  Tcl_ListObjAppendElement(Interp,obj,Tcl_NewStringObj("obsolete",-1));
               } else if (Var->Nature&DICT_CURRENT) {
                  Tcl_ListObjAppendElement(Interp,obj,Tcl_NewStringObj("current",-1));
               } else if (Var->Nature&DICT_FUTURE) {
                  Tcl_ListObjAppendElement(Interp,obj,Tcl_NewStringObj("future",-1));
               } else if (Var->Nature&DICT_INCOMPLETE) {
                  Tcl_ListObjAppendElement(Interp,obj,Tcl_NewStringObj("incomplete",-1));
               }
            }
            break;
      }
   }

   // If not param requested, return all
   if (!n) {
      Tcl_ListObjAppendElement(Interp,obj,Tcl_NewStringObj(Var->Short[(int)lang],-1));
      Tcl_ListObjAppendElement(Interp,obj,Tcl_NewStringObj(Var->Long[(int)lang],-1));
      Tcl_ListObjAppendElement(Interp,obj,Tcl_NewStringObj(Var->Origin,-1));
      Tcl_ListObjAppendElement(Interp,obj,Tcl_NewWideIntObj(Var->Date));
      if (Var->Nature&DICT_OBSOLETE) {
         Tcl_ListObjAppendElement(Interp,obj,Tcl_NewStringObj("obsolete",-1));
      } else if (Var->Nature&DICT_CURRENT) {
         Tcl_ListObjAppendElement(Interp,obj,Tcl_NewStringObj("current",-1));
      } else if (Var->Nature&DICT_FUTURE) {
         Tcl_ListObjAppendElement(Interp,obj,Tcl_NewStringObj("future",-1));
      } else if (Var->Nature&DICT_INCOMPLETE) {
         Tcl_ListObjAppendElement(Interp,obj,Tcl_NewStringObj("incomplete",-1));
      }
      if (Var->Nature&DICT_INTEGER) {
         Tcl_ListObjAppendElement(Interp,obj,Tcl_NewStringObj("Integer",-1));
      } else if (Var->Nature&DICT_REAL) {
         Tcl_ListObjAppendElement(Interp,obj,Tcl_NewStringObj("Real",-1));
      } else if (Var->Nature&DICT_LOGICAL) {
         Tcl_ListObjAppendElement(Interp,obj,Tcl_NewStringObj("Logical",-1));
      } else if (Var->Nature&DICT_CODE) {
         Tcl_ListObjAppendElement(Interp,obj,Tcl_NewStringObj("Coded",-1));
      }
      Tcl_ListObjAppendElement(Interp,obj,Tcl_NewStringObj(Var->Units,-1));
      if (Var->Min!=DICT_NOTSET) {
         Tcl_ListObjAppendElement(Interp,obj,Tcl_NewDoubleObj(Var->Min));
      } else {
         Tcl_ListObjAppendElement(Interp,obj,Tcl_NewStringObj("",-1));
      }
      if (Var->Max!=DICT_NOTSET) {
         Tcl_ListObjAppendElement(Interp,obj,Tcl_NewDoubleObj(Var->Max));
      } else {
         Tcl_ListObjAppendElement(Interp,obj,Tcl_NewStringObj("",-1));
      }
      if (Var->Magnitude!=DICT_NOTSET) {
         Tcl_ListObjAppendElement(Interp,obj,Tcl_NewDoubleObj(Var->Magnitude));
      } else {
         Tcl_ListObjAppendElement(Interp,obj,Tcl_NewStringObj("",-1));
      }
      if (Var->Factor!=DICT_NOTSET) {
         Tcl_ListObjAppendElement(Interp,obj,Tcl_NewDoubleObj(Var->Factor));
      } else {
         Tcl_ListObjAppendElement(Interp,obj,Tcl_NewStringObj("",-1));
      }
      if (Var->Delta!=DICT_NOTSET) {
         Tcl_ListObjAppendElement(Interp,obj,Tcl_NewDoubleObj(Var->Delta));
      } else {
         Tcl_ListObjAppendElement(Interp,obj,Tcl_NewStringObj("",-1));
      }
   }

   return(obj);
}

/*----------------------------------------------------------------------------
 * Nom      : <FSTD_DictTypeInfo>
 * Creation : Mai 2014 - J.P. Gauthier - CMC/CMOE
 *
 * But      : Gestion des parametres des variables
 *
 * Parametres :
 *  <Interp>  : Interpreteur TCL
 *  <Var>     : Variable
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
static int FSTD_DictTypeInfo(Tcl_Interp *Interp,TDictType *Type,int Objc,Tcl_Obj *CONST Objv[]){

   Tcl_Obj *obj;
   int      i,idx;
   char     lang=0,n=0;;

   static CONST char *sopt[] = { "-lang","-short","-long",NULL };
   enum                opt { LANG,SHORT,LONG };

   if (!Type) {
      Tcl_AppendResult(Interp,"FSTD_DictTypeInfo: Invalid type",(char*)NULL);
      return(TCL_ERROR);
   }

   obj=Tcl_NewListObj(0,NULL);

   for(i=0;i<Objc;i++) {

      if (Tcl_GetIndexFromObj(Interp,Objv[i],sopt,"option",TCL_EXACT,&idx)!=TCL_OK) {
         return(TCL_ERROR);
      }

      switch ((enum opt)idx) {
         case LANG:
            lang=Tcl_GetString(Objv[++i])[0];
            if (lang=='e' || lang=='E' || lang=='1') {
               lang=1;
            } else {
               lang=0;
            }
            break;

         case SHORT:
            n++;
            if (i+1<Objc && Tcl_GetString(Objv[i+1])[0]!='-') {
               strncpy(Type->Short[(int)lang],Tcl_GetString(Objv[++i]),128);
            } else {
               Tcl_ListObjAppendElement(Interp,obj,Tcl_NewStringObj(Type->Short[(int)lang],-1));
            }
            break;

         case LONG:
            n++;
            if (i+1<Objc && Tcl_GetString(Objv[i+1])[0]!='-') {
               strncpy(Type->Long[(int)lang],Tcl_GetString(Objv[++i]),128);
            } else {
               Tcl_ListObjAppendElement(Interp,obj,Tcl_NewStringObj(Type->Long[(int)lang],-1));
            }
            break;
      }
   }

   // If not param requested, return all
   if (!n) {
      Tcl_ListObjAppendElement(Interp,obj,Tcl_NewStringObj(Type->Short[(int)lang],-1));
      Tcl_ListObjAppendElement(Interp,obj,Tcl_NewStringObj(Type->Long[(int)lang],-1));
   }

   Tcl_SetObjResult(Interp,obj);

   return(TCL_OK);
}

#endif
