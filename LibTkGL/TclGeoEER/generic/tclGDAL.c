/*=========================================================
 * Environnement Canada
 * Centre Meteorologique Canadien
 * 2100 Trans-Canadienne
 * Dorval, Quebec
 *
 * Projet       : Lecture et traitements de fichiers raster
 * Fichier      : tclGDAL.c
 * Creation     : Juin 2004 - J.P. Gauthier
 *
 * Description  : Fonctions de manipulations et d'affichage de fichiers raster.
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
 *
 *=========================================================
 */

#include <strings.h>

#include "tclGDAL.h"
#include "Projection.h"

/*Table contenant la liste des fichiers en memoire*/
static Tcl_HashTable GDAL_FileTable;
static Tcl_HashTable GDAL_BandTable;
static int GDALInit=0;

static int GDAL_FileCmd(ClientData clientData,Tcl_Interp *Interp,int Objc,Tcl_Obj *CONST Objv[]);
static int GDAL_BandCmd(ClientData clientData,Tcl_Interp *Interp,int Objc,Tcl_Obj *CONST Objv[]);

int GDAL_Type[]={ 0,2,4,5,6,7,10,11,0,0,0,0 };
int TD2GDAL[]={ 0,0,1,1,2,3,4,5,0,0,6,7 };

/*----------------------------------------------------------------------------
 * Nom      : <tclGDAL_Init>
 * Creation : Juin 2004 - J.P. Gauthier - CMC/CMOE
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
int TclGDAL_Init(Tcl_Interp *Interp) {

   if (!GDALInit++) {
      GDALAllRegister();

      Tcl_InitHashTable(&GDAL_FileTable,TCL_STRING_KEYS);
      Tcl_InitHashTable(&GDAL_BandTable,TCL_STRING_KEYS);
   }

   Tcl_CreateObjCommand(Interp,"gdalfile",GDAL_FileCmd,(ClientData)NULL,(Tcl_CmdDeleteProc*)NULL);
   Tcl_CreateObjCommand(Interp,"gdalband",GDAL_BandCmd,(ClientData)NULL,(Tcl_CmdDeleteProc*)NULL);

   return(TCL_OK);
}

/*--------------------------------------------------------------------------------------------------------------
 * Nom          : <GDAL_BandCmd>
 * Creation     : Juin 2004 J.P. Gauthier - CMC/CMOE
 *
 * But          : Effectuer les commandes du package
 *
 * Parametres    :
 *  <clientData> : Nom du geotiff
 *  <Interp>     : Interpreteur Tcl
 *  <Objc>       : Nombre d'arguments
 *  <Objv>       : Pointeur sur la liste des arguments
 *
 * Retour        : Code de retour standard TCL
 *
 * Remarques     :
 *
 *---------------------------------------------------------------------------------------------------------------
*/

static int GDAL_BandCmd(ClientData clientData,Tcl_Interp *Interp,int Objc,Tcl_Obj *CONST Objv[]) {

   double         lat,lon,x,y,*table;
   char           idxid[4][128],*field,imode,itype;
   const char   **list;
   int            width=0,height=0,space=0,full=1;

   Tcl_Obj       *obj,*sub;
   int            idx,nidx,idxfi[4],i,k,n,ni,nj,nk,id,code,x0,y0,x1,y1,bd;
   double         c0,c1,a;
   TData         *data;
   TDataSpec     *spec;
   OGR_Layer     *layer;
   GDAL_Band     *band,*comb,*bandt;
   TObs          *obs;
   GDALDataType   type;

   static CONST char *moderas[] = { "NEAREST","LINEAR","CUBIC","NORMALIZED_CONSERVATIVE","CONSERVATIVE","MAXIMUM","MINIMUM","SUM","AVERAGE","AVERAGE_VARIANCE","AVERAGE_SQUARE","NORMALIZED_COUNT","COUNT","LENGTH_CONSERVATIVE","LENGTH_ALIASED","LENGTH_NORMALIZED_CONSERVATIVE","NOP","ACCUM","BUFFER",NULL };
   static CONST char *modeogr[] = { "FAST","WITHIN","INTERSECT","CONSERVATIVE","NORMALIZED_CONSERVATIVE","ALIASED","POINT_CONSERVATIVE","LENGTH_CONSERVATIVE","LENGTH_NORMALIZED_CONSERVATIVE","LENGTH_ALIASED",NULL };
   static CONST char *sopt[] = { "create","copy","free","read","write","tile","gridinterp","import","configure","define","stats","clean","clear","combine","mapimage","is","project","unproject","pick","all","wipe",NULL };
   enum                opt { CREATE,COPY,FREE,READ,WRITE,TILE,GRIDINTERP,IMPORT,CONFIGURE,DEFINE,STATS,CLEAN,CLEAR,COMBINE,MAPIMAGE,IS,PROJECT,UNPROJECT,PICK,ALL,WIPE };

   Tcl_ResetResult(Interp);

   if (Objc<2) {
      Tcl_WrongNumArgs(Interp,1,Objv,"command ?arg arg ...?");
      return TCL_ERROR;
   }

   if (Tcl_GetIndexFromObj(Interp,Objv[1],sopt,"command",0,&idx)!=TCL_OK) {
      return TCL_ERROR;
   }

   switch ((enum opt)idx) {
      case CREATE:
         if (Objc!=7 && Objc!=3) {
            Tcl_WrongNumArgs(Interp,2,Objv,"band width height space type(Byte UInt16 Int16 Uint32 Int32 Float32 Float64)");
            return TCL_ERROR;
         }
         if (!(band=GDAL_BandCreate(Interp,Tcl_GetString(Objv[2])))) {
            return(TCL_ERROR);
         }

         if (Objc>3) {
            Tcl_GetIntFromObj(Interp,Objv[3],&width);
            Tcl_GetIntFromObj(Interp,Objv[4],&height);
            Tcl_GetIntFromObj(Interp,Objv[5],&space);

            if (!width || !height || !space) {
               Tcl_AppendResult(Interp,"\n   GDAL_BandCmd : Invalid dimensions",(char*)NULL);
               return(TCL_ERROR);
            }
            type=GDALGetDataTypeByName(Tcl_GetString(Objv[6]));
            if (!(band->Def=DataDef_New(width,height,1,space,GDAL_Type[type]))) {
               Tcl_AppendResult(Interp,"\n   GDAL_BandCmd : Unable to allocate band",(char*)NULL);
               return(TCL_ERROR);
            }
         } else {
            band->Def=DataDef_New(0,0,0,0,TD_Unknown);
         }
         break;

      case COPY:
         if(Objc<4) {
            Tcl_WrongNumArgs(Interp,2,Objv,"bandto bandfrom");
            return TCL_ERROR;
         }
         if(!GDAL_BandCopy(Interp,GDAL_BandGet(Tcl_GetString(Objv[3])),Tcl_GetString(Objv[2]),1)) {
            return(TCL_ERROR);
         } else {
            return(TCL_OK);
         }
         break;

      case COMBINE:
         if (Objc<4 || Objc>7) {
            Tcl_WrongNumArgs(Interp,2,Objv,"band red [green band] [blue band] [alpha band]");
            return(TCL_ERROR);
         }
         if (!(band=GDAL_BandGet(Tcl_GetString(Objv[2])))) {
            Tcl_AppendResult(Interp,"\n   GDAL_BandCmd : invalid band",(char*)NULL);
            return(TCL_ERROR);
         }

         if (!band->Def->Container) {
            Tcl_AppendResult(Interp,"\n   GDAL_BandCmd : band is not a container",(char*)NULL);
            return(TCL_ERROR);
         }

         band->Def->NC=0;
         for(nidx=0;nidx<Objc-3;nidx++) {
            if (!(comb=GDAL_BandGet(Tcl_GetString(Objv[nidx+3])))) {
               Tcl_AppendResult(Interp,"\n   GDAL_BandCmd : invalid band",Tcl_GetString(Objv[nidx+3]),(char*)NULL);
               return(TCL_ERROR);
            }
            if (nidx==0) {
               band->Ref=GeoRef_Copy(comb->Ref);
               band->Def->NI=comb->Def->NI;
               band->Def->NJ=comb->Def->NJ;
               band->Def->NK=comb->Def->NK;
               band->Def->Type=comb->Def->Type;
            } else {
               if (FSIZE2D(band->Def)!=FSIZE2D(comb->Def)) {
                  Tcl_AppendResult(Interp,"\n   GDAL_BandCmd : incompatible dimension",(char*)NULL);
                  return(TCL_ERROR);
               }
            }
            band->Def->NC++;
            band->Def->Data[nidx]=comb->Def->Data[0];
            band->Band[nidx]=comb->Band[0];
         }
         break;

      case IMPORT:
      case GRIDINTERP:
         if (Objc<4) {
            Tcl_WrongNumArgs(Interp,2,Objv,"band ...");
            return(TCL_ERROR);
         }

         if (!(band=GDAL_BandGet(Tcl_GetString(Objv[2])))) {
            Tcl_AppendResult(Interp,"\n   GDAL_BandCmd : invalid band",(char*)NULL);
            return(TCL_ERROR);
         }

          n=-1;
         /*Interpolate a layer*/
         layer=OGR_LayerGet(Tcl_GetString(Objv[3]));
         if (layer) {
            if (Objc!=5 && Objc!=6) {
               Tcl_WrongNumArgs(Interp,2,Objv,"band layer type [field]");
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
            return(Data_GridOGR(Interp,band->Def,band->Ref,layer,imode,itype,1,field,x));
         }

         /*Interpolate a field*/
         data=Data_Get(Tcl_GetString(Objv[3]));
         if (data) {
            if (Objc!=4 && Objc!=5) {
               Tcl_WrongNumArgs(Interp,2,Objv,"band field [scale]");
               return(TCL_ERROR);
            }
            if (data->Ref->Grid[0]=='V') {
               space=0;
               if (Objc==5) {
                  Tcl_GetBooleanFromObj(Interp,Objv[4],&space);
               }
               return(GDAL_BandFSTDImportV(Interp,GDAL_BandGet(Tcl_GetString(Objv[2])),data,space));
            } else {
               return(GDAL_BandFSTDImport(Interp,GDAL_BandGet(Tcl_GetString(Objv[2])),data));
            }
         }

         /*Interpolate a field
         field1=Data_Get(Tcl_GetString(Objv[3]));
         if (field1) {
            if (Objc>4) {
               if (Tcl_GetIndexFromObj(Interp,Objv[4],moderas,"mode",0,&n)!=TCL_OK) {
                  return(TCL_ERROR);
               }
            }

            if (field0->Ref->Ids[field0->Ref->NId]==field1->Ref->Ids[field1->Ref->NId]) {
               return(Data_Copy(Interp,field1,Tcl_GetString(Objv[2])));
            }

            if (n==3 || n==4) {
               if (Objc>8 || Objc<6) {
                  Tcl_WrongNumArgs(Interp,2,Objv,"fldto fldfrom [Type] [Split] [Final] [Index list variable]");
                  return(TCL_ERROR);
               }
               Tcl_GetIntFromObj(Interp,Objv[5],&ni);
               nj=0;
               obj=NULL;
               if (Objc>6) {
                  if (Tcl_GetBooleanFromObj(Interp,Objv[6],&nj)==TCL_ERROR) {
                     obj=Objv[6];
                  }
               }
               if (Objc>7) {
                  obj=Objv[7];
               }
               FSTD_FieldSetTo(field0,field1);
               return(Data_GridConservative(Interp,field0->Ref,field0->Def,field1->Ref,field1->Def,Tcl_GetString(Objv[4])[0],nj,ni,obj));
            } else if (n>=5 && n<=16) {
               if (Objc!=5 && Objc!=6) {
                  Tcl_WrongNumArgs(Interp,2,Objv,"fldto fldfrom [Type] [Final]");
                  return(TCL_ERROR);
               }
               ni=1;
               if(Objc==6) {
                  Tcl_GetBooleanFromObj(Interp,Objv[5],&ni);
               }
               return(Data_GridAverage(Interp,field0->Ref,field0->Def,field1->Ref,field1->Def,n,ni));
            } else {
               return(FSTD_FieldGridInterpolate(Interp,field0,field1,n));
            }
            break;
         }
*/
         /*Interpolate a band*/
         comb=GDAL_BandGet(Tcl_GetString(Objv[3]));
         if (comb) {
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
               return(Data_GridConservative(Interp,band->Ref,band->Def,comb->Ref,comb->Def,Tcl_GetString(Objv[4])[0],nj,ni,obj));
            } else if (n>=5 && n<=18) {
               if (Objc<5 || Objc>7) {
                  Tcl_WrongNumArgs(Interp,2,Objv,"bandto bandfrom [Type] [Values] [Final]");
                  return(TCL_ERROR);
               }
               ni=1;
               table=NULL;
               bandt=NULL;
               if (Objc>5) {
                  if (Tcl_GetBooleanFromObj(Interp,Objv[5],&ni)==TCL_ERROR) {
                     if (!(bandt=GDAL_BandGet(Tcl_GetString(Objv[5])))) {
                        Tcl_ListObjLength(Interp,Objv[5],&nk);
                        if (!(table=(double*)malloc((nk+1)*sizeof(double)))) {
                           Tcl_AppendResult(Interp,"GDAL_BandCmd: Unable to allocate memory for temporary buffer",(char*)NULL);
                           return(TCL_ERROR);                          
                        }
                        for(k=0;k<nk;k++) {
                           Tcl_ListObjIndex(Interp,Objv[5],k,&obj);
                           Tcl_GetDoubleFromObj(Interp,obj,&table[k]);
                        }
                        table[k]=band->Def->NoData;
                        if (nk!=band->Def->NK) {
                           band->Def=DataDef_Resize(band->Def,band->Def->NI,band->Def->NJ,nk);
                           for(k=0;k<FSIZE3D(band->Def);k++) {
                              Def_Set(band->Def,0,k,0);
                           }
                           GeoRef_Resize(band->Ref,band->Def->NI,band->Def->NJ,nk,band->Ref->ZRef.Type,band->Ref->ZRef.Levels);
                        }
                     }
                  }
               }
               if (Objc==7) {
                  Tcl_GetBooleanFromObj(Interp,Objv[6],&ni);
               }
               code=Data_GridAverage(Interp,band->Ref,band->Def,comb->Ref,comb->Def,table,bandt?bandt->Def:NULL,n,ni);
               if (table)
                  free(table);
               return(code);
            } else {
               return(Data_GridInterpolate(Interp,Objv[4]?Tcl_GetString(Objv[4])[0]:'L',band->Ref,band->Def,comb->Ref,comb->Def));
            }
            break;
         }


         /*Interpolate an obs*/
         obs=Obs_Get(Tcl_GetString(Objv[3]));
         if (obs) {
            if(Objc!=8 && Objc!=9) {
               Tcl_WrongNumArgs(Interp,2,Objv,"fldto obsfrom Nugget(C0) Sill(C1) Range(A) [SPHERICAL | EXPONENTIAL | GAUSSIAN | LINEAR] [Outside]");
               return(TCL_ERROR);
            }
            Tcl_GetDoubleFromObj(Interp,Objv[4],&c0);
            Tcl_GetDoubleFromObj(Interp,Objv[5],&c1);
            Tcl_GetDoubleFromObj(Interp,Objv[6],&a);

            n=1;id=1;
            if(Objc==9) {
               Tcl_GetBooleanFromObj(Interp,Objv[6],&n);
            }

            switch(Tcl_GetString(Objv[7])[0]) {
               case 'S':id=1;break;
               case 'E':id=2;break;
               case 'G':id=3;break;
               case 'L':id=4;break;
               default:  id=1;
            }

            if (FFKrigging(band->Ref,band->Def,obs,c0,c1,a,id,n)) {
               return(TCL_OK);
            } else {
               Tcl_AppendResult(Interp,"Krigging failed",(char*)NULL);
               return(TCL_ERROR);
            }
            break;
         }

         /* If we get here, it has to be a NOP or ACCUM*/
         if (Objc>4) {
            if (Tcl_GetIndexFromObj(Interp,Objv[4],moderas,"mode",0,&n)!=TCL_OK) {
               return(TCL_ERROR);
            }
         }
         if (n==16 || n==17 || n==18) {
            return(Data_GridAverage(Interp,band->Ref,band->Def,NULL,NULL,NULL,NULL,n,1));
         } else {
            Tcl_AppendResult(Interp,"invalid data type",(char*)NULL);
            return(TCL_ERROR);
         }
         break;

      case READ:
         if (Objc!=4 && Objc!=5 && Objc!=8 && Objc!=9) {
            Tcl_WrongNumArgs(Interp,2,Objv,"band { { set idx } { ... } ... } [Full]|[X0 Y0 X1 Y1] [Border]");
            return(TCL_ERROR);
         }

         Tcl_ListObjLength(Interp,Objv[3],&nidx);
         nj=0;
         if (nidx>0) {
            for (i=0;i<(nidx>4?4:nidx);i++) {
               Tcl_ListObjIndex(Interp,Objv[3],i,&obj);
               Tcl_ListObjLength(Interp,obj,&ni);
               if (ni>=2) {
                  Tcl_ListObjIndex(Interp,obj,0,&sub);
                  strcpy(idxid[nj],Tcl_GetString(sub));
                  Tcl_ListObjIndex(Interp,obj,1,&sub);
                  Tcl_GetIntFromObj(Interp,sub,&idxfi[nj]);
                  nj++;
               }
            }
         }
         x0=y0=x1=y1=-1;bd=0;
         if (Objc==5) {
            Tcl_GetBooleanFromObj(Interp,Objv[4],&full);
         }
         if (Objc>=8) {
            Tcl_GetIntFromObj(Interp,Objv[4],&x0);
            Tcl_GetIntFromObj(Interp,Objv[5],&y0);
            Tcl_GetIntFromObj(Interp,Objv[6],&x1);
            Tcl_GetIntFromObj(Interp,Objv[7],&y1);
         }
         if (Objc==9) {
            Tcl_GetIntFromObj(Interp,Objv[8],&bd);
         }

         return(GDAL_BandRead(Interp,Tcl_GetString(Objv[2]),idxid,idxfi,nj,x0,y0,x1,y1,bd,full));
         break;

      case WRITE:
         if (Objc<4) {
            Tcl_WrongNumArgs(Interp,2,Objv,"band set [options]");
            return(TCL_ERROR);
         }

         if (Objc==5) {
            if (Tcl_SplitList(Interp,Tcl_GetString(Objv[4]),&nidx,&list)==TCL_ERROR) {
               Tcl_AppendResult(Interp,"\n   GDAL_BandCmd : Invalid list of creation options",(char*)NULL);
               return(TCL_ERROR);
            }
         } else {
            list=NULL;
            nidx=0;
         }

         code=GDAL_BandWrite(Interp,Objv[2],Tcl_GetString(Objv[3]),(char**)list);
         if (list) Tcl_Free((char*)list);

         return(code);
         break;

      case TILE:
         if (Objc!=6) {
            Tcl_WrongNumArgs(Interp,2,Objv,"band bandtile x y");
            return(TCL_ERROR);
         }

         if (!(band=GDAL_BandGet(Tcl_GetString(Objv[2])))) {
            Tcl_AppendResult(Interp,"\n   GDAL_BandCmd : invalid destination band",(char*)NULL);
            return(TCL_ERROR);
         }
         if (!(comb=GDAL_BandGet(Tcl_GetString(Objv[3])))) {
            Tcl_AppendResult(Interp,"\n   GDAL_BandCmd : invalid tile band",(char*)NULL);
            return(TCL_ERROR);
         }
         Tcl_GetIntFromObj(Interp,Objv[4],&x0);
         Tcl_GetIntFromObj(Interp,Objv[5],&y0);

         DataDef_Tile(band->Def,comb->Def,x0,y0);
         return(TCL_OK);
         break;

      case FREE:
         if (Objc<3) {
            Tcl_WrongNumArgs(Interp,2,Objv,"band");
            return(TCL_ERROR);
         }
         for(n=2;n<Objc;n++) {
            GDAL_BandDestroy(Interp,Tcl_GetString(Objv[n]));
         }
         return(TCL_OK);
         break;

      case PROJECT:
         if (Objc!=5) {
            Tcl_WrongNumArgs(Interp,2,Objv,"band X Y");
            return(TCL_ERROR);
         }

         obj=Tcl_NewListObj(0,NULL);
         Tcl_GetDoubleFromObj(Interp,Objv[3],&x);
         Tcl_GetDoubleFromObj(Interp,Objv[4],&y);
         band=GDAL_BandGet(Tcl_GetString(Objv[2]));

         if (!band) {
            Tcl_AppendResult(Interp,"\n   GDAL_BandCmd : Invalid band",(char*)NULL);
            return(TCL_ERROR);
         } else {
            if (band->Ref && band->Ref->Project(band->Ref,x,y,&lat,&lon,1,1)) {
               Tcl_ListObjAppendElement(Interp,obj,Tcl_NewDoubleObj(lat));
               Tcl_ListObjAppendElement(Interp,obj,Tcl_NewDoubleObj(lon));
            } else {
               Tcl_ListObjAppendElement(Interp,obj,Tcl_NewStringObj("-",-1));
               Tcl_ListObjAppendElement(Interp,obj,Tcl_NewStringObj("-",-1));
            }
            Tcl_SetObjResult(Interp,obj);
            return(TCL_OK);
         }
         break;

      case UNPROJECT:
         if (Objc!=5) {
            Tcl_WrongNumArgs(Interp,2,Objv,"band lat lon");
            return(TCL_ERROR);
         }

         obj=Tcl_NewListObj(0,NULL);
         Tcl_GetDoubleFromObj(Interp,Objv[3],&lat);
         Tcl_GetDoubleFromObj(Interp,Objv[4],&lon);
         band=GDAL_BandGet(Tcl_GetString(Objv[2]));

         if (!band) {
            Tcl_AppendResult(Interp,"\n   GDAL_BandCmd : Invalid band",(char*)NULL);
            return(TCL_ERROR);
         } else {
            if (band->Ref && band->Ref->UnProject(band->Ref,&x,&y,lat,lon,1,1)) {
               Tcl_ListObjAppendElement(Interp,obj,Tcl_NewDoubleObj(x));
               Tcl_ListObjAppendElement(Interp,obj,Tcl_NewDoubleObj(y));
            } else {
               Tcl_ListObjAppendElement(Interp,obj,Tcl_NewStringObj("-",-1));
               Tcl_ListObjAppendElement(Interp,obj,Tcl_NewStringObj("-",-1));
            }
            Tcl_SetObjResult(Interp,obj);
            return(TCL_OK);
         }

         break;

      case PICK:
         if (Objc!=4) {
            Tcl_WrongNumArgs(Interp,2,Objv,"band ");
            return(TCL_ERROR);
         }
         
         return(GDAL_Pick(Interp,GDAL_BandGet(Tcl_GetString(Objv[2])),Objv[3]));
         break;

      case CONFIGURE:
         if(Objc<3) {
            Tcl_WrongNumArgs(Interp,2,Objv,"band ?option?");
            return(TCL_ERROR);
         }
         band=GDAL_BandGet(Tcl_GetString(Objv[2]));
         if (!band) {
            Tcl_AppendResult(Interp,"invalid band",(char*)NULL);
            return(TCL_ERROR);
         }

         if (!band->Stat)
            GDAL_BandGetStat(band);

         if (strcmp(Tcl_GetString(Objv[3]),"-dataspec")==0) {
            if (Objc==4) {
               if (band->Spec) {
                  band->Spec->NRef++;
                  Tcl_SetObjResult(Interp,Tcl_NewStringObj(band->Spec->Name,-1));
               }
            } else {
               if ((spec=DataSpec_Get(Tcl_GetString(Objv[4])))) {
                  if (band->Spec) {
                     DataSpec_FreeHash(Interp,band->Spec->Name);
                  }
                  band->Spec=spec;
                  spec->NRef++;
               } else {
                  Tcl_AppendResult(Interp,"GDAL_BandCmd: invalid configuration object",(char*)NULL);
                  return(TCL_ERROR);
               }
            }
         } else {
            if (DataSpec_Config(Interp,band->Spec,Objc-3,Objv+3)==TCL_OK) {
               return(TCL_OK);
            } else {
               return(TCL_ERROR);
            }
         }
         break;

      case DEFINE:
         if (Objc<3) {
            Tcl_WrongNumArgs(Interp,2,Objv,"band ?option?");
            return(TCL_ERROR);
         }
         return(GDAL_BandDefine(Interp,Tcl_GetString(Objv[2]),Objc-3,Objv+3));
         break;

      case STATS:
         if(Objc<3) {
            Tcl_WrongNumArgs(Interp,2,Objv,"band ?option?");
            return(TCL_ERROR);
         }
         return(GDAL_BandStat(Interp,Tcl_GetString(Objv[2]),Objc-3,Objv+3));
         break;

      case MAPIMAGE:
         if(Objc!=3) {
            Tcl_WrongNumArgs(Interp,2,Objv,"band ?option?");
            return(TCL_ERROR);
         }
         band=GDAL_BandGet(Tcl_GetString(Objv[2]));
         if (!band) {
            Tcl_AppendResult(Interp,"invalid band",(char*)NULL);
            return(TCL_ERROR);
         }
         return(GDAL_GetMapImage(Interp,band));
         break;


      case CLEAN:
         if(Objc<3) {
            Tcl_WrongNumArgs(Interp,2,Objv,"band");
            return(TCL_ERROR);
         }
         GDAL_BandClean(GDAL_BandGet(Tcl_GetString(Objv[2])),1,1,1);
         break;

      case CLEAR:
         if(Objc<3) {
            Tcl_WrongNumArgs(Interp,2,Objv,"band [values]");
            return(TCL_ERROR);
         }
         band=GDAL_BandGet(Tcl_GetString(Objv[2]));
         if (!band) {
            Tcl_AppendResult(Interp,"invalid band",(char*)NULL);
            return(TCL_ERROR);
         }

         a=band->Def->NoData;

         for(n=0;n<DSIZE(band->Def->Data);n++) {
            /*If clearing values are specified*/
            if (Objc>(3+n)) {
               Tcl_GetDoubleFromObj(Interp,Objv[3+n],&a);
            }
            for(i=0;i<FSIZE3D(band->Def);i++) {
               Def_Set(band->Def,n,i,a);
            }
         }
         if (band->Def->Buffer) {
            free(band->Def->Buffer);
            band->Def->Buffer=NULL;
         }
         if (band->Def->Accum) {
            free(band->Def->Accum);
            band->Def->Accum=NULL;
         }
         if (band->Def->Mask) {
            free(band->Def->Mask);
            band->Def->Mask=NULL;
         }
         break;

      case IS:
         if (Objc!=3) {
            Tcl_WrongNumArgs(Interp,2,Objv,"band");
            return(TCL_ERROR);
         }
         if (GDAL_BandGet(Tcl_GetString(Objv[2]))) {
            Tcl_SetObjResult(Interp,Tcl_NewBooleanObj(1));
         } else {
            Tcl_SetObjResult(Interp,Tcl_NewBooleanObj(0));
         }
         break;

      case ALL:
         TclY_HashAll(Interp,&GDAL_BandTable);
         break;

      case WIPE:
         TclY_HashWipe(&GDAL_FileTable,(TclY_HashFreeEntryDataFunc*)NULL);
         TclY_HashWipe(&GDAL_BandTable,(TclY_HashFreeEntryDataFunc*)NULL);
         break;
   }

   return TCL_OK;
}

/*--------------------------------------------------------------------------------------------------------------
 * Nom          : <GDAL_BandCreate>
 * Creation     : Juin 2004 - J.P. Gauthier - CMC/CMOE
 *
 * But          : Creation d'un objet GDAL_Bandf et insertion d'un nouveau nom dans la table.
 *
 * Parametres   :
 *   <Interp>   : Interpreteur Tcl
 *   <Name>     : Nom de la bande a creer
 *
 * Retour       : Code de retour standard TCL
 *
 * Remarques    :
 *
 *---------------------------------------------------------------------------------------------------------------
*/

GDAL_Band* GDAL_BandCreate(Tcl_Interp *Interp,char *Name) {

   GDAL_Band* band;


  if (!(band=(GDAL_Band*)TclY_HashPut(Interp,&GDAL_BandTable,Name,sizeof(GDAL_Band)))) {
      return(NULL);
   }

   /* Initialisation de la structure geo */
   band->Tex.ThreadId =0;
   band->Tex.Proj     =NULL;
   band->Tex.Tile     =NULL;
   band->Tex.Indexed  =0;
   band->Tex.Res      =0;
   band->Tex.ResN     =0;
   band->Tex.Nx       =0;
   band->Tex.Ny       =0;

   band->NbGCPs     =0;
   band->GCPs       =NULL;

   band->File       =NULL;
   band->Ref        =NULL;
   band->Spec       =NULL;
   band->Def        =NULL;
   band->Stat       =NULL;
   band->Tag        =NULL;
   band->Date       =0;

   memset(band->Band,0x0,256*sizeof(GDALRasterBandH));

   if (!(band->Spec=DataSpec_Create(Interp,NULL))) {
      Tcl_AppendResult(Interp,"GDAL_BandCreate: Not enough memory to allocate band",(char *)NULL);
      return(NULL);
   }
   band->Spec->RenderTexture=1;

   return(band);
}

/*--------------------------------------------------------------------------------------------------------------
 * Nom          : <GDALBand_Clean>
 * Creation     : Juillet 2004 J.P. Gauthier - CMC/CMOE
 *
 * But          : Supprimer les textures et positions d'une bande
 *
 * Parametres   :
 *   <Band>     : Bande
 *   <Map>      : Clean de la Texture
 *   <Pos>      : Clean des Positions
 *   <Seg>      : Clean des Contours
 *
 * Retour       :
 *
 * Remarques    :
 *
 *---------------------------------------------------------------------------------------------------------------
*/
void GDAL_BandClean(GDAL_Band *Band,int Map,int Pos,int Seg) {

   if (Band) {
      if (Pos)
         GeoTex_Signal(&Band->Tex,GEOTEX_CLRCOO);

      if (Seg || (Map && (Band->Tex.Indexed || (GLRender && !GLRender->ShaderAvailable))))
         GeoTex_Clear(&Band->Tex,GEOTEX_CLRTEX,0,0);
         //GeoTex_Signal(&Band->Tex,GEOTEX_CLRTEX);
   }
}

/*--------------------------------------------------------------------------------------------------------------
 * Nom          : <GDALBand_CleanAll>
 * Creation     : Juillet 2004 J.P. Gauthier - CMC/CMOE
 *
 * But          : Supprimer les textures et positions de toutes les bandes en memoire
 *
 * Parametres   :
 *   <Band>     : Bande
 *   <Map>      : Clean de la Texture
 *   <Pos>      : Clean des Positions
 *   <Seg>      : Clean des Contours
 *
 * Retour       :
 *
 * Remarques    :
 *
 *---------------------------------------------------------------------------------------------------------------
*/
void GDAL_BandCleanAll(TDataSpec *Spec,int Map,int Pos,int Seg) {

   GDAL_Band      *band;
   Tcl_HashSearch  ptr;
   Tcl_HashEntry  *entry=NULL;

   TclY_LockHash();
   entry=Tcl_FirstHashEntry(&GDAL_BandTable,&ptr);
   while (entry) {
      band=Tcl_GetHashValue(entry);

      if (band && band->Spec && band->Spec==Spec) {
         GDAL_BandClean(band,Map,Pos,Seg);
      }
      entry=Tcl_NextHashEntry(&ptr);
   }
   TclY_UnlockHash();
}

/*--------------------------------------------------------------------------------------------------------------
 * Nom          : <GDAL_BandCopy>
 * Creation     : Juillet 2004 J.P. Gauthier - CMC/CMOE
 *
 * But          : Copier une bande dans une autre
 *
 * Parametres   :
 *   <Interp>   : Interpreteur Tcl
 *   <Band>     : Bande a copier
 *   <Name>     : Nom de la nouvelle bande destination
 *   <Def>      : Flag pouir copier le datadef
 *
 * Retour       :
 *
 * Remarques    :
 *
 *---------------------------------------------------------------------------------------------------------------
*/
GDAL_Band *GDAL_BandCopy(Tcl_Interp *Interp,GDAL_Band *Band,char *Name,int Def){

   GDAL_Band *band;

   /* Verifier que le champs n'est pas lui-meme*/
   if ((band=GDAL_BandGet(Name))) {
      if (band!=Band) {
         GDAL_BandDestroy(Interp,Name);
      } else {
         if (!Def && band->Def) {
            DataDef_Free(band->Def);
            band->Def=NULL;
         }
         return(band);
      }
   }

   /* Est-ce que le champs existe et si oui, verifier les dimensions */
   band=GDAL_BandCreate(Interp,Name);
   if (!band)
      return(NULL);

   band->Ref=GeoRef_Copy(Band->Ref);
   memcpy(&band->Tex,&Band->Tex,sizeof(TGeoTex));

   if (Def) {
      band->Def=DataDef_Copy(Band->Def);
      if (!band->Def) {
         Tcl_AppendResult(Interp,"\n   GDAL_BandCopy : Unable to allocate data definition",(char*)NULL);
         return(NULL);
      }
   }

   if (Band->Spec->Map) {
      char  *mapname;

      band->Spec->Map=CMap_New(NULL,Band->Spec->Map->NbPixels);
      mapname = band->Spec->Map->Name;
      memcpy(band->Spec->Map,Band->Spec->Map,sizeof(CMap_Rec));
      band->Spec->Map->Name = mapname;
   }
   return(band);
}

/*--------------------------------------------------------------------------------------------------------------
 * Nom          : <GDAL_BandDestroy>
 * Creation     : Mai 2002 J.P. Gauthier & David Dube
 *
 * But          : Destruction d'une image Geotiff a partir de son nom.
 *
 * Parametres   :
 *   <Interp>   : Interpreteur Tcl
 *   <Name>     : Nom de l'image Geotiff a detruire
 *
 * Retour       : Code de retour standard TCL
 *
 * Remarques :
 *    1) Recherche du nom dans la table
 *    2) Obtention du pointeur sur la structure TGeoImage
 *    3) Deallocation dynamique de la structure
 *    4) Retrait de l'entree de la table GeotiffTable
 *
 *---------------------------------------------------------------------------------------------------------------
*/
int GDAL_BandDestroy(Tcl_Interp *Interp,char *Name) {

   GDAL_Band *band;

   if ((band=(GDAL_Band*)TclY_HashDel(&GDAL_BandTable,Name))) {

      /*Wait for it's rendering thread to die*/
      band->Tex.Res=0;
      while(band->Tex.ThreadId) { sleep(1); };

      if (band->Spec) {
         DataSpec_FreeHash(Interp,band->Spec->Name);
      }

      /*Liberation de la memoire allouee pour les textures*/
      GeoTex_Clear(&band->Tex,GEOTEX_CLRALL,0,0);

      if (band->Stat)      free(band->Stat);
      if (band->Tag)       Tcl_DecrRefCount(band->Tag);
      if (band->GCPs)      free(band->GCPs);
      if (band->Def)       DataDef_Free(band->Def);
      if (band->Ref)       GeoRef_Destroy(Interp,band->Ref->Name);

      free(band);
   }
   return(TCL_OK);
}

/*--------------------------------------------------------------------------------------------------------------
 * Nom          : <GDAL_BandGet>
 * Creation     : Juin 2004 - J.P. Gauthier - CMC/CMOE
 *
 * But          : Obtenir une bande en fonction de son nom dans la table de Tcl
 *
 * Parametres   :
 *   <Name>     : Nom de l'objet bande a obtenir.
 *
 * Retour       : Une structure GDAL_Band ou un pointeur NULL si rien trouve.
 *
 * Remarques    :
 *   1) Recherche de l'entree dans la table GDAL_BandTable
 *   2) Obtention du pointeur sur la structure GDAL_Band desiree
 *   3) Retour de cette structure
 *
 *---------------------------------------------------------------------------------------------------------------
*/
GDAL_Band* GDAL_BandGet(char *Name) {
   return((GDAL_Band*)TclY_HashGet(&GDAL_BandTable,Name));
}

/*----------------------------------------------------------------------------
 * Nom      : <GDAL_FileCmd>
 * Creation : Juin 2004 - J.P. Gauthier - CMC/CMOE
 *
 * But      : Appel des commandes relies aux fichiers raster.
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
static int GDAL_FileCmd(ClientData clientData,Tcl_Interp *Interp,int Objc,Tcl_Obj *CONST Objv[]){

   GDAL_File  *file;
   Tcl_Obj     *obj;

   double      x,y,lat0,lon0,lat1,lon1;
   int         n,idx,i,in;
   char        buf[256],**meta,*dri=NULL;
   static CONST char *sopt[] = { "open","close","createcopy","format","driver","width","height","georef","metadata","project","unproject","within","filename","colorinterp","error",NULL };
   enum                opt { OPEN,CLOSE,CREATECOPY,FORMAT,DRIVER,WIDTH,HEIGHT,GEOREF,METADATA,PROJECT,UNPROJECT,WITHIN,FILENAME,COLORINTERP,ERROR };

   Tcl_ResetResult(Interp);

   if (Objc<2) {
      Tcl_WrongNumArgs(Interp,1,Objv,"command ?arg arg ...?");
      return TCL_ERROR;
   }

   if (Tcl_GetIndexFromObj(Interp,Objv[1],sopt,"command",0,&idx)!=TCL_OK) {
      return TCL_ERROR;
   }

   switch ((enum opt)idx) {
      case ERROR:
         if(Objc!=3) {
            Tcl_WrongNumArgs(Interp,2,Objv,"[quiet|normal]");
            return(TCL_ERROR);
         }
         if (strcasecmp(Tcl_GetString(Objv[2]),"quiet")==0) {
            CPLSetErrorHandler(CPLQuietErrorHandler);
         } else if (strcasecmp(Tcl_GetString(Objv[2]),"normal")==0) {
            CPLSetErrorHandler(CPLDefaultErrorHandler);
         } else {
            Tcl_AppendResult(Interp,"invalid error reporting mode must be \"quiet\" or \"normal\"",(char *)NULL);
            return(TCL_ERROR);
         }
         break;

      case OPEN:
         if(Objc!=5 && Objc!=6) {
            Tcl_WrongNumArgs(Interp,2,Objv,"id mode filename ?driver?");
            return(TCL_ERROR);
         }
         if (Objc==6)
            dri=Tcl_GetString(Objv[5]);
         return(GDAL_FileOpen(Interp,Tcl_GetString(Objv[2]),Tcl_GetString(Objv[3])[0],Tcl_GetString(Objv[4]),dri,NULL));
         break;

      case CREATECOPY:
         if(Objc!=5) {
            Tcl_WrongNumArgs(Interp,2,Objv,"filename bands driver");
            return(TCL_ERROR);
         }
         if ((GDAL_FileCreateCopy(Interp,Objv[3],Tcl_GetString(Objv[2]),Tcl_GetString(Objv[4])))==TCL_ERROR) {
            return(TCL_ERROR);
         }
             
         if (strstr(Tcl_GetString(Objv[2]),"/vsimem/")) {
            vsi_l_offset length;
            unsigned char *bytes;
            
            if (!(bytes=VSIGetMemFileBuffer(Tcl_GetString(Objv[2]),&length,TRUE))) {
               Tcl_AppendResult(Interp,"invalid memory buffer",(char*)NULL);
               return(TCL_ERROR);
            }
            
            Tcl_SetObjResult(Interp,Tcl_NewByteArrayObj(bytes,length));
            free(bytes);     
         }
         return(TCL_OK);
      
         break;

      case FORMAT:
         if(Objc!=2 && Objc!=3) {
            Tcl_WrongNumArgs(Interp,2,Objv,"[write]");
            return(TCL_ERROR);
         }

         for (i=0;i<GDALGetDriverCount();i++) {
            GDALDriverH hDriver=GDALGetDriver(i);
            if (Objc==3 && !CSLFetchBoolean(meta=GDALGetMetadata(hDriver,NULL),GDAL_DCAP_CREATE,FALSE)) {
               continue;
            }
            sprintf(buf,"%s \"%s\"",GDALGetDriverLongName(hDriver),GDALGetDriverShortName(hDriver));
            Tcl_AppendElement(Interp,buf);
         }
         break;

      case METADATA:
         if(Objc!=3 && Objc!=4) {
            Tcl_WrongNumArgs(Interp,2,Objv,"id [metadata]");
            return(TCL_ERROR);
         }
         if (!(file=GDAL_FileGet(Interp,Tcl_GetString(Objv[2])))) {
            return(TCL_ERROR);
         }
         if (Objc==4) {
            if (file->Meta)
               Tcl_Free((char*)file->Meta);

            file->Meta=NULL;
            if (Tcl_SplitList(Interp,Tcl_GetString(Objv[3]),&n,(const char***)&file->Meta)==TCL_ERROR) {
               Tcl_AppendResult(Interp,"\n   GDAL_BandCmd : Invalid list of metadata",(char*)NULL);
               return(TCL_ERROR);
            }
         } else {
            obj=Tcl_NewListObj(0,NULL);
            if (file->Meta) {
               i=0;
               while(file->Meta[i]) {
                  Tcl_ListObjAppendElement(Interp,obj,Tcl_NewStringObj(file->Meta[i],-1));
                  i++;
               }
            } else {
               meta=GDALGetMetadata(file->Set,NULL);
               if (CSLCount(meta)) {
                  for (i=0;meta[i];i++) {
                     strtrim(meta[i],' ');
                     Tcl_ListObjAppendElement(Interp,obj,Tcl_NewStringObj(meta[i],-1));
                  }
      //            CSLDestroy(meta);
               }
            }
            Tcl_SetObjResult(Interp,obj);
         }
         break;

     case DRIVER:
         if(Objc!=3) {
            Tcl_WrongNumArgs(Interp,2,Objv,"id");
            return(TCL_ERROR);
         }
         if (!(file=GDAL_FileGet(Interp,Tcl_GetString(Objv[2])))) {
            return(TCL_ERROR);
         }
         obj=Tcl_NewObj();
         Tcl_AppendStringsToObj(obj,GDALGetDriverShortName(file->Driver),"/",GDALGetDriverLongName(file->Driver));
         Tcl_SetObjResult(Interp,obj);
         break;

      case WIDTH:
         if(Objc!=3) {
            Tcl_WrongNumArgs(Interp,2,Objv,"id");
            return(TCL_ERROR);
         }

         if (!(file=GDAL_FileGet(Interp,Tcl_GetString(Objv[2])))) {
            return(TCL_ERROR);
         }
         Tcl_SetObjResult(Interp,Tcl_NewIntObj(GDALGetRasterXSize(file->Set)));
         break;

      case HEIGHT:
         if(Objc!=3) {
            Tcl_WrongNumArgs(Interp,2,Objv,"id");
            return TCL_ERROR;
         }

         if (!(file=GDAL_FileGet(Interp,Tcl_GetString(Objv[2])))) {
            return(TCL_ERROR);
         }
         Tcl_SetObjResult(Interp,Tcl_NewIntObj(GDALGetRasterYSize(file->Set)));
         break;

      case GEOREF:
         if (Objc==3) {
            if (!(file=GDAL_FileGet(Interp,Tcl_GetString(Objv[2])))) {
               return(TCL_ERROR);
            }
            if (file->Ref)
               Tcl_SetObjResult(Interp,Tcl_NewStringObj(file->Ref->Name,-1));
         }
         break;

      case PROJECT:
         if(Objc!=5) {
            Tcl_WrongNumArgs(Interp,2,Objv,"id x y");
            return TCL_ERROR;
         }

         if (!(file=GDAL_FileGet(Interp,Tcl_GetString(Objv[2])))) {
            return(TCL_ERROR);
         }
         Tcl_GetDoubleFromObj(Interp,Objv[3],&x);
         Tcl_GetDoubleFromObj(Interp,Objv[4],&y);
         file->Ref->Project(file->Ref,x,y,&lat0,&lon0,1,1);
         obj=Tcl_NewListObj(0,NULL);
         Tcl_ListObjAppendElement(Interp,obj,Tcl_NewDoubleObj(lat0));
         Tcl_ListObjAppendElement(Interp,obj,Tcl_NewDoubleObj(lon0));
         Tcl_SetObjResult(Interp,obj);
         break;

      case UNPROJECT:
         if(Objc!=5) {
            Tcl_WrongNumArgs(Interp,2,Objv,"id lat lon");
            return TCL_ERROR;
         }

         if (!(file=GDAL_FileGet(Interp,Tcl_GetString(Objv[2])))) {
            return(TCL_ERROR);
         }
         Tcl_GetDoubleFromObj(Interp,Objv[3],&lat0);
         Tcl_GetDoubleFromObj(Interp,Objv[4],&lon0);
         file->Ref->UnProject(file->Ref,&x,&y,lat0,lon0,1,1);
         obj=Tcl_NewListObj(0,NULL);
         Tcl_ListObjAppendElement(Interp,obj,Tcl_NewDoubleObj(x));
         Tcl_ListObjAppendElement(Interp,obj,Tcl_NewDoubleObj(y));
         Tcl_SetObjResult(Interp,obj);
         break;

      case WITHIN:
         if (Objc!=7 && Objc!=8) {
            Tcl_WrongNumArgs(Interp,2,Objv,"id lat0 lon0 lat1 lon1 [included]");
            return TCL_ERROR;
         } else {
             if (!(file=GDAL_FileGet(Interp,Tcl_GetString(Objv[2])))) {
               return(TCL_ERROR);
            } else {
               Tcl_GetDoubleFromObj(Interp,Objv[3],&lat0);
               Tcl_GetDoubleFromObj(Interp,Objv[4],&lon0);
               Tcl_GetDoubleFromObj(Interp,Objv[5],&lat1);
               Tcl_GetDoubleFromObj(Interp,Objv[6],&lon1);
               in=1;
               if (Objc==8)
                 Tcl_GetBooleanFromObj(Interp,Objv[7],&in);

               Tcl_SetObjResult(Interp,Tcl_NewBooleanObj(GeoRef_WithinRange(file->Ref,lat0,lon0,lat1,lon1,in)));
               return(TCL_OK);
            }
         }
         break;

      case CLOSE:
         if(Objc<3) {
            Tcl_WrongNumArgs(Interp,2,Objv,"fileid");
            return(TCL_ERROR);
         }
         for(n=2;n<Objc;n++) {
            GDAL_FileClose(Interp,Tcl_GetString(Objv[n]));
         }
         return(TCL_OK);
         break;

      case FILENAME:
         if(Objc!=3) {
            Tcl_WrongNumArgs(Interp,2,Objv,"id");
            return(TCL_ERROR);
         }
         if (!(file=GDAL_FileGet(Interp,Tcl_GetString(Objv[2])))) {
            return(TCL_ERROR);
         }
         Tcl_SetObjResult(Interp,Tcl_NewStringObj(file->Name,-1));
         return(TCL_OK);
         break;

      case COLORINTERP:
         if(Objc!=3) {
            Tcl_WrongNumArgs(Interp,2,Objv,"id");
            return(TCL_ERROR);
         }
         if (!(file=GDAL_FileGet(Interp,Tcl_GetString(Objv[2])))) {
            return(TCL_ERROR);
         }
         Tcl_SetObjResult(Interp,Tcl_NewStringObj(GDALGetColorInterpretationName(file->ColorInterp),-1));
         return(TCL_OK);
         break;
   }
   return TCL_OK;
}

/*----------------------------------------------------------------------------
 * Nom      : <GDAL_FileClose>
 * Creation : Juin 2004 - J.P. Gauthier - CMC/CMOE
 *
 * But      : Ferme le fichier GDAL.
 *
 * Parametres :
 *  <Interp>  : Interpreteur TCL.
 *  <Id>      : Identificateur du fichier
 *
 * Retour:
 *  <TCL_...> : Code de reussite.
 *
 * Remarques :
 *
 *----------------------------------------------------------------------------
*/
int GDAL_FileClose(Tcl_Interp *Interp,char *Id) {

   GDAL_File *file=NULL;
   int        si;
   char       subid[1024];

   if ((file=(GDAL_File*)TclY_HashDel(&GDAL_FileTable,Id))) {
      for(si=0;si<file->Sub;si++) {
         snprintf(subid,1024,"%s%04i",Id,si/2);
         GDAL_FileClose(Interp,subid);
      }
      if (file->Meta) {
         GDALSetMetadata(file->Set,file->Meta,NULL);
         Tcl_Free((char*)file->Meta);
      }

      GDALClose(file->Set);
      if (file->Ref)
         GeoRef_Destroy(Interp,file->Ref->Name);

      free(file->Id);
      free(file->Name);
      free(file);
   }

   return(TCL_OK);
}

/*----------------------------------------------------------------------------
 * Nom      : <GDAL_FileGet>
 * Creation : Juin 2004 - J.P. Gauthier - CMC/CMOE
 *
 * But      : Extrait l'adresse d'un champs de la liste des champs
 *            connu (HashTable) et en retourne la valeur.
 *
 * Parametres    :
 *  <Interp>     : Interpreteur TCL.
 *  <Name>       : Nom du champ
 *
 * Retour:
 *  <GDAL_File>  : Pointeur sur la structure du fichier
 *
 * Remarques :
 *
 *----------------------------------------------------------------------------
*/
GDAL_File* GDAL_FileGet(Tcl_Interp *Interp,char *Id){

   Tcl_HashEntry *entry;

   if (Id && strlen(Id)>0) {
      entry=TclY_FindHashEntry(&GDAL_FileTable,Id);
      if (!entry) {
         if (Interp) Tcl_AppendResult(Interp,"GDAL_FileGet: Unknown file",(char *)NULL);
         return(NULL);
      } else {
         return((GDAL_File*)(Tcl_GetHashValue(entry)));
      }
   }
   return(NULL);
}

int GDAL_FilePut(Tcl_Interp *Interp,GDAL_File *File){

   Tcl_HashEntry *entry;
   int            new;

   entry=TclY_CreateHashEntry(&GDAL_FileTable,File->Id,&new);

   if (!new) {
      Tcl_AppendResult(Interp,"\n   GDAL_FilePut: File already openned",(char *)NULL);
      return(TCL_ERROR);
   }

   Tcl_SetHashValue(entry,File);

   return TCL_OK;
}

/*----------------------------------------------------------------------------
 * Nom      : <GDAL_FileOpen>
 * Creation : Juin 2004 - J.P. Gauthier - CMC/CMOE
 *
 * But      : Effectue l'ouverture d'un fichier HDF.
 *
 * Parametres :
 *  <Interp>  : Interpreteur TCL.
 *  <Id>      : Identificateur a donner au fichier
 *  <Mode>    : Mode d'ouverture (R ou W)
 *  <Name>    : Non du fichier
 *  <Driver>  : Driver a utiliser (Mode W)
 *  <Desc>    : Descripteur du dataset
 *
 * Retour:
 *  <TCL_...> : Code d'erreur de TCL.
 *  <tcl>     : Liste des parametres des enregistrements contenue dans le fichier
 *
 * Remarques :
 *
 *----------------------------------------------------------------------------
*/
int GDAL_FileOpen(Tcl_Interp *Interp,char *Id,char Mode,char *Name,char *Driver,char *Desc) {

   GDALColorInterp ci=0;
   GDALDatasetH    set=NULL;
   GDALRasterBandH band=NULL;
   GDALDriverH     driver=NULL;
   GDAL_File      *file;
   int             i,si=0;
   char            buf[1024];
   char          **sub,*subid,*idx,*desc;

   if (GDAL_FileGet(NULL,Id)) {
      Tcl_AppendResult(Interp,"GDAL_FileOpen: Cannot reuse openned file identificator ",Id,(char*)NULL);
      return(TCL_ERROR);
   }

   if (Mode=='w' || Mode=='W') {                 /*Write Mode*/
      if (!Driver) {
         Tcl_AppendResult(Interp," GDAL_FileOpen: Invalid driver ",Driver,(char*)NULL);
         return(TCL_ERROR);
      }

      /* Look for the specified driver */
      for(i=0;i<GDALGetDriverCount();i++) {
         driver=GDALGetDriver(i);
         if (EQUAL(Driver,GDALGetDriverShortName(driver))) break;
         if (EQUAL(Driver,GDALGetDriverLongName(driver))) break;
      }

      if (!driver) {
         Tcl_AppendResult(Interp," GDAL_FileOpen: Invalid driver ",Driver,(char*)NULL);
         return(TCL_ERROR);
      }

   } else if (Mode=='a' ||  Mode=='A') {         /*Append Mode*/
      set=GDALOpen(Name,GA_Update);
   } else {                                      /*ReadOnly Mode*/
      set=GDALOpen(Name,GA_ReadOnly);
   }

   if (Mode!='w' && Mode!='W') {
      if (!set) {
         Tcl_AppendResult(Interp," GDAL_FileOpen: Cannot open GDAL file ",Name,(char *)NULL);
         return(TCL_ERROR);
      }
      driver=GDALGetDatasetDriver(set);

      /* Loop over subdatasets */
      sub=GDALGetMetadata(set,"SUBDATASETS");

      if (CSLCount(sub)>0) {
         subid=(char*)alloca(strlen(Id)+8);
         /* Loop over bands */
         for (si=0;sub[si]!=NULL;si++) {
            snprintf(subid,(strlen(Id)+8),"%s%04i",Id,si>>1);

            /*Check for follow up descriptor*/
            desc=NULL;
            if (sub[si+1]!=NULL) {
               idx=index(sub[si+1],'=');
               if (*(idx-1)=='C') {
                  desc=idx+1;
               }
            }

            /*Read sub dataset*/
            idx=index(sub[si],'=');
            if (*(idx-1)=='E') {
               if (GDAL_FileOpen(Interp,subid,Mode,idx+1,NULL,desc)==TCL_ERROR) {
                  return(TCL_ERROR);
               }
            }
         }
      }

      for (i=0;i<GDALGetRasterCount(set);i++) {
         band=GDALGetRasterBand(set,i+1);
         if (!ci) {
            ci=GDALGetRasterColorInterpretation(band);
            ci=ci>=GCI_YCbCr_YBand?GCI_YCbCr_YBand:
               ci>=GCI_CyanBand?GCI_CyanBand:
               ci>=GCI_HueBand?GCI_HueBand:
               ci>=GCI_RedBand?GCI_RedBand:ci;
         }
         if (Desc) {
            snprintf(buf,1024,"%s %i {%s:%s} %d %d",Id,i+1,Desc,GDALGetDescription(band),GDALGetRasterBandXSize(band),GDALGetRasterBandYSize(band));
         } else {
            snprintf(buf,1024,"%s %i {%s} %d %d",Id,i+1,GDALGetDescription(band),GDALGetRasterBandXSize(band),GDALGetRasterBandYSize(band));
         }
         Tcl_AppendElement(Interp,buf);
      }
   }

   if (!(file=(GDAL_File*)malloc(sizeof(GDAL_File)))) {
      Tcl_AppendResult(Interp,"GDAL_FileOpen: Unable to allocate memory for file structure",Id,(char*)NULL);
      return(TCL_ERROR);      
   }
   file->ColorInterp=ci;
   file->Driver=driver;
   file->Mode=Mode;
   file->Id=strdup(Id);
   file->Name=strdup(Name);
   file->Sub=si;
   file->Set=set;
   file->Ref=NULL;
   file->Meta=NULL;

   /* Get the georeference */
   if (band && Mode!='w' && Mode!='W') {
      file->Ref=GDAL_GeoRef(set,band,NULL,0,GDALGetRasterBandXSize(band),GDALGetRasterBandYSize(band));
      GeoRef_Qualify(file->Ref);
   }

   GDAL_FilePut(Interp,file);

   return(TCL_OK);
}

/*----------------------------------------------------------------------------
 * Nom      : <GDAL_GeoRef>
 * Creation : Mars 2011 - J.P. Gauthier - CMC/CMOE
 *
 * But      : Construire un objet georef pour une bande.
 *
 * Parametres :
 *  <Set>     : Dataset.
 *  <Band>    : Bande
 *  <GCPs>    : Liste des points de controle
 *  <NbGCPs>  : Nombre de points de controle
 *  <Nx>      : Dimension en X
 *  <Ny>      : Dimension en Y
 *
 * Retour:
 *  <georef>  : Objet georef
 *
 * Remarques :
 *
 *----------------------------------------------------------------------------
*/
TGeoRef* GDAL_GeoRef(GDALDatasetH Set,GDALRasterBandH Band,GDAL_GCP *GCPs,int NbGCPs,int Nx,int Ny) {

   GDALRPCInfo rpcinfo;

   TGeoRef *ref;
   char    **meta,*projdef,*gdname;
   int      gdtype;
   double   alpha,beta,gamma,xcent,ycent,xcell,ycell,xorig,yorig;
   double   tran[6],inv[6];

   /*Get the metadata pointer*/
   meta=GDALGetMetadata(Set,NULL);

   /*Is this an IOAPI file*/
   if (meta && CSLFetchNameValue(meta,"NC_GLOBAL#IOAPI_VERSION")) {
#ifdef DEBUG
      fprintf(stderr,"(DEBUG) GDAL_GeoRef: Found IOAPI reference\n");
#endif
      projdef=(char*)alloca(1024*sizeof(char));

      /*Extract needed info from metadata*/
      gdname=(char*)CSLFetchNameValue(meta,"NC_GLOBAL#GDNAM");
      gdtype=atoi(CSLFetchNameValue(meta,"NC_GLOBAL#GDTYP"));
      alpha =atof(CSLFetchNameValue(meta,"NC_GLOBAL#P_ALP"));
      beta  =atof(CSLFetchNameValue(meta,"NC_GLOBAL#P_BET"));
      gamma =atof(CSLFetchNameValue(meta,"NC_GLOBAL#P_GAM"));

      xcent=atof(CSLFetchNameValue(meta,"NC_GLOBAL#XCENT"));
      ycent=atof(CSLFetchNameValue(meta,"NC_GLOBAL#YCENT"));
      xorig=atof(CSLFetchNameValue(meta,"NC_GLOBAL#XORIG"));
      yorig=atof(CSLFetchNameValue(meta,"NC_GLOBAL#YORIG"));
      xcell=atof(CSLFetchNameValue(meta,"NC_GLOBAL#XCELL"));
      ycell=atof(CSLFetchNameValue(meta,"NC_GLOBAL#YCELL"));

      switch (gdtype) {
         case 2: /*Lambert*/
            if (abs(beta)==90) {
               beta+=(beta<0)?(0.000001):(-0.000001);
            }
            sprintf(projdef,
                  "PROJCS[\"(%s) NAD83 / IOAPI Lambert Conformal Conic\",GEOGCS[\"NAD83\",DATUM[\"North_American_Datum_1983\",SPHEROID[\"GRS 1980\",6378137,298.257222101]],PRIMEM[\"Greenwich\",0],UNIT[\"degree\",0.0174532925199433]],PROJECTION[\"Lambert_Conformal_Conic_2SP\"],PARAMETER[\"False_Easting\",0.0],PARAMETER[\"False_Northing\",0.0],PARAMETER[\"Central_Meridian\",%f],PARAMETER[\"Standard_Parallel_1\",%f],PARAMETER[\"Standard_Parallel_2\",%f],PARAMETER[\"Latitude_Of_Origin\",%f],UNIT[\"Meter\",1.0]]",
                  gdname,xcent,alpha,beta,ycent);
            break;
         case 5: /*UTM*/
            sprintf(projdef,
                  "PROJCS[\"(%s) NAD83 / IOAPI UTM zone %.0f \",GEOGCS[\"NAD83\",DATUM[\"North_American_Datum_1983\",SPHEROID[\"GRS 1980\",6378137,298.257222101]],PRIMEM[\"Greenwich\",0],UNIT[\"degree\",0.0174532925199433]],PROJECTION[\"Transverse_Mercator\"],PARAMETER[\"latitude_of_origin\",0],PARAMETER[\"central_meridian\",%f],PARAMETER[\"scale_factor\",0.9996],PARAMETER[\"false_easting\",500000],PARAMETER[\"false_northing\",0],UNIT[\"metre\",1]]",
                  gdname,alpha,(6*alpha+3));
            break;
         case 6:  /*Polar Stereograhic*/
            sprintf(projdef,
                  "PROJCS[\"(%s) WGS 84 / IOAPI Polar Stereographic\",GEOGCS[\"WGS 84\",DATUM[\"WGS_1984\",SPHEROID[\"WGS 84\",6378137,298.257223563]],PRIMEM[\"Greenwich\",0],UNIT[\"degree\",0.0174532925199433]],PROJECTION[\"Polar_Stereographic\"],PARAMETER[\"latitude_true_scale\",%f],PARAMETER[\"latitude_of_origin\",%f],PARAMETER[\"central_meridian\",%f],PARAMETER[\"scale_factor\",0.9341],PARAMETER[\"false_easting\",0.0],PARAMETER[\"false_northing\",0.0],UNIT[\"metre\",1]]",
                  gdname,beta,ycent,gamma);
            break;
      }

      tran[0]=xorig;tran[1]=xcell;tran[2]=0.0;
      tran[3]=yorig;tran[4]=0.0;tran[5]=ycell;
      GDALInvGeoTransform(tran,inv);
      ref=GeoRef_WKTSetup(GDALGetRasterBandXSize(Band),GDALGetRasterBandYSize(Band),1,LVL_UNDEF,NULL,NULL,0,0,0,0,projdef,tran,inv,NULL);
   } else {
      projdef=GDALGetProjectionRef(Set);
      
      /*Get the projection transform*/
      if (GCPs) {
         /*If there are Ground Control Points*/
         projdef=GDALGetGCPProjection(Set);
         ref=GeoRef_WKTSetup(Nx,Ny,1,0,NULL,NULL,0,0,0,0,projdef,NULL,NULL,NULL);

#ifdef DEBUG
         fprintf(stdout,"(DEBUG) GGDAL_GeoRef: Using GCPs to get transform\n");
#endif

         /*Try to create GPC trasnform*/
         if (!(ref->GCPTransform=(void*)GDALCreateGCPTransformer(NbGCPs,GCPs,3,FALSE))) {
            fprintf(stdout,"(WARNING) GDAL_GeoRef: Unable to fit control points with GDALCreateGCPTransformer\n");

            /*If does not work, try to get a simple transform*/
            if (GDALGCPsToGeoTransform(NbGCPs,GCPs,tran,TRUE)) {
               ref->Transform=(double*)calloc(6,sizeof(double));
               ref->InvTransform=(double*)calloc(6,sizeof(double));
               memcpy(ref->Transform,tran,6*sizeof(double));
               if (!GDALInvGeoTransform(ref->Transform,ref->InvTransform)) {
                  fprintf(stdout,"(WARNING) GDAL_GeoRef: Unable to get inverse transform\n");
               }
            } else {
               fprintf(stdout,"(WARNING) GDAL_GeoRef: Unable to fit control points with GDALGCPsToGeoTransform\n");
            }
         }
      } else if (meta && 0) {
//GDALExtractRPCInfo(meta,&rpcinfo)
         /*Get the transform from RPCInfo*/
         ref=GeoRef_WKTSetup(Nx,Ny,1,0,NULL,NULL,0,0,0,0,projdef,NULL,NULL,NULL);

#ifdef DEBUG
         fprintf(stdout,"(DEBUG) GDAL_GeoRef: Using RPC Info to get transform\n");
#endif
         if (!(ref->RPCTransform=(void*)GDALCreateRPCTransformer(&rpcinfo,FALSE,0.1,NULL))) {
            printf("(WARNING) GDAL_GeoRef: Unable to fit RPC\n");
         }
      } else {
         GDALGetGeoTransform(Set,tran);
         GDALInvGeoTransform(tran,inv);
         ref=GeoRef_WKTSetup(Nx,Ny,1,LVL_UNDEF,NULL,NULL,0,0,0,0,projdef,tran,inv,NULL);
      }
   }

   GeoRef_Size(ref,0,0,0,Nx-1,Ny-1,0,0);

   return(ref);
}

/*--------------------------------------------------------------------------------------------------------------
 * Nom          : <GDAL_FileCreateCopy>
 * Creation     : Septembre 2010 J.P. Gauthier - CMC/CMOE
 *
 * But          : Ecrire les donnees d'une bande raster en utilisant le mode copy
 *
 * Parametres   :
 *   <Interp>   : L'interpreteur Tcl
 *   <Bands>    : Liste des bandes a sauvegarder
 *   <Name>     : Path du fichier
 *   <Driver>   : Driver a utiliser
 *
 * Retour       : Code d'erreur standard TCL
 *
 * Remarques    :
 *
 *---------------------------------------------------------------------------------------------------------------
*/
int GDAL_FileCreateCopy(Tcl_Interp *Interp,Tcl_Obj *Bands,char *Name,char *Driver) {

   Tcl_Obj        *obj;
   GDAL_Band      *band=NULL;
   GDALColorTableH htable;
   GDALColorEntry  centry;
   GDALDatasetH    dts,vds;
   GDALDriverH     dro,drs;
   int             i,ns,nc,n,c;
   char           *str,buf[64],*csl[2];

   Tcl_ListObjLength(Interp,Bands,&ns);
   if (!ns) {
      Tcl_AppendResult(Interp,"GDAL_FileCreateCopy: No band specified",(char*)NULL);
      return(TCL_ERROR);
   }

   Tcl_ListObjIndex(Interp,Bands,0,&obj);
   nc=0;
   for(n=0;n<ns;n++) {
      band=GDAL_BandGet(Tcl_GetString(obj));
      if (!band) {
         Tcl_AppendResult(Interp,"GDAL_FileCreateCopy: Invalid band ",Tcl_GetString(obj),(char*)NULL);
         return(TCL_ERROR);
      }
      nc+=band->Def->NC;
   }

   /*Create an in memory and output dataset*/
   if (!(dro=GDALGetDriverByName(Driver))) {
      Tcl_AppendResult(Interp,"GDAL_FileCreateCopy: Invalid driver ",Driver,(char*)NULL);
      return(TCL_ERROR);
   }

   if (!(drs=GDALGetDriverByName("MEM"))) {
      Tcl_AppendResult(Interp,"GDAL_FileCreateCopy: Could not initialise VRT driver",(char*)NULL);
      return(TCL_ERROR);
   }
   
   if (!(vds=GDALCreate(drs,"",band->Def->NI,band->Def->NJ,0,TD2GDAL[band->Def->Type],NULL))) {
      Tcl_AppendResult(Interp,"GDAL_FileCreateCopy: Could not create in memory dataset",(char*)NULL);
      return(TCL_ERROR);
   }

   /*Set the georeference stuff*/
   if (band->NbGCPs) {
      /*Set GCPS*/
      GDALSetGCPs(vds,band->NbGCPs,band->GCPs,band->Ref->String);
   } else {
      /*Set Transform*/
      if (band->Ref) {
         if (band->Ref->Transform)
            GDALSetGeoTransform(vds,band->Ref->Transform);

         if (band->Ref->Spatial) {
            OSRExportToWkt(band->Ref->Spatial,&str);
            GDALSetProjection(vds,str);
            OGRFree(str);
         }
      }
   }

   nc=0;
   for(n=0;n<ns;n++) {
      Tcl_ListObjIndex(Interp,Bands,n,&obj);
      band=GDAL_BandGet(Tcl_GetString(obj));

      /*Write every band*/
      for(i=0;i<band->Def->NC;i++) {

         /*Pass in the memory address of the data*/
         sprintf(buf,"DATAPOINTER=%p",band->Def->Data[i]);
         csl[0]=buf;csl[1]=NULL;
         GDALAddBand(vds,TD2GDAL[band->Def->Type],csl);

         /*Set band description if any*/
         band->Band[i]=GDALGetRasterBand(vds,nc+1);
         if (band->Spec->Desc)
            GDALSetDescription(band->Band[i],band->Spec->Desc);

         /*Set nodata value if any*/
         if (!isnan(band->Def->NoData))
            GDALSetRasterNoDataValue(band->Band[i],band->Def->NoData);

         /*Set the colormap if any*/
         if (nc==0 && band->Spec->Map) {
            htable=GDALCreateColorTable(GPI_RGB);
            for (c=0;c<band->Spec->Map->NbPixels;c++) {
               centry.c1=band->Spec->Map->Color[c][0];
               centry.c2=band->Spec->Map->Color[c][1];
               centry.c3=band->Spec->Map->Color[c][2];
               centry.c4=band->Spec->Map->Color[c][3];
               GDALSetColorEntry(htable,c,&centry);
            }
            GDALSetRasterColorTable(band->Band[i],htable);

            if (band->Tex.Indexed) {
               GDALSetRasterColorInterpretation(band->Band[i],GCI_PaletteIndex);
            }
            GDALDestroyColorTable(htable);
         }
         nc++;
      }
   }

   dts=GDALCreateCopy(dro,Name,vds,FALSE,NULL,NULL,NULL);

   GDALClose(vds);

   if (dts) {
      GDALClose(dts);
   } else {
      Tcl_AppendResult(Interp,"Could not create dataset ",Name,(char*)NULL);
      return(TCL_ERROR);
   }

   return(TCL_OK);
}

/*--------------------------------------------------------------------------------------------------------------
 * Nom          : <GDAL_GetMapImage>
 * Creation     : Septembre 2010 J.P. Gauthier - CMC/CMOE
 *
 * But          : Remplir un bande gdal avec la representation de la palette
 *
 * Parametres   :
 *   <Interp>   : L'interpreteur Tcl
 *   <Band>     : Bande
 *
 * Retour       : Code d'erreur standard TCL
 *
 * Remarques    :
 *
 *---------------------------------------------------------------------------------------------------------------
*/
int GDAL_GetMapImage(Tcl_Interp *Interp,GDAL_Band *Band) {

   Tcl_Obj *obj;
   double   incry,val;
   int      cidx,idx,x,y,n;
   char     buf[32];

   if (!Band || !Band->Spec || !Band->Spec->Map) {
      Tcl_AppendResult(Interp,"GDAL_GetMapImage: Invalid field or missing colormap",(char*)NULL);
      return(TCL_ERROR);
   }
   if (Band->Def->NC!=1 && Band->Def->NC!=3 && Band->Def->NC!=4) {
      Tcl_AppendResult(Interp,"GDAL_GetMapImage: Invalid number of bands, must be 1, 3 or 4",(char*)NULL);
      return(TCL_ERROR);
   }

   DataSpec_Define(Band->Spec);

   obj=Tcl_NewListObj(0,NULL);

   if (Band->Spec->InterNb) {

      /*Get value increment*/
      incry=(double)Band->Spec->InterNb/Band->Def->NJ;
      n=0;
      for(y=0;y<Band->Def->NJ;y++) {

         /*Get color for this pixel*/
         n=y*incry;
         VAL2COL(cidx,Band->Spec,Band->Spec->Inter[n]);

         /*Add to the list of values*/
         DataSpec_Format(Band->Spec,VAL2SPEC(Band->Spec,Band->Spec->Inter[n]),buf);
         Tcl_ListObjAppendElement(Interp,obj,Tcl_NewStringObj(buf,-1));

         /*Set the pixel value for the whole width*/
         for(x=0;x<Band->Def->NI;x++) {
            idx=(Band->Def->NJ-y-1)*Band->Def->NI+x;
            if (Band->Def->NC==1) {
               Def_Set(Band->Def,0,idx,cidx);
            } else {
               Def_Set(Band->Def,0,idx,Band->Spec->Map->Color[cidx][0]);
               Def_Set(Band->Def,1,idx,Band->Spec->Map->Color[cidx][1]);
               Def_Set(Band->Def,2,idx,Band->Spec->Map->Color[cidx][2]);
               if (Band->Def->NC==4) {
                  Def_Set(Band->Def,3,idx,Band->Spec->Map->Color[cidx][3]);
               }
            }
         }
      }

   } else {
      /*Get value increment*/
      incry=(double)(Band->Spec->Max-Band->Spec->Min)/(Band->Def->NJ);

      for(y=0;y<Band->Def->NJ;y++) {

         /*Get color for this pixel*/
         val=Band->Spec->Min+y*incry;
         VAL2COL(cidx,Band->Spec,val);

         /*Add to the list of values*/
         DataSpec_Format(Band->Spec,VAL2SPEC(Band->Spec,val),buf);
         Tcl_ListObjAppendElement(Interp,obj,Tcl_NewStringObj(buf,-1));

         /*Set the pixel value for the whole width*/
         for(x=0;x<Band->Def->NI;x++) {
            idx=(Band->Def->NJ-y-1)*Band->Def->NI+x;
            if (Band->Def->NC==1) {
               Def_Set(Band->Def,0,idx,cidx);
            } else {
               Def_Set(Band->Def,0,idx,Band->Spec->Map->Color[cidx][0]);
               Def_Set(Band->Def,1,idx,Band->Spec->Map->Color[cidx][1]);
               Def_Set(Band->Def,2,idx,Band->Spec->Map->Color[cidx][2]);
               if (Band->Def->NC==4) {
                  Def_Set(Band->Def,3,idx,Band->Spec->Map->Color[cidx][3]);
               }
            }
         }
      }
   }

   Tcl_SetObjResult(Interp,obj);

   return(TCL_OK);
}
