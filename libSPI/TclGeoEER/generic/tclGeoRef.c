/*=========================================================
 * Environnement Canada
 * Centre Meteorologique Canadien
 * 2100 Trans-Canadienne
 * Dorval, Quebec
 *
 * Projet       : Lecture et traitements de fichiers raster
 * Fichier      : tclGeoRef.c
 * Creation     : Mars 2005 - J.P. Gauthier
 *
 * Description  : Fonctions de manipulations de projections.
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
 *=========================================================
 */

#include "App.h"
#include "tclDataSpec.h"
#include "RPN.h"
#include "List.h"
#include "tclGeoRef.h"

static int GeoRefInit=0;
static Tcl_HashTable GeoRef_Table;
static TList        *GeoPos_Table;
static int GeoRef_Cmd(ClientData clientData,Tcl_Interp *Interp,int Objc,Tcl_Obj *CONST Objv[]);

static long GeoRef_TableNo;

static int GeoRef_Cmd(ClientData clientData,Tcl_Interp *Interp,int Objc,Tcl_Obj *CONST Objv[]);
static int GeoRef_Define(Tcl_Interp *Interp,char *Name,int Objc,Tcl_Obj *CONST Objv[]);

TGeoScan GScan;

int TclGeoRef_Init(Tcl_Interp *Interp) {

   Tcl_CreateObjCommand(Interp,"georef",GeoRef_Cmd,(ClientData)NULL,(Tcl_CmdDeleteProc*)NULL);

   if (!GeoRefInit++) {
      Tcl_InitHashTable(&GeoRef_Table,TCL_STRING_KEYS);
      GeoRef_TableNo=0;

      GeoScan_Init(&GScan);
   }

   return(TCL_OK);
}

/*--------------------------------------------------------------------------------------------------------------
 * Nom          : <GeoRef_Cmd>
 * Creation     : Juillet 2005 J.P. Gauthier - CMC/CMOE
 *
 * But          : Effectuer les commandes du package
 *
 * Parametres    :
 *  <clientData> : Nom de l'objet
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
static int GeoRef_Cmd(ClientData clientData,Tcl_Interp *Interp,int Objc,Tcl_Obj *CONST Objv[]) {

   double      x,y,lat0,lon0,lat1,lon1,dx,dy;
   int         idx,in=0,x0,y0,x1,y1,n;
   char       *proj=NULL;
   TGeoRef    *gref0,*gref1;
   Tcl_Obj    *lst;

   static CONST char *sopt[] = { "create","copy","free","define","export","project","unproject","limit","size","within","intersect","is","isequal","all","wipe",NULL };
   enum                opt { CREATE,COPY,FREE,DEFINE,EXPORT,PROJECT,UNPROJECT,LIMIT,SIZE,WITHIN,INTERSECT,IS,ISEQUAL,ALL,WIPE };

   Tcl_ResetResult(Interp);

   if (Objc<2) {
      Tcl_WrongNumArgs(Interp,1,Objv,"command ?arg arg ...?");
      return(TCL_ERROR);
   }

   if (Tcl_GetIndexFromObj(Interp,Objv[1],sopt,"command",TCL_EXACT,&idx)!=TCL_OK) {
      return(TCL_ERROR);
   }

   switch ((enum opt)idx) {
      case CREATE:
         if (Objc!=3 && Objc!=4) {
            Tcl_WrongNumArgs(Interp,2,Objv,"georef [wkt string]");
            return(TCL_ERROR);
         }

         gref0=GeoRef_New();

         if (Objc==4) {
            if (!GeoRef_WKTSet(gref0,Tcl_GetString(Objv[3]),NULL,NULL,NULL)) {
               Tcl_AppendResult(Interp,"\n   GeoRef_Cmd: Unable to create georef",(char*)NULL);
               return(TCL_ERROR);
            }
            gref0->Grid[0]='W';
            gref0->Grid[1]=gref0->Grid[2]='\0';
         }
                 
         if (!GeoRef_Put(Interp,Tcl_GetString(Objv[2]),gref0)) {
            Tcl_AppendResult(Interp,"\n   GeoRef_Cmd: Unable to create georef",(char*)NULL);
            return(TCL_ERROR);
         }
         break;

      case COPY:
         if (Objc!=4) {
            Tcl_WrongNumArgs(Interp,2,Objv,"georefto georeffrom");
            return(TCL_ERROR);
         }
         gref0=GeoRef_Get(Tcl_GetString(Objv[3]));
         if (!gref0) {
            Tcl_AppendResult(Interp,"\n   GeoRef_Cmd invalid georeference object",(char*)NULL);
            return(TCL_ERROR);
         }

         gref0=GeoRef_HardCopy(gref0);

         if (!GeoRef_Put(Interp,Tcl_GetString(Objv[2]),gref0)) {
            Tcl_AppendResult(Interp,"\n   GeoRef_Cmd: Unable to create georef",(char*)NULL);
            return(TCL_ERROR);
         }
         break;

      case FREE:
         if(Objc<3) {
            Tcl_WrongNumArgs(Interp,2,Objv,"georef");
            return(TCL_ERROR);
         }
         for(n=2;n<Objc;n++) {
            GeoRef_Destroy(Interp,Tcl_GetString(Objv[n]));
         }
         return(TCL_OK);
         break;

      case DEFINE:
         if (Objc<3) {
            Tcl_WrongNumArgs(Interp,2,Objv,"georef ?option?");
            return(TCL_ERROR);
         }
         return(GeoRef_Define(Interp,Tcl_GetString(Objv[2]),Objc-3,Objv+3));
         break;

      case EXPORT:
         if (Objc!=4) {
            Tcl_WrongNumArgs(Interp,2,Objv,"georef [WKT,PROJ4,MAPINFO,XML]");
            return(TCL_ERROR);
         }
         gref0=GeoRef_Get(Tcl_GetString(Objv[2]));
         if (strcmp(Tcl_GetString(Objv[3]),"PROJ4")==0) {
            OSRExportToProj4(gref0->Spatial,&proj);
         } else if (strcmp(Tcl_GetString(Objv[3]),"MAPINFO")==0) {
//            OSRExportToMICopordSys(gref0->Spatial,&proj);
         } else if (strcmp(Tcl_GetString(Objv[3]),"XML")==0) {
            OSRExportToXML(gref0->Spatial,&proj,NULL);
         } else {
            OSRExportToWkt(gref0->Spatial,&proj);
         }
         if (proj) {
            Tcl_SetObjResult(Interp,Tcl_NewStringObj(proj,-1));
            OGRFree(proj);
         }
         break;

      case PROJECT:
         if (Objc!=5) {
            Tcl_WrongNumArgs(Interp,2,Objv,"georef X Y");
            return(TCL_ERROR);
         }

         Tcl_GetDoubleFromObj(Interp,Objv[3],&x);
         Tcl_GetDoubleFromObj(Interp,Objv[4],&y);

         gref0=GeoRef_Get(Tcl_GetString(Objv[2]));
         if (!gref0 || !gref0->Project) {
            Tcl_AppendResult(Interp,"\n   GeoRef_Cmd invalid georeference object",(char*)NULL);
            return(TCL_ERROR);
         } else {
            lst=Tcl_NewListObj(0,NULL);
            gref0->Project(gref0,x,y,&lat0,&lon0,1,1);
            Tcl_ListObjAppendElement(Interp,lst,Tcl_NewDoubleObj(lat0));
            Tcl_ListObjAppendElement(Interp,lst,Tcl_NewDoubleObj(lon0));
            Tcl_SetObjResult(Interp,lst);
            return(TCL_OK);
         }
         break;

      case UNPROJECT:
         if (Objc!=5) {
            Tcl_WrongNumArgs(Interp,2,Objv,"georef lat lon");
            return(TCL_ERROR);
         }

         Tcl_GetDoubleFromObj(Interp,Objv[3],&lat0);
         Tcl_GetDoubleFromObj(Interp,Objv[4],&lon0);

         gref0=GeoRef_Get(Tcl_GetString(Objv[2]));
         if (!gref0 || !gref0->UnProject) {
            Tcl_AppendResult(Interp,"\n   GeoRef_Cmd invalid georeference object",(char*)NULL);
            return(TCL_ERROR);
         } else {
            lst=Tcl_NewListObj(0,NULL);
            gref0->UnProject(gref0,&x,&y,lat0,lon0,1,1);
            Tcl_ListObjAppendElement(Interp,lst,Tcl_NewDoubleObj(x));
            Tcl_ListObjAppendElement(Interp,lst,Tcl_NewDoubleObj(y));
            Tcl_SetObjResult(Interp,lst);
            return(TCL_OK);
         }
         break;

      case WITHIN:
         if (Objc!=4 && Objc!=7 && Objc!=8) {
            Tcl_WrongNumArgs(Interp,2,Objv,"georefto (lat0 lon0 lat1 lon1 [included]) | georeffrom");
            return(TCL_ERROR);
         } else {
            gref0=GeoRef_Get(Tcl_GetString(Objv[2]));
            if (!gref0) {
               Tcl_AppendResult(Interp,"\n   GeoRef_Cmd invalid georeference object",(char*)NULL);
               return(TCL_ERROR);
            } else {
               if (Objc==4) {
                  gref1=GeoRef_Get(Tcl_GetString(Objv[3]));
                  if (!gref1) {
                     Tcl_AppendResult(Interp,"\n   GeoRef_Cmd invalid georeference object",(char*)NULL);
                     return(TCL_ERROR);
                  }
                  Tcl_SetObjResult(Interp,Tcl_NewBooleanObj(GeoRef_Within(gref0,gref1)));
                  return(TCL_OK);
               }
               Tcl_GetDoubleFromObj(Interp,Objv[3],&lat0);
               Tcl_GetDoubleFromObj(Interp,Objv[4],&lon0);
               Tcl_GetDoubleFromObj(Interp,Objv[5],&lat1);
               Tcl_GetDoubleFromObj(Interp,Objv[6],&lon1);
               if (Objc==8)
                 Tcl_GetBooleanFromObj(Interp,Objv[7],&in);

               Tcl_SetObjResult(Interp,Tcl_NewBooleanObj(GeoRef_WithinRange(gref0,lat0,lon0,lat1,lon1,in)));
               return(TCL_OK);
            }
         }
         break;

      case LIMIT:
         if (Objc!=3) {
            Tcl_WrongNumArgs(Interp,2,Objv,"georef");
            return(TCL_ERROR);
         } else {
            gref0=GeoRef_Get(Tcl_GetString(Objv[2]));
            if (!gref0) {
               Tcl_AppendResult(Interp,"\n   GeoRef_Cmd invalid georeference object",(char*)NULL);
               return(TCL_ERROR);
            } else {
               GeoRef_Limits(gref0,&lat0,&lon0,&lat1,&lon1);
               lst=Tcl_NewListObj(0,NULL);
               Tcl_ListObjAppendElement(Interp,lst,Tcl_NewDoubleObj(lat0));
               Tcl_ListObjAppendElement(Interp,lst,Tcl_NewDoubleObj(lon0));
               Tcl_ListObjAppendElement(Interp,lst,Tcl_NewDoubleObj(lat1));
               Tcl_ListObjAppendElement(Interp,lst,Tcl_NewDoubleObj(lon1));
               Tcl_SetObjResult(Interp,lst);
               return(TCL_OK);
            }
         }
         break;

      case SIZE:
         if (Objc!=3) {
            Tcl_WrongNumArgs(Interp,2,Objv,"georef");
            return(TCL_ERROR);
         } else {
            gref0=GeoRef_Get(Tcl_GetString(Objv[2]));
            if (!gref0) {
               Tcl_AppendResult(Interp,"\n   GeoRef_Cmd invalid georeference object",(char*)NULL);
               return(TCL_ERROR);
            } else {
               GeoRef_Limits(gref0,&lat0,&lon0,&lat1,&lon1);

               lat0=DEG2RAD(lat0);
               lon0=DEG2RAD(lon0);
               lat1=DEG2RAD(lat1);
               lon1=DEG2RAD(lon1);
               x=DIST(EARTHRADIUS,lat0,lon0,lat0,lon1);
               dx=DIST(EARTHRADIUS,lat1,lon0,lat1,lon1);
               dx=fmax(x,dx);

               y=DIST(EARTHRADIUS,lat0,lon0,lat1,lon0);
               dy=DIST(EARTHRADIUS,lat0,lon1,lat1,lon1);
               dy=fmax(y,dy);

               lst=Tcl_NewListObj(0,NULL);

               Tcl_ListObjAppendElement(Interp,lst,Tcl_NewDoubleObj(dx));
               Tcl_ListObjAppendElement(Interp,lst,Tcl_NewDoubleObj(dy));
               Tcl_SetObjResult(Interp,lst);
               return(TCL_OK);
            }
         }
         break;

      case INTERSECT:
         if (Objc!=4) {
            Tcl_WrongNumArgs(Interp,2,Objv,"georef0 georef1");
            return TCL_ERROR;
         } else {
            gref0=GeoRef_Get(Tcl_GetString(Objv[2]));
            gref1=GeoRef_Get(Tcl_GetString(Objv[3]));
            if (!gref0 || !gref1) {
               Tcl_AppendResult(Interp,"\n   GeoRef_Cmd invalid georeference object",(char*)NULL);
               return TCL_ERROR;
            } else {
               lst=Tcl_NewListObj(0,NULL);
               if (GeoRef_Intersect(gref0,gref1,&x0,&y0,&x1,&y1,0)) {
                  Tcl_ListObjAppendElement(Interp,lst,Tcl_NewIntObj(x0));
                  Tcl_ListObjAppendElement(Interp,lst,Tcl_NewIntObj(y0));
                  Tcl_ListObjAppendElement(Interp,lst,Tcl_NewIntObj(x1));
                  Tcl_ListObjAppendElement(Interp,lst,Tcl_NewIntObj(y1));
               }
               Tcl_SetObjResult(Interp,lst);
               return(TCL_OK);
            }
         }
         break;

      case IS:
         if (Objc!=3) {
            Tcl_WrongNumArgs(Interp,2,Objv,"georef");
            return(TCL_ERROR);
         }
         if (GeoRef_Get(Tcl_GetString(Objv[2]))) {
            Tcl_SetObjResult(Interp,Tcl_NewBooleanObj(1));
         } else {
            Tcl_SetObjResult(Interp,Tcl_NewBooleanObj(0));
         }
         break;

      case ISEQUAL:
         if (Objc!=4) {
            Tcl_WrongNumArgs(Interp,2,Objv,"georef1 georef2");
            return(TCL_ERROR);
         }
         Tcl_SetObjResult(Interp,Tcl_NewBooleanObj(GeoRef_Equal(GeoRef_Get(Tcl_GetString(Objv[2])),GeoRef_Get(Tcl_GetString(Objv[3])))));
         break;

      case ALL:
         TclY_HashAll(Interp,&GeoRef_Table);
         break;

      case WIPE:
         break;
   }
   return TCL_OK;
}

/*--------------------------------------------------------------------------------------------------------------
 * Nom          : <GeoRef_Define>
 * Creation     : Juillet 2005 J.P. Gauthier - CMC/CMOE
 *
 * But          : Effectuer les commandes de definition des georef
 *
 * Parametres    :
 *  <clientData> : Nom de l'objet
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
static int GeoRef_Define(Tcl_Interp *Interp,char *Name,int Objc,Tcl_Obj *CONST Objv[]){

   int          i,j,ni,nj,idx,nidx;
   double       tra[6],inv[6],*tm,*im,dx,dy,lat0,lon0,lat1,lon1,latr,lonr,cfl;
   TGeoRef     *gref;
   Tcl_Obj     *obj;

   static CONST char *sopt[] = { "-rpn","-projection","-transform","-invtransform","-extent","-size","-location","-type","-border","-grid",NULL };
   enum        opt { RPN,PROJECTION,TRANSFORM,INVTRANSFORM,EXTENT,SIZE,LOCATION,TYPE,BORDER,GRID };

   gref=GeoRef_Get(Name);
   if (!gref) {
      Tcl_AppendResult(Interp,"\n   GeoRef_Define: Georef name unknown: \"",Name,"\"",(char *)NULL);
      return TCL_ERROR;
   }

   for(i=0;i<Objc;i++) {

      if (Tcl_GetIndexFromObj(Interp,Objv[i],sopt,"option",TCL_EXACT,&idx)!=TCL_OK) {
         return TCL_ERROR;
      }

      switch ((enum opt)idx) {

         case RPN:
            if(Objc!=12) {
               Tcl_WrongNumArgs(Interp,2,Objv,"ni nj dx dy lat0 lon0 lat1 lon1 latr lonr cfl");
               return(TCL_ERROR);
            }
            Tcl_GetIntFromObj(Interp,Objv[++i],&ni);
            Tcl_GetIntFromObj(Interp,Objv[++i],&nj);
            Tcl_GetDoubleFromObj(Interp,Objv[++i],&dx);
            Tcl_GetDoubleFromObj(Interp,Objv[++i],&dy);
            Tcl_GetDoubleFromObj(Interp,Objv[++i],&lat0);
            Tcl_GetDoubleFromObj(Interp,Objv[++i],&lon0);
            Tcl_GetDoubleFromObj(Interp,Objv[++i],&lat1);
            Tcl_GetDoubleFromObj(Interp,Objv[++i],&lon1);
            Tcl_GetDoubleFromObj(Interp,Objv[++i],&latr);
            Tcl_GetDoubleFromObj(Interp,Objv[++i],&lonr);
            Tcl_GetDoubleFromObj(Interp,Objv[++i],&cfl);
            if (GeoRef_RPNGridZE(gref,ni,nj,dx,dy,latr,lonr,cfl,lat0,lon0,lat1,lon1)) {
               return(TCL_OK);
            } else {
               return(TCL_ERROR);
            }
            break;

         case GRID:
            if (Objc==1) {
               Tcl_SetObjResult(Interp,Tcl_NewIntObj(gref->NId));
            } else {
#ifdef HAVE_RMN
               Tcl_GetIntFromObj(Interp,Objv[++i],&nidx);
               if (gref->Ids) {
                  if (nidx>gref->NbId) {
                     Tcl_AppendResult(Interp,"\n   GeoRef_Define: Invalid subgrid index",(char*)NULL);
                     return(TCL_ERROR);
                  } else {
                     int ni,nj,ig;
                     char grtyp[2];

                     // If the subgrid index is different from the current
                     if (gref->Ids && nidx!=gref->NId && nidx<=gref->NbId) {
                        gref->NId=nidx;

                        // Define grid limits
                        c_ezgprm(gref->Ids[nidx],grtyp,&ni,&nj,&ig,&ig,&ig,&ig);
                        gref->X0=0;    gref->Y0=0;
                        gref->X1=ni-1; gref->Y1=nj-1;
                     }
                  }
               }
#else
               Tcl_AppendResult(Interp,"\n   GeoRef_Define: RMNLIB support not included",(char*)NULL);
               return(TCL_ERROR);
#endif
            }
            break;

        case TYPE:
            if (Objc==1) {
               obj=Tcl_NewListObj(0,NULL);
               if (gref->Type&GRID_REGULAR) {
                  Tcl_ListObjAppendElement(Interp,obj,Tcl_NewStringObj("REGULAR",-1));
               }
               if (gref->Type&GRID_VARIABLE) {
                  Tcl_ListObjAppendElement(Interp,obj,Tcl_NewStringObj("VARIABLE",-1));
               }
               if (gref->Type&GRID_WRAP) {
                  Tcl_ListObjAppendElement(Interp,obj,Tcl_NewStringObj("WRAP",-1));
               }
               if (gref->Type&GRID_SPARSE) {
                  Tcl_ListObjAppendElement(Interp,obj,Tcl_NewStringObj("SPARSE",-1));
               }
               if (gref->Type&GRID_TILE) {
                  Tcl_ListObjAppendElement(Interp,obj,Tcl_NewStringObj("TILE",-1));
               }
               if (gref->Type&GRID_VERTICAL) {
                  Tcl_ListObjAppendElement(Interp,obj,Tcl_NewStringObj("VERTICAL",-1));
               }
               if (gref->Type&GRID_RADIAL) {
                  Tcl_ListObjAppendElement(Interp,obj,Tcl_NewStringObj("RADIAL",-1));
               }
               if (gref->Type&GRID_REPEAT) {
                  Tcl_ListObjAppendElement(Interp,obj,Tcl_NewStringObj("REPEAT",-1));
               }
               if (gref->Type&GRID_PSEUDO) {
                  Tcl_ListObjAppendElement(Interp,obj,Tcl_NewStringObj("PSEUDO",-1));
               }
               if (gref->Type&GRID_NUNORTH) {
                  Tcl_ListObjAppendElement(Interp,obj,Tcl_NewStringObj("NUNORTH",-1));
               }
               if (gref->Type&GRID_NEGLON) {
                  Tcl_ListObjAppendElement(Interp,obj,Tcl_NewStringObj("NEGLON",-1));
               }
               Tcl_SetObjResult(Interp,obj);
            } else {
               if (Tcl_ListObjLength(Interp,Objv[++i],&nidx)==TCL_ERROR) {
                  return(TCL_ERROR);
               }

               gref->Type=GRID_NONE;
               for(j=0;j<nidx;j++) {
                  Tcl_ListObjIndex(Interp,Objv[i],j,&obj);
                  if (strcmp(Tcl_GetString(obj),"REGULAR")==0) {
                     gref->Type|=GRID_REGULAR;
                  }
                  if (strcmp(Tcl_GetString(obj),"VARIABLE")==0) {
                     gref->Type|=GRID_VARIABLE;
                  }
                  if (strcmp(Tcl_GetString(obj),"WRAP")==0) {
                     gref->Type|=GRID_WRAP;
                  }
                  if (strcmp(Tcl_GetString(obj),"SPARSE")==0) {
                     gref->Type|=GRID_SPARSE;
                  }
                  if (strcmp(Tcl_GetString(obj),"TILE")==0) {
                     gref->Type|=GRID_TILE;
                  }
                  if (strcmp(Tcl_GetString(obj),"VERTICAL")==0) {
                     gref->Type|=GRID_VERTICAL;
                  }
                  if (strcmp(Tcl_GetString(obj),"RADIAL")==0) {
                     gref->Type|=GRID_RADIAL;
                  }
                  if (strcmp(Tcl_GetString(obj),"REPEAT")==0) {
                     gref->Type|=GRID_REPEAT;
                  }
                  if (strcmp(Tcl_GetString(obj),"PSEUDO")==0) {
                     gref->Type|=GRID_PSEUDO;
                  }
                  if (strcmp(Tcl_GetString(obj),"NUNORTH")==0) {
                     gref->Type|=GRID_NUNORTH;
                  }
                  if (strcmp(Tcl_GetString(obj),"NEGLON")==0) {
                     gref->Type|=GRID_NEGLON;
                  }
               }
            }
            break;

         case PROJECTION:
            if (Objc==1) {
               if (gref->String)
                  Tcl_SetObjResult(Interp,Tcl_NewStringObj(gref->String,-1));
            } else {
               GeoRef_WKTSet(gref,Tcl_GetString(Objv[++i]),gref->Transform,gref->InvTransform,NULL);
               gref->Grid[0]='W';
               gref->Grid[1]=gref->Grid[2]='\0';
            }
            break;

         case TRANSFORM:
            if (Objc==1) {
               if (gref->Transform) {
                  obj=Tcl_NewListObj(0,NULL);
                  for(j=0;j<6;j++) {
                     Tcl_ListObjAppendElement(Interp,obj,Tcl_NewDoubleObj(gref->Transform[j]));
                  }
                  Tcl_SetObjResult(Interp,obj);
               }
            } else {
               if (Tcl_ListObjLength(Interp,Objv[++i],&nidx)==TCL_ERROR) {
                  return(TCL_ERROR);
               }

               if (nidx==0) {
                  GeoRef_WKTSet(gref,gref->String,NULL,NULL,NULL);
               } else {
                  if (nidx!=6) {
                     Tcl_AppendResult(Interp,"\n   GeoRef_Define: Invalid number of transform element, must be 6 \"",(char*)NULL);
                     return(TCL_ERROR);
                  }
                  for(j=0;j<6;j++) {
                     Tcl_ListObjIndex(Interp,Objv[i],j,&obj);
                     Tcl_GetDoubleFromObj(Interp,obj,&tra[j]);
                  }
                  tm=tra;
                  if (!GDALInvGeoTransform(tra,inv)) {
                     App_Log(WARNING,"%s: Unable to generate the inverse transform matrix\n",__func__);
                     im=NULL;
                  } else {
                     im=inv;
                  }
                  GeoRef_WKTSet(gref,gref->String,tm,im,NULL);
               }
            }
            break;

         case INVTRANSFORM:
            if (Objc==1) {
               if (gref->InvTransform) {
                  obj=Tcl_NewListObj(0,NULL);
                  for(j=0;j<6;j++) {
                     Tcl_ListObjAppendElement(Interp,obj,Tcl_NewDoubleObj(gref->InvTransform[j]));
                  }
                  Tcl_SetObjResult(Interp,obj);
               }
            } else {
               if (Tcl_ListObjLength(Interp,Objv[++i],&nidx)==TCL_ERROR) {
                  return(TCL_ERROR);
               }

               if (nidx==0) {
                  GeoRef_WKTSet(gref,gref->String,NULL,NULL,NULL);
               } else {
                  if (nidx!=6) {
                     Tcl_AppendResult(Interp,"\n   GeoRef_Define: Invalid number of transform element, must be 6 \"",(char*)NULL);
                     return(TCL_ERROR);
                  }
                  for(j=0;j<6;j++) {
                     Tcl_ListObjIndex(Interp,Objv[i],j,&obj);
                     Tcl_GetDoubleFromObj(Interp,obj,&inv[j]);
                  }
                  im=inv;
                  if (!GDALInvGeoTransform(inv,tra)) {
                     App_Log(WARNING,"%s: Unable to generate the transform matrix\n",__func__);
                     tm=NULL;
                  } else {
                     tm=tra;
                  }
                  GeoRef_WKTSet(gref,gref->String,tm,im,NULL);
               }
            }
            break;

        case LOCATION:
            if (Objc==1) {
               obj=Tcl_NewListObj(0,NULL);
               Tcl_ListObjAppendElement(Interp,obj,Tcl_NewDoubleObj(gref->Loc.Lat));
               Tcl_ListObjAppendElement(Interp,obj,Tcl_NewDoubleObj(gref->Loc.Lon));
               Tcl_ListObjAppendElement(Interp,obj,Tcl_NewDoubleObj(gref->Loc.Elev));
               Tcl_SetObjResult(Interp,obj);
            } else {
               if (Objc>4) {
                  Tcl_AppendResult(Interp,"\n   GeoRef_Define: Invalid location, must be 4 \"",(char*)NULL);
                  return(TCL_ERROR);
               }
               Tcl_GetDoubleFromObj(Interp,Objv[++i],&gref->Loc.Lat);
               if (Objc>2)
                  Tcl_GetDoubleFromObj(Interp,Objv[++i],&gref->Loc.Lon);
               if (Objc>3)
                  Tcl_GetDoubleFromObj(Interp,Objv[++i],&gref->Loc.Elev);
            }
            break;

        case EXTENT:
            if (Objc==1) {
               obj=Tcl_NewListObj(0,NULL);
               Tcl_ListObjAppendElement(Interp,obj,Tcl_NewIntObj(gref->X0));
               Tcl_ListObjAppendElement(Interp,obj,Tcl_NewIntObj(gref->Y0));
               Tcl_ListObjAppendElement(Interp,obj,Tcl_NewIntObj(gref->X1));
               Tcl_ListObjAppendElement(Interp,obj,Tcl_NewIntObj(gref->Y1));
               Tcl_SetObjResult(Interp,obj);
            } else {
               if (Tcl_ListObjLength(Interp,Objv[++i],&nidx)==TCL_ERROR) {
                  return(TCL_ERROR);
               }
               if (nidx!=4) {
                  Tcl_AppendResult(Interp,"\n   GeoRef_Define: Invalid number of values, must be 4 \"",(char*)NULL);
                  return(TCL_ERROR);
               }
               Tcl_ListObjIndex(Interp,Objv[i],0,&obj);
               Tcl_GetDoubleFromObj(Interp,obj,&tra[0]);
               Tcl_ListObjIndex(Interp,Objv[i],1,&obj);
               Tcl_GetDoubleFromObj(Interp,obj,&tra[1]);
               Tcl_ListObjIndex(Interp,Objv[i],2,&obj);
               Tcl_GetDoubleFromObj(Interp,obj,&tra[2]);
               Tcl_ListObjIndex(Interp,Objv[i],3,&obj);
               Tcl_GetDoubleFromObj(Interp,obj,&tra[3]);
               GeoRef_Size(gref,tra[0],tra[1],tra[2],tra[3],gref->BD);
            }
            break;

        case SIZE:
            if (Objc==1) {
               obj=Tcl_NewListObj(0,NULL);
               Tcl_ListObjAppendElement(Interp,obj,Tcl_NewIntObj(gref->NX));
               Tcl_ListObjAppendElement(Interp,obj,Tcl_NewIntObj(gref->NY));
               Tcl_SetObjResult(Interp,obj);
            } else {
               if (Tcl_ListObjLength(Interp,Objv[++i],&nidx)==TCL_ERROR) {
                  return(TCL_ERROR);
               }
               if (nidx!=2) {
                  Tcl_AppendResult(Interp,"\n   GeoRef_Define: Invalid number of values, must be 4 \"",(char*)NULL);
                  return(TCL_ERROR);
               }
               Tcl_ListObjIndex(Interp,Objv[i],0,&obj);
               Tcl_GetIntFromObj(Interp,obj,&ni);
               Tcl_ListObjIndex(Interp,Objv[i],1,&obj);
               Tcl_GetIntFromObj(Interp,obj,&nj);
               GeoRef_Size(gref,0,0,ni-1,nj-1,gref->BD);
            }
            break;

        case BORDER:
            if (Objc==1) {
               Tcl_SetObjResult(Interp,Tcl_NewIntObj(gref->BD));
            } else {
               Tcl_GetIntFromObj(Interp,Objv[++i],&gref->BD);
            }
            break;
      }
   }
   return(TCL_OK);
}

/*--------------------------------------------------------------------------------------------------------------
 * Nom          : <GeoRef_Put>
 * Creation     : Juillet 2005 - J.P. Gauthier - CMC/CMOE
 *
 * But          : Creer et insere dans la table la georeference
 *
 * Parametres   :
 *   <Name>     : Nom de l'objet bande a obtenir.
 *
 * Retour       : Une structure TGeoRef ou un pointeur NULL si rien trouve.
 *
 * Remarques    :
 *
 *---------------------------------------------------------------------------------------------------------------
*/
Tcl_Obj* GeoRef_Put(Tcl_Interp *Interp,char *Name,TGeoRef *GRef) {

   char buf[64];

   if (GRef) {
      if (!Name) {
         // Check for non-existing name
         sprintf(buf,"GEOREF_____%li",GeoRef_TableNo++);
         while (TclY_HashGet(&GeoRef_Table,buf)) {
            sprintf(buf,"GEOREF_____%li",GeoRef_TableNo++);
         }
         Name=buf;
      }
      if (TclY_HashSet(Interp,&GeoRef_Table,Name,GRef)==TCL_ERROR) {
         return(NULL);
      }

      GRef->Name=strdup(Name);
      return(Tcl_NewStringObj(Name,-1));
   } else {
      if (Interp)
         Tcl_AppendResult(Interp,"\n   GeoRef_Put: Georef invalid: \"",Name, "\"",(char *)NULL);
      return(NULL);
   }
}

/*--------------------------------------------------------------------------------------------------------------
 * Nom          : <GeoRef_Get>
 * Creation     : Juillet 2005 - J.P. Gauthier - CMC/CMOE
 *
 * But          : Obtenir une georef en fonction de son nom dans la table de Tcl
 *
 * Parametres   :
 *   <Name>     : Nom de l'objet bande a obtenir.
 *
 * Retour       : Une structure TGeoRef ou un pointeur NULL si rien trouve.
 *
 * Remarques    :
 *
 *---------------------------------------------------------------------------------------------------------------
*/
TGeoRef* GeoRef_Get(char *Name) {
   return((TGeoRef*)TclY_HashGet(&GeoRef_Table,Name));
}

/*--------------------------------------------------------------------------------------------------------------
 * Nom          : <GeoRef_Destroy>
 * Creation     : Juillet 2005 J.P. Gauthier
 *
 * But          : Destruction d'un georef a partir de son nom.
 *
 * Parametres   :
 *   <Interp>   : Interpreteur Tcl
 *   <Name>     : Nom du layer
 *
 * Retour       : Code de retour standard TCL
 *
 * Remarques :
 *
 *---------------------------------------------------------------------------------------------------------------
*/
int GeoRef_Destroy(Tcl_Interp *Interp,char *Name) {

   Tcl_HashEntry *entry;
   TGeoRef       *gref;

   if (Name) {
      entry=TclY_FindHashEntry(&GeoRef_Table,Name);

      if (entry) {
         gref=(TGeoRef*)Tcl_GetHashValue(entry);
         if (GeoRef_Free(gref))
            TclY_DeleteHashEntry(entry);
      }
   }
   return(TCL_OK);
}

TGeoRef* GeoRef_Find(TGeoRef *GRef) {

   Tcl_HashSearch  ptr;
   Tcl_HashEntry  *entry=NULL;
   TGeoRef        *gref;

   if (!GRef) {
      GRef=GeoRef_New();
   } else {

      TclY_LockHash();

      // Look for an already existing object that could match
      entry=Tcl_FirstHashEntry(&GeoRef_Table,&ptr);
      while (entry) {
         gref=(TGeoRef*)Tcl_GetHashValue(entry);

         if (GeoRef_Equal(gref,GRef)) {
            GeoRef_Free(GRef);
            GeoRef_Incr(gref);
            TclY_UnlockHash();
            return(gref);
         }
         entry=Tcl_NextHashEntry(&ptr);
      }
      TclY_UnlockHash();
   }

   // Otherwise, create a new one
   gref=GRef;

   GeoRef_Put(NULL,NULL,gref);

   return(gref);
}

#define GeoPos_Incr(GPOS) __sync_add_and_fetch(&GPOS->NRef,1)
#define GeoPos_Decr(GPOS) __sync_sub_and_fetch(&GPOS->NRef,1)

TGeoPos* GeoPos_Copy(TGeoPos *GPos) {

   if (GPos) GeoPos_Incr(GPos);
      
   return(GPos);
}

int GeoPos_Free(TGeoPos *GPos) {

   int n=0;
   
   if (GPos && !GeoPos_Decr(GPos)) {
      // Free reference components
      ZRef_Free(GPos->ZRef);
      if (GPos->GRef->Name) {
          GeoRef_Destroy(NULL,GPos->GRef->Name);
      } else {
          GeoRef_Free(GPos->GRef);
      }
      
      // Free grid positions
      while(GPos->Pos[n]!=(Vect3d*)SYS_PTR_END) {
         if (GPos->Pos[n]) free(GPos->Pos[n]);
         n++;
      }
      
      // Remove from table
      GeoPos_Table=TList_Del(GeoPos_Table,(void*)GPos);
     
      free(GPos->Pos);
      GPos->Pos=NULL;
   }
   return(1);
}

TGeoPos* GeoPos_Find(TGeoRef *GRef,TZRef *ZRef) {

   TList   *node;
   TGeoPos *gpos;

   if (!GRef || !ZRef) {
      return(NULL);
   }

   // Look for an already existing object that could match
   node=GeoPos_Table;
   while(node) {
      gpos=(TGeoPos*)node->Data;
      if (GeoRef_Equal(gpos->GRef,GRef) && ZRef_Equal(gpos->ZRef,ZRef)) {
         GeoPos_Incr(gpos);
         return(gpos);        
      }
      node=node->Next;
   }
 
   // Otherwise, create a new one
   gpos=(TGeoPos*)malloc(sizeof(TGeoPos));
   gpos->NRef=1;
   gpos->GRef=GRef;
   gpos->ZRef=ZRef;

   // Increment internal references
   GeoRef_Incr(GRef);
   ZRef_Incr(ZRef);
   
   if (!(gpos->Pos=(Vect3d**)calloc(ZRef->LevelNb+1,sizeof(Vect3d*)))) {
      return(NULL);
   }
   gpos->Pos[ZRef->LevelNb]=(Vect3d*)SYS_PTR_END; 
   
   // Add to table
   GeoPos_Table=TList_Add(GeoPos_Table,(void*)gpos);

   return(gpos);
}
