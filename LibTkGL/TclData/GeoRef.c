/*=========================================================
 * Environnement Canada
 * Centre Meteorologique Canadien
 * 2100 Trans-Canadienne
 * Dorval, Quebec
 *
 * Projet       : Lecture et traitements de fichiers raster
 * Fichier      : GeoRef.c
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

#include "GeoRef.h"

TCL_DECLARE_MUTEX(MUTEX_GEOREF)

static long GeoRefNo=0;
static int  GeoRefInit=0;
static Tcl_HashTable GeoRef_Table;
static int GeoRef_Cmd(ClientData clientData,Tcl_Interp *Interp,int Objc,Tcl_Obj *CONST Objv[]);

static int TGeoRef_TableNo;

TGeoScan GScan;

int GeoRef_Init(Tcl_Interp *Interp) {

   Tcl_InitHashTable(&GeoRef_Table,TCL_STRING_KEYS);
   Tcl_CreateObjCommand(Interp,"georef",GeoRef_Cmd,(ClientData)NULL,(Tcl_CmdDeleteProc*)NULL);

   if (!GeoRefInit++) {
      TGeoRef_TableNo=1;
   }

   return(TCL_OK);
}

/*--------------------------------------------------------------------------------------------------------------
 * Nom          : <GeoScan_Init>
 * Creation     : Fevrier 2008 J.P. Gauthier - CMC/CMOE
 *
 * But          : Initialiser le buffer de reprojection
 *
 * Parametres   :
 *  <Scan>      : Buffer de reprojection
 *  <To>        : Georeference destination
 *  <From>      : Georeference source
 *  <X0>        : Limite inferieure en X
 *  <Y0>        : Limite inferieure en Y
 *  <X1>        : Limite superieure en X
 *  <Y1>        : Limite superieure en Y
 *
 * Retour       :
 *
 * Remarques    :
 *
 *---------------------------------------------------------------------------------------------------------------
*/
void* GeoScan_Init(TGeoScan *Scan,TGeoRef *To,TGeoRef *From,int X0,int Y0,int X1,int Y1) {

   int nd,nv;

   /*Check for geoscan validity, so we can use the same as previous*/
//   if (Scan->ToRef==To && Scan->FromRef==From && From->Grid[0]!='Y' && From->Grid[0]!='#' && Scan->X0==X0 && Scan->Y0==Y0 && Scan->X1==X1 && Scan->Y1==Y1) {
   if (GeoRef_Equal(Scan->ToRef,To) && GeoRef_Equal(Scan->FromRef,From) && Scan->X0==X0 && Scan->Y0==Y0 && Scan->X1==X1 && Scan->Y1==Y1) {
      Scan->Valid=1;
   } else {
      Scan->X0=X0;
      Scan->Y0=Y0;
      Scan->X1=X1;
      Scan->Y1=Y1;

      if (Scan->ToRef) {
         Scan->ToRef->Lat=NULL;
         Scan->ToRef->Lon=NULL;
         GeoRef_Free(Scan->ToRef);
      }
      Scan->ToRef=GeoRef_HardCopy(To);
      Scan->ToRef->Lat=To->Lat;
      Scan->ToRef->Lon=To->Lon;

      if (Scan->FromRef) {
         Scan->FromRef->Lat=NULL;
         Scan->FromRef->Lon=NULL;
         GeoRef_Free(Scan->FromRef);
      }
      Scan->FromRef=GeoRef_HardCopy(From);
      Scan->FromRef->Lat=From->Lat;
      Scan->FromRef->Lon=From->Lon;

      /*Adjust scan buffer sizes*/
      Scan->DX=X1-X0+1;
      Scan->DY=Y1-Y0+1;
      nd=(Scan->DX+1)*(Scan->DY+1);
      nv=Scan->DX*Scan->DY;
      if (Scan->S<nv) {
         if (!(Scan->X=(void*)realloc(Scan->X,nd*sizeof(double))))
            return(NULL);
         if (!(Scan->Y=(void*)realloc(Scan->Y,nd*sizeof(double))))
            return(NULL);
         if (!(Scan->V=(unsigned int*)realloc(Scan->V,nv*sizeof(unsigned int))))
            return(NULL);
         Scan->S=nv;
      }
      Scan->N=0;
      Scan->Valid=0;
   }
   return(Scan->V);
}

/*--------------------------------------------------------------------------------------------------------------
 * Nom          : <GeoScan_Clear>
 * Creation     : Fevrier 2008 J.P. Gauthier - CMC/CMOE
 *
 * But          : Reinitialiser le buffer de reprojection
 *
 * Parametres   :
 *  <Scan>      : Buffer de reprojection
 *
 * Retour       :
 *
 * Remarques    :
 *
 *---------------------------------------------------------------------------------------------------------------
*/
void GeoScan_Clear(TGeoScan *Scan) {

   if (Scan->X) free(Scan->X);
   if (Scan->Y) free(Scan->Y);
   if (Scan->V) free(Scan->V);

   Scan->X=Scan->Y=NULL;
   Scan->V=NULL;
}

/*--------------------------------------------------------------------------------------------------------------
 * Nom          : <GeoFunc_RadialPointRatio>
 * Creation     : Fevrier 2008 J.P. Gauthier - CMC/CMOE
 *
 * But          : Interpoler la position d'un point sur un grand cercle
 *
 * Parametres   :
 *  <C1>        : Coordonne du premier point
 *  <C2>        : Coordonne du deuxieme point
 *  <C3>        : Coordonne du point a localier sur le grand cercle
 *
 * Retour       : Ratio de distance
 *
 * Remarques    :
 *
 *---------------------------------------------------------------------------------------------------------------
*/
double GeoFunc_RadialPointRatio(Coord C1,Coord C2,Coord C3) {

   Coord cr;

   GeoFunc_RadialPointOn(C1,C2,C3,&cr);

   return(DIST(0,C1.lat,C1.lon,cr.lat,cr.lon)/DIST(0,C1.lat,C1.lon,C2.lat,C2.lon));
}

/*--------------------------------------------------------------------------------------------------------------
 * Nom          : <GeoFunc_RadialPointOn>
 * Creation     : Fevrier 2008 J.P. Gauthier - CMC/CMOE
 *
 * But          : Calculer le point d'intersection en tracant un angle droit d'un point sur un grand cercle
 *
 * Parametres   :
 *  <C1>        : Coordonne du premier point
 *  <C2>        : Coordonne du deuxieme point
 *  <C3>        : Coordonne du point a localier sur le grand cercle
 *  <CR>        : Coordonne du point localise
 *
 * Retour       : Intersection existe
 *
 * Remarques    :
 *
 *---------------------------------------------------------------------------------------------------------------
*/
int GeoFunc_RadialPointOn(Coord C1,Coord C2,Coord C3,Coord *CR) {

   double crs12,crs13,crs3x;

   /*Calculates 90 degree course crossing*/
   crs12=COURSE(C1.lat,C1.lon,C2.lat,C2.lon);
   crs13=COURSE(C1.lat,C1.lon,C3.lat,C3.lon);
   crs3x=crs13>crs12?crs12-M_PI2:crs12+M_PI2;

   return(GeoFunc_RadialIntersect(C1,C3,crs12,crs3x,CR));
}

/*--------------------------------------------------------------------------------------------------------------
 * Nom          : <GeoFunc_RadialIntersect>
 * Creation     : Fevrier 2008 J.P. Gauthier - CMC/CMOE
 *
 * But          : Calculer le point d'intersection de deux grand cercle
 *
 * Parametres   :
 *  <C1>        : Coordonne du premier point
 *  <C2>        : Coordonne du deuxieme point
 *  <CRS13>     : Direction entre le premier et le troisieme point
 *  <CRS23>     : Direction entre le deuxieme et troisieme point
 *  <C3>        : Point d'intersection
 *
 * Retour       : Intersection existe
 *
 * Remarques    :
 *
 *---------------------------------------------------------------------------------------------------------------
*/
int GeoFunc_RadialIntersect(Coord C1,Coord C2,double CRS13,double CRS23,Coord *C3) {

   double dst13,dst12,crs12,crs21,ang1,ang2,ang3;

   Coord sinc2,cosc2,sinc1,cosc1;

   sinc1.lat=sin(C1.lat);sinc1.lon=sin(C1.lon);
   cosc1.lat=cos(C1.lat);cosc1.lon=cos(C1.lon);
   sinc2.lat=sin(C2.lat);sinc2.lon=sin(C2.lon);
   cosc2.lat=cos(C2.lat);cosc2.lon=cos(C2.lon);

   dst12=2*asin(sqrt(pow((sin((C1.lat-C2.lat)/2)),2) + cosc1.lat*cosc2.lat*pow(sin((C1.lon-C2.lon)/2),2)));

   if (sin(C2.lon-C1.lon)<0) {
      crs12=acos((sinc2.lat-sinc1.lat*cos(dst12))/(sin(dst12)*cosc1.lat));
   } else {
      crs12=2.0*M_PI-acos((sinc2.lat-sinc1.lat*cos(dst12))/(sin(dst12)*cosc1.lat));
   }

   if (sin(C1.lon-C2.lon)<0) {
      crs21=acos((sinc1.lat-sinc2.lat*cos(dst12))/(sin(dst12)*cosc2.lat));
   } else {
      crs21=M_2PI-acos((sinc1.lat-sinc2.lat*cos(dst12))/(sin(dst12)*cosc2.lat));
   }

   ang1=fmod(CRS13-crs12+M_PI,M_2PI)-M_PI;
   ang2=fmod(crs21-CRS23+M_PI,M_2PI)-M_PI;

   if (sin(ang1)*sin(ang2)<=sqrt(10e-15)) {
      /*No intersection*/
      return(0);
   } else {
      ang1=fabs(ang1);
      ang2=fabs(ang2);
      ang3=acos(-cos(ang1)*cos(ang2)+sin(ang1)*sin(ang2)*cos(dst12));
      dst13=asin(sin(ang2)*sin(dst12)/sin(ang3));
      C3->lat=asin(sinc1.lat*cos(dst13)+cosc1.lat*sin(dst13)*cos(CRS13));
      C3->lon=fmod(C1.lon-asin(sin(CRS13)*sin(dst13)/cos(C3->lat))+M_PI,M_2PI)-M_PI;
   }

   return(1);
}

/*--------------------------------------------------------------------------------------------------------------
 * Nom          : <TeoRef_Cmd>
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

   double      x,y,lat0,lon0,lat1,lon1;
   int         idx,in=0,x0,y0,x1,y1,n;
   TGeoRef    *ref0,*ref1;
   Tcl_Obj    *lst;
   char       *buf;

   static CONST char *sopt[] = { "create","copy","free","define","project","unproject","limit","within","intersect","is","all","wipe",NULL };
   enum                opt { CREATE,COPY,FREE,DEFINE,PROJECT,UNPROJECT,LIMIT,WITHIN,INTERSECT,IS,ALL,WIPE };

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
         if (Objc!=3 && Objc!=4) {
            Tcl_WrongNumArgs(Interp,2,Objv,"georef [wkt string]");
            return(TCL_ERROR);
         }
         if (Objc==4) {
            buf=Tcl_GetString(Objv[3]);
         }

         ref0=GeoRef_New();
         ref0->Id=-(++TGeoRef_TableNo);

         if (!GeoRef_Put(Interp,Tcl_GetString(Objv[2]),ref0)) {
            Tcl_AppendResult(Interp,"\n   GeoRef_Cmd: Unable to create georef",(char*)NULL);
            return(TCL_ERROR);
         }
         break;

      case COPY:
         if (Objc!=4) {
            Tcl_WrongNumArgs(Interp,2,Objv,"georefto georeffrom");
            return(TCL_ERROR);
         }
         ref0=GeoRef_Get(Tcl_GetString(Objv[3]));
         if (!ref0) {
            Tcl_AppendResult(Interp,"\n   GeoRef_Cmd invalid georeference object",(char*)NULL);
            return(TCL_ERROR);
         }

         ref0=GeoRef_HardCopy(ref0);
         GeoRef_Incr(ref0);

         if (!GeoRef_Put(Interp,Tcl_GetString(Objv[2]),ref0)) {
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

      case PROJECT:
         if (Objc!=5) {
            Tcl_WrongNumArgs(Interp,2,Objv,"georef X Y");
            return(TCL_ERROR);
         }

         Tcl_GetDoubleFromObj(Interp,Objv[3],&x);
         Tcl_GetDoubleFromObj(Interp,Objv[4],&y);

         ref0=GeoRef_Get(Tcl_GetString(Objv[2]));
         if (!ref0 || !ref0->Project) {
            Tcl_AppendResult(Interp,"\n   GeoRef_Cmd invalid georeference object",(char*)NULL);
            return(TCL_ERROR);
         } else {
            lst=Tcl_NewListObj(0,NULL);
            ref0->Project(ref0,x,y,&lat0,&lon0,1,1);
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

         ref0=GeoRef_Get(Tcl_GetString(Objv[2]));
         if (!ref0 || !ref0->UnProject) {
            Tcl_AppendResult(Interp,"\n   GeoRef_Cmd invalid georeference object",(char*)NULL);
            return(TCL_ERROR);
         } else {
            lst=Tcl_NewListObj(0,NULL);
            ref0->UnProject(ref0,&x,&y,lat0,lon0,1,1);
            Tcl_ListObjAppendElement(Interp,lst,Tcl_NewDoubleObj(x));
            Tcl_ListObjAppendElement(Interp,lst,Tcl_NewDoubleObj(y));
            Tcl_SetObjResult(Interp,lst);
            return(TCL_OK);
         }
         break;

      case WITHIN:
         if (Objc!=4 && Objc!=7 && Objc!=8) {
            Tcl_WrongNumArgs(Interp,2,Objv,"georefto lat0 lon0 lat1 lon1 [included] | georefto georeffrom");
            return(TCL_ERROR);
         } else {
            ref0=GeoRef_Get(Tcl_GetString(Objv[2]));
            if (!ref0) {
               Tcl_AppendResult(Interp,"\n   GeoRef_Cmd invalid georeference object",(char*)NULL);
               return(TCL_ERROR);
            } else {
               if (Objc==4) {
                  ref1=GeoRef_Get(Tcl_GetString(Objv[3]));
                  if (!ref1) {
                     Tcl_AppendResult(Interp,"\n   GeoRef_Cmd invalid georeference object",(char*)NULL);
                     return(TCL_ERROR);
                  }
                  Tcl_SetObjResult(Interp,Tcl_NewBooleanObj(GeoRef_Within(ref0,ref1)));
                  return(TCL_OK);
               }
               Tcl_GetDoubleFromObj(Interp,Objv[3],&lat0);
               Tcl_GetDoubleFromObj(Interp,Objv[4],&lon0);
               Tcl_GetDoubleFromObj(Interp,Objv[5],&lat1);
               Tcl_GetDoubleFromObj(Interp,Objv[6],&lon1);
               if (Objc==8)
                 Tcl_GetBooleanFromObj(Interp,Objv[7],&in);

               Tcl_SetObjResult(Interp,Tcl_NewBooleanObj(GeoRef_WithinRange(ref0,lat0,lon0,lat1,lon1,in)));
               return(TCL_OK);
            }
         }
         break;

      case LIMIT:
         if (Objc!=3) {
            Tcl_WrongNumArgs(Interp,2,Objv,"georef");
            return(TCL_ERROR);
         } else {
            ref0=GeoRef_Get(Tcl_GetString(Objv[2]));
            if (!ref0) {
               Tcl_AppendResult(Interp,"\n   GeoRef_Cmd invalid georeference object",(char*)NULL);
               return(TCL_ERROR);
            } else {
               GeoRef_Limits(ref0,&lat0,&lon0,&lat1,&lon1);
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

      case INTERSECT:
         if (Objc!=4) {
            Tcl_WrongNumArgs(Interp,2,Objv,"georef0 georef1");
            return TCL_ERROR;
         } else {
            ref0=GeoRef_Get(Tcl_GetString(Objv[2]));
            ref1=GeoRef_Get(Tcl_GetString(Objv[3]));
            if (!ref0 || !ref1) {
               Tcl_AppendResult(Interp,"\n   GeoRef_Cmd invalid georeference object",(char*)NULL);
               return TCL_ERROR;
            } else {
               lst=Tcl_NewListObj(0,NULL);
               if (GeoRef_Intersect(ref0,ref1,&x0,&y0,&x1,&y1,0)) {
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
            return TCL_ERROR;
         }
         if (GeoRef_Get(Tcl_GetString(Objv[2]))) {
            Tcl_SetObjResult(Interp,Tcl_NewBooleanObj(1));
         } else {
            Tcl_SetObjResult(Interp,Tcl_NewBooleanObj(0));
         }
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
int GeoRef_Define(Tcl_Interp *Interp,char *Name,int Objc,Tcl_Obj *CONST Objv[]){

   int          i,j,idx,nidx;
   double       tra[6],inv[6],*tm,*im;
   TGeoRef     *ref;
   Tcl_Obj     *obj;

   static CONST char *sopt[] = { "-projection","-transform","-invtransform","-extent","-location","-type","-border",NULL };
   enum        opt {  PROJECTION,TRANSFORM,INVTRANSFORM,EXTENT,LOCATION,TYPE,BORDER };

   ref=GeoRef_Get(Name);
   if (!ref) {
      Tcl_AppendResult(Interp,"\n   GeoRef_Define: Georef name unknown: \"",Name,"\"",(char *)NULL);
      return TCL_ERROR;
   }

   for(i=0;i<Objc;i++) {

      if (Tcl_GetIndexFromObj(Interp,Objv[i],sopt,"option",0,&idx)!=TCL_OK) {
         return TCL_ERROR;
      }

      switch ((enum opt)idx) {

         case TYPE:
            if (Objc==1) {
               obj=Tcl_NewListObj(0,NULL);
               if (ref->Type&=GRID_REGULAR) {
                  Tcl_ListObjAppendElement(Interp,obj,Tcl_NewStringObj("REGULAR",-1));
               }
               if (ref->Type&=GRID_VARIABLE) {
                  Tcl_ListObjAppendElement(Interp,obj,Tcl_NewStringObj("VARIABLE",-1));
               }
               if (ref->Type&=GRID_WRAP) {
                  Tcl_ListObjAppendElement(Interp,obj,Tcl_NewStringObj("WRAP",-1));
               }
               if (ref->Type&=GRID_SPARSE) {
                  Tcl_ListObjAppendElement(Interp,obj,Tcl_NewStringObj("SPARSE",-1));
               }
               if (ref->Type&=GRID_TILE) {
                  Tcl_ListObjAppendElement(Interp,obj,Tcl_NewStringObj("TILE",-1));
               }
               if (ref->Type&=GRID_VERTICAL) {
                  Tcl_ListObjAppendElement(Interp,obj,Tcl_NewStringObj("VERTICAL",-1));
               }
               if (ref->Type&=GRID_RADIAL) {
                  Tcl_ListObjAppendElement(Interp,obj,Tcl_NewStringObj("RADIAL",-1));
               }
               Tcl_SetObjResult(Interp,obj);
            } else {
               if (Tcl_ListObjLength(Interp,Objv[++i],&nidx)==TCL_ERROR) {
                  return(TCL_ERROR);
               }

               ref->Type=GRID_NONE;
               for(j=0;j<6;j++) {
                  Tcl_ListObjIndex(Interp,Objv[i],j,&obj);
                  if (strcmp(Tcl_GetString(obj),"REGULAR")==0) {
                     ref->Type|=GRID_REGULAR;
                  }
                  if (strcmp(Tcl_GetString(obj),"VARIABLE")==0) {
                     ref->Type|=GRID_VARIABLE;
                  }
                  if (strcmp(Tcl_GetString(obj),"WRAP")==0) {
                     ref->Type|=GRID_WRAP;
                  }
                  if (strcmp(Tcl_GetString(obj),"SPARSE")==0) {
                     ref->Type|=GRID_SPARSE;
                  }
                  if (strcmp(Tcl_GetString(obj),"TILE")==0) {
                     ref->Type|=GRID_TILE;
                  }
                  if (strcmp(Tcl_GetString(obj),"VERTICAL")==0) {
                     ref->Type|=GRID_VERTICAL;
                  }
                  if (strcmp(Tcl_GetString(obj),"RADIAL")==0) {
                     ref->Type|=GRID_RADIAL;
                  }
               }
            }
            break;

         case PROJECTION:
            if (Objc==1) {
               if (ref->String)
                  Tcl_SetObjResult(Interp,Tcl_NewStringObj(ref->String,-1));
            } else {
               GeoRef_WKTSet(ref,Tcl_GetString(Objv[++i]),ref->Transform,ref->InvTransform,NULL);
            }
            break;

         case TRANSFORM:
            if (Objc==1) {
               if (ref->Transform) {
                  obj=Tcl_NewListObj(0,NULL);
                  for(j=0;j<6;j++) {
                     Tcl_ListObjAppendElement(Interp,obj,Tcl_NewDoubleObj(ref->Transform[j]));
                  }
                  Tcl_SetObjResult(Interp,obj);
               }
            } else {
               if (Tcl_ListObjLength(Interp,Objv[++i],&nidx)==TCL_ERROR) {
                  return TCL_ERROR;
               }

               if (nidx==0) {
                  GeoRef_WKTSet(ref,ref->String,NULL,NULL,NULL);
               } else {
                  if (nidx!=6) {
                     Tcl_AppendResult(Interp,"\n   GeoRef_Define: Invalid number of transform element, must be 6 \"",(char*)NULL);
                     return TCL_ERROR;
                  }
                  for(j=0;j<6;j++) {
                     Tcl_ListObjIndex(Interp,Objv[i],j,&obj);
                     Tcl_GetDoubleFromObj(Interp,obj,&tra[j]);
                  }
                  tm=tra;
                  if (!GDALInvGeoTransform(tra,inv)) {
                     fprintf(stderr,"(WARNING) GeoRef_Define: Unable to generate the inverse transform matrix\n");
                     im=NULL;
                  } else {
                     im=inv;
                  }
                  GeoRef_WKTSet(ref,ref->String,tm,im,NULL);
               }
            }
            break;

         case INVTRANSFORM:
            if (Objc==1) {
               if (ref->InvTransform) {
                  obj=Tcl_NewListObj(0,NULL);
                  for(j=0;j<6;j++) {
                     Tcl_ListObjAppendElement(Interp,obj,Tcl_NewDoubleObj(ref->InvTransform[j]));
                  }
                  Tcl_SetObjResult(Interp,obj);
               }
            } else {
               if (Tcl_ListObjLength(Interp,Objv[++i],&nidx)==TCL_ERROR) {
                  return TCL_ERROR;
               }

               if (nidx==0) {
                  GeoRef_WKTSet(ref,ref->String,NULL,NULL,NULL);
               } else {
                  if (nidx!=6) {
                     Tcl_AppendResult(Interp,"\n   GeoRef_Define: Invalid number of transform element, must be 6 \"",(char*)NULL);
                     return TCL_ERROR;
                  }
                  for(j=0;j<6;j++) {
                     Tcl_ListObjIndex(Interp,Objv[i],j,&obj);
                     Tcl_GetDoubleFromObj(Interp,obj,&inv[j]);
                  }
                  im=inv;
                  if (!GDALInvGeoTransform(inv,tra)) {
                     fprintf(stderr,"(WARNING) GeoRef_Define: Unable to generate the transform matrix\n");
                     tm=NULL;
                  } else {
                     tm=tra;
                  }
                  GeoRef_WKTSet(ref,ref->String,tm,im,NULL);
               }
            }
            break;

        case LOCATION:
            if (Objc==1) {
               obj=Tcl_NewListObj(0,NULL);
               Tcl_ListObjAppendElement(Interp,obj,Tcl_NewDoubleObj(ref->Loc.lat));
               Tcl_ListObjAppendElement(Interp,obj,Tcl_NewDoubleObj(ref->Loc.lon));
               Tcl_ListObjAppendElement(Interp,obj,Tcl_NewDoubleObj(ref->Loc.elev));
               Tcl_SetObjResult(Interp,obj);
            } else {
               if (Objc>4) {
                  Tcl_AppendResult(Interp,"\n   GeoRef_Define: Invalid location, must be 4 \"",(char*)NULL);
                  return(TCL_ERROR);
               }
               Tcl_GetDoubleFromObj(Interp,Objv[++i],&ref->Loc.lat);
               if (Objc>2)
                  Tcl_GetDoubleFromObj(Interp,Objv[++i],&ref->Loc.lon);
               if (Objc>3)
                  Tcl_GetDoubleFromObj(Interp,Objv[++i],&ref->Loc.elev);
            }
            break;

        case EXTENT:
            if (Objc==1) {
               obj=Tcl_NewListObj(0,NULL);
               Tcl_ListObjAppendElement(Interp,obj,Tcl_NewIntObj(ref->X0));
               Tcl_ListObjAppendElement(Interp,obj,Tcl_NewIntObj(ref->Y0));
               Tcl_ListObjAppendElement(Interp,obj,Tcl_NewIntObj(ref->X1));
               Tcl_ListObjAppendElement(Interp,obj,Tcl_NewIntObj(ref->Y1));
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
               GeoRef_Size(ref,tra[0],tra[1],0,tra[2],tra[3],0,ref->BD);
            }
            break;

        case BORDER:
            if (Objc==1) {
               Tcl_SetObjResult(Interp,Tcl_NewIntObj(ref->BD));
            } else {
               Tcl_GetIntFromObj(Interp,Objv[++i],&ref->BD);
            }
            break;
      }
   }
   return TCL_OK;
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
Tcl_Obj* GeoRef_Put(Tcl_Interp *Interp,char *Name,TGeoRef *Ref) {

   char buf[64];

   if (Ref) {
      if (!Name) {
         sprintf(buf,"GEOREF_____%li",GeoRefNo++);
         Name=buf;
      }

      if (TclY_HashSet(Interp,&GeoRef_Table,Name,Ref)==TCL_ERROR) {
         return(NULL);
      }

      Ref->Name=strdup(Name);
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
   TGeoRef       *ref;

   if (Name) {
      entry=Tcl_FindHashEntry(&GeoRef_Table,Name);

      if (entry) {
         ref=(TGeoRef*)Tcl_GetHashValue(entry);
         if (GeoRef_Free(ref))
            Tcl_DeleteHashEntry(entry);
      }
   }
   return(TCL_OK);
}

/*--------------------------------------------------------------------------------------------------------------
 * Nom          : <GeoRef_Size>
 * Creation     : Juillet 2005 J.P. Gauthier
 *
 * But          : Initialiser les limites d'un georef.
 *
 * Parametres   :
 *   <Ref>      : Pointeur sur la reference geographique
 *   <X0>       : Coordonnee X minimale
 *   <Y0>       : Coordonnee Y minimale
 *   <Z0>       : Coordonnee Z minimale
 *   <X1>       : Coordonnee X maximale
 *   <Y1>       : Coordonnee Y maximale
 *   <Z1>       : Coordonnee Z maximale
 *   <BD>       : Bordure
 *
 * Retour       : Code de retour standard TCL
 *
 * Remarques :
 *
 *---------------------------------------------------------------------------------------------------------------
*/
void GeoRef_Size(TGeoRef *Ref,int X0,int Y0,int Z0,int X1,int Y1,int Z1,int BD) {

   Ref->X0=X0;
   Ref->X1=X1;
   Ref->Y0=Y0;
   Ref->Y1=Y1;
   Ref->Z0=Z0;
   Ref->Z1=Z1;
   Ref->BD=BD;
}

/*--------------------------------------------------------------------------------------------------------------
 * Nom          : <GeoRef_Free>
 * Creation     : Juillet 2005 J.P. Gauthier
 *
 * But          : Liberer les resources alloeur par un georef.
 *
 * Parametres   :
 *   <Ref>      : Pointeur sur la reference geographique
 *
 * Retour       : Code de retour standard TCL
 *
 * Remarques :
 *
 *---------------------------------------------------------------------------------------------------------------
*/
int GeoRef_Free(TGeoRef *Ref) {

   Tcl_MutexLock(&MUTEX_GEOREF);
   if (!Ref || --Ref->NRef>0) {
      Tcl_MutexUnlock(&MUTEX_GEOREF);
      return(0);
   }

   if (Ref->RefFrom) {
      Tcl_MutexUnlock(&MUTEX_GEOREF);
      GeoRef_Free(Ref->RefFrom);
      Tcl_MutexLock(&MUTEX_GEOREF);
   }
   GeoRef_Clear(Ref,1);
   free(Ref);
   Tcl_MutexUnlock(&MUTEX_GEOREF);
   return(1);
}

/*--------------------------------------------------------------------------------------------------------------
 * Nom          : <GeoRef_Clear>
 * Creation     : Mars 2005 J.P. Gauthier - CMC/CMOE
 *
 * But          : Liberer une structure de projection WKT.
 *
 * Parametres  :
 *   <Ref>     : Pointeur sur la reference geographique
 *   <New>     : Nouveau georef
 *
 * Retour       : Un code d'erreur Tcl standard.
 *
 * Remarques   :
 *
 *---------------------------------------------------------------------------------------------------------------
*/
void GeoRef_Clear(TGeoRef *Ref,int New) {

   if (New) {
      if (Ref->Name)         free(Ref->Name);         Ref->Name=NULL;
      if (Ref->Levels)       free(Ref->Levels);       Ref->Levels=NULL;
   }

   if (Ref->String)       free(Ref->String);       Ref->String=NULL;
   if (Ref->Transform)    free(Ref->Transform);    Ref->Transform=NULL;
   if (Ref->InvTransform) free(Ref->InvTransform); Ref->InvTransform=NULL;
   if (Ref->Pos)          free(Ref->Pos);          Ref->Pos=NULL;
   if (Ref->Lat)          free(Ref->Lat);          Ref->Lat=NULL;
   if (Ref->Lon)          free(Ref->Lon);          Ref->Lon=NULL;
   if (Ref->Hgt)          free(Ref->Hgt);          Ref->Hgt=NULL;
   if (Ref->Idx)          free(Ref->Idx);          Ref->Idx=NULL; Ref->NIdx=0;
   if (Ref->AX)           free(Ref->AX);           Ref->AX=NULL;
   if (Ref->AY)           free(Ref->AY);           Ref->AY=NULL;

// Due to ezscint bugs, I cannot release the grid (gdaxes)
//   if (Ref->Id>-1)
//      GeoRefEZ_Lock();
//      c_gdrls(Ref->Id);
//      GeoRefEZ_UnLock();

   if (Ref->GCPTransform) {
      GDALDestroyGCPTransformer(Ref->GCPTransform);
      Ref->GCPTransform=NULL;
   }

   if (Ref->Spatial) {
      OSRDestroySpatialReference(Ref->Spatial);
   }

   if (Ref->Function) {
      OCTDestroyCoordinateTransformation(Ref->Function);
      Ref->Function=NULL;
   }

   if (Ref->InvFunction) {
      OCTDestroyCoordinateTransformation(Ref->InvFunction);
      Ref->InvFunction=NULL;
   }
}

void GeoRef_Qualify(TGeoRef *Ref) {

  Coord co[2];
  double d[2];

  if (Ref->Grid[0]=='M' || Ref->Grid[0]=='Y' || Ref->Grid[1]=='Y' || Ref->Grid[1]=='Z' || Ref->Grid[0]=='X') {
      Ref->Type|=GRID_SPARSE;
   } else {
      Ref->Type|=GRID_REGULAR;
   }

   if (Ref->Grid[0]=='#') {
      Ref->Type|=GRID_TILE;
   }

   if (Ref->Grid[0]=='A' || Ref->Grid[0]=='G') {
      Ref->Type|=GRID_WRAP;
   } else {
      Ref->Project(Ref,Ref->X0+(Ref->X1-Ref->X0)/2.0,Ref->Y0+(Ref->Y1-Ref->Y0)/2.0,&co[0].lat,&co[0].lon,1,1);
      Ref->Project(Ref,Ref->X0+(Ref->X1-Ref->X0)/2.0+1.0,Ref->Y0+(Ref->Y1-Ref->Y0)/2.0,&co[1].lat,&co[1].lon,1,1);
      d[0]=DIST(0.0,DEG2RAD(co[0].lat),DEG2RAD(co[0].lon),DEG2RAD(co[1].lat),DEG2RAD(co[1].lon));

      Ref->Project(Ref,Ref->X0,Ref->Y0+(Ref->Y1-Ref->Y0)/2.0,&co[0].lat,&co[0].lon,1,1);
      Ref->Project(Ref,Ref->X1,Ref->Y0+(Ref->Y1-Ref->Y0)/2.0,&co[1].lat,&co[1].lon,1,1);
      d[1]=DIST(0.0,DEG2RAD(co[0].lat),DEG2RAD(co[0].lon),DEG2RAD(co[1].lat),DEG2RAD(co[1].lon));

      if (d[1]<=(d[0]*1.5)) {
         Ref->Type|=GRID_WRAP;
      }
   }

   if (Ref->Grid[0]=='V') {
      Ref->Type|=GRID_VERTICAL;
   }

   if (Ref->Grid[0]=='R') {
      Ref->Type|=GRID_RADIAL;
   }
}

int GeoRef_Equal(TGeoRef *Ref0,TGeoRef *Ref1) {

   if (!Ref0 || !Ref1) {
      return(0);
   }

   /*Pacth temporaire du au lagrangien qui doivent avoir des GeoRef differents*/
   if (Ref0->Grid[0]=='M' || Ref0->Grid[0]=='Y' || Ref0->Grid[1]=='Y' || Ref0->Grid[0]=='#' || Ref0->Grid[0]=='X')
      return(0);

   if (Ref1->Grid[0]=='M' || Ref1->Grid[0]=='Y' || Ref1->Grid[1]=='Y' || Ref1->Grid[0]=='#' || Ref1->Grid[0]=='X')
      return(0);

   if (Ref0->BD!=Ref1->BD || Ref0->X0!=Ref1->X0 || Ref0->X1!=Ref1->X1 || Ref0->Y0!=Ref1->Y0 || Ref0->Y1!=Ref1->Y1 || Ref0->Z0!=Ref1->Z0 || Ref0->Z1!=Ref1->Z1)
      return(0);

   if (Ref0->Grid[0]!=Ref1->Grid[0] || Ref0->LevelType!=Ref1->LevelType)
      return(0);

   if ((Ref0->LevelNb!=Ref1->LevelNb) || (Ref0->Levels && memcmp(Ref0->Levels,Ref1->Levels,Ref0->LevelNb*sizeof(float))!=0))
      return(0);

   if (Ref0->Id>-1 && Ref1->Id>-1 && Ref0->Id!=Ref1->Id)
      return(0);

   if (Ref0->R!=Ref1->R || Ref0->ResR!=Ref1->ResR || Ref0->ResA!=Ref1->ResA || Ref0->Loc.lat!=Ref1->Loc.lat || Ref0->Loc.lon!=Ref1->Loc.lon || Ref0->Loc.elev!=Ref1->Loc.elev)
      return(0);

   if ((Ref0->String && !Ref1->String) || (!Ref0->String && Ref1->String))
      return(0);

   if (Ref0->String && Ref1->String)
      if (strcmp(Ref0->String,Ref1->String)!=0)
         return(0);

   if ((Ref0->Transform && !Ref1->Transform) || (!Ref0->Transform && Ref1->Transform))
      return(0);

   if (Ref0->Transform && Ref1->Transform)
      if (memcmp(Ref0->Transform,Ref1->Transform,6*sizeof(double))!=0)
         return(0);

   return(1);
}

TGeoRef *GeoRef_Copy(TGeoRef *Ref) {

   if (Ref)
      GeoRef_Incr(Ref);
   return(Ref);
}

TGeoRef *GeoRef_Reference(TGeoRef *Ref) {

   TGeoRef *ref;

   ref=GeoRef_New();

   GeoRef_Incr(Ref);
   GeoRef_Incr(ref);
   ref->RefFrom=Ref;
   ref->Project=Ref->Project;
   ref->UnProject=Ref->UnProject;
   ref->Value=Ref->Value;
   ref->Distance=Ref->Distance;

   ref->Loc.lat=Ref->Loc.lat;
   ref->Loc.lon=Ref->Loc.lon;
   ref->Loc.elev=Ref->Loc.elev;
   ref->R=Ref->R;
   ref->ResR=Ref->ResR;
   ref->ResA=Ref->ResA;

   GeoRef_Put(NULL,NULL,ref);
   return(ref);
}

TGeoRef* GeoRef_Find(TGeoRef *Ref) {

   Tcl_HashSearch  ptr;
   Tcl_HashEntry  *entry=NULL;
   TGeoRef        *ref;

   if (!Ref) {
      Ref=GeoRef_New();
   }

   /*Look for an already existing object that could match*/
   entry=Tcl_FirstHashEntry(&GeoRef_Table,&ptr);

   while (entry) {
      ref=(TGeoRef*)Tcl_GetHashValue(entry);

      if (GeoRef_Equal(ref,Ref)) {
#ifdef DEBUG
         fprintf(stderr,"(DEBUG) GeoRef_Find: Found existing georef\n");
#endif
         GeoRef_Free(Ref);
         GeoRef_Incr(ref);
         return(ref);
      }
      entry=Tcl_NextHashEntry(&ptr);
   }

#ifdef DEBUG
         fprintf(stderr,"(DEBUG) GeoRef_Find: New georef\n");
#endif
   /*Otherwise, create a new one*/
   ref=Ref;
   ref->Id=ref->Id==-1?-(++TGeoRef_TableNo):ref->Id;

   GeoRef_Incr(ref);
   GeoRef_Put(NULL,NULL,ref);

   return(ref);
}

TGeoRef *GeoRef_HardCopy(TGeoRef *Ref) {

   TGeoRef *ref;

   ref=GeoRef_New();
   GeoRef_Size(ref,Ref->X0,Ref->Y0,Ref->Z0,Ref->X1,Ref->Y1,Ref->Z1,Ref->BD);

   ref->LevelNb=Ref->LevelNb;
   ref->Levels=(float*)malloc(ref->LevelNb*sizeof(float));
   memcpy(ref->Levels,Ref->Levels,ref->LevelNb*sizeof(float));
   ref->Grid[0]=Ref->Grid[0];
   ref->Project=Ref->Project;
   ref->UnProject=Ref->UnProject;
   ref->Value=Ref->Value;
   ref->Type=Ref->Type;

   switch(ref->Grid[0]) {
      case 'R' :
         ref->Id=-(++TGeoRef_TableNo);
         ref->Loc.lat=Ref->Loc.lat;
         ref->Loc.lon=Ref->Loc.lon;
         ref->Loc.elev=Ref->Loc.elev;
         ref->R=Ref->R;
         ref->ResR=Ref->ResR;
         ref->ResA=Ref->ResA;
      case 'W' :
         ref->Id=-(++TGeoRef_TableNo);
         GeoRef_WKTSet(ref,Ref->String,Ref->Transform,Ref->InvTransform,Ref->Spatial);
      default  :
         ref->Id=Ref->Id;
  }
  return(ref);
}

TGeoRef *GeoRef_Resize(TGeoRef *Ref,int NI,int NJ,int NK,int Type,float *Levels) {

   TGeoRef *ref;

   if (!Ref) {
      ref=GeoRef_New();
   } else {
      ref=GeoRef_HardCopy(Ref);
   }
   GeoRef_Size(ref,0,0,0,NI-1,NJ-1,NK-1,0);

   ref->LevelNb=(Ref && Ref->Grid[0]=='V')?NJ:NK;
   ref->LevelType=Type;
   ref->Levels=(float*)realloc(ref->Levels,ref->LevelNb*sizeof(float));
   if (Levels)
      memcpy(ref->Levels,Levels,ref->LevelNb*sizeof(float));

   return(GeoRef_Find(ref));
}

int GeoRef_Project(TGeoRef *Ref,double X,double Y,double *Lat,double *Lon,int Extrap,int Transform) {

   if (X>Ref->X1 || Y>Ref->Y1 || X<Ref->X0 || Y<Ref->Y0) {
      if (!Extrap) {
         *Lon=-999.0;
         *Lat=-999.0;
         return(0);
      }
   }
   *Lon=X/(Ref->X1-Ref->X0);
   *Lat=Y/(Ref->Y1-Ref->Y0);

   return(1);
}

int GeoRef_UnProject(TGeoRef *Ref,double *X,double *Y,double Lat,double Lon,int Extrap,int Transform) {

   *X=Lon*(Ref->X1-Ref->X0);
   *Y=Lat*(Ref->Y1-Ref->Y0);

   /*Check the grid limits*/
   if (*X>Ref->X1 || *Y>Ref->Y1 || *X<Ref->X0 || *Y<Ref->Y0) {
      if (!Extrap) {
         *X=-1.0;
         *Y=-1.0;
      }
      return(0);
   }
   return(1);
}

TGeoRef* GeoRef_New() {

   TGeoRef *ref=malloc(sizeof(TGeoRef));

   GeoRef_Size(ref,0,0,0,0,0,0,0);

   /*General*/
   ref->Name=NULL;
   ref->Id=-1;
   ref->Type=GRID_NONE;
   ref->NRef=0;
   ref->NIdx=0;
   ref->Pos=NULL;
   ref->Lat=NULL;
   ref->Lon=NULL;
   ref->Hgt=NULL;
   ref->Idx=NULL;
   ref->AX=NULL;
   ref->AY=NULL;
   ref->RefFrom=NULL;
   ref->Grid[0]='X';
   ref->Grid[1]='\0';
   ref->Grid[2]='\0';

   /*WKT Specific*/
   ref->String=NULL;
   ref->Spatial=NULL;
   ref->Function=NULL;
   ref->InvFunction=NULL;
   ref->Transform=NULL;
   ref->InvTransform=NULL;
   ref->GCPTransform=NULL;
   ref->GCPInvTransform=NULL;

   ref->Levels=NULL;
   ref->LevelType=LVL_UNDEF;
   ref->LevelNb=0;

   /*RDR Specific*/
   ref->Loc.lat=-999;
   ref->Loc.lon=-999;
   ref->Loc.elev=-999;
   ref->R=0;
   ref->CTH=0;
   ref->STH=0;
   ref->ResR=0;
   ref->ResA=0;

   /*General functions*/
   ref->Project=GeoRef_Project;
   ref->UnProject=GeoRef_UnProject;
   ref->Value=NULL;
   ref->Distance=NULL;

   return(ref);
}

/*--------------------------------------------------------------------------------------------------------------
 * Nom          : <GeoRef_Intersect>
 * Creation     : Aout 2006 J.P. Gauthier - CMC/CMOE
 *
 * But          : Verifier l'intersection de deux georeference, Ref0 dans Ref1.
 *
 * Parametres  :
 *   <Ref0>     : Pointeur sur la reference geographique 1
 *   <Ref1>     : Pointeur sur la reference geographique 2
 *   <X0,Y0,...>: Limites dans le Ref 1 du Ref 0
 *   <Border>   : Include border
 *
 * Retour       :
 *   <Inter>    : Booleen indiquant l'intersection
 *
 * Remarques   :
 *
 *---------------------------------------------------------------------------------------------------------------
*/
int GeoRef_Intersect(TGeoRef *Ref0,TGeoRef *Ref1,int *X0,int *Y0,int *X1,int *Y1,int Border) {

   double lat,lon,di,dj,in=0;
   double x0,y0,x1,y1;
   int    x,y;

   /*Source grid Y*/
   if (Ref1->Grid[0]=='Y') {
      *X0=Ref1->X0; *Y0=Ref1->Y0;
      *X1=Ref1->X1; *Y1=Ref1->Y1;
      return(1);
   }

   /*If destination is global*/
   if (Ref0->Type&GRID_WRAP) {
      *X0=Ref1->X0; *Y0=Ref1->Y0;
      *X1=Ref1->X1; *Y1=Ref1->Y1;
      in=1;
   }

   /*Test for limit source inclusion into destination*/
   if (!in) {
      x=0;
      Ref1->Project(Ref1,Ref1->X0,Ref1->Y0,&lat,&lon,0,1);
      x+=Ref0->UnProject(Ref0,&di,&dj,lat,lon,0,1);
      Ref1->Project(Ref1,Ref1->X0,Ref1->Y1,&lat,&lon,0,1);
      x+=Ref0->UnProject(Ref0,&di,&dj,lat,lon,0,1);
      Ref1->Project(Ref1,Ref1->X1,Ref1->Y0,&lat,&lon,0,1);
      x+=Ref0->UnProject(Ref0,&di,&dj,lat,lon,0,1);
      Ref1->Project(Ref1,Ref1->X1,Ref1->Y1,&lat,&lon,0,1);
      x+=Ref0->UnProject(Ref0,&di,&dj,lat,lon,0,1);
      if (x==4) {
         *X0=Ref1->X0; *Y0=Ref1->Y0;
         *X1=Ref1->X1; *Y1=Ref1->Y1;
         in=1;
      }
   }

   if (!in) {
      x0=y0=1e32;
      x1=y1=-1e32;

      /*Project Ref0 Border within Ref1 and get limits*/
      for(x=Ref0->X0,y=Ref0->Y0;x<=Ref0->X1;x++) {
         Ref0->Project(Ref0,x,y,&lat,&lon,0,1);
         Ref1->UnProject(Ref1,&di,&dj,lat,lon,1,1);
         x0=FMIN(x0,di); y0=FMIN(y0,dj);
         x1=FMAX(x1,di); y1=FMAX(y1,dj);
      }

      for(x=Ref0->X0,y=Ref0->Y1;x<=Ref0->X1;x++) {
         Ref0->Project(Ref0,x,y,&lat,&lon,0,1);
         Ref1->UnProject(Ref1,&di,&dj,lat,lon,1,1);
         x0=FMIN(x0,di); y0=FMIN(y0,dj);
         x1=FMAX(x1,di); y1=FMAX(y1,dj);
      }

      for(y=Ref0->Y0,x=Ref0->X0;y<=Ref0->Y1;y++) {
         Ref0->Project(Ref0,x,y,&lat,&lon,0,1);
         Ref1->UnProject(Ref1,&di,&dj,lat,lon,1,1);
         x0=FMIN(x0,di); y0=FMIN(y0,dj);
         x1=FMAX(x1,di); y1=FMAX(y1,dj);
      }

      for(y=Ref0->Y0,x=Ref0->X1;y<=Ref0->Y1;y++) {
         Ref0->Project(Ref0,x,y,&lat,&lon,0,1);
         Ref1->UnProject(Ref1,&di,&dj,lat,lon,1,1);
         x0=FMIN(x0,di); y0=FMIN(y0,dj);
         x1=FMAX(x1,di); y1=FMAX(y1,dj);
      }

      /*Test for north and south pole including grid*/
      if (Ref0->UnProject(Ref0,&di,&dj,90.0,0.0,0,1) && dj>Ref0->Y0+2 && dj<Ref0->Y1-2 && di>Ref0->X0+2 && di<Ref0->X1-2) {
         Ref1->UnProject(Ref1,&di,&dj,90.0,0.0,1,1);
         x0=FMIN(x0,di); y0=FMIN(y0,dj);
         x1=FMAX(x1,di); y1=FMAX(y1,dj);
      }
      if (Ref0->UnProject(Ref0,&di,&dj,-90.0,0.0,0,1) && dj>Ref0->Y0+2 && dj<Ref0->Y1-2 && di>Ref0->X0+2 && di<Ref0->X1-2) {
         Ref1->UnProject(Ref1,&di,&dj,-90.0,0.0,1,1);
         x0=FMIN(x0,di); y0=FMIN(y0,dj);
         x1=FMAX(x1,di); y1=FMAX(y1,dj);
      }

      *X0=floor(x0); *Y0=floor(y0);
      *X1=ceil(x1);  *Y1=ceil(y1);

      if (!VOUT(*X0,*X1,Ref1->X0,Ref1->X1) && !VOUT(*Y0,*Y1,Ref1->Y0,Ref1->Y1)) {
         in=1;
      }
   }

   /*Clamp the coordinates*/
   FCLAMP(Ref1,*X0,*Y0,*X1,*Y1);

   if (Border) {
      if (*X0==Ref1->X0) *X0+=Ref1->BD;
      if (*Y0==Ref1->Y0) *Y0+=Ref1->BD;
      if (*X1==Ref1->X1) *X1-=Ref1->BD;
      if (*Y1==Ref1->Y1) *Y1-=Ref1->BD;
   }

   return(in);
}

int GeoRef_Limits(TGeoRef *Ref,double *Lat0,double *Lon0,double *Lat1,double *Lon1) {

   int x,y;
   double di,dj,lat,lon;

   *Lat0=*Lon0=1e32;
   *Lat1=*Lon1=-1e32;

   /*Source grid Y*/
   if (Ref->Grid[0]=='Y') {
      for(x=0;x<((Ref->X1-Ref->X0)+1)*((Ref->Y1-Ref->Y0)+1);x++) {
         *Lat0=FMIN(*Lat0,Ref->Lat[x]); *Lon0=FMIN(*Lon0,Ref->Lon[x]);
         *Lat1=FMAX(*Lat1,Ref->Lat[x]);  *Lon1=FMAX(*Lon1,Ref->Lon[x]);
      }
      return(1);
   }

   /*If destination is global*/
   if (Ref->Type&GRID_WRAP) {
      *Lat0=-90.0;  *Lat1=90.0;
      *Lon0=-180.0; *Lon1=180.0;
      return(1);
   }

   /*Project Ref0 Border within Ref1 and get limits*/
   for(x=Ref->X0,y=Ref->Y0;x<=Ref->X1;x++) {
      Ref->Project(Ref,x,y,&lat,&lon,0,1);
      *Lat0=FMIN(*Lat0,lat); *Lon0=FMIN(*Lon0,lon);
      *Lat1=FMAX(*Lat1,lat); *Lon1=FMAX(*Lon1,lon);
   }

   for(x=Ref->X0,y=Ref->Y1;x<=Ref->X1;x++) {
      Ref->Project(Ref,x,y,&lat,&lon,0,1);
      *Lat0=FMIN(*Lat0,lat); *Lon0=FMIN(*Lon0,lon);
      *Lat1=FMAX(*Lat1,lat); *Lon1=FMAX(*Lon1,lon);
   }

   for(y=Ref->Y0,x=Ref->X0;y<=Ref->Y1;y++) {
      Ref->Project(Ref,x,y,&lat,&lon,0,1);
      *Lat0=FMIN(*Lat0,lat); *Lon0=FMIN(*Lon0,lon);
      *Lat1=FMAX(*Lat1,lat); *Lon1=FMAX(*Lon1,lon);
   }

   for(y=Ref->Y0,x=Ref->X1;y<=Ref->Y1;y++) {
      Ref->Project(Ref,x,y,&lat,&lon,0,1);
      *Lat0=FMIN(*Lat0,lat); *Lon0=FMIN(*Lon0,lon);
      *Lat1=FMAX(*Lat1,lat); *Lon1=FMAX(*Lon1,lon);
   }

   /*Test for north and south pole including grid*/
   if (Ref->UnProject(Ref,&di,&dj,90.0,0.0,0,1) && dj>Ref->Y0+2 && dj<Ref->Y1-2 && di>Ref->X0+2 && di<Ref->X1-2) {
      *Lat1=90.0;
   }
   if (Ref->UnProject(Ref,&di,&dj,-90.0,0.0,0,1) && dj>Ref->Y0+2 && dj<Ref->Y1-2 && di>Ref->X0+2 && di<Ref->X1-2) {
      *Lat0=-90.0;
   }
   return(1);
}

int GeoRef_Within(TGeoRef *Ref0,TGeoRef *Ref1) {

   double lat,lon,di,dj;
   int    x,y;

   /*Project Ref0 Border within Ref1 and get limits*/
   for(x=Ref0->X0,y=Ref0->Y0;x<=Ref0->X1;x++) {
      Ref0->Project(Ref0,x,y,&lat,&lon,0,1);
      if (!Ref1->UnProject(Ref1,&di,&dj,lat,lon,0,1)) {
         return(0);
      }
   }

   for(x=Ref0->X0,y=Ref0->Y1;x<=Ref0->X1;x++) {
      Ref0->Project(Ref0,x,y,&lat,&lon,0,1);
      if (!Ref1->UnProject(Ref1,&di,&dj,lat,lon,0,1)) {
         return(0);
      }
   }

   for(y=Ref0->Y0,x=Ref0->X0;y<=Ref0->Y1;y++) {
      Ref0->Project(Ref0,x,y,&lat,&lon,0,1);
      if (!Ref1->UnProject(Ref1,&di,&dj,lat,lon,0,1)) {
         return(0);
      };
   }

   for(y=Ref0->Y0,x=Ref0->X1;y<=Ref0->Y1;y++) {
      Ref0->Project(Ref0,x,y,&lat,&lon,0,1);
      if (!Ref1->UnProject(Ref1,&di,&dj,lat,lon,0,1)) {
         return(0);
      }
   }
   return(1);
}

int GeoRef_WithinRange(TGeoRef *Ref,double Lat0,double Lon0,double Lat1,double Lon1,int In) {

   double lat[4],lon[4],dl;
   int    d0,d1,d2,d3;

   if (Lon0*Lon1<0) {
      dl=Lon1-Lon0;
   } else {
      dl=0;
   }

   if (Lat0>Lat1) {
      dl=Lat0;
      Lat0=Lat1;
      Lat1=dl;
   }

   if (Lon0>Lon1) {
      dl=Lon0;
      Lon0=Lon1;
      Lon1=dl;
   }

   /* Check image within range */
   Ref->Project(Ref,Ref->X0,Ref->Y0,&lat[0],&lon[0],0,1);
   d0=FWITHIN(dl,Lat0,Lon0,Lat1,Lon1,lat[0],lon[0]);
   if (!In && d0) return(1);

   Ref->Project(Ref,Ref->X1,Ref->Y0,&lat[1],&lon[1],0,1);
   d1=FWITHIN(dl,Lat0,Lon0,Lat1,Lon1,lat[1],lon[1]);
   if (!In && d1) return(1);

   Ref->Project(Ref,Ref->X1,Ref->Y1,&lat[2],&lon[2],0,1);
   d2=FWITHIN(dl,Lat0,Lon0,Lat1,Lon1,lat[2],lon[2]);
   if (!In && d2) return(1);

   Ref->Project(Ref,Ref->X0,Ref->Y1,&lat[3],&lon[3],0,1);
   d3=FWITHIN(dl,Lat0,Lon0,Lat1,Lon1,lat[3],lon[3]);
   if (!In && d3) return(1);

   /* Check for all contained */
   if (In) {
      if (d0 && d1 && d2 && d3) {
         return(1);
      } else {
         return(0);
      }
   }

   /* Check range within image */
   lat[0]=FMIN(FMIN(FMIN(lat[0],lat[1]),lat[2]),lat[3]);
   lat[1]=FMAX(FMAX(FMAX(lat[0],lat[1]),lat[2]),lat[3]);
   lon[0]=FMIN(FMIN(FMIN(lon[0],lon[1]),lon[2]),lon[3]);
   lon[1]=FMAX(FMAX(FMAX(lon[0],lon[1]),lon[2]),lon[3]);

   if (FWITHIN(dl,lat[0],lon[0],lat[1],lon[1],Lat0,Lon0)) return(1);
   if (FWITHIN(dl,lat[0],lon[0],lat[1],lon[1],Lat0,Lon1)) return(1);
   if (FWITHIN(dl,lat[0],lon[0],lat[1],lon[1],Lat1,Lon1)) return(1);
   if (FWITHIN(dl,lat[0],lon[0],lat[1],lon[1],Lat1,Lon0)) return(1);

   return(0);
}
