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

   Tcl_CreateObjCommand(Interp,"georef",GeoRef_Cmd,(ClientData)NULL,(Tcl_CmdDeleteProc*)NULL);

   if (!GeoRefInit++) {
      Tcl_InitHashTable(&GeoRef_Table,TCL_STRING_KEYS);
      TGeoRef_TableNo=1;
   }

   return(TCL_OK);
}

void GeoRef_Incr(TGeoRef *Ref) {
   Tcl_MutexLock(&MUTEX_GEOREF);
   if (Ref) Ref->NRef++;
   Tcl_MutexUnlock(&MUTEX_GEOREF);
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
   if (Scan->D) free(Scan->D);

   Scan->X=Scan->Y=NULL;
   Scan->V=NULL;
   Scan->D=NULL;
   Scan->N=Scan->S=Scan->DX=Scan->DY=0;
}

/*--------------------------------------------------------------------------------------------------------------
 * Nom          : <GeoScan_Get>
 * Creation     : Fevrier 2008 J.P. Gauthier - CMC/CMOE
 *
 * But          : Reprojeter et extraire les valeurs a ces points
 *
 * Parametres   :
 *  <Scan>      : Buffer de reprojection
 *  <ToRef>     : Georeference destination
 *  <ToDef>     : Data definition destination
 *  <FromRef>   : Georeference source
 *  <FromDef>   : Data definition source
 *  <X0>        : Limite inferieure en X
 *  <Y0>        : Limite inferieure en Y
 *  <X1>        : Limite superieure en X
 *  <Y1>        : Limite superieure en Y
 *  <Dim>       : Dimension dee cellules de grilles (1=point, 2=area)
 *  <Degree>    : Interpolation degree
 *  <To>        : Georeference destination
 *  <From>      : Georeference source
 *
 * Retour       : Dimension des resultats
 *
 * Remarques    :
 *
 *---------------------------------------------------------------------------------------------------------------
*/

int GeoScan_Get(TGeoScan *Scan,TGeoRef *ToRef,TDataDef *ToDef,TGeoRef *FromRef,TDataDef *FromDef,int X0,int Y0,int X1,int Y1,int Dim,char *Degree) {

   register int idx,x,y,n=0;
   int          d=0,sz,dd;
   double       x0,y0,x1,y1;

   if (!Scan || !ToRef || !FromRef) {
      return(0);
   }

   /*Adjust scan buffer sizes*/
   Scan->DX=X1-X0+1;
   Scan->DY=Y1-Y0+1;
   dd=(Scan->DX+1)*(Scan->DY+1);
   sz=Scan->DX*Scan->DY;

   if (Scan->S<sz) {
      if (!(Scan->X=(void*)realloc(Scan->X,dd*sizeof(double))))
         return(0);
      if (!(Scan->Y=(void*)realloc(Scan->Y,dd*sizeof(double))))
         return(0);
      if (!(Scan->V=(unsigned int*)realloc(Scan->V,sz*sizeof(unsigned int))))
         return(0);
      if (!(Scan->D=(float*)realloc(Scan->D,sz*sizeof(float))))
         return(0);
      Scan->S=sz;
   }

   dd=Dim-1;
   Scan->N=0;

   /*WKT grid type*/
   if (FromRef->Grid[0]=='W') {
      for(y=Y0;y<=Y1+dd;y++) {
         idx=(y-FromRef->Y0)*FromDef->NI+(X0-FromRef->X0);
         for(x=X0;x<=X1+dd;x++,idx++,n++) {
            if (x<=X1 && y<=Y1) {
               Scan->V[Scan->N++]=idx;
            }

            x0=dd?x-0.5:x;
            y0=dd?y-0.5:y;
            if (FromRef->Transform) {
               Scan->X[n]=FromRef->Transform[0]+FromRef->Transform[1]*x0+FromRef->Transform[2]*y0;
               Scan->Y[n]=FromRef->Transform[3]+FromRef->Transform[4]*x0+FromRef->Transform[5]*y0;
            } else {
               Scan->X[n]=x0;
               Scan->Y[n]=y0;
            }
         }
      }
      if (FromRef->Function) {
         OCTTransform(FromRef->Function,n,Scan->X,Scan->Y,NULL);
      }

      d=dd?2:1;
      sz=8;

   /*Y Grid type*/
   } else if (FromRef->Grid[0]=='Y') {
      for(n=0;n<FromDef->NI;n++,idx++) {
         Scan->V[n]=idx;
         ((float*)Scan->X)[n]=FromRef->Lon[idx];
         ((float*)Scan->Y)[n]=FromRef->Lat[idx];
      }
      d=1;
      sz=4;
      Scan->N=n;

   /*Other RPN grids*/
   } else {
      for(y=Y0;y<=Y1+dd;y++) {
         idx=(y-FromRef->Y0)*FromDef->NI+(X0-FromRef->X0);
         for(x=X0;x<=X1+dd;x++,idx++,n++) {
            if (x<=X1 && y<=Y1) {
               Scan->V[Scan->N++]=idx;
            }

            ((float*)Scan->X)[n]=dd?x+0.5:x+1.0;
            ((float*)Scan->Y)[n]=dd?y+0.5:y+1.0;
         }
      }
      EZLock_RPNInt();
      c_gdllfxy(FromRef->Id,(float*)Scan->Y,(float*)Scan->X,(float*)Scan->X,(float*)Scan->Y,n);
      EZUnLock_RPNInt();

      d=dd?2:1;
      sz=4;
   }

   /*Project to destination grid*/
   if (ToRef->Grid[0]=='W') {
      for(x=n-1;x>=0;x--) {
         if (sz==4) {
            x0=(double)((float*)Scan->X)[x];
            y0=(double)((float*)Scan->Y)[x];
         } else {
            x0=Scan->X[x];
            y0=Scan->Y[x];

         }
         if (ToRef->UnProject(ToRef,&Scan->X[x],&Scan->Y[x],y0,x0,0,1)) {

            if (ToDef) {
               ToRef->Value(ToRef,ToDef,Degree[0],0,Scan->X[x],Scan->Y[x],0,&Scan->D[x],NULL);
            } else {
               /*If we're outside, set to nodata*/
               if (ToDef)
                  Scan->D[x]=ToDef->NoData;
            }
         }
      }

/*
         if (sz==4) {
            for(x=n-1;x>=0;x--) {
               Scan->X[x]=(double)((float*)Scan->X)[x];
               Scan->Y[x]=(double)((float*)Scan->Y)[x];
            }
         }

         if (ToRef->Function)
            OCTTransform(ToRef->InvFunction,n,Scan->X,Scan->Y,NULL);

         if (ToRef->InvTransform) {
            for(x=0;x<n;x++) {
               x0=ToRef->InvTransform[0]+ToRef->InvTransform[1]*Scan->X[x]+ToRef->InvTransform[2]*Scan->Y[x];
               y0=ToRef->InvTransform[3]+ToRef->InvTransform[4]*Scan->X[x]+ToRef->InvTransform[5]*Scan->Y[x];
               Scan->X[x]=x0;
               Scan->Y[x]=y0;
            }
         }
*/
   } else {
      if (sz==8) {
         for(x=0;x<n;x++) {
            /*RPN functions go from 0 to 360 instead of -180 to 180*/
            x0=Scan->X[x]<0?Scan->X[x]+360:Scan->X[x];
            y0=Scan->Y[x];
            ((float*)Scan->X)[x]=x0;
            ((float*)Scan->Y)[x]=y0;
         }
      }

      EZLock_RPNInt();
      c_gdxyfll(ToRef->Id,(float*)Scan->X,(float*)Scan->Y,(float*)Scan->Y,(float*)Scan->X,n);
      /*If we have the data of source, get it's values right now*/
      if (ToDef) {
         if (Degree)
            c_ezsetopt("INTERP_DEGREE",Degree);
         c_gdxysval(ToRef->Id,Scan->D,ToDef->Mode,(float*)Scan->X,(float*)Scan->Y,n);
      }
      EZUnLock_RPNInt();

      /*Cast back to double*/
      for(x=n-1;x>=0;x--) {
         Scan->X[x]=(double)((float*)Scan->X)[x]-1.0;
         Scan->Y[x]=(double)((float*)Scan->Y)[x]-1.0;

         /*If we're outside, set to nodata*/
         if (ToDef && !FIN2D(ToDef,Scan->X[x],Scan->Y[x])) {
            Scan->D[x]=ToDef->NoData;
         }
      }
   }
   return(d);
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
   double d0,d1,d2;

   GeoFunc_RadialPointOn(C1,C2,C3,&cr);

   d0=DIST(0,C1.Lat,C1.Lon,C2.Lat,C2.Lon);
   d1=DIST(0,C1.Lat,C1.Lon,cr.Lat,cr.Lon);
   d2=DIST(0,C2.Lat,C2.Lon,cr.Lat,cr.Lon);

   if(d2>d0) {
      return(-(d2-d0)/d0);
   } else {
      return(d1/d0);
   }
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
   crs12=COURSE(C1.Lat,C1.Lon,C2.Lat,C2.Lon);
   crs13=COURSE(C1.Lat,C1.Lon,C3.Lat,C3.Lon);
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

   sinc1.Lat=sin(C1.Lat);sinc1.Lon=sin(C1.Lon);
   cosc1.Lat=cos(C1.Lat);cosc1.Lon=cos(C1.Lon);
   sinc2.Lat=sin(C2.Lat);sinc2.Lon=sin(C2.Lon);
   cosc2.Lat=cos(C2.Lat);cosc2.Lon=cos(C2.Lon);

   dst12=2*asin(sqrt(pow((sin((C1.Lat-C2.Lat)/2)),2) + cosc1.Lat*cosc2.Lat*pow(sin((C1.Lon-C2.Lon)/2),2)));

   if (sin(C2.Lon-C1.Lon)<0) {
      crs12=acos((sinc2.Lat-sinc1.Lat*cos(dst12))/(sin(dst12)*cosc1.Lat));
   } else {
      crs12=2.0*M_PI-acos((sinc2.Lat-sinc1.Lat*cos(dst12))/(sin(dst12)*cosc1.Lat));
   }

   if (sin(C1.Lon-C2.Lon)<0) {
      crs21=acos((sinc1.Lat-sinc2.Lat*cos(dst12))/(sin(dst12)*cosc2.Lat));
   } else {
      crs21=M_2PI-acos((sinc1.Lat-sinc2.Lat*cos(dst12))/(sin(dst12)*cosc2.Lat));
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
      C3->Lat=asin(sinc1.Lat*cos(dst13)+cosc1.Lat*sin(dst13)*cos(CRS13));
      C3->Lon=fmod(C1.Lon-asin(sin(CRS13)*sin(dst13)/cos(C3->Lat))+M_PI,M_2PI)-M_PI;
   }

   return(1);
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

   double      x,y,lat0,lon0,lat1,lon1;
   int         idx,in=0,x0,y0,x1,y1,n;
   TGeoRef    *ref0,*ref1;
   Tcl_Obj    *lst;

   static CONST char *sopt[] = { "create","copy","free","define","project","unproject","limit","within","intersect","is","isequal","all","wipe",NULL };
   enum                opt { CREATE,COPY,FREE,DEFINE,PROJECT,UNPROJECT,LIMIT,WITHIN,INTERSECT,IS,ISEQUAL,ALL,WIPE };

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

         ref0=GeoRef_New();
         ref0->Id=-(++TGeoRef_TableNo);
         GeoRef_Incr(ref0);

         if (Objc==4) {
            GeoRef_WKTSet(ref0,Tcl_GetString(Objv[3]),NULL,NULL,NULL);
            ref0->Grid[0]='W';
            ref0->Grid[1]=ref0->Grid[2]='\0';
         }
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
            Tcl_WrongNumArgs(Interp,2,Objv,"georefto (lat0 lon0 lat1 lon1 [included]) | georeffrom");
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
         Tcl_SetObjResult(Interp,Tcl_NewBooleanObj(GeoRef_Equal(GeoRef_Get(Tcl_GetString(Objv[2])),GeoRef_Get(Tcl_GetString(Objv[3])),2)));
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
               ref->Grid[0]='W';
               ref->Grid[1]=ref->Grid[2]='\0';
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
                  return(TCL_ERROR);
               }

               if (nidx==0) {
                  GeoRef_WKTSet(ref,ref->String,NULL,NULL,NULL);
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
                     fprintf(stdout,"(WARNING) GeoRef_Define: Unable to generate the inverse transform matrix\n");
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
                  return(TCL_ERROR);
               }

               if (nidx==0) {
                  GeoRef_WKTSet(ref,ref->String,NULL,NULL,NULL);
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
                     fprintf(stdout,"(WARNING) GeoRef_Define: Unable to generate the transform matrix\n");
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
               Tcl_ListObjAppendElement(Interp,obj,Tcl_NewDoubleObj(ref->Loc.Lat));
               Tcl_ListObjAppendElement(Interp,obj,Tcl_NewDoubleObj(ref->Loc.Lon));
               Tcl_ListObjAppendElement(Interp,obj,Tcl_NewDoubleObj(ref->Loc.Elev));
               Tcl_SetObjResult(Interp,obj);
            } else {
               if (Objc>4) {
                  Tcl_AppendResult(Interp,"\n   GeoRef_Define: Invalid location, must be 4 \"",(char*)NULL);
                  return(TCL_ERROR);
               }
               Tcl_GetDoubleFromObj(Interp,Objv[++i],&ref->Loc.Lat);
               if (Objc>2)
                  Tcl_GetDoubleFromObj(Interp,Objv[++i],&ref->Loc.Lon);
               if (Objc>3)
                  Tcl_GetDoubleFromObj(Interp,Objv[++i],&ref->Loc.Elev);
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
         /*Check for non-existing name*/
         sprintf(buf,"GEOREF_____%li",GeoRefNo++);
         while (TclY_HashGet(&GeoRef_Table,buf)) {
            sprintf(buf,"GEOREF_____%li",GeoRefNo++);
         }
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
      entry=TclY_FindHashEntry(&GeoRef_Table,Name);

      if (entry) {
         ref=(TGeoRef*)Tcl_GetHashValue(entry);
         if (GeoRef_Free(ref))
            TclY_DeleteHashEntry(entry);
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

   int n;

   if (New) {
      if (Ref->Name)         free(Ref->Name);         Ref->Name=NULL;
      ZRef_Free(&Ref->ZRef);
   }

   if (Ref->String)       free(Ref->String);       Ref->String=NULL;
   if (Ref->Transform)    free(Ref->Transform);    Ref->Transform=NULL;
   if (Ref->InvTransform) free(Ref->InvTransform); Ref->InvTransform=NULL;
   if (Ref->Lat)          free(Ref->Lat);          Ref->Lat=NULL;
   if (Ref->Lon)          free(Ref->Lon);          Ref->Lon=NULL;
   if (Ref->Hgt)          free(Ref->Hgt);          Ref->Hgt=NULL;
   if (Ref->Idx)          free(Ref->Idx);          Ref->Idx=NULL; Ref->NIdx=0;
   if (Ref->AX)           free(Ref->AX);           Ref->AX=NULL;
   if (Ref->AY)           free(Ref->AY);           Ref->AY=NULL;

   Ref->IG1=Ref->IG2=Ref->IG3=Ref->IG4=0;

   if (Ref->Pos) {
      for(n=0;n<Ref->ZRef.LevelNb;n++) {
         if (Ref->Pos[n]) free(Ref->Pos[n]);
      }
      free(Ref->Pos);
      Ref->Pos=NULL;
   }

// Due to ezscint bugs, I cannot release the grid (gdaxes)
   if (Ref->Id>-1) {
/*
      EZLock_RPNInt();
      c_gdrls(Ref->Id);
      EZUnLock_RPNInt();
*/
   }

   if (Ref->GCPTransform) {
      GDALDestroyGCPTransformer(Ref->GCPTransform);
      Ref->GCPTransform=NULL;
   }
   if (Ref->TPSTransform) {
      GDALDestroyTPSTransformer(Ref->TPSTransform);
      Ref->TPSTransform=NULL;
   }
   if (Ref->RPCTransform) {
      GDALDestroyRPCTransformer(Ref->RPCTransform);
      Ref->RPCTransform=NULL;
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

   if (Ref) {
      if (Ref->Grid[0]=='X') {
         Ref->Type=GRID_NONE;
         return;
      }

      if (Ref->Grid[0]=='M' || Ref->Grid[0]=='Y' || Ref->Grid[1]=='X' || Ref->Grid[1]=='Y' || Ref->Grid[1]=='Z') {
         Ref->Type|=GRID_SPARSE;
      } else {
         Ref->Type|=GRID_REGULAR;
      }

      if (Ref->Grid[0]=='#') {
         Ref->Type|=GRID_TILE;
      }

      if (Ref->Grid[0]=='A' || Ref->Grid[0]=='G') {
         Ref->Type|=GRID_WRAP;
      } else if (Ref->Grid[0]!='V') {
         /*Get size of a gridpoint*/
         Ref->Project(Ref,Ref->X0+(Ref->X1-Ref->X0)/2.0,Ref->Y0+(Ref->Y1-Ref->Y0)/2.0,&co[0].Lat,&co[0].Lon,1,1);
         Ref->Project(Ref,Ref->X0+(Ref->X1-Ref->X0)/2.0+1.0,Ref->Y0+(Ref->Y1-Ref->Y0)/2.0,&co[1].Lat,&co[1].Lon,1,1);
         d[0]=DIST(0.0,DEG2RAD(co[0].Lat),DEG2RAD(co[0].Lon),DEG2RAD(co[1].Lat),DEG2RAD(co[1].Lon));

         /*Get distance between first and lat point*/
         Ref->Project(Ref,Ref->X0,Ref->Y0+(Ref->Y1-Ref->Y0)/2.0,&co[0].Lat,&co[0].Lon,1,1);
         Ref->Project(Ref,Ref->X1,Ref->Y0+(Ref->Y1-Ref->Y0)/2.0,&co[1].Lat,&co[1].Lon,1,1);
         d[1]=DIST(0.0,DEG2RAD(co[0].Lat),DEG2RAD(co[0].Lon),DEG2RAD(co[1].Lat),DEG2RAD(co[1].Lon));

         /*If we're within 1.5 grid point, we wrap*/
         if (d[1]<=(d[0]*1.5)) {
            Ref->Type|=GRID_WRAP;
         }

         /*If we're within 0.25 grid point, we repeat*/
         if (d[1]<=(d[0]*0.25)) {
            Ref->Type|=GRID_REPEAT;
         }
      }


      if (Ref->Grid[0]=='V') {
         Ref->Type|=GRID_VERTICAL;
      }

      if (Ref->Grid[0]=='R') {
         Ref->Type|=GRID_RADIAL;
      }
   }
}

int GeoRef_Equal(TGeoRef *Ref0,TGeoRef *Ref1,int Dim) {

   if (!Ref0 || !Ref1) {
      return(0);
   }

   if (Ref0->IG1!=Ref1->IG1 || Ref0->IG2!=Ref1->IG2 || Ref0->IG3!=Ref1->IG3 || Ref0->IG4!=Ref1->IG4)
     return(0);

   /*Pacth temporaire du au lagrangien qui doivent avoir des GeoRef differents*/
   if (Ref0->Grid[0]=='M' || Ref0->Grid[0]=='Y' || Ref0->Grid[1]=='Y' || Ref0->Grid[0]=='#')
      return(0);

   if (Ref1->Grid[0]=='M' || Ref1->Grid[0]=='Y' || Ref1->Grid[1]=='Y' || Ref1->Grid[0]=='#')
      return(0);

   /*Test for limits but only for ther refs with transforms (bands)*/
   if (Ref0->Transform || Ref1->Transform)
      if (Ref0->BD!=Ref1->BD || Ref0->X0!=Ref1->X0 || Ref0->X1!=Ref1->X1 || Ref0->Y0!=Ref1->Y0 || Ref0->Y1!=Ref1->Y1 || Ref0->Z0!=Ref1->Z0 || Ref0->Z1!=Ref1->Z1)
         return(0);

   if (Ref0->Grid[0]!=Ref1->Grid[0] || Ref0->Grid[1]!=Ref1->Grid[1])
      return(0);

   if (Dim==3 && !ZRef_Equal(&Ref0->ZRef,&Ref1->ZRef))
      return(0);

   if (Ref0->Id>-1 && Ref1->Id>-1 && Ref0->Id!=Ref1->Id)
      return(0);

   if (Ref0->R!=Ref1->R || Ref0->ResR!=Ref1->ResR || Ref0->ResA!=Ref1->ResA || Ref0->Loc.Lat!=Ref1->Loc.Lat || Ref0->Loc.Lon!=Ref1->Loc.Lon || Ref0->Loc.Elev!=Ref1->Loc.Elev)
      return(0);

   if ((Ref0->Spatial && !Ref1->Spatial) || (!Ref0->Spatial && Ref1->Spatial))
      return(0);

   if (Ref0->Spatial && Ref1->Spatial && !OSRIsSame(Ref0->Spatial,Ref1->Spatial))
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

   ref->Loc.Lat=Ref->Loc.Lat;
   ref->Loc.Lon=Ref->Loc.Lon;
   ref->Loc.Elev=Ref->Loc.Elev;
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
   } else {

      TclY_LockHash();

      /*Look for an already existing object that could match*/
      entry=Tcl_FirstHashEntry(&GeoRef_Table,&ptr);

      while (entry) {
         ref=(TGeoRef*)Tcl_GetHashValue(entry);

         if (GeoRef_Equal(ref,Ref,3)) {

            GeoRef_Free(Ref);
            GeoRef_Incr(ref);
            TclY_UnlockHash();
            return(ref);
         }
         entry=Tcl_NextHashEntry(&ptr);
      }
      TclY_UnlockHash();
   }

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

   ref->Grid[0]=Ref->Grid[0];
   ref->Project=Ref->Project;
   ref->UnProject=Ref->UnProject;
   ref->Value=Ref->Value;
   ref->Type=Ref->Type;

   ref->IG1==Ref->IG1;
   ref->IG2==Ref->IG2;
   ref->IG3==Ref->IG3;
   ref->IG4==Ref->IG4;

   ZRef_Copy(&ref->ZRef,&Ref->ZRef);

   switch(ref->Grid[0]) {
      case 'R' :
         ref->Id=-(++TGeoRef_TableNo);
         ref->Loc.Lat=Ref->Loc.Lat;
         ref->Loc.Lon=Ref->Loc.Lon;
         ref->Loc.Elev=Ref->Loc.Elev;
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

   ref->ZRef.LevelNb=(Ref && Ref->Grid[0]=='V')?NJ:NK;
   ref->ZRef.Type=Type;
   ref->ZRef.Levels=(float*)realloc(ref->ZRef.Levels,ref->ZRef.LevelNb*sizeof(float));
   if (Levels)
      memcpy(ref->ZRef.Levels,Levels,ref->ZRef.LevelNb*sizeof(float));

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
   ref->IG1=ref->IG2=ref->IG3=ref->IG4=0;

   /*WKT Specific*/
   ref->String=NULL;
   ref->Spatial=NULL;
   ref->Function=NULL;
   ref->InvFunction=NULL;
   ref->Transform=NULL;
   ref->InvTransform=NULL;
   ref->GCPTransform=NULL;
   ref->TPSTransform=NULL;
   ref->RPCTransform=NULL;
   ref->LLExtent.MinX=1e32;
   ref->LLExtent.MinY=1e32;
   ref->LLExtent.MaxX=-1e32;
   ref->LLExtent.MaxY=-1e32;

   /*RDR Specific*/
   ref->Loc.Lat=-999;
   ref->Loc.Lon=-999;
   ref->Loc.Elev=-999;
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

   /*Vertical reference*/
   ZRef_Init(&ref->ZRef);

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
   x0=y0=1e32;
   x1=y1=-1e32;
   x=0;

   if (!in) {
      Ref1->Project(Ref1,Ref1->X0,Ref1->Y0,&lat,&lon,0,1);
      if (Ref0->UnProject(Ref0,&di,&dj,lat,lon,0,1)) {
         x0=Ref1->X0;y0=Ref1->Y0;
         x++;
      }
      Ref1->Project(Ref1,Ref1->X0,Ref1->Y1,&lat,&lon,0,1);
      if (Ref0->UnProject(Ref0,&di,&dj,lat,lon,0,1)) {
         x0=Ref1->X0;y1=Ref1->Y1;
         x++;
      }
      Ref1->Project(Ref1,Ref1->X1,Ref1->Y0,&lat,&lon,0,1);
      if (Ref0->UnProject(Ref0,&di,&dj,lat,lon,0,1)) {
         x1=Ref1->X1;y0=Ref1->Y0;
         x++;
      }
      Ref1->Project(Ref1,Ref1->X1,Ref1->Y1,&lat,&lon,0,1);
      if (Ref0->UnProject(Ref0,&di,&dj,lat,lon,0,1)) {
         x1=Ref1->X1;y1=Ref1->Y1;
      }
      *X0=x0; *Y0=y0;
      *X1=x1; *Y1=y1;

      if (x>=3) {
         in=1;
      }
   }

   if (!in) {

      /*Project Ref0 Border within Ref1 and get limits*/
      for(x=Ref0->X0;x<=Ref0->X1;x++) {
         Ref0->Project(Ref0,x,Ref0->Y0,&lat,&lon,0,1);
         Ref1->UnProject(Ref1,&di,&dj,lat,lon,1,1);
         x0=FMIN(x0,di); y0=FMIN(y0,dj);
         x1=FMAX(x1,di); y1=FMAX(y1,dj);

         Ref0->Project(Ref0,x,Ref0->Y1,&lat,&lon,0,1);
         Ref1->UnProject(Ref1,&di,&dj,lat,lon,1,1);
         x0=FMIN(x0,di); y0=FMIN(y0,dj);
         x1=FMAX(x1,di); y1=FMAX(y1,dj);
      }

      for(y=Ref0->Y0;y<=Ref0->Y1;y++) {
         Ref0->Project(Ref0,Ref0->X0,y,&lat,&lon,0,1);
         Ref1->UnProject(Ref1,&di,&dj,lat,lon,1,1);
         x0=FMIN(x0,di); y0=FMIN(y0,dj);
         x1=FMAX(x1,di); y1=FMAX(y1,dj);

         Ref0->Project(Ref0,Ref0->X1,y,&lat,&lon,0,1);
         Ref1->UnProject(Ref1,&di,&dj,lat,lon,1,1);
         x0=FMIN(x0,di); y0=FMIN(y0,dj);
         x1=FMAX(x1,di); y1=FMAX(y1,dj);
      }

      /*Test for north and south pole including grid*/
      if (Ref0->UnProject(Ref0,&di,&dj,89.9,0.0,0,1) && dj>Ref0->Y0+2 && dj<Ref0->Y1-2 && di>Ref0->X0+2 && di<Ref0->X1-2) {
         Ref1->UnProject(Ref1,&di,&dj,89.9,0.0,1,1);
         x0=FMIN(x0,di); y0=FMIN(y0,dj);
         x1=FMAX(x1,di); y1=FMAX(y1,dj);
      }
      if (Ref0->UnProject(Ref0,&di,&dj,-89.9,0.0,0,1) && dj>Ref0->Y0+2 && dj<Ref0->Y1-2 && di>Ref0->X0+2 && di<Ref0->X1-2) {
         Ref1->UnProject(Ref1,&di,&dj,-89.9,0.0,1,1);
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

/*--------------------------------------------------------------------------------------------------------------
 * Nom          : <GeoRef_Limits>
 * Creation     : Aout 2006 J.P. Gauthier - CMC/CMOE
 *
 * But          : Calculer les limites en latlon de la couverture d'une georeference.
 *
 * Parametres  :
 *   <Ref>     : Pointeur sur la reference geographique
 *   <Lat0>    : Latitude inferieure
 *   <Lon0>    : Longitude inferieure
 *   <Lat1>    : Latitude superieure
 *   <Lon1>    : Longitude superieure
 *
 * Retour       :
 *   <valid>    : Booleen indiquant la validite
 *
 * Remarques   :
 *
 *---------------------------------------------------------------------------------------------------------------
*/
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

/*--------------------------------------------------------------------------------------------------------------
 * Nom          : <GeoRef_Valid>
 * Creation     : Fevrier 2009. Gauthier - CMC/CMOE
 *
 * But          : Verifier la validite d'une georeference.
 *
 * Parametres  :
 *   <Ref>      : Pointeur sur la reference
 *
 * Retour       :
 *   <valid>    : Booleen indiquant la validite
 *
 * Remarques   :
 *    - On projete la bounding box et si les latitudes sont en dehors de -90 90 alors c'est pas bon
 *---------------------------------------------------------------------------------------------------------------
*/
int GeoRef_Valid(TGeoRef *Ref) {

   Coord co[2];

   Ref->Project(Ref,Ref->X0,Ref->Y0,&co[0].Lat,&co[0].Lon,1,1);
   Ref->Project(Ref,Ref->X1,Ref->Y1,&co[1].Lat,&co[1].Lon,1,1);
   if (co[0].Lat<-91 || co[0].Lat>91.0 || co[1].Lat<-91 || co[1].Lat>91.0) {
      return(0);
   }
   return(1);
}


/*--------------------------------------------------------------------------------------------------------------
 * Nom          : <GeoRef_Positional>
 * Creation     : Mars 2010 J.P. Gauthier - CMC/CMOE
 *
 * But          : Assigner des vecteurs de positions X et Y
 *
 * Parametres   :
 *   <Ref>      : Pointeur sur la reference
 *   <XDef>     : Data definition des positions en X
 *   <YDef>     : Data definition des positions en Y
 *
 * Retour       : Dimension des resultats
 *
 * Remarques    :
 *
 *---------------------------------------------------------------------------------------------------------------
*/
int GeoRef_Positional(TGeoRef *Ref,TDataDef *XDef,TDataDef *YDef) {

   int d;

   /*Clear arrays*/
   if (Ref->Lat) free(Ref->Lat);
   if (Ref->Lon) free(Ref->Lon);

   Ref->Lat=(float*)malloc(FSIZE2D(YDef)*sizeof(float));
   Ref->Lon=(float*)malloc(FSIZE2D(XDef)*sizeof(float));

   if (!Ref->Lat || !Ref->Lon) {
      return(0);
   }

   /*Assign positionals, if size is float, just memcopy otherwise, assign*/
   if (XDef->Type==TD_Float32) {
      memcpy(Ref->Lon,XDef->Data[0],FSIZE2D(XDef)*sizeof(float));
   } else {
      for(d=0;d<FSIZE2D(XDef);d++) {
         Def_Get(XDef,0,d,Ref->Lon[d]);
      }
   }

   if (YDef->Type==TD_Float32) {
      memcpy(Ref->Lat,YDef->Data[0],FSIZE2D(YDef)*sizeof(float));
   } else {
      for(d=0;d<FSIZE2D(YDef);d++) {
         Def_Get(YDef,0,d,Ref->Lat[d]);
      }
   }

   /*Get rid of transforms and projection functions if the positionnale are already in latlon (GDAL case)*/
   if (Ref->Grid[1]=='\0') {
      if (Ref->Transform)    free(Ref->Transform);    Ref->Transform=NULL;
      if (Ref->InvTransform) free(Ref->InvTransform); Ref->InvTransform=NULL;
      if (Ref->Function) {
         OCTDestroyCoordinateTransformation(Ref->Function);
         Ref->Function=NULL;
      }
   }

   /*Set secondary gridtype to Y for the project/unproject functions to work correctly*/
   if (Ref->Grid[0]=='W')
      Ref->Grid[1]='Y';

   return(1);
}
