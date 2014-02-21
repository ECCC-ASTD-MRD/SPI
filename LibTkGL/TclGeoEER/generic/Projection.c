/*=============================================================================
 * Environnement Canada
 * Centre Meteorologique Canadian
 * 2100 Trans-Canadienne
 * Dorval, Quebec
 *
 * Projet    : Projection diverses de la carte vectorielle.
 * Fichier   : Projection.c
 * Creation  : Juin 1996 - J.P. Gauthier
 *
 * Description: Fichier de definition du module Projection.
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

#include <stdio.h>
#include <string.h>
#include <math.h>
#include <sys/types.h>
#include "Projection.h"

#include "tclGDAL.h"
#include "tclOGR.h"
#include "tcl3DModel.h"

static Tcl_HashTable  ProjectionTable;
static Tcl_HashTable  ProjectionTypes;
static int            ProjectionInit=0;

static int Projection_Cmd(ClientData clientData,Tcl_Interp *Interp,int Objc,Tcl_Obj *CONST Objv[]);
static int Projection_Config(Tcl_Interp *Interp,char *Name,int Objc,Tcl_Obj *CONST Objv[]);
static int Projection_Create(Tcl_Interp *Interp,char *Name);
static int Projection_Destroy(Tcl_Interp *Interp,char *Name);
static int Projection_Function(Tcl_Interp *Interp,char *Name,int Objc,Tcl_Obj *CONST Objv[]);

/*----------------------------------------------------------------------------
 * Nom      : <Projection_Cmd>
 * Creation : Aout 1999 - J.P. Gauthier
 *
 * But      : Effectuer les comandes du package.
 *
 * Parametres    :
 *  <clientData> : Nom de la projection
 *  <interp>     : Interpreteur TCL
 *  <Objc>       : Nombre d'arguments
 *  <Objv>       : Pointeur sur la liste des arguments
 *
 * Retour:
 *  <TCL_...>    : Code de retour standard TCL
 *
 * Remarques :
 *
 *----------------------------------------------------------------------------
*/
static int Projection_Cmd(ClientData clientData,Tcl_Interp *Interp,int Objc,Tcl_Obj *CONST Objv[]){

   int                idx,t;
   short              zs;
   unsigned char      zc;
   double             lat,lon;
   Projection        *proj;
   static CONST char *sopt[] = { "create","configure","function","destroy","clean","is","data","loading",NULL };
   enum               opt { CREATE,CONFIGURE,FUNCTION,DESTROY,CLEAN,IS,DATA,LOADING };

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
         if (Objc<3) {
            Tcl_WrongNumArgs(Interp,2,Objv,"name ?option?");
            return(TCL_ERROR);
         }
         t=Projection_Create(Interp,Tcl_GetString(Objv[2]));
         if (t!=TCL_ERROR && Objc>3) {
            t=Projection_Config(Interp,Tcl_GetString(Objv[2]),Objc-3,Objv+3);
         }
         return(t);
         break;

      case CONFIGURE:
         if (Objc<3) {
            Tcl_WrongNumArgs(Interp,2,Objv,"name ?option?");
            return(TCL_ERROR);
         }
         return(Projection_Config(Interp,Tcl_GetString(Objv[2]),Objc-3,Objv+3));
         break;

      case FUNCTION:
         if (Objc<3) {
            Tcl_WrongNumArgs(Interp,2,Objv,"name ?option?");
            return(TCL_ERROR);
         }
         return(Projection_Function(Interp,Tcl_GetString(Objv[2]),Objc-3,Objv+3));
         break;

      case DESTROY:
         if (Objc!=3) {
            Tcl_WrongNumArgs(Interp,2,Objv,"name");
            return(TCL_ERROR);
         }
         return(Projection_Destroy(Interp,Tcl_GetString(Objv[2])));
         break;

      case CLEAN:
         proj=Projection_Get(Tcl_GetString(Objv[2]));
         if (!proj) {
            Tcl_AppendResult(Interp,"\n   Projection_Cmd: Projection name unknown: \"",Tcl_GetString(Objv[2]),"\"",(char *)NULL);
            return(TCL_ERROR);
         }
         if (strcmp(proj->Type->Name,"grid")==0) {
            Grid_Setup(Interp,proj);
         }
         ViewportClean(proj->VP,1,1);
         Projection_Clean(Interp,proj,GDB_FORCE);
         break;

      case DATA:
        if (Objc<6) {
            Tcl_WrongNumArgs(Interp,2,Objv,"name [-BATHYMETRY|-TOPOGRAPHY|-MASK|-TYPE] lat lon");
            return(TCL_ERROR);
         }
         proj=Projection_Get(Tcl_GetString(Objv[2]));
         Tcl_GetDoubleFromObj(Interp,Objv[4],&lat);
         Tcl_GetDoubleFromObj(Interp,Objv[5],&lon);
#ifdef HAVE_GDB
         if (strncmp(Tcl_GetString(Objv[3]),"-BATH",5)==0) {
            t=2;
            gdb_mapget(proj->Geo->Maps[GDB_MAP_BAT],lat,lon,(char*)&zs);
            zs=zs>0?0:zs;
         } else if (strncmp(Tcl_GetString(Objv[3]),"-TOPO",5)==0) {
            t=2;
            gdb_mapget(proj->Geo->Maps[GDB_MAP_DEM],lat,lon,(char*)&zs);
         } else if (strncmp(Tcl_GetString(Objv[3]),"-MASK",5)==0) {
            t=1;
            gdb_mapget(proj->Geo->Maps[GDB_MAP_MSK],lat,lon,(char*)&zc);
            zs=zc<127?0:1;
         } else if (strncmp(Tcl_GetString(Objv[3]),"-TYPE",5)==0) {
            t=1;
            gdb_mapget(proj->Geo->Maps[GDB_MAP_TER],lat,lon,(char*)&zc);
            zs=zc;
         } else {
            Tcl_AppendResult(Interp,"Invalid data type , must be one of -BATHYMETRY, -TOPOGRAPHY, -MASK, -TYPE",(char*)NULL);
         }
#endif
         Tcl_SetObjResult(Interp,Tcl_NewIntObj(zs));
         break;

      case IS:
         if (Objc!=3) {
            Tcl_WrongNumArgs(Interp,2,Objv,"name");
            return(TCL_ERROR);
         }
         if (Projection_Get(Tcl_GetString(Objv[2]))) {
            Tcl_SetObjResult(Interp,Tcl_NewBooleanObj(1));
         } else {
            Tcl_SetObjResult(Interp,Tcl_NewBooleanObj(0));
         }
         break;

      case LOADING:
        proj=Projection_Get(Tcl_GetString(Objv[2]));
         if (!proj) {
            Tcl_AppendResult(Interp,"\n   Projection_Cmd: Projection name unknown: \"",Tcl_GetString(Objv[2]),"\"",(char *)NULL);
            return(TCL_ERROR);
         }
         Tcl_SetObjResult(Interp,Tcl_NewIntObj(proj->Loading));
         break;
   }
   return TCL_OK;
}

/*----------------------------------------------------------------------------
 * Nom      : <Projection_Function>
 * Creation : Janvier 2005 - J.P. Gauthier - CMC/CMOE
 *
 * But      : Cette procedure fait l'appel des fonctions de transformations
 *            dans les projections.
 *
 * Parametres :
 *  <Interp>  : Interpreter TCL
 *  <Name>    : Projection
 *  <Objc>    : Nombre d'arguments
 *  <Objv>    : Liste des arguments
 *
 * Retour:
 *  <TCL_..>  : Code de reussite TCL.
 *
 * Remarques :
 *
 *----------------------------------------------------------------------------
*/
static int Projection_Function(Tcl_Interp *Interp,char *Name,int Objc,Tcl_Obj *CONST Objv[]){

   Tcl_Obj    *obj,*tmp;
   Projection *proj;
   int         idx,n,nobj;
   Coord       loc0,loc1,loct;
   double      x,y,d;

   static CONST char *sopt[] = { "-path","-dist","-area","-bearing","-circle","-coordgrid","-gridcoord",NULL };
   enum                opt { PATH,DIST,AREA,BEARING,CIRCLE,COORDGRID,GRIDCOORD };

   proj=Projection_Get(Name);
   if (!proj) {
      Tcl_AppendResult(Interp,"\n   Projection_Config: Projection name unknown: \"",Name,"\"",(char *)NULL);
      return TCL_ERROR;
   }

   Tcl_ResetResult(Interp);

   if (Objc<2) {
      Tcl_WrongNumArgs(Interp,1,Objv,"command ?arg arg ...?");
      return(TCL_ERROR);
   }

   if (Tcl_GetIndexFromObj(Interp,Objv[0],sopt,"command",0,&idx)!=TCL_OK) {
      return(TCL_ERROR);
   }

   switch ((enum opt)idx) {

      case COORDGRID:
         if (Objc!=3){
            Tcl_WrongNumArgs(Interp,2,Objv,"x y");
            return(TCL_ERROR);
         }
         Tcl_GetDoubleFromObj(Interp,Objv[1],&x);
         Tcl_GetDoubleFromObj(Interp,Objv[2],&y);
         if (proj->Ref && proj->Geographic) {
            proj->Ref->Project(proj->Ref,x,y,&loc0.Lat,&loc0.Lon,1,1);
         } else {
            loc0.Lat=y;
            loc0.Lon=x;
         }
         obj=Tcl_NewListObj(0,NULL);
         Tcl_ListObjAppendElement(Interp,obj,Tcl_NewDoubleObj(loc0.Lat));
         Tcl_ListObjAppendElement(Interp,obj,Tcl_NewDoubleObj(loc0.Lon));
         Tcl_SetObjResult(Interp,obj);
         break;

      case GRIDCOORD:
         if (Objc!=3){
            Tcl_WrongNumArgs(Interp,2,Objv,"lat lon");
            return(TCL_ERROR);
         }
         Tcl_GetDoubleFromObj(Interp,Objv[1],&loc0.Lat);
         Tcl_GetDoubleFromObj(Interp,Objv[2],&loc0.Lon);
         if (proj->Ref && proj->Geographic) {
            proj->Ref->UnProject(proj->Ref,&x,&y,loc0.Lat,loc0.Lon,1,1);
         } else {
            y=loc0.Lat;
            x=loc0.Lon;
         }
         obj=Tcl_NewListObj(0,NULL);
         Tcl_ListObjAppendElement(Interp,obj,Tcl_NewDoubleObj(x));
         Tcl_ListObjAppendElement(Interp,obj,Tcl_NewDoubleObj(y));
         Tcl_SetObjResult(Interp,obj);
         break;

      case PATH:
         if (Objc!=3){
            Tcl_WrongNumArgs(Interp,0,Objv,"{ coords } dist");
            return(TCL_ERROR);
         }
         Tcl_GetDoubleFromObj(Interp,Objv[2],&x);

         if (proj->Type && proj->Type->Def==PROJPLANE) {
            Tcl_SetObjResult(Interp,(Tcl_Obj*)Grid_Path(Interp,proj,Objv[1],x));
         } else {
            Tcl_SetObjResult(Interp,(Tcl_Obj*)Projection_Path(Interp,Objv[1],x));
         }
         break;

      case CIRCLE:
         if (Objc!=5 && Objc!=6) {
            Tcl_WrongNumArgs(Interp,0,Objv,"lat lon dist angle [coords]");
            return(TCL_ERROR);
         }
         if (!proj->Geographic) {
            Tcl_AppendResult(Interp,"Projection is not geographic",(char*)NULL);
            return(TCL_ERROR);
         }

         Tcl_GetDoubleFromObj(Interp,Objv[1],&loc0.Lat);
         Tcl_GetDoubleFromObj(Interp,Objv[2],&loc0.Lon);
         Tcl_GetDoubleFromObj(Interp,Objv[3],&d);
         Tcl_GetDoubleFromObj(Interp,Objv[4],&x);

         loc0.Lat=DEG2RAD(loc0.Lat);loc0.Lon=DEG2RAD(loc0.Lon);
         x=DEG2RAD(x);
         d=M2RAD(d);

         loc1.Lat=asin(sin(loc0.Lat)*cos(d)+cos(loc0.Lat)*sin(d)*cos(x));
         loc1.Lon=fmod(loc0.Lon-(atan2(sin(x)*sin(d)*cos(loc0.Lat),cos(d)-sin(loc0.Lat)*sin(loc1.Lat)))+M_PI,M_2PI)-M_PI;

         obj=Tcl_NewListObj(0,NULL);

         if (Objc==6) {
            // There is a list of coordinates to translate
            Tcl_ListObjLength(Interp,Objv[5],&nobj);
            for(n=0;n<nobj;n+=2) {
               // Get the point coordinate
               Tcl_ListObjIndex(Interp,Objv[5],n,&tmp);
               Tcl_GetDoubleFromObj(Interp,tmp,&loct.Lat);
               Tcl_ListObjIndex(Interp,Objv[5],n+1,&tmp);
               Tcl_GetDoubleFromObj(Interp,tmp,&loct.Lon);
               loct.Lat=DEG2RAD(loct.Lat);loct.Lon=DEG2RAD(loct.Lon);

               // Get its distance and angle from original location
               d=M2RAD(DIST(0.0,loct.Lat,loct.Lon,loc0.Lat,loc0.Lon));
               x=COURSE(loc0.Lat,loc0.Lon,loct.Lat,loct.Lon);

               // Figure out new location
               loct.Lat=asin(sin(loc1.Lat)*cos(d)+cos(loc1.Lat)*sin(d)*cos(x));
               loct.Lon=fmod(loc1.Lon-(atan2(sin(x)*sin(d)*cos(loc1.Lat),cos(d)-sin(loc1.Lat)*sin(loct.Lat)))+M_PI,M_2PI)-M_PI;

               Tcl_ListObjAppendElement(Interp,obj,Tcl_NewDoubleObj(RAD2DEG(loct.Lat)));
               Tcl_ListObjAppendElement(Interp,obj,Tcl_NewDoubleObj(RAD2DEG(loct.Lon)));
            }
         } else {
            // Translate only the specified location
            Tcl_ListObjAppendElement(Interp,obj,Tcl_NewDoubleObj(RAD2DEG(loc1.Lat)));
            Tcl_ListObjAppendElement(Interp,obj,Tcl_NewDoubleObj(RAD2DEG(loc1.Lon)));
         }
         Tcl_SetObjResult(Interp,obj);
         break;

      case DIST:
         if (Objc!=2 && Objc!=3){
            Tcl_WrongNumArgs(Interp,0,Objv,"{ coords } elev");
            return(TCL_ERROR);
         }
         if (!proj->Geographic) {
            Tcl_AppendResult(Interp,"Projection is not geographic",(char*)NULL);
            return(TCL_ERROR);
         }

         if (Objc==3) {
            Tcl_GetDoubleFromObj(Interp,Objv[2],&x);
         } else {
            x=0;
         }

         Tcl_ListObjLength(Interp,Objv[1],&nobj);

         n=0;d=0;
         if (nobj<4 || nobj%2!=0) {
            Tcl_SetObjResult(Interp,Tcl_NewDoubleObj(0.0));
         } else {
            Tcl_ListObjIndex(Interp,Objv[1],n++,&obj);
            Tcl_GetDoubleFromObj(Interp,obj,&loc0.Lat);
            Tcl_ListObjIndex(Interp,Objv[1],n++,&obj);
            Tcl_GetDoubleFromObj(Interp,obj,&loc0.Lon);
            
            while (n<nobj) {
               Tcl_ListObjIndex(Interp,Objv[1],n++,&obj);
               Tcl_GetDoubleFromObj(Interp,obj,&loc1.Lat);
               Tcl_ListObjIndex(Interp,Objv[1],n++,&obj);
               Tcl_GetDoubleFromObj(Interp,obj,&loc1.Lon);

               loc0.Lat=DEG2RAD(loc0.Lat);loc0.Lon=DEG2RAD(loc0.Lon);
               loct.Lat=DEG2RAD(loc1.Lat);loct.Lon=DEG2RAD(loc1.Lon);
               d+=DIST(x,loc0.Lat,loc0.Lon,loct.Lat,loct.Lon);

               loc0=loc1;
            }
            Tcl_SetObjResult(Interp,Tcl_NewDoubleObj(d));
         }
         break;

     case AREA:
         if (Objc!=2 && Objc!=3){
            Tcl_WrongNumArgs(Interp,0,Objv,"{ coords } elev");
            return(TCL_ERROR);
         }
         if (!proj->Geographic) {
            Tcl_AppendResult(Interp,"Projection is not geographic",(char*)NULL);
            return(TCL_ERROR);
         }

         if (Objc==3) {
            Tcl_GetDoubleFromObj(Interp,Objv[2],&x);
         } else {
            x=0;
         }

         Tcl_ListObjLength(Interp,Objv[1],&nobj);

         n=0;d=0;
         if (nobj<4 || nobj%2!=0) {
            Tcl_SetObjResult(Interp,Tcl_NewDoubleObj(0.0));
         } else {
            Tcl_ListObjIndex(Interp,Objv[1],n++,&obj);
            Tcl_GetDoubleFromObj(Interp,obj,&loc0.Lat);
            Tcl_ListObjIndex(Interp,Objv[1],n++,&obj);
            Tcl_GetDoubleFromObj(Interp,obj,&loc0.Lon);
            loct=loc0;
            while (n<nobj) {
               Tcl_ListObjIndex(Interp,Objv[1],n++,&obj);
               Tcl_GetDoubleFromObj(Interp,obj,&loc1.Lat);
               Tcl_ListObjIndex(Interp,Objv[1],n++,&obj);
               Tcl_GetDoubleFromObj(Interp,obj,&loc1.Lon);

               d+=DEG2RAD(loc1.Lon-loc0.Lon)*(2+sin(DEG2RAD(loc0.Lat))+sin(DEG2RAD(loc1.Lat)));
               loc0=loc1;
            }
            // Check for polygon closing
            if (loc0.Lat!=loct.Lat && loc0.Lon!=loct.Lon) {
               d+=DEG2RAD(loc0.Lon-loct.Lon)*(2+sin(DEG2RAD(loct.Lat))+sin(DEG2RAD(loc0.Lat)));
            }
            d*=(EARTHRADIUS+x)*(EARTHRADIUS+x)/2.0;
            Tcl_SetObjResult(Interp,Tcl_NewDoubleObj(fabs(d)));
         }
         break;
                
     case BEARING:
         if (Objc!=5){
            Tcl_WrongNumArgs(Interp,0,Objv,"lat0 lon0 lat1 lon1");
            return(TCL_ERROR);
         }
         if (!proj->Geographic) {
            Tcl_AppendResult(Interp,"Projection is not geographic",(char*)NULL);
            return(TCL_ERROR);
         }

         Tcl_GetDoubleFromObj(Interp,Objv[1],&loc0.Lat);
         Tcl_GetDoubleFromObj(Interp,Objv[2],&loc0.Lon);
         Tcl_GetDoubleFromObj(Interp,Objv[3],&loc1.Lat);
         Tcl_GetDoubleFromObj(Interp,Objv[4],&loc1.Lon);

         loc0.Lat=DEG2RAD(loc0.Lat);loc0.Lon=DEG2RAD(loc0.Lon);
         loc1.Lat=DEG2RAD(loc1.Lat);loc1.Lon=DEG2RAD(loc1.Lon);

         x=COURSE(loc0.Lat,loc0.Lon,loc1.Lat,loc1.Lon);
         Tcl_SetObjResult(Interp,Tcl_NewDoubleObj(RAD2DEG(x)));
         break;
   }

   return(TCL_OK);
}

/*----------------------------------------------------------------------------
 * Nom      : <Projection_Clip>
 * Creation : Juin 2003 - J.P. Gauthier - CMC/CMOE
 *
 * But      : Activer le clipping.
 *
 * Parametres :
 *  <Proj>    : Parametres de la projection
 *
 * Retour:
 *
 * Remarques :
 *
 *----------------------------------------------------------------------------
*/
void Projection_Clip(Projection *Proj) {

   if (Proj->Type->Def==PROJPLANE) {
      glEnable(GL_CLIP_PLANE0);
      glEnable(GL_CLIP_PLANE1);
      glEnable(GL_CLIP_PLANE2);
      glEnable(GL_CLIP_PLANE3);
   } else if (Proj->Type->Def==PROJCYLIN) {
      glEnable(GL_CLIP_PLANE2);
      glEnable(GL_CLIP_PLANE3);
   }
}

/*----------------------------------------------------------------------------
 * Nom      : <Projection_UnClip>
 * Creation : Juin 2003 - J.P. Gauthier - CMC/CMOE
 *
 * But      : Desactiver le clipping.
 *
 * Parametres :
 *  <Proj>    : Parametres de la projection
 *
 * Retour:
 *
 * Remarques :
 *
 *----------------------------------------------------------------------------
*/
void Projection_UnClip(Projection *Proj) {

   glDisable(GL_CLIP_PLANE0);
   glDisable(GL_CLIP_PLANE1);
   glDisable(GL_CLIP_PLANE2);
   glDisable(GL_CLIP_PLANE3);
}

/*----------------------------------------------------------------------------
 * Nom      : <Projection_Path>
 * Creation : Mars 2002 - J.P. Gauthier - CMC/CMOE
 *
 * But      : Calculer les positions le long d'un GC.
 *
 * Parametres    :
 *  <Interp>     : Interpreteur Tcl
 *  <List>       : Liste des coordonnees
 *  <Dist>       : Distance entre les point intermediaire (en metres)
 *
 * Retour        :
 *  <RCL_...>    : Code de retour Tcl
 *
 * Remarques :
 *
 *----------------------------------------------------------------------------
*/
Tcl_Obj *Projection_Path(Tcl_Interp *Interp,Tcl_Obj *List,double Dist){

   Tcl_Obj *obj,*objo;
   double   td,tc,d,id,lat,lon;
   double   slat0,clat0,clat1,sd,cd,st,ct;
   int      i=0,nobj;
   Coord    loc0,loc1,loct;

   Tcl_ListObjLength(Interp,List,&nobj);
   objo=Tcl_NewListObj(0,NULL);

   if (nobj<4 || nobj%2!=0) {
      return(objo);
   }

   /* Get the first path coordinate*/
   Tcl_ListObjIndex(Interp,List,i++,&obj);
   Tcl_GetDoubleFromObj(Interp,obj,&loc0.Lat);
   Tcl_ListObjIndex(Interp,List,i++,&obj);
   Tcl_GetDoubleFromObj(Interp,obj,&loc0.Lon);

   while (i<nobj) {

      /* Output start of path segment*/
      Tcl_ListObjAppendElement(Interp,objo,Tcl_NewDoubleObj(loc0.Lat));
      Tcl_ListObjAppendElement(Interp,objo,Tcl_NewDoubleObj(loc0.Lon));

      /* Parse all the path coordinates*/
      Tcl_ListObjIndex(Interp,List,i++,&obj);
      Tcl_GetDoubleFromObj(Interp,obj,&loc1.Lat);
      Tcl_ListObjIndex(Interp,List,i++,&obj);
      Tcl_GetDoubleFromObj(Interp,obj,&loc1.Lon);

      loc0.Lat=DEG2RAD(loc0.Lat);
      loc0.Lon=DEG2RAD(loc0.Lon);
      loct.Lat=DEG2RAD(loc1.Lat);
      loct.Lon=DEG2RAD(loc1.Lon);

      slat0=sin(loc0.Lat);
      clat0=cos(loc0.Lat);
      clat1=cos(loct.Lat);

      /* Figure out true course and distance*/
      td=2.0*asin(sqrt(pow((sin((loc0.Lat-loct.Lat)/2.0)),2.0)+clat0*clat1*pow((sin((loc0.Lon-loct.Lon)/2.0)),2.0)));
      tc=fmod(atan2(sin(loc0.Lon-loct.Lon)*clat1,clat0*sin(loct.Lat)-slat0*clat1*cos(loc0.Lon-loct.Lon)),M_2PI);

      st=sin(tc);
      ct=cos(tc);
      d=id=M_PI/(180.0*60.0)*(Dist/1852.0);

      /* Iterate on the true course at the specified step*/
      while (d<=td) {
         sd=sin(d);
         cd=cos(d);
         lat=asin(slat0*cd+clat0*sd*ct);
         lon=fmod(loc0.Lon-atan2(st*sd*clat0,cd-slat0*sin(lat))+M_PI,M_2PI)-M_PI;

         lon=RAD2DEG(lon);
         lat=RAD2DEG(lat);
         d+=id;

         Tcl_ListObjAppendElement(Interp,objo,Tcl_NewDoubleObj(lat));
         Tcl_ListObjAppendElement(Interp,objo,Tcl_NewDoubleObj(lon));
      }

      loc0=loc1;
   }

   /* Output las path coordinate */
   Tcl_ListObjAppendElement(Interp,objo,Tcl_NewDoubleObj(loc1.Lat));
   Tcl_ListObjAppendElement(Interp,objo,Tcl_NewDoubleObj(loc1.Lon));

   return(objo);
}

/*----------------------------------------------------------------------------
 * Nom      : <Projection_Clean>
 * Creation : Aout 2003 - J.P. Gauthier - CMC/CMOE
 *
 * But      : Reinitialiser les donnees apres une modification de la projection.
 *
 * Parametres :
 *  <Interp>  : Interpreteur Tcl
 *  <Proj>    : Projection
 *  <Mode>    : Qu'est ce que l'on clean
 *
 * Retour     :
 *
 * Remarques :
 *
 *----------------------------------------------------------------------------
 */
void Projection_Clean(Tcl_Interp *Interp,Projection *Proj,int Mode) {

   int        i;
   Tcl_Obj   *obj;
   GDAL_Band *band;
   OGR_Layer *layer;
   T3DModel  *mdl;

   GDB_TileFreeAll(Proj->Geo,Mode);
   if (Proj->VP && Proj->VP->Cam)
      Proj->VP->Cam->Update=1;

   for (i=0;i<Proj->NbData;i++) {
      Tcl_ListObjIndex(Interp,Proj->Data,i,&obj);

      band=GDAL_BandGet(Tcl_GetString(obj));
      if (band) {
         GDAL_BandClean(band,0,1,0);
      }

      layer=OGR_LayerGet(Tcl_GetString(obj));
      if (layer) {
         OGR_LayerClean(layer,-1);
      }

      mdl=Model_Get(Tcl_GetString(obj));
      if (mdl) {
         Model_Clean(mdl);
      }
   }
}

/*----------------------------------------------------------------------------
 * Nom      : <Projection_Config>
 * Creation : Janvier 1999 - J.P. Gauthier - CMC/CMOE
 *
 * But      : Configuration des parametres des projections.
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
 *----------------------------------------------------------------------------
*/
static int Projection_Config(Tcl_Interp *Interp,char *Name,int Objc,Tcl_Obj *CONST Objv[]){

   Tcl_Obj    *obj;
   TGeoRef    *ref;
   Projection *proj;
   int         i,tok,idx;
   double      lat,lon,ni,nj,tmp;

   static CONST char *sopt[] = { "-perspective","-location","-gridpoint","-gridsize","-gridextent","-mapres","-maptopo","-mapbath","-maptext","-mapcoast","-maplake","-mapriver","-mappolit",
                                 "-mapadmin","-mapcity","-maproad","-maprail","-mapplace","-mapcoord","-scale","-data","-license","-type","-georef","-geographic","-mask","-date","-late","-sun","-draw","-axiscoord","-axis","-minsize",NULL };
   enum                opt { PERSPECTIVE,LOCATION,GRIDPOINT,GRIDSIZE,GRIDEXTENT,MAPRES,MAPTOPO,MAPBATH,MAPTEXT,MAPCOAST,MAPLAKE,MAPRIVER,MAPPOLIT,
                             MAPADMIN,MAPCITY,MAPROAD,MAPRAIL,MAPPLACE,MAPCOORD,SCALE,DATA,LICENSE,TYPE,GEOREF,GEOGRAPHIC,MASK,DATE,LATE,SUN,DRAW,AXISCOORD,AXIS,MINSIZE };

   proj=Projection_Get(Name);
   if (!proj) {
      Tcl_AppendResult(Interp,"\n   Projection_Config: Projection name unknown: \"",Name,"\"",(char *)NULL);
      return(TCL_ERROR);
   }

  for(i=0;i<Objc;i++) {

      if (Tcl_GetIndexFromObj(Interp,Objv[i],sopt,"option",0,&idx)!=TCL_OK) {
         return(TCL_ERROR);
      }

      switch ((enum opt)idx) {

         case PERSPECTIVE:
            if (Objc==1) {
               Tcl_SetObjResult(Interp,Tcl_NewBooleanObj(proj->Perspective));
            } else {
               Tcl_GetBooleanFromObj(Interp,Objv[++i],&proj->Perspective);
            }
            break;

         case LOCATION:
            if (Objc==1) {
               obj=Tcl_NewListObj(0,NULL);
               Tcl_ListObjAppendElement(Interp,obj,Tcl_NewDoubleObj(proj->Lat));
               Tcl_ListObjAppendElement(Interp,obj,Tcl_NewDoubleObj(proj->Lon));
               Tcl_SetObjResult(Interp,obj);
            } else {
               Tcl_GetDoubleFromObj(Interp,Objv[++i],&lat);
               Tcl_GetDoubleFromObj(Interp,Objv[++i],&lon);

               if (proj->Geographic) {
                  if ((lat>-90.0 && lat<90.0) || (lon>-360.0 && lon<360.0)) {
                     proj->Lat=lat;
                     proj->Lon=lon;

                     if (proj->Type && proj->Type->Def==PROJSPHERE) {
                        ViewportClean(proj->VP,1,1);
                        Projection_Clean(Interp,proj,GDB_FORCE);
                     }
                     if (proj->Ref) {
                        proj->Ref->UnProject(proj->Ref,&ni,&nj,proj->Lat,proj->Lon,1,1);
                        proj->I=ni;
                        proj->J=nj;
                     }
                  }
               } else {
                  proj->I=proj->Lon=lon;
                  proj->J=proj->Lat=lat;
               }
               if (proj->VP) {
                  ViewportSetup(proj->VP->canvas,proj->VP,proj,Tk_Width(Tk_CanvasTkwin(proj->VP->canvas)),Tk_Height(Tk_CanvasTkwin(proj->VP->canvas)),0,1,0);
                  Projection_Setup(proj->VP,proj,0);
              }
            }
            break;

         case GRIDPOINT:
            if (Objc==1) {
               obj=Tcl_NewListObj(0,NULL);
               Tcl_ListObjAppendElement(Interp,obj,Tcl_NewDoubleObj(proj->I));
               Tcl_ListObjAppendElement(Interp,obj,Tcl_NewDoubleObj(proj->J));
               Tcl_SetObjResult(Interp,obj);
            } else {
               Tcl_GetDoubleFromObj(Interp,Objv[++i],&proj->I);
               Tcl_GetDoubleFromObj(Interp,Objv[++i],&proj->J);

               /*Enforce limits*/
               if (proj->Ref) {
                  proj->I=proj->I<proj->Ref->X0?proj->Ref->X0:(proj->I>proj->Ref->X1?proj->Ref->X1:proj->I);
                  proj->J=proj->J<proj->Ref->Y0?proj->Ref->Y0:(proj->J>proj->Ref->Y1?proj->Ref->Y1:proj->J);
               }

               if (proj->Ref && proj->Geographic) {
                  proj->Ref->Project(proj->Ref,proj->I,proj->J,&lat,&lon,0,1);
                  proj->Lat=lat;
                  proj->Lon=lon;
               } else {
                  proj->Lat=proj->J;
                  proj->Lon=proj->I;
               }
               if (proj->VP) {
                  ViewportSetup(proj->VP->canvas,proj->VP,proj,Tk_Width(Tk_CanvasTkwin(proj->VP->canvas)),Tk_Height(Tk_CanvasTkwin(proj->VP->canvas)),0,1,0);
                  Projection_Setup(proj->VP,proj,0);
               }
            }
            break;

        case GRIDSIZE:
            if (Objc==1) {
               obj=Tcl_NewListObj(0,NULL);
               if (proj->Ref) {
                  Tcl_ListObjAppendElement(Interp,obj,Tcl_NewIntObj(proj->Ref->X1-proj->Ref->X0));
                  Tcl_ListObjAppendElement(Interp,obj,Tcl_NewIntObj(proj->Ref->Y1-proj->Ref->Y0));
                } else {
                  Tcl_ListObjAppendElement(Interp,obj,Tcl_NewIntObj(0));
                  Tcl_ListObjAppendElement(Interp,obj,Tcl_NewIntObj(0));
               }
               Tcl_SetObjResult(Interp,obj);
            } else {
            }
            break;

        case GRIDEXTENT:
            if (Objc==1) {
               obj=Tcl_NewListObj(0,NULL);
               if (proj->Ref) {
                  Tcl_ListObjAppendElement(Interp,obj,Tcl_NewIntObj(proj->Ref->X0));
                  Tcl_ListObjAppendElement(Interp,obj,Tcl_NewIntObj(proj->Ref->Y0));
                  Tcl_ListObjAppendElement(Interp,obj,Tcl_NewIntObj(proj->Ref->X1));
                  Tcl_ListObjAppendElement(Interp,obj,Tcl_NewIntObj(proj->Ref->Y1));
                } else {
                  Tcl_ListObjAppendElement(Interp,obj,Tcl_NewIntObj(0));
                  Tcl_ListObjAppendElement(Interp,obj,Tcl_NewIntObj(0));
                  Tcl_ListObjAppendElement(Interp,obj,Tcl_NewIntObj(0));
                  Tcl_ListObjAppendElement(Interp,obj,Tcl_NewIntObj(0));
               }
               Tcl_SetObjResult(Interp,obj);
            } else {
            }
            break;

        case MAPRES:
            if (Objc==1) {
               Tcl_SetObjResult(Interp,Tcl_NewIntObj(proj->Geo->Res));
            } else {
               Tcl_GetIntFromObj(Interp,Objv[++i],&proj->Geo->Res);
            }
            break;

        case DATE:
            if (Objc==1) {
               Tcl_SetObjResult(Interp,Tcl_NewLongObj(proj->Date));
            } else {
               Tcl_GetLongFromObj(Interp,Objv[++i],&proj->Date);
            }
            break;

        case LATE:
            if (Objc==1) {
               Tcl_SetObjResult(Interp,Tcl_NewLongObj(proj->Late));
            } else {
               Tcl_GetLongFromObj(Interp,Objv[++i],&proj->Late);
            }
            break;

        case SUN:
           if (Objc==1) {
               Tcl_SetObjResult(Interp,Tcl_NewBooleanObj(proj->Sun));
            } else {
               Tcl_GetBooleanFromObj(Interp,Objv[++i],&proj->Sun);
            }
            break;

        case DRAW:
           if (Objc==1) {
               Tcl_SetObjResult(Interp,Tcl_NewBooleanObj(proj->Draw));
            } else {
               Tcl_GetBooleanFromObj(Interp,Objv[++i],&proj->Draw);
            }
            break;
            
        case MAPTOPO:
           if (Objc==1) {
               Tcl_SetObjResult(Interp,Tcl_NewIntObj(proj->Geo->Params.Topo));
            } else {
               Tcl_GetIntFromObj(Interp,Objv[++i],&proj->Geo->Params.Topo);
               if (proj->Geo->Params.Topo==0) {
                  GDB_TileFreeType(proj->Geo,GDB_TYPE_MAP);
               }
            }
            break;

        case MAPBATH:
            if (Objc==1) {
               Tcl_SetObjResult(Interp,Tcl_NewIntObj(proj->Geo->Params.Bath));
            } else {
               Tcl_GetIntFromObj(Interp,Objv[++i],&proj->Geo->Params.Bath);
               if (proj->Geo->Params.Bath==0) {
                  GDB_TileFreeType(proj->Geo,GDB_TYPE_MAP);
               }
            }
            break;

        case MAPTEXT:
            if (Objc==1) {
               Tcl_SetObjResult(Interp,Tcl_NewIntObj(proj->Geo->Params.Text));
            } else {
               Tcl_GetIntFromObj(Interp,Objv[++i],&proj->Geo->Params.Text);
               if (proj->Geo->Params.Text==0) {
                  GDB_TileFreeType(proj->Geo,GDB_TYPE_TEXT);
               }
            }
            break;

        case MAPCOAST:
            if (Objc==1) {
               Tcl_SetObjResult(Interp,Tcl_NewIntObj(proj->Geo->Params.Coast));
            } else {
               Tcl_GetIntFromObj(Interp,Objv[++i],&tok);
               if (tok==0 || (tok<0 && proj->Geo->Params.Coast>0) || (proj->Geo->Params.Coast<0 && tok>0))
                  GDB_TileFreeType(proj->Geo,GDB_TYPE_COAST);
               proj->Geo->Params.Coast=tok;
            }
            break;

        case MAPLAKE:
            if (Objc==1) {
               Tcl_SetObjResult(Interp,Tcl_NewIntObj(proj->Geo->Params.Lake));
            } else {
               Tcl_GetIntFromObj(Interp,Objv[++i],&tok);
               if (tok==0 || (tok<0 && proj->Geo->Params.Lake>0) || (proj->Geo->Params.Lake<0 && tok>0))
                  GDB_TileFreeType(proj->Geo,GDB_TYPE_LAKE);
               proj->Geo->Params.Lake=tok;
            }
            break;

        case MAPRIVER:
            if (Objc==1) {
               Tcl_SetObjResult(Interp,Tcl_NewIntObj(proj->Geo->Params.River));
            } else {
               Tcl_GetIntFromObj(Interp,Objv[++i],&proj->Geo->Params.River);
               if (proj->Geo->Params.River==0)
                  GDB_TileFreeType(proj->Geo,GDB_TYPE_RIVER);
            }
            break;

        case MAPPOLIT:
            if (Objc==1) {
               Tcl_SetObjResult(Interp,Tcl_NewIntObj(proj->Geo->Params.Polit));
            } else {
               Tcl_GetIntFromObj(Interp,Objv[++i],&proj->Geo->Params.Polit);
               if (proj->Geo->Params.Polit==0)
                  GDB_TileFreeType(proj->Geo,GDB_TYPE_POLIT);
            }
            break;

        case MAPADMIN:
            if (Objc==1) {
               Tcl_SetObjResult(Interp,Tcl_NewIntObj(proj->Geo->Params.Admin));
            } else {
               Tcl_GetIntFromObj(Interp,Objv[++i],&proj->Geo->Params.Admin);
               if (proj->Geo->Params.Admin==0)
                  GDB_TileFreeType(proj->Geo,GDB_TYPE_ADMIN);
            }
            break;

        case MAPCITY:
            if (Objc==1) {
               Tcl_SetObjResult(Interp,Tcl_NewIntObj(proj->Geo->Params.City));
            } else {
               Tcl_GetIntFromObj(Interp,Objv[++i],&proj->Geo->Params.City);
               if (proj->Geo->Params.City==0)
                  GDB_TileFreeType(proj->Geo,GDB_TYPE_CITY);
            }
            break;

        case MAPROAD:
            if (Objc==1) {
               Tcl_SetObjResult(Interp,Tcl_NewIntObj(proj->Geo->Params.Road));
            } else {
               Tcl_GetIntFromObj(Interp,Objv[++i],&proj->Geo->Params.Road);
               if (proj->Geo->Params.Road==0)
                  GDB_TileFreeType(proj->Geo,GDB_TYPE_ROAD);
            }
            break;

        case MAPRAIL:
            if (Objc==1) {
               Tcl_SetObjResult(Interp,Tcl_NewIntObj(proj->Geo->Params.Rail));
            } else {
               Tcl_GetIntFromObj(Interp,Objv[++i],&proj->Geo->Params.Rail);
               if (proj->Geo->Params.Rail==0)
                  GDB_TileFreeType(proj->Geo,GDB_TYPE_RAIL);
            }
            break;

        case MAPPLACE:
            if (Objc==1) {
               Tcl_SetObjResult(Interp,Tcl_NewIntObj(proj->Geo->Params.Place));
            } else {
               Tcl_GetIntFromObj(Interp,Objv[++i],&proj->Geo->Params.Place);
               if (proj->Geo->Params.Place==0)
                  GDB_TileFreeType(proj->Geo,GDB_TYPE_PLACE);
            }
            break;

        case MAPCOORD:
            if (Objc==1) {
               obj=Tcl_NewListObj(0,NULL);
               Tcl_ListObjAppendElement(Interp,obj,Tcl_NewIntObj(proj->Geo->Params.CoordLoc));
               Tcl_ListObjAppendElement(Interp,obj,Tcl_NewDoubleObj(proj->Geo->Params.CoordDef));
               Tcl_ListObjAppendElement(Interp,obj,Tcl_NewIntObj(proj->Geo->Params.CoordNum));
               Tcl_SetObjResult(Interp,obj);
            } else {
               Tcl_GetIntFromObj(Interp,Objv[++i],&proj->Geo->Params.CoordLoc);
               Tcl_GetDoubleFromObj(Interp,Objv[++i],&proj->Geo->Params.CoordDef);
               Tcl_GetIntFromObj(Interp,Objv[++i],&proj->Geo->Params.CoordNum);
           }
            break;

        case SCALE:
            if (Objc==1) {
               Tcl_SetObjResult(Interp,Tcl_NewDoubleObj(proj->Scale));
            } else {
               Tcl_GetDoubleFromObj(Interp,Objv[++i],&tmp);
               if (tmp!=proj->Scale) {
                  proj->Scale=tmp;
                  ViewportClean(proj->VP,1,1);
                  Projection_Clean(Interp,proj,GDB_RASTER);
               }
            }
            break;

        case AXISCOORD:
            if (Objc==1) {
               obj=Tcl_NewListObj(0,NULL);
               Tcl_ListObjAppendElement(Interp,obj,Tcl_NewDoubleObj(proj->ZAxis.Lat));
               Tcl_ListObjAppendElement(Interp,obj,Tcl_NewDoubleObj(proj->ZAxis.Lon));
               Tcl_ListObjAppendElement(Interp,obj,Tcl_NewDoubleObj(proj->ZAxis.Elev));
               Tcl_SetObjResult(Interp,obj);
            } else {
               Tcl_GetDoubleFromObj(Interp,Objv[++i],&proj->ZAxis.Lat);
               Tcl_GetDoubleFromObj(Interp,Objv[++i],&proj->ZAxis.Lon);
               Tcl_GetDoubleFromObj(Interp,Objv[++i],&proj->ZAxis.Elev);
            }
            break;

        case AXIS:
            if (Objc==1) {
               Tcl_SetObjResult(Interp,Tcl_NewIntObj(proj->TAxis));
            } else {
               Tcl_GetIntFromObj(Interp,Objv[++i],&proj->TAxis);
            }
            break;

        case MASK:
            if (Objc==1) {
               switch(proj->Geo->Params.Mask) {
                  case -1: Tcl_SetObjResult(Interp,Tcl_NewStringObj("DATA",-1));break;
                  case  0: Tcl_SetObjResult(Interp,Tcl_NewStringObj("NONE",-1));break;
                  case  1: Tcl_SetObjResult(Interp,Tcl_NewStringObj("LAND",-1));break;
                  case  2: Tcl_SetObjResult(Interp,Tcl_NewStringObj("SEA",-1));break;
               }
            } else {
               ++i;
               if (strcmp(Tcl_GetString(Objv[i]),"NONE")==0) {
                  proj->Geo->Params.Mask=0;
               } else if (strcmp(Tcl_GetString(Objv[i]),"DATA")==0) {
                  proj->Geo->Params.Mask=-1;
               } else if (strcmp(Tcl_GetString(Objv[i]),"LAND")==0) {
                  proj->Geo->Params.Mask=1;
               } else if (strcmp(Tcl_GetString(Objv[i]),"SEA")==0) {
                  proj->Geo->Params.Mask=2;
               } else if (Tcl_GetIntFromObj(Interp,Objv[i],&proj->Geo->Params.Mask)==TCL_ERROR) {
                  proj->Geo->Params.Mask=0;
               }
            }
            break;

        case DATA:
            if (Objc==1) {
               Tcl_SetObjResult(Interp,proj->Data);
            } else {
               if (proj->Data) {
                  Tcl_DecrRefCount(proj->Data);
               }
               proj->Data=Objv[++i];
               Tcl_IncrRefCount(proj->Data);
               Tcl_ListObjLength(Interp,proj->Data,&proj->NbData);
            }
            break;

        case LICENSE:
            if (Objc==1) {
               Tcl_SetObjResult(Interp,Tcl_NewStringObj(proj->License,-1));
            } else {
               if (proj->License) {
                  free(proj->License);
               }
               proj->License=strdup(Tcl_GetString(Objv[++i]));
            }
            break;

       case GEOREF:
            if (Objc==1) {
               if (proj->Ref)
                  Tcl_SetObjResult(Interp,Tcl_NewStringObj(proj->Ref->Name,-1));
            } else {
               if (!strlen(Tcl_GetString(Objv[++i]))) {
                  ref=NULL;
               } else if (!(ref=GeoRef_Get(Tcl_GetString(Objv[i])))) {
                  Tcl_AppendResult(Interp,"\n   Projection_Config: Georeference unknown (",Tcl_GetString(Objv[i]),")",(char*)NULL);
                  return(TCL_ERROR);
               }
               if (ref!=proj->Ref) {
                  if (proj->Ref)
                     GeoRef_Destroy(Interp,proj->Ref->Name);
                  proj->Ref=ref;
                  GeoRef_Incr(proj->Ref);
                  ViewportClean(proj->VP,1,1);
                  Projection_Clean(Interp,proj,GDB_FORCE);
               }
               if (strcmp(proj->Type->Name,"grid")==0) {
                  Grid_Setup(Interp,proj);
               }
            }
            break;

       case GEOGRAPHIC:
            if (Objc==1) {
               Tcl_SetObjResult(Interp,Tcl_NewBooleanObj(proj->Geographic));
            } else {
               Tcl_GetBooleanFromObj(Interp,Objv[++i],&proj->Geographic);
               if (strcmp(proj->Type->Name,"grid")==0) {
                  Grid_Setup(Interp,proj);
               }
               ViewportClean(proj->VP,1,1);
               Projection_Clean(Interp,proj,GDB_FORCE);
            }
            break;

       case TYPE:
            if (Objc==1) {
               Tcl_SetObjResult(Interp,Tcl_NewStringObj(proj->Type->Name,-1));
            } else {
               struct ProjectionType *type=Projection_GetType(Tcl_GetString(Objv[++i]));
               if (!type) {
                  Tcl_AppendResult(Interp,"\n   Projection_Config: Projection type unknown (",Tcl_GetString(Objv[i]),")",(char*)NULL);
                  return(TCL_ERROR);
               } else {
                  if (type!=proj->Type) {
                     proj->Type=type;
                     proj->L=proj->LI=proj->LJ=1.0;
                     proj->Geographic=1;
                     if (strcmp(Tcl_GetString(Objv[i]),"grid")==0) {
                        Grid_Setup(Interp,proj);
                      }
                     ViewportClean(proj->VP,1,1);
                     Projection_Clean(Interp,proj,GDB_FORCE);
                  }
               }
            }
            break;

       case MINSIZE:
            if (Objc==1) {
               Tcl_SetObjResult(Interp,Tcl_NewIntObj(proj->MinSize));
            } else {
               Tcl_GetIntFromObj(Interp,Objv[++i],&proj->MinSize);
               proj->MinSize=proj->MinSize<0?0:proj->MinSize;
            }
            break;

      }
   }
   return(TCL_OK);
}

/*----------------------------------------------------------------------------
 * Nom      : <Projection_Create>
 * Creation : Aout 1998 - J.P. Gauthier
 *
 * But      : Creation d'un object projection.
 *
 * Parametres    :
 *  <Interp>     : Interpreteur TCL
 *  <Name>       : Nom de la projection
 *
 * Retour:
 *  <TCL_...>    : Code de retour standard TCL
 *
 * Remarques :
 *
 *----------------------------------------------------------------------------
*/
static int Projection_Create(Tcl_Interp *Interp,char *Name){

   Projection *proj;

   if (!(proj=(Projection*)TclY_HashPut(Interp,&ProjectionTable,Name,sizeof(Projection)))) {
      return(TCL_ERROR);
   }

   /*Initialiaser les valeurs des parametres*/

   proj->PixDist     = 0.0;
   proj->Type        = NULL;
   proj->Data        = NULL;
   proj->License     = NULL;
   proj->NbData      = 0;
   proj->Date        = 0;
   proj->Late        = 60;
   proj->Sun         = 0;
   proj->Draw        = 1;
   proj->Loading     = 0;
   proj->MinSize     = 5;
   proj->Perspective  = 0;

   /*Parametres commun*/
   proj->Scale        = 1;
   proj->ZFactor      = 1.0;
   proj->TAxis        = 1;
   proj->ZAxis.Lat    = 0.0;
   proj->ZAxis.Lon    = 0.0;
   proj->ZAxis.Elev   = 0.0;
   proj->Lat          = 0.0;
   proj->Lon          = 0.0;
   proj->VP           = NULL;

   /*Parametres de projections sur grille*/
   proj->Ref          = NULL;
   proj->Geographic   = 1;
   proj->I            = 0.0;
   proj->J            = 0.0;
   proj->L            = 0.0;
   proj->LI           = 1.0;
   proj->LJ           = 1.0;

   /*Initialisation de la base de donnees geographique*/
   proj->Geo=(GDB_Data*)malloc(sizeof(GDB_Data));
   GDB_Init(proj->Geo);

   return(TCL_OK);
}

/*----------------------------------------------------------------------------
 * Nom      : <Projection_CreateType>
 * Creation : Aout 1998 - J.P. Gauthier
 *
 * But      : Ajout d'un nouveau type de vue specifie de Tcl.
 *
 * Parametres    :
 *  <Name>       : Nom de la projection
 *  <Draw>       : Pointeur sur la fonction de d'affichage de la projection
 *  <Config>     : Pointeur sur la fonction de configuration
 *
 * Retour        :
 *  <TCL_...>    : Code de retour standard TCL
 *
 * Remarques :
 *
 *----------------------------------------------------------------------------
*/
int Projection_CreateType(Tcl_Interp *Interp,char *Name,int Def,
   Projection_CallLocate *Locate,Projection_CallRender *Render,Projection_CallDraw *DrawFirst,
   Projection_CallDraw *DrawLast,Projection_CallDraw *DrawGlobe,Projection_CallUnProject *UnProject,Projection_CallProject *Project,
   Projection_CallProjectPoint *ProjectPoint,Projection_CallProjectLine *ProjectLine) {

   ProjectionType *project_type;

   if (!(project_type=(ProjectionType*)TclY_HashPut(NULL,&ProjectionTypes,Name,sizeof(ProjectionType)))) {
      Tcl_AppendResult(Interp,"Projection_CreateType: Unable ot create new Projection type ",Name,(char*)NULL);
      return(TCL_ERROR);
   }

   project_type->Name         = strdup(Name);
   project_type->Def          = Def;
   project_type->Locate       = Locate;
   project_type->Render       = Render;
   project_type->DrawFirst    = DrawFirst;
   project_type->DrawLast     = DrawLast;
   project_type->DrawGlobe    = DrawGlobe;
   project_type->UnProject    = UnProject;
   project_type->Project      = Project;
   project_type->ProjectPoint = ProjectPoint;
   project_type->ProjectLine  = ProjectLine;

   return(TCL_OK);
}

/*----------------------------------------------------------------------------
 * Nom      : <Projection_Destroy>
 * Creation : Janvier 2000 - J.P. Gauthier - CMC/CMOE
 *
 * But      : Procedure de destruction d'un projection.
 *
 * Parametres :
 *  <Interp>  : Interpreteur Tcl
 *  <Name>    : Nom de la projection
 *
 * Retour     :
 *  <TCL_...> : Code de retour standard TCL
 *
 * Remarques :
 *
 *----------------------------------------------------------------------------
*/
static int Projection_Destroy(Tcl_Interp *Interp, char *Name) {

   Projection *proj;

   if ((proj=(Projection*)TclY_HashDel(&ProjectionTable,Name))) {
      if (proj->Ref)
         GeoRef_Destroy(Interp,proj->Ref->Name);

      if (proj->Geo) {
         Projection_Clean(Interp,proj,GDB_FORCE);
         GDB_Clean(proj->Geo);
         free(proj->Geo);
      }

      if (proj->Data) {
         Tcl_DecrRefCount(proj->Data);
      }

      if (proj->License) {
         free(proj->License);
      }
      free(proj);
   }
   return(TCL_OK);
}

/*----------------------------------------------------------------------------
 * Nom      : <Projection_Get>
 * Creation : Janvier 1999 - J.P. Gauthier - CMC/CMOE
 *
 * But      : Permet de recuperer l'adresse de la projections selon son nom.
 *
 * Parametres    :
 *  <Name>       : Nom de la projection
 *
 * Retour        :
 *  <Projection> : Pointeur sur les donnees de la projection
 *
 * Remarques :
 *
 *----------------------------------------------------------------------------
*/
Projection* Projection_Get(char *Name) {
   return((Projection*)TclY_HashGet(&ProjectionTable,Name));
}

/*----------------------------------------------------------------------------
 * Nom      : <Projection_GetType>
 * Creation : Fevrier 2000 - J.P. Gauthier - CMC/CMOE
 *
 * But      : Permet de recuperer l'adresse des parametres du type de projection.
 *
 * Parametres    :
 *  <Type>       : Nom de la projection
 *
 * Retour        :
 *  <Projection> : Pointeur sur les donnees de la projection
 *
 * Remarques :
 *
 *----------------------------------------------------------------------------
*/
ProjectionType* Projection_GetType(char *Type) {
   return((ProjectionType*)TclY_HashGet(&ProjectionTypes,Type));
}

/*----------------------------------------------------------------------------
 * Nom      : <Projection_Init>
 * Creation : Aout 1998 - J.P. Gauthier - CMC/CMOE
 *
 * But      : Initialisation des tables et commandes du package de projection.
 *
 * Parametres  :
 *  <Interp>   : Interpreteur Tcl
 *
 * Retour      :
 *  <TCL_...>  : Code de retour TCL
 *
 * Remarques :
 *
 *----------------------------------------------------------------------------
*/
int Projection_Init(Tcl_Interp *Interp){

   extern int Cylin_Init();
   extern int Ortho_Init();
   extern int Grid_Init();
   extern int Azimuth_Init();

   if (!ProjectionInit++) {
      Tcl_InitHashTable(&ProjectionTable,TCL_STRING_KEYS);
      Tcl_InitHashTable(&ProjectionTypes,TCL_STRING_KEYS);

      /*Initialisation du package de projection cylindrique*/
      if (Azimuth_Init(Interp)==TCL_ERROR)
         return(TCL_ERROR);

      /*Initialisation du package de projection cylindrique*/
      if (Cylin_Init(Interp)==TCL_ERROR)
         return(TCL_ERROR);

      /*Initialisation du package de projection orthographique*/
      if (Ortho_Init(Interp)==TCL_ERROR)
         return(TCL_ERROR);

      /*Initialisation du package de projection usager*/
      if (Grid_Init(Interp)==TCL_ERROR)
         return(TCL_ERROR);
   }

   VBuffer_Init();

   Tcl_CreateObjCommand(Interp,"projection",Projection_Cmd,(ClientData)NULL,(Tcl_CmdDeleteProc*)NULL);

   return(TCL_OK);
}

/*----------------------------------------------------------------------------
 * Nom      : <Projection_Map>
 * Creation : Aout 2003 - J.P. Gauthier - CMC/CMOE
 *
 * But      : Segmenter une liste de coordonnees selon le type de segmentation.
 *
 * Parametres  :
 *  <Interp>   : Interpreteur Tcl
 *  <Pos>      : Tableaux de coordonnees resultantes
 *  <Type>     : Type de segmentation (NONE COORD TRUE)
 *  <Liot>     : Objet Liste
 *
 * Retour      :
 *  <Nb>       : Nombre de coordonnees
 *
 * Remarques :
 *
 *----------------------------------------------------------------------------
*/
int Projection_Map(Tcl_Interp *Interp,Coord *Pos,char Type,Tcl_Obj *List) {

   int    i=0,j,nb=0,split;
   Coord  pt,p0;
   double ilat,ilon,ielev;
   double lat0,lon0,lat1,lon1;
   double sinlat0,sinlat1,coslat0,coslat1;
   double dn,tc,sindn,cosdn;

   int      nobj;
   Tcl_Obj *obj;

   Tcl_ListObjLength(Interp,List,&nobj);


   if (nobj%3!=0 || !nobj) {
      Tcl_AppendResult(Interp,"Projection_MapLine: Invalid number of coordinates (lat lon elev ... )",(char*)NULL);
      return TCL_ERROR;
   }

   Tcl_ListObjIndex(Interp,List,i++,&obj);
   Tcl_GetDoubleFromObj(Interp,obj,&Pos[nb].Lat);

   Tcl_ListObjIndex(Interp,List,i++,&obj);
   Tcl_GetDoubleFromObj(Interp,obj,&Pos[nb].Lon);

   Tcl_ListObjIndex(Interp,List,i++,&obj);
   Tcl_GetDoubleFromObj(Interp,obj,&Pos[nb].Elev);
   nb++;

   while(i<nobj){

      Tcl_ListObjIndex(Interp,List,i++,&obj);
      Tcl_GetDoubleFromObj(Interp,obj,&pt.Lat);

      Tcl_ListObjIndex(Interp,List,i++,&obj);
      Tcl_GetDoubleFromObj(Interp,obj,&pt.Lon);

      Tcl_ListObjIndex(Interp,List,i++,&obj);
      Tcl_GetDoubleFromObj(Interp,obj,&pt.Elev);

      switch(Type) {

         case 'N' :
            Pos[nb++]=pt;
            break;

         case 'C' :
            /*Define increments*/
            ilat =pt.Lat -Pos[nb-1].Lat;
            ilon =pt.Lon -Pos[nb-1].Lon;
            ielev=pt.Elev-Pos[nb-1].Elev;

            /*If we went from 180.0 ou -180.0*/
            ilon=ilon>180.0f?-(360.0f-ilon):ilon<-180.0f?360.0f+ilon:ilon;

            /*If segment too long*/
            split=(int)floor(MAX(fabs(ilat),fabs(ilon))/5.0f);
            if (split) {
               ilat/=split;
               ilon/=split;
               ielev/=split;
               pt=Pos[nb-1];

               /*Partition and increment segment*/
               while(split--){
                  pt.Lat+=ilat;
                  pt.Lon+=ilon;
                  pt.Elev+=ielev;
                  Pos[nb++]=pt;
               }
            } else {
               Pos[nb++]=pt;
            }
            break;

         case 'T' :
            lat0=DEG2RAD(Pos[nb-1].Lat);
            lon0=DEG2RAD(Pos[nb-1].Lon);
            lat1=DEG2RAD(pt.Lat);
            lon1=DEG2RAD(pt.Lon);
            ielev=pt.Elev-Pos[nb-1].Elev;

            sinlat0=sin(lat0);
            sinlat1=sin(lat1);
            coslat0=cos(lat0);
            coslat1=cos(lat1);

            /*Distance between points*/
            dn=acos(sinlat0*sinlat1+cos(lon0-lon1)*coslat0*coslat1);
            split=(int)floor(dn/(M_PI/36.0));
            if (split) {

               /*True course along which to iterate*/
               tc=fmod(atan2(sin(lon0-lon1)*coslat1,coslat0*sinlat1-sinlat0*coslat1*cos(lon0-lon1)),M_2PI);
               dn/=split;
               ielev/=split;
               p0.Elev=Pos[nb-1].Elev;

               /*Partition and increment segment*/
               for(j=1;j<split;j++) {
                  sindn=sin(dn*j);
                  cosdn=cos(dn*j);
                  p0.Lat=asin(sinlat0*cosdn+coslat0*sindn*cos(tc));
                  p0.Lon=fmod(lon0-atan2(sin(tc)*sindn*coslat0,cosdn-sinlat0*sin(p0.Lat))+M_PI,M_2PI)-M_PI;
                  p0.Lat=RAD2DEG(p0.Lat);
                  p0.Lon=RAD2DEG(p0.Lon);
                  p0.Elev+=ielev;
                  Pos[nb++]=p0;
               }
            }
            Pos[nb++]=pt;
            break;

         default:
            Tcl_AppendResult(Interp,"Projection_MapLine: Invalid mapping type must be [ NONE COORD TRUE ]",(char*)NULL);
            return TCL_ERROR;
      }
   }
   return(nb);
}

/*----------------------------------------------------------------------------
 * Nom      : <Projection_Pixel>
 * Creation : Aout 2004 - J.P. Gauthier - CMC/CMOE
 *
 * But      : Cette procedure effectue la projection d'une latlon en pixel.
 *
 * Parametres  :
 *  <Proj>     : Pointeur sur la projection
 *  <VP>       : Pointeur sur le viewport
 *  <Coord>    : Coordonne
 *  <Pix>      : Pixel
 *
 * Retour      :
 *  <int>      : Visibilite
 *
 * Remarques :
 *
 *----------------------------------------------------------------------------
*/
int Projection_Pixel(Projection *Proj,ViewportItem *VP,Coord Co,Vect3d Pix) {

   double d;
   Vect3d vr;

   Proj->Type->Project(Proj,(GeoVect*)&Co,(GeoVect*)&vr,1);
   if (Proj->Type->Def==PROJCYLIN) {
      d=vr[0]-Proj->L;
      CYLCHECK(d,vr[0]);
   }
   gluProject(vr[0],vr[1],vr[2],VP->GLModR,VP->GLProj,VP->GLView,&Pix[0],&Pix[1],&Pix[2]);

   if (VIN(Pix[0],1,VP->Width) && VIN(Pix[1],1,VP->Height) && VIN(Pix[2],0,1)) {
      return(1);
   } else {
      return(0);
   }
}

/*----------------------------------------------------------------------------
 * Nom      : <Projection_Render>
 * Creation : Janvier 2000 - J.P. Gauthier - CMC/CMOE
 *
 * But      : Effectue l'affichage de la projection.
 *
 * Parametres :
 *  <Interp>  : Interpreteur Tcl
 *  <VP>      : Parametres du viewport
 *  <Proj>    : Parametres de la projection
 *  <Mode>    : Type de donnees (GL_ALL,GL_RASTER,GL_VECTOR)
 *
 * Retour:
 *
 * Remarques :
 *
 *----------------------------------------------------------------------------
*/
int Projection_Render(Tcl_Interp *Interp,ViewportItem *VP,Projection *Proj,int Mode) {

   Tcl_Obj   *obj;
   T3DModel  *mdl;
   GDAL_Band *band;
   OGR_Layer *layer;
   int        ras=0,i;

   // If it's a grid projection without a referential or a global U grid
   if (Proj->Type->Def==PROJPLANE && !Proj->Ref) {
      return(0);
   }

   glDisable(GL_STENCIL_TEST);
   glDisable(GL_DEPTH_TEST);
   glDepthFunc(GL_LEQUAL);
   glDepthMask(GL_TRUE);
   glStencilMask(0xdf);

   if (Mode==GL_ALL || Mode==GL_VECTOR) {

      if (Proj->Draw) {
         
         /*Initialiser le mask des donnees vectorielles*/
         glEnable(GL_STENCIL_TEST);
         glStencilFunc(GL_ALWAYS,0x01,0x01);
         glStencilOp(GL_REPLACE,GL_REPLACE,GL_REPLACE);

         /*Initialiser l'impression de la geographie*/
         if (Interp) {
            Tcl_AppendResult(Interp,"\n% Postscript de la geographie\n",(char*)NULL);
         }

         /*Afficher les parties dependantes de la projection*/
         if (!Interp && Proj->Type->DrawGlobe && !Proj->Geo->Params.Bath)
            Proj->Type->DrawGlobe(Interp,VP,Proj);

         if (Proj->Geographic) {
   //      GDB_StarRender(Interp,Proj);

            if (Proj->Type->DrawFirst) {
               glDisable(GL_LIGHTING);
               glDisable(GL_LIGHT0);
               Proj->Type->DrawFirst(Interp,VP,Proj);

               if (Proj->Sun) {
                  glEnable(GL_LIGHTING);
                  glEnable(GL_LIGHT0);
               }
            }
            if (Interp) {
               GDB_TileRender(Interp,Proj,Proj->Geo,GDB_ALL^GDB_RASTER^GDB_MASK^GDB_FILL);
            } else {
               GDB_TileRender(Interp,Proj,Proj->Geo,GDB_ALL^GDB_RASTER^GDB_MASK);
            }
         }
         glStencilFunc(GL_EQUAL,0x00,0x0f);
         glStencilOp(GL_KEEP,GL_KEEP,GL_KEEP);
      }
   }

   if (Mode==GL_ALL || Mode==GL_RASTER) {

      /*Display geography filling for raster background*/
      if (Proj->Draw && Proj->Geographic) {
         if (Interp) {
            if (VP->ColorFLake && VP->ColorFCoast && !Proj->Geo->Params.Bath)
               Proj->Type->DrawGlobe(NULL,Proj->VP,Proj);
            GDB_TileRender(NULL,Proj,Proj->Geo,GDB_FILL);
         }
         ras+=GDB_TileRender(NULL,Proj,Proj->Geo,GDB_MASK | GDB_RASTER);

         if (ras && !GLRender->GLZBuf) {
            glClear(GL_DEPTH_BUFFER_BIT);
         }
      }

      for (i=0;i<Proj->NbData;i++) {
         Tcl_ListObjIndex(Interp,Proj->Data,i,&obj);

         if ((layer=OGR_LayerGet(Tcl_GetString(obj)))) {
            ras+=OGR_LayerRender(NULL,Proj,VP,layer,0);
         }
         if ((band=GDAL_BandGet(Tcl_GetString(obj)))) {
            ras+=GDAL_BandRender(Proj,VP,band);
         }
         if ((mdl=Model_Get(Tcl_GetString(obj)))) {
            ras+=Model_Render(Proj,VP,mdl);
         }
      }

      if (!GLRender->GLZBuf)
         glClear(GL_DEPTH_BUFFER_BIT);

      if (Proj->Draw && Proj->Type->DrawLast) {
         Proj->Type->DrawLast(Interp,VP,Proj);
      }
   }
   return(ras);
}

/*----------------------------------------------------------------------------
 * Nom      : <Projection_Setup>
 * Creation : Janvier 2002 - J.P. Gauthier - CMC/CMOE
 *
 * But      : Initialiser les parametres de projection.
 *
 * Parametres :
 *  <VP>      : Parametres du viewport
 *  <Proj>    : Parametres de la projection
 *  <GL>      : Setup du rendue ???
 *
 * Retour:
 *
 * Remarques :
 *
 *----------------------------------------------------------------------------
*/
void Projection_Setup(ViewportItem *VP,Projection *Proj,int GL){

   Vect3d   pt;
//   GLdouble pc[3];
   GLdouble p0[4]={  0.0, -1.0, 0.0, 1.0001 };
   GLdouble p1[4]={  0.0,  1.0, 0.0, 1.0001 };
   GLdouble p2[4]={ -1.0,  0.0, 0.0, 1.0001 };
   GLdouble p3[4]={  1.0,  0.0, 0.0, 1.0001 };

   GLfloat ref[2][4]  = { { 1.0, 1.0, 1.0, 1.0 },{ 1.0, 1.0, 1.0, 1.0 } };
   GLfloat amb[2][4]  = { { 0.5, 0.5, 0.5, 1.0 },{ 0.1, 0.1, 0.1, 1.0 } };
   GLfloat spec[2][4] = { { 0.6, 0.6, 0.6, 1.0 },{ 0.6, 0.6, 0.6, 1.0 } };
   GLfloat diff[2][4] = { { 1.0, 1.0, 1.0, 1.0 },{ 1.5, 1.5, 1.5, 1.0 } };

   double d;
   long   sec;
   Coord  co[2];

   Proj->VP=VP;

   if (Proj->Type->Def==PROJPLANE && !Proj->Ref) {
      return;
   }

   /*Rotation sur les axes lat-lon*/
   Proj->Type->Locate(Proj,Proj->Lat,Proj->Lon,0);

   /*Parametres de position*/
   Proj->SLat=sin(DEG2RAD(Proj->Lat));
   Proj->CLat=cos(DEG2RAD(Proj->Lat));

   /*Sauvegarde de la matrice de rotation*/
   glGetDoublev(GL_MODELVIEW_MATRIX,VP->GLModR);

   /*Distance en metres d'un pixel*/
   if (VP && !GLRender->TRCon) {
      if (Proj->Geographic) {
         pt[0]=(VP->Width*0.5)-0.5;
         pt[1]=VP->Height*0.5;
         pt[2]=0;
         Proj->Type->UnProject(VP,Proj,&co[0],pt);

         pt[0]=(VP->Width*0.5)+0.5;
         pt[1]=VP->Height*0.5;
         pt[2]=0;
         Proj->Type->UnProject(VP,Proj,&co[1],pt);

         co[0].Lat=DEG2RAD(co[0].Lat);
         co[0].Lon=DEG2RAD(co[0].Lon);
         co[1].Lat=DEG2RAD(co[1].Lat);
         co[1].Lon=DEG2RAD(co[1].Lon);
         d=DIST(0.0,co[0].Lat,co[0].Lon,co[1].Lat,co[1].Lon);
      } else {
         d=VP->Cam->Aspect;
      }
      if (d>0.0) {
         Proj->PixDist=d;
         if (VP->Cam->Update) {
            VP->Cam->Pix=Proj->PixDist;
            VP->Cam->Update=0;
         }
      }
   }

   /*Calcul du facteur en Z*/
   if (Proj->Type->Def==PROJPLANE) {
      if (Proj->Ref->Spatial && OSRIsProjected(Proj->Ref->Spatial)) {
         Proj->ZFactor=1.0/((Proj->L-1.0)*0.5);
      } else {
         Proj->ZFactor=1.0/((Proj->L-1.0)*2000.0);
      }
   } else {
      Proj->ZFactor=1.0/EARTHRADIUS;
   }
   /*Calcul de la position centrale*/
   if (Proj->Type->Def==PROJGLOBE) {
      gluProject(0.0,0.0,0.0,VP->GLModS,VP->GLProj,VP->GLView,&Proj->ZPos[0],&Proj->ZPos[1],&Proj->ZPos[2]);
   } else {
      Vect_Init(Proj->ZPos,0.0,0.0,1e32);
   }

   if (GL) {
      if (Proj->Type->Def==PROJPLANE) {
         p0[3]=p1[3]=Proj->LJ+0.0001;
         p2[3]=p3[3]=Proj->LI+0.0001;
         glClipPlane(GL_CLIP_PLANE0,p0);
         glClipPlane(GL_CLIP_PLANE1,p1);
         glClipPlane(GL_CLIP_PLANE2,p2);
         glClipPlane(GL_CLIP_PLANE3,p3);
         glEnable(GL_CLIP_PLANE0);
         glEnable(GL_CLIP_PLANE1);
         glEnable(GL_CLIP_PLANE2);
         glEnable(GL_CLIP_PLANE3);
      } else if (Proj->Type->Def==PROJCYLIN) {
         p2[3]=Proj->L+2+0.0001;
         p3[3]=-Proj->L+2+0.0001;
         glClipPlane(GL_CLIP_PLANE2,p2);
         glClipPlane(GL_CLIP_PLANE3,p3);
         glEnable(GL_CLIP_PLANE2);
         glEnable(GL_CLIP_PLANE3);
      }

      /*Lightning parameters*/
      glLightModeli(GL_LIGHT_MODEL_LOCAL_VIEWER,GL_FALSE);
      glLightModeli(GL_LIGHT_MODEL_TWO_SIDE,GL_FALSE);
      glLightModelfv(GL_LIGHT_MODEL_AMBIENT,amb[1]);
      glColorMaterial(GL_FRONT,GL_AMBIENT_AND_DIFFUSE);
      glMateriali(GL_FRONT,GL_SHININESS,128);

      /*Get the sun*/
      if (Proj->Sun) {
         sec=Proj->Date>86400?Proj->Date:time(NULL)+Proj->Date;
         Astro_SunPos(sec,&Proj->SunPos.Lat,&Proj->SunPos.Lon);
         Proj->SunPos.Elev=146000.0;
         Proj->Type->Project(Proj,(GeoVect*)&Proj->SunPos,(GeoVect*)&pt,1);
         Proj->LightPos[0]=pt[0];Proj->LightPos[1]=pt[1];Proj->LightPos[2]=pt[2];Proj->LightPos[3]=0.0;
      } else {
         Proj->LightPos[0]=0.0;Proj->LightPos[1]=0.943158;Proj->LightPos[2]=0.3;Proj->LightPos[3]=0.0;
//         Proj->LightPos[0]=0.0;Proj->LightPos[1]=146000.0;Proj->LightPos[2]=0.0;Proj->LightPos[3]=0.0;
//         gluProject(VP->Cam->From[0],VP->Cam->From[1],VP->Cam->From[2],VP->GLModS,VP->GLProj,VP->GLView,&pc[0],&pc[1],&pc[2]);
//         gluProject(VP->Cam->Up[0],VP->Cam->Up[1],VP->Cam->Up[2],VP->GLModS,VP->GLProj,VP->GLView,&pc[0],&pc[1],&pc[2]);
//         Proj->LightPos[0]=pc[0]*0.001;Proj->LightPos[1]=pc[1]*0.001;Proj->LightPos[2]=pc[2]*0.001;
      }
      Vect_Init(Proj->Nr,0.0,1.0,0.0);
      glLightfv(GL_LIGHT0,GL_AMBIENT,amb[Proj->Sun?1:0]);
      glLightfv(GL_LIGHT0,GL_SPECULAR,spec[Proj->Sun?1:0]);
      glLightfv(GL_LIGHT0,GL_DIFFUSE,diff[Proj->Sun?1:0]);
      glMaterialfv(GL_FRONT,GL_SPECULAR,ref[Proj->Sun?1:0]);
      glLightfv(GL_LIGHT0,GL_POSITION,Proj->LightPos);
   }

   if (Proj->Sun) {
      glEnable(GL_LIGHTING);
      glEnable(GL_LIGHT0);
      glEnable(GL_COLOR_MATERIAL);
   } else {
      glDisable(GL_LIGHTING);
      glDisable(GL_LIGHT0);
      glDisable(GL_COLOR_MATERIAL);
   }
}

/*----------------------------------------------------------------------------
 * Nom      : <LiangBarsky_LineClip2D>
 * Creation : Octobre 1998 - J.P. Gauthier - CMC/CMOE
 *
 * But      : Algorithme Liang-Barsky pour le clipping de ligne parametrique.
 *
 * Parametres :
 *  <Pt1>     : Point 1 de la ligne
 *  <Pt2>     : Point 2 de la ligne
 *  <X0>      : Point en x du premier coin du clipping rectangle
 *  <Y0>      : Point en y du premier coin du clipping rectangle
 *  <X1>      : Point en x du deuxieme coin du clipping rectangle
 *  <Y1>      : Point en y du deuxieme coin du clipping rectangle
 *
 * Retour:
 *  <visible> : La ligne est visible ou non.
 *
 * Remarques :
 *
 *----------------------------------------------------------------------------
*/
int LiangBarsky_LineClip2D(Vect3d Pt1,Vect3d Pt2,int *C1,int *C2,double X0,double Y0,double X1,double Y1) {

   double dx=Pt2[0]-Pt1[0];
   double dy=Pt2[1]-Pt1[1];
   int visible=0;

   *C1=0;
   *C2=0;

   /*Verifier si il est necessaire de faire un clipping*/
   if (INSIDE(Pt1,X0,Y0,X1,Y1) && INSIDE(Pt2,X0,Y0,X1,Y1)) {
      visible=1;
   } else {
      double Te=0.0;
      double Tl=1.0;
      if (LiangBarsky_LineClipT(-dx,Pt1[0]-X0,&Te,&Tl)) {
         if (LiangBarsky_LineClipT(dx,X1-Pt1[0],&Te,&Tl)) {
            if (LiangBarsky_LineClipT(-dy,Pt1[1]-Y0,&Te,&Tl)) {
               if (LiangBarsky_LineClipT(dy,Y1-Pt1[1],&Te,&Tl)) {

                  visible=1;

                  if (Tl<1.0){
                     Pt2[0]=Pt1[0]+Tl*dx;
                     Pt2[1]=Pt1[1]+Tl*dy;
                     *C2=1;
                  }
                  if (Te>0.0) {
                     Pt1[0]+=Te*dx;
                     Pt1[1]+=Te*dy;
                     *C1=1;
                  }
               }
            }
         }
      }
   }
   return(visible);
}

/*----------------------------------------------------------------------------
 * Nom      : <LiangBarsky_LineClipT>
 * Creation : Octobre 1998 - J.P. Gauthier - CMC/CMOE
 *
 * But      : Algorithme Liang-Barsky pour le clipping de ligne parametrique.
 *            Cette partie calcule les nouvelles valeurs de Te et Tl pour une
 *            intersection avec un cote du rectangle de clipping
 *
 * Parametres :
 *  <Denom>   :
 *  <Num>     :
 *  <Te>      :
 *  <Tl>      :
 *
 * Retour:
 *  <clipped> : La ligne a ete clipper ou non.
 *
 * Remarques :
 *
 *----------------------------------------------------------------------------
*/
int LiangBarsky_LineClipT(double Denom,double Num,double *Te,double *Tl){

   double inter;

   if (Denom < 0.0) {           /*Intersection a PE*/
      inter=Num/Denom;          /*Valeur de t a l'intersection*/
      if (inter > *Tl) {        /*Empiettement de Te et Tl*/
         return 0;              /*Rejeter la ligne*/
      } else if (inter > *Te) { /*On trouve un nouveau Te*/
         *Te=inter;
      }
   } else if (Denom > 0.0) {    /*Intersection a PL*/
      inter=Num/Denom;          /*Valeur de t a l'intersection*/
      if (inter < *Te) {        /*Empiettement de Te et Tl*/
         return 0;              /*Rejeter la ligne*/
      } else if  (inter < *Tl) {
         *Tl=inter;
      }
   } else if (Num < 0.0) {      /*La ligne est a l'exterieur*/
      return 0;
   }
   return 1;
}

/*----------------------------------------------------------------------------
 * Nom      : <LiangBarsky_PolygonClip2D>
 * Creation : Fevrier 1999 - J.P. Gauthier - CMC/CMOE
 *
 * But      : Algorithme Liang-Barsky pour le clipping de polygone former
 *            de ligne parametrique.
 *
 * Parametres :
 *  <Pt>      : Liste des points formant le polygone.
 *  <Nb>      : Nombre de points dans le polygone.
 *  <OutPt>   : Polygone "clipper" de sortie.
 *  <OutNb>   : Nombre de points du polygone "clipper".
 *  <X0>      : Coordonne X de la limite inferieur du "clipping rectangle".
 *  <Y0>      : Coordonne Y de la limite inferieur du "clipping rectangle".
 *  <X1>      : Coordonne X de la limite superieur du "clipping rectangle".
 *  <Y1>      : Coordonne Y de la limite superieur du "clipping rectangle".
 *
 * Retour:
 *
 * Remarques :
 *
 *----------------------------------------------------------------------------
*/
int LiangBarsky_PolygonClip2D(Vect3d *Pt,int Nb,Vect3d *OutPt,int *OutNb,double X0,double Y0,double X1,double Y1){

   double xin,xout,yin,yout;        /*Coordonnees des points d'entrees et de sortie*/
   double tout1,tin2,tout2;         /* */
   double tinx,toutx,tiny,touty;    /*Parametres des intersections*/
   double deltax,deltay;            /*Direction du segment*/
   Vect3d pt;                       /*Coordonnees temporaires*/

   register int i,n;

   *OutNb=0;
   Vect_Assign(pt,Pt[0]);
   n=0;

   for(i=0;i<Nb;i++){

      deltax=Pt[i][0] - pt[0];
      deltay=Pt[i][1] - pt[1];

      /*Determiner quel cote du rectangle de clipping est traverse en premier*/
      if ((deltax>0.0) || (deltax==0.0 && pt[0]>X1)) {
         xin=X0;
         xout=X1;
      } else {
         xin=X1;
         xout=X0;
      }

      if ((deltay>0.0) || (deltay==0.0 && pt[1]>Y1)) {
         yin=Y0;
         yout=Y1;
      } else {
         yin=Y1;
         yout=Y0;
      }

      /*Trouver les valeurs de t pour les points d'entree x et y*/
      if (deltax!=0.0) {
         toutx=(xout-pt[0])/deltax;
      } else if (pt[0]<=X1 && X0<=pt[0]){
         toutx=HUGE_VAL;
      } else {
         toutx=-HUGE_VAL;
      }

      if (deltay!=0.0) {
         touty=(yout-pt[1])/deltay;
      } else if (pt[1]<=Y1 && Y0<=pt[1]){
         touty=HUGE_VAL;
      } else {
         touty=-HUGE_VAL;
      }

      /*Ordonner les points de sortie*/
      if (toutx<touty){
         tout1=toutx;
         tout2=touty;
      } else {
         tout1=touty;
         tout2=toutx;
      }

      if (tout2>0.0) {
         if (deltax!=0.0) {
            tinx=(xin-pt[0])/deltax;
         } else {
            tinx=-HUGE_VAL;
         }
         if (deltay!=0.0) {
            tiny=(yin-pt[1])/deltay;
         } else {
            tiny=-HUGE_VAL;
         }
         if (tinx<tiny){
            tin2=tiny;
         } else {
            tin2=tinx;
         }

         if (tout1<tin2) {

            /*Pas de Segment visible*/
            if (0.0<tout1 && tout1<=1.0) {

               /*La ligne croise un coin intermediaire du rectangle de clipping*/
               /*Il faut ajouter le segment de coin*/
               if (tinx<tiny) {
                  n++;
                  LIANGCOPY(xout,yin,OutPt,*OutNb);
               } else {
                  n++;
                  LIANGCOPY(xin,yout,OutPt,*OutNb);
               }
            }
         } else {

            /*La ligne croise le rectangle de clipping*/
            if (0.0<tout1 && tin2<=1.0) {

               /*Segment visible*/
               if (0.0<tin2) {

                  /*Selectioner le bon cote d'intersection*/
                  if (tinx>tiny) {
                     n++;
                     LIANGCOPY(xin,pt[1]+tinx*deltay,OutPt,*OutNb);
                  } else {
                     n++;
                     LIANGCOPY(pt[0]+tiny*deltax,yin,OutPt,*OutNb);
                  }
               }
               if (1.0>tout1) {

                  /*Selectioner le bon cote d'intersection*/
                  if (toutx<touty) {
                     LIANGCOPY(xout,pt[1]+toutx*deltay,OutPt,*OutNb);
                     n++;
                  } else {
                     LIANGCOPY(pt[0]+touty*deltax,yout,OutPt,*OutNb);
                     n++;
                  }
               } else {
                  /*Garder le point de fin*/
                  LIANGCOPY(Pt[i][0],Pt[i][1],OutPt,*OutNb);
               }
            }
         }
         if (0.0<tout2 && tout2<=1.0) {
            LIANGCOPY(xout,yout,OutPt,*OutNb);
         }
      }
      Vect_Assign(pt,Pt[i]);
   }
   return(n);
}
