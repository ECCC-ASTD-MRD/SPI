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

#ifdef LNK_GDALOGR
#include "tclGDAL.h"
#include "tclOGR.h"
#endif

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

   int                idx,handle,t;
   short              zs;
   unsigned char      zc;
   double             lat,lon;
   Projection        *proj;
   static CONST char *sopt[] = { "create","configure","function","destroy","clean","is","data",NULL };
   enum               opt { CREATE,CONFIGURE,FUNCTION,DESTROY,CLEAN,IS,DATA };

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
         if (Objc!=3) {
            Tcl_WrongNumArgs(Interp,2,Objv,"name");
            return TCL_ERROR;
         }
         return Projection_Create(Interp,Tcl_GetString(Objv[2]));
         break;

      case CONFIGURE:
         if (Objc<3) {
            Tcl_WrongNumArgs(Interp,2,Objv,"name ?option?");
            return TCL_ERROR;
         }
         return Projection_Config(Interp,Tcl_GetString(Objv[2]),Objc-3,Objv+3);
         break;

      case FUNCTION:
         if (Objc<3) {
            Tcl_WrongNumArgs(Interp,2,Objv,"name ?option?");
            return TCL_ERROR;
         }
         return Projection_Function(Interp,Tcl_GetString(Objv[2]),Objc-3,Objv+3);
         break;

      case DESTROY:
         if (Objc!=3) {
            Tcl_WrongNumArgs(Interp,2,Objv,"name");
            return TCL_ERROR;
         }
         return Projection_Destroy(Interp,Tcl_GetString(Objv[2]));
         break;

      case CLEAN:
        proj=Projection_Get(Tcl_GetString(Objv[2]));
         if (!proj) {
            Tcl_AppendResult(Interp,"\n   Projection_Cmd: Projection name unknown: \"",Tcl_GetString(Objv[2]),"\"",(char *)NULL);
            return TCL_ERROR;
         }
         if (strcmp(proj->Type->Name,"grid")==0) {
            Grid_Setup(Interp,proj);
         } else {
            ViewportClean(proj->Params->VP,1,1);
            Projection_Clean(Interp,proj,GDB_FORCE);
         }
         break;

      case DATA:
        if (Objc<6) {
            Tcl_WrongNumArgs(Interp,2,Objv,"name [-BATHYMETRY|-TOPOGRAPHY|-MASK|-TYPE] lat lon");
            return TCL_ERROR;
         }
         proj=Projection_Get(Tcl_GetString(Objv[2]));
         Tcl_GetDoubleFromObj(Interp,Objv[4],&lat);
         Tcl_GetDoubleFromObj(Interp,Objv[5],&lon);

         if (strncmp(Tcl_GetString(Objv[3]),"-BATH",5)==0) {
            t=2;
            handle=gdb_mapopen(proj->Geo->Res,GDB_MAP_BAT,&t);
            gdb_mapget(handle,lat,lon,(void*)&zs);
            Tcl_SetObjResult(Interp,Tcl_NewIntObj(zs>0?0:zs));
         } else if (strncmp(Tcl_GetString(Objv[3]),"-TOPO",5)==0) {
            t=2;
            handle=gdb_mapopen(proj->Geo->Res,GDB_MAP_DEM,&t);
            gdb_mapget(handle,lat,lon,(void*)&zs);
            Tcl_SetObjResult(Interp,Tcl_NewIntObj(zs));
         } else if (strncmp(Tcl_GetString(Objv[3]),"-MASK",5)==0) {
            t=1;
            handle=gdb_mapopen(proj->Geo->Res,GDB_MAP_MSK,&t);
            gdb_mapget(handle,lat,lon,(void*)&zc);
            Tcl_SetObjResult(Interp,Tcl_NewIntObj(zc<127?0:1));
         } else if (strncmp(Tcl_GetString(Objv[3]),"-TYPE",5)==0) {
            t=1;
            handle=gdb_mapopen(proj->Geo->Res,GDB_MAP_TER,&t);
            gdb_mapget(handle,lat,lon,(void*)&zc);
            Tcl_SetObjResult(Interp,Tcl_NewIntObj(zc));
         } else {
            Tcl_AppendResult(Interp,"Invalid data type , must be one of -BATHYMETRY, -TOPOGRAPHY, -MASK, -TYPE",(char*)NULL);
         }
         gdb_mapclose(handle);
         break;

      case IS:
         if (Objc!=3) {
            Tcl_WrongNumArgs(Interp,2,Objv,"name");
            return TCL_ERROR;
         }
         if (Projection_Get(Tcl_GetString(Objv[2]))) {
            Tcl_SetObjResult(Interp,Tcl_NewBooleanObj(1));
         } else {
            Tcl_SetObjResult(Interp,Tcl_NewBooleanObj(0));
         }
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

   Tcl_Obj    *obj;
   Projection *proj;
   int         idx,n,nobj;
   Coord       loc0,loc1,loct;
   double      x,y,d;

   static CONST char *sopt[] = { "-path","-dist","-bearing","-circle","-coordgrid","-gridcoord",NULL };
   enum                opt { PATH,DIST,BEARING,CIRCLE,COORDGRID,GRIDCOORD };

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
         if (proj->Params->Ref) {
            proj->Params->Ref->Project(proj->Params->Ref,x,y,&loc0.Lat,&loc0.Lon,1,1);
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
         if (proj->Params->Ref) {
            proj->Params->Ref->UnProject(proj->Params->Ref,&x,&y,loc0.Lat,loc0.Lon,1,1);
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
         if (Objc!=5){
            Tcl_WrongNumArgs(Interp,0,Objv,"lat lon dist angle");
            return(TCL_ERROR);
         }
         Tcl_GetDoubleFromObj(Interp,Objv[1],&loc0.Lat);
         Tcl_GetDoubleFromObj(Interp,Objv[2],&loc0.Lon);
         Tcl_GetDoubleFromObj(Interp,Objv[3],&d);
         Tcl_GetDoubleFromObj(Interp,Objv[4],&x);

         loc0.Lat=DEG2RAD(loc0.Lat);
         loc0.Lon=DEG2RAD(loc0.Lon);
         x=DEG2RAD(x);
         d=M2RAD(d);

         loc1.Lat=asin(sin(loc0.Lat)*cos(d)+cos(loc0.Lat)*sin(d)*cos(x));
         loc1.Lon=fmod(loc0.Lon+(atan2(sin(x)*sin(d)*cos(loc0.Lat),cos(d)-sin(loc0.Lat)*sin(loc1.Lat)))+M_PI,M_2PI)-M_PI;
         loc1.Lat=RAD2DEG(loc1.Lat);loc1.Lon=RAD2DEG(loc1.Lon);

         obj=Tcl_NewListObj(0,NULL);
         Tcl_ListObjAppendElement(Interp,obj,Tcl_NewDoubleObj(loc1.Lat));
         Tcl_ListObjAppendElement(Interp,obj,Tcl_NewDoubleObj(loc1.Lon));
         Tcl_SetObjResult(Interp,obj);
         break;

      case DIST:
         if (Objc!=2 && Objc!=3){
            Tcl_WrongNumArgs(Interp,0,Objv,"{ coords } elev");
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

     case BEARING:
         if (Objc!=5){
            Tcl_WrongNumArgs(Interp,0,Objv,"lat0 lon0 lat1 lon1");
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
#ifdef LNK_GDALOGR
   GDAL_Band *band;
   OGR_Layer *layer;
   T3DModel  *mdl;
#endif

   GDB_TileFreeAll(Proj->Geo,Mode);
   if (Proj->Params->VP && Proj->Params->VP->Cam)
      Proj->Params->VP->Cam->Update=1;

   for (i=0;i<Proj->NbData;i++) {
      Tcl_ListObjIndex(Interp,Proj->Data,i,&obj);
#ifdef LNK_GDALOGR
      band=GDAL_BandGet(Tcl_GetString(obj));
      if (band) {
         GDAL_BandClean(band,0,1,0);
      }

      layer=OGR_LayerGet(Tcl_GetString(obj));
      if (layer) {
         OGR_LayerClean(layer);
      }

      mdl=Model_Get(Tcl_GetString(obj));
      if (mdl) {
         Model_Clean(mdl);
      }
#endif
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

   static CONST char *sopt[] = { "-location","-gridpoint","-gridsize","-gridextent","-mapres","-maptopo","-mapbath","-maptext","-mapcoast","-maplake","-mapriver","-mappolit",
                                 "-mapadmin","-mapcity","-maproad","-maprail","-mapplace","-mapcoord","-scale","-data","-type","-georef","-geographic","-mask","-date","-sun","-axiscoord","-axis",NULL };
   enum                opt { LOCATION,GRIDPOINT,GRIDSIZE,GRIDEXTENT,MAPRES,MAPTOPO,MAPBATH,MAPTEXT,MAPCOAST,MAPLAKE,MAPRIVER,MAPPOLIT,
                             MAPADMIN,MAPCITY,MAPROAD,MAPRAIL,MAPPLACE,MAPCOORD,SCALE,DATA,TYPE,GEOREF,GEOGRAPHIC,MASK,DATE,SUN,AXISCOORD,AXIS };

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
         case LOCATION:
            if (Objc==1) {
               obj=Tcl_NewListObj(0,NULL);
               Tcl_ListObjAppendElement(Interp,obj,Tcl_NewDoubleObj(proj->Params->Lat));
               Tcl_ListObjAppendElement(Interp,obj,Tcl_NewDoubleObj(proj->Params->Lon));
               Tcl_SetObjResult(Interp,obj);
            } else {
               Tcl_GetDoubleFromObj(Interp,Objv[++i],&lat);
               Tcl_GetDoubleFromObj(Interp,Objv[++i],&lon);

               if (proj->Params->Geographic) {
                  if (lat>-90.0 && lat<90.0 || lon>-360.0 && lon<360.0) {
                     proj->Params->Lat=lat;
                     proj->Params->Lon=lon;

                     if (proj->Type && proj->Type->Def==PROJSPHERE) {
                        ViewportClean(proj->Params->VP,1,1);
                        Projection_Clean(Interp,proj,GDB_FORCE);
                     }
                     if (proj->Params->Ref) {
                        proj->Params->Ref->UnProject(proj->Params->Ref,&ni,&nj,proj->Params->Lat,proj->Params->Lon,1,1);
                        proj->Params->I=ni;
                        proj->Params->J=nj;
                     }
                  }
               } else {
                  proj->Params->I=proj->Params->Lon=lon;
                  proj->Params->J=proj->Params->Lat=lat;
               }
               if (proj->Params->VP) {
                  ViewportSetup(proj->Params->VP->canvas,proj->Params->VP,proj,Tk_Width(Tk_CanvasTkwin(proj->Params->VP->canvas)),Tk_Height(Tk_CanvasTkwin(proj->Params->VP->canvas)),0,1,0);
                  Projection_Setup(proj->Params->VP,proj,0);
              }
            }
            break;

         case GRIDPOINT:
            if (Objc==1) {
               obj=Tcl_NewListObj(0,NULL);
               Tcl_ListObjAppendElement(Interp,obj,Tcl_NewDoubleObj(proj->Params->I));
               Tcl_ListObjAppendElement(Interp,obj,Tcl_NewDoubleObj(proj->Params->J));
               Tcl_SetObjResult(Interp,obj);
            } else {
               Tcl_GetDoubleFromObj(Interp,Objv[++i],&proj->Params->I);
               Tcl_GetDoubleFromObj(Interp,Objv[++i],&proj->Params->J);

               if (proj->Params->Ref && proj->Params->Geographic) {
                  proj->Params->Ref->Project(proj->Params->Ref,proj->Params->I,proj->Params->J,&lat,&lon,1,1);
                  proj->Params->Lat=lat;
                  proj->Params->Lon=lon;
               } else {
                  proj->Params->Lat=proj->Params->J;
                  proj->Params->Lon=proj->Params->I;
               }
               if (proj->Params->VP) {
                  ViewportSetup(proj->Params->VP->canvas,proj->Params->VP,proj,Tk_Width(Tk_CanvasTkwin(proj->Params->VP->canvas)),Tk_Height(Tk_CanvasTkwin(proj->Params->VP->canvas)),0,1,0);
                  Projection_Setup(proj->Params->VP,proj,0);
               }
            }
            break;

        case GRIDSIZE:
            if (Objc==1) {
               obj=Tcl_NewListObj(0,NULL);
               if (proj->Params->Ref) {
                  Tcl_ListObjAppendElement(Interp,obj,Tcl_NewIntObj(proj->Params->Ref->X1-proj->Params->Ref->X0));
                  Tcl_ListObjAppendElement(Interp,obj,Tcl_NewIntObj(proj->Params->Ref->Y1-proj->Params->Ref->Y0));
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
               if (proj->Params->Ref) {
                  Tcl_ListObjAppendElement(Interp,obj,Tcl_NewIntObj(proj->Params->Ref->X0));
                  Tcl_ListObjAppendElement(Interp,obj,Tcl_NewIntObj(proj->Params->Ref->Y0));
                  Tcl_ListObjAppendElement(Interp,obj,Tcl_NewIntObj(proj->Params->Ref->X1));
                  Tcl_ListObjAppendElement(Interp,obj,Tcl_NewIntObj(proj->Params->Ref->Y1));
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

        case SUN:
           if (Objc==1) {
               Tcl_SetObjResult(Interp,Tcl_NewBooleanObj(proj->Sun));
            } else {
               Tcl_GetBooleanFromObj(Interp,Objv[++i],&proj->Sun);
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
               if (tok==0 || tok<0 && proj->Geo->Params.Coast>0 || proj->Geo->Params.Coast<0 && tok>0)
                  GDB_TileFreeType(proj->Geo,GDB_TYPE_COAST);
               proj->Geo->Params.Coast=tok;
            }
            break;

        case MAPLAKE:
            if (Objc==1) {
               Tcl_SetObjResult(Interp,Tcl_NewIntObj(proj->Geo->Params.Lake));
            } else {
               Tcl_GetIntFromObj(Interp,Objv[++i],&tok);
               if (tok==0 || tok<0 && proj->Geo->Params.Lake>0 || proj->Geo->Params.Lake<0 && tok>0)
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
               Tcl_SetObjResult(Interp,Tcl_NewDoubleObj(proj->Params->Scale));
            } else {
               Tcl_GetDoubleFromObj(Interp,Objv[++i],&tmp);
               if (tmp!=proj->Params->Scale) {
                  proj->Params->Scale=tmp;
                  ViewportClean(proj->Params->VP,1,1);
                  Projection_Clean(Interp,proj,GDB_RASTER);
               }
            }
            break;

        case AXISCOORD:
            if (Objc==1) {
               obj=Tcl_NewListObj(0,NULL);
               Tcl_ListObjAppendElement(Interp,obj,Tcl_NewDoubleObj(proj->Params->ZAxis.Lat));
               Tcl_ListObjAppendElement(Interp,obj,Tcl_NewDoubleObj(proj->Params->ZAxis.Lon));
               Tcl_ListObjAppendElement(Interp,obj,Tcl_NewDoubleObj(proj->Params->ZAxis.Elev));
               Tcl_SetObjResult(Interp,obj);
            } else {
               Tcl_GetDoubleFromObj(Interp,Objv[++i],&proj->Params->ZAxis.Lat);
               Tcl_GetDoubleFromObj(Interp,Objv[++i],&proj->Params->ZAxis.Lon);
               Tcl_GetDoubleFromObj(Interp,Objv[++i],&proj->Params->ZAxis.Elev);
            }
            break;

        case AXIS:
            if (Objc==1) {
               Tcl_SetObjResult(Interp,Tcl_NewIntObj(proj->Params->TAxis));
            } else {
               Tcl_GetIntFromObj(Interp,Objv[++i],&proj->Params->TAxis);
            }
            break;

        case MASK:
            if (Objc==1) {
               switch(proj->Geo->Params.Mask) {
                  case -1: Tcl_SetObjResult(Interp,Tcl_NewStringObj("DATA",0));break;
                  case  0: Tcl_SetObjResult(Interp,Tcl_NewStringObj("NONE",0));break;
                  case  1: Tcl_SetObjResult(Interp,Tcl_NewStringObj("LAND",0));break;
                  case  2: Tcl_SetObjResult(Interp,Tcl_NewStringObj("SEA",0));break;
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

       case GEOREF:
            if (Objc==1) {
               if (proj->Params->Ref)
                  Tcl_SetObjResult(Interp,Tcl_NewStringObj(proj->Params->Ref->Name,-1));
            } else {
               if (!strlen(Tcl_GetString(Objv[++i]))) {
                  ref=NULL;
               } else if (!(ref=GeoRef_Get(Tcl_GetString(Objv[i])))) {
                  Tcl_AppendResult(Interp,"\n   Projection_Config: Georeference unknown (",Tcl_GetString(Objv[i]),")",(char*)NULL);
                  return TCL_ERROR;
               }
               if (ref!=proj->Params->Ref) {
                  if (proj->Params->Ref)
                     GeoRef_Destroy(Interp,proj->Params->Ref->Name);
                  proj->Params->Ref=ref;
                  GeoRef_Incr(proj->Params->Ref);
                  if (strcmp(proj->Type->Name,"grid")==0) {
                     Grid_Setup(Interp,proj);
                  }
               }
            }
            break;

       case GEOGRAPHIC:
            if (Objc==1) {
               Tcl_SetObjResult(Interp,Tcl_NewBooleanObj(proj->Params->Geographic));
            } else {
               Tcl_GetBooleanFromObj(Interp,Objv[++i],&proj->Params->Geographic);
               if (strcmp(proj->Type->Name,"grid")==0) {
                  Grid_Setup(Interp,proj);
               }
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
                     proj->Params->L=proj->Params->LI=proj->Params->LJ=1.0;
                     if (strcmp(Tcl_GetString(Objv[i]),"grid")==0) {
                        Grid_Setup(Interp,proj);
                     } else {
                        ViewportClean(proj->Params->VP,1,1);
                        Projection_Clean(Interp,proj,GDB_FORCE);
                    }
                  }
               }
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

   proj->PixDist   = 0.0;
   proj->Type      = NULL;
   proj->Data      = NULL;
   proj->NbData    = 0;
   proj->Date      = 0;
   proj->Sun       = 0;
   proj->Loading   = 0;

   /*Parametres commun*/
   proj->Params = (ProjParams*)malloc(sizeof(ProjParams));
   proj->Params->Scale        = 1;
   proj->Params->ZFactor      = 1.0;
   proj->Params->TAxis        = 1;
   proj->Params->ZAxis.Lat    = 0.0;
   proj->Params->ZAxis.Lon    = 0.0;
   proj->Params->ZAxis.Elev   = 0.0;
   proj->Params->Lat          = 0.0;
   proj->Params->Lon          = 0.0;
   proj->Params->VP           = NULL;

   /*Parametres de projections sur grille*/
   proj->Params->Ref          = NULL;
   proj->Params->Geographic   = 1;
   proj->Params->I            = 0.0;
   proj->Params->J            = 0.0;
   proj->Params->L            = 0.0;
   proj->Params->LI           = 1.0;
   proj->Params->LJ           = 1.0;

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
void Projection_CreateType(char *Name,int Def,
   Projection_CallLocate *Locate,Projection_CallRender *Render,Projection_CallDraw *DrawFirst,
   Projection_CallDraw *DrawLast,Projection_CallDraw *DrawGlobe,Projection_CallUnProject *UnProject,Projection_CallProject *Project,
   Projection_CallProjectPoint *ProjectPoint,Projection_CallProjectLine *ProjectLine) {

   ProjectionType *project_type;

   if (!(project_type=(ProjectionType*)TclY_HashPut(NULL,&ProjectionTypes,Name,sizeof(ProjectionType)))) {
      exit(0);
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
      if (proj->Params->Ref)
         GeoRef_Destroy(Interp,proj->Params->Ref->Name);

      if (proj->Geo) {
         Projection_Clean(Interp,proj,GDB_FORCE);
         free(proj->Geo);
       }

      if (proj->Data) {
         Tcl_DecrRefCount(proj->Data);
      }

      free(proj->Params);
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
      Tcl_CreateObjCommand(Interp,"projection",Projection_Cmd,(ClientData)NULL,(Tcl_CmdDeleteProc*)NULL);

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

   Proj->Type->Project(Proj->Params,(GeoVect*)&Co,(GeoVect*)&vr,1);
   if (Proj->Type->Def==PROJCYLIN) {
      d=vr[0]-Proj->Params->L;
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
   int        ras=0,i;

#ifdef LNK_GDALOGR
   GDAL_Band *band;
   OGR_Layer *layer;
#endif

   if (Proj->Type->Def==PROJPLANE && !Proj->Params->Ref) {
      return(0);
   }

   glDisable(GL_STENCIL_TEST);
   glDisable(GL_DEPTH_TEST);
   glDepthFunc(GL_LESS);
   glDepthMask(GL_TRUE);
   glStencilMask(0xdf);

   if (Mode==GL_ALL || Mode==GL_VECTOR) {

      if (Proj->Params->Geographic) {
   //   GDB_StarRender(Interp,Proj);

         /*Initialiser le mask des donnees vectorielles*/
         glEnable(GL_STENCIL_TEST);
         glStencilFunc(GL_ALWAYS,0x01,0x01);
         glStencilOp(GL_REPLACE,GL_REPLACE,GL_REPLACE);

         /*Initialiser l'impression de la geographie*/
         if (Interp) {
            Tcl_AppendResult(Interp,"\n% Postscript de la geographie\n",(char*)NULL);
         }

         /*Afficher les parties dependantes de la projection*/
         if (Proj->Type->DrawFirst)
            Proj->Type->DrawFirst(Interp,VP,Proj);

         if (Interp) {
            GDB_TileRender(Interp,Proj,Proj->Geo,GDB_ALL^GDB_RASTER^GDB_MASK^GDB_FILL);
         } else {
            GDB_TileRender(Interp,Proj,Proj->Geo,GDB_ALL^GDB_RASTER^GDB_MASK);
         }
      }
      glStencilFunc(GL_EQUAL,0x00,0x0f);
      glStencilOp(GL_KEEP,GL_KEEP,GL_KEEP);
   }

   if (Mode==GL_ALL || Mode==GL_RASTER) {

      if (Proj->Params->Geographic) {
         if (Interp) {
            GDB_TileRender(NULL,Proj,Proj->Geo,GDB_FILL);
         }
         ras+=GDB_TileRender(Interp,Proj,Proj->Geo,GDB_MASK | GDB_RASTER);

         if (ras && !GLRender->GLZBuf) {
            glClear(GL_DEPTH_BUFFER_BIT);
         }
      }

      for (i=0;i<Proj->NbData;i++) {
         Tcl_ListObjIndex(Interp,Proj->Data,i,&obj);
#ifdef LNK_GDALOGR
         if ((layer=OGR_LayerGet(Tcl_GetString(obj)))) {
            ras+=OGR_LayerRender(NULL,Proj,VP,layer);
         }
         if ((band=GDAL_BandGet(Tcl_GetString(obj)))) {
            ras+=GDAL_BandRender(Proj,VP,band);
         }
#endif
         if ((mdl=Model_Get(Tcl_GetString(obj)))) {
            ras+=Model_Render(Proj,VP,mdl);
         }
      }
      if (!GLRender->GLZBuf)
         glClear(GL_DEPTH_BUFFER_BIT);

      if (Proj->Type->DrawLast) {
         glDisable(GL_STENCIL_TEST);
         Proj->Type->DrawLast(Interp,VP,Proj);
         glEnable(GL_STENCIL_TEST);
      }
  }

   return ras;
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
   GLdouble pc[3];
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

   Proj->Params->VP=VP;

   if (Proj->Type->Def==PROJPLANE && !Proj->Params->Ref) {
      return;
   }

   /*Rotation sur les axes lat-lon*/
   Proj->Type->Locate(Proj,Proj->Params->Lat,Proj->Params->Lon,0);

   /*Parametres de position*/
   Proj->Params->SLat=sin(DEG2RAD(Proj->Params->Lat));
   Proj->Params->CLat=cos(DEG2RAD(Proj->Params->Lat));

   /*Sauvegarde de la matrice de rotation*/
   glGetDoublev(GL_MODELVIEW_MATRIX,VP->GLModR);

   /*Distance en metres d'un pixel*/
   if (VP && !GLRender->TRCon) {
      if (Proj->Params->Geographic) {
         pt[0]=(VP->Width*0.5)-0.5;
         pt[1]=VP->Height*0.5;
         pt[2]=0;
         Proj->Type->UnProject(VP,Proj->Params,&co[0],pt);

         pt[0]=(VP->Width*0.5)+0.5;
         pt[1]=VP->Height*0.5;
         pt[2]=0;
         Proj->Type->UnProject(VP,Proj->Params,&co[1],pt);

         co[0].Lat=DEG2RAD(co[0].Lat);
         co[0].Lon=DEG2RAD(co[0].Lon);
         co[1].Lat=DEG2RAD(co[1].Lat);
         co[1].Lon=DEG2RAD(co[1].Lon);
         d=DIST(0.0,co[0].Lat,co[0].Lon,co[1].Lat,co[1].Lon);
      } else {
         d=1.0*VP->Cam->Aspect;
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
      if (Proj->Params->Ref->Spatial && OSRIsProjected(Proj->Params->Ref->Spatial)) {
         Proj->Params->ZFactor=1.0/((Proj->Params->L-1.0)*0.5);
      } else {
         Proj->Params->ZFactor=1.0/((Proj->Params->L-1.0)*2000.0);
      }
   } else {
      Proj->Params->ZFactor=1.0/EARTHRADIUS;
   }

   /*Calcul de la position centrale*/
   if (Proj->Type->Def==PROJGLOBE) {
     gluProject(0.0,0.0,0.0,VP->GLModS,VP->GLProj,VP->GLView,&Proj->Params->ZPos[0],&Proj->Params->ZPos[1],&Proj->Params->ZPos[2]);
   } else {
      Vect_Init(Proj->Params->ZPos,0.0,0.0,1e32);
   }

   if (GL) {
      if (Proj->Type->Def==PROJPLANE) {
         p0[3]=p1[3]=Proj->Params->LJ+0.0001;
         p2[3]=p3[3]=Proj->Params->LI+0.0001;
         glClipPlane(GL_CLIP_PLANE0,p0);
         glClipPlane(GL_CLIP_PLANE1,p1);
         glClipPlane(GL_CLIP_PLANE2,p2);
         glClipPlane(GL_CLIP_PLANE3,p3);
         glEnable(GL_CLIP_PLANE0);
         glEnable(GL_CLIP_PLANE1);
         glEnable(GL_CLIP_PLANE2);
         glEnable(GL_CLIP_PLANE3);
      } else if (Proj->Type->Def==PROJCYLIN) {
         p2[3]=Proj->Params->L+2+0.0001;
         p3[3]=-Proj->Params->L+2+0.0001;
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
         Proj->Type->Project(Proj->Params,(GeoVect*)&Proj->SunPos,(GeoVect*)&pt,1);
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
int LiangBarsky_LineClip2D(Vect3d Pt1,Vect3d Pt2,int *C1,int *C2,int X0,int Y0,int X1,int Y1) {

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
   return visible;
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

   register int i;

   *OutNb=0;
   Vect_Assign(pt,Pt[0]);

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
                  LIANGCOPY(xout,yin,OutPt,*OutNb);
               } else {
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
                     LIANGCOPY(xin,pt[1]+tinx*deltay,OutPt,*OutNb);
                  } else {
                     LIANGCOPY(pt[0]+tiny*deltax,yin,OutPt,*OutNb);
                  }
               }
               if (1.0>tout1) {

                  /*Selectioner le bon cote d'intersection*/
                  if (toutx<touty) {
                     LIANGCOPY(xout,pt[1]+toutx*deltay,OutPt,*OutNb);
                  } else {
                     LIANGCOPY(pt[0]+touty*deltax,yout,OutPt,*OutNb);
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
   return i;
}
