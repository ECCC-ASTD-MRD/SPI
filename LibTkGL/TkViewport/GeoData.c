/*=============================================================================
* Environnement Canada
* Centre Meteorologique Canadian
* 2100 Trans-Canadienne
* Dorval, Quebec
*
* Projet    : Projection orthographique de la carte vectorielle.
* Fichier   : GeoData.c
* Creation  : Novembre 2001 - J.P. Gauthier
*
* Description: Fichier de definition du module de donnees geographiques
*
* Remarques :
*   -On utilise la base de donnees GBD de Michel Grenier
*
* License :
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
#include <time.h>

#include "GeoData.h"
#include "Projection.h"
#include "stars.h"

static GDB_Geo  *GeoPtr;
static GDB_Txt  *TxtPtr;

Vect3d GDB_NMap[181][361];

void  GDB_CoordRender(Tcl_Interp *Interp,ViewportItem *VP,Projection *Proj,GDB_Data *GDB);
void  GDB_GeoGetVector(int Type,int Nb,float Lat0,float Lon0,float Lat1,float Lon1,float *LatLon);
void  GDB_GeoProj(GDB_Geo *Geo,Projection *Proj);
void  GDB_GeoRender(Tcl_Interp *Interp,Projection *Proj,GDB_Geo *Geo,int Width,XColor *Color,int Low);
void  GDB_GeoTess(Tcl_Interp *Interp,GDB_Geo *Geo);
void  GDB_FillRender(Tcl_Interp *Interp,Projection *Proj,GDB_Geo *Geo,XColor *Color,GLuint MaskIn);
int   GDB_Loc(GDB_Box Box,Projection *Proj,float X0,float X1,float Y0,float Y1);
void  GDB_TileClear(Projection *Proj,GDB_Tile *Tile,XColor *Color,GLuint MaskIn);
int   GDB_TileGet(void *Tile,Projection *Proj,int Type,int Data);
void  GDB_TileInit(GDB_Tile *Tile,float Lat0,float Lon0,float Delta,Projection *Proj);
int   GDB_TileRender(Tcl_Interp *Interp,Projection *Proj,GDB_Data *GDB,int Mode);
int   GDB_TileResolution(GDB_Data *GDB,double Dist);
void  GDB_TxtGet(int Type,float Lat,float Lon,char *Txt);
void  GDB_TxtFree(GDB_Txt *Txt);
void  GDB_TxtRender(Tcl_Interp *Interp,Projection *Proj,GDB_Txt *Txt,XColor *Color,int Point);
void  GDB_MapRender(Projection *Proj,GDB_Map *Topo,float Lat0,float Lon0,float Delta);

GLuint Texture_Read(char *File);

static Vect3d      *GDB_VBuffer=NULL;
static unsigned int GDB_VBufferSize=0;
static unsigned int GDB_VBufferIncr=512;
static unsigned int GDB_VBufferMax=524288;

Vect3d* GDB_VBufferCopy(Vect3d *To,unsigned int Size) {

   if (GDB_VBuffer && Size<=GDB_VBufferSize) {
      return((Vect3d*)memcpy(To,GDB_VBuffer,Size*sizeof(Vect3d)));
   } else {
      fprintf(stderr,"(ERROR) GDB_VBufferCopy: Invalid size (%i>%i)\n",Size,GDB_VBufferSize);
      return(NULL);
   }
}

unsigned int GDB_VBufferCheck() {

   if (GDB_VBufferSize>GDB_VBufferMax) {
      GDB_VBuffer=(Vect3d*)realloc(GDB_VBuffer,GDB_VBufferMax*sizeof(Vect3d));
      GDB_VBufferSize=GDB_VBufferMax;
   }
   return(GDB_VBufferSize);
}

Vect3d* GDB_VBufferAlloc(unsigned int Size) {

   Vect3d *buf=GDB_VBuffer;

   if (GDB_VBufferSize<Size) {
      Size=(Size/GDB_VBufferIncr+1)*GDB_VBufferIncr;
#ifdef DEBUG
      fprintf(stderr,"(DEBUG) GDB_VBufferAlloc: Reallocating GDB_VBuffer to %i\n",Size);
#endif
      if ((buf=(Vect3d*)realloc(GDB_VBuffer,Size*sizeof(Vect3d)))) {
         GDB_VBuffer=buf;
         GDB_VBufferSize=Size;
      } else {
         fprintf(stderr,"(ERROR) GDB_VBufferAlloc: Could not allocate coordinate buffer GDB_VBuffer\n");
      }
   }
   return(buf);
}

/*--------------------------------------------------------------------------------------------------------------
 * Nom          : <GDB_ThreadProc>
 * Creation     : Juillet 2007 J.P. Gauthier - CMC/CMOE
 *
 * But          : Lire les donnees GDB dans une thread.
 *
 * Parametres    :
 *   <clientData>: Bande
 *
 * Retour       :
 *   <...>      : Code de reussite
 *
 * Remarques    :
 *
 *---------------------------------------------------------------------------------------------------------------
*/
TCL_DECLARE_MUTEX(MUTEX_GDB);
TCL_DECLARE_MUTEX(MUTEX_GDBDATA);
TCL_DECLARE_MUTEX(MUTEX_GDBQUEUE);

typedef int (GDB_DataGetProc)  (void *Data,Projection *Proj,int Param1,int Param2);

typedef struct GDB_ThreadQueueData {
   GDB_Tile        *Tile;
   GDB_DataGetProc *Proc;
   Projection      *Proj;
   int             Param1,Param2;
   struct GDB_ThreadQueueData *Next;
} GDB_ThreadQueueData;

typedef struct GDB_ThreadData {
    Tcl_ThreadId Id;                      /* Tcl ID for this thread */
    Tcl_Condition CondWait;               /* Condition variable used to*/
    GDB_ThreadQueueData *QueueStart;
    GDB_ThreadQueueData *QueueEnd;
} GDB_ThreadData;

static Tcl_ThreadDataKey dataKey;
static GDB_ThreadData *GDBTData=NULL;

void GDB_ThreadQueueRefresh(ViewportItem *VP);
int  GDB_ThreadQueueRemove(Tcl_ThreadId Id,GDB_Tile *Tile);
int  GDB_ThreadQueueAdd(Tcl_ThreadId Id,Projection *Proj,GDB_Tile *Tile,GDB_DataGetProc *Proc,int Param1,int Param2);
int  GDB_ThreadQueueClear(Tcl_ThreadId Id);

Tcl_ThreadCreateType GDB_ThreadProc(ClientData clientData) {

   GDB_ThreadData      *tdata=Tcl_GetThreadData(&dataKey,sizeof(GDB_ThreadData));
   GDB_ThreadQueueData *qdata;
   ViewportItem        *vp=NULL;
   Projection          *proj=NULL;
   int                  r=0;

   GDB_Data *data=(GDB_Data*)clientData;
   GDBTData=tdata;

   /*Loop ad vitam eternam*/
   while(1) {

      /*Wait on queue availability condition*/
      tdata->CondWait=(Tcl_Condition)NULL;
      Tcl_MutexLock(&MUTEX_GDB);
      Tcl_ConditionWait(&tdata->CondWait,&MUTEX_GDB,NULL);
      Tcl_MutexUnlock(&MUTEX_GDB);

      /*Process queued items*/
      proj=vp=NULL;
      while(tdata->QueueStart) {
         if (!(qdata=tdata->QueueStart)) {
            break;
         }

         if (qdata->Proj && qdata->Proj->VP) {
            vp=qdata->Proj->VP;
            proj=qdata->Proj;
         }

         if (qdata->Tile->Flag) {
            r+=qdata->Proc(qdata->Tile,qdata->Proj,qdata->Param1,qdata->Param2);

            /*Redraw if needed*/
            if (vp && !vp->Update && r>25 && !GLRender->XBatch) {
               GDB_ThreadQueueRefresh(vp);
               r=0;
            }
         }

         /*Remove item from queue head*/
         Tcl_MutexLock(&MUTEX_GDBQUEUE);
         tdata->QueueStart=qdata->Next;
         if (!tdata->QueueStart) {
            tdata->QueueEnd=NULL;
         }
         qdata->Tile->Flag=qdata->Tile->Flag<=0?0:qdata->Tile->Flag-1;
         free(qdata);
         Tcl_MutexUnlock(&MUTEX_GDBQUEUE);
      }

      if (proj) proj->Loading=0;
      if (vp)   GDB_ThreadQueueRefresh(vp);

      Tcl_ConditionFinalize(&tdata->CondWait);
   }
   Tcl_ExitThread(0);
}

void GDB_ThreadQueueRefresh(ViewportItem *VP) {

   ThreadEvent *event;

   event=(ThreadEvent*)ckalloc(sizeof(ThreadEvent));
   event->event.proc=ViewportRefresh_ThreadEventProc;
   event->ptr=(void*)VP;

   /*Send redraw event to assigned viewport*/
   Tcl_ThreadQueueEvent(VP->ThreadId,(Tcl_Event*)event,TCL_QUEUE_TAIL);
   Tcl_ThreadAlert(VP->ThreadId);
}

int GDB_ThreadQueueIsEmpty(Tcl_ThreadId Id) {

   return(GDBTData?(GDBTData->QueueStart?0:1):1);
}

int GDB_ThreadQueueClear(Tcl_ThreadId Id) {

   GDB_ThreadQueueData *qdata=NULL;

   Tcl_MutexLock(&MUTEX_GDBQUEUE);

   qdata=GDBTData?GDBTData->QueueStart:NULL;
   while(qdata) {
      if (qdata->Tile) qdata->Tile->Flag=0;
      if (qdata->Proj) qdata->Proj->Loading=0;
      qdata=qdata->Next;
   }

   Tcl_MutexUnlock(&MUTEX_GDBQUEUE);
   return(1);
}

int GDB_ThreadQueueRemove(Tcl_ThreadId Id,GDB_Tile *Tile) {

   GDB_ThreadQueueData *qdata=NULL;

   if (!Tile->Flag) {
      return(0);
   }

   Tcl_MutexLock(&MUTEX_GDBQUEUE);

   qdata=GDBTData?GDBTData->QueueStart:NULL;
   while(qdata) {
      if (qdata->Tile==Tile) {
         Tile->Flag=0;
         break;
      }
      qdata=qdata->Next;
   }
   Tcl_MutexUnlock(&MUTEX_GDBQUEUE);
   return(1);
}

int GDB_ThreadQueueAdd(Tcl_ThreadId Id,Projection *Proj,GDB_Tile *Tile,GDB_DataGetProc *Proc,int Param1,int Param2) {

   GDB_ThreadQueueData *q,*qdata;

   /*Is the GeoData thread created*/
   if (!GDBTData) {
      Proc(Tile,Proj,Param1,Param2);
      return(0);
   }

   /*Allocate new queue item*/
   if (!(qdata=(GDB_ThreadQueueData*)malloc(sizeof(GDB_ThreadQueueData)))) {
      return(0);
   }

   qdata->Tile=Tile;
   qdata->Proc=Proc;
   qdata->Proj=Proj;
   qdata->Param1=Param1;
   qdata->Param2=Param2;
   qdata->Next=NULL;

   Tcl_MutexLock(&MUTEX_GDBQUEUE);

   qdata->Tile->Flag++;
   qdata->Proj->Loading++;

   /*Insert item into queue*/
   if (GDBTData->QueueEnd) {
      q=GDBTData->QueueEnd;
      q->Next=qdata;
      GDBTData->QueueEnd=qdata;
   } else {
      GDBTData->QueueEnd=GDBTData->QueueStart=qdata;
   }
   Tcl_MutexUnlock(&MUTEX_GDBQUEUE);

   /*Wake up thread*/
   Tcl_ConditionNotify(&GDBTData->CondWait);

   return(1);
}

/*----------------------------------------------------------------------------
 * Nom      : <GDB_Init>
 * Creation : Novembre 2001 - J.P. Gauthier - CMC/CMOE
 *
 * But      : Initialiser les tuiles geographiques et la structure GDB.
 *
 * Parametres:
 *   <GDB>   : Structure GDB
 *
 * Retour:
 *
 * Remarques :
 *
 *----------------------------------------------------------------------------
*/
int GDB_Init(GDB_Data *GDB) {

   int    x,y;
   int    lat,lon;
   double flat,flon,clat;

   gdb_init();

   /*Parametres de selection de donnees*/
   GDB->Params.Topo=GDB_NONE;
   GDB->Params.Bath=GDB_NONE;
   GDB->Params.Text=GDB_NONE;
   GDB->Params.Mask=GDB_NONE;

   GDB->Params.Coast=0;
   GDB->Params.Lake=0;
   GDB->Params.River=0;
   GDB->Params.Polit=0;
   GDB->Params.Admin=0;
   GDB->Params.City=0;
   GDB->Params.Road=0;
   GDB->Params.Rail=0;
   GDB->Params.Place=0;

   GDB->Params.CoordLoc=0;
   GDB->Params.CoordDef=0.0;
   GDB->Params.CoordNum=0;

   for(x=0;x<GDB_DEGX;x++) {
      for(y=0;y<GDB_DEGY;y++) {

         GDB->Tile[x][y].Topo.Map=NULL;
         GDB->Tile[x][y].Topo.Vr=NULL;
         GDB->Tile[x][y].Topo.Tex=0;

         GDB->Tile[x][y].TPlace=NULL;
         GDB->Tile[x][y].TCity=NULL;

         GDB->Tile[x][y].Coast=NULL;
         GDB->Tile[x][y].Lake=NULL;
         GDB->Tile[x][y].FCoast=NULL;
         GDB->Tile[x][y].FLake=NULL;
         GDB->Tile[x][y].FCoastIn=NULL;
         GDB->Tile[x][y].FLakeIn=NULL;
         GDB->Tile[x][y].River=NULL;
         GDB->Tile[x][y].Polit=NULL;
         GDB->Tile[x][y].Admin=NULL;
         GDB->Tile[x][y].Road=NULL;
         GDB->Tile[x][y].Road=NULL;
         GDB->Tile[x][y].Rail=NULL;
         GDB->Tile[x][y].Box.Nb=0;
         GDB->Tile[x][y].Res=0;
         GDB->Tile[x][y].Flag=0;
      }
   }

   /*Parametres des dimensions de tuiles dynamiques*/
   GDB->Res=0;
   GDB->DegX=18;
   GDB->DegY=9;
   GDB->DegT=5;

   for(lat=0;lat<=180;lat++) {
      for(lon=0;lon<=360;lon++) {
         flat=DEG2RAD(lat-90.0);
         flon=DEG2RAD(lon-180.0);
         clat=cos(flat);
         Vect_Init(GDB_NMap[lat][lon],clat*sin(flon),sin(flat),clat*cos(flon));
      }
   }
   return 1;
}

/*----------------------------------------------------------------------------
 * Nom      : <GDB_GeoGetVector>
 * Creation : Novembre 2001 - J.P. Gauthier - CMC/CMOE
 *
 * But      : Recuperer une boite de vecteur.
 *
 * Parametres :
 *   <Type>   : Type de donnees
 *   <Nb>     : Nombre de point (latlon)
 *   <Lat0>   : Latitude minimale
 *   <Lon0>   : Longitude minimale
 *   <Lat1>   : Latitude maximale
 *   <Lat1>   : Longitude maximale
 *   <LatLon> : Vecteur de coordonnees

 * Retour:
 *
 * Remarques :
 *   -Cette procedure est appeler en "callback" par GDB en retour d'une requete
 *
 *----------------------------------------------------------------------------
*/
void GDB_GeoGetVector(int Type,int Nb,float Lat0,float Lon0,float Lat1,float Lon1,float *LatLon) {

   GDB_Geo *geo;

   geo=(GDB_Geo*)malloc(sizeof(GDB_Geo));

   geo->Box.Co[0].Lat=Lat0; geo->Box.Co[0].Lon=Lon0; geo->Box.Co[0].Elev=0.0;
   geo->Box.Co[1].Lat=Lat1; geo->Box.Co[1].Lon=Lon0; geo->Box.Co[1].Elev=0.0;
   geo->Box.Co[2].Lat=Lat1; geo->Box.Co[2].Lon=Lon1; geo->Box.Co[2].Elev=0.0;
   geo->Box.Co[3].Lat=Lat0; geo->Box.Co[3].Lon=Lon1; geo->Box.Co[3].Elev=0.0;
   geo->Box.Nb=Nb;

   geo->Loc=(Vect3d*)malloc(Nb*sizeof(Vect3d));
   Nb=0;
   while (Nb<geo->Box.Nb) {
      geo->Loc[Nb][1]=*LatLon++;
      geo->Loc[Nb][0]=*LatLon++;
      geo->Loc[Nb][2]=0.0;
      Nb++;
   }

   geo->List=0;
   geo->Next=GeoPtr;
   GeoPtr=geo;
}

/*----------------------------------------------------------------------------
 * Nom      : <GDB_GeoFree>
 * Creation : Decembre 2001 - J.P. Gauthier - CMC/CMOE
 *
 * But      : Liberer une boite de vecteurs.
 *
 * Parametres:
 *   <Geo>   : Boite de donnees
 *
 * Retour:
 *
 * Remarques :
 *
 *----------------------------------------------------------------------------
*/
void GDB_GeoFree(GDB_Geo *Geo) {

   GDB_Geo *tmp;

   while(Geo) {
      tmp=Geo;
      Geo=Geo->Next;
      if (tmp->List) {
         glDeleteLists(tmp->List,1);
      }
      if (tmp->Loc) free(tmp->Loc);

      free(tmp);
   }
}

/*----------------------------------------------------------------------------
 * Nom      : <GDB_GeoProj>
 * Creation : Decembre 2001 - J.P. Gauthier - CMC/CMOE
 *
 * But      : Localiser les informations dans le referentiel de la projection.
 *
 * Parametres :
 *   <Geo>    : Donnes a localiser
 *   <Proj>   : Parametres de projection
 *
 * Retour:
 *
 * Remarques :
 *
 *----------------------------------------------------------------------------
*/
void GDB_GeoProj(GDB_Geo *Geo,Projection *Proj) {

   /*Pour toutes les boites de vecteurs*/
   while(Geo) {

      /*Projeter tout les vecteurs*/
      Proj->Type->Project(Proj,(GeoVect*)Geo->Loc,NULL,Geo->Box.Nb);

      /*Calculer les limites de la boite*/
      if (!Proj->Type->Project(Proj,(GeoVect*)Geo->Box.Co,(GeoVect*)Geo->Box.Vr,4)) {
         Geo->Box.Nb=-1;
      }

      Geo=Geo->Next;
   }
}

/*----------------------------------------------------------------------------
 * Nom      : <GDB_GetMap>
 * Creation : Janvier 2001 - J.P. Gauthier - CMC/CMOE
 *
 * But      : Recuperer la valeur des "Map" (Elevation) pour une lat-lon specifique
 *
 * Parametres:
 *   <GDB>   : Structure GDB
 *   <Lat>   : Latitude
 *   <Lon>   : Longitude
 *
 * Retour:
 *   <Elev>  : Elevation en metres
 *
 * Remarques :
 *
 *----------------------------------------------------------------------------
*/
int GDB_GetMap(GDB_Data *GDB,double Lat,double Lon) {

   int x,y,m,w,h;
   short *t;

   if (Lat>90.0 || Lat<-90.0 || Lon>180.0 || Lon<-180.0) {
      m=0;
   } else {
      Lat+=90.0;
      Lon+=180.0;

      /*Determiner la bonne tuile*/
      y=floor(Lat/GDB->DegT);
      x=floor(Lon/GDB->DegT);

      t=GDB->Tile[x][y].Topo.Map;

      if (!t) {
         m=0;
      } else {

         /*Determiner l'index dans la tuile*/
         w=GDB->Tile[x][y].Topo.Width;
         h=GDB->Tile[x][y].Topo.Height;

         y=(-(fmod(Lat,GDB->DegT)-GDB->DegT)*(h-2)/GDB->DegT)+1;
         x=fmod(Lon,GDB->DegT)*(w-2)/GDB->DegT+1;
         m=t[y*w+x];
      }
   }
   /*Recuperer l'altitude*/
   return (m);
}

/*----------------------------------------------------------------------------
 * Nom      : <GDB_TileClear>
 * Creation : Octobre 2004 - J.P. Gauthier - CMC/CMOE
 *
 * But      : Effacer une tuile avec des parametres de couleur et stencil specifique.
 *
 * Parametres:
 *   <Proj>   : Parametres de projection
 *   <Tile>   : Tuile de donnees geographiques
 *   <Color>  : Couleur des segments
 *   <MaskIn> : Masque du Stencil (Inside)
 *
 * Retour:
 *
 * Remarques :
 *
 *----------------------------------------------------------------------------
*/
void GDB_TileClear(Projection *Proj,GDB_Tile *Tile,XColor *Color,GLuint MaskIn) {

   glDisable(GL_DEPTH_TEST);
   glPolygonMode(GL_FRONT_AND_BACK,GL_FILL);

   if (Color) {
      glColor3us(Color->red,Color->green,Color->blue);
   } else {
      glColorMask(GL_FALSE,GL_FALSE,GL_FALSE,GL_FALSE);
   }

   if (MaskIn!=0xff) {
      glEnable(GL_STENCIL_TEST);
      glStencilFunc(GL_ALWAYS,MaskIn,0xf);
      glStencilOp(GL_REPLACE,GL_REPLACE,GL_REPLACE);
   } else {
      glDisable(GL_STENCIL_TEST);
   }

   Proj->Type->Render(Proj,0,Tile->Box.Vr,NULL,NULL,NULL,GL_QUADS,4,0,Tile->Box.Vr[0],Tile->Box.Vr[2]);
}

/*----------------------------------------------------------------------------
 * Nom      : <GDB_TileFreeAll>
 * Creation : Novembre 2001 - J.P. Gauthier - CMC/CMOE
 *
 * But      : Liberer toute les tuiles de vecteur geographiques.
 *
 * Parametres:
 *   <GDB>   : Structure GDB
 *   <Type>  : Type de liberation (-1=Raster,0=Tout,1=Force)
 *
 * Retour:
 *
 * Remarques :
 *
 *----------------------------------------------------------------------------
*/
void GDB_TileFreeAll(GDB_Data *GDB,int Type) {

   int x,y;

   GDB_ThreadQueueClear(0x0);

   for(x=0;x<GDB->DegX;x++) {
      for(y=0;y<GDB->DegY;y++) {
         GDB_TileFree(&GDB->Tile[x][y],Type);
      }
   }
}

/*----------------------------------------------------------------------------
 * Nom      : <GDB_TileFree>
 * Creation : Novembre 2001 - J.P. Gauthier - CMC/CMOE
 *
 * But      : Liberer une tuiles de vecteur geographiques.
 *
 * Parametres:
 *   <Tile>   : Tuile de donnees
 *   <Force>  : Forcer la liberation
 *
 * Retour:
 *
 * Remarques :
 *
 *----------------------------------------------------------------------------
*/
void GDB_TileFree(GDB_Tile *Tile,int Force) {

   Tcl_MutexLock(&MUTEX_GDBDATA);

   if (Force==GDB_FORCE) {
      Tile->Box.Nb=0;
   }
   Tile->Res=0;

   /*Donnees matricielles*/
   if (Tile->Topo.Tex) {
      glDeleteTextures(1,&Tile->Topo.Tex);
      Tile->Topo.Tex=0;
   }
   if (Tile->Topo.Map) {
      free(Tile->Topo.Map);
      Tile->Topo.Map=NULL;
   }
   if (Tile->Topo.Vr) {
      free(Tile->Topo.Vr);
      Tile->Topo.Vr=NULL;
   }

   if (Force!=GDB_RASTER) {

      /*Donnees vectorielles*/
      if (Tile->Coast) {
         GDB_GeoFree(Tile->Coast);
         Tile->Coast=NULL;
      }
      if (Tile->FCoast) {
         GDB_GeoFree(Tile->FCoast);
         Tile->FCoast=NULL;
      }
      if (Tile->FCoastIn) {
         GDB_GeoFree(Tile->FCoastIn);
         Tile->FCoastIn=NULL;
      }
      if (Tile->Lake) {
         GDB_GeoFree(Tile->Lake);
         Tile->Lake=NULL;
      }
      if (Tile->FLake) {
         GDB_GeoFree(Tile->FLake);
         Tile->FLake=NULL;
      }
      if (Tile->FLakeIn) {
         GDB_GeoFree(Tile->FLakeIn);
         Tile->FLakeIn=NULL;
      }

      if (Tile->River) {
         GDB_GeoFree(Tile->River);
         Tile->River=NULL;
      }
      if (Tile->Polit) {
         GDB_GeoFree(Tile->Polit);
         Tile->Polit=NULL;
      }
      if (Tile->Admin) {
         GDB_GeoFree(Tile->Admin);
         Tile->Admin=NULL;
      }
      if (Tile->Road) {
         GDB_GeoFree(Tile->Road);
         Tile->Road=NULL;
      }
      if (Tile->Rail) {
         GDB_GeoFree(Tile->Rail);
         Tile->Rail=NULL;
      }

      /*Donnees textuelles*/
      if (Tile->TPlace) {
         if (Tile->TPlace>(GDB_Txt*)0x1) GDB_TxtFree(Tile->TPlace);
         Tile->TPlace=NULL;
      }
      if (Tile->TCity) {
         if (Tile->TCity>(GDB_Txt*)0x1) GDB_TxtFree(Tile->TCity);
         Tile->TCity=NULL;
      }
   }
   Tcl_MutexUnlock(&MUTEX_GDBDATA);
}

/*----------------------------------------------------------------------------
 * Nom      : <GDB_TileFreeType>
 * Creation : Aout 2002 - J.P. Gauthier - CMC/CMOE
 *
 * But      : Liberer un type de donnees pour toutes les tuiles.
 *
 * Parametres:
 *   <GDB>   : Structure GDB
 *   <Type>  : Type de donnees
 *
 * Retour:
 *
 * Remarques :
 *
 *----------------------------------------------------------------------------
*/
void GDB_TileFreeType(GDB_Data *GDB,GDB_Type Type) {

   int x,y;
   GDB_Tile *tile;

   Tcl_MutexLock(&MUTEX_GDBDATA);

   for(x=0;x<GDB->DegX;x++) {
      for(y=0;y<GDB->DegY;y++) {
         tile=&GDB->Tile[x][y];

         switch(Type) {
            case GDB_TYPE_MAP  : if (tile->Topo.Map) { free(tile->Topo.Map); tile->Topo.Map=NULL; }
                                 if (tile->Topo.Vr) { free(tile->Topo.Vr); tile->Topo.Vr=NULL; }
                                 break;
            case GDB_TYPE_TEXT : if (tile->Topo.Tex) { glDeleteTextures(1,&tile->Topo.Tex); tile->Topo.Tex=0; }
                                 break;
            case GDB_TYPE_COAST: if (tile->Coast)    { GDB_GeoFree(tile->Coast); tile->Coast=NULL; }
                                 if (tile->FCoast)   { GDB_GeoFree(tile->FCoast); tile->FCoast=NULL; }
                                 if (tile->FCoastIn) { GDB_GeoFree(tile->FCoastIn); tile->FCoastIn=NULL; }
                                 break;
            case GDB_TYPE_LAKE : if (tile->Lake)    { GDB_GeoFree(tile->Lake); tile->Lake=NULL; }
                                 if (tile->FLake)   { GDB_GeoFree(tile->FLake); tile->FLake=NULL; }
                                 if (tile->FLakeIn) { GDB_GeoFree(tile->FLakeIn); tile->FLakeIn=NULL; }
                                 break;
            case GDB_TYPE_RIVER: if (tile->River) { GDB_GeoFree(tile->River); tile->River=NULL; }
                                 break;
            case GDB_TYPE_POLIT: if (tile->Polit) { GDB_GeoFree(tile->Polit); tile->Polit=NULL; }
                                 break;
            case GDB_TYPE_ADMIN: if (tile->Admin) { GDB_GeoFree(tile->Admin); tile->Admin=NULL; }
                                 break;
            case GDB_TYPE_CITY:  if (tile->TCity) { if (tile->TCity>(GDB_Txt*)0x1 )GDB_TxtFree(tile->TCity); tile->TCity=NULL; }
                                 break;
            case GDB_TYPE_PLACE: if (tile->TPlace) { if (tile->TPlace>(GDB_Txt*)0x1 )GDB_TxtFree(tile->TPlace); tile->TPlace=NULL; }
                                 break;
            case GDB_TYPE_ROAD:  if (tile->Road) { GDB_GeoFree(tile->Road); tile->Road=NULL; }
                                 break;
            case GDB_TYPE_RAIL:  if (tile->Rail) { GDB_GeoFree(tile->Rail); tile->Rail=NULL; }
                                 break;
         }
      }
   }
   Tcl_MutexUnlock(&MUTEX_GDBDATA);
}

/*----------------------------------------------------------------------------
 * Nom      : <GDB_GeoTess>
 * Creation : Aout 2002 - J.P. Gauthier - CMC/CMOE
 *
 * But      : Generer les polygones de geographie par tesselation.
 *
 * Parametres:
 *  <Tcl_Interp> : Interpreteur Tcl
 *  <Geo>        : Tuile de donnees
 *
 * Retour:
 *
 * Remarques :
 *
 *----------------------------------------------------------------------------
*/
void GDB_GeoTess(Tcl_Interp *Interp,GDB_Geo *Geo) {

   unsigned int n;

   if (GLRender->GLTess) {

      if (Geo->Box.Nb>2) {

         if (Interp) {
            glFeedbackInit(Geo->Box.Nb*20,GL_2D);
         } else {
            Geo->List=glGenLists(1);
            glNewList(Geo->List,GL_COMPILE);
         }

         gluTessBeginPolygon(GLRender->GLTess,NULL);
         gluTessBeginContour(GLRender->GLTess);

         for(n=0;n<Geo->Box.Nb;n++){
            gluTessVertex(GLRender->GLTess,Geo->Loc[n],Geo->Loc[n]);
         }

         gluTessEndContour(GLRender->GLTess);
         gluTessEndPolygon(GLRender->GLTess);

         if (Interp) {
            glFeedbackProcess(Interp,GL_2D);
         } else {
            glEndList();
         }
      }
   } else {
      fprintf(stdout,"(WARNING) GDB_GeoTess: Unable to obtain valid tesselator\n");
   }
}

/*----------------------------------------------------------------------------
 * Nom      : <GDB_TileGet>
 * Creation : Novembre 2001 - J.P. Gauthier - CMC/CMOE
 *
 * But      : Recuperer l'information geographique pour une tuile.
 *
 * Parametres :

 *   <Tile>   : Tuile
 *   <Proj>   : Parametres de projection
 *   <Type>   : Format de donnees a recuperer
 *   <Data>   : Type de donnees a recuperer
 *
 * Retour:
 *
 * Remarques :
 *   -On recupere les donnees en latlon et on les projete immediatement dans
 *    le referentiel de la projection courante.
 *
 *----------------------------------------------------------------------------
*/
int GDB_TileGet(void *Tile,Projection *Proj,int Type,int Data) {

   GDB_Tile *tile=(GDB_Tile*)Tile;
   short    *tmp;
   int       i;
   float     over;

   GeoPtr=NULL;
   TxtPtr=NULL;

   if (!tile->Res || tile->Box.Nb<1) {
      Proj->Loading--;
      return(0);
   }

   switch(Type) {
      case GDB_LIN:
         gdb_limit(tile->Box.Co[0].Lat,tile->Box.Co[0].Lon,tile->Box.Co[2].Lat,tile->Box.Co[2].Lon);
         gdb_line(tile->Res,Data,GDB_GeoGetVector);
         if (!GeoPtr) {
            GDB_GeoGetVector(Data,0,0.0,0.0,0.0,0.0,NULL);
         } else {
            GDB_GeoProj(GeoPtr,Proj);
         }
         switch(Data) {
            case GDB_LIN_COAST: tile->Coast=GeoPtr; break;
            case GDB_LIN_LAKE : tile->Lake=GeoPtr;  break;
            case GDB_LIN_RIVER: tile->River=GeoPtr; break;
            case GDB_LIN_POLIT: tile->Polit=GeoPtr; break;
            case GDB_LIN_ADMIN: tile->Admin=GeoPtr; break;
            case GDB_LIN_ROAD : tile->Road=GeoPtr;  break;
            case GDB_LIN_RAIL : tile->Rail=GeoPtr;  break;
         }
         break;

      case GDB_FIL:
         gdb_limit(tile->Box.Co[0].Lat,tile->Box.Co[0].Lon,tile->Box.Co[2].Lat,tile->Box.Co[2].Lon);
         gdb_fill(tile->Res,Data,GDB_GeoGetVector);
         if (!GeoPtr) {
            GDB_GeoGetVector(Data,0,0.0,0.0,0.0,0.0,NULL);
         } else {
            GDB_GeoProj(GeoPtr,Proj);
         }
         switch(Data) {
            case GDB_FIL_LAND:  tile->FCoast=GeoPtr; break;
            case GDB_FIL_LAKE:  tile->FLake=GeoPtr; break;
            case GDB_FIL_LAND2: tile->FCoastIn=GeoPtr; break;
            case GDB_FIL_LAKE2: tile->FLakeIn=GeoPtr; break;
         }
         break;

      case GDB_TXT:
         gdb_limit(tile->Box.Co[0].Lat,tile->Box.Co[0].Lon,tile->Box.Co[2].Lat,tile->Box.Co[2].Lon);
         gdb_text(tile->Res<16?16:tile->Res,Data,GDB_TxtGet);
         switch(Data) {
            case GDB_TXT_POLIT: tile->TPlace=TxtPtr?TxtPtr:(GDB_Txt*)0x1 ; break;
            case GDB_TXT_AIRP: tile->TPlace=TxtPtr?TxtPtr:(GDB_Txt*)0x1 ; break;
            case GDB_TXT_CITY : tile->TCity=TxtPtr?TxtPtr:(GDB_Txt*)0x1  ; break;
         }
         break;

      case GDB_MAP:
         over=1.0/MAX(1,tile->Res);
         gdb_limit(tile->Box.Co[0].Lat-over,tile->Box.Co[0].Lon-over,tile->Box.Co[2].Lat+over,tile->Box.Co[2].Lon+over);
         switch(Data) {
//            case GDB_MAP_TEX: gdb_map(MAX(1,tile->Res),GDB_MAP_BAT,(char**)&tile->Topo.Map,&tile->Topo.Width,&tile->Topo.Height,&tile->Topo.Size); break;
            case GDB_MAP_DEM: gdb_map(MAX(1,tile->Res),GDB_MAP_DEM,(char**)&tile->Topo.Map,&tile->Topo.Width,&tile->Topo.Height,&tile->Topo.Size); break;
            case GDB_MAP_BAT: gdb_map(MAX(1,tile->Res),GDB_MAP_BAT,(char**)&tile->Topo.Map,&tile->Topo.Width,&tile->Topo.Height,&tile->Topo.Size); break;
            case GDB_MAP_MSK: gdb_map(MAX(1,tile->Res),GDB_MAP_DEM,(char**)&tile->Topo.Map,&tile->Topo.Width,&tile->Topo.Height,&tile->Topo.Size);
                              gdb_map(MAX(1,tile->Res),GDB_MAP_BAT,(char**)&tmp,&tile->Topo.Width,&tile->Topo.Height,&tile->Topo.Size);
                              i=0;
                              while (i<tile->Topo.Height*tile->Topo.Width) {
                                 tile->Topo.Map[i]=tmp[i]<0?tmp[i]:tile->Topo.Map[i];
                                 i++;
         }
                              free(tmp);
         break;
      }
   }
   Proj->Loading--;

   return(1);
}

/*----------------------------------------------------------------------------
 * Nom      : <GDB_TileInit>
 * Creation : Novembre 2001 - J.P. Gauthier - CMC/CMOE
 *
 * But      : Initialiser une tuile geographique.
 *
 * Parametres:
 *   <Tile>  : Tuile a initialiser
 *   <Lat0>  : Coordonnee latitude du coin superieur gauche
 *   <Lon0>  : Coordonnee longitude du coin superieur gauche
 *   <Proj>  : Parametres de projection
 *
 * Retour:
 *
 * Remarques :
 *
 *----------------------------------------------------------------------------
*/
void GDB_TileInit(GDB_Tile *Tile,float Lat0,float Lon0,float Delta,Projection *Proj) {

   Tile->Box.Co[0].Lat=Lat0;Tile->Box.Co[0].Lon=Lon0;Tile->Box.Co[0].Elev=0.0;
   Tile->Box.Co[1].Lat=Lat0;Tile->Box.Co[1].Lon=Lon0+Delta;Tile->Box.Co[1].Elev=0.0;
   Tile->Box.Co[2].Lat=Lat0+Delta;Tile->Box.Co[2].Lon=Lon0+Delta;Tile->Box.Co[2].Elev=0.0;
   Tile->Box.Co[3].Lat=Lat0+Delta;Tile->Box.Co[3].Lon=Lon0;Tile->Box.Co[3].Elev=0.0;

   if (!(Proj->Type->Project(Proj,(GeoVect*)Tile->Box.Co,(GeoVect*)Tile->Box.Vr,4))) {
      Tile->Box.Nb=-1;
   } else {
      Tile->Box.Nb=1;
   }
   Tile->Res=0;
}

/*----------------------------------------------------------------------------
 * Nom      : <GDB_Loc>
 * Creation : Novembre 2001 - J.P. Gauthier - CMC/CMOE
 *
 * But      : Localise une boite et determine sa visibilitee.
 *
 * Parametres:
 *   <Box>   : Parametres de la boite
 *   <X0>    : Limite minimale en X
 *   <X1>    : Limite maximale en X
 *   <Y0>    : Limite minimale en Y
 *   <Y1>    : Limite maximale en Y
 *
 * Retour:
 *
 * Remarques :
 *
 *----------------------------------------------------------------------------
*/
int GDB_Loc(GDB_Box Box,Projection *Proj,float X0,float X1,float Y0,float Y1){

   Vect3d dif,min,max;
   double d,lat[2],lon[2];

   if (Box.Nb<=0)
      return(GDB_OUT);

   /*Test for inclusion of focal point*/
   lat[0]=FMIN(Box.Co[0].Lat,Box.Co[2].Lat);
   lat[1]=FMAX(Box.Co[0].Lat,Box.Co[2].Lat);
   lon[0]=FMIN(Box.Co[0].Lon,Box.Co[2].Lon);
   lon[1]=FMAX(Box.Co[0].Lon,Box.Co[2].Lon);

   if (Proj->Lat>=lat[0] && Proj->Lat<=lat[1] && Proj->Lon>=lon[0] && Proj->Lon<=lon[1]) {
      return(GDB_VIS);
   }

   /*Test for projected coordinates*/
   if (Proj->Type->Def==PROJCYLIN) {
      d=Box.Vr[0][0]-Proj->L;
      CYLCHECK(d,Box.Vr[0][0]);
      d=Box.Vr[1][0]-Proj->L;
      CYLCHECK(d,Box.Vr[1][0]);
      d=Box.Vr[2][0]-Proj->L;
      CYLCHECK(d,Box.Vr[2][0]);
      d=Box.Vr[3][0]-Proj->L;
      CYLCHECK(d,Box.Vr[3][0]);
   }

   /*Project box coordinates*/
   gluProject(Box.Vr[0][0],Box.Vr[0][1],Box.Vr[0][2],Proj->VP->GLModR,Proj->VP->GLProj,Proj->VP->GLView,&dif[0],&dif[1],&dif[2]);
   Vect_Assign(min,dif);
   Vect_Assign(max,dif);
   gluProject(Box.Vr[1][0],Box.Vr[1][1],Box.Vr[1][2],Proj->VP->GLModR,Proj->VP->GLProj,Proj->VP->GLView,&dif[0],&dif[1],&dif[2]);
   Vect_Min(min,min,dif);
   Vect_Max(max,max,dif);
   gluProject(Box.Vr[2][0],Box.Vr[2][1],Box.Vr[2][2],Proj->VP->GLModR,Proj->VP->GLProj,Proj->VP->GLView,&dif[0],&dif[1],&dif[2]);
   Vect_Min(min,min,dif);
   Vect_Max(max,max,dif);
   gluProject(Box.Vr[3][0],Box.Vr[3][1],Box.Vr[3][2],Proj->VP->GLModR,Proj->VP->GLProj,Proj->VP->GLView,&dif[0],&dif[1],&dif[2]);
   Vect_Min(min,min,dif);
   Vect_Max(max,max,dif);

   /*Is it visible (in X,Y and Z)???*/
   if (!VOUT(min[0],max[0],X0,X1) && !VOUT(min[1],max[1],Y0,Y1) && min[2]<Proj->ZPos[2]) {

      /*Is the box too small*/
      if (Vect_Weight(min,max)<Proj->MinSize) {
         return(GDB_LOW);
      } else {
         return(GDB_VIS);
      }
   } else {
      return(GDB_OUT);
   }
}

/*----------------------------------------------------------------------------
 * Nom      : <GDB_CoordRender>
 * Creation : Octobre 1998 - J.P. Gauthier - CMC/CMOE
 *
 * But      : Effectue l'affichage des identifications des latitudes
 *            sur le cote gauche et en haut de la projection.
 *
 * Parametres    :
 *  <Tcl_Interp> : Interpreteur Tcl
 *  <VP>         : Parametres du viewport
 *  <Proj>       : Parametres de la projection
 *   <Geo>   : Liste des boites de donnees geographiques
 *
 * Retour:
 *  <TCL_...>    : Code de retour standard TCL
 *
 * Remarques :
 *
 *----------------------------------------------------------------------------
*/
void GDB_CoordRender(Tcl_Interp *Interp,ViewportItem *VP,Projection *Proj,GDB_Data *GDB) {

   Coord  coord;
   Vect3d pix;
   int    txtw,i;
   char   buf[16];
   double old=-1.0,cmod,tolerance;

   Tk_FontMetrics tkm;

   Projection_UnClip(Proj);

   glMatrixMode(GL_MODELVIEW);
   glPushMatrix();
   glLoadIdentity();

   glMatrixMode(GL_PROJECTION);
   glPushMatrix();
   glLoadIdentity();
   gluOrtho2D(0,VP->Width,0,VP->Height);

   if (Interp) {
      Tk_CanvasPsColor(Interp,VP->canvas,VP->ColorCoord);
   } else {
      glColor3us(VP->ColorCoord->red,VP->ColorCoord->green,VP->ColorCoord->blue);
      glFontUse(Tk_Display(Tk_CanvasTkwin(VP->canvas)),VP->tkfont);
   }

   Tk_GetFontMetrics(VP->tkfont,&tkm);
   tolerance=200.0/(VP->Height/VP->Cam->Aspect);

   pix[0]=2;
   for(i=0;i<=VP->Height;i++) {

      pix[1]=i;
      if (Proj->Type->UnProject(VP,Proj,&coord,pix)) {
        cmod=fmod(coord.Lat,GDB->Params.CoordNum*GDB->Params.CoordDef);
         if (ABS(cmod)<tolerance) {

            coord.Lat-=cmod;
            if (old!=coord.Lat) {

               old=coord.Lat;

               if (GDB->Params.CoordDef<1.0) {
                  sprintf(buf,"%0.2f",ABS(coord.Lat));
               } else {
                  sprintf(buf,"%i",(int)ABS(coord.Lat));
               }

               if (coord.Lat!=0.0) {
                  if (coord.Lat>0.0) {
                     strcat(buf,"N");
                  } else {
                     strcat(buf,"S");
                  }
               }

               glPrint(Interp,VP->canvas,buf,3,VP->Height-(i+tkm.linespace/2),0);
               i+=tkm.linespace;
            }
         }
      }
   }

   tolerance=400.0/(VP->Width/VP->Cam->Aspect);
   pix[1]=VP->Height-2;

   for(i=0;i<=VP->Width;i++) {

      pix[0]=i;
      if (Proj->Type->UnProject(VP,Proj,&coord,pix)) {
         cmod=fmod(coord.Lon,GDB->Params.CoordNum*GDB->Params.CoordDef);
         if (ABS(cmod)<tolerance) {

            coord.Lon-=cmod;
            if (old!=coord.Lon) {

               old=coord.Lon;
               coord.Lon=CLAMPLON(coord.Lon);

               if (GDB->Params.CoordDef<1.0) {
                  sprintf(buf,"%0.2f",ABS(coord.Lon));
               } else {
                  sprintf(buf,"%i",(int)ABS(coord.Lon));
               }

               if (coord.Lon!=0.0 && ABS(coord.Lon)!=180.0) {
                  if (coord.Lon>0.0) {
                     strcat(buf,"E");
                  } else {
                     strcat(buf,"W");
                  }
               }

               txtw=Tk_TextWidth(VP->tkfont,buf,strlen(buf));
               glPrint(Interp,VP->canvas,buf,i-txtw/2,3,0);
               i+=txtw;
            }
         }
      }
   }

   glPopMatrix();
   glMatrixMode(GL_MODELVIEW);
   glPopMatrix();

   Projection_Clip(Proj);
}

/*----------------------------------------------------------------------------
 * Nom      : <GDB_GeoRender>
 * Creation : Novembre 2001 - J.P. Gauthier - CMC/CMOE
 *
 * But      : Effectuer le rendue de boite de donnees geographiques
 *
 * Parametres:
 *   <Interp>: Interpreteur TCL
 *   <Proj>  : Parametres de projection
 *   <Geo>   : Liste des boites de donnees geographiques
 *   <Width> : Largeur des segments
 *   <Color> : Couleur des segments
 *   <Low>   : Check for low resolution
 *
 * Retour:
 *
 * Remarques :
 *   -Si Interp n'est pas NULL, on produit du postscript
 *
 *----------------------------------------------------------------------------
*/
void GDB_GeoRender(Tcl_Interp *Interp,Projection *Proj,GDB_Geo *Geo,int Width,XColor *Color,int Low) {

   int  state;
   char buf[64];

   if (!Color || !Width)
      return;

   if (Interp) {
      Tk_CanvasPsColor(Interp,Proj->VP->canvas,Color);
      sprintf(buf,"%i setlinewidth 1 setlinecap 1 setlinejoin\n",Width-1);
      Tcl_AppendResult(Interp,buf,(char*)NULL);
   } else {
      glColor3us(Color->red,Color->green,Color->blue);
      glLineWidth(Width);
   }

   glEnableClientState(GL_VERTEX_ARRAY);
   while (Geo) {

      if (Geo->Box.Nb>1) {

         if (Interp)
            glFeedbackInit(Geo->Box.Nb*8,GL_2D);

         state=GDB_Loc(Geo->Box,Proj,1,Proj->VP->Width,1,Proj->VP->Height);
         if ((state==GDB_LOW && !Low) || state==GDB_VIS || Proj->Type->Def==PROJCYLIN) {
            Proj->Type->Render(Proj,0,Geo->Loc,NULL,NULL,NULL,GL_LINE_STRIP,Geo->Box.Nb,0,Geo->Box.Vr[0],Geo->Box.Vr[2]);
         }

         if (Interp)
            glFeedbackProcess(Interp,GL_2D);
      }
      Geo=Geo->Next;
   }
   glDisableClientState(GL_VERTEX_ARRAY);
}

/*----------------------------------------------------------------------------
 * Nom      : <GDB_FillRender>
 * Creation : Novembre 2001 - J.P. Gauthier - CMC/CMOE
 *
 * But      : Effectuer le rendue de boite de donnees geographiques
 *
 * Parametres :
 *   <Interp> : Interpreteur TCL
 *   <Proj>   : Parametres de projection
 *   <Geo>    : Liste des boites de donnees geographiques
 *   <Color>  : Couleur des segments
 *   <MaskIn> : Masque du Stencil (Inside)
 *
 * Retour:   int   c00,c01,c11,c10;

 *
 * Remarques :
 *   -Si Interp n'est pas NULL, on produit du postscript
 *
 *----------------------------------------------------------------------------
*/

void GDB_FillRender(Tcl_Interp *Interp,Projection *Proj,GDB_Geo *Geo,XColor *Color,GLuint MaskIn) {

   int     state,n;

   glDisable(GL_DEPTH_TEST);
   glDisable(GL_CULL_FACE);
   glPolygonMode(GL_FRONT_AND_BACK,GL_FILL);

   if (Color) {
      glColor3us(Color->red,Color->green,Color->blue);
   } else {
      glColorMask(GL_FALSE,GL_FALSE,GL_FALSE,GL_FALSE);
   }
   glEnable(GL_STENCIL_TEST);

   if (MaskIn==0x0) {
      /*Clear before mask*/
      glStencilFunc(GL_EQUAL,0x2,0xf);
      glStencilOp(GL_KEEP,GL_ZERO,GL_ZERO);
   } else if (MaskIn==0x2) {
      /*Mask */
      glStencilFunc(GL_ALWAYS,0x2,0xf);
      glStencilOp(GL_REPLACE,GL_REPLACE,GL_REPLACE);
   } else {
      /*No mask*/
      glStencilFunc(GL_NOTEQUAL,0x1,0xf);
      glStencilOp(GL_KEEP,GL_KEEP,GL_KEEP);
   }

   if (Interp) {
      if (Color) {
         Tk_CanvasPsColor(Interp,Proj->VP->canvas,Color);
      }
      Tcl_AppendResult(Interp,"0.5 setlinewidth 0 setlinecap 0 setlinejoin\n",(char*)NULL);
      while (Geo) {
         if (Geo->Box.Nb>2) {
            state=GDB_Loc(Geo->Box,Proj,1,Proj->VP->Width,1,Proj->VP->Height);
            if (state==GDB_VIS || Proj->Type->Def==PROJCYLIN) {
               GDB_GeoTess(Interp,Geo);
            }
         }
         Geo=Geo->Next;
      }
   } else {
      n=0;
      while (Geo) {
         if (Geo->Box.Nb>2) {
            state=GDB_Loc(Geo->Box,Proj,1,Proj->VP->Width,1,Proj->VP->Height);
            if (state==GDB_VIS || Proj->Type->Def==PROJCYLIN) {
               if (!Geo->List) {
                  GDB_GeoTess(Interp,Geo);
                  n++;
               }
               Proj->Type->Render(Proj,Geo->List,NULL,NULL,NULL,NULL,0,0,0,Geo->Box.Vr[0],Geo->Box.Vr[2]);
            }
         }
         Geo=Geo->Next;

         /*After some tesselation, give back control to application*/
         if (n>10) {
            ViewportRefresh_Canvas(Proj->VP->canvas);
            break;
         }
      }
   }

   if (MaskIn==0xff) {
      glStencilFunc(GL_ALWAYS,0x1,0x1);
      glStencilOp(GL_REPLACE,GL_REPLACE,GL_REPLACE);
   } else {
      glStencilFunc(GL_EQUAL,0x0,0xf);
      glStencilOp(GL_KEEP,GL_KEEP,GL_KEEP);
   }

   glColorMask(GL_TRUE,GL_TRUE,GL_TRUE,GL_TRUE);
   glEnable(GL_DEPTH_TEST);
}

/*----------------------------------------------------------------------------
 * Nom      : <GDB_MapRender>
 * Creation : Novembre 2001 - J.P. Gauthier - CMC/CMOE
 *
 * But      : Effectuer le rendue de boite de donnees geographiques
 *
 * Parametres:
 *   <Proj>  : Parametres de projection
 *   <Map>   : Liste des elevations
 *   <Lat0>  : Latitute du coin inferieur gauche
 *   <Lon0>  : Longitude du coin inferieur gauche
 *
 * Retour:
 *
 * Remarques :
 *   -Si Interp n'est pas NULL, on produit du postscript
 *   -Les map on une bordure de 1 pour calculer les normales correctement aux limites des
 *    tuiles donc, il faut modifier les indexes pour etre localise correctement
 *
 *----------------------------------------------------------------------------
*/
void GDB_MapRender(Projection *Proj,GDB_Map *Topo,float Lat0,float Lon0,float Delta) {

   Coord   loc;
   Vect3d *vbuf;
   int     dc,dr,dt=1;
   float   dx,dy;
   float   dtx,dty,tx,ty;
   float   d;

   register int     x,y;
   register short  *map=Topo->Map;
   int              width=Topo->Width;
   int              height=Topo->Height;

   GLfloat emi[4]  = { 0.012, 0.012, 0.012, 1.0 };
   GLfloat amb[4]  = { 0.97, 0.97, 0.97, 1.0 };
   GLfloat spe[4]  = { 1.0, 1.0, 1.0, 1.0 };
   GLfloat dif[4]  = { 0.9, 0.9, 0.9, 1.0 };
   GLfloat shi     = 100.0;

   if (GLRender->Resolution>2) {
      dt=2;
   }

   glNormal3dv(Proj->Nr);

   glMaterialf(GL_FRONT,GL_SHININESS,shi);
   glMaterialfv(GL_FRONT,GL_AMBIENT,amb);
   glMaterialfv(GL_FRONT,GL_DIFFUSE,dif);
   glMaterialfv(GL_FRONT,GL_SPECULAR,spe);
   glMaterialfv(GL_FRONT,GL_EMISSION,emi);

   if (Topo->Tex!=-1) {
      glEnable(GL_TEXTURE_2D);
      glBindTexture(GL_TEXTURE_2D,Topo->Tex);
      glTexEnvf(GL_TEXTURE_ENV,GL_TEXTURE_ENV_MODE,GL_MODULATE);
   }

   if (!map) {

      /*On divise chaque tuile par 5 afin de creer une courbure*/
      dc=(Delta/5.0);
      dtx=1.0/(Delta/(Delta/5.0));
      dty=1.0/(Delta/(Delta/5.0));
      loc.Elev=0.0;
      loc.Lon=Lon0;

      vbuf=GDB_VBufferAlloc(Delta*dc);

      /*Construction de la surface en serie de quad dans la verticale*/
      for(tx=-dtx,x=0;x<=Delta;x+=dc,tx+=dtx) {

         if (Proj->Type->Def==PROJCYLIN) {
            d=CYLFLIP(Proj->L,vbuf[x][0]);
            glTranslated(d,0.0,0.0);
         }
         glBegin(GL_QUAD_STRIP);
         for(ty=1.0,y=Delta;y>=0;y-=dc,ty-=dty) {

            if (x) {
               glTexCoord2f(tx,ty);
               if (Proj->Sun) {
                  loc.Lat=Lat0+Delta-y;
                  loc.Lon=Lon0+x-dc;
                  glNormal3dv(GDB_NMap[(int)loc.Lat+90][(int)loc.Lon+180]);
               }
               glVertex3dv(vbuf[y]);
            }

            loc.Lat=Lat0+Delta-y;
            loc.Lon=Lon0+x;
            Proj->Type->Project(Proj,(GeoVect*)&loc,(GeoVect*)&vbuf[y],1);

            if (x) {
               glTexCoord2f(tx+dtx,ty);
               if (Proj->Sun) {
                  glNormal3dv(GDB_NMap[(int)loc.Lat+90][(int)loc.Lon+180]);
               }
               glVertex3dv(vbuf[y]);
            }
         }
         glEnd();
         if (Proj->Type->Def==PROJCYLIN) {
            glTranslated(-d,0.0,0.0);
         }
      }
   } else {

      vbuf=GDB_VBufferAlloc(height);

      /* Calculer la position de la tuile*/
      if (!Topo->Vr) {
         Topo->Vr=(Vect3d*)malloc((width-2)*(height-2)*sizeof(Vect3d));

         dx=(float)Delta/(width-3);
         dy=(float)Delta/(height-3);

         for(x=0;x<width-2;x++) {
            for(y=0;y<height-2;y++) {

               dc=(y+1)*width+(x+1);
               dr=y*(width-2)+x;

               loc.Lat=Lat0+Delta-(y*dy);
               loc.Lon=Lon0+(x*dx);
               loc.Elev=map[dc];
               Proj->Type->Project(Proj,(GeoVect*)&loc,(GeoVect*)&Topo->Vr[dr],1);
            }
         }
      }

      dtx=1.0/(width-dt-2);
      dty=1.0/(height-dt-2);

      /*Construction de la surface en serie de quad dans la verticale*/
      for(tx=0,x=1;x<=width-2;x+=dt,tx+=dtx*dt) {

         if (Proj->Type->Def==PROJCYLIN) {
            d=CYLFLIP(Proj->L,Topo->Vr[(x-1)][0]);
            glTranslated(d,0.0,0.0);
         }

         glBegin(GL_QUAD_STRIP);
         for(ty=1.0,y=height-2;y>=1;y-=dt,ty-=dty*dt) {

            dc=y*width+x;
            dr=(y-1)*(width-2)+(x-1);

            if (x>1) {
               if (Topo->Tex!=-1)
                  glTexCoord2f(tx,ty);
               else if (map[dc]<=0)
                  glColor3ub(0,0,128);
               else
                  glColor3ub(128,128,128);

               glNormal3dv(vbuf[y]);
               glVertex3dv(Topo->Vr[dr-dt]);
            }

            vbuf[y][0]=-(map[dc+1]-map[dc-1]);
            vbuf[y][1]=map[dc+width]-map[dc-width];
            vbuf[y][2]=ABS(map[dc]);

            if (Proj->Sun && x>1) {
               vbuf[y][2]=0.0;

               Vect_SMul(vbuf[y],vbuf[y],0.001);
               Vect_Add(vbuf[y],vbuf[y],Topo->Vr[dr-dt]);
            }
            if (vbuf[y][0]==0.0 && vbuf[y][1]==0.0 && vbuf[y][2]==0.0) {
               vbuf[y][2]=1.0;
            } else {
               Vect_Normalize(vbuf[y]);
            }

            if (x>1) {
               if (Topo->Tex!=-1)
                  glTexCoord2f(tx+dtx*dt,ty);
               else if (map[dc]<=0)
                  glColor3ub(0,0,128);
               else
                  glColor3ub(128,128,128);

               glNormal3dv(vbuf[y]);
               glVertex3dv(Topo->Vr[dr]);
            }
         }
         glEnd();
         if (Proj->Type->Def==PROJCYLIN) {
            glTranslated(-d,0.0,0.0);
         }
      }
   }
   glDisable(GL_TEXTURE_2D);
}

void GDB_MapRenderShader(Projection *Proj,GDB_Map *Topo,float Lat0,float Lon0,float Delta) {

   Coord  loc;
   int    dc,dr;
   float  dx,dy;
   float  dtx,dty,tx,ty;

   GLuint      tex[2];
   GLhandleARB prog;

   register int     x,y;
   register short  *map=Topo->Map;
   int              width=Topo->Width;
   int              height=Topo->Height;

   if (!Topo)
      return;

      /* Calculer la position de la tuile*/
      if (!Topo->Vr) {
         Topo->Vr=(Vect3d*)malloc((width-2)*(height-2)*sizeof(Vect3d));

         dx=(float)Delta/(width-3);
         dy=(float)Delta/(height-3);

         for(x=0;x<width-2;x++) {
            for(y=0;y<height-2;y++) {

               dc=(y+1)*width+(x+1);
               dr=y*(width-2)+x;

               loc.Lat=Lat0+Delta-(y*dy);
               loc.Lon=Lon0+(x*dx);
               loc.Elev=map[dc];
               Proj->Type->Project(Proj,(GeoVect*)&loc,(GeoVect*)&Topo->Vr[dr],1);
            }
         }
      }

      dtx=1.0/(width-3);
      dty=1.0/(height-3);

   prog=GLRender->Prog[PROG_TOPOTEX];
   glUseProgramObjectARB(prog);
   glGenTextures(1,tex);

   /*Setup 2D Data Texture*/
   glActiveTexture(GL_TEXTURE0);
   glBindTexture(GL_TEXTURE_RECTANGLE_ARB,tex[0]);
   glTexParameteri(GL_TEXTURE_RECTANGLE_ARB,GL_TEXTURE_MIN_FILTER,GL_NEAREST);
   glTexParameteri(GL_TEXTURE_RECTANGLE_ARB,GL_TEXTURE_MAG_FILTER,GL_NEAREST);
   glTexImage2D(GL_TEXTURE_RECTANGLE_ARB,0,GL_FLOAT_R16_NV,Topo->Width,Topo->Height,0,GL_RED,GL_SHORT,Topo->Map);

   glUniform1iARB(GLShader_UniformGet(prog,"Data"),0);
   glUniform1fARB(GLShader_UniformGet(prog,"Cylindric"),(Proj->Type->Def==PROJCYLIN?Proj->L:-999.0));

   glNormal3dv(Proj->Nr);
   glColor3ub(128,128,128);

   /*Construction de la surface en serie de quad dans la verticale*/
   for(tx=0.0,x=1;x<=width-2;x++,tx+=dtx) {

      glBegin(GL_QUAD_STRIP);
      for(ty=1.0,y=height-2;y>=1;y--,ty-=dty) {
         dr=(y-1)*(width-2)+(x-1);

         glTexCoord2i(x,y);
         glVertex3dv(Topo->Vr[dr-1]);
         glTexCoord2i(x,y-1);
         glVertex3dv(Topo->Vr[dr]);
      }
      glEnd();
   }

   glDeleteTextures(1,tex);
   glUseProgramObjectARB(0);
//   GLShader_UnInstall(prog);
}

/*----------------------------------------------------------------------------
 * Nom      : <GDB_TileGetData>
 * Creation : Novembre 2001 - J.P. Gauthier - CMC/CMOE
 *
 * But      : Effectuer le rendue des tuiles (Donnees vectorielles).
 *
 * Parametres:
 *   <Interp>: Interpreteur TCL
 *   <Proj>  : Parametres de projection
 *   <GDB>   : Structure GDB
 *   <Mode>  : Mode d'affichage (GDB_ALL,GDB_VECTOR,GDB_MASK,GDB_RASTER,GDB_TEXT)
 *
 * Retour:
 *
 * Remarques :
 *
 *----------------------------------------------------------------------------
*/

int GDB_TileGetData(GDB_Tile *Tile,GDB_Data *GDB,Projection *Proj) {

   if (Tile->Flag) {
      return(0);
   }

   if (GDB->Params.Mask || Proj->VP->ColorFCoast || Proj->VP->ColorFLake) {
      if (!Tile->FCoast)
         GDB_ThreadQueueAdd(0x0,Proj,Tile,GDB_TileGet,GDB_FIL,GDB_FIL_LAND);
      if (!Tile->FCoastIn)
         GDB_ThreadQueueAdd(0x0,Proj,Tile,GDB_TileGet,GDB_FIL,GDB_FIL_LAND2);
      if (!Tile->FLake)
         GDB_ThreadQueueAdd(0x0,Proj,Tile,GDB_TileGet,GDB_FIL,GDB_FIL_LAKE);
      if (!Tile->FLakeIn)
         GDB_ThreadQueueAdd(0x0,Proj,Tile,GDB_TileGet,GDB_FIL,GDB_FIL_LAKE2);
   }

   if (GDB->Params.Coast && !Tile->Coast)
      GDB_ThreadQueueAdd(0x0,Proj,Tile,GDB_TileGet,GDB_LIN,GDB_LIN_COAST);

   if (GDB->Params.Lake && !Tile->Lake)
      GDB_ThreadQueueAdd(0x0,Proj,Tile,GDB_TileGet,GDB_LIN,GDB_LIN_LAKE);

   if (GDB->Params.River && !Tile->River)
      GDB_ThreadQueueAdd(0x0,Proj,Tile,GDB_TileGet,GDB_LIN,GDB_LIN_RIVER);

   if (GDB->Params.Polit && !Tile->Polit)
      GDB_ThreadQueueAdd(0x0,Proj,Tile,GDB_TileGet,GDB_LIN,GDB_LIN_POLIT);

   if (GDB->Params.Admin && !Tile->Admin)
      GDB_ThreadQueueAdd(0x0,Proj,Tile,GDB_TileGet,GDB_LIN,GDB_LIN_ADMIN);

   if (GDB->Params.City && !Tile->TCity)
      GDB_ThreadQueueAdd(0x0,Proj,Tile,GDB_TileGet,GDB_TXT,GDB_TXT_CITY);

   if (GDB->Params.Place && !Tile->TPlace)
      GDB_ThreadQueueAdd(0x0,Proj,Tile,GDB_TileGet,GDB_TXT,GDB_TXT_POLIT);

   if (GDB->Params.Road && !Tile->Road)
      GDB_ThreadQueueAdd(0x0,Proj,Tile,GDB_TileGet,GDB_LIN,GDB_LIN_ROAD);

   if (GDB->Params.Rail && !Tile->Rail)
      GDB_ThreadQueueAdd(0x0,Proj,Tile,GDB_TileGet,GDB_LIN,GDB_LIN_RAIL);

   if (GDB->Params.Topo && !GDB->Params.Bath && !Tile->Topo.Map)
      GDB_ThreadQueueAdd(0x0,Proj,Tile,GDB_TileGet,GDB_MAP,GDB_MAP_DEM);

   if (!GDB->Params.Topo && GDB->Params.Bath && !Tile->Topo.Map)
      GDB_ThreadQueueAdd(0x0,Proj,Tile,GDB_TileGet,GDB_MAP,GDB_MAP_BAT);

   if (GDB->Params.Topo && GDB->Params.Bath && !Tile->Topo.Map)
      GDB_ThreadQueueAdd(0x0,Proj,Tile,GDB_TileGet,GDB_MAP,GDB_MAP_MSK);

   if (GDB->Params.Text && !Tile->Topo.Tex) {
      char  file[256];

      sprintf(file,"%s/ras/tex-%i.%i.%i.tif",getenv("GDB_PATH"),Tile->Res<4?4:(Tile->Res>64?64:Tile->Res),(int)(Tile->Box.Co[0].Lon+180.0)/GDB->DegT,(int)(Tile->Box.Co[0].Lat+90.0)/GDB->DegT);
      Tile->Topo.Tex=Texture_Read(file);
   }
   return(1);
}

int GDB_TileRender(Tcl_Interp *Interp,Projection *Proj,GDB_Data *GDB,int Mode) {

   GDB_Tile *tile;
   int       x,y,res=0,ras=0,ilat,ilon;
   float     lat,lon;

   if (GDB->Res<0) {
      return(0);
   }

   if ((Mode & GDB_RASTER) && (Proj->Geo->Params.Topo || Proj->Geo->Params.Bath || Proj->Geo->Params.Text)) {
      res=1;
   }

   if ((Mode & GDB_MASK) && Proj->Geo->Params.Mask) {
      res=1;
   }

   if ((Mode & GDB_VECTOR) || (Mode & GDB_FILL)) {
      res=1;
   }

   if (!res)
      return(0);

   res=GDB_TileResolution(GDB,Proj->VP->Cam->Pix);

   for(x=0;x<GDB->DegX;x++) {
      for(y=0;y<GDB->DegY;y++) {
         lat=y*GDB->DegT-90.0;
         lon=x*GDB->DegT-180.0;

         tile=&GDB->Tile[x][y];
         if (tile->Res!=res)
            GDB_TileFree(tile,GDB_FORCE);

         if (tile->Box.Nb==0)
            GDB_TileInit(tile,lat,lon,GDB->DegT,Proj);

         if (tile->Box.Nb==-1)
            continue;

         /*Check for visibility when not doing outputs*/
         if (GLRender->TRCon || GDB_Loc(tile->Box,Proj,1,Proj->VP->Width,1,Proj->VP->Height)!=GDB_OUT) {

            /*If resolution has changed en we're not doing outputs, reload data*/
            tile->Res=res;
            if (!GLRender->TRCon && GLRender->Resolution<=2) {
               GDB_TileGetData(tile,GDB,Proj);
            }

//            ilat=(int)((tile->Box.Co[0].Lat+tile->Box.Co[2].Lat)*0.5)+90;
//            ilon=(int)((tile->Box.Co[0].Lon+tile->Box.Co[2].Lon)*0.5)+180;
//            glNormal3dv(GDB_NMap[ilat][ilon]);

            if (Mode & GDB_MASK) {
               if (GDB->Params.Mask && tile->FCoast && tile->FLake) {
                  if (GDB->Params.Mask==GDB_LAND) {
//                     GDB_TileClear(Proj,tile,NULL,0x2);
                     GDB_FillRender(NULL,Proj,tile->FCoast,NULL,0x0);
                     GDB_FillRender(NULL,Proj,tile->FLake,NULL,0x2);
                  }
                  if (GDB->Params.Mask==GDB_SEA){
                     GDB_FillRender(NULL,Proj,tile->FCoast,NULL,0x2);
                     GDB_FillRender(NULL,Proj,tile->FLake,NULL,0x0);
                  }
               }
            }

            if (Mode & GDB_FILL) {
               if (Proj->VP->ColorFCoast && tile->FCoast) {
                  GDB_FillRender(Interp,Proj,tile->FCoast,Proj->VP->ColorFCoast,0xff);
               }
               if (Proj->VP->ColorFLake && tile->FLake) {
                  GDB_FillRender(Interp,Proj,tile->FLake,Proj->VP->ColorFLake,0xff);
               }
               if (Proj->VP->ColorFCoast && tile->FCoastIn) {
                  GDB_FillRender(Interp,Proj,tile->FCoastIn,Proj->VP->ColorFCoast,0xff);
               }
               if (Proj->VP->ColorFLake && tile->FLakeIn) {
                  GDB_FillRender(Interp,Proj,tile->FLakeIn,Proj->VP->ColorFLake,0xff);
               }
            }

            glDisable(GL_LIGHTING);
            glDisable(GL_LIGHT0);

            if (Mode & GDB_VECTOR) {
               if (GDB->Params.Coast && tile->Coast) {
                  GDB_GeoRender(Interp,Proj,tile->Coast,ABS(GDB->Params.Coast),Proj->VP->ColorCoast,1);
               }

               if (GDB->Params.Lake && tile->Lake) {
                  GDB_GeoRender(Interp,Proj,tile->Lake,ABS(GDB->Params.Lake),Proj->VP->ColorLake,1);
               }

               if (GDB->Params.River && tile->River) {
                  GDB_GeoRender(Interp,Proj,tile->River,ABS(GDB->Params.River),Proj->VP->ColorRiver,0);
               }

               if (GDB->Params.Polit && tile->Polit) {
                  GDB_GeoRender(Interp,Proj,tile->Polit,ABS(GDB->Params.Polit),Proj->VP->ColorPolit,0);
                }

               if (GDB->Params.Admin && tile->Admin) {
                  GDB_GeoRender(Interp,Proj,tile->Admin,ABS(GDB->Params.Admin),Proj->VP->ColorAdmin,0);
               }

               if (GDB->Params.Road && tile->Road) {
                  GDB_GeoRender(Interp,Proj,tile->Road,ABS(GDB->Params.Road),Proj->VP->ColorRoad,0);
               }

               if (GDB->Params.Rail && tile->Rail) {
                  GDB_GeoRender(Interp,Proj,tile->Rail,ABS(GDB->Params.Rail),Proj->VP->ColorRail,0);
               }

               if (GDB->Params.Place && tile->TPlace) {
                  GDB_TxtRender(Interp,Proj,tile->TPlace,Proj->VP->ColorPlace,0);
               }

               if (GDB->Params.City && tile->TCity) {
                  GDB_TxtRender(Interp,Proj,tile->TCity,Proj->VP->ColorCity,6);
               }
            }
            if (Proj->Sun) {
               glEnable(GL_LIGHTING);
               glEnable(GL_LIGHT0);
            }

            if ((Mode & GDB_RASTER) && (GDB->Params.Text || GDB->Params.Topo || GDB->Params.Bath)) {
               if (GLRender->GLDebug) {
                  glPolygonMode(GL_FRONT,GL_LINE);
               } else {
                  glPolygonMode(GL_FRONT,GL_FILL);
               }
               glEnable(GL_DEPTH_TEST);
               glEnable(GL_LIGHTING);
               glEnable(GL_LIGHT0);
               glDisable(GL_COLOR_MATERIAL);
               GDB_MapRender(Proj,&tile->Topo,lat,lon,GDB->DegT);
//               GDB_MapRenderShader(Proj,&tile->Topo,lat,lon,GDB->DegT);
               ras++;
               glEnable(GL_COLOR_MATERIAL);
               if (!Proj->Sun) {
                  glDisable(GL_LIGHTING);
                  glDisable(GL_LIGHT0);
               }
            }
         }
      }
   }

   if (Mode & GDB_TEXT) {
      if (GDB->Params.CoordLoc && GDB->Params.CoordNum) {
         GDB_CoordRender(Interp,Proj->VP,Proj,GDB);
      }
   }

   glDisable(GL_DEPTH_TEST);
   return(ras);
}

/*----------------------------------------------------------------------------
 * Nom      : <GDB_TileResolution>
 * Creation : Novembre 2001 - J.P. Gauthier - CMC/CMOE
 *
 * But      : Determiner la resolution effective d'une tuile.
 *
 * Parametres:
 *   <GDB>   : Structure GDB
 *   <Dist>  : Distance en metres pour un pixel.
 *
 * Retour:
 *
 * Remarques :
 *
 *----------------------------------------------------------------------------
*/
int GDB_TileResolution(GDB_Data *GDB,double Dist) {

   int res=0,tile;

   res=GDB->Res?GDB->Res:111120.0/Dist;

   if (res>=128) {
      res=128;
      tile=5;
      GDB->DegX=72;
      GDB->DegY=36;
   } else if (res>=64) {
      res=64;
      tile=5;
      GDB->DegX=72;
      GDB->DegY=36;
   } else if (res>=32) {
      res=32;
      tile=10;
      GDB->DegX=36;
      GDB->DegY=18;
   } else if (res>=16) {
      res=16;
      tile=10;
      GDB->DegX=36;
      GDB->DegY=18;
   } else if (res>=8) {
      res=8;
      tile=20;
      GDB->DegX=18;
      GDB->DegY=9;
   } else if (res>=4) {
      res=4;
      tile=20;
      GDB->DegX=18;
      GDB->DegY=9;
   } else {
      res=2;
      tile=20;
      GDB->DegX=18;
      GDB->DegY=9;
   }

   if (res>GDB_RES)
      res=GDB_RES;

   if (tile!=GDB->DegT) {
      GDB->DegT=tile;
      GDB_TileFreeAll(GDB,GDB_FORCE);
   }
#ifdef DEBUG
   fprintf(stdout,"(DEBUG) GDB_TileResolution: Current GDB resolution: (%i,%i)\n",GDB->DegT,res);
#endif
   return(res);
}
/*----------------------------------------------------------------------------
 * Nom      : <GDB_TxtFree>
 * Creation : Decembre 2001 - J.P. Gauthier - CMC/CMOE
 *
 * But      : Liberer une boite de textes.
 *
 * Parametres:
 *   <Txt>   : Boite de donnees
 *
 * Retour:
 *
 * Remarques :
 *
 *----------------------------------------------------------------------------
*/
void GDB_TxtFree(GDB_Txt *Txt) {

   GDB_Txt *tmp;

   while(Txt) {
      tmp=Txt;
      Txt=tmp->Next;
      free(tmp->String);
      free(tmp);
   }
}

/*----------------------------------------------------------------------------
 * Nom      : <GDB_TxtGet>
 * Creation : Decembre 2001 - J.P. Gauthier - CMC/CMOE
 *
 * But      : Recuperation de donnees textes.
 *
 * Parametres:
 *   <Type>  : Type de donnees textes
 *   <Lat>   : Latitude des donnees
 *   <Lon>   : Longitude des donnees
 *   <Txt>   : Texte
 *
 * Retour:
 *
 * Remarques :
 *
 *----------------------------------------------------------------------------
*/
void GDB_TxtGet(int Type,float Lat,float Lon,char *Txt) {

   GDB_Txt *txt;

   txt=(GDB_Txt*)malloc(sizeof(GDB_Txt));
   txt->Co.Lat=Lat;
   txt->Co.Lon=Lon;
   txt->Co.Elev=0.0;
   txt->String=strdup(Txt);

   txt->Next=TxtPtr;
   TxtPtr=txt;
}

/*----------------------------------------------------------------------------
 * Nom      : <GDB_TxtRender>
 * Creation : Decembre 2001 - J.P. Gauthier - CMC/CMOE
 *
 * But      : Effectuer le rendue de boite de donnees geographiques
 *
 * Parametres:
 *   <Interp>: Interpreteur TCL
 *   <Proj>  : Parametres de projection
 *   <Txt>   : Liste des boites de donnees geographiques
 *   <Color> : Couleur des segments
 *
 * Retour:
 *
 * Remarques :
 *   -Si Interp n'est pas NULL, on produit du postscript
 *
 *----------------------------------------------------------------------------
*/
void GDB_TxtRender(Tcl_Interp *Interp,Projection *Proj,GDB_Txt *Txt,XColor *Color,int Point) {

   Tk_FontMetrics tkm;
   Vect3d         pos,pix;
   int            len,dx,dy,x,y;

   /*Translation deltas related to canvas/viewport location*/
   x=(Proj->VP->header.x1-((TkCanvas*)Proj->VP->canvas)->xOrigin);
   y=Tk_Height(((TkCanvas*)Proj->VP->canvas)->tkwin)-(Proj->VP->header.y1-((TkCanvas*)Proj->VP->canvas)->yOrigin+Proj->VP->Height);

   if (!Color || Txt<=(GDB_Txt*)0x1 || !Proj->VP->tkfont || GLRender->Resolution>1)
      return;

   Tk_GetFontMetrics(Proj->VP->tkfont,&tkm);
   dy=tkm.ascent;

   if (Interp) {
      Tk_CanvasPsColor(Interp,Proj->VP->canvas,Color);
   } else {
      glColor3us(Color->red,Color->green,Color->blue);
   }

   glFontUse(Tk_Display(Tk_CanvasTkwin(Proj->VP->canvas)),Proj->VP->tkfont);

   glMatrixMode(GL_MODELVIEW);
   glPushMatrix();
   glLoadIdentity();

   glMatrixMode(GL_PROJECTION);
   glPushMatrix();
   glLoadIdentity();
   gluOrtho2D(0,Proj->VP->Width,0,Proj->VP->Height);

   Projection_UnClip(Proj);

   while (Txt) {

      len=strlen(Txt->String);

      if (len>1 && (Proj->PixDist<500 || isupper(Txt->String[strlen(Txt->String)-2]))) {
         if (Proj->Type->Project(Proj,(GeoVect*)&Txt->Co,(GeoVect*)&pos,1)) {
            gluProject(pos[0],pos[1],pos[2],Proj->VP->GLModR,Proj->VP->GLProj,Proj->VP->GLView,&pix[0],&pix[1],&pix[2]);

            pix[0]+=Point;
            pix[1]+=Point;

            /*Get length in pixel and max it to pixel buffer size (1024)*/
            dx=Tk_TextWidth(Proj->VP->tkfont,Txt->String,len);

            /*If within the viewport limits*/
            if ((pix[0]-Point+x-5)>0 && (pix[1]-Point+y-5)>0 && (pix[0]+x+dx+5)<Proj->VP->Width && (pix[1]+y+dy+5)<Proj->VP->Height) {

              /*If not overlapping another label*/
              if (glCrowdPush(pix[0]+x,pix[1]+y,pix[0]+x+dx,pix[1]+y+dy,10)) {
                  if (Point) {
                     glPrint(Interp,Proj->VP->canvas,"o",pix[0]-Point,pix[1]-Point,0);
                  }
                  glPrint(Interp,Proj->VP->canvas,Txt->String,pix[0],pix[1],0);
               }
            }
         }
      }
      Txt=Txt->Next;
   }

   Projection_Clip(Proj);

   glMatrixMode(GL_PROJECTION);
   glPopMatrix();
   glMatrixMode(GL_MODELVIEW);
   glPopMatrix();
}

void GDB_StarRender(Tcl_Interp *Interp,Projection *Proj) {

   int i;

   glPointSize(2.0);
   glBegin(GL_POINTS);
   for (i=0;i<500;i++) {
      glColor3d(StarCat[i].r,StarCat[i].g,StarCat[i].b);
      glVertex3d(StarCat[i].x*0.1,StarCat[i].y*0.1,StarCat[i].z*0.1);
   }
   glEnd();
}

/*--------------------------------------------------------------------------------------------------------------
 * Nom          : <Texture_Read>
 * Creation     : Mai 2002 J.P. Gauthier & David Dube
 *
 * But          : Lire les donnees d'une image TIFF ou GeoTIFF
 *
 * Parametres   :
 *   <File>     : Le nom du fichier TIFF ou GeoTIFF a lire. (IN)
 *   <Width>    : X pixels (OUT)
 *   <Height>   : Y pixels (OUT)
 *   <Desc>     : Le descripteur du fichier TIFF (OUT)
 *   <Buffer>   : Le buffer de donnes (OUT)
 *
 * Retour       : Code d'erreur standard TCL
 *
 * Remarques    :
 *
 *---------------------------------------------------------------------------------------------------------------
*/
GLuint Texture_Read(char *File) {

   GDALDatasetH    set=NULL;
   GDALRasterBandH hband;
   GDALColorTableH hTable;
   GDALColorEntry  entry;
   GDALDataType    t;
   GLuint          id=0;
   int             w,h,s,n,nc,i;
   char           *buf=NULL;
   char           *map=NULL;

   if (!(set=GDALOpen(File,GA_ReadOnly))) {
      fprintf(stderr,"(ERROR) Texture_Read: Unable to open texture file %s\n",File);
      return(-1);
   }

   n=GDALGetRasterCount(set);
   hband=GDALGetRasterBand(set,1);
   w=GDALGetRasterBandXSize(hband);
   h=GDALGetRasterBandYSize(hband);
   t=GDALGetRasterDataType(hband);
   s=GDALGetDataTypeSize(t);

   n=n>3?3:n;
   if (!(buf=(char*)malloc(w*h*n*s))) {
      fprintf(stderr,"(ERROR) Texture_Read: Unable to alocate system memory for texture\n");
      GDALClose(set);
      return(-1);
   }
   s>>=n;

   for(i=0;i<n;i++) {
      hband=GDALGetRasterBand(set,i+1);
      GDALRasterIO(hband,GF_Read,0,0,w,h,buf+i*s,w,h,t,s*n,0);
   }

   /* La texture n'est pas initialisee */
   glGenTextures(1,&id);
   glBindTexture(GL_TEXTURE_2D,id);

   /* Parametre de cette texture */
   glTexEnvf(GL_TEXTURE_ENV,GL_TEXTURE_ENV_MODE,GL_MODULATE);
   glTexParameteri(GL_TEXTURE_2D,GL_TEXTURE_WRAP_S,GL_CLAMP);
   glTexParameteri(GL_TEXTURE_2D,GL_TEXTURE_WRAP_T,GL_CLAMP);
   glTexParameteri(GL_TEXTURE_2D,GL_TEXTURE_MAG_FILTER,GL_LINEAR);
   glTexParameteri(GL_TEXTURE_2D,GL_TEXTURE_MIN_FILTER,GL_LINEAR);

   if (GDALGetRasterColorInterpretation(hband)==GCI_PaletteIndex && (hTable=GDALGetRasterColorTable(hband))) {
      nc=GDALGetColorEntryCount(hTable);
      map=(char*)malloc(nc*sizeof(char)*3);

      for (i=0;i<nc;i++) {
         GDALGetColorEntryAsRGB(hTable,i,&entry);
         map[i*3]=entry.c1;
         map[i*3+1]=entry.c2;
         map[i*3+2]=entry.c3;
//         map[i*3+3]=entry.c4;
      }

      glEnable(GL_COLOR_TABLE);
      glColorTable(GL_COLOR_TABLE,GL_RGB,nc,GL_RGB,GL_UNSIGNED_BYTE,(GLvoid*)map);
   }

//   gluBuild2DMipmaps(GL_TEXTURE_2D,n,w,h,GL_RGB,GL_UNSIGNED_BYTE,(GLvoid*)buf);
   glTexImage2D(GL_TEXTURE_2D,0,n,w,h,0,GL_RGB,GL_UNSIGNED_BYTE,(GLvoid*)buf);
   glDisable(GL_COLOR_TABLE);

   if (map) free(map);
   if (buf) free(buf);
   GDALClose(set);
   return(id);
}

