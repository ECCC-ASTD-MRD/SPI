/*==============================================================================
 * Environnement Canada
 * Centre Meteorologique Canadian
 * 2100 Trans-Canadienne
 * Dorval, Quebec
 *
 * Projet    : Projection orthographique de la carte vectorielle.
 * Fichier   : tkCanvVP.c
 * Creation  :
 *
 * Description: Definition de l'item viewport.
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
#include "tkInt.h"
#include "tkglCanvas.h"
#include "tkglCanvText.h"
#include "tkCanvVP.h"
#include "Projection.h"
#include "tclGDAL.h"
#include "tclOGR.h"
#include "tcl3DModel.h"

extern int   Data_Render(Tcl_Interp *Interp,TData *Field,ViewportItem *VP,ClientData Proj,GLuint GLMode,int Mode);
extern int   Traj_Render(Tcl_Interp *Interp,TTraj *Traj,ViewportItem *VP,Projection *Proj,GLuint GLMode);
extern int   Obs_Render(Tcl_Interp *Interp,TObs *Obs,ViewportItem *VP,Projection *Proj,GLuint GLMode);

#ifdef HAVE_ECBUFR
extern int   MetObs_Render(Tcl_Interp *Interp,TMetObs *Obs,ViewportItem *VP,Projection *Proj,GLuint GLMode);
#endif

static int    ViewportCommand(ClientData Data,Tcl_Interp *Interp,int Objc,Tcl_Obj *const Objv[]);
static int    ViewportCoords(Tcl_Interp *Interp,Tk_Canvas Canvas,Tk_Item *Item,int Argc,Tcl_Obj *const Argv[]);
static int    ViewportToArea(Tk_Canvas Canvas,Tk_Item *Item,double *RectPtr);
static double ViewportToPoint(Tk_Canvas Canvas,Tk_Item *Item,double *CoordPtr);
static int    ViewportToPostscript(Tcl_Interp *Interp,Tk_Canvas Canvas,Tk_Item *Item,int Prepass);
static void   ViewportBBox(Tk_Canvas Canvas,ViewportItem *VP);
static int    ViewportConfigure(Tcl_Interp *Interp,Tk_Canvas Canvas,Tk_Item *Item,int Argc,Tcl_Obj *const Argv[],int Flags);
static int    ViewportCreate(Tcl_Interp *Interp,Tk_Canvas Canvas,Tk_Item *Item,int Argc,Tcl_Obj *const Argv[]);
static void   ViewportDelete(Tk_Canvas Canvas,Tk_Item *Item,Display *Disp);
static void   ViewportDisplay(Tk_Canvas Canvas,Tk_Item *Item,Display *Disp,Drawable Drawt,int X,int Y,int Width,int Height);
static void   ViewportScale(Tk_Canvas Canvas,Tk_Item *Item, double OriginX,double OriginY,double ScaleX,double ScaleY);
static void   ViewportTranslate(Tk_Canvas Canvas,Tk_Item *Item,double DeltaX,double DeltaY);
static void   ViewportIntrusion(Tcl_Interp *Interp,Tk_Canvas Canvas,Tk_Item *Item);
static void   ViewportLicense(Tcl_Interp *Interp,ViewportItem *VP,Projection *Proj);

static int         VP_CamParseProc(ClientData clientData,Tcl_Interp *interp,Tk_Window tkwin,const char *value,char *widgRec,int offset);
static const char *VP_CamPrintProc(ClientData clientData,Tk_Window tkwin,char *widgRec,int offset,Tcl_FreeProc **freeProcPtr);
static int         VP_ProjParseProc(ClientData clientData,Tcl_Interp *interp,Tk_Window tkwin,const char *value,char *widgRec,int offset);
static const char *VP_ProjPrintProc(ClientData clientData,Tk_Window tkwin,char *widgRec,int offset,Tcl_FreeProc **freeProcPtr);
static int         VP_ArrayParseProc(ClientData clientData,Tcl_Interp *interp,Tk_Window tkwin,const char *value,char *widgRec,int offset);
static const char *VP_ArrayPrintProc(ClientData clientData,Tk_Window tkwin,char *widgRec,int offset,Tcl_FreeProc **freeProcPtr);

static const Tk_CustomOption CamOption   = { VP_CamParseProc,VP_CamPrintProc,NULL };
static const Tk_CustomOption ArrayOption = { VP_ArrayParseProc,VP_ArrayPrintProc,NULL };
static const Tk_CustomOption ProjOption  = { VP_ProjParseProc,VP_ProjPrintProc,NULL };
static const Tk_CustomOption tagsOption  = { Tk_CanvasTagsParseProc,Tk_CanvasTagsPrintProc,NULL };

void ViewportSetup(Tk_Canvas Canvas,ViewportItem *VP,Projection *Proj,int Width,int Height,int Tile,int Clear,int PS);
void ViewportSet(ViewportItem *VP,Projection *Proj);
void ViewportUnset(ViewportItem *VP);

static ViewportItem *ViewportTable[256];

/*Information used for parsing configuration specs:*/

static const Tk_ConfigSpec configSpecs[] = {
   { TK_CONFIG_ANCHOR, "-anchor", NULL, NULL, "nw",Tk_Offset(ViewportItem,anchor),TK_CONFIG_DONT_SET_DEFAULT },
   { TK_CONFIG_FONT, "-font", NULL, NULL, "Helvetica 12",Tk_Offset(ViewportItem,tkfont),TK_CONFIG_NULL_OK },
   { TK_CONFIG_DOUBLE, "-x", NULL, NULL, "0",Tk_Offset(ViewportItem,x),TK_CONFIG_DONT_SET_DEFAULT },
   { TK_CONFIG_DOUBLE, "-y", NULL, NULL, "0",Tk_Offset(ViewportItem,y),TK_CONFIG_DONT_SET_DEFAULT },
   { TK_CONFIG_PIXELS, "-width", NULL, NULL, "0",Tk_Offset(ViewportItem,Width),TK_CONFIG_DONT_SET_DEFAULT },
   { TK_CONFIG_PIXELS, "-height", NULL, NULL, "0",Tk_Offset(ViewportItem,Height),TK_CONFIG_DONT_SET_DEFAULT },
   { TK_CONFIG_PIXELS, "-bd", NULL, NULL, "0",Tk_Offset(ViewportItem,BDWidth),TK_CONFIG_DONT_SET_DEFAULT },
   { TK_CONFIG_COLOR, "-colorfillcoast", NULL, NULL, NULL,Tk_Offset(ViewportItem,ColorFCoast),TK_CONFIG_NULL_OK },
   { TK_CONFIG_COLOR, "-colorfilllake", NULL, NULL, NULL,Tk_Offset(ViewportItem,ColorFLake),TK_CONFIG_NULL_OK },
   { TK_CONFIG_COLOR, "-colorcoast", NULL, NULL, "black",Tk_Offset(ViewportItem,ColorCoast),TK_CONFIG_NULL_OK },
   { TK_CONFIG_COLOR, "-colorlake", NULL, NULL, "black",Tk_Offset(ViewportItem,ColorLake),TK_CONFIG_NULL_OK },
   { TK_CONFIG_COLOR, "-colorriver", NULL, NULL, "black",Tk_Offset(ViewportItem,ColorRiver),TK_CONFIG_NULL_OK },
   { TK_CONFIG_COLOR, "-colorpolit", NULL, NULL, "black",Tk_Offset(ViewportItem,ColorPolit),TK_CONFIG_NULL_OK },
   { TK_CONFIG_COLOR, "-coloradmin", NULL, NULL, "black",Tk_Offset(ViewportItem,ColorAdmin),TK_CONFIG_NULL_OK },
   { TK_CONFIG_COLOR, "-colorcity", NULL, NULL, "black",Tk_Offset(ViewportItem,ColorCity),TK_CONFIG_NULL_OK },
   { TK_CONFIG_COLOR, "-colorplace", NULL, NULL, "black",Tk_Offset(ViewportItem,ColorPlace),TK_CONFIG_NULL_OK },
   { TK_CONFIG_COLOR, "-colorroad", NULL, NULL, "black",Tk_Offset(ViewportItem,ColorRoad),TK_CONFIG_NULL_OK },
   { TK_CONFIG_COLOR, "-colorrail", NULL, NULL, "black",Tk_Offset(ViewportItem,ColorRail),TK_CONFIG_NULL_OK },
   { TK_CONFIG_COLOR, "-colorutil", NULL, NULL, "black",Tk_Offset(ViewportItem,ColorUtil),TK_CONFIG_NULL_OK },
   { TK_CONFIG_COLOR, "-colorcanal", NULL, NULL, "black",Tk_Offset(ViewportItem,ColorCanal),TK_CONFIG_NULL_OK },
   { TK_CONFIG_COLOR, "-colorcoord", NULL, NULL, "black",Tk_Offset(ViewportItem,ColorCoord),TK_CONFIG_NULL_OK },
   { TK_CONFIG_COLOR, "-fg","-foreground", NULL, "black",Tk_Offset(ViewportItem,FGColor),TK_CONFIG_NULL_OK },
   { TK_CONFIG_COLOR, "-bg","-background", NULL, "white",Tk_Offset(ViewportItem,BGColor),TK_CONFIG_NULL_OK },
   { TK_CONFIG_BOOLEAN, "-update", NULL, NULL, "1",Tk_Offset(ViewportItem,Update),TK_CONFIG_DONT_SET_DEFAULT },
   { TK_CONFIG_BOOLEAN, "-secondary", NULL, NULL, "0",Tk_Offset(ViewportItem,Secondary),TK_CONFIG_DONT_SET_DEFAULT },
   { TK_CONFIG_STRING, "-command", NULL, NULL, NULL ,Tk_Offset(ViewportItem,Command),TK_CONFIG_DONT_SET_DEFAULT },
   { TK_CONFIG_CUSTOM, "-projection", NULL, NULL, NULL,Tk_Offset(ViewportItem,Projection),TK_CONFIG_NULL_OK,&ProjOption },
   { TK_CONFIG_CUSTOM, "-data", NULL, NULL, NULL,Tk_Offset(ViewportItem,DataItem),TK_CONFIG_NULL_OK,&ArrayOption },
   { TK_CONFIG_CUSTOM, "-maskitem", NULL, NULL, NULL,Tk_Offset(ViewportItem,MaskItem),TK_CONFIG_NULL_OK,&ArrayOption },
   { TK_CONFIG_PIXELS, "-maskwidth", NULL, NULL, "0",Tk_Offset(ViewportItem,MaskWidth),TK_CONFIG_DONT_SET_DEFAULT },
   { TK_CONFIG_PIXELS, "-crowd", NULL, NULL, "0",Tk_Offset(ViewportItem,CrowdBuffer),TK_CONFIG_DONT_SET_DEFAULT },
   { TK_CONFIG_CUSTOM, "-camera", NULL, NULL, NULL,Tk_Offset(ViewportItem,Cam),TK_CONFIG_NULL_OK,&CamOption },
   { TK_CONFIG_INT, "-frame", NULL, NULL, "-1",Tk_Offset(ViewportItem,Frame),TK_CONFIG_DONT_SET_DEFAULT },
   { TK_CONFIG_CUSTOM, "-tags", NULL, NULL, NULL,0,TK_CONFIG_NULL_OK,&tagsOption },
   { TK_CONFIG_END, NULL, NULL, NULL, NULL,0,0 }
};

/*
 * The structures below defines the pixmap item type in terms of
 * procedures that can be invoked by generic item code.
 */

Tk_ItemType tkViewportType = {
   "viewport",            /* name */
   sizeof(ViewportItem),  /* itemSize */
   ViewportCreate,        /* createProc */
   configSpecs,           /* configSpecs */
   ViewportConfigure,     /* configureProc */
   ViewportCoords,        /* coordProc */
   ViewportDelete,        /* deleteProc */
   ViewportDisplay,       /* displayProc */
   0,                     /* alwaysRedraw */
   ViewportToPoint,       /* pointProc */
   ViewportToArea,        /* areaProc */
   ViewportToPostscript,  /* postscriptProc */
   ViewportScale,         /* scaleProc */
   ViewportTranslate,     /* translateProc */
   NULL,                  /* indexProc */
   NULL,                  /* icursorProc */
   NULL,                  /* selectionProc */
   NULL,                  /* insertProc */
   NULL,                  /* dTextProc */
   NULL                   /* nextPtr */
};

double ViewportY(ViewportItem *VP) {
   return (((TkCanvas*)VP->canvas)->yOrigin);
}

double ViewportX(ViewportItem *VP) {
   return(((TkCanvas*)VP->canvas)->xOrigin);
}

/*----------------------------------------------------------------------------
 * Nom      : <ViewportCrowdPush>
 * Creation : Janvier 2011 - J.P. Gauthier - CMC/CMOE
 *
 * But      : Controle de peuplement, Verifie si l'espace est disponible et si
 *            oui, ajoute a la liste de peuplement
 *
 * Parametres :
 *   <VP>     : Viewport
 *  <X0>       : Coordonnee X0
 *  <Y0>       : Coordonnee Y0
 *  <X1>       : Coordonnee X1
 *  <Y1>       : Coordonnee Y1
 *  <Delta>    : Espacement
 *
 * Retour:
 *  <Exist>   : Trouve ou non
 *
 * Remarques :
 *
 *----------------------------------------------------------------------------
*/
int ViewportCrowdPush(ViewportItem *VP,int X0,int Y0,int X1,int Y1,int Delta) {

   int   *box,x0,x1,y0,y1,d;
   TList *node;

   d=Delta<0?VP->CrowdBuffer:Delta;
      
   if (X0>X1) { x0=X0;X0=X1;X1=x0; }
   if (Y0>Y1) { y0=Y0;Y0=Y1;Y1=y0; }
   
   x0=X0-d;
   x1=X1+d;
   y0=Y0-d;
   y1=Y1+d;

   /*If not within the viewport limits*/
   if (!(x0>0 && y0>0 && x1<VP->Width && y1<VP->Height)) {
      return(0);
   }
      
   /*If there's a buffer to check ovrelap for */
   if (d) {
      /*Check for bbox intersection*/
      node=VP->CrowdList;
      while(node) {
         box=node->Data;
         if (VOUT(x0,x1,box[0],box[2]) || VOUT(y0,y1,box[1],box[3])) {
            /*No intersection here, continue*/
            node=node->Next;
         } else {
            /*Found an intersection*/
            return(0);
         }
      }

      /*If no intersection found, add in node list*/
      box=(int*)malloc(4*sizeof(int));
      box[0]=X0; box[1]=Y0;
      box[2]=X1; box[3]=Y1;

      node=(TList*)malloc(sizeof(TList));
      node->Next=VP->CrowdList;
      node->Prev=NULL;
      node->Data=box;

      if (VP->CrowdList) {
         VP->CrowdList->Prev=node;
      }
      VP->CrowdList=node;
   }
   
   return(1);
}

/*----------------------------------------------------------------------------
 * Nom      : <ViewportCrowdPop>
 * Creation : Janvier 2011 - J.P. Gauthier - CMC/CMOE
 *
 * But      : Controle de peuplement, supprime le dernier item ajoute.
 *
 * Parametres :
 *   <VP>     : Viewport
 *
 * Retour:
 *
 * Remarques :
 *
 *----------------------------------------------------------------------------
*/
void ViewportCrowdPop(ViewportItem *VP) {

   TList *tmp;

   if (VP->CrowdList) {
      tmp=VP->CrowdList;
      VP->CrowdList=VP->CrowdList->Next;

      if (VP->CrowdList)
         VP->CrowdList->Prev=NULL;

      free((int*)(tmp->Data));
      free(tmp);
   }
}

/*----------------------------------------------------------------------------
 * Nom      : <ViewportCrowdClear>
 * Creation : Janvier 2011 - J.P. Gauthier - CMC/CMOE
 *
 * But      : Reinitialiser la liste de peuplement
 *
 * Parametres :
 *   <VP>     : Viewport
 *
 * Retour:
 *
 * Remarques :
 *
 *----------------------------------------------------------------------------
*/
void ViewportCrowdClear(ViewportItem *VP) {

   TList *tmp;

   while(VP->CrowdList) {
      tmp=VP->CrowdList;
      VP->CrowdList=VP->CrowdList->Next;

      free((int*)(tmp->Data));
      free(tmp);
   }
}

/*----------------------------------------------------------------------------
 * Nom      : <Tkgeoeer_Init>
 * Creation : Avril 1998 - J.P. Gauthier - CMC/CMOE
 *
 * But      : Cette procedure effectue les initialisations specifiques aux divers
 *            package.
 *
 * Parametres  :
 *  <Interp>   : Interpreteur Tcl/Tk
 *
 * Retour      :
 *  <TCL...>   : Code de retour TCL
 *
 * Remarques :
 *
 *----------------------------------------------------------------------------
*/
int Tkgeoeer_Init(Tcl_Interp *Interp) {

   extern int Tcldata_Init();
   extern int Tkcolorbar_Init();
   extern int Tkgraph_Init();

   Tcl_ThreadId tid;

   if (Tcl_PkgProvide(Interp,"TkGeoEER",PACKAGE_VERSION) != TCL_OK) {
      return(TCL_ERROR);
   }

   Tk_glCreateItemType(&tkViewportType);

   /*Initialisation du package tcl geo*/
   if (Tclgeoeer_Init(Interp)==TCL_ERROR)
      return(TCL_ERROR);

   /*Initialisation du package colorbar*/
   if (Tkcolorbar_Init(Interp)==TCL_ERROR)
      return(TCL_ERROR);

   /*Initialisation du package graph*/
   if (Tkgraph_Init(Interp)==TCL_ERROR)
      return(TCL_ERROR);

   /*In batch mode, bypass the geodata thread mechanism and execute in master thread*/
   if (GLRender->UseThreads && !GLRender->XBatch) {
      if (Tcl_CreateThread(&tid,GDB_ThreadProc,NULL,TCL_THREAD_STACK_DEFAULT,TCL_THREAD_NOFLAGS)==TCL_ERROR) {
         fprintf(stderr,"(WARNING) Tkviewport_Init: Unable to initiate GDB thread\n");
      }
   }

   memset(ViewportTable,0x0,VPMAX*sizeof(ViewportItem*));

   return(TCL_OK);
}

/*----------------------------------------------------------------------------
 * Nom      : <ViewportCreate>
 * Creation : ???
 *
 * But      : This procedure is invoked to create a new pixmap
 *       item in a canvas.
 *
 * Parametres :
 *  <Interp>  : Interpreter for error reporting
 *  <Canvas>  : Canvas to hold new item
 *  <Item>    : Record to hold new item
 *  <Argc>    : Number of arguments in argv
 *  <Argv>    : Arguments describing rectangle
 *
 * Retour:
 *  <TCL_..>  : Code de reussite TCL.
 *
 * Remarques :
 *
 *----------------------------------------------------------------------------
*/
static int ViewportCreate(Tcl_Interp *Interp,Tk_Canvas Canvas,Tk_Item *Item,int Argc,Tcl_Obj *const Argv[]){

   ViewportItem *vp=(ViewportItem *)Item;
   int i;

   /*Initialize item's record.*/

   vp->canvas      = Canvas;
   vp->anchor      = TK_ANCHOR_NW;
   vp->tkfont      = NULL;
   vp->BGColor     = NULL;
   vp->FGColor     = NULL;
   vp->ColorFCoast = NULL;
   vp->ColorFLake  = NULL;
   vp->ColorCoast  = NULL;
   vp->ColorLake   = NULL;
   vp->ColorRiver  = NULL;
   vp->ColorPolit  = NULL;
   vp->ColorAdmin  = NULL;
   vp->ColorCity   = NULL;
   vp->ColorPlace  = NULL;
   vp->ColorRoad   = NULL;
   vp->ColorRail   = NULL;
   vp->ColorUtil   = NULL;
   vp->ColorCanal  = NULL;
   vp->ColorCoord  = NULL;
   vp->Width       = 0;
   vp->Height      = 0;
   vp->x           = 0;
   vp->y           = 0;
   vp->BDWidth     = 1;
   vp->Command     = NULL;
   vp->Cam         = NULL;
   vp->CamStr      = NULL;
   vp->Projection  = NULL;
   vp->Update      = 1;
   vp->Realloc     = 1;
   vp->ForcePick   = 1;
   vp->Frame       = 0;
   vp->Ratio       = 0;
   vp->Secondary   = 0;
   
   vp->MaskWidth      = 10;
   vp->MaskItem.Array = NULL;
   vp->MaskItem.String= NULL;
   vp->MaskItem.Nb    = 0;
   
   vp->DataItem.Array = NULL;
   vp->DataItem.String= NULL;
   vp->DataItem.Nb    = 0;

   vp->CrowdBuffer = 10;
   vp->CrowdList   = NULL;

   vp->Loading     = 0;
   vp->ThreadId    = Tcl_GetCurrentThread();
   vp->Timer       = NULL;

   for (i=0;i<NBFRAMEMAX;i++){
      vp->Frames[i]=NULL;
   }

   /*Add to viewport table*/
   for (i=0;i<VPMAX;i++){
      if (ViewportTable[i]==NULL) {
         ViewportTable[i]=vp;
         break;
      }
   }

   /*Process the arguments to fill in the item record*/
   if (ViewportConfigure(Interp,Canvas,Item,Argc,Argv,0) != TCL_OK){
      ViewportDelete(Canvas,Item,Tk_Display(Tk_CanvasTkwin(Canvas)));
      return(TCL_ERROR);
   }

   /*Creer la commande qui permet les transformations*/
   if (vp->Command) {
      Tcl_CreateObjCommand(Interp,vp->Command,ViewportCommand,(ClientData)vp,(Tcl_CmdDeleteProc *)NULL);
   }

   return(TCL_OK);
}

/*----------------------------------------------------------------------------
 * Nom      : <ViewportCommand>
 * Creation : Fevrier 1999 - J.P. Gauthier - CMC/CMOE
 *
 * But      : Cette procedure fait l'appel des fonctions de transformations
 *            pour le viewport.
 *
 * Parametres :
 *  <Data>    : Pointeur sur le Viewport
 *  <Interp>  : Interpreter TCL
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
static int ViewportCommand(ClientData Data,Tcl_Interp *Interp,int Objc,Tcl_Obj *const Objv[]){

   ViewportItem *vp=(ViewportItem *)Data;
   Projection   *proj;
   TTraj        *traj;
   TObs         *obs;
   TData        *data;
   Tcl_Obj      *obj;

   int           nco,idx,i,bool,n,np,pick,ix,iy;
   Vect3d        pt,pt0,pt1;
   Coord         loc,loc0,loc1,co[1000];
   double        x,y,h,d;

   static const char *sopt[] = { "-ungrid","-grid","-unproject","-project","-projectline","-projectcircle","-distxy","-distpix","-distll","-bearing","-circle","-pick","-bbox",NULL };
   enum                opt { UNGRID,GRID,UNPROJECT,PROJECT,PROJECTLINE,PROJECTCIRCLE,DISTXY,DISTPIX,DISTLL,BEARING,CIRCLE,PICK,BBOX };

   proj=Projection_Get(vp->Projection);

   if (!proj) {
      Tcl_AppendResult(Interp,"\n   ViewportCommand: Projection name unknown: \"",vp->Projection,"\"",(char *)NULL);
      return(TCL_ERROR);
   }

   /*Si la projection n'a pas encore initialise ses dimensions*/
   if (!proj->VP) {
      obj=Tcl_NewListObj(0,NULL);
      Tcl_ListObjAppendElement(Interp,obj,Tcl_NewIntObj(0));
      Tcl_ListObjAppendElement(Interp,obj,Tcl_NewIntObj(0));
      Tcl_SetObjResult(Interp,obj);
      return(TCL_OK);
   }

   if (proj->Type) {

      Tcl_ResetResult(Interp);

      if (Objc<2) {
         Tcl_WrongNumArgs(Interp,1,Objv,"command ?arg arg ...?");
         return(TCL_ERROR);
      }

      if (Tcl_GetIndexFromObj(Interp,Objv[1],sopt,"command",0,&idx)!=TCL_OK) {
         return(TCL_ERROR);
      }

      switch ((enum opt)idx) {
         case UNGRID:
            if(Objc!=4) {
               Tcl_WrongNumArgs(Interp,2,Objv,"x y");
               return(TCL_ERROR);
            }
            h=0.0;
            if (proj->Ref) {
               Tcl_GetDoubleFromObj(Interp,Objv[2],&pt0[0]);
               Tcl_GetDoubleFromObj(Interp,Objv[3],&pt0[1]);
               pt0[0]-=vp->x;
               pt0[1]-=vp->y;
               // If geographic, get the height
               if (proj->Geographic) {
                  proj->Type->UnProject(vp,proj,&loc0,pt0);
                  CLAMPLON(loc0.Lon);
                  h=GDB_GetMap(proj->Geo,loc0.Lat,loc0.Lon);
               }
               // Force non-geographic to get the gridpoint
               n=proj->Geographic;
               proj->Geographic=0;
               proj->Type->UnProject(vp,proj,&loc0,pt0);
               proj->Geographic=n;
               pt0[0]=loc0.Lon;
               pt0[1]=loc0.Lat;

            } else {
               pt0[0]=pt0[1]=-1.0;
            }
            obj=Tcl_NewListObj(0,NULL);
            Tcl_ListObjAppendElement(Interp,obj,Tcl_NewDoubleObj(pt0[0]));
            Tcl_ListObjAppendElement(Interp,obj,Tcl_NewDoubleObj(pt0[1]));
            Tcl_ListObjAppendElement(Interp,obj,Tcl_NewDoubleObj(h));
            Tcl_SetObjResult(Interp,obj);
            break;

         case GRID:
            if(Objc!=4 && Objc!=5) {
               Tcl_WrongNumArgs(Interp,2,Objv,"i j [Anywhere]");
               return(TCL_ERROR);
            }
            Tcl_GetDoubleFromObj(Interp,Objv[2],&pt0[0]);
            Tcl_GetDoubleFromObj(Interp,Objv[3],&pt0[1]);
            if (Objc==5) {
               Tcl_GetBooleanFromObj(Interp,Objv[5],&bool);
            } else {
               bool=0;
            }

            if (proj->Ref) {
               if (proj->Geographic) {
                  proj->Ref->Project(proj->Ref,pt0[0],pt0[1],&loc0.Lat,&loc0.Lon,0,1);
               } else {
                  loc0.Lat=pt0[1];
                  loc0.Lon=pt0[0];
               }
            } else {
               pt0[0]=pt0[1]=-1.0;
               h=0.0;
             }
            Tcl_SetObjResult(Interp,proj->Type->ProjectPoint(Interp,vp,proj,loc0,bool));
            break;

         case UNPROJECT:
            if(Objc!=4 && Objc!=6 && Objc!=7) {
               Tcl_WrongNumArgs(Interp,2,Objv,"x0 y0 [x1 y1] [delta]");
               return(TCL_ERROR);
            }
            Tcl_GetDoubleFromObj(Interp,Objv[2],&pt0[0]);
            Tcl_GetDoubleFromObj(Interp,Objv[3],&pt0[1]);
            pt1[0]=pt0[0]-=vp->x;
            pt1[1]=pt0[1]-=vp->y;

            if (Objc==6) {
               Tcl_GetDoubleFromObj(Interp,Objv[4],&pt1[0]);
               Tcl_GetDoubleFromObj(Interp,Objv[5],&pt1[1]);
               pt1[0]-=vp->x;
               pt1[1]-=vp->y;
            }

            if (pt0[0]>pt1[0]) { d=pt1[0]; pt1[0]=pt0[0]; pt0[0]=d; }
            if (pt0[1]>pt1[1]) { d=pt1[1]; pt1[1]=pt0[1]; pt0[1]=d; }

            d=1.0;
            if (Objc==7) {
               Tcl_GetDoubleFromObj(Interp,Objv[6],&d);
            }

            obj=Tcl_NewListObj(0,NULL);
            for(pt[1]=pt0[1];pt[1]<=pt1[1];pt[1]+=d) {
               for(pt[0]=pt0[0];pt[0]<=pt1[0];pt[0]+=d) {
                  proj->Type->UnProject(vp,proj,&loc0,pt);
                  loc0.Elev=0.0;
                  if (proj->Geographic) {
                     CLAMPLON(loc0.Lon);
                     loc0.Elev=GDB_GetMap(proj->Geo,loc0.Lat,loc0.Lon);
                  }
                  Tcl_ListObjAppendElement(Interp,obj,Tcl_NewDoubleObj(loc0.Lat));
                  Tcl_ListObjAppendElement(Interp,obj,Tcl_NewDoubleObj(loc0.Lon));
                  Tcl_ListObjAppendElement(Interp,obj,Tcl_NewDoubleObj(loc0.Elev));
               }
            }
            Tcl_SetObjResult(Interp,obj);
            break;

         case PROJECT:
            if (Objc!=5 && Objc!=6){
               Tcl_WrongNumArgs(Interp,2,Objv,"lat lon elev [Anywhere]");
               return(TCL_ERROR);
            }
            Tcl_GetDoubleFromObj(Interp,Objv[2],&loc0.Lat);
            Tcl_GetDoubleFromObj(Interp,Objv[3],&loc0.Lon);
            Tcl_GetDoubleFromObj(Interp,Objv[4],&loc0.Elev);
            if (Objc==6) {
               Tcl_GetBooleanFromObj(Interp,Objv[5],&bool);
            } else {
               bool=0;
            }
            Tcl_SetObjResult(Interp,proj->Type->ProjectPoint(Interp,vp,proj,loc0,bool));
            break;

         case PROJECTLINE:
            if (Objc!=4){
               Tcl_WrongNumArgs(Interp,2,Objv,"NONE|COORD|TRUE coords");
               return(TCL_ERROR);
            }
            nco=Projection_Map(Interp,co,Tcl_GetString(Objv[2])[0],Objv[3]);
            Tcl_SetObjResult(Interp,proj->Type->ProjectLine(Interp,vp,proj,co,nco));
            break;

         case PROJECTCIRCLE:
            if (Objc!=5 && Objc!=6){
               Tcl_WrongNumArgs(Interp,0,Objv,"lat lon dist [deg]");
               return(TCL_ERROR);
            }
            Tcl_GetDoubleFromObj(Interp,Objv[2],&loc0.Lat);
            Tcl_GetDoubleFromObj(Interp,Objv[3],&loc0.Lon);
            Tcl_GetDoubleFromObj(Interp,Objv[4],&d);

            np=10;
            if (Objc==6) {
               Tcl_GetIntFromObj(Interp,Objv[5],&np);
            }

            loc0.Lat=DEG2RAD(loc0.Lat);
            loc0.Lon=DEG2RAD(loc0.Lon);
            d=M2RAD(d);

            obj=Tcl_NewListObj(0,NULL);
            for(n=0;n<=360;n+=np) {
               x=DEG2RAD(n);

               loc1.Lat=asin(sin(loc0.Lat)*cos(d)+cos(loc0.Lat)*sin(d)*cos(x));
               loc1.Lon=fmod(loc0.Lon-(atan2(sin(x)*sin(d)*cos(loc0.Lat),cos(d)-sin(loc0.Lat)*sin(loc1.Lat)))+M_PI,M_2PI)-M_PI;
               loc1.Lat=RAD2DEG(loc1.Lat);loc1.Lon=RAD2DEG(loc1.Lon);

               proj->Type->Project(proj,(GeoVect*)&loc1,(GeoVect*)&pt0,1);
               gluProject(pt0[0],pt0[1],pt0[2],vp->GLModR,vp->GLProj,vp->GLView,&pt1[0],&pt1[1],&pt1[2]);

               /*Repositionner dans le referentiel de Tcl*/
               pt1[1]=vp->Height-pt1[1];
               pt1[2]=1.0-pt1[2];

               if (INSIDE(pt1,0,0,vp->Width,vp->Height)) {
                  Tcl_ListObjAppendElement(Interp,obj,Tcl_NewDoubleObj(pt1[0]+vp->x));
                  Tcl_ListObjAppendElement(Interp,obj,Tcl_NewDoubleObj(pt1[1]+vp->y));
               }
            }
            Tcl_SetObjResult(Interp,obj);
            break;

         case DISTXY:
            if (Objc!=6){
               Tcl_WrongNumArgs(Interp,2,Objv," x1 y1 x2 y2");
               return(TCL_ERROR);
            }
            Tcl_GetDoubleFromObj(Interp,Objv[2],&pt0[0]);
            Tcl_GetDoubleFromObj(Interp,Objv[3],&pt0[1]);
            Tcl_GetDoubleFromObj(Interp,Objv[4],&pt1[0]);
            Tcl_GetDoubleFromObj(Interp,Objv[5],&pt1[1]);

            if (proj->Type->UnProject(vp,proj,&loc0,pt0) && proj->Type->UnProject(vp,proj,&loc1,pt1)) {
               loc0.Lat=DEG2RAD(loc0.Lat);loc0.Lon=DEG2RAD(loc0.Lon);
               loc1.Lat=DEG2RAD(loc1.Lat);loc1.Lon=DEG2RAD(loc1.Lon);
               Tcl_SetObjResult(Interp,Tcl_NewDoubleObj(DIST(0.0,loc0.Lat,loc0.Lon,loc1.Lat,loc1.Lon)));
            } else {
               Tcl_SetObjResult(Interp,Tcl_NewDoubleObj(0.0));
            }
            break;

         case DISTLL:
            if (Objc!=7){
               Tcl_WrongNumArgs(Interp,2,Objv,"lat0 lon0 lat1 lon1 elev");
               return(TCL_ERROR);
            }
            Tcl_GetDoubleFromObj(Interp,Objv[2],&loc0.Lat);
            Tcl_GetDoubleFromObj(Interp,Objv[3],&loc0.Lon);
            Tcl_GetDoubleFromObj(Interp,Objv[4],&loc1.Lat);
            Tcl_GetDoubleFromObj(Interp,Objv[5],&loc1.Lon);
            Tcl_GetDoubleFromObj(Interp,Objv[6],&x);

            loc0.Lat=DEG2RAD(loc0.Lat);loc0.Lon=DEG2RAD(loc0.Lon);
            loc1.Lat=DEG2RAD(loc1.Lat);loc1.Lon=DEG2RAD(loc1.Lon);
            Tcl_SetObjResult(Interp,Tcl_NewDoubleObj(DIST(x,loc0.Lat,loc0.Lon,loc1.Lat,loc1.Lon)));
            break;

         case DISTPIX:
            if (Objc==2){
               Tcl_SetObjResult(Interp,Tcl_NewDoubleObj(proj->PixDist));
            } else if (Objc==3) {
               Tcl_GetDoubleFromObj(Interp,Objv[2],&x);
               x=vp->Cam->Lens*proj->PixDist/x;
               proj->PixDist=x;
               Tcl_SetObjResult(Interp,Tcl_NewDoubleObj(x));
            } else {
               Tcl_WrongNumArgs(Interp,2,Objv,"?dist?");
               return(TCL_ERROR);
            }
            break;

         case BEARING:
            if (Objc!=2 && Objc!=6){
               Tcl_WrongNumArgs(Interp,2,Objv,"?lat0 lon0 lat1 lon1?");
               return(TCL_ERROR);
            }
            if (Objc==2) {
               x=RAD2DEG(atan2(vp->Cam->Up[0],vp->Cam->Up[1]));
               Tcl_SetObjResult(Interp,Tcl_NewDoubleObj(vp->Cam->Up[2]>0.9999999?x+180:x));
            } else {
               Tcl_GetDoubleFromObj(Interp,Objv[2],&loc0.Lat);
               Tcl_GetDoubleFromObj(Interp,Objv[3],&loc0.Lon);
               Tcl_GetDoubleFromObj(Interp,Objv[4],&loc1.Lat);
               Tcl_GetDoubleFromObj(Interp,Objv[5],&loc1.Lon);

               loc0.Lat=DEG2RAD(loc0.Lat);loc0.Lon=DEG2RAD(loc0.Lon);
               loc1.Lat=DEG2RAD(loc1.Lat);loc1.Lon=DEG2RAD(loc1.Lon);

               x=COURSE(loc0.Lat,loc0.Lon,loc1.Lat,loc1.Lon);
               Tcl_SetObjResult(Interp,Tcl_NewDoubleObj(RAD2DEG(x)));
            }
            break;

         case CIRCLE:
            if (Objc!=6){
               Tcl_WrongNumArgs(Interp,0,Objv,"lat lon dist angle");
               return(TCL_ERROR);
            }
            Tcl_GetDoubleFromObj(Interp,Objv[2],&loc0.Lat);
            Tcl_GetDoubleFromObj(Interp,Objv[3],&loc0.Lon);
            Tcl_GetDoubleFromObj(Interp,Objv[4],&d);
            Tcl_GetDoubleFromObj(Interp,Objv[5],&x);

            loc0.Lat=DEG2RAD(loc0.Lat);
            loc0.Lon=DEG2RAD(loc0.Lon);
            x=DEG2RAD(x);
            d=M2RAD(d);

            loc1.Lat=asin(sin(loc0.Lat)*cos(d)+cos(loc0.Lat)*sin(d)*cos(x));
            loc1.Lon=fmod(loc0.Lon-(atan2(sin(x)*sin(d)*cos(loc0.Lat),cos(d)-sin(loc0.Lat)*sin(loc1.Lat)))+M_PI,M_2PI)-M_PI;
            loc1.Lat=RAD2DEG(loc1.Lat);loc1.Lon=RAD2DEG(loc1.Lon);

            obj=Tcl_NewListObj(0,NULL);
            Tcl_ListObjAppendElement(Interp,obj,Tcl_NewDoubleObj(loc1.Lat));
            Tcl_ListObjAppendElement(Interp,obj,Tcl_NewDoubleObj(loc1.Lon));
            Tcl_SetObjResult(Interp,obj);
            break;

         case PICK:
            if (Objc<4 || Objc>6){
               Tcl_WrongNumArgs(Interp,2,Objv,"x y [type] [force]");
               return(TCL_ERROR);
            }
            Tcl_GetDoubleFromObj(Interp,Objv[2],&x);
            Tcl_GetDoubleFromObj(Interp,Objv[3],&y);

            vp->ForcePick=1;
            pick=PICK_ALL;

            if (Objc>=5) {
               Tcl_ListObjLength(Interp,Objv[4],&np);
               if (np) pick=PICK_NONE;
               for(n=0;n<np;n++) {
                  Tcl_ListObjIndex(Interp,Objv[4],n,&obj);

                  if (strcmp(Tcl_GetString(obj),"trajectory")==0) {
                     pick|=PICK_TRAJ;
                  } else if (strcmp(Tcl_GetString(obj),"observation")==0) {
                     pick|=PICK_OBS;
                  } else if (strcmp(Tcl_GetString(obj),"metobs")==0) {
                     pick|=PICK_METOBS;
                  } else if (strcmp(Tcl_GetString(obj),"fstdfield")==0) {
                     pick|=PICK_FSTDFIELD;
                  }
               }
            }

            if (Objc>=6) {
               Tcl_GetBooleanFromObj(Interp,Objv[5],&vp->ForcePick);
            }

            trViewport(GLRender->TRCon,(int)vp->x,Tk_Height(Tk_CanvasTkwin(vp->canvas))-(vp->y+vp->Height),vp->Width,vp->Height);
            ViewportCrowdClear(vp);
            glPickInit(x,Tk_Height(Tk_CanvasTkwin(vp->canvas))-y,2.0,2.0);
            glGetDoublev(GL_PROJECTION_MATRIX,vp->GLPick);
            ViewportSetup(vp->canvas,vp,proj,Tk_Width(Tk_CanvasTkwin(vp->canvas)),Tk_Height(Tk_CanvasTkwin(vp->canvas)),0,0,0);
            Projection_Setup(vp,proj,0);

            /*Rendue des donnees vectorielle*/
            for (i=0;i<vp->DataItem.Nb;i++) {
               glPushName(i);
               if ((pick&PICK_FSTDFIELD) && (data=Data_Get(vp->DataItem.Array[i]))) {
                  Data_Render(NULL,data,vp,proj,GL_SELECT,GL_VECTOR);
               }
               if ((pick&PICK_OBS) && (obs=Obs_Get(vp->DataItem.Array[i]))) {
                  Obs_Render(NULL,obs,vp,proj,GL_SELECT);
               }
#ifdef HAVE_ECBUFR
               if (pick&PICK_METOBS) {
                  MetObs_Render(NULL,MetObs_Get(vp->DataItem.Array[i]),vp,proj,GL_SELECT);
               }
#endif
               if ((pick&PICK_TRAJ) && (traj=Traj_Get(vp->DataItem.Array[i]))) {
                  Traj_Render(NULL,traj,vp,proj,GL_SELECT);
               }
               glPopName();
            }

            if (glPickProcess()>0) {
               obj=Tcl_NewListObj(0,NULL);
               switch(GLRender->GLPick[1]) {
                  case PICK_TRAJ:      Tcl_ListObjAppendElement(Interp,obj,Tcl_NewStringObj("trajectory",-1));  break;
                  case PICK_OBS:       Tcl_ListObjAppendElement(Interp,obj,Tcl_NewStringObj("observation",-1)); break;
                  case PICK_METOBS:    Tcl_ListObjAppendElement(Interp,obj,Tcl_NewStringObj("metobs",-1)); break;
                  case PICK_FSTDFIELD: Tcl_ListObjAppendElement(Interp,obj,Tcl_NewStringObj("fstdfield",-1)); break;
               }

               Tcl_ListObjAppendElement(Interp,obj,Tcl_NewStringObj(vp->DataItem.Array[GLRender->GLPick[0]],-1));

               switch(GLRender->GLPick[1]) {
#ifdef HAVE_ECBUFR
                  case PICK_METOBS:    Tcl_ListObjAppendElement(Interp,obj,Tcl_NewStringObj(MetObs_GetTag(MetObs_Get(vp->DataItem.Array[GLRender->GLPick[0]]),GLRender->GLPick[2]),-1)); break;
#endif
                  case PICK_TRAJ:
                  case PICK_OBS:
                  case PICK_FSTDFIELD: Tcl_ListObjAppendElement(Interp,obj,Tcl_NewIntObj(GLRender->GLPick[2])); break;
               }

               Tcl_ListObjAppendElement(Interp,obj,Tcl_NewIntObj(GLRender->GLPick[3]));
               Tcl_SetObjResult(Interp,obj);
            }
            vp->ForcePick=1;
            break;

         case BBOX:
            if (Objc!=2){
               Tcl_WrongNumArgs(Interp,0,Objv,"");
               return(TCL_ERROR);
            }
            loc0.Lat=90.0;  loc0.Lon=360.0;
            loc1.Lat=-90.0; loc1.Lon=-360.0;
            n=0;

            pt0[1]=0;
            pt1[1]=vp->Height;

            for(ix=0;ix<vp->Width;ix+=5) {
               pt0[0]=ix;
               pt1[0]=ix;

               if (proj->Type->UnProject(vp,proj,&loc,pt0)) {
                  loc0.Lat=FMIN(loc0.Lat,loc.Lat);
                  loc0.Lon=FMIN(loc0.Lon,loc.Lon);
                  loc1.Lat=FMAX(loc1.Lat,loc.Lat);
                  loc1.Lon=FMAX(loc1.Lon,loc.Lon);
                  n++;
               }
               if (proj->Type->UnProject(vp,proj,&loc,pt1)) {
                  loc0.Lat=FMIN(loc0.Lat,loc.Lat);
                  loc0.Lon=FMIN(loc0.Lon,loc.Lon);
                  loc1.Lat=FMAX(loc1.Lat,loc.Lat);
                  loc1.Lon=FMAX(loc1.Lon,loc.Lon);
                  n++;
               }
            }

            pt0[0]=0;
            pt1[0]=vp->Width;
            for(iy=0;iy<vp->Width;iy+=5) {
               pt0[1]=iy;
               pt1[1]=iy;

               if (proj->Type->UnProject(vp,proj,&loc,pt0)) {
                  loc0.Lat=FMIN(loc0.Lat,loc.Lat);
                  loc0.Lon=FMIN(loc0.Lon,loc.Lon);
                  loc1.Lat=FMAX(loc1.Lat,loc.Lat);
                  loc1.Lon=FMAX(loc1.Lon,loc.Lon);
                  n++;
               }
               if (proj->Type->UnProject(vp,proj,&loc,pt1)) {
                  loc0.Lat=FMIN(loc0.Lat,loc.Lat);
                  loc0.Lon=FMIN(loc0.Lon,loc.Lon);
                  loc1.Lat=FMAX(loc1.Lat,loc.Lat);
                  loc1.Lon=FMAX(loc1.Lon,loc.Lon);
                  n++;
               }
            }

            if (!n) {
               loc0.Lat=-90.0; loc0.Lon=-180.0;
               loc1.Lat=90.0;  loc1.Lon=180.0;
            }
            obj=Tcl_NewListObj(0,NULL);
            Tcl_ListObjAppendElement(Interp,obj,Tcl_NewDoubleObj(loc0.Lat));
            Tcl_ListObjAppendElement(Interp,obj,Tcl_NewDoubleObj(loc0.Lon));
            Tcl_ListObjAppendElement(Interp,obj,Tcl_NewDoubleObj(loc1.Lat));
            Tcl_ListObjAppendElement(Interp,obj,Tcl_NewDoubleObj(loc1.Lon));
            Tcl_SetObjResult(Interp,obj);
            break;
         }
   }
   return(TCL_OK);
}

/*----------------------------------------------------------------------------
 * Nom      : <ViewportCoords>
 * Creation : ???
 *
 * But      : This procedure is invoked to process the "coords" widget
 *       command on pixmap items.  See the user documentation for
 *       details on what it does
 *
 * Parametres :
 *  <Interp>  : Interpreter for error reporting
 *  <Canvas>  : Canvas containing item
 *  <Item>    : Item whose coordinates are to be read or modified
 *  <Argc>    : Number of coordinates supplied in argv
 *  <Argv>    : Array of coordinates: x1, y1, x2, y2, ...
 *
 * Retour:
 *  <TCL_..>  : Code de reussite TCL.
 *
 * Remarques :
 *
 *----------------------------------------------------------------------------
*/
static int ViewportCoords(Tcl_Interp *Interp,Tk_Canvas Canvas,Tk_Item *Item,int Argc,Tcl_Obj *const Argv[]){

   ViewportItem *vp=(ViewportItem *)Item;
   char x[TCL_DOUBLE_SPACE],y[TCL_DOUBLE_SPACE];

   if (Argc == 0) {
      Tcl_PrintDouble(Interp,vp->x,x);
      Tcl_PrintDouble(Interp,vp->y,y);
      Tcl_AppendResult(Interp,x," ",y,(char *)NULL);
   } else if (Argc == 2) {
      if ((Tk_CanvasGetCoordFromObj(Interp,Canvas,Argv[0],&vp->x) != TCL_OK) ||
          (Tk_CanvasGetCoordFromObj(Interp,Canvas,Argv[1],&vp->y) != TCL_OK)) {
          return(TCL_ERROR);
      }
      ViewportBBox(Canvas,vp);
   } else {
      Tcl_AppendResult(Interp,"ViewportCoords: wrong # coordinates,  expected 0 or 2\n",(char*)NULL);
      return(TCL_ERROR);
   }
   return(TCL_OK);
}

/*----------------------------------------------------------------------------
 * Nom      : <ViewportConfigure>
 * Creation : ???
 *
 * But      : This procedure is invoked to configure various aspects
 *       of a pixmap item, such as its anchor position.
 *
 * Parametres :
 *  <Interp>  : Interpreter for error reporting
 *  <Canvas>  : Canvas containing item
 *  <Item>    : Viewport item to reconfigure
 *  <Argc>    : Number of elements in argv
 *  <Argv>    : Arguments describing things to configure
 *  <Flags>   : Flags to pass to Tk_ConfigureWidget
 *
 * Retour:
 *  <TCL_..>  : Code de reussite TCL.
 *
 * Remarques :
 *
 *----------------------------------------------------------------------------
*/
static int ViewportConfigure(Tcl_Interp *Interp,Tk_Canvas Canvas,Tk_Item *Item,int Argc,Tcl_Obj *const Argv[],int Flags){

   ViewportItem *vp =(ViewportItem *)Item;
   Projection   *proj=NULL;
   int           width,height,frame;

   width  = vp->Width;
   height = vp->Height;
   frame  = vp->Frame;

   if (Tk_ConfigureWidget(Interp,Tk_CanvasTkwin(Canvas),configSpecs,Argc,(const char**)Argv,(char*)vp,Flags) != TCL_OK) {
      return(TCL_ERROR);
   }

   if (width!=vp->Width || height!=vp->Height) {
      vp->Realloc=1;
   }

   /*Liberation des frames*/
   if (GLRender->Resolution>1) {
      vp->Frame=0;
   }

   /*Check frame count*/
   vp->Frame=vp->Frame<0?0:vp->Frame;
   if (vp->Frame>NBFRAMEMAX-1) {
      vp->Frame=vp->Frame>NBFRAMEMAX-1;
      Tcl_AppendResult(Interp,"ViewportConfigure: Too many frame defined\n",(char*)NULL);
      return(TCL_ERROR);
   }

   if (vp->Realloc || (vp->Frame==0 && frame!=0)) {
      ViewportClean(vp,0,1);
   }


   if (!vp->Frame || !vp->Frames[vp->Frame]) {
      vp->Update=1;
   }

   /*Calculer le ratio*/
   if (vp->Cam) {
      if (vp->Width>vp->Height) {
         vp->Ratio=vp->Cam->Aspect/vp->Height*2;
      } else {
         vp->Ratio=vp->Cam->Aspect/vp->Width*2;
      }
   } else {
      vp->Ratio = 0;
   }
   ViewportBBox(Canvas,vp);

   /*If the window associated with the canvas is mapped, apply transforms*/
   proj=Projection_Get(vp->Projection);
   if (Tk_IsMapped(Tk_CanvasTkwin(Canvas)) && proj) {
      ViewportSetup(Canvas,vp,proj,Tk_Width(Tk_CanvasTkwin(Canvas)),Tk_Height(Tk_CanvasTkwin(Canvas)),0,1,0);
      Projection_Setup(vp,proj,0);
   }

   return(TCL_OK);
}

/*----------------------------------------------------------------------------
 * Nom      : <ViewportDelete>
 * Creation : ???
 *
 * But      : Cette procedure effectue le nettoyage memoire des structures du
 *            viewport.
 *
 * Parametres :
 *  <Canvas>  : Info about overall canvas widget
 *  <Item>    : Item that is being deleted
 *  <Disp>    : Display containing window for canvas
 *
 * Retour:
 *
 * Remarques :
 *
 *----------------------------------------------------------------------------
*/
static void ViewportDelete(Tk_Canvas Canvas,Tk_Item *Item,Display *Disp){

   ViewportItem *vp  =(ViewportItem *)Item;
   Projection   *proj=NULL;
   int           i;

   ViewportClean(vp,1,1);

   Tk_FreeFont(vp->tkfont);
   if (vp->BGColor) Tk_FreeColor(vp->BGColor);
   if (vp->FGColor) Tk_FreeColor(vp->FGColor);
//   Tk_FreeColor(vp->ColorCoast);
   if (vp->ColorLake)   Tk_FreeColor(vp->ColorLake);
   if (vp->ColorRiver)  Tk_FreeColor(vp->ColorRiver);
   if (vp->ColorPolit)  Tk_FreeColor(vp->ColorPolit);
   if (vp->ColorAdmin)  Tk_FreeColor(vp->ColorAdmin);
   if (vp->ColorCity)   Tk_FreeColor(vp->ColorCity);
   if (vp->ColorPlace)  Tk_FreeColor(vp->ColorPlace);
   if (vp->ColorRoad)   Tk_FreeColor(vp->ColorRoad);
   if (vp->ColorRail)   Tk_FreeColor(vp->ColorRail);
   if (vp->ColorUtil)   Tk_FreeColor(vp->ColorUtil);
   if (vp->ColorCanal)  Tk_FreeColor(vp->ColorCanal);
   if (vp->ColorCoord)  Tk_FreeColor(vp->ColorCoord);
   if (vp->ColorFCoast) Tk_FreeColor(vp->ColorFCoast);
   if (vp->ColorFLake)  Tk_FreeColor(vp->ColorFLake);

   if (vp->Command) {
      Tcl_DeleteCommand(((TkCanvas*)Canvas)->interp,vp->Command);
      Tcl_Free((char*)vp->Command);
   }

   if (vp->DataItem.Array) {
      free(vp->DataItem.String);
      Tcl_Free((char*)vp->DataItem.Array);
   }
   if (vp->MaskItem.Array) {
      free(vp->MaskItem.String);
      Tcl_Free((char*)vp->MaskItem.Array);
   }

   ViewportCrowdClear(vp);

   /*Cleanup the projection in case it used a viewport field*/
   if (vp->Projection) {
      proj=Projection_Get(vp->Projection);
      if (proj) {
         if (proj->VP==vp) {
            proj->VP=NULL;
         }
      }
      free(vp->Projection);
   }

   /*Remove for viewport table*/
   for (i=0;i<VPMAX;i++){
      if (ViewportTable[i]==vp) {
         ViewportTable[i]=NULL;
         break;
      }
   }
}

/*----------------------------------------------------------------------------
 * Nom      : <ViewportBBox>
 * Creation : ???
 *
 * But      : This procedure is invoked to compute the bounding box of
 *       all the pixels that may be drawn as part of a pixmap item.
 *       This procedure is where the child pixmap's placement is
 *       computed
 *
 * Parametres :
 *  <Canvas>  : Canvas that contains item
 *  <VP>      : Item whose bbox is to be recomputed
 *
 * Retour:
 *
 * Remarques :
 *
 *----------------------------------------------------------------------------
*/
static void ViewportBBox(Tk_Canvas Canvas,ViewportItem *VP){

   int x, y;

   x=(int)(VP->x+((VP->x>=0)?0.5:- 0.5));
   y=(int)(VP->y+((VP->y>=0)?0.5:- 0.5));

   if (VP->Width==0 || VP->Height==0) {
      VP->header.x1 = VP->header.x2 = x;
      VP->header.y1 = VP->header.y2 = y;
      return;
   }

   /*Compute location and size of pixmap, using anchor information.*/

   switch (VP->anchor) {
      case TK_ANCHOR_N:
         x -= VP->Width/2;
         break;
      case TK_ANCHOR_NE:
         x -= VP->Width;
         break;
      case TK_ANCHOR_E:
         x -= VP->Width;
         y -= VP->Height/2;
         break;
      case TK_ANCHOR_SE:
         x -= VP->Width;
         y -= VP->Height;
         break;
      case TK_ANCHOR_S:
         x -= VP->Width/2;
         y -= VP->Height;
         break;
      case TK_ANCHOR_SW:
         y -= VP->Height;
         break;
      case TK_ANCHOR_W:
         y -= VP->Height/2;
         break;
      case TK_ANCHOR_NW:
         break;
      case TK_ANCHOR_CENTER:
         x -= VP->Width/2;
         y -= VP->Height/2;
         break;
   }

   /*Store the information in the item header.*/
   VP->header.x1 = x ;
   VP->header.y1 = y ;
   VP->header.x2 = x + VP->Width;
   VP->header.y2 = y + VP->Height;

   /*Translation deltas related to canvas/viewport location*/
   VP->VX=VP->header.x1-ViewportX(VP);
   VP->VY=Tk_Height(Tk_CanvasTkwin(VP->canvas))-(VP->header.y1-ViewportY(VP)+VP->Height);
}

/*----------------------------------------------------------------------------
 * Nom      : <ViewportClean>
 * Creation : Fervier 2001
 *
 * But      : Reinitialiser les structures de donnees afin que les changements d'options
 *            soit pris en compte par tout les package.
 *
 * Parametres :
 *   <VP>     : Viewport
 *   <Data>   : Reinitialisation des donnees de champs
 *   <Buff>   : Buffer de retentions
 *
 * Retour:
 *
 * Remarques :
 *
 *----------------------------------------------------------------------------
*/
void ViewportClean(ViewportItem *VP,int Data,int Buff){

   TData *fld;
   int    i;

   if (VP) {
      if (Data) {
         for (i=0;i<VP->DataItem.Nb;i++) {
            fld=Data_Get(VP->DataItem.Array[i]);
            if (fld)
               Data_Clean(fld,1,1,1);
         }
      }
      if (Buff) {
         for (i=Buff-1;i<NBFRAMEMAX;i++) {
            if (VP->Frames[i]) {
               free(VP->Frames[i]);
               VP->Frames[i]=NULL;
            }
         }
         VP->Update=1;
      }
   }
}

/*----------------------------------------------------------------------------
 * Nom      : <ViewportRefresh>
 * Creation : Avril 2007
 *
 * But      : Activer le refresh du canvas associe au Viewport.
 *
 * Parametres :
 *   <VP>     : Viewport
 *   <Delay>  : Temps en millisecondes avant le refresh
 *
 * Retour:
 *
 * Remarques :
 *
 *----------------------------------------------------------------------------
*/
void ViewportRefresh(ClientData clientData,int Delay) {

   ViewportItem *vp=(ViewportItem*)clientData;

   int i,d=0;

   /*If viewport is not already being updated*/
   if (!vp->Update) {

      /*Check into viewport table for join projection*/
      for (i=0;i<VPMAX;i++){
         if (ViewportTable[i]==vp) d++;

         if (ViewportTable[i] && ViewportTable[i]->canvas==vp->canvas && strcmp(ViewportTable[i]->Projection,vp->Projection)==0) {
            ViewportTable[i]->Update=1;
         }
      }

      if (d && vp->canvas) {
         vp->Update=1;
         if (Delay<2000) {
            if (!vp->Timer) {
               vp->Timer=Tcl_CreateTimerHandler(Delay,ViewportRefresh_Canvas,vp->canvas);
            }
         } else {
            ViewportRefresh_Canvas(vp->canvas);
         }
      }
   }
}

void ViewportRefresh_Canvas(ClientData clientData) {
   extern void Tk_glCanvasEventuallyRedraw(Tk_Canvas canvas,int x1,int y1,int x2,int y2);

   Tk_glCanvasEventuallyRedraw((Tk_Canvas)clientData,1,1,2,2);
}

int ViewportRefresh_ThreadEventProc(Tcl_Event *Event,int Mask) {

   VPThreadEvent *ev=(VPThreadEvent*)Event;

   ViewportRefresh((ViewportItem*)ev->ptr,0);

   return(1);
}

/*----------------------------------------------------------------------------
 * Nom      : <ViewportDisplay>
 * Creation : ???
 *
 * But      : This procedure is invoked to draw a pixmap item in a given
 *       drawable.
 *
 * Parametres :
 *  <Canvas>  : Canvas that contains item
 *  <Item>    : Item to be displayed
 *  <Disp>    : Display on which to draw item
 *  <Draw>    : Pixmap or window in which to draw item
 *  <X>       : Describes region of canvas that must be redisplayed (not used)
 *  <Y>       : Describes region of canvas that must be redisplayed (not used)
 *  <Width>   : Describes region of canvas that must be redisplayed (not used)
 *  <Height>  : Describes region of canvas that must be redisplayed (not used)
 *
 * Retour:
 *
 * Remarques :
 *
 *----------------------------------------------------------------------------
*/

static void ViewportDisplay(Tk_Canvas Canvas,Tk_Item *Item,Display *Disp,Drawable Draw,int X,int Y,int Width,int Height){

   ViewportItem *vp=(ViewportItem*)Item;
   Projection   *proj;
   TData        *fld;
   TTraj        *traj;
   TObs         *obs;
   T3DModel     *mdl;
   GDAL_Band    *band;
   OGR_Layer    *layer;
   int           i,load;
   clock_t       sec;
   int           n;

   // When in batch mode, do not render unless this is the final output
   if (GLRender->XBatch && !GLRender->TRCon) {
      return;
   }

   /*Take care of automated refresh handler*/
   load=vp->Loading;
   Tcl_DeleteTimerHandler(vp->Timer);vp->Timer=NULL;
   if (GLRender->Delay<2000)
      ViewportRefresh(vp,GLRender->Delay);

   if ((proj=Projection_Get(vp->Projection))) {
      load+=(proj->Loading=proj->Loading<0?0:proj->Loading);

      /*Force update if rendering within PBuffer*/
      if (GLRender->TRCon) {

         /*Wait for everything to be loaded*/
         sec=clock();
         while (((clock()-sec)<(60*CLOCKS_PER_SEC)) && (!GDB_ThreadQueueIsEmpty(0x0) || (vp->Loading+proj->Loading)));
         if ((clock()-sec)>=(60*CLOCKS_PER_SEC)) {
            fprintf(stdout,"(WARNING) ViewportDisplay: Waited too long for data, rendering anyway\n");
         }
         vp->Update=1;
      }

      /*Force update on XExposure event (if in hardware mode) and backbuffer refresh*/
      if ((GLRender->XExpose>0 && !GLRender->Soft) || !vp->Frames[vp->Frame] || load) {
         vp->Update=1;
      }

      /*If update is needed or the back buffer frame is not rendered yet*/
      if (vp->Update || (!vp->Frames[vp->Frame])) {

         ViewportSet(vp,proj);
         ViewportSetup(Canvas,vp,proj,Width,Height,0,1,0);
         Projection_Setup(vp,proj,1);
         ViewportCrowdClear(vp);

        /*Allouer les frames de retentions si ce n'est pas deja fait*/
         if (!vp->Frames[vp->Frame]) {
            vp->Frames[vp->Frame]=(GLubyte*)malloc(vp->Width*vp->Height*4);
         }

         /*Effectuer le rendue des champs*/
         Projection_Render(NULL,vp,proj,GL_ALL);
         ProjCam_Render(vp->Cam,proj);

        /*Rendue des donnees raster*/
         for (i=0;i<vp->DataItem.Nb;i++) {
            fld=Data_Get(vp->DataItem.Array[i]);
            if (fld) {
               Data_Render(NULL,fld,vp,proj,GL_RENDER,GL_RASTER);
               if (GLRender->GLZBuf)
                  Data_Render(NULL,fld,vp,proj,GL_RENDER,GL_VECTOR);
            }
            if ((layer=OGR_LayerGet(vp->DataItem.Array[i]))) {
               OGR_LayerRender(NULL,proj,vp,layer,0);
            }
            if ((band=GDAL_BandGet(vp->DataItem.Array[i]))) {
               GDAL_BandRender(proj,vp,band);
            }
            if ((mdl=Model_Get(vp->DataItem.Array[i]))) {
               Model_Render(proj,vp,mdl);
            }
         }

         /*Rendue des donnees vectorielle*/
         for (i=0;i<vp->DataItem.Nb;i++) {
            if (!GLRender->GLZBuf) {
               if ((fld=Data_Get(vp->DataItem.Array[i]))) {
                  Data_Render(NULL,fld,vp,proj,GL_RENDER,GL_VECTOR);
               }
            }
            if ((obs=Obs_Get(vp->DataItem.Array[i]))) {
               Obs_Render(NULL,obs,vp,proj,GL_RENDER);
            }
#ifdef HAVE_ECBUFR
            TMetObs *met;
            if ((met=MetObs_Get(vp->DataItem.Array[i]))) {
               MetObs_Render(NULL,met,vp,proj,GL_RENDER);
            }
#endif
            if ((traj=Traj_Get(vp->DataItem.Array[i]))) {
               Traj_Render(NULL,traj,vp,proj,GL_RENDER);
            }
         }

         if (vp->Frames[vp->Frame] && GLRender->XExpose<=1 && !GLRender->TRCon) {
            glReadBuffer(GL_BACK);
            glReadPixels(vp->header.x1-((TkCanvas*)Canvas)->xOrigin,Height-vp->header.y2+((TkCanvas*)Canvas)->yOrigin,vp->Width,vp->Height,GL_RGBA,GL_UNSIGNED_BYTE,vp->Frames[vp->Frame]);
         }
         ViewportUnset(vp);
      } else {
         /*Copy the backbuffer*/
         if (vp->Frames[vp->Frame]) {
            trRasterPos2i(vp->header.x1-((TkCanvas*)Canvas)->xOrigin,-(vp->header.y2-((TkCanvas*)Canvas)->yOrigin));
            glDrawPixels(vp->Width,vp->Height,GL_RGBA,GL_UNSIGNED_BYTE,vp->Frames[vp->Frame]);
         }
      }
   }

   /*Pourtour*/
   if (vp->FGColor && vp->BDWidth) {
      glLineWidth(vp->BDWidth);
      glPolygonMode(GL_FRONT,GL_LINE);
      glColor3us(vp->FGColor->red,vp->FGColor->green,vp->FGColor->blue);

      /*There's a 1 pixel offset on X0 when saving image in GPU mode, can't find why*/
      n=(GLRender->TRCon && !GLRender->Soft)?1:0;
      glBegin(GL_QUADS);
         glVertex2i(vp->header.x1-((TkCanvas*)Canvas)->xOrigin+n,vp->header.y1-n-((TkCanvas*)Canvas)->yOrigin);
         glVertex2i(vp->header.x1-((TkCanvas*)Canvas)->xOrigin+n,vp->header.y2-((TkCanvas*)Canvas)->yOrigin);
         glVertex2i(vp->header.x2-((TkCanvas*)Canvas)->xOrigin+n,vp->header.y2-((TkCanvas*)Canvas)->yOrigin);
         glVertex2i(vp->header.x2-((TkCanvas*)Canvas)->xOrigin+n,vp->header.y1-n-((TkCanvas*)Canvas)->yOrigin);
      glEnd();
   }

   /*Mask intrusions*/
   ViewportIntrusion(NULL,Canvas,Item);
   ViewportLicense(NULL,vp,proj);

   /*Loading data*/
   if (load && !GLRender->TRCon) {
      glMatrixMode(GL_PROJECTION);
      glPushMatrix();
      glEnable(GL_BLEND);
      glPolygonMode(GL_FRONT_AND_BACK,GL_FILL);
      glTranslated(16+vp->header.x1+(vp->header.x2-vp->header.x1)*0.5-((TkCanvas*)Canvas)->xOrigin,16+vp->header.y1+(vp->header.y2-vp->header.y1)*0.5-((TkCanvas*)Canvas)->yOrigin,0.0);
      glScalef(16,16,1.0);
      glColor4us(0x00,0x00,0x00,16384);
      glDrawCircle(64,GL_POLYGON);
      glColor4us(0xFFFF,0xFFFF,0x00,32768);
      glDrawArc(0,((load%100)/100.0*360),10,GL_POLYGON,0);
      glDisable(GL_BLEND);
      glPopMatrix();
   }

   vp->Update =0;
   vp->Realloc=0;
}

/*----------------------------------------------------------------------------
 * Nom      : <ViewportLicense>
 * Creation : Novembre 2010
 *
 * But      : This procedure is invoked to draw an intrusion into th viewport.
 *
 * Parametres :
 *  <Interp>  : Tcl Interpreter
 *  <VP>      : Viewport
 *  <Proj>    : Projection
 *
 * Retour:
 *
 * Remarques :
 *
 *----------------------------------------------------------------------------
*/
static void ViewportLicense(Tcl_Interp *Interp,ViewportItem *VP,Projection *Proj) {

   Tk_TextLayout text;
   int           width,height;

   if (Proj->License) {
      text=Tk_ComputeTextLayout(VP->tkfont,Proj->License,Tcl_NumUtfChars(Proj->License,strlen(Proj->License)),0,TK_JUSTIFY_CENTER,0,&width,&height);
      glFontUse(Tk_Display(Tk_CanvasTkwin(VP->canvas)),VP->tkfont);
      if (Interp) {
         glPostscripTextLayout(Interp,VP->canvas,text,VP->FGColor,NULL,0,VP->header.x2/2-width/2,VP->header.y2-height-10,TK_ANCHOR_NW,TK_JUSTIFY_CENTER);
      } else {
         glDisplayTextLayout(text,0,VP->header.x2/2-width/2,VP->header.y2-height-10,0,-1,1);
      }
      Tk_FreeTextLayout(text);
   }
}

/*----------------------------------------------------------------------------
 * Nom      : <ViewportIntrusion>
 * Creation : Novembre 2010
 *
 * But      : This procedure is invoked to draw an intrusion into th viewport.
 *
 * Parametres :
 *  <Interp>  : Tcl Interpreter
 *  <Canvas>  : Canvas that contains item
 *  <Item>    : Item to be displayed
 *
 * Retour:
 *
 * Remarques :
 *
 *----------------------------------------------------------------------------
*/
static void ViewportIntrusion(Tcl_Interp *Interp,Tk_Canvas Canvas,Tk_Item *Item) {

   ViewportItem *vp=(ViewportItem *)Item;
   double       coords[8];
   char         buf[100];

   Tk_Item      *item;
   Tcl_Obj      *obj;
   Vect3d        clips[5],v[5];
   double        vps[4];
   int           i,j,n,nbclips,c0,c1;

   /*Mask inslusions*/
   if (vp->MaskItem.Array) {

      obj=Tcl_NewObj();
      for(i=0;i<vp->MaskItem.Nb;i++) {

         Tcl_SetStringObj(obj,vp->MaskItem.Array[i],-1);
         if (!(item=Tk_glGetItem(Canvas,obj))) {
            continue;
         }

         n=5;
         vps[0]=vp->header.x1-vp->BDWidth;
         vps[1]=vp->header.y1-vp->BDWidth;
         vps[2]=vp->header.x2+vp->BDWidth;
         vps[3]=vp->header.y2+vp->BDWidth;

         Vect_Init(v[0],item->x1+1,item->y1+1,0.0);
         Vect_Init(v[1],item->x2-1,item->y1+1,0.0);
         Vect_Init(v[2],item->x2-1,item->y2-1,0.0);
         Vect_Init(v[3],item->x1+1,item->y2-1,0.0);

         if (INSIDE(v[0],vps[0],vps[1],vps[2],vps[3]) || INSIDE(v[1],vps[0],vps[1],vps[2],vps[3]) ||
             INSIDE(v[2],vps[0],vps[1],vps[2],vps[3]) || INSIDE(v[3],vps[0],vps[1],vps[2],vps[3])) {

            /*Get the box corners*/
            Vect_Init(v[0],item->x1-vp->MaskWidth,item->y1-vp->MaskWidth,0.0);
            Vect_Init(v[1],item->x2+vp->MaskWidth,item->y1-vp->MaskWidth,0.0);
            Vect_Init(v[2],item->x2+vp->MaskWidth,item->y2+vp->MaskWidth,0.0);
            Vect_Init(v[3],item->x1-vp->MaskWidth,item->y2+vp->MaskWidth,0.0);
            Vect_Init(v[4],item->x1-vp->MaskWidth,item->y1-vp->MaskWidth,0.0);

            /*Clip the polygon interior to define the intrusion*/
            if (LiangBarsky_PolygonClip2D(v,n,clips,&nbclips,vps[0],vps[1],vps[2],vps[3])) {

               if (Interp) {
                  coords[0]=clips[0][0]; coords[1]=clips[0][1];
                  coords[2]=clips[1][0]; coords[3]=clips[1][1];
                  coords[4]=clips[2][0]; coords[5]=clips[2][1];
                  coords[6]=clips[3][0]; coords[7]=clips[3][1];
                  Tk_CanvasPsColor(Interp,Canvas,Tk_3DBorderColor(((TkCanvas*)Canvas)->bgBorder));
                  Tk_CanvasPsPath(Interp,Canvas,coords,4);
                  Tcl_AppendResult(Interp,"closepath fill\n",(char*)NULL);
                  sprintf(buf,"%i setlinewidth 1 setlinecap 1 setlinejoin\n",vp->BDWidth);
                  Tcl_AppendResult(Interp,buf,(char*)NULL);
                  Tk_CanvasPsColor(Interp,Canvas,vp->FGColor);
               } else {
                  glPolygonMode(GL_FRONT_AND_BACK,GL_FILL);
                  glColor3us(Tk_3DBorderColor(((TkCanvas*)Canvas)->bgBorder)->red,
                     Tk_3DBorderColor(((TkCanvas*)Canvas)->bgBorder)->green,Tk_3DBorderColor(((TkCanvas*)Canvas)->bgBorder)->blue);

                  glBegin(GL_QUADS);
                     glVertex2d(clips[0][0]-((TkCanvas*)Canvas)->xOrigin,clips[0][1]-((TkCanvas*)Canvas)->yOrigin);
                     glVertex2d(clips[1][0]-((TkCanvas*)Canvas)->xOrigin,clips[1][1]-((TkCanvas*)Canvas)->yOrigin);
                     glVertex2d(clips[2][0]-((TkCanvas*)Canvas)->xOrigin,clips[2][1]-((TkCanvas*)Canvas)->yOrigin);
                     glVertex2d(clips[3][0]-((TkCanvas*)Canvas)->xOrigin,clips[3][1]-((TkCanvas*)Canvas)->yOrigin);
                  glEnd();
               }

               /*Clip the coutour per line segment so as to only drwa the intrusion*/
               glLineWidth(vp->BDWidth);
               glColor3us(vp->FGColor->red,vp->FGColor->green,vp->FGColor->blue);
               glBegin(GL_LINES);
               for(j=0;j<n-1;j++) {
                  Vect_Assign(clips[0],v[j]);
                  Vect_Assign(clips[1],v[j+1]);
                  if (LiangBarsky_LineClip2D(clips[0],clips[1],&c0,&c1,vps[0],vps[1],vps[2],vps[3])) {
                     if (Interp) {
                        coords[0]=clips[0][0]; coords[1]=clips[0][1];
                        coords[2]=clips[1][0]; coords[3]=clips[1][1];
                        Tk_CanvasPsPath(Interp,Canvas,coords,2);
                     } else {
                        glVertex2d(clips[0][0]-((TkCanvas*)Canvas)->xOrigin,clips[0][1]-((TkCanvas*)Canvas)->yOrigin);
                        glVertex2d(clips[1][0]-((TkCanvas*)Canvas)->xOrigin,clips[1][1]-((TkCanvas*)Canvas)->yOrigin);
                     }
                  }
                  if (Interp)
                    Tcl_AppendResult(Interp,"stroke\n",(char*)NULL);
               }
               glEnd();
            }
         }
      }
      Tcl_DecrRefCount(obj);
   }
}

/*----------------------------------------------------------------------------
 * Nom      : <ViewportSetup>
 * Creation : Janvier 2002
 *
 * But      : Initialiser le viewport de projection et la position du viewer.
 *
 * Parametres :
 *  <VP>      : Parametres du viewport
 *  <Proj>    : Parametres de la projection
 *  <Width>   : Largeur du canvas
 *  <Height>  : Hauteur du canvas
 *  <Tile>    : Utilisation du mode Tile
 *  <Clear>   : Reinitialiser avant de faire le setup
 *  <PS>      : Mode Postscript
 *
 * Retour:
 *
 * Remarques :
 *
 *----------------------------------------------------------------------------
*/
void ViewportOrtho(ViewportItem *VP){

   glMatrixMode(GL_PROJECTION);
   glPushMatrix();
   glLoadIdentity();

   gluOrtho2D(0,VP->Width-1,0,VP->Height-1);
}

void ViewportUnOrtho(ViewportItem *VP){

   glMatrixMode(GL_PROJECTION);
   glPopMatrix();
}

void ViewportSetup(Tk_Canvas Canvas,ViewportItem *VP,Projection *Proj,int Width,int Height,int Tile,int Clear,int PS){

   double as,z,dl;
   int x,y;

   if (!VP || !Proj)
      return;

   if (GLRender->MagScale>1)
      Height=GLRender->MagY+GLRender->MagD/GLRender->MagScale;

   x=(VP->header.x1-((TkCanvas*)Canvas)->xOrigin-GLRender->MagX)*GLRender->MagScale+GLRender->MagD/2.0;
   y=(Height-(VP->header.y1-((TkCanvas*)Canvas)->yOrigin+VP->Height))*GLRender->MagScale-GLRender->MagD/2.0;

   glDepthFunc(GL_LESS);
   glDepthMask(GL_TRUE);
   if (VP->BGColor)
      glClearColor(VP->BGColor->red/65535.0f,VP->BGColor->green/65535.0f,VP->BGColor->blue/65535.0f,0.0f);
   glEnable(GL_CULL_FACE);
   glMatrixMode(GL_PROJECTION);

   if (Clear) {
      glLoadIdentity();
   }

   if (PS) {
      glViewport(0,0,VP->Width,VP->Height);
   } else {
      trViewport(GLRender->TRCon,x,y,VP->Width*GLRender->MagScale,VP->Height*GLRender->MagScale);
   }

   if (Proj->Type->Def==PROJGLOBE) {
      z=Proj->Perspective?1.0+VP->Cam->Clip*0.85:VP->Cam->Clip;
   } else {
      z=Proj->Perspective?3.0+VP->Cam->Clip*0.85:4.0*VP->Cam->Clip;
   }

   /*Ajuster la projection pour garder un aspect correct*/
   if ((VP->Width/Proj->LI)>(VP->Height/Proj->LJ)) {
      as=(double)VP->Width/VP->Height;
      dl=Proj->LJ;
      if (Tile) {
         trOrtho(GLRender->TRCon,-VP->Cam->Aspect*as*dl,VP->Cam->Aspect*as*dl,-VP->Cam->Aspect*dl,VP->Cam->Aspect*dl,-VP->Cam->Aspect,z);
      } else {
         if (Proj->Perspective) {
            dl*=0.35;
            glFrustum(-VP->Cam->Aspect*as*dl,VP->Cam->Aspect*as*dl,-VP->Cam->Aspect*dl,VP->Cam->Aspect*dl,1.0,z);
         } else {
            glOrtho(-VP->Cam->Aspect*as*dl,VP->Cam->Aspect*as*dl,-VP->Cam->Aspect*dl,VP->Cam->Aspect*dl,-VP->Cam->Aspect,z);
         }
      }

   } else {
      as=(double)VP->Height/VP->Width;
      dl=Proj->LI;
      if (Tile) {
         trOrtho(GLRender->TRCon,-VP->Cam->Aspect*dl,VP->Cam->Aspect*dl,-VP->Cam->Aspect*as*dl,VP->Cam->Aspect*as*dl,-VP->Cam->Aspect,z);
      } else {
         if (Proj->Perspective) {
            dl*=0.35;
            glFrustum(-VP->Cam->Aspect*as*dl,VP->Cam->Aspect*as*dl,-VP->Cam->Aspect*dl,VP->Cam->Aspect*dl,1.0,z);
         } else {
            glOrtho(-VP->Cam->Aspect*dl,VP->Cam->Aspect*dl,-VP->Cam->Aspect*as*dl,VP->Cam->Aspect*as*dl,-VP->Cam->Aspect,z);
         }
      }
   }

   VP->Cam->DOV[1]=VP->Cam->DOV[0]=VP->Cam->Aspect*as*dl;
   VP->Cam->FOV[1]=VP->Cam->FOV[0]=atan(VP->Cam->DOV[0]*0.5);

   /*Effectuer les manipulations aux niveau du modele*/
   glMatrixMode(GL_MODELVIEW);
   glLoadIdentity();

   /*In case of perspective projection*/
   if (Proj->Perspective) {
      glTranslatef (0.0, 0.0,-1.0);
   }

   /*Positionner la camera*/
   ProjCam_Place(VP->Cam);

   /*Conserver les parametres pour les transformations*/
   if (Clear && GLRender->MagScale<=1) {
      glGetDoublev(GL_MODELVIEW_MATRIX,VP->GLModS);
      glGetDoublev(GL_PROJECTION_MATRIX,VP->GLProj);
      glGetIntegerv(GL_VIEWPORT,VP->GLView);
      VP->GLView[0]=0;
      VP->GLView[1]=0;
   }
}

/*----------------------------------------------------------------------------
 * Nom      : <ViewportSet>
 * Creation : Novembre 2002
 *
 * But      : Initialiser le contexte OpenGL aux parametres du viewport.
 *
 * Parametres :
 *  <Proj>    : Parametres de la projection
 *  <VP>      : Parametres du viewport
 *
 * Retour:
 *
 * Remarques :
 *
 *----------------------------------------------------------------------------
*/
void ViewportClear(ViewportItem *VP,int Page) {

   glDisable(GL_CULL_FACE);
   glPolygonMode(GL_FRONT_AND_BACK,GL_FILL);
   glBegin(GL_QUADS);
   if (Page) {
      glVertex3i(VP->header.x1-((TkCanvas*)VP->canvas)->xOrigin,VP->header.y1-((TkCanvas*)VP->canvas)->yOrigin,-1);
      glVertex3i(VP->header.x1-((TkCanvas*)VP->canvas)->xOrigin,VP->header.y2-((TkCanvas*)VP->canvas)->yOrigin,-1);
      glVertex3i(VP->header.x2-((TkCanvas*)VP->canvas)->xOrigin,VP->header.y2-((TkCanvas*)VP->canvas)->yOrigin,-1);
      glVertex3i(VP->header.x2-((TkCanvas*)VP->canvas)->xOrigin,VP->header.y1-((TkCanvas*)VP->canvas)->yOrigin,-1);
   } else {
      glVertex3i(0,0,-1);
      glVertex3i(0,VP->Height-1,-1);
      glVertex3i(VP->Width-1,VP->Height-1,-1);
      glVertex3i(VP->Width-1,0,-1);
   }
   glEnd();
   glEnable(GL_CULL_FACE);
}

void ViewportSet(ViewportItem *VP,Projection *Proj) {

   glPushAttrib(GL_ALL_ATTRIB_BITS);
   glEnable(GL_STENCIL_TEST);
   glStencilMask(0xff);

   /*Dans le cas d'un mask terre, clear avec 0x2*/
   if (Proj->Geo->Params.Mask==1 || Proj->Geo->Params.Mask==-1) {
      glStencilFunc(GL_ALWAYS,0x2,0xf);
   } else {
      glStencilFunc(GL_ALWAYS,0x0,0xf);
   }
   glStencilOp(GL_REPLACE,GL_REPLACE,GL_REPLACE);

   glEnable(GL_DEPTH_TEST);
   glDepthFunc(GL_ALWAYS);
   glDepthMask(GL_TRUE);

   if (VP->BGColor)
      glColor3us(VP->BGColor->red,VP->BGColor->green,VP->BGColor->blue);
   ViewportClear(VP,1);
   glDisable(GL_DEPTH_TEST);
   glDisable(GL_STENCIL_TEST);

   glMatrixMode(GL_PROJECTION);
   glPushMatrix();
   glMatrixMode(GL_MODELVIEW);
   glPushMatrix();
}

/*----------------------------------------------------------------------------
 * Nom      : <ViewportUnSet>
 * Creation : Novembre 2002
 *
 * But      : De-Initialiser le contexte OpenGL aux parametres du viewport.
 *
 * Parametres :
 *  <VP>      : Parametres du viewport
 *
 * Retour:
 *
 * Remarques :
 *
 *----------------------------------------------------------------------------
*/
void ViewportUnset(ViewportItem *VP) {

   glMatrixMode(GL_MODELVIEW);
   glPopMatrix();
   glMatrixMode(GL_PROJECTION);
   glPopMatrix();
   glPopAttrib();
}

/*----------------------------------------------------------------------------
 * Nom      : <ViewportToPoint>
 * Creation : ???
 *
 * But      : Computes the distance from a given point to a given
 *       rectangle, in canvas units.
 *
 * Parametres :
 *  <Canvas>   : Canvas that contains item
 *  <Item>     : Item to check against point
 *  <CoordPtr> : Pointer to x and y coordinates
 *
 * Retour:
 *
 * Remarques :
 *
 *----------------------------------------------------------------------------
*/
static double ViewportToPoint(Tk_Canvas Canvas,Tk_Item *Item,double *CoordPtr){

   ViewportItem *vp=(ViewportItem *)Item;

   double xDiff,yDiff;

   /*Point is outside rectangle*/

   if (CoordPtr[0] < vp->header.x1) {
      xDiff = vp->header.x1 - CoordPtr[0];
   } else if (CoordPtr[0] > vp->header.x2) {
      xDiff = CoordPtr[0] - vp->header.x2;
   } else {
      xDiff = 0;
   }

   if (CoordPtr[1] < vp->header.y1) {
      yDiff = vp->header.y1 - CoordPtr[1];
   } else if (CoordPtr[1] > vp->header.y2) {
      yDiff = CoordPtr[1] - vp->header.y2;
   } else {
      yDiff = 0;
   }

   if (xDiff==0.0 && yDiff==0.0) {
      return(0.0);
   } else {
      return(hypot(xDiff,yDiff));
   }
}

/*----------------------------------------------------------------------------
 * Nom      : <ViewportToArea>
 * Creation : ???
 *
 * But      : This procedure is called to determine whether an item
 *       lies entirely inside, entirely outside, or overlapping
 *       a given rectangle.
 *
 * Parametres :
 *  <Canvas>   : Canvas that contains item
 *  <Item>     : Item to check against point
 *  <RectPtr>  : Pointer to array of four coordinates (x1, y1, x2, y2) describing rectangular
 *               areaPointer to x and y coordinates
 *
 * Retour:
 *
 * Remarques :
 *
 *----------------------------------------------------------------------------
*/
static int ViewportToArea(Tk_Canvas Canvas,Tk_Item *Item,double *RectPtr){

   ViewportItem *vp=(ViewportItem *)Item;

   if ((RectPtr[2] <= vp->header.x1) ||
       (RectPtr[0] >= vp->header.x2) ||
       (RectPtr[3] <= vp->header.y1) ||
       (RectPtr[1] >= vp->header.y2)) {
      return(-1);
   }
   if ((RectPtr[0] <= vp->header.x1) &&
       (RectPtr[1] <= vp->header.y1) &&
       (RectPtr[2] >= vp->header.x2) &&
       (RectPtr[3] >= vp->header.y2)) {
      return(1);
   }
   return(0);
}

/*----------------------------------------------------------------------------
 * Nom      : <ViewportScale>
 * Creation : ???
 *
 * But      : This procedure is invoked to rescale a pixmap item in a
 *       canvas.  It is one of the standard item procedures for
 *       pixmap items, and is invoked by the generic canvas code.
 *
 * Parametres :
 *  <Canvas>  : Canvas containing rectangle
 *  <Item>    : Rectangle to be scaled
 *  <OriginX> : Origin about which to scale item
 *  <OriginY> : Origin about which to scale item
 *  <ScaleX>  : Amount to scale in X direction
 *  <ScaleY>  : Amount to scale in Y direction
 *
 * Retour:
 *
 * Remarques :
 * The item referred to by itemPtr is rescaled so that the
 * following transformation is applied to all point coordinates:
 *    x' = originX + scaleX*(x-originX)
 *    y' = originY + scaleY*(y-originY)
 *
 *----------------------------------------------------------------------------
*/
static void ViewportScale(Tk_Canvas Canvas,Tk_Item *Item,double OriginX,double OriginY,double ScaleX,double ScaleY){

   ViewportItem *vp=(ViewportItem *)Item;

   vp->x = OriginX + ScaleX*(vp->x - OriginX);
   vp->y = OriginY + ScaleY*(vp->y - OriginY);
   vp->Width = ScaleX*vp->Width;
   vp->Height= ScaleY*vp->Height;
   ViewportBBox(Canvas,vp);

   vp->Update=1;
   vp->Realloc=1;
   ViewportClean(vp,0,1);
}

/*----------------------------------------------------------------------------
 * Nom      : <ViewportTranslate>
 * Creation : ???
 *
 * But      : This procedure is called to move an item by a given amount.
 *
 * Parametres :
 *  <Canvas>  : Canvas containing item
 *  <Item>    : Item that is being moved
 *  <DeltaX>  : Amount by which item is to be moved
 *  <DeltaY>  : Amount by which item is to be moved
 *
 * Retour:
 *
 * Remarques :
 *
 *----------------------------------------------------------------------------
*/
static void ViewportTranslate(Tk_Canvas Canvas,Tk_Item *Item,double DeltaX,double DeltaY){

   ViewportItem *vp=(ViewportItem *)Item;

   vp->x += DeltaX;
   vp->y += DeltaY;
   ViewportBBox(Canvas,vp);
}

/*----------------------------------------------------------------------------
 * Nom      : <ViewportToPostscript
 * Creation : ???
 *
 * But      : Generer le code Postscript pour un item Viewport.
 *
 * Parametres :
 *  <Interp>  : Interpreteur Tcl
 *  <Canvas>  : Item canvas
 *  <Item>    : Item pour lquel on genere le Postscript
 *  <Prepass> : Prepass pour recuperer les polices
 *
 * Retour:
 *  <TCL_..>  : Code de retour Tcl.
 *
 * Remarques :
 *
 *----------------------------------------------------------------------------
*/
static int ViewportToPostscript(Tcl_Interp *Interp,Tk_Canvas Canvas,Tk_Item *Item,int Prepass){

   Projection   *proj;
   ViewportItem *vp=(ViewportItem *)Item;
   GLXPbuffer   pbuf;
   double       coords[8];
   int          i,ras=0;
   unsigned int w,h;
   char         buf[100];
   clock_t      sec;

   TData     *fld;
   TTraj     *traj;
   TObs      *obs;
   T3DModel  *mdl;
   GDAL_Band *band;
   OGR_Layer *layer;

   if (!(proj=Projection_Get(vp->Projection))) {
      return(TCL_ERROR);
   }

   /*Definir le font du viewport*/
   if (Tk_CanvasPsFont(Interp,Canvas,vp->tkfont) != TCL_OK) {
      return(TCL_ERROR);
   }

   if (Prepass) {
      return(TCL_OK);
   }

   /*Coordonnee du viewport*/
   coords[0]=vp->header.x1+1 ; coords[1]=vp->header.y1;
   coords[2]=vp->header.x2   ; coords[3]=vp->header.y1;
   coords[4]=vp->header.x2   ; coords[5]=vp->header.y2;
   coords[6]=vp->header.x1+1 ; coords[7]=vp->header.y2;

   /*Creer le background*/
   if (vp->BGColor) {
      Tk_CanvasPsColor(Interp,Canvas,vp->BGColor);
      Tk_CanvasPsPath(Interp,Canvas,coords,4);
      Tcl_AppendResult(Interp,"closepath fill\n",(char*)NULL);
   }

   /*Creer le clipping box*/
   Tk_CanvasPsPath(Interp,Canvas,coords,4);
   Tcl_AppendResult(Interp,"closepath clip newpath\n",(char*)NULL);

   sprintf(buf,"gsave\n%.1f %.1f translate\n",vp->x,Tk_CanvasPsY(Canvas,vp->Height+vp->y));
   Tcl_AppendResult(Interp,buf,(char*)NULL);

   w=vp->Width;
   h=vp->Height;
   if (!(pbuf=glXGetPBuffer(Tk_CanvasTkwin(Canvas),&w,&h))) {
      Tcl_AppendResult(Interp,"ViewportToPostscript: Unable to allocate rendering PBuffer",(char*)NULL);
      return(TCL_ERROR);
   }

   if (!glXMakeContextCurrent(GLRender->XDisplay,pbuf,pbuf,GLRender->GLCon)) {
      Tcl_AppendResult(Interp,"ViewportToPostscript: Unable to link the pbuffer to the GLXContext",(char*)NULL);
      glXFreePBuffer(pbuf);
      return(TCL_ERROR);
   }

   /*Wait for everything to be loaded*/
   sec=clock();
   while (((clock()-sec)<(30*CLOCKS_PER_SEC)) && (!GDB_ThreadQueueIsEmpty(0x0) || (vp->Loading+proj->Loading)));
   if ((clock()-sec)>=(30*CLOCKS_PER_SEC)) {
      fprintf(stdout,"(WARNING) ViewportToPostscript: Warning, waited too long for data, rendering anyway\n");
   }

   /* Setup the tile rendering engine */
   GLRender->TRCon=trNew();
   trTileSize(GLRender->TRCon,w,h,0);
   trImageSize(GLRender->TRCon,vp->Width,vp->Height);

   ViewportSet(vp,proj);

   /* Render the polygonized geographical data */
   ViewportSetup(Canvas,vp,proj,0,0,0,1,1);
   Projection_Setup(vp,proj,1);
   proj->Type->DrawGlobe(Interp,vp,proj);
   GDB_TileRender(Interp,proj,proj->Geo,GDB_FILL);

   ViewportSetup(Canvas,vp,proj,Tk_Width(Tk_CanvasTkwin(Canvas)),Tk_Height(Tk_CanvasTkwin(Canvas)),1,1,0);
   Projection_Setup(vp,proj,1);

   /* Render the tiles */
   do {
      trBeginTile(GLRender->TRCon);

      /*Conserver les parametres pour les transformations*/
      glGetDoublev(GL_MODELVIEW_MATRIX,vp->GLModS);
      glGetDoublev(GL_PROJECTION_MATRIX,vp->GLProj);
      glGetIntegerv(GL_VIEWPORT,vp->GLView);

      if (proj->Geo->Params.Mask==1 || proj->Geo->Params.Mask==-1) {
         glClearStencil(0x2);
      } else {
         glClearStencil(0x0);
      }
      glStencilMask(0xFF);
      glClear(GL_COLOR_BUFFER_BIT|GL_DEPTH_BUFFER_BIT|GL_STENCIL_BUFFER_BIT);
      ViewportCrowdClear(vp);

      /*Generation des donnees raster*/
      ViewportSet(vp,proj);
      ras+=Projection_Render(Interp,vp,proj,GL_RASTER);
      for (i=0;i<vp->DataItem.Nb;i++) {
         fld=Data_Get(vp->DataItem.Array[i]);
         if (fld) {
            ras+=Data_Render(NULL,fld,vp,proj,GL_RENDER,GL_RASTER);
            if (GLRender->GLZBuf) {
               Data_Render(NULL,fld,vp,proj,GL_RENDER,GL_VECTOR);
            }
         }
         if ((layer=OGR_LayerGet(vp->DataItem.Array[i]))) {
            ras+=OGR_LayerRender(NULL,proj,vp,layer,0);
         }
         if ((band=GDAL_BandGet(vp->DataItem.Array[i]))) {
            ras+=GDAL_BandRender(proj,vp,band);
         }
         if ((mdl=Model_Get(vp->DataItem.Array[i]))) {
            ras+=Model_Render(proj,vp,mdl);
         }
      }
      if (GLRender->GLZBuf) {
         for (i=0;i<vp->DataItem.Nb;i++) {
            if ((obs=Obs_Get(vp->DataItem.Array[i]))) {
               Obs_Render(NULL,obs,vp,proj,GL_RENDER);
            }
#ifdef HAVE_ECBUFR
            TMetObs *met;
            if ((met=MetObs_Get(vp->DataItem.Array[i]))) {
               MetObs_Render(NULL,met,vp,proj,GL_RENDER);
            }
#endif
            if ((traj=Traj_Get(vp->DataItem.Array[i]))) {
               Traj_Render(NULL,traj,vp,proj,GL_RENDER);
            }
         }
      }
      ViewportUnset(vp);

      /*Sortie des donnees raster*/
      if (ras) {
         trPostscriptBuffer(Interp,GL_BACK,0,0,w,h,GLRender->TRCon);
      }
   } while (trEndTile(GLRender->TRCon));

   trDelete(GLRender->TRCon);
   GLRender->TRCon=NULL;

   /*Rendue des donnees vectorielle*/
   ViewportSetup(Canvas,vp,proj,0,0,0,1,1);
   Projection_Setup(vp,proj,1);

   if (!GLRender->GLZBuf) {
      for (i=0;i<vp->DataItem.Nb;i++) {
         if ((fld=Data_Get(vp->DataItem.Array[i]))) {
            Data_Render(Interp,fld,vp,proj,GL_RENDER,GL_VECTOR);
         }
         if ((obs=Obs_Get(vp->DataItem.Array[i]))) {
            Obs_Render(Interp,obs,vp,proj,GL_RENDER);
         }
#ifdef HAVE_ECBUFR
         TMetObs *met;
         if ((met=MetObs_Get(vp->DataItem.Array[i]))) {
            MetObs_Render(Interp,met,vp,proj,GL_RENDER);
         }
#endif
         if ((traj=Traj_Get(vp->DataItem.Array[i]))) {
            Traj_Render(Interp,traj,vp,proj,GL_RENDER);
         }
      }
   }

   /*Sortie des donnees vectorielles de la projection*/
   Projection_Render(Interp,vp,proj,GL_VECTOR);

   Tcl_AppendResult(Interp,"grestore\n",(char*)NULL);

   /*Creer le pourtour*/
   if (vp->FGColor && vp->BDWidth) {
      sprintf(buf,"%i setlinewidth 1 setlinecap 1 setlinejoin\n",vp->BDWidth);
      Tcl_AppendResult(Interp,buf,(char*)NULL);
      Tk_CanvasPsColor(Interp,Canvas,vp->FGColor);
      Tk_CanvasPsPath(Interp,Canvas,coords,4);
      Tcl_AppendResult(Interp,"closepath stroke\n",(char*)NULL);
   }

   /*Mask inslusions*/
   ViewportIntrusion(Interp,Canvas,Item);
   ViewportLicense(Interp,vp,proj);

   ViewportUnset(vp);
   glXFreePBuffer(pbuf);

   SetglCanvas(Canvas(Canvas));
   return(TCL_OK);
}

/*----------------------------------------------------------------------------
 * Nom      : <VP_CamParseProc>
 * Creation : Janvier  - J.P. Gauthier - CMC/CMOE
 *
 * But      : Cette procedure sert a extraire la valeur de la Camera.
 *
 * Parametres    :
 *  <Data>       : Non utilise
 *  <Interp>     : Interpreteur Tcl de l'application
 *  <TkWin>      : Identificateur de la fenetre
 *  <Value>      : Valeur de l'option
 *  <WidgRec>    : Pointeur sur l'enregistrement
 *  <Offset>     : Offset a l'interieur de l'item (Non utilise)
 *
 * Retour        :
 *  <TCL_...>    : Code de retour Tcl
 *
 * Remarques :
 *
 *----------------------------------------------------------------------------
*/
static int VP_CamParseProc(ClientData Data,Tcl_Interp *Interp,Tk_Window TkWin,const char *Value,char *WidgRec,int Offset){

   ViewportItem *vp=(ViewportItem *)WidgRec;

   if (vp->Cam) {
      free(vp->CamStr);
   }

   if (strlen(Value) > 0) {
      vp->CamStr=strdup(Value);
      vp->Cam=ProjCam_Get(Value);
   } else {
      vp->Cam=NULL;
      vp->CamStr=NULL;
   }

   return(TCL_OK);
}

/*----------------------------------------------------------------------------
 * Nom      : <VP_CamPrintProc>
 * Creation :
 *
 * But      : Cette procedure sert a extraire la valeur de la camera.
 *
 * Parametres    :
 *  <Data>       : Non utilise
 *  <TkWin>      : Identificateur de la fenetre
 *  <WidgRec>    : Pointeur sur l'enregistrement
 *  <Offset>     : Offset a l'interieur de l'item (Non utilise)
 *  <FreeProcPtr>: Pointeur sur la procedure de liberation
 *
 * Retour        :
 *  <TCL_...>    : Code de retour Tcl
 *
 * Remarques :
 *
 *----------------------------------------------------------------------------
*/
static const char *VP_CamPrintProc(ClientData Data,Tk_Window TkWin,char *WidgRec,int Offset,Tcl_FreeProc **FreeProcPtr){

   ViewportItem *vp=(ViewportItem *)WidgRec;

   return(vp->CamStr);
}

/*----------------------------------------------------------------------------
 * Nom      : <VP_ArrayParseProc>
 * Creation : Aout 1998 - J.P. Gauthier - CMC/CMOE
 *
 * But      : Cette procedure sert a extraire la valeur de la vue.
 *
 * Parametres    :
 *  <Data>       : Non utilise
 *  <Interp>     : Interpreteur Tcl de l'application
 *  <TkWin>      : Identificateur de la fenetre
 *  <Value>      : Valeur de l'option
 *  <WidgRec>    : Pointeur sur l'enregistrement
 *  <Offset>     : Offset a l'interieur de l'item (Non utilise)
 *
 * Retour        :
 *  <TCL_...>    : Code de retour Tcl
 *
 * Remarques :
 *
 *----------------------------------------------------------------------------
*/
static int VP_ArrayParseProc(ClientData Data,Tcl_Interp *Interp,Tk_Window TkWin,const char *Value,char *WidgRec,int Offset){

   Obj2Array *array=(Obj2Array*)(WidgRec+Offset);

   if (array->Array) {
      free(array->String);
      Tcl_Free((char*)array->Array);
      array->Array=NULL;
      array->String=NULL;
      array->Nb=0;
   }
   array->String=strdup(Value);
   Tcl_SplitList(Interp,Value,&array->Nb,(const char ***)&array->Array);

   return(TCL_OK);
}

/*----------------------------------------------------------------------------
 * Nom      : <VP_ArrayPrintProc>
 * Creation :
 *
 * But      : Cette procedure sert a extraire la valeur des donnees associees.
 *
 * Parametres    :
 *  <Data>       : Non utilise
 *  <TkWin>      : Identificateur de la fenetre
 *  <WidgRec>    : Pointeur sur l'enregistrement
 *  <Offset>     : Offset a l'interieur de l'item (Non utilise)
 *  <FreeProcPtr>: Pointeur sur la procedure de liberation
 *
 * Retour        :
 *  <TCL_...>    : Code de retour Tcl
 *
 * Remarques :
 *
 *----------------------------------------------------------------------------
*/
static const char *VP_ArrayPrintProc(ClientData Data,Tk_Window TkWin,char *WidgRec,int Offset,Tcl_FreeProc **FreeProcPtr){

   Obj2Array *array=(Obj2Array*)(WidgRec+Offset);

   return(array->String);
}

/*----------------------------------------------------------------------------
 * Nom      : <VP_ProjParseProc>
 * Creation : Aout 1998 - J.P. Gauthier - CMC/CMOE
 *
 * But      : Cette procedure sert a extraire la valeur de la vue.
 *
 * Parametres    :
 *  <Data>       : Non utilise
 *  <Interp>     : Interpreteur Tcl de l'application
 *  <TkWin>      : Identificateur de la fenetre
 *  <Value>      : Valeur de l'option
 *  <WidgRec>    : Pointeur sur l'enregistrement
 *  <Offset>     : Offset a l'interieur de l'item (Non utilise)
 *
 * Retour        :
 *  <TCL_...>    : Code de retour Tcl
 *
 * Remarques :
 *
 *----------------------------------------------------------------------------
*/
static int VP_ProjParseProc(ClientData Data,Tcl_Interp *Interp,Tk_Window TkWin,const char *Value,char *WidgRec,int Offset){

   ViewportItem *vp=(ViewportItem *)WidgRec;

   free(vp->Projection);
   vp->Projection=NULL;

   if (strlen(Value) > 0) {
      vp->Projection=strdup(Value);
   }

   if (!Projection_Get(vp->Projection)) {
      Tcl_AppendResult(Interp,"VP_ProjParseProc: Invalid Projection \"",vp->Projection,"\"",(char*)NULL);
   }

   return(TCL_OK);
}

/*----------------------------------------------------------------------------
 * Nom      : <VP_ProjPrintProc>
 * Creation :
 *
 * But      : Cette procedure sert a extraire la valeur de la vue.
 *
 * Parametres    :
 *  <Data>       : Non utilise
 *  <TkWin>      : Identificateur de la fenetre
 *  <WidgRec>    : Pointeur sur l'enregistrement
 *  <Offset>     : Offset a l'interieur de l'item (Non utilise)
 *  <FreeProcPtr>: Pointeur sur la procedure de liberation
 *
 * Retour        :
 *  <TCL_...>    : Code de retour Tcl
 *
 * Remarques :
 *
 *----------------------------------------------------------------------------
*/
static const char *VP_ProjPrintProc(ClientData Data,Tk_Window TkWin,char *WidgRec,int Offset,Tcl_FreeProc **FreeProcPtr){

   ViewportItem *vp=(ViewportItem *)WidgRec;

   return(vp->Projection);
}
