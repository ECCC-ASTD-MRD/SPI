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
#include "tkCanvVP.h"
#include "Projection.h"

extern void  Grid_Setup();
extern int   Data_Render(Tcl_Interp *Interp,TData *Field,ViewportItem *VP,ClientData Proj,GLuint GLMode,int Mode);
extern int   Traj_Render(Tcl_Interp *Interp,TTraj *Traj,ViewportItem *VP,Projection *Proj,GLuint GLMode);
extern int   Obs_Render(Tcl_Interp *Interp,TObs *Obs,ViewportItem *VP,Projection *Proj,GLuint GLMode);
extern int   MetObs_Render(Tcl_Interp *Interp,TMetObs *Obs,ViewportItem *VP,Projection *Proj,GLuint GLMode);

static int   VP_CamParseProc  _ANSI_ARGS_((ClientData clientData,Tcl_Interp *interp,Tk_Window tkwin,char *value,char *widgRec,int offset));
static char *VP_CamPrintProc  _ANSI_ARGS_((ClientData clientData,Tk_Window tkwin,char *widgRec,int offset,Tcl_FreeProc **freeProcPtr));
static int   VP_ProjParseProc _ANSI_ARGS_((ClientData clientData,Tcl_Interp *interp,Tk_Window tkwin,char *value,char *widgRec,int offset));
static char *VP_ProjPrintProc _ANSI_ARGS_((ClientData clientData,Tk_Window tkwin,char *widgRec,int offset,Tcl_FreeProc **freeProcPtr));
static int   VP_DataParseProc _ANSI_ARGS_((ClientData clientData,Tcl_Interp *interp,Tk_Window tkwin,char *value,char *widgRec,int offset));
static char *VP_DataPrintProc _ANSI_ARGS_((ClientData clientData,Tk_Window tkwin,char *widgRec,int offset,Tcl_FreeProc **freeProcPtr));

static Tk_CustomOption CamOption   = { (Tk_OptionParseProc*)VP_CamParseProc,VP_CamPrintProc,(ClientData)NULL };
static Tk_CustomOption DataOption  = { (Tk_OptionParseProc*)VP_DataParseProc,VP_DataPrintProc,(ClientData)NULL };
static Tk_CustomOption ProjOption  = { (Tk_OptionParseProc*)VP_ProjParseProc,VP_ProjPrintProc,(ClientData)NULL };
static Tk_CustomOption tagsOption  = { Tk_CanvasTagsParseProc,Tk_CanvasTagsPrintProc,(ClientData)NULL };

void ViewportSetup(Tk_Canvas Canvas,ViewportItem *VP,Projection *Proj,int Width,int Height,int Tile,int Clear,int PS);
void ViewportSet(ViewportItem *VP,Projection *Proj);
void ViewportUnset(ViewportItem *VP);

#define VPMAX 256
static ViewportItem *ViewportTable[256];

/*Information used for parsing configuration specs:*/

static Tk_ConfigSpec configSpecs[] = {
   { TK_CONFIG_ANCHOR, "-anchor",(char *)NULL,(char *)NULL,
        "nw",Tk_Offset(ViewportItem,anchor),TK_CONFIG_DONT_SET_DEFAULT },
   { TK_CONFIG_FONT, "-font",(char*)NULL,(char*)NULL,
        "Helvetica 12",Tk_Offset(ViewportItem,tkfont),TK_CONFIG_NULL_OK },
   { TK_CONFIG_DOUBLE,"-x",(char *)NULL,(char *)NULL,
        "0",Tk_Offset(ViewportItem,x),TK_CONFIG_DONT_SET_DEFAULT },
   { TK_CONFIG_DOUBLE,"-y",(char *)NULL,(char *)NULL,
        "0",Tk_Offset(ViewportItem,y),TK_CONFIG_DONT_SET_DEFAULT },
   { TK_CONFIG_PIXELS,"-width",(char *)NULL,(char *)NULL,
        "0",Tk_Offset(ViewportItem,Width),TK_CONFIG_DONT_SET_DEFAULT },
   { TK_CONFIG_PIXELS,"-height",(char *)NULL,(char *)NULL,
        "0",Tk_Offset(ViewportItem,Height),TK_CONFIG_DONT_SET_DEFAULT },
   { TK_CONFIG_PIXELS,"-bd",(char *)NULL,(char *)NULL,
        "0",Tk_Offset(ViewportItem,BDWidth),TK_CONFIG_DONT_SET_DEFAULT },
   { TK_CONFIG_COLOR,"-colorfillcoast",(char *)NULL,(char *)NULL,
        NULL,Tk_Offset(ViewportItem,ColorFCoast),TK_CONFIG_NULL_OK },
   { TK_CONFIG_COLOR,"-colorfilllake",(char *)NULL,(char *)NULL,
         NULL,Tk_Offset(ViewportItem,ColorFLake),TK_CONFIG_NULL_OK },
   { TK_CONFIG_COLOR,"-colorcoast",(char *)NULL,(char *)NULL,
        "black",Tk_Offset(ViewportItem,ColorCoast),TK_CONFIG_NULL_OK },
   { TK_CONFIG_COLOR,"-colorlake",(char *)NULL,(char *)NULL,
        "black",Tk_Offset(ViewportItem,ColorLake),TK_CONFIG_NULL_OK },
   { TK_CONFIG_COLOR,"-colorriver",(char *)NULL,(char *)NULL,
        "black",Tk_Offset(ViewportItem,ColorRiver),TK_CONFIG_NULL_OK },
   { TK_CONFIG_COLOR,"-colorpolit",(char *)NULL,(char *)NULL,
        "black",Tk_Offset(ViewportItem,ColorPolit),TK_CONFIG_NULL_OK },
   { TK_CONFIG_COLOR,"-coloradmin",(char *)NULL,(char *)NULL,
        "black",Tk_Offset(ViewportItem,ColorAdmin),TK_CONFIG_NULL_OK },
   { TK_CONFIG_COLOR,"-colorcity",(char *)NULL,(char *)NULL,
        "black",Tk_Offset(ViewportItem,ColorCity),TK_CONFIG_NULL_OK },
   { TK_CONFIG_COLOR,"-colorroad",(char *)NULL,(char *)NULL,
        "black",Tk_Offset(ViewportItem,ColorRoad),TK_CONFIG_NULL_OK },
   { TK_CONFIG_COLOR,"-colorrail",(char *)NULL,(char *)NULL,
        "black",Tk_Offset(ViewportItem,ColorRail),TK_CONFIG_NULL_OK },
   { TK_CONFIG_COLOR,"-colorutil",(char *)NULL,(char *)NULL,
        "black",Tk_Offset(ViewportItem,ColorUtil),TK_CONFIG_NULL_OK },
   { TK_CONFIG_COLOR,"-colorcanal",(char *)NULL,(char *)NULL,
        "black",Tk_Offset(ViewportItem,ColorCanal),TK_CONFIG_NULL_OK },
   { TK_CONFIG_COLOR,"-colorcoord",(char *)NULL,(char *)NULL,
        "black",Tk_Offset(ViewportItem,ColorCoord),TK_CONFIG_NULL_OK },
   { TK_CONFIG_COLOR,"-fg","-foreground",(char *)NULL,
        "black",Tk_Offset(ViewportItem,FGColor),TK_CONFIG_NULL_OK },
   { TK_CONFIG_COLOR,"-bg","-background",(char *)NULL,
        "white",Tk_Offset(ViewportItem,BGColor),TK_CONFIG_NULL_OK },
   { TK_CONFIG_BOOLEAN,"-update",(char *)NULL,(char *)NULL,
        "1",Tk_Offset(ViewportItem,Update),TK_CONFIG_DONT_SET_DEFAULT },
   { TK_CONFIG_BOOLEAN,"-secondary",(char *)NULL,(char *)NULL,
        "0",Tk_Offset(ViewportItem,Secondary),TK_CONFIG_DONT_SET_DEFAULT },
   { TK_CONFIG_BOOLEAN,"-backbuffer",(char *)NULL,(char *)NULL,
        "1",Tk_Offset(ViewportItem,BackBuffer),TK_CONFIG_DONT_SET_DEFAULT },
   { TK_CONFIG_STRING,"-command",(char*)NULL,(char *)NULL,
        (char *)NULL,Tk_Offset(ViewportItem,Command),TK_CONFIG_DONT_SET_DEFAULT },
   { TK_CONFIG_CUSTOM,"-projection",(char *)NULL,(char *)NULL,
        (char *)NULL,0,TK_CONFIG_NULL_OK,&ProjOption },
   { TK_CONFIG_CUSTOM,"-data",(char *)NULL,(char *)NULL,
        (char *)NULL,0,TK_CONFIG_NULL_OK,&DataOption },
   { TK_CONFIG_CUSTOM,"-camera",(char *)NULL,(char *)NULL,
        (char *)NULL,0,TK_CONFIG_NULL_OK,&CamOption },
   { TK_CONFIG_INT,"-frame",(char *)NULL,(char *)NULL,
        "-1",Tk_Offset(ViewportItem,Frame),TK_CONFIG_DONT_SET_DEFAULT },
   { TK_CONFIG_CUSTOM,"-tags",(char *)NULL,(char *)NULL,
        (char *)NULL,0,TK_CONFIG_NULL_OK,&tagsOption },
   { TK_CONFIG_END,(char *)NULL,(char *)NULL,(char *)NULL,(char *)NULL,0,0 }
};

/*
 * The structures below defines the pixmap item type in terms of
 * procedures that can be invoked by generic item code.
 */

Tk_ItemType tkViewportType = {
   "viewport",          /* name */
   sizeof(ViewportItem),      /* itemSize */
   ViewportCreate,         /* createProc */
   configSpecs,                 /* configSpecs */
   ViewportConfigure,         /* configureProc */
   ViewportCoords,         /* coordProc */
   ViewportDelete,         /* deleteProc */
   ViewportDisplay,        /* displayProc */
   0,             /* alwaysRedraw */
   ViewportToPoint,        /* pointProc */
   ViewportToArea,         /* areaProc */
   ViewportToPostscript,      /* postscriptProc */
   ViewportScale,       /* scaleProc */
   ViewportTranslate,         /* translateProc */
   (Tk_ItemIndexProc *)NULL,     /* indexProc */
   (Tk_ItemCursorProc *)NULL,    /* icursorProc */
   (Tk_ItemSelectionProc *)NULL, /* selectionProc */
   (Tk_ItemInsertProc *)NULL,    /* insertProc */
   (Tk_ItemDCharsProc *)NULL,    /* dTextProc */
   (Tk_ItemType *)NULL             /* nextPtr */
};

/*----------------------------------------------------------------------------
 * Nom      : <TkViewport_Init>
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
int Tkviewport_Init(Tcl_Interp *Interp) {

   extern int ProjCam_Init();
   extern int Tcldata_Init();
   extern int Tkcolorbar_Init();
   extern int Tkgraph_Init();

   Tcl_ThreadId tid;

   Tk_glCreateItemType(&tkViewportType);

   /*Initialisation du package fichier standard*/
   if (Tcldata_Init(Interp)==TCL_ERROR)
      return(TCL_ERROR);

   /*Initialisation du package colorbar*/
   if (Tkcolorbar_Init(Interp)==TCL_ERROR)
      return(TCL_ERROR);

   /*Initialisation du package graph*/
   if (Tkgraph_Init(Interp)==TCL_ERROR)
      return(TCL_ERROR);

   if (Tcl_CreateThread(&tid,GDB_ThreadProc,NULL,TCL_THREAD_STACK_DEFAULT,TCL_THREAD_NOFLAGS)==TCL_ERROR) {
      fprintf(stderr,"(ERROR) Tkviewport_Init: Unable to initiate GDB thread\n");
   }

   memset(ViewportTable,0x0,VPMAX*sizeof(ViewportItem*));

   Tcl_PkgProvide(Interp,"TkViewport",TKVIEWPORT_VERSION);
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
static int ViewportCreate(Tcl_Interp *Interp,Tk_Canvas Canvas,Tk_Item *Item,int Argc,Tcl_Obj *CONST Argv[]){

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
   vp->Frame       = 0;
   vp->Data        = NULL;
   vp->DataStr     = NULL;
   vp->NbData      = 0;
   vp->Ratio       = 0;
   vp->Secondary   = 0;
   vp->BackBuffer  = 1;

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
static int ViewportCommand(ClientData Data,Tcl_Interp *Interp,int Objc,Tcl_Obj *CONST Objv[]){

   ViewportItem *vp=(ViewportItem *)Data;
   Projection   *proj;
   TTraj        *traj;
   TObs         *obs;
   TMetObs      *met;
   TData        *data;
   Tcl_Obj      *obj;

   int           nco,idx,i,bool,n,np,pick;
   Vect3d        pt0,pt1;
   Coord         loc0,loc1,co[1000];
   double        x,y,h,d;

   static CONST char *sopt[] = { "-ungrid","-grid","-unproject","-project","-projectline","-distxy","-distpix","-distll","-bearing","-circle","-pick",NULL };
   enum                opt { UNGRID,GRID,UNPROJECT,PROJECT,PROJECTLINE,DISTXY,DISTPIX,DISTLL,BEARING,CIRCLE,PICK };

   proj=Projection_Get(vp->Projection);

   if (!proj) {
      Tcl_AppendResult(Interp,"\n   ViewportCommand: Projection name unknown: \"",vp->Projection,"\"",(char *)NULL);
      return(TCL_ERROR);
   }

   /*Si la projection n'a pas encore initialise ses dimensions*/
   if (!proj->Params->VP) {
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
            if (proj->Params->Ref) {
               Tcl_GetDoubleFromObj(Interp,Objv[2],&pt0[0]);
               Tcl_GetDoubleFromObj(Interp,Objv[3],&pt0[1]);
               pt0[0]-=vp->x;
               pt0[1]-=vp->y;
               proj->Type->UnProject(vp,proj->Params,&loc0,pt0);
               if (proj->Params->Geographic) {
                  CLAMPLON(loc0.lon);
                  h=GDB_GetMap(proj->Geo,loc0.lat,loc0.lon);

                  proj->Params->Ref->UnProject(proj->Params->Ref,&pt0[0],&pt0[1],loc0.lat,loc0.lon,0,1);
               } else {
                  pt0[0]=loc0.lon;
                  pt0[1]=loc0.lat;
               }
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

            if (proj->Params->Ref) {
               if (proj->Params->Geographic) {
                  proj->Params->Ref->Project(proj->Params->Ref,pt0[0],pt0[1],&loc0.lat,&loc0.lon,0,1);
               } else {
                  loc0.lat=pt0[1];
                  loc0.lon=pt0[0];
               }
            } else {
               pt0[0]=pt0[1]=-1.0;
               h=0.0;
             }
            Tcl_SetObjResult(Interp,proj->Type->ProjectPoint(Interp,vp,proj,loc0,bool));
            break;

         case UNPROJECT:
            if(Objc!=4) {
               Tcl_WrongNumArgs(Interp,2,Objv,"x y");
               return(TCL_ERROR);
            }
            Tcl_GetDoubleFromObj(Interp,Objv[2],&pt0[0]);
            Tcl_GetDoubleFromObj(Interp,Objv[3],&pt0[1]);
            pt0[0]-=vp->x;
            pt0[1]-=vp->y;
            proj->Type->UnProject(vp,proj->Params,&loc0,pt0);
            loc0.elev=0.0;
            if (proj->Params->Geographic) {
               CLAMPLON(loc0.lon);
               loc0.elev=GDB_GetMap(proj->Geo,loc0.lat,loc0.lon);
            }
            obj=Tcl_NewListObj(0,NULL);
            Tcl_ListObjAppendElement(Interp,obj,Tcl_NewDoubleObj(loc0.lat));
            Tcl_ListObjAppendElement(Interp,obj,Tcl_NewDoubleObj(loc0.lon));
            Tcl_ListObjAppendElement(Interp,obj,Tcl_NewDoubleObj(loc0.elev));
            Tcl_SetObjResult(Interp,obj);
            break;

         case PROJECT:
            if (Objc!=5 && Objc!=6){
               Tcl_WrongNumArgs(Interp,2,Objv,"lat lon elev [Anywhere]");
               return(TCL_ERROR);
            }
            Tcl_GetDoubleFromObj(Interp,Objv[2],&loc0.lat);
            Tcl_GetDoubleFromObj(Interp,Objv[3],&loc0.lon);
            Tcl_GetDoubleFromObj(Interp,Objv[4],&loc0.elev);
            if (Objc==6) {
               Tcl_GetBooleanFromObj(Interp,Objv[5],&bool);
            } else {
               bool=0;
            }
            Tcl_SetObjResult(Interp,proj->Type->ProjectPoint(Interp,vp,proj,loc0,bool));
            break;

         case PROJECTLINE:
            if (Objc!=4){
               Tcl_WrongNumArgs(Interp,2,Objv,"NONE|COORD|TRUE coords }");
               return(TCL_ERROR);
            }
            nco=Projection_Map(Interp,co,Tcl_GetString(Objv[2])[0],Objv[3]);
            Tcl_SetObjResult(Interp,proj->Type->ProjectLine(Interp,vp,proj,co,nco));
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

            if (proj->Type->UnProject(vp,proj->Params,&loc0,pt0) && proj->Type->UnProject(vp,proj->Params,&loc1,pt1)) {
               loc0.lat=DEG2RAD(loc0.lat);loc0.lon=DEG2RAD(loc0.lon);
               loc1.lat=DEG2RAD(loc1.lat);loc1.lon=DEG2RAD(loc1.lon);
               Tcl_SetObjResult(Interp,Tcl_NewDoubleObj(DIST(0.0,loc0.lat,loc0.lon,loc1.lat,loc1.lon)));
            } else {
               Tcl_SetObjResult(Interp,Tcl_NewDoubleObj(0.0));
            }
            break;

         case DISTLL:
            if (Objc!=7){
               Tcl_WrongNumArgs(Interp,2,Objv,"lat0 lon0 lat1 lon1 elev");
               return(TCL_ERROR);
            }
            Tcl_GetDoubleFromObj(Interp,Objv[2],&loc0.lat);
            Tcl_GetDoubleFromObj(Interp,Objv[3],&loc0.lon);
            Tcl_GetDoubleFromObj(Interp,Objv[4],&loc1.lat);
            Tcl_GetDoubleFromObj(Interp,Objv[5],&loc1.lon);
            Tcl_GetDoubleFromObj(Interp,Objv[6],&x);

            loc0.lat=DEG2RAD(loc0.lat);loc0.lon=DEG2RAD(loc0.lon);
            loc1.lat=DEG2RAD(loc1.lat);loc1.lon=DEG2RAD(loc1.lon);
            Tcl_SetObjResult(Interp,Tcl_NewDoubleObj(DIST(x,loc0.lat,loc0.lon,loc1.lat,loc1.lon)));
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
               Tcl_GetDoubleFromObj(Interp,Objv[2],&loc0.lat);
               Tcl_GetDoubleFromObj(Interp,Objv[3],&loc0.lon);
               Tcl_GetDoubleFromObj(Interp,Objv[4],&loc1.lat);
               Tcl_GetDoubleFromObj(Interp,Objv[5],&loc1.lon);

               loc0.lat=DEG2RAD(loc0.lat);loc0.lon=DEG2RAD(loc0.lon);
               loc1.lat=DEG2RAD(loc1.lat);loc1.lon=DEG2RAD(loc1.lon);

               x=COURSE(loc0.lat,loc0.lon,loc1.lat,loc1.lon);
               Tcl_SetObjResult(Interp,Tcl_NewDoubleObj(RAD2DEG(x)));
            }
            break;

         case CIRCLE:
            if (Objc!=6){
               Tcl_WrongNumArgs(Interp,0,Objv,"lat lon dist angle");
               return(TCL_ERROR);
            }
            Tcl_GetDoubleFromObj(Interp,Objv[2],&loc0.lat);
            Tcl_GetDoubleFromObj(Interp,Objv[3],&loc0.lon);
            Tcl_GetDoubleFromObj(Interp,Objv[4],&d);
            Tcl_GetDoubleFromObj(Interp,Objv[5],&x);

            loc0.lat=DEG2RAD(loc0.lat);
            loc0.lon=DEG2RAD(loc0.lon);
            x=DEG2RAD(x);
            d=M2RAD(d);

            loc1.lat=asin(sin(loc0.lat)*cos(d)+cos(loc0.lat)*sin(d)*cos(x));
            loc1.lon=fmod(loc0.lon+(atan2(sin(x)*sin(d)*cos(loc0.lat),cos(d)-sin(loc0.lat)*sin(loc1.lat)))+M_PI,M_2PI)-M_PI;
            loc1.lat=RAD2DEG(loc1.lat);loc1.lon=RAD2DEG(loc1.lon);

            obj=Tcl_NewListObj(0,NULL);
            Tcl_ListObjAppendElement(Interp,obj,Tcl_NewDoubleObj(loc1.lat));
            Tcl_ListObjAppendElement(Interp,obj,Tcl_NewDoubleObj(loc1.lon));
            Tcl_SetObjResult(Interp,obj);
            break;

         case PICK:
            if (Objc!=4 && Objc!=5){
               Tcl_WrongNumArgs(Interp,2,Objv,"x y [type]");
               return(TCL_ERROR);
            }
            Tcl_GetDoubleFromObj(Interp,Objv[2],&x);
            Tcl_GetDoubleFromObj(Interp,Objv[3],&y);

            if (Objc==5) {
               pick=PICK_NONE;
               Tcl_ListObjLength(Interp,Objv[4],&np);
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
            } else {
               pick=PICK_ALL;
            }

            trViewport(GLRender->TRCon,(int)vp->x,Tk_Height(Tk_CanvasTkwin(vp->canvas))-(vp->y+vp->Height),vp->Width,vp->Height);
            glPickInit(x,Tk_Height(Tk_CanvasTkwin(vp->canvas))-y,2.0,2.0);
            glGetDoublev(GL_PROJECTION_MATRIX,vp->GLPick);
            ViewportSetup(vp->canvas,vp,proj,Tk_Width(Tk_CanvasTkwin(vp->canvas)),Tk_Height(Tk_CanvasTkwin(vp->canvas)),0,0,0);
            Projection_Setup(vp,proj,0);

            /*Rendue des donnees vectorielle*/
            for (i=0;i<vp->NbData;i++) {
               glPushName(i);
               if ((pick&PICK_FSTDFIELD) && (data=Data_Get(vp->Data[i]))) {
                  Data_Render(NULL,data,vp,proj,GL_SELECT,GL_VECTOR);
               }
               if ((pick&PICK_OBS) && (obs=Obs_Get(vp->Data[i]))) {
                  Obs_Render(NULL,obs,vp,proj,GL_SELECT);
               }
               if ((pick&PICK_METOBS) && (met=MetObs_Get(vp->Data[i]))) {
                  MetObs_Render(NULL,met,vp,proj,GL_SELECT);
               }
               if ((pick&PICK_TRAJ) && (traj=Traj_Get(vp->Data[i]))) {
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

               Tcl_ListObjAppendElement(Interp,obj,Tcl_NewStringObj(vp->Data[GLRender->GLPick[0]],-1));

               switch(GLRender->GLPick[1]) {
                  case PICK_METOBS:    Tcl_ListObjAppendElement(Interp,obj,Tcl_NewStringObj(MetObs_GetTag(MetObs_Get(vp->Data[GLRender->GLPick[0]]),GLRender->GLPick[2]),-1)); break;
                  case PICK_TRAJ:
                  case PICK_OBS:
                  case PICK_FSTDFIELD: Tcl_ListObjAppendElement(Interp,obj,Tcl_NewIntObj(GLRender->GLPick[2])); break;
               }

               Tcl_ListObjAppendElement(Interp,obj,Tcl_NewIntObj(GLRender->GLPick[3]));
               Tcl_SetObjResult(Interp,obj);
            }
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
static int ViewportCoords(Tcl_Interp *Interp,Tk_Canvas Canvas,Tk_Item *Item,int Argc,Tcl_Obj *CONST Argv[]){

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
      sprintf(Interp->result,"ViewportCoords: wrong # coordinates,  expected 0 or 2, got %d",Argc);
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
static int ViewportConfigure(Tcl_Interp *Interp,Tk_Canvas Canvas,Tk_Item *Item,int Argc,Tcl_Obj *CONST Argv[],int Flags){

   ViewportItem *vp =(ViewportItem *)Item;
   Projection   *proj=NULL;
   int           width,height,frame;

   width  = vp->Width;
   height = vp->Height;
   frame  = vp->Frame;

   if (Tk_ConfigureWidget(Interp,Tk_CanvasTkwin(Canvas),configSpecs,Argc,(CONST84 char**)Argv,(char*)vp,Flags) != TCL_OK) {
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

   if (vp->Realloc || !vp->BackBuffer || (vp->Frame==0 && frame!=0)) {
      ViewportClean(vp,0,1);
   }

   vp->Update=1;

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

   proj=Projection_Get(vp->Projection);
   if (proj) {
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
   Tk_FreeColor(vp->ColorLake);
   Tk_FreeColor(vp->ColorRiver);
   Tk_FreeColor(vp->ColorPolit);
   Tk_FreeColor(vp->ColorAdmin);
   Tk_FreeColor(vp->ColorCity);
   Tk_FreeColor(vp->ColorRoad);
   Tk_FreeColor(vp->ColorRail);
   Tk_FreeColor(vp->ColorUtil);
   Tk_FreeColor(vp->ColorCanal);
   Tk_FreeColor(vp->ColorCoord);
   if (vp->ColorFCoast) Tk_FreeColor(vp->ColorFCoast);
   if (vp->ColorFLake)  Tk_FreeColor(vp->ColorFLake);

   if (vp->Command) {
      Tcl_DeleteCommand(((TkglCanvas*)Canvas)->interp,vp->Command);
      Tcl_Free((char*)vp->Command);
   }

   if (vp->Data) {
      free(vp->DataStr);
      Tcl_Free((char*)vp->Data);
   }

   /*Cleanup the projection in case it used a viewport field*/
   if (vp->Projection) {
      proj=Projection_Get(vp->Projection);
      if (proj) {
         if (proj->Params->VP==vp) {
            proj->Params->VP=NULL;
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
         for (i=0;i<VP->NbData;i++) {
            fld=Data_Get(VP->Data[i]);
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

void ViewportRefresh_Canvas(ClientData clientData) {
   extern void Tk_glCanvasEventuallyRedraw(Tk_Canvas canvas,int x1,int y1,int x2,int y2);

   Tk_glCanvasEventuallyRedraw((Tk_Canvas)clientData,1,1,2,2);
}

int ViewportRefresh_ThreadEventProc(Tcl_Event *Event,int Mask) {

   ThreadEvent *ev=(ThreadEvent*)Event;

   ViewportRefresh((ViewportItem*)ev->ptr,0);
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
   char          alloc=0;
   int           i,load;
   clock_t       sec;

   TData        *fld;
   TTraj        *traj;
   TObs         *obs;
   TMetObs      *met;

   extern int MetObs_Render(Tcl_Interp *Interp,TMetObs *Obs,ViewportItem *VP,Projection *Proj,GLuint GLMode);
   extern int Obs_Render(Tcl_Interp *Interp,TObs *Obs,ViewportItem *VP,Projection *Proj,GLuint GLMode);
   extern int Traj_Render(Tcl_Interp *Interp,TTraj *Traj,ViewportItem *VP,Projection *Proj,GLuint GLMode);
   extern int Data_Render(Tcl_Interp *Interp,TData *Field,ViewportItem *VP,ClientData Proj,GLuint GLMode,int Mode);

   load=vp->Loading;
   /*Take care of automated refresh handler*/
   Tcl_DeleteTimerHandler(vp->Timer);vp->Timer=NULL;
   if (GLRender->Delay<2000)
      ViewportRefresh(vp,GLRender->Delay);

   if (proj=Projection_Get(vp->Projection)) {
      load+=proj->Loading;

      /* Si rendue dans un pixmap, forcer le rendue*/
      if (GLRender->TRCon) {
         /*Wait for everything to be loaded*/
         sec=clock();
         while (((clock()-sec)<(300*CLOCKS_PER_SEC)) && (!GDB_ThreadQueueIsEmpty(0x0) || (vp->Loading+proj->Loading)));
         if ((clock()-sec)>=(300*CLOCKS_PER_SEC)) {
            fprintf(stderr,"(WARNING) ViewportDisplay: Warning, waited too long for data, rendering anyway\n");
         }
         vp->Update=1;
      }

      if (GLRender->XExpose>0) {
         vp->Update=1;
      }

      if (vp->Update || !vp->BackBuffer) {

         ViewportSet(vp,proj);
         ViewportSetup(Canvas,vp,proj,Width,Height,0,1,0);
         Projection_Setup(vp,proj,1);

         if (!GLRender->XBatch || vp->Update || GLRender->TRCon) {
            /*Allouer les frames de retentions si ce n'est pas deja fait*/
            if (!vp->Frames[vp->Frame] && vp->BackBuffer) {
               vp->Frames[vp->Frame]=(GLubyte*)malloc(vp->Width*vp->Height*4);
               alloc=1;
            }

            /*Effectuer le rendue des champs*/
            if (vp->Frame==0 || alloc) {
               Projection_Render(NULL,vp,proj,GL_ALL);
               ProjCam_Project(vp->Cam,proj);

               /*Rendue des donnees raster*/
               for (i=0;i<vp->NbData;i++) {
                  fld=Data_Get(vp->Data[i]);
                  if (fld) {
                     Data_Render(NULL,fld,vp,proj,GL_RENDER,GL_RASTER);
                  }
               }

               /*Rendue des donnees vectoriellle*/
               for (i=0;i<vp->NbData;i++) {
                  if ((fld=Data_Get(vp->Data[i]))) {
                     Data_Render(NULL,fld,vp,proj,GL_RENDER,GL_VECTOR);
                  }
                  if ((obs=Obs_Get(vp->Data[i]))) {
                     Obs_Render(NULL,obs,vp,proj,GL_RENDER);
                  }
                  if ((met=MetObs_Get(vp->Data[i]))) {
                     MetObs_Render(NULL,met,vp,proj,GL_RENDER);
                  }
                  if ((traj=Traj_Get(vp->Data[i]))) {
                     Traj_Render(NULL,traj,vp,proj,GL_RENDER);
                  }
               }

               if (!GLRender->TRCon && vp->Frames[vp->Frame]) {
                  glReadBuffer(GL_BACK);
                  glReadPixels(vp->header.x1-((TkCanvas*)Canvas)->xOrigin,Height-vp->header.y2+((TkCanvas*)Canvas)->yOrigin,vp->Width,vp->Height,GL_RGBA,GL_UNSIGNED_BYTE,vp->Frames[vp->Frame]);
               }
            }
         }
         ViewportUnset(vp);
      }

      /*Recopier le frame seulement si il n'as pas deja ete generer plut tot*/
      if ((!vp->Update || !alloc) && !GLRender->XBatch && !GLRender->TRCon) {
         if (vp->Frames[vp->Frame]) {
            trRasterPos2i(vp->header.x1-((TkCanvas *)Canvas)->xOrigin,-(vp->header.y2-((TkCanvas *)Canvas)->yOrigin));
            glDrawPixels(vp->Width,vp->Height,GL_RGBA,GL_UNSIGNED_BYTE,vp->Frames[vp->Frame]);
         }
      }
   }

   /*Pourtour*/
   if (vp->FGColor && vp->BDWidth) {
      glLineWidth(vp->BDWidth);
      glPolygonMode(GL_FRONT,GL_LINE);
      glColor3us(vp->FGColor->red,vp->FGColor->green,vp->FGColor->blue);
      glBegin(GL_QUADS);
         glVertex2i(vp->header.x1-((TkCanvas*)Canvas)->xOrigin,vp->header.y1-((TkCanvas*)Canvas)->yOrigin);
         glVertex2i(vp->header.x1-((TkCanvas*)Canvas)->xOrigin,vp->header.y2-((TkCanvas*)Canvas)->yOrigin);
         glVertex2i(vp->header.x2-((TkCanvas*)Canvas)->xOrigin,vp->header.y2-((TkCanvas*)Canvas)->yOrigin);
         glVertex2i(vp->header.x2-((TkCanvas*)Canvas)->xOrigin,vp->header.y1-((TkCanvas*)Canvas)->yOrigin);
      glEnd();
   }

   /*Loading data*/
   if (load) {
      glMatrixMode(GL_PROJECTION);
      glPushMatrix();
      glEnable(GL_BLEND);
      glPolygonMode(GL_FRONT_AND_BACK,GL_FILL);
      glTranslated(16+vp->header.x1+(vp->header.x2-vp->header.x1)*0.5-((TkCanvas*)Canvas)->xOrigin,16+vp->header.y1+(vp->header.y2-vp->header.y1)*0.5-((TkCanvas*)Canvas)->yOrigin,0.0);
      glScalef(16,16,1.0);
      glColor4us(0x00,0x00,0x00,16384);
      glDrawCircle(64,GL_POLYGON);
      glColor4us(0xFFFF,0xFFFF,0x00,32768);
      glDrawArc(0,(load%360),10,GL_POLYGON,0);
      glDisable(GL_BLEND);
      glPopMatrix();
   }

   vp->Update =0;
   vp->Realloc=0;
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
void ViewportSetup(Tk_Canvas Canvas,ViewportItem *VP,Projection *Proj,int Width,int Height,int Tile,int Clear,int PS){

   double as,z,dl;

   if (!VP || !Proj)
      return;

   glDepthFunc(GL_LESS);
   glDepthMask(GL_TRUE);
   if (VP->BGColor)
      glClearColor(VP->BGColor->red/65535.0f,VP->BGColor->green/65535.0f,VP->BGColor->blue/65535.0f,1.0f);
   glEnable(GL_CULL_FACE);

   glMatrixMode(GL_PROJECTION);

   if (Clear) {
      glLoadIdentity();
   }

   if (PS) {
      glViewport(0,0,VP->Width,VP->Height);
   } else {
      trViewport(GLRender->TRCon,VP->header.x1-((TkCanvas*)Canvas)->xOrigin,Height-(VP->header.y1-((TkCanvas*)Canvas)->yOrigin+VP->Height),VP->Width,VP->Height);
   }

   if (Proj->Type->Def==PROJGLOBE) {
      z=VP->Cam->Clip;
   } else {
      z=4.0*VP->Cam->Clip;
   }

   /*Ajuster la projection pour garder un aspect correct*/
   if ((VP->Width/Proj->Params->LI)>(VP->Height/Proj->Params->LJ)) {
      as=(double)VP->Width/VP->Height;
      dl=Proj->Params->LJ;
      if (Tile) {
         trOrtho(GLRender->TRCon,-VP->Cam->Aspect*as*dl,VP->Cam->Aspect*as*dl,-VP->Cam->Aspect*dl,VP->Cam->Aspect*dl,-VP->Cam->Aspect,z);
      } else {
         glOrtho(-VP->Cam->Aspect*as*dl,VP->Cam->Aspect*as*dl,-VP->Cam->Aspect*dl,VP->Cam->Aspect*dl,-VP->Cam->Aspect,z);
//         glFrustum(-VP->Cam->Aspect*as*dl,VP->Cam->Aspect*as*dl,-VP->Cam->Aspect*dl,VP->Cam->Aspect*dl,1.0,20.0);
      }
   } else {
      as=(double)VP->Height/VP->Width;
      dl=Proj->Params->LI;
      if (Tile) {
         trOrtho(GLRender->TRCon,-VP->Cam->Aspect*dl,VP->Cam->Aspect*dl,-VP->Cam->Aspect*as*dl,VP->Cam->Aspect*as*dl,-VP->Cam->Aspect,z);
      } else {
         glOrtho(-VP->Cam->Aspect*dl,VP->Cam->Aspect*dl,-VP->Cam->Aspect*as*dl,VP->Cam->Aspect*as*dl,-VP->Cam->Aspect,z);
//         glFrustum(-VP->Cam->Aspect*dl,VP->Cam->Aspect*dl,-VP->Cam->Aspect*as*dl,VP->Cam->Aspect*as*dl,-VP->Cam->Aspect,-z+1);
      }
   }

   /*Effectuer les manipulations aux niveau du modele*/
   glMatrixMode(GL_MODELVIEW);
   glLoadIdentity();

   /*Positionner la camera*/
   ProjCam_Place(VP->Cam);

   /*Conserver les parametres pour les transformations*/
   if (Clear) {
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
      glVertex3i(VP->header.x1,VP->header.y1,-1);
      glVertex3i(VP->header.x1,VP->header.y2,-1);
      glVertex3i(VP->header.x2,VP->header.y2,-1);
      glVertex3i(VP->header.x2,VP->header.y1,-1);
   } else {
      glVertex3i(0,0,-1);
      glVertex3i(0,VP->Height,-1);
      glVertex3i(VP->Width,VP->Height,-1);
      glVertex3i(VP->Width,0,-1);
   }
   glEnd();
   glEnable(GL_CULL_FACE);
}

void ViewportSet(ViewportItem *VP,Projection *Proj) {

   glPushAttrib(GL_ALL_ATTRIB_BITS);
   glMatrixMode(GL_PROJECTION);
   glPushMatrix();
   glMatrixMode(GL_MODELVIEW);
   glPushMatrix();

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
   double       coords[8];
   int          i,ras=0,w,h;
   char         buf[100];
   clock_t      sec;

   TData   *fld;
   TTraj   *traj;
   TObs    *obs;
   TMetObs *met;

   /*Definir le font du viewport*/
   if (Tk_CanvasPsFont(Interp,Canvas,vp->tkfont) != TCL_OK) {
      return(TCL_ERROR);
   }

   if (Prepass) {
      return(TCL_OK);
   }

   /*Coordonnee du viewport*/
   coords[0]=vp->header.x1 ; coords[1]=vp->header.y1;
   coords[2]=vp->header.x2 ; coords[3]=vp->header.y1;
   coords[4]=vp->header.x2 ; coords[5]=vp->header.y2;
   coords[6]=vp->header.x1 ; coords[7]=vp->header.y2;

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

   proj=Projection_Get(vp->Projection);

   w=vp->Width;
   h=vp->Height;
   w=512;
   h=512;
   if (!glXGetPBuffer(Tk_CanvasTkwin(Canvas),&w,&h)) {
      Tcl_AppendResult(Interp,"ViewportToPostscript: Unable to allocate rendering PBuffer",(char*)NULL);
      return(TCL_ERROR);
   }

   /*Wait for everything to be loaded*/
   sec=clock();
   while (((clock()-sec)<(300*CLOCKS_PER_SEC)) && (!GDB_ThreadQueueIsEmpty(0x0) || (vp->Loading+proj->Loading)));
   if ((clock()-sec)>=(300*CLOCKS_PER_SEC)) {
      fprintf(stderr,"(WARNING) ViewportToPostscript: Warning, waited too long for data, rendering anyway\n");
   }

   /* Setup the tile rendering engine */
   GLRender->TRCon=trNew();
   trTileSize(GLRender->TRCon,w,h,0);
   trImageSize(GLRender->TRCon,vp->Width,vp->Height);

   ViewportSet(vp,proj);

   /* Render the polygonized geographical data */
   ViewportSetup(Canvas,vp,proj,0,0,0,1,1);
   Projection_Setup(vp,proj,1);
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
      glClear(GL_COLOR_BUFFER_BIT|GL_DEPTH_BUFFER_BIT|GL_STENCIL_BUFFER_BIT);

      /*Generation des donnees raster*/
      ras+=Projection_Render(Interp,vp,proj,GL_RASTER);
      for (i=0;i<vp->NbData;i++) {
         fld=Data_Get(vp->Data[i]);
         if (fld) {
            ras+=Data_Render(Interp,fld,vp,proj,GL_RENDER,GL_RASTER);
         }
      }

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

   for (i=0;i<vp->NbData;i++) {
      if ((fld=Data_Get(vp->Data[i]))) {
         Data_Render(Interp,fld,vp,proj,GL_RENDER,GL_VECTOR);
      }
      if ((obs=Obs_Get(vp->Data[i]))) {
         Obs_Render(Interp,obs,vp,proj,GL_RENDER);
      }
      if ((met=MetObs_Get(vp->Data[i]))) {
         MetObs_Render(Interp,met,vp,proj,GL_RENDER);
      }
      if ((traj=Traj_Get(vp->Data[i]))) {
         Traj_Render(Interp,traj,vp,proj,GL_RENDER);
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

   ViewportUnset(vp);
   glXFreePBuffer();

   SetglCanvas(Canvas);
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
static int VP_CamParseProc(ClientData Data,Tcl_Interp *Interp,Tk_Window TkWin,char *Value,char *WidgRec,int Offset){

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
static char *VP_CamPrintProc(ClientData Data,Tk_Window TkWin,char *WidgRec,int Offset,Tcl_FreeProc **FreeProcPtr){

   ViewportItem *vp=(ViewportItem *)WidgRec;

   return(vp->CamStr);
}

/*----------------------------------------------------------------------------
 * Nom      : <VP_DataParseProc>
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
static int VP_DataParseProc(ClientData Data,Tcl_Interp *Interp,Tk_Window TkWin,char *Value,char *WidgRec,int Offset){

   ViewportItem *vp  =(ViewportItem*)WidgRec;
   Projection   *proj=NULL;

   if (vp->Data) {
      free(vp->DataStr);
      Tcl_Free((char*)vp->Data);
      vp->Data=NULL;
      vp->NbData=0;
   }
   vp->DataStr=strdup(Value);
   Tcl_SplitList(Interp,Value,&vp->NbData,&vp->Data);

   return(TCL_OK);
}

/*----------------------------------------------------------------------------
 * Nom      : <VP_DataPrintProc>
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
static char *VP_DataPrintProc(ClientData Data,Tk_Window TkWin,char *WidgRec,int Offset,Tcl_FreeProc **FreeProcPtr){

   ViewportItem *vp=(ViewportItem *)WidgRec;

   return(vp->DataStr);
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
static int VP_ProjParseProc(ClientData Data,Tcl_Interp *Interp,Tk_Window TkWin,char *Value,char *WidgRec,int Offset){

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
static char *VP_ProjPrintProc(ClientData Data,Tk_Window TkWin,char *WidgRec,int Offset,Tcl_FreeProc **FreeProcPtr){

   ViewportItem *vp=(ViewportItem *)WidgRec;

   return(vp->Projection);
}
