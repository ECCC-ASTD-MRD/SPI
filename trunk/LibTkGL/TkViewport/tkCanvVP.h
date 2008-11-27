/*==============================================================================
 * Environnement Canada
 * Centre Meteorologique Canadian
 * 2100 Trans-Canadienne
 * Dorval, Quebec
 *
 * Projet    : Projection orthographique de la carte vectorielle.
 * Fichier   : tkCanvVP.h
 * Creation  : Janvier 1999
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

#ifndef _tkCanvVP_h
#define _tkCanvVP_h

#include "tclUtils.h"

#include "tkglCanvas.h"
#include "tclCMap.h"
#include "ProjCam.h"

#include "tclFSTD.h"
#include "tclTraj.h"
#include "tclObs.h"
#include "tclMetObs.h"

#define NBFRAMEMAX 512
#define TKVIEWPORT_VERSION "7.2.4"
#define VPY(VP,Y) ((((TkCanvas*)VP->canvas)->yOrigin+VP->Height)-Y)

/*The structure below defines the record for each pixmap item.*/

typedef struct ViewportItem  {
   Tk_Item header;                /*Generic stuff that's the same for all types*/
   Tk_Canvas canvas;              /*Pointeur sur le canvas contenant le viewport*/
   Tk_Anchor anchor;              /*Where to anchor pixmap relative to (x,y)*/
   Tk_Font tkfont;                /*Font for drawing text*/
   Tcl_TimerToken Timer;          /*Tcl refresh timer event*/
   double x,y;                    /*Coordinates of positioning point for pixmap*/
   int Width,Height;              /*Dimensions du viewport*/
   int BDWidth;                   /*Largeur de la bordure*/
   XColor *ColorFCoast;           /*Couleur des cotes (polygone)*/
   XColor *ColorFLake;            /*Couleur des lacs (polygone)*/
   XColor *ColorCoast;            /*Couleur des cotes*/
   XColor *ColorLake;             /*Couleur des lacs*/
   XColor *ColorRiver;            /*Couleur des rivieres*/
   XColor *ColorPolit;            /*Couleur des delimitations politiques*/
   XColor *ColorAdmin;            /*Couleur des delimitations politiques internes*/
   XColor *ColorCity;             /*Couleur des villes*/
   XColor *ColorPlace;            /*Couleur des delimitations politiques*/
   XColor *ColorRoad;             /*Couleur des routes*/
   XColor *ColorRail;             /*Couleur des chemin de fer*/
   XColor *ColorUtil;             /*Couleur des donnees utilitaires*/
   XColor *ColorCanal;            /*Couleur des canaux/aqueducs*/
   XColor *ColorCoord;            /*Couleur des latlon*/
   XColor *BGColor;               /*Couleur d'arriere plan*/
   XColor *FGColor;               /*Couleur d'avant plan*/
   char *Command;                 /*Nom de la commande de transformation*/
   char *Projection;              /*Projection utilisee*/
   char *CamStr;                  /*Camera utilisee (String)*/
   ProjCam *Cam;                  /*Camera utilisee*/
   char **Data;                   /*Liste des donnees associees*/
   char *DataStr;                 /*Liste des donnees associees (String)*/
   int NbData;                    /*Nombre de donnees associees*/
   int Frame;                     /*Retention du rendue pour animation*/
   int Update;                    /*Indicateur de reaffichage*/
   int Realloc;                   /*Indicateur de reallocation des structures*/
   double Ratio;                  /*Ratio de hauteur/largeur/Cam aspect*/
   int Secondary;                 /*Indicateur d'importance*/
   int BackBuffer;                /*Indicateur de memorisation du rendue*/

   GLubyte *Frames[NBFRAMEMAX];   /*Liste des frames de retention*/
   GLdouble GLModS[16];           /*Matrice du modele statique*/
   GLdouble GLModR[16];           /*Matrice du modele avec rotations*/
   GLdouble GLPick[16];           /*Matrice du mode picking*/
   GLdouble GLProj[16];           /*Matrice de la projection*/
   GLint    GLView[4];            /*Matrice du viewport*/

   Tcl_ThreadId ThreadId;
   int Loading;                   /*Indicateur de lecture en arriere plan*/
} ViewportItem;

typedef struct ThreadEvent {
    Tcl_Event event;    /* Must be first */
    void *ptr;          /* Data pointer */
} ThreadEvent;

void ViewportClean(ViewportItem *VP,int Data,int Buff);
void ViewportRefresh(ClientData clientData,int Delay);
void ViewportRefresh_Canvas(ClientData clientData);
int  ViewportRefresh_ThreadEventProc(Tcl_Event *Event,int Mask);

static int    ViewportCommand(ClientData Data,Tcl_Interp *Interp,int Objc,Tcl_Obj *CONST Objv[]);
static int    ViewportCoords(Tcl_Interp *Interp,Tk_Canvas Canvas,Tk_Item *Item,int Argc,Tcl_Obj *CONST Argv[]);
static int    ViewportToArea(Tk_Canvas Canvas,Tk_Item *Item,double *RectPtr);
static double ViewportToPoint(Tk_Canvas Canvas,Tk_Item *Item,double *CoordPtr);
static int    ViewportToPostscript(Tcl_Interp *Interp,Tk_Canvas Canvas,Tk_Item *Item,int Prepass);
static void   ViewportBBox(Tk_Canvas Canvas,ViewportItem *VP);
static int    ViewportConfigure(Tcl_Interp *Interp,Tk_Canvas Canvas,Tk_Item *Item,int Argc,Tcl_Obj *CONST Argv[],int Flags);
static int    ViewportCreate(Tcl_Interp *Interp,Tk_Canvas Canvas,Tk_Item *Item,int Argc,Tcl_Obj *CONST Argv[]);
static void   ViewportDelete(Tk_Canvas Canvas,Tk_Item *Item,Display *Disp);
static void   ViewportDisplay(Tk_Canvas Canvas,Tk_Item *Item,Display *Disp,Drawable Drawt,int X,int Y,int Width,int Height);
static void   ViewportScale(Tk_Canvas Canvas,Tk_Item *Item, double OriginX,double OriginY,double ScaleX,double ScaleY);
static void   ViewportTranslate(Tk_Canvas Canvas,Tk_Item *Item,double DeltaX,double DeltaY);

int Tkviewport_Init(Tcl_Interp *Interp);

#endif
