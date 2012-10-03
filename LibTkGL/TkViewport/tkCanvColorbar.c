/*==============================================================================
 * Environnement Canada
 * Centre Meteorologique Canadian
 * 2100 Trans-Canadienne
 * Dorval, Quebec
 *
 * Projet    : Projection orthographique de la carte vectorielle.
 * Fichier   : tkCanvColormap.c
 * Creation  : Fevrier 2002
 *
 * Description: Affichage et manipulation de palette.
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

#include "tkCanvColorbar.h"

#include "tclFSTD.h"
#include "tclObs.h"
#include "tclOGR.h"
#include "tclGDAL.h"

extern void   Data_RenderBarbule(int Type,int Flip,float Axis,float Lat,float Lon,float Elev,float Speed,float Dir,float Size,void *Proj);
extern TIcon IconList[];

extern TData*     Data_Get(char *Name);
extern TDataSpec* DataSpec_Get(char *Name);
extern TObs*      Obs_Get(char *Name);
extern OGR_Layer* OGR_LayerGet(char *Name);

static int   Colorbar_DataParseProc _ANSI_ARGS_((ClientData clientData,Tcl_Interp *interp,Tk_Window tkwin,char *value,char *widgRec,int offset));
static char *Colorbar_DataPrintProc _ANSI_ARGS_((ClientData clientData,Tk_Window tkwin,char *widgRec,int offset,Tcl_FreeProc **freeProcPtr));

static Tk_CustomOption DataOption = { (Tk_OptionParseProc*)Colorbar_DataParseProc,Colorbar_DataPrintProc,(ClientData)NULL };
static Tk_CustomOption tagsOption  = { Tk_CanvasTagsParseProc,Tk_CanvasTagsPrintProc,(ClientData)NULL };

/*Information used for parsing configuration specs:*/

static Tk_ConfigSpec ColorbarSpecs[] = {
   { TK_CONFIG_ANCHOR, "-anchor",(char *)NULL,(char *)NULL,
        "center",Tk_Offset(ColorbarItem,anchor),TK_CONFIG_DONT_SET_DEFAULT },
   { TK_CONFIG_FONT, "-font",(char*)NULL,(char*)NULL,
        "Helvetica -12",Tk_Offset(ColorbarItem,Font),0 },
   { TK_CONFIG_DOUBLE,"-x",(char *)NULL,(char *)NULL,
        "0",Tk_Offset(ColorbarItem,x),TK_CONFIG_DONT_SET_DEFAULT },
   { TK_CONFIG_DOUBLE,"-y",(char *)NULL,(char *)NULL,
        "0",Tk_Offset(ColorbarItem,y),TK_CONFIG_DONT_SET_DEFAULT },
   { TK_CONFIG_PIXELS,"-width",(char *)NULL,(char *)NULL,
        "0",Tk_Offset(ColorbarItem,Width),TK_CONFIG_DONT_SET_DEFAULT },
   { TK_CONFIG_PIXELS,"-height",(char *)NULL,(char *)NULL,
        "0",Tk_Offset(ColorbarItem,Height),TK_CONFIG_DONT_SET_DEFAULT },
   { TK_CONFIG_COLOR,"-bg","-background",(char *)NULL,
        "white",Tk_Offset(ColorbarItem,BGColor),TK_CONFIG_NULL_OK },
   { TK_CONFIG_COLOR,"-fg","-foreground",(char *)NULL,
        "black",Tk_Offset(ColorbarItem,FGColor),TK_CONFIG_NULL_OK },
   { TK_CONFIG_CUSTOM,"-data",(char *)NULL,(char *)NULL,
        (char *)NULL,0,TK_CONFIG_NULL_OK,&DataOption },
   { TK_CONFIG_CUSTOM,"-tags",(char *)NULL,(char *)NULL,
        (char *)NULL,0,TK_CONFIG_NULL_OK,&tagsOption },
   { TK_CONFIG_INT, "-transparency", (char *) NULL, (char *) NULL,
        "100", Tk_Offset(ColorbarItem,Alpha), TK_CONFIG_DONT_SET_DEFAULT},
   { TK_CONFIG_BOOLEAN, "-id", (char *) NULL, (char *) NULL,
        "1", Tk_Offset(ColorbarItem,Id), TK_CONFIG_DONT_SET_DEFAULT},
   { TK_CONFIG_BOOLEAN, "-icon", (char *) NULL, (char *) NULL,
        "1", Tk_Offset(ColorbarItem,Icon), TK_CONFIG_DONT_SET_DEFAULT},
   { TK_CONFIG_INT, "-barsplit", (char *) NULL, (char *) NULL,
        "0", Tk_Offset(ColorbarItem,BarSplit), TK_CONFIG_DONT_SET_DEFAULT},
   { TK_CONFIG_INT, "-barwidth", (char *) NULL, (char *) NULL,
        "0", Tk_Offset(ColorbarItem,BarWidth), TK_CONFIG_DONT_SET_DEFAULT},
   { TK_CONFIG_INT, "-barborder", (char *) NULL, (char *) NULL,
        "0", Tk_Offset(ColorbarItem,BarBorder), TK_CONFIG_DONT_SET_DEFAULT},
   { TK_CONFIG_JUSTIFY, "-barside",(char*)NULL,(char*) NULL,
        "right", Tk_Offset(ColorbarItem,BarSide),TK_CONFIG_DONT_SET_DEFAULT },
   { TK_CONFIG_BOOLEAN, "-showfactor",(char*)NULL,(char*) NULL,
        "1", Tk_Offset(ColorbarItem,ShowFactor),TK_CONFIG_DONT_SET_DEFAULT },
   { TK_CONFIG_END,(char *)NULL,(char *)NULL,(char *)NULL,(char *)NULL,0,0 }
};

/*
 * The structures below defines the pixmap item type in terms of
 * procedures that can be invoked by generic item code.
 */

Tk_ItemType tkColorbarType = {
   "colorbar",          /* name */
   sizeof(ColorbarItem),      /* itemSize */
   ColorbarCreate,         /* createProc */
   ColorbarSpecs,            /* configSpecs */
   ColorbarConfigure,         /* configureProc */
   ColorbarCoords,         /* coordProc */
   ColorbarDelete,         /* deleteProc */
   ColorbarDisplay,        /* displayProc */
   0,             /* alwaysRedraw */
   ColorbarToPoint,        /* pointProc */
   ColorbarToArea,         /* areaProc */
   ColorbarToPostscript,      /* postscriptProc */
   ColorbarScale,       /* scaleProc */
   ColorbarTranslate,         /* translateProc */
   (Tk_ItemIndexProc *)NULL,     /* indexProc */
   (Tk_ItemCursorProc *)NULL,    /* icursorProc */
   (Tk_ItemSelectionProc *)NULL, /* selectionProc */
   (Tk_ItemInsertProc *)NULL,    /* insertProc */
   (Tk_ItemDCharsProc *)NULL,    /* dTextProc */
   (Tk_ItemType *)NULL             /* nextPtr */
};

/*----------------------------------------------------------------------------
 * Nom      : <Tkcolorbar_Init>
 * Creation : Fevrier 2002 - J.P. Gauthier - CMC/CMOE
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
int Tkcolorbar_Init(Tcl_Interp *Interp) {

   Tk_glCreateItemType(&tkColorbarType);

   return(TCL_OK);
}

/*----------------------------------------------------------------------------
 * Nom      : <ColorBarCreate>
 * Creation : Fevrier 2002
` *
 * But      : Creer un item ColorBar et l'initialiser.
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
int ColorbarCreate(Tcl_Interp *Interp,Tk_Canvas Canvas,Tk_Item *Item,int Argc,Tcl_Obj *CONST Argv[]){

   ColorbarItem *cb=(ColorbarItem *)Item;

    /*Initialize item's record.*/

   cb->canvas     = Canvas;
   cb->anchor     = TK_ANCHOR_CENTER;
   cb->Font       = NULL;
   cb->BGColor    = NULL;
   cb->FGColor    = NULL;
   cb->Width      = 0;
   cb->Height     = 0;
   cb->x          = 0;
   cb->y          = 0;
   cb->Data       = NULL;
   cb->DataStr    = NULL;
   cb->NbData     = 0;
   cb->Alpha      = 100;
   cb->Id         = 1;
   cb->Icon       = 1;
   cb->BarSplit   = 0;
   cb->BarWidth   = 15;
   cb->BarBorder  = 0;
   cb->BarSide    = TK_JUSTIFY_RIGHT;
   cb->ShowFactor = 1;

   /*Process the arguments to fill in the item record*/

   if (ColorbarConfigure(Interp,Canvas,Item,Argc,Argv,0) != TCL_OK){
      ColorbarDelete(Canvas,Item,Tk_Display(Tk_CanvasTkwin(Canvas)));
      return(TCL_ERROR);
   }
   return(TCL_OK);
}

/*----------------------------------------------------------------------------
 * Nom      : <ColorBarCoords>
 * Creation : Fevrier 2002
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
int ColorbarCoords(Tcl_Interp *Interp,Tk_Canvas Canvas,Tk_Item *Item,int Argc,Tcl_Obj *CONST Argv[]){

   ColorbarItem *cb=(ColorbarItem *)Item;
   char x[TCL_DOUBLE_SPACE],y[TCL_DOUBLE_SPACE];

   if (Argc == 0) {
      Tcl_PrintDouble(Interp,cb->x,x);
      Tcl_PrintDouble(Interp,cb->y,y);
      Tcl_AppendResult(Interp,x," ",y,(char *)NULL);
   } else if (Argc == 2) {
      if ((Tk_CanvasGetCoordFromObj(Interp,Canvas,Argv[0],&cb->x) != TCL_OK) ||
          (Tk_CanvasGetCoordFromObj(Interp,Canvas,Argv[1],&cb->y) != TCL_OK)) {
          return(TCL_ERROR);
      }
      ColorbarBBox(Canvas,cb);
   } else {
      sprintf(Interp->result,"ColorbarCoords: wrong # coordinates,  expected 0 or 2, got %d",Argc);
      return(TCL_ERROR);
   }
   return(TCL_OK);
}

/*----------------------------------------------------------------------------
 * Nom      : <ColorbarConfigure>
 * Creation : Fevrier 2002
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
int ColorbarConfigure(Tcl_Interp *Interp,Tk_Canvas Canvas,Tk_Item *Item,int Argc,Tcl_Obj *CONST Argv[],int Flags){

   ColorbarItem *cb=(ColorbarItem*)Item;

   if (Tk_ConfigureWidget(Interp,Tk_CanvasTkwin(Canvas),ColorbarSpecs,Argc,(CONST84 char**)Argv,(char*)cb,Flags) != TCL_OK) {
      return(TCL_ERROR);
   }

   cb->Alpha=cb->Alpha<0?0:cb->Alpha>100?100:cb->Alpha;

   ColorbarBBox(Canvas,cb);
   return(TCL_OK);
}

/*----------------------------------------------------------------------------
 * Nom      : <ColorbarDelete>
 * Creation : Fevrier 2002
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
void ColorbarDelete(Tk_Canvas Canvas,Tk_Item *Item,Display *Disp){

   ColorbarItem *cb=(ColorbarItem*)Item;

   if (cb->Font)    Tk_FreeFont(cb->Font);
   if (cb->BGColor) Tk_FreeColor(cb->BGColor);
   if (cb->FGColor) Tk_FreeColor(cb->FGColor);

   if (cb->DataStr) free(cb->DataStr);
   if (cb->Data)    Tcl_Free((char*)cb->Data);
}

/*----------------------------------------------------------------------------
 * Nom      : <ColorbarBBox>
 * Creation : Fevrier 2002
 *
 * But      : This procedure is invoked to compute the bounding box of
 *       all the pixels that may be drawn as part of a pixmap item.
 *       This procedure is where the child pixmap's placement is
 *       computed
 *
 * Parametres :
 *  <Canvas>  : Canvas that contains item
 *  <CB      : Item whose bbox is to be recomputed
 *
 * Retour:
 *
 * Remarques :
 *
 *----------------------------------------------------------------------------
*/
void ColorbarBBox(Tk_Canvas Canvas,ColorbarItem *CB){

   int x, y;

   x=(int)(CB->x+((CB->x>=0)?0.5:- 0.5));
   y=(int)(CB->y+((CB->y>=0)?0.5:- 0.5));

   if (CB->Width==0 || CB->Height==0) {
      CB->header.x1 = CB->header.x2 = x;
      CB->header.y1 = CB->header.y2 = y;
      return;
   }

   /*Compute location and size of pixmap, using anchor information.*/

   switch (CB->anchor) {
      case TK_ANCHOR_N:
         x -= CB->Width/2;
         break;
      case TK_ANCHOR_NE:
         x -= CB->Width;
         break;
      case TK_ANCHOR_E:
         x -= CB->Width;
         y -= CB->Height/2;
         break;
      case TK_ANCHOR_SE:
         x -= CB->Width;
         y -= CB->Height;
         break;
      case TK_ANCHOR_S:
         x -= CB->Width/2;
         y -= CB->Height;
         break;
      case TK_ANCHOR_SW:
         y -= CB->Height;
         break;
      case TK_ANCHOR_W:
         y -= CB->Height/2;
         break;
      case TK_ANCHOR_NW:
         break;
      case TK_ANCHOR_CENTER:
         x -= CB->Width/2;
         y -= CB->Height/2;
         break;
   }

   /*Store the information in the item header.*/

   CB->header.x1 = x ;
   CB->header.y1 = y ;
   CB->header.x2 = x + CB->Width;
   CB->header.y2 = y + CB->Height+1;
}

/*----------------------------------------------------------------------------
 * Nom      : <ColorbarDisplay>
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
void ColorbarDisplay(Tk_Canvas Canvas,Tk_Item *Item,Display *Disp,Drawable Draw,int X,int Y,int Width,int Height){

   ColorbarItem *cb=(ColorbarItem*)Item;
   TDataSpec    *spec=NULL;
   TData        *fld=NULL;
   TObs         *obs=NULL;
   OGR_Layer    *layer=NULL;
   GDAL_Band    *band=NULL;

   int          dv=0,dh=0,y1,y2,x1,x2,i,inc=0;
   int          w,h,x,y;

   glShadeModel(GL_FLAT);
   glEnable(GL_SCISSOR_TEST);

   if (cb->NbData) {
      if (cb->Width<cb->Height) {
         inc=(cb->header.y2-cb->header.y1-5*(cb->NbData-1))/cb->NbData;
         y1=cb->header.y1;
         x1=cb->header.x1;
      } else {
         inc=(cb->header.x2-cb->header.x1-5*(cb->NbData-1))/cb->NbData;
         y1=cb->header.y1;
         x1=cb->header.x1;
      }
   }

   glPushMatrix();
   glTranslated(-((TkCanvas *)Canvas)->xOrigin,-((TkCanvas *)Canvas)->yOrigin,0.0);

   for (i=0;i<cb->NbData;i++) {

      if (cb->Width<cb->Height) {
         y2=y1+inc-1;
         y2=y2>cb->header.y2?cb->header.y2:y2;
         x2=cb->header.x2;
      } else {
         x2=x1+inc-1;
         x2=x2>cb->header.x2?cb->header.x2:x2;
         y2=cb->header.y2;
      }

      if (GLRender->MagScale>1)
         Height=GLRender->MagY+GLRender->MagD/GLRender->MagScale;

      x=(cb->header.x1-((TkCanvas*)Canvas)->xOrigin-1-GLRender->MagX)*GLRender->MagScale+GLRender->MagD/2;
      y=(Height-(y2-((TkCanvas*)Canvas)->yOrigin)-1)*GLRender->MagScale-GLRender->MagD/2;
      w=(cb->header.x2-cb->header.x1+2)*GLRender->MagScale;
      h=(y2-y1+2)*GLRender->MagScale;
      trScissor(GLRender->TRCon,x,y,w,h);

      /*Effectuer le rendue de la colorbar*/
      if (cb->Alpha<100) {
         glEnable(GL_BLEND);
      }

      /*Pourtour*/
      if (cb->BGColor) {
         glLineWidth(1.0);
         glPolygonMode(GL_FRONT,GL_FILL);
         glColor4us(cb->BGColor->red,cb->BGColor->green,cb->BGColor->blue,cb->Alpha*655);
         glBegin(GL_QUADS);
            glVertex2i(x1,y1);
            glVertex2i(x1,y2);
            glVertex2i(x2,y2);
            glVertex2i(x2,y1);
         glEnd();

         glPolygonMode(GL_FRONT,GL_LINE);
         glColor4us(cb->FGColor->red,cb->FGColor->green,cb->FGColor->blue,cb->Alpha*655);
         glBegin(GL_QUADS);
            glVertex2i(x1,y1);
            glVertex2i(x1,y2);
            glVertex2i(x2,y2);
            glVertex2i(x2,y1);
         glEnd();
         glDisable(GL_BLEND);
      }

      if (fld=Data_Get(cb->Data[i])) {
         Data_PreInit(fld);
         spec=fld->Spec;
      } else if (obs=Obs_Get(cb->Data[i])) {
         Obs_PreInit(obs);
         spec=obs->Spec;
      } else if (layer=OGR_LayerGet(cb->Data[i])) {
         OGR_LayerPreInit(layer);
         spec=layer->Spec;
      } else if (band=GDAL_BandGet(cb->Data[i])) {
         spec=band->Spec;
      } else {
         spec=DataSpec_Get(cb->Data[i]);
      }

      if (!spec)
         continue;

      cb->UColor=spec->Outline?spec->Outline:cb->FGColor;
      cb->UFont=spec->Font?spec->Font:cb->Font;

      if (cb->UFont) {
         Tk_GetFontMetrics(cb->UFont,&cb->tkm);
         glFontUse(Disp,cb->UFont);
      }

      /*Check orientation*/
      if (cb->Width<cb->Height) {
         /*Vertical*/
         dh=Colorbar_RenderId(NULL,cb,spec,y1);
         dh+=Colorbar_RenderContour(NULL,cb,spec,x1,x2,y1+dh);
         dv=Colorbar_RenderVector(NULL,cb,spec,y1,y2);
         Colorbar_RenderTexture(NULL,cb,spec,y1+dh,y2-dv);
         y1=y2+10;
      } else {
         /*Horizontal*/
         dh=Colorbar_HRenderId(NULL,cb,spec,x1);
         dh+=Colorbar_RenderContour(NULL,cb,spec,x1,x2,y1+dh);
         dv=Colorbar_HRenderVector(NULL,cb,spec,x1,x2);
         Colorbar_HRenderTexture(NULL,cb,spec,x1,x2-dv);
         x1=x2+10;
      }
   }

   glPopMatrix();
   glDisable(GL_SCISSOR_TEST);
   glDisable(GL_BLEND);
   glShadeModel(GL_SMOOTH);
}

/*----------------------------------------------------------------------------
 * Nom      : <Colorbar_RenderContour>
 * Creation : Avril 2003 - J.P. Gauthier - CMC/CMOE
 *
 * But      : Effectue l'affichage de l'entete.
 *
 * Parametres :
 *  <Interp>  : Interpreteur TCL
 *  <CB>      : Colorbar item
 *  <Spec>    : Specification des donnees
 *  <Y1>      : Coordonne Y
 *
 * Retour:
 *
 * Remarques :
 *
 *----------------------------------------------------------------------------
*/
int Colorbar_RenderContour(Tcl_Interp *Interp,ColorbarItem *CB,TDataSpec *Spec,int X1,int X2,int Y1) {

   char buf[128];
   int  dx=0,dy=0;

   if (CB->Icon) {
      if (Interp) {
         if (Spec->Width) {
            sprintf(buf,"%i setlinewidth 1 setlinecap 1 setlinejoin\n",Spec->Width);
            Tcl_AppendResult(Interp,buf,(char*)NULL);
         }
         if (Spec->Outline)
            Tk_CanvasPsColor(Interp,CB->canvas,Spec->Outline);
      } else {
         if (Spec->Width)
            glLineWidth(Spec->Width);
         if (Spec->Outline)
            glColor3us(Spec->Outline->red,Spec->Outline->green,Spec->Outline->blue);
      }

      if (Spec->Icon) {
         if (Interp) {
            glFeedbackInit(IconList[Spec->Icon].Nb*40,GL_2D);
         }

         glPushMatrix();
         glPolygonMode(GL_FRONT_AND_BACK,GL_LINE);
         glEnableClientState(GL_VERTEX_ARRAY);
         glVertexPointer(2,GL_DOUBLE,0,IconList[Spec->Icon].Co);
         glTranslated(X2-13,Y1+8,0.0);
         glScalef(7.0,-7.0,1.0);
         glDrawArrays(IconList[Spec->Icon].Type,0,IconList[Spec->Icon].Nb);
         glDisableClientState(GL_VERTEX_ARRAY);
         glPopMatrix();
         dx+=20;
         dy=14+Spec->Width;

         if (Interp) {
            glFeedbackProcess(Interp,GL_2D);
         }
      }

      if (Spec->RenderContour || Spec->Width) {

         if (Interp) {
            glPostscriptDash(Interp,&Spec->Dash,Spec->Width);
         } else {
            glDash(&Spec->Dash);
         }

         if (Interp) {
            sprintf(buf,"%i %i moveto %i %i lineto stroke\n",X1+5,(int)Tk_CanvasPsY(CB->canvas,Y1+8),X2-5-dx,(int)Tk_CanvasPsY(CB->canvas,Y1+8));
            Tcl_AppendResult(Interp,buf,(char*)NULL);
            glPostscriptDash(Interp,NULL,Spec->Width);
         } else {
            glBegin(GL_LINES);
               glVertex2i(X1+5,Y1+8);
               glVertex2i(X2-5-dx,Y1+8);
            glEnd();
            glDisable(GL_LINE_STIPPLE);
         }
         dy=14+Spec->Width;
      }
   }

   return(dy);
}

/*----------------------------------------------------------------------------
 * Nom      : <Colorbar_RenderId>
 * Creation : Avril 2003 - J.P. Gauthier - CMC/CMOE
 *
 * But      : Effectue l'affichage de l'entete dans l'orientation verticale
 *
 * Parametres :
 *  <Interp>  : Interpreteur TCL
 *  <CB>      : Colorbar item
 *  <Spec>    : Specification des donnees
 *  <Y1>      : Coordonne Y
 *
 * Retour:
 *
 * Remarques :
 *
 *----------------------------------------------------------------------------
*/
int Colorbar_RenderId(Tcl_Interp *Interp,ColorbarItem *CB,TDataSpec *Spec,int Y1) {

   char    buf[512];
   int     y;

   if (CB->Id && CB->UFont && CB->UColor) {
      y=CB->tkm.linespace;
      if (Interp) {
         glPostscriptText(Interp,CB->canvas,Spec->Desc,CB->header.x2-6,Tk_CanvasPsY(CB->canvas,Y1+y),0,CB->UColor,-1.0,1.0,1.0);
      } else {
         Colorbar_RenderText(CB,CB->header.x2-6,Y1+y,TK_JUSTIFY_RIGHT,Spec->Desc,Spec);
      }

      if (CB->ShowFactor) {
         if (Spec->ValDelta!=0.0) {
            snprintf(buf,256,"[+%1.2e]",Spec->ValDelta);

            y+=CB->tkm.linespace+2;
            if (Interp) {
               glPostscriptText(Interp,CB->canvas,buf,CB->header.x2-6,Tk_CanvasPsY(CB->canvas,Y1+y),0,CB->UColor,-1.0,1.0,1.0);
            } else {
               Colorbar_RenderText(CB,CB->header.x2-6,Y1+y,TK_JUSTIFY_RIGHT,buf,Spec);
            }
         }

         if (Spec->ValFactor!=1.0) {
            snprintf(buf,256,"[x%1.2e]",Spec->ValFactor);

            y+=CB->tkm.linespace+2;
            if (Interp) {
               glPostscriptText(Interp,CB->canvas,buf,CB->header.x2-6,Tk_CanvasPsY(CB->canvas,Y1+y),0,CB->UColor,-1.0,1.0,1.0);
            } else {
               Colorbar_RenderText(CB,CB->header.x2-6,Y1+y,TK_JUSTIFY_RIGHT,buf,Spec);
            }
         }
      }

      if (Spec->Unit && Spec->Unit[0]!='\0') {
         snprintf(buf,256,"(%s)",Spec->Unit);
         y+=CB->tkm.linespace+2;
         if (Interp) {
            glPostscriptText(Interp,CB->canvas,buf,CB->header.x2-6,Tk_CanvasPsY(CB->canvas,Y1+y),0,CB->UColor,-1.0,1.0,1.0);
         } else {
            Colorbar_RenderText(CB,CB->header.x2-6,Y1+y,TK_JUSTIFY_RIGHT,buf,Spec);
         }
      }

      return(y+5);
   } else {
      return(0);
   }
}

/*----------------------------------------------------------------------------
 * Nom      : <Colorbar_HRenderId>
 * Creation : Avril 2003 - J.P. Gauthier - CMC/CMOE
 *
 * But      : Effectue l'affichage de l'entete dans l'orientation horizontale.
 *
 * Parametres :
 *  <Interp>  : Interpreteur TCL
 *  <CB>      : Colorbar item
 *  <Spec>    : Specification des donnees
 *  <X1>      : Coordonne X
 *
 * Retour:
 *
 * Remarques :
 *
 *----------------------------------------------------------------------------
*/
int Colorbar_HRenderId(Tcl_Interp *Interp,ColorbarItem *CB,TDataSpec *Spec,int X1) {

   char buf[512];

   if (CB->Id && CB->UFont && CB->UColor) {
      sprintf(buf,"%s",Spec->Desc);

      if (CB->ShowFactor) {
         if (Spec->ValDelta!=0.0) {
            if (Spec->ValFactor!=1.0) {
               sprintf(buf,"%s [+ %1.2e x %1.2e]",buf,Spec->ValDelta,Spec->ValFactor);
            } else {
               sprintf(buf,"%s [+ %1.2e]",buf,Spec->ValDelta);
            }
         } else if (Spec->ValFactor!=1.0) {
            sprintf(buf,"%s [x %1.2e]",buf,Spec->ValFactor);
         }
      }

      if (Spec->Unit && Spec->Unit[0]!='\0') {
         sprintf(buf,"%s (%s)",buf,Spec->Unit);
      }

      if (Interp) {
         glPostscriptText(Interp,CB->canvas,buf,X1+5,Tk_CanvasPsY(CB->canvas,CB->header.y1+CB->tkm.linespace),0,CB->UColor,0.0,1.0,0.0);
      } else {
         Colorbar_RenderText(CB,X1+5,CB->header.y1+CB->tkm.linespace,TK_JUSTIFY_LEFT,buf,Spec);
      }
      return(CB->tkm.linespace+5);
   } else {
      return(0);
   }
}

/*----------------------------------------------------------------------------
 * Nom      : <Colorbar_RenderText>
 * Creation : Avril 2002 - J.P. Gauthier - CMC/CMOE
 *
 * But      : Effectue l'affichage du texte.
 *
 * Parametres :
 *  <CB>      : Colorbar
 *  <X>       : Coordonnee en X
 *  <Y>       : Coordonnee en Y
 *  <Side>    : Cote de l'ancrage
 *  <Text>    : Chaine
 *  <Spec>    : Configuration de rendue
 *
 * Retour:
 *
 * Remarques :
 *
 *----------------------------------------------------------------------------
*/
void Colorbar_RenderText(ColorbarItem *CB,int X,int Y,Tk_Justify Side,char *Text,TDataSpec *Spec) {

   Tcl_DString runString;

   if (CB->UColor && Text && CB->UFont) {
      glColor4us(CB->UColor->red,CB->UColor->green,CB->UColor->blue,CB->Alpha*655);

         Tcl_DStringInit(&runString);
         Tcl_UtfToUniCharDString(Text,-1,&runString);

      if (Side==TK_JUSTIFY_RIGHT) {
         glDrawString(X-Tk_TextWidth(CB->UFont,Text,strlen(Text)),Y,0,Tcl_DStringValue(&runString),Tcl_DStringLength(&runString),1,1);
      } else {
         glDrawString(X,Y,0,Tcl_DStringValue(&runString),Tcl_DStringLength(&runString),1,1);
      }
      Tcl_DStringFree(&runString);
   }
}

/*----------------------------------------------------------------------------
 * Nom      : <Colorbar_RenderTexture>
 * Creation : Fevrier 2002 - J.P. Gauthier - CMC/CMOE
 *
 * But      : Effectue l'affichage de la colorbar des valeurs dans
 *            l'orientation verticale.
 *
 * Parametres :
 *  <Interp>  : Interpreteur TCL
 *  <CB>      : Colorbar item
 *  <Spec>    : Specification des donnees
 *  <Y1>      : Coordonne Y haut
 *  <Y2>      : Coordonne Y bas
 *
 * Retour:
 *
 * Remarques :
 *
 *----------------------------------------------------------------------------
*/
void Colorbar_RenderTexture(Tcl_Interp *Interp,ColorbarItem *CB,TDataSpec *Spec,int Y1,int Y2){

   int      idx,i,py0,py1,x0,x1,xt,n=0;
   double   y=0,incr,txt=0,value,height,inter;
   float    jps,jan;
   char     buf[128],*lbl;
   Tcl_Obj *obj;

   if ((!Spec->RenderTexture && !Spec->RenderParticle && !Spec->MapAll) || !Spec->Map)
      return;

   glPolygonMode(GL_FRONT,GL_FILL);

   height=Y2-Y1-10;

   /*De quel cote est le texte*/
   if (CB->BarSide==TK_JUSTIFY_RIGHT) {
      x0=CB->header.x2-(CB->BarWidth+5);
      x1=CB->header.x2-5;
      xt=CB->header.x2-(CB->BarWidth+10);
      jps=1.0;
      jan=-1.0;
   } else {
      x0=CB->header.x1+5;
      x1=CB->header.x1+(CB->BarWidth+5);
      xt=CB->header.x1+(CB->BarWidth+10);
      jps=0.0;
      jan=0.0;
   }

   if (Spec->InterLabels) {
      Tcl_ListObjLength(Interp,Spec->InterLabels,&n);
   }

   /*Dans la cas ou on a des intervalles*/
   if (Spec->InterNb>0) {

      y=Y2-5;
      incr=height/(double)(Spec->InterNb-!Spec->MapAbove+Spec->MapBellow);

      /*Rendu de l'echelle*/
      for (i=0-Spec->MapBellow;i<Spec->InterNb-!Spec->MapAbove;i++) {

         inter=i<0?Spec->Inter[0]-1:Spec->Inter[i];
         VAL2COL(idx,Spec,inter);

         /*Afficher la couleur*/
         if (Interp) {
            CMap_PostscriptColor(Interp,Spec->Map,idx);
            py0=Tk_CanvasPsY(CB->canvas,y);
            py1=Tk_CanvasPsY(CB->canvas,y-incr+CB->BarSplit-1);

            sprintf(buf,"%i %i moveto %i %i lineto %i %i lineto %i %i lineto closepath",x0,py0,x1,py0,x1,py1,x0,py1);
            Tcl_AppendResult(Interp,buf," fill\n",(char*)NULL);

            if (CB->BarBorder) {
               Tk_CanvasPsColor(Interp,CB->canvas,CB->FGColor);
                  sprintf(buf,"%i %i moveto %i %i lineto %i %i lineto %i %i lineto closepath",x0,py0,x1,py0,x1,py1,x0,py1);
               Tcl_AppendResult(Interp,buf,(char*)NULL);
               sprintf(buf," %i setlinewidth 1 setlinecap 1 setlinejoin\nstroke\n",CB->BarBorder);
               Tcl_AppendResult(Interp,buf,(char*)NULL);
            }
         } else {
            if (Spec->Map->Alpha) {
               glEnable(GL_BLEND);
            }
            glPolygonMode(GL_FRONT,GL_FILL);
            glColor4ubv(Spec->Map->Color[idx]);
            glLineWidth(1.0);
            glBegin(GL_QUADS);
               glVertex2f(x0,y);
               glVertex2f(x1,y);
               glVertex2f(x1,y-incr+CB->BarSplit);
               glVertex2f(x0,y-incr+CB->BarSplit);
            glEnd();
            glDisable(GL_BLEND);

            if (CB->BarBorder) {
               glPolygonMode(GL_FRONT,GL_LINE);
               glLineWidth(CB->BarBorder);
               glColor4us(CB->FGColor->red,CB->FGColor->green,CB->FGColor->blue,CB->Alpha*655);
               glBegin(GL_QUADS);
                  glVertex2f(x0,y);
                  glVertex2f(x1,y);
                  glVertex2f(x1,y-incr+CB->BarSplit);
                  glVertex2f(x0,y-incr+CB->BarSplit);
               glEnd();
            }
         }
          y-=incr;
      }

      /*Rendu des valeurs l'echelle*/
      for (i=0,y=Y2-5-(Spec->MapBellow*incr);i<Spec->InterNb;i++,y-=incr) {
         if (i<n) {
            Tcl_ListObjIndex(Interp,Spec->InterLabels,i,&obj);
            lbl=Tcl_GetString(obj);
         } else {
            DataSpec_Format(Spec,VAL2SPEC(Spec,Spec->Inter[i]),buf);
            lbl=buf;
         }

         /*Check last label if no mapabove color*/
         if (!Spec->MapAbove && i==Spec->InterNb-1) {
            y+=CB->tkm.linespace;
         }
         if (Interp) {
            glPostscriptText(Interp,CB->canvas,lbl,xt,Tk_CanvasPsY(CB->canvas,y),0,CB->UColor,jan,1.0,jps);
         } else {
            Colorbar_RenderText(CB,xt,y,CB->BarSide,lbl,Spec);
         }
      }

   /*Dans le cas ou le champs est continue*/
   } else {
      if (Spec->Map->Alpha) {
         glEnable(GL_BLEND);
      }
      incr=height/(double)Spec->Map->NbPixels;

      /*Rendu de l'echelle*/
      glBegin(GL_QUAD_STRIP);

         y=Y2-5;
         glColor4ubv(Spec->Map->Color[0]);
         glVertex2f(x0,y);
         glVertex2f(x1,y);

         y-=incr;

         for (idx=0;idx<Spec->Map->NbPixels;idx++,y-=incr) {

            glColor4ubv(Spec->Map->Color[idx]);

            /*Afficher la couleur*/

            if (Interp) {
               CMap_PostscriptColor(Interp,Spec->Map,idx);
               py0=Tk_CanvasPsY(CB->canvas,y+incr);
               py1=Tk_CanvasPsY(CB->canvas,Y2-5-height);

               sprintf(buf,"%i %i moveto %i %i lineto %i %i lineto %i %i lineto closepath",x0,py0,x1,py0,x1,py1,x0,py1);
               Tcl_AppendResult(Interp,buf," fill\n",(char*)NULL);
            } else {
               glVertex2f(x0,y);
               glVertex2f(x1,y);
            }
      }
      glEnd();
      glDisable(GL_BLEND);

      if (CB->BarBorder) {
         if (Interp) {
            py0=Tk_CanvasPsY(CB->canvas,Y2-5);
            py1=Tk_CanvasPsY(CB->canvas,y+incr);
            Tk_CanvasPsColor(Interp,CB->canvas,CB->FGColor);
               sprintf(buf,"%i %i moveto %i %i lineto %i %i lineto %i %i lineto closepath",x0,py0,x1,py0,x1,py1,x0,py1);
            Tcl_AppendResult(Interp,buf,(char*)NULL);
            sprintf(buf," %i setlinewidth 1 setlinecap 1 setlinejoin\nstroke\n",CB->BarBorder);
            Tcl_AppendResult(Interp,buf,(char*)NULL);
         } else {
            glPolygonMode(GL_FRONT,GL_LINE);
            glLineWidth(CB->BarBorder);
            glColor4us(CB->FGColor->red,CB->FGColor->green,CB->FGColor->blue,CB->Alpha*655);
            glBegin(GL_QUADS);
               glVertex2f(x0,Y2-5);
               glVertex2f(x1,Y2-5);
               glVertex2f(x1,y+incr);
               glVertex2f(x0,y+incr);
            glEnd();
         }
      }

      /*Rendu des valeurs l'echelle*/
      if (n) {
         incr=height/(double)(n-1);
         y=5;
         for (idx=0;idx<n;idx++,y+=incr) {
            txt+=incr;
            if (y+CB->tkm.linespace>height) {
               y=height-CB->tkm.linespace/2;
            }

            Tcl_ListObjIndex(Interp,Spec->InterLabels,idx,&obj);
            lbl=Tcl_GetString(obj);

            if (txt>=CB->tkm.linespace) {

               if (Interp) {
                  glPostscriptText(Interp,CB->canvas,lbl,xt,Tk_CanvasPsY(CB->canvas,Y2-y),0,CB->UColor,jan,1.0,jps);
               } else {
                  Colorbar_RenderText(CB,xt,Y2-y,CB->BarSide,lbl,Spec);
               }
               txt=0;
            }
         }
      } else {
         DataSpec_Format(Spec,VAL2SPEC(Spec,Spec->Min),buf);
         if (Interp) {
            glPostscriptText(Interp,CB->canvas,buf,xt,Tk_CanvasPsY(CB->canvas,Y2-5),0,CB->UColor,jan,1.0,jps);
         } else {
            Colorbar_RenderText(CB,xt,Y2-5,CB->BarSide,buf,Spec);
         }

         if (Spec->Min!=Spec->Max) {

            DataSpec_Format(Spec,VAL2SPEC(Spec,Spec->Max),buf);
            if (Interp) {
               glPostscriptText(Interp,CB->canvas,buf,xt,Tk_CanvasPsY(CB->canvas,Y1+10),0,CB->UColor,jan,1.0,jps);
            } else {
               Colorbar_RenderText(CB,xt,Y1+10,CB->BarSide,buf,Spec);
            }
            y=5;

            for (idx=0;idx<Spec->Map->NbPixels;idx++,y+=incr) {

               txt+=incr;
               if (y<5+CB->tkm.linespace || y>height-CB->tkm.linespace) {
                  continue;
               }
               COL2VAL(idx,Spec,value);

               if (txt>=CB->tkm.linespace) {

                  DataSpec_Format(Spec,VAL2SPEC(Spec,value),buf);

                  if (Interp) {
                     glPostscriptText(Interp,CB->canvas,buf,xt,Tk_CanvasPsY(CB->canvas,Y2-y),0,CB->UColor,jan,1.0,jps);
                  } else {
                     Colorbar_RenderText(CB,xt,Y2-y,CB->BarSide,buf,Spec);
                  }
                  txt=0;
               }
            }
         }
      }
   }
}

/*----------------------------------------------------------------------------
 * Nom      : <Colorbar_HRenderTexture>
 * Creation : Fevrier 2002 - J.P. Gauthier - CMC/CMOE
 *
 * But      : Effectue l'affichage de la colorbar des valeurs dans
 *           l'orientation horizontale.
 *
 * Parametres :
 *  <Interp>  : Interpreteur TCL
 *  <CB>      : Colorbar item
 *  <Spec>    : Specification des donnees
 *  <Y1>      : Coordonne Y haut
 *  <Y2>      : Coordonne Y bas
 *
 * Retour:
 *
 * Remarques :
 *
 *----------------------------------------------------------------------------
*/
void Colorbar_HRenderTexture(Tcl_Interp *Interp,ColorbarItem *CB,TDataSpec *Spec,int X1,int X2){

   int      idx,i,py0,py1,px0,px1,y0,y1,xt,n=0;
   double   x=0,incr,txt,txtr,value,height,inter;
   float    jps,jan;
   char     buf[128],*lbl;
   Tcl_Obj *obj;

   if ((!Spec->RenderTexture && !Spec->RenderParticle && !Spec->MapAll) || !Spec->Map)
      return;

   glPolygonMode(GL_FRONT,GL_FILL);

   height=X2-X1-10;
   jps=0.0;
   jan=0.0;

   /*De quel cote est le texte*/
   if (CB->BarSide==TK_JUSTIFY_RIGHT) {
      y1=CB->header.y2-5-CB->tkm.linespace;
      y0=y1-CB->BarWidth;
      xt=CB->header.y2-5;
   } else {
      y0=CB->header.y2-(CB->BarWidth+5);
      y1=CB->header.y2-5;
      xt=CB->header.y2-(CB->BarWidth+10);
   }

   if (Spec->InterLabels) {
      Tcl_ListObjLength(Interp,Spec->InterLabels,&n);
   }

   /*Dans la cas ou on a des intervalles*/
   if (Spec->InterNb>0) {

      x=X1+5;
      incr=(height+CB->BarSplit)/(double)(Spec->InterNb-!Spec->MapAbove+Spec->MapBellow);

      /*Rendu de l'echelle*/
      for (i=0-Spec->MapBellow;i<Spec->InterNb-!Spec->MapAbove;i++) {

         inter=i<0?Spec->Inter[0]-1:Spec->Inter[i];
         VAL2COL(idx,Spec,inter);

         /*Afficher la couleur*/
         if (Interp) {
            CMap_PostscriptColor(Interp,Spec->Map,idx);
            py0=Tk_CanvasPsY(CB->canvas,y0);
            py1=Tk_CanvasPsY(CB->canvas,y1);
            px0=x;
            px1=x+incr-CB->BarSplit-1;
            sprintf(buf,"%i %i moveto %i %i lineto %i %i lineto %i %i lineto closepath",px0,py0,px1,py0,px1,py1,px0,py1);
            Tcl_AppendResult(Interp,buf," fill\n",(char*)NULL);

            if (CB->BarBorder) {
               Tk_CanvasPsColor(Interp,CB->canvas,CB->FGColor);
                  sprintf(buf,"%i %i moveto %i %i lineto %i %i lineto %i %i lineto closepath",px0,py0,px1,py0,px1,py1,px0,py1);
               Tcl_AppendResult(Interp,buf,(char*)NULL);
               sprintf(buf," %i setlinewidth 1 setlinecap 1 setlinejoin\nstroke\n",CB->BarBorder);
               Tcl_AppendResult(Interp,buf,(char*)NULL);
            }
         } else {
            if (Spec->Map->Alpha) {
               glEnable(GL_BLEND);
            }
            glPolygonMode(GL_FRONT,GL_FILL);
            glColor4ubv(Spec->Map->Color[idx]);
            glLineWidth(1.0);
            glBegin(GL_QUADS);
               glVertex2f(x,y0);
               glVertex2f(x,y1);
               glVertex2f(x+incr-CB->BarSplit,y1);
               glVertex2f(x+incr-CB->BarSplit,y0);
            glEnd();
            glDisable(GL_BLEND);

            if (CB->BarBorder) {
               glPolygonMode(GL_FRONT_AND_BACK,GL_LINE);
               glLineWidth(CB->BarBorder);
               glColor4us(CB->FGColor->red,CB->FGColor->green,CB->FGColor->blue,CB->Alpha*655);
               glBegin(GL_QUADS);
                  glVertex2f(x,y0);
                  glVertex2f(x,y1);
                  glVertex2f(x+incr-CB->BarSplit,y1);
                  glVertex2f(x+incr-CB->BarSplit,y0);
               glEnd();
            }
         }
         x+=incr;
      }

      /*Rendu des valeurs l'echelle*/
      for (i=0,x=X1+5+(Spec->MapBellow*incr);i<Spec->InterNb;i++,x+=incr) {
         if (i<n) {
            Tcl_ListObjIndex(Interp,Spec->InterLabels,i,&obj);
            lbl=Tcl_GetString(obj);
         } else {
            DataSpec_Format(Spec,VAL2SPEC(Spec,Spec->Inter[i]),buf);
            lbl=buf;
         }

         /*Check last label if no mapabove color*/
         if (!Spec->MapAbove && i==Spec->InterNb-1) {
            x-=Tk_TextWidth(CB->UFont,lbl,strlen(lbl));
         }

         if (Interp) {
            glPostscriptText(Interp,CB->canvas,lbl,x,Tk_CanvasPsY(CB->canvas,xt),0,CB->UColor,jan,1.0,jps);
         } else {
            Colorbar_RenderText(CB,x,xt,TK_JUSTIFY_LEFT,lbl,Spec);
         }
      }

   /*Dans le cas ou le champs est continue*/
   } else {
      if (Spec->Map->Alpha) {
         glEnable(GL_BLEND);
      }
      incr=height/(double)Spec->Map->NbPixels;

      /*Rendu de l'echelle*/
      glBegin(GL_QUAD_STRIP);

         x=X1+5;
         glColor4ubv(Spec->Map->Color[0]);
         glVertex2f(x,y0);
         glVertex2f(x,y1);

         x+=incr;

         for (idx=0;idx<Spec->Map->NbPixels;idx++,x+=incr) {

            glColor4ubv(Spec->Map->Color[idx]);

            /*Afficher la couleur*/

            if (Interp) {
               CMap_PostscriptColor(Interp,Spec->Map,idx);
               py0=Tk_CanvasPsY(CB->canvas,y0);
               py1=Tk_CanvasPsY(CB->canvas,y1);
               px0=x;
               px1=X2-5;

               sprintf(buf,"%i %i moveto %i %i lineto %i %i lineto %i %i lineto closepath",px0,py0,px1,py0,px1,py1,px0,py1);
               Tcl_AppendResult(Interp,buf," fill\n",(char*)NULL);
            } else {
               glVertex2f(x,y0);
               glVertex2f(x,y1);
            }
      }
      glEnd();
      glDisable(GL_BLEND);

      if (CB->BarBorder) {
         px0=X1+5;
         px1=X2-5;

         if (Interp) {
            py0=Tk_CanvasPsY(CB->canvas,y0);
            py1=Tk_CanvasPsY(CB->canvas,y1);
            Tk_CanvasPsColor(Interp,CB->canvas,CB->FGColor);
               sprintf(buf,"%i %i moveto %i %i lineto %i %i lineto %i %i lineto closepath",px0,py0,px1,py0,px1,py1,px0,py1);
            Tcl_AppendResult(Interp,buf,(char*)NULL);
            sprintf(buf," %i setlinewidth 1 setlinecap 1 setlinejoin\nstroke\n",CB->BarBorder);
            Tcl_AppendResult(Interp,buf,(char*)NULL);
         } else {
            glPolygonMode(GL_FRONT_AND_BACK,GL_LINE);
            glLineWidth(CB->BarBorder);
            glColor4us(CB->FGColor->red,CB->FGColor->green,CB->FGColor->blue,CB->Alpha*655);
            glBegin(GL_QUADS);
               glVertex2f(px0,y0);
               glVertex2f(px0,y1);
               glVertex2f(px1,y1);
               glVertex2f(px1,y0);
            glEnd();
         }
      }

      /*Rendu des valeurs l'echelle*/
      if (n) {
         incr=height/(double)(n-1);

         for (idx=0;idx<n;idx++,x+=incr) {
            txtr=X2-5;
            x=0;

            if (x>txt) {
               Tcl_ListObjIndex(Interp,Spec->InterLabels,idx,&obj);
               lbl=Tcl_GetString(obj);
               txt+=Tk_TextWidth(CB->UFont,buf,strlen(buf))+CB->tkm.linespace*2;
               if (txt>=txtr) {
                  break;
               }

               if (Interp) {
                  glPostscriptText(Interp,CB->canvas,buf,X1+x,Tk_CanvasPsY(CB->canvas,xt),0,CB->UColor,jan,1.0,jps);
               } else {
                  Colorbar_RenderText(CB,X1+x,xt,TK_JUSTIFY_LEFT,buf,Spec);
               }
            }
         }
      } else {

         DataSpec_Format(Spec,VAL2SPEC(Spec,Spec->Min),buf);
         if (Interp) {
            glPostscriptText(Interp,CB->canvas,buf,X1+5,Tk_CanvasPsY(CB->canvas,xt),0,CB->UColor,0.0,1.0,0.0);
         } else {
            Colorbar_RenderText(CB,X1+5,xt,TK_JUSTIFY_LEFT,buf,Spec);
         }
         txt=Tk_TextWidth(CB->UFont,buf,strlen(buf))+CB->tkm.linespace*2;

         if (Spec->Min!=Spec->Max) {
            DataSpec_Format(Spec,VAL2SPEC(Spec,Spec->Max),buf);
            txtr=Tk_TextWidth(CB->UFont,buf,strlen(buf));
            if (Interp) {
               glPostscriptText(Interp,CB->canvas,buf,X2-5,Tk_CanvasPsY(CB->canvas,xt),0,CB->UColor,-1.0,1.0,1.0);
            } else {
               Colorbar_RenderText(CB,X2-5-txtr,xt,TK_JUSTIFY_LEFT,buf,Spec);
            }
            txtr=X2-5-txtr-X1;
            x=0;

            for (idx=0;idx<Spec->Map->NbPixels;idx++,x+=incr) {

               if (x>txt) {
                  COL2VAL(idx,Spec,value);

                  DataSpec_Format(Spec,VAL2SPEC(Spec,value),buf);
                  txt+=Tk_TextWidth(CB->UFont,buf,strlen(buf))+CB->tkm.linespace*2;
                  if (txt>=txtr) {
                     break;
                  }

                  if (Interp) {
                     glPostscriptText(Interp,CB->canvas,buf,X1+x,Tk_CanvasPsY(CB->canvas,xt),0,CB->UColor,jan,1.0,jps);
                  } else {
                     Colorbar_RenderText(CB,X1+x,xt,TK_JUSTIFY_LEFT,buf,Spec);
                  }
               }
            }
         }
      }
   }
}

/*----------------------------------------------------------------------------
 * Nom      : <Colorbar_RenderVector>
 * Creation : Fevrier 2002 - J.P. Gauthier - CMC/CMOE
 *
 * But      : Effectue l'affichage des vecteurs dans l,orientation horizontale.
 *
 * Parametres :
 *  <Interp>  : Interpreteur TCL
 *  <CB>      : Colorbar item
 *  <Spec>    : Data specification
 *  <Y1>      : Coordonne Y haut
 *  <Y2>      : Coordonne Y bas
 *
 * Retour:
 *
 * Remarques :
 *
 *----------------------------------------------------------------------------
*/
int Colorbar_RenderVector(Tcl_Interp *Interp,ColorbarItem *CB,TDataSpec *Spec,int Y1,int Y2){

   int        map[2],size,xt,x0,col,i,pix[5];
   char       buf[32];
   double     inter[5],r1,r0,od,d,sz;
   float      jps,jan;
   Tk_Justify just;

   if (!Spec->RenderVector || Spec->RenderVector>=3 || !CB->UColor)
      return(0);

   if (Spec->RenderVector==BARBULE) {
      inter[0]=5;
      inter[1]=10;
      inter[2]=25;
      inter[3]=50;
      inter[4]=100;
      sz=1.0;
   } else {
      if (Spec->InterNb) {
         r0=Spec->Inter[0];
         r1=Spec->Inter[Spec->InterNb-1];
      } else {
         r0=Spec->Min;
         r1=Spec->Max;
      }
      r1=r0==r1?r1+1:r1;

      od=RANGE_ORDER(r1-r0);
      od=RANGE_INCR(od);

      r0=floor(r0/od)*od;
      r1=ceil(r1/od)*od;
      d=(r1-r0)/4.0;
      inter[0]=r0==0.0?od/10.0:r0;
      inter[4]=r1;
      inter[1]=r0+d;
      inter[2]=r0+d*2;
      inter[3]=r0+d*3;
      sz=0.75;
   }

   /*De quel cote est le texte*/
   if (CB->BarSide==TK_JUSTIFY_RIGHT) {
      x0=CB->header.x2-10.0;
      xt=CB->header.x1+10;
      jps=0.0;
      just=TK_JUSTIFY_LEFT;
      jan=0.0;
   } else {
      x0=CB->header.x1+20+VECTORSIZE(Spec,inter[0]);
      xt=CB->header.x2-10;
      jps=1.0;
      jan=-1.0;
      just=TK_JUSTIFY_RIGHT;
   }

   glMatrixMode(GL_MODELVIEW);
   glPolygonMode(GL_FRONT,GL_FILL);
   glLineWidth(Spec->Width);

   if (Interp) {
      glFeedbackInit(200,GL_2D);
      sprintf(buf,"%i\n",Spec->Width-1);
      Tcl_AppendResult(Interp,buf," setlinewidth 1 setlinecap 1 setlinejoin\n",(char*)NULL);
   }

   map[0]=Spec->MapBellow;
   map[1]=Spec->MapAbove;
   Spec->MapAbove=1;
   Spec->MapBellow=1;
   pix[0]=10;

   for(i=0;i<5;i++) {
      if (Spec->MapAll) {
         VAL2COL(col,Spec,inter[i]);
         if (Interp) {
            CMap_PostscriptColor(Interp,Spec->Map,col);
         } else {
            glColor4ubv(Spec->Map->Color[col]);
         }
      }
      size=VECTORSIZE(Spec,inter[i]);
      Data_RenderBarbule(Spec->RenderVector,0,0.0,x0,Y2-pix[i],0.0,inter[i],270.0,size,NULL);
      pix[i+1]=pix[i]+size*sz+10;
   }
   Spec->MapBellow=map[0];
   Spec->MapAbove=map[1];

   if (Interp)
      glFeedbackProcess(Interp,GL_2D);

   for(i=0;i<5;i++) {
      if (Interp) {
         DataSpec_Format(Spec,inter[i],buf);
         glPostscriptText(Interp,CB->canvas,buf,xt,Tk_CanvasPsY(CB->canvas,Y2-pix[i]),0,CB->UColor,jan,1.0,jps);
      } else {
         DataSpec_Format(Spec,inter[i],buf);
         Colorbar_RenderText(CB,xt,Y2-pix[i],just,buf,Spec);
      }
   }

   return(pix[5]);
}

/*----------------------------------------------------------------------------
 * Nom      : <Colorbar_HRenderVector>
 * Creation : Fevrier 2002 - J.P. Gauthier - CMC/CMOE
 *
 * But      : Effectue l'affichage des vecteurs dans l'orientation verticale.
 *
 * Parametres :
 *  <Interp>  : Interpreteur TCL
 *  <CB>      : Colorbar item
 *  <Spec>    : Data specification
 *  <Y1>      : Coordonne Y haut
 *  <Y2>      : Coordonne Y bas
 *
 * Retour:
 *
 * Remarques :
 *
 *----------------------------------------------------------------------------
*/
int Colorbar_HRenderVector(Tcl_Interp *Interp,ColorbarItem *CB,TDataSpec *Spec,int X1,int X2){

   int        map[2],size,xt,y0,y1,col,i,pix[6];
   char       buf[32];
   double     inter[5],r1,r0,od,d,sz;
   float      jps,jan;
   Tk_Justify just;

   if (!Spec->RenderVector || Spec->RenderVector>=3 || !CB->UColor)
      return(0);

   if (Spec->RenderVector==BARBULE) {
      inter[0]=5;
      inter[1]=10;
      inter[2]=25;
      inter[3]=50;
      inter[4]=100;
      sz=2.0;
   } else {
      if (Spec->InterNb) {
         r0=Spec->Inter[0];
         r1=Spec->Inter[Spec->InterNb-1];
      } else {
         r0=Spec->Min;
         r1=Spec->Max;
      }
      r1=r0==r1?r1+1:r1;

      od=RANGE_ORDER(r1-r0);
      od=RANGE_INCR(od);

      r0=floor(r0/od)*od;
      r1=ceil(r1/od)*od;
      d=(r1-r0)/4.0;
      inter[0]=r0==0.0?od/10.0:r0;
      inter[4]=r1;
      inter[1]=r0+d;
      inter[2]=r0+d*2;
      inter[3]=r0+d*3;
      sz=1.0;
   }

   /*De quel cote est le texte*/
   if (CB->BarSide==TK_JUSTIFY_RIGHT) {
      y1=CB->header.y2-5-CB->tkm.linespace;
      y0=y1-CB->BarWidth;
      xt=CB->header.y2-5;
   } else {
      y0=CB->header.y2-(CB->BarWidth+5);
      y1=CB->header.y2-5;
      xt=CB->header.y2-(CB->BarWidth+10);
   }
   jps=0.0;
   jan=0.0;
   just=TK_JUSTIFY_LEFT;

   glMatrixMode(GL_MODELVIEW);
   glPushMatrix();
   glPolygonMode(GL_FRONT,GL_FILL);
   glLineWidth(Spec->Width);

   if (Interp) {
      glFeedbackInit(200,GL_2D);
      sprintf(buf,"%i\n",Spec->Width-1);
      Tcl_AppendResult(Interp,buf," setlinewidth 1 setlinecap 1 setlinejoin\n",(char*)NULL);
   }

   map[0]=Spec->MapBellow;
   map[1]=Spec->MapAbove;
   Spec->MapAbove=1;
   Spec->MapBellow=1;
   pix[0]=10;

   for(i=0;i<5;i++) {
      if (Spec->MapAll) {
         VAL2COL(col,Spec,inter[i]);
         if (Interp) {
            CMap_PostscriptColor(Interp,Spec->Map,col);
         } else {
            glColor4ubv(Spec->Map->Color[col]);
         }
      }
      size=VECTORSIZE(Spec,inter[i]);
      Data_RenderBarbule(Spec->RenderVector,0,0.0,X2-pix[i],y0,0.0,inter[i],270.0,size,NULL);
      pix[i]+=(size*sz);
      pix[i+1]=pix[i]+10;
   }
   Spec->MapBellow=map[0];
   Spec->MapAbove=map[1];

   if (Interp)
      glFeedbackProcess(Interp,GL_2D);

   glPopMatrix();

   for(i=0;i<5;i++) {
      if (Interp) {
         DataSpec_Format(Spec,inter[i],buf);
         glPostscriptText(Interp,CB->canvas,buf,X2-pix[i],Tk_CanvasPsY(CB->canvas,xt),0,CB->UColor,jan,1.0,jps);
      } else {
         DataSpec_Format(Spec,inter[i],buf);
         Colorbar_RenderText(CB,X2-pix[i],xt,just,buf,Spec);
      }
   }
   return(pix[5]);
}

/*----------------------------------------------------------------------------
 * Nom      : <ColorbarToPoint>
 * Creation : Fevrier 2002
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
double ColorbarToPoint(Tk_Canvas Canvas,Tk_Item *Item,double *CoordPtr){

   ColorbarItem *cb=(ColorbarItem*)Item;

   double xDiff,yDiff;

   /*Point is outside rectangle*/

   if (CoordPtr[0] < cb->header.x1) {
      xDiff = cb->header.x1 - CoordPtr[0];
   } else if (CoordPtr[0] > cb->header.x2) {
      xDiff = CoordPtr[0] - cb->header.x2;
   } else {
      xDiff = 0;
   }

   if (CoordPtr[1] < cb->header.y1) {
      yDiff = cb->header.y1 - CoordPtr[1];
   } else if (CoordPtr[1] > cb->header.y2) {
      yDiff = CoordPtr[1] - cb->header.y2;
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
 * Nom      : <ColorbarToArea>
 * Creation : Fevrier 2002
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
int ColorbarToArea(Tk_Canvas Canvas,Tk_Item *Item,double *RectPtr){

   ColorbarItem *cb=(ColorbarItem*)Item;

   if ((RectPtr[2] <= cb->header.x1) ||
       (RectPtr[0] >= cb->header.x2) ||
       (RectPtr[3] <= cb->header.y1) ||
       (RectPtr[1] >= cb->header.y2)) {
      return(-1);
   }
   if ((RectPtr[0] <= cb->header.x1) &&
       (RectPtr[1] <= cb->header.y1) &&
       (RectPtr[2] >= cb->header.x2) &&
       (RectPtr[3] >= cb->header.y2)) {
      return(1);
   }
   return(0);
}

/*----------------------------------------------------------------------------
 * Nom      : <ColorbarScale>
 * Creation : Fevrier 2002
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
void ColorbarScale(Tk_Canvas Canvas,Tk_Item *Item,double OriginX,double OriginY,double ScaleX,double ScaleY){

   ColorbarItem *cb=(ColorbarItem*)Item;

   cb->x = OriginX + ScaleX*(cb->x - OriginX);
   cb->y = OriginY + ScaleY*(cb->y - OriginY);
   ColorbarBBox(Canvas,cb);
}

/*----------------------------------------------------------------------------
 * Nom      : <ColorbarTranslate>
 * Creation : Fevrier 2002
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
void ColorbarTranslate(Tk_Canvas Canvas,Tk_Item *Item,double DeltaX,double DeltaY){

   ColorbarItem *cb=(ColorbarItem*)Item;

   cb->x += DeltaX;
   cb->y += DeltaY;
   ColorbarBBox(Canvas,cb);
}

/*----------------------------------------------------------------------------
 * Nom      : <ColorBarToPostscript
 * Creation : Fevrier 2002
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
int ColorbarToPostscript(Tcl_Interp *Interp,Tk_Canvas Canvas,Tk_Item *Item,int Prepass){

   ColorbarItem *cb=(ColorbarItem*)Item;
   TDataSpec    *spec=NULL;
   TData        *fld=NULL;
   TObs         *obs=NULL;
   OGR_Layer    *layer=NULL;
   GDAL_Band    *band=NULL;

   double coords[8];
   int    dh=0,dv=0,y1,y2,x1,x2,i,inc;

   if (cb->Font && Tk_CanvasPsFont(Interp,Canvas,cb->Font)!=TCL_OK) {
      return(TCL_ERROR);
   }

   if (Prepass) {
      return(TCL_OK);
   }

   if (cb->NbData) {
      if (cb->Width<cb->Height) {
         inc=(cb->header.y2-cb->header.y1-5*(cb->NbData-1))/cb->NbData;
         y1=cb->header.y1;
         x1=cb->header.x1;
      } else {
         inc=(cb->header.x2-cb->header.x1-5*(cb->NbData-1))/cb->NbData;
         y1=cb->header.y1;
         x1=cb->header.x1;
      }
   }

   for (i=0;i<cb->NbData;i++) {

      if (cb->Width<cb->Height) {
         y2=y1+inc-1;
         y2=y2>cb->header.y2?cb->header.y2:y2;
         x2=cb->header.x2;
      } else {
         x2=x1+inc-1;
         x2=x2>cb->header.x2?cb->header.x2:x2;
         y2=cb->header.y2;
      }

      Tcl_AppendResult(Interp,"% Postscript de la colorbar\n1 setlinewidth 1 setlinecap 1 setlinejoin\n",(char*)NULL);

      /*Coordonnee du viewport*/
      coords[0]=x1; coords[1]=y1;
      coords[2]=x2; coords[3]=y1;
      coords[4]=x2; coords[5]=y2;
      coords[6]=x1; coords[7]=y2;

      /*Creer le background*/
      if (cb->BGColor) {
         Tk_CanvasPsColor(Interp,Canvas,cb->BGColor);
         Tk_CanvasPsPath(Interp,Canvas,coords,4);
         Tcl_AppendResult(Interp,"closepath fill\n",(char*)NULL);
      }

      Tcl_AppendResult(Interp,"gsave\n",(char*)NULL);
      Tk_CanvasPsPath(Interp,Canvas,coords,4);
      Tcl_AppendResult(Interp,"closepath clip newpath\n",(char*)NULL);

      if (fld=Data_Get(cb->Data[i])) {
         spec=fld->Spec;
      } else if (obs=Obs_Get(cb->Data[i])) {
         spec=obs->Spec;
      } else if (layer=OGR_LayerGet(cb->Data[i])) {
         spec=layer->Spec;
      } else if (band=GDAL_BandGet(cb->Data[i])) {
         spec=band->Spec;
      } else {
         spec=DataSpec_Get(cb->Data[i]);
      }

      if (!spec)
         continue;

      if (spec->Font) {
         Tk_CanvasPsFont(Interp,Canvas,spec->Font);
      } else if (cb->Font) {
         Tk_CanvasPsFont(Interp,Canvas,cb->Font);
      }

      /*Check orientation*/
      if (cb->Width<cb->Height) {
         /*Vertical*/
         dh=Colorbar_RenderId(Interp,cb,spec,y1);
         dh+=Colorbar_RenderContour(Interp,cb,spec,x1,x2,y1+dh);
         dv=Colorbar_RenderVector(Interp,cb,spec,y1,y2);
         Colorbar_RenderTexture(Interp,cb,spec,y1+dh,y2-dv);
         y1=y2+10;
      } else {
         /*Horizontal*/
         dh=Colorbar_HRenderId(Interp,cb,spec,x1);
         dh+=Colorbar_RenderContour(Interp,cb,spec,x1,x2,y1+dh);
         dv=Colorbar_HRenderVector(Interp,cb,spec,x1,x2);
         Colorbar_HRenderTexture(Interp,cb,spec,x1,x2-dv);
         x1=x2+10;
      }

      Tcl_AppendResult(Interp,"grestore\n",(char*)NULL);
      /*Creer le pourtour*/
      if (cb->FGColor) {
         Tk_CanvasPsColor(Interp,Canvas,cb->FGColor);
         Tk_CanvasPsPath(Interp,Canvas,coords,4);
         Tcl_AppendResult(Interp,"closepath stroke\n",(char*)NULL);
      }
   }
   return(TCL_OK);
}

/*----------------------------------------------------------------------------
 * Nom      : <Colorbar_DataParseProc>
 * Creation : Fevrier 2002 - J.P. Gauthier - CMC/CMOE
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
static int Colorbar_DataParseProc(ClientData Data,Tcl_Interp *Interp,Tk_Window TkWin,char *Value,char *WidgRec,int Offset){

   ColorbarItem *cb=(ColorbarItem*)WidgRec;

   if (cb->Data) {
      free(cb->DataStr);
      Tcl_Free((char*)cb->Data);
      cb->Data=NULL;
      cb->NbData=0;
   }

   cb->DataStr=strdup(Value);
   Tcl_SplitList(Interp,Value,&cb->NbData,&cb->Data);

   return(TCL_OK);
}

/*----------------------------------------------------------------------------
 * Nom      : <Colorbar_DataPrintProc>
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
static char *Colorbar_DataPrintProc(ClientData Data,Tk_Window TkWin,char *WidgRec,int Offset,Tcl_FreeProc **FreeProcPtr){

   ColorbarItem *cb=(ColorbarItem*)WidgRec;

   return(cb->DataStr);
}
