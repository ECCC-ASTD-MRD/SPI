/*==============================================================================
 * Environnement Canada
 * Centre Meteorologique Canadian
 * 2100 Trans-Canadienne
 * Dorval, Quebec
 *
 * Projet    : Creation de graph dans le canvas Tk.
 * Fichier   : tkCanvGraph.c
 * Creation  : Mai 2005 - J.P. Gauthier - CMC/CMOE
 *
 * Description: Affichage et manipulation de graph dans le canvas.
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

#include "tkCanvGraph.h"
#include "tkglCanvText.h"

static int tkCanvGraphInit=0;

extern TData* Data_Get(char *Field);
static int glCanvasWidgetCmd(ClientData clientData,Tcl_Interp *interp,int argc,Tcl_Obj *CONST argv[]);

void   Graph_RenderContour(Tcl_Interp *Interp,GraphItem *Gr,TData *Field);
void   Graph_RenderLabel(Tcl_Interp *Interp,GraphItem *Gr,TData *Field);
void   Graph_RenderTexture(GraphItem *Gr,TData *Field,int Tile);
void   Graph_RenderScaleLevel(Tcl_Interp *Interp,GraphItem *Gr,TData *Field);
void   GraphSet(Tk_Canvas Canvas,GraphItem *GR,int Width,int Height);
void   GraphUnSet(GraphItem *GR);
int    Graph_UnProject(Tcl_Interp *Interp,GraphItem  *GR,TGraphItem *Item,double X,double Y,double Z,int Extrap);

static int    GraphCoords(Tcl_Interp *Interp,Tk_Canvas Canvas,Tk_Item *Item,int Argc,Tcl_Obj *CONST Argv[]);
static int    GraphToArea(Tk_Canvas Canvas,Tk_Item *Item,double *RectPtr);
static double GraphToPoint(Tk_Canvas Canvas,Tk_Item *Item,double *CoordPtr);
static int    GraphToPostscript(Tcl_Interp *Interp,Tk_Canvas Canvas,Tk_Item *Item,int Prepass);
static void   GraphBBox(Tk_Canvas Canvas,GraphItem *CB);
static int    GraphConfigure(Tcl_Interp *Interp,Tk_Canvas Canvas,Tk_Item *Item,int Argc,Tcl_Obj *CONST Argv[],int Flags);
static int    GraphCreate(Tcl_Interp *Interp,Tk_Canvas Canvas,Tk_Item *Item,int Argc,Tcl_Obj *CONST Argv[]);
static void   GraphDelete(Tk_Canvas Canvas,Tk_Item *Item,Display *Disp);
static void   GraphDisplay(Tk_Canvas Canvas,Tk_Item *Item,Display *Disp,Drawable Drawt,int X,int Y,int Width,int Height);
static void   GraphScale(Tk_Canvas Canvas,Tk_Item *Item, double OriginX,double OriginY,double ScaleX,double ScaleY);
static void   GraphTranslate(Tk_Canvas Canvas,Tk_Item *Item,double DeltaX,double DeltaY);

static int   GraphCommand(ClientData Data,Tcl_Interp *Interp,int Objc,Tcl_Obj *CONST Objv[]);
static int   Graph_ItemParseProc _ANSI_ARGS_((ClientData clientData,Tcl_Interp *interp,Tk_Window tkwin,char *value,char *widgRec,int offset));
static char *Graph_ItemPrintProc _ANSI_ARGS_((ClientData clientData,Tk_Window tkwin,char *widgRec,int offset,Tcl_FreeProc **freeProcPtr));

static Tk_CustomOption ItemOption = { (Tk_OptionParseProc*)Graph_ItemParseProc,Graph_ItemPrintProc,(ClientData)NULL };
static Tk_CustomOption tagsOption = { Tk_CanvasTagsParseProc,Tk_CanvasTagsPrintProc,(ClientData)NULL };

/*Information used for parsing configuration specs:*/

static Tk_ConfigSpec GraphSpecs[] = {
   { TK_CONFIG_ANCHOR, "-anchor",(char *)NULL,(char *)NULL,
        "center",Tk_Offset(GraphItem,anchor),TK_CONFIG_DONT_SET_DEFAULT },
   { TK_CONFIG_FONT, "-font",(char*)NULL,(char*)NULL,
        "Helvetica -12",Tk_Offset(GraphItem,Font),0 },
   { TK_CONFIG_DOUBLE,"-x",(char *)NULL,(char *)NULL,
        "0",Tk_Offset(GraphItem,x),TK_CONFIG_DONT_SET_DEFAULT },
   { TK_CONFIG_DOUBLE,"-y",(char *)NULL,(char *)NULL,
        "0",Tk_Offset(GraphItem,y),TK_CONFIG_DONT_SET_DEFAULT },
   { TK_CONFIG_PIXELS,"-width",(char *)NULL,(char *)NULL,
        "0",Tk_Offset(GraphItem,Width),TK_CONFIG_DONT_SET_DEFAULT },
   { TK_CONFIG_PIXELS,"-height",(char *)NULL,(char *)NULL,
        "0",Tk_Offset(GraphItem,Height),TK_CONFIG_DONT_SET_DEFAULT },
   { TK_CONFIG_PIXELS,"-bd",(char *)NULL,(char *)NULL,
        "0",Tk_Offset(GraphItem,BDWidth),TK_CONFIG_DONT_SET_DEFAULT },
   { TK_CONFIG_PIXELS,"-xlegend",(char *)NULL,(char *)NULL,
        "0",Tk_Offset(GraphItem,xi),TK_CONFIG_DONT_SET_DEFAULT },
   { TK_CONFIG_PIXELS,"-ylegend",(char *)NULL,(char *)NULL,
        "0",Tk_Offset(GraphItem,yi),TK_CONFIG_DONT_SET_DEFAULT },
   { TK_CONFIG_PIXELS,"-bdlegend",(char *)NULL,(char *)NULL,
        "1",Tk_Offset(GraphItem,BDLegend),TK_CONFIG_DONT_SET_DEFAULT },
   { TK_CONFIG_BOOLEAN, "-legend", (char *) NULL, (char *) NULL,
        "1", Tk_Offset(GraphItem,Legend), TK_CONFIG_DONT_SET_DEFAULT},
   { TK_CONFIG_BOOLEAN,"-update",(char *)NULL,(char *)NULL,
        "1",Tk_Offset(GraphItem,Update),TK_CONFIG_DONT_SET_DEFAULT },
   { TK_CONFIG_COLOR,"-bg","-background",(char *)NULL,
       "white",Tk_Offset(GraphItem,BGColor),TK_CONFIG_NULL_OK },
   { TK_CONFIG_COLOR,"-fg","-foreground",(char *)NULL,
       "black",Tk_Offset(GraphItem,FGColor),TK_CONFIG_NULL_OK },
   { TK_CONFIG_COLOR,"-fill",(char*)NULL,(char *)NULL,
       "white",Tk_Offset(GraphItem,FillColor),TK_CONFIG_NULL_OK },
   { TK_CONFIG_STRING,"-title",(char *)NULL,(char *)NULL,
       "",Tk_Offset(GraphItem,Title),0 },
   { TK_CONFIG_STRING,"-type",(char *)NULL,(char *)NULL,
       "XY",Tk_Offset(GraphItem,Type),0 },
   { TK_CONFIG_CUSTOM,"-item",(char *)NULL,(char *)NULL,
       (char *)NULL,0,TK_CONFIG_NULL_OK,&ItemOption },
   { TK_CONFIG_CUSTOM,"-tags",(char *)NULL,(char *)NULL,
       (char *)NULL,0,TK_CONFIG_NULL_OK,&tagsOption },
   { TK_CONFIG_INT, "-transparency", (char *) NULL, (char *) NULL,
      "100", Tk_Offset(GraphItem,Alpha), TK_CONFIG_DONT_SET_DEFAULT},
   { TK_CONFIG_INT, "-transparencylegend", (char *) NULL, (char *) NULL,
      "100", Tk_Offset(GraphItem,AlphaLegend), TK_CONFIG_DONT_SET_DEFAULT},
   { TK_CONFIG_STRING,"-command",(char*)NULL,(char *)NULL,
      (char *)NULL,Tk_Offset(GraphItem,Command),TK_CONFIG_DONT_SET_DEFAULT },
   { TK_CONFIG_END,(char *)NULL,(char *)NULL,(char *)NULL,(char *)NULL,0,0 }
};

/*
 * The structures below defines the pixmap item type in terms of
 * procedures that can be invoked by generic item code.
 */

Tk_ItemType tkGraphType = {
   "graph",                      /* name */
   sizeof(GraphItem),            /* itemSize */
   GraphCreate,                  /* createProc */
   GraphSpecs,                   /* configSpecs */
   GraphConfigure,               /* configureProc */
   GraphCoords,                  /* coordProc */
   GraphDelete,                  /* deleteProc */
   GraphDisplay,                 /* displayProc */
   0,                            /* alwaysRedraw */
   GraphToPoint,                 /* pointProc */
   GraphToArea,                    /* areaProc */
   GraphToPostscript,            /* postscriptProc */
   GraphScale,                   /* scaleProc */
   GraphTranslate,               /* translateProc */
   (Tk_ItemIndexProc *)NULL,     /* indexProc */
   (Tk_ItemCursorProc *)NULL,    /* icursorProc */
   (Tk_ItemSelectionProc *)NULL, /* selectionProc */
   (Tk_ItemInsertProc *)NULL,    /* insertProc */
   (Tk_ItemDCharsProc *)NULL,    /* dTextProc */
   (Tk_ItemType *)NULL           /* nextPtr */
};

/*----------------------------------------------------------------------------
 * Nom      : <Tkgraph_Init>
 * Creation : Mai 2005 - J.P. Gauthier - CMC/CMOE
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
int Tkgraph_Init(Tcl_Interp *Interp) {

   Tk_glCreateItemType(&tkGraphType);

   if (!tkCanvGraphInit++) {
      TclGraphItem_Init(Interp);
      TclGraphAxis_Init(Interp);
   }
   return TCL_OK;
}

/*----------------------------------------------------------------------------
 * Nom      : <GraphCreate>
 * Creation : Mai 2005 - J.P. Gauthier - CMC/CMOE
 *
 * But      : Creer un item Graph et l'initialiser.
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
static int GraphCreate(Tcl_Interp *Interp,Tk_Canvas Canvas,Tk_Item *Item,int Argc,Tcl_Obj *CONST Argv[]){

   GraphItem *gr=(GraphItem*)Item;
    /*Initialize item's record.*/

   gr->canvas     = Canvas;
   gr->anchor     = TK_ANCHOR_CENTER;
   gr->Text       = NULL;
   gr->Font       = NULL;
   gr->BGColor    = NULL;
   gr->FGColor    = NULL;
   gr->FillColor  = NULL;
   gr->Title      = NULL;
   gr->TitleItem  = NULL;
   gr->TitleWidth = 0;
   gr->TitleHeight= 0;
   gr->Command    = NULL;
   gr->Width      = 0;
   gr->Height     = 0;
   gr->x          = 0;
   gr->y          = 0;
   gr->xi         = 0;
   gr->yi         = 0;
   gr->Legend     = 1;
   gr->BDLegend   = 1;
   gr->BDWidth    = 1;
   gr->Item       = NULL;
   gr->ItemStr    = NULL;
   gr->CB         = NULL;
   gr->NItem      = 0;
   gr->Alpha      = 100;
   gr->AlphaLegend= 100;
   gr->Frame      = NULL;
   gr->Update     = 0;
   gr->Type       = NULL;

   Tk_GetDash(Interp,".",&gr->Dash[0]);
   Tk_GetDash(Interp,"-",&gr->Dash[1]);
   Tk_GetDash(Interp,"_",&gr->Dash[2]);

   /* Create the associated colorbar */
   gr->CB=(ColorbarItem*)malloc(sizeof(ColorbarItem));
   ColorbarCreate(Interp,Canvas,(Tk_Item*)gr->CB,0,NULL);

    /*Process the arguments to fill in the item record*/

   if (GraphConfigure(Interp,Canvas,Item,Argc,Argv,0) != TCL_OK){
      GraphDelete(Canvas,Item,Tk_Display(Tk_CanvasTkwin(Canvas)));
      return TCL_ERROR;
   }

   /*Creer la commande qui permet les transformations*/
   if (gr->Command) {
      Tcl_CreateObjCommand(Interp,gr->Command,GraphCommand,(ClientData)gr,(Tcl_CmdDeleteProc *)NULL);
   }
   return(TCL_OK);
}

/*----------------------------------------------------------------------------
 * Nom      : <GraphCommand>
 * Creation : Mai 2005 - J.P. Gauthier - CMC/CMOE
 *
 * But      : Cette procedure fait l'appel des fonctions de transformations
 *            pour le graph.
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
static int GraphCommand(ClientData Data,Tcl_Interp *Interp,int Objc,Tcl_Obj *CONST Objv[]){

   GraphItem   *gr=(GraphItem*)Data;
   TGraphItem  *item=NULL;
   Tcl_Obj     *obj=NULL;
   int          idx,ex,ok;
   double       x,y,z=0.0;

   static CONST char *sopt[] = { "-unproject","-project","-pick",NULL };
   enum                opt { UNPROJECT,PROJECT,PICK };

   Tcl_ResetResult(Interp);

   if (Objc<2) {
      Tcl_WrongNumArgs(Interp,1,Objv,"command ?arg arg ...?");
      return(TCL_ERROR);
   }

   if (Tcl_GetIndexFromObj(Interp,Objv[1],sopt,"command",0,&idx)!=TCL_OK) {
      return(TCL_ERROR);
   }

   switch ((enum opt)idx) {
      case UNPROJECT:
         if(Objc!=5 && Objc!=6) {
            Tcl_WrongNumArgs(Interp,2,Objv,"x y extrap [item]");
            return(TCL_ERROR);
         }
         Tcl_GetDoubleFromObj(Interp,Objv[2],&x);
         Tcl_GetDoubleFromObj(Interp,Objv[3],&y);
         ok=Tcl_GetBooleanFromObj(Interp,Objv[4],&ex);

         if (ok==TCL_ERROR) {
            Tcl_ResetResult(Interp);
            ex=-1;
         }

         if(Objc==6) {
            item=GraphItem_Get(Tcl_GetString(Objv[5]));
         }
         x+=((TkCanvas*)(gr->canvas))->xOrigin;
         y+=((TkCanvas*)(gr->canvas))->yOrigin;
         return Graph_UnProject(Interp,gr,item,x,y,z,ex);
         break;

      case PROJECT:
         break;

      case PICK:
         if (Objc!=4){
            Tcl_WrongNumArgs(Interp,2,Objv,"x y");
            return(TCL_ERROR);
         }
         Tcl_GetDoubleFromObj(Interp,Objv[2],&x);
         Tcl_GetDoubleFromObj(Interp,Objv[3],&y);

         trViewport(GLRender->TRCon,(int)gr->xg[0],Tk_Height(Tk_CanvasTkwin(gr->canvas))-gr->yg[0],gr->xg[1]-gr->xg[0],gr->yg[0]-gr->yg[1]);
         glPickInit(x,Tk_Height(Tk_CanvasTkwin(gr->canvas))-y,2.0,2.0);

        /*Rendue des donnees vectorielle*/
         GraphSet(gr->canvas,gr,Tk_Width(Tk_CanvasTkwin(gr->canvas)),Tk_Height(Tk_CanvasTkwin(gr->canvas)));
         gr->ISide=0;
         for(idx=0;idx<gr->NItem;idx++) {
            glPushName(idx);
            item=GraphItem_Get(gr->Item[idx]);
            GraphItem_Display(NULL,gr,item,0,0,gr->xg[1]-gr->xg[0],gr->yg[0]-gr->yg[1],GL_SELECT);
            if (item->Type==WIDEBAR) gr->ISide++;
            glPopName();
         }
         GraphUnSet(gr);

         if (glPickProcess()>1) {
            obj=Tcl_NewListObj(0,NULL);
            Tcl_ListObjAppendElement(Interp,obj,Tcl_NewStringObj(gr->Item[GLRender->GLPick[0]],-1));
            Tcl_ListObjAppendElement(Interp,obj,Tcl_NewIntObj(GLRender->GLPick[1]));
            Tcl_SetObjResult(Interp,obj);
         }
         break;
   }
   return(TCL_OK);
}

/*----------------------------------------------------------------------------
 * Nom      : <GraphCoords>
 * Creation : Mai 2005 - J.P. Gauthier - CMC/CMOE
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
static int GraphCoords(Tcl_Interp *Interp,Tk_Canvas Canvas,Tk_Item *Item,int Argc,Tcl_Obj *CONST Argv[]){

   GraphItem *gr=(GraphItem *)Item;
   char x[TCL_DOUBLE_SPACE],y[TCL_DOUBLE_SPACE];

   if (Argc == 0) {
      Tcl_PrintDouble(Interp,gr->x,x);
      Tcl_PrintDouble(Interp,gr->y,y);
      Tcl_AppendResult(Interp,x," ",y,(char *)NULL);
   } else if (Argc == 2) {
      if ((Tk_CanvasGetCoordFromObj(Interp,Canvas,Argv[0],&gr->x) != TCL_OK) ||
          (Tk_CanvasGetCoordFromObj(Interp,Canvas,Argv[1],&gr->y) != TCL_OK)) {
         return TCL_ERROR;
      }
      GraphBBox(Canvas,gr);
   } else {
      sprintf(Interp->result,"GraphCoords: wrong # coordinates,  expected 0 or 2, got %d",Argc);
      return TCL_ERROR;
   }
   return TCL_OK;
}

/*----------------------------------------------------------------------------
 * Nom      : <GraphConfigure>
 * Creation : Mai 2005 - J.P. Gauthier - CMC/CMOE
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
static int GraphConfigure(Tcl_Interp *Interp,Tk_Canvas Canvas,Tk_Item *Item,int Argc,Tcl_Obj *CONST Argv[],int Flags){

   GraphItem *gr=(GraphItem*)Item;
   int       width,height;

   width  = gr->Width;
   height = gr->Height;

   if (Tk_ConfigureWidget(Interp,Tk_CanvasTkwin(Canvas),GraphSpecs,Argc,(CONST84 char**)Argv,(char*)gr,Flags) != TCL_OK) {
      return TCL_ERROR;
   }

   if ((gr->TitleItem=Tk_glGetItem(Canvas,Tcl_NewStringObj(gr->Title,-1)))) {
      if (strcmp("text",gr->TitleItem->typePtr->name)!=0) {
         Tcl_AppendResult(Interp,"\n   GraphConfigure: invalid object type, must be text item",(char*)NULL);
         return(TCL_ERROR);
      }
   }

   if (width!=gr->Width || height!=gr->Height) {
      if (gr->Frame) {
         free(gr->Frame);
         gr->Frame=NULL;
      }
   }

   gr->Update=1;
   gr->Alpha=gr->Alpha<0?0:gr->Alpha>100?100:gr->Alpha;
   GraphBBox(Canvas,gr);
   return TCL_OK;
}

/*----------------------------------------------------------------------------
 * Nom      : <GraphDelete>
 * Creation : Mai 2005 - J.P. Gauthier - CMC/CMOE
 *
 * But      : Cette procedure effectue le nettoyage memoire des structures du
 *            graph.
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
static void GraphDelete(Tk_Canvas Canvas,Tk_Item *Item,Display *Disp){

   GraphItem *gr=(GraphItem*)Item;

   Tk_FreeFont(gr->Font);
   Tk_FreeColor(gr->BGColor);
   Tk_FreeColor(gr->FGColor);
   Tk_FreeColor(gr->FillColor);

   if (gr->Text)      Tk_FreeTextLayout(gr->Text);
   if (gr->Item) {
      Tcl_Free((char*)gr->Item);
      free(gr->ItemStr);
   }

   if (gr->Command)   Tcl_Free(gr->Command);
   if (gr->Title)     Tcl_Free(gr->Title);
   if (gr->Frame)     free(gr->Frame);

   if (gr->CB) {
//      ColorbarDelete(Canvas,gr->CB,Disp);
      free(gr->CB);
   }
}

/*----------------------------------------------------------------------------
 * Nom      : <GraphBBox>
 * Creation : Mai 2005 - J.P. Gauthier - CMC/CMOE
 *
 * But      : This procedure is invoked to compute the bounding box of
 *       all the pixels that may be drawn as part of a pixmap item.
 *       This procedure is where the child pixmap's placement is
 *       computed
 *
 * Parametres :
 *  <Canvas>  : Canvas that contains item
 *  <Gr      : Item whose bbox is to be recomputed
 *
 * Retour:
 *
 * Remarques :
 *
 *----------------------------------------------------------------------------
*/
static void GraphBBox(Tk_Canvas Canvas,GraphItem *Gr){

   int x, y;

   x = Gr->x + 0.5;
   y = Gr->y + 0.5;

   if (Gr->Width==0 || Gr->Height==0) {
      Gr->header.x1 = Gr->header.x2 = x;
      Gr->header.y1 = Gr->header.y2 = y;
      return;
   }

   /*Compute location and size of pixmap, using anchor information.*/

   switch (Gr->anchor) {
      case TK_ANCHOR_N:
         x -= Gr->Width/2;
         break;
      case TK_ANCHOR_NE:
         x -= Gr->Width;
         break;
      case TK_ANCHOR_E:
         x -= Gr->Width;
         y -= Gr->Height/2;
         break;
      case TK_ANCHOR_SE:
         x -= Gr->Width;
         y -= Gr->Height;
         break;
      case TK_ANCHOR_S:
         x -= Gr->Width/2;
         y -= Gr->Height;
         break;
      case TK_ANCHOR_SW:
         y -= Gr->Height;
         break;
      case TK_ANCHOR_W:
         y -= Gr->Height/2;
         break;
      case TK_ANCHOR_NW:
         break;
      case TK_ANCHOR_CENTER:
         x -= Gr->Width/2;
         y -= Gr->Height/2;
         break;
   }

   /*Store the information in the item header.*/

   Gr->header.x1 = x ;
   Gr->header.y1 = y ;
   Gr->header.x2 = x + Gr->Width;
   Gr->header.y2 = y + Gr->Height;

   /* Figure out title params */

   if (Gr->Text)
      Tk_FreeTextLayout(Gr->Text);

   if (!Gr->TitleItem) {
      Gr->Text=Tk_ComputeTextLayout(Gr->Font,Gr->Title,Tcl_NumUtfChars(Gr->Title,strlen(Gr->Title)),0,TK_JUSTIFY_CENTER,0,&Gr->TitleWidth,&Gr->TitleHeight);
   } else {
      Gr->TitleWidth=((glTextItem*)Gr->TitleItem)->header.x2-((glTextItem*)Gr->TitleItem)->header.x1;
      Gr->TitleHeight=((glTextItem*)Gr->TitleItem)->header.y2-((glTextItem*)Gr->TitleItem)->header.y1;
   }
}

/*----------------------------------------------------------------------------
 * Nom      : <Graph_UnProject>
 * Creation : Mai 2005 - J.P. Gauthier - CMC/CMOE
 *
 * But      : This procedure is invoked to draw a pixmap item in a given
 *       drawable.
 *
 * Parametres :
 *  <Interp>  : Interpreteur Tcl
 *  <GR>      : Graph
 *  <Item>    : Item to use
 *  <X>       : X Coordinate
 *  <Y>       : Y Coordinate
 *  <Extrap>  : Extrapolation en dehors du graph (0=none,1=extrap,-1=limit)
 *
 * Retour:
 *
 * Remarques :
 *
 *----------------------------------------------------------------------------
*/
Tcl_Obj *Graph_UnProjectItem(Tcl_Interp *Interp,TGraphItem *Item,double X,double Y,double Z) {

   Tcl_Obj    *obj;
   TGraphAxis *axis;
   TData      *data;
   double      x,y,spd,dir;
   int         i;

   obj=Tcl_NewListObj(0,NULL);

   if (Item->XAxis && Item->YAxis) {
      axis=GraphAxis_Get(Item->XAxis);
      x=AXISPPOS(axis,X);
      axis=GraphAxis_Get(Item->YAxis);
      y=AXISPPOS(axis,Y);
      Tcl_ListObjAppendElement(Interp,obj,Tcl_NewDoubleObj(x));
      Tcl_ListObjAppendElement(Interp,obj,Tcl_NewDoubleObj(y));

      if (Item->Data) {
         if ((data=Data_Get(Item->Data))) {

            if (data->Ref->Grid[0]=='V') {
               for(i=0;i<data->Def->NJ;i++) {
                  if (data->Ref->Levels[0]>data->Ref->Levels[data->Def->NJ-1]) {
                     if (y>data->Ref->Levels[i]) {
                        break;
                     }
                  } else {
                     if (y<data->Ref->Levels[i]) {
                        break;
                     }
                  }
               }

               y=i+(y-data->Ref->Levels[i-1])/(data->Ref->Levels[i]-data->Ref->Levels[i-1])-1;
            }
            spd=VertexVal(data->Ref,data->Def,x,y,0.0);
            Tcl_ListObjAppendElement(Interp,obj,Tcl_NewDoubleObj(spd));
         }
      }
   }
   return(obj);
}

int Graph_UnProject(Tcl_Interp *Interp,GraphItem  *GR,TGraphItem *Item,double X,double Y,double Z,int Extrap) {

   Tcl_Obj    *obj;
   TGraphItem *item=NULL;
   int         i;

   X-=GR->xg[0];
   Y=GR->yg[0]-Y;

   if (X<0 || X>(GR->xg[1]-GR->xg[0]) || Y<0 || Y>(GR->yg[0]-GR->yg[1])) {
      if (!Extrap) {
         Tcl_AppendResult(Interp,"",(char*)NULL);
         return(TCL_OK);
      } else if (Extrap==-1) {
         X=X<0?0:X;
         X=X>(GR->xg[1]-GR->xg[0])?(GR->xg[1]-GR->xg[0]):X;
         Y=Y<0?0:Y;
         Y=Y>(GR->yg[0]-GR->yg[1])?(GR->yg[0]-GR->yg[1]):Y;
      }
   }

   if (Item) {
      Tcl_SetObjResult(Interp,Graph_UnProjectItem(Interp,Item,X,Y,Z));
   } else {
      obj=Tcl_NewListObj(0,NULL);
      for(i=0;i<GR->NItem;i++) {
         item=GraphItem_Get(GR->Item[i]);
         Tcl_ListObjAppendElement(Interp,obj,Graph_UnProjectItem(Interp,item,X,Y,Z));
      }
      Tcl_SetObjResult(Interp,obj);
   }

   return(TCL_OK);
}

/*----------------------------------------------------------------------------
 * Nom      : <GraphDisplay>
 * Creation : Mai 2005 - J.P. Gauthier - CMC/CMOE
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
static void GraphDisplay(Tk_Canvas Canvas,Tk_Item *Item,Display *Disp,Drawable Draw,int X,int Y,int Width,int Height){

   GraphItem     *gr=(GraphItem*)Item;
   TGraphItem    *item=NULL;
   TGraphAxis    *axisx,*axisy,*axisz;
   Tk_FontMetrics tkm;
   int            a,i,idx,width,height,x,y,mx,my;
   char          *data[8];

   if (!gr->Width || !gr->Height) {
      return;
   }
   Tk_GetFontMetrics(gr->Font,&tkm);

   if (gr->Alpha<100) {
      glEnable(GL_BLEND);
   }
   glShadeModel(GL_SMOOTH);

   if (GLRender->XExpose>0) {
      gr->Update=1;
   }

   /*Si refresh a partir du backbuffer*/
   if (!gr->Update && gr->Frame && !GLRender->TRCon) {
      trRasterPos2i(gr->header.x1-((TkCanvas*)Canvas)->xOrigin,-(gr->header.y2-((TkCanvas*)Canvas)->yOrigin));
      glDrawPixels(gr->Width,gr->Height,GL_RGBA,GL_UNSIGNED_BYTE,gr->Frame);
   } else {

      glTranslated(-((TkCanvas*)Canvas)->xOrigin,-((TkCanvas*)Canvas)->yOrigin,0.0);

      /*Clear du background*/
      if (gr->BGColor) {
         glLineWidth(1.0);
         glPolygonMode(GL_FRONT_AND_BACK,GL_FILL);
         glColor4us(gr->BGColor->red,gr->BGColor->green,gr->BGColor->blue,gr->Alpha*655);
         glBegin(GL_QUADS);
            glVertex2i(gr->header.x1,gr->header.y1);
            glVertex2i(gr->header.x1,gr->header.y2);
            glVertex2i(gr->header.x2,gr->header.y2);
            glVertex2i(gr->header.x2,gr->header.y1);
         glEnd();
      }

      /*Setup borders*/
      gr->xg[0]=gr->header.x1;gr->xg[1]=gr->header.x2;
      gr->yg[0]=gr->header.y2;gr->yg[1]=gr->header.y1;
      if (gr->Type[0]!='T') {
         gr->xg[0]+=5;gr->xg[1]-=5;
         gr->yg[0]-=5;gr->yg[1]+=5;
      }
      if (gr->TitleWidth) {
         gr->yg[1]+=gr->TitleHeight+5;
      }

      gr->NSide=gr->ISide=0;
      gr->CB->NbData=0;
      if (gr->CB->Data) free(gr->CB->Data);gr->CB->Data=NULL;

      if (gr->NItem) {

         /*Clear preprocess stage*/
         for(i=0;i<gr->NItem;i++) {
            item=GraphItem_Get(gr->Item[i]);

            /* Do we have a raster item */
            if (item->Data) {
               data[gr->CB->NbData++]=item->Data;
            }

            if (item->Type==WIDEBAR) gr->NSide++;
            if ((axisx=GraphAxis_Get(item->XAxis))) axisx->Done=NOTDONE;
            if ((axisy=GraphAxis_Get(item->YAxis))) axisy->Done=NOTDONE;
         }

         /*Process axis*/
         a=mx=my=0;
         if (gr->Type[0]!='T') {
            for(i=0;i<gr->NItem;i++) {
               item=GraphItem_Get(gr->Item[i]);

               axisx=GraphAxis_Get(item->XAxis);
               if (axisx && !(axisx->Done&DONEX)) {
                  idx=axisx->Pos[0]=='L'?0:1;
                  GraphAxis_Dim(Canvas,axisx,gr,HORIZONTAL,&x,&y);
                  gr->ah[a]=y;
                  gr->ay[a]=gr->yg[idx]+=idx?gr->ah[a]:-gr->ah[a];
                  mx=mx<x?x:mx;
                  a++;
                  axisx->Done|=DONEX;
               }

               axisy=GraphAxis_Get(item->YAxis);
               if (axisy && !(axisy->Done&DONEY)) {
                  idx=axisy->Pos[1]=='L'?0:1;
                  GraphAxis_Dim(Canvas,axisy,gr,VERTICAL,&x,&y);
                  gr->ah[a]=y;
                  gr->ax[a]=gr->xg[idx]+=idx?-gr->ah[a]:gr->ah[a];
                  my=my<x?x:my;
                  a++;
                  axisy->Done|=DONEY;
               }
            }
         }

         /* Check limits*/
         if (gr->xg[0]==gr->header.x1+5) {
            gr->xg[0]+=10;
         }
         if (gr->xg[1]==gr->header.x2-5) {
            gr->xg[1]-=10;
         }

         if (gr->yg[1]==gr->header.y1+5) {
            gr->yg[1]+=10;
         }

         if (gr->yg[0]==gr->header.y2-5) {
            gr->yg[0]-=10;
         }

         /* If we need a colormap */
         if (gr->CB->NbData) {
            gr->xg[1]     -= 90;
            gr->CB->x      = gr->xg[1]+5;
            gr->CB->y      = gr->yg[1];
            gr->CB->Width  = 90;
            gr->CB->Height = gr->yg[0]-gr->yg[1];
            gr->CB->anchor = TK_ANCHOR_NW;
            gr->CB->FGColor= gr->FGColor;
            gr->CB->BGColor= gr->BGColor;
            gr->CB->Font   = gr->Font;
            gr->CB->Alpha  = gr->Alpha;

            gr->CB->Data   = (char**)malloc(gr->CB->NbData*sizeof(char*));
            memcpy(gr->CB->Data,data,gr->CB->NbData*sizeof(char*));
         }

        /*Clear preprocess stage*/
         for(i=0;i<gr->NItem;i++) {
            item=GraphItem_Get(gr->Item[i]);
            if ((axisx=GraphAxis_Get(item->XAxis))) axisx->Done=NOTDONE;
            if ((axisy=GraphAxis_Get(item->YAxis))) axisy->Done=NOTDONE;
         }
      }

      /*Clear du background*/
      if (gr->FillColor) {
         glLineWidth(0.0);
         glPolygonMode(GL_FRONT,GL_FILL);
         glColor4us(gr->FillColor->red,gr->FillColor->green,gr->FillColor->blue,gr->Alpha*655);
         glBegin(GL_QUADS);
            glVertex2i(gr->xg[0],gr->yg[0]);
            glVertex2i(gr->xg[0],gr->yg[1]);
            glVertex2i(gr->xg[1],gr->yg[1]);
            glVertex2i(gr->xg[1],gr->yg[0]);
         glEnd();
      }

      /*Process items*/
      if (gr->xg[1]>gr->xg[0] && gr->yg[0]>gr->yg[1]) {
         a=0;
         for(i=0;i<gr->NItem;i++) {
            item=GraphItem_Get(gr->Item[i]);
            GraphSet(Canvas,gr,Width,Height);
            GraphItem_Display(NULL,gr,item,0,0,gr->xg[1]-gr->xg[0],gr->yg[0]-gr->yg[1],GL_RENDER);
            GraphUnSet(gr);
            if (item->Type==WIDEBAR) gr->ISide++;
         }

         /*if graph type is XY(Z)*/
         if (gr->Type[0]=='X') {
            for(i=0;i<gr->NItem;i++) {
               item=GraphItem_Get(gr->Item[i]);
               axisx=GraphAxis_Get(item->XAxis);
               if (axisx && !(axisx->Done&DONEX)) {
                  GraphAxis_Display(NULL,gr,axisx,gr->xg[0],gr->ay[a],gr->xg[1],gr->ay[a],gr->ah[a],HORIZONTAL);
                  axisx->Done|=DONEX;
                  a++;
               }

               axisy=GraphAxis_Get(item->YAxis);
               if (axisy && !(axisy->Done&DONEY)) {
                  GraphAxis_Display(NULL,gr,axisy,gr->ax[a],gr->yg[0],gr->ax[a],gr->yg[1],gr->ah[a],VERTICAL);
                  axisy->Done|=DONEY;
                  a++;
               }
            }
         }
      }
      glDisable(GL_CULL_FACE);

      /* Colorbar if needed */
      if (gr->CB->NbData) {
         ColorbarBBox(Canvas,gr->CB);
         ColorbarDisplay(Canvas,(Tk_Item*)gr->CB,Disp,Draw,X,Y,Width,Height);
      }

      /*Legende*/
      if (gr->Alpha<100 || gr->AlphaLegend<100) {
         glEnable(GL_BLEND);
      }

      if (gr->Legend) {
         width=0;
         height=0;
         for(i=0;i<gr->NItem;i++) {
            item=GraphItem_Get(gr->Item[i]);
            GraphItem_Dim(Canvas,item,gr,&x,&y);
            height+=y+5;
            width=width<x?x:width;
         }

         x=gr->xg[0]+gr->xi;
         y=gr->yg[1]+gr->yi;

         if (gr->BDLegend) {
            glPolygonMode(GL_FRONT,GL_FILL);
            glColor4us(gr->BGColor->red,gr->BGColor->green,gr->BGColor->blue,gr->AlphaLegend*gr->Alpha*0.01*655);
            glBegin(GL_QUADS);
               glVertex2i(x,y);
               glVertex2i(x,y+height+5);
               glVertex2i(x+width+55,y+height+5);
               glVertex2i(x+width+55,y);
            glEnd();

            glLineWidth(gr->BDLegend);
            glPolygonMode(GL_FRONT,GL_LINE);
            glColor4us(gr->FGColor->red,gr->FGColor->green,gr->FGColor->blue,gr->AlphaLegend*gr->Alpha*0.01*655);
            glBegin(GL_QUADS);
               glVertex2i(x,y);
               glVertex2i(x,y+height+5);
               glVertex2i(x+width+55,y+height+5);
               glVertex2i(x+width+55,y);
            glEnd();
         }

         x+=5;y+=5;
         for(i=0;i<gr->NItem;i++) {
            item=GraphItem_Get(gr->Item[i]);
            y=GraphItem_Header(NULL,gr,item,x,y,x+width+5);
         }
      }

      /*Title*/
      x=gr->header.x1+(gr->header.x2-gr->header.x1)*0.5;
      if (gr->TitleItem) {
         ((glTextItem*)gr->TitleItem)->x=x-gr->TitleWidth/2;
         ((glTextItem*)gr->TitleItem)->y=gr->header.y1+5;
         glComputeTextBbox(Canvas,gr->TitleItem);
      } else if (gr->FGColor && gr->Font && gr->Text) {
         glColor4us(gr->FGColor->red,gr->FGColor->green,gr->FGColor->blue,gr->Alpha*655);
         glFontUse(Tk_Display(Tk_CanvasTkwin(Canvas)),gr->Font);
         glDisplayTextLayout(gr->Text,0,x-gr->TitleWidth/2,gr->header.y1+5,0,-1);
      }

      glTranslated(((TkCanvas*)Canvas)->xOrigin,((TkCanvas*)Canvas)->yOrigin,0.0);
      glDisable(GL_BLEND);

      /*Sauvegarder dans le backbuffer*/
      if (!gr->Frame) {
         gr->Frame=(GLubyte*)malloc(gr->Width*gr->Height*4);
      }

      if (!GLRender->TRCon && GLRender->XExpose<=1) {
         glReadBuffer(GL_BACK);
         glReadPixels(gr->header.x1-((TkCanvas*)Canvas)->xOrigin,Height-gr->header.y2+((TkCanvas*)Canvas)->yOrigin,gr->Width,gr->Height,GL_RGBA,GL_UNSIGNED_BYTE,gr->Frame);
      }
   }

   /*Pourtour*/
   if (gr->FGColor && gr->BDWidth) {
      glTranslated(-((TkCanvas*)Canvas)->xOrigin,-((TkCanvas*)Canvas)->yOrigin,0.0);
      glLineWidth(gr->BDWidth);
      glPolygonMode(GL_FRONT,GL_LINE);
      glColor4us(gr->FGColor->red,gr->FGColor->green,gr->FGColor->blue,gr->Alpha*655);
      glBegin(GL_QUADS);
         glVertex2i(gr->header.x1,gr->header.y1);
         glVertex2i(gr->header.x1,gr->header.y2);
         glVertex2i(gr->header.x2,gr->header.y2);
         glVertex2i(gr->header.x2,gr->header.y1);
      glEnd();
      glTranslated(((TkCanvas*)Canvas)->xOrigin,((TkCanvas*)Canvas)->yOrigin,0.0);
   }
   gr->Update=0;
}

/*----------------------------------------------------------------------------
 * Nom      : <GraphSet>
 * Creation : Mai 2005 - J.P. Gauthier - CMC/CMOE
 *
 * But      : Initialiser le graph de projection .
 *
 * Parametres :
 *  <VP>      : Parametres du graph
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
void GraphSet(Tk_Canvas Canvas,GraphItem *GR,int Width,int Height){

   int w,h,x,y;

   if (GLRender->MagScale>1)
      Height=GLRender->MagY+GLRender->MagD/GLRender->MagScale;

   w=(GR->xg[1]-GR->xg[0]);
   h=(GR->yg[0]-GR->yg[1]);
   x=(GR->xg[0]-((TkCanvas*)Canvas)->xOrigin-GLRender->MagX)*GLRender->MagScale+GLRender->MagD/2;
   y=(Height-(GR->yg[0]-((TkCanvas*)Canvas)->yOrigin))*GLRender->MagScale-GLRender->MagD/2;

   glPushAttrib(GL_VIEWPORT_BIT);
   glMatrixMode(GL_PROJECTION);
   glPushMatrix();

   glLoadIdentity();
//   trViewport(GLRender->TRCon,x,y,w,h);
   trViewport(GLRender->TRCon,x,y,w*GLRender->MagScale,h*GLRender->MagScale);

   /*Ajuster la projection pour garder un aspect correct*/
   glOrtho(0,w,0,h,-1,1);

   glMatrixMode(GL_MODELVIEW);
   glPushMatrix();
   glLoadIdentity();
}

void GraphUnSet(GraphItem *GR){

   glMatrixMode(GL_MODELVIEW);
   glPopMatrix();
   glMatrixMode(GL_PROJECTION);
   glPopMatrix();
   glPopAttrib();
}

/*----------------------------------------------------------------------------
 * Nom      : <GraphToPoint>
 * Creation : Mai 2005 - J.P. Gauthier - CMC/CMOE
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
static double GraphToPoint(Tk_Canvas Canvas,Tk_Item *Item,double *CoordPtr){

   GraphItem *gr=(GraphItem*)Item;

   double xDiff,yDiff;

   /*Point is outside rectangle*/

   if (CoordPtr[0] < gr->header.x1) {
      xDiff = gr->header.x1 - CoordPtr[0];
   } else if (CoordPtr[0] > gr->header.x2) {
      xDiff = CoordPtr[0] - gr->header.x2;
   } else {
      xDiff = 0;
   }

   if (CoordPtr[1] < gr->header.y1) {
      yDiff = gr->header.y1 - CoordPtr[1];
   } else if (CoordPtr[1] > gr->header.y2) {
      yDiff = CoordPtr[1] - gr->header.y2;
   } else {
      yDiff = 0;
   }

   if (xDiff==0.0 && yDiff==0.0) {
      return 0.0;
   } else {
      return hypot(xDiff,yDiff);
   }
}

/*----------------------------------------------------------------------------
 * Nom      : <GraphToArea>
 * Creation : Mai 2005 - J.P. Gauthier - CMC/CMOE
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
static int GraphToArea(Tk_Canvas Canvas,Tk_Item *Item,double *RectPtr){

   GraphItem *gr=(GraphItem*)Item;

   if ((RectPtr[2] <= gr->header.x1) ||
       (RectPtr[0] >= gr->header.x2) ||
       (RectPtr[3] <= gr->header.y1) ||
       (RectPtr[1] >= gr->header.y2)) {
      return -1;
   }
   if ((RectPtr[0] <= gr->header.x1) &&
       (RectPtr[1] <= gr->header.y1) &&
       (RectPtr[2] >= gr->header.x2) &&
       (RectPtr[3] >= gr->header.y2)) {
      return 1;
   }
   return 0;
}

/*----------------------------------------------------------------------------
 * Nom      : <GraphScale>
 * Creation : Mai 2005 - J.P. Gauthier - CMC/CMOE
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
static void GraphScale(Tk_Canvas Canvas,Tk_Item *Item,double OriginX,double OriginY,double ScaleX,double ScaleY){

   GraphItem *gr=(GraphItem*)Item;

   gr->x = OriginX + ScaleX*(gr->x - OriginX);
   gr->y = OriginY + ScaleY*(gr->y - OriginY);
   GraphBBox(Canvas,gr);
}

/*----------------------------------------------------------------------------
 * Nom      : <GraphTranslate>
 * Creation : Mai 2005 - J.P. Gauthier - CMC/CMOE
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
static void GraphTranslate(Tk_Canvas Canvas,Tk_Item *Item,double DeltaX,double DeltaY){

   GraphItem *gr=(GraphItem*)Item;

   gr->x += DeltaX;
   gr->y += DeltaY;
   GraphBBox(Canvas,gr);
}

/*----------------------------------------------------------------------------
 * Nom      : <GraphToPostscript
 * Creation : Mai 2005 - J.P. Gauthier - CMC/CMOE
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
static int GraphToPostscript(Tcl_Interp *Interp,Tk_Canvas Canvas,Tk_Item *Item,int Prepass){

   GraphItem     *gr=(GraphItem*)Item;
   TGraphItem    *item=NULL;
   TGraphAxis    *axisx,*axisy;
   int            a,i,x,y,width,height;

   double coords[8];
   char   buf[128];

   if (Tk_CanvasPsFont(Interp,Canvas,gr->Font)!=TCL_OK) {
      return TCL_ERROR;
   }

   if (Prepass) {
      return TCL_OK;
   }

   gr->ISide=0;

   /*Coordonnee du viewport*/
   SETRECT(coords,gr->header.x1,gr->header.y1,gr->header.x2,gr->header.y2);

   /*Creer le background*/
   if (gr->BGColor) {
      Tk_CanvasPsColor(Interp,Canvas,gr->BGColor);
      Tk_CanvasPsPath(Interp,Canvas,coords,4);
      Tcl_AppendResult(Interp,"closepath fill\n",(char*)NULL);
   }

   /*Creer le clipping box*/
   Tk_CanvasPsPath(Interp,Canvas,coords,4);
   Tcl_AppendResult(Interp,"closepath clip newpath\n",(char*)NULL);

//   sprintf(buf,"gsave\n%i %i translate\n",gr->header.x1,(int)Tk_CanvasPsY(Canvas,gr->header.y2));
//   Tcl_AppendResult(Interp,buf,(char*)NULL);

   /*Clear du background*/
   if (gr->FillColor) {
      SETRECT(coords,gr->xg[0],gr->yg[0],gr->xg[1],gr->yg[1]);

      Tk_CanvasPsColor(Interp,Canvas,gr->FillColor);
      Tk_CanvasPsPath(Interp,Canvas,coords,4);
      Tcl_AppendResult(Interp,"closepath fill\n",(char*)NULL);
   }

   /*Process axis*/
   a=0;
   for(i=0;i<gr->NItem;i++) {
      item=GraphItem_Get(gr->Item[i]);

      axisx=GraphAxis_Get(item->XAxis);
      if (axisx) {
         a++;
         axisx->Done=NOTDONE;
      }
      axisy=GraphAxis_Get(item->YAxis);
      if (axisy) {
         a++;
         axisy->Done=NOTDONE;
      }
   }

   /*Process items*/
   if (gr->xg[1]>gr->xg[0] && gr->yg[0]>gr->yg[1]) {
      a=0;
      for(i=0;i<gr->NItem;i++) {
         item=GraphItem_Get(gr->Item[i]);
         Tcl_AppendResult(Interp,"gsave\n",(char*)NULL);
         Tk_CanvasPsPath(Interp,Canvas,coords,4);
         Tcl_AppendResult(Interp,"closepath clip newpath\n",(char*)NULL);
         sprintf(buf,"%i %.15g translate\n",gr->xg[0],Tk_CanvasPsY(Canvas,gr->yg[0]));
         Tcl_AppendResult(Interp,buf,(char*)NULL);
         GraphItem_Postscript(Interp,gr,item,0,0,gr->xg[1]-gr->xg[0],gr->yg[0]-gr->yg[1]);
         Tcl_AppendResult(Interp,"grestore\n",(char*)NULL);
         if (item->Type==WIDEBAR) gr->ISide++;
      }

      for(i=0;i<gr->NItem;i++) {
         item=GraphItem_Get(gr->Item[i]);
         axisx=GraphAxis_Get(item->XAxis);
         if (axisx && !(axisx->Done&DONEX)) {
            GraphAxis_Postscript(Interp,gr,axisx,gr->xg[0],gr->ay[a],gr->xg[1],gr->ay[a],gr->ah[a],HORIZONTAL);
            axisx->Done|=DONEX;
            a++;
         }

         axisy=GraphAxis_Get(item->YAxis);
         if (axisy && !(axisy->Done&DONEY)) {
            GraphAxis_Postscript(Interp,gr,axisy,gr->ax[a],gr->yg[0],gr->ax[a],gr->yg[1],gr->ah[a],VERTICAL);
            axisy->Done|=DONEY;
            a++;
         }
      }
   }

   /* Colorbar if needed */
   if (gr->CB->NbData) {
      ColorbarToPostscript(Interp,Canvas,(Tk_Item*)gr->CB,0);
   }

   /*Legende*/
   if (gr->Legend) {
      width=0;
      height=0;
     for(i=0;i<gr->NItem;i++) {
         item=GraphItem_Get(gr->Item[i]);
         GraphItem_Dim(Canvas,item,gr,&x,&y);
         height+=y+5;
         width=width<x?x:width;
      }

      x=gr->xg[0]+gr->xi;
      y=gr->yg[1]+gr->yi;

      if (gr->BDLegend) {
         SETRECT(coords,x,y,x+width+35,y+height+5);
         sprintf(buf,"%i setlinewidth 1 setlinecap 1 setlinejoin\n",gr->BDLegend);
         Tcl_AppendResult(Interp,buf,(char*)NULL);
         Tk_CanvasPsColor(Interp,Canvas,gr->BGColor);
         Tk_CanvasPsPath(Interp,Canvas,coords,4);
         Tcl_AppendResult(Interp,"closepath fill\n",(char*)NULL);

         Tk_CanvasPsColor(Interp,Canvas,gr->FGColor);
         Tk_CanvasPsPath(Interp,Canvas,coords,4);
         Tcl_AppendResult(Interp,"closepath stroke\n",(char*)NULL);
      }

      x+=5;y+=5;
      for(i=0;i<gr->NItem;i++) {
         item=GraphItem_Get(gr->Item[i]);
         y=GraphItem_HeaderPostscript(Interp,gr,item,x,y,x+width+5);
      }
   }

   /*Creer le pourtour*/
   if (gr->FGColor && gr->BDWidth) {
      SETRECT(coords,gr->header.x1,gr->header.y1,gr->header.x2,gr->header.y2);

      sprintf(buf,"%i setlinewidth 1 setlinecap 1 setlinejoin\n",gr->BDWidth);
      Tcl_AppendResult(Interp,buf,(char*)NULL);
      Tk_CanvasPsColor(Interp,Canvas,gr->FGColor);
      Tk_CanvasPsPath(Interp,Canvas,coords,4);
      Tcl_AppendResult(Interp,"closepath stroke\n",(char*)NULL);
   }

   /*Title*/
   x=gr->header.x1+(gr->header.x2-gr->header.x1)*0.5-gr->TitleWidth/2;
   if (gr->TitleItem) {
   } else {
       glPostscripTextLayout(Interp,Canvas,gr->Text,gr->FGColor,NULL,0,x,gr->header.y1+5,TK_ANCHOR_NW,TK_JUSTIFY_CENTER);
   }
   SetglCanvas(Canvas);
   return TCL_OK;
}

/*----------------------------------------------------------------------------
 * Nom      : <Graph_ItemParseProc>
 * Creation : Mai 2005 - J.P. Gauthier - CMC/CMOE
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
static int Graph_ItemParseProc(ClientData Data,Tcl_Interp *Interp,Tk_Window TkWin,char *Value,char *WidgRec,int Offset){

   GraphItem *gr=(GraphItem*)WidgRec;
   int i;

   if (gr->Item) {
      free(gr->ItemStr);
      Tcl_Free((char*)gr->Item);
      gr->Item=NULL;
      gr->NItem=0;
   }

   gr->ItemStr=strdup(Value);
   Tcl_SplitList(Interp,Value,&gr->NItem,&gr->Item);
   for(i=0;i<gr->NItem;i++) {
      if (!GraphItem_Get(gr->Item[i])) {
         Tcl_AppendResult(Interp,"Graph_ItemParse: Invalid Item \"",gr->Item[i],"\"",(char*)NULL);
         return(TCL_ERROR);
      }
   }

   return(TCL_OK);
}

/*----------------------------------------------------------------------------
 * Nom      : <Graph_ItemPrintProc>
 * Creation : Mai 2005 - J.P. Gauthier - CMC/CMOE
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
static char *Graph_ItemPrintProc(ClientData Data,Tk_Window TkWin,char *WidgRec,int Offset,Tcl_FreeProc **FreeProcPtr){

   GraphItem *gr=(GraphItem*)WidgRec;

   return(gr->ItemStr);
}
