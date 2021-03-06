/*=========================================================
 * Environnement Canada
 * Centre Meteorologique Canadien
 * 2100 Trans-Canadienne
 * Dorval, Quebec
 *
 * Projet       : Creation de graph dans le canvas Tk.
 * Fichier      : tkGraphItem.c
 * Creation     : Mai 2005 - J.P. Gauthier - CMC/CMOE
 *
 * Description  : Fichier d'entete du module d'Item.
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
 *
 *=========================================================
 */

#include "App.h"
#include "tkGraphItem.h"
#include "tkglCanvText.h"

static Tcl_HashTable GraphItemTable;
static CONST char *GraphTypeName[] = { "NONE","LINE","SPLINE","BAR","WIDEBAR","HISTOGRAM","RASTER","BOXPLOT","MINMAX" };
static CONST char *GraphDataName[] = { "False","True","data","xdata","ydata","zdata","speed","dir","error","high","low","median","min","max","pressure","drybulb","wetbulb","dewpoint",
   "SPREAD","HEIGHT","RELATIVEHUMIDITY" };

extern CONST  char *ICONS[];
extern TIcon  IconList[];
extern void   Data_RenderBarbule(int Type,int Flip,float Axis,float Lat,float Lon,float Elev,float Speed,float Dir,float Size,void *Proj);
extern int    FFStreamLine(TGeoRef *Ref,TZRef *ZRef,TDef *Def,void *VP,Vect3d *Stream,float *Map,double X,double Y,double Z,int MaxIter,double Step,double Min,double Res,int Mode,int ZDim);
extern int    FFContour(int Mode,TGeoPos *GPos,TDef *Def,TDataStat *Stat,void *Proj,int NbInter,double *Inter,int Depth,int Limit);
extern float *FFStreamMapSetup1D(double Delta);

extern void GraphTehpi_DisplayWetAdiabats(GraphItem *Graph,TGraphAxis *AxisTH,TGraphAxis *AxisT,TGraphAxis *AxisP,int X0,int Y0,int X1,int Y1,GLuint GLMode);
extern void GraphTehpi_DisplayDryAdiabats(GraphItem *Graph,TGraphAxis *AxisTH,TGraphAxis *AxisT,TGraphAxis *AxisP,int X0,int Y0,int X1,int Y1,GLuint GLMode);
extern void GraphTehpi_DisplayMixRatios(GraphItem *Graph,TGraphAxis *AxisTH,TGraphAxis *AxisT,TGraphAxis *AxisP,TGraphAxis *AxisW,int X0,int Y0,int X1,int Y1,GLuint GLMode);
extern void GraphTehpi_DisplayIsotherms(GraphItem *Graph,TGraphAxis *AxisTH,TGraphAxis *AxisT,TGraphAxis *AxisP,int X0,int Y0,int X1,int Y1,GLuint GLMode);
extern void GraphTehpi_DisplayIsobars(GraphItem *Graph,TGraphAxis *AxisTH,TGraphAxis *AxisT,TGraphAxis *AxisP,int X0,int Y0,int X1,int Y1,GLuint GLMode);
extern void GraphItem_DisplayTephi(Tcl_Interp *Interp,GraphItem *Graph,TGraphItem *Item,TGraphAxis *AxisT,TGraphAxis *AxisP,TGraphAxis *AxisTH,int X0,int Y0,int X1,int Y1,GLuint GLMode);

static int GraphItem_Cmd(ClientData clientData,Tcl_Interp *Interp,int Objc,Tcl_Obj *CONST Objv[]);
static int GraphItem_Config(Tcl_Interp *Interp,char *Name,int Objc,Tcl_Obj *CONST Objv[]);
static int GraphItem_Create(Tcl_Interp *Interp,char *Name);
static int GraphItem_Free(Tcl_Interp *Interp,char *Name);

void   GraphItem_Clear(TGraphItem *Item);
int    GraphItem_Header(Tcl_Interp *Interp,GraphItem *Graph,TGraphItem *Item,int X0,int Y0,int X1);
int    GraphItem_FitLinear(Vect3d *V,TVector *VX,TVector *VY,TGraphAxis *AxisX,TGraphAxis *AxisY,TGraphAxis *AxisZ,int X0,int Y0,int X1,int Y1);
void   GraphItem_DisplayXYZ(Tcl_Interp *Interp,GraphItem *Graph,TGraphItem *Item,TGraphAxis *AxisX,TGraphAxis *AxisY,TGraphAxis *AxisZ,int X0,int Y0,int X1,int Y1,GLuint GLMode);
void   GraphItem_DisplayBox(Tcl_Interp *Interp,GraphItem *Graph,TGraphItem *Item,TGraphAxis *AxisX,TGraphAxis *AxisY,TGraphAxis *AxisZ,int X0,int Y0,int X1,int Y1,GLuint GLMode);
void   GraphItem_DisplayMinMax(Tcl_Interp *Interp,GraphItem *Graph,TGraphItem *Item,TGraphAxis *AxisX,TGraphAxis *AxisY,TGraphAxis *AxisZ,int X0,int Y0,int X1,int Y1,GLuint GLMode);
void   GraphItem_Display2DTexture(Tcl_Interp *Interp,GraphItem *Graph,TGraphAxis *AxisX,TGraphAxis *AxisY,TGraphAxis *AxisZ,TData *Data,int X0,int Y0,int X1,int Y1);
void   GraphItem_Display2DTextureShader(Tcl_Interp *Interp,GraphItem *Graph,TGraphAxis *AxisX,TGraphAxis *AxisY,TGraphAxis *AxisZ,TData *Data,int X0,int Y0,int X1,int Y1);
void   GraphItem_Display2DContour(Tcl_Interp *Interp,GraphItem *Graph,TGraphAxis *AxisX,TGraphAxis *AxisY,TGraphAxis *AxisZ,TData *Data,int X0,int Y0,int X1,int Y1);
void   GraphItem_Display2DGrid(Tcl_Interp *Interp,GraphItem *Graph,TGraphAxis *AxisX,TGraphAxis *AxisY,TGraphAxis *AxisZ,TData *Data,int X0,int Y0,int X1,int Y1);
void   GraphItem_Display2DStream(Tcl_Interp *Interp,GraphItem *Graph,TGraphAxis *AxisX,TGraphAxis *AxisY,TGraphAxis *AxisZ,TData *Data,int X0,int Y0,int X1,int Y1);
void   GraphItem_Display2DLabel(Tcl_Interp *Interp,GraphItem *Graph,TGraphAxis *AxisX,TGraphAxis *AxisY,TGraphAxis *AxisZ,TData *Data,int X0,int Y0,int X1,int Y1);
void   GraphItem_Display2DVector(Tcl_Interp *Interp,GraphItem *Graph,TGraphAxis *AxisX,TGraphAxis *AxisY,TGraphAxis *AxisZ,TData *Data,int X0,int Y0,int X1,int Y1);
void   GraphItem_Wipe();
void   GraphItem_Postscript(Tcl_Interp *Interp,GraphItem *Graph,TGraphItem *Item,int X0,int Y0,int X1,int Y1);
void   GraphItem_PostscriptXYZ(Tcl_Interp *Interp,GraphItem *Graph,TGraphItem *Item,TGraphAxis *AxisX,TGraphAxis *AxisY,TGraphAxis *AxisZ,int X0,int Y0,int X1,int Y1);
void   GraphItem_PostscriptMinMax(Tcl_Interp *Interp,GraphItem *Graph,TGraphItem *Item,TGraphAxis *AxisX,TGraphAxis *AxisY,TGraphAxis *AxisZ,int X0,int Y0,int X1,int Y1);
void   GraphItem_PostscriptBox(Tcl_Interp *Interp,GraphItem *Graph,TGraphItem *Item,TGraphAxis *AxisX,TGraphAxis *AxisY,TGraphAxis *AxisZ,int X0,int Y0,int X1,int Y1);
int    GraphItem_HeaderPostscript(Tcl_Interp *Interp,GraphItem *Graph,TGraphItem *Item,int X0,int Y0,int X1);

/*--------------------------------------------------------------------------------------------------------------
 * Nom          : <GraphItem_ColorXYZ>
 * Creation     : Janvier 2007 - J.P. Gauthier - CMC/CMOE
 *
 * But          : Activer une couleur indexee
 *
 * Parametres   :
 *   <Interp>   : Interpreteur Tcl
 *   <Graph>    : Item graph
 *   <Item>     : Item de graph
 *   <N>        : Index de la couleur a utiliser *
 *
 * Retour       :
 *
 * Remarques :
 *
 *---------------------------------------------------------------------------------------------------------------
*/
static inline void GraphItem_ColorXYZ(Tcl_Interp *Interp,GraphItem *Graph,TGraphItem *Item,int N) {

   XColor *color;
   Tcl_Obj *col;

   if (Item->Colors) {
      Tcl_ListObjIndex(Interp,Item->Colors,N,&col);
      if (col) {
         color=Tk_AllocColorFromObj(Item->Interp,Tk_CanvasTkwin(Graph->canvas),col);
         if (Interp) {
            Tk_CanvasPsColor(Interp,Graph->canvas,color);
         } else {
            glColor4us(color->red,color->green,color->blue,Item->Alpha*Graph->Alpha*0.01*655);
         }
      }
   }
}

/*--------------------------------------------------------------------------------------------------------------
 * Nom          : <GraphItem_VectorPlace>
 * Creation     : Mai 2005 - J.P. Gauthier - CMC/CMOE
 *
 * But          : Reprojeter une coordonnee points de grille dans le referentiel du graph et selon la position des niveaux
 *
 * Parametres   :
 *   <Data>     : Champs
 *   <AxisX>    : Axe en X
 *   <AxisY>    : Axe en Y
 *   <AxisZ>    : Axe en Z
 *   <X0>       : Limite inferieure gauche du graph
 *   <Y0>       : Limite inferieure gauche du graph
 *   <VIn>      : Vecteur en entree
 *   <VOut>     : Vecteur reprojete
 *
 * Retour       :
 *
 * Remarques :
 *
 *---------------------------------------------------------------------------------------------------------------
*/
static inline void GraphItem_VectorPlace(TData *Data,TGraphAxis *AxisX,TGraphAxis *AxisY,TGraphAxis *AxisZ,int X0,int Y0,Vect3d VIn,Vect3d VOut){

   int    d[2];
   double y,v[2],h[2];

   d[0]=floor(VIn[0]);
   d[1]=floor(VIn[1]);

   if (Data->GRef->Grid[0]=='V') {
      if (Data->Spec->ZType!=LVL_UNDEF && Data->GRef->Hgt) {
         h[0]=Data->GRef->Hgt[d[1]*Data->Def->NI+d[0]];
         h[1]=Data->GRef->Hgt[d[1]*Data->Def->NI+(d[0]+1)];
         v[0]=ILIN(h[0],h[1],VIn[0]-d[0]);

         h[0]=Data->GRef->Hgt[(d[1]+1)*Data->Def->NI+d[0]];
         h[1]=Data->GRef->Hgt[(d[1]+1)*Data->Def->NI+(d[0]+1)];
         v[1]=ILIN(h[0],h[1],VIn[0]-d[0]);
      } else {
         v[0]=Data->ZRef->Levels[d[1]];
         v[1]=Data->ZRef->Levels[d[1]+1];
      }
   } else {
      v[0]=d[1];
      v[1]=d[1]+1;
   }

   y=ILIN(v[0],v[1],VIn[1]-d[1]);

   VOut[0]=X0+AXISVALUE(AxisX,VIn[0]);
   VOut[1]=Y0+AXISVALUE(AxisY,y);
   VOut[2]=0.0;
}
 
/*--------------------------------------------------------------------------------------------------------------
 * Nom          : <TclGraphItem_Init>
 * Creation     : Mai 2005 - J.P. Gauthier - CMC/CMOE
 *
 * But          : Initialisation des commandes Tcl pour utilisation des items de graphs
 *
 * Parametres   :
 *   <Interp>   : Interpreteur Tcl
 *
 * Retour       : Code de retour Tcl
 *
 * Remarques    :
 *
 *---------------------------------------------------------------------------------------------------------------
 */
int TclGraphItem_Init(Tcl_Interp *Interp) {

   Tcl_InitHashTable(&GraphItemTable,TCL_STRING_KEYS);
   Tcl_CreateObjCommand(Interp,"graphitem",GraphItem_Cmd,(ClientData)NULL,(Tcl_CmdDeleteProc*)NULL);

   return TCL_OK;
}

/*--------------------------------------------------------------------------------------------------------------
 * Nom          : <GraphItem_Cmd>
 * Creation     : Mai 2005 - J.P. Gauthier - CMC/CMOE
 *
 * But          : Effectuer les commandes du package
 *
 * Parametres    :
 *   <clientData>: Nom du vecteur
 *   <Interp>    : Interpreteur Tcl
 *   <Objc>      : Nombre d'arguments
 *   <Objv>      : Pointeur sur la liste des arguments (word)
 *
 * Retour        : Code de retour standard TCL
 *
 * Remarques     :
 *
 *---------------------------------------------------------------------------------------------------------------
*/

static int GraphItem_Cmd(ClientData clientData,Tcl_Interp *Interp,int Objc,Tcl_Obj *CONST Objv[]) {

   int         idx,n,t;
   static CONST char *sopt[] = { "create","free","configure","is","wipe",NULL };
   enum               opt { CREATE,FREE,CONFIGURE,IS,WIPE };

   Tcl_ResetResult(Interp);

   if (Objc<2) {
      Tcl_WrongNumArgs(Interp,1,Objv,"command ?arg arg ...?");
      return TCL_ERROR;
   }

   if (Tcl_GetIndexFromObj(Interp,Objv[1],sopt,"command",TCL_EXACT,&idx)!=TCL_OK) {
      return TCL_ERROR;
   }

   switch ((enum opt)idx) {
      case CREATE:
         if(Objc<3) {
            Tcl_WrongNumArgs(Interp,2,Objv,"item");
            return(TCL_ERROR);
         }
         t=GraphItem_Create(Interp,Tcl_GetString(Objv[2]));
         if (t!=TCL_ERROR && Objc>3) {
            t=GraphItem_Config(Interp,Tcl_GetString(Objv[2]),Objc-3,Objv+3);
         }
         return(t);
         break;

      case FREE:
         if(Objc<3) {
            Tcl_WrongNumArgs(Interp,2,Objv,"item");
            return(TCL_ERROR);
         }
         for(n=2;n<Objc;n++) {
            GraphItem_Free(Interp,Tcl_GetString(Objv[n]));
         }
         return(TCL_OK);
         break;

      case CONFIGURE:
         if(Objc<3) {
            Tcl_WrongNumArgs(Interp,2,Objv,"item ?option?");
            return TCL_ERROR;
         }
         return GraphItem_Config(Interp,Tcl_GetString(Objv[2]),Objc-3,Objv+3);

      case IS:
         if(Objc!=3) {
            Tcl_WrongNumArgs(Interp,2,Objv,"item");
            return TCL_ERROR;
         }
         if (GraphItem_Get(Tcl_GetString(Objv[2]))) {
            Tcl_SetObjResult(Interp,Tcl_NewBooleanObj(1));
         } else {
            Tcl_SetObjResult(Interp,Tcl_NewBooleanObj(0));
         }
         break;

      case WIPE:
         GraphItem_Wipe();
   }
   return TCL_OK;
}

/*----------------------------------------------------------------------------
 * Nom      : <GraphItem_Config>
 * Creation : Mai 2005 - J.P. Gauthier - CMC/CMOE
 *
 * But      : Configuration des parametres de l'item de graph
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

static int GraphItem_Config(Tcl_Interp *Interp,char *Name,int Objc,Tcl_Obj *CONST Objv[]){

   TGraphItem *item;
   char        buf[256];
   int         i,j,idx;


   static CONST char *sopt[] = { "-anchor","-colors","-outline","-fill","-iconoutline","-iconfill","-iconxfillvalue","-iconxshowvalue","-font","-width","-size","-stipple","-bitmap","-image","-icon","-type","-orient","-data","-xdata","-ydata","-zdata","-speed","-dir","-windpres","-pressure","-drybulb","-wetbulb","-dewpoint","-error","-high","-low","-median","-min","-max","-waxis","-xaxis","-yaxis","-zaxis","-mixaxis","-taxis","-thaxis","-paxis","-desc","-tag","-transparency","-dash","-value","-fit","-avg","-origin",NULL };
   enum                opt { ANCHOR,COLORS,OUTLINE,FILL,ICONOUTLINE,ICONFILL,ICONXFILLVALUE,ICONXSHOWVALUE,FONT,WIDTH,SIZE,STIPPLE,BITMAP,IMAGE,ICON,TYPE,ORIENT,DATA,XDATA,YDATA,ZDATA,SPEED,DIR,WINDPRES,PRESSURE,DRYBULB,WETBULB,DEWPOINT,ERRORDATA,HIGHDATA,LOWDATA,MEDIANDATA,MINDATA,MAXDATA,WAXIS,XAXIS,YAXIS,ZAXIS,MIXAXIS,TAXIS,THAXIS,PAXIS,DESC,TAG,TRANSPARENCY,DASH,VALUE,FIT,AVG,ORIGIN };
   item=GraphItem_Get(Name);
   if (!item) {
      Tcl_AppendResult(Interp,"\n   GraphItem_Config: unknown object: \"",Name,"\"",(char*)NULL);
      return TCL_ERROR;
   }
   item->Interp=Interp;

   for(i=0;i<Objc;i++) {

      if (Tcl_GetIndexFromObj(Interp,Objv[i],sopt,"option",TCL_EXACT,&idx)!=TCL_OK) {
         return TCL_ERROR;
      }

      switch ((enum opt)idx) {
         case ANCHOR:
            if (Objc==1) {
               Tcl_SetObjResult(Interp,Tcl_NewStringObj(Tk_NameOfAnchor(item->Anchor),-1));
             } else {
               Tk_GetAnchorFromObj(Interp,Objv[++i],&item->Anchor);
            }
            break;

          case COLORS:
            if (Objc==1) {
               Tcl_SetObjResult(Interp,item->Colors);
            } else {
               if (item->Colors) {
                  Tcl_DecrRefCount(item->Colors);
                  item->Colors=NULL;
               }
               Tcl_ListObjLength(Interp,Objv[++i],&j);
               if (j) {
                  item->Colors=Objv[i];
                  Tcl_IncrRefCount(item->Colors);
               }
            }
            break;

         case OUTLINE:
            if (Objc==1) {
               if (item->Outline) {
                  Tcl_AppendResult(Interp,Tk_NameOfColor(item->Outline),(char*)NULL);
               } else {
                  Tcl_AppendResult(Interp,"",(char*)NULL);
               }
            } else {
               if (item->Outline) {
                  Tk_FreeColor(item->Outline);
                  item->Outline=NULL;
               }
               if (strlen(Tcl_GetString(Objv[++i])))
                  item->Outline=Tk_AllocColorFromObj(Interp,Tk_MainWindow(Interp),Objv[i]);
            }
            break;

         case FILL:
            if (Objc==1) {
               if (item->Fill) {
                  Tcl_AppendResult(Interp,Tk_NameOfColor(item->Fill),(char*)NULL);
               } else {
                  Tcl_AppendResult(Interp,"",(char*)NULL);
               }
            } else {
               if (item->Fill) {
                  Tk_FreeColor(item->Fill);
                  item->Fill=NULL;
               }
               if (strlen(Tcl_GetString(Objv[++i])))
                  item->Fill=Tk_AllocColorFromObj(Interp,Tk_MainWindow(Interp),Objv[i]);
            }
            break;

         case ICONOUTLINE:
            if (Objc==1) {
               if (item->IconOutline) {
                  Tcl_AppendResult(Interp,Tk_NameOfColor(item->IconOutline),(char*)NULL);
               } else {
                  Tcl_AppendResult(Interp,"",(char*)NULL);
               }
            } else {
               if (item->IconOutline) {
                  Tk_FreeColor(item->IconOutline);
                  item->IconOutline=NULL;
               }
               if (strlen(Tcl_GetString(Objv[++i])))
                  item->IconOutline=Tk_AllocColorFromObj(Interp,Tk_MainWindow(Interp),Objv[i]);
            }
            break;

         case ICONFILL:
            if (Objc==1) {
               if (item->IconFill) {
                  Tcl_AppendResult(Interp,Tk_NameOfColor(item->IconFill),(char*)NULL);
               } else {
                  Tcl_AppendResult(Interp,"",(char*)NULL);
               }
            } else {
               if (item->IconFill) {
                  Tk_FreeColor(item->IconFill);
                  item->IconFill=NULL;
               }
               if (strlen(Tcl_GetString(Objv[++i])))
                  item->IconFill=Tk_AllocColorFromObj(Interp,Tk_MainWindow(Interp),Objv[i]);
            }
            break;

         case ICONXFILLVALUE:
            if (Objc==1) {
               Tcl_SetObjResult(Interp,Tcl_NewDoubleObj(item->IconXFillValue));
            } else {
               Tcl_GetDoubleFromObj(Interp,Objv[++i],&item->IconXFillValue);
            }
            break;

         case ICONXSHOWVALUE:
            if (Objc==1) {
               Tcl_SetObjResult(Interp,Tcl_NewDoubleObj(item->IconXShowValue));
            } else {
               Tcl_GetDoubleFromObj(Interp,Objv[++i],&item->IconXShowValue);
            }
            break;

         case FONT:
            if (Objc==1) {
               Tcl_SetObjResult(Interp,Tcl_NewStringObj(Tk_NameOfFont(item->Font),-1));
            } else {
               if (item->Font) Tk_FreeFont(item->Font);
               item->Font=Tk_AllocFontFromObj(Interp,Tk_MainWindow(Interp),Objv[++i]);
            }
            break;

         case STIPPLE:
            if (Objc==1) {
               if (item->Stipple) {
                  Tcl_AppendResult(Interp,item->Stipple->Name,(char*)NULL);
               } else {
                  Tcl_AppendResult(Interp,"",(char*)NULL);
               }
            } else {
               glBitmapParseProc(NULL,Interp,Tk_MainWindow(Interp),Tcl_GetString(Objv[++i]),(char*)&item->Stipple,0);
            }
            break;

         case BITMAP:
            if (Objc==1) {
               if (item->Bitmap)
                  Tcl_SetObjResult(Interp,Tcl_NewStringObj(item->Bitmap->Name,-1));
               else
                  Tcl_SetObjResult(Interp,Tcl_NewStringObj("",-1));
            } else {
               glBitmapParseProc(NULL,Interp,Tk_MainWindow(Interp),Tcl_GetString(Objv[++i]),(char*)&item->Bitmap,0);
            }
            break;

         case IMAGE:
            if (Objc==1) {
               Tcl_SetObjResult(Interp,Tcl_NewStringObj(item->ImageString,-1));
            } else {
               if (item->ImageString) {
                  free(item->ImageString);
                  item->ImageString=NULL;
               }
               if (strlen(Tcl_GetString(Objv[++i])))
                  item->ImageString=strdup(Tcl_GetString(Objv[i]));
            }
            break;

         case ICON:
            if (Objc==1) {
               if (item->Icon) {
                  Tcl_SetObjResult(Interp,Tcl_NewStringObj(ICONS[item->Icon],-1));
               }
            } else {
               if (!strlen(Tcl_GetString(Objv[++i]))) {
                  item->Icon=0;
               } else if (Tcl_GetIndexFromObj(Interp,Objv[i],ICONS,"icon",TCL_EXACT,&item->Icon)!=TCL_OK) {
                  return(TCL_ERROR);
               }
            }
            break;

         case TRANSPARENCY:
            if (Objc==1) {
               Tcl_SetObjResult(Interp,Tcl_NewIntObj(item->Alpha));
            } else {
               Tcl_GetIntFromObj(Interp,Objv[++i],&item->Alpha);
              item->Alpha=item->Alpha<0?0:item->Alpha>100?100:item->Alpha;
            }
            break;

         case DASH:
            if (Objc==1) {
               DashPrint(buf,&item->Dash);
               Tcl_AppendResult(Interp,buf,(char*)NULL);
            } else {
               Tk_GetDash(Interp,Tcl_GetString(Objv[++i]),&item->Dash);
            }
            break;

         case WIDTH:
            if (Objc==1) {
               Tcl_SetObjResult(Interp,Tcl_NewIntObj(item->Width));
            } else {
               Tcl_GetIntFromObj(Interp,Objv[++i],&item->Width);
           }
            break;

         case SIZE:
            if (Objc==1) {
               Tcl_SetObjResult(Interp,Tcl_NewDoubleObj(item->Size));
            } else {
               Tcl_GetDoubleFromObj(Interp,Objv[++i],&item->Size);
            }
            break;

         case VALUE:
            if (Objc==1) {
               Tcl_SetObjResult(Interp,Tcl_NewStringObj(GraphDataName[item->Value],-1));
            } else {
               if (Tcl_GetBooleanFromObj(Interp,Objv[++i],&item->Value)==TCL_ERROR) {
                  if (Tcl_GetIndexFromObj(Interp,Objv[i],GraphDataName,"data",TCL_EXACT,&item->Value)!=TCL_OK) {
                     return(TCL_ERROR);
                  }
               }
            }
            break;

         case TYPE:
            if (Objc==1) {
               Tcl_SetObjResult(Interp,Tcl_NewStringObj(GraphTypeName[item->Type],-1));
            } else {
               if (Tcl_GetIndexFromObj(Interp,Objv[++i],GraphTypeName,"type",TCL_EXACT,&item->Type)!=TCL_OK) {
                   return(TCL_ERROR);
               }
            }
            break;

         case ORIENT:
            if (Objc==1) {
               Tcl_SetObjResult(Interp,Tcl_NewStringObj(item->Orient,-1));
             } else {
               if (item->Orient) free(item->Orient);
               item->Orient=strdup(Tcl_GetString(Objv[++i]));
            }
            break;

         case DESC:
            if (Objc==1) {
               Tcl_SetObjResult(Interp,Tcl_NewStringObj(item->Desc,-1));
            } else {
               if (item->Desc) free(item->Desc);
               item->Desc=strdup(Tcl_GetString(Objv[++i]));
            }
            break;

         case TAG:
            if (Objc==1) {
               Tcl_SetObjResult(Interp,Tcl_NewStringObj(item->Tag,-1));
            } else {
                if (item->Tag) free(item->Tag);
                item->Tag=strdup(Tcl_GetString(Objv[++i]));
            }
            break;

         case DATA:
            if (Objc==1) {
               Tcl_SetObjResult(Interp,Tcl_NewStringObj(item->Data,-1));
            } else {
               if (item->Data) free(item->Data);
               item->Data=NULL;
               if (strlen(Tcl_GetString(Objv[++i]))) {
                  item->Data=strdup(Tcl_GetString(Objv[i]));
               }
            }
            break;

         case DRYBULB:
         case XDATA:
            if (Objc==1) {
               Tcl_SetObjResult(Interp,Tcl_NewStringObj(item->XData,-1));
            } else {
               if (item->XData) free(item->XData);
               item->XData=NULL;
               if (strlen(Tcl_GetString(Objv[++i]))) {
                  item->XData=strdup(Tcl_GetString(Objv[i]));
               }
            }
            break;

         case PRESSURE:
         case YDATA:
            if (Objc==1) {
               Tcl_SetObjResult(Interp,Tcl_NewStringObj(item->YData,-1));
            } else {
               if (item->YData) free(item->YData);
               item->YData=NULL;
               if (strlen(Tcl_GetString(Objv[++i]))) {
                  item->YData=strdup(Tcl_GetString(Objv[i]));
               }
            }
            break;

         case WINDPRES:
         case ZDATA:
            if (Objc==1) {
               Tcl_SetObjResult(Interp,Tcl_NewStringObj(item->ZData,-1));
            } else {
               if (item->ZData) free(item->ZData);
               item->ZData=NULL;
               if (strlen(Tcl_GetString(Objv[++i]))) {
                  item->ZData=strdup(Tcl_GetString(Objv[i]));
               }
            }
            break;

         case SPEED:
            if (Objc==1) {
               Tcl_SetObjResult(Interp,Tcl_NewStringObj(item->Speed,-1));
            } else {
               if (item->Speed) free(item->Speed);
               item->Speed=NULL;
               if (strlen(Tcl_GetString(Objv[++i]))) {
                  item->Speed=strdup(Tcl_GetString(Objv[i]));
               }
            }
            break;

         case DIR:
            if (Objc==1) {
               Tcl_SetObjResult(Interp,Tcl_NewStringObj(item->Dir,-1));
            } else {
               if (item->Dir) free(item->Dir);
               item->Dir=NULL;
               if (strlen(Tcl_GetString(Objv[++i]))) {
                  item->Dir=strdup(Tcl_GetString(Objv[i]));
               }
            }
            break;

         case ERRORDATA:
            if (Objc==1) {
               Tcl_SetObjResult(Interp,Tcl_NewStringObj(item->ErrorData,-1));
            } else {
               if (item->ErrorData) free(item->ErrorData);
               item->ErrorData=NULL;
               if (strlen(Tcl_GetString(Objv[++i]))) {
                  item->ErrorData=strdup(Tcl_GetString(Objv[i]));
               }
            }
            break;

         case HIGHDATA:
            if (Objc==1) {
               Tcl_SetObjResult(Interp,Tcl_NewStringObj(item->HighData,-1));
            } else {
               if (item->HighData) free(item->HighData);
               item->HighData=NULL;
               if (strlen(Tcl_GetString(Objv[++i]))) {
                  item->HighData=strdup(Tcl_GetString(Objv[i]));
               }
            }
            break;

         case LOWDATA:
            if (Objc==1) {
               Tcl_SetObjResult(Interp,Tcl_NewStringObj(item->LowData,-1));
            } else {
               if (item->LowData) free(item->LowData);
               item->LowData=NULL;
               if (strlen(Tcl_GetString(Objv[++i]))) {
                  item->LowData=strdup(Tcl_GetString(Objv[i]));
               }
            }
            break;

         case MEDIANDATA:
            if (Objc==1) {
               Tcl_SetObjResult(Interp,Tcl_NewStringObj(item->MedianData,-1));
            } else {
               if (item->MedianData) free(item->MedianData);
               item->MedianData=NULL;
               if (strlen(Tcl_GetString(Objv[++i]))) {
                  item->MedianData=strdup(Tcl_GetString(Objv[i]));
               }
            }
            break;

         case WETBULB:
         case MINDATA:
            if (Objc==1) {
               Tcl_SetObjResult(Interp,Tcl_NewStringObj(item->MinData,-1));
            } else {
               if (item->MinData) free(item->MinData);
               item->MinData=NULL;
               if (strlen(Tcl_GetString(Objv[++i]))) {
                  item->MinData=strdup(Tcl_GetString(Objv[i]));
               }
            }
            break;

         case DEWPOINT:
         case MAXDATA:
            if (Objc==1) {
               Tcl_SetObjResult(Interp,Tcl_NewStringObj(item->MaxData,-1));
            } else {
               if (item->MaxData) free(item->MaxData);
               item->MaxData=NULL;
               if (strlen(Tcl_GetString(Objv[++i]))) {
                  item->MaxData=strdup(Tcl_GetString(Objv[i]));
               }
            }
            break;

         case TAXIS:
         case XAXIS:
            if (Objc==1) {
               Tcl_SetObjResult(Interp,Tcl_NewStringObj(item->XAxis,-1));
             } else {
               if (item->XAxis) free(item->XAxis);
               item->XAxis=NULL;
               if (strlen(Tcl_GetString(Objv[++i]))) {
                  item->XAxis=strdup(Tcl_GetString(Objv[i]));
               }
            }
            break;

         case PAXIS:
         case YAXIS:
            if (Objc==1) {
               Tcl_SetObjResult(Interp,Tcl_NewStringObj(item->YAxis,-1));
            } else {
               if (item->YAxis) free(item->YAxis);
               item->YAxis=NULL;
               if (strlen(Tcl_GetString(Objv[++i]))) {
                  item->YAxis=strdup(Tcl_GetString(Objv[i]));
               }
            }
            break;

         case THAXIS:
         case ZAXIS:
            if (Objc==1) {
               Tcl_SetObjResult(Interp,Tcl_NewStringObj(item->ZAxis,-1));
            } else {
               if (item->ZAxis) free(item->ZAxis);
               item->ZAxis=NULL;
               if (strlen(Tcl_GetString(Objv[++i]))) {
                  item->ZAxis=strdup(Tcl_GetString(Objv[i]));
               }
            }
            break;

         case MIXAXIS:
         case WAXIS:
            if (Objc==1) {
               Tcl_SetObjResult(Interp,Tcl_NewStringObj(item->WAxis,-1));
            } else {
               if (item->WAxis) free(item->WAxis);
               item->WAxis=NULL;
               if (strlen(Tcl_GetString(Objv[++i]))) {
                  item->WAxis=strdup(Tcl_GetString(Objv[i]));
               }
            }
            break;

         case FIT:
            if (Objc==1) {
               Tcl_SetObjResult(Interp,Tcl_NewStringObj(item->Fit,-1));
            } else {
               if (item->Fit) free(item->Fit);
               item->Fit=strdup(Tcl_GetString(Objv[++i]));
            }
            break;

         case AVG:
            if (Objc==1) {
               Tcl_SetObjResult(Interp,Tcl_NewBooleanObj(item->Avg));
            } else {
               Tcl_GetBooleanFromObj(Interp,Objv[++i],&item->Avg);
            }
            break;
            
         case ORIGIN:
            if (Objc==1) {
               if (item->Origin!=HUGE_VAL) {
                  Tcl_SetObjResult(Interp,Tcl_NewDoubleObj(item->Origin));
                }
            } else {
               if (Tcl_GetDoubleFromObj(Interp,Objv[++i],&item->Origin)==TCL_ERROR) {
                  item->Origin=HUGE_VAL;
               }
            }
            break;
      }
   }
   return TCL_OK;
}

/*--------------------------------------------------------------------------------------------------------------
 * Nom       : <GraphItem_Create>
 * Creation  : Mai 2005 - J.P. Gauthier - CMC/CMOE
 *
 * But          : Creation d'un objet graphitem et insertion d'un nouveau nom dans la table.
 *
 * Parametres   :
 *   <Interp>   : Interpreteur Tcl
 *   <Name>     : Nom de l'observation a creer
 *
 * Retour       : Code de retour standard TCL
 *
 * Remarques    :
 *
 *---------------------------------------------------------------------------------------------------------------
*/

static int GraphItem_Create(Tcl_Interp *Interp,char *Name) {

   TGraphItem *item;

   if (!(item=(TGraphItem*)TclY_HashPut(Interp,&GraphItemTable,Name,sizeof(TGraphItem)))) {
      return(TCL_ERROR);
   }

   item->Font=NULL;
   item->Outline=NULL;
   item->Fill=NULL;
   item->Stipple=NULL;
   item->Bitmap=NULL;
   item->ImageString=NULL;
   item->Icon=0;
   item->IconOutline=NULL;
   item->IconFill=NULL;
   item->IconXFillValue=1e32;
   item->IconXShowValue=1e32;
   item->Dash.number=0;
   item->Colors=NULL;

   item->Anchor=TK_ANCHOR_CENTER;
   item->Type=NONE;
   item->Origin=HUGE_VAL;
   item->Alpha=100;
   item->Width=0;
   item->DescWidth=0;
   item->DescHeight=0;
   item->Size=1.0;
   item->Value=0;
   item->Orient=strdup("X");
   item->Fit=NULL;
   item->Avg=0;
   item->Desc=NULL;
   item->DescItem=NULL;
   item->Text=NULL;
   item->Tag=NULL;
   item->Data=NULL;
   item->XData=NULL;
   item->YData=NULL;
   item->ZData=NULL;
   item->WAxis=NULL;
   item->XAxis=NULL;
   item->YAxis=NULL;
   item->ZAxis=NULL;
   item->Speed=NULL;
   item->Dir=NULL;
   item->ErrorData=NULL;
   item->MedianData=NULL;
   item->HighData=NULL;
   item->LowData=NULL;
   item->MinData=NULL;
   item->MaxData=NULL;

   return(TCL_OK);
}

/*--------------------------------------------------------------------------------------------------------------
 * Nom          : <GraphItem_Get>
 * Creation     : Mai 2005 - J.P. Gauthier - CMC/CMOE
 *
 * But          : Obtenir un objet graphitem en fonction de son nom dans la table de Tcl
 *
 * Parametres   :
 *   <Name>     : Nom de l'objet a obtenir.
 *
 * Retour       : Une structure TOGraphAxis ou un pointeur NULL si rien trouve.
 *
 * Remarques    :
 *
 *---------------------------------------------------------------------------------------------------------------
*/
TGraphItem* GraphItem_Get(char *Name) {
   return((TGraphItem*)TclY_HashGet(&GraphItemTable,Name));
}

/*--------------------------------------------------------------------------------------------------------------
 * Nom          : <GraphItem_Free>
 * Creation     : Mai 2005 - J.P. Gauthier - CMC/CMOE
 *
 * But          : Destruction d'un item de graph a partir de son nom.
 *
 * Parametres   :
 *   <Interp>   : Interpreteur Tcl
 *   <Name>     : Nom de l'observation a detruire
 *
 * Retour       : Code de retour standard TCL
 *
 * Remarques :
 *
 *---------------------------------------------------------------------------------------------------------------
*/
static int GraphItem_Free(Tcl_Interp *Interp,char *Name) {

   TGraphItem *item;

   if ((item=(TGraphItem*)TclY_HashDel(&GraphItemTable,Name))) {
      GraphItem_Clear(item);
      free(item);
   }
   return(TCL_OK);
}

/*--------------------------------------------------------------------------------------------------------------
 * Nom          : <GraphItem_Clear>
 * Creation     : Mai 2005 - J.P. Gauthier - CMC/CMOE
 *
 * But          : Suppression des valeur et de la memoire alloue de l'item de graph.
 *
 * Parametres   :
 *   <Item>     : Item de graph
 *
 * Retour       :
 *
 * Remarques :
 *
 *---------------------------------------------------------------------------------------------------------------
*/
void GraphItem_Clear(TGraphItem *Item) {

   if (Item->Outline)     Tk_FreeColor(Item->Outline);
   if (Item->Fill)        Tk_FreeColor(Item->Fill);
   if (Item->IconOutline) Tk_FreeColor(Item->IconOutline);
   if (Item->IconFill)    Tk_FreeColor(Item->IconFill);
   if (Item->Font)        Tk_FreeFont(Item->Font);
   if (Item->Text)        Tk_FreeTextLayout(Item->Text);
   if (Item->Colors)      Tcl_DecrRefCount(Item->Colors);

   // We keep bitmap cached so don't free them
//   if (Item->Stipple) glBitmapFree(Item->Stipple);
//   if (Item->Bitmap)  glBitmapFree(Item->Bitmap);

   if (Item->ImageString) free(Item->ImageString);
   if (Item->Orient)      free(Item->Orient);
   if (Item->Fit)         free(Item->Fit);
   if (Item->Desc)        free(Item->Desc);
   if (Item->Data)        free(Item->Data);
   if (Item->XData)       free(Item->XData);
   if (Item->YData)       free(Item->YData);
   if (Item->ZData)       free(Item->ZData);
   if (Item->Speed)       free(Item->Speed);
   if (Item->Dir)         free(Item->Dir);
   if (Item->ErrorData)   free(Item->ErrorData);
   if (Item->MedianData)  free(Item->MedianData);
   if (Item->HighData)    free(Item->HighData);
   if (Item->LowData)     free(Item->LowData);
   if (Item->MinData)     free(Item->MinData);
   if (Item->MaxData)     free(Item->MaxData);
   if (Item->WAxis)       free(Item->WAxis);
   if (Item->XAxis)       free(Item->XAxis);
   if (Item->YAxis)       free(Item->YAxis);
   if (Item->ZAxis)       free(Item->ZAxis);
   if (Item->Tag)         free(Item->Tag);
}

/*----------------------------------------------------------------------------
 * Nom      : <GraphItem_Wipe>
 * Creation : Mai 2005 - J.P. Gauthier - CMC/CMOE
 *
 * But      : Liberer toutes la memoire allouee par ce package.
 *
 * Parametres     :
 *
 * Retour:
 *
 * Remarques :
 *
 *----------------------------------------------------------------------------
*/
void GraphItem_Wipe() {

   Tcl_HashSearch ptr;
   Tcl_HashEntry  *entry=NULL;

   entry=Tcl_FirstHashEntry(&GraphItemTable,&ptr);
   while (entry) {
      GraphItem_Clear((TGraphItem*)Tcl_GetHashValue(entry));
      free((TGraphItem*)Tcl_GetHashValue(entry));
      Tcl_DeleteHashEntry(entry);
      entry=Tcl_FirstHashEntry(&GraphItemTable,&ptr);
   }
}

/*--------------------------------------------------------------------------------------------------------------
 * Nom          : <GraphItem_Display>
 * Creation     : Mai 2005 - J.P. Gauthier - CMC/CMOE
 *
 * But          : Afficher l'item de graph dans le graph
 *
 * Parametres   :
 *   <Interp>   : Interpreteur Tcl
 *   <Graph>    : Item graph
 *   <Item>     : Item de graph
 *   <X0>       : Limite inferieure gauche du graph
 *   <Y0>       : Limite inferieure gauche du graph
 *   <X1>       : Limite superieur  droite du graph
 *   <Y1>       : Limite superieur  droite du graph
 *   <GLMode>   : Mode de rendue
 *
 * Retour       :
 *
 * Remarques :
 *
 *---------------------------------------------------------------------------------------------------------------
*/
void GraphItem_Display(Tcl_Interp *Interp,GraphItem *Graph,TGraphItem *Item,int X0,int Y0,int X1,int Y1,GLuint GLMode) {

   TData      *data;
   TVector    *vecx,*vecy;
   TGraphAxis *axisx,*axisy,*axisz,*axisw;

   axisx=GraphAxis_Get(Item->XAxis);
   axisy=GraphAxis_Get(Item->YAxis);
   axisz=GraphAxis_Get(Item->ZAxis);
   axisw=GraphAxis_Get(Item->WAxis);

   if (!axisx || !axisy) return;

   if (Item->Data && GLMode!=GL_SELECT) {

      if ((data=Data_Get(Item->Data)) && data->Def && data->Def->NI>1) {

         Data_PreInit(data);

         GraphAxis_Define(axisx,NULL,X1-X0);
         GraphAxis_Define(axisy,NULL,Y1-Y0);

         if (data->Spec->RenderTexture) {
            if (GLRender->ShaderAvailable) {
               GraphItem_Display2DTextureShader(Interp,Graph,axisx,axisy,axisz,data,X0,Y0,X1,Y1);
            } else {
                GraphItem_Display2DTexture(Interp,Graph,axisx,axisy,axisz,data,X0,Y0,X1,Y1);
            }
         }
         if (data->Spec->RenderContour)
            GraphItem_Display2DContour(Interp,Graph,axisx,axisy,axisz,data,X0,Y0,X1,Y1);

         if (data->Spec->RenderGrid)
            GraphItem_Display2DGrid(Interp,Graph,axisx,axisy,axisz,data,X0,Y0,X1,Y1);

         if (data->Spec->RenderVector==BARB || data->Spec->RenderVector==SPEAR ||data->Spec->RenderVector==ARROW )
            GraphItem_Display2DVector(Interp,Graph,axisx,axisy,axisz,data,X0,Y0,X1,Y1);

         if (data->Spec->RenderVector==STREAMLINE)
            GraphItem_Display2DStream(Interp,Graph,axisx,axisy,axisz,data,X0,Y0,X1,Y1);

      }
   }

   if (Item->XData && Item->YData) {

      vecx=Vector_Get(Item->XData);
      vecy=Vector_Get(Item->YData);

      if (vecx && vecx->N)
         GraphAxis_Define(axisx,vecx,X1-X0);

      if (vecy && vecy->N)
         GraphAxis_Define(axisy,vecy,Y1-Y0);

      if (Graph->Type[0]=='T') {
         GraphAxis_Define(axisz,NULL,(int)hypot(X1-X0,Y1-Y0));
         GraphAxis_Define(axisw,NULL,X1-X0);
         axisx->Delta*=COSA*0.65*((Y1-Y0)<(X1-X0)?(double)(Y1-Y0)/(X1-X0):(double)(X1-X0)/(Y1-Y0));
         axisz->Delta=780.0 / 3 * axisx->Delta;

         if (!axisx->Done) {
            axisx->Done|=DONEX;
            GraphTehpi_DisplayWetAdiabats(Graph,axisz,axisx,axisy,X0,Y0,X1,Y1,GLMode);
            GraphTehpi_DisplayMixRatios(Graph,axisz,axisx,axisy,axisw,X0,Y0,X1,Y1,GLMode);
            GraphTehpi_DisplayDryAdiabats(Graph,axisz,axisx,axisy,X0,Y0,X1,Y1,GLMode);
            GraphTehpi_DisplayIsobars(Graph,axisz,axisx,axisy,X0,Y0,X1,Y1,GLMode);
            GraphTehpi_DisplayIsotherms(Graph,axisz,axisx,axisy,X0,Y0,X1,Y1,GLMode);
         }
         GraphItem_DisplayTephi(Interp,Graph,Item,axisx,axisy,axisz,X0,Y0,X1,Y1,GLMode);
      } else {

         if (Item->Type==MINMAX)
            GraphItem_DisplayMinMax(Interp,Graph,Item,axisx,axisy,axisz,X0,Y0,X1,Y1,GLMode);
         else if (Item->Type==BOXPLOT)
            GraphItem_DisplayBox(Interp,Graph,Item,axisx,axisy,axisz,X0,Y0,X1,Y1,GLMode);
         else
            GraphItem_DisplayXYZ(Interp,Graph,Item,axisx,axisy,axisz,X0,Y0,X1,Y1,GLMode);
      }
   }
}

/*--------------------------------------------------------------------------------------------------------------
 * Nom          : <GraphItem_DisplayMinMax>
 * Creation     : Novembre 2005 - J.P. Gauthier - CMC/CMOE
 *
 * But          : Afficher l'item de graph an format MinMax
 *
 * Parametres   :
 *   <Interp>   : Interpreteur Tcl
 *   <Graph>    : Item graph
 *   <Item>     : Item de graph
 *   <AxisX>    : Axe en X
 *   <AxisY>    : Axe en Y
 *   <AxisZ>    : Axe en Z
 *   <X0>       : Limite inferieure gauche du graph
 *   <Y0>       : Limite inferieure gauche du graph
 *   <X1>       : Limite superieur  droite du graph
 *   <Y1>       : Limite superieur  droite du graph
 *   <GLMode>   : Mode de rendue
 *
 * Retour       :
 *
 * Remarques :
 *
 *---------------------------------------------------------------------------------------------------------------
*/

void GraphItem_DisplayMinMax(Tcl_Interp *Interp,GraphItem *Graph,TGraphItem *Item,TGraphAxis *AxisX,TGraphAxis *AxisY,TGraphAxis *AxisZ,int X0,int Y0,int X1,int Y1,GLuint GLMode) {

   TVector   *vec,*vec0,*vec1;
   Vect3d    *v=NULL;
   int        i,vn;

   vec=Item->Orient[0]=='X'?Vector_Get(Item->XData):Vector_Get(Item->YData);

   if (!vec)
      return;

   /* Display Min and Max */
   vec0=Vector_Get(Item->MinData);
   vec1=Vector_Get(Item->MaxData);

   if (vec0 && vec0->N<=vec->N && vec1 && vec1->N<=vec->N && Item->Outline) {

      if (!(v=(Vect3d*)malloc(2*vec->N*sizeof(Vect3d)))) {
         App_Log(ERROR,"%s: Memory allocation error",__func__);
         return;
      }
      vn=0;

      /* Compute graph curve points */
      for(i=0;i<vec->N;i++) {
         if (vec->V[i]!=vec->NoData && vec0->V[i]!=vec0->NoData) {
            if (Item->Orient[0]=='X') {
               v[vn][0]=X0+AXISVALUE(AxisX,vec->V[i]);
               v[vn][1]=Y0+AXISVALUE(AxisY,vec0->V[i]);
               v[vn][2]=0.0;
            } else {
               v[vn][0]=X0+AXISVALUE(AxisX,vec0->V[i]);
               v[vn][1]=Y0+AXISVALUE(AxisY,vec->V[i]);
               v[vn][2]=0.0;
            }
            vn++;
         }
      }

      for(i=0;i<vec->N;i++) {
         if (vec->V[vec->N-i-1]!=vec->NoData && vec0->V[vec->N-i-1]!=vec0->NoData) {
            if (Item->Orient[0]=='X') {
               v[vn][0]=X0+AXISVALUE(AxisX,vec->V[vec->N-i-1]);
               v[vn][1]=Y0+AXISVALUE(AxisY,vec1->V[vec->N-i-1]);
               v[vn][2]=0.0;
            } else {
               v[vn][0]=X0+AXISVALUE(AxisX,vec1->V[vec->N-i-1]);
               v[vn][1]=Y0+AXISVALUE(AxisY,vec->V[vec->N-i-1]);
               v[vn][2]=0.0;
            }
            vn++;
         }
      }

      if (Item->Alpha<100) {
         glEnable(GL_BLEND);
      }

      /* Display graph filling */
      if (Item->Fill) {
         if (Item->Stipple) {
            glEnable(GL_POLYGON_STIPPLE);
            glPolygonStipple(Item->Stipple->Data);
         }
         glColor4us(Item->Fill->red,Item->Fill->green,Item->Fill->blue,Item->Alpha*Graph->Alpha*0.01*655);
         glPolygonMode(GL_FRONT_AND_BACK,GL_FILL);

         gluTessBeginPolygon(GLRender->GLTess,NULL);
         gluTessBeginContour(GLRender->GLTess);

         /* Begin the tesselation */
         for(i=0;i<vn;i++) {
            gluTessVertex(GLRender->GLTess,v[i],v[i]);
         }

         gluTessEndContour(GLRender->GLTess);
         gluTessEndPolygon(GLRender->GLTess);
         glDisable(GL_POLYGON_STIPPLE);
      }

      /* Display graph outline */
      if (Item->Outline && Item->Width) {
         glDash(&Item->Dash);
         glColor4us(Item->Outline->red,Item->Outline->green,Item->Outline->blue,Item->Alpha*Graph->Alpha*0.01*655);
         glLineWidth(Item->Width);
         glPolygonMode(GL_FRONT,GL_LINE);

         glBegin(GL_LINE_STRIP);
         for(i=0;i<vn;i++) {
            glVertex3dv(v[i]);
         }
         glEnd();

         glDisable(GL_LINE_STIPPLE);
      }
   }
   
   free(v);
}

/*--------------------------------------------------------------------------------------------------------------
 * Nom          : <GraphItem_DisplayBox>
 * Creation     : Novembre 2005 - J.P. Gauthier - CMC/CMOE
 *
 * But          : Afficher l'item de graph an format boxplot
 *
 * Parametres   :
 *   <Interp>   : Interpreteur Tcl
 *   <Graph>    : Item graph
 *   <Item>     : Item de graph
 *   <AxisX>    : Axe en X
 *   <AxisY>    : Axe en Y
 *   <AxisZ>    : Axe en Z
 *   <X0>       : Limite inferieure gauche du graph
 *   <Y0>       : Limite inferieure gauche du graph
 *   <X1>       : Limite superieur  droite du graph
 *   <Y1>       : Limite superieur  droite du graph
 *   <GLMode>   : Mode de rendue
 *
 * Retour       :
 *
 * Remarques :
 *
 *---------------------------------------------------------------------------------------------------------------
*/
void GraphItem_DisplayBox(Tcl_Interp *Interp,GraphItem *Graph,TGraphItem *Item,TGraphAxis *AxisX,TGraphAxis *AxisY,TGraphAxis *AxisZ,int X0,int Y0,int X1,int Y1,GLuint GLMode) {

   TVector *vec,*vec0,*vec1;
   double  x[2],y[2];
   int     n;

   vec=Item->Orient[0]=='X'?Vector_Get(Item->XData):Vector_Get(Item->YData);

   if (!vec)
      return;

   glLineWidth(Item->Width);

   /* Display Min and Max */
   vec0=Vector_Get(Item->MinData);
   vec1=Vector_Get(Item->MaxData);

   if (vec0 && vec0->N<=vec->N && vec1 && vec1->N<=vec->N && Item->Outline) {
      glColor4us(Item->Outline->red,Item->Outline->green,Item->Outline->blue,Item->Alpha*Graph->Alpha*0.01*655);
      glBegin(GL_LINES);
      for (n=0;n<vec->N;n++) {
         if (vec->V[n]!=vec->NoData && vec0->V[n]!=vec0->NoData && vec1->V[n]!=vec1->NoData) {
            if (Item->Orient[0]=='X') {
               x[0]=X0+AXISVALUE(AxisX,vec->V[n]);
               y[0]=Y0+AXISVALUE(AxisY,vec0->V[n]);
               y[1]=Y0+AXISVALUE(AxisY,vec1->V[n]);

               glVertex2f(x[0],y[0]);
               glVertex2f(x[0],y[1]);
               x[1]=x[0]+Item->Size;
               x[0]=x[0]-Item->Size;
               glVertex2f(x[0],y[0]);
               glVertex2f(x[1],y[0]);
               glVertex2f(x[0],y[1]);
               glVertex2f(x[1],y[1]);
            } else {
               x[0]=X0+AXISVALUE(AxisX,vec0->V[n]);
               x[1]=X0+AXISVALUE(AxisX,vec1->V[n]);
               y[0]=Y0+AXISVALUE(AxisY,vec->V[n]);

               glVertex2f(x[0],y[0]);
               glVertex2f(x[1],y[0]);
               y[1]=y[0]+Item->Size;
               y[0]=y[0]-Item->Size;

               glVertex2f(x[0],y[0]);
               glVertex2f(x[0],y[1]);
               glVertex2f(x[1],y[0]);
               glVertex2f(x[1],y[1]);
            }
         }
      }
      glEnd();
   }

   /* Display High Low Centile bar */
   vec0=Vector_Get(Item->HighData);
   vec1=Vector_Get(Item->LowData);

   if (vec0 && vec0->N<=vec->N && vec1 && vec1->N<=vec->N && Item->Outline) {
      if (Item->Fill) {
         glColor4us(Item->Fill->red,Item->Fill->green,Item->Fill->blue,Item->Alpha*Graph->Alpha*0.01*655);
      } else {
         glColor4us(Graph->BGColor->red,Graph->BGColor->green,Graph->BGColor->blue,Item->Alpha*Graph->Alpha*0.01*655);
      }
      glPolygonMode(GL_FRONT,GL_FILL);
      glBegin(GL_QUADS);
      for (n=0;n<vec->N;n++) {
         if (vec->V[n]!=vec->NoData && vec0->V[n]!=vec0->NoData && vec1->V[n]!=vec1->NoData) {
            if (Item->Orient[0]=='X') {
               x[0]=x[1]=X0+AXISVALUE(AxisX,vec->V[n]);
               y[0]=Y0+AXISVALUE(AxisY,vec0->V[n]);
               y[1]=Y0+AXISVALUE(AxisY,vec1->V[n]);
               x[0]-=Item->Size;
               x[1]+=Item->Size;
            } else {
               x[0]=X0+AXISVALUE(AxisX,vec0->V[n]);
               x[1]=X0+AXISVALUE(AxisX,vec1->V[n]);
               y[0]=y[1]=Y0+AXISVALUE(AxisY,vec->V[n]);
               y[0]-=Item->Size;
               y[1]+=Item->Size;

            }
            glVertex2f(x[0],y[0]);
            glVertex2f(x[0],y[1]);
            glVertex2f(x[1],y[1]);
            glVertex2f(x[1],y[0]);
         }
      }
      glEnd();

      glColor4us(Item->Outline->red,Item->Outline->green,Item->Outline->blue,Item->Alpha*Graph->Alpha*0.01*655);
      glPolygonMode(GL_FRONT,GL_LINE);
      glBegin(GL_QUADS);
      for (n=0;n<vec->N;n++) {
         if (vec->V[n]!=vec->NoData && vec0->V[n]!=vec0->NoData && vec1->V[n]!=vec1->NoData) {
            if (Item->Orient[0]=='X') {
               x[0]=x[1]=X0+AXISVALUE(AxisX,vec->V[n]);
               y[0]=Y0+AXISVALUE(AxisY,vec0->V[n]);
               y[1]=Y0+AXISVALUE(AxisY,vec1->V[n]);
               x[0]-=Item->Size;
               x[1]+=Item->Size;
            } else {
               x[0]=X0+AXISVALUE(AxisX,vec0->V[n]);
               x[1]=X0+AXISVALUE(AxisX,vec1->V[n]);
               y[0]=y[1]=Y0+AXISVALUE(AxisY,vec->V[n]);
               y[0]-=Item->Size;
               y[1]+=Item->Size;

            }
            glVertex2f(x[0],y[0]);
            glVertex2f(x[0],y[1]);
            glVertex2f(x[1],y[1]);
            glVertex2f(x[1],y[0]);
         }
      }
      glEnd();
   }

   /* Display Median */
   vec0=Vector_Get(Item->MedianData);

   if (vec0 && vec0->N<=vec->N && Item->Outline) {
      glColor4us(Item->Outline->red,Item->Outline->green,Item->Outline->blue,Item->Alpha*Graph->Alpha*0.01*655);
      glBegin(GL_LINES);
      for (n=0;n<vec->N;n++) {
         if (vec->V[n]!=vec->NoData && vec0->V[n]!=vec0->NoData) {
            if (Item->Orient[0]=='X') {
               x[0]=X0+AXISVALUE(AxisX,vec->V[n]);
               y[0]=y[1]=Y0+AXISVALUE(AxisY,vec0->V[n]);
               x[1]=x[0]+Item->Size;
               x[0]-=Item->Size;
            } else {
               x[0]=x[1]=X0+AXISVALUE(AxisX,vec0->V[n]);
               y[0]=Y0+AXISVALUE(AxisY,vec->V[n]);
               y[1]=y[0]+Item->Size;
               y[0]-=Item->Size;
            }
            glVertex2f(x[0],y[0]);
            glVertex2f(x[1],y[1]);
         }
      }
      glEnd();
   }

   /* Display Error */
   vec0=Vector_Get(Item->ErrorData);

   if (vec0 && vec0->N<=vec->N) {
      if (Item->Icon && Item->Size>0.0) {
         glVertexPointer(2,GL_DOUBLE,0,IconList[Item->Icon].Co);
         glEnableClientState(GL_VERTEX_ARRAY);
         glMatrixMode(GL_MODELVIEW);
         for (n=0;n<vec->N;n++) {
            if (vec->V[n]!=vec->NoData && vec0->V[n]!=vec0->NoData) {
               if (Item->Orient[0]=='X') {
                  x[0]=X0+AXISVALUE(AxisX,vec->V[n]);
                  y[0]=Y0+AXISVALUE(AxisY,vec0->V[n]);
               } else {
                  x[0]=X0+AXISVALUE(AxisX,vec0->V[n]);
                  y[0]=Y0+AXISVALUE(AxisY,vec->V[n]);
               }
               glPushMatrix();
               glTranslated(x[0],y[0],0.0);
               glScalef(Item->Size*0.5,Item->Size*0.5,1.0f);
               if (Item->IconFill) {
                  glColor4us(Item->IconFill->red,Item->IconFill->green,Item->IconFill->blue,Item->Alpha*Graph->Alpha*0.01*655);
               } else {
                  glColor4us(Graph->BGColor->red,Graph->BGColor->green,Graph->BGColor->blue,Item->Alpha*Graph->Alpha*0.01*655);
               }
               glPolygonMode(GL_FRONT_AND_BACK,GL_FILL);
               glDrawArrays(IconList[Item->Icon].Type,0,IconList[Item->Icon].Nb);

               if (Item->IconOutline && Item->Width) {
                  glColor4us(Item->IconOutline->red,Item->IconOutline->green,Item->IconOutline->blue,Item->Alpha*Graph->Alpha*0.01*655);
                  glPolygonMode(GL_FRONT_AND_BACK,GL_LINE);
                  glDrawArrays(IconList[Item->Icon].Type,0,IconList[Item->Icon].Nb);
               }
               glPopMatrix();
            }
         }
         glDisableClientState(GL_VERTEX_ARRAY);
      }
   }
}

/*--------------------------------------------------------------------------------------------------------------
 * Nom          : <GraphItem_DisplayXYZ>
 * Creation     : Mai 2005 - J.P. Gauthier - CMC/CMOE
 *
 * But          : Afficher l'item de graph dans le graph en vectoriel
 *
 * Parametres   :
 *   <Interp>   : Interpreteur Tcl
 *   <Graph>    : Item graph
 *   <Item>     : Item de graph
 *   <AxisX>    : Axe en X
 *   <AxisY>    : Axe en Y
 *   <AxisZ>    : Axe en Z
 *   <X0>       : Limite inferieure gauche du graph
 *   <Y0>       : Limite inferieure gauche du graph
 *   <X1>       : Limite superieur  droite du graph
 *   <Y1>       : Limite superieur  droite du graph
 *   <GLMode>   : Mode de rendue
 *
 * Retour       :
 *
 * Remarques :
 *
 *---------------------------------------------------------------------------------------------------------------
*/
void GraphItem_DisplayXYZ(Tcl_Interp *Interp,GraphItem *Graph,TGraphItem *Item,TGraphAxis *AxisX,TGraphAxis *AxisY,TGraphAxis *AxisZ,int X0,int Y0,int X1,int Y1,GLuint GLMode) {

   Tk_FontMetrics tkm;
   TVector       *vecx,*vecy,*vecl,*vecs,*vecd,*val;
   Vect3d        *v=NULL,*vl=NULL,v0,v1,vt;
   char           buf[32];
   double        *vm,x,y,db,dh,x0,y0,sz,avg;
   int            i,j,n,vn,px,py,pw,hd;

   vecx=Vector_Get(Item->XData);
   vecy=Vector_Get(Item->YData);
   vecl=Vector_Get(Item->LowData);

   if (!vecx || !vecy)
      return;

   if (!(n=vecx->N<vecy->N?vecx->N:vecy->N))
      return;

   // Histograms may have on more value in the axis orientation side
   if (Item->Type==HISTOGRAM) {
      hd=Item->Orient[0]=='X'?(vecx->N>vecy->N):(vecx->N<vecy->N);
   } else {
      hd=0;
   }
   if (!(v=(Vect3d*)malloc((n+hd)*sizeof(Vect3d)))) {
      App_Log(ERROR,"%s: Memory allocation error",__func__);
      return;
   }

   if (vecl) {
      if (!(vl=(Vect3d*)malloc((n+hd)*sizeof(Vect3d)))) {
         App_Log(ERROR,"%s: Memory allocation error",__func__);
         return;
      }
   }
   
   // Compute graph curve points
   db=1000.0;
   vn=0;
   avg=x=x0=y0=0.0;
   val=NULL;
   for(i=0;i<n+hd;i++) {
      // Histograms check
      if (i==n) {
         x=Item->Orient[0]=='X'?vecx->V[i]:vecx->V[n-1];
         y=Item->Orient[0]=='Y'?vecy->V[i]:vecy->V[n-1];
      } else {
         x=vecx->V[i];
         y=vecy->V[i];
         avg+=(Item->Orient[0]=='X'?vecy->V[i]:vecx->V[i]);
      }

      if (x!=vecx->NoData && y!=vecy->NoData) {
         v[vn][0]=X0+AXISVALUE(AxisX,x);
         v[vn][1]=Y0+AXISVALUE(AxisY,y);
         v[vn][2]=0.0;

         if (vecl) {
            vl[vn][0]=X0+AXISVALUE(AxisX,vecl->V[i]);
            vl[vn][1]=Y0+AXISVALUE(AxisY,vecl->V[i]);
            vl[vn][2]=0.0;
         }
         
         if (vn>0) {
            if (Item->Orient[0]=='X') {
               dh=fabs(v[vn][0]-v[vn-1][0]);
               if (dh>0.0) db=db<dh?db:dh;
            } else {
               dh=fabs(v[vn][1]-v[vn-1][1]);
               if (dh>0.0) db=db<dh?db:dh;
            }
         }
         vn++;
      }
   }
   avg/=n;
   
   // Compute item spacing and width for bar and histogram graph
   switch(Item->Type) {
      case WIDEBAR   : db=db/(Graph->NSide+1)*0.5; dh=db*2.0*(Graph->NSide*0.5-Graph->ISide-0.5); break;
      case HISTOGRAM : db*=0.50;                   dh=-db;                                        break;
      case BAR       : db*=0.25;                   dh=0.0;                                        break;
      default        : db=0.0;                     dh=0.0;                                        break;
   }

   // In case we fill, we need to close the path
   if (Item->Orient[0]=='X') {
      y0=Item->Origin==HUGE_VAL?Y0:Y0+AXISVALUE(AxisY,Item->Origin);
      v0[0]=v[0][0];
      v0[1]=Y0;
      v0[2]=0.0;
      v1[0]=v[vn-1][0];
      v1[1]=Y0;
      v1[2]=0.0;
   } else {
      x0=Item->Origin==HUGE_VAL?X0:X0+AXISVALUE(AxisX,Item->Origin);
      v0[0]=X0;
      v0[1]=v[0][1];
      v0[2]=0.0;
      v1[0]=X0;
      v1[1]=v[vn-1][1];
      v1[2]=0.0;
   }

   if (Item->Alpha<100) {
      glEnable(GL_BLEND);
   }

   // Display graph filling 
   if (Item->Fill) {
      if (Item->Stipple) {
         glEnable(GL_POLYGON_STIPPLE);
         glPolygonStipple(Item->Stipple->Data);
      }

      glColor4us(Item->Fill->red,Item->Fill->green,Item->Fill->blue,Item->Alpha*Graph->Alpha*0.01*655);
      glPolygonMode(GL_FRONT_AND_BACK,GL_FILL);

      if (Item->Type==HISTOGRAM) {
         if (Item->Orient[0]=='X') {
            for(i=0;i<vn-hd;i++) {
               glPushName(i);
               GraphItem_ColorXYZ(Interp,Graph,Item,i);
               glBegin(GL_QUADS);
                  glVertex2f(v[i][0],vl?vl[i][1]:y0);
                  glVertex2f(v[i][0],v[i][1]);
                  if (!hd && (i+1)==vn) {
                     glVertex2f(X1,v[i][1]);
                     glVertex2f(X1,vl?vl[i][1]:y0);
                  } else {
                     glVertex2f(v[i+1][0],v[i][1]);
                     glVertex2f(v[i+1][0],vl?vl[i][1]:y0);
                  }
               glEnd();
               glPopName();
           }
         } else {
            for(i=0;i<vn-hd;i++) {
               glPushName(i);
               GraphItem_ColorXYZ(Interp,Graph,Item,i);
               glBegin(GL_QUADS);
                  glVertex2f(vl?vl[i][0]:x0,v[i][1]);
                  glVertex2f(v[i][0],v[i][1]);
                  if (!hd && (i+1)==vn) {
                     glVertex2f(v[i][0],Y1);
                     glVertex2f(vl?vl[i][0]:x0,Y1);
                  } else {
                     glVertex2f(v[i][0],v[i+1][1]);
                     glVertex2f(vl?vl[i][0]:x0,v[i+1][1]);
                  }
               glEnd();
               glPopName();
            }
         }
      } else if (Item->Type==BAR || Item->Type==WIDEBAR) {
         if (Item->Orient[0]=='X') {
            for(i=0;i<vn;i++) {
               glPushName(i);
               GraphItem_ColorXYZ(Interp,Graph,Item,i);
               glBegin(GL_QUADS);
                  glVertex2f(v[i][0]-db-dh,vl?vl[i][1]:y0);
                  glVertex2f(v[i][0]-db-dh,v[i][1]);
                  glVertex2f(v[i][0]+db-dh,v[i][1]);
                  glVertex2f(v[i][0]+db-dh,vl?vl[i][1]:y0);
               glEnd();
               glPopName();
            }
         } else {
            for(i=0;i<vn;i++) {
               glPushName(i);
               GraphItem_ColorXYZ(Interp,Graph,Item,i);
               glBegin(GL_QUADS);
                  glVertex2f(vl?vl[i][0]:x0,v[i][1]-db-dh);
                  glVertex2f(v[i][0],v[i][1]-db-dh);
                  glVertex2f(v[i][0],v[i][1]+db-dh);
                  glVertex2f(vl?vl[i][0]:x0,v[i][1]+db-dh);
               glEnd();
               glPopName();
            }
         }
      } else if (Item->Type!=NONE && GLMode==GL_RENDER) {

         gluTessBeginPolygon(GLRender->GLTess,NULL);
         gluTessBeginContour(GLRender->GLTess);

         // Begin the tesselation
         gluTessVertex(GLRender->GLTess,v0,v0);
         for(i=0;i<vn;i++) {
            gluTessVertex(GLRender->GLTess,v[i],v[i]);
            if (Item->Type==SPLINE && i<n-1) {
               for(j=1;j<10;j++) {
                  vm=glTessTmpGet();
                  vm[0]=InterpHermite(v[i==0?0:i-1][0],v[i][0],v[i<vn-1?i+1:vn-1][0],v[i<vn-2?i+2:vn-1][0],j/10.0,0.0,0.0);
                  vm[1]=InterpHermite(v[i==0?0:i-1][1],v[i][1],v[i<vn-1?i+1:vn-1][1],v[i<vn-2?i+2:vn-1][1],j/10.0,0.0,0.0);
//                 vt[2]=InterpHermite(v[i==0?0:i-1][2],v[i][2],v[i<vn-1?i+1:vn-1][2],v[i<vn-2?i+2:vn-1][2],j/10.0,0.0,0.0);
                  vm[2]=0.0;
                  gluTessVertex(GLRender->GLTess,vm,vm);
               }
            }
         }
         gluTessVertex(GLRender->GLTess,v1,v1);

         gluTessEndContour(GLRender->GLTess);
         gluTessEndPolygon(GLRender->GLTess);
      }
      glDisable(GL_POLYGON_STIPPLE);
   }

   // Display graph outline
   if (Item->Outline && Item->Width) {
      glDash(&Item->Dash);
      glColor4us(Item->Outline->red,Item->Outline->green,Item->Outline->blue,Item->Alpha*Graph->Alpha*0.01*655);
      glLineWidth(Item->Width);
      glPolygonMode(GL_FRONT_AND_BACK,GL_LINE);

      if (Item->Type==HISTOGRAM) {
         if (Item->Orient[0]=='X') {
            for(i=0;i<vn-hd;i++) {
               glPushName(i);
               glBegin(GL_QUADS);
                  glVertex2f(v[i][0],vl?vl[i][1]:y0);
                  glVertex2f(v[i][0],v[i][1]);
                  if (!hd && (i+1)==vn) {
                     glVertex2f(X1,v[i][1]);
                     glVertex2f(X1,vl?vl[i][1]:y0);
                   } else {
                     glVertex2f(v[i+1][0],v[i][1]);
                     glVertex2f(v[i+1][0],vl?vl[i][1]:y0);
                   }
               glEnd();
               glPopName();
           }
         } else {
            for(i=0;i<vn-hd;i++) {
               glPushName(i);
               glBegin(GL_QUADS);
                  glVertex2f(vl?vl[i][0]:x0,v[i][1]);
                  glVertex2f(v[i][0],v[i][1]);
                  if (!hd && (i+1)==vn) {
                     glVertex2f(v[i][0],Y1);
                     glVertex2f(vl?vl[i][0]:x0,Y1);
                  } else {
                     glVertex2f(v[i][0],v[i+1][1]);
                     glVertex2f(vl?vl[i][0]:x0,v[i+1][1]);
                  }
               glEnd();
               glPopName();
           }
         }
      } else if (Item->Type==BAR || Item->Type==WIDEBAR) {
         if (Item->Orient[0]=='X') {
            for(i=0;i<vn;i++) {
               glPushName(i);
               glBegin(GL_QUADS);
                  glVertex2f(v[i][0]-db-dh,vl?vl[i][1]:y0);
                  glVertex2f(v[i][0]-db-dh,v[i][1]);
                  glVertex2f(v[i][0]+db-dh,v[i][1]);
                  glVertex2f(v[i][0]+db-dh,vl?vl[i][1]:y0);
               glEnd();
               glPopName();
           }
         } else {
            for(i=0;i<vn;i++) {
               glPushName(i);
               glBegin(GL_QUADS);
                  glVertex2f(vl?vl[i][0]:x0,v[i][1]-db-dh);
                  glVertex2f(v[i][0],v[i][1]-db-dh);
                  glVertex2f(v[i][0],v[i][1]+db-dh);
                  glVertex2f(vl?vl[i][0]:x0,v[i][1]+db-dh);
               glEnd();
               glPopName();
           }
         }
      } else if (Item->Type!=NONE && GLMode==GL_RENDER) {
         glBegin(GL_LINE_STRIP);
            if (Item->Fill) glVertex3dv(v0);
            for(i=0;i<vn;i++) {
               glVertex3dv(v[i]);
               if (Item->Type==SPLINE && i<n-1) {
                  for(j=1;j<10;j++) {
                     vt[0]=InterpHermite(v[i==0?0:i-1][0],v[i][0],v[i<vn-1?i+1:vn-1][0],v[i<vn-2?i+2:vn-1][0],j/10.0,0.0,0.0);
                     vt[1]=InterpHermite(v[i==0?0:i-1][1],v[i][1],v[i<vn-1?i+1:vn-1][1],v[i<vn-2?i+2:vn-1][1],j/10.0,0.0,0.0);
//                     vt[2]=InterpHermite(v[i==0?0:i-1][2],v[i][2],v[i<vn-1?i+1:vn-1][2],v[i<vn-2?i+2:vn-1][2],j/10.0,0.0,0.0);
                     vt[2]=0.0;
                     glVertex3dv(vt);
                  }
               }
            }
            if (Item->Fill) glVertex3dv(v1);
         glEnd();
      }
      glDisable(GL_LINE_STIPPLE);

      vecs=Vector_Get(Item->Speed);
      vecd=Vector_Get(Item->Dir);
      if (vecs && vecd) {
         for(i=0;i<vn;i++) {
            Data_RenderBarbule(1,1,0.0,v[i][0],v[i][1],0.0,vecs->V[i],vecd->V[i],Item->Size*2,NULL);
         }
      }
   }

   // Display Icons
   if (Item->Icon && Item->Size>0.0) {
      sz=(Item->Size+Item->Width)*0.5;
      glLineWidth(Item->Width);
      glEnableClientState(GL_VERTEX_ARRAY);
      glVertexPointer(2,GL_DOUBLE,0,IconList[Item->Icon].Co);
      for(i=0;i<vn-hd;i++) {
         glPushName(i);
         if (i==0 || i==(vn-hd-1) || Item->IconXShowValue==1e32 || fmod(vecx->V[i],Item->IconXShowValue)==0.0) {
            glPushMatrix();
            if (Item->Orient[0]=='X') {
               glTranslated(v[i][0]-dh,v[i][1],v[i][2]);
            } else {
               glTranslated(v[i][0],v[i][1]-dh,v[i][2]);
            }
            glScalef(sz,sz,1.0f);
            glColor4us(Graph->BGColor->red,Graph->BGColor->green,Graph->BGColor->blue,Item->Alpha*Graph->Alpha*0.01*655);
            if (Item->IconFill) {
               if (Item->IconXFillValue==1e32 || fmod(vecx->V[i],Item->IconXFillValue)==0.0) {
                  glColor4us(Item->IconFill->red,Item->IconFill->green,Item->IconFill->blue,Item->Alpha*Graph->Alpha*0.01*655);
               }
            }
            GraphItem_ColorXYZ(Interp,Graph,Item,i);
            glPolygonMode(GL_FRONT_AND_BACK,GL_FILL);
            glDrawArrays(IconList[Item->Icon].Type,0,IconList[Item->Icon].Nb);

            if (Item->IconOutline && Item->Width) {
               glColor4us(Item->IconOutline->red,Item->IconOutline->green,Item->IconOutline->blue,Item->Alpha*Graph->Alpha*0.01*655);
               glPolygonMode(GL_FRONT_AND_BACK,GL_LINE);
               glDrawArrays(IconList[Item->Icon].Type,0,IconList[Item->Icon].Nb);
            }
            glPopMatrix();
         }
         glPopName();
      }
      glDisableClientState(GL_VERTEX_ARRAY);
   }

   // Display Values
   if (Item->Value && Item->Font && GLMode==GL_RENDER) {
      glFontUse(Tk_Display(Tk_CanvasTkwin(Graph->canvas)),Item->Font);
      glColor4us(Item->Outline->red,Item->Outline->green,Item->Outline->blue,Item->Alpha*Graph->Alpha*0.01*655);

      switch(Item->Value) {
         case  1: val=(Item->Orient[0]=='X')?vecy:vecx; break;
         case  2: val=vecx; break;
         case  3: val=vecx; break;
         case  4: val=vecy; break;
         case  5: val=Vector_Get(Item->ZData); break;
         case  6: val=Vector_Get(Item->Speed); break;
         case  7: val=Vector_Get(Item->Dir); break;
         case  8: val=Vector_Get(Item->ErrorData); break;
         case  9: val=Vector_Get(Item->HighData); break;
         case 10: val=Vector_Get(Item->LowData); break;
         case 11: val=Vector_Get(Item->MedianData); break;
         case 12: val=Vector_Get(Item->MinData); break;
         case 13: val=Vector_Get(Item->MaxData); break;
      }
      if (val) {
         px=py=pw=0;
         Tk_GetFontMetrics(Item->Font,&tkm);
         for(i=0;i<vn-hd;i++) {
            GraphAxis_Print(Item->Orient[0]=='X'?AxisY:AxisX,buf,val->V[i],-2);
            j=Item->Width+2+(Item->Icon?Item->Size:0);
            y=v[i][1]+j;
            j=Tk_TextWidth(Item->Font,buf,strlen(buf));
            switch(Item->Anchor) {
               case TK_ANCHOR_N:
               case TK_ANCHOR_S:
               case TK_ANCHOR_CENTER: x=v[i][0]-j/2; break;
               case TK_ANCHOR_NW:
               case TK_ANCHOR_SW:               
               case TK_ANCHOR_W:      x=v[i][0]+j/strlen(buf);     break;
               case TK_ANCHOR_NE:
               case TK_ANCHOR_SE:
               case TK_ANCHOR_E:      x=v[i][0]-j-j/strlen(buf);   break;
            }
            if (!((y>=py && y<=py+tkm.linespace) || (y+tkm.linespace>=py && y<=py)) || !((x>=px && x<=px+pw) ||  (x+j>=px && x+j<=px+pw))) {
               px=x;py=y;pw=j;
               glPrint(Interp,Graph->canvas,buf,x,y,0.0);
            }
         }
      }
   }

   // Display bitmaps and images
   if (Item->Bitmap) {
      glColor4us(Item->Outline->red,Item->Outline->green,Item->Outline->blue,Item->Alpha*Graph->Alpha*0.01*655);
      for(i=0;i<vn-hd;i++) {
         glPushName(i);
         if (Item->Orient[0]=='X') {
            trRasterPos2i((v[i][0]-dh-((TkCanvas*)Graph->canvas)->xOrigin)-Item->Bitmap->Width/2,-(v[i][1]-((TkCanvas*)Graph->canvas)->yOrigin)+Item->Bitmap->Height/2);
         } else {
            trRasterPos2i((v[i][0]-((TkCanvas*)Graph->canvas)->xOrigin)-Item->Bitmap->Width/2,-(v[i][1]-dh-((TkCanvas*)Graph->canvas)->yOrigin)+Item->Bitmap->Height/2);
         }
         glBitmap(Item->Bitmap->Width,Item->Bitmap->Height,0.0,0.0,0.0,0.0,(GLubyte *)Item->Bitmap->Data);
         glPopName();
      }
   }

   if (Item->ImageString) {
      Tk_PhotoImageBlock data;
      Tk_PhotoHandle     handle;
      GLubyte *pixel;

      handle=Tk_FindPhoto(((TkCanvas*)Graph->canvas)->interp,Item->ImageString);
      Tk_PhotoGetImage(handle,&data);

      // We have to flip the image data along the Y axis
      if (!(pixel=(GLubyte*)malloc(data.width*data.height*data.pixelSize))) {
         App_Log(ERROR,"%s: Memory allocation error",__func__);
         return;
      }
      memcpy(pixel,data.pixelPtr,data.width*data.height*data.pixelSize);
      DataFlip(data.pixelPtr,pixel,data.width,data.height,data.pixelSize);

      glEnable(GL_BLEND);
      for(i=0;i<vn-hd;i++) {
         glPushName(i);
         if (Item->Orient[0]=='X') {
            trRasterPos2i((v[i][0]-dh-((TkCanvas*)Graph->canvas)->xOrigin)-data.width/2,-(v[i][1]-((TkCanvas*)Graph->canvas)->yOrigin)+data.height/2);
         } else {
            trRasterPos2i((v[i][0]-((TkCanvas*)Graph->canvas)->xOrigin)-data.width/2,-(v[i][1]-dh-((TkCanvas*)Graph->canvas)->yOrigin)+data.height/2);
         }
         glDrawPixels(data.width,data.height,data.pixelSize==3?GL_RGB:GL_RGBA,GL_UNSIGNED_BYTE,pixel);
         glPopName();
      }
      free(pixel);
   }

   // Display AVG line
   if (Item->Avg && vn>1 && GLMode==GL_RENDER) {     
      if (Item->Outline) {
         glEnable(GL_BLEND);
         glLineWidth(Item->Width);
         glColor4us(Item->Outline->red,Item->Outline->green,Item->Outline->blue,0.25);
         glBegin(GL_LINES);
         if (Item->Orient[0]=='X') {
            avg=Y0+AXISVALUE(AxisY,avg);
            glVertex2f(v[0][0] ,avg);
            glVertex2f(v[vn-1][0],avg);
         } else {
            avg=X0+AXISVALUE(AxisX,avg);
            glVertex2f(avg,v[0][1] );
            glVertex2f(avg,v[vn-1][1] );          
         }
         glEnd();
      }
   }
   
   // Display Fit curve
   if (Item->Fit && vn>1 && GLMode==GL_RENDER) {
      switch(Item->Fit[0]) {
         case 'L' : vn=GraphItem_FitLinear(v,vecx,vecy,AxisX,AxisY,AxisZ,X0,Y0,X1,Y1); break;
//         case 'G' : vn=GraphItem_FitGauss(v,vecx,vecy,AxisX,AxisY,AxisZ,X0,Y0,X1,Y1); break;
         default  : vn=0;
      }

      if (Graph->FGColor) {
         glLineWidth(Item->Width*2+2);
         glColor4us(Graph->FGColor->red,Graph->FGColor->green,Graph->FGColor->blue,Item->Alpha*Graph->Alpha*0.01*655);
         glBegin(GL_LINE_STRIP);
            for(i=0;i<vn;i++) glVertex2f(v[i][0],v[i][1]);
         glEnd();
      }

      if (Item->Outline) {
         glLineWidth(Item->Width*2);
         glColor4us(Item->Outline->red,Item->Outline->green,Item->Outline->blue,Item->Alpha*Graph->Alpha*0.01*655);
         glBegin(GL_LINE_STRIP);
            for(i=0;i<vn;i++) glVertex2f(v[i][0],v[i][1]);
         glEnd();
      }
   }

   glDisable(GL_BLEND);
   
   if (v)  free(v);
   if (vl) free(vl);
}

int GraphItem_FitLinear(Vect3d *V,TVector *VX,TVector *VY,TGraphAxis *AxisX,TGraphAxis *AxisY,TGraphAxis *AxisZ,int X0,int Y0,int X1,int Y1) {

   double ssxx,ssxy,avgx,avgy,a,b,y0,y1,x0,x1;
   int i,vn,n=VX->N<VY->N?VX->N:VY->N;

   ssxx=ssxy=avgx=avgy=vn=0.0;
   for(i=0;i<n;i++) {
      if (VX->V[i]!=VX->NoData && VY->V[i]!=VY->NoData) {
         avgx+=VX->V[i];
         avgy+=VY->V[i];
         vn++;
      }
   }
   avgx/=vn;
   avgy/=vn;

   for(i=0;i<n;i++) {
      if (VX->V[i]!=VX->NoData && VY->V[i]!=VY->NoData) {
         x0=(VX->V[i]-avgx);
         ssxy+=x0*(VY->V[i]-avgy);
         ssxx+=x0*x0;
      }
   }
   b=ssxy/ssxx;
   a=avgy-b*avgx;

   VECTORMIN(VX,x0);
   VECTORMAX(VX,x1);

   y0=a+b*x0;
   y1=a+b*x1;

   y0=Y0+AXISVALUE(AxisY,y0);
   y1=Y0+AXISVALUE(AxisY,y1);

   x0=X0+AXISVALUE(AxisX,x0);
   x1=X0+AXISVALUE(AxisX,x1);

   V[0][0]=x0; V[0][1]=y0; V[0][2]=0.0;
   V[1][0]=x1; V[1][1]=y1; V[1][2]=0.0;

   return(2);
}

int GraphItem_FitLinear2(Vect3d *V,TVector *VX,TVector *VY,TGraphAxis *AxisX,TGraphAxis *AxisY,TGraphAxis *AxisZ,int X0,int Y0,int X1,int Y1) {

   double ssxx,ssxy,avgx,avgy,a,b,y0,y1,x0,x1;
   int i,vn,n=VX->N<VY->N?VX->N:VY->N;

   ssxx=ssxy=avgx=avgy=vn=0.0;
   for(i=0;i<n;i++) {
      if (VX->V[i]!=VX->NoData && VY->V[i]!=VY->NoData) {
         avgx+=VX->V[i];
         avgy+=VY->V[i];
         ssxx+=VX->V[i]*VX->V[i];
         ssxy+=VX->V[i]*VY->V[i];
         vn++;
      }
   }
   avgx/=vn;
   avgy/=vn;
   ssxx-=vn*avgx*avgx;
   ssxy-=vn*avgx*avgy;
   b=ssxy/ssxx;
   a=avgy-(ssxy/ssxx)*avgx;

   VECTORMIN(VX,x0);
   VECTORMAX(VX,x1);

   y0=a+b*x0;
   y1=a+b*x1;

   y0=Y0+AXISVALUE(AxisY,y0);
   y1=Y0+AXISVALUE(AxisY,y1);

   x0=X0+AXISVALUE(AxisX,x0);
   x1=X0+AXISVALUE(AxisX,x1);

   V[0][0]=x0; V[0][1]=y0; V[0][2]=0.0;
   V[1][0]=x1; V[1][1]=y1; V[1][2]=0.0;

   return(2);
}

int GraphItem_FitGauss(Vect3d *V,TVector *VX,TVector *VY,TGraphAxis *AxisX,TGraphAxis *AxisY,TGraphAxis *AxisZ,int X0,int Y0,int X1,int Y1) {

   double varx,vary,avgx,avgy,o,x,x0,x1,y1,d;
   int i,n=VX->N<VY->N?VX->N:VY->N;

   avgx=avgy=y1=0.0;
   for(i=0;i<n;i++) {
      avgx+=VX->V[i];
      avgy+=VY->V[i];
      y1=y1<VY->V[i]?VY->V[i]:y1;
   }
   avgx/=n;
   avgy/=n;

   varx=vary=0;
   for(i=0;i<n;i++) {
      varx+=(VX->V[i]-avgx)*(VX->V[i]-avgx);
      vary+=(VY->V[i]-avgy)*(VY->V[i]-avgy);
   }
   varx/=n;
   vary/=n;
   o=sqrt(varx);

   VECTORMIN(VX,x0);
   VECTORMAX(VX,x1);

   i=0;
   for (x=x0;x<=x1;x+=(x1-x0)/n+1.0) {
      d=(x-avgx)/o;
      V[i][0]=X0+AXISVALUE(AxisX,x);
      V[i][1]=Y0+AXISVALUE(AxisY,1.0/(sqrt(2*3.1416)*o)*exp(-0.5*(d*d))*y1*avgx);
      V[i][2]=0.0;
      i++;
   }

   return(i);
}

/*--------------------------------------------------------------------------------------------------------------
 * Nom          : <GraphItem_Display2DTexture>
 * Creation     : Mai 2005 - J.P. Gauthier - CMC/CMOE
 *
 * But          : Afficher l'item de graph dans le graph (champs en texture)
 *
 * Parametres   :
 *   <Interp>   : Interpreteur Tcl
 *   <Graph>    : Item graph
 *   <Item>     : Item de graph
 *   <AxisX>    : Axe en X
 *   <AxisY>    : Axe en Y
 *   <AxisZ>    : Axe en Z
 *   <Data>     : Champs
 *   <X0>       : Limite inferieure gauche du graph
 *   <Y0>       : Limite inferieure gauche du graph
 *   <X1>       : Limite superieur  droite du graph
 *   <Y1>       : Limite superieur  droite du graph
 *
 * Retour       :
 *
 * Remarques :
 *
 *---------------------------------------------------------------------------------------------------------------
*/
void GraphItem_Display2DTexture(Tcl_Interp *Interp,GraphItem *Graph,TGraphAxis *AxisX,TGraphAxis *AxisY,TGraphAxis *AxisZ,TData *Data,int X0,int Y0,int X1,int Y1){

   int    i,j,c0,c1,c2,c3,idx0,idx3;
   int    depth=0,base=0;
   Vect3d g0,g1,g2,g3,min,max;
   double v0,v1,v2,v3,vf;
   int    dx,dy;

   if (!Data || !Data->Spec->Map)
      return;

   if (GLRender->Resolution>1) {
      return;
   }

   if (GLRender->GLDebug) {
      glPolygonMode(GL_FRONT_AND_BACK,GL_LINE);
   } else {
      glPolygonMode(GL_FRONT_AND_BACK,GL_FILL);
   }

   /*Do we need transparency*/
   if (Data->Spec->Map->Color[1][3]<255) {
      glEnable(GL_BLEND);
   }

   if (Data->Spec->InterNb) {
      VAL2COL(base,Data->Spec,Data->Spec->Inter[0]);
   }

   c0=c1=c2=c3=0;
   v0=v1=v2=v3=0.0;
   
   /*Process gridpoints*/
   for(j=0;j<Data->Def->NJ-1;j++) {

      glBegin(GL_QUADS);
      depth=0;

      for(i=0;i<Data->Def->NI;i++) {

         if (i!=0) {
            v1=v0;
            v2=v3;
            c1=c0;
            c2=c3;
            Vect_Assign(g1,g0);
            Vect_Assign(g2,g3);
         }

         idx0=j*Data->Def->NI+i;
         idx3=idx0+Data->Def->NI;

         Def_GetMod(Data->Def,idx0,v0);
         Def_GetMod(Data->Def,idx3,v3);
         VAL2COL(c0,Data->Spec,v0);
         VAL2COL(c3,Data->Spec,v3);

         vf=Data->GRef->Grid[0]=='V'?((Data->Spec->ZType!=LVL_UNDEF && Data->GRef->Hgt)?Data->GRef->Hgt[idx0]:Data->ZRef->Levels[j]):j;
         g0[0]=X0+AXISVALUE(AxisX,(i));
         g0[1]=Y0+AXISVALUE(AxisY,vf);
         g0[2]=0.0;
         vf=Data->GRef->Grid[0]=='V'?((Data->Spec->ZType!=LVL_UNDEF && Data->GRef->Hgt)?Data->GRef->Hgt[idx3]:Data->ZRef->Levels[j+1]):j+1;
         g3[0]=X0+AXISVALUE(AxisX,(i));
         g3[1]=Y0+AXISVALUE(AxisY,vf);
         g3[2]=0.0;

         /*Is the quad valid ???*/
         if (i && (c0>-1 || c1>-1 || c2>-1 || c3>-1)) {

            /* Is the quad visible ???*/
            Vect_Assign(min,g0);
            Vect_Assign(max,g0);
            Vect_Min(min,min,g1);
            Vect_Max(max,max,g1);
            Vect_Min(min,min,g2);
            Vect_Max(max,max,g2);
            Vect_Min(min,min,g3);
            Vect_Max(max,max,g3);

            if (VOUT(min[0],max[0],X0,X1) || VOUT(min[1],max[1],Y0,Y1)) {
               continue;
            }

            if (depth==0) {
               dx=fabs(g2[0]-g0[0]);
               dy=fabs(g2[1]-g0[1]);
               dx=Data->Spec->InterNb?MAX(dx,dy):MIN(dx,dy);
               depth=ceil(LOG2(dx));
            }

            if (Data->Spec->InterpDegree[0]=='N') {
               FFCellQuadNearest(Data->Spec,g0,g1,g2,g3,c0,c1,c2,c3,base);
            } else {
               /*Is the quad resolution enough ???*/
               if (depth>=1 && ((c0!=c1) || (c1!=c2) || (c2!=c3) || (c3!=c0))) {
                  FFCellQuadLinear(Data->Spec,g0,g1,g2,g3,c0,c1,c2,c3,v0,v1,v2,v3,depth,base);
               } else {
                  glColor4ubv(Data->Spec->Map->Color[c0<0?base:c0]);glVertex3dv(g0);
                  glColor4ubv(Data->Spec->Map->Color[c1<0?base:c1]);glVertex3dv(g1);
                  glColor4ubv(Data->Spec->Map->Color[c2<0?base:c2]);glVertex3dv(g2);
                  glColor4ubv(Data->Spec->Map->Color[c3<0?base:c3]);glVertex3dv(g3);
               }
            }
         }
      }
      glEnd();
   }
   glDisable(GL_BLEND);
}

/*--------------------------------------------------------------------------------------------------------------
 * Nom          : <GraphItem_Display2DTextureShader>
 * Creation     : Mai 2005 - J.P. Gauthier - CMC/CMOE
 *
 * But          : Afficher l'item de graph dans le graph (champs en texture)
 *
 * Parametres   :
 *   <Interp>   : Interpreteur Tcl
 *   <Graph>    : Item graph
 *   <Item>     : Item de graph
 *   <AxisX>    : Axe en X
 *   <AxisY>    : Axe en Y
 *   <AxisZ>    : Axe en Z
 *   <Data>     : Champs
 *   <X0>       : Limite inferieure gauche du graph
 *   <Y0>       : Limite inferieure gauche du graph
 *   <X1>       : Limite superieur  droite du graph
 *   <Y1>       : Limite superieur  droite du graph
 *
 * Retour       :
 *
 * Remarques :
 *
 *---------------------------------------------------------------------------------------------------------------
*/
void GraphItem_Display2DTextureShader(Tcl_Interp *Interp,GraphItem *Graph,TGraphAxis *AxisX,TGraphAxis *AxisY,TGraphAxis *AxisZ,TData *Data,int X0,int Y0,int X1,int Y1){

   int     i,j,n;
   Vect3d  g0,g1;
   float   min,rng,inter[DATASPEC_MAX];
   char   *ptr;
   double  vf;

   GLuint      tx[3];
   GLhandleARB prog;

   if (!Data || !Data->Spec->Map)
      return;

   if (GLRender->Resolution>2) {
      return;
   }

   glPolygonMode(GL_FRONT_AND_BACK,GL_FILL);

   /*Do we need transparency*/
   if (Data->Spec->Map->Alpha || Data->Spec->Alpha<100) {
      glEnable(GL_BLEND);
   }
   glColor4f(1.0,1.0,1.0,Data->Spec->Alpha/100.0);

   min=Data->Spec->Min;
   rng=Data->Spec->Max-Data->Spec->Min;

//   prog=GLShader_Load("/home/afsr/005/eer_Tools/LibTkGL/TclData","FieldTex");
   prog=GLRender->Prog[PROG_FIELDTEX];
   glUseProgramObjectARB(prog);
   glGenTextures(3,tx);
   Def_PointerMode(Data->Def,0,ptr);

   /*Setup 1D Colormap Texture*/
   glActiveTexture(GL_TEXTURE0);
   glBindTexture(GL_TEXTURE_1D,tx[0]);
   glTexParameteri(GL_TEXTURE_1D,GL_TEXTURE_MIN_FILTER,GL_NEAREST);
   glTexParameteri(GL_TEXTURE_1D,GL_TEXTURE_MAG_FILTER,GL_NEAREST);
   glTexParameteri(GL_TEXTURE_1D,GL_TEXTURE_WRAP_S,GL_CLAMP_TO_EDGE);
   glTexImage1D(GL_TEXTURE_1D,0,GL_RGBA,Data->Spec->Map->NbPixels,0,GL_RGBA,GL_UNSIGNED_BYTE,Data->Spec->Map->Color);

   /*Setup 1D Interval Texture*/
   glBindTexture(GL_TEXTURE_RECTANGLE_ARB,tx[1]);
   glActiveTexture(GL_TEXTURE1);
   if (Data->Spec->InterNb) {
      glTexParameteri(GL_TEXTURE_RECTANGLE_ARB,GL_TEXTURE_MIN_FILTER,GL_NEAREST);
      glTexParameteri(GL_TEXTURE_RECTANGLE_ARB,GL_TEXTURE_MAG_FILTER,GL_NEAREST);

      for(n=0;n<Data->Spec->InterNb;n++) inter[n]=Data->Spec->Inter[n];
      glTexImage2D(GL_TEXTURE_RECTANGLE_ARB,0,GL_FLOAT_R32_NV,Data->Spec->InterNb,1,0,GL_LUMINANCE,GL_FLOAT,inter);
   }

   /*Setup 2D Data Texture*/
   glActiveTexture(GL_TEXTURE2);
   glBindTexture(GL_TEXTURE_RECTANGLE_ARB,tx[2]);
   glTexParameteri(GL_TEXTURE_RECTANGLE_ARB,GL_TEXTURE_MIN_FILTER,GL_NEAREST);
   glTexParameteri(GL_TEXTURE_RECTANGLE_ARB,GL_TEXTURE_MAG_FILTER,GL_NEAREST);
//   glTexImage2D(GL_TEXTURE_RECTANGLE_ARB,0,GL_FLOAT_R32_NV,Data->Def->NI,Data->Def->NJ,0,GL_LUMINANCE,GL_FLOAT,ptr);
   glTexImage2D(GL_TEXTURE_RECTANGLE_ARB,0,GLRender->Vendor==ATI?GL_INTENSITY_FLOAT32_ATI:GL_FLOAT_R32_NV,Data->Def->NI,Data->Def->NJ,0,GL_LUMINANCE,GL_FLOAT,ptr);

   glUniform1iARB(GLShader_UniformGet(prog,"Colormap"),0);
   glUniform1iARB(GLShader_UniformGet(prog,"Interval"),1);
   glUniform1iARB(GLShader_UniformGet(prog,"Data"),2);
   glUniform1iARB(GLShader_UniformGet(prog,"Mask"),3);
   glUniform1iARB(GLShader_UniformGet(prog,"IsMask"),0);
   glUniform1fARB(GLShader_UniformGet(prog,"Cylindric"),-999.0);
   glUniform1fARB(GLShader_UniformGet(prog,"Min"),min);
   glUniform1fARB(GLShader_UniformGet(prog,"Range"),rng);
   glUniform1iARB(GLShader_UniformGet(prog,"Nb"),Data->Spec->InterNb);
   glUniform1iARB(GLShader_UniformGet(prog,"Bi"),(Data->Spec->InterpDegree[0]=='N'?0:1));
   glUniform1iARB(GLShader_UniformGet(prog,"Above"),Data->Spec->MapAbove);
   glUniform1iARB(GLShader_UniformGet(prog,"Bellow"),Data->Spec->MapBellow);

   for(j=0;j<Data->Def->NJ-1;j++) {
      glBegin(GL_QUAD_STRIP);
      for(i=0;i<Data->Def->NI;i++) {
         vf=Data->GRef->Grid[0]=='V'?((Data->Spec->ZType!=LVL_UNDEF && Data->GRef->Hgt)?Data->GRef->Hgt[j*Data->Def->NI+i]:Data->ZRef->Levels[j]):j;
         g0[0]=X0+AXISVALUE(AxisX,(i));
         g0[1]=Y0+AXISVALUE(AxisY,vf);
         g0[2]=0.0;

         vf=Data->GRef->Grid[0]=='V'?((Data->Spec->ZType!=LVL_UNDEF && Data->GRef->Hgt)?Data->GRef->Hgt[(j+1)*Data->Def->NI+i]:Data->ZRef->Levels[j+1]):j+1;
         g1[0]=X0+AXISVALUE(AxisX,(i));
         g1[1]=Y0+AXISVALUE(AxisY,vf);
         g1[2]=0.0;
         glTexCoord2f((float)i+0.5,(float)j+0.5);
         glVertex3dv(g0);
         glTexCoord2f((float)i+0.5,(float)j+1+0.5);
         glVertex3dv(g1);
      }
      glEnd();
   }

   glDeleteTextures(3,tx);
   glUseProgramObjectARB(0);
//   GLShader_UnInstall(prog);

   glActiveTexture(GL_TEXTURE0);
   glDisable(GL_BLEND);
}

/*--------------------------------------------------------------------------------------------------------------
 * Nom          : <GraphItem_Display2DContour>
 * Creation     : Mai 2005 - J.P. Gauthier - CMC/CMOE
 *
 * But          : Afficher l'item de graph dans le graph (champs en contour)
 *
 * Parametres   :
 *   <Interp>   : Interpreteur Tcl
 *   <Graph>    : Item graph
 *   <Item>     : Item de graph
 *   <AxisX>    : Axe en X
 *   <AxisY>    : Axe en Y
 *   <AxisZ>    : Axe en Z
 *   <Data>     : Champs
 *   <X0>       : Limite inferieure gauche du graph
 *   <Y0>       : Limite inferieure gauche du graph
 *   <X1>       : Limite superieur  droite du graph
 *   <Y1>       : Limite superieur  droite du graph
 *
 * Retour       :
 *
 * Remarques :
 *
 *---------------------------------------------------------------------------------------------------------------
*/
void GraphItem_Display2DContour(Tcl_Interp *Interp,GraphItem *Graph,TGraphAxis *AxisX,TGraphAxis *AxisY,TGraphAxis *AxisZ,TData *Data,int X0,int Y0,int X1,int Y1) {

   int       c;
   char      buf[256];
   TList    *list;
   T3DArray *array;
   Vect3d    v;

   if (!Data)
      return;

   /*Les contours sont-ils definit*/
   if (Data->Spec->InterNb && (!Data->Def->Segments || Data->Def->NK==1)) {
      FFContour(REF_GRID,Data->GPos,Data->Def,Data->Stat,NULL,Data->Spec->InterNb,Data->Spec->Inter,3,0);
   }

   if (Data->Spec->RenderLabel && Interp)
      Tcl_AppendResult(Interp,"gsave\n",(char*)NULL);

   if (Data->Spec->RenderLabel)
      GraphItem_Display2DLabel(Interp,Graph,AxisX,AxisY,AxisZ,Data,X0,Y0,X1,Y1);

   if (Data->Def->Segments) {

      if (Interp) {
         sprintf(buf,"%% Postscript des contours\n%i setlinewidth 1 setlinecap 1 setlinejoin\n",Data->Spec->Width-1);
         Tcl_AppendResult(Interp,buf,(char*)NULL);
         Tk_CanvasPsColor(Interp,Graph->canvas,Data->Spec->Outline);
      } else {
         glColor3us(Data->Spec->Outline->red,Data->Spec->Outline->green,Data->Spec->Outline->blue);
         glLineWidth(Data->Spec->Width);
      }

      if (Interp) {
         glPostscriptDash(Interp,&Data->Spec->Dash,Data->Spec->Width);
      } else {
         glDash(&Data->Spec->Dash);
      }

      /*Afficher les segments*/

      list=Data->Def->Segments;

      while(list) {
         array=(T3DArray*)list->Data;

         DataSpec_ColorSet(NULL,Data->Spec,array->Value);
         glBegin(GL_LINE_STRIP);
         for(c=0;c<array->Size;c++) {
            GraphItem_VectorPlace(Data,AxisX,AxisY,AxisZ,X0,Y0,array->Data[c],v);
            if (Interp) {
              if (c) {
                sprintf(buf,"%.15g %.15g lineto\n",v[0],v[1]);
              } else {
                sprintf(buf,"%.15g %.15g moveto\n",v[0],v[1]);
              }
              Tcl_AppendResult(Interp,buf,(char*)NULL);
            } else {
               glVertex3dv(v);
            }
         }
         glEnd();

         if (Interp) {
            Tcl_AppendResult(Interp,"stroke\n",(char*)NULL);
         }
         list=list->Next;
      }
      if (Interp) {
         glPostscriptDash(Interp,NULL,Data->Spec->Width);
      }
      glDisable(GL_LINE_STIPPLE);
   }

   if (Data->Spec->RenderLabel && Interp)
      Tcl_AppendResult(Interp,"grestore\n",(char*)NULL);
}

/*--------------------------------------------------------------------------------------------------------------
 * Nom          : <GraphItem_Display2DGrid>
 * Creation     : Janvier 2013 2005 - J.P. Gauthier - CMC/CMOE
 *
 * But          : Afficher l'item de graph dans le graph (point de grillesr)
 *
 * Parametres   :
 *   <Interp>   : Interpreteur Tcl
 *   <Graph>    : Item graph
 *   <Item>     : Item de graph
 *   <AxisX>    : Axe en X
 *   <AxisY>    : Axe en Y
 *   <AxisZ>    : Axe en Z
 *   <Data>     : Champs
 *   <X0>       : Limite inferieure gauche du graph
 *   <Y0>       : Limite inferieure gauche du graph
 *   <X1>       : Limite superieur  droite du graph
 *   <Y1>       : Limite superieur  droite du graph
 *
 * Retour       :
 *
 * Remarques :
 *
 *---------------------------------------------------------------------------------------------------------------
*/
void GraphItem_Display2DGrid(Tcl_Interp *Interp,GraphItem *Graph,TGraphAxis *AxisX,TGraphAxis *AxisY,TGraphAxis *AxisZ,TData *Data,int X0,int Y0,int X1,int Y1) {

   int       i,j;
   char      buf[256];
   Vect3d    pin,pout;

   if (!Data)
      return;

   if (!Data->Spec->Outline)
      return;

   if (Interp) {
      glFeedbackInit(FSIZE2D(Data->Def)*3,GL_2D);
      sprintf(buf,"%% Postscript de la grille\n%.2f setlinewidth 1 setlinecap 1 setlinejoin\n",Data->Spec->RenderGrid-0.5);
      Tcl_AppendResult(Interp,buf,(char*)NULL);
      Tk_CanvasPsColor(Interp,Graph->canvas,Data->Spec->Outline);
   } else {
      glPointSize(Data->Spec->RenderGrid+0.1);
      glColor3us(Data->Spec->Outline->red,Data->Spec->Outline->green,Data->Spec->Outline->blue);
   }

   /*Afficher les points*/
   glBegin(GL_POINTS);
   for(i=0;i<Data->Def->NI;i++) {
      for(j=0;j<Data->Def->NJ;j++) {
         pin[0]=i;
         pin[1]=j;
         GraphItem_VectorPlace(Data,AxisX,AxisY,AxisZ,X0,Y0,pin,pout);
         glVertex3dv(pout);
      }
   }
   glEnd();

   if (Interp)
      glFeedbackProcess(Interp,GL_2D);

}

/*--------------------------------------------------------------------------------------------------------------
 * Nom          : <GraphItem_Display2DStream>
 * Creation     : Mai 2005 - J.P. Gauthier - CMC/CMOE
 *
 * But          : Afficher l'item de graph dans le graph (champs en lignes de courants)
 *
 * Parametres   :
 *   <Interp>   : Interpreteur Tcl
 *   <Graph>    : Item graph
 *   <Item>     : Item de graph
 *   <AxisX>    : Axe en X
 *   <AxisY>    : Axe en Y
 *   <AxisZ>    : Axe en Z
 *   <Data>     : Champs
 *   <X0>       : Limite inferieure gauche du graph
 *   <Y0>       : Limite inferieure gauche du graph
 *   <X1>       : Limite superieur  droite du graph
 *   <Y1>       : Limite superieur  droite du graph
 *
 * Retour       :
 *
 * Remarques :
 *
 *---------------------------------------------------------------------------------------------------------------
*/
void GraphItem_Display2DStream(Tcl_Interp *Interp,GraphItem *Graph,TGraphAxis *AxisX,TGraphAxis *AxisY,TGraphAxis *AxisZ,TData *Data,int X0,int Y0,int X1,int Y1) {

   double i,j,dt,v0,v1;
   int    b,f,len,dz,c;
   float  step;
   Vect3d pix,*vbuf;

   if (!Data || !Data->Def->Data[1] || Data->Def->Dir || !Data->Spec->Width || !Data->Spec->Outline)
      return;

   if (GLRender->Resolution>2) {
      return;
   }

   glMatrixMode(GL_TEXTURE);
   if (GLRender->Delay<GL_STOP) {
      Data->Spec->TexStep+=0.01;
      Data->Spec->TexStep=Data->Spec->TexStep>1.0?0.0:Data->Spec->TexStep;
  }

   glEnable(GL_STENCIL_TEST);
   glStencilMask(0x2);
   glStencilFunc(GL_NOTEQUAL,0x2,0x2);
   glStencilOp(GL_KEEP,GL_KEEP,GL_REPLACE);
   glReadBuffer(GL_STENCIL);

   glEnableClientState(GL_VERTEX_ARRAY);
   glEnableClientState(GL_TEXTURE_COORD_ARRAY);
   glColor3us(Data->Spec->Outline->red,Data->Spec->Outline->green,Data->Spec->Outline->blue);
   glEnable(GL_BLEND);

   /*Setup 1D Texture*/
   glEnable(GL_TEXTURE_1D);
   glTexCoordPointer(1,GL_FLOAT,0,(const GLvoid *)FFStreamMapSetup1D(0.25));

   dz=Data->Spec->Sample*10;
   dt=0.0;
   len=512;

   vbuf=VBuffer_Alloc(len*2+1);

   /*Recuperer les latlon des pixels sujets*/
   for (pix[0]=X0;pix[0]<X1;pix[0]+=dz) {
      for (pix[1]=Y0;pix[1]<Y1;pix[1]+=dz) {

         i=AXISPPOS(AxisX,pix[0]);
         j=AXISPPOS(AxisY,pix[1]);

         /*Get the cell resolution, if not the same, to use as step size for a constant spacing*/
         v0=fabs(pix[0]-AXISVALUE(AxisX,i+1));
         v1=fabs(pix[1]-AXISVALUE(AxisY,j+1));
         dt=v1;
         step=1.0/v0;
         
         /*Get the streamline */
         if ((j=Graph_Pixel2Grid(Data,i,j))>=0) {
            b=FFStreamLine(Data->GRef,Data->ZRef,Data->Def,NULL,vbuf,NULL,i,j,Data->Def->Level,len,-step,Data->Spec->Min,0,REF_GRID,0);
            f=FFStreamLine(Data->GRef,Data->ZRef,Data->Def,NULL,&vbuf[len],NULL,i,j,Data->Def->Level,len,step,Data->Spec->Min,0,REF_GRID,0);

            /* If we have at least some part of it */
            if (b+f>10) {
               glPushMatrix();
               /*Translate the texture to mix the zoids*/
               glTranslatef(Data->Spec->TexStep-(dt+=0.15),0.0,0.0);

               /*Place streamlin within graph*/
               for(c=len-b;c<len+f;c++) {
                  GraphItem_VectorPlace(Data,AxisX,AxisY,AxisZ,X0,Y0,vbuf[c],vbuf[c]);
               }
               glLineWidth(Data->Spec->Width);
               glColorMask(GL_TRUE,GL_TRUE,GL_TRUE,GL_TRUE);
               glVertexPointer(3,GL_DOUBLE,0,&vbuf[len-b]);

               glDrawArrays(GL_LINE_STRIP,0,b+f);

               glLineWidth(8*Data->Spec->Width);
               glColorMask(GL_FALSE,GL_FALSE,GL_FALSE,GL_FALSE);
               glDrawArrays(GL_LINE_STRIP,0,b+f);

               glPopMatrix();
            }
         }
      }
   }

   glClear(GL_STENCIL_BUFFER_BIT);
   glStencilMask(0xf);
   glStencilFunc(GL_EQUAL,0x0,0xf);
   glStencilOp(GL_KEEP,GL_KEEP,GL_KEEP);
   glColorMask(GL_TRUE,GL_TRUE,GL_TRUE,GL_TRUE);
   glDisable(GL_TEXTURE_1D);
   glDisableClientState(GL_VERTEX_ARRAY);
   glDisableClientState(GL_TEXTURE_COORD_ARRAY);
}

/*----------------------------------------------------------------------------
 * Nom      : <GraphItem_Display2DLabel>
 * Creation : Mai 2005 - J.P. Gauthier - CMC/CMOE
 *
 * But      : Effectue l'affichage des labels des niveaux.
 *
 * Parametres :
 *  <Interp>  : Interpreteur TCL
 *  <Gr>      : Graph item
 *  <Field>   : Champsh
 *
 * Retour:
 *
 * Remarques :
 *
 *----------------------------------------------------------------------------
*/
void GraphItem_Display2DLabel(Tcl_Interp *Interp,GraphItem *Graph,TGraphAxis *AxisX,TGraphAxis *AxisY,TGraphAxis *AxisZ,TData *Data,int X0,int Y0,int X1,int Y1) {

   int            n,delta;
   char           buf[256];
   double         th,dx,dy,d;
   TList         *list;
   T3DArray      *array;
   Vect3d         p1,p0;
   Tk_FontMetrics tkm;
   GLuint         s;

   if (GLRender->Resolution>1 || !Data->Spec->Font || !Data->Spec->Outline || !Data->Spec->InterNb) {
      return;
   }

   Tk_GetFontMetrics(Data->Spec->Font,&tkm);
   dy=tkm.ascent*0.5;

   glReadBuffer(GL_BACK);
   glEnable(GL_STENCIL_TEST);
   glStencilMask(0x4);
   glStencilFunc(GL_ALWAYS,0x4,0x4);
   glStencilOp(GL_REPLACE,GL_REPLACE,GL_REPLACE);
   glPolygonMode(GL_FRONT,GL_FILL);

   /* Render the contours */
   if (Data->Def->Segments) {

      if (Interp) {
         Tk_CanvasPsColor(Interp,Graph->canvas,Data->Spec->Outline);
         Tk_CanvasPsFont(Interp,Graph->canvas,Data->Spec->Font);
         Tcl_AppendResult(Interp,"clippath\n",(char*)NULL);
      } else {
         glColor3us(Data->Spec->Outline->red,Data->Spec->Outline->green,Data->Spec->Outline->blue);
         glFontUse(Tk_Display(Tk_CanvasTkwin(Graph->canvas)),Data->Spec->Font);
      }

      list=Data->Def->Segments;
      delta=Data->Spec->RenderLabel*100;

      while(list) {
         array=(T3DArray*)list->Data;
         if (array->Size<10) {
            list=list->Next;
            continue;
         }

         DataSpec_Format(Data->Spec,array->Value,buf);
         dx=Tk_TextWidth(Data->Spec->Font,buf,strlen(buf));

         DataSpec_ColorSet(NULL,Data->Spec,array->Value);

         /* Check if we need to caclulate this streamline. Will it be visible */
         GraphItem_VectorPlace(Data,AxisX,AxisY,AxisZ,X0,Y0,array->Data[0],p0);
         d=delta;

         for(n=1;n<array->Size;n++) {
            GraphItem_VectorPlace(Data,AxisX,AxisY,AxisZ,X0,Y0,array->Data[n],p1);

            d+=hypot(p1[0]-p0[0],p1[1]-p0[1]);
            if (Data->Spec->RenderLabel<0 || d>=delta) {

               /*We dont want overlapping label so check the stencil buffer*/
               glReadPixels(p1[0],p1[1],1,1,GL_STENCIL_INDEX,GL_UNSIGNED_INT,&s);
               if (s&0x4) {
                  continue;
               }

               /*Calculate the angle of the segment*/
               th=-atan2(p1[0]-p0[0],p1[1]-p0[1])+M_PI2;
               if (th<-M_PI2) th+=M_PI;
               if (th>M_PI2)  th-=M_PI;

               /*We have to translate along the rotation axis to center the label*/
               p1[0]-=dx*0.5*cos(th)-dy*0.5*cos(M_PI2-th);
               p1[1]-=dx*0.5*sin(th)+dy*0.5*sin(M_PI2-th);
               th=RAD2DEG(th);

               /*Draw the bloc in the stencil buffer*/
               glStencilMaskQuad(p1[0],p1[1],dx,dy,th,4,1);
               glDisable(GL_STENCIL_TEST);

               if (Interp) {
                  glPostscriptTextBG(Interp,Graph->canvas,p1[0],p1[1],th,dx,dy,4,1,Graph->BGColor,1);
               }

               /*Draw the text*/
               glPrint(Interp,Graph->canvas,buf,p1[0],p1[1],th);
               glEnable(GL_STENCIL_TEST);
               d=0;
            }
            if (Data->Spec->RenderLabel<0) {
               break;
            }

            Vect_Assign(p0,p1);
         }
         list=list->Next;
      }
      if (Interp) {
         Tcl_AppendResult(Interp,"clip newpath\n",(char*)NULL);
      }
   }

   glStencilMask(0xff);
   glStencilFunc(GL_EQUAL,0x0,0xff);
   glStencilOp(GL_KEEP,GL_KEEP,GL_KEEP);
}

/*----------------------------------------------------------------------------
 * Nom      : <GraphItem_Display2DVector>
 * Creation : Janvier 2007 - J.P. Gauthier - CMC/CMOE
 *
 * But      : Effectue l'affichage des valeurs en barbules et fleche.
 *
 * Parametres :
 *  <Interp>  : Interpreteur TCL
 *  <Gr>      : Graph item
 *  <Field>   : Champsh
 *
 * Retour:
 *
 * Remarques :
 *
 *----------------------------------------------------------------------------
*/
void GraphItem_Display2DVector(Tcl_Interp *Interp,GraphItem *Graph,TGraphAxis *AxisX,TGraphAxis *AxisY,TGraphAxis *AxisZ,TData *Data,int X0,int Y0,int X1,int Y1) {

   int    i,j,idx;
   double u,w,len,dir;
   Vect3d pin,pout;

   if (!Data->Def->Dir && !Data->Def->Data[2]) {
      return;
   }
            
   if (Interp) {
      Tcl_AppendResult(Interp,"%% Postscript des donnees vectorielles\n1 setlinewidth 0 setlinecap 0 setlinejoin\n",(char*)NULL);
      Tk_CanvasPsColor(Interp,Graph->canvas,Data->Spec->Outline);
   } else {
      glColor3us(Data->Spec->Outline->red,Data->Spec->Outline->green,Data->Spec->Outline->blue);
   }

   u=w=len=0.0;

   glMatrixMode(GL_MODELVIEW);
   glPolygonMode(GL_FRONT_AND_BACK,GL_FILL);
   glLineWidth(1.0);

   for(i=0;i<Data->Def->NI;i+=Data->Spec->Sample) {
      for(j=0;j<Data->Def->NJ;j+=Data->Spec->Sample) {
         idx=j*Data->Def->NI+i;
         Def_Get(Data->Def,0,idx,u);
         
         if (Data->Def->Dir) {
            // 2D vector (no vertical component)
            len=u;
            Def_Get(Data->Def,1,idx,dir);            
         } else {
            // 3D vector
            Def_Get(Data->Def,2,idx,w);
            Def_GetMod(Data->Def,idx,len);
            dir=-RAD2DEG(atan2(u,w));
         }
         if (len<=Data->Spec->Max && len>=Data->Spec->Min) {
            DataSpec_ColorSet(NULL,Data->Spec,len);

            // Position vector
            pin[0]=i;
            pin[1]=j;
            GraphItem_VectorPlace(Data,AxisX,AxisY,AxisZ,X0,Y0,pin,pout);

            // Calculate topo angle when using reprojected height (pressure or magl)
/*            if (Data->GRef->Grid[0]=='V' && Data->Spec->ZType!=LVL_UNDEF && Data->GRef->Hgt) {
               Vect3d ppout;
               pin[0]=i-1;
               pin[1]=j;
               GraphItem_VectorPlace(Data,AxisX,AxisY,AxisZ,X0,Y0,pin,ppout);
               da=i==0?0:-atan2(pout[1]-ppout[1],pout[0]-ppout[0]);
            }
*/          
            if (Interp) {
               glFeedbackInit(256,GL_2D);
               pout[1]=Tk_CanvasPsY(Graph->canvas,pout[1]);
            }
            Data_RenderBarbule(Data->Spec->RenderVector,1,0.0,pout[0],pout[1],0.0,len,dir,VECTORSIZE(Data->Spec,len),NULL);
            if (Interp) {
               glFeedbackProcess(Interp,GL_2D);
            }
         }
      }
   }
}

/*--------------------------------------------------------------------------------------------------------------
 * Nom          : <GraphItem_Dim>
 * Creation     : Mai 2005 - J.P. Gauthier - CMC/CMOE
 *
 * But          : Recuperer la largeur de la description d'un itme de graph
 *
 * Parametres   :
 *   <Item>     : Item de graph
 *   <Graph>    : graph
 *   <Width>    : Retour de la largeur de l'axe
 *   <Height>   : Retour de la hauteur de l'axe
 *
 * Retour       : Largeur du descriptif
 *
 * Remarques :
 *
 *---------------------------------------------------------------------------------------------------------------
*/
void GraphItem_Dim(Tk_Canvas Canvas,TGraphItem *Item,GraphItem *Graph,int *Width,int *Height) {

   if (Item->Desc) {
      Item->DescItem=Tk_glGetItem(Canvas,Tcl_NewStringObj(Item->Desc,-1));
      if (Item->Text) Tk_FreeTextLayout(Item->Text);

      if (Item->DescItem) {
         Item->DescWidth=((glTextItem*)Item->DescItem)->header.x2-((glTextItem*)Item->DescItem)->header.x1;
         Item->DescHeight=((glTextItem*)Item->DescItem)->header.y2-((glTextItem*)Item->DescItem)->header.y1;
      } else {
         Item->Text=Tk_ComputeTextLayout((Item->Font?Item->Font:Graph->Font),Item->Desc,Tcl_NumUtfChars(Item->Desc,strlen(Item->Desc)),0,TK_JUSTIFY_LEFT,0,&Item->DescWidth,&Item->DescHeight);
      }
      *Width=Item->DescWidth;
      *Height=Item->DescHeight;
   } else {
      *Width=0;
      *Height=0;
   }
}

/*--------------------------------------------------------------------------------------------------------------
 * Nom          : <GraphItem_Header>
 * Creation     : Mai 2005 - J.P. Gauthier - CMC/CMOE
 *
 * But          : Afficher l'entete de l'item de graph dans le graph
 *
 * Parametres   :
 *   <Interp>   : Interpreteur Tcl
 *   <Graph>    : Item graph
 *   <Item>     : Item de graph
 *   <X0>       : Limite inferieure gauche du graph
 *   <Y0>       : Limite inferieure gauche du graph
 *   <X1>       : Limite superieur  droite du graph
 *   <Y1>       : Limite superieur  droite du graph
 *
 * Retour       :
 *
 * Remarques :
 *
 *---------------------------------------------------------------------------------------------------------------
*/
int GraphItem_Header(Tcl_Interp *Interp,GraphItem *Graph,TGraphItem *Item,int X0,int Y0,int X1) {

   int    y;
   double sz;

   y=Y0+Item->DescHeight*0.5;

   if (Item->Fill) {
      if (Item->Stipple) {
         glEnable(GL_POLYGON_STIPPLE);
         glPolygonStipple(Item->Stipple->Data);
      }
      glColor4us(Item->Fill->red,Item->Fill->green,Item->Fill->blue,Item->Alpha*Graph->Alpha*0.01*655);
      glPolygonMode(GL_FRONT_AND_BACK,GL_FILL);
      glBegin(GL_QUADS);
         glVertex2i(X1,y-5);
         glVertex2i(X1+40,y-5);
         glVertex2i(X1+40,y+5);
         glVertex2i(X1,y+5);
      glEnd();
      glDisable(GL_POLYGON_STIPPLE);

      if (Item->Width && Item->Outline) {
         glDash(&Item->Dash);
         glLineWidth(Item->Width);
         glColor4us(Item->Outline->red,Item->Outline->green,Item->Outline->blue,Item->Alpha*Graph->Alpha*0.01*655);
         glPolygonMode(GL_FRONT_AND_BACK,GL_LINE);
         glBegin(GL_QUADS);
            glVertex2i(X1,y-5);
            glVertex2i(X1+40,y-5);
            glVertex2i(X1+40,y+5);
            glVertex2i(X1,y+5);
         glEnd();
         glDisable(GL_LINE_STIPPLE);
      }
   } else {

      if (Item->Width && Item->Outline) {
         glDash(&Item->Dash);
         glLineWidth(Item->Width);
         glColor4us(Item->Outline->red,Item->Outline->green,Item->Outline->blue,Item->Alpha*Graph->Alpha*0.01*655);

         glBegin(GL_LINES);
            glVertex2i(X1,y);
            glVertex2i(X1+40,y);
         glEnd();
         glDisable(GL_LINE_STIPPLE);
      }
   }

   if (Item->DescItem) {
      ((glTextItem*)Item->DescItem)->x=X0;
      ((glTextItem*)Item->DescItem)->y=Y0;
      glComputeTextBbox(Graph->canvas,(glTextItem*)Item->DescItem);
   } else {
      glColor4us(Graph->FGColor->red,Graph->FGColor->green,Graph->FGColor->blue,Item->Alpha*Graph->Alpha*0.01*655);
      glFontUse(Tk_Display(Tk_CanvasTkwin(Graph->canvas)),Item->Font?Item->Font:Graph->Font);
      glDisplayTextLayout(Item->Text,0,X0,Y0,0,-1,1);
   }

   if (Item->Icon && Item->Size>0.0) {
      glEnableClientState(GL_VERTEX_ARRAY);
      glVertexPointer(2,GL_DOUBLE,0,IconList[Item->Icon].Co);
      glPushMatrix();
      glTranslated(X1+20,y,0);
      sz=(Item->Size+Item->Width)*0.5;
      glScalef(sz,sz,1.0f);
      if (Item->IconFill) {
         glColor4us(Item->IconFill->red,Item->IconFill->green,Item->IconFill->blue,Item->Alpha*Graph->Alpha*0.01*655);
      } else {
         glColor4us(Graph->BGColor->red,Graph->BGColor->green,Graph->BGColor->blue,Item->Alpha*Graph->Alpha*0.01*655);
      }
      glPolygonMode(GL_FRONT_AND_BACK,GL_FILL);
      glDrawArrays(IconList[Item->Icon].Type,0,IconList[Item->Icon].Nb);

      if (Item->IconOutline && Item->Width) {
         glColor4us(Item->IconOutline->red,Item->IconOutline->green,Item->IconOutline->blue,Item->Alpha*Graph->Alpha*0.01*655);
         glPolygonMode(GL_FRONT_AND_BACK,GL_LINE);
         glDrawArrays(IconList[Item->Icon].Type,0,IconList[Item->Icon].Nb);
      }
      glPopMatrix();
      glDisableClientState(GL_VERTEX_ARRAY);
   }

   if (Item->Bitmap) {
      glColor4us(Item->Outline->red,Item->Outline->green,Item->Outline->blue,Item->Alpha*Graph->Alpha*0.01*655);
      trRasterPos2i((X1+20-((TkCanvas*)Graph->canvas)->xOrigin)-Item->Bitmap->Width/2,-(y-((TkCanvas*)Graph->canvas)->yOrigin)-Item->Bitmap->Height/2);
      glBitmap(Item->Bitmap->Width,Item->Bitmap->Height,0.0,0.0,0.0,0.0,(GLubyte *)Item->Bitmap->Data);
   }

   if (Item->ImageString) {
      Tk_PhotoImageBlock data;
      Tk_PhotoHandle     handle;
      GLubyte *pixel;

      handle=Tk_FindPhoto(((TkCanvas*)Graph->canvas)->interp,Item->ImageString);
      Tk_PhotoGetImage(handle,&data);

      /*We have to flip the image data along the Y axis*/
      if (!(pixel=(GLubyte*)malloc(data.width*data.height*data.pixelSize))) {
         App_Log(ERROR,"%s: Memory allocation error",__func__);
         return(0);
      }
         
      memcpy(pixel,data.pixelPtr,data.width*data.height*data.pixelSize);
      DataFlip(data.pixelPtr,pixel,data.width,data.height,data.pixelSize);

      glEnable(GL_BLEND);
      trRasterPos2i((X1+20-((TkCanvas*)Graph->canvas)->xOrigin)-data.width/2,-(y-((TkCanvas*)Graph->canvas)->yOrigin)-data.height/2);
      glDrawPixels(data.width,data.height,data.pixelSize==3?GL_RGB:GL_RGBA,GL_UNSIGNED_BYTE,pixel);
      glDisable(GL_BLEND);

      free(pixel);
   }
   return(Y0+Item->DescHeight+5);
}

/*--------------------------------------------------------------------------------------------------------------
 * Nom          : <GraphItem_Postscript>
 * Creation     : Mai 2005 - J.P. Gauthier - CMC/CMOE
 *
 * But          : Afficher l'item de graph dans le graph
 *
 * Parametres   :
 *   <Interp>   : Interpreteur Tcl
 *   <Graph>    : Item graph
 *   <Item>     : Item de graph
 *   <X0>       : Limite inferieure gauche du graph
 *   <Y0>       : Limite inferieure gauche du graph
 *   <X1>       : Limite superieur  droite du graph
 *   <Y1>       : Limite superieur  droite du graph
 *
 * Retour       :
 *
 * Remarques :
 *
 *---------------------------------------------------------------------------------------------------------------
*/
void GraphItem_Postscript(Tcl_Interp *Interp,GraphItem *Graph,TGraphItem *Item,int X0,int Y0,int X1,int Y1) {

   TData       *data;
   TGraphAxis  *axisx,*axisy,*axisz;
   GLXPbuffer   pbuf;
   unsigned int w,h;

   axisx=GraphAxis_Get(Item->XAxis);
   axisy=GraphAxis_Get(Item->YAxis);
   axisz=NULL;
   
   if (!axisx || !axisy) return;

   if (Item->Data) {

      if ((data=Data_Get(Item->Data))) {

         if (data->Spec->RenderTexture) {
            /* Setup the tile rendering engine */
            w=Graph->Width;
            h=Graph->Height;
            if (!(pbuf=glXGetPBuffer(Tk_CanvasTkwin(Graph->canvas),&w,&h))) {
               App_Log(ERROR,"%s: Unable to allocate rendering PBuffer",__func__);
               return;
            }

            if (!glXMakeContextCurrent(GLRender->XDisplay,pbuf,pbuf,GLRender->GLCon)) {
               App_Log(ERROR,"%s: Unable to link the pbuffer to the GLXContext",__func__);
               glXFreePBuffer(pbuf);
               return;
            }

            glPushAttrib(GL_TRANSFORM_BIT|GL_VIEWPORT_BIT);
            GLRender->TRCon=trNew();
            trTileSize(GLRender->TRCon,w,h,0);
            trImageSize(GLRender->TRCon,X1-X0,Y1-Y0);

            glMatrixMode(GL_PROJECTION);
            glLoadIdentity();
            glViewport(0,0,X1-X0,Y1-Y0);
            glClearColor(Graph->FillColor->red/65535.0f,Graph->FillColor->green/65535.0f,Graph->FillColor->blue/65535.0f,1.0f);
            trOrtho(GLRender->TRCon,0,X1-X0,0,Y1-Y0,-1,1);

            /* Render the tiles */
            do {
               trBeginTile(GLRender->TRCon);
               glClear(GL_COLOR_BUFFER_BIT);
               GraphItem_Display2DTexture(Interp,Graph,axisx,axisy,axisz,data,X0,Y0,X1,Y1);
               trPostscriptBuffer(Interp,GL_BACK,0,0,w,h,GLRender->TRCon);
            } while (trEndTile(GLRender->TRCon));

            trDelete(GLRender->TRCon);
            GLRender->TRCon=NULL;
            glPopAttrib();
            glXFreePBuffer(pbuf);
            SetglCanvas(Canvas(Graph->canvas));
         }

         if (data->Spec->RenderContour)
            GraphItem_Display2DContour(Interp,Graph,axisx,axisy,axisz,data,X0,Y0,X1,Y1);

         if (data->Spec->RenderGrid)
            GraphItem_Display2DGrid(Interp,Graph,axisx,axisy,axisz,data,X0,Y0,X1,Y1);

         if (data->Spec->RenderVector)
            GraphItem_Display2DVector(Interp,Graph,axisx,axisy,axisz,data,X0,Y0,X1,Y1);
      }
   }

   if (Item->XData && Item->YData) {

      if (Item->Type==MINMAX)
         GraphItem_PostscriptMinMax(Interp,Graph,Item,axisx,axisy,NULL,X0,Y0,X1,Y1);
      else if (Item->Type==BOXPLOT)
         GraphItem_PostscriptBox(Interp,Graph,Item,axisx,axisy,NULL,X0,Y0,X1,Y1);
      else
         GraphItem_PostscriptXYZ(Interp,Graph,Item,axisx,axisy,NULL,X0,Y0,X1,Y1);
   }
}

/*--------------------------------------------------------------------------------------------------------------
 * Nom          : <GraphItem_PostscriptMinMax>
 * Creation     : Novembre 2005 - J.P. Gauthier - CMC/CMOE
 *
 * But          : Afficher l'item de graph an format MinMax
 *
 * Parametres   :
 *   <Interp>   : Interpreteur Tcl
 *   <Graph>    : Item graph
 *   <Item>     : Item de graph
 *   <AxisX>    : Axe en X
 *   <AxisY>    : Axe en Y
 *   <AxisZ>    : Axe en Z
 *   <X0>       : Limite inferieure gauche du graph
 *   <Y0>       : Limite inferieure gauche du graph
 *   <X1>       : Limite superieur  droite du graph
 *   <Y1>       : Limite superieur  droite du graph
 *
 * Retour       :
 *
 * Remarques :
 *
 *---------------------------------------------------------------------------------------------------------------
*/

void GraphItem_PostscriptMinMax(Tcl_Interp *Interp,GraphItem *Graph,TGraphItem *Item,TGraphAxis *AxisX,TGraphAxis *AxisY,TGraphAxis *AxisZ,int X0,int Y0,int X1,int Y1) {

   TVector   *vec,*vec0,*vec1;
   Vect3d    *v=NULL;
   char       buf[256];
   int        i,vn;

   vec=Item->Orient[0]=='X'?Vector_Get(Item->XData):Vector_Get(Item->YData);

   if (!vec)
      return;

   /* Display Min and Max */
   vec0=Vector_Get(Item->MinData);
   vec1=Vector_Get(Item->MaxData);

   if (vec0 && vec0->N<=vec->N && vec1 && vec1->N<=vec->N && Item->Outline) {

      Tcl_AppendResult(Interp,"% Postscript du graph MinMax\n",(char*)NULL);

      if (!(v=(Vect3d*)malloc(2*vec->N*sizeof(Vect3d)))) {
         App_Log(ERROR,"%s: Memory allocation error",__func__);
         return;
      }
      vn=0;

      /* Compute graph curve points */
      for(i=0;i<vec->N;i++) {
         if (vec->V[i]!=vec->NoData && vec0->V[i]!=vec0->NoData) {
            if (Item->Orient[0]=='X') {
               v[vn][0]=X0+AXISVALUE(AxisX,vec->V[i]);
               v[vn][1]=Y0+AXISVALUE(AxisY,vec0->V[i]);
               v[vn][2]=0.0;
            } else {
               v[vn][0]=X0+AXISVALUE(AxisX,vec0->V[i]);
               v[vn][1]=Y0+AXISVALUE(AxisY,vec->V[i]);
               v[vn][2]=0.0;
            }
            vn++;
         }
      }

      for(i=0;i<vec->N;i++) {
         if (vec->V[vec->N-i-1]!=vec->NoData && vec0->V[vec->N-i-1]!=vec0->NoData) {
            if (Item->Orient[0]=='X') {
               v[vn][0]=X0+AXISVALUE(AxisX,vec->V[vec->N-i-1]);
               v[vn][1]=Y0+AXISVALUE(AxisY,vec1->V[vec->N-i-1]);
               v[vn][2]=0.0;
            } else {
               v[vn][0]=X0+AXISVALUE(AxisX,vec1->V[vec->N-i-1]);
               v[vn][1]=Y0+AXISVALUE(AxisY,vec->V[vec->N-i-1]);
               v[vn][2]=0.0;
            }
            vn++;
         }
      }

      /* Display graph filling */
      if (Item->Fill) {
         sprintf(buf,"%.15g %.15g moveto\n",v[0][0],v[0][1]);
         Tcl_AppendResult(Interp,buf,(char*)NULL);
         for(i=1;i<vn;i++) {
            sprintf(buf,"%.15g %.15g lineto\n",v[i][0],v[i][1]);
            Tcl_AppendResult(Interp,buf,(char*)NULL);
         }
         Tk_CanvasPsColor(Interp,Graph->canvas,Item->Fill);
         if (Item->Stipple) {
            Tcl_AppendResult(Interp,"eoclip ",(char*)NULL);
            glPostscriptStipple(Interp,NULL,Item->Stipple);
         } else {
            Tcl_AppendResult(Interp,"closepath fill\n",(char*)NULL);
         }
      }

      /* Display graph outline */
      if (Item->Outline && Item->Width) {
         sprintf(buf,"%.15g %.15g moveto\n",v[0][0],v[0][1]);
         Tcl_AppendResult(Interp,buf,(char*)NULL);
         for(i=1;i<vn;i++) {
            sprintf(buf,"%.15g %.15g lineto\n",v[i][0],v[i][1]);
            Tcl_AppendResult(Interp,buf,(char*)NULL);
         }
         Tk_CanvasPsColor(Interp,Graph->canvas,Item->Outline);
         glPostscriptDash(Interp,&Item->Dash,Item->Width);
         sprintf(buf,"%i setlinewidth 1 setlinecap 1 setlinejoin\nstroke\n",Item->Width);
         Tcl_AppendResult(Interp,buf,(char*)NULL);
         glPostscriptDash(Interp,NULL,Item->Width);
      }
   }
   free(v);
}

/*--------------------------------------------------------------------------------------------------------------
 * Nom          : <GraphItem_PostscriptBox>
 * Creation     : Novembre 2005 - J.P. Gauthier - CMC/CMOE
 *
 * But          : Afficher l'item de graph an format boxplot
 *
 * Parametres   :
 *   <Interp>   : Interpreteur Tcl
 *   <Graph>    : Item graph
 *   <Item>     : Item de graph
 *   <AxisX>    : Axe en X
 *   <AxisY>    : Axe en Y
 *   <AxisZ>    : Axe en Z
 *   <X0>       : Limite inferieure gauche du graph
 *   <Y0>       : Limite inferieure gauche du graph
 *   <X1>       : Limite superieur  droite du graph
 *   <Y1>       : Limite superieur  droite du graph
 *
 * Retour       :
 *
 * Remarques :
 *
 *---------------------------------------------------------------------------------------------------------------
*/
void GraphItem_PostscriptBox(Tcl_Interp *Interp,GraphItem *Graph,TGraphItem *Item,TGraphAxis *AxisX,TGraphAxis *AxisY,TGraphAxis *AxisZ,int X0,int Y0,int X1,int Y1) {

   TVector *vec,*vec0,*vec1;
   double  x[2],y[2];
   int     n;
   char    buf[256];
   double  rect[8];

   vec=Item->Orient[0]=='X'?Vector_Get(Item->XData):Vector_Get(Item->YData);

   if (!vec || !Item->Outline)
      return;

   Tcl_AppendResult(Interp,"% Postscript du graph BoxPlot\n",(char*)NULL);
   sprintf(buf,"%.15g setlinewidth 1 setlinecap 1 setlinejoin\n",(double)(Item->Width));
   Tcl_AppendResult(Interp,buf,(char*)NULL);

   /* Display Min and Max */
   vec0=Vector_Get(Item->MinData);
   vec1=Vector_Get(Item->MaxData);

   if (vec0 && vec0->N<=vec->N && vec1 && vec1->N<=vec->N) {
      Tk_CanvasPsColor(Interp,Graph->canvas,Item->Outline);
      for (n=0;n<vec->N;n++) {
         if (vec->V[n]!=vec->NoData && vec0->V[n]!=vec0->NoData && vec1->V[n]!=vec1->NoData) {
            if (Item->Orient[0]=='X') {
               x[0]=X0+AXISVALUE(AxisX,vec->V[n]);
               y[0]=Y0+AXISVALUE(AxisY,vec0->V[n]);
               y[1]=Y0+AXISVALUE(AxisY,vec1->V[n]);

               SETLINE(rect,x[0],y[0],x[0],y[1])
               Tk_glCanvasPsPath(Interp,Graph->canvas,rect,2);
               x[1]=x[0]+Item->Size;
               x[0]=x[0]-Item->Size;
               SETLINE(rect,x[0],y[0],x[1],y[0])
               Tk_glCanvasPsPath(Interp,Graph->canvas,rect,2);
               SETLINE(rect,x[0],y[1],x[1],y[1])
               Tk_glCanvasPsPath(Interp,Graph->canvas,rect,2);
            } else {
               x[0]=X0+AXISVALUE(AxisX,vec0->V[n]);
               x[1]=X0+AXISVALUE(AxisX,vec1->V[n]);
               y[0]=Y0+AXISVALUE(AxisY,vec->V[n]);

               SETLINE(rect,x[0],y[0],x[1],y[0])
               Tk_glCanvasPsPath(Interp,Graph->canvas,rect,2);
               y[1]=y[0]+Item->Size;
               y[0]=y[0]-Item->Size;
               SETLINE(rect,x[0],y[0],x[0],y[1])
               Tk_glCanvasPsPath(Interp,Graph->canvas,rect,2);
               SETLINE(rect,x[1],y[0],x[1],y[1])
               Tk_glCanvasPsPath(Interp,Graph->canvas,rect,2);
            }
         }
      }
   }
   Tcl_AppendResult(Interp,"stroke\n",(char*)NULL);

   /* Display High Low Centile bar */
   vec0=Vector_Get(Item->HighData);
   vec1=Vector_Get(Item->LowData);

   if (vec0 && vec0->N<=vec->N && vec1 && vec1->N<=vec->N) {
      if (Item->Fill) {
         Tk_CanvasPsColor(Interp,Graph->canvas,Item->Fill);
      } else {
         Tk_CanvasPsColor(Interp,Graph->canvas,Graph->BGColor);
      }
      for (n=0;n<vec->N;n++) {
         if (vec->V[n]!=vec->NoData && vec0->V[n]!=vec0->NoData && vec1->V[n]!=vec1->NoData) {
            if (Item->Orient[0]=='X') {
               x[0]=x[1]=X0+AXISVALUE(AxisX,vec->V[n]);
               y[0]=Y0+AXISVALUE(AxisY,vec0->V[n]);
               y[1]=Y0+AXISVALUE(AxisY,vec1->V[n]);
               x[0]-=Item->Size;
               x[1]+=Item->Size;
            } else {
               x[0]=X0+AXISVALUE(AxisX,vec0->V[n]);
               x[1]=X0+AXISVALUE(AxisX,vec1->V[n]);
               y[0]=y[1]=Y0+AXISVALUE(AxisY,vec->V[n]);
               y[0]-=Item->Size;
               y[1]+=Item->Size;

            }
            SETRECT(rect,x[0],y[0],x[1],y[1])
            Tk_glCanvasPsPath(Interp,Graph->canvas,rect,4);
         }
      }
      Tcl_AppendResult(Interp,"closepath fill\n",(char*)NULL);

      Tk_CanvasPsColor(Interp,Graph->canvas,Item->Outline);
      for (n=0;n<vec->N;n++) {
         if (vec->V[n]!=vec->NoData && vec0->V[n]!=vec0->NoData && vec1->V[n]!=vec1->NoData) {
            if (Item->Orient[0]=='X') {
               x[0]=x[1]=X0+AXISVALUE(AxisX,vec->V[n]);
               y[0]=Y0+AXISVALUE(AxisY,vec0->V[n]);
               y[1]=Y0+AXISVALUE(AxisY,vec1->V[n]);
               x[0]-=Item->Size;
               x[1]+=Item->Size;
            } else {
               x[0]=X0+AXISVALUE(AxisX,vec0->V[n]);
               x[1]=X0+AXISVALUE(AxisX,vec1->V[n]);
               y[0]=y[1]=Y0+AXISVALUE(AxisY,vec->V[n]);
               y[0]-=Item->Size;
               y[1]+=Item->Size;

            }
            SETRECT(rect,x[0],y[0],x[1],y[1])
            Tk_glCanvasPsPath(Interp,Graph->canvas,rect,4);
            Tcl_AppendResult(Interp,"closepath stroke\n",(char*)NULL);
         }
      }
   }

   /* Display Median */
   vec0=Vector_Get(Item->MedianData);

   if (vec0 && vec0->N<=vec->N ) {
      Tk_CanvasPsColor(Interp,Graph->canvas,Item->Outline);
      for (n=0;n<vec->N;n++) {
         if (vec->V[n]!=vec->NoData && vec0->V[n]!=vec0->NoData) {
            if (Item->Orient[0]=='X') {
               x[0]=X0+AXISVALUE(AxisX,vec->V[n]);
               y[0]=y[1]=Y0+AXISVALUE(AxisY,vec0->V[n]);
               x[1]=x[0]+Item->Size;
               x[0]-=Item->Size;
            } else {
               x[0]=x[1]=X0+AXISVALUE(AxisX,vec0->V[n]);
               y[0]=Y0+AXISVALUE(AxisY,vec->V[n]);
               y[1]=y[0]+Item->Size;
               y[0]-=Item->Size;
            }
            SETLINE(rect,x[0],y[0],x[1],y[1])
            Tk_glCanvasPsPath(Interp,Graph->canvas,rect,2);
         }
      }
      Tcl_AppendResult(Interp,"stroke\n",(char*)NULL);
   }

   /* Display Error */
   vec0=Vector_Get(Item->ErrorData);

   if (vec0 && vec0->N<=vec->N) {
      if (Item->Icon && Item->Size>0.0) {
         for (n=0;n<vec->N;n++) {
            if (vec->V[n]!=vec->NoData && vec0->V[n]!=vec0->NoData) {
               if (Item->Orient[0]=='X') {
                  x[0]=X0+AXISVALUE(AxisX,vec->V[n]);
                  y[0]=Y0+AXISVALUE(AxisY,vec0->V[n]);
               } else {
                  x[0]=X0+AXISVALUE(AxisX,vec0->V[n]);
                  y[0]=Y0+AXISVALUE(AxisY,vec->V[n]);
               }
               sprintf(buf,"gsave\n%.15g %.15g translate %f %f scale\n",x[0],y[0],Item->Size,Item->Size);
               Tcl_AppendResult(Interp,buf,(char*)NULL);

               if (Item->IconFill) {
                  Tk_CanvasPsColor(Interp,Graph->canvas,Item->IconFill);
               } else {
                  Tk_CanvasPsColor(Interp,Graph->canvas,Graph->BGColor);
               }
               Tk_glCanvasPsPath(Interp,Graph->canvas,IconList[Item->Icon].Co,IconList[Item->Icon].Nb);
               Tcl_AppendResult(Interp,"closepath fill\n",(char*)NULL);

               if (Item->IconOutline && Item->Width) {
                  Tk_CanvasPsColor(Interp,Graph->canvas,Item->IconOutline);
                  Tk_glCanvasPsPath(Interp,Graph->canvas,IconList[Item->Icon].Co,IconList[Item->Icon].Nb);
                  sprintf(buf,"%.15g setlinewidth 1 setlinecap 1 setlinejoin\nclosepath stroke\n",(double)(Item->Width)/Item->Size);
                  Tcl_AppendResult(Interp,buf,(char*)NULL);
               }
               Tcl_AppendResult(Interp,"grestore\n",(char*)NULL);
            }
         }
      }
   }
}

/*--------------------------------------------------------------------------------------------------------------
 * Nom          : <GraphItem_PostscriptXYZ>
 * Creation     : Mai 2005 - J.P. Gauthier - CMC/CMOE
 *
 * But          : Afficher l'item de graph dans le graph en vectoriel
 *
 * Parametres   :
 *   <Interp>   : Interpreteur Tcl
 *   <Graph>    : Item graph
 *   <Item>     : Item de graph
 *   <AxisX>    : Axe en X
 *   <AxisY>    : Axe en Y
 *   <AxisZ>    : Axe en Z
 *   <X0>       : Limite inferieure gauche du graph
 *   <Y0>       : Limite inferieure gauche du graph
 *   <X1>       : Limite superieur  droite du graph
 *   <Y1>       : Limite superieur  droite du graph
 *
 * Retour       :
 *
 * Remarques :
 *
 *---------------------------------------------------------------------------------------------------------------
*/
void GraphItem_PostscriptXYZ(Tcl_Interp *Interp,GraphItem *Graph,TGraphItem *Item,TGraphAxis *AxisX,TGraphAxis *AxisY,TGraphAxis *AxisZ,int X0,int Y0,int X1,int Y1) {

   Tk_FontMetrics tkm;
   TVector   *vecx,*vecy,*vecl,*val;
   Vect3d    *v,v0,v1,vt,*vl=NULL;
   char       buf[256];
   double     x,y,db,dh,x0,y0;
   int        i,j,n,vn,hd,px,py,pw;
   double     rect[8],sz;

   vecx=Vector_Get(Item->XData);
   vecy=Vector_Get(Item->YData);
   vecl=Vector_Get(Item->LowData);

   n=vecx->N<vecy->N?vecx->N:vecy->N;
   x0=y0=0.0;
   
   /* Histograms may have on more value in the axis orientation side*/
   if (Item->Type==HISTOGRAM) {
      hd=Item->Orient[0]=='X'?(vecx->N>vecy->N):(vecx->N<vecy->N);
   } else {
      hd=0;
   }
   if (!(v=(Vect3d*)malloc((n+hd)*sizeof(Vect3d)))) {
      App_Log(ERROR,"%s: Memory allocation error",__func__);
      return;
   }

   if (vecl) {
      if (!(vl=(Vect3d*)malloc((n+hd)*sizeof(Vect3d)))) {
         App_Log(ERROR,"%s: Memory allocation error",__func__);
         return;
      }
   }

   GraphAxis_Define(AxisX,vecx,X1-X0);
   GraphAxis_Define(AxisY,vecy,Y1-Y0);

   /* Compute graph curve points */
   db=1000;
   vn=0;
   for(i=0;i<n+hd;i++) {
      /* Histograms check*/
      if (i==n) {
         x=Item->Orient[0]=='X'?vecx->V[i]:vecx->V[n-1];
         y=Item->Orient[0]=='Y'?vecy->V[i]:vecy->V[n-1];
      } else {
         x=vecx->V[i];
         y=vecy->V[i];
      }

      if (x!=vecx->NoData &&y!=vecy->NoData) {
         v[vn][0]=X0+AXISVALUE(AxisX,x);
         v[vn][1]=Y0+AXISVALUE(AxisY,y);
         v[vn][2]=0.0;

         if (vecl) {
            vl[vn][0]=X0+AXISVALUE(AxisX,vecl->V[i]);
            vl[vn][1]=Y0+AXISVALUE(AxisY,vecl->V[i]);
            vl[vn][2]=0.0;
         }
         
         /* Check font spacing */
         if (vn>0) {
            if (Item->Orient[0]=='X') {
               j=v[vn][0]-v[vn-1][0];
               db=db<j?db:j;
            } else {
               j=fabs(v[vn][1]-v[vn-1][1]);
               db=db<j?db:j;
            }
         }
         vn++;
      }
   }

   /* Compute item spacing and width for bar and histogram graph */
   switch(Item->Type) {
      case WIDEBAR   : db=db/(Graph->NSide+1)*0.5; dh=db*2*(Graph->NSide*0.5-Graph->ISide-0.5); break;
      case HISTOGRAM : db*=0.50;                   dh=-db;                                      break;
      case BAR       : db*=0.25;                   dh=0.0;                                      break;
      default        : db=0.0;                     dh=0.0;                                      break;
   }

  /* In case we fill, we need to close the path */
   if (Item->Orient[0]=='X') {
      y0=Item->Origin==HUGE_VAL?Y0:Y0+AXISVALUE(AxisY,Item->Origin);
      v0[0]=v[0][0];
      v0[1]=Y0;
      v0[2]=0.0;
      v1[0]=v[n-1][0];
      v1[1]=Y0;
      v1[2]=0.0;
   } else {
      x0=Item->Origin==HUGE_VAL?X0:X0+AXISVALUE(AxisX,Item->Origin);
      v0[0]=X0;
      v0[1]=v[0][1];
      v0[2]=0.0;
      v1[0]=X0;
      v1[1]=v[n-1][1];
      v1[2]=0.0;
   }
   Tcl_AppendResult(Interp,"% Postscript du graph XYZ\n",(char*)NULL);

   if (Item->Fill)
      Tk_CanvasPsColor(Interp,Graph->canvas,Item->Fill);

   /* Display graph filling */
   if (Item->Fill) {
      if (Item->Type==HISTOGRAM) {
         if (Item->Orient[0]=='X') {
            for(i=0;i<vn-hd;i++) {
               GraphItem_ColorXYZ(Interp,Graph,Item,i);
               if (!hd && (i+1)==vn) {
                  SETRECT(rect,v[i][0],vl?vl[i][1]:y0,X1,v[i][1]);
               } else {
                  SETRECT(rect,v[i][0],vl?vl[i][1]:y0,v[i+1][0],v[i][1]);
               }
               Tk_glCanvasPsPath(Interp,Graph->canvas,rect,4);
               if (Item->Stipple) {
                  Tcl_AppendResult(Interp,"eoclip ",(char*)NULL);
                  glPostscriptStipple(Interp,NULL,Item->Stipple);
               } else {
                  Tcl_AppendResult(Interp,"eofill\n",(char*)NULL);
               }
           }
         } else {
            for(i=0;i<vn-hd;i++) {
               GraphItem_ColorXYZ(Interp,Graph,Item,i);
               if (!hd && (i+1)==vn) {
                  SETRECT(rect,vl?vl[i][0]:x0,v[i][1],v[i][0],Y1);
               } else {
                  SETRECT(rect,vl?vl[i][0]:x0,v[i][1],v[i][0],v[i+1][1]);
               }
               Tk_glCanvasPsPath(Interp,Graph->canvas,rect,4);
               if (Item->Stipple) {
                  Tcl_AppendResult(Interp,"eoclip ",(char*)NULL);
                  glPostscriptStipple(Interp,NULL,Item->Stipple);
               } else {
                  Tcl_AppendResult(Interp,"eofill\n",(char*)NULL);
               }
            }
         }
      } else if (Item->Type==BAR || Item->Type==WIDEBAR) {
         if (Item->Orient[0]=='X') {
            for(i=0;i<vn;i++) {
               GraphItem_ColorXYZ(Interp,Graph,Item,i);
               SETRECT(rect,v[i][0]-db-dh,vl?vl[i][1]:y0,v[i][0]+db-dh,v[i][1]);
               Tk_glCanvasPsPath(Interp,Graph->canvas,rect,4);
               if (Item->Stipple) {
                  Tcl_AppendResult(Interp,"eoclip ",(char*)NULL);
                  glPostscriptStipple(Interp,NULL,Item->Stipple);
               } else {
                  Tcl_AppendResult(Interp,"eofill\n",(char*)NULL);
               }
            }
         } else {
            for(i=0;i<vn;i++) {
               GraphItem_ColorXYZ(Interp,Graph,Item,i);
               SETRECT(rect,vl?vl[i][0]:x0,v[i][1]-db-dh,v[i][0],v[i][1]+db-dh);
               Tk_glCanvasPsPath(Interp,Graph->canvas,rect,4);
               if (Item->Stipple) {
                  Tcl_AppendResult(Interp,"eoclip ",(char*)NULL);
                  glPostscriptStipple(Interp,NULL,Item->Stipple);
               } else {
                  Tcl_AppendResult(Interp,"eofill\n",(char*)NULL);
               }
            }
         }
      } else if (Item->Type!=NONE) {

         sprintf(buf,"%.15g %.15g moveto\n",v0[0],v0[1]);
         Tcl_AppendResult(Interp,buf,(char*)NULL);

         /* Begin the tesselation */
         for(i=0;i<vn;i++) {
            sprintf(buf,"%.15g %.15g lineto\n",v[i][0],v[i][1]);
            Tcl_AppendResult(Interp,buf,(char*)NULL);
            if (Item->Type==SPLINE && i<vn-1) {
               for(j=1;j<10;j++) {
                  vt[0]=InterpHermite(v[i==0?0:i-1][0],v[i][0],v[i<vn-1?i+1:vn-1][0],v[i<vn-2?i+2:vn-1][0],j/10.0,0.0,0.0);
                  vt[1]=InterpHermite(v[i==0?0:i-1][1],v[i][1],v[i<vn-1?i+1:vn-1][1],v[i<vn-2?i+2:vn-1][1],j/10.0,0.0,0.0);
                  vt[2]=InterpHermite(v[i==0?0:i-1][2],v[i][2],v[i<vn-1?i+1:vn-1][2],v[i<vn-2?i+2:vn-1][2],j/10.0,0.0,0.0);
                  sprintf(buf,"%.15g %.15g lineto\n",vt[0],vt[1]);
                  Tcl_AppendResult(Interp,buf,(char*)NULL);
               }
            }
         }
         sprintf(buf,"%.15g %.15g lineto\n",v1[0],v1[1]);
         Tcl_AppendResult(Interp,buf,(char*)NULL);
         if (Item->Stipple) {
            Tcl_AppendResult(Interp,"eoclip ",(char*)NULL);
            glPostscriptStipple(Interp,NULL,Item->Stipple);
         } else {
            Tcl_AppendResult(Interp,"eofill\n",(char*)NULL);
         }
      }
   }

   /* Display graph outline */
   if (Item->Outline && Item->Width) {
      if (Item->Type==HISTOGRAM) {
         if (Item->Orient[0]=='X') {
            for(i=0;i<vn-hd;i++) {
               if (!hd && (i+1)==vn) {
                  SETRECT(rect,v[i][0],vl?vl[i][1]:y0,X1,v[i][1]);
               } else {
                  SETRECT(rect,v[i][0],vl?vl[i][1]:y0,v[i+1][0],v[i][1]);
               }
               Tk_glCanvasPsPath(Interp,Graph->canvas,rect,4);
               sprintf(buf,"%.15g %i lineto\n",v[i][0],Y0);
               Tcl_AppendResult(Interp,buf,(char*)NULL);
           }
         } else {
            for(i=0;i<vn-hd;i++) {
               if (!hd && (i+1)==vn) {
                  SETRECT(rect,vl?vl[i][0]:x0,v[i][1],v[i][0],Y1);
               } else {
                  SETRECT(rect,vl?vl[i][0]:x0,v[i][1],v[i][0],v[i+1][1]);
               }
               Tk_glCanvasPsPath(Interp,Graph->canvas,rect,4);
               sprintf(buf,"%i %.15g lineto\n",X0,v[i][1]);
               Tcl_AppendResult(Interp,buf,(char*)NULL);
            }
         }
      } else if (Item->Type==BAR || Item->Type==WIDEBAR) {
         if (Item->Orient[0]=='X') {
            for(i=0;i<vn;i++) {
               SETRECT(rect,v[i][0]-db-dh,vl?vl[i][1]:y0,v[i][0]+db-dh,v[i][1]);
               Tk_glCanvasPsPath(Interp,Graph->canvas,rect,4);
               sprintf(buf,"%.15g %i lineto\n",v[i][0]-db-dh,Y0);
               Tcl_AppendResult(Interp,buf,(char*)NULL);
           }
         } else {
            for(i=0;i<vn;i++) {
               SETRECT(rect,vl?vl[i][0]:x0,v[i][1]-db-dh,v[i][0],v[i][1]+db-dh);
               Tk_glCanvasPsPath(Interp,Graph->canvas,rect,4);
               sprintf(buf,"%i %.15g lineto\n",X0,v[i][1]-db-dh);
               Tcl_AppendResult(Interp,buf,(char*)NULL);
            }
         }
      } else if (Item->Type!=NONE) {
         if (Item->Fill) {
            sprintf(buf,"%.15g %.15g moveto\n",v0[0],v0[1]);
            Tcl_AppendResult(Interp,buf,(char*)NULL);
         } else {
            sprintf(buf,"%.15g %.15g moveto\n",v[0][0],v[0][1]);
            Tcl_AppendResult(Interp,buf,(char*)NULL);
         }
         for(i=0;i<vn;i++) {
            sprintf(buf,"%.15g %.15g lineto\n",v[i][0],v[i][1]);
            Tcl_AppendResult(Interp,buf,(char*)NULL);
            if (Item->Type==SPLINE && i<n-1) {
               for(j=1;j<10;j++) {
                  vt[0]=InterpHermite(v[i==0?0:i-1][0],v[i][0],v[i<vn-1?i+1:vn-1][0],v[i<vn-2?i+2:vn-1][0],j/10.0,0.0,0.0);
                  vt[1]=InterpHermite(v[i==0?0:i-1][1],v[i][1],v[i<vn-1?i+1:vn-1][1],v[i<vn-2?i+2:vn-1][1],j/10.0,0.0,0.0);
                  vt[2]=InterpHermite(v[i==0?0:i-1][2],v[i][2],v[i<vn-1?i+1:vn-1][2],v[i<vn-2?i+2:vn-1][2],j/10.0,0.0,0.0);
                  sprintf(buf,"%.15g %.15g lineto\n",vt[0],vt[1]);
                  Tcl_AppendResult(Interp,buf,(char*)NULL);
               }
            }
         }
         if (Item->Fill) {
            sprintf(buf,"%.15g %.15g lineto\n",v1[0],v1[1]);
            Tcl_AppendResult(Interp,buf,(char*)NULL);
         }
      }
      Tk_CanvasPsColor(Interp,Graph->canvas,Item->Outline);
      glPostscriptDash(Interp,&Item->Dash,Item->Width);
      sprintf(buf,"%i setlinewidth 1 setlinecap 1 setlinejoin\nstroke\n",Item->Width);
      Tcl_AppendResult(Interp,buf,(char*)NULL);
      glPostscriptDash(Interp,NULL,Item->Width);
   }

   /* Display Icons */
   if (Item->Icon && Item->Size>0.0) {
      for(i=0;i<vn-hd;i++) {
         if (i==0 || i==(vn-hd-1) || Item->IconXShowValue==1e32 || fmod(vecx->V[i],Item->IconXShowValue)==0.0) {
            sz=(Item->Size+Item->Width)*0.5;

            if (Item->Orient[0]=='X') {
               sprintf(buf,"gsave\n%.15g %.15g translate %f %f scale\n",v[i][0]-dh,v[i][1],sz,sz);
            } else {
               sprintf(buf,"gsave\n%.15g %.15g translate %f %f scale\n",v[i][0],v[i][1]-dh,sz,sz);
            }
            Tcl_AppendResult(Interp,buf,(char*)NULL);

            Tk_CanvasPsColor(Interp,Graph->canvas,Graph->BGColor);
            if (Item->IconFill) {
               if (Item->IconXFillValue==1e32 || fmod(vecx->V[i],Item->IconXFillValue)==0.0)
                  Tk_CanvasPsColor(Interp,Graph->canvas,Item->IconFill);
            }
            GraphItem_ColorXYZ(Interp,Graph,Item,i);
            Tk_glCanvasPsPath(Interp,Graph->canvas,IconList[Item->Icon].Co,IconList[Item->Icon].Nb);
            Tcl_AppendResult(Interp,"eofill\n",(char*)NULL);

            if (Item->IconOutline && Item->Width) {
               Tk_CanvasPsColor(Interp,Graph->canvas,Item->IconOutline);
               Tk_glCanvasPsPath(Interp,Graph->canvas,IconList[Item->Icon].Co,IconList[Item->Icon].Nb);
               sprintf(buf,"%.15g setlinewidth 1 setlinecap 1 setlinejoin\nclosepath stroke\n",(double)(Item->Width)/Item->Size);
               Tcl_AppendResult(Interp,buf,(char*)NULL);
            }
            Tcl_AppendResult(Interp,"grestore\n",(char*)NULL);
         }
      }
   }

   /* Display Values */
   if (Item->Value && Item->Font) {
      Tk_CanvasPsFont(Interp,Graph->canvas,Item->Font);
      Tk_CanvasPsColor(Interp,Graph->canvas,Item->Outline);

      switch(Item->Value) {
         case  1: val=(Item->Orient[0]=='X')?vecy:vecx; break;
         case  2: val=vecx; break;
         case  3: val=vecx; break;
         case  4: val=vecy; break;
         case  5: val=Vector_Get(Item->ZData); break;
         case  6: val=Vector_Get(Item->Speed); break;
         case  7: val=Vector_Get(Item->Dir); break;
         case  8: val=Vector_Get(Item->ErrorData); break;
         case  9: val=Vector_Get(Item->HighData); break;
         case 10: val=Vector_Get(Item->LowData); break;
         case 11: val=Vector_Get(Item->MedianData); break;
         case 12: val=Vector_Get(Item->MinData); break;
         case 13: val=Vector_Get(Item->MaxData); break;
         default: val=NULL;
      }
      if (val) {
         px=py=pw=0;
         Tk_GetFontMetrics(Item->Font,&tkm);
         for(i=0;i<vn-hd;i++) {
            GraphAxis_Print(Item->Orient[0]=='X'?AxisY:AxisX,buf,val->V[i],-2);
            j=Item->Width+2+(Item->Icon?Item->Size:0);
            y=v[i][1]+j;
            j=Tk_TextWidth(Item->Font,buf,strlen(buf));
            switch(Item->Anchor) {
               case TK_ANCHOR_N:
               case TK_ANCHOR_S:
               case TK_ANCHOR_CENTER: x=v[i][0]-j/2; break;
               case TK_ANCHOR_NW:
               case TK_ANCHOR_SW:               
               case TK_ANCHOR_W:      x=v[i][0]+j/strlen(buf);     break;
               case TK_ANCHOR_NE:
               case TK_ANCHOR_SE:
               case TK_ANCHOR_E:      x=v[i][0]-j-j/strlen(buf);   break;
               default: x=v[i][0];
            }
            if (!((y>=py && y<=py+tkm.linespace) || (y+tkm.linespace>=py && y<=py)) || !((x>=px && x<=px+pw) ||  (x+j>=px && x+j<=px+pw))) {
               px=x;py=y;pw=j;
               glPrintFlip(Interp,Graph->canvas,buf,x,y,0.0);
            }
         }
      }
   }

   /* Display bitmaps and images */
   if (Item->Bitmap) {
      glColor4us(Item->Outline->red,Item->Outline->green,Item->Outline->blue,Item->Alpha*Graph->Alpha*0.01*655);
      for(i=0;i<vn-hd;i++) {
         Tcl_AppendResult(Interp,"gsave\n",(char*)NULL);
         if (Item->Orient[0]=='X') {
            sprintf(buf,"%.15g %.15g translate %d %d true matrix {\n",(v[i][0]-dh-((TkCanvas*)Graph->canvas)->xOrigin)-Item->Bitmap->Width/2,-(v[i][1]-((TkCanvas*)Graph->canvas)->yOrigin)+Item->Bitmap->Height/2,Item->Bitmap->Width,Item->Bitmap->Height);
         } else {
            sprintf(buf,"%.15g %.15g translate %d %d true matrix {\n",(v[i][0]-((TkCanvas*)Graph->canvas)->xOrigin)-Item->Bitmap->Width/2,-(v[i][1]-dh-((TkCanvas*)Graph->canvas)->yOrigin)+Item->Bitmap->Height/2,-(int)(v[i][1]-((TkCanvas*)Graph->canvas)->yOrigin)+Item->Bitmap->Height/2,Item->Bitmap->Width);
         }
         Tcl_AppendResult(Interp,buf,(char*)NULL);
         glPostscriptBitmap(Interp,NULL,Item->Bitmap,Item->Bitmap->Width,Item->Bitmap->Height);
         Tcl_AppendResult(Interp,"grestore\n",(char*)NULL);
      }
   }

   if (Item->ImageString) {
      Tk_PhotoImageBlock data;
      Tk_PhotoHandle     handle;

      handle=Tk_FindPhoto(((TkCanvas*)Graph->canvas)->interp,Item->ImageString);
      Tk_PhotoGetImage(handle,&data);

      for(i=0;i<vn-hd;i++) {
         Tcl_AppendResult(Interp,"gsave\n",(char*)NULL);
         if (Item->Orient[0]=='X') {
            sprintf(buf,"%.15g %.15g translate\n",(v[i][0]-dh-((TkCanvas*)Graph->canvas)->xOrigin)-Item->Bitmap->Width/2,-(v[i][1]-((TkCanvas*)Graph->canvas)->yOrigin)+Item->Bitmap->Height/2);
         } else {
            sprintf(buf,"%.15g %.15g translate\n",(v[i][0]-((TkCanvas*)Graph->canvas)->xOrigin)-Item->Bitmap->Width/2,-(v[i][1]-dh-((TkCanvas*)Graph->canvas)->yOrigin)+Item->Bitmap->Height/2);
         }
         Tcl_AppendResult(Interp,buf,(char*)NULL);
         Tk_PostscriptPhoto(Interp,&data,((TkCanvas*)Graph->canvas)->psInfo,data.width,data.height);
         Tcl_AppendResult(Interp,"grestore\n",(char*)NULL);
      }
   }

  /* Display Fit curve */
   if (Item->Fit) {
      switch(Item->Fit[0]) {
         case 'L' : vn=GraphItem_FitLinear(v,vecx,vecy,AxisX,AxisY,AxisZ,X0,Y0,X1,Y1); break;
//         case 'G' : vn=GraphItem_FitGauss(v,vecx,vecy,AxisX,AxisY,AxisZ,X0,Y0,X1,Y1); break;
         default  : vn=0;
      }

      if (Graph->FGColor) {
         sprintf(buf,"%.15g %.15g moveto\n",v[0][0],v[0][1]);
         Tcl_AppendResult(Interp,buf,(char*)NULL);
         for(i=1;i<vn;i++) {
            sprintf(buf," %.15g %.15g lineto\n",v[i][0],v[i][1]);
            Tcl_AppendResult(Interp,buf,(char*)NULL);
         }

         Tk_CanvasPsColor(Interp,Graph->canvas,Graph->FGColor);
         sprintf(buf,"%i setlinewidth 1 setlinecap 1 setlinejoin\nstroke\n",Item->Width*2+2);
         Tcl_AppendResult(Interp,buf,(char*)NULL);
      }

      if (Item->Outline) {
         sprintf(buf,"%.15g %.15g moveto\n",v[0][0],v[0][1]);
         Tcl_AppendResult(Interp,buf,(char*)NULL);
         for(i=1;i<vn;i++) {
            sprintf(buf," %.15g %.15g lineto\n",v[i][0],v[i][1]);
            Tcl_AppendResult(Interp,buf,(char*)NULL);
         }

         Tk_CanvasPsColor(Interp,Graph->canvas,Item->Outline);
         sprintf(buf,"%i setlinewidth 1 setlinecap 1 setlinejoin\nstroke\n",Item->Width*2);
         Tcl_AppendResult(Interp,buf,(char*)NULL);
      }
   }
   
   if (v)  free(v);
   if (vl) free(vl);
}

/*--------------------------------------------------------------------------------------------------------------
 * Nom          : <GraphItem_HeaderPostscript>
 * Creation     : Mai 2005 - J.P. Gauthier - CMC/CMOE
 *
 * But          : Generer le code postscript pour l'entete de l'item de graph dans le graph
 *
 * Parametres   :
 *   <Interp>   : Interpreteur Tcl
 *   <Graph>    : Item graph
 *   <Item>     : Item de graph
 *   <X0>       : Limite inferieure gauche du graph
 *   <Y0>       : Limite inferieure gauche du graph
 *   <X1>       : Limite superieur  droite du graph
 *   <Y1>       : Limite superieur  droite du graph
 *
 * Retour       :
 *
 * Remarques :
 *
 *---------------------------------------------------------------------------------------------------------------
*/
int GraphItem_HeaderPostscript(Tcl_Interp *Interp,GraphItem *Graph,TGraphItem *Item,int X0,int Y0,int X1) {

   double  coords[8];
   int     y;
   char    buf[128];

   y=Y0+Item->DescHeight*0.5;

    Tcl_AppendResult(Interp,"% Postscript de l'entete du graph\n",(char*)NULL);

   if (Item->Fill) {
      SETRECT(coords,X1,y-5,X1+20,y+5);

      Tk_CanvasPsColor(Interp,Graph->canvas,Item->Fill);
      Tk_CanvasPsPath(Interp,Graph->canvas,coords,4);
      sprintf(buf,"%i ",Item->Width);
      Tcl_AppendResult(Interp,buf," setlinewidth 1 setlinecap 1 setlinejoin\n",(char*)NULL);

      if (Item->Stipple) {
         Tcl_AppendResult(Interp,"eoclip ",(char*)NULL);
         glPostscriptStipple(Interp,NULL,Item->Stipple);
         Tcl_AppendResult(Interp,"grestore gsave\n",(char*)NULL);
      } else {
         Tcl_AppendResult(Interp,"eofill\n",(char*)NULL);
      }

      if (Item->Width && Item->Outline) {

         glPostscriptDash(Interp,&Item->Dash,Item->Width);
         Tk_CanvasPsPath(Interp,Graph->canvas,coords,4);
         Tk_CanvasPsColor(Interp,Graph->canvas,Item->Outline);
         Tcl_AppendResult(Interp,"closepath stroke\n",(char*)NULL);
         glPostscriptDash(Interp,NULL,Item->Width);
      }
   } else {

      if (Item->Width && Item->Outline) {
         SETLINE(coords,X1,y,X1+20,y);
         glPostscriptDash(Interp,&Item->Dash,Item->Width);
         Tk_CanvasPsPath(Interp,Graph->canvas,coords,2);
         Tk_CanvasPsColor(Interp,Graph->canvas,Item->Outline);
         Tcl_AppendResult(Interp,"stroke\n",(char*)NULL);
         glPostscriptDash(Interp,NULL,Item->Width);
      }
   }

   if (Item->DescItem) {
   } else {
      if (Item->Font) {
         Tk_CanvasPsColor(Interp,Graph->canvas,Graph->FGColor);
         glPostscripTextLayout(Interp,Graph->canvas,Item->Text,Item->Outline,NULL,0,X0,Y0,TK_ANCHOR_NW,TK_JUSTIFY_LEFT);
      }
   }

   if (Item->Icon && Item->Size>0.0) {
      sprintf(buf,"gsave\n%i %i translate %f %f scale\n",X1+10,(int)Tk_CanvasPsY(Graph->canvas,y),Item->Size,Item->Size);
      Tcl_AppendResult(Interp,buf,(char*)NULL);

      if (Item->IconFill) {
         Tk_CanvasPsColor(Interp,Graph->canvas,Item->IconFill);
      } else {
         Tk_CanvasPsColor(Interp,Graph->canvas,Graph->BGColor);
      }
      Tk_glCanvasPsPath(Interp,Graph->canvas,IconList[Item->Icon].Co,IconList[Item->Icon].Nb);
      Tcl_AppendResult(Interp,"eofill\n",(char*)NULL);

      if (Item->IconOutline && Item->Width) {
         Tk_CanvasPsColor(Interp,Graph->canvas,Item->IconOutline);
         Tk_glCanvasPsPath(Interp,Graph->canvas,IconList[Item->Icon].Co,IconList[Item->Icon].Nb);
         sprintf(buf,"%.15g setlinewidth 1 setlinecap 1 setlinejoin\nclosepath stroke\n",(double)(Item->Width)/Item->Size);
         Tcl_AppendResult(Interp,buf,(char*)NULL);
      }
      Tcl_AppendResult(Interp,"grestore\n",(char*)NULL);
   }

   if (Item->Bitmap) {
      Tk_CanvasPsColor(Interp,Graph->canvas,Item->Outline);
      sprintf(buf,"%.15g %.15g translate %d %d true matrix {\n",X1+10-(double)Item->Bitmap->Width/2.0,Tk_CanvasPsY(Graph->canvas,y-Item->Bitmap->Height/2),Item->Bitmap->Width,Item->Bitmap->Height);
      Tcl_AppendResult(Interp,buf,(char*)NULL);
      glPostscriptBitmap(Interp,NULL,Item->Bitmap,Item->Bitmap->Width,Item->Bitmap->Height);
   }

   if (Item->ImageString) {
      Tk_PhotoHandle     handle;
      Tk_PhotoImageBlock data;
      handle=Tk_FindPhoto(((TkCanvas*)Graph->canvas)->interp,Item->ImageString);
      Tk_PhotoGetImage(handle,&data);

      sprintf(buf, "%.15g %.15g", X1+10-(double)data.width/2.0,Tk_CanvasPsY(Graph->canvas,y-data.height/2));
      Tcl_AppendResult(Interp,buf," translate\n",(char*)NULL);
      Tk_PostscriptPhoto(Interp,&data,((TkCanvas*)Graph->canvas)->psInfo,data.width,data.height);
   }
   return(Y0+Item->DescHeight+5);
}

