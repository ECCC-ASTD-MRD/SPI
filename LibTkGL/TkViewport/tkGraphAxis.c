/*=========================================================
 * Environnement Canada
 * Centre Meteorologique Canadien
 * 2100 Trans-Canadienne
 * Dorval, Quebec
 *
 * Projet       : Creation de graph dans le canvas Tk.
 * Fichier      : tkGraphAxis.c
 * Creation     : Mai 2005 - J.P. Gauthier - CMC/CMOE
 *
 * Description  : Fichier d'entete du module d'echelle de graph.
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

#include "tkGraphAxis.h"
#include "tkglCanvText.h"

#include <stdio.h>

enum GRAPHFORMATS { GRAXNONE,GRAXFIT,GRAXINTEGER,GRAXDATE,GRAXTIME,GRAXDATETIME,GRAXTIMEDATE,GRAX00HHDDMM,GRAX00HHMMDD,GRAXHHDDMM,GRAXHH,GRAXHHMM,GRAXDDMM,GRAXMMDD,GRAXTMINUSHH,GRAXTPLUSHH };

static CONST char *GRAPHFORMATS_STRING[] = { "NONE","FIT","INTEGER","DATE","TIME","DATETIME","TIME/DATE","00HH/DDMM","00HH/MMDD","HH/DDMM","HH","HHMM","DDMM","MMDD","T-HH","T+HH" };
static Tcl_HashTable GraphAxisTable;

static int GraphAxis_Cmd(ClientData clientData,Tcl_Interp *Interp,int Objc,Tcl_Obj *CONST Objv[]);
static int GraphAxis_Config(Tcl_Interp *Interp,char *Name,int Objc,Tcl_Obj *CONST Objv[]);
static int GraphAxis_Create(Tcl_Interp *Interp,char *Name);
static int GraphAxis_Free(Tcl_Interp *Interp,char *Name);

void GraphAxis_Dim(Tk_Canvas Canvas,TGraphAxis *Axis,GraphItem *Graph,int Side,int *Width,int *Height);
void GraphAxis_Display(Tcl_Interp *Interp,GraphItem *Graph,TGraphAxis *Axis,int X0,int Y0,int X1,int Y1,int Len,int Side);
void GraphAxis_Postscript(Tcl_Interp *Interp,GraphItem *Graph,TGraphAxis *Axis,int X0,int Y0,int X1,int Y1,int Len,int Side);

/*--------------------------------------------------------------------------------------------------------------
 * Nom          : <TclGraphAxis_Init>
 * Creation     : Mai 2005 - J.P. Gauthier - CMC/CMOE
 *
 * But          : Initialisation des commandes Tcl pour utilisation des axes de graphs
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
int TclGraphAxis_Init(Tcl_Interp *Interp) {

   Tcl_InitHashTable(&GraphAxisTable,TCL_STRING_KEYS);
   Tcl_CreateObjCommand(Interp,"graphaxis",GraphAxis_Cmd,(ClientData)NULL,(Tcl_CmdDeleteProc*)NULL);

   return TCL_OK;
}

/*--------------------------------------------------------------------------------------------------------------
 * Nom          : <GraphAxis_Cmd>
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
static int GraphAxis_Cmd(ClientData clientData,Tcl_Interp *Interp,int Objc,Tcl_Obj *CONST Objv[]) {

   int         idx;
   static CONST char *sopt[] = { "create","free","configure","wipe","is",NULL };
   enum               opt { CREATE,FREE,CONFIGURE,WIPE,IS };

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
         if(Objc!=3) {
            Tcl_WrongNumArgs(Interp,2,Objv,"axis");
            return TCL_ERROR;
         }
         return GraphAxis_Create(Interp,Tcl_GetString(Objv[2]));
         break;

      case FREE:
         if(Objc!=3) {
            Tcl_WrongNumArgs(Interp,2,Objv,"axis");
            return TCL_ERROR;
         }
         return GraphAxis_Free(Interp,Tcl_GetString(Objv[2]));
         break;

      case CONFIGURE:
         if(Objc<3) {
            Tcl_WrongNumArgs(Interp,2,Objv,"axis ?option?");
            return TCL_ERROR;
         }
         return GraphAxis_Config(Interp,Tcl_GetString(Objv[2]),Objc-3,Objv+3);
         break;
      case IS:
         if(Objc!=3) {
            Tcl_WrongNumArgs(Interp,2,Objv,"axis");
            return TCL_ERROR;
         }
         if (GraphAxis_Get(Tcl_GetString(Objv[2]))) {
            Tcl_SetObjResult(Interp,Tcl_NewBooleanObj(1));
         } else {
            Tcl_SetObjResult(Interp,Tcl_NewBooleanObj(0));
         }
         break;

      case WIPE:
         GraphAxis_Wipe();
   }
   return TCL_OK;
}

/*----------------------------------------------------------------------------
 * Nom      : <GraphAxis_Config>
 * Creation : Mai 2005 - J.P. Gauthier - CMC/CMOE
 *
 * But      : Configuration des parametres d'echelle
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
static int GraphAxis_Config(Tcl_Interp *Interp,char *Name,int Objc,Tcl_Obj *CONST Objv[]){

   TGraphAxis *axis;
   Tcl_Obj    *obj;
   char        buf[256];
   int         i,j,idx;

   static CONST char *sopt[] = { "-color","-width","-gridcolor","-gridwidth","-highlight","-highlightcolor","-highlightwidth","-dash","-font","-min","-max","-increment","-modulo","-intervals","-labels","-lowoffset","-highoffset","-type","-mark","-unit","-justify","-anchor","-position","-angle","-numbered","-format","-spacing",NULL };
   enum                opt { COLOR,WIDTH,GRIDCOLOR,GRIDWIDTH,HIGHLIGHT,HIGHLIGHTCOLOR,HIGHLIGHTWIDTH,GRIDDASH,FONT,MIN,MAX,INCREMENT,MODULO,INTERVALS,LABELS,LOWOFFSET,HIGHOFFSET,TYPE,MARK,UNIT,JUSTIFY,ANCHOR,POSITION,ANGLE,NUMBERED,FORMAT,SPACING };

   axis=GraphAxis_Get(Name);
   if (!axis) {
      Tcl_AppendResult(Interp,"\n   GraphAxis_Config: unknown object: \"",Name,"\"",(char*)NULL);
      return TCL_ERROR;
   }

   for(i=0;i<Objc;i++) {

      if (Tcl_GetIndexFromObj(Interp,Objv[i],sopt,"option",0,&idx)!=TCL_OK) {
         return TCL_ERROR;
      }

      switch ((enum opt)idx) {
         case COLOR:
            if (Objc==1) {
               if (axis->Color) {
                  Tcl_AppendResult(Interp,Tk_NameOfColor(axis->Color),(char*)NULL);
               }
            } else {
               if (axis->Color) Tk_FreeColor(axis->Color);
               axis->Color=Tk_AllocColorFromObj(Interp,Tk_MainWindow(Interp),Objv[++i]);
            }
            break;

         case GRIDCOLOR:
            if (Objc==1) {
               if (axis->GridColor) {
                  Tcl_AppendResult(Interp,Tk_NameOfColor(axis->GridColor),(char*)NULL);
               }
            } else {
               if (axis->GridColor) Tk_FreeColor(axis->GridColor);
               axis->GridColor=Tk_AllocColorFromObj(Interp,Tk_MainWindow(Interp),Objv[++i]);
            }
            break;

         case HIGHLIGHT:
            if (Objc==1) {
               obj=Tcl_NewListObj(0,NULL);
               for (j=0;j<axis->HighLightNb;j++){
                  Tcl_ListObjAppendElement(Interp,obj,Tcl_NewDoubleObj(axis->HighLight[j]));
               }
               Tcl_SetObjResult(Interp,obj);
            } else {
               /*Recuperation des niveaux*/
               Tcl_ListObjLength(Interp,Objv[++i],&axis->HighLightNb);

               if (axis->HighLight) {
                  free(axis->HighLight);
                  axis->HighLight=NULL;
               }

               if (axis->HighLightNb) {
                  axis->HighLight=(double*)malloc(axis->HighLightNb*sizeof(double));
                  for (j=0;j<axis->HighLightNb;j++){
                     Tcl_ListObjIndex(Interp,Objv[i],j,&obj);
                     Tcl_GetDoubleFromObj(Interp,obj,&axis->HighLight[j]);
                  }
               }
            }
            break;

         case HIGHLIGHTWIDTH:
            if (Objc==1) {
               Tcl_SetObjResult(Interp,Tcl_NewIntObj(axis->HighLightWidth));
           } else {
               Tcl_GetIntFromObj(Interp,Objv[++i],&axis->HighLightWidth);
            }
            break;

         case HIGHLIGHTCOLOR:
            if (Objc==1) {
               if (axis->HighLightColor) {
                  Tcl_AppendResult(Interp,Tk_NameOfColor(axis->HighLightColor),(char*)NULL);
               }
            } else {
               if (axis->HighLightColor) Tk_FreeColor(axis->HighLightColor);
               axis->HighLightColor=Tk_AllocColorFromObj(Interp,Tk_MainWindow(Interp),Objv[++i]);
            }
            break;

         case GRIDWIDTH:
            if (Objc==1) {
               Tcl_SetObjResult(Interp,Tcl_NewIntObj(axis->GridWidth));
           } else {
               Tcl_GetIntFromObj(Interp,Objv[++i],&axis->GridWidth);
            }
            break;

         case GRIDDASH:
            if (Objc==1) {
               DashPrint(buf,&axis->Dash);
               Tcl_AppendResult(Interp,buf,(char*)NULL);
            } else {
               Tk_GetDash(Interp,Tcl_GetString(Objv[++i]),&axis->Dash);
            }
            break;

         case WIDTH:
            if (Objc==1) {
               Tcl_SetObjResult(Interp,Tcl_NewIntObj(axis->Width));
            } else {
               Tcl_GetIntFromObj(Interp,Objv[++i],&axis->Width);
           }
            break;

         case ANGLE:
            if (Objc==1) {
               Tcl_SetObjResult(Interp,Tcl_NewDoubleObj(axis->Angle));
            } else {
               Tcl_GetDoubleFromObj(Interp,Objv[++i],&axis->Angle);
            }
            break;

         case FONT:
            if (Objc==1) {
               Tcl_AppendResult(Interp,Tk_NameOfFont(axis->Font),(char*)NULL);
            } else {
               if (axis->Font) Tk_FreeFont(axis->Font);
               axis->Font=Tk_AllocFontFromObj(Interp,Tk_MainWindow(Interp),Objv[++i]);
            }
            break;

         case INCREMENT:
            if (Objc==1) {
               Tcl_SetObjResult(Interp,Tcl_NewDoubleObj(axis->Incr));
            } else {
               if (Tcl_GetDoubleFromObj(Interp,Objv[++i],&axis->Incr)==TCL_ERROR) {
                  axis->Incr=0.0;
               }
               axis->Incr=fabs(axis->Incr);
            }
            break;

         case MIN:
            if (Objc==1) {
               Tcl_SetObjResult(Interp,Tcl_NewDoubleObj(axis->Min));
            } else {
               if (Tcl_GetDoubleFromObj(Interp,Objv[++i],&axis->Min)==TCL_ERROR) {
                  axis->Min=0.0;
               }
            }
            break;

         case MAX:
            if (Objc==1) {
               Tcl_SetObjResult(Interp,Tcl_NewDoubleObj(axis->Max));
            } else {
               if (Tcl_GetDoubleFromObj(Interp,Objv[++i],&axis->Max)==TCL_ERROR) {
                  axis->Max=0.0;
               }
            }
            break;

         case LOWOFFSET:
            if (Objc==1) {
               Tcl_SetObjResult(Interp,Tcl_NewIntObj(axis->Offset[0]));
            } else {
               Tcl_GetIntFromObj(Interp,Objv[++i],&axis->Offset[0]);
            }
            break;

         case HIGHOFFSET:
            if (Objc==1) {
               Tcl_SetObjResult(Interp,Tcl_NewIntObj(axis->Offset[1]));
            } else {
               Tcl_GetIntFromObj(Interp,Objv[++i],&axis->Offset[1]);
            }
            break;

         case MARK:
            if (Objc==1) {
               Tcl_SetObjResult(Interp,Tcl_NewDoubleObj(axis->Mark));
            } else {
               Tcl_GetDoubleFromObj(Interp,Objv[++i],&axis->Mark);
            }
            break;

         case MODULO:
            if (Objc==1) {
               Tcl_SetObjResult(Interp,Tcl_NewBooleanObj(axis->Mod));
            } else {
               Tcl_GetBooleanFromObj(Interp,Objv[++i],&axis->Mod);
            }
            break;

         case TYPE:
            if (Objc==1) {
               switch(axis->Type) {
                  case 'I': Tcl_SetObjResult(Interp,Tcl_NewStringObj("LINEAR",-1)); break;
                  case 'O': Tcl_SetObjResult(Interp,Tcl_NewStringObj("LOGARITHMIC",-1)); break;
                  case 'N': Tcl_SetObjResult(Interp,Tcl_NewStringObj("LN",-1)); break;
               }
            } else {
               axis->Type=Tcl_GetString(Objv[++i])[1];
               if (axis->Type!='I' && axis->Type!='O'&& axis->Type!='N') {
                  axis->Type='I';
                  Tcl_AppendResult(Interp,"\n   GraphAxis_Config: Invalid axis type must be [LINEAR | LOGARITHMIC]",(char*)NULL);
                  return TCL_ERROR;
               }
            }
            break;

         case POSITION:
            if (Objc==1) {
               Tcl_SetObjResult(Interp,Tcl_NewStringObj(axis->Pos,-1));
            } else {
               axis->Pos[0]=Tcl_GetString(Objv[++i])[0];
               axis->Pos[1]=Tcl_GetString(Objv[i])[1];
               if (axis->Pos[0]!='U'&& axis->Pos[0]!='L' && axis->Pos[1]!='L'&& axis->Pos[1]!='R') {
                  Tcl_AppendResult(Interp,"\n   GraphAxis_Config: Invalid axis position must be [UL | UR | LL | LR]",(char*)NULL);
                  return TCL_ERROR;
               }
            }
            break;

         case UNIT:
            if (Objc==1) {
               Tcl_SetObjResult(Interp,Tcl_NewStringObj(axis->Unit,-1));
            } else {
               if (axis->Unit) free(axis->Unit);
               axis->Unit=strdup(Tcl_GetString(Objv[++i]));
            }
            break;

         case JUSTIFY:
            if (Objc==1) {
               Tcl_SetObjResult(Interp,Tcl_NewStringObj(Tk_NameOfJustify(axis->Justify),-1));
             } else {
               Tk_GetJustifyFromObj(Interp,Objv[i],&axis->Justify);
            }
            break;

         case ANCHOR:
            if (Objc==1) {
               Tcl_SetObjResult(Interp,Tcl_NewStringObj(Tk_NameOfAnchor(axis->Anchor),-1));
             } else {
               Tk_GetAnchorFromObj(Interp,Objv[i],&axis->Anchor);
            }
            break;

         case INTERVALS:
            if (Objc==1) {
               obj=Tcl_NewListObj(0,NULL);
               for (j=0;j<axis->InterNb;j++){
                  Tcl_ListObjAppendElement(Interp,obj,Tcl_NewDoubleObj(axis->Inter[j]));
               }
               Tcl_SetObjResult(Interp,obj);
            } else {
               /*Recuperation des niveaux*/
               Tcl_ListObjLength(Interp,Objv[++i],&axis->InterNb);

               if (axis->Inter) {
                  free(axis->Inter);
                  axis->Inter=NULL;
               }

               if (axis->InterNb) {
                  axis->Inter=(double*)malloc(axis->InterNb*sizeof(double));
                  for (j=0;j<axis->InterNb;j++){
                     Tcl_ListObjIndex(Interp,Objv[i],j,&obj);
                     if (Tcl_GetDoubleFromObj(Interp,obj,&axis->Inter[j])==TCL_ERROR) {
                        return(TCL_ERROR);
                     }
                  }
               }
            }
            break;

          case LABELS:
            if (Objc==1) {
               for (j=0;j<axis->InterNb;j++){
                  Tcl_AppendElement(Interp,axis->Label[j]);
               }
            } else {
               /*Recuperation des niveaux*/
               if (axis->Label) {
                  Tcl_Free((char*)axis->Label);
                  axis->Label=NULL;
               }
               Tcl_SplitList(Interp,Tcl_GetString(Objv[++i]),&j,&axis->Label);

               if (j!=axis->InterNb && j!=0) {
                  Tcl_Free((char*)axis->Label);
                  axis->Label=NULL;
                  Tcl_AppendResult(Interp,"\n   GraphAxis_Config: Invalid axis label length, must be the same as intervals",(char*)NULL);
                  return(TCL_ERROR);
               }
            }
            break;

         case NUMBERED:
            if (Objc==1) {
               Tcl_SetObjResult(Interp,Tcl_NewBooleanObj(axis->Numbered));
             } else {
               Tcl_GetBooleanFromObj(Interp,Objv[++i],&axis->Numbered);
            }
            break;

         case FORMAT:
            if (Objc==1) {
               Tcl_SetObjResult(Interp,Tcl_NewStringObj(GRAPHFORMATS_STRING[axis->Format],-1));
            } else {
               if (Tcl_GetIndexFromObj(Interp,Objv[++i],GRAPHFORMATS_STRING,"format",0,&axis->Format)!=TCL_OK) {
                  return(TCL_ERROR);
               }
            }
            break;

         case SPACING:
            if (Objc==1) {
               Tcl_SetObjResult(Interp,Tcl_NewIntObj(axis->Spacing));
             } else {
               Tcl_GetIntFromObj(Interp,Objv[++i],&axis->Spacing);
            }
            break;
     }
   }
   return TCL_OK;
}

/*--------------------------------------------------------------------------------------------------------------
 * Nom          : <GraphAxis_Create>
 * Creation     : Mai 2005 - J.P. Gauthier - CMC/CMOE
 *
 * But          : Creation d'un objet vecteur et insertion d'un nouveau nom dans la table.
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
static int GraphAxis_Create(Tcl_Interp *Interp,char *Name) {

   TGraphAxis *axis;

   if (!(axis=(TGraphAxis*)TclY_HashPut(Interp,&GraphAxisTable,Name,sizeof(TGraphAxis)))) {
      return(TCL_ERROR);
   }

   axis->Inter=NULL;
   axis->Label=NULL;
   axis->InterNb=0;
   axis->Angle=0.0;
   axis->Min=0.0;
   axis->Max=0.0;
   axis->Color=NULL;
   axis->Width=0;
   axis->GridColor=NULL;
   axis->GridWidth=0;
   axis->HighLightColor=NULL;
   axis->HighLightWidth=0;
   axis->HighLightNb=0;
   axis->HighLight=NULL;
   axis->Numbered=1;
   axis->Spacing=5;
   axis->Format=0;
   axis->Dash.number=0;
   axis->Justify=TK_JUSTIFY_CENTER;
   axis->Anchor=TK_ANCHOR_CENTER;
   axis->Text=NULL;
   axis->Font=NULL;
   axis->Unit=NULL;
   axis->UnitItem=NULL;
   axis->UnitWidth=0;
   axis->UnitHeight=0;
   axis->Type='I';
   axis->Pos[0]='L';
   axis->Pos[1]='L';
   axis->Pos[2]='\0';

   axis->Mod=1;
   axis->T0=0.0;
   axis->T1=0.0;
   axis->Order=0;
   axis->Mark=0.0;
   axis->Incr=0.0;
   axis->Offset[0]=0;
   axis->Offset[1]=0;

   return(TCL_OK);
}

/*--------------------------------------------------------------------------------------------------------------
 * Nom          : <GraphAxis_Get>
 * Creation     : Mai 2005 - J.P. Gauthier - CMC/CMOE
 *
 * But          : Obtenir un objet vecteur en fonction de son nom dans la table de Tcl
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
TGraphAxis* GraphAxis_Get(char *Name) {
   return((TGraphAxis*)TclY_HashGet(&GraphAxisTable,Name));
}

/*--------------------------------------------------------------------------------------------------------------
 * Nom          : <GraphAxis_Free>
 * Creation     : Mai 2005 - J.P. Gauthier - CMC/CMOE
 *
 * But          : Destruction d'un vecteur a partir de son nom.
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
static int GraphAxis_Free(Tcl_Interp *Interp,char *Name) {

   TGraphAxis *axis;

   if ((axis=(TGraphAxis*)TclY_HashDel(&GraphAxisTable,Name))) {
      GraphAxis_Clear(axis);
      free(axis);
   }
   return(TCL_OK);
}

/*--------------------------------------------------------------------------------------------------------------
 * Nom          : <GraphAxis_Clear>
 * Creation     : Mai 2005 - J.P. Gauthier - CMC/CMOE
 *
 * But          : Suppression des valeur et de la memoire alloue a l'axe.
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
void GraphAxis_Clear(TGraphAxis *Axis) {

   if (Axis->Inter)          free(Axis->Inter);
   if (Axis->HighLight)      free(Axis->HighLight);
   if (Axis->Label)          Tcl_Free((char*)Axis->Label);
   if (Axis->Unit)           free(Axis->Unit);
   if (Axis->Color)          Tk_FreeColor(Axis->Color);
   if (Axis->GridColor)      Tk_FreeColor(Axis->GridColor);
   if (Axis->HighLightColor) Tk_FreeColor(Axis->HighLightColor);
   if (Axis->Font)           Tk_FreeFont(Axis->Font);
   if (Axis->Text)           Tk_FreeTextLayout(Axis->Text);

   Axis->Text=NULL;
   Axis->Inter=NULL;
   Axis->HighLight=NULL;
   Axis->HighLightColor=NULL;
   Axis->InterNb=0;
   Axis->Color=NULL;
   Axis->Font=NULL;
   Axis->Unit=NULL;
   Axis->UnitItem=NULL;
   Axis->Type='I';

   Axis->Mod=1;
   Axis->T0=0.0;
   Axis->T1=0.0;
   Axis->Min=0.0;
   Axis->Max=0.0;
   Axis->Mark=0.0;
   Axis->Incr=0.0;
}

/*----------------------------------------------------------------------------
 * Nom      : <GraphAxis_Wipe>
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
void GraphAxis_Wipe() {

   Tcl_HashSearch ptr;
   Tcl_HashEntry  *entry=NULL;

   entry=Tcl_FirstHashEntry(&GraphAxisTable,&ptr);
   while (entry) {
      GraphAxis_Clear((TGraphAxis*)Tcl_GetHashValue(entry));
      free((TGraphAxis*)Tcl_GetHashValue(entry));
      Tcl_DeleteHashEntry(entry);
      entry=Tcl_FirstHashEntry(&GraphAxisTable,&ptr);
   }
}

/*--------------------------------------------------------------------------------------------------------------
 * Nom          : <GraphAxis_Define>
 * Creation     : Mai 2005 - J.P. Gauthier - CMC/CMOE
 *
 * But          : Definir les limites de l'axe.
 *
 * Parametres   :
 *   <Axis>     : Axe
 *   <Vec>      : Vecteur de donnees associes
 *   <Delta>    :
 *
 * Retour       :
 *
 * Remarques :
 *
 *---------------------------------------------------------------------------------------------------------------
*/
void GraphAxis_Define(TGraphAxis *Axis,TVector *Vec,int Delta) {

   int    i,o;
   double d;

   if (!Axis)
      return;

   if (Axis->Min==Axis->Max) {
      if (Axis->InterNb) {
         Axis->Min=Axis->Inter[0];
         Axis->Max=Axis->Inter[Axis->InterNb-1];
      }
      if (Vec) {
         VECTORMIN(Vec,Axis->Min)
         VECTORMAX(Vec,Axis->Max)
      }
   }

   if (Axis->Min==Axis->Max) {
      Axis->Max+=1.0;
   }

   Axis->T0=Axis->Min;
   Axis->T1=Axis->Max;

   if (Axis->Type!='I') {
      if (Axis->Type=='O') {
         Axis->T0=Axis->T0==0?0.0:log10(fabs(Axis->T0));
         Axis->T1=Axis->T1==0?0.0:log10(fabs(Axis->T1));
      } else {
         Axis->T0=Axis->T0==0?0.0:log(fabs(Axis->T0));
         Axis->T1=Axis->T1==0?0.0:log(fabs(Axis->T1));
      }
      if (Axis->T0<Axis->T1) {
         Axis->T0=floor(Axis->T0);
         Axis->T1=ceil(Axis->T1);
      } else {
         Axis->T0=ceil(Axis->T0);
         Axis->T1=floor(Axis->T1);
      }

      if (Axis->T0==Axis->T1)                 Axis->T0=Axis->T0-1.0;
      if (Axis->T1<Axis->T0 && Axis->T0==0.0) Axis->T1=Axis->T1-1.0;
   }

   Axis->Order=RANGE_ORDER(Axis->T1-Axis->T0);
   for(i=1;i<Axis->InterNb;i++) {
      o=RANGE_ORDER(Axis->Inter[i]-Axis->Inter[i-1]);
      Axis->Order=abs(Axis->Order)>abs(o)?Axis->Order:o;
   }

   if (Axis->Mod && !Axis->InterNb && Axis->Incr==0.0) {
      d=RANGE_INCR(Axis->Order);
      if (Axis->T0<Axis->T1) {
         Axis->T0=floor(Axis->T0/d)*d;
         Axis->T1=ceil(Axis->T1/d)*d;
      } else {
         Axis->T0=ceil(Axis->T0/d)*d;
         Axis->T1=floor(Axis->T1/d)*d;
      }
   }

   Axis->Delta=(double)(Delta-(Axis->Offset[0]+Axis->Offset[1]))/(Axis->T1-Axis->T0);
   Axis->DT0=AXISVALUE(Axis,Axis->Min);
   Axis->DT1=AXISVALUE(Axis,Axis->Max);
}

/*--------------------------------------------------------------------------------------------------------------
 * Nom          : <GraphAxis_Dim>
 * Creation     : Mai 2005 - J.P. Gauthier - CMC/CMOE
 *
 * But          : Recuperer les dimensions d'un axe en largeur et an hauteur
 *
 * Parametres   :
 *   <Canvas>   : Canvas
 *   <Axis>     : Axe
 *   <Graph>    : Item graph
 *   <Side>     : Orientation de l'axe (HORIZONTAL,VERTICAL)
 *   <Width>    : Retour de la largeur de l'axe
 *   <Height>   : Retour de la hauteur de l'axe
 *
 * Retour       :
 *
 * Remarques :
 *
 *---------------------------------------------------------------------------------------------------------------
*/
void GraphAxis_Dim(Tk_Canvas Canvas,TGraphAxis *Axis,GraphItem *Graph,int Side,int *Width,int *Height) {

   Tk_FontMetrics tkm;
   Tk_TextLayout  text;
   Tk_Font        font=NULL;
   double         h,w,wt=0,ht=0;;
   int            i,width,height;
   char           buf0[32],buf1[32];

   *Width=*Height=0;
   Axis->UnitWidth=Axis->UnitHeight=0;

   if (!Axis)
      return;

   font=Axis->Font?Axis->Font:Graph->Font;
   if (!font)
      return;

   if (Axis->Unit) {
      Axis->UnitItem=Tk_glGetItem(Canvas,Tcl_NewStringObj(Axis->Unit,-1));

      if (Axis->Text) Tk_FreeTextLayout(Axis->Text);

      if (Axis->UnitItem) {
         Axis->UnitWidth=((glTextItem*)Axis->UnitItem)->header.x2-((glTextItem*)Axis->UnitItem)->header.x1;
         Axis->UnitHeight=((glTextItem*)Axis->UnitItem)->header.y2-((glTextItem*)Axis->UnitItem)->header.y1;
         if (Side==VERTICAL) {
               h=Axis->UnitHeight;
               Axis->UnitHeight=Axis->UnitWidth;
               Axis->UnitWidth=h;
         }
      } else {
         Axis->Text=Tk_ComputeTextLayout(font,Axis->Unit,Tcl_NumUtfChars(Axis->Unit,strlen(Axis->Unit)),0,Axis->Justify,0,&Axis->UnitWidth,&Axis->UnitHeight);
      }
   }

   Tk_GetFontMetrics(font,&tkm);

   h=sin(DEG2RAD(Axis->Angle));
   w=cos(DEG2RAD(Axis->Angle));

   if (Axis->Numbered) {
      if (Axis->InterNb) {
         for(i=0;i<Axis->InterNb;i++) {
            if (Axis->Label) {
               text=Tk_ComputeTextLayout(font,Axis->Label[i],Tcl_NumUtfChars(Axis->Label[i],strlen(Axis->Label[i])),0,TK_JUSTIFY_CENTER,0,&width,&height);
            } else {
               GraphAxis_Print(Axis,buf0,Axis->Inter[i],0);
               text=Tk_ComputeTextLayout(font,buf0,Tcl_NumUtfChars(buf0,strlen(buf0)),0,TK_JUSTIFY_CENTER,0,&width,&height);
            }
            Tk_FreeTextLayout(text);
            wt=wt<width?width:wt;
            ht=ht<height?height:ht;
         }
      } else {
         GraphAxis_Print(Axis,buf0,Axis->Min,0);
         GraphAxis_Print(Axis,buf1,Axis->Max,0);
         if (strlen(buf0)>strlen(buf1)) {
            wt=Tk_TextWidth(font,buf0,strlen(buf0));
         } else {
            wt=Tk_TextWidth(font,buf1,strlen(buf1));
         }
         ht=tkm.linespace;
      }
      wt*=1.15;
   }

   if (Side==VERTICAL) {
      *Width=ABS(h)*wt*(Axis->Angle==0.0?1.0:0.5);
      *Height=ABS(w)*0.5*ht+wt+10;

      if (Axis->Unit) {
        *Height+=Axis->UnitHeight+10;
      }

   } else {
      *Width=ABS(w)*wt*(Axis->Angle==0.0?1.0:0.5);
      *Height=ABS(h)*wt+ht+10;

      if (Axis->Unit) {
        *Height+=Axis->UnitHeight+10;
      }
   }
}

/*--------------------------------------------------------------------------------------------------------------
 * Nom          : <GraphAxis_Layout>
 * Creation     : Janvier 2007- J.P. Gauthier - CMC/CMOE
 *
 * But          : Recuperer le deplacement en x et y pour l'affichade d'une chaine sur l'axe
 *
 * Parametres   :
 *   <Axis>     : Axe
 *   <Side>     : Orientation de l'axe (HORIZONTAL,VERTICAL)
 *   <Width>    : Largeur du text layout
 *   <Height>   : Hauteur du text layout
 *   <DX>       : Deplacement en X
 *   <DY>       : Deplacement en Y
 *
 * Retour       :
 *
 * Remarques :
 *
 *---------------------------------------------------------------------------------------------------------------
*/
int GraphAxis_Layout(TGraphAxis *Axis,int Side,int Width,int Height,int *DX,int *DY) {

   double h,w;

   if (Axis->Angle!=0.0) {
      h=sin(DEG2RAD(-Axis->Angle));
      w=cos(DEG2RAD(-Axis->Angle));
   }

   if (Side&HORIZONTAL) {
      if (Axis->Angle==0.0) {
         *DX=-Width/2.0;
         *DY=Axis->Pos[0]=='L'?-15:15+Height;
      } else {
         if (Axis->Pos[0]=='L') {
            *DX=Axis->Angle>0?-w*Width:0;
            if (Axis->Angle<0) {
               *DY=-20;
            } else {
               *DY=h*Width-20;
            }
         } else {
            *DX=Axis->Angle>0?0:-w*Width;
            if (Axis->Angle<0) {
               *DY=h*Width+10+Height;
            } else {
               *DY=10+Height;
            }
         }
      }
      Width=-*DX;
   } else {
      if (Axis->Angle==0.0) {
         *DX=Axis->Pos[1]=='L'?-Width-15:15;
         *DY=Height/2.0;
      } else {
         if (Axis->Pos[1]=='L') {
            *DX=-w*Width+h*Height-15;
            *DY=h*Width+w*Height;
         } else {
            *DX=15;
            *DY=-h*Width-w*Height;
         }
      }
      Width=*DY;
   }
   return(((Axis->Angle==0 && Side&HORIZONTAL)?Width+5:Height)+Axis->Spacing);
}

/*--------------------------------------------------------------------------------------------------------------
 * Nom          : <GraphAxis_Print>
 * Creation     : Mai 2005 - J.P. Gauthier - CMC/CMOE
 *
 * But          : Formater un interval de l'axe
 *
 * Parametres   :
 *   <Axis>     : Axe
 *   <String>   : Chaine formattee
 *   <Value>    : Interval a formatter
 *
 * Retour       :
 *
 * Remarques :
 *
 *---------------------------------------------------------------------------------------------------------------
*/
void GraphAxis_Print(TGraphAxis *Axis,char *String,double Value,int DOrder) {

   struct tm *tsec;
   time_t     sec;
   double     val;

   if (Axis->Numbered) {
      if (Axis->Format>2) {
         sec=Value;
         tsec=gmtime(&sec);
      }

      switch((enum GRAPHFORMATS)Axis->Format) {
         case GRAXDATE: sprintf(String,"%i/%02i/%02i",(tsec->tm_year+1900),(tsec->tm_mon+1),tsec->tm_mday); break;
         case GRAXTIME: sprintf(String,"%02i:%02i:%02i",tsec->tm_hour,tsec->tm_min,tsec->tm_sec); break;
         case GRAXDATETIME: sprintf(String,"%i/%02i/%02i %02i:%02i:%02i",(tsec->tm_year+1900),(tsec->tm_mon+1),tsec->tm_mday,tsec->tm_hour,tsec->tm_min,tsec->tm_sec); break;
         case GRAXTIMEDATE: sprintf(String,"%02i:%02i:%02i\n%i/%02i/%02i",tsec->tm_hour,tsec->tm_min,tsec->tm_sec,(tsec->tm_year+1900),(tsec->tm_mon+1),tsec->tm_mday); break;
         case GRAX00HHDDMM: if (tsec->tm_hour==0) {
                               sprintf(String,"%02i\n%02i/%02i",tsec->tm_hour,tsec->tm_mday,(tsec->tm_mon+1));
                            } else {
                               sprintf(String,"%02i",tsec->tm_hour);
                            }
                            break;
         case GRAX00HHMMDD: if (tsec->tm_hour==0) {
                               sprintf(String,"%02i\n%02i/%02i",tsec->tm_hour,(tsec->tm_mon+1),tsec->tm_mday);
                            } else {
                               sprintf(String,"%02i",tsec->tm_hour);
                            }
                            break;
         case GRAXHHDDMM: sprintf(String,"%02i\n%02i/%02i",tsec->tm_hour,tsec->tm_mday,(tsec->tm_mon+1)); break;
         case GRAXHH:   sprintf(String,"%02i",tsec->tm_hour); break;
         case GRAXTMINUSHH: sprintf(String,"T-%.0fH",(Axis->Max-Value)/3600.0); break;
         case GRAXTPLUSHH: sprintf(String,"T+%.0fH",(Value-Axis->Min)/3600.0); break;
         case GRAXHHMM: sprintf(String,"%02i:%02i",tsec->tm_hour,tsec->tm_min); break;
         case GRAXDDMM: sprintf(String,"%02i/%02i",tsec->tm_mday,(tsec->tm_mon+1)); break;
         case GRAXMMDD: sprintf(String,"%02i/%02i",(tsec->tm_mon+1),tsec->tm_mday); break;
         case GRAXINTEGER: Value=ROUND(Value);sprintf(String,"%.0f",Value);break;
         case GRAXFIT: DOrder=0; val=Value; while (fmod(val*=10.0,1.0)>0.01) DOrder--; DOrder-=Axis->Order;
         default:
           if (Axis->Type!='I') {
               if (Value>0.0) {
                  snprintf(String,32,"%.2e",Value);
               } else {
                  snprintf(String,32,"0");
               }
            } else {
               if (!DOrder && Axis->Incr!=0.0 && (Axis->Incr-ROUND(Axis->Incr))==0.0) {
                  snprintf(String,32,"%.0f",Value);
               } else {
                  switch(Axis->Order+DOrder) {
                     case   4: snprintf(String,32,"%.2f",Value);break;
                     case   3: snprintf(String,32,"%.2f",Value);break;
                     case   2: snprintf(String,32,"%.2f",Value);break;
                     case   1: snprintf(String,32,"%.2f",Value);break;
                     case   0:
                     case  -1: snprintf(String,32,"%.2f",Value);break;
                     case  -2: snprintf(String,32,"%.3f",Value);break;
                     case  -3: snprintf(String,32,"%.4f",Value);break;
                     case  -4: snprintf(String,32,"%.3e",Value);break;
                     case  -5: snprintf(String,32,"%.4e",Value);break;
                     case  -6: snprintf(String,32,"%.5e",Value);break;
                     case  -7: snprintf(String,32,"%.6e",Value);break;
                     default : snprintf(String,32,"%.2e",Value);
                  }
               }
            }
      }
   } else {
      String[0]='\0';
   }
}

/*--------------------------------------------------------------------------------------------------------------
 * Nom          : <GraphAxis_Display>
 * Creation     : Mai 2005 - J.P. Gauthier - CMC/CMOE
 *
 * But          : Afficher l'axe dans le graph
 *
 * Parametres   :
 *   <Interp>   : Interpreteur Tcl
 *   <Graph>    : Item graph
 *   <Axis>     : Axe
 *   <X0>       : Limite inferieure gauche du graph
 *   <Y0>       : Limite inferieure gauche du graph
 *   <X1>       : Limite superieur  droite du graph
 *   <Y1>       : Limite superieur  droite du graph
 *   <Len>      : Largeur/epaisseur de l'axe
 *   <Side>     : Orientation de l'axe (HORIZONTAL,VERTICAL)
 *
 * Retour       :
 *
 * Remarques :
 *
 *---------------------------------------------------------------------------------------------------------------
*/
void GraphAxis_Display(Tcl_Interp *Interp,GraphItem *Graph,TGraphAxis *Axis,int X0,int Y0,int X1,int Y1,int Len,int Side) {

   int    i,o,dx,dy,th,w=0,width,height;
   double inter,incr,i0,i1,it,mk,x,y,yp,xp=0;
   char   buf[32];
   XColor *color;
   Tk_Font font;
   Tk_TextLayout  text;

   if (Axis->Font) {
      font=Axis->Font;
   } else {
      font=Graph->Font;
   }

   if (Axis->Color) {
      color=Axis->Color;
   } else {
      color=Graph->FGColor;
   }

   if (font==NULL || !color) {
      return;
   }

   glFontUse(Tk_Display(Tk_CanvasTkwin(Graph->canvas)),font);
   glColor3us(color->red,color->green,color->blue);
   glLineWidth(Axis->Width);

   /* Draw Units text*/
   if (Axis->Unit) {
      if (Side&HORIZONTAL) {
         if (Axis->Anchor==TK_ANCHOR_N || Axis->Anchor==TK_ANCHOR_W) {
            dx=X0;
         } else if (Axis->Anchor==TK_ANCHOR_S || Axis->Anchor==TK_ANCHOR_E) {
            dx=X1-Axis->UnitWidth;
         } else {
            dx=X0+(X1-X0)/2.0-Axis->UnitWidth/2.0;
         }
         dy=Axis->Pos[0]=='L'?Y0+Len-Axis->UnitHeight:Y0-Len;
         th=0;
      } else {
         if (Axis->Anchor==TK_ANCHOR_N || Axis->Anchor==TK_ANCHOR_W) {
            dy=Y1-Axis->UnitWidth;
         } else if (Axis->Anchor==TK_ANCHOR_S || Axis->Anchor==TK_ANCHOR_E) {
            dy=Y0;
         } else {
            dy=Y1+(Y0-Y1)/2.0+Axis->UnitWidth/2.0;
         }
         dx=Axis->Pos[1]=='L'?X0-Len:X0+Len-Axis->UnitHeight;
         th=270.0;
      }

      if (Axis->UnitItem) {
         ((glTextItem*)Axis->UnitItem)->x=dx;
         ((glTextItem*)Axis->UnitItem)->y=dy;
         ((glTextItem*)Axis->UnitItem)->angle=th;
         glComputeTextBbox(Graph->canvas,Axis->UnitItem);
      } else {
         glDisplayTextLayout(Axis->Text,th,dx,dy,0,-1);
      }
   }

   /* Draw increment bar */
   if (Axis->Mark!=0.0) {
      if (Axis->T0>Axis->T1) {
         i0=Axis->T1;
         i1=Axis->T0;
      } else {
         i0=Axis->T0;
         i1=Axis->T1;
      }

      mk=Axis->Type=='O'?1:Axis->Mark;
      inter=i0-fmod(i0,mk);
      if (inter<i0) inter+=mk;

      while(Axis->T0!=Axis->T1 && inter<=i1) {
         glBegin(GL_LINES);
         if (Axis->Type=='O' && inter<i1) {
            it=inter;
            for(o=1;o<10;o++) {
               it+=0.1;
               if (Side&HORIZONTAL) {
                  x=X0+AXISVALUE(Axis,pow(10,it));
                  dy=Axis->Pos[0]=='L'?10-o:-(10-o);
                  glVertex2f(x,Y0);
                  glVertex2f(x,Y0+dy);
               } else {
                  y=Y0-AXISVALUE(Axis,pow(10,it));
                  dx=Axis->Pos[1]=='L'?-(10-o):10-o;
                  glVertex2f(X0,y);
                  glVertex2f(X0+dx,y);
               }
            }
         } else {
            if (Side&HORIZONTAL) {
               x=X0+AXISVALUE(Axis,inter);
               dy=Axis->Pos[0]=='L'?5:-5;
               glVertex2f(x,Y0);
               glVertex2f(x,Y0+dy);
            } else {
               y=Y0-AXISVALUE(Axis,inter);
               dx=Axis->Pos[1]=='L'?-5:5;
               glVertex2f(X0,y);
               glVertex2f(X0+dx,y);
            }
         }
         inter+=mk;
         glEnd();
      }
   }

   /* Draw axis numbering */
   if (Axis->InterNb) {
      for(i=0;i<Axis->InterNb;i++) {
         if (Axis->Label) {
            text=Tk_ComputeTextLayout(font,Axis->Label[i],Tcl_NumUtfChars(Axis->Label[i],strlen(Axis->Label[i])),0,TK_JUSTIFY_CENTER,0,&width,&height);
         } else {
            GraphAxis_Print(Axis,buf,Axis->Inter[i],0);
            text=Tk_ComputeTextLayout(font,buf,strlen(buf),0,TK_JUSTIFY_CENTER,0,&width,&height);
         }

         if (Side&HORIZONTAL) {
            x=X0+AXISVALUE(Axis,Axis->Inter[i]); y=Y0;
            if (x<X0-1 || x>X1+1 || (Axis->Spacing && x<xp+w))
               continue;

            xp=x;
            dy=Axis->Pos[0]=='L'?10:-10;
            glBegin(GL_LINES);
               glVertex2f(x,Y0);
               glVertex2f(x,Y0+dy);
            glEnd();

            if (Axis->GridWidth && x!=X0) {
               if (Axis->GridColor)
                  glColor3us(Axis->GridColor->red,Axis->GridColor->green,Axis->GridColor->blue);
               glDash(&Axis->Dash);
               glLineWidth(Axis->GridWidth);
               glBegin(GL_LINES);
                  glVertex2f(x,Graph->yg[0]);
                  glVertex2f(x,Graph->yg[1]);
               glEnd();
               glColor3us(color->red,color->green,color->blue);
               glDisable(GL_LINE_STIPPLE);
            }
         } else {
            y=Y0-AXISVALUE(Axis,Axis->Inter[i]); x=X0;
            if (y<Y1-1 || y>Y0+1)
               continue;

            if (Axis->Spacing && y<yp+w && y>yp-w)
               continue;

            yp=y;
            dx=Axis->Pos[1]=='L'?-10:10;
            glBegin(GL_LINES);
               glVertex2f(X0,y);
               glVertex2f(X0+dx,y);
            glEnd();

            if (Axis->GridWidth && y!=Y0) {
               if (Axis->GridColor)
                  glColor3us(Axis->GridColor->red,Axis->GridColor->green,Axis->GridColor->blue);
               glDash(&Axis->Dash);
               glLineWidth(Axis->GridWidth);
               glBegin(GL_LINES);
                  glVertex2f(Graph->xg[0],y);
                  glVertex2f(Graph->xg[1],y);
               glEnd();
               glColor3us(color->red,color->green,color->blue);
               glDisable(GL_LINE_STIPPLE);
            }
         }
         w=GraphAxis_Layout(Axis,Side,width,height,&dx,&dy);
         glDisplayTextLayout(text,(int)-Axis->Angle,(int)(x+dx),(int)(y-dy),0,-1);
         Tk_FreeTextLayout(text);
      }
   } else {
      if (Axis->T0>Axis->T1) {
         i0=Axis->T1;
         i1=Axis->T0;
      } else {
         i0=Axis->T0;
         i1=Axis->T1;
      }

      incr=Axis->Incr!=0.0?Axis->Incr:RANGE_INCR(Axis->Order);
      inter=i0-fmod(i0,incr);
      if (inter<i0) inter+=incr;

      while(Axis->T0!=Axis->T1 && inter<=i1) {
         it=Axis->Type=='O'?pow(10,inter):inter;
         GraphAxis_Print(Axis,buf,it,0);
         text=Tk_ComputeTextLayout(font,buf,strlen(buf),0,TK_JUSTIFY_CENTER,0,&width,&height);

         if (Side&HORIZONTAL) {
            x=X0+AXISVALUE(Axis,it); y=Y0;

            if (Axis->Spacing && x<xp+w) {
               inter=(incr!=0.0)?(inter+incr):(inter==i1?i1*2:i1);
               continue;
            }
            xp=x;
            dy=Axis->Pos[0]=='L'?10:-10;
            glBegin(GL_LINES);
               glVertex2i(x,Y0);
               glVertex2i(x,Y0+dy);
            glEnd();

            if (Axis->GridWidth && x!=X0) {
               if (Axis->GridColor)
                  glColor3us(Axis->GridColor->red,Axis->GridColor->green,Axis->GridColor->blue);
               glDash(&Axis->Dash);
               glLineWidth(Axis->GridWidth);
               glBegin(GL_LINES);
                  glVertex2i(x,Graph->yg[0]);
                  glVertex2i(x,Graph->yg[1]);
               glEnd();
               glColor3us(color->red,color->green,color->blue);
               glDisable(GL_LINE_STIPPLE);
            }
         } else {
            y=Y0-AXISVALUE(Axis,it); x=X0;

            if (Axis->Spacing && y<yp+w && y>yp-w) {
               inter=(incr!=0.0)?(inter+incr):(inter==i1?i1*2:i1);
               continue;
            }
            yp=y;

            dx=Axis->Pos[1]=='L'?-10:10;
            glBegin(GL_LINES);
               glVertex2i(X0,y);
               glVertex2i(X0+dx,y);
            glEnd();

            if (Axis->GridWidth && y!=Y0) {
               if (Axis->GridColor)
                  glColor3us(Axis->GridColor->red,Axis->GridColor->green,Axis->GridColor->blue);
               glDash(&Axis->Dash);
               glLineWidth(Axis->GridWidth);
               glBegin(GL_LINES);
                  glVertex2i(Graph->xg[0],y);
                  glVertex2i(Graph->xg[1],y);
               glEnd();
               glColor3us(color->red,color->green,color->blue);
               glDisable(GL_LINE_STIPPLE);
            }
         }
         w=GraphAxis_Layout(Axis,Side,width,height,&dx,&dy);
         glDisplayTextLayout(text,(int)-Axis->Angle,(int)(x+dx),(int)(y-dy),0,-1);
         Tk_FreeTextLayout(text);
         inter=(incr!=0.0)?(inter+incr):(inter==i1?i1*2:i1);
      }
   }

   /* Draw Highlight */
   if (Axis->HighLightNb && Axis->HighLightWidth && Axis->HighLightColor) {
      for(i=0;i<Axis->HighLightNb;i++) {

         glLineWidth(Axis->HighLightWidth);
         glColor3us(Axis->HighLightColor->red,Axis->HighLightColor->green,Axis->HighLightColor->blue);
         glDash(&Axis->Dash);

         if (Side&HORIZONTAL) {
            x=X0+AXISVALUE(Axis,Axis->HighLight[i]);
            if (x<X0-1 || x>X1+1)
               continue;

             glBegin(GL_LINES);
               glVertex2i(x,Graph->yg[0]);
               glVertex2i(x,Graph->yg[1]);
            glEnd();
         } else {
            y=Y0-AXISVALUE(Axis,Axis->HighLight[i]);
            if (y<Y1-1 || y>Y0+1)
               continue;

            glBegin(GL_LINES);
               glVertex2i(Graph->xg[0],y);
               glVertex2i(Graph->xg[1],y);
            glEnd();
         }
      }
      glColor3us(color->red,color->green,color->blue);
      glDisable(GL_LINE_STIPPLE);
   }

   /* Draw bar */
   glLineWidth(Axis->Width);
   glBegin(GL_LINES);
      glVertex2i(X0,Y0);
      glVertex2i(X1,Y1);
   glEnd();
}

/*--------------------------------------------------------------------------------------------------------------
 * Nom          : <GraphAxis_Postscript>
 * Creation     : Mai 2005 - J.P. Gauthier - CMC/CMOE
 *
 * But          : Generer le code Postscript pour l'axe dans le graph
 *
 * Parametres   :
 *   <Interp>   : Interpreteur Tcl
 *   <Graph>    : Item graph
 *   <Axis>     : Axe
 *   <X0>       : Limite inferieure gauche du graph
 *   <Y0>       : Limite inferieure gauche du graph
 *   <X1>       : Limite superieur  droite du graph
 *   <Y1>       : Limite superieur  droite du graph
 *   <Len>      : Largeur/epaisseur de l'axe
 *   <Side>     : Orientation de l'axe (HORIZONTAL,VERTICAL)
 *
 * Retour       :
 *
 * Remarques :
 *
 *---------------------------------------------------------------------------------------------------------------
*/
void GraphAxis_Postscript(Tcl_Interp *Interp,GraphItem *Graph,TGraphAxis *Axis,int X0,int Y0,int X1,int Y1,int Len,int Side) {

   int    i,o,dx,dy,th,w=0,width,height;
   double inter,i0,i1,it,mk,x,y,xp=0,yp=0;
   char   buf[128],lbl[128];
   XColor *color;
   Tk_Font font;
   Tk_TextLayout  text;
   double coords[8];

   if (Axis->Font) {
      font=Axis->Font;
   } else {
      font=Graph->Font;
   }

   if (Axis->Color) {
      color=Axis->Color;
   } else {
      color=Graph->FGColor;
   }

   if (font==NULL || !color) {
      return;
   }

   Tk_CanvasPsFont(Interp,Graph->canvas,font);
   Tk_CanvasPsColor(Interp,Graph->canvas,color);
   sprintf(buf,"%i setlinewidth 1 setlinecap 1 setlinejoin\n",Axis->Width);
   Tcl_AppendResult(Interp,buf,(char*)NULL);

   /* Draw Units text*/
   if (Axis->Unit) {
      if (Side&HORIZONTAL) {
         if (Axis->Anchor==TK_ANCHOR_N || Axis->Anchor==TK_ANCHOR_W) {
            dx=X0;
         } else if (Axis->Anchor==TK_ANCHOR_S || Axis->Anchor==TK_ANCHOR_E) {
            dx=X1-Axis->UnitWidth;
         } else {
            dx=X0+(X1-X0)/2.0-Axis->UnitWidth/2.0;
         }
         dy=Axis->Pos[0]=='L'?Y0+Len-Axis->UnitHeight:Y0-Len;
         th=0;
      } else {
         if (Axis->Anchor==TK_ANCHOR_N || Axis->Anchor==TK_ANCHOR_W) {
            dy=Y1-Axis->UnitWidth;
         } else if (Axis->Anchor==TK_ANCHOR_S || Axis->Anchor==TK_ANCHOR_E) {
            dy=Y0;
         } else {
            dy=Y1+(Y0-Y1)/2.0;
         }
         /*Something wrong with vertical x positionning (-60)*/
         dx=Axis->Pos[1]=='L'?X0-Len-60:X0+Len-Axis->UnitHeight;
         th=270.0;
      }

      if (Axis->UnitItem) {
      } else {
         glPostscripTextLayout(Interp,Graph->canvas,Axis->Text,NULL,NULL,th,dx,dy,TK_ANCHOR_NW,Axis->Justify);
      }
  }

   /* Draw increment bar */
   if (Axis->Mark!=0.0) {
      if (Axis->T0>Axis->T1) {
         i0=Axis->T1;
         i1=Axis->T0;
      } else {
         i0=Axis->T0;
         i1=Axis->T1;
      }

      mk=Axis->Type=='O'?1:Axis->Mark;
      inter=i0-fmod(i0,mk);
      if (inter<i0) inter+=mk;

      while(Axis->T0!=Axis->T1 && inter<=i1) {
         if (Axis->Type=='O' && inter<i1) {
            it=inter;
            for(o=1;o<10;o++) {
               it+=0.1;
               if (Side&HORIZONTAL) {
                  x=X0+AXISVALUE(Axis,pow(10,it));
                  dy=Axis->Pos[0]=='L'?10-o:-(10-o);
                  SETLINE(coords,x,Y0,x,Y0+dy);
               } else {
                  y=Y0-AXISVALUE(Axis,pow(10,it));
                  dx=Axis->Pos[1]=='L'?-(10-o):10-o;
                  SETLINE(coords,X0,y,X0+dx,y);
               }
               Tk_CanvasPsPath(Interp,Graph->canvas,coords,2);
               Tcl_AppendResult(Interp,"stroke\n",(char*)NULL);
            }
         } else {
            if (Side&HORIZONTAL) {
               x=X0+AXISVALUE(Axis,inter);
               dy=Axis->Pos[0]=='L'?5:-5;
               SETLINE(coords,x,Y0,x,Y0+dy);
            } else {
               y=Y0-AXISVALUE(Axis,inter);
               dx=Axis->Pos[1]=='L'?-5:5;
               SETLINE(coords,X0,y,X0+dx,y);
            }
            Tk_CanvasPsPath(Interp,Graph->canvas,coords,2);
            Tcl_AppendResult(Interp,"stroke\n",(char*)NULL);
         }
         inter+=mk;
      }
   }

   /* Draw axis numbering */
   if (Axis->InterNb) {
      for(i=0;i<Axis->InterNb;i++) {
         if (Axis->Label) {
            text=Tk_ComputeTextLayout(font,Axis->Label[i],Tcl_NumUtfChars(Axis->Label[i],strlen(Axis->Label[i])),0,TK_JUSTIFY_CENTER,0,&width,&height);
         } else {
            GraphAxis_Print(Axis,lbl,Axis->Inter[i],0);
            text=Tk_ComputeTextLayout(font,lbl,strlen(lbl),0,TK_JUSTIFY_CENTER,0,&width,&height);
         }

         if (Side&HORIZONTAL) {
            x=X0+AXISVALUE(Axis,Axis->Inter[i]); y=Y0;
            if (x<X0-1 || x>X1+1 || (Axis->Spacing && x<xp+w))
               continue;

            dy=Axis->Pos[0]=='L'?10:-10;
            SETLINE(coords,x,Y0,x,Y0+dy);
            Tk_CanvasPsPath(Interp,Graph->canvas,coords,2);
            Tcl_AppendResult(Interp,"stroke\n",(char*)NULL);

            if (Axis->GridWidth && x!=X0) {
               if (Axis->GridColor)
                  Tk_CanvasPsColor(Interp,Graph->canvas,Axis->GridColor);
               glPostscriptDash(Interp,&Axis->Dash,Axis->GridWidth);
               SETLINE(coords,x,Graph->yg[0],x,Graph->yg[1]);
               Tk_CanvasPsPath(Interp,Graph->canvas,coords,2);
               sprintf(buf,"%i setlinewidth 1 setlinecap 1 setlinejoin\n",Axis->GridWidth);
               Tcl_AppendResult(Interp,buf," stroke\n",(char*)NULL);
               glPostscriptDash(Interp,NULL,Axis->GridWidth);
               Tk_CanvasPsColor(Interp,Graph->canvas,color);
               sprintf(buf,"%i setlinewidth 1 setlinecap 1 setlinejoin\n",Axis->Width);
               Tcl_AppendResult(Interp,buf,(char*)NULL);
            }
         } else {
            y=Y0-AXISVALUE(Axis,Axis->Inter[i]); x=X0;
            if (y<Y1-1 || y>Y0+1)
               continue;

            if (Axis->Spacing && y<yp+w && y>yp-w)
               continue;
            yp=y;

            dx=Axis->Pos[1]=='L'?-10:10;
            SETLINE(coords,X0,y,X0+dx,y);
            Tk_CanvasPsPath(Interp,Graph->canvas,coords,2);
            Tcl_AppendResult(Interp,"stroke\n",(char*)NULL);

            if (Axis->GridWidth && y!=Y0) {
               if (Axis->GridColor)
                  Tk_CanvasPsColor(Interp,Graph->canvas,Axis->GridColor);
               glPostscriptDash(Interp,&Axis->Dash,Axis->GridWidth);
               SETLINE(coords,Graph->xg[0],y,Graph->xg[1],y);
               Tk_CanvasPsPath(Interp,Graph->canvas,coords,2);
               sprintf(buf,"%i setlinewidth 1 setlinecap 1 setlinejoin\n",Axis->GridWidth);
               Tcl_AppendResult(Interp,buf," stroke\n",(char*)NULL);
               glPostscriptDash(Interp,NULL,Axis->GridWidth);
               Tk_CanvasPsColor(Interp,Graph->canvas,color);
               sprintf(buf,"%i setlinewidth 1 setlinecap 1 setlinejoin\n",Axis->Width);
               Tcl_AppendResult(Interp,buf,(char*)NULL);
            }
         }
         w=GraphAxis_Layout(Axis,Side,width,height,&dx,&dy);
         glPostscripTextLayout(Interp,Graph->canvas,text,color,NULL,(int)-Axis->Angle,x+dx,y-dy,TK_ANCHOR_NW,TK_JUSTIFY_CENTER);
         Tk_FreeTextLayout(text);
      }
   } else {
      if (Axis->T0>Axis->T1) {
         i0=Axis->T1;
         i1=Axis->T0;
      } else {
         i0=Axis->T0;
         i1=Axis->T1;
      }
      inter=i0-fmod(i0,Axis->Incr);
      if (inter<i0) inter+=Axis->Incr;

      while(Axis->T0!=Axis->T1 && inter<=i1) {
         it=Axis->Type=='O'?pow(10,inter):inter;
         GraphAxis_Print(Axis,lbl,it,0);
         text=Tk_ComputeTextLayout(font,lbl,strlen(lbl),0,TK_JUSTIFY_CENTER,0,&width,&height);

         if (Side&HORIZONTAL) {
            x=X0+AXISVALUE(Axis,it); y=Y0;
            if (Axis->Spacing && x<xp+w)
               continue;

            xp=x;
            dy=Axis->Pos[0]=='L'?10:-10;
            SETLINE(coords,x,Y0,x,Y0+dy);
            Tk_CanvasPsPath(Interp,Graph->canvas,coords,2);
            Tcl_AppendResult(Interp,"stroke\n",(char*)NULL);

            if (Axis->GridWidth && x!=X0) {
               if (Axis->GridColor)
                  Tk_CanvasPsColor(Interp,Graph->canvas,Axis->GridColor);
               glPostscriptDash(Interp,&Axis->Dash,Axis->GridWidth);
               SETLINE(coords,x,Graph->yg[0],x,Graph->yg[1]);
               Tk_CanvasPsPath(Interp,Graph->canvas,coords,2);
               sprintf(buf,"%i setlinewidth 1 setlinecap 1 setlinejoin\n",Axis->GridWidth);
               Tcl_AppendResult(Interp,buf," stroke\n",(char*)NULL);
               glPostscriptDash(Interp,NULL,Axis->GridWidth);
               Tk_CanvasPsColor(Interp,Graph->canvas,color);
               sprintf(buf,"%i setlinewidth 1 setlinecap 1 setlinejoin\n",Axis->Width);
               Tcl_AppendResult(Interp,buf,(char*)NULL);
            }
         } else {
            y=Y0-AXISVALUE(Axis,it); x=X0;
            if (Axis->Spacing && y<yp+w && y>yp-w) {
               inter=(Axis->Incr!=0.0)?(inter+Axis->Incr):(inter==i1?i1*2:i1);
               continue;
            }
            yp=y;

            dx=Axis->Pos[1]=='L'?-10:10;
            SETLINE(coords,X0,y,X0+dx,y);
            Tk_CanvasPsPath(Interp,Graph->canvas,coords,2);
            Tcl_AppendResult(Interp,"stroke\n",(char*)NULL);

            if (Axis->GridWidth && y!=Y0) {
               if (Axis->GridColor)
                  Tk_CanvasPsColor(Interp,Graph->canvas,Axis->GridColor);
               glPostscriptDash(Interp,&Axis->Dash,Axis->GridWidth);
               SETLINE(coords,Graph->xg[0],y,Graph->xg[1],y);
               Tk_CanvasPsPath(Interp,Graph->canvas,coords,2);
               sprintf(buf,"%i setlinewidth 1 setlinecap 1 setlinejoin\n",Axis->GridWidth);
               Tcl_AppendResult(Interp,buf," stroke\n",(char*)NULL);
               glPostscriptDash(Interp,NULL,Axis->GridWidth);
               Tk_CanvasPsColor(Interp,Graph->canvas,color);
               sprintf(buf,"%i setlinewidth 1 setlinecap 1 setlinejoin\n",Axis->Width);
               Tcl_AppendResult(Interp,buf,(char*)NULL);
            }
         }
         w=GraphAxis_Layout(Axis,Side,width,height,&dx,&dy);
         glPostscripTextLayout(Interp,Graph->canvas,text,color,NULL,(int)-Axis->Angle,x+dx,y-dy,TK_ANCHOR_NW,TK_JUSTIFY_CENTER);
         Tk_FreeTextLayout(text);
         inter=(Axis->Incr!=0.0)?(inter+Axis->Incr):(inter==i1?i1*2:i1);
      }
   }

   /* Draw Highlight */
   if (Axis->HighLightNb && Axis->HighLightWidth && Axis->HighLightColor) {
      for(i=0;i<Axis->HighLightNb;i++) {

         Tk_CanvasPsColor(Interp,Graph->canvas,Axis->HighLightColor);
         glPostscriptDash(Interp,&Axis->Dash,Axis->HighLightWidth);

         if (Side&HORIZONTAL) {
            x=X0+AXISVALUE(Axis,Axis->HighLight[i]);
            if (x<X0-1 || x>X1+1)
               continue;

            SETLINE(coords,x,Graph->yg[0],x,Graph->yg[1]);
            Tk_CanvasPsPath(Interp,Graph->canvas,coords,2);
         } else {
            y=Y0-AXISVALUE(Axis,Axis->HighLight[i]);
            if (y<Y1-1 || y>Y0+1)
               continue;

            SETLINE(coords,Graph->xg[0],y,Graph->xg[1],y);
            Tk_CanvasPsPath(Interp,Graph->canvas,coords,2);
         }
      }
      sprintf(buf,"%i setlinewidth 1 setlinecap 1 setlinejoin stroke\n",Axis->HighLightWidth);
      Tcl_AppendResult(Interp,buf," stroke\n",(char*)NULL);
   }

   /* Draw bar */
   SETLINE(coords,X0,Y0,X1,Y1);
   Tk_CanvasPsPath(Interp,Graph->canvas,coords,2);
   Tk_CanvasPsColor(Interp,Graph->canvas,color);
   sprintf(buf,"%i setlinewidth 1 setlinecap 1 setlinejoin\nstroke\n",Axis->Width);
   Tcl_AppendResult(Interp,buf,(char*)NULL);
}
