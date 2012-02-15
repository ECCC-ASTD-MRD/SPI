/*=========================================================
 * Environnement Canada
 * Centre Meteorologique Canadien
 * 2100 Trans-Canadienne
 * Dorval, Quebec
 *
 * Projet       : Lecture et traitements de divers fichiers de donnees
 * Fichier      : tclDataSpec.c
 * Creation     : Mai 2006 - J.P. Gauthier
 *
 * Description  : Fonctions generales de configuration d'affichage
 *                applicables a divers types de donnees.
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
 *    Boston, MA 02111-1307, USA.
 *
 *=========================================================
 */

#include <math.h>

#include "tclDataSpec.h"
#include "tclData.h"
#include "tclGDAL.h"

TCL_DECLARE_MUTEX(MUTEX_DATASPEC)

static Tcl_HashTable TDataSpec_Table;
static int           TDataSpecInit=0;
static long          TDataSpecNo=0;
static int DataSpec_Cmd(ClientData clientData,Tcl_Interp *Interp,int Objc,Tcl_Obj *CONST Objv[]);

CONST char *ICONS[]   = { "NONE","TRIANGLE","SQUARE","VBAR","HBAR","CIRCLE","PENTAGON","HEXAGON","LOZENGE","LIGHTNING","X","+","*" };
CONST char *INTERS[]  = { "NONE","INTERVAL","LINEAR","LOGARITHMIC","RSMC","AEGL(10min)","AEGL(30min)","AEGL(60min)","AEGL(4hr)","AEGL(8hr)","ERPG" };
CONST char *VECTORS[] = { "NONE","BARBULE","ARROW","STREAMLINE","STREAMLINE3D" };
CONST char *WMOS[]    = { "NONE","AUTO","N","WW","CL","CM","CH","A","UV" };

TIcon IconList[]={
 { GL_POINT,     1, { 0.0,0.0 } },
 { GL_POLYGON,   3, { -1.0,-1.0, 0.0,1.0, 1.0,-1.0 } },
 { GL_POLYGON,   4, { -1.0,-1.0, -1.0,1.0, 1.0,1.0, 1.0,-1.0 } },
 { GL_POLYGON,   4, { -0.5,-1.0, -0.5,1.0, 0.5,1.0, 0.5,-1.0 } },
 { GL_POLYGON,   4, { -1.0,-0.5, -1.0,0.5, 1.0,0.5, 1.0,-0.5 } },
 { GL_POLYGON,   12,{ 1.0,0.0, 0.87,0.50, 0.50,0.87, 0.0,1.0, -0.50,0.87, -0.87,0.50, -1.0,0.0, -0.87,-0.50, -0.50,-0.87, -0.00,-1.00, 0.50,-0.87, 0.87,-0.50 } },
 { GL_POLYGON,   5, { -0.59,-0.81, 0.59,-0.81, 0.95,0.31, 0.00,1.00, -0.95,0.31 } },
 { GL_POLYGON,   6, { -0.50,-0.87, 0.50,-0.87, 1.0,0.0, 0.5,0.87, -0.5,0.87, -1.0,0.0 } },
 { GL_POLYGON,   4, { -1.0,0.0, 0.0,1.0, 1.0,0.0, 0.0,-1.0 } },
 { GL_TRIANGLES, 12, { 1.0,1.0, -0.6,0.4, -0.2,0.0, 0.0,0.2, -0.2,0.0, 0.2,0.0, 0.2,0.0, 0.0,-0.2, -0.2,0.0,  0.6,-0.4, -1.0,-1.0, 0.2,0.0 } },
 { GL_LINES,     4, { -1.0,-1.0, 1.0,1.0, -1.0,1.0, 1.0,-1.0 } },
 { GL_LINES,     4, { -1.0,0.0, 1.0,0.0, 0.0,-1.0, 0.0,1.0 } },
 { GL_LINES,     8, { -1.0,0.0, 1.0,0.0, 0.0,-1.0, 0.0,1.0, -1.0,-1.0, 1.0,1.0, -1.0,1.0, 1.0,-1.0  } } };

/*--------------------------------------------------------------------------------------------------------------
 * Nom          : <TclDataSpec_Init>
 * Creation     : Avril 2006 J.P. Gauthier
 *
 * But          : Initialisation des commandes Tcl pour utilisation des obseravtions
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
int TclDataSpec_Init(Tcl_Interp *Interp) {

   if (!TDataSpecInit++) {
      Tcl_InitHashTable(&TDataSpec_Table,TCL_STRING_KEYS);
   }
   Tcl_CreateObjCommand(Interp,"dataspec",DataSpec_Cmd,(ClientData)NULL,(Tcl_CmdDeleteProc*)NULL);

   return(TCL_OK);
}

/*--------------------------------------------------------------------------------------------------------------
 * Nom          : <DataSpec_Cmd>
 * Creation     : Avril 2006 J.P. Gauthier
 *
 * But          : Effectuer les commandes du package
 *
 * Parametres    :
 *   <clientData>: Nom de l'observation
 *   <interp>    : Interpreteur Tcl
 *   <Objc>      : Nombre d'arguments
 *   <Objv>      : Pointeur sur la liste des arguments (word)
 *
 * Retour        : Code de retour standard TCL
 *
 * Remarques     :
 *
 *---------------------------------------------------------------------------------------------------------------
*/
static int DataSpec_Cmd(ClientData clientData,Tcl_Interp *Interp,int Objc,Tcl_Obj *CONST Objv[]) {

   TDataSpec  *spec;

   int         idx,n;
   static CONST char *sopt[] = { "create","free","copy","configure","is","all","wipe",NULL };
   enum               opt { CREATE,FREE,COPY,CONFIGURE,IS,ALL,WIPE };

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
         if(Objc<3) {
            Tcl_WrongNumArgs(Interp,2,Objv,"id");
            return(TCL_ERROR);
         }
         if (!(spec=DataSpec_Create(Interp,Tcl_GetString(Objv[2])))) {
            return(TCL_ERROR);
         }

         if (Objc>3) {
            if (DataSpec_Config(Interp,spec,Objc-3,Objv+3)==TCL_OK) {
               return(TCL_OK);
            } else {
               return(TCL_ERROR);
            }
         }
         break;

      case FREE:
         if(Objc!=3) {
            Tcl_WrongNumArgs(Interp,2,Objv,"id");
            return(TCL_ERROR);
         }
         for(n=2;n<Objc;n++) {
            DataSpec_FreeHash(Interp,Tcl_GetString(Objv[n]));
         }
         return(TCL_OK);
         break;

      case COPY:
         if(Objc!=4) {
            Tcl_WrongNumArgs(Interp,2,Objv,"id");
            return(TCL_ERROR);
         }
         return(DataSpec_Copy(Interp,Tcl_GetString(Objv[2]),Tcl_GetString(Objv[3])));
         break;

      case CONFIGURE:
         if(Objc<3) {
            Tcl_WrongNumArgs(Interp,2,Objv,"id ?option?");
            return(TCL_ERROR);
         }
         spec=DataSpec_Get(Tcl_GetString(Objv[2]));
         if (!spec) {
            Tcl_AppendResult(Interp,"DataSpec_Cmd: Invalid data specification",(char*)NULL);
            return(TCL_ERROR);
         }

         if (DataSpec_Config(Interp,spec,Objc-3,Objv+3)==TCL_OK) {
            return(TCL_OK);
         } else {
            return(TCL_ERROR);
         }
         break;

      case IS:
         if(Objc!=3) {
            Tcl_WrongNumArgs(Interp,2,Objv,"id");
            return TCL_ERROR;
         }
         if (DataSpec_Get(Tcl_GetString(Objv[2]))) {
            Tcl_SetObjResult(Interp,Tcl_NewBooleanObj(1));
         } else {
            Tcl_SetObjResult(Interp,Tcl_NewBooleanObj(0));
         }
         break;

      case ALL:
         TclY_HashAll(Interp,&TDataSpec_Table);
         break;

      case WIPE:
         TclY_HashWipe(&TDataSpec_Table,(TclY_HashFreeEntryDataFunc*)DataSpec_Free);
         break;
   }
   return TCL_OK;
}

/*----------------------------------------------------------------------------
 * Nom      : <DataSpec_Config>
 * Creation : Aout 1998 - J.P. Gauthier - CMC/CMOE
 *
 * But      : Effectue la configuration des parametres de rendu et de manipulation
 *            du champ et le retour des valeurs de configuration si il n'y a pas de
 *            valeur specifie (seulement le token).
 *
 * Parametres     :
 *  <Interp>      : Interpreteur TCL
 *  <Spec>        : Pointeur sur la Specification
 *  <Objc>        : Nombre d'arguments
 *  <Objv>        : Liste des arguments
 *
 * Retour:
 *  <TCL_...> : Code d'erreur de TCL.
 *
 * Remarques :
 *
 *----------------------------------------------------------------------------
*/
int DataSpec_Config(Tcl_Interp *Interp,TDataSpec *Spec,int Objc,Tcl_Obj *CONST Objv[]){

   Tcl_Obj  *obj,*lst;
   CMap_Rec *map;
   int       idx,i,ii,n,s=1,nobj,new;
   int       cminmax=0,cmap=0,cpos=0,cseg=0;
   char      buf[64];
   double    tmp,min=0.0,max=0.0;

   static CONST char *sopt[] = { "-active","-rendertexture","-renderparticle","-rendergrid","-rendercontour","-renderlabel","-rendercoord","-rendervector",
                                 "-rendervalue","-rendervolume","-renderface","-min","-max","-topography","-topographyfactor","-extrude","-extrudefactor",
                                 "-interpdegree","-extrapdegree","-factor","-delta","-dash","-stipple","-width","-transparency","-color","-fill",
                                 "-activefill","-outline","-activeoutline","-font","-value","-ranges","-intervals","-interlabels","-positions",
                                 "-intervalmode","-val2map","-map2val","-colormap","-desc","-unit","-sample","-sampletype","-step","-ztype","-gridvector",
                                 "-icon","-mark","-style","-mapall","-mapabove","-mapbellow","-set","-cube","-axis","-texsample","-texsize","-texres",
                                 "-interpolation","-light","-sprite","-wmo","-size","-sizemin","-sizemax","-sizevar","-mapvar","-labelvar","-mask",NULL };
   enum        opt { ACTIVE,RENDERTEXTURE,RENDERPARTICLE,RENDERGRID,RENDERCONTOUR,RENDERLABEL,RENDERCOORD,RENDERVECTOR,
                     RENDERVALUE,RENDERVOLUME,RENDERFACE,MIN,MAX,TOPOGRAPHY,TOPOGRAPHYFACTOR,EXTRUDE,EXTRUDEFACTOR,
                     INTERPDEGREE,EXTRAPDEGREE,FACTOR,DELTA,DASH,STIPPLE,WIDTH,TRANSPARENCY,COLOR,FILL,
                     ACTFILL,OUTLINE,ACTOUTLINE,FONT,VALUE,RANGES,INTERVALS,INTERLABELS,POSITIONS,
                     INTERVALMODE,VAL2MAP,MAP2VAL,COLORMAP,DESC,UNIT,SAMPLE,SAMPLETYPE,STEP,ZTYPE,GRIDVECTOR,
                     ICON,MARK,STYLE,MAPALL,MAPABOVE,MAPBELLOW,SET,CUBE,AXIS,TEXSAMPLE,TEXSIZE,TEXRES,
                     INTERPOLATION,LIGHT,SPRITE,WMO,SIZE,SIZEMIN,SIZEMAX,SIZEVAR,MAPVAR,LABELVAR,MASK };

   if (!Spec) {
      Tcl_AppendResult(Interp,"DataSpec_Config: invalid configuration object",(char*)NULL);
      return(TCL_ERROR);
   }

   if (Objc==1)  s=0;

   for(i=0;i<Objc;i++) {

      if (Tcl_GetIndexFromObj(Interp,Objv[i],sopt,"option",TCL_EXACT,&idx)!=TCL_OK) {
         return(TCL_ERROR);
      }

      switch ((enum opt)idx) {
         case SET:
            if (Objc==1) {
               Tcl_SetObjResult(Interp,Tcl_NewIntObj(Spec->Set));
            } else {
               Tcl_GetIntFromObj(Interp,Objv[++i],&Spec->Set);
               s=0;
            }
            break;

         case ACTIVE:
            if (Objc==1) {
               Tcl_SetObjResult(Interp,Tcl_NewBooleanObj(Spec->Active));
            } else {
               Tcl_GetBooleanFromObj(Interp,Objv[++i],&Spec->Active);
            }
            break;

         case RENDERTEXTURE:
            if (Objc==1) {
               Tcl_SetObjResult(Interp,Tcl_NewIntObj(Spec->RenderTexture));
            } else {
               Tcl_GetIntFromObj(Interp,Objv[++i],&Spec->RenderTexture);
            }
            break;

         case RENDERPARTICLE:
            if (Objc==1) {
               Tcl_SetObjResult(Interp,Tcl_NewIntObj(Spec->RenderParticle));
            } else {
               n=Spec->RenderParticle;
               Tcl_GetIntFromObj(Interp,Objv[++i],&Spec->RenderParticle);
               if (n!=Spec->RenderParticle && (n==0 || Spec->RenderParticle==0)) {
                  cmap=cpos=cseg=1;
               }
            }
            break;

         case RENDERGRID:
            if (Objc==1) {
               Tcl_SetObjResult(Interp,Tcl_NewIntObj(Spec->RenderGrid));
            } else {
               Tcl_GetIntFromObj(Interp,Objv[++i],&Spec->RenderGrid);
            }
            break;

         case RENDERCONTOUR:
            if (Objc==1) {
               Tcl_SetObjResult(Interp,Tcl_NewIntObj(Spec->RenderContour));
            } else {
               Tcl_GetIntFromObj(Interp,Objv[++i],&Spec->RenderContour);
               Spec->RenderContour=Spec->RenderContour<0?0:Spec->RenderContour>4?4:Spec->RenderContour;
            }
            break;

         case RENDERLABEL:
            if (Objc==1) {
               Tcl_SetObjResult(Interp,Tcl_NewIntObj(Spec->RenderLabel));
            } else {
               Tcl_GetIntFromObj(Interp,Objv[++i],&Spec->RenderLabel);
            }
            break;

         case RENDERCOORD:
            if (Objc==1) {
               Tcl_SetObjResult(Interp,Tcl_NewIntObj(Spec->RenderCoord));
            } else {
               Tcl_GetBooleanFromObj(Interp,Objv[++i],&Spec->RenderCoord);
            }
            break;

         case RENDERVECTOR:
            if (Objc==1) {
               Tcl_SetObjResult(Interp,Tcl_NewStringObj(VECTORS[Spec->RenderVector],-1));
            } else {
               if (Tcl_GetIndexFromObj(Interp,Objv[++i],VECTORS,"type",0,&Spec->RenderVector)!=TCL_OK) {
                  return(TCL_ERROR);
               }
            }
            break;

         case RENDERVALUE:
            if (Objc==1) {
               Tcl_SetObjResult(Interp,Tcl_NewIntObj(Spec->RenderValue));
            } else {
               Tcl_GetIntFromObj(Interp,Objv[++i],&Spec->RenderValue);
            }
            break;

         case RENDERFACE:
            if (Objc==1) {
               Tcl_SetObjResult(Interp,Tcl_NewBooleanObj(Spec->RenderFace));
            } else {
               Tcl_GetBooleanFromObj(Interp,Objv[++i],&Spec->RenderFace);
            }
            break;

         case RENDERVOLUME:
            if (Objc==1) {
               Tcl_SetObjResult(Interp,Tcl_NewIntObj(Spec->RenderVol));
            } else {
               Tcl_GetIntFromObj(Interp,Objv[++i],&ii);
               if (ii!=Spec->RenderVol && ii==0) {
                  cmap=cseg=1;
               }
               Spec->RenderVol=ii;
            }
            break;

         case MIN:
            if (Objc==1) {
               if (Spec->MinMax&DATASPEC_MINSET) {
                  Tcl_SetObjResult(Interp,Tcl_NewDoubleObj(VAL2SPEC(Spec,Spec->Min)));
               } else {
                  Tcl_SetObjResult(Interp,Tcl_NewStringObj("",-1));
               }
            } else {
               i++;
               if (strlen(Tcl_GetString(Objv[i]))==0) {
                  if (Spec->MinMax&DATASPEC_MINSET) {
                     Spec->MinMax&=~DATASPEC_MINSET;
                     Spec->Min=nan("NaN");
                     cminmax=1;
                  }
               } else {
                  Tcl_GetDoubleFromObj(Interp,Objv[i],&min);
                  Spec->MinMax|=DATASPEC_MINSET;
                  min=SPEC2VAL(Spec,min);
                  if (Spec->Min!=min) {
                     Spec->Min=min;
                     cminmax=1;
                  }
               }
            }
            break;

         case MAX:
            if (Objc==1) {
               if (Spec->MinMax&DATASPEC_MAXSET) {
                  Tcl_SetObjResult(Interp,Tcl_NewDoubleObj(VAL2SPEC(Spec,Spec->Max)));
               } else {
                  Tcl_SetObjResult(Interp,Tcl_NewStringObj("",-1));
               }
            } else {
               i++;
               if (strlen(Tcl_GetString(Objv[i]))==0) {
                  if (Spec->MinMax&DATASPEC_MAXSET) {
                     Spec->MinMax&=~DATASPEC_MAXSET;
                     Spec->Max=nan("NaN");
                     cminmax=1;
                  }
               } else {
                  Tcl_GetDoubleFromObj(Interp,Objv[i],&max);
                  Spec->MinMax|=DATASPEC_MAXSET;
                  max=SPEC2VAL(Spec,max);
                  if (Spec->Max!=max) {
                     Spec->Max=max;
                     cminmax=1;
                  }
               }
            }
            break;

         case ZTYPE:
            if (Objc==1) {
               Tcl_SetObjResult(Interp,Tcl_NewStringObj(Spec->ZType,-1));
            } else {
               ++i;
               if ((!Spec->ZType && strlen(Tcl_GetString(Objv[i]))) || (Spec->ZType && strcmp(Tcl_GetString(Objv[i]),Spec->ZType)!=0)) {
                  if (Spec->ZType) free(Spec->ZType);
                  Spec->ZType=NULL;
                  if (strlen(Tcl_GetString(Objv[i])) && strcmp("NONE",Tcl_GetString(Objv[i]))!=0)
                     Spec->ZType=(char*)strdup(Tcl_GetString(Objv[i]));
               }
            }
            break;

         case TOPOGRAPHY:
            if (Objc==1) {
               Tcl_SetObjResult(Interp,Tcl_NewStringObj(Spec->Topo,-1));
            } else {
               ++i;
               if ((!Spec->Topo && strlen(Tcl_GetString(Objv[i]))) || (Spec->Topo && strcmp(Tcl_GetString(Objv[i]),Spec->Topo)!=0)) {
                  if (Spec->Topo) free(Spec->Topo);
                  Spec->Topo=NULL;
                  if (strlen(Tcl_GetString(Objv[i])) && strcmp("NONE",Tcl_GetString(Objv[i]))!=0)
                     Spec->Topo=(char*)strdup(Tcl_GetString(Objv[i]));
                  cpos=1;
               }
            }
            break;

         case TOPOGRAPHYFACTOR:
            if (Objc==1) {
               Tcl_SetObjResult(Interp,Tcl_NewDoubleObj(Spec->TopoFactor));
            } else {
               Tcl_GetDoubleFromObj(Interp,Objv[++i],&tmp);
               if (tmp!=Spec->TopoFactor) {
                  cpos=1;
                  Spec->TopoFactor=tmp;
               }
            }
            break;

         case EXTRUDE:
            if (Objc==1) {
               Tcl_SetObjResult(Interp,Tcl_NewStringObj(Spec->Extrude,-1));
            } else {
               ++i;
               if ((!Spec->Extrude && strlen(Tcl_GetString(Objv[i]))) || (Spec->Extrude && strcmp(Tcl_GetString(Objv[i]),Spec->Extrude)!=0)) {
                  if (Spec->Extrude) free(Spec->Extrude);
                  Spec->Extrude=NULL;
                  if (strlen(Tcl_GetString(Objv[i])) && strcmp("NONE",Tcl_GetString(Objv[i]))!=0)
                     Spec->Extrude=(char*)strdup(Tcl_GetString(Objv[i]));
                  cpos=1;
               }
            }
            break;

         case EXTRUDEFACTOR:
            if (Objc==1) {
               Tcl_SetObjResult(Interp,Tcl_NewDoubleObj(Spec->ExtrudeFactor));
            } else {
               Tcl_GetDoubleFromObj(Interp,Objv[++i],&tmp);
               if (tmp!=Spec->ExtrudeFactor) {
                  cpos=1;
                  Spec->ExtrudeFactor=tmp;
               }
            }
            break;

         case INTERPDEGREE:
            if (Objc==1) {
               Tcl_SetObjResult(Interp,Tcl_NewStringObj(Spec->InterpDegree,-1));
            } else {
               if (strlen(Tcl_GetString(Objv[++i])) > 0) {
                  free(Spec->InterpDegree);
                  Spec->InterpDegree=(char*)strdup(Tcl_GetString(Objv[i]));
               } else {
                  Tcl_AppendResult(Interp,Interp,"DataSpec_Config: (-interpdegree) wrong value, must be \" NEAREST | LINEAR | CUBIC \" ",(char*)NULL);
                  return(TCL_ERROR);
               }
            }
            break;

         case EXTRAPDEGREE:
            if (Objc==1) {
               Tcl_SetObjResult(Interp,Tcl_NewStringObj(Spec->ExtrapDegree,-1));
            } else {
               if (strlen(Tcl_GetString(Objv[++i])) > 0) {
                  free(Spec->ExtrapDegree);
                  Spec->ExtrapDegree=(char*)strdup(Tcl_GetString(Objv[i]));
               } else {
                  Tcl_AppendResult(Interp,Interp,"DataSpec_Config: (-extrapdegree) wrong value, must be \" NEAREST | MAXIMUM | MINIMUM | LINEAR | CUBIC | VALUE \" ",(char*)NULL);
                  return(TCL_ERROR);
               }
            }
            break;

         case FACTOR:
            if (Objc==1) {
               Tcl_SetObjResult(Interp,Tcl_NewDoubleObj(Spec->ValFactor));
            } else {
               Spec->ValFactor=1.0;
               Tcl_GetDoubleFromObj(Interp,Objv[++i],&Spec->ValFactor);
               if (Spec->ValFactor==0.0)
                  Spec->ValFactor=1.0;
            }
            break;

         case DELTA:
            if (Objc==1) {
               Tcl_SetObjResult(Interp,Tcl_NewDoubleObj(Spec->ValDelta));
            } else {
               Tcl_GetDoubleFromObj(Interp,Objv[++i],&Spec->ValDelta);
            }
            break;

         case SPRITE:
            if (Objc==1) {
               Tcl_SetObjResult(Interp,Tcl_NewStringObj(Spec->Sprite,-1));
            } else {
               i++;
               if (Spec->Sprite && strcmp(Spec->Sprite,Tcl_GetString(Objv[i]))!=0) {
                  if (Spec->Sprite) free(Spec->Sprite);
                  Spec->SpriteImg=NULL;
               }
               Spec->Sprite=strdup(Tcl_GetString(Objv[i]));
               Spec->SpriteImg=Tk_FindPhoto(Interp,Spec->Sprite);
            }
            break;

         case DASH:
            if (Objc==1) {
               DashPrint(buf,&Spec->Dash);
               Tcl_AppendResult(Interp,buf,(char*)NULL);
            } else {
               Tk_GetDash(Interp,Tcl_GetString(Objv[++i]),&Spec->Dash);
            }
            break;

         case STIPPLE:
            if (Objc==1) {
               if (Spec->Stipple) {
                  Tcl_AppendResult(Interp,Spec->Stipple->Name,(char*)NULL);
               } else {
                  Tcl_AppendResult(Interp,"",(char*)NULL);
               }
            } else {
               glBitmapParseProc(NULL,Interp,Tk_MainWindow(Interp),Tcl_GetString(Objv[++i]),(char*)&Spec->Stipple,0);
            }
            break;

         case ICON:
            if (Objc==1) {
               Tcl_SetObjResult(Interp,Tcl_NewStringObj(ICONS[Spec->Icon],-1));
            } else {
               if (!strlen(Tcl_GetString(Objv[++i]))) {
                  Spec->Icon=0;
               } else if (Tcl_GetIndexFromObj(Interp,Objv[i],ICONS,"icon",TCL_EXACT,&Spec->Icon)!=TCL_OK) {
                  return(TCL_ERROR);
               }
            }
            break;

         case MARK:
            if (Objc==1) {
               if (Spec->Mark) {
                  Tcl_SetObjResult(Interp,Tcl_NewIntObj(Spec->Mark));
               }
            } else {
               Tcl_GetIntFromObj(Interp,Objv[++i],&Spec->Mark);
            }
            break;

         case STYLE:
            if (Objc==1) {
               if (Spec->Mark) {
                  Tcl_SetObjResult(Interp,Tcl_NewIntObj(Spec->Style));
               }
            } else {
               Tcl_GetIntFromObj(Interp,Objv[++i],&Spec->Style);
            }
            break;

         case FILL:
            if (Objc==1) {
               if (Spec->Fill) {
                  Tcl_AppendResult(Interp,Tk_NameOfColor(Spec->Fill),(char*)NULL);
               } else {
                  Tcl_AppendResult(Interp,"",(char*)NULL);
               }
            } else {
               if (Spec->Fill) {
                  Tk_FreeColor(Spec->Fill);
                  Spec->Fill=NULL;
               }
               if (strlen(Tcl_GetString(Objv[++i])))
                  Spec->Fill=Tk_AllocColorFromObj(Interp,Tk_MainWindow(Interp),Objv[i]);
            }
            break;

         case COLOR:
         case OUTLINE:
           if (Objc==1) {
               if (Spec->Outline) {
                  Tcl_AppendResult(Interp,Tk_NameOfColor(Spec->Outline),(char*)NULL);
               } else {
                  Tcl_AppendResult(Interp,"",(char*)NULL);
               }
            } else {
              if (Spec->Outline) {
                  Tk_FreeColor(Spec->Outline);
                  Spec->Outline=NULL;
               }
              if (strlen(Tcl_GetString(Objv[++i])))
                 Spec->Outline=Tk_AllocColorFromObj(Interp,Tk_MainWindow(Interp),Objv[i]);
           }
           break;

         case ACTOUTLINE:
            if (Objc==1) {
               if (Spec->HighLine) {
                  Tcl_AppendResult(Interp,Tk_NameOfColor(Spec->HighLine),(char*)NULL);
               } else {
                  Tcl_AppendResult(Interp,"",(char*)NULL);
               }
            } else {
               if (Spec->HighLine) {
                  Tk_FreeColor(Spec->HighLine);
                  Spec->HighLine=NULL;
               }
               if (strlen(Tcl_GetString(Objv[++i])))
                  Spec->HighLine=Tk_AllocColorFromObj(Interp,Tk_MainWindow(Interp),Objv[i]);
            }
            break;

         case ACTFILL:
            if (Objc==1) {
               if (Spec->HighFill) {
                  Tcl_AppendResult(Interp,Tk_NameOfColor(Spec->HighFill),(char*)NULL);
               } else {
                  Tcl_AppendResult(Interp,"",(char*)NULL);
               }
            } else {
               if (Spec->HighFill) {
                  Tk_FreeColor(Spec->HighFill);
                  Spec->HighFill=NULL;
               }
               if (strlen(Tcl_GetString(Objv[++i])))
                  Spec->HighFill=Tk_AllocColorFromObj(Interp,Tk_MainWindow(Interp),Objv[i]);
            }
            break;

         case FONT:
            if (Objc==1) {
               if (Spec->Font)
                  Tcl_SetObjResult(Interp,Tcl_NewStringObj(Tk_NameOfFont(Spec->Font),-1));
            } else {
               if (Spec->Font) Tk_FreeFont(Spec->Font);
               Spec->Font=Tk_AllocFontFromObj(Interp,Tk_MainWindow(Interp),Objv[++i]);
               if (Spec->Font)
                  Tk_GetFontMetrics(Spec->Font,&Spec->TKM);
            }
            break;

         case WIDTH:
            if (Objc==1) {
               Tcl_SetObjResult(Interp,Tcl_NewIntObj(Spec->Width));
            } else {
               Tcl_GetIntFromObj(Interp,Objv[++i],&Spec->Width);
            }
            break;

         case TRANSPARENCY:
            if (Objc==1) {
               Tcl_SetObjResult(Interp,Tcl_NewIntObj(Spec->Alpha));
            } else {
               Tcl_GetIntFromObj(Interp,Objv[++i],&Spec->Alpha);
            }
            break;

         case LIGHT:
            if (Objc==1) {
               Tcl_SetObjResult(Interp,Tcl_NewBooleanObj(Spec->Light));
            } else {
               Tcl_GetBooleanFromObj(Interp,Objv[++i],&Spec->Light);
            }
            break;

          case TEXRES:
            if (Objc==1) {
               Tcl_SetObjResult(Interp,Tcl_NewIntObj(Spec->TexRes));
            } else {
               Tcl_GetIntFromObj(Interp,Objv[++i],&Spec->TexRes);
               Spec->TexRes=Spec->TexRes<1?1:Spec->TexRes;
            }
            break;

          case TEXSAMPLE:
            if (Objc==1) {
               Tcl_SetObjResult(Interp,Tcl_NewIntObj(Spec->TexSample));
            } else {
               Tcl_GetIntFromObj(Interp,Objv[++i],&new);
               new=new<2?2:new>256?256:new;
               if (Spec->TexSample!=new) {
                  Spec->TexSample=new;
                  cpos=1;
               }
            }
            break;

          case TEXSIZE:
            if (Objc==1) {
               Tcl_SetObjResult(Interp,Tcl_NewIntObj(Spec->TexSize));
            } else {
               Tcl_GetIntFromObj(Interp,Objv[++i],&new);
               if (new!=32 && new!=64 && new!=128 && new!=256 && new!=512 && new!=1024 && new!=2048 && new!=4096) {
                  Tcl_AppendResult(Interp,"DataSpec_Config: Invalid texture size must be 32,64,128,256,512,1024,2048 or 4096",(char*)NULL);
                  return(TCL_ERROR);
               }
               if (Spec->TexSize!=new) {
                  Spec->TexSize=new;
                  cmap=cpos=cseg=1;
               }
            }
            break;

         case VALUE:
            if (Objc==1) {
               obj=Tcl_NewListObj(0,NULL);
               if (Spec->InterO==100) {
                  Tcl_ListObjAppendElement(Interp,obj,Tcl_NewStringObj("INTEGER",-1));
               } else if (Spec->InterO==101) {
                  Tcl_ListObjAppendElement(Interp,obj,Tcl_NewStringObj("FLOAT",-1));
               } else if (Spec->InterO==102) {
                  Tcl_ListObjAppendElement(Interp,obj,Tcl_NewStringObj("EXPONENTIAL",-1));
               } else {
                  Tcl_ListObjAppendElement(Interp,obj,Tcl_NewStringObj("AUTO",-1));
               }
               if (Spec->InterM<100) {
                  Tcl_ListObjAppendElement(Interp,obj,Tcl_NewIntObj(0));
               } else {
                  Tcl_ListObjAppendElement(Interp,obj,Tcl_NewIntObj(Spec->InterM-100));
               }
               Tcl_SetObjResult(Interp,obj);
            } else {
               i++;
               if (strcmp(Tcl_GetString(Objv[i]),"AUTO")==0) {
                  Spec->InterO=0;
               } else  if (strcmp(Tcl_GetString(Objv[i]),"INTEGER")==0) {
                  Spec->InterO=100;
               } else if (strcmp(Tcl_GetString(Objv[i]),"FLOAT")==0) {
                  Spec->InterO=101;
               } else if (strcmp(Tcl_GetString(Objv[i]),"EXPONENTIAL")==0) {
                  Spec->InterO=102;
               } else {
                  Tcl_AppendResult(Interp,"DataSpec_Config: wrong value, must be [ AUTO | INTEGER | FLOAT | EXPONENTIAL ]",(char*) NULL);
                  return(TCL_ERROR);
               }
               Tcl_GetIntFromObj(Interp,Objv[++i],&Spec->InterM);
               if (Spec->InterM>0)
                  Spec->InterM+=100;
            }
            break;

         case INTERVALMODE:
            if (Objc==1) {
               obj=Tcl_NewListObj(0,NULL);
               Tcl_ListObjAppendElement(Interp,obj,Tcl_NewStringObj(INTERS[Spec->InterMode],-1));
               Tcl_ListObjAppendElement(Interp,obj,Tcl_NewDoubleObj(Spec->InterModeParam));
               Tcl_SetObjResult(Interp,obj);
            } else {
               if (Tcl_GetIndexFromObj(Interp,Objv[++i],INTERS,"mode",0,&new)!=TCL_OK) {
                  return(TCL_ERROR);
               }
               if (i<Objc-1) {
                  if (Tcl_GetDoubleFromObj(Interp,Objv[++i],&tmp)==TCL_ERROR) {
                     i--;
                     tmp=10.0;
                  }
               }

               if (new!=Spec->InterMode || tmp!=Spec->InterModeParam) {
                  Spec->InterNb=0;
                  Spec->InterMode=new;
                  Spec->InterModeParam=tmp;
                  cmap=cseg=1;
               }
               if (Spec->InterVals) {
                  Tcl_DecrRefCount(Spec->InterVals);
                  Spec->InterVals=NULL;
               }
            }
            break;

         case INTERVALS:
            if (Objc==1) {
               if (Spec->InterVals) {
                  Tcl_SetObjResult(Interp,Spec->InterVals);
               } else {
                  obj=Tcl_NewListObj(0,NULL);
                  for (ii=0;ii<Spec->InterNb;ii++){
                     Tcl_ListObjAppendElement(Interp,obj,Tcl_NewDoubleObj(VAL2SPEC(Spec,Spec->Inter[ii])));
                  }
                  Tcl_SetObjResult(Interp,obj);
               }
            } else {
               Tcl_ListObjLength(Interp,Objv[++i],&nobj);
               if (nobj>DATASPEC_MAX) {
                  Tcl_AppendResult(Interp,"DataSpec_Config: too many levels, maximum is DATASPEC_MAX",(char*) NULL);
                  return(TCL_ERROR);
               }

               if (Spec->InterVals) {
                  Tcl_DecrRefCount(Spec->InterVals);
               }
               Spec->InterVals=Objv[i];
               Tcl_IncrRefCount(Spec->InterVals);

               /*Determine si ils sont nouveaux*/
               new=0;
               for (ii=0;ii<nobj;ii++){
                  Tcl_ListObjIndex(Interp,Objv[i],ii,&obj);
                  Tcl_GetDoubleFromObj(Interp,obj,&tmp);
                  tmp=SPEC2VAL(Spec,tmp);
                  if (nobj!=Spec->InterNb || tmp!=Spec->Inter[ii]) {
                     new=1;
                     Spec->Inter[ii]=tmp;
                  }
               }

               if (!Spec->InterMode && (new || nobj!=Spec->InterNb)) {
                  Spec->InterNb=nobj;
                  cmap=cseg=1;
               }
            }
            break;

         case INTERLABELS:
            if (Objc==1) {
               if (Spec->InterLabels)
                  Tcl_SetObjResult(Interp,Spec->InterLabels);
            } else {
               if (Spec->InterLabels) {
                  Tcl_DecrRefCount(Spec->InterLabels);
               }
               Spec->InterLabels=Objv[++i];
               Tcl_IncrRefCount(Spec->InterLabels);
            }
            break;

         case RANGES:
           if (Objc==1) {
               obj=Tcl_NewListObj(0,NULL);
               for (ii=0;ii<Spec->RangeNb;ii++){
                  Tcl_ListObjAppendElement(Interp,obj,Tcl_NewDoubleObj(Spec->Range[ii]));
               }
               Tcl_SetObjResult(Interp,obj);
            } else {
               Tcl_ListObjLength(Interp,Objv[++i],&Spec->RangeNb);
               for (ii=0;ii<Spec->RangeNb;ii++){
                  Tcl_ListObjIndex(Interp,Objv[i],ii,&obj);
                  Tcl_GetDoubleFromObj(Interp,obj,&tmp);
                  Spec->Range[ii]=tmp;
               }
            }
            break;

         case AXIS:
           if (Objc==1) {
               Tcl_SetObjResult(Interp,Tcl_NewStringObj(&Spec->Axis,1));
            } else {
               Spec->Axis=Tcl_GetString(Objv[++i])[0];
            }
            break;

         case CUBE:
           if (Objc==1) {
               obj=Tcl_NewListObj(0,NULL);
               Tcl_ListObjAppendElement(Interp,obj,Tcl_NewIntObj(Spec->Cube[0]));
               Tcl_ListObjAppendElement(Interp,obj,Tcl_NewIntObj(Spec->Cube[1]));
               Tcl_ListObjAppendElement(Interp,obj,Tcl_NewIntObj(Spec->Cube[2]));
               Tcl_ListObjAppendElement(Interp,obj,Tcl_NewIntObj(Spec->Cube[3]));
               Tcl_ListObjAppendElement(Interp,obj,Tcl_NewIntObj(Spec->Cube[4]));
               Tcl_ListObjAppendElement(Interp,obj,Tcl_NewIntObj(Spec->Cube[5]));
               Tcl_SetObjResult(Interp,obj);
            } else {
               Tcl_ListObjLength(Interp,Objv[++i],&n);
               if (n<6) {
                  Tcl_AppendResult(Interp,"DataSpec_Config: wrong value number of coordinates",(char*) NULL);
                  return(TCL_ERROR);
               }
               Tcl_ListObjIndex(Interp,Objv[i],0,&obj);
               Tcl_GetIntFromObj(Interp,obj,&Spec->Cube[0]);
               Tcl_ListObjIndex(Interp,Objv[i],1,&obj);
               Tcl_GetIntFromObj(Interp,obj,&Spec->Cube[1]);
               Tcl_ListObjIndex(Interp,Objv[i],2,&obj);
               Tcl_GetIntFromObj(Interp,obj,&Spec->Cube[2]);
               Tcl_ListObjIndex(Interp,Objv[i],3,&obj);
               Tcl_GetIntFromObj(Interp,obj,&Spec->Cube[3]);
               Tcl_ListObjIndex(Interp,Objv[i],4,&obj);
               Tcl_GetIntFromObj(Interp,obj,&Spec->Cube[4]);
               Tcl_ListObjIndex(Interp,Objv[i],5,&obj);
               Tcl_GetIntFromObj(Interp,obj,&Spec->Cube[5]);

               if (Spec->Cube[0]>Spec->Cube[3]) { n=Spec->Cube[3];Spec->Cube[3]=Spec->Cube[0];Spec->Cube[0]=n; }
               if (Spec->Cube[1]>Spec->Cube[4]) { n=Spec->Cube[4];Spec->Cube[4]=Spec->Cube[1];Spec->Cube[1]=n; }
               if (Spec->Cube[2]>Spec->Cube[5]) { n=Spec->Cube[5];Spec->Cube[5]=Spec->Cube[2];Spec->Cube[2]=n; }
            }
            break;

         case POSITIONS:
           if (Objc==1) {
               obj=Tcl_NewListObj(0,NULL);
               for (ii=0;ii<Spec->PosNb;ii++){
                  lst=Tcl_NewListObj(0,NULL);
                  Tcl_ListObjAppendElement(Interp,lst,Tcl_NewDoubleObj(Spec->Pos[ii][0]));
                  Tcl_ListObjAppendElement(Interp,lst,Tcl_NewDoubleObj(Spec->Pos[ii][1]));
                  Tcl_ListObjAppendElement(Interp,lst,Tcl_NewDoubleObj(Spec->Pos[ii][2]));
                  Tcl_ListObjAppendElement(Interp,obj,lst);
               }
               Tcl_SetObjResult(Interp,obj);
            } else {
               Tcl_ListObjLength(Interp,Objv[++i],&Spec->PosNb);
               for (ii=0;ii<Spec->PosNb;ii++){
                  Tcl_ListObjIndex(Interp,Objv[i],ii,&obj);
                  Tcl_ListObjLength(Interp,obj,&n);
                  if (n<3) {
                     Tcl_AppendResult(Interp,"DataSpec_Config: wrong value number of coordinates",(char*) NULL);
                     return TCL_ERROR;
                  }
                  Tcl_ListObjIndex(Interp,obj,0,&lst);
                  Tcl_GetDoubleFromObj(Interp,lst,&Spec->Pos[ii][0]);
                  Tcl_ListObjIndex(Interp,obj,1,&lst);
                  Tcl_GetDoubleFromObj(Interp,lst,&Spec->Pos[ii][1]);
                  Tcl_ListObjIndex(Interp,obj,2,&lst);
                  Tcl_GetDoubleFromObj(Interp,lst,&Spec->Pos[ii][2]);
               }
            }
            break;

         case VAL2MAP:
            Tcl_GetDoubleFromObj(Interp,Objv[++i],&tmp);
            if (Spec->Map) {
               tmp=SPEC2VAL(Spec,tmp);
               VAL2COL(ii,Spec,tmp);
               CMap_GetColorString(Interp,Spec->Map,ii);
            }
            break;

         case MAP2VAL:
            Tcl_GetIntFromObj(Interp,Objv[++i],&ii);
            if (Spec->Map) {
               COL2VAL(ii,Spec,tmp);
               Tcl_SetObjResult(Interp,Tcl_NewDoubleObj(VAL2SPEC(Spec,tmp)));
            }
            break;

         case COLORMAP:
            if (Objc==1) {
               if (Spec->Map) {
                  Tcl_SetObjResult(Interp,Tcl_NewStringObj(Spec->Map->Name,-1));
               }
            } else {
               map=CMap_Get(Tcl_GetString(Objv[++i]));
               if (map!=Spec->Map) {
                  CMap_Free(Spec->Map);
                  CMap_Incr(map);
                  Spec->Map=map;
                  cmap=1;
               }
            }
            break;

         case SIZE:
            if (Objc==1) {
               Tcl_SetObjResult(Interp,Tcl_NewIntObj(Spec->Size));
            } else {
               Tcl_GetDoubleFromObj(Interp,Objv[++i],&Spec->Size);
            }
            break;

         case SIZEMIN:
            if (Objc==1) {
               Tcl_SetObjResult(Interp,Tcl_NewDoubleObj(Spec->SizeMin));
            } else {
               Tcl_GetDoubleFromObj(Interp,Objv[++i],&Spec->SizeMin);
            }
            break;

         case SIZEMAX:
            if (Objc==1) {
               Tcl_SetObjResult(Interp,Tcl_NewDoubleObj(Spec->SizeMax));
            } else {
               Tcl_GetDoubleFromObj(Interp,Objv[++i],&Spec->SizeMax);
            }
            break;

         case MAPVAR:
            if (Objc==1) {
               Tcl_SetObjResult(Interp,Tcl_NewStringObj(Spec->MapVar,-1));
            } else {
               if (Spec->MapVar) {
                  free(Spec->MapVar);
                  Spec->MapVar=NULL;
               }
               ++i;
               if (strlen(Tcl_GetString(Objv[i])))
                  Spec->MapVar=strdup(Tcl_GetString(Objv[i]));
            }
            break;

         case SIZEVAR:
            if (Objc==1) {
               Tcl_SetObjResult(Interp,Tcl_NewStringObj(Spec->SizeVar,-1));
            } else {
               if (Spec->SizeVar) {
                  free(Spec->SizeVar);
                  Spec->SizeVar=NULL;
               }
               ++i;
               if (strlen(Tcl_GetString(Objv[i])))
                  Spec->SizeVar=strdup(Tcl_GetString(Objv[i]));
            }
            break;

         case LABELVAR:
            if (Objc==1) {
               Tcl_SetObjResult(Interp,Tcl_NewStringObj(Spec->LabelVar,-1));
            } else {
               if (Spec->LabelVar) {
                  free(Spec->LabelVar);
                  Spec->LabelVar=NULL;
               }
               ++i;
               if (strlen(Tcl_GetString(Objv[i])))
                  Spec->LabelVar=strdup(Tcl_GetString(Objv[i]));
            }
            break;

         case SAMPLE:
            if (Objc==1) {
               Tcl_SetObjResult(Interp,Tcl_NewIntObj(Spec->Sample));
            } else {
               Tcl_GetIntFromObj(Interp,Objv[++i],&Spec->Sample);
            }
            break;

         case SAMPLETYPE:
            if (Objc==1) {
               if (Spec->SampleType=='G') {
                  Tcl_SetObjResult(Interp,Tcl_NewStringObj("GRID",-1));
               } else {
                  Tcl_SetObjResult(Interp,Tcl_NewStringObj("PIXEL",-1));
               }
            } else {
               i++;
               if (strcmp(Tcl_GetString(Objv[i]),"GRID")==0) {
                  Spec->SampleType='G';
               } else  if (strcmp(Tcl_GetString(Objv[i]),"PIXEL")==0) {
                  Spec->SampleType='P';
               } else {
                  Tcl_AppendResult(Interp,"DataSpec_Config: wrong value, must be [ GRID | PIXEL ]",(char*) NULL);
                  return(TCL_ERROR);
               }
            }
            break;

         case STEP:
            if (Objc==1) {
               Tcl_SetObjResult(Interp,Tcl_NewDoubleObj(Spec->Step));
            } else {
               Tcl_GetDoubleFromObj(Interp,Objv[++i],&Spec->Step);
               Spec->Step=Spec->Step<0?0.001:Spec->Step;
            }
            break;

         case UNIT:
            if (Objc==1) {
               Tcl_SetObjResult(Interp,Tcl_NewStringObj(Spec->Unit,-1));
            } else {
               if (Spec->Unit) free(Spec->Unit);
               Spec->Unit=strdup(Tcl_GetString(Objv[++i]));
            }
            break;

         case DESC:
            if (Objc==1) {
               Tcl_SetObjResult(Interp,Tcl_NewStringObj(Spec->Desc,-1));
           } else {
               if (Spec->Desc) free(Spec->Desc);
               Spec->Desc=strdup(Tcl_GetString(Objv[++i]));
            }
            break;

         case INTERPOLATION:
            if (Objc==1) {
               switch(Spec->Interp) {
                  case GL_NEAREST: Tcl_SetObjResult(Interp,Tcl_NewStringObj("NEAREST",-1)); break;
                  case GL_LINEAR : Tcl_SetObjResult(Interp,Tcl_NewStringObj("LINEAR",-1)); break;
               }
            } else {
               i++;
               if (strcmp(Tcl_GetString(Objv[i]),"NEAREST")==0) {
                  Spec->Interp=GL_NEAREST;
               } else if (strcmp(Tcl_GetString(Objv[i]),"LINEAR")==0) {
                  Spec->Interp=GL_LINEAR;
               } else {
                  Tcl_AppendResult(Interp,"DataSpec_Config: Interpolation type unknown, must be NEAREST or LINEAR",(char*)NULL);
                  return(TCL_ERROR);
               }
            }
            break;

         case MAPALL:
            if (Objc==1) {
               Tcl_SetObjResult(Interp,Tcl_NewBooleanObj(Spec->MapAll));
            } else {
               Tcl_GetBooleanFromObj(Interp,Objv[++i],&Spec->MapAll);
            }
            break;

         case MAPABOVE:
            if (Objc==1) {
               Tcl_SetObjResult(Interp,Tcl_NewBooleanObj(Spec->MapAbove));
            } else {
               Tcl_GetBooleanFromObj(Interp,Objv[++i],&Spec->MapAbove);
            }
            break;

         case MAPBELLOW:
            if (Objc==1) {
               Tcl_SetObjResult(Interp,Tcl_NewBooleanObj(Spec->MapBellow));
            } else {
               Tcl_GetBooleanFromObj(Interp,Objv[++i],&Spec->MapBellow);
            }
            break;

         case GRIDVECTOR:
            if (Objc==1) {
               Tcl_SetObjResult(Interp,Tcl_NewBooleanObj(Spec->GridVector));
            } else {
               Tcl_GetBooleanFromObj(Interp,Objv[++i],&Spec->GridVector);
            }
            break;

         case WMO:
            if (Objc==1) {
               Tcl_SetObjResult(Interp,Tcl_NewStringObj(WMOS[Spec->WMO],-1));
            } else {
               if (Tcl_GetIndexFromObj(Interp,Objv[++i],WMOS,"type",0,&Spec->WMO)!=TCL_OK) {
                  return(TCL_ERROR);
               }
            }
            break;

         case MASK:
            if (Objc==1) {
               Tcl_SetObjResult(Interp,Tcl_NewStringObj("",-1));
            } else {
               Spec->OGRMask=OGR_GeometryGet(Tcl_GetString(Objv[++i]));
            }
            break;

      }
   }

   /*Assigner les limites d'affichage*/
   if (cminmax) {
      if (Spec->Min==Spec->Max) {
         Spec->MinMax=DATASPEC_NOTSET;
      }
      cmap=1;
   }

   /*Cleanup des tableaux*/
   if (cmap || cpos || cseg) {
      DataSpec_Clean(Spec,cmap,cpos,cseg);
   }

   if (s)
      Spec->Set=1;

   DataSpec_Define(Spec);
   return(TCL_OK);
}

/*----------------------------------------------------------------------------
 * Nom      : <Data_SpecClean>
 * Creation : Mars 2007 - J.P. Gauthier - CMC/CMOE
 *
 * But      : Reinitialiser les structures des donnees suite a une reconfiguration
 *            des parametres de rendus.
 *
 * Parametres :
 *  <Spec>    : Reference
 *  <Map>     : Clean de la Texture
 *  <Pos>     : Clean des Positions
 *  <Seg>     : Clean des Contours
 *
 * Retour:
 *
 * Remarques :
 *
 *----------------------------------------------------------------------------
*/
void DataSpec_Clean(TDataSpec *Spec,int Map,int Pos,int Seg) {

   Data_CleanAll(Spec,Map,Pos,Seg);
   GDAL_BandCleanAll(Spec,Map,Pos,Seg);
   OGR_LayerCleanAll(Spec,Map,Pos,Seg);
}

/*--------------------------------------------------------------------------------------------------------------
 * Nom          : <DataSpec_Create>
 * Creation     : Avril 2006 J.P. Gauthier
 *
 * But          : Creation d'un objet de configurationo et insertion d'un nouveau nom dans la table.
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
TDataSpec *DataSpec_Create(Tcl_Interp *Interp,char *Name) {

   Tcl_HashEntry *entry;
   TDataSpec     *spec;
   char           buf[64];
   int            new;

  if (!Name) {
      Tcl_MutexLock(&MUTEX_DATASPEC);
      /*Check for non-existing name*/
      sprintf(buf,"DATASPEC_____%li",TDataSpecNo++);
      while (TclY_HashGet(&TDataSpec_Table,buf)) {
         sprintf(buf,"DATASPEC_____%li",TDataSpecNo++);
      }
      Tcl_MutexUnlock(&MUTEX_DATASPEC);
      Name=buf;
   }
   entry=TclY_CreateHashEntry(&TDataSpec_Table,Name,&new);

   if (!new) {
      Tcl_AppendResult(Interp,"\n   DataSpec_Create: Configuration object name already used: \"",Name, "\"",(char*)NULL);
      Tcl_MutexLock(&MUTEX_DATASPEC);
     ((TDataSpec*)Tcl_GetHashValue(entry))->NRef++;
      Tcl_MutexUnlock(&MUTEX_DATASPEC);
      return((TDataSpec*)Tcl_GetHashValue(entry));
   }

   spec=DataSpec_New();
   spec->Name=strdup(Name);

   if (!spec) {
      Tcl_AppendResult(Interp,"\n   DataSpec_Create : Could not allocate memory for a configuration object",(char*)NULL);
      return(NULL);
   }
   Tcl_SetHashValue(entry,spec);

   return(spec);
}

/*--------------------------------------------------------------------------------------------------------------
 * Nom          : <DataSpec_Get>
 * Creation     : Avril 2006 J.P. Gauthier
 *
 * But          : Obtenir un objet de configurationn en fonction de son nom dans la table
 *
 * Parametres   :
 *   <Name>     : Nom de l'objet a obtenir.
 *
 * Retour       : Une structure ou un pointeur NULL si rien trouve.
 *
 * Remarques    :
 *
 *---------------------------------------------------------------------------------------------------------------
*/
TDataSpec* DataSpec_Get(char *Name) {
   return((TDataSpec*)TclY_HashGet(&TDataSpec_Table,Name));
}

/*--------------------------------------------------------------------------------------------------------------
 * Nom          : <DataSpec_FreeHash>
 * Creation     : Avril 2006 J.P. Gauthier
 *
 * But          : Destruction d'un objet a partir de son nom.
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
int DataSpec_FreeHash(Tcl_Interp *Interp,char *Name) {

   Tcl_HashEntry *entry;

   entry=TclY_FindHashEntry(&TDataSpec_Table,Name);

   if (!entry) {
      Tcl_AppendResult(Interp,"\n   DataSpec_FreeHash:  Configuration object name unknown: \"",Name,"\"",(char*)NULL);
      return(TCL_ERROR);
   } else {
      if (DataSpec_Free((TDataSpec*)Tcl_GetHashValue(entry))) {
         TclY_DeleteHashEntry(entry);
      }
   }
   return(TCL_OK);
}

/*----------------------------------------------------------------------------
 * Nom      : <DataSpec_Copy>
 * Creation : Fevrier 2003- J.P. Gauthier - CMC/CMOE
 *
 * But      : Copier une structure TDataSpec.
 *
 * Parametres :
 *   <Interp>   : Interpreteur Tcl
 *   <To>       : Nom de la configuration destination
 *   <From>     : Nom de la configuration source
 *
 * Retour    :Code de retour standard TCL
 *
 * Remarques :
 *    Si la detsination existe deja, la copie se fait et les valeurs de la destination
 *    sont ecrasee sans bronchement
 *
 *----------------------------------------------------------------------------
*/
int DataSpec_Copy(Tcl_Interp *Interp,char *To,char *From){

   TDataSpec *from,*to;
   char       buf[64];

  if (!(from=DataSpec_Get(From))) {
      Tcl_AppendResult(Interp,"\n   DataSpec_Copy:  Invalid configuration object: \"",From,"\"",(char*)NULL);
      return(TCL_ERROR);
   }

   if (!(to=DataSpec_Get(To))) {
      if (!(to=DataSpec_Create(Interp,To))) {
         return(TCL_ERROR);
      }
   }

   if (to->ZType)     free(to->ZType);
   if (to->Topo)      free(to->Topo);
   if (to->Extrude)   free(to->Extrude);
   if (to->Desc)      free(to->Desc);
   if (to->Unit)      free(to->Unit);
   if (to->Sprite)    free(to->Sprite);
   if (to->LabelVar)  free(to->LabelVar);
   if (to->SizeVar)   free(to->SizeVar);
   if (to->MapVar)    free(to->SizeVar);
   if (to->Outline)   Tk_FreeColor(to->Outline);
   if (to->Fill)      Tk_FreeColor(to->Fill);
   if (to->HighLine)  Tk_FreeColor(to->HighLine);
   if (to->HighFill)  Tk_FreeColor(to->HighFill);
   if (to->Font)      Tk_FreeFont(to->Font);

   if (to->InterpDegree) free(to->InterpDegree);
   if (to->ExtrapDegree) free(to->ExtrapDegree);

   to->Active=from->Active;
   to->OGRMask=from->OGRMask;
   to->SpriteImg=from->SpriteImg;
   to->TopoFactor=from->TopoFactor;
   to->ExtrudeFactor=from->ExtrudeFactor;
   to->Set=from->Set;

   to->Map=from->Map;
   CMap_Incr(to->Map);

   to->MapFactor=from->MapFactor;
   to->MapAll=from->MapAll;
   to->MapAbove=from->MapAbove;
   to->MapBellow=from->MapBellow;
   to->Icon=from->Icon;
   to->Mark=from->Mark;
   to->Style=from->Style;
   memcpy(to->Cube,from->Cube,6*sizeof(int));
   memcpy(to->Range,from->Range,from->RangeNb*sizeof(float));
   memcpy(to->Inter,from->Inter,from->InterNb*sizeof(float));
   memcpy(to->Pos,from->Pos,from->PosNb*sizeof(Vect3d));

   if (from->InterLabels)
      Tcl_IncrRefCount(from->InterLabels);
   to->InterLabels=from->InterLabels;

   if (from->InterVals)
      Tcl_IncrRefCount(from->InterVals);
   to->InterVals=from->InterVals;

   to->RangeNb=from->RangeNb;
   to->PosNb=from->PosNb;
   to->InterNb=from->InterNb;
   to->InterMode=from->InterMode;
   to->InterModeParam=from->InterModeParam;
   to->InterO=from->InterO;
   to->InterM=from->InterM;
   to->Axis=from->Axis;

   to->TexRes=from->TexRes;
   to->TexSize=from->TexSize;
   to->TexSample=from->TexSample;
   to->Interp=from->Interp;
   to->Alpha=from->Alpha;
   to->Light=from->Light;
   to->Width=from->Width;
   to->Size=from->Size;
   to->SizeMin=from->SizeMin;
   to->SizeMax=from->SizeMax;
   to->Sample=from->Sample;
   to->SampleType=from->SampleType;
   to->Step=from->Step;
   to->Min=from->Min;
   to->Max=from->Max;
   to->MinMax=from->MinMax;
   to->ValFactor=from->ValFactor;
   to->ValDelta=from->ValDelta;
   to->InterpDegree=strdup(from->InterpDegree);
   to->ExtrapDegree=strdup(from->ExtrapDegree);
   to->RenderTexture=from->RenderTexture;
   to->RenderFace=from->RenderFace;
   to->RenderGrid=from->RenderGrid;
   to->RenderContour=from->RenderContour;
   to->RenderCoord=from->RenderCoord;
   to->RenderLabel=from->RenderLabel;
   to->RenderParticle=from->RenderParticle;
   to->RenderVector=from->RenderVector;
   to->RenderValue=from->RenderValue;
   to->RenderVol=from->RenderVol;
   to->GridVector=from->GridVector;
   to->WMO=from->WMO;

   to->ZType=NULL;
   if (from->ZType)
      to->ZType=strdup(from->ZType);

   to->Topo=NULL;
   if (from->Topo)
      to->Topo=strdup(from->Topo);

   to->Extrude=NULL;
   if (from->Extrude)
      to->Extrude=strdup(from->Extrude);

   to->Dash.number=0;
   if (from->Dash.number) {
      DashPrint(buf,&from->Dash);
      Tk_GetDash(Interp,buf,&to->Dash);
   }

   to->Stipple=NULL;
   if (from->Stipple)
      glBitmapParseProc(NULL,Interp,Tk_MainWindow(Interp),from->Stipple->Name,(char*)&to->Stipple,0);

   to->Font=NULL;
   if (from->Font) {
      to->Font=Tk_AllocFontFromObj(Interp,Tk_MainWindow(Interp),Tcl_NewStringObj(Tk_NameOfFont(from->Font),-1));
      Tk_GetFontMetrics(to->Font,&to->TKM);
   }
   to->Fill=NULL;
   if (from->Fill)
      to->Fill=Tk_AllocColorFromObj(Interp,Tk_MainWindow(Interp),Tcl_NewStringObj(Tk_NameOfColor(from->Fill),-1));

   to->HighFill=NULL;
   if (from->HighFill)
      to->HighFill=Tk_AllocColorFromObj(Interp,Tk_MainWindow(Interp),Tcl_NewStringObj(Tk_NameOfColor(from->HighFill),-1));

   to->HighLine=NULL;
   if (from->HighLine)
      to->HighLine=Tk_AllocColorFromObj(Interp,Tk_MainWindow(Interp),Tcl_NewStringObj(Tk_NameOfColor(from->HighLine),-1));

   to->Outline=NULL;
   if (from->Outline)
      to->Outline=Tk_AllocColorFromObj(Interp,Tk_MainWindow(Interp),Tcl_NewStringObj(Tk_NameOfColor(from->Outline),-1));

   to->Desc=NULL;
   if (from->Desc)
      to->Desc=strdup(from->Desc);

   to->Unit=NULL;
   if (from->Unit)
      to->Unit=strdup(from->Unit);

   to->Sprite=NULL;
   if (from->Sprite)
      to->Sprite=strdup(from->Sprite);

   to->LabelVar=NULL;
   if (from->LabelVar)
      to->LabelVar=strdup(from->LabelVar);

   to->SizeVar=NULL;
   if (from->SizeVar)
      to->SizeVar=strdup(from->SizeVar);

   to->MapVar=NULL;
   if (from->MapVar)
      to->MapVar=strdup(from->MapVar);

   return(TCL_OK);
}

/*----------------------------------------------------------------------------
 * Nom      : <DataSpec_New>
 * Creation : Fevrier 2003- J.P. Gauthier - CMC/CMOE
 *
 * But      : Initialiser la structure TDataSpec.
 *
 * Parametres :
 *
 * Retour:
 *  <Spec>   : Nouvelle structure
 *
 * Remarques :
 *
 *----------------------------------------------------------------------------
*/
TDataSpec *DataSpec_New(){

   TDataSpec *spec;

   spec=(TDataSpec*)malloc(sizeof(TDataSpec));

   spec->Active=1;
   spec->NRef=1;
   spec->Set=0;
   spec->Name=NULL;
   spec->Map=NULL;
   spec->MapAll=0;
   spec->MapAbove=1;
   spec->MapBellow=0;
   spec->Outline=NULL;
   spec->Fill=NULL;
   spec->HighFill=NULL;
   spec->HighLine=NULL;
   spec->Font=NULL;
   spec->Desc=NULL;
   spec->Unit=NULL;
   spec->Sprite=NULL;
   spec->SpriteImg=NULL;
   spec->Icon=0;
   spec->Mark=0;
   spec->Style=0;
   spec->Stipple=NULL;
   spec->RangeNb=0;
   spec->PosNb=0;
   spec->InterNb=0;
   spec->InterMode=0;
   spec->InterLabels=NULL;
   spec->InterVals=NULL;
   spec->InterModeParam=0.0;
   spec->InterO=0;
   spec->InterM=0;
   spec->Interp=GL_NEAREST;
   spec->TexRes=1;
   spec->TexStep=0.0;
   spec->TexSample=8;
   spec->TexSize=256;
   spec->Alpha=100;
   spec->Light=0;
   spec->Width=1;
   spec->Size=10.0;
   spec->SizeMin=spec->SizeMax=0.0;
   spec->LabelVar=NULL;
   spec->SizeVar=NULL;
   spec->MapVar=NULL;
   spec->Sample=4;
   spec->SampleType='P';
   spec->Step=0.25;
   spec->Min=nan("NaN");
   spec->Max=nan("NaN");
   spec->MinMax=DATASPEC_NOTSET;
   spec->ValFactor=1.0;
   spec->ValDelta=0.0;
   spec->MapFactor=0.0f;
   spec->GridVector=1;
   spec->Cube[0]=1;spec->Cube[1]=1;spec->Cube[2]=1;
   spec->Cube[3]=10;spec->Cube[4]=1;spec->Cube[5]=1;
   spec->Axis='X';

   spec->OGRMask=NULL;
   spec->ZType=NULL;
   spec->Topo=NULL;
   spec->TopoFactor=1.0;
   spec->Extrude=NULL;
   spec->ExtrudeFactor=1.0;
   spec->InterpDegree=(char*)strdup("LINEAR");
   spec->ExtrapDegree=(char*)strdup("NEUTRAL");
   spec->Dash.number=0;
   spec->RenderTexture=0;
   spec->RenderFace=1;
   spec->RenderGrid=0;
   spec->RenderContour=0;
   spec->RenderCoord=0;
   spec->RenderLabel=0;
   spec->RenderParticle=0;
   spec->RenderVector=VNONE;
   spec->RenderValue=0;
   spec->RenderVol=0;
   spec->WMO=0;

   return(spec);
}

/*----------------------------------------------------------------------------
 * Nom      : <DataSpec_Free>
 * Creation : Fevrier 2003- J.P. Gauthier - CMC/CMOE
 *
 * But      : Supprimenr une structure TDataSpec.
 *
 * Parametres :
 *  <Spec>    : Structure a liberer
 *
 * Retour:
 *
 * Remarques :
 *
 *----------------------------------------------------------------------------
*/
int DataSpec_Free(TDataSpec *Spec){

  Tcl_MutexLock(&MUTEX_DATASPEC);
  if (!Spec || --Spec->NRef) {
      Tcl_MutexUnlock(&MUTEX_DATASPEC);
      return(0);
   }
   Tcl_MutexUnlock(&MUTEX_DATASPEC);

   if (Spec->Name)        free(Spec->Name);
   if (Spec->ZType)       free(Spec->ZType);
   if (Spec->Topo)        free(Spec->Topo);
   if (Spec->Extrude)     free(Spec->Extrude);
   if (Spec->Desc)        free(Spec->Desc);
   if (Spec->Unit)        free(Spec->Unit);
   if (Spec->Sprite)      free(Spec->Sprite);
   if (Spec->Outline)     Tk_FreeColor(Spec->Outline);
   if (Spec->Fill)        Tk_FreeColor(Spec->Fill);
   if (Spec->HighLine)    Tk_FreeColor(Spec->HighLine);
   if (Spec->HighFill)    Tk_FreeColor(Spec->HighFill);
   if (Spec->Font)        Tk_FreeFont(Spec->Font);

   if (Spec->InterLabels) Tcl_DecrRefCount(Spec->InterLabels);
   if (Spec->InterVals)   Tcl_DecrRefCount(Spec->InterVals);

   if (Spec->InterpDegree) free(Spec->InterpDegree);
   if (Spec->ExtrapDegree) free(Spec->ExtrapDegree);

   if (Spec->Map) CMap_Free(Spec->Map);

   free(Spec);
   return(1);
}

/*----------------------------------------------------------------------------
 * Nom      : <DataSpec_Define>
 * Creation : Fevrier 2003- J.P. Gauthier - CMC/CMOE
 *
 * But      : Calcule le facteur de transformation de la palette vers les valeurs
 *            du champs et les formats d'affichages des valeurs.
 *
 * Parametres :
 *  <Spec>    : Structure a definir
 *
 * Retour:
 *
 * Remarques :
 *
 *----------------------------------------------------------------------------
*/
void DataSpec_Define(TDataSpec *Spec){

   int    i=0;
   double o1,o2;

   if (!Spec) {
      return;
   }
   Spec->MapFactor=0.0;

   if (isnan(Spec->Min) || isnan(Spec->Max)) {
      return;
   }

   /*Redefinir le facteur de conversion de la palette*/
   if (Spec->Map) {
      if (Spec->InterNb>0) {
         Spec->MapFactor=(double)Spec->Map->NbPixels/(Spec->InterNb+1);

         if (Spec->InterO<100) {
            for(i=0;i<Spec->InterNb;i++){
               if (Spec->Inter[i]!=0.0) {
                  o1=log10(fabs(Spec->Inter[i]));
                  Spec->InterO=fabs(o1)>fabs(Spec->InterO)?o1:Spec->InterO;
               }
            }
         }
         if (Spec->InterM<100) {
            Spec->InterM=-(floor(log10(fabs((Spec->Inter[Spec->InterNb-1]-Spec->Inter[0])/10.0))));
         }
      } else {
         Spec->MapFactor=(double)Spec->Map->NbPixels/(Spec->Max-Spec->Min);
         if (!finite(Spec->MapFactor)) {
            Spec->MapFactor=0.0;
         }
         if (Spec->InterO<100) {
            o1=log10(fabs(Spec->Max!=0.0?Spec->Max:1.0));
            o2=log10(fabs(Spec->Min!=0.0?Spec->Min:1.0));
            Spec->InterO=fabs(o1)>fabs(o2)?o1:o2;
         }
         if (Spec->InterM<100) {
            if (Spec->Max==Spec->Min) {
               Spec->InterM=0;
            } else {
              Spec->InterM=-(floor(log10(fabs((Spec->Max-Spec->Min)/100.0))));
            }
         }
      }
   }
}

void DataSpec_IntervalsMod(TDataSpec *Spec,double Min,double Max) {

   double v,d;

   Spec->InterNb=0;

   if (Min<Max) {
      d=SPEC2VAL(Spec,Spec->InterModeParam);
      if (Spec->InterModeParam>0.0) {
         Min=ceil(Min/d)*d;

         for(v=Min;v<Max;v+=d) {
            Spec->Inter[Spec->InterNb++]=v;
            if (Spec->InterNb>=256) {
               break;
            }
         }
      }
   } else {
      Spec->Inter[Spec->InterNb++]=Min;
   }
}

void DataSpec_IntervalsLinear(TDataSpec *Spec,double Min,double Max) {

   int    i;
   double d;

   Spec->InterNb=Spec->InterModeParam;
   d=(Max-Min)/Spec->InterNb;

   for(i=0;i<Spec->InterNb;i++) {
      Spec->Inter[i]=Min+i*d;
   }
}

void DataSpec_IntervalsLogList(TDataSpec *Spec,double Min,double Max,double Delta) {

   int min,max;

   max=Max==0.0?0:ceil(log10(Max));
   min=Min==0.0?0:floor(log10(Min))-(Delta<0?Delta:0.0);

   if (Min==0.0 && max<=1 && !Spec->InterNb) {
      min=max-5;
      Delta=1;
   }

   if (Min==0)
      Spec->Inter[Spec->InterNb++]=0.0;

   while(Min>Max?min>=max:min<max) {
      Spec->Inter[Spec->InterNb++]=Delta*pow(10,min);
      min+=Delta;
   }
}

void DataSpec_IntervalsLog(TDataSpec *Spec,double Min,double Max) {

   Spec->InterNb=0;

   if (Max<=0) {                    /*Si les valeurs sont negatives*/
      DataSpec_IntervalsLogList(Spec,fabs(Min),fabs(Max),-1);
   } else if (Min<0) {              /*Si les valeurs vont du negatif au positifs*/
      DataSpec_IntervalsLogList(Spec,fabs(Min),0,-1);
      DataSpec_IntervalsLogList(Spec,0,Max,1);
   } else {                         /*Si les valeurs sont positives*/
      DataSpec_IntervalsLogList(Spec,Min,Max,1);
   }
}

void DataSpec_IntervalsRSMC(TDataSpec *Spec,double Min,double Max) {

   int exp,dexp,base;

   base=floor(log10(Max));

   /*Delta entre les contours, 10 ou 100*/
   dexp=(Max/Min)>1e9?2:1;

   Spec->InterNb=0;
   for(exp=base; exp>=base-3*dexp ;exp-=dexp) {
      if (exp<=-20) {
         Spec->Inter[255-Spec->InterNb++]=pow(10,-20);
         break;
      } else {
         Spec->Inter[255-Spec->InterNb++]=pow(10,exp);
      }
   }

   /*Invert list cause we need it increasing*/
   for(exp=0; exp<Spec->InterNb ; exp++) {
      Spec->Inter[Spec->InterNb-exp-1]=Spec->Inter[255-exp];
   }
}

void DataSpec_Intervals(TDataSpec *Spec,double Min,double Max) {

   if (Spec->InterMode) {
      switch(Spec->InterMode) {
         case 1:DataSpec_IntervalsMod(Spec,Min,Max); break;
         case 2:DataSpec_IntervalsLinear(Spec,Min,Max); break;
         case 3:DataSpec_IntervalsLog(Spec,Min,Max); break;
         case 4:DataSpec_IntervalsRSMC(Spec,Min,Max); break;
      }
   }
}

/*----------------------------------------------------------------------------
 * Nom      : <DataSpec_Format>
 * Creation : Aout 1999 - J.P. Gauthier - CMC/CMOE
 *
 * But      : Generer un chaine de caractere pour une valeur.
 *
 * Parametres :
 *  <Spec>     : Spec structure.
 *  <Val>      : Valeur a convertir.
 *  <Str>      : Chaine convertie.
 *
 * Retour:
 *
 * Remarques :
 *
 *----------------------------------------------------------------------------
*/
void DataSpec_Format(TDataSpec *Spec,double Val,char *Str){

   int o,m=Spec->InterM;

  /*Mode automatique ou selection de l'usager*/
   if (Spec->InterO >= 100) {
      o=Spec->InterO-100;
   } else {

      /*Affichage Exponent*/
      if (Spec->InterO>3 || Spec->InterO<-2) {
         m+=Spec->InterO;
         o=2;
      } else {
         o=1;
      }
   }

   if (Spec->InterM >= 100) {
      m=Spec->InterM-100;
   }

   switch(o) {
      case 1: switch(m) {
            case 10: sprintf(Str,"%.10f",Val);break;
            case  9: sprintf(Str,"%.9f",Val);break;
            case  8: sprintf(Str,"%.8f",Val);break;
            case  7: sprintf(Str,"%.7f",Val);break;
            case  6: sprintf(Str,"%.6f",Val);break;
            case  5: sprintf(Str,"%.5f",Val);break;
            case  4: sprintf(Str,"%.4f",Val);break;
            case  3: sprintf(Str,"%.3f",Val);break;
            case  2: sprintf(Str,"%.2f",Val);break;
            case  1: sprintf(Str,"%.1f",Val);break;
            default: sprintf(Str,"%.0f",nearbyint(Val));
         }
         break;
      case 2: switch(m) {
            case 10: sprintf(Str,"%.10e",Val);break;
            case  9: sprintf(Str,"%.9e",Val);break;
            case  8: sprintf(Str,"%.8e",Val);break;
            case  7: sprintf(Str,"%.7e",Val);break;
            case  6: sprintf(Str,"%.6e",Val);break;
            case  5: sprintf(Str,"%.5e",Val);break;
            case  4: sprintf(Str,"%.4e",Val);break;
            case  3: sprintf(Str,"%.3e",Val);break;
            case  2: sprintf(Str,"%.2e",Val);break;
            default: sprintf(Str,"%.1e",Val);break;
         }
         break;
      default: sprintf(Str,"%.0f",nearbyint(Val));
         break;
   }
}

/*----------------------------------------------------------------------------
 * Nom      : <Icon_Free>
 * Creation : Fevrier 2003- J.P. Gauthier - CMC/CMOE
 *
 * But      : Supprimer une structure TDataSpec.
 *
 * Parametres :
 *  <Icon>    : Structure Icon a liberer
 *
 * Retour:
 *
 * Remarques :
 *
 *----------------------------------------------------------------------------
*/

void Icon_Free(TIcon *Icon) {

   if (Icon) {
      free(Icon);
   }
}

/*----------------------------------------------------------------------------
 * Nom      : <Icon_Parse>
 * Creation : Fevrier 2003- J.P. Gauthier - CMC/CMOE
 *
 * But      : Recuperer les coordonnes a pratir d'une liste caracteres.
 *
 * Parametres :
 *  <Interp>  : Interpreteur Tcl
 *  <Str>     : Liste
 *
 * Retour:
 *  <Icon>    : Structure Icon
 *
 * Remarques :
 *
 *----------------------------------------------------------------------------
*/

TIcon* Icon_Parse(Tcl_Interp *Interp,Tcl_Obj *List) {

   TIcon       *icon;
   int          i,nobj;
   Tcl_Obj     *obj;

   Tcl_ListObjLength(Interp,List,&nobj);

   if (!nobj || nobj>256) {
      Tcl_AppendResult(Interp,"Icon_Parse: invalid number of coordinates must be less than 128",(char*)NULL);
      return NULL;
   }

   icon=(TIcon*)malloc(sizeof(TIcon));
   icon->Nb=nobj;
   for(i=0;i<icon->Nb;i++) {
      Tcl_ListObjIndex(Interp,List,i,&obj);
      Tcl_GetDoubleFromObj(Interp,obj,&icon->Co[i]);
   }

   icon->Nb/=2;
   return(icon);
}
