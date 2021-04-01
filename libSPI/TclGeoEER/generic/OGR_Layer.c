/*=========================================================
 * Environnement Canada
 * Centre Meteorologique Canadien
 * 2100 Trans-Canadienne
 * Dorval, Quebec
 *
 * Projet       : Lecture et traitements de fichiers raster
 * Fichier      : OGR_Layer.c
 * Creation     : Juillet 2004 - J.P. Gauthier
 *
 * Description  : Fonctions de manipulations et d'affichage de fichiers vectoriel.
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
#include "tclOGR.h"
#include "tclGeoRef.h"
#include "Data_FF.h"
#include <sys/types.h>
#include </usr/include/regex.h>

static OGR_Layer *QSort_Layer;

int QSort_OGR(const void *A,const void *B);

/*----------------------------------------------------------------------------
 * Nom      : <OGR_LayerDefine>
 * Creation : Juillet 2004 J.P. Gauthier - CMC/CMOE
 *
 * But      : Definition des parametres de layer.
 *
 * Parametres :
 *  <Interp>  : Interpreteur Tcl
 *  <Name>    : Nom de la projection
 *  <Objc>    : Nombre d'arguments
 *  <Objv>    : Pointeur sur la liste des arguments
 *
 * Retour     :
 *  <TCL_...> : Code de retour standard TCL
 *
 * Remarques :
 *
 *----------------------------------------------------------------------------
*/
int OGR_LayerDefine(Tcl_Interp *Interp,char *Name,int Objc,Tcl_Obj *CONST Objv[]){

   Tcl_Obj       *obj,*lst,*sublst;
   Tcl_WideInt    w;
   TGeoRef       *ref;
   OGR_Layer     *layer;
   OGRFieldDefnH  field;
   OGRGeometryH   geom;
   int            i,idx,j,f,t;

   static CONST char *sopt[] = { "-type","-space","-field","-delfield","-name","-feature","-delfeature","-nb","-nbready","-geometry","-projection","-georef",
                                  "-mask","-featurehighlight","-featureselect","-fid",NULL };
   enum                opt { TYPE,SPACE,FIELD,DELFIELD,NAME,FEATURE,DELFEATURE,NB,NBREADY,GEOMETRY,PROJECTION,GEOREF,
                             MASK,FEATUREHIGHLIGHT,FEATURESELECT,FID };

   layer=OGR_LayerGet(Name);
   if (!layer) {
      Tcl_AppendResult(Interp,"\n   OGR_LayerDefine: Layer name unknown: \"",Name,"\"",(char *)NULL);
      return(TCL_ERROR);
   }

   for(i=0;i<Objc;i++) {

      if (Tcl_GetIndexFromObj(Interp,Objv[i],sopt,"option",TCL_EXACT,&idx)!=TCL_OK) {
         return TCL_ERROR;
      }

      switch ((enum opt)idx) {

         case FID:
            if (Objc==1) {
               if (layer->File) Tcl_SetObjResult(Interp,Tcl_NewStringObj(layer->File->Id,-1));
            }
            break;

         case GEOREF:
            if (Objc==1) {
               if (layer->GRef)
                  Tcl_SetObjResult(Interp,Tcl_NewStringObj(layer->GRef->Name,-1));
            } else {
               ref=GeoRef_Get(Tcl_GetString(Objv[++i]));
               if (!ref) {
                  Tcl_AppendResult(Interp,"\n   OGR_LayerDefine: Georef name unknown: \"",Tcl_GetString(Objv[i]),"\"",(char *)NULL);
                  return(TCL_ERROR);
               }
               if (ref!=layer->GRef) {
                  GeoRef_Destroy(Interp,layer->GRef->Name);
                  layer->GRef=ref;
                  GeoRef_Incr(layer->GRef);
                  OGR_LayerClean(layer,-1);
                  layer->Changed=1;
               }
            }
            break;

        case PROJECTION:
            if (Objc==1) {
               if (layer->GRef && layer->GRef->String)
                  Tcl_SetObjResult(Interp,Tcl_NewStringObj(layer->GRef->String,-1));
            } else {
               ++i;
               if (layer->GRef && layer->GRef->String && strlen(layer->GRef->String)==strlen(Tcl_GetString(Objv[i])) && strcmp(Tcl_GetString(Objv[i]),layer->GRef->String)==0) {
               } else {
                  GeoRef_Destroy(Interp,layer->GRef->Name);
                  layer->GRef=GeoRef_Find(GeoRef_WKTSetup(0,0,NULL,0,0,0,0,Tcl_GetString(Objv[i]),NULL,NULL,NULL));
                  OGR_LayerClean(layer,-1);
                  layer->Changed=1;
               }
            }
            break;

         case TYPE:
            if (Objc==1) {
               Tcl_SetObjResult(Interp,Tcl_NewStringObj(OGRGeometryTypeToName(OGR_L_GetGeomType(layer->Layer)),-1));
            } else {
            }
            break;

         case SPACE:
            if (Objc==1) {
               Tcl_SetObjResult(Interp,Tcl_NewIntObj(layer->Space));
            } else {
            }
            break;

         case FIELD:
            if (Objc<1 || Objc>5) {
               Tcl_WrongNumArgs(Interp,2,Objv,"?Field? ?Type? ?Width? ?Precision?");
               return TCL_ERROR;
            }
            if (Objc==1) {
               lst=Tcl_NewListObj(0,NULL);
               for(j=0;j<OGR_FD_GetFieldCount(layer->Def);j++) {
                  field=OGR_FD_GetFieldDefn(layer->Def,j);
                  Tcl_ListObjAppendElement(Interp,lst,Tcl_NewStringObj(OGR_Fld_GetNameRef(field),-1));
               }
               Tcl_SetObjResult(Interp,lst);
            } else if (Objc==2) {
               j=OGR_FD_GetFieldIndex(layer->Def,Tcl_GetString(Objv[1]));
               if (j==-1) {
                  Tcl_AppendResult(Interp,"\n   OGR_LayerDefine: Invalid field",(char*)NULL);
                  return(TCL_ERROR);
               }
               field=OGR_FD_GetFieldDefn(layer->Def,j);
               lst=Tcl_NewListObj(0,NULL);
               Tcl_ListObjAppendElement(Interp,lst,Tcl_NewStringObj(OGR_GetFieldTypeName(OGR_Fld_GetType(field)),-1));
               if (OGR_Fld_GetType(field)==OFTString) {
                  Tcl_ListObjAppendElement(Interp,lst,Tcl_NewIntObj(OGR_Fld_GetWidth(field)));
               }
               if (OGR_Fld_GetType(field)==OFTReal) {
                  Tcl_ListObjAppendElement(Interp,lst,Tcl_NewIntObj(OGR_Fld_GetWidth(field)));
                  Tcl_ListObjAppendElement(Interp,lst,Tcl_NewIntObj(OGR_Fld_GetPrecision(field)));
               }
               Tcl_SetObjResult(Interp,lst);
               i++;
            } else {
               int  prec,width;

               if (strlen(Tcl_GetString(Objv[1]))>10) {
                  Tcl_AppendResult(Interp,"\n   OGR_LayerDefine: field name too long (max 10 char)",(char*)NULL);
                  return(TCL_ERROR);
               }

               j=OGR_FD_GetFieldIndex(layer->Def,Tcl_GetString(Objv[1]));
               if (j!=-1) {
                  Tcl_AppendResult(Interp,"\n   OGR_LayerDefine: field already exists",(char*)NULL);
                  return(TCL_ERROR);
               }

               i++;
               prec=0;
               width=0;
               if (Objc>=4) {
                  Tcl_GetIntFromObj(Interp,Objv[3],&width);
                  i++;
               }
               if (Objc==5) {
                  Tcl_GetIntFromObj(Interp,Objv[4],&prec);
                  i++;
               }
               
               // Update the current structure if we need to
               OGR_LayerUpdate(layer);

               for(f=0;f<layer->NFeature;f++) {
                  if (layer->Feature[f]) OGR_F_Destroy(layer->Feature[f]);
               }

               if (!OGR_FieldCreate(layer,Tcl_GetString(Objv[1]),Tcl_GetString(Objv[2]),width,prec)) {
                  Tcl_AppendResult(Interp,"\n   OGR_LayerDefine: Unable to create field ",Tcl_GetString(Objv[1]),(char*)NULL);
                  return(TCL_ERROR);
               }
               i++;
               
               // Reload the features to be in sync
               if (OGR_LayerReadFeature(Interp,layer)==TCL_ERROR) {
                  return(TCL_ERROR);
               }
               
               // Clear the new fiels
               j=OGR_FD_GetFieldIndex(layer->Def,Tcl_GetString(Objv[1]));
               for(f=0;f<layer->NFeature;f++) {
                  if (layer->Feature[f]) OGR_F_UnsetField(layer->Feature[f],j);
               }
            }
            break;

         case DELFIELD:
            if (Objc!=2) {
               Tcl_WrongNumArgs(Interp,2,Objv,"Field");
               return(TCL_ERROR);
            }
            j=OGR_FD_GetFieldIndex(layer->Def,Tcl_GetString(Objv[++i]));
            if (j==-1) {
               Tcl_AppendResult(Interp,"\n   OGR_LayerDefine: Invalid field",(char*)NULL);
                return(TCL_ERROR);
            }
            
            /*Update the current structure if we need to*/
            OGR_LayerUpdate(layer);
            
            for(f=0;f<layer->NFeature;f++) {
               if (layer->Feature[f]) OGR_F_Destroy(layer->Feature[f]);
            }
            OGR_L_DeleteField(layer->Layer,j);
      
            /*Reload the features to be in sync*/
            if (OGR_LayerReadFeature(Interp,layer)==TCL_ERROR) {
               return(TCL_ERROR);
            }
            layer->Changed=1;
            
            break;
            
         case FEATURE:
            if (Objc<2) {
               Tcl_WrongNumArgs(Interp,2,Objv,"index ?Field? ?Value?");
               return(TCL_ERROR);
            }
            Tcl_GetIntFromObj(Interp,Objv[++i],&f);
            if (f<0 || f>=layer->NFeature || !layer->Feature[f]) {
               Tcl_AppendResult(Interp,"\n   OGR_LayerDefine: Invalid feature index",(char*)NULL);
               return(TCL_ERROR);
            }
            if (Objc==2) {
               lst=Tcl_NewListObj(0,NULL);

               for(j=0;j<OGR_FD_GetFieldCount(layer->Def);j++) {
                  field=OGR_FD_GetFieldDefn(layer->Def,j);
                  sublst=Tcl_NewListObj(0,NULL);
                  Tcl_ListObjAppendElement(Interp,sublst,Tcl_NewStringObj(OGR_Fld_GetNameRef(field),-1));
                  Tcl_ListObjAppendElement(Interp,sublst,OGR_GetTypeObj(Interp,field,layer->Feature[f],j));
                  Tcl_ListObjAppendElement(Interp,lst,sublst);
               }
               Tcl_SetObjResult(Interp,lst);
            } else if (Objc==3) {
               j=OGR_F_GetFieldIndex(layer->Feature[f],Tcl_GetString(Objv[++i]));
               if (j<0) {
                  Tcl_AppendResult(Interp,"\n   OGR_LayerDefine: Invalid Field",(char*)NULL);
                  return(TCL_ERROR);
               } else {
                  field=OGR_FD_GetFieldDefn(layer->Def,j);
                  Tcl_SetObjResult(Interp,OGR_GetTypeObj(Interp,field,layer->Feature[f],j));
               }
            } else {
               j=OGR_F_GetFieldIndex(layer->Feature[f],Tcl_GetString(Objv[++i]));
               if (j<0) {
                  Tcl_AppendResult(Interp,"\n   OGR_LayerDefine: Invalid Field",(char*)NULL);
                  return(TCL_ERROR);
               } else {
                  field=OGR_FD_GetFieldDefn(layer->Def,j);
                  layer->Update=1;
                  layer->Changed=1;
                  return(OGR_SetTypeObj(Interp,Objv[++i],layer->Layer,field,layer->Feature[f],j));
               }
            }
            break;

         case DELFEATURE:
            if (Objc<2) {
               Tcl_WrongNumArgs(Interp,2,Objv,"index");
               return(TCL_ERROR);
            }
            Tcl_GetIntFromObj(Interp,Objv[++i],&f);
            if (f<0 || f>=layer->NFeature || !layer->Feature[f]) {
               Tcl_AppendResult(Interp,"\n   OGR_LayerDefine: Invalid feature index",(char*)NULL);
               return(TCL_ERROR);
            }
            
            if (OGR_L_DeleteFeature(layer->Layer,f)==OGRERR_UNSUPPORTED_OPERATION) {
               Tcl_AppendResult(Interp,"\n   OGR_LayerDefine: Unable to delete feature, operation not supported",(char*)NULL);
               return(TCL_ERROR);
            }
            
            OGR_LayerClean(layer,f);
            OGR_F_Destroy(layer->Feature[f]);
            layer->Feature[f]=NULL;
            layer->Changed=1;
            
            break;
            
         case FEATUREHIGHLIGHT:
            if (Objc==1) {
               lst=Tcl_NewListObj(0,NULL);
               for(f=0;f<layer->NSFeature;f++) {
                  Tcl_ListObjAppendElement(Interp,lst,Tcl_NewIntObj(layer->SFeature[f]));
               }
               Tcl_SetObjResult(Interp,lst);
            } else {

               Tcl_ListObjLength(Interp,Objv[++i],&f);
               layer->NSFeature=f;
               
               if (layer->SFeature) {
                  free(layer->SFeature);
                  layer->SFeature=NULL;
               }
               if (layer->NSFeature) {
                  if ((layer->SFeature=(unsigned int*)malloc(layer->NSFeature*sizeof(unsigned int)))) {
                     t=0;
                     for(f=0;f<layer->NSFeature;f++) {
                        Tcl_ListObjIndex(Interp,Objv[i],f,&obj);
                        if (Tcl_GetWideIntFromObj(Interp,obj,&w)!=TCL_ERROR) {
                           layer->SFeature[t++]=w;
                        }
                     }
                     layer->NSFeature=t;
                  } else {
                     Tcl_AppendResult(Interp,"OGR_LayerDefine: Unable to allocate feature select buffer",(char*)NULL);
                     return(TCL_ERROR);                   
                  }
               }
            }
            break;

         case FEATURESELECT:
            t=TCL_OK;
            if (Objc>1) {
               t=OGR_LayerSelect(Interp,layer,Objv[++i]);
               
               /*If there is a sort applied, refresh it*/
               OGR_LayerSort(Interp,layer);
            }
            if (t!=TCL_ERROR) {
               lst=Tcl_NewListObj(0,NULL);
               for(f=0;f<layer->NFeature;f++) {
                  if (layer->Select[f]) {
                     Tcl_ListObjAppendElement(Interp,lst,Tcl_NewIntObj(f));
                  }
               }
               Tcl_SetObjResult(Interp,lst);
            }
            return(t);
            break;
            
         case GEOMETRY:
            if (Objc<2 || Objc>4) {
               Tcl_WrongNumArgs(Interp,2,Objv,"index ?direct? ?geometry?");
               return(TCL_ERROR);
            }
            Tcl_GetIntFromObj(Interp,Objv[++i],&f);
            if (f<0 || f>=layer->NFeature || !layer->Feature[f]) {
               Tcl_AppendResult(Interp,"\n   OGR_LayerDefine: Invalid feature index",(char*)NULL);
               return(TCL_ERROR);
            }

            /* Get the direct pointer flag*/
            t=0;
            if (Objc>2) {
               Tcl_GetBooleanFromObj(Interp,Objv[++i],&t);
            }

            if (Objc<4) {
               /* Force assignation of spatial reference since it seems it is no done automatically*/
               if ((geom=OGR_F_GetGeometryRef(layer->Feature[f]))) {
                  if (!t) {
                     if ((geom=OGR_G_Clone(geom))) {
                        OGR_G_AssignSpatialReference(geom,layer->GRef->Spatial);
                     }
                  }
                  Tcl_SetObjResult(Interp,OGR_GeometryPut(Interp,NULL,geom));
               }
            } else {
               if (!(geom=OGR_GeometryGet(Tcl_GetString(Objv[++i])))) {
                  Tcl_AppendResult(Interp,"\n   OGR_LayerDefine: Invalid geometry",(char*)NULL);
                  return(TCL_ERROR);
               }

               if (t) {
                  t=OGR_F_SetGeometryDirectly(layer->Feature[f],geom);
               } else {
                  t=OGR_F_SetGeometry(layer->Feature[f],geom);
               }
               if (t!=OGRERR_NONE) {
                  Tcl_AppendResult(Interp,"\n   OGR_LayerDefine: Unsupported geometry",(char*)NULL);
                  return(TCL_ERROR);
               }
               if (OGR_L_CreateFeature(layer->Layer,layer->Feature[f])!=OGRERR_NONE) {
                  Tcl_AppendResult(Interp,"\n   OGR_LayerDefine: Problems creating feature",(char*)NULL);
                  return(TCL_ERROR);  
               }
               layer->Changed=1;
            }
            break;

         case NB:
            if (Objc==1) {
               Tcl_SetObjResult(Interp,Tcl_NewIntObj(layer->NFeature));
            } else {
               Tcl_GetIntFromObj(Interp,Objv[++i],&f);
               if (f>=layer->NFeature) {
                  layer->Feature=realloc(layer->Feature,f*sizeof(OGRFeatureH));
                  layer->Loc=realloc(layer->Loc,f*sizeof(Coord));
                  layer->Select=realloc(layer->Select,f*sizeof(char));
                  if (layer->Feature && layer->Select && layer->Loc) {
                     memset(layer->Select,0x1,f);
                     for(idx=layer->NFeature;idx<f;idx++) {
                        layer->Feature[idx]=OGR_F_Create(layer->Def);
                     }
                     layer->NFeature=f;
                     layer->Changed=1;
                  } else {
                     Tcl_AppendResult(Interp,"OGR_LayerDefine: Unable to allocate feature buffer",(char*)NULL);
                     return(TCL_ERROR);                   
                  }
               }
               Tcl_SetObjResult(Interp,Tcl_NewIntObj(layer->NFeature));
            }
            break;

         case NBREADY:
            if (Objc==1) {
               Tcl_SetObjResult(Interp,Tcl_NewIntObj(layer->GFeature));
            }
            break;

         case NAME:
            if (Objc==1) {
               Tcl_SetObjResult(Interp,Tcl_NewStringObj(OGR_FD_GetName(layer->Def),-1));
            } else {
            }
            break;

         case MASK:
            if (Objc==1) {
               Tcl_SetObjResult(Interp,Tcl_NewBooleanObj(layer->Mask));
            } else {
               Tcl_GetBooleanFromObj(Interp,Objv[++i],&layer->Mask);
            }
            break;
      }
   }
   return TCL_OK;
}

/*----------------------------------------------------------------------------
 * Nom      : <OGR_LayerStat>
 * Creation : Avril 2004 - J.P. Gauthier - CMC/CMOE
 *
 * But      : Effectue la configuration des statistiques et le retour des valeurs
 *            si il n'y a pas de valeur specifie (seulement le token).
 *
 * Parametres     :
 *  <Interp>      : Interpreteur TCL
 *  <Field>       : Pointeur sur le champs
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
static OGR_Layer* OGR_LayerResult(Tcl_Interp *Interp,OGR_Layer *From,char *Name,int NFeature) {

   OGR_Layer *layerres;
   if (!(layerres=OGR_LayerCreate(Interp,Name,(char*)OGR_FD_GetName(From->Def),wkbUnknown,NULL))) {
      Tcl_AppendResult(Interp,"OGR_LayerResult: Unable to create operation layer",(char*)NULL);
      return(NULL);
   }
 
   layerres->GRef=GeoRef_Copy(From->GRef);
   layerres->Def=From->Def;         
   layerres->NFeature=0;
   layerres->Changed=1;
   layerres->Feature=malloc(NFeature*sizeof(OGRFeatureH));
   layerres->Select=malloc(NFeature*sizeof(char));

   OGR_FD_Reference(layerres->Def);

   if (!layerres->Feature || !layerres->Select) {
      Tcl_AppendResult(Interp,"OGR_LayerResult: Unable to allocate feature buffer",(char*)NULL);
      return(NULL);
   }
   memset(layerres->Select,0x1,NFeature);
   
   if (!(layerres->Loc=(Coord*)malloc(NFeature*sizeof(Coord)))) {
      Tcl_AppendResult(Interp,"OGR_LayerResult: Unable to allocate location buffer",(char*)NULL);
      return(NULL);
   }
      
   return(layerres);
}

int OGR_LayerStat(Tcl_Interp *Interp,char *Name,int Objc,Tcl_Obj *CONST Objv[]){

   OGRCoordinateTransformationH  tr=NULL;
   OGREnvelope                   env,lim;
   OGRGeometryH                  geom,new,uni;

   const char  **iopts;
   int           j,idx,n,nseg,nopts,fld=-1;
   double        x,y,lat,lon,tol,val,min,max,area;
   unsigned int  y0,y1,fop;
   OGR_Layer    *layer,*layerop,*layerres=NULL;
   TGeoRef      *ref,*ref0;
   Tcl_Obj      *lst,*obj;
   Tcl_WideInt   f;
   char          buf[32],*str;

   static CONST char *sopt[] = { "-sort","-table","-tag","-centroid","-invalid","-transform","-project","-unproject","-min","-max","-extent","-llextent","-lupdate","-lintersection","-lunion","-lsymdifference","-lidentity","-lclip","-lerase","-buffer","-difference","-intersection",
                                 "-clip","-simplify","-segmentize","-close","-flatten","-dissolve","-boundary","-convexhull",NULL };
   enum        opt {  SORT,TABLE,TAG,CENTROID,INVALID,TRANSFORM,PROJECT,UNPROJECT,MIN,MAX,EXTENT,LLEXTENT,LUPDATE,LINTERSECTION,LUNION,LSYMDIFFERENCE,LIDENTITY,LCLIP,LERASE,BUFFER,DIFFERENCE,INTERSECTION,CLIP,SIMPLIFY,SEGMENTIZE,CLOSE,FLATTEN,DISSOLVE,BOUNDARY,CONVEXHULL };

   layer=OGR_LayerGet(Name);
   if (!layer) {
      Tcl_AppendResult(Interp,"\n   OGR_LayerStat: Layer name unknown: \"",Name,"\"",(char *)NULL);
      return(TCL_ERROR);
   }

   if (Tcl_GetIndexFromObj(Interp,Objv[0],sopt,"option",TCL_EXACT,&idx)!=TCL_OK) {
      return(TCL_ERROR);
   }

   switch ((enum opt)idx) {

      case TAG:
         if (Objc==1) {
            if (layer->Tag) {
               Tcl_SetObjResult(Interp,layer->Tag);
            }
         } else {
            if (layer->Tag) {
               Tcl_DecrRefCount(layer->Tag);
            }
            layer->Tag=Objv[1];
            Tcl_IncrRefCount(layer->Tag);
         }
         break;

      case INVALID:
         if (Objc!=1) {
            Tcl_WrongNumArgs(Interp,2,Objv,"");
            return(TCL_ERROR);
         }

         lst=Tcl_NewListObj(0,NULL);

         for(f=0;f<layer->NFeature;f++) {
            if (layer->Select[f] && layer->Feature[f] && (geom=OGR_F_GetGeometryRef(layer->Feature[f]))) {
               if (!OGR_G_IsValid(geom)) {
                  Tcl_ListObjAppendElement(Interp,lst,Tcl_NewIntObj(f));
               }
            }
         }
         Tcl_SetObjResult(Interp,lst);
         break;

      case TRANSFORM:
         if (Objc!=2 && Objc!=3) {
            Tcl_WrongNumArgs(Interp,0,Objv,"[georeffrom] georefto");
            return(TCL_ERROR);
         }
         if (Objc==3) {
            ref=GeoRef_Get(Tcl_GetString(Objv[2]));
            if (!ref || !ref->Spatial) {
               Tcl_AppendResult(Interp,"\n   OGR_LayerStat: Georef name unknown or invalid spatial reference: \"",Tcl_GetString(Objv[2]),"\"",(char *)NULL);
               return TCL_ERROR;
            }
            ref0=GeoRef_Get(Tcl_GetString(Objv[1]));
            if (!ref0 || !ref0->Spatial) {
               Tcl_AppendResult(Interp,"\n   OGR_LayerStat: Georef name unknown or invalid spatial reference: \"",Tcl_GetString(Objv[1]),"\"",(char *)NULL);
               return(TCL_ERROR);
            }
            tr=OCTNewCoordinateTransformation(ref0->Spatial,ref->Spatial);
         } else {
            ref=GeoRef_Get(Tcl_GetString(Objv[1]));
            if (!ref|| !ref->Spatial) {
               Tcl_AppendResult(Interp,"\n   OGR_LayerStat: Georef name unknown or invalid spatial reference: \"",Tcl_GetString(Objv[1]),"\"",(char *)NULL);
               return(TCL_ERROR);
            }
            tr=OCTNewCoordinateTransformation(layer->GRef->Spatial,ref->Spatial);
         }

         if (!tr) {
            Tcl_AppendResult(Interp,"\n   OGR_LayerStat: Could not initiate transformation function",(char *)NULL);
            return(TCL_ERROR);
         } else {
            for(f=0;f<layer->NFeature;f++) {
               if (layer->Feature[f] && (geom=OGR_F_GetGeometryRef(layer->Feature[f]))) {
                  OGR_G_Transform(geom,tr);
               }
            }
            layer->GRef=ref;
            layer->GRef->NRef++;
            layer->Changed=1;
            OCTDestroyCoordinateTransformation(tr);
         }
         break;

      case PROJECT:
         lst=Tcl_NewListObj(0,NULL);
         Tcl_GetDoubleFromObj(Interp,Objv[1],&x);
         Tcl_GetDoubleFromObj(Interp,Objv[2],&y);
         layer->GRef->Project(layer->GRef,x,y,&lat,&lon,1,1);
         Tcl_ListObjAppendElement(Interp,lst,Tcl_NewDoubleObj(lat));
         Tcl_ListObjAppendElement(Interp,lst,Tcl_NewDoubleObj(lon));
         Tcl_SetObjResult(Interp,lst);
         break;

      case UNPROJECT:
         lst=Tcl_NewListObj(0,NULL);
         Tcl_GetDoubleFromObj(Interp,Objv[1],&lat);
         Tcl_GetDoubleFromObj(Interp,Objv[2],&lon);
         layer->GRef->UnProject(layer->GRef,&x,&y,lat,lon,1,1);
         Tcl_ListObjAppendElement(Interp,lst,Tcl_NewDoubleObj(x));
         Tcl_ListObjAppendElement(Interp,lst,Tcl_NewDoubleObj(y));
         Tcl_SetObjResult(Interp,lst);
         break;

      case CENTROID:
         if (Objc!=1 && Objc!=2) {
            Tcl_WrongNumArgs(Interp,2,Objv,"[index]");
            return(TCL_ERROR);
         }

         n=0;
         x=y=area=0.0;
         lst=Tcl_NewListObj(0,NULL);
         new=OGR_G_CreateGeometry(wkbLinearRing);

         // A list a feature has been passed
         if (Objc>1) {           
            Tcl_ListObjLength(Interp,Objv[1],&n);
         }
        
         // A list a feature has been passed
         if (n) {   
            
            for(j=0;j<n;j++) {
               Tcl_ListObjIndex(Interp,Objv[1],j,&obj);
               Tcl_GetWideIntFromObj(Interp,Objv[1],&f);
               if (f>=0 && f<layer->NFeature && layer->Feature[f] && (geom=OGR_F_GetGeometryRef(layer->Feature[f]))) {
                  area=OGM_Centroid2D(geom,&x,&y);
                  if (n>1) OGR_G_AddPoint_2D(new,x,y);
               }
            }
            if (n>1) f=OGM_Centroid2D(new,&x,&y);
            
         } else {
            for(f=0;f<layer->NFeature;f++) {
               if (layer->Select[f] && layer->Feature[f] && (geom=OGR_F_GetGeometryRef(layer->Feature[f]))) {
                  area+=OGM_Centroid2D(geom,&x,&y);
                  OGR_G_AddPoint_2D(new,x,y);
               }
            }
            f=OGM_Centroid2D(new,&x,&y);
         }
         OGR_G_DestroyGeometry(new);
         
         // Check wrap-around
//         if (!(srs=OGR_L_GetSpatialRef(layer->Layer)) || OSRIsGeographic(srs)) {
//            OGR_L_GetExtent(layer->Layer,&env,1);
//            if (-180.0<=env.MinX && 180.0>=env.MaxX && (env.MaxX-env.MinX)>180.0) {
//               x-=180.0;
//            }
//         }
         
         Tcl_ListObjAppendElement(Interp,lst,Tcl_NewDoubleObj(x));
         Tcl_ListObjAppendElement(Interp,lst,Tcl_NewDoubleObj(y));
         Tcl_ListObjAppendElement(Interp,lst,Tcl_NewDoubleObj(area<0?-area:area));
      
         Tcl_SetObjResult(Interp,lst);
         break;

      case EXTENT:
         if (Objc!=1 && Objc!=2) {
            Tcl_WrongNumArgs(Interp,2,Objv,"[index]");
            return(TCL_ERROR);
         }
         
         lst=Tcl_NewListObj(0,NULL);
         env.MinX=env.MinY=1e32;
         env.MaxX=env.MaxY=-1e32;
         n=0;
         
         if (Objc>1) {           
            Tcl_ListObjLength(Interp,Objv[1],&n);
         }
         
         // A list a feature has been passed
         if (n) {            
            for(j=0;j<n;j++) {
               Tcl_ListObjIndex(Interp,Objv[1],j,&obj);
               Tcl_GetWideIntFromObj(Interp,obj,&f);
               if (f>=0 && f<layer->NFeature && layer->Feature[f] && (geom=OGR_F_GetGeometryRef(layer->Feature[f]))) {
                  OGR_G_GetEnvelope(geom,&lim);
                  env.MinX=lim.MinX<env.MinX?lim.MinX:env.MinX;
                  env.MinY=lim.MinY<env.MinY?lim.MinY:env.MinY;
                  env.MaxX=lim.MaxX>env.MaxX?lim.MaxX:env.MaxX;
                  env.MaxY=lim.MaxY>env.MaxY?lim.MaxY:env.MaxY;
               }
            }
         } else {
            // Check if a selection is made
            for(f=0;f<layer->NFeature;f++) {
               if (!layer->Select[f]) {
                  break;
               }
            }
                  
            // if so      
            if (f<layer->NFeature) {
               for(f=0;f<layer->NFeature;f++) {
                  if (layer->Select[f] && layer->Feature[f] && (geom=OGR_F_GetGeometryRef(layer->Feature[f]))) {
                     OGR_G_GetEnvelope(geom,&lim);
                     env.MinX=lim.MinX<env.MinX?lim.MinX:env.MinX;
                     env.MinY=lim.MinY<env.MinY?lim.MinY:env.MinY;
                     env.MaxX=lim.MaxX>env.MaxX?lim.MaxX:env.MaxX;
                     env.MaxY=lim.MaxY>env.MaxY?lim.MaxY:env.MaxY;
                  }
               }
            } else {
               /*Otherwise, use the one defined in the layer*/
               OGR_L_GetExtent(layer->Layer,&env,1);
            }
         }
         Tcl_ListObjAppendElement(Interp,lst,Tcl_NewDoubleObj(env.MinX));
         Tcl_ListObjAppendElement(Interp,lst,Tcl_NewDoubleObj(env.MinY));
         Tcl_ListObjAppendElement(Interp,lst,Tcl_NewDoubleObj(env.MaxX));
         Tcl_ListObjAppendElement(Interp,lst,Tcl_NewDoubleObj(env.MaxY));
         Tcl_SetObjResult(Interp,lst);
         break;

      case LLEXTENT:
         if (Objc!=1 && Objc!=2) {
            Tcl_WrongNumArgs(Interp,2,Objv,"[index]");
            return(TCL_ERROR);
         }

         lst=Tcl_NewListObj(0,NULL);
         env.MinX=env.MinY=1e32;
         env.MaxX=env.MaxY=-1e32;
         n=0;
         
         // A list a feature has been passed
         if (Objc>1) {           
            Tcl_ListObjLength(Interp,Objv[1],&n);
         }
         
         if (n) {        
            for(j=0;j<n;j++) {
               Tcl_ListObjIndex(Interp,Objv[1],j,&obj);
               Tcl_GetWideIntFromObj(Interp,obj,&f);
               if (f>=0 && f<layer->NFeature && layer->Feature[f] && (geom=OGR_F_GetGeometryRef(layer->Feature[f]))) {
                  OGR_G_GetEnvelope(geom,&lim);
                  env.MinX=lim.MinX<env.MinX?lim.MinX:env.MinX;
                  env.MinY=lim.MinY<env.MinY?lim.MinY:env.MinY;
                  env.MaxX=lim.MaxX>env.MaxX?lim.MaxX:env.MaxX;
                  env.MaxY=lim.MaxY>env.MaxY?lim.MaxY:env.MaxY;
               }
            }
            layer->GRef->Project(layer->GRef,env.MinX,env.MinY,&lat,&lon,1,1);
            Tcl_ListObjAppendElement(Interp,lst,Tcl_NewDoubleObj(lat));
            Tcl_ListObjAppendElement(Interp,lst,Tcl_NewDoubleObj(lon));
            layer->GRef->Project(layer->GRef,env.MaxX,env.MaxY,&lat,&lon,1,1);
            Tcl_ListObjAppendElement(Interp,lst,Tcl_NewDoubleObj(lat));
            Tcl_ListObjAppendElement(Interp,lst,Tcl_NewDoubleObj(lon));           
         } else {
            // Check if a selection is made
            for(f=0;f<layer->NFeature;f++) {
               if (!layer->Select[f]) {
                  break;
               }
            }

            // if so      
            if (f<layer->NFeature) {
               for(f=0;f<layer->NFeature;f++) {
                  if (layer->Select[f] && layer->Feature[f] && (geom=OGR_F_GetGeometryRef(layer->Feature[f]))) {
                     OGR_G_GetEnvelope(geom,&lim);
                     env.MinX=lim.MinX<env.MinX?lim.MinX:env.MinX;
                     env.MinY=lim.MinY<env.MinY?lim.MinY:env.MinY;
                     env.MaxX=lim.MaxX>env.MaxX?lim.MaxX:env.MaxX;
                     env.MaxY=lim.MaxY>env.MaxY?lim.MaxY:env.MaxY;
                  }
               }
               layer->GRef->Project(layer->GRef,env.MinX,env.MinY,&lat,&lon,1,1);
               Tcl_ListObjAppendElement(Interp,lst,Tcl_NewDoubleObj(lat));
               Tcl_ListObjAppendElement(Interp,lst,Tcl_NewDoubleObj(lon));
               layer->GRef->Project(layer->GRef,env.MaxX,env.MaxY,&lat,&lon,1,1);
               Tcl_ListObjAppendElement(Interp,lst,Tcl_NewDoubleObj(lat));
               Tcl_ListObjAppendElement(Interp,lst,Tcl_NewDoubleObj(lon));           
            } else {
               Tcl_ListObjAppendElement(Interp,lst,Tcl_NewDoubleObj(layer->GRef->LLExtent.MinY));
               Tcl_ListObjAppendElement(Interp,lst,Tcl_NewDoubleObj(layer->GRef->LLExtent.MinX));
               Tcl_ListObjAppendElement(Interp,lst,Tcl_NewDoubleObj(layer->GRef->LLExtent.MaxY));
               Tcl_ListObjAppendElement(Interp,lst,Tcl_NewDoubleObj(layer->GRef->LLExtent.MaxX));
            }
         }
         Tcl_SetObjResult(Interp,lst);
         break;

      case MIN:
         if (Objc!=2) {
            Tcl_WrongNumArgs(Interp,0,Objv,"layer field");
            return(TCL_ERROR);
         }
         j=OGR_FD_GetFieldIndex(layer->Def,Tcl_GetString(Objv[1]));
         if (j==-1) {
            Tcl_AppendResult(Interp,"\n   OGR_LayerStats: Invalid field",(char*)NULL);
            return(TCL_ERROR);
         }
         min=1e32;
         for(f=0;f<layer->NFeature;f++) {
            if (layer->Select[f] && layer->Feature[f]) {
               val=OGR_F_GetFieldAsDouble(layer->Feature[f],j);
               min=min<val?min:val;
            }
         }
         Tcl_SetObjResult(Interp,Tcl_NewDoubleObj(min));
         break;

      case MAX:
         if (Objc!=2) {
            Tcl_WrongNumArgs(Interp,0,Objv,"layer field");
            return(TCL_ERROR);
         }
         j=OGR_FD_GetFieldIndex(layer->Def,Tcl_GetString(Objv[1]));
         if (j==-1) {
            Tcl_AppendResult(Interp,"\n   OGR_LayerStats: Invalid field",(char*)NULL);
            return(TCL_ERROR);
         }
         max=-1e32;
         for(f=0;f<layer->NFeature;f++) {
            if (layer->Select[f] && layer->Feature[f]) {
               val=OGR_F_GetFieldAsDouble(layer->Feature[f],j);
               max=max>val?max:val;
            }
         }
         Tcl_SetObjResult(Interp,Tcl_NewDoubleObj(max));
         break;

      case LINTERSECTION:
      case LUNION:
      case LSYMDIFFERENCE:
      case LIDENTITY:
      case LUPDATE:
      case LCLIP:
      case LERASE:
         if (Objc!=3 && Objc!=4) {
            Tcl_WrongNumArgs(Interp,0,Objv,"layerop layerres [opts]");
            return(TCL_ERROR);
         }
         
         layerop=OGR_LayerGet(Tcl_GetString(Objv[1]));
         if (!layerop) {
            Tcl_AppendResult(Interp,"\n   OGR_LayerStat: Operator layer name unknown: \"",Tcl_GetString(Objv[1]),"\"",(char *)NULL);
            return(TCL_ERROR);
         }

         if (!(layerres=OGR_LayerResult(Interp,layer,Tcl_GetString(Objv[2]),layer->NFeature))) {
            return(TCL_ERROR);
         }

         if (Objc==4) {
            if (Tcl_SplitList(Interp,Tcl_GetString(Objv[3]),&nopts,&iopts)==TCL_ERROR) {
               Tcl_AppendResult(Interp,"\n   OGR_LayerStat : Invalid list of options",(char*)NULL);
               return(TCL_ERROR);
            }
         } else {
            iopts=NULL;
         }
         
         switch ((enum opt)idx) {       
            case LINTERSECTION:
               if ((OGR_L_Intersection(layer->Layer,layerop->Layer,layerres->Layer,(char**)iopts,NULL,NULL)!=OGRERR_NONE)) {
                  Tcl_AppendResult(Interp,"\n   OGR_LayerStat: Problem in OGR call OGR_L_Erase ",(char *)NULL);
                  return(TCL_ERROR);
               }
               if (iopts) Tcl_Free((char*)iopts);
               break;
               
            case LUNION:
               if ((OGR_L_Union(layer->Layer,layerop->Layer,layerres->Layer,(char**)iopts,NULL,NULL)!=OGRERR_NONE)) {
                  Tcl_AppendResult(Interp,"\n   OGR_LayerStat: Problem in OGR call OGR_L_Erase ",(char *)NULL);
                  return(TCL_ERROR);
               }
               if (iopts) Tcl_Free((char*)iopts);
               break;
               
            case LSYMDIFFERENCE:
               if ((OGR_L_SymDifference(layer->Layer,layerop->Layer,layerres->Layer,(char**)iopts,NULL,NULL)!=OGRERR_NONE)) {
                  Tcl_AppendResult(Interp,"\n   OGR_LayerStat: Problem in OGR call OGR_L_Erase ",(char *)NULL);
                  return(TCL_ERROR);
               }
               if (iopts) Tcl_Free((char*)iopts);
               break;
               
            case LIDENTITY:
               if ((OGR_L_Identity(layer->Layer,layerop->Layer,layerres->Layer,(char**)iopts,NULL,NULL)!=OGRERR_NONE)) {
                  Tcl_AppendResult(Interp,"\n   OGR_LayerStat: Problem in OGR call OGR_L_Erase ",(char *)NULL);
                  return(TCL_ERROR);
               }
               if (iopts) Tcl_Free((char*)iopts);
               break;
               
            case LUPDATE:
               if ((OGR_L_Update(layer->Layer,layerop->Layer,layerres->Layer,(char**)iopts,NULL,NULL)!=OGRERR_NONE)) {
                  Tcl_AppendResult(Interp,"\n   OGR_LayerStat: Problem in OGR call OGR_L_Erase ",(char *)NULL);
                  return(TCL_ERROR);
               }
               if (iopts) Tcl_Free((char*)iopts);
               break;
               
            case LCLIP:
               if ((OGR_L_Clip(layer->Layer,layerop->Layer,layerres->Layer,(char**)iopts,NULL,NULL)!=OGRERR_NONE)) {
                  Tcl_AppendResult(Interp,"\n   OGR_LayerStat: Problem in OGR call OGR_L_Erase ",(char *)NULL);
                  return(TCL_ERROR);
               }
               if (iopts) Tcl_Free((char*)iopts);
               break;
               
            case LERASE:        
               if ((OGR_L_Erase(layer->Layer,layerop->Layer,layerres->Layer,(char**)iopts,NULL,NULL)!=OGRERR_NONE)) {
                  Tcl_AppendResult(Interp,"\n   OGR_LayerStat: Problem in OGR call OGR_L_Erase ",(char *)NULL);
                  return(TCL_ERROR);
               }
               if (iopts) Tcl_Free((char*)iopts);
               break;
            default : break;  
          }
          break;
          
      case DISSOLVE:
      case BOUNDARY:
      case CONVEXHULL:

         if (Objc!=2) {
            Tcl_WrongNumArgs(Interp,0,Objv,"result");
            return(TCL_ERROR);
         }
         if (!(layerres=OGR_LayerResult(Interp,layer,Tcl_GetString(Objv[1]),1))) {
            return(TCL_ERROR);
         }
         
         new=OGM_GPCOnOGRLayer(GPC_UNION,layer);

         layerres->Feature[0]=OGR_F_Clone(layer->Feature[0]);                        
         layerres->NFeature++;
         
         if ((enum opt)idx==DISSOLVE)
            OGR_F_SetGeometryDirectly(layerres->Feature[0],new);
         if ((enum opt)idx==BOUNDARY)
            OGR_F_SetGeometryDirectly(layerres->Feature[0],OGR_G_Boundary(new));
         if ((enum opt)idx==CONVEXHULL)
            OGR_F_SetGeometryDirectly(layerres->Feature[0],OGR_G_ConvexHull(new));
         
         break;

      case BUFFER:
         if (Objc!=3 && Objc!=4) {
            Tcl_WrongNumArgs(Interp,0,Objv,"dist|field nseg [result]");
            return(TCL_ERROR);
         }
         fld=OGR_FD_GetFieldIndex(layer->Def,Tcl_GetString(Objv[1]));
         if (fld==-1) {
            Tcl_GetDoubleFromObj(Interp,Objv[1],&x);
         }
         Tcl_GetIntFromObj(Interp,Objv[2],&nseg);
         
         if (Objc==4) {
            if (!(layerres=OGR_LayerResult(Interp,layer,Tcl_GetString(Objv[3]),layer->NFeature))) {
               return(TCL_ERROR);
            }
         }
         for(f=0;f<layer->NFeature;f++) {
            if (layer->Select[f] && layer->Feature[f]) {
              if (fld!=-1) x=OGR_F_GetFieldAsDouble(layer->Feature[f],fld);

               if ((geom=OGR_F_GetGeometryRef(layer->Feature[f]))) {
                  if ((new=OGR_G_Buffer(geom,x,nseg))) {
                     if (layerres) {
                        layerres->Feature[layerres->NFeature]=OGR_F_Clone(layer->Feature[f]); 
                        OGR_F_SetGeometryDirectly(layerres->Feature[layerres->NFeature++],new);
                     } else {
                        OGR_F_SetGeometryDirectly(layer->Feature[f],new);
                        layer->Changed=1; 
                     }
                  } else {
                     App_Log(ERROR,"%s: Bad Buffer on feature %li\n",__func__,f);
                  }
               }
            }
         }
         break;

      case DIFFERENCE:
         if (Objc!=2 && Objc!=3) {
            Tcl_WrongNumArgs(Interp,0,Objv,"layer [result]");
            return(TCL_ERROR);
         }
         layerop=OGR_LayerGet(Tcl_GetString(Objv[1]));
         if (!layerop) {
            Tcl_AppendResult(Interp,"\n   OGR_LayerStat: Layer name unknown: \"",Tcl_GetString(Objv[1]),"\"",(char *)NULL);
            return(TCL_ERROR);
         }

         if (Objc==3) {
            if (!(layerres=OGR_LayerResult(Interp,layer,Tcl_GetString(Objv[2]),layer->NFeature))) {
               return(TCL_ERROR);
            }
         }

         uni=OGM_GPCOnOGRLayer(GPC_UNION,layerop); 
         
         for(f=0;f<layer->NFeature;f++) {
            if (layer->Select[f] && layer->Feature[f]) {
               if ((geom=OGR_F_GetGeometryRef(layer->Feature[f]))) {
                  new=OGM_GPCOnOGR(GPC_DIFF,geom,uni);
                  if  (new) {
                     if (layerres && !OGR_G_IsEmpty(new)) {
                        layerres->Feature[layerres->NFeature]=OGR_F_Clone(layer->Feature[f]);                        
                        OGR_F_SetGeometryDirectly(layerres->Feature[layerres->NFeature++],new);
                     } else {
                        OGR_F_SetGeometryDirectly(layer->Feature[f],new);                        
                        layer->Changed=1; 
                     }
                  }
               }
            }
         }

         break;

      case INTERSECTION:
         if (Objc!=2 && Objc!=3) {
            Tcl_WrongNumArgs(Interp,0,Objv,"layer [result]");
            return(TCL_ERROR);
         }
         layerop=OGR_LayerGet(Tcl_GetString(Objv[1]));
         if (!layerop) {
            Tcl_AppendResult(Interp,"\n   OGR_LayerStat: Layer name unknown: \"",Tcl_GetString(Objv[1]),"\"",(char *)NULL);
            return(TCL_ERROR);
         }

         if (Objc==3) {
            if (!(layerres=OGR_LayerResult(Interp,layer,Tcl_GetString(Objv[2]),layer->NFeature))) {
               return(TCL_ERROR);
            }
         }

         uni=OGM_GPCOnOGRLayer(GPC_UNION,layerop); 

         for(f=0;f<layer->NFeature;f++) {
            if (layer->Select[f] && layer->Feature[f]) {
               if ((geom=OGR_F_GetGeometryRef(layer->Feature[f]))) {
                  new=OGM_GPCOnOGR(GPC_INT,geom,uni);
                  if  (new) {
                     if (layerres && !OGR_G_IsEmpty(new)) {
                        layerres->Feature[layerres->NFeature]=OGR_F_Clone(layer->Feature[f]);                        
                        OGR_F_SetGeometryDirectly(layerres->Feature[layerres->NFeature++],new);
                     } else {
                        OGR_F_SetGeometryDirectly(layer->Feature[f],new);                        
                        layer->Changed=1; 
                     }
                  }
               }
            }
         }
         break;

      case CLIP:
         if (Objc!=2 && Objc!=3) {
            Tcl_WrongNumArgs(Interp,0,Objv,"geom [result]");
            return(TCL_ERROR);
         }
         uni=OGR_GeometryGet(Tcl_GetString(Objv[1]));
         if (!uni) {
            Tcl_AppendResult(Interp,"\n   OGR_LayerStat: Geometry name unknown: \"",Tcl_GetString(Objv[1]),"\"",(char *)NULL);
            return(TCL_ERROR);
         }

         if (Objc==3) {
            if (!(layerres=OGR_LayerResult(Interp,layer,Tcl_GetString(Objv[2]),layer->NFeature))) {
               return(TCL_ERROR);
            }
         }

         for(f=0;f<layer->NFeature;f++) {
            if (layer->Select[f] && layer->Feature[f]) {
               if ((geom=OGR_F_GetGeometryRef(layer->Feature[f]))) {
                  new=OGM_GPCOnOGR(GPC_INT,geom,uni);
                  if  (new) {
                     if (layerres && !OGR_G_IsEmpty(new)) {
                        layerres->Feature[layerres->NFeature]=OGR_F_Clone(layer->Feature[f]);                        
                        OGR_F_SetGeometryDirectly(layerres->Feature[layerres->NFeature++],new);
                     } else {
                        OGR_F_SetGeometryDirectly(layer->Feature[f],new);                        
                        layer->Changed=1; 
                     }
                  }
               }
            }
         }
         break;
         
      case SIMPLIFY:
         if (Objc!=2 && Objc!=3) {
            Tcl_WrongNumArgs(Interp,0,Objv,"tolerance [result]");
            return(TCL_ERROR);
         }
         Tcl_GetDoubleFromObj(Interp,Objv[1],&tol);

         if (Objc==3) {
            if (!(layerres=OGR_LayerResult(Interp,layer,Tcl_GetString(Objv[2]),layer->NFeature))) {
               return(TCL_ERROR);
            }
         }

         for(f=0;f<layer->NFeature;f++) {
            if (layer->Select[f] && layer->Feature[f]) {
               if ((geom=OGR_G_Clone(OGR_F_GetGeometryRef(layer->Feature[f])))) {
                  OGM_Simplify(tol,geom);
                  if (layerres) {
                     layerres->Feature[layerres->NFeature]=OGR_F_Clone(layer->Feature[f]);                        
                     OGR_F_SetGeometryDirectly(layerres->Feature[layerres->NFeature++],geom);
                  } else {
                     OGR_F_SetGeometryDirectly(layer->Feature[f],geom);
                     layer->Changed=1;   
                  }
               }
            }
         }
         break;

      case SEGMENTIZE:
         if (Objc!=2 && Objc!=3) {
            Tcl_WrongNumArgs(Interp,0,Objv,"length [result]");
            return(TCL_ERROR);
         }
         Tcl_GetDoubleFromObj(Interp,Objv[1],&tol);

         if (Objc==3) {
            if (!(layerres=OGR_LayerResult(Interp,layer,Tcl_GetString(Objv[2]),layer->NFeature))) {
               return(TCL_ERROR);
            }
         }

         for(f=0;f<layer->NFeature;f++) {
            if (layer->Select[f] && layer->Feature[f]) {
               if ((geom=OGR_G_Clone(OGR_F_GetGeometryRef(layer->Feature[f])))) {
                  OGR_G_Segmentize(geom,tol);
                  if (layerres) {
                     layerres->Feature[layerres->NFeature]=OGR_F_Clone(layer->Feature[f]);                        
                     OGR_F_SetGeometryDirectly(layerres->Feature[layerres->NFeature++],geom);
                  } else {
                     OGR_F_SetGeometryDirectly(layer->Feature[f],geom);
                     layer->Changed=1;   
                  }
               }
            }
         }
         break;

      case CLOSE:
         if (Objc!=1 && Objc!=2) {
            Tcl_WrongNumArgs(Interp,0,Objv,"[result]");
            return(TCL_ERROR);
         }
         
         if (Objc==2) {
            if (!(layerres=OGR_LayerResult(Interp,layer,Tcl_GetString(Objv[1]),layer->NFeature))) {
               return(TCL_ERROR);
            }
         }

         for(f=0;f<layer->NFeature;f++) {
            if (layer->Select[f] && layer->Feature[f]) {
               if ((geom=OGR_G_Clone(OGR_F_GetGeometryRef(layer->Feature[f])))) {
                  OGR_G_CloseRings(geom);
                  if (layerres) {
                     layerres->Feature[layerres->NFeature]=OGR_F_Clone(layer->Feature[f]);                        
                     OGR_F_SetGeometryDirectly(layerres->Feature[layerres->NFeature++],geom);
                  } else {
                     OGR_F_SetGeometryDirectly(layer->Feature[f],geom);
                     layer->Changed=1;   
                  }
               }
            }
         }
         break;

      case FLATTEN:
         if (Objc!=1 && Objc!=2) {
            Tcl_WrongNumArgs(Interp,0,Objv,"[result]");
            return(TCL_ERROR);
         }
         
         if (Objc==2) {
            if (!(layerres=OGR_LayerResult(Interp,layer,Tcl_GetString(Objv[1]),layer->NFeature))) {
               return(TCL_ERROR);
            }
         }
         
         for(f=0;f<layer->NFeature;f++) {
            if (layer->Select[f] && layer->Feature[f]) {
               if ((geom=OGR_G_Clone(OGR_F_GetGeometryRef(layer->Feature[f])))) {
                  OGR_G_FlattenTo2D(geom);
                  if (layerres) {
                     layerres->Feature[layerres->NFeature]=OGR_F_Clone(layer->Feature[f]);                        
                     OGR_F_SetGeometryDirectly(layerres->Feature[layerres->NFeature++],geom);
                  } else {
                     OGR_F_SetGeometryDirectly(layer->Feature[f],geom);
                     layer->Changed=1;   
                  }
               }
            }
         }
         break;

      case SORT:
         if (Objc!=2 && Objc!=3 ) {
            Tcl_WrongNumArgs(Interp,0,Objv,"field [invert]");
            return(TCL_ERROR);
         }
         layer->Sort.Field=-1;
         layer->Sort.Order=1;

         if (Objc==3) {
            Tcl_GetBooleanFromObj(Interp,Objv[2],&j);
            layer->Sort.Order=j?-1:1;
         }

         if (strlen(Tcl_GetString(Objv[1]))!=0) {
            str=Tcl_GetString(Objv[1]);
            f=OGR_FD_GetFieldIndex(layer->Def,str);
            if (f==-1) {
               Tcl_AppendResult(Interp,"\n   OGR_LayerStats: Invalid field",(char*)NULL);
               return(TCL_ERROR);
            }

            layer->Sort.Field=f;
            layer->Sort.Type=OGR_Fld_GetType(OGR_FD_GetFieldDefn(layer->Def,layer->Sort.Field));
         }
         return(OGR_LayerSort(Interp,layer));
         break;

      case TABLE:
         if (Objc!=1 && Objc!=2 && Objc!=3 && Objc!=4) {
            Tcl_WrongNumArgs(Interp,0,Objv,"var [Y0] [Y1]");
            return(TCL_ERROR);
         }

         y0=0;y1=0xFFFFFFF;
         if (Objc>=3) {
            Tcl_GetWideIntFromObj(Interp,Objv[2],&f);
            y0=f;
            if (Objc==4) {
               Tcl_GetWideIntFromObj(Interp,Objv[3],&f);
               y1=f;
            }
         }

         str=Tcl_GetString(Objv[1]);
         Tcl_UnsetVar2(Interp,str,NULL,0x0);

         Tcl_SetVar2Ex(Interp,str,"0,0",Tcl_NewStringObj("",-1),0x0);

         if (layer->Sort.Field!=-1 && layer->Sort.Table) {
            y0=y0<0?0:y0;
            y1=y1>layer->Sort.Nb-1?layer->Sort.Nb-1:y1;
            for(f=y0;f<=y1;f++) {
               sprintf(buf,"%li,0",f+1);
               Tcl_SetVar2Ex(Interp,str,buf,Tcl_NewIntObj(layer->Sort.Table[f]),0x0);

              for(j=0;j<OGR_FD_GetFieldCount(layer->Def);j++) {
                  sprintf(buf,"%li,%i",f+1,j+1);
                  Tcl_SetVar2Ex(Interp,str,buf,OGR_GetTypeObj(Interp,OGR_FD_GetFieldDefn(layer->Def,j),layer->Feature[layer->Sort.Table[f]],j),0x0);
               }
               Tcl_DoOneEvent(TCL_ALL_EVENTS|TCL_DONT_WAIT);
            }

         } else {
            j=0;
            for(f=0;f<layer->NFeature;f++) {
               if (layer->Select[f]) j++;
            }
            if (j!=layer->NFeature) {
               y0=0;
               y1=layer->NFeature;
            } else {
               y0=y0<0?0:y0;
               y1=(y1>layer->NFeature?layer->NFeature:y1);
            }

            for(f=y0,fop=y0;f<y1;f++) {
               if (layer->Select[f]) {
                  sprintf(buf,"%i,0",fop+1);
                  Tcl_SetVar2Ex(Interp,str,buf,Tcl_NewIntObj(f),0x0);

                  for(j=0;j<OGR_FD_GetFieldCount(layer->Def);j++) {
                     sprintf(buf,"%i,%i",fop+1,j+1);
                     Tcl_SetVar2Ex(Interp,str,buf,OGR_GetTypeObj(Interp,OGR_FD_GetFieldDefn(layer->Def,j),layer->Feature[f],j),0x0);
                  }
                  fop++;
                  Tcl_DoOneEvent(TCL_ALL_EVENTS|TCL_DONT_WAIT);
               }
            }
         }

         /*Insert field header row*/
         for(j=0;j<OGR_FD_GetFieldCount(layer->Def);j++) {
            sprintf(buf,"0,%i",j+1);
            Tcl_SetVar2Ex(Interp,Tcl_GetString(Objv[1]),buf,Tcl_NewStringObj(OGR_Fld_GetNameRef(OGR_FD_GetFieldDefn(layer->Def,j)),-1),0x0);
         }
         break;
   }

   return(TCL_OK);
}

/*----------------------------------------------------------------------------
 * Nom      : <OGR_LayerSort>
 * Creation : Fevrier 2014 - J.P. Gauthier - CMC/CMOE
 *
 * But      : Trie des feature selon in champ.
 *
 * Parametres :
 *  <Interp>  : Interpreteur TCL
 *  <Field>   : Champs a tester
 *  <Value>   : Valeur a verifier
 *  <Op>      : Test a effecuter
 *  <Exp>     : Expression reguliere a utiliser
 *
 * Retour:
 *  <Valid>   : Resultat du test (0,1)
 *
 * Remarques :
 *
 *----------------------------------------------------------------------------
*/
int OGR_LayerSort(Tcl_Interp *Interp,OGR_Layer *Layer) {
   
   int f;
 
   Layer->Sort.Nb=0;
 
   if (Layer->Sort.Field!=-1) {
      if (!Layer->Sort.Table) {
         if (!(Layer->Sort.Table=malloc(Layer->NFeature*sizeof(unsigned int)))) {
            Tcl_AppendResult(Interp,"\n   OGR_LayerSort: Unable to allocate temporary sort table",(char*)NULL);
            return(TCL_ERROR);
         }
      }
      for(f=0;f<Layer->NFeature;f++) {
         if (Layer->Select[f] && Layer->Feature[f])
            Layer->Sort.Table[Layer->Sort.Nb++]=f;
      }

      QSort_Layer=Layer;
      qsort(Layer->Sort.Table,Layer->Sort.Nb,sizeof(unsigned int),QSort_OGR);
   } else {
      if (Layer->Sort.Table) {
         free(Layer->Sort.Table);
         Layer->Sort.Table=NULL;
      }
   }
  
   return(TCL_OK);
}

/*----------------------------------------------------------------------------
 * Nom      : <OGR_LayerSelectTest>
 * Creation : Janvier 2006 - J.P. Gauthier - CMC/CMOE
 *
 * But      : Effectuer les test de selection sur les champs des features.
 *
 * Parametres :
 *  <Interp>  : Interpreteur TCL
 *  <Field>   : Champs a tester
 *  <Value>   : Valeur a verifier
 *  <Op>      : Test a effecuter
 *  <Exp>     : Expression reguliere a utiliser
 *
 * Retour:
 *  <Valid>   : Resultat du test (0,1)
 *
 * Remarques :
 *
 *----------------------------------------------------------------------------
*/
int OGR_LayerSelectTest(Tcl_Interp *Interp,Tcl_Obj *Field,Tcl_Obj *Value,Tcl_Obj *Op,regex_t *Exp) {

   Tcl_Obj *obj;
   double   fld,vld;
   char    *fls=NULL,*vls=NULL;
   int      n,i;

   if (Tcl_GetDoubleFromObj(Interp,Field,&fld)==TCL_ERROR) {
      fls=Tcl_GetString(Field);
   }

   Tcl_ListObjLength(Interp,Value,&n);
   if (Tcl_GetDoubleFromObj(Interp,Value,&vld)==TCL_ERROR) {
      vls=Tcl_GetString(Value);
   }

   if (n==1 && ((fls && !vls) || (!fls && vls))) {
      Tcl_AppendResult(Interp,"\n   OGR_LayerSelectTest: Invalid predicate types",(char*)NULL);
      return(0);
   }

   if (strcmp(Tcl_GetString(Op),"==")==0) {
      if (fls) {
         if (Tcl_StringMatch(fls,vls)) return(1);
      } else  {
         if (fld==vld) return(1);
      }
   } else if (strcmp(Tcl_GetString(Op),"~=")==0) {
      if (fls && Exp) {
         if (regexec(Exp,fls,0,NULL,0)==0) return(1);
      } else  {
         return(0);
      }
   } else if (strcasecmp(Tcl_GetString(Op),"!=")==0) {
      if (fls) {
         if (strcmp(fls,vls)!=0) return(1);
      } else  {
         if (fld!=vld) return(1);
      }
   } else if (strcmp(Tcl_GetString(Op),"<")==0) {
      if (fls) {
         if (strcmp(fls,vls)<0) return(1);
      } else  {
         if (fld<vld) return(1);
      }
   } else if (strcmp(Tcl_GetString(Op),"<=")==0) {
      if (fls) {
         if (strcmp(fls,vls)<=0) return(1);
      } else  {
         if (fld<=vld) return(1);
      }
   } else if (strcmp(Tcl_GetString(Op),">")==0) {
      if (fls) {
         if (strcmp(fls,vls)>0) return(1);
      } else  {
         if (fld>vld) return(1);
      }
   } else if (strcmp(Tcl_GetString(Op),">=")==0) {
      if (fls) {
         if (strcmp(fls,vls)>=0) return(1);
      } else  {
         if (fld>=vld) return(1);
      }
   } else if (strcmp(Tcl_GetString(Op),"<>")==0) {
      Tcl_ListObjLength(Interp,Value,&n);
      if (n>=2) {
         Tcl_ListObjIndex(Interp,Value,0,&obj);
         Tcl_GetDoubleFromObj(Interp,obj,&vld);
         if (fld>=vld) {
            Tcl_ListObjIndex(Interp,Value,1,&obj);
            Tcl_GetDoubleFromObj(Interp,obj,&vld);
            if (fld<=vld) {
               return(1);
            }
         }
      }
   } else if (strcmp(Tcl_GetString(Op),"[]")==0) {
      fls=Tcl_GetString(Field);
      Tcl_ListObjLength(Interp,Value,&n);
      for(i=0;i<n;i++) {
         Tcl_ListObjIndex(Interp,Value,i,&obj);
         if (Tcl_StringMatch(fls,Tcl_GetString(obj))) {
            return(1);
         }
      }
   }
   return(0);
}

/*----------------------------------------------------------------------------
 * Nom      : <OGR_LayerSelect>
 * Creation : Janvier 2006 - J.P. Gauthier - CMC/CMOE
 *
 * But      : Effectuer les test de selection sur les champs des features.
 *
 * Parametres  :
 *  <Interp>   : Interpreteur TCL
 *  <Layer>    : Couche
 *  <Predicates: Predicat a tester
 *
 * Retour:
 *  <TCL_...>  : Code de retour TCL
 *
 * Remarques :
 *
 *----------------------------------------------------------------------------
*/
int OGR_LayerSelect(Tcl_Interp *Interp,OGR_Layer *Layer,Tcl_Obj *Predicates) {

   Tcl_Obj      *st,*it,*op,*val,*fd;
   Tcl_WideInt   w;
   int           n,i,ns,nf,fld,ni,len,err;
   unsigned int  f;
   regex_t      *exp=NULL;
   char         *msg;
   OGRFieldDefnH defn=NULL;

   Tcl_ListObjLength(Interp,Predicates,&ns);

   /*Reset the selection list*/
   memset(Layer->Select,0x1,Layer->NFeature);

   /*If no predicate, done*/
   if (!ns)
      return(TCL_OK);

   /*Parse every predicate*/
   for(n=0;n<ns;n++) {
      Tcl_ListObjIndex(Interp,Predicates,n,&st);
      Tcl_ListObjLength(Interp,st,&ni);
      if (ni!=3) {
         Tcl_AppendResult(Interp,"\n   OGR_LayerSelect: Malformed predicates",(char*)NULL);
         return TCL_ERROR;
      }
      /*Extract predicate parts*/
      Tcl_ListObjIndex(Interp,st,0,&it);
      Tcl_ListObjIndex(Interp,st,1,&op);
      Tcl_ListObjIndex(Interp,st,2,&val);

      /*In case of index selection*/
      if (strcmp(Tcl_GetString(op),"#")==0) {
         memset(Layer->Select,0x0,Layer->NFeature);
         Tcl_ListObjLength(Interp,val,&nf);
         for(i=0;i<nf;i++) {
            Tcl_ListObjIndex(Interp,val,i,&fd);
            Tcl_GetWideIntFromObj(Interp,fd,&w);
            if (w>=0 && w<Layer->NFeature)
               Layer->Select[w]=1;
         }
         return(TCL_OK);
      }

      /*Get the specified feature field*/
      fld=OGR_FD_GetFieldIndex(Layer->Def,Tcl_GetString(it));
      if (fld==-1) {
         Tcl_AppendResult(Interp,"\n   OGR_LayerSelect: Invalid field",(char*)NULL);
         return(TCL_ERROR);
      }
      defn=OGR_FD_GetFieldDefn(Layer->Def,fld);

      /*In case of regexp selection*/
      if (strcmp(Tcl_GetString(op),"~=")==0) {
         if (!(exp=(regex_t*)malloc(sizeof(regex_t)))) {
            Tcl_AppendResult(Interp,"\n   OGR_LayerSelect: Unable to allocate regexp buffer",(char*)NULL);
            return(TCL_ERROR);
         }
         if ((err=regcomp(exp,Tcl_GetString(val),REG_ICASE|REG_NOSUB))!=0) {
            len=regerror(err,exp,NULL,0);
            msg=(char*)alloca(len);
            regerror(err,exp,msg,len);
            Tcl_AppendResult(Interp,"\n   OGR_LayerSelect: Invalid regular expression ( ",msg," )",(char*)NULL);
            regfree(exp);
            return(TCL_ERROR);
         }
      }

      /*Parse every feature*/
      for(f=0;f<Layer->NFeature;f++) {
         if (Layer->Feature[f]) {
            fd=OGR_GetTypeObj(Interp,defn,Layer->Feature[f],fld);

            /*Test for validity*/
            if (!OGR_LayerSelectTest(Interp,fd,val,op,exp)) {
               Layer->Select[f]=0;
            }
            Tcl_DecrRefCount(fd);
         }
      }
      if (exp) {
         regfree(exp);
         exp=NULL;
      }
   }
   return(TCL_OK);
}

/*----------------------------------------------------------------------------
 * Nom      : <OGR_GetTypeObj>
 * Creation : Aout J.P. Gauthier - CMC/CMOE
 *
 * But      : Recuperer le type d'un champs
 *
 * Parametres :
 *  <Interp>  : Interpreteur Tcl
 *  <Field>   : Structure Champs
 *  <Feature> : Feature
 *  <Index>   : Index du champs recuperer
 *
 * Retour     :
 *
 * Remarques  :
 *
 *----------------------------------------------------------------------------
*/
Tcl_Obj* OGR_GetTypeObj(Tcl_Interp *Interp,OGRFieldDefnH Field,OGRFeatureH Feature,int Index) {

   int             n,nb,year,month,day,hour,min,sec,tz;
   time_t          time;
   char            **clist;
   const int       *ilist;
   const long long *llist;
   const double    *dlist;
   Tcl_Obj         *obj;

   obj=Tcl_NewObj();

   if (Feature) {
      switch (OGR_Fld_GetType(Field)) {
         case OFTInteger:
            Tcl_SetIntObj(obj,OGR_F_GetFieldAsInteger(Feature,Index));
            break;

         case OFTIntegerList:
            ilist=OGR_F_GetFieldAsIntegerList(Feature,Index,&nb);
            for(n=0;n<nb;n++) {
               Tcl_ListObjAppendElement(Interp,obj,Tcl_NewIntObj(ilist[n]));
            }
            break;

         case OFTInteger64:
            Tcl_SetWideIntObj(obj,OGR_F_GetFieldAsInteger64(Feature,Index));
            break;

         case OFTInteger64List:
            llist=OGR_F_GetFieldAsInteger64List(Feature,Index,&nb);
            for(n=0;n<nb;n++) {
               Tcl_ListObjAppendElement(Interp,obj,Tcl_NewWideIntObj(llist[n]));
            }
            break;
            
         case OFTReal:
            Tcl_SetDoubleObj(obj,OGR_F_GetFieldAsDouble(Feature,Index));
            break;

         case OFTRealList:
            dlist=OGR_F_GetFieldAsDoubleList(Feature,Index,&nb);
            for(n=0;n<nb;n++) {
               Tcl_ListObjAppendElement(Interp,obj,Tcl_NewDoubleObj(dlist[n]));
            }
            break;

         case OFTString:
            Tcl_SetStringObj(obj,OGR_F_GetFieldAsString(Feature,Index),-1);
            break;

         case OFTStringList:
            clist=OGR_F_GetFieldAsStringList(Feature,Index);
            n=0;
            while(clist[n]) {
               Tcl_ListObjAppendElement(Interp,obj,Tcl_NewStringObj(clist[n++],-1));
            }
            break;

         case OFTTime:
            OGR_F_GetFieldAsDateTime(Feature,Index,&year,&month,&day,&hour,&min,&sec,&tz);
            time=hour*3600+min*60+sec;
            Tcl_SetLongObj(obj,time);
            break;

         case OFTDate:
            OGR_F_GetFieldAsDateTime(Feature,Index,&year,&month,&day,&hour,&min,&sec,&tz);
            time=System_DateTime2Seconds(year*10000+month*100+day,0,tz==100?1:0);
            Tcl_SetLongObj(obj,time);
         break;

         case OFTDateTime:
            OGR_F_GetFieldAsDateTime(Feature,Index,&year,&month,&day,&hour,&min,&sec,&tz);
            time=System_DateTime2Seconds(year*10000+month*100+day,hour*10000+min*100+sec,tz==100?1:0);
            Tcl_SetLongObj(obj,time);
            break;

         case OFTWideString:
         case OFTWideStringList:
         case OFTBinary:
            break;
      }
   }
   return(obj);
}

int OGR_SetTypeObj(Tcl_Interp *Interp,Tcl_Obj* Obj,OGRLayerH Layer,OGRFieldDefnH Field,OGRFeatureH Feature,int Index) {

   int       year,month,day,hour,min,sec,tz,dt,tm,n,nobj,*ival;
   long long *lval;
   double    *dval;
   time_t    time;
   Tcl_Obj  *obj;
   char    **list;

   switch (OGR_Fld_GetType(Field)) {
      case OFTInteger:
         OGR_F_SetFieldString(Feature,Index,Tcl_GetString(Obj));
         break;

      case OFTIntegerList:
         Tcl_ListObjLength(Interp,Obj,&nobj);
         if (!(ival=(int*)calloc(nobj,sizeof(int)))) {
            Tcl_AppendResult(Interp,"\n   OGR_SetTypeObj: Unable to allocate list array",(char*)NULL);
            return(TCL_ERROR);
         }
         for(n=0;n<nobj;n++) {
            Tcl_ListObjIndex(Interp,Obj,n,&obj);
            Tcl_GetIntFromObj(Interp,obj,&ival[n]);
         }
         OGR_F_SetFieldIntegerList(Feature,Index,n,ival);
         break;

      case OFTInteger64:
         OGR_F_SetFieldString(Feature,Index,Tcl_GetString(Obj));
         break;

      case OFTInteger64List:
         Tcl_ListObjLength(Interp,Obj,&nobj);
         if (!(lval=(long long*)calloc(nobj,sizeof(long long)))) {
            Tcl_AppendResult(Interp,"\n   OGR_SetTypeObj: Unable to allocate list array",(char*)NULL);
            return(TCL_ERROR);
         }
         for(n=0;n<nobj;n++) {
            Tcl_ListObjIndex(Interp,Obj,n,&obj);
            Tcl_GetWideIntFromObj(Interp,obj,(Tcl_WideInt*)&lval[n]);
         }
         OGR_F_SetFieldInteger64List(Feature,Index,n,lval);
         break;

      case OFTReal:
         OGR_F_SetFieldString(Feature,Index,Tcl_GetString(Obj));
         break;

      case OFTRealList:
         Tcl_ListObjLength(Interp,Obj,&nobj);
         if (!(dval=(double*)calloc(nobj,sizeof(double)))) {
            Tcl_AppendResult(Interp,"\n   OGR_SetTypeObj: Unable to allocate list array",(char*)NULL);
            return(TCL_ERROR);
         }
         for(n=0;n<nobj;n++) {
            Tcl_ListObjIndex(Interp,Obj,n,&obj);
            Tcl_GetDoubleFromObj(Interp,obj,&dval[n]);
         }
         OGR_F_SetFieldDoubleList(Feature,Index,n,dval);
         break;

      case OFTString:
         OGR_F_SetFieldString(Feature,Index,Tcl_GetString(Obj));
         break;

      case OFTStringList:
         if (Tcl_SplitList(Interp,Tcl_GetString(Obj),&nobj,(const char***)&list)==TCL_ERROR) {
            return TCL_ERROR;
         }
         OGR_F_SetFieldStringList(Feature,Index,list);                       
         Tcl_Free((char*)list);
         break;

      case OFTTime:
      case OFTDate:
      case OFTDateTime:
         Tcl_GetLongFromObj(Interp,Obj,&time);
         tz=100;
         time=System_Seconds2DateTime(time,&dt,&tm,tz==100?1:0);
         year=dt/10000;
         month=(dt-year*10000)/100;
         day=dt-year*10000-month*100;
         hour=tm/10000;
         min=(tm-hour*10000)/100;
         sec=tm-hour*10000-min*100;
         OGR_F_SetFieldDateTime(Feature,Index,year,month,day,hour,min,sec,tz);
         break;

      case OFTWideString:
      case OFTWideStringList:
      case OFTBinary:
         break;
   }

   return(TCL_OK);
}

/*----------------------------------------------------------------------------
 * Nom      : <OGR_SingleTypeString>
 * Creation : Aout J.P. Gauthier - CMC/CMOE
 *
 * But      : Recuperer un champs en chaine de caractere
 *
 * Parametres :
 *  <Buf>     : Chaine a recuperer
 *  <Spec>    : Data specification
 *  <Field>   : Structure Champs a recuperer
 *  <Feature> : Feature
 *  <Index>   : Index du champs recuperer
 *  <Desc>    : Liste de la geometrie
 *
 * Retour     :
 *
 * Remarques  :
 *
 *----------------------------------------------------------------------------
*/
void OGR_SingleTypeString(char *Buf,TDataSpec *Spec,OGRFieldDefnH Field,OGRFeatureH Feature,int Index) {

   int   year,month,day,hour,min,sec,tz;

   switch (OGR_Fld_GetType(Field)) {
      case OFTInteger:
         sprintf(Buf,"%i",OGR_F_GetFieldAsInteger(Feature,Index));
         break;

      case OFTInteger64:
         sprintf(Buf,"%lli",OGR_F_GetFieldAsInteger64(Feature,Index));
         break;
         
      case OFTReal:
         DataSpec_Format(Spec,VAL2SPEC(Spec,OGR_F_GetFieldAsDouble(Feature,Index)),Buf);
//         sprintf(Buf,"%f",OGR_F_GetFieldAsDouble(Feature,Index));
         break;

      case OFTString:
         sprintf(Buf,"%s",OGR_F_GetFieldAsString(Feature,Index));
         break;

      case OFTTime:
         OGR_F_GetFieldAsDateTime(Feature,Index,&year,&month,&day,&hour,&min,&sec,&tz);
         sprintf(Buf,"%02i:%02i:%02i",hour,min,sec);
         break;

      case OFTDate:
         OGR_F_GetFieldAsDateTime(Feature,Index,&year,&month,&day,&hour,&min,&sec,&tz);
         sprintf(Buf,"%i-%02i-%02i",year,month,day);
         break;

      case OFTDateTime:
         OGR_F_GetFieldAsDateTime(Feature,Index,&year,&month,&day,&hour,&min,&sec,&tz);
         sprintf(Buf,"%i-%02i-%02i %02i:%02i:%02i",year,month,day,hour,min,sec);
         break;

      case OFTIntegerList:
      case OFTInteger64List:
      case OFTRealList:
      case OFTStringList:
      case OFTWideString:
      case OFTWideStringList:
      case OFTBinary:
         sprintf(Buf,"Not Recognised");
         break;
   }
}

/*----------------------------------------------------------------------------
 * Nom      : <OGR_FieldCreate>
 * Creation : Aout J.P. Gauthier - CMC/CMOE
 *
 * But      : Creation d'un nouveau champs
 *
 * Parametres :
 *  <Layer>   : Layer maitre
 *  <Field>   : Nouveau champs
 *  <Type>    : Type du champs
 *  <Width>   : Largeur du champs
 *  <Prec>    : Precision du champs
 *
 * Retour     :
 * <OGRFieldDefnH> : Field def
 * *
 * Remarques  :
 *
 *----------------------------------------------------------------------------
*/
OGRFieldDefnH OGR_FieldCreate(OGR_Layer *Layer,char *Field,char *Type,int Width,int Prec) {

   OGRFieldDefnH  field=NULL;

   if (Field && strlen(Field)) {
      if (strcmp(Type,"Integer")==0) {
         field=OGR_Fld_Create(Field,OFTInteger);
      } else if (strcmp(Type,"Real")==0) {
         field=OGR_Fld_Create(Field,OFTReal);
         if (Prec) OGR_Fld_SetPrecision(field,Prec);
         if (Width) OGR_Fld_SetWidth(field,Width);
      } else if (strcmp(Type,"String")==0) {
         field=OGR_Fld_Create(Field,OFTString);
         if (Width) OGR_Fld_SetWidth(field,Width);
      } else if (strcmp(Type,"WideString")==0) {
         field=OGR_Fld_Create(Field,OFTWideString);
         if (Width) OGR_Fld_SetWidth(field,Width);
      } else if (strcmp(Type,"IntegerList")==0) {
         field=OGR_Fld_Create(Field,OFTIntegerList);
      } else if (strcmp(Type,"RealList")==0) {
         field=OGR_Fld_Create(Field,OFTRealList);
         if (Prec) OGR_Fld_SetPrecision(field,Width);
         if (Width) OGR_Fld_SetWidth(field,Width);
      } else if (strcmp(Type,"StringList")==0) {
         field=OGR_Fld_Create(Field,OFTStringList);
         if (Width) OGR_Fld_SetWidth(field,Width);
      } else if (strcmp(Type,"WideStringList")==0) {
         field=OGR_Fld_Create(Field,OFTWideStringList);
         if (Width) OGR_Fld_SetWidth(field,Width);
      } else if (strcmp(Type,"DateTime")==0) {
         field=OGR_Fld_Create(Field,OFTDateTime);
      } else if (strcmp(Type,"Date")==0) {
         field=OGR_Fld_Create(Field,OFTDate);
      } else if (strcmp(Type,"Time")==0) {
         field=OGR_Fld_Create(Field,OFTTime);
      } else if (strcmp(Type,"Binary")==0) {
         field=OGR_Fld_Create(Field,OFTBinary);
      }
   }

   if (field) {

      /*Add the field to the structure*/
      if (OGR_L_CreateField(Layer->Layer,field,1)!=OGRERR_NONE) {
         OGR_Fld_Destroy(field);
         return(NULL);
      }

      Layer->Changed=1;
   }
   return(field);
}

/*--------------------------------------------------------------------------------------------------------------
 * Nom          : <OGR_LayerUpdate>
 * Creation     : Septembre 2010 J.P. Gauthier - CMC/CMOE
 *
 * But          : Mettre a jour les donnees de la couche (OGR side)
 *
 * Parametres   :
 *  <Layer>     : Couche
 *  <Reload>    : Reload or not
 *
 * Retour       :
 *
 * Remarques    :
 *
 *---------------------------------------------------------------------------------------------------------------
*/
int OGR_LayerUpdate(OGR_Layer *Layer) {

   unsigned int f;

   if (Layer->Update) {
      for(f=0;f<Layer->NFeature;f++) {
         if (Layer->Feature[f]) {
            if (OGR_L_SetFeature(Layer->Layer,Layer->Feature[f])!=OGRERR_NONE) {
               return(FALSE);
            }
         }
      }
      Layer->Update=0;
   }
   return(TRUE);
}

/*--------------------------------------------------------------------------------------------------------------
 * Nom          : <OGR_LayerRead>
 * Creation     : Juillet 2004 J.P. Gauthier - CMC/CMOE
 *
 * But          : Lire les donnees d'une bande raster
 *
 * Parametres   :
 *   <Interp>   : L'interpreteur Tcl
 *   <Name>     : Le nom de la bande a creer
 *   <FileId>   : Identificateur du fichier
 *   <Idx>      : Index de la couche
 *
 * Retour       : Code d'erreur standard TCL
 *
 * Remarques    :
 *
 *---------------------------------------------------------------------------------------------------------------
*/
int OGR_LayerReadFeature(Tcl_Interp *Interp,OGR_Layer *Layer) {
   
   unsigned int f;

   OGR_L_ResetReading(Layer->Layer);
   for(f=0;f<Layer->NFeature;f++) {
      
      if (!(Layer->Feature[f]=OGR_L_GetNextFeature(Layer->Layer))) {
         Tcl_AppendResult(Interp,"OGR_LayerReadFeature: Unable to read features",(char*)NULL);
         return(TCL_ERROR);
      }
      if (!Layer->Space) 
         Layer->Space=OGR_G_GetCoordinateDimension(OGR_F_GetGeometryRef(Layer->Feature[f]));
   }
   return(TCL_OK);   
}

int OGR_LayerRead(Tcl_Interp *Interp,char *Name,char *FileId,int Idx) {

   OGR_File    *file=NULL;
   OGR_Layer   *layer=NULL;
   OGREnvelope  env;
   
   if (!(file=OGR_FileGet(Interp,FileId))) {
      return(TCL_ERROR);
   }

   layer=OGR_LayerGet(Name);
   if (layer) {
      OGR_LayerDestroy(Interp,Name);
   }

   if (!(layer=OGR_LayerCreate(Interp,Name,NULL,wkbNone,NULL))) {
      return(TCL_ERROR);
   }

   // Copy layer in memory using Memory driver to be able to do whatever we want with it
   layer->Data=GDALCreate(OGRGetDriverByName("Memory"),Name,0,0,0,GDT_Unknown,NULL);
   layer->Layer=GDALDatasetCopyLayer(layer->Data,GDALDatasetGetLayer(file->Data,Idx),Name,NULL);   
   
//   layer->Layer=GDALDatasetGetLayer(file->Data,Idx);
   if (!layer->Layer) {
      Tcl_AppendResult(Interp,"OGR_LayerRead: Unable to read layer",(char*)NULL);
      return(TCL_ERROR);
   }
   layer->Def=OGR_L_GetLayerDefn(layer->Layer);
   if (!layer->Def) {
      Tcl_AppendResult(Interp,"OGR_LayerRead: Unable to read layer definition",(char*)NULL);
      return(TCL_ERROR);
   }
   OGR_FD_Reference(layer->Def);

   layer->NFeature=OGR_L_GetFeatureCount(layer->Layer,1);
   if (layer->NFeature) {
      layer->Feature=calloc(layer->NFeature,sizeof(OGRFeatureH));
      layer->Select=malloc(layer->NFeature*sizeof(char));
      
      if (!layer->Feature || !layer->Select) {
         Tcl_AppendResult(Interp,"OGR_LayerRead: Unable to allocate feature buffer",(char*)NULL);
         return(TCL_ERROR);
      }
      memset(layer->Select,0x1,layer->NFeature);

      /* Parse features */
      if (OGR_LayerReadFeature(Interp,layer)==TCL_ERROR) {
         return(TCL_ERROR);
      }
      
      if (!(layer->Loc=(Coord*)malloc(layer->NFeature*sizeof(Coord)))) {
         Tcl_AppendResult(Interp,"OGR_LayerRead: Unable to allocate location buffer",(char*)NULL);
         return(TCL_ERROR);
      }
   }
   
   layer->File=file;
   layer->GRef=GeoRef_Find(GeoRef_WKTSetup(0,0,NULL,0,0,0,0,NULL,NULL,NULL,OGR_L_GetSpatialRef(layer->Layer)));
   OGR_L_GetExtent(layer->Layer,&env,1);
   
   // Size the georef for grid interpolation
   GeoRef_Size(layer->GRef,env.MinX-1.0,env.MinY-1.0,env.MaxX+1.0,env.MaxY+1.0,0);
   return(TCL_OK);
}

/*--------------------------------------------------------------------------------------------------------------
 * Nom          : <OGR_LayerCopy>
 * Creation     : Aout 2013 J.P. Gauthier - CMC/CMOE
 *
 * But          : Copier une couche
 *
 * Parametres   :
 *   <Interp>   : L'interpreteur Tcl
 *   <From>     : Nom de la couche source
 *   <To>       : Nom de la chouche destination
 *
 * Retour       : Code d'erreur standard TCL
 *
 * Remarques    :
 *
 *---------------------------------------------------------------------------------------------------------------
*/
int OGR_LayerCopy(Tcl_Interp *Interp,char *From,char *To) {

   OGR_Layer   *from=NULL,*to=NULL;
   unsigned int f;

   from=OGR_LayerGet(From);
   if (!from) {
      Tcl_AppendResult(Interp,"OGR_LayerCopy: Invalid layer: ",From,(char*)NULL);
      return(TCL_ERROR);
   }

   to=OGR_LayerGet(To);
   if (to) {
      Tcl_AppendResult(Interp,"OGR_LayerCopy: Layer already exist: ",To,(char*)NULL);
      return(TCL_ERROR);
   }
   
   if (!(to=OGR_LayerCreate(Interp,To,(char*)OGR_FD_GetName(from->Def),wkbUnknown,NULL))) {
      Tcl_AppendResult(Interp,"OGR_LayerCopy: Unable to create layer",(char*)NULL);
      return(TCL_ERROR);
   }
 
   to->GRef=GeoRef_Copy(from->GRef);
   to->Def=from->Def;
   to->Changed=1;
   OGR_FD_Reference(to->Def);

   to->NFeature=from->NFeature;
   if (to->NFeature) {
      to->Feature=calloc(to->NFeature,sizeof(OGRFeatureH));
      to->Select=malloc(to->NFeature*sizeof(char));
      
      if (!to->Feature || !to->Select) {
         Tcl_AppendResult(Interp,"OGR_LayerCopy: Unable to allocate feature buffer",(char*)NULL);
         return(TCL_ERROR);
      }
      memset(to->Select,0x1,to->NFeature);

      /* Parse features */
      for(f=0;f<to->NFeature;f++) {
         if (from->Feature[f])
            to->Feature[f]=OGR_F_Clone(from->Feature[f]);
      }
      
      if (!(to->Loc=(Coord*)malloc(to->NFeature*sizeof(Coord)))) {
         Tcl_AppendResult(Interp,"OGR_LayerCopy: Unable to allocate location buffer",(char*)NULL);
         return(TCL_ERROR);
      }
   }
  
   return(TCL_OK);
}

/*--------------------------------------------------------------------------------------------------------------
 * Nom          : <OGR_LayerWrite>
 * Creation     : Juillet 2006 J.P. Gauthier - CMC/CMOE
 *
 * But          : Ecrire un layer dans un fichier
 *
 * Parametres   :
 *   <Interp>   : L'interpreteur Tcl
 *   <Name>     : Le nom de la bande a creer
 *   <FileId>   : Identificateur du fichier
 *
 * Retour       : Code d'erreur standard TCL
 *
 * Remarques    :
 *     Cette fonction permet d'ecrire un layer dans un nouveau fichier, sans avoir a creer le fichier
 *     avant, donc effectivement de copier des layers
 *
 *---------------------------------------------------------------------------------------------------------------
*/
int OGR_LayerWrite(Tcl_Interp *Interp,char *Name,char *FileId) {

   char        **opt=NULL;
   unsigned int  f,trans=0;

   OGR_File  *file=NULL;
   OGR_Layer *layer=NULL;

   OGRFeatureH     feature;
   OGRFeatureDefnH defn;
   OGRLayerH       olayer;

   if (!(file=OGR_FileGet(Interp,FileId))) {
      return(TCL_ERROR);
   }

   layer=OGR_LayerGet(Name);
   if (!layer) {
      Tcl_AppendResult(Interp,"OGR_LayerRead: Invalid layer ",Name,(char*)NULL);
      return(TCL_ERROR);
   }

   if (GDALDatasetTestCapability(file->Data,ODsCCreateLayer)) {
      olayer=GDALDatasetCreateLayer(file->Data,OGR_FD_GetName(layer->Def),layer->GRef->Spatial,wkbUnknown,opt);
      defn=OGR_L_GetLayerDefn(olayer);
   } else {
      Tcl_AppendResult(Interp,"OGR_LayerWrite: Write operation not supported for this file type",(char*)NULL);
      return(TCL_ERROR);
   }

   for(f=0;f<OGR_FD_GetFieldCount(layer->Def);f++) {
      OGR_L_CreateField(olayer,OGR_FD_GetFieldDefn(layer->Def,f),1);
   }

   // Do everything as part of a single transaction (very slow for DB formats that supports transactions otherwise (SQLite, GPKG))
   if (GDALDatasetTestCapability(file->Data,ODsCTransactions)) {
      if (GDALDatasetStartTransaction(file->Data,0)==OGRERR_NONE) {
         trans=1;
      }
   }

   for(f=0;f<layer->NFeature;f++) {
      if (layer->Feature[f]) {
         feature=OGR_F_Create(defn);
         OGR_F_SetFrom(feature,layer->Feature[f],True);
         if (OGR_L_CreateFeature(olayer,feature)!=OGRERR_NONE) {
            Tcl_AppendResult(Interp,"\n   OGR_LayerWrite: Problems creating feature",(char*)NULL);
            if (trans && GDALDatasetRollbackTransaction(file->Data)!=OGRERR_NONE) {
               Tcl_AppendResult(Interp,"\n   OGR_LayerWrite: Could not rollback transaction",(char*)NULL);
            }
            return(TCL_ERROR);
         }
      }
   }

   // Close the transaction
   if (trans) {
      if (GDALDatasetCommitTransaction(file->Data)!=OGRERR_NONE) {
         Tcl_AppendResult(Interp,"\n   OGR_LayerWrite: Could not commit transaction",(char*)NULL);
         return(TCL_ERROR);
      }
   }

   layer->File=file;
   layer->Update=0;
   layer->Changed=0;

   return(TCL_OK);
}

/*--------------------------------------------------------------------------------------------------------------
 * Nom          : <OGR_LayerSQLSelect>
 * Creation     : Juillet 2004 J.P. Gauthier - CMC/CMOE
 *
 * But          : Lire les donnees d'une bande raster
 *
 * Parametres   :
 *   <Interp>   : L'interpreteur Tcl
 *   <Name>     : Le nom de la bande a creer
 *   <FileId>   : Identificateur du fichier
 *   <Idx>      : Index de la couche
 *
 * Retour       : Code d'erreur standard TCL
 *
 * Remarques    :
 *
 *---------------------------------------------------------------------------------------------------------------
*/
int OGR_LayerSQLSelect(Tcl_Interp *Interp,char *Name,char *FileId,char *Statement,char *Geom) {

   OGR_File     *file=NULL;
   OGR_Layer    *layer;
   OGRLayerH     srcl;
   OGRGeometryH  geom;
   OGREnvelope   env;

   if (!(file=OGR_FileGet(Interp,FileId))) {
      return(TCL_ERROR);
   }

   layer=OGR_LayerGet(Name);
   if (layer) {
      OGR_LayerDestroy(Interp,Name);
   }

   if (!(layer=OGR_LayerCreate(Interp,Name,NULL,wkbNone,NULL))) {
      Tcl_AppendResult(Interp,"OGR_LayerSQLSelect: Could not create layer: ",Name,(char*)NULL);
      return(TCL_ERROR);
   }

   geom=NULL;
   if (Geom) {
      geom=OGR_GeometryGet(Geom);
      if (!geom) {
         Tcl_AppendResult(Interp,"OGR_LayerSQLSelect: invalid geometry: ",Geom,(char*)NULL);
         return(TCL_ERROR);
      }
   }

   srcl=GDALDatasetExecuteSQL(file->Data,Statement,geom,NULL);
   
   // Copy layer in memory using Memory driver to be able to do whatever we want with it
   layer->Data=GDALCreate(OGRGetDriverByName("Memory"),Name,0,0,0,GDT_Unknown,NULL);
   layer->Layer=GDALDatasetCopyLayer(layer->Data,srcl,Name,NULL);   

   GDALDatasetReleaseResultSet(file->Data,srcl);
   
   if (layer->Layer) {
      layer->Def=OGR_L_GetLayerDefn(layer->Layer);
      if (!layer->Def) {
         Tcl_AppendResult(Interp,"OGR_LayerRead: Unable to read layer definition",(char*)NULL);
         return(TCL_ERROR);
      }
      OGR_FD_Reference(layer->Def);

      layer->NFeature=OGR_L_GetFeatureCount(layer->Layer,1);

      if (layer->NFeature) {
         layer->Feature=calloc(layer->NFeature,sizeof(OGRFeatureH));
         layer->Select=malloc(layer->NFeature*sizeof(char));
         
         if (!layer->Feature || !layer->Select) {
            Tcl_AppendResult(Interp,"OGR_LayerRead: Unable to allocate feature buffer",(char*)NULL);
            return(TCL_ERROR);
         }
         memset(layer->Select,0x1,layer->NFeature);

         /* Parse features */
         if (OGR_LayerReadFeature(Interp,layer)==TCL_ERROR) {
            return(TCL_ERROR);
         }
         
         if (!(layer->Loc=(Coord*)malloc(layer->NFeature*sizeof(Coord)))) {
            Tcl_AppendResult(Interp,"OGR_LayerRead: Unable to allocate location buffer",(char*)NULL);
            return(TCL_ERROR);
         }
      }

      layer->GRef=GeoRef_Find(GeoRef_WKTSetup(0,0,NULL,0,0,0,0,NULL,NULL,NULL,OGR_L_GetSpatialRef(layer->Layer)));
      OGR_L_GetExtent(layer->Layer,&env,1);
      GeoRef_Size(layer->GRef,env.MinX,env.MinY,env.MaxX,env.MaxY,0);
   }
   return(TCL_OK);
}

/*--------------------------------------------------------------------------------------------------------------
 * Nom          : <ORG_LayerImport>
 * Creation     : Novembre 2004 J.P. Gauthier - CMC/CMOE
 *
 * But          : Importer des donnees dans une couche OGR
 *
 * Parametres   :
 *   <Interp>   : L'interpreteur Tcl
 *   <Band>     : Bande
 *   <Field>    : Champs
 *   <Grid>     : Coordonnee points de grille
 *   <Side>     : Side of coordinates reference to force coordinates fro LatLon referential (-1:negative,1:positive,0:keep as is)
 *
 * Retour       : Code d'erreur standard TCL
 *
 * Remarques    :
 *
 *---------------------------------------------------------------------------------------------------------------
*/
int OGR_LayerImport(Tcl_Interp *Interp,OGR_Layer *Layer,Tcl_Obj *Fields,int Grid,int Side) {

   int       i,j,k,n,d,idx=0,cidx=-1,f,nf,yyyy,mm,dd,h,m,s;
   double    lat,lon,x,y,spd,dir;
   char      buf[64],*mask=NULL,style[256];
   OGRGeometryH poly=NULL,geom=NULL,cont=NULL;
   TList     *list;
   T3DArray  *array;
   TData    **field;
   TDataSpec *spec;
   Tcl_Obj   *obj;

   if (!Layer) {
      Tcl_AppendResult(Interp,"OGR_LayerImport: Invalid layer",(char*)NULL);
      return(TCL_ERROR);
   }

   // Get the number of fields provided
   Tcl_ListObjLength(Interp,Fields,&nf);
   field=(TData**)alloca(nf*sizeof(TData*));

   // Get the fields from their name
   for(f=0;f<nf;f++) {

      Tcl_ListObjIndex(Interp,Fields,f,&obj);
      field[f]=Data_Get(Tcl_GetString(obj));

      if (!field[f]) {
         Tcl_AppendResult(Interp,"OGR_LayerImport: Invalid field ",Tcl_GetString(obj),(char*)NULL);
         return(TCL_ERROR);
      }

      if (!f) {
          idx=FSIZE2D(field[0]->Def);
          spec=field[0]->Spec;
      } else {
         // Make sure the size of the field is compatible
         if (idx!=FSIZE2D(field[f]->Def)) {
            Tcl_AppendResult(Interp,"OGR_LayerImport: field size differ",(char*)NULL);
            return(TCL_ERROR);
         }

         // Make sure the specs are compatible
         if (spec->RenderContour!=0 ^ field[f]->Spec->RenderContour!=0
               || spec->RenderParticle!=0 ^ field[f]->Spec->RenderParticle!=0
               || spec->RenderTexture!=0 ^ field[f]->Spec->RenderTexture!=0
               || spec->RenderVector!=0 ^ field[f]->Spec->RenderVector!=0
               || spec->RenderGrid!=0 ^ field[f]->Spec->RenderGrid!=0) {
            Tcl_AppendResult(Interp,"OGR_LayerImport: Cannot import multiple fields in a single layer if their rendering types vary",(char*)NULL);
            return(TCL_ERROR);
         }
      }
      Data_PreInit(field[f]);
   }

#ifdef HAVE_RMN
   if (spec->RenderParticle && !FSTD_FieldReadMesh(field[0])) {
      Tcl_AppendResult(Interp,"OGR_LayerImport: Cannot import field particles",(char*)NULL);
      return(TCL_ERROR);
   }
#endif
   Layer->Changed=1;

   if (spec->RenderContour) {
      //Check if the contours are compatible between the fields
      if (nf>1) {
         for(f=1;f<nf;f++) {
            // Make sure we have the same number of intervals
            if (spec->InterNb != field[f]->Spec->InterNb) {
               Tcl_AppendResult(Interp,"OGR_LayerImport: Cannot import multiple field contours in a single layer if the number of intervals vary",(char*)NULL);
               return(TCL_ERROR);
            }
            // Make sure we have the same intervals
            for(idx=0;idx<spec->InterNb;++idx) {
               if (spec->Inter[idx] != field[f]->Spec->Inter[idx]) {
                  Tcl_AppendResult(Interp,"OGR_LayerImport: Cannot import multiple field contours in a single layer if the intervals vary",(char*)NULL);
                  return(TCL_ERROR);
               }
            }
         }
      }

      // Check for how many features
      Layer->NFeature=0;
      for(f=0;f<nf;f++) {
         strtrim(field[f]->Spec->Desc,' ');
         Layer->NFeature+=field[f]->Spec->InterNb;
      }
      Layer->Feature=realloc(Layer->Feature,Layer->NFeature*sizeof(OGRFeatureH));

      OGR_FieldCreate(Layer,"Desc","String",32,0);
      OGR_FieldCreate(Layer,"Interval","Real",32,32);
      OGR_FieldCreate(Layer,"Height","Real",16,16);
      OGR_FieldCreate(Layer,"HeightUnit","String",32,0);
      OGR_FieldCreate(Layer,"DateTime","DateTime",0,0);

      for(f=0,n=0;f<nf;f++) {
         System_StampDecode(((TRPNHeader*)field[f]->Head)->DATEV,&yyyy,&mm,&dd,&h,&m,&s);

         for(idx=0;idx<spec->InterNb;idx++,++n) {
            Layer->Feature[n]=OGR_F_Create(Layer->Def);
            OGR_F_SetFieldString(Layer->Feature[n],0,spec->Desc);
            OGR_F_SetFieldDouble(Layer->Feature[n],1,VAL2SPEC(spec,spec->Inter[idx]));
            OGR_F_SetFieldDouble(Layer->Feature[n],2,field[f]->ZRef->Levels[0]);
            OGR_F_SetFieldString(Layer->Feature[n],3,(char*)ZRef_LevelNames()[field[f]->ZRef->Type]);
            OGR_F_SetFieldDateTime(Layer->Feature[n],4,yyyy,mm,dd,h,m,s,100);

            if (spec->MapAll && spec->Map) {
               VAL2COL(cidx,spec,spec->Inter[idx]);
               snprintf(style,sizeof(style),"PEN(c:#%02x%02x%02x%02x,w:%ipx)",spec->Map->Color[cidx][0],spec->Map->Color[cidx][1],spec->Map->Color[cidx][2],spec->Map->Color[cidx][3],spec->Width);
               OGR_F_SetStyleString(Layer->Feature[n],style);
            } else if (spec->Outline) {
               snprintf(style,sizeof(style),"PEN(c:#%02x%02x%02x%02x,w:%ipx)",spec->Outline->red,spec->Outline->green,spec->Outline->blue,255,spec->Width);
               OGR_F_SetStyleString(Layer->Feature[n],style);
            }

            Data_Clean(field[f],0,0,1);
            FFContour(Grid?REF_GRID:REF_COOR,field[f]->GPos,field[f]->Def,field[f]->Stat,NULL,1,&spec->Inter[idx],3,spec->RenderTexture?1:0);
            cont=OGR_G_CreateGeometry(spec->RenderTexture?wkbPolygon:wkbMultiLineString);

            list=field[f]->Def->Segments;
            while(list) {
               array=(T3DArray*)list->Data;

               geom=OGR_G_CreateGeometry(spec->RenderTexture?wkbLinearRing:wkbLineString);
               for(k=0;k<array->Size;k++) {
                  Layer->GRef->UnProject(Layer->GRef,&x,&y,array->Data[k][1],array->Data[k][0],1,1);
                  OGR_G_AddPoint_2D(geom,SIDELON(Side,x),y);
               }
               OGR_G_AddGeometryDirectly(cont,geom);
               list=list->Next;
            }
            OGR_F_SetGeometryDirectly(Layer->Feature[n],cont);
         }
      }
   } else {
      int ndates=nf,ndescs=nf;
      int dates[nf],fldidx[nf];
      char *descs[nf],*ndpd=NULL,*desc;

      // If we have multiple fields
      if (nf>1) {
         // Check if we have more than one date or desription
         for(f=0;f<nf;++f) {
            dates[f] = ((TRPNHeader*)field[f]->Head)->DATEV;

            strtrim(field[f]->Spec->Desc,' ');
            descs[f] = (desc=field[f]->Spec->Desc) ? desc : ((desc=((TRPNHeader*)field[f]->Head)->NOMVAR) ? desc : " ");
         }

         // Sort-unique desc and dates to get the real number of unique desc and dates
         qsort(dates,ndates,sizeof(*dates),QSort_Int);
         qsort(descs,ndescs,sizeof(*descs),(QSort_Fn)strcmp);

         Unique(dates,&ndates,sizeof(*dates));
         for(f=1,ndescs=1;f<nf;++f) {
            if (strcmp(descs[f-1],descs[f])) {
               if (f!=ndescs) {
                  descs[ndescs]=descs[f];
               }
               ++ndescs;
            }
         }

         // Count the number of desc per date to see if we need a tie breaker
         if (ndescs!=nf && ndates!=nf) {
            ndpd=calloc(ndescs*ndates,sizeof(*ndpd));
            for(f=0;f<nf;++f) {
               // Find the index of desc in the descs array
               desc = (desc=field[f]->Spec->Desc) ? desc : ((desc=((TRPNHeader*)field[f]->Head)->NOMVAR) ? desc : " ");
               fldidx[f] = d = ndescs==1 ? 0 : (int)((char**)bsearch(&desc,descs,ndescs,sizeof(*descs),QSort_StrPtr)-(char**)descs);
               // Check which flag we need to check/adjust (vector or not)
               i = 1<<(field[f]->Spec->RenderVector!=0 && field[f]->Def->Data[1]!=NULL);

               // Check if we don't already have the flag for dedup set
               n = ndates==1 ? 0 : (int)((int*)bsearch(&((TRPNHeader*)field[f]->Head)->DATEV,dates,ndates,sizeof(*dates),QSort_Int)-(int*)dates);
               if (ndpd[d*ndates+n]&i) {
                  // Flag this item as needing a level for a tiebreaker for the right vect component
                  ndpd[d*ndates] |= (i<<2);
                  // Set the flag for that desc/date/vect combo
                  ndpd[d*ndates+n] |= i;
               }
            }
         }
      }

      //----- Create the feature fields

      // Create the date field if needed
      if (ndates>1) {
         OGR_FieldCreate(Layer,"DateTime","DateTime",0,0);
      }

      // Create the desc fields
      for(f=0,j=(ndates>1);f<nf;f++) {
         // Get the vect flag
         i = (field[f]->Spec->RenderVector!=0 && field[f]->Def->Data[1]!=NULL);

         // Build the field name
         n = sizeof(buf)-1;
         idx = snprintf(buf,n,"%s",(desc=field[f]->Spec->Desc) ? desc : ((desc=((TRPNHeader*)field[f]->Head)->NOMVAR) ? desc : " "));
         // Add the level if that desc is duplicated at any date
         if (n>idx && ndpd && ndpd[fldidx[f]*ndates]&(1<<(2+i))) {
            idx += snprintf(buf+idx,n-idx,"_%.4f%s",field[f]->ZRef->Levels[0],ZRef_LevelNames()[field[f]->ZRef->Type]);
         }
         // Add to field for the vector components (speed and direction) if needed
         if (i && n>idx) {
            idx += snprintf(buf+idx,n-idx," (spd)");
         }
         if (idx>n) {
            Tcl_AppendResult(Interp,"OGR_LayerImport: field name too long: ",buf,(char*)NULL);
            free(ndpd);
            return(TCL_ERROR);
         }

         // Add the field if it doesn't already exists
         if ((fldidx[f]=OGR_FD_GetFieldIndex(OGR_L_GetLayerDefn(Layer->Layer),buf)) == -1) {
            // Create the field
            if (i) {
               if (!OGR_FieldCreate(Layer,buf,"Real",16,16)) {
                  Tcl_AppendResult(Interp,"OGR_LayerImport: Couldn't create field ",buf,(char*)NULL);
                  free(ndpd);
                  return(TCL_ERROR);
               }
               if (n>idx) {
                  sprintf(buf+(idx-4),"dir)");
                  if (OGR_FieldCreate(Layer,buf,"Real",16,16)) {
                     Tcl_AppendResult(Interp,"OGR_LayerImport: Couldn't create field ",buf,(char*)NULL);
                     free(ndpd);
                     return(TCL_ERROR);
                  }
               }
            } else {
               if (!OGR_FieldCreate(Layer,buf,"Real",32,32)) {
                  Tcl_AppendResult(Interp,"OGR_LayerImport: Couldn't create field ",buf,(char*)NULL);
                  free(ndpd);
                  return(TCL_ERROR);
               }
            }
            // Save the field position
            fldidx[f] = j;
            j += 1+i;
         }
      }
      free(ndpd);

      /*Build a mask of valid cells*/
      if (!(mask=(char*)calloc(FSIZE2D(field[0]->Def),sizeof(char)))) {
         Tcl_AppendResult(Interp,"OGR_LayerImport: Unable to allocate temporary buffer",(char*)NULL);
         return(TCL_ERROR);
      }
      Layer->NFeature=0;

      for(f=0;f<nf;f++) {
         for(i=0;i<FSIZE2D(field[f]->Def);i++) {
            if (!mask[i]) {
               Def_GetMod(field[f]->Def,i,spd);
               mask[i]=Data_Within(field[f],spd);
               if (mask[i]) Layer->NFeature++;
            }
         }
      }

      // Adjust the number of features based on the amount of date fields that we have
      Layer->NFeature *= ndates;

      if (!(Layer->Feature=realloc(Layer->Feature,Layer->NFeature*sizeof(OGRFeatureH)))) {
         Tcl_AppendResult(Interp,"OGR_LayerImport: Unable to allocate feature buffer",(char*)NULL);
         free(mask);
         return(TCL_ERROR);
      }

      n=0;
      for(i=0;i<field[0]->Def->NI;i++) {
         for(j=0;j<field[0]->Def->NJ;j++) {
            idx=j*field[0]->Def->NI+i;
            if (mask[idx]) {
               for(d=0;d<ndates;++d)
                  Layer->Feature[n+d]=OGR_F_Create(Layer->Def);

               // Set the date for the features
               if( ndates>1 ) {
                  for(d=0;d<ndates;++d) {
                     System_StampDecode(dates[d],&yyyy,&mm,&dd,&h,&m,&s);
                     OGR_F_SetFieldDateTime(Layer->Feature[n+d],0,yyyy,mm,dd,h,m,s,100);
                  }
               }

               for(f=0;f<nf;f++) {
                  // Get the date for the feature
                  d = ndates==1 ? 0 : (int)((int*)bsearch(&((TRPNHeader*)field[f]->Head)->DATEV,dates,ndates,sizeof(*dates),QSort_Int)-(int*)dates);

                  // Add the feature field's value
                  if (field[f]->Spec->RenderVector && field[f]->Def->Data[1]) {
                     field[f]->GRef->Value(field[f]->GRef,field[f]->Def,field[f]->Spec->InterpDegree[0],0,i,j,0,&spd,&dir);
                     OGR_F_SetFieldDouble(Layer->Feature[n+d],fldidx[f],VAL2SPEC(field[f]->Spec,spd));
                     OGR_F_SetFieldDouble(Layer->Feature[n+d],fldidx[f]+1,dir);
                  } else {
                     Def_GetMod(field[f]->Def,idx,spd);
                     OGR_F_SetFieldDouble(Layer->Feature[n+d],fldidx[f],VAL2SPEC(field[f]->Spec,spd));
                  }
               }

               if (spec->Map) {
                  VAL2COL(cidx,spec,spd);
               }

               if (spec->RenderParticle) {
                  if (cidx>-1) {
                     snprintf(style,sizeof(style),"PEN(c:#%02x%02x%02x%02x,w:%ipx)",spec->Map->Color[cidx][0],spec->Map->Color[cidx][1],spec->Map->Color[cidx][2],spec->Map->Color[cidx][3],spec->RenderParticle);
                  } else if (spec->Outline) {
                     snprintf(style,sizeof(style),"PEN(c:#%02x%02x%02x%02x,w:%ipx)",spec->Outline->red,spec->Outline->green,spec->Outline->blue,255,spec->RenderParticle);
                  }
                  OGR_F_SetStyleString(Layer->Feature[n],style);

                  geom=OGR_G_CreateGeometry(wkbPoint25D);
                  if (Grid) {
                     OGR_G_AddPoint(geom,i,j,(field[0]->GRef->Hgt?field[0]->GRef->Hgt[idx]:0.0));
                  } else {
                     Layer->GRef->UnProject(Layer->GRef,&x,&y,field[0]->GRef->AY[idx],field[0]->GRef->AX[idx],1,1);
                     OGR_G_AddPoint(geom,SIDELON(Side,x),y,(field[0]->GRef->Hgt?field[0]->GRef->Hgt[idx]:0.0));
                  }
               } else if (spec->RenderTexture) {
                  if (cidx>-1) {
                     snprintf(style,sizeof(style),"BRUSH(fc:#%02x%02x%02x%02x);PEN(c:#%02x%02x%02x%02x,w:%ipx)",spec->Map->Color[cidx][0],spec->Map->Color[cidx][1],spec->Map->Color[cidx][2],spec->Map->Color[cidx][3],spec->Map->Color[cidx][0],spec->Map->Color[cidx][1],spec->Map->Color[cidx][2],spec->Map->Color[cidx][3],1);
                     OGR_F_SetStyleString(Layer->Feature[n],style);
                  }

                  geom=OGR_G_CreateGeometry(wkbPolygon);
                  poly=OGR_G_CreateGeometry(wkbLinearRing);
                  if (Grid) {
                     OGR_G_AddPoint_2D(poly,i-0.5,j-0.5);
                     OGR_G_AddPoint_2D(poly,i-0.5,j+0.5);
                     OGR_G_AddPoint_2D(poly,i+0.5,j+0.5);
                     OGR_G_AddPoint_2D(poly,i+0.5,j-0.5);
                     OGR_G_AddPoint_2D(poly,i-0.5,j-0.5);
                  } else {
                     field[0]->GRef->Project(field[0]->GRef,i-0.5,j-0.5,&lat,&lon,1,1);
                     Layer->GRef->UnProject(Layer->GRef,&x,&y,CLAMPLAT(lat),lon,1,1);
                     OGR_G_AddPoint_2D(poly,SIDELON(Side,x),y);
                     field[0]->GRef->Project(field[0]->GRef,i-0.5,j+0.5,&lat,&lon,1,1);
                     Layer->GRef->UnProject(Layer->GRef,&x,&y,CLAMPLAT(lat),lon,1,1);
                     OGR_G_AddPoint_2D(poly,SIDELON(Side,x),y);
                     field[0]->GRef->Project(field[0]->GRef,i+0.5,j+0.5,&lat,&lon,1,1);
                     Layer->GRef->UnProject(Layer->GRef,&x,&y,CLAMPLAT(lat),lon,1,1);
                     OGR_G_AddPoint_2D(poly,SIDELON(Side,x),y);
                     field[0]->GRef->Project(field[0]->GRef,i+0.5,j-0.5,&lat,&lon,1,1);
                     Layer->GRef->UnProject(Layer->GRef,&x,&y,CLAMPLAT(lat),lon,1,1);
                     OGR_G_AddPoint_2D(poly,SIDELON(Side,x),y);
                     field[0]->GRef->Project(field[0]->GRef,i-0.5,j-0.5,&lat,&lon,1,1);
                     Layer->GRef->UnProject(Layer->GRef,&x,&y,CLAMPLAT(lat),lon,1,1);
                     OGR_G_AddPoint_2D(poly,SIDELON(Side,x),y);
                  }
                  OGR_G_AddGeometryDirectly(geom,poly);
               } else {
                  if (spec->Map && spec->MapAll && cidx>-1) {
                     snprintf(style,sizeof(style),"PEN(c:#%02x%02x%02x%02x,w:%ipx)",spec->Map->Color[cidx][0],spec->Map->Color[cidx][1],spec->Map->Color[cidx][2],spec->Map->Color[cidx][3],spec->RenderGrid);
                  } else if (spec->Outline) {
                     snprintf(style,sizeof(style),"PEN(c:#%02x%02x%02x%02x,w:%ipx)",spec->Outline->red,spec->Outline->green,spec->Outline->blue,255,spec->RenderGrid);
                  }
                  OGR_F_SetStyleString(Layer->Feature[n],style);
                  geom=OGR_G_CreateGeometry(wkbPoint);

                  if (Grid) {
                     OGR_G_AddPoint_2D(geom,i,j);
                  } else {
                     field[0]->GRef->Project(field[0]->GRef,i,j,&lat,&lon,1,1);
                     Layer->GRef->UnProject(Layer->GRef,&x,&y,lat,lon,1,1);
                     OGR_G_AddPoint_2D(geom,SIDELON(Side,x),y);
                  }
               }

               // If we have more than one feature linked to this geom/style
               if (ndates>1) {
                  const char *sl=OGR_F_GetStyleString(Layer->Feature[n]);

                  for(d=1;d<ndates;++d) {
                     // Copy the style
                     if (sl)
                        OGR_F_SetStyleString(Layer->Feature[n+d],sl);

                     // Copy the geometry
                     OGR_F_SetGeometry(Layer->Feature[n+d],geom);
                  }
               }

               // Add the geometry (and its ownership) to the first feature
               OGR_F_SetGeometryDirectly(Layer->Feature[n],geom);

               n += ndates;
            }
         }
      }
      free(mask);
   }

   // Add the features to the layer
   for(n=0; n<Layer->NFeature; ++n) {
      if (OGR_L_CreateFeature(Layer->Layer,Layer->Feature[n])!=OGRERR_NONE) {
         Tcl_AppendResult(Interp,"\n   OGR_LayerImport: Problems creating feature",(char*)NULL);
         return(TCL_ERROR);
      }
   }

   Layer->Changed=1;

   if (!(Layer->Select=malloc(Layer->NFeature*sizeof(char)))) {
      Tcl_AppendResult(Interp,"OGR_LayerImport: Unable to allocate feature buffer",(char*)NULL);
      return(TCL_ERROR);
   }
   memset(Layer->Select,0x1,Layer->NFeature);

   if (!(Layer->Loc=(Coord*)malloc(Layer->NFeature*sizeof(Coord)))) {
      Tcl_AppendResult(Interp,"OGR_LayerImport: Unable to allocate location buffer",(char*)NULL);
      return(TCL_ERROR);
   }

   return(TCL_OK);
}

/*--------------------------------------------------------------------------------------------------------------
 * Nom          : <OGR_LayerClear>
 * Creation     : Fevrier 2008 J.P. Gauthier - CMC/CMOE
 *
 * But          : Reinitialiser un champs pour toutes les features
 *
 * Parametres   :
 *   <Interp>   : L'interpreteur Tcl
 *   <Layer>    : Couche
 *   <Field>    : Champs
 *
 * Retour       : Code d'erreur standard TCL
 *
 * Remarques    :
 *
 *---------------------------------------------------------------------------------------------------------------
*/
int OGR_LayerClear(Tcl_Interp *Interp,OGR_Layer *Layer,int Field,double Value) {

   unsigned int f;

   if (!Layer) {
      Tcl_AppendResult(Interp,"OGR_LayerClear: Invalid layer",(char*)NULL);
      return(TCL_ERROR);
   }

   if (Field<0 || Field>=OGR_FD_GetFieldCount(Layer->Def)) {
      Tcl_AppendResult(Interp,"OGR_LayerClear: Invalid Field",(char*)NULL);
      return(TCL_ERROR);
   }

   for(f=0;f<Layer->NFeature;f++) {
      if (Layer->Select[f] && Layer->Feature[f]) {
         OGR_F_SetFieldDouble(Layer->Feature[f],Field,Value);
         Layer->Update=1;
         Layer->Changed=1;
      }
   }
   return(TCL_OK);
}

/*--------------------------------------------------------------------------------------------------------------
 * Nom          : <OGR_LayerInterp>
 * Creation     : Fevrier 2008 J.P. Gauthier - CMC/CMOE
 *
 * But          : Importer des donnees dans une couche OGR
 *
 * Parametres   :
 *   <Interp>   : L'interpreteur Tcl
 *   <Layer>    : Couche
 *   <Field>    : Champs
 *
 * Retour       : Code d'erreur standard TCL
 *
 * Remarques    :
 *
 *---------------------------------------------------------------------------------------------------------------
*/
int OGR_LayerInterp(Tcl_Interp *Interp,OGR_Layer *Layer,int Field,TGeoRef *FromRef,TDef *FromDef,char Mode,int Final,int Prec,float *Index) {

   int           i,j,n=0,nt=0,rw=0;
   unsigned int  f;
   double        val0,val1,area,*accum=NULL,r,rt,dp;
   OGRGeometryH  cell,ring,inter,geom;
   OGREnvelope   env0,*env1=NULL;
   Vect3d        vr;
   Coord         co;
   float       *ip=NULL;

   if (!Layer) {
      Tcl_AppendResult(Interp,"OGR_LayerInterp: Invalid layer",(char*)NULL);
      return(0);
   }

   if (Field<0 || Field>=OGR_FD_GetFieldCount(Layer->Def)) {
      Tcl_AppendResult(Interp,"OGR_LayerInterp: Invalid Field",(char*)NULL);
      return(0);
   }

   cell=OGR_G_CreateGeometry(wkbPolygon);
   ring=OGR_G_CreateGeometry(wkbLinearRing);
   OGR_G_AddGeometryDirectly(cell,ring);

   if (Mode=='N' || Mode=='A') {
      accum=(double*)calloc(Layer->NFeature,sizeof(double));
      if (!accum) {
         Tcl_AppendResult(Interp,"OGR_LayerInterp: Unable to allocate accumulation buffer",(char*)NULL);
         return(0);
      }
   }

   // Do we have and index
   if (Index && Index[0]!=DEF_INDEX_EMPTY) {
      // As long as the file or the list is not empty
      ip=Index;
      while(*ip!=DEF_INDEX_END) {

         // Get the gridpoint
         i=*(ip++);
         j=*(ip++);
         
         if (!FIN2D(FromDef,i,j)) {
            Tcl_AppendResult(Interp,"OGR_LayerInterp: Wrong index, index coordinates are greater than field size",(char*)NULL);
            return(0);
         }
         
         // Get this gridpoint value
         Def_Get(FromDef,0,FIDX2D(FromDef,i,j),val1);

         // Get the geometry intersections
         while(*ip!=DEF_INDEX_SEPARATOR) {
            f=*(ip++);
            area=*(ip++);

            // Check for nodata value
            if (DEFVALID(FromDef,val1) && Layer->Feature[f]) {
               val0=OGR_F_GetFieldAsDouble(Layer->Feature[f],Field);

               switch(Mode) {
                  case 'N': accum[f]+=area;
                  case 'C': val0+=val1*area;
                           break;
                  case 'W': if (area>0.999) {
                              val0+=val1;
                           }
                           break;
                  case 'A': accum[f]+=1.0;
                  case 'I': val0+=val1;
                           break;
               }
               OGR_F_SetFieldDouble(Layer->Feature[f],Field,val0);
            }
         }
         // Skip separator
         ip++;
      } 
   } else {

      // Damn, we dont have the index
      if (Index && Index[0]==DEF_INDEX_EMPTY) {
         ip=Index;
      }

      // Allocate enveloppe array to speedup intersection test
      if (!(env1=(OGREnvelope*)calloc(Layer->NFeature,sizeof(OGREnvelope)))) {
         Tcl_AppendResult(Interp,"OGR_LayerInterp: Unable to allocate envelope buffer",(char*)NULL);
         return(0);
      }

      // Loop on gridpoints
      for(j=0;j<FromDef->NJ;j++) {
         for(i=0;i<FromDef->NI;i++) {
            
            // Tranform gridpoint into OGR quad projected into layer's referential
            if (!Def_GridCell2OGR(ring,Layer->GRef,FromRef,i,j,Prec)) {
               continue;
            }
            rt=n=rw=0;
            
            // If this cell's area is valid
            if ((area=OGR_G_Area(cell))>0.0) {
               Def_Get(FromDef,0,FIDX2D(FromDef,i,j),val1);
               OGR_G_GetEnvelope(ring,&env0);

               // Append gridpoint to the index
               if (ip) {
                  *(ip++)=i;
                  *(ip++)=j;
               }

               // Check which feature intersects with the cell
               for(f=0;f<Layer->NFeature;f++) {
                  if (Layer->Feature[f]) {
                     geom=OGR_F_GetGeometryRef(Layer->Feature[f]);
                     r=0.0;

                     // If it's a point, do simple interpolation 
                     if (wkbFlatten(OGR_G_GetGeometryType(geom))==wkbPoint) {
                        if (Mode!='N' && Mode!='L') {
                           Tcl_AppendResult(Interp,"OGR_LayerInterp: Invalid interpolation method, must be  NEAREST or LINEAR",(char*)NULL);
                           return(0);
                        }

                        OGR_G_GetPoint(geom,n,&vr[0],&vr[1],&vr[2]);
                        Layer->GRef->Project(Layer->GRef,vr[0],vr[1],&co.Lat,&co.Lon,1,0);
                        FromRef->UnProject(FromRef,&vr[0],&vr[1],co.Lat,co.Lon,1,1);
                        FromRef->Value(FromRef,FromDef,Mode,0,vr[0],vr[1],FromDef->Level,&val0,&val1);
                        OGR_F_SetFieldDouble(Layer->Feature[f],Field,val0);
                        rw++;
                        nt++;
                     } else {

                        // If this feature envelope is not yet calculated
                        if (env1[f].MinX==0 && env1[f].MaxX==0 && env1[f].MinY==0 && env1[f].MaxY==0)
                           OGR_G_GetEnvelope(geom,&env1[f]);

                        // Do the grid cell intersect this feature
                        if (OGM_Intersect(cell,geom,&env0,&env1[f])) {
                           
                           // Get fraction of cell in intersection
                           inter=OGM_GPCOnOGR(GPC_INT,cell,geom);
                           dp=OGR_G_Area(inter);
                           r=dp/area;
                           rt+=r;
                           nt++;
                           
                           if (DEFVALID(FromDef,val1)) {
                              val0=OGR_F_GetFieldAsDouble(Layer->Feature[f],Field);
                              switch(Mode) {
                                 case 'N': accum[f]+=r;
                                 case 'C': val0+=val1*r;
                                          break;
                                 case 'W': if (r>0.999) {
                                             val0+=val1;
                                          } else {
                                             r=0.0;
                                          }
                                          break;
                                 case 'A': accum[f]+=1.0;
                                 case 'I': val0+=val1;
                                          break;
                                 default:
                                    Tcl_AppendResult(Interp,"OGR_LayerInterp: Invalid interpolation method, must be  WITHIN, INTERSECT, AVERAGE, CONSERVATIVE or NORMALIZED_CONSERVATIVE",(char*)NULL);
                                    return(0);
                              }
                              OGR_F_SetFieldDouble(Layer->Feature[f],Field,val0);
                           }
                        }

                        // Append intersection info to the list
                        if (ip && r>0.0) {
                           n++;
                           *(ip++)=f;
                           *(ip++)=r;
                        }

                        // If this model gridcell is fully distributed, select next cell
                        if (rt>0.999) {
                           break;
                        }
                     }
                  }
               }

               // Append this gridpoint intersections to the index
               if (ip) {
                  if (n) {
                     *(ip++)=DEF_INDEX_SEPARATOR; // End the list for this gridpoint
                  } else {
                     ip-=2;                       // No intersection found, removed previously inserted gridpoint
                  }
               }
            }
            if (rw==Layer->NFeature) break;
         }
         if (rw==Layer->NFeature) break;
      }
      if (ip) *(ip++)=DEF_INDEX_END;
      free(env1);
  }

   // Finalize and reassign
   if (Final && (Mode=='N' || Mode=='A')) {
      for(f=0;f<Layer->NFeature;f++) {
         if (Layer->Feature[f]) {
            val0=OGR_F_GetFieldAsDouble(Layer->Feature[f],Field);
            if (accum[f]!=0.0) {
               val0/=accum[f];
               OGR_F_SetFieldDouble(Layer->Feature[f],Field,val0);
            }
         }
      }
   }

   OGR_G_DestroyGeometry(cell);

   if (accum)
      free(accum);

   Layer->Update=1;
   Layer->Changed=1;

   // Return size of index or number of hits, or 1 if nothing found
   nt=Index?(ip-Index)+1:nt;
   return(nt==0?1:nt);
}

/*----------------------------------------------------------------------------
 * Nom      : <OGR_LayerPreInit>
 * Creation : Juillet 2006 - J.P. Gauthier - CMC/CMOE
 *
 * But      : Effectue la pre-initialisation des parametres avant le rendue.
 *
 * Parametres :
 *   <Layer>  : Couche a afficher
 *
 * Retour:
 *
 * Remarques :
 *
 *----------------------------------------------------------------------------
*/
void OGR_LayerPreInit(OGR_Layer *Layer) {

   /*Assigner les limites d'affichage*/
   if (!(Layer->Spec->MinMax&DATASPEC_MINSET)) Layer->Spec->Min=Layer->Min;
   if (!(Layer->Spec->MinMax&DATASPEC_MAXSET)) Layer->Spec->Max=Layer->Max;

   if (Layer->Spec->InterMode) {
      DataSpec_Intervals(Layer->Spec,Layer->Spec->Min,Layer->Spec->Max);
   }
   DataSpec_Define(Layer->Spec);
}

/*--------------------------------------------------------------------------------------------------------------
 * Nom          : <OGR_Layer>
 * Creation     : August 2010 J.P. Gauthier - CMC/CMOE
 *
 * But          : Effecture les reprojections de la geometrie de la couche.
 *
 * Parametres   :
 *   <Layer>    : Couche a afficher
 *   <Proj>     : La projection courante
 *   <Delay>    : Mode refresh interactif
 *
 * Retour       :
 *   <Nb>       : Nombre de feature reprojetees
 *
 * Remarques    :
 *    - En activant le delay, on active un refresh du viewport apres 1/4 de secondes
 *---------------------------------------------------------------------------------------------------------------
*/

int OGR_LayerParseBuild(OGR_Layer *Layer,Projection *Proj,int Index) {
   
   Vect3d        vr;
   double        elev=0.0,extr=0.0;
   OGRGeometryH  geom;

   glNewList(Layer->LFeature+Index,GL_COMPILE);
   if (Layer->Feature[Index]) {
      if ((geom=OGR_F_GetGeometryRef(Layer->Feature[Index]))) {
         if (Layer->Topo>0)
            elev=OGR_F_GetFieldAsDouble(Layer->Feature[Index],Layer->Topo-1);

         if (Layer->Extrude!=-1)
            extr=OGR_F_GetFieldAsDouble(Layer->Feature[Index],Layer->Extrude);

         OGR_GeometryRender(Proj,Layer->GRef,Layer,geom,elev*Layer->Spec->TopoFactor,extr*Layer->Spec->ExtrudeFactor);
         OGM_Centroid2D(geom,&vr[0],&vr[1]);
         Layer->GRef->Project(Layer->GRef,vr[0],vr[1],&Layer->Loc[Index].Lat,&Layer->Loc[Index].Lon,1,1);
         Layer->Loc[Index].Elev=extr*Layer->Spec->ExtrudeFactor;
      }
   }
   glEndList();
   
   return(1);
}

int OGR_LayerParse(OGR_Layer *Layer,Projection *Proj,int Delay) {

   Coord        co[2];
   int          t;
   unsigned int f;
   clock_t      sec;

   if (Layer->GFeature==Layer->NFeature) {
      return(0);
   }

   t=Layer->GFeature;
   sec=clock();
   
   /*Generate the display lists*/
   for(f=Layer->GFeature;f<Layer->NFeature;f++) {

      OGR_LayerParseBuild(Layer,Proj,f);      
      Layer->GFeature++;

      /*Refrech viewport if it's been too long*/
      if (Delay && (clock()-sec)>(0.25*CLOCKS_PER_SEC)) {
         Proj->Loading=(double)(Layer->GFeature*100.0/Layer->NFeature);
         Proj->Loading=Proj->Loading<=0?1:Proj->Loading;
         Tcl_CreateTimerHandler(0,ViewportRefresh_Canvas,Proj->VP->canvas);
         break;
      }
   }

   /*Make sure we reset the loading flag*/
   if (Layer->GFeature==Layer->NFeature) {
      co[0].Lat=Layer->GRef->LLExtent.MinY;
      co[0].Lon=Layer->GRef->LLExtent.MinX;
      co[1].Lat=Layer->GRef->LLExtent.MaxY;
      co[1].Lon=Layer->GRef->LLExtent.MaxX;

      Proj->Type->Project(Proj,(GeoVect*)co,(GeoVect*)Layer->Vr,2);
      t=0;
      Proj->Loading=0;
      if (Delay)
         Tcl_CreateTimerHandler(0,ViewportRefresh_Canvas,Proj->VP->canvas);
   }
   return(t);
}

/*--------------------------------------------------------------------------------------------------------------
 * Nom          : <OGR_LayerRender>
 * Creation     : Juin 2004 J.P. Gauthier - CMC/CMOE
 *
 * But          : Rendu de la bande raster.
 *
 * Parametres  :
 *   <Interp>   : Interpreteur Tcl
 *   <Proj>     : La projection courante
 *   <VP>       : Le viewport ou le rendu doit etre fait
 *   <Layer>    : Couche a afficher
 *
 * Retour       :
 *
 * Remarques    :
 *
 *---------------------------------------------------------------------------------------------------------------
*/
int OGR_LayerRender(Tcl_Interp *Interp,Projection *Proj,ViewportItem *VP,OGR_Layer *Layer,int Mask) {

   int     f,idx=-1,x,y,g,nf,dx,w;
   int     fsize=-1,fmap=-1,flabel=-1;
   Vect3d  vr;
   char    lbl[256];
   double  val,sz,szon=0,alpha;

   OGRFieldDefnH field;
   TDataSpec     *spec=Layer->Spec;

   extern TIcon IconList[];

   if (!Layer || !spec) {
      App_Log(ERROR,"%s: Invalid layer object\n",__func__);
      return(0);
   }

   if (!Layer->NFeature) {
      return(0);
   }

   if (!spec->Active) {
      return(0);
   }

   /*Check for invalid georeference*/
   if (!GeoRef_Valid(Layer->GRef)) {
      App_Log(ERROR,"%s: Invalid georeference\n",__func__);
      return(0);
   }

   if (!Layer->LFeature) {
      Layer->LFeature=glGenLists(Layer->NFeature);
      if (!Layer->LFeature) {
         App_Log(ERROR,"%s: Unable to allocate display list\n",__func__);
         return(0);
      }
   }

   /*Check for topography value*/
   Layer->Topo=-1;
   if (spec->Topo) {
      if (strcmp(spec->Topo,"NONE")==0) {
         Layer->Topo=-1;
      } else if (strcmp(spec->Topo,"INTERNAL")==0) {
         Layer->Topo=0;
      } else {
         Layer->Topo=OGR_FD_GetFieldIndex(Layer->Def,spec->Topo)+1;
      }
   }

   /*Check for extrude value*/
   Layer->Extrude=-1;
   if (spec->Extrude) {
      if (strcmp(spec->Extrude,"NONE")==0) {
         Layer->Extrude=-1;
      } else {
         Layer->Extrude=OGR_FD_GetFieldIndex(Layer->Def,spec->Extrude);
      }
   }

   /*Read in data in another thread*/
   g=OGR_LayerParse(Layer,Proj,(Mask || GLRender->Wait || GLRender->XBatch || GLRender->TRCon)?0:1);

   // A feature might need refresh
   if (Layer->CFeature!=-1 && Layer->CFeature<Layer->GFeature) {
      OGR_LayerParseBuild(Layer,Proj,Layer->CFeature);
      Layer->CFeature=-1;
   }
  
   glDash(&spec->Dash);
   glEnable(GL_BLEND);
   glEnable(GL_STENCIL_TEST);
   if (!Mask)
      glStencilMask(0xff);

   if (spec->Icon) {
      glEnableClientState(GL_VERTEX_ARRAY);
      glVertexPointer(2,GL_DOUBLE,0,IconList[spec->Icon].Co);

      /*Check for icon size value*/
      if (spec->SizeVar) {
         if ((fsize=OGR_FD_GetFieldIndex(Layer->Def,spec->SizeVar))>-1) {
            spec->SizeMin=1e32;
            spec->SizeMax=-1e32;

            for(nf=0;nf<Layer->NFeature;nf++) {
               if (Layer->Feature[nf]) {
                  val=OGR_F_GetFieldAsDouble(Layer->Feature[nf],fsize);
                  spec->SizeMin=fmin(spec->SizeMin,val);
                  spec->SizeMax=fmax(spec->SizeMax,val);
               }
            }
         }
      }
   }
   sz=VP->Ratio*(spec->Size+spec->Width);

   /*Check for color mapping value*/
   if (spec->MapVar) {
      if ((fmap=OGR_FD_GetFieldIndex(Layer->Def,spec->MapVar))>-1) {
         Layer->Min=1e32;
         Layer->Max=-1e32;

         for(nf=0;nf<Layer->NFeature;nf++) {
            if (Layer->Feature[nf]) {
               val=OGR_F_GetFieldAsDouble(Layer->Feature[nf],fmap);
               Layer->Min=fmin(Layer->Min,val);
               Layer->Max=fmax(Layer->Max,val);
            }
         }
      }
   }

   /*Check for label value*/
   if (spec->LabelVar) {
      flabel=OGR_FD_GetFieldIndex(Layer->Def,spec->LabelVar);
   }

   OGR_LayerPreInit(Layer);

   if (!Mask) {
      if (Layer->Mask) {
         if (Proj->Geo->Params.Mask==0) {
            glMatrixMode(GL_PROJECTION);
            glPushMatrix();
            glLoadIdentity();
            gluOrtho2D(0,VP->Width,0,VP->Height);

            glMatrixMode(GL_MODELVIEW);
            glPushMatrix();
            glLoadIdentity();

            glStencilFunc(GL_EQUAL,0x2,0x01);
            glStencilOp(GL_KEEP,GL_REPLACE,GL_REPLACE);
            glDisable(GL_DEPTH_TEST);

            glColorMask(GL_FALSE,GL_FALSE,GL_FALSE,GL_FALSE);
            ViewportClear(VP,0);
            glColorMask(GL_TRUE,GL_TRUE,GL_TRUE,GL_TRUE);

            glPopMatrix();
            glMatrixMode(GL_PROJECTION);
            glPopMatrix();
         }
         glStencilFunc(GL_EQUAL,0x2,0xff);
         glStencilOp(GL_KEEP,GL_ZERO,GL_ZERO);
      } else {
         glStencilFunc(GL_EQUAL,0x0,0xff);
         glStencilOp(GL_KEEP,GL_KEEP,GL_KEEP);
      }
   }

   glPolygonMode(GL_FRONT_AND_BACK,GL_FILL);

   /*Check for extrusion selection*/
   if (GLRender->GLZBuf || Layer->Space==3 || Layer->Extrude!=-1 || Layer->Topo!=-1) {
      glEnable(GL_DEPTH_TEST);
      glDepthFunc(GL_LESS);
      if (spec->Fill) {
         glEnable(GL_POLYGON_OFFSET_FILL);
         glPolygonOffset(1.0,1.0);
      }
   }
   glCullFace(GL_FRONT_AND_BACK);
   
   /*Render the features*/
   for(f=g;f<Layer->GFeature;f++) {
      
      if ((spec->NoSelectAlpha || Layer->Select[f]) && Layer->Feature[f]) {
         alpha=Layer->Select[f]?spec->Alpha:spec->NoSelectAlpha;
         
         if (fmap!=-1 && spec->Map && Layer->Select[f]) {
            val=OGR_F_GetFieldAsDouble(Layer->Feature[f],fmap);
            VAL2COL(idx,spec,val);
            if (idx<0) continue;
         }

         if (Layer->Space==3 || Layer->Extrude!=-1) {
            if (spec->Outline && spec->Width) {
               glEnable(GL_CULL_FACE);
               if (spec->Width<0) {
                  glPushAttrib(GL_STENCIL_BUFFER_BIT);
                  glStencilFunc(GL_ALWAYS,0x1,0x1);
                  glStencilOp(GL_REPLACE,GL_REPLACE,GL_REPLACE);
               }
               glLineWidth(ABS(spec->Width));
               glPointSize(ABS(spec->Width));
               if (idx>=0 && !spec->Fill) {
                  glColor4ub(spec->Map->Color[idx][0],spec->Map->Color[idx][1],spec->Map->Color[idx][2],spec->Map->Color[idx][3]*alpha*0.01);
               } else {
                  glColor4us(spec->Outline->red,spec->Outline->green,spec->Outline->blue,alpha*655.35);
               }
               Proj->Type->Render(Proj,Layer->LFeature+f,NULL,NULL,NULL,NULL,0,0,0,Layer->Vr[0],Layer->Vr[1]);
               if (spec->Width<0) {
                  glPopAttrib();
               }
            }
         }

         if (spec->Fill) {
            if (Proj->Sun || Layer->Space==3 || Layer->Extrude!=-1) {
               glEnable(GL_LIGHTING);
               glEnable(GL_LIGHT0);
               glEnable(GL_COLOR_MATERIAL);
               glColorMaterial(GL_FRONT_AND_BACK,GL_AMBIENT_AND_DIFFUSE);
               
            }
            glDisable(GL_CULL_FACE);
            glLineWidth(0.0001);
            glPointSize(0.0001);

            if (idx>=0) {
               glColor4ub(spec->Map->Color[idx][0],spec->Map->Color[idx][1],spec->Map->Color[idx][2],spec->Map->Color[idx][3]*alpha*0.01);
            } else {
               glColor4us(spec->Fill->red,spec->Fill->green,spec->Fill->blue,alpha*655.35);
            }
            Proj->Type->Render(Proj,Layer->LFeature+f,NULL,NULL,NULL,NULL,0,0,0,Layer->Vr[0],Layer->Vr[1]);

            glDisable(GL_LIGHTING);
            glDisable(GL_LIGHT0);
            glDisable(GL_COLOR_MATERIAL);
         }

         if (Layer->Space!=3 && Layer->Extrude==-1) {
            if (spec->Outline && spec->Width) {
               glEnable(GL_CULL_FACE);
               if (spec->Width<0) {
                  glPushAttrib(GL_STENCIL_BUFFER_BIT);
                  glStencilFunc(GL_ALWAYS,0x1,0x1);
                  glStencilOp(GL_REPLACE,GL_REPLACE,GL_REPLACE);
               }
               glLineWidth(ABS(spec->Width));
               glPointSize(ABS(spec->Width));
               if (idx>=0) {
                  glColor4ub(spec->Map->Color[idx][0],spec->Map->Color[idx][1],spec->Map->Color[idx][2],spec->Map->Color[idx][3]*alpha*0.01);
               } else {
                  glColor4us(spec->Outline->red,spec->Outline->green,spec->Outline->blue,alpha*655.35);
               }
               Proj->Type->Render(Proj,Layer->LFeature+f,NULL,NULL,NULL,NULL,0,0,0,Layer->Vr[0],Layer->Vr[1]);
               if (spec->Width<0) {
                  glPopAttrib();
               }
            }
         }

         if (spec->Icon) {
            if (fsize>-1) {
               szon=OGR_F_GetFieldAsDouble(Layer->Feature[f],fsize);
               szon=szon/(spec->SizeMax-spec->SizeMin)*spec->Size;
               sz=VP->Ratio*(spec->Size+szon+spec->Width);
            }

            glDisable(GL_CULL_FACE);
            glMatrixMode(GL_MODELVIEW);
            glPushMatrix();
            Proj->Type->Locate(Proj,Layer->Loc[f].Lat,Layer->Loc[f].Lon,1);
            glTranslated(0.0,0.0,ZM(Proj,Layer->Loc[f].Elev));
            glScalef(sz,sz,1.0);

            if (spec->Width<0) {
               glPushAttrib(GL_STENCIL_BUFFER_BIT);
               glStencilFunc(GL_ALWAYS,0x1,0x1);
               glStencilOp(GL_REPLACE,GL_REPLACE,GL_REPLACE);
            }
            if (spec->Outline && spec->Width) {
               glPolygonMode(GL_FRONT_AND_BACK,GL_LINE);
               if (idx>=0 && !spec->Fill) {
                  glColor4ub(spec->Map->Color[idx][0],spec->Map->Color[idx][1],spec->Map->Color[idx][2],spec->Map->Color[idx][3]*alpha*0.01);
               } else {
                  glColor4us(spec->Outline->red,spec->Outline->green,spec->Outline->blue,alpha*655.35);
               }
               glDrawArrays(IconList[spec->Icon].Type,0,IconList[spec->Icon].Nb);
            }
            if (spec->Width<0) {
               glPopAttrib();
            }

            if (spec->Fill) {
               glPolygonMode(GL_FRONT_AND_BACK,GL_FILL);
               if (idx>=0) {
                  glColor4ub(spec->Map->Color[idx][0],spec->Map->Color[idx][1],spec->Map->Color[idx][2],spec->Map->Color[idx][3]*alpha*0.01);
               } else {
                  glColor4us(spec->Fill->red,spec->Fill->green,spec->Fill->blue,alpha*655.35);
               }
               glDrawArrays(IconList[spec->Icon].Type,0,IconList[spec->Icon].Nb);
            }
            glPopMatrix();
         }
      }
   }
   
   glDisable(GL_POLYGON_OFFSET_FILL);
   glDisable(GL_DEPTH_TEST);
   glDepthFunc(GL_LEQUAL);

   /*Render the selected features*/
   if (Layer->SFeature) {
      w=spec->HighWidth?spec->HighWidth:spec->Width;
      glColor4us(spec->HighLine->red,spec->HighLine->green,spec->HighLine->blue,spec->Alpha*655.35);
     
      if (spec->HighFill) {
         glDisable(GL_CULL_FACE);
         glLineWidth(0.0);
         glPointSize(0.0);
         
         for(f=0;f<Layer->NSFeature;f++) {
            /*If it's already been projected*/
            if (f<Layer->GFeature) {
               Proj->Type->Render(Proj,Layer->LFeature+Layer->SFeature[f],NULL,NULL,NULL,NULL,0,0,0,Layer->Vr[0],Layer->Vr[1]);
            }
         }
      }
      
      if (spec->HighLine && w) {
         glEnable(GL_CULL_FACE);
         glPushAttrib(GL_STENCIL_BUFFER_BIT);
         glStencilFunc(GL_ALWAYS,0x1,0x1);
         glStencilOp(GL_REPLACE,GL_REPLACE,GL_REPLACE);        
         glLineWidth(ABS(w));
         glPointSize(ABS(w));
         
         for(f=0;f<Layer->NSFeature;f++) {
            /*If it's already been projected*/
            if (f<Layer->GFeature) {            
               Proj->Type->Render(Proj,Layer->LFeature+Layer->SFeature[f],NULL,NULL,NULL,NULL,0,0,0,Layer->Vr[0],Layer->Vr[1]);              
            }
         }
         glPopAttrib();
      }
   }

   /*Render the feature's labels only if they're all projected*/
   if (Layer->NFeature==Layer->GFeature) {
      if (flabel>-1 && GLRender->Resolution<=1 && spec->Font) {
         Projection_UnClip(Proj);

         glMatrixMode(GL_MODELVIEW);
         glPushMatrix();
         glLoadIdentity();

         glMatrixMode(GL_PROJECTION);
         glPushMatrix();
         glLoadIdentity();

         if (GLRender->TRCon) {
            x=GLRender->TRCon->CurrentColumn*GLRender->TRCon->TileWidthNB-GLRender->TRCon->TileBorder;
            y=GLRender->TRCon->CurrentRow*GLRender->TRCon->TileHeightNB-GLRender->TRCon->TileBorder;
            gluOrtho2D(0,GLRender->TRCon->CurrentTileWidth,0,GLRender->TRCon->CurrentTileHeight);
            glTranslated(-x,y,0);
         } else {
            gluOrtho2D(0,VP->Width,0,VP->Height);
         }

         glFontUse(Tk_Display(Tk_CanvasTkwin(VP->canvas)),spec->Font);

         if (spec->Outline) {
            glColor4us(spec->Outline->red,spec->Outline->green,spec->Outline->blue,spec->Alpha*655.35);
            field=OGR_FD_GetFieldDefn(Layer->Def,flabel);

            for(f=0;f<Layer->GFeature;f++) {
               
               if (Layer->Select[f]) {               
                  if (Projection_Pixel(Proj,VP,Layer->Loc[f],vr)) {
                     OGR_SingleTypeString(lbl,Layer->Spec,field,Layer->Feature[f],flabel);
                     dx=Tk_TextWidth(spec->Font,lbl,strlen(lbl));
                     vr[0]-=(dx>>1);
                     vr[1]+=5;

                     /*If not overlapping another label*/
                     if (ViewportCrowdPush(VP,vr[0],vr[1],vr[0]+dx,vr[1]+Layer->Spec->TKM.ascent,-1)) {
                        if (Interp) {
                           glPostscriptText(Interp,VP->canvas,lbl,vr[0],vr[1],0,spec->Outline,0.0,0.5,0.0);
                        } else {
                           glDrawString(vr[0],vr[1],0,lbl,strlen(lbl),0,0);
                        }
                     }
                  }
               }
            }
         }

         /*Render the selected feature's labels*/
         if (Layer->SFeature && spec->HighLine) {
            glColor4us(spec->HighLine->red,spec->HighLine->green,spec->HighLine->blue,spec->Alpha*655.35);
            glPushAttrib(GL_STENCIL_BUFFER_BIT);
            glStencilFunc(GL_ALWAYS,0x1,0x1);
            glStencilOp(GL_REPLACE,GL_REPLACE,GL_REPLACE);
            
            field=OGR_FD_GetFieldDefn(Layer->Def,flabel);
            for(f=0;f<Layer->NSFeature;f++) {
                if (Projection_Pixel(Proj,VP,Layer->Loc[Layer->SFeature[f]],vr)) {
                  OGR_SingleTypeString(lbl,Layer->Spec,field,Layer->Feature[Layer->SFeature[f]],flabel);
                  
                  dx=Tk_TextWidth(spec->Font,lbl,strlen(lbl));
                  vr[0]-=(dx>>1);
                  vr[1]+=5;

                  if (Interp) {
                     glPostscriptText(Interp,VP->canvas,lbl,vr[0],vr[1],0,spec->Outline,0.0,0.5,0.0);
                  } else {
                     glDrawString(vr[0],vr[1],0,lbl,strlen(lbl),0,0);
                  }
               }
            }
            glPopAttrib();
         }
         glPopMatrix();
         glMatrixMode(GL_MODELVIEW);
         glPopMatrix();

         Projection_Clip(Proj);
      }
   }

   glEnable(GL_CULL_FACE);
   glCullFace(GL_BACK);
   glDisable(GL_BLEND);
   glDisable(GL_DEPTH_TEST);
   glDisable(GL_LINE_STIPPLE);
   glDisableClientState(GL_VERTEX_ARRAY);
   glStencilFunc(GL_EQUAL,0x0,0xff);
   glStencilOp(GL_KEEP,GL_KEEP,GL_KEEP);

   return(1);
}

/*--------------------------------------------------------------------------------------------------------------
 * Nom          : <OGR_Pick>
 * Creation     : Juin 2004 J.P. Gauthier - CMC/CMOE
 *
 * But          : Selectionner des features selon un point ou une region.
 *
 * Parametres  :
 *   <Interp>   : Interpreteur Tcl
 *   <Layer>    : Couche
 *   <Geom>     : Geometrie de recherche
 *   <List>     : Liste de coordonnees latlon (si pas de Geom)
 *   <All>      : Selectionne tout ou la premiere feature satisfaisante
 *   <Mode>     : Mode de selection (INTERSECT,INSIDE,OUTSIDE,NEAREST)
 *
 * Retour       :
 *
 * Remarques    :
 *
 *---------------------------------------------------------------------------------------------------------------
*/
int OGR_Pick(Tcl_Interp *Interp,OGR_Layer *Layer,OGRGeometryH *Geom,Tcl_Obj *List,int All,int Mode) {

   Tcl_Obj      *obj;
   OGRGeometryH  geom,pick;
   OGREnvelope   envg,envp;
   double        x,y,lat,lon,d=1e32;
   int           nobj,n=0,nd=0;
   unsigned int  f;
   Vect3d        vr;

   if (!Layer) {
      Tcl_AppendResult(Interp,"OGR_Pick : Invalid layer",(char*)NULL);
      return(TCL_ERROR);
   }

   Tcl_ListObjLength(Interp,List,&nobj);

   // Verifie le bon nombre de coordonnees
   if (Geom) {
      pick=Geom;
   } else {
      if (nobj%2!=0 || nobj==0) {
         Tcl_AppendResult(Interp,"OGR_Pick : Invalid number of coordinates",(char*)NULL);
         return(TCL_ERROR);
      }

      switch(nobj) {
         case 2 : pick=OGR_G_CreateGeometry(wkbPoint);      break;
         default: pick=OGR_G_CreateGeometry(wkbLinearRing); break;
      }

      for(f=0;f<nobj;f+=2) {
         Tcl_ListObjIndex(Interp,List,f,&obj);
         if (Tcl_GetDoubleFromObj(Interp,obj,&lat)==TCL_ERROR) {
            return(TCL_ERROR);
         }
         Tcl_ListObjIndex(Interp,List,f+1,&obj);
         if (Tcl_GetDoubleFromObj(Interp,obj,&lon)==TCL_ERROR) {
            return(TCL_ERROR);
         }
         Layer->GRef->UnProject(Layer->GRef,&x,&y,lat,lon,1,1);
         OGR_G_AddPoint_2D(pick,x,y);
      }
   }
   OGR_G_GetEnvelope(pick,&envp);

   obj=Tcl_NewListObj(0,NULL);
   geom=NULL;
   
   // Trouve la feature en intersection
   for(f=0;f<Layer->NFeature;f++) {
      if (Layer->Select[f] && Layer->Feature[f]) {
         if ((geom=OGR_F_GetGeometryRef(Layer->Feature[f]))) {
            OGR_G_GetEnvelope(geom,&envg);
            /*Test delon le mode*/
            switch(Mode) {
               case 0: n=OGM_Intersect(geom,pick,&envg,&envp); break;   //INTERSECT
               case 1: n=OGM_Within(geom,pick,&envg,&envp); break;      //INSIDE
               case 2: n=!OGM_Intersect(geom,pick,&envg,&envp); break;  //OUTSIDE
               case 3: x=OGR_G_Distance(geom,pick);                     //NEAREST
                       if (x<d) {
                          d=x;
                          nd=f;
                       }
                       break;
            }
            // Si on a trouve, ajouter a la liste de retour
            if (n) {
               nd=f;
               if (All!=-1) 
                  Tcl_ListObjAppendElement(Interp,obj,Tcl_NewLongObj(nd));
               if (All!=1)
                  break;
            }
         }
      }
   }

   // Dans le cas d'un vertex, trouver le plus proche
   if (All==-1) {
      if (geom && OGM_PointInside(geom,pick,vr)!=-1) {
         Tcl_ListObjAppendElement(Interp,obj,Tcl_NewDoubleObj(vr[0]));
         Tcl_ListObjAppendElement(Interp,obj,Tcl_NewDoubleObj(vr[1]));
         Tcl_ListObjAppendElement(Interp,obj,Tcl_NewDoubleObj(vr[2]));         
      }
           
//      OGM_Closest(geom,pick,vr);
//      Tcl_ListObjAppendElement(Interp,obj,Tcl_NewDoubleObj(vr[0]));
//      Tcl_ListObjAppendElement(Interp,obj,Tcl_NewDoubleObj(vr[1]));
//      Tcl_ListObjAppendElement(Interp,obj,Tcl_NewDoubleObj(vr[2]));
      
   } else {  
      // Dans le cas NEAREST, retourner le plus proche
      if (Mode==3) {
         Tcl_ListObjAppendElement(Interp,obj,Tcl_NewLongObj(nd));
      }
   }

   if (!Geom) {
      OGR_G_DestroyGeometry(pick);
   }
   
   Tcl_SetObjResult(Interp,obj);
   return(TCL_OK);
}

/*--------------------------------------------------------------------------------------------------------------
 * Nom          : <QSort_OGR>
 * Creation     : Juin 2004 J.P. Gauthier - CMC/CMOE
 *
 * But          : Fonction de comparaison pour le tri
 *
 * Parametres  :
 *   <A>       : Feature index A
 *   <B>       : Feature index B
 *
 * Retour       :
 *
 * Remarques    :
 *
 *---------------------------------------------------------------------------------------------------------------
*/
int QSort_OGR(const void *A,const void *B){

   int         fai,fbi;
   double      fad,fbd;
   const char *fas,*fbs;

   switch (QSort_Layer->Sort.Type) {
      case OFTTime:
      case OFTDate:
      case OFTDateTime:
      case OFTInteger:
         fai=OGR_F_GetFieldAsInteger(QSort_Layer->Feature[*(const int*)A],QSort_Layer->Sort.Field);
         fbi=OGR_F_GetFieldAsInteger(QSort_Layer->Feature[*(const int*)B],QSort_Layer->Sort.Field);
         return(QSort_Layer->Sort.Order*(fai-fbi));
         break;

      case OFTReal:
         fad=OGR_F_GetFieldAsDouble(QSort_Layer->Feature[*(const int*)A],QSort_Layer->Sort.Field);
         fbd=OGR_F_GetFieldAsDouble(QSort_Layer->Feature[*(const int*)B],QSort_Layer->Sort.Field);
         if (fad<fbd) {
            return(QSort_Layer->Sort.Order*-1);
         } else if (fad>fbd) {
            return(QSort_Layer->Sort.Order*1);
         } else {
            return(0);
         }
         break;

      case OFTString:
         fas=OGR_F_GetFieldAsString(QSort_Layer->Feature[*(const int*)A],QSort_Layer->Sort.Field);
         fbs=OGR_F_GetFieldAsString(QSort_Layer->Feature[*(const int*)B],QSort_Layer->Sort.Field);
         return(QSort_Layer->Sort.Order*strcmp(fas,fbs));
         break;

      case OFTRealList:
      case OFTIntegerList:
      case OFTStringList:
      case OFTWideString:
      case OFTWideStringList:
      case OFTBinary:
         break;
   }
   return(0);
}
