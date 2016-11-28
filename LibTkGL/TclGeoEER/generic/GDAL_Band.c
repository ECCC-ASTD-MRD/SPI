/*=========================================================
 * Environnement Canada
 * Centre Meteorologique Canadien
 * 2100 Trans-Canadienne
 * Dorval, Quebec
 *
 * Projet       : Lecture et traitements de fichiers raster
 * Fichier      : GDAL_Band.c
 * Creation     : Juin 2004 - J.P. Gauthier
 *
 * Description  : Fonctions de manipulations et d'affichage de fichiers raster.
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

#include <math.h>

#include "App.h"
#include "tclGDAL.h"
#include "Data_FF.h"

extern int GDAL_Type[];
extern int TD2GDAL[];

/*----------------------------------------------------------------------------------------------------------
 * Nom          : <GDAL_BandRead>
 * Creation     : Juin 2004 J.P. Gauthier - CMC/CMOE
 *
 * But          : Lire les donnees d'une bande raster
 *
 * Parametres   :
 *   <Interp>   : L'interpreteur Tcl
 *   <Name>     : Le nom de la bande a creer
 *   <FileId>   : Identificateur du fichier
 *   <NIdx>     : Nombre de bande a lire
 *   <Idxs>     : Index des bandes
 *   <X0>       : Limite inferieure en X
 *   <Y0>       : Limite inferieure en Y
 *   <X1>       : Limite superieure en X
 *   <Y1>       : Limite superieure en Y
 *   <BD>       : Bordure
 *   <Full>     : Full Resolution
 *
 * Retour       : Code d'erreur standard TCL
 *
 * Remarques    :
 *
 *---------------------------------------------------------------------------------------------------------------
*/
int GDAL_BandRead(Tcl_Interp *Interp,char *Name,char FileId[][128],int *Idxs,int NIdx,int X0,int Y0,int X1,int Y1,int BD,int Full) {

   GDAL_File      *file=NULL;
   GDAL_Band      *band;
   GDALRasterBandH hband;
   GDALColorTableH hTable;
   GDALColorEntry  entry;
   GDALDataType    type=GDT_Unknown;
   int             signedByte=0;
   int             c;
   int             i,nx,ny,rx,ry;
   double          dval,minmax[2];

   if (!NIdx) {
      Tcl_AppendResult(Interp,"GDAL_BandRead: No valid band specified",(char*)NULL);
      return(TCL_ERROR);
   }

   /*Get info on all requested bands*/
   rx=ry=nx=ny=0;
   hband=NULL;

   for(i=0;i<NIdx;i++) {
      if ((file=GDAL_FileGet(Interp,FileId[i]))) {
         /*Get the band type and promote to higher type among all*/
         hband=GDALGetRasterBand(file->Set,Idxs[i]);
         type=GDALGetRasterDataType(hband)>type?GDALGetRasterDataType(hband):type;
         if (type == GDT_Byte) {
            const char *strPixelType=GDALGetMetadataItem(hband,"PIXELTYPE", "IMAGE_STRUCTURE");
            signedByte = strPixelType ? (strcmp( strPixelType, "SIGNEDBYTE" )==0) : 0 ;
         }

         /*Check for size compatibility*/
         nx=GDALGetRasterBandXSize(hband);
         ny=GDALGetRasterBandYSize(hband);
         if (i>0 && rx!=nx && ry!=ny) {
            Tcl_AppendResult(Interp,"GDAL_BandRead: Dimensions don't match",(char*)NULL);
            return(TCL_ERROR);
         }
         rx=nx;ry=ny;
      } else {
         Tcl_AppendResult(Interp,"GDAL_BandRead: Invalid file handle",(char*)NULL);
         return(TCL_ERROR);
      }
   }

   GDAL_BandDestroy(Interp,Name);
   if (!(band=GDAL_BandCreate(Interp,Name))) {
      return(TCL_ERROR);
   }

   /*Get the data units*/
//      band->Spec->Unit=GDALGetRasterUnitType(hband);

   /*If size is not specified, then read the whole thing*/
   if (X0==-1 || Y0==-1 || X1==-1 || Y1==-1) {
      nx=GDALGetRasterBandXSize(hband);
      X0=0;
      X1=nx-1;
      BD=0;
      rx=0;

      ny=GDALGetRasterBandYSize(hband);
      Y0=0;
      Y1=ny-1;
      BD=0;
      ry=0;
   } else {
      /*Add border to dimensions to read*/
      X0-=BD;X1+=BD;
      Y0-=BD;Y1+=BD;

      /*Check for limit overflow*/
      X0=X0<0?0:X0; X1=X1>=nx?nx-1:X1;
      Y0=Y0<0?0:Y0; Y1=Y1>=ny?ny-1:Y1;
      nx=X1-X0+1;
      ny=Y1-Y0+1;

      rx=ry=1;
   }

   if (!(band->Def=Def_New(nx,ny,1,(Full?NIdx:-NIdx),(type==GDT_Byte&&signedByte)?TD_Byte:GDAL_Type[type]))) {
      Tcl_AppendResult(Interp,"GDAL_BandRead: Could not allocate memory",(char*)NULL);
      return(TCL_ERROR);
   }

   /*Get the control points if any*/
   if ((band->NbGCPs=GDALGetGCPCount(file->Set))) {
      band->GCPs=(GDAL_GCP*)malloc(band->NbGCPs*sizeof(GDAL_GCP));
      memcpy(band->GCPs,GDALGetGCPs(file->Set),band->NbGCPs*sizeof(GDAL_GCP));
   }

   band->GRef=GDAL_GeoRef(file->Set,hband,band->GCPs,band->NbGCPs,nx,ny);
   band->ZRef=ZRef_Define(LVL_UNDEF,1,NULL);
   band->GPos=GeoPos_Find(band->GRef,band->ZRef);
   GeoRef_Size(band->GRef,X0+BD,Y0+BD,X1-BD,Y1-BD,BD);
   GeoRef_Qualify(band->GRef);

   /*Get the No Data Value*/
   dval=GDALGetRasterNoDataValue(hband,&i);

   if (i)
      band->Def->NoData=dval;

   for(i=0;i<NIdx;i++) {
      file=GDAL_FileGet(NULL,FileId[i]);
      if (file) {
         band->Band[i]=GDALGetRasterBand(file->Set,Idxs[i]);
         if ((!rx && GDALGetRasterBandXSize(band->Band[i])!=band->Def->NI) || (!ry && GDALGetRasterBandYSize(band->Band[i])!=band->Def->NJ)) {
            Tcl_AppendResult(Interp,"GDAL_BandRead: Incompatible dimensions between bands",(char*)NULL);
            return(TCL_ERROR);
         }

         band->Spec->Desc=strdup(GDALGetDescription(band->Band[i]));
         if (Full) {
            if (GDALRasterIOEx(band->Band[i],GF_Read,X0,Y0,band->Def->NI,band->Def->NJ,band->Def->Data[i],band->Def->NI,band->Def->NJ,type,0,0,NULL)==CE_Failure) {
               Tcl_AppendResult(Interp,"GDAL_BandRead: Unable to read band data",(char*)NULL);
               return(TCL_ERROR);
            }
         }
      } else if (Idxs[i]!=-1) {
         Tcl_AppendResult(Interp,"GDAL_BandRead: Invalid file handle",(char*)NULL);
         return(TCL_ERROR);
      }
   }

   band->File=file;
   band->Tex.Nx=nx;
   band->Tex.Ny=ny;
   band->Tex.ThreadId=NULL;

   if (band->Spec->Map)
      CMap_Free(band->Spec->Map);

   if ((hTable=GDALGetRasterColorTable(hband))) {
      App_Log(DEBUG,"%s: Color Table (%s with %d entries)\n",__func__,GDALGetPaletteInterpretationName(GDALGetPaletteInterpretation(hTable)),GDALGetColorEntryCount(hTable));
      
      if (GDALGetRasterColorInterpretation(hband)==GCI_PaletteIndex) {
         //  palette is always 256 colors so in color index mode get max value as nb colors
         band->Tex.Indexed=1;
         GDALComputeRasterMinMax(hband,TRUE,minmax);
         band->Spec->Map=CMap_New(NULL,minmax[1]);
      } else {
         band->Spec->Map=CMap_New(NULL,GDALGetColorEntryCount(hTable));
      }
      for (c=0;c<band->Spec->Map->NbPixels;c++) {
          GDALGetColorEntryAsRGB(hTable,c,&entry);
          band->Spec->Map->Control[c][0]=entry.c1;
          band->Spec->Map->Control[c][1]=entry.c2;
          band->Spec->Map->Control[c][2]=entry.c3;
          band->Spec->Map->Control[c][3]=entry.c4;
      }
      band->Spec->Map->Max[3]=255;
   } else {
      band->Spec->Map=CMap_New(NULL,256);
      band->Spec->Map->Control[0][0]=0;
      band->Spec->Map->Control[0][1]=0;
      band->Spec->Map->Control[0][2]=0;
      band->Spec->Map->Control[0][3]=NIdx==4?0:255;
      band->Spec->Map->Control[255][0]=255;
      band->Spec->Map->Control[255][1]=255;
      band->Spec->Map->Control[255][2]=255;
      band->Spec->Map->Control[255][3]=255;
   }
   CMap_ControlDefine(band->Spec->Map);
   CMap_CurveDefine(band->Spec->Map);
   CMap_RatioDefine(band->Spec->Map);
   CMap_Put(Interp,band->Spec->Map);

   return(TCL_OK);
}

/*----------------------------------------------------------------------------
 * Nom      : <GDAL_BandFSTDCopy>
 * Creation : Mars 2010 - E. Legault-Ouellet - CMC/CMOE
 *
 * But      : Interpole une coupe verticale d'un champ FSTD dans une bande
 *            GDAL.
 *
 * Parametres  :
 *  <Interp>   : Interpreteur TCL
 *  <Band>     : Bande raster
 *  <Field>    : Champs RPN
 *
 * Retour:
 *  <TCL_...> : Code d'erreur de TCL.
 *
 * Remarques :
 *
 *----------------------------------------------------------------------------
*/

int GDAL_BandFSTDImportV(Tcl_Interp *Interp,GDAL_Band *Band,TData *Field,int Scale) {

   double incri,incrj,posX,posY,dfy;
   int    x,y,cidx,idx,z,lvl=0;
   float  *levels=NULL;
   double dir,val;

   if (!Band) {
      Tcl_AppendResult(Interp,"GDAL_BandFSTDImportV: Invalid band",(char*)NULL);
      return(TCL_ERROR);
   }

   if (!Field) {
      Tcl_AppendResult(Interp,"GDAL_BandFSTDImportV: Invalid field",(char*)NULL);
      return(TCL_ERROR);
   }

   if (Field->Spec->Map) {
      Data_PreInit(Field);
      if (Band->Def->NC==1) {
         if (Band->Spec->Map)
            CMap_Free(Band->Spec->Map);
         CMap_Incr(Field->Spec->Map);
         Band->Spec->Map=Field->Spec->Map;
         Band->Tex.Indexed=1;
      }
   }

   if (!Field->GRef) {
      Tcl_AppendResult(Interp,"GDAL_BandFSTDCopy: Missing GeoRef",(char*)NULL);
      return(TCL_ERROR);
   }

   if (!Field->ZRef->Levels) {
      Tcl_AppendResult(Interp,"GDAL_BandFSTDCopy: Missing Levels",(char*)NULL);
      return(TCL_ERROR);
   }

   // Ponderates levels
   if (Scale) {
      levels=(float*)malloc(Field->Def->NJ * sizeof(float));
      for(z=0; z<Field->Def->NJ; ++z) {
         levels[z] = SCALEVALUE(Field->ZRef->Levels[0], Field->ZRef->Levels[Field->Def->NJ-1], Field->ZRef->Levels[z]);
      }
   }

   incri=(double)(Field->Def->NI)/(double)(Band->Def->NI);
   incrj=(double)(Field->Def->NJ)/(double)(Band->Def->NJ);

   // Fill image
   for(y=0, idx=0; y<Band->Def->NJ; ++y) {

      // Check at which level we are and calculate ponderate value on the Y axis
      if (Scale) {
         dfy =(double)y/((double)Band->Def->NJ-1.0);
         lvl =(levels[lvl]>dfy) ? lvl : (lvl+1>Field->Def->NJ-1?Field->Def->NJ-1:lvl+1);
         posY=(Field->Def->NJ-1)-((dfy-levels[lvl-1])/(levels[lvl]-levels[lvl-1])+lvl-1);
      } else {
         posY=(double)y*incrj;
      }

      for(x=0; x<Band->Def->NI; ++x) {
         posX=(double)x*incri;

         // Get interpolated value for that point (pixel)
         if (!Field->GRef->Value(Field->GRef,Field->Def,Field->Spec->InterpDegree[0],0,posX,posY,0,&val,&dir)) {
            val=Field->Def->NoData;
         }

         if (val!=Field->Def->NoData) {
            VAL2COL(cidx,Field->Spec,val);
         } else {
            val=0.0;
            cidx=-1;
         }

         // Get color for interpolated value and set pixel color
         if (Field->Spec->Map) {
            if (Band->Def->NC==1) {
               Def_Set(Band->Def,0,idx,cidx);
            } else {
               for (z=0;z<Band->Def->NC;z++) {
                  if (cidx>-1) {
                     Def_Set(Band->Def,z,idx,Field->Spec->Map->Color[cidx][z]);
                  } else {
                     Def_Set(Band->Def,z,idx,0);
                  }
               }
            }
         } else {
            if (Band->Def->NC>=1) {
               Def_Set(Band->Def,0,idx,val);
            }
            if (Band->Def->NC>=2) {
               Def_Set(Band->Def,1,idx,dir);
            }
         }
        ++idx;
      }
   }
   if (levels)
      free(levels);

   return(TCL_OK);
}

/*----------------------------------------------------------------------------
 * Nom      : <GDALBand_GetImage>
 * Creation : Juin 2012 - J.P. Gauthier - CMC/CMOE
 *
 * But      : Recuperer une image Tk d'une bande.
 *
 * Parametres     :
 *  <Interp>      : Interpreteur TCL
 *  <Band>        : Bande
 *  <Img>         : Image
 *
 * Retour:
 *  <TCL_...> : Code d'erreur de TCL.
 *
 * Remarques :
 *
 *----------------------------------------------------------------------------
*/
int GDALBand_GetImage(Tcl_Interp *Interp,GDAL_Band *Band,char* Img){

   unsigned int x,y,idx,nidx,val;

   Tk_PhotoImageBlock data;
   Tk_PhotoHandle     handle;

   if (!Band) {
      Tcl_AppendResult(Interp,"GDALBand_GetImage: Invalid band",(char*)NULL);
      return(TCL_ERROR);
   }

   /*Recuperer le handle de l'image specifie*/
   handle=Tk_FindPhoto(Interp,Img);

   /*Definir les parametres du bock de donnees*/
   if (Tk_PhotoSetSize(Interp,handle,Band->Def->NI,Band->Def->NJ)==TCL_ERROR) {
      return(TCL_ERROR);
   }
   data.width=Band->Def->NI;
   data.height=Band->Def->NJ;
   data.pixelSize=4;
   data.pitch=data.width*data.pixelSize;
   data.offset[0]=0;
   data.offset[1]=1;
   data.offset[2]=2;
   data.offset[3]=3;

   if (!(data.pixelPtr=(unsigned char*)malloc(data.width*data.height*data.pixelSize*sizeof(unsigned char)))) {
      Tcl_AppendResult(Interp,"GDALBand_GetImage: Unable to allocate image",(char*)NULL);
      return(TCL_ERROR);
   }

   idx=nidx=0;
   for(y=0;y<Band->Def->NJ;y++) {
      for(x=0;x<Band->Def->NI;x++) {
         Def_Get(Band->Def,0,nidx,val);

         if (Band->Def->NC==1) {
            if (Band->Spec->Map) {
               data.pixelPtr[idx++]=Band->Spec->Map->Color[val][0];
               data.pixelPtr[idx++]=Band->Spec->Map->Color[val][1];
               data.pixelPtr[idx++]=Band->Spec->Map->Color[val][2];
               data.pixelPtr[idx++]=Band->Spec->Map->Color[val][3];
            } else {
               data.pixelPtr[idx++]=val;
               data.pixelPtr[idx++]=val;
               data.pixelPtr[idx++]=val;
               data.pixelPtr[idx++]=255;
            }
         } else if (Band->Def->NC==2) {
            data.pixelPtr[idx++]=val;
            data.pixelPtr[idx++]=val;
            data.pixelPtr[idx++]=val;
            Def_Get(Band->Def,1,nidx,data.pixelPtr[idx++]);
         } else if (Band->Def->NC==3) {
            data.pixelPtr[idx++]=val;
            Def_Get(Band->Def,1,nidx,data.pixelPtr[idx++]);
            Def_Get(Band->Def,2,nidx,data.pixelPtr[idx++]);
            data.pixelPtr[idx++]=255;
         } else {
            data.pixelPtr[idx++]=val;
            Def_Get(Band->Def,1,nidx,data.pixelPtr[idx++]);
            Def_Get(Band->Def,2,nidx,data.pixelPtr[idx++]);
            Def_Get(Band->Def,3,nidx,data.pixelPtr[idx++]);
         }
         nidx++;
      }
   }

   /*Envoyer le data dans l'image Tk*/
   if (Tk_PhotoPutBlock(Interp,handle,&data,0,0,Band->Def->NI,Band->Def->NJ,TK_PHOTO_COMPOSITE_SET)==TCL_ERROR) {
     free(data.pixelPtr);
     return(TCL_ERROR);
   }
   free(data.pixelPtr);

   return(TCL_OK);
}

/*----------------------------------------------------------------------------
 * Nom      : <Murphy_WideLine>
 * Creation : Mai 2012 - J.P. Gauthier - CMC/CMOE
 *
 * But      : Algorithme de Murpy pour des lignes epaisse.
 *
 * Parametres :
 *  <P0>      : Point de depart.
 *  <P1>      : Point de fin.
 *  <Nb>      : Largeur.
*
 * Retour:
 *
 * Remarques :
 *   - Base sur le code GNU de hp2xx
 *----------------------------------------------------------------------------
*/
typedef struct TMurphy {
   TDef     *Def;                 /* Data definition */
   unsigned char Color[4];            /* Specification des donnees*/
   int           Width,Idx;                 /* Value index */
   int    u,v;               /* delta x , delta y */
   int    ku,kt,kv,kd;     /* loop constants */
   int    oct2;
   int    quad4;
} TMurphy;

static void inline Murphy_Plot(TMurphy *M,int X, int Y) {

   unsigned long n;

   if (FIN2D(M->Def,X,Y)) {
      n=FIDX2D(M->Def,X,Y);
      if (M->Def->NC==1) {
         Def_Set(M->Def,0,n,M->Idx);
      } else {
         Def_Set(M->Def,0,n,M->Color[0]);
         if (M->Def->NC>1) Def_Set(M->Def,1,n,M->Color[1]);
         if (M->Def->NC>2) Def_Set(M->Def,2,n,M->Color[2]);
         if (M->Def->NC>3) Def_Set(M->Def,3,n,M->Color[3]);
      }
   }
}

void Murphy_ParaLine(TMurphy *M,int X,int Y,int D1) {

   int p;                        /* pel counter, p=along line */
   D1 = -D1;

   for (p=0;p<=M->u;p++) {       /* test for end of parallel line */
      Murphy_Plot(M,X,Y);

      if (D1<=M->kt) {  /* square move */
         if (M->oct2 == 0) {
            X++;
         } else {
            if (M->quad4 == 0) {
               Y++;
            } else {
               Y--;
            }
         }
         D1 += M->kv;
      } else {        /* diagonal move */
         X++;
         if (M->quad4 == 0) {
            Y++;
         } else {
            Y--;
         }
         D1+=M->kd;
      }
   }
}

int Murphy_WideLine(TMurphy *M,Vect3d P0,Vect3d P1) {

   double  offset = M->Width/2.0;
   Vect2i  pt,p0,p1;

   int    tmp;
   int    d0,d1;              /* difference terms d0=perpendicular to line, d1=along line */
   int    q;                  /* pel counter,q=perpendicular to line */
   int    dd;                 /* distance along line */
   int    tk;                 /* thickness threshold */
   double ang;                /* angle for initial point calculation */

   p0[0]=lrint(P0[0]); p0[1]=lrint(P0[1]);
   p1[0]=lrint(P1[0]); p1[1]=lrint(P1[1]);

   /* Initialisation */
   M->u = p1[0] - p0[0]; /* delta x */
   M->v = p1[1] - p0[1]; /* delta y */

   if (M->u<0) {     /* swap to make sure we are in quadrants 1 or 4 */
      pt[0] = p0[0]; p0[0] = p1[0]; p1[0] = pt[0];
      pt[1] = p0[1]; p0[1] = p1[1]; p1[1] = pt[1];
      M->u *= -1;
      M->v *= -1;
   }

   if (M->v<0) {     /* swap to 1st quadrant and flag */
      M->v *= -1;
      M->quad4 = 1;
   } else {
      M->quad4 = 0;
   }

   if (M->v > M->u) {      /* swap things if in 2 octant */
      tmp = M->u; M->u = M->v; M->v = tmp;
      M->oct2 = 1;
   } else {
      M->oct2 = 0;
   }

   M->ku = M->u  + M->u;     /* change in l for square shift */
   M->kv = M->v  + M->v;     /* change in d for square shift */
   M->kd = M->kv - M->ku;    /* change in d for diagonal shift */
   M->kt = M->u  - M->kv;    /* diag/square decision threshold */

   if (!M->u && !M->v)       /* Nothing to do */
      return(0);

   d0 = d1 = dd = 0;
   ang = atan((double)M->v/(double)M->u);      /* calc new initial point - offset both sides of ideal */

   if (M->oct2 == 0) {
      pt[0] = p0[0] + lrint(offset * sin(ang));
      if (M->quad4 == 0) {
         pt[1] = p0[1] - lrint(offset * cos(ang));
      } else {
         pt[1] = p0[1] + lrint(offset * cos(ang));
      }
   } else {
      pt[0] = p0[0] - lrint(offset * cos(ang));
      if (M->quad4 == 0) {
         pt[1] = p0[1] + lrint(offset * sin(ang));
      } else {
         pt[1] = p0[1] - lrint(offset * sin(ang));
      }
   }

   tk = M->Width<=1?2:(int)(4.0 * hypot(pt[0]-p0[0], pt[1]-p0[1]) * hypot(M->u,M->v));  /* used here for constant thickness line */

   for (q=0;dd<=tk;q++) {    /* outer loop, stepping perpendicular to line */

    Murphy_ParaLine(M,pt[0],pt[1],d1);        /* call to inner loop - right edge */

      if (d0 < M->kt) {   /* square move  - M2 */
         if (M->oct2 == 0) {
            if (M->quad4 == 0) {
               pt[1]++;
            } else {
               pt[1]--;
            }
         } else {
            pt[0]++;
         }
      } else {        /* diagonal move */
         dd += M->kv;
         d0 -= M->ku;
         if (d1 < M->kt) {   /* normal diagonal - M3 */
            if (M->oct2 == 0) {
               pt[0]--;
               if (M->quad4 == 0) {
                  pt[1]++;
               } else {
                  pt[1]--;
               }
            } else {
               pt[0]++;
               if (M->quad4 == 0) {
                  pt[1]--;
               } else {
                  pt[1]++;
               }
            }
            d1 += M->kv;
         } else {        /* double square move, extra parallel line */
            if (M->oct2 == 0) {
               pt[0]--;
            } else {
               if (M->quad4 == 0) {
                  pt[1]--;
               } else {
                  pt[1]++;
               }
            }
            d1 += M->kd;
            if (dd > tk) {
               return(1); /* breakout on the extra line */
            }
            Murphy_ParaLine(M,pt[0],pt[1],d1);

            if (M->oct2 == 0) {
               if (M->quad4 == 0) {
                  pt[1]++;
               } else {
                  pt[1]--;
               }
            } else {
               pt[0]++;
            }
         }
      }
      dd+=M->ku;
      d0+=M->kv;
   }
   return(1);
}

int Murphy_Polygon(TMurphy *M,double *Poly,int Nb,int X,int Y,int Scale,double Angle,int Included) {

   int      n,i=0,j;
   double   sina,cosa;
   double   x,y,px,py,pxi,pyi,pxj,pyj,minx,miny,maxx,maxy;
   double   node[256],poly[256],swap;

   // Get grid coordinates of polygon
   minx=miny=1e32;
   maxx=maxy=-1e32;
   Angle=DEG2RAD(Angle);
   sina=sin(Angle);
   cosa=cos(Angle);
   Nb<<=1;

   for(n=0;n<Nb;n+=2) {
      px=Poly[n]*Scale;
      py=Poly[n+1]*Scale;
      poly[n]=X+(px*cosa-py*sina);
      poly[n+1]=Y+(px*sina+py*cosa);
      minx=FMIN(minx,poly[n]);
      miny=FMIN(miny,poly[n+1]);
      maxx=FMAX(maxx,poly[n]);
      maxy=FMAX(maxy,poly[n+1]);
   }

   // Render only if completely included
   if (!Included || (minx>=-2 && miny>=-2 && maxx<M->Def->NI+2 && maxy<M->Def->NJ+2)) {

      // Loop through the rows of the raster grid.
      for (y=miny;y<maxy;y++) {

         //  Build a list of nodes.
         n=0; j=Nb-2;
         for (i=0; i<Nb; i+=2) {
            pyi=poly[i+1];
            pyj=poly[j+1];
            pxi=poly[i];
            pxj=poly[j];

            if ((pyi<y && pyj>=y) || (pyj<y && pyi>=y)) {
               node[n++]=(pxi+(y-pyi)/(pyj-pyi)*(pxj-pxi));
            }
            j=i;
         }

         // Sort the nodes, via a simple Bubble sort.
         i=0;
         while (i<n-1) {
            if (node[i]>node[i+1]) {
               swap=node[i]; node[i]=node[i+1]; node[i+1]=swap;
               if (i) i--;
            } else {
               i++;
            }
         }

         // Rasterize between node pairs.
         for (i=0; i<n; i+=2) {
            if (node[i]>M->Def->NI) break;
            if (node[i+1]>0 ) {
               if (node[i]  <0 ) node[i]=0 ;
               if (node[i+1]>M->Def->NI) node[i+1]=M->Def->NI-1;

               for (x=node[i]; x<node[i+1]; x++) {
                  Murphy_Plot(M,x,y);
               }
            }
         }
      }
   }
   return(i);
}

/*----------------------------------------------------------------------------
 * Nom      : <GDAL_BandFSTDImport>
 * Creation : Mai 2006 - J.P. Gauthier - CMC/CMOE
 *
 * But      : Importe un champs RPN dans une structure GDAL
 *
 * Parametres  :
 *  <Interp>   : Interpreteur TCL
 *  <Band>     : Bande raster
 *  <Field>    : Champs RPN
 *
 * Retour:
 *  <TCL_...> : Code d'erreur de TCL.
 *
 * Remarques :
 *
 *----------------------------------------------------------------------------
*/
int GDAL_BandFSTDImport(Tcl_Interp *Interp,GDAL_Band *Band,TData *Field) {

   double    lat,lon,latd,lond,i,j;
   double    val,dir,ddir;
   int       n,x,y,z=0,idx,dy;
   TGeoScan  scan;
   TList    *list;
   T3DArray *array;
   TMurphy   m;
   TDef *def=NULL;
   Vect3d    p0,p1;

   extern TIcon IconList[14];

   if (!Band) {
      Tcl_AppendResult(Interp,"GDAL_BandFSTDImport: Invalid band",(char*)NULL);
      return(TCL_ERROR);
   }

   if (!Field) {
      Tcl_AppendResult(Interp,"GDAL_BandFSTDImport: Invalid field",(char*)NULL);
      return(TCL_ERROR);
   }

   Data_PreInit(Field);
   
   if (Field->Spec->Map) {
      if (Band->Def->NC==1) {
         if (Band->Spec->Map)
            CMap_Free(Band->Spec->Map);
         CMap_Incr(Field->Spec->Map);
         Band->Spec->Map=Field->Spec->Map;
         Band->Tex.Indexed=1;
      }
   }

   // Initialize linescan object
   GeoScan_Init(&scan);

   // Check if we need contouring
   if (Field->Spec->RenderContour && Field->Spec->Width && Field->Spec->InterNb) {
      def=Def_New(Band->Def->NI,Band->Def->NJ,1,1,TD_Float32);
   }

   // Check if we can reproject all in one shot, otherwise, do by scanline
   dy=(Band->Def->NI*Band->Def->NJ)>4194304?1:Band->Def->NJ;
   for(y=0;y<Band->Def->NJ;y+=dy) {

      // Reproject
      if (!GeoScan_Get(&scan,Field->GRef,Field->Def,Band->GRef,Band->Def,0,y,Band->Def->NI-1,y+(dy-1),1,(char*)Field->Spec->InterpDegree)) {
         Tcl_AppendResult(Interp,"GDAL_BandFSTDImport: Unable to allocate coordinate scanning buffer",(char*)NULL);
         return(TCL_ERROR);
      }

      for(n=0;n<scan.N;n++){
         // Get the value of the data field at this latlon coordinate
         val=scan.D[n];
         if (!isnan(val) && val!=(float)Field->Def->NoData) {

            if (def)
               Def_Set(def,0,scan.V[n],val);

            if (Field->Spec->RenderTexture) {
               if (Field->Spec->Map) {
                  VAL2COL(idx,Field->Spec,val);

                  if (idx>-1) {
                     if (Band->Def->NC==1) {
                        Def_Set(Band->Def,0,scan.V[n],idx);
                     } else {
                        Def_Set(Band->Def,0,scan.V[n],Field->Spec->Map->Color[idx][0]);
                        if (Band->Def->NC>1) Def_Set(Band->Def,1,scan.V[n],Field->Spec->Map->Color[idx][1]);
                        if (Band->Def->NC>2) Def_Set(Band->Def,2,scan.V[n],Field->Spec->Map->Color[idx][2]);
                        if (Band->Def->NC>3) Def_Set(Band->Def,3,scan.V[n],Field->Spec->Map->Color[idx][3]);
                     }
                  }
               } else {
                  if (Band->Def->NC<3) {
                     if (Band->Def->NC>=1) {
                        Def_Set(Band->Def,0,scan.V[n],val);
                     }
                     if (Band->Def->NC>=2) {
                        Def_Set(Band->Def,1,scan.V[n],dir);
                     }
                  }
               }
            }
         }
      }
   }

   GeoScan_Clear(&scan);

   // Check for contouring 
   if (Field->Spec->RenderContour && Field->Spec->Width && Field->Spec->InterNb) {
      FFContour(REF_GRID,Band->GPos,def,NULL,NULL,Field->Spec->InterNb,Field->Spec->Inter,1,0);

      // Initialize murphy line object
      m.Def=Band->Def;
      m.Idx=-1;
      m.Width=Field->Spec->Width;
      if (Field->Spec->Outline) {
         m.Color[0]=Field->Spec->Outline->red;
         m.Color[1]=Field->Spec->Outline->green;
         m.Color[2]=Field->Spec->Outline->blue;
      } else {
         m.Color[0]=m.Color[1]=m.Color[2]=0;
      }
      m.Color[3]=255;

      // Loop on all contours
      list=def->Segments;
      while(list) {
         array=(T3DArray*)list->Data;

         if (array->Size) {
            if (Field->Spec->MapAll && Field->Spec->Map) {
               VAL2COL(m.Idx,Field->Spec,array->Value);
               m.Color[0]=Field->Spec->Map->Color[m.Idx][0];
               m.Color[1]=Field->Spec->Map->Color[m.Idx][1];
               m.Color[2]=Field->Spec->Map->Color[m.Idx][2];
               m.Color[3]=Field->Spec->Map->Color[m.Idx][3];
            }

            // Loop on the contour points
            z=1;
            for (n=0;n<array->Size-1;n++) {
               // If length was not enough, keep fire segement point from previous
               if (z) p0[0]=array->Data[n][0];p0[1]=array->Data[n][1];
               p1[0]=array->Data[n+1][0];p1[1]=array->Data[n+1][1];

               z=Murphy_WideLine(&m,p0,p1);
           }
         }
         list=list->Next;
      }
   }

   if (def) Def_Free(def);

   // Check for vectorial
   if (Field->Spec->RenderVector && Field->Def->NC>=2) {

      m.Def=Band->Def;
      m.Idx=-1;
      m.Width=Field->Spec->Width;
      if (Field->Spec->Fill) {
         m.Color[0]=Field->Spec->Fill->red;
         m.Color[1]=Field->Spec->Fill->green;
         m.Color[2]=Field->Spec->Fill->blue;
      } else {
         m.Color[0]=m.Color[1]=m.Color[2]=0;
      }
      m.Color[3]=255;

      // Loop on raster and place arrows at specified interval
      for(x=0;x<Band->Def->NI;x+=Field->Spec->Sample) {
         for(y=0;y<Band->Def->NJ;y+=Field->Spec->Sample) {
            Band->GRef->Project(Band->GRef,x,y,&lat,&lon,0,1);

            if (Field->GRef->UnProject(Field->GRef,&i,&j,lat,lon,0,1)) {
               idx=FIDX2D(Field->Def,(int)i,(int)j);
               Field->GRef->Value(Field->GRef,Field->Def,Field->Spec->InterpDegree[0],0,i,j,0,&val,&dir);
               if ((val<=Field->Spec->Max || Field->Spec->MapAbove) && (val>=Field->Spec->Min || Field->Spec->MapBellow)) {
                  if (Field->Spec->MapAll && Field->Spec->Map) {
                     VAL2COL(m.Idx,Field->Spec,val);
                     m.Color[0]=Field->Spec->Map->Color[m.Idx][0];
                     m.Color[1]=Field->Spec->Map->Color[m.Idx][1];
                     m.Color[2]=Field->Spec->Map->Color[m.Idx][2];
                     m.Color[3]=Field->Spec->Map->Color[m.Idx][3];
                  }

                  // Reproject vector orientation by adding destination projection's north difference
                  if (Band->GRef->Type&GRID_NUNORTH) {                     
                     Band->GRef->Project(Band->GRef,x,y+1,&latd,&lond,1,1);

                     lat=DEG2RAD(lat);   lon=DEG2RAD(lon);
                     latd=DEG2RAD(latd); lond=DEG2RAD(lond);
                     ddir=COURSE(lat,lon,latd,lond);
                     dir+=RAD2DEG(ddir)+180;
                  }
                  Murphy_Polygon(&m,IconList[13].Co,IconList[13].Nb,x,y,VECTORSIZE(Field->Spec,val),dir,TRUE);
               }
            }
         }
      }
   }

   return(TCL_OK);
}

/*--------------------------------------------------------------------------------------------------------------
 * Nom          : <GDAL_BandWrite>
 * Creation     : Juin 2004 J.P. Gauthier - CMC/CMOE
 *
 * But          : Ecrire les donnees d'une bande raster
 *
 * Parametres   :
 *   <Interp>   : L'interpreteur Tcl
 *   <Bands>    : Liste des bandes a sauvegarder
 *   <FileId>   : Identificateur du fichier
 *
 * Retour       : Code d'erreur standard TCL
 *
 * Remarques    :
 *
 *---------------------------------------------------------------------------------------------------------------
*/
int GDAL_BandWrite(Tcl_Interp *Interp,Tcl_Obj *Bands,char *FileId,char **Options) {

   Tcl_Obj        *obj;
   GDAL_Band      *band;
   GDAL_File      *file;
   GDALColorTableH htable;
   GDALColorEntry  centry;
   int             i,ns,nc,n;
   char           *str;

   Tcl_ListObjLength(Interp,Bands,&ns);
   if (!ns) {
      Tcl_AppendResult(Interp,"GDAL_BandWrite: No band specified",(char*)NULL);
      return(TCL_ERROR);
   }

   Tcl_ListObjIndex(Interp,Bands,0,&obj);
   nc=0;
   band=NULL;

   for(n=0;n<ns;n++) {
      band=GDAL_BandGet(Tcl_GetString(obj));
      if (!band) {
         Tcl_AppendResult(Interp,"GDAL_BandWrite: invalid band ",Tcl_GetString(obj),(char*)NULL);
         return(TCL_ERROR);
      }
      nc+=band->Def->NC;
   }

   if (!(file=GDAL_FileGet(Interp,FileId))) {
      Tcl_AppendResult(Interp,"GDAL_BandWrite: Invalid file handle",(char*)NULL);
      return(TCL_ERROR);
   }

   /*Delayed creation until we know the dimensions*/
   if (!file->Set) {
      file->Set=GDALCreate(file->Driver,file->Name,band->Def->NI,band->Def->NJ,nc,TD2GDAL[band->Def->Type],Options);

      /*Write the georeference stuff*/
      if (band->NbGCPs) {
         /*Write GCPS*/
         GDALSetGCPs(file->Set,band->NbGCPs,band->GCPs,band->GRef->String);
      } else {
         /*Write Transform*/
         if (band->GRef) {
            if (band->GRef->Transform)
               GDALSetGeoTransform(file->Set,band->GRef->Transform);

            if (band->GRef->Spatial) {
               OSRExportToWkt(band->GRef->Spatial,&str);
               GDALSetProjection(file->Set,str);
               OGRFree(str);
            }
         }
      }
   }

   nc=0;
   for(n=0;n<ns;n++) {
      Tcl_ListObjIndex(Interp,Bands,n,&obj);
      band=GDAL_BandGet(Tcl_GetString(obj));

      /*Write the colormap if we have one*/
      if (band->Spec->Map && band->Tex.Indexed) {
         band->Band[0]=GDALGetRasterBand(file->Set,nc+1);
         htable=GDALCreateColorTable(GPI_RGB);
         for (i=0;i<band->Spec->Map->NbPixels;i++) {
            centry.c1=band->Spec->Map->Color[i][0];
            centry.c2=band->Spec->Map->Color[i][1];
            centry.c3=band->Spec->Map->Color[i][2];
            centry.c4=band->Spec->Map->Color[i][3];
            GDALSetColorEntry(htable,i,&centry);
         }
         GDALSetRasterColorTable(band->Band[0],htable);
      }

      /*Write every band*/
      for(i=0;i<band->Def->NC;i++) {
         band->Band[i]=GDALGetRasterBand(file->Set,nc+1);

         if (band->Spec->Map && band->Tex.Indexed) {
            GDALSetRasterColorInterpretation(band->Band[i],GCI_PaletteIndex);
         } else {
            GDALSetRasterColorInterpretation(band->Band[i],GCI_Undefined);
         }

         if (!isnan(band->Def->NoData))
            GDALSetRasterNoDataValue(band->Band[i],band->Def->NoData);

         if (band->Spec->Desc)
            GDALSetDescription(band->Band[i],band->Spec->Desc);

         if (GDALRasterIOEx(band->Band[i],GF_Write,0,0,band->Def->NI,band->Def->NJ,band->Def->Data[i],band->Def->NI,band->Def->NJ,TD2GDAL[band->Def->Type],0,0,NULL)==CE_Failure) {
            return(TCL_ERROR);
         }
         nc++;
      }
   }
   band->File=file;

   return(TCL_OK);
}

/*----------------------------------------------------------------------------
 * Nom      : <GDAL_BandStat>
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
int GDAL_BandStat(Tcl_Interp *Interp,char *Name,int Objc,Tcl_Obj *CONST Objv[]){

   int          i,c,w,h,idx,ex=0,tr=1,i0,i1,b,c0,c1,cnt,s,clamp;
   double       lat,lon,x0,y0,dval,min,max,mean,std,dmin,dmax;
   GDAL_Band   *band;
   Tcl_Obj     *obj,*lst;

   static CONST char *stretchs[] = { "MIN_MAX","PERCENT_CLIP","HISTOGRAM_EQUALIZED","STANDARD_DEV","RANGE",NULL };
   static CONST char *bands[] = { "red","green","blue","alpha",NULL };
   static CONST char *sopt[] = { "-tag","-component","-image","-nodata","-max","-min","-avg","-grid","-gridlat","-gridlon","-gridpoint","-coordpoint",
      "-gridvalue","-coordvalue","-project","-unproject","-extent","-llextent","-histogram","-celldim","-stretch","-approx",NULL };
   enum        opt {  TAG,COMPONENT,IMAGE,NODATA,MAX,MIN,AVG,GRID,GRIDLAT,GRIDLON,GRIDPOINT,COORDPOINT,GRIDVALUE,COORDVALUE,PROJECT,UNPROJECT,EXTENT,LLEXTENT,HISTOGRAM,CELLDIM,STRETCH,APPROX };

   band=GDAL_BandGet(Name);
   if (!band) {
      Tcl_AppendResult(Interp,"\n   GDAL_BandStat: Band name unknown: \"",Name,"\"",(char *)NULL);
      return(TCL_ERROR);
   }

   for(i=0;i<Objc;i++) {

      if (Tcl_GetIndexFromObj(Interp,Objv[i],sopt,"option",TCL_EXACT,&idx)!=TCL_OK) {
         return(TCL_ERROR);
      }

      switch ((enum opt)idx) {

         case TAG:
            if (Objc==1) {
               if (band->Tag) {
                  Tcl_SetObjResult(Interp,band->Tag);
               }
            } else {
               if (band->Tag) {
                  Tcl_DecrRefCount(band->Tag);
               }
               band->Tag=Objv[++i];
               Tcl_IncrRefCount(band->Tag);
            }
            break;

         case NODATA:
            if (Objc==1) {
               Tcl_SetObjResult(Interp,Tcl_NewDoubleObj(band->Def->NoData));
            } else {
               if (band->Stat) {
                   Data_StatFree(band->Stat);
                   band->Stat=NULL;
               }
               Tcl_GetDoubleFromObj(Interp,Objv[++i],&band->Def->NoData);
               for (c=0;c<band->Def->NC;c++) {
                  if (band->Band[c])
                     GDALSetRasterNoDataValue(band->Band[c],band->Def->NoData);
               }
            }
            break;

         case COMPONENT:
            if (Objc==1) {
               Tcl_SetObjResult(Interp,Tcl_NewIntObj(band->Def->NC));
            }
            break;

         case IMAGE:
            if(Objc!=2) {
               Tcl_WrongNumArgs(Interp,2,Objv,"image");
               return(TCL_ERROR);
            }
            return GDALBand_GetImage(Interp,band,Tcl_GetString(Objv[++i]));
            break;

         case APPROX:
            if (Objc==1) {
               Tcl_SetObjResult(Interp,Tcl_NewBooleanObj(band->Approx));
            } else {
               Tcl_GetBooleanFromObj(Interp,Objv[++i],&band->Approx);
            }
            break;

         case HISTOGRAM:
            if (Objc!=2 && Objc!=6) {
               Tcl_WrongNumArgs(Interp,2,Objv,"band index [min max bin]");
               return(TCL_ERROR);
            } else {
               b=0;
               Tcl_GetIntFromObj(Interp,Objv[++i],&b);
               // Force 2nd component on LUMINANCE_ALPHA composition
               if (b==3 && band->Def->NC==2) b=1;
               if (c<band->Def->NC) {
                  if (!band->Stat)
                     GDAL_BandGetStat(band);
                  min=band->Stat[b].Min;
                  max=band->Stat[b].Max;
                  h=256;
                  if (Objc>2) {
                     Tcl_GetDoubleFromObj(Interp,Objv[++i],&min);
                     Tcl_GetDoubleFromObj(Interp,Objv[++i],&max);
                     Tcl_GetIntFromObj(Interp,Objv[++i],&h);
                  }
                  if (!GDAL_BandGetHisto(band,b,h,min,max)) {
                     Tcl_AppendResult(Interp,"\n   GDAL_BandStat: Unable to allocate histogram array",(char*)NULL);
                     return(TCL_ERROR);
                   }

                  obj=Tcl_NewListObj(0,NULL);
                  for(c=0;c<h;c++) {
                     Tcl_ListObjAppendElement(Interp,obj,Tcl_NewWideIntObj(band->Stat[b].Histo[c]));
                  }
                  Tcl_SetObjResult(Interp,obj);
               }
            }
            break;

         case STRETCH:
            if (Objc<2) {
               Tcl_WrongNumArgs(Interp,2,Objv,"index type [params]");
               return(TCL_ERROR);
            } else {
               if (!band->Spec || !band->Spec->Map) { 
                  Tcl_AppendResult(Interp,"\n   GDAL_BandStat: Band must have a colormap assigned",(char*)NULL);
                  return(TCL_ERROR);
               }

               if (Tcl_GetIntFromObj(Interp,Objv[++i],&b)==TCL_ERROR) {
                  if (Tcl_GetIndexFromObj(Interp,Objv[i],bands,"type",TCL_EXACT,&b)!=TCL_OK) {
                     return(TCL_ERROR);
                  }
               }

               // Force 2nd component on LUMINANCE_ALPHA composition
               if (b==3 && band->Def->NC==2) b=1;

               if (b<0 || b>band->Def->NC) {
                  Tcl_AppendResult(Interp,"\n   GDAL_BandStat: Invalid band index",(char*)NULL);
                  return(TCL_ERROR);
               }

               obj=Tcl_NewListObj(0,NULL);

               if (Objc==2) {
                  Tcl_ListObjAppendElement(Interp,obj,Tcl_NewDoubleObj(band->Spec->Map->Min[b]));
                  Tcl_ListObjAppendElement(Interp,obj,Tcl_NewDoubleObj(band->Spec->Map->Max[b]));
               } else {

                  if (Tcl_GetIndexFromObj(Interp,Objv[++i],stretchs,"type",TCL_EXACT,&s)!=TCL_OK) {
                     return(TCL_ERROR);
                  }

                  switch(s) {

                     case 0: // MIN_MAX
                        if (Objc!=3) {
                           Tcl_WrongNumArgs(Interp,2,Objv,"band index type");
                           return(TCL_ERROR);
                        }
                        GDALGetRasterStatistics(band->Band[b],band->Approx,TRUE,&min,&max,&mean,&std);
                        break;

                     case 1: // PERCENT_CLIP
                        if (Objc!=5) {
                           Tcl_WrongNumArgs(Interp,2,Objv,"band index type percent_from percent_to");
                           return(TCL_ERROR);
                        }
                        if (!band->Stat)
                           GDAL_BandGetStat(band);
                        min=band->Stat[b].Min;
                        max=band->Stat[b].Max;
                        h=1024;
                        dval=(band->Stat[b].Max-band->Stat[b].Min)/h;

                        if (!GDAL_BandGetHisto(band,b,h,min,max)) {
                           Tcl_AppendResult(Interp,"\n   GDAL_BandStat: Unable to allocate histogram array",(char*)NULL);
                           return(TCL_ERROR);
                        }

                        Tcl_GetDoubleFromObj(Interp,Objv[++i],&min);
                        Tcl_GetDoubleFromObj(Interp,Objv[++i],&max);

                        c0=FSIZE2D(band->Def)*(min/100.0);
                        c1=FSIZE2D(band->Def)*(max/100.0);
                        cnt=0;
                        i0=-1;
                        i1=-1;
                        for(c=0;c<h;c++) {
                           if (cnt<=c0)
                              i0=c;

                           cnt+=band->Stat[b].Histo[c];

                           if (cnt<=c1)
                              i1=c;
                           else
                              break;
                        }

                        if (i0==i1) {
                           i0=0;
                           i1=h-1;
                        }
                        min=band->Stat[b].Min+i0*dval;
                        max=band->Stat[b].Min+i1*dval;

                        break;

                     case 2: // HISTOGRAM-EQUALIZED
                        if (Objc!=3 && Objc!=5) {
                           Tcl_WrongNumArgs(Interp,2,Objv,"band index type [min] [max]");
                           return(TCL_ERROR);
                        }
                        if (!band->Stat)
                           GDAL_BandGetStat(band);
                        min=band->Stat[b].Min;
                        max=band->Stat[b].Max;
                        h=band->Spec->Map->NbPixels;

                       if (Objc==4) 
                           Tcl_GetDoubleFromObj(Interp,Objv[++i],&min);
                        if (Objc==5) 
                           Tcl_GetDoubleFromObj(Interp,Objv[++i],&max);

                        if (!GDAL_BandGetHisto(band,b,h,min,max)) {
                           Tcl_AppendResult(Interp,"\n   GDAL_BandStat: Unable to allocate histogram array",(char*)NULL);
                           return(TCL_ERROR);
                        }

                        band->Spec->Map->Type[0][b]='\0';
                        
                        // Get stretching range
                        i0=band->Stat[b].Histo[0];
                        i1=0;
                        for(c=0;c<h;c++) {
                           i1+=band->Stat[b].Histo[c];
                        }
                        i1-=i0;

                        // Apply equalized stretching
                        cnt=0;
                        for(c=0;c<h;c++) {
                           cnt+=band->Stat[b].Histo[c];                             
                           band->Spec->Map->Curve[c][0]=band->Spec->Map->Curve[c][1]=band->Spec->Map->Curve[c][2]=band->Spec->Map->Curve[c][3]=(float)(cnt-i0)*(h-1)/i1;
                        }
                        CMap_RatioDefine(band->Spec->Map);
                        break;

                     case 3: // STANDARD_DEV
                        if (Objc!=4 && Objc!=5) {
                           Tcl_WrongNumArgs(Interp,2,Objv,"band index type nb_stdev [clamp]");
                           return(TCL_ERROR);
                        }
                        Tcl_GetDoubleFromObj(Interp,Objv[++i],&dval);
                        
                        clamp=0;
                        if (Objc==5)
                           Tcl_GetBooleanFromObj(Interp,Objv[++i],&clamp);

                        GDALGetRasterStatistics(band->Band[b],band->Approx,TRUE,&dmin,&dmax,&mean,&std);

                        min=mean-dval*std;
                        max=mean+dval*std;
                        
                        if (clamp) {
                           min=min<dmin?dmin:min;
                           max=max>dmax?dmax:max;
                        }
                        break;

                     case 4: // RANGE
                        if (Objc!=5) {
                           Tcl_WrongNumArgs(Interp,2,Objv,"band index type min max");
                           return(TCL_ERROR);
                        }
                        Tcl_GetDoubleFromObj(Interp,Objv[++i],&min);
                        Tcl_GetDoubleFromObj(Interp,Objv[++i],&max);
                        break;                       
                  }

                  // In case of LUMINANCE or LUMINANCE_ALPHA, set all the same.
                  if (band->Def->NC<3) {
                     band->Spec->Map->Min[0]=band->Spec->Map->Min[1]=band->Spec->Map->Min[2]=band->Spec->Map->Min[3]=min;
                     band->Spec->Map->Max[0]=band->Spec->Map->Max[1]=band->Spec->Map->Max[2]=band->Spec->Map->Max[3]=max;
                  } else {
                     band->Spec->Map->Min[b]=min;
                     band->Spec->Map->Max[b]=max;
                  }
                  Tcl_ListObjAppendElement(Interp,obj,Tcl_NewDoubleObj(min));
                  Tcl_ListObjAppendElement(Interp,obj,Tcl_NewDoubleObj(max));
               }
               Tcl_SetObjResult(Interp,obj);
            }
            break;

         case MAX:
            if (!band->Stat)
               GDAL_BandGetStat(band);

            if (Objc==1) {
               lst=Tcl_NewListObj(0,NULL);
               for(c=0;c<band->Def->NC;c++) {
                  obj=Tcl_NewListObj(0,NULL);
                  Tcl_ListObjAppendElement(Interp,obj,Data_Val2Obj(band->Def,band->Stat[c].Max));
                  Tcl_ListObjAppendElement(Interp,obj,Tcl_NewDoubleObj(band->Stat[c].MaxLoc.Lat));
                  Tcl_ListObjAppendElement(Interp,obj,Tcl_NewDoubleObj(band->Stat[c].MaxLoc.Lon));
                  Tcl_ListObjAppendElement(Interp,obj,Tcl_NewDoubleObj(band->Stat[c].MaxLoc.Elev));
                  Tcl_ListObjAppendElement(Interp,lst,obj);
               }
               Tcl_SetObjResult(Interp,lst);
            } else if (Objc>=2) {
               Tcl_GetIntFromObj(Interp,Objv[++i],&c);

               // Force 2nd component on LUMINANCE_ALPHA composition
               if (c==3 && band->Def->NC==2) c=1;
               if (c<band->Def->NC) {
                  if (Objc==2) {
                     obj=Tcl_NewListObj(0,NULL);
                     Tcl_ListObjAppendElement(Interp,obj,Data_Val2Obj(band->Def,band->Stat[c].Max));
                     Tcl_ListObjAppendElement(Interp,obj,Tcl_NewDoubleObj(band->Stat[c].MaxLoc.Lat));
                     Tcl_ListObjAppendElement(Interp,obj,Tcl_NewDoubleObj(band->Stat[c].MaxLoc.Lon));
                     Tcl_ListObjAppendElement(Interp,obj,Tcl_NewDoubleObj(band->Stat[c].MaxLoc.Elev));
                     Tcl_SetObjResult(Interp,obj);
                  } else if (Objc==3) {
                     Tcl_GetDoubleFromObj(Interp,Objv[++i],&band->Stat[c].Max);
                  }
               }
            }
            break;

         case MIN:
            if (!band->Stat)
               GDAL_BandGetStat(band);

            if (Objc==1) {
               lst=Tcl_NewListObj(0,NULL);
               for(c=0;c<band->Def->NC;c++) {
                  obj=Tcl_NewListObj(0,NULL);
                  Tcl_ListObjAppendElement(Interp,obj,Data_Val2Obj(band->Def,band->Stat[c].Min));
                  Tcl_ListObjAppendElement(Interp,obj,Tcl_NewDoubleObj(band->Stat[c].MinLoc.Lat));
                  Tcl_ListObjAppendElement(Interp,obj,Tcl_NewDoubleObj(band->Stat[c].MinLoc.Lon));
                  Tcl_ListObjAppendElement(Interp,obj,Tcl_NewDoubleObj(band->Stat[c].MinLoc.Elev));
                  Tcl_ListObjAppendElement(Interp,lst,obj);
               }
               Tcl_SetObjResult(Interp,lst);
            } else if (Objc>=2) {
               Tcl_GetIntFromObj(Interp,Objv[++i],&c);

               // Force 2nd component on LUMINANCE_ALPHA composition
               if (c==3 && band->Def->NC==2) c=1;
               if (c<band->Def->NC) {
                  if (Objc==2) {
                     obj=Tcl_NewListObj(0,NULL);
                     Tcl_ListObjAppendElement(Interp,obj,Data_Val2Obj(band->Def,band->Stat[c].Min));
                     Tcl_ListObjAppendElement(Interp,obj,Tcl_NewDoubleObj(band->Stat[c].MinLoc.Lat));
                     Tcl_ListObjAppendElement(Interp,obj,Tcl_NewDoubleObj(band->Stat[c].MinLoc.Lon));
                     Tcl_ListObjAppendElement(Interp,obj,Tcl_NewDoubleObj(band->Stat[c].MinLoc.Elev));
                     Tcl_SetObjResult(Interp,obj);
                  } else if (Objc==3) {
                     Tcl_GetDoubleFromObj(Interp,Objv[++i],&band->Stat[c].Min);
                  }
               }
            }
            break;

         case AVG:
            if (!band->Stat)
               GDAL_BandGetStat(band);

            if (Objc==1) {
               lst=Tcl_NewListObj(0,NULL);
               for(c=0;c<band->Def->NC;c++) {
                  Tcl_ListObjAppendElement(Interp,lst,Tcl_NewDoubleObj(band->Stat[c].Avg));
               }
               Tcl_SetObjResult(Interp,lst);
            }
            break;

         case GRID:
            if (!band->GRef) {
               Tcl_AppendResult(Interp,"GDAL_BandStat: No geographic reference defined",(char*)NULL);
               return(TCL_ERROR);
            }
            obj=Tcl_NewListObj(0,NULL);
            for (w=0;w<band->Def->NI;w++) {
               for (h=0;h<band->Def->NJ;h++) {
                  band->GRef->Project(band->GRef,w,h,&lat,&lon,0,1);
                  Tcl_ListObjAppendElement(Interp,obj,Tcl_NewDoubleObj(lat));
                  Tcl_ListObjAppendElement(Interp,obj,Tcl_NewDoubleObj(lon));
               }
            }
            Tcl_SetObjResult(Interp,obj);
            break;

         case GRIDLAT:
            if (!band->GRef) {
               Tcl_AppendResult(Interp,"GDAL_BandStat: No geographic reference defined",(char*)NULL);
               return(TCL_ERROR);
            }
             obj=Tcl_NewListObj(0,NULL);
            for (w=0;w<band->Def->NI;w++) {
               for (h=0;h<band->Def->NJ;h++) {
                  band->GRef->Project(band->GRef,w,h,&lat,&lon,0,1);
                  Tcl_ListObjAppendElement(Interp,obj,Tcl_NewDoubleObj(lat));
               }
            }
            Tcl_SetObjResult(Interp,obj);
            break;

         case GRIDLON:
            if (!band->GRef) {
               Tcl_AppendResult(Interp,"GDAL_BandStat: No geographic reference defined",(char*)NULL);
               return(TCL_ERROR);
            }
            obj=Tcl_NewListObj(0,NULL);
            for (w=0;w<band->Def->NI;w++) {
               for (h=0;h<band->Def->NJ;h++) {
                  band->GRef->Project(band->GRef,w,h,&lat,&lon,0,1);
                  Tcl_ListObjAppendElement(Interp,obj,Tcl_NewDoubleObj(lon));
               }
            }
            Tcl_SetObjResult(Interp,obj);
            break;

         case PROJECT:
            tr=0;
            ex=1;
         case GRIDPOINT:
            if (!band->GRef) {
               Tcl_AppendResult(Interp,"GDAL_BandStat: No geographic reference defined",(char*)NULL);
               return(TCL_ERROR);
            }
            obj=Tcl_NewListObj(0,NULL);
            Tcl_GetDoubleFromObj(Interp,Objv[++i],&x0);
            Tcl_GetDoubleFromObj(Interp,Objv[++i],&y0);
            if (Objc==4) {
               Tcl_GetBooleanFromObj(Interp,Objv[++i],&ex);
            }
            band->GRef->Project(band->GRef,x0,y0,&lat,&lon,ex,tr);
            Tcl_ListObjAppendElement(Interp,obj,Tcl_NewDoubleObj(lat));
            Tcl_ListObjAppendElement(Interp,obj,Tcl_NewDoubleObj(lon));
            Tcl_SetObjResult(Interp,obj);
            break;

         case UNPROJECT:
            tr=0;
            ex=1;
         case COORDPOINT:
            if (!band->GRef) {
               Tcl_AppendResult(Interp,"GDAL_BandStat: No geographic reference defined",(char*)NULL);
               return(TCL_ERROR);
            }
            obj=Tcl_NewListObj(0,NULL);
            Tcl_GetDoubleFromObj(Interp,Objv[++i],&lat);
            Tcl_GetDoubleFromObj(Interp,Objv[++i],&lon);
            if (Objc==4) {
               Tcl_GetBooleanFromObj(Interp,Objv[++i],&ex);
            }
            band->GRef->UnProject(band->GRef,&x0,&y0,lat,lon,ex,tr);
            Tcl_ListObjAppendElement(Interp,obj,Tcl_NewDoubleObj(x0));
            Tcl_ListObjAppendElement(Interp,obj,Tcl_NewDoubleObj(y0));
            Tcl_SetObjResult(Interp,obj);
            break;

         case EXTENT:
            obj=Tcl_NewListObj(0,NULL);
            Tcl_ListObjAppendElement(Interp,obj,Tcl_NewDoubleObj(0));
            Tcl_ListObjAppendElement(Interp,obj,Tcl_NewDoubleObj(0));
            Tcl_ListObjAppendElement(Interp,obj,Tcl_NewDoubleObj(band->Def->NI-1));
            Tcl_ListObjAppendElement(Interp,obj,Tcl_NewDoubleObj(band->Def->NJ-1));
            Tcl_SetObjResult(Interp,obj);
            break;

         case LLEXTENT:
            if (!band->GRef) {
               Tcl_AppendResult(Interp,"GDAL_BandStat: No geographic reference defined",(char*)NULL);
               return(TCL_ERROR);
            }
            /*If not calculated yet, get latlon extent*/
            if (band->GRef->LLExtent.MinY==1e32) {
               GeoRef_Limits(band->GRef,&band->GRef->LLExtent.MinY,&band->GRef->LLExtent.MinX,&band->GRef->LLExtent.MaxY,&band->GRef->LLExtent.MaxX);
            }
            obj=Tcl_NewListObj(0,NULL);
            Tcl_ListObjAppendElement(Interp,obj,Tcl_NewDoubleObj(band->GRef->LLExtent.MinY));
            Tcl_ListObjAppendElement(Interp,obj,Tcl_NewDoubleObj(band->GRef->LLExtent.MinX));
            Tcl_ListObjAppendElement(Interp,obj,Tcl_NewDoubleObj(band->GRef->LLExtent.MaxY));
            Tcl_ListObjAppendElement(Interp,obj,Tcl_NewDoubleObj(band->GRef->LLExtent.MaxX));
            Tcl_SetObjResult(Interp,obj);
            break;

         case GRIDVALUE:
            if (Objc==1) {
               obj=Tcl_NewListObj(0,NULL);
               for (h=0;h<band->Def->NJ;h++) {
                  for (w=0;w<band->Def->NI*band->Def->NJ;w++) {
                     if (band->Def->Data[0]) {
                        Tcl_ListObjAppendElement(Interp,obj,Data_AppendValueObj(Interp,band->Def,w,h));
                     } else {
                        Tcl_ListObjAppendElement(Interp,obj,GeoTex_AppendValueObj(Interp,&band->Tex,w,h));
                     }
                  }
               }
               Tcl_SetObjResult(Interp,obj);
            } else {

               Tcl_GetDoubleFromObj(Interp,Objv[++i],&x0);
               Tcl_GetDoubleFromObj(Interp,Objv[++i],&y0);

               x0=ROUND(x0);
               y0=ROUND(y0);

               if (Objc>3) {
                  if (Objc>4) {
                     Tcl_GetIntFromObj(Interp,Objv[++i],&h);
                  } else {
                     h=0;
                  }
                  Tcl_GetDoubleFromObj(Interp,Objv[++i],&dval);
                  Def_Set(band->Def,h,FIDX2D(band->Def,(int)x0,(int)y0),dval);
               } else {
                  if (band->Def->Data[0]) {
                     Tcl_SetObjResult(Interp,Data_AppendValueObj(Interp,band->Def,x0,y0));
                  } else {
                     Tcl_SetObjResult(Interp,GeoTex_AppendValueObj(Interp,&band->Tex,x0,y0));
                  }
              }
               if (band->Stat) {
                   Data_StatFree(band->Stat);
                   band->Stat=NULL;
               }
            }
            break;

         case COORDVALUE:
            if (!band->GRef) {
               Tcl_AppendResult(Interp,"GDAL_BandStat: No geographic reference defined",(char*)NULL);
               return(TCL_ERROR);
            }
            Tcl_GetDoubleFromObj(Interp,Objv[++i],&lat);
            Tcl_GetDoubleFromObj(Interp,Objv[++i],&lon);

            if (lat==-999.0) {
                Tcl_SetObjResult(Interp,Tcl_NewStringObj("-",-1));
                return(TCL_OK);
            }
            if (band->GRef->UnProject(band->GRef,&x0,&y0,lat,lon,0,1)) {
               DEFCLAMP(band->Def,x0,y0);
               x0=ROUND(x0);
               y0=ROUND(y0);

               if (Objc>3) {
                  Tcl_GetIntFromObj(Interp,Objv[++i],&h);
               } else {
                  h=0;
               }

               if (Objc>3) {
                  Tcl_GetDoubleFromObj(Interp,Objv[++i],&dval);
                  Def_Set(band->Def,h,FIDX2D(band->Def,(int)x0,(int)y0),dval);
                  if (band->Stat) {
                     Data_StatFree(band->Stat);
                     band->Stat=NULL;
                  }
               } else {
                  if (band->Def->Data[0]) {
                     Tcl_SetObjResult(Interp,Data_AppendValueObj(Interp,band->Def,x0,y0));
                  } else {
                     Tcl_SetObjResult(Interp,GeoTex_AppendValueObj(Interp,&band->Tex,x0,y0));
                  }
               }
            } else {
               Tcl_SetObjResult(Interp,Tcl_NewStringObj("-",-1));
               return(TCL_OK);
            }
            break;

         case CELLDIM:
            if (Objc==1) {
               Tcl_SetObjResult(Interp,Tcl_NewIntObj(band->Def->CellDim));
            } else {
               Tcl_GetIntFromObj(Interp,Objv[++i],&band->Def->CellDim);
            }
            break;
      }
   }
   return TCL_OK;
}

/*----------------------------------------------------------------------------
 * Nom      : <GDAL_BandDefine>
 * Creation : Avril 2004 - J.P. Gauthier - CMC/CMOE
 *
 * But      : Effectue la configuration des parametres.
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
int GDAL_BandDefine(Tcl_Interp *Interp,char *Name,int Objc,Tcl_Obj *CONST Objv[]){

   int         i,j,idx,nidx,order=1,c;
   double      tra[6],inv[6],*tm=NULL,*im=NULL;
   GDAL_Band  *band,*xband,*yband;
   TGeoRef    *ref;
   Tcl_Obj    *obj,*lst;

   static CONST char *sopt[] = { "-date","-georef","-projection","-transform","-invtransform","-indexed","-colorinterp","-gcps","-width",
                                 "-height","-nb","-type","-positional","-fid",NULL };
   enum        opt {  DATE,GEOREF,PROJECTION,TRANSFORM,INVTRANSFORM,INDEXED,COLORINTERP,GCPS,WIDTH,HEIGHT,NB,TYPE,POSITIONAL,FID };

   band=GDAL_BandGet(Name);
   if (!band) {
      Tcl_AppendResult(Interp,"\n   GDAL_BandDefine: Band name unknown: \"",Name,"\"",(char *)NULL);
      return TCL_ERROR;
   }

   for(i=0;i<Objc;i++) {

      if (Tcl_GetIndexFromObj(Interp,Objv[i],sopt,"option",TCL_EXACT,&idx)!=TCL_OK) {
         return TCL_ERROR;
      }

      switch ((enum opt)idx) {

         case DATE:
            if (Objc==1) {
               Tcl_SetObjResult(Interp,Tcl_NewLongObj(band->Date));
            } else {
               Tcl_GetLongFromObj(Interp,Objv[++i],&band->Date);
            }
            break;

         case FID:
            if (Objc==1) {
               Tcl_SetObjResult(Interp,Tcl_NewStringObj(band->File->Id,-1));
            }
            break;

         case WIDTH:
            if (Objc==1) {
               Tcl_SetObjResult(Interp,Tcl_NewIntObj(band->Def->NI));
            } else {
               Tcl_GetIntFromObj(Interp,Objv[++i],&band->Def->NI);
            }
            break;

         case HEIGHT:
            if (Objc==1) {
               Tcl_SetObjResult(Interp,Tcl_NewIntObj(band->Def->NJ));
            } else {
               Tcl_GetIntFromObj(Interp,Objv[++i],&band->Def->NJ);
            }
            break;

         case NB:
            if (Objc==1) {
               Tcl_SetObjResult(Interp,Tcl_NewIntObj(band->Def->NC));
            } else {
               Tcl_GetIntFromObj(Interp,Objv[++i],&band->Def->NC);
            }
            break;

         case TYPE:
            if (Objc==1) {
               Tcl_SetObjResult(Interp,Tcl_NewStringObj(GDALGetDataTypeName(TD2GDAL[band->Def->Type]),-1));
            } else {
//               band->Type=GDALGetDataTypeByName(Tcl_GetString(Objv[++i]));
            }
            break;

         case INDEXED:
            if (Objc==1) {
               Tcl_SetObjResult(Interp,Tcl_NewBooleanObj(band->Tex.Indexed));
            } else {
               Tcl_GetBooleanFromObj(Interp,Objv[++i],&band->Tex.Indexed);
            }
            break;

         case COLORINTERP:
            if (Objc==1) {
               lst=Tcl_NewListObj(0,NULL);
               for(c=0;c<band->Def->NC;c++) {
                  Tcl_ListObjAppendElement(Interp,lst,Tcl_NewStringObj(GDALGetColorInterpretationName(GDALGetRasterColorInterpretation(band->Band[c])),-1));
               }
               Tcl_SetObjResult(Interp,lst);
            } else if (Objc==2) {
               Tcl_GetIntFromObj(Interp,Objv[++i],&c);
               if (c<band->Def->NC) {
                  Tcl_SetObjResult(Interp,Tcl_NewStringObj(GDALGetColorInterpretationName(GDALGetRasterColorInterpretation(band->Band[c])),-1));
               }
            } else if (Objc==3) {
               Tcl_GetIntFromObj(Interp,Objv[++i],&c);
               if (c<band->Def->NC) {
                  GDALSetRasterColorInterpretation(band->Band[c],GDALGetColorInterpretationByName(Tcl_GetString(Objv[++i])));
               } else {
                  Tcl_AppendResult(Interp,"\n   GDAL_BandDefine: Invalid band index",(char*)NULL);
                  return(TCL_ERROR);
               }
            }
            break;

         case GCPS:
            if (Objc==1) {
               obj=Tcl_NewListObj(0,NULL);
               for(j=0;j<band->NbGCPs;j++) {
                  lst=Tcl_NewListObj(0,NULL);
                  Tcl_ListObjAppendElement(Interp,lst,Tcl_NewDoubleObj(band->GCPs[j].dfGCPPixel));
                  Tcl_ListObjAppendElement(Interp,lst,Tcl_NewDoubleObj(band->GCPs[j].dfGCPLine));
                  Tcl_ListObjAppendElement(Interp,lst,Tcl_NewDoubleObj(band->GCPs[j].dfGCPX));
                  Tcl_ListObjAppendElement(Interp,lst,Tcl_NewDoubleObj(band->GCPs[j].dfGCPY));
                  Tcl_ListObjAppendElement(Interp,obj,lst);
               }
               Tcl_SetObjResult(Interp,obj);
            } else {
               if (Objc!=2 && Objc!=3) {
                  Tcl_WrongNumArgs(Interp,1,Objv,"{ gcps } [Order]");
                  return(TCL_ERROR);
               }

               if (band->GCPs) {
                  free(band->GCPs);
                  band->GCPs=NULL;
               }
               Tcl_ListObjLength(Interp,Objv[++i],&band->NbGCPs);
               band->GCPs=(GDAL_GCP*)malloc(band->NbGCPs*sizeof(GDAL_GCP));

               for(j=0;j<band->NbGCPs;j++) {
                  Tcl_ListObjIndex(Interp,Objv[i],j,&lst);
                  if (Tcl_ListObjLength(Interp,lst,&nidx)==TCL_ERROR || nidx!=4) {
                     Tcl_AppendResult(Interp,"\n   GDAL_BandDefine: Invalid control point items, must be { pixel line x y }",(char*)NULL);
                     return(TCL_ERROR);
                  }
                  Tcl_ListObjIndex(Interp,lst,0,&obj);
                  Tcl_GetDoubleFromObj(Interp,obj,&band->GCPs[j].dfGCPPixel);
                  Tcl_ListObjIndex(Interp,lst,1,&obj);
                  Tcl_GetDoubleFromObj(Interp,obj,&band->GCPs[j].dfGCPLine);
                  Tcl_ListObjIndex(Interp,lst,2,&obj);
                  Tcl_GetDoubleFromObj(Interp,obj,&band->GCPs[j].dfGCPX);
                  Tcl_ListObjIndex(Interp,lst,3,&obj);
                  Tcl_GetDoubleFromObj(Interp,obj,&band->GCPs[j].dfGCPY);
                  band->GCPs[j].dfGCPZ=0.0;
               }

               if (Objc==3) {
                  Tcl_GetBooleanFromObj(Interp,Objv[++i],&order);
               }

               if (band->GRef->GCPTransform) {
                  GDALDestroyGCPTransformer(band->GRef->GCPTransform);
                  band->GRef->GCPTransform=NULL;
               }
               if (band->GRef->TPSTransform) {
                  GDALDestroyTPSTransformer(band->GRef->TPSTransform);
                  band->GRef->TPSTransform=NULL;
               }

               switch(order) {
                  case 0:
                     if (!band->GRef->Transform) {
                        band->GRef->Transform=(double*)calloc(6,sizeof(double));
                     }
                     if (!band->GRef->InvTransform) {
                        band->GRef->InvTransform=(double*)calloc(6,sizeof(double));
                     }
                     if (!(GDALGCPsToGeoTransform(band->NbGCPs,band->GCPs,band->GRef->Transform,1))) {
                        Tcl_AppendResult(Interp,"\n   GDAL_BandDefine: () Unable to fit control points",(char*)NULL);
                        return(TCL_ERROR);
                     }
                     c=GDALInvGeoTransform(band->GRef->Transform,band->GRef->InvTransform);
                     break;
                  case 1:
                  case 2:
                  case 3:
                     if (band->GRef->Transform) {
                        free(band->GRef->Transform);
                        band->GRef->Transform=NULL;
                     }
                     if (band->GRef->InvTransform) {
                        free(band->GRef->InvTransform);
                        band->GRef->InvTransform=NULL;
                     }
                     if (!(band->GRef->GCPTransform=(void*)GDALCreateGCPTransformer(band->NbGCPs,band->GCPs,order,FALSE))) {
                        Tcl_AppendResult(Interp,"\n   GDAL_BandDefine: (GPC) Unable to fit control points",(char*)NULL);
                        return(TCL_ERROR);
                     }
                     break;

                  case 4:
                     if (band->GRef->Transform) {
                        free(band->GRef->Transform);
                        band->GRef->Transform=NULL;
                     }
                     if (band->GRef->InvTransform) {
                        free(band->GRef->InvTransform);
                        band->GRef->InvTransform=NULL;
                     }
                     if (!(band->GRef->TPSTransform=(void*)GDALCreateTPSTransformer(band->NbGCPs,band->GCPs,FALSE))) {
                        Tcl_AppendResult(Interp,"\n   GDAL_BandDefine: (TPS) Unable to fit control points",(char*)NULL);
                        return(TCL_ERROR);
                     }
                     break;
               }
               GeoTex_Signal(&band->Tex,GEOTEX_CLRCOO);
            }
            break;

         case GEOREF:
            if (Objc==1) {
               if (band->GRef)
                  Tcl_SetObjResult(Interp,Tcl_NewStringObj(band->GRef->Name,-1));
            } else {
               ref=GeoRef_Get(Tcl_GetString(Objv[++i]));
               if (!ref) {
                  Tcl_AppendResult(Interp,"\n   GDAL_BandDefine: Georef name unknown: \"",Tcl_GetString(Objv[i]),"\"",(char *)NULL);
                  return(TCL_ERROR);
               }
               if (ref!=band->GRef) {
                  if (band->GRef)
                     GeoRef_Destroy(Interp,band->GRef->Name);
                  band->GRef=ref;
                  GeoRef_Incr(band->GRef);
                  GeoRef_Size(ref,ref->BD,ref->BD,band->Def->NI-1-ref->BD,band->Def->NJ-1-ref->BD,ref->BD);
                  GeoRef_Qualify(ref);
                  GeoTex_Signal(&band->Tex,GEOTEX_CLRCOO);
               }
            }
            break;

         case POSITIONAL:
            if (Objc<3) {
               Tcl_WrongNumArgs(Interp,0,Objv,"XBand YBand");
               return(TCL_ERROR);
            } else {
               xband=GDAL_BandGet(Tcl_GetString(Objv[++i]));
               if (!xband) {
                  Tcl_AppendResult(Interp,"invalid X Axis field :",Tcl_GetString(Objv[i]),(char*)NULL);
                  return(TCL_ERROR);
               }
               yband=GDAL_BandGet(Tcl_GetString(Objv[++i]));
               if (!yband) {
                  Tcl_AppendResult(Interp,"invalid Y Axis field :",Tcl_GetString(Objv[i]),(char*)NULL);
                  return(TCL_ERROR);
               }
               if (!GeoRef_Positional(band->GRef,xband->Def,yband->Def)) {
                  Tcl_AppendResult(Interp,"unable to initialize positional data",(char*)NULL);
                  return(TCL_ERROR);
               }
               if (band->Stat) { Data_StatFree(band->Stat); band->Stat=NULL; }

               GeoRef_Qualify(band->GRef);
               GDAL_BandClean(band,1,1,1);
               band->GPos=GeoPos_Find(band->GRef,band->ZRef);
            }
            break;

         case PROJECTION:
             if (Objc==1) {
               if (band->GRef && band->GRef->String)
                  Tcl_SetObjResult(Interp,Tcl_NewStringObj(band->GRef->String,-1));
            } else {
               ++i;
               if (band->GRef && !band->GRef->String && strlen(Tcl_GetString(Objv[i]))==0)
                  break;

               if (band->GRef && band->GRef->String && strlen(band->GRef->String)==strlen(Tcl_GetString(Objv[i])) && strcmp(Tcl_GetString(Objv[i]),band->GRef->String)==0) {
               } else {
                  ref=band->GRef;
                  if (ref) {
                     GeoRef_WKTSet(ref,Tcl_GetString(Objv[i]),ref->Transform,ref->InvTransform,NULL);
//                     band->GRef=GeoRef_Find(GeoRef_WKTSetup(band->Def->NI,band->Def->NJ,LVL_UNDEF,NULL,ref->Grid,0,0,0,0,Tcl_GetString(Objv[i]),ref->Transform,ref->InvTransform,NULL));
//                     GeoRef_Destroy(Interp,ref->Name);
                  } else {
                     band->GRef=GeoRef_Find(GeoRef_WKTSetup(band->Def->NI,band->Def->NJ,NULL,0,0,0,0,Tcl_GetString(Objv[i]),NULL,NULL,NULL));
                  }
                  GeoTex_Signal(&band->Tex,GEOTEX_CLRCOO);
               }
            }
            break;

         case TRANSFORM:
            if (Objc==1) {
               if (band->GRef && band->GRef->Transform) {
                  obj=Tcl_NewListObj(0,NULL);
                  for(j=0;j<6;j++) {
                     Tcl_ListObjAppendElement(Interp,obj,Tcl_NewDoubleObj(band->GRef->Transform[j]));
                  }
                  Tcl_SetObjResult(Interp,obj);
               }
            } else {
               if (Tcl_ListObjLength(Interp,Objv[++i],&nidx)==TCL_ERROR) {
                  return(TCL_ERROR);
               }
               if (nidx==0)
                  break;

               if (nidx!=0) {
                  if (nidx!=6) {
                     Tcl_AppendResult(Interp,"\n   GDAL_BandDefine: Invalid number of transform element, must be 6 \"",(char*)NULL);
                     return TCL_ERROR;
                  }
                  for(j=0;j<6;j++) {
                     Tcl_ListObjIndex(Interp,Objv[i],j,&obj);
                     Tcl_GetDoubleFromObj(Interp,obj,&tra[j]);
                  }
                  tm=tra;
                  if (!GDALInvGeoTransform(tra,inv)) {
                     App_Log(WARNING,"%s: Unable to generate the inverse transform matrix\n",__func__);
                     im=NULL;
                  } else {
                     im=inv;
                  }
               }
               if (!band->GRef || !band->GRef->Transform || !tm || memcmp(tm,band->GRef->Transform,6*sizeof(double))!=0) {
                  ref=band->GRef;
                  if (ref) {
                     GeoRef_WKTSet(ref,ref->String,tm,im,NULL);
//                     band->GRef=GeoRef_Find(GeoRef_WKTSetup(band->Def->NI,band->Def->NJ,LVL_UNDEF,NULL,ref->Grid,0,0,0,0,ref->String,tm,im,NULL));
//                     GeoRef_Size(band->GRef,0,0,band->Def->NI-1,band->Def->NJ-1,ref->BD);
//                     GeoRef_Destroy(Interp,ref->Name);
                  } else {
                     band->GRef=GeoRef_Find(GeoRef_WKTSetup(band->Def->NI,band->Def->NJ,NULL,0,0,0,0,NULL,tm,im,NULL));
                  }
               }
            }
            break;

         case INVTRANSFORM:
            if (Objc==1) {
               if (band->GRef && band->GRef->InvTransform) {
                  obj=Tcl_NewListObj(0,NULL);
                  for(j=0;j<6;j++) {
                     Tcl_ListObjAppendElement(Interp,obj,Tcl_NewDoubleObj(band->GRef->InvTransform[j]));
                  }
                  Tcl_SetObjResult(Interp,obj);
               }
            } else {
               if (Tcl_ListObjLength(Interp,Objv[++i],&nidx)==TCL_ERROR) {
                  return(TCL_ERROR);
               }
               if (nidx==0)
                  break;

               if (nidx!=0) {
                  if (nidx!=6) {
                     Tcl_AppendResult(Interp,"\n   GDAL_BandDefine: Invalid number of transform element, must be 6 \"",(char*)NULL);
                     return TCL_ERROR;
                  }
                  for(j=0;j<6;j++) {
                     Tcl_ListObjIndex(Interp,Objv[i],j,&obj);
                     Tcl_GetDoubleFromObj(Interp,obj,&inv[j]);
                  }
                  im=inv;
                  if (!GDALInvGeoTransform(inv,tra)) {
                     App_Log(WARNING,"%s: Unable to generate the transform matrix\n",__func__);
                     tm=NULL;
                  } else {
                     tm=tra;
                  }
               }
               if (!band->GRef || !band->GRef->InvTransform || !tm || memcmp(tm,band->GRef->InvTransform,6*sizeof(double))!=0) {
                  ref=band->GRef;
                  if (ref) {
                     GeoRef_WKTSet(ref,ref->String,tm,im,NULL);
//                     band->GRef=GeoRef_Find(GeoRef_WKTSetup(band->Def->NI,band->Def->NJ,LVL_UNDEF,NULL,ref->Grid,0,0,0,0,ref->String,tm,im,NULL));
//                     GeoRef_Size(band->GRef,0,0,band->Def->NI-1,band->Def->NJ-1,ref->BD);
//                     GeoRef_Destroy(Interp,ref->Name);
                  } else {
                     band->GRef=GeoRef_Find(GeoRef_WKTSetup(band->Def->NI,band->Def->NJ,NULL,0,0,0,0,NULL,tm,im,NULL));
                  }
                  GeoTex_Signal(&band->Tex,GEOTEX_CLRCOO);
               }
            }
            break;
      }
   }
   return TCL_OK;
}

/*--------------------------------------------------------------------------------------------------------------
 * Nom          : <GDAL_Pick>
 * Creation     : Juin 2013 J.P. Gauthier - CMC/CMOE
 *
 * But          : Selectionner des features selon un point ou une region.
 *
 * Parametres  :
 *   <Interp>   : Interpreteur Tcl
 *   <Layer>    : Couche
 *   <List>     : Liste de coordonnees latlon (si pas de Geom)
 *
 * Retour       :
 *
 * Remarques    :
 *
 *---------------------------------------------------------------------------------------------------------------
*/
int GDAL_Pick(Tcl_Interp *Interp,GDAL_Band *Band,Tcl_Obj *List) {

   Tcl_Obj      *obj;
   double        x,y,lat,lon;
   int           nobj;
   unsigned int  f;
   char          buf[32];
   const char   *str;

   if (!Band) {
      Tcl_AppendResult(Interp,"GDAL_Pick : Invalid Band",(char*)NULL);
      return(TCL_ERROR);
   }

   Tcl_ListObjLength(Interp,List,&nobj);

   /*Verifie le bon nombre de coordonnees*/
   if (nobj%2!=0 || nobj==0) {
      Tcl_AppendResult(Interp,"GDAL_Pick : Invalid number of coordinates",(char*)NULL);
      return(TCL_ERROR);
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
      Band->GRef->UnProject(Band->GRef,&x,&y,lat,lon,1,1);
      snprintf(buf,32,"Pixel_%li_%li",lrint(x),lrint(y));
      str=GDALGetMetadataItem(Band->Band[0],buf,"LocationInfo");
      Tcl_AppendElement(Interp,str);
   }

   return(TCL_OK);
}

/*----------------------------------------------------------------------------
 * Nom      : <GDAL_BandGetStat>
 * Creation : Aout 2004 - J.P. Gauthier - CMC/CMOE
 *
 * But      : Extrait des statistiques d'un champ.
 *            (Minimum,Maximum,Moyenne,LatMin,LatMax,LonMin,LonMax)
 *
 * Parametres :
 *  <Band>    : Bande a utiliser
 *
 * Retour:
 *
 * Remarques :
 *
 *----------------------------------------------------------------------------
*/
void GDAL_BandGetStat(GDAL_Band *Band) {

   double        val,pts[4],minmax[2];
   unsigned int  i,j,c;
   unsigned long n=0;

   /*Initialiser la structure*/
   if (!Band->Stat)
      Band->Stat=(TDataStat*)malloc(Band->Def->NC*sizeof(TDataStat));

   if (Band->Stat) {
      for (c=0;c<Band->Def->NC;c++) {

         Band->Stat[c].Histo=NULL;
         Band->Stat[c].HistoBin=256;
         Band->Stat[c].MinLoc.Lat=0;
         Band->Stat[c].MinLoc.Lon=0;
         Band->Stat[c].MinLoc.Elev=0;
         Band->Stat[c].MaxLoc.Lat=0;
         Band->Stat[c].MaxLoc.Lon=0;
         Band->Stat[c].MaxLoc.Elev=0;
         pts[0]=pts[1]=pts[2]=pts[3]=0.0;

         if ((Band->Def->NC==3 || Band->Def->NC==4) && Band->Def->Type==TD_UByte) {
            Band->Stat[c].Min=0.0;
            Band->Stat[c].Max=255.0;
         } else if (Band->Band[c]) {
            GDALComputeRasterMinMax(Band->Band[c],Band->Approx,minmax);
            Band->Stat[c].Min=minmax[0];
            Band->Stat[c].Max=minmax[1];
         } else {

            Band->Stat[c].Min=1e200;
            Band->Stat[c].Max=-1e200;
            Band->Stat[c].Avg=0.0;
            n=0;

            for (j=0;j<Band->Def->NJ;j++) {
               for (i=0;i<Band->Def->NI;i++) {

                  Def_Get(Band->Def,c,FIDX2D(Band->Def,i,j),val);
                  if (!isnan(val) && val!=Band->Def->NoData) {
                     n++;
                     Band->Stat[c].Avg+=val;

                     if (val<Band->Stat[c].Min) {
                        Band->Stat[c].Min=val;
                        pts[0]=i;
                        pts[1]=j;
                     }
                     if (val>Band->Stat[c].Max) {
                        Band->Stat[c].Max=val;
                        pts[2]=i;
                        pts[3]=j;
                     }
                  }
               }
            }

            if (n)
               Band->Stat[c].Avg/=n;

            if (Band->GRef && Band->GRef->Project) {
               /*Recuperer les coordonnees latlon des min max*/
               Band->GRef->Project(Band->GRef,pts[0],pts[1],&Band->Stat[c].MinLoc.Lat,&Band->Stat[c].MinLoc.Lon,0,1);
               Band->GRef->Project(Band->GRef,pts[2],pts[3],&Band->Stat[c].MaxLoc.Lat,&Band->Stat[c].MaxLoc.Lon,0,1);
            }
         }
      }
   }
}

/*----------------------------------------------------------------------------
 * Nom      : <GDAL_BandPreIni>
 * Creation : Juillet 2006 - J.P. Gauthier - CMC/CMOE
 *
 * But      : Effectue la pre-initialisation des parametres avant le rendue.
 *
 * Parametres :
 *  <Data>    : Champs
 *
 * Retour:
 *
 * Remarques :
 *
 *----------------------------------------------------------------------------
*/
void GDAL_BandPreInit(GDAL_Band *Band) {

   if (!Band->Stat)
      GDAL_BandGetStat(Band);

//   if (!(Band->Spec->MinMax&DATASPEC_MINSET)) Band->Spec->Min=Band->Stat->Min;
//   if (!(Band->Spec->MinMax&DATASPEC_MAXSET)) Band->Spec->Max=Band->Stat->Max;
//   if (!(Band->Spec->MinMax&DATASPEC_MINSET)) Band->Spec->Min=Band->Spec->Max<Band->Spec->Min?Band->Spec->Max:Band->Spec->Min;

   if (Band->Tex.Indexed) {
      Band->Spec->Min=0;
      Band->Spec->Max=Band->Spec->Map->NbPixels-1;
   } else {
      Band->Spec->Min=Band->Spec->Map->Min[0];
      Band->Spec->Max=Band->Spec->Map->Max[0];
   }

   DataSpec_Define(Band->Spec);
}

/*----------------------------------------------------------------------------
 * Nom      : <GDAL_BandGetHisto>
 * Creation : Novembre 2014 - J.P. Gauthier - CMC/CMOE
 *
 * But      : Recuperer l'histogramme des valeurs
 *
 * Parametres :
 *  <Band>    : Bande a utiliser
 *  <Index>   : Index de la bande
 *  <Bin>     : Nombre de bin
 *  <Min>     : Limite minimale
 *  <Max>     : Limite maximale
 *
 * Retour:
 *
 * Remarques :
 *
 *----------------------------------------------------------------------------
*/
int GDAL_BandGetHisto(GDAL_Band *Band,int Index,int Bin,double Min,double Max) {

   // If the number of bins requested is different
   if (Band->Stat[Index].Histo && Band->Stat[Index].HistoBin!=Bin) {
      free(Band->Stat[Index].Histo);
      Band->Stat[Index].Histo=NULL;
   }

   if (!Band->Stat[Index].Histo) {
      if (!(Band->Stat[Index].Histo=(unsigned long long*)malloc(Bin*sizeof(unsigned long long)))) {
         return(0);
      }
      Band->Stat[Index].HistoBin=Bin;
      GDALGetRasterHistogramEx(Band->Band[Index],Min,Max,Band->Stat[Index].HistoBin,Band->Stat[Index].Histo,FALSE,Band->Approx,GDALDummyProgress,NULL);
   }
   return(1);
}
/*--------------------------------------------------------------------------------------------------------------
 * Nom          : <GDAL_BandRender>
 * Creation     : Juin 2004 J.P. Gauthier - CMC/CMOE
 *
 * But          : Rendu de la bande raster.
 *
 * Parametres  :
 *   <Proj>     : La projection courante
 *   <VP>       : Le viewport ou le rendu doit etre fait
 *   <Band>     : Bande a afficher
 *
 * Retour       :
 *
 * Remarques    :
 *
 *---------------------------------------------------------------------------------------------------------------
*/
int GDAL_BandRender(Projection *Proj,ViewportItem *VP,GDAL_Band *Band) {

   int           n;
   GLuint        tx=0;
   GLhandleARB   prog;
   OGR_Layer     *layer;
   OGRGeometryH  *geom;

   GLuint    sc[]={ GL_RED_SCALE, GL_GREEN_SCALE, GL_BLUE_SCALE, GL_ALPHA_SCALE };
   GLuint    bc[]={ GL_RED_BIAS,  GL_GREEN_BIAS,  GL_BLUE_BIAS,  GL_ALPHA_BIAS  };

   if (!Band || !Band->Spec) {
      App_Log(ERROR,"%s: Invalid band object\n",__func__);
      return(0);
   }

   if (!Band->Spec->Active) {
      return(0);
   }

   /*Check for invalid georeference*/
   if (!GeoRef_Valid(Band->GRef)) {
      App_Log(ERROR,"%s: Invalid georeference\n",__func__);
      return(0);
   }

   /*Calculer les statistiques si elle ne le sont pas deja*/
   if (!Band->Stat)
      GDAL_BandGetStat(Band);

   /*Tiling des donnees selon la resolution*/
   if (GLRender->Resolution<=2) {
      GeoTex_Qualify(Band);
      GeoTex_Resolution(Band,Proj);
   }

   /*Read in data in another thread*/
   if (!GLRender->UseThreads || GLRender->XBatch || GLRender->TRCon) {
      GeoTex_Parse(Band,&Band->Tex.Tile,Proj,VP,Band->Tex.ResN,0,0);
   } else {
      if (!VP->Secondary) {
         if (!Band->Tex.ThreadId) {
            //Tcl_CreateThread(&Band->Tex.ThreadId,GeoTex_ThreadProc,Band,SYS_IOTHREAD_STACKSIZE,TCL_THREAD_NOFLAGS);
            if (Tcl_CreateThread(&Band->Tex.ThreadId,GeoTex_ThreadProc,Band,TCL_THREAD_STACK_DEFAULT,TCL_THREAD_NOFLAGS)==TCL_ERROR) {
               App_Log(ERROR,"%s: Unable to initialize geotexture thread\n",__func__);
            }
         }
      }
   }

   if (Band->Spec->OGRMask) {
      glColorMask(GL_FALSE,GL_FALSE,GL_FALSE,GL_FALSE);
      glStencilMask(0x04);
      glStencilFunc(GL_ALWAYS,0x4,0x4);
      glStencilOp(GL_REPLACE,GL_REPLACE,GL_REPLACE);
      if ((geom=OGR_GeometryGet(Tcl_GetString(Band->Spec->OGRMask)))) {
         OGR_GeometryRender(Proj,Band->GRef,NULL,geom,0.0,0.0);
      } else if ((layer=OGR_LayerGet(Tcl_GetString(Band->Spec->OGRMask)))) {
         OGR_LayerRender(NULL,Proj,Proj->VP,layer,1);
      }
      glStencilMask(0x04);
      glStencilFunc(GL_EQUAL,0x04,0x04);
      glStencilOp(GL_KEEP,GL_ZERO,GL_ZERO);
      glColorMask(GL_TRUE,GL_TRUE,GL_TRUE,GL_TRUE);
   }

   glDisable(GL_CULL_FACE);
   glDisable(GL_DEPTH_TEST);
   glEnable(GL_STENCIL_TEST);
   glPolygonMode(GL_FRONT_AND_BACK,GL_FILL);
   glNormal3dv(Proj->Nr);

   if (Band->Spec->Topo || Proj->Sun) {
      glEnable(GL_COLOR_MATERIAL);
      glEnable(GL_LIGHTING);
      glEnable(GL_LIGHT0);
   }

   if (GLRender->GLZBuf || Band->Spec->Topo) {
      glEnable(GL_DEPTH_TEST);
   }

   if (GLRender->Resolution<=2) {
      glEnable(GL_TEXTURE_2D);
      glEnable(GL_BLEND);
      glColor4f(1.0,1.0,1.0,Band->Spec->Alpha/100.0);
   } else {
      glPolygonMode(GL_FRONT_AND_BACK,GL_LINE);
      glColor3f(0.0,0.0,0.0);
   }

   if (GLRender->GLDebug) {
      glPolygonMode(GL_FRONT_AND_BACK,GL_LINE);
   }

   // Setup scalings
   Band->Tex.Scale[0]=Band->Tex.Scale[1]=Band->Tex.Scale[2]=Band->Tex.Scale[3]=-1.0;
   Band->Tex.Bias[0]=Band->Tex.Bias[1]=Band->Tex.Bias[2]=Band->Tex.Bias[3]=0.0;

   if (!Band->Tex.Indexed) {

      for (n=0;n<(Band->Def->NC<=4?Band->Def->NC:4);n++) {
         switch(Band->Def->Type) {
            case TD_Unknown:
            case TD_Binary:
            case TD_UInt64:
            case TD_Int64:   break;
            case TD_UByte:   Band->Tex.Scale[n]=((0x1<<8)-1)/(Band->Spec->Map->Max[n]-Band->Spec->Map->Min[n]);
                             Band->Tex.Bias[n]=-Band->Spec->Map->Min[n]/((0x1<<8)-1);
                             break;
            case TD_Byte:    Band->Tex.Scale[n]=((0x1<<7)-1)/(Band->Spec->Map->Max[n]-Band->Spec->Map->Min[n]);
                             Band->Tex.Bias[n]=-Band->Spec->Map->Min[n]/((0x1<<7)-1);
                             break;
            case TD_UInt16:  Band->Tex.Scale[n]=((0x1<<16)-1)/(Band->Spec->Map->Max[n]-Band->Spec->Map->Min[n]);
                             Band->Tex.Bias[n]=-Band->Spec->Map->Min[n]/((0x1<<16)-1);
                             break;
            case TD_Int16:   Band->Tex.Scale[n]=((0x1<<15)-1)/(Band->Spec->Map->Max[n]-Band->Spec->Map->Min[n]);
                             Band->Tex.Bias[n]=-Band->Spec->Map->Min[n]/((0x1<<15)-1);
                             break;
            case TD_UInt32:
            case TD_Int32:   Band->Tex.Scale[n]=(((unsigned int)0x1<<31)-1)/(Band->Spec->Map->Max[n]-Band->Spec->Map->Min[n]);
                             Band->Tex.Bias[n]=-Band->Spec->Map->Min[n]/(((unsigned int)0x1<<31)-1);
                             break;
            case TD_Float32:
            case TD_Float64: Band->Tex.Scale[n]=1.0/(Band->Spec->Map->Max[n]-Band->Spec->Map->Min[n]);
                             Band->Tex.Bias[n]=-Band->Spec->Map->Min[n];
                             break;
         }
         if (GLRender->GLDebug)
            App_Log(DEBUG,"%s: Normalizing factor (%i) Sc=%f Bc=%f\n",__func__,n,Band->Tex.Scale[n],Band->Tex.Bias[n]);
      }

      if (!GLRender->ShaderAvailable) {
         for (n=0;n<(Band->Def->NC<=4?Band->Def->NC:4);n++) {
            glPixelTransferf(sc[n],Band->Tex.Scale[n]);
            glPixelTransferf(bc[n],Band->Tex.Bias[n]*Band->Tex.Scale[n]);
         }
         for (;n<4;n++) {
            glPixelTransferf(sc[n],Band->Tex.Scale[0]);
            glPixelTransferf(bc[n],Band->Tex.Bias[0]*Band->Tex.Scale[0]);
        }
      }
   }
   
   if (!GLRender->ShaderAvailable && Band->Spec->Map) {
      glEnable(GL_COLOR_TABLE);
      glColorTable(GL_COLOR_TABLE,GL_RGBA,256,GL_RGBA,GL_UNSIGNED_BYTE,(GLvoid*)Band->Spec->Map->Color);
   }

   //   GLRender->Prog[PROG_DATATEX]=GLShader_Load("/home/afsr/005/eer_SPI/LibTkGL/TclData","DataTex");
   if ((prog=GLRender->Prog[PROG_DATATEX])) {
      glUseProgramObjectARB(prog);

      if (Band->Spec->Map) {
         glGenTextures(1,&tx);
         glActiveTexture(GL_TEXTURE1);
         glBindTexture(GL_TEXTURE_1D,tx);
         glTexParameteri(GL_TEXTURE_1D,GL_TEXTURE_WRAP_S,GL_CLAMP_TO_EDGE);
         glTexParameteri(GL_TEXTURE_1D,GL_TEXTURE_MIN_FILTER,GL_LINEAR);
         glTexParameteri(GL_TEXTURE_1D,GL_TEXTURE_MAG_FILTER,GL_LINEAR);
         glTexImage1D(GL_TEXTURE_1D,0,GL_RGBA,Band->Spec->Map->NbPixels,0,GL_RGBA,GL_UNSIGNED_BYTE,Band->Spec->Map->Color);
      }
      glUniform1iARB(GLShader_UniformGet(prog,"Map"),0);

      /*Setup 1D Colormap Texture*/
      if (!Band->Tex.Indexed && Band->Spec->Map) {
         if (Band->Def->NC==1) {
            glUniform1iARB(GLShader_UniformGet(prog,"Map"),1);
         }
      }

      glUniform1iARB(GLShader_UniformGet(prog,"Colormap"),1);
      glUniform1iARB(GLShader_UniformGet(prog,"Data"),0);
      glUniform1fARB(GLShader_UniformGet(prog,"Cylindric"),(Proj->Type->Def==PROJCYLIN?Proj->L:-999.0));
      glUniform4fvARB(GLShader_UniformGet(prog,"Sc"),1,Band->Tex.Scale);
      glUniform4fvARB(GLShader_UniformGet(prog,"Bc"),1,Band->Tex.Bias);
      glActiveTexture(GL_TEXTURE0);
   }

   GeoTex_Render(Band,Band->Tex.Tile,Proj,VP,1);

   if (tx)
      glDeleteTextures(1,&tx);

   if (prog) {
      glUseProgramObjectARB(0);
//   GLShader_UnInstall(prog);
   }

   glStencilMask(0xff);
   glStencilFunc(GL_EQUAL,0x0,0xff);
   glStencilOp(GL_KEEP,GL_KEEP,GL_KEEP);
   glDisable(GL_BLEND);
   glDisable(GL_DEPTH_TEST);
   glDisable(GL_TEXTURE_2D);
   glEnable(GL_CULL_FACE);
   glDisable(GL_COLOR_MATERIAL);
   glDisable(GL_LIGHTING);
   glDisable(GL_LIGHT0);
   glDisable(GL_COLOR_TABLE);

   for (n=0;n<4;n++) {
      glPixelTransferf(sc[n],1.0);
      glPixelTransferf(bc[n],0.0);
   }
   return(1);
}

