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

#include "tclGDAL.h"
#include "/data/cmoe/afsr005/Archive/gpc232/gpc.h"

extern OGRGeometryH gpc_ogr(gpc_op Op,OGRGeometryH Geom0,OGRGeometryH Geom1);
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
   GDALDataType    type;
   GDALRPCInfo     rpcinfo;
   int             c;
   double          tra[6],inv[6];
   int             i,nx,ny,rx,ry;
   CONST char     *prj;
   char          **meta;

   if (!NIdx) {
      Tcl_AppendResult(Interp,"GDAL_BandRead: No valid band specified",(char*)NULL);
      return(TCL_ERROR);
   }

   for(i=0;i<NIdx;i++) { if ((file=GDAL_FileGet(Interp,FileId[i]))) break; }
   if (!file) {
      Tcl_AppendResult(Interp,"GDAL_BandRead: Invalid file handle",(char*)NULL);
      return(TCL_ERROR);
   }

   band=GDAL_BandGet(Name);
   if (!band) {
      if (!(band=GDAL_BandCreate(Interp,Name))) {
         return(TCL_ERROR);
      }
   } else {
      if (band->Stat)
         free(band->Stat);
      band->Stat=NULL;

      GeoTex_Clear(&band->Tex,NULL);
   }

   /*Get the data units*/
//      band->Spec->Unit=GDALGetRasterUnitType(hband);

   /*Get the projection*/
   prj=GDALGetProjectionRef(file->Set);
   if (!prj || !strlen(prj)) {
      prj=GDALGetGCPProjection(file->Set);
      if (!prj || !strlen(prj) || GDALGetGCPCount(file->Set)<=1) {
         prj=NULL;
      }
   }

   hband=GDALGetRasterBand(file->Set,Idxs[i]);
   type=GDALGetRasterDataType(hband);

   /*Figure out dimensions to read , if passed in limits are 0, read the whole thing*/
   rx=ry=1;
   nx=GDALGetRasterBandXSize(hband);
   ny=GDALGetRasterBandYSize(hband);

   /*Add border to dimensions to read*/
   X0-=BD;X1+=BD;
   Y0-=BD;Y1+=BD;

   /*Check for limit overflow*/
   X0=X0<0?0:X0; X1=X1>=nx?nx-1:X1;
   Y0=Y0<0?0:Y0; Y1=Y1>=ny?ny-1:Y1;

   /*If size is smaller than 1 thna read the whole thing*/
   if ((nx=X1-X0+1)<=1) {
      nx=GDALGetRasterBandXSize(hband);
      X0=0;
      X1=nx-1;
      BD=0;
      rx=0;
   }
   if ((ny=Y1-Y0+1)<=1) {
      ny=GDALGetRasterBandYSize(hband);
      Y0=0;
      Y1=ny-1;
      BD=0;
      ry=0;
   }

   if (band->Def)
      Data_DefFree(band->Def);
   if (!(band->Def=Data_DefNew(nx,ny,1,(Full?NIdx:-NIdx),GDAL_Type[type]))) {
      Tcl_AppendResult(Interp,"GDAL_BandRead: Could not allocate memory",(char*)NULL);
      return(TCL_ERROR);
   }

   if (band->Ref)
      GeoRef_Destroy(Interp,band->Ref->Name);

   /*Get the projection transform*/
   if ((band->NbGCPs=GDALGetGCPCount(file->Set))) {
      if (band->GCPs) free(band->GCPs);
      band->GCPs=(GDAL_GCP*)malloc(band->NbGCPs*sizeof(GDAL_GCP));
      memcpy(band->GCPs,GDALGetGCPs(file->Set),band->NbGCPs*sizeof(GDAL_GCP));
   } else {
      GDALGetGeoTransform(file->Set,tra);
      GDALInvGeoTransform(tra,inv);
   }

   if (band->GCPs) {
      /*Get the transform from GCPs*/
      band->Ref=GeoRef_WKTSetup(nx,ny,1,0,NULL,(char*)prj,NULL,NULL,NULL);
/*
      if (!GDALGCPsToGeoTransform(band->NbGCPs,band->GCPs,tra,TRUE)) {
          printf("(WARNING) GDAL_BandRead: Unable to fit control points\n");
      }
*/
      printf("(DEBUG) GDAL_BandRead: Using GCPs to get transform\n");
      if (!(band->Ref->GCPTransform=(void*)GDALCreateGCPTransformer(band->NbGCPs,band->GCPs,3,FALSE))) {
         printf("(WARNING) GDAL_BandRead: Unable to fit control points\n");
      }

   } else if ((meta=GDALGetMetadata(file->Set,NULL)) && GDALExtractRPCInfo(meta,&rpcinfo)) {
      /*Get the transform from RPCInfo*/
      band->Ref=GeoRef_WKTSetup(nx,ny,1,0,NULL,(char*)prj,NULL,NULL,NULL);

      printf("(DEBUG) GDAL_BandRead: Using RPC Info to get transform\n");
      if (!(band->Ref->RPCTransform=(void*)GDALCreateRPCTransformer(&rpcinfo,FALSE,0.1))) {
         printf("(WARNING) GDAL_BandRead: Unable to fit RPC\n");
      }
   } else {
      band->Ref=GeoRef_WKTSetup(nx,ny,1,0,NULL,(char*)prj,tra,inv,NULL);
   }

   GeoRef_Size(band->Ref,X0+BD,Y0+BD,0,X1-BD,Y1-BD,0,BD);
   GeoRef_Qualify(band->Ref);

   /*Get the No Data Value*/
   GDALGetRasterNoDataValue(hband,&i);
   if (i)
      band->Def->NoData=GDALGetRasterNoDataValue(hband,&i);

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
            GDALRasterIO(band->Band[i],GF_Read,X0,Y0,band->Def->NI,band->Def->NJ,band->Def->Data[i],band->Def->NI,band->Def->NJ,type,0,0);
         } else {
            fprintf(stderr,"(DEBUG) GDAL_BandRead: Delaying read\n");
         }
      } else if (Idxs[i]!=-1) {
         Tcl_AppendResult(Interp,"GDAL_BandRead: Invalid file handle",(char*)NULL);
         return(TCL_ERROR);
      }
   }

   if (GDALGetRasterColorInterpretation(hband)==GCI_PaletteIndex) {
      band->Tex.Indexed=1;
   }
   band->Tex.Nx=nx;
   band->Tex.Ny=ny;

   if (band->Spec->Map)
      CMap_Free(band->Spec->Map->Name);

   if ((hTable=GDALGetRasterColorTable(hband))) {
//      printf( "Color Table (%s with %d entries)\n",GDALGetPaletteInterpretationName(GDALGetPaletteInterpretation(hTable)),GDALGetColorEntryCount(hTable));

      band->Spec->Map=CMap_New(NULL,GDALGetColorEntryCount(hTable));
      for (c=0;c<band->Spec->Map->NbPixels;c++) {
          GDALGetColorEntryAsRGB(hTable,c,&entry);
          band->Spec->Map->Control[c][0]=entry.c1;
          band->Spec->Map->Control[c][1]=entry.c2;
          band->Spec->Map->Control[c][2]=entry.c3;
          band->Spec->Map->Control[c][3]=entry.c4;
      }
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

/*----------------------------------------------------------------------------------------------------------
 * Nom          : <GDAL_Rasterize>
 * Creation     : Juin 2004 J.P. Gauthier - CMC/CMOE
 *
 * But          : Rasteriser des donnees vectorielles dans une bande ou champs
 *
 * Parametres   :
 *   <Def>      : Definition des donnees raster
 *   <Ref>      : Referentiel des donnnes raster
 *   <Geom>     : Donnees vectorielle a rasteriser
 *   <Value>    : Valuer a assigner
 *
 * Retour       :
 *
 * Remarques    :
 *
 *---------------------------------------------------------------------------------------------------------------
*/
void GDAL_Rasterize(TDataDef *Def,TGeoRef *Ref,OGRGeometryH Geom,double Value) {

   int    i,j,g,ind1,ind2;
   int    x,y,miny,maxy,minx,maxx;
   int    ints,n,ns,np;
   int   *polyInts;
   double dminy,dmaxy,dx1,dy1,dx2,dy2,dy,dx,dtx,dty,t;
   double intersect,tmpd;
   int    horizontal_x1,horizontal_x2;

   int dnx,dny,x0,x1,y0,y1,fr,sx,sy;

   OGRGeometryH geom;

   if (!Geom || !Def)
      return;

   n=0;
   dminy=1e300;
   dmaxy=-1e300;

   for (i=0;i<OGR_G_GetGeometryCount(Geom);i++) {
      geom=OGR_G_GetGeometryRef(Geom,i);
      if (EQUAL(OGR_G_GetGeometryName(geom),"LINEARRING")) {
         n+=ns=OGR_G_GetPointCount(geom);
         for (j=0;j<ns;j++) {
            dtx=OGR_G_GetX(geom,j);
            dty=OGR_G_GetY(geom,j);
            INVTRANSFORM(Ref,dx,dy,dtx,dty);
            if (dy<dminy)
               dminy=dy;
            if (dy>dmaxy)
               dmaxy=dy;
         }
      } else {
         GDAL_Rasterize(Def,Ref,geom,Value);
      }
   }
   if (!(n+=OGR_G_GetPointCount(Geom)))
      return;

   switch (OGR_G_GetDimension(Geom)) {
      case 0: /*Point type*/
         for (i=0;i<n;i++) {
            dtx=OGR_G_GetX(Geom,i);
            dty=OGR_G_GetY(Geom,i);
            INVTRANSFORM(Ref,dx1,dy1,dtx,dty);
            x=ROUND(dx1);
            y=ROUND(dy1);
            if (FIN2D(Def,x,y))
               Def_Set(Def,0,FIDX2D(Def,x,y),Value);
         }
         break;

      case 1: /*Line type*/
         for (i=1;i<n;i++) {
            ind2=i;
            ind1=i==0?n-1:i-1;

            dtx=OGR_G_GetX(Geom,ind1);
            dty=OGR_G_GetY(Geom,ind1);
            INVTRANSFORM(Ref,dx1,dy1,dtx,dty);

            dtx=OGR_G_GetX(Geom,ind2);
            dty=OGR_G_GetY(Geom,ind2);
            INVTRANSFORM(Ref,dx2,dy2,dtx,dty);

            x0=ROUND(dx1); y0=ROUND(dy1);
            x1=ROUND(dx2); y1=ROUND(dy2);
            dny=y1-y0;
            dnx=x1-x0;
            if (dny<0) {
               dny=-dny;
               sy=-1;
            } else {
               sy=1;
            }
            if (dnx<0) {
               dnx=-dnx;
               sx=-1;
            } else {
               sx=1;
            }
            dny<<=1;
            dnx<<=1;

            if (FIN2D(Def,x0,y0))
               Def_Set(Def,0,FIDX2D(Def,x0,y0),Value);
            if (dnx>dny) {
               fr=dny-(dnx>>1);
               while(x0!=x1) {
                  if (fr>=0) {
                     y0+=sy;
                     fr-=dnx;
                  }
                  x0+=sx;
                  fr+=dny;
                  if (FIN2D(Def,x0,y0))
                     Def_Set(Def,0,FIDX2D(Def,x0,y0),Value);
               }
            } else {
               fr=dnx-(dny>>1);
               while(y0!=y1) {
                  if (fr>=0) {
                     x0+=sx;
                     fr-=dny;
                  }
                  y0+=sy;
                  fr+=dnx;
                  if (FIN2D(Def,x0,y0))
                     Def_Set(Def,0,FIDX2D(Def,x0,y0),Value);
               }
            }
         }
         break;

      case 2: /*Polygon type*/
         miny=(int)(dminy<0?0:dminy);
         maxy=(int)(dmaxy>=Def->NJ?Def->NJ-1:dmaxy);
         minx=0;
         maxx=Def->NI-1;

         polyInts=(int*)malloc(sizeof(int)*n);

         /* Fix in 1.3: count a vertex only once */
         for (y=miny;y<=maxy;y++) {
            dy=y+0.5; /* center height of line*/
            ints=0 ;

            /*Initialize polyInts, otherwise it can sometimes causes a seg fault */
            for (i=0;i<n;i++) {
               polyInts[i]=-1;
            }

            ns=OGR_G_GetGeometryCount(Geom);
            for (g=0;g<(ns==0?1:ns);g++) {
               if (ns) {
                  geom=OGR_G_GetGeometryRef(Geom,g);
               } else {
                  geom=Geom;
               }
               np=OGR_G_GetPointCount(geom);

               for (i=0;i<np;i++) {
                  ind2=i;
                  ind1=i==0?np-1:i-1;

                  dtx=OGR_G_GetX(geom,ind1);
                  dty=OGR_G_GetY(geom,ind1);
                  INVTRANSFORM(Ref,dx1,dy1,dtx,dty);

                  dtx=OGR_G_GetX(geom,ind2);
                  dty=OGR_G_GetY(geom,ind2);
                  INVTRANSFORM(Ref,dx2,dy2,dtx,dty);

                  if ((dy1<dy && dy2<dy) || (dy1>dy && dy2>dy))
                     continue;

                  if (dy1<dy2) {
                  } else if (dy1>dy2) {
                     tmpd=dy2;
                     dy2=dy1;
                     dy1=tmpd;
                     tmpd=dx2;
                     dx2=dx1;
                     dx1=tmpd;
                  } else { /* if (fabs(dy1-dy2)< 1.e-6) */
                           /*AE: DO NOT skip bottom horizontal segments
                           -Fill them separately-
                           They are not taken into account twice.*/
                     if (dx1>dx2) {
                        horizontal_x1=(int)floor(dx2+0.5);
                        horizontal_x2=(int)floor(dx1+0.5);
                        if ((horizontal_x1>maxx) || (horizontal_x2<minx))
                           continue;

                        /*fill the horizontal segment (separately from the rest)*/
                        for(x=horizontal_x1;x<horizontal_x2;x++)
                          if (FIN2D(Def,x,y))
                            Def_Set(Def,0,FIDX2D(Def,x,y),Value);
                        continue;
                     } else {
                        /*skip top horizontal segments (they are already filled in the regular loop)*/
                        continue;
                     }
                  }

                  if ((dy<dy2) && (dy>=dy1)) {
                     intersect=(dy-dy1)*(dx2-dx1)/(dy2-dy1)+dx1;
                     polyInts[ints++]=(int)floor(intersect+0.5);
                  }
               }
            }
            qsort(polyInts,ints,sizeof(int),QSort_Int);

            for (i=0;i<ints;i+=2) {
               if (polyInts[i]<=maxx && polyInts[i+1]>=minx) {
                  for(x=polyInts[i];x<polyInts[i+1];x++)
                     if (FIN2D(Def,x,y))
                        Def_Set(Def,0,FIDX2D(Def,x,y),Value);
               }
            }
         }
         free(polyInts);
         break;
   }
}

/*--------------------------------------------------------------------------------------------------------------
 * Nom          : <Data_GridOGRQuad>
 * Creation     : Novembre 2004 J.P. Gauthier - CMC/CMOE
 *
 * But          : Importer des donnees dans une bande raster
 *
 * Parametres   :
 *   <Interp>   : L'interpreteur Tcl
 *   <Def>      : Definition de la donnee
 *   <Ref>      : Referentiel de la donnee
 *   <Geom>     : Geometrie a tester
 *   <Area>     : Aire de la geometrie
 *   <Value>    : Valeur a assigner
 *   <Mode>     : Mode de rasterization
 *   <Type>     : Type de rasterization
 *   <X0>       : Coin inferieur gauche
 *   <Y0>       : Coin inferieur gauche
 *   <X1>       : Coin superieur droit
 *   <Y1>       : Coin superieur droit
 *
 * Retour       : Code d'erreur standard TCL
 *
 * Remarques    :
 *
 *---------------------------------------------------------------------------------------------------------------
*/
int Data_GridOGRQuad(Tcl_Interp *Interp,Tcl_Obj *List,TDataDef *Def,TGeoRef *Ref,OGRGeometryH Geom,char Mode,char Type,double Area,double Value,double *Total,int X0,int Y0,int X1,int Y1) {

   double        dx,dy,dp=0.0,val=0.0;
   int           x,y,n=0,idx;
   OGRGeometryH  inter=NULL;

#ifdef GDAL126
   /* Setup the intersecting area */
   TRANSFORM(Ref,dx,dy,X0-0.5,Y0-0.5);
   OGR_G_SetPoint(Def->Pick,0,dx,dy,0);
   OGR_G_SetPoint(Def->Pick,4,dx,dy,0);
   TRANSFORM(Ref,dx,dy,X0-0.5,Y1+0.5);
   OGR_G_SetPoint(Def->Pick,1,dx,dy,0);
   TRANSFORM(Ref,dx,dy,X1+0.5,Y1+0.5);
   OGR_G_SetPoint(Def->Pick,2,dx,dy,0);
   TRANSFORM(Ref,dx,dy,X1+0.5,Y0-0.5);
   OGR_G_SetPoint(Def->Pick,3,dx,dy,0);

   val=Value;

   /* Test for intersection */
   if ((Area>0.0 || Mode!='C' || Mode!='N' || Mode!='A') && GPC_Intersect(Geom,Def->Pick)) {

      /* If this is a single pixel */
      if (X0==X1 && Y0==Y1) {

         idx=FIDX2D(Def,X0,Y0);

         /* If we are computing areas */
         if (Area>0.0) {
            switch(Type) {
               case 'A' :
//                  inter=OGR_G_Intersection(Geom,Def->Poly);
                  inter=GPC_OnOGR(GPC_INT,Geom,Def->Poly);
                  if (Mode=='C' || Mode=='N') {
                     dp=OGR_G_GetArea(inter)/Area;
                  } else if (Mode=='A') {
                     dp=OGR_G_GetArea(inter);
                  }
                  break;

               case 'L':
//                  inter=OGR_G_Intersection(Geom,Def->Poly);
                  inter=GPC_Clip(Geom,Def->Poly);
                  if (Mode=='C' || Mode=='N') {
                     dp=GPC_Length(inter)/Area;
                  } else if (Mode=='A') {
                     dp=GPC_Length(inter);
                  }
                  break;

               case 'P':
                  dp=1.0;
                  break;
            }
            Def_Get(Def,0,idx,val);
            if (isnan(val) || val==Def->NoData)
               val=0.0;
            val+=Value*dp;
            OGR_G_DestroyGeometry(inter);
         }

         /* Are we within */
         if (Mode!='W' || (OGR_G_GetDimension(Geom)>=2 && OGR_G_Within(Geom,Def->Poly))) {
            Def_Set(Def,0,idx,val);

            if (Mode=='N' && Total) {
               Total[idx]+=dp;
            }
            if (List) {
               Tcl_ListObjAppendElement(Interp,List,Tcl_NewIntObj(X0));
               Tcl_ListObjAppendElement(Interp,List,Tcl_NewIntObj(Y0));
               Tcl_ListObjAppendElement(Interp,List,Tcl_NewDoubleObj(dp));
            }
            n=1;
         }
     } else {
         /* Otherwise, refine the search by quad dividing*/
         x=(X1-X0)>>1;
         y=(Y1-Y0)>>1;

         /* If within 1 bloc, parse them all */
         if (x==0 || y==0) {
            for (x=X0;x<=X1;x++) {
               for (y=Y0;y<=Y1;y++) {
                  n+=Data_GridOGRQuad(Interp,List,Def,Ref,Geom,Mode,Type,Area,Value,Total,x,y,x,y);
               }
            }
         } else {
            n+=Data_GridOGRQuad(Interp,List,Def,Ref,Geom,Mode,Type,Area,Value,Total,X0,Y0,X0+x,Y0+y);
            n+=Data_GridOGRQuad(Interp,List,Def,Ref,Geom,Mode,Type,Area,Value,Total,X0+x+1,Y0,X1,Y0+y);
            n+=Data_GridOGRQuad(Interp,List,Def,Ref,Geom,Mode,Type,Area,Value,Total,X0,Y0+y+1,X0+x,Y1);
            n+=Data_GridOGRQuad(Interp,List,Def,Ref,Geom,Mode,Type,Area,Value,Total,X0+x+1,Y0+y+1,X1,Y1);
         }
      }
   }
#endif
   return(n);
}

/*----------------------------------------------------------------------------
 * Nom      : <Data_GridConservative>
 * Creation : Mai 2006 - J.P. Gauthier - CMC/CMOE
 *
 * But      : Effectue l'interpolation par moyennage minimum ou maximum
 *
 * Parametres  :
 *  <Interp>   : Interpreteur TCL
 *  <ToRef>    : Reference du champs destination
 *  <ToDef>    : Description du champs destination
 *  <FromRef>  : Reference du champs source
 *  <FromDef>  : Description du champs source
 *  <Mode>     : Type d'interpolation (N=NORMALIZE, C=CONSERVATIVE hence not normalized)
 *  <Final>    : Finalisation de l'operation (Averaging en plusieurs passe)
 *  <Prec>     : Nombre de segmentation d'une cellule (1=pas de segmentation)
 *  <List>     : liste des index , a remplir ou a utiliser
 *
 * Retour:
 *  <TCL_...> : Code d'erreur de TCL.
 *
 * Remarques :
 *
 *----------------------------------------------------------------------------
*/
int Data_GridConservative(Tcl_Interp *Interp,TGeoRef *ToRef,TDataDef *ToDef,TGeoRef *FromRef,TDataDef *FromDef,char Mode,int Final,int Prec,Tcl_Obj *List) {

   int          i,j,n,nt,p,pt,pi,pj,len=-1,idx;
   double       val0,val1,area;
   OGRGeometryH cell,ring;
   OGREnvelope  env;
   Tcl_Obj      *lst,*item=NULL;

#ifdef GDAL126
   cell=OGR_G_CreateGeometry(wkbPolygon);
   ring=OGR_G_CreateGeometry(wkbLinearRing);

   if (!ToDef->Pick)
      ToDef->Pick=OGR_G_CreateGeometry(wkbLinearRing);

   if (!ToDef->Poly) {
      ToDef->Poly=OGR_G_CreateGeometry(wkbPolygon);
      OGR_G_AddGeometryDirectly(ToDef->Poly,ToDef->Pick);
   }

   if (Mode=='N' && !ToDef->Buffer) {
      ToDef->Buffer=(double*)calloc(FSIZE2D(ToDef),sizeof(double));
      if (!ToDef->Buffer) {
         Tcl_AppendResult(Interp,"Data_GridConservative: Unable to allocate accumulation buffer",(char*)NULL);
         return(TCL_ERROR);
      }
   }

   /*Check for included index list*/
   if (List) {
      lst=Tcl_ObjGetVar2(Interp,List,NULL,0x0);
      if (!lst) {
         item=Tcl_NewListObj(0,NULL);
         List=Tcl_ObjSetVar2(Interp,List,NULL,item,0x0);
      } else {
         List=lst;
      }
      Tcl_ListObjLength(Interp,List,&len);
   }

   /*Wouou we have the index list*/
   if (len>1) {
      for(n=0;n<len;n+=3) {
         Tcl_ListObjIndex(Interp,List,n,&lst);
         Tcl_GetIntFromObj(Interp,lst,&i);
         Tcl_ListObjIndex(Interp,List,n+1,&lst);
         Tcl_GetIntFromObj(Interp,lst,&j);

         Def_Get(FromDef,0,FIDX2D(FromDef,i,j),val1);
         if (isnan(val1) || val1==FromDef->NoData) {
            continue;
         }

         Tcl_ListObjIndex(Interp,List,n+2,&lst);
         Tcl_ListObjLength(Interp,lst,&pt);
         for(p=0;p<pt;p+=3) {
            Tcl_ListObjIndex(Interp,lst,p,&item);
            Tcl_GetIntFromObj(Interp,item,&pi);
            Tcl_ListObjIndex(Interp,lst,p+1,&item);
            Tcl_GetIntFromObj(Interp,item,&pj);
            Tcl_ListObjIndex(Interp,lst,p+2,&item);
            Tcl_GetDoubleFromObj(Interp,item,&area);

            idx=FIDX2D(ToDef,pi,pj);
            Def_Get(ToDef,0,idx,val0);
            if (isnan(val0) || val0==ToDef->NoData)
               val0=0.0;
            val0+=val1*area;
            Def_Set(ToDef,0,idx,val0);

            if (Mode=='N') {
               ToDef->Buffer[idx]+=area;
            }
         }
      }
   } else {

      /*Damn, we dont have it*/
      for(j=0;j<FromDef->NJ;j++) {
         for(i=0;i<FromDef->NI;i++) {

            OGR_GridCell(ring,ToRef,FromRef,i,j,Prec);
            OGR_G_Empty(cell);
            OGR_G_AddGeometry(cell,ring);
            area=OGR_G_GetArea(cell);

            if (area>0.0) {
               Def_Get(FromDef,0,FIDX2D(FromDef,i,j),val1);
               if (isnan(val1) || val1==FromDef->NoData) {
                  continue;
               }

               /*Utilise les limites de l'enveloppe pour initialiser la selection*/
               OGR_G_GetEnvelope(ring,&env);
               env.MaxX+=0.5;env.MaxY+=0.5;
               env.MinX=env.MinX<0?0:env.MinX;
               env.MinY=env.MinY<0?0:env.MinY;
               env.MaxX=env.MaxX>ToRef->X1?ToRef->X1:env.MaxX;
               env.MaxY=env.MaxY>ToRef->Y1?ToRef->Y1:env.MaxY;

               if (List)
                  item=Tcl_NewListObj(0,NULL);

               nt+=n=Data_GridOGRQuad(Interp,item,ToDef,ToRef,cell,Mode,'A',area,val1,ToDef->Buffer,env.MinX,env.MinY,env.MaxX,env.MaxY);
               if (List && n) {
                  Tcl_ListObjAppendElement(Interp,List,Tcl_NewIntObj(i));
                  Tcl_ListObjAppendElement(Interp,List,Tcl_NewIntObj(j));
                  Tcl_ListObjAppendElement(Interp,List,item);
               }
#ifdef DEBUG
               fprintf(stderr,"(DEBUG) FSTD_FieldGridConservative: %i hits on grid point %i %i (%.0f %.0f x %.0f %.0f)\n",n,i,j,env.MinX,env.MinY,env.MaxX,env.MaxY);
#endif
            }
         }
      }
#ifdef DEBUG
      fprintf(stderr,"(DEBUG) FSTD_FieldGridConservative: %i total hits\n",nt);
#endif
   }

   /*Finalize and reassign*/
   if (Final && Mode=='N') {
      for(n=0;n<FSIZE2D(ToDef);n++) {
         Def_Get(ToDef,0,n,val0);
         if (ToDef->Buffer[n]!=0.0) {
            val0/=ToDef->Buffer[n];
            Def_Set(ToDef,0,n,val0);
         }
      }
   }

   OGR_G_DestroyGeometry(ring);
   OGR_G_DestroyGeometry(cell);
#endif
   return(TCL_OK);
}

/*--------------------------------------------------------------------------------------------------------------
 * Nom          : <Data_GridOGR>
 * Creation     : Novembre 2004 J.P. Gauthier - CMC/CMOE
 *
 * But          : Importer des donnees dans une bande raster
 *
 * Parametres   :
 *   <Interp>   : L'interpreteur Tcl
 *   <Def>      : Definition de la donnee
 *   <Ref>      : Referentiel de la donnee
 *   <Layer>    : Couche a importer
 *   <Mode>     : Mode de rasterization
 *   <Type>     : Type d'import
 *   <Final>    : Finalisation de l'operation (Averaging en plusieurs passe)
 *   <Field>    : Champs de la couvhe a utiliser
 *   <Value>    : Valeur a assigner
 *
 * Retour       : Code d'erreur standard TCL
 *
 * Remarques    :
 *
 *---------------------------------------------------------------------------------------------------------------
*/
int Data_GridOGR(Tcl_Interp *Interp,TDataDef *Def,TGeoRef *Ref,OGR_Layer *Layer,char Mode,char Type,int Final,char *Field,double Value) {

   long     f,n=0,nt=0;
   double   value,area,t,x0,y0,x1,y1;
   int      fld=-1,pr=1;

#ifdef GDAL126
   OGRCoordinateTransformationH tr=NULL;
   OGRGeometryH                 geom;
   OGREnvelope                  env;

   /*Recuperer la valeur a calculer*/
   if (Field) {
      if (strcmp(Field,"FEATURE_AREA")==0) {
         fld=-2;
      } else if (strcmp(Field,"FEATURE_LENGTH")==0) {
         fld=-3;
      } else {
         fld=OGR_FD_GetFieldIndex(Layer->Def,Field);
         if (fld==-1) {
            Tcl_AppendResult(Interp,"\n   Data_GridOGR: Invalid layer field",(char*)NULL);
            return(TCL_ERROR);
         }
      }
   }

   /*Creer la transformation*/
   if (Layer->Ref->Spatial && Ref->Spatial) {
      tr=OCTNewCoordinateTransformation(Layer->Ref->Spatial,Ref->Spatial);
   } else {
      if (Ref->Grid[0]=='W')
         fprintf(stderr,"(WARNING) Data_GridOGR: Unable to create coordinate transformation, assuming both referential are the same\n");
   }

   if (!Def->Pick)
      Def->Pick=OGR_G_CreateGeometry(wkbLinearRing);
   if (!Def->Poly) {
      Def->Poly=OGR_G_CreateGeometry(wkbPolygon);
      OGR_G_AddGeometryDirectly(Def->Poly,Def->Pick);
   }

   /*Trouve la feature en intersection*/
   for(f=0;f<Layer->NFeature;f++) {

      if (Layer->Select[f]) {
         geom=OGR_G_Clone(OGR_F_GetGeometryRef(Layer->Feature[f]));

         /*Get value to distribute*/
         if (fld==-2) {
            value=OGR_G_GetArea(geom);
         } else if (fld==-3) {
            value=GPC_Length(geom);
         } else if (fld>=0) {
            value=OGR_F_GetFieldAsDouble(Layer->Feature[f],fld);
         } else {
            value=Value;
         }

         /*Copie de la geometrie et transformation dans le bon referentiel*/
         if (tr) {
            if (pr==1 && OGR_G_Transform(geom,tr)!=OGRERR_NONE) {
               fprintf(stderr,"(WARNING) Data_GridOGR: Unable to reproject geometry to local spatial reference\n");
               pr=0;
            }
         } else {
            Data_OGRProject(geom,Layer->Ref,Ref);
         }

         if (Mode=='F') {
             GDAL_Rasterize(Def,Ref,geom,value);
         } else {

            if (Mode=='C' || Mode=='A' || Mode=='N') {
               switch(Type) {
                  case 'A': area=OGR_G_GetArea(geom); break;
                  case 'L': area=GPC_Length(geom); break;
                  case 'P': area=1.0; break;
               }
            } else {
               area=0.0;
            }

            /*Utilise les limites de l'enveloppe pour initialiser la selection*/
            OGR_G_GetEnvelope(geom,&env);
            INVTRANSFORM(Ref,x0,y0,env.MinX,env.MinY);
            INVTRANSFORM(Ref,x1,y1,env.MaxX,env.MaxY);
            if (x0>x1) {
               t=x1;x1=x0;x0=t;
            }
            if (y0>y1) {
               t=y1;y1=y0;y0=t;
            }
            x1+=0.5;y1+=0.5;

            /*Si le feature est dans le raster*/
            if (!(x1<0 || x0>Def->NI || y0>Def->NJ || y1<0)) {
               nt+=n=Data_GridOGRQuad(Interp,NULL,Def,Ref,geom,Mode,Type,area,value,NULL,x0<0?0:x0,y0<0?0:y0,x1>=Def->NI?Def->NI-1:x1,y1>=Def->NJ?Def->NJ-1:y1);
#ifdef DEBUG
               fprintf(stderr,"(DEBUG) Data_GridOGR: %i hits on feature %i of %i (%.0f %.0f x %.0f %.0f)\n",n,f,Layer->NFeature,x0,y0,x1,y1);
#endif
            }
         }
         OGR_G_DestroyGeometry(geom);
      }
   }
#ifdef DEBUG
   fprintf(stderr,"(DEGBU) Data_GridOGR: %i total hits\n",nt);
#endif
   if (tr)
      OCTDestroyCoordinateTransformation(tr);
#endif
   return(TCL_OK);
}

/*--------------------------------------------------------------------------------------------------------------
 * Nom          : <Data_OGRProject>
 * Creation     : Novembre 2005 J.P. Gauthier - CMC/CMOE
 *
 * But          : Transforme les coordonnees d'un object vectoriel OGR dans une autre referentiel
 *
 * Parametres   :
 *   <Geom>     : Object geometrique OGR
 *   <FromRef>  : Reference source
 *   <ToRef>    : Reference destination
 *
 * Retour       : Code d'erreur standard TCL
 *
 * Remarques    :
 *
 *---------------------------------------------------------------------------------------------------------------
*/
void Data_OGRProject(OGRGeometryH Geom,TGeoRef *FromRef,TGeoRef *ToRef) {

   OGRGeometryH geom;
   Vect3d       vr;
   Coord        co;
   int          n;

   for(n=0;n<OGR_G_GetGeometryCount(Geom);n++) {
      geom=OGR_G_GetGeometryRef(Geom,n);
      Data_OGRProject(geom,FromRef,ToRef);
   }

   for(n=0;n<OGR_G_GetPointCount(Geom);n++) {
      OGR_G_GetPoint(Geom,n,&vr[0],&vr[1],&vr[2]);
      FromRef->Project(FromRef,vr[0],vr[1],&co.Lat,&co.Lon,1,1);
      ToRef->UnProject(ToRef,&vr[0],&vr[1],co.Lat,co.Lon,1,1);
      OGR_G_SetPoint(Geom,n,vr[0],vr[1],vr[2]);
   }
}

int GeoScan_Get(TGeoScan *Scan,TDataDef *FromDef,int Dim) {

   register int idx,x,y,n=0;
   int          d=0,sz,dd;
   double       x0,y0,x1,y1;
   double      *transform=NULL;

   dd=Dim-1;
   Scan->N=0;

   /*WKT grid type*/
   if (Scan->FromRef->Grid[0]=='W') {
      transform=Scan->FromRef->Transform;
      for(y=Scan->Y0;y<=Scan->Y1+dd;y++) {
         idx=(y-Scan->FromRef->Y0)*FromDef->NI+(Scan->X0-Scan->FromRef->X0);
         for(x=Scan->X0;x<=Scan->X1+dd;x++,idx++,n++) {
            if (x<=Scan->X1 && y<=Scan->Y1) {
               Scan->V[Scan->N++]=idx;
            }

            if (!Scan->Valid) {
               x0=dd?x-0.5:x;
               y0=dd?y-0.5:y;
               if (transform) {
                  Scan->X[n]=transform[0]+transform[1]*x0+transform[2]*y0;
                  Scan->Y[n]=transform[3]+transform[4]*x0+transform[5]*y0;
               } else {
                  Scan->X[n]=x0;
                  Scan->Y[n]=y0;
               }
            }
         }
      }
      if (!Scan->Valid && Scan->FromRef->Function) {
         OCTTransform(Scan->FromRef->Function,n,Scan->X,Scan->Y,NULL);
      }

      d=dd?2:1;
      sz=8;
   /*Y Grid type*/
   } else if (Scan->FromRef->Grid[0]=='Y') {
      for(n=0;n<FromDef->NI;n++,idx++) {
         Scan->V[n]=idx;
         if (!Scan->Valid) {
            ((float*)Scan->X)[n]=Scan->FromRef->Lon[idx];
            ((float*)Scan->Y)[n]=Scan->FromRef->Lat[idx];
         }
      }
      d=1;
      sz=4;
      Scan->N=n;
   /*Other RPN grids*/
   } else {
      for(y=Scan->Y0;y<=Scan->Y1+dd;y++) {
         idx=(y-Scan->FromRef->Y0)*FromDef->NI+(Scan->X0-Scan->FromRef->X0);
         for(x=Scan->X0;x<=Scan->X1+dd;x++,idx++,n++) {
            if (x<=Scan->X1 && y<=Scan->Y1) {
               Scan->V[Scan->N++]=idx;
            }

            if (!Scan->Valid) {
               ((float*)Scan->X)[n]=dd?x+0.5:x+1.0;
               ((float*)Scan->Y)[n]=dd?y+0.5:y+1.0;
            }
         }
      }
      if (!Scan->Valid) {
         EZLock_RPNInt();
         c_gdllfxy(Scan->FromRef->Id,(float*)Scan->Y,(float*)Scan->X,(float*)Scan->X,(float*)Scan->Y,n);
         EZUnLock_RPNInt();
      }
      d=dd?2:1;
      sz=4;
   }

   /*Project to destination grid*/
   if (!Scan->Valid) {
      if (Scan->ToRef->Grid[0]=='W') {
         transform=Scan->ToRef->InvTransform;
         if (sz==4) {
            for(x=n-1;x>=0;x--) {
               Scan->X[x]=(double)((float*)Scan->X)[x];
               Scan->Y[x]=(double)((float*)Scan->Y)[x];
            }
         }

         if (Scan->ToRef->Function)
            OCTTransform(Scan->ToRef->InvFunction,n,Scan->X,Scan->Y,NULL);

         if (transform) {
            for(x=0;x<n;x++) {
               x0=transform[0]+transform[1]*Scan->X[x]+transform[2]*Scan->Y[x];
               y0=transform[3]+transform[4]*Scan->X[x]+transform[5]*Scan->Y[x];
               Scan->X[x]=x0;
               Scan->Y[x]=y0;
            }
         }
      } else {
         if (sz==8) {
            for(x=0;x<n;x++) {
               ((float*)Scan->X)[x]=(float)((double*)Scan->X)[x];
               ((float*)Scan->Y)[x]=(float)((double*)Scan->Y)[x];
            }
         }

         EZLock_RPNInt();
         c_gdxyfll(Scan->ToRef->Id,(float*)Scan->X,(float*)Scan->Y,(float*)Scan->Y,(float*)Scan->X,n);
         EZUnLock_RPNInt();

         for(x=n-1;x>=0;x--) {
            Scan->X[x]=(double)((float*)Scan->X)[x]-1.0;
            Scan->Y[x]=(double)((float*)Scan->Y)[x]-1.0;
         }
      }
   }
   return(d);
}

/*----------------------------------------------------------------------------
 * Nom      : <Data_GridAverage>
 * Creation : Mai 2006 - J.P. Gauthier - CMC/CMOE
 *
 * But      : Effectue l'interpolation par moyennage minimum ou maximum
 *
 * Parametres  :
 *  <Interp>   : Interpreteur TCL
 *  <ToRef>    : Reference du champs destination
 *  <ToDef>    : Description du champs destination
 *  <FromRef>  : Reference du champs source
 *  <FromDef>  : Description du champs source
 *  <Table>    : Table de donnees a verifier
 *  <TmpDef>   : Description du champs de precalcul (ex: pour VARIANCE, moyenne)
 *  <Mode>     : Mode d'interpolation
 *  <Final>    : Finalisation de l'operation (Averaging en plusieurs passe)
 *
 * Retour:
 *  <TCL_...> : Code d'erreur de TCL.
 *
 * Remarques :
 *
 *----------------------------------------------------------------------------
*/
int Data_GridAverage(Tcl_Interp *Interp,TGeoRef *ToRef,TDataDef *ToDef,TGeoRef *FromRef,TDataDef *FromDef,double *Table,TDataDef *TmpDef,TData_Interp Mode,int Final){

   double        val,vx,di[4],dj[4],*fld,di0,di1,dj0,dj1;
   int          *acc=NULL;
   unsigned long idxt,idxk,idxj;
   unsigned int  n2,ndi,ndj,nij,nijk,k,t,s,n,x,x0,x1,y,y0,y1,dx,dy;
   extern TGeoScan GScan;

   acc=ToDef->Accum;
   fld=ToDef->Buffer;
   nij=FSIZE2D(ToDef);
   nijk=FSIZE3D(ToDef);

   if (Mode!=TD_NOP && Mode!=TD_ACCUM && Mode!=TD_BUFFER) {
      if (!GeoRef_Intersect(ToRef,FromRef,&x0,&y0,&x1,&y1,1)) {
         return(TCL_OK);
      }

      /*In case of average, we need an accumulator*/
      if (Mode==TD_AVERAGE || Mode==TD_VARIANCE || Mode==TD_SQUARE || Mode==TD_NORMALIZED_COUNT || Mode==TD_COUNT) {
         if (!ToDef->Accum) {
            acc=ToDef->Accum=calloc(nij,sizeof(int));
            if (!ToDef->Accum) {
               Tcl_AppendResult(Interp,"Data_GridAverage: Unable to allocate accumulation buffer",(char*)NULL);
               return(TCL_ERROR);
            }
         }
      }

      if (!ToDef->Buffer) {
         fld=ToDef->Buffer=calloc(nijk,sizeof(double));
         if (!ToDef->Buffer) {
            Tcl_AppendResult(Interp,"Data_GridAverage: Unable to allocate buffer",(char*)NULL);
            return(TCL_ERROR);
         }
         for(x=0;x<nijk;x++)
            ToDef->Buffer[x]=ToDef->NoData;
      }
      n2=ToDef->NI>>1;

      /*if > 2048x2048, loop by lines otherwise, do it in one shot*/
      dy=((y1-y0)*(x1-x0))>4194304?0:(y1-y0);
      for(y=y0;y<=y1;y+=(dy+1)) {
         if (!GeoScan_Init(&GScan,ToRef,FromRef,x0,y,x1,y+dy)) {
            Tcl_AppendResult(Interp,"Data_GridAverage: Unable to allocate coordinate scanning buffer",(char*)NULL);
            return(TCL_ERROR);
         }
         s=GeoScan_Get(&GScan,FromDef,FromDef->CellDim);

         /*Loop over source data*/
         dx=0;
         for(x=0,n=0;x<GScan.N;x++,n++) {

            /*Check if we need to skip last x since we change row and last one is end of a cell*/
            if (s>1 && dx==GScan.DX) {
               n++;
               dx=0;
            }
            dx++;

            /*Skip if no mask*/
            if (FromDef->Mask && !FromDef->Mask[GScan.V[x]])
               continue;

            /*Skip if no data*/
            Def_Get(FromDef,0,GScan.V[x],vx);
            if ((isnan(vx) || vx==FromDef->NoData) && Mode!=TD_COUNT)
               continue;

            /*Figure out ordered coverage*/
            if (s>1) {
               di[0]=GScan.X[n];
               dj[0]=GScan.Y[n];
               di[1]=GScan.X[n+1]; di0=FMIN(di[0],di[1]); di1=FMAX(di[0],di[1]);
               dj[1]=GScan.Y[n+1]; dj0=FMIN(dj[0],dj[1]); dj1=FMAX(dj[0],dj[1]);

               di[2]=GScan.X[n+GScan.DX+1]; di0=FMIN(di0,di[2]); di1=FMAX(di1,di[2]);
               dj[2]=GScan.Y[n+GScan.DX+1]; dj0=FMIN(dj0,dj[2]); dj1=FMAX(dj1,dj[2]);
               di[3]=GScan.X[n+GScan.DX+2]; di0=FMIN(di0,di[3]); di1=FMAX(di1,di[3]);
               dj[3]=GScan.Y[n+GScan.DX+2]; dj0=FMIN(dj0,dj[3]); dj1=FMAX(dj1,dj[3]);

               di0=ROUND(di0);dj0=ROUND(dj0);
               di1=ROUND(di1);dj1=ROUND(dj1);
            } else {
               di0=di1=ROUND(GScan.X[n]);
               dj0=dj1=ROUND(GScan.Y[n]);
            }

            /*Are we within the destination field*/
            if (di0>=ToDef->NI || dj0>=ToDef->NJ || di1<0 || dj1<0)
               continue;

            /*Clamp the coordinates*/
            if (di0<0) di0=0;
            if (dj0<0) dj0=0;
            if (di1>ToDef->NI-1) di1=ToDef->NI-1;
            if (dj1>ToDef->NJ-1) dj1=ToDef->NJ-1;

            /*Are we crossing the wrap around*/
            if (ToRef->Type&GRID_WRAP && di0<n2 && di1>n2 && (di1-di0)>n2) {
               val=di0;
               di0=di1;
               di1=val+ToDef->NI;
            }

            /*Check if we need to loop*/
            if (dj0==dj1 && di0==di1) {
               idxt=dj0*ToDef->NI+di0;

               /*Skip if no mask*/
               if (!ToDef->Mask || ToDef->Mask[idxt]) {

                  /*If the previous value is nodata, initialize the counter*/
                  if (isnan(fld[idxt]) || fld[idxt]==ToDef->NoData) {
                     fld[idxt]=(Mode==TD_SUM || Mode==TD_AVERAGE || Mode==TD_VARIANCE || Mode==TD_SQUARE || Mode==TD_NORMALIZED_COUNT || Mode==TD_COUNT)?0.0:(Mode==TD_MAXIMUM?-HUGE_VAL:HUGE_VAL);
                  }

                  switch(Mode) {
                     case TD_MAXIMUM          : if (vx>fld[idxt]) fld[idxt]=vx;
                                                break;
                     case TD_MINIMUM          : if (vx<fld[idxt]) fld[idxt]=vx;
                                                break;
                     case TD_SUM              : fld[idxt]+=vx;
                                                break;
                     case TD_VARIANCE         : acc[idxt]++;
                                                Def_Get(TmpDef,0,idxt,val);
                                                fld[idxt]+=(vx-val)*(vx-val);
                                                break;
                     case TD_SQUARE           : acc[idxt]++;
                                                fld[idxt]+=vx*vx;
                                                break;
                     case TD_COUNT            : acc[idxt]++;
                     case TD_AVERAGE          :
                     case TD_NORMALIZED_COUNT : if (Table) {
                                                   t=0;
                                                   while(t<ToDef->NK) {
                                                      if (vx==Table[t] && t<ToDef->NK) {
                                                         if (Mode!=TD_COUNT) acc[idxt]++;
                                                         fld[t*nij+idxt]++;
                                                         break;
                                                      }
                                                      t++;
                                                   }
                                                } else {
                                                   if (Mode!=TD_COUNT) acc[idxt]++;
                                                   if (vx!=FromDef->NoData)
                                                      fld[idxt]+=vx;
                                                }
                                                break;
                  }
               }

            } else {
               for(ndj=dj0;ndj<=dj1;ndj++) {
                  idxj=ndj*ToDef->NI;
                  for(ndi=di0;ndi<=di1;ndi++) {
                     idxt=idxj+(ndi>=ToDef->NI?ndi-ToDef->NI:ndi);

                     /*Skip if no mask*/
                     if (!ToDef->Mask || ToDef->Mask[idxt]) {

                        /*If the previous value is nodata, initialize the counter*/
                        if (isnan(fld[idxt]) || fld[idxt]==ToDef->NoData) {
                           fld[idxt]=(Mode==TD_SUM || Mode==TD_AVERAGE || Mode==TD_VARIANCE || Mode==TD_SQUARE || Mode==TD_NORMALIZED_COUNT || Mode==TD_COUNT)?0.0:(Mode==TD_MAXIMUM?-HUGE_VAL:HUGE_VAL);
                        }

                        switch(Mode) {
                           case TD_MAXIMUM          : if (vx>fld[idxt]) fld[idxt]=vx;
                                                      break;
                           case TD_MINIMUM          : if (vx<fld[idxt]) fld[idxt]=vx;
                                                      break;
                           case TD_SUM              : fld[idxt]+=vx;
                                                      break;
                           case TD_VARIANCE         : acc[idxt]++;
                                                      Def_Get(TmpDef,0,idxt,val);
                                                      fld[idxt]+=(vx-val)*(vx-val);
                                                      break;
                           case TD_SQUARE           : acc[idxt]++;
                                                      fld[idxt]+=vx*vx;
                                                      break;
                           case TD_COUNT            : acc[idxt]++;
                           case TD_AVERAGE          :
                           case TD_NORMALIZED_COUNT : if (Table) {
                                                         t=0;
                                                         while(t<ToDef->NK) {
                                                            if (vx==Table[t] && t<ToDef->NK) {
                                                               if (Mode!=TD_COUNT) acc[idxt]++;
                                                               fld[t*nij+idxt]++;
                                                               break;
                                                            }
                                                            t++;
                                                         }
                                                      } else {
                                                         if (Mode!=TD_COUNT) acc[idxt]++;
                                                         if (vx!=FromDef->NoData)
                                                            fld[idxt]+=vx;
                                                      }
                                                      break;
                        }
                     }
                  }
               }
            }
         }
      }
   }

   /*Finalize and reassign*/
   if (Final || Mode==TD_ACCUM || Mode==TD_BUFFER) {
      idxk=0;
      for(k=0;k<ToDef->NK;k++) {
         for(x=0;x<nij;x++,idxk++) {
            switch(Mode) {
               case TD_ACCUM:
                  if (acc) {
                     Def_Set(ToDef,0,idxk,acc[x]);
                  } else {
                     Def_Set(ToDef,0,idxk,ToDef->NoData);
                  }
                  break;
               case TD_BUFFER:
                  if (fld) {
                     Def_Set(ToDef,0,idxk,fld[idxk]);
                  } else {
                     Def_Set(ToDef,0,idxk,ToDef->NoData);
                  }
                  break;
               default:
                  if (fld) {
                     if (acc && acc[x]!=0) fld[idxk]/=acc[x];
                     Def_Set(ToDef,0,idxk,fld[idxk]);
                  } else {
                     Def_Set(ToDef,0,idxk,ToDef->NoData);
                  }
           }
         }
      }
   }
   return(TCL_OK);
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

   double lat,lon,i,j;
   float  val,dir;
   int x,y,z=0,idx;

   if (!Band) {
      Tcl_AppendResult(Interp,"GDAL_BandFSTDImport: Invalid band",(char*)NULL);
      return(TCL_ERROR);
   }

   if (!Field) {
      Tcl_AppendResult(Interp,"GDAL_BandFSTDImport: Invalid field",(char*)NULL);
      return(TCL_ERROR);
   }

   if (Field->Spec->Map) {
      Data_PreInit(Field);
      if (Band->Def->NC==1) {
         if (Band->Spec->Map)
            CMap_Free(Band->Spec->Map->Name);
         Band->Spec->Map=Field->Spec->Map;
      }
   }

   for(x=0;x<Band->Def->NI;x++){
      for(y=0;y<Band->Def->NJ;y++){

         /*Get the latlon coordinate of the pixel*/
         Band->Ref->Project(Band->Ref,x,y,&lat,&lon,0,1);

         /*Get the value of the data field at this latlon coordinate*/
         if (Field->Ref->UnProject(Field->Ref,&i,&j,lat,lon,0,1)) {
            Field->Ref->Value(Field->Ref,Field->Def,Field->Spec->InterpDegree[0],0,i,j,Field->Def->Level,&val,&dir);
            if (Field->Spec->Map) {
               VAL2COL(idx,Field->Spec,val);
            }
         } else {
            val=0.0;
            idx=-1;
         }

         if (Field->Spec->Map) {
            if (Band->Def->NC==1) {
               Def_Set(Band->Def,0,FIDX2D(Band->Def,x,y),idx);
            } else {
               for (z=0;z<Band->Def->NC;z++) {
                  if (idx>-1) {
                     Def_Set(Band->Def,z,FIDX2D(Band->Def,x,y),Field->Spec->Map->Color[idx][z]);
                  } else {
                     Def_Set(Band->Def,z,FIDX2D(Band->Def,x,y),0);
                  }
               }
            }
         } else {
            if (Band->Def->NC>=1) {
               Def_Set(Band->Def,0,FIDX2D(Band->Def,x,y),val);
            }
            if (Band->Def->NC>=2) {
               Def_Set(Band->Def,1,FIDX2D(Band->Def,x,y),dir);
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
   int             i,ns,nc,n,di,sz;
   char           *str;

   Tcl_ListObjLength(Interp,Bands,&ns);
   if (!ns) {
      Tcl_AppendResult(Interp,"GDAL_BandWrite: No band specified",(char*)NULL);
      return(TCL_ERROR);
   }

   Tcl_ListObjIndex(Interp,Bands,0,&obj);
   nc=0;
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
         GDALSetGCPs(file->Set,band->NbGCPs,band->GCPs,band->Ref->String);
      } else {
         /*Write Transform*/
         if (band->Ref) {
            if (band->Ref->Transform)
               GDALSetGeoTransform(file->Set,band->Ref->Transform);

            if (band->Ref->Spatial) {
               OSRExportToWkt(band->Ref->Spatial,&str);
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
      if (band->Spec->Map) {
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

         if (band->Tex.Indexed) {
           GDALSetRasterColorInterpretation(band->Band[0],GCI_PaletteIndex);
         }
      }

      /*Write every band*/
      for(i=0;i<band->Def->NC;i++) {
         band->Band[i]=GDALGetRasterBand(file->Set,nc+1);
         if (band->Spec->Desc)
            GDALSetDescription(band->Band[i],band->Spec->Desc);

         if (!isnan(band->Def->NoData))
            GDALSetRasterNoDataValue(band->Band[i],band->Def->NoData);

         GDALRasterIO(band->Band[i],GF_Write,0,0,band->Def->NI,band->Def->NJ,band->Def->Data[i],band->Def->NI,band->Def->NJ,TD2GDAL[band->Def->Type],0,0);
         nc++;
      }
   }
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

   int          i,c,w,h,idx,*histo,ex=0,tr=1;
   double       lat,lon,x0,y0,dval,min,max;
   GDAL_Band   *band;
   Tcl_Obj     *obj,*lst;

   static CONST char *sopt[] = { "-tag","-nodata","-max","-min","-avg","-grid","-gridlat","-gridlon","-gridpoint","-coordpoint",
      "-gridvalue","-coordvalue","-project","-unproject","-extent","-histogram","-celldim",NULL };
   enum        opt {  TAG,NODATA,MAX,MIN,AVG,GRID,GRIDLAT,GRIDLON,GRIDPOINT,COORDPOINT,GRIDVALUE,COORDVALUE,PROJECT,UNPROJECT,EXTENT,HISTOGRAM,CELLDIM };

   band=GDAL_BandGet(Name);
   if (!band) {
      Tcl_AppendResult(Interp,"\n   GDAL_BandStat: Band name unknown: \"",Name,"\"",(char *)NULL);
      return TCL_ERROR;
   }

   if (!band->Ref) {
      return TCL_OK;
   }

   for(i=0;i<Objc;i++) {

      if (Tcl_GetIndexFromObj(Interp,Objv[i],sopt,"option",0,&idx)!=TCL_OK) {
         return TCL_ERROR;
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
                   free(band->Stat);
                   band->Stat=NULL;
               }
               Tcl_GetDoubleFromObj(Interp,Objv[++i],&band->Def->NoData);
               for (c=0;c<band->Def->NC;c++) {
                  if (band->Band[c])
                     GDALSetRasterNoDataValue(band->Band[c],band->Def->NoData);
               }
            }
            break;

         case HISTOGRAM:
            if (Objc!=2 && Objc!=7) {
               Tcl_WrongNumArgs(Interp,2,Objv,"band index [min max bin approx]");
            } else {
               Tcl_GetIntFromObj(Interp,Objv[++i],&c);
               if (c<band->Def->NC) {
                  min=band->Stat[c].Min;
                  max=band->Stat[c].Max;
                  h=256;
                  w=1;
                  if (Objc>2) {
                     Tcl_GetDoubleFromObj(Interp,Objv[++i],&min);
                     Tcl_GetDoubleFromObj(Interp,Objv[++i],&max);
                     Tcl_GetIntFromObj(Interp,Objv[++i],&h);
                     Tcl_GetBooleanFromObj(Interp,Objv[++i],&w);
                  }
                  histo=(int*)malloc(h*sizeof(int));
                  GeoTex_Lock();
                  GDALGetRasterHistogram(band->Band[c],min,max,h,histo,FALSE,w,GDALDummyProgress,NULL);
                  GeoTex_UnLock();
                  obj=Tcl_NewListObj(0,NULL);
                  for(c=0;c<h;c++) {
                     Tcl_ListObjAppendElement(Interp,obj,Tcl_NewIntObj(histo[c]));
                  }
                  free(histo);
                  Tcl_SetObjResult(Interp,obj);
               }
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
            } else if (Objc==2) {
               Tcl_GetIntFromObj(Interp,Objv[++i],&c);
               if (c<band->Def->NC) {
                  obj=Tcl_NewListObj(0,NULL);
                  Tcl_ListObjAppendElement(Interp,obj,Data_Val2Obj(band->Def,band->Stat[c].Max));
                  Tcl_ListObjAppendElement(Interp,obj,Tcl_NewDoubleObj(band->Stat[c].MaxLoc.Lat));
                  Tcl_ListObjAppendElement(Interp,obj,Tcl_NewDoubleObj(band->Stat[c].MaxLoc.Lon));
                  Tcl_ListObjAppendElement(Interp,obj,Tcl_NewDoubleObj(band->Stat[c].MaxLoc.Elev));
                  Tcl_SetObjResult(Interp,obj);
               }
            }
            break;

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
            } else if (Objc==2) {
               Tcl_GetIntFromObj(Interp,Objv[++i],&c);
               if (c<band->Def->NC) {
                  obj=Tcl_NewListObj(0,NULL);
                  Tcl_ListObjAppendElement(Interp,obj,Data_Val2Obj(band->Def,band->Stat[c].Min));
                  Tcl_ListObjAppendElement(Interp,obj,Tcl_NewDoubleObj(band->Stat[c].MinLoc.Lat));
                  Tcl_ListObjAppendElement(Interp,obj,Tcl_NewDoubleObj(band->Stat[c].MinLoc.Lon));
                  Tcl_ListObjAppendElement(Interp,obj,Tcl_NewDoubleObj(band->Stat[c].MinLoc.Elev));
                  Tcl_SetObjResult(Interp,obj);
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
            obj=Tcl_NewListObj(0,NULL);
            for (w=0;w<band->Def->NI;w++) {
               for (h=0;h<band->Def->NJ;h++) {
                  band->Ref->Project(band->Ref,w,h,&lat,&lon,0,1);
                  Tcl_ListObjAppendElement(Interp,obj,Tcl_NewDoubleObj(lat));
                  Tcl_ListObjAppendElement(Interp,obj,Tcl_NewDoubleObj(lon));
               }
            }
            Tcl_SetObjResult(Interp,obj);
            break;

         case GRIDLAT:
            obj=Tcl_NewListObj(0,NULL);
            for (w=0;w<band->Def->NI;w++) {
               for (h=0;h<band->Def->NJ;h++) {
                  band->Ref->Project(band->Ref,w,h,&lat,&lon,0,1);
                  Tcl_ListObjAppendElement(Interp,obj,Tcl_NewDoubleObj(lat));
               }
            }
            Tcl_SetObjResult(Interp,obj);
            break;

         case GRIDLON:
            obj=Tcl_NewListObj(0,NULL);
            for (w=0;w<band->Def->NI;w++) {
               for (h=0;h<band->Def->NJ;h++) {
                  band->Ref->Project(band->Ref,w,h,&lat,&lon,0,1);
                  Tcl_ListObjAppendElement(Interp,obj,Tcl_NewDoubleObj(lon));
               }
            }
            Tcl_SetObjResult(Interp,obj);
            break;

         case PROJECT:
            tr=0;
            ex=1;
         case GRIDPOINT:
            obj=Tcl_NewListObj(0,NULL);
            Tcl_GetDoubleFromObj(Interp,Objv[++i],&x0);
            Tcl_GetDoubleFromObj(Interp,Objv[++i],&y0);
            if (Objc==4) {
               Tcl_GetBooleanFromObj(Interp,Objv[++i],&ex);
            }
            band->Ref->Project(band->Ref,x0,y0,&lat,&lon,ex,tr);
            Tcl_ListObjAppendElement(Interp,obj,Tcl_NewDoubleObj(lat));
            Tcl_ListObjAppendElement(Interp,obj,Tcl_NewDoubleObj(lon));
            Tcl_SetObjResult(Interp,obj);
            break;

         case UNPROJECT:
            tr=0;
            ex=1;
         case COORDPOINT:
            obj=Tcl_NewListObj(0,NULL);
            Tcl_GetDoubleFromObj(Interp,Objv[++i],&lat);
            Tcl_GetDoubleFromObj(Interp,Objv[++i],&lon);
            if (Objc==4) {
               Tcl_GetBooleanFromObj(Interp,Objv[++i],&ex);
            }
            band->Ref->UnProject(band->Ref,&x0,&y0,lat,lon,ex,tr);
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

               if (Objc==5) {
                  Tcl_GetIntFromObj(Interp,Objv[++i],&h);
               } else {
                  h=0;
               }
               if (Objc>3) {
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
                   free(band->Stat);
                   band->Stat=NULL;
               }
            }
            break;

         case COORDVALUE:
            Tcl_GetDoubleFromObj(Interp,Objv[++i],&lat);
            Tcl_GetDoubleFromObj(Interp,Objv[++i],&lon);

            if (lat==-999.0) {
                Tcl_SetObjResult(Interp,Tcl_NewStringObj("-",-1));
                return(TCL_OK);
            }
            if (band->Ref->UnProject(band->Ref,&x0,&y0,lat,lon,0,1)) {
               DEFCLAMP(band->Def,x0,y0);
               x0=ROUND(x0);
               y0=ROUND(y0);

               if (Objc==4) {
                  Tcl_GetDoubleFromObj(Interp,Objv[++i],&dval);
                  Def_Set(band->Def,0,FIDX2D(band->Def,(int)x0,(int)y0),dval);
                  if (band->Stat) {
                     free(band->Stat);
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

   int         i,j,idx,nidx,nlst,order=1;
   double      tra[6],inv[6],*tm=NULL,*im=NULL;
   GDAL_Band  *band;
   TGeoRef    *ref;
   Tcl_Obj    *obj,*lst;

   static CONST char *sopt[] = { "-active","-mask","-georef","-projection","-transform","-invtransform","-indexed","-gcps","-width","-height","-nb","-type",NULL };
   enum        opt {  ACTIVE,MASK,GEOREF,PROJECTION,TRANSFORM,INVTRANSFORM,INDEXED,GCPS,WIDTH,HEIGHT,NB,TYPE };

   band=GDAL_BandGet(Name);
   if (!band) {
      Tcl_AppendResult(Interp,"\n   GDAL_BandDefine: Band name unknown: \"",Name,"\"",(char *)NULL);
      return TCL_ERROR;
   }

   for(i=0;i<Objc;i++) {

      if (Tcl_GetIndexFromObj(Interp,Objv[i],sopt,"option",0,&idx)!=TCL_OK) {
         return TCL_ERROR;
      }

      switch ((enum opt)idx) {

        case MASK:
            if (Objc==1) {
               Tcl_SetObjResult(Interp,Tcl_NewStringObj("",-1));
            } else {
               band->OGRMask=OGR_GeometryGet(Tcl_GetString(Objv[++i]));
               if (band->OGRMask) {
                  if (OGR_G_TransformTo(band->OGRMask,band->Ref->Spatial)!=OGRERR_NONE) {
                     fprintf(stderr,"(WARNING) GDAL_BandDefine: Unable to reproject mask geometry to local spatial reference\n");
                  }
               }
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

               if (band->Ref->GCPTransform) {
                  GDALDestroyGCPTransformer(band->Ref->GCPTransform);
                  band->Ref->GCPTransform=NULL;
               }
               if (band->Ref->TPSTransform) {
                  GDALDestroyTPSTransformer(band->Ref->TPSTransform);
                  band->Ref->TPSTransform=NULL;
               }

               switch(order) {
                  case 0:
                     if (!band->Ref->Transform) {
                        band->Ref->Transform=(double*)calloc(6,sizeof(double));
                     }
                     if (!band->Ref->InvTransform) {
                        band->Ref->InvTransform=(double*)calloc(6,sizeof(double));
                     }
                     if (!(GDALGCPsToGeoTransform(band->NbGCPs,band->GCPs,band->Ref->Transform,1))) {
                        Tcl_AppendResult(Interp,"\n   GDAL_BandDefine: () Unable to fit control points",(char*)NULL);
                        return(TCL_ERROR);
                     }
                     GDALInvGeoTransform(band->Ref->Transform,band->Ref->InvTransform);
                     break;
                  case 1:
                  case 2:
                  case 3:
                     if (band->Ref->Transform) {
                        free(band->Ref->Transform);
                        band->Ref->Transform=NULL;
                     }
                     if (band->Ref->InvTransform) {
                        free(band->Ref->InvTransform);
                        band->Ref->InvTransform=NULL;
                     }
                     if (!(band->Ref->GCPTransform=(void*)GDALCreateGCPTransformer(band->NbGCPs,band->GCPs,order,FALSE))) {
                        Tcl_AppendResult(Interp,"\n   GDAL_BandDefine: (GPC) Unable to fit control points",(char*)NULL);
                        return(TCL_ERROR);
                     }
                     break;

                  case 4:
                     if (band->Ref->Transform) {
                        free(band->Ref->Transform);
                        band->Ref->Transform=NULL;
                     }
                     if (band->Ref->InvTransform) {
                        free(band->Ref->InvTransform);
                        band->Ref->InvTransform=NULL;
                     }
                     if (!(band->Ref->TPSTransform=(void*)GDALCreateTPSTransformer(band->NbGCPs,band->GCPs,FALSE))) {
                        Tcl_AppendResult(Interp,"\n   GDAL_BandDefine: (TPS) Unable to fit control points",(char*)NULL);
                        return(TCL_ERROR);
                     }
                     break;
               }
               GeoTex_ClearCoord(&band->Tex,NULL);
            }
            break;

         case GEOREF:
            if (Objc==1) {
               if (band->Ref)
                  Tcl_SetObjResult(Interp,Tcl_NewStringObj(band->Ref->Name,-1));
            } else {
               ref=GeoRef_Get(Tcl_GetString(Objv[++i]));
               if (!ref) {
                  Tcl_AppendResult(Interp,"\n   GDAL_BandDefine: Georef name unknown: \"",Tcl_GetString(Objv[i]),"\"",(char *)NULL);
                  return TCL_ERROR;
               }
               if (ref!=band->Ref) {
                  if (band->Ref)
                     GeoRef_Destroy(Interp,band->Ref->Name);
                  band->Ref=ref;
                  GeoRef_Incr(band->Ref);
                  GeoRef_Size(ref,0,0,0,band->Def->NI-1,band->Def->NJ-1,band->Def->NK-1,ref->BD);
                  GeoTex_ClearCoord(&band->Tex,NULL);
               }
            }
            break;

         case PROJECTION:
             if (Objc==1) {
               if (band->Ref && band->Ref->String)
                  Tcl_SetObjResult(Interp,Tcl_NewStringObj(band->Ref->String,-1));
            } else {
               ++i;
               if (band->Ref && !band->Ref->String && strlen(Tcl_GetString(Objv[i]))==0)
                  break;

               if (band->Ref && band->Ref->String && strlen(band->Ref->String)==strlen(Tcl_GetString(Objv[i])) && strcmp(Tcl_GetString(Objv[i]),band->Ref->String)==0) {
               } else {
                  ref=band->Ref;
                  if (ref) {
                     band->Ref=GeoRef_WKTSetup(band->Def->NI,band->Def->NJ,band->Def->NK,LVL_UNDEF,NULL,Tcl_GetString(Objv[i]),band->Ref->Transform,band->Ref->InvTransform,NULL);
                     GeoRef_Destroy(Interp,ref->Name);
                  } else {
                    band->Ref=GeoRef_WKTSetup(band->Def->NI,band->Def->NJ,band->Def->NK,LVL_UNDEF,NULL,Tcl_GetString(Objv[i]),NULL,NULL,NULL);
                  }
                  GeoTex_ClearCoord(&band->Tex,NULL);
               }
            }
            break;

         case TRANSFORM:
            if (Objc==1) {
               if (band->Ref && band->Ref->Transform) {
                  obj=Tcl_NewListObj(0,NULL);
                  for(j=0;j<6;j++) {
                     Tcl_ListObjAppendElement(Interp,obj,Tcl_NewDoubleObj(band->Ref->Transform[j]));
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
                     fprintf(stderr,"(WARNING) GDAL_BandDefine: Unable to generate the inverse transform matrix\n");
                     im=NULL;
                  } else {
                     im=inv;
                  }
               }
               if (!band->Ref || !band->Ref->Transform || !tm || memcmp(tm,band->Ref->Transform,6*sizeof(double))!=0) {
                  ref=band->Ref;
                  if (ref) {
                     band->Ref=GeoRef_WKTSetup(band->Def->NI,band->Def->NJ,band->Def->NK,LVL_UNDEF,NULL,band->Ref->String,tm,im,NULL);
                     GeoRef_Size(band->Ref,0,0,0,band->Def->NI-1,band->Def->NJ-1,band->Def->NK-1,ref->BD);
                     GeoRef_Destroy(Interp,ref->Name);
                  } else {
                     band->Ref=GeoRef_WKTSetup(band->Def->NI,band->Def->NJ,band->Def->NK,LVL_UNDEF,NULL,NULL,tm,im,NULL);
                  }
                  GeoTex_ClearCoord(&band->Tex,NULL);
               }
            }
            break;

         case INVTRANSFORM:
            if (Objc==1) {
               if (band->Ref && band->Ref->InvTransform) {
                  obj=Tcl_NewListObj(0,NULL);
                  for(j=0;j<6;j++) {
                     Tcl_ListObjAppendElement(Interp,obj,Tcl_NewDoubleObj(band->Ref->InvTransform[j]));
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
                     fprintf(stderr,"(WARNING) GDAL_BandDefine: Unable to generate the transform matrix\n");
                     tm=NULL;
                  } else {
                     tm=tra;
                  }
               }
               if (!band->Ref || !band->Ref->InvTransform || !tm || memcmp(tm,band->Ref->InvTransform,6*sizeof(double))!=0) {
                  ref=band->Ref;
                  if (ref) {
                     band->Ref=GeoRef_WKTSetup(band->Def->NI,band->Def->NJ,band->Def->NK,LVL_UNDEF,NULL,band->Ref->String,tm,im,NULL);
                     GeoRef_Size(band->Ref,0,0,0,band->Def->NI-1,band->Def->NJ-1,band->Def->NK-1,ref->BD);
                     GeoRef_Destroy(Interp,ref->Name);
                  } else {
                     band->Ref=GeoRef_WKTSetup(band->Def->NI,band->Def->NJ,band->Def->NK,LVL_UNDEF,NULL,NULL,tm,im,NULL);
                  }
                  GeoTex_ClearCoord(&band->Tex,NULL);
               }
            }
            break;

         case ACTIVE:
            if (Objc==1) {
               Tcl_SetObjResult(Interp,Tcl_NewBooleanObj(band->Active));
            } else {
               Tcl_GetBooleanFromObj(Interp,Objv[++i],&band->Active);
            }
            break;
      }
   }
   return TCL_OK;
}

/*----------------------------------------------------------------------------
 * Nom      : <GDAL_BandGetStat>
 * Creation : Aout 2004 - J.P. Gauthier - CMC/CMOE
 *
 * But      : Extrait des statistiques d'un champ.
 *            (Minimum,Maximum,Moyenne,LatMin,LatMax,LonMin,LonMax)
 *
 * Parametres :
 *  <Field>   : Champs a utiliser
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
      Band->Stat=(TDataStat*)malloc(4*sizeof(TDataStat));

   for (c=0;c<Band->Def->NC;c++) {
      if (Band->Band[c]) {
         GeoTex_Lock();
         GDALComputeRasterMinMax(Band->Band[c],TRUE,minmax);
         GeoTex_UnLock();
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

         Band->Stat[c].MinLoc.Lat=0;
         Band->Stat[c].MinLoc.Lon=0;
         Band->Stat[c].MinLoc.Elev=0;
         Band->Stat[c].MaxLoc.Lat=0;
         Band->Stat[c].MaxLoc.Lon=0;
         Band->Stat[c].MaxLoc.Elev=0;

         if (Band->Ref && Band->Ref->Project) {
            /*Recuperer les coordonnees latlon des min max*/
            Band->Ref->Project(Band->Ref,pts[0],pts[1],&Band->Stat[c].MinLoc.Lat,&Band->Stat[c].MinLoc.Lon,0,1);
            Band->Ref->Project(Band->Ref,pts[2],pts[3],&Band->Stat[c].MaxLoc.Lat,&Band->Stat[c].MaxLoc.Lon,0,1);
         }
      }
   }
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

   int         n,id;
   GLuint      tx;
   GLhandleARB prog;
   TGeoTexTile *tile;

   if (!Band) {
      fprintf(stderr,"(ERROR) GDAL_BandRender: Invalid band object\n");
      return(0);
   }

   if (!Band->Active) {
      return(1);
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
   if (!Band->Tex.ThreadId && !VP->Secondary)
      Tcl_CreateThread(&id,GeoTex_ThreadProc,Band,TCL_THREAD_STACK_DEFAULT,TCL_THREAD_NOFLAGS);

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

   if (Band->OGRMask) {
      glColorMask(GL_FALSE,GL_FALSE,GL_FALSE,GL_FALSE);
      glStencilMask(0x04);
      glStencilFunc(GL_ALWAYS,0x4,0x4);
      glStencilOp(GL_REPLACE,GL_REPLACE,GL_REPLACE);
      OGR_GeometryRender(Proj,VP,Band->Ref,NULL,Band->OGRMask,0.0,0.0);
      glStencilFunc(GL_EQUAL,0x04,0x04);
      glStencilOp(GL_KEEP,GL_ZERO,GL_ZERO);
      glColorMask(GL_TRUE,GL_TRUE,GL_TRUE,GL_TRUE);
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

//   GLRender->Prog[PROG_DATATEX]=GLShader_Load("/home/afsr/005/eer_SPI/LibTkGL/TclData","DataTex");
   if ((prog=GLRender->Prog[PROG_DATATEX])) {
      glUseProgramObjectARB(prog);

   if (Band->Spec->Map) {
         glGenTextures(1,&tx);
         glActiveTexture(GL_TEXTURE1);
         glBindTexture(GL_TEXTURE_1D,tx);
         glTexParameteri(GL_TEXTURE_1D,GL_TEXTURE_WRAP_S,GL_CLAMP);
         glTexParameteri(GL_TEXTURE_1D,GL_TEXTURE_MIN_FILTER,GL_LINEAR);
         glTexParameteri(GL_TEXTURE_1D,GL_TEXTURE_MAG_FILTER,GL_LINEAR);
         glTexImage1D(GL_TEXTURE_1D,0,GL_RGBA,Band->Spec->Map->NbPixels,0,GL_RGBA,GL_UNSIGNED_BYTE,Band->Spec->Map->Color);
      }
      glUniform1iARB(GLShader_UniformGet(prog,"Map"),0);

      /*Setup 1D Colormap Texture*/
      if (Band->Tex.Indexed) {
         Band->Tex.Scale[0]=Band->Tex.Scale[1]=Band->Tex.Scale[2]=Band->Tex.Scale[3]=-1.0;
         Band->Tex.Bias[0]=Band->Tex.Bias[1]=Band->Tex.Bias[2]=Band->Tex.Bias[3]=0.0;
      } else {
         Band->Tex.Scale[0]=Band->Tex.Scale[1]=Band->Tex.Scale[2]=Band->Tex.Scale[3]=-1.0;
         Band->Tex.Bias[0]=Band->Tex.Bias[1]=Band->Tex.Bias[2]=Band->Tex.Bias[3]=0.0;

         if (Band->Spec->Map) {
            if (Band->Def->NC==1) {
               glUniform1iARB(GLShader_UniformGet(prog,"Map"),1);
            }

            for (n=0;n<Band->Def->NC;n++) {
               switch(Band->Def->Type) {
                  case TD_UByte:  Band->Tex.Scale[n]=((0x1<<8)-1)/(Band->Spec->Map->Max[n]-Band->Spec->Map->Min[n]);
                                  Band->Tex.Bias[n]=-Band->Spec->Map->Min[n]/((0x1<<8)-1);
                                  break;
                  case TD_Byte:   Band->Tex.Scale[n]=((0x1<<7)-1)/(Band->Spec->Map->Max[n]-Band->Spec->Map->Min[n]);
                                  Band->Tex.Bias[n]=-Band->Spec->Map->Min[n]/((0x1<<7)-1);
                                  break;
                  case TD_UInt16: Band->Tex.Scale[n]=((0x1<<16)-1)/(Band->Spec->Map->Max[n]-Band->Spec->Map->Min[n]);
                                  Band->Tex.Bias[n]=-Band->Spec->Map->Min[n]/((0x1<<16)-1);
                                  break;
                  case TD_Int16:  Band->Tex.Scale[n]=((0x1<<15)-1)/(Band->Spec->Map->Max[n]-Band->Spec->Map->Min[n]);
                                  Band->Tex.Bias[n]=-Band->Spec->Map->Min[n]/((0x1<<15)-1);
                                  break;
                  case TD_UInt32:
                  case TD_Int32:
                  case TD_Float32:
                  case TD_Float64: Band->Tex.Scale[n]=1.0/(Band->Spec->Map->Max[n]-Band->Spec->Map->Min[n]);
                                   Band->Tex.Bias[n]=-Band->Spec->Map->Min[n];
                                   break;
               }
            }
         }
      }
      glUniform1iARB(GLShader_UniformGet(prog,"Colormap"),1);
      glUniform1iARB(GLShader_UniformGet(prog,"Data"),0);
      glUniform1fARB(GLShader_UniformGet(prog,"Cylindric"),(Proj->Type->Def==PROJCYLIN?Proj->Params->L:-999.0));
      glUniform4fvARB(GLShader_UniformGet(prog,"Sc"),1,Band->Tex.Scale);
      glUniform4fvARB(GLShader_UniformGet(prog,"Bc"),1,Band->Tex.Bias);
      glActiveTexture(GL_TEXTURE0);
   }

   GeoTex_Render(Band,Band->Tex.Tile,Proj,VP);

   if (Band->Spec->Map)
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

   return(1);
}

