/*=========================================================
 * Environnement Canada
 * Centre Meteorologique Canadien
 * 2100 Trans-Canadienne
 * Dorval, Quebec
 *
 * Projet       : Lecture et traitements de fichiers raster
 * Fichier      : OGR_Math.c
 * Creation     : Juillet 2005 - J.P. Gauthier
 *
 * Description  : Fonctions de calculs sur les objets geometriques.
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
 * Modification :
 *
 *   Nom        :
 *   Date       :
 *   Description:
 *
 *=========================================================
 */

#include "tclOGR.h"

int GPC_QSortInter(const Vect3d *A,const Vect3d *B){

   if (*A[2]<*B[2]) {
      return(-1);
   } else if (*A[2]>*B[2]) {
      return(1);
   } else {
      return(0);
   }
}

OGRGeometryH GPC_Clip(OGRGeometryH Line,OGRGeometryH Poly) {

   OGRGeometryH clip=NULL;

   if (!(OGR_G_GetGeometryCount(Poly))) {
      return(NULL);
   }

   clip=OGR_G_CreateGeometry(wkbMultiLineString);
   GPC_ClipSegment(Line,Poly,clip);

   return(clip);
}

int GPC_ClipSegment(OGRGeometryH Line,OGRGeometryH Poly,OGRGeometryH Clip) {

   OGRGeometryH line,point,ring;
   Vect3d       pt0,pt1,ppt0,ppt1,inter[16];
   int          n,np,nr,nb,nbring,nbinter,in=0;

   for (n=0;n<OGR_G_GetGeometryCount(Line);n++) {
      line=OGR_G_GetGeometryRef(Line,n);
      GPC_ClipSegment(line,Poly,Clip);
   }

   line=NULL;
   ring=OGR_G_GetGeometryRef(Poly,0);
   point=OGR_G_CreateGeometry(wkbPoint);
   OGR_G_AddPoint_2D(point,0.0,0.0);

   if ((nb=OGR_G_GetPointCount(Line))) {
      OGR_G_GetPoint(Line,0,&pt0[0],&pt0[1],&pt0[2]);
      OGR_G_SetPoint_2D(point,0,pt0[0],pt0[1]);
      in=GPC_PointPolyIntersect(point,ring);
      /*Add the current point if inside*/
      if (in) {
         if (!line) line=OGR_G_CreateGeometry(wkbLineString);
         OGR_G_AddPoint_2D(line,pt0[0],pt0[1]);
      }

      /*Loop on the line segments*/
      for (n=1;n<OGR_G_GetPointCount(Line);n++) {
         OGR_G_GetPoint(Line,n,&pt1[0],&pt1[1],&pt1[2]);
         nbinter=0;

         /*Loop on the polygon rings*/
         for (nr=0;nr<OGR_G_GetGeometryCount(Poly);nr++) {
            ring=OGR_G_GetGeometryRef(Poly,nr);

            /*Loop on the ring segments*/
            if (!(nb=OGR_G_GetPointCount(ring)))
               continue;

            OGR_G_GetPoint(ring,0,&ppt0[0],&ppt0[1],&ppt0[2]);
            for (np=1;np<nb;np++) {
               OGR_G_GetPoint(ring,np,&ppt1[0],&ppt1[1],&ppt1[2]);

               /*If intersect, add point*/
               if ((GPC_SegmentIntersect(pt0,pt1,ppt0,ppt1,inter[nbinter]))==1) {
                  nbinter++;
               }
               Vect_Assign(ppt0,ppt1);
            }
         }

         /*Order the intersections*/
         if (nbinter>1)
            qsort(inter,nbinter,sizeof(Vect3d),GPC_QSortInter);

         /*Add intersections to the linestring*/
         for (nr=0;nr<nbinter;nr++) {
            if (!line) line=OGR_G_CreateGeometry(wkbLineString);
            OGR_G_AddPoint_2D(line,inter[nr][0],inter[nr][1]);
            /*Flip inside/outside flag*/
            in=!in;
            if (!in && line) {
               OGR_G_AddGeometry(Clip,line);
               line=NULL;
            }
         }

         /*If still in, add the point*/
         if (in) {
            OGR_G_SetPoint_2D(point,0,pt1[0],pt1[1]);
            if (GPC_PointPolyIntersect(point,ring)) {
               OGR_G_AddPoint_2D(line,pt1[0],pt1[1]);
            }
         }
         Vect_Assign(pt0,pt1);
      }
      if (line) {
         if (OGR_G_GetPointCount(line)>1) {
            OGR_G_AddGeometry(Clip,line);
         }
      }
   }
   OGR_G_DestroyGeometry(point);
   return(1);
}

void GPC_FromOGR(gpc_polygon *Poly,OGRGeometryH *Geom) {

   OGRGeometryH       geom;
   OGRwkbGeometryType type;
   unsigned long      n,nb,g,nc;
   double             tmpd;

   type=wkbFlatten(OGR_G_GetGeometryType(Geom));

   if (type==wkbMultiPolygon || type==wkbGeometryCollection) {
      for (g=0;g<OGR_G_GetGeometryCount(Geom);g++) {
         geom=OGR_G_GetGeometryRef(Geom,g);
         GPC_FromOGR(Poly,geom);
      }
   } else if (type==wkbPolygon) {
      if (nc=OGR_G_GetGeometryCount(Geom)) {
         Poly->hole=(int*)realloc(Poly->hole,(Poly->num_contours+nc)*(sizeof(int)));
         Poly->contour=(gpc_vertex_list*)realloc(Poly->contour,(Poly->num_contours+nc)*(sizeof(gpc_vertex_list)));

         for (g=0;g<nc;g++) {
            geom=OGR_G_GetGeometryRef(Geom,g);
            nb=OGR_G_GetPointCount(geom);
            Poly->contour[Poly->num_contours+g].num_vertices=nb;
            Poly->hole[Poly->num_contours+g]=(g==0?FALSE:TRUE);
            Poly->contour[Poly->num_contours+g].vertex=(nb?(gpc_vertex*)malloc(nb*sizeof(gpc_vertex)):NULL);

           for (n=0;n<nb;n++) {
               OGR_G_GetPoint(geom,n,&Poly->contour[Poly->num_contours+g].vertex[n].x,&Poly->contour[Poly->num_contours+g].vertex[n].y,&tmpd);
            }
         }
         Poly->num_contours+=nc;
      }
   } else {
      fprintf(stderr,"(ERROR) GPC_FromOGR: Unsupported geometry type\n");
   }
}

void GPC_ToOGR(gpc_polygon *Poly,OGRGeometryH *Geom) {

   OGRGeometryH  geom,ring,poly=NULL,multi=NULL;
   unsigned long n,g,nb=0;

   /*Check for multiple polygon (more thant 1 non-hole contour)*/
   for(n=0;n<Poly->num_contours;n++) {
      if (!Poly->hole[n]) {
         nb++;
      }
   }
   if (nb>1) {
      multi=OGR_G_CreateGeometry(wkbMultiPolygon);
   }

   /*Process external rings*/
   for(g=0;g<Poly->num_contours;g++) {
      if (!Poly->hole[g]) {
         ring=OGR_G_CreateGeometry(wkbLinearRing);
         for (n=0;n<Poly->contour[g].num_vertices;n++) {
            OGR_G_AddPoint_2D(ring,Poly->contour[g].vertex[n].x,Poly->contour[g].vertex[n].y);
         }
         poly=OGR_G_CreateGeometry(wkbPolygon);
         OGR_G_AddGeometryDirectly(poly,ring);
         if (multi)
            OGR_G_AddGeometryDirectly(multi,poly);
      }
   }

   /*In case of no rings, create empty geometry*/
   if (!poly) {
      poly=OGR_G_CreateGeometry(wkbPolygon);
   } else {
      /*Process internal rings*/
      for(g=0;g<Poly->num_contours;g++) {
         if (Poly->hole[g]) {
            ring=OGR_G_CreateGeometry(wkbLinearRing);
            for (n=0;n<Poly->contour[g].num_vertices;n++) {
               OGR_G_AddPoint_2D(ring,Poly->contour[g].vertex[n].x,Poly->contour[g].vertex[n].y);
            }

            if (multi) {
               for (n=0;n<OGR_G_GetGeometryCount(multi);n++) {
                  geom=OGR_G_GetGeometryRef(multi,n);
                  if (GPC_Intersect(geom,ring)) {
                     OGR_G_AddGeometryDirectly(geom,ring);
                     break;
                  }
               }
            } else {
               OGR_G_AddGeometryDirectly(poly,ring);
            }
         }
      }
   }
   *Geom=multi?multi:poly;
}

void GPC_New(gpc_polygon *Poly) {

   Poly->num_contours=0;
   Poly->hole=NULL;
   Poly->contour=NULL;
}

OGRGeometryH GPC_OnOGR(gpc_op Op,OGRGeometryH Geom0,OGRGeometryH Geom1) {

   gpc_polygon  poly0,poly1,poly;
   OGRGeometryH geom=NULL;

   GPC_New(&poly0);
   GPC_New(&poly1);

   GPC_FromOGR(&poly0,Geom0);
   GPC_FromOGR(&poly1,Geom1);

   gpc_polygon_clip(Op,&poly0,&poly1,&poly);

   GPC_ToOGR(&poly,&geom);

   gpc_free_polygon(&poly);
   gpc_free_polygon(&poly0);
   gpc_free_polygon(&poly1);

   return(geom);
}

int GPC_Intersect(OGRGeometryH Geom0,OGRGeometryH Geom1) {

   int n0,n1,t0,t1,npt;
   Vect3d v0,v1;
   OGRGeometryH pt;

   /*Boucle recursive sur les sous geometrie*/
   if (OGR_G_GetGeometryType(Geom0)!=wkbPolygon && (n0=OGR_G_GetGeometryCount(Geom0))) {
      for(n0=0;n0<OGR_G_GetGeometryCount(Geom0);n0++) {
         if (GPC_Intersect(OGR_G_GetGeometryRef(Geom0,n0),Geom1)) {
            return(1);
         }
      }
   }

   if (OGR_G_GetGeometryType(Geom1)!=wkbPolygon && (n1=OGR_G_GetGeometryCount(Geom1))) {
      for(n1=0;n1<OGR_G_GetGeometryCount(Geom1);n1++) {
         if (GPC_Intersect(Geom0,OGR_G_GetGeometryRef(Geom1,n1))) {
            return(1);
         }
      }
   }

   /*Verifier l'inclusion dans les trous pour les polygones*/
   if (OGR_G_GetGeometryType(Geom0)==wkbPolygon && OGR_G_GetGeometryCount(Geom0)>1) {
      pt=OGR_G_CreateGeometry(wkbPoint);
      for(n0=1;n0<OGR_G_GetGeometryCount(Geom0);n0++) {
         npt=0;
         for(n1=0;n1<OGR_G_GetPointCount(Geom1);n1++) {
            OGR_G_SetPoint(pt,0,OGR_G_GetX(Geom1,n1),OGR_G_GetY(Geom1,n1),0);
            npt+=GPC_PointPolyIntersect(pt,OGR_G_GetGeometryRef(Geom0,n0));
         }
         if (npt==OGR_G_GetPointCount(Geom1)) {
            return(0);
         }
      }
      OGR_G_DestroyGeometry(pt);
  }

   /*Recuperer la dimension des geometries*/
   t0=OGR_G_GetDimension(Geom0);
   t1=OGR_G_GetDimension(Geom1);

   /*Pour les polygones tester seulement le contour externe*/
   if (OGR_G_GetGeometryType(Geom0)==wkbPolygon) {
      Geom0=OGR_G_GetGeometryRef(Geom0,0);
   }
   if (OGR_G_GetGeometryType(Geom1)==wkbPolygon) {
      Geom1=OGR_G_GetGeometryRef(Geom1,0);
   }

   /*Forcer les lignes refermer comm des polygones*/
   if ((n1=OGR_G_GetPointCount(Geom1))>2) {
      OGR_G_GetPoint(Geom1,0,&v0[0],&v0[1],&v0[2]);
      OGR_G_GetPoint(Geom1,n1-1,&v1[0],&v1[1],&v1[2]);
      if (Vect_Equal(v0,v1)) t1=2;
   }

   /*Demarrer les tests selon les type de geometrie*/
   if (t0==0) {
      if (t1==0) {
         return(GPC_PointPointIntersect(Geom0,Geom1));
      } if (t1==1){
         return(GPC_PointLineIntersect(Geom0,Geom1));
      } else {
         return(GPC_PointPolyIntersect(Geom0,Geom1));
      }
   } else if (t0==1) {
      if (t1==0) {
         return(GPC_PointLineIntersect(Geom1,Geom0));
      } else if (t1==1) {
         return(GPC_LinePolyIntersect(Geom0,Geom1));
      } else {
         if (GPC_PointPolyIntersect(Geom0,Geom1)) {
            return(1);
         } else {
            return(GPC_LinePolyIntersect(Geom0,Geom1));
         }
      }
   } else {
      if (t1==0) {
         return(GPC_PointPolyIntersect(Geom1,Geom0));
      } else if (t1==1) {
         return(GPC_LinePolyIntersect(Geom1,Geom0));
      } else {
         if (GPC_PointPolyIntersect(Geom1,Geom0)) {
            return(1);
         } if (GPC_PointPolyIntersect(Geom0,Geom1)) {
            return(1);
         } else  {
            return(GPC_LinePolyIntersect(Geom1,Geom0));
         }
      }
   }
   return(0);
}

int GPC_PointPointIntersect(OGRGeometryH Geom0,OGRGeometryH Geom1) {

   register long n0,n1,n;
   Vect3d        v0,v1;

   n=OGR_G_GetPointCount(Geom1);

   for(n0=0;n0<OGR_G_GetPointCount(Geom0);n0++) {
      OGR_G_GetPoint(Geom0,n0,&v0[0],&v0[1],&v0[2]);

      for(n1=0;n1<n;n1++) {
         OGR_G_GetPoint(Geom1,0,&v1[0],&v1[1],&v1[2]);

         if (v0[0]==v1[0] && v0[1]==v1[1]) {
            return(1);
         }
      }
   }
   return(0);
}

int GPC_PointLineIntersect(OGRGeometryH Geom0,OGRGeometryH Geom1) {

   register long n0,n1,n;
   Vect3d        v0,v1[2];

   n=OGR_G_GetPointCount(Geom1);

   for(n0=0;n0<OGR_G_GetPointCount(Geom0);n0++) {
      OGR_G_GetPoint(Geom0,n0,&v0[0],&v0[1],&v0[2]);

      for(n1=0;n1<n-1;n1++) {
         OGR_G_GetPoint(Geom1,n1,&v1[0][0],&v1[0][1],&v1[0][2]);
         OGR_G_GetPoint(Geom1,n1+1,&v1[1][0],&v1[1][1],&v1[1][2]);

         if (GPC_SegmentIntersect(v0,v0,v1[0],v1[1],NULL)==1) {
            return(1);
         }
      }
   }
   return(0);
}

int GPC_PointPolyIntersect(OGRGeometryH Geom0,OGRGeometryH Geom1) {

   register long n0,n1,n,nn;
   int           c=0;
   Vect3d        v0,v1[2];

   n=OGR_G_GetPointCount(Geom1);

   for(n0=0;n0<OGR_G_GetPointCount(Geom0);n0++) {
      OGR_G_GetPoint(Geom0,n0,&v0[0],&v0[1],&v0[2]);

      c=0;

      for(n1=0,nn=n-1;n1<n;nn=n1++) {
         OGR_G_GetPoint(Geom1,n1,&v1[0][0],&v1[0][1],&v1[0][2]);
         OGR_G_GetPoint(Geom1,nn,&v1[1][0],&v1[1][1],&v1[1][2]);

         if ((((v1[0][1]<=v0[1]) && (v0[1]<v1[1][1])) || ((v1[1][1]<=v0[1]) && (v0[1]<v1[0][1]))) &&
            (v0[0]<(v1[1][0]-v1[0][0])*(v0[1]-v1[0][1])/(v1[1][1]-v1[0][1])+v1[0][0])) {
               c=!c;
        }
      }
      if (c) break;
   }
   return(c);
}

int GPC_LinePolyIntersect(OGRGeometryH Geom0,OGRGeometryH Geom1) {

   register long n0,n1,n;
   int           d;
   Vect3d        v0[2],v1[2];

   n=OGR_G_GetPointCount(Geom1);

   for(n0=0;n0<OGR_G_GetPointCount(Geom0)-1;n0++) {
      OGR_G_GetPoint(Geom0,n0,&v0[0][0],&v0[0][1],&v0[0][2]);
      OGR_G_GetPoint(Geom0,n0+1,&v0[1][0],&v0[1][1],&v0[1][2]);

      for(n1=0;n1<n-1;n1++) {
         OGR_G_GetPoint(Geom1,n1,&v1[0][0],&v1[0][1],&v1[0][2]);
         OGR_G_GetPoint(Geom1,n1+1,&v1[1][0],&v1[1][1],&v1[1][2]);

         d=GPC_SegmentIntersect(v0[0],v0[1],v1[0],v1[1],NULL);
         if (d==1) {
            return(1);
         }
      }
   }
   return(0);
}

double GPC_Length(OGRGeometryH Geom) {

   double length=0;
   int    g,n;

   /*Boucle recursive sur les sous geometrie*/
   for(g=0;g<OGR_G_GetGeometryCount(Geom);g++) {
      length+=GPC_Length(OGR_G_GetGeometryRef(Geom,g));
   }

   return(length+=GPC_SegmentLength(Geom));
}

double GPC_SegmentLength(OGRGeometryH Geom) {

   register int n;
   double       length=0;
   Vect3d       v0,v1;

   for(n=0;n<OGR_G_GetPointCount(Geom)-1;n++) {
      OGR_G_GetPoint(Geom,n,&v0[0],&v0[1],&v0[2]);
      OGR_G_GetPoint(Geom,n+1,&v1[0],&v1[1],&v1[2]);

      Vect_Substract(v0,v1,v0);
      length+=Vect_Norm(v0);
   }
   return(length);
}


/* 0----Intersection dosn't exists                                           */
/* 1----Intersection exists.                                                 */
/* 2----two line segments are parallel.                                      */
/* 3----two line segments are collinear, but not overlap.                    */
/* 4----two line segments are collinear, and share one same end point.       */
/* 5----two line segments are collinear, and overlap.                        */

int GPC_SegmentIntersect(Vect3d PointA,Vect3d PointB,Vect3d PointC,Vect3d PointD,Vect3d Inter) {

   double u,v,delta;
   double t1,t2;
   double a,b,c,d;
   double xba,yba,xdc,ydc,xca,yca;

   xba=PointB[0]-PointA[0];  yba=PointB[1]-PointA[1];
   xdc=PointD[0]-PointC[0];  ydc=PointD[1]-PointC[1];
   xca=PointC[0]-PointA[0];  yca=PointC[1]-PointA[1];

   delta=xba*ydc-yba*xdc;
   t1=xca*ydc-yca*xdc;
   t2=xca*yba-yca*xba;

   if (delta!=0.0) {

      u=t1/delta;
      v=t2/delta;

      /*two segments intersect (including intersect at end points)*/
      if (u<=1 && u>=0 && v<=1 && v>=0) {
         if (Inter) {
            Inter[0]=PointA[0]+u*(PointB[0]-PointA[0]);
            Inter[1]=PointA[1]+u*(PointB[1]-PointA[1]);
            Inter[2]=u;
         }
         return(1);
      } else {
         return(0);
      }
   } else {

      /* AB & CD are parallel. */
      if ((t1!=0) && (t2!=0))
         return(2);

      /* when AB & CD are collinear */
      /*if AB isn't a vertical line segment, project to x-axis */
      if (PointA[0]!=PointB[0]) {
         a=MIN(PointA[0],PointB[0]);
         b=MAX(PointA[0],PointB[0]);
         c=MIN(PointC[0],PointD[0]);
         d=MAX(PointC[0],PointD[0]);

         if (d<a || c>b) {
            return(3);
         } else if (d==a || c==b) {
            return(4);
         } else {
            return(5);
         }
      } else {
         /*if AB is a vertical line segment, project to y-axis*/
         a=MIN(PointA[1],PointB[1]);
         b=MAX(PointA[1],PointB[1]);
         c=MIN(PointC[1],PointD[1]);
         d=MAX(PointC[1],PointD[1]);

         if (d<a || c>b) {
            return(3);
         } else if (d==a || c==b) {
            return(4);
         } else {
            return(5);
         }
      }
   }
}
