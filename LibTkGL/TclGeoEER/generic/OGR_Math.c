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
 *=========================================================
 */

#include "tclOGR.h"

static Vect3d* GPC_Geom[2];
static unsigned int GPC_GeomNb=0;

static int GPC_ToVect3d(OGRGeometryH Geom,int No) {

   unsigned int i,n;

   n=OGR_G_GetPointCount(Geom);
   if (n>GPC_GeomNb) {
      GPC_GeomNb=n;
      GPC_Geom[0]=(Vect3d*)realloc(GPC_Geom[0],GPC_GeomNb*sizeof(Vect3d));
      GPC_Geom[1]=(Vect3d*)realloc(GPC_Geom[1],GPC_GeomNb*sizeof(Vect3d));

      if (!GPC_Geom[0] || !GPC_Geom[1]) {
         fprintf(stderr,"(ERROR) Could not allocate GPC_Geom buffer\n");
         return(0);
      }
   }
   for(i=0;i<n;i++) {
      OGR_G_GetPoint(Geom,i,&GPC_Geom[No][i][0],&GPC_Geom[No][i][1],&GPC_Geom[No][i][2]);
   }
   return(n);
}

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
   int          n,np,nr,nb,nbinter,in=0;

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
      in=GPC_PointPolyIntersect(point,ring,0);
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
            if (GPC_PointPolyIntersect(point,ring,0)) {
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
   gpc_vertex_list   *gpc;
   unsigned int       n,nb,g,nc;
   double             tmpd;

   type=wkbFlatten(OGR_G_GetGeometryType(Geom));

   if (type==wkbMultiPolygon || type==wkbGeometryCollection) {
      for (g=0;g<OGR_G_GetGeometryCount(Geom);g++) {
         geom=OGR_G_GetGeometryRef(Geom,g);
         GPC_FromOGR(Poly,geom);
      }
   } else if (type==wkbPolygon) {
      if ((nc=OGR_G_GetGeometryCount(Geom))) {
         Poly->hole=(int*)realloc(Poly->hole,(Poly->num_contours+nc)*(sizeof(int)));
         Poly->contour=(gpc_vertex_list*)realloc(Poly->contour,(Poly->num_contours+nc)*(sizeof(gpc_vertex_list)));

         for (g=0;g<nc;g++) {
            geom=OGR_G_GetGeometryRef(Geom,g);
            nb=OGR_G_GetPointCount(geom);
            Poly->hole[Poly->num_contours+g]=(g==0?FALSE:TRUE);
            gpc=&Poly->contour[Poly->num_contours+g];
            gpc->num_vertices=nb;
            gpc->vertex=(nb?(gpc_vertex*)malloc(nb*sizeof(gpc_vertex)):NULL);

            for (n=0;n<nb;n++) {
               OGR_G_GetPoint(geom,n,&gpc->vertex[n].x,&gpc->vertex[n].y,&tmpd);
            }
         }
         Poly->num_contours+=nc;
      }
   } else {
      fprintf(stderr,"(ERROR) GPC_FromOGR: Unsupported geometry type\n");
   }
}

void GPC_ToOGR(gpc_polygon *Poly,OGRGeometryH *Geom) {

   OGRGeometryH     geom,ring,poly=NULL,multi=NULL;
   gpc_vertex_list *gpc;
   unsigned int     n,g,nb=0,in;

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
         gpc=&Poly->contour[g];
         ring=OGR_G_CreateGeometry(wkbLinearRing);
         for (n=0;n<gpc->num_vertices;n++) {
            OGR_G_AddPoint_2D(ring,gpc->vertex[n].x,gpc->vertex[n].y);
         }
         /*Make sure polygon is closed*/
         if (gpc->vertex[n-1].x!=gpc->vertex[0].x || gpc->vertex[n-1].y!=gpc->vertex[0].y) {
            OGR_G_AddPoint_2D(ring,gpc->vertex[0].x,gpc->vertex[0].y);
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
      /*Process internal rings (holes)*/
      for(g=0;g<Poly->num_contours;g++) {
         if (Poly->hole[g]) {
            gpc=&Poly->contour[g];
            ring=OGR_G_CreateGeometry(wkbLinearRing);
            for (n=0;n<gpc->num_vertices;n++) {
               OGR_G_AddPoint_2D(ring,gpc->vertex[n].x,gpc->vertex[n].y);
            }
            /*Make sure polygon is closed*/
            if (gpc->vertex[n-1].x!=gpc->vertex[0].x || gpc->vertex[n-1].y!=gpc->vertex[0].y) {
               OGR_G_AddPoint_2D(ring,gpc->vertex[0].x,gpc->vertex[0].y);
            }
            if (multi) {
               in=0;

               /*Look for hole's parent*/
               for (n=0;n<OGR_G_GetGeometryCount(multi);n++) {
                  geom=OGR_G_GetGeometryRef(multi,n);
                  if (GPC_Intersect(geom,ring,NULL,NULL)) {
                     OGR_G_AddGeometryDirectly(geom,ring);
                     in=1;
                     break;
                  }
               }
               if (!in) {
                  fprintf(stderr,"(ERROR) GPC_ToOGR: Found a hole without parent\n");
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

OGRGeometryH GPC_OnOGRLayer(gpc_op Op,OGR_Layer *Layer) {

   gpc_polygon  poly0,poly1,result,*r,*p,*t;
   OGRGeometryH geom=NULL;
   unsigned int f;
   
   GPC_New(&poly0);
   GPC_New(&poly1);
   GPC_New(&result);
   
   p=&poly0;
   r=&result;
   t=NULL;
  
   for(f=0;f<Layer->NFeature;f++) {
      if (Layer->Select[f]) {
         if ((geom=OGR_F_GetGeometryRef(Layer->Feature[f]))) {

            GPC_FromOGR((t?&poly1:&result),geom);
            if (t) {
               gpc_polygon_clip(Op,p,&poly1,r);
               gpc_free_polygon(p);
            }
            t=p; p=r; r=t;
            
            gpc_free_polygon(&poly1);
         }
      }
   }
   
   GPC_ToOGR(p,&geom);
 
   gpc_free_polygon(&result);
   gpc_free_polygon(&poly0);
   gpc_free_polygon(&poly1);

   return(geom);
}

int GPC_Within(OGRGeometryH Geom0,OGRGeometryH Geom1,OGREnvelope *Env0,OGREnvelope *Env1) {

   int          n0,n1,npt=0;
   OGRGeometryH pt,geom;
   OGREnvelope  env;

   /*Check if enveloppe overlap*/
   if (Env0 && Env1) {
      if (!OGR_G_EnvelopeIntersect((*Env0),(*Env1))) {
         return(0);
      }
   }

   /*It can't be within if it's not a within polygon*/
   if (OGR_G_GetGeometryType(Geom1)!=wkbPolygon && OGR_G_GetGeometryType(Geom1)!=wkbMultiPolygon && OGR_G_GetGeometryType(Geom1)!=wkbLinearRing) {
      return(0);
   }

   /*Boucle recursive sur les sous geometrie*/
   if (OGR_G_GetGeometryType(Geom0)!=wkbPolygon && (n0=OGR_G_GetGeometryCount(Geom0))) {
      for(n0=0;n0<OGR_G_GetGeometryCount(Geom0);n0++) {
         geom=OGR_G_GetGeometryRef(Geom0,n0);
         OGR_G_GetEnvelope(geom,&env);
         if (!GPC_Within(geom,Geom1,&env,Env1)) {
            return(0);
         } else {
            npt++;
         }
      }
      return(npt==OGR_G_GetGeometryCount(Geom0));
   }

   if (OGR_G_GetGeometryType(Geom1)!=wkbPolygon && (n1=OGR_G_GetGeometryCount(Geom1))) {
     for(n1=0;n1<OGR_G_GetGeometryCount(Geom1);n1++) {
         geom=OGR_G_GetGeometryRef(Geom1,n1);
         OGR_G_GetEnvelope(geom,&env);
         if (GPC_Within(Geom0,geom,Env0,&env)) {
            return(1);
         }
      }
   }

   /*Verifier l'inclusion dans les trous pour les polygones*/
   if (OGR_G_GetGeometryType(Geom1)==wkbPolygon && OGR_G_GetGeometryCount(Geom1)>1) {
      pt=OGR_G_CreateGeometry(wkbPoint);
      for(n0=1;n0<OGR_G_GetGeometryCount(Geom1);n0++) {
         npt=0;
         for(n1=0;n1<OGR_G_GetPointCount(Geom0);n1++) {
            OGR_G_SetPoint(pt,0,OGR_G_GetX(Geom0,n1),OGR_G_GetY(Geom0,n1),0);
            if (GPC_PointPolyIntersect(pt,OGR_G_GetGeometryRef(Geom1,n0),0)) {
               OGR_G_DestroyGeometry(pt);
               return(0);
            }
         }
      }
      OGR_G_DestroyGeometry(pt);
   }

   /*Pour les polygones tester seulement le contour externe*/
   if (OGR_G_GetGeometryType(Geom0)==wkbPolygon) {
      Geom0=OGR_G_GetGeometryRef(Geom0,0);
   }
   if (OGR_G_GetGeometryType(Geom1)==wkbPolygon) {
      Geom1=OGR_G_GetGeometryRef(Geom1,0);
   }
   
//   return(GPC_PointPolyIntersect(Geom0,Geom1,1));

   /*Demarrer les tests selon les type de geometrie*/
   if (GPC_LinePolyIntersect(Geom0,Geom1)) {
      return(0);
   } else {
      return(GPC_PointPolyIntersect(Geom0,Geom1,0));
   }
}

int GPC_Intersect(OGRGeometryH Geom0,OGRGeometryH Geom1,OGREnvelope *Env0,OGREnvelope *Env1) {

   int          n0,n1,t0,t1,npt;
   Vect3d       v0,v1;
   OGRGeometryH pt,geom;
   OGREnvelope  env;

   /*Check if enveloppe overlap*/
   if (Env0 && Env1) {
      if (!OGR_G_EnvelopeIntersect((*Env0),(*Env1))) {
         return(0);
      }
   }

   /*Boucle recursive sur les sous geometrie*/
   if (OGR_G_GetGeometryType(Geom0)!=wkbPolygon && (n0=OGR_G_GetGeometryCount(Geom0))) {
      for(n0=0;n0<OGR_G_GetGeometryCount(Geom0);n0++) {
         geom=OGR_G_GetGeometryRef(Geom0,n0);
         OGR_G_GetEnvelope(geom,&env);
         if (GPC_Intersect(geom,Geom1,&env,Env1)) {
            return(1);
         }
      }
   }

   if (OGR_G_GetGeometryType(Geom1)!=wkbPolygon && (n1=OGR_G_GetGeometryCount(Geom1))) {
      for(n1=0;n1<OGR_G_GetGeometryCount(Geom1);n1++) {
         geom=OGR_G_GetGeometryRef(Geom1,n1);
         OGR_G_GetEnvelope(geom,&env);
         if (GPC_Intersect(Geom0,geom,Env0,&env)) {
            return(1);
         }
      }
   }

   /*Verifier l'inclusion dans les trous pour les polygones*/
   if (OGR_G_GetGeometryType(Geom0)==wkbPolygon && OGR_G_GetGeometryCount(Geom0)>1) {
      pt=OGR_G_CreateGeometry(wkbPoint);
      for(n0=1;n0<OGR_G_GetGeometryCount(Geom0);n0++) {
         npt=0;

         /*Verifier seulement pour le contour exterieur*/
         if (OGR_G_GetGeometryCount(Geom1)) {
            geom=OGR_G_GetGeometryRef(Geom1,0);
         } else {
            geom=Geom1;
         }
         for(n1=0;n1<OGR_G_GetPointCount(geom);n1++) {
            OGR_G_SetPoint(pt,0,OGR_G_GetX(geom,n1),OGR_G_GetY(geom,n1),0);
            npt+=GPC_PointPolyIntersect(pt,OGR_G_GetGeometryRef(Geom0,n0),0);
         }
         if (npt==OGR_G_GetPointCount(geom)) {
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

   /*Forcer les lignes referme comme des polygones*/
   if ((n0=OGR_G_GetPointCount(Geom0))>2) {
      OGR_G_GetPoint(Geom0,0,&v0[0],&v0[1],&v0[2]);
      OGR_G_GetPoint(Geom0,n0-1,&v1[0],&v1[1],&v1[2]);
      if (Vect_Equal(v0,v1)) t0=2;
   }
   if ((n1=OGR_G_GetPointCount(Geom1))>2) {
      OGR_G_GetPoint(Geom1,0,&v0[0],&v0[1],&v0[2]);
      OGR_G_GetPoint(Geom1,n1-1,&v1[0],&v1[1],&v1[2]);
      if (Vect_Equal(v0,v1)) t1=2;
   }
   /*Demarrer les tests selon les type de geometrie*/
   if (n0 && n1) {
      if (t0==0) {
         if (t1==0) {
            return(GPC_PointPointIntersect(Geom0,Geom1,0));
         } else if (t1==1){
            return(GPC_PointLineIntersect(Geom0,Geom1,0));
         } else {
            return(GPC_PointPolyIntersect(Geom0,Geom1,0));
         }
      } else if (t0==1) {
         if (t1==0) {
            return(GPC_PointLineIntersect(Geom1,Geom0,0));
         } else if (t1==1) {
            return(GPC_LinePolyIntersect(Geom0,Geom1));
         } else {
            return(GPC_PolyPolyIntersect(Geom0,Geom1));
         }
      } else {
         if (t1==0) {
            return(GPC_PointPolyIntersect(Geom1,Geom0,0));
         } else if (t1==1) {
            return(GPC_LinePolyIntersect(Geom1,Geom0));
         } else {
            if (GPC_PolyPolyIntersect(Geom0,Geom1)) {
               return(1);
            }
            return(GPC_PointPolyIntersect(Geom1,Geom0,0));
         }
      }
   }
   return(0);
}

int GPC_PointPointIntersect(OGRGeometryH Geom0,OGRGeometryH Geom1,int All) {

   unsigned int n0,n1,g0,g1,t=0;

   g0=GPC_ToVect3d(Geom0,0);
   g1=GPC_ToVect3d(Geom1,1);

   for(n0=0;n0<g0;n0++) {
      for(n1=0;n1<g1;n1++) {
         if (Vect_Equal(GPC_Geom[0][n0],GPC_Geom[1][n1])) {
            t++;
            if (!All)
               return(1);
         }
      }
   }
   return(All?t==g0:t);
}

int GPC_PointLineIntersect(OGRGeometryH Geom0,OGRGeometryH Geom1,int All) {

   unsigned int n0,n1,g0,g1,t=0,i;
   Vect3d       v0,v1[2];

   g0=GPC_ToVect3d(Geom0,0);
   g1=GPC_ToVect3d(Geom1,1);

   for(n0=0;n0<g0;n0++) {
      Vect_Assign(v0,GPC_Geom[0][n0]);

      for(n1=0;n1<g1-1;n1++) {
         Vect_Assign(v1[0],GPC_Geom[1][n1]);
         Vect_Assign(v1[1],GPC_Geom[1][n1+1]);

         i=GPC_SegmentIntersect(v0,v0,v1[0],v1[1],NULL);
         if (i==1 || i==4 || i==5) {
            t++;
            if (!All)
               return(1);
         }
      }
   }
   return(All?t==g0:t);
}

int GPC_PointPolyIntersect(OGRGeometryH Geom0,OGRGeometryH Geom1,int All) {

   unsigned int n0,n1,g0,g1,n11,t=0;
   int          c=0;
   Vect3d       v0,v1[2];

   g0=GPC_ToVect3d(Geom0,0);
   g1=GPC_ToVect3d(Geom1,1);

   if (!g0 || !g1)
      return(0);

   for(n0=0;n0<g0;n0++) {
      Vect_Assign(v0,GPC_Geom[0][n0]);

      c=0;

      for(n1=0,n11=g1-1;n1<g1;n11=n1++) {
         Vect_Assign(v1[0],GPC_Geom[1][n1]);
         Vect_Assign(v1[1],GPC_Geom[1][n11]);

         /*Check for point insidness*/
         if (OGR_PointInside(v0,v1[0],v1[1])) {
            c=!c;
         }
      }
      if (c) {
         t++;
         if (!All) {
            break;
         }
      }
   }
   return(All?t==g0:t);
}

int GPC_PolyPolyIntersect(OGRGeometryH Geom0,OGRGeometryH Geom1) {

   unsigned int n0,n1,g0,g1,n11;
   int          c,d;
   Vect3d       v0[2],v1[2];

   g0=GPC_ToVect3d(Geom0,0);
   g1=GPC_ToVect3d(Geom1,1);

   if (!g0 || !g1)
      return(0);

   for(n0=0;n0<(g0-1);n0++) {

      Vect_Assign(v0[0],GPC_Geom[0][n0]);
      Vect_Assign(v0[1],GPC_Geom[0][n0+1]);
      c=0;
      d=0;

      for(n1=0,n11=g1-2;n1<(g1-1);n11=n1++) {

         Vect_Assign(v1[0],GPC_Geom[1][n1]);
         Vect_Assign(v1[1],GPC_Geom[1][n11]);

         /*Check for segment intersection*/
         if ((GPC_SegmentIntersect(v0[0],v0[1],v1[0],v1[1],NULL)==1)) {
            return(1);
         }

         /*Check for point insidness*/
         if (OGR_PointInside(v0[0],v1[0],v1[1])) {
            c=!c;
         }
      }
      if (c) {
         return(1);
      }
   }
   return(0);
}

int GPC_LinePolyIntersect(OGRGeometryH Geom0,OGRGeometryH Geom1) {

   unsigned int n0,n1,g0,g1;
   Vect3d       v0[2],v1[2];

   g0=GPC_ToVect3d(Geom0,0);
   g1=GPC_ToVect3d(Geom1,1);

   if (!g0 || !g1)
      return(0);
   
   for(n0=0;n0<g0-1;n0++) {
      Vect_Assign(v0[0],GPC_Geom[0][n0]);
      Vect_Assign(v0[1],GPC_Geom[0][n0+1]);

      for(n1=0;n1<g1-1;n1++) {
         Vect_Assign(v1[0],GPC_Geom[1][n1]);
         Vect_Assign(v1[1],GPC_Geom[1][n1+1]);

         /*Check for segment intersection*/
         if ((GPC_SegmentIntersect(v0[0],v0[1],v1[0],v1[1],NULL)==1)) {
            return(1);
         }
      }
   }
   return(0);
}

double GPC_CoordLimit(OGRGeometryH Geom,int Coord,int Mode) {

   register unsigned int n=0;
   int                   g=0;
   Vect3d                v;
   double                val=0,valg;

   if (Coord>=0 && Coord<=2) {

      if (Mode==0) {         /*Minimum*/
         val=1e32;
      } else if (Mode==1) {  /*Maximum*/
         val=-1e32;
      } else {               /*Average*/
         val=0.0;
      }

      /*Boucle recursive sur les sous geometrie*/
      for(g=0;g<OGR_G_GetGeometryCount(Geom);g++) {
         valg=GPC_CoordLimit(OGR_G_GetGeometryRef(Geom,g),Coord,Mode);
         if (Mode==0) {
            val=valg<val?valg:val;
         } else if (Mode==1) {
            val=valg>val?valg:val;
         } else {
            val+=valg;
         }
      }

      for(n=0;n<OGR_G_GetPointCount(Geom);n++) {
         OGR_G_GetPoint(Geom,n,&v[0],&v[1],&v[2]);
         if (Mode==0) {
            val=v[Coord]<val?v[Coord]:val;
         } else if (Mode==1) {
            val=v[Coord]>val?v[Coord]:val;
         } else {
            val+=v[Coord];
         }
      }
   }
   return(Mode==2?val/(n+g):val);
}

double GPC_Length(OGRGeometryH Geom) {

   double length=0;
   int    g;

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

/* 0----Intersection doesn't exists                                          */
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

double GPC_Centroid2DProcess(OGRGeometryH Geom,double *X,double *Y) {

   Vect3d pt[2];
   int    i,j,n;
   double area=0,mid;

   /* Process current geometry */
   n=OGR_G_GetPointCount(Geom);

   if (n==1) {
      /* Proccess point */
      OGR_G_GetPoint(Geom,0,X,Y,&pt[0][2]);
      return(0.0);
   } else if (n==2) {
      /* Process line */
      OGR_G_GetPoint(Geom,0,X,Y,&pt[0][2]);
      OGR_G_GetPoint(Geom,1,&pt[1][0],&pt[1][1],&pt[1][2]);
      *X+=(pt[1][0]-*X)/2.0;
      *Y+=(pt[1][1]-*Y)/2.0;
      return(0.0);
   }

   /* Process polygon/polyline */
   for(i=0;i<n;i++) {
      OGR_G_GetPoint(Geom,i,&pt[0][0],&pt[0][1],&pt[0][2]);
      OGR_G_GetPoint(Geom,(i+1)%n,&pt[1][0],&pt[1][1],&pt[1][2]);

      mid=pt[0][0]*pt[1][1]-pt[0][1]*pt[1][0];
      area+=mid;
      *X+=(pt[0][0]+pt[1][0])*mid;
      *Y+=(pt[0][1]+pt[1][1])*mid;
   }
   area*=0.5;

   /* Process sub geometry */
   for(j=0;j<OGR_G_GetGeometryCount(Geom);j++) {
      area+=GPC_Centroid2DProcess(OGR_G_GetGeometryRef(Geom,j),X,Y);
   }

   return(area);
}

double GPC_Centroid2D(OGRGeometryH Geom,double *X,double *Y) {

   OGREnvelope env;
   OGRSpatialReferenceH srs;
   double area,d;

   *X=0.0;
   *Y=0.0;

   area=GPC_Centroid2DProcess(Geom,X,Y);

   if (area!=0.0) {
      d=1.0/(6.0*area);
      *X*=d;
      *Y*=d;
   }

   if (!(srs=OGR_G_GetSpatialReference(Geom)) || OSRIsGeographic(srs)) {
      OGR_G_GetEnvelope(Geom,&env);
      if (-180.0<=env.MinX && 180.0>=env.MaxX && (env.MaxX-env.MinX)>180.0) {
         *X-=180.0;
      }
   }
   return(area<0?-area:area);
}

// Copyright 2002, softSurfer (www.softsurfer.com)
// This code may be freely used and modified for any purpose
// providing that this copyright notice is included with it.
// SoftSurfer makes no warranty for this code, and cannot be held
// liable for any real or imagined damage resulting from its use.
// Users of this code must verify correctness for their application.
//
//  GPC_SimplifyDP():
//  This is the Douglas-Peucker recursive simplification routine
//  It just marks vertices that are part of the simplified polyline
//  for approximating the polyline subchain v[j] to v[k].
//    Input:  tol = approximation tolerance
//            v[] = polyline array of vertex points
//            j,k = indices for the subchain v[j] to v[k]
//    Output: mk[] = array of markers matching vertex array v[]
int GPC_SimplifyDP(double Tolerance,Vect3d *Pt,int J,int K,int *Markers) {

   /*There is nothing to simplify*/
   if (K<=J+1)
      return(0);

   int      i,n=0;
   int      maxi=J;                    // index of vertex farthest from S
   double   maxd2=0.0;                 // distance squared of farthest vertex
   double   tol2=Tolerance*Tolerance;  // tolerance squared
   double  cu;                         // segment length squared
   Vect3d  w,u,p;
   double  b,cw,dv2;                   // dv2 = distance v[i] to S squared

   // test each vertex v[i] for max distance from S
   // compute using the Feb 2001 Algorithm's dist_Point_to_Segment()
   // Note: this works in any dimension (2D, 3D, ...)

   Vect_Substract(u,Pt[K],Pt[J]);
   cu=Vect_DotProduct(u,u);

   for (i=J+1;i<K;i++) {
      // compute distance squared
      Vect_Substract(w,Pt[i],Pt[J]);
      cw=Vect_DotProduct(w,u);
      if (cw<=0) {
         dv2=Vect_Dist2(Pt[i],Pt[J]);
      } else if (cu<=cw) {
         dv2=Vect_Dist2(Pt[i],Pt[K]);
      } else {
         b=cw/cu;
         Vect_SMul(u,u,b);
         Vect_Add(p,Pt[J],u);
         dv2=Vect_Dist2(Pt[i],p);
      }
      // test with current max distance squared
      if (dv2<=maxd2)
         continue;

      // v[i] is a new max vertex
      maxi=i;
      maxd2=dv2;
   }

   /*Error is worse than the tolerance*/
   if (maxd2>tol2) {
      /*split the polyline at the farthest vertex from S*/
      Markers[maxi]=1;      // mark v[maxi] for the simplified polyline
      n++;
      /*Recursively simplify the two subpolylines at v[maxi]*/
      n+=GPC_SimplifyDP(Tolerance,Pt,J,maxi,Markers);  // polyline v[j] to v[maxi]
      n+=GPC_SimplifyDP(Tolerance,Pt,maxi,K,Markers);  // polyline v[maxi] to v[k]
   }
   /*Else the approximation is OK, so ignore intermediate vertices*/
   return(n);
}

// GPC_Simplify():
//    Input:  tol = approximation tolerance
//            V[] = polyline array of vertex points
//            n   = the number of points in V[]
//    Output: sV[]= simplified polyline vertices (max is n)
//    Return: m   = the number of points in sV[]
int GPC_Simplify(double Tolerance,OGRGeometryH Geom) {

   int    i,k,pv,n=-1,m=0;         // Misc counters
   double tol2=Tolerance*Tolerance;  // Tolerance squared
   Vect3d *pt,pt0,pt1;               // Vertex buffer
   int    *mk;                       // Marker buffer

   /*Simplify sub-geometry*/
   for(i=0;i<OGR_G_GetGeometryCount(Geom);i++) {
      n=GPC_Simplify(Tolerance,OGR_G_GetGeometryRef(Geom,i));
   }

   if ((n=OGR_G_GetPointCount(Geom))) {
      mk=(int*)calloc(n,sizeof(int));
      pt=(Vect3d*)calloc(n,sizeof(Vect3d));
      if (!mk || !pt) {
         fprintf(stderr,"(ERROR) GPC_Simplify: Unable to allocate buffers\n");
         return(0);
      }

      /*STAGE 1: Vertex Reduction within tolerance of prior vertex cluster*/
      for(i=k=1,pv=0;i<n;i++) {
         OGR_G_GetPoint(Geom,i,&pt0[0],&pt0[1],&pt0[2]);
         OGR_G_GetPoint(Geom,pv,&pt1[0],&pt1[1],&pt1[2]);

         if (Vect_Dist2(pt0,pt1)<tol2)
            continue;
         Vect_Assign(pt[k],pt0);
         k++;
         pv=i;
      }

      /*Start at beginning and finish at the end*/
      OGR_G_GetPoint(Geom,0,&pt[0][0],&pt[0][1],&pt[0][2]);
      if (pv<n-1) {
         OGR_G_GetPoint(Geom,n-1,&pt[k][0],&pt[k][1],&pt[k][2]);
         k++;
      }

      /*STAGE 2: Douglas-Peucker polyline simplification*/
      mk[0]=mk[k-1]=1;       // mark the first and last vertices
      m=GPC_SimplifyDP(Tolerance,pt,0,k-1,mk);

      // copy marked vertices to the output simplified polyline
      OGR_G_Empty(Geom);
      if (m>=2) {
         for (i=m=0;i<k;i++) {
            if (mk[i]) {
               OGR_G_AddPoint_2D(Geom,pt[i][0],pt[i][1]);
               m++;
            }
         }
      } else {
            OGR_G_AddPoint_2D(Geom,pt[0][0],pt[0][1]);
            OGR_G_AddPoint_2D(Geom,pt[0][0],pt[0][1]);
            OGR_G_AddPoint_2D(Geom,pt[0][0],pt[0][1]);
            OGR_G_AddPoint_2D(Geom,pt[0][0],pt[0][1]);
      }

      free(pt);
      free(mk);
   }
   return(m); // m vertices in simplified polyline
}
