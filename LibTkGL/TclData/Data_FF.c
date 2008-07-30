/*=============================================================================
 * Environnement Canada
 * Centre Meteorologique Canadian
 * 2100 Trans-Canadienne
 * Dorval, Quebec
 *
 * Projet    : Librairie Tcl de rendue donnees.
 * Fichier   : Data_FF.c
 * Creation  : Janvier 2005 - J.P. Gauthier - CMC/CMOE
 *
 * Description: Algorithmes specifiques appliquees a des donnees.
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

#include "Data_FF.h"

double *LUInvert(double *Matrix,int N) {

   int *perm=(int*)malloc(N*sizeof(int));
   int i,j,d;

   double *matrix=(double*)malloc(N*N*sizeof(double));
   double *v=(double*)malloc(N*sizeof(double));

   LUDecompose(Matrix,N,perm,&d);

   for(j=0;j<N;j++){
      for(i=0;i<N;i++) v[i]=0.0;
      v[j]=1.0;
      LUBackSub(Matrix,N,perm,v);
      for(i=0;i<N;i++) matrix[i*N+j]=v[i];
   }

   free(v);
   free(perm);
   free(Matrix);
   return(matrix);
}


int LUDecompose(double *Matrix,int N,int *Perm,int *d) {

   int imax,i,j,k,idxi,idxj;
   double sum,dum,amax;
   double *vv=(double*)malloc(N*sizeof(double));

   *d=1;

   for (i=0;i<N;i++) {
      amax=0.0;
      for(j=0;j<N;j++) {
        if ((dum=fabs(Matrix[i*N+j]))>amax)
            amax=dum;
      }
      if (amax<TINY_VALUE) {
         fprintf(stderr,"(WARNING) LUDecompose: Singular matrix");
         return(0);
      }
      vv[i]=1.0/amax;
   }


   for (j=0;j<N;j++) {
      idxj=j*N;
      for (i=0;i<j;i++) {
         idxi=i*N;
         sum=Matrix[idxi+j];
         for (k=0;k<i;k++)
            sum-=Matrix[idxi+k]*Matrix[k*N+j];
         Matrix[idxi+j]=sum;
      }
      amax=0.0;

      for(i=j;i<N;i++) {
         idxi=i*N;
         sum=Matrix[idxi+j];
         for(k=0;k<j;k++)
            sum-=Matrix[idxi+k]*Matrix[k*N+j];

         Matrix[idxi+j]=sum;
         dum=vv[i]*fabs(sum);

         if(dum>=amax) {
            imax=i;
            amax=dum;
         }
      }

      if (j!=imax) {
         for (k=0;k<N;k++) {
            dum=Matrix[imax*N+k];
            Matrix[imax*N+k]=Matrix[idxj+k];
            Matrix[idxj+k]=dum;
         }
         *d=-*d;
         vv[imax]=vv[j];
      }

      Perm[j]=imax;

      if (fabs(Matrix[idxj+j])<TINY_VALUE)
         Matrix[idxj+j]=TINY_VALUE;

      if (j!=N-1) {
         dum=1.0/Matrix[idxj+j];
         for(i=j+1;i<N;i++)
           Matrix[i*N+j]*=dum;
      }
   }
   free(vv);
   return(1);
}

void LUBackSub(double *Matrix,int N,int *Perm,double *Vect) {

   double sum;
   int    i,j,ii=-1,idxi;

   for (i=0;i<N;i++) {
      idxi=i*N;
      sum=Vect[Perm[i]];
      Vect[Perm[i]]=Vect[i];
      if (ii!=-1) {
         for (j=ii;j<=i-1;j++)
            sum-=Matrix[idxi+j]*Vect[j];
      } else if (sum!=0.0) {
         ii=i;
      }
      Vect[i]=sum;
   }

   for (i=N-1;i>=0;i--) {
      idxi=i*N;
      sum=Vect[i];
      for (j=i+1;j<N;j++)
         sum-=Matrix[idxi+j]*Vect[j];
      Vect[i]=sum/Matrix[idxi+i];
   }
}

/*----------------------------------------------------------------------------
 * Nom      : <FFKriggingValue>
 * Creation : Janvier 2005 - J.P. Gauthier - CMC/CMOE
 *
 * But      : Recuperer la valeur d'un point specifique selon les variogrammes
 *            du krigging
 *
 * Parametres :
 *  <Krig>    : Structure de kirgging
 *  <Pos>     : Liste des positions connues
 *  <X>       : Coordonnees I du point a determiner
 *  <Y>       : Coordonnees J du point a determiner
 *
 * Retour     :
 *  <valeur>  : Valeur du point
 *  <Error>   : Erreur de variance
 *
 * Remarques :
 *
 *----------------------------------------------------------------------------
 */
double FFKriggingValue(TKrigging *Krig,Vect3d *Pos,double X,double Y,double *Error) {

   int     i,j,idxi;
   double  z=0.0,d,t;

   /* Calculate distance between estimated point */
   for (i=0;i<Krig->N-1;i++) {
      d=hypot(Pos[i][0]-X,Pos[i][1]-Y);
      t=d/Krig->A;

      switch(Krig->Mode) {
         case SPHERE:
            if (d<Krig->A) {
               Krig->V[i]=Krig->C0+Krig->C1*(1.5*t-0.5*t*t*t);
            } else {
               Krig->V[i]=Krig->C0+Krig->C1;
            }
            break;
         case EXP:
            Krig->V[i]=Krig->C0+Krig->C1*(1-exp(-3*t));
            break;
         case GAUSS:
            Krig->V[i]=Krig->C0+Krig->C1*(1-exp(-3*t*t));
            break;
         case LINEAR:
            Krig->V[i]=Krig->C0+Krig->C1*t;
            break;
       }
   }
   Krig->V[i]=1;

   /* Calculate the weights */
   for(i=0;i<Krig->N;i++) {
      idxi=i*Krig->N;
      Krig->Weight[i]=0;
      for(j=0;j<Krig->N;j++) {
         Krig->Weight[i]+=Krig->Matrix[idxi+j]*Krig->V[j];
      }
   }

   /* Calculated the estimated value */
   for(i=0;i<Krig->N-1;i++) {
      z+=Krig->Weight[i]*Pos[i][2];
   }
//   z=z<0?0:z;

   /* Calculated the error variance */
   if (Error) {
      *Error=0.0;
      for(i=0;i<Krig->N-1;i++) {
         *Error+=Krig->Weight[i]*Krig->V[i];
      }
      *Error+=Krig->Weight[i];
      *Error=sqrt(*Error);
   }
   return(z);
}

/*----------------------------------------------------------------------------
 * Nom      : <FFKrigging>
 * Creation : Janvier 2005 - J.P. Gauthier - CMC/CMOE
 *
 * But      : Interpoler les valeur discretes sur une grille en utilisant
 *            l'algorithme de krigging
 *
 * Parametres :
 *  <Ref>     : GeoReference
 *  <Def>     : Definitions des donnees
 *  <Pos>     : Positions connues (observations)
 *  <NPos>    : Nombre de positions connues (observations)
 *  <C0>      : Minimum de la fonction (Nugget)
 *  <C1>      : Maximum de la fonction (Sill-C0)
 *  <A>       : Amplitude (Distance d'influance)
 *  <Mode>    : Mode de determination des variogrammes (SPHERE,EXP,GAUSS,LINEAR)
 *
 * Retour     :
 *
 * Remarques :
 *
 *----------------------------------------------------------------------------
 */
int FFKrigging(TGeoRef *Ref,TDataDef *Def,Vect3d *Pos,int NPos,double C0,double C1,double A,int Mode) {

   TKrigging krig;

   int i,j,k,idx0,idx1;
   double t,da;

   if (NPos) {
      krig.N=NPos+1;
      krig.C0=C0;
      krig.C1=C1;
      krig.A=(A<=0?1:A);
      krig.Mode=Mode;

      krig.Matrix=(double*)malloc(krig.N*krig.N*sizeof(double));
      krig.Weight=(double*)malloc(krig.N*krig.N*sizeof(double));
      krig.V=(double*)malloc(krig.N*sizeof(double));

      /* Calculate distance between known points*/
      for (j=0;j<NPos;j++) {
         for (i=0;i<NPos;i++) {
            idx0=i*krig.N+j;
            krig.Weight[idx0]=hypot(Pos[i][0]-Pos[j][0],Pos[i][1]-Pos[j][1]);

            if (krig.Weight[idx0]==0 && i!=j) {
               fprintf(stderr,"(WARNING) FFKrigging: Two observations have the same location, krigging operation will not continue\n");
               return(0);
            }
         }
      }

      /* Calculate variograms */
      for (i=0;i<krig.N-1;i++) {
         krig.Matrix[i*krig.N+krig.N-1]=1;
         krig.Matrix[(krig.N-1)*krig.N+i]=1;
      }
      krig.Matrix[(krig.N-1)*krig.N+i]=0;

      da=1.0/krig.A;
      for (j=0;j<krig.N-1;j++) {
         for (i=j;i<krig.N-1;i++) {
            idx0=i*krig.N+j;
            idx1=j*krig.N+i;

            t=krig.Weight[idx0]*da;

            switch(krig.Mode) {
               case SPHERE:
                  if (krig.Weight[idx0]<krig.A) {
                     krig.Matrix[idx0]=krig.Matrix[idx1]=krig.C0+krig.C1*(1.5*t-0.5*t*t*t);
                  } else {
                     krig.Matrix[idx0]=krig.Matrix[idx1]=krig.C0+krig.C1;
                  }
                  break;
               case EXP:
                  krig.Matrix[idx0]=krig.Matrix[idx1]=krig.C0+krig.C1*(1-exp(-3*t));
                  break;
               case GAUSS:
                  krig.Matrix[idx0]=krig.Matrix[idx1]=krig.C0+krig.C1*(1-exp(-3*t*t));
                  break;
               case LINEAR:
                  krig.Matrix[idx0]=krig.Matrix[idx1]=krig.C0+krig.C1*t;
                  break;
             }
         }
      }

      /* Invert the matrix */
      krig.Matrix=LUInvert(krig.Matrix,krig.N);

      /* Fill in the field */
      for(i=0;i<Def->NI;i++) {
         for(j=0;j<Def->NJ;j++) {
            t=FFKriggingValue(&krig,Pos,i,j,NULL);
            Def_Set(Def,0,j*Def->NI+i,t);
         }
      }

      free(krig.Matrix);
      free(krig.Weight);
      free(krig.V);
   }
   return(1);
}

unsigned char FFQuad_Cross(double Depth,unsigned char Side,double *Quad,double Inter,int *QX,int *QY,double *X,double *Y) {

   unsigned char next;

   if (!(Side&(next=FF_BOTTOM)) && ILVIN(Inter,Quad[0],Quad[1])) {
      *X+=Depth*ILFAC(Inter,Quad[0],Quad[1]);
      if (Side) {
         *QY-=Depth;
         next=FF_TOP;
      }
   } else if (!(Side&(next=FF_RIGHT)) && ILVIN(Inter,Quad[1],Quad[2])) {
      *X+=Depth;
      *Y+=Depth*ILFAC(Inter,Quad[1],Quad[2]);
      if (Side) {
         *QX+=Depth;
         next=FF_LEFT;
      }
   } else if (!(Side&(next=FF_TOP)) && ILVIN(Inter,Quad[3],Quad[2])) {
      *X+=Depth*ILFAC(Inter,Quad[3],Quad[2]);
      *Y+=Depth;
      if (Side) {
         *QY+=Depth;
         next=FF_BOTTOM;
      }
   } else if (!(Side&(next=FF_LEFT)) && ILVIN(Inter,Quad[0],Quad[3])) {
      *Y+=Depth*ILFAC(Inter,Quad[0],Quad[3]);
      if (Side) {
         *QX-=Depth;
         next=FF_RIGHT;
      }
   } else {
      next=FF_NONE;
   }
   return(next);
}

int FFQuad_Select(double Depth,double *Quad) {

/*
   double mid[5];
   double vox[4];

   mid[0]=(vox[0]+vox[1])*0.5;
   mid[1]=(vox[1]+vox[2])*0.5;
   mid[2]=(vox[2]+vox[3])*0.5;
   mid[3]=(vox[3]+vox[0])*0.5;
   mid[4]=(vox[0]+vox[1]+vox[2]+vox[3])*0.25f;

   vox[0]=Quad[0];vox[1]=mid[0];vox[2]=mid[4];vox[3]=mid[3];
  while(side=FFQuad_Cross(Depth,side,vox,Inter,&X,&Y,&x,&y)) {
   }
*/
   return(0);
}

int FFContour_Quad(TGeoRef *Ref,TDataDef *Def,Vect3d *Line,unsigned char *PMatrix,int X,int Y,int Z,float Inter,int Mode) {

   double        vox[4],x,y;
   double        lat=0.0,lon=0.0,delta;
   unsigned char side=FF_NONE,flag;
   unsigned int  idx,id,n=0;

   Z*=FSIZE2D(Def);
   delta=1.0;

   while(X>=0 && X<Def->NI-1 && Y>=0 && Y<Def->NJ-1) {

      idx=Def->NI*Y+X;
      flag=!Line?side:side<<4;

      if (PMatrix[idx]&flag) break;
      PMatrix[idx]|=flag;

      x=X;
      y=Y;

      id=idx+Z;           Def_GetMod(Def,id,vox[0]);
      id=idx+1+Z;         Def_GetMod(Def,id,vox[1]);
      id=idx+Def->NI+1+Z; Def_GetMod(Def,id,vox[2]);
      id=idx+Def->NI+Z;   Def_GetMod(Def,id,vox[3]);

      if (side=FFQuad_Cross(delta,side,vox,Inter,&X,&Y,&x,&y)) {

         if (Line) {
            switch(Mode) {
               case REF_COOR : Ref->Project(Ref,x,y,&lat,&lon,0,1);Vect_Init(Line[n],lat,lon,0.0);break;
               case REF_PROJ : VertexLoc(Ref,Def,Line[n],x,y,Z);break;
               case REF_GRID : Vect_Init(Line[n],x,y,Z);break;
            }
         }
         n++;
      } else {
         break;
      }
   }
   return(n);
}

/*----------------------------------------------------------------------------
 * Nom      : <FFContour>
 * Creation : Juin 2003 - J.P. Gauthier - CMC/CMOE
 *
 * But      : Produire le ligne de contours
 *
 * Parametres :
 *  <Ref>     : GeoReference
 *  <Def>     : Definitions des donnees
 *  <Line>    : Contour resultant (NULL -> Prepass)
 *  <PMatrix> : Matrice contenant les drapeaux de proximite des lignes de champs
 *  <X>       : Position X en point de grille
 *  <Y>       : Position Y en point de grille
 *  <Z>       : Position Z en point de grille
 *  <Level>   : Intervalle du contour
 *  <Mode>    : Type de referenciel (REF_COO,REF_GRID,REF_PROJ)
 *
 * Retour:
 *
 * Remarques :
 *
 *   La numerotation interne des vertex et des cotes/triangles d'un voxel est:
 *
 *          -4
 *       3 ----- 2
 *       | \ 2 / |
 *       |  \ /  |
 *    -3 |3  4  1| -1     4 (Valeur centrale du voxel)
 *       |  / \  |
 *       | / 0 \ |
 *       0 ----- 1
 *          -2
 *
 *   PMatrix    Permet de savoir si un voxel a deja ete visite
 *   first      Permet d'eliminer la possibilite du vertex precendent aux depart d'un cellule ou il y aura 2 vertex
 *   side       Determine quel cote ne pas tenir compte dans le prochain voxel
 *
 *   2 PASS algorithm :
 *   1st PASS - Calculating the length of a contour line (if Line NULL)
 *   2nd PASS - Stocking the position of the contour line
 *
 *----------------------------------------------------------------------------
 */
int FFContour_Triangle(TGeoRef *Ref,TDataDef *Def,Vect3d *Line,unsigned char *PMatrix,int X,int Y,int Z,float Level,int Mode,int Triangle) {

   int           idx,n=0,t=-4,z;
   double        vox[5],x,y,fac;
   double        lat=0.0,lon=0.0;
   char          side=0,first=1,found=0;
   unsigned char *tid;
   unsigned char tid1[4]      = { 0x01,0x02,0x04,0x08 };
   unsigned char tid2[4]      = { 0x10,0x20,0x40,0x80 };
   char          tri[4][3]    = { {  0,1,4 },{  1,2,4 },{  2,3,4 },{  3,0,4 } };      /*Current triangle vertices*/
   char          trn[4][3]    = { { -2,1,3 },{ -1,2,0 },{ -4,3,1 },{ -3,0,2 } };      /*Next triangle to process*/
   char          trd[4][2]    = { { 0,1 }   ,{ -1,0 }  ,{ 0,-1 }  ,{ 1,0 } };         /*Increment in voxel space*/
   double        trf[4][3][2] = { { { 0.0,0.0 },{ 0.5,0.0 },{ 0.0,0.0 } },            /*distance delta to increment*/
                                  { { 1.0,0.0 },{ 0.5,0.5 },{ 0.5,0.0 } },
                                  { { 0.0,1.0 },{ 0.0,0.5 },{ 0.5,0.5 } },
                                  { { 0.0,0.0 },{ 0.0,0.0 },{ 0.0,0.5 } } };
   char          trs[4][3][2] = { { { 0,0 }    ,{ 1,0 }    ,{ 1,1 } },                /*Inversion of interpolated length*/
                                  { { 0,0 }    ,{ 1,1 }    ,{ 0,1 } },
                                  { { 1,0 }    ,{ 0,1 }    ,{ 0,0 } },
                                  { { 0,1 }    ,{ 0,0 }    ,{ 1,0 } } };

   /*Define mask according to pass*/
   tid=(!Line)?tid1:tid2;
   z=Z*FSIZE2D(Def);
   t=Triangle-4;

   /*Process the voxels*/
   while(t<4 && X>=0 && X<Def->NI-1 && Y>=0 && Y<Def->NJ-1) {
      idx=z+Def->NI*(Y+1)+X;
      Def_GetMod(Def,idx,vox[3])
      idx++;
      Def_GetMod(Def,idx,vox[2])
      idx=z+Def->NI*Y+X;
      Def_GetMod(Def,idx,vox[0])
      idx++;
      Def_GetMod(Def,idx,vox[1])
      idx=idx-z-1;

      if (vox[0]==vox[1] && vox[1]==vox[2] && vox[2]==vox[3]) {
         break;
      }

      vox[4]=(vox[0]+vox[1]+vox[2]+vox[3])*0.25;

      t+=4;

      if (PMatrix[idx]&tid[t]) {
         return(n);
      }

      /*Process the inner triangles*/
      while(t>=0 && t<4) {
         found=1;

         if (!(side&0x01) && ILVIN(Level,vox[tri[t][0]],vox[tri[t][1]])) {
            fac=ILFAC(Level,vox[tri[t][0]],vox[tri[t][1]]);
            x=X+trf[t][0][0]+(trd[t][0]?0:ILADD(trs[t][0][0],fac));
            y=Y+trf[t][0][1]+(trd[t][0]?ILADD(trs[t][0][1],fac):0.0);
            side=0x01;
            PMatrix[idx]|=tid[t];
            if (!first) { t=trn[t][0]; } else { first=0; }
         } else if (!(side&0x02) && ILVIN(Level,vox[tri[t][1]],vox[tri[t][2]])) {
            fac=ILFAC(Level,vox[tri[t][1]],vox[tri[t][2]]);
            x=X+trf[t][1][0]+ILADD(trs[t][1][0],fac)*0.5;
            y=Y+trf[t][1][1]+ILADD(trs[t][1][1],fac)*0.5;
            side=0x04;
            PMatrix[idx]|=tid[t];
            t=trn[t][1];
         } else if (!(side&0x04) && ILVIN(Level,vox[tri[t][2]],vox[tri[t][0]])) {
            fac=ILFAC(Level,vox[tri[t][2]],vox[tri[t][0]]);
            x=X+trf[t][2][0]+ILADD(trs[t][2][0],fac)*0.5;
            y=Y+trf[t][2][1]+ILADD(trs[t][2][1],fac)*0.5;
            side=0x02;
            PMatrix[idx]|=tid[t];
            t=trn[t][2];
         } else {
            found=0;
            t++;
         }

         if (found) {
            if (Line) {
               switch(Mode) {
                  case REF_COOR : Ref->Project(Ref,x,y,&lat,&lon,0,1);Vect_Init(Line[n],lat,lon,0.0);break;
                  case REF_PROJ : VertexLoc(Ref,Def,Line[n],x,y,Z); break;
                  case REF_GRID : Vect_Init(Line[n],x,y,Z); break;
               }
            }
            n++;
         }
      }

      /*Increment voxel*/
      if (t<0) {
         X+=trd[t+4][0];
         Y+=trd[t+4][1];
      }
   }
   return(n);
}
/*----------------------------------------------------------------------------
 * Nom      : <FFMarchingCube>
 * Creation : Decembre 2000 - J.P. Gauthier - CMC/CMOE
 *
 * But      : Effectue le rendue du volume du champ.
 *
 * Parametres :
 *  <Ref>     : GeoReference
 *  <Def>     : Definitions des donnees
 *  <Proj>    : Parametres de la projection
 *  <Level>   : Valeur de l'isosurface
 *  <Vr>      : Vecteurs resultants
 *  <Do>      : Pass (1st, count, second, process
 *
 * Retour:
 *
 * Remarques :
 *
 *----------------------------------------------------------------------------
*/
int FFMarchingCube(TGeoRef *Ref,TDataDef *Def,Projection *Proj,double Level,Vect3d *Vr,int Do) {

   int    n,i,j,k;
   int    cubeidx,vridx=0;
   int    idxj,idxj1,idxk,idxk1,idxi;
   double cube[2][4];
   Vect3d vrlist[12],p0,p1;

#ifdef DEBUG
   if (Do) {
      fprintf(stderr,"(DEBUG) FFMarchingCube: Processing volume\n");
   } else {
      fprintf(stderr,"(DEBUG) FFMarchingCube: Checking volume dimension\n");
   }
#endif

   for (k=Def->Limits[2][0];k<Def->Limits[2][1];k++) {
      idxk=FSIZE2D(Def)*k;
      idxk1=idxk+FSIZE2D(Def);

      for (j=Def->Limits[1][0];j<Def->Limits[1][1];j++) {
         idxj=j*Def->NI;
         idxj1=idxj+Def->NI;

         for (i=Def->Limits[0][0];i<Def->Limits[0][1];i++) {

            /*Initialize current cube*/
            idxi=idxk+idxj+i;     Def_GetMod(Def,idxi,cube[0][0]);
            idxi++;               Def_GetMod(Def,idxi,cube[0][1]);
            idxi=idxk+idxj1+i;    Def_GetMod(Def,idxi,cube[0][3]);
            idxi++;               Def_GetMod(Def,idxi,cube[0][2]);
            idxi=idxk1+idxj+i;    Def_GetMod(Def,idxi,cube[1][0]);
            idxi++;               Def_GetMod(Def,idxi,cube[1][1]);
            idxi=idxk1+idxj1+i;   Def_GetMod(Def,idxi,cube[1][3]);
            idxi++;               Def_GetMod(Def,idxi,cube[1][2]);

            /*Find index from side table*/
            cubeidx = 0;
            if (cube[0][0] < Level) cubeidx |= 1;
            if (cube[0][1] < Level) cubeidx |= 2;
            if (cube[0][2] < Level) cubeidx |= 4;
            if (cube[0][3] < Level) cubeidx |= 8;
            if (cube[1][0] < Level) cubeidx |= 16;
            if (cube[1][1] < Level) cubeidx |= 32;
            if (cube[1][2] < Level) cubeidx |= 64;
            if (cube[1][3] < Level) cubeidx |= 128;

            /* Cube is entirely in/out of the surface */
            if (EdgeTable[cubeidx] == 0)
               continue;

            /* Find the vertices where the surface intersects the cube */
            if (EdgeTable[cubeidx] & 1) {
               Vect_Init(p0,i,j,k);
               Vect_Init(p1,i+1,j,k);
               VertexInterp(vrlist[0],p0,p1,cube[0][0],cube[0][1],Level);
            }
            if (EdgeTable[cubeidx] & 2) {
               Vect_Init(p0,i+1,j,k);
               Vect_Init(p1,i+1,j+1,k);
               VertexInterp(vrlist[1],p0,p1,cube[0][1],cube[0][2],Level);
            }
            if (EdgeTable[cubeidx] & 4) {
               Vect_Init(p0,i+1,j+1,k);
               Vect_Init(p1,i,j+1,k);
               VertexInterp(vrlist[2],p0,p1,cube[0][2],cube[0][3],Level);
            }
            if (EdgeTable[cubeidx] & 8) {
               Vect_Init(p0,i,j+1,k);
               Vect_Init(p1,i,j,k);
               VertexInterp(vrlist[3],p0,p1,cube[0][3],cube[0][0],Level);
            }
            if (EdgeTable[cubeidx] & 16) {
               Vect_Init(p0,i,j,k+1);
               Vect_Init(p1,i+1,j,k+1);
               VertexInterp(vrlist[4],p0,p1,cube[1][0],cube[1][1],Level);
            }
            if (EdgeTable[cubeidx] & 32) {
               Vect_Init(p0,i+1,j,k+1);
               Vect_Init(p1,i+1,j+1,k+1);
               VertexInterp(vrlist[5],p0,p1,cube[1][1],cube[1][2],Level);
            }
            if (EdgeTable[cubeidx] & 64) {
               Vect_Init(p0,i+1,j+1,k+1);
               Vect_Init(p1,i,j+1,k+1);
               VertexInterp(vrlist[6],p0,p1,cube[1][2],cube[1][3],Level);
            }
            if (EdgeTable[cubeidx] & 128) {
               Vect_Init(p0,i,j+1,k+1);
               Vect_Init(p1,i,j,k+1);
               VertexInterp(vrlist[7],p0,p1,cube[1][3],cube[1][0],Level);
            }
            if (EdgeTable[cubeidx] & 256) {
               Vect_Init(p0,i,j,k);
               Vect_Init(p1,i,j,k+1);
               VertexInterp(vrlist[8],p0,p1,cube[0][0],cube[1][0],Level);
            }
            if (EdgeTable[cubeidx] & 512) {
               Vect_Init(p0,i+1,j,k);
               Vect_Init(p1,i+1,j,k+1);
               VertexInterp(vrlist[9],p0,p1,cube[0][1],cube[1][1],Level);
            }
            if (EdgeTable[cubeidx] & 1024) {
               Vect_Init(p0,i+1,j+1,k);
               Vect_Init(p1,i+1,j+1,k+1);
               VertexInterp(vrlist[10],p0,p1,cube[0][2],cube[1][2],Level);
            }
            if (EdgeTable[cubeidx] & 2048) {
               Vect_Init(p0,i,j+1,k);
               Vect_Init(p1,i,j+1,k+1);
               VertexInterp(vrlist[11],p0,p1,cube[0][3],cube[1][3],Level);
            }

            /* Create the triangle */
            for (n=0;TriTable[cubeidx][n]!=-1;n+=3) {
               if (Do) {
                  VertexLoc(Ref,Def,Vr[vridx+1],vrlist[TriTable[cubeidx][n]][0]  ,vrlist[TriTable[cubeidx][n]][1]  ,vrlist[TriTable[cubeidx][n]][2]);
                  VertexLoc(Ref,Def,Vr[vridx+3],vrlist[TriTable[cubeidx][n+1]][0],vrlist[TriTable[cubeidx][n+1]][1],vrlist[TriTable[cubeidx][n+1]][2]);
                  VertexLoc(Ref,Def,Vr[vridx+5],vrlist[TriTable[cubeidx][n+2]][0],vrlist[TriTable[cubeidx][n+2]][1],vrlist[TriTable[cubeidx][n+2]][2]);

                  /*Find normals from gradient within a single voxel*/
                  Vect_Assign(Vr[vridx]  ,vrlist[TriTable[cubeidx][n]]);
                  Vect_Assign(Vr[vridx+2],vrlist[TriTable[cubeidx][n+1]]);
                  Vect_Assign(Vr[vridx+4],vrlist[TriTable[cubeidx][n+2]]);

                  VertexGradient(Ref,Def,Vr[vridx]);Vect_Mul(Vr[vridx],Vr[vridx],Proj->LightPos);        Vect_Normalize(Vr[vridx]);
                  VertexGradient(Ref,Def,Vr[vridx+2]);Vect_Mul(Vr[vridx+2],Vr[vridx+2],Proj->LightPos);  Vect_Normalize(Vr[vridx+2]);
                  VertexGradient(Ref,Def,Vr[vridx+4]);Vect_Mul(Vr[vridx+4],Vr[vridx+4],Proj->LightPos);  Vect_Normalize(Vr[vridx+4]);
                  vridx+=6;
               } else {
                  vridx+=3;
               }
            }
         }
      }
   }

#ifdef DEBUG
   if (Do) {
      fprintf(stderr,"(DEBUG) FFMarchingCube: Done processing (%i Vertex)\n",vridx/3);
   }
#endif
   return(vridx);
}

/*----------------------------------------------------------------------------
 * Nom      : <FFStreamLine>
 * Creation : Janvier 2004 - J.P. Gauthier - CMC/CMOE
 *
 * But      : Produire les lignes de champs d'un champ vectoriel
 *
 * Parametres :
 *  <Ref>     : GeoReference
 *  <Def>     : Definitions des donnees
 *  <VP>      : Viewport
 *  <Stream>  : Ligne de champs courante
 *  <Map>     : Map des norme des vecteurs
 *  <X>       : Position X en point de grille
 *  <Y>       : Position Y en point de grille
 *  <Z>       : Position Z en point de grille
 *  <MaxIter> : Le maximum de pas pour une ligne de champs
 *  <Min>     : Valeur minimale a ne pas franchir
 *  <Res>     : Resolution minimale du deplacement
 *  <Mode>    : La longueur de pas d'une ligne de champs
 *  <ZDim>    : Traitement en 3D
 *
 * Retour:
 *
 * Remarques :
 *   For each point...
 *
 *   1) Get the particle first position (grid point)
 *   2) Get the velocity vector of the particle by interpolation (if necessary) and normalize
 *   3) Calcultate the next position of the particle and store it
 *   4) Reiterate until the particle is outside the field or is stationnary
 *
 *   Maximum number of position is determinated by the max. number of iteration and
 *   the position of the particle itself.
 *
 *----------------------------------------------------------------------------
*/
int FFStreamLine(TGeoRef *Ref,TDataDef *Def,ViewportItem *VP,Vect3d *Stream,float *Map,double X,double Y,double Z,int MaxIter,double Step,double Min,double Res,int Mode,int ZDim) {

   int npos=0;         /*Number of position in a particular streamline/fieldline*/
   int iter=0;         /*Keep track of the number of iteration of a numerical method*/
   int c=0;            /*Keep track of the number of iteration before a check in stencil buffer*/
   int idx,n;          /*Current stream index*/
   Vect3d v,p;
   Vect3d rk1,rk2;     /*Keep track of Runge Kutta steps (1 to 2)*/
   Vect3d d;
   double step,t,ds,dr,dn,dv=0.0;/*Minimum delta step accepted*/

   Vect3d pix;
   GLuint s;

   Vect_Clear(p);
   ds=fabs(Step*0.001);
   dr=10000.0;

   do {

      /*Store the parcel position itself.*/
      idx=Step>0?npos:MaxIter-1-npos;

      if (X<Def->Limits[0][0]+1 || X>Def->Limits[0][1]-2 || Y<Def->Limits[1][0]+1 || Y>Def->Limits[1][1]-2 || (ZDim && (Z<Def->Limits[2][0]+1 || Z>Def->Limits[2][1]-2))) {
         break;
      } else {
         /*Keep the position if its moved enough*/
         if (dr>=Res) {
            if (Map)
               Map[idx]=dv;

            switch(Mode) {
               case REF_COOR : Ref->Project(Ref,X,Y,&Stream[idx][0],&Stream[idx][1],0,1);
                               n=floor(Z);
                               Stream[idx][2]=Data_Level2Meter(Ref->LevelType,ILIN(Ref->Levels[n],Ref->Levels[n+1],Z-n));
                               break;

               case REF_PROJ : VertexLoc(Ref,Def,Stream[idx],X,Y,Z);
                               break;

               case REF_GRID : Vect_Init(Stream[idx],X,Y,Z);
                               break;
            }
            npos++;
            iter++;
            dr=0.0;
         }
      }

      /*If in 2D mode*/
      if (!ZDim) {
         /* Did we ever cross this pixel before ? */
         /* Stencil buffer reads are very slow so we only check after a few steps */
         if (VP && c>MaxIter>>5) {
            gluProject(Stream[idx][0],Stream[idx][1],Stream[idx][2],VP->GLModR,VP->GLProj,VP->GLView,&pix[0],&pix[1],&pix[2]);
            glReadPixels(pix[0],pix[1],1,1,GL_STENCIL_INDEX,GL_UNSIGNED_INT,&s);
            if (s&0x2) break;
            c=0;
         }
         c++;
      }

      /*Next vector*/
      v[0]=VertexValN(Ref,Def,0,X,Y,Z);
      v[1]=VertexValN(Ref,Def,1,X,Y,Z);
      v[2]=ZDim?VertexValN(Ref,Def,2,X,Y,Z):0.0;
      dv=Vect_Norm(v);

      /*If we break the minimum barrier*/
      if (Min!=0.0 && dv<=Min) {
         break;
      }

      /*Normalize the velocity vector*/
      t=1.0/dv;
      Vect_SMul(v,v,t);

      /*Figure out step from direction gradient*/
      t=Vect_Weight(p,v);
      step=(1.0-t)*Step;
      step=step>0.25?0.25:step;
      Vect_Assign(p,v);

      /*Use Runge Kutta method (2nd order) to find the next particle position*/
      RK(rk1,step,v)
      rk2[0]=VertexValN(Ref,Def,0,X+rk1[0],Y,Z);
      rk2[1]=VertexValN(Ref,Def,1,X,Y+rk1[1],Z);
      rk2[2]=ZDim?VertexValN(Ref,Def,2,X,Y,Z+rk1[2]):0.0;
      Vect_Normalize(rk2);
      RK(rk2,step,rk2)
      RKT(d,rk1,rk2)

      dn=Vect_Norm(d);

      /*Check if the particle has moved enough*/
      if (dn<ds) {
         break;
      }

      dr+=dn;
      X+=d[0];
      Y+=d[1];
      Z+=d[2];

   } while (iter<MaxIter-1);

   return(npos);
}

int FFStreamPatch(TGeoRef *Ref,TDataDef *Def,ViewportItem *VP,Vect3d *Stream,float I,float J,float K,int MaxIter,float Step) {

   int npos=0;           /* Number of position in a particular streamline/fieldline */
   int iter=0;           /* Keep track of the number of iteration of a numerical method */
   int idx;              /* Current stream index */
   float u,v;            /* Decomposed vector speed of a particle (in u,v) */
   float uk1,uk2;        /* Keep track of Runge Kutta steps (1 to 2) */
   float vk1,vk2;        /* "" */
   float oi,oj;          /* Previous positions */
   float ds,di=0,dj=0;   /* Minimum delta step accepted*/

   float r=1.0,mi=1.0,mj=1.0;

   ds=Step*0.001;

   do {

      /* Did we ever cross this pixel before ? */
      /* Stencil buffer reads are very slow so we only check after a few steps
      if (c>MaxIter>>5) {
         gluProject(Stream[idx][0],Stream[idx][1],Stream[idx][2],VP->GLModR,VP->GLProj,VP->GLView,&pix[0],&pix[1],&pix[2]);
         glReadPixels(pix[0],pix[1],1,1,GL_STENCIL_INDEX,GL_UNSIGNED_INT,&s);
         if (s>0x8) break;
         c=0;
      }
      c++;
*/
      /* Next vector */
      u=VertexValN(Ref,Def,0,I,J,0);
      v=VertexValN(Ref,Def,1,I,J,0);
      NORMALIZE(u,v);

      /* Use Runge Kutta method (2nd order) to find the next particle position */
      uk1=Step*u;
      vk1=Step*v;
      uk2=VertexValN(Ref,Def,0,I+uk1,J,0);
      vk2=VertexValN(Ref,Def,1,I,J+vk1,0);
      NORMALIZE(uk2,vk2);
      uk2*=Step;
      vk2*=Step;

      di=0.5*(uk1+uk2);
      dj=0.5*(vk1+vk2);
      I+=di;
      J+=dj;

      /* Check if the particle has moved enough */
      if (fabs(I-oi)<ds && fabs(J-oj)<ds) {
         break;
      }

      if (Step>0) { di=-di;dj=-dj;}
      if (mj!=0.0 && dj!=0.0) {
         r=fabs((mi/mj)-(di/dj));
         r=r>1.0?1.0:1.0-r;
         fprintf(stderr,"(DEBUG) %f %f %f %f %f\n",r,(mi/mj),(di/dj),di,dj);
      }
      mi=di;
      mj=dj;
      dj*=8.0*r;
      di*=8.0*r;

      oi=I;
      oj=J;

      /* Store the parcel position itself */
      idx=Step>0?npos:MaxIter*2-1-npos;
      if (VertexLoc(Ref,Def,Stream[idx],I+dj,J-di,0)) {
         npos++;
      } else {
         break;
      }

      idx=Step>0?npos:MaxIter*2-1-npos;
      if (VertexLoc(Ref,Def,Stream[idx],I-dj,J+di,0)) {
         npos++;
      } else {
         break;
      }
   } while (++iter<MaxIter-1);

   return npos;
}

/*----------------------------------------------------------------------------
 * Nom      : <FFCellResolution>
 * Creation : Fevrier 2004 - J.P. Gauthier - CMC/CMOE
 *
 * But      : Determiner la resolution optimale d'une cellule
 *
 * Parametres :
 *  <VP>      : Viewport
 *  <Proj>    : Parametres de la projection
 *  <G0>      : Coin inferieur gauche
 *  <G1>      : Coin superieur droit
 *
 * Retour     :
 *  <Res>     : Nombre de pixel par cellule
 *
 * Remarques :
 *
 *----------------------------------------------------------------------------
 */
float FFCellResolution(ViewportItem *VP,Projection *Proj,Vect3d G0,Vect3d G1) {

   Vect3d dif,pix0,pix1;

   gluProject(G0[0],G0[1],G0[2],VP->GLModR,VP->GLProj,VP->GLView,&pix0[0],&pix0[1],&pix0[2]);
   gluProject(G1[0],G1[1],G1[2],VP->GLModR,VP->GLProj,VP->GLView,&pix1[0],&pix1[1],&pix1[2]);

   Vect_Substract(dif,pix1,pix0);

   dif[0]=ABS(dif[0]);
   dif[1]=ABS(dif[1]);

   return(FMAX(dif[0],dif[1]));
}

/*----------------------------------------------------------------------------
 * Nom      : <FFCellProcess>
 * Creation : Fevrier 2004 - J.P. Gauthier - CMC/CMOE
 *
 * But      : Determiner si une cellule de grille doit etre rendue
 *
 * Parametres :
 *  <VP>      : Parametres du viewport
 *  <Proj>    : Parametres de la projection
 *  <G0>      : Position du point de grille 0
 *  <G1>      : Position du point de grille 1
 *  <G2>      : Position du point de grille 2
 *  <G3>      : Position du point de grille 3
 *
 * Retour     :
 *  <Dim>     : Dimensions maximales dans chaque axes
 *
 * Remarques :
 *
 *----------------------------------------------------------------------------
 */
int FFCellProcess(ViewportItem *VP,Projection *Proj,Vect3d G0,Vect3d G1,Vect3d G2,Vect3d G3,Vect3d Dim) {

   Vect3d min,max,pix;

   PROJCHECK(Proj,G0[0]);
   PROJCHECK(Proj,G1[0]);
   PROJCHECK(Proj,G2[0]);
   PROJCHECK(Proj,G3[0]);

   if (ABS(G0[0]-G1[0])>1 || ABS(G2[0]-G3[0])>1) {
      return(0);
   }

   gluProject(G0[0],G0[1],G0[2],VP->GLModR,VP->GLProj,VP->GLView,&pix[0],&pix[1],&pix[2]);
   Vect_Assign(min,pix);
   Vect_Assign(max,pix);
   gluProject(G1[0],G1[1],G1[2],VP->GLModR,VP->GLProj,VP->GLView,&pix[0],&pix[1],&pix[2]);
   Vect_Min(min,min,pix);
   Vect_Max(max,max,pix);
   gluProject(G2[0],G2[1],G2[2],VP->GLModR,VP->GLProj,VP->GLView,&pix[0],&pix[1],&pix[2]);
   Vect_Min(min,min,pix);
   Vect_Max(max,max,pix);
   gluProject(G3[0],G3[1],G3[2],VP->GLModR,VP->GLProj,VP->GLView,&pix[0],&pix[1],&pix[2]);
   Vect_Min(min,min,pix);
   Vect_Max(max,max,pix);

   /*Is the quad visible ???*/
   if (!VOUT(min[0],max[0],0,Proj->Params->VP->Width) && !VOUT(min[1],max[1],0,Proj->Params->VP->Height) && !VOUT(min[2],max[2],0,1)) {
      if (Dim) {
         Vect_Substract(Dim,max,min);
      }
      return(1);
  }
  return(0);
}

