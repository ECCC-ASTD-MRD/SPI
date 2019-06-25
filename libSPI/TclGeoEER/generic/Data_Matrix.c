/*=============================================================================
 * Environnement Canada
 * Centre Meteorologique Canadian
 * 2100 Trans-Canadienne
 * Dorval, Quebec
 *
 * Projet    : Librairie Tcl de manipulation de type de donnees.
 * Fichier   : Data_Matrix.c
 * Creation  : Aout 2000 - J.P. Gauthier - CMC/CMOE
 *
 * Description: Fonctions d'iteration de la calculatrice de donnees.
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

#include "App.h"
#include "RPN.h"
#include "Data_Matrix.h"

extern Tcl_Interp *GInterp;
extern TData      *GField,*GFieldP;
extern TDef   *GData[256];
extern TDef_Type GType;
extern int        GDataN;

void Calc_Iterate1(TDef *R,TDef *A,TFunc1 *Func) {

   unsigned long i,sa;
   int           n=0;
   double        a=0;

   sa=FSIZE3D(A);

   while (A->Data[n]) {
      for(i=0;i<sa;i++) {
         Def_Get(A,n,i,a);
         a=Func(a);
         Def_Set(R,n,i,a);
      }
      n++;
   }
}

void Calc_Iterate2(TDef *R,TDef *A,TDef *B,TFunc2 *Func) {

   unsigned long i,sa,sb;
   int           n=0,t=0;
   double        a=0,b=0;

   sa=FSIZE3D(A);
   sb=FSIZE3D(B);

   if (sa==1) t|=0x10;
   if (sb==1) t|=0x01;

   sa=sa>sb?sa:sb;
   while (A->Data[n] || B->Data[n]) {
      for(i=0;i<sa;i++) {
         Def_Get(A,(A->Data[n]?n:0),(t&0x10?0:i),a);
         Def_Get(B,(B->Data[n]?n:0),(t&0x01?0:i),b);
            
         a=Func(a,b);
         Def_Set(R,n,i,a);
      }
      n++;
   }
}

void Calc_Iterate3(TDef *R,TDef *A,TDef *B,TDef *C,TFunc3 *Func) {

   unsigned long i,sa,sb,sc;
   int           n=0,t=0;
   double        a=0,b=0,c=0;

   sa=FSIZE3D(A);
   sb=FSIZE3D(B);
   sc=FSIZE3D(C);

   if (sa==1) t|=0x0100;
   if (sb==1) t|=0x0010;
   if (sc==1) t|=0x0001;

   sa=sa>sb?sa:sb;
   sa=sa>sc?sa:sc;

   while (A->Data[n] || B->Data[n] || C->Data[n]) {
      for(i=0;i<sa;i++) {
         Def_Get(A,(A->Data[n]?n:0),(t&0x0100?0:i),a);
         Def_Get(B,(B->Data[n]?n:0),(t&0x0010?0:i),b);
         Def_Get(C,(C->Data[n]?n:0),(t&0x0001?0:i),c);
         a=Func(a,b,c);
         Def_Set(R,n,i,a);
      }
      n++;
   }
}

/**
 * @author Christian Lavoie
 * @brief Checks for compatibility between two matrices
 * @param A Some matrix
 * @param B Some other matrix
 *
 * @return Code de retour TCL
 *
 * This call is grossly inneficient with its use of copy between buffers, but
 * this is C-based, so what did you expect?
 *
 */
TDef *Calc_Compat(Tcl_Interp *Interp,TDef *A,TDef *B,int Dim,int Vect) {

#ifdef DEBUG
   fprintf(stdout,"(DEBUG) Called Calc_Compat(A:%p,B:%p)\n",(void*)A,(void*)B);
#endif

   /* Something would be DEFINITELY wrong here */
   if (!A) {
      Tcl_AppendResult(Interp,"Calc_Compat: Field A is null!",(char*)NULL);
      return(NULL);
   }

   if (!B) {
      Tcl_AppendResult(Interp,"Calc_Compat: Field B is null!",(char*)NULL);
      return(NULL);
   }

   /* Check for the 1x1x1 wildcard sizes */
   if (FSIZE3D(A)!=1 && FSIZE3D(B)!=1) {

      /* Check dimensions */
      if (A->NI!=B->NI || A->NJ!=B->NJ || (A->NK!=B->NK && Dim==3)) {
         Tcl_AppendResult(Interp,"Calc_Compat: Dimensions don't correspond",(char*)NULL);
         return(NULL);
      }

      /* Check that both have vectorial data, or neither do */
      if (Vect) {
         if ((A->Data[1] && !B->Data[1]) || (!A->Data[1] && B->Data[1])) {
            Tcl_AppendResult(Interp,"Calc_Compat: Trying to manipulate vectorial and scalar field",(char*)NULL);
            return(NULL);
         }

         if(Vect && ((A->Data[2] && !B->Data[2]) || (!A->Data[2] && B->Data[2]))) {
            Tcl_AppendResult(Interp,"Calc_Compat: Trying to manipulate vectorial and scalar field",(char*)NULL);
            return(NULL);
         }
      }
   }
   return(FSIZE3D(A)>FSIZE3D(B)?A:B);
}

/**
 * @author Jean-Philippe Gauthier
 * @brief Converts a numeral to a TDef represented one
 * @param Val Numeral to convert
 * @return def new TDef
 *
 * This function converts a std C double to a TDef* of the right type.
 *
 * NI, NJ, NK are initialized to 1 (this shouldn't matter, but is used to
 * prevent new code from segfaulting, you get a Calc_Compat error
 * instead)
 */
TDef* Calc_MatrixFloat(double Val) {

   TDef_Type type;

   GDataN++;

   type=TD_Float32;
   if (Val!=0.0 && fabs(log(fabs(Val)))>35) {
      type=TD_Float64;
   }

   GData[GDataN]=Def_New(1,1,1,1,(GType?GType:type));
   Def_Set(GData[GDataN],0,0,Val);

   return(GData[GDataN]);
}

/**
 * @author Jean-Philippe Gauthier
 * @brief Converts a numeral to a TDef represented one
 * @param Val Numeral to convert
 * @return def new TDef
 *
 * This function converts a std C long to a TDef* of the right type.
 *
 * NI, NJ, NK are initialized to 1 (this shouldn't matter, but is used to
 * prevent new code from segfaulting, you get a Calc_Compat error
 * instead)
 */
TDef* Calc_MatrixInt(long Val) {

   TDef_Type type;

   GDataN++;

   type=TD_Byte;

   if (Val>128) {
      type=TD_Int16;
   }
   if (Val>32768) {
      type=TD_Int32;
   }
   if (Val>2147483648) {
      type=TD_Int64;
   }
   type=TD_Int32;

   GData[GDataN]=Def_New(1,1,1,1,(GType?GType:type));
   Def_Set(GData[GDataN],0,0,Val);
   return(GData[GDataN]);
}

/**
 * @author Jean-Philippe Gauthier
 * @brief  Interpolate one matrix into another
 * @param A      First TDef to iterate over
 * @param B      Second TDef to iterate over
 * @param Degree Interpolation degree
 * @return New TDef containing the results
 */
TDef* Calc_MatrixTo(TDef* A,TDef* B,char Degree) {

   unsigned long k;
   int           n=0;
   double        v=0.0;

#ifdef DEBUG
   fprintf(stdout,"(DEBUG) Called Calc_MatrixTo(A:%p,B:%p,Degree:%i)\n",(void*)A,(void*)B,Degree);
#endif

   GDataN++;
   GData[GDataN]=Def_New(A->NI,A->NJ,B->NK,B->NC,(GType?GType:A->Type));

#ifdef HAVE_RMN
   if (FSIZE3D(GData[GDataN])==1) {
      Def_Get(B,0,0,v);
      Def_Set(GData[GDataN],0,0,v);
   } else if (FSIZE3D(B)==1) {
      while (GData[GDataN]->Data[n]) {
         for(k=0;k<FSIZE3D(GData[GDataN]);k++) {
            Def_Get(B,0,0,v);
            Def_Set(GData[GDataN],n,k,v);
         }
         n++;
      }
   } else {
      if (!GField || !GFieldP || GField->GRef->Type&GRID_VERTICAL || GFieldP->GRef->Type&GRID_VERTICAL) {
         return(NULL);
      }

      if (!Def_GridInterp(GField->GRef,GData[GDataN],GFieldP->GRef,B,'L')) {
         return(NULL);
      }
      GeoRef_Incr(GField->GRef);
   }

#endif
   return(GData[GDataN]);
}

/**
 * @author Jean-Philippe Gauthier
 * @brief Returns a component of a vectorial field
 * @param A Def to work on
 * @param Index Index of the composant
 * @return New TDef* containing the results
 */
TDef* Calc_Index(TDef* A,int Index) {

#ifdef DEBUG
   fprintf(stdout,"(DEBUG) Calc_Index(A:%p,%i)\n",(void*)A,Index);
#endif

   GDataN++;
   GData[GDataN]=Def_New(A->NI,A->NJ,A->NK,1,(GType?GType:A->Type));

   memcpy(GData[GDataN]->Data[0],A->Data[Index],FSIZE3D(A)*TDef_Size[A->Type]);
   return(GData[GDataN]);
}

/**
 * @author Jean-Philippe Gauthier
 * @brief Returns a level component
 * @param A Def to work on
 * @param Index Index of the composant
 * @return New TDef* containing the results
 */
TDef* Calc_IndexValue(TDef* A,int I,int J,int K) {

   double v=0;
   int    n=0;

#ifdef DEBUG
   fprintf(stdout,"(DEBUG) Calc_IndexValue(A:%p,I=%i,J=%i,K=%i)\n",(void*)A,I,J,K);
#endif

   GDataN++;

   GData[GDataN]=Def_New(1,1,1,A->NC,(GType?GType:A->Type));

   while(A->Data[n]) {
      Def_Get(A,n,(FSIZE2D(A)*K)+J*A->NI+I,v);
      Def_Set(GData[GDataN],n,0,v);
      n++;
   }
   return(GData[GDataN]);
}

TDef* Calc_RangeValue(TDef* A,int I0,int I1,int J0,int J1,int K0,int K1) {

   unsigned int  n,j,jn,k,kn;
   void *p0,*p1;

#ifdef DEBUG
   fprintf(stdout,"(DEBUG) Calc_RangeValue(A:%p,I0=%i,I1=%i,J0=%i,J1=%i,K0=%i,K1=%i)\n",(void*)A,I0,I1,J0,J1,K0,K1);
#endif

   if (I0>I1) { n=I0;I0=I1;I1=n; }
   if (J0>J1) { n=J0;J0=J1;J1=n; }
   if (K0>K1) { n=K0;K0=K1;K1=n; }

   if (I0<0 || I1>A->NI-1 || J0<0 || J1>A->NJ-1 || K0<0 || K1>A->NK-1)
      return(NULL);

   GDataN++;
   GData[GDataN]=Def_New(I1-I0+1,J1-J0+1,K1-K0+1,A->NC,A->Type);

   for(k=K0,kn=0;k<=K1;k++,kn++) {
//      GData[GDataN]->GridLevels[kn]=A->GridLevels[k];
      for(j=J0,jn=0;j<=J1;j++,jn++) {
         n=0;
         while(A->Data[n]) {
            Def_Pointer(GData[GDataN],n,FSIZE2D(GData[GDataN])*kn+jn*GData[GDataN]->NI,p0);
            Def_Pointer(A,n,FSIZE2D(A)*k+j*A->NI+I0,p1);
            memcpy(p0,p1,GData[GDataN]->NI*TDef_Size[A->Type]);
            n++;
         }
      }
   }
   return(GData[GDataN]);
}

TDef* Calc_Slice(TDef* A,int N,int D) {

   unsigned int  n,i,j,k;
   void  *p;
   double v=0.0;

#ifdef DEBUG
   fprintf(stdout,"(DEBUG) Calc_Slice(A:%p,N=%i,D=%i)\n",(void*)A,N,D);
#endif

   switch(D) {
      case 0:
         i=N;
         GData[++GDataN]=Def_New(A->NJ,A->NK,1,A->NC,(GType?GType:A->Type));
         n=0;
         while(A->Data[n]) {
            for(k=0;k<A->NK;k++) {
               for(j=0;j<A->NJ;j++) {
                  Def_Get(A,n,FSIZE2D(A)*k+j*A->NI+i,v);
                  Def_Set(GData[GDataN],n,k*GData[GDataN]->NI+j,v);
               }
            }
            n++;
         }
         break;

      case 1:
         j=N;
         GData[++GDataN]=Def_New(A->NI,A->NK,1,A->NC,(GType?GType:A->Type));
         n=0;
         while(A->Data[n]) {
            for(k=0;k<A->NK;k++) {
               for(i=0;i<A->NI;i++) {
                  Def_Get(A,n,FSIZE2D(A)*k+j*A->NI+i,v);
                  Def_Set(GData[GDataN],n,k*GData[GDataN]->NI+i,v);
               }
            }
            n++;
         }
         break;

      case 2:
         k=N;
         n=0;
         GData[++GDataN]=Def_New(A->NI,A->NJ,1,A->NC,(GType?GType:A->Type));
//         GData[GDataN]->GridLevels[0]=A->GridLevels[k];
         while(A->Data[n]) {
            Def_Pointer(A,n,FSIZE2D(A)*k,p);
            memcpy(GData[GDataN]->Data[n],p,FSIZE2D(A)*TDef_Size[A->Type]);
            n++;
         }
         break;
   }
   return(GData[GDataN]);
}

TDef* Calc_Set(TDef* A,TDef* B,int I0,int I1,int J0,int J1,int K0,int K1) {

   unsigned int  n=0,i,j,k,bidx,uni=0;
   double  v=0.0;

#ifdef DEBUG
   fprintf(stdout,"(DEBUG) Calc_Set(A:%p,B:%p %i %i %i %i %i %i)\n",(void*)A,(void*)B,I0,I1,J0,J1,K0,K1);
#endif

   if (I0>I1) { n=I0;I0=I1;I1=n; }
   if (J0>J1) { n=J0;J0=J1;J1=n; }
   if (K0>K1) { n=K0;K0=K1;K1=n; }

   if (I0<0 || I1>A->NI-1 || J0<0 || J1>A->NJ-1 || K0<0 || K1>A->NK-1)
      return(NULL);

   if (FSIZE3D(A)==1) {
      if (!Calc_Compat(GInterp,A,B,2,1))
         return NULL;
      Def_Get(B,0,0,v);
      Def_Set(A,0,0,v);
   } else {

// Dangerous but to be revisited
//      if (!Calc_Compat(GInterp,A,B,2,1))
//         return(NULL);

      if (FSIZE3D(B)==1) {
         Def_Get(B,0,0,v);
         uni=1;
      } else if( (I1-I0+1)*(J1-J0+1)*(K1-K0+1) != FSIZE3D(B) ) {
         App_Log(ERROR,"%s: Dimension to assign in grid A (%d) is different from number of values in grid B (%d)\n",__func__,(I1-I0+1)*(J1-J0+1)*(K1-K0+1),(int)FSIZE3D(B));
         return(NULL);
      }

      n=0;
      while(A->Data[n]) {
         if (!B->Data[n] && FSIZE3D(B)!=1) {
            return(NULL);
         }
         for(k=K0,bidx=0;k<=K1;k++) {
            for(j=J0;j<=J1;j++) {
               for(i=I0;i<=I1;i++) {
                  if (!uni) {
                     Def_Get(B,n,bidx,v);
                     ++bidx;
                  }
                  Def_Set(A,n,k*FSIZE2D(A)+j*A->NI+i,v);
               }
            }
         }
         n++;
      }
   }
   return(A);
}
/**
 * @author Jean-Philippe Gauthier
 * @brief Calculates the wind speed (vector length)
 * @param A Def to work on
 * @return New TDef* containing the results
 */
TDef* Calc_Length(TDef* A) {

   unsigned long i;
   double        v[3]= { 0.0, 0.0, 0.0 };
#ifdef DEBUG
   fprintf(stdout,"(DEBUG) Calc_Length(A:%p,Func:%p)\n",(void*)A);
#endif

   GDataN++;

   if (!A->Data[1]) {
      GData[GDataN]=Def_Copy(A);
   } else {
      GData[GDataN]=Def_New(A->NI,A->NJ,A->NK,1,(GType?GType:A->Type));

      for(i=0;i<FSIZE3D(A);i++) {
         Def_Get(A,0,i,v[0]);
         Def_Get(A,1,i,v[1]);
         if (A->Data[2]) {
            Def_Get(A,2,i,v[2]);
            Def_Set(GData[GDataN],0,i,sqrt(v[0]*v[0]+v[1]*v[1]+v[2]*v[2]));
         } else {
            Def_Set(GData[GDataN],0,i,sqrt(v[0]*v[0]+v[1]*v[1]));
         }
      }
   }
   return(GData[GDataN]);
}

TDef* Calc_Dir(TDef* A) {

   unsigned int i,j,k;
   float       *x,*y,*fx,*fy,*spd;
   void        *p,*p0,*p1;

#ifdef DEBUG
   fprintf(stdout,"(DEBUG) Calc_Dir(A:%p,Func:%p)\n",(void*)A);
#endif

#ifdef HAVE_RMN
   if (!GField || GField->GRef->Grid[0]=='V') {
      App_Log(ERROR,"%s: Invalid grid while calculating direction\n",__func__);
      return(NULL);
   }

   GDataN++;

   if (!A->Data[1]) {
      GData[GDataN]=Def_Copy(A);
   } else {
      GData[GDataN]=Def_New(A->NI,A->NJ,A->NK,1,(GType?GType:A->Type));

      spd=(float*)malloc(FSIZE2D(A)*sizeof(float));
      x=fx=(float*)malloc(FSIZE2D(A)*sizeof(float));
      y=fy=(float*)malloc(FSIZE2D(A)*sizeof(float));

      if (!spd || !x || !y) {
         App_Log(ERROR,"%s: Unable to allocate temporary arrays\n",__func__);
         return(NULL);
      }
      for (j=1;j<=A->NJ;j++) {
         for (i=1;i<=A->NI;i++) {
           *x=i;
           *y=j;
            x++;y++;
         }
      }

//      RPN_IntLock();
      for(k=0;k<A->NK;k++) {
         Def_Pointer(GData[GDataN],0,FSIZE2D(GData[GDataN])*k,p);
         Def_Pointer(A,0,FSIZE2D(A)*k,p0);
         Def_Pointer(A,1,FSIZE2D(A)*k,p1);
         c_gdxywdval(GField->GRef->Ids[GField->GRef->NId],spd,p,p0,p1,fx,fy,FSIZE2D(A));
      }
//      RPN_IntUnlock();

      free(fx);
      free(fy);
      free(spd);
   }
#else
   App_ErrorSet("%s: Need RMNLIB",__func__);
#endif

   return(GData[GDataN]);
}

/**
 * @author Jean-Philippe Gauthier
 * @brief Iterates over all elements of a TDef
 * @param a to iterate over
 * @param func Float to float function to apply
 * @return New TDef* containing the results
 */
TDef* Calc_Matrix1(TDef* A,TFunc1 *Func,int Iterate,int Matrix,TDef_Type Type) {

   double v;
#ifdef DEBUG
   fprintf(stdout,"(DEBUG) Called Calc_Matrix1(A:%p,Func:%p)\n",(void*)A,(void*)Func);
#endif

   GDataN++;

   if (Iterate) {
      GData[GDataN]=Def_CopyPromote(A,(GType?GType:(Type?Type:DEFSIGNEDTYPE(A))));
      Calc_Iterate1(GData[GDataN],A,Func);
   } else {
      if (Matrix) {
         GData[GDataN]=Def_CopyPromote(A,(GType?GType:A->Type));
         ((TFunc*)Func)(GData[GDataN],A);
      } else {
         GData[GDataN]=Def_New(1,1,1,1,TD_Float64);
         v=((TFunc*)Func)(A);
         Def_Set(GData[GDataN],0,0,v);
      }
   }

   return(GData[GDataN]);
}

/**
 * @author Jean-Philippe Gauthier
 * @brief Processes two matrices, element to element
 * @param A First TDef to iterate over
 * @param B Second TDef to iterate over
 * @param Func (Float, float) function to apply
 * @return New TDef containing the results
 *
 * <strong>Note:</strong> Non-commutative operations are handle a (op) b. So a
 * division is a / b.
 */
TDef* Calc_Matrix2(TDef* A,TDef* B,TFunc2 *Func,int Iterate,int Matrix,TDef_Type Type) {

   double    v;
   TDef *m,*t;
#ifdef DEBUG
   fprintf(stdout,"(DEBUG) Called Calc_Matrix2(A:%p,B:%p,Func:%p)\n",(void*)A,(void*)B,(void*)Func);
#endif
   m=Calc_Compat(GInterp,A,B,3,1);

   if (Iterate && !m)
      return(NULL);

   GDataN++;

   if (Iterate) {
      t=DEFSELECTTYPE(A,B);
      GData[GDataN]=Def_CopyPromote(m,(GType?GType:(Type?Type:DEFSIGNEDTYPE(t))));
      Calc_Iterate2(GData[GDataN],A,B,Func);
   } else {
      if (Matrix) {
         GData[GDataN]=Def_CopyPromote(A,(GType?GType:A->Type));
         ((TFunc*)Func)(GData[GDataN],A,B);
      } else {
         GData[GDataN]=Def_New(1,1,1,1,TD_Float64);
         v=((TFunc*)Func)(A,B);
         Def_Set(GData[GDataN],0,0,v);
      }
   }
   return(GData[GDataN]);
}

/**
 * @author Jean-Philippe Gauthier
 * @brief Processes three matrices, element to element
 * @param A First TDef to iterate over
 * @param B Second TDef to iterate over
 * @param C Third TDef to iterate over
 * @param Func (float,float,float) function to apply
 * @return New TDef containing the results
 */
TDef* Calc_Matrix3(TDef* A,TDef* B,TDef* C,TFunc3 *Func,int Iterate,int Matrix,TDef_Type Type) {

   double    v;
   TDef *m,*t;

#ifdef DEBUG
   fprintf(stdout,"(DEBUG) Called Calc_Matrix3(A:%p,B:%p,C:%p,Func:%p)\n",(void*)A,(void*)B,(void*)C,(void*)Func);
#endif

/*   if (Iterate && ((Func!=((TFunc3*)ifelse) && !(m=Calc_Compat(GInterp,A,B,3))) || !(m=Calc_Compat(GInterp,B,C,3))))
      return(NULL);
*/

   if ((Func==((TFunc3*)ifelse))) {
      m=Calc_Compat(GInterp,A,Calc_Compat(GInterp,B,C,3,1),3,0);
   } else {
      m=Calc_Compat(GInterp,A,Calc_Compat(GInterp,B,C,3,1),3,1);
   }

   if (Iterate && !m)
      return(NULL);

   GDataN++;

   if (Iterate) {
      t=DEFSELECTTYPE(B,C);
      t=DEFSELECTTYPE(A,t);
      GData[GDataN]=Def_CopyPromote(m,(GType?GType:(Type?Type:DEFSIGNEDTYPE(t))));
      Calc_Iterate3(GData[GDataN],A,B,C,Func);
   } else {
      if (Matrix) {
         GData[GDataN]=Def_CopyPromote(A,(GType?GType:A->Type));
         ((TFunc*)Func)(GData[GDataN],A,B,C);
      } else {
         GData[GDataN]=Def_New(1,1,1,1,TD_Float64);
         v=((TFunc*)Func)(A,B,C);
         Def_Set(GData[GDataN],0,0,v);
      }
   }
   return(GData[GDataN]);
}