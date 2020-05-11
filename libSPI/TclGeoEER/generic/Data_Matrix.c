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
#include "Data_Calc.h"

extern Tcl_Interp *GInterp;
extern TData      *GField,*GFieldP;
extern TDef   *GData[256];
extern TDef_Type GType;
extern int        GDataN;

static double Calc_CallFunc(TFunc *Func,const int NFlds,TDef *Flds[]) {
   switch( NFlds ) {
      case 0:  return Func();
      case 1:  return Func(Flds[0]);
      case 2:  return Func(Flds[0],Flds[1]);
      case 3:  return Func(Flds[0],Flds[1],Flds[2]);
      case 4:  return Func(Flds[0],Flds[1],Flds[2],Flds[3]);
      case 5:  return Func(Flds[0],Flds[1],Flds[2],Flds[3],Flds[4]);
      case 6:  return Func(Flds[0],Flds[1],Flds[2],Flds[3],Flds[4],Flds[5]);
      case 7:  return Func(Flds[0],Flds[1],Flds[2],Flds[3],Flds[4],Flds[5],Flds[6]);
      case 8:  return Func(Flds[0],Flds[1],Flds[2],Flds[3],Flds[4],Flds[5],Flds[6],Flds[7]);
      case 9:  return Func(Flds[0],Flds[1],Flds[2],Flds[3],Flds[4],Flds[5],Flds[6],Flds[7],Flds[8]);
      case 10: return Func(Flds[0],Flds[1],Flds[2],Flds[3],Flds[4],Flds[5],Flds[6],Flds[7],Flds[8],Flds[9]);
      default: Calc_RaiseError("(Calc_CallFunc) Too many arguments to function\n"); return 0.0;
   }
}

static void Calc_Iterate(TDef *R,TFunc *Func,const int NFlds,TDef *Flds[]) {
   size_t idx,n,msk[NFlds],nc;
   double v[NFlds];
   int f;

   // Initialize the meta info for the fields
   for(f=0; f<NFlds; ++f) {
      msk[f] = FSIZE3D(Flds[f])==1 ? 0 : SIZE_MAX;
   }

   n=FSIZE3D(R);
   for(nc=0; nc<R->NC; ++nc) {
      for(idx=0; idx<n; ++idx) {
         for(f=0; f<NFlds; ++f) {
            Def_Get(Flds[f],nc&msk[f],idx&msk[f],v[f]);
         }
         switch (NFlds) {
            case 1:  v[0]=Func(v[0]);                                                  break;
            case 2:  v[0]=Func(v[0],v[1]);                                             break;
            case 3:  v[0]=Func(v[0],v[1],v[2]);                                        break;
            case 4:  v[0]=Func(v[0],v[1],v[2],v[3]);                                   break;
            case 5:  v[0]=Func(v[0],v[1],v[2],v[3],v[4]);                              break;
            case 6:  v[0]=Func(v[0],v[1],v[2],v[3],v[4],v[5]);                         break;
            case 7:  v[0]=Func(v[0],v[1],v[2],v[3],v[4],v[5],v[6]);                    break;
            case 8:  v[0]=Func(v[0],v[1],v[2],v[3],v[4],v[5],v[6],v[7]);               break;
            case 9:  v[0]=Func(v[0],v[1],v[2],v[3],v[4],v[5],v[6],v[7],v[8]);          break;
            case 10: v[0]=Func(v[0],v[1],v[2],v[3],v[4],v[5],v[6],v[7],v[8],v[9]);     break;
            default: Calc_RaiseError("(Calc_Iterate) Too many arguments to function\n"); return;
         }
         Def_Set(R,nc,idx,v[0]);
      }
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
      if (Vect && ((A->Data[1]==NULL)^(B->Data[1]==NULL) || (A->Data[2]==NULL)^(B->Data[2]==NULL))) {
         Tcl_AppendResult(Interp,"Calc_Compat: Trying to manipulate vectorial and scalar field",(char*)NULL);
         return(NULL);
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

   if (Index<0 || Index>A->NC-1) {
      App_Log(ERROR,"%s: Component out of bound of grid A (%d)\n",__func__,A->NC);
      return(NULL);
   }
   
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

   if (I<0 || I>A->NI-1 || J<0 || J>A->NJ-1 || K<0 || K>A->NK-1) {
      App_Log(ERROR,"%s: Dimensions out of bound of grid A (%d x %d x %d)\n",__func__,A->NI,A->NJ,A->NK);
      return(NULL);
   }
   
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

   if (I0<0 || I1>A->NI-1 || J0<0 || J1>A->NJ-1 || K0<0 || K1>A->NK-1) {
      App_Log(ERROR,"%s: Dimensions out of bound of grid A (%d x %d x %d)\n",__func__,A->NI,A->NJ,A->NK);
      return(NULL);
   }
   
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

   unsigned int  n,i,j,k,idx;
   //void  *p;
   double v=0.0;

#ifdef DEBUG
   fprintf(stdout,"(DEBUG) Calc_Slice(A:%p,N=%i,D=%i)\n",(void*)A,N,D);
#endif

   switch(D) {
      case 0:
         i=N;
         if (N<0 || N>A->NI-1) {
            App_Log(ERROR,"%s: Dimensions out of bound of grid A (NI=%d)\n",__func__,A->NI);
            return(NULL);
         }
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
         if (N<0 || N>A->NJ-1) {
            App_Log(ERROR,"%s: Dimensions out of bound of grid A (NJ=%d)\n",__func__,A->NJ);
            return(NULL);
         }
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
         if (N<0 || N>A->NK-1) {
            App_Log(ERROR,"%s: Dimensions out of bound of grid A (NK=%d)\n",__func__,A->NK);
            return(NULL);
         }
         k=N;
         n=0;
         GData[++GDataN]=Def_New(A->NI,A->NJ,1,A->NC,(GType?GType:A->Type));
//         GData[GDataN]->GridLevels[0]=A->GridLevels[k];
         while(A->Data[n]) {
            idx=FSIZE2D(A)*k;
            for(j=0;j<A->NJ;j++) {
               for(i=0;i<A->NI;i++) {
                  Def_Get(A,n,idx,v);
                  Def_Set(GData[GDataN],n,j*GData[GDataN]->NI+i,v);
                  idx++;
               }
            }
//            Def_Pointer(A,n,FSIZE2D(A)*k,p);
//            memcpy(GData[GDataN]->Data[n],p,FSIZE2D(A)*TDef_Size[A->Type]);
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

   if (I0<0 || I1>A->NI-1 || J0<0 || J1>A->NJ-1 || K0<0 || K1>A->NK-1) {
      App_Log(ERROR,"%s: Dimensions out of bound of grid A (%d x %d x %d)\n",__func__,A->NI,A->NJ,A->NK);
      return(NULL);
   }
   
   if (FSIZE3D(A)==1) {
      if (!Calc_Compat(GInterp,A,B,2,1))
         return(NULL);
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
            App_Log(ERROR,"%s: Grid B does not have a component %d\n",__func__,n);
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
   fprintf(stdout,"(DEBUG) Calc_Length(A:%p)\n",(void*)A);
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
   fprintf(stdout,"(DEBUG) Calc_Dir(A:%p)\n",(void*)A);
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
TDef* Calc_Matrix(TFunc *Func,int Iterate,int Matrix,TDef_Type Type,int NFlds,TDef *Flds[]) {
   int i;

#ifdef DEBUG
   fprintf(stdout,"(DEBUG) Called Calc_Matrix(Func:%p,Iterate:%d,Matrix:%d,Type:%d,NFlds:%d",Func,Iterate,Matrix,Type,NFlds);
   // Get the list of fields
   for(i=0; i<NFlds; ++i) {
      fprintf(stdout,",%c:%p",'A'+i,Flds[i]);
   }
   fprintf(stdout,")\n");
#endif

   if( Iterate ) {
      TDef *m,*t;
      if( NFlds <= 0 )
         return(NULL);
      // Select the field that will be used for master (m) and type (t)
      m=t=Flds[0];
      for(i=1; i<NFlds; ++i) {
         m=Calc_Compat(GInterp,m,Flds[i],3,(Func!=(TFunc*)ifelse)?1:0);
         t=DEFSELECTTYPE(t,Flds[i]);
      }
      if( !t || !m )
         return(NULL);

      // Make the new field
      ++GDataN;
      GData[GDataN]=Def_CopyPromote(m,(GType?GType:(Type?Type:DEFSIGNEDTYPE(t))));
      Calc_Iterate(GData[GDataN],Func,NFlds,Flds);
   } else if( Matrix ) {
      TDef *flds[NFlds+1];
      if( NFlds <= 0 )
         return(NULL);
      ++GDataN;
      GData[GDataN]=Def_CopyPromote(Flds[0],(GType?GType:Flds[0]->Type));
      flds[0]=GData[GDataN];
      memcpy(flds+1,Flds,NFlds*sizeof(*Flds));
      Calc_CallFunc(Func,NFlds+1,flds);
   } else {
      ++GDataN;
      GData[GDataN]=Def_New(1,1,1,1,TD_Float64);
      Def_Set(GData[GDataN],0,0,Calc_CallFunc(Func,NFlds,Flds));
   }

   return(Calc_InError() ? NULL : GData[GDataN]);
}

TDef* Calc_Matrixv(TFunc *Func,int Iterate,int Matrix,TDef_Type Type,int NFlds,...) {
   TDef *flds[NFlds];
   va_list v;
   int i;

   va_start(v,NFlds);
   for(i=0; i<NFlds; ++i) {
      flds[i]=va_arg(v,TDef*);
   }
   va_end(v);

   return Calc_Matrix(Func,Iterate,Matrix,Type,NFlds,flds);
}
