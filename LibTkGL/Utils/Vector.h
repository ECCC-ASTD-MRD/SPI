/*=============================================================================
* Environnement Canada
* Centre Meteorologique Canadian
* 2100 Trans-Canadienne
* Dorval, Quebec
*
* Projet    : Projection diverses de la carte vectorielle.
* Fichier   : Vector.h
* Creation  : Janvier 2000 - J.P. Gauthier
*
* Description: Fichier de definition des parametres et fonctions de vecteurs.
*
* Remarques :
*
* Modification:
*
*   Nom         :
*   Date        :
*   Description :
*
*==============================================================================
*/

#ifndef _Vector_h
#define _Vector_h

#define VMAX(A,B)                 (A<B?B:A)
#define VMID(A,B)                 (A-B)/2.0
#define VMIN(A,B)                 (A<B?A:B)

#define Vect_Add(V1,V2,V3)       V1[0]=V2[0]+V3[0];V1[1]=V2[1]+V3[1];V1[2]=V2[2]+V3[2]
#define Vect_Assign(V1,V2)       V1[0]=V2[0];V1[1]=V2[1];V1[2]=V2[2]
#define Vect_Clear(V)            V[0]=0.0;V[1]=0.0;V[2]=0.0
#define Vect_DotProduct(V1,V2)   (V1[0]*V2[0]+V1[1]*V2[1]+V1[2]*V2[2])
#define Vect_Init(V,I,J,K)       V[0]=I;V[1]=J;V[2]=K
#define Vect_Max(V1,V2,V3)       V1[0]=VMAX(V2[0],V3[0]);V1[1]=VMAX(V2[1],V3[1]);V1[2]=VMAX(V2[2],V3[2])
#define Vect_Mid(V1,V2,V3)       V1[0]=V2[0]+VMID(V3[0],V2[0]);V1[1]=V2[1]+VMID(V3[1],V2[1]);V1[2]=V2[2]+VMID(V3[2],V2[2])
#define Vect_Min(V1,V2,V3)       V1[0]=VMIN(V2[0],V3[0]);V1[1]=VMIN(V2[1],V3[1]);V1[2]=VMIN(V2[2],V3[2])
#define Vect_Norm(V)             (sqrt(V[0]*V[0]+V[1]*V[1]+V[2]*V[2]))
#define Vect_Null(V)             (V[0]==0.0 && V[1]==0.0 && V[2]==0.0)
#define Vect_Substract(V1,V2,V3) V1[0]=V2[0]-V3[0];V1[1]=V2[1]-V3[1];V1[2]=V2[2]-V3[2]
#define Vect_Weight(V1,V2)       VMAX(VMAX(fabs(V2[0]-V1[0]),fabs(V2[1]-V1[1])),fabs(V2[2]-V1[2]))
#define Vect_Equal(V1,V2)        (V1[0]==V2[0] && V1[1]==V2[1] && V1[2]==V2[2])
#define Vect_Mul(V1,V2,V3)       V1[0]=V2[0]*V3[0];V1[1]=V2[1]*V3[1];V1[2]=V2[2]*V3[2]
/*
#define Vect_Interp(V,V1,V2,R)   Vect_Substract(V,V2,V1);V[0]=V[0]<-2.0?V[0]+4.0:(V[0]>2.0?V[0]-4.0:V[0]);Vect_SMul(V,V,R);Vect_Add(V,V1,V)
*/
#define Vect_Interp(V,V1,V2,R)   Vect_Substract(V,V2,V1);Vect_SMul(V,V,R);Vect_Add(V,V1,V)
#define Vect_InterpC(V,V1,V2,R)  Vect_Substract(V,V2,V1);V[0]=V[0]>2?-(4-V[0]):(V[0]<-2?4+V[0]:V[0]);Vect_SMul(V,V,R);Vect_Add(V,V1,V)

#define Vect_SAdd(V1,V2,SC)      V1[0]=V2[0]+SC;V1[1]=V2[1]+SC;V1[2]=V2[2]+SC
#define Vect_SSubstrac(V1,V2,SC) V1[0]=V2[0]-SC;V1[1]=V2[1]-SC;V1[2]=V2[2]-SC
#define Vect_SMul(V1,V2,SC)      V1[0]=V2[0]*SC;V1[1]=V2[1]*SC;V1[2]=V2[2]*SC
#define Vect_SDiv(V1,V2,SC)      V1[0]=V2[0]/SC;V1[1]=V2[1]/SC;V1[2]=V2[2]/SC

typedef double Vect3d[3];
typedef float  Vect4f[4];
typedef float  Vect3f[3];
typedef float  Vect2f[2];

void Vect3f_CrossProduct(Vect3f V1,Vect3f V2,Vect3f V3);
void Vect3f_Normalize(Vect3f V);

void Vect_Normalize(Vect3d V);
void Vect_Normal(Vect3d V1,Vect3d V2,Vect3d V3);
void Vect_CrossProduct(Vect3d V1,Vect3d V2,Vect3d V3);
int  Vect_InterPlane(Vect3d Dir,Vect3d Pix,double R);
int  Vect_InterSphere(Vect3d Dir,double A,Vect3d Pix,double R);

#endif
