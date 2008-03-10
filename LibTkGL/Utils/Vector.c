/*=============================================================================
* Environnement Canada
* Centre Meteorologique Canadian
* 2100 Trans-Canadienne
* Dorval, Quebec
*
* Projet    : Projection diverses de la carte vectorielle.
* Fichier   : Vector.c
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

#include <math.h>
#include "Vector.h"

/*----------------------------------------------------------------------------
 * Nom      : <Vect_Normalize>
 * Creation : Janvier 2000 - J.P. Gauthier - CMC/CMOE
 *
 * But      : Normaliser un vecteur
 *
 * Parametres :
 *  <V>       : Vecteur
 *
 * Retour     :
 *
 * Remarques :
 *
 * Modifications :
 *
 *    Nom         :
 *    Date        :
 *    Description :
 *----------------------------------------------------------------------------
*/

void Vect_Normalize(Vect3d V){

   double norm;

   norm=1.0/sqrt(V[0]*V[0]+V[1]*V[1]+V[2]*V[2]);

   V[0]*=norm;
   V[1]*=norm;
   V[2]*=norm;
}

void Vect3f_Normalize(Vect3f V){

   double norm;

   norm=1.0/sqrt(V[0]*V[0]+V[1]*V[1]+V[2]*V[2]);

   V[0]*=norm;
   V[1]*=norm;
   V[2]*=norm;
}

/*----------------------------------------------------------------------------
 * Nom      : <Vect_CrossProduct>
 * Creation : Janvier 2000 - J.P. Gauthier - CMC/CMOE
 *
 * But      : Produit croise entre vecter
 *
 * Parametres :
 *  <V1>      : Vecteur resultat
 *  <V2>      : Vecteur 1
 *  <V3>      : Vecteur 2
 *
 * Retour     :
 *
 * Remarques :
 *
 * Modifications :
 *
 *    Nom         :
 *    Date        :
 *    Description :
 *----------------------------------------------------------------------------
*/
void Vect_CrossProduct(Vect3d V1,Vect3d V2,Vect3d V3){

   V1[0]=V2[1]*V3[2]-V2[2]*V3[1];
   V1[1]=V2[2]*V3[0]-V2[0]*V3[2];
   V1[2]=V2[0]*V3[1]-V2[1]*V3[0];
}

void Vect3f_CrossProduct(Vect3f V1,Vect3f V2,Vect3f V3){

   V1[0]=V2[1]*V3[2]-V2[2]*V3[1];
   V1[1]=V2[2]*V3[0]-V2[0]*V3[2];
   V1[2]=V2[0]*V3[1]-V2[1]*V3[0];
}

/*----------------------------------------------------------------------------
 * Nom      : <Vect_Normal>
 * Creation : Mai 2000 - J.P. Gauthier - CMC/CMOE
 *
 * But      : Calculer une normale a un plan
 *
 * Parametres :
 *  <V1>      : Vecteur resultat normaliser
 *  <V2>      : Vecteur 1
 *  <V3>      : Vecteur 2
 *
 * Retour     :
 *
 * Remarques :
 *     - On multiplie deux vecteur et on normalize le resultat
 *
 * Modifications :
 *
 *    Nom         :
 *    Date        :
 *    Description :
 *----------------------------------------------------------------------------
*/
void Vect_Normal(Vect3d V1,Vect3d V2,Vect3d V3){

   double norm;

   V1[0]=V2[1]*V3[2]-V2[2]*V3[1];
   V1[1]=V2[2]*V3[0]-V2[0]*V3[2];
   V1[2]=V2[0]*V3[1]-V2[1]*V3[0];

   norm=1.0/sqrt(V1[0]*V1[0]+V1[1]*V1[1]+V1[2]*V1[2]);

   V1[0]*=norm;
   V1[1]*=norm;
   V1[2]*=norm;
}

/*----------------------------------------------------------------------------
 * Nom      : <Vect_InterPlane>
 * Creation : Janvier 2000 - J.P. Gauthier - CMC/CMOE
 *
 * But      : Intersection d'un vecteur avec un plan.
 *
 * Parametres :
 *  <Dir>     : Vecteur de directionnel
 *  <Pix>     : Localisation du pixel
 *  <R>       : Distance du plan
 *
 * Retour     :
 *  <Vis>     : Validite de l'intersection
 *
 * Remarques :
 *
 * Modifications :
 *
 *    Nom         :
 *    Date        :
 *    Description :
 *----------------------------------------------------------------------------
*/
int Vect_InterPlane(Vect3d Dir,Vect3d Pix,double R){

   double t;

   if (Dir[2]!=0) {
      t=(Pix[2]-R)/Dir[2];

      Pix[0]-=Dir[0]*t;
      Pix[1]-=Dir[1]*t;
    /*Pix[2]-=Dir[2]*t;*/

      return 1;
   } else {
      Pix[0]=999.0;
      Pix[1]=999.0;
    /*Pix[2]=999.0;*/

      return 0;
   }
}


/*----------------------------------------------------------------------------
 * Nom      : <Vect_InterSphere>
 * Creation : Janvier 2000 - J.P. Gauthier - CMC/CMOE
 *
 * But      : Intersection d'un vecteur avec une sphere.
 *
 * Parametres :
 *  <Dir>     : Vecteur de directionnel
 *  <A>       : Facteur constant de la quadratique x2
 *  <Pix>     : Localisation du pixel
 *  <R>       : Rayon de la sphere
 *
 * Retour     :
 *  <Vis>     : Validite de l'intersection
 *
 * Remarques :
 *
 * Modifications :
 *
 *    Nom         :
 *    Date        :
 *    Description :
 *----------------------------------------------------------------------------
*/
int Vect_InterSphere(Vect3d Dir,double A,Vect3d Pix,double R){

   double b,c,t,delta;

   b=2*(Dir[0]*Pix[0]+Dir[1]*Pix[1]+Dir[2]*Pix[2]);
   c=Pix[0]*Pix[0]+Pix[1]*Pix[1]+Pix[2]*Pix[2]-R;

   delta=b*b-2*A*c;

   if (delta>0) {
      t=(b+sqrt(delta))/A;

      Pix[0]-=Dir[0]*t;
      Pix[1]-=Dir[1]*t;
    /*Pix[2]-=Dir[2]*t;*/

      return 1;
   } else {
      Pix[0]=999.0;
      Pix[1]=999.0;
    /*Pix[2]=999.0;*/

      return 0;
   }
}
