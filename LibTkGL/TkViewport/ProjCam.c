/*=============================================================================
 * Environnement Canada
 * Centre Meteorologique Canadian
 * 2100 Trans-Canadienne
 * Dorval, Quebec
 *
 * Projet    : Projection diverses de la carte vectorielle.
 * Fichier   : ProjCam.c
 * Creation  : Janvier 2000 - J.P. Gauthier
 *
 * Description: Fichier de definition des parametres des cameras.
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

#include "ProjCam.h"
#include "Projection.h"

static Tcl_HashTable ProjCamTable;
static int           ProjCamInit=0;

static int  ProjCam_Cmd(ClientData,Tcl_Interp *,int Objc,Tcl_Obj *CONST Objv[]);
static int  ProjCam_Config(Tcl_Interp *Interp,char *Name,int Objc,Tcl_Obj *CONST Objv[]);
static int  ProjCam_Define(Tcl_Interp *Interp,char *Name,int Objc,Tcl_Obj *CONST Objv[]);
static int  ProjCam_Stats(Tcl_Interp *Interp,char *Name,int Objc,Tcl_Obj *CONST Objv[]);
static int  ProjCam_Create(Tcl_Interp *Interp,char *Name);
static int  ProjCam_Destroy(Tcl_Interp *Interp,char *Name);
static int  ProjCam_Copy(Tcl_Interp *Interp,ProjCam *Cam,char *Name);

int  ProjCam_Path(Tcl_Interp *Interp,ProjCam *Cam,Tcl_Obj *List);
void ProjCam_Project(ProjCam *Cam,Projection *Proj);

/*----------------------------------------------------------------------------
 * Nom      : <ProjCam_Get>
 * Creation : Janvier 2000 - J.P. Gauthier - CMC/CMOE
 *
 * But      : Cette procedure retourne l'adresse de la camera.
 *
 * Parametres  :
 *  <Name>     : Nom de la camera
 *
 * Retour           :
 *  <ProjCam> :  Pointeur sur la structure de la camera
 *
 * Remarques :
 *
 *----------------------------------------------------------------------------
*/
ProjCam* ProjCam_Get(char* Name){
   return((ProjCam*)TclY_HashGet(&ProjCamTable,Name));
}

/*----------------------------------------------------------------------------
 * Nom      : <ProjCam_Cmd>
 * Creation : Janvier 2000 - J.P. Gauthier  - CMC/CMOE
 *
 * But      : Procedure invoquee pour gerer les cameras.
 *
 * Parametres    :
 *  <clientData> : Nom de la projection
 *  <Interp>     : Interpreteur TCL
 *  <Objc>       : Nombre d'arguments
 *  <Objv>       : Pointeur sur la liste des arguments
 *
 * Retour:
 *  <TCL_...>    : Code de retour standard TCL
 *
 * Remarques :
 *
 *----------------------------------------------------------------------------
*/
static int ProjCam_Cmd(ClientData clientData,Tcl_Interp *Interp,int Objc,Tcl_Obj *CONST Objv[]){

   int                idx;
   static CONST char *sopt[] = { "create","configure","define","stats","copy","destroy","is",NULL };
   enum                opt { CREATE,CONFIGURE,DEFINE,STATS,COPY,DESTROY,IS };

   Tcl_ResetResult(Interp);


   if (Objc<2) {
      Tcl_WrongNumArgs(Interp,1,Objv,"command ?arg arg ...?");
      return(TCL_ERROR);
   }

   if (Tcl_GetIndexFromObj(Interp,Objv[1],sopt,"command",0,&idx)!=TCL_OK) {
      return(TCL_ERROR);
   }

   switch ((enum opt)idx) {
      case CREATE:
         if (Objc!=3) {
            Tcl_WrongNumArgs(Interp,2,Objv,"name");
            return(TCL_ERROR);
         }
         return(ProjCam_Create(Interp,Tcl_GetString(Objv[2])));
         break;

      case CONFIGURE:
         if (Objc<3) {
            Tcl_WrongNumArgs(Interp,2,Objv,"name ?option?");
            return(TCL_ERROR);
         }
         return(ProjCam_Config(Interp,Tcl_GetString(Objv[2]),Objc-3,Objv+3));
         break;

      case DEFINE:
         if (Objc<3) {
            Tcl_WrongNumArgs(Interp,2,Objv,"name ?option?");
            return(TCL_ERROR);
         }
         return(ProjCam_Define(Interp,Tcl_GetString(Objv[2]),Objc-3,Objv+3));
         break;

      case STATS:
         if (Objc<3) {
            Tcl_WrongNumArgs(Interp,2,Objv,"name ?option?");
            return(TCL_ERROR);
         }
         return(ProjCam_Stats(Interp,Tcl_GetString(Objv[2]),Objc-3,Objv+3));
         break;

      case COPY:
         if(Objc!=4) {
            Tcl_WrongNumArgs(Interp,2,Objv,"camto camfrom");
            return(TCL_ERROR);
         }
         if (!ProjCam_Copy(Interp,ProjCam_Get(Tcl_GetString(Objv[3])),Tcl_GetString(Objv[2]))) {
            return(TCL_ERROR);
         } else {
            return(TCL_OK);
         }
         break;

      case DESTROY:
         if (Objc!=3) {
            Tcl_WrongNumArgs(Interp,2,Objv,"name");
            return(TCL_ERROR);
         }
         return(ProjCam_Destroy(Interp,Tcl_GetString(Objv[2])));
         break;

      case IS:
         if (Objc!=3) {
            Tcl_WrongNumArgs(Interp,2,Objv,"name");
            return(TCL_ERROR);
         }
         if (ProjCam_Get(Tcl_GetString(Objv[2]))) {
            Tcl_SetObjResult(Interp,Tcl_NewBooleanObj(1));
         } else {
            Tcl_SetObjResult(Interp,Tcl_NewBooleanObj(0));
         }
         break;
   }

   return TCL_OK;
}

/*----------------------------------------------------------------------------
 * Nom      : <ProjCam_Config>
 * Creation : Aout 1999 - J.P. Gauthier - CMC/CMOE
 *
 * But      : Configuration des parametres standards des diverses projections.
 *
 * Parametres :
 *  <Interp>  : Interpreteur Tcl
 *  <Name>    : Nom de la projection
 *  <Objc>    : Nombre d'arguments
 *  <Objv>    : Liste des arguments
 *
 * Retour     :
 *  <TCL_...> : Code de retour standard TCL
 *
 * Remarques :
 *
 *----------------------------------------------------------------------------
*/
static int ProjCam_Config(Tcl_Interp *Interp,char *Name,int Objc,Tcl_Obj *CONST Objv[]){

   Tcl_Obj *obj;
   ProjCam *cam=NULL;
   int      idx,i,n;

   static CONST char *sopt[] = { "-to","-from","-up","-lens","-show",NULL };
   enum                opt { TO,FROM,UP,LENS,SHOW };

   cam=ProjCam_Get(Name);
   if (!cam) {
      Tcl_AppendResult(Interp,"ProjCam_Config: Invalid camera definition",(char*)NULL);
      return(TCL_ERROR);
   }

   for(i=0;i<Objc;i++) {
      if (Tcl_GetIndexFromObj(Interp,Objv[i],sopt,"option",0,&idx)!=TCL_OK) {
         return(TCL_ERROR);
      }

      switch ((enum opt)idx) {

         case TO:
            if (Objc==1) {
               obj=Tcl_NewListObj(0,NULL);
               Tcl_ListObjAppendElement(Interp,obj,Tcl_NewDoubleObj(cam->To[0]));
               Tcl_ListObjAppendElement(Interp,obj,Tcl_NewDoubleObj(cam->To[1]));
               Tcl_ListObjAppendElement(Interp,obj,Tcl_NewDoubleObj(cam->To[2]));
               Tcl_SetObjResult(Interp,obj);
             } else {
               Tcl_ListObjLength(Interp,Objv[++i],&n);
               if (n!=3) {
                  Tcl_AppendResult(Interp,"ProjCam_Config: Invalid number of component",(char*)NULL);
                  return(TCL_ERROR);
               }
               Tcl_ListObjIndex(Interp,Objv[i],0,&obj);
               Tcl_GetDoubleFromObj(Interp,obj,&cam->To[0]);
               Tcl_ListObjIndex(Interp,Objv[i],1,&obj);
               Tcl_GetDoubleFromObj(Interp,obj,&cam->To[1]);
               Tcl_ListObjIndex(Interp,Objv[i],2,&obj);
               Tcl_GetDoubleFromObj(Interp,obj,&cam->To[2]);
            }
            break;

         case FROM:
            if (Objc==1) {
               obj=Tcl_NewListObj(0,NULL);
               Tcl_ListObjAppendElement(Interp,obj,Tcl_NewDoubleObj(cam->From[0]));
               Tcl_ListObjAppendElement(Interp,obj,Tcl_NewDoubleObj(cam->From[1]));
               Tcl_ListObjAppendElement(Interp,obj,Tcl_NewDoubleObj(cam->From[2]));
               Tcl_SetObjResult(Interp,obj);
            } else {
               Tcl_ListObjLength(Interp,Objv[++i],&n);
               if (n!=3) {
                  Tcl_AppendResult(Interp,"ProjCam_Config: Invalid number of component",(char*)NULL);
                  return(TCL_ERROR);
               }
               Tcl_ListObjIndex(Interp,Objv[i],0,&obj);
               Tcl_GetDoubleFromObj(Interp,obj,&cam->From[0]);
               Tcl_ListObjIndex(Interp,Objv[i],1,&obj);
               Tcl_GetDoubleFromObj(Interp,obj,&cam->From[1]);
               Tcl_ListObjIndex(Interp,Objv[i],2,&obj);
               Tcl_GetDoubleFromObj(Interp,obj,&cam->From[2]);
            }
            break;

         case UP:
            if (Objc==1) {
               obj=Tcl_NewListObj(0,NULL);
               Tcl_ListObjAppendElement(Interp,obj,Tcl_NewDoubleObj(cam->Up[0]));
               Tcl_ListObjAppendElement(Interp,obj,Tcl_NewDoubleObj(cam->Up[1]));
               Tcl_ListObjAppendElement(Interp,obj,Tcl_NewDoubleObj(cam->Up[2]));
               Tcl_SetObjResult(Interp,obj);
            } else {
               Tcl_ListObjLength(Interp,Objv[++i],&n);
               if (n!=3) {
                  Tcl_AppendResult(Interp,"ProjCam_Config: Invalid number of component",(char*)NULL);
                  return(TCL_ERROR);
               }
               Tcl_ListObjIndex(Interp,Objv[i],0,&obj);
               Tcl_GetDoubleFromObj(Interp,obj,&cam->Up[0]);
               Tcl_ListObjIndex(Interp,Objv[i],1,&obj);
               Tcl_GetDoubleFromObj(Interp,obj,&cam->Up[1]);
               Tcl_ListObjIndex(Interp,Objv[i],2,&obj);
               Tcl_GetDoubleFromObj(Interp,obj,&cam->Up[2]);
            }
            break;

         case LENS:
            if (Objc==1) {
               Tcl_SetObjResult(Interp,Tcl_NewDoubleObj(cam->Lens));
            } else {
               Tcl_GetDoubleFromObj(Interp,Objv[++i],&cam->Lens);
               cam->Lens=cam->Lens<=0?1.0:cam->Lens;
            }
            break;

         case SHOW:
            if (Objc==1) {
               Tcl_SetObjResult(Interp,Tcl_NewBooleanObj(cam->Show));
            } else {
               Tcl_GetBooleanFromObj(Interp,Objv[++i],&cam->Show);
            }
            break;
      }
   }

   ProjCam_ParamsInit(cam);

   return(TCL_OK);
}

/*----------------------------------------------------------------------------
 * Nom      : <ProjCam_Define>
 * Creation : Mars 2000 - J.P. Gauthier - CMC/CMOE
 *
 * But      : Configuration des definitions.
 *
 * Parametres :
 *  <Interp>  : Interpreteur Tcl
 *  <Name>    : Nom de la projection
 *  <Objc>    : Nombre d'arguments
 *  <Objv>    : Liste des arguments
 *
 * Retour     :
 *  <TCL_...> : Code de retour standard TCL
 *
 * Remarques :
 *
 *----------------------------------------------------------------------------
*/
static int ProjCam_Define(Tcl_Interp *Interp,char *Name,int Objc,Tcl_Obj *CONST Objv[]){

   Tcl_Obj           *obj;
   Vect3d             val;
   ProjCam           *cam=NULL;
   int                idx,i;
   static CONST char *sopt[] = { "-fly","-path","-circlefrom","-circleto",NULL };
   enum                opt { FLY,PATH,CIRCLEFROM,CIRCLETO };

   cam=ProjCam_Get(Name);

   if (!cam) {
      Tcl_AppendResult(Interp,"ProjCam_Define: invalid camera definition",(char *)NULL);
      return TCL_ERROR;
   }

   for(i=0;i<Objc;i++) {

      if (Tcl_GetIndexFromObj(Interp,Objv[i],sopt,"option",0,&idx)!=TCL_OK) {
         return TCL_ERROR;
      }

      switch ((enum opt)idx) {
         case PATH:
            return(ProjCam_Path(Interp,cam,Objv[++i]));
            break;

         case FLY:
            Tcl_GetDoubleFromObj(Interp,Objv[++i],&cam->Frame);
            ProjCam_Fly(cam);
            break;

         case CIRCLEFROM:
            Tcl_GetDoubleFromObj(Interp,Objv[++i],&val[0]);
            Tcl_GetDoubleFromObj(Interp,Objv[++i],&val[1]);
            Tcl_GetDoubleFromObj(Interp,Objv[++i],&val[2]);
            ProjCam_CircleFrom(cam,DEG2RAD(val[0]),DEG2RAD(val[1]),val[2]);
            obj=Tcl_NewListObj(0,NULL);
            Tcl_ListObjAppendElement(Interp,obj,Tcl_NewDoubleObj(cam->From[0]));
            Tcl_ListObjAppendElement(Interp,obj,Tcl_NewDoubleObj(cam->From[1]));
            Tcl_ListObjAppendElement(Interp,obj,Tcl_NewDoubleObj(cam->From[2]));
            Tcl_SetObjResult(Interp,obj);
            break;

         case CIRCLETO:
            Tcl_GetDoubleFromObj(Interp,Objv[++i],&val[0]);
            Tcl_GetDoubleFromObj(Interp,Objv[++i],&val[1]);
            Tcl_GetDoubleFromObj(Interp,Objv[++i],&val[2]);
            ProjCam_CircleTo(cam,DEG2RAD(val[0]),DEG2RAD(val[1]),val[2]);
            obj=Tcl_NewListObj(0,NULL);
            Tcl_ListObjAppendElement(Interp,obj,Tcl_NewDoubleObj(cam->To[0]));
            Tcl_ListObjAppendElement(Interp,obj,Tcl_NewDoubleObj(cam->To[1]));
            Tcl_ListObjAppendElement(Interp,obj,Tcl_NewDoubleObj(cam->To[2]));
            Tcl_SetObjResult(Interp,obj);
            break;
      }
   }
   ProjCam_ParamsInit(cam);
   return(TCL_OK);
}

/*----------------------------------------------------------------------------
 * Nom      : <ProjCam_Define>
 * Creation : Mars 2000 - J.P. Gauthier - CMC/CMOE
 *
 * But      : Configuration des definitions.
 *
 * Parametres :
 *  <Interp>  : Interpreteur Tcl
 *  <Name>    : Nom de la projection
 *  <Objc>    : Nombre d'arguments
 *  <Objv>    : Liste des arguments
 *
 * Retour     :
 *  <TCL_...> : Code de retour standard TCL
 *
 * Remarques :
 *
 *----------------------------------------------------------------------------
*/
static int ProjCam_Stats(Tcl_Interp *Interp,char *Name,int Objc,Tcl_Obj *CONST Objv[]){

   ProjCam     *cam=NULL;
   int                idx,i;
   static CONST char *sopt[] = { "-dist","-aspect","-angle",NULL };
   enum                opt { DIST,ASPECT,ANGLE };

   cam=ProjCam_Get(Name);

   if (!cam) {
      Tcl_AppendResult(Interp,"ProjCam_Define: invalid camera definition",(char *)NULL);
      return TCL_ERROR;
   }

   for(i=0;i<Objc;i++) {

      if (Tcl_GetIndexFromObj(Interp,Objv[i],sopt,"option",0,&idx)!=TCL_OK) {
         return TCL_ERROR;
      }

      switch ((enum opt)idx) {
         case DIST:
            Tcl_SetObjResult(Interp,Tcl_NewDoubleObj(cam->Dist*EARTHRADIUS));
            return(TCL_OK);
            break;

         case ASPECT:
            Tcl_SetObjResult(Interp,Tcl_NewDoubleObj(cam->Aspect*EARTHRADIUS));
            return(TCL_OK);
            break;

         case ANGLE:
            Tcl_SetObjResult(Interp,Tcl_NewDoubleObj(RAD2DEG(atan2(fabs(cam->Up[2]),fabs(cam->Up[1])))));
            return(TCL_OK);
            break;
      }
   }
   return TCL_OK;
}

/*----------------------------------------------------------------------------
 * Nom      : <ProjCam_Create>
 * Creation : Janvier 2000 - J.P. Gauthier - CMC/CMOE
 *
 * But      : Procedure invoquee pour creer les cameras.
 *
 * Parametres    :
 *  <Interp>     : Interpreteur TCL
  *  <Name>       : Nom de la camera a creer
*
 * Retour:
 *  <TCL_...>    : Code de retour standard TCL
 *
 * Remarques :
 *
 *----------------------------------------------------------------------------
*/
static int ProjCam_Create(Tcl_Interp *Interp,char *Name){

   ProjCam *cam;

   if (!(cam=(ProjCam*)TclY_HashPut(Interp,&ProjCamTable,Name,sizeof(ProjCam)))) {
      return(TCL_ERROR);
   }

   /*Initialise les parametres de la camera*/

   Vect_Init(cam->From    ,0.0,0.0,2.0);
   Vect_Init(cam->To      ,0.0,0.0,1.0);
   Vect_Init(cam->Up      ,0.0,1.0,0.0);
   cam->Lens         = 1.0;
   cam->NbC          = 0;
   cam->Frame        = 0;
   cam->Show         = 0;
   cam->Update       = 0;
   cam->Pix          = 0.0;
   cam->Controls     = NULL;

   ProjCam_ParamsInit(cam);

   return(TCL_OK);
}

/*----------------------------------------------------------------------------
 * Nom      : <ProjCam_Copy>
 * Creation : Decembre 2009 - J.P. Gauthier - CMC/CMOE
 *
 * But      : Copier une camera.
 *
 * Parametres    :
 *  <Interp>     : Interpreteur TCL
 *  <Cam>        : Parametres de la camera
 *  <Name>       : Nom de la camera a creer
 *
 * Retour:
 *  <TCL_...>    : Code de retour standard TCL
 *
 * Remarques :
 *
 *----------------------------------------------------------------------------
*/
static int ProjCam_Copy(Tcl_Interp *Interp,ProjCam *Cam,char *Name){

   ProjCam *cam;

   if (!(cam=(ProjCam*)TclY_HashPut(Interp,&ProjCamTable,Name,sizeof(ProjCam)))) {
      return(TCL_ERROR);
   }

   /*Initialise les parametres de la camera*/
   memcpy(cam,Cam,sizeof(ProjCam));
   cam->Controls=NULL;

   return(1);
}

/*----------------------------------------------------------------------------
 * Nom      : <ProjCam_ParamsInit>
 * Creation : Janvier 2000 - J.P. Gauthier - CMC/CMOE
 *
 * But      : Initialisation des parametres de la localisation et de la
 *            disposition de la camera.
 *
 * Parametres :
 *  <Cam>     : Parametres de la camera
 *
 * Retour     :
 *
 * Remarques :
 *    -C'est ici que l'on construit la base othonormee de la camera
 *
 *----------------------------------------------------------------------------
*/
void ProjCam_ParamsInit(ProjCam *Cam) {

   double as;

   Vect_Substract(Cam->Basis,Cam->To,Cam->From);
   Cam->Dist=Vect_Norm(Cam->Basis);

   Vect_Normalize(Cam->Basis);
   Vect_Normalize(Cam->Up);

   Cam->A=2*Vect_DotProduct(Cam->Basis,Cam->Basis);

   as=Cam->Aspect;
   Cam->Aspect=Cam->Dist/Cam->Lens;
   Cam->Update=(as!=Cam->Aspect?1:Cam->Update);

   Cam->Clip=sqrt(Cam->Up[0]*Cam->Up[0]+Cam->Up[1]*Cam->Up[1])+Cam->Dist;
}

/*----------------------------------------------------------------------------
 * Nom      : <ProjCam_CircleFrom>
 * Creation : Mars 2000 - J.P. Gauthier - CMC/CMOE
 *
 * But      : Deplacer la camera autour de point To dans des plans de
 *            visualisations consistants.
 *
 * Parametres :
 *  <Cam>     : Parametres de la camera
 *  <ThetaXY> : Angle de rotation dans le plan XY de la camera
 *  <ThetaYZ> : Angle de rotation dans le plan YZ de la camera
 *  <Delta>   : Distance ou rayon de rotation
 *
 * Retour     :
 *
 * Remarques :
 *    -Pour calculer le vecteur Up, il faut creer un deuxieme vecteur de
 *     rotation et multiplier vectoriellement les deux.
 *
 *----------------------------------------------------------------------------
*/
void ProjCam_CircleFrom(ProjCam *Cam,double ThetaXY,double ThetaYZ,double Delta) {

   Vect3d tmp;
   double delta=0.0;

   /*Limiter les angles a 90 Degrees*/
   ThetaYZ=ThetaYZ>M_PI2?M_PI2:ThetaYZ;
   ThetaYZ=ThetaYZ<-M_PI2?-M_PI2:ThetaYZ;

   /*Fixer la distance du point To*/
   Delta=Delta<0?0:Delta;

   /*Rotation dans le plan YZ*/
   Cam->From[1]=Delta*sin(ThetaYZ);
   Cam->From[2]=Delta*cos(ThetaYZ);

   if (ThetaYZ==0) {
      Cam->Up[0]=2.0*sin(ThetaXY);
      Cam->Up[1]=2.0*cos(ThetaXY);

      Cam->From[0]=Cam->From[1]*sin(ThetaXY);
      Cam->From[1]=Cam->From[1]*cos(ThetaXY);
   } else {
      /*Rotation dans le plan XY*/
      delta=ThetaYZ<0.0?-0.1:0.1;
      tmp[0]=Cam->From[1]*sin(ThetaXY+delta);
      tmp[1]=Cam->From[1]*cos(ThetaXY+delta);
      tmp[2]=Cam->From[2];

      Cam->From[0]=Cam->From[1]*sin(ThetaXY);
      Cam->From[1]=Cam->From[1]*cos(ThetaXY);

      /*Determiner le vecteur haut Cam->Up*/
      Vect_CrossProduct(Cam->Up,Cam->From,tmp);
   }
   /*Localiser dans l'espace*/
   Vect_Add(Cam->From,Cam->From,Cam->To);
}

/*----------------------------------------------------------------------------
 * Nom      : <ProjCam_CircleTo>
 * Creation : Mars 2000 - J.P. Gauthier - CMC/CMOE
 *
 * But      : Deplacer la camera autour de point From dans des plans de
 *            visualisations consistants.
 *
 * Parametres :
 *  <Cam>     : Parametres de la camera
 *  <ThetaXY> : Angle de rotation dans le plan XY de la camera
 *  <ThetaYZ> : Angle de rotation dans le plan YZ de la camera
 *  <Delta>   : Distance ou rayon de rotation
 *
 * Retour     :
 *
 * Remarques :
 *    -Pour calculer le vecteur Up, il faut creer un deuxieme vecteur de
 *     rotation et multiplier vectoriellement les deux.
 *
 *----------------------------------------------------------------------------
*/
void ProjCam_CircleTo(ProjCam *Cam,double ThetaXZ,double ThetaYZ,double Delta) {

   Vect3d tmp;

   /*Rotation dans le plan XZ-YZ*/
   tmp[0]=Delta*cos(ThetaYZ)*sin(ThetaXZ);
   tmp[1]=Delta*sin(ThetaYZ);
   tmp[2]=Delta*cos(ThetaYZ)*cos(ThetaXZ);

   /*Determiner le vecteur haut Cam->Up*/
   Cam->Up[0]=cos(ThetaYZ+M_PI4)*sin(ThetaXZ);
   Cam->Up[1]=sin(ThetaYZ+M_PI4);
   Cam->Up[2]=cos(ThetaYZ+M_PI4)*cos(ThetaXZ);

   /*Localiser dans l'espace*/
   Vect_Substract(Cam->To,Cam->From,tmp);
}

/*----------------------------------------------------------------------------
 * Nom      : <ProjCam_Path>
 * Creation : Mars 2002 - J.P. Gauthier - CMC/CMOE
 *
 * But      : Creer un path entre une liste de point en interpolant cubiquement
 *            a projection.
 *
 * Parametres :
 *  <Interp>  : Interpreteur Tcl
 *  <Cam>     : Parametres de la Camera
 *  <Objc>    : Nombre de point de controle
 *  <Objv>    : Liste des points de controles (From,To,Up)(X,Y,Z)
 *
 * Retour     :
 *  <TCL_...> : Code de retour standard TCL
 *
 * Remarques :
 *
 *----------------------------------------------------------------------------
*/

int ProjCam_Path(Tcl_Interp *Interp,ProjCam *Cam,Tcl_Obj *List) {

   int      n,nc,i;
   Tcl_Obj *obj,*list,*param;

   Tcl_ListObjLength(Interp,List,&nc);

   /*Free previous control point list*/
   if (nc!=Cam->NbC) {
      if (Cam->Controls) {
         free(Cam->Controls);Cam->Controls=NULL;
         Cam->NbC=0;
      }
      /*Allocate new control point list*/
      if (nc) {
         Cam->Controls=(ProjCam*)malloc((nc+3)*sizeof(ProjCam));
         if (!Cam->Controls) {
            Tcl_AppendResult(Interp,"ProjCam_Control:: Could not allocate memory for control point list",(char*)NULL);
            return(TCL_ERROR);
         }
      }
      Cam->NbC=nc;
   }

   if (!nc) {
      return(TCL_OK);
   }

   /*Process current control point list*/
   for(n=1,i=0;i<Cam->NbC;n++,i++) {

      Tcl_ListObjIndex(Interp,List,i,&list);
      Tcl_ListObjLength(Interp,list,&nc);
      if (nc!=4) {
         Tcl_AppendResult(Interp,"ProjCam_Control:: Invalid number of values",(char*)NULL);
         return(TCL_ERROR);
      }

      Tcl_ListObjIndex(Interp,list,0,&param);
      Tcl_ListObjIndex(Interp,param,0,&obj);
      Tcl_GetDoubleFromObj(Interp,obj,&Cam->Controls[n].From[0]);
      Tcl_ListObjIndex(Interp,param,1,&obj);
      Tcl_GetDoubleFromObj(Interp,obj,&Cam->Controls[n].From[1]);
      Tcl_ListObjIndex(Interp,param,2,&obj);
      Tcl_GetDoubleFromObj(Interp,obj,&Cam->Controls[n].From[2]);

      Tcl_ListObjIndex(Interp,list,1,&param);
      Tcl_ListObjIndex(Interp,param,0,&obj);
      Tcl_GetDoubleFromObj(Interp,obj,&Cam->Controls[n].To[0]);
      Tcl_ListObjIndex(Interp,param,1,&obj);
      Tcl_GetDoubleFromObj(Interp,obj,&Cam->Controls[n].To[1]);
      Tcl_ListObjIndex(Interp,param,2,&obj);
      Tcl_GetDoubleFromObj(Interp,obj,&Cam->Controls[n].To[2]);

      Tcl_ListObjIndex(Interp,list,2,&param);
      Tcl_ListObjIndex(Interp,param,0,&obj);
      Tcl_GetDoubleFromObj(Interp,obj,&Cam->Controls[n].Up[0]);
      Tcl_ListObjIndex(Interp,param,1,&obj);
      Tcl_GetDoubleFromObj(Interp,obj,&Cam->Controls[n].Up[1]);
      Tcl_ListObjIndex(Interp,param,2,&obj);
      Tcl_GetDoubleFromObj(Interp,obj,&Cam->Controls[n].Up[2]);

      Tcl_ListObjIndex(Interp,list,3,&param);
      Tcl_GetDoubleFromObj(Interp,param,&Cam->Controls[n].Lens);
   }

   /*Copy control point for interpolation purposes*/
   if (Vect_Equal(Cam->Controls[1].From,Cam->Controls[n-1].From)) {     /*Path is closed*/
      Vect_Assign(Cam->Controls[0].From,Cam->Controls[n-2].From);
      Vect_Assign(Cam->Controls[n].From,Cam->Controls[2].From);
      Vect_Assign(Cam->Controls[n+1].From,Cam->Controls[3].From);
   } else {                                                 /*Path is open*/
      Vect_Assign(Cam->Controls[0].From,Cam->Controls[1].From);
      Vect_Assign(Cam->Controls[n].From,Cam->Controls[n-1].From);
      Vect_Assign(Cam->Controls[n+1].From,Cam->Controls[n-1].From);
   }

   if (Vect_Equal(Cam->Controls[1].To,Cam->Controls[n-1].To)) {     /*Path is closed*/
      Vect_Assign(Cam->Controls[0].To,Cam->Controls[n-2].To);
      Vect_Assign(Cam->Controls[n].To,Cam->Controls[2].To);
      Vect_Assign(Cam->Controls[n+1].To,Cam->Controls[3].To);
   } else {                                                 /*Path is open*/
      Vect_Assign(Cam->Controls[0].To,Cam->Controls[1].To);
      Vect_Assign(Cam->Controls[n].To,Cam->Controls[n-1].To);
      Vect_Assign(Cam->Controls[n+1].To,Cam->Controls[n-1].To);
   }

   if (Vect_Equal(Cam->Controls[1].Up,Cam->Controls[n-1].Up)) {     /*Path is closed*/
      Vect_Assign(Cam->Controls[0].Up,Cam->Controls[n-2].Up);
      Vect_Assign(Cam->Controls[n].Up,Cam->Controls[2].Up);
      Vect_Assign(Cam->Controls[n+1].Up,Cam->Controls[3].Up);
   } else {                                                 /*Path is open*/
      Vect_Assign(Cam->Controls[0].Up,Cam->Controls[1].Up);
      Vect_Assign(Cam->Controls[n].Up,Cam->Controls[n-1].Up);
      Vect_Assign(Cam->Controls[n+1].Up,Cam->Controls[n-1].Up);
   }

   if (Cam->Controls[1].Lens==Cam->Controls[n-1].Lens) {     /*Path is closed*/
      Cam->Controls[0].Lens=Cam->Controls[n-2].Lens;
      Cam->Controls[n].Lens=Cam->Controls[2].Lens;
      Cam->Controls[n+1].Lens=Cam->Controls[3].Lens;
   } else {                                                 /*Path is open*/
      Cam->Controls[0].Lens=Cam->Controls[1].Lens;
      Cam->Controls[n].Lens=Cam->Controls[n-1].Lens;
      Cam->Controls[n+1].Lens=Cam->Controls[n-1].Lens;
   }
   return(TCL_OK);
}

/*----------------------------------------------------------------------------
 * Nom      : <ProjCam_Fly>
 * Creation : Fevrier 2004 - J.P. Gauthier - CMC/CMOE
 *
 * But      : Interpoler la position de la camera selon le path.
 *
 * Parametres :
 *  <Cam>     : Parametres de la Camera
 *
 * Retour     :
 *
 * Remarques :
 *
 *----------------------------------------------------------------------------
*/
void ProjCam_Fly(ProjCam *Cam) {

   int     idx;
   double  d;

   if (!Cam->Controls) {
      return;
   }

   if (Cam->Frame<0 || Cam->Frame>=Cam->NbC) {
      return;
   }

   idx=floor(Cam->Frame);
   d=Cam->Frame-idx;
   idx++;

   Cam->From[0]=InterpHermite(Cam->Controls[idx-1].From[0],Cam->Controls[idx].From[0],Cam->Controls[idx+1].From[0],Cam->Controls[idx+2].From[0],d,0.0,0.0);
   Cam->From[1]=InterpHermite(Cam->Controls[idx-1].From[1],Cam->Controls[idx].From[1],Cam->Controls[idx+1].From[1],Cam->Controls[idx+2].From[1],d,0.0,0.0);
   Cam->From[2]=InterpHermite(Cam->Controls[idx-1].From[2],Cam->Controls[idx].From[2],Cam->Controls[idx+1].From[2],Cam->Controls[idx+2].From[2],d,0.0,0.0);

   Cam->To[0]=InterpHermite(Cam->Controls[idx-1].To[0],Cam->Controls[idx].To[0],Cam->Controls[idx+1].To[0],Cam->Controls[idx+2].To[0],d,0.0,0.0);
   Cam->To[1]=InterpHermite(Cam->Controls[idx-1].To[1],Cam->Controls[idx].To[1],Cam->Controls[idx+1].To[1],Cam->Controls[idx+2].To[1],d,0.0,0.0);
   Cam->To[2]=InterpHermite(Cam->Controls[idx-1].To[2],Cam->Controls[idx].To[2],Cam->Controls[idx+1].To[2],Cam->Controls[idx+2].To[2],d,0.0,0.0);

   Cam->Up[0]=InterpHermite(Cam->Controls[idx-1].Up[0],Cam->Controls[idx].Up[0],Cam->Controls[idx+1].Up[0],Cam->Controls[idx+2].Up[0],d,0.0,0.0);
   Cam->Up[1]=InterpHermite(Cam->Controls[idx-1].Up[1],Cam->Controls[idx].Up[1],Cam->Controls[idx+1].Up[1],Cam->Controls[idx+2].Up[1],d,0.0,0.0);
   Cam->Up[2]=InterpHermite(Cam->Controls[idx-1].Up[2],Cam->Controls[idx].Up[2],Cam->Controls[idx+1].Up[2],Cam->Controls[idx+2].Up[2],d,0.0,0.0);

   Cam->Lens=InterpHermite(Cam->Controls[idx-1].Lens,Cam->Controls[idx].Lens,Cam->Controls[idx+1].Lens,Cam->Controls[idx+2].Lens,d,0.0,0.0);
}

/*----------------------------------------------------------------------------
 * Nom      : <ProjCam_Place>
 * Creation : Fevrier 2004 - J.P. Gauthier - CMC/CMOE
 *
 * But      : Positionner la camera.
 *
 * Parametres :
 *  <Cam>     : Parametres de la Camera
 *
 * Retour     :
 *
 * Remarques :
 *
 *----------------------------------------------------------------------------
*/
void ProjCam_Place(ProjCam *Cam) {

   gluLookAt(Cam->From[0],Cam->From[1],Cam->From[2],Cam->To[0],Cam->To[1],Cam->To[2],Cam->Up[0],Cam->Up[1],Cam->Up[2]);
}

/*----------------------------------------------------------------------------
 * Nom      : <ProjCam_Project>
 * Creation : Fevrier 2004 - J.P. Gauthier - CMC/CMOE
 *
 * But      : Afficher le path et les orientations.
 *
 * Parametres :
 *  <Cam>     : Parametres de la Camera
 *
 * Retour     :
 *
 * Remarques :
 *
 *----------------------------------------------------------------------------
*/
void ProjCam_Project(ProjCam *Cam,Projection *Proj) {

   int    j;
   double d,p;
   Vect3d from,to,up;

   if (!Cam->Controls || !Cam->Show) {
      return;
   }

   glMatrixMode(GL_MODELVIEW);
   glPushMatrix();
   glLineWidth(2.0f);
   glColor3f(1.0f,0.0f,0.0f);

   /* Set To vector position */
   Proj->Type->Locate(Proj,Proj->Lat,Proj->Lon,1);

   /* Draw de Path */
   glBegin(GL_LINE_STRIP);

   j=1;
   d=0.01;
   while(j<Cam->NbC) {
      for(p=0;p<1.0;p+=d) {
         from[0]=InterpHermite(Cam->Controls[j-1].From[0],Cam->Controls[j].From[0],Cam->Controls[j+1].From[0],Cam->Controls[j+2].From[0],p,0.0,0.0);
         from[1]=InterpHermite(Cam->Controls[j-1].From[1],Cam->Controls[j].From[1],Cam->Controls[j+1].From[1],Cam->Controls[j+2].From[1],p,0.0,0.0);
         from[2]=InterpHermite(Cam->Controls[j-1].From[2],Cam->Controls[j].From[2],Cam->Controls[j+1].From[2],Cam->Controls[j+2].From[2],p,0.0,0.0);

         glVertex3dv(from);
      }
      j++;
   }

   glEnd();

   d=Cam->Aspect*0.01;

   /* Draw current position* an aspect*/
   if (Cam->Frame>0 && Cam->Frame<Cam->NbC) {
      j=floor(Cam->Frame);
      p=Cam->Frame-j;
      j++;

      from[0]=InterpHermite(Cam->Controls[j-1].From[0],Cam->Controls[j].From[0],Cam->Controls[j+1].From[0],Cam->Controls[j+2].From[0],p,0.0,0.0);
      from[1]=InterpHermite(Cam->Controls[j-1].From[1],Cam->Controls[j].From[1],Cam->Controls[j+1].From[1],Cam->Controls[j+2].From[1],p,0.0,0.0);
      from[2]=InterpHermite(Cam->Controls[j-1].From[2],Cam->Controls[j].From[2],Cam->Controls[j+1].From[2],Cam->Controls[j+2].From[2],p,0.0,0.0);
      to[0]=InterpHermite(Cam->Controls[j-1].To[0],Cam->Controls[j].To[0],Cam->Controls[j+1].To[0],Cam->Controls[j+2].To[0],p,0.0,0.0);
      to[1]=InterpHermite(Cam->Controls[j-1].To[1],Cam->Controls[j].To[1],Cam->Controls[j+1].To[1],Cam->Controls[j+2].To[1],p,0.0,0.0);
      to[2]=InterpHermite(Cam->Controls[j-1].To[2],Cam->Controls[j].To[2],Cam->Controls[j+1].To[2],Cam->Controls[j+2].To[2],p,0.0,0.0);
      up[0]=InterpHermite(Cam->Controls[j-1].Up[0],Cam->Controls[j].Up[0],Cam->Controls[j+1].Up[0],Cam->Controls[j+2].Up[0],p,0.0,0.0);
      up[1]=InterpHermite(Cam->Controls[j-1].Up[1],Cam->Controls[j].Up[1],Cam->Controls[j+1].Up[1],Cam->Controls[j+2].Up[1],p,0.0,0.0);
      up[2]=InterpHermite(Cam->Controls[j-1].Up[2],Cam->Controls[j].Up[2],Cam->Controls[j+1].Up[2],Cam->Controls[j+2].Up[2],p,0.0,0.0);

      ProjCam_Render(from,to,up,d*2);
   }

   /* Draw the position and aspect */
   j=0;
   while(j++<Cam->NbC) {
      ProjCam_Render(Cam->Controls[j].From,Cam->Controls[j].To,Cam->Controls[j].Up,d);
   }

   /* Draw the To position */
   glColor3f(0.0f,1.0f,0.0f);
   glTranslated(Cam->To[0],Cam->To[1],Cam->To[2]);
   gluSphere(GLRender->GLQuad,d,10,10);
   glPopMatrix();
}

/*----------------------------------------------------------------------------
 * Nom      : <ProjCam_Render>
 * Creation : Fevrier 2004 - J.P. Gauthier - CMC/CMOE
 *
 * But      : Afficher la position de la camera et son orientation
 *
 * Parametres :
 *  <From>    : Vecteur From
 *  <To>      : Vecteur To
 *  <Up>      : Vecteur Up
 *  <Size>    : Dimension
 *
 * Retour     :
 *
 * Remarques :
 *
 *----------------------------------------------------------------------------
*/
void ProjCam_Render(Vect3d From,Vect3d To,Vect3d Up,double Size) {

   Vect3d tmp;

   glMatrixMode(GL_MODELVIEW);
   glPushMatrix();
   glTranslated(From[0],From[1],From[2]);
   glColor3f(1.0f,0.0f,0.0f);
   gluSphere(GLRender->GLQuad,Size,10,10);

   glScalef(10*Size,10*Size,10*Size);
   glBegin(GL_LINES);

   Vect_Substract(tmp,To,From);
   Vect_Normalize(tmp);
   glColor3f(0.0f,1.0f,0.0f);
   glVertex3f(0.0f,0.0f,0.0f);
   glVertex3dv(tmp);

   glColor3f(0.0f,0.0f,1.0f);
   glVertex3f(0.0f,0.0f,0.0f);
   glVertex3dv(Up);

   glEnd();
   glPopMatrix();
}

/*----------------------------------------------------------------------------
 * Nom      : <ProjCam_Destroy>
 * Creation : Janvier 2000 - J.P. Gauthier - CMC/CMOE
 *
 * But      : This procedure is invoked by Projection_Cmd to destroy
 *            a projection.
 *
 * Parametres :
 *  <Interp>  : Interpreteur Tcl
 *  <Name>    : Nom de la projection
 *
 * Retour     :
 *  <TCL_...> : Code de retour standard TCL
 *
 * Remarques :
 *
 *----------------------------------------------------------------------------
*/
static int ProjCam_Destroy(Tcl_Interp *Interp, char *Name) {

   ProjCam *cam=NULL;

   if ((cam=(ProjCam*)TclY_HashDel(&ProjCamTable,Name))) {
      if (cam->Controls) free(cam->Controls);
      free(cam);
   }

   return(TCL_OK);
}

/*----------------------------------------------------------------------------
 * Nom      : <ProjCam_Init>
 * Creation : Janvier 2000 - J.P. Gauthier - CMC/CMOE
 *
 * But      : Initialisation du package des cameras.
 *
 * Parametres  :
 *  <Interp>   : Interpreteur Tcl
 *
 * Retour      :
 *  <TCL_...>  : Code de retour TCL
 *
 * Remarques :
 *
 *----------------------------------------------------------------------------
*/
int ProjCam_Init(Tcl_Interp *Interp){

   if (!ProjCamInit++) {
      Tcl_InitHashTable(&ProjCamTable,TCL_STRING_KEYS);
   }
   Tcl_CreateObjCommand(Interp,"projcam",ProjCam_Cmd,(ClientData)NULL,(Tcl_CmdDeleteProc*)NULL);

   return(TCL_OK);
}
