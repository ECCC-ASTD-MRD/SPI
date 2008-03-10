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
 * Modification:
 *
 *   Nom         :
 *   Date        :
 *   Description :
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

void ProjCam_CircleFrom(ProjCam *Cam,double ThetaXY,double ThetaYZ,double Delta);
void ProjCam_CircleTo(ProjCam *Cam,double ThetaXZ,double ThetaYZ,double Delta);
void ProjCam_ParamsInit(ProjCam *Cam);
int  ProjCam_Path(Tcl_Interp *Interp,ProjCam *Cam,int Objc,Tcl_Obj *CONST Objv[]);
void ProjCam_Fly(ProjCam *Cam);
void ProjCam_Place(ProjCam *Cam);
void ProjCam_Project(ProjCam *Cam,Projection *Proj);
void ProjCam_Render(Vect3d From,Vect3d To,Vect3d Up,double Size);

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
 * Modifications :
 *
 *    Nom         :
 *    Date        :
 *    Description :
 *----------------------------------------------------------------------------
*/
ProjCam* ProjCam_Get(char* Name){
   return((ProjCam*)Tcl_HashGet(&ProjCamTable,Name));
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
 * Modifications :
 *
 *    Nom         : J.P. Gauthier
 *    Date        : Avril 1998
 *    Description : Ajout du parametre resolution lors de la creation de la vue
 *----------------------------------------------------------------------------
*/
static int ProjCam_Cmd(ClientData clientData,Tcl_Interp *Interp,int Objc,Tcl_Obj *CONST Objv[]){

   int                idx;
   static CONST char *sopt[] = { "create","configure","define","stats","destroy","is",NULL };
   enum                opt { CREATE,CONFIGURE,DEFINE,STATS,DESTROY,IS };

   Tcl_ResetResult(Interp);

   if (Objc<2) {
      Tcl_WrongNumArgs(Interp,1,Objv,"command ?arg arg ...?");
      return TCL_ERROR;
   }

   if (Tcl_GetIndexFromObj(Interp,Objv[1],sopt,"command",0,&idx)!=TCL_OK) {
      return TCL_ERROR;
   }

   switch ((enum opt)idx) {
      case CREATE:
         if (Objc!=3) {
            Tcl_WrongNumArgs(Interp,2,Objv,"name");
            return TCL_ERROR;
         }
         return ProjCam_Create(Interp,Tcl_GetString(Objv[2]));
         break;

      case CONFIGURE:
         if (Objc<3) {
            Tcl_WrongNumArgs(Interp,2,Objv,"name ?option?");
            return TCL_ERROR;
         }
         return ProjCam_Config(Interp,Tcl_GetString(Objv[2]),Objc-3,Objv+3);
         break;

      case DEFINE:
         if (Objc<3) {
            Tcl_WrongNumArgs(Interp,2,Objv,"name ?option?");
            return TCL_ERROR;
         }
         return ProjCam_Define(Interp,Tcl_GetString(Objv[2]),Objc-3,Objv+3);
         break;

      case STATS:
         if (Objc<3) {
            Tcl_WrongNumArgs(Interp,2,Objv,"name ?option?");
            return TCL_ERROR;
         }
         return ProjCam_Stats(Interp,Tcl_GetString(Objv[2]),Objc-3,Objv+3);
         break;

      case DESTROY:
         if (Objc!=3) {
            Tcl_WrongNumArgs(Interp,2,Objv,"name");
            return TCL_ERROR;
         }
         return ProjCam_Destroy(Interp,Tcl_GetString(Objv[2]));
         break;

      case IS:
         if (Objc!=3) {
            Tcl_WrongNumArgs(Interp,2,Objv,"name");
            return TCL_ERROR;
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
 * Modifications :
 *
 *    Nom         :
 *    Date        :
 *    Description :
 *----------------------------------------------------------------------------
*/
static int ProjCam_Config(Tcl_Interp *Interp,char *Name,int Objc,Tcl_Obj *CONST Objv[]){

   Tcl_Obj       *obj;
   ProjCam *cam=NULL;
   int            idx,i;

   static CONST char *sopt[] = { "-to","-from","-up","-lens","-show",NULL };
   enum                opt { TO,FROM,UP,LENS,SHOW };

   cam=ProjCam_Get(Name);
   if (!cam) {
      Tcl_AppendResult(Interp,"ProjCam_Config: invalid camera definition",(char *)NULL);
      return TCL_ERROR;
   }

   for(i=0;i<Objc;i++) {

      if (Tcl_GetIndexFromObj(Interp,Objv[i],sopt,"option",0,&idx)!=TCL_OK) {
         return TCL_ERROR;
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
               Tcl_GetDoubleFromObj(Interp,Objv[++i],&cam->To[0]);
               Tcl_GetDoubleFromObj(Interp,Objv[++i],&cam->To[1]);
               Tcl_GetDoubleFromObj(Interp,Objv[++i],&cam->To[2]);
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
               Tcl_GetDoubleFromObj(Interp,Objv[++i],&cam->From[0]);
               Tcl_GetDoubleFromObj(Interp,Objv[++i],&cam->From[1]);
               Tcl_GetDoubleFromObj(Interp,Objv[++i],&cam->From[2]);
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
               Tcl_GetDoubleFromObj(Interp,Objv[++i],&cam->Up[0]);
               Tcl_GetDoubleFromObj(Interp,Objv[++i],&cam->Up[1]);
               Tcl_GetDoubleFromObj(Interp,Objv[++i],&cam->Up[2]);
            }
            break;

         case LENS:
            if (Objc==1) {
               Tcl_SetObjResult(Interp,Tcl_NewDoubleObj(cam->Lens));
            } else {
               Tcl_GetDoubleFromObj(Interp,Objv[++i],&cam->Lens);
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

   return TCL_OK;
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
 * Modifications :
 *
 *    Nom         :
 *    Date        :
 *    Description :
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
            return(ProjCam_Path(Interp,cam,Objc-1,Objv+1));
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
   return TCL_OK;
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
 * Modifications :
 *
 *    Nom         :
 *    Date        :
 *    Description :
 *----------------------------------------------------------------------------
*/
static int ProjCam_Stats(Tcl_Interp *Interp,char *Name,int Objc,Tcl_Obj *CONST Objv[]){

   ProjCam     *cam=NULL;
   int                idx,i;
   static CONST char *sopt[] = { "-dist","-angle",NULL };
   enum                opt { DIST,ANGLE };

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
 *
 * Retour:
 *  <TCL_...>    : Code de retour standard TCL
 *
 * Remarques :
 *
 * Modifications :
 *
 *    Nom         : J.P. Gauthier
 *    Date        : Avril 1998
 *    Description : Ajout du parametre resolution lors de la creation de la projection
 *----------------------------------------------------------------------------
*/
static int ProjCam_Create(Tcl_Interp *Interp,char *Name){

   ProjCam *cam;

   if (!(cam=(ProjCam*)Tcl_HashPut(Interp,&ProjCamTable,Name,sizeof(ProjCam)))) {
      return(TCL_ERROR);
   }

   /*Initialise les parametres de la camera*/

   Vect_Init(cam->From    ,0.0,0.0,2.0);
   Vect_Init(cam->To      ,0.0,0.0,1.0);
   Vect_Init(cam->Up      ,0.0,1.0,0.0);
   cam->Lens=1.0;
   cam->NbC=0;
   cam->Frame=0;
   cam->Show=0;
   cam->Update=0;
   cam->Pix=0.0;
   cam->CFrom=NULL;
   cam->CTo=NULL;
   cam->CUp=NULL;

   ProjCam_ParamsInit(cam);

   return(TCL_OK);
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
 * Modifications :
 *
 *    Nom         :
 *    Date        :
 *    Description :
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
 * Modifications :
 *
 *    Nom         :
 *    Date        :
 *    Description :
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

   /*Rotation dans le plan XY*/
   delta=ThetaYZ<0.0?-0.1:0.1;
   tmp[0]=Cam->From[1]*sin(ThetaXY+delta);
   tmp[1]=Cam->From[1]*cos(ThetaXY+delta);
   tmp[2]=Cam->From[2];

   Cam->From[0]=Cam->From[1]*sin(ThetaXY);
   Cam->From[1]=Cam->From[1]*cos(ThetaXY);

   /*Determiner le vecteur haut Cam->Up*/
   Vect_CrossProduct(Cam->Up,Cam->From,tmp);

   if (Cam->Up[0]==0.0 && Cam->Up[1]==0.0 && Cam->Up[2]==0.0) {
      Cam->Up[1]=1.0;
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
 * Modifications :
 *
 *    Nom         :
 *    Date        :
 *    Description :
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
 * Modifications :
 *
 *    Nom         :
 *    Date        :
 *    Description :
 *----------------------------------------------------------------------------
*/

int ProjCam_Path(Tcl_Interp *Interp,ProjCam *Cam,int Objc,Tcl_Obj *CONST Objv[]) {

   int i,j;

   /*Free previous control point list*/
   if (Cam->CFrom) {
      free(Cam->CFrom);Cam->CFrom=NULL;
      free(Cam->CTo);Cam->CTo=NULL;
      free(Cam->CUp);Cam->CUp=NULL;
      Cam->NbC=0;
   }

   if (Objc<=1) {
      return(TCL_OK);
   }

   if (Objc%3!=0) {
      Tcl_AppendResult(Interp,"ProjCam_Control:: Invalid number of coordinates",(char*)NULL);
      return(TCL_ERROR);
   }

   /*Process current control point list*/
   Cam->CFrom=(Vect3d*)malloc((Objc/3+2)*sizeof(Vect3d));
   Cam->CTo  =(Vect3d*)malloc((Objc/3+2)*sizeof(Vect3d));
   Cam->CUp  =(Vect3d*)malloc((Objc/3+2)*sizeof(Vect3d));
   Cam->Frame=0;

   if (!Cam->From || !Cam->CTo || !Cam->CUp) {
      Tcl_AppendResult(Interp,"ProjCam_Control:: Could not allocate memory for control point list",(char*)NULL);
      return(TCL_ERROR);
   }

   j=0;i=1;
   while(j<Objc){
      Tcl_GetDoubleFromObj(Interp,Objv[j++],&Cam->CFrom[i][0]);
      Tcl_GetDoubleFromObj(Interp,Objv[j++],&Cam->CFrom[i][1]);
      Tcl_GetDoubleFromObj(Interp,Objv[j++],&Cam->CFrom[i][2]);

      Tcl_GetDoubleFromObj(Interp,Objv[j++],&Cam->CTo[i][0]);
      Tcl_GetDoubleFromObj(Interp,Objv[j++],&Cam->CTo[i][1]);
      Tcl_GetDoubleFromObj(Interp,Objv[j++],&Cam->CTo[i][2]);

      Tcl_GetDoubleFromObj(Interp,Objv[j++],&Cam->CUp[i][0]);
      Tcl_GetDoubleFromObj(Interp,Objv[j++],&Cam->CUp[i][1]);
      Tcl_GetDoubleFromObj(Interp,Objv[j++],&Cam->CUp[i][2]);
      Cam->NbC++;
      i++;
   }

   /*Copy control point for interpolation purposes*/
   if (Vect_Equal(Cam->CFrom[1],Cam->CFrom[i-1])) {     /*Path is closed*/
      Vect_Assign(Cam->CFrom[0],Cam->CFrom[i-2]);
      Vect_Assign(Cam->CFrom[i],Cam->CFrom[2]);
      Vect_Assign(Cam->CFrom[i+1],Cam->CFrom[3]);
   } else {                                                 /*Path is open*/
      Vect_Assign(Cam->CFrom[0],Cam->CFrom[1]);
      Vect_Assign(Cam->CFrom[i],Cam->CFrom[i-1]);
      Vect_Assign(Cam->CFrom[i+1],Cam->CFrom[i-1]);
   }

   /*Copy control point for interpolation purposes*/
   if (Vect_Equal(Cam->CTo[1],Cam->CTo[i-1])) {     /*Path is closed*/
      Vect_Assign(Cam->CTo[0],Cam->CTo[i-2]);
      Vect_Assign(Cam->CTo[i],Cam->CTo[2]);
      Vect_Assign(Cam->CTo[i+1],Cam->CTo[3]);
   } else {                                                 /*Path is open*/
      Vect_Assign(Cam->CTo[0],Cam->CTo[1]);
      Vect_Assign(Cam->CTo[i],Cam->CTo[i-1]);
      Vect_Assign(Cam->CTo[i+1],Cam->CTo[i-1]);
   }

   /*Copy control point for interpolation purposes*/
   if (Vect_Equal(Cam->CUp[1],Cam->CUp[i-1])) {     /*Path is closed*/
      Vect_Assign(Cam->CUp[0],Cam->CUp[i-2]);
      Vect_Assign(Cam->CUp[i],Cam->CUp[2]);
      Vect_Assign(Cam->CUp[i+1],Cam->CUp[3]);
   } else {                                                 /*Path is open*/
      Vect_Assign(Cam->CUp[0],Cam->CUp[1]);
      Vect_Assign(Cam->CUp[i],Cam->CUp[i-1]);
      Vect_Assign(Cam->CUp[i+1],Cam->CUp[i-1]);
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
 * Modifications :
 *
 *    Nom         :
 *    Date        :
 *    Description :
 *----------------------------------------------------------------------------
*/
void ProjCam_Fly(ProjCam *Cam) {

   int     idx;
   double  d;

   if (!Cam->CFrom) {
      return;
   }

   if (Cam->Frame<0 || Cam->Frame>=Cam->NbC) {
      return;
   }

   idx=floor(Cam->Frame);
   d=Cam->Frame-idx;
   idx++;

   Cam->From[0]=InterpHermite(Cam->CFrom[idx-1][0],Cam->CFrom[idx][0],Cam->CFrom[idx+1][0],Cam->CFrom[idx+2][0],d,0.0,0.0);
   Cam->From[1]=InterpHermite(Cam->CFrom[idx-1][1],Cam->CFrom[idx][1],Cam->CFrom[idx+1][1],Cam->CFrom[idx+2][1],d,0.0,0.0);
   Cam->From[2]=InterpHermite(Cam->CFrom[idx-1][2],Cam->CFrom[idx][2],Cam->CFrom[idx+1][2],Cam->CFrom[idx+2][2],d,0.0,0.0);

   Cam->To[0]=InterpHermite(Cam->CTo[idx-1][0],Cam->CTo[idx][0],Cam->CTo[idx+1][0],Cam->CTo[idx+2][0],d,0.0,0.0);
   Cam->To[1]=InterpHermite(Cam->CTo[idx-1][1],Cam->CTo[idx][1],Cam->CTo[idx+1][1],Cam->CTo[idx+2][1],d,0.0,0.0);
   Cam->To[2]=InterpHermite(Cam->CTo[idx-1][2],Cam->CTo[idx][2],Cam->CTo[idx+1][2],Cam->CTo[idx+2][2],d,0.0,0.0);

   Cam->Up[0]=InterpHermite(Cam->CUp[idx-1][0],Cam->CUp[idx][0],Cam->CUp[idx+1][0],Cam->CUp[idx+2][0],d,0.0,0.0);
   Cam->Up[1]=InterpHermite(Cam->CUp[idx-1][1],Cam->CUp[idx][1],Cam->CUp[idx+1][1],Cam->CUp[idx+2][1],d,0.0,0.0);
   Cam->Up[2]=InterpHermite(Cam->CUp[idx-1][2],Cam->CUp[idx][2],Cam->CUp[idx+1][2],Cam->CUp[idx+2][2],d,0.0,0.0);
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
 * Modifications :
 *
 *    Nom         :
 *    Date        :
 *    Description :
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
 * Modifications :
 *
 *    Nom         :
 *    Date        :
 *    Description :
 *----------------------------------------------------------------------------
*/
void ProjCam_Project(ProjCam *Cam,Projection *Proj) {

   int    j;
   double d,p;
   Vect3d from,to,up;

   if (!Cam->CFrom || !Cam->Show) {
      return;
   }

   glMatrixMode(GL_MODELVIEW);
   glPushMatrix();
   glLineWidth(2.0f);
   glColor3f(1.0f,0.0f,0.0f);

   /* Set To vector position */
   Proj->Type->Locate(Proj,Proj->Params->Lat,Proj->Params->Lon,1);

   /* Draw de Path */
   glBegin(GL_LINE_STRIP);

   j=1;
   d=0.01;
   while(j<Cam->NbC) {
      for(p=0;p<1.0;p+=d) {
         from[0]=InterpHermite(Cam->CFrom[j-1][0],Cam->CFrom[j][0],Cam->CFrom[j+1][0],Cam->CFrom[j+2][0],p,0.0,0.0);
         from[1]=InterpHermite(Cam->CFrom[j-1][1],Cam->CFrom[j][1],Cam->CFrom[j+1][1],Cam->CFrom[j+2][1],p,0.0,0.0);
         from[2]=InterpHermite(Cam->CFrom[j-1][2],Cam->CFrom[j][2],Cam->CFrom[j+1][2],Cam->CFrom[j+2][2],p,0.0,0.0);

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

      from[0]=InterpHermite(Cam->CFrom[j-1][0],Cam->CFrom[j][0],Cam->CFrom[j+1][0],Cam->CFrom[j+2][0],p,0.0,0.0);
      from[1]=InterpHermite(Cam->CFrom[j-1][1],Cam->CFrom[j][1],Cam->CFrom[j+1][1],Cam->CFrom[j+2][1],p,0.0,0.0);
      from[2]=InterpHermite(Cam->CFrom[j-1][2],Cam->CFrom[j][2],Cam->CFrom[j+1][2],Cam->CFrom[j+2][2],p,0.0,0.0);
      to[0]=InterpHermite(Cam->CTo[j-1][0],Cam->CTo[j][0],Cam->CTo[j+1][0],Cam->CTo[j+2][0],p,0.0,0.0);
      to[1]=InterpHermite(Cam->CTo[j-1][1],Cam->CTo[j][1],Cam->CTo[j+1][1],Cam->CTo[j+2][1],p,0.0,0.0);
      to[2]=InterpHermite(Cam->CTo[j-1][2],Cam->CTo[j][2],Cam->CTo[j+1][2],Cam->CTo[j+2][2],p,0.0,0.0);
      up[0]=InterpHermite(Cam->CUp[j-1][0],Cam->CUp[j][0],Cam->CUp[j+1][0],Cam->CUp[j+2][0],p,0.0,0.0);
      up[1]=InterpHermite(Cam->CUp[j-1][1],Cam->CUp[j][1],Cam->CUp[j+1][1],Cam->CUp[j+2][1],p,0.0,0.0);
      up[2]=InterpHermite(Cam->CUp[j-1][2],Cam->CUp[j][2],Cam->CUp[j+1][2],Cam->CUp[j+2][2],p,0.0,0.0);

      ProjCam_Render(from,to,up,d*2);
   }

   /* Draw the position and aspect */
   j=0;
   while(j++<Cam->NbC) {
      ProjCam_Render(Cam->CFrom[j],Cam->CTo[j],Cam->CUp[j],d);
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
 * Modifications :
 *
 *    Nom         :
 *    Date        :
 *    Description :
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
 * Modifications :
 *
 *    Nom         :
 *    Date        :
 *    Description :
 *----------------------------------------------------------------------------
*/
static int ProjCam_Destroy(Tcl_Interp *Interp, char *Name) {

   ProjCam *cam=NULL;

   if ((cam=(ProjCam*)Tcl_HashDel(&ProjCamTable,Name))) {
      if (cam->CFrom) free(cam->CFrom);
      if (cam->CTo)   free(cam->CTo);
      if (cam->CUp)   free(cam->CUp);
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
 * Modifications :
 *
 *    Nom         :
 *    Date        :
 *    Description :
 *----------------------------------------------------------------------------
*/
int ProjCam_Init(Tcl_Interp *Interp){

   if (!ProjCamInit++) {
      Tcl_InitHashTable(&ProjCamTable,TCL_STRING_KEYS);
   }
   Tcl_CreateObjCommand(Interp,"projcam",ProjCam_Cmd,(ClientData)NULL,(Tcl_CmdDeleteProc*)NULL);

   return TCL_OK;
}
