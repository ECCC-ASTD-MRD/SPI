/*=========================================================
 * Environnement Canada
 * Centre Meteorologique Canadien
 * 2100 Trans-Canadienne
 * Dorval, Quebec
 *
 * Projet       : Lecture et traitements de divers fichiers de donnees
 * Fichier      : Helpers.c
 * Creation     : Avril 2006 - J.P. Gauthier
 *
 * Description  : Fonctions generales d'utilites courantes.
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
 *    Boston, MA 02111-1307, USA.
 *
 *=========================================================
 */

#include "Helpers.h"
#include "rpnmacros.h"

#include <stdio.h>
#include <string.h>
#include <strings.h>
#include <stdlib.h>
#include <math.h>

int System_IsBigEndian(void) {
   short w=0x4321;

   if ((*(char*)&w)!=0x21)
     return(1);
   else
     return(0);
}

int System_ByteOrder(void) {
   short w=0x0001;
   char *byte=(char*)&w;

   return(byte[0]?SYS_LITTLE_ENDIAN:SYS_BIG_ENDIAN);
}

/*----------------------------------------------------------------------------
 * Nom      : <System_DateTime2Seconds>
 * Creation : Mai 2006 - J.P. Gauthier - CMC/CMOE
 *
 * But      : Convertir une date en secondes.
 *
 * Parametres  :
 *  <YYYYMMDD> : Date
 *  <HHMMSS>   : Heure
 *
 * Retour:
 *  <Sec>      : Secondes
 *
 * Remarques :
 *
 *----------------------------------------------------------------------------
*/
time_t System_DateTime2Seconds(int YYYYMMDD,int HHMMSS) {

   struct tm date;

   extern time_t timezone;

   date.tm_sec=fmod(HHMMSS,100);       /*seconds apres la minute [0,61]*/
   HHMMSS/=100;
   date.tm_min=fmod(HHMMSS,100);       /*minutes apres l'heure [0,59]*/
   HHMMSS/=100;
   date.tm_hour=HHMMSS;                /*heures depuis minuit [0,23]*/

   date.tm_mday=fmod(YYYYMMDD,100);    /*jour du mois [1,31]*/
   YYYYMMDD/=100;
   date.tm_mon=fmod(YYYYMMDD,100)-1;   /*mois apres Janvier [0,11]*/
   YYYYMMDD/=100;
   date.tm_year=YYYYMMDD-1900;         /*annee depuis 1900*/
   date.tm_isdst=0;                   /*Flag de l'heure avancee*/

   /* Force GMT and set back to original TZ after*/
   return(mktime(&date)-timezone);
}

/*----------------------------------------------------------------------------
 * Nom      : <System_Seconds2DateTime>
 * Creation : Mai 2006 - J.P. Gauthier - CMC/CMOE
 *
 * But      : Convertir une date en secondes.
 *
 * Parametres  :
 *  <Sec>      : Secondes
 *  <YYYYMMDD> : Date
 *  <HHMMSS>   : Heure
 *
 * Retour:
 *  <Sec>      : Secondes
 *
 * Remarques :
 *
 *----------------------------------------------------------------------------
*/
time_t System_Seconds2DateTime(time_t Sec,int *YYYYMMDD,int *HHMMSS) {

   struct tm *tsec;

   tsec=gmtime(&Sec);
   *YYYYMMDD=(tsec->tm_year+1900)*10000+(tsec->tm_mon+1)*100+tsec->tm_mday;
   *HHMMSS=tsec->tm_hour*10000+tsec->tm_min*100+tsec->tm_sec;

   return(Sec);
}

/*----------------------------------------------------------------------------
 * Nom      : <System_Julian2Stamp>
 * Creation : Mai 2006 - J.P. Gauthier - CMC/CMOE
 *
 * But      : Convertir une date julienne date stamp.
 *
 * Parametres  :
 *  <Year>     : Annee
 *  <Day>      : Jour
 *  <Time>     : Heure
 *
 * Retour:
 *  <Stamp>    : RPN Date stamp
 *
 * Remarques :
 *
 *----------------------------------------------------------------------------
*/
int System_Julian2Stamp(int Year,int Day,int Time) {

   struct tm date;
   int       stamp,op,d,t;

   date.tm_sec=fmod(Time,100);   /*seconds apres la minute [0,61]*/
   Time/=100.0;
   date.tm_min=fmod(Time,100);   /*minutes apres l'heure [0,59]*/
   Time/=100.0;
   date.tm_hour=(Day-1)*24+Time; /*heures depuis minuit [0,23]*/
   date.tm_mday=1;               /*jour du mois [1,31]*/
   date.tm_mon=0;                /*mois apres Janvier [0,11]*/
   date.tm_year=Year-1900;       /*annee depuis 1900*/
   date.tm_isdst=0;             /*Flag de l'heure avancee*/
   mktime(&date);

   /*yyyymmdd hhmmss00*/
   op=3;
   d=(date.tm_year+1900)*10000+(date.tm_mon+1)*100+date.tm_mday;
   t=date.tm_hour*1000000+date.tm_min*10000+date.tm_sec*100;
   f77name(newdate)(&stamp,&d,&t,&op);

   return(stamp);
}

/*----------------------------------------------------------------------------
 * Nom      : <System_StampDecode>
 * Creation : Mars 2006 - J.P. Gauthier - CMC/CMOE
 *
 * But      : Decoder un stamp dans ses composantes anne,mois,jou,heure,minute,seconde.
 *
 * Parametres  :
 *  <Stamp>    : Date stampe RPN.
 *  <YYYY>     : Annee
 *  <MM>       : Mois
 *  <DD>       : Jour
 *  <H>        : Heure
 *  <M>        : Minute
 *  <S>        : Seconde
 *
 * Retour:
 *
 * Remarques :
 *
 *----------------------------------------------------------------------------
*/
void System_StampDecode(int Stamp,int *YYYY,int *MM,int *DD,int *H,int *M,int *S) {

   int op=-3,date,time;

   f77name(newdate)(&Stamp,&date,&time,&op);

   *YYYY=date/1e4;
   *DD=date-((*YYYY)*1e4);
   *MM=(*DD)/1e2;
   *DD-=((*MM)*1e2);

   *H=time/1e6;
   *S=time-(*H)*1e6;
   *M=(*S)/1e4;
   *S-=(*M)*1e4;
   *S/=1e2;
}

/*----------------------------------------------------------------------------
 * Nom      : <System_Seconds2Stamp>
 * Creation : Mai 2006 - J.P. Gauthier - CMC/CMOE
 *
 * But      : Encoder des secondes en date stamp.
 *
 * Parametres  :
 *  <Sec>      : Secondes
 *
 * Retour:
 *  <Stamp>    : RPN Date stamp
 *
 * Remarques :
 *
 *----------------------------------------------------------------------------
*/
int System_Seconds2Stamp(long Sec) {

   int         stamp,date,time,op=3;
   struct tm  *tsec;

   tsec=gmtime(&Sec);
   date=(tsec->tm_year+1900)*10000+(tsec->tm_mon+1)*100+tsec->tm_mday;
   time=tsec->tm_hour*1000000+tsec->tm_min*10000+tsec->tm_sec*100;

   f77name(newdate)(&stamp,&date,&time,&op);

   return(stamp);
}

/*----------------------------------------------------------------------------
 * Nom      : <System_Stamp2Seconds>
 * Creation : Mai 2006 - J.P. Gauthier - CMC/CMOE
 *
 * But      : Decoder un date stamp en secondes.
 *
 * Parametres  :
 *  <Stamp>    : RPN Date stamp
 *
 * Retour:
 *  <Sec>      : Secondes
 *
 * Remarques :
 *
 *----------------------------------------------------------------------------
*/
long System_Stamp2Seconds(int Stamp) {

   int           yyyy,mm,dd,hh,nn,ss;
   struct tm     tdate;

   extern time_t timezone;

   System_StampDecode(Stamp,&yyyy,&mm,&dd,&hh,&nn,&ss);

   tdate.tm_sec=ss;           /*seconds apres la minute [0,61]*/
   tdate.tm_min=nn;           /*minutes apres l'heure [0,59]*/
   tdate.tm_hour=hh;          /*heures depuis minuit [0,23]*/
   tdate.tm_mday=dd;          /*jour du mois [1,31]*/
   tdate.tm_mon=mm-1;         /*mois apres Janvier [0,11]*/
   tdate.tm_year=yyyy-1900;   /*annee depuis 1900*/
   tdate.tm_isdst=0;          /*Flag de l'heure avancee*/

   /* Force GMT and set back to original TZ after*/
   return(mktime(&tdate)-timezone);
}

/*----------------------------------------------------------------------------
 * Nom      : <InterpCubic>
 * Creation : Octobre 1998 - J.P. Gauthier - CMC/CMOE
 *
 * But      : Interpolation Cubique.
 *
 * Parametres :
 *  <X0>      :
 *  <X1>      :
 *  <X2>      :
 *  <X3>      :
 *  <F>       :
 *
 * Retour:
 *  <R>  : Valeur interpolee
 *
 * Remarques :
 *
 *----------------------------------------------------------------------------
*/
double InterpCubic(double X0,double X1,double X2, double X3,double F) {

   double a0,a1,a2,f2;

   f2=F*F;
   a0=X3-X2-X0+X1;
   a1=X0-X1-a0;
   a2=X2-X0;

   return(a0*F*f2+a1*f2+a2*F+X1);
}

/*----------------------------------------------------------------------------
 * Nom      : <InterpHermite>
 * Creation : Octobre 1998 - J.P. Gauthier - CMC/CMOE
 *
 * But      : Interpolation Hermite.
 *
 * Parametres :
 *  <X0>      :
 *  <X1>      :
 *  <X2>      :
 *  <X3>      :
 *  <F>       :
 *
 * Retour:
 *  <R>  : Valeur interpolee
 *
 * Remarques :
 *
 *----------------------------------------------------------------------------
*/
double InterpHermite(double X0,double X1,double X2, double X3,double F,double T,double B) {

  double a0,a1,a2,a3,f0,f1,f2,f3;

   f2=F*F;
   f3=f2*F;

   f0=(X1-X0)*(1+B)*(1-T)/2 + (X2-X1)*(1-B)*(1-T)/2;
   f1=(X2-X1)*(1+B)*(1-T)/2 + (X3-X2)*(1-B)*(1-T)/2;

   a0=2*f3-3*f2+1;
   a1=f3-2*f2+F;
   a2=f3-f2;
   a3=-2*f3+3*f2;

   return(a0*X1+a1*f0+a2*f1+a3*X2);
}

/*----------------------------------------------------------------------------
 * Nom      : <QSort_Double>
 * Creation : Octobre 2005 - J.P. Gauthier - CMC/CMOE
 *
 * But      : Fonction de comparaison pour le tri.
 *
 * Parametres :
 *  <V0>      : Valeur 0
 *  <V1>      : Valeur 1
 *
 * Retour:
 *  <<>=>:      -1< 0= 1>
 *

 * Remarques :
 *
 *----------------------------------------------------------------------------
*/
int QSort_Double(const void *A,const void *B){

   if (*(const double*)A<*(const double*)B) {
      return(-1);
   } else if (*(const double*)A>*(const double*)B) {
      return(1);
   } else {
      return(0);
   }
}

int QSort_Int(const void *A, const void *B) {
   return(*(const int*)A)-(*(const int*)B);
}

double HCentile(double *M,int N,int K) {

   register int    i,j,l,m;
   register double x,d;

   l=0;m=N-1;

   while(l<m) {
      x=M[K];
      i=l;
      j=m;
      do {
         while (M[i]<x) i++;
         while (x<M[j]) j--;
         if (i<=j) {
            d=M[i];M[i]=M[j];M[j]=d;
            i++;
            j--;
         }
      } while (i<=j);
      if (j<K) l=i;
      if (K<i) m=j;
   }
   return(M[K]);
}

char* strpath(char *Path,char *File) {

   char *c;
   char *new;

   c=strrchr(Path,'/');
   if (!c) c=Path;
   new=(char*)calloc(strlen(File)+(c-Path)+2,1);

   strncpy(new,Path,(c-Path));
   strcat(new,"/");
   strcat(new,File);
   return(new);
}

char* strcatalloc(char *StrTo,char *StrFrom) {

   if (StrTo) {
      StrTo=(char*)realloc(StrTo,strlen(StrTo)+strlen(StrFrom)+1);
      strcat(StrTo,StrFrom);
   } else {
      StrTo=strdup(StrFrom);
   }
   return(StrTo);
}

void strrep(char *Str,char Tok,char Rep) {

   while(*Str++!='\0')
     if (*Str==Tok)
        *Str=Rep;
}

void strtrim(char *Str,char Tok) {

   register int i=0;
   char *s;

   /*Clear fisrt blanks*/
   while((*Str+i)==Tok)
     i++;

   if (i) strcpy(Str,Str+i);

   /*Clear end blanks*/
   s=Str+strlen(Str);
   while(*--s==Tok)
      *s='\0';
}

int strrindex(char *Str) {

   char *l,*r,*s;
   int   n,k=0,t=1;

   s=strdup(Str);
   l=index(s,'(');
   r=index(s,')');

   if (!l || !r) {
      free(s);
      return(-1);
   }

   sscanf(l,"(%i)",&n);

   l=s;
   while(*s!='\0') {
      if (*s=='(') {
         t=0;
      }

      if (t) Str[k++]=*s;

      if (*s==')') {
         t=1;
      }
      s++;
   }
   Str[k]='\0';
   free(l);

   return(n);
}
