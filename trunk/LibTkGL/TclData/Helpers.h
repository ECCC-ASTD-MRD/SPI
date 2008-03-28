/*=========================================================
 * Environnement Canada
 * Centre Meteorologique Canadien
 * 2100 Trans-Canadienne
 * Dorval, Quebec
 *
 * Projet       : Lecture et traitements de divers fichiers de donnees
 * Fichier      : Helpers.h
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
 * Modification :
 *
 *    Nom         :
 *    Date        :
 *    Description :
 *
 *=========================================================
 */
#ifndef _Helpers_h
#define _Helpers_h

#include <time.h>

#define SYS_BIG_ENDIAN     0
#define SYS_LITTLE_ENDIAN  1

#define SYS_FIX2(x) ((((x) & 0xff)<<8) | ((unsigned short)(x)>>8))
#define SYS_FIX4(x) (((x)<<24) | (((x)<<8) & 0x00ff0000) | (((x)>>8) & 0x0000ff00) | ((x)>>24))
#define SYS_FIX8(x) (((long long)(SYS_SWAP4((unsigned int)((x<<32)>>32)))<<32) | (unsigned int)SYS_SWAP4(((int)(x>>32))))

#define SYS_SWAP2(x) (*(unsigned short*)&(x)=SYS_FIX2(*(unsigned short*)&(x)))
#define SYS_SWAP4(x) (*(unsigned int*)&(x)=SYS_FIX4(*(unsigned int*)&(x)))
#define SYS_SWAP8(x) (*(unsigned long long*)&(x)=SYS_FIX8(*(unsigned long long*)&(x)))

#define SYS_IFSWAP2(i,t,x) if (i!=t) SYS_SWAP2(x)
#define SYS_IFSWAP4(i,t,x) if (i!=t) SYS_SWAP4(x)
#define SYS_IFSWAP8(i,t,x) if (i!=t) SYS_SWAP8(x)

#define CLOSE(V)                          ((V-(int)V)<0.5?(V-(int)V):(V-(int)V)-0.5)
#define ROUND(V)                          ((int)(V+0.5))
#define ILIN(X,Y,Z)                       (X+(Y-X)*(Z))
#define ILFAC(Level,V0,V1)                ((V1==V0)?0.5:(Level-V0)/(V1-V0))
#define ILVIN(VAL,A,B)                    ((VAL>=A && VAL<=B) || (VAL<=A && VAL>=B))
#define ILADD(SIDE,F)                     (SIDE?1.0f-F:F)
#define FARENOUGH(DT,X0,Y0,X1,Y1)         (hypot((Y1-Y0),(X1-X0))>DT)
#define LOG2(V)                           (floor(log10(V)/log10(2)))

int   QSort_Double(const void *A,const void *B);
int   QSort_Int(const void *A,const void *B);

double InterpCubic(double X0,double X1,double X2, double X3,double F);
double InterpHermite(double X0,double X1,double X2, double X3,double F,double T,double B);

double HCentile(double *M,int N,int K);

char* strpath(char *Path,char *File);
char* strcatalloc(char *StrTo,char *StrFrom);
void  strtrim(char* Str,char Tok);
void  strrep(char *Str,char Tok,char Rep);
int   strfind(char *Str,char Tok);
int   strrindex(char *Str);

#define System_IsStamp(S) (S<999999999)

int    System_IsBigEndian(void);
int    System_ByteOrder(void);
time_t System_DateTime2Seconds(int YYYYMMDD,int HHMMSS,int GMT);
time_t System_Seconds2DateTime(time_t Sec,int *YYYYMMDD,int *HHMMSS,int GMT);
int    System_Julian2Stamp(int Year,int Day,int Time);
long   System_Stamp2Seconds(int Stamp);
int    System_Seconds2Stamp(long Sec);
void   System_StampDecode(int Stamp,int *YYYY,int *MM,int *DD,int *H,int *M,int *S);

#endif
