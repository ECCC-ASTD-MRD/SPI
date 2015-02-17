/*=============================================================================
 * Environnement Canada
 * Centre Meteorologique Canadian
 * 2100 Trans-Canadienne
 * Dorval, Quebec
 *
 * Projet    : Librairie Tcl de manipulation de type de donnees.
 * Fichier   : Data_Matrix.h
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

#ifndef _DATA_MATRIX_H_
#define _DATA_MATRIX_H_

#include "Data_Funcs.h"

void Calc_Iterate1(TDef *R,TDef *A,TFunc1 *Func);
void Calc_Iterate2(TDef *R,TDef *A,TDef *B,TFunc2 *Func);
void Calc_Iterate3(TDef *R,TDef *A,TDef *B,TDef *C,TFunc3 *Func);

TDef *Calc_Compat(Tcl_Interp *Interp,TDef *A,TDef *B,int Dim,int Vect);
TDef* Calc_Length(TDef* A);
TDef* Calc_Dir(TDef* A);
TDef* Calc_Index(TDef* A,int Index);
TDef* Calc_IndexValue(TDef* A,int I,int J,int K);
TDef* Calc_RangeValue(TDef* A,int I0,int I1,int J0,int J1,int K0,int K1);
TDef* Calc_Slice(TDef* A,int N,int D);
TDef* Calc_Set(TDef* A,TDef* B,int I0,int I1,int J0,int J1,int K0,int K1);
TDef* Calc_MatrixInt(long Val);
TDef* Calc_MatrixFloat(double Val);
TDef* Calc_Matrix1(TDef* A,TFunc1 *Func,int Iterate,int Matrix,TDef_Type Type);
TDef* Calc_Matrix2(TDef* A,TDef* B,TFunc2 *Func,int Iterate,int Matrix,TDef_Type Type);
TDef* Calc_Matrix3(TDef* A,TDef* B,TDef* C,TFunc3 *Func,int Iterate,int Matrix,TDef_Type Type);
TDef* Calc_MatrixTo(TDef* A,TDef* B,char Degree);

#endif
