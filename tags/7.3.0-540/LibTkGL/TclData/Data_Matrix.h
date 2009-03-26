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

void Calc_Iterate1(TDataDef *R,TDataDef *A,TFunc1 *Func);
void Calc_Iterate2(TDataDef *R,TDataDef *A,TDataDef *B,TFunc2 *Func);
void Calc_Iterate3(TDataDef *R,TDataDef *A,TDataDef *B,TDataDef *C,TFunc3 *Func);

TDataDef *Calc_Compat(Tcl_Interp *Interp,TDataDef *A,TDataDef *B,int Dim,int Vect);
TDataDef* Calc_Length(TDataDef* A);
TDataDef* Calc_Dir(TDataDef* A);
TDataDef* Calc_Index(TDataDef* A,int Index);
TDataDef* Calc_IndexValue(TDataDef* A,int I,int J,int K);
TDataDef* Calc_RangeValue(TDataDef* A,int I0,int I1,int J0,int J1,int K0,int K1);
TDataDef* Calc_Slice(TDataDef* A,int N,int D);
TDataDef* Calc_Set(TDataDef* A,TDataDef* B,int I0,int I1,int J0,int J1,int K0,int K1);
TDataDef* Calc_MatrixInt(long Val);
TDataDef* Calc_MatrixFloat(double Val);
TDataDef* Calc_Matrix1(TDataDef* A,TFunc1 *Func,int Iterate,int Matrix,TData_Type Type);
TDataDef* Calc_Matrix2(TDataDef* A,TDataDef* B,TFunc2 *Func,int Iterate,int Matrix,TData_Type Type);
TDataDef* Calc_Matrix3(TDataDef* A,TDataDef* B,TDataDef* C,TFunc3 *Func,int Iterate,int Matrix,TData_Type Type);
TDataDef* Calc_MatrixTo(TDataDef* A,TDataDef* B,char Degree);

#endif
