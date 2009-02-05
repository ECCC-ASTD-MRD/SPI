/*=============================================================================
 * Environnement Canada
 * Centre Meteorologique Canadian
 * 2100 Trans-Canadienne
 * Dorval, Quebec
 *
 * Projet    : Conversion du canvas Tk en Canvas GL.
 * Fichier   : glShader.h
 * Creation  : Aout 2005 - J.P. Gauthier - CMC/CMOE
 *
 * Description: Fonctions de support au shader GL.
 *
 * Remarques :
 *
 * License   :
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

#include <strings.h>
#include <stdlib.h>
#include <stdio.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>

#include <glStuff.h>

void GLShader_InfoLog(GLhandleARB Object);
GLint GLShader_UniformGet(const GLhandleARB Prog,const GLcharARB *Name);
GLint GLShader_AttribGet(const GLhandleARB Prog,const GLcharARB *Name);
GLhandleARB GLShader_Load(const GLcharARB *Path,const GLcharARB *Name);
GLhandleARB GLShader_Install(const GLcharARB *VertSrc,const GLcharARB *FragSrc);
void GLShader_UnInstall(GLhandleARB Prog);
