/*=========================================================
 * Environnement Canada
 * Centre Meteorologique Canadien
 * 2100 Trans-Canadienne
 * Dorval, Quebec
 *
 * Projet       : Application de photos geotiff diverses
 * Fichier      : tcl3DModel3DS.h
 * Creation     : Aout2007 - J.P. Gauthier
 *
 * Description  : Module de lecture de modele 3D en format 3DS
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
#ifndef _tcl3DModel3DS_h
#define _tcl3DModel3DS_h

#include <stdio.h>
#include <malloc.h>

// Primary Chunk, at the beginning of each file
#define PRIMARY       0x4D4D

// Main Chunks
#define OBJECTINFO    0x3D3D           // This gives the version of the mesh and is found right before the material and object information
#define VERSION       0x0002           // This gives the version of the .3ds file
#define EDITKEYFRAME  0xB000           // This is the header for all of the key frame info

// sub defines of OBJECTINFO
#define MATERIAL    0xAFFF             // This stored the texture info
#define OBJECT      0x4000             // This stores the faces, vertices, etc...

// sub defines of MATERIAL
#define MATNAME       0xA000           // This holds the material name
#define MATAMBIENT    0xA010           // This holds the color of the object/material
#define MATDIFFUSE    0xA020           // This holds the color of the object/material
#define MATSPECULAR   0xA030           // This holds the color of the object/material
#define MATSHININESS  0xA040           // This holds the color of the object/material
#define MATMAP        0xA200           // This is a header for a new material
#define MATMAPFILE    0xA300           // This holds the file name of the texture

#define OBJECT_MESH   0x4100           // This lets us know that we are reading a new object

// sub defines of OBJECT_MESH
#define OBJECT_VERTICES 0x4110         // The objects vertices
#define OBJECT_FACES    0x4120         // The objects faces
#define OBJECT_MATERIAL 0x4130         // This is found if the object has a material, either texture map or color
#define OBJECT_UV       0x4140         // The UV texture coordinates

// This holds the chunk info
typedef struct T3DSChunk {
   unsigned short int ID;              // The chunk's ID
   unsigned int length;                // The length of the chunk
   unsigned int bytes;                 // The amount of bytes read within that chunk
} T3DSChunk;

int Model_Load3DS(T3DModel *M,char *Path);

int  Model3DS_GetString(FILE *File,char *Buf);
void Model3DS_ChunkRead(FILE *File,T3DSChunk *Chunk);
void Model3DS_ChunkReadColor(FILE *File,float *Color,T3DSChunk *Chunk);
void Model3DS_ChunkReadVertexIndices(FILE *File,T3DObject *Obj,T3DSChunk *PreviousChunk);
void Model3DS_ChunkReadVertices(FILE *File,T3DObject *Obj,T3DSChunk *PreviousChunk);
void Model3DS_ChunkReadUVCoordinates(FILE *File,T3DObject *Obj,T3DSChunk *PreviousChunk);
void Model3DS_ChunkReadObjectMaterial(FILE *File,T3DModel *Model,T3DObject *Obj,T3DSChunk *PreviousChunk);
void Model3DS_ChunkProcessNext(FILE *File,T3DModel *Model,T3DSChunk *PreviousChunk);
void Model3DS_ChunkProcessNextObject(FILE *File,T3DModel *Model,T3DObject *Obj,T3DSChunk *PreviousChunk);
void Model3DS_ChunkProcessNextMaterial(FILE *File,T3DModel *Model,T3DSChunk *PreviousChunk);

#endif
