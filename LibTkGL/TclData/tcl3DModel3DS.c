/*=========================================================
 * Environnement Canada
 * Centre Meteorologique Canadien
 * 2100 Trans-Canadienne
 * Dorval, Quebec
 *
 * Projet       : Affichage de modele 3D
 * Fichier      : tcl3DModel3DS.c
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

#include "tcl3DModel.h"
#include "tcl3DModel3DS.h"

/*--------------------------------------------------------------------------------------------------------------
 * Nom          : <Model_Load3DS>
 * Creation     : Aout 2007 J.P. Gauthier
 *
 * But          : Lire un fichier MDL
 *
 * Parametres   :
 *   <M>        : Objet Model
 *   <Path>     : Path complet du fichier MDL
 *
 * Retour       :
 *
 * Remarques    :
 *
 *---------------------------------------------------------------------------------------------------------------
*/
int Model_Load3DS(T3DModel *M,char *Path) {

   T3DSChunk chunk;
   FILE *file;

   if (!(file=fopen(Path,"r"))) {
      return(0);
   }

   Model3DS_ChunkRead(file,&chunk);

   // Make sure this is a 3DS file
   if (chunk.ID!=PRIMARY) {
      fclose(file);
      return(0);
   }

   // Begin loading objects, by calling this recursive function
   Model3DS_ChunkProcessNext(file,M,&chunk);

   fclose(file);
   return(1);
}

int Model3DS_GetString(FILE *File,char *Buf) {

   int index=0;

   fread(Buf,1,1,File);

   // Loop until we get NULL reading in a character at a time.
   while (*(Buf+index++)) {
      fread(Buf+index,1,1,File);
   }
   return(strlen(Buf)+1);
}

/*--------------------------------------------------------------------------------------------------------------
 * Nom          : <Model3DS_ChunkRead>
 * Creation     : Aout 2007 2003 J.P. Gauthier (Based on gametutorials.com code)
 *
 * But          : Reads in a chunk ID and it's length in bytes.
 *
 * Parametres   :
 *   <File>     : File pointer
 *   <Chunk>    : Data Chunck
 *
 * Retour       :
 *
 * Remarques    :
 *
 *---------------------------------------------------------------------------------------------------------------
*/
void Model3DS_ChunkRead(FILE *File,T3DSChunk *Chunk) {
   // This reads the chunk ID which is 2 bytes.
   // The chunk ID is like OBJECT or MATERIAL.  It tells what data is
   // able to be read in within the chunks section.
   Chunk->bytes=fread(&Chunk->ID,1,2,File);

   // Then, we read the length of the chunk which is 4 bytes.
   // This is how we know how much to read in, or read past.
   Chunk->bytes+=fread(&Chunk->length,1,4,File);
}

/*--------------------------------------------------------------------------------------------------------------
 * Nom          : <Model3DS_ChunkReadColor>
 * Creation     : Aout 2007 2003 J.P. Gauthier (Based on gametutorials.com code)
 *
 * But          : Reads in a color chunk.
 *
 * Parametres   :
 *   <File>     : File pointer
 *   <Color>    : Color to be read
 *   <Chunk>    : Data Chunck
 *
 * Retour       :
 *
 * Remarques    :
 *
 *---------------------------------------------------------------------------------------------------------------
*/
void Model3DS_ChunkReadColor(FILE *File,float *Color,T3DSChunk *Chunk) {
   T3DSChunk tempChunk;

   char color[3];
   char n;
   // Read the color chunk info
   Model3DS_ChunkRead(File,&tempChunk);

   n=tempChunk.length-tempChunk.bytes;

   // Read in the R G B color (3 bytes - 0 through 255)
   tempChunk.bytes+=fread(color,1,tempChunk.length-tempChunk.bytes,File);
   Color[0]=color[0]/255;
   if (n>1) {
      Color[1]=color[1]/255;
      Color[2]=color[2]/255;
      Color[3]=1.0;
   }

   // Add the bytes read to our chunk
   Chunk->bytes+=tempChunk.bytes;
}

/*--------------------------------------------------------------------------------------------------------------
 * Nom          : <Model3DS_ChunkReadVertexIndices>
 * Creation     : Aout 2007 2003 J.P. Gauthier (Based on gametutorials.com code)
 *
 * But          : Reads in the indices for the vertex array.
 *
 * Parametres   :
 *   <File>     : File pointer
 *   <Obj>      : Object
 *   <Chunk>    : Data Chunck
 *
 * Retour       :
 *
 * Remarques    :
 *
 *---------------------------------------------------------------------------------------------------------------
*/
void Model3DS_ChunkReadVertexIndices(FILE *File,T3DObject *Obj,T3DSChunk *PreviousChunk) {

   unsigned short i,j,index=0;              // This is used to read in the current face index

   // In order to read in the vertex indices for the object, we need to first
   // read in the number of them, then read them in.  Remember,
   // we only want 3 of the 4 values read in for each face.  The fourth is
   // a visibility flag for 3D Studio Max that doesn't mean anything to us.

   // Read in the number of faces that are in this object (int)
   PreviousChunk->bytes+=fread(&Obj->NFc,1,2,File);

   // Alloc enough memory for the faces and initialize the structure
   Obj->Fc=(TFace*)calloc(Obj->NFc,sizeof(TFace));

   // Go through all of the faces in this object
   for(i=0;i<Obj->NFc;i++) {
      // Next, we read in the A then B then C index for the face, but ignore the 4th value.
      // The fourth value is a visibility flag for 3D Studio Max, we don't care about this.
      Obj->Fc[i].NIdx=3;
      Obj->Fc[i].Mt=NULL;
      Obj->Fc[i].Idx=(unsigned int*)malloc(Obj->Fc[i].NIdx*sizeof(unsigned int));

      for(j=0;j<4;j++) {
         // Read the first vertice index for the current face
         PreviousChunk->bytes+=fread(&index,1,sizeof(index),File);

         if(j<3) {
            // Store the index in our face structure.
            Obj->Fc[i].Idx[j]=index;
         }
      }
   }
}

/*--------------------------------------------------------------------------------------------------------------
 * Nom          : <Model3DS_ChunkReadVertices>
 * Creation     : Aout 2007 2003 J.P. Gauthier (Based on gametutorials.com code)
 *
 * But          : Reads in the vertices for the object.
 *
 * Parametres   :
 *   <File>     : File pointer
 *   <Obj>      : Object
 *   <Chunk>    : Data Chunck
 *
 * Retour       :
 *
 * Remarques    :
 *
 *---------------------------------------------------------------------------------------------------------------
*/
void Model3DS_ChunkReadVertices(FILE *File,T3DObject *Obj,T3DSChunk *PreviousChunk) {

   int   i;
   float v;

   // Read in the number of vertices (int)
   PreviousChunk->bytes+=fread(&(Obj->NVr),1,2,File);

   // Allocate the memory for the verts and initialize the structure
   Obj->Vr=(Vect3f*)calloc(Obj->NVr,sizeof(Vect3f));

   // Read in the array of vertices (an array of 3 floats)
   PreviousChunk->bytes+=fread(Obj->Vr,1,PreviousChunk->length-PreviousChunk->bytes,File);

   // Now we should have all of the vertices read in.  Because 3D Studio Max
   // Models with the Z-Axis pointing up (strange and ugly I know!), we need
   // to flip the y values with the z values in our vertices.  That way it
   // will be normal, with Y pointing up.  If you prefer to work with Z pointing
   // up, then just delete this next loop.  Also, because we swap the Y and Z
   // we need to negate the Z to make it come out correctly.

   // Go through all of the vertices that we just read and swap the Y and Z values
   for(i=0;i<Obj->NVr;i++) {
      v=Obj->Vr[i][1];
      Obj->Vr[i][1]=Obj->Vr[i][2];

      // Set the Z value to the Y value, but negative Z because 3D Studio max does the opposite.
      Obj->Vr[i][2]=-v;
   }
}

/*--------------------------------------------------------------------------------------------------------------
 * Nom          : <Model3DS_ChunkReadUVCoordinates>
 * Creation     : Aout 2007 2003 J.P. Gauthier (Based on gametutorials.com code)
 *
 * But          : Reads in the UV coordinates for the object.
 *
 * Parametres   :
 *   <File>     : File pointer
 *   <Obj>      : Object
 *   <Chunk>    : Data Chunck
 *
 * Retour       :
 *
 * Remarques    :
 *
 *---------------------------------------------------------------------------------------------------------------
*/
void Model3DS_ChunkReadUVCoordinates(FILE *File,T3DObject *Obj,T3DSChunk *PreviousChunk) {

   float *co;
   int    n,nc;
   short  nt;
   // In order to read in the UV indices for the object, we need to first
   // read in the amount there are, then read them in.

   // Read in the number of UV coordinates there are (int)
   PreviousChunk->bytes+=fread(&nt,1,2,File);

   // Allocate memory to hold the UV coordinates
   co=(float*)malloc(nt*2*sizeof(float));
   Obj->Tx=(Vect3f*)malloc(nt*sizeof(Vect3f));

   // Read in the texture coodinates (an array 2 float)
   PreviousChunk->bytes+=fread(co,1,PreviousChunk->length-PreviousChunk->bytes,File);
   for(n=0,nc=0;n<nt;n++) {
      Obj->Tx[n][0]=co[nc++];
      Obj->Tx[n][1]=1.0-co[nc++];
      Obj->Tx[n][2]=0.0;
   }
   free(co);
}

/*--------------------------------------------------------------------------------------------------------------
 * Nom          : <Model3DS_ChunkReadObjectMaterial>
 * Creation     : Aout 2007 2003 J.P. Gauthier (Based on gametutorials.com code)
 *
 * But          : Reads in the material name assigned to the object and sets the materialID.
 *
 * Parametres   :
 *   <File>     : File pointer
 *   <Obj>      : Object
 *   <Chunk>    : Data Chunck
 *
 * Retour       :
 *
 * Remarques    :
 *
 *---------------------------------------------------------------------------------------------------------------
*/
void Model3DS_ChunkReadObjectMaterial(FILE *File,T3DModel *Model,T3DObject *Obj,T3DSChunk *PreviousChunk) {

   char       name[255];
   int        i;
   short      n,f;
   TMaterial *mt=NULL;

   // *What is a material?*  - A material is either the color or the texture map of the object.
   // It can also hold other information like the brightness, shine, etc... Stuff we don't
   // really care about.  We just want the color, or the texture map file name really.

   // Here we read the material name that is assigned to the current object.
   // strMaterial should now have a string of the material name, like "Material #2" etc..
   PreviousChunk->bytes+=Model3DS_GetString(File,name);

   // Now that we have a material name, we need to go through all of the materials
   // and check the name against each material.  When we find a material in our material
   // list that matches this name we just read in, then we assign the materialID
   // of the object to that material index.
   for (i=0;i<Model->NMt;i++) {
      // If the material we just read in matches the current texture name
      if (strcmp(name,Model->Mt[i].Name)==0) {
         mt=&Model->Mt[i];
         break;
      }
   }

   // Read in the number of faces with this materials and assignt the material to those faces
   PreviousChunk->bytes+=fread(&n,1,2,File);
   for (i=0;i<n;i++) {
      PreviousChunk->bytes+=fread(&f,1,2,File);
      Obj->Fc[f].Mt=mt;
   }

   // Read past the rest of the chunk since we don't care about shared vertices
   // You will notice we subtract the bytes already read in this chunk from the total length.
   fseek(File,PreviousChunk->length-PreviousChunk->bytes,SEEK_CUR);
   PreviousChunk->bytes+=PreviousChunk->length-PreviousChunk->bytes;
}

/*--------------------------------------------------------------------------------------------------------------
 * Nom          : <Model3DS_ChunkProcessNext>
 * Creation     : Aout 2007 J.P. Gauthier (Based on gametutorials.com code)
 *
 * But          : Reads the main sections of the .3DS file, then dives deeper with recursion
 *
 * Parametres   :
 *
 * Retour       :
 *
 * Remarques    :
 *
 *---------------------------------------------------------------------------------------------------------------
*/
void Model3DS_ChunkProcessNext(FILE *File,T3DModel *Model,T3DSChunk *PreviousChunk) {

   T3DSChunk currentChunk,tempChunk;
   int       ver;

   // Below we check our chunk ID each time we read a new chunk.  Then, if
   // we want to extract the information from that chunk, we do so.
   // If we don't want a chunk, we just read past it.

   // Continue to read the sub chunks until we have reached the length.
   // After we read ANYTHING we add the bytes read to the chunk and then check
   // check against the length.
   while (PreviousChunk->bytes<PreviousChunk->length) {

      // Read next Chunk
      Model3DS_ChunkRead(File,&currentChunk);

      // Check the chunk ID
      switch (currentChunk.ID) {

         case VERSION: // This holds the version of the file
#ifdef DEBUG
            printf("(DEBUG) Model3DS_ChunkProcessNext: VERSION\n");
#endif
            // If the file was made in 3D Studio Max, this chunk has an int that
            // holds the file version.  Since there might be new additions to the 3DS file
            // format in 4.0, we give a warning to that problem.
            // However, if the file wasn't made by 3D Studio Max, we don't 100% what the
            // version length will be so we'll simply ignore the value

            // Read the file version and add the bytes read to our bytes variable
            // If the file version is over 3, give a warning that there could be a problem
            if ((currentChunk.length-currentChunk.bytes==4)) {
               currentChunk.bytes+=fread(&ver,1,currentChunk.length-currentChunk.bytes,File);
               if (ver>0x03) {
                  printf("(WARNING) Model3DS_ChunkProcessNext: This 3DS file is over version 3 so it may load incorrectly");
               }
            } else {
               fseek(File,currentChunk.length-currentChunk.bytes,SEEK_CUR);
               currentChunk.bytes+=currentChunk.length-currentChunk.bytes;
            }
            break;

         case OBJECTINFO: // This holds the version of the mesh
#ifdef DEBUG
            printf("(DEBUG) Model3DS_ChunkProcessNext: OBJECTINFO\n");
#endif
            // This chunk holds the version of the mesh.  It is also the head of the MATERIAL
            // and OBJECT chunks.  From here on we start reading in the material and object info.

            // Read the next chunk
            Model3DS_ChunkRead(File,&tempChunk);

            // Get the version of the mesh
            fseek(File,tempChunk.length-tempChunk.bytes,SEEK_CUR);
            tempChunk.bytes+=tempChunk.length-tempChunk.bytes;

            // Increase the bytes by the bytes read from the last chunk
            currentChunk.bytes+=tempChunk.bytes;

            // Go to the next chunk, which is the object has a texture, it should be MATERIAL, then OBJECT.
            Model3DS_ChunkProcessNext(File,Model,&currentChunk);
            break;

         case MATERIAL: // This holds the material information
#ifdef DEBUG
            printf("(DEBUG) Model3DS_ChunkProcessNext: MATERIAL\n");
#endif
            // This chunk is the header for the material info chunks

            // Increase the number of materials
            Model->NMt++;

            // Add a empty texture structure to our texture list.
            // If you are unfamiliar with STL's "vector" class, all push_back()
            // does is add a new node onto the list.  I used the vector class
            // so I didn't need to write my own link list functions.
            Model->Mt=(TMaterial*)realloc(Model->Mt,Model->NMt*sizeof(TMaterial));
            Model->Mt[Model->NMt-1].Tex=0;
            Model->Mt[Model->NMt-1].Name[0]='\0';
            Model->Mt[Model->NMt-1].Path[0]='\0';

            // Proceed to the material loading function
            Model3DS_ChunkProcessNextMaterial(File,Model,&currentChunk);
            break;

         case OBJECT: // This holds the name of the object being read
#ifdef DEBUG
            printf("(DEBUG) Model3DS_ChunkProcessNext: OBJECT\n");
#endif
            // This chunk is the header for the object info chunks.  It also
            // holds the name of the object.

            // Add a new tObject node to our list of objects (like a link list)
            Model_ObjAdd(Model,1);
            // Get the name of the object and store it, then add the read bytes to our byte counter.
            currentChunk.bytes+=Model3DS_GetString(File,Model->Obj[Model->NObj-1].Name);

            // Now proceed to read in the rest of the object information
            Model3DS_ChunkProcessNextObject(File,Model,&(Model->Obj[Model->NObj-1]),&currentChunk);
            break;

         case EDITKEYFRAME:
#ifdef DEBUG
            printf("(DEBUG) Model3DS_ChunkProcessNext: EDITKEYFRAME\n");
#endif
            // Because I wanted to make this a SIMPLE tutorial as possible, I did not include
            // the key frame information.  This chunk is the header for all the animation info.
            // In a later tutorial this will be the subject and explained thoroughly.

            //ProcessNextKeyFrameChunk(pModel, currentChunk);

            // Read past this chunk and add the bytes read to the byte counter
            fseek(File,currentChunk.length-currentChunk.bytes,SEEK_CUR);
            currentChunk.bytes+=currentChunk.length-currentChunk.bytes;
            break;

         default:
#ifdef DEBUG
            printf("(DEBUG) Model3DS_ChunkProcessNext: OTHER\n");
#endif
            // If we didn't care about a chunk, then we get here.  We still need
            // to read past the unknown or ignored chunk and add the bytes read to the byte counter.
            fseek(File,currentChunk.length-currentChunk.bytes,SEEK_CUR);
            currentChunk.bytes+=currentChunk.length-currentChunk.bytes;
            break;
      }

      // Add the bytes read from the last chunk to the previous chunk passed in.
      PreviousChunk->bytes+=currentChunk.bytes;
   }
}

/*--------------------------------------------------------------------------------------------------------------
 * Nom          : <Model3DS_ChunkProcessNextObject>
 * Creation     : Aout 2007 2003 J.P. Gauthier (Based on gametutorials.com code)
 *
 * But          : Handles all the information about the objects in the file.
 *
 * Parametres   :
 *   <File>     : File pointer
 *   <Model>    : 3D Model object
 *   <Chunk>    : Data Chunck
 *
 * Retour       :
 *
 * Remarques    :
 *
 *---------------------------------------------------------------------------------------------------------------
*/
void Model3DS_ChunkProcessNextObject(FILE *File,T3DModel *Model,T3DObject *Obj,T3DSChunk *PreviousChunk) {

   // The current chunk to work with
   T3DSChunk currentChunk;

   // Continue to read these chunks until we read the end of this sub chunk
   while (PreviousChunk->bytes<PreviousChunk->length) {
      // Read the next chunk
      Model3DS_ChunkRead(File,&currentChunk);

      // Check which chunk we just read
      switch (currentChunk.ID) {

         case OBJECT_MESH: // This lets us know that we are reading a new object
#ifdef DEBUG
            printf("(DEBUG) Model3DS_ChunkProcessNextObject: OBJECT_MESH\n");
#endif
            // We found a new object, so let's read in it's info using recursion
            Model3DS_ChunkProcessNextObject(File,Model,Obj,&currentChunk);
            break;

         case OBJECT_VERTICES: // This is the objects vertices
#ifdef DEBUG
            printf("(DEBUG) Model3DS_ChunkProcessNextObject: OBJECT_VERTICES\n");
#endif
            Model3DS_ChunkReadVertices(File,Obj,&currentChunk);
            break;

         case OBJECT_FACES: // This is the objects face information
#ifdef DEBUG
            printf("(DEBUG) Model3DS_ChunkProcessNextObject: OBJECT_FACES\n");
#endif
            Model3DS_ChunkReadVertexIndices(File,Obj,&currentChunk);
            break;

         case OBJECT_MATERIAL: // This holds the material name that the object has
#ifdef DEBUG
            printf("(DEBUG) Model3DS_ChunkProcessNextObject: OBJECT_MATERIAL\n");
#endif
            // This chunk holds the name of the material that the object has assigned to it.
            // This could either be just a color or a texture map.  This chunk also holds
            // the faces that the texture is assigned to (In the case that there is multiple
            // textures assigned to one object, or it just has a texture on a part of the object.
            // Since most of my game objects just have the texture around the whole object, and
            // they aren't multitextured, I just want the material name.

            // We now will read the name of the material assigned to this object
            Model3DS_ChunkReadObjectMaterial(File,Model,Obj,&currentChunk);
            break;

         case OBJECT_UV: // This holds the UV texture coordinates for the object
#ifdef DEBUG
            printf("(DEBUG) Model3DS_ChunkProcessNextObject: OBJECT_UV\n");
#endif
            // This chunk holds all of the UV coordinates for our object.  Let's read them in.
            Model3DS_ChunkReadUVCoordinates(File,Obj,&currentChunk);
            break;

         default:
#ifdef DEBUG
            printf("(DEBUG) Model3DS_ChunkProcessNextObject: OTHER\n");
#endif
            // Read past the ignored or unknown chunks
            fseek(File,currentChunk.length-currentChunk.bytes,SEEK_CUR);
            currentChunk.bytes+=currentChunk.length-currentChunk.bytes;
            break;
      }

      // Add the bytes read from the last chunk to the previous chunk passed in.
      PreviousChunk->bytes+=currentChunk.bytes;
   }
}

/*--------------------------------------------------------------------------------------------------------------
 * Nom          : <Model3DS_ChunkProcessNextMaterial>
 * Creation     : Aout 2007 2003 J.P. Gauthier (Based on gametutorials.com code)
 *
 * But          : Handles all the information about the material (Texture).
 *
 * Parametres   :
 *   <File>     : File pointer
 *   <Model>    : 3D Model object
 *   <Chunk>    : Data Chunck
 *
 * Retour       :
 *
 * Remarques    :
 *
 *---------------------------------------------------------------------------------------------------------------
*/
void Model3DS_ChunkProcessNextMaterial(FILE *File,T3DModel *Model,T3DSChunk *PreviousChunk) {

   // The current chunk to work with
   T3DSChunk currentChunk;

   // Continue to read these chunks until we read the end of this sub chunk
   while (PreviousChunk->bytes<PreviousChunk->length) {
      // Read the next chunk
      Model3DS_ChunkRead(File,&currentChunk);

      // Check which chunk we just read in
      switch (currentChunk.ID) {
         case MATNAME: // This chunk holds the name of the material
#ifdef DEBUG
            printf("(DEBUG) Model3DS_ChunkProcessNextMaterial: MATNAME\n");
 #endif
           // Here we read in the material name
            currentChunk.bytes+=fread(Model->Mt[Model->NMt-1].Name,1,currentChunk.length-currentChunk.bytes,File);
            break;

         case MATAMBIENT: // This holds the R G B color of our object
#ifdef DEBUG
            printf("(DEBUG) Model3DS_ChunkProcessNextMaterial: MATAMBIENT\n");
#endif
            Model3DS_ChunkReadColor(File,Model->Mt[Model->NMt-1].Amb,&currentChunk);
            break;

         case MATDIFFUSE: // This holds the R G B color of our object
#ifdef DEBUG
            printf("(DEBUG) Model3DS_ChunkProcessNextMaterial: MATDIFFUSE\n");
#endif
            Model3DS_ChunkReadColor(File,Model->Mt[Model->NMt-1].Dif,&currentChunk);
            break;

          case MATSPECULAR: // This holds the R G B color of our object
#ifdef DEBUG
            printf("(DEBUG) Model3DS_ChunkProcessNextMaterial: MATSPECULAR\n");
#endif
            Model3DS_ChunkReadColor(File,Model->Mt[Model->NMt-1].Spe,&currentChunk);
            break;

         case MATSHININESS: // This holds the R G B color of our object
#ifdef DEBUG
            printf("(DEBUG) Model3DS_ChunkProcessNextMaterial: MATSHININESS\n");
#endif
            Model3DS_ChunkReadColor(File,&(Model->Mt[Model->NMt-1].Shi),&currentChunk);
            break;

         case MATMAP: // This is the header for the texture info
#ifdef DEBUG
            printf("(DEBUG) Model3DS_ChunkProcessNextMaterial: MATMAP\n");
#endif
           // Proceed to read in the material information
            Model3DS_ChunkProcessNextMaterial(File,Model,&currentChunk);
            break;

         case MATMAPFILE: // This stores the file name of the material
#ifdef DEBUG
            printf("(DEBUG) Model3DS_ChunkProcessNextMaterial: MATMAPFILE\n");
#endif
            // Here we read in the material's file name
            currentChunk.bytes+=fread(Model->Mt[Model->NMt-1].Path,1,currentChunk.length-currentChunk.bytes,File);
            break;

         default:
#ifdef DEBUG
            printf("(DEBUG) Model3DS_ChunkProcessNextMaterial: OTHER\n");
#endif
            // Read past the ignored or unknown chunks
            fseek(File,currentChunk.length-currentChunk.bytes,SEEK_CUR);
            currentChunk.bytes+=currentChunk.length-currentChunk.bytes;
            break;
      }

      // Add the bytes read from the last chunk to the previous chunk passed in.
      PreviousChunk->bytes+=currentChunk.bytes;
   }
}
