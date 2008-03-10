#include <strings.h>
#include <stdlib.h>
#include <stdio.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>

#include <glStuff.h>

void GLShader_InfoLog(GLhandleARB Object);
GLint GLShader_UniformGet(GLhandleARB Prog,const GLcharARB *Name);
GLint GLShader_AttribGet(GLhandleARB Prog,const GLcharARB *Name);
GLhandleARB GLShader_Load(GLcharARB *Path,GLcharARB *Name);
GLhandleARB GLShader_Install(GLcharARB *VertSrc,GLcharARB *FragSrc);
void GLShader_UnInstall(GLhandleARB Prog);
