ARCH        = $(shell uname)

# Source in configuration

include ../Makefile.$(ARCH)

# Makefile Specific definitions

INCLUDES   = $(INCLUDE)
CFLAGS     = $(CDEBUGFLAGS) $(CCOPTIONS) $(INCLUDES) $(PROTO) $(DEFINES)
LDFLAGS    = $(LDDEBUGFLAGS)
LIBS       = -L$(GL_PATH)/lib -lGL -lGLU -L$(TK_PATH)/lib -ltk$(TCLTK_BASE) -L$(TCL_PATH)/lib -ltcl$(TCLTK_BASE) 
#LIBS       = -L$(LIB_PATH)/GL -lGL -lGLU -L$(TK_PATH)/lib -ltk$(TCLTK_BASE) -L$(TCL_PATH)/lib -ltcl$(TCLTK_BASE) 
OBJ        = tkglCanvas.o tkglCanvLine.o tkglCanvBmap.o tkglCanvPoly.o tkglRectOval.o tkglCanvArc.o tkglCanvText.o \
	tkglCanvImg.o tkCanvWind.o tkCanvPs.o tkCanvUtil.o glFont.o glStuff.o glShader.o tr.o

all:$(OBJ)

link:
	$(LD) -o libTkglCanvas-$(TCLTK_VER).so $(OBJ) $(LDFLAGS) $(LIBS)
	cp libTkglCanvas-$(TCLTK_VER).so $(LIB_PATH)
	rm -f libTkglCanvas.so $(LIB_PATH)/libTkglCanvas.so
	ln -s libTkglCanvas-$(TCLTK_VER).so libTkglCanvas.so
	ln -s libTkglCanvas-$(TCLTK_VER).so $(LIB_PATH)/libTkglCanvas.so

install: all link

clean:
	-rm -f *.o *~ *.lo *.la *.so

