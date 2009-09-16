ARCH        = $(shell uname)

# Source in configuration

include ../Makefile.$(ARCH)

# Makefile Specific definitions

INCLUDES  = $(INCLUDE)
CFLAGS    = $(CDEBUGFLAGS) $(CCOPTIONS) $(INCLUDES) $(LIBS) $(PROTO) $(DEFINES)
LIBGL     = -L$(GL_PATH)/lib -lGL -lGLU
LIBTK     = -L$(TK_PATH)/lib -ltk$(TCLTK_BASE)
LIBTCL    = -L$(TCL_PATH)/lib -ltcl$(TCLTK_BASE)

# Start of Makefile

OBJ         = tkglCanvas.o tkglCanvLine.o tkglCanvBmap.o tkglCanvPoly.o tkglRectOval.o tkglCanvArc.o tkglCanvText.o tkglCanvImg.o tkCanvWind.o tkCanvPs.o tkCanvUtil.o\
	glFont.o glStuff.o glShader.o tr.o

all:$(OBJ)

link:
	$(LD) -o libTkglCanvas-$(TCLTK_VER).so $(OBJ) $(LIBGL) $(LIBTK)
	cp libTkglCanvas-$(TCLTK_VER).so $(INSTALL_PATH)
	rm -f libTkglCanvas.so $(INSTALL_PATH)/libTkglCanvas.so
	ln -s libTkglCanvas-$(TCLTK_VER).so libTkglCanvas.so
	ln -s libTkglCanvas-$(TCLTK_VER).so $(INSTALL_PATH)/libTkglCanvas.so

install: all link

clean:
	-rm -f *.o *~ *.lo *.la *.so

