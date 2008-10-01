ARCH        = $(shell uname)

# Source in configuration

include ../Makefile.$(ARCH)

# Makefile Specific definitions

INCLUDES  = $(INCLUDE)
CFLAGS    = $(CDEBUGFLAGS) $(CCOPTIONS) $(INCLUDES) $(LIBS) $(PROTO) $(DEFINES)
LIBGL     = -L$(GL_PATH)/lib -lGL -lGLU
LIBTK     = -L$(TK_PATH)/lib -ltk8.4
LIBTCL    = -L$(TCL_PATH)/lib -ltcl8.4

# Start of Makefile

OBJ         = tkglCanvas.o tkglCanvLine.o tkglCanvBmap.o tkglCanvPoly.o tkglRectOval.o tkglCanvArc.o tkglCanvText.o tkglCanvImg.o tkCanvWind.o tkCanvPs.o tkCanvUtil.o\
	glFont.o glStuff.o glShader.o ../Utils/tr.o

all:$(OBJ)

link:
	$(LD) -o libTkglCanvas-$(TCLTK_VER).so $(OBJ) $(LIBGL) $(LIBTK)
	cp libTkglCanvas-$(TCLTK_VER).so $(INSTALL_PATH)/Shared/$(ARCH)
	rm -f libTkglCanvas.so $(INSTALL_PATH)/Shared/$(ARCH)/libTkglCanvas.so
	ln -s libTkglCanvas-$(TCLTK_VER).so libTkglCanvas.so
	ln -s libTkglCanvas-$(TCLTK_VER).so $(INSTALL_PATH)/Shared/$(ARCH)/libTkglCanvas.so

install: all link

clean:
	-rm -f *.o *~ *.lo *.la *.so

