ARCH        = $(shell uname)

# Source in configuration

include ../Makefile.$(ARCH)

# Makefile Specific definitions

INCLUDES   = $(X_INCLUDE) $(INCLUDE)
CFLAGS     = $(CDEBUGFLAGS) $(CCOPTIONS) $(INCLUDES) $(LIBS) $(PROTO) $(DEFINES)
LIBHM      = -L$(EER_PATH)/lib/$(ARCH) -leerUtils -L$(INSTALL_PATH) -lproj -lGL -lGLU  -lTclData  -lTkglCanvas -L$(GDB_PATH)/lib/$(ARCH) -L$(GDB_PATH)/publib -lgdb -lc
OBJS       = tkCanvVP.o Projection.o ProjCam.o ProjAzimuth.o ProjCylin.o ProjOrtho.o ProjGrid.o GeoData.o tkCanvColorbar.o tkCanvGraph.o tkGraphTephi.o tkGraphAxis.o tkGraphItem.o

# Start of Makefile

all  : $(OBJS)

link: 
	$(LD) -o libTkViewport-$(LIB_VER).so $(OBJS) $(LIBHM)
	cp libTkViewport-$(LIB_VER).so $(INSTALL_PATH)
	rm -f libTkViewport.so $(INSTALL_PATH)/libTkViewport.so
	ln -s libTkViewport-$(LIB_VER).so libTkViewport.so
	ln -s libTkViewport-$(LIB_VER).so $(INSTALL_PATH)/libTkViewport.so

install: all link

clean:
	-rm -f *.o *~ *.lo *.la *.so
