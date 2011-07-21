ARCH        = $(shell uname)

# Source in configuration

include ../Makefile.$(ARCH)

# Makefile Specific definitions

INCLUDES   = $(X_INCLUDE) $(INCLUDE)
CFLAGS     = $(CDEBUGFLAGS) $(CCOPTIONS) $(INCLUDES) $(PROTO) $(DEFINES)
LDFLAGS    = $(LDDEBUGFLAGS)
LIBS       = -L$(LIB_PATH) -leerUtils -lGL -lGLU  -lTclData -lTkglCanvas -lgdb -lc -lpthread
OBJS       = tkCanvVP.o Projection.o ProjCam.o ProjAzimuth.o ProjCylin.o ProjOrtho.o ProjGrid.o GeoData.o tkCanvColorbar.o tkCanvGraph.o tkGraphTephi.o tkGraphAxis.o tkGraphItem.o

# Start of Makefile

all  : $(OBJS)

link: 
	$(LD) -o libTkViewport-$(LIB_VER).so $(OBJS) $(LDFLAGS) $(LIBS)
	cp libTkViewport-$(LIB_VER).so $(LIB_PATH)
	rm -f libTkViewport.so $(LIB_PATH)/libTkViewport.so
	ln -s libTkViewport-$(LIB_VER).so libTkViewport.so
	ln -s libTkViewport-$(LIB_VER).so $(LIB_PATH)/libTkViewport.so

install: all link

clean:
	-rm -f *.o *~ *.lo *.la *.so
