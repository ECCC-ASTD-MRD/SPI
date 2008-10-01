ARCH        = $(shell uname)

# Source in configuration

include ../Makefile.$(ARCH)

# Makefile Specific definitions

GRAMMAR = Data_Calc

INCLUDES  = $(INCLUDE)
LIBS      = -L$(INSTALL_PATH)/Shared/$(ARCH) -L$(EER_PATH)/lib/$(ARCH) -leerUtils -lgdal -lproj -lrmn -L$(URP_PATH)/lib -lmut -ldrp -lurp -L$(GRIB_PATH)/lib -lgrib_api -L$(BUFR_PATH)/lib -lecbufr -L$(FLT_PATH)/lib -lflt -lc
LIBTK     = -L$(TK_PATH)/lib -ltk8.4
LIBTCL    = -L$(TCL_PATH)/lib -ltcl8.4
LDFLAGS   = $(LIBS) $(LIBTCL) $(LIBTK) --warn-once --no-undefined 
LDFLAGS   = $(LIBS) $(LIBTCL) $(LIBTK)
CFLAGS    = $(CDEBUGFLAGS) $(CCOPTIONS) $(INCLUDES) $(PROTO) $(DEFINES)
LEXFLAGS  =
YACCFLAGS =

# Start of Makefile

OBJ = $(GRAMMAR).tab.o lex.yy.o $(GRAMMAR).o \
	Data_Funcs.o Data_Matrix.o Data_FF.o Data_Render.o Data_RenderShader.o \
	../Utils/Vector.o ../Utils/gpc.o tclUtils.o Vertex.o  \
	GeoTex.o GeoRef.o GeoRef_RPN.o GeoRef_WKT.o GeoRef_RDR.o \
	tclVector.o tclDataSpec.o tclData.o tclCMap.o tclTraj.o tclObs.o \
	tclFSTD.o FSTD_Field.o FSTD_Export.o \
	tclGDAL.o GDAL_Band.o tcl3DModel.o  tcl3DModel3DS.o tcl3DModelFLT.o \
	tclOGR.o OGR_Layer.o OGR_Geometry.o OGR_Math.o \
	tclRADAR.o Radar_Scan.o tclMetObs.o tclMetModel.o tclMetDataset.o tclGRIB.o GRIB_Field.o


all: $(OBJ)

link:
	$(LD) -o libTclData-$(LIB_VER).so $(OBJ) $(LDFLAGS)
	cp libTclData-$(LIB_VER).so $(INSTALL_PATH)/Shared/$(ARCH)
	rm -f libTclData.so $(INSTALL_PATH)/Shared/$(ARCH)/libTclData.so
	ln -s libTclData-$(LIB_VER).so libTclData.so
	ln -s libTclData-$(LIB_VER).so $(INSTALL_PATH)/Shared/$(ARCH)/libTclData.so

install: all link

clean:
	rm -Rf \
	lex.yy.c \
	$(GRAMMAR).tab.c $(GRAMMAR).tab.h $(GRAMMAR).output \
	*.o *.so $(ALL)


# Lexer parts

# Kill built-in rules
%.c: %.l
.l.c:

lex.yy.o: lex.yy.c $(GRAMMAR).tab.h

lex.yy.c: $(GRAMMAR).l
	$(LEX) $(LEXFLAGS) $(GRAMMAR).l

# Parser parts

# Kill built-in rules
%.c: %.y
.y.c:

$(GRAMMAR).c: $(GRAMMAR).o

$(GRAMMAR).tab.o: $(GRAMMAR).tab.c $(GRAMMAR).tab.h

$(GRAMMAR).tab.h: $(GRAMMAR).y
	$(YACC) $(YACCFLAGS) --defines $(GRAMMAR).y

$(GRAMMAR).tab.c: $(GRAMMAR).y
	$(YACC) $(YACCFLAGS) $(GRAMMAR).y

