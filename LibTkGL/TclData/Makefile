ARCH        = $(shell uname)

# Source in configuration

include ../Makefile.$(ARCH)

# Makefile Specific definitions

GRAMMAR = Data_Calc

INCLUDES   = $(INCLUDE)
CFLAGS     = $(CDEBUGFLAGS) $(CCOPTIONS) $(INCLUDES) $(PROTO) $(DEFINES)
LDFLAGS    = $(LDDEBUGFLAGS) 
#LIBS       = -L$(LIB_PATH) -lgdal -lproj -lrmn -lgrib_api -lecbufr -lflt -leerUtils -lc -lexpat -lmut -ldrp -lurp
LIBS       = -L$(LIB_PATH) -lgdal -lproj -lrmn -lgrib_api -lecbufr -lflt -leerUtils -lc -lexpat
LIBTCLTK   = -L$(TK_PATH)/lib -ltk$(TCLTK_BASE) -L$(TCL_PATH)/lib -ltcl$(TCLTK_BASE)
LEXFLAGS   =
YACCFLAGS  =
OBJ = $(GRAMMAR).tab.o lex.yy.o $(GRAMMAR).o gpc.o \
	Data_Funcs.o Data_Matrix.o Data_FF.o Data_Render.o Data_RenderShader.o Vertex.o \
	GeoTex.o GeoRef.o GeoRef_RPN.o GeoRef_WKT.o GeoRef_RDR.o \
	tclVector.o tclDataDef.o tclDataSpec.o tclData.o tclCMap.o tclTraj.o tclObs.o \
	tclFSTD.o FSTD_Field.o FSTD_Export.o \
	tclGDAL.o GDAL_Band.o tclOGR.o OGR_Layer.o OGR_Geometry.o OGR_Math.o \
        tcl3DModel.o tcl3DModelMDL.o tcl3DModel3DS.o tcl3DModelFLT.o tcl3DModelDAE.o tcl3DModelKML.o \
	tclRADAR.o Radar_Scan.o tclMetObs.o tclMetModel.o tclMetDataset.o \
	tclGRIB.o GRIB_Field.o


all: $(OBJ)

link:
	$(LD) -o libTclData-$(LIB_VER).so $(OBJ) $(LDFLAGS) $(LIBS) $(LIBTCLTK)
	cp libTclData-$(LIB_VER).so $(LIB_PATH)
	rm -f libTclData.so $(LIB_PATH)/libTclData.so
	ln -s libTclData-$(LIB_VER).so libTclData.so
	ln -s libTclData-$(LIB_VER).so $(LIB_PATH)/libTclData.so

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

