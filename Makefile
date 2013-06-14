SHELL = tcsh
FC = gfortran
FFLAGS = -O
# Linux
#LIBS += -lm -lpgplot -lX11 -lcfitsio
# Mac OS x
LIBS = -ldl -lm -lX11 -L/opt/local/lib -lpgplot /opt/local/lib/libcfitsio.a
TARGET = ${HOME}/bin/findabs

OBJECTS = findabs.o ratomdat.o rqsodata.o scanz.o ewregion.o plotdat.o transname.o wrizabs.o wchunks.o findolddirs.o scanzobs.o getlen.o rchar.o rdble.o rUVESpoplerFITS.o sepchar.o srchdblearr.o srchdbletol.o strstr.o number.o rounddble.o

findabs: $(OBJECTS)
	 $(FC) $(FFLAGS) -o $(TARGET) $(OBJECTS) $(LIBS)

clean:	
	rm -f *.o *~

findabs.o: findabs.f findabs_inc.f charlen_inc.f ratomdat_inc.f

ratomdat.o: ratomdat.f findabs_inc.f charlen_inc.f ratomdat_inc.f

rqsodata.o: rqsodata.f findabs_inc.f charlen_inc.f

sepchar.o: sepchar.f charlen_inc.f

scanz.o: scanz.f findabs_inc.f charlen_inc.f constants_inc.f ratomdat_inc.f

plotdat.o: plotdat.f findabs_inc.f charlen_inc.f constants_inc.f ratomdat_inc.f

transname.o: transname.f charlen_inc.f ratomdat_inc.f

wrizabs.o: wrizabs.f findabs_inc.f charlen_inc.f constants_inc.f ratomdat_inc.f

findolddirs.o: findolddirs.f findabs_inc.f charlen_inc.f

wchunks.o: wchunks.f findabs_inc.f charlen_inc.f constants_inc.f ratomdat_inc.f

rounddble.o: rounddble.f charlen_inc.f

scanzobs.o: scanzobs.f findabs_inc.f charlen_inc.f constants_inc.f ratomdat_inc.f

