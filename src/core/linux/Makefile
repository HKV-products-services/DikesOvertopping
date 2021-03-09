FCOMP = ifort
FCFLAGS = -O2 -fpp -fPIC
PROGRAM =  libDikesOvertopping.so
SRCS = \
precision.f90 \
utilities.f90 \
feedback_parameters.f90 \
errorMessages.f90 \
equalReals.f90 \
vectorUtilities.f90 \
versionInfo.f90 \
typeDefinitionsOvertopping.f90\
OvertoppingMessages.f90 \
ModuleLogging.f90 \
formulaModuleOvertopping.f90 \
overtoppingInterface.f90 \
geometryModuleOvertopping.f90 \
factorModuleOvertopping.f90 \
waveRunup.f90 \
mainModuleOvertopping.f90 \
zFunctionsOvertopping.f90 \
omkeerVariantModule.f90 \
dllOvertopping.f90

OBJECTS = $(SRCS:.f90=.o)

all: $(PROGRAM)

$(PROGRAM): $(OBJECTS)
	$(FCOMP) -shared $(FCFLAGS) -o $@ $^ $(LDFLAGS)
	

%.o: %.f90
	$(FCOMP) $(FCFLAGS) -c $< 

.PHONY: clean veryclean

clean:
	rm -f *.o *.mod *.MOD

veryclean: clean
	rm -f *~ $(PROGRAM)
