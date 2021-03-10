FCOMP = ifort
FCFLAGS = -O2 -fpp
PROGRAM =  unitTestProgram
SRCS = \
precision.f90 \
utilities.f90 \
feedback_parameters.f90 \
errorMessages.f90 \
equalReals.f90 \
vectorUtilities.f90 \
versionInfo.f90 \
waveParametersUtilities.f90 \
angleUtilities.f90 \
ftnunit_hooks_teamcity.f90 \
ftnunit.f90 \
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
testHelper.f90 \
readCrossSectionForTests.f90 \
loadTests.f90 \
crossSectionsTests.f90 \
omkeerVariantTests.f90 \
roughnessTests.f90 \
overtoppingUnitTests.f90 \
overtoppingTests.f90 \
unitTestsProgram.F90


OBJECTS = $(SRCS:.f90=.o)

all: $(PROGRAM)

$(PROGRAM): $(OBJECTS)
	$(FCOMP) $(FCFLAGS) -o $@ $^ $(LDFLAGS)
	

%.o: %.f90
	$(FCOMP) $(FCFLAGS) -c $< 

.PHONY: clean veryclean

clean:
	rm -f *.o *.mod *.MOD

veryclean: clean
	rm -f *~ $(PROGRAM)
