#!/bin/sh

cp ../../external/tools/precision.f90 .
cp ../../external/tools/utilities.f90 .
cp ../../external/feedback/feedback_parameters.f90 .
cp ../../external/tools/errorMessages.f90 .
cp ../../external/tools/equalReals.f90 .
# cp ../../external/general/vectorUtilities.f90 .
# cp ../../external/general/versionInfo.f90 .
cp ../../external/general/waveParametersUtilities.f90 .
cp ../../external/general/angleUtilities.f90 .
cp ../../external/ftnunit/ftnunit_hooks.f90 .
cp ../../external/ftnunit/ftnunit.f90 .
cp ../../libDikesOvertopping/typeDefinitionsOvertopping.f90 .
cp ../../libDikesOvertopping/OvertoppingMessages.f90 .
cp ../../libDikesOvertopping/ModuleLogging.f90 .
cp ../../libDikesOvertopping/formulaModuleOvertopping.f90 .
cp ../../libDikesOvertopping/overtoppingInterface.f90 .
cp ../../libDikesOvertopping/geometryModuleOvertopping.f90 .
cp ../../libDikesOvertopping/factorModuleOvertopping.f90 .
cp ../../libDikesOvertopping/waveRunup.f90 .
cp ../../libDikesOvertopping/mainModuleOvertopping.f90 .
cp ../../libDikesOvertopping/zFunctionsOvertopping.f90 .
cp ../../libDikesOvertopping/omkeerVariantModule.f90 .
# cp ../../dllDikesOvertopping/dllOvertopping.f90 .

cp ../unitTests/unitTestsProgram.F90 .

cp ../DikesOvertoppingTests/crossSectionsTests.f90 .
cp ../DikesOvertoppingTests/loadTests.f90 .
cp ../DikesOvertoppingTests/readCrossSectionForTests.f90 .
cp ../DikesOvertoppingTests/omkeerVariantTests.f90 .
cp ../DikesOvertoppingTests/roughnessTests.f90 .
cp ../DikesOvertoppingTests/overtoppingTests.f90 .
cp ../DikesOvertoppingTests/testHelper.f90 .
cp ../DikesOvertoppingTests/overtoppingUnitTests.f90 .

