#!/bin/sh

cp ../external/tools/precision.f90 .
cp ../external/tools/utilities.f90 .
cp ../external/feedback/feedback_parameters.f90 .
cp ../external/tools/errorMessages.f90 .
cp ../external/tools/equalReals.f90 .
# cp ../external/general/vectorUtilities.f90 .
# cp ../external/general/versionInfo.f90 .
cp ../libDikesOvertopping/typeDefinitionsOvertopping.f90 .
cp ../libDikesOvertopping/OvertoppingMessages.f90 .
cp ../libDikesOvertopping/ModuleLogging.f90 .
cp ../libDikesOvertopping/formulaModuleOvertopping.f90 .
cp ../libDikesOvertopping/overtoppingInterface.f90 .
cp ../libDikesOvertopping/geometryModuleOvertopping.f90 .
cp ../libDikesOvertopping/factorModuleOvertopping.f90 .
cp ../libDikesOvertopping/waveRunup.f90 .
cp ../libDikesOvertopping/mainModuleOvertopping.f90 .
cp ../libDikesOvertopping/zFunctionsOvertopping.f90 .
cp ../libDikesOvertopping/omkeerVariantModule.f90 .
cp ../dllDikesOvertopping/dllOvertopping.f90 .

cp /opt/apps/intel/18.0.3/compilers_and_libraries_2018.3.222/linux/compiler/lib/intel64_lin/libifport.so.5 .
cp /opt/apps/intel/18.0.3/compilers_and_libraries_2018.3.222/linux/compiler/lib/intel64_lin/libifcoremt.so.5 .
cp /opt/apps/intel/18.0.3/compilers_and_libraries_2018.3.222/linux/compiler/lib/intel64_lin/libimf.so .
cp /opt/apps/intel/18.0.3/compilers_and_libraries_2018.3.222/linux/compiler/lib/intel64_lin/libsvml.so .
cp /opt/apps/intel/18.0.3/compilers_and_libraries_2018.3.222/linux/compiler/lib/intel64_lin/libintlc.so.5 .

