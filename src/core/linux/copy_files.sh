#!/bin/sh

cp ../external/HRcommon_src/tools/precision.f90 .
cp ../external/HRcommon_src/tools/utilities.f90 .
cp ../external/HRcommon_src/feedback/feedback.f90 .
cp ../external/HRcommon_src/feedback_dll/feedback_parameters.f90 .
cp ../external/HRcommon_src/feedback_dll/feedbackDLL.f90 .
cp ../external/HRcommon_src/feedback_dll/feedbackDLL_implementation.f90 .
cp ../external/HRcommon_src/tools/errorMessages.f90 .
cp ../external/HRcommon_src/tools/equalReals.f90 .
cp ../external/HRcommon_src/general/vectorUtilities.f90 .
# cp ../external/HRcommon_src/general/versionInfo.f90 .
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

if [ -d /opt/apps/intel/18.0.3 ]; then
   cp /opt/apps/intel/18.0.3/compilers_and_libraries_2018.3.222/linux/compiler/lib/intel64_lin/libifport.so.5 .
   cp /opt/apps/intel/18.0.3/compilers_and_libraries_2018.3.222/linux/compiler/lib/intel64_lin/libifcoremt.so.5 .
   cp /opt/apps/intel/18.0.3/compilers_and_libraries_2018.3.222/linux/compiler/lib/intel64_lin/libimf.so .
   cp /opt/apps/intel/18.0.3/compilers_and_libraries_2018.3.222/linux/compiler/lib/intel64_lin/libsvml.so .
   cp /opt/apps/intel/18.0.3/compilers_and_libraries_2018.3.222/linux/compiler/lib/intel64_lin/libintlc.so.5 .
fi

if [ -f /opt/apps/gcc/10.1.0/lib64/libgfortran.so.5 ]; then
   cp /opt/apps/gcc/10.1.0/lib64/libgfortran.so.5 .
fi

