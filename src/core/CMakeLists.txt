# set minimum cmake version
cmake_minimum_required(VERSION 3.5 FATAL_ERROR)

# project name and language
project(DikesOvertopping LANGUAGES Fortran)

#set(CMAKE_WINDOWS_EXPORT_ALL_SYMBOLS ON)

set(CMAKE_Fortran_FLAGS "-g -cpp -ffree-line-length-none -fopenmp -fPIC -fcray-pointer ")

add_compile_definitions(maxParallelThreads=8)

add_library(HRtools
  STATIC
    external/tools/precision.f90
    external/tools/utilities.f90
    external/tools/errorMessages.f90
    external/tools/equalReals.f90
    linux/versionInfo.f90
    external/general/vectorUtilities.f90
    external/general/waveParametersUtilities.f90
    external/general/angleUtilities.f90
    external/ftnunit/ftnunit.f90  
    external/ftnunit/ftnunit_hooks_teamcity.f90  
)

set_target_properties(HRtools
    PROPERTIES
      POSITION_INDEPENDENT_CODE 1
)

add_library(Feedback
  STATIC
    external/feedback/feedback.f90
)

set_target_properties(Feedback
    PROPERTIES
      POSITION_INDEPENDENT_CODE 1
)

target_link_libraries(Feedback HRtools)

add_library(FeedbackDll
  SHARED
    external/feedback_dll/feedback_parameters.f90
    external/feedback_dll/feedbackDLL_implementation.f90
    external/feedback_dll/feedbackDLL.f90
)

set_target_properties(FeedbackDll
    PROPERTIES
      POSITION_INDEPENDENT_CODE 1
)

target_link_libraries(FeedbackDll HRtools)

add_library(DikesOvertoppingLib
  STATIC
    libDikesOvertopping/factorModuleOvertopping.f90
    libDikesOvertopping/mainModuleOvertopping.f90
    libDikesOvertopping/OvertoppingMessages.f90
    libDikesOvertopping/formulaModuleOvertopping.f90
    libDikesOvertopping/ModuleLogging.f90
    libDikesOvertopping/typeDefinitionsOvertopping.f90
    libDikesOvertopping/geometryModuleOvertopping.f90
    libDikesOvertopping/omkeerVariantModule.f90
    libDikesOvertopping/waveRunup.f90
    libDikesOvertopping/overtoppingInterface.f90
    libDikesOvertopping/zFunctionsOvertopping.f90
)

set_target_properties(DikesOvertoppingLib
    PROPERTIES
      POSITION_INDEPENDENT_CODE 1
)

target_link_libraries(DikesOvertoppingLib FeedbackDll Feedback)

add_library(DllDikesOvertopping
  SHARED
    dllDikesOvertopping/dllOvertopping.f90
)

set_target_properties(DllDikesOvertopping
    PROPERTIES
      POSITION_INDEPENDENT_CODE 1
)

target_link_libraries(DllDikesOvertopping DikesOvertoppingLib FeedbackDll Feedback HRtools)

add_executable(DikesOvertoppingUnitTest
    tests/DikesOvertoppingTests/crossSectionsTests.f90
    tests/DikesOvertoppingTests/overtoppingTests.f90
    tests/DikesOvertoppingTests/testHelper.f90
    tests/DikesOvertoppingTests/loadTests.f90
    tests/DikesOvertoppingTests/overtoppingUnitTests.f90
    tests/DikesOvertoppingTests/omkeerVariantTests.f90
    tests/DikesOvertoppingTests/readCrossSectionForTests.f90
    tests/DikesOvertoppingTests/roughnessTests.f90
    tests/unitTests/unitTestsProgram.F90
    tests/linux/throwexception.f90
)

target_link_libraries(DikesOvertoppingUnitTest DllDikesOvertopping DikesOvertoppingLib HRtools FeedbackDll Feedback)
