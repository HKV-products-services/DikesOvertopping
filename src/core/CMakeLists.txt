# set minimum cmake version
cmake_minimum_required(VERSION 3.5 FATAL_ERROR)

# project name and language
project(DikesOvertopping LANGUAGES Fortran)

#set(CMAKE_WINDOWS_EXPORT_ALL_SYMBOLS ON)

if(WIN32)
    set(openmp_flag /Qopenmp)
endif(WIN32)
if(UNIX)
    set(CMAKE_Fortran_FLAGS "-g -cpp -ffree-line-length-none -fopenmp -fPIC -fcray-pointer ")
endif(UNIX)

add_compile_definitions(maxParallelThreads=8)

add_library(HRtools
  STATIC
    external/HRcommon_src/tools/precision.f90
    external/HRcommon_src/tools/utilities.f90
    external/HRcommon_src/tools/errorMessages.f90
    external/HRcommon_src/tools/equalReals.f90
)

set_target_properties(HRtools
    PROPERTIES
      POSITION_INDEPENDENT_CODE 1
)

add_library(HRgeneral
  STATIC
    external/versionInfo.f90
    external/HRcommon_src/general/vectorUtilities.f90
    external/HRcommon_src/general/waveParametersUtilities.f90
    external/HRcommon_src/general/angleUtilities.f90
    external/HRcommon_src/ftnunit/ftnunit.f90  
    external/HRcommon_src/ftnunit/ftnunit_hooks_teamcity.f90  
)

target_link_libraries(HRgeneral Feedback)

set_target_properties(HRgeneral
    PROPERTIES
      POSITION_INDEPENDENT_CODE 1
)

add_library(FeedbackDll
  SHARED
    external/HRcommon_src/feedback_dll/feedback_parameters.f90
    external/HRcommon_src/feedback_dll/feedbackDLL_implementation.f90
    external/HRcommon_src/feedback_dll/feedbackDLL.f90
)

set_target_properties(FeedbackDll
    PROPERTIES
      POSITION_INDEPENDENT_CODE 1
)
target_link_libraries(FeedbackDll HRtools)

if(WIN32)
    set_target_properties(FeedbackDll PROPERTIES COMPILE_FLAGS ${openmp_flag})
    target_link_options(FeedbackDll PRIVATE ${openmp_flag})
endif(WIN32)

add_library(Feedback
  STATIC
    external/HRcommon_src/feedback/feedback.f90
)

set_target_properties(Feedback
    PROPERTIES
      POSITION_INDEPENDENT_CODE 1
)

target_link_libraries(Feedback HRtools FeedbackDll)

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

target_link_libraries(DikesOvertoppingLib FeedbackDll Feedback HRgeneral)

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
    external/throwexception.f90
)

target_link_libraries(DikesOvertoppingUnitTest DllDikesOvertopping DikesOvertoppingLib HRtools FeedbackDll Feedback)
