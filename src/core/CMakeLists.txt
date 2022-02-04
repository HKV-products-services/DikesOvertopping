# set minimum cmake version
cmake_minimum_required(VERSION 3.5 FATAL_ERROR)

# project name and language
project(DikesOvertopping LANGUAGES Fortran)

#set(CMAKE_WINDOWS_EXPORT_ALL_SYMBOLS ON)

if(WIN32)
    set(openmp_flag /Qopenmp)
endif(WIN32)
if(UNIX)
    if(CMAKE_Fortran_COMPILER_ID MATCHES "GNU")
        set(CMAKE_Fortran_FLAGS "-O2 -cpp -ffree-line-length-none -fopenmp -fPIC -fcray-pointer ")
    endif()
    if(CMAKE_Fortran_COMPILER_ID MATCHES "Intel")
        set(CMAKE_Fortran_FLAGS "-O2 -fpp -qopenmp ")
    endif()
endif(UNIX)

add_subdirectory(external)

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

add_library(DikesOvertopping
  SHARED
    dllDikesOvertopping/dllOvertopping.f90
)

set_target_properties(DikesOvertopping
    PROPERTIES
      POSITION_INDEPENDENT_CODE 1
)

target_link_libraries(DikesOvertopping DikesOvertoppingLib FeedbackDll Feedback HRtools)

add_executable(unitTestProgram
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

target_link_libraries(unitTestProgram DikesOvertoppingLib HRtools FeedbackDll Feedback)

enable_testing()

add_test(
  NAME overtopping-unit-tests
  COMMAND $<TARGET_FILE:unitTestProgram> /s 1
  WORKING_DIRECTORY ../tests/unitTests
)

