####################################################################################################
# Usage                                                                                            #
####################################################################################################
#
# CMake configures the Visual Studio solution and project files based on this CMakeLists.txt.
#
# To generate these build files, run:
#
#     cmake -A <platform> -DDOTNET_FRAMEWORK=<.NET version> <directory of this file>
#
# in a directory where you want to generate the build files. Here,
#     <platform>                hardware platform for which to compile, use "Win32" or "x64", if not
#                               specified, the default is "Win32".
#     <directory of this file>  path to the directory where this CMakeLists.txt file resides.
#     <.NET version>            version of the .NET framework, use "2.0", "4.0", "4.5", or "4.5.1".
#                               If not specified, the default is "4.0".
#
# CMake configures the build for each platform separately, but generates three different build
# configurations for all projects: "Debug" and "Release". "Debug" builds include debug information
# and are non-optimised, while "Release" is an optimised build.
#

####################################################################################################
# Helper function definitions                                                                      #
####################################################################################################

# Gathers a list of sources from a directory (non-recursively) based on a pattern.
#
# Options:
#     SKIP: Do not add files specified in the FILES argument.
#     ONLY: Add only the files specified in the FILES argument.
#
# One-value arguments:
#     SRCDIR: Source directory to check in.
#     PATTERN: Glob-pattern to select source files with.
#
# Multi-value arguments:
#     FILES: Files to be skipped or selected (see options).
#
# This function returns a list of sources by setting the LISTSRC_SOURCES variable in the caller's
# scope.
function(list_sources)
    cmake_parse_arguments(
        LISTSRC
        "SKIP;ONLY"
        "SRCDIR;PATTERN"
        "FILES"
        ${ARGN}
    )

    # Find all sources that match the glob-pattern in the search directory. Note: this function
    # searches non-recursively.
    file(GLOB allSourceFiles "${LISTSRC_SRCDIR}/${LISTSRC_PATTERN}")
    set(sources "")
    foreach(sourceFile ${allSourceFiles})
        if (LISTSRC_SKIP OR LISTSRC_ONLY)
            # If we're not an exception, we need to add the file only if we're doing SKIP.
            set(doAdd ${LISTSRC_SKIP})

            foreach (exceptionFile ${LISTSRC_FILES})
                if ("${sourceFile}" MATCHES "${exceptionFile}")
                    # If we're in an exception and we're not doing ONLY, we're doing SKIP, so we do
                    # not add this file.
                    set(doAdd ${LISTSRC_ONLY})
                    break()
                endif()
            endforeach()
        else()
            set(doAdd TRUE)
        endif()

        if (${doAdd})
            list(APPEND sources ${sourceFile})
        endif()
    endforeach()
    # Set output list of sources to variable in the caller's scope.
    set(LISTSRC_SOURCES ${sources} PARENT_SCOPE)
endfunction()

# Gathers a list of source and resource files from a directory (non-recursively) based on a pattern.
#
# The source files are gathered according to the specified glob. Resources files are gather based on
# their .rc.svn extension; the file generated from this file is included in the sources.
#
# One-value arguments:
#     SRCDIR: Source directory to check in.
#     PATTERN: Glob-pattern to select source files with.
#
# Multi-value arguments:
#     SKIP: Do not add these specified files.
#     ONLY: Only add these specified files.
#
# This function returns a list of sources and resources by setting respectively the GETSRC_SOURCES
# and GETSRC_RESOURCES variables in the caller's scope.
function(get_source_files)
    cmake_parse_arguments(
        GETSRC
        ""
        "SRCDIR;PATTERN"
        "SKIP;ONLY"
        ${ARGN}
    )

    # Gather source files and resource files from the specified directory.
    if (GETSRC_SKIP)
        list_sources(
            SRCDIR "${GETSRC_SRCDIR}"
            PATTERN "${GETSRC_PATTERN}"
            FILES ${GETSRC_SKIP} SKIP
        )
    elseif (GETSRC_ONLY)
        list_sources(
            SRCDIR "${GETSRC_SRCDIR}"
            PATTERN "${GETSRC_PATTERN}"
            FILES ${GETSRC_ONLY} ONLY
        )
    else()
        list_sources(
            SRCDIR "${GETSRC_SRCDIR}"
            PATTERN "${GETSRC_PATTERN}"
        )
    endif()
    set(sources ${LISTSRC_SOURCES})

    # Gather resources files---only if we specify SKIP, we need to use the function that respects
    # skipping files, otherwise, we gather all *.rc.svn files (also for ONLY).
    if (GETSRC_SKIP)
        list_sources(
            SRCDIR "${GETSRC_SRCDIR}"
            PATTERN "*.rc.svn"
            FILES ${GETSRC_SKIP} SKIP
        )
    else()
        list_sources(
            SRCDIR "${CMAKE_CURRENT_SOURCE_DIR}/${GETSRC_SRCDIR}"
            PATTERN "*.rc.svn"
        )
    endif()
    set(resources ${LISTSRC_SOURCES})

    # Add the source files created from the resource files to the sources.
    set(resourcesOut "")
    foreach(resource ${resources})
        # Get the name of the resource file without the .svn extension and add it to the sources.
        # Make sure it exists by creating an empty file at configure time.
        get_filename_component(filename "${resource}" NAME_WLE)
        set(resourceOut "${CMAKE_CURRENT_BINARY_DIR}/${filename}")
        file(TOUCH "${resourceOut}")

        list(APPEND resourcesOut ${resourceOut})
        list(APPEND sources ${resourceOut})
    endforeach()

    set(GETSRC_SOURCES "${sources}" PARENT_SCOPE)
    set(GETSRC_RESOURCES "${resourcesOut}" PARENT_SCOPE)
endfunction()

# Adds a pre-build command to create resources files for the specified target.
#
# This ensures that the git_insert_hash.cmd script is executed on all resource files before the
# build of the specified project is started.
#
# One-value arguments:
#     NAME: Name of the target to generate resources for.
#     SRCDIR: Source directory of the target.
#
# Multi-value arguments:
#     RESOURCES: Resources to be generated for this target. Note: this the required output from the
#         pre-build script.
function(target_create_resources)
    cmake_parse_arguments(
        CREATERC
        ""
        "NAME;SRCDIR"
        "RESOURCES"
        ${ARGN}
    )

    set_target_properties(${CREATERC_NAME} PROPERTIES RESOURCE "${resources}")
    target_include_directories(${CREATERC_NAME} PRIVATE ${CREATERC_SRCDIR})

    # Before building the target with the specified name, run the git_insert_hash.cmd script to
    # insert a revision into the resource.
    foreach (resource ${CREATERC_RESOURCES})
        get_filename_component(filename "${resource}" NAME)
        set(resourceIn "${CMAKE_CURRENT_SOURCE_DIR}/${CREATERC_SRCDIR}/${filename}.svn")

        # Make sure we use backslashes in the path.
        string(REPLACE "/" "\\" resourceIn ${resourceIn})
        string(REPLACE "/" "\\" resourceOut ${resource})
        string(REPLACE "/" "\\" workpath ${CMAKE_CURRENT_SOURCE_DIR}/${CREATERC_SRCDIR})

        add_custom_command(
            TARGET ${CREATERC_NAME} PRE_BUILD
            COMMAND ${CMAKE_CURRENT_SOURCE_DIR}/lib/utils/git_insert_hash.cmd
                "${workpath}" "${resourceIn}" "${resourceOut}.new"
            COMMAND ${CMAKE_COMMAND} -E copy_if_different "${resourceOut}.new" "${resourceOut}"
	        WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
        )
    endforeach()
endfunction()

# Adds a project from Fortran sources.
#
# This will (non-recursively) find all *.f90 files in the specified directory and add them as
# sources to this project.
#
# One of the options STATIC, SHARED, or EXECUTABLE should be set.
#
# Options:
#     STATIC: Create a static library.
#     SHARED: Create a shared library.
#     EXECUTABLE: Create an executable.
#
# One-value arguments:
#     NAME: Project name.
#     PATH: Source directory.
#     FOLDER: Solution folder that the project should be placed in.
#
# Multi-value arguments:
#     SKIP: Do not add the specified files as sources.
#     ONLY: Only add the specified files as sources.
function(add_fortran_project)
    cmake_parse_arguments(
        FORTRAN
        "STATIC;SHARED;EXECUTABLE"
        "NAME;PATH;FOLDER"
        "SKIP;ONLY"
        ${ARGN}
    )
    get_source_files(
        SRCDIR "${FORTRAN_PATH}"
        PATTERN "*.f90"
        SKIP ${FORTRAN_SKIP}
        ONLY ${FORTRAN_ONLY}
    )
    set(sources ${GETSRC_SOURCES})
    set(resources ${GETSRC_RESOURCES})

    if (${FORTRAN_STATIC})
        add_library(${FORTRAN_NAME} STATIC ${sources})
    elseif (${FORTRAN_SHARED})
        add_library(${FORTRAN_NAME} SHARED ${sources})
    elseif (${FORTRAN_EXECUTABLE})
        add_executable(${FORTRAN_NAME} ${sources})
    else()
        message(FATAL_ERROR "Specify either STATIC, SHARED or EXECUTABLE for a Fortran project.")
    endif()

    target_include_directories(${FORTRAN_NAME} PRIVATE "${CMAKE_Fortran_MODULE_DIRECTORY}")
    set_target_properties(${FORTRAN_NAME} PROPERTIES
        Fortran_MODULE_DIRECTORY "${CMAKE_Fortran_MODULE_DIRECTORY}"
        FOLDER "${FORTRAN_FOLDER}"
    )

    target_create_resources(
        NAME ${FORTRAN_NAME}
        SRCDIR ${FORTRAN_PATH}
        RESOURCES ${resources}
    )
endfunction()

# Adds a project from C# sources.
#
# This will (non-recursively) find all *.cs files in the specified directory and add them as
# sources to this project.
#
# One of the options SHARED, or EXECUTABLE should be set.
#
# Options:
#     SHARED: Create a shared library.
#     EXECUTABLE: Create an executable.
#
# One-value arguments:
#     NAME: Project name.
#     PATH: Source directory.
#     FOLDER: Solution folder that the project should be placed in.
#     EMBED_PATTERN: Glob pattern for files to embed.
#     COPY_PATTERN: Glob pattern for files to copy to the output directory after the build has
#         finished.
#
# Multi-value arguments:
#     REFERENCES: Names of the external dependencies to add a reference to.
#     SKIP: Do not add the specified files as sources.
#     ONLY: Only add the specified files as sources.
function(add_csharp_project)
    cmake_parse_arguments(
        CSHARP
        "SHARED;EXECUTABLE"
        "NAME;PATH;FOLDER;EMBED_PATTERN;COPY_PATTERN"
        "REFERENCES;SKIP;ONLY"
        ${ARGN}
    )

    get_source_files(
        SRCDIR "${CSHARP_PATH}"
        PATTERN "*.cs"
        SKIP ${CSHARP_SKIP}
        ONLY ${CSHARP_ONLY}
    )
    set(sources ${GETSRC_SOURCES})
    set(resources ${GETSRC_RESOURCES})

    if ("${CSHARP_EMBED_PATTERN}" STREQUAL "")
        set(embeds "")
    else()
        file(GLOB embeds "${CSHARP_PATH}/${CSHARP_EMBED_PATTERN}")
        set_property(SOURCE ${embeds} PROPERTY VS_TOOL_OVERRIDE "EmbeddedResource")
    endif()

    if (${CSHARP_SHARED})
        add_library(${CSHARP_NAME} SHARED ${sources} ${embeds})
    elseif (${CSHARP_EXECUTABLE})
        add_executable(${CSHARP_NAME} ${sources} ${embeds})
    else()
        message(FATAL_ERROR "Specify either SHARED or EXECUTABLE for a C# project.")
    endif()

    set_target_properties(${CSHARP_NAME} PROPERTIES
        VS_DOTNET_PLATFORM "${Platform}"
        VS_GLOBAL_PreferredToolArchitecture "${Platform}"
        VS_DOTNET_TARGET_FRAMEWORK_VERSION  "v${DOTNET_FRAMEWORK}"
        FOLDER "${CSHARP_FOLDER}"
    )

    target_create_resources(
        NAME ${CSHARP_NAME}
        SRCDIR "${CSHARP_PATH}"
        RESOURCES ${resources}
    )

    # Version information for C# projects is contained in a file AssemblyInfo.cs.svn, on which
    # svn_insert_version.cmd should be called to insert the SVN revision. If it is given only a
    # directory, it will by default process AssemblyInfo.cs.svn
    set(propertiesDir "${CMAKE_CURRENT_SOURCE_DIR}/${CSHARP_PATH}/Properties")
    if (IS_DIRECTORY ${propertiesDir})
        if (EXISTS "${propertiesDir}/AssemblyInfo.cs.svn")
            file(TOUCH "${propertiesDir}/AssemblyInfo.cs")
            target_sources(${CSHARP_NAME} PRIVATE "${propertiesDir}/AssemblyInfo.cs")
            string(REPLACE "/" "\\" scriptPath ${CMAKE_CURRENT_SOURCE_DIR}/lib/utils/git_insert_hash.cmd)
            string(REPLACE "/" "\\" propertiesDirWindows ${propertiesDir})
            string(REPLACE "/" "\\" inputPropertiesFile ${propertiesDir}/AssemblyInfo.cs.svn)
            string(REPLACE "/" "\\" outputPropertiesFile ${propertiesDir}/AssemblyInfo.cs)
            add_custom_command(
                TARGET ${CSHARP_NAME} PRE_BUILD
                COMMAND ${scriptPath} ${propertiesDirWindows} ${inputPropertiesFile} ${outputPropertiesFile}
                WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
            )
        elseif (EXISTS "${propertiesDir}/AssemblyInfo.cs")
            target_sources(${CSHARP_NAME} PRIVATE "${CSHARP_PATH}/Properties/AssemblyInfo.cs")
        endif()
    endif()

    if (EXISTS "${CMAKE_CURRENT_SOURCE_DIR}/src/GlobalAssemblyInfo.cs")
        target_sources(${CSHARP_NAME} PRIVATE "${CMAKE_CURRENT_SOURCE_DIR}/src/GlobalAssemblyInfo.cs")
    else()
        message("Could not find ${CMAKE_CURRENT_SOURCE_DIR}/src/GlobalAssemblyInfo.cs")
    endif()

    set(references "")
    foreach (reference ${CSHARP_REFERENCES})
        if (${reference} STREQUAL "nunit.framework")
            set_target_properties(${CSHARP_NAME} PROPERTIES
                VS_DOTNET_REFERENCE_nunit.framework "${nunit_framework}"
            )
        elseif (${reference} STREQUAL "DelftTools.Utils")
            set_target_properties(${CSHARP_NAME} PROPERTIES
                VS_DOTNET_REFERENCE_DelftTools.Utils "${DelftTools}"
            )
        elseif (${reference} STREQUAL "DeltaShell.Plugins.MorphAn.Domain")
            set_target_properties(${CSHARP_NAME} PROPERTIES
                VS_DOTNET_REFERENCE_DeltaShell.Plugins.MorphAn.Domain "${DeltaShell_DOMAIN}"
            )
        elseif (${reference} STREQUAL "DeltaShell.Plugins.MorphAn.TRDA")
            set_target_properties(${CSHARP_NAME} PROPERTIES
                VS_DOTNET_REFERENCE_DeltaShell.Plugins.MorphAn.TRDA "${DeltaShell_TRDA}"
            )
        elseif (${reference} STREQUAL "PostSharp")
            set_target_properties(${CSHARP_NAME} PROPERTIES
                VS_DOTNET_REFERENCE_PostSharp "${PostSharp}"
            )
        elseif (${reference} STREQUAL "Deltares.Mathematics")
            set_target_properties(${CSHARP_NAME} PROPERTIES
                VS_DOTNET_REFERENCE_Deltares.Mathematics "${Deltares_Mathematics}"
            )
        elseif (${reference} STREQUAL "Deltares.Standard")
            set_target_properties(${CSHARP_NAME} PROPERTIES
                VS_DOTNET_REFERENCE_Deltares.Standard "${Deltares_Standard}"
            )
        elseif (${reference} STREQUAL "Deltares.WTIPiping")
            set_target_properties(${CSHARP_NAME} PROPERTIES
                VS_DOTNET_REFERENCE_Deltares.WTIPiping "${Deltares_WTIPiping}"
            )
        elseif (${reference} STREQUAL "Deltares.WTIStructuralFailure")
            set_target_properties(${CSHARP_NAME} PROPERTIES
                VS_DOTNET_REFERENCE_Deltares.WTIStructuralFailure "${Deltares_WTIStructuralFailure}"
            )
        elseif (${reference} STREQUAL "System.Data.SQLite")
            set_target_properties(${CSHARP_NAME} PROPERTIES
                VS_DOTNET_REFERENCE_System.Data.SQLite "${SQLite}"
            )
        elseif (${reference} STREQUAL "System.Data.SQLite.Linq")
            set_target_properties(${CSHARP_NAME} PROPERTIES
                VS_DOTNET_REFERENCE_System.Data.SQLite.Linq "${SQLite_Linq}"
            )
        else()
            # If we are not one of the "special" reference with a specific path, just append it to
            # the list.
            list(APPEND references ${reference})
        endif()
    endforeach()

    foreach(embed ${embeds})
        string(REPLACE "${CMAKE_CURRENT_SOURCE_DIR}/${CSHARP_PATH}/" "${CSHARP_NAME}/" embedShort ${embed})
        string(REPLACE "/" "\\" embedShort ${embedShort})
        # Setting the undocumented VS_CSHARP_Link property controls under which path the file is
        # embedded.
        set_source_files_properties("${embed}" PROPERTIES
            VS_CSHARP_Link "${embedShort}"
        )
    endforeach()

    if (NOT "${CSHARP_COPY_PATTERN}" STREQUAL "")
        # Add a post-build event to copy the files that match the specified glob to the output
        # directory.
        file(GLOB filesToCopy "${CSHARP_PATH}/${CSHARP_COPY_PATTERN}")
        add_custom_command(
            TARGET ${CSHARP_NAME} POST_BUILD
            COMMENT "Copying ${filesToCopy} to ${outputDir}"
            COMMAND ${CMAKE_COMMAND} -E copy_if_different "${filesToCopy}" "${outputDir}"
        )
    endif()

    set_target_properties(${CSHARP_NAME} PROPERTIES
        VS_DOTNET_REFERENCES "${references}"
    )

    # Make all C# projects dependent on copying the DLLs to the output directory.
    if (TARGET CopyDlls)
        add_dependencies(${CSHARP_NAME} CopyDLLs)
    endif()
endfunction()

# Adds a project from C++ sources.
#
# This will (non-recursively) find all *.cpp files in the specified directory and add them as
# sources to this project.
#
# One of the options STATIC, SHARED, or EXECUTABLE should be set.
#
# Options:
#     STATIC: Create a static library.
#     SHARED: Create a shared library.
#     EXECUTABLE: Create an executable.
#     SHARED_MFC: Whether to use the shared Microsoft Foundation Classes (MFC) library; the static
#         library is used by default.
#     NODOTNET: Compile as native, rather than .NET.
#
# One-value arguments:
#     NAME: Project name.
#     PATH: Source directory.
#     FOLDER: Solution folder that the project should be placed in.
#
# Multi-value arguments:
#     SKIP: Do not add the specified files as sources.
#     ONLY: Only add the specified files as sources.
function(add_cpp_project)
    cmake_parse_arguments(
        CPP
        "STATIC;SHARED;EXECUTABLE;SHARED_MFC;NODOTNET"
        "NAME;PATH;FOLDER"
        "SKIP;ONLY"
        ${ARGN}
    )
    get_source_files(
        SRCDIR "${CPP_PATH}"
        PATTERN "*.cpp"
        SKIP ${CPP_SKIP}
        ONLY ${CPP_ONLY}
    )
    set(sources ${GETSRC_SOURCES})
    set(resources ${GETSRC_RESOURCES})

    if (${CPP_STATIC})
        add_library(${CPP_NAME} STATIC ${sources})
    elseif (${CPP_SHARED})
        add_library(${CPP_NAME} SHARED ${sources})
    elseif (${CPP_EXECUTABLE})
        add_executable(${CPP_NAME} ${sources})
    else()
        message(FATAL_ERROR "Specify either STATIC, SHARED or EXECUTABLE for a C++ project.")
    endif()

    target_create_resources(
        NAME ${CPP_NAME}
        SRCDIR ${CPP_PATH}
        RESOURCES ${resources}
    )

    set_target_properties(${CPP_NAME} PROPERTIES
        FOLDER "${CPP_FOLDER}"
        MSVC_RUNTIME_LIBRARY "MultiThreaded$<$<OR:$<CONFIG:Debug>,$<CONFIG:CodeCoverage>>:Debug>DLL"
    )
    # Make sure we set _DEBUG (i.e. enable asserts) for Debug build, and NDEBUG
    # (i.e. disable asserts) for Release builds.
    target_compile_definitions(${CPP_NAME} PRIVATE
        "$<$<CONFIG:Debug>:_DEBUG>"
        "$<$<CONFIG:Release>:NDEBUG>"
    )
    if (${CPP_SHARED_MFC})
        target_compile_definitions(${CPP_NAME} PRIVATE _AFXDLL)
        set_target_properties(${CPP_NAME} PROPERTIES MFC_FLAG 2)
    endif()
    if (NOT ${CPP_NODOTNET})
        set_target_properties(${CPP_NAME} PROPERTIES
            VS_DOTNET_TARGET_FRAMEWORK_VERSION "v${DOTNET_FRAMEWORK}"
            COMMON_LANGUAGE_RUNTIME ""
        )
    endif()
endfunction()

# Adds an git_insert_hash.cmd pre-build command, formatting paths appropriately for Windows.
#
# One-value arguments:
#     TARGET: Target to associate the pre-build command with.
#     OUT_PATH: Path to run the script in.
#     IN_FILE: File that should be the modified by this command.
#     OUT_FILE: File that should be the result of this command.
#
function(add_update_version_prebuild_command)
    cmake_parse_arguments(
        UPDVER
        ""
        "TARGET;OUT_PATH;IN_FILE;OUT_FILE"
        ""
        ${ARGN}
    )
    string(REPLACE "/" "\\" scriptPath ${CMAKE_CURRENT_SOURCE_DIR}/lib/utils/git_insert_hash.cmd)
    string(REPLACE "/" "\\" workPath ${UPDVER_OUT_PATH})
    string(REPLACE "/" "\\" inputFile ${UPDVER_IN_FILE})
    string(REPLACE "/" "\\" outputFile ${UPDVER_OUT_FILE})
    add_custom_command(
        TARGET ${UPDVER_TARGET} PRE_BUILD
        COMMAND ${scriptPath} ${workPath} ${inputFile} ${outputFile}
        WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
    )
endfunction()

# Enables adding profiling instrumentation to the specified Fortran target
#
# For CodeCoverage builds for Fortran projects, adding profile instrumentation is enabled by
# adding the following compiler flags:
#
#     /Qprof-gen:srcpos;/Qprof-dir:<directory>
#
function(enable_fortran_profile_instrumentation target directory)
    if (CODECOV)
        string(REPLACE "/" "\\" directoryWindows ${directory})
        target_compile_options(${target} PRIVATE
            "$<$<CONFIG:Debug>:/Qprof-gen:srcpos;/Qprof-dir:${directoryWindows}>"
        )
    endif()
endfunction()

