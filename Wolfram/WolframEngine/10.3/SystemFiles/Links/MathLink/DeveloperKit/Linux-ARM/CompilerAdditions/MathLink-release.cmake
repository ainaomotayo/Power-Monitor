#----------------------------------------------------------------
# Generated CMake target import file for configuration "Release".
#----------------------------------------------------------------

# Commands may need to know the format version.
SET(CMAKE_IMPORT_FILE_VERSION 1)

# Make sure the targets which have been exported in some other 
# export set exist.

# Import target "ML32i4" for configuration "Release"
SET_PROPERTY(TARGET ML32i4 APPEND PROPERTY IMPORTED_CONFIGURATIONS RELEASE)
SET_TARGET_PROPERTIES(ML32i4 PROPERTIES
  IMPORTED_LINK_INTERFACE_LANGUAGES_RELEASE "CXX"
  IMPORTED_LOCATION_RELEASE "/Developer/jenkins/workspace/MathLink.Linux-ARM/Files/CompilerAdditions/libML32i4.a"
  )

LIST(APPEND _IMPORT_CHECK_TARGETS ML32i4 )
LIST(APPEND _IMPORT_CHECK_FILES_FOR_ML32i4 "/Developer/jenkins/workspace/MathLink.Linux-ARM/Files/CompilerAdditions/libML32i4.a" )

# Make sure the targets which have been exported in some other 
# export set exist.

# Import target "ML32i3" for configuration "Release"
SET_PROPERTY(TARGET ML32i3 APPEND PROPERTY IMPORTED_CONFIGURATIONS RELEASE)
SET_TARGET_PROPERTIES(ML32i3 PROPERTIES
  IMPORTED_LINK_INTERFACE_LANGUAGES_RELEASE "CXX"
  IMPORTED_LOCATION_RELEASE "/Developer/jenkins/workspace/MathLink.Linux-ARM/Files/CompilerAdditions/libML32i3.a"
  )

LIST(APPEND _IMPORT_CHECK_TARGETS ML32i3 )
LIST(APPEND _IMPORT_CHECK_FILES_FOR_ML32i3 "/Developer/jenkins/workspace/MathLink.Linux-ARM/Files/CompilerAdditions/libML32i3.a" )

# Make sure the targets which have been exported in some other 
# export set exist.

# Import target "ML32i4dyn" for configuration "Release"
SET_PROPERTY(TARGET ML32i4dyn APPEND PROPERTY IMPORTED_CONFIGURATIONS RELEASE)
SET_TARGET_PROPERTIES(ML32i4dyn PROPERTIES
  IMPORTED_LOCATION_RELEASE "/Developer/jenkins/workspace/MathLink.Linux-ARM/Files/CompilerAdditions/libML32i4.so"
  IMPORTED_SONAME_RELEASE "libML32i4.so"
  )

LIST(APPEND _IMPORT_CHECK_TARGETS ML32i4dyn )
LIST(APPEND _IMPORT_CHECK_FILES_FOR_ML32i4dyn "/Developer/jenkins/workspace/MathLink.Linux-ARM/Files/CompilerAdditions/libML32i4.so" )

# Make sure the targets which have been exported in some other 
# export set exist.

# Import target "ML32i3dyn" for configuration "Release"
SET_PROPERTY(TARGET ML32i3dyn APPEND PROPERTY IMPORTED_CONFIGURATIONS RELEASE)
SET_TARGET_PROPERTIES(ML32i3dyn PROPERTIES
  IMPORTED_LOCATION_RELEASE "/Developer/jenkins/workspace/MathLink.Linux-ARM/Files/CompilerAdditions/libML32i3.so"
  IMPORTED_SONAME_RELEASE "libML32i3.so"
  )

LIST(APPEND _IMPORT_CHECK_TARGETS ML32i3dyn )
LIST(APPEND _IMPORT_CHECK_FILES_FOR_ML32i3dyn "/Developer/jenkins/workspace/MathLink.Linux-ARM/Files/CompilerAdditions/libML32i3.so" )

# Loop over all imported files and verify that they actually exist
FOREACH(target ${_IMPORT_CHECK_TARGETS} )
  FOREACH(file ${_IMPORT_CHECK_FILES_FOR_${target}} )
    IF(NOT EXISTS "${file}" )
      MESSAGE(FATAL_ERROR "The imported target \"${target}\" references the file
   \"${file}\"
but this file does not exist.  Possible reasons include:
* The file was deleted, renamed, or moved to another location.
* An install or uninstall procedure did not complete successfully.
* The installation package was faulty and contained
   \"${CMAKE_CURRENT_LIST_FILE}\"
but not all the files it references.
")
    ENDIF()
  ENDFOREACH()
  UNSET(_IMPORT_CHECK_FILES_FOR_${target})
ENDFOREACH()
UNSET(_IMPORT_CHECK_TARGETS)

# Commands beyond this point should not need to know the version.
SET(CMAKE_IMPORT_FILE_VERSION)
