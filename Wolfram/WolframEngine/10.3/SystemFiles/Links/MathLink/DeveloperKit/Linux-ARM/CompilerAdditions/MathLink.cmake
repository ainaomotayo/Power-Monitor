# Generated by CMake 2.8.10.3

IF("${CMAKE_MAJOR_VERSION}.${CMAKE_MINOR_VERSION}" LESS 2.5)
   MESSAGE(FATAL_ERROR "CMake >= 2.6.0 required")
ENDIF("${CMAKE_MAJOR_VERSION}.${CMAKE_MINOR_VERSION}" LESS 2.5)
CMAKE_POLICY(PUSH)
CMAKE_POLICY(VERSION 2.6)
#----------------------------------------------------------------
# Generated CMake target import file.
#----------------------------------------------------------------

# Commands may need to know the format version.
SET(CMAKE_IMPORT_FILE_VERSION 1)

# Create imported target ML32i4
ADD_LIBRARY(ML32i4 STATIC IMPORTED)

# Create imported target ML32i3
ADD_LIBRARY(ML32i3 STATIC IMPORTED)

# Create imported target ML32i4dyn
ADD_LIBRARY(ML32i4dyn SHARED IMPORTED)

# Create imported target ML32i3dyn
ADD_LIBRARY(ML32i3dyn SHARED IMPORTED)

# Load information for each installed configuration.
GET_FILENAME_COMPONENT(_DIR "${CMAKE_CURRENT_LIST_FILE}" PATH)
FILE(GLOB CONFIG_FILES "${_DIR}/MathLink-*.cmake")
FOREACH(f ${CONFIG_FILES})
  INCLUDE(${f})
ENDFOREACH(f)

# Commands beyond this point should not need to know the version.
SET(CMAKE_IMPORT_FILE_VERSION)
CMAKE_POLICY(POP)
