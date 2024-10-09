cmake_minimum_required(VERSION 3.25)

project(basexx
        VERSION 1.0
        DESCRIPTION "RFC 4648 encoding/decoding"
        LANGUAGES CXX)

# Only enable testing if this library is not included from a separate project.
string(COMPARE EQUAL "${CMAKE_CURRENT_SOURCE_DIR}" "${CMAKE_SOURCE_DIR}" ENABLE_TESTING)

add_subdirectory(src)

if (ENABLE_TESTING)
        add_subdirectory(test)
endif()
