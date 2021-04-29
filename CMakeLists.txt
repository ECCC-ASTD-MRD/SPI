cmake_minimum_required(VERSION 3.14)

project(spi C)

if("${CMAKE_INSTALL_PREFIX}" STREQUAL "" OR ${CMAKE_INSTALL_PREFIX} STREQUAL /usr/local)
    set(CMAKE_INSTALL_PREFIX ${CMAKE_BINARY_DIR}/install-site)
endif()

add_subdirectory(libSPI)

