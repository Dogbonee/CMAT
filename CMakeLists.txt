cmake_minimum_required(VERSION 3.10)
project(CMAT C)

# ------------------------------------------------------------------
# 1. DEFINE THE TOOLCHAIN PATH
# Change this path to where you installed the CE Toolchain.
# ------------------------------------------------------------------

set(CE_TOOLCHAIN_ROOT "C:/Users/101142429/Documents/CEdev")

# ------------------------------------------------------------------
# 2. ADD COMPILER DEFINITIONS
# These macros are usually defined by the compiler automatically.
# We must define them here so CLion knows we are on a calculator.
# ------------------------------------------------------------------
add_compile_definitions(__CE__ __OZ__ z80 eZ80)

# ------------------------------------------------------------------
# 3. INCLUDE THE CALCULATOR HEADERS
# This tells CLion where <tice.h> and <graphx.h> actually live.
# ------------------------------------------------------------------
include_directories(
        "${CE_TOOLCHAIN_ROOT}/include"
        "${CE_TOOLCHAIN_ROOT}/lib"
)

# ------------------------------------------------------------------
# 4. ADD YOUR SOURCE FILES
# Tells CLion which files to analyze.
# ------------------------------------------------------------------
file(GLOB_RECURSE SOURCES "src/*.c" "src/*.h")

add_executable(CMAT ${SOURCES})