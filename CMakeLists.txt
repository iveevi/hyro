cmake_minimum_required(VERSION 3.20)

project(hyro VERSION 0.1.0 LANGUAGES CXX)

find_package(Vulkan REQUIRED)
find_package(glfw3 REQUIRED)
find_package(glslang REQUIRED)

include(FetchContent)
FetchContent_Declare(lexy URL https://lexy.foonathan.net/download/lexy-src.zip)
FetchContent_MakeAvailable(lexy)

add_executable(hyro main.cpp thirdparty/fmt/src/format.cc)

target_compile_features(hyro PRIVATE cxx_std_20)

include_directories(. thirdparty thirdparty/glm thirdparty/fmt/include thirdparty/lexy/include)

target_link_libraries(hyro PRIVATE glfw Vulkan::Vulkan SPIRV glslang::glslang-default-resource-limits foonathan::lexy)
