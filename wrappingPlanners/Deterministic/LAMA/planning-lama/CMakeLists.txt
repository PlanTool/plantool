# CMakeLists.txt has to be located in the project folder and cmake has to be
# executed from 'project/build' with 'cmake ../'.
cmake_minimum_required(VERSION 2.6)
find_package(Rock)
rock_init(lama 0.1)

configure_file( ${CMAKE_CURRENT_SOURCE_DIR}/plan.in ${CMAKE_CURRENT_BINARY_DIR}/lama-planner)

add_subdirectory(lama)

install(FILES ${CMAKE_CURRENT_BINARY_DIR}/lama-planner
    PERMISSIONS OWNER_READ OWNER_WRITE OWNER_EXECUTE GROUP_READ GROUP_EXECUTE WORLD_READ WORLD_EXECUTE
    DESTINATION bin
    )

