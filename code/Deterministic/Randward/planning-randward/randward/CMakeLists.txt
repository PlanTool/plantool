rock_library(${PROJECT_NAME}-search
    SOURCES
        search/axioms.cc
        search/best_first_search.cc
        search/closed_list.cc
        search/domain_transition_graph.cc
        search/ff_heuristic.cc
        search/globals.cc
        search/heuristic.cc
        search/landmarks_count_heuristic.cc
        search/landmarks_graph.cc
        search/landmarks_graph_rpg_sasp.cc
        search/open_list.cc
        search/operator.cc
        search/planner.cc
        search/search_engine.cc
        search/state.cc
        search/successor_generator.cc
        search/wa_star_search.cc
    HEADERS
        search/axioms.h
        search/best_first_search.h
        search/closed_list.h
        search/domain_transition_graph.h
        search/ff_heuristic.h
        search/globals.h
        search/heuristic.h
        search/landmarks_count_heuristic.h
        search/landmarks_graph.h
        search/landmarks_graph_rpg_sasp.h
        search/landmarks_types.h
        search/open_list.h
        search/operator.h
        search/search_engine.h
        search/state.h
        search/successor_generator.h
        search/wa_star_search.h
)

rock_executable(${PROJECT_NAME}-planner-search
        search/planner.cc
        DEPS ${PROJECT_NAME}-search
        NOINSTALL)

rock_library(${PROJECT_NAME}-preprocess
    SOURCES
        preprocess/axiom.cc
        preprocess/causal_graph.cc
        preprocess/domain_transition_graph.cc
        preprocess/helper_functions.cc
        preprocess/max_dag.cc
        preprocess/operator.cc
        preprocess/planner.cc
        preprocess/scc.cc
        preprocess/state.cc
        preprocess/successor_generator.cc
        preprocess/variable.cc
    HEADERS
        preprocess/axiom.h
        preprocess/causal_graph.h
        preprocess/domain_transition_graph.h
        preprocess/helper_functions.h
        preprocess/max_dag.h
        preprocess/operator.h
        preprocess/scc.h
        preprocess/state.h
        preprocess/successor_generator.h
        preprocess/variable.h
)
rock_executable(${PROJECT_NAME}-planner-preprocess
        preprocess/planner.cc
        DEPS ${PROJECT_NAME}-preprocess
        NOINSTALL)

# Installation of the driver directory
# Python wrapper to facilitate running the system
install(CODE "execute_process(COMMAND ${CMAKE_COMMAND} -E copy_directory
\"${CMAKE_CURRENT_SOURCE_DIR}/translate\"
\"${CMAKE_INSTALL_PREFIX}/bin/${PROJECT_NAME}/translate\") ")

install(FILES ${CMAKE_CURRENT_BINARY_DIR}/${PROJECT_NAME}-planner-preprocess
    PERMISSIONS OWNER_READ OWNER_WRITE OWNER_EXECUTE GROUP_READ GROUP_EXECUTE WORLD_READ WORLD_EXECUTE
    DESTINATION bin/${PROJECT_NAME}/preprocess
    RENAME preprocess)
install(FILES ${CMAKE_CURRENT_BINARY_DIR}/${PROJECT_NAME}-planner-search
    PERMISSIONS OWNER_READ OWNER_WRITE OWNER_EXECUTE GROUP_READ GROUP_EXECUTE WORLD_READ WORLD_EXECUTE
    DESTINATION bin/${PROJECT_NAME}/search
    RENAME release-search)
install(FILES ${CMAKE_CURRENT_SOURCE_DIR}/../plan
    PERMISSIONS OWNER_READ OWNER_WRITE OWNER_EXECUTE GROUP_READ GROUP_EXECUTE WORLD_READ WORLD_EXECUTE
    DESTINATION bin
    RENAME randward-planner)
