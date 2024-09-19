# Load objects as instantiated with current master version
load("_fixtures/pcg_hierarchy.rda")

grouped_pcg <- GErapcg::pcg_grouping(test_data, GErapcg::pcg_import(test_pcglist))
all_pcg <- data.table::fread(system.file("extdata/pcg.csv", package = "GErapcg"))[["pcg"]]


