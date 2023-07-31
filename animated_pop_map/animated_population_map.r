#######################################################
#                 Animate rasters with R
#                 Abhishek Rawat
#                 2023/07/31
########################################################

remotes::install_github(
    "dieghernan/tidyterra"
)

libs <- c(
    "tidyverse", "terra", "tidyterra",
    "osmdata", "sf", "ggmap", "classInt",
    "gifski"
)

installed_libraries <- libs %in% rownames(
    installed.packages()
)

if(any(installed_libraries == F)){
    install.packages(
        libs[!installed_libraries]
    )
}

invisible(lapply(
    libs, library, character.only = T
))

### 1. GET POPULATION DATA
### ----------------------
years <- seq(1975, 2030, by = 5)

urls <- paste0(
    "https://jeodpp.jrc.ec.europa.eu/ftp/jrc-opendata/GHSL/GHS_POP_GLOBE_R2023A/GHS_POP_E",
    years,
    "_GLOBE_R2023A_54009_100/V1-0/tiles/GHS_POP_E",
    years, "_GLOBE_R2023A_54009_100_V1_0_R6_C26.zip"
)

for (url in urls) {
    download.file(
        url,
        destfile = basename(url),
        mode = "wb"
    )
}

fnames <- list.files(
    path = getwd(),
    pattern = ".zip",
    full.names = T
)

lapply(fnames, unzip)

raster_files <- list.files(
    path = getwd(),
    pattern = ".tif",
    full.names = T
)

pop_rasters <- lapply(
    raster_files, terra::rast
)
