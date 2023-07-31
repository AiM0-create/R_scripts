################################################################################
#                 Map rivers with sf and ggplot2 in R
#                 Abhishek Rawat
#                 2023/07/31
################################################################################

# libraries we need
libs <- c("httr", "tidyverse", "sf")

# install missing libraries
installed_libs <- libs %in% rownames(installed.packages())
if (any(installed_libs == F)) {
    install.packages(libs[!installed_libs])
}

# load libraries
invisible(lapply(libs, library, character.only = T))

# 1. GET RIVERS DATA
#---------

get_data <- function() {
    url <- "https://data.hydrosheds.org/file/HydroRIVERS/HydroRIVERS_v10_as_shp.zip" # nolint
    res <- httr::GET(  # nolint
        url, # nolint
        write_disk("as_rivers.zip"), # nolint
        progress() # nolint
    ) # nolint
    unzip("as_rivers.zip") # unzip
    filenames <- list.files("HydroRIVERS_v10_as_shp",
        pattern = "*.shp", full.names = T # nolint
    ) # nolint

    return(filenames) # nolint
}

filenames <- get_data()

# 2. CREATE RIVER WIDTH
#---------

load_rivers <- function() {
    list_riv <- lapply(filenames, sf::st_read)
    as_riv <- list_riv[[1]] |>
        sf::st_cast("MULTILINESTRING")

    return(as_riv)
}

as_riv <- load_rivers()

get_river_width <- function() {
    as_riv_width <- as_riv |>
        dplyr::mutate(
            width = as.numeric(ORD_FLOW), # nolint
            width = dplyr::case_when(
                width == 2 ~ 0.8,
                width == 3 ~ 0.6,
                width == 4 ~ 0.6,
                width == 5 ~ 0.3,
                width == 6 ~ 0.3,
                width == 7 ~ 0.1,
                width == 8 ~ 0.1,
                width == 9 ~ 0.05,
                width == 10 ~ 0.05,
                TRUE ~ 0
            )
        ) |>
        sf::st_as_sf()

    return(as_riv_width)
}

as_riv_width <- get_river_width()

# 3. MAKE BOUNDING BOX
#---------

crsLONGLAT <- "+proj=longlat +datum=WGS84 +no_defs"

get_bounding_box <- function(bbox, new_prj, bb) {
    bbox <- st_sfc( # nolint
        st_polygon(list(cbind( # nolint
            c(65, 97.82, 97.82, 65, 65),
            c(5.00, 5.00, 35.96, 35.96, 5.00)
        ))),
        crs = crsLONGLAT
    )

    new_prj <- sf::st_transform(bbox, crs = 32643)
    bb <- sf::st_bbox(new_prj)

    return(bb)
}

bbox <- get_bounding_box()

# 4. MAP
#---------

get_river_map <- function() {
    p <-
        ggplot() +
        geom_sf(
            data = as_riv_width,
            aes(
                color = factor(ORD_FLOW), size = width,
                alpha = factor(ORD_FLOW)
            )
        ) +
        coord_sf(
            crs = 32643,
            xlim = c(bbox["xmin"], bbox["xmax"]),
            ylim = c(bbox["ymin"], bbox["ymax"])
        ) +
        labs(
            y = NULL, subtitle = "",
            x = "©2023 Abhishek Rawat | Data: ©World Wildlife Fund, Inc. (2006-2013) HydroSHED database http://www.hydrosheds.org",
            title = "Rivers of India",
            caption = ""
        ) +
        scale_color_manual(
            name = "",
            values = c(
                "#440154", "#472a62", "#474770",
                "#42627b", "#3a7d83", "#2c9986",
                "#48b279", "#83c760", "#bdd946"
            )
        ) +
        scale_size(range = c(0, .3)) +
        scale_alpha_manual(values = c(
            "2"=1, "3" = .8, "4" = .8, "5" = .6, "6" = .6,
            "7" = .4, "8" = .4, "9" = .2, "10" = .2
        )) +
        theme_minimal() +
        theme(
            panel.background = element_blank(),
            legend.background = element_blank(),
            legend.position = "none",
            panel.border = element_blank(),
            panel.grid.minor = element_blank(),
            panel.grid.major = element_blank(),
            plot.title = element_text(
                size = 20, color = "#000000", hjust = 0.5, vjust = 0
            ),
            plot.subtitle = element_text(
                size = 12, color = "#ac63a0", hjust = 0.5, vjust = 0
            ),
            plot.caption = element_text(
                size = 8, color = "grey60", hjust = 0.5, vjust = 10
            ),
            axis.title.x = element_text(
                size = 2, color = "grey20", hjust = 0.5, vjust = 0
            ),
            legend.text = element_text(
                size = 9, color = "grey20"
            ),
            legend.title = element_text(size = 5, color = "grey20"),
            strip.text = element_text(size = 5),
            plot.margin = unit(c(t = 1, r = -2, b = -1, l = -2), "lines"),
            axis.title.y = element_blank(),
            axis.ticks = element_blank(),
            axis.text.x = element_blank(),
            axis.text.y = element_blank()
        )

    return(p)
}

p1 <- get_river_map()

ggsave(
    filename = "india_rivers.png",
    width = 3, height = 5, dpi = 900,
    device = "png", bg = "white", p1
)