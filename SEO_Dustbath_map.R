###─────────────────────────────────────────────────────────────────────────###
# Floreana Ecosystem Map – Dustbath Camera Trap Locations
#
# Converts the ecosystem shapefile to GeoPackage, then produces two maps of
# Floreana Island ecosystem types with camera trap locations and a
# Galápagos archipelago inset:
#   Map 1 – Detailed classification (11 ecosystem types)
#   Map 2 – Simplified classification (5 classes)
#
# Colours follow Paul Tol's colorblind-safe qualitative palettes
# ("muted" and "light"); see https://personal.sron.nl/~pault/
#
# Josue Arteaga-Torres | josue.arteaga.t@gmail.com
###─────────────────────────────────────────────────────────────────────────###

library(here)
library(sf)
library(dplyr)
library(ggplot2)
library(ggspatial)
library(patchwork)

here()

###─────────────────────────────────────────────────────────────────────────###
# SEC. 1 · LOAD SHAPEFILE AND ADD DETAILED ENGLISH LABELS ####
###─────────────────────────────────────────────────────────────────────────###
# 
# ecosystem_sf <- st_read("./ecosystems_floreana/FloreanaEcosystemas.shp")
# 
# ecosystem_en <- ecosystem_sf |>
#   mutate(simplified_class = case_when(
#     Ecosis_Nat == "Bosque Decíduo"                  ~ "Deciduous Forest",
#     Ecosis_Nat == "Bosque Siempreverde Estacional"  ~ "Evergreen Seasonal Forest",
#     Ecosis_Nat == "Arbustal Decíduo"                ~ "Deciduous Shrub",
#     Ecosis_Nat == "Especies Invasoras"              ~ "Invasive Plants",
#     Ecosis_Nat == "Bosque y Arbustal Siempreverde"  ~ "Evergreen Shrub and Forest",
#     Ecosis_Nat == "Herbazal Decíduo"                ~ "Deciduous Grassland",
#     Ecosis_Nat == "Area Agrícola"                   ~ "Agriculture",
#     Ecosis_Nat == "Buffer Agrícola"                 ~ "Agriculture Buffer",
#     Ecosis_Nat == "Agua"                            ~ "Water",
#     Ecosis_Nat == "Area Urbana"                     ~ "Urban",
#     Ecosis_Nat == "Lava Reciente"                   ~ "Lava Field",
#     TRUE                                            ~ "Other"
#   ))

###─────────────────────────────────────────────────────────────────────────###
# SEC. 2 · SAVE AS GEOPACKAGE ####
###─────────────────────────────────────────────────────────────────────────###
#
# GeoPackage (.gpkg) is preferred over Shapefile for sharing:
#   - Single self-contained file (no .shp + .dbf + .prj + .shx bundle)
#   - OGC open standard; supported by QGIS, ArcGIS Pro, Python (geopandas),
#     R (sf), GRASS, MapInfo, and most modern GIS software
#   - No 10-character field name limit; full UTF-8 support
#   - Supports multiple layers, vector and raster, in one file

# st_write(
#   ecosystem_en,
#   "./floreana_ecosystems.gpkg",
#   driver     = "GPKG",
#   delete_dsn = TRUE   # overwrite if file already exists
# )

###─────────────────────────────────────────────────────────────────────────###
# SEC. 3 · RELOAD GEOPACKAGE + SIMPLIFIED RECLASSIFICATION ####
###─────────────────────────────────────────────────────────────────────────###

ecosystem_gpkg <- st_read("./floreana_ecosystems.gpkg")

# Quick checks
cat("CRS:", st_crs(ecosystem_gpkg)$input, "\n")
cat("Fields:", paste(names(ecosystem_gpkg), collapse = ", "), "\n")
print(table(ecosystem_gpkg$simplified_class))

# Simplified reclassification for Map 2 (applied to the reloaded GeoPackage)
ecosystem_simple <- ecosystem_gpkg |>
  mutate(simplified_class = case_when(
    Ecosis_Nat %in% c("Bosque Decíduo",
                      "Bosque Siempreverde Estacional",
                      "Arbustal Decíduo",
                      "Especies Invasoras",
                      "Bosque y Arbustal Siempreverde") ~ "Forest",
    Ecosis_Nat %in% c("Herbazal Decíduo",
                      "Area Agrícola",
                      "Buffer Agrícola")               ~ "Low Vegetation",
    Ecosis_Nat %in% c("Agua")                          ~ "Water",
    Ecosis_Nat %in% c("Area Urbana")                   ~ "Urban",
    Ecosis_Nat %in% c("Lava Reciente")                 ~ "Lava",
    TRUE                                               ~ "Other"
  ))

###─────────────────────────────────────────────────────────────────────────###
# SEC. 4 · CAMERA TRAP LOCATIONS ####
###─────────────────────────────────────────────────────────────────────────###

camera_traps <- data.frame(
  name = c("Cemetery", "Red Quarry"),
  lon  = c(-90.48126,  -90.452518),
  lat  = c(-1.27814,   -1.291315)
) |>
  st_as_sf(coords = c("lon", "lat"), crs = 4326) |>
  st_transform(32715)   # UTM Zone 15S, consistent with all spatial layers

###─────────────────────────────────────────────────────────────────────────###
# SEC. 5 · GALÁPAGOS INSET ####
###─────────────────────────────────────────────────────────────────────────###
#
# Uses the archipelago shapefile from the SEO_RSF_Before project.
# Floreana (Isla Santa María) is located with an orange bounding-box rectangle.

galapagos_islands <- st_read(
  "./galapagos_shape/islas galapagos.shp",
  quiet = TRUE
) |>
  st_set_crs(32715)

galapagos_flor <- galapagos_islands |>
  filter(NOMBRE == "Isla Santa María")

# Pad the bounding box so the orange rectangle is clearly visible at inset scale
flor_bb <- st_bbox(galapagos_flor)
pad      <- 15000   # metres
floreana_bbox_sf <- st_as_sfc(
  structure(
    c(xmin = unname(flor_bb["xmin"]) - pad,
      ymin = unname(flor_bb["ymin"]) - pad,
      xmax = unname(flor_bb["xmax"]) + pad,
      ymax = unname(flor_bb["ymax"]) + pad),
    class = "bbox",
    crs   = st_crs(galapagos_flor)
  )
)

inset_map <- ggplot() +
  geom_sf(
    data      = galapagos_islands,
    fill      = "gray75",
    color     = "gray40",
    linewidth = 0.2
  ) +
  geom_sf(
    data      = floreana_bbox_sf,
    fill      = NA,
    color     = "#D55E00",
    linewidth = 0.8
  ) +
  theme_void() +
  theme(
    panel.border = element_rect(color = "gray30", fill = NA, linewidth = 0.5)
  )

###─────────────────────────────────────────────────────────────────────────###
# SEC. 6 · MAP 1 – DETAILED CLASSIFICATION (11 ECOSYSTEM TYPES) ####
###─────────────────────────────────────────────────────────────────────────###
#
# Colours from Paul Tol's "muted" (primary) and "light" (secondary) palettes.
# Both are colorblind-safe qualitative schemes.
# Reference: https://personal.sron.nl/~pault/

ecosystem_utm <- st_transform(ecosystem_gpkg, 32715)

# Paul Tol "muted" palette: #CC6677 #332288 #DDCC77 #117733 #88CCEE
#                            #882255 #44AA99 #999933 #AA4499
# Paul Tol "light" palette:  #EE8866 #EEDD88 #FFAABB #BBBBBB
ecosystem_cols <- c(
  "Deciduous Forest"           = "#117733",  # muted green
  "Evergreen Seasonal Forest"  = "#44AA99",  # muted teal
  "Evergreen Shrub and Forest" = "#999933",  # muted olive
  "Deciduous Shrub"            = "#DDCC77",  # muted sand
  "Deciduous Grassland"        = "#EEDD88",  # light yellow
  "Invasive Plants"            = "#CC6677",  # muted rose
  "Agriculture"                = "#EE8866",  # light orange
  "Agriculture Buffer"         = "#FFAABB",  # light pink
  "Lava Field"                 = "#BBBBBB",  # light grey
  "Water"                      = "#88CCEE",  # muted cyan
  "Urban"                      = "#882255"   # muted wine
)

map1 <- ggplot() +
  geom_sf(data = ecosystem_utm, aes(fill = simplified_class), color = NA) +
  scale_fill_manual(
    values   = ecosystem_cols,
    name     = "Ecosystem Type",
    na.value = "white"
  ) +
  # Camera trap locations — hollow circles
  geom_sf(
    data   = camera_traps,
    shape  = 21,
    fill   = NA,
    color  = "black",
    size   = 3,
    stroke = 1.2
  ) +
  annotation_scale(location = "br", width_hint = 0.25) +
  coord_sf(crs = 32715, expand = FALSE) +
  theme_minimal(base_size = 11) +
  theme(
    axis.title       = element_blank(),
    axis.text        = element_text(size = 8),
    legend.position  = "right",
    legend.title     = element_text(size = 9, face = "bold"),
    legend.text      = element_text(size = 8),
    panel.background = element_rect(fill = "#88CCEE", color = NA)
  )

map1_final <- map1 +
  inset_element(inset_map, left = 0.65, bottom = 0.65, right = 1.00, top = 1.00)

map1_final

# ggsave("./floreana_dustbath_map1_detailed.pdf",  map1_final, width = 10, height = 8)
# ggsave("./floreana_dustbath_map1_detailed.tiff", map1_final, width = 10, height = 8,
#         dpi = 600, compression = "lzw")

###─────────────────────────────────────────────────────────────────────────###
# SEC. 7 · MAP 2 – SIMPLIFIED CLASSIFICATION (5 CLASSES) ####
###─────────────────────────────────────────────────────────────────────────###

ecosystem_simple_utm <- st_transform(ecosystem_simple, 32715)

# Paul Tol "muted" palette — fewer classes, higher contrast between categories
simple_cols <- c(
  "Forest"        = "#117733",  # muted green
  "Low Vegetation"= "#DDCC77",  # muted sand
  "Water"         = "#88CCEE",  # muted cyan
  "Urban"         = "#882255",  # muted wine
  "Lava"          = "#BBBBBB",  # light grey
  "Other"         = "#DDDDDD"   # pale grey
)

map2 <- ggplot() +
  geom_sf(data = ecosystem_simple_utm, aes(fill = simplified_class), color = NA) +
  scale_fill_manual(
    values   = simple_cols,
    name     = "Ecosystem Type",
    na.value = "white"
  ) +
  # Camera trap locations — hollow circles
  geom_sf(
    data   = camera_traps,
    shape  = 21,
    fill   = NA,
    color  = "black",
    size   = 3,
    stroke = 1.2
  ) +
  annotation_scale(location = "br", width_hint = 0.25) +
  coord_sf(crs = 32715, expand = FALSE) +
  theme_minimal(base_size = 11) +
  theme(
    axis.title       = element_blank(),
    axis.text        = element_text(size = 8),
    legend.position  = "right",
    legend.title     = element_text(size = 9, face = "bold"),
    legend.text      = element_text(size = 8),
    panel.background = element_rect(fill = "#88CCEE", color = NA)
  )

map2_final <- map2 +
  inset_element(inset_map, left = 0.65, bottom = 0.65, right = 1.00, top = 1.00)

map2_final

# ggsave("./floreana_dustbath_map2_simplified.pdf",  map2_final, width = 10, height = 8)
# ggsave("./floreana_dustbath_map2_simplified.tiff", map2_final, width = 10, height = 8,
#         dpi = 600, compression = "lzw")
