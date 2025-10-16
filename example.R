# this script downloads the open access CBS PC6 shapefile data which comes with around 130 additional datapoints for each postcode.
# It trims to a gemeente and grids the data to a custom grid and plots as a 3d rayshader plot
library(sf)
library(raster)
library(ggplot2)
library(dplyr)
library(mapview)
library(polyglotr)
library(httr)
library(rayshader)

# create directory for data download, make sure your working directory is where this script is saved first, in the menu above go to "Session-Set working directory-to source file location"
dir.create("data/")
dir.create("plots/")

download.file("https://download.cbs.nl/postcode/2025-cbs_pc6_2024_v1.zip", destfile = "data/2025-cbs_pc6_2024_v1.zip")

unzip("data/2025-cbs_pc6_2024_v1.zip", exdir = "data/")

# read in the unzipped shape file
pc6 <- st_read("data/cbs_pc6_2024_v1.gpkg")

# you now have a variable in the environment. It is just like spreadsheet table, but with a column (geom) with the shape data
# Each row represents a PC6 shape (i.e. your postcode is PC6 (6 characters))

# however, the headers are in dutch, so get the headers from the data frame
pc6_colnames <- names(pc6)

# and translate them
headers_en <- polyglotr::google_translate(text = pc6_colnames, target_language = "en")

# have a look at them
headers_en

# replace the headers with english versions
names(pc6) <- headers_en

# but it is a huge amount of data let's so trim it

# first load in some dutch shapes
list_of_geos <- c("landsdeel", "provincie", "coropgebied", "gemeente")

## choose year and geometry
for (geo_nam in list_of_geos){

  url <- parse_url("https://service.pdok.nl/cbs/gebiedsindelingen/2023/wfs/v1_0")
  url$query <- list(service = "WFS",
                    version = "2.0.0",
                    request = "GetFeature",
                    typename = paste0("gebiedsindelingen:", geo_nam, "_gegeneraliseerd"),
                    outputFormat = "application/json")
  request <- build_url(url)

  geo_sf <- st_read(request, quiet = TRUE)

  assign(geo_nam, geo_sf)

}

# there are now 4 geospatial variables in your environment, these are the 4 main ways of dividing NL
mapview(gemeente)

# lets pick out Utrecht
utrecht <- filter(gemeente, statnaam == "Utrecht")

# and trim the data to it. This bit of code is basically comparing the geo columns in each file and picking out the ones that overlap
# putting pc6 first means we want this data that overlaps with utrecht, if we swapped it it would pick out gemeente that overlapped with pc6 (which would be all of them)
utrecht_pc6 <- pc6[utrecht,]

# lets have a look at it comparing with the Utrecht gemeente shape
mapview(utrecht_pc6)+utrecht

#is a bit messy because it is working on the outside of the shapes and pc6 areas that are outside also touch utrecht
#best to use the pc6 centre points i.e. the middle of each pc6
pc6_centroids <- st_centroid(pc6)

# do the same intersection
utrecht_pc6_cent <- pc6_centroids[utrecht,]

# now they all fit, but we only have dots
mapview(utrecht_pc6_cent)+utrecht

# use the dot data to pick out the pc6 shapes
pc6_utrecht <- filter(pc6, postcode6 %in% utrecht_pc6_cent$postcode6)

# looks nice
mapview(pc6_utrecht)+utrecht

# can view the data, just include a column heading in the quotation bit
mapview(pc6_utrecht['number_of_residents'])

# say we wanted to grid this data
# get min and max coordinates of utrecht - called the extent
utrecht_ext <- extent(utrecht)

## generate desired raster (grid)
r <- raster(nrows=10, ncols=10, xmn=utrecht_ext[1], xmx=utrecht_ext[2], ymn=utrecht_ext[3], ymx=utrecht_ext[4],
            crs = 28992)

r_poly <- st_as_sf(rasterToPolygons(r))

# show how the raster grid looks like compared to the shapes
mapview(r_poly)+pc6_utrecht


pc6_utrecht_n_res <- pc6_utrecht |>
  select(number_of_residents, geom) |>
  filter(number_of_residents >= 0)

# rasterize number of residents column the shape to the grid.
pc6_utrecht_gridded <- raster::rasterize(pc6_utrecht_n_res, r, "number_of_residents")

# quick plot
plot(pc6_utrecht_gridded)

# interactive plot
mapview(pc6_utrecht_gridded)

## generate desired raster (grid)
r_high_res <- raster(nrows=100, ncols=100, xmn=utrecht_ext[1], xmx=utrecht_ext[2], ymn=utrecht_ext[3], ymx=utrecht_ext[4],
            crs = 28992)

# grid the number of residents column from the pc6 data using the high res raster grid
hr_r <- terra::rasterize(pc6_utrecht_n_res, r_high_res, "number_of_residents")

# quick plot
plot(hr_r)

# interactive plot
mapview(hr_r)

# convert raster to an xyz dataframe i.e. every centre point of each grid x,y and z is the value of the cell
r_xyz <- data.frame(rasterToPoints(hr_r))

# 2d plot
pp = ggplot(r_xyz, aes(x = x, y = y)) +
  geom_raster(aes(x = x, y = y, fill = layer)) +
  scale_fill_viridis_c(option = "C")

# plot in 3d
par(mfrow = c(1, 2))
plot_gg(pp, width = 5, height = 4, scale = 300, raytrace = FALSE, preview = TRUE)
plot_gg(pp, width = 5, height = 4, scale = 300, multicore = TRUE, windowsize = c(1000, 800))

# save as a simple render
render_snapshot(filename = "plots/grid_render.png")

# do a hex plot of the same data
pp <- ggplot(r_xyz, aes(x = x, y = y, z = layer)) +
  stat_summary_hex(aes(fill = ..value..),
                   fun = mean, bins = 30, colour = "black", size = 0.2) +
  scale_fill_viridis_c(option = "C") +
  coord_equal(ratio = 1) +
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "white", colour = NA),
    plot.background  = element_rect(fill = "white", colour = NA)
  )

# how is it looking?
pp

# now in 3D
plot_gg(pp,
        width = 6, height = 6,
        scale = 300,
        multicore = TRUE,
        windowsize = c(1000, 800),
        zoom = 0.6, phi = 40, theta = 45)

# do a high quality render
render_highquality(
  filename = "plots/hex_render.png",
  environment_light = "white",
  light = TRUE,
  samples = 200
)




