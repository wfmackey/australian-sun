#' Daylight savings time and sunrises/sunsets in Australia =====================
#' Will Mackey, recreating this nice chart by Andy Woodruff via Vox: 
#' https://twitter.com/voxdotcom/status/1503819992822951945?s=20&t=S7m4FMTnyA-8fGl9lpOwDQ


# SET UP =======================================================================

## Library ---------------------------------------------------------------------
library(tidyverse)
  library(lubridate) # for handling dates
  library(hms)       # for handling times
library(data.table)  # fifelse is just very fast 
library(sf)          # handle geometry data
library(strayr)      # source Australian geometry data; remotes::install_github("runapp-aus/strayr")
library(lutz)        # get time-zones based on location
library(suncalc)     # get sunrise/set times based on location
library(patchwork)   # plotting arrangement
library(ggtext)      # plotting text formatting
library(ggpubr)      # help extracting the legend from a plot

## Project settings ------------------------------------------------------------

### dividing each lat/lon into pieces of size:
hex_cell_size <- 0.2 

### define dates (all days in 2022, separated by 2 because otherwise my 
### computer was crashing)
dates <- seq.Date(ymd("2022-01-01"), 
                  ymd("2022-12-31"), 
                  by = 2, 
                  tz = "GMT")

### start and end dates for Australian DLS time: ---------------------------------
### https://info.australia.gov.au/about-australia/facts-and-figures/time-zones-and-daylight-saving
dst_starts <- ymd("2022-10-01")
dst_ends <- ymd("2022-04-03")

### this is always useful:
`%nin%` <- Negate(`%in%`)

# BUILD DATA ===================================================================




## Australia state geography ---------------------------------------------------
aus <- read_absmap("state2021", remove_year_suffix = TRUE, 
                   export_dir = "data") %>% 
  # drop other territories
  filter(state_code %nin% c("9", "Z")) %>%  
  select(state_name)

states <- aus$state_name

## Create cells (hex grid) for Australia ---------------------------------------

base_grid <- aus %>% 
  st_make_grid(cellsize = c(hex_cell_size, 
                            hex_cell_size),
               square = FALSE) %>% 
  st_as_sf() %>% 
  # only join cells completely within an aus state, (noting that join = st_within 
  # will remove some cells completely; which is fine here)
  st_join(aus, left = FALSE, 
          join = st_within)
  
### Sense-check the grid ----
plot_grid_all <- aus %>% 
  ggplot() +
  geom_sf() +
  geom_sf(data = base_grid, fill = NA) + 
  theme_void()

ggsave("charts/grid_all.png", plot_grid_all, height = 10, width = 10)


## Get timezone for each cell --------------------------------------------------

grid <- base_grid %>%
  # make centroids
  st_centroid() %>% 
  st_coordinates() %>% 
  bind_cols(base_grid, .) %>% 
  # clean up
  select(lat = Y, lon = X, geometry = 1) %>% 
  st_as_sf() %>% 
  # create id and get timezome for each geom
  mutate(id = row_number(),
         timezone = tz_lookup_coords(lat = lat, lon = lon, 
                                     method = "accurate"))


## Timezone details with and without DLS ---------------------------------------

timezones <- unique(grid$timezone)

timezone_details <- tz_list() %>% 
  filter(tz_name %in% timezones) %>% 
  group_by(tz_name) %>% 
  mutate(has_dst = any(is_dst))

timezone_non_dst <- timezone_details %>% 
  filter(!is_dst) %>% 
  select(timezone = tz_name, 
         non_dst_offset = utc_offset_h)

timezone_dst <- timezone_details %>% 
  mutate(is_dst = if_else(!has_dst, TRUE, is_dst)) %>% 
  filter(is_dst) %>% 
  select(timezone = tz_name, dst_offset = utc_offset_h)

timezone_offsets <- timezone_dst %>% 
  left_join(timezone_non_dst)


## Get sunset and sunrise times for each cell-date -----------------------------

grid_utc <- grid %>% 
    st_drop_geometry() %>% 
    crossing(date = dates) %>% 
    # get sunrise
    getSunlightTimes(
      data = .,
      keep = c("sunrise", "sunset"), 
      tz = "UTC")

## Convert UTC times to: current, always dst, never dst ------------------------

grid_sun <- grid_utc %>%
  left_join(grid) %>% 
  left_join(timezone_offsets) %>% 
  mutate(is_non_dst = date %>% between(dst_ends, dst_starts),
         is_dst = !is_non_dst,
         # current settings: dst if dst; not dst if not dst
         across(c(sunrise, sunset), \(x) fifelse(is_dst, 
                                                 x + hms(hours = dst_offset),
                                                 x + hms(hours = non_dst_offset)), 
                .names = "current_{.col}"),
         # if never dst: not dst
         across(c(sunrise, sunset), \(x) x + hms(hours = non_dst_offset), 
                .names = "no_dst_{.col}"),
         # if always dst (regardless of whether the region had dst): not dst plus 1
         across(c(sunrise, sunset), \(x) x + hms(hours = non_dst_offset + 1), 
                .names = "always_dst_{.col}")
         ) %>% 
  select(-c(sunrise, sunset)) %>% 
  # convert to hour-min-sec rather than a date-time
  mutate(across(contains("_sun"), as_hms))
  

## Generate summary statistics for each cell -----------------------------------

grid_sun_sum <- grid_sun %>% 
  mutate(across(contains("sunrise"), \(x) x < hms(hours = 7)),
         across(contains("sunset"),  \(x) x > hms(hours = 18.5))) %>% 
  group_by(id) %>% 
  summarise(n = n() * 2, # x2 because we did every second day in line 24 (to cut RAM)
            across(contains("sunrise"), sum) * 2, # x2 as above
            across(contains("sunset"), sum) * 2,  # x2 as above
            ) %>% 
  left_join(grid)
  


# PLOT =========================================================================

## Function for consistently plotting particular variables ---------------------
plot_sun <- function(title = "", subtitle = "", var, leg = "none") {
  grid_sun_sum %>% 
    ggplot() + 
    geom_sf(aes(geometry = geometry,
                fill = {{ var }}),
            lwd = 0) + 
    theme_void() + 
    scale_fill_viridis_c(
      option = "B", limits = c(0, 366), 
      breaks = c(0, 100, 200, 300, 365),
      guide = guide_colorbar(
        title = "Days",
        title.theme = element_text(size = 16, hjust = 0.5, vjust = 1),
        label.theme = element_text(size = 14),
        barwidth = unit(15, "cm"))) + 
    labs(title = title,
         subtitle = subtitle) + 
    theme(legend.position = leg,
          plot.title = element_markdown(size = 16),
          plot.subtitle = element_markdown(size = 14, vjust = 1))
}

## Generate plots --------------------------------------------------------------

### Plot legend (for independent use later) ------------------------------------
p_leg <- plot_sun(var = current_sunrise, leg = "top") %>% 
  get_legend() %>% 
  as_ggplot()

### Plot current settings -------------------------------------------------------
p_current_sunrise <- plot_sun(title = "**Current daylight savings settings**", 
                              subtitle = "Number of days with sunrise before 7am", 
                              var = current_sunrise)

p_current_sunset <- plot_sun(subtitle = "Number of days with sunset after 6:30pm", 
                             var = current_sunset)

### Plot no daylight savings ----------------------------------------------------
p_no_dst_sunrise <- plot_sun(title = "**No daylight savings anywhere**", 
                             subtitle = "Number of days with sunrise before 7am", 
                             var = no_dst_sunrise)

p_no_dst_sunset <- plot_sun(subtitle = "Number of days with sunset after 6:30pm", 
                            var = no_dst_sunset)

### Plot always daylight savings ------------------------------------------------
p_always_dst_sunrise <- plot_sun(title = "**Always daylight savings, everywhere**", 
                                 subtitle = "Number of days with sunrise before 7am", 
                                 var = always_dst_sunrise)

p_always_dst_sunset <- plot_sun(subtitle = "Number of days with sunset after 6:30pm", 
                                var = always_dst_sunset)


## Define layout for plot arrangement ------------------------------------------
## see https://patchwork.data-imaginist.com/reference/area.html

layout <- c(
  # legend:
  area(t = 1, b = 2,   l =  7, r = 12),
  # plot current:
  area(t = 3, b = 12,   l =  1, r = 10),
  area(t = 3, b = 12,   l =  9, r = 18),
  # plot no dst:
  area(t = 13, b = 22,   l =  1, r = 10),
  area(t = 13, b = 22,   l =  9, r = 18),
  # plot no dst:
  area(t = 23, b = 32,   l =  1, r = 10),
  area(t = 23, b = 32,   l =  9, r = 18)
)

### view your plot arrangement: 
plot(layout)

## Combine final patchwork plot ------------------------------------------------

p <- p_leg +
  p_current_sunrise + p_current_sunset +
  p_no_dst_sunrise + p_no_dst_sunset +
  p_always_dst_sunrise + p_always_dst_sunset +
  plot_layout(design = layout) + 
  plot_annotation(
    title = "**What if Australia stopped<br>changing its clocks?**<br>", 
    theme = theme(plot.title = element_markdown(size = 30, hjust = 0.5),
                  plot.subtitle = element_text(size = 14, hjust = 0.5)),
                  caption = 'Source: Will Mackey, recreation of Andy Woodruff, via {lutz}, {suncalc}, {sf} and other R packages. Note: quick analysis; no accuracy guaranteed.')

## Combine final patchwork plot ------------------------------------------------
ggsave('charts/australian-sun.png', plot = p,
       height = 13, width = 10)
 
# end, yayyy ===================================================================