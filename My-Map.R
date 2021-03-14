
# Load Libs ---------------------------------------------------------------
library(tidyverse) # general use ----
library(gganimate) # to create animated gif ----
library(extrafont) # access and enable fonts ----
library(ggtext)    # style text in ggplot ----
library(glue)      # paste strings ---


# Load Font(s) ------------------------------------------------------------
# import fonts; only needed one time or after new font installation ----
# fonts may install to multiple directories; figure out where yours are installed ----
# and adjust path accordingly if needed ----
# this can take a while ----
#extrafont::font_import(paths = "C:/Users/Jake Scott/Desktop/Fonts-for-R",prompt = F)

# do this each R session in which you want to use fonts ----
extrafont::loadfonts(device = "win")
# examine font family names available for use (output not shown) ----
extrafont::fonts()



# Load Data ---------------------------------------------------------------
us_states <- ggplot2::map_data("state")

#Locations lived
residence <- tribble(
  ~city,           ~state,  ~lat,   ~long, ~years, ~description,
  "Whitesboro",      "NY", 43.12,  -75.29,  18,    "Childhood",
  "Hamilton",        "NY", 42.83,  -75.54,  2.5,   "Undergrad  at Colgate University",
  "Washington",      "DC", 38.91,  -77.04,  0.5,   "Off-Campus Study in DC",
  "Hamilton",        "NY", 42.83,  -75.54,  1.0,   "Undergrad  at Colgate University",
  "Washington",      "DC", 38.91,  -77.04,  0.25,  "Research Assistant at Federal Reserve",
  "Whitesboro",      "NY", 43.12,  -75.29,  .75,    "Work-from-home as RA at Federal Reserve",
) 

#Control the transition between animations
residence_connections_prelim <- residence %>% 
  mutate(
    # need this to create transition state ----
    city_order = row_number() + 1,
    # where I moved to next, for curved arrows ----
    lat_next = lead(lat),
    long_next = lead(long),
    # label to show in plot, styled using ggtext ---
    label = glue::glue("**{city}, {state}** ({years} yrs)<br>*{description}*"),
    # label of next location ----
    label_next = lead(label)
  ) 


#make the arrows work for the first entry
residence_connections <- residence_connections_prelim %>%
  # get first row of residence ----
slice(1) %>% 
  # manually modify for plotting ----
mutate(
  city_order = 1,
  label_next = label,
  lat_next = lat,
  long_next = long,
) %>% 
  # combine with all other residences ----
bind_rows(residence_connections_prelim) %>% 
  # last (7th) row irrelevant ----
slice(1:6) %>% 
  # keep what we neeed ----
dplyr::select(city_order, lat, long, lat_next, long_next, label_next)



# Base Map  ---------------------------------------------------------------
(base_map <- ggplot() +
   # plot states ----
 geom_polygon(
   data = us_states,
   aes(
     x     = long, 
     y     = lat, 
     group = group
   ),
   fill  = "#F2F2F2",
   color = "white"
 ) +
   # lines for pins ----
 geom_segment(
   data = residence,
   aes(
     x    = long,
     xend = long,
     y    = lat,
     yend = lat + 0.5
   ),
   color = "#181818",
   size = 0.3
 ) +
   # pin heads, a bit above actual location, color with R ladies lighter purple ----
 geom_point(
   data = residence,
   aes(
     x = long, 
     y = lat + 0.5
   ),
   size = 0.5,
   color = "#88398A"
 ) +
   theme_void())



# Animating the plot ------------------------------------------------------
anim <- base_map +
  # show arrows connecting residences ----
geom_curve(
  # do not include 1st residence in arrows as no arrow is intended ----
  # and inclusion messes up transition ---
  data = residence_connections %>% slice(-1),
  # add slight adjustment to arrow positioning ----
  aes(
    y     = lat - 0.1,
    x     = long,
    yend  = lat_next - 0.2,
    xend  = long_next,
    # group is used to create the transition ----
    group = seq_along(city_order)
  ),
  color = "#181818",
  curvature = -0.5,
  arrow = arrow(length = unit(0.02, "npc")),
  size  = 0.2
) +
  # add in labels for pins, with inward positioning ----
# show labels either top left or top right of pin ----
geom_richtext(
  data = residence_connections,
  aes(
    x     = ifelse(long_next < -100, long_next + 1, long_next - 1),
    y     = lat_next + 5,
    label = label_next,
    vjust = "top",
    hjust = ifelse(long_next < -100, 0, 1),
    # group is used to create the transition ----
    group = seq_along(city_order)
  ),
  size = 2,
  label.colour = "white",
  # R ladies purple ----
  color = "#562457",
  # R ladies font used in xaringan theme ----
  family = "Lato"
) +
  # title determined by group value in transition ----
ggtitle("Home {closest_state} of 6") +
  # create animation ----
transition_states(
  city_order,
  transition_length = 2,
  state_length = 5
) +
  # style title ----
theme(
  plot.title = element_text(
    color = "#562457",
    family = "Permanent Marker",
    size = 12
  )
)

# render and save transition ----
# the default nframes 100 frames, 150 makes the gif a bit longer for readability ----
# changing dimensions for output w/ height & width ----
# increasing resolution with res ----
animate(anim, nframes = 150, height = 2, width = 3, units = "in", res = 150)

