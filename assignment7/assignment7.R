library(sf)
library(spData)
data(world)
library(dplyr)
library(tidyr)
library(ggplot2)

# 1.1 Inspection of an sf object ------------------------------------------

# a) Load the world dataset and inspect its structure. Run class(world), names(world),
# and nrow(world). In a comment, describe what makes an sf object different from a
# regular R data frame. What is the geometry column, and how is it stored?

class(world)
# [1] "sf"         "tbl_df"     "tbl"        "data.frame"

names(world)
# [1] "iso_a2"    "name_long" "continent" "region_un"
# [5] "subregion" "type"      "area_km2"  "pop"      
# [9] "lifeExp"   "gdpPercap" "geom"

nrow(world)
# [1] 177

# An sf object is a regular data frame with an extra geometry column that stores 
# the spatial shapes. The geometry column is "sticky", standard dplyr retain it 
# automatically, so spatial attributes travel with the data. 

# b)  Check the coordinate reference system (CRS) with st crs(world). What EPSG 
# code does the dataset use? In a comment, explain what WGS84 means and why it is 
# the standard CRS for global geographic data.

st_crs(world)
# Coordinate Reference System:
#User input: EPSG:4326 
#wkt:
#  GEOGCRS["WGS 84",
#          DATUM["World Geodetic System 1984",
#                ELLIPSOID["WGS 84",6378137,298.257223563,
#                          LENGTHUNIT["metre",1]]],
#          PRIMEM["Greenwich",0,
#                 ANGLEUNIT["degree",0.0174532925199433]],
#          CS[ellipsoidal,2],
#          AXIS["geodetic latitude (Lat)",north,
#               ORDER[1],
#               ANGLEUNIT["degree",0.0174532925199433]],
#          AXIS["geodetic longitude (Lon)",east,
#               ORDER[2],
#               ANGLEUNIT["degree",0.0174532925199433]],
#          USAGE[
#            SCOPE["Horizontal component of 3D system."],
#            AREA["World."],
#            BBOX[-90,-180,90,180]],
#          ID["EPSG",4326]]

# The dataset uses EPSG:4326 (WGS84- world geodetic system 1984)
# WGS84 is the global standard coordinate system used by GPS and most web mapping tools. 
# Coordinates are expressed in decimal degrees of longitude (east-west) and 
# latitude (north-south). This makes it ideal for global datasets where a common 
# method is needed across all regions. 

# c) Use st geometry type(world) and unique(st geometry type(world)) to inspect the
# geometry type. In a comment, explain what a MULTIPOLYGON is and give two concrete 
# examples of countries that would require multiple polygons to represent their
# territory. 
unique(st_geometry_type(world))
# [1] MULTIPOLYGON
# 18 Levels: GEOMETRY POINT LINESTRING ... TRIANGLE
# The geometry type is multipolygon, this is a collection of one or more polygons
# treated as a single geographic feature. Countries require multiple polygons when 
# their territory is not a single continuous land mass. 

# d) Produce a quick map of GDP per capita using base R graphics, describe what 
# is seen, which regions appear wealthiest and which poorest? 

pdf("world_gdp_base.pdf")
plot(world["gdpPercap"])
dev.off()

# The wealthiest regions are the dark end of the scale, the wealthy areas are 
# Europe, while the poorest are Africa. 


# 1.2 Attribute operations ------------------------------------------------

# a) Using filter(), create a subset of world containing only African countries. 
# Call it africa. How many African countries are in the dataset? Plot africa
# ["gdpPercap"] using base graphics. In a comment, note whether the country count 
# matches your expectations.
library(dplyr)

africa <- filter(world, continent == "Africa")
nrow(africa)
#[1] 51
plot(africa["gdpPercap"], main = "GDP per capita -- Africa")

# The dataset has 51 African countries, there are 54 sovereign African states so 
# the amount in the dataset is slighly below expected. 

# b) Add a new variable pop millions equal to population divided by 1,000,000 using
# mutate(). Then compute the average GDP per capita by continent using group by()
# and summarise(). 

world = world %>%
  mutate(pop_millions = pop / 1e6)

gdp_by_continent = world %>%
  group_by(continent) %>%
  summarise(mean_gdpPercap = mean(gdpPercap, na.rm = TRUE))

print(st_drop_geometry(gdp_by_continent))

#  A tibble: 8 × 2
#continent               mean_gdpPercap
#* <chr>                            <dbl>
#  1 Africa                           5042.
#2 Antarctica                        NaN 
#3 Asia                            20026.
#4 Europe                          29451.
#5 North America                   18384.
#6 Oceania                         15828.
#7 Seven seas (open ocean)           NaN 
#8 South America                   13762.

# When summarise() is called on a grouped sf object, it unions the geometries 
# within each group and retains the resulting geometry column. To obtain a plain
# data frame without spatial information use st_drop_geometry() before or after 
# the summary step. This avoids carrying unneeded geometry through purely tabular 
# analyses. 

# c)  Sort the African countries by GDP per capita (descending) using arrange(). 
# Print the top 5 rows with name long and gdpPercap. Name the five countries 
# in a comment.
africa_sorted = africa %>%
  arrange(desc(gdpPercap)) %>%
  select(name_long, gdpPercap)

print(head(st_drop_geometry(africa_sorted), 5))

# # A tibble: 5 × 2
# name_long         gdpPercap
# <chr>                 <dbl>
#1 Equatorial Guinea    31543.
#2 Gabon                16679.
#3 Libya                16372.
#4 Botswana             15915.
#5 Algeria              13483.

# The 5 African countries with the highest GDP per capita are shown in the tibble
# above in order from 1-5 they are: Equatorial Guinea, Gabon, Libya, Botswana,
# and Algeria. 


# 1.3. Simple visualization with ggplot2 ------------------------------------
# The geom sf() layer in ggplot2 allows you to plot sf objects using the standard 
# grammar of graphics.
# a) Make a choropleth map of the world colored by gdpPercap, describe patterns. 
library(ggplot2)
ggplot(world) +
  geom_sf(aes(fill = gdpPercap)) +
  scale_fill_viridis_c(option = "plasma", na.value = "grey80",
                       name = "GDP per capita") +
  theme_void() +
  labs(title = "GDP per capita by country")


ggsave("world_gdp.pdf", width = 10, height = 5)

# The wealthiest regions appear to be North America and Oceania. The poorest  
# regions appear to be Africa and south east Asia. 

# b) Make the same map restricted to the africa object. Use scale fill viridis c() with
# option = "magma" and save as africa gdp.pdf. Describe the variation in GDP per
# capita across African countries.
ggplot(africa) +
  geom_sf(aes(fill = gdpPercap)) +
  scale_fill_viridis_c(option = "magma", na.value = "grey80",
                       name = "GDP per capita") +
  theme_void() +
  labs(title = "GDP per capita -- Africa")

# There is a lot of variation within Africa, central Africa has the lowest GDP 
# per capita with north and southernmost tips of Africa having the wealthiest 
# countries. 

# c) Improve the Africa map by adding white country borders: modify geom sf() to
# include color = "white" and linewidth = 0.3. Save as africa gdp borders.pdf. In a
# comment, explain how the border layer improves readability.

ggplot(africa) +
  geom_sf(aes(fill = gdpPercap), color = "white", linewidth = 0.3) +
  scale_fill_viridis_c(option = "magma", na.value = "grey80",
                       name = "GDP per capita") +
  theme_void() +
  labs(title = "GDP per capita -- Africa (with borders)")

ggsave("africa_gdp_borders.pdf", width = 7, height = 6)

# The border layer improves readability because the white contrast of the lines 
# helps differentiate between the countries. This is especially useful in the 
# central area where most of the countries are at the same level of GDP and are 
# impossible to tell apart since they are all the same color. 

##################################################################################################
# PART 2: TAKE HOME (point data & spatial joins) --------------------------
##################################################################################################

df <- read.csv("C:/Users/lesle/OneDrive/Desktop/repos/aqmss2_2026/assignment7/conflict_events.csv")
install.packages(c("rnaturalearth", "rnaturalearthdata", "sf"))
library(sf)
library(rnaturalearth)
library(ggplot2)
library(ggplot2)
library(dplyr)

# 2.1 Converting tabular data to sf ---------------------------------------

# a) Convert the events data frame to an sf object. Hint: use st as sf(), specifying the
# coordinate columns with the coords argument and the CRS with crs = 4326. Run
# class() and st crs() on the result to verify it worked. In a comment, explain what
# st as sf() does: what does the coords argument specify, and what does crs = 4326
# mean?

names(df)
# [1] "event_id""year""longitude""latitude""fatalities"[6]"event_type"

library(sf)

events_sf <- st_as_sf(df,
                      coords = c("longitude", "latitude"),
                      crs = 4326)

class(events_sf)
# [1] "sf"         "data.frame"

st_crs(events_sf)
#Coordinate Reference System:
#  User input: EPSG:4326 
#wkt:
#  GEOGCRS["WGS 84",
#          ENSEMBLE["World Geodetic System 1984 ensemble",
#                   MEMBER["World Geodetic System 1984 (Transit)"],
#                   MEMBER["World Geodetic System 1984 (G730)"],
#                   MEMBER["World Geodetic System 1984 (G873)"],
#                   MEMBER["World Geodetic System 1984 (G1150)"],
#                   MEMBER["World Geodetic System 1984 (G1674)"],
#                   MEMBER["World Geodetic System 1984 (G1762)"],
#                   MEMBER["World Geodetic System 1984 (G2139)"],
#                   MEMBER["World Geodetic System 1984 (G2296)"],
#                   ELLIPSOID["WGS 84",6378137,298.257223563,
#                             LENGTHUNIT["metre",1]],
#                   ENSEMBLEACCURACY[2.0]],
#          PRIMEM["Greenwich",0,
#                ANGLEUNIT["degree",0.0174532925199433]],
#          CS[ellipsoidal,2],
#          AXIS["geodetic latitude (Lat)",north,
#               ORDER[1],
#               ANGLEUNIT["degree",0.0174532925199433]],
#          AXIS["geodetic longitude (Lon)",east,
#               ORDER[2],
#               ANGLEUNIT["degree",0.0174532925199433]],
#          USAGE[
#            SCOPE["Horizontal component of 3D system."],#
#            AREA["World."],
#            BBOX[-90,-180,90,180]],
#          ID["EPSG",4326]]

# st_as_sf() converts a regular data fram into a spatial (sf) object. The coords 
# argument specifies which columns contain the geographic coordinates
# (longitude and latitude) which are used to create point geometries. 
# The crs = 4326 sets the coordinate reference system to WGS84, the standard
# system for GPS coordinates in latitude & longitude. 

# b) How many events are in the dataset? Use nrow() and table(events sf$event type)
# to show the count by event type. In a comment, which event type is most common?
nrow(events_sf)
#[1] 68354
names(events_sf)
# [1] "event_id" "year" "fatalities" "event_type" "geometry" 
table(events_sf$event_type)
#  non-state   one-sided state-based 
#   10418       24449       33487

# There are 3 types of events: non-state, one-sided, and state-based events. 
# The most common event type are the state-based events. 

# c) Make a map of conflict events overlaid on the world polygon. Use ggplot() with
# two geom sf() layers: the first for the world polygons (as a grey background) and the
# second for events sf colored by event type. Save it with ggsave(). In a comment,
# describe the geographic pattern. In which regions are conflict events most 
# concentrated?

# Get world polygons as an sf object
world <- ne_countries(scale = "medium", returnclass = "sf")
str(world$continent)
ggplot() +
  geom_sf(data = world, fill = "lightgrey", color = "white") +
  geom_sf(data = events_sf, aes(color = event_type), size = 0.8, alpha = 0.6) +
  theme_minimal() +
  labs(color = "Event Type", title = "Global Conflict Events")

ggsave("conflict_map.png", width = 10, height = 6)

summary(events_sf$geometry)
# Extract coordinates from sf object
coords <- st_coordinates(events_sf)
summary(coords)

# There are only conflicts in the continent of Africa. After running some checks 
# on the data's geographic locations there are only coordinates in Africa, even
# though there was an attempt at mapping data globally, there is only data in
# Africa. 


# 2.2 Spatial join: events to countries -----------------------------------

# a) Use st join() to assign country attributes (e.g. name long, continent, gdpPercap)
#from the world polygon to each conflict event. Before joining, verify that both objects
#share the same CRS. Run nrow() on the result and verify it equals nrow(events sf).
#In a comment, explain what st join() is doing: how does it determine which country
#polygon each event point falls within? Why is checking the CRS before joining
#important?

# Check CRS match
st_crs(events_sf)
# Coordinate Reference System:
#User input: EPSG:4326 
#wkt:
#  GEOGCRS["WGS 84",
#          ENSEMBLE["World Geodetic System 1984 ensemble",

st_crs(world)
# Coordinate Reference System:
#User input: EPSG:4326 
#wkt:
#  GEOGCRS["WGS 84",
#          ENSEMBLE["World Geodetic System 1984 ensemble",

world <- st_transform(world, crs = st_crs(events_sf))

# Spatial join
events_joined <- st_join(events_sf, world)

# Check row count
nrow(events_joined)
# [1] 68354
nrow(events_sf)
# [1] 68354

# st_join creates a spatial join, this matches each point (event) to a polygon 
# (country) based on location. Checking the CRS before merging is important 
# because if the coordinate systems the geometries won't align correctly in space. 
# It can lead to incorrect or missing matches. 

# b) Some events may not match any country polygon (e.g., events at sea, on 
# islands, or exactly on a border). Check with sum(is.na(events joined$name long)). 
# What fraction of events has no matching country? In a comment, list two possible 
# reasons why a point might not match any polygon.

# Count missing matches
sum(is.na(events_joined$name_long))
# [1] 1010
# Fraction missing
mean(is.na(events_joined$name_long))
# [1] 0.01477602

# The fraction of events that have no matching country is 0.01477602. There may 
# be no matching polygon for a case that is not on the central land mass of the 
# assigned country polygon (at sea or a disputed territory). Another reason 
# would be if the point lies on a border, causing ambiguity in which polygon 
# contains it. 

# c) Count the number of events and total fatalities per country. Hint: filter out events
#with no matching country, then use group by() and summarise() with n() and sum().
#Arrange by descending event count and print the top 10 (use st drop geometry() to
#get a clean table). In a comment, are the results consistent with your knowledge of
#contemporary armed conflicts?

library(dplyr)

country_summary <- events_joined %>%
  filter(!is.na(name_long)) %>%
  group_by(name_long) %>%
  summarise(
    n_events = n(),
    total_fatalities = sum(fatalities, na.rm = TRUE)
  ) %>%
  arrange(desc(n_events))

# Show top 10 (drop geometry for clean table)
country_summary %>%
  st_drop_geometry() %>%
  head(10)
#  name_long                        n_events total_fatalities
#<chr>                               <int>            <int>
#  1 Democratic Republic of the Congo     8767           136880
#2 Nigeria                              7467            73889
#3 Somalia                              6420            56240
#4 Ethiopia                             5343           384676
#5 Algeria                              4057            20959
#6 Sudan                                3657            65944
#7 Burundi                              3150            53355
#8 South Africa                         2711             5084
#9 Rwanda                               2668           792882
#10 Mali                                 2547            17474

# Since the dataset only contains data from countries in Africa the results are
# consistent. The regions listed have experienced prolonged/intense armed 
# conflicts so the list makes sense. 


# 2.3 Choropleth of conflict intensity  -----------------------------------

#a) Join the event counts back to the world polygon data. Hint: first use st drop 
#geometry() on the event counts (since it is still an sf object), then use left 
#join() to merge by country name. Replace NA values with 0 for countries with no 
#events (see replace na() from tidyr). Verify that the row count matches nrow(world).

# Drop geometry from country summary
country_df <- country_summary %>%
  st_drop_geometry()

# Join to world polygons
world_events <- world %>%
  left_join(country_df, by = c("name_long"))

# Replace NA with 0
world_events <- world_events %>%
  mutate(
    n_events = replace_na(n_events, 0),
    total_fatalities = replace_na(total_fatalities, 0)
  )

# Verify row counts match 
nrow(world_events)
#[1] 242

nrow(world)
#[1] 242

# st_drop_geometry converts the sf object into a regular data frame so it can be 
# joined. Left_join attaches the event data to each country polygon using 
# name_long. Countries with no events get NA, which are replaced with 0. The row 
# count should match nrow(world) because data was added not removed. 

#b) Make a choropleth map of conflict event counts by country using geom sf() with
#n events as the fill variable. Use scale fill distiller() with the "Reds" palette.
#Save with ggsave(). In a comment, describe the map. Does the geographic pattern
#match the event-level map from question 2.1c?

ggplot(world_events) +
  geom_sf(aes(fill = n_events), color = "white") +
  scale_fill_distiller(palette = "Reds", direction = 1) +
  theme_minimal() +
  labs(
    fill = "Number of Events",
    title = "Conflict Events by Country"
  )

ggsave("conflict_choropleth.png", width = 10, height = 6)

# The map shows events in Africa, just as the map from 2.1c. Few countries have 
# very high counts of conflict events. Most countries range from light (closer to zero)
# to middle of the scale pale red, with a handful of dark red countries. 

# c) Make a second map using log-transformed counts: use log1p(n events) as the 
#fill variable (so countries with zero events are handled). Use scale fill 
#distiller() with palette = "YlOrRd", direction = 1, and name = "Log(events+1)". 
#Save as conflict log map.pdf. In a comment, explain why the log transformation 
#is useful and what it reveals that the raw count map did not.
ggplot(world_events) +
  geom_sf(aes(fill = log1p(n_events)), color = "white") +
  scale_fill_distiller(
    palette = "YlOrRd",
    direction = 1,
    name = "Log(events+1)"
  ) +
  theme_minimal() +
  labs(
    title = "Log-Transformed Conflict Events by Country"
  )

ggsave("conflict_log_map.pdf", width = 10, height = 6)

# The log transformation reduces the dominance of extreme values and makes 
# moderate levels of conflict more visible. This is why more countries appear 
# darker in the log map. The log transformation compresses high values and
# expands lower ones, making moderate level conflicts more visible and resulting
# in more countries appearing darker compared to the raw count map. 

