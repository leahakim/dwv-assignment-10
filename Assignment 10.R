#Author: Leah Kim 
#Date: 11/10/2026
#Topic: Election Maps

#Load packages 
install.packages(c("socviz","sf","statebins","ggrepel","maps", "usmap"))
library(tidyverse)
library(socviz)
library(sf)
library(statebins)
library(ggrepel)
library(maps)
library(dplyr)
library(ggplot2)
library(lattice)
library(usmap)

setwd('/Users/leahkim/Desktop/academics ✩ ° ｡⋆/PAI 741/Assignments /Assignment 10')

#Section 1 
#Fig 1. 2024 Election Winner by State
#Examine the dataset and pick columns and sample 
election24 |>
  select(state, fips, total_vote, r_points, party, census) |>
  sample_n(5)
#Create hex colors for parties 
party_colors <- c("Democratic" = "#2E74C0",
                  "Republican" = "#CB454A")
#Draw a map 
us_states <- map_data('state') |> as_tibble()
us_states
us_states |>
  ggplot(mapping = aes(x = long, y = lat)) +
  geom_point(size = 0.1)
us_states |>
  ggplot(mapping = aes(x=long, y=lat, group = group)) +
  geom_polygon(fill = "white", color = "black")

#Merge data sets 
elections24_sf <- left_join(states_sf, election24, 
                            by = join_by(fips, st, state, census))
elections24_sf

fig1<- elections24_sf |>
  ggplot() + 
  geom_sf(aes(fill=party)) + 
  scale_fill_manual(values = party_colors) + 
  labs(title = "Election Results 2024") + 
  theme_void()
fig1

#Fig 2. 2024 Election Winner by County
names(election24_county_df)
colnames(election24_county_df)
unique(election24_county_df$winner)
str(counties_sf)
#Merge data 
counties24_sf <- left_join(counties_sf, election24_county_df, by ="fips")

fig2 <- counties24_sf |>
  ggplot(mapping = aes(fill = winner)) + 
  geom_sf(color = "white", linewidth = 0.05) +
  scale_fill_manual(values = party_colors) +
  labs(title = "Election Results 2024 by County") + 
  theme_void()
fig2
#Fig 3. 2024 Population Density by County 
counties_sf |>
  select(fips:pop, pop_dens_disc) |>
  sample_n(10)
fig3 <- counties_sf |>
  ggplot(mapping = aes(fill = pop_dens_disc)) +
  geom_sf(linewidth = 0.05, color = "black") +
  scale_fill_discrete(palette = "Greens") + 
  guides(fill = guide_legend(nrow = 1)) + 
  labs(fill = "People per square mile") + 
  theme_socviz_map()
fig3

#Fig 4. % Black by County 
fig4 <- counties_sf |>
  ggplot(mapping = aes(fill = black_disc)) + 
  geom_sf(linewidth = 0.01, color = "gray80") +
  scale_fill_brewer(palette = "Purples") + 
  guides(fill = guide_legend(nrow = 1)) +
  labs(fill = "Percent") + 
  theme_void()
fig4
#Section 2: On the state level 
#Load new data 
elections26 <- read_csv('/Users/leahkim/Desktop/academics ✩ ° ｡⋆/PAI 741/Data for HW/midterms_2026_preliminary.csv') |>
  filter(!is.na(candidate)) |>
  mutate(race = if_else(office == "Governor", "Governor", "Senate"))
names(elections26)
unique(elections26$office)

#Check all the states 
elections26 |>
  distinct(state, race) |>
  arrange(race, state) 


#Create a winners to see the votes 
winners26 <- elections26 |>
  group_by(state, race) |>
  arrange(desc(vote_pct), .by_group = TRUE) |>
  summarise(
    winner = if(isTRUE(vote_pct[1] == vote_pct[2])) NA_character_ else party[1], 
    .groups = "drop"
  ) |>
  mutate(winner = case_when(winner == "D" ~ "Democratic",
                            winner == "R" ~ "Republican",
                            TRUE ~ NA_character_))

#Fig 5. 2026 Senate Winner 
#Seeing which states didn't have a midterm
senate_sf |>
  st_drop_geometry() |>
  filter(is.na(winner)) |>
  select(state)

senate_sf <- states_sf |>
  left_join(filter(winners26, race == "Senate"), by = "state")
fig5 <- senate_sf |>
  ggplot(mapping= aes(fill = winner)) + 
  geom_sf(color = "white") + 
  scale_fill_manual(values = party_colors, na.value = "grey85") + 
  labs(title = "2026 Senate Winner by State",
       caption = "Grey: no race/results available yet.") + 
  theme_void()
fig5
#Fig 6. 2026 Governor Winner 
gov_sf <- states_sf |>
  left_join(filter(winners26, race == "Governor"), by = "state")
#Checking which states didn't have a Governor race
gov_sf |>
  st_drop_geometry() |>
  filter(is.na(winner)) |>
  select(state)

fig6 <- gov_sf |>
  ggplot(mapping = aes(fill = winner)) +
  geom_sf(color = "white") + 
  scale_fill_manual(values = party_colors, na.value = "grey85") + 
  labs(title = "2026 Governor Winner by State",
       caption = "Grey: no race/results available yet.") + 
  theme_void()
fig6

#Section 3: Create maps that show swing from 2024-2026 
#Margins for plots 
margins26 <- elections26 |> 
  filter(party %in% c("D", "R")) |>
  group_by(state, race, party) |>
  slice_max(vote_pct, n = 1, with_ties = FALSE) |>
  ungroup() |>
  select(state, race, party, vote_pct) |>
  pivot_wider(names_from = party, values_from = vote_pct) |>
  mutate(r_margin26 = R - D)

#Swing results 
results26 <- winners26 |>
  left_join(margins26, by = c("state", "race")) |>
  left_join(election24 |> select(state, r_points), by = "state") |>
  mutate(swing = r_margin26 - r_points) 
  
#Fig 7. 2026 Senate 
senate_sf <- states_sf |>
  left_join(filter(results26, race == "Senate"), by = "state")

fig7 <- senate_sf |>
  ggplot(mapping = aes(fill = swing)) + 
  geom_sf(color = "white") + 
  scale_fill_gradient2(low = party_colors[["Democratic"]], mid="white",
                      high = party_colors[["Democratic"]], midpoint = 0,
                      na.value = "grey85", 
                      name = "Swing (pts)") + 
  labs(title = "Swing Results in Senate Races (2024-2026)", 
       caption = "Blue: shift towards Dem; Red: shift towards Rep") + 
  theme_void()
fig7
#Fig 8. 2026 Governor 
gov_sf <- states_sf |>
  left_join(filter(results26, race == "Governor"), by = "state")

fig8 <- gov_sf |>
  ggplot(mapping = aes(fill = swing)) + 
  geom_sf(color = "white") +
  scale_fill_gradient2(low = party_colors[["Democratic"]], mid="white", 
                       high = party_colors[["Republican"]], midpoint = 0,
                       na.value = "grey85",
                       name = "Swing(pts)") + 
  labs(title = "Swing Results in Governor Races (2024-2026)", 
       caption = "Blue: shift towards Dem; Red: shift towards Rep") +
  theme_void()
fig8
#Section 4: Plot location of SU on national map 
#Fig 9. SU Location on US map coordinates 

#Create a data frame with city's 
syracuse <- tibble(place = "Syracuse University",
                   long = -76.1474,
                   lat = 43.0481)

#Build a star 
make_star <- function(long, lat, r_out = 1.5, r_in = r_out *0.4) {
  angles <- pi/2 + (0:10) * pi/5
  radii <- c(rep(c(r_out, r_in), 5), r_out)
  tibble(long = long + radii * cos(angles) / cos(lat*pi/180),
         lat = lat + radii * sin(angles))
}

syracuse_star <- make_star(syracuse$long, syracuse$lat)

#Plotting 
fig9 <- us_states |>
  ggplot(mapping = aes(x = long, y = lat, group = group)) +
  geom_polygon(fill = "white", color = "black") +
  geom_polygon(data = syracuse_star, mapping = aes(x = long, y = lat),
               fill = "#F76900", color = "#000E54", linewidth = 0.4,
               inherit.aes = FALSE) +
  coord_quickmap() + 
  labs(title = "Location of SU") + 
  theme_void()
fig9

#Section 5: Tornaods across the US and also in TX 
#Load Data
tornado <- read_csv('/Users/leahkim/Desktop/academics ✩ ° ｡⋆/PAI 741/Data for HW/1950-2025_actual_tornadoes.csv') |>
  filter(yr >= 2000, !st %in% c("AK", "HI", "PR", "VI"),
         !is.na(slat), slat !=0, slon !=0, mag>=0)
tornado_sf <- st_as_sf(tornado, coords = c("slon", "slat"), crs = 4326)

#Make the base maps for US and then TX counties 
states <- st_as_sf(maps::map("state", fill=TRUE, plot = FALSE))
texas <- st_as_sf(maps::map("county", "texas", fill=TRUE, plot=FALSE))

#Now reference the coordinate systems with numbers for long/lat & US/TX maps
states_us <- st_transform(states, 5070)
tornado_us <- st_transform(tornado_sf, 5070)
texas_counties <- st_transform(texas, 3083)
tornado_texas <- tornado_sf |> filter(st == "TX") |> st_transform(3083)

#Fig 10
#Plotting time 
fig10 <- ggplot() + 
  geom_sf(data = states_us, fill = "grey85", color = "white") + 
  geom_sf(data = tornado_us |> filter(mag >= 2),
           aes(color = factor(mag)), size = 0.6, alpha= 0.6) +
  scale_colour_viridis_d(option = "inferno", end = 0.85, name = "EF scale") +
  labs(title = "Significant Tornados, 2000-2023", 
       caption = "Source: NOAA Storm Predicton Center") + 
  theme_void()
fig10

#Fig 11 
#Mutate the information to get the TX counties 
texas_counties <- texas_counties |>
  mutate(area_sqmi = as.numeric(st_area(texas_counties)) / 2.59e6)

#Join the information for TX 
counts <- st_join(tornado_texas, texas_counties["ID"]) |>
  st_drop_geometry() |>
  filter(!is.na(ID)) |>
  count(ID, name = "n_tornadoes")

#Make code for the TX map with square miles 
tx_map <- texas_counties |>
  left_join(counts, by = "ID") |>
  mutate(n_tornadoes = replace_na(n_tornadoes, 0),
         per_1000sqmi = n_tornadoes/area_sqmi * 1000)

#Plotting
fig11 <- ggplot(tx_map) + 
  geom_sf(aes(fill = per_1000sqmi), color = "white", linewidth = 0.1) + 
  scale_fill_viridis_c(option = "magma", direction = -1,
                       name = "Tornadoes per/n*1,000 sq mi") + 
  labs(title = "Tornado Frequency in Texas Counties, 2000-2023",
       caption = "Source: NOAA Storm Prediction Center") + 
  theme_void()
fig11

#Save all plots 
getwd()
list.files("figures")
dir.create("figures", showWarnings = FALSE)
ggsave("figures/figure1.png", plot = fig1, width = 8, height = 5, dpi = 300)
ggsave("figures/figure2.png", plot = fig2, width = 8, height = 5, dpi = 300)
ggsave("figures/figure3.png", plot = fig3, width = 8, height = 5, dpi = 300)
ggsave("figures/figure4.png", plot = fig4, width = 8, height = 5, dpi = 300)
ggsave("figures/figure5.png", plot = fig5, width = 8, height = 5, dpi = 300)
ggsave("figures/figure6.png", plot = fig6, width = 8, height = 5, dpi = 300)
ggsave("figures/figure7.png", plot = fig7, width = 8, height = 5, dpi = 300)
ggsave("figures/figure8.png", plot = fig8, width = 8, height = 5, dpi = 300)
ggsave("figures/figure9.png", plot = fig9, width = 8, height = 5, dpi = 300)
ggsave("figures/figure10.png", plot = fig10, width = 8, height = 5, dpi = 300)
ggsave("figures/figure11.png", plot = fig11, width = 8, height = 5, dpi = 300)

file.exists("figures/figure1.png")
getwd()
