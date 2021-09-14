
# mapcan choropleth for Canada

library(mapcan)


#--- boundaries = province for geographic data at the province level
# boundaries = census 
# boundaries = ridings

#--- geographic data for a standard choropleth map
# type = standard

# maps that alter the geography based on the 
# population size at the province or census
# type = cartogram.
# mapcan::riding_binplot() 

# mapcan() will provide geographic data for the entire country. 
# province: only one province (or territory)
#         (options are NL, PE, NS, NB, QC, ON, MB, SK, AB, BC, YT, NT, and NU)



# --- basic data
mapcan(boundaries = province,
       type = standard) %>%
  head()



library(ggplot2)
pr_map <- mapcan(boundaries = province,
                 type = standard) %>%
  ggplot(aes(x = long, y = lat, group = group))



pr_map <- pr_map +
  geom_polygon() +
  coord_fixed()

pr_map

# axis ticks and background grid using theme_mapcan function
pr_map +
  theme_mapcan() +
  ## Add a title
  ggtitle("Map of Canada with Provincial/Territorial Boundaries")

# province_pop_annual data frame that is included in the mapcan package.
# dataset provides annual provincial/territorial population estimates dating back to 1971.

library(dplyr)
canada = mapcan::province_pop_annual
can2017 = canada %>% filter(canada$year == 2017)

pop_2017 <- mapcan::province_pop_annual %>%
  filter(year == 2017)

head(pop_2017)

# attach these numbers to every point on the polygons of the provinces
# use inner_join() 

pr_geographic <- mapcan(boundaries = province,
                        type = standard)


pr_geographic <- inner_join(pr_geographic, 
                            pop_2017, 
                            by = c("pr_english" = "province"))


# colour the provinces according to their population size, 
# set the population variable as a fill aesthetic.
pr_geographic %>%
  ggplot(aes(x = long, y = lat, group = group, fill = population)) +
  geom_polygon( col="white") +
  coord_fixed() +
  theme_mapcan() +
  scale_fill_viridis_c(name ="Population") +
  ggtitle("Canadian Population by Province (2017)") + theme(legend.position = "left")





# --------------
bc_ridings <- mapcan(boundaries = ridings,
                     type = standard,
                     province = BC)

head(bc_ridings)

# Plot geographic data with riding boundaries
ggplot(bc_ridings, aes(x = long, y = lat, group = group)) +
  geom_polygon() +
  coord_fixed() +
  theme_mapcan() +
  ggtitle("British Columbia \nFederal Electoral Ridings")

# Incorporate riding-level statistics
# federal_election_results (up to 2015) data frame that is included in the mapcan package.

bc_results <- mapcan::federal_election_results %>%
  # Restrict data to include just 2015 election results from BC
  filter(election_year == 2015 & pr_alpha == "BC")

head(bc_results)

bc_ridings <- inner_join(bc_results, bc_ridings, by = "riding_code")

bc_riding_map <- bc_ridings %>%
  ggplot(aes(x = long, y = lat, group = group, fill = party)) +
  geom_polygon() +
  coord_fixed() +
  theme_mapcan() +
  ggtitle("British Columbia \n2015 Federal Electoral Results")

bc_riding_map

#  change colors
bc_riding_map +
  scale_fill_manual(name = "Winning party",
                    values = c("blue", "springgreen3", "red", "Orange")) 












