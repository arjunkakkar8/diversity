library(leaflet)
library(ggmap)
library(sp)
library(scatterpie)

locations <- read.csv("building_locations.csv", header = TRUE, stringsAsFactors = FALSE)%>%
  mutate(lat = as.numeric(lat), long = as.numeric(long))

plot_data <- building_rosters %>% 
  group_by(Building)%>%
  count(predrace)%>%
  spread(predrace, n, fill = 0) %>%
  mutate(total = p_asi+p_bla+p_his+p_oth+p_whi)%>%
  left_join(locations)

williamsmap <- get_map(location = c(-73.20554, 42.71343), zoom = 16)
ggmap(williamsmap) + 
  geom_point(aes(x=long, y=lat), 
             data=plot_data, 
             col="orange", 
             alpha=0.4,
             size = plot_data$total/5)+
  scale_size_continuous(range=range(plot_data$total))

ggmap(williamsmap)+
  geom_scatterpie(aes(x = long, y = lat, group = Building, r = total/300000), 
                  data = plot_data, 
                  cols = c("p_asi","p_bla","p_his","p_oth","p_whi"), 
                  color = NA, 
                  alpha = 1)

m <- leaflet(data = plot_data) %>%
  addTiles()%>%
  setView(-73.20554, 42.71343, zoom = 16)%>%
  addCircles(~long, ~lat, 
             popup = ~paste(paste0("<b>Ethinic/Racial Composition</b>"),
                            paste0("Percent White: ", formatC(100*p_whi/total, digits = 2),"%"),
                            paste0("Percent Hispanic: ", formatC(100*p_his/total, digits = 2),"%"),
                            paste0("Percent Asian: ", formatC(100*p_asi/total, digits = 2),"%"), sep = "<br/>"), 
             label = ~as.character(Building),
             radius = ~total^(1/2)*2)
m
