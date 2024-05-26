#install.packages("sf")
#install.packages(c("sf", "raster", "sp"))

library(sf)
library(ggplot2)
library(viridis)

setwd("E:/projects/EastAndSouthAfrica")
easa = st_read(dsn='esa_admin1_region',layer='esa_admin1_region')
plot(easa$geometry)

setwd("E:/projects/EastAndSouthAfrica")
afri = st_read(dsn='Africa_Boundaries-shp')
plot(afri$geometry)

setwd("E:/projects/EastAndSouthAfrica")
globe = st_read(dsn='World_Countries',layer='World_Countries')
plot(globe$geometry)

women_incidence <- read_excel('Cancer data.xlsx', sheet = 'women')
men_incidence <- read_excel('Cancer data.xlsx', sheet = 'men')

names(women_incidence) <- c('country','incidences','asr')
str(women_incidence)

names(men_incidence)
names(men_incidence) <- c('country','incidences','asr')

women_incidence <- women_incidence[-1,]
men_incidence <- men_incidence[-1,]

women_incidence$gender <- 'women'
men_incidence$gender <- 'men'


men_women_incidence <- rbind(women_incidence,men_incidence )

# Summarize total incidences for each country
country_combined_totals <- men_women_incidence %>%
  group_by(country) %>%
  summarise(total_incidences = sum(incidences))

worldm49 <- read_excel('m49_codes.xlsx')

merged_data <- left_join(country_combined_totals, worldm49, 
                         by = c("country" = "Country"))

ggplot() +
  geom_sf(data = merged_data, aes(fill = total_incidences),) +  
  scale_fill_gradient(name = "Total Incidences", low = "lightblue", high = "darkblue") + 
  ggtitle("Cancer Incidence by Country") +
  theme_minimal()

globe$COUNTRY<-merged_data$total_incidences

merged_world_countries<-left_join(globe,merged_data, by= c("COUNTRY" = "country"))

merged_african_countries<-left_join(merged_data,afri, by= c("country" = "NAME_0"))

#### working
ggplot() +
  geom_sf(data = merged_world_countries, aes(fill = total_incidences),) +  
  scale_fill_gradient(name = "Total Incidences", low = "lightblue", high = "darkblue") + 
  ggtitle("Cancer Incidence by Country") +
  theme_minimal()

ggplot() +
  geom_sf(data = merged_african_countries, aes(fill = total_incidences),) +  
  scale_fill_gradient(name = "Total Incidences", low = "lightblue", high = "darkblue") + 
  ggtitle("Cancer Incidence by Country") +
  theme_minimal()


  
plot(men_women_incidence$afri)

globe$COUNTRY<- men_women_incidence$incidences

world_map <- map_data('world')
country_coords <- world_map%>%
  group_by(region)%>%
  summarise(long = mean(long), lat = mean(lat))

names(country_coords)[1] <- 'country'
men_women_incidence <- left_join(men_women_incidence, country_coords)

ggplot(men_women_incidence) + geom_sf(aes(fill = asr)) +
  scale_fill_viridis() + theme_bw()


# UI
ui <- fluidPage(
  titlePanel("Cancer Incidence Visualization"),
  
  # Sidebar layout
  sidebarLayout(
    sidebarPanel(
      # Add inputs or controls for user interaction
      selectInput("gender_selection", "Select Gender:", choices = c("All", "Men", "Women"), selected = "All"),
      checkboxInput("show_legend", "Show Legend", value = TRUE),
      br(),
      downloadButton("download_data", "Download Data")
    ),
    
    # Main panel
    mainPanel(
      tabsetPanel(
        tabPanel("Globe Map", plotOutput()),
        
        
      )
    )
  )
)


plot_men_women_map = function(selected_gender){
  map_gender = merged_men_women %>% filter(gender==selected_gender)
  print(selected_gender)
  
  # Extract the M49 codes for africa region countries
  merged_men_women <- merge(merged_data, men_women_incidence, by = c("country"="country"))
  # filter the merged data for continent countries using m49 codes
  merged_men_women = merged_men_women %>% filter(gender %in% men_women_incidence)
  
  plot_title = paste("Cancer Incidence in", selected_gender)
  # Plot the map for selected continent countries
  ggplot() +
    geom_sf(data = merged_men_women, aes(fill = total_incidences)) +  
    scale_fill_gradient(name = "Total Incidences", low = "green", high = "red") + 
    ggtitle(plot_title) +
    theme_minimal()
}



if(input$gender_selection=='All'){
  merged_men_women <- merge(merged_data, men_women_incidence, by = c("country"="country"))
  
  ggplot() +
    geom_sf(data = merged_men_women, aes(fill = total_incidences)) +
    scale_fill_gradient(name = "Gender Incidences", low = "green", high = "red") +
    ggtitle("Cancer Incidence by Gender") +
    theme_minimal()
}
else{
  if(input$gender_selection=='Men'){
    gender_map(input$gender_selection)
  }
}



# Server
server <- function(input, output,session) { }

shinyApp(ui)
