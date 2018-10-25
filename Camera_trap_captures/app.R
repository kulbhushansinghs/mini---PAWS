library(shiny)
library(rdrop2)
library(tidyverse)
library(sf)
library(ggsn)
drop_auth()   ## Login to the PAWS dropbox folder

hp <- st_read('IND_adm1.shp')
hp <- hp %>% filter(NAME_1 == 'Himachal Pradesh')
sltheoretical <- st_read('HP_3200_5200UTM.shp')

trap_info <- drop_search('trap_info  csv')
trap_info <- grep(unlist(lapply(lapply(trap_info$matches,'[[','metadata'),'[[','path_lower')), pattern = 'secr', value = T)
dat <- do.call(bind_rows,lapply(trap_info[1:6], drop_read_csv))
dat <- dat %>% dplyr::select(Area,Station,Longitude,Latitude)
colnames(dat) <- c('Region','Station','long','lat')
dat$Station <- trimws(dat$Station,which = 'both')


locations <- drop_search('species  csv')
locations <- grep(unlist(lapply(lapply(locations$matches,'[[','metadata'),'[[','path_lower')), pattern = 'secr', value = T)
species <- do.call(bind_rows,lapply(locations[1:6], drop_read_csv))
species$Station <- trimws(species$Station,'both')

species$Species[species$Species == 'Red fox'] <- 'Red Fox'
sp_sum <- species %>% group_by(Station,Species) %>% tally()
samp_cam <- expand.grid(Station = unique(species$Station), Species = unique(species$Species))
sp_sum <- left_join(samp_cam,sp_sum)
covered_regions <- na.omit(unique(right_join(dat,sp_sum) %>% dplyr::select(Region)))
hp <- rbind(hp,hp,hp)

ui <- fluidPage(
   
   # Application title
   titlePanel("PAWS Himachal"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
         selectInput('sp','Select Species',choices = unique(species$Species))
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
         plotOutput("distPlot",width = '100%')
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$distPlot <- renderPlot({
    dat_sp <- sp_sum %>% filter(Species == input$sp)
    dat_sp <- left_join(dat,dat_sp)
    dat_sp$n[dat_sp$Region %in% covered_regions$Region & is.na(dat_sp$n)] <- 0
    dat_sp <- st_as_sf(dat_sp, coords = c('long','lat'))
    st_crs(dat_sp) <- 4326
    capture_plot <- ggplot() +
      geom_sf(data = hp, fill = 'grey')+
      geom_sf(data = sltheoretical, fill = 'palegreen3', show.legend = F)+
      geom_sf(data = dat_sp[dat_sp$n == 0,], fill = 'white', shape = 21, show.legend = 'point',size =3)+
      geom_sf(data = dat_sp[!is.na(dat_sp$n)&dat_sp$n >0,], aes(fill = n), shape = 21, show.legend = 'point',size =3)+
      scale_fill_gradient(low = 'orange',high = 'red', name = paste(input$sp,'Captures', sep = '\n'))+
      geom_sf(data = dat_sp[is.na(dat_sp$n),], shape = 21, show.legend = T,size =2, fill = 'grey30', alpha = 0.8)+
      geom_sf(data = hp, fill = NA, colour = 'black',size = 1)+
      north(hp,symbol = 12) +
      scalebar(hp,dd2km = T, dist = 50) +
      blank()
    
    capture_plot
    
  },width = 1000, height = 750)
  
}

# Run the application 
shinyApp(ui = ui, server = server)

