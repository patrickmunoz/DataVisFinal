#Group 2 west
#Anthony Lu
#Nat Rivera
#Patrick Munoz

#import libraries

library(shiny)
library(shinydashboard)
library(sf)
library(leaflet)
library(ggplot2)
library(lubridate)
library(plyr)
#library(DT)

#global settings
#setwd("~livesession4") #This is here to be run in the console, not on the server
#enforecement <- read.csv("Code_Enforcement_Cases.csv")
#lights <- read.csv("Street_Lights.csv")



#TAB 1 Reading in & manipulating data for first tab on business table
business <- read.csv("Business_Licenses_geocoded.csv")
business2 <- business %>% dplyr::select("Business_N", "Street_Add","Zip_Code", "Business_P","Classifica",
                                        "License_Ex","License__1" ,"Council_Di")
business2 <- plyr::rename(business2, c("Business_N" = "Business_Name", "Street_Add" = "Street_Address" ,
                                 "Business_P"="Phone_Number" ,"Classifica" = "Classification" ,
                                "License_Ex" = "License_Expiration" ,"License__1" = "License_Status",
                                 "Council_Di" = "District"  ))
business2 <- dplyr::filter(business2, business2$Zip_Code < 99999)
business2 <- dplyr::filter(business2, business2$District != "")
business2$License_Expiration <- as.Date(business2$License_Expiration)
business2 <- na.omit(business2)


#TAB 2 Read in data for second tab on parks and facilities
#create the polygons for council and the color palettes
council <- st_read("City_Council_Districts.shp")
factpal <- colorFactor(topo.colors(5), council$Dist)
    #Data for parks and public facilities
parks <- read.csv("Parks_Locations_and_Features.csv")
public_set <- read.csv("Public_Facilities.csv")
public <- public_set
public.spatial <- public %>% st_as_sf(coords = c("Lon", "Lat")) %>% st_set_crs(st_crs(council))
public.spatial.set <- public.spatial
google.crs <- 3857
council.google <- council %>% st_transform(crs = google.crs)
st_crs(council.google)
ov <- st_intersects(council, public.spatial)
ov <- st_join(x = public.spatial, y = council %>% dplyr::select(Dist))
ov %>% st_set_geometry(NULL) %>% dplyr::group_by(Dist) %>% dplyr::summarize(public = dplyr::n())

#TAB 3 DATA read in data for schools
school <- st_read("School_Boundaries.shp")
school.data <- school %>% st_set_geometry(NULL)
aband <- st_read("Abandoned_Property_Parcels.shp")
aband.data <- aband %>% st_set_geometry(NULL)

#TAB 4 read in data
census <- st_read("2010_CensusData.shp")
#cen_data <- census %>% st_set_geometry(NULL)
cen_data <- census %>% dplyr::select("NAMELSAD", "SE_T003_00", "SE_T003_01", "SE_T003_02",
                                     "SE_T008_01", "SE_T008_02","SE_T008_03", "SE_T008_04",
                                     "SE_T008_05", "SE_T008_06","SE_T008_07", "SE_T008_08",
                                     "SE_T008_09", "SE_T008_10","SE_T008_11", "SE_T008_12",
                                     "SE_T054_01", "SE_T054_02","SE_T054_03", "SE_T054_04",
                                     "SE_T054_05", "SE_T054_06","SE_T054_07")
cen_data <- plyr::rename(cen_data, c("NAMELSAD" = "Census Tract", "SE_T003_00" = "Total Population" ,
                                       "SE_T003_01"="Male Pop" ,"SE_T003_02" = "Female Pop" ,
                                       "SE_T008_01" = "Under_5" ,"SE_T008_02" = "5_to_9",
                                       "SE_T008_03" = "10_to_14", "SE_T008_04" = "15_to_17",
                                     "SE_T008_05" = "18_to_24", "SE_T008_06" = "25_to_34",
                                     "SE_T008_07" = "35_to_44", "SE_T008_08" = "45_to_54",
                                     "SE_T008_09" = "55_to_64", "SE_T008_10" = "65_to_74",
                                     "SE_T008_11" = "75_to_84", "SE_T008_12" = "85_&_Older",
                                     "SE_T054_01" = "White" ,"SE_T054_02" = "African_American",
                                     "SE_T054_03" = "American_Indian", "SE_T054_04" = "Asian",
                                     "SE_T054_05" = "Pacific_Islander", "SE_T054_06" = "Other",
                                     "SE_T054_07" = "Two_or_More"))
cen_data <- cen_data[order(cen_data$`Census Tract`),]


#UI UI UI UI UI
# Define UI for application that draws a histogram
ui <- dashboardPage(skin = "green",
    dashboardHeader(title = "Group 2 West - South Bend City Dashboard"),
    dashboardSidebar(
        sidebarMenu(
        menuItem("Business Information", tabName = "busi", icon = icon("money")),
        menuItem("Parks and Public Facilities", tabName = "parks", icon = icon("leaf")),
        menuItem("Schools and Census Data", tabName = "schools", icon = icon("school"))
        #menuItem("Census Data", tabName = "demog", icon = icon("male") )
        )#End of sidebarMenu
    ),#end of dashboardSidebar
    #Body of DashBoard
    dashboardBody(
        tabItems(
            #TAB 1 TAB 1
            tabItem("busi",
                    fluidPage(
                             fluidRow(
                                 column(4,
                                        selectInput("dist",
                                                    "District:",
                                                    c("All",
                                                      unique(as.character(business2$District))))
                                 ),
                                 column(4,
                                        selectInput("class",
                                                    "Business Type:",
                                                    c("All",
                                                      unique(as.character(business2$Classification))))
                                 ),
                                 # Create a new row for the table.
                                 DT::dataTableOutput("table")
                                 ) ,
                             box(plotOutput("distbar"), width = 4),
                             box(plotOutput("exhist"), width = 4),
                             box(plotOutput("pieplot"), width = 4)
                        )
            ), # End of tab item "busi"
 
            #TAB 2 TAB 2
            tabItem("parks",
                    tabPanel(title = "Parks and Public Facilities",
                             leafletOutput(outputId = "mymap", height = 800),
                             absolutePanel(top = 80, left = 380,
                                           selectInput("facility_input",
                                                       "Looking for a :",
                                                       c("All",
                                                         unique(as.character(parks$Park_Type)),
                                                         unique(as.character(public$POPL_TYPE))))
                             )
                    )
            ),
            #TAB 3 TAB 3
            tabItem("schools",
                    fluidPage(
                        fluidRow(
                            column(4,
                                   selectInput("tract",
                                               "Tract :",
                                               c("All",
                                                 unique(as.character(cen_data$`Census Tract`))))
                            ),
                            # Create a new row for the table.
                            
                            #DT::dataTableOutput("table2"),
                            leafletOutput(outputId = "mymap2", height = 600),
                            fluidRow(DT::dataTableOutput("table2") )
                        ) ,
                        
                    )


            )
        )
 
        
        
    )
)#end of dashboardPage


# SERVER SERVER SERVER
# Define server logic required to draw a histogram
server <- function(input, output) {
    
    #TAB 1 TAB 1
    # Filter data based on selections
    output$table <- DT::renderDataTable(DT::datatable({
        bus_data <- business2
        if (input$dist != "All") {
            bus_data <- bus_data[bus_data$District == input$dist,]
            #recalc distbar
            output$distbar <- renderPlot({
             barplot(prop.table(table(bus_data$District)), xlab = "District", main = "Distribution by District")
            })
            #recalc hist exp dates
            output$exhist <- renderPlot({
                ggplot(bus_data, aes(x=License_Expiration, fill = District)) + geom_histogram(binwidth=90, colour="white") +
                    scale_x_date(breaks = seq(min(bus_data$License_Expiration)-5, max(bus_data$License_Expiration)+5, 365),
                                 limits = c(as.Date("2004-01-01"), as.Date("2020-01-01"))) +
                    ylab("Frequency") + xlab("Year") + ggtitle("License Expiration Dates by Quarter")+
                    theme_bw()+theme(axis.text.x = element_text(angle = 90))
            })
            
            #recalc pie graph
            output$pieplot <- renderPlot({
                pie(prop.table(table(bus_data$License_Status)) , main = "License Status")
            })
        }
        if (input$class != "All") {
            bus_data <- bus_data[bus_data$Classification == input$class,]
            #recalc distbar
            output$distbar <- renderPlot({
                barplot(prop.table(table(bus_data$District)), xlab = "District", main = "Distribution by District")
            })
            #recalc hist exp dates
            output$exhist <- renderPlot({
                ggplot(bus_data, aes(x=License_Expiration, fill = District)) + geom_histogram(binwidth=90, colour="white") +
                    scale_x_date(breaks = seq(min(bus_data$License_Expiration)-5, max(bus_data$License_Expiration)+5, 365),
                                 limits = c(as.Date("2004-01-01"), as.Date("2020-01-01"))) +
                    ylab("Frequency") + xlab("Year") + ggtitle("License Expiration Dates by Quarter")+
                    theme_bw()+theme(axis.text.x = element_text(angle = 90))
            })
            
            #recalc pie graph
            output$pieplot <- renderPlot({
                pie(prop.table(table(bus_data$License_Status)) , main = "License Status")
            })
        }
        
        bus_data
    }))
    
    #initial rendering of the bar chart 
    output$distbar <- renderPlot({
        bus_data <- business2
        barplot(prop.table(table(bus_data$District)), xlab = "District", main = "Distribution by District")
    })

    #initial rendering of date bar char color
    output$exhist <- renderPlot({
        bus_data <- business2
        ggplot(bus_data, aes(x=License_Expiration, fill = District)) + geom_histogram(binwidth=90, colour="white") +
            scale_x_date(breaks = seq(min(bus_data$License_Expiration)-5, max(bus_data$License_Expiration)+5, 365),
                         limits = c(as.Date("2004-01-01"), as.Date("2020-01-01"))) +
            ylab("Frequency") + xlab("Year") + ggtitle("License Expiration Dates by Quarter")+
            theme_bw()+theme(axis.text.x = element_text(angle = 90))
    })

    #initial rendering of pie chart
    output$pieplot <- renderPlot({
        bus_data <- business2
        pie(prop.table(table(bus_data$License_Status)) , main = "License Status")
    })


    #TAB @        
    #Tab 2 map of council districts and parks and public facilities
    park.spatial <- parks %>%
        st_as_sf(coords  = c("Lon","Lat"))%>%
        st_set_crs(value = 4326)
    park.spatial.set <- park.spatial
    park.spatial$popup <- paste("<b>",park.spatial$Park_Name,"</b><br>",
                                      "Type: ",park.spatial$Park_Type ,"<br>",
                                      "Address: ",park.spatial$Address,sep ="")
    park.pal <- colorFactor(palette = 'Set2', domain =park.spatial$Park_Type)
    public.spatial <- public %>%
        st_as_sf(coords  = c("Lon","Lat"))%>%
        st_set_crs(value = 4326)
    public.spatial$popup <- paste("<b>",public.spatial$POPL_NAME,"</b><br>",
                                "Type: ",public.spatial$POPL_TYPE ,"<br>",
                                "Address: ",public.spatial$POPL_ADDR1,sep ="")
    public.pal <- colorFactor(palette = 'Set1', domain =public.spatial$POPL_TYPE)

    output$mymap <- renderLeaflet({
        if(input$facility_input != "All") {
            public.spatial <- public.spatial[public.spatial$POPL_TYPE == input$facility_input,]
            park.spatial <- park.spatial[park.spatial$Park_Type == input$facility_input,]
        }
        labels <- sprintf(
            "<strong>District %s</strong><br/>%s",
            council$Num, council$Council_Me
            ) %>% lapply(htmltools::HTML)
        leaflet()%>%
            addTiles()%>%
            addPolygons(data = council, stroke = FALSE, smoothFactor = 0.2,
                    fillOpacity = 0.2,
                    color = ~factpal(council$Dist),
                    label = labels) %>%
            addLegend(pal = factpal, values = council$Dist, opacity = 0.7, title = "Districts",
                      position = "bottomright") %>%
            addCircleMarkers(data = park.spatial, popup = ~popup, group = "Parks", 
                             color = ~park.pal(Park_Type), stroke = 0, fillOpacity = 1, radius = 5) %>%
            addLegend(pal = park.pal, values = park.spatial$Park_Type, opacity = 0.7, title = "Park Type",
                      position = "topright") %>%
            addCircleMarkers(data = public.spatial, popup = ~popup, group = "Public", 
                             color = ~public.pal(POPL_TYPE), stroke = 0, fillOpacity = 1, radius = 5) %>%
            addLegend(pal = public.pal, values = public.spatial$POPL_TYPE, opacity = 0.7, title = "Public Facility Type",
                      position = "topright") %>%
            #addMarkers(data = public.spatial, icon = public.icons[public.spatial$POPL_TYPE],popup = ~popup,group = "Public") %>%
            addLayersControl(
                overlayGroups= c("Parks", "Public"),
                options = layersControlOptions(collapsed = FALSE))
    })
    
    
    
   
    
    
    
    
    #Tab3 

    school.pal <- colorFactor(palette = c("red", "blue"), domain =school$SchoolType)
    unique(aband$Outcome_St)
    aband.pal <- colorFactor(palette = "Set1", domain = aband$Outcome_St)
    cen.pal <- colorFactor(palette = "Set2", domain = cen_data$`Census Tract`)
    output$mymap2 <- renderLeaflet({
        cen_data2 <- cen_data
        if (input$tract != "All") {
            cen_data2 <- cen_data[cen_data$`Census Tract` == input$tract,]
            
        }
        tlabels <- sprintf(
            "<strong> %s",
            cen_data2$`Census Tract`
        ) %>% lapply(htmltools::HTML)
        schoollabels <- sprintf(
            "<strong> %s</strong><br/>%s",
            school$School, school$SchoolType
        ) %>% lapply(htmltools::HTML)
        leaflet()%>%
            addTiles()%>%
            addPolygons(data = cen_data2, stroke = FALSE, smoothFactor = 0.2,
                        fillOpacity = 0.4,
                        color = cen.pal(cen_data2$`Census Tract`),
                        label = tlabels, group = "Tracts") %>%
            addPolygons(data = school, stroke = FALSE, smoothFactor = 0.2,
                        color = school.pal(school$SchoolType),
                        fillOpacity = 1, label = schoollabels, group = "Schools") %>%
            addLegend(pal = school.pal, values = school$SchoolType, opacity = 0.7, title = "School Type",
                      position = "topright") %>%
            addPolygons(data = aband, stroke = FALSE, smoothFactor = 0.2,
                        color = aband.pal(aband$Outcome_St),
                        fillOpacity = 1, group = "Abandoned Properties") %>%
            addLegend(pal = aband.pal, values = aband$Outcome_St, opacity = 0.7, title = "Abandoned Properties",
                      position = "topright") %>%
            addLayersControl(
                overlayGroups= c("Schools", "Tracts","Abandoned Properties" ),
                options = layersControlOptions(collapsed = FALSE))
            
    })
    
    
    output$table2 <- DT::renderDataTable(DT::datatable({
        cen_data2 <- cen_data
        if (input$tract != "All") {
            cen_data2 <- cen_data[cen_data$`Census Tract` == input$tract,]
            
        }
        
        cen_data2
       
    }))
    

 
    
    #initial rendering of the bar chart 
    output$distbar <- renderPlot({
        bus_data <- business2
        barplot(prop.table(table(bus_data$District)), xlab = "District", main = "Distribution by District")
    })
    
    #initial rendering of date bar char color
    output$exhist <- renderPlot({
        bus_data <- business2
        ggplot(bus_data, aes(x=License_Expiration, fill = District)) + geom_histogram(binwidth=90, colour="white") +
            scale_x_date(breaks = seq(min(bus_data$License_Expiration)-5, max(bus_data$License_Expiration)+5, 365),
                         limits = c(as.Date("2004-01-01"), as.Date("2020-01-01"))) +
            ylab("Frequency") + xlab("Year") + ggtitle("License Expiration Dates by Quarter")+
            theme_bw()+theme(axis.text.x = element_text(angle = 90))
    })
    
    #initial rendering of pie chart
    output$pieplot <- renderPlot({
        bus_data <- business2
        pie(prop.table(table(bus_data$License_Status)) , main = "License Status")
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
