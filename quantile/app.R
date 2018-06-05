library("waterData")
library("lubridate")
library("ggplot2")
library("tidyverse")
library("scales")
library("dplyr")
library("shiny")
library("shinythemes")
library("reshape")


station = '02323500'   

#get site name to use in plot titles and such
stinfo  = siteInfo(station)

#read entire time series
dis   = importDVs(staid=station,code='00060',stat='00003', sdate= "1950-01-01") 

dis$year    = as.numeric(strftime(dis$dates,format="%Y"))
#dis$month   = as.numeric(strftime(dis$dates,format="%m")) 

#Naming columns, using the Diver sensors, collects date, pressure, temp, conductivity
colnames(dis) <- c("StaID", "Discharge", "dates", "QualCode", "Year")

dis$Date <- as.Date(dis$dates,origin= "1899-12-30")



# Define UI for application that draws a histogram
ui <- fluidPage(theme = shinytheme("yeti"),
                
                
                # Application title
                
                h3("River Discharge Comparison"),
                
                sidebarLayout(
                  sidebarPanel(
                    selectInput("year1", label= h4("Year"), 
                                choices=unique(dis$Year)),
                    
                    selectInput("year2", label= h4("Comparison Year"), 
                                choices=unique(dis$Year)),
                    
                    
                    radioButtons("type", "Plot type", choices =c ("Tile"= "tile",
                                                                  "Boxplot" = "box"), 
                                 selected = "line"),
                    
                  ),
                  
                  
                  # Show a plot of the generated distribution
                  mainPanel(
                    plotOutput("quant")
                    
                  )
                )
)



# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  rd <- reactive({
    
    dis <- dis %>% 
      filter(Year == input$year1|Year == input$year2)
    
    
    if(input$type == "line") { 
      ggplot(data=dis, aes(x= Date, y=Discharge/1000)) + 
        geom_line() + 
        scale_x_date(
          breaks = date_breaks("1 month") ,
          labels = date_format("%b")) +
        theme(panel.border = element_rect(color = "black", size = 0.5, fill = NA, linetype="solid"),axis.text.x = element_text(angle = 90, hjust = 1, size=12)) +
        labs(x="Month", y="River Discharge (1,000 cfs)") +
        facet_wrap(~Year, ncol=1, scale = "free_x")
      
    }    else {
      ggplot(data=dis, aes(x= Month, y=Discharge/1000)) + 
        geom_boxplot(position = 'identity') +
        scale_x_discrete(
          limits = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")) +
        theme(panel.border = element_rect(color = "black", size = 0.5, fill = NA, linetype="solid"),axis.text.x = element_text(angle = 90, hjust = 1, size=12)) +
        labs(x="Month", y="River Discharge (1,000 cfs)") +
        facet_wrap(~Year, ncol=1) 
    }
    
  })
  
  quant <- reactive({
    
    if(input$type1 == "quant"){ 
      
      ggplot(dis4) + 
        geom_area(aes_string(x= "id", y= "value", fill= "variable")) + 
        geom_line(aes_string(x= "id", y= input$year), size=1.3)+                      
        ylab(expression(paste('Mean Daily Discharge','  ',(ft^3 / sec)))) + 
        xlab('Day of the Year') + 
        scale_fill_discrete(guide = guide_legend(title = "Quantiles", reverse=TRUE),
                            labels=c('0%','25%','50%','75%','100%')) +
        theme(panel.border = element_rect(color = "black", size = 0.5, fill = NA, linetype="solid"),axis.text.x = element_text(angle = 90, hjust = 1, size=12), text = element_text(size=12)) 
      
    } else { }
    
  })
  
  output$rd <-renderPlot({rd()})
  
  
  output$quant <-renderPlot({quant()})
  
}


# Run the application 
shinyApp(ui = ui, server = server)