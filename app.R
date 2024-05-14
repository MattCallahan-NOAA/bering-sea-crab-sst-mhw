library(shiny)
library(tidyverse)
library(shinycssloaders)
library(lubridate)
library(cowplot)
library(httr)
library(heatwaveR)
library(gridExtra)
library(scales)

areas<-c("NBS", "StMattBKC", "PribRKC", "BBRKC", "TannerW", "TannerE")

#Load Base data
data<-readRDS("data/crabsst.RDS")

ui <- fluidPage(
    
    
    #define sidebar layout
    sidebarLayout(
      #Define sidebar panel
      sidebarPanel(
        #region input
        selectInput(inputId = "crab_area",
                    label = "Select crab area",
                    choices = areas,
                    selected = "NBS"
        ),
        tags$blockquote("We present daily sea surface temperatures and marine heatwave status for  by the Alaska Fisheries Science Center. Temperatures are updated automatically
 Bering Sea crab management areas using satellite data curated by NOAA's Coral Reef Watch Program (https://coralreefwatch.noaa.gov/)."),
 tags$blockquote("Marine heatwave calculations are performed on the daily SST data using the heatwaveR package (https://robwschlegel.github.io/heatwaveR/)."),
 tags$blockquote("More information can be found here (https://www.fisheries.noaa.gov/feature-story/current-sea-surface-temperatures-eastern-bering-sea),
 on github (https://github.com/MattCallahan-NOAA/SST-shiny/), or by contacting emily.lemagie@noaa.gov or matt.callahan@noaa.gov."),
        #map of esr regions
        img(src='crab_map.png', width="100%", height="100%")
      ),
 mainPanel(plotOutput(outputId ="sstplot", height = "1000", width="100%")%>%
             withSpinner(),
           tags$blockquote(paste0("The current year's daily temperatures (black lines; updated ",max(data$date),") are compared to the previous year (blue line),
  the daily average (1985-2014), and each of the individual years since 1985 (grey lines).
  Heatwave intensity increases\n(successive dotted lines)\nas waters warm.
           Heatwaves occur when daily\nSST exceeds the 90th\npercentile of normal\n(lowest dotted line) for\n5 consecutive days."))
 )

 

))

server <- function(input, output) {

  
  ####-----------------------------------------------------------------####
  #code relevent to all upper panels in subsequent plots
  
  #  Load 508 compliant NOAA colors
  OceansBlue1='#0093D0'
  OceansBlue2='#0055A4' # rebecca dark blue
  Crustacean1='#FF8300'
  UrchinPurple1='#7F7FFF'
  SeagrassGreen4='#D0D0D0' # This is just grey
  #  Assign colors to different time series.
  current.year.color <- "black"#CoralRed1 #OceansBlue1
  last.year.color <- OceansBlue1#WavesTeal1
  mean.color <- UrchinPurple1
  #  Set default plot theme
  theme_set(theme_cowplot())
  
  #  Specify legend position coordinates (top panel)
  mylegx <- 0.2
  mylegy <- 0.7
  
  ####-------------------------------------------------------------####

  
 
  #  Set year criteria to automatically identify the current and previous years
  current.year <- max(data$year2)
  last.year <- current.year-1
  mean.years <- 1985:2015 # We use the oldest 30-year time series as our climatological baseline.
  mean.lab <- "Mean 1985-2015"
  
  ####---------------------------------------------------####
  
  #filter data
  data_1<-reactive(data%>%filter(crabarea==input$crab_area))
  #data_1<-  data%>%filter(crabarea=="NBS")
  
  #Plots for P1
  #  Create plotting function that will allow selection of 2 ESR regions
p1 <-reactive(
  ggplot() +
      geom_line(data=data_1() %>% filter(year2<last.year ), # Older years are grey lines.
                aes(newdate,meansst,group=factor(year2),col='mygrey'),size=0.3) +
      geom_line(data=data_1() %>% filter(year2==last.year ), # The previous year
                aes(newdate,meansst,color='last.year.color'),size=0.75) +
      geom_line(data=data_1() %>% 
                  filter(year%in%mean.years ) %>% # The mean from 1986-2015
                  group_by(newdate) %>% 
                  summarise(meantemp=mean(meansst,na.rm=TRUE)),
                aes(newdate,meantemp,col='mean.color'),size=0.65,linetype="solid") +
      geom_line(data=data_1() %>% filter(year2==current.year ), # This year
                aes(newdate,meansst,color='current.year.color'),size=0.75) +
      scale_color_manual(name="",
                         breaks=c('current.year.color','last.year.color','mygrey','mean.color'),
                         values=c('current.year.color'=current.year.color,'last.year.color'=last.year.color,'mygrey'=SeagrassGreen4,'mean.color'=mean.color),
                         labels=c(current.year,last.year,paste0('1985-',last.year-1),mean.lab)) +
      scale_linetype_manual(values=c("solid","solid","solid","dashed")) +
      ylab("Sea Surface Temperature (°C)") + 
      xlab("") +
      scale_x_date(date_breaks="1 month",
                   date_labels = "%b",
                   expand = c(0.025,0.025)) + 
      theme(legend.position=c(mylegx,mylegy),
            legend.text = element_text(size=20,family="sans"),
            legend.background = element_blank(),
            legend.title = element_blank(),
            strip.text = element_text(size=24,color="white",family="sans",face="bold"),
            strip.background = element_rect(fill=OceansBlue2),
            axis.title.y = element_text(size=20,family="sans"),
            axis.text.y = element_text(size=16,family="sans"),
            panel.border=element_rect(colour="black",size=0.75),
            axis.text.x=element_blank(),
            legend.key.size = unit(0.35,"cm"),
            plot.margin=unit(c(-0.1,0.05,0,0),"cm")) 
    
)

  

  
  ####-------------------------------------------------####
  #Code used for all lower panels

  #  Create custom categories for lines
  lineColCat <- c(
    "Temperature" = "black",
    "Baseline" = mean.color,
    "Moderate (1x Threshold)" = "gray60",
    "Strong (2x Threshold)" = "gray60",
    "Severe (3x Threshold)" = "gray60",
    "Extreme (4x Threshold)" = "gray60"
  )

  #  Create flame fill parameters
  fillColCat <- c(
    "Moderate" = "#ffc866",
    "Strong" = "#ff6900",
    "Severe" = "#9e0000",
    "Extreme" = "#2d0000"
  )

  #  Modified flame fill parameters
  Moderate = "#ffc866"
  Strong = "#ff6900"
  Severe = "#9e0000"
  Extreme = "#2d0000"


  ####------------------------------------------------####
  #Bering Sea lower panel code

  # Use heatwaveR package to detect marine heatwaves.
  mhw <- reactive(
    (detect_event(ts2clm(data_1() %>%
                                  rename(t=date,temp=meansst) %>%
                                  arrange(t), climatologyPeriod = c("1985-12-01", "2015-11-30"))))$clim
  )


  #  Create a vector of the days remaining in the year without data.
  yearvec <- reactive(seq.Date(max(mhw()$t)+1,as_date(paste0(current.year,"-11-30")),"day"))
  #create year length vector
  current.year.length<-ifelse(leap_year(current.year)==FALSE, 365,366)

  #  Replace the current year with the previous year for our remaining days vector.
  dummydat <- reactive(
    data.frame(t=yearvec()-current.year.length,newt=yearvec()) %>%
    inner_join(mhw() %>% dplyr::select(thresh,seas,t)) %>%
    dplyr::select(t=newt,thresh,seas) %>%
    mutate(temp=NA)
    )
  # Calculate threshold values for heatwave categories. This code directly from Schegel & Smit
  clim_cat <- reactive(
    mhw() %>%
    bind_rows(dummydat()) %>%
    dplyr::mutate(diff = thresh - seas,
                  thresh_2x = thresh + diff,
                  thresh_3x = thresh_2x + diff,
                  thresh_4x = thresh_3x + diff,
                  year=year(t)) %>%
    arrange(t))

    #  Plotting code only slightly modified from heatwaveR vignette
  p2 <- reactive(
    ggplot(data = clim_cat() %>% filter(t>=as.Date(paste0(last.year,"-12-01"))), aes(x = t, y = temp)) +
    geom_line(aes(y = temp, col = "Temperature"), size = 0.85) +
    geom_flame(aes(y2 = thresh, fill = Moderate)) +
    geom_flame(aes(y2 = thresh_2x, fill = Strong)) +
    geom_flame(aes(y2 = thresh_3x, fill = Severe)) +
    geom_flame(aes(y2 = thresh_4x, fill = Extreme)) +
    geom_line(aes(y = thresh_2x, col = "Strong (2x Threshold)"), size = 0.5, linetype = "dotted") +
    geom_line(aes(y = thresh_3x, col = "Severe (3x Threshold)"), size = 0.5, linetype = "dotted") +
    geom_line(aes(y = thresh_4x, col = "Extreme (4x Threshold)"), size = 0.5, linetype = "dotted") +
    geom_line(aes(y = seas, col = "Baseline"), size = 0.65,linetype="solid") +
    geom_line(aes(y = thresh, col = "Moderate (1x Threshold)"), size = 0.5,linetype= "dotted") +
    scale_colour_manual(name = NULL, values = lineColCat,
                        breaks = c("Temperature", "Baseline", "Moderate (1x Threshold)"),guide="none") +
    scale_fill_manual(name = "Heatwave\nIntensity", values = c(Extreme,Severe,Strong,Moderate),labels=c("Extreme","Severe","Strong","Moderate")#, guide = FALSE
    ) +
    scale_x_date(limits=c(as_date(paste0(last.year,"-12-01")),as_date(paste0(current.year,"-11-30"))),date_breaks="1 month",date_labels = "%b",expand=c(0.01,0)) +
    scale_y_continuous(labels = scales::number_format(accuracy = 1)) +
    labs(y = "Sea Surface Temperature (°C)", x = NULL) +
    theme(strip.text=element_text(size=24),
          legend.position=c(mylegx, mylegy),
          legend.title = element_text(size=20),
          legend.key.size = unit(0.75,"line"),
          legend.text = element_text(size=16),
          axis.title.x=element_blank(),
          axis.text.x=element_text(size=16, color=c("black",NA,NA,"black",NA,NA,"black",NA,NA,"black",NA,NA,NA)),
          plot.margin=unit(c(-0.7,0.05,3,0),"cm"),
          #strip.text = element_text(size=24,color="white",family="sans",face="bold"),
          #strip.background = element_rect(fill=OceansBlue2),
          axis.title.y = element_text(size=20,family="sans",color="black"),
          axis.text.y = element_text(size=16,family="sans",color="black"),
          panel.border=element_rect(colour="black",fill=NA,size=0.5),
          panel.background = element_blank(),
          legend.background = element_blank()))
     
  
  # p2<-reactive(ggdraw(p2()) +
  #                annotate("text",x=0.5 ,y=0.065,label=paste0("NOAA Coral Reef Watch data, courtesy National Environmental Satellite, Data, and Information Service (Updated: ",
  #                                                            format(max(data$date),"%m-%d-%Y"),
  #                                                            ")\n Data are modeled satellite products and periodic discrepancies or gaps may exist across sensors and products.\n                                    Contact: matt.callahan@noaa.gov "),
  #                         hjust=0.5, size=7,family="sans",fontface=1,color=OceansBlue2,lineheight=0.85) )

  ####-----------------------------------------------####
  #combine plots
  p3<-reactive(plot_grid(p1(),p2(),ncol=1))
  
  ####--------------------------------------------------------####
  #print to shiny
  #BS render plot
  output$sstplot<- renderPlot({
    p3()
  })
  

}

shinyApp(ui = ui, server = server)
