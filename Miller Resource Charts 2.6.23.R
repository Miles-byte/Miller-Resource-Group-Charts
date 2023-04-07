#Loading lots of default packages
pacman::p_load(transformr,artyfarty,quantmod,tidyquant,datetime,yearmon,lubridate,readr,stringi,jsonlite,cli,remotes,magick,cowplot,knitr,ghostscript,png,httr,grid,usethis,pacman,rio,ggplot2,ggthemes,quantmod,dplyr,data.table,lubridate,forecast,gifski,av,tidyr,gganimate,zoo,RCurl,Cairo,datetime,stringr,pollster,tidyquant,hrbrthemes,plotly,fredr)
#Loading "artyfarty" which allows for some nicer plot themes
install_github('datarootsio/artyfarty')
library("artyfarty")

#Downloading Blue and White Logos
miller_blue_logo <- image_read("https://github.com/Miles-byte/Miller-Resource-Group-Charts/blob/9434a49b489ad26b2542ed6409107cd1a419021d/miller_blue.png?raw=true")
miller_blue_logo_rast <- rasterGrob(miller_blue_logo, interpolate=TRUE)

miller_white_logo <- image_read("https://github.com/Miles-byte/Miller-Resource-Group-Charts/blob/9434a49b489ad26b2542ed6409107cd1a419021d/miller_white.png?raw=true")
miller_white_logo_rast <- rasterGrob(miller_white_logo, interpolate=TRUE)

#Downloading industrial production numbers and taking a 3/12MMA and annual growth rates in 3/12MMA
fredr_set_key("1a7706ab5c5b5d56ca965973990adbcf")
INDPRO <- fredr(series_id = "INDPRO", observation_start = as.Date("1999-02-01"))%>%
  mutate(value12MMA = c(NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,rollmean(value, 12))) %>%
  mutate(value3MMA = c(NA,NA,rollmean(value, 3))) %>%
  mutate(Growth12MMA = value12MMA/lag(value12MMA,12)-1) %>%
  mutate(Growth3MMA = value3MMA/lag(value3MMA,12)-1) %>%
  drop_na()

INDPRO_INDEX_Graph <- ggplot() + #plotting Industrial Production 12MMT Index
  #annotate("hline", y = 0, yintercept = 0, color = "gray", size = .5) +
  geom_bar(data=INDPRO, aes(x=date,y= value12MMA*12-900,fill= "Industrial Production, 12M Moving Total"), stat = "identity", color = NA, width = 31)+ 
  xlab("Date") +
  ylab("Index, 2017 = 1200") +
  scale_y_continuous(labels = c(900,1000,1100,1200,1300), expand = c(0,0), limits = c(0,400), breaks = c(0,100,200,300,400)) +
  ggtitle("Industrial Production") +
  theme_scientific() +
  scale_fill_manual(name= NULL,values = c("#08323f","#EF6E24","#068D9D","#6D9DC5")) +
  theme(legend.text = element_text(size = 15), plot.title = element_text(size = 30), legend.position = "top")+
  annotation_custom(miller_white_logo_rast, xmin = as.Date("2002-01-01")-(.1861*(today()-as.Date("2002-01-01"))), xmax = as.Date("2002-01-01")-(0.049*(today()-as.Date("2002-01-01"))), ymin = 500-(.3*200), ymax = 500) +
  coord_cartesian(clip = "off")


INDPRO_GROWTH_Graph <- ggplot() + #plotting Industrial Production Growth
  annotate("hline", y = 0, yintercept = 0, color = "gray", size = .5) +
  geom_line(data=INDPRO, aes(x=date,y= Growth12MMA,color= "Annual Change in 12M Moving Total"), size = 1.25)+ 
  geom_line(data=INDPRO, aes(x=date,y= Growth3MMA,color= "Annual Change in 3M Moving Total"), size = 1.25)+ 
  xlab("Date") +
  ylab("Percent Growth") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), expand = c(0,0)) +
  ggtitle("Industrial Production") +
  theme_scientific() +
  scale_color_manual(name= NULL,values = c("#08323f","#EF6E24","#068D9D","#6D9DC5")) +
  theme(legend.text = element_text(size = 15), plot.title = element_text(size = 34), legend.position = "top") +
  annotation_custom(miller_white_logo_rast, xmin = as.Date("2002-01-01")-(.1861*(today()-as.Date("2002-01-01"))), xmax = as.Date("2002-01-01")-(0.049*(today()-as.Date("2002-01-01"))), ymin = .22-(.3*0.15), ymax = .22) +
  coord_cartesian(clip = "off")

#Saving Industrial Production Charts
ggsave(dpi = "retina",plot = INDPRO_INDEX_Graph, "INDPRO Index.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")
ggsave(dpi = "retina",plot = INDPRO_GROWTH_Graph, "INDPRO Growth.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")




#Downloading GDP numbers and taking a annual growth rate
GDP <- fredr(series_id = "GDPC1", observation_start = as.Date("2000-01-01"))%>%
  mutate(growth = value/lag(value,4)-1) %>%
  drop_na()

GDP_INDEX_Graph <- ggplot() + #plotting GDP index
  #annotate("hline", y = 0, yintercept = 0, color = "gray", size = .5) +
  geom_bar(data=GDP, aes(x=date,y= value/1000-12,fill= "US Real GDP, Trillions of 2012 Dollars"), stat = "identity", color = NA, width = 95)+ 
  xlab("Date") +
  ylab("Trillions of 2012 Dollars") +
  scale_y_continuous(labels = c("$12T","$14T","$16T","$18T","$20T","$22T","$24T","$26T"), breaks = c(0,2,4,6,8,10,12,14), expand = c(0,0), limits = c(0, max(GDP$value)/1000-11)) + 
  ggtitle("Real GDP") +
  theme_scientific() +
  scale_fill_manual(name= NULL,values = c("#08323f","#EF6E24","#068D9D","#6D9DC5")) +
  theme(legend.text = element_text(size = 14.5), plot.title = element_text(size = 34), legend.position = "top") +
  annotation_custom(miller_white_logo_rast, xmin = as.Date("2002-01-01")-(.1861*(today()-as.Date("2002-01-01"))), xmax = as.Date("2002-01-01")-(0.049*(today()-as.Date("2002-01-01"))), ymin = 12-(.3*6), ymax = 12) +
  coord_cartesian(clip = "off")

GDP_GROWTH_Graph <- ggplot() + #plotting GDP growth
  annotate("hline", y = 0, yintercept = 0, color = "gray", size = .5) +
  geom_line(data=GDP, aes(x=date,y= growth,color= "Annual Change in Real GDP"), size = 1.25)+ 
  xlab("Date") +
  ylab("Percent Growth") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), expand = c(0,0)) + 
  ggtitle("Real GDP Growth") +
  theme_scientific() +
  scale_color_manual(name= NULL,values = c("#08323f","#EF6E24","#068D9D","#6D9DC5")) +
  theme(legend.text = element_text(size = 14.5), plot.title = element_text(size = 34), legend.position = "top") +
  annotation_custom(miller_white_logo_rast, xmin = as.Date("2002-01-01")-(.1861*(today()-as.Date("2002-01-01"))), xmax = as.Date("2002-01-01")-(0.049*(today()-as.Date("2002-01-01"))), ymin = .185-(.3*0.15), ymax = .185) +
  coord_cartesian(clip = "off")

#Saving GDP charts
ggsave(dpi = "retina",plot = GDP_INDEX_Graph, "GDP Index.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")
ggsave(dpi = "retina",plot = GDP_GROWTH_Graph, "GDP Growth.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")




#Downloading Single-Family starts numbers, taking a 12MMA, and taking a annual growth rate of the 12MMA
SFSTARTS <- fredr(series_id = "HOUST1F", observation_start = as.Date("1998-02-01"))%>%
  mutate(value = c(NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,rollmean(value, 12))) %>%
  mutate(growth = (value-lag(value,12))/lag(value,12)) %>%
  drop_na()

STARTS_INDPRO_Graph <- ggplot() + #plotting Single-Family starts against Industrial Production
  annotate("hline", y = 0, yintercept = 0, color = "gray", size = .5) +
  geom_line(data=INDPRO, aes(x=date,y= Growth12MMA,color= "Industrial Production, 12MMT"), size = 1.25)+ 
  geom_line(data=SFSTARTS, aes(x=date + 365,y= growth,color= "Single-Family Housing Starts, 12MMT, Offset 1yr"), size = 1.25)+ 
  xlab("Date") +
  ylab("Percent Growth") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), expand = c(0,0)) + 
  ggtitle("Housing Starts & Production") +
  theme_scientific() +
  scale_color_manual(name= NULL,values = c("#08323f","#EF6E24","#068D9D","#6D9DC5")) +
  theme(legend.text = element_text(size = 15), plot.title = element_text(size = 34), legend.position = "top") +
  annotation_custom(miller_white_logo_rast, xmin = as.Date("2002-01-01")-(.1861*(today()-as.Date("2002-01-01"))), xmax = as.Date("2002-01-01")-(0.049*(today()-as.Date("2002-01-01"))), ymin = .475-(.3*0.35), ymax = .475) +
  coord_cartesian(clip = "off")

#Saving Single-Family Starts Charts
ggsave(dpi = "retina",plot = STARTS_INDPRO_Graph, "Starts Indpro.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")



#Downloading Retail Sales numbers, taking a 12MMA and 3MMA, and taking a annual growth rate of the 12MMA & 3MMA
RETAIL <- fredr(series_id = "RSAFS", observation_start = as.Date("1999-02-01"))%>%
  mutate(value12MMA = c(NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,rollmean(value, 12))) %>%
  mutate(value3MMA = c(NA,NA,rollmean(value, 3))) %>%
  mutate(Growth12MMA = value12MMA/lag(value12MMA,12)-1) %>%
  mutate(Growth3MMA = value3MMA/lag(value3MMA,12)-1) %>%
  drop_na()

RETAIL_INDEX_Graph <- ggplot() + #plotting retail sales index
  geom_bar(data=RETAIL, aes(x=date,y= value12MMA/1000000*12-3,fill= "Retail Sales: Retail Trade and Food Services, 12 Month Moving Total"), stat = "identity", color = NA, width = 32)+ 
  xlab("Date") +
  ylab("Trillions of Dollars") +
  scale_y_continuous(labels = c("$3T","$4T","$5T","$6T","$7T","$8T","9T","10T","11T","12T"), breaks = c(0,1,2,3,4,5,6,7,8,9), expand = c(0,0), limits = c(0, max(RETAIL$value12MMA)/1000000*12-2.5)) + 
  ggtitle("Total Retail Sales") +
  theme_monokai() + theme(plot.background = element_rect(fill = "#08323f"), panel.border = element_blank(), axis.line = element_line(color = "#08323f"), panel.background = element_rect(fill = "#08323f", color = NA),legend.position = "top", legend.key = element_rect(fill = "#08323f"),legend.background = element_rect(fill = "#08323f")) +
  theme(text = element_text(color = "white"), axis.title.x = element_text(color = "white"), axis.title.y = element_text(color = "white"), legend.text = element_text(color = "white", size = 15), axis.text.x = element_text(color = "white"), axis.text.y = element_text(color = "white"), plot.title = element_text(color = "white", size = 30)) +
  scale_fill_manual(name= NULL,values = c("#EF6E24","#ffffff","#068D9D","#6D9DC5")) +
  annotation_custom(miller_blue_logo_rast, xmin = as.Date("2002-01-01")-(.1861*(today()-as.Date("2002-01-01"))), xmax = as.Date("2002-01-01")-(0.049*(today()-as.Date("2002-01-01"))), ymin = 7.25-(.3*2.5), ymax = 7.25) +
  coord_cartesian(clip = "off")

RETAIL_GROWTH_Graph <- ggplot() + #plotting retail sales growth
  annotate("hline", y = 0, yintercept = 0, color = "white", size = .5) +
  geom_line(data=RETAIL, aes(x=date,y= Growth12MMA,color= "Annual Change in 12M Moving Total"), size = 1.25)+ 
  geom_line(data=RETAIL, aes(x=date,y= Growth3MMA,color= "Annual Change in 3M Moving Total"), size = 1.25)+ 
  xlab("Date") +
  ylab("Percent Change") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), expand = c(0,0)) +
  ggtitle("Total Retail Sales") +
  theme_monokai() + theme(plot.background = element_rect(fill = "#08323f"), panel.border = element_blank(), axis.line = element_line(color = "#08323f"), panel.background = element_rect(fill = "#08323f", color = NA),legend.position = "top", legend.key = element_rect(fill = "#08323f"),legend.background = element_rect(fill = "#08323f")) +
  theme(text = element_text(color = "white"), axis.title.x = element_text(color = "white"), axis.title.y = element_text(color = "white"), legend.text = element_text(color = "white", size = 15), axis.text.x = element_text(color = "white"), axis.text.y = element_text(color = "white"), plot.title = element_text(color = "white", size = 30)) +
  scale_color_manual(name= NULL,values = c("#EF6E24","#ffffff","#068D9D","#6D9DC5")) +
  annotation_custom(miller_blue_logo_rast, xmin = as.Date("2002-01-01")-(.1861*(today()-as.Date("2002-01-01"))), xmax = as.Date("2002-01-01")-(0.049*(today()-as.Date("2002-01-01"))), ymin = .525-(.3*0.35), ymax = .525) +
  coord_cartesian(clip = "off")


#Saving Retail Sales Charts
ggsave(dpi = "retina",plot = RETAIL_INDEX_Graph, "Retail Index.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")
ggsave(dpi = "retina",plot = RETAIL_GROWTH_Graph, "Retail Growth.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")



#Downloading nondefense capital orders numbers, taking a 12MMA and 3MMA, and taking a annual growth rate of the 12MMA & 3MMA
NEWORDER <- fredr(series_id = "NEWORDER", observation_start = as.Date("1999-02-01"))%>%
  mutate(value12MMA = c(NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,rollmean(value, 12))) %>%
  mutate(value3MMA = c(NA,NA,rollmean(value, 3))) %>%
  mutate(Growth12MMA = value12MMA/lag(value12MMA,12)-1) %>%
  mutate(Growth3MMA = value3MMA/lag(value3MMA,12)-1) %>%
  drop_na()

NEWORDER_INDEX_Graph <- ggplot() + #plotting nondefense capital orders index
  geom_bar(data=NEWORDER, aes(x=date,y= value12MMA*12/1000-550,fill= "New Orders: Nondefense Capital Goods Ex Aircraft, 12MMT"), stat = "identity", color = NA, width = 32)+ 
  xlab("Date") +
  ylab("Billions of Dollars") +
  scale_y_continuous(labels = c("$600B","$700B","$800B","$900B","$900B"), breaks = c(50,150,250,350,450), expand = c(0,0), limits = c(0, max(NEWORDER$value)*12/1000-500)) + 
  ggtitle("CapEx Spending") +
  theme_monokai() + theme(plot.background = element_rect(fill = "#08323f"), panel.border = element_blank(), axis.line = element_line(color = "#08323f"), panel.background = element_rect(fill = "#08323f", color = NA),legend.position = "top", legend.key = element_rect(fill = "#08323f"),legend.background = element_rect(fill = "#08323f")) +
  theme(text = element_text(color = "white"), axis.title.x = element_text(color = "white"), axis.title.y = element_text(color = "white"), legend.text = element_text(color = "white", size = 15), axis.text.x = element_text(color = "white"), axis.text.y = element_text(color = "white"), plot.title = element_text(color = "white", size = 30)) +
  scale_fill_manual(name= NULL,values = c("#EF6E24","#ffffff","#068D9D","#6D9DC5")) +
  annotation_custom(miller_blue_logo_rast, xmin = as.Date("2002-01-01")-(.1861*(today()-as.Date("2002-01-01"))), xmax = as.Date("2002-01-01")-(0.049*(today()-as.Date("2002-01-01"))), ymin = 520-(.3*200), ymax = 520) +
  coord_cartesian(clip = "off")

NEWORDER_GROWTH_Graph <- ggplot() + #plotting nondefense capital orders index
  annotate("hline", y = 0, yintercept = 0, color = "white", size = .5) +
  geom_line(data=NEWORDER, aes(x=date,y= Growth12MMA,color= "Annual Change in 12M Moving Total"), size = 1.25)+ 
  geom_line(data=NEWORDER, aes(x=date,y= Growth3MMA,color= "Annual Change in 3M Moving Total"), size = 1.25)+ 
  xlab("Date") +
  ylab("Percent Change") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), expand = c(0,0)) +
  ggtitle("CapEx Spending") +
  theme_monokai() + theme(plot.background = element_rect(fill = "#08323f"), panel.border = element_blank(), axis.line = element_line(color = "#08323f"), panel.background = element_rect(fill = "#08323f", color = NA),legend.position = "top", legend.key = element_rect(fill = "#08323f"),legend.background = element_rect(fill = "#08323f")) +
  theme(text = element_text(color = "white"), axis.title.x = element_text(color = "white"), axis.title.y = element_text(color = "white"), legend.text = element_text(color = "white", size = 15), axis.text.x = element_text(color = "white"), axis.text.y = element_text(color = "white"), plot.title = element_text(color = "white", size = 30)) +
  scale_color_manual(name= NULL,values = c("#EF6E24","#ffffff","#068D9D","#6D9DC5")) +
  annotation_custom(miller_blue_logo_rast, xmin = as.Date("2002-01-01")-(.1861*(today()-as.Date("2002-01-01"))), xmax = as.Date("2002-01-01")-(0.049*(today()-as.Date("2002-01-01"))), ymin = .34-(.3*0.35), ymax = .34) +
  coord_cartesian(clip = "off")


#Saving Retail Sales Charts
ggsave(dpi = "retina",plot = NEWORDER_INDEX_Graph, "Neworder Index.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")
ggsave(dpi = "retina",plot = NEWORDER_GROWTH_Graph, "Neworder Growth.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")


#Downloading PMI Numbers—NOTE: MUST BE MANUALLY UPDATED IN THE CSV FILE WITH EACH MONTH'S NEW DATA
#Make sure to keep dates in the YYYY-MM-DD format in the CSV if using Excel to Edit 
PMI <- read.csv("https://raw.githubusercontent.com/Miles-byte/Miller-Resource-Group-Charts/main/PMI.csv") %>%
  mutate(Date = as.Date(Date))

PMI_Graph <- ggplot() + #plotting PMI and INDPRO data
  annotate("hline", y = 0, yintercept = 0, color = "gray", size = .5) +
  geom_line(data=subset(INDPRO, date >= as.Date("2004-12-01")), aes(x=date,y= Growth12MMA,color= "Industrial Production: Annual Change in 12MMT"), size = 1.25)+ 
  geom_line(data=PMI, aes(x=Date+365,y= PMI/100-.50,color= "ISM Manufacturing PMI, Offset 1yr"), size = 1.25)+ 
  xlab("Date") +
  ylab("Percent Growth") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), expand = c(0,0), sec.axis = sec_axis(~.*100+(.50*100), name="PMI, >50 = Expansion", labels = scales::number_format())) + 
  ggtitle("Production & PMI") +
  theme_scientific() +
  scale_color_manual(name= NULL,values = c("#08323f","#EF6E24","#068D9D","#6D9DC5")) +
  theme(legend.text = element_text(size = 14.5), plot.title = element_text(size = 34), legend.position = "top") +
  annotation_custom(miller_white_logo_rast, xmin = as.Date("2006-01-01")-(.1861*(today()-as.Date("2006-01-01"))), xmax = as.Date("2006-01-01")-(0.049*(today()-as.Date("2006-01-01"))), ymin = .24-(.3*0.15), ymax = .24) +
  coord_cartesian(clip = "off")

#Saving PMI Charts
ggsave(dpi = "retina",plot = PMI_Graph, "PMI Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")


cat("\014")  # ctrl+L

rm(list = ls())

dev.off()
