library(tidyr)
library(dplyr)
library(ggplot2)
library(readr)
library(gridExtra)

LPI <- read.csv("LPIdata_CC.csv")
LPI2 <- gather(LPI, "year", "abundance", 9:53)

LPI2$year <- parse_number(LPI2$year)
LPI2$abundance <- as.numeric(LPI2$abundance)

gannets = LPI2[LPI2$Common.Name == "Northern gannet",]
unique(gannets$Country.list);
gannet_scatter_facets <- ggplot(gannets, aes (x = year, y = abundance, colour = Country.list)) +
   geom_point(size = 2) +                                         
   geom_smooth(method = "lm", aes(fill = Country.list)) +           
   facet_wrap(~ Country.list, scales = "free") +  
   theme_bw() +
   ylab("Northern gannet abundance\n") +                             
   xlab("\nYear")  +
   theme(axis.text.x = element_text(size = 12, angle = 45, vjust = 1, hjust = 1),   
         axis.text.y = element_text(size = 12),
         axis.title = element_text(size = 14, face = "plain"),                        
         panel.grid = element_blank(),                                          
         plot.margin = unit(c(1,1,1,1), units = , "cm"),                
         legend.text = element_text(size = 12, face = "italic"),      
         legend.title = element_blank(),                              
         legend.position = "none")  

guillemots = LPI2[LPI2$Common.Name == "Black guillemot",]

guillemot_scatter_facets <- ggplot(guillemots, aes (x = year, y = abundance, colour = Country.list)) +
    geom_point(size = 2) +                                         
    geom_smooth(method = "lm", aes(fill = Country.list)) +           
    facet_wrap(~ Country.list, scales = "free") +  
    theme_bw() +
    ylab("Black guillemot abundance\n") +                             
    xlab("\nYear")  +
    theme(axis.text.x = element_text(size = 12, angle = 45, vjust = 1, hjust = 1),   
          axis.text.y = element_text(size = 12),
          axis.title = element_text(size = 14, face = "plain"),                        
          panel.grid = element_blank(),                                          
          plot.margin = unit(c(1,1,1,1), units = , "cm"),                
          legend.text = element_text(size = 12, face = "italic"),      
          legend.title = element_blank(),
          legend.position = "none")

(panel <- grid.arrange(
  
  gannet_scatter_facets + ggtitle("(c)") + ylab("Abundance of Gannets") + xlab("Year") +
    theme(plot.margin = unit(c(0.2, 0.2, 0.2, 0.2), units = , "cm")) +
    theme(legend.position = "none"), # changing the legend position so that it fits within the panel
  
  guillemot_scatter_facets + ggtitle("(c)") + ylab("Abundance of Guillemots") + xlab("Year") +
    theme(plot.margin = unit(c(0.2, 0.2, 0.2, 0.2), units = , "cm")) +
    theme(legend.position = "none"), # changing the legend position so that it fits within the panel
  
  ncol = 1)) # ncol determines how many columns you have

