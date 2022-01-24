library(tidyverse)
library(ggHoriPlot) 
library(ggthemes)

library(glue)
library(ggtext)

library(sp)
library(cartography)


#### set working directory 

setwd('Users/Mohamed/Documents/ESPON-DATA/03_Figures/02_Report Plots')

### read the data 

ds = read_csv('Input/d_figure_1.csv')


### data reprocessing 


cutpoints <- ds  %>% 
  mutate(
    outlier = between(
      mortality, 
      quantile(mortality, 0.25, na.rm=T)-
        1.5*IQR(mortality, na.rm=T),
      quantile(mortality, 0.75, na.rm=T)+
        1.5*IQR(mortality, na.rm=T))) %>% 
  filter(outlier)

ori <- sum(range(cutpoints$mortality))/2
sca <- seq(range(cutpoints$mortality)[1], 
           range(cutpoints$mortality)[2], 
           length.out = 7)[-4]

round(ori, 2) # The origin

round(sca, 2) # The horizon scale cutpoints

### make the plot 

ds %>% ggplot() +
  geom_horizon(aes(date, 
                   mortality,
                   fill = ..Cutpoints..), 
               origin = ori, horizonscale = sca) +
  scale_fill_hcl(reverse = T) +
  facet_grid(country~.) +
  theme_few() +
  theme(
    panel.spacing.y=unit(0, "lines"),
    strip.text.y = element_text(size = 7, angle = 0, hjust = 0),
    axis.text.y = element_blank(),
    axis.title.y = element_blank(),
    axis.ticks.y = element_blank(),
    panel.border = element_blank(),
    legend.position = "bottom",
  ) +
  scale_x_date(expand=c(0,0), 
               date_breaks = "1 month", 
               date_labels = "%b") +
  xlab('Date') +
  ggtitle('Average weekly excess mortality (EU countries)', 
          'from January 2020 to November 2021')


ggsave("Figures/Fig_01.png", width = 30, height = 18, units = "cm", dpi = 400)


### mobility data plot 

mob <- read_csv('Input/mob_data.csv')

mob %>%
  ggplot(aes(value, country_region_code, color = wave))+
  geom_vline(aes(xintercept=0), color = 'gray', lwd=1.5) +
  geom_point(size = 3.5, alpha = .7) +
  facet_grid(~variable,space = "fixed") +
  labs(x=NULL, y=NULL,
       title="Mobility data trends across European countries during the pandemic waves", 
       subtitle = 'Percentage change compared to the baseline') +
  theme(
    legend.position = 'bottom',
    plot.title.position = "plot",
    plot.title = element_text(face="bold", margin= margin(b=5)),
    plot.caption = element_markdown(hjust=0, color="darkgray"),
    plot.caption.position = "plot",
    panel.background = element_blank(),
    axis.ticks = element_blank(),
    axis.text.x = element_text(color="darkgray"),
    panel.grid.major.x = element_line(color="gray", size=0.1),
    panel.grid.major.y = element_line(color="gray",
                                      size=0.1, linetype="dotted"))  +
  scale_color_manual(name=NULL,
                     breaks=c("First wave", "Second wave", 'Third wave'),
                     values=c("#E8C547", "#15607a", "#BA1B1D"),
                     labels=c("First wave", "Second wave", 'Third wave'))
  
ggsave("Figures/Fig_03.png",width = 30, height = 18, dpi = 500, unit = 'cm')
  
### make the map in R 


data("nuts2006")
# Plot a layer with the extent of the EU28 countries with only a background color
plot(nuts0.spdf, border = NA, col = NA, bg = "#A6CAE0")

# Plot non European space
plot(world.spdf, col  = "#E3DEBF", border = NA, add = TRUE)
# Plot Nuts2 regions
plot(nuts0.spdf, col = "grey60",border = "white", lwd = 0.4, add = TRUE)
# plot the countries population
propSymbolsLayer(
  spdf = nuts0.spdf, 
  df = nuts0.df, 
  spdfid = "id", 
  dfid = "id", 
  var = "pop2008", 
  legend.pos = "topright", 
  col = "red4", 
  border = "white", 
  legend.title.txt = "Population" 
)
# layout
layoutLayer(title = "Population in Europe, 2008",
            sources = "Data: Eurostat, 2008",
            author =  paste0("cartography ", packageVersion("cartography")),
            scale = 500, frame = TRUE, col = "#688994") 
# north arrow
north("topleft")

  

    