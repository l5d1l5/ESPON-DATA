library(tidyverse)
library(ggHoriPlot) 
library(ggthemes)

#### set working directory 

setwd('Users/Mohamed/Documents/ESPON-DATA/03_Figures/02_Report Plots')

### read the data 

ds = read_csv('ESPON-DATA/03_Figures/02_Report Plots/Input/d_figure_1.csv')


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
#> [1] 6.58

round(sca, 2) # The horizon scale cutpoints

### make the plot 

ds %>% ggplot() +
  geom_horizon(aes(date, 
                   mortality,
                   fill = ..Cutpoints..), 
               origin = ori, horizonscale = sca) +
  scale_fill_hcl( reverse = T) +
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
          'from February 2020 to Octobre 2021')


ggsave("Fig_01.png", width = 30, height = 18, units = "cm", dpi = 400)
