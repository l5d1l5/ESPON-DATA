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
  scale_fill_hcl(palette = 'RdBu',reverse = T) +
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


iris %>%
  ggplot(aes(x =  Sepal.Length, y = Sepal.Length)) +
  geom_point() +
  facet_wrap(~Species, scale = 'free')


mob <- read_csv('Input/mob_data.csv')

mob %>%
  ggplot(aes(value, country_region_code, color = wave))+
  geom_vline(aes(xintercept=0), color = 'gray', lwd=1.5) +
  geom_point(size = 3.5, alpha = .7) +
  facet_wrap(~variable, scales = "free_y", nrow = 1) +
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

  
### clusters 
library(sf)
library(ClustGeo)
library(tidyverse)

### 2019 
map <- st_read('Med/GIS.shp')

dat_2019 <- read_csv('D0_2019.csv') %>% as.data.frame()
rownames(dat_2019) <- dat_2019[, 1]  ## set rownames
dat_2019 <- dat_2019[, -1]   


D.geo <- read_csv("D.geo.csv")

D0 <- dist(dat_2019) 

tree <- hclustgeo(D0)

P5 <- cutree(tree,6)

plot(tree,hang = -1, label = FALSE, 
     xlab = "", sub = "",
     main = "Ward dendrogram with D0 only")

rect.hclust(tree ,k = 6, border = c(4,5,3,2,1))
legend("topright", legend = paste("cluster",1:6), 
       fill=1:6,bty= "n", border = "white")


plot(map, border = "grey", col = P5, 
     main = "Partition P5 obtained with D1-matrix only")
legend("topleft", legend = paste("cluster",1:6), 
       fill = 1:6, bty = "n", border = "white")


D1 <- as.dist(D.geo)

range.alpha <- seq(0,1,0.1)
K <- 6
cr <- choicealpha(D0, D1, range.alpha, 
                  K, graph = FALSE)
cr$Q # proportion of explained inertia

plot(cr)


tree <- hclustgeo(D0,D1,alpha=0.1)
P5bis <- cutree(tree,6)
    
sp::plot(map, border = "grey", col = P5bis, 
         main = "Partition P5bis obtained with alpha=0.1 
         and geographical distances")
legend("topleft", legend=paste("cluster",1:6), 
       fill=1:6, bty="n",border="white")

### Change the partition to take neighborhood constraint into account

library(spdep)

list.nb <- poly2nb(map, row.names = colnames(dat_2019)) #list of neighbours of each city

A <- nb2mat(list.nb,style="B", zero.policy = TRUE)

diag(A) <- 1
colnames(A) <- rownames(A) <- city_label
A[1:5,1:5]

D1 <- as.dist(1-A)

range.alpha <- seq(0,1,0.1)
K <- 6
cr <- choicealpha(D0, D1, range.alpha,
                  K, graph=FALSE)
plot(cr)

cr$Qnorm

plot(cr, norm = TRUE)

tree <- hclustgeo(D0, D1, alpha  =0.1)
P5ter <- cutree(tree,6)
sp::plot(map, border="grey", col=P5ter, 
         main=" Partition P5ter obtained with
         alpha=0.1 and neighborhood dissimilarities")
legend("topleft", legend=1:6, fill=1:6, col=P5ter)

write.csv(P5ter, 'index_2019.csv')


#### 2020 

dat_2020 <- read_csv('D0_2020.csv') %>% as.data.frame()
rownames(dat_2020) <- dat_2020[, 1]  ## set rownames
dat_2020 <- dat_2020[, -1]   

D0 <- dist(dat_2020) 

tree <- hclustgeo(D0)

P5 <- cutree(tree,6)

plot(tree,hang = -1, label = FALSE, 
     xlab = "", sub = "",
     main = "Ward dendrogram with D0 only")

rect.hclust(tree ,k = 6, border = c(4,5,3,2,1))
legend("topright", legend = paste("cluster",1:6), 
       fill=1:6,bty= "n", border = "white")


plot(map, border = "grey", col = P5, 
     main = "Partition P5 obtained with D0 only")
legend("topleft", legend = paste("cluster",1:6), 
       fill = 1:6, bty = "n", border = "white")


D1 <- as.dist(D.geo)

range.alpha <- seq(0,1,0.1)
K <- 6
cr <- choicealpha(D0, D1, range.alpha, 
                  K, graph = FALSE)
cr$Q # proportion of explained inertia

plot(cr)


tree <- hclustgeo(D0,D1,alpha=0.1)
P5bis <- cutree(tree,6)

sp::plot(map, border = "grey", col = P5bis, 
         main = "Partition P5bis obtained with alpha=0.1 
         and geographical distances")
legend("topleft", legend=paste("cluster",1:6), 
       fill=1:6, bty="n",border="white")

### Change the partition to take neighborhood constraint into account


list.nb <- poly2nb(map, row.names = rownames(dat_2020)) #list of neighbours of each city

A <- nb2mat(list.nb,style="B")
diag(A) <- 1
colnames(A) <- rownames(A) <- city_label
A[1:5,1:5]

D1 <- as.dist(1-A)

range.alpha <- seq(0,1,0.1)
K <- 6
cr <- choicealpha(D0, D1, range.alpha,
                  K, graph=FALSE)
plot(cr)

cr$Qnorm

plot(cr, norm = TRUE)

tree <- hclustgeo(D0, D1, alpha  =0.1)
P5ter <- cutree(tree,6)
sp::plot(map, border="grey", col=P5ter, 
         main=" Partition P5ter obtained with
         alpha=0.1 and neighborhood dissimilarities")
legend("topleft", legend=1:6, fill=1:6, col=P5ter)


write.csv(P5ter, 'index_2020.csv')


#### test map 

s <- cbind(dat_2019, map)




#### test map 

library(mapsf)

mf_theme("dark")

mtq <- mf_get_mtq()
# define the figure layout (1 row, 2 columns)
par(mfrow = c(1, 2))
# first map
mf_map(mtq)
mf_map(mtq, "POP", "prop")
mf_title("Population")
# second map
mf_map(mtq, "MED", "choro")
mf_title("Median Income")




library(sf)
library(cartography)
# path to the geopackage file embedded in cartography
path_to_gpkg <- system.file("gpkg/mtq.gpkg", package="cartography")
# import to an sf object
mtq <- st_read(dsn = path_to_gpkg, quiet = TRUE)
# plot municipalities (only the backgroung color is plotted)
plot(st_geometry(mtq), col = NA, border = NA, bg = "lightblue1")
# plot isopleth map
smoothLayer(
  x = mtq, 
  var = 'POP',
  typefct = "exponential",
  span = 4000,
  beta = 2,
  nclass = 12,
  col = carto.pal(pal1 = 'brown.pal', n1 = 12),
  border = "grey",
  lwd = 0.1, 
  mask = mtq, 
  legend.values.rnd = -3,
  legend.title.txt = "Population\nPotential",
  legend.pos = "topright", 
  add=TRUE
)
## Functions related to Stewart's potential are deprecated.
## Please use the `potential` package instead.
## https://riatelab.github.io/potential/
# annotation on the map
text(x = 692582, y = 1611478, cex = 0.8, adj = 0, font = 3,  labels = 
       "Distance function:\n- type = exponential\n- beta = 2\n- span = 4 km")
# layout
layoutLayer(title = "Population Distribution in Martinique",
            sources = "Sources: Insee and IGN, 2018",
            author = paste0("cartography ", packageVersion("cartography")),
            frame = FALSE, north = FALSE, tabtitle = TRUE, theme = "brown.pal")
# north arrow
north(pos = "topleft")


