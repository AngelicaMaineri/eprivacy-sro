# STEP 1: Install/Load packages ------------------------------------------------
#install.packages("[x]")
#library([x])

library(tidyverse) # basic functions
library(ggplot2) #for graphs
library(ggthemes) #for graphs' aestethics
library(extrafont) #for graphs' aestethics
library(rworldmap) # for maps!
library(mapproj) # for maps!
library(dplyr) # for data transformations
library(readxl) # to import excel files
library(haven) # to import spss and stata files

citation(package = "ggplot2")
citation(package = "rworldmap")
citation(package = "mapproj")



# STEP 2. Get the world map ----------------------------------------------------
world <- map_data("world")

#Data look like this:
head(world, 25)

# STEP 3: Read in data to be mapped --------------------------------------------
desi <- read_dta("Data/desi2017.dta")
head(desi)


# STEP 4: Prepare data
 # check if country names match with 'world' data
unique(desi$name) %in% unique(world$region)

check <- unique(desi$name) %in% unique(world$region)
unique(desi$name)[!check]
desi$name[desi$name=="Czechia"] <- "Czech Republic"
desi$name[desi$name=="United Kingdom"] <- "UK"

unique(desi$name) %in% unique(world$region)

# reshape dataset
desiw <- reshape(desi, idvar = "name", timevar = "series", direction = "wide")

desi2 <- desi %>%
  arrange(name) %>%
  separate(series, into = c("indicator", "label"), sep = " ") %>%
  dplyr:::select(label, name, y)


# STEP 4.2: summarize data at country level and clean var ------------------------

desi_w<- desi2 %>% 
  spread(label, y)
colnames(desi_w)

desi_w$desi <- rowSums(desi_w[, c(2, 3, 4, 5,6)])


# STEP 5:  Join data ----------------------------------------------------------
desimap <- world %>% 
  left_join(desi_w, by = c("region" = "name"))

# STEP 6: Define baseline graphic features
theme_info <- 
  theme_classic() +
  theme(axis.line = element_blank(), axis.text.x = element_blank(),
        axis.text.y = element_blank(), axis.ticks = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.position = c(0.1, 0.3), 
        legend.title = element_blank(), 
        plot.title=element_text(hjust=0.5), 
        plot.subtitle = element_text(hjust=0.5))

# STEP 7: Make maps --------------------------------------------------------------------
desimap %>% 
    mutate(desi = round(desi, 2)) %>% 
    ggplot(aes(x = long, y = lat,
               group = group, fill = desi)) + 
    geom_polygon(color = "white") + 
    xlim(-25, 45.0) + ylim(35, 70) +
    labs(fill = "desi",
         title = "Digital Economy and Society Index (DESI) 2017 ",
         subtitle = "Source: European Commission, Digital Scoreboard via Eurostat") +
    scale_fill_gradientn(colors = c("darkslategray1", "deepskyblue4"),
                         values = scales::rescale(c(min(desi_w$desi),
                                                    mean(desi_w$desi), 
                                                    max(desi_w$desi))),
                         breaks = c(min(desi_w$desi),
                                    mean(desi_w$desi), 
                                    max(desi_w$desi)),
                         labels = c(round(min(desi_w$desi), 2),
                                    round(mean(desi_w$desi), 2), 
                                    round(max(desi_w$desi), 2)),
                         limits = c(min(desi_w$desi), 
                                    max(desi_w$desi)))  +
    theme_info

# STEP 8: Save graphs ----------------------------------------------------------
ggsave("G:/My Drive/Paper Eprivacy/Figures/figure1.png", 
       plot=last_plot(),  width=9, height=7)
