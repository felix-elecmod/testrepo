################################################################################
## PURPOSE: R code to generate spatial charts on fti data
## NOTES: Requires FTI dataset with project region (Landkreis), jobs created
## and project id
## Code can be adapted for anz German region
## version: Schleswig-Holstein v0.1
################################################################################
# Packages
library(tidyverse)
library(rgdal)
#install.packages("maps")
library(maps)
library(data.table)
library(janitor)
#install.packages("ggrepel")
library(ggrepel)

################################################################################
# Globals

# clear working directory
rm(list=ls())

#set working directorz to file location
wd <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(wd)

################################################################################
# Code

# Download dataset from: 
# https://hub.arcgis.com/datasets/ae25571c60d94ce5b7fcbf74e27c00e0/about 
# And unzip in working directory

spdf <- readOGR(
    dsn = paste0(getwd(),"/","vg2500_geo84"), # folder name
    layer = "vg2500_krs", #krs stands for Kreis
    verbose = FALSE
)

# filter to Schleswig-Holstein
sh_rs <- spdf@data %>% filter(str_sub(RS,1,2)=="01") %>% pull(RS) 
# SH has prefix 01

spdf <- subset(spdf,spdf$RS%in%sh_rs)

# extract coordinates for every Kreis
coords <- matrix(NA,ncol = 3,nrow=length(spdf)) %>% as_tibble()
colnames(coords) <- c("region","long","lat")
for (i in 1:length(spdf)){
  coords[i,1] <- spdf@data$GEN[i]
  coords[i,2] <- spdf@polygons[[i]]@labpt[1]
  coords[i,3] <- spdf@polygons[[i]]@labpt[2]
}


# import project data
projects <- fread("fdi.csv") %>% clean_names()
proj_summary <- projects %>% group_by(region) %>%
  summarise(projects=n(),jobs_created=sum(jobs_created))

coords <- coords %>% left_join(y=proj_summary,by="region")
coords2 <- coords %>%
  mutate(projects=replace_na(projects,0),
         jobs_created=replace_na(jobs_created,0))

# Create chart for number of projects
ggplot() +
  geom_polygon(data = spdf,
               aes( x = long, y = lat, group = group),
               fill="#DDDDDD", color="darkgrey")+
  geom_point(data=coords,
             aes(x=long,y=lat,size=projects),color="#6482A6",alpha=0.7)+
  scale_size_continuous(range=c(1,19),breaks = c(3,5,8,10))+
  geom_text_repel(data=coords2 %>% filter(!is.na(projects)),
                  aes(x=long,y=lat,label=paste0(region,", ",projects)),
                  size=3.5,color="#151E29",fontface="bold")+
  theme_void(base_family = "Arial")+labs(size="Projektanzahl")+
  theme(legend.title=element_text(face="bold",color="#333F50"))
ggsave(filename = "projects_map.jpg",dpi=600)


# Create chart for number of jobs created
ggplot() +
  geom_polygon(data = spdf,
               aes( x = long, y = lat, group = group),
               fill="#DDDDDD", color="darkgrey")+
  geom_point(data=coords,
             aes(x=long,y=lat,size=jobs_created),color="#6482A6",alpha=0.7)+
  scale_size_continuous(range=c(1,19))+
  geom_text_repel(data=coords2 %>% filter(!is.na(jobs_created)),
                  aes(x=long,y=lat,label=paste0(region,", ",jobs_created)),
                  size=3.5,color="#151E29",fontface="bold")+
  theme_void()+labs(size="Geschaffene \n Arbeitsplätze")+
  theme(legend.title=element_text(face="bold",color="#333F50"))
ggsave(filename = "jobs_map.jpg",dpi=600)

    