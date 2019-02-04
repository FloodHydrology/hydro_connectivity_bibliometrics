##################################################################################
#Name: Hydrologic Connectivity Data Viz
#Coder: C. Nathan Jones
#Date: 1/1/2019
#Purpose: Create plot for JAWRA Featured Collection Introduction 
##################################################################################

##################################################################################
#Step 1:  Setup Workspace---------------------------------------------------------
##################################################################################
#Clear Memory
rm(list=ls(all=TRUE))

#required packages
library(tidyverse)    #tidyverse family of packages
library(lubridate)    #for handling dates
library(readxl)       #for importing data from MS Excel 
library(bibliometrix) #bibliography analysis
library(cowplot)      #plotting
library(gridExtra)    #plotting
library(treemapify)   #plotting
library(packcircles)  #plotting

#Define data directory
data_dir<-"data/"

##################################################################################
#Step 2:  Wrangle Citaiton Data---------------------------------------------------
##################################################################################
#Import .bib file info (see Bibliometrix vignette: https://cran.r-project.org/web/packages/bibliometrix/vignettes/bibliometrix-vignette.html)
#read .bib file
bib<-readFiles(paste0(data_dir,"connectivity_lit.bib"))

#Convert to df
bib<-convert2df(bib, dbsource = 'isi', format='bibtex')

#create summary
results <- biblioAnalysis(bib, sep = ";")
results <- summary(object = results, k=500, pause = F)

#read in manual classification tables
journals<-read_excel(paste0(data_dir,"classification.xlsx"), sheet="journals")
key_words<-read_excel(paste0(data_dir,"classification.xlsx"), sheet="key_words")
hydro_units<-read_excel(paste0(data_dir,"classification.xlsx"), sheet="hydro_units")

##################################################################################
#Step 3:  Plotting----------------------------------------------------------------
##################################################################################
#A) Publication rate over time----------------------------------------------------
#create tibble of year and # of pubs
df<-as_tibble(results$AnnualProduction) 
colnames(df)<-c("year", "pubs")
df$year<-as.numeric(paste(df$year))

#create exponential model
model<-lm(log(pubs)~year, df)
model_data<-tibble(x=df$year, y=exp(fitted(model)))

#create plot
p1<-df %>%
  #Start plotting device
  ggplot(aes(x=year, y=pubs)) +
  #Add points
  geom_point(col="grey30") +
  #Add exp model
  geom_line(data = model_data, 
            aes(x, y), 
            col="grey70",
            size=1.3,
            linetype = 2) +
  #Add text describing model
  geom_text(aes(x=2003, y=75), label="RSQ=0.94\np<0.001", col="grey30")+
  #Axis labels
  labs(x="Year", y = "Number of Publications") +
  #Add Theme Elements
  theme_bw(base_size=12) +
  theme(
    #bold font for both axis text
    axis.text=element_text(face="bold"),
    #set thickness of axis ticks
    axis.ticks=element_line(size=0.4),
    #remove plot background
    plot.background=element_blank(), 
    #Decrease right margin for final plot
    plot.margin=unit(c(5.5, 12, 5.5, 5.5), "points")
  )

#B) Treemap of discpline and journal---------------------------------------------
#create tibble of journal and number of pubs
df<-as_tibble(results$MostRelSources) %>%
  #Rename collumns
  rename(journal = `Sources       `, pubs=Articles) %>%
  #format collumns
  mutate(#trim white space in journal names
         journal = paste(trimws(journal)), 
         #make pubs numeric
         pubs = as.numeric(paste(pubs))
         ) %>%
  #Remove journals with <4 citations
  filter(pubs>3) %>%
  #left join with journal classification table
  left_join(.,journals) %>%
  #redo journal names
  mutate(journal=str_to_title(journal))

#Shorten journals that are hard to read on plot
df$journal[df$journal == "Ecohydrology" ] <- "Eco- hydrology"
df$journal[df$journal == "Bioscience" ] <- "Bio- science"
df$journal[df$journal == "Hydrobiologia" ] <- "Hydro- biologia"
df$journal[df$journal == "Hydrobiologia" ] <- "Hydro- biologia"
df$journal[df$journal == "Biogeochemistry" ] <- "Biogeo- chemistry"
df$journal[df$journal == "Canadian Journal Of Fisheries And Aquatic Sciences" ] <- "Can J Fish Aquat Sci"
df$journal[df$journal == "Science Of The Total Environment"] <- "Sci Total Environ"
df$journal[df$journal == "Journal Of The American Water Resources Association"] <- "JAWRA"
df$journal[df$journal == "Journal Of Geophysical Research-Biogeosciences"] <- "JGR-Biogeoscience"
df$pubs[df$journal=="Freshwater Science"]<-df$pubs[df$journal=="Freshwater Science"]+
                                            df$pubs[df$journal=="Journal Of The North American Benthological Society"]
df<-df[df$journal!="Journal Of The North American Benthological Society",]
df$journal[df$journal == "Transactions Of The American Fisheries Society"] <- "Trans Am Fish Soc"

#Create color pallete matrix
color<- tibble(classification = c("environmental science","hydrology","ecology","biogeochemistry", "earth science"), 
             org=c(4,2,3,1,5))
df<-left_join(df, color) %>% 
  arrange(org) %>%
  mutate(color = factor(org))

#Plot tree map
p2<-ggplot(df, aes(area = pubs, label=journal, subgroup = classification, fill=color)) +
  geom_treemap() +
  geom_treemap_text(colour = "white", place = "centre",grow = F, reflow=T) +
  geom_treemap_subgroup_border(lwd=0.2, col="grey90") +
  scale_fill_brewer(palette="Set1") +
  theme_bw() +
  theme(legend.position="none")

#C) Circle plot of functions-----------------------------------------------------
#Add unique ID to bib tibble
bib$UID<-seq(1, nrow(bib))

#Create long-fromat tibble with ID and key word (for both author and ISI defined IDs)
w1<-tibble(ID = bib$UID, key_word = bib$ID) %>%
  #create long format
  separate_rows(., key_word,sep=";") %>%
  #trim white space
  mutate(key_word = trimws(paste(key_word)))
w2<-tibble(ID = bib$UID, key_word = bib$DE) %>%
  #create long format
  separate_rows(., key_word,sep=";") %>%
  #trim white space
  mutate(key_word = trimws(paste(key_word)))
w<-bind_rows(w1, w2)

#Estimate number of pubs 
df<-key_words %>%
  #format key words tibble
  select(key_word, class) %>% 
  filter(str_detect(class, "function")) %>%  
  #join tibbles [ie journal key words and key word classifications]
  left_join(w, .) %>% 
  #select unique combinations of ID and class
  na.omit(.) %>%
  select(ID, class) %>%
  distinct(ID, class) %>%
  #tally functions represented by different pubs
  count(class) 
  
#Plot (https://www.r-graph-gallery.com/306-custom-circle-packing-with-one-level/)
#setup plotting layout
packing<-circleProgressiveLayout(df$n, sizetype = "area")
df <- cbind(df, packing)
df.gg<-circleLayoutVertices(packing, npoints = 50)
df.gg$value=rep(df$class, each=51)

#Format Names for text
df$class[df$class=="biological function"]<-"Biological"
df$class[df$class=="chemical function"]<-"Chemical"
df$class[df$class=="physical function"]<-"Physical"

#plot
p3<-ggplot() + 
  #Plot Polygongs
  geom_polygon(data = df.gg, 
               aes(x, y, group = id, fill=as.factor(id)), 
               colour = "black") +
  scale_fill_manual(values = c("#4daf4a","#e41a1c",  "#377eb8")) +
  #Add text
  geom_text(data = df, aes(x, y, label = paste0(class,"\n[n=",n,"]"), size=4)) +
  #theme 
  theme_void(base_size = 12) + 
  theme(legend.position="none") +
  coord_equal()

#D) Lolly-pop plot of hydrologic units------------------------------------------
#Create tibble of publication counts
df<-tibble(ID = bib$UID, key_word = bib$ID) %>%
  #create long format
  separate_rows(., key_word,sep=";") %>%
  #trim white space
  mutate(key_word = trimws(paste(key_word))) %>%
  #join hydro units classification table
  left_join(. , hydro_units) %>%
  #Remove irrelivant key words
  select(ID, hydro_units) %>%
  na.omit(.) %>%
  distinct(.) %>%
  #Tally hydro units
  count(hydro_units)

#Change hydro units for display names
df$hydro_units[df$hydro_units=="Watershed"] <-"Watersheds and Stream Networks"
df$hydro_units[df$hydro_units=="River"] <-"Rivers and Streams"
df$hydro_units[df$hydro_units=="Wetland"] <-"Wetlands"
df$hydro_units[df$hydro_units=="Riparian"] <-"Riparian Zones and Floodplains"
df$hydro_units[df$hydro_units=="Hillslope"] <-"Critical Zone, Hillslopes, and Soils"
df$hydro_units[df$hydro_units=="Lake"] <-"Lakes, Reservoirs, and Ponds"
df$hydro_units[df$hydro_units=="Hyporheic Zone"] <-"Hyporheic Zones"

#preserve order in plot by changing hydro_units to factor
df<- df %>% 
  arrange(n) %>%
  mutate(hydro_units = factor(hydro_units, hydro_units))

#Create plot
p4<-ggplot(df, aes(x=hydro_units, y=n)) + 
  #Add lollipops
  geom_segment(aes(x=hydro_units, xend=hydro_units, y=0, yend=n), color="grey30") +
  geom_point(col="blue", size=4, alpha=0.8) +
  #Add lables
  labs(x="Hydrologic Units", y="Number of Publications")+
  #Add theme info
  theme_bw(base_size = 12) +
  coord_flip() +
  theme(
    panel.grid.major.y = element_blank(),
    panel.border = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text.y =  element_text(margin=margin(l=2), hjust=1)
  )
  
#Export multi plot -------------------------------------------------------------------------------
ggdraw()+
  draw_plot(p1, x=0.05,   y=0.5, width =0.55, height =0.5) +
  draw_plot(p3, x=0.6, y=0.5, width =0.4, height =0.5) +
  draw_plot(p4, x=0.05,   y=0,   width =0.9, height =0.5) +
  draw_plot_label(c("A)", "B)", "C)"), 
                  x=c(0,0.6, 0), 
                  y=c(1,1,0.5), 
                  size=12)
ggsave("figure1.pdf", device="pdf", width = 6, height= 4.5, units="in")
