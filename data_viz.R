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
library(tidyverse)
library(bibliometrix)

#Define working directory
working_dir<-"/nfs/njones-data/Research Projects/JAWRA_Special_Issue/lit_analysis/"

##################################################################################
#Step 2:  Wrangle Citaiton Data---------------------------------------------------
##################################################################################
#Import .bib file info (see Bibliometrix vignette: https://cran.r-project.org/web/packages/bibliometrix/vignettes/bibliometrix-vignette.html)

#read .bib file
bib<-readFiles(paste0(working_dir,"connectivity_lit.bib"))

#Convert to df
bib<-convert2df(bib, dbsource = 'isi', format='bibtex')

#create summary
results <- biblioAnalysis(bib, sep = ";")
results <- summary(object = results, k=10, pause = F)
