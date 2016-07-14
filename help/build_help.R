
rm(list=ls())
library(knitr)
library(markdown)
library(rmarkdown)

help_dir <- '/Volumes/ECHANGE/github/CDT/help/html'
mathjax_dir <- '/Users/rija/markdown/MathJax-master/unpacked'
sources_dir <- '../sources'
r_images <- 'images/R_images'

setwd(help_dir)
Sys.setenv(RMARKDOWN_MATHJAX_PATH = mathjax_dir)

all_src <- list.files(path=sources_dir, pattern='.Rmd',full.names=TRUE,recursive=TRUE)
rmd_files <- basename(all_src)
parent_dir <- sapply(strsplit(all_src,'/'), function(x) x[3])
all_files <- gsub('.Rmd','',rmd_files)
html_files <- paste(all_files,'.html',sep='')
figure_dir <- paste(all_files,'_files',sep='')
figure_files <- list.files(r_images,recursive=TRUE,full.names=TRUE)

## Test change 
# git_modifed_added <- do.call('rbind',lapply(strsplit(system('git status -s',intern=TRUE),' '), function(x) x[x!=""]))
# is_change <- grep('help',git_modifed_added[,2])
# if(length(is_change)>0)
# changed_files <- git_modifed_added[is_change,2]
# # and do build else do nothing/ ifif(length(is_change)>0){buil}

# # search all Rmd files 
# # search in images and get files corresponding
# # if any change in common_dir re-build all

## replace parent_dir, rmd_files, all_files, html_files and figure_dir to the new changed files

# all_files <- c("index","cdt_data_input","cdt_file_menu")
# parent_dir<-c("index","cdt_file","cdt_file")
# rmd_files<-paste(all_files,'.Rmd',sep='')
# html_files<-paste(all_files,'.html',sep='')
# figure_dir<-paste(all_files,'_files',sep='')

# all_files <- "cdt_file_menu"
# rmd_files<-"cdt_file_menu.Rmd"
# parent_dir<-"cdt_file"
# html_files<-"cdt_file_menu.html"
# figure_dir<-"cdt_file_menu_files"

######

exist_Rimage_fig<-sapply(strsplit(list.files(r_images),'\\.'),'[[',1)%in%all_files
if(any(exist_Rimage_fig)) unlink(figure_files[exist_Rimage_fig])
exist_html_fig <- dir.exists(figure_dir)
if(any(exist_html_fig)) unlink(figure_dir[exist_html_fig],recursive = TRUE, force = TRUE)
exist_html_files <- file.exists(html_files)
if(any(exist_html_files)) unlink(html_files[exist_html_files])
exist_rmd_wd_files <- file.exists(rmd_files)
if(any(exist_rmd_wd_files)) unlink(rmd_files[exist_rmd_wd_files])


file.copy(file.path(sources_dir,parent_dir,rmd_files),'.')
for(jj in 1:length(rmd_files)) render(rmd_files[jj])

exist_rmd_wd_files <- file.exists(rmd_files)
if(any(exist_rmd_wd_files)) unlink(rmd_files[exist_rmd_wd_files])

unlink('../index.html')
file.symlink(file.path(getwd(),'index.html'), '../index.html')
# system('ln -s "$PWD"/index.html ../index.html')

