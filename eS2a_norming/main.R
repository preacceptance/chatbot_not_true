## clear workspace
rm(list = ls())

options(download.file.method = "libcurl")

if (!require(pacman)) { install.packages("pacman") }
pacman::p_load('effsize')

setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) #set working directory to current directory

d <- read.csv('data_v5.csv')

## perform attention exclusions: 
# this will remove responses from the dataframe that failed attention checks (i.e., "1" or "2")
d <- subset(d, (d$att_1 == 2 & d$att_2 == 2))
dim(d) # number of participants should decrease after attention exclusions

## get number of participants BEFORE exclusions: 
n_original <- dim(d)[1] # extracting number of rows only, not columns
print(paste0("Number or participants hired: ", n_original))

## perform comprehension exclusions: 
# this will remove responses from the dataframe that failed comprehension checks (i.e., "2")
d <- subset(d, ((d$comp_1 == 2 & d$condition == "abstract") | (d$comp_1 == 5 & d$condition == "concrete")))
print(paste0("Number or participants after comprehension exclusion: ", dim(d)[[1]]))


table(d$gender)
mean(na.omit(as.numeric(d$age)))

########## t-test of valence between conditions ##########
x <- as.numeric(d[d$condition == "abstract", "valence_4"])
y <- as.numeric(d[d$condition == "concrete", "valence_4"])
vart <- var.test(x, y)
ttest <- t.test(x, y, paired = FALSE, var.equal = vart$p.value > .05, data = d)
print(ttest)

# Cohen's d
cohens_d <- effsize::cohen.d(x, y)
print(cohens_d)


########### t-test of sociality between conditions ###########
x <- as.numeric(d[d$condition == "abstract", "sociality_4"])
y <- as.numeric(d[d$condition == "concrete", "sociality_4"])
vart <- var.test(x, y)
ttest <- t.test(x, y, paired = FALSE, var.equal = vart$p.value > .05, data = d)
print(ttest)

# Cohen's d
cohens_d <- effsize::cohen.d(x, y)
print(cohens_d)
