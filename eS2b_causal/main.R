## ================================================================================================================
## Analysis, Chatbot Stigma -- Interaction Study
## ================================================================================================================

## clear workspace
rm(list = ls())

options(download.file.method = "libcurl")

if (!require(pacman)) { install.packages("pacman") }
pacman::p_load('effsize', 'pwr')

setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) #set working directory to current directory

d <- read.csv('data.csv')

## perform attention exclusions: 
# this will remove responses from the dataframe that failed attention checks (i.e., "1" or "2")
d <- subset(d, (d$att1 == 2 & d$att2 == 2))
dim(d) # number of participants should decrease after attention exclusions

## get number of participants BEFORE exclusions: 
n_original <- dim(d)[1] # extracting number of rows only, not columns
print(paste0("Number or participants hired: ", n_original))

## perform comprehension exclusions: 
# this will remove responses from the dataframe that failed comprehension checks (i.e., "2")
d <- subset(d, ((d$comp_1 == 2 & (((d$essence == 2) & (d$condition == "abstract")) | ((d$essence == 1) & (d$condition == "concrete"))))))
print(paste0("Number or participants after comprehension exclusion: ", dim(d)[[1]]))

d$wtp <- as.numeric(d$wtp)
d$willing_friend_1 <- as.numeric(d$willing_friend_1)
d$willing_romantic_1 <- as.numeric(d$willing_romantic_1)

## Check if WTP is skewed and fix
hist(d$wtp,  main = "Willingness to Pay", xlab = "Money (USD)", border = "black", col = "light blue")
shapiro.test(d$wtp)

table(d$condition)

## transform values to reduce skew
d$wtp_logged <- log(d$wtp+1) + 1
hist(d$wtp_logged,  main = "Willingness to Pay", xlab = "Money (USD)", border = "black", col = "light blue")
shapiro.test(d$wtp_logged)

table(d$gender)[2] / sum(table(d$gender)) ## percentage of females
mean(na.omit(as.numeric(d$age)))

table(d$ai_companion_exp)

dvs <- c('willing_friend_1', 'willing_romantic_1', 'wtp')

for (dv in dvs) {
    print(paste0("*-*-*-*  abstract v. concrete - ", dv, "  *-*-*-*"))
  # t-test between conditions
    x <- as.numeric(d[d$condition == "abstract", dv])
    y <- as.numeric(d[d$condition == "concrete", dv])
    vart <- var.test(x, y)
    ttest <- t.test(x, y, paired = FALSE, var.equal = vart$p.value > .05, data = d)
    print(ttest)

    # Cohen's d
    cohens_d <- effsize::cohen.d(x, y)
    print(cohens_d)
}

# Power test with pwr package, for t-test with cohen's d of 0.12
pwr.t.test(d = 0.1240717, sig.level = 0.05, power = 0.8)
