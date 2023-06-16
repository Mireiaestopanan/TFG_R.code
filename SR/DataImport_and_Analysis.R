### ARTICLES

#install.packages("sqldf")
#install.packages("dplyr")
#install.packages("ggpubr")

library(sqldf)
library(tidyverse)
library(tidyr)
library(Hmisc)
library(readxl)
library(plyr)
library(dplyr)
library(scales)
library(kableExtra)
library(gridExtra)
library(stats)
library(gplots)
library(cluster)
library(factoextra)
library(FactoMineR)
library(data.table)
library(ggpubr)
library(grDevices)
library(ggplot2)

###################################################### DESCRIPTIVE ANALYSIS ####################################################################################

carac <- read_excel("C:/Users/34692/OneDrive/Escriptori/MIREIA/Universitat/4rt carrera/1. TFG/Reportar_vars_selected_articles_ordinal.xlsx",
                    sheet='Analisis')
carac <- as.data.frame(carac)

# General characteristics of the articles
## Country
(country <- table(carac$`Country of the data`))
carac_country <- carac %>%
  separate_rows(`Country of the data`, sep = ", ")
carac_country[carac_country$`Country of the data`=="Australia and England", "Country of the data"] <- "Australia"
carac_country[nrow(carac_country) + 1, "Country of the data"] <- "England"
(country <- table(carac$`Country of the data`))
(country <- table(carac_country$`Country of the data`))
country_vec <- names(country)
continent_vec <- rep(NA, length(country_vec))
continent_vec[country_vec == "Australia" | country_vec == "New Zealand"] <- "Oceania"
continent_vec[country_vec == "Belgium" | country_vec == "England" | country_vec == "Finland" | country_vec == "France" | country_vec == "Germany" | country_vec == "Greece" | country_vec == "Italy" | country_vec == "Netherlands" | country_vec == "Spain" | country_vec == "Switzerland"] <- "Europe"
continent_vec[country_vec == "Brazil"] <- "America"
continent_vec[country_vec == "United States"] <- "America"
continent_vec[country_vec == "Not Reported"] <- "Unknown"
carac_country_continent <- data.frame(Country = country_vec, Continent = continent_vec, stringsAsFactors = FALSE)
carac_country1 <- merge(carac_country, carac_country_continent, by.x = "Country of the data", by.y = "Country")
(continent <- table(carac_country1$`Continent`))
(pcountry <- prop.table(continent))
(country <- table(carac_country1$`Country`))
(pcountry <- prop.table(country))
one_freq <- which(country == 1)
if (length(one_freq) > 0) {
  carac_country$`Country of the data` <- ifelse(carac_country$`Country of the data` %in% names(country)[one_freq], "Others", carac_country$`Country of the data`)
}
(country <- table(carac_country$`Country of the data`))
(pcountry <- prop.table(country))

## Publication Year
(any <- table(carac$`Publication Year`))
(pany <- prop.table(any))
levels(carac$`Publication Year`)
freq <- c(nrow(carac[carac$`Publication Year` == 2000,]),nrow(carac[carac$`Publication Year` == 2005,]),
          nrow(carac[carac$`Publication Year` == 2006,]),nrow(carac[carac$`Publication Year` == 2008,]),
          nrow(carac[carac$`Publication Year` == 2012,]),
          nrow(carac[carac$`Publication Year` == 2014,]),nrow(carac[carac$`Publication Year` == 2015,]),
          nrow(carac[carac$`Publication Year` == 2016,]),nrow(carac[carac$`Publication Year` == 2017,]),
          nrow(carac[carac$`Publication Year` == 2018,]),nrow(carac[carac$`Publication Year` == 2019,]),
          nrow(carac[carac$`Publication Year` == 2021,]),nrow(carac[carac$`Publication Year` == 2022,]),
          nrow(carac[carac$`Publication Year` == 2023,]))
data <- data.frame(any)
data$Var1 <- as.character(data$Var1)
all_years <- seq(2000, 2023)
existing_years <- unique(data$Var1)
missing_data <- data.frame(Var1 = all_years[!all_years %in% existing_years], Freq = 0)
data <- rbind(data, missing_data)
data <- data[order(data$Var1), ]
(any <- table(carac$`Publication Year`))
(pany <- prop.table(any))

## Type of Design
(tstudy <- table(carac$`Type of design`))
(ptstudy <- prop.table(tstudy))

## Type of Journal
carac$`Type of Journal` <- ifelse(carac$`Type of Journal` %in% c("Mathematics", "Sports Psychology"), "Others", carac$`Type of Journal`)
(tjournal <- table(carac$`Type of Journal`))
(ptjournal <- prop.table(tjournal))


# General characteristics of the sport
## Sport Type
(esport <- table(carac$Sport))
carac_sport <- carac %>%
  separate_rows(Sport, sep = ", ")
one_freq <- which(esport == 1)
if (length(one_freq) > 0) {
  carac_sport$`Sport` <- ifelse(carac_sport$`Sport` %in% names(esport)[one_freq], "Others", carac_sport$`Sport`)
}
(esport <- table(carac_sport$`Sport`))
(pesport <- prop.table(esport))

## Gender
(sexe <- table(carac$Gender))
(psexe <- prop.table(sexe))

## Category Participant
(cpar <- table(carac$`Category participants`))
(pcpar <- prop.table(cpar))

## Category Classification
(cclas <- table(carac$`Category classification`))
(pcclas <- prop.table(cclas))


# General characteristics of the ordinal model
## Type of Model
(tipus <- table(carac$`Type of model`))
carac[carac$`Type of model`== "Ordinal Logistic Mixed-Effects Model", "Type of model"]<- "Mixed Effects Proportional Odds Model"
(ptipus <- prop.table(tipus))

## Model Proposed*
(new<-table(carac$`Proposes a model or method?`))
(pnew <- prop.table(new))
typenew <- carac[, c("Proposes a model or method?", "Type of model", "Author (citation APA)")]
typenew$Model_Type <- ifelse(carac$`Proposes a model or method?` == "Yes", "Proposed Model", carac$`Type of model`)
(newtable <- table(typenew$Model_Type))
(prop.table(newtable))

## Validation
(valid <- table(carac$Validation))
carac_valid <- carac %>%
  separate_rows(`Validation`, sep = ", ")
(valid <- table(carac_valid$Validation))
one_freq <- which(valid == 1)
if (length(one_freq) > 0) {
  carac_valid$`Validation` <- ifelse(carac_valid$`Validation` %in% names(valid)[one_freq], "Others", carac_valid$`Validation`)
}
carac_valid[carac_valid$Validation == "Brant Test", "Validation"] <- "Test to determine the assumption of proportionality"
carac_valid[carac_valid$Validation == "Test for Parallellines", "Validation"] <- "Test to determine the assumption of proportionality"
(valid <- table(carac_valid$Validation))
(pvalid <- prop.table(valid))

## Type of Paradigm
(parad <- table(carac$`Type of paradigm`))
(pparad <- prop.table(parad))

## Type of Objective of the model
(obj <- table(carac$`Type of objective of model`))
(pobj <- prop.table(obj))

## Software used
(soft <- table(carac$`Statistical software`))
carac_soft <- carac %>%
  separate_rows(`Statistical software`, sep = ", ")
(soft <- table(carac_soft$`Statistical software`))
(psoft <- prop.table(soft))

## Package used
(pack <- table(carac$`Statistical package`))
carac_pack <- carac_soft %>%
  separate_rows(`Statistical package`, sep = ", ")
(table(carac_pack$`Statistical package`))
carac$reported_package <- ifelse(carac$`Statistical package` == "Not Reported", "Not Reported", "Reported")
(pack <- table(carac$reported_package))
(ppack <- prop.table(pack))

## Data, code and repository shared
(dat <- table(carac$`Data shared`))
(pdat <- prop.table(dat))
(cod <- table(carac$`Code shared`))
(pcod <- prop.table(cod))
(datcod <- table(carac$`Repository shared`))
(pdatcod <- prop.table(datcod))

############################################################### GRAPHICS ###################################################################################

## PUBLICATION YEAR
data$Var1 <- as.numeric(data$Var1)
windows()
ggplot(data, aes(x=Var1, y=Freq)) +
  geom_line(color = "#0072B2", size = 1.5) +
  geom_smooth(color = "#E69F00", method = 'loess', span = 1, formula = 'y ~ x', se=FALSE) +
  scale_x_continuous(breaks = seq(min(data$Var1), max(data$Var1), 1), expand = c(0, 0)) +
  scale_y_continuous(limits = c(0, 8), breaks = seq(0, 8, 1), expand = c(0, 0)) +
  labs(x = 'Year', y = 'Frequency', title = '') +
  theme_minimal() +
  theme(text = element_text(family = "Calibri", size = 14),
        plot.title = element_text(hjust = 0.5, size = 14, face = "bold", color = "black"),
        axis.title.x = element_text(vjust = -0.5, size = 14),
        axis.title.y = element_text(vjust = 1, size = 14),
        axis.text = element_text(size = 15),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 14))

## SPORT TYPE
carac_sport<- carac_sport[, c("Sport")]
dfsport<- (sportype <- table(carac_sport$Sport))
dfsport<- data.frame(dfsport)
dfsport <- dfsport[order(dfsport$Freq, decreasing = TRUE), ]
sporttype <- data.frame(stype= c("Others","Shooting","Cricket","Volleyball","Rugby","Multidisciplinary","Basketball","Football"), 
                        freq= c(6,2,2,3,3,3,6,10),
                        pct = c(17.1,5.7,5.7,8.6,8.6,8.6,17.1,28.6))
sporttype$stype <- factor(sporttype$stype, levels = sporttype$stype)
windows()
ggplot(sporttype, aes(x = pct, y = stype)) +
  geom_bar(stat = "identity", aes(fill = ifelse(stype == "Others", "lightblue", "lightblue"))) +
  geom_text(aes(label = paste0("N = ", freq)), 
            position = position_stack(vjust = 0.5), 
            size = 4, 
            color = "white") +
  labs(x = "%", y = "Sport type") +
  theme_bw() +
  scale_fill_identity() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        legend.position = "none")


## MODEL TYPE
typenew1<- typenew[, c("Model_Type")]
dfmodeltype<-data.frame(typenew1)
dfmodeltype <- (table(dfmodeltype$typenew1))
dfmodeltype <- data.frame(dfmodeltype)  
modeltype <- data.frame(mtype=c("Not Reported","GPCM","Mixed Effects POM","Proposed Model","POM"),
                        freq= c( 10,1,4,8,11),
                        pct= c(29.4,2.9,11.8,23.5,32.3))
modeltype$mtype <- factor(modeltype$mtype, levels=modeltype$mtype)
windows()
ggplot(modeltype, aes(x = pct, y = mtype)) +
  geom_bar(stat = "identity", aes(fill = ifelse(mtype == "Not Reported","#0072B2", "lightblue"))) +
  geom_text(aes(label = paste0("N = ", freq)), 
            position = position_stack(vjust = 0.5), 
            size = 4, 
            color = "white") +
  labs(x = "%", y = "Model type") +
  theme_bw() +
  scale_fill_identity() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        legend.position = "none")

## Model type per Validation used
merged_data <- merge(typenew, carac_valid, by = "Author (citation APA)", all = TRUE)
modxval <- merged_data[, c("Model_Type", "Validation")]
windows()
ggplot(merged_data, aes(x = reorder(Model_Type, -table(Model_Type)[Model_Type]), fill = Validation)) +
  geom_bar() +
  xlab("Model Type") +
  ylab("Number of Articles") +
  ggtitle("") +
  scale_fill_brewer(palette = "Spectral") +
  theme(plot.title = element_text(hjust = 0.5, size=14),
        panel.background = element_rect(fill = "white"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        legend.position = "bottom") +
  theme_bw()

################################################## CROSS TABLE ###################################################################################

carac[,c("Type of model","Validation")]
filtered_carac1 <- carac[carac$`Type of model` != "Not Reported" & carac$`Statistical software` != "Not Reported", c("Type of model", "Statistical software")]
filtered_carac1
filtered_carac2 <- carac[carac$`Type of model` == "Not Reported" & carac$`Statistical software` == "Not Reported", c("Type of model", "Statistical software")]
filtered_carac2
filtered_carac3 <- carac[carac$`Type of model` != "Not Reported" & carac$`Statistical software` == "Not Reported", c("Type of model", "Statistical software")]
filtered_carac3
filtered_carac4 <- carac[carac$`Type of model` == "Not Reported" & carac$`Statistical software` != "Not Reported", c("Type of model", "Statistical software")]
filtered_carac4

############################################### POM VALIDATION ##################################################################################

filtered_carac1 <- carac[carac$`Type of model` == "Proportional Odds Model" | carac$`Type of model`=="Mixed Effects Proportional Odds Model", c("Type of model", "Validation_extended")]
filtered_carac1
pomvalid <- data.frame(modtype= c(rep("POM",11),rep("Mixed Effects POM",4)), 
                        freq= c(rep("Not Reported",4), rep("Reported", 7), rep("Not Reported", 4)),
                        n = c(rep(4 ,1), rep("Na", 3), rep(7, 1), rep("NA", 6), rep(4,1), rep("NA",3)))
pomvalid
pomvalid$n <- as.numeric(as.character(pomvalid$n))


windows()
reported_color <- "#0072B2"
not_reported_color <- "lightblue"
ggplot(pomvalid, aes(x = modtype, fill = freq)) +
  geom_bar() +
  geom_text(aes(label = paste0("N = ", n), y = n), 
            position = position_stack(vjust = 0.5), 
            size = 6, 
            color = "white") +
  labs(x = "Type of Model",
       y = "Frequency") +
  theme_minimal() +
  theme(legend.title = element_blank(),
        axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 15),
        legend.text = element_text(size = 13),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank()) +
  scale_fill_manual(values = c(reported_color, not_reported_color)) +
  scale_y_continuous(breaks = seq(0, 12, 1), limits = c(0, 12))





