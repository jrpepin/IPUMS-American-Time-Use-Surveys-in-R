## Working directory
# setwd("C:/Users/jpepin/Dropbox/Sayer/MaritalStatus/Marital Status & Time Use --JP/Data Points")
setwd("C:/Users/Joanna/Dropbox/Sayer/MaritalStatus/Marital Status & Time Use --JP/Data Points")
dataDir <- file.path("C:/Users/Joanna/Dropbox/Data/ATUS")
rawdata <- "atus_00049.xml"

## Create a data extract using ATUS-X

# Samples:          2003-2018
# Variables:
  # "RECTYPE"       "YEAR"          "CASEID"        "REGION"        "HH_SIZE"       "HH_CHILD"      "AGEYCHILD"     
  # "HH_NUMADULTS"  "HHTENURE"      "PERNUM"        "LINENO"        "DAY"           "WT06"          "AGE"           "SEX"                    
  # "RACE"          "HISPAN"        "MARST"         "EDUC"          "FULLPART"      "SPOUSEPRES"    "HH_NUMOWNKIDS" "KIDUND13"             
  # "ACTLINE"       "ACTIVITY"      "DURATION"       


## Set up instructions for importing the data 
  # https://cran.r-project.org/web/packages/ipumsr/vignettes/ipums.html
  # Updated ATUS Data

# Load libraries
library(ipumsr)
library(tidyverse, warn.conflicts = FALSE)
library(labelled) # convert labeled data to factors
library(ggeffects) # Linear models and marginals


# Load ATUS Data into R
ddi <- read_ipums_ddi(file.path(dataDir, rawdata))
data <- read_ipums_micro(ddi)

# Make sure data is now a dataframe
class(data)
# Check-out the variable names
names(data)
# Make them lowercase
data <- data %>% rename_all(tolower)

#Look at structure of data
summary(data)

## Clean the data
  # Change class from labelled

fcols <- c("hhtenure", "day", "sex", "race", "hispan", "marst", "spousepres", "spsex",
           "fullpart", "kidund13", "region")

icols <- c("year", "hh_size", "hh_child", "ageychild", "hh_numownkids", 
          "age",  "educ", "wt06")

ccols <- c("caseid", "activity")

data[fcols] <- lapply(data[fcols], to_factor)
data[icols] <- lapply(data[icols], as.integer)
data[ccols] <- lapply(data[ccols], as.character)


## Change NA to 0 for duration minutes
data[["duration"]][is.na(data[["duration"]])] <- 0
summary(data$duration)

data[["activity"]][is.na(data[["activity"]])] <- "0"
summary(data$activity)


## Check that duration = 1440
data %>%
  group_by(caseid) %>%
  summarise(total= sum(duration))

# Activity summary variables by person

# Childcare
ccare <- data$activity %in% 
  c(030100:030400, 080100:080200, 180381)

# Housework
hswrk <- data$activity %in% 
  c(020101:030000, 080200:080300, 080700:080800, 090100:100000, 160106, 070101, 180701, 180904, 180807, 180903, 080699, 160106)

# Leisure
leisure <- data$activity %in% 
  c(120100:130000, 130100:140000, 160101, 160102, 181200:181400)

# Sleep
sleep <- data$activity %in% 
  c(010100:020000)

#Leisure sub-types
socl <- data$activity %in% 
  c(120100:120200, 120200:120300, 120400:120500, 120501, 120502, 120504, 120599, 130200:130300, 130302, 130402)

actl   <- data$activity %in% 
  c(120307, 120309:120313, 130100:130200, 130301, 130401, 130499)

pass   <- data$activity %in% 
  c(120301:120306, 120308, 120399, 120503)

# Television
tele   <- data$activity %in% 
  c(120303, 120304)

data$actcat<-NA
data$actcat[ccare]   <- "child care"
data$actcat[hswrk]   <- "housework"
data$actcat[leisure] <- "leisure"
data$actcat[sleep]   <- "sleep"
data$actcat <- as.character(data$actcat)

data$leiscat<-NA
data$leiscat[socl] <- "social leisure"
data$leiscat[actl] <- "active leisure"
data$leiscat[pass] <- "passive leisure"
data$leiscat <- as.character(data$leiscat)

data$telecat<-NA
data$telecat[tele] <- "television"
data$telecat <- as.character(data$telecat)

# Master activity variables
data <- data %>%
  group_by(caseid) %>%
  summarise (leisure  = sum(duration[actcat ==  "leisure"],    na.rm=TRUE),
             sleep    = sum(duration[actcat ==  "sleep"],      na.rm=TRUE),
             hswrk    = sum(duration[actcat ==  "housework"],  na.rm=TRUE),
             ccare    = sum(duration[actcat ==  "child care"], na.rm=TRUE)) %>%
  inner_join(data, by='caseid')

# Leisure activity variables
data <- data %>%
  group_by(caseid) %>%
  summarise (socl     = sum(duration[leiscat ==  "social leisure"],      na.rm=TRUE),
             actl     = sum(duration[leiscat ==  "active leisure"],      na.rm=TRUE),
             pass     = sum(duration[leiscat ==  "passive leisure"],     na.rm=TRUE)) %>%
  inner_join(data, by='caseid')

# Television variables
data <- data %>%
  group_by(caseid) %>%
  summarise (tele     = sum(duration[telecat ==  "television"],          na.rm=TRUE)) %>%
  inner_join(data, by='caseid')

# Create person level data -- need to add formatted variables above, then add to rec1 and rec2 -- add activity variables to max
max <- data %>%
  select(caseid, tele, socl, actl, pass, ccare, hswrk, leisure, sleep, year) %>%
  group_by(caseid) %>% 
  summarise_all(list(~max(.)))

rec1 <- data %>% 
  filter(rectype == 1) %>%
  select(caseid,  hh_size, hh_child, ageychild, hh_numadults, hhtenure, region)

rec2 <- data %>% 
  filter(rectype == 2) %>% 
  select(caseid, age, sex, race, hispan, marst, educ, fullpart, spousepres, spsex,  
         hh_numownkids, kidund13, day, wt06)

atus <- reduce(list(max, rec1, rec2), 
                  left_join, by = "caseid")

head(atus, n = 5)
# remove unnecessary databases
remove(max)
remove(rec1)
remove(rec2)

## Clean variables

# Gender
atus$sex <-atus$sex %>%
  droplevels()
levels(atus$sex) <- c('Men', 'Women')

# Age
summary(atus$age)

# Marital status
# cohabitors in one group regardless of marital history
# use spousepres variable as indicator of marital status at time of ATUS diary
atus <- atus %>%
  mutate(
    mar = case_when(
      spousepres == "Spouse present"                                                         ~ "Married",
      spousepres == "Unmarried partner present"                                              ~ "Cohabiting",
      marst      == "Never married" & spousepres == "No spouse or unmarried partner present" ~ "Single",
      marst      != "Widowed" & marst != "Never married" & 
      spousepres == "No spouse or unmarried partner present"                                 ~ "Divorced\nSeparated", 
      TRUE                                                                                   ~  NA_character_ 
    ))
atus$mar <- factor(atus$mar, levels = c("Married", "Cohabiting", "Single", "Divorced\nSeparated"), ordered = FALSE)

# Spouse/partner sex
atus <- atus %>%
  mutate(
    spsex = case_when(
      spsex == "Male"                     ~ "Male",
      spsex == "Female"                   ~ "Female",
      spsex == "NIU (Not in universe)"    ~ "NIU",
      TRUE                                ~  NA_character_ 
    ))

atus$spsex <- factor(atus$spsex, levels = c("Male", "Female", "NIU"))

# Extended Family Member
atus <- atus %>%
  mutate(
    exfam = case_when(
      ((spousepres == "Spouse present" | spousepres == "Unmarried partner present") & hh_numadults <= 2) |
      ((spousepres == "No spouse or unmarried partner present") & hh_numadults <=1) ~ "No extra adults",
      ((spousepres == "Spouse present" | spousepres == "Unmarried partner present") & hh_numadults >= 3) |
      ((spousepres == "No spouse or unmarried partner present") & hh_numadults >=2) ~ "Extra adults",
      TRUE                        ~  NA_character_
    ))
atus$exfam <- factor(atus$exfam, levels = c("No extra adults", "Extra adults"), ordered = FALSE)

#Kid under 2
atus <- atus %>%
  mutate(
    kidu2 = case_when(
      ageychild <=2 ~ "Child < 2",
      TRUE          ~ "No children < 2"
    ))
atus$kidu2 <- as_factor(atus$kidu2)

# Number of own HH kids
summary(atus$hh_numownkids)

# Employment
atus <- atus %>%
  mutate(
    employ = case_when(
      fullpart == "Full time"  ~ "Full-time",
      fullpart == "Part time"  ~ "Part-time",
      fullpart == "NIU (Not in universe)" ~ "Not employed",
      TRUE             ~  NA_character_
    ))
atus$employ <- factor(atus$employ, levels = c("Full-time", "Part-time", "Not employed"))

# Education
atus <- atus %>%
  mutate(
    educ = case_when(
      (educ >= 10 & educ <= 17)   ~ "Less than high school",
      (educ >= 20 & educ <= 21)   ~ "High school",
      (educ >= 30 & educ <= 31)   ~ "Some college",
      (educ >= 40 & educ <= 43)   ~ "BA or higher",
      TRUE                        ~  NA_character_
    ))
atus$educ <- factor(atus$educ, levels = c("Less than high school", "High school", "Some college", "BA or higher"))

# Race
atus <- atus %>%
  mutate(
    raceth = case_when(
      race   == "White only" & hispan == "Not Hispanic"  ~ "White",
      race   == "Black only" & hispan == "Not Hispanic"  ~ "Black",
      race   == "Asian only" & hispan == "Not Hispanic"  ~ "Asian",
      hispan != "Not Hispanic"                           ~ "Hispanic",
      TRUE                                               ~ "Other"
    ))
atus$raceth <- factor(atus$raceth, levels = c("White", "Black", "Hispanic", "Asian", "Other"))

# Weekend
atus <- atus %>%
  mutate(
    weekend = case_when(
      day == "Sunday"   | day == "Saturday"     ~ "Weekend",
      day == "Monday"   | day == "Tuesday" | day == "Wednesday" | 
        day == "Thursday" | day == "Friday"       ~ "Weekday",
      TRUE              ~  NA_character_
    ))
atus$weekend <- factor(atus$weekend, levels = c("Weekday", "Weekend"))

# Region
summary(atus$region)

# Home ownership
atus <- atus %>%
  mutate(
    ownrent = case_when(
      hhtenure == "Owned or being bought by a household member"     ~ "Own",
      hhtenure == "Rented for cash"                                 ~ "Rent",
      hhtenure == "Occupied without payment of cash rent"           ~ "Other",     
      hhtenure == "NIU (Not in universe)"                           ~ "Other",
      TRUE                                                          ~  NA_character_ 
    ))

atus$ownrent <- factor(atus$ownrent, levels = c("Own", "Rent", "Other"))

# Sample selection
atus <- filter(atus, sex == "Women") # women
atus <- filter(atus, spsex == "Male" | spsex == "NIU") # Different sex couples or singles
atus <- filter(atus, hh_numownkids >=1) # mothers
atus <- filter(atus, ageychild <=13) #mothers of kids 13 or younger
atus <- filter(atus, age >= 18 & age <=54) #prime working age
atus <- filter(atus, raceth != "Asian" & raceth != "Other") # white, black, and Hispanic

# Listwise deletion
atus <- select(atus, caseid, wt06, year, ccare, hswrk, leisure, sleep, socl, actl, pass, tele, age, sex, 
               mar, exfam, kidu2, hh_numownkids, employ, educ, raceth, weekend, region, ownrent)
atus <- na.omit(atus)

# If want don't want to look at only recent survey years, add # below.
atus <- filter(atus, year >= 2015)

## Linear models and margins

#Housework
lm_hswrk <- lm(hswrk  ~ mar + exfam + hh_numownkids + kidu2 + educ + employ + raceth + age + weekend + region + ownrent, data = atus, weight=wt06)
phswrk   <- ggeffect(lm_hswrk, terms = "mar")

#Leisure
lm_leis <- lm(leisure ~ mar + exfam + hh_numownkids + kidu2 + educ + employ + raceth + age + weekend + region + ownrent, data = atus, weight=wt06)
pleis   <- ggeffect(lm_leis, terms = "mar")

#Childcare
lm_care <- lm(ccare   ~ mar + exfam + hh_numownkids + kidu2 + educ + employ + raceth + age + weekend + region + ownrent, data = atus, weight=wt06)
pcare   <- ggeffect(lm_care, terms = "mar")

#Sleep
lm_sleep <- lm(sleep  ~ mar + exfam + hh_numownkids + kidu2 + educ + employ + raceth + age + weekend + region + ownrent, data = atus, weight=wt06)
psleep   <- ggeffect(lm_sleep, terms = "mar")

# Combine predictions for core activities
levels(psleep$group)[levels(psleep$group)=="1"] <- "Sleep"
levels(pcare$group)[levels(pcare$group)=="1"]   <- "Childcare"
levels(pleis$group)[levels(pleis$group)=="1"]   <- "Leisure"
levels(phswrk$group)[levels(phswrk$group)=="1"] <- "Housework"

psleep$x <- as.factor(psleep$x)
pcare$x <- as.factor(pcare$x)
pleis$x <- as.factor(pleis$x)
phswrk$x <- as.factor(phswrk$x)

pred <- rbind(psleep, pcare, pleis, phswrk)
levels(pred$x)[levels(pred$x)=="3"] <- "Married"
levels(pred$x)[levels(pred$x)=="1"] <- "Cohabiting"
levels(pred$x)[levels(pred$x)=="4"] <- "Single"
levels(pred$x)[levels(pred$x)=="2"] <- "Divorced\nSeparated"

pred$x     <- ordered(pred$x, levels = c("Married", "Cohabiting", "Single", "Divorced\nSeparated"))
pred$group <- ordered(pred$group, levels = c("Childcare", "Housework", "Leisure", "Sleep"))

# Calculated difference minutes
pred <- pred %>%
  group_by(group) %>%
  mutate(diff = predicted - predicted[x == "Married"])

## Data visualization
pred %>%
  filter(x != "Married") %>%
  ggplot(aes(x, diff, fill = x, label = round(diff, 0))) +
  geom_col() +
  facet_grid(~group) +
  ggtitle("Married Mothers Report More Housework and Less Leisure and Sleep Than Other Mothers") +
  labs(x = NULL, y = NULL, subtitle = "Predicted minutes per day with model controls",
       caption = "Source: American Time Use Surveys (2003 - 2019) \n Models control for extra adults, number of household kids, kids under 2, 
       education, employment, race-ethnicity, age, weekend diary day, home ownership and region") +
  theme_minimal() +
  theme(plot.subtitle = element_text(vjust = 1),
        plot.caption  = element_text(vjust = 1), 
        legend.position="none",
        strip.text.x  = element_text(face = "bold"),
        plot.title    = element_text(face = "bold"),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank()) +
  scale_y_continuous(breaks = c(-30, -20, -10, 0, 10, 20, 30), labels=c("-30", "-20", "-10", "Married mothers' \n minutes per day", "10", "20", "30")) +
  scale_fill_manual(values=c("#18BC9C", "#F39C12", "#E74C3C")) +
  geom_errorbar(aes(ymin=diff-std.error, ymax=diff+std.error), width=.2,
                position=position_dodge(.9), color="grey")