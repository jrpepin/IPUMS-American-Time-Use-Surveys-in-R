## Working directory
# setwd("C:/Users/jpepin/Dropbox/Sayer/MaritalStatus/Marital Status & Time Use --JP/Data Points")
setwd("C:/Users/Joanna/Dropbox/Sayer/MaritalStatus/Marital Status & Time Use --JP/Data Points")

## Create a data extract using ATUS-X

# Samples:          2003-2017
# Variables:
  # "RECTYPE"       "YEAR"          "CASEID"        "REGION"        "HH_SIZE"       "HH_CHILD"      "HH_NUMKIDS"    "AGEYCHILD"     
  # "HH_NUMADULTS"  "PERNUM"        "LINENO"        "DAY"           "WT06"          "AGE"           "SEX"           "RACE"         
  # "HISPAN"        "MARST"         "EDUC"          "FULLPART"      "SPOUSEPRES"    "HH_NUMOWNKIDS" "KIDUND13"      "ACTLINE"       
  # "ACTIVITY"      "DURATION"      "START"         "STOP"         
  # "telesolo"      "telesp"

  # telesolo -- constructed time use variable in ATUSX -- 120303 & 120304 activities -- ALONE
  # telesp -- constructed time use variable in ATUSX -- 120303 & 120304 activities -- spouse (??)

## Set up instructions for importing the data 
  # https://cran.r-project.org/web/packages/ipumsr/vignettes/ipums.html
  # Updated ATUS Data

# Load libraries
library(ipumsr)
library(dplyr, warn.conflicts = FALSE)

# Load ATUS Data into R
ddi <- read_ipums_ddi("atus_00026.xml")
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
library(tidyverse)

data <- data %>%
  mutate( year       = as.integer(lbl_clean(year)),
          caseid     = as.character(lbl_clean(caseid)),
          hh_size    = as.integer(lbl_clean(hh_size)),
          hh_child   = as.integer(lbl_clean(hh_child)),
          hh_numkids = as.integer(lbl_clean(hh_numkids)),
          ageychild  = as.integer(lbl_clean(ageychild)),
          hh_numadults = as.integer(lbl_clean(hh_numadults)),
          day        = as_factor(lbl_clean(day)),
          wt06       = as.integer(lbl_clean(wt06)),
          age        = as.integer(lbl_clean(age)),
          sex        = as_factor(lbl_clean(sex)),
          race       = as_factor(lbl_clean(race)),
          hispan     = as_factor(lbl_clean(hispan)),
          marst      = as_factor(lbl_clean(marst)),
          educ       = as.integer(lbl_clean(educ)),
          fullpart   = as.character(lbl_clean(fullpart)),
          spousepres = as_factor(lbl_clean(spousepres)),
          hh_numownkids = as.integer(lbl_clean(hh_numownkids)),
          kidund13   = as_factor(lbl_clean(kidund13)),
          activity   = as.character(lbl_clean(activity)),
          region     = as_factor(lbl_clean(region)),
          telesolo   = as.integer(lbl_clean(telesolo)),
          telesp     = as.integer(lbl_clean(telesp)))

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
ccare <- data$activity %in% c(
  "30101" ,	"30102" ,	"30103" ,	"30104" ,	"30105" ,	"30108" ,	"30109" ,	"30110" ,	
  "30111" ,	"30112" ,	"30199" ,	"30201" ,	"30202" ,	"30203" ,	"30204" ,	"30299" ,	
  "30301" ,	"30302" ,	"30303" ,	"30399" ,	"30186")

# Housework
hswrk <- data$activity %in% c(
  "20101" ,	"20102" ,	"20103" ,	"20104" ,	"20199")

# Leisure
leisure <- data$activity %in% c(
  "120101" , "120199" , "120201" , 	"120202" , 	"120299" , 	"120401" , 	"120402" , 	"120403" , 	
  "120404" , "120405" ,	"120499" , 	"120501" , 	"120502" , 	"120504" , 	"120599" ,	"130201" ,	
  "130202" , "130203" ,	"130204" ,	"130205" ,	"130206" , 	"130207" , 	"130208" ,	"130209" , 	
  "130210" , "130211" ,	"130212" , 	"130213" , 	"130214" ,	"130215" ,	"130216" ,	"130217" ,	
  "130218" , "130219" ,	"130220" ,	"130221" ,	"130222" ,	"130223" ,	"130224" ,	"130225" ,	
  "130226" , "130227" ,	"130228" ,	"130229" ,	"130230" ,	"130231" ,	"130232" ,	"130299" ,	
  "130302" , "130402" , "120307" ,	"120309" ,	"120310" ,	"120311" ,	"120312" ,	"120313" ,	
  "130101" , "130102" ,	"130103" ,	"130104" ,	"130105" ,	"130106" ,	"130107" ,	"130108" , 	
  "130109" , "130110" ,	"130111" ,	"130112" ,	"130113" ,	"130114" ,	"130115" ,	"130116" ,	
  "130117" , "130118" ,	"130119" ,	"130120" ,	"130121" ,	"130122" ,	"130123" ,	"130124" ,
  "130125" , "130126" ,	"130127" ,	"130128" ,	"130129" ,	"130130" ,	"130131" ,	"130132" ,
  "130133" , "130134" ,	"130135" ,	"130136" ,	"130199" ,	"130201" ,	"130301" ,	"130401" ,	
  "130499" , "120301" , "120302" ,	"120303" ,	"120304" ,	"120305" ,	"120306" ,	"120308" ,	
  "120399" , "120503" , "129999" ,	"130399" ,	"139999" ,	"160101" ,	"160102" ,	"181201" ,	
  "181202" , "181204" ,	"181283" ,	"181299" ,	"181301" ,	"181302" ,	"181399")

# Sleep
sleep <- data$activity %in% c(
  "10101" , "10102" ,  "10199")

#Leisure sub-types
socl <- data$activity %in% c(
  "120101" , "120199"  ,  "120201" ,  "120202" ,  "120299" ,  "120401" ,  "120402" ,  "120403" , 	
  "120404" , "120405"  ,	"120499" ,  "120501" ,  "120502" ,  "120504" ,  "120599" ,  "130201" ,	
  "130202" , "130203"  ,	"130204" ,  "130205" ,	"130206" ,  "130207" ,  "130208" ,  "130209" , 	
  "130210" , "130211"  ,	"130212" ,  "130213" ,  "130214" ,  "130215" ,	"130216" ,  "130217" ,	
  "130218" , "130219"	 ,	"130220" ,  "130221" ,	"130222" ,  "130223" ,	"130224" ,  "130225" ,	
  "130226" , "130227"  ,	"130228" ,  "130229" ,	"130230" ,  "130231" ,	"130232" ,  "130299" ,	
  "130302" , "130402")

actl   <- data$activity %in% c(
  "120307" ,	"120309" ,	"120310" ,	"120311" ,	"120312" ,	"120313" ,	"130101" ,	"130102" ,	
  "130103" ,	"130104" ,	"130105" ,	"130106" ,	"130107" ,	"130108" , 	"130109" ,	"130110" ,	
  "130111" ,	"130112" ,	"130113" ,	"130114" ,	"130115" ,	"130116" ,	"130117" ,	"130118" ,	
  "130119" ,	"130120" ,	"130121" ,	"130122" ,	"130123" ,	"130124" ,	"130125" ,	"130126" ,	
  "130127" ,	"130128" ,	"130129" ,	"130130" ,	"130131" ,	"130132" ,	"130133" ,	"130134" ,	
  "130135" ,	"130136" ,	"130199" ,	"130201" ,	"130301" ,	"130401" ,	"130499")

pass   <- data$activity %in% c(
  "120301" , 	"120302" ,	"120303" ,	"120304" ,	"120305" ,	"120306" ,	"120308" ,	"120399" ,	
  "120503")

# Television
tele   <- data$activity %in% c(
  "120303" , "120304")

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

# Master activty variables
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
  group_by(caseid) %>% 
  summarise_at(vars(tele, socl, actl, pass, ccare, hswrk, leisure, sleep, year), funs(max))

rec1 <- data %>% 
  group_by(caseid) %>% 
  summarise_at(vars(hh_size, hh_child, hh_numkids, ageychild, hh_numadults, region), funs(first))

rec2 <- data %>% 
  group_by(caseid) %>% 
  summarise_at(vars(age, sex, race, hispan, marst, educ, fullpart, spousepres,  
                    hh_numownkids, kidund13, day, wt06, telesolo, telesp), funs(nth(., 2)))

atus <- reduce(list(max, rec1, rec2), 
                  left_join, by = "caseid")

head(atus, n = 5)
# remove unnecessary databases
remove(view)
remove(max)
remove(rec1)
remove(rec2)

## Clean variables

# Gender
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
        spousepres == "No spouse or unmarried partner present"                                 ~ "Divorced/Separated", 
      TRUE                                                                                   ~  NA_character_ 
    ))
atus$mar <- as_factor(atus$mar, levels = c("Married", "Cohabiting", "Single", "Divorced/Separated", ordered = TRUE))

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
atus$exfam <- as_factor(atus$exfam, levels = c("No extra adults", "Extra adults"))
atus$exfam <- relevel(atus$exfam, ref = "No extra adults")

#Kid under 2
atus <- atus %>%
  mutate(
    kidu2 = case_when(
      ageychild <=2 ~ "Child < 2",
      TRUE          ~ "No children < 2"
    ))
atus$kidu2 <- as_factor(atus$kidu2, levels = c("Child < 2", "No children < 2", ref = "No children < 2"))

# Number of own HH kids
summary(atus$hh_numownkids)

# Employment
atus <- atus %>%
  mutate(
    employ = case_when(
      fullpart == "1"  ~ "Full-time",
      fullpart == "2"  ~ "Part-time",
      fullpart == "99" ~ "Not employed",
      TRUE             ~  NA_character_
    ))
atus$employ <- as_factor(atus$employ, levels = c("Full-time", "Part-time", "Not employed"))


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
atus$educ <- as_factor(atus$educ, levels = c("Less than high school", "High school", "Some college", "BA or higher", ref = "High school"))

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
atus$raceth <- as_factor(atus$raceth, levels = c("White", "Black", "Hispanic", "Asian", "Other"))
atus$raceth <- relevel(atus$raceth, ref = "White")

# Weekend
atus <- atus %>%
  mutate(
    weekend = case_when(
      day == "Sunday"   | day == "Saturday"     ~ "Weekend",
      day == "Monday"   | day == "Tuesday" | day == "Wednesday" | 
        day == "Thursday" | day == "Friday"       ~ "Weekday",
      TRUE              ~  NA_character_
    ))
atus$weekend <- as_factor(atus$weekend, levels = c("Weekday", "Weekend"))
atus$weekend <- relevel(atus$weekend, ref = "Weekday")

# Region
summary(atus$region)

# Sample selection
atus <- filter(atus, sex == "Women") # women
atus <- filter(atus, hh_numownkids >=1) # mothers
atus <- filter(atus, ageychild <=13) #mothers of kids 13 or younger
atus <- filter(atus, age >= 18 & age <=54) #prime working age
atus <- filter(atus, raceth != "Asian" & raceth != "Other") # white, black, and Hispanic

# Listwise deletion
atus <- select(atus, caseid, wt06, year, ccare, hswrk, leisure, sleep, socl, actl, pass, tele, telesolo, telesp, age, sex, 
               mar, exfam, kidu2, hh_numownkids, employ, educ, raceth, weekend, region)
atus <- na.omit(atus)

## Linear models and margins
library(ggeffects)

#Housework
lm_hswrk <- lm(hswrk  ~ mar + exfam + hh_numownkids + kidu2 + educ + employ + raceth + age + weekend + region , data = atus, weight=wt06)
phswrk   <- ggeffect(lm_hswrk, terms = "mar")

#Leisure
lm_leis <- lm(leisure ~ mar + exfam + hh_numownkids + kidu2 + educ + employ + raceth + age + weekend + region , data = atus, weight=wt06)
pleis   <- ggeffect(lm_leis, terms = "mar")

#Childcare
lm_care <- lm(ccare   ~ mar + exfam + hh_numownkids + kidu2 + educ + employ + raceth + age + weekend + region , data = atus, weight=wt06)
pcare   <- ggeffect(lm_care, terms = "mar")

#Sleep
lm_sleep <- lm(sleep  ~ mar + exfam + hh_numownkids + kidu2 + educ + employ + raceth + age + weekend + region , data = atus, weight=wt06)
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
levels(pred$x)[levels(pred$x)=="2"] <- "Divorced/Separated"

pred$x     <- ordered(pred$x, levels = c("Married", "Cohabiting", "Single", "Divorced/Separated"))
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
  labs(x = NULL, y = "Minutes per day", subtitle = "Predicted minutes per day with model controls",
       caption = "Source: American Time Use Surveys (2003 - 2017) \n Models control for extra adults, number of household kids, kids under 2, \n education, employment, race-ethnicity, age, weekend diary day, and region") +
  theme_minimal() +
  theme(plot.subtitle = element_text(size = 11, vjust = 1),
        plot.caption  = element_text(vjust = 1), 
        legend.position="none",
        strip.text.x  = element_text(size = 16),
        axis.title    = element_text(size = 14), 
        axis.text     = element_text(size = 12), 
        plot.title    = element_text(size = 16, face = "bold"),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank()) +
  scale_y_continuous(breaks = c(-10, 0, 10, 20, 30), labels=c("-10", "Married mothers", "10", "20", "30")) +
  scale_fill_manual(values=c("#5D478B", "#CD3278", "#116A66")) +
  geom_errorbar(aes(ymin=diff-std.error, ymax=diff+std.error), width=.2,
                position=position_dodge(.9), color="grey")