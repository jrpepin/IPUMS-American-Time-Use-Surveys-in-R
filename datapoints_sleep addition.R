# Parent
atus <- atus %>%
  mutate(
    parent = case_when(
      hh_numownkids ==0 ~ "Not a parent",
      hh_numownkids >=1 ~ "Parent"
    ))
atus$parent <- as_factor(atus$parent)

## Create racesex
atus <- atus %>%
  mutate(
    sexpar = case_when(
      parent == "Not a parent" & sex == "Men"   ~ "Man",
      parent == "Not a parent" & sex == "Women" ~ "Woman",
      parent == "Parent"       & sex == "Men"   ~ "Father",
      parent == "Parent"       & sex == "Women" ~ "Mother",
      TRUE                                ~  NA_character_))
atus$sexpar <- as.factor(atus$sexpar)

# Sample selection
atus <- filter(atus, age >= 18 & age <=54) #prime working age

# Listwise deletion
atus <- select(atus, caseid, wt06, year, ccare, hswrk, leisure, sleep, socl, actl, pass, tele, telesolo, telesp, age, sex, 
               mar, exfam, kidu2, hh_numownkids, parent, sexpar, employ, educ, raceth, weekend, region, ownrent)
atus <- na.omit(atus)

# If want to look at only recent survey years, remove # below.
# atus <- filter(atus, year >= 2012)

## Linear models and margins
library(ggeffects)

#Sleep
lm_sleep <- lm(sleep  ~ sexpar + mar + exfam + hh_numownkids + kidu2 + educ + employ + raceth + age + weekend + region + ownrent, data = atus, weight=wt06)
psleep   <- ggeffect(lm_sleep, terms = "sexpar")

psleep
plot(psleep)

#Housework
lm_hswrk <- lm(hswrk  ~ mar + exfam + hh_numownkids + kidu2 + educ + employ + raceth + age + weekend + region + ownrent, data = atus, weight=wt06)
phswrk   <- ggeffect(lm_hswrk, terms = "mar")

#Leisure
lm_leis <- lm(leisure ~ mar + exfam + hh_numownkids + kidu2 + educ + employ + raceth + age + weekend + region + ownrent, data = atus, weight=wt06)
pleis   <- ggeffect(lm_leis, terms = "mar")

#Childcare
lm_care <- lm(ccare   ~ mar + exfam + hh_numownkids + kidu2 + educ + employ + raceth + age + weekend + region + ownrent, data = atus, weight=wt06)
pcare   <- ggeffect(lm_care, terms = "mar")


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
  labs(x = NULL, y = NULL, subtitle = "Predicted minutes per day with model controls",
       caption = "Source: American Time Use Surveys (2003 - 2018) \n Models control for extra adults, number of household kids, kids under 2, 
       education, employment, race-ethnicity, age, weekend diary day, home ownership and region") +
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
  scale_y_continuous(breaks = c(-30, -20, -10, 0, 10, 20, 30), labels=c("-30", "-20", "-10", "Married mothers' \n minutes per day", "10", "20", "30")) +
  scale_fill_manual(values=c("#5D478B", "#CD3278", "#116A66")) +
  geom_errorbar(aes(ymin=diff-std.error, ymax=diff+std.error), width=.2,
                position=position_dodge(.9), color="grey")