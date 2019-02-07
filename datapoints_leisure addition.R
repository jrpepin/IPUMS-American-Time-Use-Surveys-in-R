#Social leisure
lm_socl <- lm(socl    ~ mar + exfam + hh_numownkids + kidu2 + educ + employ + raceth + age + weekend + region , data = atus, weight=wt06)
psocl   <- ggeffect(lm_socl, terms = "mar")

#Active leisure
lm_actl <- lm(actl    ~ mar + exfam + hh_numownkids + kidu2 + educ + employ + raceth + age + weekend + region , data = atus, weight=wt06)
pactl   <- ggeffect(lm_actl, terms = "mar")

#Passive leisure
lm_pass <- lm(pass    ~ mar + exfam + hh_numownkids + kidu2 + educ + employ + raceth + age + weekend + region , data = atus, weight=wt06)
ppass   <- ggeffect(lm_pass, terms = "mar")

#Tele
lm_tele <- lm(tele    ~ mar + exfam + hh_numownkids + kidu2 + educ + employ + raceth + age + weekend + region , data = atus, weight=wt06)
ptele   <- ggeffect(lm_tele, terms = "mar")

# Combine predictions for leisure activities
levels(psocl$group)[levels(psocl$group)=="1"]   <- "Social"
levels(pactl$group)[levels(pactl$group)=="1"]   <- "Active"
levels(ppass$group)[levels(ppass$group)=="1"]   <- "Passive"
levels(ptele$group)[levels(ptele$group)=="1"]   <- "Television"


psocl$x <- as.factor(psocl$x)
pactl$x <- as.factor(pactl$x)
ppass$x <- as.factor(ppass$x)
ptele$x <- as.factor(ptele$x)

leis <- rbind(pleis, psocl, pactl, ppass, ptele)
levels(leis$x)[levels(leis$x)=="3"] <- "Married"
levels(leis$x)[levels(leis$x)=="1"] <- "Cohabiting"
levels(leis$x)[levels(leis$x)=="4"] <- "Single"
levels(leis$x)[levels(leis$x)=="2"] <- "Divorced/Separated"

levels(ptele$x)[levels(ptele$x)=="3"] <- "Married"
levels(ptele$x)[levels(ptele$x)=="1"] <- "Cohabiting"
levels(ptele$x)[levels(ptele$x)=="4"] <- "Single"
levels(ptele$x)[levels(ptele$x)=="2"] <- "Divorced/Separated"

levels(pleis$x)[levels(pleis$x)=="3"] <- "Married"
levels(pleis$x)[levels(pleis$x)=="1"] <- "Cohabiting"
levels(pleis$x)[levels(pleis$x)=="4"] <- "Single"
levels(pleis$x)[levels(pleis$x)=="2"] <- "Divorced/Separated"


diff$tele <- 0
diff$tele[diff$group == "Leisure"] <- (ptele$predicted - ptele$predicted[ptele$x == "Married"])
diff$tele <-  as.integer(diff$tele)

test <- diff
test$diff <- test$diff - test$tele #subtract tele time from leisure time
test <- test %>%
  gather(key, diff, -x, -predicted, -std.error, -conf.low,  -conf.high, -group)

t <- test %>%
  filter(x != "Married" & group != "Television") %>%
  ggplot(aes(x, diff, fill = key, label = round(diff, 0))) +
  geom_col() +
  facet_grid(~group) +
  ggtitle("Married Mothers Report More Housework and Less Leisure and Sleep Than Other Mothers") +
  labs(x = NULL, y = "Minutes per day", subtitle = "Predicted minutes per day with model controls",
       caption = "Source: American Time Use Surveys (2003 - 2017) \n Models control for extra adults, number of household kids, kids under 2, \n education, employment, race-ethnicity, age, weekend diary day, and region") +
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
  scale_fill_manual(values=c("#5D478B", "#D1AF00")) +
  geom_text(aes(x=1,y=15))
t


#Leisure sub-categories
leis$x <- ordered(leis$x, levels = c("Married", "Cohabiting", "Single", "Divorced/Separated"))

leis %>%
  filter(group != "Leisure" & group != "Television") %>%
  ggplot(aes(x, predicted, fill = x)) +
  geom_col() +
  facet_wrap(~group) +
  ggtitle("Married Mothers Report Less Passive Leisure Than Other Mothers") +
  labs(x = NULL, y = "Minutes per day", subtitle = "Predicted minutes per day with model controls",
       caption = "Source: American Time Use Surveys (2003 - 2017) \n Models control for extra adults, number of household kids, kids under 2, \n education, employment, race-ethnicity, age, weekend diary day, and region") +
  theme(plot.subtitle = element_text(size = 11, vjust = 1),
        plot.caption  = element_text(vjust = 1), 
        legend.position="none",
        strip.text.x  = element_text(size = 16),
        axis.title    = element_text(size = 14), 
        axis.text     = element_text(size = 12), 
        plot.title    = element_text(size = 16, face = "bold"),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank()) +
  scale_fill_manual(values=c("#5D478B", "#CD3278", "#116A66", "#CD661D")) +
  geom_errorbar(aes(ymin=conf.low, ymax=conf.high), width=.2,
                position=position_dodge(.9), color="grey")


# Leisure and Television
ptele$x <- ordered(ptele$x, levels = c("Married", "Cohabiting", "Single", "Divorced/Separated"))
pleis$x <- ordered(pleis$x, levels = c("Married", "Cohabiting", "Single", "Divorced/Separated"))

gg <- ggplot()
gg <- gg + geom_col(data = ptele, aes(x, predicted))
gg <- gg + geom_point(data = pleis,   aes(x, predicted, fill = x), size = 3)
gg <- gg + scale_fill_manual(values=c("#e1e1e1", "#e1e1e1", "#e1e1e1", "#e1e1e1"))
gg <- gg +
  ggtitle("Married Mothers Report Less Total Leisure Than Other Mothers \n but mostly less Television Time") +
  labs(x = NULL, y = "Minutes per day", subtitle = "Predicted minutes per day with model controls",
       caption = "Source: American Time Use Surveys (2003 - 2017) \n Models control for extra adults, number of household kids, kids under 2, \n education, employment, race-ethnicity, age, weekend diary day, and region") +
  theme(plot.subtitle = element_text(size = 11, vjust = 1),
        plot.caption  = element_text(vjust = 1), 
        legend.position="none",
        strip.text.x  = element_text(size = 16),
        axis.title    = element_text(size = 14), 
        axis.text     = element_text(size = 12), 
        plot.title    = element_text(size = 16, face = "bold"),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank())
gg


### Predicted minutes by activity
# Housework
ggplot(phswrk, aes(x=factor(x), predicted)) + 
  geom_bar(stat = "identity") +
  scale_x_discrete(labels = get_x_labels(phswrk)) +
  labs(
    x = get_x_title(phswrk))

# Leisure
ggplot(pleis, aes(x=factor(x), predicted)) + 
  geom_bar(stat = "identity") +
  scale_x_discrete(labels = get_x_labels(pleis)) +
  labs(
    x = get_x_title(pleis))

# Childcare
ggplot(pcare, aes(x=factor(x), predicted)) + 
  geom_bar(stat = "identity") +
  scale_x_discrete(labels = get_x_labels(pcare)) +
  labs(
    x = get_x_title(pcare))

# Sleep
ggplot(psleep, aes(x=factor(x), predicted)) + 
  geom_bar(stat = "identity") +
  scale_x_discrete(labels = get_x_labels(psleep)) +
  labs(
    x = get_x_title(psleep))

## Basic descriptions
# Chidcare
atus %>% 
  group_by(mar) %>% 
  summarise(mccare = mean(ccare)) %>%
  ggplot(aes(x = mar, y = mccare)) +
  geom_col()

# Housework
atus %>% 
  group_by(mar) %>% 
  summarise(mhswrk = mean(hswrk)) %>%
  ggplot(aes(x = mar, y = mhswrk)) +
  geom_col()

# Leisure
atus %>% 
  group_by(mar) %>% 
  summarise(mleis = mean(leisure)) %>%
  ggplot(aes(x = mar, y = mleis)) +
  geom_col()

# Sleep
atus %>% 
  group_by(mar) %>% 
  summarise(msleep = mean(sleep)) %>%
  ggplot(aes(x = mar, y = msleep)) +
  geom_col()

# Work
atus %>% 
  group_by(mar) %>% 
  summarise(mwork = mean(work)) %>%
  ggplot(aes(x = mar, y = mwork)) +
  geom_col()

# Television
atus %>% 
  group_by(mar) %>% 
  summarise(mtele = mean(tele)) %>%
  ggplot(aes(x = mar, y = mtele)) +
  geom_col()

# Television alone
atus %>% 
  group_by(mar) %>% 
  summarise(mtele = mean(telesolo)) %>%
  ggplot(aes(x = mar, y = mtele)) +
  geom_col()
