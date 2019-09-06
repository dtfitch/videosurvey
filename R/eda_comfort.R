# Exploratory analysis of bike comfort data
# Jane Carlen, May 2019

# Notes
#   Rating distributions are more bimodal than normal (bumps for high and low), and certain qualities (e.g. don't like biking) seem to flatten out the distribution rather than just moving the mean
#   Only three have so much spread that their intercept is not statistically significiant 
#     see lm.video_name -- note one that's not stat. sig. is the reference level -> not shown in summary

# Questions:
#   What explains the tails in the ratings, i.e. people who deviate from the general trend?
#   Do people have low-rating and high-rating tendencies, and are they explained by covariates?
#----------------------------------------------------------------------------------------------
# 0. Setup ---- assumes we're in the R folder of the videosurvey repository (the one cloned in Box) ----

library(ggplot2)
library(ggridges)
library(dplyr)
library(forcats)
library(reshape2)

# Load data ---
source("data_comfort.R") 

#    plot themes ----

theme1 <- theme( axis.text.x = element_text(angle = 45, hjust = 1, family = "Times", size  = 10),
                 axis.title.x = element_text(family = "Times", size  = 10),
                 axis.text.y = element_text(family = "Times", size  = 10),
                 axis.title.y = element_text(family = "Times", size  = 10),
                 title = element_text(family = "Times New Roman", size  = 10, face = "plain"))

theme2 = theme( axis.text.x = element_text( family = "Times", size  = 14),
                axis.title.x = element_text(family = "Times", size  = 16),
                axis.text.y = element_blank(),
                axis.ticks.y = element_blank(),
                axis.title.y = element_text(family = "Times", size  = 16),
                title = element_text(family = "Times", size  = 16, face = "plain"),
                strip.text = element_text(family = "Times", size  = 16),
                legend.position = "top",
                legend.text = element_text(family = "Times", size  = 16),
                legend.title = element_blank())

theme3 = theme(axis.text.x = element_text(family = "Times", size  = 18),
               axis.title.x = element_text(family = "Times", size  = 18),
               axis.text.y = element_text(family = "Times", size  = 18),
               axis.title.y = element_text(family = "Times", size  = 18),
               legend.text = element_text(family = "Times", size  = 18),
               title = element_text(family = "Times", size  = 18, face = "plain"))

# 1. Sample characteristics (minimal) ####
plot.gender = ggplot(data = d) + theme1 +
  geom_bar(aes(x = as.factor(female))) + 
  xlab(label = "") + 
  scale_x_discrete(labels = c("Male", "Female", "NA")) +
  ggtitle("Gender (N = 15114)")

plot.primary.role = ggplot(data = d) + theme1 +
  geom_bar(aes(x = as.factor(primary_role))) +
  scale_x_discrete(labels = c("Faculty", "Grad. Student", "Staff", "Undergraduate", #undergrad incl. Post-Bac)
                              "Visiting Scholar")) +
  ggtitle("Primary Role (N = 15288)") +
  xlab("")

plot.age = ggplot(data = d) + theme1 + geom_bar(aes(x = age)) +
  ggtitle("Age (N = 14777)")

plot.usual.mode = ggplot(data = d) + theme1 + 
  geom_bar(aes(x = as.numeric(usual_mode_4lev))) +
  scale_x_continuous(breaks = 1:4, labels = levels(d$usual_mode_4lev)) +
  ggtitle("Usual Mode (N = 15283)") +
  xlab("")

ggsave("../IMG/survey_char.png", plot_grid(plot.gender, plot.age, 
                                        plot.primary.role, plot.usual.mode, 
                                        align = "h"), width = 6, height = 6)

# 2. Explore video ratings distributions ----

# There seems to be high-comfort and low-comfort streets

#   Overall ratings distribution ----

png("../IMG/ratings_overall_hists.png", width = 800)
par(mfrow = c(1,2))
#mean
hist(d.video$mean_comfort, breaks = 20, xlab = "mean rating", main = "Distribution of mean video ratings")
#median
hist(d.video$median_comfort, breaks = 20, xlab = "median rating", main = "Distribution of median video ratings")
dev.off()

plot.ratings.discrete =  ggplot(data = d) +  xlab(label = "") + theme2 + 
  geom_histogram(aes(x = comfort_rating, fill = comfort_rating), color = "black", stat = "count", show.legend = F) +
  scale_fill_brewer(direction = 1) +
  theme( axis.text.x = element_text(angle = 45, hjust = 1, family = "Times", size  = 16)) +
  scale_x_discrete(drop=FALSE, labels = levels(d$comfort_rating)) + 
  ggtitle("Distribution of all comfort rating of streets")
plot.ratings.discrete

plot.med.ratings = ggplot(data = d.video) +  xlab(label = "") + theme2 + 
  geom_histogram(aes(x = factor(median_comfort, levels = 1:7), color = I("black"), 
                     fill = factor(median_comfort, levels = 1:7)), 
                 stat = "count", show.legend = F) +
  scale_fill_brewer(direction = 1) +
  theme( axis.text.x = element_text(angle = 45, hjust = 1, family = "Times", size  = 16)) +
  scale_x_discrete(drop=FALSE, labels = levels(d$comfort_rating)) + 
  ggtitle("Distribution of median comfort rating of streets")

ggsave("../IMG/overall_ratings.png", plot_grid(plot.ratings.discrete, plot.med.ratings, align = "h", nrow = 1), width = 12)



#   By video ratings distribution ----

d.tmp =  d %>% group_by(video_name) %>% mutate(mean_cr = mean(as.numeric(comfort_rating), na.rm  = T))
                                      
plot.ratings = ggplot(d.tmp, aes(x = comfort_rating, y = fct_reorder(video_name, mean_cr), 
                                 group = video_name, fill = as.factor(female))) +
  stat_density_ridges(geom = "density_ridges_gradient", bandwidth = .5) +
  xlab("") + ylab("") + 
  theme1

ggsave("../IMG/ratings_video_dists.png", plot.ratings, width = 4, height = 8)

# Which videos have wide variation? (4th_AddisonUniversity, Tunnel_OakRidgeUplands, Ashby_ColbyRegent)
d$video_name = relevel(d$video_name, ref = "4th_AddisonUniversity")
summary(lm(as.numeric(d$comfort_rating) ~ video_name, data = d)) #R^2 .2039

#Video rating distribution by video and gender
plot.ratings.female = ggplot(d.tmp %>% filter(!is.na(female)), 
       aes(x = comfort_rating, y = fct_reorder(interaction(female, video_name), mean_cr), 
              group = interaction(female, video_name), fill = as.factor(female))) +
  stat_density_ridges(geom = "density_ridges_gradient", bandwidth = .5) +
  scale_y_discrete(labels = as.vector(rbind(levels(fct_reorder(d.video$video_name, d.video$mean_comfort)), ""))) +
  scale_fill_manual(values = rainbow(2,  alpha = .7), labels= c("male", "female"), name = "Gender") +
  xlab("") + ylab("") +
  theme( axis.text.x = element_text(angle = 45, hjust = 1, family = "Times", size  = 16),
         axis.title.x = element_text(family = "Times", size  = 16),
         axis.text.y = element_text(family = "Times", size  = 16),
         axis.title.y = element_text(family = "Times", size  = 16),
         title = element_text(family = "Times", size  = 16, face = "plain"), 
         legend.position = "top",
         legend.direction = "vertical",
         legend.title =  element_text(family = "Times", size  = 16),
         legend.text = element_text(family = "Times", size  = 12),
         plot.margin = margin(1,0,0,0, "cm"))
ggsave("../IMG/ratings_video_gender_dists.png", plot.ratings.female, width = 6, height = 12)

#Video rating distribution by video and how comfortable someone is bicycling on a four lane road with no bike lane (3 levels)

plot.ratings.4lane = ggplot(d %>% group_by(video_name) %>% 
         mutate(mean_cr = mean(as.numeric(comfort_rating), na.rm  = T)) %>%
         filter(!is.na(comfort_four_no_lane)), 
       aes(x = comfort_rating, y = fct_reorder(interaction(comfort_four_no_lane, video_name), mean_cr), 
           group = interaction(comfort_four_no_lane, video_name), fill = comfort_four_no_lane)) +
  scale_y_discrete(labels = as.vector(rbind(levels(fct_reorder(d.video$video_name, d.video$mean_comfort)), "", ""))) +
  stat_density_ridges(geom = "density_ridges_gradient", bandwidth = .5) +
  scale_fill_manual(values = rainbow(3,  alpha = .7), name = "Comfort on 4-lane road, no bike lane") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  xlab("") + ylab("") +
  theme( axis.text.x = element_text(angle = 45, hjust = 1, family = "Times", size  = 16),
         axis.title.x = element_text(family = "Times", size  = 16),
         axis.text.y = element_text(family = "Times", size  = 16),
         axis.title.y = element_text(family = "Times", size  = 16),
         title = element_text(family = "Times", size  = 16, face = "plain"),
         legend.position = "top",
         legend.direction = "vertical",
         legend.title =  element_text(family = "Times", size  = 16),
         legend.text = element_text(family = "Times", size  = 12),
         plot.margin = margin(1,0,0,0, "cm") #trbl
         )

ggsave("../IMG/ratings_video_4lane_dists.png", plot.ratings.4lane, width = 6, height = 12)

ggsave("../IMG/ratings_video_char_dists.png", plot_grid(plot.ratings.female, plot.ratings.4lane, align = "h"), width = 15, height = 20)

#Video rating distribution by video and "op_like_biking" (3 levels)
plot.ratings.lkbike = ggplot(d %>% group_by(video_name) %>% 
         mutate(mean_cr = mean(as.numeric(comfort_rating), na.rm  = T)) %>%
         filter(op_like_biking_3lev != "neutral" & !is.na(op_like_biking_3lev)), 
       aes(x = comfort_rating, y = fct_reorder(interaction(as.numeric(op_like_biking_3lev)-1, video_name), mean_cr), 
           group = interaction(as.numeric(op_like_biking_3lev), video_name), fill = as.factor(op_like_biking_3lev))) +
  stat_density_ridges(geom = "density_ridges_gradient", bandwidth = .5) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_manual(values = rainbow(2,  alpha = .7), labels= c("don't like", "like"), name = "Like biking?") +
  xlab("") + ylab("") +
  theme( axis.text.x = element_text(angle = 45, hjust = 1, family = "Times", size  = 10),
         axis.title.x = element_text(family = "Times", size  = 10),
         axis.text.y = element_text(family = "Times", size  = 10),
         axis.title.y = element_text(family = "Times", size  = 10),
         title = element_text(family = "Times", size  = 10, face = "plain"))
ggsave("../IMG/ratings_video_lkbike_dists.png", plot.ratings.lkbike, width = 7, height = 10)

#also tried: Video rating distribution by video and "usual_mode =="Bike " (3 levels)

#   Four lane comfort opinion vs. ratings ----
#  Compare distribution of ratings on four-lane road to comfort on four-lane road (as text vs. video clip) -- filter by 4 lanes and no bike lane. (SanPablo_GilmanHarrison, SanPablo_CedarVirginia)

plot.comfort = filter(d, num_lanes_ST == 4, bike_lane_ST==0) %>% 
  group_by(video_name) %>% filter(!is.na(comfort_four_no_lane)) %>%
  ggplot(aes(fill = comfort_rating, x = comfort_four_no_lane)) + 
  geom_bar(position = "dodge", color = "black", size = .3) +
  xlab("Comfort bicycling on a four lane road with no bike lane") + theme1 +
  scale_fill_brewer(direction = -1, name = "") +
  theme2 + 
  theme(axis.text.x = element_text(angle = 0, hjust = .5),
        legend.position = "top")
  
ggsave("../IMG/plot_comfort.png", plot.comfort, width = 12)


# 3. Explore independent variables  ----
# Plot video score against all variables

## Probably important varaibles based on next plots ##

# Composite scores (NCHRP_BLOST_ST, HCM_BLOS_ST, LTS_ST), as expected
# bike lane and it's width/seperation as expressed by buffered / parking lane width / bikeway width / shoulder width / outside land width
# Speed limit and prevailing speed, possibly
# Vehicle volume and pavement condition
# Whether road is divided and urban (these are less intuitive, may be conflation)

d.video.melt = d %>% select(c("comfort_rating", "video_name", contains("_ST", ignore.case = FALSE))) %>%
  mutate_if(is.factor, list(as.numeric)) %>%
  mutate(comfort_rating = d$comfort_rating) %>% #actually don't want this one numeric
  select(-c("NCHRP_BLOS_score_ST", "veh_volume_ST", "speed_limit_mph_ST_3lev")) %>% #remove near duplicats
  melt(id = c("video_name", "comfort_rating")) %>%
  group_by(value, variable, comfort_rating) %>%
  summarize(count = n())

plot.allvar = ggplot(d.video.melt, aes(x = reorder(as.factor(value), sort(value)), y = count, fill = as.factor(comfort_rating))) +
  scale_fill_brewer(palette = "RdYlGn") + 
  geom_bar(stat = "identity", position = "dodge", color = "black", lwd = .1) +
  facet_wrap(~variable, scales = "free", ncol = 4) +
  theme_bw() + 
  xlab("") +
  theme2

ggsave("../IMG/ratings_by_variable.png", plot.allvar, width = 12, height = 18)

# Condensed ratings to negative, positive
d.video.melt3 = d.video.melt %>% mutate(comfort_rating_3lev = as.factor(sapply(as.numeric(comfort_rating), condense_ratings, levels = 7))) %>%
  filter(comfort_rating_3lev != "neutral" & !is.na(comfort_rating_3lev)) %>%
  group_by(comfort_rating_3lev, variable, value) %>% summarize(count = sum(count))

plot.allvar.binary = ggplot(d.video.melt3 %>% group_by(variable), aes(x = reorder(as.factor(value), 
                        rank(value)), y = count, fill = as.factor(comfort_rating_3lev))) +
  scale_fill_manual(values = rainbow(3, alpha = .7)) + 
  geom_bar(stat = "identity", position = "dodge", color = "black", lwd = .2) +
  facet_wrap(~variable, scales = "free", ncol = 4) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme_bw() + xlab("") +
  theme2

ggsave("../IMG/ratings_binary_by_variable.png",  plot.allvar.binary, width = 12, height = 18)


#    Explore type of effects (additive vs. multiplicative) ----

# Does liking biking imply a shift, or multiplicative? Generally a shift
d %>% group_by(video_name, op_like_biking_3lev) %>%
  summarize(m = mean(as.numeric(comfort_rating), na.rm = T)) %>%
  filter(op_like_biking_3lev!="neutral") %>%
  ggplot() + 
  geom_path(aes(x = fct_reorder(video_name, m), y = m, color = op_like_biking_3lev, group = video_name))  +
  geom_point(aes(x = fct_reorder(video_name, m), y = m, color = op_like_biking_3lev), size = 5) +
  scale_color_manual(values = c("red", "green")) + coord_flip()

# Does gender imply a shift, or multiplicative?
d %>% group_by(video_name, female) %>% summarize(m = mean(as.numeric(comfort_rating), na.rm = T)) %>%
  ggplot() + 
  geom_path(aes(x = fct_reorder(video_name, m), y = m, color = as.factor(female), group = video_name))  +
  geom_point(aes(x = fct_reorder(video_name, m), y = m, color = as.factor(female)), size = 5) +
  scale_color_manual(values = c("red", "green")) + coord_flip()

# Does bike line have an interaction effect with liking biking? (Seems not)
d %>% group_by(bike_lane_ST, op_like_biking_3lev) %>% 
  summarize(m = mean(as.numeric(comfort_rating), na.rm = T)) %>%
  filter(op_like_biking_3lev!="neutral") %>%
  ggplot() + 
  geom_path(aes(x = bike_lane_ST, y = m, color = as.factor(op_like_biking_3lev), group = bike_lane_ST))  +
  geom_point(aes(x = bike_lane_ST, y = m, color = as.factor(op_like_biking_3lev)), size = 5) +
  scale_color_manual(values = c("red", "green")) + coord_flip()


# 4. Comparing ratings to "scores" ----

d.scores = melt(d %>% select(matches("NCHRP|HCM|LTS|_rating|video_name")) %>% mutate_if(is.ordered, as.numeric) %>%
                  select(-c("comfort_rating_3lev", "NCHRP_BLOS_score_ST")) %>%
                  mutate(NCHRP_BLOS_ST = 6 - NCHRP_BLOS_ST,
                         HCM_BLOS_ST = 6 - HCM_BLOS_ST),
                id = c("video_name", "comfort_rating")) %>%
  mutate(value = recode(value, "1" = "A", "2" = "B", "3" = "C", "4" = "D", "5" = "E")) %>% 
  group_by(interaction(value, variable))

# line plot
plot.score = ggplot(d.scores %>% summarize(x = as.factor(first(value)), Score = first(variable), m = mean(comfort_rating))) + 
  geom_point(aes(y = m, x = fct_rev(x), group = Score, color = Score)) +
  geom_line(aes(y = m, x = fct_rev(x), group = Score, color = Score)) +
  theme_bw() +
  ylab("Average comfort rating") +
  xlab("") + 
  scale_y_continuous(breaks = 1:7, limits = c(1,7)) +
  theme3 +
  ggtitle("Average Rating from Survey versus External Score of Streets")

ggsave(filename = "../IMG/ratings_vs_scores.png", plot.score, height = 5)

# distributions with mean line ch
ggplot(d.scores %>% mutate(m = mean(comfort_rating))) + 
  geom_density(aes(x = comfort_rating, fill = variable, color = variable), bw = .6) + 
  facet_wrap(~interaction(value, variable), scales = "free_y", ncol = 1)  +
  geom_vline(aes(xintercept = m), col = "red") + 
  theme_bw() +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank())


plot.score.dists = ggplot(d.scores %>% mutate(m = mean(comfort_rating))) + 
         geom_density_ridges(aes(x = comfort_rating, y = fct_rev(interaction(value, variable)), 
               fill = variable), bandwidth = .6, color = "black") + 
  scale_x_continuous(breaks = 1:7, limits = c(1,7)) +
  scale_fill_discrete(name = "Score") +
  scale_y_discrete(labels = rev(c(rep(LETTERS[1:5], 3)[1:14]))) + 
  xlab("comfort rating") +
  ylab("") +
  theme_bw() +
  theme3 +
  theme(axis.text.y = element_text(vjust = -.1))

ggsave(filename = "../IMG/ratings_vs_scores_joy.png", plot.score.dists, width = 6, height = 6)

