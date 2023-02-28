################################################################################
##
## Script to compute GLMM studying the effect of protection and time sequences
## ... on feeding activities
##
## 5_Models_seq_effect_time_protection.R
##
## 27/01/2023
##
## Camille Magneville
##
################################################################################


# Model: does the protection level and time affect activity intensity? ####
# AT THE TIMESLOT (3 in a day) LEVEL (see previous script for the sequence levels)


# 1 - Load data:


bites_seq_guilds_03_df <- readRDS(here::here("transformed_data",
                                             "bites_seq_guilds_03_df.rds"))
# add timeslot info:
bites_seq_guilds_03_df$timeslot <- c(rep("timeslot_1", 4), rep("timeslot_2", 4),
                                     rep("timeslot_3", 4))
bites_seq_guilds_04_df <- readRDS(here::here("transformed_data",
                                             "bites_seq_guilds_04_df.rds"))
# add timeslot info:
bites_seq_guilds_04_df$timeslot <- c(rep("timeslot_1", 4), rep("timeslot_2", 4),
                                     rep("timeslot_3", 4))
bites_seq_guilds_05_df <- readRDS(here::here("transformed_data",
                                             "bites_seq_guilds_05_df.rds"))
# add timeslot info:
bites_seq_guilds_05_df$timeslot <- c(rep("timeslot_1", 4), rep("timeslot_2", 4),
                                     rep("timeslot_3", 4))
bites_seq_guilds_06_df <- readRDS(here::here("transformed_data",
                                             "bites_seq_guilds_06_df.rds"))
# add timeslot info:
bites_seq_guilds_06_df$timeslot <- c(rep("timeslot_1", 4), rep("timeslot_2", 4),
                                     rep("timeslot_3", 4))
bites_seq_final_df <- dplyr::bind_rows(bites_seq_guilds_03_df,
                                       bites_seq_guilds_04_df,
                                       bites_seq_guilds_05_df,
                                       bites_seq_guilds_06_df)
bites_seq_final_df$site <- as.factor(bites_seq_final_df$site)
bites_seq_final_df$timeslot <- as.factor(bites_seq_final_df$timeslot)


# 2 - GLMM effect site (protection) and timeslot (time of the day)?


# change the name of the studied column to study the different guilds:
trophic_guild <- "coral_bites"

# Does the number of bites follow a Poisson distrib?
hist(bites_seq_final_df[, trophic_guild])
poisson <- MASS::fitdistr(bites_seq_final_df[, trophic_guild], "Poisson")
car::qqp(bites_seq_final_df[, trophic_guild], "pois", lambda = poisson$estimate)

theoretic_count <- rpois(nrow(bites_seq_final_df)
                         , mean(bites_seq_final_df[, trophic_guild]))
tc_df <- data.frame(theoretic_count)
ggplot2::ggplot(bites_seq_final_df, ggplot2::aes(get(trophic_guild))) +
  ggplot2::geom_bar(fill = "#1E90FF") +
  ggplot2::geom_bar(data = tc_df, ggplot2::aes(theoretic_count,fill = "#1E90FF", alpha = 0.5)) +
  ggplot2::theme_classic() +
  ggplot2::theme(legend.position="none")
# seems like data is overdispersed: negative binom (model done with poisson ...
# ... and overdisp signif)

# Plot data:
bites_seq_final_df$timeslot <-  factor(bites_seq_final_df$timeslot,
                                  levels = paste0("timeslot", sep ="_", c(1:3)))

plot_bites <- ggplot2::ggplot(data = bites_seq_final_df) +

  ggplot2::geom_boxplot(ggplot2::aes(y = get(trophic_guild),
                                     x = timeslot,
                                     fill = site),
                        outlier.shape = NA)
plot_bites

# Get mean values:
mean_bites <- bites_seq_final_df %>%
  dplyr::group_by(timeslot, site) %>%
  dplyr::summarise(nb = n(),
                   avg = mean(get(trophic_guild)))
mean_bites

# Build the model:
model_bites <- lme4::glmer.nb(get(trophic_guild) ~ site + timeslot + (1|day),
                              data = bites_seq_final_df)

summary(model_bites)
car::Anova(model_bites) # H0: no effect of the variable

# Check residuals and other:
DHARMa::simulateResiduals(fittedModel = model_bites, plot = TRUE)
performance::check_overdispersion(model_bites)
performance::r2_nakagawa(model_bites, by_group = FALSE, tolerance = 1e-8)
performance::check_model(model_bites)


# 2 - GLMM effect timeslot (time of the day) only on each site?


# change the name of the studied column to study the different guilds:
trophic_guild <- "coral_bites"


## FPA:
# Does the number of bites follow a Poisson distrib?
bites_feedact_df <- bites_seq_final_df[bites_seq_final_df$site == "FPA", ]
hist(bites_feedact_df[, trophic_guild])
poisson <- MASS::fitdistr(bites_feedact_df[, trophic_guild], "Poisson")
car::qqp(bites_feedact_df[, trophic_guild], "pois", lambda = poisson$estimate)

theoretic_count <- rpois(nrow(bites_feedact_df)
                         , mean(bites_feedact_df[, trophic_guild]))
tc_df <- data.frame(theoretic_count)
ggplot2::ggplot(bites_feedact_df, ggplot2::aes(get(trophic_guild))) +
  ggplot2::geom_bar(fill = "#1E90FF") +
  ggplot2::geom_bar(data = tc_df, ggplot2::aes(theoretic_count,fill = "#1E90FF", alpha = 0.5)) +
  ggplot2::theme_classic() +
  ggplot2::theme(legend.position="none")
# seems like data is overdispersed: negative binom (model done with poisson ...
# ... and overdisp signif)

# Plot data:
bites_feedact_df$timeslot <-  factor(bites_feedact_df$timeslot,
                                       levels = paste0("timeslot", sep ="_", c(1:3)))

plot_bites <- ggplot2::ggplot(data = bites_feedact_df) +

  ggplot2::geom_boxplot(ggplot2::aes(y = get(trophic_guild),
                                     x = timeslot),
                        outlier.shape = NA)
plot_bites

# Get mean values:
mean_bites <- bites_feedact_df %>%
  dplyr::group_by(timeslot, site) %>%
  dplyr::summarise(nb = n(),
                   avg = mean(get(trophic_guild)))
mean_bites

# Build the model:
model_bites <- lme4::glmer.nb(get(trophic_guild) ~ timeslot + (1|day),
                              data = bites_feedact_df)
summary(model_bites)
car::Anova(model_bites) # H0: no effect of the variable

# Check residuals and other:
DHARMa::simulateResiduals(fittedModel = model_bites, plot = TRUE)
performance::check_overdispersion(model_bites)
performance::r2_nakagawa(model_bites, by_group = FALSE, tolerance = 1e-8)
performance::check_model(model_bites)


## PPA:
# Does the number of bites follow a Poisson distrib?
bites_feedact_df <- bites_seq_final_df[bites_seq_final_df$site == "PPA", ]
hist(bites_feedact_df[, trophic_guild])
poisson <- MASS::fitdistr(bites_feedact_df[, trophic_guild], "Poisson")
car::qqp(bites_feedact_df[, trophic_guild], "pois", lambda = poisson$estimate)

theoretic_count <- rpois(nrow(bites_feedact_df)
                         , mean(bites_feedact_df[, trophic_guild]))
tc_df <- data.frame(theoretic_count)
ggplot2::ggplot(bites_feedact_df, ggplot2::aes(get(trophic_guild))) +
  ggplot2::geom_bar(fill = "#1E90FF") +
  ggplot2::geom_bar(data = tc_df, ggplot2::aes(theoretic_count,fill = "#1E90FF", alpha = 0.5)) +
  ggplot2::theme_classic() +
  ggplot2::theme(legend.position="none")
# seems like data is overdispersed: negative binom (model done with poisson ...
# ... and overdisp signif)

# Plot data:
bites_feedact_df$timeslot <-  factor(bites_feedact_df$timeslot,
                                     levels = paste0("timeslot", sep ="_", c(1:3)))

plot_bites <- ggplot2::ggplot(data = bites_feedact_df) +

  ggplot2::geom_boxplot(ggplot2::aes(y = get(trophic_guild),
                                     x = timeslot),
                        outlier.shape = NA)
plot_bites

# Get mean values:
mean_bites <- bites_feedact_df %>%
  dplyr::group_by(timeslot, site) %>%
  dplyr::summarise(nb = n(),
                   avg = mean(get(trophic_guild)))
mean_bites

# Build the model:
model_bites <- lme4::glmer.nb(get(trophic_guild) ~ timeslot + (1|day),
                              data = bites_feedact_df)
summary(model_bites)
car::Anova(model_bites) # H0: no effect of the variable

# Check residuals and other:
DHARMa::simulateResiduals(fittedModel = model_bites, plot = TRUE)
performance::check_overdispersion(model_bites)
performance::r2_nakagawa(model_bites, by_group = FALSE, tolerance = 1e-8)
performance::check_model(model_bites)
