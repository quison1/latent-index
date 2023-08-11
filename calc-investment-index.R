
# install.packages("tidyverse")
library(tidyverse)

# install.packages("mirt")
library(mirt)

# loading data
inv_raw <- read_csv("inv_sample_simulated.csv")

# discretise variable
inv_sample_discretised <- inv_raw %>%
  mutate(nswitch_cut = cut(nswitch_past, breaks = c(0, 1, 2, 3, 5, Inf), right = FALSE),
         noption_cut = cut(noption, breaks = c(1, 2, 3, 4, 5, Inf), right = FALSE))

# turn discretised variables into numeric
inv_sample <- inv_sample_discretised %>%
  transmute(nswitch_cut = as.numeric(nswitch_cut),
            noption_cut = as.numeric(noption_cut))

# fit a graded response model, with 1 latent variable, suppress printing iteration info
model2 <- mirt(inv_sample, model = 1, itemtype = "graded", verbose = FALSE)

# retrieve thetas (latent variable), investment activity index
inv_res <- inv_sample %>% mutate(fscore = fscores(model2)[, "F1"])

# get a summary table for the distinct 5x5 types of members
inv_res_summary <- inv_res %>%
  transmute(nswitch_cut, noption_cut, fscore = round(fscore, 2)) %>%
  group_by(nswitch_cut, noption_cut) %>%
  summarise(theta = mean(fscore), count = n(), .groups = "drop")

# write_csv(inv_res_summary, "investment-activity-index-summary.csv")

# visualise the score for each group
inv_res_summary %>%
  ggplot(aes(x = nswitch_cut, y = noption_cut, fill = theta)) +
  geom_tile() +
  geom_text(aes(label = round(theta, 2))) +
  scale_fill_viridis_c() +
  labs(title = "Investment activity index (all four periods)",
       x = "Number of switch in past 12-month", y = "Number of options held")

# visualise the count in each group
inv_res_summary %>%
  ggplot(aes(x = nswitch_cut, y = noption_cut, fill = log(count))) +
  geom_tile() +
  geom_text(aes(label = count)) +
  scale_fill_viridis_c() +
  labs(title = "Member counts (all four periods)",
       x = "Number of switch in past 12-month", y = "Number of options held")
