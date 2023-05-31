
# Modeling fracture arrest number of joints -------------------------------
library(tidyverse)
library(infer)
library(ggpmisc)

arrest <- 30

mcvn <- 50

sdcvn <- 0.2*mcvn

cvn_pop <- tibble(cvn =rnorm(5e5,mcvn, sdcvn))

arrested <- rep_sample_n(cvn_pop, size = 2e3,reps = 1e4,replace = TRUE) %>% 
  summarise(J = which.max(cvn>=arrest), JC = cvn[J])



tbl_j <- arrested %>%
  group_by(J) %>%
  summarise(nj = n()) %>%
  ungroup() %>%
  mutate(rsnj = cumsum(nj), 
         pnj = (1e4 - rsnj) / 1e4) %>%
  filter(J %in% c(5, 10, 20, 40, 100)) %>% 
  select(J, pnj) %>% 
  mutate(pnj = round(pnj,3)) %>% 
  rename('P' = pnj)


mxc <- arrested %>%
  ggplot() +
  geom_histogram(aes(x = J),
                 fill = 'mediumturquoise',
                 col = 'black') +
  labs(
    title = "Simulated Number of Joints to Arrest a Crack",
    subtitle = glue::glue("Based on Mean CVN = {mcvn} & Min Arrest CVN = {arrest}"),
    x = "Number of Joints",
    y = "Count",
    caption = "J = No. of Joints Before Arrest\nP = Probability of Exceedance"
  ) +
  scale_x_continuous(breaks = scales::pretty_breaks()) +
  theme_bw(14) +
  theme(plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm"))

maxcount <- max(ggplot_build(mxc)$data[[1]]$count)

mxc +
  annotate(
    geom = "table",
    x = max(arrested$J) * 0.80,
    y = maxcount * 0.85,
    label = list(tbl_j),
    size = 6
  )

# try CVN vs. POE ---------------------------------------------------------

mcs <- seq(20,35, by =5)
sdcs <- 0.2*mcs

cvn_tbl <- tibble(id = 1:length(mcs), mcs, sdcs) %>%
  nest(data = -id) %>%
  mutate(cvn_pop = map(data, ~ rnorm(5e5, .$mcs, .$sdcs))) %>%
  unnest(everything())


c35 <- cvn_tbl %>%
  filter(mcs == 35) %>%
  rep_sample_n(size = 2e3,
               reps = 1e4,
               replace = TRUE) %>%
  summarise(J = which.max(cvn_pop >= arrest), 
            JC = cvn_pop[J]) %>%
  mutate(cvn = 35)


c40 <- cvn_tbl %>%
  filter(mcs == 40) %>%
  rep_sample_n(size = 2e3,
               reps = 1e4,
               replace = TRUE) %>%
  summarise(J = which.max(cvn_pop >= arrest), 
            JC = cvn_pop[J]) %>%
  mutate(cvn = 40)

c45 <- cvn_tbl %>%
  filter(mcs == 45) %>%
  rep_sample_n(size = 2e3,
               reps = 1e4,
               replace = TRUE) %>%
  summarise(J = which.max(cvn_pop >= arrest), 
            JC = cvn_pop[J]) %>%
  mutate(cvn = 45)

c50 <- cvn_tbl %>%
  filter(mcs == 50) %>%
  rep_sample_n(size = 2e3,
               reps = 1e4,
               replace = TRUE) %>%
  summarise(J = which.max(cvn_pop >= arrest), 
            JC = cvn_pop[J]) %>%
  mutate(cvn = 50)

c55 <- cvn_tbl %>%
  filter(mcs == 55) %>%
  rep_sample_n(size = 2e3,
               reps = 1e4,
               replace = TRUE) %>%
  summarise(J = which.max(cvn_pop >= arrest), 
            JC = cvn_pop[J]) %>%
  mutate(cvn = 55)

c60 <- cvn_tbl %>%
  filter(mcs == 60) %>%
  rep_sample_n(size = 2e3,
               reps = 1e4,
               replace = TRUE) %>%
  summarise(J = which.max(cvn_pop >= arrest), 
            JC = cvn_pop[J]) %>%
  mutate(cvn = 60)

c65 <- cvn_tbl %>%
  filter(mcs == 65) %>%
  rep_sample_n(size = 2e3,
               reps = 1e4,
               replace = TRUE) %>%
  summarise(J = which.max(cvn_pop >= arrest), 
            JC = cvn_pop[J]) %>%
  mutate(cvn = 65)


bind_rows(c40, c45, c50, c55, c60, c65) %>%
  group_by(cvn, J) %>%
  summarise(nj = n()) %>%
  # ungroup() %>%
  mutate(rsnj = cumsum(nj),
         pnj = (1e4 - rsnj) / 1e4) %>%
  # filter(J %in% c(5, 10, 20, 40, 100)) %>%
  select(cvn, J, pnj) %>%
  mutate(pnj = round(pnj, 2)) %>%
  rename('P' = pnj) %>%
  ggplot(aes(J, P)) +
  geom_line(aes(col = factor(cvn)), linewidth = 1.1) +
  theme_bw(14) +
  xlim(0, 40) +
  scale_y_continuous(breaks = scales::pretty_breaks()) +
  labs(
    color = "CVN",
    title = "Probability of Exceeding a Number of Joints",
    subtitle = glue::glue('For a Minimum Arresting CVN = {arrest}'),
    x = "Number of Joints",
    y = "Probability of Exceedance"
  ) +
  theme(plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm"))


# loop --------------------------------------------------------------------

# mcs <- seq(40,70, by =5)
# sdcs <- 0.2*mcs
# 
# cvn_tbl <- tibble(id = 1:length(mcs), mcs, sdcs) %>%
#   nest(data = -id) %>%
#   mutate(cvn_pop = map(data, ~ rnorm(5e5, .$mcs, .$sdcs))) %>%
#   unnest(everything())
# 
# cvnm <- c()
# 
# for(i in 1:length(mcs)) {
#   cvnm[i] <-  cvn_tbl %>%
#     filter(mcs == mcs[i]) %>%
#     rep_sample_n(size = 2e3,
#                  reps = 1e4,
#                  replace = TRUE)
#   
# }
