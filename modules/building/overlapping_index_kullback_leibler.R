library(dplyr)
library(KNN)
library(overlap)
library(overlapping)

set.seed(1)
batter = c(1,1,1,1,1,2,2,2,2,3,3,3,3,3,3,3,3,3,3)
half   = c(1,1,1,2,2,1,1,2,2,1,1,1,1,2,2,2,2,2,2)
dat = data.frame(batter = batter, 
                 half = ifelse(half == 1, "first", "second"), 
                 x = runif(length(half)))

# examine counts
dat %>%
  mutate(., cnt = 1) %>% 
  group_by(., batter, half) %>%
  summarise(., tot_bats = sum(cnt))


# overlap::overlapTrue appears to be same formula as overlapping::overlap
# ?overlap::overlapTrue
# overlapping::overlap see Pastore paper 

# compute (should work with unequal counts)
dat %>%
  group_by(., batter) %>%
  group_map(
    ~mutate(., 
            lap = overlap::overlapTrue(
              filter(., half == "first")$x,
              filter(., half == "second")$x
            ),
            over_ind = overlapping::overlap(
              list(filter(., half == "first")$x,
                   filter(., half == "second")$x),
              type = '1' # proportional
            ),
            kl_div1 = FNN::KL.divergence(
              filter(., half == "first")$x,
              filter(., half == "second")$x, k = 1),
            kl_div2 = FNN::KL.divergence(
              filter(., half == "second")$x,
              filter(., half == "first")$x, k = 1)
    )) %>% 
  suppressWarnings() %>%
  bind_rows(., .id = "batter") %>%
  group_by(., batter) %>%
  select(., batter, lap, over_ind, kl_div1, kl_div2) %>%
  summarise(., across(where(is.numeric), ~mean(.x, na.rm = TRUE))) %>%
  mutate(kl_div_mean = rowMeans(
    select(., starts_with("kl_")), na.rm = TRUE))
