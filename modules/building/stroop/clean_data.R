# libraries
library(ggplot2)
#library(dplyr)






######################################################
# Variables to calculate:
# 1. Accuracy 
# 2. General Errors (incorrect responses)
# 3. Word color errors Errors 
# 4. Trialtype as factor (order by passing levels = c() argument)
# 5. Measure of Stroop interference effect for each id (pivot?)

# Variables to summarize:
# 1. Probability/Percentage of General Errors 
# 2. Probability/Percentage of Word Color Error (specific)
# 3. Average response times for accurate trials for both congruent and incongruent 
# 4. Stroop interference effect 

# For Emotional Stroop
# Variables to calculate:
# 1. Accuracy 
# 2. Errors (incorrect responses)
# 3. Stimulus type as factor (order doesn't matter here but for practice order by ROYGBIV by passing levels = c() argument)
# 4. Difference between neutral and negative (for each id pivot?)
# 5. Difference between neutral and negative (for each id pivot?)

# Variables to summarize:
# 1. Probability/Percentage of Errors 
# 2. Average response times for accurate trials for both congruent and incongruent 
# 3. Average difference between neutral and negative
# 4. Avearge difference between neutral and positive
# 
# Average reaction times (RTs) on correct trials and error rates were 
# calculated for both congruent and incongruent trials for each subject 
# in each session. The Stroop interference effect (incongruent â€“ congruent)
# in both RT and also error rate was calculated separately

######################################################
# functions
#source("https://pastebin.com/raw/97NNTTzu")
#names_of_functions <- get_functions_from_file("https://pastebin.com/raw/97NNTTzu")

################################################################################
#df %>% dplyr::glimpse()


################################################################################
# clean up the file, rename vars, trim cases, etc.

df <- df %>%
  dplyr::rename(id = participant,
                corr = responseexper.corr,
                response = responseexper.keys,   # response
                ) %>%
  dplyr::mutate(counter = 1,                     # make a counter 
                rt = 1000 * responseexper.rt,    # make into ms
                trialnum = trials.thisN + 1,     # fix 0 base
                congruency = gsub("incongruengt", "incongruent", congruency)) %>%
  dplyr::filter(congruency != "",
                #corr == 1
                ) %>%
  dplyr::select(id,          # id
                #expName,              # project name
                counter,
                trialnum,             # the trial num
                word,                 # spelled word
                color,                # font color
                congruency,           # congruency
                congruent,            #
                corrAns,              # correct response
                response,             # response
                corr,                 # accuracy
                rt)                   # response latency


################################################################################
# check levels (note the levels retained because filtered after factor created)
# first level has to be reference/baseline
levels(df$congruency)

df <- df %>%
  dplyr::mutate(congruency = as.factor(congruency))

levels(df$congruency)

################################################################################
# trim rts
# df %>% dplyr::arrange(desc(rt)) # sort dataframe by descending to view extreme cases

# trim out long and short rts
# consider examining rates of rts beyond long and short and remove ids with high rates
df <- df %>%
  dplyr::mutate(
    short_rt = ifelse(rt < 200, 1, 0),  # flag
    long_rt  = ifelse(rt > 3000, 1, 0)  # flag
  )  %>% 
  dplyr::filter(
    long_rt == 0,    # filter out
    short_rt == 0    # filter out
  ) 

# trim out +-3 sd





###############################################################################
# by id, summary by each id
df %>%
  dplyr::group_by(id, congruency) %>%
  dplyr::summarise(rt = mean(rt),
                   corr = mean(corr)
  ) -> id.rt.corr

id.rt.corr %>% dplyr::arrange(corr)

# the mean corr
#mean(id.rt.corr$corr)

# filter out those who are too erroneous
id.rt.corr <- id.rt.corr %>%
  dplyr::filter(corr > .70)   # keep if > 70

# store the ids in an object for use later
keep_ids <- unique(id.rt.corr$id)


################################################################################
# Stroop interfereence effect
# 
# # Cognitive Control Measures. 
# Average reaction times (RTs) on correct trials 
# error rates were calculated for both congruent and incongruent trials 
# for each subject in each session. 
# Stroop interference effect (incongruent â€“ congruent) - RT and also error rate

# SIF

# to obtain a subtraction measure, we need to pivot the data frame and make 
# two columns out of the congruency column
head(df)

# spread(data, key, value)
#arguments: 
#  data: The data frame used to reshape the dataset
#  key: Column to reshape long to wide
#  value: Rows used to fill the new column
df2 <- df %>%
  dplyr::select(id, congruency, rt, corr) #%>%






id.sif.rt <- df %>%
  dplyr::filter(id %in% keep_ids) %>%             # keep those in the keep list
  dplyr::select(id, congruency, rt, corr) %>%     # select the key vars
  dplyr::group_by(corr) %>%                       # group by 0,1 corr
  tidyr::pivot_wider(., 
                     names_from = congruency, 
                     values_from = rt,
                     values_fn = list(rt = mean) # https://tidyr.tidyverse.org/articles/pivot.html
  ) %>%
  dplyr::group_by(id) %>%
  dplyr::mutate(sif = incongruent-congruent) %>%
  dplyr::filter(corr == 1) %>%                         # keep only accurate
  dplyr::select(id, congruent, incongruent, sif)


# Same using spread() and gather()
# 
# gather(data, key value, â€¦)
#arguments:
#  data: Name of the data frame
#  key: Name of the key column to create
#  value: Name of the value column to create

# spread() column x across n columns
run_if = F
if (run_if) {
df %>%
  dplyr::filter(id %in% keep_ids) %>%             # keep those in the keep list
  dplyr::select(id, congruency, rt, corr) %>%     # keep the key vars
  dplyr::filter(corr == 1) %>%                    # keep only accurate response trials
  dplyr::group_by(id, congruency) %>%             # group by id and type
  dplyr::summarize(rt = mean(na.omit(rt))) %>%    # then get means
  tidyr::spread(., key = congruency, value = rt) %>% # make wide
  dplyr::mutate(sif = incongruent-congruent) %>%    # get difference
  tidyr::gather(., key = "congruency", value = "sif", congruent:incongruent) %>% # make into one var names congruency
  dplyr::arrange(id)                               # arrange by id
}



# repeat for accuracy/corr
id.sif.acc <- df %>%
  dplyr::filter(id %in% keep_ids) %>%             # keep those in the keep list
  dplyr::select(id, congruency, corr) %>%
  #dplyr::group_by(corr) %>%
  tidyr::pivot_wider(., 
                     names_from = congruency, 
                     values_from = corr,
                     values_fn = list(corr = mean) # https://tidyr.tidyverse.org/articles/pivot.html
  ) %>%
  dplyr::group_by(id) %>%
  dplyr::mutate(sif = incongruent-congruent) %>%
  dplyr::select(id, congruent, incongruent, sif)

# merging/joining data frames
# some tips here https://www.guru99.com/r-dplyr-tutorial.html
# rename column for merge
names(id.sif.rt) <- paste0("rt_",names(id.sif.rt))
names(id.sif.acc) <- paste0("corr_",names(id.sif.acc))

# change first to "id" for matching
names(id.sif.rt)[1] <- "id"
names(id.sif.acc)[1] <- "id"  

head(id.sif.rt)
head(id.sif.acc)

# using base R
id_full <- merge(id.sif.rt, id.sif.acc, by = "id") 
#id_full %>% dplyr::distinct()

# or using dplyr
id_full <- dplyr::full_join(id.sif.rt, id.sif.acc, by = 'id')

id_full <- id_full %>%
  na.omit() #%>% head()

#id_full <- id_full %>%
#  as.data.frame() %>%
#  na.omit()
# change the id to a number for better plots
#rownames(id_full)

#id_full$id <- rownames(id_full) 
#head(id_full)

str(id_full)


id_full_rt <- id_full %>%
  tidyr::pivot_longer(., 
                      cols = tidyr::starts_with("rt_"), #rt_congruent:rt_sif,
                      names_to = "congruency", # make the column name
                      names_prefix = "rt_",    # this will strip off the prefix
                      values_to = "rt"      # 
  ) %>%
  #dplyr::arrange(id, congruency) %>%
  dplyr::select(id, congruency, rt)

#id_full_rt

# replicate for corr cols
id_full_acc <- id_full %>%
  tidyr::pivot_longer(., 
                      cols = tidyr::starts_with("corr_"), #rt_congruent:rt_sif,
                      names_to = "congruency", # make the column name
                      names_prefix = "corr_",    # this will strip off the prefix
                      values_to = "corr"      # 
  ) %>%
  #dplyr::arrange(id, congruency) %>%
  dplyr::select(id, congruency, corr)

# base R
id_full <- merge(id_full_rt, id_full_acc, by = c("id", "congruency"))

# dplyr
id_full <- dplyr::full_join(id_full_rt, id_full_acc, by = c('id', 'congruency'))


id_full %>%
  dplyr::filter()


################################################################################
# write data file
# ################################################################################
# create the dir if not exist
dir.create(paste0(data_dir, proj_name), showWarnings = F) # don't display warning if exists

readr::write_csv(id_full, 
                 paste0(data_dir, proj_name, "_data.csv"))

################################################################################
# end
################################################################################


