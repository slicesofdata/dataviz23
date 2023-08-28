# MVP 
# - clean data: get rt for correct trials only, trim long responses, ....
# - plot density plot or histogram for congruent and incongruent trials in one plot
# - model data by predicting rt by congruence 
# - rt ~ congruency and binary game liking; game rating; or other variable in the data that might be interesting
# - sif to predict game liking; incongruence rt to predict game liking
# - rating games metrics

#install.packages("easystats", repos = "https://easystats.r-universe.dev")
#install.packages("report")
library(see)
library(magrittr)
library(ggplot2)
source("C:/Users/gcook/Sync/Progs/R/R-Portable/mypackages/zaatar Scripts/model_tables.txt")

lm(wt ~ ., mtcars) %>% 
  gtsummary::tbl_regression()

m1 <- lm(wt ~ mpg, mtcars)
m2 <- lm(wt ~ cyl + mpg, mtcars)

performance::compare_performance(m1, m2)
plot(performance::compare_performance(m1, m2, rank = TRUE))
- look up bias-variance tradeoff principle (for comparing models)
- 
spider plot - https://www.data-to-viz.com/caveat/spider.html
model <- lm(wt ~ cyl + mpg, mtcars)
model <- lm(Sepal.Length ~ Species, data = iris)


report::report(model)
report::report_table(model)
report::report_parameters(model) #%>% htmlTable::htmlTableWidget()


- train models
-- performance::model_performance(m1)
- test models

-- # checking model assumptions
  check_model(model)
Assessing model quality: performance::r2()
Model diagnostics
Comprehensive visualization of model checks: performance::check_model()
- Summarizing Model Performance
-- performance::model_performance() with argument `rank = TRUE``
- Comparing Models
-- performance::compare_performance()

performance::check_collinearity()
`check_collinearity()`, `check_normality()` or `check_heteroscedasticity()`. 
To get a comprehensive check, use `check_model()`


library(performance)
plot(compare_performance(m1, m2, m4, rank = TRUE))


http://niatur.kennesawglass.com/charm-https-github.com/easystats/performance



function(data, 
         x = NULL, by = NULL, filter = NULL, trim = .1) {
  # summarizes a data frame
  # dependent packages
  dep = c("dplyr", "DescTools", "magrittr"); 
  pkgs = dep[!(dep %in% installed.packages()[,"Package"])]
  if(length(pkgs)) install.packages(pkgs, dep = T)
  
  #dplyr::select_(data, as.name(x))
  # manipulate the data frame
  if (is.data.frame(data)) {
    
    data = data %>% 
      dplyr::group_by_at(by) %>%
      dplyr::summarise(
        n = dplyr::n(),
        mean = mean(.data[[x]], na.rm = T),
        mean.trim = mean(DescTools::Trim(.data[[x]], trim = trim)),
        mdn = median(.data[[x]], na.rm = T),
        sum = sum(.data[[x]], na.rm = T), 
        sd = sd(.data[[x]], na.rm = T),
        se = sd(.data[[x]], na.rm = T) / sqrt(length(na.omit(.data[[x]]))),
        skew = moments::skewness(.data[[x]], na.rm = T),
        kurt = moments::kurtosis(.data[[x]], na.rm = T),
        min = min(.data[[x]], na.rm = T),
        max = max(.data[[x]], na.rm = T),
        mad = mad(.data[[x]], na.rm = T)
      )
  }
  if (is.vector(data)) {
    data = data.frame(x = data, var = gsub(".*\\$", "", deparse(substitute(D$V1))))
    
    data = data %>% 
      dplyr::group_by(var) %>%
      dplyr::summarise(
        n = dplyr::n(),
        mean = mean(x, na.rm = T),
        mean.trim = mean(DescTools::Trim(x, trim = trim)),
        mdn = median(x, na.rm = T),
        sum = sum(x, na.rm = T), 
        sd = sd(x, na.rm = T),
        se = sd(x, na.rm = T) / sqrt(length(na.omit(x) ) ),
        skew = moments::skewness(x, na.rm = T),
        kurt = moments::kurtosis(x, na.rm = T),
        min = min(x, na.rm = T),
        max = max(x, na.rm = T),
        mad = mad(x, na.rm = T)
      )
  }
  
  
  return(data)
}


aov.kable()

x1 = lm(mpg ~ qsec + factor(am) + wt + factor(gear), data = mtcars) 
x2 = aov(mpg ~ qsec + factor(am) + wt + factor(gear), data = mtcars) 

kableMod(x1, caption = "Model")
kableMod(lm(mpg ~ qsec + factor(am) + wt + factor(gear), data = mtcars), caption = "Model")
kableMod(x2, caption = "Model")
#

# libraries
#library(dplyr)

# source functions
#source("https://pastebin.com/raw/97NNTTzu")







#head(df)

#head(id_full) 

################################################################################
# plotting distributions

# new plot (limits don't need setting because extremes have been trimmed)
p_rt_by_color <- ggplot(df, aes(rt, fill = congruency)) + 
  geom_density(alpha = .75,
               position = "identity") + 
  facet_wrap(~color) + 
  ggtitle("RT ~ Font Color") +
  # add better x-axis breaks
  scale_x_continuous(limits = c(0, 3000), breaks = seq(0, 3000, 500))

#+ theme_bw(15)

print(p_rt_by_color)


p_rt_by_word <- ggplot(df, 
                       aes(rt, fill = congruency)) + 
  geom_density(alpha = .75,
               position = "identity") + 
  facet_wrap(~word) + 
  ggtitle("RT ~ Word") +
  scale_x_continuous(limits = c(0, 3000), breaks = seq(0, 3000, 500))

print(p_rt_by_word)


p_rt_by_cong <- 
  id_full %>%
  dplyr::filter(congruency != "sif") %>%
  ggplot(., aes(rt, fill = congruency)) + 
  geom_density(alpha = .75,
               position = "identity") + 
   
  ggtitle("RT ~ Congruency") +
  scale_x_continuous(limits = c(0, 3000), breaks = seq(0, 3000, 500))

print(p_rt_by_cong)

id_full %>%
  as.data.frame() %>%
  dplyr::filter(congruency == "sif") %>%
  dplyr::summarise(
    n = dplyr::n(),
    x = mean(rt, na.rm = T)
                   )

length(id_full$corr)


a = id_full %>%
  dplyr::filter(congruency != "sif") #%>%
str(a)
length(a$rt)
mean(a$rt, na.rm = T)
sd(a$rt, na.rm = T)

id_full$congruency

as.data.frame(id_full)

means(id_full, "rt", "congruency") %>%
  htmlTable::htmlTableWidget()



id_full %>%
  as.data.frame() %>%
  #dplyr::select(rt) %>%
  #mean(.)
  dplyr::group_by(congruency) %>%
  dplyr::summarise(
    n = dplyr::n(),
    rt = mean(as.vector(rt)),
    rtx = sd(as.vector(rt), na.rm = T),
    corr = mean(corr, na.rm = T),
    corr_sd = sd(corr, na.rm = T),
  )
  
    #
    #corr_sd = sd(corr, na.rm = T),
#  )

p_rt_sif <- 
  id_full %>%
  dplyr::filter(congruency == "sif") %>%
  ggplot(., aes(rt, fill = congruency)) + 
  geom_density(alpha = .75,
               position = "identity") + 
  #facet_wrap(~word) + 
  #ggtitle("RT ~ Word") +
  scale_x_continuous(limits = c(0, 3000), breaks = seq(0, 3000, 500))

print(p_rt_sif)

p_corr_sif <- 
  id_full %>%
  dplyr::filter(congruency == "sif") %>%
  ggplot(., aes(corr, fill = congruency)) + 
  geom_density(alpha = .75,
               position = "identity") + 
  #facet_wrap(~word) + 
  #ggtitle("RT ~ Word") +
  scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, .1))


print(p_corr_sif)

################################################################################
cor(id.rt.corr$rt, id.rt.corr$corr, 
    method = c("pearson", "kendall", "spearman"))


# scatter plot
# some examples at http://sthda.com/english/wiki/ggplot2-scatter-plots-quick-start-guide-r-software-and-data-visualization

p_rt_by_corr <- ggplot(id.rt.corr, 
                       aes(x = rt, 
                           y = corr,
                           color = congruency)) + 
  geom_point() +
  geom_smooth(method = lm, se = FALSE, fullrange = TRUE) +
  theme_classic()


print(p_rt_by_corr)

## Conducting model
get_influential <- function(model) {
  # this function passes model to determine influential 
  # points and returns the row index as a vector
  return(
    car::influencePlot(model) %>% rownames() %>% as.numeric()
  )
}



id_full_mod <- id_full %>%
  dplyr::filter(congruency != "sif") %>%
  lm(rt ~ congruency, data = .) #

predict(id_full_mod, newdata = tibble(trial_type.e = 0.5))

residuals(id_full_mod) %>%
  ggplot(aes(.))

congruency.types <- c("congruent" = 0.5, "incongruent" = -0.5) # effect code

id_full_rt_cong <- id_full_rt %>%
  dplyr::filter(congruency != "sif") %>%
  dplyr::mutate(
    congruency.e = dplyr::recode(congruency, !!!congruency.types)
    )

id_full_rt_cong %>%
  #dplyr::filter(congruency != "sif") %>%
  ggplot(., aes(congruency, rt)) + 
  geom_violin() +
  geom_boxplot(aes(fill = congruency), 
               width = 0.25, show.legend = FALSE)

id_full_rt_cong_mod <- id_full_rt %>%
  #dplyr::filter(congruency != "sif") %>%
  lm(rt ~ congruency, data = .)
#  lm(rt ~ 1, data = .))



id_full_mod %>% lm.kable()
id_full_mod %>% 
  gtsummary::tbl_regression()
# using gtsummary
id_full_mod %>% 
  gtsummary::tbl_regression() %>%
  #gtsummary::add_vif() #%>%
  #gtsummary::add_glance_source_note() #%>%
  gtsummary::add_global_p() %>%
  gtsummary::add_q()

function(model) {
  
  t = model %>% gtsummary::tbl_regression()
    
  if (dim(model$model)[2] > 2) { 
    t = t %>% gtsummary::add_vif() 
    }
  
}  
  
adds the global p-value for a categorical variables


adds statistics from `broom::glance()` as source note


adds column of the variance inflation factors (VIF)



?gtsummary::tbl_regression(
  
?gtsummary::tbl_regression
# using easy stats
id_full_mod %>% parameters::model_parameters()# %>%
  htmlTable::htmlTableWidget()
  

fix_mod <- function(model) {
  model %>% 
    parameters::select_parameters() %>%
    parameters::model_parameters() %>%
    as.data.frame() %>%
    dplyr::mutate_if(is.numeric, round, digits = 2) %>%
    dplyr::mutate_if(p digits = 2)
}  

fix_mod(id_full_mod) %>% htmlTable::htmlTableWidget()

id_full_mod %>% performance::check_model()

performance::performance(id_full_mod) %>%
  performance::print_md()

# using parsnip
parsnip::linear_reg() %>%
  parsnip::set_engine("lm") %>%
  parsnip::fit(hwy ~ displ, data = mpg) %>%
  performance::check_model()

mpg %>%
  lm(hwy ~ displ, data = .) %>%
  performance::check_model()
  
?compare_performance
cases.inf <- get_influential(id_full_mod)

performance::compare_performance(model1, model2, model3) %>%
  performance::print_md()

[comparing models](https://easystats.github.io/performance/articles/compare.html)


plot(compare_performance(model1, model2, model3))


id_full_mod$terms

lm(rt ~ congruency, 
   data = .data[-id_full[c(cases.inf), ]])

lm(rt ~ congruency, data = id_full_mod$model[-(cases.inf),]) %>%
  performance::check_model()


id_full[c(abc), ] %>% head()
  




car::influencePlot(id_full_mod) %>%
  rownames()
id_full_mod$


t_test_rt_by_cong <- t.test(rt ~ congruency, 
                            data = id_full[id_ful], 
                            paired = FALSE)

t_test_rt_by_cong %>%
  parameters::parameters()

mod_rt_by_cong <- lm(rt ~ congruency, data = means)
summary(mod_rt_by_cong)

###############################################################################
###############################################################################

# for some fancy plots borror code from ggExtra simulation
#ggExtra::runExample()


p <- ggplot(id.rt.corr, aes_string('rt', 'corr')) +
  aes_string(colour = 'congruency') +
  geom_point() + 
  geom_smooth(method = lm, se = F, fullrange = T) +
  theme_bw(15)

ggExtra::ggMarginal(
  p,
  type = 'density',
  margins = 'both',
  size = 5,
  groupColour = TRUE,
  groupFill = TRUE
)







df %>% dplyr::glimpse()

#names(df)

# check if factor and levels
levels(df$congruency)


df <- df %>%
  dplyr::rename(id = participant,
                corr = responseexper.corr,       # accuracy
                response = responseexper.keys,   # response
                ) %>%
  dplyr::filter(congruency != "",                # filter BEFORE as.factor
                ) %>%  
  dplyr::mutate(counter = 1,                     # make a counter 
                rt = (1000 * responseexper.rt),    # make into ms
                trialnum = trials.thisN + 1,     # fix 0 base
                congruency = as.factor(gsub("incongruengt", "incongruent", congruency))
                ) %>%

  dplyr::select(id,                   # id
                #expName,             # project name
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
  #dplyr::select(., -c(responseexper.corr, responseexper.rt, trials.thisN))

# check levels (note the levels retained because filtered after factor created)
# first level has to be reference/baseline
levels(df$congruency)

#make sure the congruency variable is what you think it is
# df$match <- with(df, ifelse(word == color, "congruent", "incongruent"))
#df$congruency <- relevel(df$congruency, ref = 2)  # Apply relevel function

# check max for plots
max(df$rt)

# congruency only plot
p_rt_by_cong <- ggplot(df, aes(rt, fill = congruency)) + 
  geom_density(alpha = .5,
               position = "identity") + 
  xlim(0, 5000) +
  ggtitle("RT ~ Congruency")

print(p_rt_by_cong)





# congruency x color  plot
p_rt_by_cong_by_col <- ggplot(df, aes(rt, fill = congruency)) + 
  geom_density(alpha = .75,
               position = "identity") + 
  facet_wrap(~color) +
  xlim(0, 5000) +
  ggtitle("RT ~ Font Color") +
  theme(strip.text = element_text(face = "bold"))
  

print(p_rt_by_cong_by_col)


# new plot
p_rt_by_cong_by_col <- ggplot(df, aes(rt, fill = congruency)) + 
  geom_density(alpha = .75,
               position = "identity") + 
  facet_wrap(~color) + 
  ggtitle("RT ~ Font Color") #+ theme_bw(15)

print(p_rt_by_cong_by_col)


p_rt_by_cong_by_word <- ggplot(df, aes(rt, fill = congruency)) + 
  geom_density(alpha = .75,
               position = "identity") + 
  facet_wrap(~word) + 
  ggtitle("RT ~ Word")

print(p_rt_by_cong_by_word)

###############################################################################
# # Cognitive Control Measures. 
# Average reaction times (RTs) on correct trials 
# error rates were calculated for both congruent and incongruent trials 
# for each subject in each session. 
# Stroop interference effect (incongruent – congruent) - RT and also error rate




cor(id.rt.corr$rt, id.rt.corr$corr, 
    method = c("pearson", "kendall", "spearman"))


# scatter plot
# some examples at http://sthda.com/english/wiki/ggplot2-scatter-plots-quick-start-guide-r-software-and-data-visualization

p_rt_by_corr <- ggplot(id.rt.corr, 
                       aes(x = rt, 
                           y = corr,
                           color = congruency)) + 
  geom_point() +
  geom_smooth(method = lm, se = FALSE, fullrange = TRUE) +
  theme_classic()

print(p_rt_by_corr)

## Conducting t-test
t_test_rt_by_cong <- t.test(rt ~ congruency, 
                            data = means, 
                            paired = FALSE)

t_test_rt_by_cong %>%
  parameters::parameters()

mod_rt_by_cong <- lm(rt ~ congruency, data = means)
summary(mod_rt_by_cong)

###############################################################################
###############################################################################

# for some fancy plots borror code from ggExtra simulation
#ggExtra::runExample()


p <- ggplot(id.rt.corr, aes_string('rt', 'corr')) +
  aes_string(colour = 'congruency') +
  geom_point() + 
  geom_smooth(method = lm, se = F, fullrange = T) +
  theme_bw(15)

ggExtra::ggMarginal(
  p,
  type = 'density',
  margins = 'both',
  size = 5,
  groupColour = TRUE,
  groupFill = TRUE
)








# Basic rt and accuracy
df.rt.acc <- df %>%
  dplyr::group_by(id, congruency) %>%
  dplyr::summarise(rt = mean(rt),
                   corr = mean(corr)
  ) -> df.rt.acc

head(df.rt.acc)

p_rt_by_cong_hist <- ggplot(df.rt.acc, aes(rt, color = congruency, fill = congruency)) + 
  geom_histogram(
    bins = 30,
    alpha = .5,
    position = "identity") + 
  ggtitle("RT ~ Congruency")

print(p_rt_by_cong_hist)

#####################################################################
# rt model
# linear model
mod_rt_by_cong <- lm(rt ~ congruency, data = df.rt.acc)

# check model performance using easystats performance library
performance::check_model(mod_rt_by_cong)

# regular model parameters
parameters::model_parameters(mod_rt_by_cong)

#plot(parameters::parameters(mod.rt))

mod_rt_by_cong %>%
  parameters::select_parameters() %>%
  parameters::model_parameters() %>%
  as.data.frame() %>%
  dplyr::mutate_if(is.numeric, round, digits = 2) %>% # round
  htmlTable::htmlTableWidget()
  

# standardized parameters
parameters::model_parameters(mod_rt_by_cong, standardize = "refit")

# 
predicted <- modelbased::estimate_expectation(mod_rt_by_cong, data = "grid")
plot(predicted)


#####################################################################
# accuracy: file best predictors (exploratory)
#lm(rt ~ ., data = means) |>
#  parameters::select_parameters() |>
#  parameters::model_parameters()

# accuracy
mod_acc_by_cong <- lm(corr ~ congruency, data = df.rt.acc)


# check model performance using easystats performance library
performance::check_model(mod_acc_by_cong)


# regular model parameters
parameters::model_parameters(mod_acc_by_cong)

#plot(parameters::parameters(mod.rt))

mod_acc_by_cong %>%
  parameters::select_parameters() %>%
  parameters::model_parameters() %>%
  as.data.frame() %>%
  dplyr::mutate_if(is.numeric, round, digits = 2) %>% # round
  htmlTable::htmlTableWidget()


# standardized parameters
parameters::model_parameters(mod_acc_by_cong, standardize = "refit")

# 
predicted <- modelbased::estimate_expectation(mod_acc_by_cong, data = "grid")
plot(predicted)

#####################################################################




head(df.sif.acc)


# correlations
## Conducting t-test
t.test(rt ~ congruency, 
       data = df, 
       paired = T)

mod <- lm(rt ~ congruency, data = df)
summary(mod)





# Correlation test and graph
cor(df$rt, df$corr, method = "pearson")







cor(means$rt, means$corr, 
    method = c("pearson", "kendall", "spearman"))



ggpubr::ggscatter(df, x = "rt", y = "congruency", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Duration", ylab = "Correct")
###############################################################################
###############################################################################

head(df, 200)
  dplyr::filter(
    
  )

df %>%
  dplyr::group_by(id, 
                  congruency
                  ) %>%
  dplyr::summarise(trials = sum(counter),
                   rt_max = max(rt, na.rm = FALSE),
                   rt_min = min(rt, na.rm = FALSE),
                   rt_mn  = mean(rt, na.rm = FALSE),
                   rt_sd  = sd(rt, na.rm = FALSE),
                   corr   = mean(corr, na.rm = FALSE)
                   )


ggExtra::runExample()
# Note the long trials
df %>%
  ggplot2::ggplot(., rt_sd) +
  ggplot2::geom_histogram()
                     

library(ggplot2)

# Histogram by group in ggplot2




