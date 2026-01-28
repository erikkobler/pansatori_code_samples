# PACKAGES ----------------------------------------------------------------

if(!require("pacman")) install.packages("pacman")
library(pacman)
pacman::p_load(dplyr,ggplot2,lubridate, tidyr, purrr, broom, lmtest, showtext, writexl,stringr, openxlsx)
source("scripts/00_helper_functions.R")


# PARAMETERS --------------------------------------------------------------

PARAM_VARIABLE = "loudness" # run analysis for tinnitus loudness over time
PARAM_TIME_WINDOW = c(as.Date("2015-08-01"),as.Date("2023-07-31")) #use patients within window #NULL for all
PARAM_TIME_INTERVAL_SINCE_BEGINNING = 7*12 #in days -> 7*12 = 12 weeks
PARAM_ALPHA = 0.1
PARAM_MIN_ENTRIES_PERC = 0.5 #minimum entries necessary to be included
PARAM_MAX_GAP = 3 #maximum gap (in days) that is allowed between entries to be included

# CREATE DIRECTORY PATHS --------------------------------------------------

if(!dir.exists(file.path("results","app_analysis"))) {
  dir.create(path = file.path("results","app_analysis"))
}

res_path = file.path("results","app_analysis",Sys.Date())
if(!dir.exists(res_path)) {
  dir.create(path = res_path)
}

# GET ANONYMIZED DATA -----------------------------------------------------

app = read.csv(file="data/app_simulated_sample.csv",header=T)

app = app %>%
  select(
    vpvec=vpvec,
    datetime,
    loudness = lautstaerke,
    strain=belastung,
    jaw = kiefer_verspannung,
    neck =nacken_verspannung,
    distress = stress,
    mood =stimmung
  )


# CLEANING ----------------------------------------------------------------

app = app %>%
  mutate(datetime = dmy_hm(datetime)) %>%
  filter(!is.na(datetime))

app = app %>%
  mutate(
    loudness = as.numeric(loudness),
    strain = as.numeric(strain),
    jaw = as.numeric(jaw),
    neck = as.numeric(neck),
    distress = as.numeric(distress),
    mood = as.numeric(mood))

#sorting by patient and time
app = app %>%
  arrange(vpvec,datetime)


# NEW COLUMNS -------------------------------------------------------------

#timespan between first and last entry (app)
app = app %>%
  group_by(vpvec) %>%
  mutate(total_time_range_days = last(datetime) - first(datetime),
         total_time_range_days = round(as.integer(total_time_range_days)/(60*60*24),0)
  )

#timespan between consecutive entries
app = app %>%
  group_by(vpvec) %>%
  mutate(hours_since_last_entry = round(as.numeric(datetime-lag(datetime,default=first(datetime)))/(60*60),0),
         hours_since_start = round(as.numeric(datetime-first(datetime))/(60*60)+1,0),
         days_since_start = hours_since_start/24+1)


# SELECT TIME WINDOW ------------------------------------------------------

#only keep subjects who started within time window
time_window = as.Date(PARAM_TIME_WINDOW)
subjects_in_time_window = unique(app$vpvec)
if(length(time_window)==2) {
  selected_subjects = app %>%
    group_by(vpvec) %>%
    slice_head(n=1) %>%
    filter(datetime>=time_window[1] & datetime<=time_window[2])
  
  subjects_in_time_window = unique(selected_subjects$vpvec)
}
app = app %>%
  filter(vpvec %in% subjects_in_time_window)


# only keep entries entries within pre-defined interval since beginning
app = app %>%
  filter(days_since_start <= PARAM_TIME_INTERVAL_SINCE_BEGINNING)

#only keep participants where no gaps of pre-defined length are exceeded
long_gaps = app %>%
  filter(hours_since_last_entry/24 > PARAM_MAX_GAP)
selected_subjects = unique(long_gaps$vpvec)

app = app %>%
  filter(!(vpvec %in% selected_subjects))

#only keep subjects that made at least a pre-defined number of entries proportionally to time-window-length
threshold = PARAM_MIN_ENTRIES_PERC * PARAM_TIME_INTERVAL_SINCE_BEGINNING

selected_subjects = app %>%
  group_by(vpvec) %>%
  summarise(entries = n()) %>%
  filter(entries >= threshold)

selected_subjects = unique(selected_subjects$vpvec)

app = app %>%
  filter(vpvec %in% selected_subjects)


# PREPARE MODELING --------------------------------------------------------

#only keep variable to be trend-analyzed over time
app = app %>%
  rename(variable = all_of(PARAM_VARIABLE) ) %>% 
  select(vpvec , total_time_range_days,hours_since_last_entry,hours_since_start,days_since_start,
         variable)


# FIT LINEAR REGRESSIONS --------------------------------------------------

regressions = app %>%
  nest(data=-vpvec) %>%
  mutate(
    model = map(data, ~lm(variable~hours_since_start,data=.x)),
    tidy = map(model,broom::tidy),
    glance = map(model,broom::glance),
    R2 = map_dbl(glance,
                 ~if(is.null(.x) || nrow(.x)==0) NA_real_ else .x$adj.r.squared),
    type = "regression"
    )


# FIT EXP. SATURATION CURVE -----------------------------------------------

fit_exp_plateau <- function(patient_data) {
  
  patient_data <- patient_data %>%
    dplyr::filter(!is.na(hours_since_start), !is.na(variable))
  
  if (nrow(patient_data) < 3) return(NULL)
  if (sd(patient_data$variable) == 0) return(NULL)
  
  t <- patient_data$hours_since_start
  y <- patient_data$variable
  
  y_start <- y[which.min(t)]
  y_end   <- y[which.max(t)]
  k0 <- 1 / (max(t) - min(t) + 1e-6)
  
  do_fit <- function(formula, start) {
    withCallingHandlers(
      tryCatch(
        nls(
          formula,
          data = patient_data,
          start = start,
          control = nls.control(maxiter = 200, warnOnly = TRUE)
        ),
        error = function(e) NULL
      ),
      warning = function(w) {
        if (grepl("minFactor", conditionMessage(w), fixed = TRUE)) {
          invokeRestart("muffleWarning")
          stop("nls minFactor warning treated as failure")
        }
      }
    )
  }
  
  if (y_end >= y_start) {
    start <- list(c = y_start, A = max(y) - y_start, k = k0)
    tryCatch(
      do_fit(variable ~ c + A * (1 - exp(-k * hours_since_start)), start),
      error = function(e) NULL
    )
  } else {
    start <- list(c = y_end, A = y_start - y_end, k = k0)
    tryCatch(
      do_fit(variable ~ c + A * exp(-k * hours_since_start), start),
      error = function(e) NULL
    )
  }
}

r2_nls <- function(model, data) {
  y <- data$variable
  y_hat <- predict(model, newdata = data)
  
  ss_res <- sum((y - y_hat)^2)
  ss_tot <- sum((y - mean(y))^2)
  
  1 - ss_res / ss_tot
}

safe_tidy   <- purrr::possibly(broom::tidy,   otherwise = tibble())
safe_glance <- purrr::possibly(broom::glance, otherwise = tibble())

sat_curves <- app %>%
  nest(data = -vpvec) %>%
  mutate(
    model  = map(data, ~ possibly(fit_exp_plateau, otherwise = NULL)(.x)),
    tidy   = map(model, ~ if (is.null(.x)) tibble() else safe_tidy(.x)),
    glance = map(model, ~ if (is.null(.x)) tibble() else safe_glance(.x)),
    R2 = map2_dbl(
      model, data,
      ~ if (is.null(.x)) NA_real_ else r2_nls(.x, .y)
    ),
    type   = "saturation"
  )

#COMBINE RESULTS
results = rbind(regressions , sat_curves)


# SORT BY BETTER MODEL DEPENDING ON R^2 ----------------------------------------

results = results %>%
  group_by(vpvec) %>%
  arrange(desc(R2)) %>%
  slice_head(n=1)

#filter out models that didn't converge
results = results %>%
  filter(!is.na(R2) & !is.nan(R2))

#sort subjects alphabetically and better model on top
results = results %>%
  arrange(vpvec,desc(R2))

#prepare adding predictions to each participant (for plotting)
results = results %>%
  mutate(
    time_min = map(data , ~min(.x$hours_since_start)),
    time_max = map(data , ~max(.x$hours_since_start)),
  )

# PLOT --------------------------------------------------------------------

vpvecs = unique(results$vpvec)
for(vpvec_it in vpvecs) {
  df = filter(results, vpvec == vpvec_it)
  data = df$data[[1]]
  n_rows = nrow(df)
  
  if(n_rows==0) next
  model = df$model[[1]]
  predictions = data.frame(hours_since_start=seq(df$time_min[[1]] , df$time_max[[1]], by=1))
  predictions$variable = predict(model , newdata = predictions)
  plot = data %>%
    ggplot(aes(x=hours_since_start/24 , y=variable))+
    geom_point(stroke=0.2,shape=1,size=0.6,position=position_jitter(height = 0.1 , width=0.0))  +
    geom_line(linewidth = 0.1,color="darkgrey")+
    geom_line(data=predictions,linewidth = 0.3)+
    plot_template_theme +ylab(PARAM_VARIABLE)+xlab("Time (Days)") + 
    labs(title=paste0(PARAM_VARIABLE,": Subject ",vpvec_it))+
    theme(
      axis.title = element_text(size = 4),
      plot.title = element_text(size = 4),
      axis.text.x  = element_text(size=3),
      axis.text.y = element_text(size=3)
    )
    
  if(n_rows==2) {
    model = df$model[[2]]
    predictions = data.frame(hours_since_start=seq(df$time_min[[2]] , df$time_max[[2]], by=1))
    predictions$variable = predict(model , newdata = predictions)
    
    plot = plot + 
      geom_line(data=predictions,linewidth = 0.3,linetype = "dashed",color="grey50")
  }
  ggsave(file = file.path(res_path,paste0(PARAM_VARIABLE,"_series_",vpvec_it,".jpg")),width = 5,
         height = 3)
}


