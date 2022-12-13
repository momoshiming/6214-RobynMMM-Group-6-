
##################################################################
###################################Install Robyn##################

#reticulate::install_miniconda()
#remotes::install_github("facebookexperimental/Robyn/R")

#library(reticulate)
#virtualenv_create("r-reticulate-cassandra")
#use_virtualenv("r-reticulate-cassandra", required = TRUE)
#py_install("nevergrad", pip = TRUE)
#py_config()
#reticulate::py_config()


################################################################
############################ Step 0: Setup environment##########
library(Robyn)
set.seed(123)
Sys.setenv(R_FUTURE_FORK_ENABLE = "true")
options(future.fork.enable = TRUE)
library(reticulate)
use_virtualenv("r-reticulate-cassandra", required = TRUE)


################################################################
#### Step 1: Load data##########################################

data_week <- read.csv('data_week_final.csv')
colnames(data_week)[colnames(data_week) == 'week'] <- 'date'
data_week$date <- as.Date(data_week$date, format='%Y-%m-%d')
data_week$year <- substr(data_week$date, 1, 4)
data_week$year <- as.factor(data_week$year)


data_2021<-data_week[data_week$date >= '2021-01-01' & data_week$date <= '2022-04-30',]
#head(data_mmm)

data("dt_prophet_holidays")
#head(dt_prophet_holidays)

## # Directory where you want to export results to (will create new folders) 
robyn_object <- "~/Desktop/Fall 2022/MISM 6214/project/1/model/MyRobyn.RDS"


################################################################
#### Step 2a: For first time user: Model specification in 4 steps

#### 2a-1: First, specify input variables
## Create InputCollect
InputCollect <- robyn_inputs(
  dt_input = data_2021,
  dt_holidays = dt_prophet_holidays,
  
  #1.set all variables    
  ###set dependent variables
  date_var = "date",
  dep_var = "sales", # there should be only one dependent variable
  dep_var_type = 'revenue', #here the type of sales is revenue instead of conversion
  
  ###set time variables, here we see four different types of variables
  prophet_vars = c("trend", "season",'holiday'), #see the different timeline output of model
  prophet_signs = c('default','default','default'), #we want robin to look the relation by itself,
  # so we don't need to input negative or positive unless we know the trend exactly
  prophet_country = "US", #we focus on the US and want to see effects on it
  
  ###set context variables
  context_vars = c("unemployment", "temperature",'GDP','Income'), 
  # e.g. competitors, discount, unemployment etc
  context_signs = c("default", "default","default", "default"),
  #control the signs of coefficients for baseline variables
  factor_vars = c("unemployment","temperature",'GDP','Income'), # force variables in context_vars or organic_vars to be categorical
  
  ###set media variables
  paid_media_vars = c("facebook_newsfeed_impressions", "youtube_brand_impressions", "search_clicks", "youtube_performance_impressions", "newspaper_readership","tv_gross_rating_points"), 
  paid_media_signs = c("positive","positive","positive","positive","positive","positive"),
  paid_media_spends = c("facebook_newsfeed_spend", "youtube_brand_spend", "search_spend", "youtube_performance_spend", "newspaper_spend","tv_spend"), 
  
  ###set organic_variables
  #organic_vars = c("newsletter"),
  #organic_signs = c("positive") 

  #2. set model parameters  
  ##set core for parallel computing
  cores=4,
  
  ##set rolling window start
  window_start = "2021-01-01",
  window_end = "2022-04-30",
  
  ##set model core features
  adstock = "weibull_cdf",# geometric, weibull_cdf or weibull_pdf. 
  #iterations=2000, #number of allowed iterations per trail
  intercept_sign='non_negative',
  nevergrad_algo='TwoPointsDE', #recommended algorithm for Nevergrad
  #trials=1 #number of allowed trails
)
print(InputCollect)

#### 2a-2: Second, define and add hyperparameters

## Guide to setup & understand hyperparameters

## 1. IMPORTANT: set plot = TRUE to see helper plots of hyperparameter's effect in transformation
plot_adstock(plot = FALSE)
plot_saturation(plot = FALSE)

# Example hyperparameters ranges for Geometric adstock
hyper_names(adstock = InputCollect$adstock, all_media = InputCollect$all_media)

#Set hyperparamaters
hyperparameters <- list(
  facebook_newsfeed_spend_alphas = c(0.5, 3),
  facebook_newsfeed_spend_gammas = c(0.3, 1),
  facebook_newsfeed_spend_shapes= c(1e-04, 2),
  facebook_newsfeed_spend_scales =c(0, 0.1),
  
  newspaper_spend_alphas = c(0.5, 3),
  newspaper_spend_gammas = c(0.3, 1),
  newspaper_spend_shapes= c(1e-04, 2),
  newspaper_spend_scales=c(0, 0.1),
  
  search_spend_alphas = c(0.5, 3),
  search_spend_gammas = c(0.3, 1),
  search_spend_shapes= c(1e-04, 2),
  search_spend_scales=c(0, 0.1),
  
  tv_spend_alphas = c(0.5, 3),
  tv_spend_gammas = c(0.3, 1),
  tv_spend_shapes= c(1e-04, 2),
  tv_spend_scales=c(0, 0.1),
  
  youtube_brand_spend_alphas = c(0.5, 3),
  youtube_brand_spend_gammas = c(0.3, 1),
  youtube_brand_spend_shapes= c(1e-04, 2),
  youtube_brand_spend_scales=c(0, 0.1),
  
  youtube_performance_spend_alphas = c(0.5, 3),
  youtube_performance_spend_gammas = c(0.3, 1),
  youtube_performance_spend_shapes= c(1e-04, 2),
  youtube_performance_spend_scales=c(0, 0.1)
)

#### 2a-3: Third, add hyperparameters into robyn_inputs()
InputCollect <- robyn_inputs(InputCollect = InputCollect, hyperparameters = hyperparameters)
print(InputCollect)


################################################################
#### Step 3: Build initial model
## Run all trials and iterations. Use ?robyn_run to check parameter definition
## Set output
OutputModels <- robyn_run(
  InputCollect = InputCollect, # feed in all model specification
  # cores = NULL, # default to max available
  # add_penalty_factor = FALSE, # Untested feature. Use with caution.
  outputs = FALSE, # outputs = FALSE disables direct model output - robyn_outputs()
  iterations=2000,
  trials=5
)
print(OutputModels)

#Put models in files
OutputCollect <- robyn_outputs(
  InputCollect, OutputModels,
  pareto_fronts = "auto",
  # calibration_constraint = 0.1, # range c(0.01, 0.1) & default at 0.1
  csv_out = "pareto", # "pareto" or "all"
  clusters = TRUE, # Set to TRUE to cluster similar models by ROAS. See ?robyn_clusters
  plot_pareto = TRUE, # Set to FALSE to deactivate plotting and saving model one-pagers
  plot_folder = robyn_object # path for plots export
)

################################################################
#### Step 5: Model refresh based on selected model##############

OutputCollect$allSolutions #get all model IDs in result
# Pick one of the models from OutputCollect to proceed
select_model <- "3_244_1"

ExportedModelOld <- robyn_save( 
  robyn_object = robyn_object, # model object location and name
  select_model = select_model, # selected model ID
  InputCollect = InputCollect, # all model input
  OutputCollect = OutputCollect # all model output
)

#Yes

# It can be any model, initial or a refresh model
data_refresh<-data_week[data_week$date >= '2021-01-03' & data_week$date <= '2022-07-01',]

?robyn_refresh

RobynRefresh <- robyn_refresh(
  robyn_object = robyn_object,
  dt_input = data_refresh,
  dt_holidays = dt_prophet_holidays,
  refresh_steps = 9,
  refresh_mode= 'auto',
  refresh_iters = 1000, # 1k is an estimation
  refresh_trials = 3,
  clusters=TRUE
)

# Check media summary for selected model
print(ExportedModelOld)

################################################################
#### Step 6: Get budget allocation based on the selected model above

# Predict without Budget Allocation
AllocatorCollect1 <- robyn_allocator(
  InputCollect = InputCollect,
  OutputCollect = OutputCollect,
  select_model = select_model,
  scenario = "max_historical_response",  #what's the most I can get in terms of revenue by changing the allocation of budgets
  channel_constr_low = c(0.5, 0.8, 0.5,0.8, 0.8, 0.8), 
  channel_constr_up = c(1.1, 1.5, 1.1, 1.4, 1.6, 1.7)
)
print(AllocatorCollect1)

# Predict with 1.3 million Budget Allocation
AllocatorCollect2 <- robyn_allocator(
  InputCollect = InputCollect,
  OutputCollect = OutputCollect,
  select_model = select_model,
  scenario = "max_response_expected_spend",
  channel_constr_low = c(0.5, 0.8, 0.5,0.8, 0.8, 0.8),
  channel_constr_up = c(1.1, 1.5, 1.1, 1.4, 1.6, 1.7),
  expected_spend = 1300000, # Total spend to be simulated
  expected_spend_days = 120, # Duration of expected_spend in days
  export = TRUE
)
print(AllocatorCollect2)
AllocatorCollect2$dt_optimOut





