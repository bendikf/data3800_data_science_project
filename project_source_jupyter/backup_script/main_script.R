# 0. Load libraries and set seed: ----------------------------------------------

library(dplyr)
library(tidyr)
library(stringr)
library(lme4)

# Set seed to ensure reproducibility:
set.seed(3800)

# 1. Source data files ---------------------------------------------------------

lice_per_fish  <- readr::read_csv("data/lakselus_per_fisk.csv")
lice_treatment <- readr::read_csv("data/tiltak_mot_lakselus.csv")

# 2. Data wrangling ------------------------------------------------------------

# Replace spaces with underscores in all column names and make lowercase: ======
list(lice_per_fish = lice_per_fish,
     lice_treatment = lice_treatment) %>% 
  lapply(function(df) {
    rename_all(df, ~tolower(.)) %>%                 # Change to lowercase.
    rename_all(.,  ~str_replace_all(., " ", "_"))   # Spaces to underscores.
  }) %>% 
  list2env(envir = .GlobalEnv)  # Save to global environment.

# Prepare data frame 'lice_treatment' for joining: =============================
treatment <- lice_treatment %>% 
  select(uke, år, lokalitetsnavn, tiltak) %>% 
  
  # Remove duplicate rows:
  distinct() %>% 
  
  # Give each of the treatment categories its own boolean column:
  mutate(value = TRUE) %>% 
  pivot_wider(
    names_from = tiltak,  # Column to be split.
    values_from = value,
    values_fill = FALSE)

# Join the dataframe 'lice_treatment' to 'lice_per_fish': ======================
working_df <- left_join(lice_per_fish,
                        treatment) %>% 
  
  # Remove rows with no salmon lice counts:
  subset(har_telt_lakselus == "Ja") %>% 
  
  # Fill NAs in the newly added boolean columns with FALSE:
  mutate_if(is.logical, coalesce, FALSE) %>% 
  
  # Change week 53 to week 52 for all records:
  mutate(uke = if_else(uke == 53, 52, uke)) %>% 
  
  # Add variables for cyclic time:
  {
    # Assign local variable 'angle':
    angle = (2 * pi * .$uke) / 52 
    
    # Calculate sine and cosine transformed values:
    mutate(., sin = -sin(angle),
              cos = -cos(angle))
  } %>% 
  
  # Create the log transformed response variable:
  {
    const <- 0.001  # Add small constant to handle zeros.
    
    # NB! log() in R refers to the natural logarithm.
    mutate(., log_response = log(voksne_hunnlus + const))
  } %>% 

  # Arrange rows chronologically:
  arrange(år, uke, lokalitetsnummer)

# Identify all rows containing NA values: ======================================
incomplete <- working_df[!complete.cases(working_df),]

# Fill NAs with correct values based on shared site IDs: =======================
working_df <- working_df %>% 
  group_by(lokalitetsnummer) %>%  
  
  {
    # Local variable containing the names of affected columns:
    incomplete_cols <- colnames(.)[colSums(is.na(.)) > 0]
    
    # Fill NA values where possible:
    mutate(., across(all_of(incomplete_cols), ~ first(na.omit(.))))
  } %>% 
  ungroup()

# Remove rows which still contain NA values: ===================================
working_df <- working_df %>% na.omit()
  
# 3. Analysis: -----------------------------------------------------------------

# Split the data into test and training sets: ==================================
sample_index <- sample(seq_len(nrow(working_df)),
                       size = floor(0.8 * nrow(working_df)),  # 80 / 20 split.
                       replace = FALSE)

train <- working_df[ sample_index,]  # Include given rows.
test  <- working_df[-sample_index,]  # Exclude given rows.

# Fit the Linear Mixed Model: ==================================================
model <- lmer(log_response ~ sjøtemperatur +
                             sjøtemperatur * cos +
                             sjøtemperatur * sin +
                             sjøtemperatur * lat +
                             cos * lat +
                             sin * lat +
                             (1 | lokalitetsnummer),
              data = train)

# Apply model to the test set: =================================================
test <- test %>% 
  # Save the fitted response to a new column:
  mutate(fitted_log_response = predict(model, 
                                       newdata = test,
                                       allow.new.levels = TRUE),
         
         # Back-transform the fitted response to avg. count:
         fitted_voksne_hunnlus = exp(fitted_log_response) - 0.01)

# Calculate metrics for model performance evaluation: ==========================
rmse_value <- Metrics::rmse(test$fitted_log_response,  # Root Mean Square Error.
                            test$log_response)
mae_value  <- Metrics::mae(test$fitted_log_response,   # Mean Absolute Error.
                           test$log_response)
r_squared  <- MuMIn::r.squaredGLMM(model)              # Approximated R squared.

# Define variable fixed effects: ===============================================
# NB! Sea temperature is always included.
predictors <- c(
  "lat",
  "cos",
  "sin",
  "sjøtemperatur * lat",
  "sjøtemperatur * cos",
  "sjøtemperatur * sin",
  "cos * lat",
  "sin * lat"
)

# Generate combinations of predictors: =========================================
predictor_combinations <- 
  lapply(1:length(predictors), 
         function(n) {
           combinations <- gtools::combinations(length(predictors), 
                                                n, 
                                                as.character(predictors))
           lapply(1:nrow(combinations), 
                  function(i) combinations[i, ])})

# Simplify the data structure: =================================================
predictor_combinations <- unlist(predictor_combinations, 
                                 recursive = FALSE)

# Function to remove strings that are substrings of another string: ============
remove_substrings <- function(lst) {
  lst[!vapply(seq_along(lst), 
              function(i) any(grepl(lst[i], 
                                    lst[-i])), 
              logical(1))]
}

# Apply the function to the combinations of predictors: ========================
predictor_combinations <- predictor_combinations %>% 
  
  # Remove substrings:
  lapply(remove_substrings) %>% 
  
  # Remove duplicate list entries:
  unique()

# Create a list to store model results: ========================================
models <- list()

# Iterate through the combinations list and fit models for each combination: ===
# Grab a cup of coffee, this will take a little while!
for (combination in predictor_combinations) {
  
  # Specify the formula based on the list of combinations:
  formula <- as.formula(paste("log_response ~ sjøtemperatur +
                                              (1 | lokalitetsnavn) +",
                              paste(combination, collapse = " + ")))
  
  # Run the model:
  model <- lmer(formula = formula, data = train)
  
  # Save the model to the list we created:
  models[[paste(c("sjøtemperatur", 
                  combination), collapse = " + ")]] <- model
}

# Create the data frame to store the evaluation metrics: =======================
model_evaluation <- data.frame(model = names(models),
                               rmse  = NA,
                               mae   = NA,
                               r2m   = NA,
                               r2c   = NA)

for (i in seq_along(models)) {
  
  # Run the model through the test set:
  prediction <- predict(models[[i]],
                        newdata = test,
                        allow.new.levels = TRUE)
  
  # Calculate RMSE:
  model_evaluation[i, "rmse"] <- Metrics::rmse(prediction,
                                               test$log_response)
  # Calculate MAE:
  model_evaluation[i, "mae"] <- Metrics::mae(prediction,
                                             test$log_response)
  # Calculate R^2:
  model_evaluation[i, c("r2m", "r2c")] <- MuMIn::r.squaredGLMM(models[[i]]) %>% 
    as.vector()
}

# Find best model according to all evaluation metrics: =========================
model_evaluation %>%
  filter(rmse == min(rmse))  # Find minimum RMSE value.

model_evaluation %>%
  filter(mae == min(mae))    # Find minimum MAE value.

model_evaluation %>%
  filter(r2m == max(r2m))    # Find maximum R^2m value.

model_evaluation %>%
  filter(r2c == max(r2c))    # Find maximum R^2c value.

# 4. Plotting: -----------------------------------------------------------------

# Create a visualisation of the cyclically transformed time variable: ==========
working_df %>% 
  select(uke, sin, cos) %>% 
  arrange(uke) %>% 
  {
    # Specify where to save the finished plot:
    pdf("figures/cyclic_time.pdf",
        width  = 8,
        height = 5)
    
    # Adjust padding:
    par(mar = c(4, 4, 2, 2))
    
    attach(.)  # Lets me select columns by name without indexing.
    
    # Plot the sin curve:
    plot(sin~uke, 
         type = "l", 
         lwd = 2, 
         cex.lab = 1.2,
         col = "orange",
         ylab = "Yearly oscillation", 
         xlab = "Week number")
    
    # Plot the cos curve:
    lines(cos~uke, 
          lwd = 2, 
          col = "dimgray")
    
    # Add a legend:
    legend("topleft", 
           inset = c(0.05,0.08),
           legend = c("sin -1", "cos -1"), 
           col = c("orange", "dimgray"), 
           lwd = 2, 
           cex = 1.2)
    
    detach(.)  # Undo the previous attach() command.
    
    # Save the plot to PDF:
    dev.off()
  }

# Histogram of the residuals: ==================================================
# Find the best model (any metric will do, as they all agree):
model_evaluation %>%
  filter(r2c == max(r2c)) %>% select(model)

hist_model <- models[["sjøtemperatur_cos * lat_sin * lat_sjøtemperatur * cos_sjøtemperatur * lat_sjøtemperatur * sin"]]

hist(residuals(hist_model), 
     freq = FALSE,
     xlab = "Residuals",
     main = "Distribution of the residuals")

# Right-skewed distribution: ===================================================

# Generate a sequence of values based on the fitted distribution
x <- seq(0, max(working_df$voksne_hunnlus + 0.01), length.out = 1000)

# Plot histogram of the data
hist(working_df$voksne_hunnlus + 0.01, freq = FALSE, xlim = c(0, max(2)), 
     breaks = 200,
     main = "Fitted Log-Normal distribution",
     xlab = "Distribution")

# Add the fitted log-normal density curve
curve(dlnorm(x, meanlog = fit$estimate["meanlog"], sdlog = fit$estimate["sdlog"]), 
      col = "blue", lwd = 2, add = TRUE)
