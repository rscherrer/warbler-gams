# This script looks into the problems experienced by Claire when fitting GAMs.
# Done following the following tutorial:
# https://noamross.github.io/gams-in-r-course/

rm(list = ls())

library(tidyverse)
library(mgcv)
library(chron)

theme_set(theme_classic())

# Read the data
data <- read_csv("data.csv")

# Pre-processing
data <- data %>%
  filter(

    # Only positive age
    age_year > 0,

    # Remove unidentified sexes
    !is.na(SexEstimate)

  ) %>%
  mutate(

    # Name the sexes
    SexEstimate = if_else(SexEstimate == 0, "Female", "Male"),

    # Convert to categorical variables where needed
    across(c(BirdID, SexEstimate, newstat), as.factor),

    # Time variables
    CatchTime = chron(times = CatchTime),
    CatchTime_mins = 60 * 24 * as.numeric(times(CatchTime)),

    # Standardize the response variable
    sum_oxy_z = scale(sum_oxy)
  )

# Eyeball the data
data %>%
  ggplot(aes(x = age_year, y = sum_oxy_z, color = SexEstimate)) +
  geom_point() +
  facet_grid(newstat ~ SexEstimate) +
  theme(legend.position = "none")

# No discernable pattern overall. Let us fit the simplest GAM we can think of.

# The simplest model we can fit
fit <- gam(sum_oxy_z ~ s(age_year), data = data, family = gaussian, method = "REML")

# The model has many basis functions...
coef(fit)

# ... but the effect of age is not significant and effectively linear (EDF close to 1)
summary(fit)

# It looks like it too
plot(fit, residuals = TRUE, pages = 1)

# Let's split by sex
fit <- gam(sum_oxy_z ~ s(age_year, by = SexEstimate) + SexEstimate, data = data, family = gaussian, method = "REML")

# Still linear and flat trends with age
summary(fit)

# Yup, looks pretty flat
plot(fit, residuals = TRUE, pages = 1, all.terms = TRUE, seWithMean = TRUE, shift = coef(fit)[1])

# Residuals look okay
gam.check(fit)

# Look out for concurvity
concurvity(fit, full = TRUE)
concurvity(fit, full = FALSE) # looks okay

# Let us include both sex and status (categorical variables to account for)
# but as factor-smooths, i.e. we only want to correct for them.

# Fit the new model
fit <- gam(sum_oxy_z ~ s(age_year, interaction(SexEstimate, newstat), bs = "fs"), data = data, family = gaussian, method = "REML")

# Still flat and not significant across the board
summary(fit)
plot(fit, residuals = TRUE, pages = 1, all.terms = TRUE, seWithMean = TRUE, shift = coef(fit)[1])

# We need to keep in mind that there were repeated measures here.
# The factor we want to take into account is bird identity. We may want to model
# it as a random effect. Let's see how many birds and how many observations
# we have per bird.

# Have a look
tibble(n = table(data$BirdID)) %>%
  ggplot(aes(x = n)) +
  geom_bar() +
  xlab("Observations per individual") +
  ylab("Count") +
  ggtitle(paste0("No. birds = ", length(unique(data$BirdID)), ", no. observations = ", nrow(data)))

# So we have many many birds and not many observations per bird. This tells us
# that fitting a random-slope model, which would account for differences in
# trends among birds, is not a good idea, simply because we do not have enough
# data (or more importantly repeated measures for each bird) to measure that.
# It makes more sense to treat bird identity as a random intercept.

# Augment the model with random intercepts
fit <- gam(

  sum_oxy_z ~ s(age_year, interaction(SexEstimate, newstat), bs = "fs") +
    s(BirdID, bs = "re"),

  data = data, family = gaussian, method = "REML"

)

# Check out the results
summary(fit)
plot(fit, pages = 1)

# Let us now look at the relationship of our response variable with other
# potential explanatory continuous variables.

# Function to plot a given predictor against the response
PLOTFUN <- function(data, x, y = "sum_oxy_z", color_by = NULL, by_categ = TRUE) {

  # data: the data set
  # x: the predictor
  # y: the response
  # color_by: optional extra predictor to color by
  # by_categ: whether to split by category (sex and status)

  # Plot
  plot <- data %>%
    ggplot(aes(x = get(x), y = get(y))) +
    geom_point() +
    geom_smooth() +
    xlab(x) +
    ylab(y)

  # Split by category if needed
  if (by_categ) plot <- plot + facet_grid(newstat ~ SexEstimate)

  # Add color if needed
  if (!is.null(color_by)) plot <- plot + aes(color = get(color_by)) + labs(color = color_by)

  return(plot)

}

# Explore relationships with other variables
data %>% PLOTFUN("lifespan")
data %>% PLOTFUN("TQ")
data %>% PLOTFUN("avg_invert")
data %>% PLOTFUN("CatchTime_mins")
data %>% PLOTFUN("birthyear")

# Looks like there may be some consistent effect of some of those variables,
# that may not be linear either. So, let's include them. That said, we know
# that some variables may be colinear (e.g. age, birth year and lifespan).
# Check that.

# Check colinearity with age (since it is our main variable of interest)
data %>% PLOTFUN("birthyear", y = "age_year")
data %>% PLOTFUN("lifespan", y = "age_year")
data %>% PLOTFUN("TQ", y = "age_year")
data %>% PLOTFUN("avg_invert", y = "age_year")
data %>% PLOTFUN("CatchTime_mins", y = "age_year")

# Indeed age, birth year and lifespan are correlated, but mostly birth year
# and age (life span does not seem to be a problem).

# Check the colinearity between birth year and life span
data %>% PLOTFUN("birthyear", y = "lifespan")

# Those two variables seem to be telling similar stories, but not entirely.
# If anything, birth year may be redundant with age. That said, where we have
# not detected any trend of the response variable with age so far, from the
# plots above it looks like there is a dip towards towards more recent birth
# year that does not seem to be captured when looking at the effect of age.

# Repeat the plots, just to show the difference
data %>% PLOTFUN("age_year")
data %>% PLOTFUN("birthyear")

# So, it may be a good idea to keep birth year in. Even if it is colinear with
# age, there may be deviations from year to year due to some external
# circumstances, such as dry or wet years. Hence, to remove the problem of
# colinearity, it could be a good idea to model birth year (or the circumstances
# during that year) as a random effect, to add some wiggle room around the main
# effect of time (here captured by age). Just like for bird identity, there
# are many different birth years in the data (almost continuous) so let us go
# for a simple random intercept model. Let us also not forget to add the other
# continuous covariates.

# Fit the new model (from here on fitting starts to take a while...)
fit <- gam(

  sum_oxy_z ~ s(age_year, interaction(SexEstimate, newstat), bs = "fs") +
    s(lifespan) +
    s(CatchTime_mins) +
    s(TQ) +
    s(avg_invert, k = 3) +
    s(BirdID, bs = "re") +
    s(birthyear, bs = "re"),

  # Note: avg_invert has few unique values so keep the number of basis functions
  # low to avoid convergence problems.

  data = data %>% mutate(birthyear = as.factor(birthyear)),
  family = gaussian, method = "REML"

)

# Looks like some things are significant and (importantly) nonlinear
summary(fit)
plot(fit, pages = 1)

# Now we may want to include further interactions. For example, the continuous
# covariates may interact with age, our main variable of interest.

# Add interactions among covariates
fit <- gam(

  sum_oxy_z ~ s(age_year, interaction(SexEstimate, newstat), bs = "fs") +
    s(lifespan) +
    s(CatchTime_mins) +
    s(TQ) +
    s(avg_invert, k = 3) +
    ti(age_year, lifespan) +
    ti(age_year, CatchTime_mins) +
    ti(age_year, TQ) +
    ti(age_year, avg_invert) +
    s(BirdID, bs = "re") +
    s(birthyear, bs = "re"),

  data = data %>% mutate(birthyear = as.factor(birthyear)),
  family = gaussian, method = "REML"

)

# Check it out
summary(fit)
plot(fit, pages = 1)

# We could also allow for an interaction of the extra continuous covariates
# with the categorical variables. Or interactions among the covariates. Or
# higher-order interactions (other than pairwise). However, we are not hitting
# the limits of how complex of a model can be fitted to the data. For example,
# adding interactions between continuous covariates and categorical variables
# to the model above will result in an error (not shown). So, what do we do now?

# Maybe some of the interactions we have not yet included are obvious if we
# look at the data. So, let's visually explore and see if there is something
# obvious we have missed.

# Fish for potential interactions
data %>% PLOTFUN("age_year", color_by = "lifespan")
data %>% PLOTFUN("age_year", color_by = "CatchTime_mins")
data %>% PLOTFUN("age_year", color_by = "TQ")
data %>% PLOTFUN("age_year", color_by = "avg_invert")

# It does not look like there are any obvious differences among categories in
# the interaction between any continuous covariate with age. So, adding
# interactions with categorical variables may not be warranted.

# No obvious effects of interactions among continuous covariates on the response
data %>% PLOTFUN("lifespan", color_by = "CatchTime_mins", by_categ = FALSE)
data %>% PLOTFUN("lifespan", color_by = "TQ", by_categ = FALSE)
data %>% PLOTFUN("lifespan", color_by = "avg_invert", by_categ = FALSE)
data %>% PLOTFUN("CatchTime_mins", color_by = "TQ", by_categ = FALSE)
data %>% PLOTFUN("CatchTime_mins", color_by = "avg_invert", by_categ = FALSE)
data %>% PLOTFUN("TQ", color_by = "avg_invert", by_categ = FALSE)

# And no obvious higher-order interactions involving covariates and categories
data %>% PLOTFUN("lifespan", color_by = "CatchTime_mins")
data %>% PLOTFUN("lifespan", color_by = "TQ")
data %>% PLOTFUN("lifespan", color_by = "avg_invert")
data %>% PLOTFUN("CatchTime_mins", color_by = "TQ")
data %>% PLOTFUN("CatchTime_mins", color_by = "avg_invert")
data %>% PLOTFUN("TQ", color_by = "avg_invert")

# So, from here it is probably safe to ignore further interactions. We have
# the ones that matter the most (i.e. the ones involving our main predictor, age).
# Well, the research question is about extracting the precise trend with age
# while accounting for confounding variables. So, it sounds like we do not
# necessarily want to reduce our model to increase power in assessing the
# significance of the leftover terms. Instead, we want to keep everything in
# and look at the trend with age...

# ... which now appears to be significant
summary(fit)

# No effect involving age was significant other than the factor-smooth depending
# on sex and status. Note that there are many P-values computed,
# so this could be a false positive.

# Indeed the effect looks small
plot(fit, select = 1)

# Let us dig into this effect. Which groups go up, and which go down?

# Re-fit with separate smooths
fit <- gam(

  sum_oxy_z ~ SexEstimate * newstat +
    s(age_year, by = interaction(SexEstimate, newstat)) +
    s(lifespan) +
    s(CatchTime_mins) +
    s(TQ) +
    s(avg_invert, k = 3) +
    ti(age_year, lifespan) +
    ti(age_year, CatchTime_mins) +
    ti(age_year, TQ) +
    ti(age_year, avg_invert) +
    s(BirdID, bs = "re") +
    s(birthyear, bs = "re"),

  data = data %>% mutate(birthyear = as.factor(birthyear)),
  family = gaussian, method = "REML"

)

# Check out the results
summary(fit)

# The trend is closest to being detectable for subordinate females, but no
# group passes the significance threshold any longer. Let us merge dominants
# and subordinates together, since subordinates have much fewer data points.

# Remove status
fit1 <- gam(

  sum_oxy_z ~ SexEstimate * newstat +
    s(age_year, by = SexEstimate) +
    s(lifespan) +
    s(CatchTime_mins) +
    s(TQ) +
    s(avg_invert, k = 3) +
    ti(age_year, lifespan) +
    ti(age_year, CatchTime_mins) +
    ti(age_year, TQ) +
    ti(age_year, avg_invert) +
    s(BirdID, bs = "re") +
    s(birthyear, bs = "re"),

  data = data %>% mutate(birthyear = as.factor(birthyear)),
  family = gaussian, method = "REML"

)

# Still no signal left of a trend through time across any of the sexes
summary(fit1)

# If we now remove the interaction with sex entirely...
fit2 <- gam(

  sum_oxy_z ~ SexEstimate * newstat +
    s(age_year) +
    s(lifespan) +
    s(CatchTime_mins) +
    s(TQ) +
    s(avg_invert, k = 3) +
    ti(age_year, lifespan) +
    ti(age_year, CatchTime_mins) +
    ti(age_year, TQ) +
    ti(age_year, avg_invert) +
    s(BirdID, bs = "re") +
    s(birthyear, bs = "re"),

  data = data %>% mutate(birthyear = as.factor(birthyear)),
  family = gaussian, method = "REML"

)

# The trend through time still is not significant
summary(fit2)

# And we compare all three possibilities?
AIC(fit, fit1, fit2)

# The simplest model wins, which means that accounting for differences
# among sexes and statuses does not improve the fit of the model predictions
# to the data, everything else being equal and all other variables being
# taken into account. Hence, this suggests that we can safely extract a single
# tendency through time for the entire dataset to study the senescence of the
# response variable, i.e. the same trend applies to everyone. Let us show it.

# Pick the best model
fit <- fit2

# Function to find the level of a random effect closest to its average
get_avg_re_level <- function(fit, term) {

  # fit: the model fit
  # term: name of the random effect

  # Extract model coefficients
  x <- coef(fit)

  # Isolate coefficients corresponding to the specified term
  x <- x[str_detect(names(x), term)]

  # Find the random deviation closest to zero
  x <- x[which(abs(x) == min(abs(x)))]

  # Return the name of that level
  str_remove(names(x), paste0("s\\(", term, "\\)."))

}

# Find the levels of our random effects closest to the intercept
avg_BirdID <- get_avg_re_level(fit, "BirdID")
avg_birthyear <- get_avg_re_level(fit, "birthyear")

# Make a mock dataset for predictions
newdata <- data %>%
  mutate(
    across(
      c(birthyear, lifespan, CatchTime_mins, TQ, avg_invert),
      \(x) mean(x, na.rm = TRUE)
    ),
    BirdID = avg_BirdID,
    birthyear = avg_birthyear
  )

# Predict outcome
newdata <- newdata %>% mutate(pred = predict(fit, newdata))

# Show
newdata %>%
  ggplot(aes(x = age_year, y = pred, group = interaction(SexEstimate, newstat))) +
  geom_line(aes(color = SexEstimate, linetype = newstat)) +
  geom_point(data = data, aes(y = sum_oxy_z))

# Prepare a table of residuals
resids <- tibble(resid = fit$residuals, fitted = fit$fitted.values)

# Normality looks okay
resids %>%
  ggplot(aes(x = resid)) +
  geom_histogram()

# Also here
resids %>%
  ggplot(aes(sample = resid)) +
  stat_qq_line() +
  stat_qq()

# No strong pattern detected against fitted values
resids %>%
  ggplot(aes(x = fitted, y = resid)) +
  geom_point()
