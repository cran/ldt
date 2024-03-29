% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sur.R
\name{search.sur}
\alias{search.sur}
\title{Create a Model Set for SUR Models}
\usage{
search.sur(
  data = get.data(),
  combinations = get.combinations(),
  metrics = get.search.metrics(),
  modelChecks = get.search.modelchecks(),
  items = get.search.items(),
  options = get.search.options(),
  searchSigMaxIter = 0,
  searchSigMaxProb = 0.1
)
}
\arguments{
\item{data}{A list that determines data and other required information for the search process.
Use \code{\link[=get.data]{get.data()}} function to generate it from a \code{matrix} or a \code{data.frame}.}

\item{combinations}{A list that determines the combinations of endogenous and exogenous variables in the search process.
Use \code{\link[=get.combinations]{get.combinations()}} function to define it.}

\item{metrics}{A list of options for measuring performance. Use \link{get.search.metrics} function to get them.}

\item{modelChecks}{A list of options for excluding a subset of the model set. Use \link{get.search.modelchecks} function to get them.}

\item{items}{A list of options for specifying the purpose of the search. Use \link{get.search.items} function to get them.}

\item{options}{A list of extra options for performing the search. Use \link{get.search.options} function to get them.}

\item{searchSigMaxIter}{Maximum number of iterations in searching for significant coefficients. Use 0 to disable the search.}

\item{searchSigMaxProb}{Maximum value of type I error to be used in searching for significant coefficients. If p-value is less than this, it is interpreted as significant.}
}
\value{
A nested list with the following members:
\item{counts}{Information about the expected number of models, number of estimated models, failed estimations, and some details about the failures.}
\item{results}{A data frame with requested information in \code{items} list.}
\item{info}{The arguments and some general information about the search process such as the elapsed time.}

Note that the output does not contain any estimation results, but minimum required data to estimate the models (Use \code{summary()} function to get the estimation).
}
\description{
Use this function to create a Seemingly Unrelated Regression model set and search for the best models (and other information) based on in-sample and out-of-sample evaluation metrics.
}
\examples{
num_y <- 2L # number of equations
num_x_r <- 3L # number of relevant explanatory variables
num_x_ir <-
  10 # (relatively large) number of irrelevant explanatory variables
num_obs = 100  # number of observations

# create random data
sample <- sim.sur(sigma = num_y, coef = num_x_r, nObs = num_obs)
x_ir <- matrix(rnorm(num_obs * num_x_ir), ncol = num_x_ir) # irrelevant data

# prepare data for estimation
data <- data.frame(sample$y, sample$x, x_ir)
colnames(data) <- c(colnames(sample$y), colnames(sample$x), paste0("z", 1:num_x_ir))

# Use systemfit to estimate and analyse:
exp_names <- paste0(colnames(data)[(num_y + 1):(length(colnames((data))))], collapse = " + ")
fmla <- lapply(1:num_y, function(i) as.formula(paste0("Y", i, " ~ -1 + ", exp_names)))
fit <- systemfit::systemfit(fmla, data = data, method = "SUR")
summary(fit)

# You can also use this package estimation function:
fit <- estim.sur(data = get.data(data, endogenous = num_y, addIntercept = FALSE))
print(fit)

# Alternatively, You can define an SUR model set:
x_sizes = c(1:3) # assuming we know the number of relevant explanatory variables is less than 3
num_targets = 2
metric_options <- get.search.metrics(typesIn = c("sic")) # We use SIC for searching
search_res <- search.sur(data = get.data(data, endogenous = num_y, addIntercept = FALSE),
                         combinations = get.combinations(numTargets = num_targets,
                                                         sizes = x_sizes,
                                                         innerGroups = list(c(1), c(2))),
                         metrics = metric_options)
print(search_res)

# Use summary function to estimate the best models:
search_sum <- summary(search_res)

# Print the best model:
print(search_sum$results[[2]]$value)
#   see 'estim.sur' function

# Using a step-wise search to build a larger model set:
x_sizes_steps = list(c(1, 2, 3), c(4))
counts_steps = c(NA, 7)
search_step_res <- search.sur(data = get.data(data, endogenous = num_y, addIntercept = FALSE),
                              combinations = get.combinations(numTargets = num_targets,
                                                              sizes = x_sizes_steps,
                                                              stepsNumVariables = counts_steps,
                                                              innerGroups = list(c(1,2))),
                              metrics = metric_options)
# combinations argument is different

print(search_step_res)

}
\seealso{
\link{estim.sur}
}
