

#' Creates JSON Data for an \code{LDTSurvey} Project
#'
#' @param data A list of Variables with consistent frequency. Use \code{Variable}
#'  function. Target is the one with 'role:target; field or if missing, the first variable.
#' @param description A list of string arrays that provides basic information. Each array
#' provides 4 elements: 1. title of the project, 2. a short description in plain text,
#' 3. a longer description in mark-down format, and, 4. culture-name of the information.
#' The first array is the default and culture-name must be unique.
#' @param relatedIds ID of the related projects (e.g., this can be a project with
#' the same data but in another frequency).
#' @param survey_IsEnabled If \code{FALSE}, users cannot submit prediction.
#' @param survey_RulesChange If \code{TRUE}, owner can change the survey rules in the future edits.
#' @param survey_MaxHorizon Prediction horizons. E.g., 2 means a users can submit
#' her prediction for the next 2 periods. It can be 1 to 5.
#' @param survey_MinRequired Required minimum number of data points to be predicted by a user.
#' @param survey_showAI If \code{TRUE}, user must submit her prediction first, before
#' being able to see any automatic algorithm-based forecast
#' @param survey_showUser If \code{TRUE}, user must submit her prediction first, before
#' being able to see any other user-based prediction.
#' @param survey_RestrictTo A list of e-mails for restricting access
#' (see \code{survey_RestrictType}). Leave it empty for a public page.
#' Otherwise, don't forget to add your email or you cannot submit prediction.
#' @param survey_RestrictType Type of the restriction (see \code{survey_RestrictTo}).
#' \code{view} means only the permitted users can view the page. \code{submit} means
#' everyone can view, but the permitted users can submit prediction. \code{none} means
#' no restriction (use it for communication purposes).
#' @param survey_EndConditionOn Determines the type of the condition to end a survey
#' automatically (see \code{survey_EndCondition}).
#' @param survey_EndConditionValue Determines a condition to end a survey automatically.
#' E.g., if \code{survey_EndConditionOn} is \code{hourOfDay} and this value is 20, the
#' session will end (and users cannot submit predictions) on and
#' after 20:00 (based on Gregorian calendar and UTC).
#' @param forecast_IsEnabled If \code{TRUE}, an automatic algorithm-based forecast
#' is reported (see also \code{survey_showAI}).
#' @param forecast_External An array for providing an external forecast up to
#' \code{survey_MaxHorizon}. A forecast should be 'up' or 'down' for a direction forecast,
#' a number for a point forecast, and 'dist:distribution-name(comma-separated parameters)'
#' for a distribution forecast (e.g., 'normal(0,1)')
#' @param forecast_ExternalDesc A short description on what is provided in
#' \code{forecast_External} (e.g., the name of the numerical method)
#'
#' @export
#' @return The JSON content
CreateProject <- function(data,
                          description = list(c(
                            "Title", "Short Description",
                            "Long Description", "en"
                          )),
                          relatedIds = list(),
                          survey_IsEnabled = TRUE,
                          survey_RulesChange = TRUE,
                          survey_MaxHorizon = 2,
                          survey_MinRequired = 1,
                          survey_showAI = FALSE,
                          survey_showUser = FALSE,
                          survey_RestrictTo = list(),
                          survey_RestrictType = c("none", "view", "submit"),
                          survey_EndConditionOn = c(
                            "none", "dayOfYear", "dayOfHalfYear", "dayOfQuarter",
                            "dayOfMonth", "dayOfWeek", "hourOfDay"
                          ),
                          survey_EndConditionValue = 0,
                          forecast_IsEnabled = TRUE,
                          forecast_External = list(),
                          forecast_ExternalDesc = "") {
  # TODO:
  #  data_IsDiscrete = FALSE
  #  data_MinData = -Inf
  #  data_MaxData = Inf

  if (is.null(data) || length(data) == 0) {
    stop("Data is missing. Use 'Variable(...)' function to provide it.")
  }

  description <- as.list(description)
  for (e in description) {
    if (length(e) != 4) {
      stop("Each element in 'description' must have 4 items:
       Title, Short Description, Long Description, Culture-Info")
    }
  }

  relatedIds <- as.list(relatedIds)
  for (e in relatedIds) {
    if (IsGuidValid(e) == FALSE) {
      stop(paste0("The given item in 'relatedIds' is not a valid id. value=", e))
    }
  }

  survey_MaxHorizon <- as.integer(survey_MaxHorizon)
  if (survey_MaxHorizon < 1 || survey_MaxHorizon > 5) {
    stop("Invalid 'survey_MaxHorizon'. It must be in [1,5]")
  }

  survey_MinRequired <- as.integer(survey_MinRequired)
  if (survey_MinRequired < 1 || survey_MinRequired > survey_MinRequired) {
    stop("Invalid 'survey_MinRequired'. It must be in [1,'survey_MaxHorizon']")
  }

  survey_RestrictTo <- as.vector(survey_RestrictTo)
  for (e in survey_RestrictTo) {
    if (IsEmailValid(e) == FALSE) {
      stop(paste0("The given item in 'survey_RestrictTo' is not a valid email. value=", e))
    }
  }

  survey_RestrictType <- match.arg(tolower(survey_RestrictType), c("none", "view", "submit"))

  survey_EndConditionOn <- match.arg(
    tolower(survey_EndConditionOn),
    tolower(c(
      "none", "dayOfYear", "dayOfHalfYear", "dayOfQuarter",
      "dayOfMonth", "dayOfWeek", "hourOfDay"
    ))
  )
  survey_EndConditionValue <- as.integer(survey_EndConditionValue)

  forecast_External <- as.vector(forecast_External)
  if (is.null(forecast_External) == FALSE && length(forecast_External) > 0) {
    # TODO: check external forecasts
    # for (e in forecast_External)
    #   if (is.numeric(e) == FALSE && e != "up" || e != "down" || e )
  }

  e <- c(as.list(environment()))
  e$data <- sapply(data, function(v) VariableToString(v))
  e$fileName <- NULL
  e$e <- NULL

  res <- jsonlite::toJSON(e, pretty = FALSE, digits = NA, auto_unbox = TRUE)

  #f <- paste0(fileName, ".json")
  #if (file.exists(f) && override == FALSE) {
  #  stop(paste0("File (", f, ") exists."))
  #}
  #write(res, file = f)
  #print(paste0("File created: ", getwd(), "/", f))

  return(res)
}
