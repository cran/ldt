




is.metric.pos.orien <- function(metric){
  if (metric %in% c("aucIn", "aucOut", "sign", "direction"))
    return(TRUE)
  return(FALSE)
}

#' Combine a List of \code{ldt.search} Objects
#'
#' @param list A list of \code{ldt.search} objects
#' @param method Method in objects
#'
#' @return the combined \code{ldtsearch} object
combine.search <- function(list, method) {
  if (length(list) == 1)
    return(list[[1]])

  cmb_inclusion <- function(A1, A2) {

    if(any((is.na(A1[, 1]) | is.nan(A1[, 1])) & A1[, 2] != 0) |
       any((is.na(A2[, 1]) | is.nan(A2[, 1])) & A2[, 2] != 0)) {
      stop("Error: NA or NaN found in 'mean' column where 'count' is not zero.")
    }

    result <- matrix(nrow = nrow(A1), ncol = 2)
    colnames(result) <- colnames(A1)
    rownames(result) <- rownames(A1)

    for (i in seq_len(nrow(A1))) {
      row_name <- rownames(A1)[i]
      if (row_name %in% rownames(A2)) {
        j <- which(rownames(A2) == row_name)
        total_count <- sum(A1[i,2], A2[j,2], na.rm = TRUE)
        weighted_mean <- sum(A1[i,1] * A1[i,2], A2[j,1] * A2[j,2], na.rm = TRUE) / total_count
      } else {
        total_count <- A1[i,2]
        weighted_mean <- A1[i,1]
      }
      result[i, 1] <- weighted_mean
      result[i, 2] <- total_count
    }
    result
  }

  cmb_cdf <- function(first, second) {
    # assuming columns are: mean, count, weights
    firstNames <- rownames(first)
    secondNames <- rownames(second)
    first[, 1] <- first[, 1] * first[, 3] # convert first column to weighted sum
    j <- 0
    for (n in secondNames) {
      j <- j + 1
      ind <- which(n == firstNames)
      if (length(ind) > 0) {
        ind <- ind[[1]]
        first[[ind, 1]] <- first[[ind, 1]] + second[[j, 1]] * second[[j, 3]]
        first[[ind, 2]] <- first[[ind, 2]] + second[[j, 2]]
        first[[ind, 3]] <- first[[ind, 3]] + second[[j, 3]]
      }
    }
    first[, 1] <- first[, 1] / first[, 3] # convert back to mean
    return(first)
  }

  cmb_extremebound <- function(first, second) {
    # assuming first is lower and second column is upper bounds
    firstNames <- rownames(first)
    secondNames <- rownames(second)
    j <- 0
    for (n in secondNames) {
      j <- j + 1
      ind <- which(n == firstNames)
      if (length(ind) > 0) {
        ind <- ind[[1]]
        first[[ind, 1]] <- min(first[[ind, 1]], second[[j, 1]])
        first[[ind, 2]] <- max(first[[ind, 2]], second[[j, 2]])
      }
    }
    return(first)
  }

  cmb_mixture <- function(first, second) {
    # assuming that columns are: mean, variance, skewness, kurtosis,count, sumWeights

    firstNames <- rownames(first)
    secondNames <- rownames(second)
    j <- 0
    for (n in secondNames) {
      j <- j + 1
      ind <- which(n == firstNames)
      if (length(ind) > 0) {
        ind <- ind[[1]]
        mix <- s.combine.stats4(
          list(
            mean = first[[ind, 1]], variance = first[[ind, 2]],
            skewness = first[[ind, 3]], kurtosis = first[[ind, 4]],
            count = first[[ind, 5]], weight = first[[ind, 6]]
          ),
          list(
            mean = second[[j, 1]], variance = second[[j, 2]],
            skewness = second[[j, 3]], kurtosis = second[[j, 4]],
            count = second[[j, 5]], weight = second[[j, 6]]
          )
        )
        first[ind, 1] <- mix$mean
        first[ind, 2] <- mix$variance
        first[ind, 3] <- mix$skewness
        first[ind, 4] <- mix$kurtosis
        first[ind, 5] <- mix$count
        first[ind, 6] <- mix$weight
      }
    }

    return(first)
  }


  result <- list[[1]]
  for (i in c(2:length(list))) {
    newR <- list[[i]]

    result$info$startTime <- min(newR$info$startTime, result$info$startTime)
    result$info$endTime <- max(newR$info$endTime, result$info$endTime)

    result$counts$expectedCount <- result$counts$expectedCount + newR$counts$expectedCount
    result$counts$searchedCount <- result$counts$searchedCount + newR$counts$searchedCount
    result$counts$failedCount <- result$counts$failedCount + newR$counts$failedCount
    if (is.null(result$counts$failedDetails))
      result$counts$failedDetails = list()

    for (f in newR$counts$failedDetails) {
      j <- 0
      added <- FALSE
      for (f0 in result$counts$failedDetails){
        j <- j + 1
        if (identical(f$message, f0$message)){
          added <- TRUE
          result$counts$failedDetails[[j]]$count = f0$count + f$count
          break
        }
      }

      if (added == FALSE){
        ind <- length(result$counts$failedDetails) + 1
      }
    }

    new_best_models <- newR$results[which(sapply(newR$results, function(item) item$typeName == "best model"))]
    new_best_coefs <-newR$results[which(sapply(newR$results, function(item) startsWith(item$typeName, "best item for")))]
    new_inclusions <- newR$results[which(sapply(newR$results, function(item) item$typeName == "inclusion"))]
    new_cdfs <- newR$results[which(sapply(newR$results, function(item) item$typeName == "cdf"))]
    new_mixtures <- newR$results[which(sapply(newR$results, function(item) item$typeName == "mixture"))]
    new_extremebounds <- newR$results[which(sapply(newR$results, function(item) item$typeName == "extreme bound"))]

    dealt_with <- list()
    to_be_added <- list()

    j <- 0
    for (item in result$results){
      j <- j + 1
      if (length(item) == 1 && is.na(item))
        next

      is_pos_oriented <- is.metric.pos.orien(item$evalName)
      eval_target_type <- paste0(item$evalName, "_", item$targetName, "_", item$typeName)

      if (item$typeName == "best model"){
        if (item$info == 0 && !eval_target_type %in% dealt_with){
          nbm <- new_best_models[which(sapply(new_best_models, function(new_item) item$evalName == new_item$evalName && item$targetName == new_item$targetName))]
          ids <- which(sapply(result$results, function(new_item) length(new_item) != 1 && new_item$typeName == item$typeName && item$evalName == new_item$evalName && item$targetName == new_item$targetName))

          current_best_models <- result$results[ids]
          result$results[ids] <- NA
          bms <- append(current_best_models, nbm)
          nbms <- bms[order(sapply(bms, function(b) b$value$metric),
                            decreasing = is_pos_oriented)][1:length(ids)]
          for (k in 1:length(nbms)) #update info
            nbms[[k]]$info <- k-1
          to_be_added[[length(to_be_added) + 1]] <- nbms

          dealt_with[[length(dealt_with) + 1]] <- eval_target_type
        }
        #else we have already deal with them
      }

      if (startsWith(item$typeName, "best item for")){
        if (item$info == 0 && !eval_target_type %in% dealt_with){
          nbm <- new_best_coefs[which(sapply(new_best_coefs, function(new_item) item$evalName == new_item$evalName && item$targetName == new_item$targetName && item$typeName == new_item$typeName))]
          ids <- which(sapply(result$results, function(new_item) length(new_item) != 1 && new_item$typeName == item$typeName && item$evalName == new_item$evalName && item$targetName == new_item$targetName))

          current_best_coefs <- result$results[ids]
          result$results[ids] <- NA
          bms <- append(current_best_coefs, nbm)
          nbms <- bms[order(sapply(bms, function(b) b$value$metric), decreasing = is_pos_oriented)][1:min(list[[1]]$info$items$bestK,length(bms))]
          for (k in 1:length(nbms)) #update info
            nbms[[k]]$info <- k-1
          to_be_added[[length(to_be_added) + 1]] <- nbms

          dealt_with[[length(dealt_with) + 1]] <- eval_target_type
        }
        #else we have already deal with them
      }

      if (item$typeName == "inclusion"){
        nbm <- new_inclusions[which(sapply(new_inclusions, function(new_item) item$evalName == new_item$evalName && item$targetName == new_item$targetName))]
        if (length(nbm) == 1)
          result$results[[j]]$value <- cmb_inclusion(item$value, nbm[[1]]$value)
      }

      if (item$typeName == "cdf"){
        nbm <- new_cdfs[which(sapply(new_cdfs, function(new_item) item$evalName == new_item$evalName && item$targetName == new_item$targetName && item$info == new_item$info))]
        if (length(nbm) == 1)
          result$results[[j]]$value <- cmb_cdf(item$value, nbm[[1]]$value)
      }

      if (item$typeName == "extreme bound"){
        nbm <- new_extremebounds[which(sapply(new_extremebounds, function(new_item) item$evalName == new_item$evalName && item$targetName == new_item$targetName && item$info == new_item$info))]
        if (length(nbm) == 1)
          result$results[[j]]$value <- cmb_extremebound(item$value, nbm[[1]]$value)
      }

      if (item$typeName == "mixture"){
        nbm <- new_mixtures[which(sapply(new_mixtures, function(new_item) item$evalName == new_item$evalName && item$targetName == new_item$targetName))]
        if (length(nbm) == 1)
          result$results[[j]]$value <- cmb_mixture(item$value, nbm[[1]]$value)
      }

    }

    result$results <- result$results[which(sapply(result$results, function(r)length(r) != 1))]
    for (a in to_be_added)
      result$results <- append(result$results, a)


    # Add All (if this affects performance, we can move it to after the loop.)
    newModels <- newR$results[which(sapply(newR$results, function(item) item$typeName == "model"))]
    result$results <- c(result$results, newModels)

  }
  return(result)
}


#' Adjust Indices in a List
#'
#' This function adjusts a list of indices after certain indices have been removed.
#' The new indices will point to the same elements as the original indices.
#' If an index is removed, it will also be removed from the indices list.
#'
#' @param indicesList A list of integer vectors, each representing a set of indices.
#' @param removedIndices A vector of integers representing the indices to be removed.
#'
#' @return A list of adjusted indices. Each set of indices is adjusted separately.
adjust_indices_after_remove <- function(indicesList, removedIndices){
  if (length(removedIndices) == 0)
    return(indicesList)
  lapply(indicesList, function(indices){
    di <- setdiff(indices, removedIndices)
    sapply(di, function(x) x - sum(removedIndices < x))
  })
}

#' Step-wise estimation
#'
#' This function uses the calculated inclusion weights and selects a subset of variables in each step.
#' Note that it uses the values for the first target variable and first metric and might not be suitable for multi-target or multi-metric searches.
#'
#' @param method sur, bin or varma
#' @param isInnerExogenous Determines if the inner indices are for exogenous variables.
#' @param ... Additional arguments for the search function.
#'
#' @return the result
search.steps <- function(method, isInnerExogenous, ...) {
  dots <- unserialize(serialize(list(...), NULL))

  sizes = dots$combinations$sizes
  counts = dots$combinations$stepsNumVariables
  fixedNames = dots$combinations$stepsFixedNames
  savePre = dots$combinations$stepsSavePre


  printMsg = dots$options$reportInterval > 0
  if (is.null(printMsg))
    printMsg = FALSE

  # TODO: check search items here
  dots <- list(...)
  if (is.null(dots$data))
    stop("'data' argument is missing.")
  if (is.null(dots$combinations))
    stop("'combinations' argument is missing.")

  # if (useInclusion){
  if (is.null(dots$items))
    dots$items <- get.search.items()
  if (!dots$items$inclusion)
    dots$items$inclusion = TRUE

  if (length(sizes) != length(counts)) {
    stop("Invalid number of elements in 'counts' argument (sizes=",
         paste0(sizes, collapse=", "), "; counts=",
         paste0(counts, collapse = ", ") ,")")
  }

  endo_names <- colnames(dots$data$data)[1:dots$data$numEndo]
  target_names <- endo_names[1:dots$combinations$numTargets]
  exo_names <- colnames(dots$data$data)[(dots$data$numEndo + ifelse(dots$data$hasWeight,2,1)):ncol(dots$data$data)]

  fixed_names <- c(target_names, fixedNames)

  # first target and first metric
  tName <- target_names[1]
  eName <- ifelse(is.null(dots$metrics$typesIn),
                  dots$metrics$typesOut[1], dots$metrics$typesIn[1])
  if (printMsg){
    print(paste0("Target used in filtering variables: ", tName))
    print(paste0("Metric used in filtering variables: ", eName))
  }
  estims <- list()

  pre_endo_names <- endo_names
  pre_exo_names <- exo_names

  # Main Loop
  for (i in c(1:length(sizes))) {
    if(i == 1) {
      data_i <- dots$data
      combinations_i <- dots$combinations
    }
    else {
      data_i <- estims[[i-1]]$info$data
      combinations_i <- estims[[i-1]]$info$combinations
    }

    size_i <- sizes[[i]]
    num_var_i <- ifelse(is.na(counts[[i]]), ncol(data_i$data), num_var_i) # it is generally less than number of columns in data

    if (num_var_i < length(fixed_names)){
      stop("Invalid 'stepsNumVariables'. Requested number of variables (=", num_var_i ,") in this step is less than the number of fixed variables (names=", paste0(fixed_names, collapse = "; "),")")
    }

    if (printMsg){
      print(paste0("Step: ", i))
      print(paste0("  Size of models: ", paste0(size_i,collapse = ", ")))
      print(paste0("  Number of potential variables: ", num_var_i))
      print("--------------------------------")
      print("estimation started...")
    }

    #try to load results from file
    if (!is.null(savePre)) {
      res <- suppressWarnings(tryCatch(readRDS(paste0(
        savePre, i,
        ".RData"
      )), error = function(e) NULL))
      if (!is.null(res)) {
        estims[[i]] <- res
        if (printMsg)
          print(paste0("  ... finished (read from file)."))
        next()
      }
    }

    names_i <- NULL
    if (num_var_i == ncol(data_i$data))
      names_i <- colnames(data_i$data)
    else{ # we should remove some variables from the data using previous estimation
      if (i == 1){
        names_i <- colnames(data_i$data)[1:num_var_i]
        warning("Variables are removed from the analysis in the first step, without evaluation. It is recommended to use 'NA' as the first element of 'stepsNumVariables' argument.")
      }
      else{
        est <- estims[[i - 1]]
        values <- est$results[which(sapply(est$results, function(y) {y$targetName == tName && y$evalName == eName}))]
        inclusion <- values[which(sapply(values, function(y){y$typeName == "inclusion"}))][[1]]
        if (is.null(inclusion))
          stop("'inclusion' matrix is missing in the current step.")
        sorted_inclusion <- inclusion$value[order(inclusion$value[,1], decreasing = TRUE),]
        names_i <- unique(c(fixed_names, rownames(sorted_inclusion)[1:(num_var_i)]))
        names_i <- names_i[1:num_var_i]
      }
    }

    # use 'names_i' to adjust data and combinations:
    endo_names_i <- intersect(names_i, endo_names)
    exo_names_i <- intersect(names_i, exo_names)
    endo_names_removed_i <- setdiff(endo_names_i, pre_endo_names)
    exo_names_removed_i <- setdiff(exo_names_i, pre_exo_names)
    endo_indices_removed_i <- match(endo_names_removed_i, pre_endo_names)
    exo_indices_removed_i <- match(exo_names_removed_i, pre_exo_names)
    pre_endo_names <- endo_names_i
    pre_exo_names <- exo_names_i

    if (dots$data$hasWeight) # insert weights
      names_i <- append(names, "(Weights)", after = length(endo_names_i))
    indices_i <- match(names_i, colnames(data_i$data))
    if (any(is.na(indices_i)))
      stop("A selected variable is not present in the 'data'!")

    if (length(indices_i) != ncol(data_i$data)){
      data_i$data <- data_i$data[, indices_i]
      data_i$numEndo <- length(endo_names_i)
      data_i$numExo <- length(exo_names_i)
    }

    combinations_i$sizes <- size_i
    if (isInnerExogenous){
      combinations_i$innerGroups <- adjust_indices_after_remove(combinations_i$innerGroups, exo_indices_removed_i-1)
      combinations_i$partitions <- adjust_indices_after_remove(combinations_i$partitions, endo_indices_removed_i - 1)
    }else{
      combinations_i$innerGroups <- adjust_indices_after_remove(combinations_i$innerGroups, endo_indices_removed_i-1)
      combinations_i$partitions <- adjust_indices_after_remove(combinations_i$partitions, exo_indices_removed_i - 1)
    }
    # reset:
    # zero-based indexation and moving exogenous indices:
    # see the end of get.indexation function
    for (j in c(1:length(combinations_i$partitions))){
      combinations_i$partitions[[j]] <- combinations_i$partitions[[j]] + 1
      if (isInnerExogenous == FALSE)
        combinations_i$partitions[[j]] <- combinations_i$partitions[[j]] - data_i$numEndo
    }
    for (j in c(1:length(combinations_i$innerGroups))){
      combinations_i$innerGroups[[j]] <- combinations_i$innerGroups[[j]] + 1
      if (isInnerExogenous)
        combinations_i$innerGroups[[j]] <- combinations_i$innerGroups[[j]] - data_i$numEndo
    }


    dots$data <- data_i
    dots$combinations <- combinations_i

    if (printMsg){
      rec.print.list(list(
        `selected variables` = names_i,
        `removed endogenous` = endo_names_removed_i,
        `removed exogenous` = exo_names_removed_i
      ), "        ")
    }
    if (method == "rfunc")
      dots$isInnerExogenous <- isInnerExogenous
    estims[[i]] <- do.call(paste0("search.", method), c(dots))

    lastStep = i == length(sizes)
    allFailed = estims[[i]]$counts$searchedCount == estims[[i]]$counts$failedCount
    if (lastStep == FALSE && allFailed) {
      if (printMsg){
        print("......Failures.......")
        print(estims[[i]]$counts$failedDetails)
      }
      stop("all estimations failed")

      #TODO: an argument for moving to the next step without reducing the size.
    }

    if (is.null(savePre) == FALSE) {
      saveRDS(list(estim = estims[[i]], data = data_i), paste0(savePre, i, ".RData"))
      print(paste0("Data saved: ", file.path(getwd(), paste0(savePre, i, ".RData"))))
    }

    if (printMsg) {
      print(paste0("  ... finished"))
    }
  }
  combine.search(estims, method)
}

