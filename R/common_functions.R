#' Heuristic functions checker
#'
#' @description
#' Self-check of allowed functions for all heuristics of Airport operations.
#'
#' @param file File name in working directory or path to file. File ('.txt') following the exact format from Atenea.
#' @param loops Number of loops allowed for the current heuristic, by default *loops = 0*. See Details.
#'
#' @returns
#' Report with information about your result.
#'
#' @details
#' If any ERROR found, code will not run on any self-evaluator.
#'
#' Maximum number of loops per activity:
#' - Activity 2: 2 loops
#' - Activity 4 (part 1): 0 loops
#' - Activity 4 (part 2): 2 loops
#'
#' @examples
#' check_functions(file = "rs_heur.txt", loops = 2)
#' check_functions(file = "stand_util.txt", loops = 0)
#' check_functions(file = "sa_heur.txt", loops = 2)
check_functions <- function(file, loops = 0){
  tokens_vull <- c("SYMBOL_FUNCTION_CALL", "FOR", "WHILE", "IF", "ELSE", "NEXT", "BREAK", "FUNCTION")
  data <- data.table(getParseData(parse(file=file)))
  function_names <- data[token %in% tokens_vull, text]
  fnc <- data.table(function_names)
  fnc <- fnc[,.N, by = function_names]
  fnc <- fnc[!function_names %in% c(".")]
  setnames(fnc, c("Function", "N"))
  own_fnc <- data[line1 %in% data[token %in% "FUNCTION", line1] & token == "SYMBOL", text]

  data <- data[which(data$token != "COMMENT"),]
  ini <- suppressWarnings(data[min(which(token %in% "FUNCTION")), line1] == min(data$line1))
  if(is.na(ini)) ini = F
  fi <- suppressWarnings(data[max(which(token %in% "'}'")), line1] == max(data$line1))
  if(is.na(fi)) fi = F

  allow_funct <- c("`:=`","abs","apply","as.character","as.factor","as.IDate","as.integer",
                   "as.ITime","as.numeric","as.POSIXct","assign","break","c","cbind","ceiling",
                   "copy","cumsum","data.table","dcast","difftime","duplicated","else",
                   "expand.grid","floor","for","fread","function","grep","gsub","hour","if",
                   "is.na","is.null","lapply","length","list","match","max","mean","melt",
                   "merge","min","minute","names","nchar","ncol","next","nrow","order","paste",
                   "paste0","rbind","rbindlist","readline","readRDS","rep","return","rm",
                   "rnorm","round","rowid","sample","sample.int","sapply","sd","second","seq",
                   "setcolorder","setnames","setorder","shift","sort","strftime","substr",
                   "sum","t","tryCatch","tstrsplit","unique","unlist","while","stand_util")
  els_loops <- c("for", "while", "apply", "lapply", "sapply")
  allow_funct <- unique(c(allow_funct, els_loops))
  fnc[, Allowed := as.character(Function %in% allow_funct)]
  fnc[Function %in% own_fnc, Allowed := "OWN"]
  if(loops < fnc[Function %in% els_loops, sum(N)]) fnc[Function %in% els_loops, Allowed := "TOO MANY"]
  fnc[, xord := nchar(Allowed)]
  setorder(fnc, xord, -N, Function)
  fnc <- unique(fnc, by = "Function")
  setorder(fnc, -xord, -N, Function)
  fnc[, xord := NULL]
  fnc_fls <- fnc[Allowed == "FALSE", .N] == 0
  fnc_too <- fnc[Allowed == "TOO MANY", .N] == 0

  if(!ini | !fi | !fnc_fls | !fnc_too){
    if(!ini) cat0("ERROR: Code doesn't start with a function")
    if(!fi) cat0("ERROR: Code doesn't ends with a function")
    if(!fnc_fls) cat0("ERROR: Using forbidden functions")
    if(!fnc_too) cat0("ERROR: Using too many loops")
    cat0()
    cat0("Your functions:")
    print(fnc)
    return(FALSE)
  }
  return(TRUE)
}

myTryCatch <- function(expr, nmb) {
  if(nmb == 0) tout = 5
  if(nmb > 0) tout = tmaxs[[nmb]]
  warn <- err <- NULL
  t1 = Sys.time()
  value <- withCallingHandlers(
    tryCatch(withTimeout(expr, timeout = tout),
             TimeoutException = function(ex) {
               err <<- "Time out"
               NULL
             },
             error=function(e) {
               err <<- e
               NULL
             }),
    warning=function(w) {
      warn <<- w
      invokeRestart("muffleWarning")
    })
  t2 = Sys.time()
  tspent <- round(as.numeric(difftime(t2, t1, units = "secs")), 2)
  tspent <- paste("Time:", tspent, "secs")
  list(value=value, warning=warn, error=err, tspent = tspent)
}

codi_ok <- function(file){
  codiok <- myTryCatch(source(file), nmb = 0)
  if(is.null(codiok$value$value)){
    cat0("ERROR: Code can't be read")
    setwd(el_fold)
    cat0(as.character(codiok$error))
    return(FALSE)
  }
  return(TRUE)
}

cat0 <- function(...) {cat(..., "\n", sep ="")}

tmaxs <- list(rs_heur = 10*60,
              sa_heur1 = 30,
              sa_heur2 = 10*60,
              gm_heur1 = 0*60,
              gm_heur2 = 0*60)

loops_max <- list(rs_heur = 2,
                  sa_heur1 = 0,
                  sa_heur2 = 2,
                  gm_heur1 = 0,
                  gm_heur2 = 0)
