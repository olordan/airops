#' Runway Sequencing Manual Evaluation
#'
#' @description
#' Self-evaluator for Activity 1 - RS Manual of Airport operations.
#'
#' @param file File name in working directory or path to file. File ('rst_result.csv') following the exact format from Atenea.
#' @param group Number of your group.
#'
#' @returns
#' Report with information about your result.
#'
#' @examples
#' rse_manual(file = "rst_result.csv", group = 1)
rse_manual <- function(file, group, get_grade){
  if(missing("file")) return(cat0("ERROR: you must provide your file"))
  if(missing("group")) return(cat0("ERROR: you must provide your group"))
  if(!regexpr("\\.csv$", file)>0) return(cat0("ERROR: file must be a CSV"))
  if(missing("get_grade")) get_grade = F
  fl_name <- strsplit(file, "/")[[1]]
  fl_name <- fl_name[length(fl_name)]
  if(fl_name != "rst_result.csv") return(cat0("ERROR: your file must be named 'rst_result.csv'"))
  rm(fl_name)

  sep_times <- wake_turbulence$WT_TOY
  loseu <- fread(file, header = T)

  cat0("Your file:")
  print(loseu)
  cat0()

  if(length(sum(names(loseu) == "callsign")) != 1) return(cat0("ERROR: No callsign column"))
  if(length(sum(names(loseu) == "ac_cat")) != 1) return(cat0("ERROR: No ac_cat column"))
  if(length(sum(names(loseu) == "time_atm")) != 1) return(cat0("ERROR: No time_atm column"))

  if(!identical(names(loseu), c("callsign", "ac_cat", "time_atm"))) return(cat0("ERROR: Columns are not in the right order"))

  if(sum(!(loseu$ac_cat %in% LETTERS[1:max(sep_times$earlier)]))>0) return(cat0("ERROR: ac_cat is not correct"))
  if(sum(regexpr("^[[:digit:]]{1,2}:[[:digit:]]{2}:[[:digit:]]{2}$", loseu$time_atm) <= 0) > 0) return(cat0("ERROR: ac_cat is not correct"))

  loseu <- loseu$callsign

  if(length(loseu) != 20) return(cat0("ERROR: Flights != 20"))

  el_toy <- toys_rs[[group]]
  el_toy_d <- el_toy[a_d == "D"][1:20]
  el_toy_a <- el_toy[a_d == "A"][1:20]

  m_d <- match(loseu, el_toy_d$callsign)
  m_a <- match(loseu, el_toy_a$callsign)

  if(!(sum(is.na(m_d)) == 0 | sum(is.na(m_a)) == 0)) return(cat0("ERROR: Didn't take just A or D"))

  if(sum(is.na(m_d)) == 0){
    el_ord <- m_d
    el_toy <- el_toy_d
    aod <- "D"
  }

  if(sum(is.na(m_a)) == 0){
    el_ord <- m_a
    el_toy <- el_toy_a
    aod <- "A"
  }

  el_toy[, time_atm := as.ITime(time_atm)]
  el_toy <- merge(el_toy, ac_types, by = "ac_model", sort = F)
  el_toy[, ac_cat := as.integer(as.factor(ac_cat))]
  el_toy[, secs := hour(time_atm)*3600 + minute(time_atm)*60 + second(time_atm)]

  delay <- TotalDelay(SepTimes = sep_times,
                      FlightsScheduled = el_toy[,.(ac_cat, secs)],
                      sequence = copy(el_ord))$delay

  aod2 <- "Departures"
  if(aod == "A") aod2 <- "Arrivals"
  cat0(aod2, " of Group ", group)
  cat0("Total delay: ", delay, " sec")

  res <- eval_files$rs_manual[Group == group & A_D == aod]
  res[, nota := 0]
  res[delay <= Great, nota := 10]
  res[delay >= Switch & delay <= FIFO, nota := (delay-FIFO)/(Switch-FIFO)*5]
  res[delay >= Great & delay <= Switch, nota := (delay-Switch)/(Great-Switch)*5 + 5]
  res[, nota := ceiling(nota*10)/10]
  cat0("Grade: ", round(res$nota,2))
  if(get_grade) return(round(res$nota,2))
  return(cat())
}

#' Runway Sequencing Heuristic Evaluation
#'
#' @description
#' Self-evaluator for Activity 2 - RS Heuristic of Airport operations.
#'
#' @param file File name in working directory or path to file. File ('rs_heur.txt') following the exact format from Atenea.
#' @param toys_to_test A vector with toy numbers to check your heuristic against. Database has 10 toy models and by default it runs all of them.
#' @param recat_csv File name in working directory or path to file. File ('rs_recat.csv') following the exact format from Atenea. Result of your heuristic using RECAT wake turbulence on your group's schedule.
#' @param icao_csv File name in working directory or path to file. File ('rs_icao.csv') following the exact format from Atenea. Result of your heuristic using ICAO wake turbulence on your group's schedule.
#' @param group Number of your group.
#'
#' @returns
#' Report with information about your result.
#'
#' @details
#' Maximum execution time of your code is 10 min.
#'
#' Inputs *recat_csv*, *icao_csv* and *group* are optional just for checking your schedule results. *group* is mandatory when having a *recat_csv* or *icao_csv* input.
#'
#' @examples
#' rse_heur(file = "rs_heur.txt", toys_to_test = 1:10)
#' rse_heur(file = "rs_heur.txt", toys_to_test = 1:2, recat_csv = "rs_recat.csv", icao_csv = "rs_icao.csv", group = 1)
rse_heur <- function(file, toys_to_test = 1:10, recat_csv, icao_csv, group, get_grade){
  if(missing("file")) return(cat0("ERROR: you must provide your file"))
  if(!regexpr("\\.txt$", file)>0) return(cat0("ERROR: file must be a plain TXT"))
  if(length(readLines(file)) == 0) return(cat0("ERROR: file is empty"))
  if(!missing("recat_csv")) {
    if(!is.na(recat_csv)) {
      if(!regexpr("\\.csv$", recat_csv)>0) return(cat0("ERROR: recat_csv must be a CSV"))
      fl_name <- strsplit(recat_csv, "/")[[1]]
      fl_name <- fl_name[length(fl_name)]
      if(fl_name != "rs_recat.csv") return(cat0("ERROR: your RECAT file must be named 'rs_recat.csv'"))
    }
  }
  if(!missing("icao_csv")) {
    if(!is.na(icao_csv)) {
      if(!regexpr("\\.csv$", icao_csv)>0) return(cat0("ERROR: icao_csv must be a CSV"))
      fl_name <- strsplit(icao_csv, "/")[[1]]
      fl_name <- fl_name[length(fl_name)]
      if(fl_name != "rs_icao.csv") return(cat0("ERROR: your ICAO file must be named 'rs_icao.csv'"))
    }
  }
  if(missing("get_grade")) get_grade = F
  fl_name <- strsplit(file, "/")[[1]]
  fl_name <- fl_name[length(fl_name)]
  if(fl_name != "rs_heur.txt") return(cat0("ERROR: your heuristic file must be named 'rs_heur.txt'"))
  rm(fl_name)

  el_fold <- getwd()
  if((!missing("recat_csv") | !missing("icao_csv")) & missing("group")) return(cat0("ERROR: you must provide your group"))
  n_toys <- unique(round(toys_to_test[toys_to_test > 0 & toys_to_test < 11]))
  if(length(n_toys) == 0) return(cat0("ERROR: toys_to_test must have values between 1 to 10"))
  if(sum(is.na(n_toys))) return(cat0("ERROR: toys_to_test must have values between 1 to 10"))

  file.copy(from = file, to = paste0(tempdir(), "/la_heur.txt"), overwrite = T)

  setwd(tempdir())
  on.exit({
    unlink("schedules/", recursive = T)
    unlink("rs_toys/", recursive = T)
    unlink("data/", recursive = T)
    unlink("la_heur.txt")
    suppressWarnings(rm(rs_heur, envir = .GlobalEnv))
    setwd(el_fold)
  })

  if(!check_functions("la_heur.txt", loops_max$rs_heur)){
    return(cat())
  }
  #test the code ----

  if(!codi_ok("la_heur.txt")){
    return(cat())
  }

  source("la_heur.txt")

  suppressWarnings(dir.create("schedules"))
  suppressWarnings(dir.create("rs_toys"))
  suppressWarnings(dir.create("data"))

  for(i in seq(length(schedules))) fwrite(schedules[[i]], file = paste0("schedules/", names(schedules)[i],".csv"))
  for(i in seq(length(toys_rs))) fwrite(toys_rs[[i]], file = paste0("rs_toys/", names(toys_rs)[i],".csv"))
  for(i in seq(length(wake_turbulence2))) fwrite(wake_turbulence2[[i]], file = paste0("data/", names(wake_turbulence2)[i],".csv"))
  fwrite(ac_types2, file = "data/ac_types.csv")

  #check els seus csv ----
  wktt <- NULL
  if(!missing("icao_csv")) if(!is.na(recat_csv)) wktt <- c(wktt, 1)
  if(!missing("recat_csv")) if(!is.na(icao_csv)) wktt <- c(wktt, 2)

  res_sch = 0
  if(length(wktt) > 0){
    sch_test <- fread(list.files("schedules/", full.names = T)[group], select = 1)
    estabe = estabe1 = 0
    for(wkt in wktt){
      cat0(c("## ICAO", "## RECAT")[wkt], " csv evaluation")
      loseu <- fread(paste0(el_fold, "/", c(icao_csv, recat_csv))[wkt], header = T)
      cat0("Your file:")
      print(loseu)
      cat0()
      if(length(sum(names(loseu) == "callsign")) != 1) {
        cat0("ERROR: No callsign column")
        cat0()
        next
      }
      if(length(sum(names(loseu) == "ac_cat")) != 1)  {
        cat0("ERROR: No ac_cat column")
        cat0()
        next
      }
      if(length(sum(names(loseu) == "time_atm")) != 1) {
        cat0("ERROR: No time_atm column")
        cat0()
        next
      }
      if(!identical(names(loseu), c("callsign", "ac_cat", "time_atm")))  {
        cat0("ERROR: Columns are not in the right order")
        cat0()
        next
      }
      if(sum(!(loseu$ac_cat %in% LETTERS[1:max(wake_turbulence[[wkt]]$earlier)]))>0)  {
        cat0("ERROR: ac_cat is not correct")
        cat0()
        next
      }
      if(sum(regexpr("^[[:digit:]]{1,2}:[[:digit:]]{2}:[[:digit:]]{2}$", loseu$time_atm) <= 0) > 0)  {
        cat0("ERROR: ac_cat is not correct")
        cat0()
        next
      }

      res <- myTryCatch(rs_heur(schedule = list.files("schedules/")[group], sep_time = c("icao","recat")[wkt]), nmb = 1)

      cat0(res$tspent)
      if(!is.null(res$warning)) {
        cat("WARNING: ")
        cat0(as.character(res$warning))
      }
      if(!is.null(res$error)) {
        cat0("ERROR: Code doesn't run")
        cat0(as.character(res$error))
        next
      } else {
        res <- res$value
        if(sum(sch_test[, !callsign %in% res$callsign]) > 0) {
          cat0("ERROR: Output doesn't have all operations")
          next
        } else {
          estabe = estabe + 1
          estabe <- estabe + as.integer(sum(sch_test[, !callsign %in% loseu$callsign]) == 0)
          cat0("Grade: ", (estabe - estabe1)/2*10)
          cat0()
          estabe1 = estabe
        }
      }
    }
    res_sch <- 10*estabe/4
    rm(loseu, res, sch_test, estabe, estabe1)
  }

  #check tots els toys ----
  resultats <- list()
  notaf <- c()
  cat0("## Heuristic evaluation (with TOY)")

  for(arx2 in n_toys){
    arx <- list.files("rs_toys/")[arx2]
    cat0("# File: ", eval_files$rs_heur$File[arx2])

    res <- myTryCatch(rs_heur(schedule = arx, sep_time = "toy"), nmb = 1)
    cat0(res$tspent)

    if(!is.null(res$warning)) {
      cat("WARNING: ")
      cat0(as.character(res$warning))
    }

    if(!is.null(res$error)) {
      cat0("ERROR: Code doesn't run")
      cat0(as.character(res$error))
      cat0()
      notaf <- c(notaf, 0)
      next
    }
    res <- res$value
    cat0("Your file:")
    print(res)
    cat0()
    dades <- toys_rs2[[arx2]]
    if(sum(dades[, !callsign %in% res$callsign]) > 0) {
      cat0("ERROR: Output doesn't have all operations")
      cat0()
      notaf <- c(notaf, 0)
      next
    }

    dels <- c()
    for(type in c("A","D")){
      el_ord <- match(res$callsign, dades[a_d == type, callsign])
      el_ord <- el_ord[!is.na(el_ord)]
      delay <- TotalDelay(SepTimes = wake_turbulence$WT_TOY,
                          FlightsScheduled = dades[a_d == type, .(ac_cat, secs)],
                          copy(el_ord))$delay
      dels <- c(dels, delay)
      cat0("Delay ", type, ": ", delay, " secs")
    }
    dels <- sum(dels)
    cat0("Total delay (A + D): ", dels, " secs")
    nf <- eval_files$rs_heur[arx2]
    nf[, V1 := dels]
    nf[, nota := 0]
    nf[V1 <= Great, nota := 10]
    nf[V1 >= Switch & V1 <= FIFO, nota := (V1-FIFO)/(Switch-FIFO)*5]
    nf[V1 >= Great & V1 <= Switch, nota := (V1-Switch)/(Great-Switch)*5 + 5]
    nf[, ceiling(mean(nota)*10)/10]
    cat0("Grade: ", nf$nota)
    notaf <- c(notaf, nf$nota)
    cat0()
  }
  cat0("Heuristic grade (expected): ", round(mean(notaf),2))
  cat0()
  if(!missing("recat_csv") & !missing("icao_csv")) cat0("Final grade (expected): ", round((mean(notaf)*10 + res_sch*5)/15,2))

  if(get_grade) return(round((mean(notaf)*10 + res_sch*5)/15,2))
  return(cat())
}
