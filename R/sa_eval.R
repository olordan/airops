PlottingSolution_ggplot <- function(stands_ids, solution){
  stands_ids <- copy(stands_ids)
  stands_ids[order(-seq(.N)), mark := as.character(seq(.N))]

  solution <- merge(solution, stands_ids, all.x = TRUE)
  solution[, secs1 := secs1]
  solution[, secs2 := secs2]
  solution[, xtext := secs1+(secs2-secs1)/2]
  solution[, ramp := factor(ramp, unique(stands_ids$ramp))]
  solution[op == "A", colorets := "#F8766D"] #roig
  solution[op == "D", colorets := "#00BC59"] #verd
  solution[op == "P", colorets := "#619CFF"] #blau
  # solution[op == "O", colorets := "#C77CFF"] #lila
  solution[op == "O", colorets := "#440154FF"] #lila fort
  stands_ids <- stands_ids[match(sort(unique(solution$mark)), mark)]

  if(sum(solution$contact == 0) == 0){
    elplot <- ggplot(solution,
                     aes(y = mark)) +
      geom_segment(data = solution,
                   aes(x = secs1,
                       y = mark,
                       xend = secs2,
                       yend = mark,
                       colour = colorets),
                   size = 7) +
      scale_colour_identity() +
      geom_text(data = solution, aes(x = xtext, y = mark, label = id), cex = 2.5) +
      scale_y_discrete(breaks = stands_ids$mark, labels = stands_ids$stand) +
      coord_cartesian(xlim = c(min(solution$secs1)+3600/4,max(solution$secs2)-3600/4)) +
      scale_x_datetime(date_breaks = "30 min", date_labels = "%H:%M") +

      facet_grid(ramp ~ ., scales = 'free_y', space = "free_y") +
      theme_bw() +
      theme(axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            legend.position='none')
  } else {
    elplot <- ggplot(solution,
                     aes(y = mark)) +
      geom_segment(data = stands_ids[contact == 0],
                   aes(x = min(solution$secs1),
                       y = mark,
                       xend = max(solution$secs2),
                       yend = mark,
                       alpha = 0.1),
                   color = "yellow", alpha = 0.2,
                   size = 8) +
      geom_segment(data = solution,
                   aes(x = secs1,
                       y = mark,
                       xend = secs2,
                       yend = mark,
                       colour = colorets),
                   size = 7) +
      scale_colour_identity() +
      geom_text(data = solution, aes(x = xtext, y = mark, label = id), cex = 2.5) +
      scale_y_discrete(breaks = stands_ids$mark, labels = stands_ids$stand) +
      coord_cartesian(xlim = c(min(solution$secs1)+3600,max(solution$secs2)-3600)) +
      scale_x_datetime(date_breaks = "30 min", date_labels = "%H:%M") +

      facet_grid(ramp ~ ., scales = 'free_y', space = "free_y") +
      theme_bw() +
      theme(axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            legend.position='none')
  }
  return(elplot)
}

get_eval <- function(sol, gr) {
  stands_plot <- copy(clean_s)
  stands_plot <- stands_plot[, .(ramp, stand, contact)]
  incomp <- copy(clean_s)[, .(stand, S1, S2, S3)]
  inc1 <- incomp$stand
  incomp <- melt(incomp, "stand")
  incomp[, variable := NULL]
  incomp <- incomp[value != ""]
  incomp <- rbind(data.table(stand = inc1, value = inc1), incomp)
  setnames(incomp, "value", "inc")
  rm(inc1)

  solucio <- fread(sol, header = T)
  cat0("Your file:")
  print(solucio)
  cat0()

  if(length(sum(names(solucio) == "callsign")) != 1) return(cat0("ERROR: No callsign column"))
  if(length(sum(names(solucio) == "op")) != 1) return(cat0("ERROR: No op column"))
  if(length(sum(names(solucio) == "stand")) != 1) return(cat0("ERROR: No stand column"))

  if(!identical(names(solucio), c("callsign", "op", "stand"))) return(cat0("ERROR: Columns are not in the right order"))

  if(sum(!(solucio$op %in% c("A","D","P")))>0) return(cat0("ERROR: op is not correct"))

  solucio <- solucio[, .(callsign, stand)]
  solucio <- unique(solucio)
  solucio[, stand := toupper(stand)]
  if(sum(duplicated(solucio, by = "callsign"))) {
    cat0("WARNING: Returning duplicated callsigns, considering just the first elements")
    solucio <- unique(solucio, by = "callsign")
  }

  stand_utility <- copy(utils_sa[[gr]])
  pel_toy <- unique(stand_utility[time1 >= as.ITime("10:00") & time1 <= as.ITime("15:00") & ramp == "R2", id]) # pel toy
  stand_utility[, c("time1", "time2") := NULL]
  stand_utility[, op_time1 := hour(op_time1)*60 + minute(op_time1)]
  stand_utility[, op_time2 := hour(op_time2)*60 + minute(op_time2)]
  loseu <- merge(solucio, stand_utility, by = c("callsign", "stand"), all.x = T)

  if(length(unique(loseu[!is.na(util)]$ramp)) == 1) { # pel toy
    stands_plot <- stands_plot[ramp == "R2"]
    stand_utility <- stand_utility[id %in% pel_toy]
  }
  rm(pel_toy)

  necessaris <- unique(stand_utility[, .(callsign)])

  sol_nec <- merge(necessaris, solucio, by = c("callsign"), all.x = T)
  err1_data <- sol_nec[is.na(stand)]
  err1 <- nrow(err1_data) #s'han deixat alguna operacio per assignar

  err2_data <- loseu[is.na(util), .(callsign,stand)]
  err2 <- nrow(err2_data) #han assignat algo a un stand que no tenia utilitat (no es podia o que no existeix)

  loseu <- loseu[!is.na(util)]

  isok <- loseu[, .(time = seq(from = min(op_time1), to = max(op_time2), by = 5)), by = .(id, op, stand)]
  isok <- isok[, .SD[-1], by = .(id, op, stand)]
  isok_inc <- merge(isok, incomp, by = "stand", allow.cartesian = T)
  isok_inc[, stand := inc]
  isok_inc[, inc := NULL]
  isok_3 <- merge(isok, isok_inc, by = c("stand", "time"), all.x = T, allow.cartesian = T)
  setnames(isok_3, c("id.x", "op.x", "id.y", "op.y"), c("id1", "op1", "id2", "op2"))
  # isok_3 <- isok_3[op1 != op2 & id1 != id2]
  isok_3 <- isok_3[id1 != id2]
  err3 <- err4 <- err5 <- err6 <- 0
  te456 <- nrow(isok_3)>0
  if(te456){
    isok_3[id1 > id2, `:=` (id1 = id2, id2 = id1, op1 = op2, op2 = op1)]
    isok_3 <- unique(isok_3)
    # max(isok_3[, .N, by = .(stand, time)][,N])*2
    # unique(isok_3$id1, isok_3$id2)
    isok_4 <- rbind(isok_3[,.(stand, time, id = id1, op = op1)], isok_3[,.(stand, time, id = id2, op = op2)])
    isok_4[, dups := rowid(stand, time)]
    err3 <- max(isok_4$dups) #max de id solapats en un mateix stand i hora

    err456 <- unique(merge(unique(isok_4[dups > 1, .(stand, time)]), isok_4, by = c("stand", "time"))[, .(id, op, stand)])
    err4 <- length(unique(err456$id)) #num de ids solapats
    err5 <- length(unique(err456$stand)) #num de stands diferents solapats
    err6 <- nrow(err456) #num de operacions solapades

    isok_3[, time := NULL]
    isok_3 <- unique(isok_3)
    isok_3 <- merge(isok_3, loseu[, .(id1 = id, op1 = op, callsign1 = callsign)], by = c("id1", "op1"))
    isok_3 <- merge(isok_3, loseu[, .(id2 = id, op2 = op, callsign2 = callsign)], by = c("id2", "op2"))

    isok_3 <- isok_3[, .(stand = paste0(stand, collapse = "-")), by = .(callsign1,op1,callsign2,op2)]

    loseu <- merge(loseu, err456[, .(id, op, err = stand)], by = c("id", "op"), all.x = T)
    loseu[!is.na(err), `:=` (op = "O", util = 0)]
    loseu[, err := NULL]
    loseu <- unique(loseu)
  }

  tows <- dcast(loseu[op != "O"], id ~ op, value.var = "stand")
  setcolorder(tows, c("id","A","P","D"))
  tows[, tow := 0]
  tows[A != P, tow := tow + 1]
  tows[D != P, tow := tow + 1]
  tows[is.na(P) & A != D, tow := 1]
  ntows <- sum(tows$tow) #numero de tows

  loseu <- merge(loseu, tows[, .(id, tow)], by = "id")
  loseu[, util := util-25*tow/.N, by = id]
  loseu[, tow := NULL]

  loseu <- merge(loseu, clean_s[, .(stand, contact)], by = "stand", sort = F)
  contrem <- loseu[, .N, by = .(op, contact)]
  contrem[,contact2 := "contact"]
  contrem[contact == 0, contact2 := "remote"]
  contrem[, contact := NULL]
  contrem <- dcast(contrem, op ~ contact2, value.var = "N")
  if(sum(names(contrem) == "remote") == 0) contrem[, remote := 0]
  contrem[is.na(contact), contact := 0]
  contrem[is.na(remote), remote := 0]
  contrem[, `rem/cont` := paste0(round(remote/contact*100,2),"%")]
  tomatch <- c("A", "P", "D")
  if(te456) tomatch <- c("A", "P", "D", "O")
  contrem <- contrem[match(tomatch, op)]


  meansd <- loseu[, mean(util), by = id][, .(mean = mean(V1), sd = sd(V1))]
  r1 <- sum(loseu$util) - 500*(err1 + err2 + err4) #total utility
  r2 <- meansd$mean #mean utility de cada mean de id
  r3 <- meansd$sd #sd utility de cada mean de id

  ##INFORME----
  # loseu
  loseu[, secs1 := floor(op_time1/60)]
  loseu[, secs1 := as.POSIXct(paste0(secs1, ":", op_time1 - secs1*60), format = "%H:%M")]
  loseu[, secs2 := floor(op_time2/60)]
  loseu[, secs2 := as.POSIXct(paste0(secs2, ":", op_time2 - secs2*60), format = "%H:%M")]
  loseu[, c("op_time1", "op_time2") := NULL]

  result <- list(
    r1 = r1, #total utility
    r2 = r2, #mean utility de cada mean de id
    r3 = r3, #sd utility de cada mean de id
    ntows = ntows, #numero de tows
    contrem = contrem, #operacions en contacte i remote i divisio
    # err0_data = err0_data,
    # err0 = err0, #contactes equivocats -- donara utilitats equivocades
    err1_data = err1_data,
    err1 = err1, #s'han deixat alguna operacio per assignar
    err2_data = err2_data,
    err2 = err2, #han assignat algo a un stand que no tenia utilitat (no es podia o que no existeix)
    err3 = err3, #max de id solapats en un mateix stand i hora
    isok_3 = isok_3, #solapaments
    # err456 = err456, #summary solapaments
    err4 = err4, #num de ids solapats
    err5 = err5, #num de stands diferents solapats
    err6 = err6, #num d'operacions solapades
    loseu = loseu,
    stands_plot = stands_plot)
  return(result)
}

sa_perun <- function(sol, gr, satype = c("manual", "heur"), get_grade){
  res <- get_eval(sol = sol, gr = gr)
  if(is.null(res)) return(cat())

  if(!get_grade){
    cat0("## Global stats")

    cat0("Total utility: ", round(res$r1, 2))
    cat0("Mean utility: ", round(res$r2, 2))
    cat0("SD utility: ", round(res$r3, 2))
    cat0("Tows: ", res$ntows)
    cat0()

    cat("## Summary")
    if(res$err4>0) cat(" (O: overlapped operations)")
    cat0(":")
    print(res$contrem)
    cat0()

    if(sum(res$err1, res$err2, res$err3, res$err4, res$err5, res$err6)>0) cat0("## Errors")
    if(res$err1 > 0) {
      cat0(res$err1, " operation/s left to assign:")
      print(res$err1_data[, .(callsign)], nrows = 10)
      cat0()
    }
    if(res$err2 > 0) {
      cat0(res$err2, " assigned operation/s to unvalid stands:")
      print(res$err2_data, nrows = 10)
      cat0()
    }
    if(res$err3 > 0) {
      cat0(res$err4," operation/s overlapped:")
      print(res$isok_3, nrows = 10)
      cat0()
    }

    cat0("## Stand allocation chart")
    cat0("Legend (by colors):")
    cat0("Red: arrival")
    cat0("Blue: parking")
    cat0("Green: departure")
    if(res$err4>0) cat0("Dark: overlapped operations")
    cat0("Yellow (background): remote stands")
    cat0()
    print(PlottingSolution_ggplot(res$stands_plot, res$loseu))
    # if(satype == "manual") print(PlottingSolution_ggplot(res$stands_plot, res$loseu))
    # if(satype == "heur")   print(PlottingSolution_ggplot(res$stands_plot, res$loseu))
  }

  n_mahe <- round(grade_mahe(res$r1, satype, gr), 2)
  cat0("Grade: ", n_mahe)

  if(get_grade) return(n_mahe)
  return(cat())
}

grade_mahe <- function(utr1, satype, gr){
  if(satype == "manual") el_eval <- eval_files$sa_manual[gr]
  if(satype == "heur")   el_eval <- eval_files$sa_heur[gr]
  el_eval[, ut := utr1]
  el_eval[, nota := 0]
  el_eval[ut >= Great, nota := 10]
  el_eval[ut <= Good & ut >= Poor, nota := (ut-Poor)/(Good-Poor)*5]
  el_eval[ut <= Great & ut >= Good, nota := (ut-Good)/(Great-Good)*5 + 5]
  el_eval[, nota := ceiling(nota*10)/10]
  return(el_eval$nota)
}

#' Stand Allocation Manual Evaluation
#'
#' @description
#' Self-evaluator for Activity 3 - SA Manual of Airport operations. It can also be used to review (not evaluate) your heuristic's result from Activity 4 - SA Heuristic (part 2).
#'
#' @param file File name in working directory or path to file. File ('sa_result.csv') following the exact format from Atenea. You can also review the heuristic's result ('sa_heur.csv').
#' @param group Number of your group.
#' @param sa_type default is "manual". Select "heur" when reviewing the heuristic's result.
#'
#' @returns
#' Report with information about your result.
#'
#' @examples
#' sae_manual(file = "sa_result.csv", group = 1) # Manual evaluation
#' sae_manual(file = "sa_heur.csv", group = 1, sa_type = "heur") # Heuristic review
sae_manual <- function(file, group, sa_type = c("manual", "heur"), get_grade){
  if(missing("file")) return(cat0("ERROR: you must provide your file"))
  if(missing("group")) return(cat0("ERROR: you must provide your group"))
  if(!regexpr("\\.csv$", file)>0) return(cat0("ERROR: file must be a CSV"))
  if(missing("get_grade")) get_grade = F
  fl_name <- strsplit(file, "/")[[1]]
  fl_name <- fl_name[length(fl_name)]
  if(!sa_type[1] %in% c("manual", "heur")) return(cat0("ERROR: sa_type can only be either 'manual' or 'heur'"))
  if(sa_type[1] == "heur"){
    if(fl_name != "sa_heur.csv") return(cat0("ERROR: your file must be named 'sa_heur.csv'"))
  } else {
    if(fl_name != "sa_result.csv") return(cat0("ERROR: your file must be named 'sa_result.csv'"))
  }
  rm(fl_name)

  return(sa_perun(sol = file, gr = group, satype = sa_type[1], get_grade))
}

stand_util <- function(schedule, stand_info){
  utsa <- copy(utils_sa[[as.integer(substr(schedule,
                                           nchar(schedule) - 5,
                                           nchar(schedule) - 4))]])
  utsa[, poss := NULL]
  return(utsa)
}

#' Stand Allocation Heuristic (part 2) Evaluation
#'
#' @description
#' Self-evaluator for Activity 4 - SA Heuristic (part 2) of Airport operations.
#'
#' @param file File name in working directory or path to file. File ('sa_heur.txt') following the exact format from Atenea.
#' @param sch_to_test A vector with schedule numbers to check your heuristic against. Database has schedules and by default it runs all of them.
#' @param sa_csv File name in working directory or path to file. File ('sa_heur.csv') following the exact format from Atenea. Result of your heuristic on your group's schedule.
#' @param group Number of your group.
#'
#' @returns
#' Report with information about your result.
#'
#' @details
#' Maximum execution time of your code is 10 min.
#'
#' With a single schedule to test in *sch_to_test* the functions shows a report and plots your solution. For a correct visualization it is recommended to 'Export as Image...' the plot with a 1000x3000 ratio.
#' Input *sa_csv* and *group* are optional just for checking your schedule results. *group* is mandatory when having a *sa_csv* input.
#'
#' @examples
#' sae_heur2(file = "sa_heur.txt", sch_to_test = 1) # with a single schedule to test it shows report and plot
#' sae_heur2(file = "sa_heur.txt", sch_to_test = c(1, 3))
#' sae_heur2(file = "sa_heur.txt", sch_to_test = 1:2, sa_csv = "sa_heur.csv", group = 1)
sae_heur2 <- function(file, sch_to_test = 1:10, sa_csv, group, get_grade){
  if(missing("file")) return(cat0("ERROR: you must provide your file"))
  if(!regexpr("\\.txt$", file)>0) return(cat0("ERROR: file must be a plain TXT"))
  if(length(readLines(file)) == 0) return(cat0("ERROR: file is empty"))
  if(!missing("sa_csv")) {
    if(!is.na(sa_csv)) {
      if(!regexpr("\\.csv$", sa_csv)>0) return(cat0("ERROR: sa_csv must be a CSV"))
      fl_name <- strsplit(sa_csv, "/")[[1]]
      fl_name <- fl_name[length(fl_name)]
      if(fl_name != "sa_heur.csv") return(cat0("ERROR: your results file must be named 'sa_heur.csv'"))
      file.copy(from = sa_csv, to = paste0(tempdir(), "/sa_heur.csv"), overwrite = T)
    }
  }
  if(missing("get_grade")) get_grade = F
  fl_name <- strsplit(file, "/")[[1]]
  fl_name <- fl_name[length(fl_name)]
  if(fl_name != "sa_heur.txt") return(cat0("ERROR: your heuristic file must be named 'sa_heur.txt'"))
  rm(fl_name)

  # suppressWarnings(rm("stand_util"))
  suppressWarnings(rm(stand_util, envir = .GlobalEnv))

  el_fold <- getwd()

  if(!missing("sa_csv") & missing("group")) return(cat0("ERROR: you must provide your group"))

  n_sch <- unique(round(sch_to_test[sch_to_test > 0 & sch_to_test < 11]))
  if(length(n_sch) == 0) return(cat0("ERROR: sch_to_test must have values between 1 to 10"))
  if(sum(is.na(n_sch))) return(cat0("ERROR: sch_to_test must have values between 1 to 10"))

  file.copy(from = file, to = paste0(tempdir(), "/la_heur.txt"), overwrite = T)

  setwd(tempdir())
  on.exit({
    unlink("schedules/", recursive = T)
    unlink("stands_info.csv")
    unlink("la_heur.txt")
    unlink("el_res.csv")
    suppressWarnings(rm(sa_heur, stand_util, envir = .GlobalEnv))
    setwd(el_fold)
  })

  if(!check_functions("la_heur.txt", loops_max$sa_heur2)) {
    return(cat())
  }
  #test the code ----

  if(!codi_ok("la_heur.txt")) {
    return(cat())
  }

  fwrite(clean_s, "stands_info.csv")

  source("la_heur.txt")
  .GlobalEnv$stand_util <- stand_util

  suppressWarnings(dir.create("schedules"))
  for(i in seq(length(schedules))) fwrite(schedules[[i]], file = paste0("schedules/", names(schedules)[i],".csv"))

  res_sch <- 0
  if(!missing("sa_csv")){
    if(!is.na(sa_csv)) {
      sch_test <- unique(utils_sa[[group]], by = "callsign")
      for(inu in 1){
        estabe = 0
        cat0("## SA csv evaluation")
        loseu <- fread("sa_heur.csv", header = T)
        cat0("Your file:")
        print(loseu)
        cat0()
        if(length(sum(names(loseu) == "callsign")) != 1) {
          cat0("ERROR: No callsign column")
          cat0()
          next
        }
        if(length(sum(names(loseu) == "op")) != 1)  {
          cat0("ERROR: No op column")
          cat0()
          next
        }
        if(length(sum(names(loseu) == "stand")) != 1) {
          cat0("ERROR: No stand column")
          cat0()
          next
        }
        if(!identical(names(loseu), c("callsign", "op", "stand")))  {
          cat0("ERROR: Columns are not in the right order")
          cat0()
          next
        }
        if(sum(!(loseu$op %in% c("A","D","P")))>0)  {
          cat0("ERROR: op is not correct")
          cat0()
          next
        }
        res <- myTryCatch(sa_heur(schedule = list.files("schedules/", full.names = T)[group],
                                  stand_info = "stands_info.csv"), nmb = 3)

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
            cat0("ERROR: Output doesn't have all callsigns")
            next
          } else {
            estabe = estabe + 1
            estabe <- estabe + as.integer(sum(sch_test[, !callsign %in% loseu$callsign]) == 0)
          }
        }
      }
      res_sch <- 10*estabe/2
      cat0("Grade: ", res_sch)
      cat0()
      rm(loseu, res, estabe)
    }
  }

  cat0("## Heuristic evaluation")
  notaf <- c()
  for(el_j in n_sch){
    cat0("# File: ", eval_files$sa_heur$File[el_j])
    res <- myTryCatch(sa_heur(schedule = list.files("schedules/", full.names = T)[el_j],
                              stand_info = "stands_info.csv"), nmb = 3)

    cat0(res$tspent)
    if(!is.null(res$warning)) {
      cat("WARNING: ")
      cat0(as.character(res$warning))
    }
    if(!is.null(res$error)) {
      cat0("ERROR: Code doesn't run")
      cat0(as.character(res$error))
      notaf <- c(notaf, 0)
      next
    }
    res <- res$value

    fwrite(res, "el_res.csv")

    nota <- grade_mahe(get_eval(sol = "el_res.csv", gr = el_j)$r1, "heur", el_j)
    notaf <- c(notaf, nota)
    cat0("Grade: ", nota)
    cat0()
  }

  cat0("Heuristic grade (expected): ", round(mean(notaf), 2))
  cat0()
  if(!missing("sa_csv")) cat0("Final grade (expected): ", round((mean(notaf)*15 + res_sch*5)/20, 2))
  if(get_grade) return(round((mean(notaf)*15 + res_sch*5)/20, 2))
  return(cat())
}

#' Stand Allocation Heuristic (part 1) Evaluation
#'
#' @description
#' Self-evaluator for Activity 4 - SA Heuristic (part 1) of Airport operations.
#'
#' @param file File name in working directory or path to file. File ('stand_util.txt') following the exact format from Atenea.
#' @param sch_to_test A vector with schedule numbers to check your heuristic against. Database has schedules and by default it runs all of them.
#' @param st_fill File name in working directory or path to file. File ('lebl_stands.csv') following the exact format from Atenea. Stand priorities filled.
#'
#' @returns
#' Report with information about your result.
#'
#' @details
#' Maximum execution time of your code is 30 sec.
#'
#' Input *st_fill* can be evaluated independently from *file*.
#' .
#' @examples
#' sae_heur1(file = "stand_util.txt", sch_to_test = 1:2, st_fill = "lebl_stands.csv")
#' sae_heur1(file = "stand_util.txt", sch_to_test = 1:3)
#' sae_heur1(st_fill = "lebl_stands.csv")
sae_heur1 <- function(file, sch_to_test = 1:10, st_fill, get_grade){
  if(missing("file") & missing("st_fill")) return(cat0("ERROR: you must provide your file or st_fill"))
  if(!missing("file")){
    if(!regexpr("\\.txt$", file)>0) return(cat0("ERROR: file must be a plain TXT"))
    if(length(readLines(file)) == 0) return(cat0("ERROR: file is empty"))
    fl_name <- strsplit(file, "/")[[1]]
    fl_name <- fl_name[length(fl_name)]
    if(fl_name != "stand_util.txt") return(cat0("ERROR: your heuristic file must be named 'stand_util.txt'"))
    rm(fl_name)
  }
  if(!missing("st_fill")) {
    if(!is.na(st_fill)) {
      if(!regexpr("\\.csv$", st_fill)>0) return(cat0("ERROR: st_fill must be a CSV"))
      fl_name <- strsplit(st_fill, "/")[[1]]
      fl_name <- fl_name[length(fl_name)]
      if(fl_name != "lebl_stands.csv") return(cat0("ERROR: your LEBL stands file must be named 'lebl_stands.csv'"))
    }
  }
  if(missing("get_grade")) get_grade = F

  el_fold <- getwd()

  stands_clean <- copy(clean_s)
  stands_clean[is.na(stands_clean)] <- 0

  stands_clean2 <- copy(stands_clean)
  stands_clean2[T2 == 1, shut := 0]
  stands_clean2[T1 == 1 & shut == 0, shut := 5]
  stands_clean2[ramp == "R0", c("sch", "park") := 5]

  pena_fill = -10
  if(!missing("st_fill")){
    if(!is.na(st_fill)) {
      cat0("## Stand preferences evaluation")
      fill_seu <- fread(st_fill)
      if(!identical(names(fill_seu), c("ramp","stand","ac_max","S1","S2","S3","contact",
                              "T1","T2","sch","shut","no_sch","no_ue","park"))){
        cat0("Please, upload the right LEBL stands table")
      } else {
        pena_fill = 0
        stmix <- merge(stands_clean2, fill_seu, by = c("ramp","stand","ac_max","S1","S2","S3"))
        stmix[, id0 := seq(.N)]
        stmix[, `:=` (contact = abs(contact.x - contact.y), T1 = abs(T1.x - T1.y),
                      T2 = abs(T2.x - T2.y), sch = abs(sch.x - sch.y),
                      shut = abs(shut.x - shut.y), no_sch = abs(no_sch.x - no_sch.y),
                      no_ue = abs(no_ue.x - no_ue.y),  park = abs(park.x - park.y))]

        stmix[sch.x == 0 & sch.y != 0, sch := 4]
        stmix[shut.x == 0 & shut.y != 0, shut := 4]
        stmix[no_sch.x == 0 & no_sch.y != 0, no_sch := 4]
        stmix[no_ue.x == 0 & no_ue.y != 0, no_ue := 4]
        stmix[park.x == 0 & park.y != 0, park := 4]

        stmix[sch.x != 0 & sch.y == 0, sch := abs(6 - sch.x)]
        stmix[shut.x != 0 & shut.y == 0, shut := abs(6 - shut.x)]
        stmix[no_sch.x != 0 & no_sch.y == 0, no_sch := abs(6 - no_sch.x)]
        stmix[no_ue.x != 0 & no_ue.y == 0, no_ue := abs(6 - no_ue.x)]
        stmix[park.x != 0 & park.y == 0, park := abs(6 - park.x)]

        if(nrow(stmix) < 209) {
          pena_fill = pena_fill + 10
          cat0("Please check you have all rows and haven't changed any column with information: -10 p")
        }
        tst <- stmix[contact > 0, stand]
        if(length(tst) > 0) {
          pena_fill = pena_fill + length(tst)/2
          cat0("Inconsistent Contact information: -", length(tst)/2, " p")
          cat0("Stand/s: ", paste(tst, collapse = ", "))
        }
        tst <- stmix[T1 > 0 | T2 > 0, stand]
        if(length(tst) > 0) {
          pena_fill = pena_fill + length(tst)/2
          cat0("Inconsistent Terminal information: -", length(tst)/2, " p")
          cat0("Stand/s: ", paste(tst, collapse = ", "))
        }
        tst <- stmix[sch > 1 & sch < 5, stand]
        if(length(tst) > 0) {
          pena_fill = pena_fill + length(tst)/2
          cat0("Inconsistent Schengen information: -", length(tst)/2, " p")
          cat0("Stand/s: ", paste(tst, collapse = ", "))
        }
        tst <- stmix[shut > 1 & shut < 5, stand]
        if(length(tst) > 0) {
          pena_fill = pena_fill + length(tst)/2
          cat0("Inconsistent Shuttle information: -", length(tst)/2, " p")
          cat0("Stand/s: ", paste(tst, collapse = ", "))
        }
        tst <- stmix[no_sch > 1 & no_sch < 5, stand]
        if(length(tst) > 0) {
          pena_fill = pena_fill + length(tst)/2
          cat0("Inconsistent Non-Schengen information: -", length(tst)/2, " p")
          cat0("Stand/s: ", paste(tst, collapse = ", "))
        }
        tst <- stmix[no_ue > 1 & no_ue < 5, stand]
        if(length(tst) > 0) {
          pena_fill = pena_fill + length(tst)/2
          cat0("Inconsistent Non-UE information: -", length(tst)/2, " p")
          cat0("Stand/s: ", paste(tst, collapse = ", "))
        }
        tst <- stmix[park > 1 & park < 5, stand]
        if(length(tst) > 0) {
          pena_fill = pena_fill + length(tst)/2
          cat0("Inconsistent Parking information: -", length(tst)/2, " p")
          cat0("Stand/s: ", paste(tst, collapse = ", "))
        }
        pena_fill = -min(10, pena_fill)
        if(pena_fill < 0) cat0("\nPenalty for Stand preferences: ", pena_fill, "/10 = ", round(pena_fill/10, 2), " p")
        if(pena_fill == 0) cat0("Your LEBL stands file is correct: -0 p")
        cat0()
      }
    }
  }
  pena_fill <- round(pena_fill/10, 2)
  rm(stands_clean2)

  notaf = 0
  if(!missing("file")){
    n_sch <- unique(round(sch_to_test[sch_to_test > 0 & sch_to_test < 11]))
    if(length(n_sch) == 0) return(cat0("ERROR: sch_to_test must have values between 1 to 10"))
    if(sum(is.na(n_sch))) return(cat0("ERROR: sch_to_test must have values between 1 to 10"))

    file.copy(from = file, to = paste0(tempdir(), "/la_heur.txt"), overwrite = T)

    setwd(tempdir())
    on.exit({
      unlink("schedules/", recursive = T)
      unlink("data/", recursive = T)
      unlink("stands_info.csv")
      unlink("la_heur.txt")
      suppressWarnings(rm(stand_util, envir = .GlobalEnv))
      setwd(el_fold)
    })

    if(!check_functions("la_heur.txt", loops_max$sa_heur1)) {
      return(cat())
    }
    #test the code ----

    if(!codi_ok("la_heur.txt")) {
      return(cat())
    }

    source("la_heur.txt")

    suppressWarnings(dir.create("schedules"))
    suppressWarnings(dir.create("data"))

    for(i in seq(length(schedules))) fwrite(schedules[[i]], file = paste0("schedules/", names(schedules)[i],".csv"))
    for(i in seq(length(extra_sa))) fwrite(extra_sa[[i]], file = paste0("data/", names(extra_sa)[i],".csv"))
    fwrite(stands_clean, "stands_info.csv")

    cat0("## Heuristic evaluation")
    notaf <- c()
    for(el_j in sch_to_test){
      cat0("# File: ", eval_files$sa_heur$File[el_j])
      nota = 10
      err1_d = NULL
      err2_d = NULL
      err1 = NULL
      err2 = NULL
      res <- myTryCatch(.GlobalEnv$stand_util(schedule = list.files("schedules/")[el_j], stand_info = "stands_info.csv"), nmb = 2)
      cat0(res$tspent)

      if(!is.null(res$warning)) {
        cat("WARNING: ")
        cat0(as.character(res$warning))
      }

      if(!is.null(res$error)) {
        cat0("ERROR: Code doesn't run")
        cat0(as.character(res$error))
        notaf <- c(notaf, 0)
        next
      }
      res <- res$value
      cat0("Your file:")
      print(res)

      colmeu <- data.table(la_col = c("callsign","id","op","tail","ramp","stand","op_time1","op_time2","time1","time2","util"),
                           la_class = c("character", "integer", rep("character", 4), rep("ITime",4), "numeric"), id = 1:11,
                           pena = c(rep(10,2), rep(1, 3), 3, rep(1,4), 2))
      colseu <- sapply(res, class)
      colseu <- data.table(la_col = names(colseu), la_class2 = colseu)

      colck <- merge(colmeu, colseu, all = T, sort = F)
      e1 <- colck[is.na(la_class2)]
      e2 <- colck[is.na(la_class)]
      e3 <- colck[!is.na(la_class) & !is.na(la_class2)]
      e3 <- e3[la_class != la_class2]

      if(e1[,.N] > 0) {
        xres <- e1[, cat0(la_col, " column missing: -", pena, " p"), by = id]
        nota = nota - e1[, pena]
      }
      if(e2[,.N] > 0) {
        xres <- e2[, cat0(la_col, " column not wanted: -1 p"), by = id]
        nota = nota - e2[,.N]
      }
      if(e3[,.N] > 0){
        xres <- e3[, cat0(la_col, " class is not ", la_class, ": -0.25 p"), by = id]
        nota = nota - e3[,.N]*0.25
        cat("*Forcing class correction: ")
        fixclass <- myTryCatch({fixclass <- e3[,paste0("res[, ", la_col, " := as.",la_class,"(",la_col,")]")]
        fixclass <- lapply(fixclass, function(x) eval(parse(text = x)))}, nmb = 2)

        if(!is.null(fixclass$warning)) {
          cat0("Didn't work")
          cat("WARNING: ")
          cat0(as.character(fixclass$warning))
          notaf <- c(notaf, 0)
          next
        }

        if(!is.null(fixclass$error)) {
          cat0("Didn't work")
          cat("ERROR: ")
          cat0(as.character(fixclass$error))
          notaf <- c(notaf, 0)
          next
        }
        if(is.null(fixclass$error) & is.null(fixclass$warning)) cat0("Worked")
      }

      elmeu <- copy(utils_sa[[el_j]])
      elmeu[, poss := NULL]
      setnames(res, "id", "id2")

      if("util" %in% colseu$la_col) setnames(res, "util", "util2")

      les_col <- c("callsign","op","tail","ramp",
                   "stand","op_time1","op_time2","time1","time2")
      meuseu <- merge(elmeu, res, by = les_col[les_col %in% names(res)],
                      all = T, allow.cartesian = T)

      if("util" %in% colseu$la_col){
        meuseu[, udif := abs(util - util2)]
        err2_d <- meuseu[udif > 0.1]
        meuseu[, udif := NULL]
        suppressWarnings(err2_d[, udif := NULL])
        err2 <- err2_d[, unique(callsign)]
      }

      err1_d <- meuseu[is.na(id2) | is.na(id)]
      err1 <- err1_d[, unique(callsign)]
      rowerr <- nrow(unique(rbind(err1_d, err2_d)))/nrow(meuseu)*100
      cat0()

      if(length(err1) > 0) {
        cat0(length(err1), " callsigns with wrong information (e.g. time1, ramp, ...)")
        cat0("id and util: right solution -- id2 and util2: your solution")
        cat0()
        print(err1_d, nrows = 10)
      }
      if(length(err2) > 0) {
        cat0(length(err2), " callsigns with wrong util (< or > 0.1 from expected)")
        cat0("id and util: right solution -- id2 and util2: your solution")
        cat0()
        print(err2_d, nrows = 10)
      }
      if(length(unique(c(err1, err2))) > 0) cat0(round(rowerr, 1), "% of incorrect rows: -", round(rowerr/10, 2), " p")

      nota <- nota - round(rowerr/10, 2)
      nota <-  max(nota, 0)
      cat0("Grade: ", nota)
      notaf <- c(notaf, nota)
      cat0()
    }
    cat0("Heuristic grade (expected): ", round(mean(notaf), 2))
  }

  if(!missing(file) & !missing(st_fill)) cat0("\nFinal grade (expected): ", max(0, round(mean(notaf), 2) + pena_fill))
  if(get_grade) return(max(0, round(mean(notaf), 2) + pena_fill))
  return(cat())
}
