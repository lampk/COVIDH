get_case_bctruc_data <- function(case_url,
                                 bctruc_url,
                                 download = c("both", "case", "bctruc"),
                                 outdir = file.path("data", "HCDC", "database", "daily"),
                                 timestamp = NULL) {
  require(googlesheets4)
  require(tidyverse)
  require(clock)
  require(readr)

  ## authorization
  gs4_auth(email = "ncov2019tphcm@gmail.com")

  if (is.null(timestamp)) {
    ## get current datetime
    timestamp <- gsub(pattern = "-|:| |+", replacement = "", x = Sys.time())
  }

  if (download %in% c("both", "case")) {
    # case data
    message("Get case data !!! \n")
    sheets1 <- sheet_names(ss = case_url)
    sheets2 <- paste("sheet", formatC(1:length(sheets1), width = nchar(length(sheets1)), flag = "0"), sep = "_")
    for (i in c(1:length(sheets2))) {
      assign(sheets2[i],
             read_sheet(case_url, sheet = sheets1[i], col_types = "c") %>% mutate(sheet_name = sheets1[i]))
    }

    data_imported <- eval(parse(text = paste0("list(", paste0(sheets2, " = get('", sheets2, "')", collapse = ", "), ")")))
    saveRDS(data_imported, file = file.path(outdir, "hcmcovid19", paste0("HCDCdata_linelist_raw_", timestamp, ".rds")))
  }

  if (download %in% c("both", "bctruc")) {
    ## bctruc data
    message("Get bctruc data !!! \n")
    sheets <- sheet_names(ss = bctruc_url)
    sheets2 <- sheets[sheets != "TỔNG HỢP"] ## exclude "TỔNG HỢP"
    sheets3 <- paste("sheet", formatC(1:length(sheets2), width = nchar(length(sheets2)), flag = "0"), sep = "_")

    for (i in c(1:length(sheets3))) {
      assign(sheets3[i],
             read_sheet(bctruc_url, sheet = sheets2[i], col_types = "c") %>% mutate(sheet_name = sheets2[i]))
    }

    data_imported <- eval(parse(text = paste0("list(", paste0(sheets3, " = get('", sheets3, "')", collapse = ", "), ")")))
    saveRDS(data_imported, file = file.path(outdir, "bctruc", "combine", paste0("HCDCdata_BCTRUC_", timestamp, ".rds")))
  }
}

bctruc_split <- function(input, outdir = file.path("data", "HCDC", "database", "daily", "bctruc", "separated")) {
  require(dplyr)
  require(linelist)
  timestamp <- gsub(pattern = "-|:| |+", replacement = "", x = Sys.time())
  data_imported <- readRDS(input)

  #browser()
  if (length(data_imported[[1]]) == 2) {
    sheet_info <- data.frame(sheet = names(data_imported),
                             name = sapply(1:length(data_imported), function(i){data_imported[[i]]$name})) %>%
      clean_variables() %>%
      mutate(file = paste0("bctruc_", name, "_", timestamp, ".rds"))
    for (i in c(1:nrow(sheet_info))) {
      saveRDS(data_imported[[i]]$data %>% mutate(sheet_name = sheet_info$name[i]),
              file = file.path(outdir, sheet_info$file[i]))
    }
  } else {
    sheet_info <- data.frame(sheet = names(data_imported),
                             name = sapply(1:length(data_imported), function(i){data_imported[[i]]$sheet_name[1]})) %>%
      clean_variables() %>%
      mutate(file = paste0("bctruc_", name, "_", timestamp, ".rds"))
    for (i in c(1:nrow(sheet_info))) {
      saveRDS(data_imported[sheet_info$sheet[i]], file = file.path(outdir, sheet_info$file[i]))
    }
  }
}


remove_dupfile <- function(path = file.path("data", "HCDC", "database", "daily", "bctruc", "separated")) {
  files <- list.files(path = path, full.names = TRUE)
  tmp <- do.call("rbind",
                 lapply(files, function(x){
                   x1 <- basename(x)
                   x2 <- gsub(pattern = "bctruc_|[.]rds", replacement = "", x = x1)
                   dt <- substr(x = x2, start = nchar(x2) - 13, stop = nchar(x2))
                   data.frame(
                     idx = as.integer(strsplit(x2, split = "_")[[1]][1]),
                     dt = dt,
                     name = gsub(pattern = paste0("_", dt), replacement = "", x = x2),
                     file = x,
                     md5sum = tools::md5sum(x)
                   )
                 })) %>%
    group_by(name, md5sum) %>%
    arrange(dt) %>%
    mutate(keep = c(1, rep(0, n() - 1))) %>%
    ungroup() %>%
    arrange(name, dt)
  toremove <- tmp$file[tmp$keep == 0]
  cat(paste(length(toremove), "files will be removed !!! \n"))
  file.remove(toremove)
}

## create first final dataset
# dat <- readRDS(file = file.path("data", "HCDC", "database", "daily", "linelist", "hcdc_linelist_20210723044846.rds"))
# dat$pid <- 1:nrow(dat)
# tmp <- do.call(rbind,
#                lapply(list.files(path = file.path("data", "HCDC", "database", "daily", "bctruc", "separated")),
#                       function(x) {
#                         x2 <- gsub(pattern = "bctruc_|[.]rds", replacement = "", x = x)
#                         dt <- substr(x = x2, start = nchar(x2) - 13, stop = nchar(x2))
#                         data.frame(
#                           idx = as.integer(strsplit(x2, split = "_")[[1]][1]),
#                           dt = dt,
#                           name = gsub(pattern = paste0("_", dt), replacement = "", x = x2),
#                           file = x
#                         )
#                       })) %>%
#   group_by(idx) %>%
#   arrange(desc(dt)) %>%
#   slice(1) %>%
#   ungroup() %>%
#   filter(!is.na(as.integer(idx)) & !name %in% c("0806_tu_06h_06h0906", "0906_tu_06h_06h_1006", "1006_tu_06h_06h_1106",
#                                                 "1106_tu_08h_08h_1206", "1206_tu_08h_08h_1306", "1306_tu_08h_08h_1406",
#                                                 "1406_tu_08h_08h_1506"))
# final_20210723044846 <- list(data = dat,
#                              file = c(tmp$file, "HCDCdata_linelist_raw_20210720073835.rds"))
# saveRDS(final_20210723044846, file = file.path("data", "HCDC", "database", "final", "final_linelist_20210723044846.rds"))

bctruc_new <- function(path = file.path("data", "HCDC", "database", "daily", "bctruc", "separated"),
                       timestamp = NULL) {
  require(dplyr)
  if (is.null(timestamp)) {
    ## get current datetime
    timestamp <- gsub(pattern = "-|:| |+", replacement = "", x = Sys.time())
  }

  ## select updated files
  tmp <- do.call(rbind, lapply(list.files(path = file.path("data", "HCDC", "database", "final"), full.names = TRUE),
                               function(x){file.info(x)}))
  file_old <- readRDS(file = rownames(tmp)[which.max(tmp$mtime)])$file
  files <- list.files(path = path)
  exclude <- c("0806_tu_06h_06h0906", "0906_tu_06h_06h_1006", "1006_tu_06h_06h_1106",
               "1106_tu_08h_08h_1206", "1206_tu_08h_08h_1306", "1306_tu_08h_08h_1406",
               "1406_tu_08h_08h_1506", "11_cuahuyen_dungxoa")
  tmp <- do.call("rbind",
                 lapply(files, function(x){
                   x0 <- gsub(pattern = "bctruc_|[.]rds", replacement = "", x = x)
                   dt <- substr(x = x0, start = nchar(x0) - 13, stop = nchar(x0))
                   data.frame(
                     idx = as.integer(strsplit(x0, split = "_")[[1]][1]),
                     dt = dt,
                     name = gsub(pattern = paste0("_", dt), replacement = "", x = x0),
                     file = x
                   )
                 })) %>%
    group_by(idx) %>%
    arrange(desc(dt)) %>%
    slice(1) %>%
    ungroup() %>%
    filter(!is.na(as.integer(idx)) & !name %in% exclude & !file %in% file_old)

  ## import & combine
  #browser()
  if (nrow(tmp) == 0) {
    message("No new bctruc data !!!")
  } else {
    sheets <- paste("sheet", formatC(tmp$idx, width = nchar(nrow(tmp)), flag = "0"), sep = "_")
    for (i in c(1:nrow(tmp))) {
      assign(sheets[i],
             readRDS(file = file.path(path, tmp$file[i]))[[1]] %>%
               mutate(sheet_name = tmp$name[i]))
    }
    bctruc <- eval(parse(text = paste0("list(", paste0(sheets, " = get('", sheets, "')", collapse = ", "), ")")))
    #browser()
    output <- list(data = bctruc,
                   file = tmp$file)
    saveRDS(output, file = file.path(dirname(path), "new", paste0("bctruc_", timestamp, ".rds")))
  }
}

bctruc_process2 <- function(bctruc_file,
                            outdir = file.path("data", "HCDC", "database", "daily", "bctruc", "cleaned"),
                            timestamp = NULL) {
  require(dplyr)
  require(writexl)
  require(linelist)

  if (is.null(timestamp)) {
    ## get current datetime
    timestamp <- gsub(pattern = "-|:| |+", replacement = "", x = Sys.time())
  }
  files <- bctruc_file$file
  bctruc_file <- bctruc_file$data

  ## get all variable's names
  namevars_list <- lapply(names(bctruc_file), function(x){names(bctruc_file[[x]])})
  namevars_unq <- unique(unlist(namevars_list))
  namevars0 <- do.call(what = "cbind",
                       args = lapply(1:length(namevars_list),
                                     function(i){
                                       sapply(namevars_unq,
                                              function(x){
                                                ifelse(any(namevars_list[[i]] == x),
                                                       which(namevars_list[[i]] == x),
                                                       0)
                                              })
                                     }))
  colnames(namevars0) <- names(namevars_list)
  #browser()

  ## identified new column names
  label_bctruc <- readRDS(file = file.path("data", "info", "label_bctruc.rds"))
  #label_bctruc$name_cor[label_bctruc$name_org == "HỌ VÀ TÊN (IN HOA) KHÔNG GHI CHÚ SAU HỌ TÊN"] <- "bc_name"
  #label_bctruc$name_cor[label_bctruc$name_org == "HỌ TÊN (IN HOA)"] <- "bc_name"
  curvar <- row.names(namevars0)
  newvar <- curvar[! curvar %in% label_bctruc$name_org]
  #browser()

  if (length(newvar) > 0){
    message(paste("New columns in bctruc:", paste0("[", newvar, "]", collapse = ", ")))
    newcor <- paste("bc", clean_variables(data.frame(x = newvar))[[1]], sep = "_")

    ## update label_bctruc
    label_bctruc <- rbind(label_bctruc, cbind(name_org = newvar, name_cor = newcor))
    saveRDS(label_bctruc, file = file.path("data", "info", "label_bctruc.rds"))
  }
  #browser()

  ## rename & create new columns
  curvarcor <- unique(label_bctruc$name_cor)
  #browser()

  for (i in c(1:length(bctruc_file))) {
    m <- ncol(bctruc_file[[i]])
    ## rename
    names(bctruc_file[[i]]) <- label_bctruc$name_cor[match(x = names(bctruc_file[[i]]), table = label_bctruc$name_org)]
    ## create new columns
    needvar <- unique(label_bctruc$name_cor[! label_bctruc$name_cor %in% names(bctruc_file[[i]])])
    if (length(needvar) > 0) {
      bctruc_file[[i]][, needvar] <- NA
    }
  }
  #browser()

  ## combine all datasets
  bctruc_all0 <- do.call("rbind", arg = bctruc_file) %>%
    filter(!is.na(bc_name) & !bc_name %in% c("QUA SHEET 68 NHA!")) %>%
    mutate(bc_idx = 1:n(),
           bc_name = toupper(bc_name)) %>%
    mutate_all(.funs = toupper) %>%
    mutate(bc_tel2 = gsub(pattern = "[[:alpha:]]|[[:punct:]]|[[:space:]]", replacement = "", x = bc_tel))

  ## check duplicated
  ### first round
  duppat <- "\\( TRÙNG|\\(TRÙNG|TRÙNG\\(|TRÙNG \\(|TRÙNG TUA|TRÙNG CA|TRÙNG PHIÊN|TRÙNG TÊN|TRÙNG NGÀY"
  idx <- unlist(apply(bctruc_all0, 2, function(y){grep(pattern = duppat, x = y)}))
  if (length(idx) > 0) {
    bctruc_all1 <- bctruc_all0[-idx,]
  } else {
    bctruc_all1 <- bctruc_all0
  }


  #browser()
  ### automatic check
  check_dup <- iden_dup(data = bctruc_all1, type = "bctruc")
  bctruc_all2 <- link_merge(data = bctruc_all1, check_dup = check_dup)
  bctruc_all <- bctruc_all2[-check_dup$duplication,]

  #browser()

  ## correct first
  source(file.path("R", "correct_bctruc.R"), local = TRUE)

  #browser()

  ## clean yob
  bctruc_all$bc_yob <- clean_yob(yob = bctruc_all$bc_yob)

  ## clean date
  ### test_01_date_taken
  bctruc_all$bc_test_01_date_taken2 <- date_parse(clean_date(date = bctruc_all$bc_test_01_date_taken, guess_year = 2021))
  ### test_02_date_taken
  bctruc_all$bc_test_02_date_taken2 <- date_parse(clean_date(date = bctruc_all$bc_test_02_date_taken, guess_year = 2021))
  ### test_03_date_taken
  bctruc_all$bc_test_03_date_taken2 <- date_parse(clean_date(date = bctruc_all$bc_test_03_date_taken, guess_year = 2021))
  ### test_04_date_taken
  bctruc_all$bc_test_04_date_taken2 <- date_parse(clean_date(date = bctruc_all$bc_test_04_date_taken, guess_year = 2021))
  ### test_05_date_taken
  bctruc_all$bc_test_05_date_taken2 <- date_parse(clean_date(date = bctruc_all$bc_test_05_date_taken, guess_year = 2021))

  ### date report
  bctruc_all$bc_date_report2 <- date_parse(clean_date_report(date_report = bctruc_all$bc_date_report, guess_year = 2021))

  ### bc_clin_date_onset
  bctruc_all$bc_clin_date_onset2 <- date_parse(clean_date(date = bctruc_all$bc_clin_date_onset, guess_year = 2021))
  ### bc_qrt_date
  bctruc_all$bc_qrt_date2 <- date_parse(clean_date(date = bctruc_all$bc_qrt_date, guess_year = 2021))

  ## test
  test_date_taken <- dplyr::select(bctruc_all, bc_idx, bc_test_01_date_taken2, bc_test_02_date_taken2, bc_test_03_date_taken2, bc_test_04_date_taken2, bc_test_05_date_taken2) %>%
    gather(-bc_idx, key = "test_number", value = "test_date_taken") %>%
    mutate(test_number = gsub(pattern = "bc_test_|_date_taken2", replacement = "", x = test_number))

  test_result <- dplyr::select(bctruc_all, bc_idx, bc_test_01_result, bc_test_02_result, bc_test_03_result, bc_test_04_result, bc_test_05_result) %>%
    gather(-bc_idx, key = "test_number", value = "test_result") %>%
    mutate(test_number = gsub(pattern = "bc_test_|_result", replacement = "", x = test_number),
           test_result = ifelse(is.na(test_result), NA,
                                ifelse(grepl(pattern = "am", x = tolower(stringi::stri_trans_general(str = test_result,  id = "Latin-ASCII"))), "AM",
                                       ifelse(grepl(pattern = "duong", x = tolower(stringi::stri_trans_general(str = test_result,  id = "Latin-ASCII"))), "DUONG", NA))))

  test <- list(test_date_taken, test_result) %>%
    reduce(full_join, by = c("bc_idx", "test_number"))

  test2 <- test %>%
    filter(test_result == "DUONG") %>%
    group_by(bc_idx) %>%
    arrange(test_date_taken) %>%
    slice(1) %>%
    ungroup() %>%
    mutate(bc_first_date_pos = test_date_taken)

  ### first date
  bc_alldate <- merge(dplyr::select(bctruc_all, bc_idx, bc_clin_date_onset2, bc_date_report2),
                      dplyr::select(test2, bc_idx, bc_first_date_pos),
                      by = "bc_idx", all.x = TRUE) %>%
    mutate_all(.funs = as.character) %>%
    mutate(bc_date_report2 = ifelse(is.na(bc_date_report2), NA,
                                    ifelse(bc_date_report2 == "2021-06-07" & bc_first_date_pos %in% c("2021-07-04", "2021-07-05"), "2021-07-06", bc_date_report2))) %>%
    gather(-bc_idx, key = "type", value = "date") %>%
    mutate(date = date_parse(date)) %>%
    arrange(bc_idx, date) %>%
    group_by(bc_idx) %>%
    arrange(date) %>%
    summarise(bc_date_report2 = date[type == "bc_date_report2"],
              bc_first_date_pos = date[type == "bc_first_date_pos"],
              bc_first_date = date[type %in% c("bc_clin_date_onset2", "bc_first_date_pos")][1],
              bc_first_date_all = na.omit(date)[1]) %>%
    ungroup()


  ## classification
  #browser()

  ## final
  qh <- c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12",
          "BINH CHANH", "BINH TAN", "BINH THANH", "CAN GIO", "CU CHI", "GO VAP",
          "HOC MON", "NHA BE", "PHU NHUAN", "TAN BINH", "TAN PHU", "THU DUC")

  bctruc_all2 <- merge(
    bctruc_all %>%
      mutate(bc_idx2 = 1:n(),
             bc_yob = as.integer(bc_yob),
             bc_res_loc_dis_tmp = toupper(stringi::stri_trans_general(str = bc_res_loc_dis,  id = "Latin-ASCII")),
             bc_res_loc_dis2 = ifelse(is.na(bc_res_loc_dis_tmp), NA,
                                      ifelse(grepl(pattern = "12", x = bc_res_loc_dis_tmp), "12",
                                             ifelse(grepl(pattern = "11", x = bc_res_loc_dis_tmp), "11",
                                                    ifelse(grepl(pattern = "10", x = bc_res_loc_dis_tmp), "10",
                                                           ifelse(grepl(pattern = "9", x = bc_res_loc_dis_tmp), "09",
                                                                  ifelse(grepl(pattern = "8", x = bc_res_loc_dis_tmp), "08",
                                                                         ifelse(grepl(pattern = "7", x = bc_res_loc_dis_tmp), "07",
                                                                                ifelse(grepl(pattern = "6", x = bc_res_loc_dis_tmp), "06",
                                                                                       ifelse(grepl(pattern = "5", x = bc_res_loc_dis_tmp), "05",
                                                                                              ifelse(grepl(pattern = "4", x = bc_res_loc_dis_tmp), "04",
                                                                                                     ifelse(grepl(pattern = "3", x = bc_res_loc_dis_tmp), "03",
                                                                                                            ifelse(bc_res_loc_dis_tmp %in% c("02", "QUAN 2"), "02",
                                                                                                                   ifelse(bc_res_loc_dis_tmp %in% c("01", "QUAN 1"), "01",
                                                                                                                          ifelse(bc_res_loc_dis_tmp %in% c(qh), bc_res_loc_dis_tmp, NA)))))))))))))),
             bc_nhapcanh = ifelse(grepl(pattern = "nhập cảnh|nhap canh", x = tolower(bc_case_direct)), 1,
                                  ifelse(grepl(pattern = "nhập cảnh|nhap canh", x = tolower(bc_note)), 1,
                                         ifelse(grepl(pattern = "nhập cảnh|nhap canh", x = tolower(bc_group)), 1,
                                                ifelse(grepl(pattern = "nhập cảnh|nhap canh", x = tolower(bc_res_loc_dis)), 1, 0)))),
             bc_reason_test = ifelse(is.na(bc_group), NA,
                                     ifelse(bc_group %in% c("SÀNG LỌC TẠI BV", "TẦM SOÁT TẠI BV CÓ TRIỆU CHỨNG", "SÀNG LỌC BV") | (!is.na(bc_class) & bc_class %in% c("SÀNG LỌC TẠI BV")), toupper("Tầm soát người đến khám bệnh"),
                                            ifelse(bc_group %in% c("KHU PHONG TỎA", "KHU CÁCH LY", "KCL-THIẾU THÔNG TIN", "CÁCH LY TẠI NHÀ", "F1") | (!is.na(bc_class) & bc_class %in% c("KHU CÁCH LY", "KHU PHONG TỎA", "F1 TRONG KHU CÁCH LY", "F1 TRONG KHU PHONG TỎA")), toupper("Tầm soát người nguy cơ cao/rất cao"),
                                                   ifelse(bc_group %in% c("MỞ RỘNG KHU XÉT NHIỆM", "MỞ RỘNG  XÉT NHIỆM"), toupper("Tầm soát người nguy cơ"),
                                                          ifelse(bc_group %in% c("CỘNG ĐỒNG", "SÀNG LỌC CỘNG ĐỒNG", "XN  CỘNG ĐỒNG", "CHƯA NGUỒN LÂY"), toupper("Giám sát cộng đồng"),
                                                                 ifelse(bc_group %in% c("XN TẦM SOÁT ĐỊNH KỲ", "TẦM SOÁT ĐỊNH KỲ", "PHƠI NHIỄM NGHỀ NGHIỆP", "PHƠI NHIỄM"), toupper("Giám sát thường quy"),
                                                                        ifelse(bc_group %in% c("TEST NHANH DƯƠNG TÍNH"), toupper("Test nhanh dương tính"),
                                                                               ifelse(bc_group %in% c("CÓ NGUỒN LÂY", "CA CĐ CÓ NGUỒN LÂY", "CỘNG ĐỒNG CÓ NGUỒN LÂY"), toupper("Tầm soát người nguy cơ"),
                                                                                      ifelse(bc_group %in% c("SAU CLTT"), toupper("Sau cách ly tập trung"),
                                                                                             ifelse(bc_group %in% c("CÁCH LY TẠI NHÀ SAU ĐIỀU TRỊ", "SAU XUẤT VIỆN", "TÁI DƯƠNG TÍNH", "CA TÁI DƯƠNG"), toupper("Sau điều trị"),
                                                                                                    ifelse(bc_group %in% c("THUYỀN VIÊN", "NHẬP CẢNH", "NHẬP CẢNH TRÁI PHÉP"), toupper("Kiểm dịch"),
                                                                                                           ifelse(bc_group %in% c("ĐANG ĐIỀU TRA", "ĐANG XÁC MINH", "X`", "THIẾU THÔNG TIN"), toupper("Không rõ"), NA
                                                                                                           ))))))))))))
             # ,
             # bc_group = ifelse(is.na(bc_group), NA,
             #                   ifelse(bc_group %in% toupper(c("Sàng lọc tại BV", "Sàng lọc tại Bv", "Sàng lọc bv", "Tầm soát tại BV có triệu chứng")), "SÀNG LỌC TẠI BV",
             #                          ifelse(bc_group %in% c("khu phong tỏa", "Khu phong tỏa", "KHU PHONG TỎA"), "KHU PHONG TỎA", bc_group))),
             # bc_type = ifelse(bc_nhapcanh == 1, "Nhập cảnh",
             #                  ifelse(!is.na(bc_group) & tolower(bc_group) %in% c("khu cách ly", "kcl-thiếu thông tin"), "Khu cách li",
             #                         ifelse(!is.na(bc_group) & tolower(bc_group) %in% c("khu phong tỏa"), "Khu phong toả",
             #                                ifelse(!is.na(bc_group) & tolower(bc_group) %in% c("cách ly tại nhà"), "Cách ly tại nhà",
             #                                       ifelse(!is.na(bc_group) & tolower(bc_group) %in% c("sau cách ly tập trung", "sau cách ly tt", "sau cltt"), "Sau cách ly tập trung",
             #                                              ifelse(!is.na(bc_group) & tolower(bc_group) %in% c("phơi nhiễm nghề nghiệp", "phơi nhiễm"), "Phơi nhiễm nghề nghiệp",
             #                                                     ifelse(!is.na(bc_group) & tolower(bc_group) %in% c("sau xuất viện"), "Sau xuất viện",
             #                                                            ifelse(!is.na(bc_group) & tolower(bc_group) %in% c("tầm soát tại bv có triệu chứng", "sàng lọc tại bv"), "sàng lọc tại bv",
             #                                                                   ifelse(!is.na(bc_group) & tolower(bc_group) %in% c("có nguồn lây", "ca cđ có nguồn lây"), "Có nguồn lây",
             #                                                                          ifelse(!is.na(bc_group) & tolower(bc_group) %in% c("Chưa nguồn lây"), "Chưa nguồn lây",
             #                                                                                 ifelse(is.na(bc_group) | (!is.na(bc_group) & bc_group %in% c("Đang điều tra")), "Chưa rõ",
             #                                                                                        ifelse(bc_group %in% c("Mở rộng khu xét nhiệm"), "Tầm soát cộng đồng", "Khác"))))))))))))
      ) %>%
      dplyr::select(-bc_res_loc_dis_tmp, -bc_date_report2),
    bc_alldate,
    by = "bc_idx", all.x = TRUE
  )

  output <- list(data = bctruc_all2,
                 file = files)
  saveRDS(output, file = file.path(outdir, paste0("bctruc_cleaned_", timestamp, ".rds")))
}

link_current_bctruc <- function(bctruc,
                                outdir = file.path("data", "HCDC", "database", "final"),
                                timestamp = NULL) {
  require(RecordLinkage)
  tmp0 <- toupper(gsub(pattern = "bctruc_|[.]rds", replacement = "", x = bctruc$file))
  sheet <- substr(x = tmp0, start = 1, stop = nchar(tmp0) - 15)

  ## select updated final files
  tmp <- do.call(rbind, lapply(list.files(path = file.path("data", "HCDC", "database", "final"), full.names = TRUE),
                               function(x){file.info(x)}))
  final_file <- readRDS(file = rownames(tmp)[which.max(tmp$mtime)])
  current <- subset(final_file$data, !sheet_name %in% sheet)
  files <- c(final_file$file[! final_file$file %in% bctruc$file], bctruc$file)
  bctruc <- bctruc$data
  newvar <- names(current)[!names(current) %in% names(bctruc)]
  bctruc[, newvar] <- NA
  newvar <- names(bctruc)[!names(bctruc) %in% names(current)]
  current[, newvar] <- NA

  ## merge dataset
  ktc <- c("KHÔNG CÓ TC", "KHONG", "KHÔNG", "KO", "KHÔNG CÓ", "KHÔNG TRIỆU CHỨNG.", "KHÔNG TRIỆU CHỨNG", "KHÔNG TRIỆU CHỨNG LÂM SÀNG.", "KKHÔNG", "BÌNH THƯỜNG", "BINH THƯỜNG", "KHÔNG RÕ")
  tphcm <- c("01", "02", "03", "04", "05", "Q5", "06", "07", "08", "09", "10", "11", "12",
             "TAN PHU", "BINH TAN", "PHU NHUAN", "THU DUC", "GO VAP", "BINH THANH", "HOC MON", "HÓC MÔN",
             "CU CHI", "TAN BINH", "BINH CHANH", "NHA BE", "CAN GIO")

  allcase00 <- rbind(current %>% mutate(flow = "current"),
                     bctruc %>% mutate(pid = (1:n()) + nrow(current),
                                       flow = "new"))
  #browser()

  allcase00$bc_date_report2[is.na(allcase00$bc_date_report2) & !is.na(allcase00$bc_date_report) & allcase00$bc_date_report == "44393,49764" &
                              !is.na(allcase00$sheet_name) & allcase00$sheet_name == "42_6H_16_7_6H_17_7_2021"] <- date_parse("2021-07-16")
  allcase00$bc_date_report2[is.na(allcase00$bc_date_report2) & is.na(allcase00$bc_date_report) & !is.na(allcase00$name) & allcase00$name == "NGUYỄN THỊ PHƯƠNG THẢO" &
                              !is.na(allcase00$sheet_name) & allcase00$sheet_name == "46_6H_20_7_6H_21_7"] <- date_parse("2021-07-21")
  allcase00$bc_date_report2[is.na(allcase00$bc_date_report2) & !is.na(allcase00$bc_date_report) & allcase00$bc_date_report == "CA BỔ SUNG" &
                              !is.na(allcase00$sheet_name) & allcase00$sheet_name == "77_18H_1908_18H_2008"] <- date_parse("2021-08-20")
  allcase00$bc_date_report2[is.na(allcase00$bc_date_report2) & !is.na(allcase00$bc_date_report) & allcase00$bc_date_report == "BỔ SUNG NGÀY" &
                              !is.na(allcase00$sheet_name) & allcase00$sheet_name == "77_18H_1908_18H_2008"] <- date_parse("2021-08-20")
  allcase00$bc_date_report2[is.na(allcase00$bc_date_report2) & !is.na(allcase00$bc_date_report) & allcase00$bc_date_report == "18:27 19/0/2021" &
                              !is.na(allcase00$sheet_name) & allcase00$sheet_name == "77_18H_1908_18H_2008"] <- date_parse("2021-08-19")

  allcase0 <- allcase00 %>%
    mutate(congbo = ifelse(is.na(congbo), 0, congbo),
           name = ifelse(is.na(name), bc_name, name),
           yob = ifelse(is.na(yob), bc_yob, yob),
           sex = ifelse(is.na(sex), bc_sex, sex),
           tel = ifelse(is.na(tel2), bc_tel2, tel2),
           res_loc_final = ifelse(is.na(res_loc), bc_res_loc, res_loc),
           res_loc_com_final = ifelse(is.na(res_loc_com), bc_res_loc_com, res_loc_com),
           res_loc_dis_final = ifelse(is.na(res_loc_dis), as.character(bc_res_loc_dis2), as.character(res_loc_dis)),
           res_loc_pro_final = ifelse(!is.na(bc_res_loc_dis) & !bc_res_loc_dis %in% tphcm, bc_res_loc_dis, res_loc_pro),
           job_loc_final = ifelse(is.na(job_loc), bc_job_loc, job_loc),
           clin_symptoms_final = ifelse(is.na(clin_symptoms),
                                        ifelse(is.na(bc_clin_symptoms), NA,
                                               ifelse(bc_clin_symptoms %in% ktc, "KHONG",
                                                      ifelse(bc_clin_symptoms %in% c("KHÔNG RÕ"), "KHONG RO", "CO"))),
                                        ifelse(clin_symptoms %in% c("KHÔNG RÕ"), "KHONG RO", clin_symptoms)),
           clin_prog_final = ifelse(is.na(clin_prog), bc_clin_symptoms,
                                    ifelse(is.na(bc_clin_symptoms), clin_prog, paste(clin_prog, bc_clin_symptoms, sep = ","))),
           date_clin_onset_final = date_parse(ifelse(is.na(clin_date_onset2) & is.na(bc_clin_date_onset2), NA,
                                                     ifelse(is.na(clin_date_onset2), as.character(bc_clin_date_onset2), as.character(clin_date_onset2)))),
           date_first_pos_final = date_parse(ifelse(is.na(first_date_pos) & is.na(bc_first_date_pos), NA,
                                                    ifelse(is.na(first_date_pos), as.character(bc_first_date_pos), as.character(first_date_pos)))),
           date_report_final = date_parse(ifelse(is.na(date_report2) & is.na(bc_date_report2), NA,
                                                 ifelse(is.na(date_report2), as.character(bc_date_report2), as.character(date_report2)))),
           date_first_final = date_parse(ifelse(is.na(first_date) & is.na(bc_first_date), NA,
                                                ifelse(is.na(first_date), as.character(bc_first_date), as.character(first_date)))),
           date_first_all_final = date_parse(ifelse(is.na(first_date_all) & is.na(bc_first_date_all), NA,
                                                    ifelse(is.na(first_date_all), as.character(bc_first_date_all), as.character(first_date_all)))),
           date_qrt = date_parse(ifelse(is.na(qrt_date2), as.character(bc_qrt_date2), as.character(qrt_date2))),
           congdong = factor(ifelse((!is.na(bc_nhapcanh) & bc_nhapcanh == 1), "NHẬP CẢNH",
                                    ifelse(!is.na(congdong), as.character(congdong),
                                           ifelse(!is.na(bc_res_loc_dis) & !bc_res_loc_dis %in% tphcm, "NGOÀI TPHCM", "CỘNG ĐỒNG TẠI TPHCM"))),
                             levels = c("NHẬP CẢNH", "NGOÀI TPHCM", "CỘNG ĐỒNG TẠI TPHCM"))
    ) %>%
    arrange(date_report_final)

  check_dup <- iden_dup(data = allcase0, type = "allcase")
  ## reduce check_dup
  check_dup$pair <- check_dup$pair[check_dup$pair$id1 %in% which(allcase0$flow == "current") &
                                     check_dup$pair$id2 %in% which(allcase0$flow == "new"),]
  check_dup$duplication <- check_dup$pair$id2

  ## merge info of duplication
  allcase1 <- link_merge2(data = allcase0, check_dup = check_dup)
  allcase <- allcase1[-check_dup$duplication,]

  output <- list(data = allcase,
                 file = files)

  #source(file.path("R", "correct_allcase.R"), local = TRUE)

  ## save
  saveRDS(output, file = file.path(outdir, paste0("final_linelist_", timestamp, ".rds")))
}

mydaily2 <- function(datetime_update = Sys.time(), step = c(1:6),
                     case_url, bctruc_url,
                     outdir_daily, outdir_final) {
  timestamp <- gsub(pattern = "-|:| |+", replacement = "", x = datetime_update)

  if (1 %in% step) {
    cat("Step 1: start at", as.character(Sys.time()), "\n")
    ## 1. download bctruc data
    get_case_bctruc_data(case_url = case_url,
                         bctruc_url = bctruc_url,
                         download = "bctruc",
                         outdir = outdir_daily,
                         timestamp = timestamp)
    cat("Step 1: finish at", as.character(Sys.time()), "\n")
  }

  if (2 %in% step) {
    cat("Step 2: start at", as.character(Sys.time()), "\n")
    ## 2. split bctruc data into one file per sheet & save into database folder
    bctruc_split(input = file.path(outdir_daily, "bctruc", "combine", paste0("HCDCdata_BCTRUC_", timestamp, ".rds")),
                 outdir = file.path(outdir_daily, "bctruc", "separated"))
    remove_dupfile(path = file.path(outdir_daily, "bctruc", "separated"))
    cat("Step 2: finish at", as.character(Sys.time()), "\n")
  }

  if (3 %in% step) {
    cat("Step 3: start at", as.character(Sys.time()), "\n")
    ## 3. import & process bctruc data (select most updated files)
    bctruc_new(path = file.path(outdir_daily, "bctruc", "separated"),
               timestamp = timestamp)
    cat("Step 3: finish at", as.character(Sys.time()), "\n")
  }

  if (4 %in% step) {
    cat("Step 4: start at", as.character(Sys.time()), "\n")
    bctruc_file <- readRDS(file = file.path(outdir_daily, "bctruc", "new", paste0("bctruc_", timestamp, ".rds")))
    bctruc_process2(bctruc_file = bctruc_file,
                    outdir = file.path(outdir_daily, "bctruc", "cleaned2"),
                    timestamp = timestamp)
    cat("Step 4: finish at", as.character(Sys.time()), "\n")
  }

  ## 5. link linelist A & bctruc data to final data --> linelist B
  if (5 %in% step) {
    cat("Step 5: start at", as.character(Sys.time()), "\n")
    link_current_bctruc(bctruc = readRDS(file.path(outdir_daily, "bctruc", "cleaned2", paste0("bctruc_cleaned_", timestamp, ".rds"))),
                        outdir = outdir_final,
                        timestamp = timestamp)
    cat("Step 5: finish at", as.character(Sys.time()), "\n")
  }

  ## simple report
  tmp <- readRDS(file.path(outdir_final, paste0("final_linelist_", timestamp, ".rds")))
  bctruc_file <- readRDS(file = file.path(outdir_daily, "bctruc", "new", paste0("bctruc_", timestamp, ".rds")))
  dat <- tmp$data
  cat("\n Tóm tắt \n")
  cat("Tổng số ca: ", nrow(dat), "\n")
  cat("Số không có date_report_final: ", sum(is.na(dat$date_report_final)), "\n")
  tmp0 <- bctruc_file$data[[length(bctruc_file$data)]]; tmp0 <- filter(tmp0, !is.na(tmp0[, 3]))
  tmp1 <- bctruc_file$data[[length(bctruc_file$data) - 1]]; tmp1 <- filter(tmp1, !is.na(tmp1[, 3]))
  cat("Số được báo cáo trong 2 ca trực mới nhất:", "\n")
  cat(bctruc_file$file[[length(bctruc_file$data)]], ":", nrow(tmp0), "\n")
  cat(bctruc_file$file[[length(bctruc_file$data) - 1]], ":", nrow(tmp1), "\n")
  cat("Bảng 1: \n")
  print(table(dat$date_report_final))
  cat("\n Bảng 2: \n")
  with(subset(dat, date_report_final >= (date(datetime_update) - 1)), print(table(date_report_final, sheet_name, useNA = "ifany")))
  cat("\n Bảng 3: \n")
  id0 <- sort(sapply(unique(dat$sheet_name), function(x) as.numeric(strsplit(x, split = "_")[[1]][1])))
  with(subset(dat, sheet_name == last(names(id0))), table(date_report_final, sheet_name, useNA = "ifany"))
}


get_xn_file_latest <- function(path = file.path("data", "HCDC", "database", "testing", "tatca"), type = c("tong", "chitiet")) {
  if (length(type) == 2) {stop("Please select type of lab data")}
  require(dplyr)
  tmp_files <- list.files(path = path, pattern = paste0(type, "_"), full.names = TRUE)
  tmp_files2 <- basename(tmp_files)
  return(data.frame(
    file_name = tmp_files,
    real_name = sapply(tmp_files2, function(x) {
      return(paste(strsplit(gsub(pattern = ".xlsx", replacement = "", x = x), split = "_")[[1]][1:2], collapse = "_"))
    }),
    mtime = sapply(tmp_files, function(x) file.info(x)$mtime)
  ) %>%
    group_by(real_name) %>%
    arrange(desc(mtime)) %>%
    slice(1) %>%
    ungroup())
}

get_ma <- function(data, datevar = "date_report_final", t = 7, type = c("mean", "sum")) {
  require(data.table)
  require(tidyverse)
  if (length(type) > 1) type <- "mean"

  tmp0 <- as.data.frame(data)
  tmp <- tmp0 %>%
    group_by(across({{ datevar }})) %>%
    summarise(nob = n(),
              .groups = "drop")
  names(tmp) <- c("date", "nob")
  tmp0 <- data.frame(date = seq(from = min(tmp$date), to = max(tmp$date), by = 1))

  output <- merge(tmp, tmp0, by = c("date"), all.y = TRUE) %>%
    mutate(nob = ifelse(is.na(nob), 0, nob))

  if (type == "mean") {
    output$ma <- frollmean(x = output$nob, n = t, align = "right")
  } else {
    output$ma <- frollsum(x = output$nob, n = t, align = "right")
  }

  return(output)
}
