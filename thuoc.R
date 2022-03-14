library(tidyverse)
library(lubridate)
library(readxl)
library(writexl)

data_path <- file.path(Lmisc::get.dropbox.folder(), "Workspace", "Database", "COVID19", "DOH", "nvduoc", "data", "20211225")

ttyt <- read.csv(file = file.path(data_path, "BÁO CÁO SỞ Y TẾ HÀNG NGÀY 03 TÚI THUỐC F0 - v2.0.csv"))
names(ttyt) <- c("datetime", "email", "date_report", "reporter", "phone", "place",
                 "A_TTYT_out_cum", "A_TTYT_rem_cum", "A_TTYT_out_inc", "A_TYT_out_cum", "A_TYT_rem_cum", "A_TYT_out_inc",
                 "B_TTYT_out_cum", "B_TTYT_rem_cum", "B_TTYT_out_inc", "B_TYT_out_cum", "B_TYT_rem_cum", "B_TYT_out_inc",
                 "C_TTYT_out_cum", "C_TTYT_rem_cum", "C_TTYT_out_inc", "C_TYT_out_cum", "C_TYT_rem_cum", "C_TYT_out_inc",
                 "C_get_cum", "C_get_inc",
                 "K_TTYT_out_cum", "K_TTYT_rem_cum", "K_TTYT_out_inc", "K_TYT_out_cum", "K_TYT_rem_cum", "K_TYT_out_inc")
key_ttyt <- c("Quận 1", "Quận 3", "Quận 4", "Quận 5", "Quận 6", "Quận 7", "Quận 8", "Quận 10", "Quận 11", "Quận 12",
              "Quận Bình Tân", "Huyện Bình Chánh", "Quận Bình Thạnh", "Huyện Nhà Bè", "Huyện Hóc Môn", "Huyện Cần Giờ",
              "Huyện Củ Chi", "Quận Tân Bình", "Quận Tân Phú", "Quận Phú Nhuận", "Quận Gò Vấp", "Thành Phố Thủ Đức")

ttyt2 <- ttyt %>%
  mutate(ngay = ymd(sapply(datetime, FUN = function(x){strsplit(x, split = " ")[[1]][1]})),
         ngaygio = ymd_hms(sapply(datetime, FUN = function(x){paste(strsplit(x, split = " ")[[1]][1:2], collapse = " ")})),
         pm = sapply(datetime, FUN = function(x){strsplit(x, split = " ")[[1]][3]}),
         ngaygio = ymd_hms(ifelse(pm == "PM", as.character(ngaygio + hours(12)), as.character(ngaygio))),
         place = factor(ifelse(place %in% c("Tân Phú", "Quận Tân Phú"), "Quận Tân Phú",
                        ifelse(place %in% c("Hóc Môn", "Huyện Hóc Môn"), "Huyện Hóc Môn",
                               ifelse(place %in% c("Củ Chi", "Huyện Củ Chi"), "Huyện Củ Chi",
                                      ifelse(place %in% c("Nhà Bè", "Huyện Nhà Bè"), "Huyện Nhà Bè",
                                             ifelse(place %in% c("Cần Giờ", "Huyện Cần Giờ"), "Huyện Cần Giờ",
                                                    ifelse(place %in% c("Bình Thạnh", "Quận Bình Thạnh"), "Quận Bình Thạnh",
                                                           ifelse(place %in% c("Phú Nhuận", "Quận Phú Nhuận"), "Quận Phú Nhuận",
                                                                  ifelse(place %in% c("Gò Vấp", "Quận Gò Vấp"), "Quận Gò Vấp",
                                                                         ifelse(place %in% c("Thủ Đức", "Thành Phố Thủ Đức"), "Thành Phố Thủ Đức",
                                                                                ifelse(place %in% c("Bình Tân", "Quận Bình Tân"), "Quận Bình Tân",
                                                                                       ifelse(place %in% c("Tân Bình", "Quận Tân Bình"), "Quận Tân Bình",
                                                                                              ifelse(place %in% c("Bình Chánh", "Huyện Bình Chánh"), "Huyện Bình Chánh", place)))))))))))),
                        levels = key_ttyt)) %>%
  group_by(place, ngay) %>%
  arrange(desc(ngaygio)) %>%
  slice(1) %>%
  ungroup()
ttyt_bc <- merge(
  data.frame(place = factor(key_ttyt, levels = key_ttyt)),
  ttyt2 %>%
  select(ngay, ngaygio, date_report, email, place,
         A_TTYT_out_cum, A_TTYT_rem_cum, A_TYT_out_inc, A_TYT_out_cum,
         B_TTYT_out_cum, B_TTYT_rem_cum, B_TYT_out_inc, B_TYT_out_cum,
         C_TTYT_out_cum, C_TTYT_rem_cum, C_TYT_out_inc, C_TYT_out_cum,
         K_TTYT_out_cum, K_TTYT_rem_cum, K_TYT_out_inc, K_TYT_out_cum) %>%
  filter(ngay == ymd("2021-12-25")),
  by = "place", all.x = TRUE)
write_xlsx(x = ttyt_bc, path = file.path(data_path, "..", "tmp", "ttyt_bc.xlsx"))

## BVDC
bvdc <- read.csv(file = file.path(data_path, "BÁO CÁO SỞ Y TẾ HÀNG NGÀY 03 TÚI THUỐC F0 - BV Da chien.csv"))
names(bvdc) <- c("datetime", "email", "date_report", "reporter", "phone", "place",
                 "C_out_cum", "C_out_inc", "C_rem_cum", "C_get_cum", "C_get_inc")
key_bvdc <- c("BV Dã chiến thu dung ĐT COVID-19 số 1",
              "BV Dã chiến thu dung ĐT COVID-19 số 2",
              "BV Dã chiến thu dung ĐT COVID-19 số 3",
              "BV Dã chiến thu dung ĐT COVID-19 số 4",
              "BV Dã chiến thu dung ĐT COVID-19 số 5",
              "BV Dã chiến thu dung ĐT COVID-19 số 6",
              "BV Dã chiến thu dung ĐT COVID-19 số 7",
              "BV Dã chiến thu dung ĐT COVID-19 số 8",
              "BV Dã chiến thu dung ĐT COVID-19 số 9",
              "BV Dã chiến thu dung ĐT COVID-19 số 10",
              "BV Dã chiến thu dung ĐT COVID-19 số 11",
              "BV Dã chiến thu dung ĐT COVID-19 số 12",
              "BV Dã chiến thu dung ĐT COVID-19 số 13",
              "Bệnh viện dã chiến điều trị COVID-19 Bình Thạnh số 1 (số 01 Chu Văn An)",
              "BV Dã chiến thu dung ĐT COVID-19 số 16",
              "BV Dã chiến số 5A/QK7 (QK 317)",
              "BV Dã chiến Củ Chi",
              "Bệnh viện Dã chiến thu dung điều trị COVID-19 Số 14",
              "Bệnh viện Dã chiến điều trị COVID - 19 Củ Chi số 01 (Liên minh công nông)",
              "Bệnh viện Dã chiến Điều trị Covid Thủ Đức số 1 (Chung cư Bình Minh)",
              "Bệnh viện Dã chiến điều trị COVID-19 đa tầng Tân Bình",
              "Bệnh viện Dã chiến điều trị Covid-19 Huyện Bình Chánh",
              "Bệnh viện Dã chiến Điều trị COVID-19 Phú Nhuận số 1 (05 Hoàng Minh Giám)",
              "Bệnh viện Dã chiến Điều trị COVID-19 Phú Nhuận số 2 (128 Nguyễn Trọng Tuyển)",
              "Bệnh viện Dã chiến Điều trị COVID-19 Quận 1",
              "Bệnh viện Dã chiến Điều trị Covid-19 Quận 7 số 1",
              "Bệnh viện Dã chiến Điều trị COVID-19 Quận 8",
              "Bệnh viện Dã chiến Điều trị Covid-19 Thủ Đức số 2 (KTX Cao đẳng công thương)",
              "Bệnh viện Dã chiến Điều trị Covid-19 Thủ Đức số 3 (Chung cư C8)",
              "Bệnh viện thu dung, điều trị COVID-19 Quận 11",
              "Bệnh viện dã chiến điều trị COVID-19 Quận 1 số 1",
              "Bệnh viện dã chiến 3 tầng số 16",
              "Bệnh viện dã chiến truyền nhiễm số 5C thuộc Cục hậu cần- Quân khu 7",
              "Bệnh viện thu dung điều trị COVID-19 Quận 5 (trực thuộc UBND Quận 5)")

bvdc2 <- bvdc %>%
  mutate(ngay = ymd(sapply(datetime, FUN = function(x){strsplit(x, split = " ")[[1]][1]})),
         ngaygio = ymd_hms(sapply(datetime, FUN = function(x){paste(strsplit(x, split = " ")[[1]][1:2], collapse = " ")})),
         pm = sapply(datetime, FUN = function(x){strsplit(x, split = " ")[[1]][3]}),
         ngaygio = ymd_hms(ifelse(pm == "PM", as.character(ngaygio + hours(12)), as.character(ngaygio))),
         place = factor(place, levels = key_bvdc)) %>%
  group_by(place, ngay) %>%
  arrange(desc(ngaygio)) %>%
  slice(1) %>%
  ungroup()
bvdc_bc <- merge(
  data.frame(place = factor(key_bvdc, levels = key_bvdc)),
  bvdc2 %>%
    select(ngay, ngaygio, email, place,
           C_out_inc, C_out_cum) %>%
    filter(ngay == ymd("2021-12-25")),
  by = "place", all.x = TRUE)
write_xlsx(x = bvdc_bc, path = file.path(data_path, "..", "tmp", "bvdc_bc.xlsx"))

## BVDT
bvdt <- read.csv(file = file.path(data_path, "Bệnh viện Khác - BÁO CÁO SỞ Y TẾ HÀNG NGÀY TÚI THUỐC C.csv"))
names(bvdt) <- c("datetime", "email", "date_report", "reporter", "phone", "place",
                 "C_out_cum", "C_out_inc", "C_rem_cum", "C_get_cum", "C_get_inc")
key_bvdt <- c("Bệnh viện 175", "Bệnh viện 30/4", "Bệnh viện An Bình", "Bệnh viện Bình Dân",
              "Bệnh viện Điều trị Covid 19 Sài Gòn", "Bệnh viện Điều trị COVID-19 Cần Giờ",
              "Bệnh viện ĐKKV Thủ Đức", "Bệnh viện FV", "Bệnh viện Gia An 115",
              "Bệnh viện Hùng Vương", "Bệnh viện Huyện Củ Chi", "Bệnh viện Lê Văn Thịnh",
              "Bệnh viện Mắt", "Bệnh viện Nguyễn Tri Phương", "Bệnh viện Nhân dân 115",
              "Bệnh viện Nhi Đồng Thành phố", "Bệnh viện quận 4", "Bệnh viện Quân dân y Miền Đông",
              "Bệnh viện Quận Gò Vấp", "Bệnh viện Quận Phú Nhuận", "Bệnh viện Quận Tân Phú",
              "Bệnh viện Tâm Thần", "Bệnh viện Tân Hưng", "Bệnh viện Tim Tâm Đức", "Bệnh viện Triều An",
              "Bệnh viện Trung ương Huế - Trung tâm Hồi sức tích cực COVID-19", "Bệnh viện Trưng Vương", "Bệnh viện Vạn Hạnh",
              "Bệnh viện Công an Thành phố Hồ Chí Minh", "Bệnh viện Thành phố Thủ Đức",
              "Viện Tim", "Bệnh viện Nhân dân Gia Định", "Bệnh viện Nguyễn Trãi",
              "Bệnh viện ĐKKV Hóc Môn", "Bệnh viện quận 8", "Bệnh viện Lê Văn Việt",
              "Bệnh viện Tai Mũi Họng", "Ban Bảo vệ sức khỏe TW",
              "Bệnh viện Đa khoa Hoàn Mỹ Sài Gòn", "Bệnh viện điều trị Covid-19 Hoàn Mỹ Thủ Đức",
              "Bệnh viên Đa khoa Quốc tế Nam Sài Gòn", "Bệnh viện Đa khoa Hồng Đức III",
              "Bệnh viện Quận 1", "Bệnh viện Quận 11", "Bệnh viện Quận 12",
              "Bệnh viện điều trị Covid -19 Bình Chánh", "Bệnh viện Quận Bình Thạnh",
              "Bệnh viện Nhân Ái", "Bệnh viện Nhà Bè", "Bệnh viện Đa khoa Tâm Trí Sài Gòn",
              "Bệnh viện PHCN & ĐTBNN", "Bệnh viện Nhi Đồng 1", "Bệnh viện Quân y 7A", "Bệnh viện Nhi Đồng 2")

bvdt2 <- bvdt %>%
  mutate(ngay = ymd(sapply(datetime, FUN = function(x){strsplit(x, split = " ")[[1]][1]})),
         ngaygio = ymd_hms(sapply(datetime, FUN = function(x){paste(strsplit(x, split = " ")[[1]][1:2], collapse = " ")})),
         pm = sapply(datetime, FUN = function(x){strsplit(x, split = " ")[[1]][3]}),
         ngaygio = ymd_hms(ifelse(pm == "PM", as.character(ngaygio + hours(12)), as.character(ngaygio))),
         place = factor(place, levels = key_bvdt)) %>%
  group_by(place, ngay) %>%
  arrange(desc(ngaygio)) %>%
  slice(1) %>%
  ungroup()
bvdt_bc <- merge(
  data.frame(place = factor(key_bvdt, levels = key_bvdt)),
  bvdt2 %>%
    select(ngay, ngaygio, email, place,
           C_out_inc, C_out_cum) %>%
    filter(ngay == ymd("2021-12-25")),
  by = "place", all.x = TRUE)
write_xlsx(x = bvdt_bc, path = file.path(data_path, "..", "tmp", "bvdt_bc.xlsx"))
