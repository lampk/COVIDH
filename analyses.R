library(tidyverse)
library(data.table)
library(readxl)
library(lubridate)
library(Lmisc)
library(janitor)

source(file.path("R", "functions.R"))

data_path <- file.path(Lmisc::get.dropbox.folder(), "Workspace", "Database", "COVID19", "HCDC")


# PCR ---------------------------------------------------------------------

## xn_chitiet
xn_chitiet_newest <- get_xn_file_latest(path = file.path(data_path, "testing"), type = c("chitiet"))
xnchitiet <- NULL

for (i in c(1:nrow(xn_chitiet_newest))) {
  cat(i, "/", nrow(xn_chitiet_newest), "\n")
  xni <- janitor::clean_names(data.table(read_excel(path = xn_chitiet_newest$file_name[i], skip = 0, col_types = "text")))
  xni$file <- xn_chitiet_newest$real_name[i]
  xnchitiet <- rbind(xnchitiet, xni, fill = TRUE)
}

maugop_duong <- c("MAU GOP DUONG TINH", "MAU GOP DUONG TINH (KHAN CAP)")
testnhanh_duong <- c("TEST NHANH DUONG TINH", "TEST NHANH DUONG TINH (KHAN CAP)")
tamsoat_csyt <- c("TAM SOAT KHI CO SO Y TE CO F0", "TAM SOAT KHI CO SO Y TE CO F0 (KHAN CAP)", "TAM SOAT PK/BV", "TAM SOAT TAI BENH VIEN", "TAM SOAT TAI BENH VIEN (KHAN CAP)")
tamsoat_khambenh <- c("TAM SOAT NGUOI DEN KHAM BENH, SU DUNG DICH VU Y TE (KHAN CAP)",
                      "TAM SOAT NGUOI DEN KHAM BENH, SU DUNG DICH VU Y TE",
                      "TAM SOAT NGUOI DEN KHAM BENH", "SANG LOC KHAM BENH",
                      "SANG LOC KHAM BENH (KHAN CAP)", "DOI TUONG CO TRIEU CHUNG TAI CO SO KHAM CHUA BENH")
tamsoat_nguyco_ratcao <- c("TAM SOAT NGUOI NGUY CO RAT CAO (#F1 GAN)", "TAM SOAT NGUOI NGUY CO RAT CAO (#F1 GAN) (KHAN CAP)")
tamsoat_nguyco_cao <- c("TAM SOAT NGUOI NGUY CO CAO (#F1 XA)", "TAM SOAT NGUOI NGUY CO CAO (#F1 XA) (KHAN CAP)")
tamsoat_nguyco_cao_bv <- c("DOI TUONG NGUY CO CAO TAI CO SO KHAM CHUA BENH" )
tamsoat_nguyco_cao_congdong <- c("DOI TUONG NGUY CO CAO TRONG CONG DONG")
tamsoat_nguyco <- c("TAM SOAT NGUOI CO NGUY CO (#F2)", "TAM SOAT NGUOI CO NGUY CO (#F2, F KHAC)", "TAM SOAT NGUOI CO NGUY CO (#F2, F KHAC) (THUONG QUY)", "TAM SOAT F KHAC")
tamsoat_vhh <- c("TAM SOAT CHUM CA VIEM HO HAP", "TAM SOAT CHUM CA VIEM HO HAP (KHAN CAP)")
dieutra_dich <- c("DIEU TRA DICH", "DOI TUONG DIEU TRA DICH", "DIEU TRA DICH (KHAN CAP)")
kiemdich <- c("KIEM DICH", "DOI TUONG KIEM DICH")
kiemdich_trongnuoc <- c("KIEM DICH NGUOI VE TU VUNG DICH TRONG NUOC", "KIEM DICH NGUOI VE TU VUNG DICH TRONG NUOC (THUONG QUY)")
kiemdich_ngoainuoc <- c("KIEM DICH NGUOI VE TU NUOC NGOAI", "KIEM DICH NGUOI VE TU NUOC NGOAI (THUONG QUY)")
giamsat_congdong <- c("GIAM SAT CONG DONG (THUONG QUY)", "GIAM SAT CONG DONG")
giamsat_bv <- c("GIAM SAT THUONG QUY TAI BV", "GIAM SAT THUONG QUY", "GIAM SAT THUONG QUY (THUONG QUY)")
theo_yeucau <- c("THEO YEU CAU")
xetnghiemlai <- c("XET NGHIEM LAI")
sau_cltt <- c("SAU CACH LY TAP TRUNG")
theodoi_f0_csyt <- c("XET NGHIEM THEO DOI F0 TAI CO SO Y TE", "XET NGHIEM THEO DOI F0 TAI CO SO Y TE (THUONG QUY)")
theodoi_f0_nha <- c("XET NGHIEM THEO DOI F0 TAI NHA", "XET NGHIEM THEO DOI F0 TAI NHA (THUONG QUY)")
theodoi_f0_saudieutri <- c("XET NGHIEM THEO DOI F0 SAU DIEU TRI (THUONG QUY)", "SAU DIEU TRI", "XET NGHIEM THEO DOI F0 SAU DIEU TRI")
theodoi_khac <- c("XET NGHIEM THEO DOI TRUONG HOP KHAC", "XET NGHIEM THEO DOI TRUONG HOP KHAC (THUONG QUY)")
khac <- c("KHAC")

xn_chitiet <- data.table(xnchitiet %>%
                           dplyr::select(-thoi_gian_co_ket_qua_theo_he_thong, -orf1ab) %>%
                           mutate_all(.funs = toupper) %>%
                           rowwise() %>%
                           mutate(so_nguoi = as.numeric(ifelse(loai_mau_xn == "MẪU ĐƠN", 1, length(strsplit(ho_va_ten, split = ",")[[1]])))) %>%
                           ungroup() %>%
                           mutate(hcm = ifelse(is.na(tinh_thanh), 2,
                                               ifelse(tinh_thanh == "THÀNH PHỐ HỒ CHÍ MINH", 1, 0)),
                                  quanhuyen = ifelse(hcm == 2, "0. KHONG BIET",
                                                     ifelse(hcm == 0, "0. NGOAI TPHCM",
                                                            ifelse(is.na(quan_huyen), "0. KHONG BIET",
                                                                   ifelse(quan_huyen %in% c("QUẬN 01", "QUẬN 1"), "QUẬN 01",
                                                                          ifelse(quan_huyen %in% c("QUẬN 03", "QUẬN 3"), "QUẬN 03",
                                                                                 ifelse(quan_huyen %in% c("QUẬN 04", "QUẬN 4"), "QUẬN 04",
                                                                                        ifelse(quan_huyen %in% c("QUẬN 05", "QUẬN 5"), "QUẬN 05",
                                                                                               ifelse(quan_huyen %in% c("QUẬN 06", "QUẬN 6"), "QUẬN 06",
                                                                                                      ifelse(quan_huyen %in% c("QUẬN 07", "QUẬN 7"), "QUẬN 07",
                                                                                                             ifelse(quan_huyen %in% c("QUẬN 08", "QUẬN 8"), "QUẬN 08",
                                                                                                                    ifelse(quan_huyen %in% c("QUẬN 02", "QUẬN 2", "QUẬN 09", "QUẬN 9", "QUẬN THỦ ĐỨC", "THÀNH PHỐ THỦ ĐỨC"), "THÀNH PHỐ THỦ ĐỨC",
                                                                                                                           ifelse(quan_huyen %in% c("HUYỆN BÌNH CHÁNH", "HUYỆN CẦN GIỜ", "HUYỆN CỦ CHI", "HUYỆN HÓC MÔN", "HUYỆN NHÀ BÈ", "QUẬN BÌNH TÂN", "QUẬN BÌNH THẠNH", "QUẬN GÒ VẤP", "QUẬN PHÚ NHUẬN", "QUẬN TÂN BÌNH", "QUẬN TÂN PHÚ"), quan_huyen, "0. KHONG BIET")))))))))))),
                                  namsinh = ifelse(is.na(nam_sinh), NA,
                                                   ifelse(as.numeric(nam_sinh) <= 1921 | as.numeric(nam_sinh) > 2021, NA, as.numeric(nam_sinh))),
                                  tuoi = 2021 - namsinh,
                                  nhomtuoi = cut(tuoi, breaks = c(seq(0, 85, 5), 150), right = FALSE,
                                                 labels = c("0-4", "5-9", "10-14", "15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49", "50-54", "55-59", "60-64", "65-69", "70-74", "75-79", "80-84", "85+")),
                                  nhomtuoi2 = ifelse(is.na(nhomtuoi), "MISSING", as.character(nhomtuoi)),
                                  gioi = gioi_tinh,
                                  tuoi_gioi = ifelse(is.na(gioi) | is.na(tuoi), NA,
                                                     ifelse(gioi == "NAM",
                                                            ifelse(tuoi <= 14, "NAM 0-14",
                                                                   ifelse(tuoi <= 24, "NAM 15-24",
                                                                          ifelse(tuoi <= 44, "NAM 25-44",
                                                                                 ifelse(tuoi <= 59, "NAM 45-59", "NAM 60+")))),
                                                            ifelse(tuoi <= 14, "NU 0-14",
                                                                   ifelse(tuoi <= 24, "NU 15-24",
                                                                          ifelse(tuoi <= 44, "NU 25-44",
                                                                                 ifelse(tuoi <= 59, "NU 45-59", "NU 60+")))))),
                                  test_datetime_taken = dmy_hm(thoi_gian_lay_mau, tz = "Asia/Ho_Chi_Minh"),
                                  test_date_taken = date(test_datetime_taken),
                                  test_datetime_result = dmy_hm(thoi_gian_tra_ket_qua, tz = "Asia/Ho_Chi_Minh"),
                                  test_date_result = date(test_datetime_result),
                                  test_result = ifelse(is.na(ket_qua), "CHUA XAC DINH",
                                                       ifelse(grepl(pattern = "DƯƠNG", x = ket_qua), "DUONG TINH",
                                                              ifelse(grepl(pattern = "ÂM", x = ket_qua), "AM TINH",
                                                                     ifelse(grepl(pattern = "XÁC ĐỊNH", x = ket_qua), "CHUA XAC DINH",
                                                                            ifelse(grepl(pattern = "LẤY MẪU LẠI|MẪU KHÔNG ĐẠT|NGHI NGỜ", x = ket_qua), "KHONG DAT", "KHAC"))))),
                                  test_reason = toupper(stringi::stri_trans_general(str = ly_do_xn,  id = "Latin-ASCII")),
                                  test_reason2 = ifelse(is.na(test_reason), "MISSING",
                                                        ifelse(test_reason %in% maugop_duong, "01. MAU GOP DUONG",
                                                               ifelse(test_reason %in% testnhanh_duong, "02. TEST NHANH DUONG",
                                                                      ifelse(test_reason %in% tamsoat_csyt, "03. TAM SOAT CSYT",
                                                                             ifelse(test_reason %in% tamsoat_khambenh, "04. TAM SOAT KHAM BENH",
                                                                                    ifelse(test_reason %in% tamsoat_nguyco_ratcao, "05. TAM SOAT NGUY CO RAT CAO",
                                                                                           ifelse(test_reason %in% c(tamsoat_nguyco_cao, tamsoat_nguyco_cao_bv, tamsoat_nguyco_cao_congdong), "06. TAM SOAT NGUY CO CAO",
                                                                                                  ifelse(test_reason %in% tamsoat_nguyco, "07. TAM SOAT NGUY CO",
                                                                                                         ifelse(test_reason %in% tamsoat_vhh, "08. TAM SOAT VHH",
                                                                                                                ifelse(test_reason %in% dieutra_dich, "09. DIEU TRA DICH",
                                                                                                                       ifelse(test_reason %in% c(kiemdich, kiemdich_trongnuoc, kiemdich_ngoainuoc), "10. KIEM DICH",
                                                                                                                              ifelse(test_reason %in% c(giamsat_congdong), "11. GIAM SAT CONG DONG",
                                                                                                                                     ifelse(test_reason %in% giamsat_bv, "12. GIAM SAT BV",
                                                                                                                                            ifelse(test_reason %in% theo_yeucau, "13. THEO YEU CAU",
                                                                                                                                                   ifelse(test_reason %in% xetnghiemlai, "14. XET NGHIEM LAI",
                                                                                                                                                          ifelse(test_reason %in% c(sau_cltt, theodoi_f0_csyt, theodoi_f0_nha, theodoi_f0_saudieutri, theodoi_khac), "15. THEO DOI KHAC",
                                                                                                                                                                 ifelse(test_reason %in% khac, "16. KHAC", test_reason))))))))))))))))),
                                  test_type = ifelse(is.na(ky_thuat_xn), "MISSING",
                                                     ifelse((ky_thuat_xn %in% c("11-08-2021", "14-08-2021")) | (grepl(pattern = "COUNTIF|PCR|U07T|VLOOKUP", x = ky_thuat_xn)), "PCR",
                                                            ifelse(grepl(pattern = "TEST NHANH", x = ky_thuat_xn), "TEST NHANH",
                                                                   ifelse(grepl(pattern = "TEST MD", x = ky_thuat_xn), "TEST MD",
                                                                          ifelse(grepl(pattern = "NUÔI CẤY TẾ BÀO|PHÂN LẬP", x = ky_thuat_xn), "PHAN LAP VIRUS", "KHONG RO"))))),
                                  test_place = ifelse(is.na(noi_lay_mau), NA,
                                                      ifelse(noi_lay_mau %in% c("PHIÊN XÉT NGHIỆM MẪU"), NA,
                                                             ifelse(noi_lay_mau %in% c("1. NƠI ĐIỀU TRỊ NGƯỜI BỆNH COVID"), "01. NƠI ĐIỀU TRỊ NGƯỜI BỆNH COVID",
                                                                    ifelse(noi_lay_mau %in% c("2. CƠ SỞ Y TẾ KHÔNG ĐIỀU TRỊ COVID"), "02. CƠ SỞ Y TẾ KHÔNG ĐIỀU TRỊ COVID",
                                                                           ifelse(noi_lay_mau %in% c("3. KHU CÁCH LY"), "03. KHU CÁCH LY",
                                                                                  ifelse(noi_lay_mau %in% c("4. KHU PHONG TỎA"), "04. KHU PHONG TỎA",
                                                                                         ifelse(noi_lay_mau %in% c("5. CƠ SỞ SẢN XUẤT"), "05. CƠ SỞ SẢN XUẤT",
                                                                                                ifelse(noi_lay_mau %in% c("6. TRỤ SỞ LÀM VIỆC"), "06. TRỤ SỞ LÀM VIỆC",
                                                                                                       ifelse(noi_lay_mau %in% c("7. CỘNG ĐỒNG"), "07. CỘNG ĐỒNG",
                                                                                                              ifelse(noi_lay_mau %in% c("8. TẠI NHÀ"), "08. TẠI NHÀ",
                                                                                                                     ifelse(noi_lay_mau %in% c("9. VÙNG XANH"), "09. VÙNG XANH",
                                                                                                                            ifelse(noi_lay_mau %in% c("10. VÙNG CẬN XANH"), "10. VÙNG CẬN XANH",
                                                                                                                                   ifelse(noi_lay_mau %in% c("11. VÙNG VÀNG"), "11. VÙNG VÀNG",
                                                                                                                                          ifelse(noi_lay_mau %in% c("12. VÙNG CAM"), "12. VÙNG CAM",
                                                                                                                                                 ifelse(noi_lay_mau %in% c("13. VÙNG ĐỎ"), "13. VÙNG ĐỎ", "14. KHAC")))))))))))))))

                           ))


## xn_tong
xn_tong_newest <- get_xn_file_latest(path = file.path(data_path, "testing"), type = c("tong")) %>%
  filter(grepl(pattern = paste("202107", c("09", "10", "11", "12", "13", "14", "15"), sep = "", collapse = "|"), x = real_name))
xntong <- NULL

for (i in c(1:nrow(xn_tong_newest))) {
  cat(i, "/", nrow(xn_tong_newest), "\n")
  xni <- janitor::clean_names(data.table(read_excel(path = xn_tong_newest$file_name[i], skip = 5, col_types = "text")))
  xni$file <- xn_tong_newest$real_name[i]
  xntong <- rbind(xntong, xni, fill = TRUE)
}

xn_tong <- data.table(xntong %>%
                        mutate_all(.funs = toupper) %>%
                        rowwise() %>%
                        mutate(so_nguoi = as.numeric(ifelse(loai_mau == "MẪU ĐƠN", 1, length(strsplit(ho_va_ten, split = ",")[[1]])))) %>%
                        ungroup() %>%
                        mutate(hcm = ifelse(is.na(tinh_thanh), 2,
                                            ifelse(tinh_thanh == "THÀNH PHỐ HỒ CHÍ MINH", 1, 0)),
                               quanhuyen = ifelse(hcm == 2, "0. KHONG BIET",
                                                  ifelse(hcm == 0, "0. NGOAI TPHCM",
                                                         ifelse(is.na(quan_huyen), "0. KHONG BIET",
                                                                ifelse(quan_huyen %in% c("QUẬN 01", "QUẬN 1"), "QUẬN 01",
                                                                       ifelse(quan_huyen %in% c("QUẬN 03", "QUẬN 3"), "QUẬN 03",
                                                                              ifelse(quan_huyen %in% c("QUẬN 04", "QUẬN 4"), "QUẬN 04",
                                                                                     ifelse(quan_huyen %in% c("QUẬN 05", "QUẬN 5"), "QUẬN 05",
                                                                                            ifelse(quan_huyen %in% c("QUẬN 06", "QUẬN 6"), "QUẬN 06",
                                                                                                   ifelse(quan_huyen %in% c("QUẬN 07", "QUẬN 7"), "QUẬN 07",
                                                                                                          ifelse(quan_huyen %in% c("QUẬN 08", "QUẬN 8"), "QUẬN 08",
                                                                                                                 ifelse(quan_huyen %in% c("QUẬN 02", "QUẬN 2", "QUẬN 09", "QUẬN 9", "QUẬN THỦ ĐỨC", "THÀNH PHỐ THỦ ĐỨC"), "THÀNH PHỐ THỦ ĐỨC",
                                                                                                                        ifelse(quan_huyen %in% c("HUYỆN BÌNH CHÁNH", "HUYỆN CẦN GIỜ", "HUYỆN CỦ CHI", "HUYỆN HÓC MÔN", "HUYỆN NHÀ BÈ", "QUẬN BÌNH TÂN", "QUẬN BÌNH THẠNH", "QUẬN GÒ VẤP", "QUẬN PHÚ NHUẬN", "QUẬN TÂN BÌNH", "QUẬN TÂN PHÚ"), quan_huyen, "0. KHONG BIET")))))))))))),
                               namsinh = ifelse(is.na(nam_sinh), NA,
                                                ifelse(as.numeric(nam_sinh) <= 1921 | as.numeric(nam_sinh) > 2021, NA, as.numeric(nam_sinh))),
                               tuoi = 2021 - namsinh,
                               nhomtuoi = cut(tuoi, breaks = c(seq(0, 85, 5), 150), right = FALSE,
                                              labels = c("0-4", "5-9", "10-14", "15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49", "50-54", "55-59", "60-64", "65-69", "70-74", "75-79", "80-84", "85+")),
                               nhomtuoi2 = ifelse(is.na(nhomtuoi), "MISSING", as.character(nhomtuoi)),
                               gioi = gioi_tinh,
                               tuoi_gioi = ifelse(is.na(gioi) | is.na(tuoi), NA,
                                                  ifelse(gioi == "NAM",
                                                         ifelse(tuoi <= 14, "NAM 0-14",
                                                                ifelse(tuoi <= 24, "NAM 15-24",
                                                                       ifelse(tuoi <= 44, "NAM 25-44",
                                                                              ifelse(tuoi <= 59, "NAM 45-59", "NAM 60+")))),
                                                         ifelse(tuoi <= 14, "NU 0-14",
                                                                ifelse(tuoi <= 24, "NU 15-24",
                                                                       ifelse(tuoi <= 44, "NU 25-44",
                                                                              ifelse(tuoi <= 59, "NU 45-59", "NU 60+")))))),
                               test_datetime_taken = dmy_hm(thoi_gian_lay_mau, tz = "Asia/Ho_Chi_Minh"),
                               test_date_taken = date(test_datetime_taken),
                               test_datetime_result = dmy_hm(thoi_gian_ket_qua, tz = "Asia/Ho_Chi_Minh"),
                               test_date_result = date(test_datetime_result),
                               test_result = ifelse(is.na(ket_qua), "CHUA XAC DINH",
                                                    ifelse(grepl(pattern = "DƯƠNG", x = ket_qua), "DUONG TINH",
                                                           ifelse(grepl(pattern = "ÂM", x = ket_qua), "AM TINH",
                                                                  ifelse(grepl(pattern = "XÁC ĐỊNH", x = ket_qua), "CHUA XAC DINH",
                                                                         ifelse(grepl(pattern = "LẤY MẪU LẠI|MẪU KHÔNG ĐẠT|NGHI NGỜ", x = ket_qua), "KHONG DAT", "KHAC"))))),
                               test_reason = toupper(stringi::stri_trans_general(str = ly_do_xn,  id = "Latin-ASCII")),
                               test_reason2 = ifelse(is.na(test_reason), "MISSING",
                                                     ifelse(test_reason %in% maugop_duong, "01. MAU GOP DUONG",
                                                            ifelse(test_reason %in% testnhanh_duong, "02. TEST NHANH DUONG",
                                                                   ifelse(test_reason %in% tamsoat_csyt, "03. TAM SOAT CSYT",
                                                                          ifelse(test_reason %in% tamsoat_khambenh, "04. TAM SOAT KHAM BENH",
                                                                                 ifelse(test_reason %in% tamsoat_nguyco_ratcao, "05. TAM SOAT NGUY CO RAT CAO",
                                                                                        ifelse(test_reason %in% c(tamsoat_nguyco_cao, tamsoat_nguyco_cao_bv, tamsoat_nguyco_cao_congdong), "06. TAM SOAT NGUY CO CAO",
                                                                                               ifelse(test_reason %in% tamsoat_nguyco, "07. TAM SOAT NGUY CO",
                                                                                                      ifelse(test_reason %in% tamsoat_vhh, "08. TAM SOAT VHH",
                                                                                                             ifelse(test_reason %in% dieutra_dich, "09. DIEU TRA DICH",
                                                                                                                    ifelse(test_reason %in% c(kiemdich, kiemdich_trongnuoc, kiemdich_ngoainuoc), "10. KIEM DICH",
                                                                                                                           ifelse(test_reason %in% c(giamsat_congdong), "11. GIAM SAT CONG DONG",
                                                                                                                                  ifelse(test_reason %in% giamsat_bv, "12. GIAM SAT BV",
                                                                                                                                         ifelse(test_reason %in% theo_yeucau, "13. THEO YEU CAU",
                                                                                                                                                ifelse(test_reason %in% xetnghiemlai, "14. XET NGHIEM LAI",
                                                                                                                                                       ifelse(test_reason %in% c(sau_cltt, theodoi_f0_csyt, theodoi_f0_nha, theodoi_f0_saudieutri, theodoi_khac), "15. THEO DOI KHAC",
                                                                                                                                                              ifelse(test_reason %in% khac, "16. KHAC", test_reason))))))))))))))))),
                               test_type = ifelse(is.na(ky_thuat_xn), "MISSING",
                                                  ifelse((ky_thuat_xn %in% c("11-08-2021", "14-08-2021")) | (grepl(pattern = "COUNTIF|PCR|U07T|VLOOKUP", x = ky_thuat_xn)), "PCR",
                                                         ifelse(grepl(pattern = "TEST NHANH", x = ky_thuat_xn), "TEST NHANH",
                                                                ifelse(grepl(pattern = "TEST MD", x = ky_thuat_xn), "TEST MD",
                                                                       ifelse(grepl(pattern = "NUÔI CẤY TẾ BÀO|PHÂN LẬP", x = ky_thuat_xn), "PHAN LAP VIRUS", "KHONG RO")))))

                        )) %>%
  rename(loai_mau_xn = loai_mau,
         co_so_xet_nghiem = don_vi_xet_nghiem,
         thoi_gian_tra_ket_qua = thoi_gian_ket_qua)

xn <- rbindlist(list(xn_tong[loai_mau_xn == "MẪU ĐƠN"], xn_chitiet), fill = TRUE)

xn_moi <- xn[!test_reason %in% c("XET NGHIEM THEO DOI F0 TAI CO SO Y TE", "XET NGHIEM THEO DOI TRUONG HOP KHAC", "XET NGHIEM THEO DOI F0 SAU DIEU TRI", "XET NGHIEM THEO DOI F0 TAI NHA", "SAU DIEU TRI", "XET NGHIEM THEO DOI F0")]

pcr <- xn_moi[test_type == "PCR"]

pcr_sum1 <- pcr[(loai_mau_xn == "MẪU ĐƠN" | (loai_mau_xn == "MẪU GỘP" & test_result == "AM TINH")),
           .(hcdc_n_pcr_nguoi = sum(so_nguoi)),
           by = c("test_date_taken", "test_reason2", "test_place")
]

pcr_sum2 <- pcr[,
               .(hcdc_n_pcr_test = length(unique(ma_xn)),
                 hcdc_n_pcr_test_don = length(unique(ma_xn[loai_mau_xn == "MẪU ĐƠN"])),
                 hcdc_n_pcr_test_gop = length(unique(ma_xn[loai_mau_xn == "MẪU GỘP"])),
                 hcdc_n_pcr_test_don_duong = length(unique(ma_xn[loai_mau_xn == "MẪU ĐƠN" & test_result == "DUONG TINH"]))),
               by = c("test_date_taken", "test_reason2", "test_place")
]

pcr_sum <- merge(pcr_sum1, pcr_sum2, by = c("test_date_taken", "test_reason2", "test_place")) %>%
  rename(date = test_date_taken) %>%
  mutate(hcdc_pcr_pos_rate = hcdc_n_pcr_test_don_duong/hcdc_n_pcr_nguoi)

pcr_sum_date <- pcr_sum %>%
  group_by(date) %>%
  summarise(hcdc_n_pcr_nguoi = sum(hcdc_n_pcr_nguoi, na.rm = TRUE),
            hcdc_n_pcr_test = sum(hcdc_n_pcr_test, na.rm = TRUE),
            hcdc_n_pcr_test_don = sum(hcdc_n_pcr_test_don, na.rm = TRUE),
            hcdc_n_pcr_test_gop = sum(hcdc_n_pcr_test_gop, na.rm = TRUE),
            hcdc_n_pcr_test_don_duong = sum(hcdc_n_pcr_test_don_duong, na.rm = TRUE),
            hcdc_pcr_pos_rate = hcdc_n_pcr_test_don_duong/hcdc_n_pcr_nguoi,
            .groups = "drop")

pcr_sum_date_reason <- pcr_sum %>%
  group_by(date, test_reason2) %>%
  summarise(hcdc_n_pcr_nguoi = sum(hcdc_n_pcr_nguoi, na.rm = TRUE),
            hcdc_n_pcr_test = sum(hcdc_n_pcr_test, na.rm = TRUE),
            hcdc_n_pcr_test_don = sum(hcdc_n_pcr_test_don, na.rm = TRUE),
            hcdc_n_pcr_test_gop = sum(hcdc_n_pcr_test_gop, na.rm = TRUE),
            hcdc_n_pcr_test_don_duong = sum(hcdc_n_pcr_test_don_duong, na.rm = TRUE),
            hcdc_pcr_pos_rate = hcdc_n_pcr_test_don_duong/hcdc_n_pcr_nguoi,
            .groups = "drop")

pcr_sum_date_place <- pcr_sum %>%
  group_by(date, test_place) %>%
  summarise(hcdc_n_pcr_nguoi = sum(hcdc_n_pcr_nguoi, na.rm = TRUE),
            hcdc_n_pcr_test = sum(hcdc_n_pcr_test, na.rm = TRUE),
            hcdc_n_pcr_test_don = sum(hcdc_n_pcr_test_don, na.rm = TRUE),
            hcdc_n_pcr_test_gop = sum(hcdc_n_pcr_test_gop, na.rm = TRUE),
            hcdc_n_pcr_test_don_duong = sum(hcdc_n_pcr_test_don_duong, na.rm = TRUE),
            hcdc_pcr_pos_rate = hcdc_n_pcr_test_don_duong/hcdc_n_pcr_nguoi,
            .groups = "drop")

cases_pcr <- pcr[loai_mau_xn == "MẪU ĐƠN" & test_result == "DUONG TINH"]

saveRDS(xn, file = file.path(file.path(data_path, "save"), "xn_cds.rds"))
saveRDS(xn_moi, file = file.path(file.path(data_path, "save"), "xn_cds_moi.rds"))
saveRDS(pcr, file = file.path(file.path(data_path, "save"), "pcr.rds"))
saveRDS(cases_pcr, file = file.path(file.path(data_path, "save"), "cases_pcr.rds"))
save(pcr_sum, pcr_sum_date, pcr_sum_date_reason, pcr_sum_date_place, file = file.path(file.path(data_path, "save"), "pcr_sum.Rdata"))

library(ggplot2)
ggplot(data = pcr_sum_date, aes(x = date, y = hcdc_pcr_pos_rate)) +
  geom_line() +
  geom_point() +
  scale_x_date(breaks = "1 month") +
  theme_bw()

ggplot(data = pcr_sum_date_reason, aes(x = date, y = hcdc_pcr_pos_rate, group = test_reason2, color = test_reason2)) +
  geom_line() +
  geom_point() +
  scale_x_date(breaks = "1 month") +
  facet_wrap(~ test_reason2) +
  theme_bw()

ggplot(data = pcr_sum_date_place, aes(x = date, y = hcdc_pcr_pos_rate, group = test_place, color = test_place)) +
  geom_line() +
  geom_point() +
  scale_x_date(breaks = "1 month") +
  facet_wrap(~ test_place) +
  theme_bw()

# test nhanh --------------------------------------------------------------

xn_testnhanh_newest <- get_xn_file_latest(path = file.path(data_path, "testing"), type = c("testnhanh"))
xntestnhanh <- NULL

for (i in c(1:nrow(xn_testnhanh_newest))) {
  cat(i, "/", nrow(xn_testnhanh_newest), "\n")
  xni <- janitor::clean_names(data.table(read_excel(path = xn_testnhanh_newest$file_name[i], skip = 3, col_types = "text")))
  xni$file <- xn_testnhanh_newest$real_name[i]
  xntestnhanh <- rbind(xntestnhanh, xni, fill = TRUE)
}

xn_testnhanh <- data.table(xntestnhanh %>%
                           mutate_all(.funs = toupper) %>%
                             setNames(nm = c("stt", "ho_va_ten", "nam_sinh", "gioi_tinh", "cmnd_cccd_passport", "sdt", "tiem_vaccine", "ngay_tiem_gan_nhat",
                                             "so_nha", "tinh_thanh", "quan_huyen", "phuong_xa", "kp_ap", "to",
                                             "noi_lam_viec_di_hoc_o_nha", "dia_chi_lam_viec_o_nha", "tinh_thanh_lv", "quan_huyen_lv", "phuong_xa_lv",
                                             "thoi_gian_lay_mau", "ket_qua", "trieu_chung", "ly_do_xn", "noi_lay_mau", "ngay_he_thong_ghi_nhan", "co_so_xet_nghiem",
                                             "file")) %>%
                           mutate(hcm = ifelse(is.na(tinh_thanh), 2,
                                               ifelse(tinh_thanh == "THÀNH PHỐ HỒ CHÍ MINH", 1, 0)),
                                  quanhuyen = ifelse(hcm == 2, "0. KHONG BIET",
                                                     ifelse(hcm == 0, "0. NGOAI TPHCM",
                                                            ifelse(is.na(quan_huyen), "0. KHONG BIET",
                                                                   ifelse(quan_huyen %in% c("QUẬN 01", "QUẬN 1"), "QUẬN 01",
                                                                          ifelse(quan_huyen %in% c("QUẬN 03", "QUẬN 3"), "QUẬN 03",
                                                                                 ifelse(quan_huyen %in% c("QUẬN 04", "QUẬN 4"), "QUẬN 04",
                                                                                        ifelse(quan_huyen %in% c("QUẬN 05", "QUẬN 5"), "QUẬN 05",
                                                                                               ifelse(quan_huyen %in% c("QUẬN 06", "QUẬN 6"), "QUẬN 06",
                                                                                                      ifelse(quan_huyen %in% c("QUẬN 07", "QUẬN 7"), "QUẬN 07",
                                                                                                             ifelse(quan_huyen %in% c("QUẬN 08", "QUẬN 8"), "QUẬN 08",
                                                                                                                    ifelse(quan_huyen %in% c("QUẬN 02", "QUẬN 2", "QUẬN 09", "QUẬN 9", "QUẬN THỦ ĐỨC", "THÀNH PHỐ THỦ ĐỨC"), "THÀNH PHỐ THỦ ĐỨC",
                                                                                                                           ifelse(quan_huyen %in% c("HUYỆN BÌNH CHÁNH", "HUYỆN CẦN GIỜ", "HUYỆN CỦ CHI", "HUYỆN HÓC MÔN", "HUYỆN NHÀ BÈ", "QUẬN BÌNH TÂN", "QUẬN BÌNH THẠNH", "QUẬN GÒ VẤP", "QUẬN PHÚ NHUẬN", "QUẬN TÂN BÌNH", "QUẬN TÂN PHÚ"), quan_huyen, "0. KHONG BIET")))))))))))),
                                  namsinh = ifelse(is.na(nam_sinh), NA,
                                                   ifelse(as.numeric(nam_sinh) <= 1921 | as.numeric(nam_sinh) > 2021, NA, as.numeric(nam_sinh))),
                                  tuoi = 2021 - namsinh,
                                  nhomtuoi = cut(tuoi, breaks = c(seq(0, 85, 5), 150), right = FALSE,
                                                 labels = c("0-4", "5-9", "10-14", "15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49", "50-54", "55-59", "60-64", "65-69", "70-74", "75-79", "80-84", "85+")),
                                  nhomtuoi2 = ifelse(is.na(nhomtuoi), "MISSING", as.character(nhomtuoi)),
                                  gioi = gioi_tinh,
                                  tuoi_gioi = ifelse(is.na(gioi) | is.na(tuoi), NA,
                                                     ifelse(gioi == "NAM",
                                                            ifelse(tuoi <= 14, "NAM 0-14",
                                                                   ifelse(tuoi <= 24, "NAM 15-24",
                                                                          ifelse(tuoi <= 44, "NAM 25-44",
                                                                                 ifelse(tuoi <= 59, "NAM 45-59", "NAM 60+")))),
                                                            ifelse(tuoi <= 14, "NU 0-14",
                                                                   ifelse(tuoi <= 24, "NU 15-24",
                                                                          ifelse(tuoi <= 44, "NU 25-44",
                                                                                 ifelse(tuoi <= 59, "NU 45-59", "NU 60+")))))),
                                  test_datetime_taken = parse_date_time(x = thoi_gian_lay_mau, orders = "HM dmy", tz = "Asia/Ho_Chi_Minh"),
                                  test_date_taken = date(test_datetime_taken),
                                  test_datetime_doc = parse_date_time(x = ngay_he_thong_ghi_nhan, orders = "HM dmy", tz = "Asia/Ho_Chi_Minh"),
                                  test_date_doc = date(test_datetime_doc),
                                  test_result = ifelse(is.na(ket_qua), "CHUA XAC DINH",
                                                       ifelse(grepl(pattern = "DƯƠNG", x = ket_qua), "DUONG TINH",
                                                              ifelse(grepl(pattern = "ÂM", x = ket_qua), "AM TINH",
                                                                     ifelse(grepl(pattern = "XÁC ĐỊNH", x = ket_qua), "CHUA XAC DINH",
                                                                            ifelse(grepl(pattern = "LẤY MẪU LẠI|MẪU KHÔNG ĐẠT|NGHI NGỜ", x = ket_qua), "KHONG DAT", "KHAC"))))),
                                  test_reason = toupper(stringi::stri_trans_general(str = ly_do_xn,  id = "Latin-ASCII")),
                                  test_reason2 = ifelse(is.na(test_reason), "MISSING",
                                                        ifelse(test_reason %in% maugop_duong, "01. MAU GOP DUONG",
                                                               ifelse(test_reason %in% testnhanh_duong, "02. TEST NHANH DUONG",
                                                                      ifelse(test_reason %in% tamsoat_csyt, "03. TAM SOAT CSYT",
                                                                             ifelse(test_reason %in% tamsoat_khambenh, "04. TAM SOAT KHAM BENH",
                                                                                    ifelse(test_reason %in% tamsoat_nguyco_ratcao, "05. TAM SOAT NGUY CO RAT CAO",
                                                                                           ifelse(test_reason %in% c(tamsoat_nguyco_cao, tamsoat_nguyco_cao_bv, tamsoat_nguyco_cao_congdong), "06. TAM SOAT NGUY CO CAO",
                                                                                                  ifelse(test_reason %in% tamsoat_nguyco, "07. TAM SOAT NGUY CO",
                                                                                                         ifelse(test_reason %in% tamsoat_vhh, "08. TAM SOAT VHH",
                                                                                                                ifelse(test_reason %in% dieutra_dich, "09. DIEU TRA DICH",
                                                                                                                       ifelse(test_reason %in% c(kiemdich, kiemdich_trongnuoc, kiemdich_ngoainuoc), "10. KIEM DICH",
                                                                                                                              ifelse(test_reason %in% c(giamsat_congdong), "11. GIAM SAT CONG DONG",
                                                                                                                                     ifelse(test_reason %in% giamsat_bv, "12. GIAM SAT BV",
                                                                                                                                            ifelse(test_reason %in% theo_yeucau, "13. THEO YEU CAU",
                                                                                                                                                   ifelse(test_reason %in% xetnghiemlai, "14. XET NGHIEM LAI",
                                                                                                                                                          ifelse(test_reason %in% c(sau_cltt, theodoi_f0_csyt, theodoi_f0_nha, theodoi_f0_saudieutri, theodoi_khac), "15. THEO DOI KHAC",
                                                                                                                                                                 ifelse(test_reason %in% khac, "16. KHAC", test_reason))))))))))))))))),
                                  test_place = ifelse(is.na(noi_lay_mau), NA,
                                                      ifelse(noi_lay_mau %in% c("PHIÊN XÉT NGHIỆM MẪU"), NA,
                                                             ifelse(noi_lay_mau %in% c("1. NƠI ĐIỀU TRỊ NGƯỜI BỆNH COVID", "1. KHU CÁCH LY ĐIỀU TRỊ F0, BỆNH VIỆN ĐIỀU TRỊ COVID"), "01. NƠI ĐIỀU TRỊ NGƯỜI BỆNH COVID",
                                                                    ifelse(noi_lay_mau %in% c("2. CƠ SỞ Y TẾ KHÔNG ĐIỀU TRỊ COVID"), "02. CƠ SỞ Y TẾ KHÔNG ĐIỀU TRỊ COVID",
                                                                           ifelse(noi_lay_mau %in% c("3. KHU CÁCH LY"), "03. KHU CÁCH LY",
                                                                                  ifelse(noi_lay_mau %in% c("11. KHU PHONG TỎA"), "04. KHU PHONG TỎA",
                                                                                         ifelse(noi_lay_mau %in% c("4. KCN, KCX, KHU CÔNG NGHỆ CAO, CSSX/KD"), "05. CƠ SỞ SẢN XUẤT",
                                                                                                ifelse(noi_lay_mau %in% c("6. TRỤ SỞ LÀM VIỆC"), "06. TRỤ SỞ LÀM VIỆC",
                                                                                                       ifelse(noi_lay_mau %in% c("7. CỘNG ĐỒNG"), "07. CỘNG ĐỒNG",
                                                                                                              ifelse(noi_lay_mau %in% c("10. TẠI NHÀ"), "08. TẠI NHÀ",
                                                                                                                     ifelse(noi_lay_mau %in% c("9. VÙNG XANH"), "09. VÙNG XANH",
                                                                                                                            ifelse(noi_lay_mau %in% c("10. VÙNG CẬN XANH"), "10. VÙNG CẬN XANH",
                                                                                                                                   ifelse(noi_lay_mau %in% c("11. VÙNG VÀNG"), "11. VÙNG VÀNG",
                                                                                                                                          ifelse(noi_lay_mau %in% c("12. VÙNG CAM"), "12. VÙNG CAM",
                                                                                                                                                 ifelse(noi_lay_mau %in% c("13. VÙNG ĐỎ"), "13. VÙNG ĐỎ",
                                                                                                                                                        ifelse(noi_lay_mau %in% c("5. CƠ QUAN HÀNH CHÍNH, CÔNG SỞ"), "15. CƠ QUAN HÀNH CHÍNH, CÔNG SỞ",
                                                                                                                                                               ifelse(noi_lay_mau %in% c("6. CHỢ, SIÊU THỊ, CỬA HÀNG TIỆN LỢI"),"16. CHỢ, SIÊU THỊ, CỬA HÀNG TIỆN LỢI",
                                                                                                                                                                      ifelse(noi_lay_mau %in% c("7. SÂN BAY, BẾN XE, BẾN TÀU, GA ĐƯỜNG SẮT"),"17. SÂN BAY, BẾN XE, BẾN TÀU, GA ĐƯỜNG SẮT",
                                                                                                                                                                             ifelse(noi_lay_mau %in% c("8. ĐƠN VỊ THƯỜNG XUYÊN LƯU TRÚ ĐÔNG NGƯỜI"), "18. ĐƠN VỊ THƯỜNG XUYÊN LƯU TRÚ ĐÔNG NGƯỜI",
                                                                                                                                                                                    ifelse(noi_lay_mau %in% c("9. TRƯỜNG HỌC"), "19. TRƯỜNG HỌC", "14. KHAC"))))))))))))))))))))

                           ))


xn_testnhanh_moi <- xn_testnhanh[!test_reason2 %in% c("XET NGHIEM THEO DOI F0", "15. THEO DOI KHAC")]
cases_testnhanh <- xn_testnhanh_moi[test_result == "DUONG TINH"]

saveRDS(xn_testnhanh, file = file.path(file.path(data_path, "save"), "xn_testnhanh.rds"))
saveRDS(xn_testnhanh_moi, file = file.path(file.path(data_path, "save"), "xn_testnhanh_moi.rds"))
saveRDS(cases_testnhanh, file = file.path(file.path(data_path, "save"), "cases_testnhanh.rds"))

## tong so test nhanh
rtest_sum <- clean_names(data.table(read_excel(path = file.path(data_path, "additional", "test_nhanh", "summary.xlsx"), col_types = "text", sheet = "summary"))) %>%
  mutate(date = as.Date(as.numeric(ngay), origin = "1899-12-30", tz = Sys.timezone()))


rtest_sum_date <- rbind(
  rtest_sum %>%
    filter(date < "2021-09-21" & qh == "Tổng") %>%
    mutate(rtest_test = ifelse(is.na(tong), as.numeric(tong_2), as.numeric(tong)),
           rtest_nguoi = as.numeric(tong_2),
           rtest_duong = as.numeric(duong_tinh)) %>%
    group_by(date) %>%
    summarise(hcdc_n_rtest_test = sum(rtest_test),
              hcdc_n_rtest_nguoi = sum(rtest_nguoi),
              hcdc_n_rtest_duong = sum(rtest_duong),
              .groups = "drop"),
  rtest_sum %>%
    filter(date >= "2021-09-21" & vung_nguy_co == "Tổng") %>%
    mutate(rtest_test = as.numeric(so_test_da_su_dung),
           rtest_nguoi = as.numeric(tong_so_nguoi_trong_ho_gia_dinh_duoc_lay_theo_doi_xn),
           rtest_duong = as.numeric(so_ca_f0_moi)) %>%
    group_by(date) %>%
    summarise(hcdc_n_rtest_test = sum(rtest_test, na.rm = TRUE),
              hcdc_n_rtest_nguoi = sum(rtest_nguoi, na.rm = TRUE),
              hcdc_n_rtest_duong = sum(rtest_duong, na.rm = TRUE),
              .groups = "drop")
)

saveRDS(rtest_sum, file = file.path(data_path, "save", "rtest_sum.rds"))
saveRDS(rtest_sum_date, file = file.path(data_path, "save", "rtest_sum_date.rds"))

# PCR + test nhanh --------------------------------------------------------

cases_pcr <- readRDS(file.path(file.path(data_path, "save"), "cases_pcr.rds"))
cases_testnhanh <- readRDS(file.path(file.path(data_path, "save"), "cases_testnhanh.rds"))

hcdc_ca <- rbindlist(list(cases_pcr, cases_testnhanh %>% mutate(test_type = "TEST NHANH")), fill = TRUE)
saveRDS(hcdc_ca, file = file.path(file.path(data_path, "save"), "hcdc_ca.rds"))

library(RecordLinkage)
tmp <- hcdc_ca[, c("ho_va_ten", "nam_sinh", "gioi_tinh", "quan_huyen", "phuong_xa")]
check_name <- compare.dedup(dataset = tmp, blockfld = 1, strcmp = TRUE)$pairs

pair <- subset(check_name, nam_sinh > 0.99 & gioi_tinh > 0.99 & (quan_huyen > 0.95) & (phuong_xa > 0.95))
dup <- unique(pair$id2)
hcdc_ca_final <- hcdc_ca[-dup, ]
saveRDS(hcdc_ca_final, file = file.path(file.path(data_path, "save"), "hcdc_ca_loctrung.rds"))

hcdc_ca_final2 <- hcdc_ca[test_date_taken < "2021-08-23" | (test_date_taken >= "2021-10-02" & test_date_taken < "2021-10-12") | (((test_date_taken >= "2021-08-23" & test_date_taken < "2021-10-02") | (test_date_taken >= "2021-10-12")) & ! test_reason2 %in% c("02. TEST NHANH DUONG"))]
saveRDS(hcdc_ca_final2, file = file.path(file.path(data_path, "save"), "hcdc_ca_loctrung2.rds"))

# get hospitalization data -----------------------------------------------------

## tuvong
bv3 <- c("BỆNH VIỆN CHỢ RẪY", "BỆNH VIỆN BỆNH NHIỆT ĐỚI", "BỆNH VIỆN HỒI SỨC COVID-19 (BỆNH VIỆN UNG BƯỚU CƠ SỞ 2)", "BỆNH VIỆN QUÂN Y 175", "TRUNG TÂM HỒI SỨC COVID-19 - ĐẠI HỌC Y DƯỢC", "TRUNG TÂM HỒI SỨC COVID-19 - BỆNH VIỆN VIỆT ĐỨC", "TRUNG TÂM HỒI SỨC COVID -19 TW HUẾ", "TRUNG TÂM HỒI SỨC COVID-19 BẠCH MAI", "BỆNH VIỆN DÃ CHIẾN ĐIỀU TRỊ COVID-19 PHƯỚC LỘC", "BỆNH VIỆN DÃ CHIẾN ĐIỀU TRỊ COVID-19 5G", "TRUNG TÂM HỒI SỨC NGƯỜI BỆNH COVID-19 DÃ CHIẾN SỐ 16",
         "BV CHO RAY", "BV BENH NHIET DOI", "BV HS COVID-19", "BV QUAN Y 175")

load(file = file.path(Lmisc::get.dropbox.folder(), "Workspace", "Research", "DOH_COVID19", "dulieu", "raw", "dulieu_cu", "data", "final", "dieutri_20210807143236.Rdata"))
dieutri01 <- dieutri_ngaybc_tang %>%
  group_by(ngaybc) %>%
  summarise(n_dieutri = sum(n_dieutri, na.rm = TRUE),
            n_tuvong = sum(n_tuvong, na.rm = TRUE),
            n_xuatvien = sum(n_xuatvien2, na.rm = TRUE),
            n_tuvong_congdon = sum(n_tuvong_congdon, na.rm = TRUE),
            n_chuyenden = sum(n_chuyenden, na.rm = TRUE),
            n_chuyenlen = sum(n_chuyen_tangtren, na.rm = TRUE),
            n_giuong = sum(n_bed_tk, na.rm = TRUE)) %>%
  ungroup() %>%
  arrange(ngaybc) %>%
  mutate(n_tuvong_congdon2 = cumsum(n_tuvong),
         n_xuatvien_congdon2 = cumsum(n_xuatvien),
         n_dieutri_congdon2 = n_dieutri + n_tuvong_congdon2 + n_xuatvien_congdon2,
         n_chuyenden_congdon2 = cumsum(n_chuyenden),
         n_chuyenlen_congdon2 = cumsum(n_chuyenlen))

dieutri <- read_excel(path = file.path(Lmisc::get.dropbox.folder(), "Workspace", "Research", "DOH_COVID19", "dulieu", "database", "doh_bcn_bvdt.xlsx"))
tuvong <- dieutri %>%
  filter(tangdt %in% c("TẦNG 2", "TẦNG 3") & (!ngaybc %in% c("20210804_chieu", "20210805_chieu"))) %>%
  group_by(ngaybc) %>%
  summarise(n_tuvong = sum(as.numeric(n_tuvong), na.rm = TRUE),
            n_tuvong_khac = sum(as.numeric(n_tuvong_khac), na.rm = TRUE),
            n_tuvong_congdon = sum(as.numeric(n_tuvong_congdon), na.rm = TRUE),
            n_tuvong_khac_congdon = sum(as.numeric(n_tuvong_khac_congdon), na.rm = TRUE),
            n_dieutri = sum(as.numeric(n_dieutri), na.rm = TRUE),
            n_dieutri_congdon = sum(as.numeric(n_dieutri_congdon), na.rm = TRUE),
            n_xuatvien = sum(n_xuatvien, na.rm = TRUE),
            n_xuatvien_congdon = sum(n_xuatvien_congdon, na.rm = TRUE),
            n_chuyenden = sum(n_chuyenden, na.rm = TRUE),
            n_chuyenlen = sum(n_chuyenlen, na.rm = TRUE),
            n_giuong = sum(as.numeric(n_tonggiuong), na.rm = TRUE),
            n_oxy_tong = sum(as.numeric(n_oxy_tong), na.rm = TRUE),
            n_oxy_dung = sum(as.numeric(n_oxy), na.rm = TRUE)
  )
tuvong$ngaybc[tuvong$ngaybc == "20210804_sang"] <- "20210804"
tuvong$ngaybc[tuvong$ngaybc == "20210805_sang"] <- "20210805"
tuvong$ngaybc <- substr(tuvong$ngaybc, start = 1, stop = 8)
tuvong$n_tuvong[tuvong$ngaybc == "20210919"] <- tuvong$n_tuvong_khac[tuvong$ngaybc == "20210919"]
tuvong$n_tuvong[tuvong$ngaybc == "20210920"] <- tuvong$n_tuvong_khac[tuvong$ngaybc == "20210920"]
tuvong$n_tuvong_congdon[tuvong$ngaybc == "20210919"] <- tuvong$n_tuvong_khac_congdon[tuvong$ngaybc == "20210919"]
tuvong$n_tuvong_congdon[tuvong$ngaybc == "20210920"] <- tuvong$n_tuvong_khac_congdon[tuvong$ngaybc == "20210920"]
tuvong$n_dieutri[tuvong$ngaybc == "20210812"] <- 97975 - 3803 - 65135
tuvong$n_xuatvien_congdon[tuvong$ngaybc == "20210920"] <- tuvong$n_xuatvien_congdon[tuvong$ngaybc == "20210921"] - tuvong$n_xuatvien[tuvong$ngaybc == "20210921"]
tuvong$n_xuatvien_congdon[tuvong$ngaybc == "20210919"] <- 220652 - 38174 - 13277
tuvong$n_xuatvien[tuvong$ngaybc == "20210919"] <- tuvong$n_xuatvien_congdon[tuvong$ngaybc == "20210919"] - tuvong$n_xuatvien_congdon[tuvong$ngaybc == "20210918"]
tuvong$n_xuatvien[tuvong$ngaybc == "20210920"] <- tuvong$n_xuatvien_congdon[tuvong$ngaybc == "20210920"] - tuvong$n_xuatvien_congdon[tuvong$ngaybc == "20210919"]
tuvong <- tuvong[, c("ngaybc", "n_dieutri", "n_dieutri_congdon", "n_tuvong", "n_tuvong_congdon", "n_xuatvien", "n_xuatvien_congdon", "n_chuyenden", "n_chuyenlen", "n_giuong", "n_oxy_tong", "n_oxy_dung")] %>%
  mutate(ngaybc = ymd(ngaybc)) %>%
  filter(ngaybc > "2021-08-06")

dieutri02 <- rbind(dieutri01[, c("ngaybc", "n_dieutri", "n_tuvong", "n_xuatvien", "n_chuyenden", "n_chuyenlen", "n_giuong")] %>%
                     mutate(n_oxy_tong = NA,
                            n_oxy_dung = NA),
                   tuvong[, c("ngaybc", "n_dieutri", "n_tuvong", "n_xuatvien", "n_chuyenden", "n_chuyenlen", "n_giuong", "n_oxy_tong", "n_oxy_dung")]) %>%
  arrange(ngaybc) %>%
  mutate(n_tuvong_congdon = cumsum(n_tuvong),
         n_xuatvien_congdon = cumsum(n_xuatvien),
         n_dieutri_congdon = n_dieutri + n_tuvong_congdon + n_xuatvien_congdon,
         n_chuyenden_congdon = cumsum(n_chuyenden),
         n_chuyenlen_congdon = cumsum(n_chuyenlen)) %>%
  setNames(nm = c("date", "doh_n_dieutri", "doh_n_tuvong", "doh_n_xuatvien", "doh_n_chuyenden", "doh_n_chuyenlen", "doh_n_giuong", "doh_n_oxy_tong", "doh_n_oxy_dung", "doh_n_tuvong_congdon", "doh_n_xuatvien_congdon", "doh_n_dieutri_congdon", "doh_n_chuyenden_congdon", "doh_n_chuyenlen_congdon"))

tang231 <- dieutri_final2 %>%
  filter(!is.na(tangdt2)) %>%
  mutate(tang = ifelse(bv %in% bv3, 3, 2),
         ngaybc = ymd(ngaybc)) %>%
  group_by(ngaybc, tang) %>%
  summarise(n_dieutri = sum(n_dieutri, na.rm = TRUE),
            n_tuvong = sum(n_tuvong, na.rm = TRUE),
            n_xuatvien = sum(n_xuatvien2, na.rm = TRUE),
            n_tuvong_congdon = sum(n_tuvong_congdon, na.rm = TRUE),
            n_chuyenden = sum(n_chuyenden, na.rm = TRUE),
            n_chuyenlen = sum(n_chuyen_tangtren, na.rm = TRUE),
            n_chuyenvien = NA,
            n_giuong = sum(n_bed_tk, na.rm = TRUE)) %>%
  ungroup() %>%
  group_by(tang) %>%
  arrange(ngaybc) %>%
  mutate(n_tuvong_congdon2 = cumsum(n_tuvong),
         n_xuatvien_congdon2 = cumsum(n_xuatvien),
         n_dieutri_congdon2 = n_dieutri + n_tuvong_congdon2 + n_xuatvien_congdon2,
         n_chuyenden_congdon2 = cumsum(n_chuyenden),
         n_chuyenlen_congdon2 = cumsum(n_chuyenlen)) %>%
  ungroup()

tang232 <- dieutri %>%
  filter(tangdt %in% c("TẦNG 2", "TẦNG 3") & (!ngaybc %in% c("20210804_chieu", "20210805_chieu"))) %>%
  mutate(tang = ifelse(bv %in% bv3, 3, 2)) %>%
  group_by(ngaybc, tang) %>%
  summarise(n_tuvong = sum(as.numeric(n_tuvong), na.rm = TRUE),
            n_tuvong_congdon = sum(as.numeric(n_tuvong_congdon), na.rm = TRUE),
            n_dieutri = sum(as.numeric(n_dieutri), na.rm = TRUE),
            n_dieutri_congdon = sum(as.numeric(n_dieutri_congdon), na.rm = TRUE),
            n_xuatvien = sum(n_xuatvien, na.rm = TRUE),
            n_xuatvien_congdon = sum(n_xuatvien_congdon, na.rm = TRUE),
            n_chuyenden = sum(n_chuyenden, na.rm = TRUE),
            n_chuyenlen = sum(n_chuyenlen, na.rm = TRUE),
            n_chuyenvien = NA,
            n_giuong = sum(as.numeric(n_tonggiuong), na.rm = TRUE),
            n_oxy_tong = sum(as.numeric(n_oxy_tong), na.rm = TRUE),
            n_oxy_dung = sum(as.numeric(n_oxy), na.rm = TRUE),
            .groups = "drop") %>%
  mutate(ngaybc = ymd(substr(ngaybc, start = 1, stop = 8))) %>%
  filter(ngaybc > "2021-08-06")

tang23 <- rbind(tang231[, c("ngaybc", "tang", "n_dieutri", "n_tuvong", "n_xuatvien", "n_chuyenden", "n_chuyenlen", "n_chuyenvien", "n_giuong")] %>%
                  mutate(n_oxy_tong = NA,
                         n_oxy_dung = NA),
                tang232[, c("ngaybc", "tang", "n_dieutri", "n_tuvong", "n_xuatvien", "n_chuyenden", "n_chuyenlen", "n_chuyenvien", "n_giuong", "n_oxy_tong", "n_oxy_dung")]) %>%
  group_by(tang) %>%
  arrange(ngaybc) %>%
  mutate(n_tuvong_congdon = cumsum(n_tuvong),
         n_xuatvien_congdon = cumsum(n_xuatvien),
         n_chuyenlen_congdon = cumsum(n_chuyenlen),
         n_dieutri_congdon = n_dieutri + n_tuvong_congdon + n_xuatvien_congdon + n_chuyenlen_congdon,
         n_chuyenden_congdon = cumsum(n_chuyenden),
         n_chuyenvien_congdon = NA
  ) %>%
  ungroup() %>%
  setNames(nm = c("date", "tang", "doh_n_dieutri", "doh_n_tuvong", "doh_n_xuatvien", "doh_n_chuyenden", "doh_n_chuyenlen", "doh_n_chuyenvien", "doh_n_giuong", "doh_n_oxy_tong", "doh_n_oxy_dung", "doh_n_tuvong_congdon", "doh_n_xuatvien_congdon", "doh_n_chuyenlen_congdon", "doh_n_dieutri_congdon", "doh_n_chuyenden_congdon", "doh_n_chuyenvien_congdon"))

tang01 <- read_excel(path = file.path(Lmisc::get.dropbox.folder(), "Workspace", "Research", "DOH_COVID19", "dulieu", "database", "doh_bcn_bvdt_tang01.xlsx"))
tang1 <- tang01 %>%
  select(ngaybc, n_tonggiuong, n_cachly, n_cachly_congdon, n_chuyenden, n_chuyenlen, n_oxy_tong, n_oxy, n_tuvong_vien, n_tuvong_vien_congdon, n_xuatvien, n_xuatvien_congdon) %>%
  mutate(ngaybc = ymd(substr(ngaybc, start = 1, stop = 8))) %>%
  group_by(ngaybc) %>%
  summarise(tang = 1,
            n_dieutri = sum(as.numeric(n_cachly), na.rm = TRUE),
            n_tuvong = sum(as.numeric(n_tuvong_vien), na.rm = TRUE),
            n_xuatvien = sum(as.numeric(n_xuatvien), na.rm = TRUE),
            n_chuyenden = sum(as.numeric(n_chuyenden), na.rm = TRUE),
            n_chuyenlen = sum(as.numeric(n_chuyenlen), na.rm = TRUE),
            n_chuyenvien = NA,
            n_giuong = sum(as.numeric(n_tonggiuong), na.rm = TRUE),
            n_oxy_tong = sum(as.numeric(n_oxy_tong), na.rm = TRUE),
            n_oxy_dung = sum(as.numeric(n_oxy), na.rm = TRUE),
            n_tuvong_congdon = sum(as.numeric(n_tuvong_vien_congdon), na.rm = TRUE),
            n_xuatvien_congdon = sum(as.numeric(n_xuatvien_congdon), na.rm = TRUE),
            #n_dieutri_congdon = sum(as.numeric(n_cachly_congdon), na.rm = TRUE),
            .groups = "drop") %>%
  arrange(ngaybc) %>%
  mutate(n_tuvong_congdon = cumsum(n_tuvong) + 306,
         n_xuatvien_congdon = cumsum(n_xuatvien) + 10035,
         n_chuyenlen_congdon = cumsum(n_chuyenlen),
         n_dieutri_congdon = n_dieutri + n_tuvong_congdon + n_xuatvien_congdon + n_chuyenlen_congdon + 9089,
         n_chuyenden_congdon = cumsum(n_chuyenden),
         n_chuyenvien_congdon = NA) %>%
  setNames(nm = c("date", "tang", "doh_n_dieutri", "doh_n_tuvong", "doh_n_xuatvien", "doh_n_chuyenden", "doh_n_chuyenlen", "doh_n_chuyenvien", "doh_n_giuong", "doh_n_oxy_tong", "doh_n_oxy_dung", "doh_n_tuvong_congdon", "doh_n_xuatvien_congdon", "doh_n_chuyenlen_congdon", "doh_n_dieutri_congdon", "doh_n_chuyenden_congdon", "doh_n_chuyenvien_congdon"))

tang0 <- tang01 %>%
  select(ngaybc, n_f0tainha_moi_hiendien, n_f0tainha_moi_24h, n_chuyenlen, n_f0tainha_oxy_tong, n_f0tainha_oxy_used, n_f0tainha_tuvong, n_f0tainha_tuvong_congdon, n_f0tainha_hoanthanh_24h, n_f0tainha_hoanthanh_congdon, n_f0moi_chuyencachly_24h, n_f0moi_chuyencachly_congdon, n_f0moi_canchuyenvien_24h, n_f0moi_canchuyenvien_congdon) %>%
  mutate(ngaybc = ymd(substr(ngaybc, start = 1, stop = 8))) %>%
  group_by(ngaybc) %>%
  summarise(tang = 0,
            n_dieutri = sum(as.numeric(n_f0tainha_moi_hiendien), na.rm = TRUE),
            n_tuvong = sum(as.numeric(n_f0tainha_tuvong), na.rm = TRUE),
            n_xuatvien = sum(as.numeric(n_f0tainha_hoanthanh_24h), na.rm = TRUE),
            n_chuyenden = sum(as.numeric(n_f0tainha_moi_24h), na.rm = TRUE),
            n_chuyenlen = sum(as.numeric(n_f0moi_chuyencachly_24h), na.rm = TRUE) + sum(as.numeric(n_f0moi_canchuyenvien_24h), na.rm = TRUE),
            n_chuyenvien = sum(as.numeric(n_f0moi_canchuyenvien_24h), na.rm = TRUE),
            n_giuong = NA,
            n_oxy_tong = sum(as.numeric(n_f0tainha_oxy_tong), na.rm = TRUE),
            n_oxy_dung = sum(as.numeric(n_f0tainha_oxy_used), na.rm = TRUE),
            n_tuvong_congdon = sum(as.numeric(n_f0tainha_tuvong_congdon), na.rm = TRUE),
            n_xuatvien_congdon = sum(as.numeric(n_f0tainha_hoanthanh_congdon), na.rm = TRUE),
            .groups = "drop") %>%
  filter(ngaybc >= "2021-08-19") %>%
  arrange(ngaybc) %>%
  mutate(n_tuvong_congdon = cumsum(n_tuvong) + 694,
         n_xuatvien_congdon = cumsum(n_xuatvien) + 6951,
         n_chuyenlen_congdon = cumsum(n_chuyenlen),
         n_dieutri_congdon = n_dieutri + n_tuvong_congdon + n_xuatvien_congdon + n_chuyenlen_congdon,
         n_chuyenden_congdon = cumsum(n_chuyenden),
         n_chuyenvien_congdon = cumsum(n_chuyenvien)) %>%
  setNames(nm = c("date", "tang", "doh_n_dieutri", "doh_n_tuvong", "doh_n_xuatvien", "doh_n_chuyenden", "doh_n_chuyenlen", "doh_n_chuyenvien", "doh_n_giuong", "doh_n_oxy_tong", "doh_n_oxy_dung", "doh_n_tuvong_congdon", "doh_n_xuatvien_congdon", "doh_n_chuyenlen_congdon", "doh_n_dieutri_congdon", "doh_n_chuyenden_congdon", "doh_n_chuyenvien_congdon"))

dieutri_tang <- rbind(tang0, tang1, tang23) %>%
  group_by(tang) %>%
  arrange(date) %>%
  mutate(doh_n_dieutri_congdon_t5 = lag(doh_n_dieutri_congdon, 5),
         cfr = doh_n_tuvong_congdon/doh_n_dieutri_congdon_t5,
         cfr_up = cfr + 1.96 * sqrt(cfr * (1 - cfr)/doh_n_dieutri_congdon_t5),
         cfr_lo = cfr - 1.96 * sqrt(cfr * (1 - cfr)/doh_n_dieutri_congdon_t5)) %>%
  ungroup()

save(dieutri02, dieutri_tang, file = file.path(file.path(data_path, "save"), "dieutri.Rdata"))


# ncsc --------------------------------------------------------------------

ncsc_covid <- readRDS(file = file.path(data_path, "..", "NCSC", "cleancovid.rds")) %>%
  filter(province == "TP HCM") %>%
  dplyr::select(-province) %>%
  pivot_wider(names_from = var,
              values_from = n) %>%
  setNames(nm = c("date", "ncsc_prev", "ncsc_inc", "ncsc_cum_inc", "ncsc_cum_death"))

saveRDS(ncsc_covid, file = file.path(data_path, "save", "ncsc_covid.rds"))


# vaccine -----------------------------------------------------------------

vaccine <- merge(
  read_excel(path = file.path(data_path, "additional", "tiemchung", "vaccine_sum.xlsx"), sheet = "sum", col_types = "text") %>%
    mutate(date = as.Date(as.numeric(ngay), origin = "1899-12-30", tz = Sys.timezone()),
           mui01 = as.numeric(mui01),
           mui02 = as.numeric(mui02),
           pmui01 = as.numeric(mui01)/as.numeric(n),
           pmui02 = as.numeric(mui02)/as.numeric(n)) %>%
    arrange(date) %>%
    dplyr::select(date, mui01, mui02, pmui01, pmui02),

  read_excel(path = file.path(data_path, "additional", "tiemchung", "vaccine_sum.xlsx"), sheet = "tren50", col_types = "text") %>%
    mutate(date = as.Date(as.numeric(ngay), origin = "1899-12-30", tz = Sys.timezone()),
           mui01_cum = as.numeric(mui01_cum),
           mui02_cum = as.numeric(mui02_cum),
           n = as.numeric(n)) %>%
    group_by(date) %>%
    summarise(mui01_50_cum = sum(mui01_cum),
              mui01_65_cum = mui01_cum[nhomtuoi == "65+"],
              mui02_50_cum = sum(mui02_cum),
              mui02_65_cum = mui02_cum[nhomtuoi == "65+"],
              pmui01_50_cum = sum(mui01_cum)/sum(n),
              pmui01_65_cum = mui01_cum[nhomtuoi == "65+"]/n[nhomtuoi == "65+"],
              pmui02_50_cum = sum(mui02_cum)/sum(n),
              pmui02_65_cum = mui02_cum[nhomtuoi == "65+"]/n[nhomtuoi == "65+"],
              .groups = "drop"),
  by = "date", all = TRUE
)
saveRDS(vaccine, file = file.path(data_path, "save", "vaccine.rds"))

# all covid data ---------------------------------------------------------------

hcdc_ca <- readRDS(file.path(file.path(data_path, "save"), "hcdc_ca.rds"))
hcdc_ca_loctrung <- readRDS(file.path(file.path(data_path, "save"), "hcdc_ca_loctrung2.rds"))
load(file.path(file.path(data_path, "save"), "pcr_sum.Rdata"))
rtest_sum_date <- readRDS(file.path(data_path, "save", "rtest_sum_date.rds"))
load(file.path(file.path(data_path, "save"), "dieutri.Rdata"))
ncsc_covid <- readRDS(file.path(data_path, "save", "ncsc_covid.rds"))
vaccine <- readRDS(file.path(data_path, "save", "vaccine.rds"))


hcdc_ca2 <- merge(merge(merge(merge(
  get_ma(data = subset(hcdc_ca, test_type == "PCR"), datevar = "test_date_taken", t = 7, type = "mean") %>%
    dplyr::select(date, nob) %>%
    setNames(nm = c("date", "hcdc_n_capcr")),
  get_ma(data = subset(hcdc_ca, test_type == "TEST NHANH"), datevar = "test_date_taken", t = 7, type = "mean") %>%
    dplyr::select(date, nob) %>%
    setNames(nm = c("date", "hcdc_n_cartest")),
  by = "date", all = TRUE
),
get_ma(data = hcdc_ca_loctrung, datevar = "test_date_taken", t = 7, type = "mean") %>%
  dplyr::select(date, nob) %>%
  setNames(nm = c("date", "hcdc_n_ca")),
by = "date", all = TRUE
),
pcr_sum_date,
by = "date", all = TRUE),
rtest_sum_date, by = "date", all.x = TRUE) %>%
  mutate(hcdc_n_capcr = ifelse(is.na(hcdc_n_capcr), 0, hcdc_n_capcr),
         hcdc_n_cartest = ifelse(is.na(hcdc_n_cartest), 0, hcdc_n_cartest),
         hcdc_n_pcr_nguoi = ifelse(is.na(hcdc_n_pcr_nguoi), 0, hcdc_n_pcr_nguoi),
         hcdc_n_pcr_test = ifelse(is.na(hcdc_n_pcr_test), 0, hcdc_n_pcr_test),
         hcdc_n_pcr_test_don = ifelse(is.na(hcdc_n_pcr_test_don), 0, hcdc_n_pcr_test_don),
         hcdc_n_pcr_test_gop = ifelse(is.na(hcdc_n_pcr_test_gop), 0, hcdc_n_pcr_test_gop),
         hcdc_n_pcr_test_don_duong = ifelse(is.na(hcdc_n_pcr_test_don_duong), 0, hcdc_n_pcr_test_don_duong),
         hcdc_n_rtest_test = ifelse(is.na(hcdc_n_rtest_test), 0, hcdc_n_rtest_test),
         hcdc_n_rtest_nguoi = ifelse(is.na(hcdc_n_rtest_nguoi), 0, hcdc_n_rtest_nguoi),
         hcdc_n_rtest_duong = ifelse(is.na(hcdc_n_rtest_duong), 0, hcdc_n_rtest_duong),
         hcdc_n_ca = ifelse(is.na(hcdc_n_ca), 0, hcdc_n_ca),
         hcdc_n_test = hcdc_n_pcr_test + hcdc_n_rtest_test,
         hcdc_n_test_nguoi = hcdc_n_pcr_nguoi + hcdc_n_rtest_nguoi) %>%
  arrange(date) %>%
  mutate(hcdc_cum_inc_capcr = cumsum(hcdc_n_capcr),
         hcdc_cum_inc_cartest = cumsum(hcdc_n_cartest),
         hcdc_cum_inc_ca = cumsum(hcdc_n_ca),
         hcdc_cum_pcr_nguoi = cumsum(hcdc_n_pcr_nguoi),
         hcdc_cum_pcr_test = cumsum(hcdc_n_pcr_test),
         hcdc_cum_rtest_test = cumsum(hcdc_n_rtest_test),
         hcdc_cum_rtest_nguoi = cumsum(hcdc_n_rtest_nguoi),
         hcdc_cum_test = cumsum(hcdc_n_test),
         hcdc_cum_test_nguoi = cumsum(hcdc_n_test_nguoi)
  )

covid <- merge(merge(merge(hcdc_ca2, dieutri02, by = "date", all = TRUE), ncsc_covid, by = "date", all.x = TRUE), vaccine, by = "date", all.x = TRUE) %>%
  arrange(date) %>%
  mutate(hcdc_ma_mean_capcr_07 = frollmean(x = hcdc_n_capcr, n = 7, align = "right"),
         hcdc_ma_mean_capcr_14 = frollmean(x = hcdc_n_capcr, n = 14, align = "right"),
         hcdc_ma_sum_capcr_14 = frollsum(x = hcdc_n_capcr, n = 14, align = "right"),
         hcdc_ma_mean_cartest_07 = frollmean(x = hcdc_n_cartest, n = 7, align = "right"),
         hcdc_ma_mean_cartest_14 = frollmean(x = hcdc_n_cartest, n = 14, align = "right"),
         hcdc_ma_sum_cartest_14 = frollsum(x = hcdc_n_cartest, n = 14, align = "right"),
         hcdc_ma_mean_ca_07 = frollmean(x = hcdc_n_ca, n = 7, align = "right"),
         hcdc_ma_mean_ca_14 = frollmean(x = hcdc_n_ca, n = 14, align = "right"),
         hcdc_ma_sum_ca_14 = frollsum(x = hcdc_n_ca, n = 14, align = "right"),
         hcdc_ma_mean_pcr_nguoi_07 = frollmean(x = hcdc_n_pcr_nguoi, n = 7, align = "right"),
         hcdc_ma_mean_pcr_nguoi_14 = frollmean(x = hcdc_n_pcr_nguoi, n = 14, align = "right"),
         hcdc_ma_sum_pcr_nguoi_14 = frollsum(x = hcdc_n_pcr_nguoi, n = 14, align = "right"),
         hcdc_ma_mean_pcr_test_07 = frollmean(x = hcdc_n_pcr_test, n = 7, align = "right"),
         hcdc_ma_mean_pcr_test_14 = frollmean(x = hcdc_n_pcr_test, n = 14, align = "right"),
         hcdc_ma_sum_pcr_test_14 = frollsum(x = hcdc_n_pcr_test, n = 14, align = "right"),
         hcdc_ma_mean_rtest_test_07 = frollmean(x = hcdc_n_rtest_test, n = 7, align = "right"),
         hcdc_ma_mean_rtest_test_14 = frollmean(x = hcdc_n_rtest_test, n = 14, align = "right"),
         hcdc_ma_sum_rtest_test_14 = frollsum(x = hcdc_n_rtest_test, n = 14, align = "right"),
         hcdc_ma_mean_rtest_nguoi_07 = frollmean(x = hcdc_n_rtest_nguoi, n = 7, align = "right"),
         hcdc_ma_mean_rtest_nguoi_14 = frollmean(x = hcdc_n_rtest_nguoi, n = 14, align = "right"),
         hcdc_ma_sum_rtest_nguoi_14 = frollsum(x = hcdc_n_rtest_nguoi, n = 14, align = "right"),
         hcdc_ma_mean_test_07 = frollmean(x = hcdc_n_test, n = 7, align = "right"),
         hcdc_ma_mean_test_14 = frollmean(x = hcdc_n_test, n = 14, align = "right"),
         hcdc_ma_sum_test_14 = frollsum(x = hcdc_n_test, n = 14, align = "right"),
         hcdc_ma_mean_test_nguoi_07 = frollmean(x = hcdc_n_test_nguoi, n = 7, align = "right"),
         hcdc_ma_mean_test_nguoi_14 = frollmean(x = hcdc_n_test_nguoi, n = 14, align = "right"),
         hcdc_ma_sum_test_nguoi_14 = frollsum(x = hcdc_n_test_nguoi, n = 14, align = "right"),


         doh_ma_mean_tuvong_07 = frollmean(x = doh_n_tuvong, n = 7, align = "right"),
         doh_ma_mean_dieutri_07 = frollmean(x = doh_n_dieutri, n = 7, align = "right"),
         doh_ma_mean_xuatvien_07 = frollmean(x = doh_n_xuatvien, n = 7, align = "right"),
         mui01 = ifelse(is.na(mui01), 0, mui01),
         mui02 = ifelse(is.na(mui02), 0, mui02),
         pmui01 = ifelse(is.na(pmui01), 0, pmui01),
         pmui02 = ifelse(is.na(pmui02), 0, pmui02),
         mui01_cum = cumsum(mui01),
         mui02_cum = cumsum(mui02),
         pmui01_cum = cumsum(pmui01),
         pmui02_cum = cumsum(pmui02),

         mui01_t14 = lag(mui01, 14),
         mui02_t14 = lag(mui02, 14),
         mui01_t14 = ifelse(is.na(mui01_t14), 0, mui01_t14),
         mui02_t14 = ifelse(is.na(mui02_t14), 0, mui02_t14),
         mui01_t14_cum = cumsum(mui01_t14),
         mui02_t14_cum = cumsum(mui02_t14),

         pmui01_t14 = lag(pmui01, 14),
         pmui02_t14 = lag(pmui02, 14),
         pmui01_t14 = ifelse(is.na(pmui01_t14), 0, pmui01_t14),
         pmui02_t14 = ifelse(is.na(pmui02_t14), 0, pmui02_t14),
         pmui01_t14_cum = cumsum(pmui01_t14),
         pmui02_t14_cum = cumsum(pmui02_t14),

         doh_n_dieutri_congdon_t5 = lag(doh_n_dieutri_congdon, 5),
         hcdc_cum_inc_capcr_t5 = lag(hcdc_cum_inc_capcr, 5),
         hcdc_cum_inc_ca_t5 = lag(hcdc_cum_inc_ca, 5),

         cfr1 = doh_n_tuvong_congdon/doh_n_dieutri_congdon,
         cfr12 = doh_n_tuvong_congdon/doh_n_dieutri_congdon_t5,
         cfr2 = doh_n_tuvong_congdon/hcdc_cum_inc_capcr,
         cfr22 = doh_n_tuvong_congdon/hcdc_cum_inc_capcr_t5,
         cfr3 = doh_n_tuvong_congdon/hcdc_cum_inc_ca,
         cfr32 = doh_n_tuvong_congdon/hcdc_cum_inc_ca_t5)

saveRDS(covid, file.path(data_path, "save", "covid.rds"))

# Rt ----------------------------------------------------------------------

date_start <- ymd("2021-04-27") # ngay bat dau
datetime_update <- ymd("2021-10-28") # ngay ket thuc
#timestamp <- gsub(pattern = "-|:| |+", replacement = "", x = Sys.time())
timestamp <- "20211101055217"

### calculate Rt
covid <- readRDS(file.path(data_path, "save", "covid.rds")) %>%
  filter(date >= date_start & date <= datetime_update)
#load(file.path("data", "HCDC", "database", "derived", "si_20210716.Rdata"))
#load(file.path("data", "HCDC", "database", "derived", "rd_pos2death.Rdata"))
#load(file.path("data", "HCDC", "database", "derived", "rd_pos2hos.Rdata"))

#### set up
library(EpiNow2)
## incubation time
incubation_period <- get_incubation_period(disease = "SARS-CoV-2", source = "lauer")
## reporting delay
# load(file.path(data_path, "derived", "rd_pos.Rdata"))

reporting_delay <- list(
  mean = convert_to_logmean(mean = 3, sd = 1), mean_sd = 0.1,
  sd = convert_to_logsd(mean = 3, sd = 1), sd_sd = 0.1,
  max = 10
)

pos2hos_delay <- list(
  mean = convert_to_logmean(mean = 5, sd = 1), mean_sd = 0.1,
  sd = convert_to_logsd(mean = 5, sd = 1), sd_sd = 0.1,
  max = 30
)

pos2death_delay <- list(
  mean = convert_to_logmean(mean = 15, sd = 1), mean_sd = 0.1,
  sd = convert_to_logsd(mean = 15, sd = 1), sd_sd = 0.1,
  max = 50
)

## generation time
# load(file.path(data_path, "derived", "si_20210716.Rdata"))
# generation_time <- list(mean = si_gamma_fit$mu,
#                         mean_sd = si_gamma_fit$se[1],
#                         sd = si_gamma_fit$sd,
#                         sd_sd = si_gamma_fit$se[2],
#                         max = max(si_obs))

generation_time <- get_generation_time(disease = "SARS-CoV-2", source = "ganyani")

## f0 - pcr
epinow_pcr_est <- epinow(
  reported_cases = transmute(covid, date = date, confirm = hcdc_n_capcr),
  generation_time = generation_time,
  delays = delay_opts(incubation_period, reporting_delay),
  return_output = TRUE,
  verbose = TRUE,
  rt = NULL, backcalc = backcalc_opts(),
  obs = obs_opts(scale = list(mean = 0.4, sd = 0.05)),
  horizon = 0,
  stan = stan_opts(samples = 2000, chains = 4, cores = 4)
)

epinow_pcr_sum <- epinow_pcr_est$summary
epinow_pcr_est$measure <- c("Số ca mới mắc bệnh ngày hôm qua",
                            "Khuynh hướng thay đổi số ca mỗi ngày",
                            "Hệ số lây nhiễm hiệu quả",
                            "Tốc độ tăng trưởng của dịch",
                            "Thời gian tăng gấp đôi/giảm một nửa (ngày)")
epinow_pcr_est_sum <- rbind(
  subset(epinow_pcr_est$estimates$summarised, variable == "R"),
  subset(epinow_pcr_est$estimates$summarised, variable == "reported_cases"),
  subset(epinow_pcr_est$estimates$summarised, variable == "infections"),
  subset(epinow_pcr_est$estimates$summarised, variable == "growth_rate")
)
save(epinow_pcr_est, epinow_pcr_sum, epinow_pcr_est_sum, file = file.path(data_path, "derived", paste0("epinow_pcr_", timestamp, ".Rdata")))

## f0 - all
epinow_f0_est <- epinow(
  reported_cases = transmute(covid, date = date, confirm = hcdc_n_ca),
  generation_time = generation_time,
  delays = delay_opts(incubation_period, reporting_delay),
  return_output = TRUE,
  verbose = TRUE,
  rt = NULL, backcalc = backcalc_opts(),
  obs = obs_opts(scale = list(mean = 0.4, sd = 0.05)),
  horizon = 0,
  stan = stan_opts(samples = 2000, chains = 4, cores = 4)
)

epinow_f0_sum <- epinow_f0_est$summary
epinow_f0_est$measure <- c("Số ca mới mắc bệnh ngày hôm qua",
                            "Khuynh hướng thay đổi số ca mỗi ngày",
                            "Hệ số lây nhiễm hiệu quả",
                            "Tốc độ tăng trưởng của dịch",
                            "Thời gian tăng gấp đôi/giảm một nửa (ngày)")
epinow_f0_est_sum <- rbind(
  subset(epinow_f0_est$estimates$summarised, variable == "R"),
  subset(epinow_f0_est$estimates$summarised, variable == "reported_cases"),
  subset(epinow_f0_est$estimates$summarised, variable == "infections"),
  subset(epinow_f0_est$estimates$summarised, variable == "growth_rate")
)
save(epinow_f0_est, epinow_f0_sum, epinow_f0_est_sum, file = file.path(data_path, "derived", paste0("epinow_f0_", timestamp, ".Rdata")))

## death cases
epinow_death_est <- epinow(
  reported_cases = transmute(covid, date = date, confirm = doh_n_tuvong),
  generation_time = generation_time,
  delays = delay_opts(incubation_period, reporting_delay, pos2death_delay),
  return_output = TRUE,
  verbose = TRUE,
  rt = NULL, backcalc = backcalc_opts(),
  obs = obs_opts(scale = list(mean = 0.4, sd = 0.05)),
  horizon = 0,
  stan = stan_opts(samples = 2000, chains = 4, cores = 4)
)

epinow_death_sum <- epinow_death_est$summary
epinow_death_est$measure <- c("Số ca mới mắc bệnh ngày hôm qua",
                              "Khuynh hướng thay đổi số ca mỗi ngày",
                              "Hệ số lây nhiễm hiệu quả",
                              "Tốc độ tăng trưởng của dịch",
                              "Thời gian tăng gấp đôi/giảm một nửa (ngày)")
epinow_death_est_sum <- rbind(
  subset(epinow_death_est$estimates$summarised, variable == "R"),
  subset(epinow_death_est$estimates$summarised, variable == "reported_cases"),
  subset(epinow_death_est$estimates$summarised, variable == "infections"),
  subset(epinow_death_est$estimates$summarised, variable == "growth_rate")
)
save(epinow_death_est, epinow_death_sum, epinow_death_est_sum, file = file.path(data_path, "derived", paste0("epinow_death_", timestamp, ".Rdata")))

## hospitalization
epinow_hos_est <- epinow(
  reported_cases = transmute(covid, date = date, confirm = doh_n_chuyenden ),
  generation_time = generation_time,
  delays = delay_opts(incubation_period, reporting_delay, pos2hos_delay),
  return_output = TRUE,
  verbose = TRUE,
  rt = NULL, backcalc = backcalc_opts(),
  obs = obs_opts(scale = list(mean = 0.4, sd = 0.05)),
  horizon = 0,
  stan = stan_opts(samples = 2000, chains = 4, cores = 4)
)
#saveRDS(epinow_tp_est, file = file.path("data", "HCDC", "database", "derived", paste0("epinow_tp_", timestamp, ".rds")))

epinow_hos_sum <- epinow_hos_est$summary
epinow_hos_est$measure <- c("Số ca mới mắc bệnh ngày hôm qua",
                            "Khuynh hướng thay đổi số ca mỗi ngày",
                            "Hệ số lây nhiễm hiệu quả",
                            "Tốc độ tăng trưởng của dịch",
                            "Thời gian tăng gấp đôi/giảm một nửa (ngày)")
epinow_hos_est_sum <- rbind(
  subset(epinow_hos_est$estimates$summarised, variable == "R"),
  subset(epinow_hos_est$estimates$summarised, variable == "reported_cases"),
  subset(epinow_hos_est$estimates$summarised, variable == "infections"),
  subset(epinow_hos_est$estimates$summarised, variable == "growth_rate")
)
save(epinow_hos_est, epinow_hos_sum, epinow_hos_est_sum, file = file.path(data_path, "derived", paste0("epinow_hos_", timestamp, ".Rdata")))

## chuyen len
epinow_refer_est <- epinow(
  reported_cases = transmute(covid, date = date, confirm = doh_n_chuyenlen),
  generation_time = generation_time,
  delays = delay_opts(incubation_period, reporting_delay, pos2hos_delay),
  return_output = TRUE,
  verbose = TRUE,
  rt = NULL, backcalc = backcalc_opts(),
  obs = obs_opts(scale = list(mean = 0.4, sd = 0.05)),
  horizon = 0,
  stan = stan_opts(samples = 2000, chains = 4, cores = 4)
)
#saveRDS(epinow_tp_est, file = file.path("data", "HCDC", "database", "derived", paste0("epinow_tp_", timestamp, ".rds")))

epinow_refer_sum <- epinow_refer_est$summary
epinow_refer_est$measure <- c("Số ca mới mắc bệnh ngày hôm qua",
                            "Khuynh hướng thay đổi số ca mỗi ngày",
                            "Hệ số lây nhiễm hiệu quả",
                            "Tốc độ tăng trưởng của dịch",
                            "Thời gian tăng gấp đôi/giảm một nửa (ngày)")
epinow_refer_est_sum <- rbind(
  subset(epinow_refer_est$estimates$summarised, variable == "R"),
  subset(epinow_refer_est$estimates$summarised, variable == "reported_cases"),
  subset(epinow_refer_est$estimates$summarised, variable == "infections"),
  subset(epinow_refer_est$estimates$summarised, variable == "growth_rate")
)
save(epinow_refer_est, epinow_refer_sum, epinow_refer_est_sum, file = file.path(data_path, "derived", paste0("epinow_refer_", timestamp, ".Rdata")))

## theo quan huyen:
hcdc_ca_loctrung <- readRDS(file.path(data_path, "save", "hcdc_ca_loctrung2.rds")) %>%
  group_by(test_date_taken, quanhuyen, .drop = FALSE) %>%
  summarise(confirm = n(),
            .groups = "drop") %>%
  rename(date = test_date_taken,
         region = quanhuyen)
tmp <- expand.grid(date = unique(hcdc_ca_loctrung$date),
                   region = unique(hcdc_ca_loctrung$region))
tmp2 <- merge(tmp, hcdc_ca_loctrung, by = c("date", "region"), all.x = TRUE) %>%
  mutate(confirm = ifelse(is.na(confirm), 0, confirm))

epinow_qh_est <- regional_epinow(
  reported_cases = tmp2,
  generation_time = generation_time,
  delays = delay_opts(incubation_period, reporting_delay),
  return_output = TRUE,
  verbose = TRUE,
  rt = NULL, backcalc = backcalc_opts(),
  obs = obs_opts(scale = list(mean = 0.4, sd = 0.05)),
  horizon = 0,
  stan = stan_opts(samples = 2000, chains = 4, cores = 4)
)

epinow_qh_sum <- epinow_qh_est$summary$summarised_results$table
epinow_qh_est_sum <- do.call(rbind,
                             lapply(1:length(epinow_qh_est$regional), function(i){
                               subset(epinow_qh_est$regional[[i]]$estimates$summarised, variable %in% c("R", "reported_cases", "growth_rate")) %>%
                                 mutate(region = names(epinow_qh_est$regional[i]))
                             }))
save(epinow_qh_est, epinow_qh_sum, epinow_qh_est_sum, file = file.path("data", "HCDC", "database", "derived", paste0("epinow_qh_", timestamp, ".Rdata")))

# plot --------------------------------------------------------------------

timestamp <- "20211101055217"
date_start <- ymd("2021-04-27") # ngay bat dau
datetime_update <- ymd("2021-10-28") # ngay ket thuc

## load Rt
load(file.path(data_path, "derived", paste0("epinow_pcr_", timestamp, ".Rdata")))
load(file.path(data_path, "derived", paste0("epinow_f0_", timestamp, ".Rdata")))
load(file.path(data_path, "derived", paste0("epinow_death_", timestamp, ".Rdata")))
load(file.path(data_path, "derived", paste0("epinow_hos_", timestamp, ".Rdata")))
load(file.path(data_path, "derived", paste0("epinow_refer_", timestamp, ".Rdata")))

estimate <- rbind(
  epinow_pcr_est_sum %>%
    filter(variable %in% c("R", "infections") & type %in% c("estimate", "estimate based on partial data")) %>%
    dplyr::select(date, variable, median, lower_90, upper_90) %>%
    mutate(data = "pcr"),
  epinow_f0_est_sum %>%
    filter(variable %in% c("R", "infections") & type %in% c("estimate", "estimate based on partial data")) %>%
    dplyr::select(date, variable, median, lower_90, upper_90) %>%
    mutate(data = "f0"),
  epinow_hos_est_sum %>%
    filter(variable %in% c("R", "infections") & type %in% c("estimate", "estimate based on partial data")) %>%
    dplyr::select(date, variable, median, lower_90, upper_90) %>%
    mutate(data = "hos"),
  epinow_death_est_sum %>%
    filter(variable %in% c("R", "infections") & type %in% c("estimate", "estimate based on partial data")) %>%
    dplyr::select(date, variable, median, lower_90, upper_90) %>%
    mutate(data = "death"),
  epinow_refer_est_sum %>%
    filter(variable %in% c("R", "infections") & type %in% c("estimate", "estimate based on partial data")) %>%
    dplyr::select(date, variable, median, lower_90, upper_90) %>%
    mutate(data = "refer")
)

### calculate Rt
covid <- readRDS(file.path(data_path, "save", "covid.rds")) %>%
  filter(date >= date_start & date <= datetime_update)

### plot f0

ggplot(data = covid, aes(x = date)) +
  geom_line(aes(y = hcdc_n_capcr)) +
  geom_line(aes(y = hcdc_n_cartest), col = "grey") +
  geom_line(aes(y = hcdc_n_ca), col = "red") +
  scale_x_date(breaks = "1 week") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90))

ggplot(data = covid, aes(x = date)) +
  geom_line(aes(y = doh_n_tuvong)) +
  geom_line(aes(y = doh_n_chuyenden), col = "grey") +
  scale_x_date(breaks = "1 week") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90))

plotdat <- covid %>%
  select(date, hcdc_n_capcr, hcdc_n_ca, doh_n_tuvong, doh_n_chuyenden, doh_n_chuyenlen) %>%
  pivot_longer(cols = -date,
               names_to = "indicator",
               values_to = "value") %>%
  mutate(indicator = )

ggplot(data = plotdat, aes(x = date, y = value)) +
  geom_line() +
  scale_x_date(breaks = "1 week") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90)) +
  facet_grid(indicator ~ ., scales = "free_y")

library(ggplot2)
library(scales)
library(patchwork)

p1 <- ggplot(data = covid, aes(x = date)) +
  annotate(geom = "rect",
           xmin = date_start, xmax = date_parse("2021-07-07"),
           ymin = -Inf, ymax = Inf, fill = "grey90") +
  annotate(geom = "rect",
           xmin = date_parse("2021-08-23"), xmax = date_parse("2021-09-30"),
           ymin = -Inf, ymax = Inf, fill = "grey90") +
  geom_bar(aes(y = hcdc_n_ca), stat = "identity", fill = "#9ecae1", width = 1) +
  geom_line(data = subset(estimate, data == "f0" & variable == "infections"), aes(y = median * (15000/33449)), size = 0.5, col = "#31a354", alpha = 0.5) +
  geom_ribbon(data = subset(estimate, data == "f0" & variable == "infections"), aes(x = date, ymin = lower_90 * (14773/33449), ymax = upper_90 * (14773/33449)), fill = "#a1d99b", alpha = 0.5) +
  geom_line(data = subset(estimate, data == "f0" & variable == "R"), aes(y = median * (15000/3)), size = 0.5, col = "#de2d26", alpha = 0.5) +
  geom_ribbon(data = subset(estimate, data == "f0" & variable == "R"), aes(x = date, ymin = lower_90 * (15000/3), ymax = upper_90 * (14773/3)), fill = "#fc9272", alpha = 0.5) +
  geom_hline(yintercept = 1 * (15000/3), size = 1, linetype = 2) +
  scale_y_continuous(name = "Số F0 theo ngày xét nghiệm", labels = comma,
                     breaks = seq(from = 0, to = 15000, by = 2000),
                     sec.axis = sec_axis(~ . /(15000/3),
                                         name = "Hệ số lây nhiễm Rt", labels = comma,
                                         breaks = seq(from = 0, to = 3, by = 0.5))) +
  scale_x_date(name = "Ngày", breaks = "1 week") +
  theme_bw() +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 90),
        #plot.margin = unit(c(1, 4, 1, 1), "lines"),
        panel.grid = element_blank(),
        axis.text.y.right = element_text(color = "#de2d26"),
        axis.title.y.right = element_text(color = "#de2d26")) +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank())

p2 <- ggplot(data = covid, aes(x = date)) +
  annotate(geom = "rect",
           xmin = date_start, xmax = date_parse("2021-07-07"),
           ymin = -Inf, ymax = Inf, fill = "grey90") +
  annotate(geom = "rect",
           xmin = date_parse("2021-08-23"), xmax = date_parse("2021-09-30"),
           ymin = -Inf, ymax = Inf, fill = "grey90") +
  geom_bar(aes(y = doh_n_chuyenden), stat = "identity", fill = "#9ecae1", width = 1) +
  geom_line(data = subset(estimate, data == "hos" & variable == "infections"), aes(y = median * (4500/12000)), size = 0.5, col = "#31a354", alpha = 0.5) +
  geom_ribbon(data = subset(estimate, data == "hos" & variable == "infections"), aes(x = date, ymin = lower_90 * (4500/12000), ymax = upper_90 * (14773/33449)), fill = "#a1d99b", alpha = 0.5) +
  geom_line(data = subset(estimate, data == "hos" & variable == "R"), aes(y = median * (4500/3)), size = 0.5, col = "#de2d26", alpha = 0.5) +
  geom_ribbon(data = subset(estimate, data == "hos" & variable == "R"), aes(x = date, ymin = lower_90 * (4500/3), ymax = upper_90 * (4500/3)), fill = "#fc9272", alpha = 0.5) +
  geom_hline(yintercept = 1 * (4500/3), size = 1, linetype = 2) +
  scale_y_continuous(name = "Số F0 nhập viện tầng 2-3", labels = comma,
                     breaks = seq(from = 0, to = 4500, by = 500),
                     sec.axis = sec_axis(~ . /(4500/3),
                                         name = "Hệ số lây nhiễm Rt", labels = comma,
                                         breaks = seq(from = 0, to = 3, by = 0.5))) +
  scale_x_date(name = "Ngày", breaks = "1 week") +
  theme_bw() +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 90),
        #plot.margin = unit(c(1, 4, 1, 1), "lines"),
        panel.grid = element_blank(),
        axis.text.y.right = element_text(color = "#de2d26"),
        axis.title.y.right = element_text(color = "#de2d26")) +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank())

p3 <- ggplot(data = covid, aes(x = date)) +
  annotate(geom = "rect",
           xmin = date_start, xmax = date_parse("2021-07-07"),
           ymin = -Inf, ymax = Inf, fill = "grey90") +
  annotate(geom = "rect",
           xmin = date_parse("2021-08-23"), xmax = date_parse("2021-09-30"),
           ymin = -Inf, ymax = Inf, fill = "grey90") +
  geom_bar(aes(y = doh_n_chuyenlen), stat = "identity", fill = "#9ecae1", width = 1) +
  geom_line(data = subset(estimate, data == "refer" & variable == "infections"), aes(y = median * (300/700)), size = 0.5, col = "#31a354", alpha = 0.5) +
  geom_ribbon(data = subset(estimate, data == "refer" & variable == "infections"), aes(x = date, ymin = lower_90 * (300/700), ymax = upper_90 * (300/700)), fill = "#a1d99b", alpha = 0.5) +
  geom_line(data = subset(estimate, data == "refer" & variable == "R"), aes(y = median * (300/2)), size = 0.5, col = "#de2d26", alpha = 0.5) +
  geom_ribbon(data = subset(estimate, data == "refer" & variable == "R"), aes(x = date, ymin = lower_90 * (300/2), ymax = upper_90 * (300/2)), fill = "#fc9272", alpha = 0.5) +
  geom_hline(yintercept = 1 * (300/2), size = 1, linetype = 2) +
  scale_y_continuous(name = "Số F0 chuyển tầng trên", labels = comma,
                     breaks = seq(from = 0, to = 300, by = 50),
                     sec.axis = sec_axis(~ . /(300/2),
                                         name = "Hệ số lây nhiễm Rt", labels = comma,
                                         breaks = seq(from = 0, to = 2, by = 0.5))) +
  scale_x_date(name = "Ngày", breaks = "1 week") +
  theme_bw() +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 90),
        #plot.margin = unit(c(1, 4, 1, 1), "lines"),
        panel.grid = element_blank(),
        axis.text.y.right = element_text(color = "#de2d26"),
        axis.title.y.right = element_text(color = "#de2d26")) +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank())

p4 <- ggplot(data = covid, aes(x = date)) +
  annotate(geom = "rect",
           xmin = date_start, xmax = date_parse("2021-07-07"),
           ymin = -Inf, ymax = Inf, fill = "grey90") +
  annotate(geom = "rect",
           xmin = date_parse("2021-08-23"), xmax = date_parse("2021-09-30"),
           ymin = -Inf, ymax = Inf, fill = "grey90") +
  geom_bar(aes(y = doh_n_tuvong), stat = "identity", fill = "#9ecae1", width = 1) +
  geom_line(data = subset(estimate, data == "death" & variable == "infections"), aes(y = median * (350/1000)), size = 0.5, col = "#31a354", alpha = 0.5) +
  geom_ribbon(data = subset(estimate, data == "death" & variable == "infections"), aes(x = date, ymin = lower_90 * (350/1000), ymax = upper_90 * (350/1000)), fill = "#a1d99b", alpha = 0.5) +
  geom_line(data = subset(estimate, data == "death" & variable == "R"), aes(y = median * (350/2.5)), size = 0.5, col = "#de2d26", alpha = 0.5) +
  geom_ribbon(data = subset(estimate, data == "death" & variable == "R"), aes(x = date, ymin = lower_90 * (350/2.5), ymax = upper_90 * (350/2.5)), fill = "#fc9272", alpha = 0.5) +
  geom_hline(yintercept = 1 * (350/2), size = 1, linetype = 2) +
  scale_y_continuous(name = "Số F0 tử vong", labels = comma,
                     breaks = seq(from = 0, to = 350, by = 50),
                     sec.axis = sec_axis(~ . /(350/2.5),
                                         name = "Hệ số lây nhiễm Rt", labels = comma,
                                         breaks = seq(from = 0, to = 2.5, by = 0.5))) +
  scale_x_date(name = "Ngày", breaks = "1 week") +
  theme_bw() +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 90),
        #plot.margin = unit(c(1, 4, 1, 1), "lines"),
        panel.grid = element_blank(),
        axis.text.y.right = element_text(color = "#de2d26"),
        axis.title.y.right = element_text(color = "#de2d26"))

p1 + p2 + p3 + p4 + plot_layout(ncol = 1, heights = c(1, 1, 1, 1))
ggsave(filename = file.path("figures", "DichHCMC.png"), width = 10, height = 10)

# by district ------------------------------------------------------------------

ggplot(data = tmp2, aes(x = date)) +
  geom_bar(aes(y = confirm), stat = "identity", fill = "#9ecae1", width = 1) +
  cale_x_date(name = "Ngày", breaks = "1 week") +
  theme_bw() +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 90),
        #plot.margin = unit(c(1, 4, 1, 1), "lines"),
        panel.grid = element_blank(),
        axis.text.y.right = element_text(color = "#de2d26"),
        axis.title.y.right = element_text(color = "#de2d26"))


