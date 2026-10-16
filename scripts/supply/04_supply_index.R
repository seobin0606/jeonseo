
# ==================================================
# 04. 시군구별 자가용 수소승용차 충전 인프라 공급 분석
# 기준: 2024년 KTDB 252존체계
# ==================================================

source("scripts/supply/00_packages.R")

# --------------------------------------------------
# STEP 1. 파일 경로 설정
# --------------------------------------------------

station_path <- "data/processed/supply/station_verified.csv"
zone_path <- "data/raw/zone_code.xlsx"

region_output <- "data/processed/region_master.csv"
station_output <- "data/processed/supply/station_region.csv"
unmatched_output <- "results/supply/unmatched_region.csv"
supply_output <- "results/supply/supply_index.csv"

dir.create("data/processed", recursive = TRUE,
           showWarnings = FALSE)

dir.create("data/processed/supply", recursive = TRUE,
           showWarnings = FALSE)

dir.create("results/supply", recursive = TRUE,
           showWarnings = FALSE)

# --------------------------------------------------
# STEP 2. 입력 파일 확인
# --------------------------------------------------

if (!file.exists(station_path)) {
  stop("station_verified.csv 파일이 없습니다.")
}

if (!file.exists(zone_path)) {
  stop(
    "zone_code.xlsx 파일이 없습니다.\n",
    "data/raw/zone_code.xlsx에 저장하세요."
  )
}


# --------------------------------------------------
# STEP 3. 2024년 KTDB 시군구 기준표 생성
# --------------------------------------------------

zone_raw <- read_excel(
  zone_path,
  sheet = "전국지역간",
  skip = 2,
  col_names = FALSE
)

# 엑셀 열 위치를 기준으로 데이터 추출
region_master <- tibble::tibble(
  sido = as.character(zone_raw[[1]]),
  sigungu = as.character(zone_raw[[2]]),
  region_code = as.character(zone_raw[[10]])
) %>%
  filter(
    !is.na(sido),
    !is.na(sigungu),
    !is.na(region_code)
  ) %>%
  mutate(
    region_name = paste(sido, sigungu),
    region_code = str_trim(region_code)
  ) %>%
  distinct(region_code, .keep_all = TRUE) %>%
  arrange(as.integer(region_code))

# 지역 개수 확인
cat("시군구 개수:", nrow(region_master), "개\n")

if (nrow(region_master) != 252) {
  stop(
    "252개 지역이 아닙니다. 현재: ",
    nrow(region_master), "개"
  )
}

# 저장
write_excel_csv(
  region_master,
  region_output
)

cat("시군구 기준표 저장 완료\n")



# --------------------------------------------------
# STEP 4. 충전소 데이터 불러오기
# --------------------------------------------------

station <- read_csv(
  station_path,
  show_col_types = FALSE,
  col_types = cols(
    station_id = col_character(),
    region_code = col_character(),
    .default = col_guess()
  )
)

if (anyDuplicated(station$station_id)) {
  stop("station_id가 중복되어 있습니다.")
}

# --------------------------------------------------
# STEP 5. 주소 표기 통일
# --------------------------------------------------

normalize_address <- function(x) {
  
  x <- coalesce(as.character(x), "")
  
  x <- str_squish(x)
  
  # 시도 약칭 및 변경 명칭 통일
  x <- str_replace(x, "^서울( |$)", "서울특별시 ")
  x <- str_replace(x, "^부산( |$)", "부산광역시 ")
  x <- str_replace(x, "^대구( |$)", "대구광역시 ")
  x <- str_replace(x, "^인천( |$)", "인천광역시 ")
  x <- str_replace(x, "^광주( |$)", "광주광역시 ")
  x <- str_replace(x, "^대전( |$)", "대전광역시 ")
  x <- str_replace(x, "^울산( |$)", "울산광역시 ")
  
  x <- str_replace(x, "^경기( |$)", "경기도 ")
  x <- str_replace(x, "^강원( |$)", "강원특별자치도 ")
  x <- str_replace(x, "^강원도( |$)", "강원특별자치도 ")
  
  x <- str_replace(x, "^충북( |$)", "충청북도 ")
  x <- str_replace(x, "^충남( |$)", "충청남도 ")
  x <- str_replace(x, "^전북( |$)", "전북특별자치도 ")
  x <- str_replace(x, "^전라북도( |$)", "전북특별자치도 ")
  x <- str_replace(x, "^전남( |$)", "전라남도 ")
  x <- str_replace(x, "^경북( |$)", "경상북도 ")
  x <- str_replace(x, "^경남( |$)", "경상남도 ")
  
  x <- str_replace(x, "^제주( |$)", "제주특별자치도 ")
  x <- str_replace(x, "^제주도( |$)", "제주특별자치도 ")
  x <- str_replace(x, "^세종( |$)", "세종특별자치시 ")
  
  # 세종시 주소의 특수 표기 처리
  x <- str_replace(
    x,
    "^세종특별자치시 세종시( |$)",
    "세종특별자치시 "
  )
  
  str_squish(x)
}

station <- station %>%
  mutate(
    address_clean = normalize_address(address)
  )

# --------------------------------------------------
# STEP 5-1. 주소 표기 오류 보정
# --------------------------------------------------

station <- station %>%
  mutate(
    address_clean = case_when(
      station_id %in% c("8", "14") ~
        str_replace(
          address_clean,
          "^경상남도 창원 ",
          "경상남도 창원시 "
        ),
      
      station_id %in% c("16", "17") ~
        str_replace(
          address_clean,
          "^경기도 안성 ",
          "경기도 안성시 "
        ),
      
      station_id == "19" ~
        str_replace(
          address_clean,
          "^경기도 여주 ",
          "경기도 여주시 "
        ),
      
      station_id == "119" ~
        str_replace(
          address_clean,
          "^광구 ",
          "광주광역시 "
        ),
      
      TRUE ~ address_clean
    )
  )


# --------------------------------------------------
# STEP 6. 시군구 코드 매칭
# --------------------------------------------------

# 시도와 시군구 이름을 주소와 비교
# 이름이 긴 지역부터 확인하여 구 단위 매칭 우선

region_lookup <- region_master %>%
  mutate(
    match_prefix = if_else(
      sido == "세종특별자치시",
      "세종특별자치시",
      paste(sido, sigungu)
    )
  ) %>%
  arrange(desc(nchar(match_prefix)))

# 주소가 해당 시군구 이름으로 시작하는지 확인
match_region <- function(addr) {
  
  if (is.na(addr) || addr == "") {
    return(NA_character_)
  }
  
  matched <- which(
    startsWith(addr, region_lookup$match_prefix)
  )
  
  if (length(matched) == 0) {
    return(NA_character_)
  }
  
  region_lookup$region_code[matched[1]]
}

station_region <- station %>%
  select(-any_of(c("region_code", "region_name"))) %>%
  mutate(
    region_code = vapply(
      address_clean,
      match_region,
      character(1)
    )
  ) %>%
  left_join(
    region_master %>%
      select(region_code, region_name),
    by = "region_code"
  )

# --------------------------------------------------
# STEP 7. 시군구 매칭 결과 확인
# --------------------------------------------------

cat("\n===== 시군구 매칭 결과 =====\n")

cat("전체 충전소:", nrow(station_region), "개\n")

cat(
  "매칭 성공:",
  sum(!is.na(station_region$region_code)),
  "개\n"
)

cat(
  "매칭 실패:",
  sum(is.na(station_region$region_code)),
  "개\n"
)

unmatched_region <- station_region %>%
  filter(is.na(region_code)) %>%
  select(
    station_id,
    station_name,
    address,
    address_clean,
    passenger_status,
    charger_count
  )

write_excel_csv(
  unmatched_region,
  unmatched_output
)

write_excel_csv(
  station_region,
  station_output
)

# --------------------------------------------------
# STEP 8. 시군구별 공급량 집계
# --------------------------------------------------

# 가능_후보: 승용차 충전 가능 잠정 분류
# 불가능_후보: 승용차 충전 불가능 잠정 분류
# 미확인: 승용차 충전 가능 여부 확인되지 않음

station_region <- station_region %>%
  mutate(
    charger_count = as.numeric(charger_count),
    passenger_status = coalesce(
      passenger_status,
      "미확인"
    )
  )

# 충전기 수 결측은 임의로 0으로 바꾸지 않음
# 결측이 있는 지역은 별도 확인

supply_count <- station_region %>%
  filter(!is.na(region_code)) %>%
  group_by(region_code) %>%
  summarise(
    total_station_count = n(),
    
    passenger_station_count = sum(
      passenger_status == "가능_후보"
    ),
    
    passenger_charger_count = sum(
      if_else(
        passenger_status == "가능_후보",
        charger_count,
        0
      ),
      na.rm = TRUE
    ),
    
    unknown_station_count = sum(
      passenger_status == "미확인"
    ),
    
    unavailable_station_count = sum(
      passenger_status == "불가능_후보"
    ),
    
    unknown_charger_count = sum(
      if_else(
        passenger_status == "미확인",
        charger_count,
        0
      ),
      na.rm = TRUE
    ),
    
    missing_charger_count = sum(
      passenger_status == "가능_후보" &
        is.na(charger_count)
    ),
    
    .groups = "drop"
  )

# --------------------------------------------------
# STEP 9. 충전소 없는 시군구도 0으로 포함
# --------------------------------------------------

supply <- region_master %>%
  select(region_code, region_name) %>%
  left_join(
    supply_count,
    by = "region_code"
  ) %>%
  mutate(
    across(
      c(
        total_station_count,
        passenger_station_count,
        passenger_charger_count,
        unknown_station_count,
        unavailable_station_count,
        unknown_charger_count,
        missing_charger_count
      ),
      ~ replace_na(.x, 0)
    )
  )

# --------------------------------------------------
# STEP 10. 공급 지수 정규화
# --------------------------------------------------

minmax_100 <- function(x) {
  
  if (all(is.na(x))) {
    return(rep(NA_real_, length(x)))
  }
  
  x_min <- min(x, na.rm = TRUE)
  x_max <- max(x, na.rm = TRUE)
  
  if (x_max == x_min) {
    return(rep(0, length(x)))
  }
  
  100 * (x - x_min) / (x_max - x_min)
}

supply <- supply %>%
  mutate(
    station_index = minmax_100(
      passenger_station_count
    ),
    
    charger_index = minmax_100(
      passenger_charger_count
    ),
    
    # 충전소 수와 충전기 수를 동일 가중치로 반영
    supply_index = round(
      0.5 * station_index +
        0.5 * charger_index,
      2
    )
  ) %>%
  arrange(as.integer(region_code))

# --------------------------------------------------
# STEP 11. 결과 검증
# --------------------------------------------------

if (nrow(supply) != 252) {
  stop("최종 공급 데이터가 252개 지역이 아닙니다.")
}

if (anyDuplicated(supply$region_code)) {
  stop("최종 공급 데이터에 중복 지역 코드가 있습니다.")
}

cat("\n===== 공급 분석 결과 =====\n")

cat("분석 지역:", nrow(supply), "개\n")

cat(
  "승용차 충전 가능 후보:",
  sum(supply$passenger_station_count),
  "개\n"
)

cat(
  "승용차 충전 가능 후보 충전기:",
  sum(supply$passenger_charger_count),
  "개\n"
)

cat(
  "미확인 충전소:",
  sum(supply$unknown_station_count),
  "개\n"
)

cat(
  "충전소 없는 지역:",
  sum(supply$total_station_count == 0),
  "개\n"
)

cat(
  "승용차 충전 가능 후보가 없는 지역:",
  sum(supply$passenger_station_count == 0),
  "개\n"
)

# 미매칭 지역 또는 충전기 결측이 있으면 경고
if (nrow(unmatched_region) > 0) {
  warning(
    "시군구 미매칭 충전소가 있습니다. ",
    "unmatched_region.csv를 확인하세요."
  )
}

if (sum(supply$missing_charger_count) > 0) {
  warning(
    "승용차 충전 가능 후보 중 충전기 수 결측이 있습니다."
  )
}

# --------------------------------------------------
# STEP 12. 최종 공급 지수 저장
# --------------------------------------------------

write_excel_csv(
  supply,
  supply_output
)

cat("\n저장 완료:", supply_output, "\n")

# --------------------------------------------------
# STEP 13. 공급 지수 상위 10개 지역 확인
# --------------------------------------------------

cat("\n===== 공급 지수 상위 10개 지역 =====\n")

supply %>%
  arrange(desc(supply_index)) %>%
  select(
    region_code,
    region_name,
    passenger_station_count,
    passenger_charger_count,
    supply_index
  ) %>%
  slice_head(n = 10) %>%
  print(n = 10)

