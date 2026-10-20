
# ============================================================
# 수소충전소 최적 입지 선정
# 04. 시군구별 수소승용차 충전 인프라 공급지수 산출
#
# 목적:
# 전국 시군구별 수소승용차 충전 인프라의 공급 수준을
# 정량화하여 잠재수요와 비교하기 위한 공급지수 생성
#
# 분석 대상:
# 2026년 상업용 수소충전소 중 승용차 충전 가능 후보
#
# 분석 지역:
# 2024년 KTDB 252개 교통존 기준
#
# 공급지수 산정:
# 승용차 충전 가능 후보 충전소 수 50%
# 승용차 충전 가능 후보 충전기 수 50%
# 각각 0~100 Min-Max 정규화 후 결합
#
# 입력:
# data/processed/supply/station_verified.csv
# data/raw/zone_code.xlsx
#
# 출력:
# data/processed/region_master.csv
# data/processed/supply/station_region.csv
# results/supply/unmatched_region.csv
# results/supply/supply_index.csv
#
# 주의:
# '가능_후보'는 최종 검증된 충전 가능 상태가 아님
# '미확인' 충전소는 기본 공급지수에서 제외하고
# 이후 민감도 분석에서 별도로 검토
# ============================================================

# 패키지 불러오기
source("scripts/supply/00_packages.R")

# ============================================================
# STEP 1. 파일 경로 설정
# ============================================================

station_path <- "data/processed/supply/station_verified.csv"
zone_path <- "data/raw/zone_code.xlsx"

region_output <- "data/processed/region_master.csv"
station_output <- "data/processed/supply/station_region.csv"
unmatched_output <- "results/supply/unmatched_region.csv"
supply_output <- "results/supply/supply_index.csv"

# 결과 저장 폴더 생성
dir.create(
  "data/processed",
  recursive = TRUE,
  showWarnings = FALSE
)

dir.create(
  "data/processed/supply",
  recursive = TRUE,
  showWarnings = FALSE
)

dir.create(
  "results/supply",
  recursive = TRUE,
  showWarnings = FALSE
)

# ============================================================
# STEP 2. 입력 파일 확인
# ============================================================

# 충전소 검증 데이터 존재 여부 확인
if (!file.exists(station_path)) {
  stop("station_verified.csv 파일이 없습니다.")
}

# KTDB 교통존 코드표 존재 여부 확인
if (!file.exists(zone_path)) {
  stop(
    "zone_code.xlsx 파일이 없습니다.\n",
    "data/raw/zone_code.xlsx에 저장하세요."
  )
}

# ============================================================
# STEP 3. 2024년 KTDB 시군구 기준표 생성
# ============================================================

# KTDB 전국지역간 시트에서 상단 설명 2행 제외
zone_raw <- read_excel(
  zone_path,
  sheet = "전국지역간",
  skip = 2,
  col_names = FALSE
)

# 시도, 시군구, 교통존 코드 추출
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

# KTDB 기준 지역 수 확인
cat("시군구 개수:", nrow(region_master), "개\n")

if (nrow(region_master) != 252) {
  stop(
    "252개 지역이 아닙니다. 현재: ",
    nrow(region_master), "개"
  )
}

# 지역 기준표 저장
write_excel_csv(
  region_master,
  region_output
)

cat("시군구 기준표 저장 완료\n")

# ============================================================
# STEP 4. 충전소 검증 데이터 불러오기
# ============================================================

# 03_station_verification.R에서 생성한 파일 사용
# 상업용 충전소와 승용차 충전 가능 여부 후보 정보 포함

station <- read_csv(
  station_path,
  show_col_types = FALSE,
  col_types = cols(
    station_id = col_character(),
    region_code = col_character(),
    .default = col_guess()
  )
)

# 충전소 ID 중복 여부 확인
if (anyDuplicated(station$station_id)) {
  stop("station_id가 중복되어 있습니다.")
}

# ============================================================
# STEP 5. 충전소 주소 표기 통일
# ============================================================

# 주소에 사용된 시도 약칭 및 변경 명칭을
# KTDB 지역 기준표의 행정구역명과 일치하도록 정리

normalize_address <- function(x) {
  
  x <- coalesce(as.character(x), "")
  
  x <- str_squish(x)
  
  # 특별시 및 광역시 약칭 통일
  x <- str_replace(x, "^서울( |$)", "서울특별시 ")
  x <- str_replace(x, "^부산( |$)", "부산광역시 ")
  x <- str_replace(x, "^대구( |$)", "대구광역시 ")
  x <- str_replace(x, "^인천( |$)", "인천광역시 ")
  x <- str_replace(x, "^광주( |$)", "광주광역시 ")
  x <- str_replace(x, "^대전( |$)", "대전광역시 ")
  x <- str_replace(x, "^울산( |$)", "울산광역시 ")
  
  # 도 단위 약칭 및 변경 명칭 통일
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
  
  # 세종시 주소의 중복 표기 보정
  x <- str_replace(
    x,
    "^세종특별자치시 세종시( |$)",
    "세종특별자치시 "
  )
  
  str_squish(x)
}

# 정규화된 주소 변수 생성
station <- station %>%
  mutate(
    address_clean = normalize_address(address)
  )

# ============================================================
# STEP 5-1. 일부 충전소의 주소 표기 오류 보정
# ============================================================

# 기존 데이터에서 확인된 특정 충전소의 주소 보정
# station_id는 원본 2026년 충전소 행 순서 기준
# 원본 파일의 행 순서가 변경되면 재검토 필요

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

# ============================================================
# STEP 6. 충전소 주소와 KTDB 시군구 코드 매칭
# ============================================================

# 시도와 시군구 이름을 결합하여 주소 비교용 기준 생성
# 이름이 긴 지역을 먼저 확인하여 잘못된 부분 매칭 방지

region_lookup <- region_master %>%
  mutate(
    match_prefix = if_else(
      sido == "세종특별자치시",
      "세종특별자치시",
      paste(sido, sigungu)
    )
  ) %>%
  arrange(desc(nchar(match_prefix)))

# 주소가 특정 시군구명으로 시작하는지 확인하는 함수
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

# 충전소별 지역 코드 및 지역명 연결
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

# ============================================================
# STEP 7. 시군구 매칭 결과 확인 및 저장
# ============================================================

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

# 행정구역 매칭에 실패한 충전소 추출
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

# 매칭 실패 목록 저장
write_excel_csv(
  unmatched_region,
  unmatched_output
)

# 충전소별 행정구역 매칭 결과 저장
write_excel_csv(
  station_region,
  station_output
)

# ============================================================
# STEP 8. 시군구별 충전 인프라 공급량 집계
# ============================================================

# passenger_status 분류:
#
# 가능_후보:
#   매칭된 자료에서 승용차 충전 가능성이 확인된 후보
#   기본 공급지수 산출에 반영
#
# 불가능_후보:
#   승용차 충전이 불가능한 것으로 분류된 후보
#   기본 공급지수에서 제외
#
# 미확인:
#   승용차 충전 가능 여부를 확인하지 못한 충전소
#   기본 공급지수에서 제외하고 민감도 분석에서 검토
#
# 주의:
# 상업용 충전소 전체를 공급지수에 포함하는 것이 아님

station_region <- station_region %>%
  mutate(
    charger_count = as.numeric(charger_count),
    passenger_status = coalesce(
      passenger_status,
      "미확인"
    )
  )

# 충전기 수 결측은 실제 충전기 0개와 구분
# 합계 계산 시에는 결측값을 제외하고
# 결측이 발생한 충전소 수를 별도로 기록

supply_count <- station_region %>%
  filter(!is.na(region_code)) %>%
  group_by(region_code) %>%
  summarise(
    
    # 해당 지역의 전체 분석 대상 상업용 충전소 수
    total_station_count = n(),
    
    # 승용차 충전 가능 후보 충전소 수
    passenger_station_count = sum(
      passenger_status == "가능_후보"
    ),
    
    # 승용차 충전 가능 후보의 충전기 수 합계
    passenger_charger_count = sum(
      if_else(
        passenger_status == "가능_후보",
        charger_count,
        0
      ),
      na.rm = TRUE
    ),
    
    # 승용차 충전 가능 여부 미확인 충전소 수
    unknown_station_count = sum(
      passenger_status == "미확인"
    ),
    
    # 승용차 충전 불가능 후보 충전소 수
    unavailable_station_count = sum(
      passenger_status == "불가능_후보"
    ),
    
    # 미확인 충전소의 충전기 수 합계
    unknown_charger_count = sum(
      if_else(
        passenger_status == "미확인",
        charger_count,
        0
      ),
      na.rm = TRUE
    ),
    
    # 승용차 충전 가능 후보 중 충전기 수 결측 건수
    missing_charger_count = sum(
      passenger_status == "가능_후보" &
        is.na(charger_count)
    ),
    
    .groups = "drop"
  )

# ============================================================
# STEP 9. 충전소가 없는 지역도 분석에 포함
# ============================================================

# KTDB 252개 지역 기준표를 중심으로 결합
# 충전소가 없는 지역은 공급량을 0으로 처리
# 이를 통해 전국 지역을 동일한 기준으로 비교

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

# ============================================================
# STEP 10. 공급지수 정규화 및 산출
# ============================================================

# 지역별 충전소 수와 충전기 수는 규모가 다르므로
# 각각 0~100 범위로 Min-Max 정규화
#
# 기본 공급지수:
# 0.5 × 충전소 수 정규화 지수
# + 0.5 × 충전기 수 정규화 지수
#
# 가중치는 두 공급 요소에 동일하게 적용
# 실제 충전 가능 여부가 미확인된 시설은 반영하지 않음

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
    
    # 승용차 충전 가능 후보 충전소 수 정규화
    station_index = minmax_100(
      passenger_station_count
    ),
    
    # 승용차 충전 가능 후보 충전기 수 정규화
    charger_index = minmax_100(
      passenger_charger_count
    ),
    
    # 최종 기본 공급지수
    supply_index = round(
      0.5 * station_index +
        0.5 * charger_index,
      2
    )
  ) %>%
  arrange(as.integer(region_code))

# ============================================================
# STEP 11. 공급 분석 결과 검증
# ============================================================

# 분석 지역이 KTDB 기준 252개인지 확인
if (nrow(supply) != 252) {
  stop("최종 공급 데이터가 252개 지역이 아닙니다.")
}

# 지역 코드 중복 여부 확인
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

# 행정구역 매칭 실패 시 경고
if (nrow(unmatched_region) > 0) {
  warning(
    "시군구 미매칭 충전소가 있습니다. ",
    "unmatched_region.csv를 확인하세요."
  )
}

# 충전기 수 결측이 있으면 경고
if (sum(supply$missing_charger_count) > 0) {
  warning(
    "승용차 충전 가능 후보 중 충전기 수 결측이 있습니다."
  )
}

# ============================================================
# STEP 12. 최종 공급지수 저장
# ============================================================

# 이후 수요·공급 불균형 분석의 입력 자료로 활용
write_excel_csv(
  supply,
  supply_output
)

cat("\n저장 완료:", supply_output, "\n")

# ============================================================
# STEP 13. 공급지수 상위 10개 지역 확인
# ============================================================

# 공급지수가 높은 지역부터 정렬하여 확인
# 공급지수가 높을수록 분석 기준상 충전 인프라가
# 상대적으로 많이 공급된 지역임을 의미

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


