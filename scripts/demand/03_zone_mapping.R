
# ============================================================
# 수소충전소 최적 입지 선정
# 03. KTDB 교통존 및 행정구역 매핑
#
# 입력:
# data/raw/zone_code.xlsx
# data/processed/car_registration_preliminary.csv
# data/processed/traffic_origin_2024_preliminary.csv
#
# 출력:
# data/processed/car_registration_final.csv
# data/processed/traffic_origin_2024_final.csv
#
# 자료 기준:
# 자동차 등록대수: 2026년 8월
# KTDB 승용차 발생통행량: 2024년
# ============================================================

# 1. 패키지 불러오기
library(tidyverse)
library(readxl)

# 2. 입력 파일 확인
stopifnot(
  file.exists("data/raw/zone_code.xlsx"),
  file.exists("data/processed/car_registration_preliminary.csv"),
  file.exists("data/processed/traffic_origin_2024_preliminary.csv")
)

# ============================================================
# 3. KTDB 교통존 코드표 전처리
# ============================================================

zone_raw <- read_excel(
  "data/raw/zone_code.xlsx",
  sheet = "전국지역간",
  col_names = FALSE
)

zone_clean <- zone_raw %>%
  slice(-(1:2)) %>%
  transmute(
    교통존코드 = as.integer(...10),
    시도 = as.character(...1),
    시군구 = as.character(...2)
  )

# 코드표 검증
stopifnot(
  nrow(zone_clean) == 252,
  !anyNA(zone_clean$교통존코드),
  !anyNA(zone_clean$시도),
  !anyNA(zone_clean$시군구),
  !anyDuplicated(zone_clean$교통존코드)
)

# ============================================================
# 4. KTDB 발생통행량과 교통존 코드 연결
# ============================================================

traffic_origin_2024 <- read_csv(
  "data/processed/traffic_origin_2024_preliminary.csv",
  show_col_types = FALSE
) %>%
  mutate(
    출발시군구 = as.integer(출발시군구)
  )

traffic_region_2024 <- traffic_origin_2024 %>%
  left_join(
    zone_clean,
    by = c("출발시군구" = "교통존코드"),
    relationship = "many-to-one"
  )

# 매핑 누락 및 통행량 결측치 검증
stopifnot(
  !anyNA(traffic_region_2024$시도),
  !anyNA(traffic_region_2024$시군구),
  !anyNA(traffic_region_2024$승용차_발생통행량)
)

# ============================================================
# 5. 자동차 등록대수의 시도명 표준화
# ============================================================

car_final <- read_csv(
  "data/processed/car_registration_preliminary.csv",
  show_col_types = FALSE
) %>%
  mutate(
    시도 = case_when(
      시도 == "전남광주" &
        시군구 %in% c("동구", "서구", "남구", "북구", "광산구") ~ "광주",
      시도 == "전남광주" ~ "전남",
      TRUE ~ 시도
    ),
    시도 = recode(
      시도,
      "서울" = "서울특별시",
      "부산" = "부산광역시",
      "대구" = "대구광역시",
      "인천" = "인천광역시",
      "광주" = "광주광역시",
      "대전" = "대전광역시",
      "울산" = "울산광역시",
      "세종" = "세종특별자치시",
      "경기" = "경기도",
      "강원" = "강원특별자치도",
      "충북" = "충청북도",
      "충남" = "충청남도",
      "전북" = "전북특별자치도",
      "전남" = "전라남도",
      "경북" = "경상북도",
      "경남" = "경상남도",
      "제주" = "제주특별자치도"
    )
  )

stopifnot(
  !anyNA(car_final$시도),
  !anyNA(car_final$자가용_승용차),
  !any(car_final$시도 == "전남광주")
)

# ============================================================
# 6. KTDB 통행량의 행정구역 통합
# ============================================================

# 일반구가 있는 도시를 시 단위로 통합
traffic_standard <- traffic_region_2024 %>%
  mutate(
    시군구_통합 = case_when(
      str_detect(시군구, "시 ") ~
        str_extract(시군구, "^[^ ]+시"),
      시도 == "세종특별자치시" ~ "세종특별자치시",
      TRUE ~ 시군구
    )
  )

traffic_final <- traffic_standard %>%
  group_by(시도, 시군구_통합) %>%
  summarise(
    승용차_발생통행량 = sum(승용차_발생통행량),
    .groups = "drop"
  ) %>%
  rename(시군구 = 시군구_통합)

# 통합 전후 통행량 총합 검증
stopifnot(
  isTRUE(all.equal(
    sum(traffic_region_2024$승용차_발생통행량),
    sum(traffic_final$승용차_발생통행량)
  ))
)

# ============================================================
# 7. 자동차 등록대수 행정구역 보정
# ============================================================

# 인천광역시의 일부 개편 지역을 기존 분석 단위로 통합
# 실제 행정구역 경계와 완전히 일치한다는 의미는 아님
car_adjusted <- car_final %>%
  mutate(
    시군구 = case_when(
      시도 == "인천광역시" &
        시군구 %in% c("검단구", "서해구") ~ "서구",
      시도 == "인천광역시" &
        시군구 == "영종구" ~ "중구",
      TRUE ~ 시군구
    )
  ) %>%
  group_by(기준월, 시도, 시군구) %>%
  summarise(
    자가용_승용차 = sum(자가용_승용차),
    .groups = "drop"
  )

# 군위군 및 청원군 행정구역 보정
car_adjusted2 <- car_adjusted %>%
  mutate(
    시도 = case_when(
      시도 == "경상북도" &
        시군구 == "군위군" ~ "대구광역시",
      TRUE ~ 시도
    ),
    시군구 = case_when(
      시도 == "충청북도" &
        시군구 == "청원군" ~ "청주시",
      TRUE ~ 시군구
    )
  ) %>%
  group_by(기준월, 시도, 시군구) %>%
  summarise(
    자가용_승용차 = sum(자가용_승용차),
    .groups = "drop"
  )

# ============================================================
# 8. 인천광역시 통합권역 설정
# ============================================================

# 중구·동구 통합권역은 데이터 결합을 위한 분석용 지역
# 실제 행정구역 또는 제물포구 경계와 동일하지 않음

car_final_ready <- car_adjusted2 %>%
  mutate(
    시군구 = case_when(
      시도 == "인천광역시" &
        시군구 %in% c("중구", "동구", "제물포구") ~
        "중구·동구 통합권역",
      TRUE ~ 시군구
    )
  ) %>%
  group_by(기준월, 시도, 시군구) %>%
  summarise(
    자가용_승용차 = sum(자가용_승용차),
    .groups = "drop"
  )

traffic_final_ready <- traffic_final %>%
  mutate(
    시군구 = case_when(
      시도 == "인천광역시" &
        시군구 %in% c("중구", "동구") ~
        "중구·동구 통합권역",
      TRUE ~ 시군구
    )
  ) %>%
  group_by(시도, 시군구) %>%
  summarise(
    승용차_발생통행량 = sum(승용차_발생통행량),
    .groups = "drop"
  )

# ============================================================
# 9. 최종 데이터 검증
# ============================================================

# 행정구역 통합 전후 총합 확인
stopifnot(
  isTRUE(all.equal(
    sum(car_final$자가용_승용차),
    sum(car_final_ready$자가용_승용차)
  )),
  isTRUE(all.equal(
    sum(traffic_final$승용차_발생통행량),
    sum(traffic_final_ready$승용차_발생통행량)
  ))
)

# 최종 분석지역 수 확인
stopifnot(
  nrow(car_final_ready) == 228,
  nrow(traffic_final_ready) == 228
)

# 중복 지역 확인
stopifnot(
  !anyDuplicated(
    car_final_ready[c("기준월", "시도", "시군구")]
  ),
  !anyDuplicated(
    traffic_final_ready[c("시도", "시군구")]
  )
)

# 자동차 등록대수와 KTDB 지역 일치 여부 확인
stopifnot(
  nrow(anti_join(
    car_final_ready,
    traffic_final_ready,
    by = c("시도", "시군구")
  )) == 0,
  nrow(anti_join(
    traffic_final_ready,
    car_final_ready,
    by = c("시도", "시군구")
  )) == 0
)

# ============================================================
# 10. 최종 데이터 저장
# ============================================================

dir.create(
  "data/processed",
  recursive = TRUE,
  showWarnings = FALSE
)

write_csv(
  car_final_ready,
  "data/processed/demand/car_registration_final.csv"
)

write_csv(
  traffic_final_ready,
  "data/processed/demand/traffic_origin_2024_final.csv"
)

cat("교통존 및 행정구역 매핑 완료\n")
cat("자동차 등록대수 지역 수:", nrow(car_final_ready), "\n")
cat("KTDB 발생통행량 지역 수:", nrow(traffic_final_ready), "\n")










