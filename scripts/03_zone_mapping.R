# ==========================================
# 자가용 수소승용차 미래 잠재수요 분석
# 03. 교통존-행정구역 매핑
# 입력: zone_code.xlsx 및 앞선 두 스크립트의 preliminary CSV
# 출력: car_registration_final.csv, traffic_origin_2024_final.csv
# ==========================================

library(tidyverse)
library(readxl)

# KTDB 252존 코드표의 설명 2행을 제거하고 코드·시도·시군구를 추출한다.
zone_raw <- read_excel("data/raw/zone_code.xlsx",sheet = "전국지역간",col_names = FALSE)

zone_clean <- zone_raw %>%
  slice(-(1:2)) %>%
  transmute(
    교통존코드 = as.integer(...10),
    시도 = as.character(...1),
    시군구 = as.character(...2)
  )

# 교통존 코드 중복 검증
stopifnot(
  !anyDuplicated(zone_clean$교통존코드)
)

# KTDB 승용차 발생통행량 데이터 불러오기
traffic_origin_2024 <- read_csv(
  "data/processed/traffic_origin_2024_preliminary.csv", show_col_types = FALSE)

# 교통존 코드를 기준으로 행정구역명 연결
traffic_region_2024 <- traffic_origin_2024 %>%
  left_join(zone_clean, by = c("출발시군구" = "교통존코드"))

# 행정구역 매핑 누락 검증
stopifnot(
  !anyNA(traffic_region_2024$시군구)
)



# 자동차 등록 자료의 '전남광주'를 광주광역시와 전라남도로 분리한다.

# ============================================================
# 자동차 등록대수 데이터의 행정구역 명칭 통일
# ============================================================

car_final <- read_csv(
  "data/processed/car_registration_preliminary.csv", show_col_types = FALSE
) %>%
  mutate(
    시도 = case_when(
      시도 == "전남광주" &
        시군구 %in% c("동구", "서구", "남구", "북구", "광산구") ~ "광주",
      시도 == "전남광주" ~ "전남",
      TRUE ~ 시도
    )
  ) %>%
  mutate(
    시도 = recode(
      시도,
      "서울" = "서울특별시", "부산" = "부산광역시",
      "대구" = "대구광역시", "인천" = "인천광역시",
      "광주" = "광주광역시", "대전" = "대전광역시",
      "울산" = "울산광역시", "세종" = "세종특별자치시",
      "경기" = "경기도", "강원" = "강원특별자치도",
      "충북" = "충청북도", "충남" = "충청남도",
      "전북" = "전북특별자치도", "전남" = "전라남도",
      "경북" = "경상북도", "경남" = "경상남도",
      "제주" = "제주특별자치도"
    )
  )
  
# 시도 명칭 통일 결과 검증
stopifnot(
  !anyNA(car_final$시도),
  !any(car_final$시도 == "전남광주")
)

# ============================================================
# KTDB 통행량 데이터의 행정구역 통합
# ============================================================

# 일반구가 있는 도시를 시 단위로 통합
traffic_standard <- traffic_region_2024 %>%
  mutate(
    시군구_통합 = case_when(
      str_detect(시군구, "시 ") ~ str_extract(시군구, "^[^ ]+시"),
      시도 == "세종특별자치시" ~ "세종특별자치시",
      TRUE ~ 시군구
    )
  )

# 동일한 시도·시군구별 발생통행량 합산
traffic_final <- traffic_standard %>%
  group_by(시도, 시군구_통합) %>%
  summarise(
    승용차_발생통행량 = sum(승용차_발생통행량, na.rm = TRUE),
    .groups = "drop") %>%
  rename(시군구 = 시군구_통합)

# ============================================================
# 행정구역 통합 결과 검증
# ============================================================

# 지역 통합 전후 통행량 합계 검증
stopifnot(
  isTRUE(all.equal(
    sum(traffic_region_2024$승용차_발생통행량),
    sum(traffic_final$승용차_발생통행량)
  ))
)

# 자동차 등록대수 데이터에만 있는 지역
car_only_final <- car_final %>%
  anti_join(
    traffic_final,
    by = c("시도", "시군구")
  )

# KTDB 통행량 데이터에만 있는 지역
traffic_only_final <- traffic_final %>%
  anti_join(
    car_final,
    by = c("시도", "시군구")
  )

# ============================================================
# 지역 중복 검증
# ============================================================

# 자동차 등록대수 데이터의 지역 중복 검증
stopifnot(
  !anyDuplicated(car_final[c("시도", "시군구")])
)

# KTDB 통행량 데이터의 지역 중복 검증
stopifnot(
  !anyDuplicated(traffic_final[c("시도", "시군구")])
)


# ============================================================
# 인천광역시 행정구역 명칭 보정
# ============================================================

# 자동차 등록대수 데이터의 행정구역 통합
car_adjusted <- car_final %>%
  mutate(
    시군구 = case_when(
      시도 == "인천광역시" & 시군구 %in% c("검단구", "서해구") ~ "서구",
      시도 == "인천광역시" & 시군구 == "영종구" ~ "중구",
      TRUE ~ 시군구
    )
  ) %>%
  group_by(기준월, 시도, 시군구) %>%
  summarise(
    자가용_승용차 = sum(자가용_승용차, na.rm = TRUE),.groups = "drop")

# 행정구역 통합 전후 자동차 등록대수 합계 검증
stopifnot(
  isTRUE(all.equal(
    sum(car_final$자가용_승용차, na.rm = TRUE),
    sum(car_adjusted$자가용_승용차, na.rm = TRUE)
  ))
)


# ============================================================
# 행정구역 보정 및 최종 데이터 통합
# ============================================================

# 1. 군위군 및 청원군 행정구역 보정
car_adjusted2 <- car_adjusted %>%
  mutate(
    시도 = case_when(
      시도 == "경상북도" & 시군구 == "군위군" ~ "대구광역시",
      TRUE ~ 시도
    ),
    시군구 = case_when(
      시도 == "충청북도" & 시군구 == "청원군" ~ "청주시",
      TRUE ~ 시군구
    )
   ) %>%
  group_by(기준월, 시도, 시군구) %>%
  summarise(
    자가용_승용차 = sum(자가용_승용차, na.rm = TRUE),.groups = "drop")


# 2. 인천광역시 자동차 등록 데이터 통합
# 이 단위는 실제 제물포구 경계와 같지 않으므로 입지 추천 전 재검토 필요
car_final_ready <- car_adjusted2 %>%
  mutate(
    시군구 = case_when(
      시도 == "인천광역시" &
        시군구 %in% c("중구", "동구", "제물포구") ~ "중구·동구 통합권역",
      TRUE ~ 시군구
    )
  ) %>%
  group_by(기준월, 시도, 시군구) %>%
  summarise(
    자가용_승용차 = sum(자가용_승용차, na.rm = TRUE),.groups = "drop")


# 3. 인천광역시 KTDB 통행량 데이터 통합
traffic_final_ready <- traffic_final %>%
  mutate(
    시군구 = case_when(
      시도 == "인천광역시" &
        시군구 %in% c("중구", "동구") ~ "중구·동구 통합권역",
      TRUE ~ 시군구
    )
  ) %>%
  group_by(시도, 시군구) %>%
  summarise(
    승용차_발생통행량 = sum(승용차_발생통행량, na.rm = TRUE),.groups = "drop")

# 4. 행정구역 통합 전후 데이터 합계 검증
stopifnot(
  isTRUE(all.equal(
    sum(car_adjusted$자가용_승용차),
    sum(car_final_ready$자가용_승용차)
  )),
  isTRUE(all.equal(
    sum(traffic_final$승용차_발생통행량),
    sum(traffic_final_ready$승용차_발생통행량)
  ))
)

# 5. 자동차 등록 데이터와 KTDB 지역 일치 검증
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

# 전처리 완료 데이터 저장

write_csv(car_final_ready,"data/processed/car_registration_final.csv")
write_csv(traffic_final_ready,"data/processed/traffic_origin_2024_final.csv")
















