library(tidyverse)
library(readxl)

# 1. 코드표 불러오기
zone_raw <- read_excel(
  "data/raw/zone_code.xlsx",
  sheet = "전국지역간",
  col_names = FALSE
)

# 2. 데이터 크기 확인
dim(zone_raw)

# 3. 상위 10행 확인
head(zone_raw, 10)

# ==========================================
# STEP 23. KTDB 252존 코드표 전처리
# ==========================================

# 1. 제목 및 설명 행 제거
zone_clean <- zone_raw %>%
  slice(-(1:2)) %>%
  
  # 2. 필요한 열만 추출
  transmute(
    교통존코드 = as.integer(...10),
    시도 = as.character(...1),
    시군구 = as.character(...2)
  )

# 3. 결과 확인
head(zone_clean, 10)

# 4. 지역 개수 확인
nrow(zone_clean)

# 5. 교통존 코드 중복 확인
zone_clean %>%
  count(교통존코드) %>%
  filter(n > 1)

# STEP 23-2. 교통존 코드와 통행량 연결
# ==========================================

# 1. 저장한 승용차 발생통행량 불러오기
traffic_origin_2024 <- read_csv(
  "data/processed/traffic_origin_2024_preliminary.csv",
  show_col_types = FALSE
)

# 2. 교통존 코드 기준으로 지역명 연결
traffic_region_2024 <- traffic_origin_2024 %>%
  left_join(
    zone_clean,
    by = c("출발시군구" = "교통존코드")
  )

# 3. 결과 확인
head(traffic_region_2024, 10)

# 4. 지역명이 연결되지 않은 데이터 확인
traffic_region_2024 %>%
  filter(is.na(시군구))

# 5. 데이터 개수 확인
nrow(traffic_region_2024)

# STEP 24. 지역 코드 검증
# ==========================================

# 1. 시도별 교통존 개수
traffic_region_2024 %>%
  group_by(시도) %>%
  summarise(
    교통존수 = n(),
    .groups = "drop"
  ) %>%
  arrange(시도)

# 2. 원본 출발시도 코드와 연결된 시도 비교
traffic_region_2024 %>%
  group_by(출발시도, 시도) %>%
  summarise(
    교통존수 = n(),
    .groups = "drop"
  ) %>%
  arrange(출발시도)

# STEP 25. 자동차 등록 데이터와 지역 비교
# ==========================================

# 자동차 등록 데이터 불러오기
car_final <- read_csv(
  "data/processed/car_registration_preliminary.csv",
  show_col_types = FALSE
)

# 자동차 등록 데이터 확인
head(car_final, 10)

# 지역 개수 확인
nrow(car_final)

# 자동차 등록 데이터의 시도 목록
sort(unique(car_final$시도))

# KTDB 데이터의 시도 목록
sort(unique(traffic_region_2024$시도))

# STEP 26. 전남광주 데이터 확인
# ==========================================

car_final %>%
  filter(시도 == "전남광주") %>%
  arrange(시군구) %>%
  print(n = Inf)

# STEP 26-2. 전남광주 지역 분리
# ==========================================

car_final <- car_final %>%
  mutate(
    시도 = case_when(
      시도 == "전남광주" &
        시군구 %in% c("동구", "서구", "남구", "북구", "광산구") ~ "광주",
      
      시도 == "전남광주" ~ "전남",
      
      TRUE ~ 시도
    )
  )

# 시도 목록 확인
sort(unique(car_final$시도))

# 광주와 전남의 지역 개수 확인
car_final %>%
  filter(시도 %in% c("광주", "전남")) %>%
  count(시도)

# STEP 26-3. 시도 이름 통일
# ==========================================

car_final <- car_final %>%
  mutate(
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

# 변경 결과 확인
sort(unique(car_final$시도))

# 지역 개수 확인
nrow(car_final)

# STEP 27. 자동차 등록 데이터와 KTDB 지역 비교
# ==========================================

# 자동차 등록 데이터에만 존재하는 지역
car_only <- car_final %>%
  anti_join(
    traffic_region_2024,
    by = c("시도", "시군구")
  )

# 결과 확인
car_only %>%
  select(시도, 시군구, 자가용_승용차) %>%
  print(n = Inf)

# 지역 개수
nrow(car_only)

# KTDB에만 존재하는 지역
traffic_only <- traffic_region_2024 %>%
  anti_join(
    car_final,
    by = c("시도", "시군구")
  )

# 결과 확인
traffic_only %>%
  select(시도, 시군구, 승용차_발생통행량) %>%
  print(n = Inf)

# 지역 개수
nrow(traffic_only)

# STEP 28. KTDB 지역 단위 통일
# ==========================================

traffic_standard <- traffic_region_2024 %>%
  mutate(
    시군구_통합 = case_when(
      # 일반구가 있는 시는 시 단위로 통합
      str_detect(시군구, "시 ") ~
        str_extract(시군구, "^[^ ]+시"),
      
      # 세종시 이름 통일
      시도 == "세종특별자치시" ~ "세종특별자치시",
      
      # 나머지 지역은 그대로 유지
      TRUE ~ 시군구
    )
  )

# 결과 확인
traffic_standard %>%
  select(시도, 시군구, 시군구_통합) %>%
  head(20)

# STEP 28-2. 시군구별 통행량 합산
# ==========================================

traffic_final <- traffic_standard %>%
  group_by(시도, 시군구_통합) %>%
  summarise(
    승용차_발생통행량 = sum(승용차_발생통행량, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  rename(시군구 = 시군구_통합)

# 통합된 지역 수 확인
nrow(traffic_final)

# 통합 결과 확인
head(traffic_final, 10)

# 통행량 합계 검증
all.equal(
  sum(traffic_region_2024$승용차_발생통행량),
  sum(traffic_final$승용차_발생통행량)
)

# STEP 29. 지역 통합 후 불일치 확인
# ==========================================

# 1. 자동차 등록 데이터에만 있는 지역
car_only_final <- car_final %>%
  anti_join(
    traffic_final,
    by = c("시도", "시군구")
  )

# 2. KTDB 데이터에만 있는 지역
traffic_only_final <- traffic_final %>%
  anti_join(
    car_final,
    by = c("시도", "시군구")
  )

# 3. 자동차 등록 데이터에만 있는 지역 출력
car_only_final %>%
  select(시도, 시군구, 자가용_승용차) %>%
  print(n = Inf)

# 4. KTDB 데이터에만 있는 지역 출력
traffic_only_final %>%
  select(시도, 시군구, 승용차_발생통행량) %>%
  print(n = Inf)

# 5. 불일치 지역 개수
nrow(car_only_final)
nrow(traffic_only_final)

# STEP 29. 지역 통합 후 불일치 확인
# ==========================================

# 1. 자동차 등록 데이터에만 있는 지역
car_only_final <- car_final %>%
  anti_join(
    traffic_final,
    by = c("시도", "시군구")
  )

# 2. KTDB 데이터에만 있는 지역
traffic_only_final <- traffic_final %>%
  anti_join(
    car_final,
    by = c("시도", "시군구")
  )

# 3. 자동차 등록 데이터에만 있는 지역 출력
car_only_final %>%
  select(시도, 시군구, 자가용_승용차) %>%
  print(n = Inf)

# 4. KTDB 데이터에만 있는 지역 출력
traffic_only_final %>%
  select(시도, 시군구, 승용차_발생통행량) %>%
  print(n = Inf)

# 5. 불일치 지역 개수
nrow(car_only_final)
nrow(traffic_only_final)

# STEP 30. 지역 중복 및 불일치 검증
# ==========================================

# 1. 자동차 등록 데이터의 중복 지역 확인
car_final %>%
  count(시도, 시군구) %>%
  filter(n > 1) %>%
  print(n = Inf)

# 2. KTDB 데이터의 중복 지역 확인
traffic_final %>%
  count(시도, 시군구) %>%
  filter(n > 1) %>%
  print(n = Inf)

# 3. 실제 고유 지역 수 확인
car_final %>%
  distinct(시도, 시군구) %>%
  nrow()

traffic_final %>%
  distinct(시도, 시군구) %>%
  nrow()

# 4. 자동차 등록 데이터에만 있는 지역 확인
car_only_final %>%
  select(시도, 시군구, 자가용_승용차) %>%
  print(n = Inf)

# 5. KTDB에만 있는 지역 확인
traffic_only_final %>%
  select(시도, 시군구, 승용차_발생통행량) %>%
  print(n = Inf)

# STEP 31. 인천광역시 지역 비교
# ==========================================

# 자동차 등록 데이터
car_final %>%
  filter(시도 == "인천광역시") %>%
  select(시군구, 자가용_승용차) %>%
  arrange(시군구) %>%
  print(n = Inf)

# KTDB 통행량 데이터
traffic_final %>%
  filter(시도 == "인천광역시") %>%
  select(시군구, 승용차_발생통행량) %>%
  arrange(시군구) %>%
  print(n = Inf)

car_final %>%
  filter(시도 == "인천광역시") %>%
  summarise(
    지역수 = n(),
    총등록대수 = sum(자가용_승용차, na.rm = TRUE)
  )

# STEP 32. 인천 행정구역 통합
# ==========================================

car_adjusted <- car_final %>%
  mutate(
    시군구 = case_when(
      
      # 검단구, 서해구 → 기존 서구
      시도 == "인천광역시" &
        시군구 %in% c("검단구", "서해구") ~ "서구",
      
      # 영종구 → 기존 중구
      시도 == "인천광역시" &
        시군구 == "영종구" ~ "중구",
      
      # 나머지는 유지
      TRUE ~ 시군구
    )
  ) %>%
  
  group_by(기준월, 시도, 시군구) %>%
  summarise(
    자가용_승용차 = sum(자가용_승용차, na.rm = TRUE),
    .groups = "drop"
  )

# 인천 지역 확인
car_adjusted %>%
  filter(시도 == "인천광역시") %>%
  arrange(시군구) %>%
  print(n = Inf)

# STEP 32-2. 등록대수 합계 검증
# ==========================================

all.equal(
  sum(car_final$자가용_승용차, na.rm = TRUE),
  sum(car_adjusted$자가용_승용차, na.rm = TRUE)
)

# 통합 후 전체 지역 수
nrow(car_adjusted)

# 아직 일치하지 않는 지역 확인
car_adjusted %>%
  anti_join(
    traffic_final,
    by = c("시도", "시군구")
  ) %>%
  select(시도, 시군구, 자가용_승용차) %>%
  print(n = Inf)

# STEP 33. 군위군 및 청원군 확인
# ==========================================

# 1. 군위군 확인
car_adjusted %>%
  filter(str_detect(시군구, "군위")) %>%
  print(n = Inf)

# 2. 청원군 및 청주시 확인
car_adjusted %>%
  filter(str_detect(시군구, "청원|청주")) %>%
  print(n = Inf)

# 3. KTDB 군위군 확인
traffic_final %>%
  filter(str_detect(시군구, "군위")) %>%
  print(n = Inf)

# 4. KTDB 청주시 확인
traffic_final %>%
  filter(str_detect(시군구, "청주")) %>%
  print(n = Inf)

# STEP 34. 군위군 및 청원군 통합
# ==========================================

car_adjusted2 <- car_adjusted %>%
  mutate(
    
    # 시도 변경
    시도 = case_when(
      시도 == "경상북도" & 시군구 == "군위군" ~ "대구광역시",
      TRUE ~ 시도
    ),
    
    # 시군구 변경
    시군구 = case_when(
      시도 == "충청북도" & 시군구 == "청원군" ~ "청주시",
      TRUE ~ 시군구
    )
    
  ) %>%
  
  group_by(기준월, 시도, 시군구) %>%
  summarise(
    자가용_승용차 = sum(자가용_승용차, na.rm = TRUE),
    .groups = "drop"
  )


# 군위군 결과 확인
car_adjusted2 %>%
  filter(시군구 == "군위군") %>%
  print(n = Inf)


# 청주시 결과 확인
car_adjusted2 %>%
  filter(시군구 == "청주시") %>%
  print(n = Inf)

# STEP 35. 데이터 검증
# ==========================================

# 1. 전체 등록대수 유지 여부
all.equal(
  sum(car_final$자가용_승용차, na.rm = TRUE),
  sum(car_adjusted2$자가용_승용차, na.rm = TRUE)
)

# 2. 전체 지역 수
nrow(car_adjusted2)

# 3. KTDB와 일치하지 않는 지역
car_adjusted2 %>%
  anti_join(
    traffic_final,
    by = c("시도", "시군구")
  ) %>%
  print(n = Inf)

# STEP 36-1. 인천 중구·동구 통합
# ==========================================

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
    자가용_승용차 = sum(자가용_승용차, na.rm = TRUE),
    .groups = "drop"
  )

# STEP 36-2. KTDB 중구·동구 통합
# ==========================================

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
    승용차_발생통행량 = sum(승용차_발생통행량, na.rm = TRUE),
    .groups = "drop"
  )

# STEP 37. 최종 검증
# ==========================================

# 1. 자동차 등록대수 유지
all.equal(
  sum(car_final$자가용_승용차, na.rm = TRUE),
  sum(car_final_ready$자가용_승용차, na.rm = TRUE)
)

# 2. KTDB 통행량 유지
all.equal(
  sum(traffic_final$승용차_발생통행량, na.rm = TRUE),
  sum(traffic_final_ready$승용차_발생통행량, na.rm = TRUE)
)

# 3. 지역 수 확인
nrow(car_final_ready)
nrow(traffic_final_ready)

# 4. 일치하지 않는 지역 확인
car_final_ready %>%
  anti_join(
    traffic_final_ready,
    by = c("시도", "시군구")
  )

traffic_final_ready %>%
  anti_join(
    car_final_ready,
    by = c("시도", "시군구")
  )

# STEP 38 전처리 완료 데이터 저장
# ==========================================

write_csv(
  car_final_ready,
  "data/processed/car_registration_final.csv"
)

write_csv(
  traffic_final_ready,
  "data/processed/traffic_origin_2024_final.csv"
)

# 저장 확인
file.exists("data/processed/car_registration_final.csv")
file.exists("data/processed/traffic_origin_2024_final.csv")















