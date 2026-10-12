# ==========================================
# 개인용 수소승용차 미래 잠재수요 분석
# 02. KTDB 승용차 통행량 전처리
# ==========================================

# 1. 라이브러리 불러오기
library(tidyverse)
library(readxl)
library(dplyr)
library(readr)

# 2. 작업 경로 확인
getwd()

# 3. 파일 존재 여부 확인
file.exists("data/raw/traffic_od.xlsx")

# 4. 엑셀 시트 목록 확인
excel_sheets("data/raw/traffic_od.xlsx")

# ==========================================
# 2024년 KTDB 승용차 OD 데이터 불러오기
# ==========================================

traffic_2024 <- read_excel(
  "data/raw/traffic_od.xlsx",
  sheet = "2024_주수단OD"
)

# 데이터 크기 확인
dim(traffic_2024)

# 컬럼명 확인
names(traffic_2024)

# 상위 10개 행 확인
head(traffic_2024, 10)

# 데이터 구조 확인
str(traffic_2024)

# ==========================================
# 17. 승용차 통행량 데이터 추출
# ==========================================

traffic_car_2024 <- traffic_2024 %>%
  select(
    출발시도,
    도착시도,
    출발시군구,
    도착시군구,
    승용차
  )

# 1. 데이터 상위 10행 확인
head(traffic_car_2024, 10)

# 2. 데이터 크기 확인
dim(traffic_car_2024)

# 3. 결측치 확인
colSums(is.na(traffic_car_2024))

# 4. 승용차 통행량 기초통계량
summary(traffic_car_2024$승용차)

# ==========================================
# 18. KTDB 교통존 코드 확인
# ==========================================

# 출발시도 코드 확인
sort(unique(traffic_car_2024$출발시도))

# 출발시군구 코드 확인
sort(unique(traffic_car_2024$출발시군구))

# 출발시군구 코드 개수
n_distinct(traffic_car_2024$출발시군구)

# 출발시도별 출발시군구 코드 개수
traffic_car_2024 %>%
  group_by(출발시도) %>%
  summarise(
    지역수 = n_distinct(출발시군구),
    .groups = "drop"
  )

# ==========================================
# 19. 2024년 승용차 발생통행량 계산
# ==========================================

traffic_origin_2024 <- traffic_car_2024 %>%
  group_by(출발시도, 출발시군구) %>%
  summarise(
    승용차_발생통행량 = sum(승용차, na.rm = TRUE),
    .groups = "drop"
  )

# 데이터 확인
head(traffic_origin_2024, 10)

# 지역 개수 확인
nrow(traffic_origin_2024)

# 기초통계량 확인
summary(traffic_origin_2024$승용차_발생통행량)

# ==========================================
# 20. 발생통행량 데이터 검증
# ==========================================

# 1. 결측치 확인
colSums(is.na(traffic_origin_2024))

# 2. 중복 지역 확인
traffic_origin_2024 %>%
  count(출발시도, 출발시군구) %>%
  filter(n > 1)

# 3. 발생통행량 상위 10개 교통존
traffic_origin_2024 %>%
  arrange(desc(승용차_발생통행량)) %>%
  head(10)

# 4. 발생통행량 하위 10개 교통존
traffic_origin_2024 %>%
  arrange(승용차_발생통행량) %>%
  head(10)

# 5. 전체 통행량 합계 비교
sum(traffic_car_2024$승용차, na.rm = TRUE)

sum(traffic_origin_2024$승용차_발생통행량)

# ==========================================
# 21. 2024년 발생통행량 임시 저장
# ==========================================

write_csv(
  traffic_origin_2024,
  "data/processed/traffic_origin_2024_preliminary.csv"
)

# 저장 확인
file.exists(
  "data/processed/traffic_origin_2024_preliminary.csv"
)


