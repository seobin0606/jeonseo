# ==========================================
# 자가용 수소승용차 미래 잠재수요 분석
# 02. KTDB 승용차 통행량 전처리
# 입력: data/raw/traffic_od.xlsx (2024_주수단OD 시트)
# 출력: data/processed/traffic_origin_2024_preliminary.csv
# ==========================================

# 라이브러리 불러오기
library(tidyverse)
library(readxl)

# 파일 존재 여부 확인
file.exists("data/raw/traffic_od.xlsx")

# 엑셀 시트 목록 확인
excel_sheets("data/raw/traffic_od.xlsx")


# OD(출발지-도착지) 자료에서 승용차 통행량과 출발·도착 코드만 추출한다.
traffic_2024 <- read_excel("data/raw/traffic_od.xlsx",sheet = "2024_주수단OD")
traffic_car_2024 <- traffic_2024 %>%
  select(출발시도,도착시도,출발시군구,도착시군구,승용차)

# 1. 데이터 상위 10행 확인
head(traffic_car_2024, 10)

# 2. 결측치 확인
colSums(is.na(traffic_car_2024))

# 3. 승용차 통행량 기초통계량
summary(traffic_car_2024$승용차)


# KTDB 교통존 코드 검증

# 1.출발시군구 코드 개수
n_distinct(traffic_car_2024$출발시군구)

# 출발시도와 출발시군구 조합의 개수 확인
traffic_car_2024 %>%
  distinct(출발시도, 출발시군구) %>%
  nrow()

# 출발지 기준으로 모든 도착지의 승용차 통행량을 합산한다.
# '발생통행량'은 해당 지역에서 출발한 통행량이며 실제 수소차 충전 수요는 아니다.
traffic_origin_2024 <- traffic_car_2024 %>%
  group_by(출발시도, 출발시군구) %>%
  summarise(승용차_발생통행량 = sum(승용차, na.rm = TRUE),.groups = "drop")

# 분석지역 수 확인
nrow(traffic_origin_2024)

# 발생통행량 기초통계 확인
summary(traffic_origin_2024$승용차_발생통행량)

# 결측치 확인
colSums(is.na(traffic_origin_2024))

# 중복 지역 확인
traffic_origin_2024 %>%
  count(출발시도, 출발시군구) %>%
  filter(n > 1)

# 원본 통행량과 집계된 통행량의 합계 비교
stopifnot(
  isTRUE(all.equal(
    sum(traffic_car_2024$승용차, na.rm = TRUE),
    sum(traffic_origin_2024$승용차_발생통행량)
  ))
)

# 2024년 승용차 발생통행량 임시 저장
write_csv(traffic_origin_2024,"data/processed/traffic_origin_2024_preliminary.csv")




