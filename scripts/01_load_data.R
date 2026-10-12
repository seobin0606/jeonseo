# =====================================
# 개인용 수소승용차 미래 잠재수요 분석
# 01. 데이터 불러오기
# =====================================

# 1. 필요한 패키지 설치
install.packages("tidyverse")
install.packages("readxl")
install.packages("janitor")
install.packages("readr")


# 2. 라이브러리 불러오기
library(tidyverse)
library(readxl)
library(janitor)
library(readr)

# 3. 현재 작업 경로 확인
getwd()

file.exists("data/raw/car_registration.xlsx")

# ==========================================
# 4. 자동차 등록 현황 데이터 불러오기
# ==========================================

car_raw <- read_excel(
  "data/raw/car_registration.xlsx",
  col_names = FALSE
)

# 데이터 크기 확인
dim(car_raw)

# 컬럼명 확인
names(car_raw)

# 데이터 상위 10행 확인
head(car_raw, 10)

# ==========================================
# 5. 자동차 등록 현황 데이터 전처리
# ==========================================

# 필요한 컬럼만 선택
car_clean <- car_raw %>%
  slice(-(1:6)) %>% 
  transmute(
    기준월 = as.character(...1),
    시도 = as.character(...2),
    시군구 = as.character(...3),
    자가용_승용차 = parse_number(as.character(...5))
  )

# 데이터 확인
head(car_clean, 10)

# 데이터 구조 확인
str(car_clean)

# 결측치 확인
colSums(is.na(car_clean))

# 기준월 확인
unique(car_clean$기준월)

# 기준월별 데이터 개수 확인
table(car_clean$기준월)

# ==========================================
# 6. 시군구별 자동차 등록 데이터 검증
# ==========================================

# 전체 데이터 개수
nrow(car_clean)

# 시도별 데이터 개수
car_clean %>%
  count(시도)

# 시군구 결측치 확인
sum(is.na(car_clean$시군구))

# 시군구 이름 확인
unique(car_clean$시군구)

# 시도 + 시군구 기준 중복 확인
car_clean %>%
  count(시도, 시군구) %>%
  filter(n > 1)

# 결측치 확인
colSums(is.na(car_clean))

# 자가용 승용차 등록대수 기초통계량
summary(car_clean$자가용_승용차)

# ==========================================
# 7. 시도 합계 및 이상치 확인
# ==========================================

# 자가용 승용차 등록대수가 많은 상위 15개 지역
car_clean %>%
  arrange(desc(자가용_승용차)) %>%
  head(15)

# 자가용 승용차 등록대수가 적은 하위 15개 지역
car_clean %>%
  arrange(자가용_승용차) %>%
  head(15)

# 시군구에 합계 관련 명칭이 포함된 행 확인
car_clean %>%
  filter(
    grepl("합계|총계|소계|전체", 시군구)
  )

# 시도와 시군구 이름이 같은 행 확인
car_clean %>%
  filter(시도 == 시군구)

# ==========================================
# 8. 행정구역 데이터 추가 검증
# ==========================================

# 1. 등록대수가 가장 많은 상위 20개 지역
car_clean %>%
  arrange(desc(자가용_승용차)) %>%
  print(n = 20)

# 2. 행정구역 변경 관련 지역 확인
car_clean %>%
  filter(
    시군구 %in% c(
      "청원군",
      "군위군",
      "부천시",
      "화성시",
      "화성특례시",
      "청주시"
    )
  ) %>%
  arrange(시도, 시군구)

# 3. 시도별 등록대수 합계 확인
car_clean %>%
  group_by(시도) %>%
  summarise(
    자가용_승용차_합계 = sum(자가용_승용차),
    지역수 = n(),
    .groups = "drop"
  ) %>%
  arrange(desc(자가용_승용차_합계))

# ==========================================
# 9. 시도별 지역 목록 확인
# ==========================================

# 서울 전체 지역 확인
car_clean %>%
  filter(시도 == "서울") %>%
  arrange(시군구) %>%
  print(n = Inf)

# 경기 전체 지역 확인
car_clean %>%
  filter(시도 == "경기") %>%
  arrange(시군구) %>%
  print(n = Inf)

# ==========================================
# 10. 시도 전체 합계 제거
# ==========================================

car_no_total <- car_clean %>%
  filter(시군구 != "계")

# 제거 후 데이터 개수 확인
nrow(car_no_total)

# 합계 행이 남아 있는지 확인
car_no_total %>%
  filter(시군구 == "계")

# ==========================================
# 11. 시군구 단위 통일
# ==========================================

library(dplyr)
library(stringr)

car_region <- car_no_total %>%
  mutate(
    시군구_통합 = case_when(
      # 일반구가 있는 시는 시 단위로 통합
      str_detect(시군구, "시 ") ~
        str_extract(시군구, "^[^ ]+시"),
      
      # 나머지는 기존 시군구 유지
      TRUE ~ 시군구
    )
  )

# 통합 결과 확인
car_region %>%
  select(시도, 시군구, 시군구_통합, 자가용_승용차) %>%
  print(n = 30)

# ==========================================
# 12. 시군구별 자가용 승용차 등록대수 집계
# ==========================================

car_final <- car_region %>%
  group_by(기준월, 시도, 시군구_통합) %>%
  summarise(
    자가용_승용차 = sum(자가용_승용차),
    .groups = "drop"
  ) %>%
  rename(
    시군구 = 시군구_통합
  )

# 결과 확인
head(car_final, 20)

# 최종 지역 개수 확인
nrow(car_final)

# 중복 지역 확인
car_final %>%
  count(시도, 시군구) %>%
  filter(n > 1)

# 1. 최종 지역 개수
nrow(car_final)

# 2. 경기도 지역별 등록대수 확인
car_final %>%
  filter(시도 == "경기") %>%
  arrange(시군구) %>%
  print(n = Inf)

# ==========================================
# 13. 최종 데이터 검증
# ==========================================

# 과거 행정구역 명칭 확인
car_final %>%
  filter(
    시군구 %in% c("청원군", "군위군")
  )

# 시도별 지역 개수 확인
car_final %>%
  count(시도)

# 최종 결측치 확인
colSums(is.na(car_final))

# 최종 중복 확인
car_final %>%
  count(시도, 시군구) %>%
  filter(n > 1)

# ==========================================
# 14. 자동차 등록 데이터 임시 저장
# ==========================================

write_csv(
  car_final,
  "data/processed/car_registration_preliminary.csv"
)

# 저장 확인
file.exists(
  "data/processed/car_registration_preliminary.csv"
)










