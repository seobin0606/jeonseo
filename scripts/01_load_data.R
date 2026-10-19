# =====================================
# 자가용 수소승용차 미래 잠재수요 분석
# 01. 자동차 등록대수 데이터 전처리
# 입력: data/raw/car_registration.xlsx
# 출력: data/processed/car_registration_preliminary.csv
# =====================================

# 필요한 패키지 설치
install.packages("tidyverse")
install.packages("readxl")
install.packages("janitor")
install.packages("readr")

# 라이브러리 불러오기
library(tidyverse)
library(readxl)
library(janitor)

# 원본 데이터 존재 여부 확인
stopifnot(file.exists("data/raw/car_registration.xlsx"))

# 원본 Excel의 상단 설명 6행을 제외하고 분석에 필요한 열만 선택한다.
# ...1: 기준월, ...2: 시도, ...3: 시군구, ...5: 자가용 승용차 등록대수
car_raw <- read_excel("data/raw/car_registration.xlsx",col_names = FALSE)
car_clean <- car_raw %>%
  slice(-(1:6)) %>% 
  transmute(
    기준월 = as.character(...1),
    시도 = as.character(...2),
    시군구 = as.character(...3),
    자가용_승용차 = parse_number(as.character(...5))
  )

# 데이터 구조 및 기초통계 확인
glimpse(car_clean)
summary(car_clean$자가용_승용차)

# 분석 대상 기준월 확인
unique(car_clean$기준월)

# ==========================================
# 시군구별 자동차 등록 데이터 검증
# ==========================================

# 시도 + 시군구 기준 중복 확인
car_clean %>%
  count(시도, 시군구) %>%
  filter(n > 1)

# 결측치 확인
colSums(is.na(car_clean))

# 자가용 승용차 등록대수 기초통계량
summary(car_clean$자가용_승용차)

# ==========================================
# 시도 합계 및 이상치 확인
# ==========================================

# 시군구에 합계 관련 명칭이 포함된 행 확인
car_clean %>%
  filter(
    시군구 == "계" |
    grepl("합계|총계|소계|전체", 시군구)
  )

# 시도와 시군구 이름이 같은 행 확인
car_clean %>%
  filter(시도 == 시군구)

# ==========================================
# 행정구역 변경 관련 지역 확인
# ==========================================

# 행정구역 변경 및 통합 검토 대상 지역 확인
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

# ==========================================
# 시도 전체 합계 제거
# ==========================================

# 시군구별 등록대수만 남기기 위해 합계 행 제거
car_no_total <- car_clean %>%
  filter(시군구 != "계")

# 합계 행이 정상적으로 제거되었는지 검증
stopifnot(
  !any(car_no_total$시군구 == "계", na.rm = TRUE)
)


# 일반구가 있는 도시(예: '수원시 장안구')는 시 단위로 묶는다.
# 그 외 시군구는 원래 이름을 유지한다.
car_region <- car_no_total %>%
  mutate(
    시군구_통합 = case_when(
      str_detect(시군구, "시 ") ~ str_extract(시군구, "^[^ ]+시"),
      TRUE ~ 시군구
    )
  )

# ============================================================
# 시군구별 자가용 승용차 등록대수 집계
# ============================================================

# 동일한 기준월, 시도, 시군구에 속한 등록대수를 합산한다.
car_final <- car_region %>%
  group_by(기준월, 시도, 시군구_통합) %>%
  summarise(자가용_승용차 = sum(자가용_승용차),.groups = "drop") %>%
  rename(시군구 = 시군구_통합)

# ============================================================
# 집계 결과 검증
# ============================================================

# 집계된 최종 지역 개수 확인
nrow(car_final)

# 시도 + 시군구 기준 중복 지역 확인
car_final %>%  
  count(시도, 시군구) %>%
  filter(n > 1)

# 최종 결측치 확인
colSums(is.na(car_final))

# 과거 행정구역 명칭 확인
car_final %>%
  filter(
    시군구 %in% c("청원군", "군위군"))


# 자동차 등록 데이터 임시 저장
write_csv(car_final, "data/processed/car_registration_preliminary.csv")












