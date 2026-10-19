
# =====================================================
# 02. 수소충전소 공급 데이터 전처리
# 담당: 전서빈
# =====================================================

source("scripts/supply/00_packages.R")

dir.create("data/processed/supply",
           recursive = TRUE, showWarnings = FALSE)

# 1. 2026년 충전소 데이터 불러오기
station <- read_csv(
  "data/raw/supply/station_2026.csv",
  locale = locale(encoding = "CP949"),
  show_col_types = FALSE
)

# 2. 연구용 / 상업용 분류
print(table(station$용도, useNA = "ifany"))

# 3. 상업용 충전소만 추출
commercial <- station %>%
  mutate(
    station_id = row_number()
  ) %>%
  filter(용도 == "상업용")

# 4. 공급 관련 변수 정리
station_clean <- commercial %>%
  transmute(
    station_id,
    station_name = 충전소명,
    address = 주소,
    charger_count = parse_number(
      as.character(`충전기 개수`)
    ),
    latitude = as.numeric(위도),
    longitude = as.numeric(경도),
    construction_year = 구축연도,
    supply_method = 공급방식,
    region_code = NA_character_,
    region_name = NA_character_,
    passenger_status = "미확인",
    operation_status = "미확인"
  )

# 5. 결측치 확인
colSums(is.na(station_clean))

# 6. 중복 충전소명 확인
station_clean %>%
  count(station_name) %>%
  filter(n > 1) %>%
  print()

# 7. 저장
write_excel_csv(
  station_clean,
  "data/processed/supply/station_clean.csv"
)

# 8. 결과 확인
cat("상업용 충전소 수:", nrow(station_clean), "\n")
cat("충전기 수 합계:",
    sum(station_clean$charger_count, na.rm = TRUE),
    "\n")

head(station_clean)
