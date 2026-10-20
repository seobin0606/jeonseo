
# ============================================================
# 수소충전소 최적 입지 선정
# 02. 수소충전소 공급 데이터 전처리
#
# 목적:
# 2026년 수소충전소 현황에서 상업용 충전소를 추출하고
# 공급 분석에 필요한 변수와 충전소별 ID를 정리
#
# 입력:
# data/raw/supply/station_2026.csv
#
# 출력:
# data/processed/supply/station_clean.csv
#
# 주의:
# station_id는 원본 데이터의 행 순서를 기준으로 부여
# 이후 매칭 스크립트의 ID와 일치하도록 유지
# ============================================================

# 1. 패키지 불러오기
source("scripts/supply/00_packages.R")

# 2. 입력 파일 확인
input_path <- "data/raw/supply/station_2026.csv"

stopifnot(file.exists(input_path))

# 3. 결과 저장 폴더 생성
dir.create(
  "data/processed/supply",
  recursive = TRUE,
  showWarnings = FALSE
)

# ============================================================
# 4. 2026년 수소충전소 현황 불러오기
# ============================================================

station <- read_csv(
  input_path,
  locale = locale(encoding = "CP949"),
  show_col_types = FALSE
)

cat("전체 충전소 수:", nrow(station), "\n")

# ============================================================
# 5. 연구용 및 상업용 충전소 분류
# ============================================================

print(table(station$용도, useNA = "ifany"))

# 원본 행 순서에 따라 충전소 ID 부여
# ID 부여 후 상업용 충전소만 추출
commercial <- station %>%
  mutate(
    station_id = row_number()
  ) %>%
  filter(
    용도 == "상업용"
  )

# ============================================================
# 6. 공급 분석 변수 정리
# ============================================================

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

# ============================================================
# 7. 데이터 검증
# ============================================================

# 상업용 충전소 수 유지 여부
stopifnot(
  nrow(station_clean) == nrow(commercial)
)

# 충전소 ID 중복 여부
stopifnot(
  !anyDuplicated(station_clean$station_id)
)

# 주요 변수 결측치 확인
print(colSums(is.na(station_clean)))

# 충전소명 중복 확인
duplicate_names <- station_clean %>%
  count(
    station_name,
    name = "count"
  ) %>%
  filter(count > 1)

print(duplicate_names)

# 충전기 수가 음수인 경우 확인
invalid_chargers <- station_clean %>%
  filter(
    !is.na(charger_count),
    charger_count < 0
  )

stopifnot(nrow(invalid_chargers) == 0)

# 좌표 결측 및 범위 이상 확인
invalid_coordinates <- station_clean %>%
  filter(
    is.na(latitude) |
      is.na(longitude) |
      !between(latitude, -90, 90) |
      !between(longitude, -180, 180)
  )

cat(
  "좌표 확인 필요 충전소:",
  nrow(invalid_coordinates),
  "\n"
)

# ============================================================
# 8. 전처리 결과 저장
# ============================================================

write_excel_csv(
  station_clean,
  "data/processed/supply/station_clean.csv"
)

# ============================================================
# 9. 결과 요약
# ============================================================

cat("상업용 충전소 수:", nrow(station_clean), "\n")

cat(
  "충전기 수 합계:",
  sum(station_clean$charger_count, na.rm = TRUE),
  "\n"
)

cat("수소충전소 공급 데이터 전처리 완료\n")

