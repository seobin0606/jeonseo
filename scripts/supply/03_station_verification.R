
# ============================================================
# 수소충전소 최적 입지 선정
# 03. 수소충전소 매칭 검증 및 승용차 충전 가능 여부 정리
#
# 목적:
# 2026년 상업용 수소충전소와 한국석유관리원 자료의
# 매칭 후보를 연결하여 승용차 충전 가능 여부를 분류
#
# 입력:
# data/processed/supply/station_clean.csv
# data/processed/supply/match_candidates_2026.csv
#
# 출력:
# data/processed/supply/station_verified.csv
#
# 분류 기준:
# 1. 후보 없음       → 미확인
# 2. 후보 1개       → 충전가능차량 정보로 가능/불가능 후보 분류
# 3. 후보 2개 이상  → 미확인
#
# 주의:
# 매칭 결과와 승용차 충전 가능 여부는 검증 전 후보 상태
# 최종 공급지수 산출 전에 추가 검증 필요
# ============================================================

# 1. 패키지 불러오기
source("scripts/supply/00_packages.R")

# ============================================================
# 2. 파일 경로 설정
# ============================================================

clean_path <- "data/processed/supply/station_clean.csv"

match_path <- "data/processed/supply/match_candidates_2026.csv"

output_path <- "data/processed/supply/station_verified.csv"

# ============================================================
# 3. 입력 파일 존재 여부 확인
# ============================================================

input_paths <- c(clean_path, match_path)

if (!all(file.exists(input_paths))) {
  stop(
    "입력 파일을 찾을 수 없습니다:\n",
    paste(
      input_paths[!file.exists(input_paths)],
      collapse = "\n"
    )
  )
}

# ============================================================
# 4. 데이터 불러오기
# ============================================================

clean <- read_csv(
  clean_path,
  show_col_types = FALSE
)

match_data <- read_csv(
  match_path,
  show_col_types = FALSE
)

cat("상업용 충전소 수:", nrow(clean), "\n")
cat("전체 매칭 후보 쌍:", nrow(match_data), "\n")

# ============================================================
# 5. 기준 충전소 ID 검증
# ============================================================

# ID를 문자형으로 통일
clean <- clean %>%
  mutate(
    station_id = as.character(station_id)
  )

# 충전소 ID 결측 및 중복 검증
stopifnot(
  !anyNA(clean$station_id),
  !anyDuplicated(clean$station_id)
)

# ============================================================
# 6. 매칭 후보 ID 정리
# ============================================================

match_clean <- match_data %>%
  mutate(
    gas_id = as.character(gas_id),
    petroleum_id = as.character(petroleum_id)
  ) %>%
  filter(
    !is.na(gas_id),
    !is.na(petroleum_id)
  ) %>%
  distinct(
    gas_id,
    petroleum_id,
    .keep_all = TRUE
  )

# ============================================================
# 7. 상업용 충전소에 해당하는 후보만 추출
# ============================================================

# 매칭 후보는 원본 2026년 전체 충전소를 기준으로 생성됨
# station_clean은 연구용 충전소를 제외한 상업용 충전소만 포함
# 따라서 상업용 충전소 ID에 해당하는 후보만 사용

match_clean <- match_clean %>%
  filter(
    gas_id %in% clean$station_id
  )

# 중복 후보 검증
stopifnot(
  !anyDuplicated(
    match_clean[c("gas_id", "petroleum_id")]
  )
)

cat(
  "상업용 충전소 매칭 후보 쌍:",
  nrow(match_clean),
  "\n"
)

# ============================================================
# 8. 충전소별 매칭 후보 정리
# ============================================================

match_summary <- match_clean %>%
  group_by(gas_id) %>%
  summarise(
    candidate_count = n_distinct(petroleum_id),
    
    petroleum_id = if (n_distinct(petroleum_id) == 1) {
      first(petroleum_id)
    } else {
      NA_character_
    },
    
    available_vehicle = if (n_distinct(petroleum_id) == 1) {
      first(충전가능차량)
    } else {
      NA_character_
    },
    
    match_method = if (n_distinct(petroleum_id) == 1) {
      first(match_method)
    } else {
      NA_character_
    },
    
    distance_m = if (n_distinct(petroleum_id) == 1) {
      first(distance_m)
    } else {
      NA_real_
    },
    
    .groups = "drop"
  ) %>%
  rename(
    station_id = gas_id
  )

# ============================================================
# 9. 기준 충전소 데이터와 매칭 결과 결합
# ============================================================

station_verified <- clean %>%
  select(
    -any_of(
      c("passenger_status", "operation_status")
    )
  ) %>%
  left_join(
    match_summary,
    by = "station_id",
    relationship = "one-to-one"
  ) %>%
  mutate(
    candidate_count = coalesce(
      candidate_count,
      0L
    ),
    
    match_status = case_when(
      candidate_count == 0 ~ "미매칭",
      candidate_count == 1 ~ "단일후보_검증필요",
      TRUE ~ "복수후보_검증필요"
    ),
    
    passenger_status = case_when(
      candidate_count != 1 ~ "미확인",
      is.na(available_vehicle) ~ "미확인",
      str_detect(available_vehicle, "승용") ~ "가능_후보",
      TRUE ~ "불가능_후보"
    ),
    
    operation_status = "미확인",
    
    verification_status = "검증필요"
  )

# ============================================================
# 10. 결과 데이터 검증
# ============================================================

# 상업용 충전소 수 유지 여부
stopifnot(
  nrow(station_verified) == nrow(clean)
)

# 충전소 ID 중복 여부
stopifnot(
  !anyDuplicated(station_verified$station_id)
)

# 매칭 상태 결측 여부
stopifnot(
  !anyNA(station_verified$match_status),
  !anyNA(station_verified$passenger_status)
)

# ============================================================
# 11. 결과 저장
# ============================================================

dir.create(
  dirname(output_path),
  recursive = TRUE,
  showWarnings = FALSE
)

# 기존 파일이 있으면 백업 생성
if (file.exists(output_path)) {
  backup_path <- sub(
    "\\.csv$",
    "_backup.csv",
    output_path
  )
  
  if (!file.exists(backup_path)) {
    file.copy(
      output_path,
      backup_path
    )
  }
}

write_excel_csv(
  station_verified,
  output_path
)

# ============================================================
# 12. 결과 요약
# ============================================================

cat("\n===== 수소충전소 매칭 검증 결과 =====\n")

cat(
  "전체 상업용 충전소:",
  nrow(station_verified),
  "개\n"
)

cat("\n[매칭 상태]\n")

print(
  station_verified %>%
    count(match_status)
)

cat("\n[승용차 충전 가능 여부]\n")

print(
  station_verified %>%
    count(passenger_status)
)

cat("\n[검증 상태]\n")

print(
  station_verified %>%
    count(verification_status)
)

cat(
  "\n저장 완료:",
  output_path,
  "\n"
)

cat("수소충전소 매칭 검증 데이터 정리 완료\n")

