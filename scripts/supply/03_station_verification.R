
# ==============================================
# 03. 수소충전소 매칭 및 승용차 충전 가능 여부 정리
# ==============================================

source("scripts/supply/00_packages.R")

# 1. 파일 경로
clean_path <- "data/processed/supply/station_clean.csv"
match_path <- "data/processed/supply/match_candidates_2026.csv"
output_path <- "data/processed/supply/station_verified.csv"

# 2. 입력 파일 확인
paths <- c(clean_path, match_path)

if (!all(file.exists(paths))) {
  stop(
    "입력 파일을 찾을 수 없습니다:\n",
    paste(paths[!file.exists(paths)], collapse = "\n")
  )
}

# 3. 데이터 불러오기
clean <- read_csv(clean_path, show_col_types = FALSE)
match <- read_csv(match_path, show_col_types = FALSE)

# 4. 기준 충전소 ID 확인
if (anyDuplicated(clean$station_id)) {
  stop("기준 충전소 ID가 중복되어 있습니다.")
}

# 5. 중복 매칭 행 제거
match_clean <- match %>%
  mutate(
    gas_id = as.character(gas_id),
    petroleum_id = as.character(petroleum_id)
  ) %>%
  filter(!is.na(gas_id), !is.na(petroleum_id)) %>%
  group_by(gas_id, petroleum_id) %>%
  arrange(
    match(
      match_method,
      c("name", "address", "coordinate")
    ),
    distance_m,
    .by_group = TRUE
  ) %>%
  slice(1) %>%
  ungroup()

# 6. 충전소별 매칭 결과 정리
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
  rename(station_id = gas_id)

# 7. 기준 충전소 데이터와 결합
station_verified <- clean %>%
  mutate(station_id = as.character(station_id)) %>%
  select(
    -any_of(c(
      "passenger_status",
      "operation_status"
    ))
  ) %>%
  left_join(
    match_summary,
    by = "station_id"
  ) %>%
  mutate(
    candidate_count = coalesce(candidate_count, 0L),
    
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

# 8. 충전소 수 검증
if (nrow(station_verified) != nrow(clean)) {
  stop("충전소 수가 변경되었습니다.")
}

# 9. 결과 저장
dir.create(
  dirname(output_path),
  recursive = TRUE,
  showWarnings = FALSE
)

write_excel_csv(station_verified, output_path)

# 10. 결과 확인
cat("\n===== 공급 데이터 통합 결과 =====\n")

cat("전체 충전소:", nrow(station_verified), "개\n")

cat("\n[매칭 결과]\n")
print(station_verified %>% count(match_status))

cat("\n[승용차 충전 가능 여부]\n")
print(station_verified %>% count(passenger_status))

cat("\n저장 완료:", output_path, "\n")

