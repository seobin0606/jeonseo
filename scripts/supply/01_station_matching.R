
# =====================================================
# 01. 수소충전소 데이터 매칭
# 담당: 팀원 B
# =====================================================

source("scripts/supply/00_packages.R")

dir.create("data/processed/supply",
           recursive = TRUE, showWarnings = FALSE)

# 1. CSV 불러오기
read_data <- function(path) {
  read_csv(
    path,
    locale = locale(encoding = "CP949"),
    show_col_types = FALSE
  )
}

station_2026 <- read_data(
  "data/raw/supply/station_2026.csv"
)

station_2024 <- read_data(
  "data/raw/supply/station_2024.csv"
)

# 2. 데이터 구조 확인
cat("2026년 충전소:", nrow(station_2026), "\n")
cat("2024년 충전소:", nrow(station_2024), "\n")

print(names(station_2026))
print(names(station_2024))

# 3. 충전소명과 주소 정규화 함수
clean_text <- function(x) {
  x %>%
    str_to_lower() %>%
    str_replace_all("[^가-힣a-z0-9]", "")
}

# 4. 2026년 데이터 정리
base <- station_2026 %>%
  mutate(
    station_id = row_number(),
    name_clean = clean_text(충전소명),
    address_clean = clean_text(주소)
  )

# 5. 2024년 데이터 정리
old <- station_2024 %>%
  mutate(
    old_id = row_number(),
    name_clean = clean_text(상호),
    address_clean = clean_text(주소)
  )

# 6. 이름 또는 주소가 정확히 일치하는 후보 찾기
name_match <- base %>%
  inner_join(
    old,
    by = "name_clean",
    suffix = c("_2026", "_2024"),
    relationship = "many-to-many"
  ) %>%
  mutate(match_method = "name")

address_match <- base %>%
  inner_join(
    old,
    by = "address_clean",
    suffix = c("_2026", "_2024"),
    relationship = "many-to-many"
  ) %>%
  mutate(match_method = "address")

# 7. 중복 후보 통합
candidates <- bind_rows(
  name_match,
  address_match
) %>%
  distinct(station_id, old_id, .keep_all = TRUE)

# 8. 검토할 후보 수 집계
candidate_count <- candidates %>%
  count(station_id, name = "candidate_count")

# 9. 2026년 전체 충전소 유지
station_review <- base %>%
  left_join(candidate_count, by = "station_id") %>%
  mutate(
    candidate_count = replace_na(candidate_count, 0L),
    match_status = case_when(
      candidate_count == 0 ~ "미매칭",
      candidate_count == 1 ~ "후보1개",
      TRUE ~ "복수후보"
    )
  )

# 10. 결과 저장
write_excel_csv(
  candidates,
  "data/processed/supply/match_candidates.csv"
)

write_excel_csv(
  station_review,
  "data/processed/supply/station_review.csv"
)

# 11. 매칭 결과 확인
print(table(station_review$match_status))
cat("기준 충전소 수:", nrow(station_review), "\n")
cat("매칭 후보 행 수:", nrow(candidates), "\n")

