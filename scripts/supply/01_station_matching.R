
# ============================================================
# 수소충전소 최적 입지 선정
# 01. 수소충전소 연도 간 매칭
#
# 목적:
# 2026년 수소충전소 현황과 2024년 충전가능차량 자료의
# 충전소명 및 주소를 비교하여 매칭 후보 생성
#
# 입력:
# data/raw/supply/station_2026.csv
# data/raw/supply/station_2024.csv
#
# 출력:
# data/processed/supply/match_candidates.csv
# data/processed/supply/station_review.csv
#
# 주의:
# 매칭 후보는 검토용이며 최종 확정 결과가 아님
# ============================================================

# 1. 패키지 불러오기
source("scripts/supply/00_packages.R")

# 2. 입력 파일 확인
stopifnot(
  file.exists("data/raw/supply/station_2026.csv"),
  file.exists("data/raw/supply/station_2024.csv")
)

# 3. 결과 저장 폴더 생성
dir.create(
  "data/processed/supply",
  recursive = TRUE,
  showWarnings = FALSE
)

# ============================================================
# 4. 충전소 데이터 불러오기
# ============================================================

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

cat("2026년 충전소:", nrow(station_2026), "\n")
cat("2024년 충전소:", nrow(station_2024), "\n")

# ============================================================
# 5. 충전소명 및 주소 정규화
# ============================================================

# 영문 소문자 변환 및 공백·특수문자 제거
clean_text <- function(x) {
  x %>%
    str_to_lower() %>%
    str_replace_all("[^가-힣a-z0-9]", "")
}

base <- station_2026 %>%
  mutate(
    station_id = row_number(),
    name_clean = clean_text(충전소명),
    address_clean = clean_text(주소)
  )

old <- station_2024 %>%
  mutate(
    old_id = row_number(),
    name_clean = clean_text(상호),
    address_clean = clean_text(주소)
  )

# ============================================================
# 6. 충전소명 기준 매칭
# ============================================================

name_match <- base %>%
  filter(
    !is.na(name_clean),
    name_clean != ""
  ) %>%
  inner_join(
    old %>%
      filter(
        !is.na(name_clean),
        name_clean != ""
      ),
    by = "name_clean",
    suffix = c("_2026", "_2024"),
    relationship = "many-to-many"
  ) %>%
  mutate(
    match_method = "name"
  )

# ============================================================
# 7. 주소 기준 매칭
# ============================================================

address_match <- base %>%
  filter(
    !is.na(address_clean),
    address_clean != ""
  ) %>%
  inner_join(
    old %>%
      filter(
        !is.na(address_clean),
        address_clean != ""
      ),
    by = "address_clean",
    suffix = c("_2026", "_2024"),
    relationship = "many-to-many"
  ) %>%
  mutate(
    match_method = "address"
  )

# ============================================================
# 8. 매칭 후보 통합
# ============================================================

# 동일한 충전소 쌍이 이름과 주소 모두 일치하면
# 매칭 근거를 name+address로 기록

candidates <- bind_rows(
  name_match,
  address_match
) %>%
  group_by(station_id, old_id) %>%
  mutate(
    match_method = paste(
      sort(unique(match_method)),
      collapse = "+"
    )
  ) %>%
  slice(1) %>%
  ungroup()

# ============================================================
# 9. 충전소별 매칭 후보 수 계산
# ============================================================

candidate_count <- candidates %>%
  count(
    station_id,
    name = "candidate_count"
  )

# 2026년 전체 충전소 유지
station_review <- base %>%
  left_join(
    candidate_count,
    by = "station_id"
  ) %>%
  mutate(
    candidate_count = replace_na(candidate_count, 0L),
    match_status = case_when(
      candidate_count == 0 ~ "미매칭",
      candidate_count == 1 ~ "후보1개",
      TRUE ~ "복수후보"
    )
  )

# ============================================================
# 10. 매칭 결과 검증
# ============================================================

# 전체 충전소 수 유지 여부
stopifnot(
  nrow(station_review) == nrow(station_2026)
)

# 충전소 쌍 중복 여부
stopifnot(
  !anyDuplicated(
    candidates[c("station_id", "old_id")]
  )
)

# ============================================================
# 11. 매칭 결과 저장
# ============================================================

write_excel_csv(
  candidates,
  "data/processed/supply/match_candidates.csv"
)

write_excel_csv(
  station_review,
  "data/processed/supply/review/station_review.csv"
)

# ============================================================
# 12. 결과 요약
# ============================================================

print(table(station_review$match_status))

cat("기준 충전소 수:", nrow(station_review), "\n")
cat("매칭 후보 쌍:", nrow(candidates), "\n")
cat(
  "매칭 후보가 있는 충전소:",
  sum(station_review$candidate_count > 0),
  "\n"
)

cat("2026년·2024년 충전소 매칭 완료\n")


