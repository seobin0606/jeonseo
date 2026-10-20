
# ============================================================
# 수소충전소 최적 입지 선정
# 01-2. 2026년 수소충전소 기관 간 매칭
#
# 목적:
# 한국가스안전공사와 한국석유관리원의
# 2026년 충전소 자료를 비교하여 매칭 후보 생성
#
# 입력:
# data/raw/supply/station_2026.csv
# data/raw/supply/station_petroleum_2026.csv
#
# 출력:
# data/processed/supply/match_candidates_2026.csv
# data/processed/supply/station_review_2026.csv
# data/processed/supply/single_candidates_2026.csv
# data/processed/supply/multiple_candidates_2026.csv
# data/processed/supply/unmatched_stations_2026.csv
#
# 매칭 기준:
# 충전소명, 주소, 좌표 간 거리 100m 이내
#
# 주의:
# 매칭 후보는 검토용이며 최종 확정 결과가 아님
# ============================================================

# 1. 패키지 불러오기
source("scripts/supply/00_packages.R")

# 2. 입력 파일 확인
stopifnot(
  file.exists("data/raw/supply/station_2026.csv"),
  file.exists("data/raw/supply/station_petroleum_2026.csv")
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

gas <- read_csv(
  "data/raw/supply/station_2026.csv",
  locale = locale(encoding = "CP949"),
  show_col_types = FALSE
)

petroleum <- read_csv(
  "data/raw/supply/station_petroleum_2026.csv",
  show_col_types = FALSE
)

cat("가스안전공사 충전소:", nrow(gas), "\n")
cat("석유관리원 충전소:", nrow(petroleum), "\n")

# ============================================================
# 5. 충전소명 및 주소 정규화
# ============================================================

clean_text <- function(x) {
  x %>%
    str_to_lower() %>%
    str_replace_all("[^가-힣a-z0-9]", "")
}

gas_clean <- gas %>%
  mutate(
    gas_id = row_number(),
    gas_name = 충전소명,
    gas_address = 주소,
    name_key = clean_text(충전소명),
    address_key = clean_text(주소),
    gas_lat = as.numeric(위도),
    gas_lon = as.numeric(경도)
  )

petroleum_clean <- petroleum %>%
  mutate(
    petroleum_id = row_number(),
    petroleum_name = 충전소명,
    petroleum_address = 도로명주소,
    name_key = clean_text(충전소명),
    address_key = clean_text(도로명주소),
    petroleum_lat = as.numeric(위도),
    petroleum_lon = as.numeric(경도)
  )

# ============================================================
# 6. 충전소명 기준 매칭
# ============================================================

name_candidates <- gas_clean %>%
  filter(
    !is.na(name_key),
    name_key != ""
  ) %>%
  inner_join(
    petroleum_clean %>%
      filter(
        !is.na(name_key),
        name_key != ""
      ),
    by = "name_key",
    relationship = "many-to-many"
  ) %>%
  transmute(
    gas_id,
    petroleum_id,
    match_method = "name"
  )

# ============================================================
# 7. 주소 기준 매칭
# ============================================================

address_candidates <- gas_clean %>%
  filter(
    !is.na(address_key),
    address_key != ""
  ) %>%
  inner_join(
    petroleum_clean %>%
      filter(
        !is.na(address_key),
        address_key != ""
      ),
    by = "address_key",
    relationship = "many-to-many"
  ) %>%
  transmute(
    gas_id,
    petroleum_id,
    match_method = "address"
  )

# ============================================================
# 8. 좌표 거리 계산 함수
# ============================================================

# Haversine 공식 기반 대권거리 계산
# 거리 단위: 미터(m)

distance_m <- function(lat1, lon1, lat2, lon2) {
  r <- 6371000
  rad <- pi / 180
  
  a <- sin((lat2 - lat1) * rad / 2)^2 +
    cos(lat1 * rad) *
    cos(lat2 * rad) *
    sin((lon2 - lon1) * rad / 2)^2
  
  2 * r * asin(
    sqrt(pmin(1, pmax(0, a)))
  )
}

# ============================================================
# 9. 좌표 기준 매칭
# ============================================================

# 좌표 간 거리가 100m 이내인 충전소 후보 추출

geo_candidates <- crossing(
  gas_id = gas_clean$gas_id,
  petroleum_id = petroleum_clean$petroleum_id
) %>%
  left_join(
    gas_clean %>%
      select(gas_id, gas_lat, gas_lon),
    by = "gas_id"
  ) %>%
  left_join(
    petroleum_clean %>%
      select(
        petroleum_id,
        petroleum_lat,
        petroleum_lon
      ),
    by = "petroleum_id"
  ) %>%
  mutate(
    distance = distance_m(
      gas_lat,
      gas_lon,
      petroleum_lat,
      petroleum_lon
    )
  ) %>%
  filter(
    !is.na(distance),
    distance <= 100
  ) %>%
  transmute(
    gas_id,
    petroleum_id,
    match_method = "coordinate"
  )

# ============================================================
# 10. 매칭 후보 통합
# ============================================================

candidates <- bind_rows(
  name_candidates,
  address_candidates,
  geo_candidates
) %>%
  group_by(gas_id, petroleum_id) %>%
  summarise(
    match_method = paste(
      sort(unique(match_method)),
      collapse = "+"
    ),
    .groups = "drop"
  )

# ============================================================
# 11. 매칭 후보에 검토 정보 추가
# ============================================================

candidates <- candidates %>%
  left_join(
    gas_clean %>%
      select(
        gas_id,
        gas_name,
        gas_address,
        gas_lat,
        gas_lon
      ),
    by = "gas_id"
  ) %>%
  left_join(
    petroleum_clean %>%
      select(
        petroleum_id,
        petroleum_name,
        petroleum_address,
        petroleum_lat,
        petroleum_lon,
        충전가능차량,
        이용가능요일
      ),
    by = "petroleum_id"
  ) %>%
  mutate(
    distance_m = round(
      distance_m(
        gas_lat,
        gas_lon,
        petroleum_lat,
        petroleum_lon
      ),
      1
    )
  )

# ============================================================
# 12. 충전소별 매칭 후보 수 계산
# ============================================================

candidate_summary <- candidates %>%
  count(
    gas_id,
    name = "candidate_count"
  )

station_review <- gas_clean %>%
  left_join(
    candidate_summary,
    by = "gas_id"
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
# 13. 매칭 결과 검증
# ============================================================

stopifnot(
  nrow(station_review) == nrow(gas),
  !anyDuplicated(
    candidates[c("gas_id", "petroleum_id")]
  )
)

# ============================================================
# 14. 단일·복수·미매칭 후보 분류
# ============================================================

# 후보가 1개인 충전소
single_candidates <- candidates %>%
  group_by(gas_id) %>%
  filter(n() == 1) %>%
  ungroup() %>%
  mutate(
    review_status = "검토필요"
  )

# 후보가 2개 이상인 충전소
multiple_candidates <- candidates %>%
  group_by(gas_id) %>%
  filter(n() > 1) %>%
  ungroup()

# 후보가 없는 충전소
unmatched_stations <- station_review %>%
  filter(
    match_status == "미매칭"
  )

# ============================================================
# 15. 매칭 결과 저장
# ============================================================

write_excel_csv(
  candidates,
  "data/processed/supply/match_candidates_2026.csv"
)

write_excel_csv(
  station_review,
  "data/processed/supply/station_review_2026.csv"
)

write_excel_csv(
  single_candidates,
  "data/processed/supply/single_candidates_2026.csv"
)

write_excel_csv(
  multiple_candidates,
  "data/processed/supply/multiple_candidates_2026.csv"
)

write_excel_csv(
  unmatched_stations,
  "data/processed/supply/unmatched_stations_2026.csv"
)

# ============================================================
# 16. 매칭 결과 요약
# ============================================================

print(table(station_review$match_status))
print(table(candidates$match_method))

cat("기준 충전소 수:", nrow(station_review), "\n")
cat("매칭 후보 쌍:", nrow(candidates), "\n")

cat(
  "후보가 있는 충전소:",
  sum(station_review$candidate_count > 0),
  "\n"
)

cat(
  "단일 후보 충전소:",
  n_distinct(single_candidates$gas_id),
  "\n"
)

cat(
  "복수 후보 충전소:",
  n_distinct(multiple_candidates$gas_id),
  "\n"
)

cat(
  "미매칭 충전소:",
  nrow(unmatched_stations),
  "\n"
)

cat("2026년 수소충전소 기관 간 매칭 완료\n")

