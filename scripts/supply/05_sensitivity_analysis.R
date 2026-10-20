
# ============================================================
# 수소충전소 최적 입지 선정
# 05. 미확인 충전소 반영에 따른 공급 민감도 분석
#
# 목적:
# 승용차 충전 가능 여부가 미확인된 충전소를
# 공급에 포함했을 때 지역별 공급지수와
# 수요·공급 불균형 순위가 어떻게 변하는지 비교
#
# 분석 시나리오:
#
# 기본 시나리오:
#   승용차 충전 가능 후보만 공급에 반영
#
# 확대 시나리오:
#   승용차 충전 가능 후보 + 미확인 충전소 반영
#   (미확인 충전소가 모두 이용 가능하다고 가정)
#
# 입력:
# data/processed/demand_supply_integrated.csv
# results/supply/supply_index.csv
#
# 출력:
# results/supply/sensitivity_analysis.csv
# results/supply/rank_comparison.csv
# results/supply/top10_sensitivity.csv
# results/supply/sensitivity_comparison.png
#
# 주의:
# 확대 시나리오는 가정에 따른 민감도 분석이며
# 미확인 충전소의 실제 이용 가능 여부를 확정하지 않음
# ============================================================

# ============================================================
# STEP 1. 패키지 불러오기
# ============================================================

source("scripts/supply/00_packages.R")

library(ggplot2)

# ============================================================
# STEP 2. 입력 파일 경로 설정 및 확인
# ============================================================

integrated_path <- "data/processed/demand_supply_integrated.csv"
supply_path <- "results/supply/supply_index.csv"

input_paths <- c(integrated_path, supply_path)

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
# STEP 3. 데이터 불러오기
# ============================================================

# 수요·공급 통합 데이터
integrated <- read_csv(
  integrated_path,
  show_col_types = FALSE
)

# 공급지수 데이터
supply <- read_csv(
  supply_path,
  show_col_types = FALSE
)

cat("\n===== 입력 데이터 확인 =====\n")

cat("수요·공급 통합 지역:", nrow(integrated), "개\n")
cat("공급 분석 지역:", nrow(supply), "개\n")

# 공급 데이터는 252개 KTDB 지역 기준
stopifnot(nrow(supply) == 252)

# ============================================================
# STEP 4. 민감도 분석에 필요한 변수 검증
# ============================================================

required_columns <- c(
  "시도",
  "시군구",
  "잠재수요지수",
  "passenger_station_count",
  "passenger_charger_count",
  "unknown_station_count",
  "unknown_charger_count",
  "supply_index_01",
  "gap_index"
)

missing_columns <- setdiff(
  required_columns,
  names(integrated)
)

if (length(missing_columns) > 0) {
  stop(
    "통합 데이터에 필요한 변수가 없습니다:\n",
    paste(missing_columns, collapse = ", ")
  )
}

# 분석 지역 중복 확인
stopifnot(
  !anyDuplicated(
    integrated[c("시도", "시군구")]
  )
)

# ============================================================
# STEP 5. 공급지수 정규화 함수
# ============================================================

# 공급지수는 0~1 범위로 Min-Max 정규화
# 모든 지역의 값이 동일하면 0으로 처리

normalize <- function(x) {
  
  if (all(is.na(x))) {
    stop("정규화할 데이터가 모두 NA입니다.")
  }
  
  if (anyNA(x)) {
    stop("정규화할 데이터에 NA가 있습니다.")
  }
  
  if (max(x) == min(x)) {
    return(rep(0, length(x)))
  }
  
  (x - min(x)) / (max(x) - min(x))
}

# ============================================================
# STEP 6. 기본 및 확대 시나리오 계산
# ============================================================

sensitivity <- integrated %>%
  mutate(
    
    # ----------------------------------------
    # 기본 시나리오
    # ----------------------------------------
    # 기존 수요·공급 통합 분석 결과 사용
    # 승용차 충전 가능 후보만 공급에 반영
    
    basic_supply_index = supply_index_01,
    
    basic_gap_index = gap_index,
    
    # ----------------------------------------
    # 확대 시나리오
    # ----------------------------------------
    # 미확인 충전소가 모두 승용차 충전 가능하다고 가정
    
    expanded_station_count =
      passenger_station_count + unknown_station_count,
    
    expanded_charger_count =
      passenger_charger_count + unknown_charger_count
    
  ) %>%
  mutate(
    
    # 확대 시나리오 공급지수
    # 충전소 수와 충전기 수에 동일 가중치 적용
    
    expanded_supply_index =
      0.5 * normalize(expanded_station_count) +
      0.5 * normalize(expanded_charger_count),
    
    # 확대 시나리오 수요·공급 불균형지수
    # 값이 클수록 수요 대비 공급이 부족한 지역
    
    expanded_gap_index =
      잠재수요지수 - expanded_supply_index
  )

# ============================================================
# STEP 7. 시나리오별 불균형 순위 계산
# ============================================================

sensitivity <- sensitivity %>%
  mutate(
    
    # 기본 시나리오 불균형 순위
    basic_rank = rank(
      -basic_gap_index,
      ties.method = "min"
    ),
    
    # 확대 시나리오 불균형 순위
    expanded_rank = rank(
      -expanded_gap_index,
      ties.method = "min"
    ),
    
    # 양수: 확대 시나리오에서 순위 하락
    # 음수: 확대 시나리오에서 순위 상승
    # 0: 순위 변화 없음
    rank_change = expanded_rank - basic_rank
    
  ) %>%
  arrange(basic_rank)

# ============================================================
# STEP 8. 시나리오별 순위 비교 데이터 생성
# ============================================================

rank_comparison <- sensitivity %>%
  select(
    시도,
    시군구,
    basic_gap_index,
    basic_rank,
    expanded_gap_index,
    expanded_rank,
    rank_change,
    unknown_station_count
  )

cat("\n===== 기본 시나리오 상위 20개 지역 =====\n")

print(
  rank_comparison,
  n = 20
)

# ============================================================
# STEP 9. 기본 시나리오 상위 20개 지역 시각화
# ============================================================

# 기본 시나리오 상위 20개 지역을 기준으로
# 확대 시나리오에서 순위가 어떻게 달라지는지 비교

plot_data <- rank_comparison %>%
  slice_head(n = 20) %>%
  mutate(
    지역 = paste(시도, 시군구)
  ) %>%
  select(
    지역,
    basic_rank,
    expanded_rank
  ) %>%
  pivot_longer(
    cols = c(basic_rank, expanded_rank),
    names_to = "scenario",
    values_to = "rank"
  )

# 시나리오별 불균형 순위 막대그래프
p <- ggplot(
  plot_data,
  aes(
    x = reorder(지역, -rank),
    y = rank,
    fill = scenario
  )
) +
  geom_col(
    position = "dodge",
    width = 0.7
  ) +
  coord_flip() +
  scale_y_reverse() +
  labs(
    title = "수소충전소 공급 시나리오별 불균형 순위 비교",
    subtitle = "기본 시나리오 상위 20개 지역",
    x = "지역",
    y = "불균형 순위",
    fill = "시나리오"
  ) +
  theme_minimal()

# 그래프 출력
print(p)

# ============================================================
# STEP 10. 민감도 분석 결과 저장
# ============================================================

dir.create(
  "results/supply",
  recursive = TRUE,
  showWarnings = FALSE
)

# 시나리오별 전체 분석 결과
write_excel_csv(
  sensitivity,
  "results/supply/sensitivity_analysis.csv"
)

# 시나리오별 지역 순위 비교
write_excel_csv(
  rank_comparison,
  "results/supply/rank_comparison.csv"
)

# 순위 비교 그래프
ggsave(
  "results/supply/sensitivity_comparison.png",
  plot = p,
  width = 10,
  height = 8,
  dpi = 300
)

# ============================================================
# STEP 11. 순위 변화가 큰 지역 확인
# ============================================================

# 기본·확대 시나리오 간 순위 차이가 큰 지역 추출
# 미확인 충전소 반영에 따른 민감도가 높은 지역 확인

cat("\n===== 순위 변화가 큰 상위 10개 지역 =====\n")

rank_comparison %>%
  mutate(
    abs_change = abs(rank_change)
  ) %>%
  arrange(desc(abs_change)) %>%
  select(
    시도,
    시군구,
    basic_rank,
    expanded_rank,
    rank_change,
    unknown_station_count
  ) %>%
  slice_head(n = 10) %>%
  print(n = 10, width = Inf)

# ============================================================
# STEP 12. 기본·확대 시나리오 상위 5개 비교
# ============================================================

cat("\n===== 기본 시나리오 상위 5개 =====\n")

sensitivity %>%
  arrange(basic_rank) %>%
  select(
    시도,
    시군구,
    basic_rank,
    expanded_rank
  ) %>%
  slice_head(n = 5) %>%
  print(n = 5)

cat("\n===== 확대 시나리오 상위 5개 =====\n")

sensitivity %>%
  arrange(expanded_rank) %>%
  select(
    시도,
    시군구,
    basic_rank,
    expanded_rank
  ) %>%
  slice_head(n = 5) %>%
  print(n = 5)

# ============================================================
# STEP 13. 기본 시나리오 상위 10개 상세 비교
# ============================================================

# 상위 10개 지역의 공급량과 미확인 충전소 현황 비교

top10_sensitivity <- sensitivity %>%
  arrange(basic_rank) %>%
  select(
    시도,
    시군구,
    basic_rank,
    expanded_rank,
    basic_gap_index,
    expanded_gap_index,
    passenger_station_count,
    unknown_station_count,
    passenger_charger_count,
    unknown_charger_count
  ) %>%
  slice_head(n = 10)

# 결과 출력
print(
  top10_sensitivity,
  n = 10,
  width = Inf
)

# 상위 10개 상세 비교 데이터 저장
write_excel_csv(
  top10_sensitivity,
  "results/supply/top10_sensitivity.csv"
)

# ============================================================
# STEP 14. 최종 민감도 분석 결과 검증
# ============================================================

cat("\n===== 민감도 분석 검증 결과 =====\n")

# 분석 지역 수
cat(
  "분석 지역:",
  nrow(sensitivity),
  "개\n"
)

# 확대 공급지수 결측치
cat(
  "확대 공급지수 결측:",
  sum(is.na(sensitivity$expanded_supply_index)),
  "개\n"
)

# 확대 불균형지수 결측치
cat(
  "확대 불균형지수 결측:",
  sum(is.na(sensitivity$expanded_gap_index)),
  "개\n"
)

# 미확인 충전소 수
cat(
  "미확인 충전소:",
  sum(sensitivity$unknown_station_count),
  "개\n"
)

# 미확인 충전소의 충전기 수
cat(
  "미확인 충전기:",
  sum(sensitivity$unknown_charger_count),
  "개\n"
)

cat("\n민감도 분석 완료\n")

