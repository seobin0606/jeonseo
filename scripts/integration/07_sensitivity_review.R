
# ============================================================
# 수소충전소 최적 입지 선정
# 07. 공급 시나리오별 우선 확충 후보지역 검토
#
# 목적:
# 기본 시나리오에서 수요·공급 불균형이 큰 상위 20개
# 지역을 대상으로 미확인 충전소를 반영했을 때
# 불균형 순위가 어떻게 달라지는지 검토
#
# 분석 방법:
# 1. 기본 시나리오 상위 20개 지역 추출
# 2. 기본·확대 시나리오 순위 변화 계산
# 3. 확대 시나리오 순위에 따라 후보지역 분류
#
# 입력:
# results/supply/rank_comparison.csv
#
# 출력:
# results/integrated/priority_candidates.csv
#
# 주의:
# rank_stability는 확대 시나리오 순위 구간에 따른
# 분류이며, 실제 순위 변동 폭의 안정성을 의미하지 않음
# ============================================================

# ============================================================
# STEP 1. 패키지 불러오기
# ============================================================

library(tidyverse)

# ============================================================
# STEP 2. 입력 및 출력 파일 경로 설정
# ============================================================

rank_path <- "results/supply/rank_comparison.csv"

output_path <- "results/integrated/priority_candidates.csv"

# 결과 저장 폴더 생성
dir.create(
  "results/integrated",
  recursive = TRUE,
  showWarnings = FALSE
)

# 입력 파일 존재 여부 확인
if (!file.exists(rank_path)) {
  stop(
    "순위 비교 파일이 없습니다.\n",
    "05_sensitivity_analysis.R을 먼저 실행하세요."
  )
}

# ============================================================
# STEP 3. 민감도 분석 결과 불러오기
# ============================================================

# 기본·확대 시나리오의 지역별 불균형 순위
rank_data <- read_csv(
  rank_path,
  show_col_types = FALSE
)

cat("전체 분석 지역:", nrow(rank_data), "개\n")

# ============================================================
# STEP 4. 필수 변수 검증
# ============================================================

required_columns <- c(
  "시도",
  "시군구",
  "basic_rank",
  "expanded_rank",
  "rank_change"
)

missing_columns <- setdiff(
  required_columns,
  names(rank_data)
)

if (length(missing_columns) > 0) {
  stop(
    "분석에 필요한 변수가 없습니다: ",
    paste(missing_columns, collapse = ", ")
  )
}

# 지역 중복 여부 확인
stopifnot(
  !anyDuplicated(
    rank_data[c("시도", "시군구")]
  )
)

# 순위 결측치 확인
stopifnot(
  !anyNA(rank_data$basic_rank),
  !anyNA(rank_data$expanded_rank),
  !anyNA(rank_data$rank_change)
)

# ============================================================
# STEP 5. 기본 시나리오 상위 20개 지역 추출
# ============================================================

# 기본 시나리오 불균형 순위가 20위 이내인 지역
# 동순위가 있는 경우 20개보다 많을 수 있음

priority_candidates <- rank_data %>%
  filter(basic_rank <= 20) %>%
  mutate(
    
    # 기본·확대 시나리오 순위 차이의 절댓값
    abs_rank_change = abs(rank_change),
    
    # 확대 시나리오 순위에 따른 후보지역 분류
    rank_stability = case_when(
      expanded_rank <= 5 ~ "매우 안정",
      expanded_rank <= 10 ~ "안정",
      expanded_rank <= 20 ~ "검토 필요",
      TRUE ~ "순위 하락"
    )
  ) %>%
  arrange(basic_rank)

# ============================================================
# STEP 6. 후보지역 분석 결과 확인
# ============================================================

cat("\n===== 우선 확충 후보지역 검토 =====\n")

print(
  priority_candidates,
  n = Inf
)

# 확대 시나리오 순위 구간별 지역 수
cat("\n===== 후보지역 분류 결과 =====\n")

priority_candidates %>%
  count(rank_stability) %>%
  print()

# ============================================================
# STEP 7. 분석 결과 저장
# ============================================================

write_csv(
  priority_candidates,
  output_path
)

cat("\n저장 완료:", output_path, "\n")

# ============================================================
# STEP 8. 최종 분석 결과 검증
# ============================================================

cat("\n===== 최종 검증 결과 =====\n")

# 전체 분석 지역 수
cat(
  "전체 분석 지역:",
  nrow(rank_data),
  "개\n"
)

# 우선 확충 후보지역 수
cat(
  "후보지역:",
  nrow(priority_candidates),
  "개\n"
)

# 순위 변화 결측치 확인
cat(
  "순위 변화 결측:",
  sum(is.na(priority_candidates$abs_rank_change)),
  "개\n"
)

# 결과 파일 생성 확인
cat(
  "파일 저장 여부:",
  file.exists(output_path),
  "\n"
)

cat("\n우선 확충 후보지역 검토 완료\n")
