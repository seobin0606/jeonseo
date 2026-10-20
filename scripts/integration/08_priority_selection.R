
# ============================================================
# 수소충전소 최적 입지 선정
# 08. 최종 우선 확충지역 5곳 선정
#
# 목적:
# 기본·확대 공급 시나리오의 수요·공급 불균형 순위를
# 종합하여 우선 확충지역 5곳을 선정
#
# 분석 방법:
# 1. 기본·확대 시나리오 분석 결과 결합
# 2. 두 시나리오 중 더 불리한 순위를 보수적 순위로 설정
# 3. 보수적 순위 → 기본 순위 → 확대 순위 순서로 정렬
# 4. 최종 우선 확충지역 5곳 선정
#
# 입력:
# results/supply/rank_comparison.csv
# results/supply/sensitivity_analysis.csv
#
# 출력:
# results/integrated/priority_top5.csv
#
# 주의:
# 본 결과는 시군구 단위의 우선 확충지역 선정이며,
# 실제 충전소 설치 지점은 추가 입지 분석이 필요
# ============================================================

# ============================================================
# STEP 1. 패키지 불러오기
# ============================================================

library(tidyverse)

# ============================================================
# STEP 2. 입력 및 출력 파일 경로 설정
# ============================================================

rank_path <- "results/supply/rank_comparison.csv"

sensitivity_path <- "results/supply/sensitivity_analysis.csv"

output_path <- "results/integrated/priority_top5.csv"

# 결과 저장 폴더 생성
dir.create(
  "results/integrated",
  recursive = TRUE,
  showWarnings = FALSE
)

# 입력 파일 존재 여부 확인
input_paths <- c(rank_path, sensitivity_path)

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
# STEP 3. 민감도 분석 결과 불러오기
# ============================================================

# 기본·확대 시나리오 불균형 순위
rank_data <- read_csv(
  rank_path,
  show_col_types = FALSE
)

# 기본·확대 공급지수 및 잠재수요지수
sensitivity_data <- read_csv(
  sensitivity_path,
  show_col_types = FALSE
)

cat("순위 데이터 지역 수:", nrow(rank_data), "개\n")

cat(
  "민감도 분석 지역 수:",
  nrow(sensitivity_data),
  "개\n"
)

# ============================================================
# STEP 4. 필수 변수 검증
# ============================================================

rank_required <- c(
  "시도",
  "시군구",
  "basic_rank",
  "expanded_rank",
  "unknown_station_count"
)

sensitivity_required <- c(
  "시도",
  "시군구",
  "잠재수요지수",
  "basic_supply_index",
  "expanded_supply_index"
)

if (!all(rank_required %in% names(rank_data))) {
  stop("순위 데이터에 필요한 변수가 없습니다.")
}

if (!all(sensitivity_required %in% names(sensitivity_data))) {
  stop("민감도 분석 데이터에 필요한 변수가 없습니다.")
}

# 지역 중복 검증
stopifnot(
  !anyDuplicated(rank_data[c("시도", "시군구")]),
  !anyDuplicated(sensitivity_data[c("시도", "시군구")])
)

# 순위 결측치 확인
stopifnot(
  !anyNA(rank_data$basic_rank),
  !anyNA(rank_data$expanded_rank)
)

# ============================================================
# STEP 5. 기본·확대 시나리오 데이터 통합
# ============================================================

priority_data <- rank_data %>%
  left_join(
    sensitivity_data %>%
      select(
        시도,
        시군구,
        잠재수요지수,
        basic_supply_index,
        expanded_supply_index
      ),
    by = c("시도", "시군구")
  )

# 결합 후 지역 수 검증
stopifnot(
  nrow(priority_data) == nrow(rank_data)
)

# 공급지수 및 잠재수요지수 결측치 검증
stopifnot(
  !anyNA(priority_data$잠재수요지수),
  !anyNA(priority_data$basic_supply_index),
  !anyNA(priority_data$expanded_supply_index)
)

# ============================================================
# STEP 6. 보수적 순위 계산
# ============================================================

# 두 시나리오 중 더 불리한 순위를 사용
# 예: 기본 3위, 확대 8위 → 보수적 순위 8위

priority_data <- priority_data %>%
  mutate(
    conservative_rank = pmax(
      basic_rank,
      expanded_rank
    )
  )

# ============================================================
# STEP 7. 최종 우선순위 정렬
# ============================================================

# 보수적 순위가 낮을수록 우선순위가 높음
# 보수적 순위가 같으면 기본 순위, 확대 순위 순으로 비교

priority_data <- priority_data %>%
  arrange(
    conservative_rank,
    basic_rank,
    expanded_rank
  ) %>%
  mutate(
    priority_rank = row_number()
  )

# ============================================================
# STEP 8. 최종 우선 확충지역 5곳 선정
# ============================================================

priority_top5 <- priority_data %>%
  slice_head(n = 5)

# ============================================================
# STEP 9. 최종 분석 결과 저장
# ============================================================

write_csv(
  priority_top5,
  output_path
)

cat("\n저장 완료:", output_path, "\n")

# ============================================================
# STEP 10. 최종 선정 결과 확인
# ============================================================

cat("\n===== 최종 우선 확충지역 5곳 =====\n")

priority_top5 %>%
  select(
    priority_rank,
    시도,
    시군구,
    잠재수요지수,
    basic_rank,
    expanded_rank,
    conservative_rank,
    unknown_station_count
  ) %>%
  print(n = 5)

# ============================================================
# STEP 11. 최종 결과 검증
# ============================================================

cat("\n===== 최종 검증 결과 =====\n")

cat(
  "전체 분석 지역:",
  nrow(priority_data),
  "개\n"
)

cat(
  "최종 선정 지역:",
  nrow(priority_top5),
  "개\n"
)

cat(
  "순위 결측치:",
  sum(is.na(priority_top5$conservative_rank)),
  "개\n"
)

cat(
  "파일 저장 여부:",
  file.exists(output_path),
  "\n"
)

cat("\n최종 우선 확충지역 선정 완료\n")
