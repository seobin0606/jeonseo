
# ============================================================
# 수소충전소 최적 입지 선정
# 06. 수요·공급 불균형 상위 20개 지역 비교 시각화
#
# 목적:
# 수요·공급 불균형지수가 높은 상위 20개 지역의
# 잠재수요지수와 충전 인프라 공급지수를 비교하여
# 지역별 수요 대비 공급 부족 수준을 시각적으로 확인
#
# 분석 방법:
# 1. 불균형지수 기준 상위 20개 지역 선정
# 2. 잠재수요지수와 공급지수를 0~100점으로 변환
# 3. 지역별 두 지수를 나란히 표시하는 막대그래프 생성
#
# 입력:
# results/integrated/demand_supply_gap.csv
#
# 출력:
# results/integrated/demand_supply_comparison.png
#
# 주의:
# 본 스크립트는 기존 불균형 분석 결과를 시각화하며
# 잠재수요지수, 공급지수 및 불균형지수를 재계산하지 않음
# ============================================================

# ============================================================
# STEP 1. 패키지 불러오기
# ============================================================

library(tidyverse)

# ============================================================
# STEP 2. 입력 및 출력 경로 설정
# ============================================================

input_path <- "results/integrated/demand_supply_gap.csv"

output_path <- "results/integrated/demand_supply_comparison.png"

# 결과 저장 폴더 생성
dir.create(
  "results/integrated",
  recursive = TRUE,
  showWarnings = FALSE
)

# 입력 파일 존재 여부 확인
if (!file.exists(input_path)) {
  stop(
    "수요·공급 불균형 분석 파일이 없습니다.\n",
    "05_demand_supply_analysis.R을 먼저 실행하세요."
  )
}

# ============================================================
# STEP 3. 수요·공급 불균형 데이터 불러오기
# ============================================================

gap_data <- read_csv(
  input_path,
  show_col_types = FALSE
)

# 분석 지역 수 확인
cat("전체 분석 지역:", nrow(gap_data), "개\n")

# ============================================================
# STEP 4. 필수 변수 및 데이터 검증
# ============================================================

required_columns <- c(
  "시도",
  "시군구",
  "잠재수요지수",
  "supply_index_01",
  "gap_index"
)

missing_columns <- setdiff(
  required_columns,
  names(gap_data)
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
    gap_data[c("시도", "시군구")]
  )
)

# 지수 결측치 확인
stopifnot(
  !anyNA(gap_data$잠재수요지수),
  !anyNA(gap_data$supply_index_01),
  !anyNA(gap_data$gap_index)
)

# ============================================================
# STEP 5. 불균형지수 상위 20개 지역 추출
# ============================================================

# 불균형지수가 높은 순서대로 정렬
# 잠재수요 대비 공급 수준이 부족한 지역 우선 확인

gap_top20 <- gap_data %>%
  arrange(desc(gap_index)) %>%
  slice_head(n = 20)

cat("\n===== 불균형 상위 20개 지역 =====\n")

gap_top20 %>%
  select(
    시도,
    시군구,
    잠재수요지수,
    supply_index_01,
    gap_index
  ) %>%
  print(n = 20)

# ============================================================
# STEP 6. 수요·공급 비교 그래프용 데이터 생성
# ============================================================

# 잠재수요지수와 공급지수를 0~100점으로 변환
# 지수의 상대적인 크기를 쉽게 비교하기 위한 처리

comparison_data <- gap_top20 %>%
  mutate(
    지역명 = paste(시도, 시군구),
    
    잠재수요 = 잠재수요지수 * 100,
    
    공급 = supply_index_01 * 100
  ) %>%
  select(
    지역명,
    잠재수요,
    공급
  ) %>%
  pivot_longer(
    cols = c(잠재수요, 공급),
    names_to = "구분",
    values_to = "지수"
  ) %>%
  mutate(
    지역명 = factor(
      지역명,
      levels = rev(
        paste(gap_top20$시도, gap_top20$시군구)
      )
    )
  )

# ============================================================
# STEP 7. 수요·공급 비교 그래프 생성
# ============================================================

comparison_plot <- ggplot(
  comparison_data,
  aes(
    x = 지역명,
    y = 지수,
    fill = 구분
  )
) +
  geom_col(
    position = "dodge",
    width = 0.7
  ) +
  coord_flip() +
  scale_fill_manual(
    values = c(
      "잠재수요" = "#F28E8E",
      "공급" = "#4F81BD"
    )
  ) +
  labs(
    title = "수요·공급 불균형 상위 20개 지역 비교",
    subtitle = "잠재수요지수와 공급지수 비교 (0~100점)",
    x = "지역",
    y = "지수",
    fill = "구분"
  ) +
  theme_minimal(base_size = 12)

# 그래프 출력
print(comparison_plot)

# ============================================================
# STEP 8. 그래프 저장
# ============================================================

ggsave(
  filename = output_path,
  plot = comparison_plot,
  width = 11,
  height = 9,
  dpi = 300
)

cat("\n그래프 저장 완료:", output_path, "\n")

# ============================================================
# STEP 9. 최종 결과 검증
# ============================================================

cat("\n===== 수요·공급 비교 분석 결과 =====\n")

# 전체 분석 지역 수
cat("전체 분석 지역:", nrow(gap_data), "개\n")

# 상위 20개 지역 수
cat("비교 대상 지역:", nrow(gap_top20), "개\n")

# 그래프 데이터 행 수
# 20개 지역 × 2개 지수 = 40행
cat("그래프 데이터:", nrow(comparison_data), "행\n")

# 그래프 파일 생성 여부
cat(
  "그래프 저장 여부:",
  file.exists(output_path),
  "\n"
)

cat("\n수요·공급 비교 시각화 완료\n")

