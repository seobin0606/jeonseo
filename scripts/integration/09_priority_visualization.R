
# ============================================================
# 수소충전소 최적 입지 선정
# 09. 최종 우선 확충지역 5곳 시각화
#
# 목적:
# 최종 선정된 5개 지역의 기본·확대 공급 시나리오별
# 불균형 순위와 잠재수요·공급지수를 비교
#
# 분석 방법:
# 1. 최종 우선 확충지역 5곳 데이터 불러오기
# 2. 기본·확대 시나리오별 불균형 순위 시각화
# 3. 잠재수요지수와 기본·확대 공급지수 시각화
#
# 입력:
# results/integrated/priority_top5.csv
#
# 출력:
# results/integrated/priority_top5_rank.png
# results/integrated/priority_top5_demand_supply.png
#
# 주의:
# 기존 분석 결과를 시각화하며
# 지역 선정 기준과 지수 계산 방법은 변경하지 않음
# ============================================================

# ============================================================
# STEP 1. 패키지 불러오기
# ============================================================

library(tidyverse)

# ============================================================
# STEP 2. 입력 및 출력 경로 설정
# ============================================================

input_path <- "results/integrated/priority_top5.csv"

rank_output <- "results/integrated/priority_top5_rank.png"

supply_output <- "results/integrated/priority_top5_demand_supply.png"

dir.create(
  "results/integrated",
  recursive = TRUE,
  showWarnings = FALSE
)

# 입력 파일 확인
if (!file.exists(input_path)) {
  stop(
    "최종 우선 확충지역 파일이 없습니다.\n",
    "08_priority_selection.R을 먼저 실행하세요."
  )
}

# ============================================================
# STEP 3. 최종 선정 데이터 불러오기
# ============================================================

priority_top5 <- read_csv(
  input_path,
  show_col_types = FALSE
)

cat("최종 선정 지역 수:", nrow(priority_top5), "개\n")

# ============================================================
# STEP 4. 필수 변수 및 데이터 검증
# ============================================================

required_columns <- c(
  "시도",
  "시군구",
  "priority_rank",
  "basic_rank",
  "expanded_rank",
  "잠재수요지수",
  "basic_supply_index",
  "expanded_supply_index"
)

missing_columns <- setdiff(
  required_columns,
  names(priority_top5)
)

if (length(missing_columns) > 0) {
  stop(
    "필요한 변수가 없습니다: ",
    paste(missing_columns, collapse = ", ")
  )
}

# 최종 선정 지역 수 검증
stopifnot(
  nrow(priority_top5) == 5
)

# 지역 중복 여부 검증
stopifnot(
  !anyDuplicated(
    priority_top5[c("시도", "시군구")]
  )
)

# 순위 및 지수 결측치 검증
stopifnot(
  !anyNA(priority_top5$basic_rank),
  !anyNA(priority_top5$expanded_rank),
  !anyNA(priority_top5$잠재수요지수),
  !anyNA(priority_top5$basic_supply_index),
  !anyNA(priority_top5$expanded_supply_index)
)

# 최종 우선순위 기준으로 정렬
priority_top5 <- priority_top5 %>%
  arrange(priority_rank)

# ============================================================
# STEP 5. 시나리오별 순위 그래프 데이터 생성
# ============================================================

rank_plot_data <- priority_top5 %>%
  mutate(
    지역 = paste(시도, 시군구),
    지역 = factor(
      지역,
      levels = rev(지역)
    )
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
  ) %>%
  mutate(
    scenario = recode(
      scenario,
      basic_rank = "기본 시나리오",
      expanded_rank = "확대 시나리오"
    )
  )

# ============================================================
# STEP 6. 시나리오별 순위 비교 그래프 생성
# ============================================================

p1 <- ggplot(
  rank_plot_data,
  aes(
    x = 지역,
    y = rank,
    fill = scenario
  )
) +
  geom_col(
    position = position_dodge(width = 0.75),
    width = 0.65
  ) +
  coord_flip() +
  scale_fill_manual(
    values = c(
      "기본 시나리오" = "#2C7FB8",
      "확대 시나리오" = "#F28E2B"
    )
  ) +
  labs(
    title = "최종 우선 확충지역 5곳의 시나리오별 순위",
    subtitle = "순위가 낮을수록 불균형이 큰 지역",
    x = NULL,
    y = "수요·공급 불균형 순위",
    fill = "시나리오"
  ) +
  theme_minimal(base_size = 13)

# 그래프 출력
print(p1)

# ============================================================
# STEP 7. 시나리오별 순위 그래프 저장
# ============================================================

ggsave(
  filename = rank_output,
  plot = p1,
  width = 10,
  height = 6,
  dpi = 300
)

cat("순위 비교 그래프 저장 완료:", rank_output, "\n")

# ============================================================
# STEP 8. 잠재수요·공급지수 그래프 데이터 생성
# ============================================================

supply_plot_data <- priority_top5 %>%
  mutate(
    지역 = paste(시도, 시군구),
    지역 = factor(
      지역,
      levels = rev(지역)
    )
  ) %>%
  select(
    지역,
    잠재수요지수,
    basic_supply_index,
    expanded_supply_index
  ) %>%
  pivot_longer(
    cols = c(
      잠재수요지수,
      basic_supply_index,
      expanded_supply_index
    ),
    names_to = "index_type",
    values_to = "index_value"
  ) %>%
  mutate(
    index_type = recode(
      index_type,
      잠재수요지수 = "잠재수요",
      basic_supply_index = "기본 공급",
      expanded_supply_index = "확대 공급"
    )
  )

# ============================================================
# STEP 9. 잠재수요·공급지수 비교 그래프 생성
# ============================================================

p2 <- ggplot(
  supply_plot_data,
  aes(
    x = 지역,
    y = index_value,
    fill = index_type
  )
) +
  geom_col(
    position = position_dodge(width = 0.8),
    width = 0.7
  ) +
  coord_flip() +
  scale_y_continuous(
    limits = c(0, 1)
  ) +
  scale_fill_manual(
    values = c(
      "잠재수요" = "#E15759",
      "기본 공급" = "#4E79A7",
      "확대 공급" = "#59A14F"
    )
  ) +
  labs(
    title = "최종 우선 확충지역의 잠재수요 및 공급지수 비교",
    subtitle = "기본·확대 공급 시나리오 비교",
    x = NULL,
    y = "정규화 지수 (0~1)",
    fill = "구분"
  ) +
  theme_minimal(base_size = 13)

# 그래프 출력
print(p2)

# ============================================================
# STEP 10. 잠재수요·공급지수 그래프 저장
# ============================================================

ggsave(
  filename = supply_output,
  plot = p2,
  width = 10,
  height = 6,
  dpi = 300
)

cat("수요·공급 비교 그래프 저장 완료:", supply_output, "\n")

# ============================================================
# STEP 11. 최종 결과 검증
# ============================================================

cat("\n===== 최종 시각화 결과 =====\n")

cat(
  "최종 선정 지역:",
  nrow(priority_top5),
  "개\n"
)

cat(
  "순위 그래프 데이터:",
  nrow(rank_plot_data),
  "행\n"
)

cat(
  "수요·공급 그래프 데이터:",
  nrow(supply_plot_data),
  "행\n"
)

cat(
  "순위 그래프 저장 여부:",
  file.exists(rank_output),
  "\n"
)

cat(
  "수요·공급 그래프 저장 여부:",
  file.exists(supply_output),
  "\n"
)

cat("\n최종 우선 확충지역 시각화 완료\n")

