library(tidyverse)

# 민감도 분석 결과 불러오기
rank_data <- read_csv(
  "results/supply/rank_comparison.csv",
  show_col_types = FALSE
)

sensitivity_data <- read_csv(
  "results/supply/sensitivity_analysis.csv",
  show_col_types = FALSE
)

# 기본 시나리오 상위 20개 지역의 순위 변화 분석
priority_candidates <- rank_data %>%
  filter(basic_rank <= 20) %>%
  mutate(
    abs_rank_change = abs(rank_change),
    rank_stability = case_when(
      expanded_rank <= 5 ~ "매우 안정",
      expanded_rank <= 10 ~ "안정",
      expanded_rank <= 20 ~ "검토 필요",
      TRUE ~ "순위 하락"
    )
  ) %>%
  arrange(basic_rank)

# 결과 확인
print(priority_candidates, n = 20)

# 결과 저장
write_csv(
  priority_candidates,
  "results/integrated/priority_candidates.csv"
)