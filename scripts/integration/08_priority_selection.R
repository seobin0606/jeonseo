library(tidyverse)

# --------------------------------------------------
# 1. 데이터 불러오기
# --------------------------------------------------

rank_data <- read_csv(
  "results/supply/rank_comparison.csv",
  show_col_types = FALSE
)

sensitivity_data <- read_csv(
  "results/supply/sensitivity_analysis.csv",
  show_col_types = FALSE
)

# --------------------------------------------------
# 2. 기본·확대 시나리오 통합
# --------------------------------------------------

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

# --------------------------------------------------
# 3. 보수적 순위 계산
# --------------------------------------------------

priority_data <- priority_data %>%
  mutate(
    conservative_rank = pmax(
      basic_rank,
      expanded_rank
    )
  )

# --------------------------------------------------
# 4. 최종 우선순위 정렬
# --------------------------------------------------

priority_data <- priority_data %>%
  arrange(
    conservative_rank,
    basic_rank,
    expanded_rank
  ) %>%
  mutate(
    priority_rank = row_number()
  )

# --------------------------------------------------
# 5. 최종 상위 5개 지역 선정
# --------------------------------------------------

priority_top5 <- priority_data %>%
  slice_head(n = 5)

# --------------------------------------------------
# 6. 결과 저장
# --------------------------------------------------

write_csv(
  priority_top5,
  "results/integrated/priority_top5.csv"
)

# --------------------------------------------------
# 7. 결과 확인
# --------------------------------------------------

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