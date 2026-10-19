library(tidyverse)

# 저장 폴더 생성
dir.create(
  "results/integrated",
  recursive = TRUE,
  showWarnings = FALSE
)

# 최종 선정 파일 불러오기
priority_top5 <- read_csv(
  "results/integrated/priority_top5.csv",
  show_col_types = FALSE
)

# 그래프 데이터 생성
rank_plot_data <- priority_top5 %>%
  mutate(
    지역 = paste(시도, 시군구),
    지역 = factor(지역, levels = rev(지역))
  ) %>%
  select(지역, basic_rank, expanded_rank) %>%
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

# 그래프
p1 <- ggplot(
  rank_plot_data,
  aes(x = 지역, y = rank, fill = scenario)
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
    subtitle = "순위가 낮을수록 확충 우선순위가 높음",
    x = NULL,
    y = "우선 확충 순위",
    fill = "시나리오"
  ) +
  theme_minimal(base_size = 13)

print(p1)

ggsave(
  "results/integrated/priority_top5_rank.png",
  plot = p1,
  width = 10,
  height = 6,
  dpi = 300
)


# 그래프 데이터
supply_plot_data <- priority_top5 %>%
  mutate(
    지역 = paste(시도, 시군구),
    지역 = factor(지역, levels = rev(지역))
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

# 그래프
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
  scale_y_continuous(limits = c(0, 1)) +
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

print(p2)

ggsave(
  "results/integrated/priority_top5_demand_supply.png",
  plot = p2,
  width = 10,
  height = 6,
  dpi = 300
)
