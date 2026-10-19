library(tidyverse)

# 결과 저장 폴더 생성
dir.create(
  "results/integrated",
  recursive = TRUE,
  showWarnings = FALSE
)

# 수요·공급 불균형 데이터 불러오기
gap_data <- read_csv(
  "results/integrated/demand_supply_gap.csv",
  show_col_types = FALSE
)

# 필수 변수 확인
stopifnot(
  all(c("시도", "시군구", "잠재수요지수",
        "supply_index_01", "gap_index") %in% names(gap_data))
)

# 불균형지수 상위 20개 지역 추출
gap_top20 <- gap_data %>%
  arrange(desc(gap_index)) %>%
  slice_head(n = 20)

# 결과 저장
write_csv(
  gap_top20,
  "results/integrated/gap_top20.csv"
)

# 수요·공급 비교 그래프용 데이터
comparison_data <- gap_top20 %>%
  mutate(
    지역명 = paste(시도, 시군구),
    잠재수요 = 잠재수요지수 * 100,
    공급 = supply_index_01 * 100
  ) %>%
  select(지역명, 잠재수요, 공급) %>%
  pivot_longer(
    cols = c(잠재수요, 공급),
    names_to = "구분",
    values_to = "지수"
  ) %>%
  mutate(
    지역명 = factor(
      지역명,
      levels = rev(paste(gap_top20$시도, gap_top20$시군구))
    )
  )

# 그래프 생성
comparison_plot <- ggplot(
  comparison_data,
  aes(x = 지역명, y = 지수, fill = 구분)
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

# 그래프 저장
ggsave(
  "results/integrated/demand_supply_comparison.png",
  plot = comparison_plot,
  width = 11,
  height = 9,
  dpi = 300
)
