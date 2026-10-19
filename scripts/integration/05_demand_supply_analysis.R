library(dplyr)
library(readr)

integrated <- read_csv(
  "data/processed/demand_supply_integrated.csv",
  show_col_types = FALSE
)

# 데이터 확인
nrow(integrated)
names(integrated)

#수요.공급 불균형 상위 20개 지역 확인
# 불균형 지수가 높은 순서대로 정렬
gap_result <- integrated %>%
  arrange(desc(gap_index))

# 상위 20개 지역 확인
gap_top20 <- gap_result %>%
  select(
    시도,
    시군구,
    잠재수요지수,
    supply_index_01,
    gap_index,
    gap_rank,
    passenger_station_count,
    unknown_station_count
  ) %>%
  slice_head(n = 20)

print(gap_top20, n = 20)

#상위 20개 지역 그래프 생성
library(ggplot2)

gap_top20 <- gap_top20 %>%
  mutate(
    지역 = paste(시도, 시군구),
    지역 = reorder(지역, gap_index)
  )

ggplot(
  gap_top20,
  aes(x = 지역, y = gap_index)
) +
  geom_col(fill = "#FF6B6B") +
  coord_flip() +
  labs(
    title = "수소충전소 수요·공급 불균형 상위 20개 지역",
    x = "지역",
    y = "수요·공급 격차 지수"
  ) +
  theme_minimal()

#분석 결과 저장
dir.create(
  "results/integrated",
  recursive = TRUE,
  showWarnings = FALSE
)

write_excel_csv(
  gap_result,
  "results/integrated/demand_supply_gap.csv"
)

write_excel_csv(
  gap_top20,
  "results/integrated/gap_top20.csv"
)

