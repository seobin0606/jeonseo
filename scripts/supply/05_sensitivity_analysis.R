source("scripts/supply/00_packages.R")

# 통합 데이터
integrated <- read_csv(
  "data/processed/demand_supply_integrated.csv",
  show_col_types = FALSE
)

# 공급 데이터
supply <- read_csv(
  "results/supply/supply_index.csv",
  show_col_types = FALSE
)

# 데이터 크기
nrow(integrated)
nrow(supply)

# 컬럼 확인
names(integrated)
names(supply)

# 미확인 충전소 관련 변수 확인
integrated %>%
  select(
    any_of(c(
      "시도",
      "시군구",
      "passenger_station_count",
      "passenger_charger_count",
      "unknown_station_count",
      "unknown_charger_count",
      "missing_charger_count",
      "supply_index_01",
      "gap_index"
    ))
  ) %>%
  head()
#기본.확대 시나리오 계산
# 0~1 정규화 함수
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

# 민감도 분석
sensitivity <- integrated %>%
  mutate(
    # 기본 시나리오
    basic_supply_index = supply_index_01,
    basic_gap_index = gap_index,
    
    # 확대 시나리오: 미확인 시설 포함
    expanded_station_count =
      passenger_station_count + unknown_station_count,
    
    expanded_charger_count =
      passenger_charger_count + unknown_charger_count
  ) %>%
  mutate(
    expanded_supply_index =
      0.5 * normalize(expanded_station_count) +
      0.5 * normalize(expanded_charger_count),
    
    expanded_gap_index =
      잠재수요지수 - expanded_supply_index
  )
#시나리오별 순위 비교
sensitivity <- sensitivity %>%
  mutate(
    basic_rank = rank(
      -basic_gap_index,
      ties.method = "min"
    ),
    
    expanded_rank = rank(
      -expanded_gap_index,
      ties.method = "min"
    ),
    
    rank_change = expanded_rank - basic_rank
  ) %>%
  arrange(basic_rank)

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

# 기본 시나리오 상위 20개 지역 비교
print(rank_comparison, n = 20)


#민감도 분석 결과 그래프 생성
library(ggplot2)
library(tidyr)

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

print(p)


#결과 저장
dir.create(
  "results/supply",
  recursive = TRUE,
  showWarnings = FALSE
)

write_excel_csv(
  sensitivity,
  "results/supply/sensitivity_analysis.csv"
)

write_excel_csv(
  rank_comparison,
  "results/supply/rank_comparison.csv"
)

ggsave(
  "results/supply/sensitivity_comparison.png",
  plot = p,
  width = 10,
  height = 8,
  dpi = 300
)


#결과 확인
# 순위가 가장 크게 달라진 지역
rank_comparison %>%
  mutate(abs_change = abs(rank_change)) %>%
  arrange(desc(abs_change)) %>%
  slice_head(n = 10) %>%
  print(n = 10)

# 기본·확대 시나리오 각각 상위 5개
sensitivity %>%
  arrange(basic_rank) %>%
  select(시도, 시군구, basic_rank, expanded_rank) %>%
  slice_head(n = 5)

sensitivity %>%
  arrange(expanded_rank) %>%
  select(시도, 시군구, basic_rank, expanded_rank) %>%
  slice_head(n = 5)










rank_comparison %>%
  mutate(abs_change = abs(rank_change)) %>%
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



sensitivity %>%
  arrange(basic_rank) %>%
  select(
    시도,
    시군구,
    basic_rank,
    expanded_rank,
    passenger_station_count,
    unknown_station_count,
    passenger_charger_count,
    unknown_charger_count
  ) %>%
  slice_head(n = 10) %>%
  print(n = 10, width = Inf)

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

write_excel_csv(
  top10_sensitivity,
  "results/supply/top10_sensitivity.csv"
)


nrow(sensitivity)

sum(is.na(sensitivity$expanded_supply_index))
sum(is.na(sensitivity$expanded_gap_index))

sum(sensitivity$unknown_station_count)
sum(sensitivity$unknown_charger_count)
