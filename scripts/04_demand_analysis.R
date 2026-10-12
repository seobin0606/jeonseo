# STEP 39. 분석 데이터 불러오기
# ==========================================

library(tidyverse)

# 1. 자동차 등록 데이터
car_data <- read_csv(
  "data/processed/car_registration_final.csv",
  show_col_types = FALSE
)

# 2. KTDB 승용차 통행량 데이터
traffic_data <- read_csv(
  "data/processed/traffic_origin_2024_final.csv",
  show_col_types = FALSE
)

# 3. 데이터 구조 확인
glimpse(car_data)
glimpse(traffic_data)

# 4. 지역 수 확인
nrow(car_data)
nrow(traffic_data)

# STEP 40. 잠재수요 분석용 데이터 결합
# ==========================================

demand_data <- car_data %>%
  left_join(
    traffic_data,
    by = c("시도", "시군구")
  )

# 데이터 확인
glimpse(demand_data)

# 상위 10개 행 확인
head(demand_data, 10)

# 전체 지역 수
nrow(demand_data)

# STEP 41. 결합 데이터 검증
# ==========================================

# 1. 전체 지역 수
nrow(demand_data)

# 2. 자동차 등록대수 결측 확인
sum(is.na(demand_data$자가용_승용차))

# 3. 승용차 통행량 결측 확인
sum(is.na(demand_data$승용차_발생통행량))

# 4. 지역 중복 확인
demand_data %>%
  count(시도, 시군구) %>%
  filter(n > 1)

# 5. 자동차 등록대수 합계 유지 확인
all.equal(
  sum(car_data$자가용_승용차, na.rm = TRUE),
  sum(demand_data$자가용_승용차, na.rm = TRUE)
)

# 6. KTDB 통행량 합계 유지 확인
all.equal(
  sum(traffic_data$승용차_발생통행량, na.rm = TRUE),
  sum(demand_data$승용차_발생통행량, na.rm = TRUE)
)

# STEP 43. 잠재수요 변수 정규화
# ==========================================

# Min-Max 정규화 함수
min_max <- function(x) {
  
  if (max(x, na.rm = TRUE) == min(x, na.rm = TRUE)) {
    return(rep(0, length(x)))
  }
  
  (x - min(x, na.rm = TRUE)) /
    (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))
}

# 정규화
demand_normalized <- demand_data %>%
  mutate(
    자동차등록_정규화 = min_max(자가용_승용차),
    
    통행량_정규화 = min_max(승용차_발생통행량)
  )

# 결과 확인
demand_normalized %>%
  select(
    시도,
    시군구,
    자동차등록_정규화,
    통행량_정규화
  ) %>%
  head(10)

# STEP 44. 잠재수요 지수 산출
# ==========================================

demand_index <- demand_normalized %>%
  mutate(
    
    잠재수요지수 =
      0.5 * 자동차등록_정규화 +
      0.5 * 통행량_정규화
    
  )

# 잠재수요 높은 지역 순서로 정렬
demand_rank <- demand_index %>%
  arrange(desc(잠재수요지수)) %>%
  mutate(잠재수요순위 = row_number())

# 상위 20개 지역 확인
demand_rank %>%
  select(
    잠재수요순위,
    시도,
    시군구,
    자가용_승용차,
    승용차_발생통행량,
    잠재수요지수
  ) %>%
  head(20)

# STEP 45. 잠재수요 지수 검증
# ==========================================

# 1. 지역 수
nrow(demand_rank)

# 2. 잠재수요 지수 결측값
sum(is.na(demand_rank$잠재수요지수))

# 3. 잠재수요 지수 범위
range(demand_rank$잠재수요지수)

# 4. 잠재수요 지수 요약
summary(demand_rank$잠재수요지수)

# 5. 잠재수요 지수 상위 10개 지역
demand_rank %>%
  select(
    잠재수요순위,
    시도,
    시군구,
    잠재수요지수
  ) %>%
  head(10)

# STEP 46. 잠재수요 분석 결과 저장
# ==========================================

write_csv(
  demand_rank,
  "data/processed/demand_index_preliminary.csv"
)

# 저장 확인
file.exists("data/processed/demand_index_preliminary.csv")

# STEP 47. 가중치 민감도 분석
# ==========================================

demand_sensitivity <- demand_normalized %>%
  mutate(
    
    # A. 자동차 등록대수 중심
    수요지수_A =
      0.7 * 자동차등록_정규화 +
      0.3 * 통행량_정규화,
    
    # B. 동일 가중치
    수요지수_B =
      0.5 * 자동차등록_정규화 +
      0.5 * 통행량_정규화,
    
    # C. 통행량 중심
    수요지수_C =
      0.3 * 자동차등록_정규화 +
      0.7 * 통행량_정규화
    
  ) %>%
  mutate(
    
    순위_A = min_rank(desc(수요지수_A)),
    순위_B = min_rank(desc(수요지수_B)),
    순위_C = min_rank(desc(수요지수_C))
    
  )

# 결과 확인
head(demand_sensitivity, 10)

# STEP 47-2. 시나리오별 상위 지역 비교
# ==========================================

# A. 자동차 중심
demand_sensitivity %>%
  arrange(순위_A) %>%
  select(시도, 시군구, 순위_A, 수요지수_A) %>%
  head(10)

# B. 동일 가중치
demand_sensitivity %>%
  arrange(순위_B) %>%
  select(시도, 시군구, 순위_B, 수요지수_B) %>%
  head(10)

# C. 통행량 중심
demand_sensitivity %>%
  arrange(순위_C) %>%
  select(시도, 시군구, 순위_C, 수요지수_C) %>%
  head(10)

# STEP 48. 시나리오별 순위 변동 분석
# ==========================================

rank_comparison <- demand_sensitivity %>%
  mutate(
    
    # A와 B의 순위 차이
    순위차이_A_B = abs(순위_A - 순위_B),
    
    # B와 C의 순위 차이
    순위차이_B_C = abs(순위_B - 순위_C)
    
  ) %>%
  select(
    시도,
    시군구,
    순위_A,
    순위_B,
    순위_C,
    순위차이_A_B,
    순위차이_B_C
  )

# 순위 차이가 큰 지역 확인
rank_comparison %>%
  arrange(desc(순위차이_A_B)) %>%
  head(10)

# STEP 49. 순위 상관관계 분석
# ==========================================

cor(
  demand_sensitivity %>%
    select(순위_A, 순위_B, 순위_C),
  method = "spearman"
)

# STEP 50. 민감도 분석 결과 저장
# ==========================================

write_csv(
  demand_sensitivity,
  "data/processed/demand_sensitivity.csv"
)

write_csv(
  rank_comparison,
  "results/demand_rank_comparison.csv"
)

# 저장 확인
file.exists("data/processed/demand_sensitivity.csv")
file.exists("results/demand_rank_comparison.csv")

# STEP 51 상위 10개 지역의 안정성 확인
# ==========================================

top10_stability <- demand_sensitivity %>%
  filter(
    순위_A <= 10 |
      순위_B <= 10 |
      순위_C <= 10
  ) %>%
  mutate(
    상위10_포함횟수 =
      (순위_A <= 10) +
      (순위_B <= 10) +
      (순위_C <= 10)
  ) %>%
  select(
    시도,
    시군구,
    순위_A,
    순위_B,
    순위_C,
    상위10_포함횟수
  ) %>%
  arrange(
    desc(상위10_포함횟수),
    순위_B
  )

print(top10_stability, n = Inf)

# STEP 52. 잠재수요 상위 20개 지역 시각화
# ==========================================

library(ggplot2)

# 상위 20개 지역 추출
top20_demand <- demand_rank %>%
  slice_head(n = 20) %>%
  mutate(
    지역명 = paste(시도, 시군구)
  )

# 그래프 생성
p1 <- ggplot(
  top20_demand,
  aes(
    x = reorder(지역명, 잠재수요지수),
    y = 잠재수요지수
  )
) +
  geom_col(fill = "#4F81BD", width = 0.7) +
  coord_flip() +
  labs(
    title = "전국 잠재수요 상위 20개 지역",
    subtitle = "자가용 승용차 등록대수 50% + 승용차 발생통행량 50%",
    x = "지역",
    y = "잠재수요 지수"
  ) +
  theme_minimal(base_size = 12)

# 그래프 출력
print(p1)

# STEP 53. 잠재수요 그래프 저장
# ==========================================

ggsave(
  filename = "results/demand_top20.png",
  plot = p1,
  width = 10,
  height = 8,
  dpi = 300
)

# 저장 확인
file.exists("results/demand_top20.png")

# STEP 54. 수요 분석 결과 최종 정리
# ==========================================

demand_final <- demand_sensitivity %>%
  mutate(
    잠재수요지수 = 수요지수_B,
    잠재수요순위 = 순위_B
  ) %>%
  select(
    기준월,
    시도,
    시군구,
    자가용_승용차,
    승용차_발생통행량,
    자동차등록_정규화,
    통행량_정규화,
    잠재수요지수,
    잠재수요순위,
    수요지수_A,
    수요지수_C,
    순위_A,
    순위_C
  ) %>%
  arrange(잠재수요순위)

# 결과 확인
glimpse(demand_final)

# 상위 10개 지역 확인
head(demand_final, 10)

# STEP 55. 최종 데이터 검증
# ==========================================

# 1. 전체 지역 수
nrow(demand_final)

# 2. 결측값 확인
colSums(is.na(demand_final))

# 3. 지역 중복 확인
demand_final %>%
  count(시도, 시군구) %>%
  filter(n > 1)

# 4. 잠재수요 지수 범위
range(demand_final$잠재수요지수)

# 5. 잠재수요 순위 범위
range(demand_final$잠재수요순위)

# STEP 56. 최종 수요 데이터 저장
# ==========================================

write_csv(
  demand_final,
  "data/processed/demand_final.csv"
)

# 저장 확인
file.exists("data/processed/demand_final.csv")














































