# ==========================================
# 자가용 수소승용차 미래 잠재수요 분석
# 04. 지수 산출 및 민감도 분석
# 입력: 03_zone_mapping.R에서 생성한 최종 CSV 2개
# 출력: demand_final.csv, 민감도 분석 CSV, 상위 20개 지역 그래프
# ==========================================

library(tidyverse)

car_data <- read_csv("data/processed/car_registration_final.csv",show_col_types = FALSE)
traffic_data <- read_csv("data/processed/traffic_origin_2024_final.csv",show_col_types = FALSE)


# 시도·시군구를 공통 키로 두 자료를 결합한다.
demand_data <- car_data %>%
  left_join(traffic_data, by = c("시도", "시군구"))

# ============================================================
# 데이터 결합 결과 검증
# ============================================================

# 결합 후 분석지역 수 확인
nrow(demand_data)

# 중복 지역 검증
stopifnot(
  !anyDuplicated(demand_data[, c("시도", "시군구")])
)

# 자동차 등록대수 합계 유지 검증
stopifnot(
  isTRUE(all.equal(
    sum(car_data$자가용_승용차, na.rm = TRUE),
    sum(demand_data$자가용_승용차, na.rm = TRUE)
  ))
)

# KTDB 통행량 합계 유지 검증
stopifnot(
  isTRUE(all.equal(
    sum(traffic_data$승용차_발생통행량, na.rm = TRUE),
    sum(demand_data$승용차_발생통행량, na.rm = TRUE)
  ))
)


# 단위가 다른 등록대수와 통행량을 각각 0~1 범위로 Min-Max 정규화한다.
# 모든 값이 같을 때는 0으로 처리하여 0으로 나누는 오류를 피한다.

# ============================================================
# Min-Max 정규화
# ============================================================

# 서로 다른 단위의 변수를 0~1 범위로 정규화
min_max <- function(x) {
  
  if (max(x, na.rm = TRUE) == min(x, na.rm = TRUE)) {
    return(rep(0, length(x)))
  }
  
  (x - min(x, na.rm = TRUE)) /
    (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))
}

# 자동차 등록대수와 승용차 발생통행량 정규화
demand_normalized <- demand_data %>%
  mutate(
    자동차등록_정규화 = min_max(자가용_승용차),
    
    통행량_정규화 = min_max(승용차_발생통행량)
  )

# 정규화 결과 검증
stopifnot(
  !anyNA(demand_normalized$자동차등록_정규화),
  !anyNA(demand_normalized$통행량_정규화),
  all(demand_normalized$자동차등록_정규화 >= 0),
  all(demand_normalized$자동차등록_정규화 <= 1),
  all(demand_normalized$통행량_정규화 >= 0),
  all(demand_normalized$통행량_정규화 <= 1)
)

# ============================================================
# 잠재수요 지수 및 지역별 순위 산출
# ============================================================

# 자동차 등록대수와 발생통행량에 동일한 가중치 적용
demand_index <- demand_normalized %>%
  mutate(잠재수요지수 = 0.5 * 자동차등록_정규화 + 0.5 * 통행량_정규화)

# 잠재수요 지수 기준 내림차순 정렬 및 순위 부여
demand_rank <- demand_index %>%
  arrange(desc(잠재수요지수)) %>%
  mutate(잠재수요순위 = row_number())

# 잠재수요 분석 결과 저장
write_csv(demand_rank, "data/processed/demand_index_preliminary.csv")


# ============================================================
# 가중치 민감도 분석
# ============================================================

# 가중치를 달리해도 상위 지역이 유지되는지 민감도를 검토
# A: 등록 70/통행 30, B: 50/50, C: 등록 30/통행 70
demand_sensitivity <- demand_normalized %>%
  mutate(
    수요지수_A = 0.7 * 자동차등록_정규화 + 0.3 * 통행량_정규화,
    수요지수_B = 0.5 * 자동차등록_정규화 + 0.5 * 통행량_정규화,
    수요지수_C = 0.3 * 자동차등록_정규화 + 0.7 * 통행량_정규화
  ) %>%
  mutate(
    순위_A = min_rank(desc(수요지수_A)),
    순위_B = min_rank(desc(수요지수_B)),
    순위_C = min_rank(desc(수요지수_C))
  )



# ============================================================
# 가중치 민감도 분석 결과 비교
# ============================================================

# 시나리오별 순위 차이 계산
rank_comparison <- demand_sensitivity %>%
  mutate(
    순위차이_A_B = abs(순위_A - 순위_B),
    순위차이_B_C = abs(순위_B - 순위_C)
  ) %>%
  select(시도,시군구,
         순위_A,순위_B,순위_C,
         순위차이_A_B,순위차이_B_C
  )

# 시나리오별 순위 간 스피어만 상관계수 계산
rank_correlations <- cor(
  demand_sensitivity %>%
    select(순위_A, 순위_B, 순위_C),
  method = "spearman"
)

# 순위 상관계수 출력
print(rank_correlations)

# ============================================================
# 가중치 민감도 분석 결과 정리 및 저장
# ============================================================

# 각 가중치 시나리오에서 상위 10위에 들어간 횟수 계산
top10_stability <- demand_sensitivity %>%
  filter(순위_A <= 10 | 순위_B <= 10 | 순위_C <= 10) %>%
  mutate(
    상위10_포함횟수 =
      as.integer(순위_A <= 10) +
      as.integer(순위_B <= 10) +
      as.integer(순위_C <= 10)
  ) %>%
  select(
    시도, 시군구,
    순위_A, 순위_B, 순위_C,
    상위10_포함횟수
  ) %>%
  arrange(desc(상위10_포함횟수), 순위_B)

# 민감도 분석 결과 저장
write_csv(demand_sensitivity, "data/processed/demand_sensitivity.csv")

# 가중치별 순위 비교 결과 저장
write_csv(rank_comparison,"results/demand_rank_comparison.csv")

# 상위 10위 진입 안정성 분석 결과 저장 (새로 추가)
write_csv(top10_stability,"results/demand_top10_stability.csv")


# ============================================================
# 잠재수요 상위 20개 지역 시각화
# ============================================================

# 잠재수요 상위 20개 지역 추출
top20_demand <- demand_rank %>%
  slice_head(n = 20) %>%
  mutate(지역명 = paste(시도, 시군구))

# 잠재수요 지수 막대그래프 생성
p1 <- ggplot(
  top20_demand,
  aes(x = reorder(지역명, 잠재수요지수), y = 잠재수요지수)
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

# 그래프 저장
ggsave(filename = "results/demand_top20.png", plot = p1, width = 10, height = 8, dpi = 300)

# ============================================================
# 최종 잠재수요 분석 데이터 저장
# ============================================================

# 팀원의 공급 분석 데이터와 결합할 최종 수요 데이터 생성
demand_final <- demand_sensitivity %>%
  mutate(
    잠재수요지수 = 수요지수_B, 
    잠재수요순위 = 순위_B
  ) %>%
  select(
    기준월, 시도, 시군구, 자가용_승용차, 승용차_발생통행량,
    자동차등록_정규화, 통행량_정규화, 잠재수요지수, 잠재수요순위,
    수요지수_A, 수요지수_C, 순위_A, 순위_C
  ) %>%
  arrange(잠재수요순위)

# 최종 분석 결과 CSV 저장
write_csv(demand_final, "data/processed/demand_final.csv")

















































