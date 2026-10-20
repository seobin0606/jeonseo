
# ============================================================
# 수소충전소 최적 입지 선정
# 05. 수요·공급 불균형 분석
#
# 목적:
# 지역별 수소승용차 잠재수요와 충전 인프라 공급 수준을
# 비교하여 수요 대비 공급이 부족한 지역을 확인
#
# 분석 방법:
# 수요·공급 불균형지수 = 잠재수요지수 - 공급지수
# 불균형지수가 높은 지역을 우선적으로 확인
#
# 입력:
# data/processed/demand_supply_integrated.csv
#
# 출력:
# results/integrated/demand_supply_gap.csv
# results/integrated/gap_top20.csv
# results/integrated/gap_top20.png
#
# 주의:
# 본 스크립트는 기존 통합 데이터의 불균형지수를
# 이용하여 순위 확인 및 시각화를 수행
# ============================================================

# ============================================================
# STEP 1. 패키지 불러오기
# ============================================================

library(dplyr)
library(readr)
library(ggplot2)

# ============================================================
# STEP 2. 입력 파일 확인 및 불러오기
# ============================================================

input_path <- "data/processed/demand_supply_integrated.csv"

if (!file.exists(input_path)) {
  stop("수요·공급 통합 데이터 파일이 없습니다.")
}

integrated <- read_csv(
  input_path,
  show_col_types = FALSE
)

# 분석 지역 수 확인
cat("분석 지역 수:", nrow(integrated), "개\n")

# ============================================================
# STEP 3. 분석에 필요한 변수 확인
# ============================================================

required_columns <- c(
  "시도",
  "시군구",
  "잠재수요지수",
  "supply_index_01",
  "gap_index",
  "gap_rank",
  "passenger_station_count",
  "unknown_station_count"
)

missing_columns <- setdiff(
  required_columns,
  names(integrated)
)

if (length(missing_columns) > 0) {
  stop(
    "필요한 변수가 없습니다: ",
    paste(missing_columns, collapse = ", ")
  )
}

# 시도·시군구 기준 중복 지역 확인
stopifnot(
  !anyDuplicated(
    integrated[c("시도", "시군구")]
  )
)

# 불균형지수 결측치 확인
stopifnot(
  !anyNA(integrated$gap_index)
)

# ============================================================
# STEP 4. 수요·공급 불균형지수 기준 정렬
# ============================================================

# 불균형지수가 높은 지역부터 정렬
# 값이 클수록 잠재수요 대비 공급이 부족한 지역

gap_result <- integrated %>%
  arrange(desc(gap_index))

# ============================================================
# STEP 5. 불균형 상위 20개 지역 추출
# ============================================================

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

cat("\n===== 수요·공급 불균형 상위 20개 지역 =====\n")

print(gap_top20, n = 20)

# ============================================================
# STEP 6. 불균형 상위 20개 지역 시각화
# ============================================================

# 시도와 시군구를 결합하여 그래프용 지역명 생성
plot_data <- gap_top20 %>%
  mutate(
    지역 = paste(시도, 시군구),
    지역 = reorder(지역, gap_index)
  )

# 불균형지수 막대그래프 생성
p <- ggplot(
  plot_data,
  aes(x = 지역, y = gap_index)
) +
  geom_col(
    fill = "#FF6B6B",
    width = 0.7
  ) +
  coord_flip() +
  labs(
    title = "수소충전소 수요·공급 불균형 상위 20개 지역",
    subtitle = "잠재수요지수와 공급지수의 차이",
    x = "지역",
    y = "수요·공급 불균형지수"
  ) +
  theme_minimal(base_size = 12)

# 그래프 출력
print(p)

# ============================================================
# STEP 7. 분석 결과 저장
# ============================================================

dir.create(
  "results/integrated",
  recursive = TRUE,
  showWarnings = FALSE
)

# 전체 지역 불균형 분석 결과 저장
write_excel_csv(
  gap_result,
  "results/integrated/demand_supply_gap.csv"
)

# 불균형 상위 20개 지역 저장
write_excel_csv(
  gap_top20,
  "results/integrated/gap_top20.csv"
)

# 불균형 상위 20개 지역 그래프 저장
ggsave(
  filename = "results/integrated/gap_top20.png",
  plot = p,
  width = 10,
  height = 8,
  dpi = 300
)

# ============================================================
# STEP 8. 최종 분석 결과 검증
# ============================================================

cat("\n===== 수요·공급 불균형 분석 결과 =====\n")

cat("전체 분석 지역:", nrow(gap_result), "개\n")
cat("상위 지역:", nrow(gap_top20), "개\n")

cat(
  "불균형지수 결측치:",
  sum(is.na(gap_result$gap_index)),
  "개\n"
)

cat(
  "상위 20개 지역 중 공급지수 0인 지역:",
  sum(gap_top20$supply_index_01 == 0),
  "개\n"
)

cat("\n수요·공급 불균형 분석 완료\n")



