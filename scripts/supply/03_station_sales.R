
# ============================================================
# 수소충전소 최적 입지 선정
# 03-2. 수소충전소 판매량 및 이용 실적 분석
#
# 목적:
# 충전소별 수소 판매량과 승용·상용 충전 기록을 집계하여
# 수소충전소 이용 수준을 보조적으로 분석
#
# 입력:
# data/raw/supply/station_sales_2025.csv
#
# 출력:
# data/processed/supply/station_sales_summary.csv
#
# 주의:
# 평균 판매량은 기록이 존재하는 날짜 기준으로 계산
# 승용(6kg이하)은 제공기관의 분류이며
# 개인용 수소승용차 이용량과 완전히 동일하지 않음
# ============================================================

# 1. 패키지 불러오기
source("scripts/supply/00_packages.R")

# 2. 입력 파일 확인
input_path <- "data/raw/supply/station_sales_2025.csv"

stopifnot(file.exists(input_path))

# 3. 저장 폴더 생성
dir.create(
  "data/processed/supply",
  recursive = TRUE,
  showWarnings = FALSE
)

# ============================================================
# 4. 판매량 데이터 불러오기
# ============================================================

sales <- read_csv(
  input_path,
  locale = locale(encoding = "CP949"),
  show_col_types = FALSE
)

cat("원본 판매 기록 수:", nrow(sales), "\n")

# ============================================================
# 5. 날짜 및 숫자형 변수 정리
# ============================================================

sales_clean <- sales %>%
  mutate(
    sale_date = as.Date(판매일),
    sale_year = as.integer(format(sale_date, "%Y")),
    
    hydrogen_kg = parse_number(
      as.character(`kg(H2)`)
    ),
    
    passenger_sessions = parse_number(
      as.character(`승용(6kg이하)`)
    ),
    
    commercial_sessions = parse_number(
      as.character(`상용(6kg초과)`)
    )
  )

# ============================================================
# 6. 데이터 검증
# ============================================================

# 날짜 변환 실패 여부
cat(
  "날짜 변환 실패 기록:",
  sum(is.na(sales_clean$sale_date)),
  "\n"
)

# 판매량 결측치
cat(
  "수소 판매량 결측 기록:",
  sum(is.na(sales_clean$hydrogen_kg)),
  "\n"
)

# 승용 충전 기록 결측치
cat(
  "승용 충전 기록 결측:",
  sum(is.na(sales_clean$passenger_sessions)),
  "\n"
)

# ============================================================
# 7. 충전소별·연도별 이용 실적 집계
# ============================================================

sales_summary <- sales_clean %>%
  group_by(
    `수소충전소`,
    sale_year
  ) %>%
  summarise(
    record_days = n_distinct(
      sale_date,
      na.rm = TRUE
    ),
    
    total_hydrogen_kg = sum(
      hydrogen_kg,
      na.rm = TRUE
    ),
    
    passenger_sessions = sum(
      passenger_sessions,
      na.rm = TRUE
    ),
    
    commercial_sessions = sum(
      commercial_sessions,
      na.rm = TRUE
    ),
    
    avg_kg_per_recorded_day = if_else(
      record_days > 0,
      total_hydrogen_kg / record_days,
      NA_real_
    ),
    
    .groups = "drop"
  )

# ============================================================
# 8. 결과 저장
# ============================================================

write_excel_csv(
  sales_summary,
  "data/processed/supply/station_sales_summary.csv"
)

# ============================================================
# 9. 결과 확인
# ============================================================

cat(
  "충전소별·연도별 집계 행 수:",
  nrow(sales_summary),
  "\n"
)

print(
  summary(sales_summary$total_hydrogen_kg)
)

cat("수소충전소 판매량 분석 완료\n")

