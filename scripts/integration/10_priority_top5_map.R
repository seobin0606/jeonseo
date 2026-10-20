
# ============================================================
# 수소충전소 최적 입지 선정
# 10. 최종 우선 확충지역 및 기존 수소충전소 지도 시각화
#
# 목적:
# 최종 선정된 우선 확충지역 5곳의 공간적 분포와
# 기존 수소충전소 위치를 지도에서 비교
#
# 분석 방법:
# 1. 대한민국 시군구 행정구역 지도 불러오기
# 2. 최종 우선 확충지역 5곳 지도에 표시
# 3. 기존 수소충전소 좌표 및 승용차 충전 가능 여부 정리
# 4. 전국 및 경기도 지도 생성
# 5. 우선 확충지역과 기존 충전소를 함께 표시
#
# 입력:
# results/integrated/priority_top5.csv
# data/processed/supply/station_verified.csv
# data/raw/map/ (GADM 지도 데이터 저장 경로)
#
# 출력:
# results/integrated/priority_top5_map.png
# results/integrated/priority_top5_gyeonggi_map.png
# results/integrated/priority_top5_station_map.png
# results/integrated/priority_top5_gyeonggi_station_map.png
#
# 주의:
# 본 지도는 시군구 단위의 우선 확충지역과
# 기존 충전소의 위치를 나타낸 것으로,
# 실제 신규 충전소 설치 지점을 의미하지 않음
# ============================================================


# ============================================================
# STEP 1. 패키지 불러오기
# ============================================================

library(tidyverse)
library(sf)

# ============================================================
# STEP 2. 입력 및 출력 경로 설정
# ============================================================

priority_path <- "results/integrated/priority_top5.csv"

station_path <- "data/processed/supply/station_verified.csv"

map_path <- "data/raw/map"

output_dir <- "results/integrated"

dir.create(
  output_dir,
  recursive = TRUE,
  showWarnings = FALSE
)

dir.create(
  map_path,
  recursive = TRUE,
  showWarnings = FALSE
)

# 입력 파일 존재 여부 확인
input_paths <- c(priority_path, station_path)

if (!all(file.exists(input_paths))) {
  stop(
    "입력 파일을 찾을 수 없습니다:\n",
    paste(
      input_paths[!file.exists(input_paths)],
      collapse = "\n"
    )
  )
}

# ============================================================
# STEP 3. 대한민국 행정구역 지도 불러오기
# ============================================================

# GADM 대한민국 시군구 단위 행정구역
# 처음 실행 시 지도 데이터 다운로드가 필요할 수 있음

korea_map <- geodata::gadm(
  country = "KOR",
  level = 2,
  path = map_path
) %>%
  st_as_sf()

cat("지도 행정구역 수:", nrow(korea_map), "개\n")

# ============================================================
# STEP 4. 최종 우선 확충지역 데이터 불러오기
# ============================================================

priority_top5 <- read_csv(
  priority_path,
  show_col_types = FALSE
)

# 필수 변수 확인
stopifnot(
  all(
    c("시도", "시군구") %in% names(priority_top5)
  )
)

# 최종 선정 지역 수 확인
stopifnot(
  nrow(priority_top5) == 5
)

cat("\n===== 최종 우선 확충지역 =====\n")

priority_top5 %>%
  select(시도, 시군구) %>%
  print(n = 5)

# ============================================================
# STEP 5. 최종 5개 지역의 지도 명칭 연결
# ============================================================

# 현재 최종 선정지역은 모두 경기도에 위치
# GADM 지도에서는 시군구 이름이 영어로 표시됨
# 따라서 분석 결과의 한글 지역명을 영어 지도명으로 연결

region_mapping <- tibble(
  시도 = rep("경기도", 5),
  시군구 = c(
    "수원시",
    "용인시",
    "남양주시",
    "시흥시",
    "고양시"
  ),
  NAME_1 = rep("Gyeonggi-do", 5),
  NAME_2 = c(
    "Suwon",
    "Yongin",
    "Namyangju",
    "Siheung",
    "Goyang"
  )
)

# 최종 선정지역과 지도명 매칭
priority_regions <- priority_top5 %>%
  left_join(
    region_mapping,
    by = c("시도", "시군구")
  )

# 지도명 매핑 누락 확인
if (anyNA(priority_regions$NAME_2)) {
  stop(
    "최종 선정지역 중 지도명 매핑이 없는 지역이 있습니다."
  )
}

# ============================================================
# STEP 6. 지도에 우선 확충지역 표시
# ============================================================

priority_map <- korea_map %>%
  left_join(
    priority_regions %>%
      select(NAME_1, NAME_2) %>%
      mutate(selected = TRUE),
    by = c("NAME_1", "NAME_2")
  ) %>%
  mutate(
    selected = replace_na(selected, FALSE)
  )

# 정확히 5개 지역이 선택되었는지 확인
selected_count <- sum(priority_map$selected)

cat(
  "지도에서 선택된 우선 확충지역:",
  selected_count,
  "개\n"
)

stopifnot(selected_count == 5)

# ============================================================
# STEP 7. 전국 우선 확충지역 지도 생성
# ============================================================

p_map <- ggplot() +
  geom_sf(
    data = priority_map,
    aes(fill = selected),
    color = "gray65",
    linewidth = 0.15
  ) +
  scale_fill_manual(
    values = c(
      "FALSE" = "#F0F0F0",
      "TRUE" = "#E64B35"
    ),
    labels = c(
      "FALSE" = "기타 지역",
      "TRUE" = "우선 확충지역"
    ),
    name = "구분"
  ) +
  coord_sf(datum = NA) +
  labs(
    title = "자가용 수소승용차 충전 인프라 우선 확충지역",
    subtitle = "기본·확대 시나리오를 고려한 최종 5개 지역"
  ) +
  theme_void(base_size = 13) +
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold")
  )

print(p_map)

ggsave(
  filename = file.path(
    output_dir,
    "priority_top5_map.png"
  ),
  plot = p_map,
  width = 9,
  height = 10,
  dpi = 300
)

# ============================================================
# STEP 8. 경기도 우선 확충지역 확대 지도
# ============================================================

gyeonggi_map <- priority_map %>%
  filter(NAME_1 == "Gyeonggi-do")

p_gyeonggi <- ggplot() +
  geom_sf(
    data = gyeonggi_map,
    aes(fill = selected),
    color = "white",
    linewidth = 0.4
  ) +
  scale_fill_manual(
    values = c(
      "FALSE" = "#E8E8E8",
      "TRUE" = "#E64B35"
    ),
    guide = "none"
  ) +
  coord_sf(datum = NA) +
  labs(
    title = "경기도 우선 확충지역 5곳",
    subtitle = "수원시·용인시·남양주시·시흥시·고양시"
  ) +
  theme_void(base_size = 14)

print(p_gyeonggi)

ggsave(
  filename = file.path(
    output_dir,
    "priority_top5_gyeonggi_map.png"
  ),
  plot = p_gyeonggi,
  width = 9,
  height = 8,
  dpi = 300
)

# ============================================================
# STEP 9. 기존 수소충전소 데이터 불러오기
# ============================================================

station <- read_csv(
  station_path,
  show_col_types = FALSE
)

# 필수 변수 확인
station_required <- c(
  "latitude",
  "longitude",
  "passenger_status"
)

stopifnot(
  all(station_required %in% names(station))
)

cat(
  "\n전체 기존 충전소:",
  nrow(station),
  "개\n"
)

# ============================================================
# STEP 10. 충전소 좌표 정리
# ============================================================

# 좌표가 없거나 대한민국 범위를 벗어난 충전소 제외
station_clean <- station %>%
  mutate(
    latitude = as.numeric(latitude),
    longitude = as.numeric(longitude)
  ) %>%
  filter(
    !is.na(latitude),
    !is.na(longitude),
    between(latitude, 33, 39),
    between(longitude, 124, 132)
  )

cat(
  "좌표가 유효한 충전소:",
  nrow(station_clean),
  "개\n"
)

# ============================================================
# STEP 11. 충전소 공간 데이터 변환
# ============================================================

station_sf <- st_as_sf(
  station_clean,
  coords = c("longitude", "latitude"),
  crs = 4326,
  remove = FALSE
)

# 지도와 좌표계 통일
station_sf <- st_transform(
  station_sf,
  st_crs(priority_map)
)

# ============================================================
# STEP 12. 승용차 충전 가능 여부 분류
# ============================================================

station_sf <- station_sf %>%
  mutate(
    station_type = case_when(
      passenger_status == "가능_후보" ~
        "승용차 충전 가능 후보",
      
      passenger_status == "미확인" ~
        "미확인 시설",
      
      passenger_status == "불가능_후보" ~
        "승용차 충전 불가 후보",
      
      TRUE ~ "기타"
    )
  )

cat("\n===== 충전소 상태별 개수 =====\n")

station_sf %>%
  st_drop_geometry() %>%
  count(station_type) %>%
  print()

# ============================================================
# STEP 13. 전국 우선 확충지역 및 충전소 지도
# ============================================================

p_station_map <- ggplot() +
  geom_sf(
    data = priority_map,
    aes(fill = selected),
    color = "gray70",
    linewidth = 0.15
  ) +
  geom_sf(
    data = station_sf,
    aes(color = station_type),
    size = 1.3,
    alpha = 0.85
  ) +
  scale_fill_manual(
    values = c(
      "FALSE" = "#F0F0F0",
      "TRUE" = "#E64B35"
    ),
    name = "지역"
  ) +
  scale_color_manual(
    values = c(
      "승용차 충전 가능 후보" = "#2C7FB8",
      "미확인 시설" = "#F28E2B",
      "승용차 충전 불가 후보" = "#888888",
      "기타" = "#333333"
    ),
    name = "충전소 구분"
  ) +
  coord_sf(datum = NA) +
  labs(
    title = "수소충전소 우선 확충지역 및 기존 충전소 분포",
    subtitle = "최종 선정지역 5곳과 충전소 위치 비교"
  ) +
  theme_void(base_size = 13) +
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold")
  )

print(p_station_map)

ggsave(
  filename = file.path(
    output_dir,
    "priority_top5_station_map.png"
  ),
  plot = p_station_map,
  width = 10,
  height = 11,
  dpi = 300
)

# ============================================================
# STEP 14. 경기도 우선 확충지역 및 충전소 지도
# ============================================================

# 경기도에 위치한 충전소만 추출
gyeonggi_stations <- st_filter(
  station_sf,
  gyeonggi_map
)

p_gyeonggi_station <- ggplot() +
  geom_sf(
    data = gyeonggi_map,
    aes(fill = selected),
    color = "white",
    linewidth = 0.4
  ) +
  geom_sf(
    data = gyeonggi_stations,
    aes(color = station_type),
    size = 2,
    alpha = 0.9
  ) +
  scale_fill_manual(
    values = c(
      "FALSE" = "#E8E8E8",
      "TRUE" = "#E64B35"
    ),
    name = "지역"
  ) +
  scale_color_manual(
    values = c(
      "승용차 충전 가능 후보" = "#2C7FB8",
      "미확인 시설" = "#F28E2B",
      "승용차 충전 불가 후보" = "#888888",
      "기타" = "#333333"
    ),
    name = "충전소 구분"
  ) +
  coord_sf(datum = NA) +
  labs(
    title = "경기도 우선 확충지역과 수소충전소 분포",
    subtitle = "수원시·용인시·남양주시·시흥시·고양시"
  ) +
  theme_void(base_size = 13) +
  theme(
    legend.position = "bottom"
  )

print(p_gyeonggi_station)

ggsave(
  filename = file.path(
    output_dir,
    "priority_top5_gyeonggi_station_map.png"
  ),
  plot = p_gyeonggi_station,
  width = 10,
  height = 8,
  dpi = 300
)

# ============================================================
# STEP 15. 최종 결과 검증
# ============================================================

output_files <- c(
  "priority_top5_map.png",
  "priority_top5_gyeonggi_map.png",
  "priority_top5_station_map.png",
  "priority_top5_gyeonggi_station_map.png"
)

output_paths <- file.path(
  output_dir,
  output_files
)

cat("\n===== 지도 시각화 최종 결과 =====\n")

cat(
  "최종 선정 지역:",
  nrow(priority_top5),
  "개\n"
)

cat(
  "지도에 표시된 우선 확충지역:",
  sum(priority_map$selected),
  "개\n"
)

cat(
  "좌표가 유효한 기존 충전소:",
  nrow(station_sf),
  "개\n"
)

cat(
  "경기도 기존 충전소:",
  nrow(gyeonggi_stations),
  "개\n"
)

cat("\n===== 지도 파일 저장 확인 =====\n")

print(
  tibble(
    파일명 = output_files,
    저장여부 = file.exists(output_paths)
  )
)

cat("\n최종 우선 확충지역 지도 시각화 완료\n")
