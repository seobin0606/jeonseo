library(tidyverse)
library(sf)

# 1. 지도 데이터에 최종 5개 지역 표시
priority_map <- korea_map %>%
  mutate(
    selected = NAME_1 == "Gyeonggi-do" &
      NAME_2 %in% c(
        "Suwon",
        "Yongin",
        "Namyangju",
        "Siheung",
        "Goyang"
      )
  )

# 2. 정확히 5개 지역이 선택됐는지 확인
sum(priority_map$selected)

# 3. 전국 지도
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

# 4. 지도 저장
dir.create(
  "results/integrated",
  recursive = TRUE,
  showWarnings = FALSE
)

ggsave(
  "results/integrated/priority_top5_map.png",
  plot = p_map,
  width = 9,
  height = 10,
  dpi = 300
)

# 경기도 행정구역 추출
gyeonggi_map <- priority_map %>%
  filter(NAME_1 == "Gyeonggi-do")

# 경기도 확대 지도
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
  "results/integrated/priority_top5_gyeonggi_map.png",
  plot = p_gyeonggi,
  width = 9,
  height = 8,
  dpi = 300
)

library(tidyverse)
library(sf)

# 1. 대한민국 지도 불러오기
korea_map <- geodata::gadm(
  country = "KOR",
  level = 2,
  path = "data/raw/map"
) %>%
  st_as_sf()

# 2. 최종 우선 확충지역 불러오기
priority_top5 <- read_csv(
  "results/integrated/priority_top5.csv",
  show_col_types = FALSE
)

# 3. 기존 수소충전소 불러오기
station <- read_csv(
  "data/processed/supply/station_verified.csv",
  show_col_types = FALSE
)

# 4. 좌표가 있는 충전소만 추출
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

# 5. sf 공간 데이터로 변환
station_sf <- st_as_sf(
  station_clean,
  coords = c("longitude", "latitude"),
  crs = 4326,
  remove = FALSE
)

# 6. 충전소 상태별 개수 확인
table(station_sf$passenger_status, useNA = "ifany")


# 지도 데이터에 최종 선정지역 표시
priority_map <- korea_map %>%
  mutate(
    selected = NAME_1 == "Gyeonggi-do" &
      NAME_2 %in% c(
        "Suwon",
        "Yongin",
        "Namyangju",
        "Siheung",
        "Goyang"
      )
  )

# 선택된 지역 개수 확인
sum(priority_map$selected)

# 상태별 지도 범례
station_sf <- station_sf %>%
  mutate(
    station_type = case_when(
      passenger_status == "가능_후보" ~ "승용차 충전 가능 후보",
      passenger_status == "미확인" ~ "미확인 시설",
      passenger_status == "불가능_후보" ~ "승용차 충전 불가 후보",
      TRUE ~ "기타"
    )
  )

# 지도
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

# 결과 저장
dir.create(
  "results/integrated",
  recursive = TRUE,
  showWarnings = FALSE
)

ggsave(
  "results/integrated/priority_top5_station_map.png",
  plot = p_station_map,
  width = 10,
  height = 11,
  dpi = 300
)
gyeonggi_map <- priority_map %>%
  filter(NAME_1 == "Gyeonggi-do")

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
  "results/integrated/priority_top5_gyeonggi_station_map.png",
  plot = p_gyeonggi_station,
  width = 10,
  height = 8,
  dpi = 300
)

