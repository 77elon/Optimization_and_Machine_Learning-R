library(TSP)

# csv 파일로부터 시간 데이터 불러오기(data.frame type)
nw.tsp  <- read.csv("Optimization/04-14/result_shortest.csv", header=FALSE)
# UTF-8 Encoded, Windows 인코딩 문제로 건물 이름을 불러옴
name <- read.csv("Optimization/04-14/names.csv", header=FALSE)

# data.frame Cols name Deletion, data.frame의 열 이름을 제거해주는 작업
names(name) <- NULL
# TSP 함수에 사용하기 위해, data.frame에서 행렬로 바꿔줌
nw.tsp <- as.matrix(nw.tsp)
#nw.tsp %>% class

# Triangular Matrix + Transposed Triangular Matrix, 삼각행렬의 데이터를 전치함수와 더해줌으로, 0이 들어간 데이터를 시간 데이터로 바꿔준다.
nw.tsp <- nw.tsp + t(nw.tsp)
# 시간 데이터의 열 이름을 지정해줌.
colnames(nw.tsp) <- rownames(nw.tsp) <- as.character(name)
#TSP 함수에 데이터 전달
tsp <- TSP(nw.tsp) 
#노드 수 출력
n_of_cities(tsp) 
#labels(tsp)
# 함수를 통해 최단 거리 도출
tour <- solve_TSP(tsp) 
tour
# 도출된 최단 거리를 따로 저장
tour_route <- names(tour)
# 학생회관의 인덱스를 찾아냄.
idx <- which(tour_route == "학생회관")
# 학생회관 ~ 마지막 인덱스, 처음부터 학생회관 인덱스까지 출력함으로, 최단 경로를 알려줌.
cat(tour_route[idx:length(tour_route)], tour_route[1:idx]) 