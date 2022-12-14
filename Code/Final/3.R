library(dplyr)
library(TSP)


# csv 파일로부터 거리(km) 데이터 불러오기(data.frame type)
nw.tsp  <- read.csv("Optimization/Final/3.csv", header=TRUE)
nw.tsp <- nw.tsp[,-1]
rownames(nw.tsp) <- colnames(nw.tsp)
name <- colnames(nw.tsp)

#TSP 함수에 사용하기 위해, data.frame에서 행렬로 바꿔줌
nw.tsp <- as.matrix(nw.tsp)
#nw.tsp %>% class

name <- c('삼척시청','강원도청','강릉시청','동해시청','속초시청','원주시청','춘천시청','태백시청','고성군청','양구군청',
                '양양군청','영월군청','인제군청','정선군청','철원군청','평창군청','홍천군청','화천군청','횡성군청')

# 거리 데이터의 열 이름을 지정해주었다.
colnames(nw.tsp) <- rownames(nw.tsp) <- as.character(name)

# Triangular Matrix + Transposed Triangular Matrix, 삼각행렬의 데이터를 전치함수와 더해줌으로, 비어있는 삼각행렬에 데이터를 넣어준다.
nw.tsp <- nw.tsp + t(nw.tsp)

#TSP 함수에 데이터 전달
tsp <- TSP(nw.tsp) 

#노드 수 출력
n_of_cities(tsp) 
#labels(tsp)

# 함수를 통해 최단 거리 출력해준다.
tour <- solve_TSP(tsp) 
tour
# 도출된 최단 거리를 따로 저장하였다
tour_route <- names(tour)

# 삼척시청의 인덱스, 위치를 찾아냄.
idx <- which(tour_route == "삼척시청")
# 삼척시청부터 마지막 장소의 인덱스까지... 정렬해서 최단 경로를 알려주었다.
cat(tour_route[idx:length(tour_route)], tour_route[1:idx]) 



nw.tsp = matrix(c(0,208,59,16,119,170,208,47,147,117,102,108,160,79,292,138,183,237,156,
                  0,0,170,209,109,80,1,213,116,43,115,150,72,148,77,118,38,29,60,
                  0,0,0,50,70,120,159,99,97,151,49,117,110,84,244,88,134,187,110,
                  0,0,0,0,106,156,197,55,136,167,88,116,149,77,282,127,173,226,146,
                  0,0,0,0,0,170,130,158,25,66,16,171,49,139,178,143,105,117,150,
                  0,0,0,0,0,0,83,136,165,122,142,72,109,100,170,69,59,112,30,
                  0,0,0,0,0,0,0,213,118,43,126,149,74,147,77,123,38,33,60,
                  0,0,0,0,0,0,0,0,186,210,138,63,199,52,302,94,187,240,158,
                  0,0,0,0,0,0,0,0,0,75,53,199,57,166,180,171,109,125,142,
                  0,0,0,0,0,0,0,0,0,0,84,189,31,197,108,143,64,54,99,
                  0,0,0,0,0,0,0,0,0,0,0,151,56,119,199,123,89,143,134,
                  0,0,0,0,0,0,0,0,0,0,0,0,176,53,237,29,123,179,96,
                  0,0,0,0,0,0,0,0,0,0,0,0,0,143,136,127,53,82,86,
                  0,0,0,0,0,0,0,0,0,0,0,0,0,0,255,31,139,197,106,
                  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,214,122,60,148,
                  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,114,168,81,
                  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,66,33,
                  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,89,
                  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0), ncol = 19)
