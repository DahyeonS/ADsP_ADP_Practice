# 연관분석을 위한 간단한 데이터 생성
items <- c('chicken', 'coke', 'cider')
count <- sample(3, 100, replace = T)
transactionId <- c() # 거래 번호호
transactionItem <- c() # 아이템 번호('치킨', '콜라', '사이다')
for (i in 1 : 100) {
  current_transaction <- sample(items, count[i])
  for (j in 1 : length(current_transaction)) {
    transactionId <- c(transactionId, i)
    transactionItem <- c(transactionItem, current_transaction[j])
  }
}
# 거래 데이터 생성 완료
transaction <- data.frame(transactionId, transactionItem)
head(transaction, 4)
#   transactionId transactionItem
# 1             1            coke
# 2             1           cider
# 3             2            coke
# 4             3           cider
# 연관분석을 위한 apriori 패키지 호출
install.packages('arules')
library(arules)
# 데이터 전처리
transactionById <- split(transaction$transactionItem, transaction$transactionId)
# as와 파라미터 값으로 'transactions'(거래)를 사용하여 거래 데이터로 변환
transactionById_processed <- as(transactionById, 'transactions')
# 연관분석 실시
# parameter를 활용하여 최소 지지도(supp)와 최소 신뢰도(conf) 값을 설정할 수 있다.
# 최소 지지도 기본값은 0.1, 최소 신뢰도 기본값은 0.8이다.
result <- apriori(transactionById_processed, parameter = list(supp = 0.2, conf = 0.7))

# 몇 개의 규칙이 발견되었는지 확인할 수 있다.
result # set of 3 rules 
# 3개의 규칙이 발견되었다.
# inspect를 활용하여 규칙을 볼 수 있다.
inspect(result)
#     lhs                 rhs    support confidence coverage lift     count
# [1] {}               => {coke} 0.72    0.720      1.00     1.000000 72   
# [2] {chicken}        => {coke} 0.48    0.750      0.64     1.041667 48   
# [3] {chicken, cider} => {coke} 0.31    0.775      0.40     1.076389 31   
# 콜라의 구매확률은 72%다.
# 자기 자신에 대한 향상도는 1이다.
# 치킨과 콜라가 같이 구매될 확률은 48%다.
# '치킨 → 콜라'의 향상도가 1보다 크므로 치킨을 사면 콜라를 살 확률이 증가한다.
# 그 확률은 '치킨 → 콜라'의 신뢰도인 75%다. (치킨 구매 시 콜라를 구매할 확률)
# 치킨과 사이다와 콜라를 모두 구매할 확률은 31%다.
# '치킨, 사이다 → 콜라'의 향상도가 1보다 크므로 치킨, 사이다 구매 시 콜라를 살 확률이 증가한다.
# 그 확률은 '치킨, 사이다 → 콜라'의 신뢰도인 77%다.

# 위 결과는 sample에 의한 랜덤 데이터로 결과가 매번 다르다.