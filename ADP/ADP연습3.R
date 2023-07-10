print(pi, digits = number) # [1] 3.141593 소수점 6번째 자리까지
cat(format(pi, digits = number), "\n") # 3.141593
options(digits = number); pi # [1] 3.141593

cat("pi:", pi, "\n", file = "test1.txt", append = T) # 파일에 출력
sink("test2.txt")
pi
sink()

list.files() # 파일 목록 출력
list.files(recursive = T, all.files = T) # 모든 파일 리스트

source("C:/10.R") # 디렉토리
write.csv(transaction, "test.csv", row.names = F) # csv 파일 생성
