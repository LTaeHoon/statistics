# 통계학 입문 모델구축_로지스틱회귀분석 16.4 변수선택

#step 1 MASS library 불러오고 cement 데이터 사용
library(MASS)
cement

#step 2 전진선택법을 적용해보자
mylm1 <- lm(y~x4, data=cement)
addterm(mylm1,~.+x1, test = "F")
addterm(mylm1,~.+x2, test = "F")
addterm(mylm1,~.+x3, test = "F")

mylm2 <-lm(y~x1+x4, data=cement)
addterm(mylm2,~.+x2, test = "F")
addterm(mylm2,~.+x3, test = "F")

mylm3 <-lm(y~x1+x2+x4,data=cement)
addterm(mylm3,~.+x3,test = "F")


#step 3 후진선택법
droplm1 <- lm(y~x1+x2+x3+x4,data=cement)
dropterm(droplm1,test="F")
droplm2 <- lm(y~x1+x2+x4,data=cement)
dropterm(droplm2,test="F")
droplm3 <- lm(y~x1+x2,data=cement)
dropterm(droplm3,test="F")

#step 4 단계적선택법
steplm3.2 <- lm(y~x1+x2+x4, data=cement)
dropterm(steplm3.2, test="F")

#step 5 모형의 적합성 검사
stepwise <- lm(y~x1+x2,data=cement)
summary(stepwise)

forward <- lm(y~x1+x2+x4,data=cement)
summary(forward)
