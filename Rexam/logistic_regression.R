# 통계학 입문 모델구축_로지스틱회귀분석 16.3

#step 1 데이터파일  mtcars 내용을 확인하고 attach(해당 데이터 활성화)
mtcars
attach(mtcars)

#step 2 중량과 변속기에 관한 로지스틱회귀분석을 수행
logit1 <- glm(am~wt,family = binomial)
summary(logit1)

plot(am~wt, ylab = "변속기(am)", xlab="중량(wt)")
curve(exp(logit1$coefficients[1]+logit1$coefficients[2]*x)/(1+exp(logit1$coefficients[1]+logit1$coefficients[2]*x)),add=T)
title("로지스틱회귀")

#step 3  마력과 중량을 X변수로 하는 로지스틱 회귀분석을 수행하여 보자
logit2 <- glm(am~hp+wt,data=mtcars,family = binomial)
summary(logit2)
(test.statistics <- logit2$null.deviance - logit2$deviance)
(degree.freedom <- logit2$df.null - logit2$df.residual)
1-pchisq(test.statistics,degree.freedom)
logLik(logit2)

#step 4 마력 한 단위가 증가하면서 수동 변속기일 가능성의 오즈비를 구해보자
exp(0.036)

#step 5 중량 한 단위가 증가하면서 수동 변속기일 가능성의 오즈비를 구해보자
exp(-8.08)
