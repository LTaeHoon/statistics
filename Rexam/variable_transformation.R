#통계학 입문 로지스틱 회귀분석 예시 16.1

#모형구축 변수변환

#tree 데이터 확인
trees

#높이와 부피의 산점도를 그려서 선형관계 확인

attach(trees)
par(mfrow=c(1,2))
plot(Height,Volume)
plot(Girth,Volume)

#Volume과 Girth의 회귀식을 추정하자. 변수변환 없이 회귀식 추정
my_lm <- lm(Volume ~ Girth)
summary(my_lm)

#Volume자료의 회귀식 적합과 잔차 그림을 그려 곡선 형태가 나타나는 것을 확인
par(mfcol=c(1,3))
plot(Girth,Volume)
abline(my_lm)
plot(my_lm$fitted.values, my_lm$residuals)
abline(h=0)
plot(Girth, my_lm$residuals)
abline(h=0)

#Volume을 log(volume)로 치환하여 회귀식을 구해 적합성 확인
log_Volume = log(Volume)
my_lm_log <- lm(log_Volume~Girth)
summary(my_lm_log)

#log(Volume) 자료의 회귀식 적합과 잔차그림을 그려보자
par(mfcol=c(1,3))
plot(Girth,log_Volume)
abline(my_lm_log)
plot(my_lm_log$fitted.values, my_lm_log$residuals)
abline(h=0)
plot(Girth, my_lm_log$residuals)
abline(h=0)

#Girth를 log(Girth), Volume을 log(Volume)로 치환하여 회귀식을 구해보자
log_Girth = log(Girth)
my_lm_log1 <- lm(log_Volume~log_Girth)
summary(my_lm_log1)

#log(Girth), log(Volume)자료의 회귀식 적합과 잔차그림을 그려보자
par(mfcol=c(1,3))
plot(log_Girth,log_Volume)
abline(my_lm_log1)
plot(my_lm_log1$fitted.values, my_lm_log1$residuals)
abline(h=0)
plot(log_Girth, my_lm_log1$residuals)
abline(h=0)
