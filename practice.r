## assignment 3

##(1) 데이터 분석 환경 설정 -> (2) 데이터 탐색 -> (3)데이터 클랜징(전처리) -> (4) 데이터 분석 

#-------- (1) 데이터 분석 환경 설정 --------
# Working Background Setting
getwd()
setwd("/Users/yangseongwoon/Desktop/DataForHW1")

# checking directory
dir()

# read excel
install.packages("readxl")
library(readxl)

# importing excel file
ksubs = read_excel("401ksubs.xls", col_names = F)
ksubs
# ---------(2) 데이터 탐색 ----------
nrow(ksubs)
head(ksubs)
tail(ksubs)
summary(ksubs)
#-------- 변수명 지정(R의 기능을 다양하게 쓰기 위하여 해봄) ---------
title = c("e401k     inc       marr      male      age       fsize     nettfa    p401k    pira      incsq     agesq")
title
# 변수명 정리를 위한 stringr 패키지 설치 및 라이브러리 
install.packages("stringr")
library("stringr")
# 변수명 사이에 빈공간 정리
title_cln <-str_squish(title)
title_cln
# 변수명 부여
colnames(ksubs) = c("e401k", "inc", "marr", "male", "age", "fsize", "nettfa", "p401k", "pira", "incsq", "agesq")
# 변수명 부여 됐는지 확인  -> data에서 ksubs 클릭 후 변수명이 잘 지정되었는지 확인


# checking missing value
# complete.cases 결측치가 없으면 TRUE, 결측치가 있으면 FALSE 
complete.cases(ksubs)
# 결측치가 없는 데이터의 합 9275
sum(complete.cases(ksubs)) 
# !complete.cases 결측치가 있으면 TRUE, 결측치가 없으면 FALSE
!complete.cases(ksubs)
sum(!complete.cases(ksubs)) 
# 결측치가 na인지 여부 반환 na인 경우 TRUE, 아닌 경우 False
is.na(ksubs)
# 결측치가 na인 경우 합 = 0 
sum(is.na(ksubs))
## 결측치 검정을 통해서 결측치가 없는 것으로 나타남

# ----------(3) 데이터 클랜징
# 데이터 추출 (효율적 데이터 핸들링을 위하여 dplyr 패키지 설치)
install.packages("dplyr")
library(dplyr)
# ----------(4) 데이터 분석
# marr=1이면서 fsize=2 인 조건 충족하는 경우
couplewithoutchild <- ksubs %>% filter(marr == 1 & fsize == 2)
couplewithoutchild
couplewithoutchild %>% summarise(n=n())

# Multiple Linear Regression
model <- lm(nettfa ~ inc + age, data=couplewithoutchild)
model
summary(model)
# confidence interval 95% 신뢰구간
confint(model, 'age', level = 0.95)

#age slope = 1.66470 & s.e= 0.16843, mu = 1
age.slope = 1.66470
age.se = 0.16843
mu = 1
age.tvalue = (age.slope-mu)/age.se
age.tvalue
n <- 1494
df = n-3
#양측검정
#critical value
qt(0.995,df)
#p-value
2*(1-pt(age.tvalue,df))
#단측검정
qt(0.99,df)
1-pt(age.tvalue,df)


#-----------개인적 호기심으로 작업한 것
#독립성가정의 평가
durbinWatsonTest(model)
# 더빈왓슨 검정통계량은 1.987813으로 2와 상당히 가까운 수로 오차의 독립성 가정이 만족한다. p-value가 0.7라는 점은 이러한 결론을 확인해준다. 확률이 0.05보다 크므로 이 검정통계량은 전혀 유의하지 않다. 
#다중공선성 평가
vif(model)
model %>% vif %>% mean
#inc과 age의 평균 vif 값이 1보다 큼으로 회귀모형이 편향될 가능성이 있다. 하지만 vif 값이 10보다 작기 때문에 크게 걱정할 문제는 없을 수 있다. 

# W-value가 0.91906이고 유의 확률이 0.05미만 정규성을 벗어남
# 1% significant level
# B2 =1 or B2 != 1
t.test(couplewithoutchild$age, var.equal=T, mu=1, alt ='two.sided', conf.level = 0.99)
t.test(couplewithoutchild$age, var.equal=F, mu=1, alt ='two.sided', conf.level = 0.99)

t.test(couplewithoutchild$age, var.equal=T, mu=1, alt ='greater', conf.level = 0.99)
t.test(couplewithoutchild$age, var.equal=F, mu=1, alt ='greater', conf.level = 0.99)
