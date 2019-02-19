# 단어 24개와 유사단어 24개가 한 개씩 화면에 제시되었습니다. 참가자는 단어와 유사단어 중 
# 절반씩을 이전 세션에서 8번 반복 관찰하였습니다. 나머지 절반의 단어와 유사단어는
# 이번 처음 등장하였습니다. 참가자는 선호도 판단(1=like, 2=dislike)을 하였습니다. 

rm(list=ls())

# 가장 먼저 pacman 패키지를 설치하세요.

# 1. 자료를 불러옵니다.
file_url <- "https://raw.githubusercontent.com/cogneuro/rworkshop2019/master/data_NovSM_Exp2Word_SRC.csv"
SS <- read.csv(file_url)

# 2. 자료의 내용을 대략 확인합시다.
head(SS)
tail(SS)

pacman::p_load(psych)
headTail(SS)

# 3. 자료 유형
str(SS)
summary(SS)

pacman::p_load(tidyverse)
glimpse(SS)

# 4. 참가자는 몇 명일까?
SS$SID
unique(SS$SID)
length(unique(SS$SID))

# 5. 1번 참가자의 첫 시행 자료를 추출하세요.
SS[1,]

# 6. 1번 참가자의 반응시간을 추출하세요.
SS$RT[SS$SID==1]

SS[SS$SID==2 & SS$Trial==2,]

# 7. 1번 참가자의 정확율을 계산하세요.
unique(SS$Corr)

mean(SS$Corr[SS$SID==1])


# 8. 각 참가자들의 정확율을 계산하세요.
s.acc <- double()
xsn <- unique(SS$SID)
for (x in 1:length(unique(SS$SID))){
  s.acc[x] <- mean(SS$Pref[SS$SID==xsn[x]])
}
s.acc

# 9. 전체 시행 중 참가자가 응답하지 않은 비율은?
1-mean(SS$Corr)

# 10. 선호도(0=dislike, 1=like) 변수를 만드세요.

unique(SS$Resp)

SS$Pref <- ifelse(SS$Resp==1,1,0)
glimpse(SS)

# 11. 각 참가자들의 선호도를 계산하세요.
a.pref <- double()
xsn <- unique(SS$SID)
for (x in 1:length(unique(SS$SID))){
  a.pref[x] <- mean(SS$Pref[SS$SID==xsn[x]])
}
a.pref

hist(a.pref)
boxplot(a.pref, ylim=c(0,1))


# 12. 친숙성 요인과 반복 요인을 확인하세요.
table(SS$Familiarity, SS$SID)
table(SS$Repetition, SS$SID)


# 13. 네 가지 조건별 선호도를 계산하세요.

con.pref <- matrix(NA, nrow = 30, ncol = 4)
con.pref[,1] <- 1:30
con.pref[01:15,2] <- "Word"
con.pref[16:30,2] <- "Pseudoword"
xsubject <- unique(SS$SID)
for (xsn in 1:length(unique(SS$SID))){
      for (xrep in 1:2){
        con.pref[xsn, xrep+2] <- 
          mean(SS$Pref[SS$SID==xsubject[xsn] & SS$Repetition==xrep])
      }
}
con.pref


# 14. 요약 결과를 저장합시다.  
write.csv(con.pref, file = "x.csv", row.names = FALSE)
getwd() # setwd()

df.con.pref <- as.data.frame(con.pref)
colnames(df.con.pref) <- c("ID","Familiarity","Repeated","Unrepeated")
write.csv(df.con.pref, file = "x.csv", row.names = FALSE)


# 15. 친숙성과 반복을 요인으로 변환하세요.
SS$Familiarity = factor(SS$Familiarity, levels=c(1,2), labels=c("Words","Pseudowords"))
SS$Repetition = factor(SS$Repetition, levels=c(1,2), labels=c("Repeated","Unrepeated"))
glimpse(SS, width = 70)


# 16. 조건별 선호도를 그래프로 그리세요.
pacman::p_load(beeswarm)
pacman::p_load_gh("crsh/papaja")

apa_beeplot(data = SS,
            id="SID", dv="Pref", factors=c("Familiarity", "Repetition"), 
            dispersion = conf_int,
            ylim = c(0, 1),
            xlab = "Pre-experimental Stimulus Familiarity",
            ylab = "Preference Judgment (0 = dislike, 1 = like)",
            args_legend = list(title = 'Item Repetition',
                               x = "topright", inset = 0.05),
            las=1)

#-------EOF.