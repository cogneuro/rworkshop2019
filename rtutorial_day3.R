# 단어 24개와 유사단어 24개가 한 개씩 화면에 제시되었습니다. 
# 참가자는 단어와 유사단어 중 절반씩을 이전 세션에서 8번 
# 반복 관찰하였습니다. 나머지 절반의 단어와 유사단어는
# 이번 세션에 처음 등장하였습니다. 
# 참가자는 선호도 판단(1=like, 2=dislike)을 하였습니다. 

rm(list=ls())

# 패키지는 업로드 합니다.
pacman::p_load(tidyverse, papaja, psych)

# 원자료를 읽어옵니다.
file_url <- "https://raw.githubusercontent.com/cogneuro/rworkshop2019/master/data_NovSM_Exp2Word_SRC.csv"
SS <- read.csv(file_url)

# 선호도 변인(0=dislike, 1=like) 추가
SS$Pref <- ifelse(SS$Resp==1,1,0)
glimpse(SS)

# 개별참가자들의 조건별 선호도
mat.pref <- matrix(NA, nrow = 30, ncol = 4)
mat.pref[,1] <- 1:30
mat.pref[01:15,2] <- 1
mat.pref[16:30,2] <- 2
xsubject <- unique(SS$SID)
for (xsn in 1:length(unique(SS$SID))){
  for (xrep in 1:2){
    mat.pref[xsn, xrep+2] <- 
      mean(SS$Pref[SS$SID==xsubject[xsn] & SS$Repetition==xrep])
  }
}
df.pref <- as.data.frame(mat.pref)
colnames(df.pref) <- c("SID","Familiarity","Repeated","Unrepeated")
df.pref # SPSS/JASP에 필요한 포맷.

# 핵심 요인을 factor로 변환
SS$Familiarity = factor(SS$Familiarity, levels=c(2,1), labels=c("Pseudowords","Words"))
SS$Repetition = factor(SS$Repetition, levels=c(1,2), labels=c("Repeated","Unrepeated"))

df.pref$Familiarity = factor(df.pref$Familiarity, levels=c(2,1), labels=c("Pseudowords","Words"))

glimpse(SS)
glimpse(df.pref)

# 1. 참가자 전체의 조건별 선호도 평균을 계산하세요.
tot.pref <- matrix(NA, nrow = 4, ncol = 3)
tot.pref[,1] <- c("Words","Words","Pseudowords","Pseudowords")
tot.pref[,2] <- c("Repeated","Unrepeated","Repeated","Unrepeated")

tot.pref[1,3] <- mean(df.pref$Repeated[df.pref$Familiarity=="Words"])
tot.pref[2,3] <- mean(df.pref$Unrepeated[df.pref$Familiarity=="Words"])
tot.pref[3,3] <- mean(df.pref$Repeated[df.pref$Familiarity=="Pseudowords"])
tot.pref[4,3] <- mean(df.pref$Unrepeated[df.pref$Familiarity=="Pseudowords"])

tot.pref

# 2. long-format으로 변환하세요.
# https://r4ds.had.co.nz/tidy-data.html
long.pref <- gather(df.pref, key = "Repetition", value = "Pref", -c("SID", "Familiarity"))
headTail(long.pref)

# 3. 다음 두 코드를 비교해보세요.
apa_beeplot(data = SS,
            id="SID", dv="Pref", factors=c("Familiarity", "Repetition"), 
            dispersion = conf_int,
            ylim = c(0, 1),
            xlab = "Pre-experimental Stimulus Familiarity",
            ylab = "Preference Judgment (0 = dislike, 1 = like)",
            args_legend = list(title = 'Item Repetition',
                               x = "bottom", inset = 0.05),
            las=1)

apa_beeplot(data = long.pref,
            id="SID", dv="Pref", factors=c("Familiarity", "Repetition"), 
            dispersion = conf_int,
            ylim = c(0, 1),
            xlab = "Pre-experimental Stimulus Familiarity",
            ylab = "Preference Judgment (0 = dislike, 1 = like)",
            args_legend = list(title = 'Item Repetition',
                               x = "bottom", inset = 0.05),
            las=1)

# 4.다음을 비교하세요.
# https://dplyr.tidyverse.org
# https://wsyang.com/2014/02/introduction-to-dplyr/
  
( result1 <- filter(long.pref, Familiarity=="Words") )
( result2 <- select(result1, SID, Repetition, Pref) )

long.pref %>% filter(Familiarity=="Words") %>% select(SID, Repetition, Pref)

# 5. 방금 사용했던 방법으로 참가자들의 조건별 선호도를 다시 계산합니다.
sum1 <- SS %>% group_by(SID, Familiarity, Repetition) %>% 
  summarise(Pref = mean(Pref)) %>% 
  ungroup

# 6. 참가자 전체의 조건별 선호도 평균을 다시 계산하세요.
sum2 <- sum1 %>% 
  group_by(Familiarity, Repetition) %>%
  summarise(mean.Preference = mean(Pref)) %>% 
  ungroup

# 7. 2x2 mixed ANOVA
# https://cran.r-project.org/web/packages/afex/index.html
pacman::p_load(afex)

aov.pref <- aov_ez(id = "SID", dv = "Pref", data = SS, 
                      between = "Familiarity", within = "Repetition")
anova(aov.pref)
anova(aov.pref, es = "pes")

# 8. ANOVA type?
afex_options("type") # Type III

# 9. Post-hoc을 해봅시다. 
t.test(Pref ~ Repetition, sum1 %>% filter(Familiarity=="Words"), paired = TRUE)
t.test(Pref ~ Repetition, sum1 %>% filter(Familiarity=="Pseudowords"), paired = TRUE)

# https://cran.r-project.org/web/packages/emmeans/index.html
pacman::p_load(emmeans)

( aov.pref.emm <- emmeans(aov.pref, pairwise ~ Repetition | Familiarity) )
plot(aov.pref.emm, comparison = TRUE)

# 10. 반응시간을 요약해보세요.
cSS <- SS %>% filter(Corr ==1)
hist(cSS$RT)

tSS <- SS %>% filter(RT > 200 & RT < 10000) %>% 
  group_by(Familiarity, Repetition) %>% 
  nest() %>% 
  mutate(lbound = map(data, ~mean(.$RT)-3*sd(.$RT)),
         ubound = map(data, ~mean(.$RT)+3*sd(.$RT))) %>% 
  unnest(lbound, ubound) %>% 
  unnest(data) %>% 
  mutate(Outlier = (RT < lbound)|(RT > ubound)) %>% 
  filter(Outlier == FALSE) %>% 
  select(SID, Familiarity, Repetition, RT, ImgName)

## 11. Outlier로 제거된 시행의 비율은?
100 - 100*nrow(tSS)/nrow(cSS)

## 12. 조건별 반응시간을 그림으로 그려보세요.
apa_beeplot(data = tSS,
            id="SID", dv="RT", factors=c("Familiarity", "Repetition"), 
            dispersion = conf_int,
            # ylim = c(0, 1),
            xlab = "Pre-experimental Stimulus Familiarity",
            ylab = "Response Time (msec)",
            args_legend = list(title = 'Item Repetition',
                               x = "bottom", inset = 0.05),
            las=1)

# 13. 반응시간에 대하여 Linear Mixed Modeling을 해보자.
# https://www.sciencedirect.com/science/article/pii/S0749596X07001398
# https://cran.r-project.org/web/packages/afex/vignettes/introduction-mixed-models.pdf
# https://www.sciencedirect.com/science/article/pii/S0022249602000287
pacman::p_load(lme4)
lmer()

rt.full <- lmer(RT ~ Familiarity*Repetition + (1|SID) + (1|ImgName), tSS,
                control = lmerControl(optimizer = "bobyqa"))
summary(rt.full)

# https://stats.idre.ucla.edu/spss/faq/coding-systems-for-categorical-variables-in-regression-analysis-2/
afex::set_sum_contrasts() # sum-to-zero coding

rt.red1 <- lmer(RT ~ Familiarity+Repetition + (1|SID) + (1|ImgName), tSS,
                control = lmerControl(optimizer = "bobyqa"))
rt.red2 <- lmer(RT ~ Repetition + (1|SID) + (1|ImgName), tSS,
                control = lmerControl(optimizer = "bobyqa"))
rt.red3 <- lmer(RT ~ 1 + (1|SID) + (1|ImgName), tSS,
                control = lmerControl(optimizer = "bobyqa"))
anova(rt.full, rt.red1, rt.red2)


pacman::p_load(afex)
rt.m1 <- mixed(RT ~ Familiarity*Repetition + (1|SID) + (1|ImgName), tSS)
rt.m2 <- mixed(RT ~ Familiarity*Repetition + (Repetition|SID) + (1|ImgName), tSS)

anova(rt.m1, rt.m2)

# 14. 선호도에 대하여 generalized Linear Mixed Modeling을 해보자.
# https://www.sciencedirect.com/science/article/pii/S0749596X07001337
# https://ko.wikipedia.org/wiki/로지스틱_회귀

conf.full <- glmer(Pref ~ Familiarity*Repetition + (Repetition|SID) + (1|ImgName), 
                    SS, family=binomial(link="logit"))
conf.red1 <- glmer(Pref ~ Familiarity+Repetition + (Repetition|SID) + (1|ImgName), 
                   SS, family=binomial(link="logit"))
conf.red2 <- glmer(Pref ~ Repetition + (Repetition|SID) + (1|ImgName), 
                   SS, family=binomial(link="logit"))
conf.red3 <- glmer(Pref ~ 1 + (Repetition|SID) + (1|ImgName), 
                   SS, family=binomial(link="logit"))
anova(conf.full, conf.red1, conf.red2, conf.red3)


conf.m1 <- mixed(Pref ~ Familiarity*Repetition + (1|SID) + (1|ImgName), 
            SS, method = "LRT", family=binomial(link="logit"))
conf.m2 <- mixed(Pref ~ Familiarity*Repetition + (Repetition|SID) + (1|ImgName), 
                 SS, method = "LRT", family=binomial(link="logit"))

anova(conf.m1, conf.m2)

# 15. GLMM에 대해 Post-hoc을 해보자.
( conf.m1.emm <- emmeans(conf.m2, pairwise ~ Repetition | Familiarity) )
( conf.m1.emm <- emmeans(conf.m2, pairwise ~ Repetition | Familiarity, type = "response") )
plot(aov.pref.emm, comparison = TRUE)

#------------EOF.