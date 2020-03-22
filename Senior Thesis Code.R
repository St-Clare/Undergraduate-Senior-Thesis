#### Senior Thesis: Examining Individual Perceptions
#### Towards Collective Guilt Political Behavior
#### Jacob N. Oliver
#### Department of Political Science
#### University of Colorado, Boulder
#### Spring 2020
setwd("C:/Users/olive/Dropbox/Senior Thesis/MTurk")
getwd()
guilt_no_support_no <- read.csv("guilt(no) support(no).csv", TRUE, ",")
guilt_yes_support_no <- read.csv("guilt(yes) support(no).csv", TRUE, ",")
guilt_no_support_yes <- read.csv("guilt(no) support(yes).csv", TRUE, ",")
guilt_yes_support_yes <- read.csv("guilt(yes) support(yes).csv", TRUE, ",")
guilt_no_support_no_num <- read.csv("guilt(no) support(no)_num.csv", TRUE, ",")
guilt_yes_support_no_num <- read.csv("guilt(yes) support(no)_num.csv", TRUE, ",")
guilt_no_support_yes_num <- read.csv("guilt(no) support(yes)_num.csv", TRUE, ",")
guilt_yes_support_yes_num <- read.csv("guilt(yes) support(yes)_num.csv", TRUE, ",")
library(ggplot2)
library(psych)
library(readxl)
library(tidyverse)
library(stargazer)
#### Legitimacy bar graphs for all races
Legitimacy_Data <- read_excel("C:/Users/olive/Dropbox/Senior Thesis/MTurk/Legitimacy Data All Races.xlsx")
cell1 <- subset(Legitimacy_Data, TestGroup == 'Control' & Change == 'No')
cell2 <- subset(Legitimacy_Data, TestGroup == 'Control' & Change == 'Yes')
cell3 <- subset(Legitimacy_Data, TestGroup == 'GuiltTreatment' & Change == 'No')
cell4 <- subset(Legitimacy_Data, TestGroup == 'GuiltTreatment' & Change == 'Yes')
a <- describe(cell1$LegitimacyScore)
b <- describe(cell2$LegitimacyScore)
c <- describe(cell3$LegitimacyScore)
d <- describe(cell4$LegitimacyScore)
a
legitdat <- data.frame(TestGroup = c("Control", "Control", "Guilt Treatment", "Guilt Treatment"), Change = c("No", "Yes", "No", "Yes"), Means = c(a$mean, b$mean, c$mean, d$mean), Serrs = c(a$se, b$se, c$se, d$se))
legitdat
legitbar <- ggplot(data=legitdat, aes(x = TestGroup, y=Means, fill = Change))
legitbar <- legitbar + geom_bar(stat="identity", position = position_dodge()) + geom_errorbar(aes(ymin=Means-2*Serrs, ymax=Means+2*Serrs), width=.2, position=position_dodge(.9)) + labs(x="Test Group", y="Perceived Legitimacy", title="Perceived Legitimacy of Elite Collective Guilt to Change School Curricula")+scale_fill_manual(values=c('darkgray', 'lightgray')) + theme_classic()
legitbar
legitbar <- legitbar +theme(axis.title.x = element_text(size=14, face="bold"), axis.title.y = element_text(size=14, face="bold"), legend.title = element_text(size=14, face="bold"), axis.title = element_text(size=14, face="bold"))
legitbar
#### Legitimacy bar graphs for whites only
Legitimacy_Data_White <- read_excel("Legitimacy Data White.xlsx")
View(Legitimacy_Data_White)
cell5 <- subset(Legitimacy_Data_White, TestGroup == 'Control' & Change == 'No')
cell6 <- subset(Legitimacy_Data_White, TestGroup == 'Control' & Change == 'Yes')
cell7 <- subset(Legitimacy_Data_White, TestGroup == 'GuiltTreatment' & Change == 'No')
cell8 <- subset(Legitimacy_Data_White, TestGroup == 'GuiltTreatment' & Change == 'Yes')
e <- describe(cell5$LegitimacyScore)
f <- describe(cell6$LegitimacyScore)
g <- describe(cell7$LegitimacyScore)
h <- describe(cell8$LegitimacyScore)
e
legitdat_white <- data.frame(TestGroup = c("Control", "Control", "Guilt Treatment", "Guilt Treatment"), Change = c("No", "Yes", "No", "Yes"), Means = c(e$mean, f$mean, g$mean, h$mean), Serrs = c(e$se, f$se, g$se, h$se))
legitdat_white
legitbar_white <- ggplot(data=legitdat_white, aes(x = TestGroup, y=Means, fill = Change))
legitbar_white <- legitbar_white + geom_bar(stat="identity", position = position_dodge()) + geom_errorbar(aes(ymin=Means-2*Serrs, ymax=Means+2*Serrs), width=.2, position=position_dodge(.9)) + labs(x="Test Group", y="Perceived Legitimacy", title="Whites' Perceived Legitimacy of State Policy")+scale_fill_manual(values=c('darkgray', 'lightgray')) + theme_classic()
legitbar_white
legitbar_white <- legitbar_white +theme(axis.title.x = element_text(size=14, face="bold"), axis.title.y = element_text(size=14, face="bold"), legend.title = element_text(size=14, face="bold"), axis.title = element_text(size=14, face="bold"))
legitbar_white
#### White guilt perception index and plot
White_Guilt_Perception <- read_excel("White Guilt Perception.xlsx")
cell9 <- subset(White_Guilt_Perception, TestGroup == 'Control' & Change == 'No')
cell10 <- subset(White_Guilt_Perception, TestGroup == 'Control' & Change == 'Yes')
cell11 <- subset(White_Guilt_Perception, TestGroup == 'GuiltTreatment' & Change == 'No')
cell12 <- subset(White_Guilt_Perception, TestGroup == 'GuiltTreatment' & Change == 'Yes')
i <- describe(cell9$GuiltScore)
j <- describe(cell10$GuiltScore)
k <- describe(cell11$GuiltScore)
l <- describe(cell12$GuiltScore)
i
guiltdat_white <- data.frame(TestGroup = c("Control", "Control", "Guilt Treatment", "Guilt Treatment"), Change = c("No", "Yes", "No", "Yes"), Means = c(i$mean, j$mean, k$mean, l$mean), Serrs = c(i$se, j$se, k$se, l$se))
guiltdat_white
guiltbar_white <- ggplot(data=guiltdat_white, aes(x = TestGroup, y=Means, fill = Change))
guiltbar_white <- guiltbar_white + geom_bar(stat="identity", position = position_dodge()) + geom_errorbar(aes(ymin=Means-2*Serrs, ymax=Means+2*Serrs), width=.2, position=position_dodge(.9)) + labs(x="Test Group", y="Collective Guilt Acceptance", title="Whites' Collective Guilt Acceptance")+scale_fill_manual(values=c('darkgray', 'lightgray')) + theme_classic()
guiltbar_white
guiltbar_white <- guiltbar_white +theme(axis.title.x = element_text(size=14, face="bold"), axis.title.y = element_text(size=14, face="bold"), legend.title = element_text(size=14, face="bold"), axis.title = element_text(size=14, face="bold"))
guiltbar_white
#### Preference Alignment Whites Bar Graphs
Preference_Alignment_Whites <- read_excel("Preference Alignment Whites.xlsx")
#### Data check for Preference Alignment
descr::freq(Preference_Alignment_Whites$PreferenceAlignment)
descr::freq(guilt_no_support_no$Q190)
descr::freq(guilt_yes_support_no$Q136)
descr::freq(guilt_yes_support_yes$Q25)
descr::freq(guilt_no_support_yes$Q80)
#### Graph
cell13 <- subset(Preference_Alignment_Whites, TestGroup == 'Control' & Change == 'No')
cell14 <- subset(Preference_Alignment_Whites, TestGroup == 'Control' & Change == 'Yes')
cell15 <- subset(Preference_Alignment_Whites, TestGroup == 'GuiltTreatment' & Change == 'No')
cell16 <- subset(Preference_Alignment_Whites, TestGroup == 'GuiltTreatment' & Change == 'Yes')
m <- describe(cell13$PreferenceAlignment)
n <- describe(cell14$PreferenceAlignment)
o <- describe(cell15$PreferenceAlignment)
p <- describe(cell16$PreferenceAlignment)
p
preference_dat <- data.frame(TestGroup = c("Control", "Control", "Guilt Treatment", "Guilt Treatment"), Change = c("No", "Yes", "No", "Yes"), Means = c(m$mean, n$mean, o$mean, p$mean), Serrs = c(m$se, n$se, o$se, p$se))
preference_dat
preference_bar <- ggplot(data=preference_dat, aes(x = TestGroup, y=Means, fill = Change))
preference_bar <- preference_bar + geom_bar(stat="identity", position = position_dodge()) + geom_errorbar(aes(ymin=Means-2*Serrs, ymax=Means+2*Serrs), width=.2, position=position_dodge(.9)) + labs(x="Test Group", y="Preference Alignment", title="Whites' Perceived Preference Alignment with State Policy")+scale_fill_manual(values=c('darkgray', 'lightgray')) + theme_classic()
preference_bar
preference_bar <- preference_bar +theme(axis.title.x = element_text(size=14, face="bold"), axis.title.y = element_text(size=14, face="bold"), legend.title = element_text(size=14, face="bold"), axis.title = element_text(size=14, face="bold"))
preference_bar
#### White Guilt and BlackRacism Regression Lines and Coefficents
Guilt_and_Black_Racism_for_Whites<- read_excel("Guilt and Black Racism for Whites.xlsx")
ggplot(Guilt_and_Black_Racism_for_Whites, aes(x=GuiltScore, y=BlackRacism, color=TestGroup)) + geom_smooth(method = "lm")
summary(lm(BlackRacism ~ GuiltScore, data=Guilt_and_Black_Racism_for_Whites))
#### OLS Regression Output
OLS_guilt_no_change_no_ <- read_excel("OLS guilt (no) change (no).xlsx")
OLS_guilt_no_change_yes_ <- read_excel("OLS guilt (no) change (yes).xlsx")
OLS_guilt_yes_change_no_ <- read_excel("OLS guilt (yes) change (no).xlsx")
OLS_guilt_yes_change_yes_ <- read_excel("OLS guilt (yes) change (yes).xlsx")
m1 <- lm(LegitimacyScore ~ PreferenceAlignment + BlackRacism + GuiltScore + Liberalism + Education, data=OLS_guilt_no_change_no_)
m2 <- lm(LegitimacyScore ~ PreferenceAlignment + BlackRacism + GuiltScore + Liberalism + Education, data=OLS_guilt_no_change_yes_)
m3 <- lm(LegitimacyScore ~ PreferenceAlignment + BlackRacism + GuiltScore + Liberalism + Education, data=OLS_guilt_yes_change_no_)
m4 <- lm(LegitimacyScore ~ PreferenceAlignment + BlackRacism + GuiltScore + Liberalism + Education, data=OLS_guilt_yes_change_yes_)
stargazer(m1, m2, m3, m4, type="html", dep.var.labels=c("Whites' Perceived Legitimacy of Pat Williams' State Policy"), covariate.labels=c("Preference Alignment", "Black Racism", "White Guilt Acceptance", "Liberalism", "Education"), out="guiltmodels.htm")
#### Williams Votes Against or For Me Bar Plot
Williams_Vote_With_Me <- read_excel("Williams Vote With Me.xlsx")
View(Williams_Vote_With_Me)
cell17 <- subset(Williams_Vote_With_Me, TestGroup == 'Control' & PreferenceAlignment == 'WilliamsVotesWithMe')
cell18 <- subset(Williams_Vote_With_Me, TestGroup == 'Control' & PreferenceAlignment == 'Undecided')
cell19 <- subset(Williams_Vote_With_Me, TestGroup == 'Control' & PreferenceAlignment == 'WilliamsVotesAgainstMe')
cell20 <- subset(Williams_Vote_With_Me, TestGroup == 'GuiltTreatment' & PreferenceAlignment == 'WilliamsVotesWithMe')
cell21 <- subset(Williams_Vote_With_Me, TestGroup == 'GuiltTreatment' & PreferenceAlignment == 'Undecided')
cell22 <- subset(Williams_Vote_With_Me, TestGroup == 'GuiltTreatment' & PreferenceAlignment == 'WilliamsVotesAgainstMe')
q <- describe(cell17$LegitimacyScore)
r <- describe(cell18$LegitimacyScore)
s <- describe(cell19$LegitimacyScore)
t <- describe(cell20$LegitimacyScore)
u <- describe(cell21$LegitimacyScore)
v <- describe(cell22$LegitimacyScore)
v
Williams_With_Me_dat <- data.frame(TestGroup = c("Control", "Control", "Control", "Guilt Treatment", "Guilt Treatment", "Guilt Treatment"), PreferenceAlignment = c("Williams Votes With Me", "Undecided", "Williams Votes Against Me", "Williams Votes With Me", "Undecided", "Williams Votes Against Me"), Means = c(q$mean, r$mean, s$mean, t$mean, u$mean, v$mean), Serrs = c(q$se, r$se, s$se, t$se, u$se, v$se))
Williams_With_Me_dat
Williams_With_Me_bar <- ggplot(data=Williams_With_Me_dat, aes(x = TestGroup, y=Means, fill = PreferenceAlignment))
Williams_With_Me_bar <- Williams_With_Me_bar + geom_bar(stat="identity", position = position_dodge()) + geom_errorbar(aes(ymin=Means-2*Serrs, ymax=Means+2*Serrs), width=.1, position=position_dodge(.9)) + labs(x="Test Group", y="Perceived Legitimacy", title="Comparing Whites' Perceived Legitimacy and Preferences with State Policy")+scale_fill_manual(values=c('gray40', 'gray60', 'gray80')) + theme_classic()
Williams_With_Me_bar
Williams_With_Me_bar <- Williams_With_Me_bar +theme(axis.title.x = element_text(size=14, face="bold"), axis.title.y = element_text(size=14, face="bold"), legend.title = element_text(size=14, face="bold"), axis.title = element_text(size=14, face="bold"))
Williams_With_Me_bar
#### Guilt and Legitimacy
GuiltScore_Legitimacy <- read_excel("GuiltScore and Legitmacy.xlsx")
View(GuiltScore_Legitimacy)
cell23 <- subset(GuiltScore_Legitimacy, TestGroup == 'Control' & GuiltScore == 'LowGuilt')
cell24 <- subset(GuiltScore_Legitimacy, TestGroup == 'Control' & GuiltScore == 'ModerateGuilt')
cell25 <- subset(GuiltScore_Legitimacy, TestGroup == 'Control' & GuiltScore == 'HighGuilt')
cell26 <- subset(GuiltScore_Legitimacy, TestGroup == 'GuiltTreatment' & GuiltScore == 'LowGuilt')
cell27 <- subset(GuiltScore_Legitimacy, TestGroup == 'GuiltTreatment' & GuiltScore == 'ModerateGuilt')
cell28 <- subset(GuiltScore_Legitimacy, TestGroup == 'GuiltTreatment' & GuiltScore == 'HighGuilt')
w <- describe(cell23$LegitimacyScore)
x <- describe(cell24$LegitimacyScore)
y <- describe(cell25$LegitimacyScore)
z <- describe(cell26$LegitimacyScore)
aa <- describe(cell27$LegitimacyScore)
bb <- describe(cell28$LegitimacyScore)
bb
Final_Bar_Dat <- data.frame(TestGroup = c("Control", "Control", "Control", "Guilt Treatment", "Guilt Treatment", "Guilt Treatment"), GuiltScore = c("Low Guilt", "Moderate Guilt", "High Guilt", "Low Guilt", "Moderate Guilt", "High Guilt"), Means = c(w$mean, x$mean, y$mean, z$mean, aa$mean, bb$mean), Serrs = c(w$se, x$se, y$se, z$se, aa$se, bb$se))
Final_Bar_Dat
Final_Bar_Final_Bar <- ggplot(data=Final_Bar_Dat, aes(x = TestGroup, y=Means, fill = GuiltScore))
Final_Bar_Final_Bar <- Final_Bar_Final_Bar + geom_bar(stat="identity", position = position_dodge()) + geom_errorbar(aes(ymin=Means-2*Serrs, ymax=Means+2*Serrs), width=.1, position=position_dodge(.9)) + labs(x="Test Group", y="Perceived Legitimacy", title="Perceived Legitimacy and Collective Guilt Acceptance Towards Changing State Policy")+scale_fill_manual(values=c('gray40', 'gray60', 'gray80')) + theme_classic()
Final_Bar_Final_Bar
Final_Bar_Final_Bar <- Final_Bar_Final_Bar +theme(axis.title.x = element_text(size=14, face="bold"), axis.title.y = element_text(size=14, face="bold"), legend.title = element_text(size=14, face="bold"), axis.title = element_text(size=14, face="bold"))
Final_Bar_Final_Bar
#### Ideology and Legitimacy
OLS_Regression_Output_Whites <- read_excel("C:/Users/olive/Dropbox/Senior Thesis/MTurk/OLS Regression Output Whites.xlsx")
View(OLS_Regression_Output_Whites)
cell29 <- subset(OLS_Regression_Output_Whites, TestGroup == 'Control' & Liberalism == 'Liberal')
cell30 <- subset(OLS_Regression_Output_Whites, TestGroup == 'Control' & Liberalism == 'Moderate')
cell31 <- subset(OLS_Regression_Output_Whites, TestGroup == 'Control' & Liberalism == 'Conservative')
cell32 <- subset(OLS_Regression_Output_Whites, TestGroup == 'GuiltTreatment' & Liberalism == 'Liberal')
cell33 <- subset(OLS_Regression_Output_Whites, TestGroup == 'GuiltTreatment' & Liberalism == 'Moderate')
cell34 <- subset(OLS_Regression_Output_Whites, TestGroup == 'GuiltTreatment' & Liberalism == 'Conservative')
cc <- describe(cell29$LegitimacyScore)
dd <- describe(cell30$LegitimacyScore)
ee <- describe(cell31$LegitimacyScore)
ff <- describe(cell32$LegitimacyScore)
gg <- describe(cell33$LegitimacyScore)
hh <- describe(cell34$LegitimacyScore)
hh
Ideology_Legitimacy_Dat <- data.frame(TestGroup = c("Control", "Control", "Control", "Guilt Treatment", "Guilt Treatment", "Guilt Treatment"), Liberalism = c("Liberal", "Moderate", "Conservative", "Liberal", "Moderate", "Conservative"), Means = c(cc$mean, dd$mean, ee$mean, ff$mean, gg$mean, hh$mean), Serrs = c(cc$se, dd$se, ee$se, ff$se, gg$se, hh$se))
Ideology_Legitimacy_Dat 
Ideology_Bars <- ggplot(data=Ideology_Legitimacy_Dat, aes(x = TestGroup, y=Means, fill = Liberalism))
Ideology_Bars <- Ideology_Bars + geom_bar(stat="identity", position = position_dodge()) + geom_errorbar(aes(ymin=Means-2*Serrs, ymax=Means+2*Serrs), width=.1, position=position_dodge(.9)) + labs(x="Test Group", y="Perceived Legitimacy", title="Whites' Perceived Legitimacy and Ideology on State Policy")+scale_fill_manual(values=c('gray40', 'gray60', 'gray80')) + theme_classic()
Ideology_Bars
Ideology_Bars <- Ideology_Bars +theme(axis.title.x = element_text(size=14, face="bold"), axis.title.y = element_text(size=14, face="bold"), legend.title = element_text(size=14, face="bold"), axis.title = element_text(size=14, face="bold"))
Ideology_Bars
#### Legitimacy and Guilt Acceptance
Legitimacy_and_Guilt_Acceptance <- read_excel("Legitimacy and Guilt Acceptance.xlsx")
View(Legitimacy_and_Guilt_Acceptance)
cell35 <- subset(Legitimacy_and_Guilt_Acceptance, GuiltScore == 'LowGuilt' & Group == 'Control_No')
cell36 <- subset(Legitimacy_and_Guilt_Acceptance, GuiltScore == 'LowGuilt' & Group == 'Control_Yes')
cell37 <- subset(Legitimacy_and_Guilt_Acceptance, GuiltScore == 'LowGuilt' & Group == 'GuiltTreatment_No')
cell38 <- subset(Legitimacy_and_Guilt_Acceptance, GuiltScore == 'LowGuilt' & Group == 'GuiltTreatment_Yes')
cell39 <- subset(Legitimacy_and_Guilt_Acceptance, GuiltScore == 'ModerateGuilt' & Group == 'Control_No')
cell40 <- subset(Legitimacy_and_Guilt_Acceptance, GuiltScore == 'ModerateGuilt' & Group == 'Control_Yes')
cell41 <- subset(Legitimacy_and_Guilt_Acceptance, GuiltScore == 'ModerateGuilt' & Group == 'GuiltTreatment_No')
cell42 <- subset(Legitimacy_and_Guilt_Acceptance, GuiltScore == 'ModerateGuilt' & Group == 'GuiltTreatment_Yes')
cell43 <- subset(Legitimacy_and_Guilt_Acceptance, GuiltScore == 'HighGuilt' & Group == 'Control_No')
cell44 <- subset(Legitimacy_and_Guilt_Acceptance, GuiltScore == 'HighGuilt' & Group == 'Control_Yes')
cell45 <- subset(Legitimacy_and_Guilt_Acceptance, GuiltScore == 'HighGuilt' & Group == 'GuiltTreatment_No')
cell46 <- subset(Legitimacy_and_Guilt_Acceptance, GuiltScore == 'HighGuilt' & Group == 'GuiltTreatment_Yes')
ii <- describe(cell35$LegitimacyScore)
jj <- describe(cell36$LegitimacyScore)
kk <- describe(cell37$LegitimacyScore)
ll <- describe(cell38$LegitimacyScore)
mm <- describe(cell39$LegitimacyScore)
nn <- describe(cell40$LegitimacyScore)
oo <- describe(cell41$LegitimacyScore)
pp <- describe(cell42$LegitimacyScore)
qq <- describe(cell43$LegitimacyScore)
rr <- describe(cell44$LegitimacyScore)
ss <- describe(cell45$LegitimacyScore)
tt <- describe(cell46$LegitimacyScore)
tt
Legitimacy_Guilt_Acceptance_Dat <- data.frame(GuiltScore = c("Low", "Low", "Low", "Low", "Moderate", "Moderate", "Moderate", "Moderate", "High", "High", "High", "High"), Group = c("Bill Vetoed Control Group", "Bill Approved Control Group", "Bill Vetoed Guilt Treatment", "Bill Approved Guilt Treatment", "Bill Vetoed Control Group", "Bill Approved Control Group", "Bill Vetoed Guilt Treatment", "Bill Approved Guilt Treatment", "Bill Vetoed Control Group", "Bill Approved Control Group", "Bill Vetoed Guilt Treatment", "Bill Approved Guilt Treatment"), Means = c(ii$mean, jj$mean, kk$mean, ll$mean, mm$mean, nn$mean, oo$mean, pp$mean, qq$mean, rr$mean, ss$mean, tt$mean), Serrs = c(ii$se, jj$se, kk$se, ll$se, mm$se, nn$se, oo$se, pp$se, qq$se, rr$se, ss$se, tt$se))
Legitimacy_Guilt_Acceptance_Dat 
Legitimacy_Guilt_Accept_Bars <- ggplot(data=Legitimacy_Guilt_Acceptance_Dat, aes(x = GuiltScore, y=Means, fill = Group))
Legitimacy_Guilt_Accept_Bars <- Legitimacy_Guilt_Accept_Bars + geom_bar(stat="identity", position = position_dodge()) + geom_errorbar(aes(ymin=Means-2*Serrs, ymax=Means+2*Serrs), width=.1, position=position_dodge(.9)) + labs(x="Collective Guilt Acceptance Towards Blacks", y="Perceived Legitimacy", title="Whites' Perceived Legitimacy and Guilt Acceptance on State Policy")+scale_fill_manual(values=c('gray20', 'gray40', 'gray60', 'gray80')) + theme_classic()
Legitimacy_Guilt_Accept_Bars
Legitimacy_Guilt_Accept_Bars <- Legitimacy_Guilt_Accept_Bars +theme(axis.title.x = element_text(size=14, face="bold"), axis.title.y = element_text(size=14, face="bold"), legend.title = element_text(size=14, face="bold"), axis.title = element_text(size=14, face="bold"))
Legitimacy_Guilt_Accept_Bars
#### Legitimacy and Covert Racism Towards Blacks
Legitimacy_and_Guilt_Acceptance <- read_excel("Legitimacy and Guilt Acceptance.xlsx")
View(Legitimacy_and_Guilt_Acceptance)
cell47 <- subset(Legitimacy_and_Guilt_Acceptance, BlackRacism == 'Low' & Group == 'Control_No')
cell48 <- subset(Legitimacy_and_Guilt_Acceptance, BlackRacism == 'Low' & Group == 'Control_Yes')
cell49 <- subset(Legitimacy_and_Guilt_Acceptance, BlackRacism == 'Low' & Group == 'GuiltTreatment_No')
cell50 <- subset(Legitimacy_and_Guilt_Acceptance, BlackRacism == 'Low' & Group == 'GuiltTreatment_Yes')
cell51 <- subset(Legitimacy_and_Guilt_Acceptance, BlackRacism == 'Moderate' & Group == 'Control_No')
cell52 <- subset(Legitimacy_and_Guilt_Acceptance, BlackRacism == 'Moderate' & Group == 'Control_Yes')
cell53 <- subset(Legitimacy_and_Guilt_Acceptance, BlackRacism == 'Moderate' & Group == 'GuiltTreatment_No')
cell54 <- subset(Legitimacy_and_Guilt_Acceptance, BlackRacism == 'Moderate' & Group == 'GuiltTreatment_Yes')
cell55 <- subset(Legitimacy_and_Guilt_Acceptance, BlackRacism == 'High' & Group == 'Control_No')
cell56 <- subset(Legitimacy_and_Guilt_Acceptance, BlackRacism == 'High' & Group == 'Control_Yes')
cell57 <- subset(Legitimacy_and_Guilt_Acceptance, BlackRacism == 'High' & Group == 'GuiltTreatment_No')
cell58 <- subset(Legitimacy_and_Guilt_Acceptance, BlackRacism == 'High' & Group == 'GuiltTreatment_Yes')
uu <- describe(cell47$LegitimacyScore)
vv <- describe(cell48$LegitimacyScore)
ww <- describe(cell49$LegitimacyScore)
xx <- describe(cell50$LegitimacyScore)
yy <- describe(cell51$LegitimacyScore)
zz <- describe(cell52$LegitimacyScore)
aaa <- describe(cell53$LegitimacyScore)
bbb <- describe(cell54$LegitimacyScore)
ccc <- describe(cell55$LegitimacyScore)
ddd <- describe(cell56$LegitimacyScore)
eee <- describe(cell57$LegitimacyScore)
fff <- describe(cell58$LegitimacyScore)
fff
Legitimacy_Covert_Racism_Dat <- data.frame(BlackRacism = c("Low", "Low", "Low", "Low", "Moderate", "Moderate", "Moderate", "Moderate", "High", "High", "High", "High"), Group = c("Bill Vetoed Control Group", "Bill Approved Control Group", "Bill Vetoed Guilt Treatment", "Bill Approved Guilt Treatment", "Bill Vetoed Control Group", "Bill Approved Control Group", "Bill Vetoed Guilt Treatment", "Bill Approved Guilt Treatment", "Bill Vetoed Control Group", "Bill Approved Control Group", "Bill Vetoed Guilt Treatment", "Bill Approved Guilt Treatment"), Means = c(uu$mean, vv$mean, ww$mean, xx$mean, yy$mean, zz$mean, aaa$mean, bbb$mean, ccc$mean, ddd$mean, eee$mean, fff$mean), Serrs = c(uu$se, vv$se, ww$se, xx$se, yy$se, zz$se, aaa$se, bbb$se, ccc$se, ddd$se, eee$se, fff$se))
Legitimacy_Covert_Racism_Dat
Legitimacy_Covert_Racism_Bars <- ggplot(data=Legitimacy_Covert_Racism_Dat, aes(x = BlackRacism, y=Means, fill = Group))
Legitimacy_Covert_Racism_Bars <- Legitimacy_Covert_Racism_Bars + geom_bar(stat="identity", position = position_dodge()) + geom_errorbar(aes(ymin=Means-2*Serrs, ymax=Means+2*Serrs), width=.1, position=position_dodge(.9)) + labs(x="Covert Racism Towards Blacks", y="Perceived Legitimacy", title="Whites' Perceived Legitimacy and Covert Racism on State Policy")+scale_fill_manual(values=c('gray20', 'gray40', 'gray60', 'gray80')) + theme_classic()
Legitimacy_Covert_Racism_Bars
Legitimacy_Covert_Racism_Bars <- Legitimacy_Covert_Racism_Bars +theme(axis.title.x = element_text(size=14, face="bold"), axis.title.y = element_text(size=14, face="bold"), legend.title = element_text(size=14, face="bold"), axis.title = element_text(size=14, face="bold"))
Legitimacy_Covert_Racism_Bars