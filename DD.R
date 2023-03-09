#
library(ppcor)
library(mirt)
library(dplyr)
library(ggplot2)
library(gridExtra)
library(cowplot)
library(corrplot)
library(RColorBrewer)
library(stringr)
library(psych)
library(BayesFactor)
library(ggpubr)


# general sheet
l=dys_td_wj
# sheet per snr with A_cat
k=dys_td_wj
# modalities sheet
m=dys_td_wj
# modality per SNR
j=dys_td_wj

#basic stats
summary(l)

# reading skills (WJBRS) ifo age
ggplot(l, aes(y=l$`WJ Basic Reading Skills`, x=Age,col =Type)) + xlab("Age (Years)")+ ylab("WJ-BRS (Normed Scores)")+ geom_point() + geom_smooth(method=lm, fullrange=FALSE,se=TRUE,aes(fill=Type))+scale_color_manual(name="Group",labels = c("Dyslexia", "TD"),values=c("turquoise4", "tomato3"))+ scale_fill_manual(name="Group",labels = c("Dyslexia", "TD"),values=c("turquoise4", "tomato3"))

# performance over age, per modality, per group
m$Type <- factor(m$Type, levels = c("TD", "Dyslexic")) #re-order groups
new_labels <- c("TD" = "TD", "Dyslexic" = "DD") #change names
m$Score=m$Score*100
ggplot(m, aes(y=Score, x=Age,col =Modality)) + xlab("Age (years)")+ ylab("Performance (% correct)")+ geom_point() + geom_smooth(method=lm, fullrange=FALSE,se=TRUE,aes(fill=Modality)) +scale_color_manual(values=c("blue2", "springgreen4","orange"),labels = c("audio-only", "AV", "visual-only")) + scale_fill_manual(values=c("blue2", "springgreen4","orange"),labels = c("audio-only", "AV", "visual-only"))+scale_y_continuous(breaks=seq(0,100,by=20))+theme(text = element_text(size=15,color="black",face="bold"))+facet_wrap(~Type,labeller = labeller(Type = new_labels))+theme(legend.position = "top",strip.background = element_rect(colour="black", fill=("white")))
anova(lmer(m$Score~m$Modality*m$Type*m$Age+(1|m$subject)))

#per SNR
j$Modality <- factor(j$Modality, levels = c("A -5dB SNR","A -8dB SNR","A -11dB SNR","AV -5dB SNR","AV -8dB SNR","AV -11dB SNR","V")) #re-order groups
j$Score=j$Score*100
j$Type <- factor(j$Type, levels = c("TD", "Dyslexic")) #re-order groups
ggplot(j, aes(x=Type, y=Score)) + xlab("") +ylab("Performance (% correct)")+  geom_boxplot()+facet_wrap(~Modality)+ geom_boxplot(aes(fill=Type))+theme(legend.position = "top",strip.background = element_rect(colour="black", fill=("white")))    + stat_summary(fun=mean, geom="point", shape=20, size=5, color="dark grey")+scale_fill_manual(name="Group",labels = c("TD", "Dyslexia"), values = c("turquoise4", "tomato3"))+ theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank())
anova(lmer(j$Score~j$Mod*j$SNR*j$Type*j$Age+(1|j$subject)))

#AV GAIN
#overall per group
ggplot(l, aes(y=Norm_Gain, x=Age,col=Type)) + xlab("Age (Years)")+ ylab("AV Gain [(AV-A)/(1-A)]")+ geom_point(size=2, position=position_jitter(h=0.025,w=0.025)) + geom_smooth(method=lm, fullrange=FALSE,se=TRUE,aes(fill=Type)) +scale_color_manual(values=c("tomato3","turquoise4"),name="Group",labels=c("TD","Dyslexia"))+ scale_fill_manual(values=c("tomato3","turquoise4"),name="Group",labels=c("TD","Dyslexia"))+theme(legend.position = "top")
# reading skills raw
ggplot(l, aes(y=Norm_Gain, x=`WJ Letter-Word ID raw`,col=Type)) + xlab("WJ BRS raw scores")+ ylab("AV Gain [(AV-A)/(1-A)]")+ geom_point(size=2, position=position_jitter(h=0.025,w=0.025)) + geom_smooth(method=lm, fullrange=FALSE,se=TRUE,aes(fill=Type)) +scale_color_manual(values=c("turquoise4", "tomato3"),name="Group",labels=c("Dyslexia","TD"))+ scale_fill_manual(values=c("turquoise4", "tomato3"),name="Group",labels=c("Dyslexia","TD"))+theme(legend.position = "top")
# reading skills normed
ggplot(l, aes(y=Norm_Gain, x=l$`WJ Basic Reading Skills`)) + xlab("WJ BRS normed scores")+ ylab("AV Gain [(AV-A)/(1-A)]")+ geom_point(size=2, position=position_jitter(h=0.025,w=0.025)) + geom_smooth(method=lm, fullrange=FALSE,se=TRUE)
#PA raw
ggplot(l, aes(y=Norm_Gain, x=`CTOPP combition score raw`,col=Type)) + xlab("CTOPP raw scores")+ ylab("AV Gain [(AV-A)/(1-A)]")+ geom_point(size=2, position=position_jitter(h=0.025,w=0.025)) + geom_smooth(method=lm, fullrange=FALSE,se=TRUE,aes(fill=Type)) +scale_color_manual(values=c("turquoise4", "tomato3"),name="Group",labels=c("Dyslexia","TD"))+ scale_fill_manual(values=c("turquoise4", "tomato3"),name="Group",labels=c("Dyslexia","TD"))+theme(legend.position = "top")
#PA normed
ggplot(l, aes(y=Norm_Gain, x=l$`CTOPP Phonological Awareness`)) + xlab("CTOPP normed scores")+ ylab("AV Gain [(AV-A)/(1-A)]")+ geom_point(size=2, position=position_jitter(h=0.025,w=0.025)) + geom_smooth(method=lm, fullrange=FALSE,se=TRUE)
# ctopp and WJ
ggplot(l, aes(y=l$`WJ Basic Reading Skills`, x=l$`CTOPP Phonological Awareness`)) + xlab("CTOPP normed scores")+ ylab("WJ BRS normed")+ geom_point(size=2, position=position_jitter(h=0.025,w=0.025)) + geom_smooth(method=lm, fullrange=FALSE,se=TRUE)
cor.test(l$`CTOPP Phonological Awareness`,l$`WJ Basic Reading Skills`)
# bayes factor
anovaBF(formula = Norm_Gain ~ Type, data =l)

#individual audio-only performance
k$Type <- factor(k$Type, levels = c("TD", "Dyslexic")) #re-order groups
#per group
ggplot(k,aes(x=A_z,y=normgainpersnr, col=SNR))+ylab("Normalized AV Gain, [(AV-A)/(1-A)]")+xlab("Z-scores of individual audio-only performance")+geom_point(size=1, position=position_jitter(h=0.025,w=0.025))+ylim(-1.8,1.5)+geom_smooth(method="lm",fullrange=FALSE, aes(fill=SNR))+scale_color_manual(values=c("#6A6599FF", "#F8766D","#00B81F"),labels = c("-5 dB", "-8 dB", "-11 dB")) + scale_fill_manual(values=c("#6A6599FF", "#F8766D","#00B81F"),labels = c("-5 dB", "-8 dB", "-11 dB"))+theme(text = element_text(color="black",face="bold"),legend.position = "top")+ stat_cor(method = "pearson",digits=2,p.accuracy=0.001,size=5,  label.x.npc = "left",label.y.npc = "bottom")+facet_wrap(~Type)
# per SNR
ggplot(k,aes(x=A_z,y=normgainpersnr, col=Type))+ylab("Normalized AV Gain, [(AV-A)/(1-A)]")+xlab("Z-scores of individual audio-only performance")+geom_point(size=1, position=position_jitter(h=0.025,w=0.025))+ylim(-1.8,1.5)+geom_smooth(method="lm",fullrange=FALSE, aes(fill=Type))+scale_color_manual(values=c("turquoise4", "tomato3"),labels = c("Dyslexia","TD"),name="Group") + scale_fill_manual(values=c("turquoise4", "tomato3"),labels = c("Dyslexia","TD"),name="Group")+theme(text = element_text(color="black",face="bold"),legend.position = "top")+ stat_cor(method = "pearson",digits=2,p.accuracy=0.001,size=5,  label.x.npc = "left",label.y.npc = "bottom")+facet_wrap(~SNR,scales="free_x")
# linear model
#scale factors
k$V.scaled = scale(k$V, center= TRUE, scale=TRUE)
k$Age.scaled = scale(k$Age, center= TRUE, scale=TRUE)

#V only vs Gain
l$V=l$V*100
ggplot(l,aes(x=V,y=Norm_Gain, col=Type))+ylab("Normalized AV Gain, [(AV-A)/(1-A)]")+xlab("visual-only performance")+geom_point(size=1, position=position_jitter(h=0.025,w=2.5))+ylim(-1,1)+geom_smooth(method="lm",fullrange=FALSE, aes(fill=Type))+scale_color_manual(values=c("turquoise4", "tomato3"),labels = c("Dyslexia","TD"),name="Group") + scale_fill_manual(values=c("turquoise4", "tomato3"),labels = c("Dyslexia","TD"),name="Group")+theme(text = element_text(color="black",face="bold"),legend.position = "top")

# PA vs V
ggplot(l,aes(x=V,y=l$`CTOPP combition score sraw`, col=Type))+ylab("CTOPP raw scores")+xlab("Visual-only performance")+geom_point(size=1, position=position_jitter(h=0.025,w=2.5))+geom_smooth(method="lm",fullrange=FALSE, aes(fill=Type))+theme(text = element_text(color="black",face="bold"),legend.position = "top")+ stat_cor(method = "pearson",digits=2,p.accuracy=0.001,size=5,  label.x.npc = "left",label.y.npc = "bottom")+scale_color_manual(values=c("tomato3","turquoise4"),labels = c("TD","Dyslexia"),name="Group") + scale_fill_manual(values=c( "tomato3","turquoise4"),labels = c("TD","Dyslexia"),name="Group")
ggplot(l,aes(x=V,y=l$`CTOPP combition score raw`, col=Type))+ylab("CTOPP raw scores")+xlab("Visual-only performance")+geom_point(size=1, position=position_jitter(h=0.025,w=2.5))+geom_smooth(method="lm",fullrange=FALSE, aes(fill=Type))+theme(text = element_text(color="black",face="bold"),legend.position = "top")+scale_color_manual(values=c("tomato3","turquoise4"),labels = c("TD","Dyslexia"),name="Group") + scale_fill_manual(values=c( "tomato3","turquoise4"),labels = c("TD","Dyslexia"),name="Group")+scale_x_continuous(expand = c(0, 0), limits = c(0, 100),breaks=seq(0,100,by=20))

#PA vs A and AV per SNR
k$A=k$A*100
k$AV=k$AV*100
#AV
ggplot(k,aes(x=AV,y=`CTOPP combition score raw`, col=Type))+ylab("CTOPP raw scores")+xlab("AV performance")+geom_point(size=1, position=position_jitter(h=0.025,w=2.5))+geom_smooth(method="lm",fullrange=FALSE, aes(fill=Type))+theme(text = element_text(color="black",face="bold"),legend.position = "top")+ stat_cor(method = "pearson",digits=2,p.accuracy=0.001,size=5,  label.x.npc = "left",label.y.npc = "bottom")+scale_color_manual(values=c("tomato3","turquoise4"),labels = c("TD","Dyslexia"),name="Group") + scale_fill_manual(values=c( "tomato3","turquoise4"),labels = c("TD","Dyslexia"),name="Group")+facet_wrap(~SNR,scales="free_x")
#A
ggplot(k,aes(x=A,y=`CTOPP combition score raw`, col=Type))+ylab("CTOPP raw scores")+xlab("audio-only performance")+geom_point(size=1, position=position_jitter(h=0.025,w=2.5))+geom_smooth(method="lm",fullrange=FALSE, aes(fill=Type))+theme(text = element_text(color="black",face="bold"),legend.position = "top")+ stat_cor(method = "pearson",digits=2,p.accuracy=0.001,size=5,  label.x.npc = "left",label.y.npc = "bottom")+scale_color_manual(values=c("tomato3","turquoise4"),labels = c("TD","Dyslexia"),name="Group") + scale_fill_manual(values=c( "tomato3","turquoise4"),labels = c("TD","Dyslexia"),name="Group")+facet_wrap(~SNR,scales="free_x")

#Reading vs A and AV per SNR
ggplot(k,aes(x=AV,y=k$`WJ Letter-Word ID raw`, col=Type))+ylab("WJ-BRS raw scores")+xlab("AV performance")+geom_point(size=1, position=position_jitter(h=0.025,w=2.5))+geom_smooth(method="lm",fullrange=FALSE, aes(fill=Type))+theme(text = element_text(color="black",face="bold"),legend.position = "top")+ stat_cor(method = "pearson",digits=2,p.accuracy=0.001,size=5,  label.x.npc = "left",label.y.npc = "bottom")+scale_color_manual(values=c("tomato3","turquoise4"),labels = c("TD","Dyslexia"),name="Group") + scale_fill_manual(values=c( "tomato3","turquoise4"),labels = c("TD","Dyslexia"),name="Group")+facet_wrap(~SNR,scales="free_x")
ggplot(k,aes(x=A,y=k$`WJ Letter-Word ID raw`, col=Type))+ylab("WJ-BRS raw scores")+xlab("Audio-only performance")+geom_point(size=1, position=position_jitter(h=0.025,w=2.5))+geom_smooth(method="lm",fullrange=FALSE, aes(fill=Type))+theme(text = element_text(color="black",face="bold"),legend.position = "top")+ stat_cor(method = "pearson",digits=2,p.accuracy=0.001,size=5,  label.x.npc = "left",label.y.npc = "bottom")+scale_color_manual(values=c("tomato3","turquoise4"),labels = c("TD","Dyslexia"),name="Group") + scale_fill_manual(values=c( "tomato3","turquoise4"),labels = c("TD","Dyslexia"),name="Group")+facet_wrap(~SNR,scales="free_x")
# reading vs V
ggplot(k,aes(x=V,y=k$`WJ Letter-Word ID raw`, col=Type))+ylab("WJ-BRS raw scores")+xlab("Visual-only performance")+geom_point(size=1, position=position_jitter(h=0.025,w=5))+geom_smooth(method="lm",fullrange=FALSE, aes(fill=Type))+theme(text = element_text(color="black",face="bold"),legend.position = "top")+ stat_cor(method = "pearson",digits=2,p.accuracy=0.001,size=5,  label.x.npc = "left",label.y.npc = "bottom")+scale_color_manual(values=c("tomato3","turquoise4"),labels = c("TD","Dyslexia"),name="Group") + scale_fill_manual(values=c( "tomato3","turquoise4"),labels = c("TD","Dyslexia"),name="Group")+scale_x_continuous(expand = c(0, 0), limits = c(0, 100),breaks=seq(0,100,by=20))
#overall
ggplot(k,aes(x=A,y=k$`WJ Letter-Word ID raw`, col=Type))+ylab("WJ-BRS raw scores")+xlab("Audio-only performance")+geom_point(size=1, position=position_jitter(h=0.025,w=2.5))+geom_smooth(method="lm",fullrange=FALSE, aes(fill=Type))+theme(text = element_text(color="black",face="bold"),legend.position = "top")+ stat_cor(method = "pearson",digits=2,p.accuracy=0.001,size=5,  label.x.npc = "left",label.y.npc = "bottom")+scale_color_manual(values=c("tomato3","turquoise4"),labels = c("TD","Dyslexia"),name="Group") + scale_fill_manual(values=c( "tomato3","turquoise4"),labels = c("TD","Dyslexia"),name="Group")+scale_x_continuous(breaks=seq(0,100,by=20))
#CTOPP overall
ggplot(k,aes(x=AV,y=k$`CTOPP combition score raw`, col=Type))+ylab("CTOPP raw scores")+xlab("AV performance")+geom_point(size=1, position=position_jitter(h=0.025,w=5))+geom_smooth(method="lm",fullrange=FALSE, aes(fill=Type))+theme(text = element_text(color="black",face="bold"),legend.position = "top")+ stat_cor(method = "pearson",digits=2,p.accuracy=0.001,size=5,  label.x.npc = "left",label.y.npc = "bottom")+scale_color_manual(values=c("tomato3","turquoise4"),labels = c("TD","Dyslexia"),name="Group") + scale_fill_manual(values=c( "tomato3","turquoise4"),labels = c("TD","Dyslexia"),name="Group")+scale_x_continuous(expand = c(0, 0), limits = c(0, 100),breaks=seq(0,100,by=20))

#Mismatch
s=dys_td_wj #a vs a2 sheet
s$Type <- factor(s$Type, levels = c("TD", "Dyslexic")) #re-order groups
ggplot(s, aes(y=Score, x=Age,col =Modality)) + xlab("Age (years)")+ ylab("Performance (% correct)")+ geom_point(size=0.5) + geom_smooth(method=lm, fullrange=FALSE,se=TRUE,aes(fill=Modality)) +scale_color_manual(values=c("dodgerblue", "#006666"),name="Stimulus Type",labels=c("Still face","Mismatch video"))+ scale_fill_manual(values=c("dodgerblue", "#006666"),name="Stimulus Type",labels=c("Still face","Mismatch video"))+theme(text = element_text(size=13,color="black",face="bold"),legend.position="top",legend.title=element_text(size=11))+ stat_cor(method = "pearson",digits=2,p.accuracy=0.001,size=5,  label.x = 11,label.y.npc = "bottom")+facet_wrap(~Type)


#models
anova(lm(k$normgainpersnr~k$A_z*k$V*k$SNR*k$`WJ Basic Reading Skills`*k$Age))
anova(lm(k$normgainpersnr~k$A_z*k$V*k$SNR*k$`WJ Letter-Word ID raw`*k$Age))
anova(lm(k$normgainpersnr~k$A_z*k$V*k$SNR*k$`CTOPP Phonological Awareness`*k$Age))
