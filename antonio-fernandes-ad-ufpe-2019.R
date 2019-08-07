###################### SCRIPT TRABALHO FINAL AD-UFPE-2019 ####################################################################


##CARREGANDO PACOTES
library(dplyr)
library(ggplot2)
library(tidyr)
library(scales)
library(nortest)
library(readxl)
library(ggpubr)
library(ggpmisc)
library(broom)
library(car) 
library(stargazer) 
library(effects) 
library(sjPlot) 
library(sjmisc) 
library(ggplot2)
library(ResourceSelection)
library(questionr)
library(tidyr)
library(plyr)
library(jtools)
library(interactions)
library(Rmisc)


options(scipen=999)
##LENDO BANCO

BASE <- read_excel("antonio-fernandes-ad-ufpe-2019.xlsx")

##CONVERTENDO VARIAVEIS PARA NUMERICO
BASE$DESPESA_DEFLACIONADA <- as.numeric(BASE$DESPESA_DEFLACIONADA)
BASE$VOTO <- as.numeric(BASE$VOTO)

##TRANSFORMANDO VARIAVEIS EM LOGARITMO
BASE$VOTOS_LOG <- log(BASE$VOTO)
BASE$DES_DEFL_LOG <- log(BASE$DESPESA_DEFLACIONADA)

###################### GRÁFICO 1 ##########################################################

##BARPLOT DINHEIRO

plot_data <- aggregate(BASE$DESPESA_DEFLACIONADA, by = list(BASE$SEXO_COD, BASE$ANO), FUN = sum)

plot_data <- plot_data[ which(plot_data$Group.1 != 'NÃO INFORMADO'),]

plot_data <- transform(plot_data, percentage = ave(x, Group.2, FUN = function(x) (as.numeric(paste0(round(x/sum(x), 3)*100)))))

ggplot(plot_data, aes(Group.1, percentage), xlab="") +
  geom_bar(stat="identity", width=.5, position = "dodge")+  
  facet_wrap(~Group.2) +
  ylab("%") + xlab("") + theme(axis.text=element_text(size=12),
                               axis.title=element_text(size=14)) +
  geom_text(aes(label = paste0(plot_data$percentage)), position = position_stack(vjust = 1.1),
            size = 4.0)


###################### GRÁFICO 2 ##########################################################
plot_data <- transform(BASE, percentage = ave(DESPESA_DEFLACIONADA, UF, ANO, FUN = function(x) as.numeric(paste0(round(x/sum(x), 3)*100))))
plot_data <- plot_data[ which(plot_data$SEXO_COD != 'NÃO INFORMADO'),]

##BOX PLOT INDICE

ggplot(plot_data, aes(x=UF, y = percentage,  color=SEXO_COD)) +
  geom_boxplot(na.rm = TRUE) + labs(title="",x="", y = "%") + theme(axis.text=element_text(size=12),
                                                                    axis.title=element_text(size=14)) +
                                                                theme(legend.title = element_blank())


###################### GRÁFICO 3 ##########################################################
A <- aggregate(BASE$DESPESA_DEFLACIONADA, by = list(BASE$SEXO_COD, BASE$UF), FUN = sum)
A <- A[ which(A$Group.1 != 'NÃO INFORMADO'),]
A <- transform(A, percentage = ave(x, Group.2, FUN = function(x) as.numeric(paste0(round(x/sum(x), 3)*100))))


ggplot(A, aes(x=Group.2, y=percentage, fill=Group.1), xlab = "") + 
  scale_y_continuous(limits=c(0,100), expand = c(0,0)) +
  geom_bar(position=position_dodge(), stat="identity", width=.5) +
  ylab("%") + xlab("") + theme(axis.text=element_text(size=12),
                               axis.title=element_text(size=14))+
  theme(legend.title = element_blank())

###################### GRÁFICO 4 ##########################################################

##REMOVENDO CASOS NAN
COR <- BASE[!is.infinite((BASE$VOTOS_LOG)),]
COR <- COR[!is.infinite((COR$DES_DEFL_LOG)),]
COR <- COR[ which(COR$SEXO_COD != 'NÃO INFORMADO'),]

##CORRELAÇÃO

ddply(COR, "SEXO_COD", summarise, corr=cor.test(VOTOS_LOG, DES_DEFL_LOG, use = "complete.obs"))

##GRÁFICO CORRELAÇÃO
ggscatter(COR, x = "VOTOS_LOG", y = "DES_DEFL_LOG",
          add = "reg.line",                         
          conf.int = TRUE,                         
          color = "SEXO_COD",           
          shape = "SEXO_COD", legend = 'right',
          xlab = "Votos (Log)", ylab = "Gastos de Campanha (Log)")+
  stat_cor(method = "pearson", aes(color = SEXO_COD)) +
  theme(legend.title = element_blank())


###################### GRÁFICO 5 ##########################################################

options( scipen = 1 )
ggscatter(COR, x = "VOTOS_LOG", y = "DES_DEFL_LOG",
          add = "reg.line",                         
          conf.int = TRUE,                         
          color = "SEXO_COD",           
          shape = "SEXO_COD", legend = 'right',
          xlab = "Votos (Log)", ylab = "Gastos de Campanha (Log)",
          facet.by = "ANO"
          
)+
  stat_cor(method = "pearson", label.y.npc = 0.3, aes(color = SEXO_COD)) +
  theme(legend.title = element_blank())

###################### GRÁFICO 6 e regressao ##########################################################

## CRIANDO VARIÁVEL DE MAGNITUDE DO DISTRITO PARA CONTROLE
BASE$MAG_DIS <- ifelse(BASE$UF == 'TO', 24, ifelse(BASE$UF == 'SE', 24, ifelse(BASE$UF == 'RR', 24,
                                            ifelse(BASE$UF == 'RO', 24, ifelse(BASE$UF == 'RN', 24, ifelse(BASE$UF == 'MS', 24,
                                            ifelse(BASE$UF == 'MT', 24, ifelse(BASE$UF == 'DF', 24, ifelse(BASE$UF == 'AM', 24,
                                            ifelse(BASE$UF == 'AP', 24, ifelse(BASE$UF == 'AC', 24, ifelse(BASE$UF == 'AL', 27,
                                            ifelse(BASE$UF == 'PI', 30, ifelse(BASE$UF == 'ES', 30, ifelse(BASE$UF == 'PB', 36,
                                            ifelse(BASE$UF == 'SC', 40, ifelse(BASE$UF == 'PA', 41, ifelse(BASE$UF == 'GO', 41,
                                            ifelse(BASE$UF == 'MA', 42, ifelse(BASE$UF == 'CE', 46, ifelse(BASE$UF == 'PE', 49,
                                            ifelse(BASE$UF == 'PR', 54, ifelse(BASE$UF == 'RS', 55, ifelse(BASE$UF == 'BA', 63,
                                            ifelse(BASE$UF == 'RJ', 70, ifelse(BASE$UF == 'MG', 77, ifelse(BASE$UF == 'SP', 94, NA)))))))))))))))))))))))))))

##REMOVENDO CASOS NAN
COR <- BASE[!is.infinite((BASE$VOTOS_LOG)),]
COR <- COR[!is.infinite((COR$DES_DEFL_LOG)),]

##REGRESSÃO 1 e 2
M2 <- lm(formula = VOTOS_LOG ~ DES_DEFL_LOG + SEXO + DES_DEFL_LOG*SEXO + MAG_DIS, data = COR)
M1 <- lm(formula = VOTOS_LOG ~ DES_DEFL_LOG + SEXO + MAG_DIS, data = COR)

##TABELA 1 E 2
stargazer(M1, M2,type="html", 
          column.labels = c("Main Effects", "Interaction"), 
          intercept.bottom = FALSE, 
          single.row=FALSE,     
          notes.append = FALSE, 
          header=FALSE) #output em html e depois tiro um print

coef_names <- c("Despesa Deflacionada" = "DES_DEFL_LOG" , "Sexo" = "SEXO", "Magnitude do Distrito" = "MAG_DIS",
                "Des. Defl x Sexo" = "DES_DEFL_LOG:SEXO")

plot_summs(M1, M2, 
           scale = TRUE, robust = TRUE, inner_ci_level = .95, model.names = c("Modelo 1", "Modelo 2"), coefs = coef_names)

# ajustes dos modelos

plot(M1, 1)
plot(M1, 2)
plot(M1, 3)
plot(M1, 4)

plot(M2, 1)
plot(M2, 2)
plot(M2, 3)
plot(M2, 4)

###################### GRÁFICO 7 ############################################################

interact_plot(M2, pred = "DES_DEFL_LOG", modx = "SEXO", x.label  = "Despesa (defl. e log)", y.label = "Votos (log.)",
              modx.labels = c("MASCULINO", "FEMININO"), interval = TRUE)

###################### GRÁFICO 8 e regressao ############################################################

##REMOVENDO 2002 e ALTERANDO INCUMBENCIA

S_06 <- COR [which(COR$ANO != '2002'),]

S_06$INCUMBENTE <- ifelse(S_06$INCUMBENTE =='Incumbente', 1, ifelse(S_06$INCUMBENTE=='Desafiante', 0, NA))

##REGRESSÃO COM INCUMBENCIA

M3 <- lm(formula = VOTOS_LOG ~ DES_DEFL_LOG + SEXO + INCUMBENTE + MAG_DIS, data = S_06)
M4 <- lm(formula = VOTOS_LOG ~ DES_DEFL_LOG + SEXO + INCUMBENTE + MAG_DIS + DES_DEFL_LOG*SEXO + INCUMBENTE*SEXO, data = S_06)

# TABELA MODELO 3 E 4
stargazer(M3, M4,type="html", 
          column.labels = c("Main Effects", "Interaction"), 
          intercept.bottom = FALSE, 
          single.row=FALSE,     
          notes.append = FALSE, 
          header=FALSE) 


coef_names <- c("Despesa Deflacionada" = "DES_DEFL_LOG" , "Sexo" = "SEXO", "Magnitude do Distrito" = "MAG_DIS",
                "Des. Defl x Sexo" = "DES_DEFL_LOG:SEXO", "Incumbente" = "INCUMBENTE", "Incumbente x Sexo" = "SEXO:INCUMBENTE" )

plot_summs(M3, M4, scale = TRUE, inner_ci_level = .9,
           model.names = c("Modelo 3", "Modelo 4"), coefs = coef_names)

# ajustes dos modelos

plot(M3, 1)
plot(M3, 2)
plot(M3, 3)
plot(M3, 4)

plot(M4, 1)
plot(M4, 2)
plot(M4, 3)
plot(M4, 4)

###################### GRÁFICO 9 ############################################################

interact_plot(M4, pred = "DES_DEFL_LOG", modx = "SEXO", x.label  = "Despesa (defl. e log)", y.label = "Votos (log.)",
              modx.labels = c("MASCULINO", "FEMININO"), interval = TRUE)

###################### GRÁFICO 10 ############################################################

interact_plot(M4, pred = "INCUMBENTE", modx = "SEXO", x.label  = "Incumbente", y.label = "Votos (log.)",
              modx.labels = c("MASCULINO", "FEMININO"), interval = TRUE) 


###################### Regressao por ano ############################################################

##02

COR_02 <- COR [which(COR$ANO == '2002'),]

M_2_02 <- lm(formula = VOTOS_LOG ~ DES_DEFL_LOG + SEXO + DES_DEFL_LOG*SEXO + MAG_DIS, data = COR_02)
M_1_02 <- lm(formula = VOTOS_LOG ~ DES_DEFL_LOG + SEXO + MAG_DIS, data = COR_02)

##06

COR_06 <- COR [which(COR$ANO == '2006'),]

M_2_06 <- lm(formula = VOTOS_LOG ~ DES_DEFL_LOG + SEXO + DES_DEFL_LOG*SEXO + MAG_DIS, data = COR_06)
M_1_06 <- lm(formula = VOTOS_LOG ~ DES_DEFL_LOG + SEXO + MAG_DIS, data = COR_06)

##10

COR_10 <- COR [which(COR$ANO == '2010'),]

M_2_10 <- lm(formula = VOTOS_LOG ~ DES_DEFL_LOG + SEXO + DES_DEFL_LOG*SEXO + MAG_DIS, data = COR_10)
M_1_10 <- lm(formula = VOTOS_LOG ~ DES_DEFL_LOG + SEXO + MAG_DIS, data = COR_10)

##14

COR_14 <- COR [which(COR$ANO == '2014'),]

M_2_14 <- lm(formula = VOTOS_LOG ~ DES_DEFL_LOG + SEXO + DES_DEFL_LOG*SEXO + MAG_DIS, data = COR_14)
M_1_14 <- lm(formula = VOTOS_LOG ~ DES_DEFL_LOG + SEXO + MAG_DIS, data = COR_14)

##EXPORTANDO TABELA
stargazer(M_1_02, M_2_02, M_1_06, M_2_06, M_1_10, M_2_10, M_1_14, M_2_14, type="html", 
          column.labels = c("2002", "2002(I)", "2006", "2006(I)","2010", "2010(I)","2014", "2014(I)"), 
          intercept.bottom = FALSE, 
          single.row=FALSE,     
          notes.append = FALSE, 
          header=FALSE,
          decimal.mark = ",") 

###################### GRAFICOS TERMO INTERATIVO ANO ##########################################################


interact_plot(M_2_02, pred = "DES_DEFL_LOG", modx = "SEXO", x.label  = "Despesa (defl. e log)", y.label = "Votos (log.)", interval = TRUE)
interact_plot(M_2_06, pred = "DES_DEFL_LOG", modx = "SEXO", x.label  = "Despesa (defl. e log)", y.label = "Votos (log.)", interval = TRUE)
interact_plot(M_2_10, pred = "DES_DEFL_LOG", modx = "SEXO", x.label  = "Despesa (defl. e log)", y.label = "Votos (log.)", interval = TRUE)
interact_plot(M_2_14, pred = "DES_DEFL_LOG", modx = "SEXO", x.label  = "Despesa (defl. e log)", y.label = "Votos (log.)", interval = TRUE)

###################### GRAFICOS 11 e 12 ##########################################################

##ESTATISTICA DESCRITIVA DO GRAFICO 11

fivenum(BASE$DESPESA_DEFLACIONADA)
mean(BASE$DESPESA_DEFLACIONADA)
sd(BASE$DESPESA_DEFLACIONADA)

##GRAFICO I

qplot(BASE$DESPESA_DEFLACIONADA, geom="histogram", xlab = "") +
  ylab("Contagem") + xlab("R$") +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14)) 

##ESTATISTICA DESCRITIVA DO GRAFICO 12

fivenum(BASE$DES_DEFL_LOG)

##GRAFICO II

qplot(BASE$DES_DEFL_LOG, geom="histogram", xlab = "") +
  ylab("Contagem") + xlab("R$") +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14))

################################# GRAFICO 13 #####################################################

##AGRUPANDO PORCENTAGEM POR UF

df2 <- BASE %>% 
  group_by(SEXO, UF) %>% 
  tally() %>% 
  complete(SEXO, fill = list(n = 0))

df2 <- transform(df2, percentage = ave(n, UF, FUN = function(x) as.numeric(paste0(round(x/sum(x), 3)*100))))


##SELECIONANDO APENAS AS MULHERES

CAND_MUL <- df2[ which(df2$SEXO == 1),]

## GRÁFICO SEM % EM CADA BARRA

ggplot(data=CAND_MUL, aes(x= reorder(UF, -percentage), y=percentage)) +
  geom_bar(stat="identity", width = 0.7) +
  geom_text(aes(label = paste0(CAND_MUL$percentage)), position = position_stack(vjust = 1.1),
            size = 4.3)+
  scale_y_continuous(limits=c(0,100), expand = c(0,0)) +
  ylab("%") + xlab("") + theme(axis.text=element_text(size=12),
                               axis.title=element_text(size=14)) 

############################## GRAFICO 14 #########################################################

##AGRUPANDO PORCENTAGEM POR UF E ANO

df2 <- BASE %>% 
  group_by(ANO, UF, SEXO) %>% 
  tally() %>% 
  complete(SEXO, fill = list(n = 0))

df2 <- transform(df2, percentage = ave(n, UF, ANO, FUN = function(x) as.numeric(paste0(round(x/sum(x), 3)*100))))


##SELECIONANDO APENAS AS MULHERES

CAND_MUL <- df2[ which(df2$SEXO == 1),]

##ARREDONDANDO PARA 1 CASA DECIMAL 

CAND_MUL$percentage <- round(CAND_MUL$percentage, 1)

## GRÁFICO 

ggplot(data=CAND_MUL, aes(x=UF, y=percentage)) +
  geom_bar(stat="identity", width = 0.7) +
  facet_wrap(~ANO) +
  geom_hline(yintercept = 30, linetype = 'dashed', color = "red") +
  scale_y_continuous(limits=c(0,100), expand = c(0,0)) +
  ylab("%") + xlab("") + theme(axis.text=element_text(size=12, face = 'bold'),
                               axis.title=element_text(size=14),
                               strip.text = element_text(size=12))


############################## GRAFICO 15 #########################################################

##GRÁFICO GASTOS PARTIDO

BASE <- BASE[ which(BASE$SEXO_COD != 'NÃO INFORMADO'),]
B <- summarySE(BASE, measurevar="DESPESA_DEFLACIONADA", groupvars=c("SEXO_COD","PARTIDO"))

ggplot(B, aes(x=PARTIDO, y=DESPESA_DEFLACIONADA, fill=SEXO_COD), xlab = "") + 
  geom_bar(position=position_dodge(), stat="identity", width=.5) +
  ylab("R$ Defl.") + xlab("") + theme(axis.text=element_text(size=12),
                                      axis.title=element_text(size=14),
                                      axis.text.x = element_text(angle = 90, vjust=0.5, hjust=1))


############################## GRAFICO 16 #########################################################

GASTOS_ANO <- aggregate(BASE$DESPESA_DEFLACIONADA, by = list(BASE$SEXO_COD, BASE$UF, BASE$ANO),
                        FUN = sum)


GASTOS_ANO$SEXO <- GASTOS_ANO$Group.1
GASTOS_ANO$UF <- GASTOS_ANO$Group.2
GASTOS_ANO$ANO <- GASTOS_ANO$Group.3
GASTOS_ANO$DESPESA <- GASTOS_ANO$x

GASTOS_ANO$Group.1 <- NULL
GASTOS_ANO$Group.2 <- NULL
GASTOS_ANO$Group.3 <- NULL
GASTOS_ANO$x <- NULL

GASTOS_ANO <- GASTOS_ANO %>% group_by(UF, ANO) 

##SELECIONANDO APENAS AS MULHERES



GASTOS_ANO <- transform(GASTOS_ANO, percentage = ave(DESPESA, UF, ANO, FUN = function(x) as.numeric(paste0(round(x/sum(x), 3)*100))))
GASTOS_ANO <- GASTOS_ANO[ which(GASTOS_ANO$SEXO == 'FEMININO'),]

## GRÁFICO 

ggplot(data=GASTOS_ANO, aes(x=UF, y=percentage)) +
  geom_bar(stat="identity", width = 0.7) +
  facet_wrap(~ANO) +
  geom_hline(yintercept = 30, linetype = 'dashed', color = "red") +
  scale_y_continuous(limits=c(0,100), expand = c(0,0)) +
  ylab("%") + xlab("") + theme(axis.text=element_text(size=12, face = 'bold'),
                               axis.title=element_text(size=14),
                               strip.text = element_text(size=12))
