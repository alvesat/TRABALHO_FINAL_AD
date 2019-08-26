###################### SCRIPT TRABALHO FINAL AD-UFPE-2019 ####################################################################

 
##CARREGANDO PACOTES
if(require(dplyr)==F)install.packages('dplyr');require(dplyr)
if(require(devtools)==F)install.packages('devtools');require(devtools)
if(require(ggplot2)==F)install.packages('ggplot2');require(ggplot2)
if(require(arm)==F)install.packages('arm');require(arm)
if(require(tidyr)==F)install.packages('tidyr');require(tidyr)
if(require(scales)==F)install.packages('scales');require(scales)
if(require(nortest)==F)install.packages('nortest');require(nortest)
if(require(readxl)==F)install.packages('readxl');require(readxl)
if(require(ggpubr)==F)install.packages('ggpubr');require(ggpubr)
if(require(ggpmisc)==F)install.packages('ggpmisc');require(ggpmisc)
if(require(broom)==F)install.packages('broom');require(broom)
if(require(car)==F)install.packages('car');require(car)
if(require(stargazer)==F)install.packages('stargazer');require(stargazer)
if(require(sjPlot)==F)install.packages('sjPlot');require(sjPlot)
if(require(sjmisc)==F)install.packages('sjmisc');require(sjmisc)
if(require(ResourceSelection)==F)install.packages('ResourceSelection');require(ResourceSelection)
if(require(questionr)==F)install.packages('questionr');require(questionr)
if(require(tidyr)==F)install.packages('tidyr');require(tidyr)
if(require(plyr)==F)install.packages('plyr');require(plyr)
if(require(jtools)==F)install.packages('jtools');require(jtools)
if(require(interactions)==F)install.packages('interactions');require(interactions)
if(require(Rmisc)==F)install.packages('Rmisc');require(Rmisc)
if(require(huxtable)==F)install.packages('huxtable');require(huxtable)
if(require(mctest)==F)install.packages('mctest');require(mctest)
if(require(DescTools)==F)install.packages('DescTools');require(DescTools)
if(require(effects)==F)install.packages('effects');require(effects)


##LENDO BANCO

BASE <- read_excel("antonio-fernandes-ad-ufpe-2019.xlsx")

## REMOVENDO O ANO DE 2002 DA ANÁLISE

BASE <- BASE[ which(BASE$ANO != 2002), ]

##CONVERTENDO VARIAVEIS PARA NUMERICO
BASE$DESPESA_DEFLACIONADA <- as.numeric(BASE$DESPESA_DEFLACIONADA)
BASE$VOTO <- as.numeric(BASE$VOTO)

## TRANSFORMANDO VD E DESPESA EM %
options(scipen=1)

BASEE <- BASE

# VOTOS


BASEE <- transform(BASEE, percentage = ave(VOTO, UF, ANO, FUN = function(x) as.numeric(paste0(round(x/sum(x), 6)*100))))

BASEE$percentage <- as.numeric(BASEE$percentage)

hist(BASEE$percentage)


# DESPESA

BASEE <- transform(BASEE, percentage_des = ave(DESPESA_DEFLACIONADA, UF, ANO, FUN = function(x) as.numeric(paste0(round(x/sum(x), 6)*100))))

BASEE$percentage_des <- as.numeric(BASEE$percentage_des)

hist(BASEE$percentage_des)

##TRANSFORMANDO VARIAVEIS EM LOGARITMO
BASEE$VOTOS_PER_LOG <- log(BASEE$percentage)
BASEE$DES_PER_LOG <- log(BASEE$percentage_des)

BASE <- BASEE
rm(BASEE)

BASE$VOTOS_LOG <- log(BASE$VOTO)
BASE$DES_DEFL_LOG <- log(BASE$DESPESA_DEFLACIONADA)


###################### GRÁFICO 1 ##########################################################

##CORRELAÇÃO

ddply(BASE, "SEXO_COD", summarise, corr=cor.test(VOTOS_LOG, DES_DEFL_LOG, use = "complete.obs"))

##GRÁFICO CORRELAÇÃO
ggscatter(BASE, x = "VOTOS_LOG", y = "DES_DEFL_LOG",
          add = "reg.line",                         
          conf.int = TRUE,                         
          color = "SEXO_COD", 
          palette = c("#000000", "#999999"),
          shape = "SEXO_COD", legend = 'right',
          xlab = "Votos (Log)", ylab = "Gastos de Campanha (Log)")+
  stat_cor(method = "pearson", aes(color = SEXO_COD)) +
  theme(legend.title = element_blank())


###################### GRÁFICO 2 ##########################################################

options( scipen = 1 )
ggscatter(BASE, x = "VOTOS_LOG", y = "DES_DEFL_LOG",
          add = "reg.line",                         
          conf.int = TRUE,                         
          color = "SEXO_COD",
          palette = c("#000000", "#999999"),
          shape = "SEXO_COD", legend = 'right',
          xlab = "Votos (Log)", ylab = "Gastos de Campanha (Log)",
          facet.by = "ANO"
          
)+
  stat_cor(method = "pearson", label.y.npc = 0.3, aes(color = SEXO_COD)) +
  theme(legend.title = element_blank())

###################### GRÁFICO 3 e regressao ##########################################################

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
COR <- BASE[!is.infinite((BASE$VOTOS_PER_LOG)),]
COR <- COR[!is.infinite((COR$DES_PER_LOG)),]

BASE$ANO_DIC <- ifelse(BASE$ANO == 2006, 1, ifelse(BASE$ANO == 2010, 2, ifelse(BASE$ANO == 2014, 3,0)))

COR$ANO_DIC <- ifelse(COR$ANO == 2006, 1, ifelse(COR$ANO == 2010, 2, ifelse(COR$ANO == 2014, 3,0)))

COR$INCUMBENTE <- ifelse(COR$INCUMBENTE =='Incumbente', 1, ifelse(COR$INCUMBENTE=='Desafiante', 0, NA))

COR$INCUMBENTE <- as.numeric(COR$INCUMBENTE)

##REGRESSÕES

COR$SEXO <- factor(COR$SEXO, levels = c(0,1), labels = c("Masculino", "Feminino"))
COR$INCUMBENTE <- factor(COR$INCUMBENTE, levels = c(0,1), labels = c("Não Incumbente", "Incumbente"))

M1 <- lm(formula = VOTOS_PER_LOG ~ DES_PER_LOG + SEXO + MAG_DIS + UF + ANO_DIC + INCUMBENTE, data = COR)
M2 <- lm(formula = VOTOS_PER_LOG ~ DES_PER_LOG + SEXO + MAG_DIS + UF + ANO_DIC + INCUMBENTE + DES_PER_LOG*SEXO, data = COR)
M3 <- lm(formula = VOTOS_PER_LOG ~ DES_PER_LOG + SEXO + MAG_DIS + UF + ANO_DIC + INCUMBENTE + INCUMBENTE*SEXO, data = COR)
M4 <- lm(formula = VOTOS_PER_LOG ~ DES_PER_LOG + SEXO + MAG_DIS + UF + ANO_DIC + INCUMBENTE + INCUMBENTE*SEXO + DES_PER_LOG*SEXO, data = COR)
M5 <- lm(formula = VOTOS_PER_LOG ~ DES_PER_LOG + SEXO + MAG_DIS + UF + ANO_DIC + INCUMBENTE + INCUMBENTE*SEXO + DES_PER_LOG*SEXO + INCUMBENTE*SEXO*DES_PER_LOG, data = COR)


  # teste homocedasticidade e RMSE
options(scipen=999)

ncvTest(M1)
sqrt(mean(M1$residuals^2))

ncvTest(M2)
sqrt(mean(M2$residuals^2))

ncvTest(M3)
sqrt(mean(M3$residuals^2))

ncvTest(M4)
sqrt(mean(M4$residuals^2))

ncvTest(M5)
sqrt(mean(M5$residuals^2))

##TABELA 1 a 5


stargazer(M1, M2, M3, M4, M5, type="html", 
          column.labels = c("M1", "M2", "M3", "M4", "M5"), 
          intercept.bottom = FALSE, 
          single.row=FALSE,     
          notes.append = FALSE, 
          header=FALSE) #output em html e depois tiro um print

coef_names <- c("ln(Despesa Deflacionada %)" = "DES_PER_LOG" , "Sexo" = "SEXOFeminino", "Magnitude do Distrito" = "MAG_DIS",
               "Incumbente" = "INCUMBENTEIncumbente", "ln(Despesa Deflacionada %)*Sexo" = "DES_PER_LOG:SEXOFeminino", "Incumbente*Sexo" = "SEXOFeminino:INCUMBENTEIncumbente",
               "ln(Despesa Deflacionada %)*Incumbente*Sexo" = "DES_PER_LOG:SEXOFeminino:INCUMBENTEIncumbente")

plot_coefs(M1, M2, M3, M4, M5, scale = TRUE, robust = TRUE, inner_ci_level = .95, model.names = c("Modelo 1", "Modelo 2", "Modelo 3", "Modelo 4", "Modelo 5"),
           coefs = coef_names, legend.title = "Modelo")


# ajustes dos modelos

  #### M1 ####

par(mfrow = c(2, 2))
plot(M1)

  
  #### M2 ####

par(mfrow = c(2, 2))
plot(M2)

  #### M3 ####

par(mfrow = c(2, 2))
plot(M3)

  #### M4 ####

par(mfrow = c(2, 2))
plot(M4)

  #### M5 ####

par(mfrow = c(2, 2))
plot(M5)



###################### GRÁFICO 4 ############################################################

interact_plot(M2, pred = "DES_PER_LOG", modx = "SEXO", x.label  = "ln(Despesa Deflacionada %)", y.label = "ln(% Votos)",
              modx.labels = c("MASCULINO", "FEMININO"), interval = TRUE)

###################### GRÁFICO 5 ############################################################

cat_plot(M3, pred = INCUMBENTE, modx = SEXO, x.label  = "Incumbência", y.label = "ln(% Votos)")

###################### GRÁFICO 6 e 7  ############################################################

interact_plot(M4, pred = "DES_PER_LOG", modx = "SEXO", x.label  = "ln(Despesa Deflacionada %)", y.label = "ln(% Votos)",
              modx.labels = c("MASCULINO", "FEMININO"), interval = TRUE) 

cat_plot(M4, pred = INCUMBENTE, modx = SEXO, x.label  = "Incumbência", y.label = "ln(% Votos)")


###################### GRAFICO 8 ##########################################################

plot_model(M5, type = "pred", terms = c("DES_PER_LOG", "SEXO", "INCUMBENTE"), title = "", axis.title = c("ln(Despesa Deflacionada %)","ln(% Votos)"))


#################### REGRESSÕES DOS MODELOS 1,2,3,4 e 5 COM REMOCAO DE OUTLIER ######################################


#### SUBSTITUIR OUTLIERS POR MEIO DA TÉCNICA DE "WINSORIZE"

Sem_out <- COR

Sem_out$DES_PER_LOG <- Winsorize(Sem_out$DES_PER_LOG)
Sem_out$VOTOS_PER_LOG <- Winsorize(Sem_out$VOTOS_PER_LOG)

### REGRESSÕES

M1_o <- lm(formula = VOTOS_PER_LOG ~ DES_PER_LOG + SEXO + MAG_DIS + UF + ANO_DIC + INCUMBENTE, data = Sem_out)
M2_o <- lm(formula = VOTOS_PER_LOG ~ DES_PER_LOG + SEXO + MAG_DIS + UF + ANO_DIC + INCUMBENTE + DES_PER_LOG*SEXO, data = Sem_out)
M3_o <- lm(formula = VOTOS_PER_LOG ~ DES_PER_LOG + SEXO + MAG_DIS + UF + ANO_DIC + INCUMBENTE + INCUMBENTE*SEXO, data = Sem_out)
M4_o <- lm(formula = VOTOS_PER_LOG ~ DES_PER_LOG + SEXO + MAG_DIS + UF + ANO_DIC + INCUMBENTE + INCUMBENTE*SEXO + DES_PER_LOG*SEXO, data = Sem_out)
M5_o <- lm(formula = VOTOS_PER_LOG ~ DES_PER_LOG + SEXO + MAG_DIS + UF + ANO_DIC + INCUMBENTE + INCUMBENTE*SEXO + DES_PER_LOG*SEXO + INCUMBENTE*SEXO*DES_PER_LOG, data = Sem_out)

stargazer(M1_o, M2_o, M3_o, M4_o, M5_o, type="html", 
                      column.labels = c("M1", "M2", "M3", "M4", "M5"), 
                      intercept.bottom = FALSE, 
                      single.row=FALSE,     
                      notes.append = FALSE, 
                      header=FALSE) #output em html e depois tiro um print

########################################################################

##ESTATISTICA DESCRITIVA DO GRAFICO 9 

fivenum(COR$DESPESA_DEFLACIONADA)
mean(COR$DESPESA_DEFLACIONADA)
sd(COR$DESPESA_DEFLACIONADA)

##GRAFICO 9

qplot(COR$DESPESA_DEFLACIONADA, geom="histogram", xlab = "") +
  ylab("Contagem") + xlab("R$") +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14)) 

##ESTATISTICA DESCRITIVA DO GRAFICO 10

fivenum(COR$DES_PER_LOG)
mean(COR$DES_PER_LOG)
sd(COR$DES_PER_LOG)

##GRAFICO 10

qplot(COR$DES_PER_LOG, geom="histogram", xlab = "") +
  ylab("Contagem") + xlab("R$") +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14))

###################### GRÁFICO 11 ##########################################################

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

# SEM TEXTO

ggplot(plot_data, aes(Group.1, percentage), xlab="") +
  geom_bar(stat="identity", width=.5, position = "dodge")+  
  facet_wrap(~Group.2) +
  ylab("%") + xlab("") + theme(axis.text=element_text(size=12),
                               axis.title=element_text(size=14)) 


###################### GRÁFICO 12 ##########################################################
plot_data <- transform(BASE, percentage = ave(DESPESA_DEFLACIONADA, UF, ANO, FUN = function(x) as.numeric(paste0(round(x/sum(x), 3)*100))))
plot_data <- plot_data[ which(plot_data$SEXO_COD != 'NÃO INFORMADO'),]

##BOX PLOT INDICE

ggplot(plot_data, aes(x=UF, y = percentage,  color=SEXO_COD)) +
  geom_boxplot(na.rm = TRUE) + labs(title="",x="", y = "%") + 
  facet_wrap(~ANO, ncol = 1)+
  theme(axis.text=element_text(size=12), axis.title=element_text(size=14)) +
  theme(legend.title = element_blank())


###################### GRÁFICO 13 ##########################################################
A <- aggregate(BASE$DESPESA_DEFLACIONADA, by = list(BASE$SEXO_COD, BASE$UF), FUN = sum)
A <- A[ which(A$Group.1 != 'NÃO INFORMADO'),]
A <- transform(A, percentage = ave(x, Group.2, FUN = function(x) as.numeric(paste0(round(x/sum(x), 3)*100))))


ggplot(A, aes(x=Group.2, y=percentage, fill=Group.1), xlab = "") + 
  scale_y_continuous(limits=c(0,100), expand = c(0,0)) +
  geom_bar(position=position_dodge(), stat="identity", width=.5) +
  ylab("%") + xlab("") + theme(axis.text=element_text(size=12),
                               axis.title=element_text(size=14))+
  theme(legend.title = element_blank())

################################# GRAFICO 14 #####################################################

##AGRUPANDO PORCENTAGEM POR UF

df2 <- BASE %>% 
  group_by(SEXO, UF) %>% 
  tally() %>% 
  complete(SEXO, fill = list(n = 0))

df2 <- transform(df2, percentage = ave(n, UF, FUN = function(x) as.numeric(paste0(round(x/sum(x), 3)*100))))


##SELECIONANDO APENAS AS MULHERES

CAND_MUL <- df2[ which(df2$SEXO == 1),]

mean(CAND_MUL$percentage)
sd(CAND_MUL$percentage)

## GRÁFICO SEM % EM CADA BARRA

ggplot(data=CAND_MUL, aes(x= reorder(UF, -percentage), y=percentage)) +
  geom_bar(stat="identity", width = 0.7) +
  geom_text(aes(label = paste0(CAND_MUL$percentage)), position = position_stack(vjust = 1.1),
            size = 4.3)+
  scale_y_continuous(limits=c(0,100), expand = c(0,0)) +
  ylab("%") + xlab("") + theme(axis.text=element_text(size=12),
                               axis.title=element_text(size=14)) 

############################## GRAFICO 15 #########################################################

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
  facet_wrap(~ANO, ncol = 1) +
  geom_hline(yintercept = 30, linetype = 'dashed', color = "red") +
  scale_y_continuous(limits=c(0,100), expand = c(0,0)) +
  ylab("%") + xlab("") + theme(axis.text=element_text(size=12, face = 'bold'),
                               axis.title=element_text(size=14),
                               strip.text = element_text(size=12))


############################## GRAFICO 16 #########################################################

##GRÁFICO GASTOS PARTIDO

BASE <- BASE[ which(BASE$SEXO_COD != 'NÃO INFORMADO'),]
B <- summarySE(BASE, measurevar="DESPESA_DEFLACIONADA", groupvars=c("SEXO_COD","PARTIDO"))

options(scipen=999)

ggplot(B, aes(x=PARTIDO, y=DESPESA_DEFLACIONADA, fill=SEXO_COD), xlab = "") + 
  geom_bar(position=position_dodge(), stat="identity", width=.5) +
  ylab("R$ Defl.") + xlab("") + theme(axis.text=element_text(size=12),
                                      axis.title=element_text(size=14),
                                      axis.text.x = element_text(angle = 90, vjust=0.5, hjust=1))



############################## GRAFICO 17 #########################################################

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
  facet_wrap(~ANO, ncol = 1) +
  geom_hline(yintercept = 30, linetype = 'dashed', color = "red") +
  scale_y_continuous(limits=c(0,100), expand = c(0,0)) +
  ylab("%") + xlab("") + theme(axis.text=element_text(size=12, face = 'bold'),
                               axis.title=element_text(size=14),
                               strip.text = element_text(size=12))




