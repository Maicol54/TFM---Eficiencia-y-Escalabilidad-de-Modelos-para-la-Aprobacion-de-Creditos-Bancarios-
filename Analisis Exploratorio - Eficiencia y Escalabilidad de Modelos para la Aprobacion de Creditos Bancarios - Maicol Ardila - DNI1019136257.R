# UNIVERSIDAD INTERNACIONAL DE VALENCIA (VIU)

# MASTER UNIVERSITARIO EN BIG DATA Y CIENCIA DE DATOS

# TFM - EFICIENCIA Y ESCALABILIDAD DE MODELOS PARA LA APROBACIÓN DE CRÉDITOS BANCARIOS

# ALUMNO: ARDILA AMADO, MAICOL ANDRES

# D.N.I.: 1019136257

# DIRECTOR: NATALINI, FABIO

# CONVOCATORIA: SEGUNDA


################### ---------------- LLAMADA DE LIBRERIAS ---------------- ##################
################### ----------------      LIBRERIAS       ---------------- ##################
#install.packages("PerformanceAnalytics")
library(PerformanceAnalytics)
library("PerformanceAnalytics")
library(dplyr)
library(ggplot2) 
library(readxl)
library(gmodels)
library(Hmisc)
library(ggthemes)
library(patchwork)
library(treemapify)
setwd("D:/Documentos/Maestria U.Valencia/13. TFM/Actual/DataSet")
data<-read.csv("credit.csv")
#################### ---------------- LIMPIEZA DE DATOS ---------------- ####################
#################### ---------------- TIPOS DE DATOS ---------------- ####################

glimpse(data) # Descripcion de cada columna (tipo y algunos ejemplos)

#################### ---------------- MISSING VALUES ---------------- ####################

sum(is.na(data)) # Busqueda de missing values dentro del DataSet

#################### ---------------- FACTORES ---------------- ####################

# Se creara una lista con el nombre de cada columna que es de tipo categorico

types <- lapply(data, class)
col_string <- list()
for (col in names(data)){
  if (types[[col]] == 'character'){
    col_string = c(col_string, col)
  }
}
col_string = c(col_string, 'percent_of_income') # Se agrega la columna percent_of_income
                                                # a la lista de variables categoricas
col_string_vect <- unlist(col_string)
data[col_string_vect] <- lapply(data[col_string_vect], factor)


#################### ---------------- GRAFICAS ---------------- ####################


################## -------------- GENERAL -------------- ##################


############### ----------- checking_balance ----------- ###############

Tabla_checking_balance <- data %>%
  dplyr::group_by(checking_balance, default) %>%
  dplyr::summarise(Total = n()) %>%
  dplyr::mutate(Porcentaje = round(Total/sum(Total)*100, 1)) %>%
  
  dplyr::arrange(checking_balance)  

ggplot(Tabla_checking_balance, aes(x = checking_balance, y=Total, fill=default) ) + 
  geom_bar(width = 0.9,stat="identity",) +  
  ylim(c(0,415))+
  labs(x="", y= "Frecuencia") +
  labs(fill = "Churn") +
  geom_text(aes(label=paste0(Total," ", "\n", "(", Porcentaje, "%", ")")),
            vjust=-0.9, 
            color="black", 
            hjust=0.5,
            # define text position and size
            position = position_dodge(1.1),  
            angle=0, 
            size=5
  )+
  scale_fill_discrete(name = "Churn", labels = c("Si", "No")) +
  theme(axis.text.x = element_text(angle = 0, vjust = 1, hjust=1)) +
  theme_bw(base_size = 14) +
  scale_fill_manual(values = c("#9E9AC8",  "#6A51A3")) + 
  facet_wrap(~"Checking Balance")



############### ----------- credit_history ----------- ###############

Tabla_credit_history <- data %>%
  dplyr::group_by(credit_history, default) %>%
  dplyr::summarise(Total = n()) %>%
  dplyr::mutate(Porcentaje = round(Total/sum(Total)*100, 1)) %>%
  
  dplyr::arrange(credit_history)  

ggplot(Tabla_credit_history, aes(x = credit_history, y=Total, fill=default) ) + 
  geom_bar(width = 0.9,stat="identity",) +  
  ylim(c(0,530))+
  labs(x="", y= "Frecuencia") +
  labs(fill = "Churn") +
  geom_text(aes(label=paste0(Total," ", "\n", "(", Porcentaje, "%", ")")),
            vjust=-0.9, 
            color="black", 
            hjust=0.5,
            # define text position and size
            position = position_dodge(1),  
            angle=0, 
            size=5
  )+
  scale_fill_discrete(name = "Churn", labels = c("Si", "No")) +
  theme(axis.text.x = element_text(angle = 0, vjust = 1, hjust=1)) +
  theme_bw(base_size = 14) +
  scale_fill_manual(values = c("#9E9AC8",  "#6A51A3")) + 
  facet_wrap(~"credit_history")



############### ----------- purpose ----------- ###############

Tabla_purpose <- data %>%
  dplyr::group_by(purpose, default) %>%
  dplyr::summarise(Total = n()) %>%
  dplyr::mutate(Porcentaje = round(Total/sum(Total)*100, 1)) %>%
  
  dplyr::arrange(purpose)  

ggplot(Tabla_purpose, aes(x = purpose, y=Total, fill=default) ) + 
  geom_bar(width = 0.9,stat="identity",) +  
  ylim(c(0,480))+
  labs(x="", y= "Frecuencia") +
  labs(fill = "Churn") +
  geom_text(aes(label=paste0(Total," ", "\n", "(", Porcentaje, "%", ")")),
            vjust=-0.9, 
            color="black", 
            hjust=0.5,
            # define text position and size
            position = position_dodge(0.95),  
            angle=0, 
            size=5
  )+
  scale_fill_discrete(name = "Churn", labels = c("Si", "No")) +
  theme(axis.text.x = element_text(angle = 0, vjust = 1, hjust=1)) +
  theme_bw(base_size = 14) +
  scale_fill_manual(values = c("#9E9AC8",  "#6A51A3")) + 
  facet_wrap(~"purpose")



############### ----------- savings_balance ----------- ###############

Tabla_savings_balance <- data %>%
  dplyr::group_by(savings_balance, default) %>%
  dplyr::summarise(Total = n()) %>%
  dplyr::mutate(Porcentaje = round(Total/sum(Total)*100, 1)) %>%
  
  dplyr::arrange(savings_balance)  

ggplot(Tabla_savings_balance, aes(x = savings_balance, y=Total, fill=default) ) + 
  geom_bar(width = 0.9,stat="identity",) +  
  ylim(c(0,603))+
  labs(x="", y= "Frecuencia") +
  labs(fill = "Churn") +
  geom_text(aes(label=paste0(Total," ", "", "(", Porcentaje, "%", ")")),
            vjust=-0.9, 
            color="black", 
            hjust=0.5,
            # define text position and size
            position = position_dodge(0),  
            angle=0, 
            size=5
  )+
  scale_fill_discrete(name = "Churn", labels = c("Si", "No")) +
  theme(axis.text.x = element_text(angle = 0, vjust = 1, hjust=1)) +
  theme_bw(base_size = 14) +
  scale_fill_manual(values = c("#9E9AC8",  "#6A51A3")) + 
  facet_wrap(~"savings_balance")



############### ----------- employment_duration ----------- ###############

Tabla_employment_duration <- data %>%
  dplyr::group_by(employment_duration, default) %>%
  dplyr::summarise(Total = n()) %>%
  dplyr::mutate(Porcentaje = round(Total/sum(Total)*100, 1)) %>%
  
  dplyr::arrange(employment_duration)  

ggplot(Tabla_employment_duration, aes(x = employment_duration, y=Total, fill=default) ) + 
  geom_bar(width = 0.9,stat="identity",) +  
  ylim(c(0,480))+
  labs(x="", y= "Frecuencia") +
  labs(fill = "Churn") +
  geom_text(aes(label=paste0(Total," ", "", "(", Porcentaje, "%", ")")),
            vjust=-0.9, 
            color="black", 
            hjust=0.5,
            # define text position and size
            position = position_dodge(0),  
            angle=0, 
            size=5
  )+
  scale_fill_discrete(name = "Churn", labels = c("Si", "No")) +
  theme(axis.text.x = element_text(angle = 0, vjust = 1, hjust=1)) +
  theme_bw(base_size = 14) +
  scale_fill_manual(values = c("#9E9AC8",  "#6A51A3")) + 
  facet_wrap(~"employment_duration")



############### ----------- other_credit ----------- ###############

Tabla_other_credit <- data %>%
  dplyr::group_by(other_credit, default) %>%
  dplyr::summarise(Total = n()) %>%
  dplyr::mutate(Porcentaje = round(Total/sum(Total)*100, 1)) %>%
  
  dplyr::arrange(other_credit)  

ggplot(Tabla_other_credit, aes(x = other_credit, y=Total, fill=default) ) + 
  geom_bar(width = 0.9,stat="identity",) +  
  ylim(c(0,814))+
  labs(x="", y= "Frecuencia") +
  labs(fill = "Churn") +
  geom_text(aes(label=paste0(Total," ", "\n", "(", Porcentaje, "%", ")")),
            vjust=-0.9, 
            color="black", 
            hjust=0.5,
            # define text position and size
            position = position_dodge(1),  
            angle=0, 
            size=5
  )+
  scale_fill_discrete(name = "Churn", labels = c("Si", "No")) +
  theme(axis.text.x = element_text(angle = 0, vjust = 1, hjust=1)) +
  theme_bw(base_size = 14) +
  scale_fill_manual(values = c("#9E9AC8",  "#6A51A3")) + 
  facet_wrap(~"other_credit")



############### ----------- housing ----------- ###############

Tabla_housing <- data %>%
  dplyr::group_by(housing, default) %>%
  dplyr::summarise(Total = n()) %>%
  dplyr::mutate(Porcentaje = round(Total/sum(Total)*100, 1)) %>%
  
  dplyr::arrange(housing)  

ggplot(Tabla_housing, aes(x = housing, y=Total, fill=default) ) + 
  geom_bar(width = 0.9,stat="identity",) +  
  ylim(c(0,786))+
  labs(x="", y= "Frecuencia") +
  labs(fill = "Churn") +
  geom_text(aes(label=paste0(Total," ", "", "(", Porcentaje, "%", ")")),
            vjust=-0.9, 
            color="black", 
            hjust=0.5,
            # define text position and size
            position = position_dodge(0),  
            angle=0, 
            size=5
  )+
  scale_fill_discrete(name = "Churn", labels = c("Si", "No")) +
  theme(axis.text.x = element_text(angle = 0, vjust = 1, hjust=1)) +
  theme_bw(base_size = 14) +
  scale_fill_manual(values = c("#9E9AC8",  "#6A51A3")) + 
  facet_wrap(~"housing")



############### ----------- job ----------- ###############

Tabla_job <- data %>%
  dplyr::group_by(job, default) %>%
  dplyr::summarise(Total = n()) %>%
  dplyr::mutate(Porcentaje = round(Total/sum(Total)*100, 1)) %>%
  
  dplyr::arrange(job)  

ggplot(Tabla_job, aes(x = job, y=Total, fill=default) ) + 
  geom_bar(width = 0.9,stat="identity",) +  
  ylim(c(0,630))+
  labs(x="", y= "Frecuencia") +
  labs(fill = "Churn") +
  geom_text(aes(label=paste0(Total," ", "\n", "(", Porcentaje, "%", ")")),
            vjust=-0.9, 
            color="black", 
            hjust=0.5,
            # define text position and size
            position = position_dodge(1),  
            angle=0, 
            size=5
  )+
  scale_fill_discrete(name = "Churn", labels = c("Si", "No")) +
  theme(axis.text.x = element_text(angle = 0, vjust = 1, hjust=1)) +
  theme_bw(base_size = 14) +
  scale_fill_manual(values = c("#9E9AC8",  "#6A51A3")) + 
  facet_wrap(~"job")



############### ----------- phone ----------- ###############

Tabla_phone <- data %>%
  dplyr::group_by(phone, default) %>%
  dplyr::summarise(Total = n()) %>%
  dplyr::mutate(Porcentaje = round(Total/sum(Total)*100, 1)) %>%
  
  dplyr::arrange(phone)  

ggplot(Tabla_phone, aes(x = phone, y=Total, fill=default) ) + 
  geom_bar(width = 0.9,stat="identity",) +  
  ylim(c(0,596))+
  labs(x="", y= "Frecuencia") +
  labs(fill = "Churn") +
  geom_text(aes(label=paste0(Total," ", "", "(", Porcentaje, "%", ")")),
            vjust=-0.9, 
            color="black", 
            hjust=0.5,
            # define text position and size
            position = position_dodge(0),  
            angle=0, 
            size=5
  )+
  scale_fill_discrete(name = "Churn", labels = c("Si", "No")) +
  theme(axis.text.x = element_text(angle = 0, vjust = 1, hjust=1)) +
  theme_bw(base_size = 14) +
  scale_fill_manual(values = c("#9E9AC8",  "#6A51A3")) + 
  facet_wrap(~"phone")



############### ----------- percent_of_income ----------- ###############

Tabla_percent_of_income <- data %>%
  dplyr::group_by(percent_of_income, default) %>%
  dplyr::summarise(Total = n()) %>%
  dplyr::mutate(Porcentaje = round(Total/sum(Total)*100, 1)) %>%
  
  dplyr::arrange(percent_of_income)  

ggplot(Tabla_percent_of_income, aes(x = percent_of_income, y=Total, fill=default) ) + 
  geom_bar(width = 0.9,stat="identity",) +  
  ylim(c(0,480))+
  labs(x="", y= "Frecuencia") +
  labs(fill = "Churn") +
  geom_text(aes(label=paste0(Total," ", "", "(", Porcentaje, "%", ")")),
            vjust=-0.9, 
            color="black", 
            hjust=0.5,
            # define text position and size
            position = position_dodge(0),  
            angle=0, 
            size=5
  )+
  scale_fill_discrete(name = "Churn", labels = c("Si", "No")) +
  theme(axis.text.x = element_text(angle = 0, vjust = 1, hjust=1)) +
  theme_bw(base_size = 14) +
  scale_fill_manual(values = c("#9E9AC8",  "#6A51A3")) + 
  facet_wrap(~"percent_of_income")



############### ----------- default ----------- ###############

Tabla_default <- data %>%
  dplyr::group_by(default) %>%
  dplyr::summarise(Total = n()) %>%
  dplyr::mutate(Porcentaje = round(Total/sum(Total)*100, 1)) %>%
  
  dplyr::arrange(default)  

ggplot(Tabla_default, aes(x = default, y=Total, fill=default) ) + 
  geom_bar(width = 0.9,stat="identity",) +  
  ylim(c(0,720))+
  labs(x="", y= "Frecuencia") +
  labs(fill = "Churn") +
  geom_text(aes(label=paste0(Total," ", "", "(", Porcentaje, "%", ")")),
            vjust=-0.9, 
            color="black", 
            hjust=0.5,
            # define text position and size
            position = position_dodge(0),  
            angle=0, 
            size=5
  )+
  scale_fill_discrete(name = "Churn", labels = c("Si", "No")) +
  theme(axis.text.x = element_text(angle = 0, vjust = 1, hjust=1)) +
  theme_bw(base_size = 14) +
  scale_fill_manual(values = c("#9E9AC8",  "#6A51A3")) + 
  facet_wrap(~"default")



################ ------------ BOXPLOT ------------ ################



############### ----------- years_at_residence ----------- ###############

boxplot(years_at_residence ~ default, data = data, col = c('indianred3', 'lightblue2'),
        main = "years_at_residence", xlab ='')

############### ----------- existing_loans_count ----------- ###############

boxplot(existing_loans_count ~ default, data = data, col = c('indianred3', 'lightblue2'),
        main = "existing_loans_count", xlab ='')

############### ----------- dependents ----------- ###############

boxplot(dependents ~ default, data = data, col = c('indianred3', 'lightblue2'),
        main = "dependents", xlab ='')


# Se crean dos DataFrame que contienen los clientes que hicieron o no Churn


data_num <- data[ , !(names(data) %in% col_string)]
data_y <- filter(data, default == 'yes')
data_n <- filter(data, default == 'no')
data_num_y <- data_y[ , !(names(data) %in% col_string)]
data_num_n <- data_n[ , !(names(data) %in% col_string)]

#################### ---------------- MEDIDAS ---------------- ####################

summary(data_num)
summary(data_num_y)
summary(data_num_n)

#################### ---------------- HISTOGRAMAS ---------------- ####################

############### ----------- months_loan_duration ----------- ###############

hist_months_loan_duration <- ggplot(data, aes(x=months_loan_duration)) +
  ggtitle("") +
  theme_fivethirtyeight() +
  geom_histogram(color="#28324a", fill="#3c78d8")


hist_months_loan_duration_y <- ggplot(data_num_y, aes(x=months_loan_duration)) +
  ggtitle("YES") +
  theme_fivethirtyeight() +
  geom_histogram(color="#556B2F", fill="#A2CD5A")


hist_months_loan_duration_n <- ggplot(data_num_n, aes(x=months_loan_duration)) +
  ggtitle("NO") +
  theme_fivethirtyeight() +
  geom_histogram(color="#8B1A1A", fill="#F08080")


hist_months_loan_duration/(hist_months_loan_duration_y|hist_months_loan_duration_n) + 
  plot_annotation(title = 'Duracion de Credito (meses)')
  


############### ----------- amount ----------- ###############

hist_amount <- ggplot(data, aes(x=amount)) +
  ggtitle("") +
  theme_fivethirtyeight() +
  geom_histogram(color="#28324a", fill="#3c78d8")


hist_amount_y <- ggplot(data_num_y, aes(x=amount)) +
  ggtitle("YES") +
  theme_fivethirtyeight() +
  geom_histogram(color="#556B2F", fill="#A2CD5A")


hist_amount_n <- ggplot(data_num_n, aes(x=amount)) +
  ggtitle("NO") +
  theme_fivethirtyeight() +
  geom_histogram(color="#8B1A1A", fill="#F08080")


hist_amount/(hist_amount_y|hist_amount_n) + 
  plot_annotation(title = 'Monto del Crédito')



############### ----------- age ----------- ###############

hist_age <- ggplot(data, aes(x=age)) +
  ggtitle("") +
  theme_fivethirtyeight() +
  geom_histogram(color="#28324a", fill="#3c78d8")


hist_age_y <- ggplot(data_num_y, aes(x=age)) +
  ggtitle("YES") +
  theme_fivethirtyeight() +
  geom_histogram(color="#556B2F", fill="#A2CD5A")


hist_age_n <- ggplot(data_num_n, aes(x=age)) +
  ggtitle("NO") +
  theme_fivethirtyeight() +
  geom_histogram(color="#8B1A1A", fill="#F08080")


hist_age/(hist_age_y|hist_age_n) + 
  plot_annotation(title = 'Edad')



#################################################################################



