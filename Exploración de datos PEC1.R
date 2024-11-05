datos = read.csv("https://rest.xialab.ca/api/download/metaboanalyst/human_cachexia.csv") #se prepara archivo de datos


#se descargan los recursos que se emplearán en los siguientes apartados
if (!requireNamespace("ggplot2", quietly = TRUE))
  install.packages("ggplot2")

#HISTOGRAMA

library(ggplot2)
subset_control = subset(datos, Muscle.loss == "control") #se hace un subset con los valores del grupo control
medias_control = colMeans(subset_control[, sapply(subset_control, is.numeric)], na.rm = TRUE) #se hacen las medias de cada metabolito
medias_control_list = unname(medias_control) #se obtiene una lista con los valores numéricos solo
subset_cachexic = subset(datos, Muscle.loss == "cachexic") #idem con cachexic
medias_cachexic = colMeans(subset_cachexic[, sapply(subset_cachexic, is.numeric)], na.rm = TRUE)
medias_cachexic_list = unname(medias_cachexic)


medias_df = data.frame( #se convierte en un dataframe comparativo, donde se incluyo el metabolito, el valor que tiene, y el grupo al que corresponde
  Metabolito = rep(names(means_control), 2),
  Valor_medio = c(medias_control, medias_cachexic),
  Group = rep(c("control", "cachexia"), each = length(medias_control))
)

#Se realiza histograma comparando ambos
ggplot(medias_df, aes(x = Metabolito, y = Valor_medio, fill = Group)) +
  geom_histogram(stat = "identity", position = "dodge", alpha = 0.5) +
  labs(title = "Histograma de Meyabolitos según Grupo", x = "Metabolito", y = "Valor medio") +
  scale_fill_manual(name = "Group", values = c("control" = "blue", "cachexia" = "red")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


#ANÁLISIS DE DATOS DE DIFERENCIAS SIGNIFICATIVAS (asumiendo normalidad de datos)
grupos = datos_expr$Muscle.loss #se definen los grupos 
resultados = data.frame( #se define el dataframe donde estarán los resultados, conteniendo el metabolito donde hay diferencias, su pvalue y este una vez ajustado
  Metabolite = colnames(datos_expr),
  p_value = NA,
  p_ajustado = NA 
)

for (i in 2:ncol(datos_expr)) { #para todo menos la columna de los grupos
  metabolite_values = datos_expr[, i] #para cada metabolito
  test = t.test(metabolite_values ~ grupos) #realizar t-test
  resultados$p_value[i] = test$p.value #se almacena el pvalue
}

resultados$p_ajustado = p.adjust(resultados$p_value, method = "fdr") #se ajustan las p por fdr

significant_metabolites = resultados[resultados$p_ajustado < 0.05, ] #se escogen los valores significativos
significant_metabolites = na.omit(significant_metabolites) #se eliminan los que tengan NA
resultado_final = significant_metabolites$Metabolite #se guarda la lista de metabolitos donde hay diferencias significativas entre grupos
numero_metabolitos_difsig = length(resultado_final) #se cuentan cuantos distintos son


#ANÁLISIS DE COMPONENTES PRINCIPALES

library(ggplot2)

datos_expr_escala = scale(datos_expr[,-1]) #se normalizan los datos para que esten en posiciones relativas para el gráfico 
pca_result = prcomp(datos_expr_escala, center = TRUE, scale. = TRUE) #se hace el análisis


pca_data = as.data.frame(pca_result$x) #se crea un dataframe con los resultados, definiendo los dos grupos
pca_data$Group = datos_expr$Muscle.loss


ggplot(pca_data, aes(x = PC1, y = PC2, color = Group)) + #se hace el gráfico del análisis de componente sprincipales
  geom_point(size = 3) +
  labs(title = "PCA de Metabolitos en Control y Caquexia",
       x = "PC1", y = "PC2") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))




