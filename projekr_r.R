#!/usr/bin/env Rscript
setwd("C:\\Users\\marce\\hakowanie\\programming_R")

install.packages(c("Hmisc", "dplyr", "e1071", "ggpubr", "car", "dunn.test", "FSA"), repos = 'https://cloud.r-project.org/')
library("Hmisc")

####PRZYGOTOWANIE DANYCH WEJSCIOWYCH####
args = commandArgs(trailingOnly=TRUE)
if (length(args)==0) {
  stop("Nalezy podac co najmniej jeden argument wejsciowy")
}

#medical_data <- read.csv2(file=args[1], header = TRUE)
medical_data <- read.csv2("przykladoweDane-Projekt.csv")
medical_data
groups <- unique(medical_data[, 1])
changes <- c()


repair_data <- function(med_data){
  for( i in 1:ncol(med_data)){
    if(is.numeric(med_data[, i])){   
      if(any(is.na(med_data[, i]))){
        fixed_data <- c()
        for(group in groups){
          data_in_group <- med_data[med_data[, 1] == group, i ]
          if(any(is.na(data_in_group))){
            row_num <- which(is.na(data_in_group))
            changes <<- append(changes, c(row_num, group, i))
            avg <- mean(data_in_group, na.rm = TRUE)
            data_in_group <- impute(data_in_group, avg)
          }
          fixed_data <- append(fixed_data, data_in_group)
        }
        med_data[, i] <- fixed_data
      }
    }
    else{
      if(any(is.na(med_data[, i]))){
        fixed_data <- c()
        for(group in groups){
          data_in_group <- med_data[med_data[, 1] == group, i ]
          if(any(is.na(data_in_group))){
            mediana <- median(data_in_group, na.rm = TRUE)
            data_in_group <- impute(data_in_group, mediana)
          }
          fixed_data <- append(fixed_data, data_in_group)
        }
        med_data[, i] <- fixed_data
      }
      
    }
  }
  #med_data <- med_data[complete.cases(med_data), ] #jesli kolumna ma dane nienumeryczne(jakosciowe) to usun wiersz z NA
  return(med_data)
}

if(any(is.na(medical_data))){
  cat("W danych wejœciowych obecne s¹ braki.\n")
  medical_data <- repair_data(medical_data)
  cat("Wprowadzono nastêpuj¹ce zmiany w danych wejœciowych:\n")
  for (i in 1:length(changes)){
    if(i%%3 == 0){
      cat("U pacjenta nr: ", changes[i-2], " w grupie: ", changes[i-1], " w kolumnie: ", changes[i], "\n")
    }
  }
  cat("Uzupe³niono wszystkie puste wartoœci wartoœciami œrednimi dla danej grupy\n")
} else{
  cat("W danych wejœciowych nie ma ¿adnych braków\n")
}
medical_data

###DLA ULATWIENIA###
numeric_columns <- c()                       
for( i in 1:ncol(medical_data)){
  if(is.numeric(medical_data[, i])){
    numeric_columns <- append(numeric_columns, i)
  }
}
numeric_columns
col_names <- colnames(medical_data)
col_names
data_num_rows <- medical_data[, c(numeric_columns[1]:tail(numeric_columns, n=1))]
data_num_rows
num_col_names <- names(data_num_rows)

#outliers <- boxplot.stats(med_data[, 3])$out
#outliers
#medical_data[which(medical_data[,3]==48), 1]
medical_data[which(med_data[, i] %in% c(outliers)),1]

###WARTOSCI ODSTAJACE###
find_outliers <- function(med_data){
  cat("Znaleziono nastêpujace wartoœci odstaj¹ce: \n")
  for( i in 1:ncol(med_data)){
    if(i %in% numeric_columns){ 
      cat("Dla parametru: ", col_names[i], "\n")
      outliers <- boxplot.stats(med_data[, i])$out
      if(length(outliers)==0){
        cat("Brak wartoœci odstaj¹cych\n")
        next
      }
      for(out in outliers){
        cat("wartoœci: ", outliers, "\n", "kolejno w wierszach: ", which(med_data[, i] %in% c(outliers)), "w grupach: ", medical_data[which(med_data[, i] %in% c(outliers)),1], "\n")
        #cat(out, "w grupie: ", medical_data[which(medical_data[,i]==out), 1] ,"\n")
      }
    }
  }
}
find_outliers(medical_data)

pdf("Outliers.pdf")
for(i in 1:length(data_num_rows)){
  boxplot(data_num_rows[, i], xlab = names(data_num_rows[i]), ylab = 'wartoœc', col = 'skyblue', outcol = 'red', outcex = 2, outpch = 24)
}
dev.off()


####STATYSTYKI####
library("dplyr")
library("e1071")

cat("Charakterystyki ka¿dego z parametrów wœród grup: \n")
for(col in numeric_columns){
  sum <- group_by(medical_data, grupa) %>%
    summarise(
      licznosc = n(),
      max =  format(round(max(medical_data[medical_data[, 1] == grupa, col], na.rm = TRUE), 2), nsmall = 2),
      min =  format(round(min(medical_data[medical_data[, 1] == grupa, col], na.rm = TRUE), 2), nsmall = 2),
      srednia = format(round(mean(medical_data[medical_data[, 1] == grupa, col], na.rm = TRUE), 2), nsmall = 2),
      mediana = format(round(median(medical_data[medical_data[, 1] == grupa, col], na.rm = TRUE), 2), nsmall = 2),
      wariancja = format(round(var(medical_data[medical_data[, 1] == grupa, col], na.rm = TRUE), 2), nsmall = 2),
      odchylenie_st = format(round(sd(medical_data[medical_data[, 1] == grupa, col], na.rm = TRUE), 2), nsmall = 2),
      iqr = format(round(IQR(medical_data[medical_data[, 1] == grupa, col], na.rm = TRUE), 2), nsmall = 2),
      kurtoza =  format(round(kurtosis(medical_data[medical_data[, 1] == grupa, col], na.rm = TRUE), 2), nsmall = 2),
      skosnosc =  format(round(skewness(medical_data[medical_data[, 1] == grupa, col], na.rm = TRUE), 2), nsmall = 2),
    )
  cat("Paramter: ", col_names[col])
  print(sum)
  cat("\n\n")
}


####ANALIZA POROWNAWCZA####
normal <- c()
shapiro_tests <- list()
for(col in numeric_columns){
  pvalueShapiroTest <- group_by(medical_data, grupa) %>%
    summarise(
      p.value = shapiro.test(medical_data[medical_data$grupa == grupa, col ])$p.value
    )
  #print(pvalueShapiroTest)
  shapiro_tests <- append(shapiro_tests, list(pvalueShapiroTest))
  if(any(pvalueShapiroTest$p.value<0.05)) normal <- append(normal, 0)
  else normal <- append(normal, 1)
}
normal
shapiro_tests

shapiro_tests_df <- data.frame()
for(i in 1:length(shapiro_tests)){
  shapiro_tests_df <- rbind(shapiro_tests_df,shapiro_tests[[i]]$p.value)
}
rownames(shapiro_tests_df) <- num_col_names
colnames(shapiro_tests_df) <- groups
shapiro_tests_df

library("ggpubr")

pdf("Shapiro.pdf")
for(col in numeric_columns){
  densplot <- ggdensity(medical_data, x = col_names[col], fill = "grupa", color = "grupa", add = "mean",
            xlab = col_names[col], palette = c("#D6E121", "#EC4E13", "#96EA68")) + facet_wrap(~grupa, scales = 'free')
  print(densplot)
}
dev.off()

homo_var <- c()
library("car")
levene_tests <- lapply(medical_data[, c(num_col_names)], function(x){
  leveneTest(x ~ medical_data$grupa)
  })
#levene_tests
for(col in num_col_names){
  if(levene_tests[[col]]$`Pr(>F)`[1] > 0.05) homo_var <- append(homo_var, 1)
  else homo_var <- append(homo_var, 0)
}
#homo_var

data_character <- data.frame(norm = normal, homo_va = homo_var)
rownames(data_character) <- num_col_names
data_character


##POROWNYWANIE GRUP NIEZALEZNYCH##
library("dunn.test")
library("FSA")
comp = list(c("CHOR1", "CHOR2"), c("CHOR1", "KoNTROLA"), c("CHOR2", "KONTROLA"))
pdf("roznice.pdf")
for(col in numeric_columns){
  
  if(length(groups) > 2){#WIECEJ NIZ 2 GRUPY
    
    if(data_character[names(medical_data[col]), 1] == TRUE){ #ZGODNOSC Z ROZKLADEM NORMALNYM
      
      if(data_character[names(medical_data[col]), 2] == TRUE){ #ZGODNOSC Z HOMOGEN WARIANCJI
        aovTest <- summary(aov(medical_data[[col]] ~ grupa, data = medical_data))
        pvalueAOVtest <- aovTest[[1]]$`Pr(>F)`[1]
        if(pvalueAOVtest < 0.05){
          cat("Pomiêdzy grupami w parametrze: ", col_names[col], " istniej¹ istotne statystycznie ró¿nice\n")
          print(TukeyHSD(aov(medical_data[[col]] ~ grupa, data = medical_data)))
          print(ggplot(medical_data, aes(x=grupa, y=medical_data[,col], fill=grupa)) + 
            geom_boxplot() + ggtitle(paste("Parametr: ", col_names[col], " w zale¿noœci od grupy")) + xlab("grupa") + ylab(col_names[col]) +
            theme(text = element_text(size = 13)) + stat_compare_means(comparisions = comp))
        }
        else{
          cat("Pomiêdzy grupami w parametrze: ", col_names[col], " brak istotnych statystycznie ró¿nic\n")
        }
      }
      else{ #BRAK ZGODNOSCI Z HOMOGEN WARIANCJI
        pvalueKWtest <- kruskal.test(medical_data[[col]] ~ grupa, data = medical_data)$p.value
        if(pvalueKWtest < 0.05){
          cat("Pomiêdzy grupami w parametrze: ", col_names[col], " istniej¹ istotne statystycznie ró¿nice\n")
          print(dunnTest(medical_data[, col], medical_data$grupa))
          cat("\n")
          print(ggplot(medical_data, aes(x=grupa, y=medical_data[,col], fill=grupa)) + 
            geom_boxplot() + ggtitle(paste("Parametr: ", col_names[col], " w zale¿noœci od grupy")) + xlab("grupa") + ylab(col_names[col]) +
            theme(text = element_text(size = 13)) + stat_compare_means(comparisions = comp))
        }
        else{
          cat("Pomiêdzy grupami w parametrze: ", col_names[col], " brak istotnych statystycznie ró¿nic\n")
        }
      }
    }
    else{  #BRAK ZGODNOSCI Z ROZKLADEM NORMALNYM
      pvalueKWtest <- kruskal.test(medical_data[[col]] ~ grupa, data = medical_data)$p.value
      if(pvalueKWtest < 0.05){
        cat("Pomiêdzy grupami w parametrze: ", col_names[col], " istniej¹ istotne statystycznie ró¿nice\n")
        print(dunnTest(medical_data[, col], medical_data$grupa))
        cat("\n")
        print(ggplot(medical_data, aes(x=grupa, y=medical_data[,col], fill=grupa)) + 
          geom_boxplot() + ggtitle(paste("Parametr: ", col_names[col], " w zale¿noœci od grupy")) + xlab("grupa") + ylab(col_names[col]) +
          theme(text = element_text(size = 13)) + stat_compare_means(comparisions = comp))
      }
      else{
        cat("Pomiêdzy grupami w parametrze: ", col_names[col], " brak istotnych statystycznie ró¿nic\n")
      }
    }
  }
  else{#2 GRUPY
    
    if(data_character[names(medical_data[col]), 1] == TRUE){ #ZGODNOSC Z ROZKLADEM NORMALNYM
      
      if(data_character[names(medical_data[col]), 2] == TRUE){ #ZGODNOSC Z HOMOGEN WARIANCJI
        pvalueTtest <- t.test(medical_data[[col]] ~ grupa, data = medical_data, var.equal = TRUE)$p.value
        if(pvalueTtest < 0.05){
          cat("Pomiêdzy grupami w parametrze: ", col_names[col], " istniej¹ istotne statystycznie ró¿nice\n")
          print(ggplot(medical_data, aes(x=grupa, y=medical_data[,col], fill=grupa)) + 
            geom_boxplot() + ggtitle(paste("Parametr: ", col_names[col], " w zale¿noœci od grupy")) + xlab("grupa") + ylab(col_names[col]) +
            theme(text = element_text(size = 13)))
        }
        else{
          cat("Pomiêdzy grupami w parametrze: ", col_names[col], " brak istotnych statystycznie ró¿nic\n")
        }
      }
      else{ #BRAK ZGODNOSCI Z HOMOGEN WARIANCJI
        pvalueTtest <- t.test(medical_data[[col]] ~ grupa, data = medical_data, var.equal = FALSE)$p.value
        if(pvalueTtest < 0.05){
          cat("Pomiêdzy grupami w parametrze: ", col_names[col], " istniej¹ istotne statystycznie ró¿nice\n")
          print(ggplot(medical_data, aes(x=grupa, y=medical_data[,col], fill=grupa)) + 
            geom_boxplot() + ggtitle(paste("Parametr: ", col_names[col], " w zale¿noœci od grupy")) + xlab("grupa") + ylab(col_names[col]) +
            theme(text = element_text(size = 13)))
        }
        else{
          cat("Pomiêdzy grupami w parametrze: ", col_names[col], " brak istotnych statystycznie ró¿nic\n")
        }
      }
    } 
    else{  #BRAK ZGODNOSCI Z ROZKLADEM NORMALNYM
      pvalueWilcoxtest <- wilcox.test(medical_data[[col]] ~ grupa, data = medical_data)$p.value
      if(pvalueWilcoxtest < 0.05){
        cat("Pomiêdzy grupami w parametrze: ", col_names[col], " istniej¹ istotne statystycznie ró¿nice\n")
        print(ggplot(medical_data, aes(x=grupa, y=medical_data[,col], fill=grupa)) + 
          geom_boxplot() + ggtitle(paste("Parametr: ", col_names[col], " w zale¿noœci od grupy")) + xlab("grupa") + ylab(col_names[col]) +
          theme(text = element_text(size = 13)))
      }
      else{
        cat("Pomiêdzy grupami w parametrze: ", col_names[col], " brak istotnych statystycznie ró¿nic\n")
      }
    }
  }
}
dev.off()

if(col_names[1]=="grupa" && col_names[2]=="plec"){
  cat("iloœciowy udzia³ p³ci w poszczególych grupach: \n")
  print(table(medical_data$grupa, medical_data$plec))
  pvalueChisq <- chisq.test(medical_data$grupa, medical_data$plec)$p.value
  if(pvalueChisq > 0.05){
    cat("\nBrak istotnych ró¿nic pomiêdzy grupami dla parametru: p³ec\n")
  }
  else cat("Dla parametru p³ec widoczne s¹ istotne statystycznie ró¿nice miêdzy grupami\n")
  
  pdf("gr-pl.pdf")
  barplot(table(medical_data$plec, medical_data$grupa),
          ylim = c(0,20),
          beside = TRUE,
          col = c("#AC80E9", "#69A349"),
          xlab = "grupa",
          ylab = "p³ec",
          legend = c("kobieta", "mê¿czyzna")
          )
  text(3.4, 19, paste("p-value: ", round(pvalueChisq, digits = 2)))
  dev.off()
}


####ANALIZA KORELACJI####

pdf("korelacja.pdf")
for(col in numeric_columns){
  for(coll in numeric_columns){
    
    if(col >= coll){
      next
    }
    corelation <- group_by(medical_data, grupa) %>%
      summarise(
        estimate = if(shapiro_tests_df[col_names[col], grupa] > 0.05 && shapiro_tests_df[col_names[coll], grupa] > 0.05){
          cor.test(medical_data[medical_data$grupa == grupa, col], medical_data[medical_data$grupa == grupa, coll], method = "pearson")$estimate
        }
        else{
          cor.test(medical_data[medical_data$grupa == grupa, col], medical_data[medical_data$grupa == grupa, coll], method = "spearman")$estimate
        },
        pvalue = if(shapiro_tests_df[col_names[col], grupa] > 0.05 && shapiro_tests_df[col_names[coll], grupa] > 0.05){
          cor.test(medical_data[medical_data$grupa == grupa, col], medical_data[medical_data$grupa == grupa, coll], method = "pearson")$p.value
        }
        else{
          cor.test(medical_data[medical_data$grupa == grupa, col], medical_data[medical_data$grupa == grupa, coll], method = "spearman")$p.value
        },
      )
    if(any(corelation$pvalue < 0.05)){
      relevant <- which(corelation$pvalue < 0.05)
      lapply(relevant, function(x){
        cat("Wykazano Korelacjê dla parametru: ", col_names[col], " i ", col_names[coll], " w grupie: ", corelation$grupa[x], "\n")
        cat("p-value: ", corelation$pvalue[x], " r-value: ", corelation$estimate[x], "\n")
        if(corelation$estimate[x] > -1 && corelation$estimate[x] < -0.7) {cat("korelacja bardzo silnie ujemna\n\n")}
        else if(corelation$estimate[x] > -0.7 && corelation$estimate[x] < -0.5) {cat("korelacja silnie ujemna\n\n")}
        else if(corelation$estimate[x] > -0.5 && corelation$estimate[x] < -0.3) {cat("korelacja ujemna o œrednim natê¿eniu\n\n")}
        else if(corelation$estimate[x] > -0.3 && corelation$estimate[x] < -0.2) {cat("korelacja s³abo ujemna\n\n")}
        else if(corelation$estimate[x] > -0.2 && corelation$estimate[x] < 0.2) {cat("brak korelacji\n\n")}
        else if(corelation$estimate[x] > 0.2 && corelation$estimate[x] < 0.3) {cat("korelacja d³abo dodatnia\n\n")}
        else if(corelation$estimate[x] > 0.3 && corelation$estimate[x] < 0.5) {cat("korelacja dodatnia o œrednim natê¿eniu\n\n")}
        else if(corelation$estimate[x] > 0.5 && corelation$estimate[x] < 0.7) {cat("korelacja silnie dodatnia\n\n")}
        else if(corelation$estimate[x] > 0.7 && corelation$estimate[x] < 1) {cat("korelacja bardzo silnie dodatnia\n\n")}
        if(shapiro_tests_df[col_names[col], corelation$grupa[x]] > 0.05 && shapiro_tests_df[col_names[coll],corelation$grupa[x]] > 0.05){
          print(ggscatter(medical_data[medical_data[, 1] == corelation$grupa[x], ], x = col_names[col], y = col_names[coll], add = 'reg.line', conf.int = TRUE,
                    cor.coef = TRUE, cor.method = 'pearson', color = 'grupa', fill = 'grupa', xlab = col_names[col], ylab = col_names[coll]))
        }else{
          print(ggscatter(medical_data[medical_data[, 1] == corelation$grupa[x], ], x = col_names[col], y = col_names[coll], conf.int = TRUE, 
                    cor.coef = TRUE, cor.method = 'spearman', color = 'grupa', fill = 'grupa', xlab = col_names[col], ylab = col_names[coll]))
        }
      })
    }
  } 
}
dev.off()

