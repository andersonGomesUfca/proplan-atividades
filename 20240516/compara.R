## compração entre duas series

rm(list = ls())

## 

require("dplyr")
require("stringr")
require("tidyr")

caminho = 
  paste0("C:/Users/ander/Desktop/estatistica/Estudos soltos/",
  "Testes aleatórios/jaquecgs/comparacao_chuvas/")
list.files(caminho)

d1 = read.csv(paste0(caminho, 'R10_CHIRPS_id1.csv'))
d2 = read.csv(paste0(caminho, "R10_ERA5_id1.csv"))
d3 = read.csv(paste0(caminho, "R10_PERSIANN-CDR_id1.csv"))
d4 = read.csv(paste0(caminho, "R10_ReferenceData_id1.csv"))

dados = left_join(d1, d2, by = c("system.index" = "system.index"))
dados = left_join(dados, d3, by = c("system.index" = "system.index"))
dados = left_join(dados, d4, by = c("system.index" = "system.index"))

colnames(dados)

colnames(dados) = str_replace(colnames(dados), "(?<!\\.\\w)\\.x$", ".1")
colnames(dados) = str_replace(colnames(dados), "(?<!\\.\\w)\\.y$", ".2")

colnames(dados) = str_replace(colnames(dados), "(?<!\\.\\w)\\.x\\.x$", ".3")
colnames(dados) = str_replace(colnames(dados), "(?<!\\.\\w)\\.y\\.y$", ".4")

###

ind = grepl("geo", colnames(dados))
dados = dados[,!ind]
ind = grepl("year\\.[2-4]", colnames(dados))
dados = dados[,!ind]

## remover os NAs

dados = dados[!is.na(dados$prec.1),]
dados = dados[!is.na(dados$prec.2),]
dados = dados[!is.na(dados$prec.3),]
dados = dados[!is.na(dados$prec.4),]

head(dados)

###

dados2 = gather(dados, "grupo", "valor", -c(1,3))

####

anos_unicos = unique(dados2$year.1)
d_out = c()

aux = c()
ind = c()
y_max = c()

x_max = 
  c(dados$prec.1, dados$prec.2, dados$prec.3, dados$prec4)
x_max = mean(x_max) + 2*sqrt(var(x_max))

dados_graf = c()

i = 1
for( i in 1:length(anos_unicos) ){
  
  ## plotando e exportando
  ind = dados$year.1 == anos_unicos[i]
  table(ind)
  
  y_max =
    max(c(density(dados$prec.1[ind])$y, density(dados$prec.2[ind])$y,
          density(dados$prec.3[ind])$y, density(dados$prec.4[ind])$y))
  # x_max =
  #   (c(dados$prec.1[ind], dados$prec.2[ind], dados$prec.3[ind], dados$prec4[ind]))
  # x_max = x_max + x_max/5
  # x_max = mean(x_max) + 1.5*sqrt(var(x_max))
  
  jpeg(paste0(caminho, "img/densidade_", anos_unicos[i], ".jpeg"),
       height = 400, width = 450*2)
  
  plot(density(dados$prec.1[ind]), ylim = c(0, y_max), xlim = c(0, x_max),
       col = "orange",
       main = paste0("Medições de ", anos_unicos[i]),
       xlab = "Medição", ylab = "densidade")
  lines(density(dados$prec.2), col = "red")
  lines(density(dados$prec.3), col = "blue")
  lines(density(dados$prec.4), col = "black")
  
  legend(x_max - 20, y_max, legend=c("CHIRPS", "ERA5", "PERSIANN-CDR", "ReferenceData"),
         col=c("orange", "red", "blue", "black"), lty=1, cex=0.6, plot = T)
  
  dev.off()
  
  ?legend
  
  
  for( j in 1:3 ){
    for( k in (j+1):4 ){
      x = dados2[dados2$year.1 == anos_unicos[i] & 
                   dados2$grupo %in% c( paste0("prec.", j), paste0("prec.", k) ), ]
      d_out = c(d_out,
                t.test(formula = valor ~ grupo,
                       data = x,
                       paired = T,
                       var.equal = T,
                       conf.level = 0.95)$p.value
                )
    }
  }
}

d_out = round(d_out,6)
d_out = matrix(d_out, ncol = choose(4,2), byrow = T)
d_out
colnames(d_out) = c("CHIRPS_x_ERA5",
                    "CHIRPS_x_PERSIANN",
                    "CHIRPS_x_ReferenceData",
                    "ERA5_x_PERSIANN",
                    "ERA5_x_ReferenceData",
                    "PERSIANN_x_ReferenceData"
                    )

d_out = data.frame(d_out, stringsAsFactors = F)
d_out = cbind(anos_unicos, d_out)

d_out
x = dados2[dados2$year.1 == "1987",]
x = x[x$grupo %in% c("prec.1", "prec.4"),]

z = t.test(formula = valor ~ grupo,
       data = x,
       paired = T,
       var.equal = T,
       conf.level = 0.95)
z$p.value
