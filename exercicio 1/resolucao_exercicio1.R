# Metodologia de Survey e Aplicacoes Eleitorais
# Exercicio 1 - Joao Paulo Zangrandi

set.seed(123)

if (!file.exists("macae.rds")) {
  download.file(
    "http://www.felipenunescp.com/uploads/1/4/4/2/144237945/macae.rds",
    "macae.rds",
    mode = "wb"
  )
}

dados <- readRDS("macae.rds")
names(dados)[names(dados) == "alfabetizado"] <- "alfabetizada"

n <- 1200
B <- 1000
prop_pop <- mean(dados$alfabetizada)

aas <- function() {
  pessoas <- sample(1:nrow(dados), n)
  mean(dados$alfabetizada[pessoas])
}

taxa_bairro <- aggregate(alfabetizada ~ bairro, dados, mean)
taxa_bairro$estrato <- cut(
  taxa_bairro$alfabetizada,
  breaks = quantile(taxa_bairro$alfabetizada, seq(0, 1, 0.25)),
  include.lowest = TRUE,
  labels = c("Baixa", "Media baixa", "Media alta", "Alta")
)

dados <- merge(dados, taxa_bairro[, c("bairro", "estrato")], by = "bairro")
linhas_estrato <- split(1:nrow(dados), dados$estrato)
Nh <- sapply(linhas_estrato, length)

nh_decimal <- n * Nh / sum(Nh)
nh <- floor(nh_decimal)
resto <- n - sum(nh)
if (resto > 0) {
  ajuste <- order(nh_decimal - nh, decreasing = TRUE)[1:resto]
  nh[ajuste] <- nh[ajuste] + 1
}

estratificada <- function() {
  pessoas <- c()
  for (e in names(linhas_estrato)) {
    pessoas <- c(pessoas, sample(linhas_estrato[[e]], nh[e]))
  }
  mean(dados$alfabetizada[pessoas])
}

entrevistas_por_setor <- 10
setores_na_amostra <- n / entrevistas_por_setor
linhas_setor <- split(1:nrow(dados), dados$codigo_setor)
setores_validos <- names(linhas_setor)[sapply(linhas_setor, length) >= entrevistas_por_setor]

conglomerados <- function() {
  setores <- sample(setores_validos, setores_na_amostra)
  pessoas <- c()
  for (s in setores) {
    pessoas <- c(pessoas, sample(linhas_setor[[s]], entrevistas_por_setor))
  }
  mean(dados$alfabetizada[pessoas])
}

sim <- data.frame(
  AAS = replicate(B, aas()),
  Estratificada = replicate(B, estratificada()),
  Conglomerados = replicate(B, conglomerados())
)

medias <- sapply(sim, mean)
variancias <- sapply(sim, var)

resultados <- data.frame(
  desenho = names(medias),
  media_simulada = as.numeric(medias),
  variancia_simulada = as.numeric(variancias),
  design_effect = c(1, variancias["Estratificada"] / variancias["AAS"],
                    variancias["Conglomerados"] / variancias["AAS"])
)

estratos <- data.frame(
  estrato = names(Nh),
  populacao = as.numeric(Nh),
  amostra = as.numeric(nh)
)

write.csv(resultados, "resultados_amostragem.csv", row.names = FALSE)
write.csv(estratos, "estratos_amostragem.csv", row.names = FALSE)

png("grafico_distribuicoes.png", width = 1800, height = 1200, res = 200)
plot(
  density(sim$AAS),
  col = "#2166ac",
  lwd = 3,
  xlim = range(sim),
  main = "Distribuição das proporções simuladas de alfabetizados",
  xlab = "Proporção de alfabetizados na amostra",
  ylab = "Densidade"
)
lines(density(sim$Estratificada), col = "#1b7837", lwd = 3)
lines(density(sim$Conglomerados), col = "#b2182b", lwd = 3)
abline(v = prop_pop, lty = 2, lwd = 2)
legend(
  "topright",
  legend = c("AAS", "Estratificada", "Conglomerados", "Proporção populacional"),
  col = c("#2166ac", "#1b7837", "#b2182b", "black"),
  lwd = c(3, 3, 3, 2),
  lty = c(1, 1, 1, 2),
  bty = "n"
)
dev.off()

cat("Proporcao populacional:", round(prop_pop, 4), "\n")
print(resultados)
print(estratos)
