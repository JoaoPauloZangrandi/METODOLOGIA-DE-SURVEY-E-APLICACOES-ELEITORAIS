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
  sorteio <- sample(1:nrow(dados), n)
  mean(dados$alfabetizada[sorteio])
}

taxa_bairro <- aggregate(alfabetizada ~ bairro, dados, mean)
taxa_bairro$estrato <- cut(
  taxa_bairro$alfabetizada,
  quantile(taxa_bairro$alfabetizada, seq(0, 1, 0.25)),
  include.lowest = TRUE
)

dados <- merge(dados, taxa_bairro[, c("bairro", "estrato")], by = "bairro")
linhas_estrato <- split(1:nrow(dados), dados$estrato)
Nh <- sapply(linhas_estrato, length)
nh <- round(n * Nh / sum(Nh))
nh[1] <- nh[1] + n - sum(nh)

estratificada <- function() {
  sorteio <- c()
  for (i in names(linhas_estrato)) {
    sorteio <- c(sorteio, sample(linhas_estrato[[i]], nh[i]))
  }
  mean(dados$alfabetizada[sorteio])
}

entrevistas_por_setor <- 10
setores_na_amostra <- n / entrevistas_por_setor
linhas_setor <- split(1:nrow(dados), dados$codigo_setor)
setores_validos <- names(linhas_setor)[sapply(linhas_setor, length) >= entrevistas_por_setor]

conglomerados <- function() {
  setores <- sample(setores_validos, setores_na_amostra)
  sorteio <- c()
  for (s in setores) {
    sorteio <- c(sorteio, sample(linhas_setor[[s]], entrevistas_por_setor))
  }
  mean(dados$alfabetizada[sorteio])
}

sim <- data.frame(
  AAS = replicate(B, aas()),
  Estratificada = replicate(B, estratificada()),
  Conglomerados = replicate(B, conglomerados())
)

medias <- sapply(sim, mean)
variancias <- sapply(sim, var)
deff_est <- variancias["Estratificada"] / variancias["AAS"]
deff_cong <- variancias["Conglomerados"] / variancias["AAS"]

print(round(medias, 4))
print(signif(variancias, 4))
cat("Design effect estratificada:", round(deff_est, 3), "\n")
cat("Design effect conglomerados:", round(deff_cong, 3), "\n")

pdf("resposta_exercicio1.pdf", width = 8.27, height = 11.69)

pagina <- function(linhas) {
  plot.new()
  text(0, seq(1, 0.08, length.out = length(linhas)), adj = c(0, 1),
       cex = 0.85, linhas)
}

pagina(c(
  "Tarefa I: Exercicio 1 - Amostragem",
  "Autor: Joao Paulo Zangrandi",
  "",
  paste0("Populacao: ", nrow(dados), " pessoas. Proporcao real de alfabetizados: ", round(prop_pop, 4), "."),
  "",
  "Plano 1 - AAS: sorteio aleatorio simples de 1200 pessoas, sem reposicao.",
  "",
  "Plano 2 - Estratificado: bairros agrupados em quatro estratos pela taxa media de alfabetizacao.",
  paste0("A alocacao foi proporcional: ", paste(nh, collapse = ", "), " entrevistas por estrato."),
  "",
  paste0("Plano 3 - Conglomerados: ", setores_na_amostra, " setores censitarios e ",
         entrevistas_por_setor, " pessoas em cada setor.")
))

plot(density(sim$AAS),
     col = "blue", lwd = 2, xlim = range(sim),
     main = "Distribuicao das proporcoes simuladas",
     xlab = "Proporcao de alfabetizados", ylab = "Densidade")
lines(density(sim$Estratificada), col = "darkgreen", lwd = 2)
lines(density(sim$Conglomerados), col = "red", lwd = 2)
abline(v = prop_pop, lty = 2)
legend("topright",
       c("AAS", "Estratificada", "Conglomerados", "Populacao"),
       col = c("blue", "darkgreen", "red", "black"),
       lwd = c(2, 2, 2, 1), lty = c(1, 1, 1, 2), bty = "n")

pagina(c(
  "Resultados",
  "",
  paste0("Medias: AAS = ", round(medias["AAS"], 4),
         "; estratificada = ", round(medias["Estratificada"], 4),
         "; conglomerados = ", round(medias["Conglomerados"], 4), "."),
  paste0("Variancias: AAS = ", signif(variancias["AAS"], 4),
         "; estratificada = ", signif(variancias["Estratificada"], 4),
         "; conglomerados = ", signif(variancias["Conglomerados"], 4), "."),
  paste0("Design effect: estratificada = ", round(deff_est, 3),
         "; conglomerados = ", round(deff_cong, 3), "."),
  "",
  "A estratificacao reduziu a variancia porque junta bairros com niveis parecidos de alfabetizacao.",
  "Os conglomerados aumentaram a variancia porque pessoas do mesmo setor tendem a ser parecidas."
))

dev.off()
