# Metodologia de Survey e Aplicacoes Eleitorais
# Professor: Felipe Nunes
# Exercicio 1: Amostragem
# Autor: Joao Paulo Zangrandi

set.seed(123)

arquivo <- "macae.rds"
url <- "http://www.felipenunescp.com/uploads/1/4/4/2/144237945/macae.rds"

if (!file.exists(arquivo)) {
  download.file(url, arquivo, mode = "wb")
}

dados <- readRDS(arquivo)

if ("alfabetizado" %in% names(dados)) {
  dados$alfabetizada <- dados$alfabetizado
}

N <- nrow(dados)
n <- 1200
prop_pop <- mean(dados$alfabetizada)

# Plano 1: amostragem aleatoria simples
amostra_aas <- function() {
  mean(dados$alfabetizada[sample(1:N, n)])
}

# Plano 2: AAS estratificada.
# Os estratos sao formados agrupando bairros com taxas parecidas de alfabetizacao.
taxa_bairro <- aggregate(alfabetizada ~ bairro, dados, mean)
taxa_bairro$estrato <- cut(
  taxa_bairro$alfabetizada,
  breaks = quantile(taxa_bairro$alfabetizada, probs = seq(0, 1, 0.25)),
  include.lowest = TRUE,
  labels = c("baixa", "media baixa", "media alta", "alta")
)

dados <- merge(dados, taxa_bairro[, c("bairro", "estrato")], by = "bairro", all.x = TRUE)
linhas_por_estrato <- split(1:nrow(dados), dados$estrato)
Nh <- sapply(linhas_por_estrato, length)

nh_decimal <- n * Nh / sum(Nh)
nh <- floor(nh_decimal)
resto <- n - sum(nh)
if (resto > 0) {
  maiores_restos <- order(nh_decimal - nh, decreasing = TRUE)[1:resto]
  nh[maiores_restos] <- nh[maiores_restos] + 1
}

amostra_estratificada <- function() {
  escolhidos <- c()
  for (e in names(linhas_por_estrato)) {
    escolhidos <- c(escolhidos, sample(linhas_por_estrato[[e]], nh[e]))
  }
  mean(dados$alfabetizada[escolhidos])
}

# Plano 3: amostragem por conglomerados.
# Cada setor censitario selecionado contribui com 10 entrevistas.
entrevistas_por_setor <- 10
setores_necessarios <- n / entrevistas_por_setor
tamanho_setor <- table(dados$codigo_setor)
setores_validos <- names(tamanho_setor[tamanho_setor >= entrevistas_por_setor])
linhas_por_setor <- split(1:nrow(dados), dados$codigo_setor)

amostra_conglomerados <- function() {
  setores <- sample(setores_validos, setores_necessarios)
  escolhidos <- c()
  for (s in setores) {
    escolhidos <- c(escolhidos, sample(linhas_por_setor[[s]], entrevistas_por_setor))
  }
  mean(dados$alfabetizada[escolhidos])
}

B <- 1000
resultado <- data.frame(
  AAS = replicate(B, amostra_aas()),
  Estratificada = replicate(B, amostra_estratificada()),
  Conglomerados = replicate(B, amostra_conglomerados())
)

variancias <- sapply(resultado, var)
medias <- sapply(resultado, mean)
vies <- medias - prop_pop
deff_estratificada <- variancias["Estratificada"] / variancias["AAS"]
deff_conglomerados <- variancias["Conglomerados"] / variancias["AAS"]

cat("Proporcao populacional de alfabetizados:", round(prop_pop, 4), "\n")
cat("Medias simuladas:\n")
print(round(medias, 4))
cat("Variancias simuladas:\n")
print(signif(variancias, 4))
cat("Design effect - estratificada:", round(deff_estratificada, 3), "\n")
cat("Design effect - conglomerados:", round(deff_conglomerados, 3), "\n")
cat("Tamanhos dos estratos:\n")
print(Nh)
cat("Amostra por estrato:\n")
print(nh)

pdf("resposta_exercicio1.pdf", width = 8.27, height = 11.69)

library(grid)

pagina_texto <- function(titulo, texto) {
  grid.newpage()
  grid.text(titulo, x = 0.08, y = 0.94, just = c("left", "top"),
            gp = gpar(fontsize = 16, fontface = "bold"))
  linhas <- unlist(strwrap(texto, width = 90))
  y <- 0.88
  for (linha in linhas) {
    grid.text(linha, x = 0.08, y = y, just = c("left", "top"),
              gp = gpar(fontsize = 10.5))
    y <- y - 0.026
    if (y < 0.06) {
      grid.newpage()
      y <- 0.94
    }
  }
}

texto1 <- paste0(
  "Autor: Joao Paulo Zangrandi\n\n",
  "A base de Macae possui ", format(N, big.mark = "."), " pessoas. ",
  "Assumindo a base como a populacao correta, a proporcao populacional de alfabetizados e ",
  round(prop_pop, 4), ". O objetivo foi comparar tres desenhos amostrais com n = 1200: ",
  "AAS, AAS estratificada e conglomerados por setor censitario.\n\n",
  "1. No desenho AAS, cada pessoa da populacao recebeu a mesma probabilidade de selecao. ",
  "Em cada simulacao foram sorteadas 1200 linhas diretamente da base, sem reposicao. ",
  "Esse plano serve como referencia porque nao usa informacao auxiliar sobre bairro, distrito ou setor.\n\n",
  "2. No desenho estratificado, os bairros foram agrupados em quatro estratos de acordo com ",
  "a taxa media de alfabetizacao do proprio bairro: baixa, media baixa, media alta e alta. ",
  "A escolha segue o criterio discutido em aula: os estratos devem reunir unidades relativamente ",
  "homogeneas na variavel de interesse. A alocacao foi proporcional ao tamanho populacional ",
  "de cada estrato. Os tamanhos populacionais dos estratos foram: ",
  paste(names(Nh), Nh, sep = " = ", collapse = "; "), ". As entrevistas por estrato foram: ",
  paste(names(nh), nh, sep = " = ", collapse = "; "), ".\n\n",
  "3. No desenho por conglomerados, os conglomerados foram os setores censitarios. Foram ",
  "sorteados ", setores_necessarios, " setores e, dentro de cada setor sorteado, ",
  entrevistas_por_setor, " pessoas aleatoriamente. Assim, cada amostra tem exatamente ",
  setores_necessarios, " x ", entrevistas_por_setor, " = 1200 pessoas. Usei setores com pelo menos ",
  entrevistas_por_setor, " moradores na base para que a selecao sem reposicao fosse possivel."
)

pagina_texto("Tarefa I: Exercicio 1 - Amostragem", texto1)

plot(
  density(resultado$AAS),
  col = "#1f77b4",
  lwd = 2,
  xlim = range(resultado),
  main = "Distribuicao das proporcoes simuladas de alfabetizados",
  xlab = "Proporcao de alfabetizados na amostra",
  ylab = "Densidade"
)
lines(density(resultado$Estratificada), col = "#2ca02c", lwd = 2)
lines(density(resultado$Conglomerados), col = "#d62728", lwd = 2)
abline(v = prop_pop, lty = 2, lwd = 2)
legend(
  "topright",
  legend = c("AAS", "Estratificada", "Conglomerados", "Proporcao populacional"),
  col = c("#1f77b4", "#2ca02c", "#d62728", "black"),
  lwd = c(2, 2, 2, 2),
  lty = c(1, 1, 1, 2),
  bty = "n"
)

texto2 <- paste0(
  "Resultados das 1000 simulacoes\n\n",
  "Media das estimativas: AAS = ", round(medias["AAS"], 4),
  "; estratificada = ", round(medias["Estratificada"], 4),
  "; conglomerados = ", round(medias["Conglomerados"], 4), ".\n\n",
  "Variancia das estimativas: AAS = ", signif(variancias["AAS"], 4),
  "; estratificada = ", signif(variancias["Estratificada"], 4),
  "; conglomerados = ", signif(variancias["Conglomerados"], 4), ".\n\n",
  "Design effect em relacao ao AAS: estratificada = ", round(deff_estratificada, 3),
  "; conglomerados = ", round(deff_conglomerados, 3), ".\n\n",
  "Interpretacao. O desenho estratificado apresentou variancia ",
  ifelse(deff_estratificada < 1, "menor", "maior"),
  " que a do AAS. Isso ocorre porque os estratos foram construidos a partir de bairros com ",
  "niveis semelhantes de alfabetizacao; quando ha homogeneidade dentro dos estratos e ",
  "diferenca entre eles, a alocacao proporcional reduz a variacao das estimativas entre amostras.\n\n",
  "O desenho por conglomerados apresentou variancia ",
  ifelse(deff_conglomerados < 1, "menor", "maior"),
  " que a do AAS. Esse resultado e esperado quando pessoas do mesmo setor censitario se parecem ",
  "entre si em relacao a alfabetizacao. Como a amostra por conglomerados sorteia primeiro os setores ",
  "e depois poucas pessoas dentro de cada setor, ela carrega menos diversidade geografica do que uma ",
  "AAS de 1200 pessoas espalhadas por toda a populacao. A semelhanca interna dos setores aumenta a ",
  "correlacao entre observacoes da mesma amostra e tende a elevar a variancia."
)

pagina_texto("Resultados e interpretacao", texto2)

dev.off()
