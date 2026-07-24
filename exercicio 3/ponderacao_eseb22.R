# Tarefa III - Ponderacao de surveys
# Autor: Joao Paulo Zangrandi
#
# Este script resolve a lista 3 quando o arquivo correto eseb22.csv esta na
# pasta. O link publicado no site da disciplina retornava 404 em 24/07/2026;
# por isso, o script valida o arquivo antes de calcular qualquer resultado.

rm(list = ls())

arquivo <- "eseb22.csv"

if (!file.exists(arquivo)) {
  stop("Coloque o arquivo eseb22.csv correto nesta pasta antes de rodar.")
}

primeira_linha <- readLines(arquivo, n = 1, warn = FALSE)
if (grepl("<!DOCTYPE|<html", primeira_linha, ignore.case = TRUE)) {
  stop("O arquivo eseb22.csv atual nao e uma base CSV: e uma pagina HTML de erro.")
}

dados <- read.csv(arquivo, stringsAsFactors = FALSE, fileEncoding = "UTF-8-BOM")

vars_necessarias <- c(
  "id_entrevista", "regiao", "sexo", "idade", "escolaridade",
  "confianca_governo", "confianca_judiciario", "economia_12meses", "voto_2t"
)

faltantes <- setdiff(vars_necessarias, names(dados))
if (length(faltantes) > 0) {
  stop(paste("Variaveis ausentes no CSV:", paste(faltantes, collapse = ", ")))
}

faixa_idade <- function(x) {
  x <- suppressWarnings(as.numeric(x))
  cut(
    x,
    breaks = c(15, 24, 34, 44, 54, 64, Inf),
    labels = c("16-24", "25-34", "35-44", "45-54", "55-64", "65+"),
    right = TRUE
  )
}

limpa <- function(x) {
  x <- trimws(as.character(x))
  x[x == "" | x %in% c("NA", "NaN")] <- NA
  x
}

amostra <- within(dados, {
  regiao <- limpa(regiao)
  sexo <- limpa(sexo)
  escolaridade <- limpa(escolaridade)
  idade_fx <- faixa_idade(idade)
})

amostra <- amostra[!is.na(amostra$regiao) & !is.na(amostra$sexo) &
                     !is.na(amostra$idade_fx), ]

# Alvos populacionais aproximados do Censo 2022. A lista pede obter totais ou
# proporcoes populacionais; aqui usamos margens por regiao, sexo e idade, que
# sao variaveis disponiveis no survey e dimensoes centrais de pos-estratificacao.
alvo_regiao <- c(
  "Norte" = 17349619,
  "Nordeste" = 54644582,
  "Sudeste" = 84847187,
  "Sul" = 29933315,
  "Centro-Oeste" = 16287809
)

alvo_sexo <- c(
  "Masculino" = 0.485,
  "Feminino" = 0.515
)

alvo_idade <- c(
  "16-24" = 0.145,
  "25-34" = 0.175,
  "35-44" = 0.195,
  "45-54" = 0.175,
  "55-64" = 0.145,
  "65+" = 0.165
)

normaliza <- function(x) x / sum(x)
alvos <- list(
  regiao = normaliza(alvo_regiao),
  sexo = normaliza(alvo_sexo),
  idade_fx = normaliza(alvo_idade)
)

rake_simples <- function(df, alvos, max_iter = 100, tol = 1e-7) {
  w <- rep(1, nrow(df))
  for (iter in seq_len(max_iter)) {
    w_anterior <- w
    for (v in names(alvos)) {
      alvo <- alvos[[v]]
      atual <- tapply(w, df[[v]], sum, na.rm = TRUE)
      atual <- atual / sum(atual)
      comum <- intersect(names(alvo), names(atual))
      fator <- alvo[comum] / atual[comum]
      w <- w * ifelse(df[[v]] %in% comum, fator[as.character(df[[v]])], 1)
    }
    if (max(abs(w - w_anterior)) < tol) break
  }
  w / mean(w)
}

amostra$peso <- rake_simples(amostra, alvos)

tabela_var <- function(df, var) {
  bruto <- prop.table(table(df[[var]], useNA = "no"))
  pond <- tapply(df$peso, df[[var]], sum, na.rm = TRUE)
  pond <- pond / sum(pond)
  cats <- sort(unique(c(names(bruto), names(pond))))
  data.frame(
    variavel = var,
    categoria = cats,
    original_pct = round(100 * as.numeric(bruto[cats]), 1),
    ponderada_pct = round(100 * as.numeric(pond[cats]), 1),
    row.names = NULL
  )
}

tabelas_resultado <- do.call(
  rbind,
  lapply(
    c("confianca_governo", "confianca_judiciario", "economia_12meses", "voto_2t"),
    function(v) tabela_var(amostra, v)
  )
)

diagnostico_pesos <- data.frame(
  n = nrow(amostra),
  peso_min = min(amostra$peso),
  peso_p25 = unname(quantile(amostra$peso, 0.25)),
  peso_mediana = median(amostra$peso),
  peso_media = mean(amostra$peso),
  peso_p75 = unname(quantile(amostra$peso, 0.75)),
  peso_max = max(amostra$peso),
  efeito_desenho_aprox = 1 + (sd(amostra$peso) / mean(amostra$peso))^2
)

write.csv(tabelas_resultado, "tabelas_resultado.csv", row.names = FALSE)
write.csv(diagnostico_pesos, "diagnostico_pesos.csv", row.names = FALSE)
write.csv(amostra, "eseb22_ponderado.csv", row.names = FALSE)

print(diagnostico_pesos)
print(tabelas_resultado)
