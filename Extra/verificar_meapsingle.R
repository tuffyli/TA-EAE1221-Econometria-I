# Verificacao da tabela da questao nova com dados MEAPSINGLE
# Nao usa o pacote modelsummary.

# install.packages("wooldridge")
library(wooldridge)

data("meapsingle")

m1 <- lm(math4 ~ lexppp, data = meapsingle)
m2 <- lm(math4 ~ lexppp + free + lmedinc + pctsgle, data = meapsingle)
m3 <- lm(math4 ~ lexppp + free + lmedinc + pctsgle + read4, data = meapsingle)
m4 <- lm(math4 ~ lexppp + I(lexppp^2) + free + lmedinc + pctsgle, data = meapsingle)
m5 <- lm(math4 ~ lexppp * free + lmedinc + pctsgle, data = meapsingle)

models <- list(m1, m2, m3, m4, m5)
names(models) <- paste0("(", seq_along(models), ")")

rows <- c(
  "lexppp",
  "I(lexppp^2)",
  "free",
  "lmedinc",
  "pctsgle",
  "read4",
  "lexppp:free",
  "(Intercept)"
)

labels <- c(
  "lexppp",
  "lexppp^2",
  "free",
  "lmedinc",
  "pctsgle",
  "read4",
  "lexppp x free",
  "Constante"
)

fmt <- function(x, digits = 3) {
  if (is.na(x)) return("--")
  formatC(x, digits = digits, format = "f", decimal.mark = ",")
}

get_est <- function(model, term) {
  b <- coef(model)
  if (!term %in% names(b)) return(NA_real_)
  unname(b[term])
}

get_se <- function(model, term) {
  b <- coef(model)
  if (!term %in% names(b)) return(NA_real_)
  unname(sqrt(diag(vcov(model)))[term])
}

table_out <- data.frame(Regressor = labels, check.names = FALSE)

for (j in seq_along(models)) {
  model <- models[[j]]
  col_est <- character(length(rows))
  col_se <- character(length(rows))

  for (i in seq_along(rows)) {
    col_est[i] <- fmt(get_est(model, rows[i]))
    se <- get_se(model, rows[i])
    col_se[i] <- if (is.na(se)) "--" else paste0("(", fmt(se), ")")
  }

  table_out[[paste0(names(models)[j], " coef.")]] <- col_est
  table_out[[paste0(names(models)[j], " se")]] <- col_se
}

print(table_out, row.names = FALSE)

cat("\nObservacoes:\n")
print(sapply(models, nobs))

cat("\nR2:\n")
print(sapply(models, function(x) unname(1 - deviance(x) / sum((model.response(model.frame(x)) - mean(model.response(model.frame(x))))^2))))

cat("\nCalculos para os itens da questao:\n")

cat("\nItem (b): efeito de aumento de 10% nos gastos, coluna (2)\n")
efeito_10 <- coef(m2)["lexppp"] * log(1.10)
print(efeito_10)

cat("\nItem (b): estatistica t de lexppp, coluna (2)\n")
t_lexppp_m2 <- coef(m2)["lexppp"] / sqrt(diag(vcov(m2)))["lexppp"]
print(t_lexppp_m2)

cat("\nItem (c): ponto em que efeito marginal de lexppp zera, coluna (4)\n")
b1 <- coef(m4)["lexppp"]
b2 <- coef(m4)["I(lexppp^2)"]
turning_point <- -b1 / (2 * b2)
print(turning_point)

cat("\nItem (e): efeitos marginais de lexppp, coluna (5)\n")
effect_free_20 <- coef(m5)["lexppp"] + coef(m5)["lexppp:free"] * 20
effect_free_60 <- coef(m5)["lexppp"] + coef(m5)["lexppp:free"] * 60
print(c(free_20 = effect_free_20, free_60 = effect_free_60))

# Exporta uma versao CSV para inspecao manual.
csv_out <- if (dir.exists("Extra")) {
  file.path("Extra", "tabela_meapsingle_manual.csv")
} else {
  "tabela_meapsingle_manual.csv"
}

write.csv(
  table_out,
  file = csv_out,
  row.names = FALSE,
  fileEncoding = "UTF-8"
)
