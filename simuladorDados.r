#nome da função/script: Simulador de Dados para Melhoramento Genético Animal

#descrição: Realiza o cálculo de combinações genéticas de pai e mãe para recom-
#binação no filho

#autor: Lucas Satoshi Cipriano Oikawa

#data: 2025-03-08

#contato: 2226364@aluno.univesp.br

#notas: essa função ainda está em fase de desenvolvimento e algumas funcionali-
#dades ainda não foram testadas ou podem sofrer alterações. 
################################################################################

cat("Bem-vindo ao Simulador de Geração de Novas Progênies\n\n")
cat("Por favor, insira o nome do arquivo CSV contendo os cromossomos do pai (2 colunas):\n")
arquivo_pai <- readline()
cat("Por favor, insira o nome do arquivo CSV contendo os cromossomos da mãe (2 colunas):\n")
arquivo_mae <- readline()

# Leitura dos dados
dados_pai <- read.csv(arquivo_pai, header = TRUE, sep = ";", stringsAsFactors = FALSE)
dados_mae <- read.csv(arquivo_mae, header = TRUE, sep = ";", stringsAsFactors = FALSE)

# Verificação de consistência
if(ncol(dados_pai) != 2 || ncol(dados_mae) != 2) {
  stop("Ambos arquivos devem conter exatamente 2 colunas")
}

if(nrow(dados_pai) != nrow(dados_mae)) {
  stop("Os arquivos devem ter o mesmo número de linhas")
}

# Processamento
resultado <- data.frame(
  Cromossomo_Pai = character(),
  Cromossomo_Mae = character(),
  stringsAsFactors = FALSE
)

for(i in 1:nrow(dados_pai)) {
  # Seleção aleatória do cromossomo paterno
  crom_pai <- sample(dados_pai[i, ], 1)
  
  # Seleção aleatória do cromossomo materno
  crom_mae <- sample(dados_mae[i, ], 1)
  
  # Armazenamento do resultado
  resultado[i, ] <- c(crom_pai, crom_mae)
}

# Salvamento do resultado
write.table(
  resultado,
  file = "progenie_selecionada.csv",
  sep = ";",
  row.names = FALSE,
  quote = FALSE
)

cat("\nSimulação concluída! Resultados salvos em 'progenie_selecionada.csv'\n")