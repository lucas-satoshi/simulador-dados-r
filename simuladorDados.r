#nome da função/script: Simulador de Dados

#descrição: Realiza o cálculo de combinações genéticas de pai e mãe para recom-
#binação no filho

#autor: Lucas Satoshi Cipriano Oikawa

#data: 2025-01-16

#contato: 2226364@aluno.univesp.br

#notas: essa função ainda está em fase de desenvolvimento e algumas funcionali-
#dades ainda não foram testadas ou podem sofrer alterações. 
################################################################################

# Função para gerar os cromossomos da progênie
gerar_cromossomos <- function(cromossomo_pai, cromossomo_mae, taxa_recombinacao = 0.01) {
  if (length(cromossomo_pai) != length(cromossomo_mae)) {
    stop("Os cromossomos do pai e da mãe devem ter o mesmo comprimento.")
  }
  
  # Inicialização dos cromossomos da progênie
  cromossomo_filho_paterno <- vector("integer", length(cromossomo_pai))
  cromossomo_filho_materno <- vector("integer", length(cromossomo_mae))
  
  # Controle de recombinação
  recombinacao_pai <- runif(1) < taxa_recombinacao
  recombinacao_mae <- runif(1) < taxa_recombinacao
  
  for (i in seq_along(cromossomo_pai)) {
    # Seleção do alelo do pai
    if (recombinacao_pai && i > 1) {
      # Troca de homólogo caso haja recombinação
      cromossomo_filho_paterno[i] <- cromossomo_pai[length(cromossomo_pai) - i + 1]
    } else {
      cromossomo_filho_paterno[i] <- cromossomo_pai[i]
    }
    
    # Seleção do alelo da mãe
    if (recombinacao_mae && i > 1) {
      cromossomo_filho_materno[i] <- cromossomo_mae[length(cromossomo_mae) - i + 1]
    } else {
      cromossomo_filho_materno[i] <- cromossomo_mae[i]
    }
  }
  
  return(list(cp = cromossomo_filho_paterno, cm = cromossomo_filho_materno))
}

# Interface com o usuário para leitura de arquivo
cat("Bem-vindo ao Simulador de Geração de Novas Progênies\n\n")
cat("Por favor, insira o nome do arquivo CSV contendo os dados dos cromossomos:\n")
arquivo <- readline()

# Leitura dos dados do arquivo
dados <- read.csv(arquivo, header = TRUE, sep = ";", check.names = FALSE)

# Verificar se o arquivo contém as colunas esperadas
colunas_esperadas <- c("Pai_Cromossomo1", "Pai_Cromossomo2", "Mae_Cromossomo1", "Mae_Cromossomo2")
if (!all(colunas_esperadas %in% names(dados))) {
  stop(paste("O arquivo deve conter as seguintes colunas:", paste(colunas_esperadas, collapse = ", ")))
}

# Entrada da taxa de recombinação
cat("\nInsira a taxa de recombinação (valor entre 0 e 1, ex: 0.05 para 5%):\n")
taxa_recombinacao <- as.numeric(readline())

if (taxa_recombinacao < 0 || taxa_recombinacao > 1) {
  stop("A taxa de recombinação deve estar entre 0 e 1.")
}

# Gerar progênie para cada linha do arquivo
resultados <- data.frame()

# Gerar os cromossomos da progênie para cada pai e mãe
for (i in 1:nrow(dados)) {
  # Selecionar cromossomos dos pais
  cromossomo_pai <- ifelse(runif(1) < 0.5, 
                           as.integer(unlist(strsplit(as.character(dados$Pai_Cromossomo1[i]), ","))), 
                           as.integer(unlist(strsplit(as.character(dados$Pai_Cromossomo2[i]), ","))))
  cromossomo_mae <- ifelse(runif(1) < 0.5, 
                           as.integer(unlist(strsplit(as.character(dados$Mae_Cromossomo1[i]), ","))), 
                           as.integer(unlist(strsplit(as.character(dados$Mae_Cromossomo2[i]), ","))))
  
  # Gerar progênie
  progenie <- gerar_cromossomos(cromossomo_pai, cromossomo_mae, taxa_recombinacao)
  
  # Criar um data frame para a progênie com cromossomos separados por coluna
  progenie_df <- data.frame(ID = i)
  
  # Adicionar as colunas dos cromossomos paternos e maternos separadamente
  num_cromossomos <- length(progenie$cp)
  for (j in 1:num_cromossomos) {
    progenie_df[paste("Cromossomo_Paterno_", j, sep = "")] <- progenie$cp[j]
    progenie_df[paste("Cromossomo_Materno_", j, sep = "")] <- progenie$cm[j]
  }
  
  # **Importante: Remover a conversão para string**
  progenie_df <- data.frame(lapply(progenie_df, as.numeric))
  
  # Adicionar os resultados completos à tabela final
  resultados <- rbind(resultados, progenie_df)
}

# Salvar os resultados em um arquivo CSV com as colunas separadas corretamente
arquivo_saida <- "progenie_simulacao.csv"
write.table(resultados, file = arquivo_saida, row.names = FALSE, sep = ";", col.names = TRUE)

cat("\nOs cromossomos da progênie foram salvos no arquivo:", arquivo_saida, "\n")