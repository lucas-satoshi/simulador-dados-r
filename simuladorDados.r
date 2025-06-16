#nome da função/script: Simulador de Dados para Melhoramento Genético Animal

#descrição: Realiza o cálculo de combinações genéticas de pai e mãe para recom-
#binação no filho

#autor: Lucas Satoshi Cipriano Oikawa

#data: 2025-06-15

#contato: 2226364@aluno.univesp.br

#notas: essa função ainda está em fase de desenvolvimento e algumas funcionali-
#dades ainda não foram testadas ou podem sofrer alterações.
################################################################################

# Função para simular a geração de novas progênies
simular_progênie <- function(df_pai, df_mae, num_individuos) {
  # Verificação de consistência
  if(ncol(df_pai) != 2 || ncol(df_mae) != 2) {
    stop("Ambos data frames devem conter exatamente 2 colunas")
  }
  
  if(nrow(df_pai) != nrow(df_mae)) {
    stop("Os data frames devem ter o mesmo número de linhas")
  }
  
  # Lista para armazenar os cromossomos selecionados para cada indivíduo
  # Cada elemento da lista será um vetor de cromossomos (pai ou mãe) para um indivíduo
  colunas_para_df_final <- list()
  
  # Definindo uma semente para reprodutibilidade
  set.seed(123) # Pode ser alterado para um valor dinâmico como as.integer(Sys.time())
  
  for(j in 1:num_individuos) {
    # Seleção aleatória de uma das duas colunas (cromossomos inteiros) do pai
    selected_col_pai_index <- sample(1:ncol(df_pai), 1)
    selected_chromosome_pai <- df_pai[[selected_col_pai_index]]
    
    # Seleção aleatória de uma das duas colunas (cromossomos inteiros) da mãe
    selected_col_mae_index <- sample(1:ncol(df_mae), 1)
    selected_chromosome_mae <- df_mae[[selected_col_mae_index]]
    
    # Adiciona os cromossomos selecionados como novas colunas à lista
    colunas_para_df_final[[paste0("Individuo_", j, "_Pai")]] <- selected_chromosome_pai
    colunas_para_df_final[[paste0("Individuo_", j, "_Mae")]] <- selected_chromosome_mae
  }
  
  # Combina todos os vetores da lista em um único data frame por colunas
  resultado_final <- as.data.frame(colunas_para_df_final, stringsAsFactors = FALSE)
  
  return(resultado_final)
}

# Exemplo de uso da função (para demonstração, pode ser removido ou adaptado)

# # Definindo o número de linhas para os data frames
#num_linhas <- 10

# # Criando o data frame do pai
#df_pai <- data.frame(
#  gene_A = sample(c(0, 1), num_linhas, replace = TRUE),
#  gene_B = sample(c(0, 1), num_linhas, replace = TRUE)
#)

# # Criando o data frame da mãe
#df_mae <- data.frame(
#  gene_A = sample(c(0, 1), num_linhas, replace = TRUE),
#  gene_B = sample(c(0, 1), num_linhas, replace = TRUE)
#)

# num_individuos_progênie <- 5 # Exemplo: gerar 5 novos indivíduos
# progênie_gerada <- simular_progênie(dados_pai_exemplo, dados_mae_exemplo, num_individuos_progênie)

# # Salvamento do resultado (exemplo)
# output_filename <- "progenie_selecionada_funcao.csv"
# write.table(
#   progênie_gerada,
#   file = output_filename,
#   sep = ";",
#   row.names = FALSE,
#   quote = FALSE
# )

# cat(paste0("\nSimulação concluída! Resultados salvos em \'", output_filename, "\'\n"))