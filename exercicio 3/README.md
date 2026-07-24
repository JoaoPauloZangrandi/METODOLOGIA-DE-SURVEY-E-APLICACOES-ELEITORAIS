# Exercicio 3 - Ponderacao de surveys

Status: pasta criada e enunciado oficial consultado em 24/07/2026.

O Exercicio 3 pede a ponderacao da amostra `eseb22.csv`, com 1800 entrevistas do ESEB 2022, para aproximar a amostra da populacao brasileira do Censo 2022. O relatorio final deve incluir resumo, introducao, metodologia e resultados, comparando amostra original e ponderada nas variaveis de confianca, economia e voto no segundo turno.

Materiais identificados no site da disciplina:

- `lista3.pdf`: Exercicio 3 - Ponderacao de Surveys.
- `eseb22.csv`: banco indicado no enunciado, mas o link publicado no site retorna `404 Not Found`.
- Slides da aula de ponderacao indicados na pagina da disciplina.

Bloqueio atual:

O link oficial do banco informado no site (`http://www.felipenunescp.com/uploads/1/4/4/2/144237945/eseb22.csv`) nao esta funcional. O arquivo baixado desse endereco e uma pagina HTML de erro 404, nao um CSV. Sem a base correta, nao e possivel calcular pesos, aplicar a ponderacao nem gerar tabelas de resultados sem inventar dados.

Proximo passo:

Adicionar o arquivo correto `eseb22.csv` nesta pasta. Com o banco correto disponivel, a solucao deve gerar:

- um script R simples de ponderacao;
- tabelas comparando amostra original e ponderada;
- um PDF final com resumo, introducao, metodologia e resultados.
