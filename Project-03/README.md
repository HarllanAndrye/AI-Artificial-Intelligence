# Project 03
3º projeto da disciplina

**1. Objetivo Principal do Projeto**: Analisar o resultado obtido com o uso de redes neurais na solução de um problema de classificação.

**2. O Problema:** Letter Image Recognition Data.
“The objective is to identify each of a large number of black-and-white rectangular pixel displays as one of the 26 capital letters in the English alphabet. The character images were based on 20 different fonts and each letter within these 20 fonts was randomly distorted to produce a file of 20,000 unique stimuli. Each stimulus was converted into 16 primitive numerical attributes (statistical moments and edge counts) which were then scaled to fit into a range of integer values from 0 through 15. We typically train on the first 16000 items and then use the resulting model to predict the letter category for the remaining 4000.”


* Foram feitos treinamentos de redes MLP (Multi Layer Perceptron) usando Backpropagation padrão, no qual as configurações são:

	- Termo Momentum: 0.8 (fixo para todas execuções);
	- Quantidades máximas de iterações: 100, 300, 500;
	- Quantidades de neurônios escondidos: 5, 10 e 20;
	- Taxas de aprendizado: 0.08, 0.2 e 0.4;
	- Treinamento: 66%.

Total de 27 execuções.

## Referências:

- Machine Learning Repository. Letter Image Recognition Data. Disponível em: <https://archive.ics.uci.edu/ml/datasets/Letter+Recognition>. Acesso em: 30 de Novembro de 2015.

- Weka 3: Data Mining Software in Java. Disponível em: <http://www.cs.waikato.ac.nz/ml/weka/>. Acesso em: 30 de Novembro de 2015.

- Robinson Luís de Souza Alves. Uma contribuição ao estudo das categorias internas e de sua proliferação em redes ARTMAP. Disponível em: <http://repositorio.ufrn.br:8080/jspui/bitstream/123456789/15198/1/RobinsonLSA_TESE.pdf>. Acesso em: 30 de Novembro de 2015.

- Silva, Eugênio; Oliveira, Anderson Canêdo de. Dicas para a configuração de redes neurais. Disponível em: <http://equipe.nce.ufrj.br/thome/grad/nn/mat_didatico/dicas_configuracao_rna.pdf>. Acesso em: 01 de Dezembro de 2015.




