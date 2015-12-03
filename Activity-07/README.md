# Activity 07

**Problema:** Utilizar um Simulador FUZZY para modelar o problema: Sistema Fuzzy para predizer o número de turistas visitando um resort (descrito a seguir).

**Descrição do Problema**

Seja um sistema Fuzzy para predizer o número de turistas visitando um resort.

* Variáveis de entrada:
 - Temperatura (em graus Celsius);
 - Luz do sol (expressa em uma porcentagem do máximo esperado de luz do sol).

* Saída:
 - Quantidade estimada de turistas (expressa em porcentagem da capacidade do resort).

* Base de conhecimento - variáveis linguísticas
 - Entradas:
   - Temperatura {fria, morna, quente};
   - Luz do sol {nublado, parcialmente ensolarado, ensolarado}.
 - Saída:
   - Turistas {baixo, médio, alto}.

* Base de conhecimento - regras (definidas por um especialista):

  1. Se temperatura é quente ou luz do sol é ensolarado então turistas é alto.
  2. Se temperatura é morna e luz do sol é parcialmente ensolarado então turistas é médio.
  3. Se temperatura é fria ou luz do sol é nublado então turistas é baixo.

* Operadores de união e intersecção: max e min.

**Para a situação em que foi observado:**
- Temperatura de 19 graus Celsius;
- Luz do sol de 60%.


## Referências:

- Garibaldi, Jonathan M. et. al. A fuzzy toolbox for the R programing language. Disponível em: <http://ima.ac.uk/papers/wagner2011a.pdf>. Acesso em: 02 de Dezembro de 2015.

- FIS Evaluation. Disponível em: <http://radio.feld.cvut.cz/matlab/toolbox/fuzzy/fuzzyt22.html>. Acesso em: 02 de Dezembro de 2015.

- Tedesco, P. e Vasconcelos, G. Lógica difusa (Fuzzy). Centro de Informática. Universidade Federal de Pernambuco. Disponível em: <http://www.cin.ufpe.br/~if684/EC/aulas/Aula-logica-fuzzy-SI.pdf>. Acesso em: 02 de Dezembro de 2015.
