# ppgi-prog-lang

#### O que há por aqui?
Esse repositório contém os códigos desenvolvidos na disciplina [**Linguagens de Programação (ELC921)**](http://www-usr.inf.ufsm.br/~juvizzotto/prog-lang/), ministrada pela Professora [**Juliana Kaizer Vizzotto**](http://www-usr.inf.ufsm.br/~juvizzotto/) no [**Programa de Pós-graduação em Informática**](http://w3.ufsm.br/ppgi/) da [**Universidade Federal de Santa Maria**](http://www.ufsm.br) no primeiro semestre de 2015 (2015/1).

Recomentações gerais:
- Veja o livro [*Types and Programming Languages*](https://www.cis.upenn.edu/~bcpierce/tapl/)
- Como material complementar, veja [*Essentials of Programming Languages*](https://mitpress.mit.edu/books/essentials-programming-languages)
- Revise seus conhecimentos de Haskell em [*Learn you a Haskell for Great Good!*](http://learnyouahaskell.com/chapters)
- Dê uma olhada no material disponibilizado pela Professora Juliana no [site da disciplina](http://www-usr.inf.ufsm.br/~juvizzotto/prog-lang/)

#### Linguagens de programação e ferramentas utilizadas
- Haskell @ [GHC](https://www.haskell.org/ghc/)
- Gerador de Parser [Happy](https://www.haskell.org/happy/)
- Editor [Kate](http://kate-editor.org/) e IDE [KDevelop](https://www.kdevelop.org/)
- ~~Editor [GEdit](https://wiki.gnome.org/Apps/Gedit)~~

#### Funcionalidades da implementação:

- Cálculo lambda sem tipagem
   - Implementação de um parser através de [*Happy*](https://www.haskell.org/happy/)
   - Proteção contra captura de variáveis através de Distância estática / [Índices de Bruijn](http://en.wikipedia.org/wiki/De_Bruijn_index)
- *Type Checker* para *Simple Types* do Cálculo Lambda (*commit* 532cc87)
- Apresentação do Trabalho 4 (*commit* 373ba14): Expressões Aritméticas Tipadas e Cálculo Lambda Simplesmente Tipado
- Adição dos novos tipos Nat, Unit, Let e Seq (*commit* 095de53)
   - Melhoramento do *TypeChecker* para suportar os novos tipos (*commit* f1bccd5)
- Adição do tipo *Tuples* (*commits* ebb23b0, b00967f, 1e31363 e 6f6a4b5)
- Adição do tipo Registro (*commits* 1e35bc3 e 3a5a010)


#### Alunos envolvidos:
- Jessica Moura
- Daniel Domenico
- Fernando Campagnolo
- ~~José~~ Kadico Puiati
- Alberto Kummer
- Camila Nogueira


#### Changelog
- Atualizações subsequentes: Veja a lista de commits.
- 05/05/2015:
   - Adiciona suporte a *brackets* em algumas expressões do parser
   - Atualiza isValNL para considearar booleans e [*Church Numerals*](https://en.wikipedia.org/wiki/Church_encoding#Church_numerals)
   - Atualiza subsNL para tratar os novos termos lambda da forma primitiva
   - Atualiza a restoreNames para suportar booleans e numerals
- 01/05/2015: Atualização do parser
   - Adiciona suporte a numeros naturais no parser
   - NLam.hs: adição de comentários para identificar os passos de avaliação
   - NLam.hs: reindenta blocos para facilitar leitura
- 30/04/2015: Adicionado Booleans e Naturais no *Untyped Lambda Calculus*
   - Daniel Domenico: completou a funcionalidade do Eval no *Nameless Lambda*
   - evalCBVNL: adicionado suporte a Booleans e condições de teste
   - lexer: adicionado suporte aos tipos booleanos e expressões de teste
- 23/04/2015: *Shifting* e *Beta*-redução de *Nameless Lambda*
   - shifting: aplicação da técnica de *shifting* em termos lambda "anônimos"
   - subsNL: substituição de termos *Nameless lambda*
   - evalNL: implementação de *beta-redução para *Nameless lambda*
- 17/04/2015: Nameless Lambda Calculus
   - removeNames: Processamento de uma árvore de sintaxe concreta para eliminação dos nomes das variáveis
      - Implementação com índices de Bruijn.
   - restoreNames: Inserção de nomes em termos lambda *nameless*
   - Exemplos de execução na pasta "trabalho4"
# quantumlambdacalculus
