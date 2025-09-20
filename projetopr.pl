% 109872 Dinis Silva
:- use_module(library(clpfd)). % para poder usar transpose/2
:- set_prolog_flag(answer_write_options,[max_depth(0)]). % ver listas completas
:- ['puzzlesAcampar.pl']. % Ficheiro dado. No Mooshak tera mais puzzles.
% Atencao: nao deves copiar nunca os puzzles para o teu ficheiro de codigo
% Segue-se o codigo


% Dada uma celula, o predicado vizinhanca/2 eh verdade se Vizinhanca eh
% uma lista ordenada de cima para baixo e da esquerda para a direita,
% sem elementos repetidos, com as coordenadas das posicoes imediatamente
% acima, imediatamente a esquerda, imediatamente a direita e
% imediatamente abaixo da coordenada dada.

vizinhanca((Linha,Coluna), Vizinhanca) :-
    Direita is Linha+1,
    Esquerda is Linha-1,
    Cima is Coluna+1,
    Baixo is Coluna-1,
    Vizinhanca =[(Esquerda,Coluna),(Linha,Baixo), (Linha,Cima),(Direita,Coluna)].



% Dada uma celula, o predicado vizinhancaAlargada/2 eh verdade se
% VizinhancaAlargada eh uma lista ordenada de cima para baixo e da
% esquerda para a direita, sem elementos repetidos, com as coordenadas
% anteriores e ainda as diagonais da coordenada dada.

vizinhancaAlargada((Linha,Coluna), VizinhancaAlargada):-
    % Usamos a vizinhanca para chegar aos valores das diagonais
    vizinhanca((Linha,Coluna), Vizinhanca),
    Diagonal1 is Linha+1,
    Diagonal2 is Linha-1,
    Diagonal3 is Coluna+1,
    Diagonal4 is Coluna-1,
    append(Vizinhanca, [(Diagonal1,Diagonal3), (Diagonal1,Diagonal4),(Diagonal2, Diagonal3), (Diagonal2,Diagonal4)],Lista),
    % Usamos a funcao sort para se tornar numa lista ordenada de cima para baixo e da esquerda para a direita.
    sort(Lista, VizinhancaAlargada).



% Dada uma celula, todasCelulas/2 eh verdade se TodasCelulas
% eh uma lista ordenada de cima para baixo e da esquerda para a direita,
% sem elementos repetidos, com todas as
% coordenadas do tabuleiro Tabuleiro.

todasCelulas(Tabuleiro, TodasCelulas):-
    length(Tabuleiro, Tamanho),
    findall((X, Y), (between(1, Tamanho, X), between(1, Tamanho, Y)), TodasCelulas).



% Dada uma celula, todasCelulas/3 eh verdade se TodasCelulas
% eh uma lista ordenada de cima para baixo e da esquerda
% para a direita, sem elementos repetidos, com todas as coordenadas do
% Tabuleiro em que existe um objecto do tipo Objecto.

todasCelulas(Tabuleiro, TodasCelulas, Objecto) :-
    % Usa uma funcao auxiliar com um contador integrado (1) e um acumulador.
    todasAux(Tabuleiro, [], TodasCelulas, Objecto, 1),
    !.


% Alteramos os valores de cada variavel de cada lista do tabuleiro para
% um numero, de maneira a que nao unifique com outros valores.
alterar(Var,14356) :-
    var(Var),
    !.
alterar(Val, Val).


% Encontra todas as posicoes de um elemento numa lista.
encontraPosicao(Elemento, Lista, Posicoes):-
    findall(Posicao, nth1(Posicao, Lista, Elemento), Posicoes).


% Cria coordenadas dada uma Lista com posicoes e a sua Linha.
criaCoordenadas(Lista, Linha, Final):-
    findall((Linha,X), (member(X, Lista)),Final).


% Utiliza as duas funcoes anteriores e cria celulas para cada linha com
% os valores das posicoes de certo elemento.
celulas(Elemento, Lista, Contador, Celulas):-
    encontraPosicao(Elemento, Lista, Posicoes),
    criaCoordenadas(Posicoes, Contador, Celulas).


% Funcao auxiliar de todasCelulas, implementada recursivamente.
% No caso base, quando finalizamos de inspecionar o Tabuleiro, a
% variavel TodasCelula unifica com o acumulador (Ac).
todasAux([], Ac, Ac, _, _):- !.
todasAux([C|R], Ac, TodasCelulas, ObjectoTemp, Contador):-
    % Usamos a funcao copy_term de maneira a que caso haja
    % unificacoes com variaveis em member, nao alterar C.
    copy_term(C,CNovo),
    Objecto = ObjectoTemp,
    member(ObjectoTemp,CNovo),
    alterar(Objecto, ObjectoMod),
    maplist(alterar, C, Lista),
    celulas(ObjectoMod, Lista, Contador, Celulas),
    append(Ac, Celulas, AcAtual),
    Contador1 is Contador + 1,
    todasAux(R,AcAtual, TodasCelulas, ObjectoMod, Contador1).

todasAux([C|R], Ac, TodasCelulas, Objecto, Contador):-
    \+ member(Objecto, C),
    Contador1 is Contador + 1,
    todasAux(R, Ac, TodasCelulas, Objecto, Contador1).


% calculaObjectosTabuleiro/4 eh verdade se Tabuleiro for
% um tabuleiro, Objecto for o tipo de objecto
% que se procura, e ContagemLinhas e ContagemColunas
% forem,respectivamente, listas com o
% numero desses objectos por linha e por coluna.

calculaObjectosTabuleiro(Tabuleiro, ContagemLinhas, ContagemColunas, Objecto):-
    length(Tabuleiro, T),
    todasCelulas(Tabuleiro, Todas, Objecto),
    contaColunas(ContagemColunas, Todas,1,T),
    contaLinhas(ContagemLinhas, Todas,1,T).

% Para contar os Objectos nas Colunas, filtramos as Celulas
% de maneira a que apenas contarmos as que tem a sua Linha igual
% ao contador.
filtrar(Celula,Contador,Tipo,Resposta) :-
    tuploparalista(Celula, CelulaNova),
    findall(Cord, (member(Cord, CelulaNova), nth1(Tipo, Cord, Contador)), Resposta).


contaColunas(Contagem, Todas, 1, T):-
    contaColunasAux(_, Final, Todas, 1, T),
    Contagem = Final,
    !.

contaColunasAux(ContagemColunas,R,Todas, Contador,T):-
    Contador =< T,
    filtrar(Todas, Contador, 2, Lista),
    length(Lista, Tamanho),
    append(ContagemColunas, [Tamanho], Nova),
    Contador1 is Contador+1,
    contaColunasAux(Nova,R, Todas, Contador1, T).

contaColunasAux(ContagemColunas,ContagemColunas,_, Contador, T):-
    Contador > T,
    !.


contaLinhas(Contagem, Todas, 1, T):-
    contaLinhasAux(_, Final, Todas, 1, T),
    Contagem = Final,
    !.

contaLinhasAux(ContagemLinhas,R, Todas, Contador,T):-
    Contador =< T,
    filtrar(Todas, Contador, 1, Lista),
    length(Lista, Tamanho),
    append(ContagemLinhas, [Tamanho], Nova),
    Contador1 is Contador+1,
    contaLinhasAux(Nova,R, Todas, Contador1,T).

contaLinhasAux(ContagemLinhas,ContagemLinhas,_, Contador, T):-
    Contador > T,
    !.

tuploparalista([], []).
    tuploparalista([Tuplo | R1], [Lista | R2]) :-
    tuplolista(Tuplo, Lista),
    tuploparalista(R1, R2).


tuplolista((A,B), [A,B]).



% celulaVazia/2 eh verdade se um Tabuleiro
% for um tabuleiro que nao tem
% nada ou tem relva nas coordenadas (L, C). De notar que se as
% coordenadas nao fizerem parte
% do tabuleiro, o predicado nao deve falhar.

celulaVazia(Tabuleiro, (L,C)) :-
    length(Tabuleiro, T),
    T >= L,
    T >= C,
    C > 0,
    L > 0,
    todasCelulas(Tabuleiro, Relva, r),
    todasCelulas(Tabuleiro, Vazio, _),
    append(Relva, Vazio, Lista),
    memberSemUnif((L,C), Lista).

celulaVazia(Tabuleiro, (L,C)) :-
    length(Tabuleiro, T),
    (T < L; T < C; C =<0; L=<0).

memberSemUnif(A,[C|_]) :-
    A == C.
memberSemUnif(A,[_|R]) :-
    memberSemUnif(A,R) .


% insereObjectoCelula/3 eh verdade se Tabuleiro eh um
% tabuleiro e (L, C) sao as coordenadas onde queremos inserir o objecto
% TendaOuRelva.

insereObjectoCelula(Tabuleiro, TendaOuRelva, (L, C)) :-

    insereAux(Tabuleiro, TendaOuRelva, L,C, Novo),
    Tabuleiro = Novo.

insereObjectoCelula(_, _, _).

insereAux(Tabuleiro,TendaOuRelva, L,C, Novo):-
    nth1(L, Tabuleiro,Linha),
    trocaColuna(C,TendaOuRelva,Linha,Linha2),
    trocaLinha(L,Linha2,Tabuleiro,Novo).


trocaColuna(C,Elemento,Lista,Novo) :-
    nth1(C,Lista, _, L),
    nth1(C,Novo,Elemento,L).

trocaLinha(L, Linha,Lista,Novo) :-
    nth1(L,Lista,_,L1),
    nth1(L,Novo,Linha,L1).


% insereObjectoEntrePosicoes/4 eh verdade se
% Tabuleiro eh um tabuleiro,e (L, C1) e (L, C2) sao
% as coordenadas, na Linha L,entre as quais
% (incluindo) se insere o objecto TendaOuRelva.

insereObjectoEntrePosicoes(Tabuleiro, TendaOuRelva, (L, C1), (L, C2)):-
    insereLinhas(Tabuleiro, TendaOuRelva, L, C1, C2, Tab),
    Tabuleiro = Tab.


insereLinhas(Tabuleiro, TendaOuRelva, L, C1, C2,Tab):-
    C1 =< C2,
    insereObjectoCelula(Tabuleiro, TendaOuRelva, (L,C1)),
    Contagem is C1+1,
    insereLinhas(Tabuleiro, TendaOuRelva, L, Contagem, C2, Tab).

insereLinhas(Tabuleiro,_,_,C1,C2,Tabuleiro):-
    C1 > C2.

listasIguais([ ], [ ]).
listasIguais([C1|R1], [C2|R2]):-
    C1 == C2,
    listasIguais(R1, R2).


% relva/1 eh verdade se Puzzle eh um puzzle que,
% apos a aplicacao do predicado, tem
% relva em todas as linhas/colunas cujo numero de tendas ja atingiu o
% numero de tendas possivel nessas linhas/colunas.

relva((Tabuleiro,MaxLinhas,MaxColunas)):-
    length(Tabuleiro, Tamanho),
    calculaObjectosTabuleiro(Tabuleiro, ContagemLinhas, ContagemColunas, t),
    relvaAux(Tabuleiro, ContagemLinhas, MaxLinhas,1,Tamanho),
    transpose(Tabuleiro, Transposta),
    relvaAux(Transposta,ContagemColunas, MaxColunas,1, Tamanho).

relvaAux(_,[],[],_,_).
relvaAux(Tabuleiro,[NumeroTendas|R2], [MaxTendas|R3], Contador, Tamanho):-
    NumeroTendas = MaxTendas,
    insereObjectoEntrePosicoes(Tabuleiro, r, (Contador, 1), (Contador, Tamanho)),
    ContadorAtual is Contador + 1,
    relvaAux(Tabuleiro, R2, R3, ContadorAtual, Tamanho).

relvaAux(Tabuleiro,[NumeroTendas|R2], [MaxTendas|R3], Contador, Tamanho):-
    NumeroTendas \= MaxTendas,
    Contador1 is Contador + 1,
    relvaAux(Tabuleiro,R2, R3, Contador1, Tamanho).



% inacessiveis/1 eh verdade se Tabuleiro eh um
% tabuleiro que, apos a aplicacao do
% predicado, tem relva em todas as posicoes inacessiveis;

inacessiveis(Puzzle):-
    todasCelulas(Puzzle, Celulas),
    todasCelulas(Puzzle, Arvores, a),
    findall(X, (member(Arvore, Arvores),vizinhanca(Arvore, X)), Lista),
    alisa(Lista, Lista1),
    findall(Celula, (member(Celula, Celulas), \+member(Celula, Lista1)), Final),
    aplica(Puzzle,Final, r).



% Predicado para tornar uma lista de listas em apenas uma.
alisa([], []).
alisa([C|R], Final) :-
    alisa(R, Alisado),
    append(C, Alisado, Final).

% Predicado que aplica a funcao insereObjectoCelula
% numa lista de celulas do Puzzle.
aplica(_, [], _):- !.
aplica(Puzzle,[Celula|Resto], Tipo) :-
    insereObjectoCelula(Puzzle, Tipo, Celula),
    aplica(Puzzle, Resto, Tipo).


% aproveita/1 eh verdade se Puzzle eh um puzzle que, apos a aplicacao do
% predicado tem tendas em todas as linhas e colunas as quais faltavam
% colocar X tendas e que tinham exactamente X posicoes livres.

aproveita((Tabuleiro,MaxLinhas, _)):-
    length(Tabuleiro, Tamanho),
    calculaObjectosTabuleiro(Tabuleiro, CelulasVazias, _, _),
    calculaObjectosTabuleiro(Tabuleiro, TendasExistentes, _, t),
    aproveitaAux(Tabuleiro, MaxLinhas, CelulasVazias, TendasExistentes,1,Tamanho).


aproveitaAux(_, [], _, _,_,_).
aproveitaAux(Tabuleiro,[NumeroMaximoTendas|R2], [NumeroVazias|R3], [NumeroOcupadas|R4],Contador,Tamanho):-
    NumeroMaximoTendas =:= NumeroVazias + NumeroOcupadas,
    insereObjectoEntrePosicoes(Tabuleiro, t, (Contador, 1), (Contador, Tamanho)),
    Contador1 is Contador + 1,
    aproveitaAux(Tabuleiro, R2, R3, R4, Contador1, Tamanho).


aproveitaAux(Tabuleiro,[NumeroMaximoTendas|R2], [NumeroVazias|R3], [NumeroOcupadas|R4],Contador,Tamanho):-
    NumeroMaximoTendas \= NumeroVazias + NumeroOcupadas,
    Contador1 is Contador +1,
    aproveitaAux(Tabuleiro,R2, R3,R4,Contador1,Tamanho).



% limpaVizinhancas/1 eh verdade se Puzzle eh um puzzle
% que, apos a aplicacao do predicado,
% tem relva em todas as posicoes
% a volta de uma tenda;

limpaVizinhancas((Tabuleiro,_,_)):-
    todasCelulas(Tabuleiro, TodasCelulas, t),
    findall(Celula, (member(X, TodasCelulas), vizinhancaAlargada(X, Celula)), VizinhancaTemp),
    alisa(VizinhancaTemp,Vizinhanca),
    aplica(Tabuleiro, Vizinhanca, r).



% unicaHipotese/1 eh verdade se Puzzle eh um puzzle que, apos a
% aplicacao do predicado, todas as arvores que tinham apenas uma posicao
% livre na sua vizinhanca que lhes
% permitia ficar ligadas a uma tenda, teem agora uma tenda nessa
% posicao.

unicaHipotese((Tabuleiro, _, _)):-
    todasCelulas(Tabuleiro, Arvores, a),
    findall(Celula, (member(X, Arvores), vizinhanca(X, Celula)),Vizinhanca),
    todasCelulas(Tabuleiro, Vazio, _),
    unicaAux(Tabuleiro,Vizinhanca, Vazio).



unicaAux(_, [], _).
unicaAux(Tabuleiro,[C|R], Vazio):-
    findall(Celula, (member(Celula, C), member(Celula, Vazio)), VizinhancaVazia),
    length(VizinhancaVazia, TamanhoVazios),
    TamanhoVazios =:= 1,
    insereObjectoCelula(Tabuleiro, t, Celula),
    unicaAux(Tabuleiro, R, Vazio).

unicaAux(Tabuleiro, [_|R], Vazio):-
    unicaAux(Tabuleiro, R, Vazio).


% valida/2 eh verdade se LArv e LTen sao listas com todas as
% coordenadas em que existem, respectivamente, arvores e tendas, e eh
% avaliado para verdade se for possivel estabelecer uma relacao em que
% existe uma e uma unica tenda para cada arvore nas suas vizinhancas.


valida([],[]).
valida([Arvore|Resto], Tendas):-
    vizinhanca(Arvore, VizinhancaArvoresTemp),
    findall(X, (member(X, Tendas), member(X, VizinhancaArvoresTemp)),TendasNaVizinhanca),
    TendasNaVizinhanca = [C| _],
    findall(X,(member(X, Tendas) ,X \= C), TendasFinal),
    valida(Resto, TendasFinal).

