:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_parameters)).
:- use_module(library(uri)).
:- use_module(library(http/html_write)).

:- dynamic(nomeCliente/2).
:- dynamic(telCliente/2).
:- dynamic(pedido/7).

% edição de arq___________________________________________
processa_arq:-
    read(P),
    processa_termo(P).

processa_termo('end_of_file'):- !.
processa_termo(P):-
    assertz(P),
    processa_arq.
% fim edição de arq___________________________________________
% manipulação de arq___________________________________________
adiciona(N, T, E, TamPiz, I, H, Obs):-
    atom_concat("'", N, N0), %Tratamento dos dados % ' ??? 
    atom_concat(N0, "'", N1), %  ??? ' 
    atom_concat("'", T, T0), 
    atom_concat(T0, "'", T1), 
    atom_concat("'", E, E0), 
    atom_concat(E0, "'", E1), 
    atom_concat("'", TamPiz, TamPiz0), 
    atom_concat(TamPiz0, "'", TamPiz1),
    atom_concat("'", H, H0), 
    atom_concat(H0, "'", H1),
    atom_concat("'", Obs, Obs0),
    atom_concat(Obs0, "'", Obs1),

    reconsult('pedidos.pl'),
    append('pedidos.pl'),% ' ??? '
    write(pedido(N1, T1, E1, TamPiz1, I, H1, Obs1)), write('.'),nl,
    told;
    erro(_, 'Arquivo não existe!').

remover(N,T,E):-
    reconsult('pedidos.pl'),
    retract(pedido(N,T,E,_,_,_,_)),
    tell('pedidos.pl'),
    listing(pedido/7),
    told.

verificaExistencia(N, T, E, TamPiz, I, H):- %returna falso se já existe um fato com as mesmas informações.
    reconsult('pedidos.pl'),
    findall([N, T, E, TamPiz, I, H], pedido(N, T, E, TamPiz, I, H, _),L),% retirei Obs pois alguém pode gerar inumeros pedidos somente mudando isso o que não queremos.
    L == [],!.

% fim manipulação de arq___________________________________________
% Servidor HTTP___________________________________________
%____________________ Request's do programa
:- http_handler(root(''), index, []).
:- http_handler(root(makepedido), makepedido, []).
:- http_handler(root(makecancelar), makecancelar, []).
:- http_handler(root(pedido), pedido, []).
:- http_handler(root(cancelar), cancelar, []).
:- http_handler(root(pedidoList), pedidoList, []).
:- http_handler(root(cancelarList), cancelarList, []).
:- http_handler(root(erro), erro, []).
%____________________________
servidor(Porta) :- http_server(http_dispatch, [port(Porta)]). % Criação do servidor.
%____________________página inicial_________________________
index(_Request):-
    reply_html_page([
        title('Pizzas Qloucura')
    ],
    [
        h1('Pizzas Qloucura'),
        a([href('http://localhost:8000/makepedido')], 'Fazer um pedido'),
        br(_),
        a([href('http://localhost:8000/makecancelar')], 'Cancelar um pedido'),
        footer('Trabalho de Gabriel Joshua')
    ]),
    !;
    erro(_BadRequest,"Erro de iniciar pagina principal.").
%_________________________________________________________ 
%_______________________páginas HTML para usuários __________________________________
makepedido(_Request):-
    format('Content-type: text/html~n~n'),
    format('<!DOCTYPE HTML><html lang="pt-br"><head><meta charset="UTF-8"><meta http-equiv="X-UA-Compatible" content="IE=edge"><meta name="viewport" content="width=device-width, initial-scale=1.0"><title>Pizzas Qloucura - Pedido</title></head><body><div><h1>Pizzas Qloucura - Pedido</h1><form method="post"enctype="application/x-www-form-urlencoded"action="http://localhost:8000/pedido"><p><label>Nome do Cliente: <input type=name name="nomeCliente" ></label></p><p><label>Telefone: <input type=tel name="telCliente"></label></p><p><label>E-mail: <input type=email name="emailCliente"></label></p><fieldset><legend> Tamanho da Pizza </legend><p><label> <input type=radio name="tamanho" value="p"> Pequena </label></p><p><label> <input type=radio name="tamanho" value="m"> Media </label></p><p><label> <input type=radio name="tamanho" value="g"> Grande </label></p></fieldset><fieldset><legend> Ingrediente </legend><p><label> <input type=checkbox name="ing" value="tom"> Tomate </label></p><p><label> <input type=checkbox name="ing" value="pre"> Presunto</label></p><p><label> <input type=checkbox name="ing" value="ceb"> cebola </label></p><p><label> <input type=checkbox name="ing" value="que"> Queijo </label></p></fieldset><p><label>Horario de entrega desejado: <input type=time min="11:00" max="21:00" step="900" name="tempo"></label></p><p><label>Instrucoes para entrega: <textarea name="obs"></textarea></label></p><p><button>Enviar Pedido</button></p></form></div><footer><a href="http://localhost:8000/">Inicio</a></footer></body></html>'),
    !;
    erro(_BadRequest,"Erro de pedir Lanche.").

makecancelar(_Request):-
    format('Content-type: text/html~n~n'),
    format('<!DOCTYPE html><html lang="pt-br"><head><meta charset="UTF-8"><meta http-equiv="X-UA-Compatible" content="IE=edge"><meta name="viewport" content="width=device-width, initial-scale=1.0"><title>Pizzas Qloucura - Cancelamento</title></head><body><form method="post"enctype="application/x-www-form-urlencoded"action="http://localhost:8000/cancelar"><p><label>Nome do Cliente: <input type=name name="nomeCliente" ></label></p><p><label>Telefone: <input type=tel name="telCliente"></label></p><p><label>E-mail: <input type=email name="emailCliente"></label></p><p><button>Enviar Pedido</button></p></form><a href="http://localhost:8000/">Inicio</a></body></html>'),
    !;
    erro(_BadRequest,"Erro de pedir Cancelamento.").
%_________________________________________________________ 
%_______________________páginas HTML para requisição__________________________________
pedido(R) :-
    http_parameters(R,[nomeCliente(N,[]),
        telCliente(T,[]),
        emailCliente(E,[]),
        tamanho(S,[]),
        ing(I,[list(I)]),
        tempo(H,[]),
        obs(O,[])
    ]),
    verificaExistencia(N, T, E, S, I, H),% se existe não se pode adicionar outro, ou seja, causa falha e gera o erro().
    adiciona(N,T,E,S,I,H,O),
    pedidoList(_Request, N, T, E, S, I, H, O),
    !;
    erro(_BadRequest,"Erro de solicitar pedido. Pedido Repetido.").
    

cancelar(R):-
    http_parameters(R,[nomeCliente(N,[]),
        telCliente(T,[]),
        emailCliente(E,[])
    ]),
    remover(N, T, E),
    cancelarList(_Request, N, T, E),
    !;
    erro(_BadRequest,"Erro de solicitar Cancelamento.").
%_________________________________________________________ 
%_________________ Listar dados __________________________
pedidoList(_Request, N, T, E, S, I, H, O):-
    reply_html_page([
        title('Pizzas Qloucura - Pedidos')
    ],
    [
        h1('Pizzas Qloucura - Pedidos'),
        p('Nome:'),p(N), 
        br(_),
        p('Telefone: '),p(T),
        br(_),
        p('Email: '),p(E),
        br(_),
        p('Sabor: '),p(S),
        br(_),
        p('Ingredientes: '),p(I),
        br(_),
        p('Horas: '),p(H),
        br(_),
        p('Observacoes: '),p(O),
        br(_),
        footer(a([href('http://localhost:8000/')], 'Inicio'))
    ]),
    !;
    erro(_BadRequest,"Erro de listar informações de Pedidos.").

cancelarList(_Request, N, T, E):-
    reply_html_page([
        title('Pizzas Qloucura - Cancelamento')
    ],[
        h1('Pizzas Qloucura - Cancelamento'),
        p('Nome:'),p(N), 
        br(_),
        p('Telefone: '),p(T),
        br(_),
        p('Email: '),p(E),
        br(_),
        footer(a([href('http://localhost:8000/')], 'Inicio'))]
    ),
    !;
    erro(_BadRequest,"Erro de listar informações de Cancelamento.").
%_____________________________
%_____________Erros trataveis________________
erro(_BadRequest,Info):-
    reply_html_page([
    title('Pizzas Qloucura - Erro')
    ],[
    h1('Pizzas Qloucura - Erro de Processamento'),
    p(Info),
    br(_),
    footer(a([href('http://localhost:8000/')], 'Inicio'))]
    ).
%_________________________________
%Fim script Servidor HTTP ________________
% start Servidor HTTP na 8000___________________________________________
:- servidor(8000).