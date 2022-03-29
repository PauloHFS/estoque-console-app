:- module(storage, []).

:- use_module(library(csv)).

%Verify the existence of a row in the knowledge base.
product(N):- product(N,_,_,_,_).

%Reads the CSV file and returns a list of rules.
read_storage:-
    csv_read_file('E:\\Developer\\estoque-console-app\\prolog\\storage-example.csv',Products,[functor(product)]),
    write(Products),
    assert_storage(Products).

%Writes the rules in the CSV file.
write_storage(File):-
    condese_prod(0, Products),
    csv_write_file(File, Products).

%Writes the rules in the knowledge base.
assert_storage(Products):-
    maplist(assertz, Products).

%Condenses the Products of the knowledge base into a list.
condese_prod(N, Products):- call(row(N)),
    findall(product(Id,Nome,Quantidade,Preco,Data),product(Id,Nome,Quantidade,Preco,Data),Products).

update_uid(Produtos, ID, NovosProdutos):- not(call(product(N))).
update_uid(Produtos, ID, NovosProdutos).

update_uid_aux(Produtos, ID, NovosProdutos).

verify_storage(Produtos, ProdutosVazios).

verify_validade_produto(Produto, ProdutoValido).

verify_validade_estoque(Produto, Data, EstoqueValido).