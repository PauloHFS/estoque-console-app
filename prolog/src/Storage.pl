:- module(storage, []).

:- use_module(library(csv)).

%Verify the existence of a row in the knowledge base.
row(N):- row(N,_,_,_,_).

%Reads the CSV file and returns a list of rules.
read_storage:-
    csv_read_file('E:\\Developer\\estoque-console-app\\prolog\\storage-example.csv', Rows),
    assert_storage(Rows).

%Writes the rules in the CSV file.
write_storage(File):-
    condese_rows(0, Rows),
    csv_write_file(File, Rows).

%Writes the rules in the knowledge base.
assert_storage(Rows):-
    maplist(assertz, Rows).

%Condenses the rows of the knowledge base into a list.
condese_rows(N, Rows):- call(row(N)),
    findall(row(Id,Nome,Quantidade,Preco,Data),row(Id,Nome,Quantidade,Preco,Data),Rows).

update_uid(Produtos, ID, NovosProdutos):- not(call(row(N))).
update_uid(Produtos, ID, NovosProdutos).

update_uid_aux(Produtos, ID, NovosProdutos).

verify_storage(Produtos, ProdutosVazios).

verify_validade_produto(Produto, ProdutoValido).

verify_validade_estoque(Produto, Data, EstoqueValido).