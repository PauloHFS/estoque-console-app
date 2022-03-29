:- use_module(library(csv)).

row(N):- row(N,_,_,_,_).

%Reads the CSV file and returns a list of rules.
read_storage(File):-
    csv_read_file(File, Rows),
    assert_storage(Rows).

%Asserts the rules in the knowledge db.
assert_storage(Rows):-
    maplist(assertz, Rows).

write_storage(File):-
    condese_rows(0, Rows),
    csv_write_file(File, Rows).

condese_rows(N, Rows):- call(row(N)),
    findall(row(Id,Nome,Quantidade,Preco,Data),row(Id,Nome,Quantidade,Preco,Data), Rows).

update_uid(Produtos, ID, NovosProdutos):- not(call(row(N))).
update_uid(Produtos, ID, NovosProdutos).

update_uid_aux(Produtos, ID, NovosProdutos).

verify_storage(Produtos, ProdutosVazios).

verify_validade_produto(Produto, ProdutoValido).

verify_validade_estoque(Produto, Data, EstoqueValido).