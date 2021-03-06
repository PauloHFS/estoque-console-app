:- use_module('storage.pl').
:- use_module(util, [read_string/1, read_number/1]).

:- dynamic
    product/5.

menu:-
    write(""),nl,
    write("███████╗███╗   ███╗ █████╗ ██████╗ ████████╗    ███╗   ███╗ ██████╗ ███╗   ███╗████████╗"),nl,
    write("██╔════╝████╗ ████║██╔══██╗██╔══██╗╚══██╔══╝    ████╗ ████║██╔════╝ ████╗ ████║╚══██╔══╝"),nl,
    write("███████╗██╔████╔██║███████║██████╔╝   ██║       ██╔████╔██║██║  ███╗██╔████╔██║   ██║   "),nl,
    write("╚════██║██║╚██╔╝██║██╔══██║██╔══██╗   ██║       ██║╚██╔╝██║██║   ██║██║╚██╔╝██║   ██║   "),nl,
    write("███████║██║ ╚═╝ ██║██║  ██║██║  ██║   ██║       ██║ ╚═╝ ██║╚██████╔╝██║ ╚═╝ ██║   ██║   "),nl,
    write("╚══════╝╚═╝     ╚═╝╚═╝  ╚═╝╚═╝  ╚═╝   ╚═╝       ╚═╝     ╚═╝ ╚═════╝ ╚═╝     ╚═╝   ╚═╝   "),nl,
    nl,
    write("\t\t\tGERENCIAMENTO DE ESTOQUE RÁPIDO E SIMPLES"),nl,
    nl,
    write("Comandos:"),nl,
    write("c       - Adiciona um novo produto ao inventário"),nl,
    write("l       - Lista todos os produtos do inventário"),nl,
    write("mq      - Modifica a quantidade de um produto"),nl,
    write("mp      - Modifica o preço de um produto"),nl,
    write("d       - Remove um produto do inventário"),nl,
    write("v       - Verifica validade dos produtos"),nl,
    write("cv      - Verifica validade de um Produto"),nl,
    write("z       - Verifica itens zerados"),nl,
    write("h       - Mostrar comandos"),nl,
    write("q       - Sair"),nl,
    prompt.
    
prompt:-
    nl,
    write("Comando desejado:"),
    nl,
    read_string(Choice),
    (
        Choice == "c" -> create;
        Choice == "l" -> list;
        Choice == "mq" -> update_quantity;
        Choice == "mp" -> update_price;
        Choice == "d" -> delete;
        Choice == "v" -> filter_expired;
        Choice == "cv" -> verify_expired;
        Choice == "z" -> filter_out_of_stock;
        Choice == "h" -> menu;
        Choice == "q" -> write_storage(), halt;
        write("Comando inválido!"),
        nl,
        prompt
    ).

prompt:-
    true,
    nl,
    write("Dados inválidos!"),
    nl,
    prompt.

/*
    Adiciona um novo produto ao inventário
    TODO: validar os dados de entradas
*/
create:-
    write("Digite o nome do produto: "),
    nl,
    read_string(Nome),
    write("Digite a quantidade do produto: "),
    nl,
    read_number(Quantidade),
    write("Digite o preço do produto: "),
    nl,
    read_number(Preco),
    write("Digite a data de validade do produto: "),
    nl,
    read_string(Data),
    add_product(product(_, Nome, Quantidade, Preco, Data)),
    write("Produto adicionado com sucesso!"),nl,
    prompt.

/*
    Lista todos os produtos do inventário
*/
list:-
    write("uid | nome | quantidade | preco | validade"),
    nl,
    forall(storage:product(Id, Nome, Quantidade, Preco, Data),
        (
            write(Id),
            write(" | "),
            write(Nome),
            write(" | "),
            write(Quantidade),
            write(" | "),
            write(Preco),
            write(" | "),
            write(Data),
            nl
        )
    ),
    prompt.

/*
    Modifica a quantidade de um produto
    TODO: validar os dados de entrada
    TODO: verificar se o produto com o Id existe
*/
update_quantity:-
    write("Digite o id do produto: "),
    nl,
    read_number(Id),
    write("Digite a nova quantidade do produto: "),
    nl,
    read_number(Quantidade),
    update_quantity(Id, Quantidade),
    write("Quantidade modificada com sucesso!"),
    nl,
    prompt.

/*
    Modifica o preço de um produto
    TODO: validar os dados de entrada
    TODO: verificar se o produto com o Id existe
*/
update_price:-
    write("Digite o id do produto: "),
    nl,
    read_number(Id),
    write("Digite o novo preço do produto: "),
    nl,
    read_number(Preco),
    update_price(Id, Preco),
    write("Preço modificado com sucesso!"),
    nl,
    prompt.

/*
    Remove um produto do inventário
    TODO: verificar se o produto com o Id existe
*/
delete:-
    write("Digite o id do produto: "),
    nl,
    read_number(Id),
    delete_product(Id),
    write("Produto removido com sucesso!"),
    nl,
    prompt.

/*
    Verifica validade dos produtos
    TODO:
*/
filter_expired:-
    nl,
    storage:verify_expired_storage,
    prompt.

/*
    Verifica validade de um produto
    TODO: 
*/
verify_expired:-
    write("Digite o id do produto: "),
    nl,
    read_number(Id),
    storage:check_product_exists(Id),
    (storage:verify_expired_product(Id) -> write("Produto com validade vencida!"); 
    write("Produto com validade válida!")),
    prompt.

/*
    Verifica produtos com quantidade menor igual a zero
*/
filter_out_of_stock:-
    nl,
    verify_storage,
    prompt.

main:- 
    read_storage(),
    menu.