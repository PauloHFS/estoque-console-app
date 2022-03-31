:- use_module('storage.pl').

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
    read(Choice),
    (
        Choice == 'c' -> create;
        Choice == 'l' -> list;
        Choice == 'mq' -> updateQuantity;
        Choice == 'mp' -> updatePrice;
        Choice == 'd' -> delete;
        Choice == 'v' -> filterByValidade;
        Choice == 'cv' -> checaValidade;
        Choice == 'z' -> filterByQuantityZero;
        Choice == 'h' -> menu;
        Choice == 'q' -> write_storage(), halt;
        write("Comando inválido!"),
        nl,
        prompt
    ).

/*
    Adiciona um novo produto ao inventário
    TODO: validar os dados de entradas
*/
create:-
    write("Digite o nome do produto: "),
    nl,
    read(Nome),
    write("Digite a quantidade do produto: "),
    nl,
    read(Quantidade),
    write("Digite o preço do produto: "),
    nl,
    read(Preco),
    write("Digite a data de validade do produto: "),
    nl,
    read(Data),
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
updateQuantity:-
    write("Digite o id do produto: "),
    nl,
    read(Id),
    write("Digite a nova quantidade do produto: "),
    nl,
    read(Quantidade),
    update_quantity(Id, Quantidade),
    write("Quantidade modificada com sucesso!"),
    nl,
    prompt.

/*
    Modifica o preço de um produto
    TODO: validar os dados de entrada
    TODO: verificar se o produto com o Id existe
*/
updatePrice:-
    write("Digite o id do produto: "),
    nl,
    read(Id),
    write("Digite o novo preço do produto: "),
    nl,
    read(Preco),
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
    read(Id),
    delete_product(Id),
    write("Produto removido com sucesso!"),
    nl,
    prompt.

/*
    Verifica validade dos produtos
    TODO:
*/
filterByValidade:-
    storage:verify_expired_storage,
    prompt.

/*
    Verifica validade de um produto
    TODO: 
*/
checaValidade:-
    write("Digite o id do produto: "),
    nl,
    read(Id),
    (storage:verify_expired_product(Id) -> write("Produto com validade vencida!"); 
    write("Produto com validade válida!")),
    prompt.

/*
    Verifica produtos com quantidade menor igual a zero
*/
filterByQuantityZero:-
    write("Produtos com quantidade zerada:"),
    nl,
    forall(storage:product(Id, Nome, Quantidade, Preco, Data),
        (
            not(Quantidade =< 0);
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

main:- 
    read_storage(),
    menu.