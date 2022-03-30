:- consult('src/Storage.pl').

waitKey(ValidKeys, ReturnedKey) :- 
    write('>'),                      
    get_single_char(X),                        
    char_code(Y, X),                           
    (member(Y, ValidKeys) ->
    ReturnedKey = Y;
    waitKey(ValidKeys, K), ReturnedKey = K).

get_key(X):-
    ttyflush, 
    get_single_char(Y), 
    atom_char(X, Y).

%Display a console menu in prolog
menu:-
    read_storage(),
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
    waitKey(['c','l','mq','mp','d','v','cv','z','h','q'], Choice),
    (
        Choice == 'c' -> addProduct;
        Choice == 'l' -> listProducts;
        Choice == 'mq' -> modifyQuantity;
        Choice == 'mp' -> modifyPrice;
        Choice == 'd' -> removeProduct;
        Choice == 'v' -> verifyProducts;
        Choice == 'cv' -> verifyProduct;
        Choice == 'z' -> verifyZeros;
        Choice == 'h' -> menu;
        Choice == 'q' -> halt;
        prompt
    ).

dynamic product/5.

addProduct:-
    write('Digite o id do produto: '),
    read(Id),
    write("Digite o nome do produto: "),
    read(Nome),
    write("Digite a quantidade do produto: "),
    read(Quantidade),
    write("Digite o preço do produto: "),
    read(Preco),
    write("Digite a data de validade do produto: "),
    read(Data),
    assert(product(Id, Nome, Quantidade, Preco, Data)),
    write("Produto adicionado com sucesso!"),nl,
    prompt.

listProducts:-
    write("Lista de produtos:"),nl,
    forall(product(Id, Nome, Quantidade, Preco, Data),
        (
            write("("),
            write(Id),
            write(") "),nl,
            write(Nome),
            write(" - Quantidade: "),
            write(Quantidade),
            write(" - Preço: "),
            write(Preco),
            write(" - Data: "),
            write(Data),nl
        )
    ),
    prompt.

modifyQuantity:-
    write("Digite o id do produto: "),
    read(Id),
    write("Digite a nova quantidade do produto: "),
    read(Quantidade),
    retract(product(Id, Nome, _, Preco, Data)),
    assert(product(Id, Nome, Quantidade, Preco, Data)),
    write("Quantidade modificada com sucesso!"),nl,
    prompt.

modifyPrice:-
    write("Digite o id do produto: "),
    read(Id),
    write("Digite o novo preço do produto: "),
    read(Preco),
    retract(product(Name, Quantidade, _, Data)),
    assert(product(Nome, Quantidade, Preco, Data)),
    write("Preço modificado com sucesso!"),nl,
    prompt.

removeProduct:-
    write("Digite o id do produto: "),
    read(Id),
    retract(product(Id, _, _, _)),
    write("Produto removido com sucesso!"),nl,
    prompt.

verifyProducts:-
    write("Lista de produtos:"),nl,
    forall(product(Id, Nome, Quantidade, Preco, Data),
        (
            write("("),
            write(Id),
            write(") "),
            write(Nome),
            write(" - Quantidade: "),
            write(Quantidade),
            write(" - Preço: "),
            write(Preco),
            write(" - Data:"),
            write(Data),nl
        )
    ),
    write("Produtos com validade vencida:"),nl,
    forall(product(Id, Nome, Quantidade, Preco, Data),
        (
            (
                Quantity < 0 ->
                write("("),
                write(Id),
                write(") "),
                write(Nome),
                write(" - Quantidade: "),
                write(Quantidade),
                write(" - Preço: "),
                write(Preco),
                write(" - Data:"),
                write(Data),nl
            )
        )
    ),
    prompt.

verifyProduct:-
    write("Digite o nome do produto: "),
    read(Id),
    (
        product(Id, Nome, Quantidade, Preco, Data) ->
        write("Produto encontrado!"),nl,
        write("("),
        write(Id),
        write(") "),
        write(Nome),
        write(" - Quantidade: "),
        write(Quantidade),
        write(" - Preço: "),
        write(Preco),
        write(" - Data:"),
        write(Data),nl;
        write("Produto não encontrado!"),nl
    ),
    prompt.

verifyZeros:-
    write("Lista de produtos:"),nl,
    forall(product(Id, Nome, Quantidade, Preco, Data),
        (
            write("("),
            write(Id),
            write(") "),
            write(Nome),
            write(" - Quantidade: "),
            write(Quantidade),
            write(" - Preço: "),
            write(Preco),
            write(" - Data:"),
            write(Data),nl
        )
    ),
    write("Produtos com quantidade zerada:"),nl,
    forall(product(Id, Nome, Quantidade, Preco, Data),
        (
            (
                Quantity == 0 ->
                write("("),
                write(Id),
                write(") "),
                write(Nome),
                write(" - Quantidade: "),
                write(Quantidade),
                write(" - Preço: "),
                write(Preco),
                write(" - Data:"),
                write(Data),nl
            )
        )
    ),
    prompt.


main:- menu.