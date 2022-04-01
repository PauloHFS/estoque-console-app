:- module(util, [check_product/4, check_price/1, check_quantity/1]).

%Checks if a product is valid.
check_product(Nome, Quantidade, Preco, Data):-
    Nome \= '',
    Quantidade \= '',
    Preco \= '',
    Data \= '',
    Preco > 0,
    Quantidade >= 0.

%Checks if a quantity is valid.
check_quantity(NewQuantity):-
    NewQuantity >= 0.

%Checks if a price is valid.
check_price(NewPrice):-
    NewPrice > 0.