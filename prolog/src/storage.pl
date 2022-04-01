:- module(storage, [read_storage/0, write_storage/0, delete_product/1, add_product/1, update_price/2, update_quantity/2, verify_storage/0]).

:- use_module(library(csv)).
:- use_module(util).

:- dynamic
    product/5.

%% create_storage_file is det.
%Creates the storage file if it doesn't exist.
create_storage_file:-
    open('storage.csv', write, Stream),
    close(Stream).

%% read_storage is multi
%Reads the CSV file and returns a list of facts.
read_storage:-
    clean_up,
    exists_file('storage.csv'),
    csv_read_file('storage.csv',Products,[functor(product)]),
    assert_storage(Products).

% Reads the CSV file and returns a list of facts.
%If the file does not exist, it creates a new one, and returns an empty list.
read_storage:-
    clean_up,
    not(exists_file('storage.csv')),
    create_storage_file.

%% write_storage is multi
%Writes the facts in the CSV file.
write_storage:-
    exists_file('storage.csv'),
    condense_prod(Products),
    csv_write_file('storage.csv', Products).

%Writes the facts in the CSV file.
%If the file does not exist, it creates a new one, and writes the facts.
write_storage:-
    not(exists_file('storage.csv')),
    create_storage_file,
    condense_prod(Products),
    csv_write_file('storage.csv', Products).

%% assert_storage(+Products) is det
%Writes the rules in the knowledge base.
% @param Products is a list of facts.
assert_storage(Products):-
    maplist(assertz, Products).

%% condense_prod(-Products) is det
%Condenses the Products of the knowledge base into a list.
% @param Products is a list of facts.
condense_prod(Products):-
    findall(product(Id,Nome,Quantidade,Preco,Data),product(Id,Nome,Quantidade,Preco,Data),Products).

%% clean_up is det
%Resets the knowledge base, to avoid reading twice.
clean_up:-
    retractall(product(_,_,_,_,_)).

%% add_product(+Product) is det
%Add a product to the knowledge base.
% @param Product is a fact.
%TODO: Verify if a date is valid.
add_product(Product):-
    generate_id_length(Id),
    Product = product(Id,Nome,Quantidade,Preco,Data),
    util:check_product(Nome,Quantidade,Preco,Data),
    assertz(Product).

%% generate_id_length(-Id) is det
%Generates an ID for a product.
% @param Id is an integer.
generate_id_length(NextId):-
    findall(Id,product(Id,_,_,_,_),Ids),
    length(Ids,LastId),
    NextId is LastId.

%% generate_id_empty(-Id) is det
%Generates an ID for a product by the empty spot at the knowledge base.
% @param NextId is an integer.
generate_id_empty(NextId):-
    call(product(NextId,_,_,_,_)),
    NextId is Id + 1,
    generate_if_empty(NextId).

%% generate_id_empty(+Id) is det
%Ends the generation of an ID for a product.
% @param NextId is an integer.
generate_id_empty(NextId).

%% delete_product(+Id) is semidet
%Remove a product from the knowledge base by ID.
% @param Id is an integer.
delete_product(Id):- 
    call(product(Id,_,_,_,_)),
    retract(product(Id,_,_,_,_)),
    update_id(Id).

%% update_id(+Id) is det
%Update a ID of product in the knowledge base.
% @param Id is an integer.
update_product_id(Id):-
    NewId is Id - 1,
    retract(product(Id,Nome,Quantidade,Preco,Data)),
    assertz(product(NewId,Nome,Quantidade,Preco,Data)).

%% update_quantity(+Id, +Quantity) is semidet
%Update the quantity of a product in the knowledge base.
% @param Id is an integer.
% @param NewQuantity is an integer.
update_quantity(Id,NewQuant):-
    call(product(Id,_,_,_,_)),
    util:check_quantity(NewQuant),
    retract(product(Id,Nome,_,Preco,Data)),
    assertz(product(Id,Nome,NewQuant,Preco,Data)).

%% update_id(+Id) is det
%Update the Id of all products greater than Old Id in the knowledge base.
% @param Id is an integer.
update_id(OldId):-
    forall(product(Id,_,_,_,_), not(OldId<Id);update_product_id(Id)).

%% update_price(+Id, +Price) is semidet
%Update a price of product in the knowledge base.
% @param Id is an integer.
% @param Price is a float.
update_price(Id,NewPreco):-
    call(product(Id,_,_,_,_)),
    util:check_price(NewPreco),
    retract(product(Id,Nome,Quantidade,_,Data)),
    assertz(product(Id,Nome,Quantidade,NewPreco,Data)).

%% verify_storage is det
%Returns all products with quantity equals to 0 in the knowledge base.
verify_storage:-
    forall(product(Id, Nome, Quantidade, Preco, Data),
        (
            not(Quantidade =< 0);
            print_prod(product(Id, Nome, Quantidade, Preco, Data))
        )
    ).

%% verify_expired_product(+Id) is semidet
%Returns a product expired in the knowledge base.
% @param Id is an integer.
verify_expired_product(Id):-
    product(Id,_,_,_,DateString),
    util:parse_date(DateString, Date),
    date_time_stamp(Date, ProductTime),
    get_time(CurrentTime),
    util:diff_days(CurrentTime, ProductTime, Diff),
    Diff>0.

%% verify_expired_storage is det
%Returns all products expired in the knowledge base.
verify_expired_storage:-
    forall(product(Id, Nome, Quantidade, Preco, Data),  
        (
            not(verify_expired_product(Id));
            print_prod(product(Id, Nome, Quantidade, Preco, Data))
        )
    ).

%% print_prod(+Product) is det
%Prints a product in the knowledge base.
% @param Product is a fact.
print_prod(product(Id,Nome,Quantidade,Preco,Data)):-
    write(Id),
    write(" | "),
    write(Nome),
    write(" | "),
    write(Quantidade),
    write(" | "),
    write(Preco),
    write(" | "),
    write(Data),nl.