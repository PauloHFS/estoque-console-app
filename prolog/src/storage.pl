:- module(storage, [read_storage/0, write_storage/0, delete_product/1, add_product/1, update_price/2, update_quantity/2]).

:- use_module(library(csv)).

:- dynamic
    product/5.

create_storage_file:-
    open('storage.csv', write, Stream),
    close(Stream).

%Reads the CSV file and returns a list of rules.
read_storage:-
    clean_up,
    exists_file('storage.csv'),
    csv_read_file('storage.csv',Products,[functor(product)]),
    assert_storage(Products).

read_storage:-
    clean_up,
    not(exists_file('storage.csv')),
    create_storage_file.

%Writes the rules in the CSV file.
write_storage:-
    exists_file('storage.csv'),
    condense_prod(Products),
    csv_write_file('storage.csv', Products).

write_storage:-
    not(exists_file('storage.csv')),
    create_storage_file,
    condense_prod(Products),
    csv_write_file('storage.csv', Products).

%Writes the rules in the knowledge base.
assert_storage(Products):-
    maplist(assertz, Products).

%Condenses the Products of the knowledge base into a list.
condense_prod(Products):-
    findall(product(Id,Nome,Quantidade,Preco,Data),product(Id,Nome,Quantidade,Preco,Data),Products).

%Resets the knowledge base, to avoid reading twice.
clean_up:-
    retractall(product(_,_,_,_,_)).

%Add a product to the knowledge base.
%TODO: Add a check to avoid duplicates.
%TODO: Add a check to avoid empty fields.
%TODO: Add a check to verify if the fields are valid.
%TODO: Add a generation of the ID.
add_product(Product):-
    generate_id(Id),
    Product = product(Id,Nome,Quantidade,Preco,Data),
    check_product(Nome,Quantidade,Preco,Data),
    assertz(Product).

%Generates an ID for a product.
generate_id(NextId):-
    findall(Id,product(Id,_,_,_,_),Ids),
    length(Ids,LastId),
    NextId is LastId.

%Remove a product from the knowledge base by ID.
%TODO: Add a check to verify if the ID exists.
delete_product(Id):- 
    call(product(Id,_,_,_,_)),
    retract(product(Id,_,_,_,_)),
    update_id(Id).

%Update a ID of product in the knowledge base.
update_product_id(Id):-
    NewId is Id - 1,
    retract(product(Id,Nome,Quantidade,Preco,Data)),
    assertz(product(NewId,Nome,Quantidade,Preco,Data)).

update_quantity(Id,NewQuant):-
    retract(product(Id,Nome,Quantidade,Preco,Data)),
    assertz(product(Id,Nome,NewQuant,Preco,Data)).

%Update the Id of all products greater than Old Id in the knowledge base.
update_id(OldId):-
    forall(product(Id,Nome,Quantidade,Preco,Data), not(OldId<Id);update_product_id(Id)).

%Update a price of product in the knowledge base.
%TODO: Add a check to verify if the ID exists.
%TODO: Add a check to verify if the price is valid.
%TODO: Add a check to verify if the price is different from the old one.
update_price(Id,NewPreco):-
    retract(product(Id,Nome,Quantidade,Preco,Data)),
    assertz(product(Id,Nome,Quantidade,NewPreco,Data)).

verify_storage(Product, ProductVazios).

verify_expired_product(Id):-
    product(Id,Nome,Quantidade,Preco,DateString),
    parse_date(DateString, Date),
    date_time_stamp(Date, ProductTime),
    get_time(CurrentTime),
    diff_days(CurrentTime, ProductTime, Diff),
    Diff>0.

verify_expired_storage:-
    forall(product(Id, Nome, Quantidade, Preco, Data), not(verify_expired_product(Id)); 
    (
    write(Id),
    write(" | "),
    write(Nome),
    write(" | "),
    write(Quantidade),
    write(" | "),
    write(Preco),
    write(" | "),
    write(Data),nl)
    ).

/*
    Calculates the difference between two Timestamps in days.
*/
diff_days(Time1, Time2, Diff):-
    stamp_date_time(Time1, DateTime, local),
    date_time_value(date, DateTime, Date),
	date(YYYY, MM, DD) = Date,
	date_time_stamp(date(YYYY,MM,DD,0,0,0,0,-,-), Time1Date),
    Diff is (Time1Date - Time2) / 86400.

/*
    Parses a String in the DD/MM/YYYY format into a Date.
*/
parse_date(String, Date):-
	atom_chars(String, [C1,C2,C3,C4,C5,C6,C7,C8,C9,C10]),
	number_chars(Day, [C1,C2]),
	number_chars(Month, [C4,C5]),
	number_chars(Year, [C7,C8,C9,C10]),
	date(Year,Month,Day) = Date.

%Checks if a product is valid.
check_product(Nome, Quantidade, Preco, Data):-
    Nome \= '',
    Quantidade \= '',
    Preco \= '',
    Data \= '',
    Preco > 0,
    Quantidade > 0.
