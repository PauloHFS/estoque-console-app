:- module(storage, [read_storage/0, write_storage/0]).

:- use_module(library(csv)).

:- dynamic
    product/5.

%Reads the CSV file and returns a list of rules.
read_storage:-
    clean_up,
    csv_read_file('storage-example.csv',Products,[functor(product)]),
    assert_storage(Products),
    write('Storage loaded.'),nl.

%Writes the rules in the CSV file.
write_storage:-
    condense_prod(Products),
    csv_write_file('storage-example.csv', Products).

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
    assertz(Product).

%Remove a product from the knowledge base by ID.
delete_product(Id):- 
    call(product(Id,_,_,_,_)),
    retract(product(Id,_,_,_,_)).

%Update a product in the knowledge base.
change_product_id(Id):-
    NewId is Id - 1,
    call(product(Id,Nome,Quantidade,Preco,Data)),
    retract(product(Id,Nome,Quantidade,Preco,Data)),
    assertz(product(NewId,Nome,Quantidade,Preco,Data)).

update_uid(OldID):-
    retractall(product((N,_,_,_,_), N>OldId)),
    write(condense_prod(Products)).

verify_storage(Product, ProductVazios).

verify_expired_product(Product, ProductExpired).

verify_expired_storage(Product, StorageExpired).

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

