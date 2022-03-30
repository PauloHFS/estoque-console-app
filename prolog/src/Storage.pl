:- module(storage, [read_storage/0]).

:- use_module(library(csv)).

:- dynamic
    product/5.

%Reads the CSV file and returns a list of rules.
read_storage:-
    clean_up,
    csv_read_file('E:\\Developer\\estoque-console-app\\prolog\\storage-example.csv',Products,[functor(product)]),
    assert_storage(Products).

%Writes the rules in the CSV file.
write_storage:-
    condense_prod(Products),
    csv_write_file('E:\\Developer\\estoque-console-app\\prolog\\storage-example.csv', Products).

%Writes the rules in the knowledge base.
assert_storage(Products):-
    maplist(assertz, Products).

%Condenses the Products of the knowledge base into a list.
condense_prod(Products):-
    findall(product(Id,Nome,Quantidade,Preco,Data),product(Id,Nome,Quantidade,Preco,Data),Products),
    writeln(Products).

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

update_uid(OldID):-
    retractall(product((N,_,_,_,_), N>OldId)),
    write(condense_prod(Products)).

verify_storage(Product, ProductVazios).

verify_expired_product(Product, ProductExpired).

verify_expired_storage(Product, StorageExpired).

diffDays(Time1, Time2, Diff):-
    stamp_date_time(Time1, DateTime, local),
    date_time_value(date, DateTime, Date),
	date(YYYY, MM, DD) = Date,
	date_time_stamp(date(YYYY,MM,DD,0,0,0,0,-,-), Time1Date),
    Diff is (Time1Date - Time2) / 86400.
