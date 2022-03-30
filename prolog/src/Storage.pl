:- module(storage, [read_storage/0]).

:- use_module(library(csv)).

%Reads the CSV file and returns a list of rules.
read_storage:-
    csv_read_file('storage-example.csv',Products,[functor(product)]),
    write(Products),
    assert_storage(Products).

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