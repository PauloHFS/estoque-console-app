:- module(util, [check_product/4, check_price/1, check_quantity/1, check_date/1, parse_date/2, diff_days/3, read_string/1, read_number/1]).

%% check_product(+Product, +Quantity, +Price, +Data) is semidet
%Checks if a product is valid.
% @param Nome is the name of the product.
% @param Quantidade is the quantity of the product.
% @param Preco is the price of the product.
% @param Data is expiration date of the product.
check_product(Nome, Quantidade, Preco, Data):-
    Nome \= '',
    Quantidade \= '',
    Preco \= '',
    Data \= '',
    Preco > 0,
    Quantidade >= 0,
    check_date(Data).

%% check_quantity(+Quantity) is semidet
%Checks if a quantity is valid.
% @param Quantidade is the quantity of the product.
check_quantity(NewQuantity):-
    NewQuantity >= 0.

%% check_price(+Price) is semidet
%Checks if a price is valid.
% @param Preco is the price of the product.
check_price(NewPrice):-
    NewPrice > 0.
    
%% check_date(+Date) is semidet
%Checks if a date is valid.
% @param Date is the expiration date of the product.
check_date(Date):-
	atom_chars(Date, [C1,C2,C3,C4,C5,C6,C7,C8,C9,C10]),
    forall(member(X,[C1,C2,C4,C5,C7,C8,C9,C10]), char_type(X, digit)),
    C3 == '/',
   	C6 = '/'.
       
%% read_string(-String) is nondet
%Reads a line from the terminal and returns it as String.
% @param String is the line read from the terminal.
read_string(String) :-
    current_input(Input),
    read_line_to_codes(Input, Codes),
    string_codes(String, Codes).
%% read_number(-Number) is nondet
%Reads a line from the terminal and returns it as Number.
% @param Number is the line read from the terminal.
read_number(Number):-
    read_string(String),
    number_string(Number, String).
%% diff_days(+Date1, +Date2, -Diff) is det
%Calculates the difference between two Timestamps in days.
% @param Date1 is the first Timestamp.
% @param Date2 is the second Timestamp.
% @param Diff is the difference between the two Timestamps.
diff_days(Time1, Time2, Diff):-
    stamp_date_time(Time1, DateTime, local),
    date_time_value(date, DateTime, Date),
	date(YYYY, MM, DD) = Date,
	date_time_stamp(date(YYYY,MM,DD,0,0,0,0,-,-), Time1DateOnly),
    Diff is (Time1DateOnly - Time2) / 86400.

%% parse_date(+String, -Date) is nondet
%Parses a String to a Date.
% @param String is the String to be parsed.
parse_date(String, Date):-
	atom_chars(String, [C1,C2,C3,C4,C5,C6,C7,C8,C9,C10]),
	number_chars(Day, [C1,C2]),
	number_chars(Month, [C4,C5]),
	number_chars(Year, [C7,C8,C9,C10]),
	date(Year,Month,Day) = Date.