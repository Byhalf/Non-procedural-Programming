?- use_module(library(clpfd)).

%the months
month(feb,2,28,Y):- total_days(Y,N), N #= 365.
month(feb,2,29,Y):- total_days(Y,N), N #= 366.

month(jan,1,31,_).
month(mar,3,31,_).
month(apr,4,30,_).
month(may,5,31,_).
month(jun,6,30,_).
month(jul,7,31,_).
month(aug,8,31,_).
month(sep,9,30,_).
month(oct,10,31,_).
month(nov,11,30,_).
month(dec,12,31,_).

%convert date to days after new years
convert_date(date(Day,Month,Year), Days):-
    Day #=< Nbr_days,
    Day #> 0,
    month(Month,Month_nbr,Nbr_days,Year),
    Month_nbr #\= 1,
    Last_month_nbr #= Month_nbr - 1,
    Last_year #= Year -1,
    Days #= Day + Day2,
    month(Last_month,Last_month_nbr,Last_month_days,Last_year),
    convert_date(date(Last_month_days,Last_month,Last_year),Day2).


convert_date(date(Day,Month,Year), Days):-
    Day #=< Nbr_days,
    Day #> 0,
    month(Month,Month_nbr,Nbr_days,Year),
    Month_nbr #= 1,
    Days #= Day.


add_date(date(Day1,Month1,Year1),N,date(Day2,Month2,Year2)):-
    convert_date(date(Day1,Month1,Year1),Days1),
    convert_date(date(Day2,Month2,Year2),Days2),
    total_days(Year2, Days_in_year2),
    Days_to_end_year #= Days_in_year2 - Days2,
    total_days(Year1,Year2, Days_between_years),
    Days1 + Days_between_years + Days_to_end_year #= N.

total_days(Y,N) :- 
    N #=365, 
    R4 #= Y mod 4, 
    R4#\=0.

total_days(Y,N) :- 
    N #=366, 
    R4 #= Y mod 4, 
    R4#=0,
    R100 #= Y mod 100,
    R100 #\=0.

total_days(Y,N) :- 
    N #=366, 
    R4 #= Y mod 4, 
    R4#=0,
    R100 #= Y mod 100,
    R100 #=0,
    R400 #= Y mod 400,
    R400 #= 0.


total_days(Y,Y,N) :- total_days(Y,N).
total_days(Y,Z,N) :- 
        Z #> Y,
        Z1 #= Z -1,
        total_days(Z,N1),
        total_days(Y,Z1,N2),
        N #= N1 + N2.

