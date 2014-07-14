/* vim: set ft=prolog: */
foo(x).
foo(y) :- !.
foo(z).
bar(x).
bar(y).
bar(z).
?- foo(X).
?- bar(X).
?- bar(X),!.
?- foo(X),fail.
?- bar(X),fail.
?- fail,fail.
true.
not(X) :- X,!,fail.
not(_).
?- not(true).
?- not(fail).
not1(X,x) :- X,!,fail.
not1(_,y).
?- not1(true,Z).
?- not1(fail,Z).
?- bar(X).
?- !,bar(X).
