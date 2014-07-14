/* vim: set ft=prolog: */

included([],_).
included([A|X],Y) :- member(A,Y), included(X,Y).

replaceOnceWith([A|X],A,C,[C|X]).
replaceOnceWith([A|X],B,C,[A|Y]) :- replaceOnceWith(X,B,C,Y).

winpattern(X,[X,X,X,_,_,_,_,_,_]) :- !.
winpattern(X,[_,_,_,X,X,X,_,_,_]) :- !.
winpattern(X,[_,_,_,_,_,_,X,X,X]) :- !.
winpattern(X,[X,_,_,X,_,_,X,_,_]) :- !.
winpattern(X,[_,X,_,_,X,_,_,X,_]) :- !.
winpattern(X,[_,_,X,_,_,X,_,_,X]) :- !.
winpattern(X,[X,_,_,_,X,_,_,_,X]) :- !.
winpattern(X,[_,_,X,_,X,_,X,_,_]) :- !.

win(_,Y,B) :- winpattern(Y,B), !, fail.
win(X,Y,B) :- replaceOnceWith(B,e,X,B2),lose(Y,X,B2),!.
lose(_,Y,B) :- winpattern(Y,B), !.
lose(X,Y,B) :- included(B,[X,Y]), !, fail.
lose(X,Y,[B0,B1,B2,B3,B4,B5,B6,B7,B8]) :-
  lose(X,Y,B0,[ X,B1,B2,B3,B4,B5,B6,B7,B8]),
  lose(X,Y,B1,[B0, X,B2,B3,B4,B5,B6,B7,B8]),
  lose(X,Y,B2,[B0,B1, X,B3,B4,B5,B6,B7,B8]),
  lose(X,Y,B3,[B0,B1,B2, X,B4,B5,B6,B7,B8]),
  lose(X,Y,B4,[B0,B1,B2,B3, X,B5,B6,B7,B8]),
  lose(X,Y,B5,[B0,B1,B2,B3,B4, X,B6,B7,B8]),
  lose(X,Y,B6,[B0,B1,B2,B3,B4,B5, X,B7,B8]),
  lose(X,Y,B7,[B0,B1,B2,B3,B4,B5,B6, X,B8]),
  lose(X,Y,B8,[B0,B1,B2,B3,B4,B5,B6,B7, X]).
lose(X,Y,e,B) :- !, win(Y,X,B).
lose(_,_,_,_).

tie(X,Y,B) :- \+ win(X,Y,B), \+ lose(X,Y,B).

member(X,[X|_]).
member(X,[_|A]) :- member(X,A).
\+ X :- X, ! , fail.
\+ _.

%?- tie(o,x,[e,e,e,e,e,e,e,e,e]). % it takes long time on shumilog
%?- win(o,x,[o,e,e,x,e,e,e,e,e]).
%?- win(o,x,[e,e,e,x,o,e,e,e,e]).
%?- win(o,x,[e,o,e,x,e,e,e,e,e]).
