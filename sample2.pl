/* vim: set ft=prolog: */
append([],X,X).
append([A|X],Y,[A|Z]) :- append(X,Y,Z).
?- append([x,y], [z], X).
