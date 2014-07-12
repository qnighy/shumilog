/* vim: set ft=prolog: */
male(kobo).
male(koji).
male(iwao).
female(sanae).
female(mine).
parent(kobo,koji).
parent(kobo,sanae).
parent(sanae,iwao).
parent(sanae,mine).
father(X,Y) :- parent(X,Y),male(Y).
mother(X,Y) :- parent(X,Y),female(Y).
?-(male(kobo)).
?-(female(sanae)).
?-(female(koji)).
?-(male(X)).
?-(parent(kobo,X)).
?-(parent(X,iwao)).
?-(mother(kobo,X)).
?-(father(kobo,X)).
?-(father(_,X)).
?-(mother(_,X)).
