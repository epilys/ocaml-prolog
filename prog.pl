male(eric).
male(kyle).
male(stan).
male(randy).
parent(randy,stan).

is_father_to_son(X,Y) :-
    male(X),
    male(Y),
    parent(X,Y).
