% Here are a bunch of facts describing the Simpson's family tree.
% Don't change them!

female(mona).
female(jackie).
female(marge).
female(patty).
female(selma).
female(lisa).
female(maggie).
female(ling).

male(abe).
male(clancy).
male(herb).
male(homer).
male(bart).

married_(abe,mona).
married_(clancy,jackie).
married_(homer,marge).

married(X,Y) :- married_(X,Y).
married(X,Y) :- married_(Y,X).

parent(abe,herb).
parent(abe,homer).
parent(mona,homer).

parent(clancy,marge).
parent(jackie,marge).
parent(clancy,patty).
parent(jackie,patty).
parent(clancy,selma).
parent(jackie,selma).

parent(homer,bart).
parent(marge,bart).
parent(homer,lisa).
parent(marge,lisa).
parent(homer,maggie).
parent(marge,maggie).

parent(selma,ling).




%%
% Part 1. Family relations
%%

% 1. Define a predicate `child/2` that inverts the parent relationship.
child(Y,X) :- parent(X,Y).


% 2. Define two predicates `isMother/1` and `isFather/1`.
isMother(X) :- parent(X,_), female(X).
isFather(X) :- parent(X,_), male(X).

% 3. Define a predicate `grandparent/2`.
%if X is grandparent and Y is grandchild
grandparent(X,Y) :- parent(Z,Y),parent(X,Z).

% 4. Define a predicate `sibling/2`. Siblings share at least one parent.
sibling(X,Y) :- parent(Z,X), parent(Z,Y), X \= Y.

% 5. Define two predicates `brother/2` and `sister/2`.
brother(X,Y) :- sibling(X,Y), male(X).
sister(X,Y) :- sibling(X,Y), female(X).

% 6. Define a predicate `siblingInLaw/2`. A sibling-in-law is either married to
%    a sibling or the sibling of a spouse.
siblingInLaw(X,Y) :- married(Y,Z), sibling(X,Z).
siblingInLaw(X,Y) :- married(X,Z), sibling(Y,Z).
siblingInLaw(X,Y) :- sibling(X,Z), married(Z,Y).
siblingInLaw(X,Y) :- sibling(Y,Z), married(Z,X).

% 7. Define two predicates `aunt/2` and `uncle/2`. Your definitions of these
%    predicates should include aunts and uncles by marriage.
aunt(X,Y) :- sibling(X,Z), parent(Z,Y) ,female(X).
aunt(X,Y) :- siblingInLaw(X,Z), parent(Z,Y), female(X).

uncle(X,Y) :- sibling(X,Z), parent(Z,Y) ,male(X).
uncle(X,Y) :- siblingInLaw(X,Z), parent(Z,Y), male(X).

% 8. Define the predicate `cousin/2`.
cousin(X,Y) :- parent(Z,X), aunt(Z,Y).
cousin(X,Y) :- parent(Z,Y), aunt(Z,X).
cousin(X,Y) :- parent(Z,X), uncle(Z,Y).
cousin(X,Y) :- parent(Z,Y), uncle(Z,X).

% 9. Define the predicate `ancestor/2`.
ancestor(X,Y) :- parent(X,Y).
ancestor(X,Y) :- parent(X,Z), ancestor(Z,Y).

% Extra credit: Define the predicate `related/2`.

related(X,Y) :- ancestor(X,Y).
related(X,Y) :- ancestor(Y,X).
related(X,Y) :- aunt(X,Y).
related(X,Y) :- aunt(Y,X).
related(X,Y) :- parent(X,Y).
related(X,Y) :- parent(X,Z), married(Z, Y).
related(X,Y) :- parent(X,Z), married(Z, W), parent(Y,W).
related(X,Y) :- parent(X,Z), married(Z, W), sibling(W,Y).
related(X,Y) :- parent(X,Z), married(Z, W), sibling(W,V), parent(V,Y).
related(X,Y) :- child(X,Y).
related(X,Y) :- cousin(X,Y).
related(X,Y) :- uncle(X,Y).
related(X,Y) :- uncle(Y,X).
related(X,Y) :- sibling(X,Y).
related(X,Y) :- siblingInLaw(X,Y).
related(X,Y) :- married(X,Y).






%%
% Part 2. Language implementation (see course web page)
%%

%% Define the predicate cmd/3, which describes the effect of a command on the stack. 
%% That is, the predicate cmd(C,S1,S2) means that executing command C 
%% with stack S1 produces stack S2.

cmd(add, [A, B|List], R)    :- Result is (A+B), R = [Result|List].
cmd(lte, [A, B|List], R)    :- Bool = (A =< B -> Res=t; Res=f ), call(Bool), R = [Res|List].
% cmd(lte, [A, B|Stack], R)   :- Bool = (A >= B -> Res=f ), call(Bool), R = [Res|Stack]
cmd(if(T,_), [t|List], R)   :- prog(T, List, R).
cmd(if(_,F), [f|List], R)   :- prog(F, List, R).
cmd(Cmd, Stack1, Stack2)              :- Stack2 = [Cmd|Stack1].


%% Define the predicate prog/3, which describes the effect of a program on the stack. 
%% That is, the predicate prog(P,S1,S2) means that executing program P with 
%% stack S1 produces stack S2.

prog([], Stack1, Stack2)    :- Stack2 = Stack1.
prog([X|L], Stack1, Stack2)         :-cmd(X, Stack1, Stack3), prog(L, Stack3, Stack2).
