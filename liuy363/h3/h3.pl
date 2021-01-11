% leaf tree
leaf(X).
branch(X,Y).

% bin tree
empty.
node(Left,Value,Right).

% flatten for leaf tree
flatten(leaf(X),[L|[]]) :- L is X. 
flatten(branch(X,Y),L) :- flatten(X,M), flatten(Y,N), append(M,N,L).

% flatten for bin tree
flatten(empty, []).
flatten(node(Left,Value,Right),L) :- flatten(Left,ListL),flatten(Right,ListR),append(ListL,[Value|ListR],L).

% sorting using accumulator
mySort(List,SortedList) :- insertion_sort(List,[],SortedList).
insertion_sort([],Acc,Acc).                                                            % when the list is empty
insertion_sort([H|T],Acc,SortedList) :- insert_first(H,Acc,NAcc),insertion_sort(T,NAcc,SortedList).   % insert the first element to the accumulator, then recursively insert the tail
% insert the first element(only one element) to a list  
insert_first(X,[],[X]).
insert_first(X,[Y|T],[Y|NT]) :- X>Y,insert_first(X,T,NT).    % when X is bigger than the first element of accumulator, recursively insert X to the part of list starting after Y
insert_first(X,[Y|T],[X,Y|T]) :- X=<Y.                 % when X is smaller or equal to the first element in the accumulator, insert X in the head

% sort the tree
elemsOrdered(T,L) :- flatten(T,Lt),mySort(Lt,L).

