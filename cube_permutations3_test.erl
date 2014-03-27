%%% Elaine Arbaugh 12/21/13

%%% cube_permutations3_test module: includes functions implementing the logic of
%%% finding the smallest cube that has three permutations that are also 
%%% cubes. Tests whether the answer obtained is equal to the answer 
%%% in the problem statement: 41063625.
%%% Exports: start/0 indicates whether the answer obtained is equal to 
%%% 41063625.
-module(cube_permutations3_test).
-export([start/0]).

%% cube/1: cubes every number in a list.
cube([H|T]) -> [H*H*H|cube(T)];
cube([]) -> [].

%% make_list/1: changes a number into a list with the same digits
%% ordered from lowest to highest. 
make_list(Num) -> lists:sort(integer_to_list(Num)).

%% num_digits_list/1: takes a list of numbers and creates a new
%% list consisting of tuples of the sorted digits of a number and the
%% number.
num_digits_list([H|T]) -> [{make_list(H),H}|num_digits_list(T)];
num_digits_list([]) -> [].

%% duplicate_list/2: takes a list and a number N as inputs
%% and duplicates that list N times. 
duplicate_list([H|T],N) -> lists:append(lists:duplicate(N,H),
	                       duplicate_list(T,N));
duplicate_list([],_) -> [].

%% find_element/2: Takes two lists as input. For each element of the first 
%% input list, finds the tuple in the second list with first element 
%% equal to that element.
find_element([H|T],L) -> [lists:keyfind(H,1,L)|find_element(T,L)];
find_element([],_) -> [].

%% start/0: outputs the value of the smallest cube for which three
%% permutations of its digits are cube. 
start() -> 
    % Cubes_digits: a list of tuples of the cubes of every number between
    % 1 and 999 and the digits ordered from lowest to highest for these 
    % cubes. 999 is the upper bound because its cube is the largest cube
    % with 9 digits. 
    Cubes_digits = lists:sort(num_digits_list(cube(lists:seq(1,999)))),

    % Extract the digits from the tuples in Cubes_digits into Digits.
    {Digits,_} = lists:unzip(Cubes_digits),

    % Find the unique digits in Digits. 
    Unique_digits = lists:usort(Digits),

    % Subtract Unique_digits from Digits_list four times, leaving the digits 
    % which were initially there three or more times--the digits for which
    % at least three permutations are cube.
    Three_perms = lists:usort(lists:subtract(Digits, 
        duplicate_list(Unique_digits,2))),

    % Find the digits for which at least four permutations are cube in the 
    % same way.
    Four_perms = lists:usort(lists:subtract(Digits,
    	duplicate_list(Unique_digits,3))),

    % Find the digits for which exactly three permutations are cube by 
    % subtracting Four_perms from Three_perms.
    Exactly_three = lists:subtract(Three_perms,Four_perms),

    % Find the {digits, cubes} tuples corresponding to the first appearance
    % of the digits in Exactly_five in Cubes_digits. Since Cubes_digits
    % is sorted from lowest to highest, these will be the lowest cubes
    % with these digits. Extract the Cubes tuples.
    {_,Cubes} = lists:unzip(find_element(Exactly_three,Cubes_digits)),

    % The value of the smallest cube for which five permutations of its
    % digits are cubes is the smallest number in the Cubes list.
    Answer = lists:min(Cubes),

    % Answer should be the smallest cube such that two other permutations
    % of its digits are also cube. From the problem statement, we know this
    % is 41063625. If it is this, the tests passes.
    if Answer =:= 41063625 ->
    	io:format("Test passed! Result is 41063625.\n");
    true ->
        io:format("Test failed. Result is not 41063625.\n")
    end.




