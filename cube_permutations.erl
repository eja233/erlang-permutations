%%% cube_permutations module: implements the logic of finding the 
%%% smallest cube that has five permutations that are also cubes.
%%% Exports: start/0 outputs the smallest cube that has five permutations
%%% of its digits that are also cube: 127035954683.

%%% Possible optimizations: cutting down on the number of cubes generated
%%% would make the start/0 function faster. If we knew beforehand that
%%% the answer was 12 digits, we could restrict them to 4642 to 
%%% 9999, for instance. We could also remove numbers that have cubes
%%% with fewer than five permutations: for instance, 1000^3 = 1000000000
%%% does not have any permutations because a number won't start with 0. 
-module(cube_permutations).
-export([start/0]).

%% cube/1: cubes every number in a list.
cube([H|T]) -> [H*H*H|cube(T)];
cube([]) -> [].

%% make_list/1: changes a number into a list of the same digits
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

%% find_element/2: takes two lists as input. For each element of the first 
%% input list, finds the tuple in the second list with first element equal 
%% to that element.
find_element([H|T],L) -> [lists:keyfind(H,1,L)|find_element(T,L)];
find_element([],_) -> [].

%% start\0: outputs the value of the smallest cube for which five
%% permutations of its digits are cube. 
start() -> 
    % Cubes_digits: a list of tuples of the cubes of every number between
    % 345 and 9999 and the digits ordered from lowest to highest for these 
    % cubes. 345 is the lower bound because it's the cube root of the smallest 
    % number which has  three permutations of its digits which are also cube, 
    % so the smallest cube with five permutations should be equal or larger. 
    % 9999 is the upper bound because it's the smallest number whose cube 
    % has 12 digits, so  all possible permutations of cubes up to 12 digits
    % will be considered.
    Cubes_digits = lists:sort(num_digits_list(cube(lists:seq(345,9999)))),

    % Extract the digits from the tuples in Cubes_digits into Digits.
    {Digits,_} = lists:unzip(Cubes_digits),

    % Find the unique digits in Digits. 
    Unique_digits = lists:usort(Digits),

    % Subtract Unique_digits from Digits_list four times, leaving the digits 
    % which were initially there five or more times--the digits for which
    % at least five permutations are cube.
    Five_perms = lists:usort(lists:subtract(Digits, 
    	duplicate_list(Unique_digits,4))),

    % Find the digits for which at least six permutations are cube in the 
    % same way.
    Six_perms = lists:usort(lists:subtract(Digits,
    	duplicate_list(Unique_digits,5))),

    % Find the digits for which exactly five permutations are cube by 
    % subtracting Six_perms from Five_perms.
    Exactly_five = lists:subtract(Five_perms,Six_perms),

    % Find the {digits, cubes} tuples corresponding to the first appearance
    % of the digits in Exactly_five in Cubes_digits. Since Cubes_digits
    % is sorted from lowest to highest, these will be the lowest cubes
    % with these digits. Extract the Cubes tuples.
    {_,Cubes} = lists:unzip(find_element(Exactly_five,Cubes_digits)),

    % The value of the smallest cube for which five permutations of its
    % digits are cubes is the smallest number in the Cubes list.
    % Output that value.
    lists:min(Cubes).



