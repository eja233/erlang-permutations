%%% cube_permutations_test module: implements tests to check if the 
%%% number output by cube_permutations is the smallest cube for which 
%%% exactly five permutations of its digits are cube. 
%%% Exports: is_cube/0 checks that the number is a cube, five permutations/0 
%%% checks that five  permutations of it are cube, and smallest_cube/0 checks 
%%% that the cube is the smallest cube that is a permuation of its digits. 
-module(cube_permutations_test).
-export([is_cube/0,five_permutations/0,smallest_cube/0]).

%% cube/1: takes a list as input and cubes every number in a list.
cube([H|T]) -> [H*H*H|cube(T)];
cube([]) -> [].

%% cubes_tuple/1: takes a list of numbers as input and outputs
%% tuples consisting of the numbers and their cubes.
cubes_tuple([H|T]) -> [{H,H*H*H}|cubes_tuple(T)];
cubes_tuple([]) -> [].

%% digits/1: takes a list as input and finds the digits of each number in the
%% list and makes a new list consisting of tuples of those digits. 
digits([H|T]) -> [lists:sort(integer_to_list(H))|digits(T)];
digits([]) -> [].

%% digits_tuple/1: takes a list as input and finds the digits of each number 
%% in a list and makes a new list consisting of tuples of the numbers and 
%% their digits.
digits_tuple([H|T]) -> [{H,lists:sort(integer_to_list(H))}|digits_tuple(T)];
digits_tuple([]) -> [].


%% is_cube/0: checks that the answer given by cube_permutations is cube.
is_cube() ->
    % Create a list of tuples of numbers and cubes for which the cubes
    % have 12 digits (like the answer in cube_permutations). The lowest
    % number with a 12 digit square is 4642, and the highest is 9999. 
    Cubes_list = cubes_tuple(lists:seq(4642,9999)),
    
    % Store the answer found in cube_permutations in Answer.
    Answer = cube_permutations:start(),

    % Look for the answer given by cube_permutations in Cubes_list. If
    % the answer is not found, lists:keyfind will return false, and an
    % error will occur because it will not pattern match the tuple on the LHS. 
    % Otherwise, a tuple of a number and its cube (Answer) will be output, 
    % and the test has been passed.
    {_,Cube} = lists:keyfind(Answer,2,Cubes_list),
    if Cube =:= Answer ->
    	io:format("Test passed! Result is a cube.~n");
    true ->
        io:format("Test failed. Result isn't a cube.~n")
    end.


%% five_permutations/0: checks if there are five permutations of 
%% the digits of the answer found in cube_permutations() that are 
%% cubes. 
five_permutations() ->
    % Create a list of all 12-digit cubes. The cube root of these numbers
    % will be between 4642 and 9999.
    Cubes = cube(lists:seq(4642,9999)),

    % Convert the list of cubes into a list of digits. 
    Digits = digits(Cubes),

    % Find the digits of the answer from cube_permutations.
    Answer_digits = lists:sort(integer_to_list(cube_permutations:start())),

    % Filter out all the digits in Digits that are not Answer_cubes.
    Answer_occurrences = lists:filter(fun(X) -> X =:= Answer_digits end,Digits),

    % Count the number of times the digits are cubes by finding the length
    % of Answer_occurrences.
    Num_occur = length(Answer_occurrences),
    
    % Pass the test if there are 5 cubes, otherwise, fail.
    if Num_occur =:= 5 ->
        io:format("Test passed! There are 5 permutations.~n");
    true ->
    	io:format("Test failed. There are ~p permutations.~n",[Num_occur])
    end.


%% smallest_cube/0: checks that the cube in the answer is the smallest
%% cube permutation of the digits. 
smallest_cube() ->
    % Create a list of all 12-digit cubes. The cube root of these numbers
    % will be between 4642 and 9999.
    Cubes = cube(lists:seq(4642,9999)),

    % Create a list of tuples of cubes and their digits.
    Cubes_digits = digits_tuple(Cubes),

    % Store the answer from cube_permutations.
    Answer = cube_permutations:start(),

    % Find the digits of the answer from cube_permutations.
    Answer_digits = lists:sort(integer_to_list(Answer)),

    % Filter out the tuples which have digits that aren't Answer_digits.
    Answer_tuples = lists:filter(fun({_,Y}) -> Y =:= Answer_digits end,
    	Cubes_digits),

    % Make a new list of just the cubes which have digits the same as
    % Answer_digits.
    {Answer_cubes,_} = lists:unzip(Answer_tuples),

    % Select the minimum value of the Cubes which have digits the same
    % as Answer_digits. 
    Min_cube = lists:min(Answer_cubes),
    

    % If Min_cube is the same as the answer obtained from cube_permutations,
    % the answer from cube_permutations is the lowest cube, and the 
    % test passes. Otherwise, the test fails. 
    if Min_cube =:= Answer ->
    	io:format("Test passed! The result is the minimum cube.~n");
    true -> 
        io:format("Test failed. The smallest cube is ~p.~n", [Min_cube])
    end.