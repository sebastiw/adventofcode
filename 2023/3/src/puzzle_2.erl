-module(puzzle_2).

-export([run/1]).

run(FileName) ->
    {ok, Bin0} = file:read_file(FileName),
    Bin = re:replace(Bin0, "[^.*0-9\n]", ".", [global, {return, binary}]),
    Ls = binary:split(Bin, <<"\n">>, [global, trim_all]),
    Lines = lists:zip(lists:seq(1, length(Ls)), [<<"..", L/binary, "..">> || L <- Ls]),
    C = star_pos(Lines),
    Muls = extract_matches(C),
    lists:sum(Muls).

star_pos(Ls) ->
    L = hd(Ls),
    star_pos(L, Ls, 0).

star_pos(false, _Ls, _Start) ->
    [];
star_pos({Ln, L}, Ls, Start) ->
    %% Check if there is a star in the line
    Match = binary:match(L, <<"*">>, [{scope, {Start, byte_size(L)-Start}}]),
    C = check_match(Ln, Ls, Match),
    case Match of
        nomatch ->
            %% no more matches this line, take next
            [C|star_pos(lists:keyfind(Ln+1, 1, Ls), Ls, 0)];
        {M, _} ->
            %% continue on this line, next match
            [C|star_pos({Ln, L}, Ls, M+1)]
    end.

check_match(_Ln, _Ls, nomatch) ->
    undefined;
check_match(Ln, Ls, {M, _}) ->
    P = check_line(Ln, -1, Ls, M),
    S = check_line(Ln, 0, Ls, M),
    N = check_line(Ln, +1, Ls, M),
    {P, S, N}.

check_line(Ln, Offset, Ls, M) ->
    case lists:keyfind(Ln+Offset, 1, Ls) of
        false ->
            %% No line found with offset
            false;
        {_, L} ->
            L1 = binary:at(L, M-1),
            L2 = binary:at(L, M),
            L3 = binary:at(L, M+1),
            valid_numbers(L, M, L1, L2, L3)
    end.

valid_numbers(Line, M, A, B, C) ->
    case {is_val_num(A), is_val_num(B), is_val_num(C)} of
        {true, false, true} ->
            {match, Cs} = re:run(Line, "\([0-9]+\)[^0-9]\([0-9]+\)", [{capture, all_but_first, list}, {offset, M-3}]),
            Cs;
        {true, false, false} ->
            {match, Cs} = re:run(Line, "\([0-9]+\).", [{capture, all_but_first, list}, {offset, M-3}]),
            Cs;
        {true, true, false} ->
            {match, Cs} = re:run(Line, "\([0-9]+\)", [{capture, all_but_first, list}, {offset, M-2}]),
            Cs;
        {true, true, true} ->
            {match, Cs} = re:run(Line, "\([0-9]+\)", [{capture, all_but_first, list}, {offset, M-1}]),
            Cs;
        {false, true, _} ->
            {match, Cs} = re:run(Line, "\([0-9]+\)", [{capture, all_but_first, list}, {offset, M}]),
            Cs;
        {false, false, true} ->
            {match, Cs} = re:run(Line, "\([0-9]+\)", [{capture, all_but_first, list}, {offset, M+1}]),
            Cs;
        {false, false, false} ->
            false
    end.

is_val_num(Byte) ->
    Byte >= $0 andalso Byte =< $9.

extract_matches([]) ->
    [];
extract_matches([undefined|C]) ->
    extract_matches(C);
extract_matches([T|C]) ->
    case [L || L <- tuple_to_list(T), L =/= false] of
        [[A], [B]] ->
            [list_to_integer(A)*list_to_integer(B)|extract_matches(C)];
        [[A, B]] ->
            [list_to_integer(A)*list_to_integer(B)|extract_matches(C)];
        _ ->
            extract_matches(C)
    end.
