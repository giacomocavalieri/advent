-module(advent_ffi).

-export([decode_error/1]).

decode_error({ErrorMap, _}) ->
    case ErrorMap of
        #{gleam_error := let_assert, message := Message} -> {crashed, Message};
        #{gleam_error := assert, message := Message} -> {crashed, Message};
        #{gleam_error := panic, message := Message} -> {crashed, Message};
        #{gleam_error := todo, message := Message} -> {todo, Message};
        _ -> {crashed, "unknown reason"}
    end;
decode_error(_) -> {crashed, "unknown reason"}.
