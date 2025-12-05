-module(advent_ffi).

-export([decode_error/1, error_message/1]).

decode_error({ErrorMap, _}) ->
    case ErrorMap of
        #{gleam_error := let_assert, message := Message} -> {crashed, Message};
        #{gleam_error := assert, message := Message} -> {crashed, Message};
        #{gleam_error := panic, message := Message} -> {crashed, Message};
        #{gleam_error := todo, message := Message} -> {todo, Message};
        _ -> {crashed, "unknown reason"}
    end;
decode_error(_) -> {crashed, "unknown reason"}.

error_message({ErrorMap, _}) -> error_message(ErrorMap);
error_message(ErrorMap) ->
    case ErrorMap of
        #{gleam_error := let_assert, message := Message} -> {ok, Message};
        #{gleam_error := assert, message := Message} -> {ok, Message};
        #{gleam_error := panic, message := Message} -> {ok, Message};
        #{gleam_error := todo, message := Message} -> {ok, Message};
        _ -> {error, nil}
    end;
error_message(_) -> {error, nil}.
