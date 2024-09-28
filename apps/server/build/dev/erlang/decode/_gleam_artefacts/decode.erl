-module(decode).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export([into/1, parameter/1, from/2, map/2, map_errors/2, then/2, list/1, dict/2, optional/1, collapse_errors/2, one_of/1, at/2, subfield/3, field/3, fail/1]).
-export_type([decoder/1]).

-opaque decoder(SMN) :: {decoder,
        fun((gleam@dynamic:dynamic_()) -> {ok, SMN} |
            {error, list(gleam@dynamic:decode_error())})}.

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/decode/src/decode.gleam", 339).
-spec into(SMS) -> decoder(SMS).
into(Constructor) ->
    {decoder, fun(_) -> {ok, Constructor} end}.

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/decode/src/decode.gleam", 365).
-spec parameter(fun((SMU) -> SMV)) -> fun((SMU) -> SMV).
parameter(Body) ->
    Body.

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/decode/src/decode.gleam", 464).
-spec from(decoder(SNJ), gleam@dynamic:dynamic_()) -> {ok, SNJ} |
    {error, list(gleam@dynamic:decode_error())}.
from(Decoder, Data) ->
    (erlang:element(2, Decoder))(Data).

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/decode/src/decode.gleam", 667).
-spec map(decoder(SON), fun((SON) -> SOP)) -> decoder(SOP).
map(Decoder, Transformer) ->
    {decoder, fun(D) -> case (erlang:element(2, Decoder))(D) of
                {ok, A} ->
                    {ok, Transformer(A)};

                {error, E} ->
                    {error, E}
            end end}.

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/decode/src/decode.gleam", 678).
-spec map_errors(
    decoder(SOR),
    fun((list(gleam@dynamic:decode_error())) -> list(gleam@dynamic:decode_error()))
) -> decoder(SOR).
map_errors(Decoder, Transformer) ->
    {decoder, fun(D) -> case (erlang:element(2, Decoder))(D) of
                {ok, A} ->
                    {ok, A};

                {error, E} ->
                    {error, Transformer(E)}
            end end}.

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/decode/src/decode.gleam", 719).
-spec then(decoder(SOZ), fun((SOZ) -> decoder(SPB))) -> decoder(SPB).
then(Decoder, Next) ->
    {decoder, fun(D) -> case (erlang:element(2, Decoder))(D) of
                {ok, A} ->
                    _pipe = Next(A),
                    from(_pipe, D);

                {error, E} ->
                    {error, E}
            end end}.

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/decode/src/decode.gleam", 553).
-spec list(decoder(SNM)) -> decoder(list(SNM)).
list(Item) ->
    {decoder, gleam@dynamic:list(erlang:element(2, Item))}.

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/decode/src/decode.gleam", 572).
-spec dict(decoder(SNQ), decoder(SNS)) -> decoder(gleam@dict:dict(SNQ, SNS)).
dict(Key, Value) ->
    {decoder,
        gleam@dynamic:dict(erlang:element(2, Key), erlang:element(2, Value))}.

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/decode/src/decode.gleam", 600).
-spec optional(decoder(SNX)) -> decoder(gleam@option:option(SNX)).
optional(Item) ->
    {decoder, gleam@dynamic:optional(erlang:element(2, Item))}.

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/decode/src/decode.gleam", 705).
-spec collapse_errors(decoder(SOW), binary()) -> decoder(SOW).
collapse_errors(Decoder, Name) ->
    {decoder, fun(D) -> case (erlang:element(2, Decoder))(D) of
                {ok, A} ->
                    {ok, A};

                {error, _} ->
                    {error,
                        [{decode_error, Name, gleam@dynamic:classify(D), []}]}
            end end}.

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/decode/src/decode.gleam", 750).
-spec run_decoders(gleam@dynamic:dynamic_(), list(decoder(SPI))) -> {ok, SPI} |
    {error, list(gleam@dynamic:decode_error())}.
run_decoders(Data, Decoders) ->
    case Decoders of
        [] ->
            {error,
                [{decode_error,
                        <<"nothing"/utf8>>,
                        gleam@dynamic:classify(Data),
                        []}]};

        [Decoder] ->
            from(Decoder, Data);

        [Decoder@1 | Decoders@1] ->
            case from(Decoder@1, Data) of
                {ok, Value} ->
                    {ok, Value};

                {error, _} ->
                    run_decoders(Data, Decoders@1)
            end
    end.

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/decode/src/decode.gleam", 746).
-spec one_of(list(decoder(SPE))) -> decoder(SPE).
one_of(Decoders) ->
    {decoder, fun(D) -> run_decoders(D, Decoders) end}.

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/decode/src/decode.gleam", 764).
-spec push_path(list(gleam@dynamic:decode_error()), any()) -> list(gleam@dynamic:decode_error()).
push_path(Errors, Key) ->
    Key@1 = gleam@dynamic:from(Key),
    Decoder = gleam@dynamic:any(
        [fun gleam@dynamic:string/1,
            fun(X) ->
                gleam@result:map(
                    gleam@dynamic:int(X),
                    fun gleam@int:to_string/1
                )
            end]
    ),
    Key@3 = case Decoder(Key@1) of
        {ok, Key@2} ->
            Key@2;

        {error, _} ->
            <<<<"<"/utf8, (gleam@dynamic:classify(Key@1))/binary>>/binary,
                ">"/utf8>>
    end,
    gleam@list:map(
        Errors,
        fun(Error) ->
            erlang:setelement(4, Error, [Key@3 | erlang:element(4, Error)])
        end
    ).

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/decode/src/decode.gleam", 641).
-spec index(
    any(),
    fun((gleam@dynamic:dynamic_()) -> {ok, SOH} |
        {error, list(gleam@dynamic:decode_error())}),
    gleam@dynamic:dynamic_()
) -> {ok, SOH} | {error, list(gleam@dynamic:decode_error())}.
index(Key, Inner, Data) ->
    case decode_ffi:index(Data, Key) of
        {ok, Data@1} ->
            case Inner(Data@1) of
                {ok, Data@2} ->
                    {ok, Data@2};

                {error, Errors} ->
                    {error, push_path(Errors, Key)}
            end;

        {error, Kind} ->
            {error, [{decode_error, Kind, gleam@dynamic:classify(Data), []}]}
    end.

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/decode/src/decode.gleam", 630).
-spec at(list(any()), decoder(SOD)) -> decoder(SOD).
at(Path, Inner) ->
    {decoder,
        fun(Data) ->
            Decoder = gleam@list:fold_right(
                Path,
                erlang:element(2, Inner),
                fun(Dyn_decoder, Segment) ->
                    fun(_capture) -> index(Segment, Dyn_decoder, _capture) end
                end
            ),
            Decoder(Data)
        end}.

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/decode/src/decode.gleam", 432).
-spec subfield(decoder(fun((SNC) -> SND)), list(any()), decoder(SNC)) -> decoder(SND).
subfield(Decoder, Field_path, Field_decoder) ->
    {decoder,
        fun(Data) ->
            Constructor = (erlang:element(2, Decoder))(Data),
            Data@1 = from(at(Field_path, Field_decoder), Data),
            case {Constructor, Data@1} of
                {{ok, Constructor@1}, {ok, Data@2}} ->
                    {ok, Constructor@1(Data@2)};

                {{error, E1}, {error, E2}} ->
                    {error, lists:append(E1, E2)};

                {_, {error, Errors}} ->
                    {error, Errors};

                {{error, Errors}, _} ->
                    {error, Errors}
            end
        end}.

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/decode/src/decode.gleam", 400).
-spec field(decoder(fun((SMW) -> SMX)), any(), decoder(SMW)) -> decoder(SMX).
field(Decoder, Field_name, Field_decoder) ->
    subfield(Decoder, [Field_name], Field_decoder).

-file("/home/raphaelac/repositories/lauraxv/server/build/packages/decode/src/decode.gleam", 781).
-spec fail(binary()) -> decoder(any()).
fail(Expected) ->
    {decoder,
        fun(D) ->
            {error, [{decode_error, Expected, gleam@dynamic:classify(D), []}]}
        end}.
