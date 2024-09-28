-module(rada@date).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export([from_rata_die/1, to_rata_die/1, compare/2, min/2, max/2, clamp/3, month_to_number/1, number_to_month/1, number_to_weekday/1, weekday_number/1, from_ordinal_date/2, from_week_date/3, weekday/1, with_ordinal_suffix/1, from_calendar_date/3, today/0, year/1, ordinal_day/1, month/1, month_number/1, quarter/1, day/1, week_year/1, week_number/1, format_with_language/3, format/2, to_iso_string/1, add/3, diff/3, floor/2, ceiling/2, range/4, from_iso_string/1, is_between/3]).
-export_type([month/0, weekday/0, date/0, ordinal_date/0, calendar_date/0, week_date/0, language/0, day_of_year/0, unit/0, interval/0]).

-type month() :: jan |
    feb |
    mar |
    apr |
    may |
    jun |
    jul |
    aug |
    sep |
    oct |
    nov |
    dec.

-type weekday() :: mon | tue | wed | thu | fri | sat | sun.

-opaque date() :: {rd, integer()}.

-type ordinal_date() :: {ordinal_date, integer(), integer()}.

-type calendar_date() :: {calendar_date, integer(), month(), integer()}.

-type week_date() :: {week_date, integer(), integer(), weekday()}.

-type language() :: {language,
        fun((month()) -> binary()),
        fun((month()) -> binary()),
        fun((weekday()) -> binary()),
        fun((weekday()) -> binary()),
        fun((integer()) -> binary())}.

-type day_of_year() :: {month_and_day, integer(), integer()} |
    {week_and_weekday, integer(), integer()} |
    {ordinal_day, integer()}.

-type unit() :: years | months | weeks | days.

-type interval() :: year |
    quarter |
    month |
    week |
    monday |
    tuesday |
    wednesday |
    thursday |
    friday |
    saturday |
    sunday |
    day.

-spec from_rata_die(integer()) -> date().
from_rata_die(Rd) ->
    {rd, Rd}.

-spec to_rata_die(date()) -> integer().
to_rata_die(Date) ->
    {rd, Rd} = Date,
    Rd.

-spec string_take_right(binary(), integer()) -> binary().
string_take_right(Str, Count) ->
    gleam@string:slice(Str, -1 * Count, Count).

-spec string_take_left(binary(), integer()) -> binary().
string_take_left(Str, Count) ->
    gleam@string:slice(Str, 0, Count).

-spec month_to_name(month()) -> binary().
month_to_name(Month) ->
    case Month of
        jan ->
            <<"January"/utf8>>;

        feb ->
            <<"February"/utf8>>;

        mar ->
            <<"March"/utf8>>;

        apr ->
            <<"April"/utf8>>;

        may ->
            <<"May"/utf8>>;

        jun ->
            <<"June"/utf8>>;

        jul ->
            <<"July"/utf8>>;

        aug ->
            <<"August"/utf8>>;

        sep ->
            <<"September"/utf8>>;

        oct ->
            <<"October"/utf8>>;

        nov ->
            <<"November"/utf8>>;

        dec ->
            <<"December"/utf8>>
    end.

-spec weekday_to_name(weekday()) -> binary().
weekday_to_name(Weekday) ->
    case Weekday of
        mon ->
            <<"Monday"/utf8>>;

        tue ->
            <<"Tuesday"/utf8>>;

        wed ->
            <<"Wednesday"/utf8>>;

        thu ->
            <<"Thursday"/utf8>>;

        fri ->
            <<"Friday"/utf8>>;

        sat ->
            <<"Saturday"/utf8>>;

        sun ->
            <<"Sunday"/utf8>>
    end.

-spec parse_digit() -> nibble:parser(rada@date@parse:parse_date_token(), rada@date@parse:parse_date_token(), any()).
parse_digit() ->
    nibble:take_if(<<"Expecting digit"/utf8>>, fun(Token) -> case Token of
                {digit, _} ->
                    true;

                _ ->
                    false
            end end).

-spec int_4() -> nibble:parser(integer(), rada@date@parse:parse_date_token(), any()).
int_4() ->
    nibble:do(
        nibble:optional(nibble:token(dash)),
        fun(Negative) ->
            Negative@1 = begin
                _pipe = Negative,
                _pipe@1 = gleam@option:map(_pipe, fun(_) -> <<"-"/utf8>> end),
                gleam@option:unwrap(_pipe@1, <<""/utf8>>)
            end,
            nibble:do(
                begin
                    _pipe@2 = parse_digit(),
                    nibble:take_exactly(_pipe@2, 4)
                end,
                fun(Tokens) ->
                    Str@1 = begin
                        _pipe@3 = gleam@list:map(
                            Tokens,
                            fun(Token) ->
                                {digit, Str} = case Token of
                                    {digit, _} -> Token;
                                    _assert_fail ->
                                        erlang:error(
                                                #{gleam_error => let_assert,
                                                    message => <<"Assertion pattern match failed"/utf8>>,
                                                    value => _assert_fail,
                                                    module => <<"rada/date"/utf8>>,
                                                    function => <<"int_4"/utf8>>,
                                                    line => 1091}
                                            )
                                end,
                                Str
                            end
                        ),
                        gleam@string:concat(_pipe@3)
                    end,
                    _assert_subject = gleam@int:parse(
                        <<Negative@1/binary, Str@1/binary>>
                    ),
                    {ok, Int} = case _assert_subject of
                        {ok, _} -> _assert_subject;
                        _assert_fail@1 ->
                            erlang:error(#{gleam_error => let_assert,
                                        message => <<"Assertion pattern match failed"/utf8>>,
                                        value => _assert_fail@1,
                                        module => <<"rada/date"/utf8>>,
                                        function => <<"int_4"/utf8>>,
                                        line => 1096})
                    end,
                    nibble:return(Int)
                end
            )
        end
    ).

-spec int_3() -> nibble:parser(integer(), rada@date@parse:parse_date_token(), any()).
int_3() ->
    nibble:do(
        begin
            _pipe = parse_digit(),
            nibble:take_exactly(_pipe, 3)
        end,
        fun(Tokens) ->
            Str@1 = begin
                _pipe@1 = gleam@list:map(
                    Tokens,
                    fun(Token) ->
                        {digit, Str} = case Token of
                            {digit, _} -> Token;
                            _assert_fail ->
                                erlang:error(#{gleam_error => let_assert,
                                            message => <<"Assertion pattern match failed"/utf8>>,
                                            value => _assert_fail,
                                            module => <<"rada/date"/utf8>>,
                                            function => <<"int_3"/utf8>>,
                                            line => 1109})
                        end,
                        Str
                    end
                ),
                gleam@string:concat(_pipe@1)
            end,
            _assert_subject = gleam@int:parse(Str@1),
            {ok, Int} = case _assert_subject of
                {ok, _} -> _assert_subject;
                _assert_fail@1 ->
                    erlang:error(#{gleam_error => let_assert,
                                message => <<"Assertion pattern match failed"/utf8>>,
                                value => _assert_fail@1,
                                module => <<"rada/date"/utf8>>,
                                function => <<"int_3"/utf8>>,
                                line => 1114})
            end,
            nibble:return(Int)
        end
    ).

-spec parse_ordinal_day() -> nibble:parser(day_of_year(), rada@date@parse:parse_date_token(), any()).
parse_ordinal_day() ->
    nibble:do(int_3(), fun(Day) -> nibble:return({ordinal_day, Day}) end).

-spec int_2() -> nibble:parser(integer(), rada@date@parse:parse_date_token(), any()).
int_2() ->
    nibble:do(
        begin
            _pipe = parse_digit(),
            nibble:take_exactly(_pipe, 2)
        end,
        fun(Tokens) ->
            Str@1 = begin
                _pipe@1 = gleam@list:map(
                    Tokens,
                    fun(Token) ->
                        {digit, Str} = case Token of
                            {digit, _} -> Token;
                            _assert_fail ->
                                erlang:error(#{gleam_error => let_assert,
                                            message => <<"Assertion pattern match failed"/utf8>>,
                                            value => _assert_fail,
                                            module => <<"rada/date"/utf8>>,
                                            function => <<"int_2"/utf8>>,
                                            line => 1127})
                        end,
                        Str
                    end
                ),
                gleam@string:concat(_pipe@1)
            end,
            _assert_subject = gleam@int:parse(Str@1),
            {ok, Int} = case _assert_subject of
                {ok, _} -> _assert_subject;
                _assert_fail@1 ->
                    erlang:error(#{gleam_error => let_assert,
                                message => <<"Assertion pattern match failed"/utf8>>,
                                value => _assert_fail@1,
                                module => <<"rada/date"/utf8>>,
                                function => <<"int_2"/utf8>>,
                                line => 1132})
            end,
            nibble:return(Int)
        end
    ).

-spec parse_month_and_day(boolean()) -> nibble:parser(day_of_year(), rada@date@parse:parse_date_token(), any()).
parse_month_and_day(Extended) ->
    nibble:do(
        int_2(),
        fun(Month) ->
            Dash_count = gleam@bool:to_int(Extended),
            nibble:do(
                nibble:one_of(
                    [begin
                            _pipe = nibble:take_exactly(
                                nibble:token(dash),
                                Dash_count
                            ),
                            nibble:then(_pipe, fun(_) -> int_2() end)
                        end,
                        begin
                            _pipe@1 = nibble:eof(),
                            nibble:then(
                                _pipe@1,
                                fun(_) -> nibble:succeed(1) end
                            )
                        end]
                ),
                fun(Day) -> nibble:return({month_and_day, Month, Day}) end
            )
        end
    ).

-spec int_1() -> nibble:parser(integer(), rada@date@parse:parse_date_token(), any()).
int_1() ->
    nibble:do(
        begin
            _pipe = parse_digit(),
            nibble:take_exactly(_pipe, 1)
        end,
        fun(Tokens) ->
            [{digit, Str}] = case Tokens of
                [{digit, _}] -> Tokens;
                _assert_fail ->
                    erlang:error(#{gleam_error => let_assert,
                                message => <<"Assertion pattern match failed"/utf8>>,
                                value => _assert_fail,
                                module => <<"rada/date"/utf8>>,
                                function => <<"int_1"/utf8>>,
                                line => 1143})
            end,
            _assert_subject = gleam@int:parse(Str),
            {ok, Int} = case _assert_subject of
                {ok, _} -> _assert_subject;
                _assert_fail@1 ->
                    erlang:error(#{gleam_error => let_assert,
                                message => <<"Assertion pattern match failed"/utf8>>,
                                value => _assert_fail@1,
                                module => <<"rada/date"/utf8>>,
                                function => <<"int_1"/utf8>>,
                                line => 1145})
            end,
            nibble:return(Int)
        end
    ).

-spec parse_week_and_weekday(boolean()) -> nibble:parser(day_of_year(), rada@date@parse:parse_date_token(), any()).
parse_week_and_weekday(Extended) ->
    nibble:do(
        nibble:token(week_token),
        fun(_) ->
            nibble:do(
                int_2(),
                fun(Week) ->
                    Dash_count = gleam@bool:to_int(Extended),
                    nibble:do(
                        nibble:one_of(
                            [begin
                                    _pipe = nibble:take_exactly(
                                        nibble:token(dash),
                                        Dash_count
                                    ),
                                    nibble:then(_pipe, fun(_) -> int_1() end)
                                end,
                                nibble:succeed(1)]
                        ),
                        fun(Day) ->
                            nibble:return({week_and_weekday, Week, Day})
                        end
                    )
                end
            )
        end
    ).

-spec parse_day_of_year() -> nibble:parser(day_of_year(), rada@date@parse:parse_date_token(), any()).
parse_day_of_year() ->
    nibble:one_of(
        [begin
                _pipe = nibble:token(dash),
                nibble:then(
                    _pipe,
                    fun(_) ->
                        nibble:one_of(
                            [nibble:backtrackable(parse_ordinal_day()),
                                parse_month_and_day(true),
                                parse_week_and_weekday(true)]
                        )
                    end
                )
            end,
            nibble:backtrackable(parse_month_and_day(false)),
            parse_ordinal_day(),
            parse_week_and_weekday(false),
            nibble:succeed({ordinal_day, 1})]
    ).

-spec interval_to_units(interval()) -> {integer(), unit()}.
interval_to_units(Interval) ->
    case Interval of
        year ->
            {1, years};

        quarter ->
            {3, months};

        month ->
            {1, months};

        day ->
            {1, days};

        _ ->
            {1, weeks}
    end.

-spec compare(date(), date()) -> gleam@order:order().
compare(Date1, Date2) ->
    {rd, Rd_1} = Date1,
    {rd, Rd_2} = Date2,
    gleam@int:compare(Rd_1, Rd_2).

-spec min(date(), date()) -> date().
min(Date1, Date2) ->
    {rd, Rd_1} = Date1,
    {rd, Rd_2} = Date2,
    case Rd_1 < Rd_2 of
        true ->
            Date1;

        false ->
            Date2
    end.

-spec max(date(), date()) -> date().
max(Date1, Date2) ->
    {rd, Rd_1} = Date1,
    {rd, Rd_2} = Date2,
    case Rd_1 < Rd_2 of
        true ->
            Date2;

        false ->
            Date1
    end.

-spec clamp(date(), date(), date()) -> date().
clamp(Value, Lower, Upper) ->
    {rd, Value_rd} = Value,
    {rd, Lower_rd} = Lower,
    {rd, Upper_rd} = Upper,
    case Value_rd < Lower_rd of
        true ->
            Lower;

        false ->
            case Value_rd > Upper_rd of
                true ->
                    Upper;

                false ->
                    Value
            end
    end.

-spec month_to_number(month()) -> integer().
month_to_number(Month) ->
    case Month of
        jan ->
            1;

        feb ->
            2;

        mar ->
            3;

        apr ->
            4;

        may ->
            5;

        jun ->
            6;

        jul ->
            7;

        aug ->
            8;

        sep ->
            9;

        oct ->
            10;

        nov ->
            11;

        dec ->
            12
    end.

-spec month_to_quarter(month()) -> integer().
month_to_quarter(Month) ->
    (month_to_number(Month) + 2) div 3.

-spec number_to_month(integer()) -> month().
number_to_month(Month_number) ->
    case gleam@int:max(1, Month_number) of
        1 ->
            jan;

        2 ->
            feb;

        3 ->
            mar;

        4 ->
            apr;

        5 ->
            may;

        6 ->
            jun;

        7 ->
            jul;

        8 ->
            aug;

        9 ->
            sep;

        10 ->
            oct;

        11 ->
            nov;

        _ ->
            dec
    end.

-spec quarter_to_month(integer()) -> month().
quarter_to_month(Quarter) ->
    _pipe = (Quarter * 3) - 2,
    number_to_month(_pipe).

-spec weekday_to_number(weekday()) -> integer().
weekday_to_number(Weekday) ->
    case Weekday of
        mon ->
            1;

        tue ->
            2;

        wed ->
            3;

        thu ->
            4;

        fri ->
            5;

        sat ->
            6;

        sun ->
            7
    end.

-spec number_to_weekday(integer()) -> weekday().
number_to_weekday(Weekday_number) ->
    case gleam@int:max(1, Weekday_number) of
        1 ->
            mon;

        2 ->
            tue;

        3 ->
            wed;

        4 ->
            thu;

        5 ->
            fri;

        6 ->
            sat;

        _ ->
            sun
    end.

-spec pad_signed_int(integer(), integer()) -> binary().
pad_signed_int(Value, Length) ->
    Prefix = case Value < 0 of
        true ->
            <<"-"/utf8>>;

        false ->
            <<""/utf8>>
    end,
    Suffix = begin
        _pipe = Value,
        _pipe@1 = gleam@int:absolute_value(_pipe),
        _pipe@2 = gleam@int:to_string(_pipe@1),
        gleam@string:pad_left(_pipe@2, Length, <<"0"/utf8>>)
    end,
    <<Prefix/binary, Suffix/binary>>.

-spec floor_div(integer(), integer()) -> integer().
floor_div(Dividend, Divisor) ->
    case (((Dividend > 0) andalso (Divisor < 0)) orelse ((Dividend < 0) andalso (Divisor
    > 0)))
    andalso ((case Divisor of
        0 -> 0;
        Gleam@denominator -> Dividend rem Gleam@denominator
    end) /= 0) of
        true ->
            (case Divisor of
                0 -> 0;
                Gleam@denominator@1 -> Dividend div Gleam@denominator@1
            end) - 1;

        false ->
            case Divisor of
                0 -> 0;
                Gleam@denominator@2 -> Dividend div Gleam@denominator@2
            end
    end.

-spec days_before_year(integer()) -> integer().
days_before_year(Year1) ->
    Year = Year1 - 1,
    Leap_years = (floor_div(Year, 4) - floor_div(Year, 100)) + floor_div(
        Year,
        400
    ),
    (365 * Year) + Leap_years.

-spec first_of_year(integer()) -> date().
first_of_year(Year) ->
    {rd, days_before_year(Year) + 1}.

-spec modulo_unwrap(integer(), integer()) -> integer().
modulo_unwrap(Dividend, Divisor) ->
    Remainder = case Divisor of
        0 -> 0;
        Gleam@denominator -> Dividend rem Gleam@denominator
    end,
    case ((Remainder > 0) andalso (Divisor < 0)) orelse ((Remainder < 0) andalso (Divisor
    > 0)) of
        true ->
            Remainder + Divisor;

        false ->
            Remainder
    end.

-spec is_leap_year(integer()) -> boolean().
is_leap_year(Year) ->
    ((modulo_unwrap(Year, 4) =:= 0) andalso (modulo_unwrap(Year, 100) /= 0))
    orelse (modulo_unwrap(Year, 400) =:= 0).

-spec weekday_number(date()) -> integer().
weekday_number(Date) ->
    {rd, Rd} = Date,
    case modulo_unwrap(Rd, 7) of
        0 ->
            7;

        N ->
            N
    end.

-spec days_before_week_year(integer()) -> integer().
days_before_week_year(Year) ->
    Jan4 = days_before_year(Year) + 4,
    Jan4 - weekday_number({rd, Jan4}).

-spec is_53_week_year(integer()) -> boolean().
is_53_week_year(Year) ->
    Wdn_jan1 = weekday_number(first_of_year(Year)),
    (Wdn_jan1 =:= 4) orelse ((Wdn_jan1 =:= 3) andalso is_leap_year(Year)).

-spec from_ordinal_date(integer(), integer()) -> date().
from_ordinal_date(Year, Ordinal) ->
    Days_in_year = case is_leap_year(Year) of
        true ->
            366;

        false ->
            365
    end,
    {rd, days_before_year(Year) + gleam@int:clamp(Ordinal, 1, Days_in_year)}.

-spec from_week_date(integer(), integer(), weekday()) -> date().
from_week_date(Week_year, Week_number, Weekday) ->
    Weeks_in_year = case is_53_week_year(Week_year) of
        true ->
            53;

        false ->
            52
    end,
    {rd,
        (days_before_week_year(Week_year) + ((gleam@int:clamp(
            Week_number,
            1,
            Weeks_in_year
        )
        - 1)
        * 7))
        + weekday_to_number(Weekday)}.

-spec weekday(date()) -> weekday().
weekday(Date) ->
    _pipe = Date,
    _pipe@1 = weekday_number(_pipe),
    number_to_weekday(_pipe@1).

-spec ordinal_suffix(integer()) -> binary().
ordinal_suffix(Value) ->
    Value_mod_100 = modulo_unwrap(Value, 100),
    Value@1 = case Value_mod_100 < 20 of
        true ->
            Value_mod_100;

        false ->
            modulo_unwrap(Value_mod_100, 10)
    end,
    case gleam@int:min(Value@1, 4) of
        1 ->
            <<"st"/utf8>>;

        2 ->
            <<"nd"/utf8>>;

        3 ->
            <<"rd"/utf8>>;

        _ ->
            <<"th"/utf8>>
    end.

-spec with_ordinal_suffix(integer()) -> binary().
with_ordinal_suffix(Value) ->
    <<(gleam@int:to_string(Value))/binary, (ordinal_suffix(Value))/binary>>.

-spec language_en() -> language().
language_en() ->
    {language, fun month_to_name/1, fun(Val) -> _pipe = Val,
            _pipe@1 = month_to_name(_pipe),
            string_take_left(_pipe@1, 3) end, fun weekday_to_name/1, fun(Val@1) ->
            _pipe@2 = Val@1,
            _pipe@3 = weekday_to_name(_pipe@2),
            string_take_left(_pipe@3, 3)
        end, fun with_ordinal_suffix/1}.

-spec days_since_previous_weekday(weekday(), date()) -> integer().
days_since_previous_weekday(Weekday, Date) ->
    modulo_unwrap((weekday_number(Date) + 7) - weekday_to_number(Weekday), 7).

-spec days_in_month(integer(), month()) -> integer().
days_in_month(Year, Month) ->
    case Month of
        jan ->
            31;

        feb ->
            case is_leap_year(Year) of
                true ->
                    29;

                false ->
                    28
            end;

        mar ->
            31;

        apr ->
            30;

        may ->
            31;

        jun ->
            30;

        jul ->
            31;

        aug ->
            31;

        sep ->
            30;

        oct ->
            31;

        nov ->
            30;

        dec ->
            31
    end.

-spec to_calendar_date_helper(integer(), month(), integer()) -> calendar_date().
to_calendar_date_helper(Year, Month, Ordinal_day) ->
    Month_days = days_in_month(Year, Month),
    Month_number = month_to_number(Month),
    case (Month_number < 12) andalso (Ordinal_day > Month_days) of
        true ->
            to_calendar_date_helper(
                Year,
                number_to_month(Month_number + 1),
                Ordinal_day - Month_days
            );

        false ->
            {calendar_date, Year, Month, Ordinal_day}
    end.

-spec days_before_month(integer(), month()) -> integer().
days_before_month(Year, Month) ->
    Leap_days = gleam@bool:to_int(is_leap_year(Year)),
    case Month of
        jan ->
            0;

        feb ->
            31;

        mar ->
            59 + Leap_days;

        apr ->
            90 + Leap_days;

        may ->
            120 + Leap_days;

        jun ->
            151 + Leap_days;

        jul ->
            181 + Leap_days;

        aug ->
            212 + Leap_days;

        sep ->
            243 + Leap_days;

        oct ->
            273 + Leap_days;

        nov ->
            304 + Leap_days;

        dec ->
            334 + Leap_days
    end.

-spec first_of_month(integer(), month()) -> date().
first_of_month(Year, Month) ->
    {rd, (days_before_year(Year) + days_before_month(Year, Month)) + 1}.

-spec from_calendar_date(integer(), month(), integer()) -> date().
from_calendar_date(Year, Month, Day) ->
    {rd,
        (days_before_year(Year) + days_before_month(Year, Month)) + gleam@int:clamp(
            Day,
            1,
            days_in_month(Year, Month)
        )}.

-spec today() -> date().
today() ->
    {Year, Month_number, Day} = rada_ffi:get_year_month_day(),
    from_calendar_date(Year, number_to_month(Month_number), Day).

-spec div_with_remainder(integer(), integer()) -> {integer(), integer()}.
div_with_remainder(A, B) ->
    {floor_div(A, B), modulo_unwrap(A, B)}.

-spec year(date()) -> integer().
year(Date) ->
    {rd, Rd} = Date,
    {N400, R400} = div_with_remainder(Rd, 146097),
    {N100, R100} = div_with_remainder(R400, 36524),
    {N4, R4} = div_with_remainder(R100, 1461),
    {N1, R1} = div_with_remainder(R4, 365),
    N = case R1 =:= 0 of
        true ->
            0;

        false ->
            1
    end,
    ((((N400 * 400) + (N100 * 100)) + (N4 * 4)) + N1) + N.

-spec to_ordinal_date(date()) -> ordinal_date().
to_ordinal_date(Date) ->
    {rd, Rd} = Date,
    Year_ = year(Date),
    {ordinal_date, Year_, Rd - days_before_year(Year_)}.

-spec to_calendar_date(date()) -> calendar_date().
to_calendar_date(Date) ->
    Ordinal_date = to_ordinal_date(Date),
    to_calendar_date_helper(
        erlang:element(2, Ordinal_date),
        jan,
        erlang:element(3, Ordinal_date)
    ).

-spec to_week_date(date()) -> week_date().
to_week_date(Date) ->
    {rd, Rd} = Date,
    Weekday_number_ = weekday_number(Date),
    Week_year = year({rd, Rd + (4 - Weekday_number_)}),
    Week_1_day_1 = days_before_week_year(Week_year) + 1,
    {week_date,
        Week_year,
        1 + ((Rd - Week_1_day_1) div 7),
        number_to_weekday(Weekday_number_)}.

-spec ordinal_day(date()) -> integer().
ordinal_day(Date) ->
    erlang:element(3, to_ordinal_date(Date)).

-spec month(date()) -> month().
month(Date) ->
    erlang:element(3, to_calendar_date(Date)).

-spec month_number(date()) -> integer().
month_number(Date) ->
    _pipe = Date,
    _pipe@1 = month(_pipe),
    month_to_number(_pipe@1).

-spec quarter(date()) -> integer().
quarter(Date) ->
    _pipe = Date,
    _pipe@1 = month(_pipe),
    month_to_quarter(_pipe@1).

-spec day(date()) -> integer().
day(Date) ->
    erlang:element(4, to_calendar_date(Date)).

-spec week_year(date()) -> integer().
week_year(Date) ->
    erlang:element(2, to_week_date(Date)).

-spec week_number(date()) -> integer().
week_number(Date) ->
    erlang:element(3, to_week_date(Date)).

-spec format_field(date(), language(), binary(), integer()) -> binary().
format_field(Date, Language, Char, Length) ->
    case Char of
        <<"y"/utf8>> ->
            case Length of
                2 ->
                    _pipe = Date,
                    _pipe@1 = year(_pipe),
                    _pipe@2 = gleam@int:to_string(_pipe@1),
                    _pipe@3 = gleam@string:pad_left(_pipe@2, 2, <<"0"/utf8>>),
                    string_take_right(_pipe@3, 2);

                _ ->
                    _pipe@4 = Date,
                    _pipe@5 = year(_pipe@4),
                    pad_signed_int(_pipe@5, Length)
            end;

        <<"Y"/utf8>> ->
            case Length of
                2 ->
                    _pipe@6 = Date,
                    _pipe@7 = week_year(_pipe@6),
                    _pipe@8 = gleam@int:to_string(_pipe@7),
                    _pipe@9 = gleam@string:pad_left(_pipe@8, 2, <<"0"/utf8>>),
                    string_take_right(_pipe@9, 2);

                _ ->
                    _pipe@10 = Date,
                    _pipe@11 = week_year(_pipe@10),
                    pad_signed_int(_pipe@11, Length)
            end;

        <<"Q"/utf8>> ->
            case Length of
                1 ->
                    _pipe@12 = Date,
                    _pipe@13 = quarter(_pipe@12),
                    gleam@int:to_string(_pipe@13);

                2 ->
                    _pipe@14 = Date,
                    _pipe@15 = quarter(_pipe@14),
                    gleam@int:to_string(_pipe@15);

                3 ->
                    _pipe@16 = Date,
                    _pipe@17 = quarter(_pipe@16),
                    _pipe@18 = gleam@int:to_string(_pipe@17),
                    (fun(Str) -> <<"Q"/utf8, Str/binary>> end)(_pipe@18);

                4 ->
                    _pipe@19 = Date,
                    _pipe@20 = quarter(_pipe@19),
                    with_ordinal_suffix(_pipe@20);

                5 ->
                    _pipe@21 = Date,
                    _pipe@22 = quarter(_pipe@21),
                    gleam@int:to_string(_pipe@22);

                _ ->
                    <<""/utf8>>
            end;

        <<"M"/utf8>> ->
            case Length of
                1 ->
                    _pipe@23 = Date,
                    _pipe@24 = month_number(_pipe@23),
                    gleam@int:to_string(_pipe@24);

                2 ->
                    _pipe@25 = Date,
                    _pipe@26 = month_number(_pipe@25),
                    _pipe@27 = gleam@int:to_string(_pipe@26),
                    gleam@string:pad_left(_pipe@27, 2, <<"0"/utf8>>);

                3 ->
                    _pipe@28 = Date,
                    _pipe@29 = month(_pipe@28),
                    (erlang:element(3, Language))(_pipe@29);

                4 ->
                    _pipe@30 = Date,
                    _pipe@31 = month(_pipe@30),
                    (erlang:element(2, Language))(_pipe@31);

                5 ->
                    _pipe@32 = Date,
                    _pipe@33 = month(_pipe@32),
                    _pipe@34 = (erlang:element(3, Language))(_pipe@33),
                    string_take_left(_pipe@34, 1);

                _ ->
                    <<""/utf8>>
            end;

        <<"w"/utf8>> ->
            case Length of
                1 ->
                    _pipe@35 = Date,
                    _pipe@36 = week_number(_pipe@35),
                    gleam@int:to_string(_pipe@36);

                2 ->
                    _pipe@37 = Date,
                    _pipe@38 = week_number(_pipe@37),
                    _pipe@39 = gleam@int:to_string(_pipe@38),
                    gleam@string:pad_left(_pipe@39, 2, <<"0"/utf8>>);

                _ ->
                    <<""/utf8>>
            end;

        <<"d"/utf8>> ->
            case Length of
                1 ->
                    _pipe@40 = Date,
                    _pipe@41 = day(_pipe@40),
                    gleam@int:to_string(_pipe@41);

                2 ->
                    _pipe@42 = Date,
                    _pipe@43 = day(_pipe@42),
                    _pipe@44 = gleam@int:to_string(_pipe@43),
                    gleam@string:pad_left(_pipe@44, 2, <<"0"/utf8>>);

                3 ->
                    _pipe@45 = Date,
                    _pipe@46 = day(_pipe@45),
                    (erlang:element(6, Language))(_pipe@46);

                _ ->
                    <<""/utf8>>
            end;

        <<"D"/utf8>> ->
            case Length of
                1 ->
                    _pipe@47 = Date,
                    _pipe@48 = ordinal_day(_pipe@47),
                    gleam@int:to_string(_pipe@48);

                2 ->
                    _pipe@49 = Date,
                    _pipe@50 = ordinal_day(_pipe@49),
                    _pipe@51 = gleam@int:to_string(_pipe@50),
                    gleam@string:pad_left(_pipe@51, 2, <<"0"/utf8>>);

                3 ->
                    _pipe@52 = Date,
                    _pipe@53 = ordinal_day(_pipe@52),
                    _pipe@54 = gleam@int:to_string(_pipe@53),
                    gleam@string:pad_left(_pipe@54, 3, <<"0"/utf8>>);

                _ ->
                    <<""/utf8>>
            end;

        <<"E"/utf8>> ->
            case Length of
                1 ->
                    _pipe@55 = Date,
                    _pipe@56 = weekday(_pipe@55),
                    (erlang:element(5, Language))(_pipe@56);

                2 ->
                    _pipe@57 = Date,
                    _pipe@58 = weekday(_pipe@57),
                    (erlang:element(5, Language))(_pipe@58);

                3 ->
                    _pipe@59 = Date,
                    _pipe@60 = weekday(_pipe@59),
                    (erlang:element(5, Language))(_pipe@60);

                4 ->
                    _pipe@61 = Date,
                    _pipe@62 = weekday(_pipe@61),
                    (erlang:element(4, Language))(_pipe@62);

                5 ->
                    _pipe@63 = Date,
                    _pipe@64 = weekday(_pipe@63),
                    _pipe@65 = (erlang:element(5, Language))(_pipe@64),
                    string_take_left(_pipe@65, 1);

                6 ->
                    _pipe@66 = Date,
                    _pipe@67 = weekday(_pipe@66),
                    _pipe@68 = (erlang:element(5, Language))(_pipe@67),
                    string_take_left(_pipe@68, 2);

                _ ->
                    <<""/utf8>>
            end;

        <<"e"/utf8>> ->
            case Length of
                1 ->
                    _pipe@69 = Date,
                    _pipe@70 = weekday_number(_pipe@69),
                    gleam@int:to_string(_pipe@70);

                2 ->
                    _pipe@71 = Date,
                    _pipe@72 = weekday_number(_pipe@71),
                    gleam@int:to_string(_pipe@72);

                _ ->
                    _pipe@73 = Date,
                    format_field(_pipe@73, Language, <<"E"/utf8>>, Length)
            end;

        _ ->
            <<""/utf8>>
    end.

-spec format_with_tokens(language(), list(rada@date@pattern:token()), date()) -> binary().
format_with_tokens(Language, Tokens, Date) ->
    gleam@list:fold(Tokens, <<""/utf8>>, fun(Formatted, Token) -> case Token of
                {field, Char, Length} ->
                    <<(format_field(Date, Language, Char, Length))/binary,
                        Formatted/binary>>;

                {literal, Str} ->
                    <<Str/binary, Formatted/binary>>
            end end).

-spec format_with_language(date(), language(), binary()) -> binary().
format_with_language(Date, Language, Pattern_text) ->
    Tokens = begin
        _pipe = Pattern_text,
        _pipe@1 = rada@date@pattern:from_string(_pipe),
        lists:reverse(_pipe@1)
    end,
    format_with_tokens(Language, Tokens, Date).

-spec format(date(), binary()) -> binary().
format(Date, Pattern) ->
    format_with_language(Date, language_en(), Pattern).

-spec to_iso_string(date()) -> binary().
to_iso_string(Date) ->
    format(Date, <<"yyyy-MM-dd"/utf8>>).

-spec add(date(), integer(), unit()) -> date().
add(Date, Count, Unit) ->
    {rd, Rd} = Date,
    case Unit of
        years ->
            add(Date, (12 * Count), months);

        months ->
            Calendar_date = to_calendar_date(Date),
            Whole_months = ((12 * (erlang:element(2, Calendar_date) - 1)) + (month_to_number(
                erlang:element(3, Calendar_date)
            )
            - 1))
            + Count,
            Year = floor_div(Whole_months, 12) + 1,
            Month = number_to_month(modulo_unwrap(Whole_months, 12) + 1),
            {rd,
                (days_before_year(Year) + days_before_month(Year, Month)) + gleam@int:min(
                    erlang:element(4, Calendar_date),
                    days_in_month(Year, Month)
                )};

        weeks ->
            {rd, Rd + (7 * Count)};

        days ->
            {rd, Rd + Count}
    end.

-spec to_months(integer()) -> float().
to_months(Rd) ->
    Calendar_date = to_calendar_date({rd, Rd}),
    Whole_months = (12 * (erlang:element(2, Calendar_date) - 1)) + (month_to_number(
        erlang:element(3, Calendar_date)
    )
    - 1),
    Fraction = gleam@int:to_float(erlang:element(4, Calendar_date)) / 100.0,
    gleam@int:to_float(Whole_months) + Fraction.

-spec diff(unit(), date(), date()) -> integer().
diff(Unit, Date1, Date2) ->
    {rd, Rd1} = Date1,
    {rd, Rd2} = Date2,
    case Unit of
        years ->
            _pipe = (to_months(Rd2) - to_months(Rd1)),
            _pipe@1 = gleam@float:truncate(_pipe),
            _pipe@2 = gleam@int:divide(_pipe@1, 12),
            gleam@result:unwrap(_pipe@2, 0);

        months ->
            _pipe@3 = (to_months(Rd2) - to_months(Rd1)),
            gleam@float:truncate(_pipe@3);

        weeks ->
            _pipe@4 = gleam@int:divide((Rd2 - Rd1), 7),
            gleam@result:unwrap(_pipe@4, 0);

        days ->
            Rd2 - Rd1
    end.

-spec floor(date(), interval()) -> date().
floor(Date, Interval) ->
    {rd, Rd} = Date,
    case Interval of
        year ->
            first_of_year(year(Date));

        quarter ->
            first_of_month(
                year(Date),
                begin
                    _pipe = quarter(Date),
                    quarter_to_month(_pipe)
                end
            );

        month ->
            first_of_month(year(Date), month(Date));

        week ->
            {rd, Rd - days_since_previous_weekday(mon, Date)};

        monday ->
            {rd, Rd - days_since_previous_weekday(mon, Date)};

        tuesday ->
            {rd, Rd - days_since_previous_weekday(tue, Date)};

        wednesday ->
            {rd, Rd - days_since_previous_weekday(wed, Date)};

        thursday ->
            {rd, Rd - days_since_previous_weekday(thu, Date)};

        friday ->
            {rd, Rd - days_since_previous_weekday(fri, Date)};

        saturday ->
            {rd, Rd - days_since_previous_weekday(sat, Date)};

        sunday ->
            {rd, Rd - days_since_previous_weekday(sun, Date)};

        day ->
            Date
    end.

-spec ceiling(date(), interval()) -> date().
ceiling(Date, Interval) ->
    Floored_date = floor(Date, Interval),
    case Date =:= Floored_date of
        true ->
            Date;

        false ->
            {N, Unit} = interval_to_units(Interval),
            add(Floored_date, N, Unit)
    end.

-spec range_help(unit(), integer(), integer(), list(date()), integer()) -> list(date()).
range_help(Unit, Step, Until_rd, Reversed_list, Current_rd) ->
    case Current_rd < Until_rd of
        true ->
            {rd, Next_rd} = add({rd, Current_rd}, Step, Unit),
            range_help(
                Unit,
                Step,
                Until_rd,
                [{rd, Current_rd} | Reversed_list],
                Next_rd
            );

        false ->
            lists:reverse(Reversed_list)
    end.

-spec range(interval(), integer(), date(), date()) -> list(date()).
range(Interval, Step, Start_date, Until_date) ->
    {N, Unit} = interval_to_units(Interval),
    {rd, First_rd} = ceiling(Start_date, Interval),
    {rd, Until_rd} = Until_date,
    case First_rd < Until_rd of
        true ->
            range_help(Unit, gleam@int:max(1, Step * N), Until_rd, [], First_rd);

        false ->
            []
    end.

-spec is_between_int(integer(), integer(), integer()) -> boolean().
is_between_int(Value, Lower, Upper) ->
    (Lower =< Value) andalso (Value =< Upper).

-spec from_ordinal_parts(integer(), integer()) -> {ok, date()} |
    {error, binary()}.
from_ordinal_parts(Year, Ordinal) ->
    Days_in_year = case is_leap_year(Year) of
        true ->
            366;

        false ->
            365
    end,
    case not is_between_int(Ordinal, 1, Days_in_year) of
        true ->
            {error,
                <<<<<<<<"Invalid ordinal date: "/utf8,
                                ((<<<<"ordinal-day "/utf8,
                                        (gleam@int:to_string(Ordinal))/binary>>/binary,
                                    " is out of range"/utf8>>))/binary>>/binary,
                            ((<<<<" (1 to "/utf8,
                                    (gleam@int:to_string(Days_in_year))/binary>>/binary,
                                ")"/utf8>>))/binary>>/binary,
                        ((<<" for "/utf8, (gleam@int:to_string(Year))/binary>>))/binary>>/binary,
                    ((<<<<<<<<"; received (year "/utf8,
                                    (gleam@int:to_string(Year))/binary>>/binary,
                                ", ordinal-day "/utf8>>/binary,
                            (gleam@int:to_string(Ordinal))/binary>>/binary,
                        ")"/utf8>>))/binary>>};

        false ->
            {ok, {rd, days_before_year(Year) + Ordinal}}
    end.

-spec from_calendar_parts(integer(), integer(), integer()) -> {ok, date()} |
    {error, binary()}.
from_calendar_parts(Year, Month_number, Day) ->
    case {is_between_int(Month_number, 1, 12),
        is_between_int(
            Day,
            1,
            days_in_month(Year, number_to_month(Month_number))
        )} of
        {false, _} ->
            {error,
                <<<<<<"Invalid date: "/utf8,
                            ((<<<<"month "/utf8,
                                    (gleam@int:to_string(Month_number))/binary>>/binary,
                                " is out of range"/utf8>>))/binary>>/binary,
                        " (1 to 12)"/utf8>>/binary,
                    ((<<<<<<<<<<<<"; received (year "/utf8,
                                            (gleam@int:to_string(Year))/binary>>/binary,
                                        ", month "/utf8>>/binary,
                                    (gleam@int:to_string(Month_number))/binary>>/binary,
                                ", day "/utf8>>/binary,
                            (gleam@int:to_string(Day))/binary>>/binary,
                        ")"/utf8>>))/binary>>};

        {true, false} ->
            {error,
                <<<<<<<<<<"Invalid date: "/utf8,
                                    ((<<<<"day "/utf8,
                                            (gleam@int:to_string(Day))/binary>>/binary,
                                        " is out of range"/utf8>>))/binary>>/binary,
                                ((<<<<" (1 to "/utf8,
                                        (gleam@int:to_string(
                                            days_in_month(
                                                Year,
                                                number_to_month(Month_number)
                                            )
                                        ))/binary>>/binary,
                                    ")"/utf8>>))/binary>>/binary,
                            ((<<" for "/utf8,
                                (begin
                                    _pipe = Month_number,
                                    _pipe@1 = number_to_month(_pipe),
                                    month_to_name(_pipe@1)
                                end)/binary>>))/binary>>/binary,
                        ((case (Month_number =:= 2) andalso (Day =:= 29) of
                            true ->
                                <<<<" ("/utf8,
                                        (gleam@int:to_string(Year))/binary>>/binary,
                                    " is not a leap year)"/utf8>>;

                            false ->
                                <<""/utf8>>
                        end))/binary>>/binary,
                    ((<<<<<<<<<<<<"; received (year "/utf8,
                                            (gleam@int:to_string(Year))/binary>>/binary,
                                        ", month "/utf8>>/binary,
                                    (gleam@int:to_string(Month_number))/binary>>/binary,
                                ", day "/utf8>>/binary,
                            (gleam@int:to_string(Day))/binary>>/binary,
                        ")"/utf8>>))/binary>>};

        {true, true} ->
            {ok,
                {rd,
                    (days_before_year(Year) + days_before_month(
                        Year,
                        number_to_month(Month_number)
                    ))
                    + Day}}
    end.

-spec from_week_parts(integer(), integer(), integer()) -> {ok, date()} |
    {error, binary()}.
from_week_parts(Week_year, Week_number, Weekday_number) ->
    Weeks_in_year = case is_53_week_year(Week_year) of
        true ->
            53;

        false ->
            52
    end,
    case {is_between_int(Week_number, 1, Weeks_in_year),
        is_between_int(Weekday_number, 1, 7)} of
        {false, _} ->
            {error,
                <<<<<<<<"Invalid week date: "/utf8,
                                ((<<<<"week "/utf8,
                                        (gleam@int:to_string(Week_number))/binary>>/binary,
                                    " is out of range"/utf8>>))/binary>>/binary,
                            ((<<<<" (1 to "/utf8,
                                    (gleam@int:to_string(Weeks_in_year))/binary>>/binary,
                                ")"/utf8>>))/binary>>/binary,
                        ((<<" for "/utf8,
                            (gleam@int:to_string(Week_year))/binary>>))/binary>>/binary,
                    ((<<<<<<<<<<<<"; received (year "/utf8,
                                            (gleam@int:to_string(Week_year))/binary>>/binary,
                                        ", week "/utf8>>/binary,
                                    (gleam@int:to_string(Week_number))/binary>>/binary,
                                ", weekday "/utf8>>/binary,
                            (gleam@int:to_string(Weekday_number))/binary>>/binary,
                        ")"/utf8>>))/binary>>};

        {true, false} ->
            {error,
                <<<<<<"Invalid week date: "/utf8,
                            ((<<<<"weekday "/utf8,
                                    (gleam@int:to_string(Weekday_number))/binary>>/binary,
                                " is out of range"/utf8>>))/binary>>/binary,
                        " (1 to 7)"/utf8>>/binary,
                    ((<<<<<<<<<<<<"; received (year "/utf8,
                                            (gleam@int:to_string(Week_year))/binary>>/binary,
                                        ", week "/utf8>>/binary,
                                    (gleam@int:to_string(Week_number))/binary>>/binary,
                                ", weekday "/utf8>>/binary,
                            (gleam@int:to_string(Weekday_number))/binary>>/binary,
                        ")"/utf8>>))/binary>>};

        {true, true} ->
            {ok,
                {rd,
                    (days_before_week_year(Week_year) + ((Week_number - 1) * 7))
                    + Weekday_number}}
    end.

-spec from_year_and_day_of_year(integer(), day_of_year()) -> {ok, date()} |
    {error, binary()}.
from_year_and_day_of_year(Year, Day_of_year) ->
    case Day_of_year of
        {month_and_day, Month_number, Day} ->
            from_calendar_parts(Year, Month_number, Day);

        {week_and_weekday, Week_number, Weekday_number} ->
            from_week_parts(Year, Week_number, Weekday_number);

        {ordinal_day, Ordinal_day} ->
            from_ordinal_parts(Year, Ordinal_day)
    end.

-spec parser() -> nibble:parser({ok, date()} | {error, binary()}, rada@date@parse:parse_date_token(), any()).
parser() ->
    nibble:do(
        int_4(),
        fun(Year) ->
            nibble:do(
                parse_day_of_year(),
                fun(Day_of_year) ->
                    nibble:return(from_year_and_day_of_year(Year, Day_of_year))
                end
            )
        end
    ).

-spec from_iso_string(binary()) -> {ok, date()} | {error, binary()}.
from_iso_string(Str) ->
    _assert_subject = nibble@lexer:run(Str, rada@date@parse:lexer()),
    {ok, Tokens} = case _assert_subject of
        {ok, _} -> _assert_subject;
        _assert_fail ->
            erlang:error(#{gleam_error => let_assert,
                        message => <<"Assertion pattern match failed"/utf8>>,
                        value => _assert_fail,
                        module => <<"rada/date"/utf8>>,
                        function => <<"from_iso_string"/utf8>>,
                        line => 950})
    end,
    Result = nibble:run(
        Tokens,
        begin
            _pipe = parser(),
            nibble:then(
                _pipe,
                fun(Val) ->
                    nibble:one_of(
                        [begin
                                _pipe@1 = nibble:eof(),
                                nibble:then(
                                    _pipe@1,
                                    fun(_) -> nibble:succeed(Val) end
                                )
                            end,
                            begin
                                _pipe@2 = nibble:token(time_token),
                                nibble:then(
                                    _pipe@2,
                                    fun(_) ->
                                        nibble:succeed(
                                            {error,
                                                <<"Expected a date only, not a date and time"/utf8>>}
                                        )
                                    end
                                )
                            end,
                            nibble:succeed(
                                {error, <<"Expected a date only"/utf8>>}
                            )]
                    )
                end
            )
        end
    ),
    case Result of
        {ok, {ok, Value}} ->
            {ok, Value};

        {ok, {error, Err}} ->
            {error, Err};

        {error, _} ->
            {error, <<"Expected a date in ISO 8601 format"/utf8>>}
    end.

-spec is_between(date(), date(), date()) -> boolean().
is_between(Value, Lower, Upper) ->
    {rd, Value_rd} = Value,
    {rd, Lower_rd} = Lower,
    {rd, Upper_rd} = Upper,
    is_between_int(Value_rd, Lower_rd, Upper_rd).
