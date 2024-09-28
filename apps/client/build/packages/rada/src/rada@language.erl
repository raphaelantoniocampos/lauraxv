-module(rada@language).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export([italian/0, german/0, spanish/0]).

-spec month_name_it(rada@date:month()) -> binary().
month_name_it(Month) ->
    case Month of
        jan ->
            <<"gennaio"/utf8>>;

        feb ->
            <<"febbraio"/utf8>>;

        mar ->
            <<"marzo"/utf8>>;

        apr ->
            <<"aprile"/utf8>>;

        may ->
            <<"maggio"/utf8>>;

        jun ->
            <<"giugno"/utf8>>;

        jul ->
            <<"luglio"/utf8>>;

        aug ->
            <<"agosto"/utf8>>;

        sep ->
            <<"settembre"/utf8>>;

        oct ->
            <<"ottobre"/utf8>>;

        nov ->
            <<"novembre"/utf8>>;

        dec ->
            <<"dicembre"/utf8>>
    end.

-spec month_name_short_it(rada@date:month()) -> binary().
month_name_short_it(Month) ->
    case Month of
        jan ->
            <<"gen"/utf8>>;

        feb ->
            <<"feb"/utf8>>;

        mar ->
            <<"mar"/utf8>>;

        apr ->
            <<"apr"/utf8>>;

        may ->
            <<"mag"/utf8>>;

        jun ->
            <<"giu"/utf8>>;

        jul ->
            <<"lug"/utf8>>;

        aug ->
            <<"ago"/utf8>>;

        sep ->
            <<"set"/utf8>>;

        oct ->
            <<"ott"/utf8>>;

        nov ->
            <<"nov"/utf8>>;

        dec ->
            <<"dic"/utf8>>
    end.

-spec weekday_name_it(rada@date:weekday()) -> binary().
weekday_name_it(Weekday) ->
    case Weekday of
        mon ->
            <<"lunedì"/utf8>>;

        tue ->
            <<"martedì"/utf8>>;

        wed ->
            <<"mercoledì"/utf8>>;

        thu ->
            <<"giovedì"/utf8>>;

        fri ->
            <<"venerdì"/utf8>>;

        sat ->
            <<"sabato"/utf8>>;

        sun ->
            <<"domenica"/utf8>>
    end.

-spec weekday_name_short_it(rada@date:weekday()) -> binary().
weekday_name_short_it(Weekday) ->
    case Weekday of
        mon ->
            <<"lun"/utf8>>;

        tue ->
            <<"mar"/utf8>>;

        wed ->
            <<"mer"/utf8>>;

        thu ->
            <<"gio"/utf8>>;

        fri ->
            <<"ven"/utf8>>;

        sat ->
            <<"sab"/utf8>>;

        sun ->
            <<"dom"/utf8>>
    end.

-spec day_with_suffix_it(integer()) -> binary().
day_with_suffix_it(Day) ->
    gleam@int:to_string(Day).

-spec italian() -> rada@date:language().
italian() ->
    {language,
        fun month_name_it/1,
        fun month_name_short_it/1,
        fun weekday_name_it/1,
        fun weekday_name_short_it/1,
        fun day_with_suffix_it/1}.

-spec month_name_de(rada@date:month()) -> binary().
month_name_de(Month) ->
    case Month of
        jan ->
            <<"Januar"/utf8>>;

        feb ->
            <<"Februar"/utf8>>;

        mar ->
            <<"März"/utf8>>;

        apr ->
            <<"April"/utf8>>;

        may ->
            <<"Mai"/utf8>>;

        jun ->
            <<"Juni"/utf8>>;

        jul ->
            <<"Juli"/utf8>>;

        aug ->
            <<"August"/utf8>>;

        sep ->
            <<"September"/utf8>>;

        oct ->
            <<"Oktober"/utf8>>;

        nov ->
            <<"November"/utf8>>;

        dec ->
            <<"Dezember"/utf8>>
    end.

-spec month_name_short_de(rada@date:month()) -> binary().
month_name_short_de(Month) ->
    _pipe = month_name_de(Month),
    gleam@string:slice(_pipe, 0, 3).

-spec weekday_name_de(rada@date:weekday()) -> binary().
weekday_name_de(Weekday) ->
    case Weekday of
        mon ->
            <<"Montag"/utf8>>;

        tue ->
            <<"Dienstag"/utf8>>;

        wed ->
            <<"Mittwoch"/utf8>>;

        thu ->
            <<"Donnerstag"/utf8>>;

        fri ->
            <<"Freitag"/utf8>>;

        sat ->
            <<"Samstag"/utf8>>;

        sun ->
            <<"Sonntag"/utf8>>
    end.

-spec weekday_name_short_de(rada@date:weekday()) -> binary().
weekday_name_short_de(Weekday) ->
    _pipe = weekday_name_de(Weekday),
    gleam@string:slice(_pipe, 0, 2).

-spec day_with_suffix_de(integer()) -> binary().
day_with_suffix_de(Day) ->
    <<(gleam@int:to_string(Day))/binary, "."/utf8>>.

-spec german() -> rada@date:language().
german() ->
    {language,
        fun month_name_de/1,
        fun month_name_short_de/1,
        fun weekday_name_de/1,
        fun weekday_name_short_de/1,
        fun day_with_suffix_de/1}.

-spec month_name_es(rada@date:month()) -> binary().
month_name_es(Month) ->
    case Month of
        jan ->
            <<"enero"/utf8>>;

        feb ->
            <<"febrero"/utf8>>;

        mar ->
            <<"marzo"/utf8>>;

        apr ->
            <<"abril"/utf8>>;

        may ->
            <<"mayo"/utf8>>;

        jun ->
            <<"junio"/utf8>>;

        jul ->
            <<"julio"/utf8>>;

        aug ->
            <<"agosto"/utf8>>;

        sep ->
            <<"setiembre"/utf8>>;

        oct ->
            <<"octubre"/utf8>>;

        nov ->
            <<"noviembre"/utf8>>;

        dec ->
            <<"diciembre"/utf8>>
    end.

-spec month_name_short_es(rada@date:month()) -> binary().
month_name_short_es(Month) ->
    case Month of
        jan ->
            <<"ene"/utf8>>;

        feb ->
            <<"feb"/utf8>>;

        mar ->
            <<"mar"/utf8>>;

        apr ->
            <<"abr"/utf8>>;

        may ->
            <<"may"/utf8>>;

        jun ->
            <<"jun"/utf8>>;

        jul ->
            <<"jul"/utf8>>;

        aug ->
            <<"ago"/utf8>>;

        sep ->
            <<"set"/utf8>>;

        oct ->
            <<"oct"/utf8>>;

        nov ->
            <<"nov"/utf8>>;

        dec ->
            <<"dic"/utf8>>
    end.

-spec weekday_name_es(rada@date:weekday()) -> binary().
weekday_name_es(Weekday) ->
    case Weekday of
        mon ->
            <<"lunes"/utf8>>;

        tue ->
            <<"martes"/utf8>>;

        wed ->
            <<"miercoles"/utf8>>;

        thu ->
            <<"jueves"/utf8>>;

        fri ->
            <<"viernes"/utf8>>;

        sat ->
            <<"sabado"/utf8>>;

        sun ->
            <<"domingo"/utf8>>
    end.

-spec weekday_name_short_es(rada@date:weekday()) -> binary().
weekday_name_short_es(Weekday) ->
    case Weekday of
        mon ->
            <<"lun"/utf8>>;

        tue ->
            <<"mar"/utf8>>;

        wed ->
            <<"mie"/utf8>>;

        thu ->
            <<"jue"/utf8>>;

        fri ->
            <<"vie"/utf8>>;

        sat ->
            <<"sab"/utf8>>;

        sun ->
            <<"dom"/utf8>>
    end.

-spec day_with_suffix_es(integer()) -> binary().
day_with_suffix_es(Day) ->
    gleam@int:to_string(Day).

-spec spanish() -> rada@date:language().
spanish() ->
    {language,
        fun month_name_es/1,
        fun month_name_short_es/1,
        fun weekday_name_es/1,
        fun weekday_name_short_es/1,
        fun day_with_suffix_es/1}.
