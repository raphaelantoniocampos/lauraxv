-module(lustre@ui@util@colour).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export([encode_scale/1, scale_decoder/1, grey/0, grey_dark/0, mauve/0, mauve_dark/0, slate/0, slate_dark/0, sage/0, sage_dark/0, olive/0, olive_dark/0, sand/0, sand_dark/0, gold/0, gold_dark/0, bronze/0, bronze_dark/0, brown/0, brown_dark/0, yellow/0, yellow_dark/0, amber/0, amber_dark/0, orange/0, orange_dark/0, tomato/0, tomato_dark/0, red/0, red_dark/0, ruby/0, ruby_dark/0, crimson/0, crimson_dark/0, pink/0, pink_dark/0, plum/0, plum_dark/0, purple/0, purple_dark/0, violet/0, violet_dark/0, iris/0, iris_dark/0, indigo/0, indigo_dark/0, blue/0, blue_dark/0, cyan/0, cyan_dark/0, teal/0, teal_dark/0, jade/0, jade_dark/0, green/0, green_dark/0, grass/0, grass_dark/0, lime/0, lime_dark/0, mint/0, mint_dark/0, sky/0, sky_dark/0]).
-export_type([scale/0]).

-type scale() :: {scale,
        gleam_community@colour:colour(),
        gleam_community@colour:colour(),
        gleam_community@colour:colour(),
        gleam_community@colour:colour(),
        gleam_community@colour:colour(),
        gleam_community@colour:colour(),
        gleam_community@colour:colour(),
        gleam_community@colour:colour(),
        gleam_community@colour:colour(),
        gleam_community@colour:colour(),
        gleam_community@colour:colour(),
        gleam_community@colour:colour()}.

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/lustre_ui/src/lustre/ui/util/colour.gleam", 47).
-spec encode_scale(scale()) -> gleam@json:json().
encode_scale(Scale) ->
    gleam@json:object(
        [{<<"app_background"/utf8>>,
                gleam_community@colour:encode(erlang:element(2, Scale))},
            {<<"app_background_subtle"/utf8>>,
                gleam_community@colour:encode(erlang:element(3, Scale))},
            {<<"app_border"/utf8>>,
                gleam_community@colour:encode(erlang:element(4, Scale))},
            {<<"element_background"/utf8>>,
                gleam_community@colour:encode(erlang:element(5, Scale))},
            {<<"element_background_hover"/utf8>>,
                gleam_community@colour:encode(erlang:element(6, Scale))},
            {<<"element_background_strong"/utf8>>,
                gleam_community@colour:encode(erlang:element(7, Scale))},
            {<<"element_border_subtle"/utf8>>,
                gleam_community@colour:encode(erlang:element(8, Scale))},
            {<<"element_border_strong"/utf8>>,
                gleam_community@colour:encode(erlang:element(9, Scale))},
            {<<"solid_background"/utf8>>,
                gleam_community@colour:encode(erlang:element(10, Scale))},
            {<<"solid_background_hover"/utf8>>,
                gleam_community@colour:encode(erlang:element(11, Scale))},
            {<<"text_high_contrast"/utf8>>,
                gleam_community@colour:encode(erlang:element(12, Scale))},
            {<<"text_low_contrast"/utf8>>,
                gleam_community@colour:encode(erlang:element(13, Scale))}]
    ).

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/lustre_ui/src/lustre/ui/util/colour.gleam", 67).
-spec scale_decoder(gleam@dynamic:dynamic_()) -> {ok, scale()} |
    {error, list(gleam@dynamic:decode_error())}.
scale_decoder(Json) ->
    Attempt = fun(Field, Then) ->
        gleam@result:'try'(
            (gleam@dynamic:field(Field, fun gleam_community@colour:decoder/1))(
                Json
            ),
            Then
        )
    end,
    Attempt(
        <<"app_background"/utf8>>,
        fun(App_background) ->
            Attempt(
                <<"app_background_subtle"/utf8>>,
                fun(App_background_subtle) ->
                    Attempt(
                        <<"app_border"/utf8>>,
                        fun(App_border) ->
                            Attempt(
                                <<"element_background"/utf8>>,
                                fun(Element_background) ->
                                    Attempt(
                                        <<"element_background_hover"/utf8>>,
                                        fun(Element_background_hover) ->
                                            Attempt(
                                                <<"element_background_strong"/utf8>>,
                                                fun(Element_background_strong) ->
                                                    Attempt(
                                                        <<"element_border_subtle"/utf8>>,
                                                        fun(
                                                            Element_border_subtle
                                                        ) ->
                                                            Attempt(
                                                                <<"element_border_strong"/utf8>>,
                                                                fun(
                                                                    Element_border_strong
                                                                ) ->
                                                                    Attempt(
                                                                        <<"solid_background"/utf8>>,
                                                                        fun(
                                                                            Solid_background
                                                                        ) ->
                                                                            Attempt(
                                                                                <<"solid_background_hover"/utf8>>,
                                                                                fun(
                                                                                    Solid_background_hover
                                                                                ) ->
                                                                                    Attempt(
                                                                                        <<"text_high_contrast"/utf8>>,
                                                                                        fun(
                                                                                            Text_high_contrast
                                                                                        ) ->
                                                                                            Attempt(
                                                                                                <<"text_low_contrast"/utf8>>,
                                                                                                fun(
                                                                                                    Text_low_contrast
                                                                                                ) ->
                                                                                                    {ok,
                                                                                                        {scale,
                                                                                                            App_background,
                                                                                                            App_background_subtle,
                                                                                                            App_border,
                                                                                                            Element_background,
                                                                                                            Element_background_hover,
                                                                                                            Element_background_strong,
                                                                                                            Element_border_subtle,
                                                                                                            Element_border_strong,
                                                                                                            Solid_background,
                                                                                                            Solid_background_hover,
                                                                                                            Text_high_contrast,
                                                                                                            Text_low_contrast}}
                                                                                                end
                                                                                            )
                                                                                        end
                                                                                    )
                                                                                end
                                                                            )
                                                                        end
                                                                    )
                                                                end
                                                            )
                                                        end
                                                    )
                                                end
                                            )
                                        end
                                    )
                                end
                            )
                        end
                    )
                end
            )
        end
    ).

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/lustre_ui/src/lustre/ui/util/colour.gleam", 101).
-spec from_radix_scale(
    integer(),
    integer(),
    integer(),
    integer(),
    integer(),
    integer(),
    integer(),
    integer(),
    integer(),
    integer(),
    integer(),
    integer()
) -> scale().
from_radix_scale(A, B, C, D, E, F, G, H, I, J, K, L) ->
    _assert_subject = gleam_community@colour:from_rgb_hex(A),
    {ok, App_background} = case _assert_subject of
        {ok, _} -> _assert_subject;
        _assert_fail ->
            erlang:error(#{gleam_error => let_assert,
                        message => <<"Pattern match failed, no pattern matched the value."/utf8>>,
                        value => _assert_fail,
                        module => <<"lustre/ui/util/colour"/utf8>>,
                        function => <<"from_radix_scale"/utf8>>,
                        line => 115})
    end,
    _assert_subject@1 = gleam_community@colour:from_rgb_hex(B),
    {ok, App_background_subtle} = case _assert_subject@1 of
        {ok, _} -> _assert_subject@1;
        _assert_fail@1 ->
            erlang:error(#{gleam_error => let_assert,
                        message => <<"Pattern match failed, no pattern matched the value."/utf8>>,
                        value => _assert_fail@1,
                        module => <<"lustre/ui/util/colour"/utf8>>,
                        function => <<"from_radix_scale"/utf8>>,
                        line => 116})
    end,
    _assert_subject@2 = gleam_community@colour:from_rgb_hex(C),
    {ok, App_border} = case _assert_subject@2 of
        {ok, _} -> _assert_subject@2;
        _assert_fail@2 ->
            erlang:error(#{gleam_error => let_assert,
                        message => <<"Pattern match failed, no pattern matched the value."/utf8>>,
                        value => _assert_fail@2,
                        module => <<"lustre/ui/util/colour"/utf8>>,
                        function => <<"from_radix_scale"/utf8>>,
                        line => 117})
    end,
    _assert_subject@3 = gleam_community@colour:from_rgb_hex(D),
    {ok, Element_background} = case _assert_subject@3 of
        {ok, _} -> _assert_subject@3;
        _assert_fail@3 ->
            erlang:error(#{gleam_error => let_assert,
                        message => <<"Pattern match failed, no pattern matched the value."/utf8>>,
                        value => _assert_fail@3,
                        module => <<"lustre/ui/util/colour"/utf8>>,
                        function => <<"from_radix_scale"/utf8>>,
                        line => 118})
    end,
    _assert_subject@4 = gleam_community@colour:from_rgb_hex(E),
    {ok, Element_background_hover} = case _assert_subject@4 of
        {ok, _} -> _assert_subject@4;
        _assert_fail@4 ->
            erlang:error(#{gleam_error => let_assert,
                        message => <<"Pattern match failed, no pattern matched the value."/utf8>>,
                        value => _assert_fail@4,
                        module => <<"lustre/ui/util/colour"/utf8>>,
                        function => <<"from_radix_scale"/utf8>>,
                        line => 119})
    end,
    _assert_subject@5 = gleam_community@colour:from_rgb_hex(F),
    {ok, Element_background_strong} = case _assert_subject@5 of
        {ok, _} -> _assert_subject@5;
        _assert_fail@5 ->
            erlang:error(#{gleam_error => let_assert,
                        message => <<"Pattern match failed, no pattern matched the value."/utf8>>,
                        value => _assert_fail@5,
                        module => <<"lustre/ui/util/colour"/utf8>>,
                        function => <<"from_radix_scale"/utf8>>,
                        line => 120})
    end,
    _assert_subject@6 = gleam_community@colour:from_rgb_hex(G),
    {ok, Element_border_strong} = case _assert_subject@6 of
        {ok, _} -> _assert_subject@6;
        _assert_fail@6 ->
            erlang:error(#{gleam_error => let_assert,
                        message => <<"Pattern match failed, no pattern matched the value."/utf8>>,
                        value => _assert_fail@6,
                        module => <<"lustre/ui/util/colour"/utf8>>,
                        function => <<"from_radix_scale"/utf8>>,
                        line => 121})
    end,
    _assert_subject@7 = gleam_community@colour:from_rgb_hex(H),
    {ok, Element_border_subtle} = case _assert_subject@7 of
        {ok, _} -> _assert_subject@7;
        _assert_fail@7 ->
            erlang:error(#{gleam_error => let_assert,
                        message => <<"Pattern match failed, no pattern matched the value."/utf8>>,
                        value => _assert_fail@7,
                        module => <<"lustre/ui/util/colour"/utf8>>,
                        function => <<"from_radix_scale"/utf8>>,
                        line => 122})
    end,
    _assert_subject@8 = gleam_community@colour:from_rgb_hex(I),
    {ok, Solid_background} = case _assert_subject@8 of
        {ok, _} -> _assert_subject@8;
        _assert_fail@8 ->
            erlang:error(#{gleam_error => let_assert,
                        message => <<"Pattern match failed, no pattern matched the value."/utf8>>,
                        value => _assert_fail@8,
                        module => <<"lustre/ui/util/colour"/utf8>>,
                        function => <<"from_radix_scale"/utf8>>,
                        line => 123})
    end,
    _assert_subject@9 = gleam_community@colour:from_rgb_hex(J),
    {ok, Solid_background_hover} = case _assert_subject@9 of
        {ok, _} -> _assert_subject@9;
        _assert_fail@9 ->
            erlang:error(#{gleam_error => let_assert,
                        message => <<"Pattern match failed, no pattern matched the value."/utf8>>,
                        value => _assert_fail@9,
                        module => <<"lustre/ui/util/colour"/utf8>>,
                        function => <<"from_radix_scale"/utf8>>,
                        line => 124})
    end,
    _assert_subject@10 = gleam_community@colour:from_rgb_hex(K),
    {ok, Text_high_contrast} = case _assert_subject@10 of
        {ok, _} -> _assert_subject@10;
        _assert_fail@10 ->
            erlang:error(#{gleam_error => let_assert,
                        message => <<"Pattern match failed, no pattern matched the value."/utf8>>,
                        value => _assert_fail@10,
                        module => <<"lustre/ui/util/colour"/utf8>>,
                        function => <<"from_radix_scale"/utf8>>,
                        line => 125})
    end,
    _assert_subject@11 = gleam_community@colour:from_rgb_hex(L),
    {ok, Text_low_contrast} = case _assert_subject@11 of
        {ok, _} -> _assert_subject@11;
        _assert_fail@11 ->
            erlang:error(#{gleam_error => let_assert,
                        message => <<"Pattern match failed, no pattern matched the value."/utf8>>,
                        value => _assert_fail@11,
                        module => <<"lustre/ui/util/colour"/utf8>>,
                        function => <<"from_radix_scale"/utf8>>,
                        line => 126})
    end,
    {scale,
        App_background,
        App_background_subtle,
        App_border,
        Element_background,
        Element_background_hover,
        Element_background_strong,
        Element_border_subtle,
        Element_border_strong,
        Solid_background,
        Solid_background_hover,
        Text_high_contrast,
        Text_low_contrast}.

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/lustre_ui/src/lustre/ui/util/colour.gleam", 146).
-spec grey() -> scale().
grey() ->
    from_radix_scale(
        16#FCFCFC,
        16#F9F9F9,
        16#DDDDDD,
        16#F1F1F1,
        16#EBEBEB,
        16#E4E4E4,
        16#BBBBBB,
        16#D4D4D4,
        16#8D8D8D,
        16#808080,
        16#202020,
        16#646464
    ).

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/lustre_ui/src/lustre/ui/util/colour.gleam", 163).
-spec grey_dark() -> scale().
grey_dark() ->
    from_radix_scale(
        16#111111,
        16#191919,
        16#3A3A3A,
        16#222222,
        16#2A2A2A,
        16#313131,
        16#484848,
        16#606060,
        16#6E6E6E,
        16#7B7B7B,
        16#EEEEEE,
        16#B4B4B4
    ).

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/lustre_ui/src/lustre/ui/util/colour.gleam", 180).
-spec mauve() -> scale().
mauve() ->
    from_radix_scale(
        16#FDFCFD,
        16#FAF9FB,
        16#DFDCE3,
        16#F3F1F5,
        16#ECEAEF,
        16#E6E3E9,
        16#BCBAC7,
        16#D5D3DB,
        16#8E8C99,
        16#817F8B,
        16#211F26,
        16#65636D
    ).

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/lustre_ui/src/lustre/ui/util/colour.gleam", 197).
-spec mauve_dark() -> scale().
mauve_dark() ->
    from_radix_scale(
        16#121113,
        16#1A191B,
        16#3C393F,
        16#232225,
        16#2B292D,
        16#323035,
        16#49474E,
        16#625F69,
        16#6F6D78,
        16#7C7A85,
        16#EEEEF0,
        16#B5B2BC
    ).

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/lustre_ui/src/lustre/ui/util/colour.gleam", 214).
-spec slate() -> scale().
slate() ->
    from_radix_scale(
        16#FCFCFD,
        16#F9F9FB,
        16#DDDDE3,
        16#F2F2F5,
        16#EBEBEF,
        16#E4E4E9,
        16#B9BBC6,
        16#D3D4DB,
        16#8B8D98,
        16#7E808A,
        16#1C2024,
        16#60646C
    ).

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/lustre_ui/src/lustre/ui/util/colour.gleam", 231).
-spec slate_dark() -> scale().
slate_dark() ->
    from_radix_scale(
        16#111113,
        16#18191B,
        16#363A3F,
        16#212225,
        16#272A2D,
        16#2E3135,
        16#43484E,
        16#5A6169,
        16#696E77,
        16#777B84,
        16#EDEEF0,
        16#B0B4BA
    ).

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/lustre_ui/src/lustre/ui/util/colour.gleam", 248).
-spec sage() -> scale().
sage() ->
    from_radix_scale(
        16#FBFDFC,
        16#F7F9F8,
        16#DCDFDD,
        16#F0F2F1,
        16#E9ECEB,
        16#E3E6E4,
        16#B8BCBA,
        16#D2D5D3,
        16#868E8B,
        16#7A817F,
        16#1A211E,
        16#5F6563
    ).

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/lustre_ui/src/lustre/ui/util/colour.gleam", 265).
-spec sage_dark() -> scale().
sage_dark() ->
    from_radix_scale(
        16#101211,
        16#171918,
        16#373B39,
        16#202221,
        16#272A29,
        16#2E3130,
        16#444947,
        16#5B625F,
        16#63706B,
        16#717D79,
        16#ECEEED,
        16#ADB5B2
    ).

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/lustre_ui/src/lustre/ui/util/colour.gleam", 282).
-spec olive() -> scale().
olive() ->
    from_radix_scale(
        16#FCFDFC,
        16#F8FAF8,
        16#DBDEDB,
        16#F1F3F1,
        16#EAECEA,
        16#E3E5E3,
        16#B9BCB8,
        16#D2D4D1,
        16#898E87,
        16#7C817B,
        16#1D211C,
        16#60655F
    ).

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/lustre_ui/src/lustre/ui/util/colour.gleam", 299).
-spec olive_dark() -> scale().
olive_dark() ->
    from_radix_scale(
        16#111210,
        16#181917,
        16#383A36,
        16#212220,
        16#282A27,
        16#2F312E,
        16#454843,
        16#5C625B,
        16#687066,
        16#767D74,
        16#ECEEEC,
        16#AFB5AD
    ).

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/lustre_ui/src/lustre/ui/util/colour.gleam", 316).
-spec sand() -> scale().
sand() ->
    from_radix_scale(
        16#FDFDFC,
        16#F9F9F8,
        16#DDDDDA,
        16#F2F2F0,
        16#EBEBE9,
        16#E4E4E2,
        16#BCBBB5,
        16#D3D2CE,
        16#8D8D86,
        16#80807A,
        16#21201C,
        16#63635E
    ).

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/lustre_ui/src/lustre/ui/util/colour.gleam", 333).
-spec sand_dark() -> scale().
sand_dark() ->
    from_radix_scale(
        16#111110,
        16#191918,
        16#3B3A37,
        16#222221,
        16#2A2A28,
        16#31312E,
        16#494844,
        16#62605B,
        16#6F6D66,
        16#7C7B74,
        16#EEEEEC,
        16#B5B3AD
    ).

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/lustre_ui/src/lustre/ui/util/colour.gleam", 350).
-spec gold() -> scale().
gold() ->
    from_radix_scale(
        16#FDFDFC,
        16#FBF9F2,
        16#DAD1BD,
        16#F5F2E9,
        16#EEEADD,
        16#E5DFD0,
        16#B8A383,
        16#CBBDA4,
        16#978365,
        16#89775C,
        16#3B352B,
        16#71624B
    ).

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/lustre_ui/src/lustre/ui/util/colour.gleam", 367).
-spec gold_dark() -> scale().
gold_dark() ->
    from_radix_scale(
        16#121211,
        16#1B1A17,
        16#444039,
        16#24231F,
        16#2D2B26,
        16#38352E,
        16#544F46,
        16#696256,
        16#978365,
        16#A39073,
        16#E8E2D9,
        16#CBB99F
    ).

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/lustre_ui/src/lustre/ui/util/colour.gleam", 384).
-spec bronze() -> scale().
bronze() ->
    from_radix_scale(
        16#FDFCFC,
        16#FDF8F6,
        16#E0CEC7,
        16#F8F1EE,
        16#F2E8E4,
        16#EADDD7,
        16#BFA094,
        16#D1B9B0,
        16#A18072,
        16#947467,
        16#43302B,
        16#7D5E54
    ).

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/lustre_ui/src/lustre/ui/util/colour.gleam", 401).
-spec bronze_dark() -> scale().
bronze_dark() ->
    from_radix_scale(
        16#141110,
        16#1C1917,
        16#493E3A,
        16#262220,
        16#302A27,
        16#3B3330,
        16#5A4C47,
        16#6F5F58,
        16#A18072,
        16#AE8C7E,
        16#EDE0D9,
        16#D4B3A5
    ).

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/lustre_ui/src/lustre/ui/util/colour.gleam", 418).
-spec brown() -> scale().
brown() ->
    from_radix_scale(
        16#FEFDFC,
        16#FCF9F6,
        16#E8CDB5,
        16#F8F1EA,
        16#F4E9DD,
        16#EFDDCC,
        16#D09E72,
        16#DDB896,
        16#AD7F58,
        16#9E7352,
        16#3E332E,
        16#815E46
    ).

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/lustre_ui/src/lustre/ui/util/colour.gleam", 435).
-spec brown_dark() -> scale().
brown_dark() ->
    from_radix_scale(
        16#12110F,
        16#1C1816,
        16#4D3C2F,
        16#28211D,
        16#322922,
        16#3E3128,
        16#614A39,
        16#7C5F46,
        16#AD7F58,
        16#B88C67,
        16#F2E1CA,
        16#DBB594
    ).

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/lustre_ui/src/lustre/ui/util/colour.gleam", 452).
-spec yellow() -> scale().
yellow() ->
    from_radix_scale(
        16#FDFDF9,
        16#FFFBE0,
        16#ECDD85,
        16#FFF8C6,
        16#FCF3AF,
        16#F7EA9B,
        16#C9AA45,
        16#DAC56E,
        16#FBE32D,
        16#F9DA10,
        16#473B1F,
        16#775F28
    ).

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/lustre_ui/src/lustre/ui/util/colour.gleam", 469).
-spec yellow_dark() -> scale().
yellow_dark() ->
    from_radix_scale(
        16#14120B,
        16#1B180F,
        16#524202,
        16#2D2305,
        16#362B00,
        16#433500,
        16#665417,
        16#836A21,
        16#FFE629,
        16#FFFF57,
        16#F6EEB4,
        16#F5E147
    ).

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/lustre_ui/src/lustre/ui/util/colour.gleam", 486).
-spec amber() -> scale().
amber() ->
    from_radix_scale(
        16#FEFDFB,
        16#FFF9ED,
        16#F5D08C,
        16#FFF3D0,
        16#FFECB7,
        16#FFE0A1,
        16#D6A35C,
        16#E4BB78,
        16#FFC53D,
        16#FFBA1A,
        16#4F3422,
        16#915930
    ).

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/lustre_ui/src/lustre/ui/util/colour.gleam", 503).
-spec amber_dark() -> scale().
amber_dark() ->
    from_radix_scale(
        16#16120C,
        16#1D180F,
        16#5C3D05,
        16#302008,
        16#3F2700,
        16#4D3000,
        16#714F19,
        16#8F6424,
        16#FFC53D,
        16#FFD60A,
        16#FFE7B3,
        16#FFCA16
    ).

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/lustre_ui/src/lustre/ui/util/colour.gleam", 520).
-spec orange() -> scale().
orange() ->
    from_radix_scale(
        16#FEFCFB,
        16#FFF8F4,
        16#FFC291,
        16#FFEDD5,
        16#FFE0BB,
        16#FFD3A4,
        16#ED8A5C,
        16#FFAA7D,
        16#F76808,
        16#ED5F00,
        16#582D1D,
        16#99543A
    ).

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/lustre_ui/src/lustre/ui/util/colour.gleam", 537).
-spec orange_dark() -> scale().
orange_dark() ->
    from_radix_scale(
        16#17120E,
        16#1E160F,
        16#66350C,
        16#331E0B,
        16#462100,
        16#562800,
        16#7E451D,
        16#A35829,
        16#F76B15,
        16#FF801F,
        16#FFE0C2,
        16#FFA057
    ).

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/lustre_ui/src/lustre/ui/util/colour.gleam", 554).
-spec tomato() -> scale().
tomato() ->
    from_radix_scale(
        16#FFFCFC,
        16#FFF8F7,
        16#FAC7BE,
        16#FFF0EE,
        16#FFE6E2,
        16#FDD8D3,
        16#EA9280,
        16#F3B0A2,
        16#E54D2E,
        16#D84224,
        16#5C271F,
        16#C33113
    ).

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/lustre_ui/src/lustre/ui/util/colour.gleam", 571).
-spec tomato_dark() -> scale().
tomato_dark() ->
    from_radix_scale(
        16#181111,
        16#1F1513,
        16#6E2920,
        16#391714,
        16#4E1511,
        16#5E1C16,
        16#853A2D,
        16#AC4D39,
        16#E54D2E,
        16#EC6142,
        16#FBD3CB,
        16#FF977D
    ).

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/lustre_ui/src/lustre/ui/util/colour.gleam", 588).
-spec red() -> scale().
red() ->
    from_radix_scale(
        16#FFFCFC,
        16#FFF7F7,
        16#F9C6C6,
        16#FFEFEF,
        16#FFE5E5,
        16#FDD8D8,
        16#EB9091,
        16#F3AEAF,
        16#E5484D,
        16#D93D42,
        16#641723,
        16#C62A2F
    ).

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/lustre_ui/src/lustre/ui/util/colour.gleam", 605).
-spec red_dark() -> scale().
red_dark() ->
    from_radix_scale(
        16#191111,
        16#201314,
        16#72232D,
        16#3B1219,
        16#500F1C,
        16#611623,
        16#8C333A,
        16#B54548,
        16#E5484D,
        16#EC5D5E,
        16#FFD1D9,
        16#FF9592
    ).

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/lustre_ui/src/lustre/ui/util/colour.gleam", 622).
-spec ruby() -> scale().
ruby() ->
    from_radix_scale(
        16#FFFCFD,
        16#FFF7F9,
        16#F5C7D1,
        16#FEEFF3,
        16#FDE5EA,
        16#FAD8E0,
        16#E592A2,
        16#EEAFBC,
        16#E54666,
        16#DA3A5C,
        16#64172B,
        16#CA244D
    ).

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/lustre_ui/src/lustre/ui/util/colour.gleam", 639).
-spec ruby_dark() -> scale().
ruby_dark() ->
    from_radix_scale(
        16#191113,
        16#1E1517,
        16#6F2539,
        16#3A141E,
        16#4E1325,
        16#5E1A2E,
        16#883447,
        16#B3445A,
        16#E54666,
        16#EC5A72,
        16#FED2E1,
        16#FF949D
    ).

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/lustre_ui/src/lustre/ui/util/colour.gleam", 656).
-spec crimson() -> scale().
crimson() ->
    from_radix_scale(
        16#FFFCFD,
        16#FFF7FB,
        16#F4C6DB,
        16#FEEFF6,
        16#FCE5F0,
        16#F9D8E7,
        16#E58FB1,
        16#EDADC8,
        16#E93D82,
        16#DC3175,
        16#621639,
        16#CB1D63
    ).

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/lustre_ui/src/lustre/ui/util/colour.gleam", 673).
-spec crimson_dark() -> scale().
crimson_dark() ->
    from_radix_scale(
        16#191114,
        16#201318,
        16#6D2545,
        16#381525,
        16#4D122F,
        16#5C1839,
        16#873356,
        16#B0436E,
        16#E93D82,
        16#EE518A,
        16#FDD3E8,
        16#FF92AD
    ).

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/lustre_ui/src/lustre/ui/util/colour.gleam", 690).
-spec pink() -> scale().
pink() ->
    from_radix_scale(
        16#FFFCFE,
        16#FFF7FC,
        16#F3C6E2,
        16#FEEEF8,
        16#FCE5F3,
        16#F9D8EC,
        16#E38EC3,
        16#ECADD4,
        16#D6409F,
        16#CD3093,
        16#651249,
        16#C41C87
    ).

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/lustre_ui/src/lustre/ui/util/colour.gleam", 707).
-spec pink_dark() -> scale().
pink_dark() ->
    from_radix_scale(
        16#191117,
        16#21121D,
        16#692955,
        16#37172F,
        16#4B143D,
        16#591C47,
        16#833869,
        16#A84885,
        16#D6409F,
        16#DE51A8,
        16#FDD1EA,
        16#FF8DCC
    ).

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/lustre_ui/src/lustre/ui/util/colour.gleam", 724).
-spec plum() -> scale().
plum() ->
    from_radix_scale(
        16#FEFCFF,
        16#FFF8FF,
        16#EBC8ED,
        16#FCEFFC,
        16#F9E5F9,
        16#F3D9F4,
        16#CF91D8,
        16#DFAFE3,
        16#AB4ABA,
        16#A43CB4,
        16#53195D,
        16#9C2BAD
    ).

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/lustre_ui/src/lustre/ui/util/colour.gleam", 741).
-spec plum_dark() -> scale().
plum_dark() ->
    from_radix_scale(
        16#181118,
        16#201320,
        16#5E3061,
        16#351A35,
        16#451D47,
        16#512454,
        16#734079,
        16#92549C,
        16#AB4ABA,
        16#B658C4,
        16#F4D4F4,
        16#E796F3
    ).

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/lustre_ui/src/lustre/ui/util/colour.gleam", 758).
-spec purple() -> scale().
purple() ->
    from_radix_scale(
        16#FEFCFE,
        16#FDFAFF,
        16#E3CCF4,
        16#F9F1FE,
        16#F3E7FC,
        16#EDDBF9,
        16#BE93E4,
        16#D3B4ED,
        16#8E4EC6,
        16#8445BC,
        16#402060,
        16#793AAF
    ).

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/lustre_ui/src/lustre/ui/util/colour.gleam", 775).
-spec purple_dark() -> scale().
purple_dark() ->
    from_radix_scale(
        16#18111B,
        16#1E1523,
        16#54346B,
        16#301C3B,
        16#3D224E,
        16#48295C,
        16#664282,
        16#8457AA,
        16#8E4EC6,
        16#9A5CD0,
        16#ECD9FA,
        16#D19DFF
    ).

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/lustre_ui/src/lustre/ui/util/colour.gleam", 792).
-spec violet() -> scale().
violet() ->
    from_radix_scale(
        16#FDFCFE,
        16#FBFAFF,
        16#D7CFF9,
        16#F5F2FF,
        16#EDE9FE,
        16#E4DEFC,
        16#AA99EC,
        16#C4B8F3,
        16#6E56CF,
        16#644FC1,
        16#2F265F,
        16#5746AF
    ).

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/lustre_ui/src/lustre/ui/util/colour.gleam", 809).
-spec violet_dark() -> scale().
violet_dark() ->
    from_radix_scale(
        16#14121F,
        16#1B1525,
        16#473876,
        16#291F43,
        16#33255B,
        16#3C2E69,
        16#56468B,
        16#6958AD,
        16#6E56CF,
        16#7D66D9,
        16#E2DDFE,
        16#BAA7FF
    ).

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/lustre_ui/src/lustre/ui/util/colour.gleam", 826).
-spec iris() -> scale().
iris() ->
    from_radix_scale(
        16#FDFDFF,
        16#FAFAFF,
        16#D0D0FA,
        16#F3F3FF,
        16#EBEBFE,
        16#E0E0FD,
        16#9B9EF0,
        16#BABBF5,
        16#5B5BD6,
        16#5353CE,
        16#272962,
        16#4747C2
    ).

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/lustre_ui/src/lustre/ui/util/colour.gleam", 843).
-spec iris_dark() -> scale().
iris_dark() ->
    from_radix_scale(
        16#13131E,
        16#171625,
        16#3D3E82,
        16#202248,
        16#262A65,
        16#303374,
        16#4A4A95,
        16#5958B1,
        16#5B5BD6,
        16#6E6ADE,
        16#E0DFFE,
        16#B1A9FF
    ).

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/lustre_ui/src/lustre/ui/util/colour.gleam", 860).
-spec indigo() -> scale().
indigo() ->
    from_radix_scale(
        16#FDFDFE,
        16#F8FAFF,
        16#C6D4F9,
        16#F0F4FF,
        16#E6EDFE,
        16#D9E2FC,
        16#8DA4EF,
        16#AEC0F5,
        16#3E63DD,
        16#3A5CCC,
        16#1F2D5C,
        16#3451B2
    ).

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/lustre_ui/src/lustre/ui/util/colour.gleam", 877).
-spec indigo_dark() -> scale().
indigo_dark() ->
    from_radix_scale(
        16#11131F,
        16#141726,
        16#304384,
        16#182449,
        16#1D2E62,
        16#253974,
        16#3A4F97,
        16#435DB1,
        16#3E63DD,
        16#5472E4,
        16#D6E1FF,
        16#9EB1FF
    ).

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/lustre_ui/src/lustre/ui/util/colour.gleam", 894).
-spec blue() -> scale().
blue() ->
    from_radix_scale(
        16#FBFDFF,
        16#F5FAFF,
        16#B7D9F8,
        16#EDF6FF,
        16#E1F0FF,
        16#CEE7FE,
        16#5EB0EF,
        16#96C7F2,
        16#0091FF,
        16#0880EA,
        16#113264,
        16#0B68CB
    ).

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/lustre_ui/src/lustre/ui/util/colour.gleam", 911).
-spec blue_dark() -> scale().
blue_dark() ->
    from_radix_scale(
        16#0D1520,
        16#111927,
        16#104D87,
        16#0D2847,
        16#003362,
        16#004074,
        16#205D9E,
        16#2870BD,
        16#0090FF,
        16#3B9EFF,
        16#C2E6FF,
        16#70B8FF
    ).

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/lustre_ui/src/lustre/ui/util/colour.gleam", 928).
-spec cyan() -> scale().
cyan() ->
    from_radix_scale(
        16#FAFDFE,
        16#F2FCFD,
        16#AADEE6,
        16#E7F9FB,
        16#D8F3F6,
        16#C4EAEF,
        16#3DB9CF,
        16#84CDDA,
        16#05A2C2,
        16#0894B3,
        16#0D3C48,
        16#0C7792
    ).

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/lustre_ui/src/lustre/ui/util/colour.gleam", 945).
-spec cyan_dark() -> scale().
cyan_dark() ->
    from_radix_scale(
        16#0B161A,
        16#101B20,
        16#045468,
        16#082C36,
        16#003848,
        16#004558,
        16#12677E,
        16#11809C,
        16#00A2C7,
        16#23AFD0,
        16#B6ECF7,
        16#4CCCE6
    ).

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/lustre_ui/src/lustre/ui/util/colour.gleam", 962).
-spec teal() -> scale().
teal() ->
    from_radix_scale(
        16#FAFEFD,
        16#F1FCFA,
        16#AFDFD7,
        16#E7F9F5,
        16#D9F3EE,
        16#C7EBE5,
        16#53B9AB,
        16#8DCEC3,
        16#12A594,
        16#0E9888,
        16#0D3D38,
        16#067A6F
    ).

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/lustre_ui/src/lustre/ui/util/colour.gleam", 979).
-spec teal_dark() -> scale().
teal_dark() ->
    from_radix_scale(
        16#0D1514,
        16#111C1B,
        16#145750,
        16#0D2D2A,
        16#023B37,
        16#084843,
        16#1C6961,
        16#207E73,
        16#12A594,
        16#0EB39E,
        16#ADF0DD,
        16#0BD8B6
    ).

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/lustre_ui/src/lustre/ui/util/colour.gleam", 996).
-spec jade() -> scale().
jade() ->
    from_radix_scale(
        16#FBFEFD,
        16#EFFDF6,
        16#B0E0CC,
        16#E4FAEF,
        16#D7F4E6,
        16#C6ECDB,
        16#56BA9F,
        16#8FCFB9,
        16#29A383,
        16#259678,
        16#1D3B31,
        16#1A7A5E
    ).

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/lustre_ui/src/lustre/ui/util/colour.gleam", 1013).
-spec jade_dark() -> scale().
jade_dark() ->
    from_radix_scale(
        16#0D1512,
        16#121C18,
        16#1B5745,
        16#0F2E22,
        16#0B3B2C,
        16#114837,
        16#246854,
        16#2A7E68,
        16#29A383,
        16#27B08B,
        16#ADF0D4,
        16#1FD8A4
    ).

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/lustre_ui/src/lustre/ui/util/colour.gleam", 1030).
-spec green() -> scale().
green() ->
    from_radix_scale(
        16#FBFEFC,
        16#F2FCF5,
        16#B4DFC4,
        16#E9F9EE,
        16#DDF3E4,
        16#CCEBD7,
        16#5BB98C,
        16#92CEAC,
        16#30A46C,
        16#299764,
        16#193B2D,
        16#18794E
    ).

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/lustre_ui/src/lustre/ui/util/colour.gleam", 1047).
-spec green_dark() -> scale().
green_dark() ->
    from_radix_scale(
        16#0E1512,
        16#121B17,
        16#20573E,
        16#132D21,
        16#113B29,
        16#174933,
        16#28684A,
        16#2F7C57,
        16#30A46C,
        16#33B074,
        16#B1F1CB,
        16#3DD68C
    ).

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/lustre_ui/src/lustre/ui/util/colour.gleam", 1064).
-spec grass() -> scale().
grass() ->
    from_radix_scale(
        16#FBFEFB,
        16#F3FCF3,
        16#B7DFBA,
        16#EBF9EB,
        16#DFF3DF,
        16#CEEBCF,
        16#65BA75,
        16#97CF9C,
        16#46A758,
        16#3D9A50,
        16#203C25,
        16#297C3B
    ).

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/lustre_ui/src/lustre/ui/util/colour.gleam", 1081).
-spec grass_dark() -> scale().
grass_dark() ->
    from_radix_scale(
        16#0E1511,
        16#141A15,
        16#2D5736,
        16#1B2A1E,
        16#1D3A24,
        16#25482D,
        16#366740,
        16#3E7949,
        16#46A758,
        16#53B365,
        16#C2F0C2,
        16#71D083
    ).

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/lustre_ui/src/lustre/ui/util/colour.gleam", 1098).
-spec lime() -> scale().
lime() ->
    from_radix_scale(
        16#FCFDFA,
        16#F7FCF0,
        16#C6DE99,
        16#EDFADA,
        16#E2F5C4,
        16#D5EDAF,
        16#9AB654,
        16#B2CA7F,
        16#BDEE63,
        16#B0E64C,
        16#37401C,
        16#59682C
    ).

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/lustre_ui/src/lustre/ui/util/colour.gleam", 1115).
-spec lime_dark() -> scale().
lime_dark() ->
    from_radix_scale(
        16#11130C,
        16#151A10,
        16#3D522A,
        16#1F2917,
        16#29371D,
        16#334423,
        16#496231,
        16#577538,
        16#BDEE63,
        16#D4FF70,
        16#E3F7BA,
        16#BDE56C
    ).

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/lustre_ui/src/lustre/ui/util/colour.gleam", 1132).
-spec mint() -> scale().
mint() ->
    from_radix_scale(
        16#F9FEFD,
        16#EFFEFA,
        16#A6E1D3,
        16#DDFBF3,
        16#CCF7EC,
        16#BBEEE2,
        16#51BDA7,
        16#87D0BF,
        16#86EAD4,
        16#7FE1CC,
        16#16433C,
        16#27756A
    ).

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/lustre_ui/src/lustre/ui/util/colour.gleam", 1149).
-spec mint_dark() -> scale().
mint_dark() ->
    from_radix_scale(
        16#0E1515,
        16#0F1B1B,
        16#105650,
        16#092C2B,
        16#003A38,
        16#004744,
        16#1E685F,
        16#277F70,
        16#86EAD4,
        16#A8F5E5,
        16#C4F5E1,
        16#58D5BA
    ).

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/lustre_ui/src/lustre/ui/util/colour.gleam", 1166).
-spec sky() -> scale().
sky() ->
    from_radix_scale(
        16#F9FEFF,
        16#F1FCFF,
        16#A5DCED,
        16#E2F9FF,
        16#D2F4FD,
        16#BFEBF8,
        16#46B8D8,
        16#82CAE0,
        16#7CE2FE,
        16#72DBF8,
        16#19404D,
        16#256E93
    ).

-file("/home/raphaelac/repositories/lauraxv/client/build/packages/lustre_ui/src/lustre/ui/util/colour.gleam", 1183).
-spec sky_dark() -> scale().
sky_dark() ->
    from_radix_scale(
        16#0D141F,
        16#111A27,
        16#1B537B,
        16#112840,
        16#113555,
        16#154467,
        16#1F6692,
        16#197CAE,
        16#7CE2FE,
        16#A8EEFF,
        16#C2F3FF,
        16#75C7F0
    ).
