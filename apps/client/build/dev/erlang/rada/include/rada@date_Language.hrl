-record(language, {
    month_name :: fun((rada@date:month()) -> binary()),
    month_name_short :: fun((rada@date:month()) -> binary()),
    weekday_name :: fun((rada@date:weekday()) -> binary()),
    weekday_name_short :: fun((rada@date:weekday()) -> binary()),
    day_with_suffix :: fun((integer()) -> binary())
}).
