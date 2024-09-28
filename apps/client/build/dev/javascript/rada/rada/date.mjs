import * as $bool from "../../gleam_stdlib/gleam/bool.mjs";
import * as $float from "../../gleam_stdlib/gleam/float.mjs";
import * as $int from "../../gleam_stdlib/gleam/int.mjs";
import * as $list from "../../gleam_stdlib/gleam/list.mjs";
import * as $option from "../../gleam_stdlib/gleam/option.mjs";
import * as $order from "../../gleam_stdlib/gleam/order.mjs";
import * as $result from "../../gleam_stdlib/gleam/result.mjs";
import * as $string from "../../gleam_stdlib/gleam/string.mjs";
import * as $nibble from "../../nibble/nibble.mjs";
import * as $nibble_lexer from "../../nibble/nibble/lexer.mjs";
import {
  Ok,
  Error,
  toList,
  prepend as listPrepend,
  CustomType as $CustomType,
  makeError,
  remainderInt,
  divideFloat,
  divideInt,
  isEqual,
} from "../gleam.mjs";
import * as $days_parse from "../rada/date/parse.mjs";
import { Dash, Digit, TimeToken, WeekToken } from "../rada/date/parse.mjs";
import * as $pattern from "../rada/date/pattern.mjs";
import { Field, Literal } from "../rada/date/pattern.mjs";
import { get_year_month_day } from "../rada_ffi.mjs";

export class Jan extends $CustomType {}

export class Feb extends $CustomType {}

export class Mar extends $CustomType {}

export class Apr extends $CustomType {}

export class May extends $CustomType {}

export class Jun extends $CustomType {}

export class Jul extends $CustomType {}

export class Aug extends $CustomType {}

export class Sep extends $CustomType {}

export class Oct extends $CustomType {}

export class Nov extends $CustomType {}

export class Dec extends $CustomType {}

export class Mon extends $CustomType {}

export class Tue extends $CustomType {}

export class Wed extends $CustomType {}

export class Thu extends $CustomType {}

export class Fri extends $CustomType {}

export class Sat extends $CustomType {}

export class Sun extends $CustomType {}

class RD extends $CustomType {
  constructor(x0) {
    super();
    this[0] = x0;
  }
}

class OrdinalDate extends $CustomType {
  constructor(year, ordinal_day) {
    super();
    this.year = year;
    this.ordinal_day = ordinal_day;
  }
}

class CalendarDate extends $CustomType {
  constructor(year, month, day) {
    super();
    this.year = year;
    this.month = month;
    this.day = day;
  }
}

class WeekDate extends $CustomType {
  constructor(week_year, week_number, weekday) {
    super();
    this.week_year = week_year;
    this.week_number = week_number;
    this.weekday = weekday;
  }
}

export class Language extends $CustomType {
  constructor(month_name, month_name_short, weekday_name, weekday_name_short, day_with_suffix) {
    super();
    this.month_name = month_name;
    this.month_name_short = month_name_short;
    this.weekday_name = weekday_name;
    this.weekday_name_short = weekday_name_short;
    this.day_with_suffix = day_with_suffix;
  }
}

class MonthAndDay extends $CustomType {
  constructor(x0, x1) {
    super();
    this[0] = x0;
    this[1] = x1;
  }
}

class WeekAndWeekday extends $CustomType {
  constructor(x0, x1) {
    super();
    this[0] = x0;
    this[1] = x1;
  }
}

class OrdinalDay extends $CustomType {
  constructor(x0) {
    super();
    this[0] = x0;
  }
}

export class Years extends $CustomType {}

export class Months extends $CustomType {}

export class Weeks extends $CustomType {}

export class Days extends $CustomType {}

export class Year extends $CustomType {}

export class Quarter extends $CustomType {}

export class Month extends $CustomType {}

export class Week extends $CustomType {}

export class Monday extends $CustomType {}

export class Tuesday extends $CustomType {}

export class Wednesday extends $CustomType {}

export class Thursday extends $CustomType {}

export class Friday extends $CustomType {}

export class Saturday extends $CustomType {}

export class Sunday extends $CustomType {}

export class Day extends $CustomType {}

export function from_rata_die(rd) {
  return new RD(rd);
}

export function to_rata_die(date) {
  let rd = date[0];
  return rd;
}

function string_take_right(str, count) {
  return $string.slice(str, -1 * count, count);
}

function string_take_left(str, count) {
  return $string.slice(str, 0, count);
}

function month_to_name(month) {
  if (month instanceof Jan) {
    return "January";
  } else if (month instanceof Feb) {
    return "February";
  } else if (month instanceof Mar) {
    return "March";
  } else if (month instanceof Apr) {
    return "April";
  } else if (month instanceof May) {
    return "May";
  } else if (month instanceof Jun) {
    return "June";
  } else if (month instanceof Jul) {
    return "July";
  } else if (month instanceof Aug) {
    return "August";
  } else if (month instanceof Sep) {
    return "September";
  } else if (month instanceof Oct) {
    return "October";
  } else if (month instanceof Nov) {
    return "November";
  } else {
    return "December";
  }
}

function weekday_to_name(weekday) {
  if (weekday instanceof Mon) {
    return "Monday";
  } else if (weekday instanceof Tue) {
    return "Tuesday";
  } else if (weekday instanceof Wed) {
    return "Wednesday";
  } else if (weekday instanceof Thu) {
    return "Thursday";
  } else if (weekday instanceof Fri) {
    return "Friday";
  } else if (weekday instanceof Sat) {
    return "Saturday";
  } else {
    return "Sunday";
  }
}

function parse_digit() {
  return $nibble.take_if(
    "Expecting digit",
    (token) => {
      if (token instanceof Digit) {
        return true;
      } else {
        return false;
      }
    },
  );
}

function int_4() {
  return $nibble.do$(
    $nibble.optional($nibble.token(new Dash())),
    (negative) => {
      let negative$1 = (() => {
        let _pipe = negative;
        let _pipe$1 = $option.map(_pipe, (_) => { return "-"; });
        return $option.unwrap(_pipe$1, "");
      })();
      return $nibble.do$(
        (() => {
          let _pipe = parse_digit();
          return $nibble.take_exactly(_pipe, 4);
        })(),
        (tokens) => {
          let str = (() => {
            let _pipe = $list.map(
              tokens,
              (token) => {
                if (!(token instanceof Digit)) {
                  throw makeError(
                    "let_assert",
                    "rada/date",
                    1091,
                    "",
                    "Pattern match failed, no pattern matched the value.",
                    { value: token }
                  )
                }
                let str = token[0];
                return str;
              },
            );
            return $string.concat(_pipe);
          })();
          let $ = $int.parse(negative$1 + str);
          if (!$.isOk()) {
            throw makeError(
              "let_assert",
              "rada/date",
              1096,
              "",
              "Pattern match failed, no pattern matched the value.",
              { value: $ }
            )
          }
          let int = $[0];
          return $nibble.return$(int);
        },
      );
    },
  );
}

function int_3() {
  return $nibble.do$(
    (() => {
      let _pipe = parse_digit();
      return $nibble.take_exactly(_pipe, 3);
    })(),
    (tokens) => {
      let str = (() => {
        let _pipe = $list.map(
          tokens,
          (token) => {
            if (!(token instanceof Digit)) {
              throw makeError(
                "let_assert",
                "rada/date",
                1109,
                "",
                "Pattern match failed, no pattern matched the value.",
                { value: token }
              )
            }
            let str = token[0];
            return str;
          },
        );
        return $string.concat(_pipe);
      })();
      let $ = $int.parse(str);
      if (!$.isOk()) {
        throw makeError(
          "let_assert",
          "rada/date",
          1114,
          "",
          "Pattern match failed, no pattern matched the value.",
          { value: $ }
        )
      }
      let int = $[0];
      return $nibble.return$(int);
    },
  );
}

function parse_ordinal_day() {
  return $nibble.do$(
    int_3(),
    (day) => { return $nibble.return$(new OrdinalDay(day)); },
  );
}

function int_2() {
  return $nibble.do$(
    (() => {
      let _pipe = parse_digit();
      return $nibble.take_exactly(_pipe, 2);
    })(),
    (tokens) => {
      let str = (() => {
        let _pipe = $list.map(
          tokens,
          (token) => {
            if (!(token instanceof Digit)) {
              throw makeError(
                "let_assert",
                "rada/date",
                1127,
                "",
                "Pattern match failed, no pattern matched the value.",
                { value: token }
              )
            }
            let str = token[0];
            return str;
          },
        );
        return $string.concat(_pipe);
      })();
      let $ = $int.parse(str);
      if (!$.isOk()) {
        throw makeError(
          "let_assert",
          "rada/date",
          1132,
          "",
          "Pattern match failed, no pattern matched the value.",
          { value: $ }
        )
      }
      let int = $[0];
      return $nibble.return$(int);
    },
  );
}

function parse_month_and_day(extended) {
  return $nibble.do$(
    int_2(),
    (month) => {
      let dash_count = $bool.to_int(extended);
      return $nibble.do$(
        $nibble.one_of(
          toList([
            (() => {
              let _pipe = $nibble.take_exactly(
                $nibble.token(new Dash()),
                dash_count,
              );
              return $nibble.then$(_pipe, (_) => { return int_2(); });
            })(),
            (() => {
              let _pipe = $nibble.eof();
              return $nibble.then$(_pipe, (_) => { return $nibble.succeed(1); });
            })(),
          ]),
        ),
        (day) => { return $nibble.return$(new MonthAndDay(month, day)); },
      );
    },
  );
}

function int_1() {
  return $nibble.do$(
    (() => {
      let _pipe = parse_digit();
      return $nibble.take_exactly(_pipe, 1);
    })(),
    (tokens) => {
      if (!tokens.hasLength(1) || !(tokens.head instanceof Digit)) {
        throw makeError(
          "let_assert",
          "rada/date",
          1143,
          "",
          "Pattern match failed, no pattern matched the value.",
          { value: tokens }
        )
      }
      let str = tokens.head[0];
      let $ = $int.parse(str);
      if (!$.isOk()) {
        throw makeError(
          "let_assert",
          "rada/date",
          1145,
          "",
          "Pattern match failed, no pattern matched the value.",
          { value: $ }
        )
      }
      let int = $[0];
      return $nibble.return$(int);
    },
  );
}

function parse_week_and_weekday(extended) {
  return $nibble.do$(
    $nibble.token(new WeekToken()),
    (_) => {
      return $nibble.do$(
        int_2(),
        (week) => {
          let dash_count = $bool.to_int(extended);
          return $nibble.do$(
            $nibble.one_of(
              toList([
                (() => {
                  let _pipe = $nibble.take_exactly(
                    $nibble.token(new Dash()),
                    dash_count,
                  );
                  return $nibble.then$(_pipe, (_) => { return int_1(); });
                })(),
                $nibble.succeed(1),
              ]),
            ),
            (day) => { return $nibble.return$(new WeekAndWeekday(week, day)); },
          );
        },
      );
    },
  );
}

function parse_day_of_year() {
  return $nibble.one_of(
    toList([
      (() => {
        let _pipe = $nibble.token(new Dash());
        return $nibble.then$(
          _pipe,
          (_) => {
            return $nibble.one_of(
              toList([
                $nibble.backtrackable(parse_ordinal_day()),
                parse_month_and_day(true),
                parse_week_and_weekday(true),
              ]),
            );
          },
        );
      })(),
      $nibble.backtrackable(parse_month_and_day(false)),
      parse_ordinal_day(),
      parse_week_and_weekday(false),
      $nibble.succeed(new OrdinalDay(1)),
    ]),
  );
}

function interval_to_units(interval) {
  if (interval instanceof Year) {
    return [1, new Years()];
  } else if (interval instanceof Quarter) {
    return [3, new Months()];
  } else if (interval instanceof Month) {
    return [1, new Months()];
  } else if (interval instanceof Day) {
    return [1, new Days()];
  } else {
    return [1, new Weeks()];
  }
}

export function compare(date1, date2) {
  let rd_1 = date1[0];
  let rd_2 = date2[0];
  return $int.compare(rd_1, rd_2);
}

export function min(date1, date2) {
  let rd_1 = date1[0];
  let rd_2 = date2[0];
  let $ = rd_1 < rd_2;
  if ($) {
    return date1;
  } else {
    return date2;
  }
}

export function max(date1, date2) {
  let rd_1 = date1[0];
  let rd_2 = date2[0];
  let $ = rd_1 < rd_2;
  if ($) {
    return date2;
  } else {
    return date1;
  }
}

export function clamp(value, lower, upper) {
  let value_rd = value[0];
  let lower_rd = lower[0];
  let upper_rd = upper[0];
  let $ = value_rd < lower_rd;
  if ($) {
    return lower;
  } else {
    let $1 = value_rd > upper_rd;
    if ($1) {
      return upper;
    } else {
      return value;
    }
  }
}

export function month_to_number(month) {
  if (month instanceof Jan) {
    return 1;
  } else if (month instanceof Feb) {
    return 2;
  } else if (month instanceof Mar) {
    return 3;
  } else if (month instanceof Apr) {
    return 4;
  } else if (month instanceof May) {
    return 5;
  } else if (month instanceof Jun) {
    return 6;
  } else if (month instanceof Jul) {
    return 7;
  } else if (month instanceof Aug) {
    return 8;
  } else if (month instanceof Sep) {
    return 9;
  } else if (month instanceof Oct) {
    return 10;
  } else if (month instanceof Nov) {
    return 11;
  } else {
    return 12;
  }
}

function month_to_quarter(month) {
  return divideInt((month_to_number(month) + 2), 3);
}

export function number_to_month(month_number) {
  let $ = $int.max(1, month_number);
  if ($ === 1) {
    return new Jan();
  } else if ($ === 2) {
    return new Feb();
  } else if ($ === 3) {
    return new Mar();
  } else if ($ === 4) {
    return new Apr();
  } else if ($ === 5) {
    return new May();
  } else if ($ === 6) {
    return new Jun();
  } else if ($ === 7) {
    return new Jul();
  } else if ($ === 8) {
    return new Aug();
  } else if ($ === 9) {
    return new Sep();
  } else if ($ === 10) {
    return new Oct();
  } else if ($ === 11) {
    return new Nov();
  } else {
    return new Dec();
  }
}

function quarter_to_month(quarter) {
  let _pipe = quarter * 3 - 2;
  return number_to_month(_pipe);
}

function weekday_to_number(weekday) {
  if (weekday instanceof Mon) {
    return 1;
  } else if (weekday instanceof Tue) {
    return 2;
  } else if (weekday instanceof Wed) {
    return 3;
  } else if (weekday instanceof Thu) {
    return 4;
  } else if (weekday instanceof Fri) {
    return 5;
  } else if (weekday instanceof Sat) {
    return 6;
  } else {
    return 7;
  }
}

export function number_to_weekday(weekday_number) {
  let $ = $int.max(1, weekday_number);
  if ($ === 1) {
    return new Mon();
  } else if ($ === 2) {
    return new Tue();
  } else if ($ === 3) {
    return new Wed();
  } else if ($ === 4) {
    return new Thu();
  } else if ($ === 5) {
    return new Fri();
  } else if ($ === 6) {
    return new Sat();
  } else {
    return new Sun();
  }
}

function pad_signed_int(value, length) {
  let prefix = (() => {
    let $ = value < 0;
    if ($) {
      return "-";
    } else {
      return "";
    }
  })();
  let suffix = (() => {
    let _pipe = value;
    let _pipe$1 = $int.absolute_value(_pipe);
    let _pipe$2 = $int.to_string(_pipe$1);
    return $string.pad_left(_pipe$2, length, "0");
  })();
  return prefix + suffix;
}

function floor_div(dividend, divisor) {
  let $ = (((dividend > 0) && (divisor < 0)) || ((dividend < 0) && (divisor > 0))) && ((remainderInt(
    dividend,
    divisor
  )) !== 0);
  if ($) {
    return (divideInt(dividend, divisor)) - 1;
  } else {
    return divideInt(dividend, divisor);
  }
}

function days_before_year(year1) {
  let year$1 = year1 - 1;
  let leap_years = (floor_div(year$1, 4) - floor_div(year$1, 100)) + floor_div(
    year$1,
    400,
  );
  return 365 * year$1 + leap_years;
}

function first_of_year(year) {
  return new RD(days_before_year(year) + 1);
}

function modulo_unwrap(dividend, divisor) {
  let remainder = remainderInt(dividend, divisor);
  let $ = ((remainder > 0) && (divisor < 0)) || ((remainder < 0) && (divisor > 0));
  if ($) {
    return remainder + divisor;
  } else {
    return remainder;
  }
}

function is_leap_year(year) {
  return ((modulo_unwrap(year, 4) === 0) && (modulo_unwrap(year, 100) !== 0)) || (modulo_unwrap(
    year,
    400,
  ) === 0);
}

export function weekday_number(date) {
  let rd = date[0];
  let $ = modulo_unwrap(rd, 7);
  if ($ === 0) {
    return 7;
  } else {
    let n = $;
    return n;
  }
}

function days_before_week_year(year) {
  let jan4 = days_before_year(year) + 4;
  return jan4 - weekday_number(new RD(jan4));
}

function is_53_week_year(year) {
  let wdn_jan1 = weekday_number(first_of_year(year));
  return (wdn_jan1 === 4) || ((wdn_jan1 === 3) && is_leap_year(year));
}

export function from_ordinal_date(year, ordinal) {
  let days_in_year = (() => {
    let $ = is_leap_year(year);
    if ($) {
      return 366;
    } else {
      return 365;
    }
  })();
  return new RD(days_before_year(year) + $int.clamp(ordinal, 1, days_in_year));
}

export function from_week_date(week_year, week_number, weekday) {
  let weeks_in_year = (() => {
    let $ = is_53_week_year(week_year);
    if ($) {
      return 53;
    } else {
      return 52;
    }
  })();
  return new RD(
    (days_before_week_year(week_year) + ($int.clamp(
      week_number,
      1,
      weeks_in_year,
    ) - 1) * 7) + weekday_to_number(weekday),
  );
}

export function weekday(date) {
  let _pipe = date;
  let _pipe$1 = weekday_number(_pipe);
  return number_to_weekday(_pipe$1);
}

function ordinal_suffix(value) {
  let value_mod_100 = modulo_unwrap(value, 100);
  let value$1 = (() => {
    let $ = value_mod_100 < 20;
    if ($) {
      return value_mod_100;
    } else {
      return modulo_unwrap(value_mod_100, 10);
    }
  })();
  let $ = $int.min(value$1, 4);
  if ($ === 1) {
    return "st";
  } else if ($ === 2) {
    return "nd";
  } else if ($ === 3) {
    return "rd";
  } else {
    return "th";
  }
}

export function with_ordinal_suffix(value) {
  return $int.to_string(value) + ordinal_suffix(value);
}

function language_en() {
  return new Language(
    month_to_name,
    (val) => {
      let _pipe = val;
      let _pipe$1 = month_to_name(_pipe);
      return string_take_left(_pipe$1, 3);
    },
    weekday_to_name,
    (val) => {
      let _pipe = val;
      let _pipe$1 = weekday_to_name(_pipe);
      return string_take_left(_pipe$1, 3);
    },
    with_ordinal_suffix,
  );
}

function days_since_previous_weekday(weekday, date) {
  return modulo_unwrap(
    (weekday_number(date) + 7) - weekday_to_number(weekday),
    7,
  );
}

function days_in_month(year, month) {
  if (month instanceof Jan) {
    return 31;
  } else if (month instanceof Feb) {
    let $ = is_leap_year(year);
    if ($) {
      return 29;
    } else {
      return 28;
    }
  } else if (month instanceof Mar) {
    return 31;
  } else if (month instanceof Apr) {
    return 30;
  } else if (month instanceof May) {
    return 31;
  } else if (month instanceof Jun) {
    return 30;
  } else if (month instanceof Jul) {
    return 31;
  } else if (month instanceof Aug) {
    return 31;
  } else if (month instanceof Sep) {
    return 30;
  } else if (month instanceof Oct) {
    return 31;
  } else if (month instanceof Nov) {
    return 30;
  } else {
    return 31;
  }
}

function to_calendar_date_helper(loop$year, loop$month, loop$ordinal_day) {
  while (true) {
    let year = loop$year;
    let month = loop$month;
    let ordinal_day = loop$ordinal_day;
    let month_days = days_in_month(year, month);
    let month_number$1 = month_to_number(month);
    let $ = (month_number$1 < 12) && (ordinal_day > month_days);
    if ($) {
      loop$year = year;
      loop$month = number_to_month(month_number$1 + 1);
      loop$ordinal_day = ordinal_day - month_days;
    } else {
      return new CalendarDate(year, month, ordinal_day);
    }
  }
}

function days_before_month(year, month) {
  let leap_days = $bool.to_int(is_leap_year(year));
  if (month instanceof Jan) {
    return 0;
  } else if (month instanceof Feb) {
    return 31;
  } else if (month instanceof Mar) {
    return 59 + leap_days;
  } else if (month instanceof Apr) {
    return 90 + leap_days;
  } else if (month instanceof May) {
    return 120 + leap_days;
  } else if (month instanceof Jun) {
    return 151 + leap_days;
  } else if (month instanceof Jul) {
    return 181 + leap_days;
  } else if (month instanceof Aug) {
    return 212 + leap_days;
  } else if (month instanceof Sep) {
    return 243 + leap_days;
  } else if (month instanceof Oct) {
    return 273 + leap_days;
  } else if (month instanceof Nov) {
    return 304 + leap_days;
  } else {
    return 334 + leap_days;
  }
}

function first_of_month(year, month) {
  return new RD((days_before_year(year) + days_before_month(year, month)) + 1);
}

export function from_calendar_date(year, month, day) {
  return new RD(
    (days_before_year(year) + days_before_month(year, month)) + $int.clamp(
      day,
      1,
      days_in_month(year, month),
    ),
  );
}

export function today() {
  let $ = get_year_month_day();
  let year$1 = $[0];
  let month_number$1 = $[1];
  let day$1 = $[2];
  return from_calendar_date(year$1, number_to_month(month_number$1), day$1);
}

function div_with_remainder(a, b) {
  return [floor_div(a, b), modulo_unwrap(a, b)];
}

export function year(date) {
  let rd = date[0];
  let $ = div_with_remainder(rd, 146_097);
  let n400 = $[0];
  let r400 = $[1];
  let $1 = div_with_remainder(r400, 36_524);
  let n100 = $1[0];
  let r100 = $1[1];
  let $2 = div_with_remainder(r100, 1461);
  let n4 = $2[0];
  let r4 = $2[1];
  let $3 = div_with_remainder(r4, 365);
  let n1 = $3[0];
  let r1 = $3[1];
  let n = (() => {
    let $4 = r1 === 0;
    if ($4) {
      return 0;
    } else {
      return 1;
    }
  })();
  return (((n400 * 400 + n100 * 100) + n4 * 4) + n1) + n;
}

function to_ordinal_date(date) {
  let rd = date[0];
  let year_ = year(date);
  return new OrdinalDate(year_, rd - days_before_year(year_));
}

function to_calendar_date(date) {
  let ordinal_date = to_ordinal_date(date);
  return to_calendar_date_helper(
    ordinal_date.year,
    new Jan(),
    ordinal_date.ordinal_day,
  );
}

function to_week_date(date) {
  let rd = date[0];
  let weekday_number_ = weekday_number(date);
  let week_year$1 = year(new RD(rd + (4 - weekday_number_)));
  let week_1_day_1 = days_before_week_year(week_year$1) + 1;
  return new WeekDate(
    week_year$1,
    1 + (divideInt((rd - week_1_day_1), 7)),
    number_to_weekday(weekday_number_),
  );
}

export function ordinal_day(date) {
  return to_ordinal_date(date).ordinal_day;
}

export function month(date) {
  return to_calendar_date(date).month;
}

export function month_number(date) {
  let _pipe = date;
  let _pipe$1 = month(_pipe);
  return month_to_number(_pipe$1);
}

export function quarter(date) {
  let _pipe = date;
  let _pipe$1 = month(_pipe);
  return month_to_quarter(_pipe$1);
}

export function day(date) {
  return to_calendar_date(date).day;
}

export function week_year(date) {
  return to_week_date(date).week_year;
}

export function week_number(date) {
  return to_week_date(date).week_number;
}

function format_field(loop$date, loop$language, loop$char, loop$length) {
  while (true) {
    let date = loop$date;
    let language = loop$language;
    let char = loop$char;
    let length = loop$length;
    if (char === "y") {
      if (length === 2) {
        let _pipe = date;
        let _pipe$1 = year(_pipe);
        let _pipe$2 = $int.to_string(_pipe$1);
        let _pipe$3 = $string.pad_left(_pipe$2, 2, "0");
        return string_take_right(_pipe$3, 2);
      } else {
        let _pipe = date;
        let _pipe$1 = year(_pipe);
        return pad_signed_int(_pipe$1, length);
      }
    } else if (char === "Y") {
      if (length === 2) {
        let _pipe = date;
        let _pipe$1 = week_year(_pipe);
        let _pipe$2 = $int.to_string(_pipe$1);
        let _pipe$3 = $string.pad_left(_pipe$2, 2, "0");
        return string_take_right(_pipe$3, 2);
      } else {
        let _pipe = date;
        let _pipe$1 = week_year(_pipe);
        return pad_signed_int(_pipe$1, length);
      }
    } else if (char === "Q") {
      if (length === 1) {
        let _pipe = date;
        let _pipe$1 = quarter(_pipe);
        return $int.to_string(_pipe$1);
      } else if (length === 2) {
        let _pipe = date;
        let _pipe$1 = quarter(_pipe);
        return $int.to_string(_pipe$1);
      } else if (length === 3) {
        let _pipe = date;
        let _pipe$1 = quarter(_pipe);
        let _pipe$2 = $int.to_string(_pipe$1);
        return ((str) => { return "Q" + str; })(_pipe$2);
      } else if (length === 4) {
        let _pipe = date;
        let _pipe$1 = quarter(_pipe);
        return with_ordinal_suffix(_pipe$1);
      } else if (length === 5) {
        let _pipe = date;
        let _pipe$1 = quarter(_pipe);
        return $int.to_string(_pipe$1);
      } else {
        return "";
      }
    } else if (char === "M") {
      if (length === 1) {
        let _pipe = date;
        let _pipe$1 = month_number(_pipe);
        return $int.to_string(_pipe$1);
      } else if (length === 2) {
        let _pipe = date;
        let _pipe$1 = month_number(_pipe);
        let _pipe$2 = $int.to_string(_pipe$1);
        return $string.pad_left(_pipe$2, 2, "0");
      } else if (length === 3) {
        let _pipe = date;
        let _pipe$1 = month(_pipe);
        return language.month_name_short(_pipe$1);
      } else if (length === 4) {
        let _pipe = date;
        let _pipe$1 = month(_pipe);
        return language.month_name(_pipe$1);
      } else if (length === 5) {
        let _pipe = date;
        let _pipe$1 = month(_pipe);
        let _pipe$2 = language.month_name_short(_pipe$1);
        return string_take_left(_pipe$2, 1);
      } else {
        return "";
      }
    } else if (char === "w") {
      if (length === 1) {
        let _pipe = date;
        let _pipe$1 = week_number(_pipe);
        return $int.to_string(_pipe$1);
      } else if (length === 2) {
        let _pipe = date;
        let _pipe$1 = week_number(_pipe);
        let _pipe$2 = $int.to_string(_pipe$1);
        return $string.pad_left(_pipe$2, 2, "0");
      } else {
        return "";
      }
    } else if (char === "d") {
      if (length === 1) {
        let _pipe = date;
        let _pipe$1 = day(_pipe);
        return $int.to_string(_pipe$1);
      } else if (length === 2) {
        let _pipe = date;
        let _pipe$1 = day(_pipe);
        let _pipe$2 = $int.to_string(_pipe$1);
        return $string.pad_left(_pipe$2, 2, "0");
      } else if (length === 3) {
        let _pipe = date;
        let _pipe$1 = day(_pipe);
        return language.day_with_suffix(_pipe$1);
      } else {
        return "";
      }
    } else if (char === "D") {
      if (length === 1) {
        let _pipe = date;
        let _pipe$1 = ordinal_day(_pipe);
        return $int.to_string(_pipe$1);
      } else if (length === 2) {
        let _pipe = date;
        let _pipe$1 = ordinal_day(_pipe);
        let _pipe$2 = $int.to_string(_pipe$1);
        return $string.pad_left(_pipe$2, 2, "0");
      } else if (length === 3) {
        let _pipe = date;
        let _pipe$1 = ordinal_day(_pipe);
        let _pipe$2 = $int.to_string(_pipe$1);
        return $string.pad_left(_pipe$2, 3, "0");
      } else {
        return "";
      }
    } else if (char === "E") {
      if (length === 1) {
        let _pipe = date;
        let _pipe$1 = weekday(_pipe);
        return language.weekday_name_short(_pipe$1);
      } else if (length === 2) {
        let _pipe = date;
        let _pipe$1 = weekday(_pipe);
        return language.weekday_name_short(_pipe$1);
      } else if (length === 3) {
        let _pipe = date;
        let _pipe$1 = weekday(_pipe);
        return language.weekday_name_short(_pipe$1);
      } else if (length === 4) {
        let _pipe = date;
        let _pipe$1 = weekday(_pipe);
        return language.weekday_name(_pipe$1);
      } else if (length === 5) {
        let _pipe = date;
        let _pipe$1 = weekday(_pipe);
        let _pipe$2 = language.weekday_name_short(_pipe$1);
        return string_take_left(_pipe$2, 1);
      } else if (length === 6) {
        let _pipe = date;
        let _pipe$1 = weekday(_pipe);
        let _pipe$2 = language.weekday_name_short(_pipe$1);
        return string_take_left(_pipe$2, 2);
      } else {
        return "";
      }
    } else if (char === "e") {
      if (length === 1) {
        let _pipe = date;
        let _pipe$1 = weekday_number(_pipe);
        return $int.to_string(_pipe$1);
      } else if (length === 2) {
        let _pipe = date;
        let _pipe$1 = weekday_number(_pipe);
        return $int.to_string(_pipe$1);
      } else {
        let _pipe = date;
        loop$date = _pipe;
        loop$language = language;
        loop$char = "E";
        loop$length = length;
      }
    } else {
      return "";
    }
  }
}

function format_with_tokens(language, tokens, date) {
  return $list.fold(
    tokens,
    "",
    (formatted, token) => {
      if (token instanceof Field) {
        let char = token[0];
        let length = token[1];
        return format_field(date, language, char, length) + formatted;
      } else {
        let str = token[0];
        return str + formatted;
      }
    },
  );
}

export function format_with_language(date, language, pattern_text) {
  let tokens = (() => {
    let _pipe = pattern_text;
    let _pipe$1 = $pattern.from_string(_pipe);
    return $list.reverse(_pipe$1);
  })();
  return format_with_tokens(language, tokens, date);
}

export function format(date, pattern) {
  return format_with_language(date, language_en(), pattern);
}

export function to_iso_string(date) {
  return format(date, "yyyy-MM-dd");
}

export function add(loop$date, loop$count, loop$unit) {
  while (true) {
    let date = loop$date;
    let count = loop$count;
    let unit = loop$unit;
    let rd = date[0];
    if (unit instanceof Years) {
      loop$date = date;
      loop$count = 12 * count;
      loop$unit = new Months();
    } else if (unit instanceof Months) {
      let calendar_date = to_calendar_date(date);
      let whole_months = (12 * (calendar_date.year - 1) + (month_to_number(
        calendar_date.month,
      ) - 1)) + count;
      let year$1 = floor_div(whole_months, 12) + 1;
      let month$1 = number_to_month(modulo_unwrap(whole_months, 12) + 1);
      return new RD(
        (days_before_year(year$1) + days_before_month(year$1, month$1)) + $int.min(
          calendar_date.day,
          days_in_month(year$1, month$1),
        ),
      );
    } else if (unit instanceof Weeks) {
      return new RD(rd + 7 * count);
    } else {
      return new RD(rd + count);
    }
  }
}

function to_months(rd) {
  let calendar_date = to_calendar_date(new RD(rd));
  let whole_months = 12 * (calendar_date.year - 1) + (month_to_number(
    calendar_date.month,
  ) - 1);
  let fraction = divideFloat($int.to_float(calendar_date.day), 100.0);
  return $int.to_float(whole_months) + fraction;
}

export function diff(unit, date1, date2) {
  let rd1 = date1[0];
  let rd2 = date2[0];
  if (unit instanceof Years) {
    let _pipe = (to_months(rd2) - to_months(rd1));
    let _pipe$1 = $float.truncate(_pipe);
    let _pipe$2 = $int.divide(_pipe$1, 12);
    return $result.unwrap(_pipe$2, 0);
  } else if (unit instanceof Months) {
    let _pipe = (to_months(rd2) - to_months(rd1));
    return $float.truncate(_pipe);
  } else if (unit instanceof Weeks) {
    let _pipe = $int.divide((rd2 - rd1), 7);
    return $result.unwrap(_pipe, 0);
  } else {
    return rd2 - rd1;
  }
}

export function floor(date, interval) {
  let rd = date[0];
  if (interval instanceof Year) {
    return first_of_year(year(date));
  } else if (interval instanceof Quarter) {
    return first_of_month(
      year(date),
      (() => {
        let _pipe = quarter(date);
        return quarter_to_month(_pipe);
      })(),
    );
  } else if (interval instanceof Month) {
    return first_of_month(year(date), month(date));
  } else if (interval instanceof Week) {
    return new RD(rd - days_since_previous_weekday(new Mon(), date));
  } else if (interval instanceof Monday) {
    return new RD(rd - days_since_previous_weekday(new Mon(), date));
  } else if (interval instanceof Tuesday) {
    return new RD(rd - days_since_previous_weekday(new Tue(), date));
  } else if (interval instanceof Wednesday) {
    return new RD(rd - days_since_previous_weekday(new Wed(), date));
  } else if (interval instanceof Thursday) {
    return new RD(rd - days_since_previous_weekday(new Thu(), date));
  } else if (interval instanceof Friday) {
    return new RD(rd - days_since_previous_weekday(new Fri(), date));
  } else if (interval instanceof Saturday) {
    return new RD(rd - days_since_previous_weekday(new Sat(), date));
  } else if (interval instanceof Sunday) {
    return new RD(rd - days_since_previous_weekday(new Sun(), date));
  } else {
    return date;
  }
}

export function ceiling(date, interval) {
  let floored_date = floor(date, interval);
  let $ = isEqual(date, floored_date);
  if ($) {
    return date;
  } else {
    let $1 = interval_to_units(interval);
    let n = $1[0];
    let unit = $1[1];
    return add(floored_date, n, unit);
  }
}

function range_help(
  loop$unit,
  loop$step,
  loop$until_rd,
  loop$reversed_list,
  loop$current_rd
) {
  while (true) {
    let unit = loop$unit;
    let step = loop$step;
    let until_rd = loop$until_rd;
    let reversed_list = loop$reversed_list;
    let current_rd = loop$current_rd;
    let $ = current_rd < until_rd;
    if ($) {
      let $1 = add(new RD(current_rd), step, unit);
      let next_rd = $1[0];
      loop$unit = unit;
      loop$step = step;
      loop$until_rd = until_rd;
      loop$reversed_list = listPrepend(new RD(current_rd), reversed_list);
      loop$current_rd = next_rd;
    } else {
      return $list.reverse(reversed_list);
    }
  }
}

export function range(interval, step, start_date, until_date) {
  let $ = interval_to_units(interval);
  let n = $[0];
  let unit = $[1];
  let $1 = ceiling(start_date, interval);
  let first_rd = $1[0];
  let until_rd = until_date[0];
  let $2 = first_rd < until_rd;
  if ($2) {
    return range_help(
      unit,
      $int.max(1, step * n),
      until_rd,
      toList([]),
      first_rd,
    );
  } else {
    return toList([]);
  }
}

function is_between_int(value, lower, upper) {
  return (lower <= value) && (value <= upper);
}

function from_ordinal_parts(year, ordinal) {
  let days_in_year = (() => {
    let $ = is_leap_year(year);
    if ($) {
      return 366;
    } else {
      return 365;
    }
  })();
  let $ = !is_between_int(ordinal, 1, days_in_year);
  if ($) {
    return new Error(
      ((("Invalid ordinal date: " + (("ordinal-day " + $int.to_string(ordinal)) + " is out of range")) + ((" (1 to " + $int.to_string(
        days_in_year,
      )) + ")")) + (" for " + $int.to_string(year))) + (((("; received (year " + $int.to_string(
        year,
      )) + ", ordinal-day ") + $int.to_string(ordinal)) + ")"),
    );
  } else {
    return new Ok(new RD(days_before_year(year) + ordinal));
  }
}

function from_calendar_parts(year, month_number, day) {
  let $ = is_between_int(month_number, 1, 12);
  let $1 = is_between_int(
    day,
    1,
    days_in_month(year, number_to_month(month_number)),
  );
  if (!$) {
    return new Error(
      (("Invalid date: " + (("month " + $int.to_string(month_number)) + " is out of range")) + " (1 to 12)") + (((((("; received (year " + $int.to_string(
        year,
      )) + ", month ") + $int.to_string(month_number)) + ", day ") + $int.to_string(
        day,
      )) + ")"),
    );
  } else if ($ && !$1) {
    return new Error(
      (((("Invalid date: " + (("day " + $int.to_string(day)) + " is out of range")) + ((" (1 to " + $int.to_string(
        days_in_month(year, number_to_month(month_number)),
      )) + ")")) + (" for " + (() => {
        let _pipe = month_number;
        let _pipe$1 = number_to_month(_pipe);
        return month_to_name(_pipe$1);
      })())) + (() => {
        let $2 = (month_number === 2) && (day === 29);
        if ($2) {
          return (" (" + $int.to_string(year)) + " is not a leap year)";
        } else {
          return "";
        }
      })()) + (((((("; received (year " + $int.to_string(year)) + ", month ") + $int.to_string(
        month_number,
      )) + ", day ") + $int.to_string(day)) + ")"),
    );
  } else {
    return new Ok(
      new RD(
        (days_before_year(year) + days_before_month(
          year,
          number_to_month(month_number),
        )) + day,
      ),
    );
  }
}

function from_week_parts(week_year, week_number, weekday_number) {
  let weeks_in_year = (() => {
    let $ = is_53_week_year(week_year);
    if ($) {
      return 53;
    } else {
      return 52;
    }
  })();
  let $ = is_between_int(week_number, 1, weeks_in_year);
  let $1 = is_between_int(weekday_number, 1, 7);
  if (!$) {
    return new Error(
      ((("Invalid week date: " + (("week " + $int.to_string(week_number)) + " is out of range")) + ((" (1 to " + $int.to_string(
        weeks_in_year,
      )) + ")")) + (" for " + $int.to_string(week_year))) + (((((("; received (year " + $int.to_string(
        week_year,
      )) + ", week ") + $int.to_string(week_number)) + ", weekday ") + $int.to_string(
        weekday_number,
      )) + ")"),
    );
  } else if ($ && !$1) {
    return new Error(
      (("Invalid week date: " + (("weekday " + $int.to_string(weekday_number)) + " is out of range")) + " (1 to 7)") + (((((("; received (year " + $int.to_string(
        week_year,
      )) + ", week ") + $int.to_string(week_number)) + ", weekday ") + $int.to_string(
        weekday_number,
      )) + ")"),
    );
  } else {
    return new Ok(
      new RD(
        (days_before_week_year(week_year) + (week_number - 1) * 7) + weekday_number,
      ),
    );
  }
}

function from_year_and_day_of_year(year, day_of_year) {
  if (day_of_year instanceof MonthAndDay) {
    let month_number$1 = day_of_year[0];
    let day$1 = day_of_year[1];
    return from_calendar_parts(year, month_number$1, day$1);
  } else if (day_of_year instanceof WeekAndWeekday) {
    let week_number$1 = day_of_year[0];
    let weekday_number$1 = day_of_year[1];
    return from_week_parts(year, week_number$1, weekday_number$1);
  } else {
    let ordinal_day$1 = day_of_year[0];
    return from_ordinal_parts(year, ordinal_day$1);
  }
}

function parser() {
  return $nibble.do$(
    int_4(),
    (year) => {
      return $nibble.do$(
        parse_day_of_year(),
        (day_of_year) => {
          return $nibble.return$(from_year_and_day_of_year(year, day_of_year));
        },
      );
    },
  );
}

export function from_iso_string(str) {
  let $ = $nibble_lexer.run(str, $days_parse.lexer());
  if (!$.isOk()) {
    throw makeError(
      "let_assert",
      "rada/date",
      950,
      "from_iso_string",
      "Pattern match failed, no pattern matched the value.",
      { value: $ }
    )
  }
  let tokens = $[0];
  let result = $nibble.run(
    tokens,
    (() => {
      let _pipe = parser();
      return $nibble.then$(
        _pipe,
        (val) => {
          return $nibble.one_of(
            toList([
              (() => {
                let _pipe$1 = $nibble.eof();
                return $nibble.then$(
                  _pipe$1,
                  (_) => { return $nibble.succeed(val); },
                );
              })(),
              (() => {
                let _pipe$1 = $nibble.token(new TimeToken());
                return $nibble.then$(
                  _pipe$1,
                  (_) => {
                    return $nibble.succeed(
                      new Error("Expected a date only, not a date and time"),
                    );
                  },
                );
              })(),
              $nibble.succeed(new Error("Expected a date only")),
            ]),
          );
        },
      );
    })(),
  );
  if (result.isOk() && result[0].isOk()) {
    let value = result[0][0];
    return new Ok(value);
  } else if (result.isOk() && !result[0].isOk()) {
    let err = result[0][0];
    return new Error(err);
  } else {
    return new Error("Expected a date in ISO 8601 format");
  }
}

export function is_between(value, lower, upper) {
  let value_rd = value[0];
  let lower_rd = lower[0];
  let upper_rd = upper[0];
  return is_between_int(value_rd, lower_rd, upper_rd);
}
