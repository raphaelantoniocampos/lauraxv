import * as $regex from "../../../gleam_stdlib/gleam/regex.mjs";
import * as $nl from "../../../nibble/nibble/lexer.mjs";
import { toList, CustomType as $CustomType, makeError } from "../../gleam.mjs";

export class Digit extends $CustomType {
  constructor(x0) {
    super();
    this[0] = x0;
  }
}

export class WeekToken extends $CustomType {}

export class Dash extends $CustomType {}

export class TimeToken extends $CustomType {}

export class Other extends $CustomType {
  constructor(x0) {
    super();
    this[0] = x0;
  }
}

export function lexer() {
  let options = new $regex.Options(false, true);
  let $ = $regex.compile("^[0-9]+$", options);
  if (!$.isOk()) {
    throw makeError(
      "let_assert",
      "rada/date/parse",
      14,
      "lexer",
      "Pattern match failed, no pattern matched the value.",
      { value: $ }
    )
  }
  let digits_regex = $[0];
  let is_digits = (str) => { return $regex.check(digits_regex, str); };
  return $nl.simple(
    toList([
      $nl.custom(
        (mode, lexeme, _) => {
          if (lexeme === "") {
            return new $nl.Drop(mode);
          } else if (lexeme === "W") {
            return new $nl.Keep(new WeekToken(), mode);
          } else if (lexeme === "T") {
            return new $nl.Keep(new TimeToken(), mode);
          } else if (lexeme === "-") {
            return new $nl.Keep(new Dash(), mode);
          } else {
            let $1 = is_digits(lexeme);
            if ($1) {
              return new $nl.Keep(new Digit(lexeme), mode);
            } else {
              return new $nl.Keep(new Other(lexeme), mode);
            }
          }
        },
      ),
    ]),
  );
}
