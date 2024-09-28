import * as $float from "../../gleam_stdlib/gleam/float.mjs";
import * as $function from "../../gleam_stdlib/gleam/function.mjs";
import * as $int from "../../gleam_stdlib/gleam/int.mjs";
import * as $list from "../../gleam_stdlib/gleam/list.mjs";
import * as $regex from "../../gleam_stdlib/gleam/regex.mjs";
import * as $result from "../../gleam_stdlib/gleam/result.mjs";
import * as $set from "../../gleam_stdlib/gleam/set.mjs";
import * as $string from "../../gleam_stdlib/gleam/string.mjs";
import {
  Ok,
  Error,
  toList,
  prepend as listPrepend,
  CustomType as $CustomType,
  makeError,
} from "../gleam.mjs";

class Matcher extends $CustomType {
  constructor(run) {
    super();
    this.run = run;
  }
}

export class Keep extends $CustomType {
  constructor(x0, x1) {
    super();
    this[0] = x0;
    this[1] = x1;
  }
}

export class Skip extends $CustomType {}

export class Drop extends $CustomType {
  constructor(x0) {
    super();
    this[0] = x0;
  }
}

export class NoMatch extends $CustomType {}

export class Token extends $CustomType {
  constructor(span, lexeme, value) {
    super();
    this.span = span;
    this.lexeme = lexeme;
    this.value = value;
  }
}

export class Span extends $CustomType {
  constructor(row_start, col_start, row_end, col_end) {
    super();
    this.row_start = row_start;
    this.col_start = col_start;
    this.row_end = row_end;
    this.col_end = col_end;
  }
}

export class NoMatchFound extends $CustomType {
  constructor(row, col, lexeme) {
    super();
    this.row = row;
    this.col = col;
    this.lexeme = lexeme;
  }
}

class Lexer extends $CustomType {
  constructor(matchers) {
    super();
    this.matchers = matchers;
  }
}

class State extends $CustomType {
  constructor(source, tokens, current, row, col) {
    super();
    this.source = source;
    this.tokens = tokens;
    this.current = current;
    this.row = row;
    this.col = col;
  }
}

export function simple(matchers) {
  return new Lexer((_) => { return matchers; });
}

export function advanced(matchers) {
  return new Lexer((mode) => { return matchers(mode); });
}

export function keep(f) {
  return new Matcher(
    (mode, lexeme, lookahead) => {
      let _pipe = f(lexeme, lookahead);
      let _pipe$1 = $result.map(
        _pipe,
        (_capture) => { return new Keep(_capture, mode); },
      );
      return $result.unwrap(_pipe$1, new NoMatch());
    },
  );
}

export function drop(f) {
  return new Matcher(
    (mode, lexeme, lookahead) => {
      let $ = f(lexeme, lookahead);
      if ($) {
        return new Drop(mode);
      } else {
        return new NoMatch();
      }
    },
  );
}

export function custom(f) {
  return new Matcher(f);
}

export function map(matcher, f) {
  return new Matcher(
    (mode, lexeme, lookahead) => {
      let $ = matcher.run(mode, lexeme, lookahead);
      if ($ instanceof Keep) {
        let value = $[0];
        let mode$1 = $[1];
        return new Keep(f(value), mode$1);
      } else if ($ instanceof Skip) {
        return new Skip();
      } else if ($ instanceof Drop) {
        let mode$1 = $[0];
        return new Drop(mode$1);
      } else {
        return new NoMatch();
      }
    },
  );
}

export function then$(matcher, f) {
  return new Matcher(
    (mode, lexeme, lookahead) => {
      let $ = matcher.run(mode, lexeme, lookahead);
      if ($ instanceof Keep) {
        let value = $[0];
        return f(value);
      } else if ($ instanceof Skip) {
        return new Skip();
      } else if ($ instanceof Drop) {
        let mode$1 = $[0];
        return new Drop(mode$1);
      } else {
        return new NoMatch();
      }
    },
  );
}

export function into(matcher, f) {
  return new Matcher(
    (mode, lexeme, lookahead) => {
      let $ = matcher.run(mode, lexeme, lookahead);
      if ($ instanceof Keep) {
        let value = $[0];
        let mode$1 = $[1];
        return new Keep(value, f(mode$1));
      } else if ($ instanceof Skip) {
        return new Skip();
      } else if ($ instanceof Drop) {
        let mode$1 = $[0];
        return new Drop(f(mode$1));
      } else {
        return new NoMatch();
      }
    },
  );
}

export function ignore(matcher) {
  return new Matcher(
    (mode, lexeme, lookahead) => {
      let $ = matcher.run(mode, lexeme, lookahead);
      if ($ instanceof Keep) {
        let mode$1 = $[1];
        return new Drop(mode$1);
      } else if ($ instanceof Skip) {
        return new Skip();
      } else if ($ instanceof Drop) {
        let mode$1 = $[0];
        return new Drop(mode$1);
      } else {
        return new NoMatch();
      }
    },
  );
}

export function token(str, value) {
  return new Matcher(
    (mode, lexeme, _) => {
      let $ = lexeme === str;
      if ($) {
        return new Keep(value, mode);
      } else {
        return new NoMatch();
      }
    },
  );
}

export function symbol(str, breaker, value) {
  let $ = $regex.from_string(breaker);
  if (!$.isOk()) {
    throw makeError(
      "let_assert",
      "nibble/lexer",
      246,
      "symbol",
      "Pattern match failed, no pattern matched the value.",
      { value: $ }
    )
  }
  let break$ = $[0];
  return new Matcher(
    (mode, lexeme, lookahead) => {
      let $1 = (lexeme === str) && ((lookahead === "") || $regex.check(
        break$,
        lookahead,
      ));
      if ($1) {
        return new Keep(value, mode);
      } else {
        return new NoMatch();
      }
    },
  );
}

export function keyword(str, breaker, value) {
  let $ = $regex.from_string(breaker);
  if (!$.isOk()) {
    throw makeError(
      "let_assert",
      "nibble/lexer",
      262,
      "keyword",
      "Pattern match failed, no pattern matched the value.",
      { value: $ }
    )
  }
  let break$ = $[0];
  return new Matcher(
    (mode, lexeme, lookahead) => {
      let $1 = (lexeme === str) && ((lookahead === "") || $regex.check(
        break$,
        lookahead,
      ));
      if ($1) {
        return new Keep(value, mode);
      } else {
        return new NoMatch();
      }
    },
  );
}

export function string(char, to_value) {
  let $ = $regex.from_string(
    ((((("^" + char) + "([^") + char) + "\\\\]|\\\\[\\s\\S])*") + char) + "$",
  );
  if (!$.isOk()) {
    throw makeError(
      "let_assert",
      "nibble/lexer",
      389,
      "string",
      "Pattern match failed, no pattern matched the value.",
      { value: $ }
    )
  }
  let is_string = $[0];
  return new Matcher(
    (mode, lexeme, _) => {
      let $1 = $regex.check(is_string, lexeme);
      if ($1) {
        let _pipe = lexeme;
        let _pipe$1 = $string.drop_left(_pipe, 1);
        let _pipe$2 = $string.drop_right(_pipe$1, 1);
        let _pipe$3 = to_value(_pipe$2);
        return new Keep(_pipe$3, mode);
      } else {
        return new NoMatch();
      }
    },
  );
}

export function identifier(start, inner, reserved, to_value) {
  let $ = $regex.from_string((("^" + start) + inner) + "*$");
  if (!$.isOk()) {
    throw makeError(
      "let_assert",
      "nibble/lexer",
      414,
      "identifier",
      "Pattern match failed, no pattern matched the value.",
      { value: $ }
    )
  }
  let ident = $[0];
  let $1 = $regex.from_string(inner);
  if (!$1.isOk()) {
    throw makeError(
      "let_assert",
      "nibble/lexer",
      415,
      "identifier",
      "Pattern match failed, no pattern matched the value.",
      { value: $1 }
    )
  }
  let inner$1 = $1[0];
  return new Matcher(
    (mode, lexeme, lookahead) => {
      let $2 = $regex.check(inner$1, lookahead);
      let $3 = $regex.check(ident, lexeme);
      if ($2 && $3) {
        return new Skip();
      } else if (!$2 && $3) {
        let $4 = $set.contains(reserved, lexeme);
        if ($4) {
          return new NoMatch();
        } else {
          return new Keep(to_value(lexeme), mode);
        }
      } else {
        return new NoMatch();
      }
    },
  );
}

export function try_identifier(start, inner, reserved, to_value) {
  return $result.then$(
    $regex.from_string((("^" + start) + inner) + "*$"),
    (ident) => {
      return $result.map(
        $regex.from_string(inner),
        (inner) => {
          return new Matcher(
            (mode, lexeme, lookahead) => {
              let $ = $regex.check(inner, lookahead);
              let $1 = $regex.check(ident, lexeme);
              if ($ && $1) {
                return new Skip();
              } else if (!$ && $1) {
                let $2 = $set.contains(reserved, lexeme);
                if ($2) {
                  return new NoMatch();
                } else {
                  return new Keep(to_value(lexeme), mode);
                }
              } else {
                return new NoMatch();
              }
            },
          );
        },
      );
    },
  );
}

export function variable(reserved, to_value) {
  return identifier("[a-z]", "[a-zA-Z0-9_]", reserved, to_value);
}

export function spaces_(to_value) {
  let $ = $regex.from_string("^[ \\t]+");
  if (!$.isOk()) {
    throw makeError(
      "let_assert",
      "nibble/lexer",
      472,
      "spaces_",
      "Pattern match failed, no pattern matched the value.",
      { value: $ }
    )
  }
  let spaces$1 = $[0];
  return new Matcher(
    (mode, lexeme, _) => {
      let $1 = $regex.check(spaces$1, lexeme);
      if ($1) {
        return new Keep(to_value(lexeme), mode);
      } else {
        return new NoMatch();
      }
    },
  );
}

export function spaces(token) {
  return spaces_($function.constant(token));
}

export function whitespace(token) {
  let $ = $regex.from_string("^\\s+$");
  if (!$.isOk()) {
    throw makeError(
      "let_assert",
      "nibble/lexer",
      485,
      "whitespace",
      "Pattern match failed, no pattern matched the value.",
      { value: $ }
    )
  }
  let whitespace$1 = $[0];
  return new Matcher(
    (mode, lexeme, _) => {
      let $1 = $regex.check(whitespace$1, lexeme);
      if ($1) {
        return new Keep(token, mode);
      } else {
        return new NoMatch();
      }
    },
  );
}

export function comment(start, to_value) {
  let drop_length = $string.length(start);
  return new Matcher(
    (mode, lexeme, lookahead) => {
      let $ = $string.starts_with(lexeme, start);
      if ($ && lookahead === "\n") {
        let _pipe = lexeme;
        let _pipe$1 = $string.drop_left(_pipe, drop_length);
        let _pipe$2 = to_value(_pipe$1);
        return new Keep(_pipe$2, mode);
      } else if ($) {
        return new Skip();
      } else {
        return new NoMatch();
      }
    },
  );
}

function do_match(mode, str, lookahead, matchers) {
  return $list.fold_until(
    matchers,
    new NoMatch(),
    (_, matcher) => {
      let $ = matcher.run(mode, str, lookahead);
      if ($ instanceof Keep) {
        let match = $;
        return new $list.Stop(match);
      } else if ($ instanceof Skip) {
        return new $list.Stop(new Skip());
      } else if ($ instanceof Drop) {
        let match = $;
        return new $list.Stop(match);
      } else {
        return new $list.Continue(new NoMatch());
      }
    },
  );
}

function next_col(col, str) {
  if (str === "\n") {
    return 1;
  } else {
    return col + 1;
  }
}

function next_row(row, str) {
  if (str === "\n") {
    return row + 1;
  } else {
    return row;
  }
}

function do_run(loop$lexer, loop$mode, loop$state) {
  while (true) {
    let lexer = loop$lexer;
    let mode = loop$mode;
    let state = loop$state;
    let matchers = lexer.matchers(mode);
    let $ = state.source;
    let $1 = state.current;
    if ($.hasLength(0) && $1[2] === "") {
      return new Ok($list.reverse(state.tokens));
    } else if ($.hasLength(0)) {
      let start_row = $1[0];
      let start_col = $1[1];
      let lexeme = $1[2];
      let $2 = do_match(mode, lexeme, "", matchers);
      if ($2 instanceof NoMatch) {
        return new Error(new NoMatchFound(start_row, start_col, lexeme));
      } else if ($2 instanceof Skip) {
        return new Error(new NoMatchFound(start_row, start_col, lexeme));
      } else if ($2 instanceof Drop) {
        return new Ok($list.reverse(state.tokens));
      } else {
        let value = $2[0];
        let span = new Span(start_row, start_col, state.row, state.col);
        let token$1 = new Token(span, lexeme, value);
        return new Ok($list.reverse(listPrepend(token$1, state.tokens)));
      }
    } else {
      let lookahead = $.head;
      let rest = $.tail;
      let start_row = $1[0];
      let start_col = $1[1];
      let lexeme = $1[2];
      let row = next_row(state.row, lookahead);
      let col = next_col(state.col, lookahead);
      let $2 = do_match(mode, lexeme, lookahead, matchers);
      if ($2 instanceof Keep) {
        let value = $2[0];
        let mode$1 = $2[1];
        let span = new Span(start_row, start_col, state.row, state.col);
        let token$1 = new Token(span, lexeme, value);
        loop$lexer = lexer;
        loop$mode = mode$1;
        loop$state = new State(
          rest,
          listPrepend(token$1, state.tokens),
          [state.row, state.col, lookahead],
          row,
          col,
        );
      } else if ($2 instanceof Skip) {
        loop$lexer = lexer;
        loop$mode = mode;
        loop$state = new State(
          rest,
          state.tokens,
          [start_row, start_col, lexeme + lookahead],
          row,
          col,
        );
      } else if ($2 instanceof Drop) {
        let mode$1 = $2[0];
        loop$lexer = lexer;
        loop$mode = mode$1;
        loop$state = new State(
          rest,
          state.tokens,
          [state.row, state.col, lookahead],
          row,
          col,
        );
      } else {
        loop$lexer = lexer;
        loop$mode = mode;
        loop$state = new State(
          rest,
          state.tokens,
          [start_row, start_col, lexeme + lookahead],
          row,
          col,
        );
      }
    }
  }
}

export function run(source, lexer) {
  let _pipe = $string.to_graphemes(source);
  let _pipe$1 = new State(_pipe, toList([]), [1, 1, ""], 1, 1);
  return ((_capture) => { return do_run(lexer, undefined, _capture); })(_pipe$1);
}

export function run_advanced(source, mode, lexer) {
  return do_run(
    lexer,
    mode,
    new State($string.to_graphemes(source), toList([]), [1, 1, ""], 1, 1),
  );
}

export function float_with_separator(separator, to_value) {
  let $ = $regex.from_string(("[0-9" + separator) + "]");
  if (!$.isOk()) {
    throw makeError(
      "let_assert",
      "nibble/lexer",
      313,
      "float_with_separator",
      "Pattern match failed, no pattern matched the value.",
      { value: $ }
    )
  }
  let digit = $[0];
  let $1 = $regex.from_string(("^-*[0-9" + separator) + "]+$");
  if (!$1.isOk()) {
    throw makeError(
      "let_assert",
      "nibble/lexer",
      314,
      "float_with_separator",
      "Pattern match failed, no pattern matched the value.",
      { value: $1 }
    )
  }
  let integer = $1[0];
  let $2 = $regex.from_string(
    ((("^-*[0-9" + separator) + "]+\\.[0-9") + separator) + "]+$",
  );
  if (!$2.isOk()) {
    throw makeError(
      "let_assert",
      "nibble/lexer",
      315,
      "float_with_separator",
      "Pattern match failed, no pattern matched the value.",
      { value: $2 }
    )
  }
  let number$1 = $2[0];
  return new Matcher(
    (mode, lexeme, lookahead) => {
      let is_int = !$regex.check(digit, lookahead) && $regex.check(
        integer,
        lexeme,
      );
      let is_float = !$regex.check(digit, lookahead) && $regex.check(
        number$1,
        lexeme,
      );
      if (lexeme === "." && (is_int)) {
        return new NoMatch();
      } else if (is_float) {
        let $3 = (() => {
          let _pipe = lexeme;
          let _pipe$1 = $string.replace(_pipe, separator, "");
          return $float.parse(_pipe$1);
        })();
        if (!$3.isOk()) {
          throw makeError(
            "let_assert",
            "nibble/lexer",
            328,
            "",
            "Pattern match failed, no pattern matched the value.",
            { value: $3 }
          )
        }
        let num = $3[0];
        return new Keep(to_value(num), mode);
      } else {
        return new NoMatch();
      }
    },
  );
}

export function float(to_value) {
  return float_with_separator("", to_value);
}

export function int_with_separator(separator, to_value) {
  let $ = $regex.from_string(("[0-9" + separator) + "]");
  if (!$.isOk()) {
    throw makeError(
      "let_assert",
      "nibble/lexer",
      284,
      "int_with_separator",
      "Pattern match failed, no pattern matched the value.",
      { value: $ }
    )
  }
  let digit = $[0];
  let $1 = $regex.from_string(("^-*[0-9" + separator) + "]+$");
  if (!$1.isOk()) {
    throw makeError(
      "let_assert",
      "nibble/lexer",
      285,
      "int_with_separator",
      "Pattern match failed, no pattern matched the value.",
      { value: $1 }
    )
  }
  let integer = $1[0];
  return new Matcher(
    (mode, lexeme, lookahead) => {
      let $2 = !$regex.check(digit, lookahead) && $regex.check(integer, lexeme);
      if (!$2) {
        return new NoMatch();
      } else {
        let $3 = (() => {
          let _pipe = lexeme;
          let _pipe$1 = $string.replace(_pipe, separator, "");
          return $int.parse(_pipe$1);
        })();
        if (!$3.isOk()) {
          throw makeError(
            "let_assert",
            "nibble/lexer",
            292,
            "",
            "Pattern match failed, no pattern matched the value.",
            { value: $3 }
          )
        }
        let num = $3[0];
        return new Keep(to_value(num), mode);
      }
    },
  );
}

export function int(to_value) {
  return int_with_separator("", to_value);
}

export function number_with_separator(separator, from_int, from_float) {
  let $ = $regex.from_string(("[0-9" + separator) + "]");
  if (!$.isOk()) {
    throw makeError(
      "let_assert",
      "nibble/lexer",
      351,
      "number_with_separator",
      "Pattern match failed, no pattern matched the value.",
      { value: $ }
    )
  }
  let digit = $[0];
  let $1 = $regex.from_string(("^-*[0-9" + separator) + "]+$");
  if (!$1.isOk()) {
    throw makeError(
      "let_assert",
      "nibble/lexer",
      352,
      "number_with_separator",
      "Pattern match failed, no pattern matched the value.",
      { value: $1 }
    )
  }
  let integer = $1[0];
  let $2 = $regex.from_string(
    ((("^-*[0-9" + separator) + "]+\\.[0-9") + separator) + "]+$",
  );
  if (!$2.isOk()) {
    throw makeError(
      "let_assert",
      "nibble/lexer",
      353,
      "number_with_separator",
      "Pattern match failed, no pattern matched the value.",
      { value: $2 }
    )
  }
  let number$1 = $2[0];
  return new Matcher(
    (mode, lexeme, lookahead) => {
      let is_int = !$regex.check(digit, lookahead) && $regex.check(
        integer,
        lexeme,
      );
      let is_float = !$regex.check(digit, lookahead) && $regex.check(
        number$1,
        lexeme,
      );
      if (lexeme === "." && (is_int)) {
        return new NoMatch();
      } else if (lookahead === "." && (is_int)) {
        return new NoMatch();
      } else if (is_int) {
        let $3 = (() => {
          let _pipe = lexeme;
          let _pipe$1 = $string.replace(_pipe, separator, "");
          return $int.parse(_pipe$1);
        })();
        if (!$3.isOk()) {
          throw makeError(
            "let_assert",
            "nibble/lexer",
            367,
            "",
            "Pattern match failed, no pattern matched the value.",
            { value: $3 }
          )
        }
        let num = $3[0];
        return new Keep(from_int(num), mode);
      } else if (is_float) {
        let $3 = (() => {
          let _pipe = lexeme;
          let _pipe$1 = $string.replace(_pipe, separator, "");
          return $float.parse(_pipe$1);
        })();
        if (!$3.isOk()) {
          throw makeError(
            "let_assert",
            "nibble/lexer",
            375,
            "",
            "Pattern match failed, no pattern matched the value.",
            { value: $3 }
          )
        }
        let num = $3[0];
        return new Keep(from_float(num), mode);
      } else {
        return new NoMatch();
      }
    },
  );
}

export function number(from_int, from_float) {
  return number_with_separator("", from_int, from_float);
}
