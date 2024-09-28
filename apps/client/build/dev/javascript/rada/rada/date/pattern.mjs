import * as $list from "../../../gleam_stdlib/gleam/list.mjs";
import * as $result from "../../../gleam_stdlib/gleam/result.mjs";
import * as $set from "../../../gleam_stdlib/gleam/set.mjs";
import * as $string from "../../../gleam_stdlib/gleam/string.mjs";
import * as $nibble from "../../../nibble/nibble.mjs";
import * as $lexer from "../../../nibble/nibble/lexer.mjs";
import {
  Ok,
  Error,
  toList,
  prepend as listPrepend,
  CustomType as $CustomType,
  makeError,
} from "../../gleam.mjs";

export class Field extends $CustomType {
  constructor(x0, x1) {
    super();
    this[0] = x0;
    this[1] = x1;
  }
}

export class Literal extends $CustomType {
  constructor(x0) {
    super();
    this[0] = x0;
  }
}

export class Alpha extends $CustomType {
  constructor(x0) {
    super();
    this[0] = x0;
  }
}

export class Quote extends $CustomType {}

export class EscapedQuote extends $CustomType {}

export class Text extends $CustomType {
  constructor(x0) {
    super();
    this[0] = x0;
  }
}

function is_alpha(token) {
  if (token instanceof Alpha) {
    return true;
  } else {
    return false;
  }
}

function is_specific_alpha(char) {
  return (token) => {
    if (token instanceof Alpha) {
      let c = token[0];
      return c === char;
    } else {
      return false;
    }
  };
}

function is_text(token) {
  if (token instanceof Text) {
    return true;
  } else {
    return false;
  }
}

function is_quote(token) {
  if (token instanceof Quote) {
    return true;
  } else {
    return false;
  }
}

function extract_content(tokens) {
  if (tokens.hasLength(0)) {
    return "";
  } else {
    let token = tokens.head;
    let rest = tokens.tail;
    if (token instanceof Alpha) {
      let str = token[0];
      return str + extract_content(rest);
    } else if (token instanceof Quote) {
      return "'" + extract_content(rest);
    } else if (token instanceof EscapedQuote) {
      return "'" + extract_content(rest);
    } else {
      let str = token[0];
      return str + extract_content(rest);
    }
  }
}

function field() {
  return $nibble.do$(
    $nibble.take_if("Expecting an Alpha token", is_alpha),
    (alpha) => {
      if (!(alpha instanceof Alpha)) {
        throw makeError(
          "let_assert",
          "rada/date/pattern",
          170,
          "",
          "Pattern match failed, no pattern matched the value.",
          { value: alpha }
        )
      }
      let char = alpha[0];
      return $nibble.do$(
        $nibble.take_while(is_specific_alpha(char)),
        (rest) => {
          return $nibble.return$(new Field(char, $list.length(rest) + 1));
        },
      );
    },
  );
}

function escaped_quote() {
  let _pipe = $nibble.token(new EscapedQuote());
  return $nibble.then$(
    _pipe,
    (_) => { return $nibble.succeed(new Literal("'")); },
  );
}

function literal() {
  return $nibble.do$(
    $nibble.take_if("Expecting an Text token", is_text),
    (text) => {
      return $nibble.do$(
        $nibble.take_while(is_text),
        (rest) => {
          let joined = (() => {
            let _pipe = $list.map(
              listPrepend(text, rest),
              (entry) => {
                if (!(entry instanceof Text)) {
                  throw makeError(
                    "let_assert",
                    "rada/date/pattern",
                    216,
                    "",
                    "Pattern match failed, no pattern matched the value.",
                    { value: entry }
                  )
                }
                let text$1 = entry[0];
                return text$1;
              },
            );
            return $string.concat(_pipe);
          })();
          return $nibble.return$(new Literal(joined));
        },
      );
    },
  );
}

function quoted_help(result) {
  return $nibble.one_of(
    toList([
      $nibble.do$(
        $nibble.take_while1(
          "Expecting a non-Quote",
          (token) => { return !is_quote(token); },
        ),
        (tokens) => {
          let str = extract_content(tokens);
          return quoted_help(result + str);
        },
      ),
      (() => {
        let _pipe = $nibble.token(new EscapedQuote());
        return $nibble.then$(
          _pipe,
          (_) => { return quoted_help(result + "'"); },
        );
      })(),
      $nibble.succeed(result),
    ]),
  );
}

function quoted() {
  return $nibble.do$(
    $nibble.take_if("Expecting an Quote", is_quote),
    (_) => {
      return $nibble.do$(
        quoted_help(""),
        (text) => {
          return $nibble.do$(
            $nibble.one_of(
              toList([
                (() => {
                  let _pipe = $nibble.take_if("Expecting an Quote", is_quote);
                  return $nibble.map(_pipe, (_) => { return undefined; });
                })(),
                $nibble.eof(),
              ]),
            ),
            (_) => { return $nibble.return$(new Literal(text)); },
          );
        },
      );
    },
  );
}

function finalize(tokens) {
  return $list.fold(
    tokens,
    toList([]),
    (tokens, token) => {
      if (token instanceof Literal &&
      tokens.atLeastLength(1) &&
      tokens.head instanceof Literal) {
        let x = token[0];
        let y = tokens.head[0];
        let rest = tokens.tail;
        return listPrepend(new Literal(x + y), rest);
      } else {
        return listPrepend(token, tokens);
      }
    },
  );
}

function parser(tokens) {
  return $nibble.one_of(
    toList([
      (() => {
        let _pipe = $nibble.one_of(
          toList([field(), literal(), escaped_quote(), quoted()]),
        );
        return $nibble.then$(
          _pipe,
          (token) => { return parser(listPrepend(token, tokens)); },
        );
      })(),
      $nibble.succeed(finalize(tokens)),
    ]),
  );
}

export function from_string(str) {
  let alpha = (() => {
    let _pipe = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ";
    let _pipe$1 = $string.to_graphemes(_pipe);
    return $set.from_list(_pipe$1);
  })();
  let is_alpha$1 = (char) => { return $set.contains(alpha, char); };
  let l = $lexer.simple(
    toList([
      $lexer.keep(
        (lexeme, _) => {
          let $ = is_alpha$1(lexeme);
          if ($) {
            return new Ok(new Alpha(lexeme));
          } else {
            return new Error(undefined);
          }
        },
      ),
      $lexer.custom(
        (mode, lexeme, next_grapheme) => {
          if (lexeme === "'") {
            if (next_grapheme === "'") {
              return new $lexer.Skip();
            } else {
              return new $lexer.Keep(new Quote(), mode);
            }
          } else if (lexeme === "''") {
            return new $lexer.Keep(new EscapedQuote(), mode);
          } else {
            return new $lexer.NoMatch();
          }
        },
      ),
      $lexer.keep(
        (lexeme, _) => {
          if (lexeme === "") {
            return new Error(undefined);
          } else {
            return new Ok(new Text(lexeme));
          }
        },
      ),
    ]),
  );
  let tokens_result = $lexer.run(str, l);
  if (tokens_result.isOk()) {
    let tokens = tokens_result[0];
    let _pipe = $nibble.run(tokens, parser(toList([])));
    return $result.unwrap(_pipe, toList([new Literal(str)]));
  } else {
    return toList([]);
  }
}
