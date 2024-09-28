import * as $list from "../../gleam_stdlib/gleam/list.mjs";
import { Ok, Error, toList, CustomType as $CustomType } from "../gleam.mjs";
import * as $nibble from "../nibble.mjs";

class Config extends $CustomType {
  constructor(one_of, and_then_one_of, spaces) {
    super();
    this.one_of = one_of;
    this.and_then_one_of = and_then_one_of;
    this.spaces = spaces;
  }
}

class Operator extends $CustomType {
  constructor(x0) {
    super();
    this[0] = x0;
  }
}

function operation(expr, config, current_precedence) {
  let _pipe = config.and_then_one_of;
  let _pipe$1 = $list.filter_map(
    _pipe,
    (operator) => {
      let op = operator[0];
      let $ = op(config);
      if ($[0] > current_precedence) {
        let precedence = $[0];
        let parser = $[1];
        return new Ok(parser(expr));
      } else {
        return new Error(undefined);
      }
    },
  );
  return $nibble.one_of(_pipe$1);
}

export function sub_expression(config, precedence) {
  let expr = $nibble.lazy(
    () => {
      let _pipe = config.one_of;
      let _pipe$1 = $list.map(_pipe, (p) => { return p(config); });
      return $nibble.one_of(_pipe$1);
    },
  );
  let go = (expr) => {
    return $nibble.do$(
      config.spaces,
      (_) => {
        return $nibble.one_of(
          toList([
            (() => {
              let _pipe = operation(expr, config, precedence);
              return $nibble.map(
                _pipe,
                (var0) => { return new $nibble.Continue(var0); },
              );
            })(),
            (() => {
              let _pipe = $nibble.return$(expr);
              return $nibble.map(
                _pipe,
                (var0) => { return new $nibble.Break(var0); },
              );
            })(),
          ]),
        );
      },
    );
  };
  return $nibble.do$(
    config.spaces,
    (_) => { return $nibble.do$(expr, (e) => { return $nibble.loop(e, go); }); },
  );
}

export function expression(first, then$, spaces) {
  let config = new Config(first, then$, spaces);
  return sub_expression(config, 0);
}

export function prefix(precedence, operator, apply) {
  return (config) => {
    return $nibble.do$(
      operator,
      (_) => {
        return $nibble.do$(
          sub_expression(config, precedence),
          (subexpr) => { return $nibble.return$(apply(subexpr)); },
        );
      },
    );
  };
}

export function postfix(precedence, operator, apply) {
  return new Operator(
    (_) => {
      return [
        precedence,
        (lhs) => {
          return $nibble.do$(
            operator,
            (_) => { return $nibble.return$(apply(lhs)); },
          );
        },
      ];
    },
  );
}

function make_infix(precedence, operator, apply) {
  let left_precedence = precedence[0];
  let right_precedence = precedence[1];
  return new Operator(
    (config) => {
      return [
        left_precedence,
        (lhs) => {
          return $nibble.do$(
            operator,
            (_) => {
              return $nibble.do$(
                sub_expression(config, right_precedence),
                (subexpr) => { return $nibble.return$(apply(lhs, subexpr)); },
              );
            },
          );
        },
      ];
    },
  );
}

export function infix_left(precedence, operator, apply) {
  return make_infix([precedence, precedence], operator, apply);
}

export function infix_right(precedence, operator, apply) {
  return make_infix([precedence, precedence - 1], operator, apply);
}
