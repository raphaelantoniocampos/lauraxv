import * as $bool from "../gleam_stdlib/gleam/bool.mjs";
import * as $dict from "../gleam_stdlib/gleam/dict.mjs";
import * as $io from "../gleam_stdlib/gleam/io.mjs";
import * as $list from "../gleam_stdlib/gleam/list.mjs";
import * as $option from "../gleam_stdlib/gleam/option.mjs";
import { None, Some } from "../gleam_stdlib/gleam/option.mjs";
import * as $string from "../gleam_stdlib/gleam/string.mjs";
import {
  Ok,
  Error,
  toList,
  prepend as listPrepend,
  CustomType as $CustomType,
  isEqual,
} from "./gleam.mjs";
import * as $lexer from "./nibble/lexer.mjs";
import { Span, Token } from "./nibble/lexer.mjs";

class Parser extends $CustomType {
  constructor(x0) {
    super();
    this[0] = x0;
  }
}

class Cont extends $CustomType {
  constructor(x0, x1, x2) {
    super();
    this[0] = x0;
    this[1] = x1;
    this[2] = x2;
  }
}

class Fail extends $CustomType {
  constructor(x0, x1) {
    super();
    this[0] = x0;
    this[1] = x1;
  }
}

class State extends $CustomType {
  constructor(src, idx, pos, ctx) {
    super();
    this.src = src;
    this.idx = idx;
    this.pos = pos;
    this.ctx = ctx;
  }
}

class CanBacktrack extends $CustomType {
  constructor(x0) {
    super();
    this[0] = x0;
  }
}

export class Continue extends $CustomType {
  constructor(x0) {
    super();
    this[0] = x0;
  }
}

export class Break extends $CustomType {
  constructor(x0) {
    super();
    this[0] = x0;
  }
}

export class BadParser extends $CustomType {
  constructor(x0) {
    super();
    this[0] = x0;
  }
}

export class Custom extends $CustomType {
  constructor(x0) {
    super();
    this[0] = x0;
  }
}

export class EndOfInput extends $CustomType {}

export class Expected extends $CustomType {
  constructor(x0, got) {
    super();
    this[0] = x0;
    this.got = got;
  }
}

export class Unexpected extends $CustomType {
  constructor(x0) {
    super();
    this[0] = x0;
  }
}

export class DeadEnd extends $CustomType {
  constructor(pos, problem, context) {
    super();
    this.pos = pos;
    this.problem = problem;
    this.context = context;
  }
}

class Empty extends $CustomType {}

class Cons extends $CustomType {
  constructor(x0, x1) {
    super();
    this[0] = x0;
    this[1] = x1;
  }
}

class Append extends $CustomType {
  constructor(x0, x1) {
    super();
    this[0] = x0;
    this[1] = x1;
  }
}

function runwrap(state, parser) {
  let parse = parser[0];
  return parse(state);
}

function next(state) {
  let $ = $dict.get(state.src, state.idx);
  if (!$.isOk()) {
    return [new $option.None(), state];
  } else {
    let span$1 = $[0].span;
    let tok = $[0].value;
    return [
      new $option.Some(tok),
      state.withFields({ idx: state.idx + 1, pos: span$1 }),
    ];
  }
}

export function return$(value) {
  return new Parser(
    (state) => { return new Cont(new CanBacktrack(false), value, state); },
  );
}

export function succeed(value) {
  return return$(value);
}

export function lazy(parser) {
  return new Parser((state) => { return runwrap(state, parser()); });
}

export function backtrackable(parser) {
  return new Parser(
    (state) => {
      let $ = runwrap(state, parser);
      if ($ instanceof Cont) {
        let a = $[1];
        let state$1 = $[2];
        return new Cont(new CanBacktrack(false), a, state$1);
      } else {
        let bag = $[1];
        return new Fail(new CanBacktrack(false), bag);
      }
    },
  );
}

function should_commit(a, b) {
  let a$1 = a[0];
  let b$1 = b[0];
  return new CanBacktrack(a$1 || b$1);
}

export function do$(parser, f) {
  return new Parser(
    (state) => {
      let $ = runwrap(state, parser);
      if ($ instanceof Cont) {
        let to_a = $[0];
        let a = $[1];
        let state$1 = $[2];
        let $1 = runwrap(state$1, f(a));
        if ($1 instanceof Cont) {
          let to_b = $1[0];
          let b = $1[1];
          let state$2 = $1[2];
          return new Cont(should_commit(to_a, to_b), b, state$2);
        } else {
          let to_b = $1[0];
          let bag = $1[1];
          return new Fail(should_commit(to_a, to_b), bag);
        }
      } else {
        let can_backtrack = $[0];
        let bag = $[1];
        return new Fail(can_backtrack, bag);
      }
    },
  );
}

export function then$(parser, f) {
  return do$(parser, f);
}

export function map(parser, f) {
  return do$(parser, (a) => { return return$(f(a)); });
}

export function replace(parser, b) {
  return map(parser, (_) => { return b; });
}

export function span() {
  return new Parser(
    (state) => { return new Cont(new CanBacktrack(false), state.pos, state); },
  );
}

function loop_help(loop$f, loop$commit, loop$loop_state, loop$state) {
  while (true) {
    let f = loop$f;
    let commit = loop$commit;
    let loop_state = loop$loop_state;
    let state = loop$state;
    let $ = runwrap(state, f(loop_state));
    if ($ instanceof Cont && $[1] instanceof Continue) {
      let can_backtrack = $[0];
      let next_loop_state = $[1][0];
      let next_state = $[2];
      loop$f = f;
      loop$commit = should_commit(commit, can_backtrack);
      loop$loop_state = next_loop_state;
      loop$state = next_state;
    } else if ($ instanceof Cont && $[1] instanceof Break) {
      let can_backtrack = $[0];
      let result = $[1][0];
      let next_state = $[2];
      return new Cont(should_commit(commit, can_backtrack), result, next_state);
    } else {
      let can_backtrack = $[0];
      let bag = $[1];
      return new Fail(should_commit(commit, can_backtrack), bag);
    }
  }
}

export function loop(init, step) {
  return new Parser(
    (state) => { return loop_help(step, new CanBacktrack(false), init, state); },
  );
}

export function take_while(predicate) {
  return new Parser(
    (state) => {
      let $ = next(state);
      let tok = $[0];
      let next_state = $[1];
      let $1 = $option.map(tok, predicate);
      if (tok instanceof Some && $1 instanceof Some && $1[0]) {
        let tok$1 = tok[0];
        return runwrap(
          next_state,
          do$(
            take_while(predicate),
            (toks) => { return return$(listPrepend(tok$1, toks)); },
          ),
        );
      } else if (tok instanceof Some && $1 instanceof Some && !$1[0]) {
        return new Cont(new CanBacktrack(false), toList([]), state);
      } else {
        return new Cont(new CanBacktrack(false), toList([]), state);
      }
    },
  );
}

export function take_until(predicate) {
  return take_while((tok) => { return $bool.negate(predicate(tok)); });
}

export function take_exactly(parser, count) {
  if (count === 0) {
    return return$(toList([]));
  } else {
    return do$(
      parser,
      (x) => {
        return do$(
          take_exactly(parser, count - 1),
          (xs) => { return return$(listPrepend(x, xs)); },
        );
      },
    );
  }
}

export function take_map_while(f) {
  return new Parser(
    (state) => {
      let $ = next(state);
      let tok = $[0];
      let next_state = $[1];
      let $1 = $option.then$(tok, f);
      if (tok instanceof None) {
        return new Cont(new CanBacktrack(true), toList([]), state);
      } else if (tok instanceof Some && $1 instanceof None) {
        return new Cont(new CanBacktrack(true), toList([]), state);
      } else {
        let x = $1[0];
        return runwrap(
          next_state,
          (() => {
            let _pipe = take_map_while(f);
            return map(
              _pipe,
              (_capture) => { return $list.prepend(_capture, x); },
            );
          })(),
        );
      }
    },
  );
}

function bag_from_state(state, problem) {
  return new Cons(new Empty(), new DeadEnd(state.pos, problem, state.ctx));
}

export function throw$(message) {
  return new Parser(
    (state) => {
      let error = new Custom(message);
      let bag = bag_from_state(state, error);
      return new Fail(new CanBacktrack(false), bag);
    },
  );
}

export function fail(message) {
  return throw$(message);
}

export function token(tok) {
  return new Parser(
    (state) => {
      let $ = next(state);
      if ($[0] instanceof $option.Some && (isEqual(tok, $[0][0]))) {
        let t = $[0][0];
        let state$1 = $[1];
        return new Cont(new CanBacktrack(true), undefined, state$1);
      } else if ($[0] instanceof $option.Some) {
        let t = $[0][0];
        let state$1 = $[1];
        return new Fail(
          new CanBacktrack(false),
          bag_from_state(state$1, new Expected($string.inspect(tok), t)),
        );
      } else {
        let state$1 = $[1];
        return new Fail(
          new CanBacktrack(false),
          bag_from_state(state$1, new EndOfInput()),
        );
      }
    },
  );
}

export function eof() {
  return new Parser(
    (state) => {
      let $ = next(state);
      if ($[0] instanceof $option.Some) {
        let tok = $[0][0];
        let state$1 = $[1];
        return new Fail(
          new CanBacktrack(false),
          bag_from_state(state$1, new Unexpected(tok)),
        );
      } else {
        return new Cont(new CanBacktrack(false), undefined, state);
      }
    },
  );
}

export function guard(cond, expecting) {
  if (cond) {
    return return$(undefined);
  } else {
    return fail(expecting);
  }
}

export function take_if(expecting, predicate) {
  return new Parser(
    (state) => {
      let $ = next(state);
      let tok = $[0];
      let next_state = $[1];
      let $1 = $option.map(tok, predicate);
      if (tok instanceof Some && $1 instanceof Some && $1[0]) {
        let tok$1 = tok[0];
        return new Cont(new CanBacktrack(false), tok$1, next_state);
      } else if (tok instanceof Some && $1 instanceof Some && !$1[0]) {
        let tok$1 = tok[0];
        return new Fail(
          new CanBacktrack(false),
          bag_from_state(next_state, new Expected(expecting, tok$1)),
        );
      } else {
        return new Fail(
          new CanBacktrack(false),
          bag_from_state(next_state, new EndOfInput()),
        );
      }
    },
  );
}

export function any() {
  return take_if("a single token", (_) => { return true; });
}

export function take_while1(expecting, predicate) {
  return do$(
    take_if(expecting, predicate),
    (x) => {
      return do$(
        take_while(predicate),
        (xs) => { return return$(listPrepend(x, xs)); },
      );
    },
  );
}

export function take_until1(expecting, predicate) {
  return take_while1(
    expecting,
    (tok) => { return $bool.negate(predicate(tok)); },
  );
}

export function take_map(expecting, f) {
  return new Parser(
    (state) => {
      let $ = next(state);
      let tok = $[0];
      let next_state = $[1];
      let $1 = $option.then$(tok, f);
      if (tok instanceof None) {
        return new Fail(
          new CanBacktrack(false),
          bag_from_state(next_state, new EndOfInput()),
        );
      } else if (tok instanceof Some && $1 instanceof None) {
        let tok$1 = tok[0];
        return new Fail(
          new CanBacktrack(false),
          bag_from_state(next_state, new Expected(expecting, tok$1)),
        );
      } else {
        let a = $1[0];
        return new Cont(new CanBacktrack(false), a, next_state);
      }
    },
  );
}

export function take_map_while1(expecting, f) {
  return do$(
    take_map(expecting, f),
    (x) => {
      return do$(
        take_map_while(f),
        (xs) => { return return$(listPrepend(x, xs)); },
      );
    },
  );
}

function to_deadends(loop$bag, loop$acc) {
  while (true) {
    let bag = loop$bag;
    let acc = loop$acc;
    if (bag instanceof Empty) {
      return acc;
    } else if (bag instanceof Cons && bag[0] instanceof Empty) {
      let deadend = bag[1];
      return listPrepend(deadend, acc);
    } else if (bag instanceof Cons) {
      let bag$1 = bag[0];
      let deadend = bag[1];
      loop$bag = bag$1;
      loop$acc = listPrepend(deadend, acc);
    } else {
      let left = bag[0];
      let right = bag[1];
      loop$bag = left;
      loop$acc = to_deadends(right, acc);
    }
  }
}

export function run(src, parser) {
  let src$1 = $list.index_fold(
    src,
    $dict.new$(),
    (dict, tok, idx) => { return $dict.insert(dict, idx, tok); },
  );
  let init = new State(src$1, 0, new Span(1, 1, 1, 1), toList([]));
  let $ = runwrap(init, parser);
  if ($ instanceof Cont) {
    let a = $[1];
    return new Ok(a);
  } else {
    let bag = $[1];
    return new Error(to_deadends(bag, toList([])));
  }
}

function add_bag_to_step(step, left) {
  if (step instanceof Cont) {
    let can_backtrack = step[0];
    let a = step[1];
    let state = step[2];
    return new Cont(can_backtrack, a, state);
  } else {
    let can_backtrack = step[0];
    let right = step[1];
    return new Fail(can_backtrack, new Append(left, right));
  }
}

export function one_of(parsers) {
  return new Parser(
    (state) => {
      let init = new Fail(new CanBacktrack(false), new Empty());
      return $list.fold_until(
        parsers,
        init,
        (result, next) => {
          if (result instanceof Cont) {
            return new $list.Stop(result);
          } else if (result instanceof Fail &&
          result[0] instanceof CanBacktrack &&
          result[0][0]) {
            return new $list.Stop(result);
          } else {
            let bag = result[1];
            let _pipe = runwrap(state, next);
            let _pipe$1 = add_bag_to_step(_pipe, bag);
            return new $list.Continue(_pipe$1);
          }
        },
      );
    },
  );
}

function more(x, parser, separator) {
  return loop(
    toList([x]),
    (xs) => {
      let break$ = () => { return return$(new Break($list.reverse(xs))); };
      let continue$ = do$(
        separator,
        (_) => {
          return do$(
            parser,
            (x) => { return return$(new Continue(listPrepend(x, xs))); },
          );
        },
      );
      return one_of(toList([continue$, lazy(break$)]));
    },
  );
}

export function sequence(parser, sep) {
  return one_of(
    toList([
      (() => {
        let _pipe = parser;
        return then$(
          _pipe,
          (_capture) => { return more(_capture, parser, sep); },
        );
      })(),
      return$(toList([])),
    ]),
  );
}

export function many(parser) {
  return sequence(parser, return$(undefined));
}

export function many1(parser) {
  return do$(
    parser,
    (x) => {
      return do$(many(parser), (xs) => { return return$(listPrepend(x, xs)); });
    },
  );
}

export function take_at_least(parser, count) {
  if (count === 0) {
    return many(parser);
  } else {
    return do$(
      parser,
      (x) => {
        return do$(
          take_at_least(parser, count - 1),
          (xs) => { return return$(listPrepend(x, xs)); },
        );
      },
    );
  }
}

export function or(parser, default$) {
  return one_of(toList([parser, return$(default$)]));
}

export function take_up_to(parser, count) {
  if (count === 0) {
    return return$(toList([]));
  } else {
    let _pipe = do$(
      parser,
      (x) => {
        return do$(
          take_up_to(parser, count - 1),
          (xs) => { return return$(listPrepend(x, xs)); },
        );
      },
    );
    return or(_pipe, toList([]));
  }
}

export function optional(parser) {
  return one_of(
    toList([
      map(parser, (var0) => { return new Some(var0); }),
      return$(new None()),
    ]),
  );
}

function push_context(state, context) {
  return state.withFields({ ctx: listPrepend([state.pos, context], state.ctx) });
}

function pop_context(state) {
  let $ = state.ctx;
  if ($.hasLength(0)) {
    return state;
  } else {
    let context = $.tail;
    return state.withFields({ ctx: context });
  }
}

export function in$(parser, context) {
  return new Parser(
    (state) => {
      let $ = runwrap(push_context(state, context), parser);
      if ($ instanceof Cont) {
        let can_backtrack = $[0];
        let a = $[1];
        let state$1 = $[2];
        return new Cont(can_backtrack, a, pop_context(state$1));
      } else {
        let can_backtrack = $[0];
        let bag = $[1];
        return new Fail(can_backtrack, bag);
      }
    },
  );
}

export function do_in(context, parser, f) {
  let _pipe = do$(parser, f);
  return in$(_pipe, context);
}

export function inspect(parser, message) {
  return new Parser(
    (state) => {
      $io.println(message + ": ");
      let _pipe = runwrap(state, parser);
      return $io.debug(_pipe);
    },
  );
}
