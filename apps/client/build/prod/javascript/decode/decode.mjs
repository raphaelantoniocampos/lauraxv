import * as $dict from "../gleam_stdlib/gleam/dict.mjs";
import * as $dynamic from "../gleam_stdlib/gleam/dynamic.mjs";
import { DecodeError } from "../gleam_stdlib/gleam/dynamic.mjs";
import * as $int from "../gleam_stdlib/gleam/int.mjs";
import * as $list from "../gleam_stdlib/gleam/list.mjs";
import * as $option from "../gleam_stdlib/gleam/option.mjs";
import * as $result from "../gleam_stdlib/gleam/result.mjs";
import { index as bare_index } from "./decode_ffi.mjs";
import { Ok, Error, toList, prepend as listPrepend, CustomType as $CustomType } from "./gleam.mjs";

class Decoder extends $CustomType {
  constructor(continuation) {
    super();
    this.continuation = continuation;
  }
}

export function into(constructor) {
  return new Decoder((_) => { return new Ok(constructor); });
}

export function parameter(body) {
  return body;
}

export function from(decoder, data) {
  return decoder.continuation(data);
}

export function map(decoder, transformer) {
  return new Decoder(
    (d) => {
      let $ = decoder.continuation(d);
      if ($.isOk()) {
        let a = $[0];
        return new Ok(transformer(a));
      } else {
        let e = $[0];
        return new Error(e);
      }
    },
  );
}

export function map_errors(decoder, transformer) {
  return new Decoder(
    (d) => {
      let $ = decoder.continuation(d);
      if ($.isOk()) {
        let a = $[0];
        return new Ok(a);
      } else {
        let e = $[0];
        return new Error(transformer(e));
      }
    },
  );
}

export function then$(decoder, next) {
  return new Decoder(
    (d) => {
      let $ = decoder.continuation(d);
      if ($.isOk()) {
        let a = $[0];
        let _pipe = next(a);
        return from(_pipe, d);
      } else {
        let e = $[0];
        return new Error(e);
      }
    },
  );
}

export const string = /* @__PURE__ */ new Decoder($dynamic.string);

export const bool = /* @__PURE__ */ new Decoder($dynamic.bool);

export const int = /* @__PURE__ */ new Decoder($dynamic.int);

export const float = /* @__PURE__ */ new Decoder($dynamic.float);

export const dynamic = /* @__PURE__ */ new Decoder(
  (var0) => { return new Ok(var0); },
);

export function list(item) {
  return new Decoder($dynamic.list(item.continuation));
}

export function dict(key, value) {
  return new Decoder($dynamic.dict(key.continuation, value.continuation));
}

export function optional(item) {
  return new Decoder($dynamic.optional(item.continuation));
}

export function collapse_errors(decoder, name) {
  return new Decoder(
    (d) => {
      let $ = decoder.continuation(d);
      if ($.isOk()) {
        let a = $[0];
        return new Ok(a);
      } else {
        return new Error(
          toList([new DecodeError(name, $dynamic.classify(d), toList([]))]),
        );
      }
    },
  );
}

function run_decoders(loop$data, loop$decoders) {
  while (true) {
    let data = loop$data;
    let decoders = loop$decoders;
    if (decoders.hasLength(0)) {
      return new Error(
        toList([new DecodeError("nothing", $dynamic.classify(data), toList([]))]),
      );
    } else if (decoders.hasLength(1)) {
      let decoder = decoders.head;
      return from(decoder, data);
    } else {
      let decoder = decoders.head;
      let decoders$1 = decoders.tail;
      let $ = from(decoder, data);
      if ($.isOk()) {
        let value = $[0];
        return new Ok(value);
      } else {
        loop$data = data;
        loop$decoders = decoders$1;
      }
    }
  }
}

export function one_of(decoders) {
  return new Decoder((d) => { return run_decoders(d, decoders); });
}

function push_path(errors, key) {
  let key$1 = $dynamic.from(key);
  let decoder = $dynamic.any(
    toList([
      $dynamic.string,
      (x) => { return $result.map($dynamic.int(x), $int.to_string); },
    ]),
  );
  let key$2 = (() => {
    let $ = decoder(key$1);
    if ($.isOk()) {
      let key$2 = $[0];
      return key$2;
    } else {
      return ("<" + $dynamic.classify(key$1)) + ">";
    }
  })();
  return $list.map(
    errors,
    (error) => {
      return error.withFields({ path: listPrepend(key$2, error.path) });
    },
  );
}

function index(key, inner, data) {
  let $ = bare_index(data, key);
  if ($.isOk()) {
    let data$1 = $[0];
    let $1 = inner(data$1);
    if ($1.isOk()) {
      let data$2 = $1[0];
      return new Ok(data$2);
    } else {
      let errors = $1[0];
      return new Error(push_path(errors, key));
    }
  } else {
    let kind = $[0];
    return new Error(
      toList([new DecodeError(kind, $dynamic.classify(data), toList([]))]),
    );
  }
}

export function at(path, inner) {
  return new Decoder(
    (data) => {
      let decoder = $list.fold_right(
        path,
        inner.continuation,
        (dyn_decoder, segment) => {
          return (_capture) => { return index(segment, dyn_decoder, _capture); };
        },
      );
      return decoder(data);
    },
  );
}

export function subfield(decoder, field_path, field_decoder) {
  return new Decoder(
    (data) => {
      let constructor = decoder.continuation(data);
      let data$1 = from(at(field_path, field_decoder), data);
      if (constructor.isOk() && data$1.isOk()) {
        let constructor$1 = constructor[0];
        let data$2 = data$1[0];
        return new Ok(constructor$1(data$2));
      } else if (!constructor.isOk() && !data$1.isOk()) {
        let e1 = constructor[0];
        let e2 = data$1[0];
        return new Error($list.append(e1, e2));
      } else if (!data$1.isOk()) {
        let errors = data$1[0];
        return new Error(errors);
      } else {
        let errors = constructor[0];
        return new Error(errors);
      }
    },
  );
}

export function field(decoder, field_name, field_decoder) {
  return subfield(decoder, toList([field_name]), field_decoder);
}

export function fail(expected) {
  return new Decoder(
    (d) => {
      return new Error(
        toList([new DecodeError(expected, $dynamic.classify(d), toList([]))]),
      );
    },
  );
}

export const bit_array = /* @__PURE__ */ new Decoder($dynamic.bit_array);
