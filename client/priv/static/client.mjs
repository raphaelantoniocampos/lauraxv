// build/dev/javascript/prelude.mjs
var CustomType = class {
  withFields(fields) {
    let properties = Object.keys(this).map(
      (label2) => label2 in fields ? fields[label2] : this[label2]
    );
    return new this.constructor(...properties);
  }
};
var List = class {
  static fromArray(array3, tail) {
    let t = tail || new Empty();
    for (let i = array3.length - 1; i >= 0; --i) {
      t = new NonEmpty(array3[i], t);
    }
    return t;
  }
  [Symbol.iterator]() {
    return new ListIterator(this);
  }
  toArray() {
    return [...this];
  }
  // @internal
  atLeastLength(desired) {
    for (let _ of this) {
      if (desired <= 0)
        return true;
      desired--;
    }
    return desired <= 0;
  }
  // @internal
  hasLength(desired) {
    for (let _ of this) {
      if (desired <= 0)
        return false;
      desired--;
    }
    return desired === 0;
  }
  countLength() {
    let length5 = 0;
    for (let _ of this)
      length5++;
    return length5;
  }
};
function prepend(element2, tail) {
  return new NonEmpty(element2, tail);
}
function toList(elements2, tail) {
  return List.fromArray(elements2, tail);
}
var ListIterator = class {
  #current;
  constructor(current) {
    this.#current = current;
  }
  next() {
    if (this.#current instanceof Empty) {
      return { done: true };
    } else {
      let { head, tail } = this.#current;
      this.#current = tail;
      return { value: head, done: false };
    }
  }
};
var Empty = class extends List {
};
var NonEmpty = class extends List {
  constructor(head, tail) {
    super();
    this.head = head;
    this.tail = tail;
  }
};
var BitArray = class _BitArray {
  constructor(buffer) {
    if (!(buffer instanceof Uint8Array)) {
      throw "BitArray can only be constructed from a Uint8Array";
    }
    this.buffer = buffer;
  }
  // @internal
  get length() {
    return this.buffer.length;
  }
  // @internal
  byteAt(index4) {
    return this.buffer[index4];
  }
  // @internal
  floatFromSlice(start3, end, isBigEndian) {
    return byteArrayToFloat(this.buffer, start3, end, isBigEndian);
  }
  // @internal
  intFromSlice(start3, end, isBigEndian, isSigned) {
    return byteArrayToInt(this.buffer, start3, end, isBigEndian, isSigned);
  }
  // @internal
  binaryFromSlice(start3, end) {
    return new _BitArray(this.buffer.slice(start3, end));
  }
  // @internal
  sliceAfter(index4) {
    return new _BitArray(this.buffer.slice(index4));
  }
};
function byteArrayToInt(byteArray, start3, end, isBigEndian, isSigned) {
  let value3 = 0;
  if (isBigEndian) {
    for (let i = start3; i < end; i++) {
      value3 = value3 * 256 + byteArray[i];
    }
  } else {
    for (let i = end - 1; i >= start3; i--) {
      value3 = value3 * 256 + byteArray[i];
    }
  }
  if (isSigned) {
    const byteSize = end - start3;
    const highBit = 2 ** (byteSize * 8 - 1);
    if (value3 >= highBit) {
      value3 -= highBit * 2;
    }
  }
  return value3;
}
function byteArrayToFloat(byteArray, start3, end, isBigEndian) {
  const view2 = new DataView(byteArray.buffer);
  const byteSize = end - start3;
  if (byteSize === 8) {
    return view2.getFloat64(start3, !isBigEndian);
  } else if (byteSize === 4) {
    return view2.getFloat32(start3, !isBigEndian);
  } else {
    const msg = `Sized floats must be 32-bit or 64-bit on JavaScript, got size of ${byteSize * 8} bits`;
    throw new globalThis.Error(msg);
  }
}
var Result = class _Result extends CustomType {
  // @internal
  static isResult(data) {
    return data instanceof _Result;
  }
};
var Ok = class extends Result {
  constructor(value3) {
    super();
    this[0] = value3;
  }
  // @internal
  isOk() {
    return true;
  }
};
var Error = class extends Result {
  constructor(detail) {
    super();
    this[0] = detail;
  }
  // @internal
  isOk() {
    return false;
  }
};
function isEqual(x, y) {
  let values = [x, y];
  while (values.length) {
    let a2 = values.pop();
    let b = values.pop();
    if (a2 === b)
      continue;
    if (!isObject(a2) || !isObject(b))
      return false;
    let unequal = !structurallyCompatibleObjects(a2, b) || unequalDates(a2, b) || unequalBuffers(a2, b) || unequalArrays(a2, b) || unequalMaps(a2, b) || unequalSets(a2, b) || unequalRegExps(a2, b);
    if (unequal)
      return false;
    const proto = Object.getPrototypeOf(a2);
    if (proto !== null && typeof proto.equals === "function") {
      try {
        if (a2.equals(b))
          continue;
        else
          return false;
      } catch {
      }
    }
    let [keys2, get3] = getters(a2);
    for (let k of keys2(a2)) {
      values.push(get3(a2, k), get3(b, k));
    }
  }
  return true;
}
function getters(object3) {
  if (object3 instanceof Map) {
    return [(x) => x.keys(), (x, y) => x.get(y)];
  } else {
    let extra = object3 instanceof globalThis.Error ? ["message"] : [];
    return [(x) => [...extra, ...Object.keys(x)], (x, y) => x[y]];
  }
}
function unequalDates(a2, b) {
  return a2 instanceof Date && (a2 > b || a2 < b);
}
function unequalBuffers(a2, b) {
  return a2.buffer instanceof ArrayBuffer && a2.BYTES_PER_ELEMENT && !(a2.byteLength === b.byteLength && a2.every((n, i) => n === b[i]));
}
function unequalArrays(a2, b) {
  return Array.isArray(a2) && a2.length !== b.length;
}
function unequalMaps(a2, b) {
  return a2 instanceof Map && a2.size !== b.size;
}
function unequalSets(a2, b) {
  return a2 instanceof Set && (a2.size != b.size || [...a2].some((e) => !b.has(e)));
}
function unequalRegExps(a2, b) {
  return a2 instanceof RegExp && (a2.source !== b.source || a2.flags !== b.flags);
}
function isObject(a2) {
  return typeof a2 === "object" && a2 !== null;
}
function structurallyCompatibleObjects(a2, b) {
  if (typeof a2 !== "object" && typeof b !== "object" && (!a2 || !b))
    return false;
  let nonstructural = [Promise, WeakSet, WeakMap, Function];
  if (nonstructural.some((c) => a2 instanceof c))
    return false;
  return a2.constructor === b.constructor;
}
function remainderInt(a2, b) {
  if (b === 0) {
    return 0;
  } else {
    return a2 % b;
  }
}
function divideInt(a2, b) {
  return Math.trunc(divideFloat(a2, b));
}
function divideFloat(a2, b) {
  if (b === 0) {
    return 0;
  } else {
    return a2 / b;
  }
}
function makeError(variant, module, line, fn, message, extra) {
  let error = new globalThis.Error(message);
  error.gleam_error = variant;
  error.module = module;
  error.line = line;
  error.fn = fn;
  for (let k in extra)
    error[k] = extra[k];
  return error;
}

// build/dev/javascript/gleam_stdlib/gleam/option.mjs
var Some = class extends CustomType {
  constructor(x0) {
    super();
    this[0] = x0;
  }
};
var None = class extends CustomType {
};
function to_result(option, e) {
  if (option instanceof Some) {
    let a2 = option[0];
    return new Ok(a2);
  } else {
    return new Error(e);
  }
}
function from_result(result) {
  if (result.isOk()) {
    let a2 = result[0];
    return new Some(a2);
  } else {
    return new None();
  }
}
function unwrap(option, default$) {
  if (option instanceof Some) {
    let x = option[0];
    return x;
  } else {
    return default$;
  }
}
function map(option, fun) {
  if (option instanceof Some) {
    let x = option[0];
    return new Some(fun(x));
  } else {
    return new None();
  }
}

// build/dev/javascript/gleam_stdlib/gleam/regex.mjs
var Match = class extends CustomType {
  constructor(content, submatches) {
    super();
    this.content = content;
    this.submatches = submatches;
  }
};
var CompileError = class extends CustomType {
  constructor(error, byte_index) {
    super();
    this.error = error;
    this.byte_index = byte_index;
  }
};
var Options = class extends CustomType {
  constructor(case_insensitive, multi_line) {
    super();
    this.case_insensitive = case_insensitive;
    this.multi_line = multi_line;
  }
};
function compile(pattern, options) {
  return compile_regex(pattern, options);
}
function scan(regex, string3) {
  return regex_scan(regex, string3);
}

// build/dev/javascript/gleam_stdlib/gleam/float.mjs
function truncate2(x) {
  return truncate(x);
}

// build/dev/javascript/gleam_stdlib/gleam/int.mjs
function parse(string3) {
  return parse_int(string3);
}
function to_string2(x) {
  return to_string(x);
}
function to_float(x) {
  return identity(x);
}
function min(a2, b) {
  let $ = a2 < b;
  if ($) {
    return a2;
  } else {
    return b;
  }
}
function max(a2, b) {
  let $ = a2 > b;
  if ($) {
    return a2;
  } else {
    return b;
  }
}
function clamp(x, min_bound, max_bound) {
  let _pipe = x;
  let _pipe$1 = min(_pipe, max_bound);
  return max(_pipe$1, min_bound);
}
function divide(dividend, divisor) {
  if (divisor === 0) {
    return new Error(void 0);
  } else {
    let divisor$1 = divisor;
    return new Ok(divideInt(dividend, divisor$1));
  }
}

// build/dev/javascript/gleam_stdlib/gleam/pair.mjs
function second(pair) {
  let a2 = pair[1];
  return a2;
}

// build/dev/javascript/gleam_stdlib/gleam/list.mjs
function do_reverse(loop$remaining, loop$accumulator) {
  while (true) {
    let remaining = loop$remaining;
    let accumulator = loop$accumulator;
    if (remaining.hasLength(0)) {
      return accumulator;
    } else {
      let item = remaining.head;
      let rest$1 = remaining.tail;
      loop$remaining = rest$1;
      loop$accumulator = prepend(item, accumulator);
    }
  }
}
function reverse(xs) {
  return do_reverse(xs, toList([]));
}
function first(list2) {
  if (list2.hasLength(0)) {
    return new Error(void 0);
  } else {
    let x = list2.head;
    return new Ok(x);
  }
}
function do_map(loop$list, loop$fun, loop$acc) {
  while (true) {
    let list2 = loop$list;
    let fun = loop$fun;
    let acc = loop$acc;
    if (list2.hasLength(0)) {
      return reverse(acc);
    } else {
      let x = list2.head;
      let xs = list2.tail;
      loop$list = xs;
      loop$fun = fun;
      loop$acc = prepend(fun(x), acc);
    }
  }
}
function map2(list2, fun) {
  return do_map(list2, fun, toList([]));
}
function do_try_map(loop$list, loop$fun, loop$acc) {
  while (true) {
    let list2 = loop$list;
    let fun = loop$fun;
    let acc = loop$acc;
    if (list2.hasLength(0)) {
      return new Ok(reverse(acc));
    } else {
      let x = list2.head;
      let xs = list2.tail;
      let $ = fun(x);
      if ($.isOk()) {
        let y = $[0];
        loop$list = xs;
        loop$fun = fun;
        loop$acc = prepend(y, acc);
      } else {
        let error = $[0];
        return new Error(error);
      }
    }
  }
}
function try_map(list2, fun) {
  return do_try_map(list2, fun, toList([]));
}
function drop(loop$list, loop$n) {
  while (true) {
    let list2 = loop$list;
    let n = loop$n;
    let $ = n <= 0;
    if ($) {
      return list2;
    } else {
      if (list2.hasLength(0)) {
        return toList([]);
      } else {
        let xs = list2.tail;
        loop$list = xs;
        loop$n = n - 1;
      }
    }
  }
}
function do_take(loop$list, loop$n, loop$acc) {
  while (true) {
    let list2 = loop$list;
    let n = loop$n;
    let acc = loop$acc;
    let $ = n <= 0;
    if ($) {
      return reverse(acc);
    } else {
      if (list2.hasLength(0)) {
        return reverse(acc);
      } else {
        let x = list2.head;
        let xs = list2.tail;
        loop$list = xs;
        loop$n = n - 1;
        loop$acc = prepend(x, acc);
      }
    }
  }
}
function take(list2, n) {
  return do_take(list2, n, toList([]));
}
function do_append(loop$first, loop$second) {
  while (true) {
    let first3 = loop$first;
    let second2 = loop$second;
    if (first3.hasLength(0)) {
      return second2;
    } else {
      let item = first3.head;
      let rest$1 = first3.tail;
      loop$first = rest$1;
      loop$second = prepend(item, second2);
    }
  }
}
function append(first3, second2) {
  return do_append(reverse(first3), second2);
}
function reverse_and_prepend(loop$prefix, loop$suffix) {
  while (true) {
    let prefix = loop$prefix;
    let suffix = loop$suffix;
    if (prefix.hasLength(0)) {
      return suffix;
    } else {
      let first$1 = prefix.head;
      let rest$1 = prefix.tail;
      loop$prefix = rest$1;
      loop$suffix = prepend(first$1, suffix);
    }
  }
}
function do_concat(loop$lists, loop$acc) {
  while (true) {
    let lists = loop$lists;
    let acc = loop$acc;
    if (lists.hasLength(0)) {
      return reverse(acc);
    } else {
      let list2 = lists.head;
      let further_lists = lists.tail;
      loop$lists = further_lists;
      loop$acc = reverse_and_prepend(list2, acc);
    }
  }
}
function concat(lists) {
  return do_concat(lists, toList([]));
}
function fold(loop$list, loop$initial, loop$fun) {
  while (true) {
    let list2 = loop$list;
    let initial = loop$initial;
    let fun = loop$fun;
    if (list2.hasLength(0)) {
      return initial;
    } else {
      let x = list2.head;
      let rest$1 = list2.tail;
      loop$list = rest$1;
      loop$initial = fun(initial, x);
      loop$fun = fun;
    }
  }
}
function do_index_fold(loop$over, loop$acc, loop$with, loop$index) {
  while (true) {
    let over = loop$over;
    let acc = loop$acc;
    let with$ = loop$with;
    let index4 = loop$index;
    if (over.hasLength(0)) {
      return acc;
    } else {
      let first$1 = over.head;
      let rest$1 = over.tail;
      loop$over = rest$1;
      loop$acc = with$(acc, first$1, index4);
      loop$with = with$;
      loop$index = index4 + 1;
    }
  }
}
function index_fold(over, initial, fun) {
  return do_index_fold(over, initial, fun, 0);
}
function do_repeat(loop$a, loop$times, loop$acc) {
  while (true) {
    let a2 = loop$a;
    let times = loop$times;
    let acc = loop$acc;
    let $ = times <= 0;
    if ($) {
      return acc;
    } else {
      loop$a = a2;
      loop$times = times - 1;
      loop$acc = prepend(a2, acc);
    }
  }
}
function repeat(a2, times) {
  return do_repeat(a2, times, toList([]));
}
function key_set(list2, key, value3) {
  if (list2.hasLength(0)) {
    return toList([[key, value3]]);
  } else if (list2.atLeastLength(1) && isEqual(list2.head[0], key)) {
    let k = list2.head[0];
    let rest$1 = list2.tail;
    return prepend([key, value3], rest$1);
  } else {
    let first$1 = list2.head;
    let rest$1 = list2.tail;
    return prepend(first$1, key_set(rest$1, key, value3));
  }
}

// build/dev/javascript/gleam_stdlib/gleam/result.mjs
function map3(result, fun) {
  if (result.isOk()) {
    let x = result[0];
    return new Ok(fun(x));
  } else {
    let e = result[0];
    return new Error(e);
  }
}
function map_error(result, fun) {
  if (result.isOk()) {
    let x = result[0];
    return new Ok(x);
  } else {
    let error = result[0];
    return new Error(fun(error));
  }
}
function try$(result, fun) {
  if (result.isOk()) {
    let x = result[0];
    return fun(x);
  } else {
    let e = result[0];
    return new Error(e);
  }
}
function then$(result, fun) {
  return try$(result, fun);
}
function unwrap2(result, default$) {
  if (result.isOk()) {
    let v = result[0];
    return v;
  } else {
    return default$;
  }
}
function nil_error(result) {
  return map_error(result, (_) => {
    return void 0;
  });
}

// build/dev/javascript/gleam_stdlib/gleam/string_builder.mjs
function from_strings(strings) {
  return concat2(strings);
}
function from_string(string3) {
  return identity(string3);
}
function to_string3(builder) {
  return identity(builder);
}
function split2(iodata, pattern) {
  return split(iodata, pattern);
}

// build/dev/javascript/gleam_stdlib/gleam/string.mjs
function length2(string3) {
  return string_length(string3);
}
function lowercase2(string3) {
  return lowercase(string3);
}
function starts_with2(string3, prefix) {
  return starts_with(string3, prefix);
}
function concat3(strings) {
  let _pipe = strings;
  let _pipe$1 = from_strings(_pipe);
  return to_string3(_pipe$1);
}
function pop_grapheme2(string3) {
  return pop_grapheme(string3);
}
function do_slice(string3, idx, len) {
  let _pipe = string3;
  let _pipe$1 = graphemes(_pipe);
  let _pipe$2 = drop(_pipe$1, idx);
  let _pipe$3 = take(_pipe$2, len);
  return concat3(_pipe$3);
}
function slice(string3, idx, len) {
  let $ = len < 0;
  if ($) {
    return "";
  } else {
    let $1 = idx < 0;
    if ($1) {
      let translated_idx = length2(string3) + idx;
      let $2 = translated_idx < 0;
      if ($2) {
        return "";
      } else {
        return do_slice(string3, translated_idx, len);
      }
    } else {
      return do_slice(string3, idx, len);
    }
  }
}
function drop_left(string3, num_graphemes) {
  let $ = num_graphemes < 0;
  if ($) {
    return string3;
  } else {
    return slice(string3, num_graphemes, length2(string3) - num_graphemes);
  }
}
function split3(x, substring) {
  if (substring === "") {
    return graphemes(x);
  } else {
    let _pipe = x;
    let _pipe$1 = from_string(_pipe);
    let _pipe$2 = split2(_pipe$1, substring);
    return map2(_pipe$2, to_string3);
  }
}

// build/dev/javascript/gleam_stdlib/gleam/dynamic.mjs
var DecodeError = class extends CustomType {
  constructor(expected, found, path) {
    super();
    this.expected = expected;
    this.found = found;
    this.path = path;
  }
};
function from(a2) {
  return identity(a2);
}
function classify(data) {
  return classify_dynamic(data);
}
function int(data) {
  return decode_int(data);
}
function bool(data) {
  return decode_bool(data);
}
function shallow_list(value3) {
  return decode_list(value3);
}
function any(decoders) {
  return (data) => {
    if (decoders.hasLength(0)) {
      return new Error(
        toList([new DecodeError("another type", classify(data), toList([]))])
      );
    } else {
      let decoder = decoders.head;
      let decoders$1 = decoders.tail;
      let $ = decoder(data);
      if ($.isOk()) {
        let decoded = $[0];
        return new Ok(decoded);
      } else {
        return any(decoders$1)(data);
      }
    }
  };
}
function all_errors(result) {
  if (result.isOk()) {
    return toList([]);
  } else {
    let errors = result[0];
    return errors;
  }
}
function push_path(error, name) {
  let name$1 = from(name);
  let decoder = any(
    toList([string, (x) => {
      return map3(int(x), to_string2);
    }])
  );
  let name$2 = (() => {
    let $ = decoder(name$1);
    if ($.isOk()) {
      let name$22 = $[0];
      return name$22;
    } else {
      let _pipe = toList(["<", classify(name$1), ">"]);
      let _pipe$1 = from_strings(_pipe);
      return to_string3(_pipe$1);
    }
  })();
  return error.withFields({ path: prepend(name$2, error.path) });
}
function list(decoder_type) {
  return (dynamic) => {
    return try$(
      shallow_list(dynamic),
      (list2) => {
        let _pipe = list2;
        let _pipe$1 = try_map(_pipe, decoder_type);
        return map_errors(
          _pipe$1,
          (_capture) => {
            return push_path(_capture, "*");
          }
        );
      }
    );
  };
}
function map_errors(result, f) {
  return map_error(
    result,
    (_capture) => {
      return map2(_capture, f);
    }
  );
}
function string(data) {
  return decode_string(data);
}
function field(name, inner_type) {
  return (value3) => {
    let missing_field_error = new DecodeError("field", "nothing", toList([]));
    return try$(
      decode_field(value3, name),
      (maybe_inner) => {
        let _pipe = maybe_inner;
        let _pipe$1 = to_result(_pipe, toList([missing_field_error]));
        let _pipe$2 = try$(_pipe$1, inner_type);
        return map_errors(
          _pipe$2,
          (_capture) => {
            return push_path(_capture, name);
          }
        );
      }
    );
  };
}
function decode5(constructor, t1, t2, t3, t4, t5) {
  return (x) => {
    let $ = t1(x);
    let $1 = t2(x);
    let $2 = t3(x);
    let $3 = t4(x);
    let $4 = t5(x);
    if ($.isOk() && $1.isOk() && $2.isOk() && $3.isOk() && $4.isOk()) {
      let a2 = $[0];
      let b = $1[0];
      let c = $2[0];
      let d = $3[0];
      let e = $4[0];
      return new Ok(constructor(a2, b, c, d, e));
    } else {
      let a2 = $;
      let b = $1;
      let c = $2;
      let d = $3;
      let e = $4;
      return new Error(
        concat(
          toList([
            all_errors(a2),
            all_errors(b),
            all_errors(c),
            all_errors(d),
            all_errors(e)
          ])
        )
      );
    }
  };
}

// build/dev/javascript/gleam_stdlib/dict.mjs
var referenceMap = /* @__PURE__ */ new WeakMap();
var tempDataView = new DataView(new ArrayBuffer(8));
var referenceUID = 0;
function hashByReference(o) {
  const known = referenceMap.get(o);
  if (known !== void 0) {
    return known;
  }
  const hash = referenceUID++;
  if (referenceUID === 2147483647) {
    referenceUID = 0;
  }
  referenceMap.set(o, hash);
  return hash;
}
function hashMerge(a2, b) {
  return a2 ^ b + 2654435769 + (a2 << 6) + (a2 >> 2) | 0;
}
function hashString(s) {
  let hash = 0;
  const len = s.length;
  for (let i = 0; i < len; i++) {
    hash = Math.imul(31, hash) + s.charCodeAt(i) | 0;
  }
  return hash;
}
function hashNumber(n) {
  tempDataView.setFloat64(0, n);
  const i = tempDataView.getInt32(0);
  const j = tempDataView.getInt32(4);
  return Math.imul(73244475, i >> 16 ^ i) ^ j;
}
function hashBigInt(n) {
  return hashString(n.toString());
}
function hashObject(o) {
  const proto = Object.getPrototypeOf(o);
  if (proto !== null && typeof proto.hashCode === "function") {
    try {
      const code = o.hashCode(o);
      if (typeof code === "number") {
        return code;
      }
    } catch {
    }
  }
  if (o instanceof Promise || o instanceof WeakSet || o instanceof WeakMap) {
    return hashByReference(o);
  }
  if (o instanceof Date) {
    return hashNumber(o.getTime());
  }
  let h = 0;
  if (o instanceof ArrayBuffer) {
    o = new Uint8Array(o);
  }
  if (Array.isArray(o) || o instanceof Uint8Array) {
    for (let i = 0; i < o.length; i++) {
      h = Math.imul(31, h) + getHash(o[i]) | 0;
    }
  } else if (o instanceof Set) {
    o.forEach((v) => {
      h = h + getHash(v) | 0;
    });
  } else if (o instanceof Map) {
    o.forEach((v, k) => {
      h = h + hashMerge(getHash(v), getHash(k)) | 0;
    });
  } else {
    const keys2 = Object.keys(o);
    for (let i = 0; i < keys2.length; i++) {
      const k = keys2[i];
      const v = o[k];
      h = h + hashMerge(getHash(v), hashString(k)) | 0;
    }
  }
  return h;
}
function getHash(u) {
  if (u === null)
    return 1108378658;
  if (u === void 0)
    return 1108378659;
  if (u === true)
    return 1108378657;
  if (u === false)
    return 1108378656;
  switch (typeof u) {
    case "number":
      return hashNumber(u);
    case "string":
      return hashString(u);
    case "bigint":
      return hashBigInt(u);
    case "object":
      return hashObject(u);
    case "symbol":
      return hashByReference(u);
    case "function":
      return hashByReference(u);
    default:
      return 0;
  }
}
var SHIFT = 5;
var BUCKET_SIZE = Math.pow(2, SHIFT);
var MASK = BUCKET_SIZE - 1;
var MAX_INDEX_NODE = BUCKET_SIZE / 2;
var MIN_ARRAY_NODE = BUCKET_SIZE / 4;
var ENTRY = 0;
var ARRAY_NODE = 1;
var INDEX_NODE = 2;
var COLLISION_NODE = 3;
var EMPTY = {
  type: INDEX_NODE,
  bitmap: 0,
  array: []
};
function mask(hash, shift) {
  return hash >>> shift & MASK;
}
function bitpos(hash, shift) {
  return 1 << mask(hash, shift);
}
function bitcount(x) {
  x -= x >> 1 & 1431655765;
  x = (x & 858993459) + (x >> 2 & 858993459);
  x = x + (x >> 4) & 252645135;
  x += x >> 8;
  x += x >> 16;
  return x & 127;
}
function index(bitmap, bit) {
  return bitcount(bitmap & bit - 1);
}
function cloneAndSet(arr, at, val) {
  const len = arr.length;
  const out = new Array(len);
  for (let i = 0; i < len; ++i) {
    out[i] = arr[i];
  }
  out[at] = val;
  return out;
}
function spliceIn(arr, at, val) {
  const len = arr.length;
  const out = new Array(len + 1);
  let i = 0;
  let g = 0;
  while (i < at) {
    out[g++] = arr[i++];
  }
  out[g++] = val;
  while (i < len) {
    out[g++] = arr[i++];
  }
  return out;
}
function spliceOut(arr, at) {
  const len = arr.length;
  const out = new Array(len - 1);
  let i = 0;
  let g = 0;
  while (i < at) {
    out[g++] = arr[i++];
  }
  ++i;
  while (i < len) {
    out[g++] = arr[i++];
  }
  return out;
}
function createNode(shift, key1, val1, key2hash, key2, val2) {
  const key1hash = getHash(key1);
  if (key1hash === key2hash) {
    return {
      type: COLLISION_NODE,
      hash: key1hash,
      array: [
        { type: ENTRY, k: key1, v: val1 },
        { type: ENTRY, k: key2, v: val2 }
      ]
    };
  }
  const addedLeaf = { val: false };
  return assoc(
    assocIndex(EMPTY, shift, key1hash, key1, val1, addedLeaf),
    shift,
    key2hash,
    key2,
    val2,
    addedLeaf
  );
}
function assoc(root, shift, hash, key, val, addedLeaf) {
  switch (root.type) {
    case ARRAY_NODE:
      return assocArray(root, shift, hash, key, val, addedLeaf);
    case INDEX_NODE:
      return assocIndex(root, shift, hash, key, val, addedLeaf);
    case COLLISION_NODE:
      return assocCollision(root, shift, hash, key, val, addedLeaf);
  }
}
function assocArray(root, shift, hash, key, val, addedLeaf) {
  const idx = mask(hash, shift);
  const node = root.array[idx];
  if (node === void 0) {
    addedLeaf.val = true;
    return {
      type: ARRAY_NODE,
      size: root.size + 1,
      array: cloneAndSet(root.array, idx, { type: ENTRY, k: key, v: val })
    };
  }
  if (node.type === ENTRY) {
    if (isEqual(key, node.k)) {
      if (val === node.v) {
        return root;
      }
      return {
        type: ARRAY_NODE,
        size: root.size,
        array: cloneAndSet(root.array, idx, {
          type: ENTRY,
          k: key,
          v: val
        })
      };
    }
    addedLeaf.val = true;
    return {
      type: ARRAY_NODE,
      size: root.size,
      array: cloneAndSet(
        root.array,
        idx,
        createNode(shift + SHIFT, node.k, node.v, hash, key, val)
      )
    };
  }
  const n = assoc(node, shift + SHIFT, hash, key, val, addedLeaf);
  if (n === node) {
    return root;
  }
  return {
    type: ARRAY_NODE,
    size: root.size,
    array: cloneAndSet(root.array, idx, n)
  };
}
function assocIndex(root, shift, hash, key, val, addedLeaf) {
  const bit = bitpos(hash, shift);
  const idx = index(root.bitmap, bit);
  if ((root.bitmap & bit) !== 0) {
    const node = root.array[idx];
    if (node.type !== ENTRY) {
      const n = assoc(node, shift + SHIFT, hash, key, val, addedLeaf);
      if (n === node) {
        return root;
      }
      return {
        type: INDEX_NODE,
        bitmap: root.bitmap,
        array: cloneAndSet(root.array, idx, n)
      };
    }
    const nodeKey = node.k;
    if (isEqual(key, nodeKey)) {
      if (val === node.v) {
        return root;
      }
      return {
        type: INDEX_NODE,
        bitmap: root.bitmap,
        array: cloneAndSet(root.array, idx, {
          type: ENTRY,
          k: key,
          v: val
        })
      };
    }
    addedLeaf.val = true;
    return {
      type: INDEX_NODE,
      bitmap: root.bitmap,
      array: cloneAndSet(
        root.array,
        idx,
        createNode(shift + SHIFT, nodeKey, node.v, hash, key, val)
      )
    };
  } else {
    const n = root.array.length;
    if (n >= MAX_INDEX_NODE) {
      const nodes = new Array(32);
      const jdx = mask(hash, shift);
      nodes[jdx] = assocIndex(EMPTY, shift + SHIFT, hash, key, val, addedLeaf);
      let j = 0;
      let bitmap = root.bitmap;
      for (let i = 0; i < 32; i++) {
        if ((bitmap & 1) !== 0) {
          const node = root.array[j++];
          nodes[i] = node;
        }
        bitmap = bitmap >>> 1;
      }
      return {
        type: ARRAY_NODE,
        size: n + 1,
        array: nodes
      };
    } else {
      const newArray = spliceIn(root.array, idx, {
        type: ENTRY,
        k: key,
        v: val
      });
      addedLeaf.val = true;
      return {
        type: INDEX_NODE,
        bitmap: root.bitmap | bit,
        array: newArray
      };
    }
  }
}
function assocCollision(root, shift, hash, key, val, addedLeaf) {
  if (hash === root.hash) {
    const idx = collisionIndexOf(root, key);
    if (idx !== -1) {
      const entry = root.array[idx];
      if (entry.v === val) {
        return root;
      }
      return {
        type: COLLISION_NODE,
        hash,
        array: cloneAndSet(root.array, idx, { type: ENTRY, k: key, v: val })
      };
    }
    const size = root.array.length;
    addedLeaf.val = true;
    return {
      type: COLLISION_NODE,
      hash,
      array: cloneAndSet(root.array, size, { type: ENTRY, k: key, v: val })
    };
  }
  return assoc(
    {
      type: INDEX_NODE,
      bitmap: bitpos(root.hash, shift),
      array: [root]
    },
    shift,
    hash,
    key,
    val,
    addedLeaf
  );
}
function collisionIndexOf(root, key) {
  const size = root.array.length;
  for (let i = 0; i < size; i++) {
    if (isEqual(key, root.array[i].k)) {
      return i;
    }
  }
  return -1;
}
function find(root, shift, hash, key) {
  switch (root.type) {
    case ARRAY_NODE:
      return findArray(root, shift, hash, key);
    case INDEX_NODE:
      return findIndex(root, shift, hash, key);
    case COLLISION_NODE:
      return findCollision(root, key);
  }
}
function findArray(root, shift, hash, key) {
  const idx = mask(hash, shift);
  const node = root.array[idx];
  if (node === void 0) {
    return void 0;
  }
  if (node.type !== ENTRY) {
    return find(node, shift + SHIFT, hash, key);
  }
  if (isEqual(key, node.k)) {
    return node;
  }
  return void 0;
}
function findIndex(root, shift, hash, key) {
  const bit = bitpos(hash, shift);
  if ((root.bitmap & bit) === 0) {
    return void 0;
  }
  const idx = index(root.bitmap, bit);
  const node = root.array[idx];
  if (node.type !== ENTRY) {
    return find(node, shift + SHIFT, hash, key);
  }
  if (isEqual(key, node.k)) {
    return node;
  }
  return void 0;
}
function findCollision(root, key) {
  const idx = collisionIndexOf(root, key);
  if (idx < 0) {
    return void 0;
  }
  return root.array[idx];
}
function without(root, shift, hash, key) {
  switch (root.type) {
    case ARRAY_NODE:
      return withoutArray(root, shift, hash, key);
    case INDEX_NODE:
      return withoutIndex(root, shift, hash, key);
    case COLLISION_NODE:
      return withoutCollision(root, key);
  }
}
function withoutArray(root, shift, hash, key) {
  const idx = mask(hash, shift);
  const node = root.array[idx];
  if (node === void 0) {
    return root;
  }
  let n = void 0;
  if (node.type === ENTRY) {
    if (!isEqual(node.k, key)) {
      return root;
    }
  } else {
    n = without(node, shift + SHIFT, hash, key);
    if (n === node) {
      return root;
    }
  }
  if (n === void 0) {
    if (root.size <= MIN_ARRAY_NODE) {
      const arr = root.array;
      const out = new Array(root.size - 1);
      let i = 0;
      let j = 0;
      let bitmap = 0;
      while (i < idx) {
        const nv = arr[i];
        if (nv !== void 0) {
          out[j] = nv;
          bitmap |= 1 << i;
          ++j;
        }
        ++i;
      }
      ++i;
      while (i < arr.length) {
        const nv = arr[i];
        if (nv !== void 0) {
          out[j] = nv;
          bitmap |= 1 << i;
          ++j;
        }
        ++i;
      }
      return {
        type: INDEX_NODE,
        bitmap,
        array: out
      };
    }
    return {
      type: ARRAY_NODE,
      size: root.size - 1,
      array: cloneAndSet(root.array, idx, n)
    };
  }
  return {
    type: ARRAY_NODE,
    size: root.size,
    array: cloneAndSet(root.array, idx, n)
  };
}
function withoutIndex(root, shift, hash, key) {
  const bit = bitpos(hash, shift);
  if ((root.bitmap & bit) === 0) {
    return root;
  }
  const idx = index(root.bitmap, bit);
  const node = root.array[idx];
  if (node.type !== ENTRY) {
    const n = without(node, shift + SHIFT, hash, key);
    if (n === node) {
      return root;
    }
    if (n !== void 0) {
      return {
        type: INDEX_NODE,
        bitmap: root.bitmap,
        array: cloneAndSet(root.array, idx, n)
      };
    }
    if (root.bitmap === bit) {
      return void 0;
    }
    return {
      type: INDEX_NODE,
      bitmap: root.bitmap ^ bit,
      array: spliceOut(root.array, idx)
    };
  }
  if (isEqual(key, node.k)) {
    if (root.bitmap === bit) {
      return void 0;
    }
    return {
      type: INDEX_NODE,
      bitmap: root.bitmap ^ bit,
      array: spliceOut(root.array, idx)
    };
  }
  return root;
}
function withoutCollision(root, key) {
  const idx = collisionIndexOf(root, key);
  if (idx < 0) {
    return root;
  }
  if (root.array.length === 1) {
    return void 0;
  }
  return {
    type: COLLISION_NODE,
    hash: root.hash,
    array: spliceOut(root.array, idx)
  };
}
function forEach(root, fn) {
  if (root === void 0) {
    return;
  }
  const items = root.array;
  const size = items.length;
  for (let i = 0; i < size; i++) {
    const item = items[i];
    if (item === void 0) {
      continue;
    }
    if (item.type === ENTRY) {
      fn(item.v, item.k);
      continue;
    }
    forEach(item, fn);
  }
}
var Dict = class _Dict {
  /**
   * @template V
   * @param {Record<string,V>} o
   * @returns {Dict<string,V>}
   */
  static fromObject(o) {
    const keys2 = Object.keys(o);
    let m = _Dict.new();
    for (let i = 0; i < keys2.length; i++) {
      const k = keys2[i];
      m = m.set(k, o[k]);
    }
    return m;
  }
  /**
   * @template K,V
   * @param {Map<K,V>} o
   * @returns {Dict<K,V>}
   */
  static fromMap(o) {
    let m = _Dict.new();
    o.forEach((v, k) => {
      m = m.set(k, v);
    });
    return m;
  }
  static new() {
    return new _Dict(void 0, 0);
  }
  /**
   * @param {undefined | Node<K,V>} root
   * @param {number} size
   */
  constructor(root, size) {
    this.root = root;
    this.size = size;
  }
  /**
   * @template NotFound
   * @param {K} key
   * @param {NotFound} notFound
   * @returns {NotFound | V}
   */
  get(key, notFound) {
    if (this.root === void 0) {
      return notFound;
    }
    const found = find(this.root, 0, getHash(key), key);
    if (found === void 0) {
      return notFound;
    }
    return found.v;
  }
  /**
   * @param {K} key
   * @param {V} val
   * @returns {Dict<K,V>}
   */
  set(key, val) {
    const addedLeaf = { val: false };
    const root = this.root === void 0 ? EMPTY : this.root;
    const newRoot = assoc(root, 0, getHash(key), key, val, addedLeaf);
    if (newRoot === this.root) {
      return this;
    }
    return new _Dict(newRoot, addedLeaf.val ? this.size + 1 : this.size);
  }
  /**
   * @param {K} key
   * @returns {Dict<K,V>}
   */
  delete(key) {
    if (this.root === void 0) {
      return this;
    }
    const newRoot = without(this.root, 0, getHash(key), key);
    if (newRoot === this.root) {
      return this;
    }
    if (newRoot === void 0) {
      return _Dict.new();
    }
    return new _Dict(newRoot, this.size - 1);
  }
  /**
   * @param {K} key
   * @returns {boolean}
   */
  has(key) {
    if (this.root === void 0) {
      return false;
    }
    return find(this.root, 0, getHash(key), key) !== void 0;
  }
  /**
   * @returns {[K,V][]}
   */
  entries() {
    if (this.root === void 0) {
      return [];
    }
    const result = [];
    this.forEach((v, k) => result.push([k, v]));
    return result;
  }
  /**
   *
   * @param {(val:V,key:K)=>void} fn
   */
  forEach(fn) {
    forEach(this.root, fn);
  }
  hashCode() {
    let h = 0;
    this.forEach((v, k) => {
      h = h + hashMerge(getHash(v), getHash(k)) | 0;
    });
    return h;
  }
  /**
   * @param {unknown} o
   * @returns {boolean}
   */
  equals(o) {
    if (!(o instanceof _Dict) || this.size !== o.size) {
      return false;
    }
    let equal = true;
    this.forEach((v, k) => {
      equal = equal && isEqual(o.get(k, !v), v);
    });
    return equal;
  }
};

// build/dev/javascript/gleam_stdlib/gleam_stdlib.mjs
var Nil = void 0;
var NOT_FOUND = {};
function identity(x) {
  return x;
}
function parse_int(value3) {
  if (/^[-+]?(\d+)$/.test(value3)) {
    return new Ok(parseInt(value3));
  } else {
    return new Error(Nil);
  }
}
function to_string(term) {
  return term.toString();
}
function string_length(string3) {
  if (string3 === "") {
    return 0;
  }
  const iterator = graphemes_iterator(string3);
  if (iterator) {
    let i = 0;
    for (const _ of iterator) {
      i++;
    }
    return i;
  } else {
    return string3.match(/./gsu).length;
  }
}
function graphemes(string3) {
  const iterator = graphemes_iterator(string3);
  if (iterator) {
    return List.fromArray(Array.from(iterator).map((item) => item.segment));
  } else {
    return List.fromArray(string3.match(/./gsu));
  }
}
function graphemes_iterator(string3) {
  if (Intl && Intl.Segmenter) {
    return new Intl.Segmenter().segment(string3)[Symbol.iterator]();
  }
}
function pop_grapheme(string3) {
  let first3;
  const iterator = graphemes_iterator(string3);
  if (iterator) {
    first3 = iterator.next().value?.segment;
  } else {
    first3 = string3.match(/./su)?.[0];
  }
  if (first3) {
    return new Ok([first3, string3.slice(first3.length)]);
  } else {
    return new Error(Nil);
  }
}
function lowercase(string3) {
  return string3.toLowerCase();
}
function split(xs, pattern) {
  return List.fromArray(xs.split(pattern));
}
function concat2(xs) {
  let result = "";
  for (const x of xs) {
    result = result + x;
  }
  return result;
}
function starts_with(haystack, needle) {
  return haystack.startsWith(needle);
}
var unicode_whitespaces = [
  " ",
  // Space
  "	",
  // Horizontal tab
  "\n",
  // Line feed
  "\v",
  // Vertical tab
  "\f",
  // Form feed
  "\r",
  // Carriage return
  "\x85",
  // Next line
  "\u2028",
  // Line separator
  "\u2029"
  // Paragraph separator
].join();
var left_trim_regex = new RegExp(`^([${unicode_whitespaces}]*)`, "g");
var right_trim_regex = new RegExp(`([${unicode_whitespaces}]*)$`, "g");
function truncate(float3) {
  return Math.trunc(float3);
}
function compile_regex(pattern, options) {
  try {
    let flags = "gu";
    if (options.case_insensitive)
      flags += "i";
    if (options.multi_line)
      flags += "m";
    return new Ok(new RegExp(pattern, flags));
  } catch (error) {
    const number = (error.columnNumber || 0) | 0;
    return new Error(new CompileError(error.message, number));
  }
}
function regex_scan(regex, string3) {
  const matches = Array.from(string3.matchAll(regex)).map((match) => {
    const content = match[0];
    const submatches = [];
    for (let n = match.length - 1; n > 0; n--) {
      if (match[n]) {
        submatches[n - 1] = new Some(match[n]);
        continue;
      }
      if (submatches.length > 0) {
        submatches[n - 1] = new None();
      }
    }
    return new Match(content, List.fromArray(submatches));
  });
  return List.fromArray(matches);
}
function new_map() {
  return Dict.new();
}
function map_to_list(map7) {
  return List.fromArray(map7.entries());
}
function map_get(map7, key) {
  const value3 = map7.get(key, NOT_FOUND);
  if (value3 === NOT_FOUND) {
    return new Error(Nil);
  }
  return new Ok(value3);
}
function map_insert(key, value3, map7) {
  return map7.set(key, value3);
}
function classify_dynamic(data) {
  if (typeof data === "string") {
    return "String";
  } else if (typeof data === "boolean") {
    return "Bool";
  } else if (data instanceof Result) {
    return "Result";
  } else if (data instanceof List) {
    return "List";
  } else if (data instanceof BitArray) {
    return "BitArray";
  } else if (data instanceof Dict) {
    return "Dict";
  } else if (Number.isInteger(data)) {
    return "Int";
  } else if (Array.isArray(data)) {
    return `Tuple of ${data.length} elements`;
  } else if (typeof data === "number") {
    return "Float";
  } else if (data === null) {
    return "Null";
  } else if (data === void 0) {
    return "Nil";
  } else {
    const type = typeof data;
    return type.charAt(0).toUpperCase() + type.slice(1);
  }
}
function decoder_error(expected, got) {
  return decoder_error_no_classify(expected, classify_dynamic(got));
}
function decoder_error_no_classify(expected, got) {
  return new Error(
    List.fromArray([new DecodeError(expected, got, List.fromArray([]))])
  );
}
function decode_string(data) {
  return typeof data === "string" ? new Ok(data) : decoder_error("String", data);
}
function decode_int(data) {
  return Number.isInteger(data) ? new Ok(data) : decoder_error("Int", data);
}
function decode_bool(data) {
  return typeof data === "boolean" ? new Ok(data) : decoder_error("Bool", data);
}
function decode_list(data) {
  if (Array.isArray(data)) {
    return new Ok(List.fromArray(data));
  }
  return data instanceof List ? new Ok(data) : decoder_error("List", data);
}
function decode_field(value3, name) {
  const not_a_map_error = () => decoder_error("Dict", value3);
  if (value3 instanceof Dict || value3 instanceof WeakMap || value3 instanceof Map) {
    const entry = map_get(value3, name);
    return new Ok(entry.isOk() ? new Some(entry[0]) : new None());
  } else if (value3 === null) {
    return not_a_map_error();
  } else if (Object.getPrototypeOf(value3) == Object.prototype) {
    return try_get_field(value3, name, () => new Ok(new None()));
  } else {
    return try_get_field(value3, name, not_a_map_error);
  }
}
function try_get_field(value3, field2, or_else) {
  try {
    return field2 in value3 ? new Ok(new Some(value3[field2])) : or_else();
  } catch {
    return or_else();
  }
}

// build/dev/javascript/gleam_stdlib/gleam/dict.mjs
function new$() {
  return new_map();
}
function insert(dict2, key, value3) {
  return map_insert(key, value3, dict2);
}
function reverse_and_concat(loop$remaining, loop$accumulator) {
  while (true) {
    let remaining = loop$remaining;
    let accumulator = loop$accumulator;
    if (remaining.hasLength(0)) {
      return accumulator;
    } else {
      let item = remaining.head;
      let rest = remaining.tail;
      loop$remaining = rest;
      loop$accumulator = prepend(item, accumulator);
    }
  }
}
function do_keys_acc(loop$list, loop$acc) {
  while (true) {
    let list2 = loop$list;
    let acc = loop$acc;
    if (list2.hasLength(0)) {
      return reverse_and_concat(acc, toList([]));
    } else {
      let x = list2.head;
      let xs = list2.tail;
      loop$list = xs;
      loop$acc = prepend(x[0], acc);
    }
  }
}
function do_keys(dict2) {
  let list_of_pairs = map_to_list(dict2);
  return do_keys_acc(list_of_pairs, toList([]));
}
function keys(dict2) {
  return do_keys(dict2);
}

// build/dev/javascript/gleam_json/gleam_json_ffi.mjs
function json_to_string(json) {
  return JSON.stringify(json);
}
function object(entries) {
  return Object.fromEntries(entries);
}
function identity2(x) {
  return x;
}
function decode(string3) {
  try {
    const result = JSON.parse(string3);
    return new Ok(result);
  } catch (err) {
    return new Error(getJsonDecodeError(err, string3));
  }
}
function getJsonDecodeError(stdErr, json) {
  if (isUnexpectedEndOfInput(stdErr))
    return new UnexpectedEndOfInput();
  return toUnexpectedByteError(stdErr, json);
}
function isUnexpectedEndOfInput(err) {
  const unexpectedEndOfInputRegex = /((unexpected (end|eof))|(end of data)|(unterminated string)|(json( parse error|\.parse)\: expected '(\:|\}|\])'))/i;
  return unexpectedEndOfInputRegex.test(err.message);
}
function toUnexpectedByteError(err, json) {
  let converters = [
    v8UnexpectedByteError,
    oldV8UnexpectedByteError,
    jsCoreUnexpectedByteError,
    spidermonkeyUnexpectedByteError
  ];
  for (let converter of converters) {
    let result = converter(err, json);
    if (result)
      return result;
  }
  return new UnexpectedByte("", 0);
}
function v8UnexpectedByteError(err) {
  const regex = /unexpected token '(.)', ".+" is not valid JSON/i;
  const match = regex.exec(err.message);
  if (!match)
    return null;
  const byte = toHex(match[1]);
  return new UnexpectedByte(byte, -1);
}
function oldV8UnexpectedByteError(err) {
  const regex = /unexpected token (.) in JSON at position (\d+)/i;
  const match = regex.exec(err.message);
  if (!match)
    return null;
  const byte = toHex(match[1]);
  const position = Number(match[2]);
  return new UnexpectedByte(byte, position);
}
function spidermonkeyUnexpectedByteError(err, json) {
  const regex = /(unexpected character|expected .*) at line (\d+) column (\d+)/i;
  const match = regex.exec(err.message);
  if (!match)
    return null;
  const line = Number(match[2]);
  const column = Number(match[3]);
  const position = getPositionFromMultiline(line, column, json);
  const byte = toHex(json[position]);
  return new UnexpectedByte(byte, position);
}
function jsCoreUnexpectedByteError(err) {
  const regex = /unexpected (identifier|token) "(.)"/i;
  const match = regex.exec(err.message);
  if (!match)
    return null;
  const byte = toHex(match[2]);
  return new UnexpectedByte(byte, 0);
}
function toHex(char) {
  return "0x" + char.charCodeAt(0).toString(16).toUpperCase();
}
function getPositionFromMultiline(line, column, string3) {
  if (line === 1)
    return column - 1;
  let currentLn = 1;
  let position = 0;
  string3.split("").find((char, idx) => {
    if (char === "\n")
      currentLn += 1;
    if (currentLn === line) {
      position = idx + column;
      return true;
    }
    return false;
  });
  return position;
}

// build/dev/javascript/gleam_json/gleam/json.mjs
var UnexpectedEndOfInput = class extends CustomType {
};
var UnexpectedByte = class extends CustomType {
  constructor(byte, position) {
    super();
    this.byte = byte;
    this.position = position;
  }
};
var UnexpectedFormat = class extends CustomType {
  constructor(x0) {
    super();
    this[0] = x0;
  }
};
function do_decode(json, decoder) {
  return then$(
    decode(json),
    (dynamic_value) => {
      let _pipe = decoder(dynamic_value);
      return map_error(
        _pipe,
        (var0) => {
          return new UnexpectedFormat(var0);
        }
      );
    }
  );
}
function decode2(json, decoder) {
  return do_decode(json, decoder);
}
function to_string5(json) {
  return json_to_string(json);
}
function string2(input2) {
  return identity2(input2);
}
function object2(entries) {
  return object(entries);
}

// build/dev/javascript/gleam_stdlib/gleam/uri.mjs
var Uri = class extends CustomType {
  constructor(scheme, userinfo, host, port, path, query, fragment) {
    super();
    this.scheme = scheme;
    this.userinfo = userinfo;
    this.host = host;
    this.port = port;
    this.path = path;
    this.query = query;
    this.fragment = fragment;
  }
};
function regex_submatches(pattern, string3) {
  let _pipe = pattern;
  let _pipe$1 = compile(_pipe, new Options(true, false));
  let _pipe$2 = nil_error(_pipe$1);
  let _pipe$3 = map3(
    _pipe$2,
    (_capture) => {
      return scan(_capture, string3);
    }
  );
  let _pipe$4 = try$(_pipe$3, first);
  let _pipe$5 = map3(_pipe$4, (m) => {
    return m.submatches;
  });
  return unwrap2(_pipe$5, toList([]));
}
function noneify_query(x) {
  if (x instanceof None) {
    return new None();
  } else {
    let x$1 = x[0];
    let $ = pop_grapheme2(x$1);
    if ($.isOk() && $[0][0] === "?") {
      let query = $[0][1];
      return new Some(query);
    } else {
      return new None();
    }
  }
}
function noneify_empty_string(x) {
  if (x instanceof Some && x[0] === "") {
    return new None();
  } else if (x instanceof None) {
    return new None();
  } else {
    return x;
  }
}
function extra_required(loop$list, loop$remaining) {
  while (true) {
    let list2 = loop$list;
    let remaining = loop$remaining;
    if (remaining === 0) {
      return 0;
    } else if (list2.hasLength(0)) {
      return remaining;
    } else {
      let xs = list2.tail;
      loop$list = xs;
      loop$remaining = remaining - 1;
    }
  }
}
function pad_list(list2, size) {
  let _pipe = list2;
  return append(
    _pipe,
    repeat(new None(), extra_required(list2, size))
  );
}
function split_authority(authority) {
  let $ = unwrap(authority, "");
  if ($ === "") {
    return [new None(), new None(), new None()];
  } else if ($ === "//") {
    return [new None(), new Some(""), new None()];
  } else {
    let authority$1 = $;
    let matches = (() => {
      let _pipe = "^(//)?((.*)@)?(\\[[a-zA-Z0-9:.]*\\]|[^:]*)(:(\\d*))?";
      let _pipe$1 = regex_submatches(_pipe, authority$1);
      return pad_list(_pipe$1, 6);
    })();
    if (matches.hasLength(6)) {
      let userinfo = matches.tail.tail.head;
      let host = matches.tail.tail.tail.head;
      let port = matches.tail.tail.tail.tail.tail.head;
      let userinfo$1 = noneify_empty_string(userinfo);
      let host$1 = noneify_empty_string(host);
      let port$1 = (() => {
        let _pipe = port;
        let _pipe$1 = unwrap(_pipe, "");
        let _pipe$2 = parse(_pipe$1);
        return from_result(_pipe$2);
      })();
      return [userinfo$1, host$1, port$1];
    } else {
      return [new None(), new None(), new None()];
    }
  }
}
function do_parse(uri_string) {
  let pattern = "^(([a-z][a-z0-9\\+\\-\\.]*):)?(//([^/?#]*))?([^?#]*)(\\?([^#]*))?(#.*)?";
  let matches = (() => {
    let _pipe = pattern;
    let _pipe$1 = regex_submatches(_pipe, uri_string);
    return pad_list(_pipe$1, 8);
  })();
  let $ = (() => {
    if (matches.hasLength(8)) {
      let scheme2 = matches.tail.head;
      let authority_with_slashes = matches.tail.tail.head;
      let path2 = matches.tail.tail.tail.tail.head;
      let query_with_question_mark = matches.tail.tail.tail.tail.tail.head;
      let fragment2 = matches.tail.tail.tail.tail.tail.tail.tail.head;
      return [
        scheme2,
        authority_with_slashes,
        path2,
        query_with_question_mark,
        fragment2
      ];
    } else {
      return [new None(), new None(), new None(), new None(), new None()];
    }
  })();
  let scheme = $[0];
  let authority = $[1];
  let path = $[2];
  let query = $[3];
  let fragment = $[4];
  let scheme$1 = noneify_empty_string(scheme);
  let path$1 = unwrap(path, "");
  let query$1 = noneify_query(query);
  let $1 = split_authority(authority);
  let userinfo = $1[0];
  let host = $1[1];
  let port = $1[2];
  let fragment$1 = (() => {
    let _pipe = fragment;
    let _pipe$1 = to_result(_pipe, void 0);
    let _pipe$2 = try$(_pipe$1, pop_grapheme2);
    let _pipe$3 = map3(_pipe$2, second);
    return from_result(_pipe$3);
  })();
  let scheme$2 = (() => {
    let _pipe = scheme$1;
    let _pipe$1 = noneify_empty_string(_pipe);
    return map(_pipe$1, lowercase2);
  })();
  return new Ok(
    new Uri(scheme$2, userinfo, host, port, path$1, query$1, fragment$1)
  );
}
function parse2(uri_string) {
  return do_parse(uri_string);
}
function do_remove_dot_segments(loop$input, loop$accumulator) {
  while (true) {
    let input2 = loop$input;
    let accumulator = loop$accumulator;
    if (input2.hasLength(0)) {
      return reverse(accumulator);
    } else {
      let segment = input2.head;
      let rest = input2.tail;
      let accumulator$1 = (() => {
        if (segment === "") {
          let accumulator$12 = accumulator;
          return accumulator$12;
        } else if (segment === ".") {
          let accumulator$12 = accumulator;
          return accumulator$12;
        } else if (segment === ".." && accumulator.hasLength(0)) {
          return toList([]);
        } else if (segment === ".." && accumulator.atLeastLength(1)) {
          let accumulator$12 = accumulator.tail;
          return accumulator$12;
        } else {
          let segment$1 = segment;
          let accumulator$12 = accumulator;
          return prepend(segment$1, accumulator$12);
        }
      })();
      loop$input = rest;
      loop$accumulator = accumulator$1;
    }
  }
}
function remove_dot_segments(input2) {
  return do_remove_dot_segments(input2, toList([]));
}
function path_segments(path) {
  return remove_dot_segments(split3(path, "/"));
}
function to_string6(uri) {
  let parts = (() => {
    let $ = uri.fragment;
    if ($ instanceof Some) {
      let fragment = $[0];
      return toList(["#", fragment]);
    } else {
      return toList([]);
    }
  })();
  let parts$1 = (() => {
    let $ = uri.query;
    if ($ instanceof Some) {
      let query = $[0];
      return prepend("?", prepend(query, parts));
    } else {
      return parts;
    }
  })();
  let parts$2 = prepend(uri.path, parts$1);
  let parts$3 = (() => {
    let $ = uri.host;
    let $1 = starts_with2(uri.path, "/");
    if ($ instanceof Some && !$1 && $[0] !== "") {
      let host = $[0];
      return prepend("/", parts$2);
    } else {
      return parts$2;
    }
  })();
  let parts$4 = (() => {
    let $ = uri.host;
    let $1 = uri.port;
    if ($ instanceof Some && $1 instanceof Some) {
      let port = $1[0];
      return prepend(":", prepend(to_string2(port), parts$3));
    } else {
      return parts$3;
    }
  })();
  let parts$5 = (() => {
    let $ = uri.scheme;
    let $1 = uri.userinfo;
    let $2 = uri.host;
    if ($ instanceof Some && $1 instanceof Some && $2 instanceof Some) {
      let s = $[0];
      let u = $1[0];
      let h = $2[0];
      return prepend(
        s,
        prepend(
          "://",
          prepend(u, prepend("@", prepend(h, parts$4)))
        )
      );
    } else if ($ instanceof Some && $1 instanceof None && $2 instanceof Some) {
      let s = $[0];
      let h = $2[0];
      return prepend(s, prepend("://", prepend(h, parts$4)));
    } else if ($ instanceof Some && $1 instanceof Some && $2 instanceof None) {
      let s = $[0];
      return prepend(s, prepend(":", parts$4));
    } else if ($ instanceof Some && $1 instanceof None && $2 instanceof None) {
      let s = $[0];
      return prepend(s, prepend(":", parts$4));
    } else if ($ instanceof None && $1 instanceof None && $2 instanceof Some) {
      let h = $2[0];
      return prepend("//", prepend(h, parts$4));
    } else {
      return parts$4;
    }
  })();
  return concat3(parts$5);
}

// build/dev/javascript/gleam_stdlib/gleam/bool.mjs
function to_int(bool3) {
  if (!bool3) {
    return 0;
  } else {
    return 1;
  }
}
function guard(requirement, consequence, alternative) {
  if (requirement) {
    return consequence;
  } else {
    return alternative();
  }
}

// build/dev/javascript/lustre/lustre/effect.mjs
var Effect = class extends CustomType {
  constructor(all) {
    super();
    this.all = all;
  }
};
function custom(run3) {
  return new Effect(
    toList([
      (actions) => {
        return run3(actions.dispatch, actions.emit, actions.select);
      }
    ])
  );
}
function from2(effect) {
  return custom((dispatch, _, _1) => {
    return effect(dispatch);
  });
}
function none() {
  return new Effect(toList([]));
}
function batch(effects) {
  return new Effect(
    fold(
      effects,
      toList([]),
      (b, _use1) => {
        let a2 = _use1.all;
        return append(b, a2);
      }
    )
  );
}

// build/dev/javascript/lustre/lustre/internals/vdom.mjs
var Text = class extends CustomType {
  constructor(content) {
    super();
    this.content = content;
  }
};
var Element = class extends CustomType {
  constructor(key, namespace, tag, attrs, children2, self_closing, void$) {
    super();
    this.key = key;
    this.namespace = namespace;
    this.tag = tag;
    this.attrs = attrs;
    this.children = children2;
    this.self_closing = self_closing;
    this.void = void$;
  }
};
var Map2 = class extends CustomType {
  constructor(subtree) {
    super();
    this.subtree = subtree;
  }
};
var Attribute = class extends CustomType {
  constructor(x0, x1, as_property) {
    super();
    this[0] = x0;
    this[1] = x1;
    this.as_property = as_property;
  }
};
var Event = class extends CustomType {
  constructor(x0, x1) {
    super();
    this[0] = x0;
    this[1] = x1;
  }
};
function attribute_to_event_handler(attribute2) {
  if (attribute2 instanceof Attribute) {
    return new Error(void 0);
  } else {
    let name = attribute2[0];
    let handler = attribute2[1];
    let name$1 = drop_left(name, 2);
    return new Ok([name$1, handler]);
  }
}
function do_element_list_handlers(elements2, handlers2, key) {
  return index_fold(
    elements2,
    handlers2,
    (handlers3, element2, index4) => {
      let key$1 = key + "-" + to_string2(index4);
      return do_handlers(element2, handlers3, key$1);
    }
  );
}
function do_handlers(loop$element, loop$handlers, loop$key) {
  while (true) {
    let element2 = loop$element;
    let handlers2 = loop$handlers;
    let key = loop$key;
    if (element2 instanceof Text) {
      return handlers2;
    } else if (element2 instanceof Map2) {
      let subtree = element2.subtree;
      loop$element = subtree();
      loop$handlers = handlers2;
      loop$key = key;
    } else if (element2 instanceof Element) {
      let attrs = element2.attrs;
      let children2 = element2.children;
      let handlers$1 = fold(
        attrs,
        handlers2,
        (handlers3, attr) => {
          let $ = attribute_to_event_handler(attr);
          if ($.isOk()) {
            let name = $[0][0];
            let handler = $[0][1];
            return insert(handlers3, key + "-" + name, handler);
          } else {
            return handlers3;
          }
        }
      );
      return do_element_list_handlers(children2, handlers$1, key);
    } else {
      let elements2 = element2.elements;
      return do_element_list_handlers(elements2, handlers2, key);
    }
  }
}
function handlers(element2) {
  return do_handlers(element2, new$(), "0");
}

// build/dev/javascript/lustre/lustre/attribute.mjs
function attribute(name, value3) {
  return new Attribute(name, from(value3), false);
}
function property(name, value3) {
  return new Attribute(name, from(value3), true);
}
function on(name, handler) {
  return new Event("on" + name, handler);
}
function class$(name) {
  return attribute("class", name);
}
function id(name) {
  return attribute("id", name);
}
function type_(name) {
  return attribute("type", name);
}
function value(val) {
  return attribute("value", val);
}
function autocomplete(name) {
  return attribute("autocomplete", name);
}
function disabled(is_disabled) {
  return property("disabled", is_disabled);
}
function required(is_required) {
  return property("required", is_required);
}
function for$(id2) {
  return attribute("for", id2);
}
function href(uri) {
  return attribute("href", uri);
}
function src(uri) {
  return attribute("src", uri);
}
function alt(text3) {
  return attribute("alt", text3);
}

// build/dev/javascript/lustre/lustre/element.mjs
function element(tag, attrs, children2) {
  if (tag === "area") {
    return new Element("", "", tag, attrs, toList([]), false, true);
  } else if (tag === "base") {
    return new Element("", "", tag, attrs, toList([]), false, true);
  } else if (tag === "br") {
    return new Element("", "", tag, attrs, toList([]), false, true);
  } else if (tag === "col") {
    return new Element("", "", tag, attrs, toList([]), false, true);
  } else if (tag === "embed") {
    return new Element("", "", tag, attrs, toList([]), false, true);
  } else if (tag === "hr") {
    return new Element("", "", tag, attrs, toList([]), false, true);
  } else if (tag === "img") {
    return new Element("", "", tag, attrs, toList([]), false, true);
  } else if (tag === "input") {
    return new Element("", "", tag, attrs, toList([]), false, true);
  } else if (tag === "link") {
    return new Element("", "", tag, attrs, toList([]), false, true);
  } else if (tag === "meta") {
    return new Element("", "", tag, attrs, toList([]), false, true);
  } else if (tag === "param") {
    return new Element("", "", tag, attrs, toList([]), false, true);
  } else if (tag === "source") {
    return new Element("", "", tag, attrs, toList([]), false, true);
  } else if (tag === "track") {
    return new Element("", "", tag, attrs, toList([]), false, true);
  } else if (tag === "wbr") {
    return new Element("", "", tag, attrs, toList([]), false, true);
  } else {
    return new Element("", "", tag, attrs, children2, false, false);
  }
}
function text(content) {
  return new Text(content);
}
function none2() {
  return new Text("");
}

// build/dev/javascript/gleam_stdlib/gleam/set.mjs
var Set2 = class extends CustomType {
  constructor(dict2) {
    super();
    this.dict = dict2;
  }
};
function new$3() {
  return new Set2(new$());
}

// build/dev/javascript/lustre/lustre/internals/patch.mjs
var Diff = class extends CustomType {
  constructor(x0) {
    super();
    this[0] = x0;
  }
};
var Emit = class extends CustomType {
  constructor(x0, x1) {
    super();
    this[0] = x0;
    this[1] = x1;
  }
};
var Init = class extends CustomType {
  constructor(x0, x1) {
    super();
    this[0] = x0;
    this[1] = x1;
  }
};
function is_empty_element_diff(diff3) {
  return isEqual(diff3.created, new$()) && isEqual(
    diff3.removed,
    new$3()
  ) && isEqual(diff3.updated, new$());
}

// build/dev/javascript/lustre/lustre/internals/runtime.mjs
var Attrs = class extends CustomType {
  constructor(x0) {
    super();
    this[0] = x0;
  }
};
var Batch = class extends CustomType {
  constructor(x0, x1) {
    super();
    this[0] = x0;
    this[1] = x1;
  }
};
var Debug = class extends CustomType {
  constructor(x0) {
    super();
    this[0] = x0;
  }
};
var Dispatch = class extends CustomType {
  constructor(x0) {
    super();
    this[0] = x0;
  }
};
var Emit2 = class extends CustomType {
  constructor(x0, x1) {
    super();
    this[0] = x0;
    this[1] = x1;
  }
};
var Event2 = class extends CustomType {
  constructor(x0, x1) {
    super();
    this[0] = x0;
    this[1] = x1;
  }
};
var Shutdown = class extends CustomType {
};
var Subscribe = class extends CustomType {
  constructor(x0, x1) {
    super();
    this[0] = x0;
    this[1] = x1;
  }
};
var Unsubscribe = class extends CustomType {
  constructor(x0) {
    super();
    this[0] = x0;
  }
};
var ForceModel = class extends CustomType {
  constructor(x0) {
    super();
    this[0] = x0;
  }
};

// build/dev/javascript/lustre/vdom.ffi.mjs
function morph(prev, next, dispatch) {
  let out;
  let stack = [{ prev, next, parent: prev.parentNode }];
  while (stack.length) {
    let { prev: prev2, next: next2, parent } = stack.pop();
    while (next2.subtree !== void 0)
      next2 = next2.subtree();
    if (next2.content !== void 0) {
      if (!prev2) {
        const created = document.createTextNode(next2.content);
        parent.appendChild(created);
        out ??= created;
      } else if (prev2.nodeType === Node.TEXT_NODE) {
        if (prev2.textContent !== next2.content)
          prev2.textContent = next2.content;
        out ??= prev2;
      } else {
        const created = document.createTextNode(next2.content);
        parent.replaceChild(created, prev2);
        out ??= created;
      }
    } else if (next2.tag !== void 0) {
      const created = createElementNode({
        prev: prev2,
        next: next2,
        dispatch,
        stack
      });
      if (!prev2) {
        parent.appendChild(created);
      } else if (prev2 !== created) {
        parent.replaceChild(created, prev2);
      }
      out ??= created;
    } else if (next2.elements !== void 0) {
      for (const fragmentElement of forceChild(next2)) {
        stack.unshift({ prev: prev2, next: fragmentElement, parent });
        prev2 = prev2?.nextSibling;
      }
    }
  }
  return out;
}
function createElementNode({ prev, next, dispatch, stack }) {
  const namespace = next.namespace || "http://www.w3.org/1999/xhtml";
  const canMorph = prev && prev.nodeType === Node.ELEMENT_NODE && prev.localName === next.tag && prev.namespaceURI === (next.namespace || "http://www.w3.org/1999/xhtml");
  const el2 = canMorph ? prev : namespace ? document.createElementNS(namespace, next.tag) : document.createElement(next.tag);
  let handlersForEl;
  if (!registeredHandlers.has(el2)) {
    const emptyHandlers = /* @__PURE__ */ new Map();
    registeredHandlers.set(el2, emptyHandlers);
    handlersForEl = emptyHandlers;
  } else {
    handlersForEl = registeredHandlers.get(el2);
  }
  const prevHandlers = canMorph ? new Set(handlersForEl.keys()) : null;
  const prevAttributes = canMorph ? new Set(Array.from(prev.attributes, (a2) => a2.name)) : null;
  let className = null;
  let style = null;
  let innerHTML = null;
  if (canMorph && next.tag === "textarea") {
    const innertText = next.children[Symbol.iterator]().next().value?.content;
    if (innertText !== void 0)
      el2.value = innertText;
  }
  for (const attr of next.attrs) {
    const name = attr[0];
    const value3 = attr[1];
    if (attr.as_property) {
      if (el2[name] !== value3)
        el2[name] = value3;
      if (canMorph)
        prevAttributes.delete(name);
    } else if (name.startsWith("on")) {
      const eventName = name.slice(2);
      const callback = dispatch(value3, eventName === "input");
      if (!handlersForEl.has(eventName)) {
        el2.addEventListener(eventName, lustreGenericEventHandler);
      }
      handlersForEl.set(eventName, callback);
      if (canMorph)
        prevHandlers.delete(eventName);
    } else if (name.startsWith("data-lustre-on-")) {
      const eventName = name.slice(15);
      const callback = dispatch(lustreServerEventHandler);
      if (!handlersForEl.has(eventName)) {
        el2.addEventListener(eventName, lustreGenericEventHandler);
      }
      handlersForEl.set(eventName, callback);
      el2.setAttribute(name, value3);
    } else if (name === "class") {
      className = className === null ? value3 : className + " " + value3;
    } else if (name === "style") {
      style = style === null ? value3 : style + value3;
    } else if (name === "dangerous-unescaped-html") {
      innerHTML = value3;
    } else {
      if (el2.getAttribute(name) !== value3)
        el2.setAttribute(name, value3);
      if (name === "value" || name === "selected")
        el2[name] = value3;
      if (canMorph)
        prevAttributes.delete(name);
    }
  }
  if (className !== null) {
    el2.setAttribute("class", className);
    if (canMorph)
      prevAttributes.delete("class");
  }
  if (style !== null) {
    el2.setAttribute("style", style);
    if (canMorph)
      prevAttributes.delete("style");
  }
  if (canMorph) {
    for (const attr of prevAttributes) {
      el2.removeAttribute(attr);
    }
    for (const eventName of prevHandlers) {
      handlersForEl.delete(eventName);
      el2.removeEventListener(eventName, lustreGenericEventHandler);
    }
  }
  if (next.key !== void 0 && next.key !== "") {
    el2.setAttribute("data-lustre-key", next.key);
  } else if (innerHTML !== null) {
    el2.innerHTML = innerHTML;
    return el2;
  }
  let prevChild = el2.firstChild;
  let seenKeys = null;
  let keyedChildren = null;
  let incomingKeyedChildren = null;
  let firstChild = children(next).next().value;
  if (canMorph && firstChild !== void 0 && // Explicit checks are more verbose but truthy checks force a bunch of comparisons
  // we don't care about: it's never gonna be a number etc.
  firstChild.key !== void 0 && firstChild.key !== "") {
    seenKeys = /* @__PURE__ */ new Set();
    keyedChildren = getKeyedChildren(prev);
    incomingKeyedChildren = getKeyedChildren(next);
    for (const child of children(next)) {
      prevChild = diffKeyedChild(
        prevChild,
        child,
        el2,
        stack,
        incomingKeyedChildren,
        keyedChildren,
        seenKeys
      );
    }
  } else {
    for (const child of children(next)) {
      stack.unshift({ prev: prevChild, next: child, parent: el2 });
      prevChild = prevChild?.nextSibling;
    }
  }
  while (prevChild) {
    const next2 = prevChild.nextSibling;
    el2.removeChild(prevChild);
    prevChild = next2;
  }
  return el2;
}
var registeredHandlers = /* @__PURE__ */ new WeakMap();
function lustreGenericEventHandler(event2) {
  const target = event2.currentTarget;
  if (!registeredHandlers.has(target)) {
    target.removeEventListener(event2.type, lustreGenericEventHandler);
    return;
  }
  const handlersForEventTarget = registeredHandlers.get(target);
  if (!handlersForEventTarget.has(event2.type)) {
    target.removeEventListener(event2.type, lustreGenericEventHandler);
    return;
  }
  handlersForEventTarget.get(event2.type)(event2);
}
function lustreServerEventHandler(event2) {
  const el2 = event2.currentTarget;
  const tag = el2.getAttribute(`data-lustre-on-${event2.type}`);
  const data = JSON.parse(el2.getAttribute("data-lustre-data") || "{}");
  const include = JSON.parse(el2.getAttribute("data-lustre-include") || "[]");
  switch (event2.type) {
    case "input":
    case "change":
      include.push("target.value");
      break;
  }
  return {
    tag,
    data: include.reduce(
      (data2, property2) => {
        const path = property2.split(".");
        for (let i = 0, o = data2, e = event2; i < path.length; i++) {
          if (i === path.length - 1) {
            o[path[i]] = e[path[i]];
          } else {
            o[path[i]] ??= {};
            e = e[path[i]];
            o = o[path[i]];
          }
        }
        return data2;
      },
      { data }
    )
  };
}
function getKeyedChildren(el2) {
  const keyedChildren = /* @__PURE__ */ new Map();
  if (el2) {
    for (const child of children(el2)) {
      const key = child?.key || child?.getAttribute?.("data-lustre-key");
      if (key)
        keyedChildren.set(key, child);
    }
  }
  return keyedChildren;
}
function diffKeyedChild(prevChild, child, el2, stack, incomingKeyedChildren, keyedChildren, seenKeys) {
  while (prevChild && !incomingKeyedChildren.has(prevChild.getAttribute("data-lustre-key"))) {
    const nextChild = prevChild.nextSibling;
    el2.removeChild(prevChild);
    prevChild = nextChild;
  }
  if (keyedChildren.size === 0) {
    for (const currChild of children(child)) {
      stack.unshift({ prev: prevChild, next: currChild, parent: el2 });
      prevChild = prevChild?.nextSibling;
    }
    return prevChild;
  }
  if (seenKeys.has(child.key)) {
    console.warn(`Duplicate key found in Lustre vnode: ${child.key}`);
    stack.unshift({ prev: null, next: child, parent: el2 });
    return prevChild;
  }
  seenKeys.add(child.key);
  const keyedChild = keyedChildren.get(child.key);
  if (!keyedChild && !prevChild) {
    stack.unshift({ prev: null, next: child, parent: el2 });
    return prevChild;
  }
  if (!keyedChild && prevChild !== null) {
    const placeholder = document.createTextNode("");
    el2.insertBefore(placeholder, prevChild);
    stack.unshift({ prev: placeholder, next: child, parent: el2 });
    return prevChild;
  }
  if (!keyedChild || keyedChild === prevChild) {
    stack.unshift({ prev: prevChild, next: child, parent: el2 });
    prevChild = prevChild?.nextSibling;
    return prevChild;
  }
  el2.insertBefore(keyedChild, prevChild);
  stack.unshift({ prev: keyedChild, next: child, parent: el2 });
  return prevChild;
}
function* children(element2) {
  for (const child of element2.children) {
    yield* forceChild(child);
  }
}
function* forceChild(element2) {
  if (element2.elements !== void 0) {
    for (const inner of element2.elements) {
      yield* forceChild(inner);
    }
  } else if (element2.subtree !== void 0) {
    yield* forceChild(element2.subtree());
  } else {
    yield element2;
  }
}

// build/dev/javascript/lustre/lustre.ffi.mjs
var LustreClientApplication = class _LustreClientApplication {
  /**
   * @template Flags
   *
   * @param {object} app
   * @param {(flags: Flags) => [Model, Lustre.Effect<Msg>]} app.init
   * @param {(msg: Msg, model: Model) => [Model, Lustre.Effect<Msg>]} app.update
   * @param {(model: Model) => Lustre.Element<Msg>} app.view
   * @param {string | HTMLElement} selector
   * @param {Flags} flags
   *
   * @returns {Gleam.Ok<(action: Lustre.Action<Lustre.Client, Msg>>) => void>}
   */
  static start({ init: init4, update: update2, view: view2 }, selector, flags) {
    if (!is_browser())
      return new Error(new NotABrowser());
    const root = selector instanceof HTMLElement ? selector : document.querySelector(selector);
    if (!root)
      return new Error(new ElementNotFound(selector));
    const app = new _LustreClientApplication(root, init4(flags), update2, view2);
    return new Ok((action) => app.send(action));
  }
  /**
   * @param {Element} root
   * @param {[Model, Lustre.Effect<Msg>]} init
   * @param {(model: Model, msg: Msg) => [Model, Lustre.Effect<Msg>]} update
   * @param {(model: Model) => Lustre.Element<Msg>} view
   *
   * @returns {LustreClientApplication}
   */
  constructor(root, [init4, effects], update2, view2) {
    this.root = root;
    this.#model = init4;
    this.#update = update2;
    this.#view = view2;
    this.#tickScheduled = window.requestAnimationFrame(
      () => this.#tick(effects.all.toArray(), true)
    );
  }
  /** @type {Element} */
  root;
  /**
   * @param {Lustre.Action<Lustre.Client, Msg>} action
   *
   * @returns {void}
   */
  send(action) {
    if (action instanceof Debug) {
      if (action[0] instanceof ForceModel) {
        this.#tickScheduled = window.cancelAnimationFrame(this.#tickScheduled);
        this.#queue = [];
        this.#model = action[0][0];
        const vdom = this.#view(this.#model);
        const dispatch = (handler, immediate = false) => (event2) => {
          const result = handler(event2);
          if (result instanceof Ok) {
            this.send(new Dispatch(result[0], immediate));
          }
        };
        const prev = this.root.firstChild ?? this.root.appendChild(document.createTextNode(""));
        morph(prev, vdom, dispatch);
      }
    } else if (action instanceof Dispatch) {
      const msg = action[0];
      const immediate = action[1] ?? false;
      this.#queue.push(msg);
      if (immediate) {
        this.#tickScheduled = window.cancelAnimationFrame(this.#tickScheduled);
        this.#tick();
      } else if (!this.#tickScheduled) {
        this.#tickScheduled = window.requestAnimationFrame(() => this.#tick());
      }
    } else if (action instanceof Emit2) {
      const event2 = action[0];
      const data = action[1];
      this.root.dispatchEvent(
        new CustomEvent(event2, {
          detail: data,
          bubbles: true,
          composed: true
        })
      );
    } else if (action instanceof Shutdown) {
      this.#tickScheduled = window.cancelAnimationFrame(this.#tickScheduled);
      this.#model = null;
      this.#update = null;
      this.#view = null;
      this.#queue = null;
      while (this.root.firstChild) {
        this.root.firstChild.remove();
      }
    }
  }
  /** @type {Model} */
  #model;
  /** @type {(model: Model, msg: Msg) => [Model, Lustre.Effect<Msg>]} */
  #update;
  /** @type {(model: Model) => Lustre.Element<Msg>} */
  #view;
  /** @type {Array<Msg>} */
  #queue = [];
  /** @type {number | undefined} */
  #tickScheduled;
  /**
   * @param {Lustre.Effect<Msg>[]} effects
   * @param {boolean} isFirstRender
   */
  #tick(effects = [], isFirstRender = false) {
    this.#tickScheduled = void 0;
    if (!this.#flush(effects, isFirstRender))
      return;
    const vdom = this.#view(this.#model);
    const dispatch = (handler, immediate = false) => (event2) => {
      const result = handler(event2);
      if (result instanceof Ok) {
        this.send(new Dispatch(result[0], immediate));
      }
    };
    const prev = this.root.firstChild ?? this.root.appendChild(document.createTextNode(""));
    morph(prev, vdom, dispatch);
  }
  #flush(effects = [], didUpdate = false) {
    while (this.#queue.length > 0) {
      const msg = this.#queue.shift();
      const [next, effect] = this.#update(this.#model, msg);
      didUpdate ||= this.#model !== next;
      effects = effects.concat(effect.all.toArray());
      this.#model = next;
    }
    while (effects.length > 0) {
      const effect = effects.shift();
      const dispatch = (msg) => this.send(new Dispatch(msg));
      const emit2 = (event2, data) => this.root.dispatchEvent(
        new CustomEvent(event2, {
          detail: data,
          bubbles: true,
          composed: true
        })
      );
      const select = () => {
      };
      effect({ dispatch, emit: emit2, select });
    }
    if (this.#queue.length > 0) {
      return this.#flush(effects, didUpdate);
    } else {
      return didUpdate;
    }
  }
};
var start = LustreClientApplication.start;
var LustreServerApplication = class _LustreServerApplication {
  static start({ init: init4, update: update2, view: view2, on_attribute_change }, flags) {
    const app = new _LustreServerApplication(
      init4(flags),
      update2,
      view2,
      on_attribute_change
    );
    return new Ok((action) => app.send(action));
  }
  constructor([model, effects], update2, view2, on_attribute_change) {
    this.#model = model;
    this.#update = update2;
    this.#view = view2;
    this.#html = view2(model);
    this.#onAttributeChange = on_attribute_change;
    this.#renderers = /* @__PURE__ */ new Map();
    this.#handlers = handlers(this.#html);
    this.#tick(effects.all.toArray());
  }
  send(action) {
    if (action instanceof Attrs) {
      for (const attr of action[0]) {
        const decoder = this.#onAttributeChange.get(attr[0]);
        if (!decoder)
          continue;
        const msg = decoder(attr[1]);
        if (msg instanceof Error)
          continue;
        this.#queue.push(msg);
      }
      this.#tick();
    } else if (action instanceof Batch) {
      this.#queue = this.#queue.concat(action[0].toArray());
      this.#tick(action[1].all.toArray());
    } else if (action instanceof Debug) {
    } else if (action instanceof Dispatch) {
      this.#queue.push(action[0]);
      this.#tick();
    } else if (action instanceof Emit2) {
      const event2 = new Emit(action[0], action[1]);
      for (const [_, renderer] of this.#renderers) {
        renderer(event2);
      }
    } else if (action instanceof Event2) {
      const handler = this.#handlers.get(action[0]);
      if (!handler)
        return;
      const msg = handler(action[1]);
      if (msg instanceof Error)
        return;
      this.#queue.push(msg[0]);
      this.#tick();
    } else if (action instanceof Subscribe) {
      const attrs = keys(this.#onAttributeChange);
      const patch = new Init(attrs, this.#html);
      this.#renderers = this.#renderers.set(action[0], action[1]);
      action[1](patch);
    } else if (action instanceof Unsubscribe) {
      this.#renderers = this.#renderers.delete(action[0]);
    }
  }
  #model;
  #update;
  #queue;
  #view;
  #html;
  #renderers;
  #handlers;
  #onAttributeChange;
  #tick(effects = []) {
    if (!this.#flush(false, effects))
      return;
    const vdom = this.#view(this.#model);
    const diff3 = elements(this.#html, vdom);
    if (!is_empty_element_diff(diff3)) {
      const patch = new Diff(diff3);
      for (const [_, renderer] of this.#renderers) {
        renderer(patch);
      }
    }
    this.#html = vdom;
    this.#handlers = diff3.handlers;
  }
  #flush(didUpdate = false, effects = []) {
    while (this.#queue.length > 0) {
      const msg = this.#queue.shift();
      const [next, effect] = this.#update(this.#model, msg);
      didUpdate ||= this.#model !== next;
      effects = effects.concat(effect.all.toArray());
      this.#model = next;
    }
    while (effects.length > 0) {
      const effect = effects.shift();
      const dispatch = (msg) => this.send(new Dispatch(msg));
      const emit2 = (event2, data) => this.root.dispatchEvent(
        new CustomEvent(event2, {
          detail: data,
          bubbles: true,
          composed: true
        })
      );
      const select = () => {
      };
      effect({ dispatch, emit: emit2, select });
    }
    if (this.#queue.length > 0) {
      return this.#flush(didUpdate, effects);
    } else {
      return didUpdate;
    }
  }
};
var start_server_application = LustreServerApplication.start;
var is_browser = () => globalThis.window && window.document;
var prevent_default = (event2) => event2.preventDefault();

// build/dev/javascript/lustre/lustre.mjs
var App = class extends CustomType {
  constructor(init4, update2, view2, on_attribute_change) {
    super();
    this.init = init4;
    this.update = update2;
    this.view = view2;
    this.on_attribute_change = on_attribute_change;
  }
};
var ElementNotFound = class extends CustomType {
  constructor(selector) {
    super();
    this.selector = selector;
  }
};
var NotABrowser = class extends CustomType {
};
function application(init4, update2, view2) {
  return new App(init4, update2, view2, new None());
}
function start2(app, selector, flags) {
  return guard(
    !is_browser(),
    new Error(new NotABrowser()),
    () => {
      return start(app, selector, flags);
    }
  );
}

// build/dev/javascript/lustre/lustre/element/html.mjs
function text2(content) {
  return text(content);
}
function body(attrs, children2) {
  return element("body", attrs, children2);
}
function h1(attrs, children2) {
  return element("h1", attrs, children2);
}
function h2(attrs, children2) {
  return element("h2", attrs, children2);
}
function h3(attrs, children2) {
  return element("h3", attrs, children2);
}
function main(attrs, children2) {
  return element("main", attrs, children2);
}
function nav(attrs, children2) {
  return element("nav", attrs, children2);
}
function div(attrs, children2) {
  return element("div", attrs, children2);
}
function li(attrs, children2) {
  return element("li", attrs, children2);
}
function p(attrs, children2) {
  return element("p", attrs, children2);
}
function ul(attrs, children2) {
  return element("ul", attrs, children2);
}
function a(attrs, children2) {
  return element("a", attrs, children2);
}
function span(attrs, children2) {
  return element("span", attrs, children2);
}
function strong(attrs, children2) {
  return element("strong", attrs, children2);
}
function img(attrs) {
  return element("img", attrs, toList([]));
}
function button(attrs, children2) {
  return element("button", attrs, children2);
}
function form(attrs, children2) {
  return element("form", attrs, children2);
}
function input(attrs) {
  return element("input", attrs, toList([]));
}
function label(attrs, children2) {
  return element("label", attrs, children2);
}

// build/dev/javascript/gleam_http/gleam/http.mjs
var Get = class extends CustomType {
};
var Post = class extends CustomType {
};
var Head = class extends CustomType {
};
var Put = class extends CustomType {
};
var Delete = class extends CustomType {
};
var Trace = class extends CustomType {
};
var Connect = class extends CustomType {
};
var Options2 = class extends CustomType {
};
var Patch = class extends CustomType {
};
var Http = class extends CustomType {
};
var Https = class extends CustomType {
};
function method_to_string(method) {
  if (method instanceof Connect) {
    return "connect";
  } else if (method instanceof Delete) {
    return "delete";
  } else if (method instanceof Get) {
    return "get";
  } else if (method instanceof Head) {
    return "head";
  } else if (method instanceof Options2) {
    return "options";
  } else if (method instanceof Patch) {
    return "patch";
  } else if (method instanceof Post) {
    return "post";
  } else if (method instanceof Put) {
    return "put";
  } else if (method instanceof Trace) {
    return "trace";
  } else {
    let s = method[0];
    return s;
  }
}
function scheme_to_string(scheme) {
  if (scheme instanceof Http) {
    return "http";
  } else {
    return "https";
  }
}
function scheme_from_string(scheme) {
  let $ = lowercase2(scheme);
  if ($ === "http") {
    return new Ok(new Http());
  } else if ($ === "https") {
    return new Ok(new Https());
  } else {
    return new Error(void 0);
  }
}

// build/dev/javascript/gleam_http/gleam/http/request.mjs
var Request = class extends CustomType {
  constructor(method, headers, body2, scheme, host, port, path, query) {
    super();
    this.method = method;
    this.headers = headers;
    this.body = body2;
    this.scheme = scheme;
    this.host = host;
    this.port = port;
    this.path = path;
    this.query = query;
  }
};
function to_uri(request) {
  return new Uri(
    new Some(scheme_to_string(request.scheme)),
    new None(),
    new Some(request.host),
    request.port,
    request.path,
    request.query,
    new None()
  );
}
function from_uri(uri) {
  return then$(
    (() => {
      let _pipe = uri.scheme;
      let _pipe$1 = unwrap(_pipe, "");
      return scheme_from_string(_pipe$1);
    })(),
    (scheme) => {
      return then$(
        (() => {
          let _pipe = uri.host;
          return to_result(_pipe, void 0);
        })(),
        (host) => {
          let req = new Request(
            new Get(),
            toList([]),
            "",
            scheme,
            host,
            uri.port,
            uri.path,
            uri.query
          );
          return new Ok(req);
        }
      );
    }
  );
}
function set_header(request, key, value3) {
  let headers = key_set(request.headers, lowercase2(key), value3);
  return request.withFields({ headers });
}
function set_body(req, body2) {
  let method = req.method;
  let headers = req.headers;
  let scheme = req.scheme;
  let host = req.host;
  let port = req.port;
  let path = req.path;
  let query = req.query;
  return new Request(method, headers, body2, scheme, host, port, path, query);
}
function set_method(req, method) {
  return req.withFields({ method });
}
function to(url) {
  let _pipe = url;
  let _pipe$1 = parse2(_pipe);
  return then$(_pipe$1, from_uri);
}

// build/dev/javascript/gleam_http/gleam/http/response.mjs
var Response = class extends CustomType {
  constructor(status, headers, body2) {
    super();
    this.status = status;
    this.headers = headers;
    this.body = body2;
  }
};

// build/dev/javascript/gleam_javascript/gleam_javascript_ffi.mjs
var PromiseLayer = class _PromiseLayer {
  constructor(promise) {
    this.promise = promise;
  }
  static wrap(value3) {
    return value3 instanceof Promise ? new _PromiseLayer(value3) : value3;
  }
  static unwrap(value3) {
    return value3 instanceof _PromiseLayer ? value3.promise : value3;
  }
};
function resolve(value3) {
  return Promise.resolve(PromiseLayer.wrap(value3));
}
function then_await(promise, fn) {
  return promise.then((value3) => fn(PromiseLayer.unwrap(value3)));
}
function map_promise(promise, fn) {
  return promise.then(
    (value3) => PromiseLayer.wrap(fn(PromiseLayer.unwrap(value3)))
  );
}
function rescue(promise, fn) {
  return promise.catch((error) => fn(error));
}

// build/dev/javascript/gleam_javascript/gleam/javascript/promise.mjs
function tap(promise, callback) {
  let _pipe = promise;
  return map_promise(
    _pipe,
    (a2) => {
      callback(a2);
      return a2;
    }
  );
}
function try_await(promise, callback) {
  let _pipe = promise;
  return then_await(
    _pipe,
    (result) => {
      if (result.isOk()) {
        let a2 = result[0];
        return callback(a2);
      } else {
        let e = result[0];
        return resolve(new Error(e));
      }
    }
  );
}

// build/dev/javascript/gleam_fetch/ffi.mjs
async function raw_send(request) {
  try {
    return new Ok(await fetch(request));
  } catch (error) {
    return new Error(new NetworkError(error.toString()));
  }
}
function from_fetch_response(response) {
  return new Response(
    response.status,
    List.fromArray([...response.headers]),
    response
  );
}
function to_fetch_request(request) {
  let url = to_string6(to_uri(request));
  let method = method_to_string(request.method).toUpperCase();
  let options = {
    headers: make_headers(request.headers),
    method
  };
  if (method !== "GET" && method !== "HEAD")
    options.body = request.body;
  return new globalThis.Request(url, options);
}
function make_headers(headersList) {
  let headers = new globalThis.Headers();
  for (let [k, v] of headersList)
    headers.append(k.toLowerCase(), v);
  return headers;
}
async function read_text_body(response) {
  let body2;
  try {
    body2 = await response.body.text();
  } catch (error) {
    return new Error(new UnableToReadBody());
  }
  return new Ok(response.withFields({ body: body2 }));
}

// build/dev/javascript/gleam_fetch/gleam/fetch.mjs
var NetworkError = class extends CustomType {
  constructor(x0) {
    super();
    this[0] = x0;
  }
};
var UnableToReadBody = class extends CustomType {
};
function send(request) {
  let _pipe = request;
  let _pipe$1 = to_fetch_request(_pipe);
  let _pipe$2 = raw_send(_pipe$1);
  return try_await(
    _pipe$2,
    (resp) => {
      return resolve(new Ok(from_fetch_response(resp)));
    }
  );
}

// build/dev/javascript/lustre_http/lustre_http.mjs
var BadUrl = class extends CustomType {
  constructor(x0) {
    super();
    this[0] = x0;
  }
};
var InternalServerError = class extends CustomType {
  constructor(x0) {
    super();
    this[0] = x0;
  }
};
var JsonError = class extends CustomType {
  constructor(x0) {
    super();
    this[0] = x0;
  }
};
var NetworkError2 = class extends CustomType {
};
var NotFound = class extends CustomType {
};
var OtherError = class extends CustomType {
  constructor(x0, x1) {
    super();
    this[0] = x0;
    this[1] = x1;
  }
};
var Unauthorized = class extends CustomType {
};
var ExpectTextResponse = class extends CustomType {
  constructor(run3) {
    super();
    this.run = run3;
  }
};
function do_send(req, expect, dispatch) {
  let _pipe = send(req);
  let _pipe$1 = try_await(_pipe, read_text_body);
  let _pipe$2 = map_promise(
    _pipe$1,
    (response) => {
      if (response.isOk()) {
        let res = response[0];
        return expect.run(new Ok(res));
      } else {
        return expect.run(new Error(new NetworkError2()));
      }
    }
  );
  let _pipe$3 = rescue(
    _pipe$2,
    (_) => {
      return expect.run(new Error(new NetworkError2()));
    }
  );
  tap(_pipe$3, dispatch);
  return void 0;
}
function get2(url, expect) {
  return from2(
    (dispatch) => {
      let $ = to(url);
      if ($.isOk()) {
        let req = $[0];
        return do_send(req, expect, dispatch);
      } else {
        return dispatch(expect.run(new Error(new BadUrl(url))));
      }
    }
  );
}
function post(url, body2, expect) {
  return from2(
    (dispatch) => {
      let $ = to(url);
      if ($.isOk()) {
        let req = $[0];
        let _pipe = req;
        let _pipe$1 = set_method(_pipe, new Post());
        let _pipe$2 = set_header(
          _pipe$1,
          "Content-Type",
          "application/json"
        );
        let _pipe$3 = set_body(_pipe$2, to_string5(body2));
        return do_send(_pipe$3, expect, dispatch);
      } else {
        return dispatch(expect.run(new Error(new BadUrl(url))));
      }
    }
  );
}
function response_to_result(response) {
  if (response instanceof Response && (200 <= response.status && response.status <= 299)) {
    let status = response.status;
    let body2 = response.body;
    return new Ok(body2);
  } else if (response instanceof Response && response.status === 401) {
    return new Error(new Unauthorized());
  } else if (response instanceof Response && response.status === 404) {
    return new Error(new NotFound());
  } else if (response instanceof Response && response.status === 500) {
    let body2 = response.body;
    return new Error(new InternalServerError(body2));
  } else {
    let code = response.status;
    let body2 = response.body;
    return new Error(new OtherError(code, body2));
  }
}
function expect_json(decoder, to_msg) {
  return new ExpectTextResponse(
    (response) => {
      let _pipe = response;
      let _pipe$1 = then$(_pipe, response_to_result);
      let _pipe$2 = then$(
        _pipe$1,
        (body2) => {
          let $ = decode2(body2, decoder);
          if ($.isOk()) {
            let json = $[0];
            return new Ok(json);
          } else {
            let json_error = $[0];
            return new Error(new JsonError(json_error));
          }
        }
      );
      return to_msg(_pipe$2);
    }
  );
}

// build/dev/javascript/modem/modem.ffi.mjs
var defaults = {
  handle_external_links: false,
  handle_internal_links: true
};
var initial_location = window?.location?.href;
var do_init = (dispatch, options = defaults) => {
  document.addEventListener("click", (event2) => {
    const a2 = find_anchor(event2.target);
    if (!a2)
      return;
    try {
      const url = new URL(a2.href);
      const uri = uri_from_url(url);
      const is_external = url.host !== window.location.host;
      if (!options.handle_external_links && is_external)
        return;
      if (!options.handle_internal_links && !is_external)
        return;
      event2.preventDefault();
      if (!is_external) {
        window.history.pushState({}, "", a2.href);
        window.requestAnimationFrame(() => {
          if (url.hash) {
            document.getElementById(url.hash.slice(1))?.scrollIntoView();
          }
        });
      }
      return dispatch(uri);
    } catch {
      return;
    }
  });
  window.addEventListener("popstate", (e) => {
    e.preventDefault();
    const url = new URL(window.location.href);
    const uri = uri_from_url(url);
    window.requestAnimationFrame(() => {
      if (url.hash) {
        document.getElementById(url.hash.slice(1))?.scrollIntoView();
      }
    });
    dispatch(uri);
  });
  window.addEventListener("modem-push", ({ detail }) => {
    dispatch(detail);
  });
  window.addEventListener("modem-replace", ({ detail }) => {
    dispatch(detail);
  });
};
var find_anchor = (el2) => {
  if (!el2 || el2.tagName === "BODY") {
    return null;
  } else if (el2.tagName === "A") {
    return el2;
  } else {
    return find_anchor(el2.parentElement);
  }
};
var uri_from_url = (url) => {
  return new Uri(
    /* scheme   */
    url.protocol ? new Some(url.protocol.slice(0, -1)) : new None(),
    /* userinfo */
    new None(),
    /* host     */
    url.hostname ? new Some(url.hostname) : new None(),
    /* port     */
    url.port ? new Some(Number(url.port)) : new None(),
    /* path     */
    url.pathname,
    /* query    */
    url.search ? new Some(url.search.slice(1)) : new None(),
    /* fragment */
    url.hash ? new Some(url.hash.slice(1)) : new None()
  );
};

// build/dev/javascript/modem/modem.mjs
function init2(handler) {
  return from2(
    (dispatch) => {
      return guard(
        !is_browser(),
        void 0,
        () => {
          return do_init(
            (uri) => {
              let _pipe = uri;
              let _pipe$1 = handler(_pipe);
              return dispatch(_pipe$1);
            }
          );
        }
      );
    }
  );
}

// build/dev/javascript/shared/shared.mjs
var Gift = class extends CustomType {
  constructor(id2, name, pic, link, selected_by) {
    super();
    this.id = id2;
    this.name = name;
    this.pic = pic;
    this.link = link;
    this.selected_by = selected_by;
  }
};
var User = class extends CustomType {
  constructor(id2, name, email, password, confirmed) {
    super();
    this.id = id2;
    this.name = name;
    this.email = email;
    this.password = password;
    this.confirmed = confirmed;
  }
};
var server_url = "http://localhost:8000";

// build/dev/javascript/client/client/pages/event.mjs
function event_view() {
  return main(
    toList([class$("w-full max-w-6xl p-8 mt-12 flex flex-col items-center")]),
    toList([
      h1(
        toList([
          attribute("style", "font-family: 'Pacifico', cursive;"),
          class$("text-5xl text-white font-bold mb-12")
        ]),
        toList([text("Detalhes do Evento")])
      ),
      div(
        toList([class$("w-full flex flex-col lg:flex-row gap-8")]),
        toList([
          div(
            toList([class$("flex-1")]),
            toList([
              img(
                toList([
                  class$("rounded-lg shadow-lg w-full mb-8 lg:mb-0"),
                  alt("Local da Festa"),
                  src("/priv/static/paiol.jpg")
                ])
              )
            ])
          ),
          div(
            toList([
              class$("flex-1 bg-white text-gray-800 rounded-lg shadow-lg p-6")
            ]),
            toList([
              h2(
                toList([class$("text-3xl font-bold text-pink-700 mb-4")]),
                toList([text("Anivers\xE1rio de 15 Anos da Laura")])
              ),
              p(
                toList([class$("text-lg text-gray-700 mb-4")]),
                toList([text("Pomp\xE9u, MG - 14 de Dezembro de 2024")])
              ),
              p(
                toList([class$("text-lg text-gray-700 mb-8")]),
                toList([text("Hor\xE1rio: 19:00")])
              ),
              h2(
                toList([class$("text-2xl font-semibold text-pink-700 mb-4")]),
                toList([text("Detalhes do Evento")])
              ),
              p(
                toList([class$("text-lg text-gray-700 mb-2")]),
                toList([
                  strong(toList([]), toList([text("Endere\xE7o:")])),
                  text("R. Padre Jo\xE3o Porto, 579 - Centro, Pomp\xE9u")
                ])
              ),
              p(
                toList([class$("text-lg text-gray-700 mb-4")]),
                toList([
                  text(
                    'O evento ser\xE1 realizado no sal\xE3o de festas do "Paiol Mineiro", um ambiente requintado e aconchegante, perfeito para uma noite inesquec\xEDvel.'
                  )
                ])
              ),
              h2(
                toList([class$("text-2xl font-semibold text-pink-700 mb-4")]),
                toList([text("Traje")])
              ),
              p(
                toList([class$("text-lg text-gray-700 mb-2")]),
                toList([
                  strong(toList([]), toList([text("Traje:")])),
                  text("Esporte Fino")
                ])
              ),
              p(
                toList([class$("text-lg text-gray-700")]),
                toList([
                  text(
                    "Sugerimos aos convidados vestirem-se confortavelmente para uma noite de muita divers\xE3o."
                  )
                ])
              )
            ])
          )
        ])
      )
    ])
  );
}

// build/dev/javascript/client/client/state.mjs
var Home = class extends CustomType {
};
var Login = class extends CustomType {
};
var GiftsPage = class extends CustomType {
};
var EventPage = class extends CustomType {
};
var PhotosPage = class extends CustomType {
};
var NotFound2 = class extends CustomType {
};
var Model2 = class extends CustomType {
  constructor(route, user, gifts, select_gift, photos2, login_name, login_email, login_password, login_error, confirm_presence, sign_up_name, sign_up_email, sign_up_password, sign_up_error) {
    super();
    this.route = route;
    this.user = user;
    this.gifts = gifts;
    this.select_gift = select_gift;
    this.photos = photos2;
    this.login_name = login_name;
    this.login_email = login_email;
    this.login_password = login_password;
    this.login_error = login_error;
    this.confirm_presence = confirm_presence;
    this.sign_up_name = sign_up_name;
    this.sign_up_email = sign_up_email;
    this.sign_up_password = sign_up_password;
    this.sign_up_error = sign_up_error;
  }
};
var OnRouteChange = class extends CustomType {
  constructor(x0) {
    super();
    this[0] = x0;
  }
};
var UserRecieved = class extends CustomType {
  constructor(x0) {
    super();
    this[0] = x0;
  }
};
var GiftsRecieved = class extends CustomType {
  constructor(x0) {
    super();
    this[0] = x0;
  }
};
var PhotosRecieved = class extends CustomType {
  constructor(x0) {
    super();
    this[0] = x0;
  }
};
var LoginUpdateName = class extends CustomType {
  constructor(value3) {
    super();
    this.value = value3;
  }
};
var LoginUpdateEmail = class extends CustomType {
  constructor(value3) {
    super();
    this.value = value3;
  }
};
var LoginUpdatePassword = class extends CustomType {
  constructor(value3) {
    super();
    this.value = value3;
  }
};
var LoginUpdateError = class extends CustomType {
  constructor(value3) {
    super();
    this.value = value3;
  }
};
var RequestLogin = class extends CustomType {
};

// build/dev/javascript/client/client/pages/gifts.mjs
function gift_widget(gift) {
  return div(
    toList([class$("size-150 bg-white p-4 rounded-lg shadow-lg")]),
    (() => {
      let $ = gift.selected_by;
      if ($ === 0) {
        return toList([
          div(
            toList([class$("bg-white p-4 rounded-lg shadow-lg")]),
            toList([
              img(
                toList([
                  class$("w-200 h-150 rounded-lg"),
                  alt("Presente " + to_string2(gift.id)),
                  src(gift.pic)
                ])
              ),
              h3(
                toList([class$("text-xl font-semibold text-pink-700 mt-4")]),
                toList([text(gift.name)])
              ),
              a(
                toList([
                  class$("text-pink-600 hover:text-pink-800 underline"),
                  href(gift.link)
                ]),
                toList([text("Ver refer\xEAncia")])
              ),
              button(
                toList([
                  class$(
                    "mt-4 w-full bg-pink-600 hover:bg-pink-700 text-white font-bold py-2 px-4 rounded-full transition duration-300"
                  )
                ]),
                toList([text("Escolher")])
              )
            ])
          )
        ]);
      } else {
        return toList([
          div(
            toList([
              class$("absolute inset-0 bg-black opacity-50 rounded-lg z-10")
            ]),
            toList([])
          ),
          div(
            toList([
              class$("absolute inset-0 flex items-center justify-center z-20")
            ]),
            toList([
              span(
                toList([class$("bg-red-600 text-white px-4 py-1 rounded-full")]),
                toList([text("Selecionado")])
              )
            ])
          ),
          img(
            toList([
              class$("w-200 h-150 rounded-lg grayscale z-0"),
              alt("Presente " + to_string2(gift.id)),
              src(gift.pic)
            ])
          ),
          h3(
            toList([class$("text-xl font-semibold text-pink-700 mt-4")]),
            toList([text(gift.name)])
          ),
          a(
            toList([
              class$("text-pink-600 hover:text-pink-800 underline"),
              href(gift.link)
            ]),
            toList([text("Ver refer\xEAncia")])
          ),
          button(
            toList([
              disabled(true),
              class$(
                "mt-4 w-full bg-gray-500 text-white font-bold py-2 px-4 rounded-full cursor-not-allowed"
              )
            ]),
            toList([text("Escolher")])
          )
        ]);
      }
    })()
  );
}
function gifts_view(model) {
  return main(
    toList([class$("w-full max-w-6xl p-8 mt-12 flex flex-col items-center")]),
    toList([
      h1(
        toList([
          attribute("style", "font-family: 'Pacifico', cursive;"),
          class$("text-5xl text-white font-bold mb-12")
        ]),
        toList([text("Lista de Presentes")])
      ),
      div(
        toList([
          class$("grid grid-cols-1 sm:grid-cols-2 lg:grid-cols-3 gap-8 w-full")
        ]),
        map2(model.gifts, gift_widget)
      )
    ])
  );
}

// build/dev/javascript/lustre/lustre/event.mjs
function on2(name, handler) {
  return on(name, handler);
}
function value2(event2) {
  let _pipe = event2;
  return field("target", field("value", string))(
    _pipe
  );
}
function on_input(msg) {
  return on2(
    "input",
    (event2) => {
      let _pipe = value2(event2);
      return map3(_pipe, msg);
    }
  );
}
function on_submit(msg) {
  return on2(
    "submit",
    (event2) => {
      let $ = prevent_default(event2);
      return new Ok(msg);
    }
  );
}

// build/dev/javascript/rada/rada_ffi.mjs
function get_year_month_day() {
  let date = /* @__PURE__ */ new Date();
  return [date.getFullYear(), date.getMonth() + 1, date.getDate()];
}

// build/dev/javascript/rada/rada/date.mjs
var Jan = class extends CustomType {
};
var Feb = class extends CustomType {
};
var Mar = class extends CustomType {
};
var Apr = class extends CustomType {
};
var May = class extends CustomType {
};
var Jun = class extends CustomType {
};
var Jul = class extends CustomType {
};
var Aug = class extends CustomType {
};
var Sep = class extends CustomType {
};
var Oct = class extends CustomType {
};
var Nov = class extends CustomType {
};
var Dec = class extends CustomType {
};
var RD = class extends CustomType {
  constructor(x0) {
    super();
    this[0] = x0;
  }
};
var OrdinalDate = class extends CustomType {
  constructor(year2, ordinal_day) {
    super();
    this.year = year2;
    this.ordinal_day = ordinal_day;
  }
};
var CalendarDate = class extends CustomType {
  constructor(year2, month, day) {
    super();
    this.year = year2;
    this.month = month;
    this.day = day;
  }
};
var Years = class extends CustomType {
};
var Months = class extends CustomType {
};
var Weeks = class extends CustomType {
};
var Days = class extends CustomType {
};
function month_to_number(month) {
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
function number_to_month(month_number) {
  let $ = max(1, month_number);
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
function floor_div(dividend, divisor) {
  let $ = (dividend > 0 && divisor < 0 || dividend < 0 && divisor > 0) && remainderInt(
    dividend,
    divisor
  ) !== 0;
  if ($) {
    return divideInt(dividend, divisor) - 1;
  } else {
    return divideInt(dividend, divisor);
  }
}
function days_before_year(year1) {
  let year$1 = year1 - 1;
  let leap_years = floor_div(year$1, 4) - floor_div(year$1, 100) + floor_div(
    year$1,
    400
  );
  return 365 * year$1 + leap_years;
}
function modulo_unwrap(dividend, divisor) {
  let remainder = remainderInt(dividend, divisor);
  let $ = remainder > 0 && divisor < 0 || remainder < 0 && divisor > 0;
  if ($) {
    return remainder + divisor;
  } else {
    return remainder;
  }
}
function is_leap_year(year2) {
  return modulo_unwrap(year2, 4) === 0 && modulo_unwrap(year2, 100) !== 0 || modulo_unwrap(
    year2,
    400
  ) === 0;
}
function days_in_month(year2, month) {
  if (month instanceof Jan) {
    return 31;
  } else if (month instanceof Feb) {
    let $ = is_leap_year(year2);
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
    let year2 = loop$year;
    let month = loop$month;
    let ordinal_day = loop$ordinal_day;
    let month_days = days_in_month(year2, month);
    let month_number$1 = month_to_number(month);
    let $ = month_number$1 < 12 && ordinal_day > month_days;
    if ($) {
      loop$year = year2;
      loop$month = number_to_month(month_number$1 + 1);
      loop$ordinal_day = ordinal_day - month_days;
    } else {
      return new CalendarDate(year2, month, ordinal_day);
    }
  }
}
function days_before_month(year2, month) {
  let leap_days = to_int(is_leap_year(year2));
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
function from_calendar_date(year2, month, day) {
  return new RD(
    days_before_year(year2) + days_before_month(year2, month) + clamp(
      day,
      1,
      days_in_month(year2, month)
    )
  );
}
function today() {
  let $ = get_year_month_day();
  let year$1 = $[0];
  let month_number$1 = $[1];
  let day$1 = $[2];
  return from_calendar_date(year$1, number_to_month(month_number$1), day$1);
}
function div_with_remainder(a2, b) {
  return [floor_div(a2, b), modulo_unwrap(a2, b)];
}
function year(date) {
  let rd = date[0];
  let $ = div_with_remainder(rd, 146097);
  let n400 = $[0];
  let r400 = $[1];
  let $1 = div_with_remainder(r400, 36524);
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
  return n400 * 400 + n100 * 100 + n4 * 4 + n1 + n;
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
    ordinal_date.ordinal_day
  );
}
function to_months(rd) {
  let calendar_date = to_calendar_date(new RD(rd));
  let whole_months = 12 * (calendar_date.year - 1) + (month_to_number(
    calendar_date.month
  ) - 1);
  let fraction = divideFloat(to_float(calendar_date.day), 100);
  return to_float(whole_months) + fraction;
}
function diff2(unit, date1, date2) {
  let rd1 = date1[0];
  let rd2 = date2[0];
  if (unit instanceof Years) {
    let _pipe = to_months(rd2) - to_months(rd1);
    let _pipe$1 = truncate2(_pipe);
    let _pipe$2 = divide(_pipe$1, 12);
    return unwrap2(_pipe$2, 0);
  } else if (unit instanceof Months) {
    let _pipe = to_months(rd2) - to_months(rd1);
    return truncate2(_pipe);
  } else if (unit instanceof Weeks) {
    let _pipe = divide(rd2 - rd1, 7);
    return unwrap2(_pipe, 0);
  } else {
    return rd2 - rd1;
  }
}

// build/dev/javascript/client/client/pages/home.mjs
function countdown() {
  return diff2(
    new Days(),
    today(),
    from_calendar_date(2024, new Dec(), 14)
  );
}
function home_view() {
  return main(
    toList([class$("w-full max-w-6xl p-8 mt-12 flex flex-col items-center")]),
    toList([
      h1(
        toList([
          attribute("style", "font-family: 'Pacifico', cursive;"),
          class$("text-5xl text-white font-bold mb-12")
        ]),
        toList([text("Laura 15 Anos")])
      ),
      h3(
        toList([class$("text-xl text-white mt-4")]),
        toList([text("14 de Dezembro de 2024")])
      ),
      div(
        toList([class$("text-center mt-6")]),
        toList([
          p(
            toList([class$("text-3xl text-white font-bold")]),
            toList([
              text("Faltam "),
              span(
                toList([class$("text-yellow-300"), id("countdown")]),
                toList([text(to_string2(countdown()))])
              ),
              text(" dias para a festa!")
            ])
          )
        ])
      ),
      div(
        toList([
          id("evento"),
          class$(
            "bg-white text-gray-800 rounded-lg shadow-lg p-12 max-w-4xl w-full mx-4 mt-12 border border-gray-200"
          )
        ]),
        toList([
          div(
            toList([class$("flex items-center justify-between mb-8")]),
            toList([
              img(
                toList([
                  class$(
                    "rounded-full shadow-md transform hover:scale-105 transition duration-500 w-1/3"
                  ),
                  alt("Laura's Birthday"),
                  src("/priv/static/profile.jpeg")
                ])
              ),
              div(
                toList([class$("flex-1 ml-12")]),
                toList([
                  h1(
                    toList([class$("text-5xl font-bold text-pink-600 mb-4")]),
                    toList([text("Anivers\xE1rio de 15 Anos de Laura")])
                  ),
                  p(
                    toList([class$("text-gray-600 text-lg mb-6")]),
                    toList([
                      text(
                        "Lhe convido para celebrar esse dia t\xE3o especial em minha vida, meus 15 anos! Confirme sua presen\xE7a at\xE9 o dia 06/12 para receber seu convite individual."
                      )
                    ])
                  ),
                  div(
                    toList([class$("space-x-4")]),
                    toList([
                      button(
                        toList([
                          class$(
                            "bg-pink-600 hover:bg-pink-700 text-white font-bold py-2 px-6 rounded-full shadow-lg transition duration-300 transform hover:scale-105"
                          )
                        ]),
                        toList([text("Confirmar Presen\xE7a")])
                      ),
                      button(
                        toList([
                          class$(
                            "bg-yellow-500 hover:bg-yellow-600 text-white font-bold py-2 px-6 rounded-full shadow-lg transition duration-300 transform hover:scale-105"
                          )
                        ]),
                        toList([text("Lista de Presentes")])
                      )
                    ])
                  )
                ])
              )
            ])
          ),
          div(
            toList([class$("bg-gray-100 p-6 rounded-lg shadow-inner")]),
            toList([
              h2(
                toList([class$("text-3xl font-semibold text-pink-700 mb-4")]),
                toList([text("Sobre Laura")])
              ),
              p(
                toList([class$("text-gray-700 text-lg")]),
                toList([
                  text(
                    "Laura est\xE1 completando 15 anos e queremos celebrar com todos que fazem parte de sua vida. A festa ser\xE1 cheia de alegria, m\xFAsica, e muita divers\xE3o. N\xE3o perca!"
                  )
                ])
              )
            ])
          )
        ])
      )
    ])
  );
}

// build/dev/javascript/client/client/pages/login.mjs
function login(model) {
  let decoder = decode5(
    (var0, var1, var2, var3, var4) => {
      return new User(var0, var1, var2, var3, var4);
    },
    field("id", int),
    field("name", string),
    field("email", string),
    field("password", string),
    field("confirmed", bool)
  );
  return post(
    server_url + "/auth/login",
    object2(
      toList([
        ["email", string2(model.login_email)],
        ["password", string2(model.login_password)]
      ])
    ),
    expect_json(
      decoder,
      (var0) => {
        return new UserRecieved(var0);
      }
    )
  );
}
function login_view(model) {
  return main(
    toList([class$("w-full max-w-6xl p-8 mt-12 flex flex-col items-center")]),
    toList([
      div(
        toList([class$("w-full max-w-md p-8 bg-white rounded-lg shadow-lg")]),
        toList([
          h1(
            toList([
              attribute("style", "font-family: 'Pacifico', cursive;"),
              class$("text-4xl text-pink-700 font-bold mb-12 text-center")
            ]),
            toList([text("Entrar ou Cadastrar")])
          ),
          form(
            toList([class$("space-y-6"), on_submit(new RequestLogin())]),
            toList([
              div(
                toList([]),
                toList([
                  label(
                    toList([
                      class$("block text-sm font-medium text-gray-700"),
                      for$("email")
                    ]),
                    toList([text("Nome")])
                  ),
                  input(
                    toList([
                      class$(
                        "mt-1 block w-full px-4 py-2 border border-gray-300 rounded-lg shadow-sm focus:ring-pink-500 focus:border-pink-500"
                      ),
                      on_input(
                        (var0) => {
                          return new LoginUpdateName(var0);
                        }
                      ),
                      id("name"),
                      type_("name"),
                      autocomplete("name"),
                      required(true),
                      value(model.login_name)
                    ])
                  )
                ])
              ),
              div(
                toList([]),
                toList([
                  label(
                    toList([
                      class$("block text-sm font-medium text-gray-700"),
                      for$("email")
                    ]),
                    toList([text("Email")])
                  ),
                  input(
                    toList([
                      class$(
                        "mt-1 block w-full px-4 py-2 border border-gray-300 rounded-lg shadow-sm focus:ring-pink-500 focus:border-pink-500"
                      ),
                      on_input(
                        (var0) => {
                          return new LoginUpdateEmail(var0);
                        }
                      ),
                      id("email"),
                      type_("email"),
                      autocomplete("email"),
                      required(true),
                      value(model.login_email)
                    ])
                  )
                ])
              ),
              div(
                toList([]),
                toList([
                  label(
                    toList([
                      class$("block text-sm font-medium text-gray-700"),
                      for$("password")
                    ]),
                    toList([text("Senha")])
                  ),
                  input(
                    toList([
                      class$(
                        "mt-1 block w-full px-4 py-2 border border-gray-300 rounded-lg shadow-sm focus:ring-pink-500 focus:border-pink-500"
                      ),
                      on_input(
                        (var0) => {
                          return new LoginUpdatePassword(var0);
                        }
                      ),
                      id("password"),
                      type_("password"),
                      required(true),
                      value(model.login_password)
                    ])
                  )
                ])
              ),
              div(
                toList([class$("flex items-center justify-center")]),
                toList([
                  button(
                    toList([
                      class$(
                        "w-full bg-pink-600 hover:bg-pink-700 text-white font-bold py-2 px-4 rounded-lg transition duration-300 transform hover:scale-105"
                      ),
                      type_("submit")
                    ]),
                    toList([text("Entrar")])
                  )
                ])
              ),
              (() => {
                let $ = model.login_error;
                if ($ instanceof Some) {
                  let err = $[0];
                  return p(
                    toList([class$("text-red-500 text-center")]),
                    toList([text("Error: " + err)])
                  );
                } else {
                  return none2();
                }
              })()
            ])
          )
        ])
      )
    ])
  );
}

// build/dev/javascript/client/client/pages/not_found.mjs
function not_found_view() {
  return div(
    toList([class$("flex items-center justify-center min-h-screen text-white")]),
    toList([strong(toList([]), toList([text2("404 Not Found")]))])
  );
}

// build/dev/javascript/client/client/pages/photos.mjs
function photo_widget(photo) {
  return img(
    toList([
      class$("w-full h-auto rounded-lg shadow-lg"),
      alt("Foto"),
      src(photo)
    ])
  );
}
function photos_view(model) {
  return main(
    toList([class$("w-full max-w-6xl p-8 mt-12 flex flex-col items-center")]),
    toList([
      h1(
        toList([
          attribute("style", "font-family: 'Pacifico', cursive;"),
          class$("text-5xl text-white font-bold mb-12")
        ]),
        toList([text("Fotos do Evento")])
      ),
      div(
        toList([
          class$("grid grid-cols-1 sm:grid-cols-2 lg:grid-cols-3 gap-8 w-full")
        ]),
        map2(model.photos, photo_widget)
      )
    ])
  );
}

// build/dev/javascript/client/ffi.mjs
function get_route() {
  return window.location.pathname;
}

// build/dev/javascript/client/client.mjs
function update(model, msg) {
  if (msg instanceof OnRouteChange) {
    let route = msg[0];
    return [model.withFields({ route }), none()];
  } else if (msg instanceof UserRecieved) {
    let user_result = msg[0];
    if (user_result.isOk()) {
      let user = user_result[0];
      return [model.withFields({ user: new Some(user) }), none()];
    } else {
      return [model, none()];
    }
  } else if (msg instanceof GiftsRecieved) {
    let gifts_result = msg[0];
    if (gifts_result.isOk()) {
      let gifts = gifts_result[0];
      return [model.withFields({ gifts }), none()];
    } else {
      return [model, none()];
    }
  } else if (msg instanceof PhotosRecieved) {
    let photos_result = msg[0];
    if (photos_result.isOk()) {
      let photos$1 = photos_result[0];
      return [model.withFields({ photos: photos$1 }), none()];
    } else {
      return [model, none()];
    }
  } else if (msg instanceof RequestLogin) {
    return [model, login(model)];
  } else if (msg instanceof LoginUpdateEmail) {
    let value3 = msg.value;
    return [model.withFields({ login_email: value3 }), none()];
  } else if (msg instanceof LoginUpdatePassword) {
    let value3 = msg.value;
    return [model.withFields({ login_password: value3 }), none()];
  } else if (msg instanceof LoginUpdateError) {
    let value3 = msg.value;
    return [model.withFields({ login_error: value3 }), none()];
  } else {
    return [model, none()];
  }
}
function get_route2() {
  let uri = (() => {
    let $2 = (() => {
      let _pipe = get_route();
      return parse2(_pipe);
    })();
    if ($2.isOk()) {
      let uri2 = $2[0];
      return uri2;
    } else {
      throw makeError("panic", "client", 128, "get_route", "Invalid uri", {});
    }
  })();
  let $ = (() => {
    let _pipe = uri.path;
    return path_segments(_pipe);
  })();
  if ($.hasLength(0)) {
    return new Home();
  } else if ($.hasLength(1) && $.head === "login") {
    return new Login();
  } else if ($.hasLength(1) && $.head === "gifts") {
    return new GiftsPage();
  } else if ($.hasLength(1) && $.head === "event") {
    return new EventPage();
  } else if ($.hasLength(1) && $.head === "photos") {
    return new PhotosPage();
  } else {
    return new NotFound2();
  }
}
function on_url_change(uri) {
  return new OnRouteChange(get_route2());
}
function get_gifts() {
  let url = server_url + "/gifts";
  let decoder = list(
    decode5(
      (var0, var1, var2, var3, var4) => {
        return new Gift(var0, var1, var2, var3, var4);
      },
      field("id", int),
      field("name", string),
      field("pic", string),
      field("link", string),
      field("selected_by", int)
    )
  );
  return get2(
    url,
    expect_json(
      decoder,
      (var0) => {
        return new GiftsRecieved(var0);
      }
    )
  );
}
function view(model) {
  return body(
    toList([
      class$(
        "bg-gradient-to-r from-pink-400 to-pink-200 min-h-screen flex flex-col items-center justify-start"
      ),
      id("app")
    ]),
    toList([
      nav(
        toList([
          class$(
            "w-full bg-white shadow-md py-4 px-8 flex justify-between items-center"
          )
        ]),
        toList([
          div(toList([]), toList([])),
          ul(
            toList([class$("flex space-x-8 text-pink-600 font-semibold")]),
            toList([
              li(
                toList([]),
                toList([
                  a(
                    toList([
                      class$("hover:text-pink-800 transition duration-300"),
                      href("/")
                    ]),
                    toList([text2("Home")])
                  )
                ])
              ),
              li(
                toList([]),
                toList([
                  a(
                    toList([
                      class$("hover:text-pink-800 transition duration-300"),
                      href("/event")
                    ]),
                    toList([text2("Evento")])
                  )
                ])
              ),
              li(
                toList([]),
                toList([
                  a(
                    toList([
                      class$("hover:text-pink-800 transition duration-300"),
                      href("/gifts")
                    ]),
                    toList([text2("Presentes")])
                  )
                ])
              ),
              li(
                toList([]),
                toList([
                  a(
                    toList([
                      class$("hover:text-pink-800 transition duration-300"),
                      href("/photos")
                    ]),
                    toList([text2("Fotos")])
                  )
                ])
              )
            ])
          ),
          nav(
            toList([class$("flex space-x-8 text-pink-600 font-semibold")]),
            toList([
              (() => {
                let $ = model.user;
                if ($ instanceof None) {
                  return a(
                    toList([
                      class$("hover:text-pink-800 transition duration-300"),
                      href("/login")
                    ]),
                    toList([text2("Login")])
                  );
                } else {
                  let user = $[0];
                  return a(
                    toList([
                      class$("hover:text-pink-800 transition duration-300")
                    ]),
                    toList([text2("Ol\xE1 " + user.name)])
                  );
                }
              })()
            ])
          )
        ])
      ),
      (() => {
        let $ = model.route;
        if ($ instanceof Home) {
          return home_view();
        } else if ($ instanceof EventPage) {
          return event_view();
        } else if ($ instanceof PhotosPage) {
          return photos_view(model);
        } else if ($ instanceof GiftsPage) {
          return gifts_view(model);
        } else if ($ instanceof Login) {
          return login_view(model);
        } else if ($ instanceof NotFound2) {
          return not_found_view();
        } else {
          return not_found_view();
        }
      })()
    ])
  );
}
var photos = /* @__PURE__ */ toList([
  "/priv/static/photo1.jpeg",
  "/priv/static/photo2.jpeg",
  "/priv/static/photo3.jpeg"
]);
function init3(_) {
  return [
    new Model2(
      get_route2(),
      new None(),
      toList([]),
      toList([]),
      photos,
      "",
      "",
      "",
      new None(),
      0,
      "",
      "",
      "",
      new None()
    ),
    batch(toList([init2(on_url_change), get_gifts()]))
  ];
}
function main2() {
  let _pipe = application(init3, update, view);
  return start2(_pipe, "#app", void 0);
}

// build/.lustre/entry.mjs
main2();
