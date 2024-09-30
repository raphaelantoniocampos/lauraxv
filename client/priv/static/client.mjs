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
    for (let i2 = array3.length - 1; i2 >= 0; --i2) {
      t = new NonEmpty(array3[i2], t);
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
  // @internal
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
  byteAt(index3) {
    return this.buffer[index3];
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
  sliceAfter(index3) {
    return new _BitArray(this.buffer.slice(index3));
  }
};
function byteArrayToInt(byteArray, start3, end, isBigEndian, isSigned) {
  let value3 = 0;
  if (isBigEndian) {
    for (let i2 = start3; i2 < end; i2++) {
      value3 = value3 * 256 + byteArray[i2];
    }
  } else {
    for (let i2 = end - 1; i2 >= start3; i2--) {
      value3 = value3 * 256 + byteArray[i2];
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
  let values2 = [x, y];
  while (values2.length) {
    let a2 = values2.pop();
    let b = values2.pop();
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
      values2.push(get3(a2, k), get3(b, k));
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
  return a2.buffer instanceof ArrayBuffer && a2.BYTES_PER_ELEMENT && !(a2.byteLength === b.byteLength && a2.every((n, i2) => n === b[i2]));
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

// build/dev/javascript/common/common.mjs
var Gift = class extends CustomType {
  constructor(id2, name2, pic, link, selected_by) {
    super();
    this.id = id2;
    this.name = name2;
    this.pic = pic;
    this.link = link;
    this.selected_by = selected_by;
  }
};
var Comment = class extends CustomType {
  constructor(name2, comment) {
    super();
    this.name = name2;
    this.comment = comment;
  }
};

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
function compile(pattern2, options) {
  return compile_regex(pattern2, options);
}
function scan(regex, string3) {
  return regex_scan(regex, string3);
}

// build/dev/javascript/gleam_stdlib/gleam/order.mjs
var Lt = class extends CustomType {
};
var Eq = class extends CustomType {
};
var Gt = class extends CustomType {
};

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
function compare(a2, b) {
  let $ = a2 === b;
  if ($) {
    return new Eq();
  } else {
    let $1 = a2 < b;
    if ($1) {
      return new Lt();
    } else {
      return new Gt();
    }
  }
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
function count_length(loop$list, loop$count) {
  while (true) {
    let list2 = loop$list;
    let count = loop$count;
    if (list2.atLeastLength(1)) {
      let list$1 = list2.tail;
      loop$list = list$1;
      loop$count = count + 1;
    } else {
      return count;
    }
  }
}
function length(list2) {
  return count_length(list2, 0);
}
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
function update_group(f) {
  return (groups, elem) => {
    let $ = get(groups, f(elem));
    if ($.isOk()) {
      let existing = $[0];
      return insert(groups, f(elem), prepend(elem, existing));
    } else {
      return insert(groups, f(elem), toList([elem]));
    }
  };
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
function group(list2, key) {
  return fold(list2, new$(), update_group(key));
}
function do_index_fold(loop$over, loop$acc, loop$with, loop$index) {
  while (true) {
    let over = loop$over;
    let acc = loop$acc;
    let with$ = loop$with;
    let index3 = loop$index;
    if (over.hasLength(0)) {
      return acc;
    } else {
      let first$1 = over.head;
      let rest$1 = over.tail;
      loop$over = rest$1;
      loop$acc = with$(acc, first$1, index3);
      loop$with = with$;
      loop$index = index3 + 1;
    }
  }
}
function index_fold(over, initial, fun) {
  return do_index_fold(over, initial, fun, 0);
}
function tail_recursive_range(loop$start, loop$stop, loop$acc) {
  while (true) {
    let start3 = loop$start;
    let stop = loop$stop;
    let acc = loop$acc;
    let $ = compare(start3, stop);
    if ($ instanceof Eq) {
      return prepend(stop, acc);
    } else if ($ instanceof Gt) {
      loop$start = start3;
      loop$stop = stop + 1;
      loop$acc = prepend(stop, acc);
    } else {
      loop$start = start3;
      loop$stop = stop - 1;
      loop$acc = prepend(stop, acc);
    }
  }
}
function range(start3, stop) {
  return tail_recursive_range(start3, stop, toList([]));
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
function append_builder(builder, suffix) {
  return add(builder, suffix);
}
function from_strings(strings) {
  return concat2(strings);
}
function from_string(string3) {
  return identity(string3);
}
function append2(builder, second2) {
  return append_builder(builder, from_string(second2));
}
function to_string3(builder) {
  return identity(builder);
}

// build/dev/javascript/gleam_stdlib/gleam/string.mjs
function length3(string3) {
  return string_length(string3);
}
function lowercase2(string3) {
  return lowercase(string3);
}
function uppercase2(string3) {
  return uppercase(string3);
}
function starts_with2(string3, prefix) {
  return starts_with(string3, prefix);
}
function append4(first3, second2) {
  let _pipe = first3;
  let _pipe$1 = from_string(_pipe);
  let _pipe$2 = append2(_pipe$1, second2);
  return to_string3(_pipe$2);
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
      let translated_idx = length3(string3) + idx;
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
    return slice(string3, num_graphemes, length3(string3) - num_graphemes);
  }
}
function capitalise(s) {
  let $ = pop_grapheme2(s);
  if ($.isOk()) {
    let first$1 = $[0][0];
    let rest = $[0][1];
    return append4(uppercase2(first$1), lowercase2(rest));
  } else {
    return "";
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
function shallow_list(value3) {
  return decode_list(value3);
}
function optional(decode6) {
  return (value3) => {
    return decode_option(value3, decode6);
  };
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
function push_path(error, name2) {
  let name$1 = from(name2);
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
function field(name2, inner_type) {
  return (value3) => {
    let missing_field_error = new DecodeError("field", "nothing", toList([]));
    return try$(
      decode_field(value3, name2),
      (maybe_inner) => {
        let _pipe = maybe_inner;
        let _pipe$1 = to_result(_pipe, toList([missing_field_error]));
        let _pipe$2 = try$(_pipe$1, inner_type);
        return map_errors(
          _pipe$2,
          (_capture) => {
            return push_path(_capture, name2);
          }
        );
      }
    );
  };
}
function optional_field(name2, inner_type) {
  return (value3) => {
    return try$(
      decode_field(value3, name2),
      (maybe_inner) => {
        if (maybe_inner instanceof None) {
          return new Ok(new None());
        } else {
          let dynamic_inner = maybe_inner[0];
          let _pipe = dynamic_inner;
          let _pipe$1 = decode_option(_pipe, inner_type);
          return map_errors(
            _pipe$1,
            (_capture) => {
              return push_path(_capture, name2);
            }
          );
        }
      }
    );
  };
}
function decode2(constructor, t1, t2) {
  return (value3) => {
    let $ = t1(value3);
    let $1 = t2(value3);
    if ($.isOk() && $1.isOk()) {
      let a2 = $[0];
      let b = $1[0];
      return new Ok(constructor(a2, b));
    } else {
      let a2 = $;
      let b = $1;
      return new Error(concat(toList([all_errors(a2), all_errors(b)])));
    }
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
  for (let i2 = 0; i2 < len; i2++) {
    hash = Math.imul(31, hash) + s.charCodeAt(i2) | 0;
  }
  return hash;
}
function hashNumber(n) {
  tempDataView.setFloat64(0, n);
  const i2 = tempDataView.getInt32(0);
  const j = tempDataView.getInt32(4);
  return Math.imul(73244475, i2 >> 16 ^ i2) ^ j;
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
    for (let i2 = 0; i2 < o.length; i2++) {
      h = Math.imul(31, h) + getHash(o[i2]) | 0;
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
    for (let i2 = 0; i2 < keys2.length; i2++) {
      const k = keys2[i2];
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
  for (let i2 = 0; i2 < len; ++i2) {
    out[i2] = arr[i2];
  }
  out[at] = val;
  return out;
}
function spliceIn(arr, at, val) {
  const len = arr.length;
  const out = new Array(len + 1);
  let i2 = 0;
  let g = 0;
  while (i2 < at) {
    out[g++] = arr[i2++];
  }
  out[g++] = val;
  while (i2 < len) {
    out[g++] = arr[i2++];
  }
  return out;
}
function spliceOut(arr, at) {
  const len = arr.length;
  const out = new Array(len - 1);
  let i2 = 0;
  let g = 0;
  while (i2 < at) {
    out[g++] = arr[i2++];
  }
  ++i2;
  while (i2 < len) {
    out[g++] = arr[i2++];
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
      for (let i2 = 0; i2 < 32; i2++) {
        if ((bitmap & 1) !== 0) {
          const node = root.array[j++];
          nodes[i2] = node;
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
  for (let i2 = 0; i2 < size; i2++) {
    if (isEqual(key, root.array[i2].k)) {
      return i2;
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
      let i2 = 0;
      let j = 0;
      let bitmap = 0;
      while (i2 < idx) {
        const nv = arr[i2];
        if (nv !== void 0) {
          out[j] = nv;
          bitmap |= 1 << i2;
          ++j;
        }
        ++i2;
      }
      ++i2;
      while (i2 < arr.length) {
        const nv = arr[i2];
        if (nv !== void 0) {
          out[j] = nv;
          bitmap |= 1 << i2;
          ++j;
        }
        ++i2;
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
  for (let i2 = 0; i2 < size; i2++) {
    const item = items[i2];
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
    for (let i2 = 0; i2 < keys2.length; i2++) {
      const k = keys2[i2];
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
    let i2 = 0;
    for (const _ of iterator) {
      i2++;
    }
    return i2;
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
function uppercase(string3) {
  return string3.toUpperCase();
}
function add(a2, b) {
  return a2 + b;
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
function compile_regex(pattern2, options) {
  try {
    let flags = "gu";
    if (options.case_insensitive)
      flags += "i";
    if (options.multi_line)
      flags += "m";
    return new Ok(new RegExp(pattern2, flags));
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
function decode_list(data) {
  if (Array.isArray(data)) {
    return new Ok(List.fromArray(data));
  }
  return data instanceof List ? new Ok(data) : decoder_error("List", data);
}
function decode_option(data, decoder) {
  if (data === null || data === void 0 || data instanceof None)
    return new Ok(new None());
  if (data instanceof Some)
    data = data[0];
  const result = decoder(data);
  if (result.isOk()) {
    return new Ok(new Some(result[0]));
  } else {
    return result;
  }
}
function decode_field(value3, name2) {
  const not_a_map_error = () => decoder_error("Dict", value3);
  if (value3 instanceof Dict || value3 instanceof WeakMap || value3 instanceof Map) {
    const entry = map_get(value3, name2);
    return new Ok(entry.isOk() ? new Some(entry[0]) : new None());
  } else if (value3 === null) {
    return not_a_map_error();
  } else if (Object.getPrototypeOf(value3) == Object.prototype) {
    return try_get_field(value3, name2, () => new Ok(new None()));
  } else {
    return try_get_field(value3, name2, not_a_map_error);
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
function get(from3, get3) {
  return map_get(from3, get3);
}
function insert(dict, key, value3) {
  return map_insert(key, value3, dict);
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
function do_keys(dict) {
  let list_of_pairs = map_to_list(dict);
  return do_keys_acc(list_of_pairs, toList([]));
}
function keys(dict) {
  return do_keys(dict);
}
function do_fold(loop$list, loop$initial, loop$fun) {
  while (true) {
    let list2 = loop$list;
    let initial = loop$initial;
    let fun = loop$fun;
    if (list2.hasLength(0)) {
      return initial;
    } else {
      let k = list2.head[0];
      let v = list2.head[1];
      let rest = list2.tail;
      loop$list = rest;
      loop$initial = fun(initial, k, v);
      loop$fun = fun;
    }
  }
}
function fold2(dict, initial, fun) {
  let _pipe = dict;
  let _pipe$1 = map_to_list(_pipe);
  return do_fold(_pipe$1, initial, fun);
}
function do_map_values(f, dict) {
  let f$1 = (dict2, k, v) => {
    return insert(dict2, k, f(k, v));
  };
  let _pipe = dict;
  return fold2(_pipe, new$(), f$1);
}
function map_values(dict, fun) {
  return do_map_values(fun, dict);
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
function regex_submatches(pattern2, string3) {
  let _pipe = pattern2;
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
  let pattern2 = "^(([a-z][a-z0-9\\+\\-\\.]*):)?(//([^/?#]*))?([^?#]*)(\\?([^#]*))?(#.*)?";
  let matches = (() => {
    let _pipe = pattern2;
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
function to_string4(uri) {
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
function decode3(json, decoder) {
  return do_decode(json, decoder);
}
function to_string6(json) {
  return json_to_string(json);
}
function string2(input2) {
  return identity2(input2);
}
function object2(entries) {
  return object(entries);
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
function from2(effect2) {
  return custom((dispatch, _, _1) => {
    return effect2(dispatch);
  });
}
function none() {
  return new Effect(toList([]));
}
function batch(effects2) {
  return new Effect(
    fold(
      effects2,
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
    let name2 = attribute2[0];
    let handler = attribute2[1];
    let name$1 = drop_left(name2, 2);
    return new Ok([name$1, handler]);
  }
}
function do_element_list_handlers(elements2, handlers2, key) {
  return index_fold(
    elements2,
    handlers2,
    (handlers3, element2, index3) => {
      let key$1 = key + "-" + to_string2(index3);
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
            let name2 = $[0][0];
            let handler = $[0][1];
            return insert(handlers3, key + "-" + name2, handler);
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
function attribute(name2, value3) {
  return new Attribute(name2, from(value3), false);
}
function property(name2, value3) {
  return new Attribute(name2, from(value3), true);
}
function on(name2, handler) {
  return new Event("on" + name2, handler);
}
function class$(name2) {
  return attribute("class", name2);
}
function id(name2) {
  return attribute("id", name2);
}
function type_(name2) {
  return attribute("type", name2);
}
function value(val) {
  return attribute("value", val);
}
function placeholder(text3) {
  return attribute("placeholder", text3);
}
function autocomplete(name2) {
  return attribute("autocomplete", name2);
}
function disabled(is_disabled) {
  return property("disabled", is_disabled);
}
function name(name2) {
  return attribute("name", name2);
}
function pattern(regex) {
  return attribute("pattern", regex);
}
function required(is_required) {
  return property("required", is_required);
}
function for$(id2) {
  return attribute("for", id2);
}
function max2(val) {
  return attribute("max", val);
}
function min2(val) {
  return attribute("min", val);
}
function href(uri) {
  return attribute("href", uri);
}
function target(target2) {
  return attribute("target", target2);
}
function rel(relationship) {
  return attribute("rel", relationship);
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
  constructor(dict) {
    super();
    this.dict = dict;
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
    const name2 = attr[0];
    const value3 = attr[1];
    if (attr.as_property) {
      if (el2[name2] !== value3)
        el2[name2] = value3;
      if (canMorph)
        prevAttributes.delete(name2);
    } else if (name2.startsWith("on")) {
      const eventName = name2.slice(2);
      const callback = dispatch(value3, eventName === "input");
      if (!handlersForEl.has(eventName)) {
        el2.addEventListener(eventName, lustreGenericEventHandler);
      }
      handlersForEl.set(eventName, callback);
      if (canMorph)
        prevHandlers.delete(eventName);
    } else if (name2.startsWith("data-lustre-on-")) {
      const eventName = name2.slice(15);
      const callback = dispatch(lustreServerEventHandler);
      if (!handlersForEl.has(eventName)) {
        el2.addEventListener(eventName, lustreGenericEventHandler);
      }
      handlersForEl.set(eventName, callback);
      el2.setAttribute(name2, value3);
    } else if (name2 === "class") {
      className = className === null ? value3 : className + " " + value3;
    } else if (name2 === "style") {
      style = style === null ? value3 : style + value3;
    } else if (name2 === "dangerous-unescaped-html") {
      innerHTML = value3;
    } else {
      if (el2.getAttribute(name2) !== value3)
        el2.setAttribute(name2, value3);
      if (name2 === "value" || name2 === "selected")
        el2[name2] = value3;
      if (canMorph)
        prevAttributes.delete(name2);
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
  const target2 = event2.currentTarget;
  if (!registeredHandlers.has(target2)) {
    target2.removeEventListener(event2.type, lustreGenericEventHandler);
    return;
  }
  const handlersForEventTarget = registeredHandlers.get(target2);
  if (!handlersForEventTarget.has(event2.type)) {
    target2.removeEventListener(event2.type, lustreGenericEventHandler);
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
        for (let i2 = 0, o = data2, e = event2; i2 < path.length; i2++) {
          if (i2 === path.length - 1) {
            o[path[i2]] = e[path[i2]];
          } else {
            o[path[i2]] ??= {};
            e = e[path[i2]];
            o = o[path[i2]];
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
    const placeholder2 = document.createTextNode("");
    el2.insertBefore(placeholder2, prevChild);
    stack.unshift({ prev: placeholder2, next: child, parent: el2 });
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
  constructor(root, [init4, effects2], update2, view2) {
    this.root = root;
    this.#model = init4;
    this.#update = update2;
    this.#view = view2;
    this.#tickScheduled = window.requestAnimationFrame(
      () => this.#tick(effects2.all.toArray(), true)
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
  #tick(effects2 = [], isFirstRender = false) {
    this.#tickScheduled = void 0;
    if (!this.#flush(effects2, isFirstRender))
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
  #flush(effects2 = [], didUpdate = false) {
    while (this.#queue.length > 0) {
      const msg = this.#queue.shift();
      const [next, effect2] = this.#update(this.#model, msg);
      didUpdate ||= this.#model !== next;
      effects2 = effects2.concat(effect2.all.toArray());
      this.#model = next;
    }
    while (effects2.length > 0) {
      const effect2 = effects2.shift();
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
      effect2({ dispatch, emit: emit2, select });
    }
    if (this.#queue.length > 0) {
      return this.#flush(effects2, didUpdate);
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
  constructor([model, effects2], update2, view2, on_attribute_change) {
    this.#model = model;
    this.#update = update2;
    this.#view = view2;
    this.#html = view2(model);
    this.#onAttributeChange = on_attribute_change;
    this.#renderers = /* @__PURE__ */ new Map();
    this.#handlers = handlers(this.#html);
    this.#tick(effects2.all.toArray());
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
  #tick(effects2 = []) {
    if (!this.#flush(false, effects2))
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
  #flush(didUpdate = false, effects2 = []) {
    while (this.#queue.length > 0) {
      const msg = this.#queue.shift();
      const [next, effect2] = this.#update(this.#model, msg);
      didUpdate ||= this.#model !== next;
      effects2 = effects2.concat(effect2.all.toArray());
      this.#model = next;
    }
    while (effects2.length > 0) {
      const effect2 = effects2.shift();
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
      effect2({ dispatch, emit: emit2, select });
    }
    if (this.#queue.length > 0) {
      return this.#flush(didUpdate, effects2);
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
function footer(attrs, children2) {
  return element("footer", attrs, children2);
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
function i(attrs, children2) {
  return element("i", attrs, children2);
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
  let url = to_string4(to_uri(request));
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
        let _pipe$3 = set_body(_pipe$2, to_string6(body2));
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
          let $ = decode3(body2, decoder);
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
var initial_location = window?.location?.href;

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

// build/dev/javascript/client/client/router.mjs
var Home = class extends CustomType {
};
var Login = class extends CustomType {
};
var Gifts = class extends CustomType {
};
var Event3 = class extends CustomType {
};
var Gallery = class extends CustomType {
};
var Comments = class extends CustomType {
};
var Admin = class extends CustomType {
};
var ConfirmPresence = class extends CustomType {
};

// build/dev/javascript/client/client/model.mjs
var Model2 = class extends CustomType {
  constructor(route, auth_user, gift_status, gallery_images, login_form, confirm_form, event_countdown, admin_settings, comments) {
    super();
    this.route = route;
    this.auth_user = auth_user;
    this.gift_status = gift_status;
    this.gallery_images = gallery_images;
    this.login_form = login_form;
    this.confirm_form = confirm_form;
    this.event_countdown = event_countdown;
    this.admin_settings = admin_settings;
    this.comments = comments;
  }
};
var LoginForm = class extends CustomType {
  constructor(username, email, password, confirm_password, sign_up, error) {
    super();
    this.username = username;
    this.email = email;
    this.password = password;
    this.confirm_password = confirm_password;
    this.sign_up = sign_up;
    this.error = error;
  }
};
var ConfirmForm = class extends CustomType {
  constructor(name2, invite_name, email, phone, people_count, person_name, people_names, comments, error) {
    super();
    this.name = name2;
    this.invite_name = invite_name;
    this.email = email;
    this.phone = phone;
    this.people_count = people_count;
    this.person_name = person_name;
    this.people_names = people_names;
    this.comments = comments;
    this.error = error;
  }
};
var GiftStatus = class extends CustomType {
  constructor(sugestion, unique, error) {
    super();
    this.sugestion = sugestion;
    this.unique = unique;
    this.error = error;
  }
};
var AdminSettings = class extends CustomType {
  constructor(total, confirmations, show_details, show_all) {
    super();
    this.total = total;
    this.confirmations = confirmations;
    this.show_details = show_details;
    this.show_all = show_all;
  }
};
function init2() {
  return new Model2(
    new Home(),
    new None(),
    new GiftStatus(toList([]), toList([]), new None()),
    toList([]),
    new LoginForm("", "", "", "", false, new None()),
    new ConfirmForm("", "", "", "", 1, "", new$(), new None(), new None()),
    0,
    new AdminSettings(0, toList([]), new$(), false),
    toList([])
  );
}
function update_user(model, auth_user) {
  return model.withFields({ auth_user: new Some(auth_user) });
}
function update_gifts(model, gift_status) {
  return model.withFields({ gift_status });
}
function update_images(model, gallery_images) {
  return model.withFields({ gallery_images });
}
function update_comments(model, comments) {
  return model.withFields({ comments });
}
function update_admin_settings(model, admin_settings) {
  return model.withFields({ admin_settings });
}
function update_event_countdown(model, event_countdown) {
  return model.withFields({ event_countdown });
}
function update_login_username(model, username) {
  return model.withFields({
    login_form: model.login_form.withFields({ username })
  });
}
function update_login_email(model, email) {
  return model.withFields({
    login_form: model.login_form.withFields({ email })
  });
}
function update_login_password(model, password) {
  return model.withFields({
    login_form: model.login_form.withFields({ password })
  });
}
function update_login_confirm_password(model, confirm_password) {
  return model.withFields({
    login_form: model.login_form.withFields({
      confirm_password
    })
  });
}
function update_login_error(model, error) {
  return model.withFields({
    login_form: model.login_form.withFields({ error })
  });
}

// build/dev/javascript/client/client/msg.mjs
var AuthUserRecieved = class extends CustomType {
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
var ImagesRecieved = class extends CustomType {
  constructor(x0) {
    super();
    this[0] = x0;
  }
};
var CommentsRecieved = class extends CustomType {
  constructor(x0) {
    super();
    this[0] = x0;
  }
};
var ConfirmationsRecieved = class extends CustomType {
  constructor(x0) {
    super();
    this[0] = x0;
  }
};
var CountdownUpdated = class extends CustomType {
  constructor(value3) {
    super();
    this.value = value3;
  }
};
var LoginUpdateUsername = class extends CustomType {
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
var LoginUpdateConfirmPassword = class extends CustomType {
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
var UserRequestedLoginSignUp = class extends CustomType {
};
var LoginResponded = class extends CustomType {
  constructor(resp_result) {
    super();
    this.resp_result = resp_result;
  }
};
var UserClickedSignUp = class extends CustomType {
};
var SignUpResponded = class extends CustomType {
  constructor(resp_result) {
    super();
    this.resp_result = resp_result;
  }
};
var UserOpenedGiftsView = class extends CustomType {
};
var UserOpenedGalleryView = class extends CustomType {
};
var AdminOpenedAdminView = class extends CustomType {
};
var AdminClickedShowConfirmationDetails = class extends CustomType {
  constructor(id2) {
    super();
    this.id = id2;
  }
};
var AdminClickedShowAll = class extends CustomType {
};
var UserRequestedSelectGift = class extends CustomType {
  constructor(gift, to2) {
    super();
    this.gift = gift;
    this.to = to2;
  }
};
var ConfirmUpdateName = class extends CustomType {
  constructor(value3) {
    super();
    this.value = value3;
  }
};
var ConfirmUpdateInviteName = class extends CustomType {
  constructor(value3) {
    super();
    this.value = value3;
  }
};
var ConfirmUpdateEmail = class extends CustomType {
  constructor(value3) {
    super();
    this.value = value3;
  }
};
var ConfirmUpdatePhone = class extends CustomType {
  constructor(value3) {
    super();
    this.value = value3;
  }
};
var ConfirmUpdatePeopleCount = class extends CustomType {
  constructor(value3) {
    super();
    this.value = value3;
  }
};
var ConfirmUpdatePersonName = class extends CustomType {
  constructor(key, value3) {
    super();
    this.key = key;
    this.value = value3;
  }
};
var ConfirmUpdateComments = class extends CustomType {
  constructor(value3) {
    super();
    this.value = value3;
  }
};
var UserRequestedConfirmPresence = class extends CustomType {
};
var MessageErrorResponse = class extends CustomType {
  constructor(message, error) {
    super();
    this.message = message;
    this.error = error;
  }
};
function message_error_decoder() {
  return decode2(
    (var0, var1) => {
      return new MessageErrorResponse(var0, var1);
    },
    optional_field("message", string),
    optional_field("error", string)
  );
}

// build/dev/javascript/client/client/update.mjs
function effect(model, effect2) {
  return [model, effect2];
}
function none3(model) {
  return [model, none()];
}
function effects(model, effects2) {
  return [model, batch(effects2)];
}

// build/dev/javascript/lustre/lustre/event.mjs
function on2(name2, handler) {
  return on(name2, handler);
}
function on_click(msg) {
  return on2("click", (_) => {
    return new Ok(msg);
  });
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

// build/dev/javascript/client/client/views/home_view.mjs
function home_view(model) {
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
                toList([class$("text-emerald-300"), id("countdown")]),
                toList([
                  text(
                    (() => {
                      let _pipe = model.event_countdown;
                      return to_string2(_pipe);
                    })()
                  )
                ])
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
                            "bg-emerald-600 hover:bg-emerald-700 min-w-40 text-white font-bold py-2 px-6 rounded-full shadow-lg transition duration-300 transform hover:scale-105"
                          )
                        ]),
                        toList([
                          a(
                            toList([href("/confirm")]),
                            toList([text("Confirmar Presen\xE7a")])
                          )
                        ])
                      ),
                      button(
                        toList([
                          class$(
                            "bg-pink-600 hover:bg-pink-700 text-white font-bold py-2 px-6 rounded-full shadow-lg transition duration-300 transform hover:scale-105"
                          )
                        ]),
                        toList([
                          a(
                            toList([href("/gifts")]),
                            toList([text("Lista de Presentes")])
                          )
                        ])
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
                toList([class$("text-3xl font-semibold text-emerald-600 mb-4")]),
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

// build/dev/javascript/client/client/views/admin_view.mjs
function names_text(name2) {
  return li(toList([]), toList([text(name2)]));
}
function details(confirmation) {
  return div(
    toList([
      id(
        (() => {
          let _pipe = confirmation.id;
          return to_string2(_pipe);
        })()
      ),
      class$("detalhes mt-4")
    ]),
    toList([
      p(
        toList([]),
        toList([
          strong(toList([]), toList([text("Nome no convite: ")])),
          text(confirmation.invite_name)
        ])
      ),
      p(
        toList([]),
        toList([
          strong(toList([]), toList([text("Telefone: ")])),
          text(confirmation.phone)
        ])
      ),
      (() => {
        let $ = confirmation.comments;
        if ($ instanceof Some) {
          let comment = $[0];
          return p(
            toList([]),
            toList([
              strong(toList([]), toList([text("Coment\xE1rio: ")])),
              text(comment)
            ])
          );
        } else {
          return none2();
        }
      })(),
      (() => {
        let $ = (() => {
          let _pipe = confirmation.people_names;
          return length(_pipe);
        })();
        if ($ === 0) {
          return none2();
        } else {
          let n = $;
          return p(
            toList([]),
            toList([
              strong(toList([]), toList([text("Total de pessoas: ")])),
              text(
                (() => {
                  let _pipe = n;
                  return to_string2(_pipe);
                })()
              )
            ])
          );
        }
      })(),
      (() => {
        let $ = (() => {
          let _pipe = confirmation.people_names;
          return length(_pipe);
        })();
        if ($ === 0) {
          return none2();
        } else {
          return div(
            toList([]),
            toList([
              strong(toList([]), toList([text("Acompanhantes")])),
              ul(
                toList([class$("list-disc ml-6 mt-2")]),
                (() => {
                  let _pipe = confirmation.people_names;
                  return map2(_pipe, names_text);
                })()
              )
            ])
          );
        }
      })()
    ])
  );
}
function confirmation_box(model, confirmation) {
  return div(
    toList([
      class$(
        "relative bg-white p-6 rounded-lg shadow-lg transition duration-300"
      )
    ]),
    toList([
      div(
        toList([class$("flex justify-between items-center")]),
        toList([
          h2(
            toList([class$("text-2xl font-semibold text-pink-700")]),
            toList([text(confirmation.name)])
          ),
          button(
            toList([
              attribute(
                "data-id",
                (() => {
                  let _pipe = confirmation.id;
                  return to_string2(_pipe);
                })()
              ),
              class$(
                "bg-pink-600 hover:bg-pink-700 text-white font-bold py-2 px-4 rounded-full transition duration-300"
              ),
              on_click(
                new AdminClickedShowConfirmationDetails(confirmation.id)
              )
            ]),
            toList([text("Mostrar detalhes")])
          )
        ])
      ),
      (() => {
        let $ = (() => {
          let _pipe = model.admin_settings.show_details;
          return get(_pipe, confirmation.id);
        })();
        if ($.isOk()) {
          let show = $[0];
          let $1 = show || model.admin_settings.show_all;
          if ($1) {
            return details(confirmation);
          } else {
            return none2();
          }
        } else {
          return none2();
        }
      })()
    ])
  );
}
function auth_admin_view(model) {
  return main(
    toList([class$("w-full max-w-6xl p-8 mt-12 flex flex-col items-center")]),
    toList([
      h1(
        toList([
          attribute("style", "font-family: 'Pacifico', cursive;"),
          class$("text-5xl text-white font-bold mb-12")
        ]),
        toList([text("Lista de confirmados")])
      ),
      p(
        toList([class$("text-3xl font-bold mb-6")]),
        toList([
          span(
            toList([class$("text-white")]),
            toList([text("Total de confirmados: ")])
          ),
          span(
            toList([class$("text-emerald-300")]),
            toList([
              text(
                (() => {
                  let _pipe = model.admin_settings.total;
                  return to_string2(_pipe);
                })()
              )
            ])
          )
        ])
      ),
      button(
        toList([
          class$(
            "bg-emerald-600 hover:bg-emerald-700 text-white font-bold py-2 px-4 rounded-full transition duration-300 mb-6"
          ),
          id("show_all"),
          on_click(new AdminClickedShowAll())
        ]),
        toList([text("Mostrar todos os dados")])
      ),
      div(
        toList([
          class$("grid grid-cols-1 gap-6 w-full"),
          id("lista_confirmados")
        ]),
        (() => {
          let confirmations = model.admin_settings.confirmations;
          let _pipe = confirmations;
          return map2(
            _pipe,
            (confirmation) => {
              return confirmation_box(model, confirmation);
            }
          );
        })()
      )
    ])
  );
}
function admin_view(model) {
  let $ = model.auth_user;
  if ($ instanceof None) {
    return home_view(model);
  } else {
    let user = $[0];
    let $1 = user.is_admin;
    if ($1) {
      return auth_admin_view(model);
    } else {
      return home_view(model);
    }
  }
}

// build/dev/javascript/client/client/views/comments_view.mjs
function comment_list_item(comment) {
  let $ = comment.name;
  let $1 = comment.comment;
  if ($1 instanceof Some && $1[0] !== "") {
    let name2 = $;
    let comment$1 = $1[0];
    return li(
      toList([]),
      toList([
        div(
          toList([class$("space-y-6")]),
          toList([
            div(
              toList([class$("bg-gray-100 p-6 rounded-lg shadow-inner")]),
              toList([
                p(
                  toList([class$("text-lg font-semibold text-pink-600")]),
                  toList([text(name2)])
                ),
                p(toList([class$("text-gray-600")]), toList([text(comment$1)]))
              ])
            )
          ])
        )
      ])
    );
  } else {
    return none2();
  }
}
function comments_view(model) {
  return main(
    toList([class$("w-full max-w-6xl p-8 mt-12 flex flex-col items-center")]),
    toList([
      div(
        toList([class$("text-center mt-12")]),
        toList([
          h1(
            toList([
              attribute("style", "font-family: 'Pacifico', cursive;"),
              class$("text-5xl text-white font-bold")
            ]),
            toList([text("Coment\xE1rios")])
          ),
          p(
            toList([class$("text-xl text-white mt-4")]),
            toList([text("Para fazer um coment\xE1rio, confirme sua presen\xE7a")])
          )
        ])
      ),
      div(
        toList([
          class$(
            "bg-white text-gray-800 rounded-lg shadow-lg p-12 max-w-4xl w-full mx-4 mt-12 border border-gray-200"
          )
        ]),
        toList([
          h2(
            toList([class$("text-3xl font-semibold text-pink-600 mb-6")]),
            toList([text("Coment\xE1rios")])
          ),
          ul(
            toList([]),
            (() => {
              let _pipe = model.comments;
              return map2(_pipe, comment_list_item);
            })()
          )
        ])
      )
    ])
  );
}

// build/dev/javascript/client/client/views/components/footer.mjs
function footer_view() {
  return footer(
    toList([class$("text-white py-1 w-full mt-2")]),
    toList([
      div(
        toList([class$("max-w-4xl mx-auto text-center")]),
        toList([
          div(
            toList([class$("mt-2")]),
            toList([
              a(
                toList([
                  class$("text-white hover:text-emerald-300 text-sm mx-4"),
                  href("https://github.com/raphaelantoniocampos"),
                  target("_blank")
                ]),
                toList([
                  i(toList([class$("fab fa-github ")]), toList([])),
                  text("Github")
                ])
              ),
              a(
                toList([
                  class$("text-white hover:text-emerald-300 text-sm mx-4"),
                  href("https://www.linkedin.com/in/raphael-antonio-campos/"),
                  target("_blank")
                ]),
                toList([
                  i(toList([class$("fab fa-linkedin ")]), toList([])),
                  text("Linkedin")
                ])
              )
            ])
          ),
          p(
            toList([class$("text-sm mt-4")]),
            toList([
              text("\xA9 2024 Raphael Campos. Todos os direitos reservados.")
            ])
          )
        ])
      )
    ])
  );
}

// build/dev/javascript/client/client/views/components/navigation_bar.mjs
function navigation_bar_view(model) {
  return nav(
    toList([
      class$(
        "fixed z-50 w-full bg-white shadow-md py-4 px-8 flex justify-between items-center"
      )
    ]),
    toList([
      div(
        toList([class$("flex min-w-10 text-pink-600 font-semibold")]),
        toList([
          a(
            toList([
              class$("text-2xl hover:text-emerald-800 transition duration-300"),
              href("../")
            ]),
            toList([text("\u21B6")])
          )
        ])
      ),
      (() => {
        let $ = model.auth_user;
        if ($ instanceof Some) {
          return div(toList([]), toList([]));
        } else {
          return none2();
        }
      })(),
      ul(
        toList([class$("flex space-x-8  font-semibold")]),
        toList([
          li(
            toList([]),
            toList([
              a(
                toList([
                  class$(
                    "hover:text-emerald-800 " + (() => {
                      let $ = model.route;
                      if ($ instanceof Home) {
                        return "text-emerald-600";
                      } else {
                        return "text-pink-600";
                      }
                    })() + " transition duration-300"
                  ),
                  href("/")
                ]),
                toList([text("P\xE1gina Inicial")])
              )
            ])
          ),
          li(
            toList([]),
            toList([
              a(
                toList([
                  class$(
                    "hover:text-emerald-800 " + (() => {
                      let $ = model.route;
                      if ($ instanceof Event3) {
                        return "text-emerald-600";
                      } else {
                        return "text-pink-600";
                      }
                    })() + " transition duration-300"
                  ),
                  href("/event")
                ]),
                toList([text("Evento")])
              )
            ])
          ),
          li(
            toList([]),
            toList([
              a(
                toList([
                  class$(
                    "hover:text-emerald-800 " + (() => {
                      let $ = model.route;
                      if ($ instanceof Gifts) {
                        return "text-emerald-600";
                      } else {
                        return "text-pink-600";
                      }
                    })() + " transition duration-300"
                  ),
                  href("/gifts"),
                  on_click(new UserOpenedGiftsView())
                ]),
                toList([text("Presentes")])
              )
            ])
          ),
          li(
            toList([]),
            toList([
              a(
                toList([
                  class$(
                    "hover:text-emerald-800 " + (() => {
                      let $ = model.route;
                      if ($ instanceof Gallery) {
                        return "text-emerald-600";
                      } else {
                        return "text-pink-600";
                      }
                    })() + " transition duration-300"
                  ),
                  href("/gallery"),
                  on_click(new UserOpenedGalleryView())
                ]),
                toList([text("Galeria")])
              )
            ])
          ),
          li(
            toList([]),
            toList([
              a(
                toList([
                  class$(
                    "hover:text-emerald-800 " + (() => {
                      let $ = model.route;
                      if ($ instanceof Comments) {
                        return "text-emerald-600";
                      } else {
                        return "text-pink-600";
                      }
                    })() + " transition duration-300"
                  ),
                  href("/comments")
                ]),
                toList([text("Coment\xE1rios")])
              )
            ])
          )
        ])
      ),
      (() => {
        let $ = model.auth_user;
        if ($ instanceof None) {
          return div(
            toList([]),
            toList([
              span(
                toList([class$("min-w-5 text-pink-600 font-semibold")]),
                toList([
                  a(
                    toList([
                      class$(
                        "hover:text-emerald-800 " + (() => {
                          let $1 = model.route;
                          if ($1 instanceof Login) {
                            return "text-emerald-600";
                          } else {
                            return "text-pink-600";
                          }
                        })() + " transition duration-300"
                      ),
                      href("/login")
                    ]),
                    toList([text("Login")])
                  )
                ])
              )
            ])
          );
        } else {
          let user = $[0];
          return div(
            toList([class$("flex items-center space-x-4")]),
            toList([
              (() => {
                let $1 = user.is_confirmed;
                if ($1) {
                  return span(
                    toList([class$("text-emerald-600 font-semibold")]),
                    toList([text("Presen\xE7a Confirmada")])
                  );
                } else {
                  return button(
                    toList([
                      class$(
                        "bg-emerald-600 hover:bg-emerald-700 min-w-10 text-white font-bold py-2 px-6 rounded-full shadow-lg transition duration-300 transform hover:scale-105"
                      )
                    ]),
                    toList([
                      a(
                        toList([href("/confirm")]),
                        toList([text("Confirme sua presen\xE7a")])
                      )
                    ])
                  );
                }
              })(),
              span(
                toList([class$("text-pink-600 font-semibold")]),
                toList([
                  (() => {
                    let $1 = user.is_admin;
                    if ($1) {
                      return button(
                        toList([]),
                        toList([
                          a(
                            toList([
                              href("/admin"),
                              on_click(new AdminOpenedAdminView())
                            ]),
                            toList([
                              text(
                                "Ol\xE1, " + (() => {
                                  let _pipe = user.username;
                                  return capitalise(_pipe);
                                })()
                              )
                            ])
                          )
                        ])
                      );
                    } else {
                      return text(
                        "Ol\xE1, " + (() => {
                          let _pipe = user.username;
                          return capitalise(_pipe);
                        })()
                      );
                    }
                  })()
                ])
              )
            ])
          );
        }
      })()
    ])
  );
}

// build/dev/javascript/client/env.mjs
function get_api_url() {
  return "http://localhost:8083";
}

// build/dev/javascript/client/client/views/login_view.mjs
function login(model) {
  return post(
    get_api_url() + "/auth/login",
    object2(
      toList([
        ["email", string2(model.login_form.email)],
        ["password", string2(model.login_form.password)]
      ])
    ),
    expect_json(
      message_error_decoder(),
      (var0) => {
        return new LoginResponded(var0);
      }
    )
  );
}
function signup(model) {
  return post(
    get_api_url() + "/users",
    object2(
      toList([
        [
          "username",
          string2(
            (() => {
              let _pipe = model.login_form.username;
              return lowercase2(_pipe);
            })()
          )
        ],
        ["email", string2(model.login_form.email)],
        ["password", string2(model.login_form.password)],
        ["confirm_password", string2(model.login_form.confirm_password)]
      ])
    ),
    expect_json(
      message_error_decoder(),
      (var0) => {
        return new SignUpResponded(var0);
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
            toList([text("Entrar")])
          ),
          form(
            toList([
              class$("space-y-6"),
              on_submit(new UserRequestedLoginSignUp())
            ]),
            toList([
              (() => {
                let $ = model.login_form.sign_up;
                if ($) {
                  return div(
                    toList([]),
                    toList([
                      label(
                        toList([
                          class$("block text-sm font-medium text-gray-700"),
                          for$("username")
                        ]),
                        toList([text("Insira um nome de usu\xE1rio")])
                      ),
                      input(
                        toList([
                          class$(
                            "mt-1 block w-full px-4 py-2 border border-gray-300 rounded-lg shadow-sm focus:ring-pink-500 focus:border-pink-500"
                          ),
                          on_input(
                            (var0) => {
                              return new LoginUpdateUsername(var0);
                            }
                          ),
                          required(true),
                          id("username"),
                          type_("name"),
                          value(model.login_form.username)
                        ])
                      )
                    ])
                  );
                } else {
                  return none2();
                }
              })(),
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
                      value(model.login_form.email)
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
                      value(model.login_form.password)
                    ])
                  )
                ])
              ),
              (() => {
                let $ = model.login_form.sign_up;
                if ($) {
                  return div(
                    toList([]),
                    toList([
                      label(
                        toList([
                          class$("block text-sm font-medium text-gray-700"),
                          for$("password")
                        ]),
                        toList([text("Confirme sua senha")])
                      ),
                      input(
                        toList([
                          class$(
                            "mt-1 block w-full px-4 py-2 border border-gray-300 rounded-lg shadow-sm focus:ring-pink-500 focus:border-pink-500"
                          ),
                          on_input(
                            (var0) => {
                              return new LoginUpdateConfirmPassword(var0);
                            }
                          ),
                          id("confirm_password"),
                          type_("password"),
                          required(true),
                          value(model.login_form.confirm_password)
                        ])
                      )
                    ])
                  );
                } else {
                  return none2();
                }
              })(),
              (() => {
                let $ = model.login_form.sign_up;
                if (!$) {
                  return div(
                    toList([class$("flex items-center justify-center")]),
                    toList([
                      button(
                        toList([
                          class$(
                            "bg-emerald-600 hover:bg-emerald-700 min-w-60 text-white font-bold py-2 px-6 rounded-full shadow-lg transition duration-300 transform hover:scale-105"
                          ),
                          type_("submit")
                        ]),
                        toList([text("Entrar")])
                      )
                    ])
                  );
                } else {
                  return div(
                    toList([class$("flex items-center justify-center")]),
                    toList([
                      button(
                        toList([
                          class$(
                            "bg-pink-600 hover:bg-pink-700 min-w-60 text-white font-bold py-2 px-6 rounded-full shadow-lg transition duration-300 transform hover:scale-105"
                          ),
                          type_("submit")
                        ]),
                        toList([text("Cadastre-se")])
                      )
                    ])
                  );
                }
              })()
            ])
          ),
          (() => {
            let $ = model.login_form.sign_up;
            if (!$) {
              return div(
                toList([class$("flex items-center justify-center")]),
                toList([
                  text("N\xE3o tem conta?"),
                  button(
                    toList([
                      class$(
                        "p-1 text-pink-600 hover:text-pink-800 transition duration-300"
                      ),
                      on_click(new UserClickedSignUp())
                    ]),
                    toList([text("Cadastre-se")])
                  )
                ])
              );
            } else {
              return div(
                toList([class$("flex items-center justify-center")]),
                toList([
                  text("Fazer"),
                  button(
                    toList([
                      class$(
                        "p-1 text-emerald-600 hover:text-emerald-800 transition duration-300"
                      ),
                      on_click(new UserClickedSignUp())
                    ]),
                    toList([text("Login")])
                  )
                ])
              );
            }
          })(),
          (() => {
            let $ = model.login_form.error;
            if ($ instanceof Some) {
              let err = $[0];
              return p(
                toList([class$("text-red-500 text-center")]),
                toList([text("Erro: " + err)])
              );
            } else {
              return none2();
            }
          })()
        ])
      )
    ])
  );
}

// build/dev/javascript/client/client/views/confirm_presence_view.mjs
function name_box_element(model, n) {
  let string_n = (() => {
    let _pipe = n + 1;
    return to_string2(_pipe);
  })();
  return element(
    "name_box",
    toList([]),
    toList([
      (() => {
        if (n === 0) {
          return li(
            toList([class$("py-3")]),
            toList([
              label(
                toList([
                  class$("block text-sm font-medium text-gray-700"),
                  for$("people_names_" + string_n)
                ]),
                toList([text("Seu nome")])
              ),
              input(
                toList([
                  class$(
                    "mt-1 block w-full px-4 py-2 border border-gray-300 rounded-lg shadow-sm focus:ring-pink-500 focus:border-pink-500"
                  ),
                  required(true),
                  name("people_names_" + string_n),
                  id("people_names_" + string_n),
                  type_("name"),
                  value(model.confirm_form.name),
                  on_input(
                    (value3) => {
                      return new ConfirmUpdatePersonName(n, value3);
                    }
                  )
                ])
              )
            ])
          );
        } else {
          return li(
            toList([class$("py-3")]),
            toList([
              label(
                toList([
                  class$("block text-sm font-medium text-gray-700"),
                  for$("people_names_" + string_n)
                ]),
                toList([text(string_n + "\xAA pessoa")])
              ),
              input(
                toList([
                  class$(
                    "mt-1 block w-full px-4 py-2 border border-gray-300 rounded-lg shadow-sm focus:ring-pink-500 focus:border-pink-500"
                  ),
                  required(true),
                  name("people_names_" + string_n),
                  id("people_names_" + string_n),
                  type_("name"),
                  on_input(
                    (value3) => {
                      return new ConfirmUpdatePersonName(n, value3);
                    }
                  )
                ])
              )
            ])
          );
        }
      })()
    ])
  );
}
function confirmed_user_view() {
  return div(
    toList([class$("text-center p-12 mx-4")]),
    toList([
      h1(
        toList([
          attribute("style", "font-family: 'Pacifico', cursive;"),
          class$("text-4xl font-bold text-pink-600 mb-6")
        ]),
        toList([text("Presen\xE7a Confirmada!")])
      ),
      p(
        toList([class$("text-lg text-gray-700 mb-6")]),
        toList([
          text(
            "Voc\xEA j\xE1 confirmou sua presen\xE7a na festa de 15 anos da Laura. Estamos muito felizes por ter voc\xEA com a gente neste momento t\xE3o especial!"
          )
        ])
      ),
      p(
        toList([class$("text-lg text-gray-700 mb-6")]),
        toList([text("Que tal escolher um presente da nossa lista?")])
      ),
      a(
        toList([
          class$(
            "bg-emerald-600 hover:bg-emerald-700 text-white font-bold py-3 px-8 rounded-full shadow-lg transition duration-300 transform hover:scale-105"
          ),
          href("/gifts")
        ]),
        toList([text("Ver Lista de Presentes")])
      )
    ])
  );
}
function confirm_presence_view(model) {
  let $ = model.auth_user;
  if ($ instanceof None) {
    return login_view(model);
  } else {
    let user = $[0];
    return main(
      toList([
        class$("w-full max-w-2xl p-8 mt-20 bg-white rounded-lg shadow-lg")
      ]),
      toList([
        (() => {
          let $1 = user.is_confirmed;
          if ($1) {
            return confirmed_user_view();
          } else {
            return div(
              toList([class$("p-2 mt-6 mx-4")]),
              toList([
                h1(
                  toList([
                    attribute("style", "font-family: 'Pacifico', cursive;"),
                    class$("text-4xl text-pink-700 font-bold mb-6 text-center")
                  ]),
                  toList([text("Confirma\xE7\xE3o de Presen\xE7a")])
                ),
                form(
                  toList([
                    class$("space-y-6"),
                    on_submit(new UserRequestedConfirmPresence())
                  ]),
                  toList([
                    div(
                      toList([]),
                      toList([
                        label(
                          toList([
                            class$("block text-sm font-medium text-gray-700"),
                            for$("first_name")
                          ]),
                          toList([text("Nome completo")])
                        ),
                        input(
                          toList([
                            class$(
                              "mt-1 block w-full px-4 py-2 border border-gray-300 rounded-lg shadow-sm focus:ring-pink-500 focus:border-pink-500"
                            ),
                            required(true),
                            name("first_name"),
                            id("first_name"),
                            type_("name"),
                            value(model.confirm_form.name),
                            on_input(
                              (var0) => {
                                return new ConfirmUpdateName(var0);
                              }
                            )
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
                            for$("invite_name")
                          ]),
                          toList([text("Nome no Convite")])
                        ),
                        input(
                          toList([
                            class$(
                              "mt-1 block w-full px-4 py-2 border border-gray-300 rounded-lg shadow-sm focus:ring-pink-500 focus:border-pink-500"
                            ),
                            required(true),
                            name("invite_name"),
                            id("invite_name"),
                            type_("text"),
                            on_input(
                              (var0) => {
                                return new ConfirmUpdateInviteName(var0);
                              }
                            ),
                            value(model.confirm_form.invite_name)
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
                            for$("phone")
                          ]),
                          toList([text("Telefone")])
                        ),
                        input(
                          toList([
                            class$(
                              "mt-1 block w-full px-4 py-2 border border-gray-300 rounded-lg shadow-sm focus:ring-pink-500 focus:border-pink-500"
                            ),
                            required(true),
                            placeholder("Digite apenas n\xFAmeros"),
                            pattern("\\d{4,15}"),
                            name("phone"),
                            id("phone"),
                            type_("tel"),
                            on_input(
                              (var0) => {
                                return new ConfirmUpdatePhone(var0);
                              }
                            ),
                            value(model.confirm_form.phone)
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
                            for$("people_count")
                          ]),
                          toList([
                            text("Quantidade de pessoas (incluindo voc\xEA)")
                          ])
                        ),
                        input(
                          toList([
                            class$(
                              "mt-1 block w-full px-4 py-2 border border-gray-300 rounded-lg shadow-sm focus:ring-pink-500 focus:border-pink-500"
                            ),
                            required(true),
                            min2("1"),
                            max2("99"),
                            name("people_count"),
                            id("people_count"),
                            type_("number"),
                            on_input(
                              (var0) => {
                                return new ConfirmUpdatePeopleCount(var0);
                              }
                            )
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
                            for$("people_names")
                          ]),
                          toList([text("Nome completo das pessoas (se houver)")])
                        ),
                        ul(
                          toList([
                            class$("block text-sm font-medium text-gray-700")
                          ]),
                          (() => {
                            let _pipe = range(
                              0,
                              model.confirm_form.people_count - 1
                            );
                            return map2(
                              _pipe,
                              (n) => {
                                return name_box_element(model, n);
                              }
                            );
                          })()
                        )
                      ])
                    ),
                    div(
                      toList([]),
                      toList([
                        label(
                          toList([
                            class$("block text-sm font-medium text-gray-700"),
                            for$("comments")
                          ]),
                          toList([text("Coment\xE1rios (se houver)")])
                        ),
                        input(
                          toList([
                            class$(
                              "mt-1 block w-full px-4 py-2 border border-gray-300 rounded-lg shadow-sm focus:ring-pink-500 focus:border-pink-500"
                            ),
                            name("comments"),
                            id("comments"),
                            type_("text"),
                            on_input(
                              (var0) => {
                                return new ConfirmUpdateComments(var0);
                              }
                            )
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
                              "bg-emerald-600 hover:bg-emerald-700 min-w-60 text-white font-bold py-2 px-6 rounded-full shadow-lg transition duration-300 transform hover:scale-105"
                            ),
                            type_("submit")
                          ]),
                          toList([text("Enviar Confirma\xE7\xE3o")])
                        )
                      ])
                    ),
                    (() => {
                      let $2 = model.confirm_form.error;
                      if ($2 instanceof Some) {
                        let err = $2[0];
                        return p(
                          toList([class$("text-red-500 text-center")]),
                          toList([text("Erro: " + err)])
                        );
                      } else {
                        return none2();
                      }
                    })()
                  ])
                )
              ])
            );
          }
        })()
      ])
    );
  }
}

// build/dev/javascript/client/client/views/event_view.mjs
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
                toList([class$("text-3xl font-bold text-pink-600 mb-4")]),
                toList([text("Anivers\xE1rio de 15 Anos da Laura")])
              ),
              p(
                toList([class$("text-lg text-gray-700 mb-4")]),
                toList([text("Pomp\xE9u, MG - 14 de Dezembro de 2024")])
              ),
              p(
                toList([class$("text-lg text-gray-700 mb-8")]),
                toList([text("Hor\xE1rio: 22:00")])
              ),
              h2(
                toList([class$("text-2xl font-semibold text-pink-600 mb-4")]),
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
                toList([class$("text-2xl font-semibold text-pink-600 mb-4")]),
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

// build/dev/javascript/client/client/views/gallery_view.mjs
function image_widget(image) {
  return img(
    toList([
      class$("flex justify-center rounded-lg shadow-lg"),
      alt("Foto"),
      src(image)
    ])
  );
}
function gallery_view(model) {
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
        toList([class$("grid grid-cols-1 sm:grid-cols-2 md:grid-cols-3 gap-3")]),
        map2(model.gallery_images, image_widget)
      )
    ])
  );
}

// build/dev/javascript/client/client/views/gifts_view.mjs
function sugestion_gift(gift) {
  return div(
    toList([
      class$(
        "bg-white p-4 rounded-lg shadow-lg flex flex-col items-center justify-between h-80"
      )
    ]),
    toList([
      img(
        toList([
          class$("w-full h-48 rounded-lg object-cover mb-4"),
          alt(
            "Presente" + (() => {
              let _pipe = gift.id;
              return to_string2(_pipe);
            })()
          ),
          src(gift.pic)
        ])
      ),
      h3(
        toList([class$("text-lg font-semibold text-pink-700 text-center")]),
        toList([text(gift.name)])
      )
    ])
  );
}
function unselected_gift(gift, link) {
  return div(
    toList([
      class$(
        "relative bg-white p-4 rounded-lg shadow-lg items-center justify-between"
      )
    ]),
    toList([
      img(
        toList([
          class$("w-full h-48 rounded-lg object-cover z-0"),
          alt(
            "Presente" + (() => {
              let _pipe = gift.id;
              return to_string2(_pipe);
            })()
          ),
          src(gift.pic)
        ])
      ),
      h3(
        toList([class$("text-lg font-semibold text-pink-700 mt-2")]),
        toList([text(gift.name)])
      ),
      a(
        toList([
          class$("text-pink-600 hover:text-pink-800 underline text-center"),
          rel("noopener noreferrer"),
          target("_blank"),
          href(link)
        ]),
        toList([text("Ver refer\xEAncia")])
      ),
      button(
        toList([
          class$(
            "mt-3 w-full bg-emerald-600 hover:bg-emerald-700 text-white font-bold py-1 px-3 rounded-full transition duration-300"
          ),
          on_click(new UserRequestedSelectGift(gift, true))
        ]),
        toList([text("Escolher")])
      )
    ])
  );
}
function selected_by_user_gift(gift, link) {
  return div(
    toList([
      class$(
        "relative bg-white p-4 rounded-lg shadow-lg items-center justify-between"
      )
    ]),
    toList([
      img(
        toList([
          class$("w-full h-48 rounded-lg object-cover z-0"),
          alt(
            "Presente" + (() => {
              let _pipe = gift.id;
              return to_string2(_pipe);
            })()
          ),
          src(gift.pic)
        ])
      ),
      h3(
        toList([class$("text-lg font-semibold text-pink-700 mt-2")]),
        toList([text(gift.name)])
      ),
      a(
        toList([
          class$("text-pink-600 hover:text-pink-800 underline text-center"),
          rel("noopener noreferrer"),
          target("_blank"),
          href(link)
        ]),
        toList([text("Ver refer\xEAncia")])
      ),
      button(
        toList([
          class$(
            "mt-3 w-full bg-white-600 hover:bg-emerald-300 text-emerald font-bold py-1 px-3 rounded-full transition duration-300"
          ),
          on_click(new UserRequestedSelectGift(gift, false))
        ]),
        toList([text("Retirar Escolha")])
      )
    ])
  );
}
function selected_gift(gift) {
  return div(
    toList([class$("relative bg-white p-3 rounded-lg shadow-lg")]),
    toList([
      div(
        toList([class$("absolute inset-0 bg-black opacity-60 rounded-lg")]),
        toList([])
      ),
      img(
        toList([
          class$("w-full h-48 rounded-lg object-cover grayscale z-10"),
          alt(
            "Presente" + (() => {
              let _pipe = gift.id;
              return to_string2(_pipe);
            })()
          ),
          src(gift.pic)
        ])
      ),
      h3(
        toList([class$("text-lg font-semibold text-pink-300 mt-2")]),
        toList([text(gift.name)])
      ),
      a(
        toList([class$("text-pink-300 underline")]),
        toList([text("Ver refer\xEAncia")])
      ),
      button(
        toList([
          disabled(true),
          class$(
            "mt-3 w-full bg-pink-600 text-white font-bold py-1 px-3 rounded-full cursor-not-allowed"
          )
        ]),
        toList([text("Presente Selecionado")])
      )
    ])
  );
}
function unique_gift(model, gift) {
  let $ = gift.link;
  let $1 = gift.selected_by;
  let $2 = model.auth_user;
  if ($ instanceof Some && $1 instanceof Some && $2 instanceof Some && $1[0] === $2[0].user_id) {
    let link = $[0];
    let selected_by = $1[0];
    let user = $2[0];
    return selected_by_user_gift(gift, link);
  } else if ($ instanceof Some && $1 instanceof None) {
    let link = $[0];
    return unselected_gift(gift, link);
  } else if ($1 instanceof Some) {
    return selected_gift(gift);
  } else {
    return selected_gift(gift);
  }
}
function gifts_view(model) {
  return main(
    toList([class$("w-full max-w-6xl p-8 mt-12 flex flex-col items-center")]),
    toList([
      h1(
        toList([
          attribute("style", "font-family: 'Pacifico', cursive;"),
          class$("text-5xl text-white font-bold mb-12 p-12")
        ]),
        toList([text("Lista de Presentes")])
      ),
      h2(
        toList([class$("text-3xl text-white font-bold mb-6")]),
        toList([text("Sugest\xF5es de Presentes")])
      ),
      div(
        toList([
          class$("grid grid-cols-2 sm:grid-cols-3 lg:grid-cols-4 gap-6 w-full")
        ]),
        map2(model.gift_status.sugestion, sugestion_gift)
      ),
      h2(
        toList([class$("text-3xl text-white font-bold mb-6 p-12")]),
        toList([text("Presentes \xDAnicos")])
      ),
      div(
        toList([
          class$("grid grid-cols-2 sm:grid-cols-3 lg:grid-cols-4 gap-4 w-full")
        ]),
        map2(
          model.gift_status.unique,
          (gift) => {
            return unique_gift(model, gift);
          }
        )
      ),
      div(
        toList([]),
        toList([
          (() => {
            let $ = model.gift_status.error;
            if ($ instanceof Some) {
              let err = $[0];
              return p(
                toList([class$("text-red-500 text-center")]),
                toList([text("Erro: " + err)])
              );
            } else {
              return none2();
            }
          })()
        ])
      )
    ])
  );
}

// build/dev/javascript/client/client/views/not_found_view.mjs
function not_found_view() {
  return div(
    toList([class$("flex items-center justify-center min-h-screen text-white")]),
    toList([strong(toList([]), toList([text2("404 Not Found")]))])
  );
}

// build/dev/javascript/client/client.mjs
function view(model) {
  return body(
    toList([
      class$(
        "bg-gradient-to-r from-pink-400 to-pink-200 min-h-screen flex flex-col items-center justify-start"
      ),
      id("app")
    ]),
    toList([
      navigation_bar_view(model),
      div(toList([class$("mt-10")]), toList([])),
      (() => {
        let $ = model.route;
        if ($ instanceof Home) {
          return home_view(model);
        } else if ($ instanceof Event3) {
          return event_view();
        } else if ($ instanceof Gallery) {
          return gallery_view(model);
        } else if ($ instanceof Gifts) {
          return gifts_view(model);
        } else if ($ instanceof Login) {
          return login_view(model);
        } else if ($ instanceof Comments) {
          return comments_view(model);
        } else if ($ instanceof Admin) {
          return admin_view(model);
        } else if ($ instanceof ConfirmPresence) {
          return confirm_presence_view(model);
        } else {
          return not_found_view();
        }
      })(),
      footer_view()
    ])
  );
}
function get_gifts() {
  let url = get_api_url() + "/gifts";
  let decoder = list(
    decode5(
      (var0, var1, var2, var3, var4) => {
        return new Gift(var0, var1, var2, var3, var4);
      },
      field("id", int),
      field("name", string),
      field("pic", string),
      field("link", optional(string)),
      field("selected_by", optional(int))
    )
  );
  let tuple_decoder = decode2(
    (sugestion, unique) => {
      return [sugestion, unique];
    },
    field("sugestion_gifts", decoder),
    field("unique_gifts", decoder)
  );
  return get2(
    url,
    expect_json(
      tuple_decoder,
      (var0) => {
        return new GiftsRecieved(var0);
      }
    )
  );
}
function get_images() {
  let url = get_api_url() + "/images";
  let decoder = list(field("src", string));
  return get2(
    url,
    expect_json(
      decoder,
      (var0) => {
        return new ImagesRecieved(var0);
      }
    )
  );
}
function get_comments() {
  let url = get_api_url() + "/comments";
  let decoder = list(
    decode2(
      (var0, var1) => {
        return new Comment(var0, var1);
      },
      field("name", string),
      field("comment", optional(string))
    )
  );
  return get2(
    url,
    expect_json(
      decoder,
      (var0) => {
        return new CommentsRecieved(var0);
      }
    )
  );
}
function update_countdown() {
  let countdown = diff2(
    new Days(),
    today(),
    from_calendar_date(2024, new Dec(), 14)
  );
  return from2(
    (dispatch) => {
      return dispatch(new CountdownUpdated(countdown));
    }
  );
}
function handle_login_signup(model) {
  let $ = model.login_form.sign_up;
  if ($) {
    return signup(model);
  } else {
    return login(model);
  }
}
function default_transform_data(_, api_data) {
  return api_data;
}
function confirmations_to_admin_settings(model, confirmation_data) {
  return model.admin_settings.withFields({
    confirmations: confirmation_data[1],
    show_details: (() => {
      let _pipe = confirmation_data[1];
      let _pipe$1 = group(
        _pipe,
        (confirmation) => {
          return confirmation.id;
        }
      );
      return map_values(_pipe$1, (_, _1) => {
        return false;
      });
    })(),
    total: confirmation_data[0]
  });
}
function gifts_to_gift_status(model, gifts) {
  return model.gift_status.withFields({ sugestion: gifts[0], unique: gifts[1] });
}
function handle_api_response(model, response, transform_data, apply_update) {
  if (response.isOk()) {
    let api_data = response[0];
    let _pipe = model;
    let _pipe$1 = apply_update(
      _pipe,
      (() => {
        let _pipe$12 = model;
        return transform_data(_pipe$12, api_data);
      })()
    );
    return none3(_pipe$1);
  } else {
    let _pipe = model;
    return none3(_pipe);
  }
}
function update(model, msg) {
  if (msg instanceof AuthUserRecieved) {
    let user_result = msg[0];
    return handle_api_response(
      model,
      user_result,
      default_transform_data,
      update_user
    );
  } else if (msg instanceof GiftsRecieved) {
    let gifts_result = msg[0];
    return handle_api_response(
      model,
      gifts_result,
      gifts_to_gift_status,
      update_gifts
    );
  } else if (msg instanceof ImagesRecieved) {
    let images_result = msg[0];
    return handle_api_response(
      model,
      images_result,
      default_transform_data,
      update_images
    );
  } else if (msg instanceof CommentsRecieved) {
    let comments_result = msg[0];
    return handle_api_response(
      model,
      comments_result,
      default_transform_data,
      update_comments
    );
  } else if (msg instanceof ConfirmationsRecieved) {
    let confirmations_result = msg[0];
    return handle_api_response(
      model,
      confirmations_result,
      confirmations_to_admin_settings,
      update_admin_settings
    );
  } else if (msg instanceof CountdownUpdated) {
    let value3 = msg.value;
    let _pipe = update_event_countdown(model, value3);
    return none3(_pipe);
  } else if (msg instanceof LoginUpdateUsername) {
    let value3 = msg.value;
    let _pipe = update_login_username(model, value3);
    return none3(_pipe);
  } else if (msg instanceof LoginUpdateEmail) {
    let value3 = msg.value;
    let _pipe = update_login_email(model, value3);
    return effect(
      _pipe,
      from2(
        (dispatch) => {
          return dispatch(new ConfirmUpdateEmail(value3));
        }
      )
    );
  } else if (msg instanceof LoginUpdatePassword) {
    let value3 = msg.value;
    let _pipe = update_login_password(model, value3);
    return none3(_pipe);
  } else if (msg instanceof LoginUpdateConfirmPassword) {
    let value3 = msg.value;
    let _pipe = update_login_confirm_password(model, value3);
    return none3(_pipe);
  } else if (msg instanceof LoginUpdateError) {
    let value3 = msg.value;
    let _pipe = update_login_error(model, value3);
    return none3(_pipe);
  } else if (msg instanceof UserRequestedLoginSignUp) {
    handle_login_signup(model);
    let _pipe = model;
    return effect(_pipe, handle_login_signup(model));
  } else {
    let _pipe = model;
    return none3(_pipe);
  }
}
function init3(_) {
  let _pipe = init2();
  return effects(
    _pipe,
    toList([get_gifts(), update_countdown(), get_images(), get_comments()])
  );
}
function main2() {
  let _pipe = application(init3, update, view);
  return start2(_pipe, "#app", void 0);
}

// build/.lustre/entry.mjs
main2();
