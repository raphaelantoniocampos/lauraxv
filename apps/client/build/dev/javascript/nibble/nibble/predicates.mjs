import * as $list from "../../gleam_stdlib/gleam/list.mjs";
import * as $string from "../../gleam_stdlib/gleam/string.mjs";

export function string(str, predicate) {
  return (str !== "") && $list.all($string.to_graphemes(str), predicate);
}

export function is_lower_ascii(grapheme) {
  if (grapheme === "a") {
    return true;
  } else if (grapheme === "b") {
    return true;
  } else if (grapheme === "c") {
    return true;
  } else if (grapheme === "d") {
    return true;
  } else if (grapheme === "e") {
    return true;
  } else if (grapheme === "f") {
    return true;
  } else if (grapheme === "g") {
    return true;
  } else if (grapheme === "h") {
    return true;
  } else if (grapheme === "i") {
    return true;
  } else if (grapheme === "j") {
    return true;
  } else if (grapheme === "k") {
    return true;
  } else if (grapheme === "l") {
    return true;
  } else if (grapheme === "m") {
    return true;
  } else if (grapheme === "n") {
    return true;
  } else if (grapheme === "o") {
    return true;
  } else if (grapheme === "p") {
    return true;
  } else if (grapheme === "q") {
    return true;
  } else if (grapheme === "r") {
    return true;
  } else if (grapheme === "s") {
    return true;
  } else if (grapheme === "t") {
    return true;
  } else if (grapheme === "u") {
    return true;
  } else if (grapheme === "v") {
    return true;
  } else if (grapheme === "w") {
    return true;
  } else if (grapheme === "x") {
    return true;
  } else if (grapheme === "y") {
    return true;
  } else if (grapheme === "z") {
    return true;
  } else {
    return false;
  }
}

export function is_upper_ascii(grapheme) {
  if (grapheme === "A") {
    return true;
  } else if (grapheme === "B") {
    return true;
  } else if (grapheme === "C") {
    return true;
  } else if (grapheme === "D") {
    return true;
  } else if (grapheme === "E") {
    return true;
  } else if (grapheme === "F") {
    return true;
  } else if (grapheme === "G") {
    return true;
  } else if (grapheme === "H") {
    return true;
  } else if (grapheme === "I") {
    return true;
  } else if (grapheme === "J") {
    return true;
  } else if (grapheme === "K") {
    return true;
  } else if (grapheme === "L") {
    return true;
  } else if (grapheme === "M") {
    return true;
  } else if (grapheme === "N") {
    return true;
  } else if (grapheme === "O") {
    return true;
  } else if (grapheme === "P") {
    return true;
  } else if (grapheme === "Q") {
    return true;
  } else if (grapheme === "R") {
    return true;
  } else if (grapheme === "S") {
    return true;
  } else if (grapheme === "T") {
    return true;
  } else if (grapheme === "U") {
    return true;
  } else if (grapheme === "V") {
    return true;
  } else if (grapheme === "W") {
    return true;
  } else if (grapheme === "X") {
    return true;
  } else if (grapheme === "Y") {
    return true;
  } else if (grapheme === "Z") {
    return true;
  } else {
    return false;
  }
}

export function is_digit(grapheme) {
  if (grapheme === "0") {
    return true;
  } else if (grapheme === "1") {
    return true;
  } else if (grapheme === "2") {
    return true;
  } else if (grapheme === "3") {
    return true;
  } else if (grapheme === "4") {
    return true;
  } else if (grapheme === "5") {
    return true;
  } else if (grapheme === "6") {
    return true;
  } else if (grapheme === "7") {
    return true;
  } else if (grapheme === "8") {
    return true;
  } else if (grapheme === "9") {
    return true;
  } else {
    return false;
  }
}

export function is_whitespace(grapheme) {
  if (grapheme === " ") {
    return true;
  } else if (grapheme === "\t") {
    return true;
  } else if (grapheme === "\r") {
    return true;
  } else if (grapheme === "\n") {
    return true;
  } else {
    return false;
  }
}
