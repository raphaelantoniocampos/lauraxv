import * as $int from "../../gleam_stdlib/gleam/int.mjs";
import * as $string from "../../gleam_stdlib/gleam/string.mjs";
import * as $date from "../rada/date.mjs";
import { Apr, Aug, Dec, Feb, Jan, Jul, Jun, Mar, May, Nov, Oct, Sep } from "../rada/date.mjs";

function month_name_it(month) {
  if (month instanceof Jan) {
    return "gennaio";
  } else if (month instanceof Feb) {
    return "febbraio";
  } else if (month instanceof Mar) {
    return "marzo";
  } else if (month instanceof Apr) {
    return "aprile";
  } else if (month instanceof May) {
    return "maggio";
  } else if (month instanceof Jun) {
    return "giugno";
  } else if (month instanceof Jul) {
    return "luglio";
  } else if (month instanceof Aug) {
    return "agosto";
  } else if (month instanceof Sep) {
    return "settembre";
  } else if (month instanceof Oct) {
    return "ottobre";
  } else if (month instanceof Nov) {
    return "novembre";
  } else {
    return "dicembre";
  }
}

function month_name_short_it(month) {
  if (month instanceof Jan) {
    return "gen";
  } else if (month instanceof Feb) {
    return "feb";
  } else if (month instanceof Mar) {
    return "mar";
  } else if (month instanceof Apr) {
    return "apr";
  } else if (month instanceof May) {
    return "mag";
  } else if (month instanceof Jun) {
    return "giu";
  } else if (month instanceof Jul) {
    return "lug";
  } else if (month instanceof Aug) {
    return "ago";
  } else if (month instanceof Sep) {
    return "set";
  } else if (month instanceof Oct) {
    return "ott";
  } else if (month instanceof Nov) {
    return "nov";
  } else {
    return "dic";
  }
}

function weekday_name_it(weekday) {
  if (weekday instanceof $date.Mon) {
    return "lunedì";
  } else if (weekday instanceof $date.Tue) {
    return "martedì";
  } else if (weekday instanceof $date.Wed) {
    return "mercoledì";
  } else if (weekday instanceof $date.Thu) {
    return "giovedì";
  } else if (weekday instanceof $date.Fri) {
    return "venerdì";
  } else if (weekday instanceof $date.Sat) {
    return "sabato";
  } else {
    return "domenica";
  }
}

function weekday_name_short_it(weekday) {
  if (weekday instanceof $date.Mon) {
    return "lun";
  } else if (weekday instanceof $date.Tue) {
    return "mar";
  } else if (weekday instanceof $date.Wed) {
    return "mer";
  } else if (weekday instanceof $date.Thu) {
    return "gio";
  } else if (weekday instanceof $date.Fri) {
    return "ven";
  } else if (weekday instanceof $date.Sat) {
    return "sab";
  } else {
    return "dom";
  }
}

function day_with_suffix_it(day) {
  return $int.to_string(day);
}

export function italian() {
  return new $date.Language(
    month_name_it,
    month_name_short_it,
    weekday_name_it,
    weekday_name_short_it,
    day_with_suffix_it,
  );
}

function month_name_de(month) {
  if (month instanceof Jan) {
    return "Januar";
  } else if (month instanceof Feb) {
    return "Februar";
  } else if (month instanceof Mar) {
    return "März";
  } else if (month instanceof Apr) {
    return "April";
  } else if (month instanceof May) {
    return "Mai";
  } else if (month instanceof Jun) {
    return "Juni";
  } else if (month instanceof Jul) {
    return "Juli";
  } else if (month instanceof Aug) {
    return "August";
  } else if (month instanceof Sep) {
    return "September";
  } else if (month instanceof Oct) {
    return "Oktober";
  } else if (month instanceof Nov) {
    return "November";
  } else {
    return "Dezember";
  }
}

function month_name_short_de(month) {
  let _pipe = month_name_de(month);
  return $string.slice(_pipe, 0, 3);
}

function weekday_name_de(weekday) {
  if (weekday instanceof $date.Mon) {
    return "Montag";
  } else if (weekday instanceof $date.Tue) {
    return "Dienstag";
  } else if (weekday instanceof $date.Wed) {
    return "Mittwoch";
  } else if (weekday instanceof $date.Thu) {
    return "Donnerstag";
  } else if (weekday instanceof $date.Fri) {
    return "Freitag";
  } else if (weekday instanceof $date.Sat) {
    return "Samstag";
  } else {
    return "Sonntag";
  }
}

function weekday_name_short_de(weekday) {
  let _pipe = weekday_name_de(weekday);
  return $string.slice(_pipe, 0, 2);
}

function day_with_suffix_de(day) {
  return $int.to_string(day) + ".";
}

export function german() {
  return new $date.Language(
    month_name_de,
    month_name_short_de,
    weekday_name_de,
    weekday_name_short_de,
    day_with_suffix_de,
  );
}

function month_name_es(month) {
  if (month instanceof Jan) {
    return "enero";
  } else if (month instanceof Feb) {
    return "febrero";
  } else if (month instanceof Mar) {
    return "marzo";
  } else if (month instanceof Apr) {
    return "abril";
  } else if (month instanceof May) {
    return "mayo";
  } else if (month instanceof Jun) {
    return "junio";
  } else if (month instanceof Jul) {
    return "julio";
  } else if (month instanceof Aug) {
    return "agosto";
  } else if (month instanceof Sep) {
    return "setiembre";
  } else if (month instanceof Oct) {
    return "octubre";
  } else if (month instanceof Nov) {
    return "noviembre";
  } else {
    return "diciembre";
  }
}

function month_name_short_es(month) {
  if (month instanceof Jan) {
    return "ene";
  } else if (month instanceof Feb) {
    return "feb";
  } else if (month instanceof Mar) {
    return "mar";
  } else if (month instanceof Apr) {
    return "abr";
  } else if (month instanceof May) {
    return "may";
  } else if (month instanceof Jun) {
    return "jun";
  } else if (month instanceof Jul) {
    return "jul";
  } else if (month instanceof Aug) {
    return "ago";
  } else if (month instanceof Sep) {
    return "set";
  } else if (month instanceof Oct) {
    return "oct";
  } else if (month instanceof Nov) {
    return "nov";
  } else {
    return "dic";
  }
}

function weekday_name_es(weekday) {
  if (weekday instanceof $date.Mon) {
    return "lunes";
  } else if (weekday instanceof $date.Tue) {
    return "martes";
  } else if (weekday instanceof $date.Wed) {
    return "miercoles";
  } else if (weekday instanceof $date.Thu) {
    return "jueves";
  } else if (weekday instanceof $date.Fri) {
    return "viernes";
  } else if (weekday instanceof $date.Sat) {
    return "sabado";
  } else {
    return "domingo";
  }
}

function weekday_name_short_es(weekday) {
  if (weekday instanceof $date.Mon) {
    return "lun";
  } else if (weekday instanceof $date.Tue) {
    return "mar";
  } else if (weekday instanceof $date.Wed) {
    return "mie";
  } else if (weekday instanceof $date.Thu) {
    return "jue";
  } else if (weekday instanceof $date.Fri) {
    return "vie";
  } else if (weekday instanceof $date.Sat) {
    return "sab";
  } else {
    return "dom";
  }
}

function day_with_suffix_es(day) {
  return $int.to_string(day);
}

export function spanish() {
  return new $date.Language(
    month_name_es,
    month_name_short_es,
    weekday_name_es,
    weekday_name_short_es,
    day_with_suffix_es,
  );
}
