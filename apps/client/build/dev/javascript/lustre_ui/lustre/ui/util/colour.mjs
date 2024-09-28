import * as $colour from "../../../../gleam_community_colour/gleam_community/colour.mjs";
import * as $json from "../../../../gleam_json/gleam/json.mjs";
import * as $dynamic from "../../../../gleam_stdlib/gleam/dynamic.mjs";
import * as $result from "../../../../gleam_stdlib/gleam/result.mjs";
import { Ok, toList, CustomType as $CustomType, makeError } from "../../../gleam.mjs";

export class Scale extends $CustomType {
  constructor(app_background, app_background_subtle, app_border, element_background, element_background_hover, element_background_strong, element_border_subtle, element_border_strong, solid_background, solid_background_hover, text_high_contrast, text_low_contrast) {
    super();
    this.app_background = app_background;
    this.app_background_subtle = app_background_subtle;
    this.app_border = app_border;
    this.element_background = element_background;
    this.element_background_hover = element_background_hover;
    this.element_background_strong = element_background_strong;
    this.element_border_subtle = element_border_subtle;
    this.element_border_strong = element_border_strong;
    this.solid_background = solid_background;
    this.solid_background_hover = solid_background_hover;
    this.text_high_contrast = text_high_contrast;
    this.text_low_contrast = text_low_contrast;
  }
}

export function encode_scale(scale) {
  return $json.object(
    toList([
      ["app_background", $colour.encode(scale.app_background)],
      ["app_background_subtle", $colour.encode(scale.app_background_subtle)],
      ["app_border", $colour.encode(scale.app_border)],
      ["element_background", $colour.encode(scale.element_background)],
      [
        "element_background_hover",
        $colour.encode(scale.element_background_hover),
      ],
      [
        "element_background_strong",
        $colour.encode(scale.element_background_strong),
      ],
      ["element_border_subtle", $colour.encode(scale.element_border_subtle)],
      ["element_border_strong", $colour.encode(scale.element_border_strong)],
      ["solid_background", $colour.encode(scale.solid_background)],
      ["solid_background_hover", $colour.encode(scale.solid_background_hover)],
      ["text_high_contrast", $colour.encode(scale.text_high_contrast)],
      ["text_low_contrast", $colour.encode(scale.text_low_contrast)],
    ]),
  );
}

export function scale_decoder(json) {
  let attempt = (field, then$) => {
    return $result.try$($dynamic.field(field, $colour.decoder)(json), then$);
  };
  return attempt(
    "app_background",
    (app_background) => {
      return attempt(
        "app_background_subtle",
        (app_background_subtle) => {
          return attempt(
            "app_border",
            (app_border) => {
              return attempt(
                "element_background",
                (element_background) => {
                  return attempt(
                    "element_background_hover",
                    (element_background_hover) => {
                      return attempt(
                        "element_background_strong",
                        (element_background_strong) => {
                          return attempt(
                            "element_border_subtle",
                            (element_border_subtle) => {
                              return attempt(
                                "element_border_strong",
                                (element_border_strong) => {
                                  return attempt(
                                    "solid_background",
                                    (solid_background) => {
                                      return attempt(
                                        "solid_background_hover",
                                        (solid_background_hover) => {
                                          return attempt(
                                            "text_high_contrast",
                                            (text_high_contrast) => {
                                              return attempt(
                                                "text_low_contrast",
                                                (text_low_contrast) => {
                                                  return new Ok(
                                                    new Scale(
                                                      app_background,
                                                      app_background_subtle,
                                                      app_border,
                                                      element_background,
                                                      element_background_hover,
                                                      element_background_strong,
                                                      element_border_subtle,
                                                      element_border_strong,
                                                      solid_background,
                                                      solid_background_hover,
                                                      text_high_contrast,
                                                      text_low_contrast,
                                                    ),
                                                  );
                                                },
                                              );
                                            },
                                          );
                                        },
                                      );
                                    },
                                  );
                                },
                              );
                            },
                          );
                        },
                      );
                    },
                  );
                },
              );
            },
          );
        },
      );
    },
  );
}

function from_radix_scale(a, b, c, d, e, f, g, h, i, j, k, l) {
  let $ = $colour.from_rgb_hex(a);
  if (!$.isOk()) {
    throw makeError(
      "let_assert",
      "lustre/ui/util/colour",
      115,
      "from_radix_scale",
      "Pattern match failed, no pattern matched the value.",
      { value: $ }
    )
  }
  let app_background = $[0];
  let $1 = $colour.from_rgb_hex(b);
  if (!$1.isOk()) {
    throw makeError(
      "let_assert",
      "lustre/ui/util/colour",
      116,
      "from_radix_scale",
      "Pattern match failed, no pattern matched the value.",
      { value: $1 }
    )
  }
  let app_background_subtle = $1[0];
  let $2 = $colour.from_rgb_hex(c);
  if (!$2.isOk()) {
    throw makeError(
      "let_assert",
      "lustre/ui/util/colour",
      117,
      "from_radix_scale",
      "Pattern match failed, no pattern matched the value.",
      { value: $2 }
    )
  }
  let app_border = $2[0];
  let $3 = $colour.from_rgb_hex(d);
  if (!$3.isOk()) {
    throw makeError(
      "let_assert",
      "lustre/ui/util/colour",
      118,
      "from_radix_scale",
      "Pattern match failed, no pattern matched the value.",
      { value: $3 }
    )
  }
  let element_background = $3[0];
  let $4 = $colour.from_rgb_hex(e);
  if (!$4.isOk()) {
    throw makeError(
      "let_assert",
      "lustre/ui/util/colour",
      119,
      "from_radix_scale",
      "Pattern match failed, no pattern matched the value.",
      { value: $4 }
    )
  }
  let element_background_hover = $4[0];
  let $5 = $colour.from_rgb_hex(f);
  if (!$5.isOk()) {
    throw makeError(
      "let_assert",
      "lustre/ui/util/colour",
      120,
      "from_radix_scale",
      "Pattern match failed, no pattern matched the value.",
      { value: $5 }
    )
  }
  let element_background_strong = $5[0];
  let $6 = $colour.from_rgb_hex(g);
  if (!$6.isOk()) {
    throw makeError(
      "let_assert",
      "lustre/ui/util/colour",
      121,
      "from_radix_scale",
      "Pattern match failed, no pattern matched the value.",
      { value: $6 }
    )
  }
  let element_border_strong = $6[0];
  let $7 = $colour.from_rgb_hex(h);
  if (!$7.isOk()) {
    throw makeError(
      "let_assert",
      "lustre/ui/util/colour",
      122,
      "from_radix_scale",
      "Pattern match failed, no pattern matched the value.",
      { value: $7 }
    )
  }
  let element_border_subtle = $7[0];
  let $8 = $colour.from_rgb_hex(i);
  if (!$8.isOk()) {
    throw makeError(
      "let_assert",
      "lustre/ui/util/colour",
      123,
      "from_radix_scale",
      "Pattern match failed, no pattern matched the value.",
      { value: $8 }
    )
  }
  let solid_background = $8[0];
  let $9 = $colour.from_rgb_hex(j);
  if (!$9.isOk()) {
    throw makeError(
      "let_assert",
      "lustre/ui/util/colour",
      124,
      "from_radix_scale",
      "Pattern match failed, no pattern matched the value.",
      { value: $9 }
    )
  }
  let solid_background_hover = $9[0];
  let $10 = $colour.from_rgb_hex(k);
  if (!$10.isOk()) {
    throw makeError(
      "let_assert",
      "lustre/ui/util/colour",
      125,
      "from_radix_scale",
      "Pattern match failed, no pattern matched the value.",
      { value: $10 }
    )
  }
  let text_high_contrast = $10[0];
  let $11 = $colour.from_rgb_hex(l);
  if (!$11.isOk()) {
    throw makeError(
      "let_assert",
      "lustre/ui/util/colour",
      126,
      "from_radix_scale",
      "Pattern match failed, no pattern matched the value.",
      { value: $11 }
    )
  }
  let text_low_contrast = $11[0];
  return new Scale(
    app_background,
    app_background_subtle,
    app_border,
    element_background,
    element_background_hover,
    element_background_strong,
    element_border_subtle,
    element_border_strong,
    solid_background,
    solid_background_hover,
    text_high_contrast,
    text_low_contrast,
  );
}

export function grey() {
  return from_radix_scale(
    0xFCFCFC,
    0xF9F9F9,
    0xDDDDDD,
    0xF1F1F1,
    0xEBEBEB,
    0xE4E4E4,
    0xBBBBBB,
    0xD4D4D4,
    0x8D8D8D,
    0x808080,
    0x202020,
    0x646464,
  );
}

export function grey_dark() {
  return from_radix_scale(
    0x111111,
    0x191919,
    0x3A3A3A,
    0x222222,
    0x2A2A2A,
    0x313131,
    0x484848,
    0x606060,
    0x6E6E6E,
    0x7B7B7B,
    0xEEEEEE,
    0xB4B4B4,
  );
}

export function mauve() {
  return from_radix_scale(
    0xFDFCFD,
    0xFAF9FB,
    0xDFDCE3,
    0xF3F1F5,
    0xECEAEF,
    0xE6E3E9,
    0xBCBAC7,
    0xD5D3DB,
    0x8E8C99,
    0x817F8B,
    0x211F26,
    0x65636D,
  );
}

export function mauve_dark() {
  return from_radix_scale(
    0x121113,
    0x1A191B,
    0x3C393F,
    0x232225,
    0x2B292D,
    0x323035,
    0x49474E,
    0x625F69,
    0x6F6D78,
    0x7C7A85,
    0xEEEEF0,
    0xB5B2BC,
  );
}

export function slate() {
  return from_radix_scale(
    0xFCFCFD,
    0xF9F9FB,
    0xDDDDE3,
    0xF2F2F5,
    0xEBEBEF,
    0xE4E4E9,
    0xB9BBC6,
    0xD3D4DB,
    0x8B8D98,
    0x7E808A,
    0x1C2024,
    0x60646C,
  );
}

export function slate_dark() {
  return from_radix_scale(
    0x111113,
    0x18191B,
    0x363A3F,
    0x212225,
    0x272A2D,
    0x2E3135,
    0x43484E,
    0x5A6169,
    0x696E77,
    0x777B84,
    0xEDEEF0,
    0xB0B4BA,
  );
}

export function sage() {
  return from_radix_scale(
    0xFBFDFC,
    0xF7F9F8,
    0xDCDFDD,
    0xF0F2F1,
    0xE9ECEB,
    0xE3E6E4,
    0xB8BCBA,
    0xD2D5D3,
    0x868E8B,
    0x7A817F,
    0x1A211E,
    0x5F6563,
  );
}

export function sage_dark() {
  return from_radix_scale(
    0x101211,
    0x171918,
    0x373B39,
    0x202221,
    0x272A29,
    0x2E3130,
    0x444947,
    0x5B625F,
    0x63706B,
    0x717D79,
    0xECEEED,
    0xADB5B2,
  );
}

export function olive() {
  return from_radix_scale(
    0xFCFDFC,
    0xF8FAF8,
    0xDBDEDB,
    0xF1F3F1,
    0xEAECEA,
    0xE3E5E3,
    0xB9BCB8,
    0xD2D4D1,
    0x898E87,
    0x7C817B,
    0x1D211C,
    0x60655F,
  );
}

export function olive_dark() {
  return from_radix_scale(
    0x111210,
    0x181917,
    0x383A36,
    0x212220,
    0x282A27,
    0x2F312E,
    0x454843,
    0x5C625B,
    0x687066,
    0x767D74,
    0xECEEEC,
    0xAFB5AD,
  );
}

export function sand() {
  return from_radix_scale(
    0xFDFDFC,
    0xF9F9F8,
    0xDDDDDA,
    0xF2F2F0,
    0xEBEBE9,
    0xE4E4E2,
    0xBCBBB5,
    0xD3D2CE,
    0x8D8D86,
    0x80807A,
    0x21201C,
    0x63635E,
  );
}

export function sand_dark() {
  return from_radix_scale(
    0x111110,
    0x191918,
    0x3B3A37,
    0x222221,
    0x2A2A28,
    0x31312E,
    0x494844,
    0x62605B,
    0x6F6D66,
    0x7C7B74,
    0xEEEEEC,
    0xB5B3AD,
  );
}

export function gold() {
  return from_radix_scale(
    0xFDFDFC,
    0xFBF9F2,
    0xDAD1BD,
    0xF5F2E9,
    0xEEEADD,
    0xE5DFD0,
    0xB8A383,
    0xCBBDA4,
    0x978365,
    0x89775C,
    0x3B352B,
    0x71624B,
  );
}

export function gold_dark() {
  return from_radix_scale(
    0x121211,
    0x1B1A17,
    0x444039,
    0x24231F,
    0x2D2B26,
    0x38352E,
    0x544F46,
    0x696256,
    0x978365,
    0xA39073,
    0xE8E2D9,
    0xCBB99F,
  );
}

export function bronze() {
  return from_radix_scale(
    0xFDFCFC,
    0xFDF8F6,
    0xE0CEC7,
    0xF8F1EE,
    0xF2E8E4,
    0xEADDD7,
    0xBFA094,
    0xD1B9B0,
    0xA18072,
    0x947467,
    0x43302B,
    0x7D5E54,
  );
}

export function bronze_dark() {
  return from_radix_scale(
    0x141110,
    0x1C1917,
    0x493E3A,
    0x262220,
    0x302A27,
    0x3B3330,
    0x5A4C47,
    0x6F5F58,
    0xA18072,
    0xAE8C7E,
    0xEDE0D9,
    0xD4B3A5,
  );
}

export function brown() {
  return from_radix_scale(
    0xFEFDFC,
    0xFCF9F6,
    0xE8CDB5,
    0xF8F1EA,
    0xF4E9DD,
    0xEFDDCC,
    0xD09E72,
    0xDDB896,
    0xAD7F58,
    0x9E7352,
    0x3E332E,
    0x815E46,
  );
}

export function brown_dark() {
  return from_radix_scale(
    0x12110F,
    0x1C1816,
    0x4D3C2F,
    0x28211D,
    0x322922,
    0x3E3128,
    0x614A39,
    0x7C5F46,
    0xAD7F58,
    0xB88C67,
    0xF2E1CA,
    0xDBB594,
  );
}

export function yellow() {
  return from_radix_scale(
    0xFDFDF9,
    0xFFFBE0,
    0xECDD85,
    0xFFF8C6,
    0xFCF3AF,
    0xF7EA9B,
    0xC9AA45,
    0xDAC56E,
    0xFBE32D,
    0xF9DA10,
    0x473B1F,
    0x775F28,
  );
}

export function yellow_dark() {
  return from_radix_scale(
    0x14120B,
    0x1B180F,
    0x524202,
    0x2D2305,
    0x362B00,
    0x433500,
    0x665417,
    0x836A21,
    0xFFE629,
    0xFFFF57,
    0xF6EEB4,
    0xF5E147,
  );
}

export function amber() {
  return from_radix_scale(
    0xFEFDFB,
    0xFFF9ED,
    0xF5D08C,
    0xFFF3D0,
    0xFFECB7,
    0xFFE0A1,
    0xD6A35C,
    0xE4BB78,
    0xFFC53D,
    0xFFBA1A,
    0x4F3422,
    0x915930,
  );
}

export function amber_dark() {
  return from_radix_scale(
    0x16120C,
    0x1D180F,
    0x5C3D05,
    0x302008,
    0x3F2700,
    0x4D3000,
    0x714F19,
    0x8F6424,
    0xFFC53D,
    0xFFD60A,
    0xFFE7B3,
    0xFFCA16,
  );
}

export function orange() {
  return from_radix_scale(
    0xFEFCFB,
    0xFFF8F4,
    0xFFC291,
    0xFFEDD5,
    0xFFE0BB,
    0xFFD3A4,
    0xED8A5C,
    0xFFAA7D,
    0xF76808,
    0xED5F00,
    0x582D1D,
    0x99543A,
  );
}

export function orange_dark() {
  return from_radix_scale(
    0x17120E,
    0x1E160F,
    0x66350C,
    0x331E0B,
    0x462100,
    0x562800,
    0x7E451D,
    0xA35829,
    0xF76B15,
    0xFF801F,
    0xFFE0C2,
    0xFFA057,
  );
}

export function tomato() {
  return from_radix_scale(
    0xFFFCFC,
    0xFFF8F7,
    0xFAC7BE,
    0xFFF0EE,
    0xFFE6E2,
    0xFDD8D3,
    0xEA9280,
    0xF3B0A2,
    0xE54D2E,
    0xD84224,
    0x5C271F,
    0xC33113,
  );
}

export function tomato_dark() {
  return from_radix_scale(
    0x181111,
    0x1F1513,
    0x6E2920,
    0x391714,
    0x4E1511,
    0x5E1C16,
    0x853A2D,
    0xAC4D39,
    0xE54D2E,
    0xEC6142,
    0xFBD3CB,
    0xFF977D,
  );
}

export function red() {
  return from_radix_scale(
    0xFFFCFC,
    0xFFF7F7,
    0xF9C6C6,
    0xFFEFEF,
    0xFFE5E5,
    0xFDD8D8,
    0xEB9091,
    0xF3AEAF,
    0xE5484D,
    0xD93D42,
    0x641723,
    0xC62A2F,
  );
}

export function red_dark() {
  return from_radix_scale(
    0x191111,
    0x201314,
    0x72232D,
    0x3B1219,
    0x500F1C,
    0x611623,
    0x8C333A,
    0xB54548,
    0xE5484D,
    0xEC5D5E,
    0xFFD1D9,
    0xFF9592,
  );
}

export function ruby() {
  return from_radix_scale(
    0xFFFCFD,
    0xFFF7F9,
    0xF5C7D1,
    0xFEEFF3,
    0xFDE5EA,
    0xFAD8E0,
    0xE592A2,
    0xEEAFBC,
    0xE54666,
    0xDA3A5C,
    0x64172B,
    0xCA244D,
  );
}

export function ruby_dark() {
  return from_radix_scale(
    0x191113,
    0x1E1517,
    0x6F2539,
    0x3A141E,
    0x4E1325,
    0x5E1A2E,
    0x883447,
    0xB3445A,
    0xE54666,
    0xEC5A72,
    0xFED2E1,
    0xFF949D,
  );
}

export function crimson() {
  return from_radix_scale(
    0xFFFCFD,
    0xFFF7FB,
    0xF4C6DB,
    0xFEEFF6,
    0xFCE5F0,
    0xF9D8E7,
    0xE58FB1,
    0xEDADC8,
    0xE93D82,
    0xDC3175,
    0x621639,
    0xCB1D63,
  );
}

export function crimson_dark() {
  return from_radix_scale(
    0x191114,
    0x201318,
    0x6D2545,
    0x381525,
    0x4D122F,
    0x5C1839,
    0x873356,
    0xB0436E,
    0xE93D82,
    0xEE518A,
    0xFDD3E8,
    0xFF92AD,
  );
}

export function pink() {
  return from_radix_scale(
    0xFFFCFE,
    0xFFF7FC,
    0xF3C6E2,
    0xFEEEF8,
    0xFCE5F3,
    0xF9D8EC,
    0xE38EC3,
    0xECADD4,
    0xD6409F,
    0xCD3093,
    0x651249,
    0xC41C87,
  );
}

export function pink_dark() {
  return from_radix_scale(
    0x191117,
    0x21121D,
    0x692955,
    0x37172F,
    0x4B143D,
    0x591C47,
    0x833869,
    0xA84885,
    0xD6409F,
    0xDE51A8,
    0xFDD1EA,
    0xFF8DCC,
  );
}

export function plum() {
  return from_radix_scale(
    0xFEFCFF,
    0xFFF8FF,
    0xEBC8ED,
    0xFCEFFC,
    0xF9E5F9,
    0xF3D9F4,
    0xCF91D8,
    0xDFAFE3,
    0xAB4ABA,
    0xA43CB4,
    0x53195D,
    0x9C2BAD,
  );
}

export function plum_dark() {
  return from_radix_scale(
    0x181118,
    0x201320,
    0x5E3061,
    0x351A35,
    0x451D47,
    0x512454,
    0x734079,
    0x92549C,
    0xAB4ABA,
    0xB658C4,
    0xF4D4F4,
    0xE796F3,
  );
}

export function purple() {
  return from_radix_scale(
    0xFEFCFE,
    0xFDFAFF,
    0xE3CCF4,
    0xF9F1FE,
    0xF3E7FC,
    0xEDDBF9,
    0xBE93E4,
    0xD3B4ED,
    0x8E4EC6,
    0x8445BC,
    0x402060,
    0x793AAF,
  );
}

export function purple_dark() {
  return from_radix_scale(
    0x18111B,
    0x1E1523,
    0x54346B,
    0x301C3B,
    0x3D224E,
    0x48295C,
    0x664282,
    0x8457AA,
    0x8E4EC6,
    0x9A5CD0,
    0xECD9FA,
    0xD19DFF,
  );
}

export function violet() {
  return from_radix_scale(
    0xFDFCFE,
    0xFBFAFF,
    0xD7CFF9,
    0xF5F2FF,
    0xEDE9FE,
    0xE4DEFC,
    0xAA99EC,
    0xC4B8F3,
    0x6E56CF,
    0x644FC1,
    0x2F265F,
    0x5746AF,
  );
}

export function violet_dark() {
  return from_radix_scale(
    0x14121F,
    0x1B1525,
    0x473876,
    0x291F43,
    0x33255B,
    0x3C2E69,
    0x56468B,
    0x6958AD,
    0x6E56CF,
    0x7D66D9,
    0xE2DDFE,
    0xBAA7FF,
  );
}

export function iris() {
  return from_radix_scale(
    0xFDFDFF,
    0xFAFAFF,
    0xD0D0FA,
    0xF3F3FF,
    0xEBEBFE,
    0xE0E0FD,
    0x9B9EF0,
    0xBABBF5,
    0x5B5BD6,
    0x5353CE,
    0x272962,
    0x4747C2,
  );
}

export function iris_dark() {
  return from_radix_scale(
    0x13131E,
    0x171625,
    0x3D3E82,
    0x202248,
    0x262A65,
    0x303374,
    0x4A4A95,
    0x5958B1,
    0x5B5BD6,
    0x6E6ADE,
    0xE0DFFE,
    0xB1A9FF,
  );
}

export function indigo() {
  return from_radix_scale(
    0xFDFDFE,
    0xF8FAFF,
    0xC6D4F9,
    0xF0F4FF,
    0xE6EDFE,
    0xD9E2FC,
    0x8DA4EF,
    0xAEC0F5,
    0x3E63DD,
    0x3A5CCC,
    0x1F2D5C,
    0x3451B2,
  );
}

export function indigo_dark() {
  return from_radix_scale(
    0x11131F,
    0x141726,
    0x304384,
    0x182449,
    0x1D2E62,
    0x253974,
    0x3A4F97,
    0x435DB1,
    0x3E63DD,
    0x5472E4,
    0xD6E1FF,
    0x9EB1FF,
  );
}

export function blue() {
  return from_radix_scale(
    0xFBFDFF,
    0xF5FAFF,
    0xB7D9F8,
    0xEDF6FF,
    0xE1F0FF,
    0xCEE7FE,
    0x5EB0EF,
    0x96C7F2,
    0x91FF,
    0x880EA,
    0x113264,
    0xB68CB,
  );
}

export function blue_dark() {
  return from_radix_scale(
    0xD1520,
    0x111927,
    0x104D87,
    0xD2847,
    0x3362,
    0x4074,
    0x205D9E,
    0x2870BD,
    0x90FF,
    0x3B9EFF,
    0xC2E6FF,
    0x70B8FF,
  );
}

export function cyan() {
  return from_radix_scale(
    0xFAFDFE,
    0xF2FCFD,
    0xAADEE6,
    0xE7F9FB,
    0xD8F3F6,
    0xC4EAEF,
    0x3DB9CF,
    0x84CDDA,
    0x5A2C2,
    0x894B3,
    0xD3C48,
    0xC7792,
  );
}

export function cyan_dark() {
  return from_radix_scale(
    0xB161A,
    0x101B20,
    0x45468,
    0x82C36,
    0x3848,
    0x4558,
    0x12677E,
    0x11809C,
    0xA2C7,
    0x23AFD0,
    0xB6ECF7,
    0x4CCCE6,
  );
}

export function teal() {
  return from_radix_scale(
    0xFAFEFD,
    0xF1FCFA,
    0xAFDFD7,
    0xE7F9F5,
    0xD9F3EE,
    0xC7EBE5,
    0x53B9AB,
    0x8DCEC3,
    0x12A594,
    0xE9888,
    0xD3D38,
    0x67A6F,
  );
}

export function teal_dark() {
  return from_radix_scale(
    0xD1514,
    0x111C1B,
    0x145750,
    0xD2D2A,
    0x23B37,
    0x84843,
    0x1C6961,
    0x207E73,
    0x12A594,
    0xEB39E,
    0xADF0DD,
    0xBD8B6,
  );
}

export function jade() {
  return from_radix_scale(
    0xFBFEFD,
    0xEFFDF6,
    0xB0E0CC,
    0xE4FAEF,
    0xD7F4E6,
    0xC6ECDB,
    0x56BA9F,
    0x8FCFB9,
    0x29A383,
    0x259678,
    0x1D3B31,
    0x1A7A5E,
  );
}

export function jade_dark() {
  return from_radix_scale(
    0xD1512,
    0x121C18,
    0x1B5745,
    0xF2E22,
    0xB3B2C,
    0x114837,
    0x246854,
    0x2A7E68,
    0x29A383,
    0x27B08B,
    0xADF0D4,
    0x1FD8A4,
  );
}

export function green() {
  return from_radix_scale(
    0xFBFEFC,
    0xF2FCF5,
    0xB4DFC4,
    0xE9F9EE,
    0xDDF3E4,
    0xCCEBD7,
    0x5BB98C,
    0x92CEAC,
    0x30A46C,
    0x299764,
    0x193B2D,
    0x18794E,
  );
}

export function green_dark() {
  return from_radix_scale(
    0xE1512,
    0x121B17,
    0x20573E,
    0x132D21,
    0x113B29,
    0x174933,
    0x28684A,
    0x2F7C57,
    0x30A46C,
    0x33B074,
    0xB1F1CB,
    0x3DD68C,
  );
}

export function grass() {
  return from_radix_scale(
    0xFBFEFB,
    0xF3FCF3,
    0xB7DFBA,
    0xEBF9EB,
    0xDFF3DF,
    0xCEEBCF,
    0x65BA75,
    0x97CF9C,
    0x46A758,
    0x3D9A50,
    0x203C25,
    0x297C3B,
  );
}

export function grass_dark() {
  return from_radix_scale(
    0xE1511,
    0x141A15,
    0x2D5736,
    0x1B2A1E,
    0x1D3A24,
    0x25482D,
    0x366740,
    0x3E7949,
    0x46A758,
    0x53B365,
    0xC2F0C2,
    0x71D083,
  );
}

export function lime() {
  return from_radix_scale(
    0xFCFDFA,
    0xF7FCF0,
    0xC6DE99,
    0xEDFADA,
    0xE2F5C4,
    0xD5EDAF,
    0x9AB654,
    0xB2CA7F,
    0xBDEE63,
    0xB0E64C,
    0x37401C,
    0x59682C,
  );
}

export function lime_dark() {
  return from_radix_scale(
    0x11130C,
    0x151A10,
    0x3D522A,
    0x1F2917,
    0x29371D,
    0x334423,
    0x496231,
    0x577538,
    0xBDEE63,
    0xD4FF70,
    0xE3F7BA,
    0xBDE56C,
  );
}

export function mint() {
  return from_radix_scale(
    0xF9FEFD,
    0xEFFEFA,
    0xA6E1D3,
    0xDDFBF3,
    0xCCF7EC,
    0xBBEEE2,
    0x51BDA7,
    0x87D0BF,
    0x86EAD4,
    0x7FE1CC,
    0x16433C,
    0x27756A,
  );
}

export function mint_dark() {
  return from_radix_scale(
    0xE1515,
    0xF1B1B,
    0x105650,
    0x92C2B,
    0x3A38,
    0x4744,
    0x1E685F,
    0x277F70,
    0x86EAD4,
    0xA8F5E5,
    0xC4F5E1,
    0x58D5BA,
  );
}

export function sky() {
  return from_radix_scale(
    0xF9FEFF,
    0xF1FCFF,
    0xA5DCED,
    0xE2F9FF,
    0xD2F4FD,
    0xBFEBF8,
    0x46B8D8,
    0x82CAE0,
    0x7CE2FE,
    0x72DBF8,
    0x19404D,
    0x256E93,
  );
}

export function sky_dark() {
  return from_radix_scale(
    0xD141F,
    0x111A27,
    0x1B537B,
    0x112840,
    0x113555,
    0x154467,
    0x1F6692,
    0x197CAE,
    0x7CE2FE,
    0xA8EEFF,
    0xC2F3FF,
    0x75C7F0,
  );
}
