library(dauphin)

expect_error(dauphin_mobile_landline("0444 123 324", "02 9999 9999", "bb"), "must be integer")
expect_error(dauphin_landline("02 9999 9999", "bb"), "must be integer")
expect_error(dauphin_mobile_landline("0444 123 324", "02 9999 9999", integer(2)), "length.(one|1)")
expect_error(dauphin_mobile_landline("0444 123 324", "02 9999 9999", default_area_code = NA_integer_), "NA|missing")
expect_error(dauphin_mobile_landline("0444 123 324", "02 9999 9999", default_area_code = 55L), "55")


dauphin_mobile1 <- function(x) dauphin_mobile(x, ignore_calling_code = TRUE)

expect_equal(dauphin_mobile1("0401 234 234"), 401234234)
expect_equal(dauphin_mobile1("0401234234"), 401234234)
expect_equal(dauphin_mobile1("401234234"), 401234234)
expect_equal(dauphin_mobile1("61401 234 234"), 401234234)
expect_equal(dauphin_mobile1("+61401 234 234"), 401234234)
expect_equal(dauphin_mobile1("+61 401 234 234"), 401234234)
expect_equal(dauphin_mobile1("+61401234234"), 401234234)
expect_equal(dauphin_mobile1("+610401234234"), 401234234)
expect_equal(dauphin_mobile1("+61 0401234234"), 401234234)
expect_equal(dauphin_mobile1(c("+61401234234",
                               "0444 444 444",
                               NA_character_,
                               "03 1234 5678"))[1:3],
             c(401234234,
               444444444,
               NA,
               NA)[1:3])
expect_equal(dauphin_mobile1("61444 999 888"),
             dauphin_mobile1( "0444 999 888"))
expect_equal(dauphin_mobile1("BOB: 61444 999 888"),
             dauphin_mobile1( "0444 999 888"))

List_with_CC_Req <- dauphin::dauphin_mobile("+44 1234 5667")


expect_equal(dauphin_landline("98887777", default_area_code = 5L), 598887777L)
expect_equal(dauphin_landline("Home phone: 98887777", default_area_code = 5L), 598887777L)
expect_equal(dauphin_landline("9888 7777", default_area_code = 5L), 598887777L)
expect_equal(dauphin_landline(" 9888 7777", default_area_code = 5L), 598887777L)
expect_equal(dauphin_landline("05 9888 7777", default_area_code = 5L), 598887777L)
expect_equal(dauphin_landline("(05) 9888 7777", default_area_code = 5L), 598887777L)
expect_true(is.na(dauphin_landline("9888/7777", default_area_code = 5L)))
expect_true(is.na(dauphin_landline(" 9888/7777", default_area_code = 5L)))
expect_true(is.na(dauphin_landline("039888/7777", default_area_code = 5L)))
expect_true(is.na(dauphin_landline("(03)9888/7777")))
expect_true(is.na(dauphin_landline("", default_area_code = 5L)))
expect_true(is.na(dauphin_landline("NA", default_area_code = 5L)))
expect_true(is.na(dauphin_landline(NA_character_, default_area_code = 5L)))
expect_equal(dauphin_landline("0400123456"), 400123456)


expect_equal(dauphin_mobile_landline("0400 000 000", "(03)9876 5432", 2L),
             list(400e6, 398765432))
expect_equal(dauphin_mobile_landline("0400 000 000", "9876 5432", 2L),
             list(400e6, 298765432))
expect_equal(dauphin_mobile_landline("0400 000 000", "0398765432", 2L),
             list(400e6, 398765432))

expect_equal(dauphin_mobile_landline("0400 000 000/0365432100", "(03)9876 5432", 2L),
             list(400e6, 398765432))
# Reverse
expect_equal(dauphin_mobile_landline("(03)9876 5432", "0400 000 000/XX", 2L),
             list(400e6, 398765432))

expect_true(all(is.na(dauphin_mobile1(c("foo",
                                        "",
                                        NA_character_,
                                        "abcdefghiogsdfoih",
                                        "MOBILE NUMBER NOT FOUND",
                                        "Mobile Number Not Provide",
                                        "Mobile Number Not Provided",
                                        "0Mobile Number Not Provided",
                                        "23905723097235907235790",
                                        "04xxxxxxxx",
                                        "+ZZZZZZZZZZ",
                                        "9e11")))))
expect_true(all(is.na(dauphin_landline(c("foo",
                                        "",
                                        NA_character_,
                                        "abcdefghiogsdfoih",
                                        "MOBILE NUMBER NOT FOUND",
                                        "Mobile Number Not Provide",
                                        "Mobile Number Not Provided",
                                        "0Mobile Number Not Provided",
                                        "23905723097235907235790",
                                        "04xxxxxxxx",
                                        "+ZZZZZZZZZZ",
                                        "9e11")))))
for (.dauphin.long.vec in c(FALSE, TRUE)) {
  options(".dauphin.long.vec" = .dauphin.long.vec)
  x <- dauphin_mobile1("0400 000 456")
  class(x) <- "dauphin_mobile"
  expect_stdout(print(x), "61 400 000 456")

  x <- dauphin_mobile(c(NA_character_, rep("0400 000 456", 202)))
  class(x) <- "dauphin_mobile"
  expect_stdout(print(x), "--")
  expect_stdout(print(x), "NA")
  expect_stdout(dauphin:::print.dauphin_mobile(dauphin:::dauphin_mobile("+389 422 123 123")),
                "[+]389")
}

# Roundtrip
expect_equal(dauphin:::DecodeCC(dauphin:::EEncodeCC(61L)), 61L)
expect_equal(dauphin:::DecodeCC(dauphin:::EEncodeCC(44L)), 44L)

# format
format_dauphin_mobile <- dauphin:::format_dauphin_mobile
expect_equal(format_dauphin_mobile(422123123L), "+61 422 123 123")
expect_equal(format_dauphin_mobile(422123123L, raw(1)), "+61 422 123 123")
expect_true(startsWith(format_dauphin_mobile(dauphin_mobile("+44 123 123 123")), "+44"))



