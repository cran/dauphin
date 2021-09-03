
s <- c("AB", "abc ", "", NA, "90", "ABC090")
expect_equal(dauphin:::grepl_digit(s), grepl("[0-9]", s))
expect_equal(dauphin:::gsub_09("1 234"), 1234)
expect_equal(dauphin:::gsub_09(c("000 2342", NA)), c(2342L, NA))
expect_equal(dauphin:::gsub_09(c("000 2342", "123 abc 234")), c(2342L, 123L))



