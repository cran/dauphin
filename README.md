# Dauphin

Decode Australian phone numbers to integers

## Rationale

When dealing with mobile phone numbers in R they are often represented as
character vectors. However, this comes this with two problems: (1) character
vectors use excessive memory and (2) it necessitates a standard format to 
filter and match them.

`dauphin` attempts to solve these problems: using (32-bit) `integer` vectors
to store Australian mobile and landline numbers. 

```r
library(dauphin)
mobs <- c("0407 000 123", "0407000123", "61407 000 123", "+61 407 000 123")
dauphin_mobile(mobs)
```

If your data contains both landline and mobile (for the same client), you can
use `dauphin_mobile_landline` to ensure they are entered in the correct field.

```r
dauphin_mobile_landline(c("0412 345 678", "6532 1234"), 
                        c("03 6533 4444", "0400 111 222"), 
                        default_area_code = 3)
```

An unexported function `format_dauphin_mobile` is also temporarily available
for a standard character format.

