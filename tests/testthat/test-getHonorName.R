context("getHonorName")

all <- getHonorName("t0405.txt", honor = "all")
summa <- getHonorName("t0405.txt", honor = "summa")
magna <- getHonorName("t0405.txt", honor = "magna")
cum <- getHonorName("t0405.txt", honor = "cum")

test_that("check first and last elements", {
          expect_true(grepl("Bachelor of Arts, Summa Cum Laude", all[1,1]))
          expect_true(grepl("Bachelor of Arts", all[nrow(all),1]))

          expect_true(grepl("Bachelor of Arts, Summa Cum Laude", summa[1,1]))
          expect_true(grepl("Bachelor of Arts, Magna Cum Laude", summa[nrow(summa),1]))

          expect_true(grepl("Bachelor of Arts, Magna Cum Laude", magna[1,1]))
          expect_true(grepl("Bachelor of Arts, Cum Laude", magna[nrow(magna),1]))

          expect_true(grepl("Bachelor of Arts, Cum Laude", cum[1,1]))
          expect_true(grepl("Bachelor of Arts", cum[nrow(cum),1]))
          
          expect_true(grepl("Bachelor of Arts", cum[1,1]))
  })


