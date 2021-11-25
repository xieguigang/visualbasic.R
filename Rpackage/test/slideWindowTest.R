#Region "Microsoft.ROpen::25475471739d797d8f6e3704e0cdf89d, test\slideWindowTest.R"

    # Summaries:


#End Region

mz <- c(1,2,3,4,5,6,7,8,9,10);
into <- c(2353,43,4678,9347,593,52,3423,423456,4664646,54845522);

slices <- slide.windows(win_size = 2, step = 1, mz = mz, into = into);

# view debug result
data.frame(mz, into);
str(slices)
