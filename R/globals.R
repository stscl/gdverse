# see https://forum.posit.co/t/how-to-solve-no-visible-binding-for-global-variable-note/28887
# or https://forum.posit.co/t/when-programming-with-dplyr-what-is-the-correct-way-to-avoid-undefined-global-variables/55946

utils::globalVariables(c(".", "Ecological", "Interaction", "Interactive variable",
                         "Q-statistic", "Risk","qstatistic", "variable", "variable1",
                         "variable2", "x", "zone1", "zone2"))
