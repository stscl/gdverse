# see https://forum.posit.co/t/how-to-solve-no-visible-binding-for-global-variable-note/28887
# or https://forum.posit.co/t/when-programming-with-dplyr-what-is-the-correct-way-to-avoid-undefined-global-variables/55946

utils::globalVariables(c(".", "Ecological", "Interaction", "Interactive variable",
                         "Q-statistic", "Risk", "qstatistic", "variable", "variable1",
                         "variable2", "x", "zone1st", "zone2nd", "id_sample_new", "qv",
                         "spd_theta", "Variable1 and Variable2 interact Q-statistics",
                         "spd1", "spd2", "varibale", "P-value", "pv", "interactv",
                         "significance", "Variable1 SPD", "Variable2 SPD", "eco",
                         "risk", "n", "lr", "lr_before", "k", "method", "spade_cpsd",
                         "qv_theta", "qv_rid", "su", "v1", "v2", "zone_risk", "PD",
                         "Variable1 and Variable2 interact PD", "psd", "spd", "qv_text",
                         "variable_col", "name", "step", "xend", "y", "yend", "discnum",
                         "rate", "rpd", "qvalue"))

