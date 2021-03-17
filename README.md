# monolix-lixoftconnector-examples

I created this project to compare results from a PK model run in the Monolix UI to results from the same PK model, but run using the lixoftConnectors package.

1. I created example data with simulate-data.R. The fake data is saved to `data/`.
2. The structural model is saved as `ada_model.txt`.
3. The R Markdown document `gui-to-lixoftconnectors.Rmd` outlines my procedure.
    + I recorded all the setting in the Monolix UI
    + I recreated these settings using lixoftConnectors
    + I compared the results
4. The "Compare Output" section shows how the results from the Monolix UI don't match results from the lixoftConnectors package, even though the .mlxtran files are specified the same.
