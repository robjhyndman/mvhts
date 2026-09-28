# Major changes since the main branch

- **Data fixed.** The employment file on main (`Dados/Dados_emprego_rgi.csv`) had its time order reversed, so the application results on main are invalid.
  The reversal was in two blocks, each running backwards: the month labelled January 2004 was really December 2010, and the one labelled January 2011 was really December 2019.
  Months from 2020 on were in the right order.
  So the old models were fitted to a series with its trend and seasonal pattern running backwards.
  We found this by comparing the file with the official CAGED series on IpeaData.
  The data are now rebuilt from the raw PDET microdata.
- **Sample now 2007–2019, because of gaps on the PDET website.** PDET publishes no microdata before 2007, so 2004–2006 cannot be rebuilt or checked.
  Of the 156 monthly archives for 2007–2019, 22 (all between 2008 and 2014) are damaged on the server: repeated downloads give identical files that fail partway through decompression.
  For those months we use the old file with its dates corrected in code.
  In every intact month the corrected old file agrees with the raw microdata to within one record per state, so we are confident in the fix.
  Ending in 2019 also avoids the January 2020 switch to Novo CAGED and the COVID shock.
  The 2020–2023 data are built but not analysed; they appear only as a shaded period in the data plot.
- **New question and framing.** The paper no longer presents multivariate reconciliation as a new method that always helps.
  It now asks when joint reconciliation improves on reconciling each variable separately.
  A new theory section characterises exactly when the two coincide, building on and crediting existing results (Wickramasuriya 2021; Girolimetto & Di Fonzo 2025).
- **A diagnostic for real data.** New measures and a test indicate whether joint reconciliation can help for a given data set.
- **New simulations.** The old design could not show any difference between joint and separate reconciliation, so it has been replaced by one that can.
- **New application and main finding.** The application uses rolling origins over 2007–2019 and adds probabilistic forecasts.
  Joint reconciliation does not help for admissions or dismissals individually, but it improves forecasts of net employment change (national MSE down 18%).
- **New title and author.** "When does multivariate forecast reconciliation help?", with Felix Fesca (TU Dortmund) joining.
- **Paper rewritten in Quarto.** Edit the `.qmd` files; the `.tex` files are generated.
- **Reproducible workflow.** The Makefile is replaced by a targets pipeline, and package versions are locked with uvr.
