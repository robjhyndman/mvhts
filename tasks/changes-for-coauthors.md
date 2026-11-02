# Major changes since the main branch

- **Data fixed.** The employment file on main (`Dados/Dados_emprego_rgi.csv`) had its time order reversed, so the application results on main are invalid.
  The reversal was in two blocks, each running backwards: the month labelled January 2004 was really December 2010, and the one labelled January 2011 was really December 2019.
  Months from 2020 on were in the right order.
  So the old models were fitted to a series with its trend and seasonal pattern running backwards.
  We found this by comparing the file with the official CAGED series on IpeaData.
- **Sample now 2004–2019.** The data are now the official PDET state counts for 2004–2019 (re-sent with correct dates in October 2026).
  As an independent check we rebuilt the series from the raw PDET microdata, which start in 2007: the two agree exactly in every state and every month whose microdata archive is intact (134 of 156; 22 archives are damaged on the server).
  Ending in 2019 avoids the January 2020 switch to Novo CAGED and the COVID shock.
- **New question and framing.** The paper no longer presents multivariate reconciliation as a new method that always helps.
  It now asks when joint reconciliation improves on reconciling each variable separately.
  A new theory section characterises exactly when the two coincide, building on and crediting existing results (Wickramasuriya 2021; Girolimetto & Di Fonzo 2025).
- **A diagnostic for real data.** New measures and a test indicate whether joint reconciliation can help for a given data set.
- **New simulations.** The old design could not show any difference between joint and separate reconciliation, so it has been replaced by one that can.
- **New application and main finding.** The application uses rolling origins over 2004–2019 and adds probabilistic forecasts.
  With ARIMA base forecasts, joint reconciliation improves dismissals (MSE 6–9% below separate reconciliation) but not admissions (2–3% above), and with VAR base forecasts it is worse for both.
  It improves forecasts of net employment change more clearly (national MSE down 17%).
- **New title and author.** "When does multivariate forecast reconciliation help?", with Felix Fesca (TU Dortmund) joining.
- **Paper rewritten in Quarto.** Edit the `.qmd` files; the `.tex` files are generated.
- **Reproducible workflow.** The Makefile is replaced by a targets pipeline, and package versions are locked with uvr.
