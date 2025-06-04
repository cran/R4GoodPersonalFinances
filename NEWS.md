# R4GoodPersonalFinances 1.0.0

This major release introduces probably first in the world (open-sourced) implementation of a new multilevel life-cycle modeling of a household finances. 
It connects life-cycle models with single-period net-worth Markowitz 
optimization models and allows for high degree of personalization.
The implementation is based on a novel theoretical framework, 
described by Thomas M. Idzorek and Paul D. Kaplan in 
"Lifetime Financial Advice: A Personalized Optimal Multilevel Approach" (2024).

## Major changes

* Added `Household` R6 class that aggregates information about a household 
and its members and  `HouseholdMember` class that aggregates information 
about a single member of a household.
* Added functions responsible for setting, resetting and getting information about disk cache for caching the results of the simulations: `set_cache()`, `reset_cache()` and `get_cache()`.
* Added `calc_life_expectancy()` function to calculate the life expectancy of a person based on the Gompertz model.
* Added `plot_life_expectancy()` function to visualize the probability of dying and life expectancy of every member of a `Household` object and similar `plot_survival()` function for visualizing the probability of survival of each of the members of a `Household` and the probability of at least one member of a `Household` to survive.
* Added `create_portfolio_template()` function to create an exemplary `Portoflio` object - a template for default portfolio with two asset classes: `GlobalStocksIndexFund` and `InflationProtectedBonds` and with some reasonable default values that should be updated and customised further.
* Added `calc_portfolio_parameters()` function to calculate the parameters of a `Portfolio` object like current value, weights of assets, expected return and standard deviation of the whole portfolio.
* Added `calc_optimal_asset_allocation()` function to calculate optimal allocation for a particular `Household` and `Portfolio` objects.
* Added `plot_optimal_portfolio()` function to visualize the current and optimal allocation to assets for a particular `Portfolio` object.
* Added `simulate_scenario()` function to simulate a single scenario of cash flows for a particular `Household` and `Portfolio` objects using expected returns of the assets in portfolio or random returns in multiple Monte Carlo samples.
* Added `simulate_scenarios()` function to simulate multiple scenarios of cash flows for a particular `Household` and `Portfolio` objects. The scenarios parameters can triggered by different events like different years of retirement.
* Added `plot_scenarios()` function to visualize the scenarios metrics that help with choosing the optimal parameters for the best scenario, e.g. the optimal year for retirement.
* Added `plot_expected_allocation()` function to visualize the expected allocation of assets for a particular scenario over the household life cycle.
* Added `plot_expected_capital()` function to visualize the expected financial, human, and total capital, as well as liabilities for a particular scenario over the household life cycle.
* Added `plot_future_income()` and `plot_future_spending()` functions to visualize the future income and spending for a particular scenario over the household life cycle. The `plot_future_spending()` function allows to plot the relation of discretionary and non-discretionary spending, structure of non-discretionary spending and possible levels of discretionary spending over time based on Monte Carlo simulations.


# R4GoodPersonalFinances 0.2.0

This release provides functions for calculating probability of retirement ruin and functions for calibrating the underlying Gompertz model with the use of the mortality rates.

## Major changes

* Added `calc_retirement_ruin()` and `plot_retirement_ruin()` to calculate and visualize the probability of retirement ruin.
* Added `retirement-ruin` option to `run_app()` function for
running package new interactive app.
* Added `calc_gompertz_survival_probability()` to calculate the probability of survival using the Gompertz model. 
* Added `calc_gompertz_parameters()` to calculate the parameters of the Gompertz model based on mortality rates and `calc_gompertz_joint_parameters()` to calculate the parameters of the joint Gompertz model.
* Added `plot_gompertz_calibration()` for visually checking the calibration of the Gompertz model and `plot_joint_survival()` for plotting the survival probabilities for two individuals and their joint survival probability.
* Added `read_hmd_life_tables()` for reading the 'Human Mortality Database' (HMD) life tables files.

# R4GoodPersonalFinances 0.1.1

## Bugfixes

* Fixed apps errors while executing locally.

# R4GoodPersonalFinances 0.1.0

Initial release of the package includes functions designed to 
support the optimal allocation of your investment portfolio
with the use of the 'Merton Share' formula.

* Added `calc_purchasing_power()` and `plot_purchasing_power()` to calculate and visualize the effect of real interest rates—whether positive (compounding) or negative (inflation)—on the purchasing power of savings.  
* Added `calc_optimal_risky_asset_allocation()` for calculating optimal allocation using the 'Merton Share' formula.  
* Added `calc_risk_adjusted_return()` and `plot_risk_adjusted_returns()` to calculate and visualize the effect of allocation to risky assets on the risk-adjusted return.
* Added `run_app()` function that runs package interactive apps:
`risk-adjusted-returns` and `purchasing-power`.
