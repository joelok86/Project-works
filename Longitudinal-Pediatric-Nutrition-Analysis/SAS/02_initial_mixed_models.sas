/*=============================================================
  Longitudinal Pediatric Nutrition Analysis
  SAS Script 02: Initial Mixed-Effects Models

  Purpose:
  - Fit exploratory longitudinal mixed-effects models
  - Evaluate repeated anthropometric outcomes
  - Examine dietary and behavioral predictors
  - Account for within-participant correlation over follow-up

  Note:
  The original research data are not included in this repository.
  Variable names have been generalized for privacy.
=============================================================*/


/*-------------------------------------------------------------
  1. Model: Weight-for-height related outcome
-------------------------------------------------------------*/

proc mixed data=work.longitudinal_merged method=reml;

    class participant_id followup_month;

    model weight_height_z =
        dietary_carbohydrate
        dietary_fat
        dietary_protein
        total_calories
        / solution cl;

    random intercept / subject=participant_id;

run;


/*-------------------------------------------------------------
  2. Model: Weight-for-age related outcome
-------------------------------------------------------------*/

proc mixed data=work.longitudinal_merged method=reml;

    class participant_id followup_month;

    model weight_age_z =
        dietary_carbohydrate
        dietary_fat
        dietary_protein
        total_calories
        / solution cl;

    random intercept / subject=participant_id;

run;


/*-------------------------------------------------------------
  3. Model: Height-for-age related outcome
-------------------------------------------------------------*/

proc mixed data=work.longitudinal_merged method=reml;

    class participant_id followup_month;

    model height_age_z =
        dietary_carbohydrate
        dietary_fat
        dietary_protein
        total_calories
        / solution cl;

    random intercept / subject=participant_id;

run;


/*-------------------------------------------------------------
  4. Exploratory model with snack and beverage intake
-------------------------------------------------------------*/

proc mixed data=work.longitudinal_merged method=reml;

    class participant_id followup_month
          sugary_drink_frequency
          salty_snack_frequency
          sweets_frequency;

    model weight_height_z =
        sugary_drink_frequency
        salty_snack_frequency
        sweets_frequency
        / solution cl;

    random intercept / subject=participant_id;

run;


/*-------------------------------------------------------------
  5. Model including follow-up time
-------------------------------------------------------------*/

proc mixed data=work.longitudinal_merged method=reml;

    class participant_id followup_month;

    model weight_height_z =
        dietary_carbohydrate
        dietary_fat
        dietary_protein
        total_calories
        followup_month
        / solution cl;

    random intercept followup_month /
        subject=participant_id
        type=un;

    repeated followup_month /
        subject=participant_id
        type=un;

run;


/*-------------------------------------------------------------
  6. Estimated means across follow-up
-------------------------------------------------------------*/

proc mixed data=work.longitudinal_merged method=reml;

    class participant_id followup_month;

    model weight_age_z =
        dietary_carbohydrate
        dietary_fat
        dietary_protein
        total_calories
        followup_month
        / solution cl;

    random intercept / subject=participant_id;

    lsmeans followup_month / cl;

run;
