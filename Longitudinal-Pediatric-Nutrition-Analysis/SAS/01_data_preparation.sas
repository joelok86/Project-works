/*=============================================================
  Longitudinal Pediatric Nutrition Analysis
  SAS Script 01: Data Preparation

  Purpose:
  - Derive participant age in months
  - Recode housing and food-security variables
  - Standardize dates
  - Prepare participant and visit identifiers
  - Merge dietary recall and follow-up records by participant
    and follow-up month

  Note:
  The original research data are not included in this repository.
  Variable names and file references have been generalized to
  protect restricted research data.
=============================================================*/


/*-------------------------------------------------------------
  1. Derive participant-level variables
-------------------------------------------------------------*/

data work.participant_coded;
    set work.source_followup;

    /* Age in months */
    age_months = (age_years * 12) + additional_months;


    /* Housing characteristics */
    if floor_earth = "Checked" then floor_score = 3;
    else if floor_cement = "Checked" then floor_score = 2;
    else if floor_ceramic = "Checked" then floor_score = 1;
    else if floor_other = "Checked" then floor_score = 4;
    else if floor_missing = "Checked" then floor_score = 5;
    else floor_score = .;


    if roof_sheet = "Checked" then roof_score = 3;
    else if roof_straw = "Checked" then roof_score = 2;
    else if roof_terrace = "Checked" then roof_score = 1;
    else if roof_other = "Checked" then roof_score = 4;
    else if roof_missing = "Checked" then roof_score = 5;
    else roof_score = .;


    if wall_wood = "Checked" then wall_score = 3;
    else if wall_canvas = "Checked" then wall_score = 1;
    else if wall_block = "Checked" then wall_score = 4;
    else if wall_sheet = "Checked" then wall_score = 2;
    else wall_score = .;


    /* Composite housing score */
    housing_score = sum(floor_score, roof_score, wall_score);


    /* Food availability */
    if difficulty_obtaining_food = "YES" then food_availability = 1;
    else if purchased_enough_food = "YES" then food_availability = 0;
    else food_availability = .;


    /* Food sufficiency */
    if ran_out_of_food = "YES"
       or missed_meal = "YES"
       then food_sufficiency = 1;

    else if child_one_meal_day = "YES"
       then food_sufficiency = 2;

    else if child_no_meal_day = "YES"
       then food_sufficiency = 3;

    else food_sufficiency = .;

run;


/*-------------------------------------------------------------
  2. Standardize visit dates
-------------------------------------------------------------*/

data work.followup_dates;
    set work.participant_coded;

    /*
      Example conversion from a character date.
      The appropriate informat depends on the original source.
    */

    visit_date = input(visit_date_character, anydtdte.);
    format visit_date date9.;

    followup_date_numeric = input(followup_date_character, mmddyy10.);
    followup_month = month(followup_date_numeric);

run;


/*-------------------------------------------------------------
  3. Prepare dietary recall data
-------------------------------------------------------------*/

data work.diet_recall_prepared;
    set work.diet_recall_source;

    /*
      Participant identifiers are represented generically
      in this public version.
    */

    participant_id = participant_identifier;

run;


/*-------------------------------------------------------------
  4. Prepare follow-up data
-------------------------------------------------------------*/

data work.followup_prepared;
    set work.followup_dates;

    participant_id = participant_identifier;

run;


/*-------------------------------------------------------------
  5. Sort datasets before merging
-------------------------------------------------------------*/

proc sort data=work.diet_recall_prepared;
    by participant_id followup_month;
run;

proc sort data=work.followup_prepared;
    by participant_id followup_month;
run;


/*-------------------------------------------------------------
  6. Merge dietary recall and follow-up records
-------------------------------------------------------------*/

data work.longitudinal_merged;
    merge
        work.diet_recall_prepared(in=has_diet)
        work.followup_prepared(in=has_followup);

    by participant_id followup_month;

    /*
      Retain records where both dietary recall and follow-up
      information are available for the participant-month.
    */

    if has_diet and has_followup;

run;


/*-------------------------------------------------------------
  7. Basic data-quality checks
-------------------------------------------------------------*/

proc freq data=work.longitudinal_merged;
    tables _all_ / missing;
run;

proc sort data=work.longitudinal_merged
          out=work.longitudinal_merged_sorted;
    by participant_id followup_month;
run;
