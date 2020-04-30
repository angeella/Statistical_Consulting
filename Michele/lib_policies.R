policies.levels <- list(
  school_closing=list(
    "0"="No measures",
    "1"="Recommend closing",
    "2"="Require closing (only some levels or categories, eg just high school, or just public schools",
    "3"="Require closing all levels"
  ),
  workplace_closing=list(
    "0"="No measures",
    "1"="Recommend closing (or work from home)",
    "2"="require closing for some sectors or categories of workers",
    "3"="require closing (or work from home) all-but-essential workplaces (eg grocery stores, doctors)"
  ),
  cancel_events=list(
    "0"="No measures",
    "1"="Recommend cancelling",
    "2"="Require cancelling"
  ),
  transport_closing=list(
    "0"="No measures",
    "1"="Recommend closing (or significantly reduce volume/route/means of transport available)",
    "2"="Require closing (or prohibit most citizens from using it)"
  ),
  information_campaigns=list(
    "0"="No COVID-19 public information campaign",
    "1"="public officials urging caution about COVID-19",
    "2"="coordinated public information campaign (e.g. across traditional and social media)"
  ),
  internal_movement_restrictions=list(
    "0"="No measures",
    "1"="Recommend closing (or significantly reduce volume/route/means of transport)",
    "2"="Require closing (or prohibit most people from using it)"
  ),
  international_movement_restrictions=list(
    "0"="No measures",
    "1"="Screening",
    "2"="Quarantine arrivals from high-risk regions",
    "3"="Ban on high-risk regions",
    "4"="Total border closure"
  ),
  testing_policy=list(
    "0"="No testing policy",
    "1"="Only those who both (a) have symptoms AND (b) meet specific criteria (eg key workers, admitted to hospital, came into contact with a known case, returned from overseas)",
    "2"="testing of anyone showing COVID-19 symptoms",
    "3"="open public testing (eg 'drive through' testing available to asymptomatic people)"
  ),
  contact_tracing=list(
    "0"="No contact tracing",
    "1"="Limited contact tracing, not done for all cases",
    "2"="Comprehensive contact tracing, done for all cases"
  )
)
policies <- names(policies.levels)