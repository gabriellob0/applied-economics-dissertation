# Social Norms and Gender

The core idea of the dissertation is to evaluate if there are significant differences in the presence of social norms between ethnic/social groups in the US.

## Roadmap

1. Finish literature review and core of the econometric section
2. Code additional analysis (e.g., housework gap and immigration year)
3. Prepare descriptive statistics
4. Write theory section

## Data Sources

TODO: Data Availability

TODO: Summary on Data

### Notes on Variables

| variable_name | label                                                | included | cleaning_notes                            | 1-2-1 correspondence                                        | data_notes                                                               | TODO                                      |
|---------------|------------------------------------------------------|----------|-------------------------------------------|-------------------------------------------------------------|--------------------------------------------------------------------------|-------------------------------------------|
| pid           | Personal id from original dataset                    | no       |                                           | cpf_pid                                                     |                                                                          |                                           |
| hid_v1        | Household id from original dataset (across waves)    | no       |                                           | cpf_hid                                                     |                                                                          |                                           |
| cpf_pid       | CPF personal id number                               | yes      |                                           | pid                                                         |                                                                          |                                           |
| cpf_hid       | CPF household id (across waves)                      | yes      |                                           | hid_v1                                                      |                                                                          |                                           |
| wave          | wave nr                                              | no       |                                           | wavey                                                       |                                                                          |                                           |
| wavey         | Wave - main year of data collection                  | yes      |                                           | wave                                                        |                                                                          |                                           |
| rel           | Relationship to hh head                              | yes      | removed relatives                         |                                                             |                                                                          |                                           |
| female        | Gender (female)                                      | yes      |                                           |                                                             |                                                                          |                                           |
| age           | Age                                                  | yes      | included only 18 to 65 yo   individuals   |                                                             |                                                                          |                                           |
| edu4          | Education: 4 levels                                  | yes      | removed negative values                   |                                                             | there is also a eduy variable that   could help me get a larger sample   |                                           |
| mlstat5       | Formal marital status                                | yes      | limited to only married individuals       |                                                             |                                                                          |                                           |
| livpart       | Living together with partner                         | yes      | limited to only co-inhabiting   partners  |                                                             |                                                                          |                                           |
| kidsn_hh17    | Number Of Children in HH aged 0-17                   | yes      |                                           |                                                             |                                                                          |                                           |
| emplst6       | Employment status [6]                                | yes      | limited to employed individuals           | seems to handle missing values   differently than emplstat5 |                                                                          |                                           |
|               |                                                      |          |                                           |                                                             |                                                                          |                                           |
| incjobs_yg    | Individual Labor Earnings (All jobs, year, gross)    | no       |                                           |                                                             |                                                                          |                                           |
| incjob1_yn    | Salary from main job (year, net)                     | no       |                                           |                                                             |                                                                          |                                           |
| injob1_mg     | Salary from main job (month, gross) [local currency] | yes      | restricted to strictly larger than   zero |                                                             |                                                                          | reconsider cleaning to match BKP          |
| incjob1_hg    | Salary from main job (per hour, gross)               | no       |                                           |                                                             |                                                                          | consider if useful                        |
| hhinc_post    | HH income(month, post)                               | yes      | restricted to strictly larger than   zero |                                                             |                                                                          |                                           |
| sampid_psid   | Sample identified: PSID                              | no       |                                           |                                                             | might be worth considering if I want   to look at the immigration sample |                                           |
| hisp          | Ethnicity (Hispanic)                                 | yes      | removed missing                           |                                                             | hispanics are a smaller sample                                           |                                           |
| cbirth        | Grew up in US                                        | no       |                                           |                                                             |                                                                          | could be interesting for immigrant sample |
| hwork         | Time use: Housework (hrs/wkd)                        | yes      |                                           |                                                             |                                                                          |                                           |
| ccare         | Time use: Child care (hrs/wkd)                       | no       |                                           |                                                             | mostly missing                                                           |                                           |
| allcare       | Time use: All care activities (hrs/wkd)              | no       |                                           |                                                             |                                                                          |                                           |
| rstate        | Region (NUTS 2)                                      | yes      |                                           |                                                             |                                                                          |                                           |
| immiyear      | Year of immigration                                  | no       |                                           |                                                             | small sample                                                             |                                           |

## Dependencies

TODO: Explain Python Stuff

TODO: Explain R stuff

## References