# Making friends in an asymmetric game: the establishment of male-female grooming exchanges in vervet monkeys

Various concepts have been developed to explain cooperation among unrelated individuals, but linking proposed strategies to empirical evidence remains a challenge, especially for variable reciprocal investments. One famous strategy is “raise the stakes”, proposing low initial investments but a steady increase if the partner matches the other’s investments or increases their own investments in a series of exchanges. Contrarily, the “making friends” strategy predicts high investment between strangers that may either be kept up or downregulated if not fully matched. We tested these predictions on grooming exchanges between wild male-female vervet monkeys, a species with regular male dispersal. Contrary to both predictions, females had a consistently high initial investment and groomed immigrants for longer than was reciprocated, showing no adaptation to the formation of debts. Females, however, gradually reduced grooming over the first 6 months. Males, on the other hand, did not alter grooming investments over time. Thus, grooming exchanges eventually almost evened out. We present various potential explanations for these results, which are overall more compatible with the “making friends” strategy. Most importantly, this study highlights the complexity of real-life cooperation, emphasising the need for models that explicitly incorporate life history parameters into cooperative strategies.

## 0_Focal_per_male

This script processes the original focal data collected in the field (`Clean_focal.csv`) and extracts the relevant information for this study, creating `Focal_per_male_total.csv`. First, it filters the data for the males included in this study (see methods in the manuscript) and calculates the grooming duration of each interaction. Additionally, it defines the duration of a grooming bout.

The script relies on Clean_focal.csv, which contains two years of focal data collection in the IVP. This dataset is not publicly available due to data sensitivity, but access may be granted upon request.

## 1_Grooming_reciprocity_breakpoint

In this script you can find the breakpoint analysis used to define Immigrant and Resident categories for future analysis. In this script you can also find the models evaluating grooming reciprocity between males and females and generates the corresponding graph.

## 2_GroominDuration_data_cleaning

In this script we created ´GroomingDurationStruchange.csv´ dataset from ´Focal_per_male_total.csv´. In here we defined  Immigrant, Intermediate, Resident and Long term residents for the plots, based on the breakpoint model's number of days (see above). This script also prepares the dataset required for models comparing grooming investment between females and immigrants, as well as between females and residents, and vice versa.

## 3_Grooming_duration

This script contains the models that compare grooming duration investment across different categories: females grooming immigrants, immigrants grooming females, residents grooming females, and females grooming residents. The script also includes the code for generating the corresponding graph


