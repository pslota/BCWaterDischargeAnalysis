\name{SummaryStatistics}
\alias{SummaryStatistics}
\title{Statistics computed on an annual basis.}

\description{
The function \code{\link{compute.Q.stat.annual}} computes a large number of statistics on a
(calendar and water) year.

The following is a list of the summary statistics computed.
\describe{
\item{CY_MIN_01Day_SW}{Annual Min Discharge (cms)}
\item{CY_MINDOY_01Day_SW}{Date of Annual Min Discharge}
\item{CY_MIN_03Day_SW}{Minimum of Annual 3-day avg (cms)}
\item{CY_MINDOY_03Day_SW}{Date of Min Annual 3-day avg}
\item{CY_MIN_07Day_SW}{Min Annual 7-day avg (cms)}
\item{CY_MINDOY_07Day_SW}{Date of Min Annual 7-day avg}
\item{CY_MIN_30Day_SW}{Min Annual 30-day avg (cms)}
\item{CY_MINDOY_30Day_SW}{Date of Min Annual 30-day avg}
\item{CY_MIN_DAILY_SW}{Annual Min Discharge (cms)}
\item{CY_MAX_DAILY_SW}{Annual Max Discharge (cms)}
\item{CY_MEAN_DAILY_SW}{Annual Mean Discharge (cms)}
\item{CY_TOTALQ_DAILY_SW}{Annual Total Discharge (m3)}
\item{CY_YIELDMM_DAILY_SW}{Annual Water Yield (mm)}
\item{CY_CUMQ_DAILY_SW}{Cumulative Sum of Annual Daily Q (cms)}
\item{CY_Date_25P_CUMQ_DAILY_SW}{Date of 25\% of Annual Cumulative Discharge}
\item{CY_Date_50P_CUMQ_DAILY_SW}{Date of 50\% of Annual Cumulative Discharge}
\item{CY_Date_75P_CUMQ_DAILY_SW}{Date of 75\% of Annual Cumulative Discharge}
\item{JFM_TOTALQ_DAILY_SW}{Jan/Feb/Mar Total Discharge (m3)}
\item{AMJ_TOTALQ_DAILY_SW}{Apr/May/Jun Total Discharge (m3)}
\item{JAS_TOTALQ_DAILY_SW}{Jul/Aug/Sep Total Discharge (m3)}
\item{OND_TOTALQ_DAILY_SW}{Oct/Nov/Dec Total Discharge (m3)}
\item{JFM_YIELDMM_DAILY_SW}{Jan/Feb/Mar Water Yield (mm)}
\item{AMJ_YIELDMM_DAILY_SW}{Apr/May/Jun Water Yield (mm)}
\item{JAS_YIELDMM_DAILY_SW}{Jul/Aug/Sep Water Yield (mm)}
\item{OND_YIELDMM_DAILY_SW}{Oct/Nov/Dec Water Yield (mm)}
\item{JAN_MIN__SW}{Jan Min Discharge (cms)}
\item{FEB_MIN__SW}{Feb Min Discharge (cms)}
\item{MAR_MIN__SW}{Mar Min Discharge (cms)}
\item{APR_MIN__SW}{Apr Min Discharge (cms)}
\item{MAY_MIN__SW}{May Min Discharge (cms)}
\item{JUN_MIN__SW}{Jun Min Discharge (cms)}
\item{JUL_MIN__SW}{Jul Min Discharge (cms)}
\item{AUG_MIN__SW}{Aug Min Discharge (cms)}
\item{SEP_MIN__SW}{Sep Min Discharge (cms)}
\item{OCT_MIN__SW}{Oct Min Discharge (cms)}
\item{NOV_MIN__SW}{Nov Min Discharge (cms)}
\item{DEC_MIN__SW}{Dec Min Discharge (cms)}
\item{JAN_MAX__SW}{Jan Max Discharge (cms)}
\item{FEB_MAX__SW}{Feb Max Discharge (cms)}
\item{MAR_MAX__SW}{Mar Max Discharge (cms)}
\item{APR_MAX__SW}{Apr Max Discharge (cms)}
\item{MAY_MAX__SW}{May Max Discharge (cms)}
\item{JUN_MAX__SW}{Jun Max Discharge (cms)}
\item{JUL_MAX__SW}{Jul Max Discharge (cms)}
\item{AUG_MAX__SW}{Aug Max Discharge (cms)}
\item{SEP_MAX__SW}{Sep Max Discharge (cms)}
\item{OCT_MAX__SW}{Oct Max Discharge (cms)}
\item{NOV_MAX__SW}{Nov Max Discharge (cms)}
\item{DEC_MAX__SW}{Dec Max Discharge (cms)}
\item{JAN_MEAN__SW}{Jan Mean Discharge (cms)}
\item{FEB_MEAN__SW}{Feb Mean Discharge (cms)}
\item{MAR_MEAN__SW}{Mar Mean Discharge (cms)}
\item{APR_MEAN__SW}{Apr Mean Discharge (cms)}
\item{MAY_MEAN__SW}{May Mean Discharge (cms)}
\item{JUN_MEAN__SW}{Jun Mean Discharge (cms)}
\item{JUL_MEAN__SW}{Jul Mean Discharge (cms)}
\item{AUG_MEAN__SW}{Aug Mean Discharge (cms)}
\item{SEP_MEAN__SW}{Sep Mean Discharge (cms)}
\item{OCT_MEAN__SW}{Oct Mean Discharge (cms)}
\item{NOV_MEAN__SW}{Nov Mean Discharge (cms)}
\item{DEC_MEAN__SW}{Dec Mean Discharge (cms)}
\item{JAN_P50__SW}{Jan Median Discharge (cms)}
\item{FEB_P50__SW}{Feb Median Discharge (cms)}
\item{MAR_P50__SW}{Mar Median Discharge (cms)}
\item{APR_P50__SW}{Apr Median Discharge (cms)}
\item{MAY_P50__SW}{May Median Discharge (cms)}
\item{JUN_P50__SW}{Jun Median Discharge (cms)}
\item{JUL_P50__SW}{Jul Median Discharge (cms)}
\item{AUG_P50__SW}{Aug Median Discharge (cms)}
\item{SEP_P50__SW}{Sep Median Discharge (cms)}
\item{OCT_P50__SW}{Oct Median Discharge (cms)}
\item{NOV_P50__SW}{Nov Median Discharge (cms)}
\item{DEC_P50__SW}{Dec Median Discharge (cms)}
\item{JAN_P20__SW}{Jan 20th percentile Discharge (cms)}
\item{FEB_P20__SW}{Feb 20th percentile Discharge (cms)}
\item{MAR_P20__SW}{Mar 20th percentile Discharge (cms)}
\item{APR_P20__SW}{Apr 20th percentile Discharge (cms)}
\item{MAY_P20__SW}{May 20th percentile Discharge (cms)}
\item{JUN_P20__SW}{Jun 20th percentile Discharge (cms)}
\item{JUL_P20__SW}{Jul 20th percentile Discharge (cms)}
\item{AUG_P20__SW}{Aug 20th percentile Discharge (cms)}
\item{SEP_P20__SW}{Sep 20th percentile Discharge (cms)}
\item{OCT_P20__SW}{Oct 20th percentile Discharge (cms)}
\item{NOV_P20__SW}{Nov 20th percentile Discharge (cms)}
\item{DEC_P20__SW}{Dec 20th percentile Discharge (cms)}
\item{JAN_P10__SW}{Jan 10th percentile Discharge (cms)}
\item{FEB_P10__SW}{Feb 10th percentile Discharge (cms)}
\item{MAR_P10__SW}{Mar 10th percentile Discharge (cms)}
\item{APR_P10__SW}{Apr 10th percentile Discharge (cms)}
\item{MAY_P10__SW}{May 10th percentile Discharge (cms)}
\item{JUN_P10__SW}{Jun 10th percentile Discharge (cms)}
\item{JUL_P10__SW}{Jul 10th percentile Discharge (cms)}
\item{AUG_P10__SW}{Aug 10th percentile Discharge (cms)}
\item{SEP_P10__SW}{Sep 10th percentile Discharge (cms)}
\item{OCT_P10__SW}{Oct 10th percentile Discharge (cms)}
\item{NOV_P10__SW}{Nov 10th percentile Discharge (cms)}
\item{DEC_P10__SW}{Dec 10th percentile Discharge (cms)}
\item{WY_MIN_01Day_SW}{Water Year Min Discharge (cms)}
\item{WY_MINDOY_01Day_SW}{Date of Water Year Min Discharge (cms)}
\item{WY_MIN_03Day_SW}{Min of Water Year 3-day avg (cms)}
\item{WY_MINDOY_03Day_SW}{Date of Min Water Year 3-day avg}
\item{WY_MIN_07Day_SW}{Min Water Year 7-day avg (cms)}
\item{WY_MINDOY_07Day_SW}{Date of Min Water Year 7-day avg}
\item{WY_MIN_30Day_SW}{Min Water Year 30-day avg (cms)}
\item{WY_MINDOY_30Day_SW}{Date of Min Water Year 30-day avg}
\item{WY_MIN__SW}{Water Year Min Discharge (cms)}
\item{WY_MAX__SW}{Water Year Max Discharge (cms)}
\item{WY_MEAN__SW}{Water Year Mean Discharge (cms)}
\item{WY_TOTALQ__SW}{Water Year Total Discharge (m3)}
\item{WY_YIELDMM__SW}{Water Year Water Yield (mm)}
\item{WY_CUMQ__SW}{Cumulative Sum of Water Year Daily Q (cms)}
\item{WY_Date_25P_CUMQ__SW}{Date of 25\% of Water Year Cumulative Discharge}
\item{WY_Date_50P_CUMQ__SW}{Date of 50\% of Water Year Cumulative Discharge}
\item{WY_Date_75P_CUMQ__SW}{Date of 75\% of Water Year Cumulative Discharge}
\item{ONDJFM_TOTALQ__SW}{Oct to Mar Total Discharge (m3)}
\item{AMJJAS_TOTALQ__SW}{Apr to Sep Total Discharge (m3)}
\item{ONDJFM_YIELDMM__SW}{Oct to Mar Water Yield (mm)}
\item{AMJJAS_YIELDMM__SW}{Apr to Sep Water Yield (mm)}
\item{CY_N_BELOW_25}{Number of Days Below 25th Daily Percentile}
\item{CY_N_ABOVE_75}{Number of Days Above 75th Daily Percentile}
\item{CY_N_OUTSIDE_25_75}{Number of Days Outside 25th/75th Daily Percentile}
}
} % End of descriptions
