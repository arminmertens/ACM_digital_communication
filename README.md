# Mertens, A., Pradel, F. Rozyjumayeva, A. and Wäckerle, J. (2019) As the tweet, so the reply? Gender bias in digital communication with politicians
<div style="text-align: justify">
Replication materials for our paper:</div></br>

*Armin Mertens, Franziska Pradel, Ayjeren Rozyjumayeva, and Jens Wäckerle. 2019. As the tweet, so the reply? Gender bias in digital communica-tion with politicians. In 11th ACM Conference on Web Science (WebSci ’19), June 30–July 3, 2019, Boston, MA, USA*

Abstract
---
This study investigates gender bias in political interactions on digital platforms by considering how politicians present themselves on Twitter and how they are approached by others. Incorporating social identity theory, we use dictionary analyses to detect biases in individual tweets connected to the German federal elections in 2017. Besides sentiment analysis, we introduce a new measure of personal- vs. job-related content in text data, that is validated with structural topic models. Our results indicate that politicians' communication on Twitter is driven by party identity rather than gender. However, we find systematic gender differences in tweets directed at politicians: female politicians are significantly more likely to be reduced to their gender rather than to their profession compared to male politicians.

Replication
---
<div style="text-align: justify">
Since the data we used in our analysis contains private information of twitter users, we are not able to share it in this repository. Nevertheless, the tweets that were scraped during the national elections in Germany (2017) by GESIS and the tweet IDs can be downloaded using the following link: https://dbk.gesis.org/dbksearch/sdesc2.asp?no=6926). Hence, our analysis can be replicated when scraping the tweets using our provided scripts and running our analysis scripts afterwards.

For reproducibility of our code, however, we added an anonymized dataset containing all tweets used in the analysis. Here, we removed the text of the tweets, the names of the accounts and other variables that could potentially be used to identify the twitter user. Hence, the data only contains the variables necessary for the analysis (i.e. party of the politician, sentiment and personal-job-related dictionary analysis scores, etc.).
</div>
