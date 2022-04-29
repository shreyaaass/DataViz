library(reshape2)
library(ggplot2)
library("ggcorrplot")
library('psych')
library('dplyr')
data<-read.csv("employment_data.csv")
country_list=c('Afghanistan', 'Angola', 'Albania', 'United Arab Emirates',
       'Argentina', 'Armenia', 'Australia', 'Austria', 'Azerbaijan',
       'Burundi', 'Belgium', 'Benin', 'Burkina Faso', 'Bangladesh',
       'Bulgaria', 'Bahrain', 'Bahamas', 'Bosnia and Herzegovina',
       'Belarus', 'Belize', 'Bolivia', 'Brazil', 'Barbados',
       'Brunei Darussalam', 'Bhutan', 'Botswana',
       'Central African Republic', 'Canada', 'Channel Islands',
       'Switzerland', 'Chile', 'China', "C??te d'Ivoire", 'Cameroon',
       'Congo, Democratic Republic of the', 'Congo', 'Colombia',
       'Comoros', 'Cape Verde', 'Costa Rica', 'Cuba', 'Cyprus', 'Czechia',
       'Germany', 'Djibouti', 'Denmark', 'Dominican Republic', 'Algeria',
       'Ecuador', 'Egypt', 'Eritrea', 'Western Sahara', 'Spain',
       'Estonia', 'Ethiopia', 'Finland', 'Fiji', 'France', 'Gabon',
       'United Kingdom', 'Georgia', 'Ghana', 'Guinea', 'Gambia',
       'Guinea-Bissau', 'Equatorial Guinea', 'Greece', 'Guatemala',
       'Guam', 'Guyana', 'Hong Kong, China', 'Honduras', 'Croatia',
       'Haiti', 'Hungary', 'Indonesia', 'India', 'Ireland',
       'Iran, Islamic Republic of', 'Iraq', 'Iceland', 'Israel', 'Italy',
       'Jamaica', 'Jordan', 'Japan', 'Kazakhstan', 'Kenya', 'Kyrgyzstan',
       'Cambodia', 'Korea, Republic of', 'Kuwait',
       "Lao People's Democratic Republic", 'Lebanon', 'Liberia', 'Libya',
       'Saint Lucia', 'Sri Lanka', 'Lesotho', 'Lithuania', 'Luxembourg',
       'Latvia', 'Macau, China', 'Morocco', 'Moldova, Republic of',
       'Madagascar', 'Maldives', 'Mexico', 'North Macedonia', 'Mali',
       'Malta', 'Myanmar', 'Montenegro', 'Mongolia', 'Mozambique',
       'Mauritania', 'Mauritius', 'Malawi', 'Malaysia', 'Namibia',
       'New Caledonia', 'Niger', 'Nigeria', 'Nicaragua', 'Netherlands',
       'Norway', 'Nepal', 'New Zealand', 'Oman', 'Pakistan', 'Panama',
       'Peru', 'Philippines', 'Papua New Guinea', 'Poland', 'Puerto Rico',
       "Korea, Democratic People's Republic of", 'Portugal', 'Paraguay',
       'Occupied Palestinian Territory', 'French Polynesia', 'Qatar',
       'Romania', 'Russian Federation', 'Rwanda', 'Saudi Arabia', 'Sudan',
       'Senegal', 'Singapore', 'Solomon Islands', 'Sierra Leone',
       'El Salvador', 'Somalia', 'Serbia', 'South Sudan',
       'Sao Tome and Principe', 'Suriname', 'Slovakia', 'Slovenia',
       'Sweden', 'Eswatini', 'Syrian Arab Republic', 'Chad', 'Togo',
       'Thailand', 'Tajikistan', 'Turkmenistan', 'Timor-Leste', 'Tonga',
       'Trinidad and Tobago', 'Tunisia', 'Turkey', 'Taiwan, China',
       'Tanzania, United Republic of', 'Uganda', 'Ukraine', 'Uruguay',
       'United States', 'Uzbekistan', 'Saint Vincent and the Grenadines',
       'Venezuela, Bolivarian Republic of',
       'United States Virgin Islands', 'Viet Nam', 'Vanuatu', 'Samoa', 'MENA', 'CARICOM', 'Yemen', 'South Africa',
       'Zambia', 'Zimbabwe')
region_list<-c('Northern Africa', 'Sub-Saharan Africa', 'Central Africa',
       'Eastern Africa', 'Southern Africa', 'Western Africa', 'Americas',
       'Latin America and the Caribbean', 'Caribbean',
       'Central America', 'South America', 'Northern America',
       'Arab States',
       'Asia and the Pacific', 'Eastern Asia', 'South-Eastern Asia and the Pacific',
       'South-Eastern Asia', 'Pacific Islands', 'Southern Asia', 'Europe and Central Asia',
       'European Union 28', 'G20',
       'ASEAN', 'BRICS', 'World excluding BRICS', 'G7','Arab League',
       'European Union 27')
country_data<-subset(data, country %in% country_list)
region_data<-subset(data,country %in% region_list)
country_mat<-country_data[,c(2,3,4,5,6,7,8,9)]
region_mat<-region_data[,c(2,3,4,5,6,7,8,9)]
country_cor_mat<-cor(country_mat)
region_cor_mat<-cor(region_mat)
ggcorrplot(country_cor_mat)
ggcorrplot(region_cor_mat)
fieldsup=c("ttl weekly hrs worked (k)","%age of working hrs lost","% hrs lost 40hrs/week","% hrs lost 48hrs/week","labour dependency ratio","employed female 25+ 2019","employed male 25+ 2019","ratio of weekly hrs worked by popultn age <15 and >64")
datacopy=country_cor_mat
datacopy2=region_cor_mat
rownames(datacopy)=fieldsup
rownames(datacopy2)=fieldsup
barplot(sort(datacopy[,1]),col="BLUE")
barplot(sort(datacopy2[,1]),col="RED")
pairs(country_mat,col="green",pch=19)
pairs(region_mat,col="green",pch=19)
country_work_hour=tail(arrange(country_data,total_weekly_hours_worked.estimates_in_thousands.),20)
country_hourloss=tail(arrange(country_data,percentage_of_working_hrs_lost),20)
region_work_hour=tail(arrange(region_data,total_weekly_hours_worked.estimates_in_thousands.),20)
region_hourloss=tail(arrange(region_data,percentage_of_working_hrs_lost),20)
barplot(region_hourloss$percentage_of_working_hrs_lost,names.arg =region_hourloss$country,main="Top Working Hours Lost Regions",col = "red",horiz = TRUE,las=1)
barplot(country_hourloss$percentage_of_working_hrs_lost,names.arg =country_work_hour$country,main="Top Working Hours Lost Countries",col = "red",horiz = TRUE,las=1)

