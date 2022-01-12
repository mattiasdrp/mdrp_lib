type t =
  | Afghanistan
  | Albania
  | Algeria
  | Andorra
  | Angola
  | Antigua_and_Barbuda
  | Argentina
  | Armenia
  | Australia
  | Austria
  | Azerbaijan
  | The_Bahamas
  | Bahrain
  | Bangladesh
  | Barbados
  | Belarus
  | Belgium
  | Belize
  | Benin
  | Bhutan
  | Bolivia
  | Bosnia_and_Herzegovina
  | Botswana
  | Brazil
  | Brunei
  | Bulgaria
  | Burkina_Faso
  | Burundi
  | Cambodia
  | Cameroon
  | Canada
  | Cape_Verde
  | Central_African_Republic
  | Chad
  | Chile
  | China
  | Colombia
  | Comoros
  | Republic_of_the_Congo
  | Democratic_Republic_of_the_Congo
  | Costa_Rica
  | Cote_d'Ivoire
  | Croatia
  | Cuba
  | Cyprus
  | Czech_Republic
  | Denmark
  | Djibouti
  | Dominica
  | Dominican_Republic
  | East_Timor
  | Ecuador
  | Egypt
  | El_Salvador
  | Equatorial_Guinea
  | Eritrea
  | Estonia
  | Ethiopia
  | Fiji
  | Finland
  | France
  | Gabon
  | The_Gambia
  | Georgia
  | Germany
  | Ghana
  | Greece
  | Grenada
  | Guatemala
  | Guinea
  | Guinea_Bissau
  | Guyana
  | Haiti
  | Honduras
  | Hungary
  | Iceland
  | India
  | Indonesia
  | Iran
  | Iraq
  | Ireland
  | Israel
  | Italy
  | Jamaica
  | Japan
  | Jordan
  | Kazakhstan
  | Kenya
  | Kiribati
  | North_Korea
  | South_Korea
  | Kosovo
  | Kuwait
  | Kyrgyzstan
  | Laos
  | Latvia
  | Lebanon
  | Lesotho
  | Liberia
  | Libya
  | Liechtenstein
  | Lithuania
  | Luxembourg
  | Macedonia
  | Madagascar
  | Malawi
  | Malaysia
  | Maldives
  | Mali
  | Malta
  | Marshall_Islands
  | Mauritania
  | Mauritius
  | Mexico
  | Federated_States_of_Micronesia
  | Moldova
  | Monaco
  | Mongolia
  | Montenegro
  | Morocco
  | Mozambique
  | Myanmar
  | Namibia
  | Nauru
  | Nepal
  | Netherlands
  | New_Zealand
  | Nicaragua
  | Niger
  | Nigeria
  | Norway
  | Oman
  | Pakistan
  | Palau
  | Panama
  | Papua_New_Guinea
  | Paraguay
  | Peru
  | Philippines
  | Poland
  | Portugal
  | Qatar
  | Romania
  | Russia
  | Rwanda
  | Saint_Kitts_and_Nevis
  | Saint_Lucia
  | Saint_Vincent_and_the_Grenadines
  | Samoa
  | San_Marino
  | Sao_Tome_and_Principe
  | Saudi_Arabia
  | Senegal
  | Serbia
  | Seychelles
  | Sierra_Leone
  | Singapore
  | Slovakia
  | Slovenia
  | Solomon_Islands
  | Somalia
  | South_Africa
  | South_Sudan
  | Spain
  | Sri_Lanka
  | Sudan
  | Suriname
  | Swaziland
  | Sweden
  | Switzerland
  | Syria
  | Taiwan
  | Tajikistan
  | Tanzania
  | Thailand
  | Togo
  | Tonga
  | Trinidad_and_Tobago
  | Tunisia
  | Turkey
  | Turkmenistan
  | Tuvalu
  | Uganda
  | Ukraine
  | United_Arab_Emirates
  | United_Kingdom
  | United_States_of_America
  | Uruguay
  | Uzbekistan
  | Vanuatu
  | Vatican_City
  | Venezuela
  | Vietnam
  | Yemen
  | Zambia
  | Zimbabwe
  | Unrecognised of string

let of_string = function
  | "Afghanistan" -> Afghanistan
  | "Albania" -> Albania
  | "Algeria" -> Algeria
  | "Andorra" -> Andorra
  | "Angola" -> Angola
  | "Antigua and Barbuda" -> Antigua_and_Barbuda
  | "Argentina" -> Argentina
  | "Armenia" -> Armenia
  | "Australia" -> Australia
  | "Austria" -> Austria
  | "Azerbaijan" -> Azerbaijan
  | "The Bahamas" -> The_Bahamas
  | "Bahrain" -> Bahrain
  | "Bangladesh" -> Bangladesh
  | "Barbados" -> Barbados
  | "Belarus" -> Belarus
  | "Belgium" -> Belgium
  | "Belize" -> Belize
  | "Benin" -> Benin
  | "Bhutan" -> Bhutan
  | "Bolivia" -> Bolivia
  | "Bosnia and Herzegovina" -> Bosnia_and_Herzegovina
  | "Botswana" -> Botswana
  | "Brazil" -> Brazil
  | "Brunei" -> Brunei
  | "Bulgaria" -> Bulgaria
  | "Burkina Faso" -> Burkina_Faso
  | "Burundi" -> Burundi
  | "Cambodia" -> Cambodia
  | "Cameroon" -> Cameroon
  | "Canada" -> Canada
  | "Cape Verde" -> Cape_Verde
  | "Central African Republic" -> Central_African_Republic
  | "Chad" -> Chad
  | "Chile" -> Chile
  | "China" -> China
  | "Colombia" -> Colombia
  | "Comoros" -> Comoros
  | "Republic of the Congo" -> Republic_of_the_Congo
  | "Democratic Republic of the Congo" -> Democratic_Republic_of_the_Congo
  | "Costa Rica" -> Costa_Rica
  | "Cote d'Ivoire" -> Cote_d'Ivoire
  | "Croatia" -> Croatia
  | "Cuba" -> Cuba
  | "Cyprus" -> Cyprus
  | "Czech Republic" -> Czech_Republic
  | "Denmark" -> Denmark
  | "Djibouti" -> Djibouti
  | "Dominica" -> Dominica
  | "Dominican Republic" -> Dominican_Republic
  | "East Timor" -> East_Timor
  | "Ecuador" -> Ecuador
  | "Egypt" -> Egypt
  | "El Salvador" -> El_Salvador
  | "Equatorial Guinea" -> Equatorial_Guinea
  | "Eritrea" -> Eritrea
  | "Estonia" -> Estonia
  | "Ethiopia" -> Ethiopia
  | "Fiji" -> Fiji
  | "Finland" -> Finland
  | "France" -> France
  | "Gabon" -> Gabon
  | "The Gambia" -> The_Gambia
  | "Georgia" -> Georgia
  | "Germany" -> Germany
  | "Ghana" -> Ghana
  | "Greece" -> Greece
  | "Grenada" -> Grenada
  | "Guatemala" -> Guatemala
  | "Guinea" -> Guinea
  | "Guinea Bissau" | "Guinea-Bissau" -> Guinea_Bissau
  | "Guyana" -> Guyana
  | "Haiti" -> Haiti
  | "Honduras" -> Honduras
  | "Hungary" -> Hungary
  | "Iceland" -> Iceland
  | "India" -> India
  | "Indonesia" -> Indonesia
  | "Iran" -> Iran
  | "Iraq" -> Iraq
  | "Ireland" -> Ireland
  | "Israel" -> Israel
  | "Italy" -> Italy
  | "Jamaica" -> Jamaica
  | "Japan" -> Japan
  | "Jordan" -> Jordan
  | "Kazakhstan" -> Kazakhstan
  | "Kenya" -> Kenya
  | "Kiribati" -> Kiribati
  | "North Korea" -> North_Korea
  | "South Korea" -> South_Korea
  | "Kosovo" -> Kosovo
  | "Kuwait" -> Kuwait
  | "Kyrgyzstan" -> Kyrgyzstan
  | "Laos" -> Laos
  | "Latvia" -> Latvia
  | "Lebanon" -> Lebanon
  | "Lesotho" -> Lesotho
  | "Liberia" -> Liberia
  | "Libya" -> Libya
  | "Liechtenstein" -> Liechtenstein
  | "Lithuania" -> Lithuania
  | "Luxembourg" -> Luxembourg
  | "Macedonia" -> Macedonia
  | "Madagascar" -> Madagascar
  | "Malawi" -> Malawi
  | "Malaysia" -> Malaysia
  | "Maldives" -> Maldives
  | "Mali" -> Mali
  | "Malta" -> Malta
  | "Marshall Islands" -> Marshall_Islands
  | "Mauritania" -> Mauritania
  | "Mauritius" -> Mauritius
  | "Mexico" -> Mexico
  | "Federated States of Micronesia" -> Federated_States_of_Micronesia
  | "Moldova" -> Moldova
  | "Monaco" -> Monaco
  | "Mongolia" -> Mongolia
  | "Montenegro" -> Montenegro
  | "Morocco" -> Morocco
  | "Mozambique" -> Mozambique
  | "Myanmar" -> Myanmar
  | "Namibia" -> Namibia
  | "Nauru" -> Nauru
  | "Nepal" -> Nepal
  | "Netherlands" -> Netherlands
  | "New Zealand" -> New_Zealand
  | "Nicaragua" -> Nicaragua
  | "Niger" -> Niger
  | "Nigeria" -> Nigeria
  | "Norway" -> Norway
  | "Oman" -> Oman
  | "Pakistan" -> Pakistan
  | "Palau" -> Palau
  | "Panama" -> Panama
  | "Papua New Guinea" -> Papua_New_Guinea
  | "Paraguay" -> Paraguay
  | "Peru" -> Peru
  | "Philippines" -> Philippines
  | "Poland" -> Poland
  | "Portugal" -> Portugal
  | "Qatar" -> Qatar
  | "Romania" -> Romania
  | "Russia" -> Russia
  | "Rwanda" -> Rwanda
  | "Saint Kitts and Nevis" -> Saint_Kitts_and_Nevis
  | "Saint Lucia" -> Saint_Lucia
  | "Saint Vincent and the Grenadines" -> Saint_Vincent_and_the_Grenadines
  | "Samoa" -> Samoa
  | "San Marino" -> San_Marino
  | "Sao Tome and Principe" -> Sao_Tome_and_Principe
  | "Saudi Arabia" -> Saudi_Arabia
  | "Senegal" -> Senegal
  | "Serbia" -> Serbia
  | "Seychelles" -> Seychelles
  | "Sierra Leone" -> Sierra_Leone
  | "Singapore" -> Singapore
  | "Slovakia" -> Slovakia
  | "Slovenia" -> Slovenia
  | "Solomon Islands" -> Solomon_Islands
  | "Somalia" -> Somalia
  | "South Africa" -> South_Africa
  | "South Sudan" -> South_Sudan
  | "Spain" -> Spain
  | "Sri Lanka" -> Sri_Lanka
  | "Sudan" -> Sudan
  | "Suriname" -> Suriname
  | "Swaziland" -> Swaziland
  | "Sweden" -> Sweden
  | "Switzerland" -> Switzerland
  | "Syria" -> Syria
  | "Taiwan" -> Taiwan
  | "Tajikistan" -> Tajikistan
  | "Tanzania" -> Tanzania
  | "Thailand" -> Thailand
  | "Togo" -> Togo
  | "Tonga" -> Tonga
  | "Trinidad and Tobago" -> Trinidad_and_Tobago
  | "Tunisia" -> Tunisia
  | "Turkey" -> Turkey
  | "Turkmenistan" -> Turkmenistan
  | "Tuvalu" -> Tuvalu
  | "Uganda" -> Uganda
  | "Ukraine" -> Ukraine
  | "United Arab Emirates" -> United_Arab_Emirates
  | "United Kingdom" -> United_Kingdom
  | "United States of America" -> United_States_of_America
  | "Uruguay" -> Uruguay
  | "Uzbekistan" -> Uzbekistan
  | "Vanuatu" -> Vanuatu
  | "Vatican City" -> Vatican_City
  | "Venezuela" -> Venezuela
  | "Vietnam" -> Vietnam
  | "Yemen" -> Yemen
  | "Zambia" -> Zambia
  | "Zimbabwe" -> Zimbabwe
  | s ->
      Format.eprintf "Not a country: %s" s;
      Unrecognised s

let pp ppf t =
  let s =
    match t with
    | Afghanistan -> "Afghanistan"
    | Albania -> "Albania"
    | Algeria -> "Algeria"
    | Andorra -> "Andorra"
    | Angola -> "Angola"
    | Antigua_and_Barbuda -> "Antigua and Barbuda"
    | Argentina -> "Argentina"
    | Armenia -> "Armenia"
    | Australia -> "Australia"
    | Austria -> "Austria"
    | Azerbaijan -> "Azerbaijan"
    | The_Bahamas -> "The Bahamas"
    | Bahrain -> "Bahrain"
    | Bangladesh -> "Bangladesh"
    | Barbados -> "Barbados"
    | Belarus -> "Belarus"
    | Belgium -> "Belgium"
    | Belize -> "Belize"
    | Benin -> "Benin"
    | Bhutan -> "Bhutan"
    | Bolivia -> "Bolivia"
    | Bosnia_and_Herzegovina -> "Bosnia and Herzegovina"
    | Botswana -> "Botswana"
    | Brazil -> "Brazil"
    | Brunei -> "Brunei"
    | Bulgaria -> "Bulgaria"
    | Burkina_Faso -> "Burkina Faso"
    | Burundi -> "Burundi"
    | Cambodia -> "Cambodia"
    | Cameroon -> "Cameroon"
    | Canada -> "Canada"
    | Cape_Verde -> "Cape Verde"
    | Central_African_Republic -> "Central African Republic"
    | Chad -> "Chad"
    | Chile -> "Chile"
    | China -> "China"
    | Colombia -> "Colombia"
    | Comoros -> "Comoros"
    | Republic_of_the_Congo -> "Republic of the Congo"
    | Democratic_Republic_of_the_Congo -> "Democratic Republic of the Congo"
    | Costa_Rica -> "Costa Rica"
    | Cote_d'Ivoire -> "Cote d'Ivoire"
    | Croatia -> "Croatia"
    | Cuba -> "Cuba"
    | Cyprus -> "Cyprus"
    | Czech_Republic -> "Czech Republic"
    | Denmark -> "Denmark"
    | Djibouti -> "Djibouti"
    | Dominica -> "Dominica"
    | Dominican_Republic -> "Dominican Republic"
    | East_Timor -> "East Timor"
    | Ecuador -> "Ecuador"
    | Egypt -> "Egypt"
    | El_Salvador -> "El Salvador"
    | Equatorial_Guinea -> "Equatorial Guinea"
    | Eritrea -> "Eritrea"
    | Estonia -> "Estonia"
    | Ethiopia -> "Ethiopia"
    | Fiji -> "Fiji"
    | Finland -> "Finland"
    | France -> "France"
    | Gabon -> "Gabon"
    | The_Gambia -> "The Gambia"
    | Georgia -> "Georgia"
    | Germany -> "Germany"
    | Ghana -> "Ghana"
    | Greece -> "Greece"
    | Grenada -> "Grenada"
    | Guatemala -> "Guatemala"
    | Guinea -> "Guinea"
    | Guinea_Bissau -> "Guinea Bissau"
    | Guyana -> "Guyana"
    | Haiti -> "Haiti"
    | Honduras -> "Honduras"
    | Hungary -> "Hungary"
    | Iceland -> "Iceland"
    | India -> "India"
    | Indonesia -> "Indonesia"
    | Iran -> "Iran"
    | Iraq -> "Iraq"
    | Ireland -> "Ireland"
    | Israel -> "Israel"
    | Italy -> "Italy"
    | Jamaica -> "Jamaica"
    | Japan -> "Japan"
    | Jordan -> "Jordan"
    | Kazakhstan -> "Kazakhstan"
    | Kenya -> "Kenya"
    | Kiribati -> "Kiribati"
    | North_Korea -> "North Korea"
    | South_Korea -> "South Korea"
    | Kosovo -> "Kosovo"
    | Kuwait -> "Kuwait"
    | Kyrgyzstan -> "Kyrgyzstan"
    | Laos -> "Laos"
    | Latvia -> "Latvia"
    | Lebanon -> "Lebanon"
    | Lesotho -> "Lesotho"
    | Liberia -> "Liberia"
    | Libya -> "Libya"
    | Liechtenstein -> "Liechtenstein"
    | Lithuania -> "Lithuania"
    | Luxembourg -> "Luxembourg"
    | Macedonia -> "Macedonia"
    | Madagascar -> "Madagascar"
    | Malawi -> "Malawi"
    | Malaysia -> "Malaysia"
    | Maldives -> "Maldives"
    | Mali -> "Mali"
    | Malta -> "Malta"
    | Marshall_Islands -> "Marshall Islands"
    | Mauritania -> "Mauritania"
    | Mauritius -> "Mauritius"
    | Mexico -> "Mexico"
    | Federated_States_of_Micronesia -> "Federated States of Micronesia"
    | Moldova -> "Moldova"
    | Monaco -> "Monaco"
    | Mongolia -> "Mongolia"
    | Montenegro -> "Montenegro"
    | Morocco -> "Morocco"
    | Mozambique -> "Mozambique"
    | Myanmar -> "Myanmar"
    | Namibia -> "Namibia"
    | Nauru -> "Nauru"
    | Nepal -> "Nepal"
    | Netherlands -> "Netherlands"
    | New_Zealand -> "New Zealand"
    | Nicaragua -> "Nicaragua"
    | Niger -> "Niger"
    | Nigeria -> "Nigeria"
    | Norway -> "Norway"
    | Oman -> "Oman"
    | Pakistan -> "Pakistan"
    | Palau -> "Palau"
    | Panama -> "Panama"
    | Papua_New_Guinea -> "Papua New Guinea"
    | Paraguay -> "Paraguay"
    | Peru -> "Peru"
    | Philippines -> "Philippines"
    | Poland -> "Poland"
    | Portugal -> "Portugal"
    | Qatar -> "Qatar"
    | Romania -> "Romania"
    | Russia -> "Russia"
    | Rwanda -> "Rwanda"
    | Saint_Kitts_and_Nevis -> "Saint Kitts and Nevis"
    | Saint_Lucia -> "Saint Lucia"
    | Saint_Vincent_and_the_Grenadines -> "Saint Vincent and the Grenadines"
    | Samoa -> "Samoa"
    | San_Marino -> "San Marino"
    | Sao_Tome_and_Principe -> "Sao Tome and Principe"
    | Saudi_Arabia -> "Saudi Arabia"
    | Senegal -> "Senegal"
    | Serbia -> "Serbia"
    | Seychelles -> "Seychelles"
    | Sierra_Leone -> "Sierra Leone"
    | Singapore -> "Singapore"
    | Slovakia -> "Slovakia"
    | Slovenia -> "Slovenia"
    | Solomon_Islands -> "Solomon Islands"
    | Somalia -> "Somalia"
    | South_Africa -> "South Africa"
    | South_Sudan -> "South Sudan"
    | Spain -> "Spain"
    | Sri_Lanka -> "Sri Lanka"
    | Sudan -> "Sudan"
    | Suriname -> "Suriname"
    | Swaziland -> "Swaziland"
    | Sweden -> "Sweden"
    | Switzerland -> "Switzerland"
    | Syria -> "Syria"
    | Taiwan -> "Taiwan"
    | Tajikistan -> "Tajikistan"
    | Tanzania -> "Tanzania"
    | Thailand -> "Thailand"
    | Togo -> "Togo"
    | Tonga -> "Tonga"
    | Trinidad_and_Tobago -> "Trinidad and Tobago"
    | Tunisia -> "Tunisia"
    | Turkey -> "Turkey"
    | Turkmenistan -> "Turkmenistan"
    | Tuvalu -> "Tuvalu"
    | Uganda -> "Uganda"
    | Ukraine -> "Ukraine"
    | United_Arab_Emirates -> "United Arab Emirates"
    | United_Kingdom -> "United Kingdom"
    | United_States_of_America -> "United States of America"
    | Uruguay -> "Uruguay"
    | Uzbekistan -> "Uzbekistan"
    | Vanuatu -> "Vanuatu"
    | Vatican_City -> "Vatican City"
    | Venezuela -> "Venezuela"
    | Vietnam -> "Vietnam"
    | Yemen -> "Yemen"
    | Zambia -> "Zambia"
    | Zimbabwe -> "Zimbabwe"
    | Unrecognised s -> s
  in
  Format.fprintf ppf "%s" s
