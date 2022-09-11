/*=============================================================================
Analysis of livelihood and disability data
Benjamin Akatabanuse
Rolando Gonzales
version: September 11, 2022
=============================================================================*/

* ------------------------------------------------------------------------------
* Import data
	use "C:\2022\08_august\Ben paper\Ben_data.dta", clear
	*use "/Users/benjaminakatabanuse/Desktop/Working Papers/Savings Groups Paper/Results/3rd Results/Ben_data.dta", clear
* ------------------------------------------------------------------------------	
* Disability

	rename disability disability_original
	
	* Disability
		gen 	disability = 0
		replace disability = 1 if Doyouhavedifficultyseeinge == 42 |Doyouhavedifficultyseeinge == 43 | Doyouhavedifficultyseeinge == 44 | ///
								Doyouhavedifficultyhearing == 46 |Doyouhavedifficultyhearing == 47 | Doyouhavedifficultyhearing == 48 | ///
								Doyouhavedifficultywalkingo== 50 |Doyouhavedifficultywalkingo== 51 | Doyouhavedifficultywalkingo== 52 | ///  
								Doyouhavedifficultyrememberi==54 |Doyouhavedifficultyrememberi==55 | Doyouhavedifficultyrememberi==56 | ///
								Doyouhavedifficultywithself ==58 | Doyouhavedifficultywithself ==59 | Doyouhavedifficultywithself ==60 | ///
								Usingyourusuallanguagecusto ==62 |Usingyourusuallanguagecusto ==63 | Usingyourusuallanguagecusto ==64

	* Severe disability
		gen 	sevdiasb = 0
		replace sevdiasb = 1 if Doyouhavedifficultyseeinge == 43 | Doyouhavedifficultyseeinge == 44 | ///
								Doyouhavedifficultyhearing == 47 | Doyouhavedifficultyhearing == 48 | ///
								Doyouhavedifficultywalkingo== 51 | Doyouhavedifficultywalkingo== 52 | ///  
								Doyouhavedifficultyrememberi==55 | Doyouhavedifficultyrememberi==56 | ///
								Doyouhavedifficultywithself ==59 | Doyouhavedifficultywithself ==60 | ///
								Usingyourusuallanguagecusto ==63 | Usingyourusuallanguagecusto ==64
								

	/* 
		Difficulty seeing:
			41= No, no difficulty
			43= Yes, a lot of difficulty, 44= Cannot do it at all
			
		Difficulty hearing, even if using a hearing aid:
			47= Yes, a lot of difficulty, 48= Cannot do it at all
			
		Difficulty walking or climbing steps:
			51= Yes, a lot of difficulty, 52= Cannot do it at all

		Difficulty remembering or concentrating:
			55= Yes, a lot of difficulty, 56= Cannot do it at all 

		Difficulty with self care, such as washing all over or dressing
			59= Yes, a lot of difficulty, 60= Cannot do it at all
			
		Difficulty communicating
			63= Yes, a lot of difficulty, 64= Cannot do it at all
	*/
			lab define sev_lab 	1 "severe disability" 	0 "no severe disability"
			lab define dis_lab 	1 "disability" 			0 "no disability"
			lab values sevdiasb sev_lab
			lab values disability dis_lab
			lab var sevdiasb "severe disability"
			lab var disability "disability"
			ta sevdiasb disability
			ta disability
			
* ------------------------------------------------------------------------------	
* Explanatory variables:
	* Education
		clonevar education = Highestlevelofeducationcompl
		lab define edulab 33 "none" 34 "primary" 35 "secondary" 36 "vocational/technical" 37 "tertiary" 38 "university"
		lab values education edulab
			ta education, m
			
	* Gender 
		recode gender (1=0) (0=1), generate(sex)
			lab define sexlab 	1 "female" 0 "male"
			lab values sex sexlab	
					ta gender sex, m		
					
	* Marital status
		ta Maritalstatus
		lab var Maritalstatus "marital status"
			lab define marstatlab 29 "single" 30 "married" 31 "separated-divorced" 32 "widowed" 
			lab values Maritalstatus marstatlab	
					ta Maritalstatus, m
	
	* first cycle treatment
	*lab define trt_lab 	1 "more than 1 cycle" 0 "first cycle"
	*lab values Treatment trt_lab
	*ta Treatment, m
	
	* group cycles:
	clonevar groupcycles = GroupCycleNo
    lab define grclab 17 "cycle 1" 18 "cycle 2" 19 "cycle 3" 20 "cycle 4" 21 "cycle 5"
	lab values groupcycles grclab
	ta groupcycles, m

	* Social security
	ta Doyoubenefitfromsocialsecur, m
	clonevar socialsecurity = Doyoubenefitfromsocialsecur
		lab define soclab 67 "yes" 68 "no"
		lab values socialsecurity soclab
			ta socialsecurity, m	
			
	* Working situation
	ta Whatisyourcurrentworkingsit, m
		/* Coding:
			 82= Not working and not looking for paid work 
			 83= Not working, but looking for paid work 
			 84= Working full or part-time for wages (formal sector) 
			 85= Working full or part-time for wages (informal sector) 
			 86= Working in family business (paid in kind) 
			 87= Working in family business (paid cash) 
			 88= Self-employed (own business) 
			 89= On sick leave from work for more than three months 
			 90= Retired because of the health condition 
			 91= Retired because of the health age 
			 92= Other (specify)
		*/
		gen 	workingstatus = .
		replace workingstatus = 1 if Whatisyourcurrentworkingsit == 82 | Whatisyourcurrentworkingsit == 83
		replace workingstatus = 2 if Whatisyourcurrentworkingsit == 84 | Whatisyourcurrentworkingsit == 85 |  ///
									 Whatisyourcurrentworkingsit == 86 | Whatisyourcurrentworkingsit == 87 |  /// 
									 Whatisyourcurrentworkingsit == 87 | Whatisyourcurrentworkingsit == 88
		replace workingstatus = 3 if Whatisyourcurrentworkingsit == 90 | Whatisyourcurrentworkingsit == 91
		replace workingstatus = 4 if Whatisyourcurrentworkingsit == 89 | Whatisyourcurrentworkingsit == 92
		lab define worklab 	1 "not working" 2 "working" 3 "retired" 4 "other status"
		lab values workingstatus worklab
		lab var workingstatus "working status"
		ta workingstatus Whatisyourcurrentworkingsit, m
	
	* Economic sector:
		ta Whatareyoursourcesofincome, m
		/* Coding:
			 95= Retail 
			 96= Manufacturing 
			 97= Transport 
			 98= Agriculture & livestock 
			 99= Service 
			 100= Other (e.g. donations, church, politics, etc.)
		*/
		recode Whatareyoursourcesofincome (95=1) (96=2) (97=3) (98=4) (99=5) (100=6), generate(economicsector)
		lab define eseclab 	1 "retail" 2 "manufacturing" 3 "transport" 4 "agriculture & livestock" 5 "service" 6 "other"
			lab values economicsector eseclab
			lab var economicsector "economic sector"
			ta economicsector Whatareyoursourcesofincome, m

	* Number of income generating activities
		recode Howmanyincomegenerationactiv (103=1) (104=2) (105=3) (106=4) (107=5), generate(numIGA)
		lab var numIGA "number of IGAs"
		ta numIGA Howmanyincomegenerationactiv
	
	* Access to banking or microfinance
		ta Doyoucurrentlyhaveanaccount, m
		* 110= Yes, 111= No
		recode Doyoucurrentlyhaveanaccount (110=1) (111=0), generate(banking)
			lab define banklab 	1 "yes" 0 "no"
			lab values banking banklab
			lab var banking "account in a bank or MFI"
			ta banking Doyoucurrentlyhaveanaccount, m		
	
		* Value of savings at 
	ta Whatisthevalueofyoursaving
	/* UGX:
		112= below 35,000 
		113= 36,000 - 105,000
		114= 106,000 - 210,000
		115= 211,000 - 315,000
		116= 316,000 - 420,000 
		117= 421,000 - 525,000 
		118= 526,000 - 630,000 
		119= 631,000 - 735,000 
		120= Above 735,000
	*/
	
	* Value of loans
	ta Whatisthevalueofyourloani
	/* UGX:
		121= Below 105,000 
		122= 105,000 - 175,000 
		123= 176,000 - 350,000 
		124= 351,000 - 525,000 
		125= 526,000 - 700,000 
		126= 701,000 - 1,050,000
		127= 1,051,000 - 1,400,000 
		128= 1,401,000 - 1,750,000 
		129= Above 1,750,000
	*/
	
	* Access to social protection:
	ta Doyouknowhowtogetsocialpr, m
	*130= Yes, 131= No
	
	* Program benfits
	ta Doyoucurrentlyreceiveanyben, m
	* 132= Yes, 133= No 

	
	* Member of a self-help group
	ta AreyouamemberofaSelfHelp, m
	/*
		134= Yes
		135= No, but I would like to
		136= No, I don't want to 
		137= Don't know
	*/
	recode AreyouamemberofaSelfHelp (137=0) (134=1) (135=2) (136=2), generate(memberSHG)
		lab var memberSHG "member of a self-help group"
		lab define memlab 0 "don't know" 1 "yes" 2 "no" 
		lab values memberSHG memlab
		ta memberSHG AreyouamemberofaSelfHelp, m	
		
	* Training
	ta Haveyoubeentrainedonemploym, m
	ta Haveyoubeentrainedonfinanci, m 
	ta Haveyoubeentrainedonmarket,m
	clonevar trainemp = Haveyoubeentrainedonemploym
	clonevar trainfin = Haveyoubeentrainedonfinanci
	clonevar trainmar = Haveyoubeentrainedonmarket
	lab define trainlab 0 "no" 1 "yes" 
		lab values trainemp trainlab
		lab values trainfin trainlab
		lab values trainmar trainlab
		
* ------------------------------------------------------------------------------	
* Dependent variables	

	* Livelihood
		ta Ingeneralhowisyourliveliho, m
		recode Ingeneralhowisyourliveliho (77=1) (78=2) (79=3) (80=4) (81=5), generate(livelihood)
		lab var livelihood "HH livelihood compared to other livelihoods"
		/*
		livelihood situation compared to other people in your village
		77= Much worse 
		78= Worse 
		79= Same 
		80= Better 
		81= Much Better
		*/
		
	* Starting an income generation activity as a result of participating in the iSAVE group
		ta Didyoustartanyincomegenerat, m
		* 101= Yes, 102= No
		recode Didyoustartanyincomegenerat (101=1) (102=0), generate(IGAstarted)
		lab var IGAstarted "IGA started as a result of participating in the iSAVE group"
		lab define igalab 	1 "yes" 0 "no"
		lab values IGAstarted igalab
		ta IGAstarted Didyoustartanyincomegenerat, m		

		/*
	* Number of IGAs started as a result of participating in the iSAVE group
		ta Howmanyincomegenerationactiv, m
		*  103= 1, 104= 2, 105= 3, 106= 4, 107= More than 4
		recode Howmanyincomegenerationactiv (.=0) (103=1) (104=2) (105=3) (106=4) (107=5), generate(numIGAstarted)
		lab var numIGAstarted "number of IGAs started as a result of participating in the iSAVE group"
		lab define numigalab 0 "None" 1 "1 IGA" 2 "2 IGAs" 3 "3 IGAs" 4 "4 IGAs" 5 "4 or more IGAs"
		lab values numIGAstarted numigalab
		ta numIGAstarted Howmanyincomegenerationact, m
			*/
			
* ------------------------------------------------------------------------------
* Descriptive statistics:
	ta 	 education	sex

* ------------------------------------------------------------------------------			
* Model 1: orderded logit regression for livelihood
	ologit livelihood disability#groupcycles Age i.sex i.education i.Maritalstatus ib(4).workingstatus ib(6).economicsector i.socialsecurity i.banking i.DistrictNo, vce(cluster GroupCycleNo) baselevels
	
	ologit livelihood sevdiasb#groupcycles Age i.sex i.education i.Maritalstatus ib(4).workingstatus ib(6).economicsector i.socialsecurity i.banking i.DistrictNo, vce(cluster GroupCycleNo) baselevels

	* Same regression, with odd-ratios:
	ologit livelihood disability#groupcycles Age i.sex i.education i.Maritalstatus ib(4).workingstatus ib(6).economicsector i.socialsecurity i.banking i.DistrictNo, vce(cluster GroupCycleNo) or baselevels
	
	matrix odds = J(10,4,.)	
	matrix b  = e(b)
	matrix odds[1,1] = b[1,1..10]'		
		matrix  rownames odds = nodis_c1 nodis_c2 nodis_c3 nodis_c4 nodis_c5 ///
								dis_c1 dis_c2 dis_c3 dis_c4 dis_c5
		matrix  colnames odds = olog_dis olog_sdis logit_dis logit_sdis
	matrix list odds
	
	ologit livelihood sevdiasb#groupcycles Age i.sex i.education i.Maritalstatus ib(4).workingstatus ib(6).economicsector i.socialsecurity i.banking i.DistrictNo, vce(cluster GroupCycleNo) or baselevels
	matrix b  = e(b)
	matrix odds[1,2] = b[1,1..10]'		
	matrix list odds
		
	* Robustness analysis, with cycle fixed effects and sandwich errors:
	ologit livelihood disability#groupcycles Age i.sex i.education i.Maritalstatus ib(4).workingstatus ib(6).economicsector i.socialsecurity i.banking i.DistrictNo, vce(robust) baselevels
	
	ologit livelihood sevdiasb#groupcycles Age i.sex i.education i.Maritalstatus ib(4).workingstatus ib(6).economicsector i.socialsecurity i.banking i.DistrictNo, vce(robust) baselevels
	
	* Generalized orderded logit:
	*ssc install gologit2
	*gologit2 livelihood sevdiasb#groupcycles Age i.sex i.education i.Maritalstatus ib(4).workingstatus ib(6).economicsector i.socialsecurity i.banking i.DistrictNo, vce(cluster GroupCycleNo) autofit(0.05)
	/*
	Wald test for the null H0: parallel regression assumption holds:
           chi2(  1) =    2.46
         Prob > chi2 =    0.1169
	The final model does not violate the proportional odds/ parallel lines assumption
	*/
* ------------------------------------------------------------------------------			
* Model 2: logit regression
	
	* Logit
	logit IGAstarted disability#groupcycles Age i.sex i.education i.Maritalstatus ib(4).workingstatus ib(6).economicsector i.socialsecurity i.banking i.DistrictNo, vce(cluster GroupCycleNo) baselevels
		
	logit IGAstarted sevdiasb#groupcycles Age i.sex i.education i.Maritalstatus ib(4).workingstatus ib(6).economicsector i.socialsecurity i.banking i.DistrictNo, vce(cluster GroupCycleNo) baselevels
	
	* Same regression as log-odds
	logistic IGAstarted disability#groupcycles Age i.sex i.education i.Maritalstatus ib(4).workingstatus ib(6).economicsector i.socialsecurity i.banking i.DistrictNo, vce(cluster GroupCycleNo) or baselevels
	matrix b  = e(b)
	matrix odds[1,3] = b[1,1..10]'		
	matrix list odds
	
	logistic IGAstarted sevdiasb#groupcycles Age i.sex i.education i.Maritalstatus ib(4).workingstatus ib(6).economicsector i.socialsecurity i.banking i.DistrictNo, vce(cluster GroupCycleNo) or baselevels
	matrix b  = e(b)
	matrix odds[1,4] = b[1,1..10]'		
	matrix list odds
	
	* Robustness analysis, with cycle fixed effects and sandwich errors:
	logistic IGAstarted sevdiasb#groupcycles Age i.sex i.education i.Maritalstatus ib(4).workingstatus ib(6).economicsector i.socialsecurity i.banking i.DistrictNo, vce(robust) baselevels

