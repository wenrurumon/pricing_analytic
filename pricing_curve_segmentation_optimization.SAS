
%let dic = dic;
%let file = Input.csv;

%macro sales_rate;
data raw;
	infile "&dic./&file." dlm = "," dsd firstobs = 2;
	length pg_brand $20 competitor_brand $20;
	input interval left_price right_price pg_brand competitor_brand packsize value category_value raw_salesrate observation;
	count = _n_;
run;
data pro/*(keep = x label idx obs count)*/;
	set raw;
	label = compbl(interval||pg_brand||Competitor_brand||packsize);
	rename 	observation = obs
			raw_salesrate = idx;
	x = -left_price; /*If negative corr with salesrate*/
run;

proc sort data = pro out = work; by label descending x; run;
%macro smooth1(idx = );
data work(drop = N lag_: mv_:);
	set work;
	by label;
	retain N;
	lag_0 = &idx.;
	array lags(*) lag_1 - lag_8;
	array mvs(*) mv_1 - mv_8;
	%do i = 1 %to 8;
		lag_&i = lag&i(&idx.);
	%end;
	if first.label then N = 1;
	else N+1;
	do i = N to 8;
		lags(i) = .;
	end;
	%do i = 1 %to 8;
		mv_&i = mean(of lag_0 - lag_&i.);
	%end;
	min_mv = min(of mv_1 - mv_8);
	rename min_mv = trend_&idx.;
	drop i;
run;
%mend smooth1;
%smooth1(idx =idx);
proc sort data = work; by label x; run;
%macro smooth2(idx =);
data work;
	set work;
	by label;
	retain trend;
	if first.label then trend = trend_&idx.;
	else if obs >= 30 then trend = max(trend, trend_&idx.);
	else trend = trend;
	rename trend = smoothed_&idx.;
run;
%mend smooth2;
%smooth2(idx =idx);

data work;
	set work;
	x = -x;
run;
proc sort data = work; by count;
proc sort data = raw; by count;
run;
data raw(drop = count);
	merge raw(in = a) work/*(keep = label smoothed_idx count)*/;
	by count;
	if a;
run;

proc export data = work outfile = "&dic.\output.csv" dbms = csv replace; run;
%mend sales_rate;
%sales_rate;
