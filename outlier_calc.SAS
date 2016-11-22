%let flag =miss_flag; /*the missing flag option*/

proc sql noprint;
    select estimate into :coef_missing
    from perm.fixed_coeffs_2 
    where cats(Effect)="missing_flag";
quit;

/*
proc sort data = perm.upcs(keep = sku manu_desc) out = sku1 nodupkey; by sku;
proc sort data = perm.upcs(keep = sku brand_desc) out = sku2 nodupkey; by sku;
data sku1; set sku1; manu_code = _n_;
data sku2; set sku2; brand_code = _n_;
run;
data sku;
    merge sku1 sku2;
	by sku;
run;
*/

proc sort data = perm.upcs(keep = sku manu_desc brand_desc) out = sku nodupkey; by sku; run;

proc sort data = sku nodupkey; by sku manu_desc brand_desc; run;
proc sort data = perm.aggs_ori_all; by store per sku; run;
data bulksales_decomp;
	merge
		perm.aggs_ori_all(in = a keep = store per sku chain value total_volume xf zf)
    	perm.pred_2(in = b drop = chain value total_volume);
    by store per sku;
    if a;
	if xf = . then xf = 1;
	if zf = . then zf = 1;
	vol = total_volume;
run;
proc sql;
    create table bulksales_decomp as
    select  a.store, b.*, a.per,/*count(*) as total_cnt,
    sum(missing_flag>0) as flag_cnt, 
    sum(missing_flag_2>0) as flag_2_cnt, 
    sum(missing_flag_3>0) as flag_3_cnt, 
    sum(missing_flag_4>0) as flag_4_cnt, */
    sum(vol * xf) as sales_vol ,
    sum(vol * xf  * (exp(missing_flag   * &coef_missing ) -1 )/ exp(missing_flag   * &coef_missing )) as miss_flag_vol,
    sum(vol * xf  * (exp(missing_flag_2 * &coef_missing ) -1 )/ exp(missing_flag_2 * &coef_missing )) as miss_flag_2_vol,
    sum(vol * xf  * (exp(missing_flag_3 * &coef_missing ) -1 )/ exp(missing_flag_3 * &coef_missing )) as miss_flag_3_vol,
    sum(vol * xf  * (exp(missing_flag_4 * &coef_missing ) -1 )/ exp(missing_flag_4 * &coef_missing )) as miss_flag_4_vol,
    sum(value * xf) as sales_value ,
    sum(value * xf  * (exp(missing_flag   * &coef_missing ) -1 )/ exp(missing_flag   * &coef_missing )) as miss_flag_val,
    sum(value * xf  * (exp(missing_flag_2 * &coef_missing ) -1 )/ exp(missing_flag_2 * &coef_missing )) as miss_flag_2_val,
    sum(value * xf  * (exp(missing_flag_3 * &coef_missing ) -1 )/ exp(missing_flag_3 * &coef_missing )) as miss_flag_3_val,
    sum(value * xf  * (exp(missing_flag_4 * &coef_missing ) -1 )/ exp(missing_flag_4 * &coef_missing )) as miss_flag_4_val
    from bulksales_decomp a, sku b
    where a.sku=b.sku
    /*group by  b.MANU_DESC,b.BRAND_DESC, b.sku, a.per, a.store;*/
	group by a.store, a.per, a.sku, b.manu_desc, b.brand_desc;
quit;
data perm.bulksales_decomp;
    set bulksales_decomp;
run;
