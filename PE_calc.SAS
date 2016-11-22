
%macro consolidate_pe();
    
%let PE_lower_limit=-5;
%let PE_upper_limit=-0.5;
    
proc sql;
    create table perm.PE as 
    select b.sku, a.effect, a.estimate + b.estimate as Elasticity
    from perm.fixed_coeffs_2 a, perm.random_coeffs_2b b
    where a.effect=b.effect
    and a.effect in ("ln_ppi","ln_rp","ln_avg_comp_reg_price");
quit;
    
proc sql noprint;
    select min(Elasticity),max(Elasticity) into :min_rpe, :max_rpe
      from perm.pe
      where effect= "ln_rp" and elasticity between &PE_lower_limit and &PE_upper_limit;
    
    select min(Elasticity),max(Elasticity) into :min_ppe, :max_ppe
      from perm.pe
      where effect= "ln_ppi" and elasticity between &PE_lower_limit and &PE_upper_limit;

*    select min(Elasticity),max(Elasticity) into :min_cpe, :max_cpe
*      from perm.pe
*      where effect= "ln_avg_comp_reg_price" and elasticity between &PE_lower_limit and &PE_upper_limit;
*quit;

/*Aska */
/*Not sure wheather we have to control the CPE into some range, i didnt run any prior or adjustment for competitor PE so far*/
/*Checked with Pangpang, we might have to put the variable as log gap indix*/
    
%put &min_rpe &max_rpe;
%put &min_ppe &max_ppe;
    
data perm.pe;
    set perm.pe;
    if      effect= "ln_rp" and elasticity < &PE_lower_limit then elasticity= &min_rpe;
    else if effect= "ln_rp" and elasticity > &PE_upper_limit then elasticity= &max_rpe;
    else if effect= "ln_ppi" and elasticity < &PE_lower_limit then elasticity= &min_ppe;
    else if effect= "ln_ppi" and elasticity > &PE_upper_limit then elasticity= &max_ppe;
    else if effect = "ln_avg_comp_reg_price" and elasticity > 0 then elasticity = 0;
*    else if effect= "ln_avg_comp_reg_price" and elasticity < &PE_lower_limit then elasticity= &min_cpe;
*    else if effect= "ln_avg_comp_reg_price" and elasticity > &PE_upper_limit then elasticity= &max_cpe;
run;

proc transpose data=perm.pe out = perm.pe(drop = _name_); 
	var elasticity; 
	id effect;
	by sku; 
run;
    
%mend;

%consolidate_pe;

proc means data = perm.aggs nway noprint;
    class sku;
    var total_volume value;
    output out = aggs(drop = _:) sum() = mean = N = obs;
run;

proc sql;
	create table aggs2 as
	select a.*, b.*, c.*
	from aggs as a, perm.pe as b, perm.upcs as c
	where a.sku = b.sku = c.sku;
run;
