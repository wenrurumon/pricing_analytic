%macro run_mixed_model(model_data=,data_option=,dep_var=,ind_var=,ind_var_nb=,model_subject=,weight_var=, out_pred=,out_coef=);

    proc mixed data=&model_data.(&data_option.)
      method=mivque0  namelen=50 ;
      model &dep_var = / noint s ddfm=kr;
      random &ind_var  / s type=fa0(&ind_var_nb) subject=&model_subject;
      weight &weight_var;
      ods output solutionr=&out_coef;
    quit;
    
    %if %length(&out_pred) >0 %then %do;
    
      DATA coef_m;
        length effect $32;
        set &out_coef;
        effect="coef_" || effect;
        &model_subject = input(left(trim(subject)), 3.);
    
      PROC TRANSPOSE DATA= coef_m out = coef_tran(drop=_NAME_);
        VAR estimate;
        ID effect;
        BY &model_subject.;
      run;

      DATA &out_pred;
        MERGE &model_data(in=a &data_option.) coef_tran(in=b);
        by &&model_subject;
        if a and b;
        pred=0 
        %do m_var_i=1 %to &ind_var_nb;
          + %scan(&ind_var,&m_var_i," ") * coef_%scan(&ind_var,&m_var_i," ")
        %end;;
        resid=&dep_var. - pred;
        drop 
        %do m_var_i=1 %to &ind_var_nb;
           coef_%scan(&ind_var,&m_var_i," ")
        %end;;

      proc datasets library=work nolist;  delete coef_m coef_tran; run;
    %end;
%mend run_mixed_model; 
