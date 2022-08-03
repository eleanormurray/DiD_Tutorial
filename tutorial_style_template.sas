/*************************************************************/
/**********DID Tutorial: Rothsbard et al, 2022****************/
/**********CODE SECTION A2: Style Template********************/
/**********SAS Code by Eleanor J Murray***********************/
/*************************************************************/

/*To create labeled output tables matching the paper, run this style template*/
/*After finishing the tutorial, run the template reset code at the bottom of this file*/
proc format;
	value Postfmt   0 = "Pre-DBT"
				    1 = "Post-DBT";
    value DBTfmt    0 = "Control"
				    1 = "Intervention";
	value timefmt -12 = "Pre-COVID"
				   	0 = "Baseline"
				    1 = "Follow-up";

	value stratafmt 11 = "Sealer, Very challenging"
					12 = "Sealer, Somewhat challenging"
					13 = "Sealer, Not challenging"
					21 = "Stapler, Very challenging"
					22 = "Stapler, Somewhat challenging"
					23 = "Stapler, Not challenging"
					31 = "Other device, Very challenging"
					32 = "Other device,Somewhat challenging"
					33 = "Other device,Not challenging" ;
run;

ods path sasuser.templat (UPDATE) sashelp.tmplmst (READ);
ods path show;
proc template;
 edit Base.Summary;
   dynamic clmpct one_var_name one_var_label one_var _double_space_ maxdec;
   column class id type ways (varname) (label) (nmiss) (mean);
   header h;

   define h;
      text "Estimated NoTECH Score ";
      space = 1;
      just = C;
      spill_margin;
   end;

   define class;
      vjust = T;
      id;
      generic;
      blank_internal_dups;
   end;

   define id;
      vjust = T;
      id;
      generic;
      blank_internal_dups;
   end;

   define type;
      header = "Type";
      vjust = T;
      id;
      blank_internal_dups;
   end;

   define ways;
      header = "Ways";
      vjust = T;
      id;
      blank_internal_dups;
   end;

   define varname;
      header = "Variable";
      id;
      generic;
   end;

   define label;
      header = "Label";
      id;
      generic;
   end;

   define nmiss;
      header = "N Miss";
      generic;
   end;

   define mean;
      header = "Mean";
	  format = 10.8;
      generic;
   end;

   required_space = 5;
   control = _control_;
   double_space = _double_space_;
   underline;
   overline;
   byline;
   use_format_defaults;
   split_stack;
   use_name;
   order_data;
   classlevels;
end;
quit;

/*To reset template after analysis:

ods path sashelp.tmplmst(read);
proc datasets library=sasuser nolist;
   delete templat(memtype=itemstor);
run;
ods path sasuser.templat(update) sashelp.tmplmst(read);
quit;
