Rscript model1a_2/sob_opp.R & Rscript model1a_2/sob_state.R ;

echo 'woohoo' ;

Rscript model1a_2/sob_state.R & Rscript model1a_3/sob_opp.R ;

echo 'woohoo woohoo' ;

Rscript model1a_3/sob_state.R & Rscript model1a_4/sob_opp.R & Rscript model1a_4/sob_state.R ; 

echo 'woohoo woohoo woohoo' ;

Rscript model1a_1/summ.R ; Rscript model1a_2/summ.R ; Rscript model1a_3/summ.R ; Rscript model1a_4/summ.R  

# Rscript model1a_1/sob_opp.R & Rscript model1a_1/sob_state.R & Rscript model1a_2/sob_opp.R ;

# echo 'woohoo' ;

# Rscript model1a_2/sob_state.R & Rscript model1a_2/sob_state.R & Rscript model1a_3/sob_opp.R ;

# echo 'woohoo woohoo' ;

# Rscript model1a_3/sob_state.R & Rscript model1a_4/sob_opp.R & Rscript model1a_4/sob_state.R ; 

# echo 'woohoo woohoo woohoo' ;

# Rscript model1a_1/summ.R ; Rscript model1a_2/summ.R ; Rscript model1a_3/summ.R ; Rscript model1a_4/summ.R  