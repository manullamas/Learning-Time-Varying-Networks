% Different measures depending on different values of hte parameters: sp sm


% Fix one of the parameters and tune the other: analyse results
for n = sp...   % index so I can organize trials, from here I can get sp, sm by creating vector of values and use these indexes to refere them
    for m = sm...
        
      % CAMBIAR CARPETA PARA CADA ITERACION Y ASI GUARDAR LOS DATOS
      % ORDENADAMENTE??????
    
    
        sp = 
        sm = 
        str = sprintf('trial_%d = tesla2(''dataFile.mat'', sp, sm)', n)
        eval(str)
        
        
        
        % store trial_%d (1 cell with P matrices)
        
        
        % apply sign function: first I have to introduce
        % sign script in a function so I can refer it from here (difficult to define outputs, how do I
        % call them given that there are many different matrices as output)
        %%%%% One way is just to output degree, path and the
        %%%%% characteristics we want to measure.
        % Store matrices in case I find some interesting????
        
        
        
       % plots for different parameters values
       strFigure = sprintf('figure(%d%d)', n, m);  % organize figures given the indexes of parameters
       eval(strFigure)
       