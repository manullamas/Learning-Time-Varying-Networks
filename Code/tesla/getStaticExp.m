function str = getStaticExp(M,bestLambda)

fprintf('\n creating string ...');
str = 'cvx_begin'; 

%define variables
str = sprintf('%s \n variable x(%d); ',str,M);

%add constraints
str = sprintf('%s minimize( ',str);
str = sprintf('%s (sum (log(1 +exp(A * x)) - b .* (A * x)))/N + %f * norm(x .* e ,1)',...
            str,bestLambda);
str = sprintf('%s ); ',str);
str = sprintf('%s cvx_end ',str);   