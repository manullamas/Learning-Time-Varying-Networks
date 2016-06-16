function str = getCVXExp(M,T,l1,l1_t,l2,constBound,iniG,lasG)

if(nargin<6)
     constBound=0;
end

addL11 = 1;
addL2 = 0;
if(length(l1)==1)
    l1 = ones(T,1)* l1;
end

if(length(l1_t)==1)
    l1_t = ones(T,1)* l1_t;
end

fprintf('\n creating string ...');
str = 'cvx_begin';   %cvx_gp_precision(.1); cvx_precision low;  ';
%define variables
for t=1:T
    if(t==1)
        str = sprintf('%s \n variable x%d(%d); ',str,t,M);
    else
        str = sprintf('%s variable x%d(%d); ',str,t,M);
    end
end

%add constraints
str = sprintf('%s minimize( ',str);
for t=1:T
    if(t==1)
        %str = sprintf('%s norm(A%d*x%d-b%Md) + %f*norm(x%d,1) ',str,t,t,t,para.l1,t);
        str = sprintf('%s (sum (log(1 +exp(A%d * x%d)) - b%d .* (A%d * x%d)))/N%d + %f * norm(x%d .* e ,1)',...
            str,t,t,t,t,t,t,l1(t),t);
    else
        str = sprintf('%s + (sum(log(1 +exp(A%d * x%d)) - b%d .* (A%d * x%d)))/N%d + %f* norm(x%d .* e ,1)',...
                       str,t,t,t,t,t,t,l1(t),t);
    end
end

%add temporal constraints
%first add due to initial  network
if(constBound)
   str = sprintf('%s + %f*norm(x1-iniG,1) ',str,l1_t(1));    
end

for t=2:T
    if ( (addL11) && (addL2))
        str = sprintf('%s + %f*norm(x%d-x%d,1) +  %f*norm(x%d-x%d,2) ',str,l1_t(t),t,t-1,l2,t,t-1);    
    elseif ((~addL11) && (addL2))
        str = sprintf('%s + %f*(sum_square(x%d-x%d)) ',str,l2,t,t-1);    
   elseif ((addL11) && (~addL2))
        str = sprintf('%s + %f*norm(x%d-x%d,1) ',str,l1_t(t),t,t-1);    
    end
end

%add condition due to final boundary graph
if(constBound)
   str = sprintf('%s + %f*norm(x%d-lasG,1) ',str,l1_t(T),T);    
end

str = sprintf('%s ); ',str);
str = sprintf('%s cvx_end ',str);    