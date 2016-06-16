function  [iniG, lasG,flag] = getBoundary(data,selMatrix,ts,l1,l11,m)

T=max(ts);
cvx_clear; cvx_precision(.001);

flag = 0;

if(length(l1)>1)
    sp = l1(2);
else
    sp = l1;
end


str = getStaticExp(sum(selMatrix(m,:))+1,sp);
sel = find(selMatrix(m,:)==1);
sel = [sel m];

%solve for the initial graph
I= find(ts<=T/2);
str2 = sprintf('A = data(I,sel);'); eval(str2);
str2 = sprintf('b = data(I,m);'); eval(str2);
str2 = sprintf('A(:,end)=1;');  eval(str2);
str2 = sprintf('c=ones(length(I),1);'); eval(str2);
str2 = sprintf('N=%d;',length(I)); eval(str2);

%now put the result back in iniG
 e= ones(sum(selMatrix(m,:))+1,1);
 e(end)=0;    %now just run the lass
 tic; eval(str); toc
 %store the weights
 str2 = sprintf('iniG = x;');  eval(str2);

if( strcmp(cvx_status,'Solved') ~= 1)
    flag = 1;
end

%solve for the final graph
I= find(ts>=T/2);
str2 = sprintf('A = data(I,sel);'); eval(str2);
str2 = sprintf('b = data(I,m);'); eval(str2);
str2 = sprintf('A(:,end)=1;');  eval(str2);
str2 = sprintf('c=ones(length(I),1);'); eval(str2);
str2 = sprintf('N=%d;',length(I)); eval(str2);

%now put the result back in iniG
 e= ones(sum(selMatrix(m,:))+1,1);
 e(end)=0;    %now just run the lass
 tic; eval(str); toc
 %store the weights
 str2 = sprintf('lasG = x;');  eval(str2);

 if( strcmp(cvx_status,'Solved') ~= 1)
    flag = 1;
 end

 
 