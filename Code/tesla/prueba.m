


% timestapm
 n=42;    % Number of samples on each epoch 
 x=(1:2)';     % Epochs
 r=repmat(x,1,n)';
 ts=r(:);

clearvars n x r

%%

% introduce Sdata (not many nodes)



%%


% TEST:
  T = 2;  % (2 epochs), 84 samples (42 for each epoch)
  P = 8; % (nodes)
%    l1 = 0.1;    l1_t = 0.001


% Define cvx_problem
% cvx_begin 
%  variable x1(8);  
%  variable x2(8);  minimize(  (sum (log(1 +exp(A1 * x1)) - b1 .* (A1 * x1)))/N1 + 0.100000 * norm(x1 .* e ,1) ...
%      + (sum(log(1 +exp(A2 * x2)) - b2 .* (A2 * x2)))/N2 + 0.100000* norm(x2 .* e ,1) + 0.001000*norm(x2-x1,1)  );  
%  cvx_end 

 
 m = 1;    % Solo en network del primer nodo
 % Define variables to handle data on cvx
selMatrix = ones(P,P);
for i=1:P
    selMatrix(i,i)=0;
end

sel = find(selMatrix(m,:)==1);
sel = [sel m];
I1 = find(ts==1);
I2 = find(ts==2);

A1 = data(I1,sel);
A2 = data(I2,sel);
 
b1 = data(I1,m);
b2 = data(I2,m);

A1(:,end)=1;
A2(:,end)=1;
 
c1=ones(length(I1),1);
c2=ones(length(I2),1);

N1=42;
N2=42;





%% Solve the problem!


cvx_clear;
cvx_precision(.01);
e= ones(sum(selMatrix(m,:))+1,1);
e(end)=0;

tic;
cvx_begin 
 variable x1(8);  
 variable x2(8);  minimize(  (sum (log(1 +exp(A1 * x1)) - b1 .* (A1 * x1)))/N1 + 0.100000 * norm(x1 .* e ,1) ...
     + (sum(log(1 +exp(A2 * x2)) - b2 .* (A2 * x2)))/N2 + 0.100000* norm(x2 .* e ,1) + 0.001000*norm(x2-x1,1)  );  
 cvx_end 
 toc;
 
 
 
 % WORKING SO FAR
 
 %% Store data
 
 tt = zeros(P,T);  % storing interactions between node m with the rest, for each epoch (each column)
 tt_temp = zeros(sum(selMatrix(m,:))+1,T);
 for t=1:T
     str2 = sprintf('tt_temp(:,t) = x%d;',t);
     eval(str2);
     tt(sel,t) = tt_temp(:,t);   % reorganize the vectors x# (their last point always corresponds to itself, we have to reassign it to its corresponding index)
 end
  
 
    tt = tt';
    result{m} = tt;   % store the result for node m in a cell
    d22 = getAvgDegAndTrimmedDeg(tt);
    
    
 %% checking the outFile and tempFile potential issues
cd('C:\Users\Manuel\Desktop\Southampton\MasterThesis\Code\tesla')
matobj = matfile('tempFile','Writable',true);
matobj.ts = ts;
outFile = tempFile; %%%% NOT DEFINED tempFile UNTIL WE INPOUT FIRST DATA IN IT
    