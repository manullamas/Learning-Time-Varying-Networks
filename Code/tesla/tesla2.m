
%cd('C:\Users\Manuel\Desktop\Southampton\MasterThesis\Code\tesla')
function result = tesla2(dataFile,sp,sm,st,ed)

load(dataFile);  %this loads data,ts
[N,P] = size(data);

T= max(ts);

selMatrix = ones(P,P);
for i=1:P
     selMatrix(i,i)=0;
end
   consBound=0;

if(nargin<4)
    st = 1;
    ed = P;
end
    start = st;
    result = cell(P,1);

for m=start:ed   %going through the NODES
    fprintf('\n solving for variable %d/%d ...',m,P);
    iniG = 0;
    lasG = 0;
%    flag = 1;
%     trial = 1;
%     while flag==1
        fprintf('\n Now solving for sparsness=%f and smooth=%f, T=%d',sp(1),sm(1),T);
        str = getCVXExp(sum(selMatrix(m,:))+1,T,sp, sm,0,0,iniG,lasG);
        sel = find(selMatrix(m,:)==1);
        sel = [sel m];
        try
            % Define variables to use in the cvx problem (A#, b#, c#, N#)
            for t=1:T
                I= find(ts==t);
                str2 = sprintf('A%d = data(I,sel);',t);    % Subset A#: samples from 'data' corresponding to epoch # 
                eval(str2);
                str2 = sprintf('b%d = data(I,m);',t);    % Define b#: it is the current state of the node we are on (x#)
                eval(str2);
                str2 = sprintf('A%d(:,end)=0;',t);    % Turn data corresponding to the node we are to 1 (defined in the algorithm)
                eval(str2);
                str2 = sprintf('c%d=ones(length(I),1);',t);   % vector of 1, length = samples per epoch
                eval(str2);
                str2 = sprintf('N%d=%d;',t,length(I));    % N# = samples per epoch
                eval(str2);
            end
        catch
            fprintf('\n An error occured please contact the developer....');
            result = {};
            return;
        end
%         if (trial==1)
            cvx_clear;
            cvx_precision(.01);
%         end
        e= ones(sum(selMatrix(m,:))+1,1);
        e(end)=0;    %now just run the lass       ?????????????? it is running everything!
        tic; eval(str); toc    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%         if( strcmp(cvx_status,'Solved') ~= 1)
%             flag = 1;
%             trial=trial +1;
%             if(trial==2)
%                 cvx_clear;
%                 cvx_precision(.0001);
%             elseif (trial==3)
%                consBound=0;
%                fprintf('\n Trying to solve with no boundary constraints');
%                cvx_clear;
%                cvx_precision(.00001);
%             elseif (trial==4)
%                consBound=0;
%                fprintf('\n Trying to solve with no boundary constraints');
%                 cvx_clear;
%                 cvx_precision(.000001);
%             end
%         else
%             flag = 0;
%         end
%         if(trial>4)
%             flag=0;
%             fprintf('\n*************************** Failed to solve***********');
%             result={};
%             return;
%         end
    %store the weights
    tt = zeros(P,T);
    tt_temp = zeros(sum(selMatrix(m,:))+1,T);
    for t=1:T
        str2 = sprintf('tt_temp(:,t) = x%d;',t);
        eval(str2);
        tt(sel,t) = tt_temp(:,t);  %ultimo valor de tt_temp corresponde al propio nodo m (problematico) -> x(t) devuelto por getCVXExp
    end
    tt = tt';
    result{m} = tt;      % Store results: each result{m} is a matrix [TxP] where each row represents the edges from node m (€P) to the rest on the epoch t (€T).

%     fprintf('\n average Degree for %d= %f ',m,d22);
end

cd('C:\Users\Manuel\Desktop\Southampton\MasterThesis\Code\tesla')
matobj = matfile('teslaResults_sp0.001_sm0.03.mat','Writable',true);
matobj.result = result; 

