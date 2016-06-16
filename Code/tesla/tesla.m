% please first make sure that CVX is installed properly. See CVX documentations.
%
% To use
%
% result = tesla(dataFile,sp,sm,tempFile);
%
% or with full option as:
%
%result = tesla(dataFile,sp,sm,tempFile,st,ed,consBound,ontology);
%
% where:
%
%dataFile: is a .mat file with two variables, data and ts where
% data is an [N,P] binary matrix where N is the number of samples and P is the
% number of nodes in the network, and ts is an [Nx1] vector where ts(i) is
% the time stamp (i.e. epoch) associated with example i in data(i,:).
%
%sp: sparsity penalty coefficient. It is either a scalar or an [Tx1]
%vector where sp(t) is the sparsity penality at epoch t. If sp is a scalar
%then the same sparsity penalty is enforced at each epoch.
%
%sm: is the smoothness penalty. Same format as in sp: either a scalar or a [Tx1] vector.
%
%tempFile: a file where temporary result are stored, useful to resume if
% operation from where it was stopped. It is useful if the user wants to refit a subset of the network and
%resume later.
%
%st,ed: (optional) are beginning and end indexes of the subset of the network to be
%fitted. Useful if the user wants to only fit a subset graph. They are
%defaulted to 1 and P respectively.
%
%consBound: (optional) default to 0. If 1, a static network is learnt at each
%boundary using half of the data and then used as a boundary condition
%within the TESLA framework.
%
%ontology: (optional) , this is a PxP binary matrix where ontology(p,:)
%gives the candidate neighbours of node p where ontology(p,i)=0 means there
%can not be an edge between node p and node i, and ontology(p,i)=1 means
%there MIGHT be an edge. Useful if one wants to specify prior knowledge.
%
%result: is a [Px1] cell array where result{p} is an [TxP] real matrix where
%result{p}(t,i) is the coeff. over the edge that connects node p to node i
%at epoch t (can be possibly zero). Note: If st and ed are specified, then only
%result{st:ed} are filled in this run, other entries are read from the
%tmepFile of the most recent run, if any otherwise they will be left empty.
%


function result = tesla(dataFile,sp,sm,tempFile,st,ed,consBound,ontology)


load(dataFile);  %this loads data,ts
[N,P] = size(data);

T= max(ts);

if(nargin > 7)
    selMatrix = ontology;
else
    selMatrix = ones(P,P);
    for i=1:P
        selMatrix(i,i)=0;
    end
end

if(nargin<7)
    consBound=0;
end

if(nargin<5)
    st=1;
    ed = P;
end


%check where did we ended last time
outFile = tempFile;  %sprintf('%s-out.mat',tempFile);
try
    load(outFile);
    start = lastNode+1;
catch
    start = st;
    lastNode=0;
    result = cell(P,1);
end

for m=start:ed
    fprintf('\n solving for variable %d/%d ...',m,P);
    if(consBound)
            fprintf('\n Getting boundary network');
            [iniG, lasG,isBad] = getBoundary(data,selMatrix,ts,sp,sm,m);
            fprintf('\n Done Getting boundary network with');
    else
            iniG = 0;
            lasG = 0;
    end
    flag = 1;
    tmConsBound=consBound;
    trial = 1;
    while flag==1
        fprintf('\n Now solving for sparsness=%f and smooth=%f, T=%d',sp(1),sm(1),T);
        if((~consBound) || (isBad)) %failed to sovle the static problem
            str = getCVXExp(sum(selMatrix(m,:))+1,T,sp, sm,0,0,iniG,lasG);
        else
            str = getCVXExp(sum(selMatrix(m,:))+1,T,sp, sm,0,consBound,iniG,lasG);
        end
        sel = find(selMatrix(m,:)==1);
        sel = [sel m];
        try
            for t=1:T
                I= find(ts==t);
                str2 = sprintf('A%d = data(I,sel);',t);
                eval(str2);
                str2 = sprintf('b%d = data(I,m);',t);
                eval(str2);
                str2 = sprintf('A%d(:,end)=1;',t);
                eval(str2);
                str2 = sprintf('c%d=ones(length(I),1);',t);
                eval(str2);
                str2 = sprintf('N%d=%d;',t,length(I));
                eval(str2);
            end
        catch
            fprintf('\n An error occured please contact the developer....');
            result = {};
            return;
        end
        if (trial==1)
            cvx_clear;
            cvx_precision(.01);
        end
        e= ones(sum(selMatrix(m,:))+1,1);
        e(end)=0;    %now just run the lass
        tic; eval(str); toc
        %if(cvx_status()~='Solved')
        if( strcmp(cvx_status,'Solved') ~= 1)
            flag = 1;
            trial=trial +1;
            if(trial==2)
                cvx_clear;
                cvx_precision(.0001);
            elseif (trial==3)
               consBound=0;
               fprintf('\n Trying to solve with no boundary constraints');
               cvx_clear;
               cvx_precision(.00001);
            elseif (trial==4)
               consBound=0;
               fprintf('\n Trying to solve with no boundary constraints');
                cvx_clear;
                cvx_precision(.000001);
            end
        else
            flag = 0;
        end
        if(trial>4)
            flag=0;
            fprintf('\n*************************** Failed to solve***********');
            result={};
            return;
        end
    end
    consBound = tmConsBound;
    %store the weights
    tt = zeros(P,T);
    tt_temp = zeros(sum(selMatrix(m,:))+1,T);
    for t=1:T
        str2 = sprintf('tt_temp(:,t) = x%d;',t);
        eval(str2);
        tt(sel,t) = tt_temp(:,t);
    end
    tt = tt';
    result{m} = tt;
    %display average sparsness
    d22 = getAvgDegAndTrimmedDeg(tt);
    fprintf('\n average Degree for %d= %f ',m,d22);
    %write the matrix
    lastNode = m;
    save(outFile, 'lastNode', 'result');
end



    






