%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%% PROCESS AND ANALISE OUTPUT %%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%function [] = 

cd('C:\Users\Manuel\Desktop\Southampton\MasterThesis\Code\tesla');
%%%%%% Change input dataFile, tesla output and boundary of the weights

load('dataFile.mat');    % choose dataFile (same as tesla input)
T= max(ts);
[N,P] = size(data);
load('teslaResults_sp0.001_sm0.005.mat'); %result = results; clearvars results;
result_sign = cell(P,1);
degreeMatrix = zeros(P,T);
% Store networks belonging to each epoch in matrices
for t = 1:T   % epochh we are on
    str1 = sprintf('matrix_%d = zeros(P,P);', t);
    eval(str1);
    for n = 1:P  %node we are on
        str2 = sprintf('matrix_%d(%d,:) = result{%d}(%d,:);', t, n, n, t);
        eval(str2);
    end
    
% Convert to real 0 tiny values
    str3 = sprintf('m = matrix_%d',t);
    eval(str3);
    for r = 1:P
        for c = 1:P
            if abs(m(r,c)) < 0.01     %%% change reject boundary
                m(r,c) = 0;
            end
% Just plotting when both ways interactions
%             if m(r,c) ~= m(c,r)
%                 m(c,r) = 0;
%             end
            m(r,r) = 0;  % diagonal elem = 0 (igraph requirement)
        end
    end
    
% store results in a 'signed' matrix (-1, 0, 1)
    str4 = sprintf('matrix_sign_%d = zeros(P,P)', t);
    eval(str4);
    str5 = sprintf('matrix_sign_%d = m',t);
    eval(str5);
    str6 = sprintf('matrix_sign_%d = sign(matrix_sign_%d)', t, t);
    eval(str6);
%     
% % Save networks
%     dataPSath = 'C:\Users\Manuel\Desktop\Southampton\MasterThesis\Data\Test1';
%    
%     string = sprintf('csvwrite(paste0(dataPath,'network_%d'), matrix_sign_%d)', t)
%     
%     
    str7 = sprintf('degreeMatrix(:,t) = sum(abs(matrix_sign_%d), 2)', t);   % row sums of  matrix_1, matrix_2....
    eval(str7);
end

%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%% EXPLORATORY ANALYSIS %%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% compute avg degree over all time stamps
avgDegree = mean(degreeMatrix, 2);

% load stocks names as table
filename = 'C:\Users\Manuel\Desktop\Southampton\MasterThesis\Data\FTSE_0910\FTSE_indexes.csv';
delimiter = ',';
startRow = 2;
formatSpec = '%*q%q%[^\n\r]';
fileID = fopen(filename,'r');
dataArray = textscan(fileID, formatSpec, 'Delimiter', delimiter, 'HeaderLines' ,startRow-1, 'ReturnOnError', false);
fclose(fileID);
FTSEindexes = table(dataArray{1:end-1}, 'VariableNames', {'FTSE100_list'});
clearvars fileID ans;
% load stock names as list (cell): needed to add tags in plots.
fileID = fopen(filename,'r');
fclose(fileID);
FTSElist = [dataArray{1:end-1}];
clearvars filename delimiter startRow formatSpec fileID dataArray ans;

% plot avgDegree per asset
figure()
plot(1:length(avgDegree), avgDegree,'*')
hold on
for u = 1:length(avgDegree)
    plot(u, avgDegree(u), '*')
    text(u, avgDegree(u), FTSElist{u})
    hold on
    
   % https://uk.mathworks.com/matlabcentral/answers/156671-how-to-plot-points-in-different-colors-based-on-class
   % to give different color to different sectors (class)
end
line([0 length(avgDegree)+5], [25 25])
%%%%%%%% name it with sp and sm values!!!

% Exploring how connections of hubs change over epochs
for j = 1:length(degreeMatrix)
    if avgDegree(j) >= 3
        figure()
        plot(1:T, degreeMatrix(j,:), '*-')
        title(FTSElist(j))
        hold on
    end
end
    
% General info from 1st and last networks 
matrix_1;
matrix_12;
disp([FTSEindexes array2table(avgDegree)])
disp('explore values of first and last matrices: strong/weak correlations?')
disp(' ')
disp(['Max_weight net1:   ',  num2str(max(max(matrix_1)))])
disp(' ')
disp(['min_weight net1:   ',  num2str(min(min(matrix_1)))])
disp(' ')
disp(['Max_weight net12:   ',  num2str(max(max(matrix_12)))])
disp(' ')
disp(['min_weight net12:   ',  num2str(min(min(matrix_12)))])
disp(' ')
disp(['N edges 1:   ',  num2str(sum(sum(abs(matrix_sign_1))))])
disp(' ')
disp(['N edges 12:   ',  num2str(sum(sum(abs(matrix_sign_12))))])
disp(' ')
disp(['% of variation:   ', num2str((sum(sum(matrix_sign_1))-sum(sum(matrix_sign_12)))*100/sum(sum(matrix_sign_1))), '%'])
disp(' ')
disp('                                   N edges arpund 1000???')



%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%   STORE DATA AND ANALYSIS VARIABLES   %%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

close all;

cd('C:\Users\Manuel\Desktop\Southampton\MasterThesis\Data\FTSE_0910\networks');

% DATA
tags = [0:1:P]';
for t = 1:T
%     str8 = sprintf('matrix_sign_%d = [[1:P]; matrix_sign_%d];', t, t);
%     eval(str8);
%     str9 = sprintf('matrix_sign_%d = [tags, matrix_sign_%d];', t, t);
%     eval(str9);
%     str10 = sprintf('matrix_%d = [[1:P]; matrix_%d];', t, t);
%     eval(str10);
%     str11 = sprintf('matrix_%d = [tags, matrix_%d];', t, t);
%     eval(str11);

    if t < 10     % Store as 01, 02, 03...
        str12 = sprintf('csvwrite(''FTSE100_0%d_signs.csv'', matrix_sign_%d)', t, t);
        eval(str12);
        str13 = sprintf('csvwrite(''FTSE100_0%d_weights.csv'', matrix_%d)', t, t);
        eval(str13);
    else        
        str12 = sprintf('csvwrite(''FTSE100_%d_signs.csv'', matrix_sign_%d)', t, t);
        eval(str12);
        str13 = sprintf('csvwrite(''FTSE100_%d_weights.csv'', matrix_%d)', t, t);
        eval(str13);
    end
       
    % Clear processed matrices
    %str14 = sprintf('clearvars matrix_sign_%d, matrix_%d', t, t);
    %eval(str14);
end


% ANALYSIS VARIABLES
csvwrite('degreeMatrix.csv', degreeMatrix)
csvwrite('avgDegree.csv', avgDegree)


cd('C:\Users\Manuel\Desktop\Southampton\MasterThesis\Code\tesla');
clc;





%% 

% TODO
%      - transform signed and weighted matrices (sparse!) to vertices and edges lists (1st kind of input for igraph)









 %% TEST NETWORK
% 
% % automatizar el guardado de datos solamente escogiendo el numero de matriz
% % que estamos guardando en cada momento convirtiendo los siguientes pasos
% % en strings y evaluandolas: (loop for 1:T)
% 
% cd('C:\Users\Manuel\Desktop\Southampton\MasterThesis\Data\FTSE_0910\networks');
% % T = ;
% % for i = 1:T
% 
% matrix_sign_12 = [[1:P]; matrix_sign_12];
% tags = [0:1:P]';
% matrix_sign_12 = [tags, matrix_sign_12];
% csvwrite('FTSE100_12.csv', matrix_sign_12)
% cd('C:\Users\Manuel\Desktop\Southampton\MasterThesis\Code\tesla');
% %clearvars t n
% % matrix_t: rows each of the nodes represented
% 
% cd('C:\Users\Manuel\Desktop\Southampton\MasterThesis\Data\FTSE_0910\networks');
% matrix_12 = [[1:P]; matrix_12];
% matrix_12 = [tags, matrix_12];
% csvwrite('FTSE100_12_weights.csv', matrix_12)
% cd('C:\Users\Manuel\Desktop\Southampton\MasterThesis\Code\tesla');
% %clearvars t n
% % matrix_t: rows each of the nodes represented
% 





 %% Sign function to apply after TESLA results
% 
% %  sign(x) = 0      if  x = 0
% %  sign(x) = 1      if  x > 0
% %  sign(x) = -1      if  x < 0
% 
% load('dataFile.mat');
% [N,P] = size(data);
% result_sign = cell(P,1);
% for n = 1:P
%     result_sign{n} = sign(result{n});
% end
% 
