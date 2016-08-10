%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%    Create dataFile    %%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% TODO:
%      - automatizar la entrada de datos: path = ... y simplemente tener que cambiar eel paht para cargar automaticamente
%          valores, nombres y fechas
% 


%% Load data (preprocesed in R)
 
returns = csvread('C:\Users\Manuel\Desktop\Southampton\MasterThesis\Data\FTSE_0910\processed\FTSE100_returns.csv', 1, 0);
%StockNotNorm = csvread('C:\Users\Manuel\Desktop\Southampton\MasterThesis\Data\StocksNotNorm.csv', 1, 0);
% Select rows to fit
%returns = returns(1:252,:);   % from 1jan to 12dec 2009

% filename = names of the stocks
filename = 'C:\Users\Manuel\Desktop\Southampton\MasterThesis\Data\FTSE_0910\processed\FTSE_names.csv';
delimiter = '';
startRow = 2;
formatSpec = '%q%[^\n\r]';
fileID = fopen(filename,'r');
dataArray = textscan(fileID, formatSpec, 'Delimiter', delimiter, 'HeaderLines' ,startRow-1, 'ReturnOnError', false);
fclose(fileID);
StockNames = dataArray{:, 1};
clearvars filename delimiter startRow formatSpec fileID dataArray ans;

% filename = dates
filename = 'C:\Users\Manuel\Desktop\Southampton\MasterThesis\Data\FTSE_0910\processed\DatesReturns.csv';
delimiter = '';
startRow = 2;
formatSpec = '%{yyyy-MM-dd}D%[^\n\r]';
fileID = fopen(filename,'r');
dataArray = textscan(fileID, formatSpec, 'Delimiter', delimiter, 'HeaderLines' ,startRow-1, 'ReturnOnError', false);
fclose(fileID);
Dates = dataArray{:, 1};
clearvars filename delimiter startRow formatSpec fileID dataArray ans;

%Dates = Dates(1:252);

%% Create epoch vector
   Nepochs = 24;
 n = floor(length(Dates)/Nepochs);    % Number of samples on each epoch 
 x = (1:Nepochs)';     % Epochs
 r = repmat(x,1,n)';
 ts = r(:);
 nStocks = size(returns,2);
 returns = returns(1:length(ts),:);
 
% Gain/Loss matrix
GainLoss = zeros(nStocks, length(x));
for i = 1:nStocks
    for t = 1:length(x)
        GainLoss(i,t) = returns(t*n,i)-returns((t-1)*n+1,i);        
    end
end
cd('C:\Users\Manuel\Desktop\Southampton\MasterThesis\Data\FTSE_0910\networks');
csvwrite('GainLoss.csv', GainLoss)
clearvars i t

% Distance Matrix (between stocks (Euclidean)
for t = 1:length(x)
    distMatrix = zeros(nStocks, nStocks);
    for i = 1:nStocks
        for j = 1:nStocks
            distMatrix(i,j) = norm(returns(((t-1)*n+1):(t*n),i)-returns(((t-1)*n+1):(t*n),j));
        end
    end
    if t < 10
       str = sprintf('csvwrite(''distMatrix_0%d.csv'', distMatrix)', t);
       eval(str);
    else
       str = sprintf('csvwrite(''distMatrix_%d.csv'', distMatrix)', t);
       eval(str);
    end
end
clearvars n x r

%% Create dataFile

cd('C:\Users\Manuel\Desktop\Southampton\MasterThesis\Code\tesla')
matobj = matfile('dataFile','Writable',true);
matobj.data = returns;   %[NxP] ->  N: n samples (time steps);  P: n Nodes/stocks
matobj.ts = ts;  % [Nx1] ->  ts(i)= time stamp/epoch associated to i in data


%% Sparsity and Smooth penalties

sp = 0.002;   % {0.001, 0.005, ..., 0.002}
sm = 0.01;     % {0.1, 0.3, ..., 2}

% Trials:
%
% sp=0.02 too high!!!   (sm=0.002 -> degree1=217, degree12=22)
% 
% sp=0.005 (sm=0.01) still to sparse
% 
%%










% %% Plotting the normalized stock prices (checking if there are some general patterns)
% 
% for i = 1:length(returns(1,:))
%     plot(Dates, returns(:,i))
%     hold on
% end
% 
% 
% %%
% 
% %TEST
% %  37 - 60 (2008,2009)
% test = returns(37:60,:);
% %plot(Dates(37:60), test)
% ts_test = [1 1 1 1 2 2 2 2 3 3 3 3 4 4 4 4]';
% 
% matobj.data = test;
% matobj.ts = ts_test;
% 
% 

%% Create tempFile
% 
% cd('C:\Users\Manuel\Desktop\Southampton\MasterThesis\Code\tesla')
% matobj = matfile('tempFile','Writable',true);
% 
% 


