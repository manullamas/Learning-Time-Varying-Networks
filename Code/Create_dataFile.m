%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%    Create dataFile    %%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% TODO:
%      - automatizar la entrada de datos: path = ... y simplemente tener que cambiar eel paht para cargar automaticamente
%          valores, nombres y fechas
% 


%% Load data (preprocesed in R)

% filename = returns / normalize values... (has to be a matrix) 
dataToLoad = csvread('C:\Users\Manuel\Desktop\Southampton\MasterThesis\Data\FTSE_0910\processed\FTSE100_returns.csv', 1, 0);
%StockNotNorm = csvread('C:\Users\Manuel\Desktop\Southampton\MasterThesis\Data\StocksNotNorm.csv', 1, 0);

% Preparation of data (number of samples (select rows to fit)
dataToLoad = dataToLoad(1:252,:);   % from 1jan to 12dec 2009





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


%% Create epoch vector

 n=21;    % Number of samples on each epoch 
 x=(1:12)';     % Epochs
 r=repmat(x,1,n)';
 ts=r(:);

clearvars n x r
 



%% Create dataFile

cd('C:\Users\Manuel\Desktop\Southampton\MasterThesis\Code\tesla')
matobj = matfile('dataFile','Writable',true);
matobj.data = dataToLoad;   %[NxP] ->  N: n samples (time steps);  P: n Nodes/stocks
matobj.ts = ts;  % [Nx1] ->  ts(i)= time stamp/epoch associated to i in data


%% Sparsity and Smooth penalties

sp = 0.001;   % {0.001, 0.005, ..., 0.002}
sm = 0.1;     % {0.1, 0.3, ..., 2}



% %% Plotting the normalized stock prices (checking if there are some general patterns)
% 
% for i = 1:length(dataToLoad(1,:))
%     plot(Dates, dataToLoad(:,i))
%     hold on
% end
% 
% 
% %%
% 
% %TEST
% %  37 - 60 (2008,2009)
% test = dataToLoad(37:60,:);
% %plot(Dates(37:60), test)
% ts_test = [1 1 1 1 2 2 2 2 3 3 3 3 4 4 4 4]';
% 
% matobj.data = test;
% matobj.ts = ts_test;
% 
% 

%% Create tempFile

cd('C:\Users\Manuel\Desktop\Southampton\MasterThesis\Code\tesla')
matobj = matfile('tempFile','Writable',true);




