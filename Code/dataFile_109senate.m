%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%    Create dataFile: US 109th senate    %%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


dataToLoad = csvread('C:\Users\Manuel\Desktop\Southampton\MasterThesis\Data\US109senate\109senate_clean.csv', 1, 0);


filename = 'C:\Users\Manuel\Desktop\Southampton\MasterThesis\Data\US109senate\senatorsTags.csv';
delimiter = '';
startRow = 2;
formatSpec = '%q%[^\n\r]';
fileID = fopen(filename,'r');
dataArray = textscan(fileID, formatSpec, 'Delimiter', delimiter, 'HeaderLines' ,startRow-1, 'ReturnOnError', false);
fclose(fileID);
senatorsTags = dataArray{:, 1};
clearvars filename delimiter startRow formatSpec fileID dataArray ans;




%% Create epoch vector

N = length(dataToLoad(:,1));

 n=63;    % Number of samples on each epoch 
 x=(1:N/n)';     % Epochs
 r=repmat(x,1,n)';
 ts=r(:);

clearvars n x r
 

%% Create dataFile

cd('C:\Users\Manuel\Desktop\Southampton\MasterThesis\Code\tesla')
matobj = matfile('dataFileSENATE','Writable',true);
matobj.data = dataToLoad;   %[NxP] ->  N: n samples (time steps);  P: n Nodes/stocks
matobj.ts = ts;  % [Nx1] ->  ts(i)= time stamp/epoch associated to i in data


%% Sparsity and Smooth penalties

sp = 0.001;
sm = 0.1;