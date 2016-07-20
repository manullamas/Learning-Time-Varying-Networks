%% Transform output to matrices to be represented

%%%%%% Change input dataFile, tesla output and boundary of the weights

load('dataFile.mat');    % choose dataFile (same as tesla input)
T= max(ts);
[N,P] = size(data);
result_sign = cell(P,1);
result = FTSE100;        % tesla output

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
            if abs(m(r,c)) < 0.001     %%% change boundary
                m(r,c) = 0;
            end
% Just plotting both ways interactions             
%             if m(r,c) ~= m(c,r)
%                 m(c,r) = 0;
%             end
            m(r,r) = 1;
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
    
end


%% TEST NETWORK

% automatizar el guardado de datos solamente escogiendo el numero de matriz
% que estamos guardando en cada momento convirtiendo los siguientes pasos
% en strings y evaluandolas: (loop for 1:T)
%number = ;

cd('C:\Users\Manuel\Desktop\Southampton\MasterThesis\Data\FTSE_0910\networks');
matrix_sign_12 = [[1:P]; matrix_sign_12];
tags = [0:1:P]';
matrix_sign_12 = [tags, matrix_sign_12];
csvwrite('FTSE100_12.csv', matrix_sign_12)
cd('C:\Users\Manuel\Desktop\Southampton\MasterThesis\Code\tesla');
%clearvars t n
% matrix_t: rows each of the nodes represented

cd('C:\Users\Manuel\Desktop\Southampton\MasterThesis\Data\FTSE_0910\networks');
matrix_12 = [[1:P]; matrix_12];
matrix_12 = [tags, matrix_12];
csvwrite('FTSE100_12_weights.csv', matrix_12)
cd('C:\Users\Manuel\Desktop\Southampton\MasterThesis\Code\tesla');
%clearvars t n
% matrix_t: rows each of the nodes represented


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
