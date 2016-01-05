function [A] = read_data(filename)

fileID = fopen(filename,'r');

formatSpec = '%f';

A = fscanf(fileID,formatSpec);

fclose(fileID);

return


