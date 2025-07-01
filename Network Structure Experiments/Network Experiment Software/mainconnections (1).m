% main file: generate connection files for network experiment

clear all; close all; 


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% input control
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

nodesnum = 100;%100;%50;%20;  % # of nodes
trialnum = 40;  % # of trials (rounds) in the network exp
neighborsize = nodesnum;%nodesnum;%4;  % neighborhood size

simnum = 10; % the total number of experiments to simulate
for randseed = 1:simnum 
    if randseed<4
        flagsavefig = 1;  % 1: save rst fig; 0: not save
    else
        flagsavefig = 0;  % 1: save rst fig; 0: not save
    end;
    network_connection_spatial_func(randseed,nodesnum,trialnum,neighborsize,flagsavefig)
end;

