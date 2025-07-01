% spatial network connections generation

%output filename: filename = ['connection_' num2str(nodesnum) '.' num2str(trialnum) '.' num2str(neighborsize) '_' num2str(randseed)];
% connection_20.40.4_1: connection file for network with 20 nodes, 40
% trials, neighborhood size of 4, randsee number is 1
% in the CSV file, first two cols are node index for partners; the third col is the trial index

function network_connection_spatial_func(randseed,nodesnum,trialnum,neighborsize,flagsavefig)

close all; 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% input control
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% flagsavefig = 0;  % 1: save rst fig; 0: not save
% 
% randseed = 1; 
% nodesnum = 20;%100;%50;%20;  % # of nodes
% trialnum = 40;  % # of trials (rounds) in the network exp
% neighborsize = 4;%nodesnum;%4;  % neighborhood size

rng(randseed);

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
nodearrayraw = [1:nodesnum];
nodenb = round(neighborsize/2);
nodearrayexd = [nodesnum-nodenb+1:nodesnum nodearrayraw 1:nodenb];


% generate local neighborhood for each node
for i = 1:nodesnum
    idx = i+nodenb;
    neighbornodes{i} = nodearrayexd(idx-nodenb:idx+nodenb);
    neighbornodes{i}(nodenb+1) = [];  % remove the node itself
end;

ccount = 0; 
for ti = 1:trialnum
    nodeflag(1:nodesnum) = 0;  
    resampleflag = 1;
    disp(ti);
    k =randi(nodesnum,1);
    nodeseq = circshift(1:nodesnum, k);% randomize the order of node partner assignment
    %nodeseq = randperm(nodesnum);  

    while resampleflag == 1
        nodeflag(1:nodesnum) = 0;  
        count1 = ccount;
        for i = nodeseq
            if nodeflag(i) == 0  % if the node has not be used so far
                counttemp = 0; 
                while 1
                    counttemp = counttemp + 1;
                    indsample = randi([1,neighborsize],1);
                    nodecontemp = neighbornodes{i}(indsample);
                    if prod(nodeflag(neighbornodes{i}))==1  % all neighbors are taken
                        resampleflag = 1;
                        connectmat(count1+1:end,:)=[]; % remove the connections in this ran;
                        ccount = count1;
                        break;
                    end
                    if nodeflag(nodecontemp) == 0  % if the proposed partner is still availble, otherwise resample
                        ccount = ccount +1;
                        connectmat(ccount,1) = i; 
                        connectmat(ccount,2) = nodecontemp; 
                        connectmat(ccount,3) = ti;
                        nodeflag(i) = 1;
                        nodeflag(nodecontemp) = 1; 
                        resampleflag = 0;
                        break; 
                    end;
                    if counttemp>1000
                        error('seems to get into cycle of sampling. Change randseed number and retry it');
                        break;
                    end;
                end;

                if resampleflag == 1
                   break;
                end;
            end;
        end;  % node i ends

        if prod(nodeflag)==1
            resampleflag = 0;
        end;
    end;
    if prod(nodeflag)==0
        error('having the missing nodes');
    end;

end;  % trial ti ends

for i = 1:size(connectmat,1)
    if connectmat(i,1)>connectmat(i,2)
        temp = [connectmat(i,2) connectmat(i,1)];
        connectmat(i,1:2) = temp;
    end;
end;

filename = ['connection_' num2str(nodesnum) '.' num2str(trialnum) '.' num2str(neighborsize) '_' num2str(randseed)];
writematrix(connectmat, ['.\rst\' filename '.csv']);
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%% below is just for visualization purpose %%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% get node coordinates
ti = 1; 
edge1all = [];
edge2all = [];
rowindx = find(connectmat(:,3)==ti);
edge1 = connectmat(rowindx,1);
edge2 = connectmat(rowindx,2);

G = graph(edge1,edge2);
figinit = figure;
h1 = plot(G,'Layout','circle');
xval = h1.XData;
yval = h1.YData; 
nodenum = length(xval);
for i = 1:nodenum
    if rem(i,2)
        rad = 1;
    else
        rad = 0.8;
    end;
    xval(i) = rad*cos(2*pi/nodenum*i);
    yval(i) = rad*sin(2*pi/nodenum*i);
end;
close(figinit);

%% plot connection in network
figure;
% plot network connection and rst
edge1all = [];
edge2all = [];
for ti =  1:trialnum
    rowindx = find(connectmat(:,3)==ti);
    edge1 = connectmat(rowindx,1);
    edge2 = connectmat(rowindx,2);
    edge1all = [edge1all; edge1];
    edge2all = [edge2all; edge2];

    G = graph(edge1,edge2);


    h2 = plot(G,'Layout','circle','NodeLabel','');
    h2.XData = xval;
    h2.YData = yval; 

    axis square;
    axis off; 
    title(['Trial ' num2str(ti)]);

    pause(0.1);

    if flagsavefig==1
       exportgraphics(gcf,['.\rst\' filename '.gif'],'Append',true);
    end;

end;

figure;    
Ecomb = unique([edge1all edge2all],'rows');
for ei = 1:size(Ecomb,1)
    E1all(1,ei) = Ecomb(ei,1);
    E2all(1,ei) = Ecomb(ei,2);
    Enum(1,ei) = length(find(Ecomb==Ecomb(ei,:)));
end;

Gall = graph(E1all,E2all);
% Gall.Edges.Weight = Enum';
% Gall.Edges.LWidths = 7*G.Edges.Weight/max(G.Edges.Weight);
p = plot(Gall,'Layout','circle');
p.LineWidth = 2*Enum'/max(Enum);
p.XData = xval;
p.YData = yval; 
axis square;
if flagsavefig==1
    saveas(gca,['.\rst\' filename '.png']);
end;



