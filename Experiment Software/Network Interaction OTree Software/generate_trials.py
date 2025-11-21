import numpy as np
import pandas as pd
import random
import csv

# spatial network connections generation

#output filename: filename = ['connection_' num2str(nodesnum) '.' num2str(trialnum) '.' num2str(neighborsize) '_' num2str(randseed)];
# connection_20.40.4_1: connection file for network with 20 nodes, 40
# trials, neighborhood size of 4, randsee number is 1
# in the CSV file, first two cols are node index for partners; the third col is the trial index

def network_connection_spatial(randseed, nodesnum, trialnum, neighborsize, flagsavefig=0):
    """
    Generate spatial network connections (Python version of MATLAB function).

    Args:
        randseed (int): Random seed
        nodesnum (int): Number of nodes
        trialnum (int): Number of trials (rounds)
        neighborsize (int): Neighborhood size
        flagsavefig (int): 1 to save figure (unused)
    """
    random.seed(randseed)
    np.random.seed(randseed)

    nodearrayraw = np.arange(1, nodesnum + 1)
    nodenb = round(neighborsize / 2)
    nodearrayexd = np.concatenate((
        np.arange(nodesnum - nodenb + 1, nodesnum + 1),
        nodearrayraw,
        np.arange(1, nodenb + 1)
    ))

    # Generate local neighborhood for each node
    neighbornodes = []
    for i in range(nodesnum):
        idx = i + nodenb
        nb = nodearrayexd[idx - nodenb: idx + nodenb + 1].tolist()
        nb.pop(nodenb)  # remove the node itself
        neighbornodes.append(nb)

    connectmat = []
    ccount = 0

    for ti in range(1, trialnum + 1):
        nodeflag = np.zeros(nodesnum, dtype=int)
        resampleflag = True
        print(f"Trial {ti}")

        k = random.randint(0, nodesnum - 1)
        nodeseq = np.roll(np.arange(1, nodesnum + 1), k)

        while resampleflag:
            nodeflag[:] = 0
            count1 = ccount

            for i in nodeseq:
                if nodeflag[i - 1] == 0:
                    counttemp = 0
                    while True:
                        counttemp += 1
                        indsample = random.randint(0, neighborsize - 1)
                        nodecontemp = neighbornodes[i - 1][indsample]

                        # if all neighbors are taken
                        if np.prod(nodeflag[np.array(neighbornodes[i - 1]) - 1]) == 1:
                            resampleflag = True
                            connectmat = connectmat[:count1]
                            ccount = count1
                            break

                        # if the proposed partner is still available
                        if nodeflag[nodecontemp - 1] == 0:
                            ccount += 1
                            connectmat.append([i, nodecontemp, ti])
                            nodeflag[i - 1] = 1
                            nodeflag[nodecontemp - 1] = 1
                            resampleflag = False
                            break

                        if counttemp > 1000:
                            raise RuntimeError("Cycle detected in sampling. Change randseed and retry.")
                    
                    if resampleflag:
                        break
            
            if np.prod(nodeflag) == 1:
                resampleflag = False

        if np.prod(nodeflag) == 0:
            raise RuntimeError("Missing nodes in connection matrix.")

    # Sort pairs so smaller node index comes first
    for row in connectmat:
        if row[0] > row[1]:
            row[0], row[1] = row[1], row[0]

    # Optionally save to CSV
    filename = f"connection_{nodesnum}.{trialnum}.{neighborsize}_{randseed}.csv"
    with open(filename, 'w', newline='') as f:
        writer = csv.writer(f)
        writer.writerow(['Node1', 'Node2', 'Trial'])
        writer.writerows(connectmat)

    #print(f"Connection matrix saved to {filename}")
    return np.array(connectmat)


conn = network_connection_spatial(randseed=1, nodesnum=10, trialnum=10, neighborsize=10)
conn.to_csv('connections.csv')

