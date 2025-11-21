from collections import defaultdict
import networkx as nx
import random
import bisect
import datetime
import matplotlib.pyplot as plt
import os

path = os.path.dirname(os.path.realpath(__file__))+'/graphs/'
ct = str(datetime.datetime.now()).split('.')[0]

def convert(adjMatrix):
    """Convert adjacency matrix to adjacency list"""
    adjList = defaultdict(list)
    for i in range(len(adjMatrix)):
        for j in range(len(adjMatrix[i])):
            if int((adjMatrix[i][j])) == 1:
                adjList[i].append(j)
    return adjList


def makePairs(adjList, nodes, existing_pairs=None):
    """Create pairs from the adjacency list without repitition
    in the same group

    Keyword arguments:
    adjList -- the adjacency list of the graph
    nodes -- the number of nodes
    existing_pairs -- to avoid repetition of pairs throughout the rounds
    """
    groups = []
    nodeset = set()
    for k, v in adjList.items():
        if k in nodeset:
            continue
        val = -1
        for value in v[::-1]:
            if value not in nodeset:
                if existing_pairs and ((k, value) in existing_pairs or (value, k) in existing_pairs):
                    continue
                val = value
                nodeset.add(k)
                nodeset.add(value)
                break
        if val > -1:
            group = [k, val]
            groups.append(group)
        else:
            continue

    if len(groups) == nodes//2:
        return groups

    nodes_not_present = []
    for i in range(nodes):
        if i not in nodeset:
            nodes_not_present.append(i)
    L = len(nodes_not_present)
    while L > 0:
        group = []
        for _ in range(2):
            if L > 0:
                num = random.choice(nodes_not_present)
                group.append(num)
                nodes_not_present.remove(num)
                L -= 1
        groups.append(group)
    return groups

def generatePairs(adjList, nodes, rounds):
    all_groups = []
    pairs = set()
    mc = 0

    for _ in range(rounds):
        while True:
            mc += 1
            new_pairs = pairs
            groups = makePairs(adjList, nodes, new_pairs)
            flag = False
            for i, j in groups:
                if (i, j) in new_pairs:
                    flag = True
                    break
                new_pairs.add((i, j))
            if mc > rounds**2:
                break
            if flag:
                continue
            pairs = new_pairs
            for j in range(len(groups)):
                groups[j] = [groups[j][0] + 1, groups[j][1] + 1]
            all_groups.append(groups)
            break
    if len(all_groups) < rounds:
        diff = rounds - len(all_groups)
        i = 0
        while diff > 0:
            if i == len(all_groups):
                i = 0
            all_groups.append(all_groups[i])
            diff -= 1
            i += 1
    
    return all_groups

def createRandomNetwork(nodes, rounds):
    """Creates a random network using gnm_random_graph and generates pairs for the given number of rounds"""
    seed = random.randint(10, 100)
    graph = nx.random_regular_graph(4, nodes, seed=seed)    # creates a random graph with nodes having degree of 4.
    pos = nx.circular_layout(graph)
    nx.draw(graph, pos=pos)
    plt.savefig(path+"random_graph_"+ct+".jpg", format="JPG")

    adjMatrix = nx.to_numpy_array(graph)
    adjList = convert(adjMatrix)
    #print(adjList)
    all_groups = generatePairs(adjList, nodes, rounds)
    return all_groups


def createHomogeneousNetwork(nodes, rounds):
    """Creates a fully connected random graph and generates pairs for the given number of rounds"""
    seed = random.randint(10, 200)
    graph = nx.erdos_renyi_graph(nodes, 1, seed=seed, directed=False)
    pos = nx.circular_layout(graph)
    nx.draw(graph, pos=pos)
    plt.savefig(path+"homogeneous_graph_"+ct+".jpg", format="JPG")

    adjMatrix = nx.to_numpy_array(graph)
    adjList = convert(adjMatrix)
    print(adjList)
    all_groups = generatePairs(adjList, nodes, rounds)
    return all_groups


def createSpatialNetwork(nodes, rounds):
    """Creates a Spatial network using Newman Watts Strogatz graph and generates pairs for the given number of rounds"""
    all_groups = []
    pairs = set()
    seed = random.randint(100, 2000)
    graph = nx.newman_watts_strogatz_graph(nodes, 4, 0, seed)
    pos = nx.circular_layout(graph)
    nx.draw(graph, pos=pos)
    plt.savefig(path+"spatial_graph_"+ct+".jpg", format="JPG")

    adjMatrix = nx.to_numpy_array(graph)
    adjList = convert(adjMatrix)

    ls = []
    for node, adj in adjList.items():
        for v in adj:
            if node != v and (node, v) not in pairs and (v, node) not in pairs:
                pairs.add((node, v))
                ls.append((node+1, v+1))

    for i in range(1, 5):
        gc = []
        nlist = [x for x in range(1, nodes+1)]
        j = 0
        if i == 1:
            while True:
                if len(gc) == nodes//2:
                    break
                gc.append([nlist[j], nlist[j+1]])
                j += 2

        elif i == 2:
            used_nums = set()
            lgt =  nodes//2
            if lgt%2 != 0:
                lgt -= 1
            while True:
                if len(gc) == lgt:
                    break
                if nlist[j] not in used_nums:
                    gc.append([nlist[j], nlist[j+2]])
                    used_nums.add(nlist[j])
                    used_nums.add(nlist[j+2])
                j += 1
            if len(gc)!=nodes//2:
                j += 2
                gc.append([nlist[j], nlist[j+1]])

        elif i == 3:
            gc.append([nlist[j], nlist[len(nlist)-1]])
            j += 1
            while True:
                if len(gc) == nodes//2:
                    break
                gc.append([nlist[j], nlist[j+1]])
                j += 2

        elif i == 4:
            gc.append([nlist[j], nlist[len(nlist)-2]])
            j += 1
            used_nums = set()
            lgt =  nodes//2
            if lgt%2 != 0:
                lgt -= 1
            while True:
                if len(gc) == lgt:
                    break
                if nlist[j] not in used_nums:
                    gc.append([nlist[j], nlist[j+2]])
                    used_nums.add(nlist[j])
                    used_nums.add(nlist[j+2])
                j += 1
            if len(gc)!=nodes//2:
                gc.append([nlist[j], nlist[j+3]])

        all_groups.append(gc)
    
    if rounds <= 4:
        return all_groups[:rounds]
    else:
        diff = rounds - 4
        i = 0
        while diff > 0:
            if i == len(all_groups):
                i = 0
            all_groups.append(all_groups[i])
            diff -= 1
            i += 1

    return all_groups


def createInfluencerNetwork(nodes, rounds, n_influencer):
    all_groups = []
    pairs = set()
    seed = random.randint(100, 2000)
    graph = nx.newman_watts_strogatz_graph(nodes, 4, 0, seed)
    adjMatrix = nx.to_numpy_array(graph)
    adjList = convert(adjMatrix)

    for x in range(n_influencer):
        adjList[x] = [k for k in range(nodes) if k!=x]
    for node, adj in adjList.items():
        for x in range(n_influencer):
            if x != node:
                bisect.insort(adj, x)
                adjList[node] = adj

    
    g = nx.Graph(adjList)
    pos = nx.circular_layout(g)
    nx.draw(g, pos=pos)
    plt.savefig(path+str(n_influencer)+"_influencer_graph_"+ct+".jpg", format="JPG")

    ls = []
    for node, adj in adjList.items():
        for v in adj:
            if node != v and (node, v) not in pairs and (v, node) not in pairs:
                pairs.add((node, v))
                ls.append((node+1, v+1))
    print(ls)
    while True:
        if rounds == 0:
            break
        random.shuffle(ls)
        ch = random.choice(ls)
        cur_round = [ch]
        cur_num = set([ch[0], ch[1]])
        for op in ls:
            if len(cur_round) == nodes//2:
                break
            if op[0] not in cur_num and op[1] not in cur_num:
                cur_num.add(op[0])
                cur_num.add(op[1])
                cur_round.append(op)
        if len(cur_round) < nodes//2:
            continue
        print(cur_round)
        all_groups.append(cur_round)
        rounds -=1
    
    return all_groups


def getPairs(network, nodes, rounds, n_influencers=0):
    if network == 'random':
        return createRandomNetwork(nodes, rounds)
    elif network == 'homogeneous':
        return createHomogeneousNetwork(nodes, rounds)
    elif network == 'spatial':
        return createSpatialNetwork(nodes, rounds)
    elif network == 'influencer':
        return createInfluencerNetwork(nodes, rounds, n_influencers)

# getPairs('influencer', 30, 30, True)
