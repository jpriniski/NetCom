from otree.api import *
from network_experiment.generate_groups import getPairs
import random
import json
import os

#added
import csv
from collections import defaultdict

doc = """
Network Experiment
"""
path = os.path.dirname(os.path.realpath(__file__))

class C(BaseConstants):
    NAME_IN_URL = 'network_experiment'
    PLAYERS_PER_GROUP = 2
    NUM_ROUNDS = 40                     # number of rounds in the game
    num_participants = 50             # number of players in the game
    network_structure = 'homogeneous'    # network types are: random, spatial, homogeneous, influencer, and double-influencer
    n_influencer = 0                    # number of influencers (use only with influencer network)
    impressions = ['competence', 'dominance', 'maturity', 'likeability', 'trustworthiness']
    choice = [[1, 'Strongly disagree (1)'],[2, 'Disagree (2)'],[3, 'Moderately disagree (3)'],[4, 'Neither agree nor disagree (4)'],[5, 'Moderately agree (5)'],[6, 'Agree (6)'],[7, 'Strongly agree (7)']]
    custom_network = "spatial/connection_20.40.4_8.csv"     # default is "". Set it to filename.txt if using custom network

class Subsession(BaseSubsession):
    pass


class Group(BaseGroup):
    pass


class Player(BasePlayer):
    idx = models.StringField(label="Please enter your unique id assigned to you: ")
    imp_idx = models.IntegerField(initial=-1)
    name = models.StringField(label="Please enter a name associated with the person: ")
    competence = models.IntegerField(label="This person is competent: ", intial=0, blank=True)
    dominance = models.IntegerField(label="This person has a dominant personality: ", intial=0, blank=True)
    maturity = models.IntegerField(label="This person has a mature personality: ", intial=0, blank=True)
    likeability = models.IntegerField(label="This person has a like-able personality: ", intial=0, blank=True)
    trustworthiness = models.IntegerField(label="This person is trustworthy: ", intial=0, blank=True)


def creating_session(subsession):

    round_number = subsession.round_number
    if round_number == 1:
        if C.custom_network != "":
            #my_list = []
            #with open(path+"/custom_networks/"+C.custom_network, "r") as f:
            #    my_list = json.load(f)
            #    print(my_list)
            #subsession.session.group_list = my_list

            my_list = []
            with open(path+"/custom_networks/"+C.custom_network, "r") as f:
                #my_list = json.load(f) commented out 
                reader = csv.reader(f)
    
                # Use defaultdict to group pairings by trial number
                trials = defaultdict(list)
                
                for row in reader:
                    # Convert node1 and node2 to the appropriate type if necessary (int or str)
                    node1, node2, trial_number = row
                    # Append the [node1, node2] list to the corresponding trial list
                    trials[trial_number].append([int(node1), int(node2)])
                
                # Convert the defaultdict to a regular list of lists of lists
                # If trial numbers are not sequential or start from 0, this will ensure order
                my_list = [trials[key] for key in sorted(trials, key=int)]

            subsession.session.group_list = my_list

        elif C.network_structure == 'influencer':
            subsession.session.group_list = getPairs(C.network_structure, C.num_participants, C.NUM_ROUNDS, C.n_influencer)
        else:
            subsession.session.group_list = getPairs(C.network_structure, C.num_participants, C.NUM_ROUNDS)

    print(subsession.session.group_list[round_number - 1])
    subsession.set_group_matrix(subsession.session.group_list[round_number - 1])


# PAGES


class FirstPage(Page):
    form_model = "player"
    @staticmethod
    def get_form_fields(player):
        if player.round_number == 1:
            return ['idx']
        else:
            player.idx = player.in_round(1).idx

    @staticmethod
    def is_displayed(player):
        return player.round_number == 1

    @staticmethod
    def before_next_page(player, timeout_happened):
        if player.round_number == 1:
            id_list = []
            for plr in player.get_others_in_subsession():
                id_list.append(plr.field_maybe_none('idx'))
            print(id_list)
            if player.idx in id_list:
                new_val = str(random.randint(10000,99999))
                while player.idx == new_val and new_val in id_list:
                    new_val = str(random.randint(10000,99999))
                print(new_val)
                player.idx = new_val

def set_payoffs(group):
    for p in group.get_players():
        p.payoff = 0

class WaitingPage(WaitPage):
    wait_for_all_groups = True
    after_all_players_arrive = 'set_payoffs'

    @staticmethod
    def is_displayed(player):
        return player.round_number == 1


class NameSelection(Page):
    timeout_seconds = 60
    form_model = "player"
    form_fields = ['name', 'imp_idx', 'competence', 'dominance', 'maturity', 'likeability', 'trustworthiness']
    @staticmethod
    def vars_for_template(player: Player):
        return dict(curr_round=player.round_number,
                    total_rounds=C.NUM_ROUNDS)
    
    @staticmethod
    def js_vars(player):
        other_player = player.get_others_in_group()[0].field_maybe_none("imp_idx")
        c_ind = -1
        indices = [x for x in range(len(C.impressions))]
        print("imp_idx: ", other_player)
        if other_player is not None and other_player >=0:
            c_ind = other_player
        else:
            random.shuffle(indices)
            c_ind = random.choice(indices)
            player.get_others_in_group()[0].imp_idx = c_ind
        return dict(c_ind=c_ind,)

#reward policy
class ResultsWaitPage(WaitPage):
    @staticmethod
    def after_all_players_arrive(group: Group):
        player_1, player_2 = group.get_players()
        player_1.payoff, player_2.payoff = 0, 0
        if player_1.name != '' and player_2.name != '' and player_1.name.lower() == player_2.name.lower():
            player_1.payoff += 1
            player_2.payoff += 1
        p1_choice = [player_1.field_maybe_none('competence'), player_1.field_maybe_none('dominance'), player_1.field_maybe_none('maturity'), player_1.field_maybe_none('likeability'), player_1.field_maybe_none('trustworthiness')]
        p2_choice = [player_2.field_maybe_none('competence'), player_2.field_maybe_none('dominance'), player_2.field_maybe_none('maturity'), player_2.field_maybe_none('likeability'), player_2.field_maybe_none('trustworthiness')]
        for x, y in zip(p1_choice, p2_choice):
            if x and y and x == y:
                player_1.payoff += 1
                player_2.payoff += 1
                break
        
class Results(Page):
    @staticmethod
    def vars_for_template(player: Player):
        player.idx = player.in_round(1).idx
        p1_choice = [player.field_maybe_none('competence'), player.field_maybe_none('dominance'), player.field_maybe_none('maturity'), player.field_maybe_none('likeability'), player.field_maybe_none('trustworthiness')]
        player_2 = player.get_others_in_group()[0]
        p2_choice = [player_2.field_maybe_none('competence'), player_2.field_maybe_none('dominance'), player_2.field_maybe_none('maturity'), player_2.field_maybe_none('likeability'), player_2.field_maybe_none('trustworthiness')]
        p1_impression, p1_rating = None, None
        p2_impression, p2_rating = None, None

        for i in range(len(p1_choice)):
            if p1_choice[i] and p1_choice[i] > 0:
                p1_impression = C.impressions[i]
                p1_rating = C.choice[p1_choice[i]-1][1]
            
            if p2_choice[i] and p2_choice[i] > 0:
                p2_impression = C.impressions[i]
                p2_rating = C.choice[p2_choice[i]-1][1]
        
        participant = player.participant
        points = str(participant.payoff_plus_participation_fee()).split('.')[0][1:]

        return dict(current_player_name=player.name.lower(),
                    other_player_name=player.get_others_in_group()[0].name.lower(),
                    current_player_imp=p1_impression,
                    current_player_imp_rating=p1_rating,
                    other_player_imp=p2_impression,
                    other_player_imp_rating=p2_rating,
                    current_player_score=points)
    
    @staticmethod
    def js_vars(player):
        return dict(
            cp_name=player.name.lower(),
            op_name=player.get_others_in_group()[0].name.lower(),
            payoff=player.payoff,
        )
    

class LastPage(Page):
    @staticmethod
    def is_displayed(player):
        return player.round_number == C.NUM_ROUNDS
    @staticmethod
    def vars_for_template(player: Player):
        participant = player.participant
        points = str(participant.payoff_plus_participation_fee()).split('.')[0][1:]
        return dict(player_payoff=points)


page_sequence = [WaitingPage, FirstPage, NameSelection, ResultsWaitPage, Results, LastPage]
