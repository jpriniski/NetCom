from otree.api import *
from hashtag_experiment.generate_groups import getPairs
import random
import os

#added
import csv
from collections import defaultdict

doc = """
Your app description
"""
path = os.path.dirname(os.path.realpath(__file__))


class C(BaseConstants):
    NAME_IN_URL = 'hashtag_experiment'
    PLAYERS_PER_GROUP = 2
    NUM_ROUNDS = 40                      # number of rounds in the game
    num_participants = 20                # number of players in the game
    network_structure = ''    # network types are: random, spatial, homogeneous, influencer, and double-influencer
    n_influencer = 0                    # number of influencers (use only with influencer network)
    custom_network = "spatial/connection_50.40.4_7.csv"  # default is "". Set it to filename.txt if using custom network

class Subsession(BaseSubsession):
    pass


class Group(BaseGroup):
    pass


class Player(BasePlayer):
    idx = models.StringField(label="Please enter your unique id assigned to you: ")
    hashtag = models.StringField(label="Write a hashtag that best describes the event that you read (do not use #fukushima or #japan): ")


def creating_session(subsession):
    round_number = subsession.round_number
    if round_number == 1:
        if C.custom_network != "":
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
    
class HashtagSelection(Page):
    timeout_seconds = 60
    form_model = "player"
    form_fields = ['hashtag']
    @staticmethod
    def vars_for_template(player: Player):
        return dict(curr_round=player.round_number,
                    total_rounds=C.NUM_ROUNDS)


class ResultsWaitPage(WaitPage):
    @staticmethod
    def after_all_players_arrive(group: Group):
        player_1, player_2 = group.get_players()
        player_1.payoff, player_2.payoff = 0, 0
        if player_1.hashtag != '' and player_2.hashtag != '' and player_1.hashtag.lower() == player_2.hashtag.lower():
            player_1.payoff += 1
            player_2.payoff += 1


class Results(Page):
    @staticmethod
    def vars_for_template(player: Player):
        player.idx = player.in_round(1).idx
        player_2 = player.get_others_in_group()[0]
        participant = player.participant
        points = str(participant.payoff_plus_participation_fee()).split('.')[0][1:]
        return dict(current_player_ht=player.hashtag.lower(),
                    other_player_ht=player_2.hashtag.lower(),
                    current_player_score=points)
    
    @staticmethod
    def js_vars(player: Player):
        return dict(
            payoff=player.payoff,
        )


class LastPage(Page):
    @staticmethod
    def is_displayed(player: Player):
        return player.round_number == C.NUM_ROUNDS
    @staticmethod
    def vars_for_template(player: Player):
        participant = player.participant
        points = str(participant.payoff_plus_participation_fee()).split('.')[0][1:]
        return dict(player_payoff=points)

page_sequence = [WaitingPage, FirstPage, HashtagSelection, ResultsWaitPage, Results, LastPage]
