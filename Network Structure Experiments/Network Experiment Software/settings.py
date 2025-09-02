from os import environ


SESSION_CONFIGS = [
    dict(
        name='network_experiment',
        display_name="Network Experiment",
        app_sequence=['network_experiment'],
        num_demo_participants=6,
    ),
    dict(
        name='hashtag_experiment',
        display_name="Hashtag Experiment",
        app_sequence=['hashtag_experiment'],
        num_demo_participants=6,
    ),
]

# if you set a property in SESSION_CONFIG_DEFAULTS, it will be inherited by all configs
# in SESSION_CONFIGS, except those that explicitly override it.
# the session config can be accessed from methods in your apps as self.session.config,
# e.g. self.session.config['participation_fee']

SESSION_CONFIG_DEFAULTS = dict(
    real_world_currency_per_point=1.00, participation_fee=0.00, doc=""
)

PARTICIPANT_FIELDS = []
SESSION_FIELDS = ['group_list', 'id_list']

# ISO-639 code
# for example: de, fr, ja, ko, zh-hans
LANGUAGE_CODE = 'en'

# e.g. EUR, GBP, CNY, JPY
# REAL_WORLD_CURRENCY_CODE = 'USD'
USE_POINTS = True

ROOMS = [
    dict(name='network_experiment', display_name='Network Experiment'),
    dict(name='network_experiment_reward', display_name='Network Experiment Reward'),
    dict(name='causality', display_name='Causal Experiment'),
    dict(name='hashtag_experiment', display_name='Hashtag Experiment'),
    dict(name='network_experiment2', display_name = 'Network Experiment 2')
]

ADMIN_USERNAME = 'admin'
# for security, best to set admin password in an environment variable
ADMIN_PASSWORD = environ.get('OTREE_ADMIN_PASSWORD')

DEMO_PAGE_INTRO_HTML = """
oTree Projects.
"""


SECRET_KEY = ''#email priniski @ ucla.edu for key

INSTALLED_APPS = ['otree']
