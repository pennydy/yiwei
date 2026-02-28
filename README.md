# Mandarin contrafactive verb *yiwei*.

## General information

This is a working repository for the project on the Mandarin contrafactive verb *yiwei*.

Experiment 1 (`1_context_choice`): Including the contextual discourse cues for *yiwei* vs. *juede* in a 2FAC task setup.

Experiment 2 (`2_no_context_choice`): Removing the contextual discourse cues for *yiwei* vs. *juede* in a 2FAC task setup.

Experiment 3 (`3_context_blank`): Including the contextual discourse cues for *yiwei* vs. *juede* in an open-ended setup.

Experiment 4 (`4_no_context_blank`): Removing the contextual discourse cues for *yiwei* vs. *juede* in an open-ended setup.

## Structure of this repository
```bash
├── analysis
│   ├── childes
│   │   └── graphs
│   ├── 1_context_choice
│   │   ├── graphs
│   │   └── rscripts
│   ├── 2_no_context_choice
│   │   ├── graphs
│   │   └── rscripts
│   └── 3_context_blank
│       └── rscripts
├── data
│   ├── childes
│   ├── 1_context_choice
│   └── 2_no_context_choice
└── experiments
    ├── 1_context_choice
    ├── 2_no_context_choice
    ├── 3_context_blank
    └── 4_no_context_blak
```

- `analysis`: R files for the graphs and main analyses.
- `data`: raw data files and relevant pre-processing files.
- `experiments`: implementation of the experiments.
