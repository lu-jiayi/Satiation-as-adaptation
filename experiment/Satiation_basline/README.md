
## Satiation Baseline (replication of Snyder (2000))##
*The original version of this readme file was written by Ciyang Qing.*

Here we are using a template for web-based experiments developed and used in Noah Goodman's CoCoLab.

Inside `experiment` folder there is `index.html` file and 3 folders: `css`, `js`, and `shared`. These folders contain all the relevant files that power the  experiment.
The `shared` folder contains common files needed for the template and you generally do not need to change or worry about them (but you are encouraged to take a look at some point just to get a sense of what is in there). Everything else specific to the particular experiment is stored in the other two folders and the `index.html` file.

### HTML

1. The first part of the html file is between the head tags `<head> </head>`. Generally, you only need to make a few changes to reflect things specific to the current experiment (the part is close to the closing tag `</head>`).  

2. In between the body tags `<body> </body>` are (mostly) the slides used in the whole experiment. Each slide is specified within the div tags as follows `<div class="slide" id="[slideID]"></div>`. You can see that there are currently slides, whose ids are `i0`, `instructions`, `single_trial`, `one_slider`, `multi_slider`, `vertical_sliders`, `subj_info`, `thanks`.

The HTML file only specifies the skeleton of your experiment, the rest of the work is done using JavaScript.

### JavaScript

Presentation list number can be changed manually by changing the current_list variable (1-3)

The JavaScript file `index.js` in the  `js` folder specifies the flow of the experiment.

`log_responses: function(){...logging responses...}` Currently, the following information are logged for each trial: response, condition, trial_sequence_within_condition, item_number, list_number, block_number (not the presentation sequence of blocks in a list; rather, the absolute numbering of blocks in the stimuli file), trial_sequence_total; 
