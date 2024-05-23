;; Agent-Based Model for Collaborative Problem Solving
;; Yoav Bergner, 2015
;;
;; please cite:
;; Bergner, Y., Andrews, J. J., Zhu, M., & Gonzales, J. E. (2016). Agent-Based Modeling of Collaborative Problem Solving. ETS Research Report RR-16-27.


extensions [table]

globals [
  agent_names
  output_file
  initial_average_individual_score
  initial_best_individual_score
  best_individual_score
  average_individual_score
  team_score
  gain_score            ;;from Szumal, 2000; avg_ind - team_score
  synergy_score         ;;ibid. best_indiv - team_score

  ranking_table
  rankings
  allpairs
  score-list
  reached_consensus?
  number_stalled_rounds
  any_change_thisround?
  any_change_lastround?
  rev-criterion
  choose_best_criterion_met?
  random_difference_criterion_met?

  ;knowledge_sharing     ;; completeness of search; [0,1]; if 1 search all pairs; if 0.5 search half, etc.
  ;; parameters for parametrically distrubuted traits
  beta_agreeable            ;;willingness to change
  beta_talkative            ;;willingness to speak/share
  beta_comm_skill      ;;ability to communicate
  beta_critical        ;;ability to evaluate others' lists

  max_stall            ;;number of rounds in which no change to wait before giving up on consensus
]

turtles-own [
;;changing var
  ranking  ; agent's current list
  mypairs  ; used to select pairs to update
  score    ; agent's list score
  change?
  beforerank
  beforescore

;;fixed var
  knowledge                ;;content knowledge or domain skill
  agreeableness            ;;willingness to change
  talkativeness            ;;willingness to speak/share
  communication_skill      ;;ability to communicate
  trust                    ;;how much they trust others
  critical_thinking        ;;ability to evaluate others' lists
  sdo_co                   ;;social-dominance orientation vs consensus orientation

]

;;;
;;; SETUP PROCEDURES
;;;

to setup
  clear-all
;  rngs:init
;  rngs:set-seed 1 123
  import-ranking
  setup-globals
  setup-people
  ;print-headers-to-file
  reset-ticks
end

to setup-globals
  set agent_names ["Aleph" "Bet" "Gimmel" "Daled" "Hay" "Vav"]
  set reached_consensus? false
  set team_score 0
  set number_stalled_rounds 0
  set max_stall 2
  ; saving the output process
  set output_file (word "process_outfiles/" "t" team_size "_g" game_size "_k" knowledge_sharing "_run" precision (runID / 1000) 4 ".txt")
end


to setup-people
  crt team_size
    [
      setxy 12  ( 10 * (1 - ((who + 0.5)/ team_size)) )
      ;;location
      set shape "person"
      set color green
      set change? true
      set-traits
      set-ranking                     ;; also sets score
      show-ranking ranking score
      show-traits agreeableness talkativeness critical_thinking knowledge
      ;;display current ranking
      ]
  set score-list [score] of turtles
  set initial_best_individual_score max score-list
  set initial_average_individual_score mean score-list
  set best_individual_score initial_best_individual_score
  set average_individual_score initial_average_individual_score
end


to print-headers-to-file
  if (file-exists? output_file) [file-delete output_file]
  file-open output_file
  file-print (word "New Run: " runID " with team_size " team_size ", game size " game_size ", and knowledge_sharing " knowledge_sharing)
  file-print (word "Agent traits:")
  ask turtles [
      file-print (word "Agent " [item who agent_names] of self
        " A: " agreeableness " T: " talkativeness " C: " critical_thinking " K: " knowledge " initial list: " ranking)
  ]
  file-print ""
  file-close
end

;;;
;;; GO PROCEDURES
;;;

to go
  ;file-open output_file
  if (ticks > 100 or number_stalled_rounds > max_stall) [
    report-stop-condition
    stop
    ]
  set number_stalled_rounds (number_stalled_rounds + 1)

  ask turtles [
    set color green
    ]
  ask turtles
    [
      ifelse (stoch-step [talkativeness] of self)
      [
        let now_ranking [ranking] of self
        set color red
        ;file-print (word "Agent " [item who agent_names] of self " is showing " now_ranking)

        ask other turtles
          [
  ;          set change? false
            set color blue
            ;if ranking = now_ranking [print (word "Agent " [item who agent_names] of self " already agrees with this list" )]
            if ranking != now_ranking
              [
              ifelse (stoch-step [agreeableness] of self)
                [ ; feeling agreeable

                  ifelse (stoch-step [critical_thinking] of self)
                  [; thinking critically
                   ;DEBUG type "Agent " type [item who agent_names] of self print " is thinking critically"
                    set rev-criterion "critical"
                    if ([score] of self != 1) [
                      revise-ranking now_ranking rev-criterion
                    ]
                  ]
                  [; not thinking critically
                    ;DEBUG type "Agent " type [item who agent_names] of self print " is not thinking critically"
                      set rev-criterion "random"
                      revise-ranking now_ranking rev-criterion
                  ]
                  show-ranking ranking score
                 ; set color green
                 ]

                [
                 ; not agreeable
                 ;DEBUG type "Agent " type [item who agent_names] of self print " can't be bothered"
                ]
                ]
              set color green
          ]
         set color green
        ]
      [
       ;DEBUG type "Agent " type [item who agent_names] of self print " does not show list this turn"
        set color green
      ]
      ;set color green
    ]
  evaluate-team
  ;file-close
  tick

end

;;;
;;;other
;;;

to report-stop-condition
      ifelse reached_consensus? [
        set team_score average_individual_score
        set gain_score team_score - initial_average_individual_score
        set synergy_score team_score - initial_best_individual_score
        ;file-open output_file
        type "Reached consensus. Stopping!\n"
        stop
        ;file-print (word "team score: " team_score ", gain score: " gain_score ", synergy score: " synergy_score)
      ][
       ;file-open output_file
       if (number_stalled_rounds > max_stall) [type "No change for max number of rounds. Stopping!\n"]
       if (ticks > 100) [type "Ran out of allotted time. Stopping!\n"]
      ]
      ;file-close
end

to set-ranking  ;; turtle procedure
  set ranking one-of rankings
  set score table:get ranking_table ranking
  set knowledge precision ((score + 1) / 2) 2    ;; defines knowledge on a [0,1] scale
end


to set-traits  ;; turtle procedure
  set agreeableness precision (random-normal mu_agree sd_agree) 2           ;;willingness to change
  set talkativeness precision (random-normal mu_talk sd_talk) 2          ;;willingness to speak/share
  ;set communication_skill precision (random-normal mu_comm sd_comm) 2     ;;ability to communicate
  set critical_thinking precision (random-normal mu_crit sd_crit) 2       ;;ability to evaluate others' arguments
end

to evaluate-team
  let ranking_temp [ranking] of one-of turtles
  set reached_consensus? true
  ;set best_individual_score -1
  ;set average_individual_score 0
  ;set team_score 0

  ask turtles
    [
      if ranking != ranking_temp [set reached_consensus? false]
      ;let score_temp table:get ranking_table ranking
      ;if score_temp > best_individual_score [set best_individual_score score_temp]
      ;set average_individual_score average_individual_score + score_temp
    ]
  set score-list [score] of turtles
  set best_individual_score max score-list
  set average_individual_score mean score-list
  ;set average_individual_score average_individual_score / team_size
end

to revise-ranking [r revcrit]
  let revised? false
  set mypairs allpairs
 ; type "###DEBUG: length of mypairs is " print length mypairs
  while [not revised? and length mypairs > (1 - knowledge_sharing)*(length allpairs)]
    [
      ;; now look up two random locations in the ranking list
      ;; and, if the ordering (sign difference) of the two items in the now_ranking list
      ;; is different from the agent's list, then swap agents two items
      ;;
      ;; ask if critical think is ON -> choosebest (boolean) T or F
      ;; if choosebest is T then use one criterion
      ;; otherwise random criterion
      ;; if choosebest, then if (cbc) [blah]
      ;; else if (ebc) []

      ; to compare the agents lists pairwise, choose randomly through all possible pairs without replacement
      let randpair one-of mypairs
     ; type "###DEBUG: randpair selected is " print randpair
      let a (item 0 randpair - 1)
      let b (item 1 randpair - 1)
      set mypairs remove randpair mypairs
     ; type "###DEBUG: after removing " type randpair type " length of mypairs is " print length mypairs
     ;  type "Agent " type [item who agent_names] of self type " is comparing to ranking " print ranking
     ;  type "Agent " type [item who agent_names] of self type " is considering the pair " type item 0 randpair type " " type item 1 randpair type ": "

 ;     set choose_best_criterion_met? ((a != b ) and ((item a r - item b r) * (item a ranking - item b ranking) < 0) and ((item a r - item b r)*(a - b) > 0))
 ;     ifelse choose_best_criterion_met?
 ;      [type "choose best criterion met!..."]
 ;      [type "choose best not met :(..."]

      ; this is the random criterion
      set random_difference_criterion_met? ((item a r - item b r) * (item a ranking - item b ranking) < 0)

      ; this is the choosebest criterion
      set choose_best_criterion_met? (random_difference_criterion_met? and ((item a r - item b r)*(a - b) > 0))


      ; the actual decision tree
      ifelse ((revcrit = "critical" and choose_best_criterion_met?) or (revcrit = "random" and random_difference_criterion_met?))
        [
;          ifelse random_difference_criterion_met?
;               [type "order of this pair differs... "]
;               [type "order of this pair agrees..."]
;          ifelse choose_best_criterion_met?
;               [print "shown pair is better!..."]
;               [type "shown pair is not better :(..."]
;          if not choose_best_criterion_met?
;           [
;             print "changing!"
;           ]
          set beforerank ranking
          set beforescore score
          let temp1 item a ranking
          let temp2 item b ranking
          set ranking replace-item a ranking temp2
          set ranking replace-item b ranking temp1
          set score table:get ranking_table ranking
          set revised? true
          set any_change_thisround? true
          set number_stalled_rounds 0
          ]
        [
       ;   print "not gonna do it."
        ]

    ]
  if revised? = true
    [
      ;file-print (word [item who agent_names] of self " changed its ranking from " beforerank " (" beforescore ") to " ranking " (" score ")")
;      type "Agent " type [item who agent_names] of self type " changed its ranking from " type beforerank type " (" type beforescore type ") to " type ranking type " (" type score print ")"
;      file-print "Agent #" file-print [who] of self file-print " changed its ranking"
    ]

end

;;; Visualization

to show-ranking [x y]
  ask patch-at-heading-and-distance 270 6 [ set plabel x ]
  ask patch-at-heading-and-distance 270 2 [ set plabel y ]
end

to show-traits [w x y z]
      ask patch-at-heading-and-distance 90 2 [ set plabel w]
      ask patch-at-heading-and-distance 90 4 [ set plabel x]
      ask patch-at-heading-and-distance 90 6 [ set plabel y]
      ask patch-at-heading-and-distance 90 8 [ set plabel z]
end




;;; MISC PROCEDURES

;; computes the sigmoid function given an input value and the weight on the link
to-report sigmoid [input]
  report 1 / (1 + e ^ (- input))
end

;; computes the step function given an input value and the weight on the link
to-report stoch-step [input]
  report ifelse-value (input > random-float 1) [TRUE][FALSE]
end

;;import the ranking table
to import-ranking
  set ranking_table table:make
  set rankings []
  ;set scoreval []
  let ranking_table_file (word "kendallrank" game_size ".txt")
  file-open ranking_table_file
;  let tempstring file-read-line  ;; remove the title line
  while [not file-at-end?]
  [
    let items read-from-string (word "[" file-read-line "]")  ;; reads a single line into a three-item list
    table:put ranking_table item 0 items item 1 items
    set rankings lput item 0 items rankings
    ;set scoreval lput item 1 items scoreval
  ]

  file-close
;;
  set allpairs []
  let pairs_file (word "pairs" game_size ".txt")
  file-open pairs_file
  while [not file-at-end?]
  [
    let items read-from-string (word "[" file-read-line "]")  ;; reads a single line into a three-item list
    set allpairs lput item 0 items allpairs
    ;set scoreval lput item 1 items scoreval
  ]

  file-close

  ;print ranking_table
  ;print rankings
;  print scoreval
end
@#$#@#$#@
GRAPHICS-WINDOW
543
10
1362
412
-1
-1
33.73
1
24
1
1
1
0
0
0
1
0
23
0
10
0
0
1
ticks
30.0

SLIDER
8
10
180
43
team_size
team_size
0
20
5
1
1
NIL
HORIZONTAL

BUTTON
10
54
85
87
setup
setup
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
203
53
299
86
go
go
T
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

SLIDER
189
10
361
43
game_size
game_size
0
10
6
1
1
NIL
HORIZONTAL

PLOT
14
172
527
453
team level statistics
NIL
NIL
0.0
5.0
-1.1
1.1
true
true
"" ""
PENS
"average_individual_score" 1.0 0 -2674135 true "" "plot average_individual_score"
"best_individual_score" 1.0 0 -10899396 true "" "plot best_individual_score"
"consensus?" 1.0 0 -13345367 true "" "ifelse reached_consensus? [plot 1] [plot -1]"
"initial average" 1.0 2 -2674135 true "" "plot initial_average_individual_score"
"initial best" 1.0 2 -10899396 true "" "plot initial_best_individual_score"

BUTTON
90
54
186
87
go once
go
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

MONITOR
378
357
459
402
NIL
gain_score
2
1
11

MONITOR
379
403
483
448
NIL
synergy_score
2
1
11

MONITOR
378
311
463
356
NIL
team_score
2
1
11

SLIDER
367
10
539
43
knowledge_sharing
knowledge_sharing
0
1
0.8
.1
1
NIL
HORIZONTAL

TEXTBOX
812
49
962
67
agreeableness\n
11
0.0
1

TEXTBOX
1005
16
1064
34
agreeable
11
0.0
1

TEXTBOX
1082
15
1131
33
talkative
11
0.0
1

TEXTBOX
1153
15
1191
33
critical
11
0.0
1

MONITOR
368
51
529
96
NIL
any_change_thisround?
17
1
11

MONITOR
369
113
528
158
NIL
any_change_lastround?
17
1
11

MONITOR
198
110
339
155
NIL
number_stalled_rounds
0
1
11

INPUTBOX
1002
418
1064
478
mu_agree
2
1
0
Number

INPUTBOX
1003
481
1064
541
sd_agree
2
1
0
Number

INPUTBOX
1070
419
1127
479
mu_talk
2
1
0
Number

INPUTBOX
1132
418
1187
478
mu_crit
0
1
0
Number

INPUTBOX
1070
481
1127
541
sd_talk
2
1
0
Number

INPUTBOX
1132
481
1187
541
sd_crit
0.5
1
0
Number

INPUTBOX
544
419
779
479
runID
1
1
0
Number

@#$#@#$#@
## WHAT IS IT?

(a general understanding of what the model is trying to show or explain)

## HOW IT WORKS

(what rules the agents use to create the overall behavior of the model)

## HOW TO USE IT

(how to use the model, including a description of each of the items in the Interface tab)

## THINGS TO NOTICE

(suggested things for the user to notice while running the model)

## THINGS TO TRY

(suggested things for the user to try to do (move sliders, switches, etc.) with the model)

## EXTENDING THE MODEL

(suggested things to add or change in the Code tab to make the model more complicated, detailed, accurate, etc.)

## NETLOGO FEATURES

(interesting or unusual features of NetLogo that the model uses, particularly in the Code tab; or where workarounds were needed for missing features)

## RELATED MODELS

(models in the NetLogo Models Library and elsewhere which are of related interest)

## CREDITS AND REFERENCES

(a reference to the model's URL on the web if it has one, as well as any other necessary credits, citations, and links)
@#$#@#$#@
default
true
0
Polygon -7500403 true true 150 5 40 250 150 205 260 250

airplane
true
0
Polygon -7500403 true true 150 0 135 15 120 60 120 105 15 165 15 195 120 180 135 240 105 270 120 285 150 270 180 285 210 270 165 240 180 180 285 195 285 165 180 105 180 60 165 15

arrow
true
0
Polygon -7500403 true true 150 0 0 150 105 150 105 293 195 293 195 150 300 150

box
false
0
Polygon -7500403 true true 150 285 285 225 285 75 150 135
Polygon -7500403 true true 150 135 15 75 150 15 285 75
Polygon -7500403 true true 15 75 15 225 150 285 150 135
Line -16777216 false 150 285 150 135
Line -16777216 false 150 135 15 75
Line -16777216 false 150 135 285 75

bug
true
0
Circle -7500403 true true 96 182 108
Circle -7500403 true true 110 127 80
Circle -7500403 true true 110 75 80
Line -7500403 true 150 100 80 30
Line -7500403 true 150 100 220 30

butterfly
true
0
Polygon -7500403 true true 150 165 209 199 225 225 225 255 195 270 165 255 150 240
Polygon -7500403 true true 150 165 89 198 75 225 75 255 105 270 135 255 150 240
Polygon -7500403 true true 139 148 100 105 55 90 25 90 10 105 10 135 25 180 40 195 85 194 139 163
Polygon -7500403 true true 162 150 200 105 245 90 275 90 290 105 290 135 275 180 260 195 215 195 162 165
Polygon -16777216 true false 150 255 135 225 120 150 135 120 150 105 165 120 180 150 165 225
Circle -16777216 true false 135 90 30
Line -16777216 false 150 105 195 60
Line -16777216 false 150 105 105 60

car
false
0
Polygon -7500403 true true 300 180 279 164 261 144 240 135 226 132 213 106 203 84 185 63 159 50 135 50 75 60 0 150 0 165 0 225 300 225 300 180
Circle -16777216 true false 180 180 90
Circle -16777216 true false 30 180 90
Polygon -16777216 true false 162 80 132 78 134 135 209 135 194 105 189 96 180 89
Circle -7500403 true true 47 195 58
Circle -7500403 true true 195 195 58

circle
false
0
Circle -7500403 true true 0 0 300

circle 2
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240

cow
false
0
Polygon -7500403 true true 200 193 197 249 179 249 177 196 166 187 140 189 93 191 78 179 72 211 49 209 48 181 37 149 25 120 25 89 45 72 103 84 179 75 198 76 252 64 272 81 293 103 285 121 255 121 242 118 224 167
Polygon -7500403 true true 73 210 86 251 62 249 48 208
Polygon -7500403 true true 25 114 16 195 9 204 23 213 25 200 39 123

cylinder
false
0
Circle -7500403 true true 0 0 300

dot
false
0
Circle -7500403 true true 90 90 120

face happy
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 255 90 239 62 213 47 191 67 179 90 203 109 218 150 225 192 218 210 203 227 181 251 194 236 217 212 240

face neutral
false
0
Circle -7500403 true true 8 7 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Rectangle -16777216 true false 60 195 240 225

face sad
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 168 90 184 62 210 47 232 67 244 90 220 109 205 150 198 192 205 210 220 227 242 251 229 236 206 212 183

fish
false
0
Polygon -1 true false 44 131 21 87 15 86 0 120 15 150 0 180 13 214 20 212 45 166
Polygon -1 true false 135 195 119 235 95 218 76 210 46 204 60 165
Polygon -1 true false 75 45 83 77 71 103 86 114 166 78 135 60
Polygon -7500403 true true 30 136 151 77 226 81 280 119 292 146 292 160 287 170 270 195 195 210 151 212 30 166
Circle -16777216 true false 215 106 30

flag
false
0
Rectangle -7500403 true true 60 15 75 300
Polygon -7500403 true true 90 150 270 90 90 30
Line -7500403 true 75 135 90 135
Line -7500403 true 75 45 90 45

flower
false
0
Polygon -10899396 true false 135 120 165 165 180 210 180 240 150 300 165 300 195 240 195 195 165 135
Circle -7500403 true true 85 132 38
Circle -7500403 true true 130 147 38
Circle -7500403 true true 192 85 38
Circle -7500403 true true 85 40 38
Circle -7500403 true true 177 40 38
Circle -7500403 true true 177 132 38
Circle -7500403 true true 70 85 38
Circle -7500403 true true 130 25 38
Circle -7500403 true true 96 51 108
Circle -16777216 true false 113 68 74
Polygon -10899396 true false 189 233 219 188 249 173 279 188 234 218
Polygon -10899396 true false 180 255 150 210 105 210 75 240 135 240

house
false
0
Rectangle -7500403 true true 45 120 255 285
Rectangle -16777216 true false 120 210 180 285
Polygon -7500403 true true 15 120 150 15 285 120
Line -16777216 false 30 120 270 120

leaf
false
0
Polygon -7500403 true true 150 210 135 195 120 210 60 210 30 195 60 180 60 165 15 135 30 120 15 105 40 104 45 90 60 90 90 105 105 120 120 120 105 60 120 60 135 30 150 15 165 30 180 60 195 60 180 120 195 120 210 105 240 90 255 90 263 104 285 105 270 120 285 135 240 165 240 180 270 195 240 210 180 210 165 195
Polygon -7500403 true true 135 195 135 240 120 255 105 255 105 285 135 285 165 240 165 195

line
true
0
Line -7500403 true 150 0 150 300

line half
true
0
Line -7500403 true 150 0 150 150

pentagon
false
0
Polygon -7500403 true true 150 15 15 120 60 285 240 285 285 120

person
false
0
Circle -7500403 true true 110 5 80
Polygon -7500403 true true 105 90 120 195 90 285 105 300 135 300 150 225 165 300 195 300 210 285 180 195 195 90
Rectangle -7500403 true true 127 79 172 94
Polygon -7500403 true true 195 90 240 150 225 180 165 105
Polygon -7500403 true true 105 90 60 150 75 180 135 105

plant
false
0
Rectangle -7500403 true true 135 90 165 300
Polygon -7500403 true true 135 255 90 210 45 195 75 255 135 285
Polygon -7500403 true true 165 255 210 210 255 195 225 255 165 285
Polygon -7500403 true true 135 180 90 135 45 120 75 180 135 210
Polygon -7500403 true true 165 180 165 210 225 180 255 120 210 135
Polygon -7500403 true true 135 105 90 60 45 45 75 105 135 135
Polygon -7500403 true true 165 105 165 135 225 105 255 45 210 60
Polygon -7500403 true true 135 90 120 45 150 15 180 45 165 90

sheep
false
15
Circle -1 true true 203 65 88
Circle -1 true true 70 65 162
Circle -1 true true 150 105 120
Polygon -7500403 true false 218 120 240 165 255 165 278 120
Circle -7500403 true false 214 72 67
Rectangle -1 true true 164 223 179 298
Polygon -1 true true 45 285 30 285 30 240 15 195 45 210
Circle -1 true true 3 83 150
Rectangle -1 true true 65 221 80 296
Polygon -1 true true 195 285 210 285 210 240 240 210 195 210
Polygon -7500403 true false 276 85 285 105 302 99 294 83
Polygon -7500403 true false 219 85 210 105 193 99 201 83

square
false
0
Rectangle -7500403 true true 30 30 270 270

square 2
false
0
Rectangle -7500403 true true 30 30 270 270
Rectangle -16777216 true false 60 60 240 240

star
false
0
Polygon -7500403 true true 151 1 185 108 298 108 207 175 242 282 151 216 59 282 94 175 3 108 116 108

target
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240
Circle -7500403 true true 60 60 180
Circle -16777216 true false 90 90 120
Circle -7500403 true true 120 120 60

tree
false
0
Circle -7500403 true true 118 3 94
Rectangle -6459832 true false 120 195 180 300
Circle -7500403 true true 65 21 108
Circle -7500403 true true 116 41 127
Circle -7500403 true true 45 90 120
Circle -7500403 true true 104 74 152

triangle
false
0
Polygon -7500403 true true 150 30 15 255 285 255

triangle 2
false
0
Polygon -7500403 true true 150 30 15 255 285 255
Polygon -16777216 true false 151 99 225 223 75 224

truck
false
0
Rectangle -7500403 true true 4 45 195 187
Polygon -7500403 true true 296 193 296 150 259 134 244 104 208 104 207 194
Rectangle -1 true false 195 60 195 105
Polygon -16777216 true false 238 112 252 141 219 141 218 112
Circle -16777216 true false 234 174 42
Rectangle -7500403 true true 181 185 214 194
Circle -16777216 true false 144 174 42
Circle -16777216 true false 24 174 42
Circle -7500403 false true 24 174 42
Circle -7500403 false true 144 174 42
Circle -7500403 false true 234 174 42

turtle
true
0
Polygon -10899396 true false 215 204 240 233 246 254 228 266 215 252 193 210
Polygon -10899396 true false 195 90 225 75 245 75 260 89 269 108 261 124 240 105 225 105 210 105
Polygon -10899396 true false 105 90 75 75 55 75 40 89 31 108 39 124 60 105 75 105 90 105
Polygon -10899396 true false 132 85 134 64 107 51 108 17 150 2 192 18 192 52 169 65 172 87
Polygon -10899396 true false 85 204 60 233 54 254 72 266 85 252 107 210
Polygon -7500403 true true 119 75 179 75 209 101 224 135 220 225 175 261 128 261 81 224 74 135 88 99

wheel
false
0
Circle -7500403 true true 3 3 294
Circle -16777216 true false 30 30 240
Line -7500403 true 150 285 150 15
Line -7500403 true 15 150 285 150
Circle -7500403 true true 120 120 60
Line -7500403 true 216 40 79 269
Line -7500403 true 40 84 269 221
Line -7500403 true 40 216 269 79
Line -7500403 true 84 40 221 269

wolf
false
0
Polygon -16777216 true false 253 133 245 131 245 133
Polygon -7500403 true true 2 194 13 197 30 191 38 193 38 205 20 226 20 257 27 265 38 266 40 260 31 253 31 230 60 206 68 198 75 209 66 228 65 243 82 261 84 268 100 267 103 261 77 239 79 231 100 207 98 196 119 201 143 202 160 195 166 210 172 213 173 238 167 251 160 248 154 265 169 264 178 247 186 240 198 260 200 271 217 271 219 262 207 258 195 230 192 198 210 184 227 164 242 144 259 145 284 151 277 141 293 140 299 134 297 127 273 119 270 105
Polygon -7500403 true true -1 195 14 180 36 166 40 153 53 140 82 131 134 133 159 126 188 115 227 108 236 102 238 98 268 86 269 92 281 87 269 103 269 113

x
false
0
Polygon -7500403 true true 270 75 225 30 30 225 75 270
Polygon -7500403 true true 30 75 75 30 270 225 225 270

@#$#@#$#@
NetLogo 5.3.1
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
<experiments>
  <experiment name="experiment" repetitions="1000" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <metric>team_size</metric>
    <metric>game_size</metric>
    <metric>mu_agree</metric>
    <metric>sd_agree</metric>
    <metric>mu_talk</metric>
    <metric>sd_talk</metric>
    <metric>mu_crit</metric>
    <metric>sd_crit</metric>
    <metric>reached_consensus?</metric>
    <metric>team_score</metric>
    <metric>gain_score</metric>
    <metric>synergy_score</metric>
    <enumeratedValueSet variable="team_size">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="game_size">
      <value value="4"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="team5game8" repetitions="1000" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <metric>mu_agree</metric>
    <metric>sd_agree</metric>
    <metric>mu_talk</metric>
    <metric>sd_talk</metric>
    <metric>mu_crit</metric>
    <metric>sd_crit</metric>
    <metric>reached_consensus?</metric>
    <metric>team_score</metric>
    <metric>gain_score</metric>
    <metric>synergy_score</metric>
    <enumeratedValueSet variable="team_size">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="game_size">
      <value value="8"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="team5game8" repetitions="1" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <metric>mu_agree</metric>
    <metric>sd_agree</metric>
    <metric>mu_talk</metric>
    <metric>sd_talk</metric>
    <metric>mu_crit</metric>
    <metric>sd_crit</metric>
    <metric>reached_consensus?</metric>
    <metric>team_score</metric>
    <metric>gain_score</metric>
    <metric>synergy_score</metric>
    <enumeratedValueSet variable="team_size">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="game_size">
      <value value="8"/>
    </enumeratedValueSet>
    <steppedValueSet variable="runID" first="1" step="1" last="10"/>
  </experiment>
</experiments>
@#$#@#$#@
@#$#@#$#@
default
0.0
-0.2 0 0.0 1.0
0.0 1 1.0 0.0
0.2 0 0.0 1.0
link direction
true
0
Line -7500403 true 150 150 90 180
Line -7500403 true 150 150 210 180

@#$#@#$#@
0
@#$#@#$#@
