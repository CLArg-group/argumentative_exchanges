__includes [
  "random_tree_gen.nls"
]

extensions [
  py
  table
]

breed [arg-agents arg-agent]
breed [universal-bafs universal-baf]
breed [exchange-bafs exchange-baf]

globals [
  active-arg-agents      ;the set of currently running agents
  ticks-at-consensus     ;the number of steps taken to reach consensus
  deep-explanation?      ;set to true if the argumentation is deep (dynamic), false if otherwise

  ;; records the argumentation status at each timestamp
  record-universal-baf
  record-exchange-bafs
  record-arg-agentsets
  record-contributor-maps
]

turtles-own [
  ;; BAF representation
  arguments
  attacks
  supports
]

arg-agents-own [
  agent-no               ; the agent's number
  base-score             ; a table recording the base score of each argument in the agent's BAF
  evaluation-method      ; the method used to evaluate the agent's stance
  evaluation-range       ; a list of three intervals [negative neutral positive] representing evaluation range, each of the form [lower-bound upper-bound]
  stance                 ; the current stance of the agent towards the explanandum
  move-strategy          ; the stratey for contributing new arguments to the exchange BAF
  learning-strategy      ; the stratey for learning arguments from the exchange BAF
  bs-init-strategy       ; the strategy for initializing base scores
  init-stance            ; the initial stance of the agent
  correct-hits           ; number of times the agent successfully contributed the most influential argument available
]

to setup
  clear-all
  reset-ticks
  set-default-shape universal-bafs "circle"
  set-default-shape exchange-bafs "circle 2"
  set-default-shape arg-agents "person"
  set active-arg-agents []

  ;; set the random seed to random-gen-seed, NOT USED during experiments
  ;random-seed random-gen-seed

  ;; randomly create arguing agents until the agents are not already in consensus
  while [consensus?] [
    clear-turtles
    set active-arg-agents []
    create-arg-agents 2 [
      hide-turtle
      set size 3
      set active-arg-agents (lput self active-arg-agents)
      set base-score table:make
      set correct-hits 0
    ]

    init-agent-bafs num-arguments 2

    ;; Initializes agent 0 (machine agent)
    ask arg-agent 0 [
      setxy -12 0
      set agent-no 0
      set move-strategy agent-0-move-strategy
      set evaluation-method agent-0-evaluation-method
      set learning-strategy agent-0-learning-strategy
      set bs-init-strategy agent-0-bs-initialization-strategy
      init-default-evaluation-range self
      init-base-scores self
    ]

    ;; Initializes agent 1
    ask arg-agent 1 [
      setxy 12 0
      set agent-no 1
      set move-strategy agent-1-move-strategy
      set evaluation-method agent-1-evaluation-method
      set learning-strategy agent-1-learning-strategy
      set bs-init-strategy agent-1-bs-initialization-strategy
      init-default-evaluation-range self
      init-base-scores self
    ]

    ;; randomly assign evaluation methods to agents if the setting is set to true
    if random-eval-method? [
      assign-eval-methods-random
    ]

    ;; "saturate" the base scores, comment out if not needed
    ;saturate-base-scores

    evaluate
  ]

  ;; set the deep-explanation to true if either agent is using the shallow strategy, false if otherwise
  set deep-explanation? not (agent-0-move-strategy = "shallow" or agent-1-move-strategy = "shallow")

  ;; records the arguing agents' initial stance
  foreach active-arg-agents [ agt ->
    ask agt [
      set init-stance stance
    ]
  ]

  ;; initialize the universal BAF based on the arguing agents' BAFs
  create-universal-bafs 1 [
    set color grey
    set size 2
    set ycor 18
    set record-universal-baf self
    set arguments (remove-duplicates (sentence ([arguments] of arg-agent 0) ([arguments] of arg-agent 1)))
    set attacks (remove-duplicates (sentence ([attacks] of arg-agent 0) ([attacks] of arg-agent 1)))
    set supports (remove-duplicates (sentence ([supports] of arg-agent 0) ([supports] of arg-agent 1)))
  ]

  ;; initialize the exchange BAF as an empty BAF containing only the explanandum
  create-exchange-bafs 1 [
    set color grey
    set size 2
    set ycor 2
    set record-exchange-bafs (fput self [])
    set arguments (fput explanandum-at-interest [])
    set attacks []
    set supports []
    create-link-to record-universal-baf
  ]

  ;; draws the arguing agents onto the viewing window
  foreach active-arg-agents [ agt ->
    ask agt [
      create-link-to get-newest-exchange-baf
      show-turtle
    ]
  ]

  ;; initializes the arguing agentsets
  set record-arg-agentsets []
  save-agentset

  ;; initializes the contributor map
  set record-contributor-maps table:make

  draw-results
end

;; runs the simulation. Under shallow argumentation, the simulation terminates when the machine agent (agent 0)
;; has outputed SHALLOW-EXPL-CONSTANT arguments. Under dynamic argumentation, the simulation terminates when
;; either a consensus has been reached or the number of exchanges exceeds the limit MAX-EXCHANGES.
to run-exchanges
  ifelse ((not deep-explanation?)) [
    ifelse (ticks < shallow-expl-constant) [
      exchange
    ] [
      ifelse consensus? [
        output-print "consensus reached!"
      ] [
        output-print "consensus NOT reached!"
      ]
      stop
    ]
  ] [
    ifelse ((ticks < max-exchanges and not consensus?)) [
      exchange
    ] [
      ifelse consensus? [
        output-type "consensus reached at: "
        output-type ticks-at-consensus
        output-print " !"
      ] [
        output-print "consensus NOT reached!"
      ]
      stop
    ]
  ]
end

;; runs a single exchange by allowing the currently arguing agent CURRENT-AGENT to contribute one argument
to exchange
  tick
  ifelse consensus? and deep-explanation? [
    output-type "consensus reaches at: "
    output-print ticks-at-consensus
  ] [
    set ticks-at-consensus ticks
    make-move current-agent other-agent ;; first agent 0 then agent 1
    update-record
    learn other-agent ;; notify the other agent about the new move played
    evaluate
    draw-results
  ]
  output-print "---------------------------------\n"
end

;; ask agent AGT to learn from the exchange baf
to learn [agt]
  let ebaf get-newest-exchange-baf
  foreach [attacks] of ebaf [ x ->
    if not member? x ([attacks] of agt) [
      add-attack agt x
    ]
  ]
  foreach [supports] of ebaf [ x ->
    if not member? x ([supports] of agt) [
      add-support agt x
    ]
  ]
end





;;;;;;;;;;;;;;;;;;;;
;;SYSTEM UTILITIES;;
;;;;;;;;;;;;;;;;;;;;

;; reports the current (newest) exchange BAF
to-report get-newest-exchange-baf
  report last record-exchange-bafs
end

;; report the agent that is currently due to contribute an argument
to-report current-agent
  ifelse deep-explanation?
    [report get-agent-no ((ticks - 1) mod 2)]
    [report get-agent-no 0]
end

;; report the agent that the current agent is arguing against
to-report other-agent
  ifelse deep-explanation?
    [report get-agent-no (ticks mod 2)]
    [report get-agent-no 1]
end

;; marks the edge REL as contributed by agent AGT at time TM in the contributor map
to put-contributor [rel agt tm]
  table:put record-contributor-maps rel (list agt tm)
end

;; reports the agent who contributed edge EDGE using the contributor map
to-report get-edge-contributor [edge]
  report first (table:get record-contributor-maps edge)
end

;; reports the contributed time of edge EDGE using the contributor map
to-report get-edge-contributed-time [edge]
  report last (table:get record-contributor-maps edge)
end

;; reports the agent that is the contributor of argument ARG. If multiple agents contributed edges containing
;; argument ARG, report the agent that contributed first.
to-report get-arg-contributor [arg]
  let ebaf get-newest-exchange-baf
  let all-edges sentence [supports] of ebaf [attacks] of ebaf
  let edges filter [edg -> first edg = arg] all-edges
  if empty? edges [report current-agent]
  let first-edge first sort-by [ [r1 r2] ->
    get-edge-contributed-time r1 < get-edge-contributed-time r2
  ] edges
  report get-edge-contributor first-edge
end

;; reports true if edge REL is contributed by agent AGT, false if otherwise
to-report rel-contributed? [rel agt]
  report (get-edge-contributor rel = agt)
end

;; reports the stance of agent AGT
to-report get-stance [agt]
  report [stance] of agt
end

;; re-evaluates the stance of agent AGT
to evaluate-agent [agt]
  ask agt [ set stance stance-eval self ]
end

;; re-evaluates the stances of all arguing agent
to evaluate
  foreach active-arg-agents [ agt -> evaluate-agent agt ]
end

;; reports true if all arguing agents share the stance, false if otherwise
to-report consensus?
  report all-identical? map [x -> [stance] of x] active-arg-agents
end

;; reports the list containing stances of agent 0 and 1
to-report show-stances
  report map [x -> [stance] of x] (list agent-0 agent-1)
end

;; reports the list containing the explanandum's strength of agent 0 and 1
to-report show-strengths
  report map [x -> precision (strength-eval x explanandum-at-interest) 3] (list agent-0 agent-1)
end

;; reports the list containing the contribute accuracies of agent 0 and 1
to-report show-accuracies
  report map [x -> precision x 3] list agent-0-contribute-accuracy agent-1-contribute-accuracy
end

;; reports the current stance of agent AGT on the explanandum. Stances can only be either -1, 0, or 1
to-report stance-eval [agt]
  let score (strength-eval agt explanandum-at-interest)
  let negative (item 0 ([evaluation-range] of agt))
  let neutral (item 1 ([evaluation-range] of agt))
  let positive (item 2 ([evaluation-range] of agt))
  (ifelse
    score >= first negative and score < last negative [report -1]
    score >= first neutral and score <= last neutral [report 0]
    score > first positive and score <= last positive [report 1]
    [report 0]
  )
end

;; reports the strength of the explanandum EXPL in agent AGT's BAF
to-report strength-eval [agt expl]
  let attk [attacks] of agt
  let supp [supports] of agt
  let bs [base-score] of agt
  let method [evaluation-method] of agt
  (ifelse
    method = "QuAD" [report quad attk supp bs expl]
    method = "DF-QuAD" [report dfquad attk supp bs expl]
    method = "LocMax" [report locmax attk supp expl]
    method = "LocSum" [report locsum attk supp expl]
    method = "Euler" [report euler attk supp bs expl]
    method = "QEM" [report qem attk supp bs expl]
    ;else command
    [report 0]
  )
end

;; reports the distance of the argument START from the explanandum in agent AGT's BAF using a breadth-first search
to-report get-distance [start agt]
  let path-length 0
  let current start
  let nodes-to-visit []
  let edges sentence ([attacks] of agt) ([supports] of agt)
  while [current != explanandum-at-interest] [
    let children map [x -> list (last x) (path-length + 1)] (filter [x -> first x = current] edges)
    set nodes-to-visit (sentence nodes-to-visit children)
    set current first (first nodes-to-visit)
    set path-length last (first nodes-to-visit)
    set nodes-to-visit (but-first nodes-to-visit)
  ]
  report path-length
end

;; reports the agent with id number NUM
to-report get-agent-no [num]
  report first filter [x -> [agent-no] of x = num] active-arg-agents
end

;; updates the argumentation simulation record
to update-record
  save-agentset
end

;; makes a copy of the currently arguing agents and save the copy to the simulation record
to save-agentset
  let new-agentset []
  foreach active-arg-agents [ agt ->
    ask agt [
     hatch 1 [
       set new-agentset (lput self new-agentset)
       hide-turtle
     ]
    ]
  ]
  set record-arg-agentsets (lput new-agentset record-arg-agentsets)
end





;;;;;;;;;;;;;;;;;;;;;;;;;;
;;INITIALIZATION METHODS;;
;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Calls the function GENERATE-RANDOM-BAFS defined in "random_tree_gen.nls" to generate random BAFs for the arguing agents
to init-agent-bafs [args-no agts-no]
  let result generate-random-bafs args-no ubaf-degree agts-no explanandum-at-interest
  foreach active-arg-agents [ agt ->
    let baf item ([who] of agt) result
    ask agt [
      set arguments (item 0 baf)
      set attacks (item 1 baf)
      set supports (item 2 baf)
    ]
  ]
end

;; Initializes the base scores of agent AGT's BAF
to init-base-scores [agt]
  ask agt [
    foreach arguments [ arg ->
      table:put base-score arg (assign-base-score self arg true)
    ]
  ]
end

;; "Saturates" the base scores of the arguing agents to have the base scores model the arguments' strengths
to saturate-base-scores
  foreach active-arg-agents [ agt ->
    ask agt [
      foreach arguments [ arg ->
        table:put base-score arg (strength-eval agt arg)
      ]
    ]
  ]
end

;; Set the agent AGT's evaluation range to the default range of its evaluation method
to init-default-evaluation-range [agt]
  let method [evaluation-method] of agt
  let eval-range []
  (ifelse
    method = "QuAD" [set eval-range quad-range-default]
    method = "DF-QuAD" [set eval-range dfquad-range-default]
    method = "LocMax" [set eval-range locmax-range-default]
    method = "LocSum" [set eval-range locsum-range-default]
    method = "Euler" [set eval-range euler-range-default]
    method = "QEM" [set eval-range qem-range-default]
  )
  ask agt [
    set evaluation-range eval-range
  ]
end






;;;;;;;;;;;;;;;;;;;;;;;;;
;;AGENT MOVE STRATEGIES;;
;;;;;;;;;;;;;;;;;;;;;;;;;

;; ask agent agt to contribute a relation to the exchange baf, updating the exchange baf and contributor map
to make-move [agt other-agt]
  ;; output action message
  output-type "agent "
  output-type [agent-no] of agt
  output-type " is making its move at tick "
  output-print ticks
  output-type "evaluation method: "
  output-print [evaluation-method] of agt
  output-type "move strategy: "
  output-print [move-strategy] of agt
  output-print ""

  let ebaf get-newest-exchange-baf
  let new-move (get-new-move agt other-agt)

  ifelse empty? new-move [
    output-type agt
    output-print " does not play any moves"
  ] [
    output-type "agent "
    output-type [agent-no] of agt
    output-type " plays move "
    output-print  new-move

    ;; updates the contribute accuracy measurement
    if (member? new-move (correct-moves agt other-agt))
      [ask agt [set correct-hits (correct-hits + 1)] ]

    ;; updates the exchange BAF and records the exchange BAF's current state to RECORD-EXCHANGE-BAFS
    ask ebaf [
      hatch 1 [
        set record-exchange-bafs (lput self record-exchange-bafs)
        set arguments (remove-duplicates (lput (first new-move) arguments))
        ifelse member? new-move ([attacks] of agt)
          [set attacks (lput new-move attacks)]
          [set supports (lput new-move supports)]
      ]
      hide-turtle
    ]

    ;; updates the contributor map
    put-contributor new-move agt ticks
  ]
end

;; gets the arguments in the exchange BAF contributed by the supporting agent (stance >= 0)
to-report get-pro-args
  let args [arguments] of get-newest-exchange-baf
  let pro-args filter [x -> [stance] of get-arg-contributor x >= 0] (remove explanandum-at-interest args)
  report fput explanandum-at-interest pro-args
end

;; gets the arguments in the exchange BAF contributed by the opposing agent (stance < 0)
to-report get-con-args
  let args [arguments] of get-newest-exchange-baf
  report filter [x -> [stance] of get-arg-contributor x < 0] (remove explanandum-at-interest args)
end

;; reports true if the argument is pro, based on the definition of pro in the paper
to-report is-pro-arg? [agt arg]
  if arg = explanandum-at-interest [report true]
  let attk (filter [x -> first x = arg] ([attacks] of agt))
  let supp (filter [x -> first x = arg] ([supports] of agt))
  let attk-polarity ifelse-value (empty? attk) [false] [reduce [ [acc p] -> acc or p ]  (map [x -> not is-pro-arg? agt x] attk)]
  let supp-polarity ifelse-value (empty? supp) [false] [reduce [ [acc p] -> acc or p ]  (map [x -> is-pro-arg? agt x] supp)]
  report attk-polarity or supp-polarity
end

;; reports true if the argument is con, based on the definition of con in the paper
to-report is-con-arg? [agt arg]
  if arg = explanandum-at-interest [report false]
  let attk (filter [x -> first x = arg] ([attacks] of agt))
  let supp (filter [x -> first x = arg] ([supports] of agt))
  let attk-polarity ifelse-value (empty? attk) [false] [reduce [ [acc p] -> acc or p ]  (map [x -> not is-con-arg? agt x] attk)]
  let supp-polarity ifelse-value (empty? supp) [false] [reduce [ [acc p] -> acc or p ]  (map [x -> is-con-arg? agt x] supp)]
  report attk-polarity or supp-polarity
end

;; reports the next move agent AGT will contribute to the other agent OTHER-AGT based on AGT's move strategy
to-report get-new-move [agt other-agt]
  let agent-strategy ([move-strategy] of agt)
  (ifelse
    agent-strategy = "shallow" [report get-shallow-move agt other-agt]
    agent-strategy = "greedy" [report get-greedy-move agt other-agt]
    agent-strategy = "counterfactual" [
    report get-counterfactual-move agt other-agt]
    ;; else command
    [report get-random-move agt]
  )
end

;; reports the next shallow move agent AGT-A will contribute to AGT-B
to-report get-shallow-move [agt-a agt-b]
  let moves filter [x -> last x = explanandum-at-interest] (nontrivial-moves agt-a agt-b) ; only moves within distance of 1
  ifelse empty? moves [
    report []
  ] [
    ;; reports the strongest available move
    let sorted-moves sort-by [[r1 r2] ->
    ifelse-value strength-eval agt-a (first r1) = strength-eval agt-a (first r2)
      [get-distance (first r1) agt-a < get-distance (first r2) agt-a]
      [strength-eval agt-a (first r1) > strength-eval agt-a (first r2)]
    ] moves
    report first sorted-moves
  ]
end

;; reports the next greedy (strength greedy) move agent AGT-A will contribute to AGT-B
to-report get-greedy-move [agt-a agt-b]
  let moves (nontrivial-moves agt-a agt-b)
  ifelse empty? moves
  [report []] [
    let sorted-moves sort-by [[r1 r2] ->
    ifelse-value strength-eval agt-a (first r1) = strength-eval agt-a (first r2)
      [get-distance (first r1) agt-a < get-distance (first r2) agt-a]
      [strength-eval agt-a (first r1) > strength-eval agt-a (first r2)]
    ] moves
    report first sorted-moves
  ]
end

;; reports the non-trivial moves as [supports, attacks], based on arguments' polarity
to-report nontrivial-moves [agt-a agt-b]
  let avail-moves (get-available-moves agt-a)
  let avail-supp first avail-moves
  let avail-attk last avail-moves
  let pro-args (filter [x -> is-pro-arg? agt-a x] [arguments] of agt-a)
  let con-args (filter [x -> is-con-arg? agt-a x] [arguments] of agt-a)
  ifelse [stance] of agt-a > [stance] of agt-b [
    set avail-supp filter [x -> member? (last x) pro-args] avail-supp
    set avail-attk filter [x -> member? (last x) con-args] avail-attk
  ] [
    set avail-supp filter [x -> member? (last x) con-args] avail-supp
    set avail-attk filter [x -> member? (last x) pro-args] avail-attk
  ]
  report sentence avail-supp avail-attk
end

;; reports the next counterfactual move agent AGT-A will contribute to AGT-B
to-report get-counterfactual-move [agt-a agt-b]
  let avail-moves (get-available-moves agt-a)
  let moves (reduce sentence get-available-moves agt-a)
  let ebaf get-newest-exchange-baf
  ifelse empty? moves
    [report []]
    [ let sorted-moves moves
      ifelse [stance] of agt-a > [stance] of agt-b
        [ set sorted-moves sort-by [ [r1 r2] -> cf-order agt-a r1 r2 true ] moves ]
        [ set sorted-moves sort-by [ [r1 r2] -> cf-order agt-a r1 r2 false ] moves ]
      report first sorted-moves
    ]
end

;; reports the sorting order of moves R1 and R2 of agent AGT-A under the counterfactual move strategy.
;; If POSITIVE-STANCE is true (meaning AGT-A is supporting the explanandum), sorts R1 and R2 from highest to lowest
;; stance when added to the exchange BAF, and sorts them in the reverse order if POSITIVE-STANCE is false.
to-report cf-order [agt-a r1 r2 positive-stance?]
  let r1-effect get-move-effect-ebaf agt-a r1
  let r2-effect get-move-effect-ebaf agt-a r2
  ifelse r1-effect = r2-effect
    [ report strength-eval agt-a (first r1) > strength-eval agt-a (first r2) ]
    [
     ifelse positive-stance?
       [report r1-effect > r2-effect]
       [report r1-effect < r2-effect]
    ]
end

;; reports a random move that agent AGT can contribute
to-report get-random-move [agt]
  let ebaf get-newest-exchange-baf
  let available-moves (reduce sentence get-available-moves agt)
  ifelse empty? available-moves
    [report []]
    [report one-of available-moves]
end

;; reports the list of moves that the agent AGT can contribute to the exchange BAF, based on the exchange BAF's
;; existing arguments.
to-report get-available-moves [agt]
  let ebaf get-newest-exchange-baf
  report (list
      (filter [x -> member? (last x) [arguments] of ebaf] (difference ([supports] of agt) ([supports] of ebaf)))
      (filter [x -> member? (last x) [arguments] of ebaf] (difference ([attacks] of agt) ([attacks] of ebaf)))
    )
end

; reports the resulting stance when agent AGT adds move NEW-MOVE to the exchange BAF
to-report get-move-effect-ebaf [agt new-move]
  let private-ebaf (get-private-ebaf agt)

  ;; adds the move NEW-MOVE to AGT's private exchange BAF, accounting for confirmation bias modelling
  if not member? (first new-move) ([arguments] of private-ebaf) [
    ask private-ebaf [
      table:put base-score (first new-move) table:get [base-score] of agt (first new-move)
    ]
  ]

  let effect-score get-move-effect agt new-move private-ebaf
  ask private-ebaf [die]
  report effect-score
end

; reports the resulting stance when agent AGT adds move NEW-MOVE to BAF
to-report get-move-effect [agt new-move baf]
  ifelse member? new-move ([attacks] of agt)
    [add-attack baf new-move]
    [add-support baf new-move]

  ;; evaluats the resulting stance of the private exchange BAF after adding NEW-MOVE
  let score strength-eval baf explanandum-at-interest
  report score
end

;; reports the agent AGT's private view of the exchange BAF
to-report get-private-ebaf [agt]
  let ebaf get-newest-exchange-baf
  let private-ebaf agt

  ;; makes a copy of the current exchange BAF and replace its arguments' base scores with AGT's base scores
  ask agt [
    hatch 1 [
      set private-ebaf self
      set arguments ([arguments] of ebaf)
      set attacks ([attacks] of ebaf)
      set supports ([supports] of ebaf)
      set base-score table:make
      hide-turtle

      foreach [arguments] of private-ebaf [ arg ->
        table:put base-score arg table:get [base-score] of agt arg
      ]
    ]
  ]
  report private-ebaf
end





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;AGENT LEARNING STRATEGIES;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Adds EDGE to AGT's BAF as an attack
to add-attack [agt edge]
  ask agt [
    set attacks (lput edge attacks)
    set arguments (remove-duplicates (sentence edge arguments))
    foreach arguments [ arg ->
      if not table:has-key? base-score arg [
        table:put base-score arg (assign-base-score agt arg false)
      ]
    ]
  ]
end

;; Adds EDGE to AGT's BAF as a support
to add-support [agt edge]
  ask agt [
    set supports (lput edge supports)
    set arguments (remove-duplicates (sentence edge arguments))
    foreach arguments [ arg ->
      if not table:has-key? base-score arg [
        table:put base-score arg (assign-base-score agt arg false)
      ]
    ]
  ]
end

;; Assigns a base score to argument ARG in the agent AGT's base score table. If INIT is set to true, assigns the base score
;; based on AGT's initialization strategy. If false, assigns the base score based on AGT's learning strategy.
to-report assign-base-score [agt arg init?]
  let strategy ([learning-strategy] of agt)
  let min-val first (item 0 ([evaluation-range] of agt))
  let max-val last (item 2 ([evaluation-range] of agt))
  if init? [
    set strategy ([bs-init-strategy] of agt)
  ]
  (ifelse
    strategy = "confirmation bias" [report gen-conf-bs min-val max-val]
    strategy = "unintelligent" [report 0]
    strategy = "gullible" [report 1]
    strategy = "sceptical" [report 0.5]
    strategy = "random" [report gen-random-bs min-val max-val]
    ;else command
    [report 0.5]
  )
end

;; reports a random base score within the range of [MIN-VAL, MAX-VAL]
to-report gen-random-bs [min-val max-val]
  report generate-random-float min-val max-val
end


;; reports a confirmation bias modelling base score within the range of [MIN-VAL, MAX-VAL]
to-report gen-conf-bs [min-val max-val]
  let bs (generate-random-float min-val max-val) - confirmation-bias-offset
  if bs < 0 [set bs 0]
  report bs
end

;; reports a fixed credulous base score given by CREDULOUS-CONSTANT scaled to the range of [MIN-VAL, MAX-VAL]
to-report gen-fixed-credulous-bs [min-val max-val]
  report scale-val credulous-constant min-val max-val
end

;; reports a random credulous base score from a normal distribution with mean CREDULOUS-CONSTANT ranged [MIN-VAL, MAX-VAL]
to-report gen-random-credulous-bs [min-val max-val]
  report generate-random-norm credulous-constant min-val max-val
end

;; reports a random neutral base score from a normal distribution ranged [MIN-VAL, MAX-VAL]
to-report gen-random-neutral-bs [min-val max-val]
  let neutral (min-val + max-val) / 2
  report generate-random-norm neutral min-val max-val
end





;;;;;;;;;;;;;;;;;;;;;;;;
;;EXPERIMENT UTILITIES;;
;;;;;;;;;;;;;;;;;;;;;;;;

;; Assigns random evaluation methods to the active arguing agents
to assign-eval-methods-random
  foreach active-arg-agents [agt ->
    ask agt [
      set evaluation-method one-of experiment-eval-methods
    ]
  ]
end

;; Reports the expected random seed for the current rep in the BehaviorSpace experiment. The experiment must run in sequential order
to-report get-random-seed [reps]
  report behaviorspace-run-number mod reps
end

;; Reports the the number of steps taken to reach consensus. Reports false if consensus is not reached
to-report get-ticks-at-consensus
  ifelse consensus?
    [report ticks-at-consensus]
    [report false]
end

;; Reports the the efficiency, or the number of edges added to the exchange BAF, of the current simulation.
;; Reports false if consensus is not reached.
to-report efficiency
  ifelse consensus?
    [report length [attacks] of get-newest-exchange-baf + length [supports] of get-newest-exchange-baf ]
    [report false]
end

;; Reports the the machine efficiency, or the number of edges contributed by the machine (agent 0), of the
;; current simulation. Reports false if consensus is not reached.
to-report machine-efficiency
  let edges sentence ([attacks] of get-newest-exchange-baf) ([supports] of get-newest-exchange-baf)
  ifelse consensus?
    [report length filter [edg -> get-edge-contributor edg = agent-0] edges]
    [report false]
end

;; Reports the list of edges that AGT-A can play to influence AGT-B's stance maximally
to-report correct-moves [agt-a agt-b]
  let moves (reduce sentence get-available-moves agt-a)
  let move-effects map [r -> list r (get-actual-effect agt-a r agt-b)] moves
  let optimal-effect 0
  ifelse [stance] of agt-a > [stance] of agt-b
    [set optimal-effect max (map [x -> last x] move-effects)]
    [set optimal-effect min (map [x -> last x] move-effects)]
  report map [x -> first x] (filter [x -> last x = optimal-effect] move-effects)
end

;; Reports the actual effect of adding NEW-MOVE from AGT-A to agent AGT-B's BAF
to-report get-actual-effect [agt-a new-move agt-b]
  let agt-b-copy agt-b

  ;; makes a copy of agt-b's BAF
  ask agt-b [
    hatch 1 [
      set agt-b-copy self
      hide-turtle
    ]
  ]

  ;; adds the move NEW-MOVE to AGT's private exchange BAF, with a learned base score
  if not member? (first new-move) ([arguments] of agt-b-copy) [
    ifelse member? new-move ([attacks] of agt-a)
    [add-attack agt-b-copy new-move]
    [add-support agt-b-copy new-move]
  ]

  let effect-score get-move-effect agt-a new-move agt-b-copy
  ask agt-b-copy [die]
  report effect-score
end


;; Reports the winner of the current argument simulation
to-report argument-winner
  ifelse consensus? [
    (ifelse
      [stance] of agent-0 = [init-stance] of agent-0 [report 0]
      [stance] of agent-0 = [init-stance] of agent-1 [report 1]
      [report -1]
    )
  ] [
    report false
  ]
end

to-report agent-0-eval-method
  report [evaluation-method] of agent-0
end

to-report agent-1-eval-method
  report [evaluation-method] of agent-1
end

to-report agent-0-mov-strat
  report [move-strategy] of agent-0
end

to-report agent-1-mov-strat
  report [move-strategy] of agent-1
end

to-report agent-0-learn-strat
  report [learning-strategy] of agent-0
end

to-report agent-1-learn-strat
  report [learning-strategy] of agent-1
end

to-report agent-0-contribute-accuracy
  let edges sentence ([attacks] of get-newest-exchange-baf) ([supports] of get-newest-exchange-baf)
  let num-edges (length filter [edg -> get-edge-contributor edg = agent-0] edges)
  ifelse num-edges = 0
    [report 0]
    [report ([correct-hits] of agent-0) / num-edges]
end

to-report agent-1-contribute-accuracy
  let edges sentence ([attacks] of get-newest-exchange-baf) ([supports] of get-newest-exchange-baf)
  let num-edges (length filter [edg -> get-edge-contributor edg = agent-1] edges)
  ifelse num-edges = 0
    [report 0]
    [report ([correct-hits] of agent-1) / num-edges]
end


;; reports the initial BAF of agent 0
to-report agent-0-init-baf
  let initial-agents sort (first record-arg-agentsets)
  let init-agent-0 first filter [x -> [agent-no] of x = 0] initial-agents
  report (list ([arguments] of init-agent-0) ([attacks] of init-agent-0) ([supports] of init-agent-0))
end

;; reports the initial BAF of agent 1
to-report agent-1-init-baf
  let initial-agents sort (first record-arg-agentsets)
  let init-agent-1 first filter [x -> [agent-no] of x = 1] initial-agents
  report (list ([arguments] of init-agent-1) ([attacks] of init-agent-1) ([supports] of init-agent-1))
end

to-report report-contributor-maps
  report table:to-list record-contributor-maps
end






;;;;;;;;;;;;;;;;;;;;;;;;;
;;CONSTANTS DEFINITIONS;;
;;;;;;;;;;;;;;;;;;;;;;;;;

to-report explanandum-at-interest
  report "e"
end

to-report experiment-eval-methods
  report (list "QuAD" "DF-QuAD" "Euler" "QEM")
end

to-report agent-0
  report first filter [x -> [agent-no] of x = 0] active-arg-agents
end

to-report agent-1
  report first filter [x -> [agent-no] of x = 1] active-arg-agents
end






;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;VISUALIZATION UTILITIES;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;

to draw-results
  clear-patches
  ask record-universal-baf [ ask patch-at-heading-and-distance -60 2 [ set plabel "universal BAF" ]]
  ask get-newest-exchange-baf [ ask patch-at-heading-and-distance -60 2 [ set plabel "exchange BAF" ]]
  foreach active-arg-agents [ agt ->
    ask agt [ask patch-at-heading-and-distance -30 2 [ set plabel [agent-no] of myself ]]
  ]
  foreach active-arg-agents [ agt ->
    draw-agent agt 60 2
    ask agt [
      (ifelse
        stance > 0 [set color green]
        stance < 0 [set color red]
        ;;else
        [set color grey]
      )
    ]
  ]
  draw-agent get-newest-exchange-baf 180 2
  draw-agent record-universal-baf 45 3
end

to draw-agent [agt angle dist]
  let curr-patch 0
  ask agt [
    set curr-patch patch-at-heading-and-distance angle dist
  ]
  show-baf-nested-list ([attacks] of agt) ([supports] of agt) explanandum-at-interest curr-patch
end

;; displays the BAF with ATTK attacks and SUPP supps on the path CURR-PATCH as a nested list of arguments
to show-baf-nested-list [attk supp expl curr-patch]
  let new-patch curr-patch
  ask curr-patch [
    set plabel expl
    set new-patch patch-at-heading-and-distance 180 1
  ]
  ask new-patch [set new-patch patch-at-heading-and-distance 90 1]
  foreach get-supporters expl supp [ arg ->
    set new-patch show-baf-helper attk supp arg new-patch false
  ]
  foreach get-attackers expl attk [ arg ->
    set new-patch show-baf-helper attk supp arg new-patch true
  ]
end

to-report show-baf-helper [attk supp curr-arg curr-patch attacker?]
  let new-patch 0
  ask curr-patch [
    ifelse attacker?
      [set plabel word "-" curr-arg]
      [set plabel word "+" curr-arg]
    set new-patch patch-at-heading-and-distance 180 1
  ]
  ask new-patch [set new-patch patch-at-heading-and-distance 90 1]
  foreach get-supporters curr-arg supp [ arg ->
    set new-patch show-baf-helper attk supp arg new-patch false
  ]
  foreach get-attackers curr-arg attk [ arg ->
    set new-patch show-baf-helper attk supp arg new-patch true
  ]
  ask new-patch [set new-patch patch-at-heading-and-distance -90 1]
  report new-patch
end






;; code for the TEST button for testing purposes
to test
  print "----"
  print [agent-no] of current-agent
  print [correct-hits] of current-agent
  print correct-moves current-agent other-agent
end
@#$#@#$#@
GRAPHICS-WINDOW
779
28
1301
551
-1
-1
11.9535
1
12
1
1
1
0
1
1
1
-21
21
-21
21
0
0
1
ticks
30.0

BUTTON
157
38
230
71
NIL
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

MONITOR
510
37
605
82
NIL
consensus?
17
1
11

BUTTON
241
38
326
71
NIL
exchange
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

OUTPUT
307
485
613
650
13

MONITOR
511
92
607
137
strengths
show-strengths
17
1
11

BUTTON
69
38
148
71
clear all
clear-all
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
335
38
410
71
run
run-exchanges
T
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

CHOOSER
70
207
336
252
agent-0-evaluation-method
agent-0-evaluation-method
"QuAD" "DF-QuAD" "LocMax" "LocSum" "Euler" "QEM"
0

CHOOSER
69
156
337
201
agent-0-move-strategy
agent-0-move-strategy
"shallow" "greedy" "counterfactual" "random"
1

CHOOSER
70
261
336
306
agent-0-learning-strategy
agent-0-learning-strategy
"random" "confirmation bias" "unintelligent" "gullible" "sceptical"
3

SLIDER
67
93
269
126
num-arguments
num-arguments
0
150
30.0
1
1
arguments
HORIZONTAL

CHOOSER
391
152
663
197
agent-1-move-strategy
agent-1-move-strategy
"shallow" "greedy" "counterfactual" "random"
1

CHOOSER
392
207
664
252
agent-1-evaluation-method
agent-1-evaluation-method
"QuAD" "DF-QuAD" "LocMax" "LocSum" "Euler" "QEM"
0

CHOOSER
392
258
664
303
agent-1-learning-strategy
agent-1-learning-strategy
"random" "confirmation bias" "unintelligent" "gullible" "sceptical"
1

CHOOSER
70
320
336
365
agent-0-bs-initialization-strategy
agent-0-bs-initialization-strategy
"random" "cognitive bias" "fixed" "credulous" "credulous fixed" "neutral"
0

SLIDER
273
93
445
126
max-exchanges
max-exchanges
5
100
30.0
1
1
NIL
HORIZONTAL

SWITCH
477
433
662
466
random-eval-method?
random-eval-method?
0
1
-1000

BUTTON
424
39
487
72
NIL
test
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

CHOOSER
392
317
664
362
agent-1-bs-initialization-strategy
agent-1-bs-initialization-strategy
"random" "cognitive bias" "fixed" "credulous" "credulous fixed" "neutral"
0

SLIDER
68
386
276
419
confirmation-bias-offset
confirmation-bias-offset
0
0.2
0.0
0.01
1
NIL
HORIZONTAL

SLIDER
293
386
479
419
shallow-expl-constant
shallow-expl-constant
0
10
4.0
1
1
NIL
HORIZONTAL

SLIDER
291
432
463
465
ubaf-degree
ubaf-degree
1
15
6.0
1
1
NIL
HORIZONTAL

SLIDER
487
386
664
419
credulous-constant
credulous-constant
0
1
0.8
0.1
1
NIL
HORIZONTAL

INPUTBOX
84
429
237
489
random-gen-seed
66.0
1
0
Number

MONITOR
624
92
713
137
accuracies
show-accuracies
17
1
11

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
NetLogo 6.2.2
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
<experiments>
  <experiment name="size_experiment" repetitions="1000" runMetricsEveryStep="false">
    <setup>random-seed get-random-seed 1000
setup</setup>
    <go>run-exchanges</go>
    <metric>agent-0-eval-method</metric>
    <metric>agent-1-eval-method</metric>
    <metric>consensus?</metric>
    <metric>efficiency</metric>
    <metric>machine-efficiency</metric>
    <metric>argument-winner</metric>
    <metric>get-random-seed 1000</metric>
    <enumeratedValueSet variable="num-arguments">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-exchanges">
      <value value="10"/>
    </enumeratedValueSet>
    <steppedValueSet variable="static-expl-constant" first="1" step="1" last="6"/>
    <enumeratedValueSet variable="agent-0-move-strategy">
      <value value="&quot;static&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="agent-0-bs-initialization-strategy">
      <value value="&quot;random&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="agent-1-bs-initialization-strategy">
      <value value="&quot;random&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="agent-1-learning-strategy">
      <value value="&quot;cognitive bias&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="confirmation-bias-offset">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="credulous-constant">
      <value value="0.8"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="random-eval-method?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="ubaf-degree">
      <value value="6"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="cb_experiment" repetitions="1000" runMetricsEveryStep="false">
    <setup>random-seed get-random-seed 1000
setup</setup>
    <go>run-exchanges</go>
    <metric>agent-0-eval-method</metric>
    <metric>agent-1-eval-method</metric>
    <metric>consensus?</metric>
    <metric>efficiency</metric>
    <metric>machine-efficiency</metric>
    <metric>argument-winner</metric>
    <metric>get-random-seed 1000</metric>
    <enumeratedValueSet variable="num-arguments">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-exchanges">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="static-expl-constant">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="agent-0-move-strategy">
      <value value="&quot;static&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="agent-0-bs-initialization-strategy">
      <value value="&quot;random&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="agent-1-bs-initialization-strategy">
      <value value="&quot;random&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="agent-1-learning-strategy">
      <value value="&quot;cognitive bias&quot;"/>
    </enumeratedValueSet>
    <steppedValueSet variable="confirmation-bias-offset" first="0" step="0.05" last="0.4"/>
    <enumeratedValueSet variable="credulous-constant">
      <value value="0.8"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="random-eval-method?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="ubaf-degree">
      <value value="6"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="greedy_experiment" repetitions="1000" runMetricsEveryStep="false">
    <setup>random-seed get-random-seed 1000
setup</setup>
    <go>run-exchanges</go>
    <metric>agent-0-eval-method</metric>
    <metric>agent-1-eval-method</metric>
    <metric>consensus?</metric>
    <metric>efficiency</metric>
    <metric>machine-efficiency</metric>
    <metric>argument-winner</metric>
    <metric>get-random-seed 1000</metric>
    <enumeratedValueSet variable="num-arguments">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-exchanges">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="static-expl-constant">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="agent-0-move-strategy">
      <value value="&quot;static&quot;"/>
      <value value="&quot;strength greedy&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="agent-1-move-strategy">
      <value value="&quot;effective&quot;"/>
      <value value="&quot;strength greedy&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="agent-0-cb-modelling">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="agent-1-cb-modelling">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="agent-0-bs-initialization-strategy">
      <value value="&quot;random&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="agent-1-bs-initialization-strategy">
      <value value="&quot;random&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="agent-0-learning-strategy">
      <value value="&quot;unintelligent&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="agent-1-learning-strategy">
      <value value="&quot;cognitive bias&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="confirmation-bias-offset">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="credulous-constant">
      <value value="0.8"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="random-eval-method?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="ubaf-degree">
      <value value="6"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="learning_experiment" repetitions="1000" runMetricsEveryStep="false">
    <setup>random-seed get-random-seed 1000
setup</setup>
    <go>run-exchanges</go>
    <metric>agent-0-eval-method</metric>
    <metric>agent-1-eval-method</metric>
    <metric>consensus?</metric>
    <metric>efficiency</metric>
    <metric>machine-efficiency</metric>
    <metric>argument-winner</metric>
    <metric>get-random-seed 1000</metric>
    <enumeratedValueSet variable="num-arguments">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-exchanges">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="static-expl-constant">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="agent-0-move-strategy">
      <value value="&quot;strength greedy&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="agent-1-move-strategy">
      <value value="&quot;effective&quot;"/>
      <value value="&quot;strength greedy&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="agent-0-cb-modelling">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="agent-1-cb-modelling">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="agent-0-bs-initialization-strategy">
      <value value="&quot;random&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="agent-1-bs-initialization-strategy">
      <value value="&quot;random&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="agent-0-learning-strategy">
      <value value="&quot;unintelligent&quot;"/>
      <value value="&quot;sceptical&quot;"/>
      <value value="&quot;gullible&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="agent-1-learning-strategy">
      <value value="&quot;cognitive bias&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="confirmation-bias-offset">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="credulous-constant">
      <value value="0.8"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="random-eval-method?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="ubaf-degree">
      <value value="6"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="modelling_experiment" repetitions="1000" runMetricsEveryStep="false">
    <setup>random-seed get-random-seed 1000
setup</setup>
    <go>run-exchanges</go>
    <metric>agent-0-eval-method</metric>
    <metric>agent-1-eval-method</metric>
    <metric>consensus?</metric>
    <metric>efficiency</metric>
    <metric>machine-efficiency</metric>
    <metric>argument-winner</metric>
    <metric>get-random-seed 1000</metric>
    <enumeratedValueSet variable="num-arguments">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-exchanges">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="static-expl-constant">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="agent-0-move-strategy">
      <value value="&quot;effective&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="agent-1-move-strategy">
      <value value="&quot;effective&quot;"/>
      <value value="&quot;strength greedy&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="agent-0-cb-modelling">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="agent-1-cb-modelling">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="agent-0-bs-initialization-strategy">
      <value value="&quot;random&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="agent-1-bs-initialization-strategy">
      <value value="&quot;random&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="agent-0-learning-strategy">
      <value value="&quot;sceptical&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="agent-1-learning-strategy">
      <value value="&quot;cognitive bias&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="confirmation-bias-offset">
      <value value="0.2"/>
    </enumeratedValueSet>
    <steppedValueSet variable="private-ebaf-bias" first="0" step="0.1" last="0.4"/>
    <enumeratedValueSet variable="credulous-constant">
      <value value="0.8"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="random-eval-method?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="ubaf-degree">
      <value value="6"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="arg_experiment" repetitions="1000" runMetricsEveryStep="false">
    <setup>random-seed get-random-seed 1000
setup</setup>
    <go>run-exchanges</go>
    <metric>agent-0-eval-method</metric>
    <metric>agent-1-eval-method</metric>
    <metric>agent-0-contribute-accuracy</metric>
    <metric>agent-1-contribute-accuracy</metric>
    <metric>consensus?</metric>
    <metric>efficiency</metric>
    <metric>machine-efficiency</metric>
    <metric>argument-winner</metric>
    <metric>get-random-seed 1000</metric>
    <enumeratedValueSet variable="num-arguments">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-exchanges">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="static-expl-constant">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="agent-0-move-strategy">
      <value value="&quot;strength greedy&quot;"/>
      <value value="&quot;effective&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="agent-1-move-strategy">
      <value value="&quot;strength greedy&quot;"/>
      <value value="&quot;effective&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="agent-0-cb-modelling">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="agent-1-cb-modelling">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="agent-0-bs-initialization-strategy">
      <value value="&quot;random&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="agent-1-bs-initialization-strategy">
      <value value="&quot;random&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="agent-0-learning-strategy">
      <value value="&quot;sceptical&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="agent-1-learning-strategy">
      <value value="&quot;cognitive bias&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="confirmation-bias-offset">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="credulous-constant">
      <value value="0.8"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="random-eval-method?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="ubaf-degree">
      <value value="6"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="capped_greedy_experiment" repetitions="1000" runMetricsEveryStep="false">
    <setup>random-seed get-random-seed 1000
setup</setup>
    <go>run-exchanges</go>
    <metric>agent-0-eval-method</metric>
    <metric>agent-1-eval-method</metric>
    <metric>consensus?</metric>
    <metric>efficiency</metric>
    <metric>argument-winner</metric>
    <metric>get-random-seed 1000</metric>
    <enumeratedValueSet variable="num-arguments">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-exchanges">
      <value value="6"/>
      <value value="8"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="static-expl-constant">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="agent-0-move-strategy">
      <value value="&quot;strength greedy&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="agent-1-move-strategy">
      <value value="&quot;effective&quot;"/>
      <value value="&quot;strength greedy&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="agent-0-cb-modelling">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="agent-1-cb-modelling">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="agent-0-bs-initialization-strategy">
      <value value="&quot;random&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="agent-1-bs-initialization-strategy">
      <value value="&quot;random&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="agent-0-learning-strategy">
      <value value="&quot;unintelligent&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="agent-1-learning-strategy">
      <value value="&quot;cognitive bias&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="confirmation-bias-offset">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="credulous-constant">
      <value value="0.8"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="random-eval-method?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="ubaf-degree">
      <value value="6"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="leg_arg_experiment" repetitions="1000" runMetricsEveryStep="false">
    <setup>random-seed get-random-seed 1000
setup</setup>
    <go>run-exchanges</go>
    <metric>agent-0-eval-method</metric>
    <metric>agent-1-eval-method</metric>
    <metric>agent-0-contribute-accuracy</metric>
    <metric>agent-1-contribute-accuracy</metric>
    <metric>consensus?</metric>
    <metric>efficiency</metric>
    <metric>machine-efficiency</metric>
    <metric>argument-winner</metric>
    <metric>get-random-seed 1000</metric>
    <enumeratedValueSet variable="num-arguments">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-exchanges">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="static-expl-constant">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="agent-0-move-strategy">
      <value value="&quot;legacy strength greedy&quot;"/>
      <value value="&quot;effective&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="agent-1-move-strategy">
      <value value="&quot;legacy strength greedy&quot;"/>
      <value value="&quot;effective&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="agent-0-cb-modelling">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="agent-1-cb-modelling">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="agent-0-bs-initialization-strategy">
      <value value="&quot;random&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="agent-1-bs-initialization-strategy">
      <value value="&quot;random&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="agent-0-learning-strategy">
      <value value="&quot;sceptical&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="agent-1-learning-strategy">
      <value value="&quot;cognitive bias&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="confirmation-bias-offset">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="credulous-constant">
      <value value="0.8"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="random-eval-method?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="ubaf-degree">
      <value value="6"/>
    </enumeratedValueSet>
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
